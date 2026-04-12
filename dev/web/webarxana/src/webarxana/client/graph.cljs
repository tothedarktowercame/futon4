(ns webarxana.client.graph
  (:require [reagent.core :as r]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]))

;; Multi-radial layout: each pinned node is a centre, neighbours radiate out.

(defn- svg-dims
  "Compute viewBox dimensions based on pin count."
  [n-pins]
  (let [base-w 1200
        base-h 800
        ;; Scale up for many pins to avoid crowding
        scale (max 1 (/ n-pins 2))]
    [(* base-w (js/Math.sqrt scale))
     (* base-h (js/Math.sqrt scale))]))

(defn- pin-centre
  "Compute the canvas centre for each pin, distributing evenly."
  [pins svg-w svg-h]
  (let [n (count pins)
        margin-x 60
        margin-y 40
        usable-w (- svg-w (* 2 margin-x))
        usable-h (- svg-h (* 2 margin-y))]
    (if (= n 1)
      {(:id (first pins)) [(+ margin-x (/ usable-w 2) -80)
                            (+ margin-y (/ usable-h 2))]}
      (let [cols (min n (max 2 (int (js/Math.ceil (js/Math.sqrt n)))))
            rows (int (js/Math.ceil (/ n cols)))
            cell-w (/ usable-w cols)
            cell-h (/ usable-h rows)]
        (into {}
              (map-indexed
               (fn [i pin]
                 (let [col (mod i cols)
                       row (int (/ i cols))
                       x (+ margin-x (* cell-w (+ 0.5 col)))
                       y (+ margin-y (* cell-h (+ 0.5 row)))]
                   [(:id pin) [x y]]))
               pins))))))

(defn- radial-around
  "Place neighbours radially around a centre point."
  [cx cy nodes ring-idx]
  (let [n (count nodes)
        min-arc 80
        base-radius 120
        min-r (if (> n 1) (/ (* n min-arc) (* 2 js/Math.PI)) base-radius)
        r (max (* base-radius ring-idx) min-r)
        offset (* 0.3 ring-idx)]
    (into {}
          (map-indexed
           (fn [i [nema-id _]]
             (let [angle (+ offset (* 2 js/Math.PI (/ i n)))
                   x (+ cx (* r (js/Math.cos angle)))
                   y (+ cy (* r (js/Math.sin angle)))]
               [nema-id [x y]]))
           nodes))))

(defn- radial-positions-from
  "Compute positions for a neighbourhood rooted at [cx cy]."
  [cx cy {:keys [nemas links focus]}]
  (when (and focus (seq nemas))
    (let [adj (reduce (fn [m link]
                        (let [src (get-in link [:link/src :nema/id])
                              dst (get-in link [:link/dst :nema/id])]
                          (-> m
                              (update src (fnil conj #{}) dst)
                              (update dst (fnil conj #{}) src))))
                      {} links)
          distances (loop [q (conj cljs.core/PersistentQueue.EMPTY focus)
                           dist {focus 0}]
                      (if (empty? q)
                        dist
                        (let [v (peek q)
                              d (get dist v)
                              nbrs (remove dist (get adj v []))]
                          (recur (into (pop q) nbrs)
                                 (into dist (map #(vector % (inc d)) nbrs))))))
          rings (->> (dissoc distances focus)
                     (group-by val)
                     (sort-by key))]
      (reduce
       (fn [positions [ring-idx nodes]]
         (merge positions (radial-around cx cy nodes ring-idx)))
       {focus [cx cy]}
       rings))))

(defn- multi-positions
  "Compute positions for all pins merged. First-pin-wins for shared nodes."
  [pins centres merged-hood]
  (let [nema-map (into {} (map (fn [n] [(:nema/id n) n]) (:nemas merged-hood)))]
    (reduce
     (fn [positions pin]
       (let [[cx cy] (get centres (:id pin))
             hood (state/neighbourhood (:id pin) (or (:k pin) 1))
             pin-positions (when hood (radial-positions-from cx cy hood))]
         ;; Only add positions for nodes not already placed (first-pin-wins)
         (merge-with (fn [existing _new] existing) positions (or pin-positions {}))))
     {}
     pins)))

(defn nema-color [nema-type]
  (case nema-type
    "article"  "#4a9eff"
    "question" "#ff6b6b"
    "claim"    "#51cf66"
    "evidence" "#ffd43b"
    "pattern"  "#cc5de8"
    "#8899aa"))

(defn link-component
  [link src-pos dst-pos]
  (let [[x1 y1] src-pos
        [x2 y2] dst-pos
        mx (/ (+ x1 x2) 2)
        my (/ (+ y1 y2) 2)
        link-id (:link/id link)
        link-type (:link/type link)
        link-text (:link/text link)
        has-annotation (seq link-text)
        short-label (cond
                      (and has-annotation (> (count link-text) 20))
                      (str (subs link-text 0 18) "...")
                      has-annotation link-text
                      :else (or link-type "link"))
        label short-label
        label-w (+ 12 (* 6 (count label)))
        is-editing (= (:editing @state/ui-state) link-id)
        dx (- x2 x1) dy (- y2 y1)
        len (js/Math.sqrt (+ (* dx dx) (* dy dy)))
        ratio (if (> len 30) (/ (- len 30) len) 1)
        ax2 (+ x1 (* dx ratio))
        ay2 (+ y1 (* dy ratio))]
    [:g {:key link-id}
     [:line {:x1 x1 :y1 y1 :x2 ax2 :y2 ay2
             :stroke (if is-editing "#ffd43b" "#6688aa")
             :stroke-width (if is-editing 2.5 1.5)
             :stroke-dasharray (when (= link-type "scholium") "4,4")
             :opacity 0.7
             :marker-end "url(#arrowhead)"}]
     [:g {:on-click (fn [e]
                      ;; Store link info + screen position for the popup
                      (let [rect (.getBoundingClientRect (.-currentTarget e))]
                        (swap! state/ui-state assoc
                               :editing-link {:id link-id
                                              :type (or link-type "arxana/scholium")
                                              :text (or link-text "")
                                              :x (.-clientX e)
                                              :y (.-clientY e)})))
          :style {:cursor "pointer"}}
      [:rect {:x (- mx (/ label-w 2)) :y (- my 9) :width label-w :height 18
              :rx 4 :fill "#2a2a3a"
              :stroke (if is-editing "#ffd43b" "#556677")
              :stroke-width 0.5 :opacity 0.85}]
      [:text {:x mx :y (+ my 3) :text-anchor "middle"
              :fill "#aabbcc" :font-size 9 :font-family "monospace"}
       label]]]))

(defn node-component
  [nema pos is-focus is-pin]
  (let [[x y] pos
        nema-id (:nema/id nema)
        nema-name (or (:nema/name nema) nema-id)
        nema-type (or (:nema/type nema) "unknown")
        connecting (:connecting @state/ui-state)
        r (cond is-focus 40 is-pin 34 :else 28)]
    [:g {:key nema-id
         :on-click (fn []
                     (if connecting
                       (api/connect-nodes! (:node-id connecting) nema-id nil)
                       (api/browse-and-focus! nema-id nema-id)))
         :style {:cursor (if connecting "crosshair" "pointer")}}
     ;; Pin ring: all pins get a white ring; active pin is brighter/thicker
     (when is-pin
       [:circle {:cx x :cy y :r (+ r 5)
                 :fill "none"
                 :stroke (if is-focus "#ffffff" "#aaaacc")
                 :stroke-width (if is-focus 2.5 1.5)
                 :opacity (if is-focus 0.8 0.4)}])
     ;; Node circle
     [:circle {:cx x :cy y :r r
               :fill (nema-color nema-type)
               :opacity (if is-pin 1.0 0.7)
               :stroke "none"
               :stroke-width 0}]
     ;; Type badge
     [:text {:x x :y (- y 4) :text-anchor "middle"
             :fill "#ffffff" :font-size 9 :font-family "monospace"
             :opacity 0.7}
      nema-type]
     ;; Name label
     [:text {:x x :y (+ y 8) :text-anchor "middle"
             :fill "#ffffff" :font-size 11 :font-weight "bold"
             :font-family "sans-serif"}
      (if (> (count nema-name) 16)
        (str (subs nema-name 0 14) "...")
        nema-name)]]))

(defn graph-svg
  "Main SVG canvas rendering the multi-focus neighbourhood graph."
  []
  (let [_tick      (:_render-tick @state/ui-state)
        focus-id   (state/focus-id)
        pins       (:pins @state/ui-state)
        scratchpad (:scratchpad @state/ui-state)
        ;; Fall back to single-focus if no pins yet (backward compat)
        effective-pins (if (seq pins)
                         pins
                         (when focus-id [{:id focus-id :k (:hop-depth @state/ui-state)}]))
        merged-hood (when (seq effective-pins)
                      (state/multi-neighbourhood effective-pins))
        ;; Scratchpad nodes not in the merged neighbourhood
        hood-ids   (set (map :nema/id (:nemas merged-hood)))
        floating   (->> scratchpad
                        (remove #(contains? hood-ids (:id %)))
                        vec)]
    (if (or (and merged-hood (seq (:nemas merged-hood))) (seq floating))
      (let [n-pins    (count (or effective-pins []))
            [svg-w svg-h] (svg-dims (max n-pins 1))
            centres   (when (seq effective-pins) (pin-centre effective-pins svg-w svg-h))
            positions (if (seq effective-pins)
                        (multi-positions effective-pins centres merged-hood)
                        {})
            nema-map  (into {} (map (fn [n] [(:nema/id n) n]) (:nemas merged-hood)))
            pin-ids   (set (map :id effective-pins))
            float-positions (into {}
                             (map-indexed
                              (fn [i node]
                                [(:id node) [(+ 80 (* i 90)) (- svg-h 60)]])
                              floating))
            float-nemas (into {}
                         (map (fn [node]
                                [(:id node)
                                 {:nema/id   (:id node)
                                  :nema/name (if (seq (:name node)) (:name node) "(new)")
                                  :nema/type (or (:type node) "article")}])
                              floating))
            all-positions (merge positions float-positions)
            all-nemas     (merge nema-map float-nemas)]
        [:svg {:width "100%" :height "100%"
               :viewBox (str "0 0 " svg-w " " svg-h)
               :style {:background "#1a1a2e"}}
         ;; Arrowhead marker definition
         [:defs
          [:marker {:id "arrowhead" :markerWidth 10 :markerHeight 8
                    :refX 9 :refY 4 :orient "auto" :markerUnits "strokeWidth"}
           [:path {:d "M0,0 L10,4 L0,8 L3,4 Z" :fill "#99aabb"}]]]
         ;; Links
         (when merged-hood
           (doall
            (for [link (:links merged-hood)
                  :let [src-id (get-in link [:link/src :nema/id])
                        dst-id (get-in link [:link/dst :nema/id])
                        src-pos (get all-positions src-id)
                        dst-pos (get all-positions dst-id)]
                  :when (and src-pos dst-pos)]
              ^{:key (:link/id link)}
              [link-component link src-pos dst-pos])))
         ;; Nodes
         (doall
          (for [[nema-id pos] all-positions
                :let [nema (get all-nemas nema-id)]
                :when nema]
            ^{:key nema-id}
            [node-component nema pos
             (= nema-id focus-id)
             (contains? pin-ids nema-id)]))])
      ;; Empty state
      [:div.empty-graph
       [:p "No nema in focus."]
       [:p "Use the search bar to find a nema, or create a new one."]])))
