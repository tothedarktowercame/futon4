(ns webarxana.client.graph
  (:require [reagent.core :as r]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]))

;; Radial layout: focus node at center, neighbours in concentric rings.

(def svg-width 1200)
(def svg-height 800)
;; Shift centre left to leave room for the focus card on the right
(def cx (- (/ svg-width 2) 100))
(def cy (/ svg-height 2))

(defn radial-positions
  "Compute {nema-id [x y]} for a neighbourhood.
   Focus at center, others in concentric rings by hop distance."
  [{:keys [nemas links focus]}]
  (when (and focus (seq nemas))
    (let [;; BFS to compute hop distances from focus
          adj (reduce (fn [m link]
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
          ;; Group by ring
          rings (->> (dissoc distances focus)
                     (group-by val)
                     (sort-by key))
          ;; Scale ring radius so nodes don't crowd: at least 80px arc between neighbours
          min-arc 80
          base-radius 160]
      (reduce
       (fn [positions [ring-idx nodes]]
         (let [n (count nodes)
               ;; Ensure enough room: radius so that arc between adjacent nodes >= min-arc
               min-r (if (> n 1) (/ (* n min-arc) (* 2 js/Math.PI)) base-radius)
               r (max (* base-radius ring-idx) min-r)
               ;; Offset angle slightly per ring to avoid radial alignment
               offset (* 0.3 ring-idx)]
           (reduce
            (fn [pos [i [nema-id _]]]
              (let [angle (+ offset (* 2 js/Math.PI (/ i n)))
                    x (+ cx (* r (js/Math.cos angle)))
                    y (+ cy (* r (js/Math.sin angle)))]
                (assoc pos nema-id [x y])))
            positions
            (map-indexed vector nodes))))
       {focus [cx cy]}
       rings))))

(defn nema-color [nema-type]
  (case nema-type
    "article"  "#4a9eff"
    "question" "#ff6b6b"
    "claim"    "#51cf66"
    "evidence" "#ffd43b"
    "pattern"  "#cc5de8"
    "#8899aa"))

(defn link-component
  "SVG line for a link-nema, with a clickable midpoint label."
  [link src-pos dst-pos]
  (let [[x1 y1] src-pos
        [x2 y2] dst-pos
        mx (/ (+ x1 x2) 2)
        my (/ (+ y1 y2) 2)
        link-type (:link/type link)
        link-text (:link/text link)]
    (let [label (if (seq link-text) link-text (or link-type "link"))
          label-w (+ 12 (* 6 (count label)))]
      [:g {:key (:link/id link)}
       [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
               :stroke "#556677"
               :stroke-width 1.5
               :stroke-dasharray (when (= link-type "scholium") "4,4")
               :opacity 0.6}]
       ;; Label at midpoint, auto-sized
       [:g {:on-click #(swap! state/ui-state assoc :editing (:link/id link))
            :style {:cursor "pointer"}}
        [:rect {:x (- mx (/ label-w 2)) :y (- my 9) :width label-w :height 18
                :rx 4 :fill "#2a2a3a" :stroke "#556677" :stroke-width 0.5
                :opacity 0.85}]
        [:text {:x mx :y (+ my 3) :text-anchor "middle"
                :fill "#aabbcc" :font-size 9 :font-family "monospace"}
         label]]])))

(defn node-component
  "SVG group for a nema node. Click to focus, or to connect in connect-mode."
  [nema pos is-focus]
  (let [[x y] pos
        nema-id (:nema/id nema)
        nema-name (or (:nema/name nema) nema-id)
        nema-type (or (:nema/type nema) "unknown")
        connecting (:connecting @state/ui-state)
        r (if is-focus 40 28)]
    [:g {:key nema-id
         :on-click (fn []
                     (if connecting
                       ;; Connect mode: link the scratchpad node to this node
                       (api/connect-nodes! (:node-id connecting) nema-id nil)
                       ;; Normal mode: focus
                       (when-not is-focus
                         (state/set-focus! nema-id))))
         :style {:cursor (if connecting "crosshair" (if is-focus "default" "pointer"))}}
     ;; Glow ring for focus
     (when is-focus
       [:circle {:cx x :cy y :r (+ r 6)
                 :fill "none" :stroke "#4a9eff"
                 :stroke-width 2 :opacity 0.4}])
     ;; Node circle
     [:circle {:cx x :cy y :r r
               :fill (nema-color nema-type)
               :opacity (if is-focus 1.0 0.75)
               :stroke (if is-focus "#ffffff" "none")
               :stroke-width (if is-focus 2 0)}]
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
  "Main SVG canvas rendering the neighbourhood graph."
  []
  (let [_tick      (:_render-tick @state/ui-state)
        focus-id   (state/focus-id)
        hood       (state/neighbourhood focus-id (:hop-depth @state/ui-state))
        scratchpad (:scratchpad @state/ui-state)
        ;; Scratchpad nodes not already in the neighbourhood
        hood-ids   (set (map :nema/id (:nemas hood)))
        floating   (->> scratchpad
                        (remove #(contains? hood-ids (:id %)))
                        vec)]
    (if (or (and hood (seq (:nemas hood))) (seq floating))
      (let [positions (if hood (radial-positions hood) {})
            nema-map  (into {} (map (fn [n] [(:nema/id n) n]) (:nemas hood)))
            ;; Place floating scratchpad nodes along bottom-left
            float-positions (into {}
                             (map-indexed
                              (fn [i node]
                                [(:id node) [(+ 80 (* i 90)) (- svg-height 60)]])
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
               :viewBox (str "0 0 " svg-width " " svg-height)
               :style {:background "#1a1a2e"}}
         ;; Links first (behind nodes)
         (when hood
           (doall
            (for [link (:links hood)
                  :let [src-id (get-in link [:link/src :nema/id])
                        dst-id (get-in link [:link/dst :nema/id])
                        src-pos (get all-positions src-id)
                        dst-pos (get all-positions dst-id)]
                  :when (and src-pos dst-pos)]
              ^{:key (:link/id link)}
              [link-component link src-pos dst-pos])))
         ;; Nodes (neighbourhood + floating)
         (doall
          (for [[nema-id pos] all-positions
                :let [nema (get all-nemas nema-id)]
                :when nema]
            ^{:key nema-id}
            [node-component nema pos (= nema-id focus-id)]))])
      ;; Empty state
      [:div.empty-graph
       [:p "No nema in focus."]
       [:p "Use the search bar to find a nema, or create a new one."]])))
