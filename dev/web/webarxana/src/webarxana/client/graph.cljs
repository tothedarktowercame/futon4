(ns webarxana.client.graph
  (:require [reagent.core :as r]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]
            ["d3-force" :as d3]))

(def svg-width 1200)
(def svg-height 800)

;; --- Force-directed layout ---

(defn- force-layout
  "Compute positions using d3-force simulation.
   Pins are initialized at spread-out positions but free to move.
   Returns {nema-id [x y]}."
  [nemas links pin-centres]
  (let [;; Build node array — initialize pins at grid positions
        nodes (clj->js
               (mapv (fn [n]
                       (let [nid (:nema/id n)
                             pin-pos (get pin-centres nid)]
                         (cond-> {:id nid}
                           pin-pos (assoc :x (first pin-pos)
                                         :y (second pin-pos)))))
                     nemas))
        ;; Build link array
        edges (clj->js
               (keep (fn [link]
                       (let [src (get-in link [:link/src :nema/id])
                             dst (get-in link [:link/dst :nema/id])]
                         (when (and src dst)
                           {:source src :target dst})))
                     links))
        ;; Create simulation
        sim (-> (d3/forceSimulation nodes)
                (.force "charge" (.strength (d3/forceManyBody) -400))
                (.force "link" (-> (d3/forceLink edges)
                                   (.id (fn [d] (.-id d)))
                                   (.distance 120)
                                   (.strength 0.5)))
                (.force "center" (d3/forceCenter
                                  (* svg-width 0.45)
                                  (/ svg-height 2)))
                (.force "collide" (.radius (d3/forceCollide) 55))
                (.force "x" (.strength (d3/forceX (/ svg-width 2)) 0.03))
                (.force "y" (.strength (d3/forceY (/ svg-height 2)) 0.05))
                (.stop))]
    ;; Run simulation
    (dotimes [_ 200]
      (.tick sim))
    ;; Extract positions, clamp to viewBox with margin
    (into {}
          (map (fn [node]
                 [(.-id node)
                  [(max 40 (min (- svg-width 40) (.-x node)))
                   (max 40 (min (- svg-height 40) (.-y node)))]])
               (array-seq nodes)))))

(defn- pin-centres
  "Compute grid positions for pinned nodes."
  [pins]
  (let [n (count pins)
        margin-x 100
        margin-y 80
        usable-w (- svg-width (* 2 margin-x))
        usable-h (- svg-height (* 2 margin-y))]
    (if (= n 1)
      {(:id (first pins)) [(+ margin-x (/ usable-w 2) -60)
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

;; --- Rendering ---

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
        label-w (+ 14 (* 7 (count label)))
        is-editing (= (get-in @state/ui-state [:editing-link :id]) link-id)
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
                      (swap! state/ui-state assoc
                             :editing-link {:id link-id
                                            :type (or link-type "arxana/scholium")
                                            :text (or link-text "")
                                            :x (.-clientX e)
                                            :y (.-clientY e)}))
          :style {:cursor "pointer"}}
      [:rect {:x (- mx (/ label-w 2)) :y (- my 9) :width label-w :height 18
              :rx 4 :fill "#2a2a3a"
              :stroke (if is-editing "#ffd43b" "#556677")
              :stroke-width 0.5 :opacity 0.85}]
      [:text {:x mx :y (+ my 3) :text-anchor "middle"
              :fill "#aabbcc" :font-size 11 :font-family "monospace"}
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
     ;; Pin ring
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
     [:text {:x x :y (- y 6) :text-anchor "middle"
             :fill "#ffffff" :font-size 11 :font-family "monospace"
             :opacity 0.7}
      nema-type]
     ;; Name label
     [:text {:x x :y (+ y 10) :text-anchor "middle"
             :fill "#ffffff" :font-size 13 :font-weight "bold"
             :font-family "sans-serif"}
      (if (> (count nema-name) 16)
        (str (subs nema-name 0 14) "...")
        nema-name)]]))

(defn graph-svg
  "Main SVG canvas with force-directed layout."
  []
  (let [_tick      (:_render-tick @state/ui-state)
        focus-id   (state/focus-id)
        pins       (:pins @state/ui-state)
        scratchpad (:scratchpad @state/ui-state)
        effective-pins (if (seq pins)
                         pins
                         (when focus-id [{:id focus-id :k (:hop-depth @state/ui-state)}]))
        merged-hood (when (seq effective-pins)
                      (state/multi-neighbourhood effective-pins))
        hood-ids   (set (map :nema/id (:nemas merged-hood)))
        floating   (->> scratchpad
                        (remove #(contains? hood-ids (:id %)))
                        vec)]
    (if (or (and merged-hood (seq (:nemas merged-hood))) (seq floating))
      (let [centres   (when (seq effective-pins) (pin-centres effective-pins))
            ;; Force-directed layout
            positions (if (and merged-hood (seq (:nemas merged-hood)))
                        (force-layout (:nemas merged-hood) (:links merged-hood) (or centres {}))
                        {})
            nema-map  (into {} (map (fn [n] [(:nema/id n) n]) (:nemas merged-hood)))
            pin-ids   (set (map :id effective-pins))
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
