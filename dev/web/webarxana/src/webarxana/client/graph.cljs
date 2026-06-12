(ns webarxana.client.graph
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]
            ["d3-force" :as d3]))

(def svg-width 1200)
(def svg-height 800)

(def view-modes
  [{:id :organic :label "A Organic"}
   {:id :layered :label "B Layered"}
   {:id :witnesses :label "C Witnesses"}
   {:id :zoom-lod :label "D Zoom"}])

(def filter-options
  [{:id :interest-star :label "interest-star"}
   {:id :pattern-peer :label "design-pattern"}
   {:id :witness-layer :label "witness layer"}
   {:id :essay :label "arxana/essay"}
   {:id :essay-section :label "arxana/essay-section"}
   {:id :figure-tag :label "arxana/figure"}
   {:id :generic :label "generic"}])

(def default-visible-kinds #{:interest-star :pattern-peer})

(defonce !visible-kinds (r/atom default-visible-kinds))
(defonce !zoom (r/atom {:k 1 :x 0 :y 0 :signature nil}))
(defonce !drag (atom nil))

(defn- prop-value
  [m k]
  (or (get m k)
      (get m (name k))))

(defn- numeric-prop
  [m k]
  (let [v (prop-value m k)]
    (cond
      (number? v) v
      (string? v) (let [parsed (js/parseFloat v)]
                    (when-not (js/isNaN parsed) parsed))
      :else nil)))

(defn- clamp [lo hi x]
  (max lo (min hi x)))

(defn- node-kind
  [nema]
  (let [nema-type (or (:nema/type nema) "unknown")
        props (:nema/props nema)
        kind (some-> (prop-value props :node-kind) str/lower-case)]
    (cond
      (= "arxana/essay-section" nema-type)
      :essay-section

      (= "arxana/essay" nema-type)
      :essay

      (or (= "design-pattern" kind)
          (= "pattern-peer" kind)
          (= "arxana/design-pattern" nema-type)
          (= "design-pattern" nema-type))
      :pattern-peer

      (or (= "figure-tag" kind)
          (= "arxana/figure" nema-type)
          (str/includes? nema-type "figure"))
      :figure-tag

      (or (= "interest-star" kind)
          (= "arxana/interest-star" nema-type)
          (= "interest-star" nema-type))
      :interest-star

      :else
      :generic)))

(defn- node-magnitude
  [nema]
  (or (numeric-prop (:nema/props nema) :magnitude)
      1))

(defn- magnitude-radius
  [magnitude]
  (+ 22 (* 6 (dec (clamp 1 5 magnitude)))))

(defn- node-radius
  [nema is-focus is-pin]
  (let [base (case (node-kind nema)
               :pattern-peer 28
               :figure-tag 22
               :interest-star (magnitude-radius (node-magnitude nema))
               :essay 26
               :essay-section 18
               :generic 28
               28)
        pin-bump (if is-pin 2 0)
        focus-bump (if is-focus 5 0)]
    (+ base pin-bump focus-bump)))

(defn- node-fill
  [nema]
  (case (node-kind nema)
    :pattern-peer "#f59e0b"
    :figure-tag "#a78bfa"
    :interest-star "#38bdf8"
    (case (or (:nema/type nema) "unknown")
      "article"  "#4a9eff"
      "question" "#ff6b6b"
      "claim"    "#51cf66"
      "evidence" "#ffd43b"
      "pattern"  "#cc5de8"
      "#8899aa")))

(defn- diamond-points
  [x y r]
  (str x "," (- y r) " "
       (+ x r) "," y " "
       x "," (+ y r) " "
       (- x r) "," y))

(defn- node-glyph
  [{:keys [kind x y r fill opacity stroke stroke-width class-name]}]
  (case kind
    :pattern-peer
    [:polygon {:points (diamond-points x y r)
               :fill fill
               :opacity opacity
               :stroke stroke
               :stroke-width stroke-width
               :class class-name}]

    [:circle {:cx x :cy y :r r
              :fill fill
              :opacity opacity
              :stroke stroke
              :stroke-width stroke-width
              :class class-name}]))

(defn- graph-filter-active? []
  (some? (:expanded-diagram @state/ui-state)))

(defn- witness-node-ids [links]
  (->> links
       (filter #(= "interest/coalesces-into" (:link/type %)))
       (map #(get-in % [:link/src :nema/id]))
       (remove str/blank?)
       set))

(defn- effective-node-kind [witness-ids nema]
  (if (contains? witness-ids (:nema/id nema))
    :witness-layer
    (node-kind nema)))

(defn- effective-visible-kinds
  [view-mode]
  (cond-> @!visible-kinds
    (= :witnesses view-mode) (conj :witness-layer)))

(defn- apply-kind-filter [hood view-mode]
  (if-not (graph-filter-active?)
    hood
    (let [visible (effective-visible-kinds view-mode)
          witness-ids (witness-node-ids (:links hood))
          nemas (->> (:nemas hood)
                     (filter #(contains? visible (effective-node-kind witness-ids %)))
                     vec)
          visible-ids (set (map :nema/id nemas))
          links (->> (:links hood)
                     (filter #(let [src (get-in % [:link/src :nema/id])
                                    dst (get-in % [:link/dst :nema/id])]
                                (and (contains? visible-ids src)
                                     (contains? visible-ids dst))))
                     vec)]
      (assoc hood :nemas nemas :links links))))

(defn- zoom-lod-visible?
  [zoom-k nema]
  (let [magnitude (node-magnitude nema)]
    (or (>= magnitude 4)
        (and (>= zoom-k 1.35) (>= magnitude 3))
        (and (>= zoom-k 2.15) (>= magnitude 2))
        (>= zoom-k 3.0))))

(defn- apply-view-mode-filter [hood view-mode zoom-k]
  (if-not (= :zoom-lod view-mode)
    hood
    (let [nemas (->> (:nemas hood)
                     (filter #(zoom-lod-visible? zoom-k %))
                     vec)
          visible-ids (set (map :nema/id nemas))
          links (->> (:links hood)
                     (filter #(let [src (get-in % [:link/src :nema/id])
                                    dst (get-in % [:link/dst :nema/id])]
                                (and (contains? visible-ids src)
                                     (contains? visible-ids dst))))
                     vec)]
      (assoc hood :nemas nemas :links links))))

(defn- filter-controller []
  (let [visible @!visible-kinds]
    [:div {:style {:display "flex"
                   :flex-direction "column"
                   :gap "6px"
                   :padding "9px 10px"
                   :border "1px solid rgba(148, 163, 184, 0.35)"
                   :border-radius "6px"
                   :background "rgba(15, 23, 42, 0.88)"
                   :box-shadow "0 8px 24px rgba(0,0,0,0.22)"
                   :font-family "sans-serif"
                   :color "#e5e7eb"}}
     [:div {:style {:display "flex" :justify-content "space-between" :align-items "center"}}
      [:span {:style {:font-size "12px" :font-weight 700}} "Kinds"]
      [:button {:type "button"
                :on-click #(reset! !visible-kinds default-visible-kinds)
                :style {:font-size "11px" :background "#1e293b" :color "#cbd5e1"
                        :border "1px solid rgba(148, 163, 184, 0.45)"
                        :border-radius "4px" :padding "2px 6px"}}
       "core"]]
     (for [{:keys [id label]} filter-options]
       ^{:key (name id)}
       [:label {:style {:display "flex" :gap "7px" :align-items "center"
                        :font-size "12px" :cursor "pointer"}}
        [:input {:type "checkbox"
                 :checked (contains? visible id)
                 :on-change #(swap! !visible-kinds
                                    (fn [ks]
                                      (if (contains? ks id)
                                        (disj ks id)
                                        (conj ks id))))}]
        [:span label]])]))

(defn- bounds-for [positions]
  (when (seq positions)
    (let [xs (map first (vals positions))
          ys (map second (vals positions))]
      {:min-x (apply min xs)
       :max-x (apply max xs)
       :min-y (apply min ys)
       :max-y (apply max ys)})))

(defn- fit-transform [positions]
  (if-let [{:keys [min-x max-x min-y max-y]} (bounds-for positions)]
    (let [margin 80
          bw (max 1 (- max-x min-x))
          bh (max 1 (- max-y min-y))
          k (clamp 0.18 2.2 (min (/ (- svg-width (* 2 margin)) bw)
                                  (/ (- svg-height (* 2 margin)) bh)))
          cx (/ (+ min-x max-x) 2)
          cy (/ (+ min-y max-y) 2)]
      {:k k
       :x (- (/ svg-width 2) (* k cx))
       :y (- (/ svg-height 2) (* k cy))})
    {:k 1 :x 0 :y 0}))

(defn- positions-signature [positions]
  (str (count positions) ":" (str/join "|" (sort (keys positions)))))

(defn- fit-graph! [positions]
  (let [sig (positions-signature positions)]
    (swap! !zoom merge (assoc (fit-transform positions) :signature sig))))

(defn- fit-on-new-graph! [positions]
  (let [sig (positions-signature positions)]
    (when (and (seq positions) (not= sig (:signature @!zoom)))
      (fit-graph! positions))))

(defn- svg-point [e]
  (let [rect (.getBoundingClientRect (.-currentTarget e))
        sx (/ svg-width (.-width rect))
        sy (/ svg-height (.-height rect))]
    [(* (- (.-clientX e) (.-left rect)) sx)
     (* (- (.-clientY e) (.-top rect)) sy)]))

(defn- screen-delta->svg [svg-el dx dy]
  (let [rect (.getBoundingClientRect svg-el)]
    [(* dx (/ svg-width (.-width rect)))
     (* dy (/ svg-height (.-height rect)))]))

(defn- zoom-transform-attr []
  (let [{:keys [k x y]} @!zoom]
    (str "translate(" x "," y ") scale(" k ")")))

(defn- band-y
  [kind view-mode]
  (case view-mode
    :layered (case kind
               :interest-star 190
               :essay 315
               :figure-tag 410
               :witness-layer 505
               :pattern-peer 610
               :essay-section 680
               :generic 400
               400)
    :witnesses (case kind
                 :interest-star 190
                 :witness-layer 400
                 :pattern-peer 610
                 :essay 300
                 :essay-section 685
                 :figure-tag 500
                 :generic 400
                 400)
    (/ svg-height 2)))

(defn- layered-layout
  [nemas links pin-centres view-mode]
  (let [witness-ids (witness-node-ids links)
        kind-of #(effective-node-kind witness-ids %)
        groups (group-by kind-of nemas)
        ordered-kinds [:interest-star :essay :figure-tag :witness-layer
                       :pattern-peer :essay-section :generic]]
    (into {}
          (mapcat
           (fn [kind]
             (let [nodes (sort-by (comp str/lower-case #(or (:nema/name %) (:nema/id %))) (get groups kind))
                   n (count nodes)
                   span (- svg-width 220)
                   step (if (pos? n) (/ span (inc n)) span)
                   y (band-y kind view-mode)]
               (map-indexed
                (fn [i nema]
                  (let [nid (:nema/id nema)]
                    [nid (or (get pin-centres nid)
                             [(+ 110 (* step (inc i))) y])]))
                nodes)))
           ordered-kinds))))

(defn- zoom-controls [positions]
  [:button {:type "button"
            :on-click #(fit-graph! positions)
            :style {:background "rgba(15, 23, 42, 0.88)"
                    :color "#e5e7eb"
                    :border "1px solid rgba(148, 163, 184, 0.45)"
                    :border-radius "6px"
                    :font-size "12px"
                    :font-weight 700
                    :padding "8px 10px"
                    :cursor "pointer"
                    :box-shadow "0 8px 24px rgba(0,0,0,0.22)"}}
   "Reset / fit"])

(defn- view-mode-selector []
  (let [current (:view-mode @state/ui-state)]
    [:div {:style {:display "inline-flex"
                   :align-items "center"
                   :gap "8px"
                   :padding "8px 10px"
                   :border "1px solid rgba(148, 163, 184, 0.35)"
                   :border-radius "6px"
                   :background "rgba(15, 23, 42, 0.86)"
                   :box-shadow "0 8px 24px rgba(0,0,0,0.22)"
                   :font-family "sans-serif"}}
     [:span {:style {:color "#cbd5e1"
                     :font-size "12px"
                     :font-weight 700}}
      "View"]
     [:select {:value (name (or current :organic))
               :on-change (fn [e]
                            (swap! state/ui-state assoc
                                   :view-mode
                                   (keyword (.. e -target -value))))
               :style {:background "#0f172a"
                       :color "#e5e7eb"
                       :border "1px solid rgba(148, 163, 184, 0.45)"
                       :border-radius "4px"
                       :font-size "12px"
                       :padding "4px 6px"}}
      (for [{:keys [id label]} view-modes]
        ^{:key (name id)}
        [:option {:value (name id)} label])]]))

;; --- Force-directed layout ---

(defn- force-layout
  "Compute positions using d3-force simulation.
   Edge midpoints are included as phantom nodes for label spacing.
   Returns {nema-id [x y]}."
  [nemas links pin-centres view-mode]
  (let [;; Real nodes — initialize pins at grid positions
        witness-ids (witness-node-ids links)
        real-nodes (mapv (fn [n]
                           (let [nid (:nema/id n)
                                 pin-pos (get pin-centres nid)]
                             (cond-> {:id nid
                                      :kind (name (effective-node-kind witness-ids n))
                                      :radius (+ 10 (node-radius n false false))}
                               pin-pos (assoc :x (first pin-pos)
                                             :y (second pin-pos)))))
                         nemas)
        ;; Phantom nodes for edge label midpoints (prevents label overlap)
        phantom-nodes (keep (fn [link]
                              (let [lid (:link/id link)]
                                (when lid
                                  {:id (str "phantom:" lid) :radius 25})))
                            links)
        all-sim-nodes (clj->js (into real-nodes phantom-nodes))
        ;; Real edges
        real-edges (keep (fn [link]
                           (let [src (get-in link [:link/src :nema/id])
                                 dst (get-in link [:link/dst :nema/id])
                                 lid (:link/id link)
                                 pid (str "phantom:" lid)]
                             (when (and src dst lid)
                               ;; Connect phantom to both endpoints
                               [{:source src :target pid}
                                {:source pid :target dst}])))
                         links)
        all-edges (clj->js (vec (mapcat identity real-edges)))
        ;; Centre of usable area (leave room for cards on right)
        cx (* svg-width 0.42)
        cy (/ svg-height 2)
        banded? (= :witnesses view-mode)
        ;; Create simulation
        sim (-> (d3/forceSimulation all-sim-nodes)
                (.force "charge" (-> (d3/forceManyBody)
                                     (.strength (if banded? -450 -600))
                                     (.distanceMax 500)))
                (.force "link" (-> (d3/forceLink all-edges)
                                   (.id (fn [d] (.-id d)))
                                   (.distance (if banded? 125 100))
                                   (.strength (if banded? 0.45 0.7))))
                (.force "center" (d3/forceCenter cx cy))
                (.force "collide" (.radius (d3/forceCollide)
                                           (fn [d] (or (.-radius d) 30))))
                (.force "x" (.strength (d3/forceX cx) 0.04))
                (.force "y" (.strength (d3/forceY (if banded?
                                                     (fn [d]
                                                       (if (.startsWith (.-id d) "phantom:")
                                                         cy
                                                         (band-y (keyword (.-kind d)) view-mode)))
                                                     cy))
                                       (if banded? 0.2 0.04)))
                (.stop))]
    ;; Run simulation
    (dotimes [_ 250]
      (.tick sim))
    ;; Extract positions for real nodes only (skip phantoms)
    (into {}
          (keep (fn [node]
                  (let [nid (.-id node)]
                    (when-not (.startsWith nid "phantom:")
                      [nid
                       [(max 50 (min (- svg-width 50) (.-x node)))
                        (max 50 (min (- svg-height 50) (.-y node)))]]))))
               (array-seq all-sim-nodes))))

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
             :stroke-dasharray (cond
                                 (= link-type "scholium") "4,4"
                                 (= link-type "pattern/tensions") "6,5")
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
        props (:nema/props nema)
        kind (node-kind nema)
        connecting (:connecting @state/ui-state)
        r (node-radius nema is-focus is-pin)
        magnitude (node-magnitude nema)
        folded-count (numeric-prop props :fold/sub-count)]
    [:g {:key nema-id
         :class (str "graph-node node " (name kind))
         :on-click (fn []
                     (if connecting
                       (api/connect-nodes! (:node-id connecting) nema-id nil)
                       (api/browse-and-focus! nema-id nema-id)))
         :on-double-click
         (fn [_]
           (cond
             (= nema-type "arxana/essay")
             (if (contains? (:expanded-essays @state/ui-state) nema-id)
               (api/collapse-essay! nema-id)
               (api/expand-essay! nema-id))

             (= nema-type "arxana/essay-section")
             (api/open-in-emacs! {:id nema-id :type nema-type})

             (= nema-type "scope/frame")
             (api/expand-scope-frame! nema-id)))
         :style {:cursor (if connecting "crosshair" "pointer")}}
     ;; Pin ring
     (when is-pin
       [node-glyph {:kind kind
                    :x x :y y :r (+ r 6)
                    :fill "none"
                    :opacity (if is-focus 0.8 0.4)
                    :stroke (if is-focus "#ffffff" "#aaaacc")
                    :stroke-width (if is-focus 2.5 1.5)
                    :class-name "node-ring"}])
     ;; Node shape
     [node-glyph {:kind kind
                  :x x :y y :r r
                  :fill (node-fill nema)
                  :opacity (if is-pin 1.0 0.78)
                  :stroke (when (= kind :pattern-peer) "#fed7aa")
                  :stroke-width (if (= kind :pattern-peer) 1.5 0)
                  :class-name (str "node-shape " (name kind))}]
     ;; Type badge
     [:text {:x x :y (- y 6) :text-anchor "middle"
             :fill "#ffffff" :font-size 11 :font-family "monospace"
             :opacity 0.7}
      (case kind
        :pattern-peer "design-pattern"
        :interest-star (str "m" magnitude)
        (if folded-count
          (str folded-count " concepts")
          nema-type))]
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
        expanded-essay-sections (:expanded-essay-sections @state/ui-state)
        view-mode  (or (:view-mode @state/ui-state) :organic)
        zoom-k     (:k @!zoom)
        effective-pins (if (seq pins)
                         pins
                         (when focus-id [{:id focus-id :k (:hop-depth @state/ui-state)}]))
        raw-hood (when (seq effective-pins)
                   (state/multi-neighbourhood effective-pins))
        ;; Diagram filtering:
        ;; - Pinned diagram (compressed): keep the diagram node, hide
        ;;   diagram/includes links and their target entities (the "contents")
        ;; - Non-pinned diagram (neighbour): hide it entirely
        pin-id-set (set (map :id (or effective-pins [])))
        merged-hood (when raw-hood
                      (let [essay-links (->> expanded-essay-sections
                                             (mapcat (fn [[essay-id section-ids]]
                                                       (map (fn [section-id]
                                                              {:link/id (str "essay-expand:" essay-id "->" section-id)
                                                               :link/type "essay/includes-section"
                                                               :link/src {:nema/id essay-id}
                                                               :link/dst {:nema/id section-id}})
                                                            section-ids)))
                                             vec)
                            raw-hood (update raw-hood :links into essay-links)
                            ;; Diagrams that are pinned (compressed view)
                            pinned-diagrams (->> (:nemas raw-hood)
                                                 (filter #(and (= "diagram" (:nema/type %))
                                                              (contains? pin-id-set (:nema/id %))))
                                                 (map :nema/id)
                                                 set)
                            ;; Diagrams that are NOT pinned (appearing as neighbours)
                            neighbour-diagrams (->> (:nemas raw-hood)
                                                    (filter #(and (= "diagram" (:nema/type %))
                                                                 (not (contains? pin-id-set (:nema/id %)))))
                                                    (map :nema/id)
                                                    set)
                            ;; Content entities of pinned diagrams (targets of diagram/includes)
                            content-ids (->> (:links raw-hood)
                                            (filter #(and (= "diagram/includes" (:link/type %))
                                                         (contains? pinned-diagrams
                                                                    (get-in % [:link/src :nema/id]))))
                                            (map #(get-in % [:link/dst :nema/id]))
                                            set)
                            ;; Hide: neighbour diagrams + content of pinned diagrams
                            hide-ids (into neighbour-diagrams content-ids)]
                        {:nemas (remove #(contains? hide-ids (:nema/id %)) (:nemas raw-hood))
                         :links (remove #(or (= "diagram/includes" (:link/type %))
                                            (contains? neighbour-diagrams (get-in % [:link/src :nema/id]))
                                            (contains? neighbour-diagrams (get-in % [:link/dst :nema/id]))
                                            ;; Also hide links to/from hidden content nodes
                                            (contains? content-ids (get-in % [:link/src :nema/id]))
                                            (contains? content-ids (get-in % [:link/dst :nema/id])))
                                        (:links raw-hood))
                         :pins (:pins raw-hood)}))
        filtered-hood (some-> merged-hood
                              (apply-kind-filter view-mode)
                              (apply-view-mode-filter view-mode zoom-k))
        hood-ids   (set (map :nema/id (:nemas filtered-hood)))
        graph-visible-ids (when (graph-filter-active?)
                            hood-ids)
        _sync-visible-cards (when (not= (:graph-visible-ids @state/ui-state)
                                        graph-visible-ids)
                              (swap! state/ui-state assoc
                                     :graph-visible-ids graph-visible-ids))
        floating   (->> scratchpad
                        (remove #(contains? hood-ids (:id %)))
                        vec)]
    (if (or (and filtered-hood (seq (:nemas filtered-hood))) (seq floating))
      (let [centres   (when (seq effective-pins) (pin-centres effective-pins))
            ;; Force-directed layout
            positions (if (and filtered-hood (seq (:nemas filtered-hood)))
                        (if (= :layered view-mode)
                          (layered-layout (:nemas filtered-hood)
                                          (:links filtered-hood)
                                          (or centres {})
                                          view-mode)
                          (force-layout (:nemas filtered-hood)
                                        (:links filtered-hood)
                                        (or centres {})
                                        view-mode))
                        {})
            _fit      (fit-on-new-graph! positions)
            nema-map  (into {} (map (fn [n] [(:nema/id n) n]) (:nemas filtered-hood)))
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
        [:div {:style {:position "relative" :width "100%" :height "100%"}}
         [:svg {:width "100%" :height "100%"
                :viewBox (str "0 0 " svg-width " " svg-height)
                :style {:background "#1a1a2e" :touch-action "none"}
                :on-wheel (fn [e]
                            (let [[px py] (svg-point e)
                                  {:keys [k x y]} @!zoom
                                  factor (if (pos? (.-deltaY e)) 0.88 1.14)
                                  k* (clamp 0.12 5.0 (* k factor))
                                  ratio (/ k* k)]
                              (reset! !zoom {:k k*
                                             :x (- px (* ratio (- px x)))
                                             :y (- py (* ratio (- py y)))
                                             :signature (:signature @!zoom)})))
                :on-pointer-down (fn [e]
                                   (.setPointerCapture (.-currentTarget e) (.-pointerId e))
                                   (reset! !drag {:client-x (.-clientX e)
                                                  :client-y (.-clientY e)
                                                  :origin @!zoom}))
                :on-pointer-move (fn [e]
                                   (when-let [{:keys [client-x client-y origin]} @!drag]
                                     (let [[dx dy] (screen-delta->svg (.-currentTarget e)
                                                                      (- (.-clientX e) client-x)
                                                                      (- (.-clientY e) client-y))]
                                       (reset! !zoom (assoc origin
                                                            :x (+ (:x origin) dx)
                                                            :y (+ (:y origin) dy))))))
                :on-pointer-up (fn [e]
                                 (reset! !drag nil)
                                 (try
                                   (.releasePointerCapture (.-currentTarget e) (.-pointerId e))
                                   (catch :default _ nil)))
                :on-pointer-leave (fn [_] (reset! !drag nil))}
          ;; Arrowhead marker definition
          [:defs
           [:marker {:id "arrowhead" :markerWidth 10 :markerHeight 8
                     :refX 9 :refY 4 :orient "auto" :markerUnits "strokeWidth"}
            [:path {:d "M0,0 L10,4 L0,8 L3,4 Z" :fill "#99aabb"}]]]
          [:g {:class "graph-zoom-layer"
               :transform (zoom-transform-attr)}
           ;; Links
           (when filtered-hood
             (doall
              (for [link (:links filtered-hood)
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
               (contains? pin-ids nema-id)]))]]
         [:div {:style {:position "absolute" :left "8px" :top "8px" :z-index 5}}
          [view-mode-selector]]
         (when (graph-filter-active?)
           [:div {:style {:position "absolute" :left "8px" :top "58px" :z-index 5}}
            [filter-controller]])
         [:div {:style {:position "absolute" :right "8px" :top "8px" :z-index 5}}
          [zoom-controls all-positions]]])
      ;; Empty state
      [:div.empty-graph
       [:p "No nema in focus."]
       [:p "Use the search bar to find a nema, or create a new one."]])))
