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
   {:id :spiral :label "S Spiral"}
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
          (= "arxana/constellation-star" nema-type)   ; cross-EOI colimit nodes
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

(defn- depth-colour
  "Colour ramp by grounding-depth: shallow = light, deep = saturated blue.
Size encodes magnitude (overall weight); colour encodes depth -- so the two
signals are visually distinct (two nodes of equal magnitude can differ in hue)."
  [depth]
  (let [d (clamp 1 7 (or depth 1))
        t (/ (- d 1) 6.0)]
    (str "hsl(205," (int (+ 45 (* 45 t))) "%," (int (- 78 (* 38 t))) "%)")))

(defn- node-fill
  [nema]
  (case (node-kind nema)
    :pattern-peer "#f59e0b"
    :figure-tag "#a78bfa"
    :interest-star (depth-colour (numeric-prop (:nema/props nema) :grounding-depth))
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
  (if (and (contains? witness-ids (:nema/id nema))
           ;; Cross-EOI colimit nodes are real interest stars, never a witness
           ;; layer -- they just happen to be the src of coalesces-into edges,
           ;; which would otherwise filter them (and all their arcs) out.
           (not= "arxana/constellation-star" (:nema/type nema)))
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

(defn- restrict-to-pins
  "Keep only the pinned core nodes and the links whose BOTH endpoints are pins.
   Drops the depth-0 'gray' neighbour ring and every edge touching it, while
   the star-to-star edges among the core survive. Used at low k (k<=2): the BFS
   still expands one hop so inter-pin edges get captured, then this strips the
   ring back off — otherwise rings=0 isolates each star and the network goes
   edgeless."
  [hood keep-ids]
  (-> hood
      (update :nemas #(filterv (fn [n] (contains? keep-ids (:nema/id n))) %))
      (update :links #(filterv (fn [l]
                                 (and (contains? keep-ids (get-in l [:link/src :nema/id]))
                                      (contains? keep-ids (get-in l [:link/dst :nema/id]))))
                               %))))

(defn- median [xs]
  (let [s (vec (sort xs)) n (count s)]
    (when (pos? n)
      (if (odd? n)
        (nth s (quot n 2))
        (/ (+ (nth s (dec (quot n 2))) (nth s (quot n 2))) 2)))))

(defn- restrict-to-super-core
  "k=1 retraction: from the pinned core, keep only the 'super-core' — the stars
   whose magnitude is strictly ABOVE the core's median magnitude — plus whatever
   is in always-keep (the focus), and the links between two kept stars. magnitude
   is the per-star coreness weight (it also drives node radius), so this answers
   'some interests are more core than others' rather than treating all pins as
   equally core. Using the median makes the cut SCALE-RELATIVE: it adapts to each
   constellation's own weight spread instead of a hardcoded threshold. Tune the
   comparator here (median -> mean -> top-N -> fixed) to widen/narrow the cut."
  [hood always-keep]
  (let [cut      (or (median (map node-magnitude (:nemas hood))) 0)
        keep?    (fn [n] (or (contains? always-keep (:nema/id n))
                             (> (node-magnitude n) cut)))
        kept-ids (into #{} (comp (filter keep?) (map :nema/id)) (:nemas hood))]
    (-> hood
        (update :nemas #(filterv keep? %))
        (update :links #(filterv (fn [l]
                                   (and (contains? kept-ids (get-in l [:link/src :nema/id]))
                                        (contains? kept-ids (get-in l [:link/dst :nema/id]))))
                                 %)))))

(defn- connected-components
  "Undirected connected components of HOOD's nodes, as a seq of id-sets. A node
   with no surviving edge is its own singleton component."
  [hood]
  (let [ids (set (map :nema/id (:nemas hood)))
        adj (reduce (fn [m l]
                      (let [s (get-in l [:link/src :nema/id])
                            d (get-in l [:link/dst :nema/id])]
                        (if (and (contains? ids s) (contains? ids d) (not= s d))
                          (-> m (update s (fnil conj #{}) d)
                                (update d (fnil conj #{}) s))
                          m)))
                    {} (:links hood))]
    (loop [unseen ids comps []]
      (if (empty? unseen)
        comps
        (let [comp (loop [stack [(first unseen)] seen #{}]
                     (if-let [x (peek stack)]
                       (if (contains? seen x)
                         (recur (pop stack) seen)
                         (recur (into (pop stack) (get adj x)) (conj seen x)))
                       seen))]
          (recur (reduce disj unseen comp) (conj comps comp)))))))

(defn- keep-largest-component
  "Drop disconnected fragments: keep the connected component containing PREFER-ID
   (the focus) when present, else the largest by node count. Singletons and small
   islands fall away. Used at k=1 so the super-core reads as one coherent cluster
   instead of a scatter of stars with stragglers."
  [hood prefer-id]
  (let [comps (connected-components hood)]
    (if (empty? comps)
      hood
      (let [target (or (some #(when (contains? % prefer-id) %) comps)
                       (apply max-key count comps))]
        (-> hood
            (update :nemas #(filterv (fn [n] (contains? target (:nema/id n))) %))
            (update :links #(filterv (fn [l]
                                       (and (contains? target (get-in l [:link/src :nema/id]))
                                            (contains? target (get-in l [:link/dst :nema/id]))))
                                     %)))))))

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
    (let [margin 60
          bw (max 1 (- max-x min-x))
          bh (max 1 (- max-y min-y))
          ;; Don't shrink a large graph into a tiny rectangle. Clamp the fit
          ;; near natural scale: big constellations render full-size and
          ;; OVERFLOW the viewport, so wheel-zoom + drag-pan are meaningful;
          ;; only small graphs get gently scaled up to fill.
          k (clamp 0.08 1.8 (min (/ (- svg-width (* 2 margin)) bw)
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

(defn- fit-on-new-graph! [positions fit-key]
  ;; Fit ONCE per graph, keyed by a STABLE id (the diagram/focus) -- NOT the pin
  ;; set, which churns every render and would re-fit constantly, reverting the
  ;; user's zoom/pan. wheel/pan preserve :fitted-for so they never trigger a fit.
  (when (and (seq positions) (not= fit-key (:fitted-for @!zoom)))
    (swap! !zoom merge (fit-transform positions) {:fitted-for fit-key})))

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
        [:option {:value (name id)} label])]
     ;; Mission-scope fold toggle (k=2 main scopes ⇄ k=3 all subscopes): re-fetch the
     ;; focus at a new depth; reset the graph first so collapsing drops the subscopes.
     (when (:focus-name @state/ui-state)
       [:button
        {:on-click (fn [_]
                     (let [d (get @state/ui-state :scope-fold-depth 1)]
                       (swap! state/ui-state assoc :scope-fold-depth (if (= d 1) 3 1))
                       (state/reset-graph!)
                       (api/browse-and-focus! (:focus-name @state/ui-state)
                                              (:focus-id @state/ui-state))))
         :title "Mission-scope fold: main scopes only ⇄ all subscopes"
         :style {:background "#0f172a" :color "#e5e7eb"
                 :border "1px solid rgba(148,163,184,0.45)"
                 :border-radius "4px" :font-size "12px" :padding "4px 8px"
                 :cursor "pointer"}}
        (if (= 1 (get @state/ui-state :scope-fold-depth 3)) "show all (k3)" "fold to main (k2)")])]))

;; --- Force-directed layout ---

(defn- force-layout
  "Compute positions using d3-force simulation.
   Edge midpoints are included as phantom nodes for label spacing.
   Returns {nema-id [x y]}."
  [nemas links pin-centres view-mode seed-positions]
  (let [witness-ids (witness-node-ids links)
        ;; Seeding policy (two things matter):
        ;; 1. NEVER seed from the pin grid -- a grid seed biases the sim into a
        ;;    grid-shaped local minimum that ignores connectivity (an
        ;;    articulation node ends stranded on the rim instead of pulled
        ;;    interior). 2. DO seed from the PREVIOUS layout's positions: the
        ;;    expanded diagram streams ~35 pins in one at a time, recomputing on
        ;;    each arrival; without a prior seed every recompute phyllotaxis-
        ;;    reshuffles the whole graph (the "flashed then reshaped" churn).
        ;;    Seeding existing nodes at their last position keeps them put while
        ;;    only the new node settles in -- and it's a FORCE seed, not a grid,
        ;;    so it preserves real structure. Brand-new nodes seed near the prior
        ;;    centroid (not d3's origin) so they don't yank the cloud around.
        seeds (or seed-positions {})
        svals (vals seeds)
        seed-cx (when (seq svals) (/ (reduce + (map first svals)) (count svals)))
        seed-cy (when (seq svals) (/ (reduce + (map second svals)) (count svals)))
        real-nodes (vec (map-indexed
                         (fn [i n]
                           (let [nid  (:nema/id n)
                                 seed (get seeds nid)]
                             (cond-> {:id nid
                                      :kind (name (effective-node-kind witness-ids n))
                                      :radius (+ 10 (node-radius n false false))}
                               seed (assoc :x (first seed) :y (second seed))
                               (and (not seed) seed-cx)
                               (assoc :x (+ seed-cx (* 40 (js/Math.cos i)))
                                      :y (+ seed-cy (* 40 (js/Math.sin i)))))))
                         nemas))
        ;; Only links whose BOTH endpoints are real sim nodes. d3 forceLink
        ;; resolves each link's source/target against the node set; a link to a
        ;; filtered-out node (e.g. a coalesces-into target not on the canvas)
        ;; leaves the ref undefined and poisons positions with NaN — which makes
        ;; ALL edges (and nodes) vanish once the sim ticks. This was the
        ;; "edges disappear as the nodes bounce" bug.
        node-ids (set (map :id real-nodes))
        valid-links (filter (fn [link]
                              (let [s (get-in link [:link/src :nema/id])
                                    d (get-in link [:link/dst :nema/id])]
                                (and s d (contains? node-ids s) (contains? node-ids d))))
                            links)
        ;; Phantom nodes for edge label midpoints (prevents label overlap)
        phantom-nodes (keep (fn [link]
                              (let [lid (:link/id link)]
                                (when lid
                                  {:id (str "phantom:" lid) :radius 25})))
                            valid-links)
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
                         valid-links)
        all-edges (clj->js (vec (mapcat identity real-edges)))
        banded? (= :witnesses view-mode)
        n (count real-nodes)
        ;; Organic spread: a single radius grows with node count so the
        ;; constellation settles into a roughly CIRCULAR cloud (charge repulsion
        ;; balanced by centre gravity) -- NOT a fixed landscape rectangle. The
        ;; 1200x800 viewBox is a CAMERA over this world (the zoom-transform <g>),
        ;; NOT its bounds: the initial fit zooms OUT to frame the cloud and
        ;; wheel-zoom + drag-pan navigate. Banded/witness mode keeps the fixed
        ;; canvas so its y-bands stay put.
        spread (max 480 (* 175 (js/Math.sqrt (max 1 n))))
        ;; Centre of usable area (leave room for cards on right)
        cx (if banded? (* svg-width 0.42) spread)
        cy (if banded? (/ svg-height 2) spread)
        ;; Create simulation
        sim (-> (d3/forceSimulation all-sim-nodes)
                (.force "charge" (-> (d3/forceManyBody)
                                     (.strength (if banded? -450 -900))
                                     (.distanceMax (if banded? 900 (* 1.2 spread)))))
                (.force "link" (-> (d3/forceLink all-edges)
                                   (.id (fn [d] (.-id d)))
                                   (.distance (if banded? 125 150))
                                   (.strength (if banded? 0.45 0.6))))
                (.force "center" (d3/forceCenter cx cy))
                (.force "collide" (.radius (d3/forceCollide)
                                           (fn [d] (or (.-radius d) 30))))
                (.force "x" (.strength (d3/forceX cx) 0.02))
                (.force "y" (.strength (d3/forceY (if banded?
                                                     (fn [d]
                                                       (if (.startsWith (.-id d) "phantom:")
                                                         cy
                                                         (band-y (keyword (.-kind d)) view-mode)))
                                                     cy))
                                       (if banded? 0.2 0.02)))
                (.stop))]
    ;; Run simulation
    (dotimes [_ 300]
      (.tick sim))
    ;; Extract positions for real nodes only (skip phantoms)
    (into {}
          (keep (fn [node]
                  (let [nid (.-id node)]
                    (when-not (.startsWith nid "phantom:")
                      (let [x (.-x node) y (.-y node)]
                        (if banded?
                          ;; Banded mode keeps the original rectangular clamp.
                          [nid [(max 50 (min (- svg-width 50) x))
                                (max 50 (min (- svg-height 50) y))]]
                          ;; Organic mode: NO geometric clamp -- the symmetric
                          ;; centre gravity already bounds the cloud into a round
                          ;; blob. A hard radial clamp would pin stray nodes onto
                          ;; a ring, which during incremental load read as the
                          ;; layout "reshaping into a circle". Only guard NaN.
                          [nid [(if (js/isFinite x) x cx)
                                (if (js/isFinite y) y cy)]]))))))
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

(def ^:private label-tiers
  "Zoom-bucketed label policy (high-zoom -> low-zoom). Bucketing matters: labels
   only re-render when zoom CROSSES a boundary, not on every wheel event, so
   continuous zooming stays cheap. As you zoom OUT, fonts counter-scale up
   (:scale) to stay legible, and lettering drops from smaller nodes/edges
   (:node-mag threshold; :edges?) so labels don't overlap."
  ;; :node-mag is the MIN magnitude a node needs to keep its label; the depth-0
  ;; "gray" neighbour nodes have magnitude 0, so the top tier must use 0 or they
  ;; would never be labelled at all. Thresholds peel labels off lowest-magnitude
  ;; first as you zoom out.
  [{:min-k 0.85 :scale 1.0  :node-mag 0 :edges? true}
   {:min-k 0.55 :scale 1.35 :node-mag 1 :edges? true}
   {:min-k 0.38 :scale 1.8  :node-mag 3 :edges? false}
   {:min-k 0.0  :scale 2.4  :node-mag 5 :edges? false}])

(defn- label-config
  "Label policy for zoom K. Returns the SAME map object within a tier so reagent
   skips re-rendering labels until a boundary is crossed."
  [k]
  (or (some (fn [t] (when (>= k (:min-k t)) t)) label-tiers)
      (last label-tiers)))

(defn- label-group-transform
  "Scale a label group by S about anchor (ax,ay) so lettering grows around the
   node/edge rather than drifting."
  [ax ay s]
  (str "translate(" ax "," ay ") scale(" s ") translate(" (- ax) "," (- ay) ")"))

(defn- ellipsise [s n]
  (if (> (count s) n) (str (str/trimr (subs s 0 (max 0 (dec n)))) "…") s))

(defn- greedy-lines
  "Greedily pack WORDS into lines of <= per-line chars, breaking on spaces."
  [words per-line]
  (loop [[w & more] words, cur "", lines []]
    (if (nil? w)
      (cond-> lines (seq cur) (conj cur))
      (let [cur* (if (seq cur) (str cur " " w) w)]
        (cond
          (<= (count cur*) per-line) (recur more cur* lines)
          (seq cur)                  (recur (cons w more) "" (conj lines cur))
          :else                      (recur more w lines)))))) ; lone long word

(defn- wrap-label
  "Word-wrap LABEL into at most MAX-LINES lines of ~PER-LINE chars. Shows the
   full title when it fits (no aggressive truncation); ellipsises only if the
   text overflows MAX-LINES lines."
  [label per-line max-lines]
  (let [words (remove str/blank? (str/split (str (or label "")) #"\s+"))
        lines (greedy-lines words per-line)]
    (if (<= (count lines) max-lines)
      lines
      (conj (vec (take (dec max-lines) lines))
            (ellipsise (str/join " " (drop (dec max-lines) lines)) per-line)))))

(defn link-component
  [link src-pos dst-pos label-cfg]
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
     ;; Edge label: hidden when zoomed out (edges are the densest clutter), and
     ;; counter-scaled up about its midpoint when shown so it stays legible.
     (when (:edges? label-cfg)
       [:g {:transform (label-group-transform mx my (:scale label-cfg))
            :on-click (fn [e]
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
         label]])]))

(defn- reveal-pinned-card!
  "Scroll the pinned card for `nema-id` into view in the right-hand rail.
   Deferred to the next frame so it runs AFTER the focus swap! re-renders the
   rail (and the `active-pin` class lands); scrolls to centre instantly."
  [nema-id]
  (js/requestAnimationFrame
   (fn []
     (when-let [el (.querySelector js/document
                                   (str ".focus-card[data-pin-id=\"" nema-id "\"]"))]
       (.scrollIntoView el #js {:block "center" :behavior "smooth"})))))

(defn node-component
  [nema pos is-focus is-pin label-cfg]
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
         ;; Stop pointerdown bubbling to the SVG: otherwise the SVG's drag-pan
         ;; handler setPointerCaptures the pointer and the browser dispatches the
         ;; click to the SVG instead of this node -- so node clicks were silently
         ;; lost. Stopping it here keeps pan on the empty canvas while letting
         ;; node clicks (select/focus) through.
         :on-pointer-down (fn [e] (.stopPropagation e))
         :on-click (fn []
                     (if connecting
                       (api/connect-nodes! (:node-id connecting) nema-id nil)
                       ;; Single click raises this node's card on the right:
                       ;; pin! adds it to the pinned-card rail AND focuses it,
                       ;; then reveal-pinned-card! scrolls that card into view
                       ;; (focus only adds an `active-pin` highlight; the long
                       ;; rail would otherwise not move to show it).
                       (do (state/pin! nema-id)
                           (reveal-pinned-card! nema-id))))
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
     ;; Full name + weight breakdown on hover (the on-canvas label is truncated;
     ;; this native SVG tooltip is the un-truncated read).
     [:title (let [m (numeric-prop props :multiplicity)
                   d (numeric-prop props :grounding-depth)]
               (str nema-name
                    (when (and m d)
                      (str "  —  magnitude " magnitude
                           "  (multiplicity " m " × depth "
                           (/ (js/Math.round (* d 10)) 10.0) ")"))))]
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
     ;; Type badge + name, grouped so they counter-scale together about the node
     ;; as you zoom out, and dropped from smaller-magnitude nodes when zoomed out
     ;; so lettering doesn't overlap. Focus/pinned nodes always keep their label.
     (when (or is-focus is-pin (= nema-type "scope/frame")
               (>= magnitude (:node-mag label-cfg)))
       [:g {:transform (label-group-transform x y (:scale label-cfg))}
        ;; Type badge — interest-stars omit it: their magnitude is already the
        ;; node's size + hover tooltip, so an "m5"/"m0" badge is redundant
        ;; internal clutter. Other kinds keep a meaningful badge.
        (when-let [badge (case kind
                           :pattern-peer "design-pattern"
                           :interest-star nil
                           (if folded-count
                             (str folded-count " concepts")
                             nema-type))]
          [:text {:x x :y (- y 6) :text-anchor "middle"
                  :fill "#ffffff" :font-size 11 :font-family "monospace"
                  :opacity 0.7}
           badge])
        ;; Name label — word-wrapped over up to 2 lines (full title when it
        ;; fits; ellipsised only if it overflows both lines), as <tspan>s since
        ;; SVG <text> does not wrap on its own. The block is vertically CENTRED
        ;; on the node (baseline of the first line shifts up with line count) so
        ;; it doesn't hang low now that the magnitude badge above it is gone.
        (let [lines  (wrap-label nema-name 22 2)
              line-h 14
              y0     (+ y 4 (* (/ (dec (count lines)) -2.0) line-h))]
          (into [:text {:x x :y y0 :text-anchor "middle"
                        :fill "#ffffff" :font-size 13 :font-weight "bold"
                        :font-family "sans-serif"}]
                (map-indexed
                 (fn [i ln]
                   ^{:key i}
                   [:tspan {:x x :dy (if (zero? i) "0" "1.1em")} ln])
                 lines)))])]))

(defn- scope-frame-dominated?
  "True when the hood is mostly scope/frame nodes — i.e. a single mission's scope
   view, where the spiral reads far better than the force-ring."
  [hood]
  (let [nemas (:nemas hood)
        n (count nemas)
        sc (count (filter #(= "scope/frame" (:nema/type %)) nemas))]
    (and (pos? n) (>= sc (* 0.5 n)))))

(defn- catmull-rom-segments
  "Smooth Catmull-Rom spline as one map per segment: {:d <cubic-bezier> :p2 <end-xy>
   :tan <unit tangent into p2>}.  Each segment can carry its own colour (gradient along
   the curve); the final segment's tangent orients the arrowhead.  K is the tension divisor
   — smaller = more pronounced, orbital winding (classical mechanics, not graph edges)."
  [points k]
  (let [pts (vec points) n (count pts)]
    (when (>= n 2)
      (vec
       (for [i (range 0 (dec n))]
         (let [[x0 y0] (nth pts (max 0 (dec i)))
               [x1 y1] (nth pts i)
               [x2 y2] (nth pts (inc i))
               [x3 y3] (nth pts (min (dec n) (+ i 2)))
               c2x (- x2 (/ (- x3 x1) k)) c2y (- y2 (/ (- y3 y1) k))
               dx (- x2 c2x) dy (- y2 c2y)
               len (js/Math.max 0.001 (js/Math.sqrt (+ (* dx dx) (* dy dy))))]
           {:d (str "M" x1 "," y1 " C" (+ x1 (/ (- x2 x0) k)) "," (+ y1 (/ (- y2 y0) k))
                    " " c2x "," c2y " " x2 "," y2)
            :p2 [x2 y2]
            :tan [(/ dx len) (/ dy len)]}))))))

(defn- catmull-rom-d
  "The whole orbit as ONE svg path-d string (segments chained) — for the wide grace hit-area
   and the hover halo, which want a single continuous stroke rather than per-segment paths."
  [pts k]
  (let [segs (catmull-rom-segments pts k)]
    (when (seq segs)
      (str (:d (first segs))
           (apply str (map (fn [s] (str " " (subs (:d s) (str/index-of (:d s) "C"))))
                           (rest segs)))))))

(defn- orbit-hsl
  "Colour for one thread's curve: HUE identifies the thread; T∈[0,1] is turn-time, ramping
   lightness dim (earlier turns) → bright (now), so every curve also reads its own direction."
  [hue t]
  (str "hsl(" hue ", 85%, " (js/Math.round (+ 40 (* 25 t))) "%)"))

(defn- draw-orbit
  "One thread's integral curve on the scope-surface: a smooth spline whose colour ramps dim→bright
   along turn-time, an arrowhead at the latest turn, and a station dot per scope.  KP keys siblings.
   OI is the orbit index (for hover); HOVERED? haloes it; DIM? fades it (another is hovered)."
  [pts hue kp oi hovered? dim?]
  (let [segs (catmull-rom-segments pts 4.0)
        full (catmull-rom-d pts 4.0)
        ns   (count segs)
        np   (count pts)
        {[ex ey] :p2 [tx ty] :tan} (last segs)
        bx (- ex (* tx 4)) by (- ey (* ty 4))
        arrow (str (+ ex (* tx 8)) "," (+ ey (* ty 8)) " "
                   (- bx (* ty 5)) "," (+ by (* tx 5)) " "
                   (+ bx (* ty 5)) "," (- by (* tx 5)))]
    (into [:g {:key kp
               :on-mouse-enter (fn [_] (reset! state/!orbit-hover oi))
               :on-mouse-leave (fn [_] (reset! state/!orbit-hover nil))
               :style {:cursor "pointer" :opacity (if dim? 0.12 1) :transition "opacity 0.18s"}}
           ;; grace hit-area — a fat INVISIBLE stroke so the mouse has a few px of margin off the line
           [:path {:key (str kp "hit") :d full :fill "none" :stroke "transparent"
                   :stroke-width 16 :style {:pointer-events "stroke"}}]
           ;; halo — a soft glow under the line while THIS orbit is hovered
           (when hovered?
             [:path {:key (str kp "halo") :d full :fill "none" :stroke (orbit-hsl hue 0.95)
                     :stroke-width 9 :opacity 0.45 :stroke-linecap "round" :stroke-linejoin "round"
                     :style {:filter "blur(2.5px)" :pointer-events "none"}}])
           [:polygon {:key (str kp "a") :points arrow :fill (orbit-hsl hue 1.0) :opacity 0.95
                      :style {:pointer-events "none"}}]]
          (concat
           ;; the trajectory, gradient along its whole length
           (map-indexed
            (fn [i s]
              (let [t (/ i (max 1 (dec ns)))]
                ^{:key (str kp "s" i)}
                [:path {:d (:d s) :fill "none" :stroke (orbit-hsl hue t)
                        :stroke-width (+ 1.2 (* 1.3 t)) :opacity 0.9
                        :stroke-linejoin "round" :stroke-linecap "round"}]))
            segs)
           ;; the stations — earlier small/dim, latest large/bright
           (map-indexed
            (fn [i [x y]]
              (let [t (/ i (max 1 (dec np)))]
                ^{:key (str kp "n" i)}
                [:circle {:cx x :cy y :r (+ 2 (* 1.8 t)) :fill (orbit-hsl hue t) :opacity 0.92}]))
            pts)))))

(defn- orbit-layer
  "Draw the FULL phase portrait: every recurrent thread that engages the mission, retracted onto the
   scope-surface as its own integral curve (draw-orbit).  One thread is a sliver; the family is the
   coverage — the scopes the session actually swept while clocked near the mission.  Hue per thread
   (golden-angle spacing), dim→bright along each curve's turn-time, arrowhead toward NOW."
  [all-positions]
  (api/fetch-orbits!)
  (let [o @state/!orbits
        hover @state/!orbit-hover
        gk (fn [m k] (or (get m k) (get m (keyword k))))]
    (when (map? o)
      (into [:g {:class "thread-orbits" :style {:pointer-events "none"}}]
            (keep-indexed
             (fn [oi ob]
               (let [pts (vec (keep (fn [p] (get all-positions (gk p "scope"))) (gk ob "orbit")))
                     hue (mod (js/Math.round (* oi 137.5)) 360)
                     hovered? (= hover oi)
                     dim? (and (number? hover) (not hovered?))]
                 (when (> (count pts) 1)
                   (draw-orbit pts hue (str "o" oi) oi hovered? dim?))))
             (gk o "orbits"))))))

(defn- orbit-card
  "Left-side explainer for the hovered orbit: what this thread is (its pattern + sigil), how
   often it recurred, and the scopes it winds through.  Nil when nothing is hovered."
  []
  (let [hi @state/!orbit-hover
        o  @state/!orbits
        gk (fn [m k] (or (get m k) (get m (keyword k))))]
    (when (and (number? hi) (map? o))
      (when-let [ob (nth (vec (gk o "orbits")) hi nil)]
        (let [sg  (gk ob "sigil")
              hue (mod (js/Math.round (* hi 137.5)) 360)
              scopes (distinct (keep (fn [p] (gk p "name")) (gk ob "orbit")))]
          [:div {:style {:position "absolute" :left "14px" :top "64px" :width "300px"
                         :background "rgba(26,26,46,0.96)" :color "#e2e8f0"
                         :border (str "2px solid " (orbit-hsl hue 0.7)) :border-radius "8px"
                         :padding "12px 14px" :font-size "13px" :line-height "1.45"
                         :pointer-events "none" :z-index 30 :box-shadow "0 6px 20px rgba(0,0,0,0.55)"}}
           [:div {:style {:font-size "15px" :font-weight "bold" :margin-bottom "4px"}}
            (str "〘 " (or (gk sg "truth") "·") " " (or (gk sg "okipona") "") " 〙")]
           [:div {:style {:color (orbit-hsl hue 0.65) :font-weight "bold" :margin-bottom "8px"}}
            (gk ob "pattern")]
           [:div {:style {:margin-bottom "8px"}}
            (str "A recurrent thread — this pattern was retrieved across "
                 (gk ob "recurrence") " turns, drawn as the trajectory it traced through the "
                 "scopes it engaged (dim → bright = earlier → now, arrow = latest turn).")]
           ;; campaign-scale: which AGENTS' sessions this thread spans
           (when-let [ss (gk ob "sessions")]
             [:div {:style {:font-size "11px" :color "#a5b4fc" :margin-bottom "6px"}}
              "spans sessions: " (str/join " + " ss)])
           [:div {:style {:font-size "11px" :color "#94a3b8"}}
            "winds through: " (str/join "  →  " (take 6 scopes))]])))))

(defn- spiral-layout
  "Futon-City idiom: the hub (highest-degree node — a mission is the hub of its
   scope star) sits at centre, the rest spiral OUT around it on a phyllotaxis
   (golden-angle) spiral.  Deterministic positions, no force sim — the surface a
   thread-trajectory will later be drawn onto.  Returns {nema-id [x y]}."
  [nemas links _centres _view-mode]
  (let [cx (* svg-width 0.42)
        cy (/ svg-height 2)
        deg (frequencies (mapcat (fn [l] [(get-in l [:link/src :nema/id])
                                          (get-in l [:link/dst :nema/id])])
                                 links))
        hub-id (when (seq nemas)
                 (:nema/id (apply max-key #(get deg (:nema/id %) 0) nemas)))
        others (remove #(= hub-id (:nema/id %)) nemas)
        ;; Phyllotaxis (golden-angle) sunflower spiral — the Futon-City idiom Joe
        ;; preferred.  STEP is the nearest-neighbour gap; sized to clear the wide
        ;; scope labels (edges/edge-labels are hidden for this view, so only node
        ;; labels need room).
        golden (* js/Math.PI (- 3 (js/Math.sqrt 5)))   ; ~2.399 rad
        step 130]
    (into (if hub-id {hub-id [cx cy]} {})
          (map-indexed
           (fn [i n]
             (let [t (inc i)
                   r (* step (js/Math.sqrt t))
                   a (* t golden)]
               [(:nema/id n) [(+ cx (* r (js/Math.cos a)))
                              (+ cy (* r (js/Math.sin a)))]]))
           others))))

(defonce ^:private !layout-cache (atom {:sig nil :positions {}}))

(defn- cached-positions
  "Memoise the expensive (250-tick) force/layered layout: recompute only when the
   node/link set, pin centres, or view-mode change -- not on every re-render.
   Zoom, hover and incremental pin-loads re-render but must not re-run the sim."
  [filtered-hood centres view-mode]
  (if (and filtered-hood (seq (:nemas filtered-hood)))
    (let [sig [view-mode (or centres {})
               (sort (map :nema/id (:nemas filtered-hood)))
               (sort (keep :link/id (:links filtered-hood)))]]
      (if (= sig (:sig @!layout-cache))
        (:positions @!layout-cache)
        ;; Seed the next sim from the PREVIOUS layout's positions so an
        ;; incremental pin-load nudges the existing graph instead of
        ;; phyllotaxis-reshuffling it from scratch on every arrival.
        (let [prev (:positions @!layout-cache)
              pos (cond
                    (= :spiral view-mode)
                    (spiral-layout (:nemas filtered-hood) (:links filtered-hood) (or centres {}) view-mode)
                    (= :layered view-mode)
                    (layered-layout (:nemas filtered-hood) (:links filtered-hood) (or centres {}) view-mode)
                    :else
                    (force-layout  (:nemas filtered-hood) (:links filtered-hood) (or centres {}) view-mode prev))]
          (reset! !layout-cache {:sig sig :positions pos})
          pos)))
    {}))

(def ^:private legible-zoom
  "Camera scale used when flying to a node: large enough that the node's label
   (font-size 13 in world units) reads comfortably."
  1.5)

(def ^:private wheel-zoom-sensitivity
  "Exponential zoom rate per pixel of wheel delta. Lower = gentler. ~0.0015
   gives ~14% per mouse notch while keeping trackpad gestures controllable."
  0.0015)

(defn- tween-zoom!
  "Animate !zoom toward TARGET ({:k :x :y}) over MS milliseconds, ease-out
   cubic. Uses swap!/assoc so :fitted-for is preserved (the auto-fit won't
   revert the move)."
  [target ms]
  (let [start @!zoom
        t0 (js/performance.now)
        lerp (fn [a b e] (+ a (* (- b a) e)))]
    (letfn [(step [_]
              (let [p (min 1 (/ (- (js/performance.now) t0) ms))
                    e (- 1 (js/Math.pow (- 1 p) 3))]
                (swap! !zoom assoc
                       :k (lerp (:k start) (:k target) e)
                       :x (lerp (:x start) (:x target) e)
                       :y (lerp (:y start) (:y target) e))
                (when (< p 1)
                  (js/requestAnimationFrame step))))]
      (js/requestAnimationFrame step))))

(defn center-on-node!
  "Fly the camera so NEMA-ID's node sits centred in the canvas (left of the
   card rail, matching the fit's 0.42 bias) at a legible zoom. No-op if the
   node has no computed position yet."
  [nema-id]
  (when-let [pos (get-in @!layout-cache [:positions nema-id])]
    (let [[nx ny] pos
          k legible-zoom]
      (tween-zoom! {:k k
                    :x (- (* svg-width 0.42) (* k nx))
                    :y (- (/ svg-height 2) (* k ny))}
                   320))))

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
        hop-depth  (:hop-depth @state/ui-state)
        ;; Pinned graph (e.g. an expanded diagram): the toolbar k controls how
        ;; many neighbour rings expand around the pinned core. Offset by 2 so the
        ;; default k=3 keeps the familiar 1-ring view; k<=2 drops the neighbour
        ;; ring (the depth-0 "gray" nodes) + their edges. We still expand ONE hop
        ;; at low k so star-to-star edges among the core get captured by the BFS
        ;; (both endpoints must be visited for a link to surface), then strip the
        ;; gray ring back off via pins-only? below — rings=0 would isolate every
        ;; star and the whole network would go edgeless. k=1 retracts further to
        ;; the super-core (above-median-magnitude stars) via super-core? below.
        pins-only?  (and (seq pins) (<= hop-depth 2))
        super-core? (and (seq pins) (<= hop-depth 1))
        effective-pins (cond
                         (seq pins) (let [rings (max 1 (- hop-depth 2))]
                                      (mapv #(assoc % :k rings) pins))
                         focus-id   [{:id focus-id :k hop-depth}])
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
                            active-diagram-id (get-in @state/ui-state [:diagram-route :diagram-id])
                            expanded-diagram? (= :expanded (get-in @state/ui-state [:diagram-route :mode]))
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
                            ;; Core entities of pinned diagrams stay visible in compressed mode.
                            ;; This is the reduced-star layer: a diagram/core edge says "show this
                            ;; as part of the cone", while diagram/includes remains the unfurl layer.
                            core-ids (->> (:links raw-hood)
                                          (filter #(and (= "diagram/core" (:link/type %))
                                                       (contains? pinned-diagrams
                                                                  (get-in % [:link/src :nema/id]))))
                                          (map #(get-in % [:link/dst :nema/id]))
                                          set)
                            ;; Content entities of pinned diagrams (targets of diagram/includes)
                            ;; are hidden in compressed mode unless also explicitly marked core.
                            content-ids (->> (:links raw-hood)
                                            (filter #(and (= "diagram/includes" (:link/type %))
                                                         (contains? pinned-diagrams
                                                                    (get-in % [:link/src :nema/id]))))
                                            (map #(get-in % [:link/dst :nema/id]))
                                            (remove core-ids)
                                            set)
                            ;; Hide: neighbour diagrams + non-core content of pinned diagrams,
                            ;; AND the pinned diagram apexes themselves. A diagram node is a
                            ;; container / cone-apex: whenever its contents are in view it is the
                            ;; FRAME, never a peer node in its own contents (Joe 2026-06-04) — in
                            ;; every mode/route (compressed, expanded, and ad-hoc /pins). It would
                            ;; only render as a peer inside a diagram-OF-diagrams. Hiding the apex
                            ;; also drops its diagram/core + diagram/includes hub-spokes via the
                            ;; links filter below (which removes links touching hidden nodes).
                            hide-ids (into (into neighbour-diagrams content-ids) pinned-diagrams)]
                        {:nemas (remove #(contains? hide-ids (:nema/id %)) (:nemas raw-hood))
                         :links (remove #(or (= "diagram/includes" (:link/type %))
                                            (and expanded-diagram?
                                                 (= "diagram/core" (:link/type %))
                                                 (= active-diagram-id (get-in % [:link/src :nema/id])))
                                            (contains? neighbour-diagrams (get-in % [:link/src :nema/id]))
                                            (contains? neighbour-diagrams (get-in % [:link/dst :nema/id]))
                                            ;; Also hide links to/from hidden content/container nodes.
                                            (contains? hide-ids (get-in % [:link/src :nema/id]))
                                            (contains? hide-ids (get-in % [:link/dst :nema/id])))
                                        (:links raw-hood))
                         :pins (:pins raw-hood)}))
        filtered-hood (some-> merged-hood
                              (cond-> pins-only?
                                (restrict-to-pins (cond-> pin-id-set
                                                    focus-id (conj focus-id))))
                              (cond-> super-core?
                                (-> (restrict-to-super-core (if focus-id #{focus-id} #{}))
                                    (keep-largest-component focus-id)))
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
            ;; Layout: a mission-scope view (mostly scope/frame nodes) defaults to
            ;; the spiral instead of the force-ring; the View dropdown still overrides.
            ;; Such a view also HIDES edges — they all carry the same
            ;; "mission-scope/folded-frame" label (pure clutter); the spiral conveys
            ;; the structure on its own.
            scope-view? (scope-frame-dominated? filtered-hood)
            effective-vm (if (and (= :organic view-mode) scope-view?)
                           :spiral view-mode)
            positions (cached-positions filtered-hood centres effective-vm)
            _fit      (fit-on-new-graph! positions
                                         (or (get-in @state/ui-state [:diagram-route :diagram-id])
                                             focus-id "adhoc"))
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
         (when scope-view? (orbit-card))
         [:svg {:width "100%" :height "100%"
                :viewBox (str "0 0 " svg-width " " svg-height)
                :style {:background "#1a1a2e" :touch-action "none"}
                :on-wheel (fn [e]
                            (let [[px py] (svg-point e)
                                  {:keys [k x y]} @!zoom
                                  ;; Zoom by wheel-delta MAGNITUDE (exponential)
                                  ;; so a gesture's total zoom depends on total
                                  ;; scroll distance, not the NUMBER of wheel
                                  ;; events -- trackpad momentum fires dozens and
                                  ;; a fixed per-event factor compounded to
                                  ;; "outer space" in one flick. Normalise across
                                  ;; deltaMode (lines/pages -> ~px); clamp so one
                                  ;; freak delta can't jump.
                                  unit (case (.-deltaMode e) 1 16 2 400 1)
                                  dy (* (.-deltaY e) unit)
                                  factor (js/Math.exp
                                          (clamp -0.25 0.25 (* (- wheel-zoom-sensitivity) dy)))
                                  k* (clamp 0.06 5.0 (* k factor))
                                  ratio (/ k* k)]
                              ;; swap!/assoc (not reset!) keeps :fitted-for, so the
                              ;; auto-fit won't immediately revert the user's zoom.
                              (swap! !zoom assoc
                                     :k k*
                                     :x (- px (* ratio (- px x)))
                                     :y (- py (* ratio (- py y))))))
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
           ;; Links — hidden for scope views (identical "mission-scope" labels = clutter).
           (when (and filtered-hood (not scope-view?))
             (doall
              (for [link (:links filtered-hood)
                    :let [src-id (get-in link [:link/src :nema/id])
                          dst-id (get-in link [:link/dst :nema/id])
                          src-pos (get all-positions src-id)
                          dst-pos (get all-positions dst-id)]
                    :when (and src-pos dst-pos)]
                ^{:key (:link/id link)}
                [link-component link src-pos dst-pos (label-config zoom-k)])))
           ;; Thread orbit (classical-mechanics trajectory) — scope views only, under the nodes.
           (when scope-view? (orbit-layer all-positions))
           ;; Nodes
           (doall
            (for [[nema-id pos] all-positions
                  :let [nema (get all-nemas nema-id)]
                  :when nema]
              ^{:key nema-id}
              [node-component nema pos
               (= nema-id focus-id)
               (contains? pin-ids nema-id)
               (label-config zoom-k)]))]]
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
