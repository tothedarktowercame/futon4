(ns webarxana.client.interest-network
  "M-interest-network-coupling step (d): the live interest-network surface.
   Fetches /api/interest-network (essays × territories bipartite REDUCE merged
   with the lived interest-event posterior from XTDB) and renders it as a
   force-directed graph: territory/essay nodes sized by degree, event-touched
   entities coloured by current standing, bipartite fit/friction edges + the
   link/asserted event edges. Models on mission-search.cljs."
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [webarxana.client.api :as api]
            [webarxana.client.state :as state]
            ["d3-force" :as d3]))

(def svg-width 1100)
(def svg-height 760)

(defn- short-id [s] (last (str/split (str s) #"/")))

(defn- node-radius [n]
  (let [deg (double (:degree n 1))]
    (case (:kind n)
      "territory" (+ 7 (* 2.0 (js/Math.sqrt (max 1.0 deg))))
      "essay"     (+ 12 (* 2.2 (js/Math.sqrt (max 1.0 deg))))
      ;; event-posterior / ref nodes
      16)))

(defn- standing-color [s]
  (case s
    "strengthened" "#16a34a"
    "refined"      "#d97706"
    "addressed"    "#06b6d4"
    "falsified"    "#dc2626"
    "foreclosed"   "#6b7280"
    "reopened"     "#a855f7"
    "#f59e0b"))

(defn- node-color [n]
  (cond
    (:standing n)             (standing-color (:standing n))
    (= "territory" (:kind n)) "#2563eb"
    (= "essay" (:kind n))     "#7c3aed"
    (= "ref" (:kind n))       "#0ea5e9"
    :else                     "#64748b"))

(defn- with-edge-endpoints
  "Ensure every edge endpoint is a node (e.g. the buyer node referenced only by
   the link/asserted edge) so the layout + render include it."
  [nodes edges]
  (let [ids (set (map :id nodes))
        missing (->> edges
                     (mapcat (juxt :src :dst))
                     (remove nil?)
                     (remove ids)
                     distinct
                     (map (fn [id] {:id id :label (short-id id) :kind "ref"})))]
    (into (vec nodes) missing)))

(defn- force-layout [nodes edges]
  (let [sim-nodes (clj->js (mapv (fn [n] {:id (:id n) :radius (node-radius n)}) nodes))
        sim-links (clj->js (vec (keep (fn [e] (when (and (:src e) (:dst e))
                                                {:source (:src e) :target (:dst e)}))
                                      edges)))
        cx (/ svg-width 2) cy (/ svg-height 2)
        sim (-> (d3/forceSimulation sim-nodes)
                (.force "charge" (-> (d3/forceManyBody) (.strength -170) (.distanceMax 340)))
                (.force "link" (-> (d3/forceLink sim-links)
                                   (.id (fn [d] (.-id d)))
                                   (.distance 70) (.strength 0.35)))
                (.force "center" (d3/forceCenter cx cy))
                (.force "collide" (.radius (d3/forceCollide) (fn [d] (+ 5 (or (.-radius d) 12)))))
                (.stop))]
    (dotimes [_ 240] (.tick sim))
    (into {}
          (map (fn [n] [(.-id n)
                        [(max 30 (min (- svg-width 30) (.-x n)))
                         (max 30 (min (- svg-height 30) (.-y n)))]]))
          (array-seq sim-nodes))))

(defn- edge-stroke [e]
  (cond
    (= "event-link" (:kind e)) "#f59e0b"
    (= "friction" (:polarity e)) "#dc2626"
    :else "#334155"))

(defn- graph-view [data]
  (let [nodes (with-edge-endpoints (:nodes data) (:edges data))
        edges (:edges data)
        positions (when (seq nodes) (force-layout nodes edges))
        node-map (into {} (map (juxt :id identity) nodes))]
    [:svg {:width "100%" :height "100%"
           :viewBox (str "0 0 " svg-width " " svg-height)
           :style {:background "radial-gradient(circle at top, #18243a 0%, #0f172a 70%)"}}
     (for [e edges
           :let [src (get positions (:src e)) dst (get positions (:dst e))]
           :when (and src dst)]
       (let [[x1 y1] src [x2 y2] dst]
         ^{:key (str (:src e) "->" (:dst e) ":" (:kind e))}
         [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                 :stroke (edge-stroke e)
                 :stroke-width (if (= "event-link" (:kind e)) 2.5 1.0)
                 :stroke-dasharray (when (= "friction" (:polarity e)) "5,4")
                 :opacity (if (= "event-link" (:kind e)) 0.95 0.45)}]))
     (for [[nid [x y]] positions
           :let [n (get node-map nid)]
           :when n]
       ^{:key nid}
       [:g
        [:circle {:cx x :cy y :r (node-radius n)
                  :fill (node-color n)
                  :stroke (if (:standing n) "#f8fafc" "#1e293b")
                  :stroke-width (if (:standing n) 2.0 1.0)
                  :opacity 0.95}
         [:title (str (:label n)
                      (when (:standing n) (str " — " (:standing n)))
                      (when (:rationale n) (str "\n" (:rationale n))))]]
        (when (or (:standing n) (= "essay" (:kind n)) (> (double (:degree n 0)) 4))
          [:text {:x x :y (- y (+ 4 (node-radius n)))
                  :fill "#e2e8f0" :font-size 10 :text-anchor "middle"}
           (let [l (:label n)] (if (> (count l) 22) (str (subs l 0 20) "…") l))])])]))

(defn- legend []
  [:div {:style {:display "flex" :gap "14px" :flex-wrap "wrap"
                 :font-size "11px" :color "#94a3b8" :padding "8px 20px"}}
   (for [[label color] [["territory" "#2563eb"] ["essay" "#7c3aed"]
                        ["strengthened" "#16a34a"] ["refined" "#d97706"]
                        ["addressed" "#06b6d4"] ["buyer/ref" "#0ea5e9"]
                        ["event-link" "#f59e0b"] ["friction edge" "#dc2626"]]]
     ^{:key label}
     [:span {:style {:display "flex" :gap "5px" :align-items "center"}}
      [:span {:style {:width "10px" :height "10px" :border-radius "50%" :background color}}] label])])

(defn page []
  (r/create-class
   {:component-did-mount
    (fn [_] (when-not (get-in @state/ui-state [:interest-network :data])
              (api/fetch-interest-network!)))
    :reagent-render
    (fn []
      (let [{:keys [data loading? error]} (:interest-network @state/ui-state)
            meta (:meta data)]
        [:div {:style {:display "flex" :flex-direction "column" :height "100vh"
                       :background "linear-gradient(180deg, #0f172a 0%, #111827 100%)"
                       :color "#e2e8f0"}}
         [:div {:style {:display "flex" :gap "14px" :padding "16px 20px"
                        :border-bottom "1px solid #223047" :align-items "center"}}
          [:a {:href "#/" :style {:color "#93c5fd" :text-decoration "none"
                                  :font-size "13px" :font-weight 700}} "Back To Graph"]
          [:div {:style {:font-size "18px" :font-weight 800}} "Interest Network"]
          [:button {:style {:padding "8px 14px" :border-radius "10px" :border "none"
                            :background "#2563eb" :color "#fff" :font-weight 700 :cursor "pointer"}
                    :on-click #(api/fetch-interest-network!)}
           (if loading? "Loading…" "Refresh")]
          [:div {:style {:flex 1}}]
          (when meta
            [:div {:style {:font-size "12px" :color "#94a3b8"}}
             (str (:territories meta) " territories · " (:essays meta) " essays · "
                  (:events meta) " events")])]
         [legend]
         (when-let [c (:completeness data)]
           [:div {:style {:display "flex" :gap "18px" :padding "2px 20px 8px"
                          :font-size "11px" :color "#cbd5e1" :align-items "center"}}
            [:span {:style {:color "#94a3b8"}} "completeness 3-vector:"]
            (let [pc (:path-coverage c) bc (:basin-coverage c)]
              [:<>
               [:span (str "resolution-path " (or (get-in c [:resolution-path-coverage :count]) 0))]
               [:span (str "path " (:count pc) "/" (:total pc))]
               [:span (str "basin " (:count bc) "/" (:total bc)
                           (when (seq (:missing bc))
                             (str " — missing: " (str/join ", " (map short-id (:missing bc))))))]])])
         (when error
           [:div {:style {:padding "10px 20px" :color "#fca5a5"}} error])
         [:div {:style {:flex 1 :min-height 0}}
          (if (seq (:nodes data))
            [graph-view data]
            [:div {:style {:padding "24px" :color "#94a3b8"}}
             (if loading? "Loading interest network…" "No data.")])]]))}))
