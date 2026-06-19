(ns webarxana.client.mission-search
  (:require [clojure.string :as str]
            [reagent.core :as r]
            [webarxana.client.api :as api]
            [webarxana.client.state :as state]
            ["d3-force" :as d3]))

(def svg-width 900)
(def svg-height 720)

(defn- fmt2 [n]
  (.toFixed (js/Number. (double n)) 2))

(defn- mission-color [node]
  (case (:node_type node)
    "mission"
    (cond
      (str/includes? (str/upper-case (or (:status node) "")) "IDENTIFY") "#e8b339"
      (str/includes? (str/upper-case (or (:status node) "")) "MAP") "#d97706"
      (str/includes? (str/upper-case (or (:status node) "")) "DERIVE") "#2563eb"
      (str/includes? (str/upper-case (or (:status node) "")) "VERIFY") "#06b6d4"
      (str/includes? (str/upper-case (or (:status node) "")) "INSTANTIATE") "#16a34a"
      (str/includes? (str/upper-case (or (:status node) "")) "COMPLETE") "#6b7280"
      :else "#7c3aed")

    "code-path" "#475569"
    "#64748b"))

(defn- force-layout [nodes links]
  (let [sim-nodes (clj->js (mapv (fn [node]
                                   {:id (:id node)
                                    :radius (if (= "mission" (:node_type node))
                                              (+ 22 (* 10 (max 0.1 (double (:score node 0.1)))))
                                              18)})
                                 nodes))
        sim-links (clj->js (mapv (fn [link]
                                   {:source (:source link)
                                    :target (:target link)})
                                 links))
        cx (/ svg-width 2)
        cy (/ svg-height 2)
        sim (-> (d3/forceSimulation sim-nodes)
                (.force "charge" (-> (d3/forceManyBody)
                                     (.strength -450)
                                     (.distanceMax 420)))
                (.force "link" (-> (d3/forceLink sim-links)
                                   (.id (fn [d] (.-id d)))
                                   (.distance 120)
                                   (.strength 0.6)))
                (.force "center" (d3/forceCenter cx cy))
                (.force "collide" (.radius (d3/forceCollide)
                                           (fn [d] (or (.-radius d) 24))))
                (.stop))]
    (dotimes [_ 220]
      (.tick sim))
    (into {}
          (map (fn [node]
                 [(.-id node)
                  [(max 50 (min (- svg-width 50) (.-x node)))
                   (max 40 (min (- svg-height 40) (.-y node)))]]))
          (array-seq sim-nodes))))

(defn- result-card [query-id result]
  [:div {:key (:id result)
         :style {:padding "14px 16px"
                 :border-bottom "1px solid #223047"
                 :cursor "pointer"}
         :on-click #(do
                      (api/mission-search-event! {:query-id query-id
                                                  :result-id (:id result)
                                                  :title (:title result)
                                                  :action :clicked})
                      (api/open-location! (:path result)))}
    [:div {:style {:display "flex"
                  :justify-content "space-between"
                  :gap "12px"
                  :align-items "baseline"}}
    [:div {:style {:font-weight 700 :color "#f8fafc"}} (:title result)]
    [:div {:style {:font-size "12px" :color "#93c5fd"}}
     (fmt2 (:score result 0.0))]]
   [:div {:style {:font-size "12px" :color "#94a3b8" :margin-top "4px"}}
    (or (:phase result) "")
    (when (seq (:date result))
      (str " · " (:date result)))]
   [:div {:style {:font-size "12px"
                  :color "#64748b"
                  :margin-top "4px"
                  :word-break "break-all"}}
    (:path result)]
   [:div {:style {:font-size "13px"
                  :line-height 1.45
                  :color "#cbd5e1"
                  :margin-top "8px"}}
    (:summary result)]
   [:div {:style {:font-size "11px" :color "#fbbf24" :margin-top "8px"}}
    (str "confidence " (fmt2 (:confidence result 0.0))
         " · divergence " (fmt2 (:divergence_score result 0.0)))]])

(defn- graph-view []
  (let [{:keys [nodes links]} (get-in @state/ui-state [:mission-search :graph])
        positions (when (seq nodes) (force-layout nodes links))
        node-map (into {} (map (juxt :id identity) nodes))]
    [:svg {:width "100%"
           :height "100%"
           :viewBox (str "0 0 " svg-width " " svg-height)
           :style {:background "radial-gradient(circle at top, #18243a 0%, #0f172a 70%)"
                   :border-left "1px solid #223047"}}
     (for [link links
           :let [src (get positions (:source link))
                 dst (get positions (:target link))]
           :when (and src dst)]
       (let [[x1 y1] src
             [x2 y2] dst]
         ^{:key (:id link)}
         [:g
          [:line {:x1 x1 :y1 y1 :x2 x2 :y2 y2
                  :stroke (if (= "cross-reference" (:type link)) "#60a5fa" "#475569")
                  :stroke-width 1.5
                  :stroke-dasharray (when (= "mentions-path" (:type link)) "5,5")
                  :opacity 0.8}]
          [:text {:x (/ (+ x1 x2) 2)
                  :y (/ (+ y1 y2) 2)
                  :fill "#94a3b8"
                  :font-size 11
                  :text-anchor "middle"}
           (:type link)]]))
     (for [[node-id [x y]] positions
           :let [node (get node-map node-id)]]
       ^{:key node-id}
       [:g {:style {:cursor "pointer"}
            :on-click #(api/open-location! (:path node))}
        [:circle {:cx x
                  :cy y
                  :r (if (= "mission" (:node_type node))
                       (+ 20 (* 10 (max 0.15 (double (:score node 0.15)))))
                       16)
                  :fill (mission-color node)
                  :stroke "#e2e8f0"
                  :stroke-width 1.2
                  :opacity 0.95}]
        [:text {:x x
                :y (- y 4)
                :fill "#f8fafc"
                :font-size 12
                :font-weight 700
                :text-anchor "middle"}
         (if (> (count (:label node)) 22)
           (str (subs (:label node) 0 20) "…")
           (:label node))]
        [:text {:x x
                :y (+ y 12)
                :fill "#cbd5e1"
                :font-size 10
                :text-anchor "middle"}
         (:node_type node)]])]))

(defn page []
  (let [query (r/atom (or (get-in @state/ui-state [:mission-search :query]) ""))
        ;; T-1: filter controls — reactive (auto-search on change; slider debounced).
        confidence-threshold (r/atom (or (get-in @state/ui-state [:mission-search :confidence-threshold]) 0.0))
        top-k (r/atom (or (get-in @state/ui-state [:mission-search :top-k]) 8))
        agreement-only? (r/atom (or (get-in @state/ui-state [:mission-search :agreement-only?]) false))
        slider-timer (r/atom nil)
        do-search! (fn []
                     (when (seq @query)
                       (api/mission-search! @query
                                            {:top-k @top-k
                                             :confidence-threshold @confidence-threshold
                                             :agreement-only? @agreement-only?})))
        debounced-search! (fn []
                            (when-let [t @slider-timer]
                              (js/clearTimeout t))
                            (reset! slider-timer
                                    (js/setTimeout do-search! 250)))]
    (fn []
      (let [{:keys [query-id results loading? error]} (:mission-search @state/ui-state)]
        [:div {:style {:display "flex"
                       :flex-direction "column"
                       :height "100vh"
                       :background "linear-gradient(180deg, #0f172a 0%, #111827 100%)"
                       :color "#e2e8f0"}}
         [:div {:style {:display "flex"
                        :gap "12px"
                        :padding "18px 20px"
                        :border-bottom "1px solid #223047"
                        :align-items "center"}}
          [:a {:href "#/"
               :style {:color "#93c5fd"
                       :text-decoration "none"
                       :font-size "13px"
                       :font-weight 700}}
           "Back To Graph"]
          [:div {:style {:font-size "18px" :font-weight 800}}
           "Mission Search"]
          [:input {:type "text"
                   :value @query
                   :placeholder "Search missions..."
                   :style {:flex 1
                           :padding "10px 12px"
                           :border-radius "10px"
                           :border "1px solid #334155"
                           :background "#0b1220"
                           :color "#f8fafc"}
                   :on-change #(reset! query (.. % -target -value))
                   :on-key-down #(when (= 13 (.-keyCode %)) (do-search!))}]
          [:button {:style {:padding "10px 16px"
                            :border-radius "10px"
                            :border "none"
                            :background "#2563eb"
                            :color "#ffffff"
                            :font-weight 700
                            :cursor "pointer"}
                    :on-click #(do-search!)}
           (if loading? "Searching..." "Search")]]
         ;; T-1 filter controls row
         [:div {:style {:display "flex"
                        :gap "20px"
                        :padding "10px 20px"
                        :border-bottom "1px solid #223047"
                        :align-items "center"
                        :font-size "13px"
                        :color "#94a3b8"}}
          [:label {:style {:display "flex" :gap "8px" :align-items "center"}}
           [:span "Confidence ≥"]
           [:input {:type "range"
                    :min "0" :max "1" :step "0.05"
                    :value @confidence-threshold
                    :style {:width "120px"}
                    :on-change #(do (reset! confidence-threshold
                                            (js/parseFloat (.. % -target -value)))
                                    (debounced-search!))}]
           [:span {:style {:font-family "monospace"
                           :color "#e2e8f0"
                           :min-width "32px"}}
            (.toFixed @confidence-threshold 2)]]
          [:label {:style {:display "flex" :gap "8px" :align-items "center"}}
           [:span "Top-K"]
           [:input {:type "number"
                    :min "3" :max "25" :step "1"
                    :value @top-k
                    :style {:width "56px"
                            :padding "4px 6px"
                            :border-radius "6px"
                            :border "1px solid #334155"
                            :background "#0b1220"
                            :color "#f8fafc"}
                    :on-change #(do (reset! top-k
                                            (js/parseInt (.. % -target -value)))
                                    (debounced-search!))}]]
          [:label {:style {:display "flex" :gap "8px" :align-items "center" :cursor "pointer"}}
           [:input {:type "checkbox"
                    :checked @agreement-only?
                    :on-change #(do (reset! agreement-only?
                                            (.. % -target -checked))
                                    (do-search!))}]
           [:span "Show only agreement (v1+v1.1)"]]
          [:div {:style {:flex 1}}]
          [:div {:style {:font-size "12px"}}
           (str (count results) " result" (when (not= 1 (count results)) "s"))]]
         (when error
           [:div {:style {:padding "10px 20px"
                          :color "#fca5a5"
                          :border-bottom "1px solid #3f1d1d"}}
            error])
         [:div {:style {:display "grid"
                        :grid-template-columns "380px minmax(0, 1fr)"
                        :flex 1
                        :min-height 0}}
          [:div {:style {:overflow "auto"
                         :border-right "1px solid #223047"}}
           (if (seq results)
             (for [result results]
               ^{:key (:id result)}
               [result-card query-id result])
             [:div {:style {:padding "24px" :color "#94a3b8"}}
              "Run a query to load the mission landscape."])]
          [:div {:style {:min-width 0}}
           [graph-view]]]]))))
