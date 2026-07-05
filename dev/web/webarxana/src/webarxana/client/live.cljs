(ns webarxana.client.live
  (:require [clojure.string :as str]
            [reagent.core :as r]))

(def wm-url "http://localhost:3100/")
(def evidence-url "http://172-236-28-208.ip.linodeusercontent.com:7071/evidence-viewer/index.html")
(def summary-url "http://localhost:7070/api/alpha/cascade-real")
(def graph-url "http://localhost:7070/api/alpha/cascade-real/graph")

(defonce cascade-state
  (r/atom {:summary nil
           :graph nil
           :loading? false
           :error nil
           :last-loaded-ms nil}))

(defonce cascade-poller (atom nil))

(defn- fetch-json
  [url]
  (-> (js/fetch url #js {:cache "no-store"})
      (.then (fn [resp]
               (when-not (.-ok resp)
                 (throw (js/Error. (str "HTTP " (.-status resp) " for " url))))
               (.json resp)))
      (.then #(js->clj % :keywordize-keys true))))

(defn- load-cascade! []
  (when-not (:loading? @cascade-state)
    (swap! cascade-state assoc :loading? true)
    (-> (js/Promise.all #js [(fetch-json summary-url)
                             (fetch-json graph-url)])
        (.then (fn [res]
                 (reset! cascade-state
                         {:summary (aget res 0)
                          :graph (aget res 1)
                          :loading? false
                          :error nil
                          :last-loaded-ms (.now js/Date)})))
        (.catch (fn [err]
                  (swap! cascade-state assoc
                         :loading? false
                         :error (or (.-message err) (str err))))))))

(defn- ensure-poller! []
  (when-not @cascade-poller
    (load-cascade!)
    (reset! cascade-poller (js/setInterval load-cascade! 60000))))

(defn- ago
  [ms]
  (let [delta (max 0 (- (.now js/Date) (js/Number (or ms 0))))
        mins (js/Math.floor (/ delta 60000))]
    (cond
      (< mins 1) "now"
      (< mins 60) (str mins "m ago")
      :else (let [hrs (js/Math.floor (/ mins 60))]
              (cond
                (< hrs 48) (str hrs "h ago")
                :else (let [days (js/Math.floor (/ hrs 24))]
                        (if (< days 60)
                          (str days "d ago")
                          (str (js/Math.floor (/ days 30)) "mo ago"))))))))

(defn- short-target
  [target]
  (str/replace (str target) #"^[^-]*-d/(mission|excursion|campaign)/" ""))

(defn- open-link
  [href label]
  [:a {:href href
       :target "_blank"
       :rel "noopener noreferrer"
       :style {:color "#93c5fd"
               :font-size "12px"
               :font-weight 700
               :text-decoration "none"}}
   label])

(defn- tile
  [{:keys [title subtitle href body tall?]}]
  [:section {:style {:background "#171a21"
                     :border "1px solid #2a2f3a"
                     :border-radius "8px"
                     :padding "14px 16px"
                     :display "flex"
                     :flex-direction "column"
                     :gap "10px"
                     :min-height (if tall? "720px" "180px")}}
   [:div {:style {:display "flex"
                  :justify-content "space-between"
                  :align-items "baseline"
                  :gap "14px"}}
    [:div
     [:h2 {:style {:margin 0
                   :font-size "15px"
                   :color "#e6e9ef"}}
      title]
     [:div {:style {:margin-top "3px"
                    :font-size "12px"
                    :color "#9aa3b2"}}
      subtitle]]
    (when href [open-link href "open in tab"])]
   body])

(defn- iframe
  [src title height]
  [:iframe {:src src
            :title title
            :style {:width "100%"
                    :height height
                    :border "1px solid #2a2f3a"
                    :border-radius "6px"
                    :background "#05060a"}}])

(defn- chip
  [body]
  [:span {:style {:display "inline-flex"
                  :align-items "center"
                  :gap "4px"
                  :padding "4px 8px"
                  :border "1px solid #2f3846"
                  :border-radius "6px"
                  :background "#11151d"
                  :color "#cbd5e1"
                  :font-size "12px"
                  :line-height "1.35"}}
   body])

(defn- status-dot [ok?]
  [:span {:style {:width "8px"
                  :height "8px"
                  :border-radius "999px"
                  :background (if ok? "#3ddc84" "#ff6b6b")
                  :display "inline-block"}}])

(defn- native-panel
  [{:keys [id title caption count body]}]
  [:section {:id id
             :style {:background "#171a21"
                     :border "1px solid #2a2f3a"
                     :border-radius "8px"
                     :padding "14px 16px"}}
   [:div {:style {:display "flex"
                  :align-items "baseline"
                  :justify-content "space-between"
                  :gap "12px"
                  :margin-bottom "8px"}}
    [:h2 {:style {:margin 0
                  :font-size "15px"
                  :color "#e6e9ef"}}
     title
     (when count
       [:span {:style {:margin-left "6px"
                       :color "#9aa3b2"
                       :font-size "12px"
                       :font-weight 400}}
        count])]
    (when caption
      [:div {:style {:font-size "11px"
                     :color "#9aa3b2"
                     :text-align "right"
                     :max-width "520px"}}
       caption])]
   body])

(def table-style
  {:width "100%"
   :border-collapse "collapse"
   :font-size "12px"})

(def th-style
  {:text-align "left"
   :color "#9aa3b2"
   :font-weight 700
   :border-bottom "1px solid #2a2f3a"
   :padding "7px 8px"})

(def td-style
  {:border-bottom "1px solid #242a34"
   :padding "7px 8px"
   :vertical-align "top"})

(defn- summary-panel
  [summary]
  (let [dims (:dimensions summary)
        owners (:owners summary)
        spine (:spine summary)
        composition (:composition summary)
        standards (:standards summary)
        dim-order [[:O1 "mined-move arrows"]
                   [:O2 "canonical mine"]
                   [:O3 "lineage"]
                   [:O4 "upward clusters"]
                   [:O5 "honest holes"]]]
    [native-panel
     {:id "native-summary-panel"
      :title "Cascade summary"
      :count (str "· as of " (.toLocaleString (js/Date. (:as-of-ms summary))))
      :caption (if (:consistent? summary) "composed, consistent" "inconsistent")
      :body [:div {:style {:display "grid"
                           :gap "10px"}}
             [:div {:style {:display "flex" :flex-wrap "wrap" :gap "8px"}}
              (for [[k label] dim-order]
                ^{:key (name k)}
                [chip [:span [:b (name k)] " " label " · " [:b (get dims k 0)]
                       " " [:i {:style {:color "#9aa3b2"}} (get owners k "")]]])]
             [:div {:style {:display "flex" :flex-wrap "wrap" :gap "8px"}}
              [chip [:span [:b "Spine:"] " " (:canonical-mission-nodes spine 0) " canonical mission nodes"]]
              [chip [:span "O1-missions " [:b (:O1-missions spine 0)]]]
              [chip [:span "O4-missions " [:b (:O4-missions spine 0)]]]]
             [:div {:style {:display "flex" :flex-wrap "wrap" :gap "8px"}}
              [:span {:style {:color "#9aa3b2" :font-size "12px" :align-self "center"}}
               "Composition:"]
              (for [[k v] (sort-by (comp name key) composition)]
                ^{:key (name k)}
                [chip [:span (name k) " " [:b v]]])]
             [:div {:style {:display "flex" :flex-wrap "wrap" :gap "8px"}}
              [:span {:style {:color "#9aa3b2" :font-size "12px" :align-self "center"}}
               "Clause-1 standards:"]
              (for [[k v] (sort-by (comp name key) standards)]
                ^{:key (name k)}
                [chip [:span [status-dot v] (name k)]])]]}]))

(defn- tickets-panel
  [graph]
  (let [tickets (:tickets graph)
        items (:items tickets)
        total (or (:count-total tickets) (count items))]
    [native-panel
     {:id "native-tickets-panel"
      :title "Tickets"
      :count (str "· " (count items) " of " total)
      :caption "unclocked = no clock lineage, live or durable · claim by clocking (bell --mission <id> or 3 doc-edits <10 min)"
      :body (if (seq items)
              [:div {:style {:overflow-x "auto"}}
               [:table {:style table-style}
                [:thead
                 [:tr
                  [:th {:style th-style} "stem"]
                  [:th {:style th-style} "kind"]
                  [:th {:style th-style} "repo"]
                  [:th {:style th-style} "last-touched"]]]
                [:tbody
                 (for [t items]
                   ^{:key (str (:path t))}
                   [:tr
                    [:td {:style td-style} [:code (:stem t)]]
                    [:td {:style td-style} (:kind t)]
                    [:td {:style td-style} (:repo t)]
                    [:td {:style (assoc td-style :color "#9aa3b2")
                          :title (.toLocaleString (js/Date. (:mtime-ms t)))}
                     (ago (:mtime-ms t))]])]]]
              [:div {:style {:color "#9aa3b2" :font-size "12px"}}
               "no unclocked mission/excursion docs found"])}]))

(defn- lineage-panel
  [graph]
  (let [lineage (:lineage graph)]
    [native-panel
     {:id "native-lineage-panel"
      :title "Agent → mission lineage"
      :count (str "· " (count lineage) " currently-clocked")
      :caption "single-active per agent, durable XTDB"
      :body (if (seq lineage)
              [:div {:style {:overflow-x "auto"}}
               [:table {:style table-style}
                [:thead
                 [:tr
                  [:th {:style th-style} "agent"]
                  [:th {:style th-style} "target"]
                  [:th {:style th-style} "session"]
                  [:th {:style th-style} "clocked-at"]]]
                [:tbody
                 (for [e lineage]
                   ^{:key (str (:agent e) "-" (:target e) "-" (:at e))}
                   [:tr
                    [:td {:style td-style} (:agent e)]
                    [:td {:style td-style}
                     (short-target (:target e))
                     [:span {:style {:color "#9aa3b2" :margin-left "5px"}}
                      "(" (:target e) ")"]]
                    [:td {:style (assoc td-style :color "#9aa3b2")}
                     (subs (or (:session e) "") 0 (min 8 (count (or (:session e) ""))))]
                    [:td {:style (assoc td-style :color "#9aa3b2")}
                     (when (pos? (or (:at e) 0))
                       (.toLocaleString (js/Date. (:at e))))]])]]]
              [:div {:style {:color "#9aa3b2" :font-size "12px"}}
               "no lineage rows found"])}]))

(defn- native-cascade-panels []
  (let [{:keys [summary graph error loading? last-loaded-ms]} @cascade-state]
    [:div {:style {:display "grid"
                   :grid-template-columns "minmax(0, 1fr)"
                   :gap "16px"}}
     [:div {:style {:display "flex"
                    :justify-content "space-between"
                    :align-items "center"
                    :gap "12px"}}
      [:h2 {:style {:margin 0
                    :font-size "16px"
                    :color "#e6e9ef"}}
       "Native cascade panels"]
      [:div {:style {:font-size "12px" :color "#9aa3b2"}}
       (cond
         error (str "offline · " error)
         loading? "refreshing"
         last-loaded-ms (str "refreshed " (ago last-loaded-ms))
         :else "loading")]]
     (when error
       [:div {:style {:border "1px solid #a35a3a"
                      :border-radius "6px"
                      :background "#20191a"
                      :color "#f8c08f"
                      :padding "10px 12px"
                      :font-size "12px"}}
        error])
     (when summary [summary-panel summary])
     (when graph
       [:<>
        [tickets-panel graph]
        [lineage-panel graph]])]))

(defn page []
  (ensure-poller!)
  [:div {:style {:min-height "100vh"
                 :background "#0f1115"
                 :color "#e6e9ef"
                 :overflow "auto"}}
   [:header {:style {:padding "18px 22px"
                     :border-bottom "1px solid #2a2f3a"
                     :background "linear-gradient(180deg,#12151b,#0f1115)"}}
    [:div {:style {:display "flex"
                   :align-items "baseline"
                   :justify-content "space-between"
                   :gap "16px"}}
     [:div
      [:h1 {:style {:margin 0
                    :font-size "20px"
                    :letter-spacing "0"}}
       "Live Surfaces"]
      [:div {:style {:margin-top "4px"
                     :font-size "12px"
                     :color "#9aa3b2"}}
       "Nav shell for the generated EFE field, cascade tracker, War Machine UI, and evidence viewer link-out."]]
     [:a {:href "#/"
          :style {:color "#93c5fd"
                  :font-size "12px"
                  :font-weight 700
                  :text-decoration "none"}}
      "Back to graph"]]]
   [:main {:style {:padding "18px 22px 40px"
                   :display "grid"
                   :grid-template-columns "minmax(0, 1fr)"
                   :gap "16px"}}
    [native-cascade-panels]
    [tile {:title "EFE map"
           :subtitle "Generated terrain page served fresh from futon6/data; existing live overlay polls futon3c."
           :href "/live/efe-map.html"
           :tall? true
           :body [iframe "/live/efe-map.html" "Live EFE map" "680px"]}]
    [tile {:title "Cascade tracker"
           :subtitle "Existing live cascade tracker, including tickets and lineage panels."
           :href "/live/cascade.html"
           :tall? true
           :body [iframe "/live/cascade.html" "Cascade tracker" "640px"]}]
    [:div {:style {:display "grid"
                   :grid-template-columns "repeat(auto-fit,minmax(260px,1fr))"
                   :gap "16px"}}
     [tile {:title "War Machine UI"
            :subtitle "Embedded Shadow build served by the same JVM."
            :href wm-url
            :body [:div {:style {:font-size "13px"
                                 :color "#cbd5e1"}}
                   "Strategic synthesis UI at "
                   [open-link wm-url wm-url]]}]
     [tile {:title "Evidence viewer"
            :subtitle "Link-out only until E-evidence-flow settles store selection."
            :href evidence-url
            :body [:div {:style {:display "flex"
                                 :flex-direction "column"
                                 :align-items "flex-start"
                                 :gap "10px"}}
                   [:span {:style {:display "inline-block"
                                   :padding "4px 8px"
                                   :border "1px solid #a35a3a"
                                   :border-radius "6px"
                                   :color "#f8c08f"
                                   :background "#20191a"
                                   :font-size "12px"
                                   :font-weight 700}}
                    "lucy store — hot laptop store unviewered; host choice gated on E-evidence-flow"]
                   [open-link evidence-url evidence-url]]}]]]])
