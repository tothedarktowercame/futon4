(ns webarxana.client.live)

(def wm-url "http://localhost:3100/")
(def evidence-url "http://172-236-28-208.ip.linodeusercontent.com:7071/evidence-viewer/index.html")

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

(defn page []
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
