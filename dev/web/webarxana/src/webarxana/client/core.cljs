(ns webarxana.client.core
  (:require [reagent.dom.client :as rdc]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]
            [webarxana.client.graph :as graph]
            [webarxana.client.card :as card]
            [webarxana.client.interest-network :as interest-network]
            [webarxana.client.mission-search :as mission-search]
            [webarxana.client.route :as route]))

(defn app []
  (if (state/logged-in?)
    (case (:page @state/ui-state)
      :mission-search [mission-search/page]
      :interest-network [interest-network/page]
      [:div.webarxana
       [card/search-bar]
       [:a {:href "#/interest-network"
            :on-click (fn [_] (api/fetch-interest-network!))
            :style {:position "fixed" :top "10px" :right "16px" :z-index 50
                    :color "#93c5fd" :font-size "12px" :font-weight 700
                    :text-decoration "none"}}
        "Interest Network →"]
       [:div.main-area
        [card/sidebar]
        [:div.canvas-container
         [graph/graph-svg]
         [card/scratch-card]
         [card/focus-card]
         [card/link-editor]]]])
    [card/login-form]))

(defonce root (rdc/create-root (.getElementById js/document "app")))

(defn ^:dev/after-load reload []
  (rdc/render root [app]))

(defn init []
  (println "[init] Starting...")
  (route/install!)
  (api/check-auth!
   (fn []
     (println "[init] Auth succeeded, restoring hash")
     (route/restore-from-hash!)))
  (reload))
