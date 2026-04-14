(ns webarxana.client.core
  (:require [reagent.dom.client :as rdc]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]
            [webarxana.client.graph :as graph]
            [webarxana.client.card :as card]
            [webarxana.client.route :as route]))

(defn app []
  (if (state/logged-in?)
    [:div.webarxana
     [card/search-bar]
     [:div.main-area
      [card/sidebar]
      [:div.canvas-container
       [graph/graph-svg]
       [card/scratch-card]
       [card/focus-card]
       [card/link-editor]]]]
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
