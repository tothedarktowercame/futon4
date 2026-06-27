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
       ;; Top bar: search-bar takes the available width; the interest link sits
       ;; in-flow at the right (was a position:fixed overlay floating over the
       ;; canvas). Re-pointed from the stale bipartite /interest-network to the
       ;; live "Interest Constellation" diagram.
       [:div.top-bar {:style {:display "flex" :align-items "center"}}
        [:div {:style {:flex "1 1 auto" :min-width "0"}}
         [card/search-bar]]
        [:a {:href "#/diagram/Interest%20Constellation/expanded"
             :style {:flex "0 0 auto" :margin "0 16px" :color "#93c5fd"
                     :font-size "12px" :font-weight 700 :text-decoration "none"
                     :white-space "nowrap"}}
         "Interest Constellation →"]]
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

(defonce ^:private orbit-poller
  ;; Live orbit: poll the asset every 4s so the map follows the session without a reload.
  (js/setInterval api/poll-orbits! 4000))

(defn init []
  (println "[init] Starting...")
  (route/install!)
  (api/check-auth!
   (fn []
     (println "[init] Auth succeeded, restoring hash")
     (route/restore-from-hash!)))
  (reload))
