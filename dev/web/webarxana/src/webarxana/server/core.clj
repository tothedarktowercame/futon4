(ns webarxana.server.core
  (:require [org.httpkit.server :as hk]
            [reitit.ring :as ring]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.util.response :as resp]
            [webarxana.server.auth :as auth]
            [webarxana.server.emacs :as emacs]
            [webarxana.server.mission-search :as mission-search]
            [webarxana.server.proxy :as proxy]
            [webarxana.server.ws :as ws])
  (:gen-class))

(defonce !server (atom nil))

(defn default-config []
  {:futon1a-url (or (System/getenv "FUTON1A_URL")
                    (str "http://127.0.0.1:"
                         (or (System/getenv "FUTON1A_PORT") "7071")))
   :port        (or (some-> (System/getenv "PORT") Integer/parseInt)
                    3100)
   :emacs-socket (or (System/getenv "EMACS_SOCKET") "server")
   :emacsclient-bin (or (System/getenv "EMACSCLIENT") "emacsclient")
   :session-secret (or (System/getenv "SESSION_SECRET")
                       "webarxana-dev-secret-change-me!!")})

(def config
  (atom (default-config)))

(defn app-routes [cfg]
  (ring/ring-handler
   (ring/router
    [["/api"
      ["/auth/login"  {:post (fn [req] (auth/login req cfg))}]
      ["/auth/logout" {:post (fn [_]   (auth/logout))}]
      ["/auth/check"  {:get  (fn [req] (auth/check req))}]
      ["/emacs/open"  {:middleware [auth/wrap-require-auth]
                       :post (fn [req] (emacs/open-location req cfg))}]
      ["/mission-search" {:middleware [auth/wrap-require-auth]
                          :get (fn [req] (mission-search/search req))}]
      ["/mission-search/event" {:middleware [auth/wrap-require-auth]
                                :post (fn [req] (mission-search/record-event req))}]
      ["/ws"          {:get  (fn [req] (ws/handler req))}]
      ;; Proxy all futon1a API calls — require auth
      ["/futon/*path" {:middleware [auth/wrap-require-auth]
                       :get    (fn [req] (proxy/forward req cfg :get))
                       :post   (fn [req] (proxy/forward req cfg :post))
                       :put    (fn [req] (proxy/forward req cfg :put))
                       :delete (fn [req] (proxy/forward req cfg :delete))}]]]
    {:data {:middleware [#(wrap-json-body % {:keywords? true})
                         wrap-json-response]}})
   ;; Default handler — serve static files or index.html
   (ring/routes
    (ring/create-resource-handler {:path "/"})
    (ring/create-default-handler
     {:not-found (constantly (-> (resp/resource-response "public/index.html")
                                (resp/content-type "text/html")))}))))

(defn wrap-session [handler _cfg]
  (let [defaults (-> site-defaults
                     (assoc-in [:session :cookie-name] "webarxana-session")
                     (assoc-in [:session :cookie-attrs :same-site] :lax)
                     (assoc-in [:security :anti-forgery] false))]
    (wrap-defaults handler defaults)))

(defn start!
  ([] (start! {}))
  ([overrides]
   (when-let [existing @!server]
     ((:stop existing))
     (reset! !server nil))
   (let [cfg (merge (default-config) overrides)
         port (:port cfg)
         app  (-> (app-routes cfg)
                  (wrap-session cfg))
         stop-fn (hk/run-server app {:port port})
         system {:config cfg
                 :port port
                 :stop (fn []
                         (stop-fn)
                         (reset! !server nil))}]
     (reset! config cfg)
     (reset! !server system)
     (println (str "WebArxana starting on http://localhost:" port))
     (println (str "Proxying futon1a at " (:futon1a-url cfg)))
     (println "Ready.")
     system)))

(defn stop! []
  (when-let [system @!server]
    ((:stop system))))

(defn status []
  (if-let [system @!server]
    {:running? true
     :port (:port system)
     :futon1a-url (get-in system [:config :futon1a-url])}
    {:running? false}))

(defn -main [& _args]
  (start!))
