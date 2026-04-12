(ns webarxana.server.core
  (:require [org.httpkit.server :as hk]
            [reitit.ring :as ring]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [ring.middleware.json :refer [wrap-json-body wrap-json-response]]
            [ring.util.response :as resp]
            [webarxana.server.auth :as auth]
            [webarxana.server.proxy :as proxy]
            [webarxana.server.ws :as ws])
  (:gen-class))

(def config
  (atom {:futon1a-url (or (System/getenv "FUTON1A_URL")
                          (str "http://127.0.0.1:"
                               (or (System/getenv "FUTON1A_PORT") "7071")))
         :port        (or (some-> (System/getenv "PORT") Integer/parseInt)
                          3100)
         :session-secret (or (System/getenv "SESSION_SECRET")
                             "webarxana-dev-secret-change-me!!")}))

(defn app-routes [cfg]
  (ring/ring-handler
   (ring/router
    [["/api"
      ["/auth/login"  {:post (fn [req] (auth/login req cfg))}]
      ["/auth/logout" {:post (fn [_]   (auth/logout))}]
      ["/auth/check"  {:get  (fn [req] (auth/check req))}]
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

(defn wrap-session [handler cfg]
  (let [defaults (-> site-defaults
                     (assoc-in [:session :cookie-name] "webarxana-session")
                     (assoc-in [:session :cookie-attrs :same-site] :lax)
                     (assoc-in [:security :anti-forgery] false))]
    (wrap-defaults handler defaults)))

(defn -main [& _args]
  (let [cfg  @config
        port (:port cfg)
        app  (-> (app-routes cfg)
                 (wrap-session cfg))]
    (println (str "WebArxana starting on http://localhost:" port))
    (println (str "Proxying futon1a at " (:futon1a-url cfg)))
    (hk/run-server app {:port port})
    (println "Ready.")))
