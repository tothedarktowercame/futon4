(ns webarxana.server.proxy
  (:require [clj-http.client :as http]
            [ring.util.response :as resp]
            [clojure.data.json :as json]))

(defn forward
  "Proxy a request to futon1a, rewriting /api/futon/* → /api/alpha/*"
  [req cfg method]
  (let [path       (get-in req [:path-params :path])
        futon-url  (str (:futon1a-url cfg) "/api/alpha/" path)
        query      (:query-string req)
        url        (if query (str futon-url "?" query) futon-url)
        penholder  (or (System/getenv "FUTON1A_COMPAT_PENHOLDER") "api")
        opts       (cond-> {:headers      {"accept"       "application/json"
                                           "content-type" "application/json"
                                           "x-penholder"  penholder}
                            :as           :text
                            :throw-exceptions false}
                     (#{:post :put} method)
                     (assoc :body (json/write-str (:body req))))]
    (try
      (let [response (case method
                       :get    (http/get url opts)
                       :post   (http/post url opts)
                       :put    (http/put url opts)
                       :delete (http/delete url opts))
            status   (:status response)
            body     (try (json/read-str (:body response) :key-fn keyword)
                         (catch Exception _ (:body response)))]
        (-> (resp/response body)
            (resp/status status)))
      (catch Exception e
        (println "Proxy error:" (.getClass e) (.getMessage e) "url:" url)
        (-> (resp/response {:error "Futon1a unreachable"
                            :detail (.getMessage e)
                            :url    url})
            (resp/status 502))))))
