(ns webarxana.server.ws
  (:require [org.httpkit.server :as hk]
            [clojure.data.json :as json]))

;; Connected clients: channel → {:username ... :last-seen ...}
(defonce clients (atom {}))

(defn broadcast!
  "Send a message to all connected clients except the sender."
  [sender-ch msg]
  (let [payload (json/write-str msg)]
    (doseq [[ch _] @clients
            :when (not= ch sender-ch)]
      (hk/send! ch payload))))

(defn broadcast-all!
  "Send a message to all connected clients."
  [msg]
  (let [payload (json/write-str msg)]
    (doseq [[ch _] @clients]
      (hk/send! ch payload))))

(defn handler [req]
  (let [username (get-in req [:session :username] "anon")]
    (hk/with-channel req ch
      (swap! clients assoc ch {:username username
                               :connected-at (System/currentTimeMillis)})
      (hk/on-close ch
        (fn [_status]
          (swap! clients dissoc ch)
          (broadcast-all! {:type "presence"
                           :event "left"
                           :username username})))
      (broadcast! ch {:type "presence"
                      :event "joined"
                      :username username})
      (hk/on-receive ch
        (fn [data]
          (let [msg (try (json/read-str data :key-fn keyword)
                        (catch Exception _ nil))]
            (when msg
              ;; Relay nema updates to other clients
              (broadcast! ch (assoc msg :from username)))))))))
