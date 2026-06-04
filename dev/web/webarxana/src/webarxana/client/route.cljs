(ns webarxana.client.route
  (:require [clojure.string :as str]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]))

;; Hash-based deep linking.
;; Format: #/pins/<id1>,<id2>,.../focus/<active-id>/type/<type>
;;         #/focus/<entity-id>  (legacy single-focus, auto-pins)
;;         #/type/<type>
;;         #/mission-search/<query>
;;         #/diagram/<diagram-name>/<expanded|compressed>

(defn- encode [s] (js/encodeURIComponent s))
(defn- decode [s] (js/decodeURIComponent s))

(defonce ^:private restoring? (atom false))

(defn push-hash!
  "Update the URL hash to reflect current UI state."
  []
  (let [{:keys [page browse-type focus-id pins mission-search]} @state/ui-state
        pin-ids (map :id pins)
        parts (cond
                (= :mission-search page)
                (cond-> ["mission-search"]
                  (seq (:query mission-search)) (conj (encode (:query mission-search))))
                (= :interest-network page)
                ["interest-network"]
                (= :diagram-route page)
                (cond-> ["diagram" (encode (get-in @state/ui-state [:diagram-route :name]))]
                  (get-in @state/ui-state [:diagram-route :mode])
                  (conj (clojure.core/name (get-in @state/ui-state [:diagram-route :mode]))))
                :else
                (cond-> []
                  (seq pin-ids) (into ["pins" (str/join "," (map encode pin-ids))])
                  focus-id      (into ["focus" (encode focus-id)])
                  browse-type   (into ["type" (encode browse-type)])))
        new-hash (if (seq parts)
                   (str "#/" (str/join "/" parts))
                   "#")]
    (when (not= new-hash (.-hash js/location))
      (.replaceState js/history nil "" new-hash))))

(defn- parse-hash
  [hash-str]
  (let [s (str/replace hash-str #"^#/?" "")
        segments (str/split s #"/")]
    (cond
      (= "mission-search" (first segments))
      {:page :mission-search
       :query (decode (or (second segments) ""))}
      (= "interest-network" (first segments))
      {:page :interest-network}
      (= "diagram" (first segments))
      {:page :diagram-route
       :diagram-name (decode (or (second segments) ""))
       :diagram-mode (case (nth segments 2 nil)
                       "compressed" :compressed
                       :expanded)}
      :else
      (loop [segs segments
           result {}]
        (if (< (count segs) 2)
          result
          (let [[k v & rest] segs]
            (recur (vec rest)
                   (case k
                     "type"  (assoc result :type (decode v))
                     "focus" (assoc result :focus (decode v))
                     "pins"  (assoc result :pins
                                    (mapv decode (str/split v #",")))
                     result))))))))

(defn restore-from-hash!
  "On page load, read the hash and restore the view."
  []
  (let [hash (.-hash js/location)]
    (when (seq hash)
      (reset! restoring? true)
      (let [{:keys [page query type focus pins diagram-name diagram-mode]} (parse-hash hash)]
        (cond
          (= page :mission-search)
          (do
            (swap! state/ui-state assoc :page :mission-search)
            (when (seq query)
              (api/mission-search! query))
            (js/setTimeout #(reset! restoring? false) 500))
          (= page :interest-network)
          (do
            (swap! state/ui-state assoc :page :interest-network)
            (api/fetch-interest-network!)
            (js/setTimeout #(reset! restoring? false) 500))
          (= page :diagram-route)
          (do
            (api/open-diagram-by-name! diagram-name diagram-mode)
            (js/setTimeout #(reset! restoring? false) 3000))
          :else
          (do
            (swap! state/ui-state assoc :page :graph)
            (when type
              (api/browse-type! type))
            (if (seq pins)
              (do
                (println (str "[route] Restoring " (count pins) " pins"))
                (doseq [pid pins]
                  (println (str "[route] Pinning: " (subs pid 0 8)))
                  (api/pin-entity! pid pid))
                (when focus
                  (state/set-focus! focus))
                ;; Re-enable hash sync after pins have had time to load
                (js/setTimeout #(reset! restoring? false) 3000))
              ;; Legacy single-focus
              (do
                (when focus
                  (api/browse-and-focus! focus focus))
                (js/setTimeout #(reset! restoring? false) 2000)))))))))

(defn install!
  "Wire up hash sync."
  []
  (add-watch state/ui-state ::route-sync
    (fn [_ _ old new]
      (when (and (not @restoring?)
                 (or (not= (:page old) (:page new))
                     (not= (:focus-id old) (:focus-id new))
                     (not= (:pins old) (:pins new))
                     (not= (:browse-type old) (:browse-type new))
                     (not= (get-in old [:mission-search :query])
                           (get-in new [:mission-search :query]))))
        (push-hash!))))
  (.addEventListener js/window "hashchange"
    (fn [_]
      (when (and (state/logged-in?) (not @restoring?))
        (restore-from-hash!)))))
