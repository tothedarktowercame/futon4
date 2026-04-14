(ns webarxana.client.route
  (:require [clojure.string :as str]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]))

;; Hash-based deep linking.
;; Format: #/pins/<id1>,<id2>,.../focus/<active-id>/type/<type>
;;         #/focus/<entity-id>  (legacy single-focus, auto-pins)
;;         #/type/<type>

(defn- encode [s] (js/encodeURIComponent s))
(defn- decode [s] (js/decodeURIComponent s))

(defonce ^:private restoring? (atom false))

(defn push-hash!
  "Update the URL hash to reflect current UI state."
  []
  (let [{:keys [browse-type focus-id pins]} @state/ui-state
        pin-ids (map :id pins)
        parts (cond-> []
                (seq pin-ids) (into ["pins" (str/join "," (map encode pin-ids))])
                focus-id      (into ["focus" (encode focus-id)])
                browse-type   (into ["type" (encode browse-type)]))]
    (let [new-hash (if (seq parts)
                     (str "#/" (str/join "/" parts))
                     "#")]
      (when (not= new-hash (.-hash js/location))
        (.replaceState js/history nil "" new-hash)))))

(defn- parse-hash
  [hash-str]
  (let [s (str/replace hash-str #"^#/?" "")
        segments (str/split s #"/")]
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
                   result)))))))

(defn restore-from-hash!
  "On page load, read the hash and restore the view."
  []
  (let [hash (.-hash js/location)]
    (when (seq hash)
      (reset! restoring? true)
      (let [{:keys [type focus pins]} (parse-hash hash)]
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
            (js/setTimeout #(reset! restoring? false) 2000)))))))

(defn install!
  "Wire up hash sync."
  []
  (add-watch state/ui-state ::route-sync
    (fn [_ _ old new]
      (when (and (not @restoring?)
                 (or (not= (:focus-id old) (:focus-id new))
                     (not= (:pins old) (:pins new))
                     (not= (:browse-type old) (:browse-type new))))
        (push-hash!))))
  (.addEventListener js/window "hashchange"
    (fn [_]
      (when (and (state/logged-in?) (not @restoring?))
        (restore-from-hash!)))))
