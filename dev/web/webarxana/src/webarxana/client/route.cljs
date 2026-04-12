(ns webarxana.client.route
  (:require [clojure.string :as str]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]))

;; Hash-based deep linking.
;; Format: #/type/<type>/focus/<entity-id>
;;         #/focus/<entity-id>
;;         #/type/<type>

(defn- encode [s] (js/encodeURIComponent s))
(defn- decode [s] (js/decodeURIComponent s))

(defn push-hash!
  "Update the URL hash to reflect current UI state without triggering hashchange."
  []
  (let [{:keys [browse-type focus-id]} @state/ui-state
        parts (cond-> []
                browse-type (into ["type" (encode browse-type)])
                focus-id    (into ["focus" (encode focus-id)]))]
    (when (seq parts)
      (let [new-hash (str "#/" (str/join "/" parts))]
        (when (not= new-hash (.-hash js/location))
          ;; Use replaceState to avoid polluting browser history on every click
          (.replaceState js/history nil "" new-hash))))))

(defn- parse-hash
  "Parse the URL hash into a map of {:type ... :focus ...}."
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
                   result)))))))

(defn restore-from-hash!
  "On page load, read the hash and restore the view."
  []
  (let [hash (.-hash js/location)]
    (when (seq hash)
      (let [{:keys [type focus]} (parse-hash hash)]
        (when type
          (api/browse-type! type))
        (when focus
          ;; Fetch the entity by ID and set focus
          (api/browse-and-focus! focus focus))))))

(defn install!
  "Wire up hash sync: update hash when focus/type changes, restore on load."
  []
  ;; Watch ui-state for changes and update the hash
  (add-watch state/ui-state ::route-sync
    (fn [_ _ old new]
      (when (or (not= (:focus-id old) (:focus-id new))
                (not= (:browse-type old) (:browse-type new)))
        (push-hash!))))
  ;; Handle browser back/forward
  (.addEventListener js/window "hashchange"
    (fn [_]
      (let [{:keys [type focus]} (parse-hash (.-hash js/location))]
        (when (and type (not= type (:browse-type @state/ui-state)))
          (api/browse-type! type))
        (when (and focus (not= focus (:focus-id @state/ui-state)))
          (api/browse-and-focus! focus focus))))))
