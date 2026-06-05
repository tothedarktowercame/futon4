(ns webarxana.client.route
  (:require [clojure.string :as str]
            [webarxana.client.state :as state]
            [webarxana.client.api :as api]))

;; Hash-based deep linking.
;; Format: #/pins/<id1>,<id2>,.../focus/<active-id>/type/<type>
;;         #/focus/<entity-id>  (legacy single-focus, auto-pins)
;;         #/type/<type>
;;         #/mission-search/<query>
;;         #/diagram/<diagram-name>/<expanded|compressed>[/view/<mode>]

(defn- encode [s] (js/encodeURIComponent s))
(defn- decode [s] (js/decodeURIComponent s))

(defonce ^:private restoring? (atom false))

(def valid-view-modes
  #{"organic" "layered" "witnesses" "zoom-lod"})

(defn- parse-view-mode [s]
  (let [mode (some-> s decode)]
    (if (contains? valid-view-modes mode)
      (keyword mode)
      :organic)))

(defn- view-mode-segments [view-mode]
  (when view-mode
    ["view" (clojure.core/name view-mode)]))

(defn- append-view-mode [parts view-mode]
  (cond-> parts
    view-mode (into (view-mode-segments view-mode))))

(defn push-hash!
  "Update the URL hash to reflect current UI state."
  []
  (let [{:keys [page browse-type focus-id pins mission-search expanded-diagram view-mode]} @state/ui-state
        pin-ids (map :id pins)
        parts (cond
                (= :mission-search page)
                (cond-> ["mission-search"]
                  (seq (:query mission-search)) (conj (encode (:query mission-search))))
                (= :interest-network page)
                ["interest-network"]
                (= :diagram-route page)
                (append-view-mode
                 (cond-> ["diagram" (encode (get-in @state/ui-state [:diagram-route :name]))]
                   (get-in @state/ui-state [:diagram-route :mode])
                   (conj (clojure.core/name (get-in @state/ui-state [:diagram-route :mode]))))
                 view-mode)
                ;; An expanded diagram (whole-graph view) is view-state that the
                ;; pins/focus form cannot round-trip: serialize the diagram id so
                ;; restore can re-derive the full graph (expand-diagram! rebuilds
                ;; the content pins from the diagram entity). Without this, a reload
                ;; collapses to whatever pins remain, never the expanded graph.
                expanded-diagram
                (append-view-mode
                 (cond-> ["xdiagram" (encode expanded-diagram)]
                   focus-id (into ["focus" (encode focus-id)]))
                 view-mode)
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
                       :expanded)
       :view-mode (if (= "view" (nth segments 3 nil))
                    (parse-view-mode (nth segments 4 nil))
                    :organic)}
      :else
      (loop [segs segments
           result {}]
        (if (< (count segs) 2)
          result
          (let [[k v & rest] segs]
            (recur (vec rest)
                   (case k
                     "type"     (assoc result :type (decode v))
                     "focus"    (assoc result :focus (decode v))
                     "pins"     (assoc result :pins
                                       (mapv decode (str/split v #",")))
                     "xdiagram" (assoc result :expanded-diagram (decode v))
                     "view"     (assoc result :view-mode (parse-view-mode v))
                     result))))))))

(defn restore-from-hash!
  "On page load, read the hash and restore the view."
  []
  (let [hash (.-hash js/location)]
    (when (seq hash)
      (reset! restoring? true)
      (let [{:keys [page query type focus pins diagram-name diagram-mode expanded-diagram view-mode]} (parse-hash hash)]
        (swap! state/ui-state assoc :view-mode (or view-mode :organic))
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
            (cond
              ;; Expanded-diagram (whole-graph) view: re-derive the full graph from
              ;; the diagram entity rather than restoring a pin remnant.
              expanded-diagram
              (do
                (println (str "[route] Restoring expanded diagram "
                              (subs expanded-diagram 0 (min 8 (count expanded-diagram)))))
                (api/expand-diagram! expanded-diagram)
                (when focus
                  (js/setTimeout #(state/set-focus! focus) 1500))
                (js/setTimeout #(reset! restoring? false) 3000))
              (seq pins)
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
              :else
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
                     (not= (:expanded-diagram old) (:expanded-diagram new))
                     (not= (:view-mode old) (:view-mode new))
                     (not= (:browse-type old) (:browse-type new))
                     (not= (get-in old [:mission-search :query])
                           (get-in new [:mission-search :query]))))
        (push-hash!))))
  (.addEventListener js/window "hashchange"
    (fn [_]
      (when (and (state/logged-in?) (not @restoring?))
        (restore-from-hash!)))))
