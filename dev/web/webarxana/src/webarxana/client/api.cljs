(ns webarxana.client.api
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [go <!]]
            [webarxana.client.state :as state]))

;; All API calls go through our thin server at /api/futon/*
;; which proxies to futon1a /api/alpha/* with auth.

(def base "/api/futon")

(declare fetch-hyperedges save-entity! save-relation! fetch-types)

(defn fetch-entity
  "Fetch a single entity by ID and ingest into Datascript."
  [entity-id]
  (go
    (let [resp (<! (http/get (str base "/entity/" entity-id)
                             {:with-credentials? true}))]
      (when (= 200 (:status resp))
        (let [entity (or (get-in resp [:body :entity]) (:body resp))]
          (state/ingest-entity! entity)
          entity)))))

(defn fetch-ego
  "Fetch entity + outgoing relations (neighbourhood seed)."
  [entity-name]
  (go
    (let [resp (<! (http/get (str base "/ego/" (js/encodeURIComponent entity-name))
                             {:with-credentials? true}))]
      (when (= 200 (:status resp))
        (let [ego (get-in resp [:body :ego])
              entity (:entity ego)
              outgoing (:outgoing ego)]
          (when entity
            (state/ingest-entity! entity)
            ;; Ego returns [{:relation {:type ...} :entity {:id ...}} ...]
            (doseq [entry outgoing]
              (let [dst-entity (:entity entry)
                    rel-info   (:relation entry)]
                (when dst-entity
                  (state/ingest-entity! dst-entity))
                (when (and rel-info dst-entity)
                  (state/ingest-relation!
                   {:type (:type rel-info)
                    :src  (:id entity)
                    :dst  (:id dst-entity)
                    :id   (str (:id entity) "->" (:id dst-entity))}))))
            ;; Set focus to the entity we just loaded
            (let [eid (or (:id entity) (:entity/id entity))]
              (state/set-focus! eid)
              ;; Also fetch hyperedges for this entity
              (fetch-hyperedges eid)))
          ego)))))

(defn fetch-entities-by-type
  "Fetch latest entities of a given type."
  [type-str & {:keys [limit] :or {limit 50}}]
  (go
    (let [resp (<! (http/get (str base "/entities/latest")
                             {:query-params {:type type-str :limit limit}
                              :with-credentials? true}))]
      (when (= 200 (:status resp))
        (let [entities (get-in resp [:body :entities])]
          (doseq [e entities]
            (state/ingest-entity! e))
          entities)))))

(defn fetch-hyperedges
  "Fetch hyperedges where entity-id is an endpoint. Ingests connected entities."
  [entity-id]
  (go
    (let [resp (<! (http/get (str base "/hyperedges")
                             {:query-params {:end entity-id :limit 50}
                              :with-credentials? true}))]
      (when (= 200 (:status resp))
        (let [hxs (get-in resp [:body :hyperedges])]
          ;; For each hyperedge, ingest connected entities and create links
          (doseq [hx hxs]
            (let [ends     (or (:hx/ends hx) [])
                  hx-id    (or (:hx/id hx) (str (random-uuid)))
                  hx-type  (or (:hx/type hx) "hyperedge")
                  note     (get-in hx [:hx/props :note])
                  ;; Find the other endpoint(s)
                  other-ends (remove #(= entity-id (:entity-id %)) ends)]
              (doseq [ep other-ends]
                (let [ep-id (:entity-id ep)]
                  (when ep-id
                    ;; Ingest a minimal nema for the other endpoint
                    (state/ingest-entity! {:id ep-id
                                          :name (or (:passage ep) ep-id)
                                          :type (or (:role ep) "endpoint")})
                    ;; Create a link from the focused entity to the other endpoint
                    (state/ingest-relation!
                     {:id   (str hx-id "->" ep-id)
                      :type hx-type
                      :src  entity-id
                      :dst  ep-id}))))))
          ;; Nudge Reagent to re-render after datascript transactions
          (swap! state/ui-state update :_render-tick (fnil inc 0))
          hxs)))))

(defn create-scratch-node!
  "Add a local scratch entry to the scratchpad. No server call until Save."
  []
  (when (empty? (:available-types @state/ui-state))
    (fetch-types))
  (let [local-id (str "scratch-" (random-uuid))]
    (swap! state/ui-state update :scratchpad
           conj {:id local-id :name "" :type "article" :local? true})
    local-id))

(defn connect-nodes!
  "Create a relation between two nodes."
  [src-id dst-id rel-type]
  (go
    (let [rel (<! (save-relation! {:type (or rel-type "arxana/scholium")
                                   :src src-id
                                   :dst dst-id}))]
      (swap! state/ui-state assoc :connecting nil)
      ;; Nudge re-render
      (swap! state/ui-state update :_render-tick (fnil inc 0))
      rel)))

(defn fetch-types
  "Fetch registered types from futon1a."
  []
  (go
    (let [resp (<! (http/get (str base "/types")
                             {:with-credentials? true}))]
      (when (= 200 (:status resp))
        (let [types (get-in resp [:body :types])]
          (swap! state/ui-state assoc
                 :available-types
                 (->> types
                      (filter #(= "entity" (:type/kind %)))
                      (map :type/id)
                      sort
                      vec)
                 :available-relation-types
                 (->> types
                      (filter #(= "relation" (:type/kind %)))
                      (map :type/id)
                      sort
                      vec))
          types)))))

(defn browse-type!
  "Load entities of a given type into the sidebar."
  [type-str]
  (swap! state/ui-state assoc :browse-type type-str :browse-list [] :sidebar-open true)
  (go
    (let [resp (<! (http/get (str base "/entities/latest")
                             {:query-params {:type type-str :limit 50}
                              :with-credentials? true}))]
      (when (= 200 (:status resp))
        (let [entities (get-in resp [:body :entities])]
          (swap! state/ui-state assoc :browse-list (vec entities)))))))

(defn browse-and-focus!
  "Load an entity by name via ego, used when clicking sidebar items."
  [entity-name entity-id]
  (go
    (let [resp (<! (http/get (str base "/ego/" (js/encodeURIComponent entity-name))
                             {:with-credentials? true}))]
      (when (= 200 (:status resp))
        (let [ego (get-in resp [:body :ego])
              entity (:entity ego)
              outgoing (:outgoing ego)]
          (if entity
            (do
              (state/ingest-entity! entity)
              (doseq [entry outgoing]
                (let [dst-entity (:entity entry)
                      rel-info   (:relation entry)]
                  (when dst-entity
                    (state/ingest-entity! dst-entity))
                  (when (and rel-info dst-entity)
                    (state/ingest-relation!
                     {:type (:type rel-info)
                      :src  (:id entity)
                      :dst  (:id dst-entity)
                      :id   (str (:id entity) "->" (:id dst-entity))}))))
              (let [eid (or (:id entity) (:entity/id entity))]
                (state/set-focus! eid)
                (fetch-hyperedges eid)))
            ;; Ego didn't find by name — try direct entity fetch by ID
            (when entity-id
              (let [resp2 (<! (http/get (str base "/entity/" entity-id)
                                        {:with-credentials? true}))]
                (when (= 200 (:status resp2))
                  (let [e (or (get-in resp2 [:body :entity]) (:body resp2))]
                    (when e
                      (state/ingest-entity! e)
                      (let [eid (or (:id e) (:entity/id e))]
                        (state/set-focus! eid)
                        (fetch-hyperedges eid)))))))))))))

(defn save-entity!
  "Create or update an entity via futon1a."
  [entity-data]
  (go
    (let [resp (<! (http/post (str base "/entity")
                              {:json-params entity-data
                               :with-credentials? true}))]
      (when (= 200 (:status resp))
        (let [entity (get-in resp [:body :entity])]
          (state/ingest-entity! entity)
          entity)))))

(defn save-relation!
  "Create or update a relation via futon1a.
   Wraps src/dst as maps so non-UUID entity IDs resolve correctly."
  [rel-data]
  (go
    (let [payload (-> rel-data
                      (update :src (fn [s] (if (string? s) {:id s} s)))
                      (update :dst (fn [d] (if (string? d) {:id d} d))))
          resp (<! (http/post (str base "/relation")
                              {:json-params payload
                               :with-credentials? true}))]
      (when (= 200 (:status resp))
        (let [rel (get-in resp [:body :relation])]
          (state/ingest-relation! rel)
          rel)))))

;; --- Auth ---

(defn login! [username password on-success on-error]
  (go
    (let [resp (<! (http/post "/api/auth/login"
                              {:json-params {:username username
                                             :password password}}))]
      (if (= 200 (:status resp))
        (do (swap! state/ui-state assoc
                   :username username
                   :login-error nil)
            (when on-success (on-success)))
        (do (swap! state/ui-state assoc
                   :login-error "Invalid credentials")
            (when on-error (on-error)))))))

(defn check-auth! []
  (go
    (let [resp (<! (http/get "/api/auth/check"
                             {:with-credentials? true}))]
      (when (= 200 (:status resp))
        (swap! state/ui-state assoc
               :username (get-in resp [:body :username]))))))

;; --- WebSocket ---

(defonce ws-conn (atom nil))

(defn connect-ws! []
  (let [proto (if (= "https:" js/location.protocol) "wss:" "ws:")
        url   (str proto "//" js/location.host "/api/ws")
        ws    (js/WebSocket. url)]
    (set! (.-onopen ws)
          (fn [_]
            (swap! state/ui-state assoc :connected true)
            (println "WebSocket connected")))
    (set! (.-onclose ws)
          (fn [_]
            (swap! state/ui-state assoc :connected false)
            (println "WebSocket disconnected")
            ;; Reconnect after 3s
            (js/setTimeout connect-ws! 3000)))
    (set! (.-onmessage ws)
          (fn [e]
            (let [msg (js->clj (js/JSON.parse (.-data e)) :keywordize-keys true)]
              (case (:type msg)
                "nema-updated" (when-let [entity (:entity msg)]
                                 (state/ingest-entity! entity))
                "relation-updated" (when-let [rel (:relation msg)]
                                     (state/ingest-relation! rel))
                "presence" (println (str (:username msg) " " (:event msg)))
                (println "WS message:" msg)))))
    (reset! ws-conn ws)))

(defn ws-send! [msg]
  (when-let [ws @ws-conn]
    (when (= 1 (.-readyState ws))
      (.send ws (js/JSON.stringify (clj->js msg))))))
