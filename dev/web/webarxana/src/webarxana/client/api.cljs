(ns webarxana.client.api
  (:require [clojure.string :as str]
            [cljs-http.client :as http]
            [cljs.core.async :refer [go <!]]
            [webarxana.client.state :as state]))

;; All API calls go through our thin server at /api/futon/*
;; which proxies to futon1a /api/alpha/* with auth.

(def base "/api/futon")

(declare fetch-hyperedges save-entity! save-relation! fetch-types pin-entity! connect-ws! ws-send! save-diagram! fetch-recent expand-diagram! expand-essay! collapse-essay! entity-location open-in-emacs! open-location! mission-search! mission-search-event!)

(defn entity-location
  "Return an Emacs-openable location for ENTITY when one is known."
  [entity]
  (let [entity-id (or (:id entity) (:entity/id entity) (:nema/id entity))
        entity-type (or (:type entity) (:_type entity) (:nema/type entity))]
    (cond
      (= entity-type "arxana/essay")
      (str "arxana://essay/" (js/encodeURIComponent entity-id))

      (= entity-type "arxana/essay-section")
      (when-let [[_ essay-id]
                 (re-matches #"^(.*)/section/[^/]+$" (or entity-id ""))]
        (str "arxana://essay/"
             (js/encodeURIComponent essay-id)
             "/section/"
             (js/encodeURIComponent entity-id)))

      :else
      nil)))

(defn open-in-emacs!
  "Ask the local WebArxana server to open ENTITY in Emacs."
  [entity]
  (when-let [location (entity-location entity)]
    (open-location! location)))

(defn open-location!
  "Ask the local WebArxana server to open LOCATION in Emacs."
  [location]
  (when (seq location)
    (go
      (let [resp (<! (http/post "/api/emacs/open"
                                {:json-params {:location location}
                                 :with-credentials? true}))]
        (when-not (= 200 (:status resp))
          (.error js/console "Open in Emacs failed" (clj->js resp)))
        resp))))

(defn mission-search!
  "Run mission-search via the server-backed JSON endpoint.
   Optional opts (T-1): :top-k :confidence-threshold :agreement-only?"
  ([query] (mission-search! query {}))
  ([query opts]
   (let [trimmed (str/trim (or query ""))
         top-k (:top-k opts 8)
         confidence (:confidence-threshold opts 0.0)
         agreement-only? (:agreement-only? opts false)]
     (swap! state/ui-state assoc :page :mission-search)
     (swap! state/ui-state assoc-in [:mission-search :query] trimmed)
     (when (seq trimmed)
       (swap! state/ui-state assoc-in [:mission-search :loading?] true)
       (swap! state/ui-state assoc-in [:mission-search :error] nil)
       (go
         (let [resp (<! (http/get "/api/mission-search"
                                  {:query-params {:query trimmed
                                                  :top-k top-k
                                                  :confidence-threshold confidence
                                                  :agreement-only (str agreement-only?)
                                                  :consumer-id "joe-webarxana"}
                                   :with-credentials? true}))]
           (if (= 200 (:status resp))
             (let [body (:body resp)]
               (swap! state/ui-state assoc
                      :mission-search {:query trimmed
                                       :query-id (:query_id body)
                                       :results (vec (:results body))
                                       :graph (or (:graph body) {:nodes [] :links []})
                                       :top-k top-k
                                       :confidence-threshold confidence
                                       :agreement-only? agreement-only?
                                       :loading? false
                                       :error nil}))
             (swap! state/ui-state assoc
                    :mission-search {:query trimmed
                                     :query-id nil
                                     :results []
                                     :graph {:nodes [] :links []}
                                     :loading? false
                                     :error (or (get-in resp [:body :error])
                                                "mission-search failed")}))))))))

(defn mission-search-event!
  [{:keys [query-id result-id title action]}]
  (when (and query-id result-id action)
    (go
      (<! (http/post "/api/mission-search/event"
                     {:json-params {:query-id query-id
                                    :consumer-id "joe-webarxana"
                                    :result-id result-id
                                    :title title
                                    :action action}
                      :with-credentials? true})))))

(defn ingest-ego!
  "Ingest an ego response (entity + outgoing + incoming) into Datascript."
  [ego]
  (let [entity   (:entity ego)
        outgoing (:outgoing ego)
        incoming (:incoming ego)]
    (when entity
      (state/ingest-entity! entity)
      ;; Outgoing relations
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
      ;; Incoming relations
      (doseq [entry incoming]
        (let [src-entity (:entity entry)
              rel-info   (:relation entry)]
          (when src-entity
            (state/ingest-entity! src-entity))
          (when (and rel-info src-entity)
            (state/ingest-relation!
             {:type (:type rel-info)
              :src  (:id src-entity)
              :dst  (:id entity)
              :id   (str (:id src-entity) "->" (:id entity))})))))
    entity))

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
              entity (ingest-ego! ego)]
          (when entity
            (let [eid (or (:id entity) (:entity/id entity))]
              (state/pin! eid)
              (fetch-hyperedges eid)
              (swap! state/ui-state update :_render-tick (fnil inc 0))))
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

(defn expand-essay!
  "Expand ESSAY-ID into section neighbours, then fetch section hyperedges.
This is the essay-side analogue of expanding a diagram: it teaches the
generic graph enough local structure to show section-level activity
without waiting for the full dedicated lifecycle surface."
  [essay-id]
  (go
    (when-not (state/get-nema essay-id)
      (let [essay-resp (<! (http/get (str base "/entity/" essay-id)
                                     {:with-credentials? true}))]
        (when (= 200 (:status essay-resp))
          (when-let [essay-entity (or (get-in essay-resp [:body :entity])
                                      (:body essay-resp))]
            (state/ingest-entity! essay-entity)))))
    (state/pin! essay-id)
    (let [resp (<! (http/get (str base "/entities/latest")
                             {:query-params {:type "arxana/essay-section"
                                             :limit 2000}
                              :with-credentials? true}))]
      (when (= 200 (:status resp))
        (let [sections (->> (get-in resp [:body :entities])
                            (filter (fn [section]
                                      (str/starts-with?
                                       (or (:id section) (:entity/id section) "")
                                       (str essay-id "/section/"))))
                            vec)]
          (doseq [section sections]
            (let [section-id (or (:id section) (:entity/id section))]
              (state/ingest-entity! section)
              (swap! state/ui-state update :pins
                     (fn [pins]
                       (if (some #(= (:id %) section-id) pins)
                         pins
                         (conj (vec pins) {:id section-id
                                           :k 1}))))
              (<! (fetch-hyperedges section-id))))
          (state/set-focus! essay-id)
          (swap! state/ui-state update :expanded-essays (fnil conj #{}) essay-id)
          (swap! state/ui-state assoc-in
                 [:expanded-essay-sections essay-id]
                 (mapv (fn [section]
                         (or (:id section) (:entity/id section)))
                       sections))
          (swap! state/ui-state update :_render-tick (fnil inc 0))
          sections)))))

(defn collapse-essay!
  "Collapse ESSAY-ID back to its central essay node.
Removes the section pins introduced by `expand-essay!' and clears the
essay-expansion metadata used by the graph projection."
  [essay-id]
  (let [section-ids (get-in @state/ui-state [:expanded-essay-sections essay-id] [])]
    (swap! state/ui-state update :pins
           (fn [pins]
             (vec
              (remove (fn [pin]
                        (some #(= (:id pin) %) section-ids))
                      pins))))
    (swap! state/ui-state update :expanded-essays disj essay-id)
    (swap! state/ui-state update :expanded-essay-sections dissoc essay-id)
    (state/set-focus! essay-id)
    (swap! state/ui-state update :_render-tick (fnil inc 0))))

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

(defn pin-entity!
  "Pin an entity onto the canvas. Fetches ego and adds as a pin."
  [entity-name entity-id]
  (if (state/pinned? entity-id)
    ;; Already pinned — unpin
    (state/unpin! entity-id)
    ;; Pin it: fetch ego, ingest, add pin
    (go
      (let [resp (<! (http/get (str base "/ego/" (js/encodeURIComponent (or entity-name entity-id)))
                               {:with-credentials? true}))]
        (when (= 200 (:status resp))
          (let [ego (get-in resp [:body :ego])
                entity (ingest-ego! ego)]
            (when entity
              (let [eid (or (:id entity) (:entity/id entity))]
                (state/pin! eid)
                (fetch-hyperedges eid)
                (swap! state/ui-state update :_render-tick (fnil inc 0))))))))))

(defn save-diagram!
  "Save the current pin spread as a diagram entity."
  [name]
  (let [pins (:pins @state/ui-state)
        pin-ids (mapv :id pins)
        focus-id (:focus-id @state/ui-state)]
    (when (and (seq (str/trim (or name ""))) (seq pin-ids))
      (go
        (let [entity (<! (save-entity!
                          {:name   (str/trim name)
                           :type   "diagram"
                           :source (str "Spread: " (str/join ", " pin-ids))
                           :props  {:pins     pin-ids
                                    :focus    focus-id
                                    :authors  [(:username @state/ui-state)]}}))]
          (when-let [eid (or (:id entity) (:entity/id entity))]
            ;; Create relations from diagram to each pinned entity
            (doseq [pid pin-ids]
              (<! (save-relation! {:type "diagram/includes"
                                   :src  eid
                                   :dst  pid})))
            ;; Don't pin the diagram — it's metadata, not a member
            ;; Track that this diagram is currently expanded
            (swap! state/ui-state assoc :expanded-diagram eid)
            ;; Refresh the recent list so it appears in the sidebar
            (fetch-recent)
            ;; Return the entity for UI feedback
            entity))))))

(defn expand-diagram!
  "Restore a diagram's pins onto the canvas."
  [diagram-id]
  (go
    (let [resp (<! (http/get (str base "/entity/" diagram-id)
                             {:with-credentials? true}))]
      (when (= 200 (:status resp))
        (let [entity (or (get-in resp [:body :entity]) (:body resp))
              pin-ids (get-in entity [:props :pins])
              focus (get-in entity [:props :focus])]
          (when (seq pin-ids)
            ;; Clear existing pins and load diagram's pins
            (swap! state/ui-state assoc :pins [] :expanded-diagram diagram-id)
            (doseq [pid pin-ids]
              (<! (pin-entity! pid pid)))
            (when focus
              (state/set-focus! focus))))))))

(defn compress-diagram!
  "Collapse current expanded diagram — show diagram node with non-content connections."
  [diagram-id]
  (swap! state/ui-state assoc :pins [] :expanded-diagram nil)
  ;; Pin normally — the graph filter handles hiding diagram/includes content
  (pin-entity! diagram-id diagram-id))

(defn fetch-recent
  "Fetch recent entities across key types for the activity feed."
  []
  (go
    (let [types ["diagram" "article" "arxana/essay" "arxana/essay-section"
                 "arxana/song" "arxana/chorus" "pattern/language"
                 "pattern/component" "apm/problem" "claim" "question"]
          results (atom [])]
      (doseq [t types]
        (let [resp (<! (http/get (str base "/entities/latest")
                                 {:query-params {:type t :limit 10}
                                  :with-credentials? true}))]
          (when (= 200 (:status resp))
            (swap! results into
                   (->> (get-in resp [:body :entities])
                        (map #(assoc % :_type t)))))))
      ;; Sort by name (we don't have timestamps, so alphabetical is the best we can do)
      (swap! state/ui-state assoc :recent-entities
             (->> @results
                  (filter #(seq (:name %)))
                  (take 30)
                  vec)))))

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
              entity (ingest-ego! ego)]
            (if entity
              (let [eid (or (:id entity) (:entity/id entity))]
                (state/pin! eid)
                (fetch-hyperedges eid)
                (swap! state/ui-state update :_render-tick (fnil inc 0)))
            ;; Ego didn't find by name — try direct entity fetch by ID
            (when entity-id
              (let [resp2 (<! (http/get (str base "/entity/" entity-id)
                                        {:with-credentials? true}))]
                (when (= 200 (:status resp2))
                  (let [e (or (get-in resp2 [:body :entity]) (:body resp2))]
                    (when e
                      (state/ingest-entity! e)
                      (let [eid (or (:id e) (:entity/id e))]
                        (state/pin! eid)
                        (fetch-hyperedges eid)
                        (swap! state/ui-state update :_render-tick (fnil inc 0))))))))))))))


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
          ;; Broadcast to other clients
          (ws-send! {:type "nema-updated" :entity entity})
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
          (ws-send! {:type "relation-updated" :relation rel})
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
            (connect-ws!)
            (fetch-recent)
            (when on-success (on-success)))
        (do (swap! state/ui-state assoc
                   :login-error "Invalid credentials")
            (when on-error (on-error)))))))

(defn check-auth!
  ([] (check-auth! nil))
  ([on-success]
   (go
     (let [resp (<! (http/get "/api/auth/check"
                              {:with-credentials? true}))]
       (when (= 200 (:status resp))
         (swap! state/ui-state assoc
                :username (get-in resp [:body :username]))
         (connect-ws!)
         (fetch-recent)
         (when on-success (on-success)))))))

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
                "nema-updated" (do (when-let [entity (:entity msg)]
                                    (state/ingest-entity! entity))
                                  (swap! state/ui-state update :_render-tick (fnil inc 0)))
                "relation-updated" (do (when-let [rel (:relation msg)]
                                         (state/ingest-relation! rel))
                                      (swap! state/ui-state update :_render-tick (fnil inc 0)))
                "presence" (println (str (:from msg (:username msg)) " " (:event msg)))
                (println "WS message:" msg)))))
    (reset! ws-conn ws)))

(defn ws-send! [msg]
  (when-let [ws @ws-conn]
    (when (= 1 (.-readyState ws))
      (.send ws (js/JSON.stringify (clj->js msg))))))
