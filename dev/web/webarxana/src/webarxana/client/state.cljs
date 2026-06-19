(ns webarxana.client.state
  (:require [clojure.set :as set]
            [datascript.core :as d]
            [reagent.core :as r]))

;; Datascript schema for the local graph cache.
;; Mirrors futon1a's entity/relation/hyperedge model.
(def schema
  {:nema/id       {:db/unique :db.unique/identity}
   :nema/name     {}
   :nema/type     {}
   :nema/text     {}
   :nema/authors  {}
   :nema/props    {}
   :nema/payload  {}
   ;; Relations (links between nemas — themselves nemas)
   :link/id       {:db/unique :db.unique/identity}
   :link/type     {}
   :link/src      {:db/valueType :db.type/ref}
   :link/dst      {:db/valueType :db.type/ref}
   :link/text     {}
   :link/payload  {}})

(defonce conn (d/create-conn schema))

;; DIAGNOSTIC: expose datascript conn and a single bundled snapshot helper
;; to JS so developers can inspect store state from devtools. Lightweight —
;; only used during debugging, no production hot-path consumers.
(set! (.-_webarxana_conn js/window) conn)
(set! (.-_webarxana_diag js/window)
      (fn []
        (let [db @conn
              total-datoms (count (d/datoms db :eavt))
              by-attr (->> (d/datoms db :eavt)
                           (group-by (fn [d] (str (.-a d))))
                           (into {} (map (fn [[a ds]] [a (count ds)]))))]
          (clj->js {:total-datoms total-datoms
                    :datoms-by-attr by-attr
                    :link-count (count (d/datoms db :aevt :link/id))
                    :nema-count (count (d/datoms db :aevt :nema/id))}))))

;; Reactive atoms for UI state
(defonce ui-state
  (r/atom {:focus-id    nil      ;; nema/id of the active card (shown on right)
           :page        :graph   ;; :graph or :mission-search
           :pins        []       ;; [{:id "..." :k 1} ...] pinned nodes on canvas
           :hop-depth   1        ;; default hop-depth for new pins (1 = direct neighbors)
           :editing     nil      ;; nema/id currently being edited
           :username    nil      ;; logged-in user
           :connected   false    ;; WebSocket connected?
           :login-error nil
           :browse-type nil      ;; currently selected type for browsing
           :browse-list []       ;; entities loaded for the selected type
           :sidebar-open false   ;; sidebar visibility
           :expanded-essays #{}  ;; essay ids expanded to show section neighbourhoods
           :expanded-essay-sections {} ;; essay id -> ordered section-id vector
           :view-mode :organic ;; Interest Constellation renderer mode.
           :graph-visible-ids nil ;; graph-filtered node ids for the card rail.
           :diagram-route {:name nil
                           :mode nil
                           :diagram-id nil
                           :loading? false
                           :error nil}
           :scratchpad  []       ;; newly created nodes awaiting connection
           :connecting  nil      ;; {:node-id "..."} when waiting to pick a target
           :mission-search {:query ""
                            :query-id nil
                            :results []
                            :graph {:nodes [] :links []}
                            :loading? false
                            :error nil}
           }))

(defn focus-id []
  (:focus-id @ui-state))

(defn set-focus! [nema-id]
  (swap! ui-state assoc :focus-id nema-id :editing nil))

(defn logged-in? []
  (some? (:username @ui-state)))

;; --- Pin management ---

(defn pinned? [nema-id]
  (some #(= (:id %) nema-id) (:pins @ui-state)))

(defn pin!
  "Add a node to the canvas as a pinned focus. Also sets it as active card."
  [nema-id & {:keys [k] :or {k 1}}]
  (when-not (pinned? nema-id)
    (swap! ui-state update :pins conj {:id nema-id :k k}))
  (set-focus! nema-id))

(defn unpin!
  "Remove a node from the canvas pins."
  [nema-id]
  (swap! ui-state update :pins
         (fn [ps] (vec (remove #(= (:id %) nema-id) ps))))
  ;; If we unpinned the active card, focus the last remaining pin
  (when (= nema-id (:focus-id @ui-state))
    (let [remaining (:pins @ui-state)]
      (if (seq remaining)
        (set-focus! (:id (last remaining)))
        (set-focus! nil)))))

(defn set-pin-k!
  "Adjust hop-depth for a specific pin."
  [nema-id k]
  (swap! ui-state update :pins
         (fn [ps] (mapv #(if (= (:id %) nema-id)
                           (assoc % :k k)
                           %) ps))))

;; --- Queries ---

(defn get-nema [nema-id]
  (when-let [eid (d/entid @conn [:nema/id nema-id])]
    (d/pull @conn '[*] eid)))

(defn- nema-id-of [db eid]
  ;; Fast :nema/id lookup via :eavt index (no :in binding needed)
  (some (fn [d] (when (= :nema/id (.-a d)) (.-v d)))
        (d/datoms db :eavt eid)))

(defn- links-touching
  "Return [outgoing-link-eids incoming-link-eids] for the nema with eid src-eid.
   Uses :aevt index walks rather than :in-bound d/q (which is broken under
   shadow-cljs :advanced compilation in this build of DataScript)."
  [db src-eid]
  (let [src-datoms (d/datoms db :aevt :link/src)
        dst-datoms (d/datoms db :aevt :link/dst)
        outgoing (vec (keep (fn [d] (when (= (.-v d) src-eid) (.-e d))) src-datoms))
        incoming (vec (keep (fn [d] (when (= (.-v d) src-eid) (.-e d))) dst-datoms))]
    [outgoing incoming]))

(defn neighbourhood
  "All nemas within k hops of focus-nema-id."
  [focus-nema-id k]
  (when focus-nema-id
    (let [db @conn]
      (loop [frontier #{focus-nema-id}
             visited  #{}
             depth    0
             link-eids #{}]
        (if (or (>= depth k) (empty? frontier))
          ;; Include frontier in visited — they are reachable within k hops
          (let [visited      (into visited frontier)
                visited-eids (into #{} (keep #(d/entid db [:nema/id %]) visited))
                nemas        (mapv (fn [eid] (d/pull db '[*] eid)) visited-eids)
                ;; Filter the collected link-eids down to ones whose BOTH endpoints
                ;; are in visited-eids (using :eavt direct lookups)
                links (->> link-eids
                           (keep (fn [leid]
                                   (let [datoms (d/datoms db :eavt leid)
                                         src-d (some (fn [d] (when (= :link/src (.-a d)) d)) datoms)
                                         dst-d (some (fn [d] (when (= :link/dst (.-a d)) d)) datoms)]
                                     (when (and src-d dst-d
                                                (contains? visited-eids (.-v src-d))
                                                (contains? visited-eids (.-v dst-d)))
                                       (d/pull db
                                               '[:link/id :link/type :link/text
                                                 {:link/src [:nema/id]}
                                                 {:link/dst [:nema/id]}]
                                               leid)))))
                           vec)]
            {:nemas nemas :links links :focus focus-nema-id})
          ;; Expand frontier by one hop
          (let [new-visited (into visited frontier)
                ;; For each frontier nema-id, resolve eid then collect touching links
                frontier-eids (into #{} (keep #(d/entid db [:nema/id %]) frontier))
                [out-pairs link-eids']
                (reduce (fn [[neighbors links] eid]
                          (let [[out-leids in-leids] (links-touching db eid)
                                ;; Resolve the OTHER endpoint of each link
                                out-others (mapv (fn [leid]
                                                   (some (fn [d]
                                                           (when (= :link/dst (.-a d)) (.-v d)))
                                                         (d/datoms db :eavt leid)))
                                                 out-leids)
                                in-others (mapv (fn [leid]
                                                  (some (fn [d]
                                                          (when (= :link/src (.-a d)) (.-v d)))
                                                        (d/datoms db :eavt leid)))
                                                in-leids)
                                ;; Convert other-endpoint eids back to :nema/id strings
                                neighbor-ids (->> (concat out-others in-others)
                                                  (keep #(nema-id-of db %))
                                                  set)]
                            [(into neighbors neighbor-ids)
                             (into links (concat out-leids in-leids))]))
                        [#{} link-eids]
                        frontier-eids)
                next-frontier (clojure.set/difference out-pairs new-visited)]
            (recur next-frontier new-visited (inc depth) link-eids')))))))

(defn multi-neighbourhood
  "Merge neighbourhoods from all pinned nodes into a single graph."
  [pins]
  (let [hoods (keep (fn [{:keys [id k]}]
                      (neighbourhood id (or k 1)))
                    pins)
        all-nemas (->> hoods
                       (mapcat :nemas)
                       (group-by :nema/id)
                       (map (fn [[_ vs]] (first vs)))
                       vec)
        all-links (->> hoods
                       (mapcat :links)
                       (group-by :link/id)
                       (map (fn [[_ vs]] (first vs)))
                       vec)
        pin-ids (set (map :id pins))]
    {:nemas all-nemas
     :links all-links
     :pins  pin-ids}))

;; --- Ingest ---

(defn- type-name [t]
  (cond
    (keyword? t) (if-let [ns-part (namespace t)]
                   (str ns-part "/" (name t))
                   (name t))
    (string? t) t
    (some? t) (str t)
    :else nil))

(defn- weak-type? [t]
  (contains? #{"unknown" "interest"} t))

(defn ingest-entity! [entity]
  (let [nema-id (or (:entity/id entity) (:id entity) (str (:xt/id entity)))
        existing (get-nema nema-id)
        incoming-props (or (:props entity)
                           (:entity/props entity)
                           {})
        incoming-type (or (type-name (:entity/type entity))
                          (type-name (:type entity))
                          "unknown")
        stale-weak? (and existing
                         (seq (:nema/props existing))
                         (empty? incoming-props)
                         (weak-type? incoming-type)
                         (not (weak-type? (:nema/type existing))))
        incoming-name (or (:entity/name entity) (:name entity) "")
        incoming-text (or (:entity/text entity)
                          (:source entity)
                          (get-in entity [:payload :text])
                          (:notes entity)
                          "")
        nema {:nema/id   nema-id
              :nema/name (if stale-weak?
                           (:nema/name existing)
                           incoming-name)
              :nema/type (if stale-weak?
                           (:nema/type existing)
                           incoming-type)
              :nema/text (if stale-weak?
                           (:nema/text existing)
                           incoming-text)
              :nema/authors (if stale-weak?
                              (:nema/authors existing)
                              (or (get-in entity [:props :authors])
                                  []))
              :nema/props (if stale-weak?
                            (:nema/props existing)
                            incoming-props)}]
    (d/transact! conn [nema])))

(defn- ensure-nema!
  "Idempotently ensure a nema with the given :nema/id exists in the
   store, WITHOUT clobbering any pre-existing :nema/name or :nema/type
   set by an earlier ingest-entity! call.  If the nema does not yet
   exist, creates a placeholder shell using the id as the display
   name; if it already exists, the transact is a no-op upsert on
   the unique identity attr only."
  [id]
  (when (and id (not= "" id))
    (if (d/entid @conn [:nema/id id])
      ;; Already exists — only re-assert the unique attr so the
      ;; lookup-ref resolves cleanly without overwriting name/type.
      (d/transact! conn [{:nema/id id}])
      ;; New entity — seed a placeholder shell.
      (d/transact! conn [{:nema/id   id
                          :nema/name id
                          :nema/type "placeholder"}]))))

(defn ingest-relation!
  "Transact a relation as a {:link/id, :link/type, :link/src, :link/dst} edge.
   Pre-creates a placeholder shell for each endpoint that doesn't exist yet,
   so the link's :db.type/ref lookup-refs ([:nema/id src-id]) resolve cleanly.
   Endpoints that have already been ingested as full entities KEEP their
   real :nema/name / :nema/type — the placeholder upsert is conditional
   on existence to avoid clobbering them."
  [rel]
  (let [src-id (or (:relation/from rel) (:src-id rel) (:src rel))
        dst-id (or (:relation/to rel) (:dst-id rel) (:dst rel))
        link-id (or (:relation/id rel) (:id rel) (str (:xt/id rel)))
        link   {:link/id   link-id
                :link/type (or (some-> (:relation/type rel) name)
                               (some-> (:type rel) name)
                               "link")
                :link/src  [:nema/id src-id]
                :link/dst  [:nema/id dst-id]
                :link/text (or (get-in rel [:provenance :note])
                               (:notes rel)
                               "")}]
    (when (and src-id dst-id link-id (not= "" link-id))
      (try
        (ensure-nema! src-id)
        (ensure-nema! dst-id)
        (d/transact! conn [link])
        (catch :default e
          (js/console.error "ingest-relation! FAILED"
                            (clj->js {:src-id src-id :dst-id dst-id
                                      :link-id link-id
                                      :err (str e)})))))))
