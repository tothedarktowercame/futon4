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

(defonce !orbit (r/atom :unfetched))   ;; a single thread orbit (retracted onto the scope-surface)
(defonce !orbits (r/atom :unfetched))  ;; the FULL phase portrait — every engaging thread's orbit
(defonce !orbit-hover (r/atom nil))    ;; index of the orbit the mouse is over (dims the rest + shows a card)

;; Reactive atoms for UI state
(defonce ui-state
  (r/atom {:focus-id    nil      ;; nema/id of the active card (shown on right)
           :page        :graph   ;; :graph or :mission-search
           :pins        []       ;; [{:id "..." :k 1} ...] pinned nodes on canvas
           :hop-depth   3        ;; default hop-depth for new pins
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
           :scope-fold-depth 3 ;; mission-scope fold depth (3 = k3 all subscopes [default]; 1 = k2 main scopes only)
           :focus-name  nil    ;; entity NAME of the active focus (to re-fetch at a new depth)
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

(defn reset-graph!
  "Clear the Datascript graph + pins/scratchpad (keeps focus-id/name) — used before a
   re-fetch at a new scope-fold depth so collapsing actually drops the subscope nodes."
  []
  (d/reset-conn! conn (d/empty-db schema))
  (swap! ui-state assoc :pins [] :scratchpad []))

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

(defn neighbourhood
  "All nemas within k hops of focus-nema-id."
  [focus-nema-id k]
  (when focus-nema-id
    (let [db @conn]
      (loop [frontier #{focus-nema-id}
             visited  #{}
             depth    0]
        (if (or (>= depth k) (empty? frontier))
          ;; Include frontier in visited — they are reachable within k hops
          (let [visited  (into visited frontier)
                nema-eids (keep #(d/entid db [:nema/id %]) visited)
                nemas     (map #(d/pull db '[*] %) nema-eids)
                links     (d/q '[:find [(pull ?l [:link/id :link/type :link/text
                                                       {:link/src [:nema/id]}
                                                       {:link/dst [:nema/id]}]) ...]
                                 :in $ ?visited
                                 :where
                                 [?l :link/src ?s]
                                 [?l :link/dst ?d]
                                 [?s :nema/id ?sid]
                                 [?d :nema/id ?did]
                                 [(contains? ?visited ?sid)]
                                 [(contains? ?visited ?did)]]
                               db visited)]
            {:nemas nemas :links links :focus focus-nema-id})
          (let [new-visited (into visited frontier)
                outgoing (d/q '[:find [?did ...]
                                :in $ [?fid ...]
                                :where
                                [?s :nema/id ?fid]
                                [?l :link/src ?s]
                                [?l :link/dst ?d]
                                [?d :nema/id ?did]]
                              db (vec frontier))
                incoming (d/q '[:find [?sid ...]
                                :in $ [?fid ...]
                                :where
                                [?d :nema/id ?fid]
                                [?l :link/dst ?d]
                                [?l :link/src ?s]
                                [?s :nema/id ?sid]]
                              db (vec frontier))
                next-frontier (set/difference
                               (into (set outgoing) incoming)
                               new-visited)]
            (recur next-frontier new-visited (inc depth))))))))

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

(defn ingest-relation! [rel]
  (let [src-id (or (:relation/from rel) (:src-id rel) (:src rel))
        dst-id (or (:relation/to rel) (:dst-id rel) (:dst rel))
        link   {:link/id   (or (:relation/id rel) (:id rel) (str (:xt/id rel)))
                :link/type (or (some-> (:relation/type rel) name)
                               (some-> (:type rel) name)
                               "link")
                :link/src  [:nema/id src-id]
                :link/dst  [:nema/id dst-id]
                :link/text (or (get-in rel [:provenance :note])
                               (:notes rel)
                               "")}]
    (d/transact! conn [link])))
