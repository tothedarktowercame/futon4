(ns webarxana.client.state
  (:require [datascript.core :as d]
            [reagent.core :as r]))

;; Datascript schema for the local graph cache.
;; Mirrors futon1a's entity/relation/hyperedge model.
(def schema
  {:nema/id       {:db/unique :db.unique/identity}
   :nema/name     {}
   :nema/type     {}
   :nema/text     {}
   :nema/payload  {}
   ;; Relations (links between nemas — themselves nemas)
   :link/id       {:db/unique :db.unique/identity}
   :link/type     {}
   :link/src      {:db/valueType :db.type/ref}
   :link/dst      {:db/valueType :db.type/ref}
   :link/text     {}
   :link/payload  {}})

(defonce conn (d/create-conn schema))

;; Reactive atoms for UI state
(defonce ui-state
  (r/atom {:focus-id    nil      ;; nema/id of the focused card
           :hop-depth   3        ;; how many hops to show
           :editing     nil      ;; nema/id currently being edited
           :username    nil      ;; logged-in user
           :connected   false    ;; WebSocket connected?
           :login-error nil
           :browse-type nil      ;; currently selected type for browsing
           :browse-list []       ;; entities loaded for the selected type
           :sidebar-open false   ;; sidebar visibility
           }))

(defn focus-id []
  (:focus-id @ui-state))

(defn set-focus! [nema-id]
  (swap! ui-state assoc :focus-id nema-id :editing nil))

(defn logged-in? []
  (some? (:username @ui-state)))

;; Query: get a nema by its id
(defn get-nema [nema-id]
  (when-let [eid (d/entid @conn [:nema/id nema-id])]
    (d/pull @conn '[*] eid)))

;; Query: get neighbourhood — all nemas within k hops of focus
(defn neighbourhood [focus-nema-id k]
  (when focus-nema-id
    (let [db @conn]
      ;; BFS over links
      (loop [frontier #{focus-nema-id}
             visited  #{}
             depth    0]
        (if (or (>= depth k) (empty? frontier))
          ;; Return all visited nemas with their links
          (let [nema-eids (keep #(d/entid db [:nema/id %]) visited)
                nemas     (map #(d/pull db '[*] %) nema-eids)
                ;; Find all links where both endpoints are in visited set
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
          ;; Expand frontier
          (let [new-visited (into visited frontier)
                ;; Find nemas connected to frontier via links
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
                next-frontier (clojure.set/difference
                               (into (set outgoing) incoming)
                               new-visited)]
            (recur next-frontier new-visited (inc depth))))))))

;; Transact nemas from futon1a API responses into Datascript
(defn ingest-entity! [entity]
  (let [nema {:nema/id   (or (:entity/id entity) (:id entity) (str (:xt/id entity)))
              :nema/name (or (:entity/name entity) (:name entity) "")
              :nema/type (or (some-> (:entity/type entity) name)
                             (some-> (:type entity) name)
                             "unknown")
              :nema/text (or (:entity/text entity)
                             (:source entity)
                             (get-in entity [:payload :text])
                             (:notes entity)
                             "")}]
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
