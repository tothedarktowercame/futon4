(ns webarxana.server.proxy
  (:require [clj-http.client :as http]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [ring.util.codec :as codec]
            [ring.util.response :as resp]
            [clojure.data.json :as json]))

(def ^:private sip-lexicon-path
  "/home/joe/code/futon6/data/mission-self-representing-lexicon.json")

(def ^:private sip-scores
  (delay
    (try
      (let [doc (json/read-str (slurp (io/file sip-lexicon-path)) :key-fn keyword)]
        (into {}
              (keep (fn [{:keys [term score]}]
                      (when term [(str term) (double (or score 0.0))])))
              (:terms doc)))
      (catch Exception _ {}))))

(def ^:private folded-slot-roles #{"capability" "concept" "map-item" "mission"})

(defn- truthy? [v]
  (contains? #{"1" "true" "yes"} (str/lower-case (or v ""))))

(defn- prop [m k]
  (or (get m k) (get m (keyword k)) (get m (name k))))

(defn- scope-entity? [e]
  (str/starts-with? (or (:type e) "") "scope/"))

(def ^:private scope-tree-dir "/home/joe/code/futon6/data/mission-scope-trees")

(defn- scope-tree-parents
  "The detector's `original-id -> parent-original-id` nesting map for MISSION (its
   `parent` field IS the passage-containment result).  Used because the ingest drops
   `scope/parent` to nil on the substrate-2 entities (eightfold-stabilization nulls it),
   so the proxy can't read it back — but every entity still carries `scope/original-id`."
  [mission]
  (try
    (let [f (io/file scope-tree-dir (str mission ".json"))]
      (when (and mission (.exists f))
        (->> (:scope-hyperedges (json/read-str (slurp f) :key-fn keyword))
             (keep (fn [h] (when-let [p (:parent h)] [(:scope-id h) p])))
             (into {}))))
    (catch Exception _ nil)))

(defn- folded-frame-ego [body depth ego-mission]
  (let [ego (:ego body)
        outgoing (:outgoing ego)
        groups (group-by #(get-in % [:relation :hyperedge-id]) outgoing)
        scope-groups (->> groups
                          (keep (fn [[hx-id entries]]
                                  (let [t (some #(get-in % [:relation :type]) entries)]
                                    (when (and hx-id (str/starts-with? (or t "") "mission-scope/"))
                                      [hx-id {:type t :entries entries}]))))
                          (into {}))
        content (remove #(= "mission-scope/nesting" (:type (val %))) scope-groups)
        ;; Nesting comes from the detector's scope-tree (the ingest nulls scope/parent
        ;; on the entities), resolved via each scope's :scope/original-id -> entity id.
        scope-ents (->> content vals (mapcat :entries) (map :entity) (filter scope-entity?))
        mission (or (some #(prop (:props %) "mission") scope-ents) ego-mission)
        ;; Default to {} so a missing/unresolved scope tree yields no parents
        ;; instead of NPE-ing when parent-eid invokes this as a fn.
        oid->parent-oid (or (scope-tree-parents mission) {})
        oid->eid (into {} (keep (fn [e]
                                  (when-let [oid (prop (:props e) "scope/original-id")]
                                    [oid (:id e)]))
                                scope-ents))
        parent-eid (fn [scope]
                     (some-> (prop (:props scope) "scope/original-id")
                             oid->parent-oid
                             oid->eid))
        nodes (into {}
                    (keep (fn [[_ {:keys [type entries]}]]
                            (let [scope-entry (some #(when (scope-entity? (:entity %)) %) entries)
                                  scope (:entity scope-entry)
                                  sid (:id scope)
                                  props (:props scope)
                                  fillers (->> entries
                                               (map :entity)
                                               (remove scope-entity?)
                                               (filter #(contains? folded-slot-roles
                                                                    (or (prop (:props %) "scope/role")
                                                                        (:type %))))
                                               (mapv (fn [e]
                                                       (let [term (:name e)]
                                                         {:id (:id e)
                                                          :name term
                                                          :type (:type e)
                                                          :role (or (prop (:props e) "scope/role") (:type e))
                                                          :sip (double (get @sip-scores term 0.0))}))))]
                              (when sid
                                [sid {:scope-id sid
                                      :binder (or (prop props "scope/binder-type")
                                                  (last (str/split type #"/")))
                                      :title (or (:name scope) sid)
                                      :parent (parent-eid scope)
                                      :fillers fillers}]))))
                          content)
        children (reduce (fn [m [_ {:keys [entries]}]]
                           (let [scopes (->> entries (map :entity) (filter scope-entity?))]
                             (reduce (fn [m child]
                                       (if-let [parent (parent-eid child)]
                                         (update m parent (fnil conj #{}) (:id child))
                                         m))
                                     m scopes)))
                         {} scope-groups)
        nodes (into {}
                    (for [[sid n] nodes]
                      [sid (assoc n :children (vec (sort (get children sid))))]))
        child-ids (set (mapcat :children (vals nodes)))
        roots (->> (keys nodes) (remove child-ids) sort vec)
        aggregate (fn aggregate [sid seen]
                    (if (contains? @seen sid)
                      {:sub-count 0 :sub-mass 0.0 :sub-fillers []}
                      (do
                        (swap! seen conj sid)
                        (let [{:keys [fillers children]} (get nodes sid)
                              child-aggregates (map #(aggregate % seen) children)
                              sub-fillers (vec (concat fillers (mapcat :sub-fillers child-aggregates)))]
                          {:sub-count (count sub-fillers)
                           :sub-mass (double (reduce + (map :sip sub-fillers)))
                           :sub-fillers sub-fillers}))))
        aggregates (into {} (for [sid (keys nodes)] [sid (aggregate sid (atom #{}))]))
        sorted-roots (sort-by (comp - :sub-mass aggregates) roots)
        visible (letfn [(walk [sid d]
                         (cons sid
                               (when (< d depth)
                                 (->> (:children (get nodes sid))
                                      (sort-by (comp - :sub-mass aggregates))
                                      (mapcat #(walk % (inc d)))))))]
                  (vec (mapcat #(walk % 0) sorted-roots)))
        top-sip (fn [fillers]
                  (->> fillers
                       (group-by :name)
                       (map (fn [[term fs]] {:term term :score (apply max (map :sip fs))}))
                       (sort-by (comp - :score))
                       (take 4)
                       vec))
        frames (mapv (fn [sid]
                       (let [n (get nodes sid)
                             a (get aggregates sid)
                             fillers (:sub-fillers a)]
                         (assoc n
                                :sub-count (:sub-count a)
                                :sub-mass (:sub-mass a)
                                :top-concepts (top-sip fillers)
                                :fillers fillers)))
                     visible)
        frame-entity (fn [f]
                       {:id (:scope-id f)
                        :name (:title f)
                        :type "scope/frame"
                        :props {"scope/id" (:scope-id f)
                                "scope/binder" (:binder f)
                                "fold/sub-count" (:sub-count f)
                                "fold/top-concepts" (:top-concepts f)
                                "fold/fillers" (:fillers f)
                                "fold/child-count" (count (:children f))}})
        raw-scopes (count nodes)
        raw-slots (reduce + (map (comp count :fillers) (vals nodes)))]
    (if (seq frames)
      (assoc body :ego (assoc ego
                              :outgoing (mapv (fn [f]
                                                {:relation {:type "mission-scope/folded-frame"}
                                                 :entity (frame-entity f)})
                                              frames)
                              :incoming []
                              :fold {:raw-count (+ raw-scopes raw-slots)
                                     :raw-scopes raw-scopes
                                     :raw-slots raw-slots
                                     :visible-count (count frames)
                                     :depth depth
                                     :frames (mapv #(select-keys % [:scope-id :binder :title :sub-count :sub-mass
                                                                    :top-concepts :children :parent])
                                                   frames)}))
      body)))

(defn- focus-anchor
  "If PATH is `ego/<short E-/C- doc>` whose scopes anchor on `<repo>-d/mission/<lower-id>`,
   rewrite to `ego/<that-anchor>` so focusing the NATURAL excursion/campaign name shows its
   scope-surface.  (Missions resolve by short name already; excursions/campaigns don't — their
   scope hyperedges hang off the `<repo>-d/mission/…` vertex, not the bare `E-`/`C-` entity.)"
  [path]
  (or (when-let [[_ nm] (re-matches #"ego/([EC]-[^/]+)" (or path ""))]
        (let [tree (io/file scope-tree-dir (str nm ".json"))]
          (when (.exists tree)
            (when-let [doc-path (get (json/read-str (slurp tree)) "path")]
              (let [repo (-> doc-path (str/replace #"^.*/code/" "") (str/replace #"/.*$" ""))]
                (str "ego/" repo "-d/mission/" (str/lower-case nm)))))))
      path))

(defn forward
  "Proxy a request to futon1a, rewriting /api/futon/* → /api/alpha/*"
  [req cfg method]
  (let [path       (focus-anchor (get-in req [:path-params :path]))
        futon-url  (str (:futon1a-url cfg) "/api/alpha/" path)
        query      (:query-string req)
        query-params (codec/form-decode (or query ""))
        fold-ego? (and (= method :get)
                       (str/starts-with? (or path "") "ego/")
                       (truthy? (get query-params "fold")))
        fold-depth (long (or (some-> (get query-params "depth") Long/parseLong) 1))
        url        (if fold-ego?
                     ;; Fetch the RAW ego (full scope tree + parents) upstream; the fold
                     ;; — and its depth — is applied LOCALLY by folded-frame-ego.  Forwarding
                     ;; the client's fold/depth upstream truncated the tree before we could fold.
                     (str futon-url "?" (codec/form-encode
                                         (assoc query-params "fold" "0" "depth" "3")))
                     (if query (str futon-url "?" query) futon-url))
        penholder  (or (System/getenv "FUTON1A_COMPAT_PENHOLDER") "api")
        opts       (cond-> {:headers      {"accept"       "application/json"
                                           "content-type" "application/json"
                                           "x-penholder"  penholder}
                            :as           :text
                            :throw-exceptions false}
                     (#{:post :put} method)
                     (assoc :body (json/write-str (:body req))))]
    (try
      (let [response (case method
                       :get    (http/get url opts)
                       :post   (http/post url opts)
                       :put    (http/put url opts)
                       :delete (http/delete url opts))
            status   (:status response)
            body     (try (json/read-str (:body response) :key-fn keyword)
                         (catch Exception _ (:body response)))
            body     (if (and fold-ego? (= 200 status) (map? body))
                       (folded-frame-ego body fold-depth (subs path 4))
                       body)]
        (-> (resp/response body)
            (resp/status status)))
      (catch Exception e
        (println "Proxy error:" (.getClass e) (.getMessage e) "url:" url)
        (-> (resp/response {:error "Futon1a unreachable"
                            :detail (.getMessage e)
                            :url    url})
            (resp/status 502))))))
