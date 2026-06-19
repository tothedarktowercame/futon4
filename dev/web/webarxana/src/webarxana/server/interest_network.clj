(ns webarxana.server.interest-network
  "M-interest-network-coupling step (d): server-side projection for the live
   arxana://view/interest-network surface.

   Merges two sources into a renderable network:
   1. the bipartite REDUCE (essays × interest-territories) on disk, and
   2. the lived interest-event posterior in XTDB (read via futon1a /api/alpha),
   producing nodes (with current standing), edges (bipartite fit/friction +
   link/asserted events) and the completeness 3-vector.

   Recomputable from the event log alone (no in-place mutation). Read-only."
  (:require [clj-http.client :as http]
            [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]
            [ring.util.response :as resp]))

(def ^:private surface-path
  (or (System/getenv "INTEREST_SURFACE_EDN")
      "/home/joe/code/atthangika-interest-surface-v1.edn"))

(defn- load-surface []
  (edn/read-string (slurp surface-path)))

(defn- fetch-events
  "Read interest-event entities from XTDB via futon1a; return the :props maps."
  [cfg]
  (let [penholder (or (System/getenv "FUTON1A_COMPAT_PENHOLDER") "api")
        url (str (:futon1a-url cfg) "/api/alpha/entities/latest?type=interest-event&limit=500")
        resp (http/get url {:headers {"accept" "application/edn" "x-penholder" penholder}
                            :as :text :throw-exceptions false})
        parsed (try (edn/read-string (:body resp)) (catch Exception _ nil))]
    (->> (:entities parsed) (map :props) (filter map?))))

(defn- short-id [s] (last (str/split (str s) #"/")))

(def ^:private buckets-path
  (or (System/getenv "ATTHANGIKA_BUCKETS") "/home/joe/code/atthangika-buckets.json"))
(def ^:private strawmen-dir
  (or (System/getenv "STRAWMEN_DIR")
      "/home/joe/code/futon5a/holes/missions/M-expressions-of-interest.strawmen"))

;; The composite Path-arrows the eoi-engine instantiates (corpus arrow vocabulary).
(def ^:private path-arrow-universe
  ["Right View" "Right Intention" "Right Speech" "Right Mindfulness" "Right Livelihood"])

(defn- basename-md [s]
  (some-> (str s) not-empty (str/split #"/") last (str/split #" \(") first str/trim))

(defn- load-buckets []
  (try (json/read-str (slurp buckets-path) :key-fn keyword) (catch Exception _ nil)))

(defn- strawmen-basins []
  (->> (or (some-> (java.io.File. strawmen-dir) .listFiles seq) [])
       (map #(.getName %))
       (filter #(str/ends-with? % ".md"))
       (remove #(or (= "README.md" %)
                    (str/includes? % ".draft")
                    (str/includes? % "twopager")
                    (str/includes? % "annotations")))
       sort vec))

(defn- coverage
  "Path-arrow coverage (eoi arrow_components) + basin coverage (strawmen dir vs
   eoi/institution source occupancy). Returns operator-readable exercised/occupied
   /missing sets. Recomputed per request, so it stays current as data accrues."
  [buckets]
  (let [eois  (:eoi_instances buckets)
        insts (:institution_objects buckets)
        exercised (->> eois (mapcat :arrow_components) (map :arrow)
                       (map #(-> (str %) (str/split #" \(") first str/trim))
                       (remove str/blank?) set)
        occupied  (->> (concat (map :source_file eois) (map :source_file insts))
                       (map basename-md) (remove nil?) set)
        basins (strawmen-basins)
        pu (set path-arrow-universe)]
    {:path-coverage {:universe path-arrow-universe
                     :exercised (vec (filter exercised path-arrow-universe))
                     :missing   (vec (remove exercised path-arrow-universe))
                     :count (count (filter pu exercised)) :total (count path-arrow-universe)}
     :basin-coverage {:universe basins
                      :occupied (vec (filter occupied basins))
                      :missing  (vec (remove occupied basins))
                      :count (count (filter occupied basins)) :total (count basins)}}))

(def ^:private cogito-demo-nodes
  [{:id "arxana/essay/glasgow-cogito-cover-letter-final"
    :label "COGITO shipped final"
    :kind "essay"
    :degree 20
    :theme "cogito-eoi-demo"}
   {:id "arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1"
    :label "COGITO draft"
    :kind "essay"
    :degree 1
    :theme "cogito-eoi-demo"}
   {:id "arxana/artifact/glasgow-RA-proforma"
    :label "Glasgow RA proforma"
    :kind "proforma"
    :degree 20
    :theme "cogito-eoi-demo"}
   {:id "arxana/proforma/glasgow-cogito-RA/covered"
    :label "16 covered criteria"
    :kind "proforma-coverage"
    :degree 16
    :theme "cogito-eoi-demo"}
   {:id "arxana/proforma/glasgow-cogito-RA/uncovered"
    :label "4 uncovered criteria"
    :kind "proforma-gap"
    :degree 4
    :theme "cogito-eoi-demo"}])

(def ^:private cogito-demo-edges
  [{:src "arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1"
    :dst "arxana/essay/glasgow-cogito-cover-letter-final"
    :kind "arxana/eoi-projection"
    :polarity "projection"
    :rationale "draft→final provenance edge: arxana/hyperedge/glasgow-cogito-eoi-projection-draft-to-final"}
   {:src "arxana/artifact/glasgow-RA-proforma"
    :dst "arxana/essay/glasgow-cogito-cover-letter-final"
    :kind "arxana/proforma-coverage"
    :polarity "coverage"
    :rationale "20 proforma annotation hyperedges: 16 covered, 4 explicit gaps"}
   {:src "arxana/proforma/glasgow-cogito-RA/covered"
    :dst "arxana/essay/glasgow-cogito-cover-letter-final"
    :kind "coverage/covered"
    :polarity "fit"
    :rationale "Covered criteria resolve to final-letter passages"}
   {:src "arxana/proforma/glasgow-cogito-RA/uncovered"
    :dst "arxana/essay/glasgow-cogito-cover-letter-final"
    :kind "coverage/uncovered"
    :polarity "gap"
    :rationale "JD4, JD8, JD10, and JD15 are surfaced as uncovered nodes"}])

(defn project
  "Merge the surface + events + coverage into a renderable graph."
  [surface events cov]
  (let [standing (into {} (for [e events
                                :when (:posterior-state e)]
                            [(:target/entity-id e) (:posterior-state e)]))
        territory-nodes (for [[tid deg] (:interest-degree surface)]
                          {:id tid :label (short-id tid) :kind "territory" :degree deg})
        essay-nodes (for [[eid deg] (:essay-degree surface)]
                      {:id eid :label (short-id eid) :kind "essay" :degree deg})
        ;; event-target entities as their own (posterior) layer
        event-nodes (for [e events
                          :when (and (:posterior-state e) (:target/entity-id e))]
                      {:id (:target/entity-id e)
                       :label (short-id (:target/entity-id e))
                       :kind (some-> (:target/granularity e) name)
                       :standing (some-> (:posterior-state e) name)
                       :rationale (:operator/rationale e)})
        bipartite-edges (for [{:keys [essay interest polarities]} (:bipartite-edges surface)]
                          {:src essay :dst interest
                           :kind "bipartite"
                           :polarity (cond (some #{:friction} polarities) "friction" :else "fit")})
        event-edges (for [e events
                          :when (= :link/asserted (:event/type e))]
                      {:src (:link/src e) :dst (:link/dst e)
                       :kind "event-link" :polarity "asserted"
                       :rationale (:operator/rationale e)})
        resolved (for [e events
                       :when (#{:state/addressed :state/falsified} (:event/type e))]
                   (:target/entity-id e))]
    {:nodes (vec (concat territory-nodes essay-nodes event-nodes cogito-demo-nodes))
     :edges (vec (concat bipartite-edges event-edges cogito-demo-edges))
     :standing standing
     :completeness (merge {:resolution-path-coverage {:resolved (vec resolved) :count (count resolved)}}
                          cov)
     :meta {:territories (count (:interest-degree surface))
            :essays (count (:essay-degree surface))
            :events (count events)
            :cogito-demo {:status "stubbed"
                          :coverage-annotations 20
                          :provenance-edge "arxana/hyperedge/glasgow-cogito-eoi-projection-draft-to-final"}
            :generated (:generated surface)}}))

(defn projection
  "Ring handler: GET /api/interest-network → the merged network JSON."
  [_req cfg]
  (try
    (resp/response (project (load-surface) (fetch-events cfg) (coverage (load-buckets))))
    (catch Throwable t
      (-> (resp/response {:ok false :error (.getMessage t)})
          (resp/status 502)))))
