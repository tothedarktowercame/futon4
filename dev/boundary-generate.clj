#!/usr/bin/env clojure

(require '[clojure.data.json :as json])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(defn usage []
  (println "Usage: dev/boundary-generate.clj [--repo-root PATH] [--output PATH] [--media-index PATH]")
  (println)
  (println "Writes a repo-local boundary.edn snapshot with lab/media counts."))

(defn parse-args [args]
  (loop [opts {} remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--repo-root" (recur (assoc opts :repo-root (second remaining)) (nnext remaining))
        "--output" (recur (assoc opts :output (second remaining)) (nnext remaining))
        "--media-index" (recur (assoc opts :media-index (second remaining)) (nnext remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn- now-iso []
  (.toString (java.time.Instant/now)))

(defn- count-files [dir]
  (let [d (io/file dir)]
    (if (.isDirectory d)
      (count (filter #(.isFile ^java.io.File %)
                     (file-seq d)))
      0)))

(defn- newest-mtime [dirs]
  (let [times (for [dir dirs
                    :let [d (io/file dir)]
                    :when (.exists d)
                    f (file-seq d)
                    :when (.isFile ^java.io.File f)]
                (.lastModified ^java.io.File f))]
    (when (seq times)
      (-> (apply max times)
          (java.time.Instant/ofEpochMilli)
          (.toString)))))

(defn- read-json [path]
  (try
    (json/read-str (slurp path) :key-fn keyword)
    (catch Exception _ nil)))

(defn- locate-media-index [repo-root explicit]
  (let [candidates [(when explicit (io/file explicit))
                    (io/file repo-root ".." "futon0" "data" "zoom_sync_index.json")
                    (io/file repo-root "futon0" "data" "zoom_sync_index.json")
                    (io/file (System/getProperty "user.home") "code" "futon0" "data" "zoom_sync_index.json")]]
    (some (fn [f]
            (when (and f (.exists ^java.io.File f))
              (.getAbsolutePath ^java.io.File f)))
          candidates)))

(defn- media-unpersisted-count [index-path]
  (if-let [data (and index-path (read-json index-path))]
    (let [entries (or (:entries data) [])
          status-key (fn [e] (or (:status e) (:state e) (:storage_status e)))]
      (count (filter (fn [e]
                       (let [status (some-> (status-key e) str/lower-case)]
                         (or (= status "hold")
                             (= status "new")
                             (= status "unpersisted"))))
                     entries)))
    0))

(defn- boundary-entry [repo-root media-index]
  (let [lab-root (io/file repo-root "lab")
        raw-count (count-files (io/file lab-root "raw"))
        stub-count (count-files (io/file lab-root "stubs"))
        draft-count (count-files (io/file lab-root "doc-drafts"))
        media-unpersisted (media-unpersisted-count media-index)
        missing (+ draft-count media-unpersisted)
        prototypes (max 1 (+ draft-count media-unpersisted))
        last-modified (newest-mtime [(io/file lab-root "raw")
                                     (io/file lab-root "stubs")
                                     (io/file lab-root "doc-drafts")])]
    {:id "f4"
     :exists true
     :last_modified (or last-modified "(none)")
     :prototypes prototypes
     :missing_evidence missing
     :todo_count 0
     :lab_raw_count raw-count
     :lab_stub_count stub-count
     :lab_doc_draft_count draft-count
     :media_unpersisted media-unpersisted}))

(defn- write-edn [path data]
  (io/make-parents path)
  (spit path (pr-str data)))

(defn -main [& args]
  (let [{:keys [help unknown repo-root output media-index]} (parse-args args)]
    (cond
      help (usage)
      unknown (do (println "Unknown argument:" unknown) (usage) (System/exit 1))
      :else
      (let [repo-root (or repo-root (System/getProperty "user.dir"))
            media-index (locate-media-index repo-root media-index)
            output (or output (str (io/file repo-root "boundary.edn")))
            data {:generated_at (now-iso)
                  :futons [(boundary-entry repo-root media-index)]}]
        (write-edn output data)
        (println (format "[boundary] wrote %s" output))))))

(apply -main *command-line-args*)
