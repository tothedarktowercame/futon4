#!/usr/bin/env clojure

(require '[clojure.data.json :as json])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

;; TODO(org-sync): Track the def-doc backfill pipeline in the literate docs.

(defn usage []
  (println "Usage: dev/def-doc-backfill.clj [--root PATH] [--book BOOK]")
  (println "                               [--force] [--dry-run] [--help]")
  (println)
  (println "Backfills doc/defs + doc/def-count from doc/context bullets."))

(defn parse-args [args]
  (loop [opts {:dry-run false :force false}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--root" (recur (assoc opts :root (second remaining)) (nnext remaining))
        "--book" (recur (assoc opts :book (second remaining)) (nnext remaining))
        "--force" (recur (assoc opts :force true) (rest remaining))
        "--dry-run" (recur (assoc opts :dry-run true) (rest remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn- read-json [path]
  (with-open [r (io/reader path)]
    (json/read r :key-fn keyword)))

(defn- write-json [path data]
  (with-open [w (io/writer path)]
    (json/write data w :pretty true)))

(defn- parse-defs [text]
  (let [lines (str/split-lines (or text ""))]
    (->> lines
         (map str/trim)
         (remove str/blank?)
         (map (fn [line]
                (when-let [m (re-find #"^-\s*([^:]+):\s*(.*)$" line)]
                  {:def/name (str/trim (nth m 1))
                   :def/summary (str/trim (nth m 2))})))
         (remove nil?)
         vec)))

(defn- entry-files [root book]
  (let [dir (io/file root "dev" "logs" "books" book "raw")]
    (->> (file-seq dir)
         (filter #(.isFile ^java.io.File %))
         (filter #(str/ends-with? (.getName ^java.io.File %) ".json")))))

(defn -main [& args]
  (let [{:keys [help unknown root book force dry-run]}
        (merge {:root "." :book "futon4"} (parse-args args))]
    (cond
      help (usage)
      unknown (do (println "Unknown argument:" unknown) (usage) (System/exit 1))
      :else
      (let [files (entry-files root book)]
        (doseq [f files]
          (let [data (read-json f)
                has-defs (contains? data :doc/defs)
                context (or (:doc/context data) (get data "doc/context"))
                defs (parse-defs context)
                updated (cond-> data
                          (or force (not has-defs))
                          (assoc :doc/defs defs
                                 :doc/def-count (count defs)))]
            (when (or force (not has-defs))
              (if dry-run
                (println (format "[def-doc] %s defs=%d"
                                 (.getPath f) (count defs)))
                (do
                  (write-json f updated)
                  (println (format "[def-doc] backfilled %s defs=%d"
                                   (.getPath f) (count defs))))))))))))

(apply -main *command-line-args*)
