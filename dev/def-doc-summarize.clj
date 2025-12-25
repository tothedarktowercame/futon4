#!/usr/bin/env clojure

(require '[clojure.data.json :as json])
(require '[clojure.java.io :as io])
(require '[clojure.string :as str])
(require '[futon5.llm.relay :as relay])

;; TODO(org-sync): Track the def-doc summarizer pipeline in the literate docs.

(defn usage []
  (println "Usage: dev/def-doc-summarize.clj [--root PATH] [--dev-dir PATH] [--prompt PATH]")
  (println "                                [--model MODEL] [--book BOOK] [--version VERSION]")
  (println "                                [--limit N] [--dry-run] [--help]")
  (println)
  (println "Summarizes dev/*.el files into XTDB-ready docbook JSON entries."))

(defn parse-args [args]
  (loop [opts {:dry-run false}
         remaining args]
    (if-let [arg (first remaining)]
      (case arg
        "--root" (recur (assoc opts :root (second remaining)) (nnext remaining))
        "--dev-dir" (recur (assoc opts :dev-dir (second remaining)) (nnext remaining))
        "--prompt" (recur (assoc opts :prompt (second remaining)) (nnext remaining))
        "--model" (recur (assoc opts :model (second remaining)) (nnext remaining))
        "--book" (recur (assoc opts :book (second remaining)) (nnext remaining))
        "--version" (recur (assoc opts :version (second remaining)) (nnext remaining))
        "--limit" (recur (assoc opts :limit (Integer/parseInt (second remaining)))
                         (nnext remaining))
        "--dry-run" (recur (assoc opts :dry-run true) (rest remaining))
        "--help" (recur (assoc opts :help true) (rest remaining))
        (recur (assoc opts :unknown arg) (rest remaining)))
      opts)))

(defn- sha1 [^String value]
  (let [digest (java.security.MessageDigest/getInstance "SHA-1")]
    (.update digest (.getBytes value "UTF-8"))
    (format "%040x" (BigInteger. 1 (.digest digest)))))

(defn- short-hash [value]
  (subs (sha1 value) 0 12))

(defn- now-ts []
  (-> (java.time.Instant/now)
      (.toString)))

(defn- response->text [resp]
  (let [choices (or (get-in resp [:body "choices"])
                    (get-in resp [:body :choices]))]
    (if (seq choices)
      (or (get-in (first choices) ["message" "content"])
          (get-in (first choices) [:message :content])
          (json/write-str (:body resp)))
      (json/write-str (:body resp)))))

(defn- strip-code-blocks [text]
  (-> text
      (str/replace #"(?s)```.*?```" "")
      (str/replace #"(?is)#\\+BEGIN_SRC.*?#\\+END_SRC" "")
      (str/trim)))

(defn- normalize-bullets [text]
  (let [lines (str/split-lines text)
        bullets (->> lines
                     (map str/trim)
                     (remove str/blank?)
                     (map (fn [line]
                            (cond
                              (str/starts-with? line "- ") (subs line 2)
                              (str/starts-with? line "* ") (subs line 2)
                              (str/starts-with? line "• ") (subs line 2)
                              (re-matches #"^\\d+\\.\\s+.*" line)
                              (str/replace line #"^\\d+\\.\\s+" "")
                              :else nil)))
                     (remove nil?)
                     (map #(str "- " %)))]
    (vec bullets)))

(defn- parse-defs [bullets]
  (->> bullets
       (map (fn [line]
              (when-let [m (re-find #"^-\s*([^:]+):\s*(.*)$" line)]
                {"def/name" (str/trim (nth m 1))
                 "def/summary" (str/trim (nth m 2))})))
       (remove nil?)
       vec))

(defn- doc-id [book outline-path]
  (let [path-str (if (sequential? outline-path)
                   (str/join "/" outline-path)
                   (or outline-path ""))]
    (format "%s-%s" book (short-hash (format "%s::%s" book path-str)))))

(defn- list-el-files [root dev-dir]
  (let [dev-root (io/file root dev-dir)]
    (->> (file-seq dev-root)
         (filter #(.isFile ^java.io.File %))
         (filter #(str/ends-with? (.getName ^java.io.File %) ".el"))
         (map #(.getCanonicalPath ^java.io.File %))
         sort
         vec)))

(defn- rel-path [root path]
  (let [root-file (.getCanonicalFile (io/file root))
        file-file (.getCanonicalFile (io/file path))
        root-path (.toPath root-file)
        file-path (.toPath file-file)]
    (str (.relativize root-path file-path))))

(defn- toc-headings [book rel-files]
  (let [top-outline ["Dev docs"]
        top-doc-id (doc-id book top-outline)]
    (concat
     [{:doc_id top-doc-id
       :title "Dev docs"
       :outline_path top-outline
       :path_string "Dev docs"
       :level 1}]
     (map (fn [rel]
            (let [outline ["Dev docs" rel]
                  title rel]
              {:doc_id (doc-id book outline)
               :title title
               :outline_path outline
               :path_string (str "Dev docs / " rel)
               :level 2}))
          rel-files))))

(defn- ensure-dir [path]
  (io/make-parents (io/file path "placeholder"))
  path)

(defn- doc-entry [{:keys [book doc-id outline-path version timestamp run-id source-path summary defs]}]
  {"book_id" book
   "doc_id" doc-id
   "version" version
   "replaces" nil
   "timestamp" timestamp
   "run_id" run-id
   "outline_path" outline-path
   "files_touched" [source-path]
   "doc/source-path" source-path
   "doc/defs" defs
   "doc/def-count" (count defs)
   "doc/context" summary})

(defn- summarize-file [{:keys [model system path rel]}]
  (let [content (slurp path)
        user (str "File: " rel "\n\n" content)
        payload (relay/chat-request {:model model
                                     :system system
                                     :user user})
        resp (relay/call-openai! payload)
        text (-> resp response->text strip-code-blocks)
        bullets (normalize-bullets text)
        selected (take 5 bullets)]
    (when (< (count selected) 5)
      (println (format "[def-doc] warning: %s returned %d bullets" rel (count selected))))
    {:bullets selected
     :defs (parse-defs selected)
     :text (str/join "\n" selected)}))

(defn -main [& args]
  (let [{:keys [help unknown root dev-dir prompt model book version limit dry-run]}
        (merge {:root "."
                :dev-dir "dev"
                :book "futon4"
                :version "def-doc"
                :model "gpt-5.2-chat-latest"}
               (parse-args args))]
    (cond
      help (usage)
      unknown (do (println "Unknown argument:" unknown) (usage) (System/exit 1))
      :else
      (let [root (io/file root)
            prompt-path (or prompt (str (io/file root "dev" "prompts" "def-doc-summary.prompt")))
            system (slurp prompt-path)
            files (list-el-files root dev-dir)
            files (if limit (take limit files) files)
            rels (mapv #(rel-path root %) files)
            base-dir (io/file root "dev" "logs" "books" book)
            raw-dir (io/file (ensure-dir (str (io/file base-dir "raw"))))
            toc-path (io/file base-dir "toc.json")
            timestamp (now-ts)]
        (when dry-run
          (println (format "[def-doc] dry-run mode; %d files queued" (count files))))
        (let [toc (toc-headings book rels)]
          (when-not dry-run
            (io/make-parents toc-path)
            (spit toc-path (json/write-str toc {:pretty true}))
            (println (format "[def-doc] toc=%s headings=%d" (.getPath toc-path) (count toc)))))
        (doseq [[path rel] (map vector files rels)]
          (let [outline ["Dev docs" rel]
                doc-id (doc-id book outline)
                run-id (str doc-id "-" (short-hash (str timestamp "|" rel)))
                summary (summarize-file {:model model :system system :path path :rel rel})
                entry (doc-entry {:book book
                                  :doc-id doc-id
                                  :outline-path outline
                                  :version version
                                  :timestamp timestamp
                                  :run-id run-id
                                  :source-path rel
                                  :summary (:text summary)
                                  :defs (:defs summary)})
                out-path (io/file raw-dir (str run-id ".json"))]
            (if dry-run
              (do
                (println (format "[def-doc] %s" rel))
                (println (:text summary)))
              (do
                (spit out-path (json/write-str entry {:pretty true}))
                (println (format "[def-doc] entry=%s" (.getPath out-path)))))))))))

(apply -main *command-line-args*)
