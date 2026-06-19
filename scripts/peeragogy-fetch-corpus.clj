#!/usr/bin/env bb
;; peeragogy-fetch-corpus.clj
;;
;; Pull live Wikibooks wikitext for every section listed in the Peeragogy
;; Handbook book manifest, writing one .mw file per section into
;; futon4/data/essays/wikibooks/peeragogy-handbook/wikitext/.
;;
;; This makes the .mw files the canonical Essays-side source.  No format
;; conversion is performed; what we read is what's live.  Drift between
;; cached and live can be checked later (drift-check, TBD).
;;
;; Usage:
;;   bb scripts/peeragogy-fetch-corpus.clj
;;     (fetches all; writes to wikitext/ ; reports per-section status)
;;   bb scripts/peeragogy-fetch-corpus.clj --only <substring>
;;     (limits to sections whose slug contains <substring>)
;;
;; Mission: M-peeragogy-rewrite.

(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[babashka.http-client :as http]
         '[cheshire.core :as json])

(def WIKI-API "https://en.wikibooks.org/w/api.php")
(def REPO-ROOT (or (System/getenv "FUTON4_ROOT") "/home/joe/code/futon4"))
(def MANIFEST
  (str REPO-ROOT
       "/data/essays/wikibooks/peeragogy-handbook/peeragogy-handbook-book-manifest.el"))
(def WIKITEXT-DIR
  (str REPO-ROOT
       "/data/essays/wikibooks/peeragogy-handbook/wikitext"))

(defn- api-get [params]
  (let [params (assoc params :format "json" :maxlag 5)
        url (str WIKI-API "?"
                 (str/join "&" (for [[k v] params]
                                 (str (name k) "=" (java.net.URLEncoder/encode (str v) "UTF-8")))))
        resp (http/get url {:headers {"User-Agent" "futon-peeragogy/0.1 (Joe Corneli operator)"}
                            :throw false})
        body (:body resp)]
    (try
      (json/parse-string body true)
      (catch Exception _
        {:_non-json-body body :_status (:status resp)}))))

(defn fetch-wikitext
  "Returns wikitext string for PAGE-TITLE, or nil if missing.
   Retries up to 3x on non-JSON / API-error responses with backoff."
  [page-title]
  (loop [attempt 1]
    (let [resp (api-get {:action "query"
                         :prop "revisions"
                         :titles page-title
                         :rvprop "content|timestamp"
                         :rvslots "main"
                         :redirects 1})]
      (cond
        (:_non-json-body resp)
        (if (>= attempt 5)
          (do (binding [*out* *err*]
                (println (str "[fetch-corpus] non-JSON after 5 tries for " page-title
                              " (status " (:_status resp) ")")))
              nil)
          (let [backoff (* 5000 attempt)]
            (binding [*out* *err*]
              (println (str "[fetch-corpus] retry " attempt " for " page-title
                            " in " (/ backoff 1000) "s (status "
                            (:_status resp) ")")))
            (Thread/sleep backoff)
            (recur (inc attempt))))
        (:error resp)
        (do (binding [*out* *err*]
              (println (str "[fetch-corpus] API error for " page-title ": "
                            (pr-str (:error resp)))))
            nil)
        :else
        (let [page (-> resp :query :pages vals first)
              rev (first (:revisions page))]
          (when rev
            {:wikitext (get-in rev [:slots :main :*])
             :timestamp (:timestamp rev)}))))))

(defn parse-manifest-sections
  "Return seq of {:page-title ... :source-file ... :slug ...} parsed from
   the book manifest.  Regex extraction; the manifest format is regular."
  []
  (let [text (slurp MANIFEST)
        ;; (page-title . "...") followed shortly by (source-file . "...")
        re #"\(page-title\s*\.\s*\"([^\"]+)\"\)[^()]*?\(source-file\s*\.\s*\"([^\"]+)\"\)"]
    (for [[_ page src-path] (re-seq re text)]
      (let [src-base (.getName (io/file src-path))
            slug (str/replace src-base #"\.(?:mw|md)$" "")]
        {:page-title page
         :source-file src-path
         :slug slug}))))

(defn -main [& args]
  (let [only (when (and (= "--only" (first args)) (second args)) (second args))
        all-sections (parse-manifest-sections)
        targets (cond->> all-sections
                  only (filter #(str/includes? (:slug %) only)))]
    (when (seq targets)
      (.mkdirs (io/file WIKITEXT-DIR)))
    (println (str "[fetch-corpus] " (count targets) " section(s) to fetch"
                  (when only (str " (filtered by --only " only ")"))))
    (let [results
          (doall
           (for [{:keys [page-title slug]} targets]
             (let [out-path (str WIKITEXT-DIR "/" slug ".mw")]
               (cond
                 (and (.exists (io/file out-path))
                      (not (some #{"--force"} args)))
                 (do (println (str "[fetch-corpus] " (format "%6d" (.length (io/file out-path)))
                                   " bytes  CACHED              " slug ".mw"))
                     {:slug slug :status :cached})
                 :else
                 (do (Thread/sleep 1500) ;; polite pacing for the API
                     (let [fetched (fetch-wikitext page-title)]
                       (cond
                         (nil? fetched)
                         (do (binding [*out* *err*]
                               (println (str "[fetch-corpus] MISSING " page-title)))
                             {:slug slug :page-title page-title :status :missing})
                         :else
                         (let [{:keys [wikitext timestamp]} fetched]
                           (spit out-path wikitext)
                           (println (str "[fetch-corpus] " (format "%6d" (count wikitext))
                                         " bytes  " timestamp "  " slug ".mw"))
                           {:slug slug :page-title page-title :status :ok
                            :bytes (count wikitext) :timestamp timestamp}))))))))]
      (binding [*out* *err*]
        (println (str "\n[fetch-corpus] DONE. "
                      (count (filter #(= :ok (:status %)) results)) " fetched, "
                      (count (filter #(= :cached (:status %)) results)) " cached, "
                      (count (filter #(= :missing (:status %)) results)) " missing."))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
