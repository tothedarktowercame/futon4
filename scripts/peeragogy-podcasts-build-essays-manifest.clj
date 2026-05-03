#!/usr/bin/env bb
;; peeragogy-podcasts-build-essays-manifest.clj
;;
;; Generate an Essays-side catalog for the Peeragogy Podcast transcripts,
;; sibling to the Handbook catalog under `data/essays/wikibooks/`.
;;
;; Inputs:
;;   futon4/holes/labs/M-peeragogy-rewrite/podcasts/manifest.json
;;     (one entry per episode, with :title :url :duration_seconds
;;      :transcript_path; produced by peeragogy-podcasts.py)
;;   transcripts on disk under that lab dir; referenced via absolute
;;   path from the Essays catalog (canonical source remains in labs/).
;;
;; Outputs:
;;   data/essays/podcasts/peeragogy-podcasts/peeragogy-podcasts-book-manifest.el
;;     Book-level manifest with one :section per episode; each section's
;;     props carry youtube-id / youtube-url / duration-seconds plus the
;;     absolute path to the transcript file (render-whole-file).
;;   data/essays/podcasts/peeragogy-podcasts/peeragogy-podcasts-book-annotations.el
;;     Empty annotations sidecar (defconst structure matches the
;;     Handbook's; ready for the three-axis annotation pass).
;;
;; Mission: M-peeragogy-rewrite (MAP, semi-in/semi-out source).

(require '[clojure.string :as str]
         '[clojure.java.io :as io]
         '[cheshire.core :as json])

(def REPO-ROOT (or (System/getenv "FUTON4_ROOT") "/home/joe/code/futon4"))
(def LABS-MANIFEST
  (str REPO-ROOT
       "/holes/labs/M-peeragogy-rewrite/podcasts/manifest.json"))
(def LABS-PODCASTS-DIR
  (str REPO-ROOT "/holes/labs/M-peeragogy-rewrite/podcasts"))
(def OUT-DIR
  (str REPO-ROOT "/data/essays/podcasts/peeragogy-podcasts"))
(def MANIFEST-OUT
  (str OUT-DIR "/peeragogy-podcasts-book-manifest.el"))
(def ANNOTATIONS-OUT
  (str OUT-DIR "/peeragogy-podcasts-book-annotations.el"))

(defn slugify [s]
  (-> s
      str/lower-case
      (str/replace #"[^a-z0-9]+" "-")
      (str/replace #"^-+|-+$" "")))

(defn elisp-escape [s]
  (-> s
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")))

(defn build-section [{:keys [index title url id duration_seconds transcript_path]}]
  (let [slug (slugify title)
        ;; full absolute path to the transcript .txt file
        abs-transcript (str LABS-PODCASTS-DIR "/" transcript_path)
        section-id (format "arxana/essay/podcasts/book/peeragogy-podcasts/section/%d-%s"
                           index slug)]
    (format
     (str "      (:id \"%s\"\n"
          "       :name \"%s\"\n"
          "       :type \"arxana/essay-section\"\n"
          "       :props ((index . %d)\n"
          "               (heading-level . 1)\n"
          "               (heading-text . \"%s\")\n"
          "               (youtube-id . \"%s\")\n"
          "               (youtube-url . \"%s\")\n"
          "               (duration-seconds . %d)\n"
          "               (source-file . \"%s\")\n"
          "               (render-whole-file . t)))")
     section-id
     (elisp-escape title)
     index
     (elisp-escape title)
     id
     url
     (or duration_seconds 0)
     abs-transcript)))

(defn build-manifest [episodes]
  (let [sections (->> episodes
                      (sort-by :index)
                      (map build-section)
                      (str/join "\n"))]
    (str
     ";;; peeragogy-podcasts-book-manifest.el --- Generated podcasts essay manifest -*- lexical-binding: t; -*-\n\n"
     ";;; Code:\n\n"
     "(defconst arxana-browser-essays-podcasts-peeragogy-podcasts-book-manifest\n"
     "  '(:version 1\n"
     "    :essay (:id \"arxana/essay/podcasts/book/peeragogy-podcasts\"\n"
     "            :name \"Peeragogy Podcast\"\n"
     "            :type \"arxana/essay\"\n"
     "            :props ((source . \"youtube-playlist\")\n"
     "                    (playlist-id . \"PLG6fmEnfJR2yaWGiK0tSp8QSis4btdCzE\")\n"
     "                    (playlist-url . \"https://www.youtube.com/playlist?list=PLG6fmEnfJR2yaWGiK0tSp8QSis4btdCzE\")\n"
     "                    (podcasts-catalog . t)\n"
     "                    (transcribed-by . \"faster-whisper-small (local) via futon4/scripts/peeragogy-podcasts.py\")\n"
     "                    (imported . \"2026-04-29\")))\n"
     "    :sections\n"
     "     (\n"
     sections "\n"
     "     )\n"
     "    :annotations nil))\n\n"
     "(provide 'arxana-browser-essays-podcasts-peeragogy-podcasts-book-manifest)\n")))

(defn build-annotations []
  (str
   ";;; peeragogy-podcasts-book-annotations.el --- Annotations for Peeragogy Podcast transcripts -*- lexical-binding: t; -*-\n\n"
   ";;; Commentary:\n"
   ";; Stand-off review notes for the Peeragogy Podcast transcripts.\n"
   ";; Loaded as a sidecar by `arxana-browser-essays-podcasts.el' so they\n"
   ";; survive future re-transcription from the YouTube playlist.\n"
   ";;\n"
   ";; Initial state: no annotations.  The three-axis sweep\n"
   ";; (writing-coherence / critique / peeragogy / collaboration-coherence)\n"
   ";; populates this file as MAP applies the 2016-prior rubric to the\n"
   ";; transcripts.\n\n"
   ";;; Code:\n\n"
   "(defconst peeragogy-podcasts-book-annotations\n"
   "  '(:annotations ()))\n\n"
   "(provide 'peeragogy-podcasts-book-annotations)\n"))

(defn -main [& _args]
  (let [m (json/parse-string (slurp LABS-MANIFEST) true)
        episodes (map (fn [[k v]] (assoc v :id (name k)))
                      (:episodes m))]
    (when (empty? episodes)
      (binding [*out* *err*]
        (println "[essays-manifest] no episodes found in" LABS-MANIFEST))
      (System/exit 1))
    (.mkdirs (io/file OUT-DIR))
    (spit MANIFEST-OUT (build-manifest episodes))
    (spit ANNOTATIONS-OUT (build-annotations))
    (println (str "[essays-manifest] wrote " MANIFEST-OUT
                  " (" (count episodes) " sections)"))
    (println (str "[essays-manifest] wrote " ANNOTATIONS-OUT " (empty)"))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
