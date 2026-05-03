#!/usr/bin/env bb
;; peeragogy-flip-to-mw.clj
;;
;; One-shot flip of the Peeragogy Handbook Essays-side from Markdown
;; (lossy round-trip via Pandoc) to wikitext (canonical, lossless).
;;
;; Operations (idempotent — re-running on a flipped tree is a no-op):
;;
;;   1. Per-chapter manifests
;;        :source-file "<slug>.md" → :source-file "wikitext/<slug>.mw"
;;
;;   2. Book manifest
;;        (source-file . "/abs/.../<slug>.md")
;;          → (source-file . "/abs/.../wikitext/<slug>.mw")
;;
;;   3. Book-level annotations sidecar
;;        peeragogy-handbook-book-annotations.el → import-archive/...md.el
;;        peeragogy-handbook-book-annotations-mw.el
;;          → peeragogy-handbook-book-annotations.el (renamed; defconst
;;            and provide names rewritten back to the canonical base)
;;
;;   4. .md files moved to import-archive/.  The wikitext/<slug>.mw files
;;      become the sole canonical source.
;;
;; Mission: M-peeragogy-rewrite (Phase 3 of the .mw flip).

(require '[clojure.string :as str]
         '[clojure.java.io :as io])

(def REPO-ROOT (or (System/getenv "FUTON4_ROOT") "/home/joe/code/futon4"))
(def BOOK-DIR  (str REPO-ROOT "/data/essays/wikibooks/peeragogy-handbook"))
(def WIKITEXT-DIR (str BOOK-DIR "/wikitext"))
(def ARCHIVE-DIR  (str BOOK-DIR "/import-archive"))
(def ANNOTATIONS-CANON   (str BOOK-DIR "/peeragogy-handbook-book-annotations.el"))
(def ANNOTATIONS-MW      (str BOOK-DIR "/peeragogy-handbook-book-annotations-mw.el"))
(def ANNOTATIONS-ARCHIVE (str ARCHIVE-DIR "/peeragogy-handbook-book-annotations.md.el"))

(defn- abs-md->mw
  "Map an absolute .md path to its canonical wikitext counterpart."
  [abs-md]
  (let [base (.getName (io/file abs-md))
        mw-name (str/replace base #"\.md$" ".mw")]
    (str WIKITEXT-DIR "/" mw-name)))

(defn- ensure-dirs! []
  (doseq [d [ARCHIVE-DIR]]
    (.mkdirs (io/file d))))

(defn- step-1-per-chapter-manifests! []
  (let [files (->> (.listFiles (io/file BOOK-DIR))
                   (filter #(let [n (.getName %)]
                              (and (str/ends-with? n "-manifest.el")
                                   (not= n "peeragogy-handbook-book-manifest.el"))))
                   (map #(.getAbsolutePath %)))]
    (doseq [path files]
      (let [text (slurp path)
            ;; :source-file "slug.md" → :source-file "wikitext/slug.mw"
            new-text
            (str/replace
             text
             #":source-file\s+\"([^\"]+)\.md\""
             (fn [[_ stem]]
               (str ":source-file \"wikitext/" stem ".mw\"")))]
        (cond
          (= text new-text)
          (println (str "[flip] manifest already flipped: " (.getName (io/file path))))
          :else
          (do (spit path new-text)
              (println (str "[flip] flipped manifest: " (.getName (io/file path))))))))))

(defn- step-2-book-manifest! []
  (let [path (str BOOK-DIR "/peeragogy-handbook-book-manifest.el")
        text (slurp path)
        ;; (source-file . "/abs/.../<slug>.md") → wikitext/<slug>.mw
        new-text
        (str/replace
         text
         #"\(source-file\s*\.\s*\"([^\"]+)/([^/\"]+)\.md\"\)"
         (fn [[_ dir stem]]
           (str "(source-file . \"" dir "/wikitext/" stem ".mw\")")))]
    (cond
      (= text new-text)
      (println "[flip] book manifest already flipped")
      :else
      (do (spit path new-text)
          (println "[flip] flipped book manifest")))))

(defn- step-3-annotations! []
  (cond
    (not (.exists (io/file ANNOTATIONS-MW)))
    (println (str "[flip] no -mw.el file present; skipping annotations rename"))

    (and (.exists (io/file ANNOTATIONS-MW))
         (not (.exists (io/file ANNOTATIONS-CANON))))
    (do (println "[flip] annotations already flipped (canonical absent, mw present?)")
        ;; defensive: if canonical missing, still rename mw→canon
        (let [text (slurp ANNOTATIONS-MW)
              cleaned (-> text
                          (str/replace
                           "peeragogy-handbook-book-annotations-mw.el --- Review annotations (wikitext-anchored) for Peeragogy Handbook"
                           "peeragogy-handbook-book-annotations.el --- Review annotations (wikitext-anchored) for Peeragogy Handbook")
                          (str/replace "(defconst peeragogy-handbook-book-annotations-mw"
                                       "(defconst peeragogy-handbook-book-annotations")
                          (str/replace "(provide 'peeragogy-handbook-book-annotations-mw)"
                                       "(provide 'peeragogy-handbook-book-annotations)"))]
          (spit ANNOTATIONS-CANON cleaned)
          (.delete (io/file ANNOTATIONS-MW))
          (println "[flip] mw → canonical (canonical was absent)")))

    :else
    ;; both files present: archive canonical, then rename mw → canonical
    (do (.renameTo (io/file ANNOTATIONS-CANON) (io/file ANNOTATIONS-ARCHIVE))
        (let [text (slurp ANNOTATIONS-MW)
              cleaned (-> text
                          (str/replace
                           "peeragogy-handbook-book-annotations-mw.el --- Review annotations (wikitext-anchored) for Peeragogy Handbook"
                           "peeragogy-handbook-book-annotations.el --- Review annotations (wikitext-anchored) for Peeragogy Handbook")
                          (str/replace "(defconst peeragogy-handbook-book-annotations-mw"
                                       "(defconst peeragogy-handbook-book-annotations")
                          (str/replace "(provide 'peeragogy-handbook-book-annotations-mw)"
                                       "(provide 'peeragogy-handbook-book-annotations)"))]
          (spit ANNOTATIONS-CANON cleaned)
          (.delete (io/file ANNOTATIONS-MW))
          (println (str "[flip] archived old annotations → " (.getName (io/file ANNOTATIONS-ARCHIVE))))
          (println "[flip] renamed -mw.el → canonical .el")))))

(defn- step-4-archive-md! []
  ;; Only archive .md files that have a wikitext/<same-stem>.mw counterpart.
  ;; This avoids vacuuming up unrelated .md files (review notes, READMEs).
  (let [md-files (->> (.listFiles (io/file BOOK-DIR))
                      (filter #(str/ends-with? (.getName %) ".md"))
                      (filter (fn [f]
                                (let [stem (str/replace (.getName f) #"\.md$" "")
                                      mw-path (str WIKITEXT-DIR "/" stem ".mw")]
                                  (.exists (io/file mw-path))))))]
    (doseq [f md-files]
      (let [dest (io/file ARCHIVE-DIR (.getName f))]
        (cond
          (.exists dest)
          (do (.delete f)
              (println (str "[flip] removed (already archived): " (.getName f))))
          :else
          (do (.renameTo f dest)
              (println (str "[flip] archived: " (.getName f)))))))))

(defn -main [& _args]
  (ensure-dirs!)
  (println "[flip] Phase 3 (manifest rewiring + annotations rename) + Phase 4 (.md archive)")
  (println (str "[flip] book dir: " BOOK-DIR))
  (println)
  (println "Step 1 — per-chapter manifests")
  (step-1-per-chapter-manifests!)
  (println)
  (println "Step 2 — book manifest")
  (step-2-book-manifest!)
  (println)
  (println "Step 3 — annotations sidecar")
  (step-3-annotations!)
  (println)
  (println "Step 4 — archive .md imports")
  (step-4-archive-md!)
  (println)
  (println "[flip] DONE.  Next: re-eval the affected manifests + the wikibooks browser in Emacs."))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
