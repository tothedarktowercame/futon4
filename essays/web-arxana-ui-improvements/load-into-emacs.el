;;; load-into-emacs.el --- One-shot loader to register the WebArxana UI Improvements mission HEAD with the Arxana Essays browser -*- lexical-binding: t; -*-

;; Author: claude-7 for Joe (eoi-engine eoi-mission-head run, emacs-claude-repl)
;; Date: 2026-05-30
;; Purpose: Make the WebArxana UI Improvements mission HEAD appear in
;;          arxana://view/essays-home so Joe can inspect the assembled
;;          artefact and attach annotations / rewriting guidance in the
;;          reader.  The source markdown is the mission doc itself
;;          (holes/missions/M-web-arxana-ui-improvements.md); per the
;;          Arxana per-repo convention it stays in place and :source-file
;;          carries the absolute path.
;;
;; To use:  M-x load-file RET /home/joe/code/futon4/essays/web-arxana-ui-improvements/load-into-emacs.el RET
;;
;; What it does (runtime-only; does NOT modify init.el or the .el source):
;;   1. Add the manifest path to `arxana-browser-essays-manifest-files'.
;;   2. Load the manifest file so the defconst is bound in the running Emacs.
;;   3. Add a catalog entry for arxana://view/essays-home.
;;   4. Build the manifest via `arxana-browser-essays--load-manifest-file'
;;      (which folds in the sibling authoritative annotations.edn per
;;      README-essays.md), then single-manifest import — upserting the
;;      essay + section entities into XTDB with :source-file populated.
;;
;; After load: refresh arxana://view/essays-home, click into "WebArxana UI
;; Improvements — Mission HEAD (Right View, 2026-05-30)", navigate by
;; section, and extend annotations in the annotations.edn (the authoritative
;; layer) — not in this .el.  Verify any ingest with
;; `M-x arxana-browser-essays-audit-passages' (the source .md uses flat `##'
;; headings; verbatim :passage anchors live in annotations.edn).

;;; Code:

(require 'arxana-browser-essays)

(add-to-list 'arxana-browser-essays-manifest-files
             "/home/joe/code/futon4/essays/web-arxana-ui-improvements/annotations.el"
             t)

(load "/home/joe/code/futon4/essays/web-arxana-ui-improvements/annotations.el" nil nil t)

(add-to-list 'arxana-browser-essays-catalogs
             '(:id web-arxana-ui-improvements-v1
               :label "WebArxana UI Improvements — Mission HEAD (Right View)"
               :description "HEAD of M-web-arxana-ui-improvements (futon4), assembled 2026-05-30 via simulated `eoi new' eoi-mission-head flash (Right-View-only; round-collapse false). Invariant: 'WebArxana provides a coherent visual interface to live graph-structured information about the stack, both for reading and writing.' Two address-now commitments (the Emacs<->browser live contract; Evidence-Store<->Mission integration as the proof-point) and four carried-forward tensions (Interest Network feature-vs-filter; Mission Search approaches; Meaning Map warm-up; unfilled HEAD slots). Loaded into Arxana Essays for annotation and IDENTIFY-phase work."
               :essay-id "arxana/essay/web-arxana-ui-improvements-v1"
               :manifest-symbol arxana-browser-essays-web-arxana-ui-improvements-manifest
               :manifest-file "/home/joe/code/futon4/essays/web-arxana-ui-improvements/annotations.el"
               :source-file "/home/joe/code/futon4/holes/missions/M-web-arxana-ui-improvements.md")
             t)

(unless (arxana-store-ensure-sync)
  (user-error "Futon sync is disabled; enable futon4-enable-sync first"))

(let* ((manifest (arxana-browser-essays--load-manifest-file
                  "/home/joe/code/futon4/essays/web-arxana-ui-improvements/annotations.el"))
       (summary (arxana-browser-essays--import-manifest
                 manifest
                 "/home/joe/code/futon4/essays/web-arxana-ui-improvements/annotations.el"
                 nil)))
  (message "Imported Essay %s: %d section%s, %d annotation%s. Refresh arxana://view/essays-home."
           (plist-get summary :essay)
           (plist-get summary :section-count)
           (if (= (plist-get summary :section-count) 1) "" "s")
           (plist-get summary :annotation-count)
           (if (= (plist-get summary :annotation-count) 1) "" "s")))

(provide 'web-arxana-ui-improvements-load-into-emacs)
;;; load-into-emacs.el ends here
