;;; arxana-browser-core.el --- Browser core for Arxana -*- lexical-binding: t; -*-

;;; Commentary:
;; Core browser rendering and navigation helpers.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'url-util)

(require 'arxana-store)
(require 'arxana-ui nil t)

(declare-function arxana-docbook--source-brief "arxana-docbook" (&optional book))
(declare-function arxana-docbook--data-source "arxana-docbook-remote" (&optional book))
(declare-function arxana-store-sync-enabled-p "arxana-store")
(declare-function arxana-patterns-ingest-language-rows "arxana-patterns-ingest")

(declare-function arxana-browser-code-items "arxana-browser-code")
(declare-function arxana-browser-code-format "arxana-browser-code")
(declare-function arxana-browser-code-row "arxana-browser-code")
(declare-function arxana-browser-code-open "arxana-browser-code")
(declare-function arxana-browser-graph-items "arxana-browser-graph")
(declare-function arxana-browser-graph-format "arxana-browser-graph")
(declare-function arxana-browser-graph-row "arxana-browser-graph")
(declare-function arxana-browser-graph-open "arxana-browser-graph")
(declare-function arxana-browser-hypergraph-items "arxana-browser-hypergraph")
(declare-function arxana-browser-hypergraph-format "arxana-browser-hypergraph")
(declare-function arxana-browser-hypergraph-row "arxana-browser-hypergraph")
(declare-function arxana-browser-hypergraph-open "arxana-browser-hypergraph")
(declare-function arxana-browser-songs-menu-items "arxana-browser-songs")
(declare-function arxana-browser-songs-items "arxana-browser-songs" (context))
(declare-function arxana-browser-songs-format "arxana-browser-songs")
(declare-function arxana-browser-songs-row "arxana-browser-songs" (item))
(declare-function arxana-browser-essays-menu-items "arxana-browser-essays")
(declare-function arxana-browser-essays-items "arxana-browser-essays" (context))
(declare-function arxana-browser-essays-format "arxana-browser-essays" (&optional context))
(declare-function arxana-browser-essays-row "arxana-browser-essays" (item))
(declare-function arxana-browser-essays-open "arxana-browser-essays" (item))
(declare-function arxana-browser-essays-location "arxana-browser-essays" (item))
(declare-function arxana-browser-songs-open "arxana-browser-songs" (item))
(declare-function arxana-browser-songs-location "arxana-browser-songs" (item))
(declare-function arxana-browser-chorus-menu-items "arxana-browser-chorus")
(declare-function arxana-browser-chorus-items "arxana-browser-chorus" (context))
(declare-function arxana-browser-chorus-format "arxana-browser-chorus")
(declare-function arxana-browser-chorus-row "arxana-browser-chorus" (item))
(declare-function arxana-browser-chorus-open "arxana-browser-chorus" (item))
(declare-function arxana-browser-chorus-location "arxana-browser-chorus" (item))

(declare-function arxana-browser-patterns--language-index-by-path "arxana-browser-patterns" (language-rows))
(declare-function arxana-browser-patterns--filesystem-collection-items "arxana-browser-patterns" (&optional language-index))
(declare-function arxana-browser-patterns--friendly-classification "arxana-browser-patterns" (value))
(declare-function arxana-browser-patterns--browser-pattern-items "arxana-browser-patterns" (language))
(declare-function arxana-browser-patterns--browser-pattern-row "arxana-browser-patterns" (item))
(declare-function arxana-browser-patterns--move-entry "arxana-browser-patterns" (entries old-index new-index))
(declare-function arxana-browser-patterns--flexiarg-apply-order "arxana-browser-patterns" (items))
(declare-function arxana-browser-patterns--language-apply-order "arxana-browser-patterns" (language items))
(declare-function arxana-browser-patterns-open "arxana-browser-patterns" (name))
(declare-function arxana-browser-patterns-edit-collection "arxana-browser-patterns" (&optional collection))
(declare-function arxana-browser-patterns--import-library "arxana-browser-patterns" (library &optional prompt))
(declare-function arxana-browser-patterns-add-collection-root "arxana-browser-patterns" (directory))

(declare-function arxana-browser--docbook-books "arxana-browser-docbook")
(declare-function arxana-browser--docbook-book-items "arxana-browser-docbook" (book))
(declare-function arxana-browser--docbook-contents-items "arxana-browser-docbook" (book))
(declare-function arxana-browser--docbook-section-items "arxana-browser-docbook" (book heading))
(declare-function arxana-browser--docbook-items "arxana-browser-docbook" (&optional book))
(declare-function arxana-browser--docbook-contents-row "arxana-browser-docbook" (item))
(declare-function arxana-browser--docbook-row "arxana-browser-docbook" (item))
(declare-function arxana-browser--docbook-contents-format "arxana-browser-docbook")
(declare-function arxana-browser--docbook-format "arxana-browser-docbook")
(declare-function arxana-browser--docbook-location "arxana-browser-docbook" (item))
(declare-function arxana-browser--docbook-contents-dirty-p "arxana-browser-docbook" (book))
(declare-function arxana-browser-docbook-open-book "arxana-browser-docbook")
(declare-function arxana-browser-docbook-open-section-context "arxana-browser-docbook")
(declare-function arxana-browser-docbook-move-item-up "arxana-browser-docbook")
(declare-function arxana-browser-docbook-move-item-down "arxana-browser-docbook")
(declare-function arxana-browser-docbook-move-section-up "arxana-browser-docbook")
(declare-function arxana-browser-docbook-move-section-down "arxana-browser-docbook")
(declare-function arxana-browser-docbook-move-top "arxana-browser-docbook")
(declare-function arxana-browser-docbook-move-bottom "arxana-browser-docbook")
(declare-function arxana-browser-docbook-export-org "arxana-browser-docbook")
(declare-function arxana-browser-docbook-export-pdf "arxana-browser-docbook")
(declare-function arxana-browser-docbook-sync-order "arxana-browser-docbook")
(declare-function arxana-browser-docbook-toggle-mark-at-point "arxana-browser-docbook")
(declare-function arxana-browser-docbook-remove-marked "arxana-browser-docbook")
(declare-function arxana-browser-docbook-hard-delete-marked "arxana-browser-docbook")

(declare-function arxana-browser-trace-home-items "arxana-browser-trace")
(declare-function arxana-browser-trace-home-format "arxana-browser-trace")
(declare-function arxana-browser-trace-home-row "arxana-browser-trace" (item))
(declare-function arxana-browser-trace-devmaps-items "arxana-browser-trace")
(declare-function arxana-browser-trace-devmaps-format "arxana-browser-trace")
(declare-function arxana-browser-trace-devmaps-row "arxana-browser-trace" (item))
(declare-function arxana-browser-trace-all-tensions-items "arxana-browser-trace")
(declare-function arxana-browser-trace-all-components-items "arxana-browser-trace")
(declare-function arxana-browser-trace-all-components-format "arxana-browser-trace")
(declare-function arxana-browser-trace-all-components-row "arxana-browser-trace" (item))
(declare-function arxana-browser-trace-tensions-items "arxana-browser-trace" (context))
(declare-function arxana-browser-trace-tensions-format "arxana-browser-trace")
(declare-function arxana-browser-trace-tensions-row "arxana-browser-trace" (item))
(declare-function arxana-browser-trace-gates-items "arxana-browser-trace" (context))
(declare-function arxana-browser-trace-gates-format "arxana-browser-trace")
(declare-function arxana-browser-trace-gates-row "arxana-browser-trace" (item))
(declare-function arxana-browser-trace-visit-devmap "arxana-browser-trace" (item))
(declare-function arxana-browser-trace-visit-tension "arxana-browser-trace" (item))
(declare-function arxana-browser-trace-visit-gate "arxana-browser-trace" (item))

(declare-function arxana-browser--lab-items "arxana-browser-lab")
(declare-function arxana-browser--lab-row "arxana-browser-lab" (item))
(declare-function arxana-browser--lab-file-row "arxana-browser-lab" (item))
(declare-function arxana-browser--lab-format "arxana-browser-lab")
(declare-function arxana-browser--lab-file-format "arxana-browser-lab")
(declare-function arxana-browser--lab-open-trace "arxana-browser-lab")
(declare-function arxana-browser--lab-open-raw "arxana-browser-lab")
(declare-function arxana-browser--lab-open-draft "arxana-browser-lab")
(declare-function arxana-browser--tensions-items "arxana-browser-lab")
(declare-function arxana-browser--tensions-row "arxana-browser-lab" (item))
(declare-function arxana-browser--tensions-format "arxana-browser-lab")
(declare-function arxana-browser-tension-open-entry "arxana-browser-lab" (item))
(declare-function arxana-browser--devmaps-items "arxana-browser-lab")
(declare-function arxana-browser--devmaps-row "arxana-browser-lab" (item))
(declare-function arxana-browser--devmaps-format "arxana-browser-lab")
(declare-function arxana-browser-devmap-open-entry "arxana-browser-lab" (item))
(declare-function arxana-browser--narrative-trail-items "arxana-browser-lab" (mission-id))

(declare-function arxana-browser--forum-items "arxana-browser-forum")
(declare-function arxana-browser--forum-row "arxana-browser-forum" (item))
(declare-function arxana-browser--forum-format "arxana-browser-forum")
(declare-function arxana-forum-open-thread "arxana-browser-forum" (item))
(declare-function arxana-forum-compose-for-current-thread "arxana-browser-forum")
(declare-function arxana-forum-stream-connect "arxana-browser-forum")

(declare-function arxana-browser--lab-menu-items "arxana-browser-lab")
(declare-function arxana-browser--lab-menu-row "arxana-browser-lab" (item))
(declare-function arxana-browser--lab-menu-format "arxana-browser-lab")
(declare-function arxana-browser--lab-sessions-active-items "arxana-browser-lab")
(declare-function arxana-browser--lab-sessions-active-row "arxana-browser-lab" (item))
(declare-function arxana-browser--lab-sessions-active-format "arxana-browser-lab")
(declare-function arxana-browser--pattern-activation-items "arxana-browser-pattern-activation")
(declare-function arxana-browser--pattern-activation-row "arxana-browser-pattern-activation" (item))
(declare-function arxana-browser--pattern-activation-format "arxana-browser-pattern-activation")
(declare-function arxana-browser--pattern-activation-detail-items "arxana-browser-pattern-activation")
(declare-function arxana-browser--pattern-activation-detail-row "arxana-browser-pattern-activation" (item))
(declare-function arxana-browser--pattern-activation-detail-format "arxana-browser-pattern-activation")
(declare-function arxana-browser-pattern-activation-headline "arxana-browser-pattern-activation")
(declare-function arxana-browser-pattern-activation-detail-headline "arxana-browser-pattern-activation")
(declare-function arxana-browser--lab-sessions-recent-items "arxana-browser-lab")
(declare-function arxana-browser--lab-sessions-raw-items "arxana-browser-lab")
(declare-function arxana-browser--lab-sessions-raw-row "arxana-browser-lab" (item))
(declare-function arxana-browser--lab-sessions-raw-format "arxana-browser-lab")
(declare-function arxana-browser--lab-sessions-archived-items "arxana-browser-lab")
(declare-function arxana-browser--lab-sessions-archived-row "arxana-browser-lab" (item))
(declare-function arxana-browser--lab-sessions-archived-format "arxana-browser-lab")
(declare-function arxana-browser-lab-open-session "arxana-browser-lab" (item))
(declare-function arxana-browser--evidence-timeline-items "arxana-browser-lab")
(declare-function arxana-browser--evidence-timeline-row "arxana-browser-lab" (item))
(declare-function arxana-browser--evidence-timeline-format "arxana-browser-lab")
(declare-function arxana-browser--evidence-sessions-items "arxana-browser-lab")
(declare-function arxana-browser--evidence-sessions-row "arxana-browser-lab" (item))
(declare-function arxana-browser--evidence-sessions-format "arxana-browser-lab")
(declare-function arxana-browser-evidence-open-entry "arxana-browser-lab" (item))
(declare-function arxana-browser-evidence-open-session "arxana-browser-lab" (item))
(declare-function arxana-browser-evidence-filter-by-type "arxana-browser-lab")
(declare-function arxana-browser-evidence-filter-by-author "arxana-browser-lab")
(declare-function arxana-browser-evidence-clear-filter "arxana-browser-lab")
(declare-function arxana-browser--evidence-fetch-single "arxana-browser-lab" (evidence-id))
(declare-function arxana-browser-lab--browse-evidence-entry "arxana-browser-lab" (evidence-id))
(declare-function arxana-browser-lab--browse-evidence-chain "arxana-browser-lab" (evidence-id))
(declare-function arxana-browser-operational-family-location "arxana-browser-lab" (item))

(declare-function arxana-browser--encyclopedia-items "arxana-browser-encyclopedia")
(declare-function arxana-browser--encyclopedia-row "arxana-browser-encyclopedia" (item))
(declare-function arxana-browser--encyclopedia-format "arxana-browser-encyclopedia")
(declare-function arxana-browser--encyclopedia-entries-items "arxana-browser-encyclopedia" (context))
(declare-function arxana-browser--encyclopedia-entries-row "arxana-browser-encyclopedia" (item))
(declare-function arxana-browser--encyclopedia-entries-format "arxana-browser-encyclopedia")
(declare-function arxana-browser--encyclopedia-entry-items "arxana-browser-encyclopedia" (context))
(declare-function arxana-browser--encyclopedia-entry-row "arxana-browser-encyclopedia" (item))
(declare-function arxana-browser--encyclopedia-entry-format "arxana-browser-encyclopedia")
(declare-function arxana-browser-encyclopedia-open-corpus "arxana-browser-encyclopedia" (item))
(declare-function arxana-browser-encyclopedia-open-entry "arxana-browser-encyclopedia" (item))

(declare-function arxana-browser-evidence-menu-items "arxana-browser-evidence")
(declare-function arxana-browser--evidence-timeline-items "arxana-browser-evidence")
(declare-function arxana-browser--evidence-timeline-row "arxana-browser-evidence" (item))
(declare-function arxana-browser--evidence-timeline-format "arxana-browser-evidence")
(declare-function arxana-browser--evidence-sessions-items "arxana-browser-evidence")
(declare-function arxana-browser--evidence-sessions-row "arxana-browser-evidence" (item))
(declare-function arxana-browser--evidence-sessions-format "arxana-browser-evidence")
(declare-function arxana-browser--evidence-open-sessions-items "arxana-browser-evidence")
(declare-function arxana-browser--evidence-open-sessions-row "arxana-browser-evidence" (item))
(declare-function arxana-browser--evidence-open-sessions-format "arxana-browser-evidence")
(declare-function arxana-evidence-toggle-open-session-mark-at-point "arxana-browser-evidence")
(declare-function arxana-evidence-delete-open-sessions "arxana-browser-evidence")
;; Mission Control browser
(declare-function arxana-browser--missions-portfolio-items "arxana-browser-missions")
(declare-function arxana-browser--missions-portfolio-row "arxana-browser-missions" (item))
(declare-function arxana-browser--missions-portfolio-format "arxana-browser-missions")
(declare-function arxana-browser--missions-by-status-items "arxana-browser-missions")
(declare-function arxana-browser--missions-by-status-row "arxana-browser-missions" (item))
(declare-function arxana-browser--missions-by-status-format "arxana-browser-missions")
(declare-function arxana-browser--missions-status-group-items "arxana-browser-missions" (context))
(declare-function arxana-browser-missions-open-entry "arxana-browser-missions" (item))
(declare-function arxana-browser-missions-open-status-group "arxana-browser-missions" (item))
(declare-function arxana-browser-missions-menu-items "arxana-browser-missions")
(declare-function arxana-browser--evidence-session-items "arxana-browser-evidence" (context))
(declare-function arxana-browser--evidence-session-chat-row "arxana-browser-evidence" (item))
(declare-function arxana-browser--evidence-session-chat-format "arxana-browser-evidence")
(declare-function arxana-browser--evidence-threads-items "arxana-browser-evidence")
(declare-function arxana-browser--evidence-threads-row "arxana-browser-evidence" (item))
(declare-function arxana-browser--evidence-threads-format "arxana-browser-evidence")
(declare-function arxana-browser--evidence-chain-items "arxana-browser-evidence" (context))
(declare-function arxana-browser--evidence-chain-row "arxana-browser-evidence" (item))
(declare-function arxana-browser--evidence-chain-format "arxana-browser-evidence")
(declare-function arxana-browser--evidence-thread-reader-items "arxana-browser-evidence" (context))
(declare-function arxana-browser--evidence-thread-reader-row "arxana-browser-evidence" (item))
(declare-function arxana-browser--evidence-thread-reader-format "arxana-browser-evidence")
(declare-function arxana-browser--evidence-entry-detail-items "arxana-browser-evidence" (context))
(declare-function arxana-browser--evidence-entry-detail-row "arxana-browser-evidence" (item))
(declare-function arxana-browser--evidence-entry-detail-format "arxana-browser-evidence")
(declare-function arxana-browser-evidence-open-session "arxana-browser-evidence" (item))
(declare-function arxana-browser-evidence-open-live-session "arxana-browser-evidence" (item))
(declare-function arxana-browser-evidence-open-thread "arxana-browser-evidence" (item))
(declare-function arxana-browser-evidence-open-entry "arxana-browser-evidence" (item))

(declare-function arxana-media--items "arxana-media")
(declare-function arxana-media--entries "arxana-media")
(declare-function arxana-media--track-items "arxana-media" (filter))
(declare-function arxana-media--project-items "arxana-media" (entries))
(declare-function arxana-media--item-play-path "arxana-media" (item))
(declare-function arxana-media--podcast-row "arxana-media" (item))
(declare-function arxana-media--podcast-format "arxana-media")
(declare-function arxana-media--publications-items "arxana-media")
(declare-function arxana-media--publication-track-items "arxana-media" (directory))
(declare-function arxana-media--ep-staging-items "arxana-media")
(declare-function arxana-media--misc-items "arxana-media")
(declare-function arxana-media--misc-track-items "arxana-media" (directory))
(declare-function arxana-media--podcast-items "arxana-media")
(declare-function arxana-media--track-row "arxana-media" (item))
(declare-function arxana-media--publication-track-row "arxana-media" (item))
(declare-function arxana-media--misc-track-row "arxana-media" (item))
(declare-function arxana-media--track-sha "arxana-media" (item))
(declare-function arxana-media--track-format "arxana-media")
(declare-function arxana-media--publication-track-format "arxana-media")
(declare-function arxana-media--misc-track-format "arxana-media")
(declare-function arxana-media-bounce-marked "arxana-media")
(declare-function arxana-media-bounce-or-up "arxana-media")
(declare-function arxana-media-retitle-at-point "arxana-media")
(declare-function arxana-media-play-at-point "arxana-media")
(declare-function arxana-media-stop-playback "arxana-media")
(declare-function arxana-media-toggle-autoplay-next "arxana-media")
(declare-function arxana-media-edit-lyrics-at-point "arxana-media")
(declare-function arxana-media-open-in-audacity "arxana-media")
(declare-function arxana-media-playback-pause-toggle "arxana-media")
(declare-function arxana-media-playback-seek-back-10 "arxana-media")
(declare-function arxana-media-playback-seek-forward-10 "arxana-media")
(declare-function arxana-media-playback-seek-back-30 "arxana-media")
(declare-function arxana-media-playback-seek-forward-30 "arxana-media")
(declare-function arxana-media-unmark-all "arxana-media")
(declare-function arxana-media-set-status-marked "arxana-media")
(declare-function arxana-media-publish-marked "arxana-media")
(declare-function arxana-media-set-publication-url "arxana-media")
(declare-function arxana-media-open-publication-url "arxana-media")
(declare-function arxana-media-create-ep-staging "arxana-media")
(declare-function arxana-media-stage-to-ep "arxana-media")
(declare-function arxana-media-delete-at-point "arxana-media")
(declare-function arxana-media-move-misc-to-ep-at-point "arxana-media")
(declare-function arxana-media-remove-from-ep-staging-at-point "arxana-media")
(declare-function arxana-media-toggle-mark-at-point "arxana-media")
(declare-function arxana-media-transcribe-and-podcast-at-point "arxana-media")

(declare-function arxana-browser-marks-items-in-context "arxana-browser-marks" (marks key-fn &optional filter-fn))
(declare-function arxana-browser-vsatarcs-items "arxana-browser-vsatarcs")
(declare-function arxana-browser-vsatarcs-format "arxana-browser-vsatarcs")
(declare-function arxana-browser-vsatarcs-row "arxana-browser-vsatarcs" (item))
(declare-function arxana-browser-vsatarcs-open "arxana-browser-vsatarcs" (item))
(declare-function arxana-browser-scans-items "arxana-browser-scans")
(declare-function arxana-browser-scans-format "arxana-browser-scans")
(declare-function arxana-browser-scans-row "arxana-browser-scans" (item))
(declare-function arxana-browser-scans-open "arxana-browser-scans" (item))

(defun arxana-browser--require-patterns ()
  "Ensure the pattern browser helpers are available."
  (unless (featurep 'arxana-browser-patterns)
    (require 'arxana-browser-patterns nil t))
  (unless (featurep 'arxana-browser-patterns)
    (user-error "Pattern browser helpers are unavailable; load arxana-browser-patterns")))
(defun arxana-browser--ensure-tabulated-list ()
  "Ensure `tabulated-list-mode-map` is available after hot reloads."
  (unless (boundp 'tabulated-list-mode-map)
    (when (locate-library "tabulated-list")
      (load "tabulated-list" nil t)))
  (unless (boundp 'tabulated-list-mode-map)
    (message "tabulated-list-mode-map still unavailable; reload tabulated-list.el")))

(defun arxana-browser--tabulated-list-guard (orig &rest args)
  (arxana-browser--ensure-tabulated-list)
  (apply orig args))

(unless (advice-member-p #'arxana-browser--tabulated-list-guard 'tabulated-list-mode)
  (advice-add 'tabulated-list-mode :around #'arxana-browser--tabulated-list-guard))

(defface arxana-browser--highlight
  '((t :inherit hl-line :background "#61CE3C"))
  "Face used to highlight the active row in the pattern browser."
  :group 'arxana-patterns)

(defconst arxana-browser-click-default
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (expand-file-name "../resources/sounds/arxana-click.wav" dir))
  "Default click sound distributed with Futon4.")

(defcustom arxana-browser-enable-click t
  "When non-nil, play a quiet click while moving within the pattern browser."
  :type 'boolean
  :group 'arxana-patterns)

(defcustom arxana-browser-click-sound arxana-browser-click-default
  "Audio file used for the pattern browser navigation click.
Set to nil to disable the bundled sound without turning off clicks entirely."
  :type '(choice (const :tag "No sound" nil)
                 file)
  :group 'arxana-patterns)

(defcustom arxana-browser-click-volume 0.25
  "Volume multiplier (0.0–1.0) for the navigation click."
  :type 'number
  :group 'arxana-patterns)

(defcustom arxana-browser-wheel-step 1
  "Number of entries to move per wheel or trackpad gesture."
  :type 'integer
  :group 'arxana-patterns)

(defun arxana-browser--click-path ()
  (let ((path arxana-browser-click-sound))
    (when (and path (file-readable-p path))
      path)))

(defvar arxana-browser--buffer "*Arxana Browser*")

(defvar-local arxana-browser--stack nil)
(put 'arxana-browser--stack 'permanent-local t)

(defvar-local arxana-browser--context nil)
(put 'arxana-browser--context 'permanent-local t)

(defvar-local arxana-browser--last-row 1)
(defvar-local arxana-browser--suppress-click nil)

(defun arxana-browser--ensure-context ()
  (unless arxana-browser--stack
    (when arxana-browser--context
      (setq arxana-browser--stack
            (list arxana-browser--context)))))

(defun arxana-browser--root-format ()
  [("Type" 10 t)
   ("Name" 40 t)
   ("Source/Path" 50 t)
   ("Status" 16 t)
   ("Items" 7 nil)])

(defun arxana-browser--pattern-format ()
  [("Order" 8 t)
   ("Pattern" 40 t)
   ("Title" 50 t)])

(defun arxana-browser--menu-format ()
  [("Menu" 20 t)
   ("Description" 70 nil)])

(defun arxana-browser--info-format ()
  [("Name" 25 t)
   ("Details" 65 nil)])

(defface arxana-browser--dirty-header
  '((t :foreground "orange"))
  "Face used for dirty header indicators."
  :group 'arxana)

(defun arxana-browser--root-row (item)
  (let* ((type (capitalize (symbol-name (or (plist-get item :type) 'unknown))))
         (name (or (plist-get item :title)
                   (plist-get item :label)
                   ""))
         (source (or (plist-get item :source) "-"))
         (status (or (plist-get item :status) "-"))
         (count (number-to-string (or (plist-get item :count) 0))))
    (vector type name source status count)))

(defun arxana-browser--item-get (item key)
  "Return KEY from ITEM, handling both plists and alists."
  (cond
   ((null item) nil)
   ((and (listp item) (keywordp (car item)))
    (plist-get item key))
   ((listp item)
    (alist-get key item nil nil #'eq))
   (t nil)))

(defun arxana-browser--menu-row (item)
  (vector (or (plist-get item :label) "")
          (or (plist-get item :description) "")))

(defun arxana-browser--info-row (item)
  (vector (or (plist-get item :label) "")
          (or (plist-get item :description) "")))

(defun arxana-browser--menu-items ()
  (list (list :type 'menu
              :label "Patterns"
              :description "Browse pattern languages and flexiarg collections."
              :view 'patterns)
        (list :type 'menu
              :label "Pattern Activation"
              :description "Patterns activated by recent agent turns (futon3a context-retrieval evidence)."
              :view 'pattern-activation)
        (list :type 'menu
              :label "Code"
              :description "Browse code with docbook backlinks."
              :view 'code-root)
        (list :type 'menu
              :label "Graph"
              :description "Explore Futon graph types."
              :view 'graph)
        (list :type 'menu
              :label "Hypergraphs"
              :description "Inspect local hypergraph JSON datasets."
              :view 'hypergraph)
        (list :type 'menu
              :label "Media"
              :description "Zoom/Napster media library prototype."
              :view 'media)
        (list :type 'menu
              :label "Songs"
              :description "XTDB-backed songs and lyrics catalogs."
              :view 'songs-home)
        (list :type 'menu
              :label "Essays"
              :description "Annotated editions of essays linking sentences to flexiarg patterns."
              :view 'essays-home)
        (list :type 'menu
              :label "Docs"
              :description "Doc books (XTDB-backed)."
              :view 'docbook)
        (list :type 'menu
              :label "Evidence"
              :description "Evidence landscape views (timeline, sessions, threads)."
              :view 'evidence-home)
        (list :type 'menu
              :label "Sessions"
              :description "Evidence-backed semantic summaries for open REPL buffers."
              :view 'evidence-open-sessions)
        (list :type 'menu
              :label "Missions"
              :description "Mission Control portfolio (98 missions across 7 repos)."
              :view 'missions-portfolio)
        (list :type 'menu
              :label "Encyclopedia"
              :description "PlanetMath and other math reference content."
              :view 'encyclopedia)
        (list :type 'menu
              :label "Trace"
              :description "Self-representing stack: trace tensions through gates to source."
              :view 'trace-home)
        (list :type 'menu
              :label "VSATARCS"
              :description "Read VSAT-shaped anthology stories as Arxana-native scenes."
              :view 'vsatarcs)
        (list :type 'menu
              :label "Scans"
              :description "Daily-scan frames (M-daily-scan). RET opens frame EDN."
              :view 'scans)
        (list :type 'menu
              :label "Invariants"
              :description "Live invariant families first, with violations and candidates as secondary views."
              :view 'invariants-home)
        (list :type 'menu
              :label "Ledger"
              :description "Billable-hours ledger (Hyperreal) — Historical / Current / Future."
              :view 'ledger)
        (list :type 'menu
              :label "Sales"
              :description "Sales pipeline (Hyperreal) — clients by stage + Rolodex. Dual of the Ledger."
              :view 'sales)))

(defun arxana-browser--evidence-menu-items ()
  (if (require 'arxana-browser-evidence nil t)
      (arxana-browser-evidence-menu-items)
    (list (list :type 'info
                :label "Evidence module unavailable"
                :description "Load arxana-browser-evidence.el for evidence views."))))

(defun arxana-browser--ensure-evidence ()
  "Load evidence browser support when available."
  (or (featurep 'arxana-browser-evidence)
      (require 'arxana-browser-evidence nil t)))

(defun arxana-browser--code-root-items ()
  (list (list :type 'code-root
              :label "Futon4 code"
              :description "Browse futon4 code with docbook backlinks."
              :docbook "futon4")
        (list :type 'code-root
              :label "Futon3 code"
              :description "Browse futon3 code with docbook backlinks."
              :docbook "futon3")))

(defun arxana-browser--code-items ()
  (if (require 'arxana-browser-code nil t)
      (arxana-browser-code-items)
    (list (list :type 'info
                :label "Arxana source"
                :description "Hook up Futon1 code entities to browse modules here.")
          (list :type 'info
                :label "Import status"
                :description "No code catalogs detected yet."))))

(defun arxana-browser--graph-items ()
  (if (require 'arxana-browser-graph nil t)
      (arxana-browser-graph-items)
    (list (list :type 'info
                :label "Graph unavailable"
                :description "Load arxana-browser-graph.el for /types."))))

(defun arxana-browser--hypergraph-items ()
  (if (require 'arxana-browser-hypergraph nil t)
      (arxana-browser-hypergraph-items)
    (list (list :type 'info
                :label "Hypergraph viewer unavailable"
                :description "Load arxana-browser-hypergraph.el for local JSON datasets."))))

(defun arxana-browser--songs-items ()
  (let ((song-items (and (require 'arxana-browser-songs nil t)
                         (arxana-browser-songs-menu-items)))
        (chorus-items (and (require 'arxana-browser-chorus nil t)
                           (arxana-browser-chorus-menu-items))))
    (cond
     ((or song-items chorus-items)
      (append song-items chorus-items))
     (t
      (list (list :type 'info
                  :label "Songs browser unavailable"
                  :description "Load song/chorus browser modules for XTDB-backed songs and choruses."))))))

(defun arxana-browser--chorus-items ()
  (if (require 'arxana-browser-chorus nil t)
      (arxana-browser-chorus-menu-items)
    (list (list :type 'info
                :label "Chorus browser unavailable"
                :description "Load arxana-browser-chorus.el for XTDB-backed choruses."))))

(defun arxana-browser-code-select-docbook ()
  "Select the docbook used for code docs in the browser."
  (interactive)
  (if (require 'arxana-browser-code nil t)
      (call-interactively #'arxana-browser-code-set-docbook)
    (user-error "arxana-browser-code is unavailable")))

(defun arxana-browser--bounce-or-select-docbook ()
  "Use `B` as media bounce in media views, else code docbook selection."
  (interactive)
  (let* ((context (car arxana-browser--stack))
         (media-context (or (plist-get context :media-filter)
                            (memq (plist-get context :view)
                                  '(media media-projects media-publications media-publication
                                          media-ep-staging media-ep-staging-ep
                                          media-misc media-misc-folder media-podcasts))
                            (eq (plist-get context :type) 'media-category))))
    (if media-context
        (arxana-media-bounce-marked)
      (arxana-browser-code-select-docbook))))

(defun arxana-browser--header-line (context total)
  (let* ((store-status (when (and (require 'arxana-store nil t)
                                  (fboundp 'arxana-store-remote-status))
                         (arxana-store-remote-status)))
         (store-suffix
          (pcase store-status
            (:down " [Futon API down: storage unavailable]")
            (:disabled " [sync disabled: showing filesystem only]")
            (_ ""))))
    (cond
     ((not context)
      (format "Futon4 browser menu (%d entries). RET/right selects, LEFT/b returns."
              total))
     ((eq (plist-get context :view) 'patterns)
      (let ((base (format "Pattern languages & collections (%d entries). RET/right opens, LEFT/b backs up, I imports, E/e edit collections, +/- reorder, A adds a root, g refreshes."
                          total)))
        (concat base store-suffix)))
     ((eq (plist-get context :view) 'code)
      (let ((book (and (boundp 'arxana-browser-code-docbook)
                       arxana-browser-code-docbook)))
        (format "Code browser — docbook: %s (B selects). LEFT/b returns."
                (or book "unknown"))))
     ((eq (plist-get context :view) 'code-root)
      "Code browser — select a Futon. LEFT/b returns.")
     ((eq (plist-get context :view) 'graph)
      (concat "Graph types — browse /types from Futon. RET/right shows details. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'hypergraph)
      "Hypergraph viewer — inspect local JSON datasets. RET/right shows details. LEFT/b returns.")
     ((eq (plist-get context :view) 'media)
      (concat "Media library — pick All tracks, a status, or Projects to drill into recorder projects. N creates a staging EP. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'songs-home)
      (concat "Songs — browse imported lyrics, suite songs, and chorus experiments. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'essays-home)
      (concat "Essays — annotated editions linking essay sentences to flexiarg patterns. RET drills into an essay."
              store-suffix))
     ((eq (plist-get context :view) 'essays-essay)
      (concat "Essay — sections of "
              (or (plist-get context :label) "this essay")
              ". RET drills into a section to see annotations. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'essays-section)
      (concat "Essay section — annotations linking sentences to pattern-library sources. RET opens an annotation. LEFT/b returns."
              store-suffix))
     ((memq (plist-get context :view) '(songs-chapbook songs-suite))
      (concat (format "Songs — %s. RET opens the stored text. LEFT/b returns."
                      (or (plist-get context :label) "catalog"))
              store-suffix))
     ((eq (plist-get context :view) 'choruses-home)
      (concat "Choruses — browse Arxana-native chorus entities assembled from chapbook passages. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'choruses-demo)
      (concat (format "Choruses — %s. RET opens the stored text. LEFT/b returns."
                      (or (plist-get context :label) "catalog"))
              store-suffix))
     ((eq (plist-get context :view) 'media-projects)
      (concat "Media projects — select a recorder project to list its tracks. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'media-publications)
      (concat "Media publications — select an EP folder to browse its exported tracks. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'media-publication)
      (concat "Publication tracks — RET plays, p plays, s stops, B bounces marked tracks. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'media-ep-staging)
      (concat "EP staging — select an EP folder to browse its exported tracks, or press N to create one. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'media-ep-staging-ep)
      (concat "EP staging tracks — RET plays, p plays, s stops, B bounces marked tracks. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'docbook)
      (let ((line (format "Doc books — select a book, then Contents or Recent. %s. LEFT/b returns."
                          (arxana-docbook--source-brief))))
        (arxana-browser--docbook-header line nil)))
     ((eq (plist-get context :view) 'docbook-book)
      (let* ((book (plist-get context :book))
             (line (format "Doc book views — pick Contents or Recent. %s. LEFT/b returns."
                           (arxana-docbook--source-brief book))))
        (arxana-browser--docbook-header line book)))
     ((eq (plist-get context :view) 'docbook-contents)
      (let* ((book (plist-get context :book))
             (dirty (arxana-browser--docbook-contents-dirty-p book))
             (prefix (if dirty "[dirty] " ""))
             (line (format "%sDoc book contents — RET opens heading; C-c C-s syncs order; %s. LEFT/b returns."
                           prefix
                           (arxana-docbook--source-brief book))))
        (arxana-browser--docbook-header line book dirty)))
     ((eq (plist-get context :view) 'docbook-section)
      (let* ((book (plist-get context :book))
             (line (format "Doc book section — RET opens entry; %s. LEFT/b returns."
                           (arxana-docbook--source-brief book))))
        (arxana-browser--docbook-header line book)))
     ((eq (plist-get context :view) 'docbook-recent)
      (let* ((book (plist-get context :book))
             (line (format "Doc book recent entries — RET opens entry; %s. LEFT/b returns."
                           (arxana-docbook--source-brief book))))
        (arxana-browser--docbook-header line book)))
     ((eq (plist-get context :view) 'evidence-home)
      (concat "Evidence landscape — choose timeline, session, or thread projections. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'evidence-timeline)
      (let ((filter-desc (if (and (boundp 'arxana-browser--evidence-filter)
                                  arxana-browser--evidence-filter)
                             (format " [filter: %s]"
                                     (mapconcat (lambda (p) (format "%s=%s" (car p) (cdr p)))
                                                arxana-browser--evidence-filter ", "))
                           "")))
        (concat (format "Evidence timeline%s — RET opens entry details; F filters; g refreshes. LEFT/b returns."
                        filter-desc)
                store-suffix)))
     ((eq (plist-get context :view) 'evidence-sessions)
      (let ((usage (if (require 'usage-report nil t)
                       (usage-report-headline-prefix) "")))
        (concat usage
                "Evidence sessions — RET opens session timeline. LEFT/b returns."
                store-suffix)))
     ((eq (plist-get context :view) 'evidence-open-sessions)
      (concat "Open REPL sessions — RET opens live session; C-RET opens evidence timeline; m marks; D deletes; g refreshes. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'evidence-session)
      (concat "Evidence session — chronological entries for this session. RET opens entry details. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'evidence-threads)
      (concat "Evidence threads — grouped by reply-chain root. RET opens thread reader. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'evidence-thread-reader)
      (concat "Evidence thread reader — conversation-style thread view. RET opens entry details. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'evidence-thread)
      (concat "Evidence chain — root-to-leaf reply lineage. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'evidence-entry-detail)
      (concat "Evidence entry details — full metadata for the selected item. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'vsatarcs)
      "VSATARCS — select a VSAT-shaped story to read. LEFT/b returns.")
     ((eq (plist-get context :view) 'scans)
      "Daily-scan frames — RET opens frame EDN. LEFT/b returns.")
     ((eq (plist-get context :view) 'pattern-activation)
      (when (require 'arxana-browser-pattern-activation nil t)
        (concat (arxana-browser-pattern-activation-headline)
                store-suffix)))
     ((eq (plist-get context :view) 'pattern-activation-detail)
      (when (require 'arxana-browser-pattern-activation nil t)
        (concat (arxana-browser-pattern-activation-detail-headline)
                store-suffix)))
     ((memq (plist-get context :view) '(lab-sessions-active lab-sessions-recent))
      (let* ((label (or (plist-get context :label)
                        (if (eq (plist-get context :view) 'lab-sessions-active)
                            "Active Sessions" "Recent Sessions")))
             (usage (if (require 'usage-report nil t)
                        (usage-report-headline-prefix)
                      "")))
        (concat usage
                (format "%s (%d) — RET opens session, LEFT/b returns."
                        label total)
                store-suffix)))
     ((eq (plist-get context :view) 'lab)
      (concat "Lab notebook — RET stub; v trace, r raw, d draft. LEFT/b returns."
              store-suffix))
     ((eq (plist-get context :view) 'lab-files)
      (format "Lab files (%s) — RET opens file. LEFT/b returns."
              (or (plist-get context :label) "lab")))
     ((plist-get context :media-filter)
      (let* ((label (or (plist-get context :label) "Tracks"))
             (count (plist-get context :count)))
        (format "%s — %s. B bounces marked tracks. LEFT/b returns."
                label
                (if (numberp count)
                    (format "%d track%s" count (if (= count 1) "" "s"))
                  (format "%d entries" total)))))
     (t
      (let ((title (or (plist-get context :title) (plist-get context :label))))
        (format "%s — RET/right opens pattern, LEFT/b returns." title))))))

(defun arxana-browser--docbook-header (line &optional book dirty)
  (let* ((book (or book "futon4"))
         (source (and book (arxana-docbook--data-source book)))
         (face (pcase source
                 (:storage 'arxana-docbook-source-green)
                 (:filesystem 'arxana-docbook-source-amber)
                 (:state 'arxana-docbook-source-red)
                 (_ nil))))
    (cond
     (dirty
      (propertize line 'face 'arxana-browser--dirty-header))
     (face (propertize line 'face face))
     (t line))))

(defconst arxana-browser--help-buffer "*Arxana Browser Help*")

(defconst arxana-browser--media-track-help-views
  '(media-publication media-ep-staging-ep media-misc-folder media-podcasts)
  "Views whose rows are directly playable media items.")

(defconst arxana-browser--media-help-views
  '(media media-projects media-publications media-publication
          media-ep-staging media-ep-staging-ep media-misc
          media-misc-folder media-podcasts)
  "Views that belong to the media browser.")

(defconst arxana-browser--help-specs
  '((:section "Navigation"
     :keys ("RET" "<right>")
     :description "Open the entry at point."
     :predicate arxana-browser--help-any-context-p)
    (:section "Navigation"
     :keys ("<left>" "b")
     :description "Return to the parent browser view."
     :predicate arxana-browser--help-any-context-p)
    (:section "Navigation"
     :keys ("g")
     :description "Refresh the current browser view."
     :predicate arxana-browser--help-any-context-p)
    (:section "Navigation"
     :keys ("y" "Y")
     :description "Copy item or current-view Arxana location."
     :predicate arxana-browser--help-any-context-p)
    (:section "Navigation"
     :keys ("f")
     :description "Copy the file path for the current item when available."
     :predicate arxana-browser--help-any-context-p)
    (:section "Navigation"
     :keys ("?")
     :description "Show this bindings help."
     :predicate arxana-browser--help-any-context-p)
    (:section "Navigation"
     :keys ("q")
     :description "Quit the browser window."
     :predicate arxana-browser--help-any-context-p)
    (:section "Media"
     :keys ("m" "U")
     :description "Mark the current item or clear all marks in this view."
     :predicate arxana-browser--help-media-p)
    (:section "Media"
     :keys ("p" "s" "o")
     :description "Play, stop, or toggle autoplay-next."
     :predicate arxana-browser--help-media-track-list-p)
    (:section "Media"
     :keys ("C-c SPC" "C-c <left>" "C-c <right>" "C-c M-<left>" "C-c M-<right>")
     :description "Pause or seek the current player."
     :predicate arxana-browser--help-media-track-list-p)
    (:section "Media"
     :keys ("e")
     :description "Stage the current or marked tracks into an EP."
     :predicate arxana-browser--help-media-track-list-p)
    (:section "Media"
     :keys ("N")
     :description "Create a new empty staging EP and open it."
     :predicate arxana-browser--help-ep-creation-p)
    (:section "Media"
     :keys ("B")
     :description "Bounce the marked tracks in the current media view."
     :predicate arxana-browser--help-media-p)
    (:section "Media"
     :keys ("t")
     :description "Retitle the current track or file."
     :predicate arxana-browser--help-media-track-list-p)
    (:section "Media"
     :keys ("L")
     :description "Edit lyrics for the current track."
     :predicate arxana-browser--help-media-track-list-p)
    (:section "Media"
     :keys ("a")
     :description "Open the current media file in Audacity."
     :predicate arxana-browser--help-media-track-list-p)
    (:section "Media"
     :keys ("S")
     :description "Set status for marked Zoom recorder tracks."
     :predicate arxana-browser--help-zoom-track-list-p)
    (:section "Media"
     :keys ("P")
     :description "Publish marked misc or staged tracks to a publication EP."
     :predicate arxana-browser--help-publishable-media-p)
    (:section "Media"
     :keys ("u" "w")
     :description "Set or open the publication URL for the current EP."
     :predicate arxana-browser--help-publication-p)
    (:section "Media"
     :keys ("R")
     :description "Remove the current or marked tracks from this staging EP."
     :predicate arxana-browser--help-ep-staging-p)
    (:section "Docbook"
     :keys ("O")
     :description "Open the current docbook in its source buffer."
     :predicate arxana-browser--help-docbook-p)
    (:section "Docbook"
     :keys ("C")
     :description "Open surrounding section context for the current docbook item."
     :predicate arxana-browser--help-docbook-p)
    (:section "Docbook"
     :keys ("M-<up>" "M-<down>" "C-M-<up>" "C-M-<down>" "C-M-S-<up>" "C-M-S-<down>")
     :description "Reorder docbook items or sections."
     :predicate arxana-browser--help-docbook-contents-p)
    (:section "Docbook"
     :keys ("C-c C-e" "C-c C-p" "C-c C-s")
     :description "Export Org/PDF or sync docbook ordering."
     :predicate arxana-browser--help-docbook-p)
    (:section "Evidence"
     :keys ("F")
     :description "Filter the evidence timeline."
     :predicate arxana-browser--help-evidence-timeline-p)
    (:section "Lab"
     :keys ("v" "r" "d")
     :description "Open trace, raw, or draft lab buffers for the current item."
     :predicate arxana-browser--help-lab-p)
    (:section "Forum"
     :keys ("C-c C-f")
     :description "Compose a forum reply for the current thread."
     :predicate arxana-browser--help-forum-p))
  "Context-sensitive bindings shown by the Arxana browser help surface.")

(defun arxana-browser--help-context ()
  "Return the current browser context plist, if any."
  (or (and (boundp 'arxana-browser--context)
           (listp arxana-browser--context)
           arxana-browser--context)
      (car-safe (and (boundp 'arxana-browser--stack)
                     arxana-browser--stack))))

(defun arxana-browser--help-any-context-p (_context)
  "Return non-nil for help rows that should always be shown."
  t)

(defun arxana-browser--help-media-p (context)
  "Return non-nil when CONTEXT belongs to the media browser."
  (or (memq (plist-get context :view) arxana-browser--media-help-views)
      (memq (plist-get context :type) '(media-category media-project))))

(defun arxana-browser--help-media-track-list-p (context)
  "Return non-nil when CONTEXT renders playable media rows."
  (or (memq (plist-get context :view) arxana-browser--media-track-help-views)
      (memq (plist-get context :type) '(media-category media-project))))

(defun arxana-browser--help-zoom-track-list-p (context)
  "Return non-nil when CONTEXT is a Zoom track listing."
  (memq (plist-get context :type) '(media-category media-project)))

(defun arxana-browser--help-publication-p (context)
  "Return non-nil when CONTEXT is a publication browser view."
  (eq (plist-get context :view) 'media-publication))

(defun arxana-browser--help-publishable-media-p (context)
  "Return non-nil when CONTEXT can publish marked media items."
  (memq (plist-get context :view)
        '(media-ep-staging-ep media-misc-folder)))

(defun arxana-browser--help-ep-staging-p (context)
  "Return non-nil when CONTEXT is an EP staging track list."
  (eq (plist-get context :view) 'media-ep-staging-ep))

(defun arxana-browser--help-ep-creation-p (context)
  "Return non-nil when CONTEXT can create a new staging EP."
  (memq (plist-get context :view) '(media media-ep-staging)))

(defun arxana-browser--help-docbook-p (context)
  "Return non-nil when CONTEXT is any docbook browser view."
  (memq (plist-get context :view)
        '(docbook docbook-book docbook-contents docbook-section docbook-recent)))

(defun arxana-browser--help-docbook-contents-p (context)
  "Return non-nil when CONTEXT is the editable docbook contents view."
  (eq (plist-get context :view) 'docbook-contents))

(defun arxana-browser--help-evidence-timeline-p (context)
  "Return non-nil when CONTEXT is the evidence timeline view."
  (eq (plist-get context :view) 'evidence-timeline))

(defun arxana-browser--help-lab-p (context)
  "Return non-nil when CONTEXT is a lab browser view."
  (memq (plist-get context :view)
        '(lab-home lab-sessions-active lab-sessions-recent lab-sessions-raw
                   lab-sessions-archived lab tensions devmaps narrative-trail
                   lab-files)))

(defun arxana-browser--help-forum-p (context)
  "Return non-nil when CONTEXT is a forum browser view."
  (eq (plist-get context :view) 'forum))

(defun arxana-browser--help-rows (&optional context)
  "Return help rows relevant to CONTEXT."
  (let ((context (or context (arxana-browser--help-context))))
    (seq-filter
     (lambda (spec)
       (when-let* ((predicate (plist-get spec :predicate)))
         (funcall predicate context)))
     arxana-browser--help-specs)))

(defun arxana-browser--help-sections (&optional context)
  "Return grouped help sections relevant to CONTEXT."
  (let ((rows (arxana-browser--help-rows context))
        sections
        order)
    (dolist (row rows)
      (let* ((section (plist-get row :section))
             (cell (assoc section sections)))
        (unless cell
          (setq cell (cons section nil))
          (setq sections (append sections (list cell)))
          (setq order (append order (list section))))
        (setcdr cell (append (cdr cell) (list row)))))
    (mapcar (lambda (section) (assoc section sections)) order)))

(defun arxana-browser--help-title (&optional context)
  "Return a title string for browser bindings help in CONTEXT."
  (let* ((context (or context (arxana-browser--help-context)))
         (view (plist-get context :view))
         (label (plist-get context :label)))
    (cond
     ((and label view)
      (format "Arxana Browser bindings for %s (%s)" label view))
     (view
      (format "Arxana Browser bindings for %s" view))
     (t
      "Arxana Browser bindings"))))

(defun arxana-browser--insert-help-sections (sections)
  "Insert browser help SECTIONS into the current buffer."
  (dolist (section sections)
    (insert (format "%s\n" (car section)))
    (insert (make-string (length (car section)) ?-))
    (insert "\n")
    (dolist (row (cdr section))
      (insert (format " %-42s %s\n"
                      (string-join (plist-get row :keys) ", ")
                      (plist-get row :description))))
    (insert "\n")))

(defun arxana-browser-show-bindings-help ()
  "Show a contextual bindings cheatsheet for the Arxana browser."
  (interactive)
  (let* ((context (arxana-browser--help-context))
         (title (arxana-browser--help-title context))
         (sections (arxana-browser--help-sections context)))
    (with-help-window arxana-browser--help-buffer
      (with-current-buffer standard-output
        (insert title "\n")
        (insert (make-string (length title) ?=))
        (insert "\n\n")
        (insert "This list is filtered to the current browser view.\n\n")
        (arxana-browser--insert-help-sections sections)))))

(defun arxana-browser--ensure-help-hydra ()
  "Define the browser help Hydra(s) if Hydra is installed.

Return non-nil when the default Hydra body command is available after
this call. Two hydras are defined: the default and a variant for
`media-ep-staging-ep` views that surfaces publish instead of stage."
  (when (or (fboundp 'defhydra)
            (require 'hydra nil t))
    (unless (fboundp 'arxana-browser-help-hydra/body)
      (eval
       '(defhydra arxana-browser-help-hydra (:hint nil :foreign-keys run)
          "
Arxana Browser
  _RET_: open   _<left>_/_b_: up   _g_: refresh   _m_: mark   _U_: unmark
  _p_: play     _s_: stop          _e_: stage     _N_: new EP _B_: bounce
  _?_: full help
  _q_: quit
"
          ("RET" arxana-browser--visit nil)
          ("<right>" arxana-browser--visit nil)
          ("<left>" arxana-browser--up nil)
          ("b" arxana-browser--up nil)
          ("g" arxana-browser--refresh nil)
          ("m" arxana-browser--toggle-mark nil)
          ("U" arxana-media-unmark-all nil)
          ("p" arxana-media-play-at-point nil)
          ("s" arxana-media-stop-playback nil)
          ("e" arxana-browser--stage-to-ep nil)
          ("N" arxana-browser--create-ep-staging nil)
          ("B" arxana-browser--bounce-or-select-docbook nil)
          ("?" arxana-browser-show-bindings-help "full help" :exit t)
          ("q" nil "quit" :exit t))))
    (unless (fboundp 'arxana-browser-help-hydra-ep-staging/body)
      (eval
       '(defhydra arxana-browser-help-hydra-ep-staging (:hint nil :foreign-keys run)
          "
Arxana Browser (EP staging)
  _RET_: open   _<left>_/_b_: up   _g_: refresh   _m_: mark   _U_: unmark
  _p_: play     _s_: stop          _P_: publish   _N_: new EP _B_: bounce
  _?_: full help
  _q_: quit
"
          ("RET" arxana-browser--visit nil)
          ("<right>" arxana-browser--visit nil)
          ("<left>" arxana-browser--up nil)
          ("b" arxana-browser--up nil)
          ("g" arxana-browser--refresh nil)
          ("m" arxana-browser--toggle-mark nil)
          ("U" arxana-media-unmark-all nil)
          ("p" arxana-media-play-at-point nil)
          ("s" arxana-media-stop-playback nil)
          ("P" arxana-media-publish-marked nil :exit t)
          ("N" arxana-browser--create-ep-staging nil)
          ("B" arxana-browser--bounce-or-select-docbook nil)
          ("?" arxana-browser-show-bindings-help "full help" :exit t)
          ("q" nil "quit" :exit t)))))
  (fboundp 'arxana-browser-help-hydra/body))

(defun arxana-browser-help ()
  "Open browser bindings help, preferring Hydra when available.
The Hydra variant adapts to the current view: in `media-ep-staging-ep'
views the hint surfaces publish instead of stage-to-ep, since the
tracks already live in an EP and publish is the next likely action."
  (interactive)
  (if (arxana-browser--ensure-help-hydra)
      (let ((view (and (boundp 'arxana-browser--context)
                       (plist-get arxana-browser--context :view))))
        (if (and (eq view 'media-ep-staging-ep)
                 (fboundp 'arxana-browser-help-hydra-ep-staging/body))
            (arxana-browser-help-hydra-ep-staging/body)
          (arxana-browser-help-hydra/body)))
    (arxana-browser-show-bindings-help)))

(defun arxana-browser--with-help-hint (line)
  "Append the bindings help hint to LINE."
  (if (or (null line) (string-match-p "\\? for bindings\\." line))
      line
    (concat line " ? for bindings.")))

(defconst arxana-browser--description-in-header-views
  '(docbook-contents
    evidence-sessions evidence-open-sessions
    lab-sessions-active lab-sessions-recent
    pattern-activation pattern-activation-detail)
  "Views whose description is prepended to `header-line-format'.
Other views show the description in `mode-line-format'.")

(defun arxana-browser--decorate-header-line (context total)
  "Add status markers to the tabulated header line for CONTEXT."
  (when (and context (eq (plist-get context :view) 'docbook-contents))
    (let* ((book (plist-get context :book))
           (dirty (arxana-browser--docbook-contents-dirty-p book))
           (source (arxana-docbook--data-source book))
           (source-face (pcase source
                          (:storage 'arxana-docbook-source-green)
                          (:filesystem 'arxana-docbook-source-amber)
                          (:state 'arxana-docbook-source-red)
                          (_ nil)))
           (status (arxana-browser--with-help-hint
                    (arxana-browser--header-line context total)))
           (cols (and (listp header-line-format)
                      (car (last header-line-format)))))
      (when cols
        (setq cols (copy-sequence cols))
        (when source-face
          (add-face-text-property 0 (length cols) source-face t cols))
        (when dirty
          (add-face-text-property 0 (length cols)
                                  'arxana-browser--dirty-header
                                  t cols))
        (setq header-line-format (list status " " 'header-line-indent cols))
        (setq mode-line-format '(" ")))))
  (when (and context
             (not (eq (plist-get context :view) 'docbook-contents))
             (memq (plist-get context :view)
                   arxana-browser--description-in-header-views))
    (let* ((status (arxana-browser--with-help-hint
                    (arxana-browser--header-line context total)))
           (cols (and (listp header-line-format)
                      (car (last header-line-format)))))
      (when cols
        (setq header-line-format
              (list status " " 'header-line-indent (copy-sequence cols)))
        (setq mode-line-format '(" ")))))
  (when (or (not context)
            (not (memq (plist-get context :view)
                       arxana-browser--description-in-header-views)))
    (setq mode-line-format
          (list " " (arxana-browser--with-help-hint
                     (arxana-browser--header-line context total)))))
  (when (fboundp 'arxana-ui-mark-managed)
    (setq-local arxana-ui--base-header-line header-line-format)
    (arxana-ui-mark-managed "Arxana Browser")))

(defun arxana-browser--root-items ()
  (arxana-browser--require-patterns)
  (let* ((language-rows (when (arxana-store-sync-enabled-p)
                          (arxana-patterns-ingest-language-rows)))
         (language-index (and language-rows
                              (arxana-browser-patterns--language-index-by-path language-rows)))
         (language-items
          (mapcar (lambda (row)
                    (let* ((import-path (plist-get row :import-path))
                           (source (or import-path
                                       (arxana-browser-patterns--friendly-classification
                                        (plist-get row :source)))))
                      (list :type 'language
                            :label (plist-get row :name)
                            :title (or (plist-get row :title) (plist-get row :name))
                            :entity-id (plist-get row :id)
                            :source source
                            :status (or (arxana-browser-patterns--friendly-classification
                                         (plist-get row :status))
                                        "-")
                            :count (plist-get row :count)
                            :import-path import-path)))
                  (or language-rows '())))
         (collection-items (or (arxana-browser-patterns--filesystem-collection-items language-index) '())))
    (append language-items collection-items)))

(defcustom arxana-browser-frame-name "Arxana"
  "Name of the frame used for Arxana browse windows."
  :type 'string
  :group 'arxana)

(defcustom arxana-browser-frame-fullscreen nil
  "When non-nil, make the Arxana browse frame fullscreen."
  :type 'boolean
  :group 'arxana)

(defun arxana-browser--move-pattern (delta)
  (arxana-browser--ensure-context)
  (arxana-browser--require-patterns)
  (let ((context (car arxana-browser--stack))
        (entry (tabulated-list-get-id)))
    (unless context
      (user-error "Reordering only works after selecting a collection or language"))
    (unless (and entry (eq (plist-get entry :type) 'pattern))
      (user-error "Place point on a pattern entry to reorder"))
    (let* ((label (plist-get entry :label))
           (file (plist-get entry :file))
           (items (arxana-browser-patterns--browser-pattern-items context))
           (index (cl-position-if (lambda (item)
                                    (and (string= (plist-get item :label) label)
                                         (equal (plist-get item :file) file)))
                                  items))
           (new-index (and index (+ index delta))))
      (unless index
        (user-error "Could not locate this pattern in the collection"))
      (when (and new-index (>= new-index 0)
                 (< new-index (length items)))
        (setq items (arxana-browser-patterns--move-entry items index new-index))
        (pcase (plist-get context :type)
          ('collection
           (arxana-browser-patterns--flexiarg-apply-order items))
          ('language
           (arxana-browser-patterns--language-apply-order context items))
          (_
           (user-error "Reordering is not supported for %s entries"
                       (plist-get context :type))))
        (arxana-browser--render)
        (arxana-browser--goto-label label)
        (message "Moved %s to position %d" label (1+ new-index))))))

(defun arxana-browser--move-pattern-up ()
  (interactive)
  (arxana-browser--move-pattern -1))

(defun arxana-browser--move-pattern-down ()
  (interactive)
  (arxana-browser--move-pattern 1))

(defun arxana-browser--current-items ()
  (arxana-browser--ensure-context)
  (let ((context (car arxana-browser--stack)))
    (cond
     ((not context)
      (arxana-browser--menu-items))
     ((plist-get context :media-filter)
      (arxana-media--track-items (plist-get context :media-filter)))
     ((plist-get context :view)
     (pcase (plist-get context :view)
        ('patterns (arxana-browser--root-items))
        ('code-root (arxana-browser--code-root-items))
        ('code (arxana-browser--code-items))
        ('graph (arxana-browser--graph-items))
        ('hypergraph (arxana-browser--hypergraph-items))
        ('media (arxana-media--items))
        ('songs-home (arxana-browser--songs-items))
        ('songs-chapbook (if (require 'arxana-browser-songs nil t)
                             (arxana-browser-songs-items context)
                           (arxana-browser--songs-items)))
        ('songs-suite (if (require 'arxana-browser-songs nil t)
                          (arxana-browser-songs-items context)
                        (arxana-browser--songs-items)))
        ((or 'essays-home 'essays-essay 'essays-section)
         (if (require 'arxana-browser-essays nil t)
             (progn
               (require 'arxana-browser-essays-wikibooks nil t)
               (arxana-browser-essays-items context))
           (list (list :type 'info
                       :label "Essays browser unavailable"
                       :description "Load arxana-browser-essays.el."))))
        ('choruses-home (arxana-browser--chorus-items))
        ('choruses-demo (if (require 'arxana-browser-chorus nil t)
                            (arxana-browser-chorus-items context)
                          (arxana-browser--chorus-items)))
        ('docbook (arxana-browser--docbook-books))
        ('evidence-home (arxana-browser--evidence-menu-items))
        ('evidence-timeline (if (require 'arxana-browser-evidence nil t)
                                (arxana-browser--evidence-timeline-items)
                              (arxana-browser--evidence-menu-items)))
        ('evidence-sessions (if (require 'arxana-browser-evidence nil t)
                                (arxana-browser--evidence-sessions-items)
                              (arxana-browser--evidence-menu-items)))
        ('evidence-open-sessions (if (require 'arxana-browser-evidence nil t)
                                     (arxana-browser--evidence-open-sessions-items)
                                   (arxana-browser--evidence-menu-items)))
        ('evidence-session (if (require 'arxana-browser-evidence nil t)
                               (arxana-browser--evidence-session-items context)
                             (arxana-browser--evidence-menu-items)))
        ('evidence-threads (if (require 'arxana-browser-evidence nil t)
                               (arxana-browser--evidence-threads-items)
                             (arxana-browser--evidence-menu-items)))
        ('evidence-thread (if (require 'arxana-browser-evidence nil t)
                              (arxana-browser--evidence-chain-items context)
                            (arxana-browser--evidence-menu-items)))
        ('evidence-thread-reader (if (require 'arxana-browser-evidence nil t)
                                     (arxana-browser--evidence-thread-reader-items context)
                                   (arxana-browser--evidence-menu-items)))
        ('evidence-entry-detail (if (require 'arxana-browser-evidence nil t)
                                    (arxana-browser--evidence-entry-detail-items context)
                                  (arxana-browser--evidence-menu-items)))
        ;; Mission Control views
        ('missions-portfolio (if (require 'arxana-browser-missions nil t)
                                 (arxana-browser--missions-portfolio-items)
                               (list (list :type 'info :label "Missions module unavailable"))))
        ('missions-by-status (if (require 'arxana-browser-missions nil t)
                                 (arxana-browser--missions-by-status-items)
                               (list (list :type 'info :label "Missions module unavailable"))))
        ('missions-status-group (if (require 'arxana-browser-missions nil t)
                                    (arxana-browser--missions-status-group-items context)
                                  (list (list :type 'info :label "Missions module unavailable"))))
        ('docbook-book (arxana-browser--docbook-book-items (plist-get context :book)))
        ('docbook-contents (arxana-browser--docbook-contents-items (plist-get context :book)))
        ('docbook-section (arxana-browser--docbook-section-items (plist-get context :book) context))
        ('docbook-recent (arxana-browser--docbook-items (plist-get context :book)))
        ('forum (arxana-browser--forum-items))
        ('lab-home (arxana-browser--lab-menu-items))
        ('invariants-home (arxana-browser--invariants-menu-items))
        ('lab-sessions-active (arxana-browser--lab-sessions-active-items))
        ('lab-sessions-recent (arxana-browser--lab-sessions-recent-items))
        ('pattern-activation (progn (require 'arxana-browser-pattern-activation)
                                    (arxana-browser--pattern-activation-items)))
        ('pattern-activation-detail (progn (require 'arxana-browser-pattern-activation)
                                           (arxana-browser--pattern-activation-detail-items)))
        ('lab-sessions-raw (arxana-browser--lab-sessions-raw-items))
        ('lab-sessions-archived (arxana-browser--lab-sessions-archived-items))
        ('evidence-timeline (arxana-browser--evidence-timeline-items))
        ('evidence-sessions (arxana-browser--evidence-sessions-items))
        ('tensions (arxana-browser--tensions-items))
        ('violations (arxana-browser--violations-items))
        ('operational-families (arxana-browser--operational-families-items))
        ('invariant-guide (arxana-browser--invariant-guide-items))
        ('candidate-invariants (arxana-browser--candidate-invariants-items))
        ('devmaps (arxana-browser--devmaps-items))
        ('narrative-trail (arxana-browser--narrative-trail-items
                           (plist-get context :mission-id)))
        ('lab (arxana-browser--lab-items))
        ('encyclopedia (arxana-browser--encyclopedia-items))
        ('encyclopedia-entries (arxana-browser--encyclopedia-entries-items context))
        ('encyclopedia-entry (arxana-browser--encyclopedia-entry-items context))
        ('lab-files (arxana-lab-file-items (plist-get context :kind)))
        ('media-projects (arxana-media--project-items (or (arxana-media--entries) '())))
        ('media-publications (arxana-media--publications-items))
        ('media-publication (arxana-media--publication-track-items (plist-get context :publication-path)))
        ('media-ep-staging (arxana-media--ep-staging-items))
        ('media-ep-staging-ep (arxana-media--publication-track-items (plist-get context :ep-staging-path)))
        ('media-misc (arxana-media--misc-items))
        ('media-misc-folder (arxana-media--misc-track-items (plist-get context :misc-path)))
        ('media-podcasts (arxana-media--podcast-items))
        ('trace-home (if (require 'arxana-browser-trace nil t)
                         (arxana-browser-trace-home-items)
                       (list (list :type 'info :label "Trace module unavailable"
                                   :description "Load arxana-browser-trace.el"))))
        ('trace-devmaps (if (require 'arxana-browser-trace nil t)
                            (arxana-browser-trace-devmaps-items)
                          (list (list :type 'info :label "Trace module unavailable"
                                      :description "Load arxana-browser-trace.el"))))
        ('trace-all-tensions (if (require 'arxana-browser-trace nil t)
                                 (arxana-browser-trace-all-tensions-items)
                               (list (list :type 'info :label "Trace module unavailable"
                                           :description "Load arxana-browser-trace.el"))))
        ('trace-all-components (if (require 'arxana-browser-trace nil t)
                                   (arxana-browser-trace-all-components-items)
                                 (list (list :type 'info :label "Trace module unavailable"
                                             :description "Load arxana-browser-trace.el"))))
        ('trace-tensions (if (require 'arxana-browser-trace nil t)
                             (arxana-browser-trace-tensions-items context)
                           (list (list :type 'info :label "Trace module unavailable"
                                       :description "Load arxana-browser-trace.el"))))
        ('trace-gates (if (require 'arxana-browser-trace nil t)
                          (arxana-browser-trace-gates-items context)
                        (list (list :type 'info :label "Trace module unavailable"
                                    :description "Load arxana-browser-trace.el"))))
        ('vsatarcs (if (require 'arxana-browser-vsatarcs nil t)
                       (arxana-browser-vsatarcs-items)
                     (list (list :type 'info :label "VSATARCS module unavailable"
                                 :description "Load arxana-browser-vsatarcs.el"))))
        ('scans (if (require 'arxana-browser-scans nil t)
                    (arxana-browser-scans-items)
                  (list (list :type 'info :label "Scans module unavailable"
                              :description "Load arxana-browser-scans.el"))))
        ('ledger (if (require 'arxana-vsatarcs-ledger nil t)
                     (arxana-ledger--home-items)
                   (list (list :type 'info :label "Ledger module unavailable"
                               :description "Load arxana-vsatarcs-ledger.el"))))
        ('sales (if (require 'arxana-vsatarcs-sales nil t)
                    (arxana-sales--home-items)
                  (list (list :type 'info :label "Sales module unavailable"
                              :description "Load arxana-vsatarcs-sales.el"))))
        (_ (arxana-browser--menu-items))))
     (t
      (arxana-browser--require-patterns)
      (arxana-browser-patterns--browser-pattern-items context)))))

(defun arxana-browser--row-count ()
  (length (or tabulated-list-entries '())))

(defun arxana-browser--current-row ()
  (if (derived-mode-p 'tabulated-list-mode)
      (let ((row 0)
            (pos (line-beginning-position)))
        (save-excursion
          (goto-char (point-min))
          (while (< (point) pos)
            (when (tabulated-list-get-id)
              (setq row (1+ row)))
            (forward-line 1)))
        row)
    arxana-browser--last-row))

(defun arxana-browser--goto-first-entry ()
  (goto-char (point-min))
  (while (and (not (eobp)) (null (tabulated-list-get-id)))
    (forward-line 1))
  (not (eobp)))

(defun arxana-browser--goto-row (row)
  (let* ((count (arxana-browser--row-count))
         (old arxana-browser--last-row)
         (new-row arxana-browser--last-row))
    (if (<= count 0)
        (progn
          (setq arxana-browser--last-row 0)
          (goto-char (point-min)))
      (setq new-row (max 0 (min row (1- count))))
      (when (arxana-browser--goto-first-entry)
        (let ((steps new-row))
          (while (> steps 0)
            (forward-line 1)
            (while (and (not (eobp)) (null (tabulated-list-get-id)))
              (forward-line 1))
            (setq steps (1- steps)))))
      (setq arxana-browser--last-row new-row)
      (beginning-of-line)
      (when (and (> count 0)
                 (/= old new-row)
                 (not arxana-browser--suppress-click))
        (when (fboundp 'arxana-browser-patterns--play-click)
          (arxana-browser-patterns--play-click))))))

(defun arxana-browser--goto-doc-id (doc-id)
  "Move point to the first browser row matching DOC-ID."
  (when (and doc-id (derived-mode-p 'arxana-browser-mode))
    (let ((row 0)
          (found nil))
      (dolist (entry (or tabulated-list-entries '()))
        (when (and (not found)
                   (equal doc-id (plist-get (car entry) :doc-id)))
          (setq found row))
        (setq row (1+ row)))
      (when found
        (setq arxana-browser--last-row found)
        (arxana-browser--goto-row found)
        t))))

(defun arxana-browser--goto-label (label)
  (goto-char (point-min))
  (forward-line 1)
  (catch 'found
    (while (not (eobp))
      (let ((entry (tabulated-list-get-id)))
        (when (and entry (string= (plist-get entry :label) label))
          (throw 'found t)))
      (forward-line 1))))

(defun arxana-browser--move-selection (delta)
  (let* ((count (arxana-browser--row-count))
         (current arxana-browser--last-row)
         (target (if (> count 0)
                     (max 0 (min (1- count) (+ current delta)))
                   0)))
    (arxana-browser--goto-row target)))

(defun arxana-browser--wheel-steps (_event)
  (max 1 (or arxana-browser-wheel-step 1)))

(defun arxana-browser--wheel-down (event)
  (interactive "e")
  (arxana-browser--move-selection (arxana-browser--wheel-steps event)))

(defun arxana-browser--wheel-up (event)
  (interactive "e")
  (arxana-browser--move-selection (- (arxana-browser--wheel-steps event))))

(defun arxana-browser--next-line (&optional _event)
  (interactive)
  (arxana-browser--move-selection 1))

(defun arxana-browser--previous-line (&optional _event)
  (interactive)
  (arxana-browser--move-selection -1))

(defun arxana-browser--tabulated-entries (context items)
  (let ((row-fn
         (cond
         ((not context) #'arxana-browser--menu-row)
         ((plist-get context :media-filter) #'arxana-media--track-row)
         ((plist-get context :view)
          (pcase (plist-get context :view)
            ('patterns #'arxana-browser--root-row)
            ('code (if (fboundp 'arxana-browser-code-row)
                       #'arxana-browser-code-row
                     #'arxana-browser--info-row))
            ('graph (if (fboundp 'arxana-browser-graph-row)
                        #'arxana-browser-graph-row
                      #'arxana-browser--info-row))
            ('hypergraph (if (fboundp 'arxana-browser-hypergraph-row)
                             #'arxana-browser-hypergraph-row
                           #'arxana-browser--info-row))
            ('media #'arxana-browser--info-row)
            ('songs-home #'arxana-browser--info-row)
            ('songs-chapbook (if (fboundp 'arxana-browser-songs-row)
                                 #'arxana-browser-songs-row
                               #'arxana-browser--info-row))
            ('songs-suite (if (fboundp 'arxana-browser-songs-row)
                              #'arxana-browser-songs-row
                            #'arxana-browser--info-row))
            ((or 'essays-home 'essays-essay 'essays-section)
             (if (fboundp 'arxana-browser-essays-row)
                 #'arxana-browser-essays-row
               #'arxana-browser--info-row))
            ('choruses-home #'arxana-browser--info-row)
            ('choruses-demo (if (fboundp 'arxana-browser-chorus-row)
                                #'arxana-browser-chorus-row
                              #'arxana-browser--info-row))
            ('docbook #'arxana-browser--info-row)
            ('evidence-home #'arxana-browser--info-row)
            ('evidence-timeline (if (fboundp 'arxana-browser--evidence-timeline-row)
                                    #'arxana-browser--evidence-timeline-row
                                  #'arxana-browser--info-row))
            ('evidence-sessions (if (fboundp 'arxana-browser--evidence-sessions-row)
                                    #'arxana-browser--evidence-sessions-row
                                  #'arxana-browser--info-row))
            ('evidence-open-sessions (if (fboundp 'arxana-browser--evidence-open-sessions-row)
                                         #'arxana-browser--evidence-open-sessions-row
                                       #'arxana-browser--info-row))
            ('evidence-session (if (fboundp 'arxana-browser--evidence-session-chat-row)
                                   #'arxana-browser--evidence-session-chat-row
                                 #'arxana-browser--info-row))
            ('missions-portfolio (if (fboundp 'arxana-browser--missions-portfolio-row)
                                     #'arxana-browser--missions-portfolio-row
                                   #'arxana-browser--info-row))
            ('missions-by-status (if (fboundp 'arxana-browser--missions-by-status-row)
                                     #'arxana-browser--missions-by-status-row
                                   #'arxana-browser--info-row))
            ('missions-status-group (if (fboundp 'arxana-browser--missions-portfolio-row)
                                        #'arxana-browser--missions-portfolio-row
                                      #'arxana-browser--info-row))
            ('evidence-threads (if (fboundp 'arxana-browser--evidence-threads-row)
                                   #'arxana-browser--evidence-threads-row
                                 #'arxana-browser--info-row))
            ('evidence-thread (if (fboundp 'arxana-browser--evidence-chain-row)
                                  #'arxana-browser--evidence-chain-row
                                #'arxana-browser--info-row))
            ('evidence-thread-reader (if (fboundp 'arxana-browser--evidence-thread-reader-row)
                                         #'arxana-browser--evidence-thread-reader-row
                                       #'arxana-browser--info-row))
            ('evidence-entry-detail (if (fboundp 'arxana-browser--evidence-entry-detail-row)
                                        #'arxana-browser--evidence-entry-detail-row
                                      #'arxana-browser--info-row))
            ('docbook-book #'arxana-browser--info-row)
            ('docbook-contents #'arxana-browser--docbook-contents-row)
            ('docbook-section #'arxana-browser--docbook-row)
            ('docbook-recent #'arxana-browser--docbook-row)
            ('forum #'arxana-browser--forum-row)
            ('lab-home #'arxana-browser--lab-menu-row)
            ('invariants-home #'arxana-browser--invariants-menu-row)
            ('lab-sessions-active #'arxana-browser--lab-sessions-active-row)
            ('lab-sessions-recent #'arxana-browser--lab-sessions-active-row)
            ('pattern-activation #'arxana-browser--pattern-activation-row)
            ('pattern-activation-detail #'arxana-browser--pattern-activation-detail-row)
            ('lab-sessions-raw #'arxana-browser--lab-sessions-raw-row)
            ('lab-sessions-archived #'arxana-browser--lab-sessions-archived-row)
            ('evidence-timeline #'arxana-browser--evidence-timeline-row)
            ('evidence-sessions #'arxana-browser--evidence-sessions-row)
            ('tensions #'arxana-browser--tensions-row)
            ('violations #'arxana-browser--violations-row)
            ('operational-families #'arxana-browser--operational-families-row)
            ('invariant-guide #'arxana-browser--info-row)
            ('candidate-invariants #'arxana-browser--candidate-invariants-row)
            ('devmaps #'arxana-browser--devmaps-row)
            ('narrative-trail #'arxana-browser--narrative-trail-row)
            ('lab #'arxana-browser--lab-row)
            ('encyclopedia #'arxana-browser--encyclopedia-row)
            ('encyclopedia-entries #'arxana-browser--encyclopedia-entries-row)
            ('encyclopedia-entry #'arxana-browser--encyclopedia-entry-row)
            ('lab-files #'arxana-browser--lab-file-row)
           ('media-projects #'arxana-browser--info-row)
            ('media-publications #'arxana-browser--info-row)
            ('media-publication #'arxana-media--publication-track-row)
            ('media-ep-staging #'arxana-browser--info-row)
            ('media-ep-staging-ep #'arxana-media--publication-track-row)
            ('media-misc #'arxana-browser--info-row)
            ('media-misc-folder #'arxana-media--misc-track-row)
            ('media-podcasts #'arxana-media--podcast-row)
            ('trace-home (if (fboundp 'arxana-browser-trace-home-row)
                             #'arxana-browser-trace-home-row
                           #'arxana-browser--menu-row))
            ('trace-devmaps (if (fboundp 'arxana-browser-trace-devmaps-row)
                                #'arxana-browser-trace-devmaps-row
                              #'arxana-browser--info-row))
            ('trace-all-tensions (if (fboundp 'arxana-browser-trace-tensions-row)
                                     #'arxana-browser-trace-tensions-row
                                   #'arxana-browser--info-row))
            ('trace-all-components (if (fboundp 'arxana-browser-trace-all-components-row)
                                       #'arxana-browser-trace-all-components-row
                                     #'arxana-browser--info-row))
            ('trace-tensions (if (fboundp 'arxana-browser-trace-tensions-row)
                                 #'arxana-browser-trace-tensions-row
                               #'arxana-browser--info-row))
            ('trace-gates (if (fboundp 'arxana-browser-trace-gates-row)
                              #'arxana-browser-trace-gates-row
                            #'arxana-browser--info-row))
            ('vsatarcs (if (fboundp 'arxana-browser-vsatarcs-row)
                           #'arxana-browser-vsatarcs-row
                         #'arxana-browser--info-row))
            ('scans (if (fboundp 'arxana-browser-scans-row)
                        #'arxana-browser-scans-row
                      #'arxana-browser--info-row))
            (_ #'arxana-browser--menu-row)))
        (t (arxana-browser--require-patterns)
           #'arxana-browser-patterns--browser-pattern-row))))
    (mapcar (lambda (entry)
              (list entry (funcall row-fn entry)))
            items)))

(defun arxana-browser--item-at-point ()
  (tabulated-list-get-id))

(defun arxana-browser--render ()
  ;; Allow early exit when docbook sections redirect straight to a single entry.
  (cl-block arxana-browser--render
    (let ((buffer (get-buffer-create arxana-browser--buffer)))
      (with-current-buffer buffer
        (let ((desired-row (if (derived-mode-p 'arxana-browser-mode)
                               (arxana-browser--current-row)
                             arxana-browser--last-row)))
          (arxana-browser--ensure-context)
          (let* ((context (car arxana-browser--stack))
                 (items (arxana-browser--current-items)))
            (when (and context
                       (eq (plist-get context :view) 'docbook-section)
                       (listp items)
                       (= (length items) 1)
                       (eq (plist-get (car items) :type) 'docbook-entry))
              (when (cdr arxana-browser--stack)
                (setq arxana-browser--stack (cdr arxana-browser--stack)))
              (arxana-docbook-open-entry-object (plist-get (car items) :entry))
              (cl-return-from arxana-browser--render))
            (let* ((format
                    (cond
                     ((not context) (arxana-browser--menu-format))
                     ((plist-get context :media-filter)
                      (arxana-media--track-format))
                     ((plist-get context :view)
                      (pcase (plist-get context :view)
                        ('patterns (arxana-browser--root-format))
                        ('code (if (fboundp 'arxana-browser-code-format)
                                   (arxana-browser-code-format)
                                 (arxana-browser--info-format)))
                        ('graph (if (fboundp 'arxana-browser-graph-format)
                                    (arxana-browser-graph-format)
                                  (arxana-browser--info-format)))
                        ('hypergraph (if (fboundp 'arxana-browser-hypergraph-format)
                                         (arxana-browser-hypergraph-format)
                                       (arxana-browser--info-format)))
                        ('media (arxana-browser--info-format))
                        ('songs-home (arxana-browser--info-format))
                        ('songs-chapbook (if (fboundp 'arxana-browser-songs-format)
                                             (arxana-browser-songs-format)
                                           (arxana-browser--info-format)))
                        ('songs-suite (if (fboundp 'arxana-browser-songs-format)
                                          (arxana-browser-songs-format)
                                        (arxana-browser--info-format)))
                        ((or 'essays-home 'essays-essay 'essays-section)
                         (if (fboundp 'arxana-browser-essays-format)
                             (arxana-browser-essays-format context)
                           (arxana-browser--info-format)))
                        ('choruses-home (arxana-browser--info-format))
                        ('choruses-demo (if (fboundp 'arxana-browser-chorus-format)
                                            (arxana-browser-chorus-format)
                                          (arxana-browser--info-format)))
                        ('docbook (arxana-browser--info-format))
                        ('evidence-home (arxana-browser--info-format))
                        ('evidence-timeline (if (fboundp 'arxana-browser--evidence-timeline-format)
                                                (arxana-browser--evidence-timeline-format)
                                              (arxana-browser--info-format)))
                        ('evidence-sessions (if (fboundp 'arxana-browser--evidence-sessions-format)
                                                (arxana-browser--evidence-sessions-format)
                                              (arxana-browser--info-format)))
                        ('evidence-open-sessions (if (fboundp 'arxana-browser--evidence-open-sessions-format)
                                                     (arxana-browser--evidence-open-sessions-format)
                                                   (arxana-browser--info-format)))
                        ('evidence-session (if (fboundp 'arxana-browser--evidence-session-chat-format)
                                               (arxana-browser--evidence-session-chat-format)
                                             (arxana-browser--info-format)))
                        ('missions-portfolio (if (fboundp 'arxana-browser--missions-portfolio-format)
                                                 (arxana-browser--missions-portfolio-format)
                                               (arxana-browser--info-format)))
                        ('missions-by-status (if (fboundp 'arxana-browser--missions-by-status-format)
                                                 (arxana-browser--missions-by-status-format)
                                               (arxana-browser--info-format)))
                        ('missions-status-group (if (fboundp 'arxana-browser--missions-portfolio-format)
                                                    (arxana-browser--missions-portfolio-format)
                                                  (arxana-browser--info-format)))
                        ('evidence-threads (if (fboundp 'arxana-browser--evidence-threads-format)
                                               (arxana-browser--evidence-threads-format)
                                             (arxana-browser--info-format)))
                        ('evidence-thread (if (fboundp 'arxana-browser--evidence-chain-format)
                                              (arxana-browser--evidence-chain-format)
                                            (arxana-browser--info-format)))
                        ('evidence-thread-reader (if (fboundp 'arxana-browser--evidence-thread-reader-format)
                                                     (arxana-browser--evidence-thread-reader-format)
                                                   (arxana-browser--info-format)))
                        ('evidence-entry-detail (if (fboundp 'arxana-browser--evidence-entry-detail-format)
                                                    (arxana-browser--evidence-entry-detail-format)
                                                  (arxana-browser--info-format)))
                        ('docbook-book (arxana-browser--info-format))
                        ('docbook-contents (arxana-browser--docbook-contents-format))
                        ('docbook-section (arxana-browser--docbook-format))
                        ('docbook-recent (arxana-browser--docbook-format))
                        ('forum (arxana-browser--forum-format))
                        ('lab-home (arxana-browser--lab-menu-format))
                        ('invariants-home (arxana-browser--invariants-menu-format))
                        ('lab-sessions-active (arxana-browser--lab-sessions-active-format))
                        ('lab-sessions-recent (arxana-browser--lab-sessions-active-format))
                        ('pattern-activation (arxana-browser--pattern-activation-format))
                        ('pattern-activation-detail (arxana-browser--pattern-activation-detail-format))
                        ('lab-sessions-raw (arxana-browser--lab-sessions-raw-format))
                        ('lab-sessions-archived (arxana-browser--lab-sessions-archived-format))
                        ('evidence-timeline (arxana-browser--evidence-timeline-format))
                        ('evidence-sessions (arxana-browser--evidence-sessions-format))
                        ('tensions (arxana-browser--tensions-format))
                        ('violations (arxana-browser--violations-format))
                        ('operational-families (arxana-browser--operational-families-format))
                        ('invariant-guide (arxana-browser--info-format))
                        ('candidate-invariants (arxana-browser--candidate-invariants-format))
                        ('devmaps (arxana-browser--devmaps-format))
                        ('narrative-trail (arxana-browser--narrative-trail-format))
                        ('lab (arxana-browser--lab-format))
                        ('encyclopedia (arxana-browser--encyclopedia-format))
                        ('encyclopedia-entries (arxana-browser--encyclopedia-entries-format))
                        ('encyclopedia-entry (arxana-browser--encyclopedia-entry-format))
                        ('lab-files (arxana-browser--lab-file-format))
                        ('media-projects (arxana-browser--info-format))
                        ('media-publications (arxana-browser--info-format))
                        ('media-publication (arxana-media--publication-track-format))
                        ('media-ep-staging (arxana-browser--info-format))
                        ('media-ep-staging-ep (arxana-media--publication-track-format))
                        ('media-misc (arxana-browser--info-format))
                        ('media-misc-folder (arxana-media--misc-track-format))
                        ('media-podcasts (arxana-media--podcast-format))
                        ('trace-home (if (fboundp 'arxana-browser-trace-home-format)
                                         (arxana-browser-trace-home-format)
                                       (arxana-browser--menu-format)))
                        ('trace-devmaps (if (fboundp 'arxana-browser-trace-devmaps-format)
                                            (arxana-browser-trace-devmaps-format)
                                          (arxana-browser--info-format)))
                        ('trace-all-tensions (if (fboundp 'arxana-browser-trace-tensions-format)
                                                 (arxana-browser-trace-tensions-format)
                                               (arxana-browser--info-format)))
                        ('trace-all-components (if (fboundp 'arxana-browser-trace-all-components-format)
                                                   (arxana-browser-trace-all-components-format)
                                                 (arxana-browser--info-format)))
                        ('trace-tensions (if (fboundp 'arxana-browser-trace-tensions-format)
                                             (arxana-browser-trace-tensions-format)
                                           (arxana-browser--info-format)))
                        ('trace-gates (if (fboundp 'arxana-browser-trace-gates-format)
                                          (arxana-browser-trace-gates-format)
                                        (arxana-browser--info-format)))
                        ('vsatarcs (if (fboundp 'arxana-browser-vsatarcs-format)
                                       (arxana-browser-vsatarcs-format)
                                     (arxana-browser--info-format)))
                        ('scans (if (fboundp 'arxana-browser-scans-format)
                                    (arxana-browser-scans-format)
                                  (arxana-browser--info-format)))
                        (_ (arxana-browser--menu-format))))
                     ((eq (plist-get context :type) 'language)
                      (arxana-browser--pattern-format))
                     ((eq (plist-get context :type) 'collection)
                      (arxana-browser--pattern-format))
                     (t (arxana-browser--root-format))))
                   (entries (arxana-browser--tabulated-entries context items)))
              (setq arxana-browser--context context)
              (let ((inhibit-read-only t))
                (arxana-browser--ensure-mode-map)
                (arxana-browser-mode)
                (setq tabulated-list-format format
                      tabulated-list-entries entries
                      tabulated-list-use-header-line t)
                (when (and context (eq (plist-get context :view) 'docbook-contents))
                  (setq tabulated-list-sort-key nil))
                (tabulated-list-init-header)
                (arxana-browser--decorate-header-line context (length items))
                (force-mode-line-update)
                (tabulated-list-print t)
                (let* ((count (arxana-browser--row-count))
                       (clamped (if (> count 0)
                                    (max 0 (min desired-row (1- count)))
                                  0)))
                  (let ((arxana-browser--suppress-click t))
                    (arxana-browser--goto-row clamped))))))))
      (display-buffer buffer)
      (when (fboundp 'arxana-ui-refresh)
        (arxana-ui-refresh)))))

(defun arxana-browser--visit ()
  (interactive)
  (let ((item (arxana-browser--item-at-point)))
    (unless item
      (user-error "No entry on this line"))
    (pcase (arxana-browser--item-get item :type)
      ('menu
       (let ((view (arxana-browser--item-get item :view)))
         (if (not view)
             (message "No view associated with this entry")
           (when (and (eq view 'code)
                      (arxana-browser--item-get item :docbook))
             (require 'arxana-browser-code nil t)
             (when (fboundp 'arxana-browser-code-set-docbook)
               (arxana-browser-code-set-docbook (arxana-browser--item-get item :docbook))))
           (setq arxana-browser--stack
                 (cons item arxana-browser--stack))
           (arxana-browser--render))))
      ('language
       (setq arxana-browser--stack
             (cons item arxana-browser--stack))
       (arxana-browser--render))
      ('collection
       (setq arxana-browser--stack
             (cons item arxana-browser--stack))
       (arxana-browser--render))
      ('pattern
       (arxana-browser--require-patterns)
       (arxana-browser-patterns-open (plist-get item :label)))
      ('code-file
       (if (fboundp 'arxana-browser-code-open)
           (arxana-browser-code-open item)
         (let ((path (plist-get item :path)))
           (when path (find-file path)))))
      ('code-root
       (let ((docbook (plist-get item :docbook)))
         (require 'arxana-browser-code nil t)
         (when (and docbook (fboundp 'arxana-browser-code-set-docbook))
           (arxana-browser-code-set-docbook docbook))
         (setq arxana-browser--stack
               (cons (list :view 'code
                           :label (plist-get item :label)
                           :docbook docbook)
                     arxana-browser--stack))
         (arxana-browser--render)))
      ('forum-thread
       (arxana-forum-open-thread item))
      ('lab-menu
       (arxana-browser-lab-open-session item))
      ('invariants-menu
       (let ((view (arxana-browser--item-get item :view))
             (label (arxana-browser--item-get item :label)))
         (setq arxana-browser--stack
               (cons (list :view view :label label)
                     arxana-browser--stack))
         (arxana-browser--render)))
      ('pattern-activation
       ;; Drill into one pattern: push a detail context. The items fn
       ;; and headline both read `(:pattern context)' from the stack
       ;; top, so it survives the major-mode reset inside --render.
       (require 'arxana-browser-pattern-activation)
       (setq arxana-browser--stack
             (cons (list :type 'pattern-activation-detail-context
                         :label (plist-get item :id)
                         :view 'pattern-activation-detail
                         :pattern item)
                   arxana-browser--stack))
       (arxana-browser--render))
      ('pattern-activation-row
       ;; In the detail view, RET opens the originating turn.
       (let ((eid (plist-get item :evidence-id)))
         (cond
          ((and eid (fboundp 'arxana-browser-evidence-open-entry))
           (arxana-browser-evidence-open-entry
            (list :type 'evidence-entry :evidence/id eid)))
          (eid
           (message "Evidence id: %s (no opener available)" eid))
          (t (message "No evidence id on this row")))))
      ('lab-session-active
       (arxana-browser-lab-open-session item))
      ('lab-session-archived
       (arxana-browser-lab-open-session item))
      ('evidence-session
       (if (and (arxana-browser--ensure-evidence)
                (fboundp 'arxana-browser-evidence-open-session))
           (arxana-browser-evidence-open-session item)
         (message "Evidence module unavailable")))
      ('evidence-open-session
       (if (and (arxana-browser--ensure-evidence)
                (fboundp 'arxana-browser-evidence-open-live-session))
           (arxana-browser-evidence-open-live-session item)
         (message "Evidence module unavailable")))
      ('evidence-thread
       (if (and (arxana-browser--ensure-evidence)
                (fboundp 'arxana-browser-evidence-open-thread))
           (arxana-browser-evidence-open-thread item)
         (message "Evidence module unavailable")))
      ('evidence-entry
       (if (and (arxana-browser--ensure-evidence)
                (fboundp 'arxana-browser-evidence-open-entry))
           (arxana-browser-evidence-open-entry item)
         (message "Evidence module unavailable")))
      ('evidence-turn
       (if (and (arxana-browser--ensure-evidence)
                (fboundp 'arxana-browser-evidence-open-entry))
           (arxana-browser-evidence-open-entry item)
         (message "Evidence module unavailable")))
      ('evidence-chat-turn
       (if (and (arxana-browser--ensure-evidence)
                (fboundp 'arxana-browser-evidence-open-entry))
           (arxana-browser-evidence-open-entry item)
         (message "Evidence module unavailable")))
      ('evidence-meta
       (if (and (arxana-browser--ensure-evidence)
                (fboundp 'arxana-browser-evidence-open-entry))
           (arxana-browser-evidence-open-entry item)
         (message "Evidence module unavailable")))
      ('mission-entry
       (if (fboundp 'arxana-browser-missions-open-entry)
           (arxana-browser-missions-open-entry item)
         (message "Missions module unavailable")))
      ('missions-status-group
       (if (fboundp 'arxana-browser-missions-open-status-group)
           (arxana-browser-missions-open-status-group item)
         (message "Missions module unavailable")))
      ('tension-entry
       (if (fboundp 'arxana-browser-tension-open-entry)
           (arxana-browser-tension-open-entry item)
         (message "Lab tension browser unavailable")))
      ('violation-entry
       (if (fboundp 'arxana-browser-violation-open-entry)
           (arxana-browser-violation-open-entry item)
         (message "Lab violations browser unavailable")))
      ('candidate-invariant-entry
       (if (fboundp 'arxana-browser-candidate-invariant-open-entry)
           (arxana-browser-candidate-invariant-open-entry item)
         (message "Invariant candidate browser unavailable")))
      ('operational-family-entry
       (if (fboundp 'arxana-browser-operational-family-open-entry)
           (arxana-browser-operational-family-open-entry item)
         (message "Operational family browser unavailable")))
      ('devmap-entry
       (if (fboundp 'arxana-browser-devmap-open-entry)
           (arxana-browser-devmap-open-entry item)
         (message "Lab devmap browser unavailable")))
      ('trace-devmap
       (if (fboundp 'arxana-browser-trace-visit-devmap)
           (arxana-browser-trace-visit-devmap item)
         (message "Trace module unavailable")))
      ('trace-tension
       (if (fboundp 'arxana-browser-trace-visit-tension)
           (arxana-browser-trace-visit-tension item)
         (message "Trace module unavailable")))
      ('trace-gate
       (if (fboundp 'arxana-browser-trace-visit-gate)
           (arxana-browser-trace-visit-gate item)
         (message "Trace module unavailable")))
      ('encyclopedia-corpus
       (arxana-browser-encyclopedia-open-corpus item))
      ('encyclopedia-entry
       (arxana-browser-encyclopedia-open-entry item))
      ('graph-type
       (if (fboundp 'arxana-browser-graph-open)
           (arxana-browser-graph-open item)
         (message "Graph type: %s" (or (arxana-browser--item-get item :label) "?"))))
      ('hypergraph-source
       (if (fboundp 'arxana-browser-hypergraph-open)
           (arxana-browser-hypergraph-open item)
         (message "Hypergraph source: %s" (or (arxana-browser--item-get item :label) "?"))))
      ('vsatarcs-story
       (if (fboundp 'arxana-browser-vsatarcs-open)
           (arxana-browser-vsatarcs-open item)
         (message "VSATARCS module unavailable")))
      ('scans-frame
       (if (fboundp 'arxana-browser-scans-open)
           (arxana-browser-scans-open item)
         (message "Scans module unavailable")))
      ('ledger-stratum
       (if (fboundp 'arxana-ledger-open-stratum)
           (arxana-ledger-open-stratum item)
         (message "Ledger module unavailable")))
      ((or 'sales-stage 'sales-rolodex 'sales-leads 'sales-demos)
       (if (fboundp 'arxana-sales-open-stage)
           (arxana-sales-open-stage item)
         (message "Sales module unavailable")))
      ('essays-essay
       (setq arxana-browser--stack
             (cons (list :view 'essays-essay
                         :label (plist-get item :label)
                         :essay-id (plist-get item :essay-id))
                   arxana-browser--stack))
       (arxana-browser--render))
      ('essays-section
       (if (fboundp 'arxana-browser-essays-open)
           (arxana-browser-essays-open item)
         (message "Essays browser unavailable")))
      ('essays-annotation
       (if (fboundp 'arxana-browser-essays-open)
           (arxana-browser-essays-open item)
         (message "Essays browser unavailable")))
      ('media-publication
       (let ((path (plist-get item :path)))
         (unless (and path (file-directory-p path))
           (user-error "Publication path missing or not a directory"))
         (setq arxana-browser--stack
               (cons (list :view 'media-publication
                           :label (plist-get item :label)
                           :publication-path path)
                     arxana-browser--stack))
         (arxana-browser--render)))
      ('media-ep-staging
       (let ((path (plist-get item :path)))
         (unless (and path (file-directory-p path))
           (user-error "EP staging path missing or not a directory"))
         (setq arxana-browser--stack
               (cons (list :view 'media-ep-staging-ep
                           :label (plist-get item :label)
                           :ep-staging-path path)
                     arxana-browser--stack))
         (arxana-browser--render)))
      ('songs-catalog
       (setq arxana-browser--stack
             (cons (list :view (plist-get item :view)
                         :label (plist-get item :label)
                         :catalog-id (plist-get item :catalog-id))
                   arxana-browser--stack))
       (arxana-browser--render))
      ('song-entity
       (if (fboundp 'arxana-browser-songs-open)
           (arxana-browser-songs-open item)
         (message "Songs browser unavailable")))
      ('chorus-catalog
       (setq arxana-browser--stack
             (cons (list :view (plist-get item :view)
                         :label (plist-get item :label)
                         :catalog-id (plist-get item :catalog-id))
                   arxana-browser--stack))
       (arxana-browser--render))
      ('chorus-entity
       (if (fboundp 'arxana-browser-chorus-open)
           (arxana-browser-chorus-open item)
         (message "Chorus browser unavailable")))
      ('media-misc-folder
       (let ((path (plist-get item :path)))
         (unless (and path (file-directory-p path))
           (user-error "Folder path missing or not a directory"))
         (setq arxana-browser--stack
               (cons (list :view 'media-misc-folder
                           :label (plist-get item :label)
                           :misc-path path)
                     arxana-browser--stack))
         (arxana-browser--render)))
      ('media-publication-track
       (arxana-media-play-at-point))
      ('media-misc-track
       (arxana-media-play-at-point))
      ('media-podcast
       (arxana-media-play-at-point))
      ((or 'media-category 'media-project)
       (let ((filter (plist-get item :media-filter)))
         (if (not filter)
             (message "No tracks associated with %s" (plist-get item :label))
           (setq arxana-browser--stack
                 (cons item arxana-browser--stack))
           (arxana-browser--render))))
      ('media-projects
       (setq arxana-browser--stack
             (cons (list :view 'media-projects
                         :label (plist-get item :label))
                   arxana-browser--stack))
       (arxana-browser--render))
      ('media-track
      (let* ((entry (plist-get item :entry))
             (title (or (plist-get entry :title)
                        (plist-get entry :base_name)
                        (plist-get entry :sha256))))
        (message "Track: %s (%s)" title (plist-get entry :status))))
      ('docbook-book
       (setq arxana-browser--stack
             (cons (list :view 'docbook-book
                         :label (plist-get item :label)
                         :book (plist-get item :book))
                   arxana-browser--stack))
       (arxana-browser--render))
      ('docbook-contents-root
       (setq arxana-browser--stack
             (cons (list :view 'docbook-contents
                         :label (plist-get item :label)
                         :book (plist-get item :book))
                   arxana-browser--stack))
       (arxana-browser--render))
      ('docbook-heading
       (let* ((book (plist-get item :book))
              (entries (arxana-browser--docbook-section-items book item)))
         (if (and (listp entries)
                  (= (length entries) 1)
                  (eq (plist-get (car entries) :type) 'docbook-entry))
             (arxana-docbook-open-entry-object (plist-get (car entries) :entry))
           (setq arxana-browser--stack
                 (cons (plist-put (copy-sequence item) :view 'docbook-section)
                       arxana-browser--stack))
           (arxana-browser--render))))
      ('docbook-recent
       (setq arxana-browser--stack
             (cons (list :view 'docbook-recent
                         :label (plist-get item :label)
                         :book (plist-get item :book))
                   arxana-browser--stack))
       (arxana-browser--render))
      ('docbook-entry
       (let ((entry (plist-get item :entry)))
         (arxana-docbook-open-entry-object entry)))
      ('lab-entry
       (arxana-lab-open-entry-object item))
      ('lab-file
       (arxana-lab-open-file-entry item))
      ('info
       (message "%s" (or (arxana-browser--item-get item :message)
                         "Nothing to open here yet")))
      (_
       (user-error "Don't know how to open %S entries" (arxana-browser--item-get item :type))))))

(defun arxana-browser--pattern-location (item)
  (let* ((label (plist-get item :label))
         (file (plist-get item :file)))
    (cond
     ((and label (not (string-empty-p label)))
      (format "pattern://%s" label))
     ((and file (file-exists-p file))
      (format "file://%s" (expand-file-name file)))
     (t nil))))

(defun arxana-browser--collection-location (item)
  (let ((label (plist-get item :label)))
    (when (and label (not (string-empty-p label)))
      (format "pattern-library://%s" label))))

(defun arxana-browser--language-location (item)
  (let ((label (plist-get item :label)))
    (when (and label (not (string-empty-p label)))
      (format "pattern-language://%s" label))))

(defun arxana-browser--location-token (value)
  (url-hexify-string (format "%s" value)))

(defun arxana-browser--media-filter-location (filter)
  (pcase filter
    ('all "arxana://media/tracks")
    (`(status . ,status)
     (format "arxana://media/status/%s" (arxana-browser--location-token status)))
    (`(project . ,project)
     (format "arxana://media/project/%s" (arxana-browser--location-token project)))
    (_ nil)))

(defun arxana-browser--media-track-location (item)
  (let* ((type (plist-get item :type))
         (sha (ignore-errors (arxana-media--track-sha item))))
    (cond
     ((and sha (eq type 'media-track))
      (format "arxana://media/zoom/%s" sha))
     ((and sha (eq type 'media-publication-track))
      (format "arxana://media/publication-track/%s" sha))
     ((and sha (eq type 'media-misc-track))
      (format "arxana://media/misc-track/%s" sha))
     (t nil))))

(defun arxana-browser--media-location (item)
  (let ((type (plist-get item :type)))
    (pcase type
      ('media-category
       (arxana-browser--media-filter-location (plist-get item :media-filter)))
      ('media-project
       (format "arxana://media/project/%s"
               (arxana-browser--location-token (plist-get item :label))))
      ('media-projects "arxana://media/projects")
      ('media-publications "arxana://media/publications")
      ('media-publication
       (format "arxana://media/publication/%s"
               (arxana-browser--location-token (plist-get item :label))))
      ('media-ep-staging "arxana://media/ep-staging")
      ('media-ep-staging-ep
       (format "arxana://media/ep-staging/%s"
               (arxana-browser--location-token (plist-get item :label))))
      ('media-podcasts "arxana://media/podcasts")
      ('media-podcast
       (let ((path (plist-get item :path)))
         (if (and path (file-exists-p path))
             (format "file://%s" (expand-file-name path))
           (format "arxana://media/podcast/%s"
                   (arxana-browser--location-token (plist-get item :label))))))
      ('media-misc "arxana://media/misc")
      ('media-misc-folder
       (format "arxana://media/misc/%s"
               (arxana-browser--location-token (plist-get item :label))))
      ((or 'media-track 'media-publication-track 'media-misc-track)
       (or (arxana-browser--media-track-location item)
           (let ((path (plist-get item :path)))
             (when (and path (file-exists-p path))
               (format "file://%s" (expand-file-name path))))))
      (_ nil))))

(defun arxana-browser--menu-location (item)
  (let ((view (plist-get item :view)))
    (when view
      (format "arxana://view/%s" view))))

(defun arxana-browser--forum-location (item)
  (let ((thread-id (or (plist-get item :thread-id)
                       (and (fboundp 'arxana-forum--get)
                            (arxana-forum--get item :thread/id)))))
    (when thread-id
      (format "forum://%s" thread-id))))

(defun arxana-browser--copy-location ()
  "Copy a location identifier for the current browser item."
  (interactive)
  (let* ((item (arxana-browser--item-at-point))
         (type (and item (plist-get item :type)))
         (location (cond
                    ((and item (memq (plist-get item :type) '(docbook-entry docbook-heading)))
                     (arxana-browser--docbook-location item))
                    ((and item (eq (plist-get item :type) 'menu))
                     (arxana-browser--menu-location item))
                    ((and (symbolp type)
                          (string-prefix-p "media-" (symbol-name type)))
                     (arxana-browser--media-location item))
                    ((and item (eq (plist-get item :type) 'code-file))
                     (let ((path (plist-get item :path)))
                       (when path (format "file://%s" (expand-file-name path)))))
                    ((and item (eq (plist-get item :type) 'hypergraph-source))
                     (let ((path (plist-get item :path)))
                       (when path (format "file://%s" (expand-file-name path)))))
                    ((and item (eq (plist-get item :type) 'song-entity))
                     (and (fboundp 'arxana-browser-songs-location)
                          (arxana-browser-songs-location item)))
                    ((and item (eq (plist-get item :type) 'chorus-entity))
                     (and (fboundp 'arxana-browser-chorus-location)
                          (arxana-browser-chorus-location item)))
                    ((and item (memq (plist-get item :type)
                                     '(essays-essay essays-section essays-annotation)))
                     (and (fboundp 'arxana-browser-essays-location)
                          (arxana-browser-essays-location item)))
                    ((and item (eq (plist-get item :type) 'pattern))
                     (arxana-browser--pattern-location item))
                    ((and item (eq (plist-get item :type) 'collection))
                     (arxana-browser--collection-location item))
                    ((and item (eq (plist-get item :type) 'language))
                     (arxana-browser--language-location item))
                    ((and item (eq (plist-get item :type) 'forum-thread))
                     (arxana-browser--forum-location item))
                    ((and item (eq (plist-get item :type) 'operational-family-entry))
                     (and (fboundp 'arxana-browser-operational-family-location)
                          (arxana-browser-operational-family-location item)))
                    (t nil))))
    (unless location
      (user-error "No location available for this item"))
    (kill-new location)
    (message "Copied %s" location)))

(defun arxana-browser--copy-current-location ()
  "Copy a location identifier for the current browser view."
  (interactive)
  (arxana-browser--ensure-context)
  (let* ((context (or arxana-browser--context
                      (car arxana-browser--stack)))
         (context-type (and context (plist-get context :type)))
         (book (or (plist-get context :book)
                   (plist-get (car arxana-browser--stack) :book)
                   (plist-get (cadr arxana-browser--stack) :book)))
         (location (cond
                    ((and context (or (eq (plist-get context :view) 'docbook-section)
                                      (eq (plist-get context :type) 'docbook-entry)))
                     (or (arxana-browser--docbook-location context)
                         (and book (format "docbook://%s" book))))
                    ((and context (memq (plist-get context :view)
                                        '(docbook docbook-book docbook-contents docbook-recent)))
                     (and book (format "docbook://%s" book)))
                    ((and context (plist-get context :media-filter))
                     (arxana-browser--media-filter-location (plist-get context :media-filter)))
                    ((and context (eq (plist-get context :view) 'media-ep-staging-ep)
                          (plist-get context :label))
                     (format "arxana://view/media-ep-staging-%s"
                             (arxana-browser--location-token (plist-get context :label))))
                    ((and context (eq (plist-get context :view) 'media-publication)
                          (plist-get context :label))
                     (format "arxana://media/publication/%s"
                             (arxana-browser--location-token (plist-get context :label))))
                    ((and context (eq (plist-get context :view) 'media-publications))
                     "arxana://media/publications")
                    ((and context (symbolp (plist-get context :view))
                          (string-prefix-p "media-" (symbol-name (plist-get context :view))))
                     (format "arxana://view/%s" (plist-get context :view)))
                    ;; Evidence views: append session-id or evidence-id
                    ((and context (eq (plist-get context :view) 'evidence-session)
                          (plist-get context :session-id))
                     (format "arxana://evidence/session/%s"
                             (plist-get context :session-id)))
                    ((and context (eq (plist-get context :view) 'evidence-thread-reader)
                          (plist-get context :root-id))
                     (format "arxana://evidence/thread/%s"
                             (plist-get context :root-id)))
                    ((and context (eq (plist-get context :view) 'evidence-entry-detail)
                          (plist-get context :evidence-id))
                     (format "arxana://evidence/entry/%s"
                             (plist-get context :evidence-id)))
                    ((and context (eq (plist-get context :view) 'essays-essay)
                          (plist-get context :essay-id))
                     (format "arxana://essay/%s"
                             (url-hexify-string (plist-get context :essay-id))))
                    ((and context (eq (plist-get context :view) 'essays-section)
                          (plist-get context :essay-id)
                          (plist-get context :section-id))
                     (format "arxana://essay/%s/section/%s"
                             (url-hexify-string (plist-get context :essay-id))
                             (url-hexify-string (plist-get context :section-id))))
                    ((and context (plist-get context :view))
                     ;; Try item-at-point for evidence views
                     (let ((item (ignore-errors (arxana-browser--item-at-point))))
                       (or (and item
                                (plist-get item :evidence-id)
                                (not (string-empty-p (plist-get item :evidence-id)))
                                (format "arxana://evidence/entry/%s"
                                        (plist-get item :evidence-id)))
                           (format "arxana://view/%s" (plist-get context :view)))))
                    ((eq context-type 'collection)
                     (arxana-browser--collection-location context))
                    ((eq context-type 'language)
                     (arxana-browser--language-location context))
                    (book (format "docbook://%s" book))
                    ((not context) "arxana://")
                    (t nil))))
    (unless location
      (user-error "No location available for this view"))
    (kill-new location)
    (message "Copied %s" location)))

(defun arxana-browser--item-file-path (item)
  (let ((type (and item (plist-get item :type))))
    (cond
     ((eq type 'media-podcast) (plist-get item :path))
     ((memq type '(media-track media-misc-track media-publication-track))
      (arxana-media--item-play-path item))
     ((eq type 'code-file) (plist-get item :path))
     (t nil))))

(defun arxana-browser--copy-file-path ()
  "Copy a filesystem path for the current browser item."
  (interactive)
  (let* ((item (arxana-browser--item-at-point))
         (path (and item (arxana-browser--item-file-path item))))
    (unless (and path (stringp path) (not (string-empty-p path)))
      (user-error "No file path available for this item"))
    (setq path (expand-file-name path))
    (kill-new path)
    (message "Copied %s" path)))

(defun arxana-browser--up ()
  (interactive)
  (arxana-browser--ensure-context)
  (cond
   ((not arxana-browser--stack)
    (if arxana-browser--context
        (progn
          (setq arxana-browser--context nil)
          (arxana-browser--render))
      (message "Already at top level")))
   (t
    (setq arxana-browser--stack (cdr arxana-browser--stack))
    (when (null arxana-browser--stack)
      (setq arxana-browser--context nil))
    (arxana-browser--render))))

(defun arxana-browser--refresh ()
  (interactive)
  (arxana-browser--render))

(defun arxana-browser-home ()
  "Show the Arxana browser at its root menu on the current frame.
Always resets the navigation stack, and forces the
`*Arxana Browser*' buffer to appear in the selected window of
the current frame — never popping to a different frame even if
the buffer is already visible elsewhere."
  (interactive)
  (let ((buf (get-buffer-create arxana-browser--buffer)))
    (switch-to-buffer buf)
    (setq arxana-browser--stack nil
          arxana-browser--context nil)
    (let ((display-buffer-overriding-action
           '((display-buffer-same-window) (inhibit-switch-frame . t))))
      (arxana-browser--render))))

(defun arxana-browser--import-library ()
  "Ingest the flexiarg collection at point into Futon."
  (interactive)
  (arxana-browser--require-patterns)
  (let ((item (arxana-browser--item-at-point)))
    (unless (and item (eq (plist-get item :type) 'collection))
      (user-error "Place point on a collection entry to import"))
    (unless (arxana-store-sync-enabled-p)
      (user-error "Futon sync is disabled; enable futon4-enable-sync first"))
    (arxana-browser-patterns--import-library item 'prompt)
    (setq arxana-browser--stack nil
          arxana-browser--context nil)
    (arxana-browser--render)))

(defun arxana-browser--edit-collection ()
  "Open an Org buffer for editing every pattern inside the collection at point."
  (interactive)
  (arxana-browser--require-patterns)
  (let ((item (arxana-browser--item-at-point)))
    (unless (and item (eq (plist-get item :type) 'collection))
      (user-error "Place point on a collection entry to edit"))
    (arxana-browser-patterns-edit-collection item)))

(defun arxana-browser--edit-current-context ()
  "Edit the collection represented by the current browser context.
Useful after drilling into a collection so you can start editing without
returning to the top-level list."
  (interactive)
  (arxana-browser--require-patterns)
  (arxana-browser--ensure-context)
  (let ((context (car arxana-browser--stack)))
    (unless (and context (eq (plist-get context :type) 'collection))
      (user-error "Not currently viewing a collection"))
    (arxana-browser-patterns-edit-collection context)))

(defun arxana-browser--add-collection-root ()
  (interactive)
  (arxana-browser--require-patterns)
  (call-interactively #'arxana-browser-patterns-add-collection-root))

(defun arxana-browser--evidence-filter ()
  "Filter evidence entries (only works in evidence timeline view)."
  (interactive)
  (arxana-browser--ensure-context)
  (let ((context (car arxana-browser--stack)))
    (if (and context (eq (plist-get context :view) 'evidence-timeline))
        (call-interactively #'arxana-browser-evidence-filter-by-type)
      (user-error "Filtering is only available in Evidence Timeline view"))))

(defun arxana-browser--visit-alternate ()
  "Perform the alternate visit action for the row at point."
  (interactive)
  (let* ((context (car arxana-browser--stack))
         (item (arxana-browser--item-at-point)))
    (unless item
      (user-error "No entry on this line"))
    (if (and context
             (eq (plist-get context :view) 'evidence-open-sessions)
             (eq (arxana-browser--item-get item :type) 'evidence-open-session))
        (if (and (arxana-browser--ensure-evidence)
                 (fboundp 'arxana-browser-evidence-open-session))
            (arxana-browser-evidence-open-session item)
          (message "Evidence module unavailable"))
      (arxana-browser--visit))))

(defun arxana-browser--make-mode-map ()
  (let ((map (make-sparse-keymap)))
    (when (and (boundp 'tabulated-list-mode-map)
               (keymapp tabulated-list-mode-map))
      (set-keymap-parent map tabulated-list-mode-map))
    (define-key map (kbd "RET") #'arxana-browser--visit)
    (define-key map (kbd "C-<return>") #'arxana-browser--visit-alternate)
    (define-key map (kbd "<right>") #'arxana-browser--visit)
    (define-key map (kbd "<left>") #'arxana-browser--up)
    (define-key map [wheel-left] #'arxana-browser--up)
    (define-key map [wheel-right] #'arxana-browser--visit)
    (define-key map [double-wheel-left] #'arxana-browser--up)
    (define-key map [double-wheel-right] #'arxana-browser--visit)
    (define-key map [triple-wheel-left] #'arxana-browser--up)
    (define-key map [triple-wheel-right] #'arxana-browser--visit)
    (define-key map [mouse-6] #'arxana-browser--up)
    (define-key map [mouse-7] #'arxana-browser--visit)
    (define-key map [wheel-down] #'arxana-browser--wheel-down)
    (define-key map [wheel-up] #'arxana-browser--wheel-up)
    (define-key map [double-wheel-down] #'arxana-browser--wheel-down)
    (define-key map [double-wheel-up] #'arxana-browser--wheel-up)
    (define-key map [triple-wheel-down] #'arxana-browser--wheel-down)
    (define-key map [triple-wheel-up] #'arxana-browser--wheel-up)
    (define-key map [mouse-4] #'arxana-browser--wheel-up)
    (define-key map [mouse-5] #'arxana-browser--wheel-down)
    (define-key map (kbd "b") #'arxana-media-bounce-or-up)
    (define-key map (kbd "g") #'arxana-browser--refresh)
    (define-key map (kbd "I") #'arxana-browser--import-library)
    (define-key map (kbd "E") #'arxana-browser--edit-current-context)
    (define-key map (kbd "e") #'arxana-browser--stage-to-ep)
    (define-key map (kbd "N") #'arxana-browser--create-ep-staging)
    (define-key map (kbd "+") #'arxana-browser--move-pattern-up)
    (define-key map (kbd "-") #'arxana-browser--move-pattern-down)
    (define-key map (kbd "A") #'arxana-browser--add-collection-root)
    (define-key map (kbd "t") #'arxana-media-retitle-at-point)
    (define-key map (kbd "T") #'arxana-media-transcribe-and-podcast-at-point)
    (define-key map (kbd "D") #'arxana-browser--delete-marked)
    (define-key map (kbd "p") #'arxana-media-play-at-point)
    (define-key map (kbd "s") #'arxana-media-stop-playback)
    (define-key map (kbd "o") #'arxana-media-toggle-autoplay-next)
    (define-key map (kbd "L") #'arxana-media-edit-lyrics-at-point)
    (define-key map (kbd "v") #'arxana-browser--lab-open-trace)
    (define-key map (kbd "r") #'arxana-browser--lab-open-raw)
    (define-key map (kbd "d") #'arxana-browser--lab-open-draft)
    (define-key map (kbd "O") #'arxana-browser-docbook-open-book)
    (define-key map (kbd "C") #'arxana-browser-docbook-open-section-context)
    (define-key map (kbd "M-<up>") #'arxana-browser-docbook-move-item-up)
    (define-key map (kbd "M-<down>") #'arxana-browser-docbook-move-item-down)
    (define-key map (kbd "C-M-<up>") #'arxana-browser-docbook-move-section-up)
    (define-key map (kbd "C-M-<down>") #'arxana-browser-docbook-move-section-down)
    (define-key map (kbd "C-M-S-<up>") #'arxana-browser-docbook-move-top)
    (define-key map (kbd "C-M-S-<down>") #'arxana-browser-docbook-move-bottom)
    (define-key map (kbd "m") #'arxana-browser--toggle-mark)
    (define-key map (kbd "R") #'arxana-browser--remove-marked)
    (define-key map (kbd "X") #'arxana-browser--hard-delete-marked)
    (define-key map (kbd "a") #'arxana-media-open-in-audacity)
    (define-key map (kbd "C-c SPC") #'arxana-media-playback-pause-toggle)
    (define-key map (kbd "C-c <left>") #'arxana-media-playback-seek-back-10)
    (define-key map (kbd "C-c <right>") #'arxana-media-playback-seek-forward-10)
    (define-key map (kbd "C-c M-<left>") #'arxana-media-playback-seek-back-30)
    (define-key map (kbd "C-c M-<right>") #'arxana-media-playback-seek-forward-30)
    (define-key map (kbd "U") #'arxana-media-unmark-all)
    (define-key map (kbd "S") #'arxana-media-set-status-marked)
    (define-key map (kbd "P") #'arxana-media-publish-marked)
    (define-key map (kbd "u") #'arxana-media-set-publication-url)
    (define-key map (kbd "w") #'arxana-media-open-publication-url)
    (define-key map (kbd "y") #'arxana-browser--copy-location)
    (define-key map (kbd "Y") #'arxana-browser--copy-current-location)
    (define-key map (kbd "f") #'arxana-browser--copy-file-path)
    (define-key map (kbd "?") #'arxana-browser-help)
    (define-key map (kbd "C-c C-e") #'arxana-browser-docbook-export-org)
    (define-key map (kbd "C-c C-p") #'arxana-browser-docbook-export-pdf)
    (define-key map (kbd "C-c C-s") #'arxana-browser-docbook-sync-order)
    (define-key map (kbd "C-c C-f") #'arxana-forum-compose-for-current-thread)
    (define-key map (kbd "B") #'arxana-browser--bounce-or-select-docbook)
    (define-key map (kbd "F") #'arxana-browser--evidence-filter)
    (define-key map (kbd "C-c C-l") #'arxana-media-lyrics-refresh-at-point)
    (define-key map (kbd "C-c C-r") #'arxana-media-lyrics-refresh-buffer)
    (define-key map (kbd "C-c C-L") #'arxana-media-lyrics-adopt-entity-at-point)
    (define-key map (kbd "<left>") #'arxana-browser--up)
    (define-key map (kbd "<home>") #'arxana-browser-home)
    (define-key map (kbd "<next>") #'arxana-browser-home)
    (define-key map (kbd "H") #'arxana-browser-home)
    (define-key map (kbd "q") #'quit-window)
    map))

(defvar arxana-browser-mode-map
  (arxana-browser--make-mode-map)
  "Keymap for `arxana-browser-mode'.")

(defun arxana-browser--ensure-mode-map ()
  "Rebuild the mode map if it was unbound by a reload."
  (if (not (boundp 'arxana-browser-mode-map))
      (setq arxana-browser-mode-map (arxana-browser--make-mode-map))
    (let ((binding (lookup-key arxana-browser-mode-map (kbd "f")))
          (fbinding (lookup-key arxana-browser-mode-map (kbd "F"))))
      (unless (and (eq binding #'arxana-browser--copy-file-path)
                   (eq fbinding #'arxana-browser--evidence-filter))
        (setq arxana-browser-mode-map (arxana-browser--make-mode-map)))))
  (when (and (boundp 'arxana-browser-mode-map)
             (null (lookup-key arxana-browser-mode-map (kbd "B"))))
    (define-key arxana-browser-mode-map (kbd "B") #'arxana-browser--bounce-or-select-docbook))
  (when (boundp 'arxana-browser-mode-map)
    (let ((binding (lookup-key arxana-browser-mode-map (kbd "y"))))
      (unless (eq binding #'arxana-browser--copy-location)
        (define-key arxana-browser-mode-map (kbd "y") #'arxana-browser--copy-location)))))

(define-derived-mode arxana-browser-mode tabulated-list-mode "Arxana-Browse"
  "Mode for browsing Futon pattern libraries."
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (unless (and (boundp 'arxana-media--marked)
               (hash-table-p arxana-media--marked))
    (setq-local arxana-media--marked (make-hash-table :test 'equal)))
  (setq-local hl-line-face 'arxana-browser--highlight)
  (hl-line-mode 1)
  (when (fboundp 'arxana-ui-mark-managed)
    (arxana-ui-mark-managed "Arxana Browser")))

(defun arxana-browser--stage-to-ep ()
  (interactive)
  (arxana-media-move-misc-to-ep-at-point))

(defun arxana-browser--create-ep-staging ()
  (interactive)
  (arxana-browser--ensure-context)
  (let ((view (plist-get (car arxana-browser--stack) :view)))
    (unless (memq view '(media media-ep-staging))
      (user-error "New EP staging is only available from Media or EP staging views")))
  (arxana-media-create-ep-staging))

(defun arxana-browser--toggle-mark ()
  (interactive)
  (arxana-browser--ensure-context)
  (let ((context (car arxana-browser--stack)))
    (if (and context (eq (plist-get context :view) 'docbook-contents))
        (arxana-browser-docbook-toggle-mark-at-point)
      (if (and context (eq (plist-get context :view) 'evidence-open-sessions))
          (arxana-evidence-toggle-open-session-mark-at-point)
        (arxana-media-toggle-mark-at-point)))))

(defun arxana-browser--remove-marked ()
  (interactive)
  (arxana-browser--ensure-context)
  (let ((context (car arxana-browser--stack)))
    (cond
     ((and context (eq (plist-get context :view) 'docbook-contents))
      (arxana-browser-docbook-remove-marked))
     ((and context (eq (plist-get context :view) 'media-ep-staging-ep))
      (arxana-media-remove-from-ep-staging-at-point))
     (t
      (user-error "Remove is only supported in docbook contents and EP staging views")))))

(defun arxana-browser--hard-delete-marked ()
  (interactive)
  (arxana-browser--ensure-context)
  (let ((context (car arxana-browser--stack)))
    (if (and context (eq (plist-get context :view) 'docbook-contents))
        (arxana-browser-docbook-hard-delete-marked)
      (user-error "Hard delete is only supported in docbook contents views"))))

(defun arxana-browser--delete-marked ()
  (interactive)
  (arxana-browser--ensure-context)
  (let ((context (car arxana-browser--stack)))
    (cond
     ((and context (eq (plist-get context :view) 'docbook-contents))
      (user-error "Use R to remove or X to hard delete in docbook contents"))
     ((and context (eq (plist-get context :view) 'evidence-open-sessions))
      (arxana-evidence-delete-open-sessions))
     (t
      (arxana-media-delete-at-point)))))

(provide 'arxana-browser-core)
;;; arxana-browser-core.el ends here
