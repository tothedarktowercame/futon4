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

(declare-function arxana-browser--lab-items "arxana-browser-lab")
(declare-function arxana-browser--lab-row "arxana-browser-lab" (item))
(declare-function arxana-browser--lab-file-row "arxana-browser-lab" (item))
(declare-function arxana-browser--lab-format "arxana-browser-lab")
(declare-function arxana-browser--lab-file-format "arxana-browser-lab")
(declare-function arxana-browser--lab-open-trace "arxana-browser-lab")
(declare-function arxana-browser--lab-open-raw "arxana-browser-lab")
(declare-function arxana-browser--lab-open-draft "arxana-browser-lab")

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
(declare-function arxana-browser--lab-sessions-recent-items "arxana-browser-lab")
(declare-function arxana-browser--lab-sessions-archived-items "arxana-browser-lab")
(declare-function arxana-browser--lab-sessions-archived-row "arxana-browser-lab" (item))
(declare-function arxana-browser--lab-sessions-archived-format "arxana-browser-lab")
(declare-function arxana-browser-lab-open-session "arxana-browser-lab" (item))

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
(declare-function arxana-media-stage-to-ep "arxana-media")
(declare-function arxana-media-delete-at-point "arxana-media")
(declare-function arxana-media-move-misc-to-ep-at-point "arxana-media")
(declare-function arxana-media-toggle-mark-at-point "arxana-media")
(declare-function arxana-media-transcribe-and-podcast-at-point "arxana-media")

(declare-function arxana-browser-marks-items-in-context "arxana-browser-marks" (marks key-fn &optional filter-fn))

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
              :label "Code"
              :description "Browse code with docbook backlinks."
              :view 'code-root)
        (list :type 'menu
              :label "Graph"
              :description "Explore Futon graph types."
              :view 'graph)
        (list :type 'menu
              :label "Media"
              :description "Zoom/Napster media library prototype."
              :view 'media)
        (list :type 'menu
              :label "Docs"
              :description "Doc books (XTDB-backed)."
              :view 'docbook)
        (list :type 'menu
              :label "Forum"
              :description "Live forum threads (WebSocket updates)."
              :view 'forum)
        (list :type 'menu
              :label "Lab"
              :description "Lab sessions (active and archived)."
              :view 'lab-home)
        (list :type 'menu
              :label "Encyclopedia"
              :description "PlanetMath and other math reference content."
              :view 'encyclopedia)))

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

(defun arxana-browser-code-select-docbook ()
  "Select the docbook used for code docs in the browser."
  (interactive)
  (if (require 'arxana-browser-code nil t)
      (call-interactively #'arxana-browser-code-set-docbook)
    (user-error "arxana-browser-code is unavailable")))

(defun arxana-browser--header-line (context total)
  (cond
   ((not context)
    (format "Futon4 browser menu (%d entries). RET/right selects, LEFT/b returns."
            total))
   ((eq (plist-get context :view) 'patterns)
    (let ((base (format "Pattern languages & collections (%d entries). RET/right opens, LEFT/b backs up, I imports, E/e edit collections, +/- reorder, A adds a root, g refreshes."
                        total)))
      (if (arxana-store-sync-enabled-p)
          base
        (concat base " [sync disabled: showing filesystem only]"))))
   ((eq (plist-get context :view) 'code)
    (let ((book (and (boundp 'arxana-browser-code-docbook)
                     arxana-browser-code-docbook)))
      (format "Code browser — docbook: %s (B selects). LEFT/b returns."
              (or book "unknown"))))
   ((eq (plist-get context :view) 'code-root)
    "Code browser — select a Futon. LEFT/b returns.")
   ((eq (plist-get context :view) 'graph)
    "Graph types — browse /types from Futon. RET/right shows details. LEFT/b returns.")
   ((eq (plist-get context :view) 'media)
    "Media library — pick All tracks, a status, or Projects to drill into recorder projects. LEFT/b returns.")
   ((eq (plist-get context :view) 'media-projects)
    "Media projects — select a recorder project to list its tracks. LEFT/b returns.")
   ((eq (plist-get context :view) 'media-publications)
    "Media publications — select an EP folder to browse its exported tracks. LEFT/b returns.")
   ((eq (plist-get context :view) 'media-publication)
    "Publication tracks — RET plays, p plays, s stops. LEFT/b returns.")
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
   ((eq (plist-get context :view) 'lab)
    "Lab notebook — RET stub; v trace, r raw, d draft. LEFT/b returns.")
   ((eq (plist-get context :view) 'lab-files)
    (format "Lab files (%s) — RET opens file. LEFT/b returns."
            (or (plist-get context :label) "lab")))
   ((plist-get context :media-filter)
    (let* ((label (or (plist-get context :label) "Tracks"))
           (count (plist-get context :count)))
      (format "%s — %s. LEFT/b returns."
              label
              (if (numberp count)
                  (format "%d track%s" count (if (= count 1) "" "s"))
                (format "%d entries" total)))))
   (t
    (let ((title (or (plist-get context :title) (plist-get context :label))))
      (format "%s — RET/right opens pattern, LEFT/b returns." title)))))

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
           (status (arxana-browser--header-line context total))
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
  (when (or (not context)
            (not (eq (plist-get context :view) 'docbook-contents)))
    (setq mode-line-format (list " " (arxana-browser--header-line context total))))
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
        ('media (arxana-media--items))
        ('docbook (arxana-browser--docbook-books))
        ('docbook-book (arxana-browser--docbook-book-items (plist-get context :book)))
        ('docbook-contents (arxana-browser--docbook-contents-items (plist-get context :book)))
        ('docbook-section (arxana-browser--docbook-section-items (plist-get context :book) context))
        ('docbook-recent (arxana-browser--docbook-items (plist-get context :book)))
        ('forum (arxana-browser--forum-items))
        ('lab-home (arxana-browser--lab-menu-items))
        ('lab-sessions-active (arxana-browser--lab-sessions-active-items))
        ('lab-sessions-recent (arxana-browser--lab-sessions-recent-items))
        ('lab-sessions-archived (arxana-browser--lab-sessions-archived-items))
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
            ('media #'arxana-browser--info-row)
            ('docbook #'arxana-browser--info-row)
            ('docbook-book #'arxana-browser--info-row)
            ('docbook-contents #'arxana-browser--docbook-contents-row)
            ('docbook-section #'arxana-browser--docbook-row)
            ('docbook-recent #'arxana-browser--docbook-row)
            ('forum #'arxana-browser--forum-row)
            ('lab-home #'arxana-browser--lab-menu-row)
            ('lab-sessions-active #'arxana-browser--lab-sessions-active-row)
            ('lab-sessions-recent #'arxana-browser--lab-sessions-active-row)
            ('lab-sessions-archived #'arxana-browser--lab-sessions-archived-row)
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
                        ('media (arxana-browser--info-format))
                        ('docbook (arxana-browser--info-format))
                        ('docbook-book (arxana-browser--info-format))
                        ('docbook-contents (arxana-browser--docbook-contents-format))
                        ('docbook-section (arxana-browser--docbook-format))
                        ('docbook-recent (arxana-browser--docbook-format))
                        ('forum (arxana-browser--forum-format))
                        ('lab-home (arxana-browser--lab-menu-format))
                        ('lab-sessions-active (arxana-browser--lab-sessions-active-format))
                        ('lab-sessions-recent (arxana-browser--lab-sessions-active-format))
                        ('lab-sessions-archived (arxana-browser--lab-sessions-archived-format))
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
    (pcase (plist-get item :type)
      ('menu
       (let ((view (plist-get item :view)))
         (if (not view)
             (message "No view associated with this entry")
           (when (and (eq view 'code)
                      (plist-get item :docbook))
             (require 'arxana-browser-code nil t)
             (when (fboundp 'arxana-browser-code-set-docbook)
               (arxana-browser-code-set-docbook (plist-get item :docbook))))
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
      ('lab-session-active
       (arxana-browser-lab-open-session item))
      ('lab-session-archived
       (arxana-browser-lab-open-session item))
      ('encyclopedia-corpus
       (arxana-browser-encyclopedia-open-corpus item))
      ('encyclopedia-entry
       (arxana-browser-encyclopedia-open-entry item))
      ('graph-type
       (if (fboundp 'arxana-browser-graph-open)
           (arxana-browser-graph-open item)
         (message "Graph type: %s" (or (plist-get item :label) "?"))))
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
       (message "%s" (or (plist-get item :message)
                         "Nothing to open here yet")))
      (_
       (user-error "Don't know how to open %S entries" (plist-get item :type))))))

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
                    ((and item (eq (plist-get item :type) 'pattern))
                     (arxana-browser--pattern-location item))
                    ((and item (eq (plist-get item :type) 'collection))
                     (arxana-browser--collection-location item))
                    ((and item (eq (plist-get item :type) 'language))
                     (arxana-browser--language-location item))
                    ((and item (eq (plist-get item :type) 'forum-thread))
                     (arxana-browser--forum-location item))
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
                    ((and context (symbolp (plist-get context :view))
                          (string-prefix-p "media-" (symbol-name (plist-get context :view))))
                     (format "arxana://view/%s" (plist-get context :view)))
                    ((and context (plist-get context :view))
                     (format "arxana://view/%s" (plist-get context :view)))
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

(defun arxana-browser--make-mode-map ()
  (let ((map (make-sparse-keymap)))
    (when (and (boundp 'tabulated-list-mode-map)
               (keymapp tabulated-list-mode-map))
      (set-keymap-parent map tabulated-list-mode-map))
    (define-key map (kbd "RET") #'arxana-browser--visit)
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
    (define-key map (kbd "C-c C-e") #'arxana-browser-docbook-export-org)
    (define-key map (kbd "C-c C-p") #'arxana-browser-docbook-export-pdf)
    (define-key map (kbd "C-c C-s") #'arxana-browser-docbook-sync-order)
    (define-key map (kbd "C-c C-f") #'arxana-forum-compose-for-current-thread)
    (define-key map (kbd "B") #'arxana-browser-code-select-docbook)
    (define-key map (kbd "C-c C-l") #'arxana-media-lyrics-refresh-at-point)
    (define-key map (kbd "C-c C-L") #'arxana-media-lyrics-adopt-entity-at-point)
    (define-key map (kbd "<left>") #'arxana-browser--up)
    (define-key map (kbd "q") #'quit-window)
    map))

(defvar arxana-browser-mode-map
  (arxana-browser--make-mode-map)
  "Keymap for `arxana-browser-mode'.")

(defun arxana-browser--ensure-mode-map ()
  "Rebuild the mode map if it was unbound by a reload."
  (if (not (boundp 'arxana-browser-mode-map))
      (setq arxana-browser-mode-map (arxana-browser--make-mode-map))
    (let ((binding (lookup-key arxana-browser-mode-map (kbd "f"))))
      (unless (eq binding #'arxana-browser--copy-file-path)
        (setq arxana-browser-mode-map (arxana-browser--make-mode-map)))))
  (when (and (boundp 'arxana-browser-mode-map)
             (null (lookup-key arxana-browser-mode-map (kbd "B"))))
    (define-key arxana-browser-mode-map (kbd "B") #'arxana-browser-code-select-docbook))
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

(defun arxana-browser--toggle-mark ()
  (interactive)
  (arxana-browser--ensure-context)
  (let ((context (car arxana-browser--stack)))
    (if (and context (eq (plist-get context :view) 'docbook-contents))
        (arxana-browser-docbook-toggle-mark-at-point)
      (arxana-media-toggle-mark-at-point))))

(defun arxana-browser--remove-marked ()
  (interactive)
  (arxana-browser--ensure-context)
  (let ((context (car arxana-browser--stack)))
    (if (and context (eq (plist-get context :view) 'docbook-contents))
        (arxana-browser-docbook-remove-marked)
      (user-error "Remove is only supported in docbook contents views"))))

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
    (if (and context (eq (plist-get context :view) 'docbook-contents))
        (user-error "Use R to remove or X to hard delete in docbook contents")
      (arxana-media-delete-at-point))))

(provide 'arxana-browser-core)
;;; arxana-browser-core.el ends here
