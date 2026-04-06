;;; arxana-browser-chorus.el --- Chorus browser for Arxana -*- lexical-binding: t; -*-

;;; Commentary:
;; XTDB-backed browser helpers for chorus entities.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'view)

(require 'arxana-store)
(require 'arxana-ui)
(require 'arxana-browser-songs)

(declare-function arxana-store-fetch-entities-latest "arxana-store" (&rest args))
(declare-function arxana-store-fetch-entity "arxana-store" (id &optional version as-of))
(declare-function arxana-store-fetch-hyperedge "arxana-store" (id))
(declare-function arxana-store-ensure-entity "arxana-store" (&rest args))
(declare-function arxana-store-ensure-sync "arxana-store" (&optional prompt))
(declare-function arxana-browser-songs-open "arxana-browser-songs" (item))

(defgroup arxana-browser-chorus nil
  "Chorus browser."
  :group 'arxana)

(defcustom arxana-browser-chorus-latest-limit 100
  "Maximum number of chorus entities to request per catalog."
  :type 'integer
  :group 'arxana-browser-chorus)

(defcustom arxana-browser-chorus-catalogs
  '((:id demo
     :label "Demo choruses"
     :description "Arxana-native chorus experiments assembled from chapbook passages."
     :view choruses-demo
     :entity-types ("arxana/chorus")))
  "Catalog definitions used by the chorus browser."
  :type 'sexp
  :group 'arxana-browser-chorus)

(defcustom arxana-browser-chorus-text-buffer "*Arxana Chorus*"
  "Buffer name used for chorus text."
  :type 'string
  :group 'arxana-browser-chorus)

(defcustom arxana-browser-chorus-notes-buffer "*Arxana Chorus Notes*"
  "Buffer name used for chorus annotation notes."
  :type 'string
  :group 'arxana-browser-chorus)

(defcustom arxana-browser-chorus-notes-side 'right
  "Side for the chorus notes window."
  :type '(choice (const left) (const right))
  :group 'arxana-browser-chorus)

(defcustom arxana-browser-chorus-notes-width 0.45
  "Width for the chorus notes side window."
  :type 'number
  :group 'arxana-browser-chorus)

(defconst arxana-browser-chorus-demo-song-id
  "arxana/song/nightmarish-suite/the-sea/abi"
  "Song id used for the first demo chorus.")

(defconst arxana-browser-chorus-demo-id
  "arxana/chorus/demo/abi"
  "Entity id for the first demo chorus.")

(defface arxana-browser-chorus-annotation-face
  '((t :background "#eef6ff"))
  "Face for annotated chorus passages."
  :group 'arxana-browser-chorus)

(defface arxana-browser-chorus-active-face
  '((t :background "#dbeafe"))
  "Face for the active chorus passage."
  :group 'arxana-browser-chorus)

(defface arxana-browser-chorus-note-link-face
  '((t :inherit link))
  "Face for actionable lines in the chorus notes pane."
  :group 'arxana-browser-chorus)

(defface arxana-browser-chorus-note-song-face
  '((t :inherit bold))
  "Face for retrieved song passage text in the chorus notes pane."
  :group 'arxana-browser-chorus)

(defconst arxana-browser-chorus-base-overlay-priority 10
  "Priority for passive chorus annotation overlays.")

(defconst arxana-browser-chorus-active-overlay-priority 20
  "Priority for active chorus annotation overlays.")

(defun arxana-browser-chorus--catalog-spec (id-or-view)
  (seq-find
   (lambda (spec)
     (or (eq (plist-get spec :id) id-or-view)
         (eq (plist-get spec :view) id-or-view)))
   arxana-browser-chorus-catalogs))

(defun arxana-browser-chorus--entity-name (entity)
  (or (alist-get :entity/name entity)
      (alist-get :name entity)
      (alist-get :entity/id entity)
      (alist-get :id entity)
      "<untitled chorus>"))

(defun arxana-browser-chorus--entity-id (entity)
  (or (alist-get :entity/id entity)
      (alist-get :xt/id entity)
      (alist-get :entity/external-id entity)
      (alist-get :id entity)))

(defun arxana-browser-chorus--entity-type (entity)
  (or (alist-get :entity/type entity)
      (alist-get :type entity)
      "arxana/chorus"))

(defun arxana-browser-chorus--entity-source (entity)
  (or (alist-get :entity/source entity)
      (alist-get :source entity)
      ""))

(defun arxana-browser-chorus--entity-lines (entity)
  (length (split-string (arxana-browser-chorus--entity-source entity) "\n" t)))

(defun arxana-browser-chorus--entity-preview (entity)
  (let* ((source (string-trim (arxana-browser-chorus--entity-source entity)))
         (line (car (split-string source "\n" t))))
    (if (string-empty-p (or line ""))
        "(empty chorus)"
      (truncate-string-to-width line 78 nil nil t))))

(defun arxana-browser-chorus--unwrap-entities (response)
  (let ((items (and (listp response)
                    (or (ignore-errors (alist-get :entities response))
                        response))))
    (cond
     ((null items) nil)
     ((vectorp items) (append items nil))
     ((listp items) items)
     (t nil))))

(defun arxana-browser-chorus--catalog-entities (spec)
  (let* ((types (plist-get spec :entity-types))
         (entities
          (apply #'append
                 (mapcar
                  (lambda (entity-type)
                    (let ((response (arxana-store-fetch-entities-latest
                                     :type entity-type
                                     :limit arxana-browser-chorus-latest-limit)))
                      (arxana-browser-chorus--unwrap-entities response)))
                  types))))
    (sort (copy-sequence entities)
          (lambda (left right)
            (string-lessp (downcase (arxana-browser-chorus--entity-name left))
                          (downcase (arxana-browser-chorus--entity-name right)))))))

(defun arxana-browser-chorus-menu-items ()
  (mapcar
   (lambda (spec)
     (let ((count (if (arxana-store-sync-enabled-p)
                      (length (or (arxana-browser-chorus--catalog-entities spec) '()))
                    0)))
       (list :type 'chorus-catalog
             :label (plist-get spec :label)
             :description (format "%d entries. %s"
                                  count
                                  (plist-get spec :description))
             :view (plist-get spec :view)
             :catalog-id (plist-get spec :id)
             :count count)))
   arxana-browser-chorus-catalogs))

(defun arxana-browser-chorus-items (context)
  (let* ((spec (arxana-browser-chorus--catalog-spec (plist-get context :view)))
         (entities (and spec (arxana-browser-chorus--catalog-entities spec))))
    (cond
     ((not (arxana-store-sync-enabled-p))
      (list (list :type 'info
                  :label "Chorus browser unavailable"
                  :description "Enable Futon sync to browse XTDB-backed choruses.")))
     ((not spec)
      (list (list :type 'info
                  :label "Unknown chorus catalog"
                  :description "No chorus catalog is associated with this view.")))
     ((null entities)
      (list (list :type 'info
                  :label "No entries yet"
                  :description (format "No %s are stored in XTDB yet."
                                       (downcase (plist-get spec :label))))))
     (t
      (mapcar
       (lambda (entity)
         (list :type 'chorus-entity
               :label (arxana-browser-chorus--entity-name entity)
               :description (arxana-browser-chorus--entity-preview entity)
               :entity-id (arxana-browser-chorus--entity-id entity)
               :entity-type (arxana-browser-chorus--entity-type entity)
               :line-count (arxana-browser-chorus--entity-lines entity)
               :catalog-id (plist-get spec :id)
               :entity entity))
       entities)))))

(defun arxana-browser-chorus-format ()
  [("Title" 36 t)
   ("Type" 18 t)
   ("Lines" 7 nil)
   ("Entity" 42 t)])

(defun arxana-browser-chorus-row (item)
  (vector (or (plist-get item :label) "")
          (format "%s" (or (plist-get item :entity-type) ""))
          (number-to-string (or (plist-get item :line-count) 0))
          (or (plist-get item :entity-id) "")))

(defvar-local arxana-browser-chorus--passage-index nil)
(defvar-local arxana-browser-chorus--source-index nil)
(defvar-local arxana-browser-chorus--note-index nil)
(defvar-local arxana-browser-chorus--active-overlay nil)
(defvar-local arxana-browser-chorus--note-highlight-overlay nil)
(defvar-local arxana-browser-chorus--entry-overlays nil)
(defvar-local arxana-browser-chorus--peer-buffer nil)
(defvar-local arxana-browser-chorus--last-active-hyperedge nil)
(defvar-local arxana-browser-chorus--current-entity nil)

(defun arxana-browser-chorus-left-or-return ()
  "Move left or return from Chorus buffers when at point-min."
  (interactive)
  (if (> (point) (point-min))
      (backward-char)
    (arxana-ui-left-or-return)))

(defvar arxana-browser-chorus-text-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'arxana-browser-chorus-text-activate)
    (define-key map (kbd "RET") #'arxana-browser-chorus-text-activate)
    map)
  "Keymap for clickable chorus annotation markers.")

(defvar arxana-browser-chorus-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map view-mode-map)
    (define-key map (kbd "<left>") #'arxana-browser-chorus-left-or-return)
    (define-key map (kbd "RET") #'arxana-browser-chorus-text-activate)
    map)
  "Keymap for `arxana-browser-chorus-mode'.")

(define-minor-mode arxana-browser-chorus-mode
  "Minor mode for chorus display buffers."
  :lighter " Chorus"
  :keymap arxana-browser-chorus-mode-map
  :group 'arxana-browser-chorus
  (if arxana-browser-chorus-mode
      (add-hook 'post-command-hook #'arxana-browser-chorus--sync-note-from-point nil t)
    (remove-hook 'post-command-hook #'arxana-browser-chorus--sync-note-from-point t)))

(defvar arxana-browser-chorus-notes-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'arxana-browser-chorus-notes-activate)
    (define-key map (kbd "RET") #'arxana-browser-chorus-notes-activate)
    map)
  "Keymap for chorus note links.")

(defvar arxana-browser-chorus-notes-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map view-mode-map)
    (define-key map (kbd "<left>") #'arxana-browser-chorus-left-or-return)
    (define-key map (kbd "RET") #'arxana-browser-chorus-notes-activate)
    map)
  "Keymap for `arxana-browser-chorus-notes-mode'.")

(define-minor-mode arxana-browser-chorus-notes-mode
  "Minor mode for chorus notes."
  :lighter " Chorus-Notes"
  :keymap arxana-browser-chorus-notes-mode-map
  :group 'arxana-browser-chorus
  (if arxana-browser-chorus-notes-mode
      (add-hook 'post-command-hook #'arxana-browser-chorus--sync-source-from-point nil t)
    (remove-hook 'post-command-hook #'arxana-browser-chorus--sync-source-from-point t)))

(defun arxana-browser-chorus--value-name (value)
  (cond
   ((keywordp value) (substring (symbol-name value) 1))
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   ((null value) nil)
   (t (format "%s" value))))

(defun arxana-browser-chorus--alist-value (alist &rest keys)
  (seq-some (lambda (key)
              (and (listp alist)
                   (ignore-errors (alist-get key alist))))
            keys))

(defun arxana-browser-chorus--entity-props (entity)
  (or (alist-get :props entity)
      (alist-get :entity/props entity)
      '()))

(defun arxana-browser-chorus--entity-prop (entity key)
  (alist-get key (arxana-browser-chorus--entity-props entity)))

(defun arxana-browser-chorus--fallback-demo-spec (entity-id)
  (seq-find (lambda (spec)
              (equal entity-id (plist-get spec :id)))
            (arxana-browser-chorus-demo-entity-specs)))

(defun arxana-browser-chorus--annotation-ids (entity)
  (let* ((ids (arxana-browser-chorus--entity-prop entity 'chorus/annotation-ids))
         (fallback (plist-get (arxana-browser-chorus--fallback-demo-spec
                               (arxana-browser-chorus--entity-id entity))
                              :props)))
    (or ids
        (alist-get 'chorus/annotation-ids fallback)
        '())))

(defun arxana-browser-chorus--song-ids (entity)
  (let* ((props (arxana-browser-chorus--entity-props entity))
         (fallback (plist-get (arxana-browser-chorus--fallback-demo-spec
                               (arxana-browser-chorus--entity-id entity))
                              :props))
         (many (or (alist-get 'chorus/source-songs props)
                   (alist-get 'chorus/source-songs fallback)))
         (one (or (alist-get 'chorus/source-song props)
                  (alist-get 'chorus/source-song fallback))))
    (cond
     ((listp many) many)
     (one (list one))
     (t '()))))

(defun arxana-browser-chorus--unwrap-hyperedge-response (response)
  (cond
   ((null response) nil)
   ((alist-get :hyperedge response) (alist-get :hyperedge response))
   ((alist-get :hx/id response) response)
   (t nil)))

(defun arxana-browser-chorus--annotation-props (hyperedge)
  (or (alist-get :hx/props hyperedge)
      (alist-get :props hyperedge)
      '()))

(defun arxana-browser-chorus--endpoint-role (endpoint)
  (arxana-browser-chorus--value-name
   (arxana-browser-chorus--alist-value endpoint :role :hx/role)))

(defun arxana-browser-chorus--endpoint-entity-id (endpoint)
  (arxana-browser-chorus--alist-value endpoint :entity-id :id))

(defun arxana-browser-chorus--endpoint-passage (endpoint)
  (arxana-browser-chorus--alist-value endpoint :passage :hx/passage))

(defun arxana-browser-chorus--extract-song-text (entity passage)
  (let* ((source (arxana-browser-chorus--entity-source entity))
         (lines (split-string source "\n" nil))
         (bounds (arxana-browser-chorus--demo-line-bounds passage)))
    (cond
     ((and bounds
           (<= 1 (car bounds))
           (<= (cdr bounds) (length lines)))
      (string-join
       (cl-loop for idx from (1- (car bounds)) to (1- (cdr bounds))
                collect (nth idx lines))
       "\n"))
     (t
      (or (and (stringp passage)
               (string-match "\\`lines? [0-9]+\\(?:-[0-9]+\\)?:\\s-*\\(.+\\)\\'" passage)
               (match-string 1 passage))
          passage
          "")))))

(defun arxana-browser-chorus--resolve-annotation-entry (hyperedge-id allowed-song-ids entity-cache)
  (let* ((response (arxana-store-fetch-hyperedge hyperedge-id))
         (hyperedge (arxana-browser-chorus--unwrap-hyperedge-response response))
         (kind (arxana-browser-chorus--value-name
                (arxana-browser-chorus--alist-value hyperedge :hx/type :type)))
         (ends (or (alist-get :hx/ends hyperedge) '()))
         (annotated (seq-find (lambda (ep)
                                (equal (arxana-browser-chorus--endpoint-role ep) "annotated"))
                              ends))
         (source (seq-find (lambda (ep)
                             (equal (arxana-browser-chorus--endpoint-role ep) "source"))
                           ends))
         (song-id (and annotated (arxana-browser-chorus--endpoint-entity-id annotated)))
         (source-passage (and source (arxana-browser-chorus--endpoint-passage source))))
    (when (and (equal kind "annotation/supports")
               song-id
               source-passage
               (or (null allowed-song-ids)
                   (member song-id allowed-song-ids)))
      (let* ((song-entity
              (or (gethash song-id entity-cache)
                  (let* ((song-response (arxana-store-fetch-entity song-id))
                         (entity (and (listp song-response) (alist-get :entity song-response))))
                    (when entity
                      (puthash song-id entity entity-cache))
                    entity)))
             (annotated-passage (arxana-browser-chorus--endpoint-passage annotated))
             (props (arxana-browser-chorus--annotation-props hyperedge)))
        (list :hyperedge-id hyperedge-id
              :chorus-passage source-passage
              :song-id song-id
	              :song-name (or (and song-entity (arxana-browser-chorus--entity-name song-entity))
	                             song-id)
	              :song-passage annotated-passage
	              :song-text (if song-entity
	                             (arxana-browser-chorus--extract-song-text song-entity annotated-passage)
	                           annotated-passage)
	              :note (or (alist-get :note props)
	                        (alist-get 'note props)
	                        (alist-get :gloss props)
	                        (alist-get 'gloss props)
	                        ""))))))

(defun arxana-browser-chorus--chorus-line-order (entity passage)
  (let ((lines (split-string (arxana-browser-chorus--entity-source entity) "\n" nil))
        (needle (string-trim (or passage "")))
        (idx 1)
        found)
    (while (and lines (not found))
      (when (string= needle (string-trim (car lines)))
        (setq found idx))
      (setq idx (1+ idx)
            lines (cdr lines)))
    (or found most-positive-fixnum)))

(defun arxana-browser-chorus--annotation-entries (entity)
  (let ((ids (arxana-browser-chorus--annotation-ids entity))
        (allowed-song-ids (arxana-browser-chorus--song-ids entity))
        (entity-cache (make-hash-table :test 'equal))
        entries)
    (dolist (hyperedge-id ids)
      (when-let ((entry (arxana-browser-chorus--resolve-annotation-entry
                         hyperedge-id allowed-song-ids entity-cache)))
        (push entry entries)))
    (sort entries
          (lambda (left right)
            (let ((left-order (arxana-browser-chorus--chorus-line-order
                               entity (plist-get left :chorus-passage)))
                  (right-order (arxana-browser-chorus--chorus-line-order
                                entity (plist-get right :chorus-passage))))
              (or (< left-order right-order)
                  (and (= left-order right-order)
                       (string-lessp (plist-get left :song-name)
                                     (plist-get right :song-name)))))))))

(defun arxana-browser-chorus--text-header-line ()
  "Header line for chorus text buffers."
  "Chorus  RET/mouse-1 follow annotation  point syncs notes")

(defun arxana-browser-chorus--notes-header-line ()
  "Header line for chorus notes buffers."
  "Chorus Notes  RET/mouse-1 open song context  point syncs source")

(defun arxana-browser-chorus--highlight-source-bounds (bounds)
  (when (and bounds (consp bounds))
    (unless (overlayp arxana-browser-chorus--active-overlay)
      (setq arxana-browser-chorus--active-overlay
            (make-overlay (car bounds) (cdr bounds)))
      (overlay-put arxana-browser-chorus--active-overlay
                   'face 'arxana-browser-chorus-active-face)
      (overlay-put arxana-browser-chorus--active-overlay
                   'priority arxana-browser-chorus-active-overlay-priority))
    (move-overlay arxana-browser-chorus--active-overlay
                  (car bounds) (cdr bounds))))

(defun arxana-browser-chorus--highlight-note-bounds (bounds)
  (when (and bounds (consp bounds))
    (unless (overlayp arxana-browser-chorus--note-highlight-overlay)
      (setq arxana-browser-chorus--note-highlight-overlay
            (make-overlay (car bounds) (cdr bounds)))
      (overlay-put arxana-browser-chorus--note-highlight-overlay
                   'face 'arxana-browser-chorus-active-face)
      (overlay-put arxana-browser-chorus--note-highlight-overlay
                   'priority arxana-browser-chorus-active-overlay-priority))
    (move-overlay arxana-browser-chorus--note-highlight-overlay
                  (car bounds) (cdr bounds))))

(defun arxana-browser-chorus--overlay-hyperedge-id-at (pos)
  (when pos
    (let* ((candidates (seq-filter
                        (lambda (ov) (overlay-get ov 'arxana-chorus-hyperedge-id))
                        (overlays-at pos)))
           (best (car (sort candidates
                            (lambda (a b)
                              (< (- (overlay-end a) (overlay-start a))
                                 (- (overlay-end b) (overlay-start b))))))))
      (and best (overlay-get best 'arxana-chorus-hyperedge-id)))))

(defun arxana-browser-chorus--hyperedge-id-at-point ()
  (or (get-text-property (point) 'arxana-chorus-hyperedge-id)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'arxana-chorus-hyperedge-id))
      (arxana-browser-chorus--overlay-hyperedge-id-at (point))
      (and (> (point) (point-min))
           (arxana-browser-chorus--overlay-hyperedge-id-at (1- (point))))))

(defun arxana-browser-chorus--jump-to-note (hyperedge-id)
  (let ((buf (get-buffer arxana-browser-chorus-notes-buffer)))
    (when (buffer-live-p buf)
      (let ((win (display-buffer-in-side-window
                  buf
                  (list (cons 'side arxana-browser-chorus-notes-side)
                        (cons 'window-width arxana-browser-chorus-notes-width)))))
        (with-current-buffer buf
          (when-let* ((bounds (and (hash-table-p arxana-browser-chorus--note-index)
                                   (gethash hyperedge-id arxana-browser-chorus--note-index))))
            (arxana-browser-chorus--highlight-note-bounds bounds)
            (goto-char (car bounds))
            (when (window-live-p win)
              (with-selected-window win
                (goto-char (car bounds))
                (recenter)))))))))

(defun arxana-browser-chorus--sync-note-from-point ()
  (let ((hyperedge-id (arxana-browser-chorus--hyperedge-id-at-point)))
    (unless (equal hyperedge-id arxana-browser-chorus--last-active-hyperedge)
      (setq arxana-browser-chorus--last-active-hyperedge hyperedge-id)
      (if (and hyperedge-id (hash-table-p arxana-browser-chorus--source-index))
          (when-let ((bounds (gethash hyperedge-id arxana-browser-chorus--source-index)))
            (arxana-browser-chorus--highlight-source-bounds bounds))
        (when (overlayp arxana-browser-chorus--active-overlay)
          (delete-overlay arxana-browser-chorus--active-overlay)
          (setq arxana-browser-chorus--active-overlay nil)))
      (let ((notes-buf (get-buffer arxana-browser-chorus-notes-buffer)))
        (when (buffer-live-p notes-buf)
          (with-current-buffer notes-buf
            (if (and hyperedge-id (hash-table-p arxana-browser-chorus--note-index))
                (when-let ((bounds (gethash hyperedge-id arxana-browser-chorus--note-index)))
                  (arxana-browser-chorus--highlight-note-bounds bounds)
                  (let ((win (get-buffer-window notes-buf t)))
                    (when (window-live-p win)
                      (with-selected-window win
                        (goto-char (car bounds))
                        (recenter)))))
              (when (overlayp arxana-browser-chorus--note-highlight-overlay)
                (delete-overlay arxana-browser-chorus--note-highlight-overlay)
                (setq arxana-browser-chorus--note-highlight-overlay nil)))))))))

(defun arxana-browser-chorus--sync-source-from-point ()
  (let ((hyperedge-id (arxana-browser-chorus--hyperedge-id-at-point)))
    (unless (equal hyperedge-id arxana-browser-chorus--last-active-hyperedge)
      (setq arxana-browser-chorus--last-active-hyperedge hyperedge-id)
      (let ((text-buf (and (buffer-live-p arxana-browser-chorus--peer-buffer)
                           arxana-browser-chorus--peer-buffer)))
        (when (buffer-live-p text-buf)
          (with-current-buffer text-buf
            (if (and hyperedge-id (hash-table-p arxana-browser-chorus--source-index))
                (when-let ((bounds (gethash hyperedge-id arxana-browser-chorus--source-index)))
                  (arxana-browser-chorus--highlight-source-bounds bounds))
              (when (overlayp arxana-browser-chorus--active-overlay)
                (delete-overlay arxana-browser-chorus--active-overlay)
                (setq arxana-browser-chorus--active-overlay nil)))))))))

(defun arxana-browser-chorus-text-activate ()
  "Jump from an inline chorus marker to its note entry."
  (interactive)
  (let ((hyperedge-id (arxana-browser-chorus--hyperedge-id-at-point)))
    (unless hyperedge-id
      (user-error "No chorus annotation at point"))
    (arxana-browser-chorus--jump-to-note hyperedge-id)))

(defun arxana-browser-chorus-notes-activate ()
  "Open the corresponding song passage from the chorus notes pane."
  (interactive)
  (let ((song-id (or (get-text-property (point) 'arxana-chorus-song-id)
                     (and (> (point) (point-min))
                          (get-text-property (1- (point)) 'arxana-chorus-song-id))))
        (song-passage (or (get-text-property (point) 'arxana-chorus-song-passage)
                          (and (> (point) (point-min))
                               (get-text-property (1- (point)) 'arxana-chorus-song-passage)))))
    (unless song-id
      (user-error "No song context at point"))
    (arxana-browser-songs-open
     (list :entity-id song-id
           :focus-passage song-passage))))

(defun arxana-browser-chorus--render-notes (entity entries)
  (let ((buf (get-buffer-create arxana-browser-chorus-notes-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (remove-overlays (point-min) (point-max))
        (setq-local arxana-browser-chorus--note-index (make-hash-table :test 'equal))
        (setq-local arxana-browser-chorus--note-highlight-overlay nil)
        (setq-local arxana-browser-chorus--last-active-hyperedge nil)
        (setq-local arxana-browser-chorus--current-entity entity)
        (erase-buffer)
        (setq-local header-line-format '(:eval (arxana-browser-chorus--notes-header-line)))
        (insert (format "Notes for %s\n\n" (arxana-browser-chorus--entity-name entity)))
        (if (null entries)
            (insert "(none yet)\n")
          (dolist (entry entries)
            (let ((entry-start (point)))
              (insert (format "%s %s\n"
                              (or (plist-get entry :marker-label) "[*]")
                              (or (plist-get entry :song-name) "(unknown song)")))
              (add-text-properties
               entry-start (point)
               (list 'arxana-chorus-song-id (plist-get entry :song-id)
                     'arxana-chorus-song-passage (plist-get entry :song-passage)
                     'arxana-chorus-hyperedge-id (plist-get entry :hyperedge-id)
                     'face 'arxana-browser-chorus-note-link-face
                     'font-lock-face 'arxana-browser-chorus-note-link-face
                     'mouse-face 'highlight
                     'follow-link t
                     'keymap arxana-browser-chorus-notes-link-map))
              (let ((song-start (point)))
                (insert (format "  %s\n" (or (plist-get entry :song-text) "")))
                (add-text-properties
                 song-start (point)
                 (list 'arxana-chorus-song-id (plist-get entry :song-id)
                       'arxana-chorus-song-passage (plist-get entry :song-passage)
                       'arxana-chorus-hyperedge-id (plist-get entry :hyperedge-id)
                       'face 'arxana-browser-chorus-note-song-face
                       'font-lock-face 'arxana-browser-chorus-note-song-face
                       'mouse-face 'highlight
                       'follow-link t
                       'keymap arxana-browser-chorus-notes-link-map)))
              (when-let ((note (string-trim (or (plist-get entry :note) ""))))
                (unless (string-empty-p note)
                  (insert (format "  - Gloss: %s\n" note))))
              (insert "\n")
              (add-text-properties
               entry-start (point)
               (list 'arxana-chorus-song-id (plist-get entry :song-id)
                     'arxana-chorus-song-passage (plist-get entry :song-passage)
                     'arxana-chorus-hyperedge-id (plist-get entry :hyperedge-id)))
              (puthash (plist-get entry :hyperedge-id)
                       (cons (copy-marker entry-start) (copy-marker (point)))
                       arxana-browser-chorus--note-index))))
        (goto-char (point-min))
        (view-mode 1)
        (arxana-browser-chorus-notes-mode 1)))
    buf))

(defun arxana-browser-chorus--label-entries (entries)
  (let ((counter 0))
    (mapcar
     (lambda (entry)
       (setq counter (1+ counter))
       (plist-put (copy-sequence entry) :marker-label (format "[%d]" counter)))
     entries)))

(defun arxana-browser-chorus--render-entity (entity entries)
  (let ((buf (get-buffer-create arxana-browser-chorus-text-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (remove-overlays (point-min) (point-max))
        (setq-local arxana-browser-chorus--passage-index (make-hash-table :test 'equal))
        (setq-local arxana-browser-chorus--source-index (make-hash-table :test 'equal))
        (setq-local arxana-browser-chorus--entry-overlays (make-hash-table :test 'equal))
        (setq-local arxana-browser-chorus--active-overlay nil)
        (setq-local arxana-browser-chorus--last-active-hyperedge nil)
        (setq-local arxana-browser-chorus--current-entity entity)
        (erase-buffer)
        (setq-local header-line-format '(:eval (arxana-browser-chorus--text-header-line)))
        (insert (format "Title: %s\n" (arxana-browser-chorus--entity-name entity)))
        (insert (format "Entity: %s\n" (or (arxana-browser-chorus--entity-id entity) "?")))
        (insert (format "Type: %s\n" (or (arxana-browser-chorus--entity-type entity) "?")))
        (let* ((source (arxana-browser-chorus--entity-source entity))
               (lines (split-string source "\n" nil)))
          (insert (format "Lines: %d\n\n" (length lines)))
          (cl-loop for line in lines
                   for idx from 1 do
                   (let ((line-start (point)))
                     (insert (format "%3d  %s\n" idx line))
                     (puthash (string-trim line)
                              (cons (+ line-start 5)
                                    (max (+ line-start 5) (1- (point))))
                              arxana-browser-chorus--passage-index))))
        (dolist (entry entries)
          (when-let ((bounds (gethash (string-trim (or (plist-get entry :chorus-passage) ""))
                                      arxana-browser-chorus--passage-index)))
            (puthash (plist-get entry :hyperedge-id) bounds
                     arxana-browser-chorus--source-index)
            (let ((overlay (make-overlay (car bounds) (cdr bounds)))
                  (marker-label (or (plist-get entry :marker-label) "[*]")))
              (overlay-put overlay 'face 'arxana-browser-chorus-annotation-face)
              (overlay-put overlay 'priority arxana-browser-chorus-base-overlay-priority)
              (overlay-put overlay 'arxana-chorus-hyperedge-id
                           (plist-get entry :hyperedge-id))
              (overlay-put overlay 'mouse-face 'highlight)
              (overlay-put overlay 'help-echo "Jump to chorus annotation note")
              (overlay-put overlay 'keymap arxana-browser-chorus-text-link-map)
              (puthash (plist-get entry :hyperedge-id) overlay
                       arxana-browser-chorus--entry-overlays)
              (let ((marker-text
                     (propertize
                      (format " %s" marker-label)
                      'face 'arxana-browser-songs-marker-face
                      'mouse-face 'highlight
                      'keymap arxana-browser-chorus-text-link-map
                      'help-echo "Jump to chorus annotation note"
                      'arxana-chorus-hyperedge-id (plist-get entry :hyperedge-id))))
                (overlay-put (make-overlay (cdr bounds) (cdr bounds))
                             'after-string marker-text)))))
        (goto-char (point-min))
        (view-mode 1)
        (arxana-browser-chorus-mode 1)))
    buf))

(defun arxana-browser-chorus--display-buffer (buf)
  (let* ((main-win (or (and (window-live-p (selected-window))
                            (not (window-parameter (selected-window) 'window-side))
                            (selected-window))
                       (seq-find (lambda (win)
                                   (and (window-live-p win)
                                        (not (window-parameter win 'window-side))))
                                 (window-list nil 'no-mini))
                       (selected-window))))
    (set-window-buffer main-win buf)
    (select-window main-win)))

(defun arxana-browser-chorus-open (item)
  (let* ((entity-id (plist-get item :entity-id))
         (return-buffer (current-buffer))
         (return-config (current-window-configuration))
         (response (and entity-id (arxana-store-fetch-entity entity-id)))
         (entity (and (listp response) (alist-get :entity response))))
    (unless entity
      (user-error "No chorus entity found for %s" (or entity-id (plist-get item :label))))
    (let* ((entries (arxana-browser-chorus--label-entries
                     (arxana-browser-chorus--annotation-entries entity)))
           (buf (arxana-browser-chorus--render-entity entity entries))
           (notes-buf (arxana-browser-chorus--render-notes entity entries)))
      (with-current-buffer buf
        (setq-local arxana-browser-chorus--peer-buffer notes-buf)
        (setq-local arxana-ui-return-buffer return-buffer)
        (setq-local arxana-ui-return-window-config return-config)
        (when (fboundp 'arxana-ui-mark-managed)
          (arxana-ui-mark-managed "Arxana Chorus")))
      (with-current-buffer notes-buf
        (setq-local arxana-browser-chorus--peer-buffer buf)
        (setq-local arxana-ui-return-buffer return-buffer)
        (setq-local arxana-ui-return-window-config return-config))
      (arxana-browser-chorus--display-buffer buf)
      (display-buffer-in-side-window
       notes-buf
       (list (cons 'side arxana-browser-chorus-notes-side)
             (cons 'window-width arxana-browser-chorus-notes-width)))
      (with-current-buffer buf
        (arxana-browser-chorus--sync-note-from-point))
      buf)))

(defun arxana-browser-chorus-location (item)
  (let ((entity-id (plist-get item :entity-id)))
    (when entity-id
      (format "arxana://chorus/%s" (url-hexify-string entity-id)))))

(defun arxana-browser-chorus--demo-line-bounds (passage)
  (cond
   ((and (stringp passage)
         (string-match "\\`lines? \\([0-9]+\\)-\\([0-9]+\\)\\(?::\\|\\'\\)" passage))
    (cons (string-to-number (match-string 1 passage))
          (string-to-number (match-string 2 passage))))
   ((and (stringp passage)
         (string-match "\\`line \\([0-9]+\\)\\(?::\\|\\'\\)" passage))
    (let ((line (string-to-number (match-string 1 passage))))
      (cons line line)))
   (t nil)))

(defun arxana-browser-chorus--demo-song-passage (spec)
  (let* ((annotated (car (plist-get spec :endpoints))))
    (alist-get :passage annotated)))

(defun arxana-browser-chorus--demo-source-passage (spec)
  (let* ((source (cadr (plist-get spec :endpoints))))
    (alist-get :passage source)))

(defun arxana-browser-chorus--demo-order-key (spec)
  (let* ((bounds (arxana-browser-chorus--demo-line-bounds
                  (arxana-browser-chorus--demo-song-passage spec)))
         (start (or (car-safe bounds) most-positive-fixnum))
         (end (or (cdr-safe bounds) most-positive-fixnum)))
    (list end start (downcase (or (arxana-browser-chorus--demo-source-passage spec) "")))))

(defun arxana-browser-chorus--demo-order-lessp (left right)
  (let ((left-key (arxana-browser-chorus--demo-order-key left))
        (right-key (arxana-browser-chorus--demo-order-key right)))
    (or (< (nth 0 left-key) (nth 0 right-key))
        (and (= (nth 0 left-key) (nth 0 right-key))
             (or (< (nth 1 left-key) (nth 1 right-key))
                 (and (= (nth 1 left-key) (nth 1 right-key))
                      (string-lessp (nth 2 left-key) (nth 2 right-key))))))))

(defun arxana-browser-chorus--abi-demo-supports ()
  (sort
   (seq-filter
    (lambda (spec)
      (and (equal (plist-get spec :hx-type) "annotation/supports")
           (equal arxana-browser-chorus-demo-song-id
                  (alist-get :entity-id (car (plist-get spec :endpoints))))))
    arxana-browser-songs-demo-annotations)
   #'arxana-browser-chorus--demo-order-lessp))

(defun arxana-browser-chorus--abi-demo-questions ()
  (mapcar
   (lambda (spec)
     (alist-get 'question (plist-get spec :props)))
   (seq-filter
    (lambda (spec)
      (and (equal (plist-get spec :hx-type) "annotation/open-question")
           (equal arxana-browser-chorus-demo-song-id
                  (alist-get :entity-id (car (plist-get spec :endpoints))))))
    arxana-browser-songs-demo-annotations)))

(defun arxana-browser-chorus--abi-demo-source ()
  (let ((passages (mapcar #'arxana-browser-chorus--demo-source-passage
                          (arxana-browser-chorus--abi-demo-supports))))
    (string-join passages "\n")))

(defun arxana-browser-chorus-demo-entity-specs ()
  "Return demo chorus entity specs assembled from existing `Abi` annotations."
  (list
   (list :id arxana-browser-chorus-demo-id
         :name "Abi Chorus (demo)"
         :type "arxana/chorus"
         :source (arxana-browser-chorus--abi-demo-source)
         :props `((chorus/source-song . ,arxana-browser-chorus-demo-song-id)
                  (chorus/source-songs . (,arxana-browser-chorus-demo-song-id))
                  (chorus/mode . "assembled-demo")
                  (chorus/annotation-ids . ,(mapcar (lambda (spec) (plist-get spec :id))
                                                    (arxana-browser-chorus--abi-demo-supports)))
                  (chorus/questions . ,(arxana-browser-chorus--abi-demo-questions))
                  (demo . "t")))))

(defun arxana-browser-chorus-import-demo (&optional dry-run)
  "Import demo chorus entities into XTDB.
When DRY-RUN is non-nil, return the payload specs without writing them."
  (interactive "P")
  (let ((specs (arxana-browser-chorus-demo-entity-specs))
        imported)
    (unless dry-run
      (unless (arxana-store-ensure-sync)
        (user-error "Futon sync is disabled; enable futon4-enable-sync first")))
    (dolist (spec specs)
      (if dry-run
          (push spec imported)
        (push (arxana-store-ensure-entity
               :id (plist-get spec :id)
               :name (plist-get spec :name)
               :type (plist-get spec :type)
               :source (plist-get spec :source)
               :props (plist-get spec :props))
              imported)))
    (setq imported (nreverse imported))
    (when (called-interactively-p 'interactive)
      (message "%s %d demo chorus%s"
               (if dry-run "Prepared" "Imported")
               (length imported)
               (if (= (length imported) 1) "" "es")))
    imported))

(provide 'arxana-browser-chorus)

;;; arxana-browser-chorus.el ends here
