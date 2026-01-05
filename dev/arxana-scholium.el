;;; arxana-scholium.el --- Scholium authoring helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Convenience wrappers around the classic scholium authoring flows.  These
;; commands expose the historical `new-scholium-mode` entry points while we
;; reanimate Part IV tooling.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'arxana-ui nil t)

;; Resilient anchoring support
(declare-function arxana-links-make-anchor "arxana-links")
(declare-function arxana-links-make-scholium "arxana-links")
(declare-function arxana-links-persist-scholium "arxana-links")
(declare-function arxana-links-find-anchor "arxana-links")
(declare-function arxana-links-verify-scholium "arxana-links")
(declare-function arxana-links-load-scholia-for-doc "arxana-links")
(declare-function arxana-links-load-scholium-by-id "arxana-links")
(declare-function arxana-store-sync-enabled-p "arxana-store")

(defgroup arxana-scholium nil
  "Customization group for scholium authoring helpers."
  :group 'arxana)

(defvar new-scholium-name nil)
(defvar new-scholium-mode nil)
(defvar new-scholium-about nil)
(defvar arxana-scholium--display-buffer "*Arxana Scholia*")
(defvar arxana-scholium--display-source-buffer nil)
(defvar arxana-scholium--display-target-doc nil)
(defvar arxana-scholium--display-index nil)
(defvar arxana-scholium--display-highlight nil)
(defvar-local arxana-scholium--return-window-config nil)
(defvar-local arxana-scholium--last-highlight-id nil)
(defvar-local arxana-scholium--source-hook-installed nil)
(defvar-local arxana-scholium--last-hover-target nil)
(defvar-local arxana-scholium--hover-restore-buffer nil)
(defvar-local arxana-scholium--hover-restore-window nil)
(defvar-local arxana-scholium--last-echo-id nil)
(defconst arxana-scholium--focus-buffer "*Arxana Scholium*")

(defcustom arxana-scholium-hover-display t
  "When non-nil, hovering a scholium overlay shows its list in the side pane."
  :type 'boolean
  :group 'arxana-scholium)

(defcustom arxana-scholium-hover-echo t
  "When non-nil, show scholium content in the echo area on hover."
  :type 'boolean
  :group 'arxana-scholium)

(defvar arxana-scholium-display-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'arxana-scholium-display-visit)
    (define-key map (kbd "t") #'arxana-scholium-display-open-thread)
    (define-key map (kbd "<left>") #'arxana-scholium-display-left-or-return)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `arxana-scholium-display-mode'.")

(define-derived-mode arxana-scholium-display-mode special-mode "Arxana Scholia"
  "Major mode for scholia display buffers."
  (setq buffer-read-only t)
  (when (fboundp 'arxana-ui-mark-managed)
    (arxana-ui-mark-managed "Arxana Scholia")))

(defvar-local arxana-scholium--focus-return-buffer nil)
(defvar-local arxana-scholium--focus-return-window-config nil)

(defun arxana-scholium--resolve-source-buffer (source-buffer)
  (cond
   ((buffer-live-p source-buffer) source-buffer)
   ((and (derived-mode-p 'arxana-scholium-display-mode)
         (buffer-live-p arxana-scholium--display-source-buffer))
    arxana-scholium--display-source-buffer)
   ((buffer-live-p (get-buffer "*Arxana Docbook*"))
    (get-buffer "*Arxana Docbook*"))
   (t (current-buffer))))

(defvar arxana-scholium-focus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") #'arxana-scholium-focus-left-or-return)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `arxana-scholium-focus-mode'.")

(define-derived-mode arxana-scholium-focus-mode special-mode "Arxana Scholium"
  "Major mode for focused scholium buffers."
  (setq buffer-read-only t)
  (when (fboundp 'arxana-ui-mark-managed)
    (arxana-ui-mark-managed "Arxana Scholium")))

(defun arxana-scholium--compose-about (target)
  "Helper that normalizes TARGET into the expected scholium about format."
  (setq new-scholium-about (list (list target)))
  (make-scholium))

(defun arxana-scholium--ensure-support (fn)
  "Ensure FN is available for scholium workflows."
  (unless (fboundp fn)
    (user-error "Missing scholium support: %s" fn))
  t)

(defun arxana-scholium--set-name (name)
  "Set the scholium NAME when provided."
  (when (and name (stringp name))
    (setq new-scholium-name name)))

;;;###autoload
(defun arxana-scholium-compose (&optional name)
  "Open a scholium buffer in `new-scholium-mode'."
  (interactive
   (list (when current-prefix-arg
           (read-string "Scholium name: " new-scholium-name nil new-scholium-name))))
  (arxana-scholium--ensure-support 'make-scholium)
  (arxana-scholium--set-name name)
  (make-scholium)
  (message "Enter scholium text, then press C-c C-c to publish."))

(defun arxana-scholium-compose-from-region (beg end)
  "Create a scholium about the region from BEG to END."
  (interactive "r")
  (arxana-scholium--ensure-support 'make-scholium-about-part-of-current-article)
  (make-scholium-about-part-of-current-article beg end))

(defun arxana-scholium-authoring-mode (arg)
  "Toggle scholium authoring mode."
  (interactive "P")
  (arxana-scholium--ensure-support 'new-scholium-mode)
  (new-scholium-mode (if (and arg (> (prefix-numeric-value arg) 0)) 1 -1)))

(defun arxana-scholium--escape-and-display (escape-fn)
  "Exit scholium authoring via ESCAPE-FN and display the target article."
  (let ((target (and new-scholium-about (caar new-scholium-about))))
    (when (functionp escape-fn)
      (funcall escape-fn))
    (when (and target (fboundp 'display-article))
      (display-article target))))

;;;; =========================================================================
;;;; Resilient Scholia (using arxana-links anchoring)
;;;; =========================================================================

(defvar-local arxana-scholium--overlays nil
  "List of overlays for resilient scholia in current buffer.")

(defface arxana-scholium-anchored-face
  '((t :background "#e6ffe6"))
  "Face for anchored scholia regions (green tint)."
  :group 'arxana-scholium)

(defface arxana-scholium-orphaned-face
  '((t :background "#ffe6e6"))
  "Face for orphaned scholia regions (red tint)."
  :group 'arxana-scholium)

(defface arxana-scholium-fuzzy-face
  '((t :background "#fff0e6"))
  "Face for fuzzy-matched scholia regions (orange tint)."
  :group 'arxana-scholium)

(defface arxana-scholium-orphaned-list-face
  '((t :foreground "#6b7280" :slant italic))
  "Face for orphaned scholia in the list."
  :group 'arxana-scholium)

(defface arxana-scholium-legend-1
  '((t :foreground "#2563eb" :weight bold))
  "Legend face for scholia correlation."
  :group 'arxana-scholium)
(defface arxana-scholium-legend-2
  '((t :foreground "#dc2626" :weight bold))
  "Legend face for scholia correlation."
  :group 'arxana-scholium)
(defface arxana-scholium-legend-3
  '((t :foreground "#16a34a" :weight bold))
  "Legend face for scholia correlation."
  :group 'arxana-scholium)
(defface arxana-scholium-legend-4
  '((t :foreground "#d97706" :weight bold))
  "Legend face for scholia correlation."
  :group 'arxana-scholium)
(defface arxana-scholium-legend-5
  '((t :foreground "#7c3aed" :weight bold))
  "Legend face for scholia correlation."
  :group 'arxana-scholium)

(defconst arxana-scholium--legend-faces
  '(arxana-scholium-legend-1
    arxana-scholium-legend-2
    arxana-scholium-legend-3
    arxana-scholium-legend-4
    arxana-scholium-legend-5)
  "Faces used to correlate scholia list entries with overlays.")

(defun arxana-scholium--scholium-id (scholium)
  "Return a stable identifier for SCHOLIUM."
  (or (plist-get scholium :xt/id)
      (plist-get scholium :name)
      (plist-get scholium :content)))

(defun arxana-scholium--legend-face (scholium)
  "Pick a legend face for SCHOLIUM."
  (let* ((id (or (arxana-scholium--scholium-id scholium) ""))
         (idx (mod (sxhash id) (length arxana-scholium--legend-faces))))
    (nth idx arxana-scholium--legend-faces)))

(defun arxana-scholium--reply-target (scholium)
  "Return reply target id if SCHOLIUM points at another scholium."
  (let ((target (plist-get scholium :target-doc)))
    (when (and (stringp target)
               (string-prefix-p "scholium:" target))
      (substring target (length "scholium:")))))

(defun arxana-scholium--reply-p (scholium)
  "Return non-nil if SCHOLIUM targets another scholium."
  (let ((target (plist-get scholium :target-doc)))
    (and (stringp target)
         (string-prefix-p "scholium:" target))))

(defun arxana-scholium--render-list (scholia)
  (if (null scholia)
      (insert "  (none)\n")
    (dolist (scholium scholia)
      (let* ((status (plist-get scholium :status))
             (content (or (plist-get scholium :content) ""))
             (sch-id (arxana-scholium--scholium-id scholium))
             (legend-face (arxana-scholium--legend-face scholium))
             (line (format "- [%s] %s\n" (or status "unknown") content)))
        (let ((start (point))
              (marker (if (eq status :orphaned)
                          (propertize "[!] " 'face 'arxana-scholium-orphaned-list-face)
                        (arxana-scholium--legend-marker legend-face))))
          (insert marker)
          (insert line)
          (add-text-properties start (point)
                               (list 'arxana-scholium scholium
                                     'arxana-scholium-id sch-id))
          (add-text-properties (+ start (length marker)) (point)
                               (list 'face (if (eq status :orphaned)
                                               'arxana-scholium-orphaned-list-face
                                             legend-face)))
          (puthash sch-id (copy-marker start) arxana-scholium--display-index))))))

(defun arxana-scholium--render-focus (scholium)
  (let ((buf (get-buffer-create arxana-scholium--focus-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (arxana-scholium-focus-mode)
        (insert (format "Scholium %s\n\n" (or (plist-get scholium :xt/id) "unknown")))
        (insert (format "Target: %s\n" (or (plist-get scholium :target-doc) "unknown")))
        (insert (format "Status: %s\n\n" (or (plist-get scholium :status) "unknown")))
        (insert (or (plist-get scholium :content) ""))))
    buf))

(defun arxana-scholium--legend-marker (legend-face)
  "Return a marker string with LEGEND-FACE."
  (propertize "[#] " 'face legend-face))

;;;###autoload
(defun arxana-scholium-create-resilient (beg end content)
  "Create a resilient scholium on region BEG to END with CONTENT.
Uses content-hash anchoring that survives minor edits."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end)
             (read-string "Annotation: "))
     (user-error "Select a region first")))
  (unless (fboundp 'arxana-links-make-anchor)
    (user-error "arxana-links not loaded"))
  (let* ((anchor (arxana-links-make-anchor (current-buffer) beg end))
         (doc-name (or (buffer-file-name) (buffer-name)))
         (scholium (arxana-links-make-scholium
                    :target-doc doc-name
                    :anchor anchor
                    :content content)))
    (if (and (fboundp 'arxana-store-sync-enabled-p)
             (arxana-store-sync-enabled-p)
             (arxana-links-persist-scholium scholium))
        (progn
          (arxana-scholium--add-overlay beg end scholium)
          (message "Resilient scholium created and persisted"))
      (progn
        (arxana-scholium--add-overlay beg end scholium)
        (message "Resilient scholium created (local only - Futon sync disabled)")))))

(defun arxana-scholium--add-overlay (beg end scholium)
  "Add visual overlay from BEG to END for SCHOLIUM."
  (let* ((status (plist-get scholium :status))
         (status-face (pcase status
                        (:anchored 'arxana-scholium-anchored-face)
                        (:orphaned 'arxana-scholium-orphaned-face)
                        (:fuzzy-matched 'arxana-scholium-fuzzy-face)
                        (_ 'arxana-scholium-anchored-face)))
         (legend-face (arxana-scholium--legend-face scholium))
         (ov (make-overlay beg end)))
    (overlay-put ov 'face (list status-face legend-face))
    (overlay-put ov 'arxana-scholium scholium)
    (overlay-put ov 'arxana-scholium-id (arxana-scholium--scholium-id scholium))
    (overlay-put ov 'arxana-scholium-face legend-face)
    (overlay-put ov 'help-echo (plist-get scholium :content))
    (overlay-put ov 'evaporate t)
    (push ov arxana-scholium--overlays)
    (unless arxana-scholium--source-hook-installed
      (add-hook 'post-command-hook #'arxana-scholium--sync-display-from-point nil t)
      (setq arxana-scholium--source-hook-installed t))
    ov))

(defun arxana-scholium--display-highlight-id (scholium-id)
  "Highlight the scholium entry matching SCHOLIUM-ID in the display buffer."
  (let ((buf (get-buffer arxana-scholium--display-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((marker (and arxana-scholium--display-index
                           (gethash scholium-id arxana-scholium--display-index))))
          (when (overlayp arxana-scholium--display-highlight)
            (delete-overlay arxana-scholium--display-highlight))
          (when (and marker (marker-position marker))
            (goto-char (marker-position marker))
            (let ((start (line-beginning-position))
                  (end (line-end-position)))
              (setq arxana-scholium--display-highlight (make-overlay start end))
              (overlay-put arxana-scholium--display-highlight 'face 'highlight))))))))

(defun arxana-scholium--display-target-for-scholium (scholium)
  "Resolve the doc target to display for SCHOLIUM."
  (let ((target (plist-get scholium :target-doc)))
    (if (and (stringp target) (string-prefix-p "scholium:" target))
        (let ((parent-id (substring target (length "scholium:"))))
          (when (and (fboundp 'arxana-links-load-scholium-by-id) parent-id)
            (let ((parent (arxana-links-load-scholium-by-id
                           (format "scholium:%s" parent-id))))
              (or (plist-get parent :target-doc) target))))
      target)))

(defun arxana-scholium--display-for-target (target-doc source-buffer &optional hover-p)
  (when (and target-doc (fboundp 'arxana-links-load-scholia-for-doc))
    (let* ((source-win (get-buffer-window source-buffer (selected-frame)))
           (other-win (and source-win
                           (or (window-in-direction 'right source-win)
                               (seq-find (lambda (win)
                                           (not (eq win source-win)))
                                         (window-list (selected-frame) 'no-mini))))))
      (with-current-buffer source-buffer
        (setq arxana-scholium--hover-restore-window other-win)
        (setq arxana-scholium--hover-restore-buffer
              (and other-win (window-buffer other-win))))
      (let ((url-show-status nil))
        (let ((items (arxana-links-load-scholia-for-doc target-doc)))
        (if hover-p
            (let ((buf (get-buffer-create arxana-scholium--display-buffer)))
              (arxana-scholium--render-display target-doc (or items '()) source-buffer :skip)
              (when (window-live-p other-win)
                (set-window-buffer other-win buf)))
          (arxana-scholium--render-display target-doc (or items '()) source-buffer)))))))

(defun arxana-scholium--display-visible-p ()
  (get-buffer-window arxana-scholium--display-buffer (selected-frame)))

(defun arxana-scholium--sync-display-from-point ()
  "Sync scholia display highlight with overlay at point."
  (let ((scholium-id nil)
        (target-doc nil)
        (scholium-content nil))
    (dolist (ov (overlays-at (point)))
      (when (and (null scholium-id) (overlay-get ov 'arxana-scholium-id))
        (setq scholium-id (overlay-get ov 'arxana-scholium-id)))
      (when (and (null target-doc) (overlay-get ov 'arxana-scholium))
        (let ((scholium (overlay-get ov 'arxana-scholium)))
          (setq scholium-content (or scholium-content
                                     (plist-get scholium :content)))
          (setq target-doc
                (arxana-scholium--display-target-for-scholium scholium)))))
    (unless (equal scholium-id arxana-scholium--last-highlight-id)
      (setq arxana-scholium--last-highlight-id scholium-id)
      (when scholium-id
        (arxana-scholium--display-highlight-id scholium-id)))
    (when (bound-and-true-p arxana-scholium-hover-echo)
      (cond
       ((and scholium-id (not (equal scholium-id arxana-scholium--last-echo-id)))
        (setq arxana-scholium--last-echo-id scholium-id)
        (when scholium-content
          (message "%s" scholium-content)))
       ((null scholium-id)
        (setq arxana-scholium--last-echo-id nil))))
    (when (and arxana-scholium-hover-display target-doc)
      (when (or (not (equal target-doc arxana-scholium--last-hover-target))
                (not (arxana-scholium--display-visible-p)))
        (setq arxana-scholium--last-hover-target target-doc)
        (arxana-scholium--display-for-target target-doc (current-buffer) t)))
    (when (and arxana-scholium-hover-display (null target-doc)
               arxana-scholium--last-hover-target)
      (setq arxana-scholium--last-hover-target nil)
      (when (and (window-live-p arxana-scholium--hover-restore-window)
                 (buffer-live-p arxana-scholium--hover-restore-buffer))
        (set-window-buffer arxana-scholium--hover-restore-window
                           arxana-scholium--hover-restore-buffer)))))

(defun arxana-scholium--arrange-windows (source-buffer display-buffer)
  "Arrange windows so SOURCE-BUFFER is left of DISPLAY-BUFFER."
  (let* ((frame (selected-frame))
         (display-windows (get-buffer-window-list display-buffer nil t))
         (source-window (or (get-buffer-window source-buffer frame)
                            (selected-window)))
         (docbook-p (with-current-buffer source-buffer
                      (bound-and-true-p arxana-docbook--entry-book)))
         (code-window (when docbook-p
                        (cl-find-if
                         (lambda (win)
                           (let ((buf (window-buffer win)))
                             (and (buffer-live-p buf)
                                  (not (eq buf source-buffer))
                                  (buffer-file-name buf))))
                         (window-list frame :no-minibuf))))
         (code-buffer (and code-window (window-buffer code-window))))
    (dolist (win display-windows)
      (let ((win-frame (window-frame win)))
        (unless (eq win-frame frame)
          (delete-window win))))
    (if (and docbook-p code-buffer)
        (progn
          (select-window code-window)
          (delete-other-windows)
          (let* ((left (selected-window))
                 (mid (split-window-right))
                 (right (progn (select-window mid)
                               (split-window-right))))
            (set-window-buffer left code-buffer)
            (set-window-buffer mid source-buffer)
            (set-window-buffer right display-buffer)
            (select-window mid)))
      (select-window source-window)
      (delete-other-windows)
      (let* ((left (selected-window))
             (right (split-window-right)))
        (set-window-buffer left source-buffer)
        (set-window-buffer right display-buffer)
        (select-window left)))))

(defun arxana-scholium--arrange-thread-windows (focus-buffer display-buffer)
  "Arrange windows so FOCUS-BUFFER is left of DISPLAY-BUFFER."
  (let* ((frame (selected-frame)))
    (delete-other-windows)
    (let* ((left (selected-window))
           (right (split-window-right)))
      (set-window-buffer left focus-buffer)
      (set-window-buffer right display-buffer)
      (select-window left))))

(defun arxana-scholium--render-display (target-doc scholia source-buffer &optional arrange-p)
  (let ((buf (get-buffer-create arxana-scholium--display-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (arxana-scholium-display-mode)
        (setq-local arxana-scholium--display-source-buffer source-buffer)
        (setq-local arxana-scholium--display-target-doc target-doc)
        (setq-local arxana-scholium--display-index (make-hash-table :test 'equal))
        (setq-local arxana-scholium--return-window-config
                    (current-window-configuration))
        (insert (format "Scholia for %s\n\n" target-doc))
        (if (null scholia)
            (insert "No scholia found.\n")
          (arxana-scholium--render-list scholia))))
    (unless (eq arrange-p :skip)
      (arxana-scholium--arrange-windows source-buffer buf))))

;;;###autoload
(defun arxana-scholium-display-visit ()
  "Jump to the scholium's target region in the source buffer."
  (interactive)
  (let ((scholium (get-text-property (point) 'arxana-scholium))
        (source-buffer arxana-scholium--display-source-buffer))
    (unless scholium
      (user-error "No scholium at point"))
    (unless (buffer-live-p source-buffer)
      (user-error "Source buffer is gone"))
    (let ((bounds (arxana-links-verify-scholium source-buffer scholium)))
      (unless bounds
        (user-error "No anchor bounds for scholium"))
      (pop-to-buffer source-buffer)
      (goto-char (car bounds))
      (push-mark (cdr bounds) t t))))

;;;###autoload
(defun arxana-scholium-display-left-or-return ()
  "Move left or return to the source buffer when at point-min."
  (interactive)
  (if (> (point) (point-min))
      (backward-char)
    (if (and arxana-scholium--return-window-config
             (window-configuration-p arxana-scholium--return-window-config))
        (set-window-configuration arxana-scholium--return-window-config)
      (let ((source arxana-scholium--display-source-buffer))
        (when (buffer-live-p source)
          (pop-to-buffer source))))))

;;;###autoload
(defun arxana-scholium-focus-left-or-return ()
  "Move left or return to the scholia list when at point-min."
  (interactive)
  (if (> (point) (point-min))
      (backward-char)
    (if (and arxana-scholium--focus-return-window-config
             (window-configuration-p arxana-scholium--focus-return-window-config))
        (set-window-configuration arxana-scholium--focus-return-window-config)
      (let ((target arxana-scholium--focus-return-buffer))
        (when (buffer-live-p target)
          (pop-to-buffer target))))))

;;;###autoload
(defun arxana-scholium-display-open-thread ()
  "Open the scholia thread for the entry at point."
  (interactive)
  (let* ((scholium (get-text-property (point) 'arxana-scholium))
         (source-buffer (arxana-scholium--resolve-source-buffer
                         arxana-scholium--display-source-buffer)))
    (unless scholium
      (user-error "No scholium at point"))
    (let* ((parent-id (or (arxana-scholium--reply-target scholium)
                          (plist-get scholium :xt/id)))
           (parent (or (and parent-id
                            (fboundp 'arxana-links-load-scholium-by-id)
                            (arxana-links-load-scholium-by-id parent-id))
                       scholium))
           (thread-target (and parent-id (format "scholium:%s" parent-id)))
           (replies (and thread-target
                         (fboundp 'arxana-links-load-scholia-for-doc)
                         (arxana-links-load-scholia-for-doc thread-target)))
           (backlinks (let ((target (plist-get parent :target-doc)))
                        (when (and target (fboundp 'arxana-links-load-scholia-for-doc))
                          (cl-remove-if
                           (lambda (item)
                             (arxana-scholium--reply-p item))
                           (arxana-links-load-scholia-for-doc target))))))
      (let ((focus-buffer (arxana-scholium--render-focus parent))
            (display-buffer (get-buffer-create arxana-scholium--display-buffer)))
        (with-current-buffer focus-buffer
          (setq-local arxana-scholium--focus-return-buffer display-buffer)
          (setq-local arxana-scholium--focus-return-window-config
                      (current-window-configuration)))
        (with-current-buffer display-buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (arxana-scholium-display-mode)
            (setq-local arxana-scholium--display-source-buffer source-buffer)
            (setq-local arxana-scholium--display-target-doc thread-target)
            (setq-local arxana-scholium--display-index (make-hash-table :test 'equal))
            (setq-local arxana-scholium--return-window-config
                        (current-window-configuration))
            (insert (format "Thread for %s\n\n" (or thread-target "unknown")))
            (insert "Replies\n")
            (arxana-scholium--render-list replies)
            (insert "\nBacklinks\n")
            (arxana-scholium--render-list backlinks)))
        (arxana-scholium--arrange-thread-windows focus-buffer display-buffer)))))

;;;###autoload
(defun arxana-scholium-show-for-doc (target-doc &optional source-buffer scholia)
  "Load scholia for TARGET-DOC and display them with overlays."
  (interactive "sTarget doc (docbook://... or code://...): ")
  (unless (fboundp 'arxana-links-load-scholia-for-doc)
    (user-error "Missing scholium loader: arxana-links-load-scholia-for-doc"))
  (let* ((source (arxana-scholium--resolve-source-buffer source-buffer))
         (items (or scholia (arxana-links-load-scholia-for-doc target-doc))))
    (with-current-buffer source
      (when (fboundp 'arxana-scholium-clear-overlays)
        (arxana-scholium-clear-overlays))
      (dolist (scholium items)
        (let ((bounds (arxana-links-verify-scholium source scholium)))
          (when bounds
            (arxana-scholium--add-overlay (car bounds) (cdr bounds) scholium)))))
      (unless arxana-scholium--source-hook-installed
        (add-hook 'post-command-hook #'arxana-scholium--sync-display-from-point nil t)
        (setq arxana-scholium--source-hook-installed t))
    (arxana-scholium--render-display target-doc items source)
    items))

;;;###autoload
(defun arxana-scholium-verify-all ()
  "Verify all resilient scholia in current buffer.
Re-finds anchors and updates overlay positions/colors."
  (interactive)
  (unless (fboundp 'arxana-links-verify-scholium)
    (user-error "arxana-links not loaded"))
  (let ((verified 0) (orphaned 0))
    (dolist (ov arxana-scholium--overlays)
      (when (overlay-buffer ov)
        (let* ((scholium (overlay-get ov 'arxana-scholium))
               (bounds (arxana-links-verify-scholium (current-buffer) scholium))
               (status (plist-get scholium :status)))
          (if bounds
              (progn
                (move-overlay ov (car bounds) (cdr bounds))
                (overlay-put ov 'face
                             (if (eq status :fuzzy-matched)
                                 'arxana-scholium-fuzzy-face
                               'arxana-scholium-anchored-face))
                (setq verified (1+ verified)))
            (overlay-put ov 'face 'arxana-scholium-orphaned-face)
            (setq orphaned (1+ orphaned))))))
    (message "Verified: %d anchored, %d orphaned" verified orphaned)))

;;;###autoload
(defun arxana-scholium-clear-overlays ()
  "Remove all scholium overlays from current buffer."
  (interactive)
  (dolist (ov arxana-scholium--overlays)
    (when (overlay-buffer ov)
      (delete-overlay ov)))
  (setq arxana-scholium--overlays nil)
  (message "Cleared all scholium overlays"))

;;;###autoload
(defun arxana-scholium-at-point ()
  "Display the scholium annotation at point, if any."
  (interactive)
  (let ((scholium nil))
    (dolist (ov (overlays-at (point)))
      (when (overlay-get ov 'arxana-scholium)
        (setq scholium (overlay-get ov 'arxana-scholium))))
    (if scholium
        (message "Scholium: %s (status: %s)"
                 (plist-get scholium :content)
                 (plist-get scholium :status))
      (message "No scholium at point"))))

;;;###autoload
(defun arxana-scholium-edit-at-point ()
  "Edit the scholium annotation at point."
  (interactive)
  (let ((scholium nil)
        (overlay nil))
    (dolist (ov (overlays-at (point)))
      (when (and (null scholium) (overlay-get ov 'arxana-scholium))
        (setq scholium (overlay-get ov 'arxana-scholium))
        (setq overlay ov)))
    (unless scholium
      (user-error "No scholium at point"))
    (let* ((old (or (plist-get scholium :content) ""))
           (target-doc (plist-get scholium :target-doc))
           (return-config (current-window-configuration))
           (edit-buf (get-buffer-create "*Arxana Scholium Edit*")))
      (with-current-buffer edit-buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert old)
          (arxana-scholium-edit-mode)
          (setq-local arxana-scholium--edit-scholium scholium)
          (setq-local arxana-scholium--edit-overlay overlay)
          (setq-local arxana-scholium--edit-target-doc target-doc)
          (setq-local arxana-scholium--edit-return-config return-config)))
      (pop-to-buffer edit-buf))))

(defvar-local arxana-scholium--edit-scholium nil)
(defvar-local arxana-scholium--edit-overlay nil)
(defvar-local arxana-scholium--edit-target-doc nil)
(defvar-local arxana-scholium--edit-return-config nil)

(defvar arxana-scholium-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'arxana-scholium-edit-save)
    (define-key map (kbd "C-c C-k") #'arxana-scholium-edit-cancel)
    map)
  "Keymap for `arxana-scholium-edit-mode'.")

(define-derived-mode arxana-scholium-edit-mode text-mode "Scholium Edit"
  "Major mode for editing scholium content."
  (setq header-line-format "Scholium Edit  C-c C-s save  C-c C-k cancel"))

(defun arxana-scholium--edit-finish ()
  (let ((config arxana-scholium--edit-return-config))
    (kill-buffer (current-buffer))
    (when (and config (window-configuration-p config))
      (set-window-configuration config))))

;;;###autoload
(defun arxana-scholium-edit-save ()
  "Save the current scholium edit and exit."
  (interactive)
  (let* ((scholium arxana-scholium--edit-scholium)
         (overlay arxana-scholium--edit-overlay)
         (target-doc arxana-scholium--edit-target-doc)
         (new (string-trim-right (buffer-substring-no-properties
                                  (point-min) (point-max))))
         (old (and scholium (or (plist-get scholium :content) ""))))
    (unless scholium
      (user-error "No scholium to save"))
    (if (string= new old)
        (progn
          (message "No changes")
          (arxana-scholium--edit-finish))
      (let ((updated (plist-put (copy-sequence scholium) :content new)))
        (if (and (fboundp 'arxana-store-sync-enabled-p)
                 (arxana-store-sync-enabled-p)
                 (fboundp 'arxana-links-persist-scholium))
            (arxana-links-persist-scholium updated)
          (message "Futon sync disabled; updated locally only"))
        (when (overlayp overlay)
          (overlay-put overlay 'arxana-scholium updated)
          (overlay-put overlay 'help-echo new))
        (let ((display-buf (get-buffer arxana-scholium--display-buffer)))
          (when (and target-doc (buffer-live-p display-buf))
            (with-current-buffer display-buf
              (when (and (boundp 'arxana-scholium--display-target-doc)
                         (equal arxana-scholium--display-target-doc target-doc))
                (let ((url-show-status nil))
                  (arxana-scholium--render-display
                   target-doc
                   (arxana-links-load-scholia-for-doc target-doc)
                   (current-buffer)
                   :skip))))))
        (message "Scholium updated")
        (arxana-scholium--edit-finish)))))

;;;###autoload
(defun arxana-scholium-edit-cancel ()
  "Cancel scholium editing and exit."
  (interactive)
  (message "Scholium edit cancelled")
  (arxana-scholium--edit-finish))

(provide 'arxana-scholium)
