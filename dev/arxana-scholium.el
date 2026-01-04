;;; arxana-scholium.el --- Scholium authoring helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Convenience wrappers around the classic scholium authoring flows.  These
;; commands expose the historical `new-scholium-mode` entry points while we
;; reanimate Part IV tooling.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Resilient anchoring support
(declare-function arxana-links-make-anchor "arxana-links")
(declare-function arxana-links-make-scholium "arxana-links")
(declare-function arxana-links-persist-scholium "arxana-links")
(declare-function arxana-links-find-anchor "arxana-links")
(declare-function arxana-links-verify-scholium "arxana-links")
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
(defvar-local arxana-scholium--last-highlight-id nil)
(defvar-local arxana-scholium--source-hook-installed nil)

(defvar arxana-scholium-display-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'arxana-scholium-display-visit)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `arxana-scholium-display-mode'.")

(define-derived-mode arxana-scholium-display-mode special-mode "Arxana Scholia"
  "Major mode for scholia display buffers."
  (setq buffer-read-only t))

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

(defun arxana-scholium--sync-display-from-point ()
  "Sync scholia display highlight with overlay at point."
  (let ((scholium-id nil))
    (dolist (ov (overlays-at (point)))
      (when (and (null scholium-id) (overlay-get ov 'arxana-scholium-id))
        (setq scholium-id (overlay-get ov 'arxana-scholium-id))))
    (unless (equal scholium-id arxana-scholium--last-highlight-id)
      (setq arxana-scholium--last-highlight-id scholium-id)
      (when scholium-id
        (arxana-scholium--display-highlight-id scholium-id)))))

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

(defun arxana-scholium--render-display (target-doc scholia source-buffer)
  (let ((buf (get-buffer-create arxana-scholium--display-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (arxana-scholium-display-mode)
        (setq-local arxana-scholium--display-source-buffer source-buffer)
        (setq-local arxana-scholium--display-target-doc target-doc)
        (setq-local arxana-scholium--display-index (make-hash-table :test 'equal))
        (insert (format "Scholia for %s\n\n" target-doc))
        (if (null scholia)
            (insert "No scholia found.\n")
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
                (puthash sch-id (copy-marker start) arxana-scholium--display-index)))))))
    (arxana-scholium--arrange-windows source-buffer buf)))

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
(defun arxana-scholium-show-for-doc (target-doc &optional source-buffer scholia)
  "Load scholia for TARGET-DOC and display them with overlays."
  (interactive "sTarget doc (docbook://... or code://...): ")
  (unless (fboundp 'arxana-links-load-scholia-for-doc)
    (user-error "Missing scholium loader: arxana-links-load-scholia-for-doc"))
  (let* ((source (or source-buffer (current-buffer)))
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

(provide 'arxana-scholium)
