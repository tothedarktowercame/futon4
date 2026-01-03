;;; arxana-scholium.el --- Scholium authoring helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Convenience wrappers around the classic scholium authoring flows.  These
;; commands expose the historical `new-scholium-mode` entry points while we
;; reanimate Part IV tooling.

;;; Code:

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
         (face (pcase status
                 (:anchored 'arxana-scholium-anchored-face)
                 (:orphaned 'arxana-scholium-orphaned-face)
                 (:fuzzy-matched 'arxana-scholium-fuzzy-face)
                 (_ 'arxana-scholium-anchored-face)))
         (ov (make-overlay beg end)))
    (overlay-put ov 'face face)
    (overlay-put ov 'arxana-scholium scholium)
    (overlay-put ov 'help-echo (plist-get scholium :content))
    (overlay-put ov 'evaporate t)
    (push ov arxana-scholium--overlays)
    ov))

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
