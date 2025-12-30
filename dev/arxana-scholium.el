;;; arxana-scholium.el --- Scholium authoring helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Convenience wrappers around the classic scholium authoring flows.  These
;; commands expose the historical `new-scholium-mode` entry points while we
;; reanimate Part IV tooling.

;;; Code:

(require 'subr-x)

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

(provide 'arxana-scholium)
