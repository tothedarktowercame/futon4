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

;; ... (rest of arxana-scholium.el in smaller blocks) ...
