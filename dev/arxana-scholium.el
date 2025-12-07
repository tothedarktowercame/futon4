;;; arxana-scholium.el --- Scholium authoring helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Convenience wrappers around the classic scholium authoring flows.  These
;; commands make it easier to discover the historical `new-scholium-mode`
;; entry points while we reanimate the Part IV tooling.  They defer to the
;; original Org-derived implementation inside `arxana-tangled.el` so that the
;; existing data structures and hooks keep working.

;;; Code:

(require 'subr-x)

(defgroup arxana-scholium nil
  "Customization group for scholium authoring helpers."
  :group 'arxana)

(defvar new-scholium-name nil)
(defvar new-scholium-mode nil)

(defun arxana-scholium--compose-about (target)
  "Helper that normalizes TARGET into the expected scholium about format."
  (setq new-scholium-about (list (list target)))
  (make-scholium))

(defun arxana-scholium--first-about-target (about)
  "Return the primary article targeted by ABOUT data."
  (when (and about (listp about))
    (let ((entry (car about)))
      (cond
       ((and (listp entry) (car entry)) (car entry))
       ((symbolp entry) entry)
       ((stringp entry) entry)
       (t nil)))))

(defun arxana-scholium--display-about-target (about)
  "Redisplay the article referenced by ABOUT if possible."
  (let ((target (arxana-scholium--first-about-target about)))
    (when (and target (fboundp 'display-article))
      (ignore-errors
        (display-article target)))))

(declare-function make-scholium "arxana-tangled" (&rest _))
(declare-function make-scholium-about-current-article "arxana-tangled" nil)
(declare-function make-scholium-about-part-of-current-article "arxana-tangled" (beg end))
(declare-function make-scholium-about-current-line "arxana-tangled" nil)
(declare-function make-scholium-about-current-buffer "arxana-tangled" nil)
(declare-function make-scholium-about-several-parts-of-current-article "arxana-tangled" nil)
(declare-function escape-scholium-creation "arxana-tangled" nil)
(declare-function reading-regions-mode "arxana-tangled" (&optional arg))
(declare-function escape-reading-regions-mode "arxana-tangled" nil)
(declare-function new-scholium-mode "arxana-tangled" (&optional arg))

(defun arxana-scholium--ensure-support (fn)
  "Ensure scholium helper FN is available."
  (unless (fboundp fn)
    (user-error "Scholium authoring helpers are not loaded; run `arxana-build' first"))
  fn)

(defun arxana-scholium--set-name (name)
  "Remember NAME for the next scholium if provided."
  (when (and name (not (string-empty-p name)))
    (setq new-scholium-name name)))

;;;###autoload
(defun arxana-scholium-compose (&optional name)
  "Open a scholium buffer in `new-scholium-mode'.
Provide NAME with a prefix argument to prefill the scholium title."
  (interactive
   (list (when current-prefix-arg
           (read-string "Scholium name: " new-scholium-name nil new-scholium-name))))
  (arxana-scholium--ensure-support 'make-scholium)
  (arxana-scholium--set-name name)
  (make-scholium)
  (message "Enter scholium text, then press C-c C-c to publish."))

;;;###autoload
(defun arxana-scholium-compose-about-current-article ()
  "Start authoring a scholium describing the current article."
  (interactive)
  (arxana-scholium--ensure-support 'make-scholium-about-current-article)
  (make-scholium-about-current-article))

;;;###autoload
(defun arxana-scholium-compose-from-region (beg end)
  "Create a scholium about the region between BEG and END."
  (interactive "r")
  (arxana-scholium--ensure-support 'make-scholium-about-part-of-current-article)
  (make-scholium-about-part-of-current-article beg end))

;;;###autoload
(defun arxana-scholium-compose-from-line ()
  "Quick scholium for the current line."
  (interactive)
  (arxana-scholium--ensure-support 'make-scholium-about-current-line)
  (make-scholium-about-current-line))

;;;###autoload
(defun arxana-scholium-compose-from-buffer ()
  "Create a scholium that references the current buffer."
  (interactive)
  (arxana-scholium--ensure-support 'make-scholium-about-current-buffer)
  (make-scholium-about-current-buffer))

;;;###autoload
(defun arxana-scholium-compose-collect-regions ()
  "Collect multiple regions for a scholium via `reading-regions-mode'."
  (interactive)
  (arxana-scholium--ensure-support 'make-scholium-about-several-parts-of-current-article)
  (make-scholium-about-several-parts-of-current-article))

;;;###autoload
(defun arxana-scholium-exit-region-collection ()
  "Finish collecting regions and return to scholium editing."
  (interactive)
  (arxana-scholium--ensure-support 'escape-reading-regions-mode)
  (escape-reading-regions-mode))

;;;###autoload
(defun arxana-scholium-finish ()
  "Finalize the scholium currently being edited."
  (interactive)
  (arxana-scholium--ensure-support 'escape-scholium-creation)
  (escape-scholium-creation))

(define-minor-mode arxana-scholium-authoring-mode
  "Lightweight wrapper around `new-scholium-mode'."
  :init-value nil
  :lighter " Scholium"
  (arxana-scholium--ensure-support 'new-scholium-mode)
  (new-scholium-mode (if arxana-scholium-authoring-mode 1 -1))
  (when arxana-scholium-authoring-mode
    (message "Use C-c C-c to store the scholium.")))

(with-eval-after-load 'arxana-tangled
  (when (fboundp 'new-scholium-mode)
    (defalias 'arxana-scholium-mode 'new-scholium-mode))

  (when (fboundp 'make-scholium)
    (defun arxana-scholium--make-about-current-article ()
      "Fixed variant that ensures backlinks can resolve the article name."
      (interactive)
      (when name-of-current-article
        (arxana-scholium--compose-about name-of-current-article)))
    (fset 'make-scholium-about-current-article
          #'arxana-scholium--make-about-current-article))

  (when (and (fboundp 'make-scholium)
             (fboundp 'call-if-user-adds-current-buffer-to-article-list))
    (defun arxana-scholium--make-about-current-buffer ()
      "Ensure the scholium about list references the current buffer name."
      (interactive)
      (let ((article (get-article (buffer-name (current-buffer)))))
        (if (not article)
            (call-if-user-adds-current-buffer-to-article-list
             #'arxana-scholium--make-about-current-buffer)
          (arxana-scholium--compose-about (buffer-name (current-buffer))))))
    (fset 'make-scholium-about-current-buffer
          #'arxana-scholium--make-about-current-buffer))

  (when (fboundp 'make-scholium)
    (defun arxana-scholium--make-about-current-scholium ()
      "Normalize scholium-about data when referencing another scholium."
      (interactive)
      (when (equal (buffer-name (current-buffer)) "Scholia Display")
        (let ((cur (name-of-current-scholium)))
          (when cur
            (arxana-scholium--compose-about cur)))))
    (fset 'make-scholium-about-current-scholium
          #'arxana-scholium--make-about-current-scholium))

  (when (fboundp 'escape-scholium-creation)
    (defun arxana-scholium--escape-and-display (orig-fn &rest args)
      "Wrap ORIG-FN to redisplay the scholium target after saving."
      (let ((about new-scholium-about))
        (prog1 (apply orig-fn args)
          (when about
            (arxana-scholium--display-about-target about)))))
    (advice-add 'escape-scholium-creation :around
                #'arxana-scholium--escape-and-display)))

(provide 'arxana-scholium)

;;; arxana-scholium.el ends here
