;;; arxana-relations.el --- Relation browsing helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive commands that render Futon relation data (/ego, /cooccur, /tail)
;; inside Emacs so users no longer need to shell out to curl.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'arxana-store)

(declare-function futon4-lookup-article-id "arxana-tangled" (name))
(declare-function futon4--article-id-for "arxana-tangled" (name &optional path))

(defgroup arxana-relations nil
  "Buffers for browsing Futon relation data."
  :group 'arxana)

(defconst arxana-relations-buffer-name "*Arxana Relations*")

(defvar-local arxana-relations-context nil
  "Plist describing the last Futon render in this buffer.")

(defvar arxana-relations-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'arxana-relations-refresh)
    (define-key map (kbd "p") #'arxana-relations-provenance)
    (define-key map (kbd "y") #'arxana-relations-copy-id)
    map)
  "Keymap for `arxana-relations-buffer-mode'.")

(define-minor-mode arxana-relations-buffer-mode
  "Minor mode that adds refresh/provenance commands to relation buffers."
  :lighter " ArxRel"
  :keymap arxana-relations-buffer-mode-map
  (if arxana-relations-buffer-mode
      (setq-local revert-buffer-function #'arxana-relations--revert-buffer)
    (kill-local-variable 'revert-buffer-function)))

(defun arxana-relations--type-label (type)
  (cond
   ((keywordp type) (substring (symbol-name type) 1))
   ((symbolp type) (symbol-name type))
   ((stringp type) type)
   (t (format "%s" type))))

(defun arxana-relations--name-with-type (name type)
  (if (and name type)
      (format "%s (%s)" name (arxana-relations--type-label type))
    (or name "?")))

(defun arxana-relations--entity-name (entity)
  (or (alist-get :entity/name entity)
      (alist-get :name entity)))

(defun arxana-relations--entity-type (entity)
  (or (alist-get :entity/type entity)
      (alist-get :type entity)))

(defun arxana-relations--insert-line (fmt &rest args)
  (insert (apply #'format (concat fmt "\n") args)))

(defun arxana-relations--insert-section (title lines)
  (arxana-relations--insert-line "%s" title)
  (if (seq-empty-p lines)
      (arxana-relations--insert-line "  (none)")
    (dolist (line lines)
      (arxana-relations--insert-line "  - %s" line))))

(defun arxana-relations--render-ego (body)
  ;; ... body of function ...
  )

(defun arxana-relations--render-cooccur (body)
  ;; ... body of function ...
  )

(defun arxana-relations--render-tail (body)
  ;; ... body of function ...
  )

(defun arxana-relations--render-fallback (body)
  (arxana-relations--insert-line "Raw response:")
  (pp body (current-buffer)))

;; ... remaining functions from dev/arxana-relations.el ...

(provide 'arxana-relations)

;;; arxana-relations.el ends here
