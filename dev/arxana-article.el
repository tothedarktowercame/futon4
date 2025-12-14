;;; arxana-article.el --- Article lifecycle helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Captures the bridge between classic Arxana operations (scholia, metadata
;; hooks, deletion) and the Futon storage helpers.

;;; Code:

(require 'arxana-store)
(require 'subr-x)

(defvar arxana-article-path-cache (make-hash-table :test 'equal)
  "Cache of canonical paths keyed by article name.")

(defun arxana-article--canonical-path (path)
  (when (and path (fboundp 'futon4--canonical-path))
    (setq path (futon4--canonical-path path)))
  path)

;; ... (rest of arxana-article.el broken into smaller blocks) ...

(provide 'arxana-article)

;;; arxana-article.el ends here
