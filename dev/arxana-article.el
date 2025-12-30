;;; arxana-article.el --- Article lifecycle helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Captures the bridge between classic Arxana operations (scholia, metadata
;; hooks, deletion) and the Futon storage helpers.

;;; Code:

(require 'arxana-store)
(require 'subr-x)

(defvar arxana-article-path-cache (make-hash-table :test 'equal)
  "Cache of canonical paths keyed by article name.")

(defvar arxana-article--table (make-hash-table :test 'equal)
  "In-memory table of article data keyed by article name.")

(defun arxana-article--canonical-path (path)
  (when (and path (fboundp 'futon4--canonical-path))
    (setq path (futon4--canonical-path path)))
  path)

(defun arxana-article--labels-for (_name)
  "Return labels for the article named NAME, when available."
  nil)

(defun arxana-article--metadata-envelope (name)
  "Return a metadata envelope alist for article NAME."
  (let* ((entry (and (fboundp 'metadata-article)
                     (metadata-article name)))
         (meta (and entry (fboundp 'scholium-text)
                    (scholium-text entry)))
         (labels (and (fboundp 'arxana-article--labels-for)
                      (arxana-article--labels-for name))))
    (when meta
      (list (cons 'metadata
                  (append meta
                          (when labels
                            (list (cons 'labels labels)))))))))

(unless (fboundp 'make-current-buffer-into-article)
  (defun make-current-buffer-into-article (name)
    "Register the current buffer as an article named NAME."
    (interactive "sArticle name: ")
    (let ((text (buffer-substring-no-properties (point-min) (point-max))))
      (puthash name (list :name name :text text) arxana-article--table)
      (setq-local name-of-current-article name)
      name)))

(unless (fboundp 'get-article)
  (defun get-article (name)
    "Return the article entry for NAME."
    (gethash name arxana-article--table)))

(unless (fboundp 'sch-plain-text)
  (defun sch-plain-text (article)
    "Return the plain text for ARTICLE."
    (plist-get article :text)))

(unless (fboundp 'turn-article-table-into-names)
  (defun turn-article-table-into-names ()
    "Return a list of registered article names."
    (let (names)
      (maphash (lambda (key _value) (push key names)) arxana-article--table)
      (nreverse names))))

(provide 'arxana-article)

;;; arxana-article.el ends here
