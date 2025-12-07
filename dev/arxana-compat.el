;;; arxana-compat.el --- Modern helpers for legacy accessors -*- lexical-binding: t; -*-

;;; Commentary:
;; Some parts of the tumbled code still redefine `get-article' to walk the
;; historical "nema" structures, effectively ignoring the hash table that
;; `make-current-buffer-into-article' populates.  This shim keeps the modern
;; article-table path working by interposing a `get-article' that first checks
;; the in-memory hash before falling back to the legacy implementation.

;;; Code:

(defvar arxana-compat--orig-get-article nil
  "Original `get-article' captured before installing the compat shim.")

(defun arxana-compat--hash-article (name)
  "Return (NAME . VALUE) from `article-table' when available."
  (when (and (boundp 'article-table)
             (hash-table-p article-table)
             name)
    (let ((value (gethash name article-table)))
      (when value
        (cons name value)))))

(defun arxana-compat--get-article (name)
  "Prefer the modern article table before hitting the legacy fallback."
  (or (arxana-compat--hash-article name)
      (when (functionp arxana-compat--orig-get-article)
        (funcall arxana-compat--orig-get-article name))))

(with-eval-after-load 'arxana-tangled
  (unless arxana-compat--orig-get-article
    (setq arxana-compat--orig-get-article (symbol-function 'get-article))
    (fset 'get-article #'arxana-compat--get-article)))

(provide 'arxana-compat)

;;; arxana-compat.el ends here
