;;; arxana-inclusion.el --- Guard legacy include/transclude helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Ensures include/transclude commands only run when the referenced article
;; has been registered, preventing silent “nil” insertions.

;;; Code:

(require 'subr-x)
(require 'arxana-article nil t)

(defun arxana-inclusion--require-article (name caller lookup)
  "Return the article NAME using LOOKUP or raise a helpful CALLER error."
  (let ((article (and name (funcall lookup name))))
    (unless article
      (user-error "%s: unknown article '%s'. Run `make-current-buffer-into-article' in that buffer first."
                 caller name))
    article))

(defun arxana-inclusion--wrap (orig caller)
  "Return a wrapper for ORIG that checks existence before inserting."
  (lambda (name &rest args)
    (when (fboundp 'get-article)
      (arxana-inclusion--require-article name caller #'get-article))
    (apply orig name args)))

(unless (fboundp 'include-article)
  (defun include-article (name)
    "Insert the plain text of article NAME into the current buffer."
    (interactive "sInclude article: ")
    (let ((article (arxana-inclusion--require-article name "include-article" #'get-article)))
      (when (fboundp 'sch-plain-text)
        (insert (sch-plain-text article)))
      article)))

(unless (fboundp 'transclude-article)
  (defun transclude-article (name)
    "Insert the transcluded text of article NAME into the current buffer."
    (interactive "sTransclude article: ")
    (let* ((article (arxana-inclusion--require-article name "transclude-article" #'get-article))
           (target (and (boundp 'name-of-current-article) name-of-current-article)))
      (when target
        (arxana-inclusion--require-article target "transclude-article" #'get-article))
      (when (fboundp 'sch-plain-text)
        (insert (sch-plain-text article)))
      article)))

(with-eval-after-load 'arxana-tangled
  (when (fboundp 'include-article)
    (let ((orig (symbol-function 'include-article)))
      (fset 'include-article
            (arxana-inclusion--wrap orig "include-article"))))
  (when (fboundp 'transclude-article)
    (let ((orig (symbol-function 'transclude-article)))
      (fset 'transclude-article
            (arxana-inclusion--wrap orig "transclude-article")))))

(provide 'arxana-inclusion)

;;; arxana-inclusion.el ends here
