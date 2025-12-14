;;; arxana-inclusion.el --- Guard legacy include/transclude helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Ensures include/transclude commands only run when the referenced article
;; has been registered, preventing silent “nil” insertions.

;;; Code:

(require 'subr-x)

(defun arxana-inclusion--require-article (name caller lookup)
  "Return the article NAME using LOOKUP or raise a helpful CALLER error."
  (let ((article (and name (funcall lookup name))))
    (unless article
      (user-error "%s: unknown article '%s'. Run `make-current-buffer-into-article' in that buffer first."
                 caller name))
    article))

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
