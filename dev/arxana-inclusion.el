;;; arxana-inclusion.el --- Guard legacy include/transclude helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; The legacy Part VIII commands assume every article name resolves via
;; `get-article`.  Without a prior `make-current-buffer-into-article`, those
;; lookups return nil and the commands silently insert the string "nil".  This
;; shim intercepts the helpers so users get an actionable error instead of
;; mangled buffers.

;;; Code:

(require 'subr-x)

(defun arxana-inclusion--require-article (name caller lookup)
  "Return the article NAME using LOOKUP or raise a helpful CALLER error."
  (let ((article (and name (funcall lookup name))))
    (unless article
      (user-error "%s: unknown article '%s'. Run `make-current-buffer-into-article' in that buffer first."
                 caller name))
    article))

(defun arxana-inclusion--wrap (fn caller)
  "Return a wrapper for FN that validates article NAME via CALLER message."
  (lambda (&rest args)
    (let* ((article (car args))
           (orig-get (symbol-function 'get-article))
           (source (arxana-inclusion--require-article article caller orig-get)))
      (cl-letf* (((symbol-function 'get-article)
                  (lambda (name)
                    (if (equal name article)
                        source
                      (funcall orig-get name)))))
        (apply fn args)))))

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
