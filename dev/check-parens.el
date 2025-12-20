;;; check-parens.el --- Batch paren validator for Lisp/Clojure files -*- lexical-binding: t; -*-

;;; Commentary:
;; Run `check-parens` over one or more files and report mismatches with
;; filename/line/column. Designed for batch use:
;;
;;   emacs -Q --batch -l dev/check-parens.el --eval "(arxana-check-parens-cli)"
;;
;; Exit code is non-zero on the first failure. If no files are provided on the
;; command line, defaults to all *.el under dev/ and test/.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun arxana-check-parens--pos->linecol (pos)
  "Return (LINE . COL) for buffer position POS (1-based line/col)."
  (save-excursion
    (goto-char pos)
    (cons (line-number-at-pos)
          (1+ (current-column)))))

(defun arxana-check-parens--check-buffer (file)
  "Run `check-parens` in FILE. Return nil if OK, or plist with details."
  (with-temp-buffer
    (insert-file-contents file)
    ;; Choose a lisp-y syntax table so check-parens respects strings/comments.
    (let ((buffer-file-name file))
      (emacs-lisp-mode))
    (condition-case err
        (progn
          (check-parens)
          nil)
      (error
       (let* ((data (cdr err))
              (pos (or (nth 2 data) (point)))
              (lc (arxana-check-parens--pos->linecol pos)))
         (list :file file
               :pos pos
               :line (car lc)
               :col (cdr lc)
               :message (error-message-string err)))))))

(defun arxana-check-parens--file-list ()
  "Return list of files to check from `command-line-args-left` or defaults."
  (let ((files '())
        (seen-sep nil))
    (dolist (arg command-line-args-left)
      (cond
       ((string= arg "--") (setq seen-sep t))
       ((or seen-sep (not (string-prefix-p "-" arg)))
        (push arg files))))
    (setq files (nreverse files))
    (if files
        files
      (cl-remove-if-not
       (lambda (f) (string-match-p "\\.el\\'" f))
       (append (directory-files "dev" t "\\.el\\'")
               (directory-files "test" t "\\.el\\'"))))))

(defun arxana-check-parens-run (&optional files)
  "Check FILES (list of paths). Return nil on success, or plist on first error."
  (let ((targets (or files (arxana-check-parens--file-list)))
        (problem nil))
    (dolist (file targets)
      (unless problem
        (setq problem (arxana-check-parens--check-buffer file))))
    problem))

(defun arxana-check-parens-cli ()
  "Batch entry point. Checks files and prints a concise report."
  (let ((problem (arxana-check-parens-run)))
    (if (not problem)
        (progn
          (princ "OK\n")
          (kill-emacs 0))
      (princ (format "%s:%d:%d: %s\n"
                     (plist-get problem :file)
                     (plist-get problem :line)
                     (plist-get problem :col)
                     (plist-get problem :message)))
      (kill-emacs 1))))

(provide 'check-parens)

;;; check-parens.el ends here
