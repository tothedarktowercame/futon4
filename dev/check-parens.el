;;; check-parens.el --- Batch paren validator for Lisp/Clojure files -*- lexical-binding: t; -*-

;;; Commentary:
;; Run `check-parens` over one or more files and report mismatches with
;; filename/line/column. Designed for batch use:
;;
;;   emacs -Q --batch -l dev/check-parens.el --eval "(arxana-check-parens-cli)"
;;
;; Enhancements over a plain `check-parens` run:
;; - On failure, can optionally run a form-by-form `read` scan to identify the
;;   start of the first unreadable form (often closer to the true cause).
;; - Optional JSON output, and optional context lines.
;;
;; CLI args (pass after `--`):
;;   --strategy {check-parens|read|both}   (default: both)
;;   --json                                Output JSON
;;   --context N                           Include N lines of context (default: 0)
;;   --no-defaults                         Don't default to dev/*.el and test/*.el
;;   FILES...                              Files to check

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; json-serialize is available in Emacs 27+. If unavailable, we fall back to text.
(defvar arxana-check-parens--json-available (fboundp 'json-serialize))

(defun arxana-check-parens--pos->linecol (pos)
  "Return (LINE . COL) for buffer position POS (1-based line/col)."
  (save-excursion
    (goto-char (max (point-min) (min (point-max) pos)))
    (cons (line-number-at-pos)
          (1+ (current-column)))))

(defun arxana-check-parens--linecol->pos (line col)
  "Best-effort convert LINE and COL (1-based) to a buffer position."
  (save-excursion
    (goto-char (point-min))
    (forward-line (max 0 (1- line)))
    (forward-char (max 0 (1- col)))
    (point)))

(defun arxana-check-parens--mode-for-file (file)
  "Return a major mode function suitable for FILE."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (cond
     ((string= ext "el") #'emacs-lisp-mode)
     ;; For clojure-ish files, we may not have clojure-mode available in -Q.
     ;; Fundamental-mode still uses a reasonable syntax table for parens, but
     ;; won't know all comment forms. We keep it conservative.
     ((member ext '("clj" "cljs" "cljc" "edn")) #'lisp-mode)
     ;; Default: Emacs Lisp mode is safest for this script’s use case.
     (t #'emacs-lisp-mode))))

(defun arxana-check-parens--context-snippet (pos &optional nlines)
  "Return a small context snippet around POS: NLINES before and after.
If NLINES is nil or 0, return nil."
  (let ((n (or nlines 0)))
    (when (> n 0)
      (save-excursion
        (goto-char (max (point-min) (min (point-max) pos)))
        (let* ((center-line (line-number-at-pos))
               (start-line (max 1 (- center-line n)))
               (end-line (+ center-line n))
               (start-pos (arxana-check-parens--linecol->pos start-line 1))
               (end-pos (save-excursion
                          (goto-char (point-min))
                          (forward-line (max 0 (1- end-line)))
                          (line-end-position)))
               (txt (buffer-substring-no-properties start-pos (min (point-max) end-pos))))
          ;; Prefix with line numbers to help tools.
          (with-temp-buffer
            (let ((ln start-line))
              (dolist (line (split-string txt "\n" nil))
                (insert (format "%6d  %s\n" ln line))
                (setq ln (1+ ln))))
            (buffer-string)))))))

(defun arxana-check-parens--check-parens (file &optional context-lines)
  "Run `check-parens` in FILE. Return nil if OK, or plist with details."
  (with-temp-buffer
    (insert-file-contents file)
    ;; Ensure buffer-file-name is set so mode hooks behave.
    (let ((buffer-file-name file))
      (funcall (arxana-check-parens--mode-for-file file)))
    (condition-case err
        (progn (check-parens) nil)
      (error
       ;; `check-parens` error data is inconsistent across Emacs versions;
       ;; prefer current point, but keep any embedded position if present.
       (let* ((data (cdr err))
              (maybe-pos (and (listp data) (nth 2 data)))
              (pos (or (and (integerp maybe-pos) maybe-pos) (point)))
              (lc (arxana-check-parens--pos->linecol pos)))
         (list :kind "check-parens"
               :file file
               :pos pos
               :line (car lc)
               :col (cdr lc)
               :message (error-message-string err)
               :context (arxana-check-parens--context-snippet pos context-lines)))))))

(defun arxana-check-parens--read-scan (file &optional context-lines)
  "Scan FILE by repeatedly calling `read` to locate the first unreadable form.
Return nil if the file is fully readable, or a plist with details on failure."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (funcall (arxana-check-parens--mode-for-file file)))
    (goto-char (point-min))
    (let ((form-index 0)
          (last-form-start (point-min)))
      (condition-case err
          (while t
            (setq last-form-start (point))
            (read (current-buffer))
            (setq form-index (1+ form-index)))
        (end-of-file
         ;; Entire file read successfully.
         nil)
        (error
         ;; On error, point is often where the reader noticed the problem,
         ;; while last-form-start is the start of the form that failed.
         (let* ((err-pos (point))
                (start-pos last-form-start)
                (start-lc (arxana-check-parens--pos->linecol start-pos))
                (err-lc (arxana-check-parens--pos->linecol err-pos)))
           (list :kind "read"
                 :file file
                 :form-index form-index
                 :form-start-pos start-pos
                 :form-start-line (car start-lc)
                 :form-start-col (cdr start-lc)
                 :error-pos err-pos
                 :error-line (car err-lc)
                 :error-col (cdr err-lc)
                 :message (error-message-string err)
                 :context (arxana-check-parens--context-snippet start-pos context-lines))))))))

(defun arxana-check-parens--default-files ()
  "Default files to check: all *.el under dev/ and test/."
  (cl-remove-if-not
   (lambda (f) (string-match-p "\\.el\\'" f))
   (append (when (file-directory-p "dev")
             (directory-files "dev" t "\\.el\\'"))
           (when (file-directory-p "test")
             (directory-files "test" t "\\.el\\'")))))

(defun arxana-check-parens--parse-cli-args ()
  "Parse args after `--`. Return plist:
  :files (list) :strategy (string) :json (bool) :context (int) :no-defaults (bool)."
  (let ((args command-line-args-left)
        (seen-sep nil)
        (files '())
        (strategy "both")
        (json-out nil)
        (context-lines 0)
        (no-defaults nil))
    (while args
      (let ((a (pop args)))
        (cond
         ((string= a "--") (setq seen-sep t))
         ((not seen-sep)
          ;; Ignore args before `--` (emacs itself may put stuff here).
          nil)
         ((string= a "--json") (setq json-out t))
         ((string= a "--no-defaults") (setq no-defaults t))
         ((string-prefix-p "--strategy" a)
          (let ((v (if (string-match-p "=" a)
                       (cadr (split-string a "="))
                     (pop args))))
            (when v (setq strategy v))))
         ((string-prefix-p "--context" a)
          (let ((v (if (string-match-p "=" a)
                       (cadr (split-string a "="))
                     (pop args))))
            (when v (setq context-lines (max 0 (string-to-number v))))))
         ;; Anything else that doesn't look like a flag is a file.
         ((or (string-prefix-p "-" a) (string-empty-p a))
          ;; Unknown flag: ignore rather than fail hard in batch.
          nil)
         (t
          (push a files)))))
    (setq files (nreverse files))
    (list :files files
          :strategy strategy
          :json json-out
          :context context-lines
          :no-defaults no-defaults)))

(defun arxana-check-parens-run (&optional files strategy context-lines no-defaults)
  "Check FILES (list of paths). Return nil on success, or plist on first error.
STRATEGY is one of \"check-parens\", \"read\", or \"both\" (default \"both\")."
  (let* ((targets (cond
                   (files files)
                   (no-defaults nil)
                   (t (arxana-check-parens--default-files))))
         (mode (or strategy "both"))
         (ctx (or context-lines 0))
         (problem nil))
    (dolist (file targets)
      (unless problem
        (cond
         ((string= mode "check-parens")
          (setq problem (arxana-check-parens--check-parens file ctx)))
         ((string= mode "read")
          (setq problem (arxana-check-parens--read-scan file ctx)))
         (t
          ;; both: prefer check-parens as the primary validator; enrich with read-scan.
          (let ((cp (arxana-check-parens--check-parens file ctx)))
            (if (not cp)
                (setq problem nil)
              (let ((rs (arxana-check-parens--read-scan file ctx)))
                ;; Merge read-scan info if present; keep check-parens as primary kind/message.
                (setq problem (if rs
                                  (append cp (list :read-scan rs))
                                cp)))))))))
    problem))

(defun arxana-check-parens--print-problem (problem json-out)
  "Print PROBLEM in either text or JSON form."
  (cond
   ((and json-out arxana-check-parens--json-available)
    (princ (json-serialize problem))
    (princ "\n"))
   (t
    ;; Text format: always emit a grep-friendly primary line.
    (let ((file (plist-get problem :file))
          (line (plist-get problem :line))
          (col (plist-get problem :col))
          (msg (plist-get problem :message)))
      (when (and file line col msg)
        (princ (format "%s:%d:%d: %s\n" file line col msg)))
      ;; If we have read-scan augmentation, emit a second line.
      (let ((rs (plist-get problem :read-scan)))
        (when (and rs (listp rs))
          (princ (format "%s:%d:%d: read-scan: form #%d starts here; reader error at %d:%d: %s\n"
                         (plist-get rs :file)
                         (plist-get rs :form-start-line)
                         (plist-get rs :form-start-col)
                         (plist-get rs :form-index)
                         (plist-get rs :error-line)
                         (plist-get rs :error-col)
                         (plist-get rs :message)))))
      ;; Context (already line-numbered).
      (let ((ctx (plist-get problem :context)))
        (when (and ctx (stringp ctx) (not (string-empty-p ctx)))
          (princ ctx)))
      ;; If read-scan has its own context, print it too (usually same, but keep explicit).
      (let* ((rs (plist-get problem :read-scan))
             (rsctx (and (listp rs) (plist-get rs :context))))
        (when (and rsctx (stringp rsctx) (not (string-empty-p rsctx)))
          (princ rsctx)))))))

(defun arxana-check-parens-cli ()
  "Batch entry point. Checks files and prints a concise report."
  (let* ((opts (arxana-check-parens--parse-cli-args))
         (files (plist-get opts :files))
         (strategy (plist-get opts :strategy))
         (json-out (plist-get opts :json))
         (ctx (plist-get opts :context))
         (no-defaults (plist-get opts :no-defaults))
         (targets (or files (and (not no-defaults) (arxana-check-parens--default-files)))))
    (if (not targets)
        (progn
          (princ "No target files.\n")
          (kill-emacs 2))
      (let ((problem (arxana-check-parens-run targets strategy ctx no-defaults)))
        (if (not problem)
            (progn (princ "OK\n") (kill-emacs 0))
          (arxana-check-parens--print-problem problem json-out)
          (kill-emacs 1))))))

(provide 'check-parens)

;;; check-parens.el ends here
