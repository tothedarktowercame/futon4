;;; arxana-flexiarg-normalize.el --- Flexiarg template normalizer  -*- lexical-binding: t; -*-

;;; Commentary:
;; Utilities for rewriting older flexiarg files into the canonical
;; "! conclusion" + "+ CLAUSE" template referenced by
;; library-coherence/library-template-discipline.  These helpers only
;; touch bodies that still use the simplified IF/HOWEVER/THEN/BECAUSE
;; stanza so existing canonical files remain untouched.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst arxana-flexiarg--clause-regexp
  "^\\s-*\\([A-Z][A-Z0-9 /-]+\\):\\s-*\\(.*\\)$"
  "Regexp that matches simplified clause headings like `IF:' or `NEXT STEPS:'.")

(defun arxana-flexiarg--body-start ()
  "Return buffer position where the clause body begins."
  (save-excursion
    (goto-char (point-min))
    (while (and (not (eobp))
                (or (looking-at "^\\s-*$")
                    (looking-at "^@")
                    (looking-at "^;;")))
      (forward-line 1))
    (point)))

(defun arxana-flexiarg--already-normalized-p (body)
  "Return non-nil when BODY already uses the canonical template."
  (string-match-p "^!\\s-*conclusion" (string-trim-left body)))

(defun arxana-flexiarg--replace-alias-headings ()
  "Rewrite historical clause headers (e.g., `! instantiated-by:`) to canonical ones.
Returns non-nil when any replacements were made."
  (let ((changed nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^!\\s-*instantiated-by:" nil t)
        (replace-match "! conclusion:" nil nil)
        (setq changed t)))
    changed))

(defun arxana-flexiarg--extract-clauses (body)
  "Parse BODY and return an ordered list of (LABEL . TEXT) clauses."
  (let* ((lines (split-string body "\n"))
         (current nil)
         (label nil)
         (result '()))
    (cl-labels ((flush ()
                  (when label
                    (let ((block (string-trim-right (string-join (nreverse current) "\n"))))
                      (push (cons (upcase label) block) result)
                      (setq current nil label nil)))))
      (dolist (line lines)
        (if (string-match arxana-flexiarg--clause-regexp line)
            (progn
              (flush)
              (setq label (match-string 1 line)
                    current (let ((rest (match-string 2 line)))
                              (unless (string-empty-p rest)
                                (list rest)))))
          (when label
            (push line current))))
      (flush)
      (nreverse result))))

(defun arxana-flexiarg--indent-lines (text indent)
  "Indent TEXT by INDENT spaces while preserving blank lines."
  (let* ((trimmed (string-trim-right text))
         (padding (make-string indent ?\s))
         (lines (split-string trimmed "\n" nil)))
    (if (string-empty-p (string-trim trimmed))
        ""
      (mapconcat (lambda (line)
                   (if (string-empty-p line)
                       ""
                     (concat padding line)))
                 lines
                 "\n"))))

(defun arxana-flexiarg--format-clause (label text)
  "Return the canonical rendering for LABEL with TEXT."
  (format "  + %s:\n%s" (upcase label)
          (let ((indented (arxana-flexiarg--indent-lines text 4)))
            (if (string-empty-p indented)
                "    [pending]\n"
              indented))))

(defun arxana-flexiarg--build-body (clauses)
  "Return canonical body text built from CLAUSES."
  (let* ((summary (or (cdr (assoc "THEN" clauses))
                      (cdr (assoc "CONCLUSION" clauses))
                      (cdr (car clauses))
                      ""))
         (formatted (mapcar (lambda (entry)
                              (arxana-flexiarg--format-clause (car entry) (cdr entry)))
                            clauses)))
    (string-trim-right
     (concat "! conclusion:\n"
             (if (string-empty-p (string-trim summary))
                 "  [pending]\n"
               (arxana-flexiarg--indent-lines summary 2))
             "\n\n"
             (string-join formatted "\n\n")
             "\n"))))

(defun arxana-flexiarg--normalize-buffer ()
  "Normalize the current buffer if it still uses the simple clause format.
Return non-nil when modifications were made."
  (let* ((body-start (arxana-flexiarg--body-start))
         (header (buffer-substring-no-properties (point-min) body-start))
         (body (buffer-substring-no-properties body-start (point-max))))
    (if (arxana-flexiarg--already-normalized-p body)
        nil
      (let ((alias-changed (arxana-flexiarg--replace-alias-headings)))
        (let ((clauses (arxana-flexiarg--extract-clauses body)))
          (cond
           (clauses
            (let ((new-body (arxana-flexiarg--build-body clauses)))
              (erase-buffer)
              (let ((trimmed-header (string-trim-right header)))
                (insert trimmed-header)
                (unless (string-empty-p trimmed-header)
                  (insert "\n\n")))
              (insert new-body)
              (unless (bolp) (insert "\n"))
              t))
           (alias-changed
            alias-changed)
           (t
            (user-error "No clause markers found in this flexiarg"))))))))

;;;###autoload
(defun arxana-flexiarg-normalize-file (file &optional dry-run)
  "Normalize the flexiarg FILE in place.
With prefix DRY-RUN, only report what would change."
  (interactive "fFlexiarg file: \nP")
  (let ((status (with-temp-buffer
                  (insert-file-contents file)
                  (when (arxana-flexiarg--normalize-buffer)
                    (unless dry-run
                      (write-region (point-min) (point-max) file))
                    t))))
    (if status
        (progn
          (message "Normalized %s" file)
          t)
      (message "Already canonical: %s" file)
      nil)))

;;;###autoload
(defun arxana-flexiarg-normalize-directory (directory &optional dry-run)
  "Normalize every `.flexiarg' under DIRECTORY.
With DRY-RUN prefix, just report which files need changes."
  (interactive "DNormalize flexiargs under directory: \nP")
  (let* ((files (directory-files-recursively directory "\\.flexiarg\\'"))
         (touched 0)
         (visited 0)
         (skipped '()))
    (dolist (file files)
      (cl-incf visited)
      (condition-case err
          (when (arxana-flexiarg-normalize-file file dry-run)
            (cl-incf touched))
        (user-error
         (push (format "%s (%s)" file (cadr err)) skipped))))
    (message "Processed %d flexiargs (%d normalized)%s%s"
             visited touched
             (if dry-run " [dry-run]" "")
             (if skipped
                 (format "; skipped %d files (see *Messages*)" (length skipped))
               ""))
    (dolist (note (nreverse skipped))
      (message "[flexiarg-normalize] skipped %s" note))))

(provide 'arxana-flexiarg-normalize)

;;; arxana-flexiarg-normalize.el ends here
