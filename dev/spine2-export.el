;;; spine2-export.el --- Legacy Org export helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Provide repeatable commands for exporting a legacy Org snapshot into both
;; the traditional LaTeX/PDF (with inline code) and a concise variant where
;; code blocks are replaced by `code>` markers that link to source files.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org)
(require 'ob-core)
(require 'ob-tangle)
(require 'ox)
(require 'ox-latex)
(require 'url-util)

(defcustom arxana-spine-export-org-file nil
  "Org file used for legacy exports."
  :type '(choice (const :tag "Unset" nil)
                 file)
  :group 'arxana)

(defvar arxana-spine2-root-directory nil
  "Cached repo root used for legacy exports.")

(defun arxana-spine2--root ()
  "Locate the repository root for legacy exports."
  (or arxana-spine2-root-directory
      (setq arxana-spine2-root-directory
            (or (and (boundp 'arxana-root-directory) arxana-root-directory)
                (locate-dominating-file default-directory "dev")
                default-directory)))))

(defun arxana-spine2--spine-file ()
  "Absolute path to the configured Org export file."
  (let ((org-file arxana-spine-export-org-file))
    (unless org-file
      (error "Org export file not set (customize arxana-spine-export-org-file)"))
    (expand-file-name org-file (arxana-spine2--root))))

(defun arxana-spine2--output-path (name)
  "Return absolute path for export artifact NAME."
  (expand-file-name name (arxana-spine2--root)))

(defun arxana-spine2--ensure-dependencies ()
  "Load the Org export dependencies needed for batch exports."
  (dolist (feature '(org ob-core ob-tangle ox ox-latex org-element))
    (require feature))
  (require 'url-util)
  t)

(defun arxana-spine2--file-url (abs-path)
  "Convert ABS-PATH to a file: URL with percent-encoded segments."
  (let* ((abs (expand-file-name abs-path))
         (components (split-string abs "/" t))
         (encoded (mapconcat #'url-hexify-string components "/"))
         (prefix (if (string-prefix-p "/" abs) "/" "")))
    (concat "file://" prefix encoded)))

(defun arxana-spine2--latex-file-link (relative)
  "Return a LaTeX hyperlink to RELATIVE (from the repo root)."
  (let* ((root (arxana-spine2--root))
         (abs (expand-file-name relative root))
         (url (arxana-spine2--file-url abs))
         (label (file-relative-name abs root)))
    (format "\\href{%s}{\\texttt{\\detokenize{%s}}}" url label)))

(defun arxana-spine2--default-tangle-path (src-block info)
  "Compute the default output path for SRC-BLOCK when it targets the base file."
  (let* ((lang (org-element-property :language src-block))
         (extension (or (cdr (assoc lang org-babel-tangle-lang-exts))
                        lang))
         (input (or (plist-get info :input-file)
                    (arxana-spine2--spine-file))))
    (when (and lang extension input)
      (concat (file-name-sans-extension (file-name-nondirectory input))
              "." extension))))

(defun arxana-spine2--resolve-src-target (src-block info)
  "Return the output target for SRC-BLOCK, or nil if it shouldn't be replaced."
  (let* ((params (org-element-property :parameters src-block))
         (args (when params (org-babel-parse-header-arguments params)))
         (value (cdr (assoc :tangle args))))
    (cond
     ((null value) nil)
     ((memq value '(no nil)) nil)
     ((and (stringp value)
           (member (downcase value) '("no" "nil" "false"))) nil)
     ((or (eq value t)
          (and (stringp value)
               (member (downcase value) '("yes" "t" "true"))))
      (arxana-spine2--default-tangle-path src-block info))
     (t (format "%s" value)))))

(declare-function org-latex-src-block "ox-latex" (src-block contents info))

(defun arxana-spine2--code-link-src-block (src-block contents info)
  "Export SRC-BLOCK as a `code>` link when it targets a file."
  (let ((target (arxana-spine2--resolve-src-target src-block info)))
    (cond
     (target
      (format "\\begin{quote}\n\\texttt{\\detokenize{code>}}~%s\n\\end{quote}\n"
              (arxana-spine2--latex-file-link target)))
     ((arxana-spine2--dev-include-block-p src-block)
      "")
     (t
      (org-latex-src-block src-block contents info)))))

(defun arxana-spine2--dev-include-block-p (src-block)
  "Return non-nil when SRC-BLOCK looks like an expanded dev include."
  (let* ((lang (org-element-property :language src-block))
         (value (org-element-property :value src-block))
         (first-line (when value
                       (car (split-string value "\n" t)))))
    (and (equal lang "emacs-lisp")
         value
         (> (length value) 1000)
         first-line
         (string-match-p "\\`;;; \\(arxana\\|dev\\|bootstrap\\)" first-line))))

(defun arxana-spine2--code-link-before-parsing (backend)
  "Swap dev/*.el includes for short code-link hints when BACKEND matches."
  (when (eq backend 'arxana-latex-code-links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+INCLUDE: \"\\(dev/[^\"]+\\)\"\\s-+src\\b[^\n]*$" nil t)
        (let* ((relative (match-string 1))
               (replacement (format "#+BEGIN_QUOTE\n~code>~ [[file:%s][%s]]\n#+END_QUOTE"
                                    relative relative)))
          (replace-match replacement t t nil 0))))))

(org-export-define-derived-backend 'arxana-latex-code-links 'latex
  :translate-alist '((src-block . arxana-spine2--code-link-src-block)))

(defun arxana-spine2--export (backend output)
  "Export the configured Org file through BACKEND to OUTPUT."
  (arxana-spine2--ensure-dependencies)
  (let* ((spine (arxana-spine2--spine-file))
         (out (arxana-spine2--output-path output))
         (org-export-show-temporary-export-buffer nil)
         (org-export-in-background nil))
    (message "Exporting %s -> %s" spine out)
    (with-current-buffer (find-file-noselect spine)
      (let ((org-export-with-broken-links 'mark)
            (org-export-before-parsing-functions
             (if (eq backend 'arxana-latex-code-links)
                 (cons #'arxana-spine2--code-link-before-parsing
                       org-export-before-parsing-functions)
               org-export-before-parsing-functions)))
        (org-export-to-file backend out)))))

(defun arxana-spine2--default-output-name (suffix)
  (let ((base (file-name-base (arxana-spine2--spine-file))))
    (concat base suffix)))

(defun arxana-spine2-export-standard ()
  "Export the configured Org file using the default LaTeX backend."
  (interactive)
  (arxana-spine2--export 'latex (arxana-spine2--default-output-name ".tex")))

(defun arxana-spine2-export-code-links ()
  "Export the configured Org file using placeholder code links."
  (interactive)
  (arxana-spine2--export 'arxana-latex-code-links
                         (arxana-spine2--default-output-name "-code-links.tex")))

(defun arxana-spine2-export-both ()
  "Generate both the default and the code-link LaTeX files."
  (interactive)
  (arxana-spine2-export-standard)
  (arxana-spine2-export-code-links))

(provide 'spine2-export)

;;; spine2-export.el ends here
