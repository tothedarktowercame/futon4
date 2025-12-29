;;; arxana-docbook-export.el --- Docbook export helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Export docbook content to Org/PDF and normalize include paths.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'org)

(declare-function arxana-docbook--repo-root "arxana-docbook-core")
(declare-function arxana-docbook--available-books "arxana-docbook-core")
(declare-function arxana-docbook--entries-for-doc "arxana-docbook-core" (book doc-id))
(declare-function arxana-docbook--latest-non-lab "arxana-docbook-core" (entries))
(declare-function arxana-docbook--entry-content "arxana-docbook-core" (entry))
(declare-function arxana-docbook--source-link-line "arxana-docbook-core" (entry))
(declare-function arxana-docbook--heading-title "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--heading-level "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--toc-doc-id "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--toc-for-view "arxana-docbook-toc" (book))
(declare-function arxana-docbook--order-headings "arxana-docbook-toc" (headings order))
(declare-function arxana-docbook--first-descendant-entry "arxana-docbook-ui" (book heading))

(defcustom arxana-docbook-org-export-directory nil
  "Directory for docbook Org exports (nil uses dev/logs/books/<book>/export)."
  :type '(choice (const :tag "Auto" nil)
                 directory)
  :group 'arxana-docbook)
(defun arxana-docbook--demote-org (text)
  (replace-regexp-in-string "^\\(\\*+\\) " "*\\1 " (or text "")))
(defun arxana-docbook--demote-org-by (text steps)
  (let ((out (or text "")))
    (dotimes (_ (max 0 steps))
      (setq out (arxana-docbook--demote-org out)))
    out))
(defun arxana-docbook--export-context ()
  "Return plist with :host-root and :book inferred from the current buffer file."
  (when buffer-file-name
    (when (string-match "\\(.*\\)/dev/logs/books/\\([^/]+\\)/" buffer-file-name)
      (list :host-root (match-string 1 buffer-file-name)
            :book (match-string 2 buffer-file-name)))))
(defun arxana-docbook--resolve-include-path (path base-dir)
  "Resolve PATH for #+INCLUDE relative to BASE-DIR and known repo roots."
  (let* ((base-dir (or base-dir default-directory))
         (repo-root (file-name-as-directory (arxana-docbook--repo-root)))
         (ctx (arxana-docbook--export-context))
         (host-root (plist-get ctx :host-root))
         (book (plist-get ctx :book))
         (candidates
          (delq nil
                (list
                 (when (file-name-absolute-p path) path)
                 (expand-file-name path base-dir)
                 (and (string-prefix-p "dev/" path)
                      (expand-file-name path repo-root))
                 (and host-root book (string-prefix-p "dev/" path)
                      (expand-file-name path
                                        (file-name-as-directory
                                         (expand-file-name book host-root))))))))
    (or (seq-find #'file-readable-p candidates)
        path)))
(defun arxana-docbook--rewrite-include-paths (text base-dir)
  "Rewrite relative Org include paths in TEXT to absolute paths under BASE-DIR."
  (replace-regexp-in-string
   "^#\\+INCLUDE:[ \t]+\"\\([^\"]+\\)\"\\(.*\\)$"
   (lambda (line)
     (if (string-match "^#\\+INCLUDE:[ \t]+\"\\([^\"]+\\)\"\\(.*\\)$" line)
         (let* ((path (match-string 1 line))
                (rest (match-string 2 line))
                (abs (arxana-docbook--resolve-include-path path base-dir)))
           (format "#+INCLUDE: \"%s\"%s" abs rest))
       line))
   (or text "") t t))
(defun arxana-docbook--export-heading (book heading)
  (let* ((doc-id (arxana-docbook--toc-doc-id heading))
         (title (arxana-docbook--heading-title heading))
         (level (max 1 (or (arxana-docbook--heading-level heading) 1)))
         (stars (make-string level ?*))
         (entries (and doc-id (arxana-docbook--entries-for-doc book doc-id)))
         (base-entry (and entries (arxana-docbook--latest-non-lab entries)))
         (content (and base-entry (arxana-docbook--entry-content base-entry)))
         (trimmed (string-trim (or content "")))
         (source-line (and base-entry (arxana-docbook--source-link-line base-entry))))
    (insert (format "%s %s\n" stars (or title doc-id "")))
    (if (and trimmed (not (string-empty-p trimmed)))
        (progn
          (when source-line
            (insert source-line))
          (let* ((root (arxana-docbook--repo-root))
                 (content (arxana-docbook--rewrite-include-paths content root)))
            (insert (arxana-docbook--demote-org-by content 1) "\n")))
      (if-let* ((desc (arxana-docbook--first-descendant-entry book heading))
                (child-title (arxana-docbook--heading-title (plist-get desc :heading)))
                (child-content (plist-get desc :content))
                (child-entry (plist-get desc :entry))
                (child-source (arxana-docbook--source-link-line child-entry)))
          (progn
            (insert (format "%s %s\n" (make-string (1+ level) ?*) child-title))
            (when child-source
              (insert child-source))
            (let* ((root (arxana-docbook--repo-root))
                   (child-content (arxana-docbook--rewrite-include-paths child-content root)))
              (insert (arxana-docbook--demote-org-by child-content 2) "\n")))
        (insert "- (no entry content yet)\n")))
    (insert "\n")))
(defun arxana-docbook-export-org-book (&optional book output-file order)
  "Export docbook BOOK into a single Org file at OUTPUT-FILE.
ORDER, when provided, reorders TOC headings by doc-id."
  (interactive)
  (let* ((book (or book arxana-docbook--book
                   (car (arxana-docbook--available-books))
                   "futon4"))
         (default (arxana-docbook--default-org-export-path book))
         (output-file (or output-file
                          (read-file-name "Export Org file: "
                                          (file-name-directory default)
                                          default nil
                                          (file-name-nondirectory default)))))
    (let* ((toc (arxana-docbook--toc-for-view book))
           (headings (if (and order (listp order))
                         (arxana-docbook--order-headings toc order)
                       toc)))
      (with-temp-buffer
        (insert (format "#+TITLE: Docbook %s\n" book))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))
        (dolist (heading headings)
          (when heading
            (arxana-docbook--export-heading book heading)))
        (write-region (point-min) (point-max) output-file))
      (message "Wrote docbook Org export to %s" output-file)
      output-file)))
(defun arxana-docbook--export-org-to-pdf (org-file &optional pdf-file)
  "Export ORG-FILE to PDF, returning the PDF path."
  (require 'ox-latex)
  (let* ((org-file (expand-file-name org-file))
         (default-directory (file-name-directory org-file))
         (pdf-file (or pdf-file (concat (file-name-sans-extension org-file) ".pdf")))
         (org-export-show-temporary-export-buffer nil)
         (org-export-in-background nil)
         (org-export-with-broken-links 'mark))
    (with-current-buffer (find-file-noselect org-file)
      (let ((org-export-before-parsing-functions
             (cons #'arxana-docbook--normalize-include-paths
                   org-export-before-parsing-functions))
            (exported (org-latex-export-to-pdf)))
        (unless exported
          (error "Org LaTeX export did not produce a PDF"))
        (setq exported (expand-file-name exported))
        (setq pdf-file (expand-file-name pdf-file))
        (unless (string= exported pdf-file)
          (copy-file exported pdf-file t))
        pdf-file))))
(defun arxana-docbook--normalize-include-paths (&optional backend)
  "Rewrite #+INCLUDE paths to absolute paths when possible."
  (ignore backend)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+INCLUDE: \"\\([^\"]+\\)\"" nil t)
      (let* ((raw (match-string 1))
             (resolved (arxana-docbook--resolve-include-path raw default-directory)))
        (when (and resolved (not (string= raw resolved)))
          (replace-match resolved t t nil 1))))))
(defun arxana-docbook-export-pdf-book (&optional book org-file pdf-file order)
  "Export docbook BOOK to Org (ORG-FILE) and then to PDF (PDF-FILE).
ORDER, when provided, reorders TOC headings by doc-id."
  (interactive)
  (let* ((book (or book arxana-docbook--book
                   (car (arxana-docbook--available-books))
                   "futon4"))
         (org-file (or org-file (arxana-docbook--default-org-export-path book)))
         (pdf-file (or pdf-file (concat (file-name-sans-extension org-file) ".pdf")))
         (org-path (arxana-docbook-export-org-book book org-file order)))
    (arxana-docbook--export-org-to-pdf org-path pdf-file)
    (message "Wrote docbook PDF export to %s" pdf-file)
    pdf-file))
(defun arxana-docbook--export-dir (book)
  (or arxana-docbook-org-export-directory
      (let* ((root (arxana-docbook--locate-books-root))
             (dir (and root (expand-file-name book root))))
        (when dir
          (setq dir (expand-file-name "export" dir))
          (make-directory dir t)
          dir))))
(defun arxana-docbook--default-org-export-path (book)
  (let ((dir (arxana-docbook--export-dir book)))
    (if dir
        (expand-file-name (format "%s.org" book) dir)
      (expand-file-name (format "docbook-%s.org" book)
                        (arxana-docbook--repo-root)))))

(provide 'arxana-docbook-export)
;;; arxana-docbook-export.el ends here
