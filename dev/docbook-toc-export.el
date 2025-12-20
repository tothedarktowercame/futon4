;;; docbook-toc-export.el --- Export spine2.org TOC for doc book browsing  -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch-friendly helper to emit a JSON table of contents for spine2.org. The
;; output lives under dev/logs/books/<book>/toc.json and is consumed by the
;; doc book browser to mirror the outline.
;;
;; Usage:
;;   emacs --batch -l dev/docbook-toc-export.el --eval "(arxana-docbook-export-toc)"

;;; Code:

(require 'org)
(require 'org-element)
(require 'json)
(require 'subr-x)

(defun arxana-docbook--slug (text)
  (when text
    (let* ((raw (downcase (replace-regexp-in-string "[^[:alnum:]]+" "-" text))))
      (replace-regexp-in-string "-+" "-" (string-trim raw "-")))))

(defun arxana-docbook--doc-id (book outline-path)
  (let* ((path-str (if (listp outline-path)
                       (string-join outline-path "/")
                     (or outline-path "")))
         (digest (secure-hash 'sha1 (format "%s::%s" book path-str))))
    (format "%s-%s" book (substring digest 0 12))))

(defun arxana-docbook--heading-outline (element)
  (let (titles node)
    (setq node element)
    (while (and node (eq (org-element-type node) 'headline))
      (push (org-no-properties (org-element-property :raw-value node)) titles)
      (setq node (org-element-property :parent node)))
    (nreverse titles)))

(defun arxana-docbook-export-toc (&optional book org-file output-file)
  "Export a TOC JSON for BOOK (default: futon4) from ORG-FILE (default: spine2.org).
Write to OUTPUT-FILE (default: dev/logs/books/BOOK/toc.json)."
  (let* ((book (or book "futon4"))
         (root (or (locate-dominating-file default-directory "spine2.org")
                   (error "Cannot locate spine2.org from %s" default-directory)))
         (org-file (expand-file-name (or org-file "spine2.org") root))
         (books-root (expand-file-name "dev/logs/books" root))
         (output-dir (expand-file-name book books-root))
         (output-file (expand-file-name (or output-file "toc.json") output-dir)))
    (unless (file-readable-p org-file)
      (error "Org file not readable: %s" org-file))
    (with-temp-buffer
      (insert-file-contents org-file)
      (org-mode)
      (let* ((ast (org-element-parse-buffer))
             (headings
              (org-element-map ast 'headline
                (lambda (h)
                  (let* ((outline (arxana-docbook--heading-outline h))
                         (path-str (string-join outline " / "))
                         (level (org-element-property :level h)))
                    (list (cons "doc_id" (arxana-docbook--doc-id book outline))
                          (cons "title" (car (last outline)))
                          (cons "outline_path" outline)
                          (cons "path_string" path-str)
                          (cons "level" level)))))))
        (make-directory output-dir t)
        (with-temp-file output-file
          (insert (json-encode headings))
          (insert "\n"))
        (message "[arxana-docbook] Wrote %d headings to %s"
                 (length headings) output-file)))))

(when noninteractive
  (arxana-docbook-export-toc))

(provide 'docbook-toc-export)

;;; docbook-toc-export.el ends here
