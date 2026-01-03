;;; docbook-toc-export.el --- Export docbook TOC for browsing  -*- lexical-binding: t; -*-

;;; Commentary:
;; Batch-friendly helper to emit a JSON table of contents for a filesystem
;; docbook snapshot. The output lives under docs/docbook/<book>/toc.json and
;; is consumed by the doc book browser to mirror the outline.
;;
;; Usage:
;;   emacs --batch -l dev/docbook-toc-export.el \
;;     --eval "(arxana-docbook-export-toc \"futon4\" \"docs/docbook/futon4/index.org\")"

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

(defun arxana-docbook--repo-root ()
  "Return the repository root for docbook exports."
  (or (and (boundp 'arxana-root-directory) arxana-root-directory)
      (locate-dominating-file default-directory "dev")
      default-directory))

(defun arxana-docbook--default-books-root (root)
  (let ((working-root (expand-file-name "docs/docbook" root)))
    (if (file-directory-p working-root)
        working-root
      working-root)))

(defun arxana-docbook--default-org-file (book root)
  (let ((working (expand-file-name (format "docs/docbook/%s/index.org" book) root)))
    (if (file-readable-p working)
        working
      nil)))

(defun arxana-docbook-export-toc (&optional book org-file output-file)
  "Export a TOC JSON for BOOK (default: futon4) from ORG-FILE.
Write to OUTPUT-FILE (default: docs/docbook/BOOK/toc.json)."
  (let* ((book (or book "futon4"))
         (root (arxana-docbook--repo-root))
         (org-file (or org-file (arxana-docbook--default-org-file book root)))
         (org-file (and org-file (expand-file-name org-file root)))
         (books-root (arxana-docbook--default-books-root root))
         (output-dir (expand-file-name book books-root))
         (output-file (expand-file-name (or output-file "toc.json") output-dir)))
    (unless (and org-file (file-readable-p org-file))
      (error "Org file not readable; pass ORG-FILE explicitly"))
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
  (let* ((root (arxana-docbook--repo-root))
         (default-org (arxana-docbook--default-org-file "futon4" root)))
    (when default-org
      (arxana-docbook-export-toc "futon4" default-org))))

(provide 'docbook-toc-export)

;;; docbook-toc-export.el ends here
