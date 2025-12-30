;;; arxana-articles-export.el --- Org exporters for Arxana -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers that write the current article table back to Org files so
;; XTDB snapshots can be shared as portable bundles.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'arxana-article nil t)

(declare-function arxana-article--labels-for "arxana-article" (name))
(declare-function get-article "arxana-tangled" (name))
(declare-function scholium-name "arxana-tangled" (article))
(declare-function scholium-text "arxana-tangled" (article))
(declare-function scholium-about "arxana-tangled" (article))
(declare-function futon4--article-id-for "arxana-tangled" (name &optional path))
(declare-function link-type-accessor "arxana-tangled" (link type))
(declare-function arxana-store-save-snapshot "arxana-store" (&optional scope label))
(declare-function arxana-store--snapshot-scope-prompt "arxana-store" (&optional prompt default))
(declare-function arxana-store--snapshot-id-from-response "arxana-store" (response))
(declare-function arxana-store-sync-enabled-p "arxana-store" ())

(defconst arxana-export--manifest-name "MANIFEST.org"
  "Filename used for Org export manifests.")

(defun arxana-export--sanitize-filename (name)
  "Return a filesystem-safe slug for NAME."
  (let ((base (replace-regexp-in-string "[^[:alnum:]-]+" "-" (or name "article"))))
    (downcase (string-trim base "-+" "-+"))))

(defun arxana-export--labels-for (name)
  "Return labels for NAME when available."
  (when (fboundp 'arxana-article--labels-for)
    (arxana-article--labels-for name)))

(defun arxana-export--articles ()
  "Return article rows as (name body links labels id)."
  nil)

(defun arxana-export--link-kind (link)
  (let ((specs (cdr link)))
    (cond
     ((cl-find-if (lambda (item)
                    (and (listp item) (eq (car item) 'transclusion-of)))
                  specs)
      "transclusion")
     ((cl-find-if (lambda (item)
                    (and (listp item) (eq (car item) 'passage)))
                  specs)
      "passage")
     (t "relation"))))

(defun arxana-export--format-link (link)
  (let ((target (car link)))
    (format "[%s] %s" (arxana-export--link-kind link) (or target "?"))))

(defun arxana-export--format-links (links)
  (if (null links)
      ""
    (mapconcat #'arxana-export--format-link links ", ")))

(defun arxana-export--write-article-file (dir name body)
  "Write NAME/BODY as an Org file in DIR, returning the filename."
  (let* ((slug (arxana-export--sanitize-filename name))
         (filename (format "%s.org" (if (string-empty-p slug) "article" slug)))
         (path (expand-file-name filename dir)))
    (with-temp-file path
      (insert (format "#+TITLE: %s\n\n" name))
      (when body
        (insert body)))
    filename))

(defun arxana-export-org-directory (directory)
  "Export articles into DIRECTORY as Org files with a manifest."
  (interactive "DExport directory: ")
  (let* ((dest (file-name-as-directory (expand-file-name directory)))
         (articles (or (arxana-export--articles) '()))
         (snapshot-id nil)
         (snapshot-scope nil))
    (make-directory dest t)
    (when (and (fboundp 'arxana-store-sync-enabled-p)
               (arxana-store-sync-enabled-p))
      (setq snapshot-scope
            (arxana-store--snapshot-scope-prompt
             "Snapshot scope (all/latest): " "all"))
      (let ((label (read-string "Snapshot label (optional): ")))
        (when (string-empty-p label)
          (setq label nil))
        (let ((response (arxana-store-save-snapshot snapshot-scope label)))
          (when (fboundp 'arxana-store--snapshot-id-from-response)
            (setq snapshot-id
                  (arxana-store--snapshot-id-from-response response))))))
    (let ((manifest (expand-file-name arxana-export--manifest-name dest)))
      (with-temp-buffer
        (insert "#+TITLE: MANIFEST\n\n")
        (insert "* Snapshot\n")
        (when snapshot-id
          (insert (format "- ID: %s\n" snapshot-id)))
        (when snapshot-scope
          (insert (format "- Scope: %s\n" snapshot-scope)))
        (insert "- Hyperedges: 0\n\n")
        (insert "* Article Index\n")
        (insert "| Name | File | ID | Labels | Links |\n")
        (insert "|-\n")
        (dolist (article articles)
          (let* ((name (nth 0 article))
                 (body (nth 1 article))
                 (links (nth 2 article))
                 (labels (or (nth 3 article)
                             (arxana-export--labels-for name)))
                 (id (or (nth 4 article)
                         (when (fboundp 'futon4--article-id-for)
                           (futon4--article-id-for name))))
                 (file (arxana-export--write-article-file dest name body))
                 (label-str (if labels (string-join labels ", ") ""))
                 (link-str (arxana-export--format-links links)))
            (insert (format "| %s | %s | %s | %s | %s |\n"
                            name file (or id "") label-str link-str))))
        (insert "\n")
        (insert "#+COMMENT: * Article Index\n")
        (dolist (article articles)
          (let* ((name (nth 0 article))
                 (labels (or (nth 3 article)
                             (arxana-export--labels-for name)))
                 (label-str (if labels (string-join labels ", ") ""))
                 (links (nth 2 article))
                 (link-str (arxana-export--format-links links)))
            (when (not (string-empty-p label-str))
              (insert (format "- Labels: %s\n" label-str)))
            (when (not (string-empty-p link-str))
              (insert (format "- Links: %s\n" link-str)))))
        (insert "\n* Label Index\n")
        (write-region (point-min) (point-max) manifest)))
    dest))

(provide 'arxana-articles-export)

;;; arxana-articles-export.el ends here
