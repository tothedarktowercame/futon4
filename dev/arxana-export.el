;;; arxana-export.el --- Org exporters for Arxana -*- lexical-binding: t; -*-

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


(defvar arxana-export-default-extension ".org")

(defun arxana-export--normalize-name (name)
  "Return a readable string for NAME."
  (cond
   ((null name) nil)
   ((stringp name) name)
   ((symbolp name) (symbol-name name))
   (t (format "%s" name))))

(defun arxana-export--labels-for (name)
  "Return a list of label names associated with NAME."
  (let ((labels (cond
                 ((fboundp 'arxana-article--labels-for)
                  (ignore-errors (arxana-article--labels-for name)))
                 ((and (boundp 'modified-type-labels)
                       (fboundp 'get-article)
                       (fboundp 'scholium-text))
                  (let (acc)
                    (dolist (pair modified-type-labels)
                      (let* ((label (cdr pair))
                             (entry (ignore-errors (get-article label)))
                             (members (and entry (scholium-text entry))))
                        (when (and (listp members) (member name members))
                          (push label acc))))
                    acc))))
        (strings nil))
    (dolist (label labels)
      (let ((normalized (arxana-export--normalize-name label)))
        (when normalized
          (push normalized strings))))
    (nreverse (delete-dups strings))))

(defun arxana-export--link-kind (link)
  "Classify LINK for manifest summaries."
  (cond
   ((and (fboundp 'link-type-accessor)
         (link-type-accessor link 'transclusion-of))
    "transclusion")
   ((and (fboundp 'link-type-accessor)
         (link-type-accessor link 'identifies-with))
    "identification")
   ((and (fboundp 'link-type-accessor)
         (link-type-accessor link 'passage))
    "inclusion")
   (t "reference")))

(defun arxana-export--link-passage (link)
  "Return plist describing the passage payload inside LINK, when present."
  (let ((entry (and (fboundp 'link-type-accessor)
                    (link-type-accessor link 'passage))))
    (when (and entry (listp entry))
      (let* ((payload (cdr entry))
             (parts (if (and (= (length payload) 1)
                             (listp (car payload)))
                        (car payload)
                      payload))
             (article (arxana-export--normalize-name (car-safe parts)))
             (begin (nth 1 parts))
             (end (nth 2 parts)))
        (when (or article begin end)
          (list :article article :begin begin :end end))))))

(defun arxana-export--link-label (link)
  "Return the Futon label string for LINK, when available."
  (when (fboundp 'futon4--link-label)
    (let ((label (ignore-errors (futon4--link-label link))))
      (unless (string-empty-p (or label ""))
        label))))

(defun arxana-export--link-details (article)
  "Return structured metadata for ARTICLE's about links."
  (let ((about (and (fboundp 'scholium-about)
                    (scholium-about article)))
        (details nil))
    (when (listp about)
      (dolist (link about)
        (when (listp link)
          (let ((target (arxana-export--normalize-name (car-safe link))))
            (push (list :target target
                        :kind (arxana-export--link-kind link)
                        :passage (arxana-export--link-passage link)
                        :label (arxana-export--link-label link))
                  details)))))
    (nreverse details)))

(defun arxana-export--article-metadata (article)
  "Return a plist describing ARTICLE for manifest purposes."
  (let* ((name (arxana-export--normalize-name (scholium-name article)))
         (labels (and name (arxana-export--labels-for name)))
         (links (arxana-export--link-details article)))
    (list :labels labels
          :links links
          :link-count (length links))))

(defun arxana-export--slugify (name)
  "Return a filesystem-friendly slug for NAME."
  (let* ((down (downcase (replace-regexp-in-string "[^A-Za-z0-9]+" "-" (or name "article"))))
         (trimmed (string-trim down "-+" "-+")))
    (if (string-empty-p trimmed)
        "article"
      trimmed)))

(defun arxana-export--unique-path (directory base)
  "Return a unique path inside DIRECTORY using BASE (sans extension)."
  (let ((candidate base)
        (counter 0)
        (ext arxana-export-default-extension)
        (full nil))
    (while (progn
             (setq full (expand-file-name (concat candidate ext) directory))
             (file-exists-p full))
      (setq counter (1+ counter))
      (setq candidate (format "%s-%d" base counter)))
    full))

(defun arxana-export--article-id (name path)
  "Best-effort Futon id for NAME/PATH."
  (when (fboundp 'futon4--article-id-for)
    (futon4--article-id-for name path)))

(defun arxana-export--write-article (article destination snapshot)
  "Write ARTICLE into DESTINATION directory. SNAPSHOT is plist metadata."
  (let* ((raw-name (or (scholium-name article) "Untitled"))
         (name (arxana-export--normalize-name raw-name))
         (text (or (scholium-text article) ""))
         (slug (arxana-export--slugify name))
         (target (arxana-export--unique-path destination slug))
         (article-id (arxana-export--article-id name target))
         (metadata (arxana-export--article-metadata article)))
    (with-temp-file target
      (insert (format "#+ARXANA-ID: %s\n" (or article-id "<unknown>")))
      (insert (format "#+TITLE: %s\n" name))
      (when snapshot
        (insert (format "#+SNAPSHOT: %s (%s)\n"
                        (or (plist-get snapshot :id) "<unspecified>")
                        (plist-get snapshot :scope))))
      (insert "\n" text))
    (nconc (list :name name
                 :file target
                 :id (or article-id "<unknown>"))
           metadata)))

(defun arxana-export--articles ()
  "Return a list of all articles currently known."
  (let (results)
    (maphash (lambda (name _val)
               (let ((article (get-article name)))
                 (when article
                   (push article results))))
             article-table)
    results))

(defun arxana-export--manifest-path (directory)
  (expand-file-name "MANIFEST.org" directory))

(defun arxana-export--format-labels (labels)
  "Return a comma-separated string for LABELS, defaulting to '-'."
  (if (and labels (not (null labels)))
      (string-join labels ", ")
    "-"))

(defun arxana-export--format-passage (passage)
  "Return a human-readable description for PASSAGE."
  (when passage
    (let ((article (plist-get passage :article))
          (begin (plist-get passage :begin))
          (end (plist-get passage :end)))
      (cond
       ((and article begin end)
        (format " (span %s %s-%s)" article begin end))
       ((and begin end)
        (format " (span %s-%s)" begin end))
       ((and article begin)
        (format " (span %s %s)" article begin))
       ((and article)
        (format " (span %s)" article))
       (t "")))))

(defun arxana-export--format-link-label (label)
  "Return a trailing metadata note for LABEL."
  (when (and label (not (string-empty-p label)))
    (format " (metadata: %s)" label)))

(defun arxana-export--insert-hyperedge-lines (links)
  "Insert bullet list entries describing LINKS."
  (if (and links (not (null links)))
      (progn
        (insert "- Hyperedges:\n")
        (dolist (link links)
          (let* ((kind (or (plist-get link :kind) "reference"))
                 (target (or (plist-get link :target) "<unknown>"))
                 (passage (arxana-export--format-passage (plist-get link :passage)))
                 (label (arxana-export--format-link-label (plist-get link :label))))
            (insert (format "  - [%s] %s%s%s\n" kind target (or passage "") (or label ""))))))
    (insert "- Hyperedges: none\n")))

(defun arxana-export--label-index (entries)
  "Return an alist mapping label -> article names drawn from ENTRIES."
  (let ((table (make-hash-table :test 'equal))
        result)
    (dolist (entry entries)
      (dolist (label (plist-get entry :labels))
        (let ((bucket (gethash label table)))
          (puthash label (cons (plist-get entry :name) bucket) table))))
    (maphash (lambda (label names)
               (push (cons label (cl-sort (copy-seq names) #'string-lessp)) result))
             table)
    (cl-sort result #'string-lessp :key #'car)))

(defun arxana-export--insert-article-sections (directory entries)
  "Write the article index section for ENTRIES under DIRECTORY."
  (insert "* Article Index\n\n")
  (dolist (entry entries)
    (let* ((name (plist-get entry :name))
           (relative (file-relative-name (plist-get entry :file) directory))
           (labels (plist-get entry :labels))
           (links (plist-get entry :links)))
      (insert (format "** %s\n" name))
      (insert (format "- File: %s\n" relative))
      (insert (format "- Futon ID: %s\n" (plist-get entry :id)))
      (insert (format "- Labels: %s\n" (arxana-export--format-labels labels)))
      (arxana-export--insert-hyperedge-lines links)
      (insert "\n"))))

(defun arxana-export--insert-label-sections (entries)
  "Write the label index section for ENTRIES when labels exist."
  (let ((index (arxana-export--label-index entries)))
    (when index
      (insert "* Label Index\n\n")
      (dolist (pair index)
        (insert (format "** %s\n" (car pair)))
        (dolist (name (cdr pair))
          (insert (format "- %s\n" name)))
        (insert "\n")))))

(defun arxana-export--write-manifest (directory snapshot entries)
  "Write a MANIFEST.org into DIRECTORY for SNAPSHOT and ENTRIES."
  (let* ((sorted (cl-sort (copy-seq entries) #'string-lessp
                          :key (lambda (entry) (plist-get entry :name))))
         (article-count (length sorted))
         (hyperedges (cl-loop for entry in sorted
                              sum (or (plist-get entry :link-count) 0))))
    (with-temp-file (arxana-export--manifest-path directory)
      (insert "#+TITLE: Arxana Export Manifest\n\n")
      (when snapshot
        (insert (format "- Snapshot: %s\n" (or (plist-get snapshot :id) "<unspecified>")))
        (insert (format "- Scope: %s\n" (plist-get snapshot :scope)))
        (when-let ((label (plist-get snapshot :label)))
          (insert (format "- Label: %s\n" label)))
        (insert (format "- Timestamp: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S"))))
      (insert (format "- Articles: %d\n" article-count))
      (insert (format "- Hyperedges: %d\n\n" hyperedges))
      (insert "| Name | File | ID | Labels | Links |\n|-\n")
      (dolist (entry sorted)
        (insert (format "| %s | %s | %s | %s | %d |\n"
                        (plist-get entry :name)
                        (file-relative-name (plist-get entry :file) directory)
                        (plist-get entry :id)
                        (arxana-export--format-labels (plist-get entry :labels))
                        (or (plist-get entry :link-count) 0))))
      (insert "\n")
      (arxana-export--insert-article-sections directory sorted)
      (arxana-export--insert-label-sections sorted))))

(defun arxana-export--maybe-snapshot ()
  "Request a snapshot when sync is enabled."
  (when (and (fboundp 'arxana-store-sync-enabled-p)
             (arxana-store-sync-enabled-p))
    (let* ((scope (arxana-store--snapshot-scope-prompt "Snapshot scope (all/latest): " "all"))
           (label (read-string "Snapshot label (optional): " nil nil ""))
           (response (arxana-store-save-snapshot scope label))
           (snapshot-id (and response (arxana-store--snapshot-id-from-response response))))
      (when response
        (list :scope scope :label label :id snapshot-id)))))

;;;###autoload
(defun arxana-export-org-directory (directory)
  "Export the current article table to DIRECTORY as Org files."
  (interactive (list (read-directory-name "Export Org to directory: "))) 
  (let* ((dest (file-name-as-directory (expand-file-name directory)))
         (snapshot (arxana-export--maybe-snapshot))
         (articles (arxana-export--articles))
         (entries nil))
    (unless articles
      (user-error "No articles are loaded; nothing to export"))
    (make-directory dest t)
    (dolist (article articles)
      (push (arxana-export--write-article article dest snapshot) entries))
    (arxana-export--write-manifest dest snapshot (nreverse entries))
    (message "Exported %d articles to %s" (length entries) dest)
    dest))

(provide 'arxana-export)

;;; arxana-export.el ends here
