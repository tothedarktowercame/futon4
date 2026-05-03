;;; arxana-browser-essays-podcasts.el --- Register podcast catalogs in Essays  -*- lexical-binding: t; -*-

;;; Commentary:
;; Discovers podcast-source essay catalogs under
;; `arxana-browser-essays-podcasts-data-directory' and registers them
;; with the Essays browser via the same `arxana-browser-essays-catalogs'
;; / `arxana-browser-essays-manifest-files' interface used by
;; `arxana-browser-essays-wikibooks.el'.
;;
;; Mirrors the wikibooks browser's discovery + sidecar-merge flow but
;; without any platform-specific (MediaWiki API, page-title) machinery —
;; podcasts are read-only catalogs assembled from local transcripts;
;; the build path lives in `futon4/scripts/peeragogy-podcasts.py' and
;; `peeragogy-podcasts-build-essays-manifest.clj'.
;;
;; Mission: M-peeragogy-rewrite (MAP).

;;; Code:

(require 'arxana-browser-essays)

(defconst arxana-browser-essays-podcasts--file
  (or load-file-name (buffer-file-name))
  "Path of this file at load time; used to anchor the repo-root path.")

(defconst arxana-browser-essays-podcasts--repo-root
  (file-name-as-directory
   (file-name-directory
    (directory-file-name
     (file-name-directory arxana-browser-essays-podcasts--file))))
  "Repo root inferred from this file's location (parent of `dev/').")

(defcustom arxana-browser-essays-podcasts-data-directory
  (expand-file-name "data/essays/podcasts/"
                    arxana-browser-essays-podcasts--repo-root)
  "Root directory under which podcast essay catalogs live."
  :type 'directory
  :group 'arxana-browser-essays)

(defcustom arxana-browser-essays-podcasts-auto-sync t
  "When non-nil, discover podcast manifests on refresh/load."
  :type 'boolean
  :group 'arxana-browser-essays)

(defun arxana-browser-essays-podcasts--manifest-files ()
  "Return discovered podcast manifest files."
  (when (file-directory-p arxana-browser-essays-podcasts-data-directory)
    (sort
     (directory-files-recursively
      arxana-browser-essays-podcasts-data-directory
      "-book-manifest\\.el\\'")
     #'string<)))

(defun arxana-browser-essays-podcasts--annotations-file-for-manifest-file (path)
  "Return the sidecar annotations file for manifest PATH."
  (when (and path (string-suffix-p "-manifest.el" path))
    (concat (string-remove-suffix "-manifest.el" path)
            "-annotations.el")))

(defun arxana-browser-essays-podcasts--first-defconst-symbol (path)
  "Return the first defconst symbol defined in PATH."
  (let (symbol)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (condition-case nil
          (while (and (not symbol) (not (eobp)))
            (let ((form (read (current-buffer))))
              (when (and (listp form)
                         (memq (car form) '(defconst defvar defcustom))
                         (symbolp (cadr form)))
                (setq symbol (cadr form)))))
        (end-of-file nil)))
    symbol))

(defun arxana-browser-essays-podcasts--load-defconst (path)
  "Load PATH and return the first defconst's value, or nil."
  (let ((sym (arxana-browser-essays-podcasts--first-defconst-symbol path)))
    (when sym
      (load path nil t)
      (and (boundp sym) (symbol-value sym)))))

(defun arxana-browser-essays-podcasts--merge-annotations (base extras)
  "Return BASE annotations merged with EXTRAS, deduplicated by `:id'."
  (let ((seen (make-hash-table :test 'equal))
        merged)
    (dolist (ann (append base extras))
      (let ((ann-id (plist-get ann :id)))
        (unless (or (null ann-id) (gethash ann-id seen))
          (puthash ann-id t seen)
          (push ann merged))))
    (nreverse merged)))

(defun arxana-browser-essays-podcasts--merge-sidecar (path symbol manifest)
  "Merge sidecar annotations into MANIFEST under SYMBOL for PATH."
  (let* ((sidecar-path
          (arxana-browser-essays-podcasts--annotations-file-for-manifest-file
           path))
         (sidecar (and sidecar-path
                       (file-readable-p sidecar-path)
                       (arxana-browser-essays-podcasts--load-defconst
                        sidecar-path))))
    (if (not sidecar)
        manifest
      (let* ((existing (plist-get manifest :annotations))
             (extra (cond
                     ((and (listp sidecar) (plist-get sidecar :annotations))
                      (plist-get sidecar :annotations))
                     ((listp sidecar) sidecar)
                     (t nil)))
             (merged (plist-put (copy-tree manifest)
                                :annotations
                                (arxana-browser-essays-podcasts--merge-annotations
                                 existing extra))))
        (when (and symbol (boundp symbol))
          (set symbol merged))
        merged))))

(defun arxana-browser-essays-podcasts--catalog-from-manifest-file (path)
  "Return a catalog plist derived from podcast manifest PATH."
  (let ((symbol (arxana-browser-essays-podcasts--first-defconst-symbol path)))
    (when symbol
      (load path nil t)
      (let* ((manifest (and (boundp symbol) (symbol-value symbol)))
             (manifest (and manifest
                            (arxana-browser-essays-podcasts--merge-sidecar
                             path symbol manifest)))
             (essay (and (listp manifest) (plist-get manifest :essay)))
             (essay-id (and essay (plist-get essay :id)))
             (name (and essay (plist-get essay :name)))
             (essay-props (and essay (plist-get essay :props))))
        (when (and essay-id
                   (eq (alist-get 'podcasts-catalog essay-props) t))
          (list :id (intern (replace-regexp-in-string "[^a-z0-9]+" "-"
                                                      (downcase essay-id)))
                :label name
                :description "Imported from a podcast playlist."
                :essay-id essay-id
                :manifest-symbol symbol
                :manifest-file path))))))

(defun arxana-browser-essays-podcasts--owned-p (cat)
  "True when CAT was registered by the podcasts module."
  (let* ((eid (plist-get cat :essay-id)))
    (and (stringp eid) (string-prefix-p "arxana/essay/podcasts/" eid))))

(defun arxana-browser-essays-podcasts--manifest-file-p (path)
  "True when PATH lives under the podcasts data directory."
  (let ((abs-path (and path (expand-file-name path)))
        (abs-root (expand-file-name
                   (file-name-as-directory
                    arxana-browser-essays-podcasts-data-directory))))
    (and (stringp abs-path)
         (string-prefix-p abs-root abs-path))))

(defun arxana-browser-essays-podcasts-sync-registrations ()
  "Discover podcast catalogs and register them with the Essays browser."
  (interactive)
  (when arxana-browser-essays-podcasts-auto-sync
    (let* ((files (arxana-browser-essays-podcasts--manifest-files))
           (catalogs (delq nil
                           (mapcar
                            #'arxana-browser-essays-podcasts--catalog-from-manifest-file
                            files))))
      (setq arxana-browser-essays-manifest-files
            (delete-dups
             (append
              (seq-remove
               #'arxana-browser-essays-podcasts--manifest-file-p
               arxana-browser-essays-manifest-files)
              files)))
      (setq arxana-browser-essays-catalogs
            (append
             (seq-remove
              #'arxana-browser-essays-podcasts--owned-p
              arxana-browser-essays-catalogs)
             catalogs))))
  (when (called-interactively-p 'interactive)
    (message "[podcasts] Registered %d manifest(s)"
             (length (arxana-browser-essays-podcasts--manifest-files)))))

(defun arxana-browser-essays-podcasts--refresh-advice (&rest _)
  "Advice: resync podcast manifest registrations after Essays refresh."
  (arxana-browser-essays-podcasts-sync-registrations))

;; Run discovery once at load and re-run after every Essays refresh.
(arxana-browser-essays-podcasts-sync-registrations)
(advice-add 'arxana-browser-essays-refresh :after
            #'arxana-browser-essays-podcasts--refresh-advice)

(provide 'arxana-browser-essays-podcasts)

;;; arxana-browser-essays-podcasts.el ends here
