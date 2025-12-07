;;; arxana-article.el --- Article lifecycle helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Captures the bridge between classic Arxana operations (scholia, metadata
;; hooks, deletion) and the Futon storage helpers.  Keeps canonical paths,
;; hydrates metadata payloads, and bumps the backend whenever articles change.

;;; Code:

(require 'arxana-store)
(require 'subr-x)

(defvar arxana-article-path-cache (make-hash-table :test 'equal)
  "Cache of canonical paths keyed by article name.")

(defun arxana-article--canonical-path (path)
  (when (and path (fboundp 'futon4--canonical-path))
    (setq path (futon4--canonical-path path)))
  path)

(defun arxana-article--remember-path (name path)
  (when (and name path)
    (puthash name path arxana-article-path-cache)))

(defun arxana-article--path-for (name)
  (or (gethash name arxana-article-path-cache)
      (and (fboundp 'futon4-lookup-article-id)
           (let ((ctx (ignore-errors (futon4--article-context-for-buffer (current-buffer)))))
             (plist-get ctx :path)))))

(defun arxana-article--labels-for (name)
  (let (labels)
    (dolist (pair (and (boundp 'modified-type-labels) modified-type-labels))
      (let* ((label (cdr pair))
             (entry (ignore-errors (get-article label)))
             (members (and entry (scholium-text entry))))
        (when (and (listp members) (member name members))
          (push label labels))))
    (nreverse (delete-dups labels))))

(defun arxana-article--metadata-map (name)
  (let* ((meta (ignore-errors (metadata-article name)))
         (text (and meta (scholium-text meta)))
         (links (cdr (assoc 'links text)))
         (backlinks (cdr (assoc 'backlinks text)))
         (labels (arxana-article--labels-for name)))
    (delq nil
          (list (when backlinks (cons 'backlinks backlinks))
                (when links (cons 'links links))
                (when labels (cons 'labels labels))))))

(defun arxana-article--metadata-envelope (name)
  (let ((meta (arxana-article--metadata-map name)))
    (when meta
      (list (cons 'metadata meta)))))

(defun arxana-store-sync-article (&optional name path extra-props)
  "Ensure NAME has a fresh Futon entity, optionally forcing PATH/EXTRA-PROPS."
  (let* ((resolved-name (or name
                            (and (boundp 'name) name)
                            (bound-and-true-p name-of-current-article))))
    (when resolved-name
      (let* ((canonical-path (arxana-article--canonical-path (or path (arxana-article--path-for resolved-name))))
             (metadata-props (arxana-article--metadata-envelope resolved-name))
             (props (append metadata-props extra-props)))
        (arxana-store-ensure-article :name resolved-name
                                     :path canonical-path
                                     :props props)))))

(defun arxana-store-sync-current-article ()
  "Interactive entry point for `arxana-store-sync-article'."
  (interactive)
  (if (arxana-store-sync-article)
      (message "Synced article metadata to Futon")
    (message "Unable to sync article metadata")))

(defun arxana-article--sync-current ()
  (arxana-store-sync-article))

(defun arxana-article--sync-deletion ()
  (arxana-store-sync-article nil nil '((status . "deleted"))))

(with-eval-after-load 'arxana-tangled
  ;; Replace the in-tree ensure helper with a richer variant that accepts props.
  (defun futon4-ensure-article-entity (id name path &optional spine-p cb props)
    (when id
      (let* ((payload-props (delq nil (append props
                                             (list (when path (cons 'path path))
                                                   (when spine-p (cons 'spine t))))))
             (payload (delq nil (list (cons 'id id)
                                      (cons 'name (if (symbolp name) (symbol-name name) name))
                                      (cons 'type "arxana/article")
                                      (when payload-props (cons 'props payload-props))))))
        (futon4--post-json "/entity" payload cb))))

  ;; Keep path cache in sync whenever articles are created from files/buffers.
  (defun arxana-article-remember-current-path (&rest _)
    (when (boundp 'name)
      (let ((path (or (and (boundp 'maybe-path) maybe-path)
                      (ignore-errors (buffer-file-name))
                      (and (boundp 'article-path) article-path))))
        (when path
          (arxana-article--remember-path name (arxana-article--canonical-path path))))))

  (add-hook 'new-scholium-hook #'arxana-article--sync-current t)
  (add-hook 'new-scholium-hook #'arxana-article-remember-current-path t)
  (add-hook 'scholium-modified-text-hook #'arxana-article--sync-current t)
  (add-hook 'scholium-modified-about-hook #'arxana-article--sync-current t)
  (add-hook 'scholium-modified-type-hook #'arxana-article--sync-current t)
  (add-hook 'delete-article-hook #'arxana-article--sync-deletion t))

(provide 'arxana-article)

;;; arxana-article.el ends here
