;;; arxana-browse.el --- Part VI browsing helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Convenience wrappers that expose the legacy Part VI browsing flows via
;; modern `arxana-*' commands.  These commands rely on the functions provided by
;; `arxana-tangled.el` so contributors can browse scholia, article catalogs, and
;; temporal/linear relationships without memorising the historical entry points.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup arxana-browse nil
  "Part VI browsing entry points."
  :group 'arxana)

(declare-function article-menu-listing "arxana-tangled" (&optional subset accessors))
(declare-function article-menu-list-all-articles "arxana-tangled" () t)
(declare-function article-menu-list-labels "arxana-tangled" () t)
(declare-function follow-reference-or-scholium "arxana-tangled" () t)
(declare-function display-an-article-that-current-article-is-about "arxana-tangled" () t)
(declare-function display-an-article-that-current-scholium-is-about "arxana-tangled" () t)
(declare-function sb-back "arxana-tangled" () t)
(declare-function sb-forward "arxana-tangled" () t)
(declare-function scholium-about "arxana-tangled" (article))
(declare-function scholium-text "arxana-tangled" (article))
(declare-function get-article "arxana-tangled" (name))
(declare-function display-article "arxana-tangled" (path))

(defvar name-of-current-article)

(defun arxana-browse--ensure (fn)
  "Signal a user error unless FN is bound."
  (unless (fboundp fn)
    (user-error "Browsing helpers are unavailable; run `arxana-build' first"))
  fn)

;;;###autoload
(defun arxana-browse-open-catalog (&optional include-all)
  "Open the article catalog.  With INCLUDE-ALL, show every article."
  (interactive "P")
  (if include-all
      (progn
        (arxana-browse--ensure 'article-menu-list-all-articles)
        (article-menu-list-all-articles))
    (progn
      (arxana-browse--ensure 'article-menu-listing)
      (article-menu-listing))))

;;;###autoload
(defun arxana-browse-open-labels ()
  "List label articles in the catalog menu."
  (interactive)
  (arxana-browse--ensure 'article-menu-list-labels)
  (article-menu-list-labels))

;;;###autoload
(defun arxana-browse-follow-link ()
  "Follow the scholium or reference at point."
  (interactive)
  (arxana-browse--ensure 'follow-reference-or-scholium)
  (follow-reference-or-scholium))

;;;###autoload
(defun arxana-browse-follow-article-about ()
  "Jump to an article referenced by the current article."
  (interactive)
  (arxana-browse--ensure 'display-an-article-that-current-article-is-about)
  (display-an-article-that-current-article-is-about))

;;;###autoload
(defun arxana-browse-follow-scholium-about ()
  "Jump to an article referenced by the current scholium."
  (interactive)
  (arxana-browse--ensure 'display-an-article-that-current-scholium-is-about)
  (display-an-article-that-current-scholium-is-about))

;;;###autoload
(defun arxana-browse-history-back ()
  "Move to the previous article in the temporal browsing history."
  (interactive)
  (arxana-browse--ensure 'sb-back)
  (sb-back))

;;;###autoload
(defun arxana-browse-history-forward ()
  "Move to the next article in the temporal browsing history."
  (interactive)
  (arxana-browse--ensure 'sb-forward)
  (sb-forward))

(defun arxana-browse--parent-context ()
  "Return a plist describing the parent/adjacent articles for the current node."
  (when (and (boundp 'name-of-current-article) name-of-current-article)
    (let* ((article (get-article name-of-current-article))
           (links (and article (scholium-about article)))
           (parent-link (and links (cl-find-if (lambda (link)
                                                 (member 'parent (cdr link)))
                                               links))))
      (when parent-link
        (let* ((parent-name (format "%s" (car parent-link)))
               (parent (get-article (car parent-link)))
               (children (and parent (scholium-text parent)))
               (child-names (and (listp children)
                                 (mapcar (lambda (entry) (format "%s" entry)) children)))
               (current-name (format "%s" name-of-current-article))
               (sequence (and child-names (member current-name child-names)))
               (next (and sequence (cadr sequence)))
               (prev (and sequence
                          (car (last (butlast child-names (length sequence)))))))
          (list :parent parent-name
                :next next
                :prev prev))))))

(defun arxana-browse--display-target (label target)
  (if target
      (progn
        (arxana-browse--ensure 'display-article)
        (display-article target))
    (user-error "%s" label)))

;;;###autoload
(defun arxana-browse-open-parent ()
  "Display the parent article (linear browsing)."
  (interactive)
  (let* ((ctx (arxana-browse--parent-context))
         (parent (plist-get ctx :parent)))
    (arxana-browse--display-target "No parent link for this article" parent)))

;;;###autoload
(defun arxana-browse-parent-next ()
  "Display the next sibling according to the parent listing."
  (interactive)
  (let* ((ctx (arxana-browse--parent-context))
         (target (plist-get ctx :next)))
    (arxana-browse--display-target "No next article in parent listing" target)))

;;;###autoload
(defun arxana-browse-parent-previous ()
  "Display the previous sibling according to the parent listing."
  (interactive)
  (let* ((ctx (arxana-browse--parent-context))
         (target (plist-get ctx :prev)))
    (arxana-browse--display-target "No previous article in parent listing" target)))

(provide 'arxana-browse)

;;; arxana-browse.el ends here
