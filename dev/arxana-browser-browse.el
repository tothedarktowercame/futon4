;;; arxana-browser-browse.el --- Part VI browsing helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Convenience wrappers that expose the legacy Part VI browsing flows via
;; modern `arxana-*' commands. Contributors can continue to use catalog and
;; label menus without memorising the old entry points.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup arxana-browser-browse nil
  "Part VI browsing entry points."
  :group 'arxana)

(defvar name-of-current-article nil
  "Name of the article associated with the current buffer.")

(defun arxana-browse--stringify (value)
  "Return VALUE as a displayable string."
  (cond
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(declare-function article-menu-listing "arxana-tangled" (&optional subset accessors))
(declare-function article-menu-list-all-articles "arxana-tangled" () t)
(declare-function article-menu-list-labels "arxana-tangled" () t)
(declare-function display-article "arxana-tangled" (target))

(defun arxana-browse--ensure (fn)
  "Signal a user error unless FN is bound."
  (unless (fboundp fn)
    (user-error "Browsing helpers are unavailable; load the dev/ modules first"))
  fn)

;;;###autoload
(defun arxana-browse-open-catalog (&optional include-all)
  "Open the article catalog. With INCLUDE-ALL, show every article."
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

(declare-function follow-reference-or-scholium "arxana-tangled" () t)
(declare-function display-an-article-that-current-article-is-about "arxana-tangled" () t)
(declare-function display-an-article-that-current-scholium-is-about "arxana-tangled" () t)
(declare-function sb-back "arxana-tangled" () t)
(declare-function sb-forward "arxana-tangled" () t)

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
  "Return a plist describing the parent/adjacent context for the current article."
  (when (and (boundp 'name-of-current-article)
             name-of-current-article
             (fboundp 'get-article))
    (let* ((child (get-article name-of-current-article))
           (parents (and (listp child) (nth 2 child)))
           (parent-name (and (listp parents) (car (car parents)))))
      (when parent-name
        (let* ((parent-name (arxana-browse--stringify parent-name))
               (parent (get-article parent-name))
               (siblings (and (listp parent) (cadr parent)))
               (names (mapcar #'arxana-browse--stringify siblings))
               (target (arxana-browse--stringify name-of-current-article))
               (idx (cl-position target names :test #'string=))
               (prev (when (and idx (> idx 0)) (nth (1- idx) names)))
               (next (when (and idx (< (1+ idx) (length names)))
                       (nth (1+ idx) names))))
          (list :parent parent-name :prev prev :next next))))))

;;;###autoload
(defun arxana-browse-open-parent ()
  "Open the parent article of the current article."
  (interactive)
  (let ((ctx (arxana-browse--parent-context)))
    (unless (and ctx (plist-get ctx :parent))
      (user-error "No parent context available"))
    (arxana-browse--ensure 'display-article)
    (display-article (plist-get ctx :parent))))

;;;###autoload
(defun arxana-browse-parent-next ()
  "Open the next sibling in the parent outline."
  (interactive)
  (let ((ctx (arxana-browse--parent-context)))
    (unless (and ctx (plist-get ctx :next))
      (user-error "No next sibling available"))
    (arxana-browse--ensure 'display-article)
    (display-article (plist-get ctx :next))))

;;;###autoload
(defun arxana-browse-parent-previous ()
  "Open the previous sibling in the parent outline."
  (interactive)
  (let ((ctx (arxana-browse--parent-context)))
    (unless (and ctx (plist-get ctx :prev))
      (user-error "No previous sibling available"))
    (arxana-browse--ensure 'display-article)
    (display-article (plist-get ctx :prev))))

(provide 'arxana-browser-browse)

;;; arxana-browser-browse.el ends here
