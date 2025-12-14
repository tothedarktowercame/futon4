;;; arxana-browse.el --- Part VI browsing helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Convenience wrappers that expose the legacy Part VI browsing flows via
;; modern `arxana-*' commands. Contributors can continue to use catalog and
;; label menus without memorising the old entry points.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup arxana-browse nil
  "Part VI browsing entry points."
  :group 'arxana)

(declare-function article-menu-listing "arxana-tangled" (&optional subset accessors))
(declare-function article-menu-list-all-articles "arxana-tangled" () t)
(declare-function article-menu-list-labels "arxana-tangled" () t)

(defun arxana-browse--ensure (fn)
  "Signal a user error unless FN is bound."
  (unless (fboundp fn)
    (user-error "Browsing helpers are unavailable; run `arxana-build' first"))
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
