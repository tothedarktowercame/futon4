;;; arxana-browser-marks.el --- Browser mark helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared helpers for interpreting marks relative to the active browser view.
;;
;; TODO(org-sync): Track this module in dev/org-sync-tracker.org for mirroring
;; into the literate sources.

;;; Code:

(require 'seq)

(declare-function arxana-patterns--browser-current-items "arxana-patterns")

(defun arxana-browser-marks-items-in-context (marked-table mark-key &optional item-p items)
  "Return marked ITEMS in the current browser context.
MARKED-TABLE is a hash table of marks, MARK-KEY extracts the hash key from ITEM.
When ITEM-P is non-nil, only items satisfying it are considered. If ITEMS is nil,
use `arxana-patterns--browser-current-items`."
  (let ((items (or items (arxana-patterns--browser-current-items))))
    (seq-filter (lambda (item)
                  (and (or (null item-p) (funcall item-p item))
                       (let ((key (funcall mark-key item)))
                         (and key (gethash key marked-table)))))
                items)))

(provide 'arxana-browser-marks)
;;; arxana-browser-marks.el ends here
