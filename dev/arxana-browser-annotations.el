;;; arxana-browser-annotations.el --- Shared annotation helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Small shared helpers for browser modules that work with annotation
;; hyperedges and passage strings.

;;; Code:

(require 'seq)
(require 'subr-x)

(defun arxana-browser-annotations-value-name (value)
  "Return VALUE as a plain string name when possible."
  (cond
   ((null value) nil)
   ((keywordp value) (substring (symbol-name value) 1))
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   (t (format "%s" value))))

(defun arxana-browser-annotations-alist-value (alist &rest keys)
  "Return the first non-nil value for KEYS in ALIST."
  (seq-some (lambda (key)
              (and (listp alist)
                   (ignore-errors (alist-get key alist))))
            keys))

(defun arxana-browser-annotations-endpoint-role (endpoint)
  "Return ENDPOINT role as a normalized string."
  (arxana-browser-annotations-value-name
   (arxana-browser-annotations-alist-value endpoint :role :hx/role)))

(defun arxana-browser-annotations-endpoint-entity-id (endpoint)
  "Return ENDPOINT entity id."
  (arxana-browser-annotations-alist-value endpoint :entity-id :id))

(defun arxana-browser-annotations-endpoint-passage (endpoint)
  "Return ENDPOINT passage string."
  (arxana-browser-annotations-alist-value endpoint :passage :hx/passage))

(defun arxana-browser-annotations-line-bounds-from-passage (passage)
  "Parse PASSAGE strings like \"line 7: ...\" or \"lines 1-4: ...\"."
  (cond
   ((and (stringp passage)
         (string-match "\\`lines? \\([0-9]+\\)-\\([0-9]+\\)\\(?::\\|\\'\\)" passage))
    (cons (string-to-number (match-string 1 passage))
          (string-to-number (match-string 2 passage))))
   ((and (stringp passage)
         (string-match "\\`line \\([0-9]+\\)\\(?::\\|\\'\\)" passage))
    (let ((line (string-to-number (match-string 1 passage))))
      (cons line line)))
   (t nil)))

(defun arxana-browser-annotations-passage-search-text (passage)
  "Extract a searchable text fragment from PASSAGE."
  (cond
   ((not (stringp passage)) nil)
   ((string-match "\\`lines? [0-9]+\\(?:-[0-9]+\\)?:\\s-*\\(.+\\)\\'" passage)
    (string-trim (match-string 1 passage)))
   (t (string-trim passage))))

(defun arxana-browser-annotations-passage-line-count (passage)
  "Return the expected line count for PASSAGE when available."
  (when-let ((bounds (arxana-browser-annotations-line-bounds-from-passage passage)))
    (max 1 (1+ (- (cdr bounds) (car bounds))))))

(provide 'arxana-browser-annotations)

;;; arxana-browser-annotations.el ends here
