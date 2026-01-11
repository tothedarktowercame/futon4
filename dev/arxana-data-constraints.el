;;; arxana-data-constraints.el --- Reazon-based data validation  -*- lexical-binding: t; -*-

;;; Commentary:
;; Validate non-window constraints using Reazon (miniKanren).

;;; Code:

(defgroup arxana-data-constraints nil
  "Reazon-based validation of Arxana data invariants."
  :group 'arxana)

(defcustom arxana-data-constraints-enable nil
  "When non-nil, validate data invariants at Arxana entry points."
  :type 'boolean
  :group 'arxana-data-constraints)

(defcustom arxana-data-constraints-timeout 0.2
  "Max time in seconds for Reazon queries."
  :type 'number
  :group 'arxana-data-constraints)

(defcustom arxana-data-constraints-failure-action 'message
  "How to report failed data constraints.
Use `message' to warn, `error' to raise, or nil to ignore."
  :type '(choice (const :tag "Message" message)
                 (const :tag "Error" error)
                 (const :tag "Ignore" nil))
  :group 'arxana-data-constraints)

(defvar arxana-data-constraints--reazon-tried nil)
(defvar arxana-data-constraints--reazon-missing-reported nil)
(defvar arxana-data-constraints--relations-defined nil)

;;; Helpers

(defun arxana-data-constraints--ensure-reazon ()
  "Return non-nil when Reazon is available."
  (or (featurep 'reazon)
      (unless arxana-data-constraints--reazon-tried
        (setq arxana-data-constraints--reazon-tried t)
        (require 'reazon nil t))))

(defun arxana-data-constraints--string-nonempty-p (value)
  (and (stringp value) (string-match-p "\\S-" value)))

(defun arxana-data-constraints--scope-ok-p (scope)
  (let ((repo (plist-get scope :repo))
        (roots (plist-get scope :code-roots)))
    (and (arxana-data-constraints--string-nonempty-p repo)
         (listp roots)
         (let ((ok t))
           (dolist (root roots)
             (unless (arxana-data-constraints--string-nonempty-p root)
               (setq ok nil)))
           ok))))

(defun arxana-data-constraints--finder-ok-p (finder)
  (let ((ftype (plist-get finder :type)))
    (and (or (not (boundp 'arxana-links-finder-types))
             (memq ftype arxana-links-finder-types))
         (let ((auto (plist-member finder :auto-link?)))
           (if auto
               (let ((val (plist-get finder :auto-link?)))
                 (or (eq val t) (eq val nil)))
             t)))))

(defun arxana-data-constraints--strategy-ok-p (strategy)
  (and (listp strategy)
       (arxana-data-constraints--string-nonempty-p (plist-get strategy :xt/id))
       (equal (plist-get strategy :type) "arxana/link-strategy")
       (arxana-data-constraints--scope-ok-p (plist-get strategy :scope))
       (listp (plist-get strategy :finders))
       (let ((ok t))
         (dolist (finder (plist-get strategy :finders))
           (unless (arxana-data-constraints--finder-ok-p finder)
             (setq ok nil)))
         ok)
       (arxana-data-constraints--string-nonempty-p (plist-get strategy :created-at))))

(defun arxana-data-constraints--voiced-link-ok-p (link)
  (and (listp link)
       (arxana-data-constraints--string-nonempty-p (plist-get link :xt/id))
       (equal (plist-get link :type) "arxana/voiced-link")
       (listp (plist-get link :source))
       (listp (plist-get link :target))
       (or (not (boundp 'arxana-links-status-values))
           (memq (plist-get link :status) arxana-links-status-values))))

(defun arxana-data-constraints--surface-form-ok-p (form)
  (and (listp form)
       (arxana-data-constraints--string-nonempty-p (plist-get form :xt/id))
       (equal (plist-get form :type) "arxana/surface-form")
       (arxana-data-constraints--string-nonempty-p (plist-get form :concept-id))
       (arxana-data-constraints--string-nonempty-p (plist-get form :surface))
       (arxana-data-constraints--string-nonempty-p (plist-get form :term))
       (arxana-data-constraints--string-nonempty-p (plist-get form :concept-label))))

(defun arxana-data-constraints--hyperedge-ok-p (hx-type endpoints)
  (and (or (symbolp hx-type) (arxana-data-constraints--string-nonempty-p hx-type))
       (listp endpoints)
       (let ((ok t))
         (dolist (endpoint endpoints)
           (when (null endpoint)
             (setq ok nil)))
         ok)))

(defun arxana-data-constraints--inclusion-ok-p (name caller)
  (and (arxana-data-constraints--string-nonempty-p name)
       (arxana-data-constraints--string-nonempty-p caller)))

(defun arxana-data-constraints--flexiarg-span-ok-p (file-start file-end)
  (and (stringp file-start)
       (stringp file-end)
       (string= file-start file-end)))

(defun arxana-data-constraints--bounce-ok-p (data)
  (let ((entries (plist-get data :entries))
        (expected-count (plist-get data :expected-count))
        (script (plist-get data :script))
        (entry-data (plist-get data :entry-data))
        (duplicates (plist-get data :duplicates))
        (ambiguous (plist-get data :ambiguous))
        (unmatched (plist-get data :unmatched)))
    (and (listp entries)
         (or (null expected-count)
             (= (length entries) expected-count))
         (stringp script)
         (file-exists-p script)
         (file-executable-p script)
         (listp entry-data)
         (= (length entry-data) (length entries))
         (let ((ok t))
           (dolist (item entry-data)
             (let ((path (plist-get item :path)))
               (unless (and (stringp path) (file-readable-p path))
                 (setq ok nil))))
           ok)
         (null duplicates)
         (null ambiguous)
         (null unmatched))))

(defun arxana-data-constraints--define-relations ()
  "Define Reazon relations used for data checks."
  (unless arxana-data-constraints--relations-defined
    (setq arxana-data-constraints--relations-defined t)
    (reazon-defrel arxana-data-constraints--strategy-o (strategy ok)
      (reazon-project (strategy)
        (reazon-== ok (arxana-data-constraints--strategy-ok-p strategy))))
    (reazon-defrel arxana-data-constraints--voiced-link-o (link ok)
      (reazon-project (link)
        (reazon-== ok (arxana-data-constraints--voiced-link-ok-p link))))
    (reazon-defrel arxana-data-constraints--surface-form-o (form ok)
      (reazon-project (form)
        (reazon-== ok (arxana-data-constraints--surface-form-ok-p form))))
    (reazon-defrel arxana-data-constraints--hyperedge-o (hx-type endpoints ok)
      (reazon-project (hx-type endpoints)
        (reazon-== ok (arxana-data-constraints--hyperedge-ok-p hx-type endpoints))))
    (reazon-defrel arxana-data-constraints--inclusion-o (name caller ok)
      (reazon-project (name caller)
        (reazon-== ok (arxana-data-constraints--inclusion-ok-p name caller))))
    (reazon-defrel arxana-data-constraints--flexiarg-span-o (file-start file-end ok)
      (reazon-project (file-start file-end)
        (reazon-== ok (arxana-data-constraints--flexiarg-span-ok-p file-start file-end))))
    (reazon-defrel arxana-data-constraints--bounce-o (data ok)
      (reazon-project (data)
        (reazon-== ok (arxana-data-constraints--bounce-ok-p data))))))

(defun arxana-data-constraints--ensure-relations ()
  "Load Reazon and define relations if possible."
  (when (arxana-data-constraints--ensure-reazon)
    (arxana-data-constraints--define-relations)
    t))

(defun arxana-data-constraints--report (label ok details)
  "Report constraint result with LABEL, OK, and DETAILS."
  (when (and (not ok) arxana-data-constraints-failure-action)
    (pcase arxana-data-constraints-failure-action
      ('message (message "Data constraint failed (%s): %s" label details))
      ('error (error "Data constraint failed (%s): %s" label details))
      (_ nil)))
  ok)

(defun arxana-data-constraints--query (form)
  "Evaluate FORM as a Reazon query and return non-nil on success."
  (let ((reazon-timeout arxana-data-constraints-timeout))
    (condition-case _err
        (and (eval form) t)
      (error nil))))

;;; Public API

(defun arxana-data-constraints-validate-strategy (strategy)
  "Validate link STRATEGY shape."
  (if (not arxana-data-constraints-enable)
      t
    (if (not (arxana-data-constraints--ensure-relations))
        (progn
          (unless arxana-data-constraints--reazon-missing-reported
            (setq arxana-data-constraints--reazon-missing-reported t)
            (message "Reazon not available; data constraints skipped"))
          nil)
      (let ((ok (arxana-data-constraints--query
                 `(reazon-run 1 q
                    (arxana-data-constraints--strategy-o ',strategy q)))))
        (arxana-data-constraints--report
         "link-strategy"
         ok
         "expected required strategy fields")))))

(defun arxana-data-constraints-validate-voiced-link (link)
  "Validate voiced LINK shape."
  (if (not arxana-data-constraints-enable)
      t
    (if (not (arxana-data-constraints--ensure-relations))
        (progn
          (unless arxana-data-constraints--reazon-missing-reported
            (setq arxana-data-constraints--reazon-missing-reported t)
            (message "Reazon not available; data constraints skipped"))
          nil)
      (let ((ok (arxana-data-constraints--query
                 `(reazon-run 1 q
                    (arxana-data-constraints--voiced-link-o ',link q)))))
        (arxana-data-constraints--report
         "voiced-link"
         ok
         "expected required voiced link fields")))))

(defun arxana-data-constraints-validate-surface-form (form)
  "Validate surface FORM shape."
  (if (not arxana-data-constraints-enable)
      t
    (if (not (arxana-data-constraints--ensure-relations))
        (progn
          (unless arxana-data-constraints--reazon-missing-reported
            (setq arxana-data-constraints--reazon-missing-reported t)
            (message "Reazon not available; data constraints skipped"))
          nil)
      (let ((ok (arxana-data-constraints--query
                 `(reazon-run 1 q
                    (arxana-data-constraints--surface-form-o ',form q)))))
        (arxana-data-constraints--report
         "surface-form"
         ok
         "expected required surface form fields")))))

(defun arxana-data-constraints-validate-hyperedge (hx-type endpoints)
  "Validate hyperedge HX-TYPE and ENDPOINTS."
  (if (not arxana-data-constraints-enable)
      t
    (if (not (arxana-data-constraints--ensure-relations))
        (progn
          (unless arxana-data-constraints--reazon-missing-reported
            (setq arxana-data-constraints--reazon-missing-reported t)
            (message "Reazon not available; data constraints skipped"))
          nil)
      (let ((ok (arxana-data-constraints--query
                 `(reazon-run 1 q
                    (arxana-data-constraints--hyperedge-o ',hx-type ',endpoints q)))))
        (arxana-data-constraints--report
         "hyperedge"
         ok
         "expected hx/type and non-empty endpoints")))))

(defun arxana-data-constraints-validate-inclusion (name caller)
  "Validate inclusion inputs NAME and CALLER."
  (if (not arxana-data-constraints-enable)
      t
    (if (not (arxana-data-constraints--ensure-relations))
        (progn
          (unless arxana-data-constraints--reazon-missing-reported
            (setq arxana-data-constraints--reazon-missing-reported t)
            (message "Reazon not available; data constraints skipped"))
          nil)
      (let ((ok (arxana-data-constraints--query
                 `(reazon-run 1 q
                    (arxana-data-constraints--inclusion-o ',name ',caller q)))))
        (arxana-data-constraints--report
         "inclusion"
         ok
         "expected non-empty article name and caller")))))

(defun arxana-data-constraints-validate-flexiarg-span (file-start file-end)
  "Validate a flexiarg edit span from FILE-START to FILE-END."
  (if (not arxana-data-constraints-enable)
      t
    (if (not (arxana-data-constraints--ensure-relations))
        (progn
          (unless arxana-data-constraints--reazon-missing-reported
            (setq arxana-data-constraints--reazon-missing-reported t)
            (message "Reazon not available; data constraints skipped"))
          nil)
      (let ((ok (arxana-data-constraints--query
                 `(reazon-run 1 q
                    (arxana-data-constraints--flexiarg-span-o
                     ',file-start ',file-end q)))))
        (arxana-data-constraints--report
         "flexiarg-span"
         ok
         "expected edits within one flexiarg source file")))))

(defun arxana-data-constraints-validate-bounce (data)
  "Validate media bounce DATA constraints."
  (if (not arxana-data-constraints-enable)
      t
    (if (not (arxana-data-constraints--ensure-relations))
        (progn
          (unless arxana-data-constraints--reazon-missing-reported
            (setq arxana-data-constraints--reazon-missing-reported t)
            (message "Reazon not available; data constraints skipped"))
          nil)
      (let ((ok (arxana-data-constraints--query
                 `(reazon-run 1 q
                    (arxana-data-constraints--bounce-o ',data q)))))
        (arxana-data-constraints--report
         "media-bounce"
         ok
         "expected valid bounce inputs and matches")))))

(provide 'arxana-data-constraints)

;;; arxana-data-constraints.el ends here
