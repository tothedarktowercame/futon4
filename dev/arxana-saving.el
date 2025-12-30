;;; arxana-saving.el --- Snapshot-aware save/restore helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides runtime shims so the legacy `save-all-scholia` and
;; `read-scholia-file` commands can drive Futon's snapshot endpoints when
;; sync is enabled.  When sync is disabled the original file-based
;; implementations continue to run unchanged.

;;; Code:

(require 'subr-x)

(declare-function arxana-store-save-snapshot "arxana-store" (&optional scope label))
(declare-function arxana-store-restore-snapshot "arxana-store" (&optional snapshot-id scope))
(declare-function arxana-store--snapshot-scope-prompt "arxana-store" (&optional prompt default))
(declare-function arxana-store--snapshot-id-from-response "arxana-store" (response))

(defconst arxana-saving--fallback-scopes '("all" "latest")
  "Scopes offered when the store helpers are unavailable.")

(defun arxana-saving--prompt-scope (prompt default)
  "Read a snapshot scope with PROMPT and DEFAULT.
Returns DEFAULT without prompting when running noninteractively."
  (cond
   (noninteractive default)
   ((fboundp 'arxana-store--snapshot-scope-prompt)
    (arxana-store--snapshot-scope-prompt prompt default))
   (t (completing-read prompt arxana-saving--fallback-scopes nil t nil nil default))))

(defun arxana-saving--prompt-label ()
  "Ask the user for an optional snapshot label.
Returns nil without prompting when running noninteractively."
  (if noninteractive
      nil
    (let ((label (read-string "Snapshot label (optional): ")))
      (unless (string-empty-p label)
        label))))

(defun arxana-saving--snapshot-enabled-p ()
  "Return non-nil when snapshot shims should run."
  (and (boundp 'futon4-enable-sync)
       futon4-enable-sync
       (fboundp 'arxana-store-save-snapshot)))

(defun arxana-saving--restore-enabled-p ()
  "Return non-nil when restore shims should run."
  (and (boundp 'futon4-enable-sync)
       futon4-enable-sync
       (fboundp 'arxana-store-restore-snapshot)))

(defun arxana-saving--wrap-save-all (orig-fn filename &optional scope)
  "Around advice for `save-all-scholia'.
ORIG-FN is the original implementation; FILENAME and SCOPE are passed
from the caller (SCOPE is only present when invoked programmatically)."
  (if (arxana-saving--snapshot-enabled-p)
      (let* ((scope (or scope (arxana-saving--prompt-scope "Snapshot scope (all/latest): " "all")))
             (label (or (and noninteractive filename)
                        (arxana-saving--prompt-label)))
             (response (arxana-store-save-snapshot scope label))
             (id (when (fboundp 'arxana-store--snapshot-id-from-response)
                   (arxana-store--snapshot-id-from-response response))))
        (message "Saved Futon snapshot scope=%s id=%s"
                 scope (or id "<unknown>")))
    (apply orig-fn (list filename))))

(defun arxana-saving--wrap-read-scholia (orig-fn filepath &optional scope)
  "Around advice for `read-scholia-file'.
ORIG-FN is the original implementation; FILEPATH is treated as a
snapshot id when syncing is enabled."
  (if (arxana-saving--restore-enabled-p)
      (let* ((snapshot-id (unless (string-empty-p filepath) filepath))
             (scope (or scope (arxana-saving--prompt-scope "Restore scope (all/latest): " "all"))))
        (arxana-store-restore-snapshot snapshot-id scope))
    (apply orig-fn (list filepath))))

(unless (fboundp 'save-all-scholia)
  (defun save-all-scholia (filename &optional scope)
    "Fallback implementation that delegates to Futon snapshots when enabled."
    (interactive "FSave scholia to: ")
    (arxana-saving--wrap-save-all
     (lambda (&rest _args)
       (message "Saved scholia to %s" filename))
     filename scope)))

(unless (fboundp 'read-scholia-file)
  (defun read-scholia-file (filepath &optional scope)
    "Fallback implementation that restores from Futon snapshots when enabled."
    (interactive "fRead scholia file: ")
    (arxana-saving--wrap-read-scholia
     (lambda (&rest _args)
       (message "Read scholia from %s" filepath))
     filepath scope)))

(with-eval-after-load 'arxana-tangled
  (when (fboundp 'save-all-scholia)
    (advice-add 'save-all-scholia :around #'arxana-saving--wrap-save-all))
  (when (fboundp 'read-scholia-file)
    (advice-add 'read-scholia-file :around #'arxana-saving--wrap-read-scholia)))

(provide 'arxana-saving)

;;; arxana-saving.el ends here
