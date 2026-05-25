;;; arxana-vsatarcs-aif-edn-sync.el --- :aif-edn-revision-entry executor -*- lexical-binding: t; -*-

;;; Commentary:
;; Executor for the `:aif-edn-revision-entry' writer-action-class —
;; the L4 second self-documentation sibling per D5 ordering.  Appends
;; a new entry to the `:provenance :revisions' log of a `*.aif.edn'
;; file.
;;
;; Parallel in shape to `arxana-vsatarcs-mission-doc-sync.el' — same
;; cycle contract (propose → predict → consent → execute → observe →
;; prediction-error → writer-event record) but targets EDN instead of
;; markdown.
;;
;; **Recursive shape** (claude-4 bell 2026-05-20 v0.5.20): when this
;; class lands, the consent-request entry for ITS OWN landing is
;; itself an :aif-edn-revision-entry candidate.  The
;; `cycle-recursive-self-landing-test' in
;; `arxana-vsatarcs-aif-edn-sync-test.el' covers the self-application
;; case — landing an entry that documents the L4 landing itself.

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-writer-actions)
(require 'arxana-vsatarcs-consent)
(require 'arxana-vsatarcs-writer-trace)

(defgroup arxana-vsatarcs-aif-edn-sync nil
  "Executor for :aif-edn-revision-entry writer-action-class."
  :group 'arxana-vsatarcs)

;; ---------------------------------------------------------------------
;; End-to-end cycle
;; ---------------------------------------------------------------------

(cl-defun arxana-vsatarcs-aif-edn-sync-cycle
    (action &key (consent-source :operator) emit-trace-p)
  "Run one end-to-end aif-edn-revision-entry cycle for ACTION.
Returns a writer-event record.  See `arxana-vsatarcs-mission-doc-sync-cycle'
for the analogous shape on the markdown side.

CONSENT-SOURCE defaults to :operator.  Use :autopen for rules-rehearsal
in tests + dogfooding cycles."
  (cl-assert (eq :aif-edn-revision-entry (plist-get action :type)))
  ;; Admissibility (I5)
  (unless (arxana-vsatarcs-writer-actions-can-propose? action)
    (let ((record (arxana-vsatarcs-writer-trace-build-record
                   :proposed-action action
                   :consent-request nil
                   :consent-response
                   (list :response :abstain-for-now
                         :source :pre-consent-check
                         :source-id "can-propose-false"
                         :rationale "Action failed can-propose? admissibility check"
                         :timestamp (arxana-vsatarcs-trace--now-iso)))))
      (when emit-trace-p (arxana-vsatarcs-writer-trace-emit record))
      (cl-return-from arxana-vsatarcs-aif-edn-sync-cycle record)))
  ;; Categorical abstain on :pivot (D2 + I9)
  (when (eq :pivot (plist-get action :match-type))
    (let ((record (arxana-vsatarcs-writer-trace-build-record
                   :proposed-action action
                   :consent-request nil
                   :consent-response
                   (list :response :abstain-for-now
                         :source :policy
                         :source-id "I9-pivot-abstain"
                         :rationale "Match-type :pivot triggers abstain without consent request"
                         :timestamp (arxana-vsatarcs-trace--now-iso)))))
      (when emit-trace-p (arxana-vsatarcs-writer-trace-emit record))
      (cl-return-from arxana-vsatarcs-aif-edn-sync-cycle record)))
  ;; Forward-model prediction
  (let* ((predicted
          (arxana-vsatarcs-writer-actions-predict-effects action))
         (request (arxana-vsatarcs-consent-build-request
                   :proposed-action action
                   :rationale (plist-get action :rationale)
                   :predicted-effects predicted
                   :class :aif-edn-revision-entry
                   :stakes :low
                   :reversibility :trivial
                   :match-type (plist-get action :match-type)))
         (response (arxana-vsatarcs-consent-gate request consent-source))
         (executed nil)
         (observed-post-state nil)
         (prediction-error nil))
    (when (eq :confirm (plist-get response :response))
      (if (arxana-vsatarcs-writer-actions-can-execute? action)
          (progn
            (arxana-vsatarcs-aif-edn-sync--execute action)
            (setq observed-post-state
                  (arxana-vsatarcs-aif-edn-sync--observe-post-state action))
            (setq prediction-error
                  (arxana-vsatarcs-aif-edn-sync--prediction-error
                   predicted observed-post-state))
            (setq executed (list :executed-at (arxana-vsatarcs-trace--now-iso)
                                 :executed-by 'arxana-vsatarcs-aif-edn-sync
                                 :action action)))
        (setq response
              (plist-put (copy-sequence response)
                         :rationale
                         (format "%s [can-execute? failed at execution time]"
                                 (or (plist-get response :rationale) ""))))))
    (let ((record (arxana-vsatarcs-writer-trace-build-record
                   :proposed-action action
                   :consent-request request
                   :consent-response response
                   :executed-action executed
                   :predicted-post-state (plist-get predicted :predicted-post-state)
                   :observed-post-state observed-post-state
                   :prediction-error prediction-error)))
      (when emit-trace-p (arxana-vsatarcs-writer-trace-emit record))
      record)))

;; ---------------------------------------------------------------------
;; Execute (mutates target .aif.edn)
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-aif-edn-sync--execute (action)
  "Apply the aif-edn-revision-entry edit for ACTION.
Inserts a new `{:rev ... :on ... :by ... :summary ...}' entry at the
top of the `:revisions' vec (reverse-chronological convention).  Saves
the file.  Does NOT regenerate VSATARCS.md / vsatarcs.html — a
post-execution hook would handle that (deferred until needed)."
  (let ((file (plist-get action :target-file))
        (rev (plist-get action :proposed-rev))
        (on (plist-get action :proposed-on))
        (by (plist-get action :proposed-by))
        (summary (plist-get action :proposed-summary)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      ;; Locate the :revisions block opening (the `[' on the line AFTER `:revisions')
      (unless (re-search-forward "^[[:space:]]+:revisions[[:space:]]*$" nil t)
        (error "Cannot find :revisions block in %s" file))
      ;; Advance to the opening `['
      (unless (re-search-forward "^[[:space:]]+\\[" nil t)
        (error "Cannot find :revisions block `[' opening in %s" file))
      ;; Place point just after the `[' (before the first entry's `{')
      (let ((entry (arxana-vsatarcs-aif-edn-sync--format-entry rev on by summary)))
        (insert entry))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun arxana-vsatarcs-aif-edn-sync--format-entry (rev on by summary)
  "Format a new :revisions entry as EDN-text, matching the existing
indentation convention (3 spaces from column 0; `{:rev' aligned).
Output ends with a newline before the next existing entry."
  (concat "{:rev \"" rev "\"\n"
          "    :on \"" on "\"\n"
          "    :by \"" by "\"\n"
          "    :summary " (prin1-to-string summary) "}\n   "))

;; ---------------------------------------------------------------------
;; Observe + prediction-error
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-aif-edn-sync--observe-post-state (action)
  "Re-scan the target file; return observed-post-state plist of same
shape as `predicted-post-state'."
  (let* ((file (plist-get action :target-file))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string))))
    (list :revisions-count
          (arxana-vsatarcs-writer-actions--count-revisions text)
          :last-rev
          (arxana-vsatarcs-writer-actions--last-revision text))))

(defun arxana-vsatarcs-aif-edn-sync--prediction-error (predicted observed)
  "Scalar prediction-error between PREDICTED and OBSERVED post-states.
Returns 0.0 for exact match on (revisions-count, last-rev); 1.0 otherwise.
Coarse L4 metric, same shape as mission-doc-sync's metric."
  (let* ((pred-post (plist-get predicted :predicted-post-state))
         (pred-count (plist-get pred-post :revisions-count))
         (pred-last (plist-get pred-post :last-rev))
         (obs-count (plist-get observed :revisions-count))
         (obs-last (plist-get observed :last-rev)))
    (if (and (equal pred-count obs-count)
             (equal pred-last obs-last))
        0.0
      1.0)))

(provide 'arxana-vsatarcs-aif-edn-sync)
;;; arxana-vsatarcs-aif-edn-sync.el ends here
