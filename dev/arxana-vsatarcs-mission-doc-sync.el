;;; arxana-vsatarcs-mission-doc-sync.el --- :mission-doc-sync executor -*- lexical-binding: t; -*-

;;; Commentary:
;; Executor for the `:mission-doc-sync' writer-action-class — the L3
;; initial class per D5.  Applies a checkpoint-complete marker to a
;; target mission scaffold doc, observes the post-state via cluster.el,
;; computes prediction-error, and emits a writer-event trace record.
;;
;; Pipeline contract (per the DERIVE wiring sketch):
;;
;;   propose-action  →  predict-effects  →  build-consent-request
;;     →  consent-gate(:operator|:autopen|:cached-policy)
;;     →  branch on response:
;;          :confirm → execute-edit → observe-post-state →
;;                     compute-prediction-error → emit-writer-event
;;          :reject  → emit-writer-event(no execution)
;;          :ignore  → emit-writer-event(no execution)
;;          :abstain-for-now → no trace yet; re-queue (out of scope L3)
;;
;; This module IS the end-to-end glue for L3 demo.

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-cluster)
(require 'arxana-vsatarcs-writer-actions)
(require 'arxana-vsatarcs-consent)
(require 'arxana-vsatarcs-writer-trace)

(defgroup arxana-vsatarcs-mission-doc-sync nil
  "Executor for :mission-doc-sync writer-action-class."
  :group 'arxana-vsatarcs)

;; ---------------------------------------------------------------------
;; End-to-end cycle
;; ---------------------------------------------------------------------

(cl-defun arxana-vsatarcs-mission-doc-sync-cycle
    (action &key (consent-source :operator) emit-trace-p)
  "Run one end-to-end mission-doc-sync cycle for ACTION.
Returns a writer-event record (whether or not execution happened).
If EMIT-TRACE-P is non-nil, also appends the record to the daily trace.

CONSENT-SOURCE defaults to :operator (the L3 source).  Use :autopen
only for rules-rehearsal in tests."
  (cl-assert (eq :mission-doc-sync (plist-get action :type)))
  ;; Admissibility (per I5)
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
      (cl-return-from arxana-vsatarcs-mission-doc-sync-cycle record)))
  ;; Categorical abstain on :pivot (per D2 + I9)
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
      (cl-return-from arxana-vsatarcs-mission-doc-sync-cycle record)))
  ;; Forward-model prediction
  (let* ((predicted
          (arxana-vsatarcs-writer-actions-predict-effects action))
         (request (arxana-vsatarcs-consent-build-request
                   :proposed-action action
                   :rationale (plist-get action :rationale)
                   :predicted-effects predicted
                   :class :mission-doc-sync
                   :stakes :low
                   :reversibility :trivial
                   :match-type (plist-get action :match-type)))
         (response (arxana-vsatarcs-consent-gate request consent-source))
         (executed nil)
         (observed-post-state nil)
         (prediction-error nil))
    (when (eq :confirm (plist-get response :response))
      ;; Check can-execute? gate (I5 + I6)
      (if (arxana-vsatarcs-writer-actions-can-execute? action)
          (progn
            (arxana-vsatarcs-mission-doc-sync--execute action)
            (setq observed-post-state
                  (arxana-vsatarcs-mission-doc-sync--observe-post-state action))
            (setq prediction-error
                  (arxana-vsatarcs-mission-doc-sync--prediction-error
                   predicted observed-post-state))
            (setq executed (list :executed-at (arxana-vsatarcs-trace--now-iso)
                                 :executed-by 'arxana-vsatarcs-mission-doc-sync
                                 :action action)))
        ;; can-execute? failed even though consent was :confirm; record honest failure
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
;; Execute (mutates target file)
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-mission-doc-sync--execute (action)
  "Apply the mission-doc-sync edit for ACTION.
Opens the target file, inserts the proposed-status-marker below the
target-checkpoint heading, saves.  Does NOT git-commit — operator (or
later, an autopen-with-commit rule) handles version control."
  (let ((file (plist-get action :target-file))
        (checkpoint (plist-get action :target-checkpoint))
        (marker (plist-get action :proposed-status-marker)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (unless (re-search-forward
               (concat "^### " (regexp-quote checkpoint) "[[:space:]]*$")
               nil t)
        (error "Cannot find target checkpoint heading in %s: %s"
               file checkpoint))
      (forward-line 1)
      (insert marker "\n")
      (write-region (point-min) (point-max) file nil 'silent))))

;; ---------------------------------------------------------------------
;; Observe + prediction-error
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-mission-doc-sync--observe-post-state (action)
  "Re-parse the target file via cluster.el; return observed post-state plist.
Same shape as the :predicted-post-state field of `predict-effects' output."
  (let* ((file (plist-get action :target-file))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string))))
    (list :status (arxana-vsatarcs-cluster--extract-status-line text)
          :stage (arxana-vsatarcs-cluster--detect-stage text)
          :checkpoints (arxana-vsatarcs-cluster--count-checkpoints text))))

(defun arxana-vsatarcs-mission-doc-sync--prediction-error (predicted observed)
  "Compute a scalar prediction-error between PREDICTED post-state plist and OBSERVED.
For mission-doc-sync, key signal is whether the checkpoints :complete
count matches.  Returns 0.0 for exact match, 1.0 for mismatch — a
coarse but adequate L3 metric.  Per P4's scale-asymmetry practice
refinement, finer-grained metric is a calibration concern for later."
  (let* ((pred-post (plist-get predicted :predicted-post-state))
         (pred-cp (plist-get pred-post :checkpoints))
         (obs-cp (plist-get observed :checkpoints)))
    (if (and (equal (plist-get pred-cp :total) (plist-get obs-cp :total))
             (equal (plist-get pred-cp :complete) (plist-get obs-cp :complete)))
        0.0
      1.0)))

(provide 'arxana-vsatarcs-mission-doc-sync)
;;; arxana-vsatarcs-mission-doc-sync.el ends here
