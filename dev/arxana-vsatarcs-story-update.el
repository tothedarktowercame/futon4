;;; arxana-vsatarcs-story-update.el --- :story-update executor (Option B) -*- lexical-binding: t; -*-

;;; Commentary:
;; Executor for the `:story-update' writer-action-class — the L4+
;; third self-documentation sibling per Joe 2026-05-20 ("Option B
;; live test").  Appends a new `## Scene:' block to a FUTON story
;; leaf.
;;
;; Per claude-4's v0.5.22 safety-property generalisation (recorded
;; in `vsatarcs-alignment-completeness.aif.edn'): admissibility
;; predicate is `proposed scene-slug not already present in target
;; story' — same recursion-safety mechanic as mission-doc-sync (heading
;; not already complete) and aif-edn-revision-entry (rev not already
;; in log).  The apparatus terminates self-application via the
;; admissibility predicate itself, no ad-hoc cycle-detector needed.
;;
;; Parallel-shape to `arxana-vsatarcs-mission-doc-sync.el' and
;; `arxana-vsatarcs-aif-edn-sync.el'.

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-writer-actions)
(require 'arxana-vsatarcs-consent)
(require 'arxana-vsatarcs-writer-trace)

(defgroup arxana-vsatarcs-story-update nil
  "Executor for :story-update writer-action-class."
  :group 'arxana-vsatarcs)

;; ---------------------------------------------------------------------
;; End-to-end cycle
;; ---------------------------------------------------------------------

(cl-defun arxana-vsatarcs-story-update-cycle
    (action &key (consent-source :operator) emit-trace-p)
  "Run one end-to-end story-update cycle for ACTION.
Returns a writer-event record.  See `arxana-vsatarcs-mission-doc-sync-cycle'
for the analogous shape on markdown checkpoint-status; `aif-edn-sync-cycle'
for the analogous shape on EDN revisions logs.

CONSENT-SOURCE defaults to :operator.  Use :autopen for rules-rehearsal
in tests + Option-B-live cycles where operator consent was given
out-of-band (e.g., via Joe's CLI conversation)."
  (cl-assert (eq :story-update (plist-get action :type)))
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
      (cl-return-from arxana-vsatarcs-story-update-cycle record)))
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
      (cl-return-from arxana-vsatarcs-story-update-cycle record)))
  ;; Forward-model prediction
  (let* ((predicted
          (arxana-vsatarcs-writer-actions-predict-effects action))
         (request (arxana-vsatarcs-consent-build-request
                   :proposed-action action
                   :rationale (plist-get action :rationale)
                   :predicted-effects predicted
                   :class :story-update
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
            (arxana-vsatarcs-story-update--execute action)
            (setq observed-post-state
                  (arxana-vsatarcs-story-update--observe-post-state action))
            (setq prediction-error
                  (arxana-vsatarcs-story-update--prediction-error
                   predicted observed-post-state))
            (setq executed (list :executed-at (arxana-vsatarcs-trace--now-iso)
                                 :executed-by 'arxana-vsatarcs-story-update
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
;; Execute (mutates target story file)
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-story-update--execute (action)
  "Apply the story-update edit for ACTION.
Appends a new Scene block at the end of the file, before any trailing
italic footer block.  Inserts the canonical FUTON-story Scene shape:

  ---

  ## Scene: <name> | <slug>

  <content>
"
  (let ((file (plist-get action :target-file))
        (slug (plist-get action :scene-slug))
        (name (plist-get action :scene-name))
        (content (plist-get action :scene-content)))
    (with-temp-buffer
      (insert-file-contents file)
      ;; Find the right insertion point: just before the trailing italic
      ;; footer if present (line starting with `*`), else at end-of-buffer.
      (goto-char (point-max))
      (when (re-search-backward "^\\*[^*]" nil t)
        ;; Found a trailing italic-footer line; insert before it.
        ;; Move back over any blank lines so we don't double-blank.
        (skip-chars-backward "\n")
        (forward-char 1))
      ;; If we didn't move (no trailing italic), we're at point-max; insert there.
      (let ((block (arxana-vsatarcs-story-update--format-scene name slug content)))
        (insert "\n" block))
      (write-region (point-min) (point-max) file nil 'silent))))

(defun arxana-vsatarcs-story-update--format-scene (name slug content)
  "Format a new Scene block as `---\\n## Scene: NAME | SLUG\\n\\nCONTENT\\n\\n'."
  (concat "---\n\n"
          "## Scene: " name " | " slug "\n\n"
          content
          (if (string-suffix-p "\n" content) "" "\n")
          "\n"))

;; ---------------------------------------------------------------------
;; Observe + prediction-error
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-story-update--observe-post-state (action)
  "Re-scan the target file; return observed-post-state plist of same
shape as `predicted-post-state'."
  (let* ((file (plist-get action :target-file))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string))))
    (list :scenes-count
          (arxana-vsatarcs-writer-actions--count-scenes text)
          :last-slug
          (arxana-vsatarcs-writer-actions--last-scene-slug text))))

(defun arxana-vsatarcs-story-update--prediction-error (predicted observed)
  "Scalar prediction-error between PREDICTED and OBSERVED.
Returns 0.0 for exact match on (scenes-count, last-slug); 1.0 otherwise.
Same shape as the other classes' prediction-error metric."
  (let* ((pred-post (plist-get predicted :predicted-post-state))
         (pred-count (plist-get pred-post :scenes-count))
         (pred-last (plist-get pred-post :last-slug))
         (obs-count (plist-get observed :scenes-count))
         (obs-last (plist-get observed :last-slug)))
    (if (and (equal pred-count obs-count)
             (equal pred-last obs-last))
        0.0
      1.0)))

(provide 'arxana-vsatarcs-story-update)
;;; arxana-vsatarcs-story-update.el ends here
