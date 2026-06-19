;;; arxana-vsatarcs-mission-doc-sync-test.el --- End-to-end mission-doc-sync L3 cycle -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests covering the full L3 end-to-end cycle: propose → predict →
;; consent (mocked via :autopen source for non-interactive testing) →
;; execute → observe → prediction-error → writer-event record.
;;
;; This file is the L3 completion-criteria evidence per IDENTIFY: "End-to-end
;; demo: VSATARCS proposes writer-action X via consent-gate; operator
;; confirms; X is executed; post-state matches forward-model prediction
;; (within epsilon). One full cycle minimum."  Tests run with :autopen
;; source (rule-matching version of operator-confirm) so they execute
;; non-interactively; the :operator source is exercised via manual
;; dogfooding cycles per VERIFY 5.b.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-mission-doc-sync)
(require 'arxana-vsatarcs-writer-actions)
(require 'arxana-vsatarcs-writer-trace)
(require 'arxana-vsatarcs-consent)

;; ---------------------------------------------------------------------
;; Fixture
;; ---------------------------------------------------------------------

(defvar arxana-vsatarcs-mission-doc-sync-test--scaffold
  "# Mission: M-test-cycle

**Status:** INSTANTIATE (test fixture)
**Owner:** test-suite

## 1. IDENTIFY

Open.

## 2. MAP

### Checkpoint A — initial scan

Open.

### Checkpoint B — bench complete
**Status: COMPLETE** (test)

Done.

### Checkpoint C — third checkpoint

Open.
"
  "Synthetic scaffold for end-to-end cycle tests.")

(defun arxana-vsatarcs-mission-doc-sync-test--make-fixture ()
  "Write the fixture; return the path."
  (let ((path (make-temp-file "arxana-vsatarcs-cycle-" nil ".md")))
    (with-temp-buffer
      (insert arxana-vsatarcs-mission-doc-sync-test--scaffold)
      (write-region (point-min) (point-max) path nil 'silent))
    path))

(defmacro arxana-vsatarcs-mission-doc-sync-test--with-mocked-execute (&rest body)
  "Run BODY with `--git-clean-p' mocked to always return t.
This skips the I6 git-clean precondition for tests against temp
fixtures (which are outside a git worktree)."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'arxana-vsatarcs-writer-actions--git-clean-p)
              (lambda (_file) t)))
     ,@body))

;; ---------------------------------------------------------------------
;; Tests: full L3 cycle with :autopen source
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-mission-doc-sync-test--cycle-clean-autopen-confirms ()
  "Clean-match action with :autopen source → :confirm → execute → 0.0 error.
This is the L3 end-to-end completion-criteria evidence (one full cycle)."
  (let ((path (arxana-vsatarcs-mission-doc-sync-test--make-fixture)))
    (unwind-protect
        (arxana-vsatarcs-mission-doc-sync-test--with-mocked-execute
         (let* ((action (arxana-vsatarcs-writer-action-make-mission-doc-sync
                         :target-file path
                         :target-checkpoint "Checkpoint A — initial scan"
                         :proposed-status-marker
                         "**Status: COMPLETE** (test cycle)"
                         :match-type :clean
                         :source-closure-id "hx:test:v0-1:closure"
                         :rationale "L3 demo: closure landed; checkpoint A documents it"))
                (record (arxana-vsatarcs-mission-doc-sync-cycle
                         action :consent-source :autopen)))
           ;; Consent path
           (should (eq :confirm (plist-get (plist-get record :consent-response)
                                           :response)))
           (should (eq :autopen (plist-get (plist-get record :consent-response)
                                           :source)))
           ;; Execution path
           (should (plist-get record :executed-action))
           ;; Post-state observed
           (should (plist-get record :observed-post-state))
           ;; Prediction error: 0.0 (predicted matches observed)
           (should (= 0.0 (plist-get record :prediction-error)))
           ;; Trace-kind correct
           (should (eq :writer-event (plist-get record :trace-kind)))
           ;; Action-class echoes back
           (should (eq :mission-doc-sync
                       (plist-get record :writer-action-class)))))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-mission-doc-sync-test--cycle-pivot-abstains-without-consent ()
  "Match-type :pivot triggers abstain WITHOUT emitting a consent-request.
Per I9 (R-A2): pivot is the categorical abstain trigger."
  (let ((path (arxana-vsatarcs-mission-doc-sync-test--make-fixture)))
    (unwind-protect
        (arxana-vsatarcs-mission-doc-sync-test--with-mocked-execute
         (let* ((action (arxana-vsatarcs-writer-action-make-mission-doc-sync
                         :target-file path
                         :target-checkpoint "Checkpoint A — initial scan"
                         :proposed-status-marker "**Status: COMPLETE** (pivot test)"
                         :match-type :pivot
                         :source-closure-id "hx:test:v0-1:pivot"
                         :rationale "test pivot case"))
                (record (arxana-vsatarcs-mission-doc-sync-cycle
                         action :consent-source :autopen)))
           ;; No consent-request was emitted at all
           (should-not (plist-get record :consent-request))
           ;; Response is abstain from the policy layer
           (should (eq :abstain-for-now
                       (plist-get (plist-get record :consent-response) :response)))
           (should (string= "I9-pivot-abstain"
                            (plist-get (plist-get record :consent-response)
                                       :source-id)))
           ;; Nothing executed
           (should-not (plist-get record :executed-action))))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-mission-doc-sync-test--cycle-scope-creep-without-autopen-falls-through ()
  "Match-type :scope-creep with :autopen source → no rule matches → response nil.
Demonstrates D6's autopen-eligibility-is-opt-in: scope-creep is NOT in
the registry, so :autopen falls through; the response is nil which we
treat as fall-through-to-operator (operator path is non-interactive in tests)."
  (let ((path (arxana-vsatarcs-mission-doc-sync-test--make-fixture)))
    (unwind-protect
        (arxana-vsatarcs-mission-doc-sync-test--with-mocked-execute
         (let* ((action (arxana-vsatarcs-writer-action-make-mission-doc-sync
                         :target-file path
                         :target-checkpoint "Checkpoint C — third checkpoint"
                         :proposed-status-marker "**Status: COMPLETE** (creep test)"
                         :match-type :scope-creep
                         :source-closure-id "hx:test:v0-1:creep"
                         :rationale "test scope-creep case"))
                (request (arxana-vsatarcs-consent-build-request
                          :proposed-action action
                          :match-type :scope-creep
                          :reversibility :trivial
                          :class :mission-doc-sync
                          :stakes :low
                          :rationale (plist-get action :rationale)))
                (response (arxana-vsatarcs-consent-gate request :autopen)))
           ;; Autopen registry has no scope-creep rule → nil response
           (should-not response)))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-mission-doc-sync-test--trace-record-shape ()
  "Writer-event record from a successful cycle carries all expected fields."
  (let ((path (arxana-vsatarcs-mission-doc-sync-test--make-fixture)))
    (unwind-protect
        (arxana-vsatarcs-mission-doc-sync-test--with-mocked-execute
         (let* ((action (arxana-vsatarcs-writer-action-make-mission-doc-sync
                         :target-file path
                         :target-checkpoint "Checkpoint A — initial scan"
                         :proposed-status-marker "**Status: COMPLETE** (shape test)"
                         :match-type :clean
                         :source-closure-id "hx:test:v0-1:shape"
                         :rationale "test record shape"))
                (record (arxana-vsatarcs-mission-doc-sync-cycle
                         action :consent-source :autopen)))
           (dolist (key '(:writer-event-id :timestamp :writer-action-class
                          :proposed-action :consent-request :consent-response
                          :executed-action :predicted-post-state
                          :observed-post-state :prediction-error :trace-kind))
             (should (plist-member record key)))))
      (delete-file path))))

(provide 'arxana-vsatarcs-mission-doc-sync-test)
;;; arxana-vsatarcs-mission-doc-sync-test.el ends here
