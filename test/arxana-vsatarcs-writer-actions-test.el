;;; arxana-vsatarcs-writer-actions-test.el --- Tests for writer-actions R4 -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for `arxana-vsatarcs-writer-actions' — action constructor,
;; predict-effects, can-propose?, can-execute? for :mission-doc-sync.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-writer-actions)

;; ---------------------------------------------------------------------
;; Fixture helper
;; ---------------------------------------------------------------------

(defvar arxana-vsatarcs-writer-actions-test--scaffold
  "# Mission: M-test

**Status:** IDENTIFY (2026-05-20)
**Owner:** test-suite

## 1. IDENTIFY

Some text.

## 2. MAP

### Checkpoint 1 — initial scan

Some content.

### Checkpoint 2 — survey complete
**Status: COMPLETE** (2026-05-20)

Some content.

### Checkpoint 3 — second checkpoint

Open.
"
  "Synthetic mission-scaffold fixture with 3 checkpoints (1 complete).")

(defun arxana-vsatarcs-writer-actions-test--make-fixture ()
  "Write the fixture to a temp file; return the path."
  (let ((path (make-temp-file "arxana-vsatarcs-writer-fixture-" nil ".md")))
    (with-temp-buffer
      (insert arxana-vsatarcs-writer-actions-test--scaffold)
      (write-region (point-min) (point-max) path nil 'silent))
    path))

;; ---------------------------------------------------------------------
;; Constructor
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-writer-actions-test--constructor-mission-doc-sync ()
  "Constructor returns a plist with :type :mission-doc-sync."
  (let ((path (arxana-vsatarcs-writer-actions-test--make-fixture)))
    (unwind-protect
        (let ((a (arxana-vsatarcs-writer-action-make-mission-doc-sync
                  :target-file path
                  :target-checkpoint "Checkpoint 1 — initial scan"
                  :proposed-status-marker "**Status: COMPLETE** (2026-05-20)"
                  :match-type :clean
                  :source-closure-id "hx:test:v0-1:closure"
                  :rationale "test rationale")))
          (should (eq :mission-doc-sync (plist-get a :type)))
          (should (string= path (plist-get a :target-file)))
          (should (eq :clean (plist-get a :match-type))))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-writer-actions-test--constructor-rejects-bad-match-type ()
  "Constructor rejects unknown match-type."
  (should-error
   (arxana-vsatarcs-writer-action-make-mission-doc-sync
    :target-file "/tmp/x.md"
    :target-checkpoint "x"
    :proposed-status-marker "y"
    :match-type :bogus
    :source-closure-id "z"
    :rationale "w")))

;; ---------------------------------------------------------------------
;; predict-effects
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-writer-actions-test--predict-effects-checkpoints-delta ()
  "Predicted post-state increments completed-checkpoints by 1 for a fresh checkpoint."
  (let ((path (arxana-vsatarcs-writer-actions-test--make-fixture)))
    (unwind-protect
        (let* ((a (arxana-vsatarcs-writer-action-make-mission-doc-sync
                   :target-file path
                   :target-checkpoint "Checkpoint 1 — initial scan"
                   :proposed-status-marker "**Status: COMPLETE** (2026-05-20)"
                   :match-type :clean
                   :source-closure-id "hx:test:v0-1:closure"
                   :rationale "test"))
               (predicted (arxana-vsatarcs-writer-actions-predict-effects a))
               (pre (plist-get predicted :pre-state))
               (post (plist-get predicted :predicted-post-state))
               (delta (plist-get predicted :predicted-delta)))
          (should (= 3 (plist-get (plist-get pre :checkpoints) :total)))
          (should (= 1 (plist-get (plist-get pre :checkpoints) :complete)))
          (should (= 3 (plist-get (plist-get post :checkpoints) :total)))
          (should (= 2 (plist-get (plist-get post :checkpoints) :complete)))
          (should (= 1 (plist-get delta :checkpoints-complete-delta))))
      (delete-file path))))

;; ---------------------------------------------------------------------
;; can-propose?
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-writer-actions-test--can-propose-fresh-checkpoint ()
  "can-propose? returns non-nil for a fresh checkpoint."
  (let ((path (arxana-vsatarcs-writer-actions-test--make-fixture)))
    (unwind-protect
        (let ((a (arxana-vsatarcs-writer-action-make-mission-doc-sync
                  :target-file path
                  :target-checkpoint "Checkpoint 1 — initial scan"
                  :proposed-status-marker "**Status: COMPLETE** (2026-05-20)"
                  :match-type :clean
                  :source-closure-id "hx:test:v0-1:closure"
                  :rationale "test")))
          (should (arxana-vsatarcs-writer-actions-can-propose? a)))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-writer-actions-test--can-propose-rejects-already-complete ()
  "can-propose? returns nil for a checkpoint already marked complete."
  (let ((path (arxana-vsatarcs-writer-actions-test--make-fixture)))
    (unwind-protect
        (let ((a (arxana-vsatarcs-writer-action-make-mission-doc-sync
                  :target-file path
                  :target-checkpoint "Checkpoint 2 — survey complete"
                  :proposed-status-marker "**Status: COMPLETE** (2026-05-20)"
                  :match-type :clean
                  :source-closure-id "hx:test:v0-1:closure"
                  :rationale "test")))
          (should-not (arxana-vsatarcs-writer-actions-can-propose? a)))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-writer-actions-test--can-propose-rejects-missing-file ()
  "can-propose? returns nil when the target file doesn't exist."
  (let ((a (arxana-vsatarcs-writer-action-make-mission-doc-sync
            :target-file "/tmp/nonexistent-fixture-xyzzy.md"
            :target-checkpoint "Checkpoint 1"
            :proposed-status-marker "**Status: COMPLETE**"
            :match-type :clean
            :source-closure-id "hx:test:v0-1:closure"
            :rationale "test")))
    (should-not (arxana-vsatarcs-writer-actions-can-propose? a))))

(provide 'arxana-vsatarcs-writer-actions-test)
;;; arxana-vsatarcs-writer-actions-test.el ends here
