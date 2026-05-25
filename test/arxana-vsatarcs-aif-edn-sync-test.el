;;; arxana-vsatarcs-aif-edn-sync-test.el --- L4 end-to-end aif-edn-revision-entry cycle -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the L4 second self-documentation action-class.  Same
;; cycle contract as mission-doc-sync; targets .aif.edn :revisions logs
;; instead of mission-scaffold markdown.  Includes the **recursive
;; self-landing test** claude-4 flagged in v0.5.20: an
;; :aif-edn-revision-entry consent-request entry IS itself a candidate
;; for :aif-edn-revision-entry, so cycling against a fixture that
;; documents an aif-edn-revision-entry landing exercises that recursion.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-aif-edn-sync)
(require 'arxana-vsatarcs-writer-actions)
(require 'arxana-vsatarcs-writer-trace)
(require 'arxana-vsatarcs-consent)

;; ---------------------------------------------------------------------
;; Fixture
;; ---------------------------------------------------------------------

(defvar arxana-vsatarcs-aif-edn-sync-test--fixture
  ";; test fixture .aif.edn — mirrors real file's :provenance :revisions shape
{:annotations []
 :provenance
 {:authored-by \"test-suite\"
  :authored-on \"2026-05-20\"
  :revisions
  [{:rev \"v0.5.20\"
    :on \"2026-05-20\"
    :by \"claude-4\"
    :summary \"latest entry\"}
   {:rev \"v0.5.19\"
    :on \"2026-05-19\"
    :by \"claude-4\"
    :summary \"prior entry\"}
   {:rev \"v0.5.18\"
    :on \"2026-05-18\"
    :by \"claude-4\"
    :summary \"older entry\"}]}}
"
  "Minimal .aif.edn fixture with 3 revision entries, mirroring real
file's :provenance :revisions block indentation.")

(defun arxana-vsatarcs-aif-edn-sync-test--make-fixture ()
  "Write fixture; return path."
  (let ((path (make-temp-file "arxana-vsatarcs-aif-edn-fixture-" nil ".edn")))
    (with-temp-buffer
      (insert arxana-vsatarcs-aif-edn-sync-test--fixture)
      (write-region (point-min) (point-max) path nil 'silent))
    path))

(defmacro arxana-vsatarcs-aif-edn-sync-test--with-mocked-execute (&rest body)
  "Run BODY with `--git-clean-p' mocked to t (skip I6 in tests)."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'arxana-vsatarcs-writer-actions--git-clean-p)
              (lambda (_file) t)))
     ,@body))

;; ---------------------------------------------------------------------
;; Constructor + predict-effects + can-propose
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-aif-edn-sync-test--constructor ()
  "Constructor returns a plist with :type :aif-edn-revision-entry."
  (let ((path (arxana-vsatarcs-aif-edn-sync-test--make-fixture)))
    (unwind-protect
        (let ((a (arxana-vsatarcs-writer-action-make-aif-edn-revision-entry
                  :target-file path
                  :proposed-rev "v0.5.21"
                  :proposed-on "2026-05-20"
                  :proposed-by "claude-2"
                  :proposed-summary "L4 first ship"
                  :match-type :clean
                  :source-closure-id "hx:test:l4:closure"
                  :rationale "test")))
          (should (eq :aif-edn-revision-entry (plist-get a :type)))
          (should (string= "v0.5.21" (plist-get a :proposed-rev))))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-aif-edn-sync-test--predict-effects-count-delta ()
  "Predicted post-state increments :revisions-count by 1; last-rev updates."
  (let ((path (arxana-vsatarcs-aif-edn-sync-test--make-fixture)))
    (unwind-protect
        (let* ((a (arxana-vsatarcs-writer-action-make-aif-edn-revision-entry
                   :target-file path
                   :proposed-rev "v0.5.21"
                   :proposed-on "2026-05-20"
                   :proposed-by "claude-2"
                   :proposed-summary "L4 ship"
                   :match-type :clean
                   :source-closure-id "hx:test:l4:closure"
                   :rationale "test"))
               (predicted (arxana-vsatarcs-writer-actions-predict-effects a))
               (pre (plist-get predicted :pre-state))
               (post (plist-get predicted :predicted-post-state)))
          (should (= 3 (plist-get pre :revisions-count)))
          (should (string= "v0.5.20" (plist-get pre :last-rev)))
          (should (= 4 (plist-get post :revisions-count)))
          (should (string= "v0.5.21" (plist-get post :last-rev))))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-aif-edn-sync-test--can-propose-rejects-duplicate-rev ()
  "can-propose? returns nil when proposed-rev already exists in the log."
  (let ((path (arxana-vsatarcs-aif-edn-sync-test--make-fixture)))
    (unwind-protect
        (let ((a (arxana-vsatarcs-writer-action-make-aif-edn-revision-entry
                  :target-file path
                  :proposed-rev "v0.5.20" ; already in fixture
                  :proposed-on "2026-05-20"
                  :proposed-by "claude-2"
                  :proposed-summary "dup test"
                  :match-type :clean
                  :source-closure-id "hx:test:l4:dup"
                  :rationale "test")))
          (should-not (arxana-vsatarcs-writer-actions-can-propose? a)))
      (delete-file path))))

;; ---------------------------------------------------------------------
;; Full L4 cycle
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-aif-edn-sync-test--cycle-clean-autopen-confirms ()
  "L4 end-to-end completion criterion: clean-match via :autopen → confirm →
execute → 0.0 prediction-error. Mirrors the L3 cycle-clean-autopen-confirms
test but on the :aif-edn-revision-entry surface."
  (let ((path (arxana-vsatarcs-aif-edn-sync-test--make-fixture)))
    (unwind-protect
        (arxana-vsatarcs-aif-edn-sync-test--with-mocked-execute
         (let* ((action (arxana-vsatarcs-writer-action-make-aif-edn-revision-entry
                         :target-file path
                         :proposed-rev "v0.5.21"
                         :proposed-on "2026-05-20"
                         :proposed-by "claude-2"
                         :proposed-summary "L4 cycle demo: aif-edn-revision-entry shipped"
                         :match-type :clean
                         :source-closure-id "hx:test:l4:cycle"
                         :rationale "L4 demo: aif-edn-revision-entry first cycle"))
                (record (arxana-vsatarcs-aif-edn-sync-cycle
                         action :consent-source :autopen)))
           (should (eq :confirm (plist-get (plist-get record :consent-response)
                                           :response)))
           (should (eq :autopen (plist-get (plist-get record :consent-response)
                                           :source)))
           (should (string=
                    "aif-edn-revision-entry-clean-trivial"
                    (plist-get (plist-get record :consent-response) :source-id)))
           (should (plist-get record :executed-action))
           (should (= 0.0 (plist-get record :prediction-error)))
           (should (eq :aif-edn-revision-entry
                       (plist-get record :writer-action-class)))))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-aif-edn-sync-test--cycle-pivot-abstains ()
  "Pivot match-type triggers abstain without consent-request (I9)."
  (let ((path (arxana-vsatarcs-aif-edn-sync-test--make-fixture)))
    (unwind-protect
        (arxana-vsatarcs-aif-edn-sync-test--with-mocked-execute
         (let* ((action (arxana-vsatarcs-writer-action-make-aif-edn-revision-entry
                         :target-file path
                         :proposed-rev "v0.5.21"
                         :proposed-on "2026-05-20"
                         :proposed-by "claude-2"
                         :proposed-summary "pivot test"
                         :match-type :pivot
                         :source-closure-id "hx:test:l4:pivot"
                         :rationale "pivot"))
                (record (arxana-vsatarcs-aif-edn-sync-cycle
                         action :consent-source :autopen)))
           (should-not (plist-get record :consent-request))
           (should (eq :abstain-for-now
                       (plist-get (plist-get record :consent-response) :response)))
           (should-not (plist-get record :executed-action))))
      (delete-file path))))

;; ---------------------------------------------------------------------
;; **Recursive self-landing test** (per claude-4 v0.5.20 flag)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-aif-edn-sync-test--cycle-recursive-self-landing ()
  "Recursive shape: the consent-request entry for an :aif-edn-revision-entry
landing is ITSELF an :aif-edn-revision-entry candidate.

The summary text of the entry being proposed names the action-class
under question (\"L4 ship: aif-edn-revision-entry self-documented\").
After execution, the post-state contains a revision entry whose summary
references the very class that landed it.  The cycle CLOSES correctly
because (a) the proposed-rev (\"v0.5.21\") is novel against the fixture,
and (b) the prediction-error metric checks revisions-count + last-rev,
both of which are post-state-honest regardless of summary content.

This test answers claude-4's bell (2026-05-20 v0.5.20 forward-pointer):
'the consent-request entry IS itself an :aif-edn-revision-entry
candidate when v0.5.20 lands recursively — worth a test in their L4
verification.'"
  (let ((path (arxana-vsatarcs-aif-edn-sync-test--make-fixture)))
    (unwind-protect
        (arxana-vsatarcs-aif-edn-sync-test--with-mocked-execute
         (let* ((action (arxana-vsatarcs-writer-action-make-aif-edn-revision-entry
                         :target-file path
                         :proposed-rev "v0.5.21"
                         :proposed-on "2026-05-20"
                         :proposed-by "claude-2"
                         :proposed-summary
                         (concat
                          "L4 :aif-edn-revision-entry class lands. "
                          "The very entry you are reading was proposed by the "
                          ":aif-edn-revision-entry executor against this .aif.edn "
                          "as its first cycle. Recursive shape preserved per "
                          "M-vsatarcs-writer §6.f checkpoint; cross-side bilateral "
                          "with claude-4 to activate 6th :evidence-kind "
                          ":consent-gated-writer-event on next paired closure.")
                         :match-type :clean
                         :source-closure-id "hx:test:l4:recursive-self"
                         :rationale "Recursive self-landing test per claude-4 v0.5.20 flag"))
                (record (arxana-vsatarcs-aif-edn-sync-cycle
                         action :consent-source :autopen)))
           ;; Cycle closes normally
           (should (eq :confirm (plist-get (plist-get record :consent-response)
                                           :response)))
           (should (plist-get record :executed-action))
           (should (= 0.0 (plist-get record :prediction-error)))
           ;; Post-state: the new revision IS visible as the last-rev
           (let ((observed (plist-get record :observed-post-state)))
             (should (string= "v0.5.21" (plist-get observed :last-rev)))
             (should (= 4 (plist-get observed :revisions-count))))
           ;; Re-proposing the SAME action against the now-modified fixture
           ;; fails can-propose? — the recursion terminates correctly
           ;; (no infinite self-application; the duplicate-rev check is
           ;; the safety property that prevents runaway self-landing)
           (should-not (arxana-vsatarcs-writer-actions-can-propose? action))))
      (delete-file path))))

(provide 'arxana-vsatarcs-aif-edn-sync-test)
;;; arxana-vsatarcs-aif-edn-sync-test.el ends here
