;;; arxana-vsatarcs-story-update-test.el --- L4+ :story-update cycle tests -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for the third self-documentation action-class (Option B
;; per Joe 2026-05-20).  Same cycle contract as mission-doc-sync and
;; aif-edn-sync; targets FUTON story leaves (`.md').  Includes the
;; recursion-safety test per the safety-property family claude-4
;; articulated in v0.5.22.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-story-update)
(require 'arxana-vsatarcs-writer-actions)
(require 'arxana-vsatarcs-writer-trace)
(require 'arxana-vsatarcs-consent)

;; ---------------------------------------------------------------------
;; Fixture
;; ---------------------------------------------------------------------

(defvar arxana-vsatarcs-story-update-test--fixture
  "# Test Story

**Leaf:** test (IDENTIFY condition)
**Distinctive words:** test, fixture
**Size:** 2 members

---

## Scene: Overview | overview

*(opening scene)*

This is the overview scene.

[Member A](member-a) · [Member B](member-b)

---

## Scene: Member A | member-a

First member content.

---

## Scene: Member B | member-b

Second member content.

---

*Size-2 leaf footer text.*
"
  "Synthetic story fixture with 3 scenes + trailing italic footer.")

(defun arxana-vsatarcs-story-update-test--make-fixture ()
  (let ((path (make-temp-file "arxana-vsatarcs-story-" nil ".md")))
    (with-temp-buffer
      (insert arxana-vsatarcs-story-update-test--fixture)
      (write-region (point-min) (point-max) path nil 'silent))
    path))

(defmacro arxana-vsatarcs-story-update-test--with-mocked-execute (&rest body)
  "Run BODY with `--git-clean-p' mocked to t (skip I6 in tests)."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'arxana-vsatarcs-writer-actions--git-clean-p)
              (lambda (_file) t)))
     ,@body))

;; ---------------------------------------------------------------------
;; Constructor + predict-effects + can-propose
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-story-update-test--constructor ()
  (let ((path (arxana-vsatarcs-story-update-test--make-fixture)))
    (unwind-protect
        (let ((a (arxana-vsatarcs-writer-action-make-story-update
                  :target-file path
                  :scene-slug "new-scene"
                  :scene-name "New Scene"
                  :scene-content "Body."
                  :match-type :clean
                  :source-evidence-id "hx:test:story:1"
                  :rationale "test")))
          (should (eq :story-update (plist-get a :type)))
          (should (string= "new-scene" (plist-get a :scene-slug))))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-story-update-test--predict-effects-count-delta ()
  (let ((path (arxana-vsatarcs-story-update-test--make-fixture)))
    (unwind-protect
        (let* ((a (arxana-vsatarcs-writer-action-make-story-update
                   :target-file path
                   :scene-slug "scoop"
                   :scene-name "Latest Scoop"
                   :scene-content "Body of the new scene."
                   :match-type :clean
                   :source-evidence-id "hx:test:story:2"
                   :rationale "test"))
               (predicted (arxana-vsatarcs-writer-actions-predict-effects a))
               (pre (plist-get predicted :pre-state))
               (post (plist-get predicted :predicted-post-state)))
          (should (= 3 (plist-get pre :scenes-count)))
          (should (string= "member-b" (plist-get pre :last-slug)))
          (should (= 4 (plist-get post :scenes-count)))
          (should (string= "scoop" (plist-get post :last-slug))))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-story-update-test--can-propose-rejects-duplicate-slug ()
  "Per v0.5.22 safety-property: duplicate scene-slug → can-propose? nil."
  (let ((path (arxana-vsatarcs-story-update-test--make-fixture)))
    (unwind-protect
        (let ((a (arxana-vsatarcs-writer-action-make-story-update
                  :target-file path
                  :scene-slug "overview" ; already in fixture
                  :scene-name "Overview Again"
                  :scene-content "dup"
                  :match-type :clean
                  :source-evidence-id "hx:test:story:dup"
                  :rationale "test")))
          (should-not (arxana-vsatarcs-writer-actions-can-propose? a)))
      (delete-file path))))

;; ---------------------------------------------------------------------
;; Full L4+ cycle
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-story-update-test--cycle-clean-autopen-confirms ()
  "L4+ end-to-end completion criterion: clean-match :autopen → confirm
→ execute → 0.0 prediction-error."
  (let ((path (arxana-vsatarcs-story-update-test--make-fixture)))
    (unwind-protect
        (arxana-vsatarcs-story-update-test--with-mocked-execute
         (let* ((action (arxana-vsatarcs-writer-action-make-story-update
                         :target-file path
                         :scene-slug "scoop"
                         :scene-name "Latest Scoop"
                         :scene-content "New scene body text."
                         :match-type :clean
                         :source-evidence-id "hx:test:story:cycle"
                         :rationale "L4+ cycle demo"))
                (record (arxana-vsatarcs-story-update-cycle
                         action :consent-source :autopen)))
           (should (eq :confirm
                       (plist-get (plist-get record :consent-response) :response)))
           (should (string= "story-update-clean-trivial"
                            (plist-get (plist-get record :consent-response)
                                       :source-id)))
           (should (plist-get record :executed-action))
           (should (= 0.0 (plist-get record :prediction-error)))
           (should (eq :story-update
                       (plist-get record :writer-action-class)))
           ;; Trailing italic footer is preserved
           (let ((post-text (with-temp-buffer
                              (insert-file-contents path)
                              (buffer-string))))
             (should (string-match-p
                      "\\*Size-2 leaf footer text\\.\\*" post-text))
             (should (string-match-p
                      "^## Scene: Latest Scoop | scoop$" post-text)))))
      (delete-file path))))

(ert-deftest arxana-vsatarcs-story-update-test--cycle-recursive-self-landing ()
  "Recursion-safety test (claude-4 v0.5.22 generalisation): a story-update
whose scene-content references the :story-update class. The first cycle
closes; re-proposing returns can-propose? → nil via duplicate-slug check."
  (let ((path (arxana-vsatarcs-story-update-test--make-fixture)))
    (unwind-protect
        (arxana-vsatarcs-story-update-test--with-mocked-execute
         (let* ((action (arxana-vsatarcs-writer-action-make-story-update
                         :target-file path
                         :scene-slug "story-update-class-self-landing"
                         :scene-name "Story-Update Class — Self Landing"
                         :scene-content
                         (concat
                          "The :story-update action-class lands its third\n"
                          "self-documentation sibling. This very scene was\n"
                          "appended by the :story-update executor against\n"
                          "this story as a recursion-safety witness — the\n"
                          "admissibility predicate's duplicate-slug check\n"
                          "would reject any re-application of the same action.")
                         :match-type :clean
                         :source-evidence-id "hx:test:story:recursive"
                         :rationale "Recursive self-landing per v0.5.22 safety-property"))
                (record (arxana-vsatarcs-story-update-cycle
                         action :consent-source :autopen)))
           (should (= 0.0 (plist-get record :prediction-error)))
           (should (plist-get record :executed-action))
           ;; Re-applying the same action against the now-modified fixture
           ;; fails can-propose? via the duplicate-slug check
           (should-not (arxana-vsatarcs-writer-actions-can-propose? action))))
      (delete-file path))))

(provide 'arxana-vsatarcs-story-update-test)
;;; arxana-vsatarcs-story-update-test.el ends here
