;;; arxana-vsatarcs-intrinsic-values-test.el --- Tests for R12 narrow-take-up (VSATARCs side) -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-intrinsic-values.el' (v0.5.31 R12 ship).
;; Covers:
;;
;;   - posterior-mode arithmetic (prior + posterior cases)
;;   - credit-for defaults to prior mode (0.5) for unknown class
;;   - apply-update! mutates the table; idempotent in the sense that
;;     reapplying the same record yields the same state
;;   - rehydrate! latest-per-class wins (the WM-side §6 conjugate
;;     latest-wins discipline)
;;   - record-from-hyperedge coerces string-keyed alist props correctly
;;   - next-record Beta arithmetic
;;   - EFE wiring (the consumption-site proof): updating the atom
;;     changes the EFE pragmatic for actions of that class.  Mirrors
;;     claude-9's `learn-actions-intrinsic-value-tracks-atom-test'.
;;   - Prior-identity property: at Beta(1,1) prior, EFE pragmatic is
;;     unchanged from the pre-v0.5.31 value (regression guard).

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-intrinsic-values)
(require 'arxana-vsatarcs-efe)

;; Test isolation: every test resets the atom + hydrated marker AND
;; binds the rehydrate-from-store! to a no-op so a missing futon1a
;; doesn't break the suite.
(defmacro arxana-vsatarcs-intrinsic-values-test--isolated (&rest body)
  "Reset atom + stub rehydrate-from-store; run BODY."
  (declare (indent 0))
  `(let ((arxana-vsatarcs-intrinsic-values--state nil)
         (arxana-vsatarcs-intrinsic-values--hydrated? t)) ; suppress lazy fetch
     ,@body))

;; ---------------------------------------------------------------------
;; posterior-mode + fresh-entry
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-intrinsic-values-prior-mode-is-0.5 ()
  (should (= 0.5 (arxana-vsatarcs-intrinsic-values--posterior-mode 1.0 1.0))))

(ert-deftest arxana-vsatarcs-intrinsic-values-posterior-mode-skews-with-evidence ()
  ;; Beta(5, 2) posterior mode = 4/5 = 0.8
  (should (< 0.79
             (arxana-vsatarcs-intrinsic-values--posterior-mode 5.0 2.0)))
  (should (> 0.81
             (arxana-vsatarcs-intrinsic-values--posterior-mode 5.0 2.0))))

(ert-deftest arxana-vsatarcs-intrinsic-values-fresh-entry-is-prior ()
  (let ((e (arxana-vsatarcs-intrinsic-values-fresh-entry)))
    (should (= 1.0 (plist-get e :alpha)))
    (should (= 1.0 (plist-get e :beta)))
    (should (= 0.5 (plist-get e :intrinsic-value)))))

;; ---------------------------------------------------------------------
;; credit-for + reset-to-prior!
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-intrinsic-values-credit-for-defaults-to-prior ()
  (arxana-vsatarcs-intrinsic-values-test--isolated
    (should (= 0.5
               (arxana-vsatarcs-intrinsic-values-credit-for :essay-revise)))))

(ert-deftest arxana-vsatarcs-intrinsic-values-credit-for-uses-table ()
  (arxana-vsatarcs-intrinsic-values-test--isolated
    (arxana-vsatarcs-intrinsic-values-apply-update!
     (list :class :essay-revise
           :as-of "2026-05-21T10:00:00+00:00"
           :alpha-post 5.0
           :beta-post 2.0
           :intrinsic-value-post 0.8
           :n-emissions-in-window 6
           :n-followthrough-in-window 5))
    (should (= 0.8
               (arxana-vsatarcs-intrinsic-values-credit-for :essay-revise)))
    ;; Untouched class still prior
    (should (= 0.5
               (arxana-vsatarcs-intrinsic-values-credit-for :story-update)))))

;; ---------------------------------------------------------------------
;; apply-update! + rehydrate! latest-wins
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-intrinsic-values-apply-update-overwrites-class ()
  (arxana-vsatarcs-intrinsic-values-test--isolated
    (arxana-vsatarcs-intrinsic-values-apply-update!
     (list :class :essay-revise
           :alpha-post 2.0 :beta-post 5.0 :intrinsic-value-post 0.2
           :as-of "2026-05-21T10:00:00+00:00"
           :n-emissions-in-window 5 :n-followthrough-in-window 1))
    (arxana-vsatarcs-intrinsic-values-apply-update!
     (list :class :essay-revise
           :alpha-post 5.0 :beta-post 2.0 :intrinsic-value-post 0.8
           :as-of "2026-05-21T11:00:00+00:00"
           :n-emissions-in-window 5 :n-followthrough-in-window 4))
    (should (= 0.8
               (arxana-vsatarcs-intrinsic-values-credit-for :essay-revise)))))

(ert-deftest arxana-vsatarcs-intrinsic-values-rehydrate-latest-per-class-wins ()
  (arxana-vsatarcs-intrinsic-values-test--isolated
    (arxana-vsatarcs-intrinsic-values-rehydrate!
     (list
      (list :class :essay-revise
            :alpha-post 2.0 :beta-post 5.0 :intrinsic-value-post 0.2
            :as-of "2026-05-21T08:00:00+00:00"
            :n-emissions-in-window 5 :n-followthrough-in-window 1)
      (list :class :essay-revise
            :alpha-post 5.0 :beta-post 2.0 :intrinsic-value-post 0.8
            :as-of "2026-05-21T11:00:00+00:00" ; later
            :n-emissions-in-window 5 :n-followthrough-in-window 4)
      (list :class :story-update
            :alpha-post 3.0 :beta-post 1.0 :intrinsic-value-post 0.667
            :as-of "2026-05-21T09:00:00+00:00"
            :n-emissions-in-window 2 :n-followthrough-in-window 2)))
    (should (= 0.8
               (arxana-vsatarcs-intrinsic-values-credit-for :essay-revise)))
    (should (= 0.667
               (arxana-vsatarcs-intrinsic-values-credit-for :story-update)))))

;; ---------------------------------------------------------------------
;; record-from-hyperedge: string-keyed-alist props coercion
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-intrinsic-values-record-from-hyperedge-string-keys ()
  (let* ((hx (list (cons 'hx/type "code/v05/vsatarcs-hyperparameter-update")
                   (cons 'hx/props
                         (list (cons "class" "essay-revise")
                               (cons "as-of" "2026-05-21T11:00:00+00:00")
                               (cons "alpha-post" 5.0)
                               (cons "beta-post" 2.0)
                               (cons "intrinsic-value-post" 0.8)
                               (cons "n-emissions-in-window" 5)
                               (cons "n-followthrough-in-window" 4)))))
         (r (arxana-vsatarcs-intrinsic-values--record-from-hyperedge hx)))
    (should (eq :essay-revise (plist-get r :class)))
    (should (= 5.0 (plist-get r :alpha-post)))
    (should (= 0.8 (plist-get r :intrinsic-value-post)))))

(ert-deftest arxana-vsatarcs-intrinsic-values-record-from-hyperedge-rejects-malformed ()
  (let* ((hx (list (cons 'hx/type "code/v05/vsatarcs-hyperparameter-update")
                   (cons 'hx/props
                         (list (cons "class" "essay-revise")
                               ;; missing alpha-post / beta-post
                               (cons "as-of" "2026-05-21T11:00:00+00:00"))))))
    (should (null
             (arxana-vsatarcs-intrinsic-values--record-from-hyperedge hx)))))

;; ---------------------------------------------------------------------
;; next-record Beta arithmetic
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-intrinsic-values-next-record-beta-update ()
  (let* ((prior (arxana-vsatarcs-intrinsic-values-fresh-entry))
         (r (arxana-vsatarcs-intrinsic-values-next-record
             :essay-revise prior 5 4
             :as-of "2026-05-21T12:00:00+00:00"
             :window-days 14)))
    ;; Prior Beta(1,1) + 4 followthrough = Beta(5, 1+1) = Beta(5, 2)
    (should (= 5.0 (plist-get r :alpha-post)))
    (should (= 2.0 (plist-get r :beta-post)))
    ;; Posterior mode = (5-1)/(5+2-2) = 4/5 = 0.8
    (should (< 0.79 (plist-get r :intrinsic-value-post)))
    (should (> 0.81 (plist-get r :intrinsic-value-post)))))

;; ---------------------------------------------------------------------
;; EFE wiring — the consumption-site proof.  Mirrors claude-9's
;; `learn-actions-intrinsic-value-tracks-atom-test'.
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-intrinsic-values-efe-pragmatic-tracks-atom ()
  "Updating the intrinsic-value atom for :essay-revise must change the
EFE pragmatic for an :essay-revise action.  This is the v0.5.31
consumption-site wiring proof — claude-9's WM-side analogue is the
`action_proposer.clj:39' one-line wiring + its accompanying test."
  (arxana-vsatarcs-intrinsic-values-test--isolated
    ;; Stub the static pragmatic so the test is deterministic + independent
    ;; of any real essay file.  Static value = -1.0 (non-zero so the
    ;; multiplier's effect is visible).
    (cl-letf (((symbol-function 'arxana-vsatarcs-efe--g-pragmatic-static)
               (lambda (_action) -1.0)))
      (let ((action (list :type :essay-revise
                          :target-file "/tmp/fake.html")))
        ;; At Beta(1,1) prior credit=0.5, multiplier=1.0, pragmatic = -1.0
        (should (= -1.0
                   (arxana-vsatarcs-efe--g-pragmatic action)))
        ;; Boost credit to 1.0; multiplier = 2.0; pragmatic = -2.0
        (arxana-vsatarcs-intrinsic-values-apply-update!
         (list :class :essay-revise
               :alpha-post 100.0 :beta-post 1.0 :intrinsic-value-post 1.0
               :as-of "2026-05-21T12:00:00+00:00"
               :n-emissions-in-window 99 :n-followthrough-in-window 99))
        (should (= -2.0
                   (arxana-vsatarcs-efe--g-pragmatic action)))
        ;; Collapse credit to 0.0; multiplier = 0.0; pragmatic = 0.0
        (arxana-vsatarcs-intrinsic-values-apply-update!
         (list :class :essay-revise
               :alpha-post 1.0 :beta-post 100.0 :intrinsic-value-post 0.0
               :as-of "2026-05-21T13:00:00+00:00"
               :n-emissions-in-window 99 :n-followthrough-in-window 0))
        (should (= 0.0
                   (arxana-vsatarcs-efe--g-pragmatic action)))))))

(ert-deftest arxana-vsatarcs-intrinsic-values-efe-prior-identity-regression ()
  "Beta(1,1) prior is IDENTITY: EFE pragmatic at empty atom matches the
v0.5.30 static value.  Regression guard so a future refactor that drops
the (credit / 0.5) prior-identity property gets caught immediately."
  (arxana-vsatarcs-intrinsic-values-test--isolated
    (cl-letf (((symbol-function 'arxana-vsatarcs-efe--g-pragmatic-static)
               (lambda (_action) -0.7)))
      (let ((action (list :type :mission-doc-sync
                          :target-file "/tmp/fake.md")))
        (should (= -0.7
                   (arxana-vsatarcs-efe--g-pragmatic action)))))))

(provide 'arxana-vsatarcs-intrinsic-values-test)
;;; arxana-vsatarcs-intrinsic-values-test.el ends here
