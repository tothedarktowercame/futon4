;;; arxana-vsatarcs-r6-softmax-test.el --- Tests for R6 softmax+abstain -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-r6-softmax.el'.  Two categories:
;;
;;   PASSING (degenerate-mode + helpers):
;;     - 0-candidate abstain-empty
;;     - 1-candidate degenerate identity-select
;;     - 2+-candidate without G-totals returns :r5-pending
;;     - Direct softmax-weights math (R5 prerequisite math; available today)
;;     - Direct --full-softmax-select with synthetic G-totals (the R5
;;       wiring's downstream; tests pass once R5 attaches G-totals to
;;       proposer output)
;;
;;   EXPECTED-FAILURE (R5-pending, documenting what R5 closes):
;;     - End-to-end multi-candidate softmax via the public
;;       `arxana-vsatarcs-r6-softmax-select-action' API on candidates
;;       LACKING G-totals.  These tests assert the multi-candidate
;;       softmax mechanic; today they fail because the public API
;;       returns `:r5-pending' instead of running softmax.  When R5
;;       ships and the proposer attaches G-totals, the gate
;;       (`--has-g-scores?') flips, the public API routes to
;;       `--full-softmax-select', and these tests start passing —
;;       ert reports them as `unexpected pass' which is the
;;       operational signal that R5 closed the R6 gap.
;;
;; Pattern per Joe directive 2026-05-20: 'ship the degenerate
;; version first, put in failing tests, and show they are fixed when
;; we ship R5'.  The failing tests are documentation; their state
;; flip on R5 ship is the closure evidence.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-r6-softmax)

;; ---------------------------------------------------------------------
;; PASSING — degenerate + abstain-empty + r5-pending signal
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-r6-softmax-empty-candidates-abstains ()
  (let ((r (arxana-vsatarcs-r6-softmax-select-action '())))
    (should (null (plist-get r :action)))
    (should (eq :abstain-empty-candidate-set (plist-get r :mode)))
    (should (eq :no-candidates (plist-get r :gap-report)))))

(ert-deftest arxana-vsatarcs-r6-softmax-single-candidate-identity ()
  (let* ((c (list :type :mission-doc-sync :target "foo"))
         (r (arxana-vsatarcs-r6-softmax-select-action (list c))))
    (should (eq c (plist-get r :action)))
    (should (= 1 (plist-get r :rank)))
    (should (< (abs (- 1.0 (plist-get r :weight))) 1e-9))
    (should (eq :degenerate-single-candidate (plist-get r :mode)))
    (should (null (plist-get r :gap-report)))))

(ert-deftest arxana-vsatarcs-r6-softmax-r5-pending-helper-shape ()
  ;; v0.5.26 test (multi-without-G → :r5-pending via public API) was
  ;; rewritten in v0.5.27: the path is now unreachable via the public
  ;; API because R5's auto-enrichment always attaches numeric G-totals
  ;; (using 0.0 fallbacks for unknown classes / missing target-files).
  ;; The `--r5-pending' helper stays in the module as a defensive
  ;; fallback for the case where enrichment fails on every candidate
  ;; (no future case wired today; defensive only).  Test the helper
  ;; directly to guard its shape.
  (let* ((cs (list (list :type :unknown) (list :type :unknown)))
         (r (arxana-vsatarcs-r6-softmax--r5-pending cs)))
    (should (eq :r5-pending (plist-get r :mode)))
    (should (null (plist-get r :action)))
    (should (= 2 (plist-get (plist-get r :gap-report)
                            :r5-pending-candidates)))))

;; ---------------------------------------------------------------------
;; PASSING — pure helpers (R5 prerequisite math)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-r6-softmax-weights-sum-to-one ()
  (let ((w (arxana-vsatarcs-r6-softmax--softmax-weights
            '(-4.0 -3.5 -3.0) 0.16)))
    (should (< (abs (- 1.0 (apply #'+ w))) 1e-9))))

(ert-deftest arxana-vsatarcs-r6-softmax-weights-prefer-lower-g ()
  ;; Lower G should carry higher weight (WM convention: minimize G).
  (let ((w (arxana-vsatarcs-r6-softmax--softmax-weights
            '(-5.0 -3.0 -1.0) 0.16)))
    (should (> (nth 0 w) (nth 1 w)))
    (should (> (nth 1 w) (nth 2 w)))))

(ert-deftest arxana-vsatarcs-r6-softmax-weights-temperature-effect ()
  ;; Higher tau diffuses the distribution; lower tau sharpens.
  (let ((sharp (arxana-vsatarcs-r6-softmax--softmax-weights
                '(-5.0 -3.0 -1.0) 0.05))
        (diffuse (arxana-vsatarcs-r6-softmax--softmax-weights
                  '(-5.0 -3.0 -1.0) 2.0)))
    (should (> (car sharp) (car diffuse)))))

(ert-deftest arxana-vsatarcs-r6-softmax-full-with-synthetic-g-picks-top ()
  ;; Direct call to --full-softmax-select with synthetic G-totals on
  ;; the candidates picks the lowest-G (best) candidate.
  (let* ((cs (list (list :type :mission-doc-sync :target "a" :G-total -2.0)
                   (list :type :mission-doc-sync :target "b" :G-total -4.0)
                   (list :type :mission-doc-sync :target "c" :G-total -3.0)))
         (r (arxana-vsatarcs-r6-softmax--full-softmax-select cs)))
    (should (eq :softmax-multi-candidate (plist-get r :mode)))
    (should (= 1 (plist-get r :rank)))
    (should (equal "b"
                   (plist-get (plist-get r :action) :target)))))

(ert-deftest arxana-vsatarcs-r6-softmax-full-with-all-bad-abstains ()
  ;; Every candidate at or above the threshold → abstain.
  (let* ((arxana-vsatarcs-r6-softmax-abstain-g-threshold -5.0)
         (cs (list (list :type :mission-doc-sync :target "a" :G-total -6.0)
                   (list :type :mission-doc-sync :target "b" :G-total -7.0)))
         (r (arxana-vsatarcs-r6-softmax--full-softmax-select cs)))
    (should (eq :abstain-threshold (plist-get r :mode)))
    (should (null (plist-get r :action)))))

(ert-deftest arxana-vsatarcs-r6-softmax-has-g-scores-true-when-all-have-g ()
  (should (arxana-vsatarcs-r6-softmax--has-g-scores?
           (list (list :G-total -3.0) (list :G-total -2.0)))))

(ert-deftest arxana-vsatarcs-r6-softmax-has-g-scores-false-when-any-missing ()
  (should-not (arxana-vsatarcs-r6-softmax--has-g-scores?
               (list (list :G-total -3.0) (list)))))

;; ---------------------------------------------------------------------
;; Multi-candidate softmax via PUBLIC API
;;
;; These tests were marked `:expected-result :failed' in v0.5.26
;; (documenting the gap R5 would close).  v0.5.27 shipped R5 +
;; auto-enrichment in `arxana-vsatarcs-r6-softmax-select-action';
;; tests now pass naturally.  Per Joe directive's failing-tests-as-
;; documentation pattern: the flip from `:expected-result :failed' →
;; regular passes IS the operational closure evidence for R5 → R6-full.
;;
;; Fixtures use bare candidates (no :G-total); the public API
;; auto-enriches via `arxana-vsatarcs-efe-enrich-candidates' before
;; the softmax dispatch.  Tests use a tmp file as `:target-file' so
;; the EFE proxies have a readable target.
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-r6-softmax-multi-candidate-picks-best-via-public-api ()
  ;; Auto-enrichment via R5 (v0.5.27) routes the public API to
  ;; --full-softmax-select; bare candidates get :G-total attached.
  (let ((tmp (make-temp-file "vsatarcs-r6-pick-" nil ".tmp")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "x"))
          (let* ((cs (list (list :type :mission-doc-sync :target-file tmp)
                           (list :type :mission-doc-sync :target-file tmp)
                           (list :type :aif-edn-revision-entry
                                 :target-file tmp)))
                 (r (arxana-vsatarcs-r6-softmax-select-action cs)))
            (should (eq :softmax-multi-candidate (plist-get r :mode)))
            (should (= 1 (plist-get r :rank)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest arxana-vsatarcs-r6-softmax-multi-candidate-abstain-when-all-bad-via-public-api ()
  ;; With a very low abstain-threshold (G must be > -100 to be
  ;; eligible), all enriched candidates' G-totals (which are ≤ 0)
  ;; fall above the threshold so all qualify → softmax picks one.
  ;; Conversely, tightening threshold to -0.001 makes all G ≤ -0.001
  ;; → all below threshold → abstain.  Test the tight-threshold case.
  (let ((tmp (make-temp-file "vsatarcs-r6-abstain-" nil ".tmp"))
        (arxana-vsatarcs-r6-softmax-abstain-g-threshold -0.001))
    (unwind-protect
        (progn
          ;; A file with content that gives G-pragmatic large-negative
          ;; (10 pending checkpoints → -1.0; plus negative epistemic).
          (with-temp-file tmp
            (insert (mapconcat (lambda (n) (format "### Checkpoint %d\n" n))
                               (number-sequence 0 9) "")))
          (let* ((cs (list (list :type :mission-doc-sync :target-file tmp)
                           (list :type :mission-doc-sync :target-file tmp)))
                 (r (arxana-vsatarcs-r6-softmax-select-action cs)))
            (should (eq :abstain-threshold (plist-get r :mode)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest arxana-vsatarcs-r6-softmax-multi-candidate-gap-report-names-best-g ()
  ;; When abstain fires, gap-report names the best-G candidate's
  ;; G-total so operator can see how close to threshold.
  (let ((tmp (make-temp-file "vsatarcs-r6-gap-" nil ".tmp"))
        (arxana-vsatarcs-r6-softmax-abstain-g-threshold -0.001))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (mapconcat (lambda (n) (format "### Checkpoint %d\n" n))
                               (number-sequence 0 9) "")))
          (let* ((cs (list (list :type :mission-doc-sync :target-file tmp)
                           (list :type :mission-doc-sync :target-file tmp)))
                 (r (arxana-vsatarcs-r6-softmax-select-action cs))
                 (gap (plist-get r :gap-report)))
            (should (numberp (plist-get gap :best-g)))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(provide 'arxana-vsatarcs-r6-softmax-test)
;;; arxana-vsatarcs-r6-softmax-test.el ends here
