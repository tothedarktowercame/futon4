;;; arxana-vsatarcs-trace-test.el --- Tests for VSATARCS R8 trace persistence -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-trace.el' (R8 of the standard AIF
;; completeness contract).  Covers:
;;   - Event-source-agnostic emit + read-back roundtrip
;;   - build-record schema stability
;;   - follow-wm idempotence (last-followed-index works via trace scan)
;;   - follow-wm timestamp alignment (VSATARCS tick's :timestamp =
;;     WM record's :timestamp)
;;   - belief-summary shape for empty + populated states

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-belief)
(require 'arxana-vsatarcs-observation)
(require 'arxana-vsatarcs-wm-bridge)
(require 'arxana-vsatarcs-trace)

(defmacro arxana-vsatarcs-trace-test--with-trace-dir (dir-var &rest body)
  "Bind DIR-VAR to a temp trace dir; run BODY with the store override."
  (declare (indent 1))
  `(let* ((,dir-var (make-temp-file "vsatarcs-trace-test-" t))
          (arxana-vsatarcs-trace-store-directory ,dir-var))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory ,dir-var t)))))

;; ---------------------------------------------------------------------
;; Source-agnostic emit + read roundtrip
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-trace-emit-and-read-roundtrip ()
  (arxana-vsatarcs-trace-test--with-trace-dir _
    (let ((rec (arxana-vsatarcs-trace-build-record
                :tick-kind :test-event
                :source :synthetic
                :trigger "manual"
                :observation '(:story-coverage 0.5)
                :belief-summary '(:entity-count 3))))
      (arxana-vsatarcs-trace-emit rec)
      (let* ((read-back (arxana-vsatarcs-trace-read-latest)))
        (should read-back)
        (should (eq :test-event (plist-get read-back :tick-kind)))
        (should (eq :synthetic (plist-get read-back :source)))
        (should (equal "manual" (plist-get read-back :trigger)))
        (should (equal 1 (plist-get read-back :trace-version)))))))

(ert-deftest arxana-vsatarcs-trace-emit-from-arbitrary-source ()
  "The emit API takes records from any source; no source restriction."
  (arxana-vsatarcs-trace-test--with-trace-dir _
    (dolist (src '(:wm-following :reader-open :ingest :bootstrap :test :anything-goes))
      (let ((rec (arxana-vsatarcs-trace-build-record
                  :tick-kind :test :source src)))
        (arxana-vsatarcs-trace-emit rec)))
    (let ((all (arxana-vsatarcs-trace-read-all)))
      (should (= 6 (length all)))
      (should (equal '(:wm-following :reader-open :ingest :bootstrap :test :anything-goes)
                     (mapcar (lambda (r) (plist-get r :source)) all))))))

(ert-deftest arxana-vsatarcs-trace-emit-appends-not-overwrites ()
  (arxana-vsatarcs-trace-test--with-trace-dir _
    (arxana-vsatarcs-trace-emit
     (arxana-vsatarcs-trace-build-record :tick-kind :one :source :a))
    (arxana-vsatarcs-trace-emit
     (arxana-vsatarcs-trace-build-record :tick-kind :two :source :b))
    (arxana-vsatarcs-trace-emit
     (arxana-vsatarcs-trace-build-record :tick-kind :three :source :c))
    (let ((all (arxana-vsatarcs-trace-read-all)))
      (should (= 3 (length all)))
      (should (equal '(:one :two :three)
                     (mapcar (lambda (r) (plist-get r :tick-kind)) all))))))

;; ---------------------------------------------------------------------
;; build-record schema stability
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-trace-build-record-has-all-required-fields ()
  (let ((rec (arxana-vsatarcs-trace-build-record
              :tick-kind :test :source :synthetic)))
    (dolist (k '(:trace-version :timestamp :emitted-at :tick-kind :source
                 :trigger :observation :belief-summary :wm-trace-anchor
                 :bridge-snapshot :prediction-errors :candidates
                 :per-term-EFE :chosen-action :tau :F))
      (should (plist-member rec k)))))

(ert-deftest arxana-vsatarcs-trace-build-record-forward-holes-nil ()
  "Forward-compatible holes initialise to nil until future R-criteria land."
  (let ((rec (arxana-vsatarcs-trace-build-record
              :tick-kind :test :source :synthetic)))
    (dolist (k '(:prediction-errors :candidates :per-term-EFE
                 :chosen-action :tau :F))
      (should (null (plist-get rec k))))))

(ert-deftest arxana-vsatarcs-trace-build-record-version-is-current ()
  (let ((rec (arxana-vsatarcs-trace-build-record :tick-kind :t :source :s)))
    (should (equal arxana-vsatarcs-trace-version
                   (plist-get rec :trace-version)))))

;; ---------------------------------------------------------------------
;; belief-summary
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-trace-belief-summary-empty ()
  (arxana-vsatarcs-belief-reset)
  (let ((s (arxana-vsatarcs-trace-belief-summary)))
    (should (= 0 (plist-get s :entity-count)))
    (should (null (plist-get s :max-entropy)))
    (should (null (plist-get s :min-entropy)))))

(ert-deftest arxana-vsatarcs-trace-belief-summary-uniform ()
  (arxana-vsatarcs-belief-reset)
  (setq arxana-vsatarcs-belief--current
        (arxana-vsatarcs-belief-initial-state '(:e1 :e2 :e3)))
  (let ((s (arxana-vsatarcs-trace-belief-summary)))
    (should (= 3 (plist-get s :entity-count)))
    ;; All three entities have uniform priors → identical entropy
    (should (< (abs (- (plist-get s :max-entropy)
                       (plist-get s :min-entropy)))
               1e-9)))
  (arxana-vsatarcs-belief-reset))

;; ---------------------------------------------------------------------
;; follow-wm: cadence-alignment, idempotence, emit count
;; ---------------------------------------------------------------------

(defconst arxana-vsatarcs-trace-test--wm-record-template
  "{:timestamp %S, :mu-pre {\"arxana/test/a\" {:strengthened 0.14285714285714285, :addressed 0.14285714285714285, :falsified 0.14285714285714285, :reopened 0.14285714285714285, :spawned 0.14285714285714285, :refined 0.14285714285714285, :foreclosed 0.14285714285714285}}, :mu-post {\"arxana/test/a\" {:strengthened 0.14285714285714285, :addressed 0.14285714285714285, :falsified 0.14285714285714285, :reopened 0.14285714285714285, :spawned 0.14285714285714285, :refined 0.14285714285714285, :foreclosed 0.14285714285714285}}}")

(defmacro arxana-vsatarcs-trace-test--with-wm-fixture (wm-dir-var date wm-timestamps &rest body)
  "Bind WM-DIR-VAR to a temp WM-trace dir; write a fixture WM trace file at
DATE with one record per timestamp in WM-TIMESTAMPS; run BODY."
  (declare (indent 3))
  `(let* ((,wm-dir-var (make-temp-file "wm-trace-fixture-" t))
          (wm-path (expand-file-name (format "wm-trace-%s.edn" ,date)
                                     ,wm-dir-var))
          (arxana-vsatarcs-wm-bridge-trace-directory ,wm-dir-var))
     (with-temp-file wm-path
       (dolist (ts ,wm-timestamps)
         (insert (format arxana-vsatarcs-trace-test--wm-record-template ts) "\n")))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory ,wm-dir-var t)))))

(ert-deftest arxana-vsatarcs-trace-follow-wm-emits-one-tick-per-wm-record ()
  (arxana-vsatarcs-trace-test--with-trace-dir _
    (arxana-vsatarcs-trace-test--with-wm-fixture _
        "2026-05-18"
        '("2026-05-18T10:00:00Z" "2026-05-18T11:00:00Z" "2026-05-18T12:00:00Z")
      (arxana-vsatarcs-belief-reset)
      (let ((n (arxana-vsatarcs-trace-follow-wm "2026-05-18")))
        (should (= 3 n))
        (let ((all (arxana-vsatarcs-trace-read-all "2026-05-18")))
          (should (= 3 (length all)))
          (dolist (r all)
            (should (eq :wm-bridge-fetch (plist-get r :tick-kind)))
            (should (eq :wm-following (plist-get r :source)))))))))

(ert-deftest arxana-vsatarcs-trace-follow-wm-timestamps-align-with-wm ()
  "Each VSATARCS tick's :timestamp matches the WM record's :timestamp."
  (let ((timestamps '("2026-05-18T10:00:00Z"
                      "2026-05-18T11:00:00Z"
                      "2026-05-18T12:00:00Z")))
    (arxana-vsatarcs-trace-test--with-trace-dir _
      (arxana-vsatarcs-trace-test--with-wm-fixture _
          "2026-05-18" timestamps
        (arxana-vsatarcs-belief-reset)
        (arxana-vsatarcs-trace-follow-wm "2026-05-18")
        (let* ((all (arxana-vsatarcs-trace-read-all "2026-05-18"))
               (recorded-ts (mapcar (lambda (r) (plist-get r :timestamp))
                                    all)))
          (should (equal timestamps recorded-ts)))))))

(ert-deftest arxana-vsatarcs-trace-follow-wm-is-idempotent ()
  "Running follow-wm twice emits ticks only for unseen records."
  (arxana-vsatarcs-trace-test--with-trace-dir _
    (arxana-vsatarcs-trace-test--with-wm-fixture _
        "2026-05-18"
        '("2026-05-18T10:00:00Z" "2026-05-18T11:00:00Z")
      (arxana-vsatarcs-belief-reset)
      (let ((n1 (arxana-vsatarcs-trace-follow-wm "2026-05-18"))
            (n2 (arxana-vsatarcs-trace-follow-wm "2026-05-18")))
        (should (= 2 n1))
        (should (= 0 n2))
        (should (= 2 (length (arxana-vsatarcs-trace-read-all "2026-05-18"))))))))

(ert-deftest arxana-vsatarcs-trace-follow-wm-picks-up-new-records ()
  "After follow-wm runs, appending a new WM record + re-running emits one more."
  (arxana-vsatarcs-trace-test--with-trace-dir _
    (arxana-vsatarcs-trace-test--with-wm-fixture wm-dir
        "2026-05-18"
        '("2026-05-18T10:00:00Z" "2026-05-18T11:00:00Z")
      (arxana-vsatarcs-belief-reset)
      (should (= 2 (arxana-vsatarcs-trace-follow-wm "2026-05-18")))
      ;; Append a third WM record
      (let ((wm-path (expand-file-name "wm-trace-2026-05-18.edn" wm-dir)))
        (with-temp-buffer
          (insert (format arxana-vsatarcs-trace-test--wm-record-template
                          "2026-05-18T12:00:00Z")
                  "\n")
          (append-to-file (point-min) (point-max) wm-path)))
      (should (= 1 (arxana-vsatarcs-trace-follow-wm "2026-05-18")))
      (should (= 3 (length (arxana-vsatarcs-trace-read-all "2026-05-18")))))))

(ert-deftest arxana-vsatarcs-trace-follow-wm-anchor-indexes-monotonic ()
  (arxana-vsatarcs-trace-test--with-trace-dir _
    (arxana-vsatarcs-trace-test--with-wm-fixture _
        "2026-05-18"
        '("2026-05-18T10:00:00Z" "2026-05-18T11:00:00Z" "2026-05-18T12:00:00Z")
      (arxana-vsatarcs-belief-reset)
      (arxana-vsatarcs-trace-follow-wm "2026-05-18")
      (let* ((all (arxana-vsatarcs-trace-read-all "2026-05-18"))
             (indices (mapcar (lambda (r)
                                (plist-get (plist-get r :wm-trace-anchor)
                                           :line-index))
                              all)))
        (should (equal '(0 1 2) indices))))))

(ert-deftest arxana-vsatarcs-trace-follow-wm-missing-file-emits-zero ()
  (arxana-vsatarcs-trace-test--with-trace-dir _
    (let ((arxana-vsatarcs-wm-bridge-trace-directory
           (expand-file-name "no-such-dir/" temporary-file-directory)))
      (should (= 0 (arxana-vsatarcs-trace-follow-wm "2099-01-01"))))))

(ert-deftest arxana-vsatarcs-trace-follow-wm-bridge-snapshot-includes-counts ()
  (arxana-vsatarcs-trace-test--with-trace-dir _
    (arxana-vsatarcs-trace-test--with-wm-fixture _
        "2026-05-18" '("2026-05-18T10:00:00Z")
      (arxana-vsatarcs-belief-reset)
      (arxana-vsatarcs-trace-follow-wm "2026-05-18")
      (let* ((rec (arxana-vsatarcs-trace-read-latest "2026-05-18"))
             (snap (plist-get rec :bridge-snapshot)))
        (should snap)
        (should (eq t (plist-get snap :wm-belief-available?)))
        (dolist (k '(:only-in-local-count :only-in-wm-side-count
                     :expected-in-wm-only-count :posterior-diffs-count
                     :max-posterior-diff :equal-count))
          (should (plist-member snap k)))))))

;; ---------------------------------------------------------------------
;; v0.5.1 — R9 F-decrease wiring (follow-wm populates :F per tick;
;; F-total trends downward as belief updates toward observation).
;; ---------------------------------------------------------------------

(require 'arxana-vsatarcs-likelihood)

(ert-deftest arxana-vsatarcs-trace-follow-wm-populates-F-total ()
  "follow-wm fills the `:F' field with the F-total from compute-vfe."
  (arxana-vsatarcs-trace-test--with-trace-dir _
    (arxana-vsatarcs-trace-test--with-wm-fixture _
        "2026-05-18" '("2026-05-18T10:00:00Z")
      (arxana-vsatarcs-belief-reset)
      (setq arxana-vsatarcs-belief--current
            (arxana-vsatarcs-belief-initial-state
             '("arxana/test/a" "arxana/test/b")))
      (arxana-vsatarcs-trace-follow-wm "2026-05-18")
      (let* ((rec (arxana-vsatarcs-trace-read-latest "2026-05-18"))
             (f (plist-get rec :F))
             (errs (plist-get rec :prediction-errors)))
        (should (numberp f))
        (should (>= f 0.0))
        (should errs)
        (dolist (ch arxana-vsatarcs-likelihood-channels-with-likelihood)
          (should (plist-get errs ch)))))
    (arxana-vsatarcs-belief-reset)))

(ert-deftest arxana-vsatarcs-trace-r9-f-decrease-named-property ()
  "R9 named-validation property: F-total decreases as belief updates
toward the observation.  Asserts F(post-multi-step) < F(pre-multi-step)
when the multi-step is given a clear-signal observation."
  (arxana-vsatarcs-belief-reset)
  (setq arxana-vsatarcs-belief--current
        (arxana-vsatarcs-belief-initial-state '(:e1 :e2 :e3)))
  (let* ((target-obs '(:story-coverage 1.0
                       :lift-freshness 1.0
                       :annotation-overlay-presence 1.0))
         (f-before (plist-get
                    (arxana-vsatarcs-likelihood-compute-vfe target-obs)
                    :F-total))
         (result (arxana-vsatarcs-likelihood-run-multi-step target-obs))
         (final-belief (plist-get result :belief))
         (f-after (plist-get
                   (arxana-vsatarcs-likelihood-compute-vfe target-obs final-belief)
                   :F-total)))
    (should (< f-after f-before)))
  (arxana-vsatarcs-belief-reset))

;; F-decrease at the trace level (across follow-wm calls) is hard to
;; robustly fixture-test because (a) `arxana-vsatarcs-observe' pulls from
;; the real corpus (varies by environment); (b) multi-step's annealed
;; updates per call (event-weight = magnitude * anneal * 0.1) move belief
;; only a little, so per-tick F-deltas are dominated by floating-point
;; noise.  The named-property assertion lives at the compute-vfe level
;; (`arxana-vsatarcs-trace-r9-f-decrease-named-property') where the test
;; uses a clear-signal synthetic observation.

(provide 'arxana-vsatarcs-trace-test)
;;; arxana-vsatarcs-trace-test.el ends here
