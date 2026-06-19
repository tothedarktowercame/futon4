;;; arxana-vsatarcs-efe-test.el --- Tests for R5 EFE composition -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-efe.el' (R5 EFE composition).  Covers
;; per-class pragmatic helpers + the default epistemic proxy + the
;; compute / enrich pipeline + the closure-flip wiring with
;; `arxana-vsatarcs-r6-softmax-select-action' (full softmax now
;; reaches the multi-candidate path automatically).

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-efe)
(require 'arxana-vsatarcs-r6-softmax)

;; ---------------------------------------------------------------------
;; Helpers: temp file fixtures for the file-reading proxies
;; ---------------------------------------------------------------------

(defmacro arxana-vsatarcs-efe-test--with-temp-file (content var &rest body)
  "Write CONTENT into a temp file; bind its path to VAR; eval BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file "vsatarcs-efe-test-" nil ".tmp")))
     (unwind-protect
         (progn
           (with-temp-file ,var (insert ,content))
           ,@body)
       (when (file-exists-p ,var) (delete-file ,var)))))

;; ---------------------------------------------------------------------
;; --g-epistemic-default
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-efe-epistemic-no-target ()
  ;; Action without :target-file or :story-path → epistemic = 0.0.
  (should (= 0.0 (arxana-vsatarcs-efe--g-epistemic-default
                  '(:type :mission-doc-sync)))))

(ert-deftest arxana-vsatarcs-efe-epistemic-nonexistent-target ()
  (should (= 0.0 (arxana-vsatarcs-efe--g-epistemic-default
                  '(:type :mission-doc-sync :target-file "/nonexistent")))))

(ert-deftest arxana-vsatarcs-efe-epistemic-just-touched-near-zero ()
  ;; A file with mtime ~now should produce a small-magnitude negative
  ;; epistemic (close to 0).
  (arxana-vsatarcs-efe-test--with-temp-file "x" path
    (let ((e (arxana-vsatarcs-efe--g-epistemic-default
              (list :type :mission-doc-sync :target-file path))))
      (should (<= e 0.0))
      ;; Within an order of magnitude of 0 for a fresh file.
      (should (> e -0.1)))))

;; ---------------------------------------------------------------------
;; --g-pragmatic-mission-doc-sync
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-efe-pragmatic-mission-doc-pending-checkpoints ()
  ;; 3 checkpoints, 1 complete → 2 pending → G ≈ -0.2 (= -2/scale).
  (arxana-vsatarcs-efe-test--with-temp-file
      (concat
       "### Checkpoint 0 — setup\n**Status: COMPLETE**\n"
       "### Checkpoint 1 — work\n**Status: IN PROGRESS**\n"
       "### Checkpoint 2 — finish\n")
      path
    (let* ((g (arxana-vsatarcs-efe--g-pragmatic-mission-doc-sync
               (list :type :mission-doc-sync :target-file path))))
      (should (< g 0.0))
      ;; -2/10 = -0.2 with default scale.
      (should (< (abs (- g -0.2)) 1e-6)))))

(ert-deftest arxana-vsatarcs-efe-pragmatic-mission-doc-all-complete ()
  ;; All checkpoints complete → 0 pending → G = 0.0 (no motivation).
  (arxana-vsatarcs-efe-test--with-temp-file
      (concat "### Checkpoint 0\n**Status: COMPLETE**\n"
              "### Checkpoint 1\n**Status: COMPLETE**\n")
      path
    (let ((g (arxana-vsatarcs-efe--g-pragmatic-mission-doc-sync
              (list :type :mission-doc-sync :target-file path))))
      (should (= 0.0 g)))))

(ert-deftest arxana-vsatarcs-efe-pragmatic-mission-doc-no-target ()
  (should (= 0.0 (arxana-vsatarcs-efe--g-pragmatic-mission-doc-sync
                  '(:type :mission-doc-sync)))))

;; ---------------------------------------------------------------------
;; --g-pragmatic-aif-edn-revision-entry
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-efe-pragmatic-aif-edn-counts-revisions ()
  (arxana-vsatarcs-efe-test--with-temp-file
      "{:revisions [{:rev \"v0.1\" :on \"x\"} {:rev \"v0.2\" :on \"y\"}]}"
      path
    (let ((g (arxana-vsatarcs-efe--g-pragmatic-aif-edn-revision-entry
              (list :type :aif-edn-revision-entry :target-file path))))
      (should (< g 0.0))
      (should (< (abs (- g -0.2)) 1e-6)))))  ; -2/scale

;; ---------------------------------------------------------------------
;; --g-pragmatic-story-update / --g-pragmatic-stack-annotations-upsert
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-efe-pragmatic-story-update-fresh-near-zero ()
  ;; Fresh file → near-zero pragmatic.
  (arxana-vsatarcs-efe-test--with-temp-file "x" path
    (let ((g (arxana-vsatarcs-efe--g-pragmatic-story-update
              (list :type :story-update :target-file path))))
      (should (<= g 0.0))
      (should (> g -0.01)))))

(ert-deftest arxana-vsatarcs-efe-pragmatic-stack-annotations-upsert-shares-shape ()
  ;; The two lifting-shaped pragmatic helpers share the same impl;
  ;; verify symmetric behavior.  Allow tiny drift because the two
  ;; calls happen at slightly different times (mtime → now subtraction
  ;; drifts by nanoseconds between sequential calls).
  (arxana-vsatarcs-efe-test--with-temp-file "x" path
    (let ((g1 (arxana-vsatarcs-efe--g-pragmatic-story-update
               (list :type :story-update :target-file path)))
          (g2 (arxana-vsatarcs-efe--g-pragmatic-stack-annotations-upsert
               (list :type :stack-annotations-upsert :target-file path))))
      (should (< (abs (- g1 g2)) 1e-6)))))

;; ---------------------------------------------------------------------
;; --g-pragmatic dispatch
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-efe-pragmatic-dispatch-unknown-class-zero ()
  (should (= 0.0 (arxana-vsatarcs-efe--g-pragmatic
                  '(:type :unknown-action-class)))))

;; ---------------------------------------------------------------------
;; compute + enrich
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-efe-compute-attaches-three-g-fields ()
  (arxana-vsatarcs-efe-test--with-temp-file "x" path
    (let ((enriched (arxana-vsatarcs-efe-compute
                     (list :type :mission-doc-sync :target-file path))))
      (should (numberp (plist-get enriched :G-pragmatic)))
      (should (numberp (plist-get enriched :G-epistemic)))
      (should (numberp (plist-get enriched :G-total))))))

(ert-deftest arxana-vsatarcs-efe-compute-g-total-is-sum ()
  (arxana-vsatarcs-efe-test--with-temp-file
      (concat "### Checkpoint 0\n### Checkpoint 1\n")  ; 2 pending → G-pragmatic = -0.2
      path
    (let* ((e (arxana-vsatarcs-efe-compute
               (list :type :mission-doc-sync :target-file path)))
           (p (plist-get e :G-pragmatic))
           (ep (plist-get e :G-epistemic))
           (tot (plist-get e :G-total)))
      (should (< (abs (- tot (+ p ep))) 1e-9)))))

(ert-deftest arxana-vsatarcs-efe-compute-preserves-input-fields ()
  ;; Input fields like :target, :type stay on the enriched plist.
  (arxana-vsatarcs-efe-test--with-temp-file "x" path
    (let ((e (arxana-vsatarcs-efe-compute
              (list :type :mission-doc-sync
                    :target-file path
                    :target-checkpoint 5))))
      (should (eq :mission-doc-sync (plist-get e :type)))
      (should (= 5 (plist-get e :target-checkpoint))))))

(ert-deftest arxana-vsatarcs-efe-enrich-skips-already-scored ()
  ;; Candidates that already carry :G-total pass through unchanged.
  (let* ((pre-scored (list :type :mission-doc-sync :G-total -3.0))
         (out (arxana-vsatarcs-efe-enrich-candidates (list pre-scored))))
    (should (= 1 (length out)))
    (should (= -3.0 (plist-get (car out) :G-total)))
    (should-not (plist-get (car out) :G-pragmatic))))  ; unchanged

(ert-deftest arxana-vsatarcs-efe-enrich-attaches-to-unscored ()
  (arxana-vsatarcs-efe-test--with-temp-file "x" path
    (let* ((bare (list :type :mission-doc-sync :target-file path))
           (out (arxana-vsatarcs-efe-enrich-candidates (list bare))))
      (should (numberp (plist-get (car out) :G-total))))))

(ert-deftest arxana-vsatarcs-efe-enrich-mixed-batch ()
  (arxana-vsatarcs-efe-test--with-temp-file "x" path
    (let* ((pre (list :type :mission-doc-sync :G-total -2.5))
           (bare (list :type :mission-doc-sync :target-file path))
           (out (arxana-vsatarcs-efe-enrich-candidates (list pre bare))))
      (should (= -2.5 (plist-get (nth 0 out) :G-total)))   ; pre preserved
      (should (numberp (plist-get (nth 1 out) :G-total))))))   ; bare enriched

;; ---------------------------------------------------------------------
;; Closure-flip wiring: r6-softmax-select-action now routes multi
;; candidates through enrichment → full softmax (replacing v0.5.26's
;; :r5-pending stub).
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-efe-flips-r6-multi-candidate-path ()
  ;; Three bare candidates → public API enriches them → routes to
  ;; --full-softmax-select instead of --r5-pending.
  (arxana-vsatarcs-efe-test--with-temp-file "x" path
    (let* ((cs (list (list :type :mission-doc-sync :target-file path)
                     (list :type :mission-doc-sync :target-file path)
                     (list :type :aif-edn-revision-entry :target-file path)))
           (r (arxana-vsatarcs-r6-softmax-select-action cs)))
      ;; No longer :r5-pending!
      (should-not (eq :r5-pending (plist-get r :mode)))
      ;; Should be either :softmax-multi-candidate or :abstain-threshold.
      (should (memq (plist-get r :mode)
                    '(:softmax-multi-candidate :abstain-threshold))))))

(provide 'arxana-vsatarcs-efe-test)
;;; arxana-vsatarcs-efe-test.el ends here
