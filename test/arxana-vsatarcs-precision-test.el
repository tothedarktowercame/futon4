;;; arxana-vsatarcs-precision-test.el --- Tests for R7 adaptive precision -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-precision.el' (R7 adaptive precision).
;; Mirrors the WM-side test pattern at `futon2/test/futon2/aif/'.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-likelihood)
(require 'arxana-vsatarcs-precision)

;; ---------------------------------------------------------------------
;; Initial state
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-precision-initial-state-covers-r3a-channels ()
  (let ((s (arxana-vsatarcs-precision-initial-state)))
    (dolist (ch arxana-vsatarcs-likelihood-channels-with-likelihood)
      (should (assoc ch s)))))

(ert-deftest arxana-vsatarcs-precision-initial-state-default-precision ()
  (let ((s (arxana-vsatarcs-precision-initial-state '(:c1 :c2))))
    (dolist (ch '(:c1 :c2))
      (let ((cs (cdr (assoc ch s))))
        (should (= arxana-vsatarcs-precision-initial
                   (plist-get cs :precision)))
        (should (null (plist-get cs :error-history)))))))

;; ---------------------------------------------------------------------
;; Variance helper (private)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-precision-variance-zero-for-empty-or-singleton ()
  (should (= 0.0 (arxana-vsatarcs-precision--variance nil)))
  (should (= 0.0 (arxana-vsatarcs-precision--variance '(5.0)))))

(ert-deftest arxana-vsatarcs-precision-variance-zero-for-constant-history ()
  (should (= 0.0 (arxana-vsatarcs-precision--variance '(0.5 0.5 0.5 0.5)))))

(ert-deftest arxana-vsatarcs-precision-variance-positive-for-varying ()
  (let ((v (arxana-vsatarcs-precision--variance '(0.0 1.0 0.0 1.0))))
    (should (> v 0.0))))

;; ---------------------------------------------------------------------
;; update-channel-precision behaviour
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-precision-update-state-initialises-untracked-channels ()
  (let* ((errors '(:c1 (:error 0.5 :observed 0.7)))
         (out (arxana-vsatarcs-precision-update-state nil errors)))
    (should (assoc :c1 out))
    (let ((cs (cdr (assoc :c1 out))))
      (should (numberp (plist-get cs :precision)))
      (should (= 1 (length (plist-get cs :error-history)))))))

(ert-deftest arxana-vsatarcs-precision-update-state-appends-error-history ()
  (let* ((s1 (arxana-vsatarcs-precision-update-state
              nil '(:c1 (:error 0.1 :observed 0.5))))
         (s2 (arxana-vsatarcs-precision-update-state
              s1 '(:c1 (:error 0.2 :observed 0.5))))
         (s3 (arxana-vsatarcs-precision-update-state
              s2 '(:c1 (:error 0.3 :observed 0.5)))))
    (should (= 3 (length (plist-get (cdr (assoc :c1 s3)) :error-history))))
    (should (equal '(0.1 0.2 0.3)
                   (plist-get (cdr (assoc :c1 s3)) :error-history)))))

(ert-deftest arxana-vsatarcs-precision-update-state-truncates-to-window ()
  (let ((arxana-vsatarcs-precision-window-size 3))
    (let ((s nil))
      (dolist (e '(0.1 0.2 0.3 0.4 0.5))
        (setq s (arxana-vsatarcs-precision-update-state
                 s (list :c1 (list :error e :observed 0.5)))))
      (let ((hist (plist-get (cdr (assoc :c1 s)) :error-history)))
        (should (= 3 (length hist)))
        ;; Last 3 errors kept
        (should (equal '(0.3 0.4 0.5) hist))))))

(ert-deftest arxana-vsatarcs-precision-update-state-passes-through-untouched-channels ()
  (let* ((s1 (arxana-vsatarcs-precision-update-state
              nil '(:c1 (:error 0.1 :observed 0.5)
                    :c2 (:error 0.2 :observed 0.5))))
         (s2 (arxana-vsatarcs-precision-update-state
              s1 '(:c1 (:error 0.3 :observed 0.5)))))
    ;; :c2 not touched in s2's errors — should pass through
    (should (equal (cdr (assoc :c2 s1)) (cdr (assoc :c2 s2))))
    ;; :c1 updated
    (should (not (equal (cdr (assoc :c1 s1)) (cdr (assoc :c1 s2)))))))

(ert-deftest arxana-vsatarcs-precision-update-state-precision-within-bounds ()
  "After any update, precision must be in [floor, cap]."
  (let ((s (arxana-vsatarcs-precision-update-state
            nil '(:c1 (:error 0.5 :observed 0.5)
                  :c2 (:error 100.0 :observed 0.5)))))
    (dolist (kv s)
      (let ((p (plist-get (cdr kv) :precision)))
        (should (>= p arxana-vsatarcs-precision-floor))
        (should (<= p arxana-vsatarcs-precision-cap))))))

(ert-deftest arxana-vsatarcs-precision-update-state-low-variance-yields-high-precision ()
  "Constant error history → low variance → high precision.
With variance-component-only (need-component is 0 until R5 lands),
the maximum reachable precision is `1 / min-variance' (default 100),
not the configured cap (default 200).  Test asserts precision is at
this variance-component ceiling."
  (let ((s nil))
    (dotimes (_ 5)
      (setq s (arxana-vsatarcs-precision-update-state
               s '(:c1 (:error 0.5 :observed 0.5)))))
    (let ((p (plist-get (cdr (assoc :c1 s)) :precision))
          (var-component-ceiling (/ 1.0 arxana-vsatarcs-precision-min-variance)))
      ;; Precision should equal the variance-component ceiling (no
      ;; need-component contribution without preferences)
      (should (< (abs (- p var-component-ceiling)) 1e-9))
      ;; And both within bounds
      (should (>= p arxana-vsatarcs-precision-floor))
      (should (<= p arxana-vsatarcs-precision-cap)))))

(ert-deftest arxana-vsatarcs-precision-update-state-high-variance-yields-lower-precision ()
  "Wildly varying error history → high variance → lower precision."
  (let ((s nil))
    (dolist (e '(-1.0 1.0 -1.0 1.0 -1.0))
      (setq s (arxana-vsatarcs-precision-update-state
               s (list :c1 (list :error e :observed 0.5)))))
    (let ((p (plist-get (cdr (assoc :c1 s)) :precision)))
      (should (< p arxana-vsatarcs-precision-cap)))))

;; ---------------------------------------------------------------------
;; precision-for + weighted-error
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-precision-for-defaults-when-channel-absent ()
  (should (= arxana-vsatarcs-precision-initial
             (arxana-vsatarcs-precision-for nil :not-a-channel))))

(ert-deftest arxana-vsatarcs-precision-weighted-error-preserves-per-call ()
  (let* ((s (arxana-vsatarcs-precision-update-state
             nil '(:c1 (:error 0.5 :observed 0.5))))
         (err-map '(:error 0.5 :precision 1.0 :weighted-error 0.5
                    :observed 0.5 :predicted-mean 0.0 :predicted-variance 1.0))
         (out (arxana-vsatarcs-precision-weighted-error s :c1 err-map)))
    ;; per-call-precision preserved from input :precision
    (should (= 1.0 (plist-get out :per-call-precision)))
    ;; :precision replaced with adaptive
    (should (= (arxana-vsatarcs-precision-for s :c1)
               (plist-get out :precision)))
    ;; :weighted-error recomputed
    (should (= (* 0.5 (arxana-vsatarcs-precision-for s :c1))
               (plist-get out :weighted-error)))
    ;; Other fields untouched
    (should (= 0.5 (plist-get out :error)))
    (should (= 0.5 (plist-get out :observed)))))

(ert-deftest arxana-vsatarcs-precision-reweight-all-covers-all-channels ()
  (let* ((s (arxana-vsatarcs-precision-update-state
             nil '(:c1 (:error 0.5 :observed 0.5)
                   :c2 (:error 0.3 :observed 0.7))))
         (errors '(:c1 (:error 0.5 :precision 1.0 :weighted-error 0.5 :observed 0.5)
                   :c2 (:error 0.3 :precision 1.0 :weighted-error 0.3 :observed 0.7)))
         (out (arxana-vsatarcs-precision-reweight-all s errors)))
    (should (plist-get out :c1))
    (should (plist-get out :c2))
    (dolist (ch '(:c1 :c2))
      (should (plist-get (plist-get out ch) :per-call-precision))
      (should (= (arxana-vsatarcs-precision-for s ch)
                 (plist-get (plist-get out ch) :precision))))))

;; ---------------------------------------------------------------------
;; need-component is 0 until R5 (preferences) lands
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-precision-need-component-is-zero-without-preferences ()
  "Sanity: need-component returns 0.0 for any channel + observation
because preferences are deferred (R5 — writer capability)."
  (dolist (ch '(:c1 :c2 :story-coverage :anything))
    (dolist (v '(0.0 0.5 1.0 -5.0 100.0))
      (should (= 0.0
                 (arxana-vsatarcs-precision--need-component-for ch v 5.0))))))

(provide 'arxana-vsatarcs-precision-test)
;;; arxana-vsatarcs-precision-test.el ends here
