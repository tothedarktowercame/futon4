;;; arxana-vsatarcs-likelihood-test.el --- Tests for VSATARCS R3a + R3c likelihood -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-likelihood.el' — R3a (prediction error
;; per observation channel) + R3c (VFE shape over the likelihood) +
;; R3 multi-step inner loop.  Ports the v0.10/v0.11/v0.13/v0.16
;; WM-side test pattern.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-belief)
(require 'arxana-vsatarcs-likelihood)

(defun arxana-vsatarcs-likelihood-test--uniform-prior-belief ()
  "Return a small fixture belief: 2 entities, uniform priors."
  (arxana-vsatarcs-belief-initial-state '(:e1 :e2)))

(defun arxana-vsatarcs-likelihood-test--peaked-belief ()
  "Belief where :e1 is heavily strengthened, :e2 is uniform."
  (let ((b (arxana-vsatarcs-belief-initial-state '(:e1 :e2))))
    (arxana-vsatarcs-belief-update-batch
     b (make-list 10 '(:entity-id :e1 :type :strengthened :weight 2.0)))))

;; ---------------------------------------------------------------------
;; Per-channel weight tables (schema stability)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-likelihood-weight-tables-cover-status-set ()
  "Each weight table should have an entry for every status."
  (dolist (table (list arxana-vsatarcs-likelihood-story-coverage-weights
                       arxana-vsatarcs-likelihood-lift-freshness-weights
                       arxana-vsatarcs-likelihood-annotation-overlay-weights))
    (dolist (s arxana-vsatarcs-belief-status-set)
      (should (assoc s table)))))

(ert-deftest arxana-vsatarcs-likelihood-weight-values-in-unit-interval ()
  (dolist (table (list arxana-vsatarcs-likelihood-story-coverage-weights
                       arxana-vsatarcs-likelihood-lift-freshness-weights
                       arxana-vsatarcs-likelihood-annotation-overlay-weights))
    (dolist (kv table)
      (should (and (>= (cdr kv) 0.0) (<= (cdr kv) 1.0))))))

(ert-deftest arxana-vsatarcs-likelihood-channels-with-likelihood-disjoint-from-prototyping ()
  (should (null (cl-intersection arxana-vsatarcs-likelihood-channels-with-likelihood
                                 arxana-vsatarcs-likelihood-prototyping-forward-channels))))

;; ---------------------------------------------------------------------
;; Per-channel predict shape
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-likelihood-predict-story-coverage-shape ()
  (let* ((b (arxana-vsatarcs-likelihood-test--uniform-prior-belief))
         (p (arxana-vsatarcs-likelihood-predict-story-coverage b)))
    (should (plist-get p :mean))
    (should (plist-get p :variance))
    (let ((m (plist-get p :mean)) (v (plist-get p :variance)))
      (should (and (numberp m) (>= m 0.0) (<= m 1.0)))
      (should (and (numberp v) (>= v 0.0) (<= v 1.0))))))

(ert-deftest arxana-vsatarcs-likelihood-predict-empty-belief-gives-maximum-uncertainty ()
  (let ((p1 (arxana-vsatarcs-likelihood-predict-story-coverage nil))
        (p2 (arxana-vsatarcs-likelihood-predict-lift-freshness nil))
        (p3 (arxana-vsatarcs-likelihood-predict-annotation-overlay-presence nil)))
    (dolist (p (list p1 p2 p3))
      (should (= 0.0 (plist-get p :mean)))
      (should (= 1.0 (plist-get p :variance))))))

(ert-deftest arxana-vsatarcs-likelihood-predict-uniform-vs-peaked-variance ()
  "Peaked beliefs should yield lower predicted variance than uniform."
  (let* ((uniform (arxana-vsatarcs-likelihood-test--uniform-prior-belief))
         (peaked (arxana-vsatarcs-likelihood-test--peaked-belief))
         (p-uniform (arxana-vsatarcs-likelihood-predict-story-coverage uniform))
         (p-peaked (arxana-vsatarcs-likelihood-predict-story-coverage peaked)))
    (should (> (plist-get p-uniform :variance)
               (plist-get p-peaked :variance)))))

(ert-deftest arxana-vsatarcs-likelihood-predict-observation-covers-three-channels ()
  (let ((p (arxana-vsatarcs-likelihood-predict-observation
            (arxana-vsatarcs-likelihood-test--uniform-prior-belief))))
    (dolist (ch arxana-vsatarcs-likelihood-channels-with-likelihood)
      (should (plist-member p ch)))
    (dolist (ch arxana-vsatarcs-likelihood-prototyping-forward-channels)
      (should-not (plist-member p ch)))))

;; ---------------------------------------------------------------------
;; Prediction error
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-likelihood-compute-prediction-error-zero-when-aligned ()
  (let* ((pred '(:mean 0.5 :variance 0.25))
         (err (arxana-vsatarcs-likelihood-compute-prediction-error 0.5 pred)))
    (should (< (abs (plist-get err :error)) 1e-9))
    (should (< (abs (plist-get err :weighted-error)) 1e-9))))

(ert-deftest arxana-vsatarcs-likelihood-compute-prediction-error-positive-when-obs-higher ()
  (let* ((pred '(:mean 0.3 :variance 0.5))
         (err (arxana-vsatarcs-likelihood-compute-prediction-error 0.8 pred)))
    (should (> (plist-get err :error) 0))
    (should (< (abs (- (plist-get err :error) 0.5)) 1e-9))))

(ert-deftest arxana-vsatarcs-likelihood-compute-prediction-error-precision-floors-low-variance ()
  "Tiny predicted-variance should not blow up precision (uses min-variance floor)."
  (let* ((pred '(:mean 0.5 :variance 1.0e-12))
         (err (arxana-vsatarcs-likelihood-compute-prediction-error 0.7 pred)))
    (should (<= (plist-get err :precision)
                (/ 1.0 arxana-vsatarcs-likelihood-min-variance)))))

(ert-deftest arxana-vsatarcs-likelihood-compute-prediction-errors-covers-all-channels ()
  (let* ((obs '(:story-coverage 0.5
                :lift-freshness 0.7
                :annotation-overlay-presence 0.3
                :scene-density 0.5
                :link-density 0.5))
         (b (arxana-vsatarcs-likelihood-test--uniform-prior-belief))
         (preds (arxana-vsatarcs-likelihood-predict-observation b))
         (errs (arxana-vsatarcs-likelihood-compute-prediction-errors obs preds)))
    (dolist (ch arxana-vsatarcs-likelihood-channels-with-likelihood)
      (should (plist-get errs ch)))))

;; ---------------------------------------------------------------------
;; VFE shape
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-likelihood-vfe-shape-fields ()
  (let* ((obs '(:story-coverage 0.5 :lift-freshness 0.5 :annotation-overlay-presence 0.5))
         (b (arxana-vsatarcs-likelihood-test--uniform-prior-belief))
         (vfe (arxana-vsatarcs-likelihood-compute-vfe obs b)))
    (should (plist-get vfe :F-total))
    (should (plist-get vfe :F-accuracy))
    (should (plist-get vfe :F-complexity))
    (should (plist-get vfe :per-channel-errors))))

(ert-deftest arxana-vsatarcs-likelihood-vfe-decreases-when-belief-explains-observation ()
  "Belief that better predicts the observation should give lower F-total."
  ;; Use observation values that match the peaked-belief's prediction more
  ;; closely than the uniform belief's prediction.
  (let* ((uniform (arxana-vsatarcs-likelihood-test--uniform-prior-belief))
         (peaked (arxana-vsatarcs-likelihood-test--peaked-belief))
         ;; Get the peaked prediction; observe values close to those means.
         (peaked-pred (arxana-vsatarcs-likelihood-predict-observation peaked))
         (obs (list :story-coverage (plist-get (plist-get peaked-pred :story-coverage) :mean)
                    :lift-freshness (plist-get (plist-get peaked-pred :lift-freshness) :mean)
                    :annotation-overlay-presence (plist-get (plist-get peaked-pred :annotation-overlay-presence) :mean)))
         (vfe-uniform (arxana-vsatarcs-likelihood-compute-vfe obs uniform))
         (vfe-peaked (arxana-vsatarcs-likelihood-compute-vfe obs peaked)))
    (should (< (plist-get vfe-peaked :F-total) (plist-get vfe-uniform :F-total)))))

;; ---------------------------------------------------------------------
;; Multi-step inner loop
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-likelihood-multi-step-runs-up-to-K-steps ()
  (let* ((obs '(:story-coverage 1.0 :lift-freshness 1.0 :annotation-overlay-presence 1.0))
         (b (arxana-vsatarcs-likelihood-test--uniform-prior-belief))
         (result (arxana-vsatarcs-likelihood-run-multi-step obs b)))
    (should (<= (length (plist-get result :micro-step-trace))
                arxana-vsatarcs-likelihood-r3-max-steps))))

(ert-deftest arxana-vsatarcs-likelihood-multi-step-anneal-factor-decreases ()
  "Across micro-steps, the anneal-factor must monotonically decrease."
  (let* ((obs '(:story-coverage 1.0 :lift-freshness 1.0 :annotation-overlay-presence 1.0))
         (b (arxana-vsatarcs-likelihood-test--uniform-prior-belief))
         (result (arxana-vsatarcs-likelihood-run-multi-step obs b))
         (trace (plist-get result :micro-step-trace))
         (factors (mapcar (lambda (r) (plist-get r :anneal-factor))
                          (append trace nil))))
    (cl-loop for (a b) on factors while b
             do (should (>= a b)))))

(ert-deftest arxana-vsatarcs-likelihood-multi-step-early-terminates-on-aligned-observation ()
  "When observation matches prediction closely, the loop should early-terminate."
  ;; Construct a belief, get its prediction, then OBSERVE exactly that prediction.
  (let* ((b (arxana-vsatarcs-likelihood-test--uniform-prior-belief))
         (preds (arxana-vsatarcs-likelihood-predict-observation b))
         (obs (list :story-coverage (plist-get (plist-get preds :story-coverage) :mean)
                    :lift-freshness (plist-get (plist-get preds :lift-freshness) :mean)
                    :annotation-overlay-presence (plist-get (plist-get preds :annotation-overlay-presence) :mean)))
         (result (arxana-vsatarcs-likelihood-run-multi-step obs b)))
    (should (plist-get result :early-terminated?))))

(ert-deftest arxana-vsatarcs-likelihood-multi-step-belief-updates ()
  "After multi-step with non-trivial errors, belief should differ from input."
  (let* ((b (arxana-vsatarcs-likelihood-test--uniform-prior-belief))
         (obs '(:story-coverage 1.0 :lift-freshness 1.0 :annotation-overlay-presence 1.0))
         (result (arxana-vsatarcs-likelihood-run-multi-step obs b))
         (final (plist-get result :belief)))
    ;; The :e1 posterior in final should differ from uniform (events fired)
    (let* ((post-final (cdr (assoc :e1 final)))
           (post-uniform (cdr (assoc :e1 b))))
      (should (not (equal post-final post-uniform))))))

(ert-deftest arxana-vsatarcs-likelihood-aggregated-signed-error-sign-tracks-overshoot ()
  ;; Observation higher than prediction in all positive-sign channels →
  ;; positive aggregate.
  (let* ((b (arxana-vsatarcs-likelihood-test--uniform-prior-belief))
         (preds (arxana-vsatarcs-likelihood-predict-observation b))
         (high-obs (list :story-coverage 1.0 :lift-freshness 1.0 :annotation-overlay-presence 1.0))
         (low-obs (list :story-coverage 0.0 :lift-freshness 0.0 :annotation-overlay-presence 0.0))
         (high-errs (arxana-vsatarcs-likelihood-compute-prediction-errors high-obs preds))
         (low-errs (arxana-vsatarcs-likelihood-compute-prediction-errors low-obs preds))
         (high-agg (arxana-vsatarcs-likelihood--aggregated-signed-error high-errs))
         (low-agg (arxana-vsatarcs-likelihood--aggregated-signed-error low-errs)))
    (should (> high-agg 0))
    (should (< low-agg 0))))

(provide 'arxana-vsatarcs-likelihood-test)
;;; arxana-vsatarcs-likelihood-test.el ends here
