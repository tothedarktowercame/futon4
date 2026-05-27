;;; arxana-vsatarcs-likelihood.el --- R3a + R3c likelihood model for VSATARCS -*- lexical-binding: t; -*-

;;; Commentary:
;; Likelihood model + prediction-error machinery + variational free
;; energy for the VSATARCS reader surface.  Satisfies R3a (prediction
;; error per observation channel) and partially R3c (VFE shape over
;; the likelihood).
;;
;; Ports the v0.10/v0.11/v0.13/v0.16 WM-side pattern from
;; `~/code/futon2/src/futon2/aif/belief.clj' (claude-2's audit-driven
;; landings).  The VSATARCs side uses the same architectural shape —
;; per-status weight tables, per-entity expected-channel-contribution,
;; mean-entropy-variance, multi-step inner loop with K=3 anneal-factor —
;; with a different channel content (essay-corpus channels, not
;; stack-fitness channels).
;;
;; Bilateral counterpart: WM-side `futon-aif-completeness.md §R3 v0.10
;; + v0.11 landing'.  Joint-landing milestone v0.5.0 ↔ WM v0.10+ per
;; whistle-coordination 2026-05-19.
;;
;; Per-status weights are hand-tuned for v0.5.0; replaceable by R7
;; (adaptive precision / learned weights).  Covered channels (3 of 5
;; R2 channels):
;;
;;   :story-coverage              status-derivable via 'engaged-with' weights
;;   :lift-freshness              status-derivable via 'recently-touched' weights
;;   :annotation-overlay-presence status-derivable via 'analytical-evidence' weights
;;
;; Uncovered channels (logged below as `:prototyping-forward'):
;;
;;   :scene-density               — structural corpus fact; no natural
;;                                  per-entity-status mapping (parallel to
;;                                  claude-2's 10 channel-likelihood sorries)
;;   :link-density                — same shape; structural

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-belief)
(require 'arxana-vsatarcs-observation)

(defgroup arxana-vsatarcs-likelihood nil
  "Likelihood model + prediction-error + VFE for the VSATARCS reader surface (R3a / R3c)."
  :group 'arxana-vsatarcs)

;; ---------------------------------------------------------------------
;; Per-status weight tables (hand-tuned v0.5.0; replaceable by R7)
;; ---------------------------------------------------------------------
;;
;; Each channel has a per-status weight table.  Per-entity expected
;; channel-contribution = Σ posterior[s] * weight[s].  Channel mean =
;; mean over entities.  Variance = mean posterior entropy normalised by
;; log(|status-set|).
;;
;; The weights are deliberately defensible-but-rough; they encode a
;; correlation hypothesis (e.g. "addressed entities are more likely to
;; have story refs lifted to disk") rather than a causal mechanism.
;; R7 (adaptive precision) will eventually learn them from
;; prediction-error history.

(defconst arxana-vsatarcs-likelihood-story-coverage-weights
  '((strengthened . 0.85)
    (addressed    . 0.75)
    (refined      . 0.55)
    (spawned      . 0.20)
    (reopened     . 0.40)
    (foreclosed   . 0.15)
    (falsified    . 0.05))
  "Per-status weights for the `:story-coverage' channel.
Hypothesis: entities believed to be addressed/strengthened are more
likely to have story refs lifted to disk; falsified/foreclosed less so.")

(defconst arxana-vsatarcs-likelihood-lift-freshness-weights
  '((strengthened . 0.80)
    (addressed    . 0.60)
    (refined      . 0.85)
    (spawned      . 0.70)
    (reopened     . 0.75)
    (foreclosed   . 0.25)
    (falsified    . 0.20))
  "Per-status weights for the `:lift-freshness' channel.
Hypothesis: entities in actively-transitional states (refined,
spawned, reopened) are more likely to have fresh on-disk lifts.")

(defconst arxana-vsatarcs-likelihood-annotation-overlay-weights
  '((strengthened . 0.70)
    (addressed    . 0.55)
    (refined      . 0.45)
    (spawned      . 0.15)
    (reopened     . 0.50)
    (foreclosed   . 0.25)
    (falsified    . 0.15))
  "Per-status weights for the `:annotation-overlay-presence' channel.
Hypothesis: entities believed to be addressed/strengthened are more
likely to have sibling `.aif.edn' overlays (the analytical evidence
that motivated marking them addressed in the first place).")

(defconst arxana-vsatarcs-likelihood-channels-with-likelihood
  '(:story-coverage :lift-freshness :annotation-overlay-presence)
  "Subset of R2 channels for which an R3a likelihood model exists.
Remaining R2 channels (`:scene-density', `:link-density') are
structural corpus facts without a natural per-entity-status mapping;
logged as `:prototyping-forward' below.")

(defconst arxana-vsatarcs-likelihood-prototyping-forward-channels
  '(:scene-density :link-density)
  "R2 channels without a likelihood model in v0.5.0.
Logged as `:prototyping-forward' (knowingly-incomplete v1, not technical
debt) per Joe's convention.  Predicted future move: either lift these
to belief-derivable channels via a richer status set, or admit them
as exogenous corpus-shape signals not predictable from belief.  The
parallel WM-side artefact is `~/code/futon2/data/sorrys.edn'
:sorry/r3d-per-entity-attribution + the 10 channel-likelihood sorrys
(see v0.11 closure note).")

(defconst arxana-vsatarcs-likelihood-channel-health-signs
  '((:story-coverage . 1)
    (:lift-freshness . 1)
    (:annotation-overlay-presence . 1))
  "Health-sign per channel for v0.16-style multi-channel R3d aggregation.
All three covered channels are sign +1 (higher = healthier).  Used in
the multi-step inner loop to combine per-channel signed weighted
errors into a single aggregate signal.")

(defcustom arxana-vsatarcs-likelihood-min-variance 0.01
  "Floor for predicted-variance in prediction-error precision computation.
Prevents division by zero when the likelihood reports certainty."
  :type 'number
  :group 'arxana-vsatarcs-likelihood)

(defcustom arxana-vsatarcs-likelihood-r3-max-steps 3
  "Inner-loop iteration limit for R3 multi-step predictive coding.
K=3 ports the v0.13 WM-side default.  Each step: predict → error →
update belief via synthesised events → next step.  Early termination
on small error magnitude."
  :type 'integer
  :group 'arxana-vsatarcs-likelihood)

(defcustom arxana-vsatarcs-likelihood-r3-error-eps 1.0e-3
  "Early-termination threshold for R3 multi-step inner loop.
When the aggregate error magnitude drops below this, the loop exits
even before reaching `arxana-vsatarcs-likelihood-r3-max-steps'."
  :type 'number
  :group 'arxana-vsatarcs-likelihood)

;; ---------------------------------------------------------------------
;; Per-entity + mean-entropy helpers
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-likelihood--entity-expected (posterior weights)
  "Per-entity expected channel contribution: Σ posterior[s] * weights[s]."
  (let ((sum 0.0))
    (dolist (kv posterior)
      (let ((w (cdr (assoc (car kv) weights))))
        (when w
          (setq sum (+ sum (* (cdr kv) w))))))
    sum))

(defun arxana-vsatarcs-likelihood--mean-entropy-variance (belief)
  "Shared variance shape: mean posterior entropy across BELIEF, normalised
against log(|status-set|).  Empty belief → 1.0 (maximally uncertain)."
  (if (null belief) 1.0
    (let* ((n (length belief))
           (max-entropy (log (length arxana-vsatarcs-belief-status-set)))
           (mean-entropy (/ (apply #'+ (mapcar (lambda (kv)
                                                 (arxana-vsatarcs-belief-entropy
                                                  (cdr kv)))
                                               belief))
                            (float n))))
      (if (> max-entropy 0) (/ mean-entropy max-entropy) 0.0))))

(defun arxana-vsatarcs-likelihood--predict-via-weights (belief weights)
  "Predict (:mean :variance) for a channel using BELIEF and WEIGHTS."
  (if (null belief)
      (list :mean 0.0 :variance 1.0)
    (let* ((n (length belief))
           (mean (/ (apply #'+ (mapcar (lambda (kv)
                                         (arxana-vsatarcs-likelihood--entity-expected
                                          (cdr kv) weights))
                                       belief))
                    (float n))))
      (list :mean (max 0.0 (min 1.0 mean))
            :variance (arxana-vsatarcs-likelihood--mean-entropy-variance belief)))))

;; ---------------------------------------------------------------------
;; Public: per-channel + composite predict
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-likelihood-predict-story-coverage (&optional belief)
  "Predict `(:mean :variance)' for `:story-coverage' given BELIEF.
BELIEF defaults to the current in-memory state."
  (arxana-vsatarcs-likelihood--predict-via-weights
   (or belief (arxana-vsatarcs-belief-current))
   arxana-vsatarcs-likelihood-story-coverage-weights))

(defun arxana-vsatarcs-likelihood-predict-lift-freshness (&optional belief)
  "Predict `(:mean :variance)' for `:lift-freshness' given BELIEF."
  (arxana-vsatarcs-likelihood--predict-via-weights
   (or belief (arxana-vsatarcs-belief-current))
   arxana-vsatarcs-likelihood-lift-freshness-weights))

(defun arxana-vsatarcs-likelihood-predict-annotation-overlay-presence (&optional belief)
  "Predict `(:mean :variance)' for `:annotation-overlay-presence' given BELIEF."
  (arxana-vsatarcs-likelihood--predict-via-weights
   (or belief (arxana-vsatarcs-belief-current))
   arxana-vsatarcs-likelihood-annotation-overlay-weights))

(defun arxana-vsatarcs-likelihood-predict-observation (&optional belief)
  "Predict observation distributions for all R3a-covered channels.
Returns a plist of (channel-keyword (:mean F :variance F)) pairs.
Channels not in `arxana-vsatarcs-likelihood-channels-with-likelihood'
are absent from the result; callers handle absence by falling back to
e.g. preference-gap-driven scoring."
  (let ((b (or belief (arxana-vsatarcs-belief-current))))
    (list :story-coverage
          (arxana-vsatarcs-likelihood-predict-story-coverage b)
          :lift-freshness
          (arxana-vsatarcs-likelihood-predict-lift-freshness b)
          :annotation-overlay-presence
          (arxana-vsatarcs-likelihood-predict-annotation-overlay-presence b))))

;; ---------------------------------------------------------------------
;; R3a + R3b: prediction error + precision-weighted error
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-likelihood-compute-prediction-error (observed prediction)
  "Compute prediction-error report for one channel.

OBSERVED is the channel's observed value (a number).  PREDICTION is
the likelihood output `(:mean :variance)'.

Returns a plist:
  (:observed F
   :predicted-mean F
   :predicted-variance F
   :error F          ; observed - predicted-mean (R3a)
   :precision F      ; 1 / max(variance, min-variance) (R3b)
   :weighted-error F ; error * precision)"
  (let* ((pm (float (or (plist-get prediction :mean) 0.0)))
         (pv (float (or (plist-get prediction :variance) 0.0)))
         (o (float (or observed 0.0)))
         (err (- o pm))
         (precision (/ 1.0 (max pv arxana-vsatarcs-likelihood-min-variance))))
    (list :observed o
          :predicted-mean pm
          :predicted-variance pv
          :error err
          :precision precision
          :weighted-error (* err precision))))

(defun arxana-vsatarcs-likelihood-compute-prediction-errors (observation predictions)
  "Compute prediction-error reports for every channel in PREDICTIONS.
OBSERVATION is the R2 channel-map (`arxana-vsatarcs-observe' output).
Returns a plist of (channel-keyword <error-report>) pairs covering
exactly the channels present in PREDICTIONS."
  (let (out)
    (dolist (ch arxana-vsatarcs-likelihood-channels-with-likelihood)
      (push (arxana-vsatarcs-likelihood-compute-prediction-error
             (plist-get observation ch)
             (plist-get predictions ch))
            out)
      (push ch out))
    out))

;; ---------------------------------------------------------------------
;; R3c: variational free energy shape
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-likelihood-compute-vfe (observation &optional belief)
  "Compute a VFE-shape free energy for OBSERVATION given BELIEF.
Returns a plist:
  (:F-total F             ; weighted sum: 0.65 accuracy + 0.35 complexity
   :F-accuracy F          ; mean(weighted-error^2) — fit quality
   :F-complexity F        ; mean-entropy-variance — belief uncertainty
   :per-channel-errors    ; plist of (channel <error-report>))

`F-total' is the VSATARCs analogue of WM's `G-total'.  Higher F-total
= worse fit (large prediction errors) or large belief uncertainty.
Lower F-total = belief explains observation well + is confident.

This is the R3c shape: a free-energy decomposition keyed off the
likelihood model.  Strict variational identification (D_KL +
log-evidence) is a future refinement; this is the operational shape
the WM side has used since v0.10."
  (let* ((b (or belief (arxana-vsatarcs-belief-current)))
         (predictions (arxana-vsatarcs-likelihood-predict-observation b))
         (errors (arxana-vsatarcs-likelihood-compute-prediction-errors
                  observation predictions))
         (per-channel-weighted-errors
          (mapcar (lambda (ch)
                    (let ((e (plist-get errors ch)))
                      (or (plist-get e :weighted-error) 0.0)))
                  arxana-vsatarcs-likelihood-channels-with-likelihood))
         (n (length arxana-vsatarcs-likelihood-channels-with-likelihood))
         (accuracy
          (if (zerop n) 0.0
            (/ (apply #'+ (mapcar (lambda (we) (* we we))
                                  per-channel-weighted-errors))
               (float n))))
         (complexity (arxana-vsatarcs-likelihood--mean-entropy-variance b))
         (f-total (+ (* 0.65 accuracy) (* 0.35 complexity))))
    (list :F-total f-total
          :F-accuracy accuracy
          :F-complexity complexity
          :per-channel-errors errors)))

;; ---------------------------------------------------------------------
;; R3 multi-step inner loop: K=3 with anneal-factor
;;
;; Ports the v0.13 WM-side pattern from war_machine.clj:2503-2575.
;; Each step:
;;   1. Predict observation from current belief.
;;   2. Compute prediction errors per channel.
;;   3. Aggregate per-channel signed weighted errors (v0.16-style sign-
;;      aware aggregation) into one signed magnitude.
;;   4. Anneal event weight by step (step 0 = full; step K-1 = small).
;;   5. Synthesise per-entity events of `:strengthened' (positive
;;      aggregate) or `:foreclosed' (negative aggregate) typed at the
;;      annealed weight, apply to belief.
;;   6. Recur or terminate.
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-likelihood--aggregated-signed-error (errors)
  "Aggregate signed weighted errors across covered channels.
Per channel, signed-error = weighted-error * health-sign (so high
:story-coverage = healthier = positive contribution when observed
exceeds predicted).  Returns a single number."
  (apply #'+
         (mapcar (lambda (ch)
                   (let* ((e (plist-get errors ch))
                          (we (or (plist-get e :weighted-error) 0.0))
                          (sign (or (cdr (assoc ch arxana-vsatarcs-likelihood-channel-health-signs))
                                    0)))
                     (* (float sign) we)))
                 arxana-vsatarcs-likelihood-channels-with-likelihood)))

(defun arxana-vsatarcs-likelihood-run-multi-step (observation &optional belief)
  "Run the R3 multi-step inner loop for OBSERVATION against BELIEF.
Returns a plist:
  (:belief <post-loop belief alist>
   :prediction-errors <final-step per-channel errors>
   :micro-step-trace <vector of per-step records>
   :terminated-at-step <0-indexed step at termination>
   :early-terminated? <bool>)

Each micro-step record is a plist:
  (:step N
   :error-magnitude F
   :aggregated-signed-error F
   :anneal-factor F
   :events-applied N
   :event-weight F)"
  (let* ((b (or belief (arxana-vsatarcs-belief-current)))
         (max-steps arxana-vsatarcs-likelihood-r3-max-steps)
         (eps arxana-vsatarcs-likelihood-r3-error-eps)
         (current b)
         (step 0)
         (micro nil)
         (final-errors nil)
         (early? nil))
    (catch 'done
      (while (< step max-steps)
        (let* ((preds (arxana-vsatarcs-likelihood-predict-observation current))
               (errs (arxana-vsatarcs-likelihood-compute-prediction-errors
                      observation preds))
               (agg (arxana-vsatarcs-likelihood--aggregated-signed-error errs))
               (mag (abs agg))
               (anneal (max 0.0 (- 1.0 (/ (float step) (float max-steps)))))
               (event-weight (* (min 1.0 mag) anneal 0.1))
               (events
                (when (> event-weight 0)
                  (let ((type (if (> agg 0) :strengthened :foreclosed)))
                    (mapcar (lambda (kv)
                              (list :entity-id (car kv)
                                    :type type
                                    :weight event-weight))
                            current))))
               (next (if events
                         (arxana-vsatarcs-belief-update-batch current events)
                       current)))
          (push (list :step step
                      :error-magnitude mag
                      :aggregated-signed-error agg
                      :anneal-factor anneal
                      :events-applied (length events)
                      :event-weight event-weight)
                micro)
          (setq final-errors errs)
          (setq current next)
          (when (< mag eps)
            (setq early? t)
            (throw 'done nil))
          (cl-incf step))))
    (list :belief current
          :prediction-errors final-errors
          :micro-step-trace (apply #'vector (nreverse micro))
          :terminated-at-step (min step (1- max-steps))
          :early-terminated? early?)))

(provide 'arxana-vsatarcs-likelihood)
;;; arxana-vsatarcs-likelihood.el ends here
