;;; arxana-vsatarcs-r6-softmax.el --- R6 softmax + abstain selection mechanic -*- lexical-binding: t; -*-

;;; Commentary:
;; R6 (softmax action selection with abstain) for the VSATARCs writer
;; surface.  Today's apparatus ships **degenerate mode**: the
;; single-candidate flow is real (consent-gate dispatches between
;; `:confirm' / `:reject' / `:ignore' / `:abstain-for-now' per
;; `arxana-vsatarcs-consent.el'); the multi-candidate softmax-from-EFE
;; mechanic is gated on R5 (EFE composition not yet shipped).
;;
;; This module wraps the existing consent-gate primitives with the
;; selection-shape R6 requires.  Two paths:
;;
;;   - **1 candidate** (degenerate): identity select; return rank 1
;;     with weight 1.0; abstain becomes possible via the consent-gate's
;;     own abstain semantics.  This path is operational TODAY.
;;
;;   - **2+ candidates** (full softmax): would compute softmax weights
;;     over per-candidate G-totals from R5's EFE composition, sample
;;     or pick rank 1 by weight, abstain when no candidate exceeds the
;;     `:abstain-threshold' G-value.  Today this path signals
;;     `:r5-pending' because no per-candidate G score exists yet.
;;
;; **Failing-tests-as-documentation discipline** (Joe directive
;; 2026-05-20: 'ship the degenerate version first, put in failing
;; tests, and show they are fixed when we ship R5'): the test suite
;; includes `:expected-result :failed' tests that exercise the
;; multi-candidate softmax + threshold-abstain semantics.  Today they
;; fail because the apparatus signals `:r5-pending'.  When R5 ships
;; and the EFE module is wired in here, the tests start passing —
;; ert flags them as `unexpected pass' which we then remove the
;; `:expected-result :failed' tag from (the test becomes a regular
;; pass + the v0.5.x revision notes the closure-by-test-flip).
;;
;; The failing tests document what closing R6 fully requires; the
;; passing tests document what's operational today.
;;
;; Contract: closes R6 in degenerate mode (single-candidate flow);
;; failing tests document the R5-pending multi-candidate path.

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-consent)
(require 'arxana-vsatarcs-efe)

(defgroup arxana-vsatarcs-r6-softmax nil
  "R6 softmax + abstain selection for the VSATARCs writer surface."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-r6-softmax-tau 0.16
  "Softmax temperature for multi-candidate selection.
Default 0.16 mirrors WM-side `futon2.aif.policy' v0.5 default (which
the live WM-side smoke 2026-05-19 trace records observed as
`tau = 0.164').  Lower tau sharpens selection (more concentrated on
top-G); higher tau diffuses (more probability on lower-ranked
alternatives).  Effective only on the multi-candidate path (R5
pending today)."
  :type 'number
  :group 'arxana-vsatarcs-r6-softmax)

(defcustom arxana-vsatarcs-r6-softmax-abstain-g-threshold -3.0
  "G-total threshold below which all candidates trigger abstain.
A candidate must have G-total > threshold to be eligible for
selection (lower G = better in the WM convention; so 'above
threshold' here means 'sufficiently bad to require abstain').
Effective only on the multi-candidate path (R5 pending today)."
  :type 'number
  :group 'arxana-vsatarcs-r6-softmax)

(defun arxana-vsatarcs-r6-softmax--has-g-scores? (candidates)
  "Return non-nil when every candidate carries a `:G-total' numeric field.
Today returns nil for any multi-candidate input because the
EFE composition module that would populate `:G-total' hasn't
shipped (R5 pending).  When R5 lands, candidates produced by the
proposer will carry `:G-total' and this predicate becomes the
gate that routes to full softmax rather than degenerate identity."
  (and candidates
       (cl-every (lambda (c)
                   (numberp (plist-get c :G-total)))
                 candidates)))

(defun arxana-vsatarcs-r6-softmax--degenerate-select (candidate)
  "Return the single-candidate selection result for CANDIDATE.
Identity selection: the one candidate is rank 1 with weight 1.0,
tau unused.  The consent-gate is the operator-visible decision
surface for this path; this module returns the structural shape."
  (list :action candidate
        :rank 1
        :weight 1.0
        :tau nil
        :mode :degenerate-single-candidate
        :gap-report nil))

(defun arxana-vsatarcs-r6-softmax--abstain-empty ()
  "Return the abstain-on-empty-candidates result."
  (list :action nil
        :rank nil
        :weight nil
        :tau nil
        :mode :abstain-empty-candidate-set
        :gap-report :no-candidates))

(defun arxana-vsatarcs-r6-softmax--r5-pending (candidates)
  "Return the R5-pending signal for a multi-candidate input.
Today's apparatus cannot rank multiple candidates without per-candidate
G-totals from the (not-yet-shipped) EFE composition.  Returns a
result-plist whose `:mode' is `:r5-pending' so callers can
distinguish 'apparatus chose abstain' from 'apparatus couldn't
choose because R5 hasn't shipped'."
  (list :action nil
        :rank nil
        :weight nil
        :tau nil
        :mode :r5-pending
        :gap-report (list :r5-pending-candidates (length candidates))))

(defun arxana-vsatarcs-r6-softmax--softmax-weights (g-totals tau)
  "Compute softmax weights from G-TOTALS (a list of numbers) at TAU.
Lower G = higher weight (WM convention: minimize free energy).
Returns a list of normalised weights summing to 1.0.  Available
today as a pure helper so tests can exercise the math even without
the EFE composition wiring."
  (let* ((negs (mapcar (lambda (g) (/ (- g) (float tau))) g-totals))
         (m (apply #'max negs))
         (exps (mapcar (lambda (x) (exp (- x m))) negs))
         (z (apply #'+ exps)))
    (mapcar (lambda (e) (/ e z)) exps)))

(defun arxana-vsatarcs-r6-softmax--full-softmax-select (candidates)
  "Multi-candidate softmax selection (R5-pending today).
Computes softmax weights over candidates' `:G-total' fields at
`arxana-vsatarcs-r6-softmax-tau'; picks the rank-1 by weight; emits
`:abstain' when no candidate is above
`arxana-vsatarcs-r6-softmax-abstain-g-threshold'.

Today this path is NOT reached from `select-action' because the
gate `--has-g-scores?' returns nil for candidates lacking G-total.
When R5 ships and the proposer attaches G-totals, the gate flips
and this function becomes live.  Tests exercise it directly via
synthetic G-totals — those tests pass once R5 wires."
  (let* ((g-totals (mapcar (lambda (c) (plist-get c :G-total)) candidates))
         (best-g (apply #'max g-totals))
         (any-above? (> best-g arxana-vsatarcs-r6-softmax-abstain-g-threshold)))
    (if (not any-above?)
        (list :action nil
              :rank nil
              :weight nil
              :tau arxana-vsatarcs-r6-softmax-tau
              :mode :abstain-threshold
              :gap-report (list :best-g best-g
                                :threshold
                                arxana-vsatarcs-r6-softmax-abstain-g-threshold))
      (let* ((weights (arxana-vsatarcs-r6-softmax--softmax-weights
                       g-totals arxana-vsatarcs-r6-softmax-tau))
             (paired (cl-mapcar #'cons candidates weights))
             (sorted (sort (copy-sequence paired)
                           (lambda (a b) (> (cdr a) (cdr b)))))
             (rank-1 (car sorted)))
        (list :action (car rank-1)
              :rank 1
              :weight (cdr rank-1)
              :tau arxana-vsatarcs-r6-softmax-tau
              :mode :softmax-multi-candidate
              :gap-report nil)))))

(defun arxana-vsatarcs-r6-softmax-select-action (candidates)
  "Select an action from CANDIDATES per R6's softmax + abstain mechanic.
CANDIDATES is a list of action plists (each per
`arxana-vsatarcs-writer-actions' shape).  Returns a result-plist
with `:action :rank :weight :tau :mode :gap-report'.

Four operational paths:
  - 0 candidates  → `:mode :abstain-empty-candidate-set'
  - 1 candidate   → degenerate identity select; `:mode :degenerate-single-candidate'
  - 2+ candidates → auto-enriched via `arxana-vsatarcs-efe-enrich-candidates'
                    (since v0.5.27, when R5's EFE composition shipped);
                    if all candidates' G-total exceeds the abstain
                    threshold → `:mode :abstain-threshold'; otherwise
                    → `:mode :softmax-multi-candidate'.

**v0.5.26 → v0.5.27 closure flip**: previously the 2+-candidate path
returned `:mode :r5-pending' for candidates lacking `:G-total' (the
v0.5.26 R5-pending signal).  Since v0.5.27 ships EFE via
`arxana-vsatarcs-efe.el', the public API auto-enriches candidates
before the `--has-g-scores?' gate — so multi-candidate input
now routes to `--full-softmax-select' automatically.  The three
v0.5.26 `:expected-result :failed' tests start passing as the
operational signal of R5's closure."
  (cond
   ((null candidates)
    (arxana-vsatarcs-r6-softmax--abstain-empty))
   ((= 1 (length candidates))
    (arxana-vsatarcs-r6-softmax--degenerate-select (car candidates)))
   (t
    (let ((enriched (arxana-vsatarcs-efe-enrich-candidates candidates)))
      (if (arxana-vsatarcs-r6-softmax--has-g-scores? enriched)
          (arxana-vsatarcs-r6-softmax--full-softmax-select enriched)
        ;; Defensive fallback: enrichment failed to attach G-totals for
        ;; some candidate (unknown action class without per-class
        ;; pragmatic helper).  Signal :r5-pending honestly rather than
        ;; producing a partial-ranking.
        (arxana-vsatarcs-r6-softmax--r5-pending enriched))))))

(provide 'arxana-vsatarcs-r6-softmax)
;;; arxana-vsatarcs-r6-softmax.el ends here
