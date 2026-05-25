;;; arxana-vsatarcs-precision.el --- R7 adaptive precision for VSATARCS -*- lexical-binding: t; -*-

;;; Commentary:
;; Adaptive precision tracking for the VSATARCS reader surface (R7).
;;
;; Maintains per-channel precision Π updated across ticks based on
;; rolling prediction-error history.  Ports the WM-side v0.12/v0.13
;; architecture from `~/code/futon2/src/futon2/aif/precision.clj' verbatim
;; in shape; content scoped to VSATARCs's 3 R3a-covered channels
;; (`:story-coverage', `:lift-freshness', `:annotation-overlay-presence').
;;
;; Π = variance-component + need-component (bounded by floor/cap)
;;
;;   variance-component = 1 / max(rolling-variance, min-variance)
;;     — standard Bayesian/AIF precision, sourced from prediction-error
;;       history (matches WM v0.12; variance-component-only baseline).
;;
;;   need-component = need-scale × max(0, channel-gap-from-preference)
;;     — ants-style need-modulated precision.  REMAINS 0.0 ON THIS SIDE
;;       until R5 (preferences) lands; preferences are part of
;;       writer-capability (R4/R5/R6), deferred.
;;
;; State shape per channel:
;;   (:precision Π
;;    :variance-component F
;;    :need-component F
;;    :error-history [recent prediction errors, bounded])
;;
;; **Cross-call persistence: the VSATARCs trace IS the precision
;; state's home.**  `follow-wm' reads the latest trace record's
;; `:precision-state' to continue the rolling window; first call
;; falls back to `initial-precision-state'.  Same "trace is the
;; state store" discipline as the WM side (per claude-2's
;; observation 2026-05-19).
;;
;; **R3b completes here, R3 aggregate closes.**  R3b (precision
;; weighting) was blocked on R7.  With R7 satisfied, R3b is
;; satisfied via this module's `weighted-error' (re-weights raw
;; per-call errors with adaptive precision).  Per the closure
;; protocol, the R3 aggregate verdict flips from
;; `:partial-three-of-four-sub-properties-satisfied' to `:satisfied'
;; when all four sub-properties (R3a, R3b, R3c, R3d) are independently
;; satisfied.

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-likelihood)

(defgroup arxana-vsatarcs-precision nil
  "Adaptive precision tracking for the VSATARCS reader surface (R7)."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-precision-window-size 20
  "Number of recent prediction errors retained per channel.
Ports WM-side `default-window-size 20'."
  :type 'integer
  :group 'arxana-vsatarcs-precision)

(defcustom arxana-vsatarcs-precision-min-variance 0.01
  "Floor for rolling-variance in the variance-component computation.
Prevents division by zero when error history is constant."
  :type 'number
  :group 'arxana-vsatarcs-precision)

(defcustom arxana-vsatarcs-precision-initial 1.0
  "Initial precision per channel before any error history accumulates."
  :type 'number
  :group 'arxana-vsatarcs-precision)

(defcustom arxana-vsatarcs-precision-need-scale 5.0
  "Scale factor for the need-component precision term.
Ports WM-side `default-need-scale 5.0'.  Has no effect on this side
until R5 preferences land — `preference-gap' returns 0 in the
absence of preferences, so need-component is 0 regardless of
need-scale."
  :type 'number
  :group 'arxana-vsatarcs-precision)

(defcustom arxana-vsatarcs-precision-floor 0.1
  "Lower bound on adaptive precision Π.
Ports WM-side `default-precision-floor 0.1'."
  :type 'number
  :group 'arxana-vsatarcs-precision)

(defcustom arxana-vsatarcs-precision-cap 200.0
  "Upper bound on adaptive precision Π.
Ports WM-side `default-precision-cap 200.0'."
  :type 'number
  :group 'arxana-vsatarcs-precision)

;; ---------------------------------------------------------------------
;; Internal helpers
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-precision--variance (xs)
  "Sample variance of a list XS of numbers.
Returns 0.0 for lists of length 0 or 1."
  (let ((n (length xs)))
    (if (< n 2)
        0.0
      (let* ((mean (/ (apply #'+ xs) (float n)))
             (sq-devs (mapcar (lambda (x) (let ((d (- x mean))) (* d d))) xs)))
        (/ (apply #'+ sq-devs) (float n))))))

(defun arxana-vsatarcs-precision--preference-gap (_channel-id _observed)
  "Distance from OBSERVED to CHANNEL-ID's preferred range.
Returns 0.0 unconditionally on this side until R5 (preferences)
lands.  When preferences exist, the gap will be `max(0, lo - v)' or
`max(0, v - hi)' per the WM-side pattern."
  0.0)

(defun arxana-vsatarcs-precision--need-component-for (channel-id observed need-scale)
  "Per-channel need-modulated precision component.
Returns 0.0 until R5 preferences are wired (see `--preference-gap')."
  (* (float need-scale)
     (arxana-vsatarcs-precision--preference-gap channel-id observed)))

(defun arxana-vsatarcs-precision--bounded-window (history new-error window-size)
  "Append NEW-ERROR to HISTORY; truncate to last WINDOW-SIZE entries."
  (let ((appended (append history (list (float new-error)))))
    (if (> (length appended) window-size)
        (nthcdr (- (length appended) window-size) appended)
      appended)))

(defun arxana-vsatarcs-precision--update-channel
    (channel-id channel-state new-error observed)
  "Apply one new (NEW-ERROR, OBSERVED) to a single channel's CHANNEL-STATE.
Returns the updated channel-state plist."
  (let* ((prev-history (plist-get channel-state :error-history))
         (window (arxana-vsatarcs-precision--bounded-window
                  prev-history new-error
                  arxana-vsatarcs-precision-window-size))
         (v (arxana-vsatarcs-precision--variance window))
         (variance-component (/ 1.0 (max v arxana-vsatarcs-precision-min-variance)))
         (need-component (arxana-vsatarcs-precision--need-component-for
                          channel-id observed
                          arxana-vsatarcs-precision-need-scale))
         (raw (+ variance-component need-component))
         (precision (max arxana-vsatarcs-precision-floor
                         (min arxana-vsatarcs-precision-cap raw))))
    (list :precision precision
          :variance-component variance-component
          :need-component need-component
          :error-history window)))

;; ---------------------------------------------------------------------
;; Public: initial + update + lookup + re-weight
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-precision-initial-state (&optional channels)
  "Construct an initial precision state covering CHANNELS (or all R3a channels).
Each channel starts with the configured initial precision and an
empty error history.  Returns an alist of (channel-keyword
. channel-state-plist)."
  (let ((chs (or channels arxana-vsatarcs-likelihood-channels-with-likelihood)))
    (mapcar (lambda (ch)
              (cons ch (list :precision arxana-vsatarcs-precision-initial
                             :variance-component 0.0
                             :need-component 0.0
                             :error-history nil)))
            chs)))

(defun arxana-vsatarcs-precision-update-state (prev-state errors)
  "Update PREV-STATE given a fresh ERRORS plist.

PREV-STATE is an alist of (channel-keyword . channel-state-plist).
ERRORS is a plist of (channel-keyword <error-report>) pairs as
returned by `arxana-vsatarcs-likelihood-compute-prediction-errors'.

Returns the updated alist.  Channels in ERRORS without a prev-state
entry are initialised; channels in prev-state without a new error are
passed through unchanged."
  (let* ((touched-channels nil)
         (pl errors)
         (updated nil))
    ;; Build updated entries for every channel mentioned in ERRORS
    (while pl
      (let* ((ch (car pl))
             (err-map (cadr pl))
             (new-err (or (plist-get err-map :error) 0.0))
             (observed (or (plist-get err-map :observed) 0.0))
             (channel-state
              (or (cdr (assoc ch prev-state))
                  (list :precision arxana-vsatarcs-precision-initial
                        :variance-component 0.0
                        :need-component 0.0
                        :error-history nil)))
             (next (arxana-vsatarcs-precision--update-channel
                    ch channel-state new-err observed)))
        (push (cons ch next) updated)
        (push ch touched-channels))
      (setq pl (cddr pl)))
    ;; Pass-through untouched
    (dolist (kv prev-state)
      (unless (cl-find (car kv) touched-channels :test #'equal)
        (push kv updated)))
    (nreverse updated)))

(defun arxana-vsatarcs-precision-for (precision-state channel-id)
  "Look up the current precision for CHANNEL-ID in PRECISION-STATE.
Returns `arxana-vsatarcs-precision-initial' if CHANNEL-ID is absent."
  (let ((channel-state (cdr (assoc channel-id precision-state))))
    (or (and channel-state (plist-get channel-state :precision))
        arxana-vsatarcs-precision-initial)))

(defun arxana-vsatarcs-precision-weighted-error (precision-state channel-id error-map)
  "Re-weight ERROR-MAP using the adaptive precision from PRECISION-STATE.

Returns a new error-map with:
  :precision           ← `arxana-vsatarcs-precision-for'
  :weighted-error      ← :error × :precision
  :per-call-precision  ← the original :precision from the input
                         (preserved so the trace records both sources).

When PRECISION-STATE lacks CHANNEL-ID's entry, falls back to
`arxana-vsatarcs-precision-initial'."
  (let* ((adaptive (arxana-vsatarcs-precision-for precision-state channel-id))
         (per-call (plist-get error-map :precision))
         (err (or (plist-get error-map :error) 0.0))
         ;; Copy the input plist (so we don't mutate the caller's)
         (out (copy-sequence error-map)))
    (plist-put out :precision adaptive)
    (plist-put out :weighted-error (* (float err) adaptive))
    (plist-put out :per-call-precision per-call)
    out))

(defun arxana-vsatarcs-precision-reweight-all (precision-state errors)
  "Re-weight every channel in ERRORS using PRECISION-STATE.
Returns a new errors plist with each channel's error-map re-weighted
via `arxana-vsatarcs-precision-weighted-error'."
  (let (out (pl errors))
    (while pl
      (let* ((ch (car pl))
             (err-map (cadr pl))
             (re (arxana-vsatarcs-precision-weighted-error
                  precision-state ch err-map)))
        ;; Push ch before re so after nreverse the order is (ch re ch re ...)
        (push ch out)
        (push re out))
      (setq pl (cddr pl)))
    (nreverse out)))

(provide 'arxana-vsatarcs-precision)
;;; arxana-vsatarcs-precision.el ends here
