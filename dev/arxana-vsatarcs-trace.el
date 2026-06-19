;;; arxana-vsatarcs-trace.el --- R8 per-tick trace for VSATARCS -*- lexical-binding: t; -*-

;;; Commentary:
;; Per-tick trace persistence for the VSATARCS reader surface — R8 of
;; the standard AIF completeness contract
;; (`futon4/docs/vsatarcs-alignment-completeness.md').
;;
;; Design (per Joe 2026-05-18):
;;
;; - **Event-source agnostic.** `arxana-vsatarcs-trace-emit' takes a
;;   record from any caller (tests; the cadence-follower; future reader
;;   event hooks) and appends it to the day's trace file.  The caller
;;   is responsible for filling `:source' + `:tick-kind'.
;;
;; - **Load-bearing use case: cadence-following.**
;;   `arxana-vsatarcs-trace-follow-wm' consumes WM trace records the
;;   VSATARCS side hasn't yet followed and emits a corresponding tick
;;   for each.  The VSATARCS tick carries the same `:timestamp' as the
;;   WM record, plus a `:wm-trace-anchor' naming the source file +
;;   record index, plus the bridge-fetch result at that moment.
;;
;;   The "docs always in line with code" guarantee is operational:
;;   every WM tick has a matching VSATARCS tick at the same timestamp
;;   carrying the bridge comparison.  An analytic joins by anchor.
;;
;; - **The trace IS the stamp.**  No separate last-followed-index file:
;;   `--last-followed-wm-index' scans the existing VSATARCS trace for
;;   `:wm-trace-anchor :line-index' entries and returns the max.
;;
;; - **Record shape: flat plists of primitives + nested plists.**  We
;;   avoid alist-with-dotted-pairs because elisp's cons-cell printer
;;   produces `(a . b)' which our minimal EDN reader doesn't round-trip
;;   cleanly (the `.' is parsed as a symbol).  Belief is stored as
;;   summaries (entity-count, max/min entropy) rather than full alists.
;;   Bridge-snapshot is stored as counts.  Full belief is recoverable
;;   on demand via the in-memory state; historical belief in trace is
;;   a future move with a proper serializer.
;;
;; Forward-compatible holes per WM R8: `:prediction-errors',
;; `:candidates', `:per-term-EFE', `:chosen-action', `:tau', `:F' all
;; initialised to nil and populated as future R-criteria land.

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-belief)
(require 'arxana-vsatarcs-observation)
(require 'arxana-vsatarcs-wm-bridge)
(require 'arxana-vsatarcs-likelihood) ; v0.5.1: F-total + prediction-errors per tick
(require 'arxana-vsatarcs-precision) ; v0.5.3: R7 adaptive precision cross-call
(require 'arxana-browser-rewrites) ; shared EDN reader

(defgroup arxana-vsatarcs-trace nil
  "Per-tick trace persistence for the VSATARCS reader surface (R8)."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-trace-store-directory
  (expand-file-name "~/code/futon4/data/vsatarcs-trace/")
  "Directory where VSATARCS per-day trace files live.
Default is `~/code/futon4/data/vsatarcs-trace/'.  Each day's records
live in `vsatarcs-trace-YYYY-MM-DD.edn' as EDN-lines (one record per
line, append-only).  Mirrors the shape of `~/code/futon2/data/wm-trace/'."
  :type 'directory
  :group 'arxana-vsatarcs-trace)

(defconst arxana-vsatarcs-trace-version 1
  "Version of the VSATARCS trace record schema.
Bump on any change to the canonical record shape.")

;; ---------------------------------------------------------------------
;; Path + record-shape helpers
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-trace--today-iso ()
  "Return today's date as YYYY-MM-DD."
  (format-time-string "%Y-%m-%d"))

(defun arxana-vsatarcs-trace--now-iso ()
  "Return current time as ISO-8601 with milliseconds."
  (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t))

(defun arxana-vsatarcs-trace-store-path-for-date (date)
  "Return the trace file path for DATE (a YYYY-MM-DD string)."
  (expand-file-name (format "vsatarcs-trace-%s.edn" date)
                    arxana-vsatarcs-trace-store-directory))

(cl-defun arxana-vsatarcs-trace-build-record
    (&key tick-kind source trigger timestamp
          observation belief-summary
          wm-trace-anchor bridge-snapshot
          prediction-errors F precision-state)
  "Build a canonical VSATARCS trace record.
TIMESTAMP defaults to now (call to `arxana-vsatarcs-trace--now-iso').
The record's `:emitted-at' is always the current time; `:timestamp'
may name a different moment when following a foreign cadence (e.g.,
the WM record's timestamp when SOURCE is `:wm-following').

PREDICTION-ERRORS and F (v0.5.1, R9 F-decrease wiring): when provided,
populate the corresponding forward-compat fields.  PREDICTION-ERRORS
is a plist of (channel <error-report>) pairs from
`arxana-vsatarcs-likelihood-compute-prediction-errors'.  F is the
F-total scalar from `arxana-vsatarcs-likelihood-compute-vfe' — the
canonical R9 substrate for the F-decrease named property.

PRECISION-STATE (v0.5.3, R7 adaptive precision): the per-channel
precision state after this tick's update.  Read back from the
latest trace record by the next `follow-wm' call to continue the
rolling window.  Trace IS the state store for precision.

Forward-compatible holes still initialised to nil: `:candidates',
`:per-term-EFE', `:chosen-action' (R4/R5/R6 — writer capability)."
  (let ((now (arxana-vsatarcs-trace--now-iso)))
    (list :trace-version arxana-vsatarcs-trace-version
          :timestamp (or timestamp now)
          :emitted-at now
          :tick-kind tick-kind
          :source source
          :trigger trigger
          :observation observation
          :belief-summary belief-summary
          :wm-trace-anchor wm-trace-anchor
          :bridge-snapshot bridge-snapshot
          :prediction-errors prediction-errors
          :precision-state precision-state
          :candidates nil
          :per-term-EFE nil
          :chosen-action nil
          :tau nil
          :F F)))

(defun arxana-vsatarcs-trace-belief-summary (&optional belief)
  "Return a summary plist of BELIEF (or current state).
Shape: (:entity-count N :max-entropy F :min-entropy F).  Returns
(:entity-count 0 :max-entropy nil :min-entropy nil) when no belief is
tracked."
  (let ((src (or belief (arxana-vsatarcs-belief-current))))
    (if (null src)
        (list :entity-count 0 :max-entropy nil :min-entropy nil)
      (let ((entropies (mapcar (lambda (kv)
                                 (arxana-vsatarcs-belief-entropy (cdr kv)))
                               src)))
        (list :entity-count (length src)
              :max-entropy (apply #'max entropies)
              :min-entropy (apply #'min entropies))))))

;; ---------------------------------------------------------------------
;; Emit (source-agnostic): append RECORD as one EDN line
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-trace-emit (record &optional path)
  "Append RECORD as one EDN-form line to PATH (defaults to today's trace).
Creates parent directories as needed.  Returns the path written to.
RECORD is assumed to be `prin1'-safe (flat plists of primitives +
nested plists; no cons-cell dotted pairs)."
  (let ((target (or path
                    (arxana-vsatarcs-trace-store-path-for-date
                     (arxana-vsatarcs-trace--today-iso)))))
    (make-directory (file-name-directory target) t)
    (with-temp-buffer
      (let ((print-length nil)
            (print-level nil))
        (prin1 record (current-buffer)))
      (insert "\n")
      (let ((coding-system-for-write 'utf-8))
        (write-region (point-min) (point-max) target t 'silent)))
    target))

;; ---------------------------------------------------------------------
;; Read-back (for tests + follow-wm's stamp lookup)
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-trace--read-all-from-path (path)
  "Read every record from PATH (EDN-lines) as a list of plists.
Returns nil if PATH is unreadable.  Skips blank lines."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (let (records)
        (goto-char (point-min))
        (while (not (eobp))
          (let* ((line-start (point))
                 (line-end (line-end-position))
                 (line (buffer-substring-no-properties line-start line-end)))
            (when (string-match-p "[^ \t\r\n]" line)
              (with-temp-buffer
                (insert line)
                (goto-char (point-min))
                (push (arxana-browser-rewrites--edn-read-form) records)))
            (goto-char line-end)
            (unless (eobp) (forward-char 1))))
        (nreverse records)))))

(defun arxana-vsatarcs-trace-read-all (&optional date)
  "Return all VSATARCS trace records for DATE (defaults to today)."
  (let ((path (arxana-vsatarcs-trace-store-path-for-date
               (or date (arxana-vsatarcs-trace--today-iso)))))
    (arxana-vsatarcs-trace--read-all-from-path path)))

(defun arxana-vsatarcs-trace-read-latest (&optional date)
  "Return the last VSATARCS trace record for DATE (defaults to today)."
  (car (last (arxana-vsatarcs-trace-read-all date))))

;; ---------------------------------------------------------------------
;; Cadence-following: emit one VSATARCS tick per unseen WM tick
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-trace--last-followed-wm-index (date)
  "Return the max `:line-index' in DATE's VSATARCS trace's `:wm-trace-anchor's.
Returns -1 if no anchored records exist for DATE."
  (let ((records (arxana-vsatarcs-trace-read-all date))
        (max-idx -1))
    (dolist (r records)
      (let* ((anchor (plist-get r :wm-trace-anchor))
             (idx (and anchor (plist-get anchor :line-index))))
        (when (and (numberp idx) (> idx max-idx))
          (setq max-idx idx))))
    max-idx))

(defun arxana-vsatarcs-trace--bridge-snapshot-from-wm-record (wm-record)
  "Build a compact bridge-snapshot plist from one WM-RECORD's `:mu-post'.
Compares the converted WM belief against the local in-memory belief
state via `arxana-vsatarcs-belief-compare', filters the meta-sorry
keyword(s) per `arxana-vsatarcs-wm-bridge-expected-only-in-wm', and
returns a flat plist of counts (prin1-safe; no nested alists)."
  (let* ((mu-post (plist-get wm-record :mu-post))
         (wm-belief (when mu-post
                      (arxana-vsatarcs-wm-bridge--belief-from-mu-post mu-post)))
         (local (arxana-vsatarcs-belief-current)))
    (if (null wm-belief)
        (list :wm-belief-available? nil)
      (let* ((raw (arxana-vsatarcs-belief-compare local wm-belief))
             (only-a (plist-get raw :only-in-a))
             (only-b (plist-get raw :only-in-b))
             (filtered-b
              (arxana-vsatarcs-wm-bridge--filter-expected only-b))
             (expected-b
              (cl-remove-if-not
               (lambda (id)
                 (cl-find id arxana-vsatarcs-wm-bridge-expected-only-in-wm
                          :test #'equal))
               only-b))
             (diffs (plist-get raw :posterior-diffs))
             (max-diff (if diffs
                           (apply #'max (mapcar #'cdr diffs))
                         0.0)))
        (list :wm-belief-available? t
              :only-in-local-count (length only-a)
              :only-in-wm-side-count (length filtered-b)
              :expected-in-wm-only-count (length expected-b)
              :posterior-diffs-count (length diffs)
              :max-posterior-diff max-diff
              :equal-count (plist-get raw :equal-count))))))

(defun arxana-vsatarcs-trace-follow-wm (&optional date)
  "Follow WM trace cadence: emit one VSATARCS tick per unseen WM record.
DATE defaults to today (YYYY-MM-DD).  For each WM record at index > the
last-followed index, builds a `:tick-kind :wm-bridge-fetch' VSATARCS
tick carrying:
  - `:timestamp' = the WM record's `:timestamp' (cadence-aligned)
  - `:source' = `:wm-following'
  - `:wm-trace-anchor' = (:path :line-index :wm-timestamp)
  - `:bridge-snapshot' = drift counts from comparing local vs WM belief
  - `:observation' = current channel-map (per R2)
  - `:belief-summary' = current belief state summary
and appends it via `arxana-vsatarcs-trace-emit'.  Returns the number of
new ticks emitted."
  (interactive)
  (let* ((date (or date (arxana-vsatarcs-trace--today-iso)))
         (wm-path (arxana-vsatarcs-wm-bridge--trace-path-for-date date))
         (wm-records (arxana-vsatarcs-trace--read-all-from-path wm-path))
         (last-idx (arxana-vsatarcs-trace--last-followed-wm-index date))
         (observation (arxana-vsatarcs-observe))
         (belief-summary (arxana-vsatarcs-trace-belief-summary))
         ;; v0.5.1 — R9 F-decrease wiring: compute VFE shape against
         ;; the current local belief + observation.  F-total is the
         ;; substrate for the F-decrease named validation property.
         (vfe (arxana-vsatarcs-likelihood-compute-vfe observation))
         (f-total (plist-get vfe :F-total))
         (per-call-errors (plist-get vfe :per-channel-errors))
         ;; v0.5.3 — R7 adaptive precision: read latest VSATARCS trace
         ;; record's :precision-state to continue the rolling window
         ;; across calls (the trace IS the state store).  Update the
         ;; state given this call's per-channel errors, then re-weight
         ;; the errors with the updated state for storage.
         (prev-precision-state
          (or (plist-get (arxana-vsatarcs-trace-read-latest date)
                         :precision-state)
              (arxana-vsatarcs-precision-initial-state)))
         (next-precision-state
          (arxana-vsatarcs-precision-update-state
           prev-precision-state per-call-errors))
         (pred-errors
          (arxana-vsatarcs-precision-reweight-all
           next-precision-state per-call-errors))
         (emitted 0))
    (when wm-records
      (cl-loop
       for r in wm-records
       for idx from 0
       when (> idx last-idx) do
       (let* ((wm-ts (plist-get r :timestamp))
              (snapshot (arxana-vsatarcs-trace--bridge-snapshot-from-wm-record r))
              (anchor (list :path wm-path
                            :line-index idx
                            :wm-timestamp wm-ts))
              (rec (arxana-vsatarcs-trace-build-record
                    :tick-kind :wm-bridge-fetch
                    :source :wm-following
                    :trigger date
                    :timestamp wm-ts
                    :observation observation
                    :belief-summary belief-summary
                    :wm-trace-anchor anchor
                    :bridge-snapshot snapshot
                    :prediction-errors pred-errors
                    :precision-state next-precision-state
                    :F f-total)))
         ;; Emit to the DATE-specific VSATARCS trace file (not today's
         ;; wall-clock file).  The "docs always in line with code"
         ;; guarantee means a VSATARCS tick lands in the same day-bucket
         ;; as the WM record it follows, regardless of when follow-wm
         ;; runs.  Tested by `follow-wm-timestamps-align-with-wm'.
         (arxana-vsatarcs-trace-emit
          rec (arxana-vsatarcs-trace-store-path-for-date date))
         (cl-incf emitted))))
    (when (called-interactively-p 'interactive)
      (message "VSATARCS trace: emitted %d new tick(s) following %s"
               emitted (file-name-nondirectory wm-path)))
    emitted))

(provide 'arxana-vsatarcs-trace)
;;; arxana-vsatarcs-trace.el ends here
