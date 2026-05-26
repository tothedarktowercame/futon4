;;; arxana-vsatarcs-effective-sign.el --- VSATARCS↔WM directional sign projection -*- lexical-binding: t; -*-

;;; Commentary:
;; Bridge-companion module providing the "effective-sign projection"
;; called out as a forward-pointer in the R3 aggregate-status
;; bilateral-evidence entry (per claude-2's review handoff 2026-05-20):
;;
;;   "Directional information placed differently — VSATARCs embeds it
;;    in the per-status-weight tables; WM separates it across two axes
;;    for R3d aggregation.  The bridge's effective-sign projection
;;    (when needed) collapses VSATARCs's weights to a +1/-1 axis for
;;    direct comparison."
;;
;; This module computes the projection on the VSATARCs side from the
;; declared weight tables in `arxana-vsatarcs-likelihood.el', exposes a
;; defcustom for the WM-side declared signs the operator transcribes
;; from WM clojure constants, and surfaces a side-by-side directional-
;; alignment snapshot.  Useful when an operator wants to verify that
;; both AIF subsystems agree on directional intent even though the
;; specific channel sets differ (WM observes 14 stack-fitness channels;
;; VSATARCs observes 5 essay-corpus channels — channel SETS differ, but
;; both sides should project consistently to +1/-1 axes).
;;
;; **Effective-sign computation (VSATARCs side):** for a per-channel
;; weight table mapping each `:state/*' status to a [0,1] weight, the
;; effective sign is `+1` when the healthy-direction statuses
;; (`strengthened` + `addressed`) carry STRICTLY HIGHER weight than the
;; unhealthy-direction statuses (`foreclosed` + `falsified`), `-1`
;; when STRICTLY LOWER, `0` when ambiguous (neither dominates).  This
;; matches the WM-side convention where `:sorry-count-norm` carries
;; sign `-1` because more sorrys = unhealthier observation.
;;
;; **Module discipline:** static projection over module-declared tables
;; (the weight tables are `defconst` in `arxana-vsatarcs-likelihood.el');
;; no live state, no XTDB queries, no file reads.  When the weight
;; tables get learned (R7 adaptive precision against prediction-error
;; history; eventually), the projection re-runs against the learned
;; tables — no API change needed.
;;
;; Contract: closes the v0.5.13 R3 audit row's effective-sign
;; forward-pointer; provides a bilateral-comparable directional axis
;; even when channel sets differ across sides.

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-likelihood)

(defgroup arxana-vsatarcs-effective-sign nil
  "Directional sign projection for VSATARCS↔WM bilateral comparison."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-effective-sign-wm-channels
  '((:annotation-health    . +1)
    (:sorry-count-norm     . -1)
    (:mission-health       . +1)
    (:active-repo-ratio    . +1)
    (:loop-health          . +1)
    (:support-coverage     . +1)
    (:stack-pct            . +1)
    (:depositing-signal    . +1)
    (:ticks-firing-ratio   . +1)
    (:commit-pct           . +1))
  "Operator-declared signs for WM-side channels (per WM `channel-health-signs').
Transcribed from claude-2's WM clojure code; updated when WM-side
channel set or signs change (claude-2 surfaces those changes via
handoff or future bilateral evidence entry).  Used by
`arxana-vsatarcs-effective-sign-snapshot' for the side-by-side
directional-alignment report.  Default lists 10 of WM's 14 channels;
remaining 4 are documented `:prototyping-forward' on the WM side
with no shipped sign yet."
  :type '(alist :key-type symbol :value-type integer)
  :group 'arxana-vsatarcs-effective-sign)

(defconst arxana-vsatarcs-effective-sign--healthy-statuses
  '(strengthened addressed)
  "Statuses considered the healthy direction.
Their cumulative weight defines the +1 pole of the effective-sign
axis.  Order doesn't matter (we take the max).")

(defconst arxana-vsatarcs-effective-sign--unhealthy-statuses
  '(foreclosed falsified)
  "Statuses considered the unhealthy direction.
Their cumulative weight defines the -1 pole.")

(defun arxana-vsatarcs-effective-sign--weight-of (table status)
  "Return the weight of STATUS in TABLE (alist), or 0.0 if absent."
  (or (cdr (assoc status table)) 0.0))

(defun arxana-vsatarcs-effective-sign--project-table (table)
  "Project a per-status weight TABLE to a +1/-1/0 effective sign.
+1 when the healthy-direction statuses dominate the unhealthy ones;
-1 when the reverse; 0 when ambiguous (weights tied).  The MAX
across each side's statuses is used so a single dominant healthy
status carries the projection even if the others are low."
  (let* ((healthy (apply #'max
                         (mapcar (lambda (s)
                                   (arxana-vsatarcs-effective-sign--weight-of
                                    table s))
                                 arxana-vsatarcs-effective-sign--healthy-statuses)))
         (unhealthy (apply #'max
                           (mapcar (lambda (s)
                                     (arxana-vsatarcs-effective-sign--weight-of
                                      table s))
                                   arxana-vsatarcs-effective-sign--unhealthy-statuses))))
    (cond ((> healthy unhealthy) +1)
          ((< healthy unhealthy) -1)
          (t 0))))

(defconst arxana-vsatarcs-effective-sign--channel-tables
  '((:story-coverage . arxana-vsatarcs-likelihood-story-coverage-weights)
    (:lift-freshness . arxana-vsatarcs-likelihood-lift-freshness-weights)
    (:annotation-overlay-presence
     . arxana-vsatarcs-likelihood-annotation-overlay-weights))
  "Alist mapping each likelihood channel to its weight-table symbol.
Explicit because the defconst names in `arxana-vsatarcs-likelihood.el'
don't follow a strict formula (`:annotation-overlay-presence' maps
to `arxana-vsatarcs-likelihood-annotation-overlay-weights' — dropped
the `-presence' suffix).  Adding a channel here is the schema-change
when the likelihood module gains a new weight table.")

(defun arxana-vsatarcs-effective-sign-vsatarcs-channels ()
  "Return an alist (CHANNEL . SIGN) for VSATARCs's likelihood channels.
Iterates `arxana-vsatarcs-effective-sign--channel-tables' and projects
each declared weight table.  Channels declared without a shipped
weight table (the `:prototyping-forward' ones) are omitted — they
have no projection yet."
  (let (out)
    (dolist (entry arxana-vsatarcs-effective-sign--channel-tables)
      (let* ((ch (car entry))
             (table-sym (cdr entry))
             (table (when (boundp table-sym) (symbol-value table-sym))))
        (when table
          (push (cons ch
                      (arxana-vsatarcs-effective-sign--project-table
                       table))
                out))))
    (nreverse out)))

(defun arxana-vsatarcs-effective-sign-vsatarcs-declared-vs-derived ()
  "Return per-channel `(:channel :declared :derived :match?)' rows.
Compares the author-declared signs in
`arxana-vsatarcs-likelihood-channel-health-signs' against the signs
derived from the weight tables via `--project-table'.  Useful as a
schema-consistency check: a mismatch surfaces when a learned-weights
update flips the directional intent without the author updating the
declared constant.  Today every channel matches (+1 declared, +1
derived); the check is forward-compatible discipline."
  (let* ((derived (arxana-vsatarcs-effective-sign-vsatarcs-channels))
         (declared arxana-vsatarcs-likelihood-channel-health-signs))
    (mapcar
     (lambda (entry)
       (let* ((ch (car entry))
              (drv (cdr entry))
              (decl (or (cdr (assoc ch declared)) 0)))
         (list :channel ch :declared decl :derived drv :match? (= decl drv))))
     derived)))

(defun arxana-vsatarcs-effective-sign--sign-counts (alist)
  "Return a count plist over the signs in ALIST (a (CHANNEL . SIGN) list).
Output is `(:pos N :neg N :zero N :total N)' for stable reporting."
  (let ((pos 0) (neg 0) (zero 0))
    (dolist (entry alist)
      (pcase (cdr entry)
        (1 (cl-incf pos))
        (-1 (cl-incf neg))
        (0 (cl-incf zero))))
    (list :pos pos :neg neg :zero zero :total (length alist))))

(defun arxana-vsatarcs-effective-sign-snapshot ()
  "Return the side-by-side directional-alignment snapshot.

The snapshot is a plist:

  (:vsatarcs-channels   ((<channel> . <sign>) ...)
   :vsatarcs-counts     (:pos N :neg N :zero N :total N)
   :wm-channels         ((<channel> . <sign>) ...)
   :wm-counts           (:pos N :neg N :zero N :total N)
   :digest-line         <one-line operator-facing summary>)

Channel SETS differ across sides by design (WM observes stack-fitness;
VSATARCs observes essay-corpus); the snapshot does NOT attempt
per-channel pairing.  What it surfaces: each side's apparatus has
internally-consistent directional intent (no channel projects to
both +1 and -1 on the same side; ambiguous-zero rows are flagged
operator-meaningfully).  This is the bilateral-comparable axis the
v0.5.13 R3 audit row's `effective-sign projection` forward-pointer
called out."
  (let* ((v-chs (arxana-vsatarcs-effective-sign-vsatarcs-channels))
         (w-chs (copy-sequence arxana-vsatarcs-effective-sign-wm-channels))
         (v-counts (arxana-vsatarcs-effective-sign--sign-counts v-chs))
         (w-counts (arxana-vsatarcs-effective-sign--sign-counts w-chs)))
    (list :vsatarcs-channels v-chs
          :vsatarcs-counts v-counts
          :wm-channels w-chs
          :wm-counts w-counts
          :digest-line
          (format "VSATARCs: %d+ / %d- / %d? (of %d); WM: %d+ / %d- / %d? (of %d)"
                  (plist-get v-counts :pos)
                  (plist-get v-counts :neg)
                  (plist-get v-counts :zero)
                  (plist-get v-counts :total)
                  (plist-get w-counts :pos)
                  (plist-get w-counts :neg)
                  (plist-get w-counts :zero)
                  (plist-get w-counts :total)))))

(provide 'arxana-vsatarcs-effective-sign)
;;; arxana-vsatarcs-effective-sign.el ends here
