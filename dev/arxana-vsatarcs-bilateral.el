;;; arxana-vsatarcs-bilateral.el --- Bilateral-evidence block for VSATARCS reader chrome -*- lexical-binding: t; -*-

;;; Commentary:
;; Reader-criteria V-COV closure (Q7 of
;; `~/code/futon2/docs/vsatarcs-reader-criteria.md').  Reads the
;; top-level `:bilateral-evidence' block from this side's authoritative
;; `~/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn' and
;; exposes a snapshot the reader chrome surfaces alongside belief,
;; anticipation, and sorry-registry snapshots.
;;
;; The data already lives in this same .aif.edn — the entries
;; accumulate as bilateral milestones land (independent-naming,
;; joint-landing, independent-naming-of-same-r-criterion-shape, and
;; one-sided-extension are the four `:evidence-kind' values currently
;; in use).  Q7's lift is purely a renderer over data that's already
;; in canonical form on this side.
;;
;; The module does not maintain mutable state — every call re-reads the
;; .aif.edn.  Same discipline as anticipation and sorrys: the source
;; file is authoritative; the rendered surface is downstream.
;;
;; Contract: contributes to V-COV (coverage — reader can see the
;; cross-side correspondence audit trail at a glance).

;;; Code:

(require 'cl-lib)
(require 'arxana-browser-rewrites) ; shared EDN reader

(defgroup arxana-vsatarcs-bilateral nil
  "Bilateral-evidence snapshot for the VSATARCS reader surface."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-bilateral-aif-file
  (expand-file-name "~/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn")
  "Path to the authoritative .aif.edn carrying `:bilateral-evidence'.
This file is the canonical home for cross-side correspondence
records (per the v0.2.5 closure rationale); WM-side does not
duplicate the block."
  :type 'file
  :group 'arxana-vsatarcs-bilateral)

(defconst arxana-vsatarcs-bilateral-evidence-kinds
  '(:independent-naming-of-same-principle
    :joint-landing
    :independent-naming-of-same-r-criterion-shape-at-different-scopes
    :one-sided-extension
    :coordinated-empirical-observation
    :consent-gated-writer-event
    :symmetric-apparatus-port)
  "Closed set of `:evidence-kind' values used across bilateral entries.
The first five activated by v0.5.13; the sixth (`:consent-gated-writer-event')
activates at v0.5.22 with claude-2's M-vsatarcs-writer L4
`:aif-edn-revision-entry' shipment.  The seventh
(`:symmetric-apparatus-port') activates at v0.5.32 with the VSATARCs-side
port of claude-9's R12 narrow-take-up apparatus — entries of this kind
record the closure of a deferred-port where both AIF apparatuses gain
symmetric implementations of the same R-criterion's narrow take-up.
Kind name proposed by claude-9 in
`~/code/futon0/holes/handoffs/r12-vsatarcs-port-2026-05-21.md' §4.

Entries of `:consent-gated-writer-event' kind carry a `:writer-event-id'
cross-reference field naming the canonical writer-trace record id the
entry annotates.  Entries of `:symmetric-apparatus-port' kind carry
`:wm-impl-id' + `:vsatarcs-impl-id' pointing at the two symmetric
modules (e.g., `futon2.aif.intrinsic-values' ↔
`arxana-vsatarcs-intrinsic-values').

Entries carrying an unknown kind are bucketed under the symbol
`unknown' in the kind distribution.")

(defun arxana-vsatarcs-bilateral--strip-leading-colon (sym)
  "Return symbol SYM with leading `:' stripped and re-keyworded.
Helper for normalising `:evidence-kind' / `:principle' values
returned by the shared EDN reader (which interns keywords as symbols
whose names retain the leading colon)."
  (when sym
    (let ((s (symbol-name sym)))
      (if (string-prefix-p ":" s)
          (intern (concat ":" (substring s 1)))
        (intern (concat ":" s))))))

(defun arxana-vsatarcs-bilateral--plist-get (entry key)
  "Return KEY from ENTRY (a plist returned by the shared EDN reader).
The reader interns keys as symbols whose names retain the leading
`:'; KEY is the bare keyword form."
  (plist-get entry (intern (symbol-name key))))

(defun arxana-vsatarcs-bilateral--load ()
  "Return the raw `:bilateral-evidence' vector from the .aif.edn, or nil.
Returns nil silently when the file is unreadable; callers should
treat nil as `no block to render'."
  (when (file-readable-p arxana-vsatarcs-bilateral-aif-file)
    (let* ((data (arxana-browser-rewrites--read-edn-file
                  arxana-vsatarcs-bilateral-aif-file))
           (block (plist-get data (intern ":bilateral-evidence"))))
      (and block (append block nil)))))

(defun arxana-vsatarcs-bilateral--summarise (entry)
  "Return an operator-facing plist summary of ENTRY.
The output carries `:vsatarcs-id', `:wm-id', `:principle',
`:evidence-kind', `:landed', `:landed-vsatarcs', `:landed-wm', and
`:has-protocol-witnesses?'.  `:note' and `:forward-pointer' are not
included in the snapshot — they live in the source .aif.edn and the
chrome can drill in there if needed."
  (let ((principle (arxana-vsatarcs-bilateral--plist-get entry :principle))
        (kind (arxana-vsatarcs-bilateral--plist-get entry :evidence-kind))
        (witnesses (arxana-vsatarcs-bilateral--plist-get
                    entry :protocol-witnesses)))
    (list :vsatarcs-id (arxana-vsatarcs-bilateral--plist-get entry :vsatarcs-id)
          :wm-id       (arxana-vsatarcs-bilateral--plist-get entry :wm-id)
          :principle   (and principle
                            (arxana-vsatarcs-bilateral--strip-leading-colon
                             principle))
          :evidence-kind (and kind
                              (arxana-vsatarcs-bilateral--strip-leading-colon
                               kind))
          :landed      (arxana-vsatarcs-bilateral--plist-get entry :landed)
          :landed-vsatarcs (arxana-vsatarcs-bilateral--plist-get
                            entry :landed-vsatarcs)
          :landed-wm   (arxana-vsatarcs-bilateral--plist-get entry :landed-wm)
          :has-protocol-witnesses? (and witnesses
                                        (or (vectorp witnesses)
                                            (consp witnesses))
                                        (> (length witnesses) 0)))))

(defun arxana-vsatarcs-bilateral--kind-counts (summaries)
  "Return an alist of (kind . count) over SUMMARIES.
Carries an entry for each declared kind plus `unknown' so the row
shape is stable across snapshots."
  (let ((counts (mapcar (lambda (k) (cons k 0))
                        arxana-vsatarcs-bilateral-evidence-kinds))
        (unknown 0))
    (dolist (s summaries)
      (let* ((k (plist-get s :evidence-kind))
             (cell (assoc k counts)))
        (if cell
            (setcdr cell (1+ (cdr cell)))
          (setq unknown (1+ unknown)))))
    (append counts (list (cons 'unknown unknown)))))

(defun arxana-vsatarcs-bilateral--witness-count (summaries)
  "Return how many SUMMARIES carry a non-empty `:protocol-witnesses'."
  (cl-count-if (lambda (s) (plist-get s :has-protocol-witnesses?))
               summaries))

(defun arxana-vsatarcs-bilateral-snapshot ()
  "Return the bilateral-evidence snapshot.

The snapshot is a plist:

  (:block-loaded?       <t or nil>
   :aif-path            <absolute path read>
   :total               <integer>
   :kind-counts         ((<kind> . <count>) ...)
   :witness-count       <integer — entries with :protocol-witnesses>
   :entries             (<summary-plist> ...))

Entries are returned in source order (canonical order in the
.aif.edn — typically rough-chronological by `:landed' but not
guaranteed alphabetical; preserving file order keeps the operator's
mental model aligned with the source-of-truth)."
  (let* ((raw (arxana-vsatarcs-bilateral--load))
         (summaries (mapcar #'arxana-vsatarcs-bilateral--summarise
                            (or raw nil))))
    (list :block-loaded? (not (null raw))
          :aif-path arxana-vsatarcs-bilateral-aif-file
          :total (length summaries)
          :kind-counts (arxana-vsatarcs-bilateral--kind-counts summaries)
          :witness-count (arxana-vsatarcs-bilateral--witness-count summaries)
          :entries summaries)))

(defun arxana-vsatarcs-bilateral-by-kind (kind)
  "Return summaries whose `:evidence-kind' equals KIND.
KIND is a bare keyword (e.g. `:joint-landing')."
  (cl-remove-if-not
   (lambda (s) (eq kind (plist-get s :evidence-kind)))
   (plist-get (arxana-vsatarcs-bilateral-snapshot) :entries)))

(provide 'arxana-vsatarcs-bilateral)
;;; arxana-vsatarcs-bilateral.el ends here
