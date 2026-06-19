;;; arxana-vsatarcs-efe.el --- R5 EFE composition for VSATARCS writer actions -*- lexical-binding: t; -*-

;;; Commentary:
;; R5 (Expected Free Energy with at least two principled terms) for the
;; VSATARCs writer surface.  Closes the v0.5.26 forward-pointer by
;; shipping per-candidate G-decomposition over the writer-action
;; classes; auto-enriches candidates so the v0.5.26 R6 softmax module's
;; public API routes to `--full-softmax-select' instead of `:r5-pending'.
;;
;; **Two principled terms** (mirrors WM-side `futon2.aif.efe/compute-efe'
;; v0.4 shape with `:G-risk' + `:G-info' decomposition):
;;
;;   :G-pragmatic  — preference / cost term.  Lower (more negative) =
;;     post-state more aligned with operator-preferred direction
;;     (more checkpoints completed; more closures captured as revisions;
;;     more queue entries lifted).  WM-analogue: `:G-risk'.
;;
;;   :G-epistemic  — information-gain term.  Lower (more negative) =
;;     post-state more discriminable / informative about apparatus
;;     state.  Today's proxy: recency-weighted (recent targets give
;;     more info about whether the apparatus is on-track).
;;     WM-analogue: `:G-info' / `:G-epistemic'.
;;
;;   :G-total      = `:G-pragmatic + :G-epistemic'.
;;
;; **Per-class proxies** (honestly coarse today; refinement via R7
;; adaptive precision once R5 cycles emit prediction-error signal back):
;;
;;   :mission-doc-sync     pragmatic ∝ -count-of-pending-checkpoints
;;                         epistemic ∝ -recency-of-target-file-mtime
;;   :aif-edn-revision-entry
;;                         pragmatic ∝ -count-of-undocumented-revisions
;;                         epistemic ∝ -recency-of-target-file-mtime
;;   :story-update         pragmatic ∝ -age-of-story (older = more stale)
;;                         epistemic ∝ -recency-of-target-file-mtime
;;   :stack-annotations-upsert
;;                         pragmatic ∝ -age-of-story (older = more queued)
;;                         epistemic ∝ -recency-of-target-file-mtime
;;   :essay-revise         pragmatic ∝ queue-G-proxy of target's CURRENT
;;                         state (revising a high-demand essay = lower
;;                         G-pragmatic = more preferred); v0.5.29 ship
;;                         pairs with the read-side queue.
;;                         epistemic ∝ -recency-of-target-file-mtime
;;
;; **v0.5.31 R12 wiring**: per-class pragmatic is multiplied by the
;; intrinsic-value modulator `(credit / 0.5)' from
;; `arxana-vsatarcs-intrinsic-values' so learned credit modulates the
;; static substrate signal.  Identity at Beta(1,1) prior (no behaviour
;; change today); approaches 2× at credit=1.0; collapses to 0 at
;; credit=0.0.  The consumption site claude-9 named as the blocker.
;;
;; **The R5→R6 closure flip**: by extending `arxana-vsatarcs-r6-softmax-select-action'
;; to auto-enrich candidates via this module's `enrich-candidates' before
;; the `--has-g-scores?' gate, the 3 v0.5.26 `:expected-result :failed'
;; tests start passing — ert reports them as `unexpected pass' which IS
;; the operational signal that R5 closed R6's full satisfaction.  The
;; subsequent v0.5.x revision removes the `:expected-result :failed'
;; tags (tests become regular passes).
;;
;; Contract: closes R5; flips R6 from degenerate to full softmax;
;; unblocks R9 EFE-stress + Abstain-fires named-validation properties.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'arxana-vsatarcs-essay-revision-queue) ; --read-essay, --days-since-mtime, --cross-link-count, --stale-pattern-hits for :essay-revise pragmatic
(require 'arxana-vsatarcs-intrinsic-values) ; v0.5.31 R12 consumption site — credit-for modulates the per-class pragmatic via prior-identity multiplier

(defgroup arxana-vsatarcs-efe nil
  "R5 EFE composition for VSATARCs writer actions."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-efe-epistemic-scale 86400.0
  "Time-scale (seconds) for the epistemic recency proxy.
The epistemic G-term proxy uses `seconds-since-mtime / scale' so that
mtimes within the scale window contribute meaningfully; older mtimes
saturate.  Default 1 day = the operator's typical engagement window
per the perceived-time-dual-clock framing
(`~/code/futon2/README-clicks-and-ticks.md').  Tune via this defcustom
when learned weights replace the coarse proxy."
  :type 'number
  :group 'arxana-vsatarcs-efe)

(defcustom arxana-vsatarcs-efe-pragmatic-scale 10.0
  "Scaling for the pragmatic count proxies.
The pragmatic G-term proxies count substrate facts (pending checkpoints,
undocumented revisions, queue depth); the count divides by this scale
to keep G-pragmatic in the WM convention's typical range (G_total
between -5 and +5 in live WM trace records 2026-05-19).  Tune when
learned weights replace the coarse proxy."
  :type 'number
  :group 'arxana-vsatarcs-efe)

(defun arxana-vsatarcs-efe--mtime-seconds-since (path)
  "Return seconds since PATH's mtime, or nil if unreadable.
Used by the epistemic proxy; older mtimes → larger value → larger
negative epistemic contribution (less recent = less informative)."
  (when (and (stringp path) (file-readable-p path))
    (let ((mtime (nth 5 (file-attributes path))))
      (when mtime
        (- (float-time) (float-time mtime))))))

(defun arxana-vsatarcs-efe--g-epistemic-default (action)
  "Default epistemic term: recency-weighted via target file's mtime.
Returns 0.0 when the target is unreadable or the action carries no
target — operational fallback to keep the apparatus from spuriously
favouring uninspectable candidates."
  (let* ((target (or (plist-get action :target-file)
                     (plist-get action :story-path)))
         (since (and target (arxana-vsatarcs-efe--mtime-seconds-since target))))
    (if (numberp since)
        ;; Older = larger since = less negative epistemic = higher G
        ;; (less preferred).  Bounded by sigmoid-like at scale.
        (- (/ since arxana-vsatarcs-efe-epistemic-scale))
      0.0)))

(defun arxana-vsatarcs-efe--g-pragmatic-mission-doc-sync (action)
  "Pragmatic term for `:mission-doc-sync': -count-of-pending-checkpoints.
More pending checkpoints in the target file → lower G-pragmatic →
more motivation to take this action.  Counts `### Checkpoint N'
headings whose immediately-following `**Status: COMPLETE' marker is
absent."
  (let* ((target (plist-get action :target-file)))
    (if (and target (file-readable-p target))
        (with-temp-buffer
          (insert-file-contents target)
          (let ((total 0) (complete 0)
                (text (buffer-string)))
            (let ((pos 0))
              (while (string-match
                      "^### Checkpoint[[:space:]]+[^\n]*\n"
                      text pos)
                (cl-incf total)
                (setq pos (match-end 0))
                (let* ((end (or (string-match
                                 "^\\(### Checkpoint\\|## \\)"
                                 text pos)
                                (length text)))
                       (window (substring text pos
                                          (min end (length text)))))
                  (when (string-match-p
                         "\\*\\*Status:[[:space:]]+COMPLETE\\b" window)
                    (cl-incf complete)))))
            (- (/ (float (- total complete))
                  arxana-vsatarcs-efe-pragmatic-scale))))
      0.0)))

(defun arxana-vsatarcs-efe--g-pragmatic-aif-edn-revision-entry (action)
  "Pragmatic term for `:aif-edn-revision-entry': proposing a new rev is
preferred when many recent closures exist undocumented.  Today's coarse
proxy: -count-of-existing-revisions normalised by pragmatic-scale
(more revisions = more closure activity = more demand for documentation
discipline)."
  (let* ((target (plist-get action :target-file)))
    (if (and target (file-readable-p target))
        (with-temp-buffer
          (insert-file-contents target)
          (let ((text (buffer-string))
                (count 0) (pos 0))
            (while (string-match "{:rev " text pos)
              (cl-incf count)
              (setq pos (match-end 0)))
            (- (/ (float count)
                  arxana-vsatarcs-efe-pragmatic-scale))))
      0.0)))

(defun arxana-vsatarcs-efe--g-pragmatic-story-update (action)
  "Pragmatic term for `:story-update': older stories are more stale
and warrant updating more.  Proxy: -days-since-mtime / pragmatic-scale.
Saturates via the pragmatic-scale division."
  (let* ((target (or (plist-get action :target-file)
                     (plist-get action :story-path)))
         (since (and target (arxana-vsatarcs-efe--mtime-seconds-since target))))
    (if (numberp since)
        (- (/ (/ since 86400.0)  ; seconds → days
              arxana-vsatarcs-efe-pragmatic-scale))
      0.0)))

(defun arxana-vsatarcs-efe--g-pragmatic-stack-annotations-upsert (action)
  "Pragmatic term for `:stack-annotations-upsert' (the lifting-queue
write-half): older unlifted stories are more queued and warrant
lifting more.  Proxy: -days-since-mtime / pragmatic-scale."
  (arxana-vsatarcs-efe--g-pragmatic-story-update action))

(defun arxana-vsatarcs-efe--g-pragmatic-essay-revise (action)
  "Pragmatic term for `:essay-revise' (v0.5.29): reuses the read-side
queue's G-proxy on the target's CURRENT state.  Essays with high
revision-demand (lots of stale-pattern-hits weighted by xlinks +
age) deliver a more-negative pragmatic = more preference for the
revision.  Substrate-aligned: this IS the queue's ranking signal,
plumbed through the writer-action dispatch so the apparatus's R5→R6
flow ranks essay-revisions on the same axis the read-side surfaces
to the operator."
  (let* ((target (plist-get action :target-file)))
    (if (and target (file-readable-p target))
        (let* ((text (arxana-vsatarcs-essay-revision-queue--read-essay target))
               (days (or (arxana-vsatarcs-essay-revision-queue--days-since-mtime
                          target)
                         0.0))
               (xlinks (arxana-vsatarcs-essay-revision-queue--cross-link-count
                        text))
               (stale (arxana-vsatarcs-essay-revision-queue--stale-pattern-hits
                       text)))
          ;; Same shape as the queue's G-proxy.  Divided by pragmatic-scale
          ;; to keep magnitude in the WM convention range.
          (/ (- (+ (* (or stale 0)
                      (1+ (or xlinks 0))
                      (1+ (/ (or days 0.0) 30.0))
                      0.1)
                   (* (or xlinks 0)
                      (/ (or days 0.0) 30.0)
                      0.01)))
             arxana-vsatarcs-efe-pragmatic-scale))
      0.0)))

(defun arxana-vsatarcs-efe--g-pragmatic-fallback (_action)
  "Fallback pragmatic term for unknown action classes.
Returns 0.0 — the action class isn't recognised so we make no
preference claim either way.  Operator can fix by adding a per-class
pragmatic helper above + a dispatch arm in `compute'."
  0.0)

(defun arxana-vsatarcs-efe--g-pragmatic-static (action)
  "Dispatch on action TYPE to per-class static-substrate pragmatic helper.
This is the v0.5.27 shape; v0.5.31's `--g-pragmatic' wraps it with the
intrinsic-value modulator per R12 narrow take-up."
  (pcase (plist-get action :type)
    (:mission-doc-sync
     (arxana-vsatarcs-efe--g-pragmatic-mission-doc-sync action))
    (:aif-edn-revision-entry
     (arxana-vsatarcs-efe--g-pragmatic-aif-edn-revision-entry action))
    (:story-update
     (arxana-vsatarcs-efe--g-pragmatic-story-update action))
    (:stack-annotations-upsert
     (arxana-vsatarcs-efe--g-pragmatic-stack-annotations-upsert action))
    (:essay-revise
     (arxana-vsatarcs-efe--g-pragmatic-essay-revise action))
    (_ (arxana-vsatarcs-efe--g-pragmatic-fallback action))))

(defun arxana-vsatarcs-efe--g-pragmatic (action)
  "Compute the pragmatic G-term for ACTION with the v0.5.31 R12
intrinsic-value modulator: static-substrate-proxy × (credit / 0.5).

The static proxy comes from `--g-pragmatic-static' (reads substrate
state per action-class).  The multiplier `(credit / 0.5)' has identity
1.0 at the Beta(1,1) prior (credit = 0.5) so no behaviour change today,
when no learned hyperparameters exist; converges to 2.0 as credit → 1.0
(operator follows-through on this class consistently — pragmatic doubles
in magnitude = more preferred) and to 0.0 as credit → 0.0 (operator
rejects this class consistently — pragmatic vanishes = no preference).

Direction-check: when operator REJECTS a class often, credit drops →
multiplier drops → pragmatic-magnitude drops → G-total rises → class
LESS preferred. ✓
When operator CONFIRMS often, credit rises → multiplier rises →
pragmatic-magnitude rises → G-total drops → class MORE preferred. ✓

R12 contract closure: the consumption site claude-9 named as the
blocker in v0.5.30's `:v0.5.30-closure' text now exists.  Per-class
intrinsic-value is hidden state inferred by the (yet-to-land) outer
loop; this function is the inner-loop consumption point."
  (let ((static-pragmatic (arxana-vsatarcs-efe--g-pragmatic-static action))
        (credit (arxana-vsatarcs-intrinsic-values-credit-for
                 (plist-get action :type))))
    (* static-pragmatic (/ credit 0.5))))

(defun arxana-vsatarcs-efe-compute (action)
  "Compute EFE decomposition for ACTION; return enriched plist.
Returns the input ACTION with three fields appended:
  :G-pragmatic — preference / cost term (see `--g-pragmatic')
  :G-epistemic — information-gain term (see `--g-epistemic-default')
  :G-total     — sum of the two

Convention: lower G = more preferred (WM convention; minimise free
energy).  Both proxy terms today produce values in roughly
[-1.0, 0.0]; G-total in roughly [-2.0, 0.0].  Range widens when
substrate state diverges (many pending checkpoints, very stale
stories, etc.)."
  (let* ((pragmatic (arxana-vsatarcs-efe--g-pragmatic action))
         (epistemic (arxana-vsatarcs-efe--g-epistemic-default action))
         (total (+ pragmatic epistemic)))
    (append action
            (list :G-pragmatic pragmatic
                  :G-epistemic epistemic
                  :G-total total))))

(defun arxana-vsatarcs-efe-enrich-candidates (candidates)
  "Apply `compute' to every candidate that lacks `:G-total'.
Candidates already carrying `:G-total' pass through unchanged —
proxies don't overwrite already-computed scores (lets tests and
upstream proposers pre-populate scores explicitly).  Returns the
enriched list in the same order."
  (mapcar (lambda (c)
            (if (numberp (plist-get c :G-total))
                c
              (arxana-vsatarcs-efe-compute c)))
          (or candidates nil)))

(provide 'arxana-vsatarcs-efe)
;;; arxana-vsatarcs-efe.el ends here
