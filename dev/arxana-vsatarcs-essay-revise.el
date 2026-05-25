;;; arxana-vsatarcs-essay-revise.el --- Essay-revision writer-action class (R4) -*- lexical-binding: t; -*-

;;; Commentary:
;; v0.5.29 write-half for the `:essay-revise' action class, paired with
;; v0.5.28's read-side essay-revision-queue surface.  Closes the
;; v0.5.28 write-half forward-pointer after claude-2 moved on to a new
;; mission and Joe directed me (claude-4) to take it up locally rather
;; than wait for the bilateral cycle.
;;
;; **Pattern**: this is the fourth writer-action class in the
;; self-documentation family (`:mission-doc-sync',
;; `:aif-edn-revision-entry', `:story-update', and now `:essay-revise')
;; sharing the v0.5.22 safety-property generalisation: clean
;; `:match-type' AND a class-specific structural-admissibility
;; predicate that doubles as the recursion-safety mechanic.
;;
;; **Admissibility / recursion-safety for :essay-revise**: proposed
;; content must differ from current file content, AND target must be
;; readable + git-tracked.  If the apparatus proposed the current
;; essay content as its own revision, `can-propose?' returns nil on
;; the no-op detection — same termination shape as
;; `:aif-edn-revision-entry's' uniqueness check against
;; `:provenance :revisions'.
;;
;; **Action shape** (plist):
;;   (:type :essay-revise
;;    :target-file        "/abs/path/to/essay.html"
;;    :proposed-content   "<html>...</html>"      ; post-state content
;;    :match-type         :clean | :scope-creep | :pivot
;;    :source-evidence-id "queue-snapshot:..."    ; or similar
;;    :rationale          "human-readable string")
;;
;; **Why full-content rather than a patch shape**: the niche-creation
;; framing treats each revision as the corpus's post-state — the
;; observable signals (cross-link-density, stale-pattern-hits, size,
;; PKD-marker presence) are computed on the post-state content
;; directly using the same primitives as the read-side queue.  Patch
;; shapes would require ad-hoc apply + reparse; full-content is the
;; simpler general-case shape and predict-effects becomes a single
;; `--summarise-content' call on each side.  Higher per-action payload
;; size; operationally fine since revisions are infrequent (the
;; apparatus surfaces one priority candidate at a time per the
;; queue's G-proxy ordering).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'arxana-vsatarcs-essay-revision-queue)

(defgroup arxana-vsatarcs-essay-revise nil
  "Essay-revision writer-action class for VSATARCS."
  :group 'arxana-vsatarcs)

(defconst arxana-vsatarcs-essay-revise-version 1
  "Schema version for `:essay-revise' action shape.")

;; ---------------------------------------------------------------------
;; Action constructor
;; ---------------------------------------------------------------------

(cl-defun arxana-vsatarcs-essay-revise-make-action
    (&key target-file proposed-content match-type
          source-evidence-id rationale)
  "Construct an `:essay-revise' writer-action plist.
MATCH-TYPE in {:clean :scope-creep :pivot}.  TARGET-FILE absolute
path; PROPOSED-CONTENT the full post-state content of the file."
  (cl-assert (memq match-type '(:clean :scope-creep :pivot)))
  (cl-assert (file-name-absolute-p target-file))
  (cl-assert (stringp proposed-content))
  (list :type :essay-revise
        :target-file target-file
        :proposed-content proposed-content
        :match-type match-type
        :source-evidence-id source-evidence-id
        :rationale rationale))

;; ---------------------------------------------------------------------
;; Per-content metric computation (mirrors queue's per-file metrics
;; but operates on a content string so predict-effects can compute the
;; post-state without writing the file)
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-essay-revise--summarise-content (content)
  "Return per-content metrics plist for CONTENT.
Mirrors the per-essay subset of the read-side queue's summary.  Note:
omits `:days-since-mtime' (only meaningful per-file, not per-content);
the predict-effects builder reads pre-state mtime from disk for that
field and treats post-state as `0.0' (current write = mtime now)."
  (let ((xlinks (arxana-vsatarcs-essay-revision-queue--cross-link-count
                 content))
        (stale-hits (arxana-vsatarcs-essay-revision-queue--stale-pattern-hits
                     content))
        (pkd? (arxana-vsatarcs-essay-revision-queue--has-pkd-marker?
               content)))
    (list :size-bytes (length content)
          :cross-link-density (or xlinks 0)
          :stale-pattern-hits (or stale-hits 0)
          :has-pkd-epigraph? pkd?)))

(defun arxana-vsatarcs-essay-revise--g-proxy (metrics days)
  "Compute the queue's two-term G-proxy from METRICS and DAYS.
METRICS is the plist from `--summarise-content'; DAYS is
days-since-mtime (treat post-state as 0.0).  Returns a float in the
queue's convention (lower = more demand for revision)."
  (let ((xlinks (plist-get metrics :cross-link-density))
        (stale-hits (plist-get metrics :stale-pattern-hits)))
    (- (+ (* (or stale-hits 0)
             (1+ (or xlinks 0))
             (1+ (/ (or days 0.0) 30.0))
             0.1)
          (* (or xlinks 0)
             (/ (or days 0.0) 30.0)
             0.01)))))

;; ---------------------------------------------------------------------
;; Forward-model: predict-effects
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-essay-revise-predict-effects (action)
  "Predict post-state for an `:essay-revise' ACTION.
Returns a plist with `:pre-state', `:predicted-post-state', and
`:predicted-delta' built from per-content metrics.  Post-state mtime
is treated as 0.0 days (write-on-execute resets mtime to now), so the
post-state G-proxy reflects ONLY structural improvement
(stale-hits-drop, xlinks-preservation, etc.) — `revision well-spent'
shows up as G-delta-positive (G rises toward 0 = less demand)."
  (let* ((file (plist-get action :target-file))
         (proposed (plist-get action :proposed-content))
         (current (or (arxana-vsatarcs-essay-revision-queue--read-essay file)
                      ""))
         (pre-days (or (arxana-vsatarcs-essay-revision-queue--days-since-mtime
                        file)
                       0.0))
         (pre-metrics (arxana-vsatarcs-essay-revise--summarise-content current))
         (post-metrics (arxana-vsatarcs-essay-revise--summarise-content
                        proposed))
         (pre-g (arxana-vsatarcs-essay-revise--g-proxy pre-metrics pre-days))
         (post-g (arxana-vsatarcs-essay-revise--g-proxy post-metrics 0.0)))
    (list :pre-state (append pre-metrics
                             (list :days-since-mtime pre-days
                                   :G-proxy pre-g))
          :predicted-post-state (append post-metrics
                                        (list :days-since-mtime 0.0
                                              :G-proxy post-g))
          :predicted-delta
          (list :G-proxy-delta (- post-g pre-g)
                :stale-pattern-hits-delta
                (- (plist-get post-metrics :stale-pattern-hits)
                   (plist-get pre-metrics :stale-pattern-hits))
                :cross-link-density-delta
                (- (plist-get post-metrics :cross-link-density)
                   (plist-get pre-metrics :cross-link-density))
                :size-bytes-delta
                (- (plist-get post-metrics :size-bytes)
                   (plist-get pre-metrics :size-bytes))))))

;; ---------------------------------------------------------------------
;; Admissibility: can-propose? + can-execute?
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-essay-revise-can-propose? (action)
  "Return non-nil iff ACTION is admissible as a proposal.
Requires: target-file exists + readable; proposed-content non-empty;
proposed-content NOT identical to current file content (the v0.5.22
structural-admissibility predicate doubling as recursion-safety
mechanic — proposing the current content as its own revision returns
nil and the apparatus terminates self-application)."
  (let* ((file (plist-get action :target-file))
         (proposed (plist-get action :proposed-content)))
    (and (stringp proposed)
         (not (string-empty-p proposed))
         (file-exists-p file)
         (file-readable-p file)
         (let ((current (arxana-vsatarcs-essay-revision-queue--read-essay
                         file)))
           (and current
                (not (string-equal current proposed)))))))

(defun arxana-vsatarcs-essay-revise-can-execute? (action)
  "Return non-nil iff ACTION is admissible for execution (I5 + I6).
Stricter than `can-propose?': adds file-writable + git-clean state.
Reuses the writer-actions module's `--git-clean-p' contract — when
the target's repo is dirty, execution defers (the operator can stage
or stash; nothing here forces the decision)."
  (and (arxana-vsatarcs-essay-revise-can-propose? action)
       (let ((file (plist-get action :target-file)))
         (and (file-writable-p file)
              (arxana-vsatarcs-essay-revise--git-clean-p file)))))

(defun arxana-vsatarcs-essay-revise--git-clean-p (file)
  "Return non-nil iff FILE is git-tracked with no uncommitted changes.
Mirrors writer-actions's `--git-clean-p' so the safety contract is
identical across all classes in the family."
  (let* ((dir (file-name-directory (expand-file-name file)))
         (default-directory dir)
         (rel (file-relative-name (expand-file-name file) dir))
         (status (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process "git" nil t nil
                                   "status" "--porcelain" "--" rel)))))
    (and (= 0 (call-process "git" nil nil nil
                            "rev-parse" "--is-inside-work-tree"))
         (string-empty-p (string-trim status)))))

(provide 'arxana-vsatarcs-essay-revise)
;;; arxana-vsatarcs-essay-revise.el ends here
