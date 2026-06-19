;;; arxana-vsatarcs-intrinsic-values.el --- Per-writer-action-class intrinsic-value (R12 narrow take-up, VSATARCS port) -*- lexical-binding: t; -*-

;;; Commentary:
;; v0.5.31 ship — symmetric port of `futon2.aif.intrinsic-values` (claude-9
;; 2026-05-21) to the VSATARCs side, closing the R12 bilateral apparatus
;; per the handoff at `~/code/futon0/holes/handoffs/r12-to-stack-Q6-2026-05-21.md'.
;;
;; **R12 wiring**: hyperparameters consumed by the EFE composition's per-class
;; pragmatic helpers (per-writer-action-class `:intrinsic-value' in
;; `arxana-vsatarcs-efe.el') become hidden state inferred by a slower outer
;; loop.  This module owns the in-process state on the VSATARCs side.
;;
;; **Independent atom namespace** (Joe directive 2026-05-21, resolving open
;; question §11.2 of `M-the-futon-stack-Q6-r12-design-choices.md'): VSATARCs
;; and WM-side each hold their own intrinsic-value table.  Matches the
;; bilateral discipline (both contracts ship symmetric apparatus; neither
;; depends on the other's process state); costs a duplicate bootstrap-replay
;; query on JVM/Emacs startup.
;;
;; **New hyperedge type** (Joe directive 2026-05-21): persistence uses
;; `code/v05/vsatarcs-hyperparameter-update' rather than reusing the WM-side
;; `code/v05/wm-hyperparameter-update' with a discriminator field.  The
;; action-class universe differs (writer-action-classes :mission-doc-sync,
;; :aif-edn-revision-entry, :story-update, :essay-revise vs WM's
;; :address-sorry, :open-mission, :fire-pattern); Stream B substrate differs
;; (consent-decisions vs git-derived).  Two types stays cleaner than one
;; type with a discriminator that's load-bearing on every query.
;;
;; **Pattern**: atom-with-bootstrap-replay (per `feedback_reload_safety' —
;; atom-without-bootstrap-replay is the unsafe case).  Persistence is via
;; `code/v05/vsatarcs-hyperparameter-update' hyperedges in futon1a XTDB;
;; first call to `arxana-vsatarcs-intrinsic-values-credit-for' triggers
;; rehydration from the store if not yet hydrated.
;;
;; **Apparatus-in-place caveat** (same shape as claude-9's WM-side ship):
;; the outer-loop that POSTs update records is deferred to a follow-on
;; session.  Until the consent-gate (`arxana-vsatarcs-consent.el', claude-2)
;; emits decision hyperedges that this module's outer loop can consume,
;; values stay at Beta(1,1) prior mode (0.5) for every class.  R12 flip
;; here is on the *apparatus* (consumption site exists; persistence wired;
;; reload-safe; identity-at-prior in EFE composition); not on learned
;; values being meaningful yet.
;;
;; Public API:
;;   (arxana-vsatarcs-intrinsic-values-current)
;;     → snapshot of the table.
;;   (arxana-vsatarcs-intrinsic-values-credit-for CLASS)
;;     → intrinsic-value for CLASS (defaults to Beta(1,1) prior mode 0.5).
;;     First call triggers rehydration if not yet hydrated.
;;   (arxana-vsatarcs-intrinsic-values-apply-update! RECORD)
;;     → fold one record into the table; returns the record.
;;   (arxana-vsatarcs-intrinsic-values-rehydrate! RECORDS)
;;     → replace state with latest-per-class from RECORDS.
;;   (arxana-vsatarcs-intrinsic-values-rehydrate-from-store!)
;;     → fetch records from futon1a + rehydrate.  Idempotent.
;;   (arxana-vsatarcs-intrinsic-values-persist-record! RECORD)
;;     → POST one record to futon1a as a hyperedge; returns the response.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'arxana-store)

(defgroup arxana-vsatarcs-intrinsic-values nil
  "Per-writer-action-class intrinsic-value (R12 narrow take-up)."
  :group 'arxana-vsatarcs)

(defconst arxana-vsatarcs-intrinsic-values-hyperedge-type
  "code/v05/vsatarcs-hyperparameter-update"
  "XTDB hyperedge type for VSATARCs intrinsic-value update records.
Independent from the WM-side type `code/v05/wm-hyperparameter-update'
per Joe directive 2026-05-21 (independent atom namespace + new edge
type, resolving design-choices §11.2).")

(defconst arxana-vsatarcs-intrinsic-values-prior-alpha 1.0
  "Beta(α,β) prior α — uniform prior on each class's credit.")

(defconst arxana-vsatarcs-intrinsic-values-prior-beta 1.0
  "Beta(α,β) prior β — uniform prior on each class's credit.")

(defun arxana-vsatarcs-intrinsic-values--posterior-mode (alpha beta)
  "Posterior mode of Beta(ALPHA, BETA).
For α, β > 1: (α - 1) / (α + β - 2).  For α = β = 1 the distribution is
uniform and any point in (0,1) is admissible — 0.5 is the maximum-entropy
choice (mirrors `futon2.aif.intrinsic-values/posterior-mode')."
  (if (and (> alpha 1.0) (> beta 1.0))
      (/ (- alpha 1.0) (- (+ alpha beta) 2.0))
    (/ alpha (+ alpha beta))))

(defun arxana-vsatarcs-intrinsic-values-fresh-entry ()
  "Return a fresh Beta(1,1) prior entry — what every class starts at."
  (list :alpha arxana-vsatarcs-intrinsic-values-prior-alpha
        :beta arxana-vsatarcs-intrinsic-values-prior-beta
        :intrinsic-value
        (arxana-vsatarcs-intrinsic-values--posterior-mode
         arxana-vsatarcs-intrinsic-values-prior-alpha
         arxana-vsatarcs-intrinsic-values-prior-beta)
        :n-emissions 0
        :n-followthrough 0
        :as-of nil))

;; ---------------------------------------------------------------------
;; In-process state — alist of (CLASS . ENTRY-PLIST) — `defvar' not
;; `defcustom' because operator-edit doesn't apply; updates flow via
;; rehydrate! / apply-update!.
;; ---------------------------------------------------------------------

(defvar arxana-vsatarcs-intrinsic-values--state nil
  "Per-class intrinsic-value table; alist of (CLASS . ENTRY-PLIST).
Hydrate via `arxana-vsatarcs-intrinsic-values-rehydrate-from-store!'.
Tests reset via `arxana-vsatarcs-intrinsic-values-reset-to-prior!'.")

(defvar arxana-vsatarcs-intrinsic-values--hydrated? nil
  "Non-nil once `rehydrate-from-store!' has succeeded at least once.
Guards the lazy rehydrate in `credit-for' so we hit the store at most
once per Emacs session without explicit re-fetch.")

(defun arxana-vsatarcs-intrinsic-values-current ()
  "Return a snapshot of the table."
  arxana-vsatarcs-intrinsic-values--state)

(defun arxana-vsatarcs-intrinsic-values-reset-to-prior! ()
  "Test-helper: clear the table and the hydrated marker.
Production callers should use `rehydrate-from-store!' instead."
  (setq arxana-vsatarcs-intrinsic-values--state nil
        arxana-vsatarcs-intrinsic-values--hydrated? nil))

(defun arxana-vsatarcs-intrinsic-values-credit-for (class)
  "Intrinsic-value credit for CLASS.
Defaults to Beta(1,1) prior mode (0.5) for any class with no recorded
updates yet.  This is the function `arxana-vsatarcs-efe.el's' per-class
pragmatic helpers consult at scoring time.

On first call this triggers a lazy rehydrate from the futon1a store;
subsequent calls hit only the in-memory table.  Explicit re-fetch via
`arxana-vsatarcs-intrinsic-values-rehydrate-from-store!'."
  (unless arxana-vsatarcs-intrinsic-values--hydrated?
    (ignore-errors
      (arxana-vsatarcs-intrinsic-values-rehydrate-from-store!)))
  (or (plist-get (cdr (assq class arxana-vsatarcs-intrinsic-values--state))
                 :intrinsic-value)
      (plist-get (arxana-vsatarcs-intrinsic-values-fresh-entry)
                 :intrinsic-value)))

;; ---------------------------------------------------------------------
;; Update-record shape (mirrors hyperedge props).  Plain plist:
;;   :class                       :essay-revise / etc.
;;   :as-of                       ISO-8601 string
;;   :alpha-post  :beta-post      Beta posterior
;;   :intrinsic-value-post        posterior mode
;;   :n-emissions-in-window
;;   :n-followthrough-in-window
;;   :evidence-refs               optional vector
;;   :outer-loop-run-id           optional string
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-intrinsic-values--entry-from-record (record)
  "Build a table-entry plist from a RECORD plist."
  (list :alpha (plist-get record :alpha-post)
        :beta (plist-get record :beta-post)
        :intrinsic-value (plist-get record :intrinsic-value-post)
        :n-emissions (plist-get record :n-emissions-in-window)
        :n-followthrough (plist-get record :n-followthrough-in-window)
        :as-of (plist-get record :as-of)))

(defun arxana-vsatarcs-intrinsic-values-rehydrate! (records)
  "Replace state with the latest-per-class entry derived from RECORDS.
Records grouped by :class; within each class, the record with the
greatest :as-of wins.  Returns the new state.

Latest-wins (not replay-all) per claude-9's WM-side §6 rationale:
each outer-loop record IS the full Beta posterior given evidence up
to its :as-of; replaying every record would double-count conjugate
updates."
  (let ((grouped (make-hash-table :test #'eq)))
    (dolist (r records)
      (let* ((class (plist-get r :class))
             (cur (gethash class grouped)))
        (when (and class
                   (or (null cur)
                       (string> (or (plist-get r :as-of) "")
                                (or (plist-get cur :as-of) ""))))
          (puthash class r grouped))))
    (let (alist)
      (maphash
       (lambda (class r)
         (push (cons class
                     (arxana-vsatarcs-intrinsic-values--entry-from-record r))
               alist))
       grouped)
      (setq arxana-vsatarcs-intrinsic-values--state alist
            arxana-vsatarcs-intrinsic-values--hydrated? t)
      alist)))

(defun arxana-vsatarcs-intrinsic-values-apply-update! (record)
  "Fold one RECORD into the in-process table.  Returns RECORD.
Used by the (yet-to-land) outer loop right after persisting a hyperedge,
and by tests / REPL exercises to update the table without re-fetching."
  (let* ((class (plist-get record :class))
         (entry (arxana-vsatarcs-intrinsic-values--entry-from-record record))
         (stripped (assq-delete-all
                    class
                    (copy-sequence
                     arxana-vsatarcs-intrinsic-values--state))))
    (setq arxana-vsatarcs-intrinsic-values--state
          (cons (cons class entry) stripped)
          arxana-vsatarcs-intrinsic-values--hydrated? t)
    record))

;; ---------------------------------------------------------------------
;; XTDB bootstrap-replay
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-intrinsic-values--record-from-hyperedge (hx)
  "Coerce one fetched hyperedge alist into a record plist; nil on malformed.
Props arrive as a string-keyed alist (per the JSON round-trip in
`arxana-store-fetch-hyperedges'); class arrives as a string with no
leading colon and is re-keyworded here."
  (let* ((props (alist-get 'hx/props hx nil nil #'equal))
         (class-raw (cdr (or (assoc "class" props)
                             (assoc 'class props))))
         (class (cond
                 ((keywordp class-raw) class-raw)
                 ((and (stringp class-raw) (not (string-empty-p class-raw)))
                  (intern (concat ":" (replace-regexp-in-string
                                       "^:" "" class-raw))))
                 (t nil)))
         (get (lambda (k)
                (cdr (or (assoc (symbol-name k) props)
                         (assoc k props)))))
         (alpha-post (funcall get 'alpha-post))
         (beta-post (funcall get 'beta-post)))
    (when (and class (numberp alpha-post) (numberp beta-post))
      (list :class class
            :as-of (funcall get 'as-of)
            :outer-loop-run-id (funcall get 'outer-loop-run-id)
            :window-days (funcall get 'window-days)
            :alpha-pre (funcall get 'alpha-pre)
            :beta-pre (funcall get 'beta-pre)
            :alpha-post alpha-post
            :beta-post beta-post
            :intrinsic-value-pre (funcall get 'intrinsic-value-pre)
            :intrinsic-value-post (funcall get 'intrinsic-value-post)
            :n-emissions-in-window (funcall get 'n-emissions-in-window)
            :n-followthrough-in-window (funcall get 'n-followthrough-in-window)
            :evidence-refs (funcall get 'evidence-refs)))))

(defun arxana-vsatarcs-intrinsic-values-fetch-records (&optional limit)
  "Fetch all VSATARCs hyperparameter-update hyperedges from the store.
Returns a list of record plists (post-coercion).  Returns nil on
HTTP error so a fresh Emacs with an unreachable store starts at prior."
  (let* ((resp (ignore-errors
                 (arxana-store-fetch-hyperedges
                  :type arxana-vsatarcs-intrinsic-values-hyperedge-type
                  :limit (or limit 1000))))
         (hxs (and resp (alist-get 'hyperedges resp))))
    (delq nil
          (mapcar
           #'arxana-vsatarcs-intrinsic-values--record-from-hyperedge
           hxs))))

(defun arxana-vsatarcs-intrinsic-values-rehydrate-from-store! (&optional limit)
  "Fetch records from futon1a + rehydrate.  Idempotent.
Returns the new state."
  (arxana-vsatarcs-intrinsic-values-rehydrate!
   (arxana-vsatarcs-intrinsic-values-fetch-records limit)))

;; ---------------------------------------------------------------------
;; Inference: derive the next-update record (used by the outer-loop,
;; not yet shipped; provided here so the next session can build the
;; outer loop on top of the same primitive shape as WM-side
;; `intrinsic-values/next-record').
;; ---------------------------------------------------------------------

(cl-defun arxana-vsatarcs-intrinsic-values-next-record
    (class prior-entry n-emissions n-followthrough
           &key as-of outer-loop-run-id window-days evidence-refs)
  "Compute the next-update record for CLASS given PRIOR-ENTRY + observations.
N-EMISSIONS is the count of writer-action proposals of CLASS in the
trace window; N-FOLLOWTHROUGH is the count for which the operator
consent-gate dispatched :confirm.  Returns a record plist ready to
persist and apply.  Mirrors `futon2.aif.intrinsic-values/next-record'."
  (let* ((alpha-pre (or (plist-get prior-entry :alpha)
                        arxana-vsatarcs-intrinsic-values-prior-alpha))
         (beta-pre (or (plist-get prior-entry :beta)
                       arxana-vsatarcs-intrinsic-values-prior-beta))
         (delta-a (float n-followthrough))
         (delta-b (float (- n-emissions n-followthrough)))
         (alpha-post (+ alpha-pre delta-a))
         (beta-post (+ beta-pre delta-b)))
    (list :class class
          :as-of as-of
          :outer-loop-run-id outer-loop-run-id
          :window-days window-days
          :alpha-pre alpha-pre
          :beta-pre beta-pre
          :alpha-post alpha-post
          :beta-post beta-post
          :intrinsic-value-pre
          (arxana-vsatarcs-intrinsic-values--posterior-mode
           alpha-pre beta-pre)
          :intrinsic-value-post
          (arxana-vsatarcs-intrinsic-values--posterior-mode
           alpha-post beta-post)
          :n-emissions-in-window n-emissions
          :n-followthrough-in-window n-followthrough
          :evidence-refs (or evidence-refs []))))

;; ---------------------------------------------------------------------
;; XTDB write: persist a record as a hyperedge
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-intrinsic-values-persist-record! (record)
  "POST one update RECORD to futon1a as a VSATARCs-hyperparameter-update
hyperedge.  Returns the parsed response or nil on failure.  Mirrors
`futon2.aif.intrinsic-values/persist-record!'."
  (let* ((class (plist-get record :class))
         (class-id (concat "vsatarcs-class:" (substring (symbol-name class) 1)))
         (run-id (or (plist-get record :outer-loop-run-id)
                     (concat "vsatarcs-ol-run:"
                             (or (plist-get record :as-of)
                                 (format-time-string
                                  "%Y-%m-%dT%H:%M:%S%z"))))))
    (ignore-errors
      (arxana-store-create-hyperedge
       :hx-type arxana-vsatarcs-intrinsic-values-hyperedge-type
       :endpoints (vector class-id run-id)
       :props (append record
                      (list :provenance/author "vsatarcs-outer-loop"))))))

(provide 'arxana-vsatarcs-intrinsic-values)
;;; arxana-vsatarcs-intrinsic-values.el ends here
