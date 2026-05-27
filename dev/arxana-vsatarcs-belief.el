;;; arxana-vsatarcs-belief.el --- Per-entity belief state for the VSATARCS reader -*- lexical-binding: t; -*-

;;; Commentary:
;; Per-entity belief state for the VSATARCS reader surface.
;;
;; VSATARCS is a reader of the futon stack — and prospectively a writer
;; once the AIF aspects come online.  As a reader/agent it maintains a
;; belief distribution over the status of each entity it has observed
;; (mission ids, section ids, sorry ids — the same entity domain the
;; WM AIF apparatus tracks in `futon2.aif.belief').
;;
;; The shape ports `futon2/src/futon2/aif/belief.clj' (claude-2,
;; 2026-05-17, M-war-machine-aif-completion Checkpoint 1) into Emacs
;; Lisp so the reader surface can satisfy R1 of the standard contract
;; (see `~/code/futon4/docs/vsatarcs-alignment-completeness.md').  The
;; same multiplicative-likelihood update is used; the same status set
;; is tracked; the same R1 baseline test obligations apply.
;;
;; The posterior is over the M-INC event vocabulary v1 `state/*' tags;
;; updates apply a multiplicative likelihood and renormalise.  The event
;; shape is M-INC-compatible but does not depend on M-INC step (b)
;; landing — events can be synthesised locally during development,
;; then sourced from the typed-event substrate once step (b) commits.
;;
;; Contract: contributes to R1 (explicit belief state) per the VSATARCS
;; alignment-completeness doc.  Cross-maps to F1 (explicit fitness
;; state) at stack scope.

;;; Code:

(require 'cl-lib)

(defconst arxana-vsatarcs-belief-status-set
  '(spawned refined strengthened addressed falsified foreclosed reopened)
  "Tagged status set the VSATARCS reader tracks per entity.
Aligns with M-INC event vocabulary v1 `state/*' event types
(state/spawned, state/refined, ...).  The `link/*' events are
relational and tracked separately, not within this posterior.

Status symbols are the bare names (without the `state/' prefix);
events may carry full namespaced keywords (`:state/spawned' etc.) and
are normalised to the bare symbol via
`arxana-vsatarcs-belief--event-status'.")

(defun arxana-vsatarcs-belief--event-status (event-type)
  "Return bare status symbol for EVENT-TYPE, or nil if not in the status set.
EVENT-TYPE may be `:state/spawned' (a namespaced keyword from M-INC),
the bare keyword `:spawned', the bare symbol `spawned', or a string
`\"state/spawned\"' / `\"spawned\"'.  All are normalised to the bare
symbol before lookup."
  (when event-type
    (let* ((raw (cond
                 ((symbolp event-type) (symbol-name event-type))
                 ((stringp event-type) event-type)
                 (t (error "Unexpected event type: %S" event-type))))
           (stripped (replace-regexp-in-string "\\`:" "" raw))
           (stripped (replace-regexp-in-string "\\`state/" "" stripped))
           (sym (intern stripped)))
      (and (memq sym arxana-vsatarcs-belief-status-set) sym))))

(defun arxana-vsatarcs-belief-uniform-prior ()
  "Construct a uniform prior posterior over the status set.
Returns an alist of (status-symbol . probability)."
  (let* ((n (length arxana-vsatarcs-belief-status-set))
         (p (/ 1.0 n)))
    (mapcar (lambda (s) (cons s p)) arxana-vsatarcs-belief-status-set)))

(defun arxana-vsatarcs-belief--normalise (posterior)
  "Renormalise POSTERIOR so probabilities sum to 1.0.
If the input sums to zero (degenerate), returns a fresh uniform
prior — normal operation never produces a zero-sum posterior, but
this floor prevents NaN propagation."
  (let ((s (apply #'+ (mapcar #'cdr posterior))))
    (if (zerop s)
        (arxana-vsatarcs-belief-uniform-prior)
      (mapcar (lambda (kv) (cons (car kv) (/ (cdr kv) s))) posterior))))

(defun arxana-vsatarcs-belief-initial-state (entity-ids)
  "Construct a fresh belief state with uniform priors for ENTITY-IDS.
ENTITY-IDS is a list of entity identifiers (strings, symbols, or
keywords).  Returns an alist of (entity-id . posterior)."
  (mapcar (lambda (id) (cons id (arxana-vsatarcs-belief-uniform-prior)))
          entity-ids))

(defun arxana-vsatarcs-belief-update-entity (posterior event)
  "Apply one evidence EVENT to a single entity's POSTERIOR.

Event shape (M-INC-compatible plist):
  (:type      <event-type, e.g. `:state/spawned'>
   :weight    <number, default 1.0>
   :timestamp <ISO 8601 string, optional>)

Returns a new normalised posterior.  Events whose `:type' is not in
`arxana-vsatarcs-belief-status-set' are ignored (posterior unchanged)
— this keeps the update step total over a wider event stream (e.g.
`link/asserted' events that aren't per-entity-status)."
  (let* ((tag (arxana-vsatarcs-belief--event-status (plist-get event :type)))
         (w (or (plist-get event :weight) 1.0)))
    (if tag
        (arxana-vsatarcs-belief--normalise
         (mapcar (lambda (kv)
                   (if (eq (car kv) tag)
                       (cons (car kv) (* (cdr kv) (+ 1.0 w)))
                     kv))
                 posterior))
      posterior)))

(defun arxana-vsatarcs-belief-update (belief event)
  "Apply an evidence EVENT (carrying `:entity-id') to the full BELIEF state.
BELIEF is an alist of (entity-id . posterior).  If the entity-id is
not yet tracked, initialise it with a uniform prior before applying
the event."
  (let* ((eid (plist-get event :entity-id))
         (current (or (cdr (assoc eid belief))
                      (arxana-vsatarcs-belief-uniform-prior)))
         (next (arxana-vsatarcs-belief-update-entity current event))
         (without-eid (cl-remove-if (lambda (kv) (equal (car kv) eid)) belief)))
    (cons (cons eid next) without-eid)))

(defun arxana-vsatarcs-belief-update-batch (belief events)
  "Reduce a sequence of evidence EVENTS into the BELIEF state.
Order within a single entity's events is irrelevant for the final
posterior (multiplicative-likelihood updates commute under
normalisation); order across entities is irrelevant (updates to
different entities don't interact)."
  (cl-reduce #'arxana-vsatarcs-belief-update events :initial-value belief))

(defun arxana-vsatarcs-belief-most-likely-status (posterior)
  "Return the argmax status of POSTERIOR.
Discrete analogue of belief 'mean'.  Returns nil for an empty
posterior.  Tie-breaking is unspecified."
  (when posterior
    (car (cl-reduce (lambda (a b) (if (> (cdr a) (cdr b)) a b)) posterior))))

(defun arxana-vsatarcs-belief-entropy (posterior)
  "Return Shannon entropy of POSTERIOR in nats.
Discrete analogue of belief 'variance' / precision — uniform
posteriors have maximal entropy (log n); peaked posteriors approach
zero.  Used by downstream R3 (predictive-coding) and R7 (adaptive
precision) work."
  (- (apply #'+
            (mapcar (lambda (kv)
                      (let ((v (cdr kv)))
                        (if (> v 0) (* v (log v)) 0)))
                    posterior))))

(defun arxana-vsatarcs-belief-valid-posterior-p (posterior)
  "Return non-nil if POSTERIOR is a valid distribution over the status set.
Used by tests and by callers that want to validate inputs.  Requires:
keys are exactly the status set; values are in [0, 1]; sum is 1.0
within 1e-9 tolerance."
  (and (consp posterior)
       (= (length posterior) (length arxana-vsatarcs-belief-status-set))
       (cl-every (lambda (s) (assoc s posterior))
                 arxana-vsatarcs-belief-status-set)
       (cl-every (lambda (kv) (and (numberp (cdr kv))
                                   (<= 0.0 (cdr kv) 1.0)))
                 posterior)
       (< (abs (- 1.0 (apply #'+ (mapcar #'cdr posterior)))) 1e-9)))

;; ---------------------------------------------------------------------
;; Persistence + current state
;;
;; v0.2 ships a file-backed store so belief survives Emacs sessions.
;; The store is a single elisp form holding the alist of (entity-id .
;; posterior).  Path is configurable; default lives outside the repo.
;; ---------------------------------------------------------------------

(defcustom arxana-vsatarcs-belief-store-file
  (expand-file-name "var/vsatarcs-belief.eld" user-emacs-directory)
  "Path of the on-disk belief store used by VSATARCS.
The file holds a single readable elisp form: the belief alist of
(entity-id . posterior) pairs.  Created on first save."
  :type 'file
  :group 'arxana-vsatarcs)

(defvar arxana-vsatarcs-belief--current nil
  "Module-local current belief state (alist of entity-id → posterior).
Reset by `arxana-vsatarcs-belief-reset', loaded by
`arxana-vsatarcs-belief-load', mutated by
`arxana-vsatarcs-belief-ingest-events'.")

(defun arxana-vsatarcs-belief-current ()
  "Return the current in-memory belief state (alist)."
  arxana-vsatarcs-belief--current)

(defun arxana-vsatarcs-belief-reset ()
  "Clear the current in-memory belief state (does not touch the store)."
  (setq arxana-vsatarcs-belief--current nil))

(defun arxana-vsatarcs-belief-save (&optional path)
  "Persist the current belief state to PATH.
PATH defaults to `arxana-vsatarcs-belief-store-file'.  Creates parent
directories as needed.  Returns PATH."
  (let ((target (or path arxana-vsatarcs-belief-store-file)))
    (make-directory (file-name-directory target) t)
    (with-temp-file target
      (let ((print-length nil)
            (print-level nil))
        (prin1 arxana-vsatarcs-belief--current (current-buffer))))
    target))

(defun arxana-vsatarcs-belief-load (&optional path)
  "Load belief state from PATH into the current in-memory state.
PATH defaults to `arxana-vsatarcs-belief-store-file'.  If PATH does
not exist, the current state is reset to nil.  Returns the loaded
state."
  (let ((source (or path arxana-vsatarcs-belief-store-file)))
    (if (file-readable-p source)
        (with-temp-buffer
          (insert-file-contents source)
          (goto-char (point-min))
          (setq arxana-vsatarcs-belief--current (read (current-buffer))))
      (setq arxana-vsatarcs-belief--current nil))
    arxana-vsatarcs-belief--current))

(defun arxana-vsatarcs-belief-ingest-events (events &optional save?)
  "Apply EVENTS to the current belief state and optionally persist.
EVENTS is a list of M-INC-compatible plists carrying `:entity-id'.
When SAVE? is non-nil, write the resulting state to
`arxana-vsatarcs-belief-store-file' before returning.  Returns the
post-ingest state."
  (setq arxana-vsatarcs-belief--current
        (arxana-vsatarcs-belief-update-batch
         arxana-vsatarcs-belief--current events))
  (when save? (arxana-vsatarcs-belief-save))
  arxana-vsatarcs-belief--current)

;; ---------------------------------------------------------------------
;; Snapshot — chrome-friendly projection of the current state
;; ---------------------------------------------------------------------

;; ---------------------------------------------------------------------
;; Prior bootstrap from the canonical projection source
;;
;; v0.2.2 ships a bootstrap entry point that initialises per-entity
;; priors from `stack-annotations.edn` — the canonical source the
;; alignment mission projects from.  The bootstrap is non-destructive:
;; entities already in the current belief state are left alone (any
;; accumulated evidence is preserved); only new entity-ids gain a
;; uniform prior.
;;
;; One-step-lookahead integration points enabled by this bootstrap
;; (recorded so future moves don't lose sight of WHY we bootstrap):
;;
;; - :alignment-drift-detection — once both VSATARCS and WM (futon2.aif.belief)
;;   carry priors on the same entity domain, per-entity posterior
;;   comparison becomes non-trivial.  Today both sides start empty;
;;   comparison is vacuously equal.  After bootstrap, comparison
;;   becomes the operational alignment check the mission's T3 cadence
;;   (`M-stack-essay-code-alignment.md` §2.3) is designed to consume.
;;
;; - :inc-event-targeting — live M-INC `state/*' events from step (b)
;;   land on already-known entities rather than auto-creating uniform
;;   priors on first contact.  This sharpens the V-shrink-shape signal
;;   (entropy strictly decreases from log n) and lets the event stream
;;   be audited against the canonical entity-set.
;;
;; - :story-scoped-filtering (v0.3 / R2) — once entities are bootstrapped,
;;   story → entity-id lookup has a populated domain to filter against.
;;   The R2 observation channel "entities referenced by current story"
;;   is the natural projector once it lands.
;;
;; - :r8-trace-grounding — eventual per-tick trace records reference
;;   entity-ids that map to bootstrapped entries; trace becomes
;;   queryable by entity provenance rather than by event-order alone.
;;
;; - :wm-side-symmetric-bootstrap — gives the WM side a model to
;;   follow (or, ideally, both sides bootstrap from the same canonical
;;   source as a `:code-docs-correspondence' instance — deferred until
;;   the landing surface exists per Joe's direction 2026-05-17).
;; ---------------------------------------------------------------------

(defcustom arxana-vsatarcs-belief-stack-annotations-path
  (expand-file-name "~/code/futon5a/holes/stack-annotations.edn")
  "Path of the canonical projection source for VSATARCS belief bootstrap.
Read by `arxana-vsatarcs-belief-bootstrap-from-stack-annotations'.
Tagged literals (`#inst') and sets (`#{...}') are handled by the
minimal EDN reader in `arxana-browser-rewrites'."
  :type 'file
  :group 'arxana-vsatarcs)

(defun arxana-vsatarcs-belief--section-ids-from-stack-annotations (path)
  "Return the list of `:sections[]' `:id' strings from PATH.
Uses the EDN reader from `arxana-browser-rewrites'.  Returns nil if
PATH is unreadable or the file has no `:sections' key."
  (when (file-readable-p path)
    (require 'arxana-browser-rewrites)
    (let* ((data (arxana-browser-rewrites--read-edn-file path))
           (sections (plist-get data :sections)))
      (delq nil (mapcar (lambda (s) (plist-get s :id)) sections)))))

(defun arxana-vsatarcs-belief-bootstrap-from-stack-annotations (&optional path save?)
  "Initialise per-entity priors from PATH (an EDN with `:sections[]').
PATH defaults to `arxana-vsatarcs-belief-stack-annotations-path'.
Non-destructive: entities already in the current state are left
untouched (accumulated evidence is preserved); only unknown ids gain
a uniform prior.  Returns the number of new entities added.  When
SAVE? is non-nil, persists the resulting state to
`arxana-vsatarcs-belief-store-file'."
  (interactive)
  (let* ((source (or path arxana-vsatarcs-belief-stack-annotations-path))
         (ids (arxana-vsatarcs-belief--section-ids-from-stack-annotations source))
         (added 0))
    (unless ids
      (user-error "No section ids found in %s" source))
    (dolist (id ids)
      (unless (assoc id arxana-vsatarcs-belief--current)
        (setq arxana-vsatarcs-belief--current
              (cons (cons id (arxana-vsatarcs-belief-uniform-prior))
                    arxana-vsatarcs-belief--current))
        (cl-incf added)))
    (when save? (arxana-vsatarcs-belief-save))
    added))

;; ---------------------------------------------------------------------
;; Bilateral comparison primitive
;;
;; The alignment mission compares VSATARCS-side belief against WM-side
;; belief on shared entity-ids.  `arxana-vsatarcs-belief-compare' is the
;; local primitive that future cross-side checks consume: pure-function,
;; takes two belief alists, returns a drift report.  The actual WM-side
;; read path (Drawbridge or analogous) is deferred per `:enables'
;; :wm-side-bridge on the v0.2.4 closure; this primitive lets the
;; comparison shape be exercised with synthetic states today and reused
;; verbatim once the read path lands.
;; ---------------------------------------------------------------------

(defcustom arxana-vsatarcs-belief-compare-epsilon 1.0e-6
  "Tolerance for posterior equality in `arxana-vsatarcs-belief-compare'.
Two entities are considered to have matching posteriors if every
status's probability differs by less than this value.  Default 1e-6
is well below any meaningful belief delta."
  :type 'number
  :group 'arxana-vsatarcs)

(defun arxana-vsatarcs-belief--posterior-max-abs-diff (post-a post-b)
  "Return the largest absolute difference across statuses of POST-A vs POST-B.
Both inputs must cover the same status set."
  (apply #'max
         (mapcar (lambda (s)
                   (abs (- (or (cdr (assoc s post-a)) 0.0)
                           (or (cdr (assoc s post-b)) 0.0))))
                 arxana-vsatarcs-belief-status-set)))

(defun arxana-vsatarcs-belief-compare (belief-a belief-b &optional epsilon)
  "Compare two belief alists; return a drift report as a plist.

Report shape:
  (:only-in-a       (<entity-id> ...)
   :only-in-b       (<entity-id> ...)
   :posterior-diffs ((<entity-id> . <max-abs-diff>) ...)
   :equal-count     <integer>)

`:only-in-a' and `:only-in-b' name entity-ids present on one side
but not the other.  `:posterior-diffs' covers entities present in
both whose per-status probabilities differ by more than EPSILON
(defaults to `arxana-vsatarcs-belief-compare-epsilon').
`:equal-count' is the number of entities present in both whose
posteriors agree within EPSILON.

The function is pure (does not touch the in-memory current state)."
  (let* ((eps (or epsilon arxana-vsatarcs-belief-compare-epsilon))
         (ids-a (mapcar #'car belief-a))
         (ids-b (mapcar #'car belief-b))
         (only-in-a (cl-remove-if (lambda (id) (assoc id belief-b)) ids-a))
         (only-in-b (cl-remove-if (lambda (id) (assoc id belief-a)) ids-b))
         (shared (cl-remove-if-not (lambda (id) (assoc id belief-b)) ids-a))
         (diffs nil)
         (equal-count 0))
    (dolist (id shared)
      (let ((delta (arxana-vsatarcs-belief--posterior-max-abs-diff
                    (cdr (assoc id belief-a))
                    (cdr (assoc id belief-b)))))
        (if (> delta eps)
            (push (cons id delta) diffs)
          (cl-incf equal-count))))
    (list :only-in-a only-in-a
          :only-in-b only-in-b
          :posterior-diffs (nreverse diffs)
          :equal-count equal-count)))

;; ---------------------------------------------------------------------
;; Operator-triggerable wrappers (bound in the reader mode-map)
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-belief-bootstrap-and-redisplay ()
  "Bootstrap belief from the canonical source, persist, and re-render reader.
Convenience wrapper for `B' inside the VSATARCS reader buffer."
  (interactive)
  (let ((added (arxana-vsatarcs-belief-bootstrap-from-stack-annotations nil t)))
    (when (and (derived-mode-p 'arxana-vsatarcs-mode)
               (boundp 'arxana-vsatarcs--source-file)
               arxana-vsatarcs--source-file
               (fboundp 'arxana-vsatarcs-reload))
      (arxana-vsatarcs-reload))
    (message "Bootstrapped %d new entities; belief has %d total."
             added (length (arxana-vsatarcs-belief-current)))))

(defun arxana-vsatarcs-belief-reset-and-redisplay ()
  "Reset the in-memory belief (does NOT touch the store file) and re-render.
Convenience wrapper for `R' inside the VSATARCS reader buffer.  Prompts
for confirmation."
  (interactive)
  (when (y-or-n-p "Reset VSATARCS in-memory belief? (store file is not touched) ")
    (arxana-vsatarcs-belief-reset)
    (when (and (derived-mode-p 'arxana-vsatarcs-mode)
               (boundp 'arxana-vsatarcs--source-file)
               arxana-vsatarcs--source-file
               (fboundp 'arxana-vsatarcs-reload))
      (arxana-vsatarcs-reload))
    (message "Belief reset (in-memory only).")))

(defun arxana-vsatarcs-belief-ingest-interactive (events-form)
  "Read an event-list elisp form from the minibuffer; ingest + persist + re-render.
Convenience wrapper for `i' inside the VSATARCS reader buffer.

EVENTS-FORM is the string read from the minibuffer; it must parse to
a list of M-INC-compatible plists, each carrying `:entity-id', `:type',
and optionally `:weight'."
  (interactive (list (read-from-minibuffer
                      "Events (e.g. ((:entity-id \"arxana/.../leaf/2\" :type :strengthened :weight 1.0))): ")))
  (let* ((events (car (read-from-string events-form)))
         (n (length events)))
    (unless (listp events)
      (user-error "Input must parse to a list of events; got %S" events))
    (arxana-vsatarcs-belief-ingest-events events t)
    (when (and (derived-mode-p 'arxana-vsatarcs-mode)
               (boundp 'arxana-vsatarcs--source-file)
               arxana-vsatarcs--source-file
               (fboundp 'arxana-vsatarcs-reload))
      (arxana-vsatarcs-reload))
    (message "Ingested %d event(s); belief has %d entities."
             n (length (arxana-vsatarcs-belief-current)))))

(defun arxana-vsatarcs-belief-snapshot (&optional belief)
  "Return a list of (entity-id status entropy posterior) tuples.
BELIEF defaults to the current in-memory state.  Sorted by descending
entropy (most-uncertain first); ties broken by `entity-id' under
`string<' on `format'-stringified ids.  Returns nil when no entities
are tracked."
  (let ((src (or belief arxana-vsatarcs-belief--current)))
    (when src
      (sort
       (mapcar (lambda (kv)
                 (let* ((eid (car kv))
                        (post (cdr kv))
                        (status (arxana-vsatarcs-belief-most-likely-status post))
                        (h (arxana-vsatarcs-belief-entropy post)))
                   (list eid status h post)))
               src)
       (lambda (a b)
         (cond
          ((> (nth 2 a) (nth 2 b)) t)
          ((< (nth 2 a) (nth 2 b)) nil)
          (t (string< (format "%s" (car a)) (format "%s" (car b))))))))))

(provide 'arxana-vsatarcs-belief)
;;; arxana-vsatarcs-belief.el ends here
