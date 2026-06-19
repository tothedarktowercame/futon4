;;; arxana-vsatarcs-wm-decision.el --- WM decision surfacing for VSATARCS reader chrome -*- lexical-binding: t; -*-

;;; Commentary:
;; Reader-criteria V-CUR + V-COV closure (Q2 of
;; `~/code/futon2/docs/vsatarcs-reader-criteria.md').  Reads the WM
;; trace's latest record (today's `wm-trace-YYYY-MM-DD.edn' by default)
;; and surfaces the `:decision' field — `:action' (type, target,
;; rationale), `:rank', `:G-total', `:tau' — plus the strategic
;; `:mode' and the record `:timestamp', so a reader can see
;; "what action does the WM rank highest right now, and why?"
;; without opening the trace.
;;
;; The module composes with `arxana-vsatarcs-wm-bridge.el' for the
;; read primitive (`arxana-vsatarcs-wm-bridge--read-last-record');
;; same trace-file resolution discipline.  Reads on every snapshot
;; call; no in-memory cache.  When R10's file-notify tap is active,
;; the chrome refreshes automatically as the WM emits new records.
;;
;; Cross-mapping to local apparatus: when VSATARCs gains writer
;; capability (R4/R5/R6), it will emit its own `:decision' field per
;; tick.  Until then, the chrome surfaces the WM's choice and the
;; operator reads it as authoritative for stack-level action priority.
;;
;; Contract: contributes to V-CUR (latest WM decision in the chrome)
;; + V-COV (decision rationale + G-total + mode all surfaced, not
;; just the action keyword).

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-wm-bridge) ; for --read-last-record + --trace-path-for-date

(defgroup arxana-vsatarcs-wm-decision nil
  "WM decision surfacing for the VSATARCS reader surface."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-wm-decision-top-k 3
  "Number of top-ranked actions to surface in the decision snapshot.
Claude-2 (WM-side handoff 2026-05-20) recommended top-K composition
over verbatim-only because Q2's operator-facing question is `what
was decided AND why (vs the close alternatives)' — K=3 keeps the
chrome block compact while showing the close runners-up.  The
chosen action is always at rank 1; the snapshot's `:top-k' field
holds ranks 1..K inclusive."
  :type 'integer
  :group 'arxana-vsatarcs-wm-decision)

(defcustom arxana-vsatarcs-wm-decision-mu-shift-epsilon 1.0e-6
  "Per-posterior-entry epsilon for counting entities whose belief shifted.
The R3d global belief update applies a (small, annealed) weight to
all entities on every WM call.  Operator-meaningful µ-shift = at
least one status's probability moved by more than this epsilon
between `:mu-pre' and `:mu-post' on at least one entity."
  :type 'number
  :group 'arxana-vsatarcs-wm-decision)

(defun arxana-vsatarcs-wm-decision--plist-get (record key)
  "Return KEY from RECORD (a plist returned by the shared EDN reader).
The reader interns keys as symbols whose names retain the leading
`:'; KEY is the bare keyword form."
  (plist-get record (intern (symbol-name key))))

(defun arxana-vsatarcs-wm-decision--strip-leading-colon (sym)
  "Return symbol SYM with leading `:' stripped and re-keyworded."
  (when sym
    (let ((s (symbol-name sym)))
      (if (string-prefix-p ":" s)
          (intern (concat ":" (substring s 1)))
        (intern (concat ":" s))))))

(defun arxana-vsatarcs-wm-decision--summarise-action (entry)
  "Return an operator-facing plist for one `:ranked-actions' ENTRY.
ENTRY is a plist as returned by the shared EDN reader (keys like
`:rank', `:G-total', `:time-pressure', `:horizon-steps',
`:action').  Output carries `:rank', `:action-type', `:target',
`:G-total', `:time-pressure', `:horizon-steps'."
  (let* ((action (arxana-vsatarcs-wm-decision--plist-get entry :action))
         (type (and action
                    (arxana-vsatarcs-wm-decision--plist-get action :type)))
         (target (and action
                      (arxana-vsatarcs-wm-decision--plist-get
                       action :target))))
    (list :rank          (arxana-vsatarcs-wm-decision--plist-get entry :rank)
          :action-type   (and (symbolp type)
                              (arxana-vsatarcs-wm-decision--strip-leading-colon
                               type))
          :target        (cond
                          ((symbolp target)
                           (arxana-vsatarcs-wm-decision--strip-leading-colon
                            target))
                          (t target))
          :G-total       (arxana-vsatarcs-wm-decision--plist-get
                          entry :G-total)
          :time-pressure (arxana-vsatarcs-wm-decision--plist-get
                          entry :time-pressure)
          :horizon-steps (arxana-vsatarcs-wm-decision--plist-get
                          entry :horizon-steps))))

(defun arxana-vsatarcs-wm-decision--top-k-alternatives (record k)
  "Return top K alternatives from RECORD's `:ranked-actions' (1-indexed).
Returns a list of action-summary plists (per `--summarise-action')
sorted by rank ascending — rank 1 is the chosen action, ranks 2..K
are the close alternatives.  When `:ranked-actions' is absent or
shorter than K, returns whatever exists."
  (let* ((ra (arxana-vsatarcs-wm-decision--plist-get record :ranked-actions))
         (entries (and ra (append ra nil)))
         (with-ranks (cl-remove-if-not
                      (lambda (e) (numberp
                                   (arxana-vsatarcs-wm-decision--plist-get
                                    e :rank)))
                      entries))
         (sorted (sort (copy-sequence with-ranks)
                       (lambda (a b)
                         (< (arxana-vsatarcs-wm-decision--plist-get a :rank)
                            (arxana-vsatarcs-wm-decision--plist-get b :rank)))))
         (top (cl-subseq sorted 0 (min k (length sorted)))))
    (mapcar #'arxana-vsatarcs-wm-decision--summarise-action top)))

(defun arxana-vsatarcs-wm-decision--posterior-equal? (a b epsilon)
  "Return t when posterior plists A and B agree within EPSILON on every key.
A posterior is alternating (status-symbol probability) pairs.  Returns
nil as soon as any status differs by more than EPSILON, so the caller
can stop scanning at the first disagreement."
  (let ((all-equal t)
        (pa (copy-sequence a)))
    (while (and all-equal pa)
      (let* ((k (car pa))
             (va (cadr pa))
             (vb (plist-get b k)))
        (unless (and (numberp va) (numberp vb)
                     (< (abs (- va vb)) epsilon))
          (setq all-equal nil)))
      (setq pa (cddr pa)))
    all-equal))

(defun arxana-vsatarcs-wm-decision--mu-shift-count (record epsilon)
  "Return the number of entities whose `:mu-pre' and `:mu-post' differ.
Iterates entities in `:mu-pre', looks up the same entity in `:mu-post',
counts a shift when at least one status's probability differs by more
than EPSILON.  When `:mu-pre' or `:mu-post' is absent, returns 0."
  (let* ((pre (arxana-vsatarcs-wm-decision--plist-get record :mu-pre))
         (post (arxana-vsatarcs-wm-decision--plist-get record :mu-post))
         (count 0))
    (when (and pre post)
      (let ((pa pre))
        (while pa
          (let* ((id (car pa))
                 (post-id (plist-get post id))
                 (pre-id (cadr pa)))
            (when (and pre-id post-id
                       (not (arxana-vsatarcs-wm-decision--posterior-equal?
                             pre-id post-id epsilon)))
              (cl-incf count)))
          (setq pa (cddr pa)))))
    count))

(defun arxana-vsatarcs-wm-decision--summarise (record)
  "Return an operator-facing plist summary of the WM decision in RECORD.
The output carries:
  `:timestamp'   — string from the record
  `:mode'        — strategic-mode keyword (bare)
  `:action-type' — bare keyword (`:address-sorry', `:open-mission', ...)
  `:target'      — what the action operates on (string or bare keyword)
  `:rank'        — integer rank of the chosen action (1 = top)
  `:G-total'     — float expected free energy of the choice
  `:tau'         — softmax temperature
  `:weight'      — softmax weight assigned to the choice
  `:rationale'   — short string explaining the choice
  `:decision-present?' — t when :decision exists; nil otherwise"
  (let* ((decision (arxana-vsatarcs-wm-decision--plist-get record :decision))
         (action (and decision
                      (arxana-vsatarcs-wm-decision--plist-get
                       decision :action)))
         (action-type (and action
                           (arxana-vsatarcs-wm-decision--plist-get
                            action :type)))
         (target (and action
                      (arxana-vsatarcs-wm-decision--plist-get
                       action :target)))
         (rationale (and action
                         (arxana-vsatarcs-wm-decision--plist-get
                          action :rationale)))
         (weight (and action
                      (arxana-vsatarcs-wm-decision--plist-get
                       action :weight)))
         (rank (and decision
                    (arxana-vsatarcs-wm-decision--plist-get decision :rank)))
         (g-total (and decision
                       (arxana-vsatarcs-wm-decision--plist-get
                        decision :G-total)))
         (tau (and decision
                   (arxana-vsatarcs-wm-decision--plist-get decision :tau)))
         (mode (arxana-vsatarcs-wm-decision--plist-get record :mode))
         (timestamp (arxana-vsatarcs-wm-decision--plist-get
                     record :timestamp)))
    (list :timestamp timestamp
          :mode (and (symbolp mode)
                     (arxana-vsatarcs-wm-decision--strip-leading-colon mode))
          :action-type (and (symbolp action-type)
                            (arxana-vsatarcs-wm-decision--strip-leading-colon
                             action-type))
          :target (cond
                   ((symbolp target)
                    (arxana-vsatarcs-wm-decision--strip-leading-colon target))
                   (t target))
          :rank rank
          :G-total g-total
          :tau tau
          :weight weight
          :rationale rationale
          :decision-present? (not (null decision))
          :top-k (and decision
                      (arxana-vsatarcs-wm-decision--top-k-alternatives
                       record arxana-vsatarcs-wm-decision-top-k))
          :mu-shift-count (arxana-vsatarcs-wm-decision--mu-shift-count
                           record arxana-vsatarcs-wm-decision-mu-shift-epsilon)
          :horizon-steps (when decision
                           ;; Surface the chosen action's horizon-steps
                           ;; via the rank-1 entry of :ranked-actions if
                           ;; present; else nil.
                           (let ((rank-1
                                  (cl-find 1
                                           (or (arxana-vsatarcs-wm-decision--plist-get
                                                record :ranked-actions)
                                               [])
                                           :key (lambda (e)
                                                  (arxana-vsatarcs-wm-decision--plist-get
                                                   e :rank)))))
                             (and rank-1
                                  (arxana-vsatarcs-wm-decision--plist-get
                                   rank-1 :horizon-steps))))
          :chosen-time-pressure
          (when decision
            (let ((rank-1
                   (cl-find 1
                            (or (arxana-vsatarcs-wm-decision--plist-get
                                 record :ranked-actions)
                                [])
                            :key (lambda (e)
                                   (arxana-vsatarcs-wm-decision--plist-get
                                    e :rank)))))
              (and rank-1
                   (arxana-vsatarcs-wm-decision--plist-get
                    rank-1 :time-pressure))))
          :gap-report (arxana-vsatarcs-wm-decision--plist-get
                       decision :gap-report))))

(defun arxana-vsatarcs-wm-decision-snapshot (&optional date)
  "Return the WM decision snapshot for DATE's trace (default: today).

The snapshot is a plist:

  (:trace-loaded?      <t or nil>
   :trace-date         <YYYY-MM-DD>
   :trace-path         <absolute path read>
   :record-present?    <t or nil>
   :decision-present?  <t or nil>
   :summary            (<summary-plist> ... — see --summarise)
   :digest-line        <one-line operator-facing summary>)

The snapshot's shape is stable across (trace-missing /
record-missing / decision-missing) cases so the chrome layout
remains predictable even on empty days."
  (let* ((date (or date (arxana-vsatarcs-wm-bridge--today-iso)))
         (path (arxana-vsatarcs-wm-bridge--trace-path-for-date date))
         (loaded? (file-readable-p path))
         (record (and loaded?
                      (arxana-vsatarcs-wm-bridge--read-last-record path)))
         (summary (arxana-vsatarcs-wm-decision--summarise (or record nil)))
         (decision-present? (plist-get summary :decision-present?)))
    (list :trace-loaded? loaded?
          :trace-date date
          :trace-path path
          :record-present? (not (null record))
          :decision-present? decision-present?
          :summary summary
          :digest-line (arxana-vsatarcs-wm-decision--digest summary
                                                            loaded?
                                                            (not (null record))))))

(defun arxana-vsatarcs-wm-decision--digest (summary loaded? record-present?)
  "Render a one-line digest from SUMMARY + load state.
Distinguishes (no-trace) / (trace-empty) / (decision-missing) /
(decision-present) cases so the operator can read state without
opening the source."
  (cond
   ((not loaded?) "no trace file for date")
   ((not record-present?) "trace file empty")
   ((not (plist-get summary :decision-present?))
    "trace record has no :decision field")
   (t
    (format "%s :target %s (rank %s, G=%.3f, tau=%.3f)%s"
            (or (plist-get summary :action-type) "?")
            (let ((t- (plist-get summary :target)))
              (cond
               ((null t-) "?")
               ((symbolp t-) (symbol-name t-))
               (t (format "%s" t-))))
            (or (plist-get summary :rank) "?")
            (or (plist-get summary :G-total) 0.0)
            (or (plist-get summary :tau) 0.0)
            (let ((m (plist-get summary :mode)))
              (if m (format " [mode %s]" m) ""))))))

(provide 'arxana-vsatarcs-wm-decision)
;;; arxana-vsatarcs-wm-decision.el ends here
