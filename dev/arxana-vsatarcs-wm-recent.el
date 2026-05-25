;;; arxana-vsatarcs-wm-recent.el --- Recent-trace + belief-drift surfacing for VSATARCS reader chrome -*- lexical-binding: t; -*-

;;; Commentary:
;; Reader-criteria V-CUR + V-COV closure (Q5 + Q6 of
;; `~/code/futon2/docs/vsatarcs-reader-criteria.md').  Reads the last
;; N records from today's WM trace via the bridge's
;; `--read-all-records' primitive and exposes two operator-facing
;; views over the same window:
;;
;;   Q6 — Recent-trace summary: per-record digest of
;;        (timestamp, mode, decision-action, decision-target,
;;        G-total, chosen-action time-pressure, µ-shift count).
;;
;;   Q5 — Per-entity belief drift across the window: for each entity
;;        in the SHARED set across all records' `:mu-post', compute
;;        the max-abs-diff of any single posterior probability across
;;        the window, then report top-K-most-moved.  Companion
;;        question to the bridge's existing one-shot drift compare
;;        (which is local-vs-WM in current state); Q5 is WM-history
;;        over its own trace.
;;
;; The module composes with `arxana-vsatarcs-wm-bridge.el' for the
;; multi-record read and with `arxana-vsatarcs-wm-decision.el' for
;; the per-record summarisation primitives.  Reads on every snapshot
;; (no in-memory cache); the same R10 file-notify tap that drives Q2
;; refresh also drives this — operator opens VSATARCs and the
;; recent-trace block reflects the latest WM emissions.
;;
;; **Why one module for two questions:** the reader-criteria doc lists
;; Q5 and Q6 separately, but both need the same primitive — read the
;; recent N records — and both project from the same window.  Doing
;; one read and two projections is the right shape; the alternative
;; (two modules each reading the trace) would re-do the same I/O.
;;
;; Contract: contributes to V-CUR (recent trajectory in the chrome),
;; V-COV (multiple trace fields surfaced beyond `:mu-post' alone),
;; and V-BIL (drift trajectory comparable across sides once both
;; sides emit recent-trace projections).

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-wm-bridge) ; --read-all-records + --trace-path-for-date
(require 'arxana-vsatarcs-wm-decision) ; --summarise-action + --mu-shift-count

(defgroup arxana-vsatarcs-wm-recent nil
  "Recent-trace + belief-drift surfacing for the VSATARCS reader."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-wm-recent-window-size 5
  "Number of recent records to surface in the recent-trace + drift views.
The window slides from the file's end backward; smaller windows favour
chrome-compactness, larger windows favour trajectory visibility.
Default 5 matches the operator's typical visual scan capacity."
  :type 'integer
  :group 'arxana-vsatarcs-wm-recent)

(defcustom arxana-vsatarcs-wm-recent-top-k-moved 5
  "Number of top-most-moved entities to surface in the drift view.
Per Q5's reader-criterion: 'render top-K-most-moved'.  Default 5
matches the chrome's compactness norm; the operator drills into the
trace for the full distribution if needed."
  :type 'integer
  :group 'arxana-vsatarcs-wm-recent)

(defcustom arxana-vsatarcs-wm-recent-drift-epsilon 1.0e-6
  "Per-status-probability epsilon below which drift is treated as no-op.
Entities whose max-abs-diff across the window is below this floor are
filtered out of the top-K-most-moved list — they're operator-uninteresting
quiescent entities."
  :type 'number
  :group 'arxana-vsatarcs-wm-recent)

(defun arxana-vsatarcs-wm-recent--plist-get (rec key)
  "Plist accessor that handles the shared EDN reader's keyword-as-symbol form."
  (plist-get rec (intern (symbol-name key))))

(defun arxana-vsatarcs-wm-recent--summarise-record (rec)
  "Return an operator-facing plist summary of one trace record REC.
The output carries `:timestamp', `:mode', `:decision-action',
`:decision-target', `:G-total', `:tau', `:time-pressure',
`:mu-shift-count'.  Composes the existing
`arxana-vsatarcs-wm-decision--mu-shift-count' for per-record µ-shift."
  (let* ((ts (arxana-vsatarcs-wm-recent--plist-get rec :timestamp))
         (mode (arxana-vsatarcs-wm-recent--plist-get rec :mode))
         (decision (arxana-vsatarcs-wm-recent--plist-get rec :decision))
         (action (and decision
                      (arxana-vsatarcs-wm-recent--plist-get
                       decision :action)))
         (action-type (and action
                           (arxana-vsatarcs-wm-recent--plist-get
                            action :type)))
         (target (and action
                      (arxana-vsatarcs-wm-recent--plist-get action :target)))
         (g-total (and decision
                       (arxana-vsatarcs-wm-recent--plist-get
                        decision :G-total)))
         (tau (and decision
                   (arxana-vsatarcs-wm-recent--plist-get decision :tau)))
         (ranked (arxana-vsatarcs-wm-recent--plist-get rec :ranked-actions))
         (rank-1 (and ranked
                      (cl-find 1 (append ranked nil)
                               :key (lambda (e)
                                      (arxana-vsatarcs-wm-recent--plist-get
                                       e :rank)))))
         (tp (and rank-1
                  (arxana-vsatarcs-wm-recent--plist-get
                   rank-1 :time-pressure))))
    (list :timestamp ts
          :mode mode
          :decision-action action-type
          :decision-target target
          :G-total g-total
          :tau tau
          :time-pressure tp
          :mu-shift-count (arxana-vsatarcs-wm-decision--mu-shift-count
                           rec arxana-vsatarcs-wm-decision-mu-shift-epsilon))))

(defun arxana-vsatarcs-wm-recent--shared-entity-ids (records)
  "Return the set of entity ids common to all RECORDS' `:mu-post'.
Two normalisation rules:
  1. `:mu-post' is a plist of alternating (entity-id posterior) pairs;
     we extract the entity-id keys.
  2. Only entities present in EVERY record participate in the drift
     trajectory.  This keeps the drift signal honest — entities that
     appear midway can't have a max-abs-diff over the full window."
  (let ((sets (mapcar
               (lambda (rec)
                 (let ((mp (arxana-vsatarcs-wm-recent--plist-get
                            rec :mu-post))
                       acc)
                   (while mp
                     (push (car mp) acc)
                     (setq mp (cddr mp)))
                   acc))
               records)))
    (if (null sets) nil
      (cl-reduce (lambda (a b)
                   (cl-intersection a b :test #'equal))
                 sets))))

(defun arxana-vsatarcs-wm-recent--posterior-max-abs-diff (a b)
  "Return the max absolute difference between matching keys in posteriors A and B.
A and B are plists alternating (status-keyword probability).  Returns
0.0 when both are nil; treats missing keys as 0.0."
  (let ((m 0.0)
        (keys (let (ks (pa a) (pb b))
                (while pa (push (car pa) ks) (setq pa (cddr pa)))
                (while pb (push (car pb) ks) (setq pb (cddr pb)))
                (delete-dups ks))))
    (dolist (k keys m)
      (let ((va (or (plist-get a k) 0.0))
            (vb (or (plist-get b k) 0.0)))
        (when (and (numberp va) (numberp vb))
          (setq m (max m (abs (- va vb)))))))))

(defun arxana-vsatarcs-wm-recent--entity-trajectory-max-diff (id records)
  "Return the max-abs-diff for entity ID's `:mu-post' across RECORDS.
Computes pairwise max-abs-diff between consecutive records' posteriors
for ID, then returns the maximum.  Quiescent entities yield 0.0."
  (let ((trajectory (mapcar (lambda (rec)
                              (let ((mp (arxana-vsatarcs-wm-recent--plist-get
                                         rec :mu-post)))
                                (plist-get mp id)))
                            records))
        (m 0.0))
    (when trajectory
      (let ((prev (car trajectory))
            (rest (cdr trajectory)))
        (dolist (cur rest)
          (setq m (max m (arxana-vsatarcs-wm-recent--posterior-max-abs-diff
                          prev cur)))
          (setq prev cur))))
    m))

(defun arxana-vsatarcs-wm-recent--top-k-moved (records k epsilon)
  "Return the top-K entities by drift trajectory across RECORDS.
Filters out entities whose max-abs-diff is below EPSILON.  Returns a
list of plists `(:id <id> :max-abs-diff <float>)' sorted by
max-abs-diff descending, length ≤ K."
  (let* ((shared (arxana-vsatarcs-wm-recent--shared-entity-ids records))
         (scored (mapcar (lambda (id)
                           (cons id
                                 (arxana-vsatarcs-wm-recent--entity-trajectory-max-diff
                                  id records)))
                         shared))
         (above-floor (cl-remove-if (lambda (cell) (< (cdr cell) epsilon))
                                    scored))
         (sorted (sort above-floor (lambda (a b) (> (cdr a) (cdr b)))))
         (top (cl-subseq sorted 0 (min k (length sorted)))))
    (mapcar (lambda (cell)
              (list :id (car cell) :max-abs-diff (cdr cell)))
            top)))

(defun arxana-vsatarcs-wm-recent-snapshot (&optional date)
  "Return the recent-trace + drift snapshot for DATE's trace.
DATE defaults to today.  The snapshot is a plist:

  (:trace-loaded?      <t or nil>
   :trace-date         <YYYY-MM-DD>
   :trace-path         <absolute path read>
   :window-size        <integer — actual records consumed; ≤ window-size custom>
   :total-records      <integer — total records in the trace>
   :recent             ((<record-summary> ...) — newest last)
   :shared-entities    <integer — count of entities present in every record>
   :top-k-moved        ((:id <id> :max-abs-diff <float>) ...)
   :digest-line        <one-line operator-facing summary>)

When the window is shorter than the custom window-size (fewer records
exist), `:window-size' reports the actual count and the trajectory
still computes (a single record yields no drift; two records yield
one pairwise diff, etc.)."
  (let* ((date (or date (arxana-vsatarcs-wm-bridge--today-iso)))
         (path (arxana-vsatarcs-wm-bridge--trace-path-for-date date))
         (loaded? (file-readable-p path))
         (all (and loaded?
                   (arxana-vsatarcs-wm-bridge--read-all-records path)))
         (total (length (or all nil)))
         (window-size (min arxana-vsatarcs-wm-recent-window-size total))
         (window (when (> window-size 0)
                   (cl-subseq all (- total window-size))))
         (summaries (mapcar #'arxana-vsatarcs-wm-recent--summarise-record
                            (or window nil)))
         (shared (and window
                      (arxana-vsatarcs-wm-recent--shared-entity-ids window)))
         (top-k (and window
                     (>= (length window) 2)
                     (arxana-vsatarcs-wm-recent--top-k-moved
                      window
                      arxana-vsatarcs-wm-recent-top-k-moved
                      arxana-vsatarcs-wm-recent-drift-epsilon)))
         (digest (arxana-vsatarcs-wm-recent--digest
                  loaded? total window-size top-k)))
    (list :trace-loaded? loaded?
          :trace-date date
          :trace-path path
          :window-size window-size
          :total-records total
          :recent summaries
          :shared-entities (length shared)
          :top-k-moved (or top-k nil)
          :digest-line digest)))

(defun arxana-vsatarcs-wm-recent--digest (loaded? total window-size top-k)
  "Render a one-line digest covering the four edge cases."
  (cond
   ((not loaded?) "no trace file for date")
   ((zerop total) "trace file empty")
   ((< window-size 2)
    (format "%d record total; %d-record window — drift needs ≥2 records"
            total window-size))
   (t
    (format "%d records (window %d); %d entities with drift above floor; top mover: %s"
            total window-size
            (length top-k)
            (if top-k
                (let ((id (plist-get (car top-k) :id)))
                  (cond ((stringp id) id)
                        ((symbolp id) (symbol-name id))
                        (t (format "%s" id))))
              "(none — quiescent window)")))))

(provide 'arxana-vsatarcs-wm-recent)
;;; arxana-vsatarcs-wm-recent.el ends here
