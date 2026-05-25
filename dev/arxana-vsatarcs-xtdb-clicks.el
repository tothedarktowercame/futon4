;;; arxana-vsatarcs-xtdb-clicks.el --- XTDB-projection for VSATARCS engagement-time surface -*- lexical-binding: t; -*-

;;; Commentary:
;; R10 (live operation) engagement-time surface for the VSATARCS reader.
;; Queries futon1a XTDB via `arxana-store-fetch-hyperedges' for typed
;; event streams that already exist in the substrate — multi_watcher's
;; `code/v05/watcher-event' hyperedges (per `~/code/futon3/scripts/multi_watcher.clj'
;; + the JVM port), and additional click sources as they land
;; (invoke ledger, M-INC `state/*' events when step (b) commits).
;;
;; **Design redirect (Joe 2026-05-20):** v0.5.17 pioneered a local
;; click-log EDN file with multi-dir file-notify; v0.5.18 reverts that
;; substrate-pioneering move because multi_watcher + futon1a XTDB
;; already capture these events.  This module IS the click-substrate
;; projection — same surface multi_watcher's emissions land in,
;; queried at chrome-render time rather than re-discovered via
;; parallel file-watches.  No separate file, no parallel watcher; the
;; engagement-time surface lives where every other typed event lives.
;;
;; **Click taxonomy alignment** (per `~/code/futon2/README-clicks-and-ticks.md'
;; §"Two primary subclasses of click"): the watcher-event hyperedges
;; map to autonomous clicks (multi_watcher's polling cycle generates
;; them); interactive `:turn' clicks will land via the futon1a invoke
;; ledger (`:duree-click-invoke') when that surface is queried here;
;; both subclasses flow through the same XTDB store + this module's
;; projection.
;;
;; **Defensive query discipline:** futon1a's index on hx-type can be
;; slow on large event streams (`code/v05/watcher-event' has many
;; thousands of records).  Default `:limit' is small (10); query
;; failures or timeouts return a degenerate snapshot rather than
;; blocking chrome.  Operator can configure a wider window via
;; `arxana-vsatarcs-xtdb-clicks-default-limit' when the index improves
;; or when querying a smaller event stream.
;;
;; Contract: contributes to R10 (live operation — engagement-time
;; surface beyond WM-trace-only) by READING the shared substrate
;; rather than maintaining its own; bilateral by construction (both
;; sides query the same XTDB).

;;; Code:

(require 'cl-lib)
(require 'arxana-store)

(defgroup arxana-vsatarcs-xtdb-clicks nil
  "XTDB-projection for the VSATARCS engagement-time surface."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-xtdb-clicks-default-limit 10
  "Default query limit for click queries.
Kept small because futon1a's hx-type index can be slow on large
streams.  Operator can pass an explicit limit to
`arxana-vsatarcs-xtdb-clicks-snapshot' for one-off larger fetches."
  :type 'integer
  :group 'arxana-vsatarcs-xtdb-clicks)

(defcustom arxana-vsatarcs-xtdb-clicks-per-stream-timeout 2
  "Seconds before a per-stream query times out (returns nil).
Bounded so the chrome render never blocks on a slow XTDB query — a
stream that exceeds this returns nil (degenerate snapshot shape with
`:stream-loaded? nil') instead of hanging the reader.  Lift this
when querying smaller event streams interactively if needed."
  :type 'number
  :group 'arxana-vsatarcs-xtdb-clicks)

(defcustom arxana-vsatarcs-xtdb-clicks-stream-types
  '(("code/v05/watcher-event"    . :watcher-event)
    ("code/v05/heartbeat"        . :heartbeat)
    ("code/v05/file-change"      . :file-change)
    ("code/v05/cross-root-move"  . :cross-root-move)
    ("futon1a/invoke-job"        . :invoke-job)
    ("futon3c/forum-post"        . :forum-post))
  "Alist of (HX-TYPE-STRING . SHORT-NAME-KEYWORD) for click streams.
Each entry names an XTDB hyperedge type and the keyword used in
snapshot output to identify its subclass.  Operators can add new
stream types here without code changes (e.g., M-INC `state/*' when
step (b) lands); the snapshot will query each in parallel and aggregate.

**Wider defaults rationale (v0.5.19):** with parallel querying
(`arxana-vsatarcs-xtdb-clicks-parallel-enabled' = t by default),
chrome render latency is bounded by the SLOWEST stream's timeout
regardless of stream count — adding empty-but-fast types is
operator-meaningfully cheap.  The added defaults
(`code/v05/heartbeat', `code/v05/file-change', `code/v05/cross-root-move',
`futon3c/forum-post') are types that exist in the substrate
schema and will populate as the corresponding subsystems land;
their snapshot rows show `0 records returned' until then,
documenting that the stream is wired and waiting."
  :type '(alist :key-type string :value-type symbol)
  :group 'arxana-vsatarcs-xtdb-clicks)

(defcustom arxana-vsatarcs-xtdb-clicks-parallel-enabled t
  "Non-nil → use parallel async queries; nil → sequential.
Parallel uses `arxana-store--request-async' to fire all stream
queries concurrently, then `accept-process-output' to wait up to
`arxana-vsatarcs-xtdb-clicks-per-stream-timeout' for completion.
Worst-case chrome render latency drops from `n-streams × timeout'
(sequential) to `timeout' (parallel).

Sequential mode is retained for tests (which mock the sync
`arxana-store-fetch-hyperedges' at the per-stream level) and for
operator debugging."
  :type 'boolean
  :group 'arxana-vsatarcs-xtdb-clicks)

(defun arxana-vsatarcs-xtdb-clicks--extract-props (hx)
  "Return the `:hx/props' alist from hyperedge HX, or nil.
Defensive against shape variations (`:hx/props' vs `hx/props' vs
nested under `:envelope').  Caller plist-gets specific fields."
  (or (plist-get hx :hx/props)
      (cdr (assoc 'hx/props hx))
      (cdr (assoc "hx/props" hx))))

(defun arxana-vsatarcs-xtdb-clicks--prop (props key)
  "Return KEY from PROPS (an alist with string OR keyword keys).
Defensive against the JSON-decoder's choice of key kind."
  (or (cdr (assoc key props))
      (cdr (assoc (intern key) props))
      (cdr (assoc (intern (concat ":" key)) props))))

(defun arxana-vsatarcs-xtdb-clicks--summarise-hx (hx subclass)
  "Return an operator-facing plist summary of hyperedge HX.
SUBCLASS is the short-name keyword from `--stream-types'.  Output:
  (:subclass <kw>
   :ts       <epoch-ms or nil>
   :source   <string>     ; e.g. 'heartbeat' or 'file-change'
   :repo     <string>
   :run-id   <string>
   :cycle    <integer or nil>
   :counts   ((:files-seen . N) (:files-changed . N) ...)
   :raw      <the original hyperedge plist; for drill-down>)"
  (let* ((props (arxana-vsatarcs-xtdb-clicks--extract-props hx))
         (ts (arxana-vsatarcs-xtdb-clicks--prop props "ts"))
         (source (arxana-vsatarcs-xtdb-clicks--prop props "source"))
         (repo (arxana-vsatarcs-xtdb-clicks--prop props "repo"))
         (run-id (arxana-vsatarcs-xtdb-clicks--prop props "run-id"))
         (cycle (arxana-vsatarcs-xtdb-clicks--prop props "cycle"))
         (counts (cl-remove-if
                  #'null
                  (mapcar
                   (lambda (k)
                     (let ((v (arxana-vsatarcs-xtdb-clicks--prop props k)))
                       (and v (cons (intern (concat ":" k)) v))))
                   '("files-seen" "files-changed"
                     "n-deleted" "n-renamed"
                     "n-added" "n-cross-root-moves")))))
    (list :subclass subclass
          :ts ts
          :source source
          :repo repo
          :run-id run-id
          :cycle cycle
          :counts counts
          :raw hx)))

(defun arxana-vsatarcs-xtdb-clicks--fetch-stream (hx-type subclass limit)
  "Query XTDB for LIMIT hyperedges of HX-TYPE; return summary list.
Wrapped in `with-timeout' (per
`arxana-vsatarcs-xtdb-clicks-per-stream-timeout') so chrome render
never blocks on a slow XTDB query.  Returns nil on query error
(timeout / server unreachable / type unindexed).  Caller treats nil
as `stream unavailable today', distinct from empty-list `stream
queried but no records'."
  (let* ((resp (ignore-errors
                 (with-timeout (arxana-vsatarcs-xtdb-clicks-per-stream-timeout
                                nil)
                   (arxana-store-fetch-hyperedges :type hx-type :limit limit))))
         (hxs (or (cdr (assoc 'hyperedges resp))
                  (cdr (assoc :hyperedges resp))
                  (plist-get resp :hyperedges)))
         (count (or (cdr (assoc 'count resp))
                    (cdr (assoc :count resp))
                    (plist-get resp :count))))
    (when resp
      (list :stream-loaded? t
            :stream-type hx-type
            :stream-subclass subclass
            :total-in-stream count
            :records (mapcar (lambda (hx)
                               (arxana-vsatarcs-xtdb-clicks--summarise-hx
                                hx subclass))
                             (or hxs nil))))))

(defun arxana-vsatarcs-xtdb-clicks--parse-response (resp hx-type subclass)
  "Build a per-stream summary plist from RESP (or nil for unavailable).
Shared by both sequential and parallel code paths."
  (if (null resp)
      (list :stream-loaded? nil
            :stream-type hx-type
            :stream-subclass subclass
            :total-in-stream nil
            :records nil)
    (let ((hxs (or (cdr (assoc 'hyperedges resp))
                   (cdr (assoc :hyperedges resp))
                   (plist-get resp :hyperedges)))
          (count (or (cdr (assoc 'count resp))
                     (cdr (assoc :count resp))
                     (plist-get resp :count))))
      (list :stream-loaded? t
            :stream-type hx-type
            :stream-subclass subclass
            :total-in-stream count
            :records (mapcar (lambda (hx)
                               (arxana-vsatarcs-xtdb-clicks--summarise-hx
                                hx subclass))
                             (or hxs nil))))))

(defun arxana-vsatarcs-xtdb-clicks--query-string (hx-type limit)
  "Build the query-string fragment for the GET /hyperedges request.
No leading `?' — `arxana-store--build-url' adds it."
  (format "type=%s&limit=%d"
          (url-hexify-string hx-type)
          limit))

(defun arxana-vsatarcs-xtdb-clicks--fetch-streams-parallel (stream-types limit)
  "Fire all STREAM-TYPES concurrently via `arxana-store--request-async';
wait up to `arxana-vsatarcs-xtdb-clicks-per-stream-timeout' for
completion; return per-stream summaries in original alist order.

Streams that don't complete before the deadline return nil (chrome
surfaces as `unavailable').  Worst-case wait is one timeout
regardless of stream count — vs sequential's
`n-streams × timeout' worst case."
  (let* ((n (length stream-types))
         (results (make-vector n :pending))
         (deadline (+ (float-time)
                      arxana-vsatarcs-xtdb-clicks-per-stream-timeout)))
    ;; Fire all queries; each callback stores its response by index.
    (cl-loop for entry in stream-types
             for i from 0
             do (let* ((hx-type (car entry))
                       (query (arxana-vsatarcs-xtdb-clicks--query-string
                               hx-type limit))
                       ;; Capture i in callback closure
                       (idx i))
                  (ignore-errors
                    (arxana-store--request-async
                     "GET" "/hyperedges"
                     (lambda (resp _status)
                       (aset results idx (or resp :error)))
                     nil
                     query))))
    ;; Wait until all entries filled OR deadline.  Process any
    ;; pending URL events in 50ms increments.
    (while (and (cl-some (lambda (r) (eq r :pending)) results)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    ;; Build summaries.  :pending → unavailable; :error → unavailable;
    ;; real response → parsed summary.
    (cl-loop for entry in stream-types
             for i from 0
             collect (let* ((hx-type (car entry))
                            (subclass (cdr entry))
                            (raw (aref results i))
                            (resp (and (not (eq raw :pending))
                                       (not (eq raw :error))
                                       raw)))
                       (arxana-vsatarcs-xtdb-clicks--parse-response
                        resp hx-type subclass)))))

(defun arxana-vsatarcs-xtdb-clicks--fetch-streams-sequential (stream-types limit)
  "Sequential fallback: query each stream in turn via the sync fetcher.
Kept for tests (which mock `arxana-store-fetch-hyperedges' at the
sync entry point) and for operator debugging."
  (mapcar
   (lambda (entry)
     (let* ((hx-type (car entry))
            (subclass (cdr entry))
            (result (arxana-vsatarcs-xtdb-clicks--fetch-stream
                     hx-type subclass limit)))
       (or result
           (list :stream-loaded? nil
                 :stream-type hx-type
                 :stream-subclass subclass
                 :total-in-stream nil
                 :records nil))))
   stream-types))

(defun arxana-vsatarcs-xtdb-clicks-snapshot (&optional limit)
  "Return the XTDB-clicks snapshot across all configured stream types.
LIMIT defaults to `arxana-vsatarcs-xtdb-clicks-default-limit'.  The
snapshot is a plist:

  (:streams           ((:hx-type <s> :subclass <kw> :loaded? <bool>
                        :total-in-stream <n> :records (<summary> ...)) ...)
   :limit             <integer used per stream>
   :parallel?         <t when parallel path taken>
   :digest-line       <one-line operator-facing summary>)

Snapshot shape is stable across (server-up / server-down / type-
slow / type-missing) cases so chrome layout doesn't break.  Uses
the parallel async path by default
(`arxana-vsatarcs-xtdb-clicks-parallel-enabled' = t), falling back
to sequential when that's nil (tests do this so they can mock the
sync fetcher)."
  (let* ((limit (or limit arxana-vsatarcs-xtdb-clicks-default-limit))
         (parallel? arxana-vsatarcs-xtdb-clicks-parallel-enabled)
         (streams (if parallel?
                      (arxana-vsatarcs-xtdb-clicks--fetch-streams-parallel
                       arxana-vsatarcs-xtdb-clicks-stream-types limit)
                    (arxana-vsatarcs-xtdb-clicks--fetch-streams-sequential
                     arxana-vsatarcs-xtdb-clicks-stream-types limit))))
    (list :streams streams
          :limit limit
          :parallel? parallel?
          :digest-line (arxana-vsatarcs-xtdb-clicks--digest streams limit))))

(defun arxana-vsatarcs-xtdb-clicks--digest (streams limit)
  "Render a one-line operator-facing digest from STREAMS + LIMIT."
  (let* ((n-loaded (cl-count-if (lambda (s) (plist-get s :stream-loaded?))
                                streams))
         (total-records (cl-reduce #'+
                                   (mapcar (lambda (s)
                                             (length (or (plist-get s :records)
                                                         nil)))
                                           streams)
                                   :initial-value 0))
         (per-stream
          (mapconcat
           (lambda (s)
             (format "%s=%s"
                     (let ((k (plist-get s :stream-subclass)))
                       (if (keywordp k)
                           (substring (symbol-name k) 1)
                         (format "%s" k)))
                     (if (plist-get s :stream-loaded?)
                         (number-to-string (length (or (plist-get s :records)
                                                       nil)))
                       "unavail")))
           streams " ")))
    (format "%d/%d streams loaded (limit %d); %s; %d total records"
            n-loaded (length streams) limit per-stream total-records)))

(provide 'arxana-vsatarcs-xtdb-clicks)
;;; arxana-vsatarcs-xtdb-clicks.el ends here
