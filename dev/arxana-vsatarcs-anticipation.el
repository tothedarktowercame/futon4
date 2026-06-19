;;; arxana-vsatarcs-anticipation.el --- Anticipation block for VSATARCS reader chrome -*- lexical-binding: t; -*-

;;; Commentary:
;; Reader-criteria V-COV / V-CUR closure (Q3 of
;; `~/code/futon2/docs/vsatarcs-reader-criteria.md').  Reads the canonical
;; forward-axis substrate `~/code/calendar/events.edn' and exposes a
;; snapshot of upcoming events the reader can surface alongside the
;; belief snapshot.
;;
;; The shape is operator-facing: each upcoming event carries its id,
;; firing prior, days-until, basin/strawman/mission anchors, and a brief
;; rationale string lifted from `:event/p-fires-rationale'.  Per-event
;; time-pressure follows the WM-side `anticipation/time-pressure'
;; convention: a linear ramp from 0 at horizon-days to 1 at the firing
;; instant, weighted by `:event/p-fires'.  The aggregate horizon
;; time-pressure is the max over events in the horizon.
;;
;; The module does not maintain mutable state — every call re-reads the
;; canonical EDN and recomputes.  This is consistent with the
;; "events.edn is the source of truth" discipline (see
;; `~/code/futon7/holes/M-interim-director-proxy-metric-inventory.md'
;; §2.A.2.38) and avoids drift between the renderer and the substrate.
;;
;; Contract: contributes to V-COV (coverage — reader can see anticipated
;; events at all) and V-CUR (currency — reader sees the latest event
;; priors as of the most recent revision).  Cross-maps to WM-side
;; `futon2.aif.anticipation' which consumes the same substrate.

;;; Code:

(require 'cl-lib)
(require 'parse-time)
(require 'arxana-browser-rewrites) ; shared EDN reader

(defgroup arxana-vsatarcs-anticipation nil
  "Anticipation snapshot for the VSATARCS reader surface."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-anticipation-events-file
  (expand-file-name "~/code/calendar/events.edn")
  "Path to the canonical forward-axis events substrate.
This file is read on every snapshot call; no in-memory cache is held."
  :type 'file
  :group 'arxana-vsatarcs-anticipation)

(defcustom arxana-vsatarcs-anticipation-horizon-days 30
  "Number of days forward to include in the anticipation horizon.
Events past this horizon are dropped from snapshots and from
time-pressure computation."
  :type 'integer
  :group 'arxana-vsatarcs-anticipation)

(defun arxana-vsatarcs-anticipation--now-seconds (&optional now)
  "Return the wall-clock time in seconds-since-epoch, defaulting to NOW or current.
NOW may be an Emacs time value or nil."
  (float-time (or now (current-time))))

(defun arxana-vsatarcs-anticipation--inst-to-seconds (inst)
  "Convert an `:event/at' INST value to seconds-since-epoch.
The shared EDN reader returns `#inst \"...\"' literals as plain strings
because the tag is discarded; this helper parses the ISO-8601 string
into seconds.  Returns nil when INST is not parseable."
  (when (stringp inst)
    (condition-case nil
        (float-time (encode-time (parse-time-string inst)))
      (error nil))))

(defun arxana-vsatarcs-anticipation--days-until (event-at &optional now)
  "Return days from NOW until EVENT-AT (an `:event/at' string).
Negative when EVENT-AT is in the past; nil when EVENT-AT is unparseable."
  (let ((then (arxana-vsatarcs-anticipation--inst-to-seconds event-at)))
    (when then
      (/ (- then (arxana-vsatarcs-anticipation--now-seconds now))
         86400.0))))

(defun arxana-vsatarcs-anticipation--event-plist-get (event key)
  "Return KEY from EVENT (a plist returned by the shared EDN reader).
The shared reader interns keyword keys as symbols whose name retains
the leading `:'; this helper accepts the bare keyword KEY (e.g.
`:event/id') and looks up the matching symbol."
  (let ((sym (intern (symbol-name key))))
    (plist-get event sym)))

(defun arxana-vsatarcs-anticipation--load-events ()
  "Return the raw `:events' vector from the events file, or nil.
Returns nil silently when the file is unreadable; callers should
treat nil as \"no anticipation block to render\" rather than an error."
  (when (file-readable-p arxana-vsatarcs-anticipation-events-file)
    (let* ((data (arxana-browser-rewrites--read-edn-file
                  arxana-vsatarcs-anticipation-events-file))
           (events (plist-get data (intern ":events"))))
      (and events (append events nil)))))

(defun arxana-vsatarcs-anticipation--summarise-event (event &optional now)
  "Return an operator-facing plist summary of EVENT.
EVENT is a plist as returned by the shared EDN reader.  The output
plist carries `:id', `:kind', `:at', `:days-until', `:p-fires',
`:basin', `:strawman', `:mission', `:lifecycle-status', `:next-gate',
`:rationale', `:time-pressure'.  `:rationale' is the leading clause
of `:event/p-fires-rationale' (first sentence, trimmed); the full
rationale lives in the source EDN."
  (let* ((at (arxana-vsatarcs-anticipation--event-plist-get event :event/at))
         (days (arxana-vsatarcs-anticipation--days-until at now))
         (p-fires (arxana-vsatarcs-anticipation--event-plist-get event :event/p-fires))
         (rationale (arxana-vsatarcs-anticipation--event-plist-get
                     event :event/p-fires-rationale))
         (rationale-head (and (stringp rationale)
                              (let ((m (string-match "[.;\n]" rationale)))
                                (string-trim
                                 (if m (substring rationale 0 m) rationale))))))
    (list :id            (arxana-vsatarcs-anticipation--event-plist-get
                          event :event/id)
          :kind          (arxana-vsatarcs-anticipation--event-plist-get
                          event :event/kind)
          :at            at
          :days-until    days
          :p-fires       p-fires
          :basin         (arxana-vsatarcs-anticipation--event-plist-get
                          event :event/basin)
          :strawman      (arxana-vsatarcs-anticipation--event-plist-get
                          event :event/strawman)
          :mission       (arxana-vsatarcs-anticipation--event-plist-get
                          event :event/mission)
          :lifecycle-status (arxana-vsatarcs-anticipation--event-plist-get
                             event :event/lifecycle-status)
          :next-gate     (arxana-vsatarcs-anticipation--event-plist-get
                          event :event/next-gate)
          :rationale     rationale-head
          :time-pressure (arxana-vsatarcs-anticipation--event-time-pressure
                          days p-fires))))

(defun arxana-vsatarcs-anticipation--event-time-pressure (days-until p-fires)
  "Compute per-event time-pressure from DAYS-UNTIL and P-FIRES.
Linear ramp from 0 at horizon-days to 1 at the firing instant; clamped
to [0, 1]; weighted by P-FIRES.  Returns 0.0 for events past horizon
or already past; returns 0.0 when arguments are nil/non-numeric.
Mirrors the WM-side `anticipation/time-pressure' convention so the
two sides agree on the metric.

Implementation note: the WM-side reference (`futon2/src/futon2/aif/
anticipation.clj') uses days >= 0 as the in-horizon predicate; this
port preserves that — past events (negative days) yield 0.0."
  (cond
   ((not (and (numberp days-until) (numberp p-fires))) 0.0)
   ((< days-until 0.0) 0.0)
   ((> days-until arxana-vsatarcs-anticipation-horizon-days) 0.0)
   (t (let ((ramp (- 1.0 (/ days-until
                            (float arxana-vsatarcs-anticipation-horizon-days)))))
        (* p-fires (max 0.0 (min 1.0 ramp)))))))

(defun arxana-vsatarcs-anticipation-snapshot (&optional now)
  "Return the anticipation snapshot for the current horizon.
NOW defaults to the current wall-clock time; supplying NOW (an Emacs
time value) lets callers compute snapshots at a fixed reference time
(useful for testing).  The snapshot is a plist:

  (:horizon-days       <integer>
   :now-iso            <YYYY-MM-DD>
   :events-loaded?     <t or nil>
   :events             ((<event-summary-plist> ...) sorted by :days-until)
   :horizon-time-pressure <max time-pressure across in-horizon events>)

Events with `:days-until' negative or beyond the horizon are dropped.
Returns the same shape whether or not the file exists (with
`:events-loaded?' nil)."
  (let* ((raw (arxana-vsatarcs-anticipation--load-events))
         (now-secs (arxana-vsatarcs-anticipation--now-seconds now))
         (now-iso (format-time-string "%Y-%m-%d" (seconds-to-time now-secs)))
         (summaries (mapcar (lambda (e)
                              (arxana-vsatarcs-anticipation--summarise-event e now))
                            (or raw nil)))
         (in-horizon (cl-remove-if-not
                      (lambda (s)
                        (let ((d (plist-get s :days-until)))
                          (and (numberp d)
                               (>= d 0.0)
                               (<= d arxana-vsatarcs-anticipation-horizon-days))))
                      summaries))
         (sorted (sort in-horizon
                       (lambda (a b)
                         (< (plist-get a :days-until)
                            (plist-get b :days-until)))))
         (tp-max (if sorted
                     (apply #'max (mapcar (lambda (s)
                                            (or (plist-get s :time-pressure) 0.0))
                                          sorted))
                   0.0)))
    (list :horizon-days arxana-vsatarcs-anticipation-horizon-days
          :now-iso now-iso
          :events-loaded? (not (null raw))
          :events sorted
          :horizon-time-pressure tp-max)))

(defun arxana-vsatarcs-anticipation-closest-event (&optional now)
  "Return the closest in-horizon event summary, or nil when none.
Convenience wrapper over `arxana-vsatarcs-anticipation-snapshot' for
the reader chrome's single-line headline projection."
  (car (plist-get (arxana-vsatarcs-anticipation-snapshot now) :events)))

(provide 'arxana-vsatarcs-anticipation)
;;; arxana-vsatarcs-anticipation.el ends here
