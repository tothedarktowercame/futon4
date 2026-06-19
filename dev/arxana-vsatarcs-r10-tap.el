;;; arxana-vsatarcs-r10-tap.el --- R10 wakeup tap on WM trace growth -*- lexical-binding: t; -*-

;;; Commentary:
;; Minimum-viable R10 (live operation) for the VSATARCS reader surface:
;; a file-notify subscription on the WM trace directory that fires
;; `arxana-vsatarcs-trace-follow-wm' whenever a WM trace record lands.
;;
;; **History note (v0.5.17 → v0.5.18 revert):** v0.5.17 attempted a
;; multi-dir extension with path-classifiers writing a local
;; click-log EDN file.  Joe's redirect 2026-05-20: that pioneered a
;; substrate where one already existed — multi_watcher
;; (`~/code/futon3/scripts/multi_watcher.clj' + the JVM port)
;; already file-watches the futon repos and emits typed event
;; hyperedges into futon1a XTDB.  v0.5.18 drops the local click-log
;; entirely and reverts this module to its v0.5.2 single-dir shape;
;; the engagement-time surface VSATARCs needed lives in XTDB and is
;; read via the new `arxana-vsatarcs-xtdb-clicks.el' module.
;;
;; Design (v0.5.2, retained 2026-05-20):
;;
;; - **Minimum-viable wakeup signal.**  `file-notify-add-watch' on the
;;   WM trace directory (`~/code/futon2/data/wm-trace/').  When today's
;;   trace file is created or grown, the handler fires
;;   `arxana-vsatarcs-trace-follow-wm', which is already idempotent via
;;   the `:wm-trace-anchor :line-index' lookup against existing VSATARCS
;;   trace records (no separate state file — the trace IS the state
;;   store).
;;
;; - **Honest R10 satisfaction.**  R10's "loop runs on a recurring
;;   schedule without operator intervention" doesn't mandate a calendar
;;   schedule.  Event-driven schedules satisfy it cleanly: operator
;;   opens VSATARCS once; tap is active; trace grows; follow-wm fires;
;;   no per-cycle operator intervention.
;;
;; - **Wider engagement-time surface lives in XTDB.**  For events
;;   beyond WM trace emissions (mission edits, code edits, git
;;   commits, invoke ledger entries, eventual M-INC `state/*`
;;   events), VSATARCs queries futon1a XTDB via
;;   `arxana-vsatarcs-xtdb-clicks-snapshot' — same substrate the
;;   WM-side already consumes.  No separate file, no parallel
;;   file-watch system, no duplicated path-classifier — multi_watcher
;;   does the classification once and XTDB stores the typed events;
;;   both sides read from there.
;;
;; - **No new file-watch system per Joe's directive.**  Emacs's
;;   built-in `file-notify' is the mechanism for the WM-trace tap
;;   specifically (because `follow-wm' needs to run as records land);
;;   broader engagement events go through XTDB.

;;; Code:

(require 'cl-lib)
(require 'filenotify)
(require 'arxana-vsatarcs-trace)
(require 'arxana-vsatarcs-wm-bridge)

(defgroup arxana-vsatarcs-r10-tap nil
  "R10 wakeup tap on WM trace growth for the VSATARCS reader surface."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-r10-tap-trace-directory
  arxana-vsatarcs-wm-bridge-trace-directory
  "Directory containing WM trace files; defaults to the bridge's source.
The tap registers file-notify on this directory and filters callback
events to files matching the `wm-trace-YYYY-MM-DD.edn' pattern."
  :type 'directory
  :group 'arxana-vsatarcs-r10-tap)

(defvar arxana-vsatarcs-r10-tap--descriptor nil
  "Active file-notify descriptor when the tap is running; nil otherwise.
Holds the descriptor returned by `file-notify-add-watch' so
`arxana-vsatarcs-r10-tap-stop' can unregister it.")

(defvar arxana-vsatarcs-r10-tap--last-fire-result nil
  "Result of the most recent handler-triggered `follow-wm' call.
A plist `(:at <iso> :emitted <int> :file <path>)' or nil if the tap
has not fired yet.  Inspectable from `arxana-vsatarcs-r10-tap-status'.")

(defvar arxana-vsatarcs-r10-tap--fire-count 0
  "Number of times the tap handler has fired since it was started.")

;; ---------------------------------------------------------------------
;; Path filter + handler
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-r10-tap--wm-trace-file-p (path)
  "Return non-nil if PATH is a `wm-trace-YYYY-MM-DD.edn' file."
  (and (stringp path)
       (string-match-p "wm-trace-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.edn\\'"
                       path)))

(defun arxana-vsatarcs-r10-tap--date-from-path (path)
  "Extract the YYYY-MM-DD date string from a WM trace PATH, or nil."
  (when (and (stringp path)
             (string-match "wm-trace-\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)\\.edn\\'"
                           path))
    (match-string 1 path)))

(defun arxana-vsatarcs-r10-tap-handler (event)
  "file-notify callback: fire `follow-wm' when a WM trace file changes.
EVENT is the standard file-notify event tuple: (descriptor action file
&optional file1).  ACTIONS we care about: `created', `changed'.  Other
actions (deleted, attribute-changed, etc.) are ignored.  The
file-name is filtered to wm-trace EDN files via
`arxana-vsatarcs-r10-tap--wm-trace-file-p'; non-matching events return
without firing."
  (let* ((action (nth 1 event))
         (file (nth 2 event)))
    (when (and (memq action '(created changed))
               (arxana-vsatarcs-r10-tap--wm-trace-file-p file))
      (let* ((date (arxana-vsatarcs-r10-tap--date-from-path file))
             (emitted (and date (arxana-vsatarcs-trace-follow-wm date))))
        (cl-incf arxana-vsatarcs-r10-tap--fire-count)
        (setq arxana-vsatarcs-r10-tap--last-fire-result
              (list :at (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t)
                    :emitted (or emitted 0)
                    :file file
                    :date date))))))

;; ---------------------------------------------------------------------
;; Public entry points
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-r10-tap-active-p ()
  "Return non-nil if the R10 tap is currently active."
  (and arxana-vsatarcs-r10-tap--descriptor t))

(defun arxana-vsatarcs-r10-tap-start ()
  "Register file-notify on the WM trace directory; idempotent.
Returns the active descriptor.  Calling when already active is a
no-op (returns the existing descriptor).  The watch fires
`arxana-vsatarcs-r10-tap-handler' on any `created' / `changed' event
under the trace directory; the handler filters to wm-trace EDN files
and calls `arxana-vsatarcs-trace-follow-wm' with the date parsed from
the file name."
  (interactive)
  (if arxana-vsatarcs-r10-tap--descriptor
      arxana-vsatarcs-r10-tap--descriptor
    (let ((dir (expand-file-name arxana-vsatarcs-r10-tap-trace-directory)))
      (unless (file-directory-p dir)
        (make-directory dir t))
      (setq arxana-vsatarcs-r10-tap--descriptor
            (file-notify-add-watch dir
                                   '(change)
                                   #'arxana-vsatarcs-r10-tap-handler))
      (when (called-interactively-p 'interactive)
        (message "VSATARCS R10 tap: active on %s" dir))
      arxana-vsatarcs-r10-tap--descriptor)))

(defun arxana-vsatarcs-r10-tap-stop ()
  "Unregister the R10 file-notify watch; idempotent."
  (interactive)
  (when arxana-vsatarcs-r10-tap--descriptor
    (file-notify-rm-watch arxana-vsatarcs-r10-tap--descriptor)
    (setq arxana-vsatarcs-r10-tap--descriptor nil)
    (when (called-interactively-p 'interactive)
      (message "VSATARCS R10 tap: stopped"))))

(defun arxana-vsatarcs-r10-tap-status ()
  "Report the R10 tap's current state interactively."
  (interactive)
  (if (arxana-vsatarcs-r10-tap-active-p)
      (message "R10 tap active on %s; fired %d time(s); last: %S"
               arxana-vsatarcs-r10-tap-trace-directory
               arxana-vsatarcs-r10-tap--fire-count
               arxana-vsatarcs-r10-tap--last-fire-result)
    (message "R10 tap not active")))

(provide 'arxana-vsatarcs-r10-tap)
;;; arxana-vsatarcs-r10-tap.el ends here
