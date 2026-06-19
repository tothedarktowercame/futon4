;;; arxana-vsatarcs-wm-bridge.el --- Cross-side WM belief bridge for VSATARCS R1 -*- lexical-binding: t; -*-

;;; Commentary:
;; Cross-side bridge that lets the VSATARCS reader compare its local
;; belief state against the WM-side belief state (claude-2's
;; `futon2.aif.belief').  Reads the latest EDN-line record from today's
;; WM trace file (`~/code/futon2/data/wm-trace/wm-trace-YYYY-MM-DD.edn'),
;; extracts the `:mu-post' map, normalises it into the local belief
;; format, and runs `arxana-vsatarcs-belief-compare' against the current
;; in-memory state.
;;
;; The cross-side bridge is the operational form of the alignment-drift
;; check the mission's T3 cadence consumes (per
;; `~/code/futon7/holes/M-stack-essay-code-alignment.md' §2.3).
;;
;; Lands as part of M-stack-essay-code-alignment VSATARCS-side v0.2.5,
;; the bilateral counterpart to WM-side v0.9
;; (`hx:wm:v0-9:symmetric-bootstrap-closure', claude-2, 2026-05-18).
;;
;; Caveats (encoded as defaults / explicit filters):
;;
;; - `:mu-post == :mu-pre' today because the WM `judge' cycle does not
;;   yet apply observation-driven updates within one call (R3a gap).
;;   The bridge consuming `:mu-post' is forward-compatible: once R3a
;;   lands and `:mu-post' diverges from `:mu-pre', the comparison
;;   already targets the right field.
;;
;; - The WM-side belief carries a meta-sorry keyword
;;   `:sorry/wm-aif-substrate-addressability' alongside 35 string
;;   section-ids.  The keyword is expected to appear `:only-in-a' from
;;   the WM side of the compare (no corresponding entity exists on the
;;   VSATARCS side).  This is encoded as a known-expected drift filter
;;   (`arxana-vsatarcs-wm-bridge-expected-only-in-wm') so the operator
;;   isn't repeatedly told about an expected delta.

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-belief)
(require 'arxana-browser-rewrites) ; for the EDN reader

(defgroup arxana-vsatarcs-wm-bridge nil
  "Cross-side WM belief bridge for the VSATARCS R1 surface."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-wm-bridge-trace-directory
  (expand-file-name "~/code/futon2/data/wm-trace/")
  "Directory of WM trace files (one EDN-lines file per day).
Each line is a single EDN record holding `:timestamp', `:mu-pre',
`:mu-post', and forward-compatible fields.  The bridge reads the last
line of the file for today's date by default."
  :type 'directory
  :group 'arxana-vsatarcs-wm-bridge)

(defcustom arxana-vsatarcs-wm-bridge-expected-only-in-wm
  '(:sorry/wm-aif-substrate-addressability)
  "Entity ids expected to appear only on the WM side of a cross-side compare.
The WM-side belief tracks the meta-sorry
`:sorry/wm-aif-substrate-addressability' alongside the 35 shared
string section-ids; the meta-sorry has no VSATARCS counterpart by
design (it is the WM's self-referential capability-gap marker).
Drift reports filter these ids out of `:only-in-wm-side' so the
operator is not told about expected deltas every call.

Add additional ids here if the WM-side belief grows further
WM-internal entities in the future."
  :type '(repeat symbol)
  :group 'arxana-vsatarcs-wm-bridge)

(defun arxana-vsatarcs-wm-bridge--today-iso ()
  "Return today's date in YYYY-MM-DD."
  (format-time-string "%Y-%m-%d"))

(defun arxana-vsatarcs-wm-bridge--trace-path-for-date (date)
  "Return the trace file path for DATE (a YYYY-MM-DD string)."
  (expand-file-name (format "wm-trace-%s.edn" date)
                    arxana-vsatarcs-wm-bridge-trace-directory))

(defun arxana-vsatarcs-wm-bridge--read-last-record (path)
  "Read the last EDN form from PATH (a per-day EDN-lines trace file).
PATH is read line-by-line; the last non-blank line is parsed by the
shared EDN reader.  Returns nil if PATH is unreadable or empty."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-max))
      ;; Skip trailing whitespace/blank lines.
      (skip-chars-backward " \t\n\r")
      (let ((line-end (point)))
        (when (> line-end (point-min))
          (beginning-of-line)
          (let ((line (buffer-substring-no-properties (point) line-end)))
            (when (> (length line) 0)
              (with-temp-buffer
                (insert line)
                (goto-char (point-min))
                (arxana-browser-rewrites--edn-read-form)))))))))

(defun arxana-vsatarcs-wm-bridge--read-all-records (path)
  "Read all EDN-lines records from PATH; return a list in file order.
PATH is read line-by-line; each non-blank line is parsed by the shared
EDN reader.  Returns nil if PATH is unreadable; returns the empty list
if PATH is empty.  Consumers (e.g., Q5/Q6 recent-trace + drift modules)
use this to surface the last N records.  Order is file order =
chronological order (WM judge appends; no in-place rewrite)."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (let (records)
        (while (not (eobp))
          (let* ((bol (line-beginning-position))
                 (eol (line-end-position))
                 (line (buffer-substring-no-properties bol eol)))
            (when (> (length (string-trim line)) 0)
              (let ((rec (with-temp-buffer
                           (insert line)
                           (goto-char (point-min))
                           (ignore-errors
                             (arxana-browser-rewrites--edn-read-form)))))
                (when rec (push rec records))))
            (forward-line 1)))
        (nreverse records)))))

(defun arxana-vsatarcs-wm-bridge--strip-leading-colon (sym)
  "Return symbol SYM with any leading `:' stripped from its name.
The shared EDN reader interns keywords as symbols whose name retains
the leading `:' (e.g., `:spawned' is a symbol named \":spawned\").
The local belief format uses bare status names (e.g., the symbol
`spawned'), so keys must be stripped before comparison."
  (let ((s (symbol-name sym)))
    (if (string-prefix-p ":" s)
        (intern (substring s 1))
      sym)))

(defun arxana-vsatarcs-wm-bridge--posterior-from-plist (plist)
  "Convert a WM-side posterior PLIST into the local belief posterior alist.
WM-side posteriors are plists of alternating (status-keyword
probability) pairs; the local format is an alist of (bare-status-symbol
. probability).  Keys are stripped of their leading colon."
  (let (acc)
    (while plist
      (let ((k (car plist))
            (v (cadr plist)))
        (when (symbolp k)
          (push (cons (arxana-vsatarcs-wm-bridge--strip-leading-colon k)
                      (if (numberp v) (float v) v))
                acc))
        (setq plist (cddr plist))))
    (nreverse acc)))

(defun arxana-vsatarcs-wm-bridge--belief-from-mu-post (mu-post)
  "Convert a WM-side `:mu-post' MU-POST plist into a local belief alist.
MU-POST is the plist returned by the shared EDN reader for the
`:mu-post' field of a trace record: alternating (entity-id
status-posterior-plist) pairs.  Returns an alist of (entity-id
. posterior-alist) suitable for direct consumption by
`arxana-vsatarcs-belief-compare'."
  (let (acc)
    (while mu-post
      (let ((id (car mu-post))
            (post-plist (cadr mu-post)))
        (push (cons id (arxana-vsatarcs-wm-bridge--posterior-from-plist
                        post-plist))
              acc))
      (setq mu-post (cddr mu-post)))
    (nreverse acc)))

(defun arxana-vsatarcs-wm-bridge-read-wm-belief (&optional date)
  "Return the WM-side belief alist parsed from DATE's trace file.
DATE defaults to today (YYYY-MM-DD).  Returns nil if the trace file is
unreadable, empty, or carries no `:mu-post' field.  This is the public
entry the cross-side compare consumes."
  (let* ((date (or date (arxana-vsatarcs-wm-bridge--today-iso)))
         (path (arxana-vsatarcs-wm-bridge--trace-path-for-date date))
         (record (arxana-vsatarcs-wm-bridge--read-last-record path))
         (mu-post (and record (plist-get record :mu-post))))
    (when mu-post
      (arxana-vsatarcs-wm-bridge--belief-from-mu-post mu-post))))

(defun arxana-vsatarcs-wm-bridge--filter-expected (only-in-list)
  "Remove `arxana-vsatarcs-wm-bridge-expected-only-in-wm' ids from ONLY-IN-LIST."
  (cl-remove-if (lambda (id)
                  (cl-find id arxana-vsatarcs-wm-bridge-expected-only-in-wm
                           :test #'equal))
                only-in-list))

(defun arxana-vsatarcs-wm-bridge-compare-with-local (&optional epsilon date)
  "Compare today's (or DATE's) WM-side belief against local in-memory belief.
Reads the latest trace record's `:mu-post', converts to local belief
format, and runs `arxana-vsatarcs-belief-compare'.  Filters expected
WM-internal ids (per
`arxana-vsatarcs-wm-bridge-expected-only-in-wm') out of the
`:only-in-wm-side' field of the returned drift report.

Returns a plist:
  (:only-in-local      (<entity-id> ...)
   :only-in-wm-side    (<entity-id> ...)   ; expected ids filtered out
   :expected-in-wm-only (<entity-id> ...)  ; ids matched by the filter
   :posterior-diffs    ((<entity-id> . <max-abs-diff>) ...)
   :equal-count        <integer>
   :trace-date         <YYYY-MM-DD>
   :trace-path         <absolute path read>)

Returns nil with a `user-error' message if no trace file is found
for DATE."
  (interactive)
  (let* ((date (or date (arxana-vsatarcs-wm-bridge--today-iso)))
         (path (arxana-vsatarcs-wm-bridge--trace-path-for-date date))
         (wm (arxana-vsatarcs-wm-bridge-read-wm-belief date))
         (local (arxana-vsatarcs-belief-current)))
    (unless wm
      (user-error "No readable WM trace at %s" path))
    (let* ((raw (arxana-vsatarcs-belief-compare local wm epsilon))
           (only-a (plist-get raw :only-in-a))
           (only-b (plist-get raw :only-in-b))
           (filtered-b (arxana-vsatarcs-wm-bridge--filter-expected only-b))
           (expected-b (cl-remove-if-not
                        (lambda (id)
                          (cl-find id arxana-vsatarcs-wm-bridge-expected-only-in-wm
                                   :test #'equal))
                        only-b)))
      (list :only-in-local      only-a
            :only-in-wm-side    filtered-b
            :expected-in-wm-only expected-b
            :posterior-diffs    (plist-get raw :posterior-diffs)
            :equal-count        (plist-get raw :equal-count)
            :trace-date         date
            :trace-path         path))))

(defun arxana-vsatarcs-wm-bridge-report ()
  "Read WM-side belief, compare to local, message a brief summary.
Convenience for interactive use (`M-x' or bound to a key in the
reader mode-map if desired)."
  (interactive)
  (let ((rep (arxana-vsatarcs-wm-bridge-compare-with-local)))
    (message
     "Cross-side drift @ %s: local-only=%d wm-only=%d (filtered %d expected) diffs=%d equal=%d"
     (plist-get rep :trace-date)
     (length (plist-get rep :only-in-local))
     (length (plist-get rep :only-in-wm-side))
     (length (plist-get rep :expected-in-wm-only))
     (length (plist-get rep :posterior-diffs))
     (plist-get rep :equal-count))))

(provide 'arxana-vsatarcs-wm-bridge)
;;; arxana-vsatarcs-wm-bridge.el ends here
