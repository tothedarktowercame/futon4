;;; arxana-browser-pattern-activation.el --- Pattern activation browser  -*- lexical-binding: t; -*-

;; Arxana Browser (※) → Patterns → Activation.
;;
;; Aggregates `event "context-retrieval"' evidence by pattern id.
;; List view: rank, id, title, count, last fired, avg score.
;; Drill-in: per-pattern table of every retrieval (at, session, score,
;; query snippet) so Joe / Codex / any agent can sense-check WHY a
;; pattern was activated rather than only seeing it on a leaderboard.
;;
;; Data path:
;;   - Currently: aggregates client-side from
;;     GET /api/alpha/evidence?since=…&limit=… on futon1a.
;;   - Ships in source: GET /api/alpha/patterns/activation aggregates
;;     server-side; activates on next futon1a JVM restart. The list /
;;     drill-in renderers don't care which path is live; they consume
;;     the same shape produced by `arxana-pattern-activation--snapshot'.
;;
;; Cross-ref: futon3/holes/missions/M-pattern-mining.md (P-5a).

(require 'json)
(require 'subr-x)
(require 'cl-lib)

(defgroup arxana-pattern-activation nil
  "Pattern activation browser inside Arxana Browser."
  :group 'arxana-browser)

(defcustom arxana-pattern-activation-base-url
  (or (getenv "FUTON1A_URL") "http://localhost:7071")
  "Base URL of the futon1a evidence API."
  :type 'string
  :group 'arxana-pattern-activation)

(defcustom arxana-pattern-activation-window-minutes 60
  "Default rolling window for the activation view."
  :type 'integer
  :group 'arxana-pattern-activation)

(defcustom arxana-pattern-activation-fetch-limit 500
  "Cap on /api/alpha/evidence?limit when fetching for the activation view."
  :type 'integer
  :group 'arxana-pattern-activation)

(defcustom arxana-pattern-activation-cache-seconds 30
  "Seconds to reuse a cached snapshot before re-fetching."
  :type 'integer
  :group 'arxana-pattern-activation)

(defvar arxana-pattern-activation--cache nil
  "Cons of (UNIX-TIME . SNAPSHOT-PLIST) or nil.")

;; --- Fetch + aggregate ----------------------------------------------------

(defun arxana-pattern-activation--iso-since (minutes)
  (format-time-string "%FT%TZ" (time-subtract (current-time)
                                              (seconds-to-time (* 60 minutes)))
                      t))

(defun arxana-pattern-activation--fetch-evidence (since-iso)
  "Fetch evidence entries since SINCE-ISO. Returns plist list, or nil on error."
  (let* ((url (format "%s/api/alpha/evidence?limit=%d&since=%s"
                      (string-remove-suffix "/" arxana-pattern-activation-base-url)
                      arxana-pattern-activation-fetch-limit
                      (url-hexify-string since-iso)))
         (url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (buf (ignore-errors (url-retrieve-synchronously url t t 10))))
    (when buf
      (with-current-buffer buf
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((data (condition-case _err
                         (let ((json-object-type 'plist)
                               (json-array-type 'list)
                               (json-key-type 'keyword)
                               (json-false nil))
                           (json-read))
                       (error nil)))
              (entries (plist-get data :entries)))
          (kill-buffer buf)
          entries)))))

(defun arxana-pattern-activation--aggregate (entries)
  "Aggregate context-retrieval ENTRIES by pattern id.
Returns a list of plists sorted by activation count desc:
  (:id ID :title TITLE :count N :avg-score S :last-fired ISO :activations [...])."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (e entries)
      (let* ((body (plist-get e :evidence/body))
             (event (and body (plist-get body :event)))
             (results (and body (plist-get body :results)))
             (qtext (and body (or (plist-get body :query) "")))
             (at (plist-get e :evidence/at))
             (session (plist-get e :evidence/session-id))
             (eid (plist-get e :evidence/id))
             (author (plist-get e :evidence/author)))
        (when (and (equal event "context-retrieval") results)
          (dolist (r results)
            (let* ((pid (plist-get r :id))
                   (title (or (plist-get r :title) ""))
                   (score (or (plist-get r :score) 0))
                   (act (list :at at :session-id session :score score
                              :query qtext :evidence-id eid :agent-id author))
                   (cur (gethash pid tbl
                                 (list :id pid :title title :count 0
                                       :scores nil :ats nil :acts nil))))
              (puthash pid
                       (list :id pid
                             :title (if (string-empty-p (plist-get cur :title))
                                        title (plist-get cur :title))
                             :count (1+ (plist-get cur :count))
                             :scores (cons score (plist-get cur :scores))
                             :ats (cons at (plist-get cur :ats))
                             :acts (cons act (plist-get cur :acts)))
                       tbl))))))
    (let (rows)
      (maphash (lambda (_pid v)
                 (let* ((scores (plist-get v :scores))
                        (avg (when scores
                               (/ (apply #'+ (mapcar #'float scores))
                                  (length scores))))
                        (last (car (sort (copy-sequence (plist-get v :ats))
                                         #'string>)))
                        (acts (sort (copy-sequence (plist-get v :acts))
                                    (lambda (a b)
                                      (string> (plist-get a :at)
                                               (plist-get b :at))))))
                   (push (list :id (plist-get v :id)
                               :title (plist-get v :title)
                               :count (plist-get v :count)
                               :avg-score avg
                               :last-fired last
                               :activations acts)
                         rows)))
               tbl)
      (sort rows (lambda (a b) (> (plist-get a :count) (plist-get b :count)))))))

(defun arxana-pattern-activation--snapshot (&optional force)
  "Return the current activation snapshot, using the cache when fresh.
With FORCE non-nil, bypass the cache."
  (let ((now (float-time)))
    (when (or force
              (null arxana-pattern-activation--cache)
              (> (- now (car arxana-pattern-activation--cache))
                 arxana-pattern-activation-cache-seconds))
      (let* ((since (arxana-pattern-activation--iso-since
                     arxana-pattern-activation-window-minutes))
             (entries (arxana-pattern-activation--fetch-evidence since))
             (patterns (when entries (arxana-pattern-activation--aggregate entries))))
        (setq arxana-pattern-activation--cache
              (cons now (list :since since
                              :total-entries (length entries)
                              :patterns (or patterns '()))))))
    (cdr arxana-pattern-activation--cache)))

(defun arxana-pattern-activation-clear-cache ()
  "Force a fresh fetch on the next render."
  (interactive)
  (setq arxana-pattern-activation--cache nil)
  (message "[arxana-pattern-activation] cache cleared"))

;; --- List view (:view 'pattern-activation) -------------------------------

(defun arxana-browser--pattern-activation-format ()
  "Tabulated list format for the activation list view."
  [("#"      4 t)
   ("ID"    32 t)
   ("Title" 38 t)
   ("Hits"   5 (lambda (a b) (< (string-to-number (aref (cadr a) 3))
                                 (string-to-number (aref (cadr b) 3)))))
   ("Avg"    6 t)
   ("Last"  19 t)])

(defvar arxana-browser--pattern-activation-rank-counter 0)

(defun arxana-browser--pattern-activation-row (item)
  "Tabulated row for one pattern ITEM."
  (cl-incf arxana-browser--pattern-activation-rank-counter)
  (vector (number-to-string arxana-browser--pattern-activation-rank-counter)
          (or (plist-get item :id) "")
          (let ((title (or (plist-get item :title) "")))
            (if (> (length title) 36) (concat (substring title 0 35) "…") title))
          (number-to-string (or (plist-get item :count) 0))
          (let ((s (plist-get item :avg-score)))
            (if (numberp s) (format "%.2f" s) "?"))
          (let ((last (or (plist-get item :last-fired) "")))
            (if (>= (length last) 19) (substring last 0 19) last))))

(defun arxana-browser--pattern-activation-items ()
  "Return items for the activation list view."
  (setq arxana-browser--pattern-activation-rank-counter 0)
  (let* ((snap (arxana-pattern-activation--snapshot))
         (patterns (and snap (plist-get snap :patterns))))
    (cond
     ((null snap)
      (list (list :type 'info
                  :label "Activation snapshot unavailable"
                  :description (format "Could not reach %s/api/alpha/evidence"
                                       arxana-pattern-activation-base-url))))
     ((null patterns)
      (list (list :type 'info
                  :label "No activations in window"
                  :description (format "No context-retrieval events in last %d min"
                                       arxana-pattern-activation-window-minutes))))
     (t
      (mapcar (lambda (p) (append (list :type 'pattern-activation) p)) patterns)))))

(defun arxana-browser-pattern-activation-headline ()
  "Return a short summary for the activation list view's header-line."
  (let* ((snap (arxana-pattern-activation--snapshot))
         (patterns (and snap (plist-get snap :patterns)))
         (total-hits (apply #'+ (mapcar (lambda (p) (or (plist-get p :count) 0))
                                        patterns)))
         (top (car patterns)))
    (cond
     ((null patterns)
      (format "Pattern Activation — last %dm: no events"
              arxana-pattern-activation-window-minutes))
     (t
      (format "Pattern Activation — last %dm: %d hits across %d patterns%s"
              arxana-pattern-activation-window-minutes
              total-hits
              (length patterns)
              (if top
                  (format ", top %s (%d×)"
                          (plist-get top :id) (plist-get top :count))
                ""))))))

;; --- Drill-in (:view 'pattern-activation-detail) -------------------------

(defun arxana-browser--pattern-activation-current-pattern ()
  "Return the plist of the pattern currently being drilled into.
Reads from the top of `arxana-browser--stack' so the value survives
the major-mode reset that `arxana-browser--render' triggers (which is
why we cannot use a `defvar-local' for this)."
  (when (boundp 'arxana-browser--stack)
    (plist-get (car arxana-browser--stack) :pattern)))

(defun arxana-browser--pattern-activation-detail-format ()
  "Column format for the per-pattern detail view."
  [("At"      19 t)
   ("Score"    6 t)
   ("Agent"   12 t)
   ("Session" 10 t)
   ("Query"    0 nil)])

(defun arxana-browser--pattern-activation-detail-row (item)
  "Row for one activation ITEM."
  (let* ((at (or (plist-get item :at) ""))
         (score (plist-get item :score))
         (agent (or (plist-get item :agent-id) ""))
         (session (or (plist-get item :session-id) ""))
         (query (or (plist-get item :query) "")))
    (vector (if (>= (length at) 19) (substring at 0 19) at)
            (if (numberp score) (format "%.2f" score) "?")
            (if (> (length agent) 12) (substring agent 0 12) agent)
            (if (>= (length session) 8) (substring session 0 8) session)
            (if (> (length query) 200) (concat (substring query 0 199) "…") query))))

(defun arxana-browser--pattern-activation-detail-items ()
  "Return rows for the detail view of the current pattern."
  (let* ((p (arxana-browser--pattern-activation-current-pattern))
         (acts (and p (plist-get p :activations))))
    (if acts
        (mapcar (lambda (a) (append (list :type 'pattern-activation-row) a)) acts)
      (list (list :type 'info
                  :label "No activations"
                  :description "This pattern has no recorded retrievals in the window.")))))

(defun arxana-browser-pattern-activation-detail-headline ()
  "Header-line for the per-pattern detail view."
  (let* ((p (arxana-browser--pattern-activation-current-pattern))
         (id (or (plist-get p :id) "?"))
         (title (or (plist-get p :title) ""))
         (count (or (plist-get p :count) 0))
         (avg (plist-get p :avg-score)))
    (format "%s — %s · %d activations · avg score %s"
            id title count (if (numberp avg) (format "%.2f" avg) "?"))))

;; --- Public entry points -------------------------------------------------

(defun arxana-browser-pattern-activation ()
  "Open the Pattern Activation list view directly.
Reachable from Arxana Browser (※) → Pattern Activation. This is the
fast-path command; it opens the browser if needed and pushes the
activation view onto the navigation stack."
  (interactive)
  (when (fboundp 'arxana-browser)
    (arxana-browser))
  (when (and (boundp 'arxana-browser--stack)
             (fboundp 'arxana-browser--render))
    (setq arxana-browser--stack
          (cons (list :type 'menu
                      :label "Pattern Activation"
                      :description "Patterns activated by recent agent turns."
                      :view 'pattern-activation)
                arxana-browser--stack))
    (arxana-browser--render)))

(provide 'arxana-browser-pattern-activation)
;;; arxana-browser-pattern-activation.el ends here
