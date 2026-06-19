;;; arxana-browser-evidence.el --- Evidence browser views -*- lexical-binding: t; -*-

;;; Commentary:
;; Evidence landscape browsing for Arxana.
;; Provides timeline, session grouping, and thread-chain projections
;; backed by GET /api/alpha/evidence endpoints.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)

(defvar arxana-browser--stack)
(defvar arxana-browser--buffer)
(defvar arxana-browser--context)

(declare-function arxana-browser--render "arxana-browser-core")
(declare-function arxana-browser--current-row "arxana-browser-core")
(declare-function arxana-browser--goto-row "arxana-browser-core" (row))

(defgroup arxana-evidence nil
  "Evidence landscape browsing for Arxana."
  :group 'arxana)

(defun arxana-evidence--default-server ()
  "Return the canonical base URL for the evidence landscape API.

Evidence browsing is anchored on the persisted Futon1/Futon1a store, not the
local agency transport endpoint.  Falling back to the agent-chat base URL makes
Arxana show only the local session stream instead of the shared evidence
landscape."
  (or (getenv "FUTON1_API_BASE")
      (getenv "STACK_HUD_FUTON1_API_BASE")
      (and (boundp 'arxana-lab-futon1-server)
           (stringp arxana-lab-futon1-server)
           (not (string-empty-p arxana-lab-futon1-server))
           arxana-lab-futon1-server)
      "http://localhost:7071/api/alpha"))

(defcustom arxana-evidence-server
  (arxana-evidence--default-server)
  "HTTP base URL for the evidence API."
  :type 'string
  :group 'arxana-evidence)

(defcustom arxana-evidence-request-timeout 10
  "Timeout in seconds for evidence API requests."
  :type 'integer
  :group 'arxana-evidence)

(defcustom arxana-evidence-timeline-limit 200
  "Default number of entries to fetch for evidence timeline views."
  :type 'integer
  :group 'arxana-evidence)

(defcustom arxana-evidence-thread-limit 400
  "Number of entries used to build thread projections."
  :type 'integer
  :group 'arxana-evidence)

(defcustom arxana-evidence-open-session-entry-limit 24
  "Maximum Evidence entries sampled for each open REPL session summary."
  :type 'integer
  :group 'arxana-evidence)

(defcustom arxana-evidence-open-session-llm-summaries t
  "When non-nil, summarize open-session Evidence capsules with Claude."
  :type 'boolean
  :group 'arxana-evidence)

(defcustom arxana-evidence-open-session-summary-command
  (or (executable-find "claude")
      (and (file-executable-p "/home/joe/.local/bin/claude")
           "/home/joe/.local/bin/claude")
      "claude")
  "Command used for cheap open-session summaries."
  :type 'string
  :group 'arxana-evidence)

(defcustom arxana-evidence-open-session-summary-model "haiku"
  "Claude model alias used for cheap open-session summaries."
  :type 'string
  :group 'arxana-evidence)

(defcustom arxana-evidence-open-session-summary-timeout 45
  "Maximum seconds for a batched open-session summary call."
  :type 'integer
  :group 'arxana-evidence)

(defcustom arxana-evidence-open-session-summary-max-budget "0.05"
  "Maximum USD budget passed to the Claude summary call."
  :type 'string
  :group 'arxana-evidence)

(defcustom arxana-evidence-open-session-log-refresh t
  "When non-nil, log timing metrics for open-session refreshes."
  :type 'boolean
  :group 'arxana-evidence)

(defvar arxana-evidence--open-session-summary-cache
  (make-hash-table :test 'equal)
  "Cache of open-session LLM summaries keyed by session/latest/model.")

(defvar arxana-evidence--open-session-item-cache
  (make-hash-table :test 'equal)
  "Cache of open-session rows keyed by session/latest/model.")

(defvar arxana-evidence--open-session-summary-pending
  (make-hash-table :test 'equal)
  "Queued open-session summary requests keyed by summary cache key.")

(defvar arxana-evidence--open-session-summary-inflight
  (make-hash-table :test 'equal)
  "Open-session summary requests currently being processed.")

(defvar arxana-evidence--open-session-summary-process nil
  "Active asynchronous process for open-session summaries, if any.")

(defvar arxana-evidence--open-session-summary-timeout-timer nil
  "Timeout timer for the active asynchronous open-session summary process.")

(defvar arxana-evidence--open-session-marks
  (make-hash-table :test 'equal)
  "Marked open REPL sessions keyed by buffer name.")

(defconst arxana-evidence--open-session-item-cache-version "row-v3"
  "Version tag for open-session row cache entries.")

(defvar arxana-evidence--open-session-refresh-stats nil
  "Dynamic plist of timing metrics for the current open-session refresh.")

(defun arxana-evidence--open-session-stats-add (key value)
  "Add numeric VALUE to metric KEY in current open-session stats."
  (when arxana-evidence--open-session-refresh-stats
    (let ((current (or (plist-get arxana-evidence--open-session-refresh-stats key)
                       0)))
      (setq arxana-evidence--open-session-refresh-stats
            (plist-put arxana-evidence--open-session-refresh-stats
                       key
                       (+ current value))))))

(defun arxana-evidence--open-session-stats-inc (key)
  "Increment metric KEY in current open-session stats."
  (arxana-evidence--open-session-stats-add key 1))

(defun arxana-evidence--log-open-session-refresh (fmt &rest args)
  "Log open-session refresh message FMT with ARGS when logging is enabled."
  (when arxana-evidence-open-session-log-refresh
    (message "%s" (apply #'format (concat "arxana sessions: " fmt) args))))

(defun arxana-evidence--normalize-base (base)
  (let ((value (string-remove-suffix "/" (or base ""))))
    (setq value (replace-regexp-in-string
                 "/api\\(?:/alpha\\|/%ce%b1\\|/%CE%B1\\)?/evidence\\'" "" value))
    (setq value (replace-regexp-in-string "/api\\(?:/alpha\\|/%ce%b1\\|/%CE%B1\\)?\\'" "" value))
    value))

(defun arxana-evidence--query-string (params)
  (let ((pairs
         (delq nil
               (mapcar (lambda (kv)
                         (let ((k (car kv))
                               (v (cdr kv)))
                           (when (and k v (not (string-empty-p (format "%s" v))))
                             (format "%s=%s"
                                     (url-hexify-string (format "%s" k))
                                     (url-hexify-string (format "%s" v))))))
                       params))))
    (when pairs
      (string-join pairs "&"))))

(defun arxana-evidence--endpoint-url (path &optional query)
  (concat (arxana-evidence--normalize-base arxana-evidence-server)
          "/api/alpha"
          path
          (if (and query (not (string-empty-p query)))
              (concat "?" query)
            "")))

(defun arxana-evidence--parse-json (body)
  (when (and body (stringp body) (not (string-empty-p body)))
    (condition-case nil
        (if (fboundp 'json-parse-string)
            (json-parse-string body
                               :object-type 'plist
                               :array-type 'list
                               :null-object nil
                               :false-object nil)
          (let ((json-object-type 'plist)
                (json-array-type 'list)
                (json-false nil)
                (json-null nil))
            (json-read-from-string body)))
      (error nil))))

(defun arxana-evidence--request (path &optional query)
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (url (arxana-evidence--endpoint-url path query))
         (buffer (url-retrieve-synchronously url t t arxana-evidence-request-timeout)))
    (unless buffer
      (user-error "Evidence request failed: %s" url))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (let ((status (and (boundp 'url-http-response-status)
                             url-http-response-status)))
            (unless (and (integerp status) (> status 0))
              (user-error "Evidence request failed with no HTTP response: %s" url))
            (re-search-forward "\n\n" nil 'move)
            (let* ((body (buffer-substring-no-properties (point) (point-max)))
                   (parsed (arxana-evidence--parse-json body)))
              (if (and (>= status 200) (< status 300))
                  parsed
                (user-error "Evidence API error %s (%s)" status url)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun arxana-evidence--kw-name (value)
  (cond
   ((keywordp value) (substring (symbol-name value) 1))
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" (or value "")))))

(defun arxana-evidence--truncate (text max-len)
  (let ((value (format "%s" (or text ""))))
    (if (> (length value) max-len)
        (concat (substring value 0 (max 0 (- max-len 3))) "...")
      value)))

(defun arxana-evidence--entry-id (entry)
  (or (plist-get entry :evidence/id) ""))

(defun arxana-evidence--entry-time (entry)
  (or (plist-get entry :evidence/at) ""))

(defun arxana-evidence--entry-subject (entry)
  (let* ((subject (plist-get entry :evidence/subject))
         (stype (or (plist-get subject :ref/type) ""))
         (sid (or (plist-get subject :ref/id) "")))
    (if (or (string-empty-p (format "%s" stype))
            (string-empty-p (format "%s" sid)))
        "-"
      (format "%s/%s"
              (arxana-evidence--kw-name stype)
              (arxana-evidence--truncate sid 20)))))

(defun arxana-evidence--entry-summary (entry)
  (let ((body (plist-get entry :evidence/body)))
    (cond
     ((stringp body) (arxana-evidence--truncate body 48))
     ((listp body) (arxana-evidence--truncate (prin1-to-string body) 48))
     (t "-"))))

(defun arxana-evidence--entry-body-text (entry)
  "Return the plain text payload for Evidence ENTRY, or an empty string."
  (let ((body (plist-get entry :evidence/body)))
    (cond
     ((stringp body) body)
     ((listp body)
      (or (plist-get body :text)
          (plist-get body :message)
          (plist-get body :summary)
          ""))
     (t ""))))

(defun arxana-evidence--entry-role (entry)
  "Return a normalized chat role for Evidence ENTRY."
  (let* ((body (plist-get entry :evidence/body))
         (role (and (listp body) (plist-get body :role)))
         (author (downcase (or (plist-get entry :evidence/author) "")))
         (claim (arxana-evidence--kw-name (plist-get entry :evidence/claim-type))))
    (cond
     ((and (stringp role) (not (string-empty-p role))) (downcase role))
     ((member author '("joe" "user")) "user")
     ((member author '("codex" "claude" "assistant")) "assistant")
     ((string= claim "question") "user")
     ((member claim '("observation" "correction" "conjecture")) "assistant")
     (t ""))))

(defun arxana-evidence--semantic-snippet (text max-len)
  "Return a compact semantic snippet from TEXT bounded by MAX-LEN."
  (let* ((raw (arxana-evidence--extract-user-message (or text "")))
         (single (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim raw))))
    (arxana-evidence--truncate single max-len)))

(defun arxana-evidence--extract-missions (texts)
  "Return sorted distinct mission ids mentioned in TEXTS."
  (let ((missions nil))
    (dolist (text texts)
      (when (stringp text)
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (while (re-search-forward "\\bM-[[:alnum:]][[:alnum:]_-]*\\b" nil t)
            (cl-pushnew (match-string 0) missions :test #'equal)))))
    (sort missions #'string<)))

(defun arxana-evidence--extract-artifacts (texts)
  "Return a compact list of file/path-like artifacts mentioned in TEXTS."
  (let ((artifacts nil))
    (dolist (text texts)
      (when (stringp text)
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (while (re-search-forward
                  "\\(?:[[:alnum:]_.-]+/\\)+[[:alnum:]_.-]+\\|[[:alnum:]_-]+\\.[[:alpha:]][[:alnum:]]+"
                  nil t)
            (cl-pushnew (string-trim-right (match-string 0) "[.,;:]+")
                        artifacts
                        :test #'equal)))))
    (cl-subseq (sort artifacts #'string<) 0 (min 4 (length artifacts)))))

(defun arxana-evidence--entry->item (entry &optional depth)
  (list :type 'evidence-entry
        :entry entry
        :evidence-id (arxana-evidence--entry-id entry)
        :timestamp (arxana-evidence--entry-time entry)
        :etype (arxana-evidence--kw-name (plist-get entry :evidence/type))
        :claim (arxana-evidence--kw-name (plist-get entry :evidence/claim-type))
        :author (or (plist-get entry :evidence/author) "")
        :session (or (plist-get entry :evidence/session-id) "-")
        :subject (arxana-evidence--entry-subject entry)
        :summary (arxana-evidence--entry-summary entry)
        :depth depth))

(defun arxana-evidence--fetch-evidence (&optional params)
  (let* ((query (arxana-evidence--query-string params))
         (payload (arxana-evidence--request "/evidence" query)))
    (or (plist-get payload :entries) '())))

(defun arxana-browser-evidence-menu-items ()
  (list (list :type 'menu
              :label "Evidence Timeline"
              :description "Chronological stream of evidence entries."
              :view 'evidence-timeline)
        (list :type 'menu
              :label "Evidence by Session"
              :description "Group evidence entries by session-id."
              :view 'evidence-sessions)
        (list :type 'menu
              :label "Open REPL Sessions"
              :description "Evidence-backed semantic summaries for open Codex/Claude buffers."
              :view 'evidence-open-sessions)
        (list :type 'menu
              :label "Evidence Threads"
              :description "Group reply chains into thread projections."
              :view 'evidence-threads)))

(defun arxana-browser--evidence-timeline-format ()
  [("At" 20 t)
   ("Type" 18 t)
   ("Claim" 12 t)
   ("Author" 16 t)
   ("Session" 20 t)
   ("Subject" 28 t)
   ("ID" 36 t)])

(defun arxana-browser--evidence-timeline-row (item)
  (vector (arxana-evidence--truncate (or (plist-get item :timestamp) "") 19)
          (arxana-evidence--truncate (or (plist-get item :etype) "") 17)
          (arxana-evidence--truncate (or (plist-get item :claim) "") 11)
          (arxana-evidence--truncate (or (plist-get item :author) "") 15)
          (arxana-evidence--truncate (or (plist-get item :session) "") 19)
          (arxana-evidence--truncate (or (plist-get item :subject) "") 27)
          (arxana-evidence--truncate (or (plist-get item :evidence-id) "") 35)))

(defun arxana-browser--evidence-chain-format ()
  [("Depth" 6 t)
   ("At" 20 t)
   ("Type" 18 t)
   ("Claim" 12 t)
   ("Author" 16 t)
   ("Subject" 30 t)
   ("ID" 36 t)])

(defun arxana-browser--evidence-chain-row (item)
  (vector (format "%s" (or (plist-get item :depth) "-"))
          (arxana-evidence--truncate (or (plist-get item :timestamp) "") 19)
          (arxana-evidence--truncate (or (plist-get item :etype) "") 17)
          (arxana-evidence--truncate (or (plist-get item :claim) "") 11)
          (arxana-evidence--truncate (or (plist-get item :author) "") 15)
          (arxana-evidence--truncate (or (plist-get item :subject) "") 29)
          (arxana-evidence--truncate (or (plist-get item :evidence-id) "") 35)))

(defun arxana-browser--evidence-thread-reader-format ()
  [("Speaker" 24 t)
   ("At" 20 t)
   ("Text" 0 nil)])

(defun arxana-browser--evidence-thread-reader-row (item)
  (vector (arxana-evidence--truncate (or (plist-get item :speaker) "") 23)
          (arxana-evidence--truncate (or (plist-get item :timestamp) "") 19)
          (or (plist-get item :text) "")))

(defun arxana-evidence--thread-turn-item (entry depth)
  (let* ((body (plist-get entry :evidence/body))
         (body-role (and (listp body) (plist-get body :role)))
         (body-event (and (listp body) (plist-get body :event)))
         (body-text (and (listp body) (plist-get body :text)))
         (author (or (plist-get entry :evidence/author) ""))
         (claim (arxana-evidence--kw-name (plist-get entry :evidence/claim-type)))
         (role (cond
                ((and (stringp body-role) (not (string-empty-p body-role))) body-role)
                ((string= claim "question") "user")
                (t "")))
         (speaker (cond
                   ((and (not (string-empty-p role))
                         (not (string-empty-p author)))
                    (format "%s (%s)" role author))
                   ((not (string-empty-p author)) author)
                   ((not (string-empty-p role)) role)
                   (t "-")))
         (base-text (cond
                     ((and (stringp body-text) (not (string-empty-p body-text))) body-text)
                     ((stringp body) body)
                     ((and (stringp body-event) (not (string-empty-p body-event)))
                      (format "[%s] %s" body-event (arxana-evidence--entry-summary entry)))
                     (t (arxana-evidence--entry-summary entry)))))
    (list :type 'evidence-turn
          :entry entry
          :evidence-id (arxana-evidence--entry-id entry)
          :speaker speaker
          :timestamp (arxana-evidence--entry-time entry)
          :text base-text
          :depth depth)))

(defun arxana-evidence--chat-like-entry-p (entry)
  (let* ((body (plist-get entry :evidence/body))
         (event (and (listp body) (plist-get body :event)))
         (claim (arxana-evidence--kw-name (plist-get entry :evidence/claim-type))))
    (or (and (stringp event) (string= event "chat-turn"))
        (member claim '("question" "observation" "correction" "conjecture")))))

(defun arxana-evidence--stringify (value &optional max-len)
  (let ((text (cond
               ((null value) "")
               ((stringp value) value)
               (t (prin1-to-string value)))))
    (if (and (integerp max-len) (> max-len 0))
        (arxana-evidence--truncate text max-len)
      text)))

(defun arxana-browser--evidence-entry-detail-format ()
  [("Field" 18 t)
   ("Value" 0 nil)])

(defun arxana-browser--evidence-entry-detail-row (item)
  (vector (or (plist-get item :field) "")
          (or (plist-get item :value) "")))

(defun arxana-browser--evidence-entry-detail-items (context)
  (let* ((evidence-id (plist-get context :evidence-id))
         (fallback-entry (plist-get context :entry)))
    (condition-case err
        (let* ((entry (or (and (stringp evidence-id)
                               (not (string-empty-p evidence-id))
                               (plist-get
                                (arxana-evidence--request
                                 (format "/evidence/%s" (url-hexify-string evidence-id))
                                 nil)
                                :entry))
                          fallback-entry))
               (subject (or (and (listp entry) (plist-get entry :evidence/subject)) '()))
               (tags (or (and (listp entry) (plist-get entry :evidence/tags)) '()))
               (body (and (listp entry) (plist-get entry :evidence/body))))
          (if (not (listp entry))
              (list (list :type 'info
                          :label "Entry not found"
                          :description (format "No evidence entry found for %s."
                                               (or evidence-id "?"))))
            (list
             (list :type 'evidence-detail :field "ID"
                   :value (arxana-evidence--stringify (plist-get entry :evidence/id)))
             (list :type 'evidence-detail :field "At"
                   :value (arxana-evidence--stringify (plist-get entry :evidence/at)))
             (list :type 'evidence-detail :field "Type"
                   :value (arxana-evidence--kw-name (plist-get entry :evidence/type)))
             (list :type 'evidence-detail :field "Claim"
                   :value (arxana-evidence--kw-name (plist-get entry :evidence/claim-type)))
             (list :type 'evidence-detail :field "Author"
                   :value (arxana-evidence--stringify (plist-get entry :evidence/author)))
             (list :type 'evidence-detail :field "Session"
                   :value (arxana-evidence--stringify (plist-get entry :evidence/session-id)))
             (list :type 'evidence-detail :field "Subject"
                   :value (if (listp subject)
                              (format "%s/%s"
                                      (arxana-evidence--kw-name (plist-get subject :ref/type))
                                      (arxana-evidence--stringify (plist-get subject :ref/id)))
                            "-"))
             (list :type 'evidence-detail :field "In Reply To"
                   :value (arxana-evidence--stringify (plist-get entry :evidence/in-reply-to)))
             (list :type 'evidence-detail :field "Fork Of"
                   :value (arxana-evidence--stringify (plist-get entry :evidence/fork-of)))
             (list :type 'evidence-detail :field "Tags"
                   :value (if tags
                              (mapconcat #'arxana-evidence--kw-name tags ", ")
                            ""))
             (list :type 'evidence-detail :field "Body"
                   :value (arxana-evidence--stringify body 400)))))
      (error
       (list (list :type 'info
                   :label "Entry unavailable"
                   :description (format "Error: %s" (error-message-string err))))))))

(defun arxana-browser--evidence-timeline-items ()
  (condition-case err
      (let ((entries (arxana-evidence--fetch-evidence
                      `(("limit" . ,arxana-evidence-timeline-limit)))))
        (if entries
            (mapcar #'arxana-evidence--entry->item entries)
          (list (list :type 'info
                      :label "No evidence entries"
                      :description "Run futon3c workflows to populate evidence."))))
    (error
     (list (list :type 'info
                 :label "Evidence unavailable"
                 :description (format "Error: %s" (error-message-string err)))))))

(defun arxana-evidence--session-buckets (entries)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let* ((session-id (or (plist-get entry :evidence/session-id) "(none)"))
             (bucket (gethash session-id table)))
        (puthash session-id (cons entry bucket) table)))
    (let (rows)
      (maphash
       (lambda (session-id session-entries)
         (let* ((count (length session-entries))
                (latest (car (sort (copy-sequence session-entries)
                                   (lambda (a b)
                                     (string> (arxana-evidence--entry-time a)
                                              (arxana-evidence--entry-time b))))))
                (types (delete-dups (mapcar (lambda (e) (arxana-evidence--kw-name (plist-get e :evidence/type)))
                                            session-entries)))
                (authors (delete-dups (mapcar (lambda (e) (or (plist-get e :evidence/author) ""))
                                              session-entries))))
           (push (list :type 'evidence-session
                       :session-id session-id
                       :label session-id
                       :count count
                       :latest (arxana-evidence--entry-time latest)
                       :types (string-join (cl-subseq types 0 (min 3 (length types))) ", ")
                       :authors (string-join (cl-subseq authors 0 (min 3 (length authors))) ", ")
                       :entries (sort (copy-sequence session-entries)
                                      (lambda (a b)
                                        (string> (arxana-evidence--entry-time a)
                                                 (arxana-evidence--entry-time b)))))
                 rows)))
       table)
      (sort rows (lambda (a b)
                   (string> (or (plist-get a :latest) "")
                            (or (plist-get b :latest) "")))))))

(defun arxana-browser--evidence-sessions-format ()
  [("Session" 30 t)
   ("Entries" 8 nil)
   ("Latest" 20 t)
   ("Types" 24 nil)
   ("Authors" 24 nil)])

(defun arxana-browser--evidence-sessions-row (item)
  (vector (arxana-evidence--truncate (or (plist-get item :session-id) "") 29)
          (format "%d" (or (plist-get item :count) 0))
          (arxana-evidence--truncate (or (plist-get item :latest) "") 19)
          (arxana-evidence--truncate (or (plist-get item :types) "") 23)
          (arxana-evidence--truncate (or (plist-get item :authors) "") 23)))

(defun arxana-browser--evidence-sessions-items ()
  (condition-case err
      (let ((entries (arxana-evidence--fetch-evidence
                      `(("limit" . ,arxana-evidence-thread-limit)))))
        (if entries
            (arxana-evidence--session-buckets entries)
          (list (list :type 'info
                      :label "No sessions"
                      :description "No evidence entries with session context."))))
    (error
     (list (list :type 'info
                 :label "Evidence unavailable"
                 :description (format "Error: %s" (error-message-string err)))))))

(defun arxana-evidence--repl-buffer-p (buffer)
  "Return non-nil when BUFFER appears to be an open Codex/Claude REPL."
  (with-current-buffer buffer
    (or (memq major-mode '(codex-repl-mode claude-repl-mode))
        (string-match-p "\\`\\*\\(?:codex\\|claude\\)-repl\\(?::[^*]+\\)?\\*\\'"
                        (buffer-name buffer)))))

(defun arxana-evidence--buffer-session-id ()
  "Return the current buffer's REPL session id, or nil."
  (let* ((usable-p (lambda (value)
                     (and (stringp value)
                          (not (string-empty-p value))
                          (not (string= value "pending")))))
         (sid (or (and (boundp 'agent-chat--session-id)
                       (local-variable-p 'agent-chat--session-id
                                         (current-buffer))
                       (funcall usable-p agent-chat--session-id)
                       agent-chat--session-id)
                  (and (memq major-mode '(codex-repl-mode codex-repl-mirror-mode))
                       (local-variable-p 'codex-repl--evidence-session-id
                                         (current-buffer))
                       (boundp 'codex-repl--evidence-session-id)
                       (funcall usable-p codex-repl--evidence-session-id)
                       codex-repl--evidence-session-id)
                  ;; `codex-repl--last-emitted-session-id` is a fallback:
                  ;; it tracks the last session for which a session-start evidence
                  ;; event was emitted. It can lag behind the current session, but
                  ;; it is still a better Evidence-oriented fallback than
                  ;; `codex-repl-session-id`, which may reflect an older local
                  ;; buffer/session-file value.
                  (and (memq major-mode '(codex-repl-mode codex-repl-mirror-mode))
                       (local-variable-p 'codex-repl--last-emitted-session-id
                                         (current-buffer))
                       (boundp 'codex-repl--last-emitted-session-id)
                       (funcall usable-p codex-repl--last-emitted-session-id)
                       codex-repl--last-emitted-session-id)
                  (and (memq major-mode '(codex-repl-mode codex-repl-mirror-mode))
                       (local-variable-p 'codex-repl-session-id (current-buffer))
                       (boundp 'codex-repl-session-id)
                       (funcall usable-p codex-repl-session-id)
                       codex-repl-session-id))))
    (and sid
         (not (string-empty-p sid))
         (not (string= sid "pending"))
         sid)))

(defun arxana-evidence--buffer-agent ()
  "Return the current buffer's REPL agent label."
  (or (and (boundp 'agent-chat--agent-name)
           (stringp agent-chat--agent-name)
           (not (string-empty-p agent-chat--agent-name))
           agent-chat--agent-name)
      (cond
       ((string-match-p "\\`\\*codex-repl" (buffer-name)) "codex")
       ((string-match-p "\\`\\*claude-repl" (buffer-name)) "claude")
       (t "agent"))))

(defun arxana-evidence--buffer-evidence-server ()
  "Return the current REPL buffer's Evidence server base, or nil."
  (let ((url (cond
              ((and (boundp 'agent-chat--evidence-url)
                    (stringp agent-chat--evidence-url)
                    (not (string-empty-p agent-chat--evidence-url)))
               agent-chat--evidence-url)
              ((and (boundp 'codex-repl-evidence-url)
                    (stringp codex-repl-evidence-url)
                    (not (string-empty-p codex-repl-evidence-url)))
               codex-repl-evidence-url)
              ((and (boundp 'claude-repl-evidence-url)
                    (stringp claude-repl-evidence-url)
                    (not (string-empty-p claude-repl-evidence-url)))
               claude-repl-evidence-url)
	       (t nil))))
    (and url (arxana-evidence--normalize-base url))))

(defun arxana-evidence--buffer-latest-evidence-id ()
  "Return the current REPL buffer's latest Evidence id hint, or nil."
  (let ((eid (cond
              ((and (boundp 'codex-repl--last-evidence-id)
                    (local-variable-p 'codex-repl--last-evidence-id)
                    (stringp codex-repl--last-evidence-id))
               codex-repl--last-evidence-id)
              ((and (boundp 'claude-repl--last-evidence-id)
                    (local-variable-p 'claude-repl--last-evidence-id)
                    (stringp claude-repl--last-evidence-id))
               claude-repl--last-evidence-id)
              ((and (boundp 'agent-chat--last-evidence-id)
                    (local-variable-p 'agent-chat--last-evidence-id)
                    (stringp agent-chat--last-evidence-id))
               agent-chat--last-evidence-id)
              (t nil))))
    (and eid (not (string-empty-p eid)) eid)))

(defun arxana-evidence--open-repl-sessions ()
  "Return open Codex/Claude REPL buffers with their session ids.
Signals a user error if an open REPL buffer lacks a usable session id."
  (let (sessions)
    (dolist (buffer (buffer-list))
      (when (arxana-evidence--repl-buffer-p buffer)
        (with-current-buffer buffer
          (let* ((sid (arxana-evidence--buffer-session-id))
                 (evidence-server (arxana-evidence--buffer-evidence-server))
                 (state (if (and (boundp 'agent-chat--pending-process)
                                 (process-live-p agent-chat--pending-process))
                            "running"
                          "idle")))
            (cond
             ((not sid)
              (when (string= state "running")
                (user-error "Open REPL buffer has no Evidence session id: %s"
                            (buffer-name buffer))))
             (t
              (unless evidence-server
                (user-error "Open REPL buffer has no Evidence endpoint: %s"
                            (buffer-name buffer)))
              (push (list :buffer (buffer-name buffer)
                          :agent (arxana-evidence--buffer-agent)
                          :session-id sid
                          :evidence-server evidence-server
                          :latest-id-hint (arxana-evidence--buffer-latest-evidence-id)
                          :state state)
                    sessions)))))))
    (sort sessions (lambda (a b)
                     (string< (plist-get a :buffer)
                              (plist-get b :buffer))))))

(defun arxana-evidence--entries-for-open-session (session)
  "Fetch bounded Evidence entries for open REPL SESSION."
  (let* ((sid (plist-get session :session-id))
         (server (plist-get session :evidence-server))
         (start (float-time))
         (entries (unwind-protect
                      (let ((arxana-evidence-server server))
                        (arxana-evidence--fetch-evidence
                         `(("session-id" . ,sid)
                           ("limit" . ,arxana-evidence-open-session-entry-limit))))
                    (arxana-evidence--open-session-stats-inc :evidence-fetches)
                    (arxana-evidence--open-session-stats-add
                     :evidence-fetch-seconds
                     (- (float-time) start)))))
    entries))

(defun arxana-evidence--count-open-session-turns (session)
  "Return the exact Evidence turn count for open REPL SESSION."
  (let* ((sid (plist-get session :session-id))
         (server (plist-get session :evidence-server))
         (start (float-time))
         (payload
          (unwind-protect
              (let ((arxana-evidence-server server))
                (arxana-evidence--request
                 "/evidence/count"
                 (arxana-evidence--query-string
                  `(("session-id" . ,sid)
                    ("tag" . "turn")))))
            (arxana-evidence--open-session-stats-inc :turn-count-fetches)
            (arxana-evidence--open-session-stats-add
             :turn-count-seconds
             (- (float-time) start))))
         (count (plist-get payload :count)))
    (unless (integerp count)
      (user-error "Evidence count unavailable for open REPL session %s (%s)"
                  sid (plist-get session :buffer)))
    count))

(defun arxana-evidence--count-session-entries (session-id)
  "Return the exact Evidence entry count for SESSION-ID, or nil on failure."
  (when (and (stringp session-id)
             (not (string-empty-p session-id))
             (not (string= session-id "(none)")))
    (let* ((payload (arxana-evidence--request
                     "/evidence/count"
                     (arxana-evidence--query-string
                      `(("session-id" . ,session-id)))))
           (count (plist-get payload :count)))
      (when (integerp count)
        count))))

(defun arxana-evidence--open-session-summary (session entries turn-count)
  "Return a semantic summary item for open REPL SESSION using ENTRIES."
  (let* ((sorted (sort (copy-sequence entries)
                       (lambda (a b)
                         (string> (arxana-evidence--entry-time a)
                                  (arxana-evidence--entry-time b)))))
         (texts (mapcar #'arxana-evidence--entry-body-text sorted))
         (latest (car sorted))
         (latest-user
          (cl-find-if (lambda (entry)
                        (string= (arxana-evidence--entry-role entry) "user"))
                      sorted))
         (latest-assistant
          (cl-find-if (lambda (entry)
                        (string= (arxana-evidence--entry-role entry) "assistant"))
                      sorted))
         (about (if latest-user
                    (arxana-evidence--semantic-snippet
                     (arxana-evidence--entry-body-text latest-user) 120)
                  (arxana-evidence--entry-summary latest)))
         (outcome (and latest-assistant
                       (arxana-evidence--semantic-snippet
                        (arxana-evidence--entry-body-text latest-assistant) 90)))
         (missions (arxana-evidence--extract-missions texts))
         (artifacts (arxana-evidence--extract-artifacts texts)))
    (append (list :type 'evidence-open-session)
            session
            (list :count turn-count
                  :latest (arxana-evidence--entry-time latest)
                  :latest-id (arxana-evidence--entry-id latest)
                  :about about
                  :outcome (or outcome "")
                  :missions missions
                  :artifacts artifacts
                  :entries sorted))))

(defun arxana-evidence--open-session-empty-item (session)
  "Return a placeholder row for open REPL SESSION with no Evidence yet."
  (append (list :type 'evidence-open-session)
          session
          (list :count 0
                :latest ""
                :latest-id nil
                :about "No Evidence yet"
                :outcome ""
                :missions nil
                :artifacts nil
                :entries nil
                :summary-ready t)))

(defun arxana-evidence--open-session-cache-key (item)
  "Return the LLM-summary cache key for open-session ITEM."
  (format "%s|%s|%s"
          (or (plist-get item :session-id) "")
          (or (plist-get item :latest-id) "")
          arxana-evidence-open-session-summary-model))

(defun arxana-evidence--open-session-item-cache-key (item)
  "Return the row-cache key for open-session ITEM."
  (format "%s|%s"
          arxana-evidence--open-session-item-cache-version
          (arxana-evidence--open-session-cache-key item)))

(defun arxana-evidence--open-session-hint-cache-key (session)
  "Return row-cache key for open SESSION when it has a latest-id hint."
  (when-let ((eid (plist-get session :latest-id-hint)))
    (format "%s|%s|%s|%s"
            arxana-evidence--open-session-item-cache-version
            (or (plist-get session :session-id) "")
            eid
            arxana-evidence-open-session-summary-model)))

(defun arxana-evidence--open-session-refresh-cached-item (session item)
  "Return cached ITEM with volatile fields refreshed from SESSION."
  (let ((copy (copy-sequence item)))
    (dolist (key '(:buffer :agent :evidence-server :state :latest-id-hint))
      (setq copy (plist-put copy key (plist-get session key))))
    copy))

(defun arxana-evidence--cache-open-session-item (item)
  "Store open-session ITEM under its fetched and buffer-local latest ids."
  (puthash (arxana-evidence--open-session-item-cache-key item)
           item
           arxana-evidence--open-session-item-cache)
  (when-let ((hint-key (arxana-evidence--open-session-hint-cache-key item)))
    (puthash hint-key item arxana-evidence--open-session-item-cache)))

(defun arxana-evidence--open-session-item (session)
  "Return cached or freshly fetched summary item for open SESSION."
  (let* ((hint-key (arxana-evidence--open-session-hint-cache-key session))
         (cached (and hint-key
                      (gethash hint-key arxana-evidence--open-session-item-cache))))
    (if cached
        (progn
          (arxana-evidence--open-session-stats-inc :row-cache-hits)
          (arxana-evidence--open-session-refresh-cached-item session cached))
      (arxana-evidence--open-session-stats-inc :row-cache-misses)
      (let ((entries (arxana-evidence--entries-for-open-session session)))
        (if entries
            (let* ((turn-count (arxana-evidence--count-open-session-turns session))
                   (item (arxana-evidence--open-session-summary
                          session entries turn-count)))
              (arxana-evidence--cache-open-session-item item)
              item)
          (arxana-evidence--open-session-empty-item session))))))

(defun arxana-evidence--open-session-capsule (item)
  "Return a bounded text capsule for LLM summary of open-session ITEM."
  (let* ((entries (cl-subseq (or (plist-get item :entries) '())
                             0 (min 4 (length (or (plist-get item :entries) '())))))
         (turns
          (mapconcat
           (lambda (entry)
             (format "- %s %s: %s"
                     (arxana-evidence--truncate
                      (arxana-evidence--entry-time entry) 19)
                     (or (arxana-evidence--entry-role entry) "")
                     (arxana-evidence--semantic-snippet
                      (arxana-evidence--entry-body-text entry) 140)))
           entries
           "\n")))
    (format "BUFFER: %s\nAGENT: %s\nSTATE: %s\nMISSIONS: %s\nARTIFACTS: %s\nEXTRACTIVE ABOUT: %s\nLATEST OUTCOME: %s\nRECENT EVIDENCE:\n%s"
            (or (plist-get item :buffer) "")
            (or (plist-get item :agent) "")
            (or (plist-get item :state) "")
            (string-join (or (plist-get item :missions) '()) ", ")
            (string-join (or (plist-get item :artifacts) '()) ", ")
            (or (plist-get item :about) "")
            (or (plist-get item :outcome) "")
            turns)))

(defun arxana-evidence--clean-llm-summary (text)
  "Normalize one-line LLM summary TEXT."
  (let* ((single (replace-regexp-in-string "[ \t\n\r]+" " " (string-trim (or text ""))))
         (unquoted (replace-regexp-in-string "\\`[\"']\\|[\"']\\'" "" single)))
    (arxana-evidence--truncate unquoted 110)))

(defun arxana-evidence--parse-open-session-summary-lines ()
  "Parse current buffer as summary TSV output and return (ID . SUMMARY) rows."
  (let (rows)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (string-trim
                   (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))))
        (when (string-match "\\`\\(S[0-9]+\\)[\t ]+\\(.+\\)\\'" line)
          (push (cons (match-string 1 line)
                      (arxana-evidence--clean-llm-summary
                       (match-string 2 line)))
                rows)))
      (forward-line 1))
    (nreverse rows)))

(defun arxana-evidence--batch-open-session-summaries (requests)
  "Return alist of (ID . SUMMARY) for batched summary REQUESTS.
Each request is (ID . CAPSULE)."
  (when (and requests
             arxana-evidence-open-session-llm-summaries
             (executable-find arxana-evidence-open-session-summary-command))
    (let* ((system
            "You summarize coding/research sessions. Return exactly one TSV line per requested ID: ID<TAB>summary. Summaries must be plain, concrete, and at most 12 words. No bullets, no preamble.")
           (prompt
            (concat
             "Summarize each session capsule.\n\n"
             (mapconcat
              (lambda (request)
                (format "ID: %s\n%s"
                        (car request)
                        (cdr request)))
              requests
              "\n\n---\n\n")))
           (out (generate-new-buffer " *arxana-session-summary*"))
           (status nil))
      (unwind-protect
          (let (rows)
            (with-temp-buffer
              (insert prompt)
              (setq status
                    (with-timeout (arxana-evidence-open-session-summary-timeout :timeout)
                      (call-process-region
                       (point-min) (point-max)
                       arxana-evidence-open-session-summary-command
                       nil out nil
                       "-p"
                       "--model" arxana-evidence-open-session-summary-model
                       "--no-session-persistence"
                       "--max-budget-usd" arxana-evidence-open-session-summary-max-budget
                       "--tools" ""
                       "--system-prompt" system))))
            (when (eq status 0)
              (with-current-buffer out
                (setq rows (arxana-evidence--parse-open-session-summary-lines))))
            rows)
        (when (buffer-live-p out)
          (kill-buffer out))))))

(defun arxana-evidence--maybe-rerender-open-sessions ()
  "Refresh the browser buffer when it is showing open-session rows."
  (when-let ((buffer (and (boundp 'arxana-browser--buffer)
                          (get-buffer arxana-browser--buffer))))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'arxana-browser-mode)
                 (boundp 'arxana-browser--context)
                 (eq (plist-get arxana-browser--context :view)
                     'evidence-open-sessions))
        (arxana-browser--render)))))

(defun arxana-evidence--dequeue-open-session-summary-requests ()
  "Return queued summary requests as ((ID . CAPSULE) ...), or nil if none.
Also marks the corresponding cache keys as in-flight."
  (let ((index 0)
        requests
        id->key)
    (maphash
     (lambda (key capsule)
       (remhash key arxana-evidence--open-session-summary-pending)
       (puthash key t arxana-evidence--open-session-summary-inflight)
       (cl-incf index)
       (let ((id (format "S%d" index)))
         (push (cons id capsule) requests)
         (push (cons id key) id->key)))
     arxana-evidence--open-session-summary-pending)
    (when requests
      (list :requests (nreverse requests)
            :id->key (nreverse id->key)))))

(defun arxana-evidence--finish-open-session-summary-process (proc)
  "Consume completed asynchronous summary PROC and update caches."
  (when (eq proc arxana-evidence--open-session-summary-process)
    (setq arxana-evidence--open-session-summary-process nil))
  (when (timerp arxana-evidence--open-session-summary-timeout-timer)
    (cancel-timer arxana-evidence--open-session-summary-timeout-timer))
  (setq arxana-evidence--open-session-summary-timeout-timer nil)
  (let* ((out (process-buffer proc))
         (id->key (process-get proc :id->key))
         (start (or (process-get proc :started-at) (float-time)))
         (elapsed (- (float-time) start))
         (status (process-exit-status proc))
         rows)
    (unwind-protect
        (progn
          (dolist (pair id->key)
            (remhash (cdr pair) arxana-evidence--open-session-summary-inflight))
          (arxana-evidence--open-session-stats-inc :llm-calls)
          (arxana-evidence--open-session-stats-add :llm-seconds elapsed)
          (cond
           ((eq status 0)
            (when (buffer-live-p out)
              (with-current-buffer out
                (setq rows (arxana-evidence--parse-open-session-summary-lines))))
            (dolist (row rows)
              (when-let* ((key (cdr (assoc (car row) id->key)))
                          (summary (cdr row)))
                (puthash key summary arxana-evidence--open-session-summary-cache)))
            (when rows
              (arxana-evidence--maybe-rerender-open-sessions)))
           (t
            (message "arxana evidence session summaries failed (exit %s)"
                     status))))
      (when (buffer-live-p out)
        (kill-buffer out))))
  (arxana-evidence--start-open-session-summary-worker))

(defun arxana-evidence--open-session-summary-timeout (proc)
  "Abort asynchronous summary PROC when it exceeds the configured timeout."
  (when (process-live-p proc)
    (delete-process proc)
    (message "arxana evidence session summaries timed out after %ss"
             arxana-evidence-open-session-summary-timeout)))

(defun arxana-evidence--start-open-session-summary-worker ()
  "Start an asynchronous worker for any queued open-session summaries."
  (when (and (not (process-live-p arxana-evidence--open-session-summary-process))
             arxana-evidence-open-session-llm-summaries
             (executable-find arxana-evidence-open-session-summary-command))
    (when-let* ((batch (arxana-evidence--dequeue-open-session-summary-requests))
                (requests (plist-get batch :requests))
                (id->key (plist-get batch :id->key)))
      (let* ((system
              "You summarize coding/research sessions. Return exactly one TSV line per requested ID: ID<TAB>summary. Summaries must be plain, concrete, and at most 12 words. No bullets, no preamble.")
             (prompt
              (concat
               "Summarize each session capsule.\n\n"
               (mapconcat
                (lambda (request)
                  (format "ID: %s\n%s"
                          (car request)
                          (cdr request)))
                requests
                "\n\n---\n\n")))
             (out (generate-new-buffer " *arxana-session-summary*"))
             (proc
              (make-process
               :name "arxana-session-summary"
               :buffer out
               :command (list arxana-evidence-open-session-summary-command
                              "-p"
                              "--model" arxana-evidence-open-session-summary-model
                              "--no-session-persistence"
                              "--max-budget-usd" arxana-evidence-open-session-summary-max-budget
                              "--tools" ""
                              "--system-prompt" system)
               :connection-type 'pipe
               :noquery t
               :sentinel
               (lambda (process _event)
                 (when (memq (process-status process) '(exit signal))
                   (arxana-evidence--finish-open-session-summary-process process))))))
        (setq arxana-evidence--open-session-summary-process proc)
        (process-put proc :id->key id->key)
        (process-put proc :started-at (float-time))
        (setq arxana-evidence--open-session-summary-timeout-timer
              (run-at-time
               arxana-evidence-open-session-summary-timeout
               nil
               #'arxana-evidence--open-session-summary-timeout
               proc))
        (process-send-string proc prompt)
        (process-send-eof proc)))))

(defun arxana-evidence--queue-open-session-summary-requests (items)
  "Queue any missing summary requests for open-session ITEMS."
  (let ((queued 0))
    (dolist (item items)
      (when (eq (plist-get item :type) 'evidence-open-session)
        (unless (plist-get item :summary-ready)
          (let* ((key (arxana-evidence--open-session-cache-key item))
                 (cached (gethash key arxana-evidence--open-session-summary-cache))
                 (pending (gethash key arxana-evidence--open-session-summary-pending))
                 (inflight (gethash key arxana-evidence--open-session-summary-inflight)))
            (unless (or cached pending inflight)
              (puthash key
                       (arxana-evidence--open-session-capsule item)
                       arxana-evidence--open-session-summary-pending)
              (cl-incf queued))))))
    (when (> queued 0)
      (arxana-evidence--open-session-stats-add :llm-requested-sessions queued)
      (arxana-evidence--log-open-session-refresh
       "queued %d session summary request%s with %s"
       queued
       (if (= queued 1) "" "s")
       arxana-evidence-open-session-summary-model)
      (arxana-evidence--start-open-session-summary-worker))))

(defun arxana-evidence--apply-open-session-llm-summaries (items)
  "Return ITEMS with cached or newly generated LLM summaries applied."
  (if (not arxana-evidence-open-session-llm-summaries)
      items
    (arxana-evidence--queue-open-session-summary-requests items)
    (mapcar
     (lambda (item)
       (if (eq (plist-get item :type) 'evidence-open-session)
           (let* ((key (arxana-evidence--open-session-cache-key item))
                  (summary (gethash key arxana-evidence--open-session-summary-cache)))
             (if summary
                 (plist-put (plist-put item :about summary)
                            :summary-ready t)
               item))
         item))
     items)))

(defun arxana-browser--evidence-open-sessions-format ()
  [("M" 2 nil)
   ("Buffer" 28 t)
   ("Agent" 8 t)
   ("State" 8 t)
   ("Turns" 7 nil)
   ("Latest" 20 t)
   ("Missions" 30 nil)
   ("About" 0 nil)])

(defun arxana-browser--evidence-open-sessions-row (item)
  (vector (if (gethash (plist-get item :buffer)
                       arxana-evidence--open-session-marks)
              "*"
            " ")
          (arxana-evidence--truncate (or (plist-get item :buffer) "") 27)
          (arxana-evidence--truncate (or (plist-get item :agent) "") 7)
          (arxana-evidence--truncate (or (plist-get item :state) "") 7)
          (format "%d" (or (plist-get item :count) 0))
          (arxana-evidence--truncate (or (plist-get item :latest) "") 19)
          (arxana-evidence--truncate
           (string-join (or (plist-get item :missions) '()) ", ") 29)
          (or (plist-get item :about) "")))

(defun arxana-browser--evidence-open-sessions-items ()
  "Return Evidence-backed semantic summaries for open REPL buffers."
  (let* ((started (float-time))
         (arxana-evidence--open-session-refresh-stats nil))
    (unwind-protect
        (let ((sessions (arxana-evidence--open-repl-sessions)))
          (setq arxana-evidence--open-session-refresh-stats
                (list :sessions (length sessions)))
          (if sessions
              (let ((items (arxana-evidence--apply-open-session-llm-summaries
                            (mapcar #'arxana-evidence--open-session-item sessions))))
                (dolist (item items)
                  (when (eq (plist-get item :type) 'evidence-open-session)
                    (arxana-evidence--cache-open-session-item item)))
                items)
            (list (list :type 'info
                        :label "No open REPL sessions"
                        :description "Open a Codex or Claude REPL buffer to see Evidence-backed summaries."))))
      (when arxana-evidence--open-session-refresh-stats
        (let ((stats arxana-evidence--open-session-refresh-stats)
              (elapsed (- (float-time) started)))
          (arxana-evidence--log-open-session-refresh
           "%d session%s in %.3fs (row cache %d hit/%d miss; evidence %d fetch %.3fs; turns %d count %.3fs; llm %d call/%d session %.3fs)"
           (or (plist-get stats :sessions) 0)
           (if (= (or (plist-get stats :sessions) 0) 1) "" "s")
           elapsed
           (or (plist-get stats :row-cache-hits) 0)
           (or (plist-get stats :row-cache-misses) 0)
           (or (plist-get stats :evidence-fetches) 0)
           (or (plist-get stats :evidence-fetch-seconds) 0.0)
           (or (plist-get stats :turn-count-fetches) 0)
           (or (plist-get stats :turn-count-seconds) 0.0)
           (or (plist-get stats :llm-calls) 0)
           (or (plist-get stats :llm-requested-sessions) 0)
           (or (plist-get stats :llm-seconds) 0.0)))))))

(defun arxana-evidence--open-session-mark-key (item)
  "Return mark key for open-session ITEM."
  (and (eq (plist-get item :type) 'evidence-open-session)
       (plist-get item :buffer)))

(defun arxana-evidence-toggle-open-session-mark-at-point ()
  "Toggle mark for the open REPL session at point."
  (interactive)
  (let* ((row (if (fboundp 'arxana-browser--current-row)
                  (arxana-browser--current-row)
                0))
         (item (tabulated-list-get-id))
         (key (arxana-evidence--open-session-mark-key item)))
    (unless key
      (user-error "No open REPL session on this line"))
    (if (gethash key arxana-evidence--open-session-marks)
        (remhash key arxana-evidence--open-session-marks)
      (puthash key t arxana-evidence--open-session-marks))
    (arxana-browser--render)
    (when (fboundp 'arxana-browser--goto-row)
      (arxana-browser--goto-row (1+ row)))))

(defun arxana-evidence--open-session-visible-items ()
  "Return open-session items visible in the current tabulated list."
  (delq nil
        (mapcar (lambda (entry)
                  (let ((item (car-safe entry)))
                    (when (eq (plist-get item :type) 'evidence-open-session)
                      item)))
                (or tabulated-list-entries '()))))

(defun arxana-evidence--marked-open-session-items ()
  "Return marked open-session items visible in the current tabulated list."
  (cl-remove-if-not
   (lambda (item)
     (gethash (arxana-evidence--open-session-mark-key item)
              arxana-evidence--open-session-marks))
   (arxana-evidence--open-session-visible-items)))

(defun arxana-evidence--cleanup-open-session-item (item)
  "Interrupt live work and kill the Emacs buffer for open-session ITEM.
Returns non-nil when a live buffer was cleaned up."
  (let* ((buffer-name (plist-get item :buffer))
         (buffer (and buffer-name (get-buffer buffer-name)))
         invoke-buffer)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (and (boundp 'codex-repl-invoke-buffer-name)
                   (local-variable-p 'codex-repl-invoke-buffer-name)
                   (stringp codex-repl-invoke-buffer-name)
                   (not (string-empty-p codex-repl-invoke-buffer-name)))
          (setq invoke-buffer (get-buffer codex-repl-invoke-buffer-name)))
        (when (and (boundp 'agent-chat--pending-process)
                   (process-live-p agent-chat--pending-process))
          (if (fboundp 'agent-chat-interrupt)
              (agent-chat-interrupt)
            (kill-process agent-chat--pending-process)
            (setq agent-chat--pending-process nil))))
      (kill-buffer buffer)
      (when (buffer-live-p invoke-buffer)
        (kill-buffer invoke-buffer))
      (when buffer-name
        (remhash buffer-name arxana-evidence--open-session-marks))
      t)))

(defun arxana-evidence-delete-open-sessions ()
  "Delete marked open REPL session buffers, or the session at point.
Live pending agent subprocesses are interrupted before their buffers are killed."
  (interactive)
  (let* ((marked (arxana-evidence--marked-open-session-items))
         (current (tabulated-list-get-id))
         (items (if marked
                    marked
                  (and (eq (plist-get current :type) 'evidence-open-session)
                       (list current)))))
    (unless items
      (user-error "No marked open REPL sessions"))
    (unless (yes-or-no-p
             (format "Delete %d open REPL session buffer%s? "
                     (length items)
                     (if (= (length items) 1) "" "s")))
      (user-error "Delete cancelled"))
    (let ((deleted 0))
      (dolist (item items)
        (when (arxana-evidence--cleanup-open-session-item item)
          (cl-incf deleted)))
      (arxana-browser--render)
      (message "Deleted %d open REPL session buffer%s"
               deleted
               (if (= deleted 1) "" "s")))))

(defun arxana-evidence--chat-turn-item (entry)
  "Render a chat-turn evidence entry as a conversation item."
  (let* ((body (plist-get entry :evidence/body))
         (role (or (and (listp body) (plist-get body :role)) "system"))
         (text (or (and (listp body) (plist-get body :text)) ""))
         (author (or (plist-get entry :evidence/author) role))
         (time-str (or (plist-get entry :evidence/at) ""))
         (time-short (if (>= (length time-str) 19)
                         (substring time-str 11 19)
                       time-str))
         (eid (arxana-evidence--entry-id entry)))
    (list :type 'evidence-chat-turn
          :entry entry
          :evidence-id eid
          :role role
          :author author
          :time time-short
          :text text
          :label (format "%s [%s]" author time-short)
          :description text)))

(defun arxana-evidence--meta-item (entry)
  "Render a non-chat evidence entry as a subdued metadata line."
  (let* ((body (plist-get entry :evidence/body))
         (event (or (and (listp body) (plist-get body :event)) ""))
         (author (or (plist-get entry :evidence/author) ""))
         (time-str (or (plist-get entry :evidence/at) ""))
         (time-short (if (>= (length time-str) 19)
                         (substring time-str 11 19)
                       time-str))
         (detail (cond
                  ((string= event "context-retrieval")
                   (let ((results (and (listp body) (plist-get body :results))))
                     (if results
                         (mapconcat (lambda (r) (or (plist-get r :id) "?"))
                                    results ", ")
                       "")))
                  ((string= event "invoke-start") "invoke started")
                  ((string= event "invoke-complete")
                   (format "invoke complete (ok=%s)" (plist-get body :ok)))
                  (t (arxana-evidence--truncate (format "%s" event) 60)))))
    (list :type 'evidence-meta
          :entry entry
          :evidence-id (arxana-evidence--entry-id entry)
          :label (format "  %s %s · %s" time-short event author)
          :description detail)))

(defun arxana-evidence--extract-user-message (text)
  "Strip surface contract / transport headers from TEXT to get the core message."
  (let ((result text))
    ;; Strip 'Agent: ...\n\nUser message:\n' prefix
    (when (and (stringp result) (string-match "\\(?:^\\|\n\\)User message:\n" result))
      (setq result (substring result (match-end 0))))
    ;; Strip '--- CURRENT TURN ---...\n---\n' prefix
    (when (and (stringp result) (string-match "\\`---[^\n]*\n\\(?:.*\n\\)*?---\n+" result))
      (setq result (substring result (match-end 0))))
    (or result text)))

(defun arxana-evidence--dedup-session-entries (entries)
  "Remove forum-post duplicates when a chat-turn exists for the same content.
Forum-post and chat-turn are dual records of the same turn; prefer chat-turn."
  (let ((chat-turn-texts (make-hash-table :test 'equal)))
    ;; Collect normalized text from chat-turn entries
    (dolist (entry entries)
      (let* ((body (plist-get entry :evidence/body))
             (event (and (listp body) (plist-get body :event))))
        (when (and (stringp event) (string= event "chat-turn"))
          (let ((text (and (listp body) (plist-get body :text))))
            (when (and (stringp text) (> (length text) 0))
              (puthash (substring text 0 (min 60 (length text))) t chat-turn-texts))))))
    ;; Filter out forum-posts whose core text matches a chat-turn
    (cl-remove-if
     (lambda (entry)
       (let ((etype (arxana-evidence--kw-name (plist-get entry :evidence/type))))
         (and (string= etype "forum-post")
              (let* ((body (plist-get entry :evidence/body))
                     (raw-text (and (listp body)
                                    (or (plist-get body :text) "")))
                     (clean (arxana-evidence--extract-user-message raw-text)))
                (and (stringp clean)
                     (> (length clean) 0)
                     (gethash (substring clean 0 (min 60 (length clean)))
                              chat-turn-texts))))))
     entries)))

(defun arxana-browser--evidence-session-items (context)
  "Render session entries as a chat-like conversation with metadata interleaved."
  (let* ((session-id (plist-get context :session-id))
         ;; Always refetch by session-id so large sessions are not silently
         ;; truncated by whatever partial slice originally produced CONTEXT.
         (entries (or (when (and (stringp session-id) (not (string= session-id "(none)")))
                        (let ((limit (or (arxana-evidence--count-session-entries session-id)
                                         arxana-evidence-thread-limit)))
                          (arxana-evidence--fetch-evidence
                           `(("session-id" . ,session-id)
                             ("limit" . ,(number-to-string limit))))))
                      (plist-get context :entries)))
         ;; Deduplicate forum-post vs chat-turn
         (deduped (arxana-evidence--dedup-session-entries (or entries '())))
         ;; Sort chronologically
         (sorted (sort (copy-sequence deduped)
                       (lambda (a b)
                         (string< (arxana-evidence--entry-time a)
                                  (arxana-evidence--entry-time b))))))
    (if (and sorted (> (length sorted) 0))
        (mapcar (lambda (entry)
                  (if (arxana-evidence--chat-like-entry-p entry)
                      (arxana-evidence--chat-turn-item entry)
                    (arxana-evidence--meta-item entry)))
                sorted)
      (list (list :type 'info
                  :label "No evidence in session"
                  :description (format "Session %s has no visible entries."
                                       (or session-id "(none)")))))))

(defun arxana-evidence--find-root-id (entry entry-table)
  (let ((current (arxana-evidence--entry-id entry))
        (seen (make-hash-table :test 'equal))
        parent-id)
    (while (and current
                (not (gethash current seen)))
      (puthash current t seen)
      (let* ((node (gethash current entry-table))
             (parent (and node (plist-get node :evidence/in-reply-to))))
        (if (and parent (gethash parent entry-table))
            (setq current parent)
          (setq parent-id current
                current nil))))
    parent-id))

(defun arxana-evidence--thread-items (entries)
  (let ((entry-table (make-hash-table :test 'equal))
        (thread-table (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (puthash (arxana-evidence--entry-id entry) entry entry-table))
    (dolist (entry entries)
      (let* ((root-id (arxana-evidence--find-root-id entry entry-table))
             (bucket (gethash root-id thread-table)))
        (puthash root-id (cons entry bucket) thread-table)))
    (let (threads)
      (maphash
       (lambda (root-id bucket)
         (let* ((root (or (gethash root-id entry-table) (car bucket)))
                (sorted (sort (copy-sequence bucket)
                              (lambda (a b)
                                (string< (arxana-evidence--entry-time a)
                                         (arxana-evidence--entry-time b)))))
                (latest (car (last sorted))))
           (push (list :type 'evidence-thread
                       :root-id root-id
                       :leaf-id (arxana-evidence--entry-id latest)
                       :session (or (plist-get root :evidence/session-id) "-")
                       :root-type (arxana-evidence--kw-name (plist-get root :evidence/type))
                       :latest (arxana-evidence--entry-time latest)
                       :count (length bucket)
                       :summary (arxana-evidence--entry-summary root))
                 threads)))
       thread-table)
      (sort threads (lambda (a b)
                      (string> (or (plist-get a :latest) "")
                               (or (plist-get b :latest) "")))))))

(defun arxana-browser--evidence-threads-format ()
  [("Root ID" 36 t)
   ("Root Type" 16 t)
   ("Replies" 8 nil)
   ("Latest" 20 t)
   ("Session" 20 nil)
   ("Summary" 40 nil)])

(defun arxana-browser--evidence-threads-row (item)
  (vector (arxana-evidence--truncate (or (plist-get item :root-id) "") 35)
          (arxana-evidence--truncate (or (plist-get item :root-type) "") 15)
          (format "%d" (max 0 (1- (or (plist-get item :count) 0))))
          (arxana-evidence--truncate (or (plist-get item :latest) "") 19)
          (arxana-evidence--truncate (or (plist-get item :session) "") 19)
          (arxana-evidence--truncate (or (plist-get item :summary) "") 39)))

(defun arxana-browser--evidence-threads-items ()
  (condition-case err
      (let ((entries (arxana-evidence--fetch-evidence
                      `(("limit" . ,arxana-evidence-thread-limit)))))
        (if entries
            (arxana-evidence--thread-items entries)
          (list (list :type 'info
                      :label "No thread data"
                      :description "Need at least one evidence entry."))))
    (error
     (list (list :type 'info
                 :label "Evidence unavailable"
                 :description (format "Error: %s" (error-message-string err)))))))

(defun arxana-browser--evidence-chain-items (context)
  (let* ((evidence-id (or (plist-get context :evidence-id)
                          (plist-get context :leaf-id)
                          (plist-get context :root-id))))
    (condition-case err
        (if (or (null evidence-id) (string-empty-p evidence-id))
            (list (list :type 'info
                        :label "Missing evidence id"
                        :description "Cannot fetch chain without an id."))
          (let* ((payload (arxana-evidence--request
                           (format "/evidence/%s/chain"
                                   (url-hexify-string evidence-id))
                           nil))
                 (chain (or (plist-get payload :chain) '())))
            (if chain
                (cl-loop for entry in chain
                         for depth from 0
                         collect (arxana-evidence--entry->item entry depth))
              (list (list :type 'info
                          :label "No chain entries"
                          :description (format "No chain found for %s." evidence-id))))))
      (error
       (list (list :type 'info
                   :label "Chain unavailable"
                   :description (format "Error: %s" (error-message-string err))))))))

(defun arxana-browser--evidence-thread-reader-items (context)
  (let* ((evidence-id (or (plist-get context :evidence-id)
                          (plist-get context :leaf-id)
                          (plist-get context :root-id))))
    (condition-case err
        (if (or (null evidence-id) (string-empty-p evidence-id))
            (list (list :type 'info
                        :label "Missing evidence id"
                        :description "Cannot open thread without an id."))
          (let* ((payload (arxana-evidence--request
                           (format "/evidence/%s/chain"
                                   (url-hexify-string evidence-id))
                           nil))
                 (chain (or (plist-get payload :chain) '()))
                 (chat-chain (cl-remove-if-not #'arxana-evidence--chat-like-entry-p chain))
                 (display-chain (if chat-chain chat-chain chain)))
            (if chain
                (cl-loop for entry in display-chain
                         for depth from 0
                         collect (arxana-evidence--thread-turn-item entry depth))
              (list (list :type 'info
                          :label "No thread entries"
                          :description (format "No thread found for %s." evidence-id))))))
      (error
       (list (list :type 'info
                   :label "Thread unavailable"
                   :description (format "Error: %s" (error-message-string err))))))))

(defun arxana-browser-evidence-open-session (item)
  "Open a session summary ITEM into its timeline view."
  (setq arxana-browser--stack
        (cons (list :view 'evidence-session
                    :label (format "Session %s" (or (plist-get item :session-id) "(none)"))
                    :session-id (plist-get item :session-id)
                    :entries (plist-get item :entries))
              arxana-browser--stack))
  (arxana-browser--render))

(defun arxana-browser-evidence-open-live-session (item)
  "Open the live REPL buffer for open-session ITEM."
  (let* ((buffer-name (plist-get item :buffer))
         (buffer (and buffer-name (get-buffer buffer-name))))
    (unless (eq (plist-get item :type) 'evidence-open-session)
      (user-error "Not an open REPL session row"))
    (unless buffer-name
      (user-error "Open session row has no buffer name"))
    (unless buffer
      (user-error "Open REPL buffer no longer exists: %s" buffer-name))
    (switch-to-buffer buffer)))

(defun arxana-browser-evidence-open-thread (item)
  "Open THREAD ITEM into a thread-reader view."
  (let ((leaf-id (or (plist-get item :leaf-id)
                     (plist-get item :root-id))))
    (setq arxana-browser--stack
          (cons (list :view 'evidence-thread-reader
                      :label (format "Thread Reader %s" (or (plist-get item :root-id) leaf-id))
                      :evidence-id leaf-id
                      :root-id (plist-get item :root-id))
                arxana-browser--stack))
    (arxana-browser--render)))

(defun arxana-browser-evidence-open-entry (item)
  "Open EVIDENCE ENTRY ITEM into a detail view."
  (let ((evidence-id (or (plist-get item :evidence-id)
                         (arxana-evidence--entry-id (plist-get item :entry)))))
    (setq arxana-browser--stack
          (cons (list :view 'evidence-entry-detail
                      :label (format "Evidence %s" evidence-id)
                      :evidence-id evidence-id
                      :entry (plist-get item :entry))
                arxana-browser--stack))
    (arxana-browser--render)))

;; =============================================================================
;; Session chat view — format and row functions
;; =============================================================================

(defun arxana-browser--evidence-session-chat-format ()
  "Format for the session chat view — wide label + description."
  [("Speaker" 16 t)
   ("Message" 0 nil)])

(defun arxana-browser--evidence-session-chat-row (item)
  "Row renderer for session chat view. Dispatches on item type."
  (let ((type (plist-get item :type)))
    (cond
     ((eq type 'evidence-chat-turn)
      (vector (or (plist-get item :label) "")
              (or (plist-get item :description) "")))
     ((eq type 'evidence-meta)
      (vector (or (plist-get item :label) "")
              (or (plist-get item :description) "")))
     (t
      (vector (or (plist-get item :label) "")
              (or (plist-get item :description) ""))))))

(provide 'arxana-browser-evidence)
;;; arxana-browser-evidence.el ends here
