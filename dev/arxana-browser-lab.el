;;; arxana-browser-lab.el --- Lab browser views -*- lexical-binding: t; -*-

;;; Commentary:
;; Lab browsing helpers for the Arxana browser.
;; Includes active session viewing via Claude stream and archived session browsing.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-parse)
(require 'url-http)

(require 'arxana-lab)

(defvar arxana-browser--stack)
(defvar arxana-browser--context)
(defvar arxana-browser--buffer)

(declare-function arxana-browser--item-at-point "arxana-browser-core")
(declare-function arxana-browser--render "arxana-browser-core")
(declare-function fuclient-claude-stream-connect "fuclient-claude-stream")
(declare-function arxana-browser-patterns--ensure-frame "arxana-browser-patterns")
(declare-function arxana-lab-open-raw-payload "arxana-lab" (payload))

(defgroup arxana-lab-sessions nil
  "Lab session browsing."
  :group 'arxana)

(defcustom arxana-lab-sessions-server
  (or (getenv "FUTON3_SERVER") "http://localhost:5050")
  "Futon3 HTTP server URL for fetching session lists."
  :type 'string
  :group 'arxana-lab-sessions)

(defcustom arxana-lab-sessions-servers nil
  "List of Futon3 HTTP server URLs to query for session lists.
When nil, uses `arxana-lab-sessions-server`."
  :type '(repeat string)
  :group 'arxana-lab-sessions)

(defcustom arxana-lab-sessions-request-timeout 10
  "Timeout in seconds for session list requests."
  :type 'integer
  :group 'arxana-lab-sessions)

(defcustom arxana-lab-futon1-server
  (or (getenv "FUTON1_API_BASE")
      (getenv "STACK_HUD_FUTON1_API_BASE")
      "http://localhost:8080/api/alpha")
  "Futon1 API base URL for archived lab sessions."
  :type 'string
  :group 'arxana-lab-sessions)

(defun arxana-lab--parse-json (body)
  "Parse JSON BODY into plist."
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

(defun arxana-lab--session-servers ()
  (let ((servers arxana-lab-sessions-servers))
    (cond
     ((and (listp servers) (seq servers)) servers)
     ((stringp servers) (list servers))
     ((and arxana-lab-sessions-server (stringp arxana-lab-sessions-server))
      (list arxana-lab-sessions-server))
     (t nil))))

(defun arxana-lab--server-host (server)
  (condition-case _err
      (let* ((url (url-generic-parse-url server))
             (host (url-host url))
             (port (url-port url)))
        (cond
         ((and host port) (format "%s:%s" host port))
         (host host)
         (t server)))
    (error server)))

(defun arxana-lab--merge-sessions (sessions)
  (seq-sort (lambda (a b)
              (string> (or (plist-get a :modified) "")
                       (or (plist-get b :modified) "")))
            sessions))

(defcustom arxana-lab-use-plain-websocket t
  "Use plain ws:// instead of wss:// for lab-ws connections.
Set to t as workaround for Emacs 31 TLS/nginx WebSocket issues.
Set to nil to use wss:// through nginx SSL termination."
  :type 'boolean
  :group 'arxana-lab-sessions)

(defun arxana-lab--server->ws (server)
  "Convert HTTP server URL to WebSocket URL for lab-ws.
If `arxana-lab-use-plain-websocket' is t, always use ws://5056 (direct).
Otherwise, HTTP 5050 -> ws 5056, HTTPS 5051 -> wss 5057 (nginx SSL)."
  (let* ((base (string-remove-suffix "/" (or server "")))
         ;; Extract host from URL
         (host (replace-regexp-in-string
                "^\\(https?\\|wss?\\)://" ""
                (replace-regexp-in-string ":[0-9]+.*$" "" base))))
    (if arxana-lab-use-plain-websocket
        ;; Direct connection to lab-ws on 5056 (no SSL)
        (format "ws://%s:5056" host)
      ;; SSL path through nginx
      (let* ((is-ssl (or (string-prefix-p "https://" base)
                         (string-prefix-p "wss://" base)))
             (with-port (if is-ssl
                            (replace-regexp-in-string ":5051\\b" ":5057" base)
                          (replace-regexp-in-string ":5050\\b" ":5056" base))))
        (cond
         ((string-prefix-p "wss://" with-port) with-port)
         ((string-prefix-p "ws://" with-port) with-port)
         ((string-prefix-p "https://" with-port)
          (concat "wss://" (string-remove-prefix "https://" with-port)))
         ((string-prefix-p "http://" with-port)
          (concat "ws://" (string-remove-prefix "http://" with-port)))
         (t with-port))))))

(defun arxana-lab--fetch-sessions (endpoint)
  "Fetch sessions from ENDPOINT (e.g., /fulab/lab/sessions/active)."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (servers (arxana-lab--session-servers))
         (merged '()))
    (unless servers
      (user-error "No lab session servers configured"))
    (dolist (server servers)
      (let* ((url (concat (string-remove-suffix "/" server) endpoint))
             (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
        (if (not buffer)
            (message "[arxana-lab] Failed to fetch sessions from %s" url)
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "\n\n" nil 'move)
            (let* ((body (buffer-substring-no-properties (point) (point-max)))
                   (payload (arxana-lab--parse-json body))
                   (sessions (plist-get payload :sessions))
                   (host (arxana-lab--server-host server)))
              (kill-buffer buffer)
              (when (listp sessions)
                (setq merged
                      (append merged
                              (mapcar (lambda (s)
                                        (append (if (listp s) s (list :id s))
                                                (list :server server :host host)))
                                      sessions)))))))))
    (list :ok t :sessions (arxana-lab--merge-sessions merged))))

(defun arxana-lab--fetch-futon1 (endpoint)
  "Fetch lab sessions from Futon1 ENDPOINT (e.g., /lab/sessions)."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (base (string-remove-suffix "/" arxana-lab-futon1-server))
         (url (concat base endpoint))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        (list :ok nil :entries nil)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (payload (arxana-lab--parse-json body)))
          (kill-buffer buffer)
          payload)))))

(defun arxana-lab--fetch-futon1-session (session-id)
  "Fetch a single Futon1 lab session by SESSION-ID."
  (arxana-lab--fetch-futon1 (format "/lab/session/%s" session-id)))

(defun arxana-lab--truncate (text max-len)
  "Truncate TEXT to MAX-LEN characters."
  (let ((value (or text "")))
    (if (> (length value) max-len)
        (concat (substring value 0 (max 0 (- max-len 3))) "...")
      value)))

;; =============================================================================
;; Lab menu items (Active Sessions / Archived Sessions)
;; =============================================================================

(defun arxana-browser--lab-menu-items ()
  "Return the Lab sub-menu items."
  (list (list :type 'lab-menu
              :label "Evidence Timeline"
              :description "Evidence landscape entries (PSRs, PURs, PARs)"
              :view 'evidence-timeline)
        (list :type 'lab-menu
              :label "Evidence by Session"
              :description "Evidence grouped by session ID"
              :view 'evidence-sessions)
        (list :type 'lab-menu
              :label "Active Sessions"
              :description "Currently running Codex/Claude sessions"
              :view 'lab-sessions-active)
        (list :type 'lab-menu
              :label "Recent Sessions"
              :description "All sessions in ~/.claude/projects/ and ~/.codex/sessions/"
              :view 'lab-sessions-recent)
        (list :type 'lab-menu
              :label "Raw Lab Logs"
              :description "Exported sessions in lab/raw"
              :view 'lab-sessions-raw)
        (list :type 'lab-menu
              :label "Archived Sessions"
              :description "Persisted lab sessions in Futon1"
              :view 'lab-sessions-archived)
        (list :type 'lab-menu
              :label "Lab Files"
              :description "Browse raw/stubs/drafts files"
              :view 'lab)))

(defun arxana-browser--lab-menu-format ()
  "Format for Lab menu view."
  [("Option" 20 t)
   ("Description" 0 nil)])

(defun arxana-browser--lab-menu-row (item)
  "Row for Lab menu ITEM."
  (vector (or (plist-get item :label) "")
          (or (plist-get item :description) "")))

;; =============================================================================
;; Active Sessions view
;; =============================================================================

(defun arxana-browser--lab-sessions-active-format ()
  "Column format for active sessions view."
  [("Session ID" 38 t)
   ("Project" 25 t)
   ("Host" 22 t)
   ("Size" 8 nil)
   ("Active" 6 nil)
   ("Modified" 20 t)])

(defun arxana-browser--lab-sessions-active-row (item)
  "Row for active session ITEM."
  (let ((id (or (plist-get item :id) ""))
        (project (or (plist-get item :project) ""))
        (host (or (plist-get item :host) ""))
        (size (format "%dkb" (or (plist-get item :size-kb) 0)))
        (active (if (plist-get item :active) "●" ""))
        (modified (or (plist-get item :modified) "")))
    (vector (arxana-lab--truncate id 36)
            (arxana-lab--truncate project 24)
            (arxana-lab--truncate host 21)
            size
            active
            (arxana-lab--truncate modified 19))))

(defun arxana-browser--lab-sessions-active-items ()
  "Fetch and return active session items (only truly active ones)."
  (condition-case err
      (let* ((response (arxana-lab--fetch-sessions "/fulab/lab/sessions/active"))
             (sessions (plist-get response :sessions))
             ;; Filter to only active sessions
             (active-sessions (seq-filter (lambda (s) (plist-get s :active)) sessions)))
        (if active-sessions
            (mapcar (lambda (s)
                      (append (list :type 'lab-session-active) s))
                    active-sessions)
          (list (list :type 'info
                      :label "No active sessions"
                      :description "Start a Codex or Claude session to see it here"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch sessions"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Recent Sessions view (all sessions, not just active)
;; =============================================================================

(defun arxana-browser--lab-sessions-recent-items ()
  "Fetch and return all recent session items."
  (condition-case err
      (let* ((response (arxana-lab--fetch-sessions "/fulab/lab/sessions/active"))
             (sessions (plist-get response :sessions)))
        (if sessions
            (mapcar (lambda (s)
                      (append (list :type 'lab-session-active) s))
                    sessions)
          (list (list :type 'info
                      :label "No sessions found"
                      :description "No Codex/Claude sessions detected on configured hosts"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch sessions"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Raw Lab Logs view (lab/raw)
;; =============================================================================

(defun arxana-browser--lab-sessions-raw-format ()
  "Column format for raw lab logs view."
  [("Session ID" 40 t)
   ("Host" 22 t)
   ("Size" 8 nil)
   ("Modified" 20 t)])

(defun arxana-browser--lab-sessions-raw-row (item)
  "Row for raw lab log ITEM."
  (let ((id (or (plist-get item :id) ""))
        (host (or (plist-get item :host) ""))
        (size (format "%dkb" (or (plist-get item :size-kb) 0)))
        (modified (or (plist-get item :modified) "")))
    (vector (arxana-lab--truncate id 38)
            (arxana-lab--truncate host 21)
            size
            (arxana-lab--truncate modified 19))))

(defun arxana-browser--lab-sessions-raw-items ()
  "Fetch and return raw lab log items."
  (condition-case err
      (let* ((response (arxana-lab--fetch-sessions "/fulab/lab/sessions/archived"))
             (sessions (plist-get response :sessions)))
        (if sessions
            (mapcar (lambda (s)
                      (append (list :type 'lab-session-raw) s))
                    sessions)
          (list (list :type 'info
                      :label "No raw lab logs"
                      :description "Export sessions to lab/raw to see them here"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch raw logs"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Archived Sessions view (Futon1)
;; =============================================================================

(defun arxana-browser--lab-sessions-archived-format ()
  "Column format for archived sessions view."
  [("Session ID" 40 t)
   ("Start" 20 t)
   ("End" 20 t)])

(defun arxana-browser--lab-sessions-archived-row (item)
  "Row for archived session ITEM."
  (let ((id (or (plist-get item :id) ""))
        (start (or (plist-get item :timestamp-start) ""))
        (end (or (plist-get item :timestamp-end) "")))
    (vector (arxana-lab--truncate id 38)
            (arxana-lab--truncate start 19)
            (arxana-lab--truncate end 19))))

(defun arxana-browser--lab-sessions-archived-items ()
  "Fetch and return archived session items from Futon1."
  (condition-case err
      (let* ((response (arxana-lab--fetch-futon1 "/lab/sessions"))
             (entries (plist-get response :entries)))
        (if entries
            (mapcar (lambda (s)
                      (append (list :type 'lab-session-archived) s))
                    entries)
          (list (list :type 'info
                      :label "No archived sessions"
                      :description "No Futon1 lab sessions found"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch archived sessions"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Session actions
;; =============================================================================

(defun arxana-browser-lab-view-session (item)
  "View (stream) the session ITEM."
  (let ((path (plist-get item :path)))
    (unless path
      (user-error "No path for session"))
    (let ((server (arxana-lab--server->ws
                   (or (plist-get item :server) arxana-lab-sessions-server))))
      ;; Set server globally so reconnects work
      (setq fuclient-claude-stream-server server)
      (if (featurep 'fuclient-claude-stream)
          (fuclient-claude-stream-connect path)
        (if (require 'fuclient-claude-stream nil t)
            (fuclient-claude-stream-connect path)
          (user-error "fuclient-claude-stream not available"))))))

(defun arxana-browser-lab-open-session (item)
  "Open a lab session ITEM - dispatch based on type."
  (let ((type (plist-get item :type)))
    (pcase type
      ('lab-session-active
       (arxana-browser-lab-view-session item))
      ('lab-session-raw
       (let ((path (plist-get item :path)))
         (if path
             (arxana-lab-open-raw-viewer path)
           (user-error "No path for session"))))
      ('lab-session-archived
       (let* ((session-id (plist-get item :id))
              (payload (arxana-lab--fetch-futon1-session session-id))
              (doc (plist-get payload :doc)))
         (if doc
             (arxana-lab-open-raw-payload doc)
           (user-error "No Futon1 data for session"))))
      ('lab-menu
       ;; Navigate to sub-view
       (let ((view (plist-get item :view))
             (label (plist-get item :label)))
         (setq arxana-browser--stack
               (cons (list :view view :label label)
                     arxana-browser--stack))
         (arxana-browser--render)))
      (_
       (user-error "Unknown session type: %s" type)))))

;; =============================================================================
;; Original Lab entries (for Lab Files sub-view)
;; =============================================================================

(defun arxana-browser--lab-format ()
  [("Session" 36 t)
   ("Start" 20 t)
   ("Files" 7 t)
   ("Focus" 0 nil)])

(defun arxana-browser--lab-file-format ()
  [("File" 32 t)
   ("When" 17 t)
   ("Kind" 8 t)
   ("Path" 0 nil)])

(defun arxana-browser--lab-row (item)
  (let* ((session (or (plist-get item :session-id) ""))
         (start (or (plist-get item :timestamp-start) ""))
         (files (or (plist-get item :files) '()))
         (focus (or (car files) "")))
    (vector session
            start
            (format "%d" (length files))
            focus)))

(defun arxana-browser--lab-file-row (item)
  (let* ((label (or (plist-get item :label) ""))
         (modified (or (plist-get item :modified) ""))
         (kind (or (plist-get item :kind) ""))
         (path (or (plist-get item :path) "")))
    (vector label modified kind path)))

(defun arxana-browser--lab-items ()
  (let ((entries (or (arxana-lab-entries) '())))
    (if (and entries (listp entries))
        entries
      (list (list :type 'info
                  :label "No lab entries detected"
                  :description "Run dev/lab-export.clj to populate lab/raw.")))))

(defun arxana-browser-lab-browse-files (kind)
  "Open the lab files browser for KIND (raw, stubs, drafts)."
  (interactive
   (list (intern (completing-read "Lab files: " '("raw" "stubs" "drafts") nil t))))
  (let* ((kind (if (symbolp kind) kind (intern kind)))
         (label (pcase kind
                  ('raw "Raw")
                  ('stubs "Stubs")
                  ('drafts "Drafts")
                  (_ (capitalize (format "%s" kind))))))
    (with-current-buffer (get-buffer-create arxana-browser--buffer)
      (setq arxana-browser--stack (list (list :view 'lab-files
                                                       :kind kind
                                                       :label label)))
      (setq arxana-browser--context nil))
    (arxana-browser--render)))

(defun arxana-browser-lab-browse-files-other-frame (kind)
  "Open the lab files browser for KIND in the Arxana frame."
  (interactive
   (list (intern (completing-read "Lab files: " '("raw" "stubs" "drafts") nil t))))
  (let ((frame (arxana-browser-patterns--ensure-frame)))
    (with-selected-frame frame
      (let ((display-buffer-overriding-action
             '((display-buffer-same-window))))
        (arxana-browser-lab-browse-files kind))
      (select-frame-set-input-focus frame))))

(defalias 'arxana-browser-lab-browse-files-other-window
  #'arxana-browser-lab-browse-files-other-frame)

(defun arxana-browser--lab-entry-at-point ()
  (let ((item (tabulated-list-get-id)))
    (when (and item (eq (plist-get item :type) 'lab-entry))
      item)))

(defun arxana-browser--lab-open-trace ()
  (interactive)
  (let ((entry (arxana-browser--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-trace-object entry)))

(defun arxana-browser--lab-open-raw ()
  (interactive)
  (let ((entry (arxana-browser--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-raw-object entry)))

(defun arxana-browser--lab-open-draft ()
  (interactive)
  (let ((entry (arxana-browser--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-draft-object entry)))

;; =============================================================================
;; Evidence landscape views
;; =============================================================================

(defun arxana-browser--evidence-fetch (params)
  "Fetch evidence entries from Futon1a. PARAMS is an alist of query params."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (base (string-remove-suffix "/" arxana-lab-futon1-server))
         (query-string (mapconcat (lambda (pair)
                                    (format "%s=%s"
                                            (url-hexify-string (car pair))
                                            (url-hexify-string (cdr pair))))
                                  (seq-filter #'cdr params) "&"))
         (url (if (string-empty-p query-string)
                  (concat base "/evidence")
                (concat base "/evidence?" query-string)))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        (list :entries nil)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (payload (arxana-lab--parse-json body)))
          (kill-buffer buffer)
          (or payload (list :entries nil)))))))

;; -- Evidence Timeline view --

(defun arxana-browser--evidence-timeline-format ()
  "Column format for evidence timeline view."
  [("Time" 20 t)
   ("Type" 10 t)
   ("Claim" 12 t)
   ("Author" 14 t)
   ("Subject" 0 nil)])

(defun arxana-browser--evidence-timeline-row (item)
  "Row for evidence timeline ITEM."
  (let ((at (or (plist-get item :evidence/at) ""))
        (etype (or (plist-get item :evidence/type) ""))
        (claim (or (plist-get item :evidence/claim-type) ""))
        (author (or (plist-get item :evidence/author) ""))
        (subject (plist-get item :evidence/subject)))
    (vector (arxana-lab--truncate at 19)
            (arxana-lab--truncate (format "%s" etype) 9)
            (arxana-lab--truncate (format "%s" claim) 11)
            (arxana-lab--truncate author 13)
            (if (and (listp subject) (plist-get subject :ref/type))
                (format "%s:%s"
                        (or (plist-get subject :ref/type) "?")
                        (or (plist-get subject :ref/id) "?"))
              ""))))

(defun arxana-browser--evidence-timeline-items ()
  "Fetch and return evidence timeline items."
  (condition-case err
      (let* ((response (arxana-browser--evidence-fetch
                        (list (cons "limit" "100"))))
             (entries (plist-get response :entries)))
        (if entries
            (mapcar (lambda (e)
                      (append (list :type 'evidence-entry) e))
                    entries)
          (list (list :type 'info
                      :label "No evidence entries"
                      :description "Append evidence via futon3c to see entries here"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch evidence"
                 :description (format "Error: %s" (error-message-string err)))))))

;; -- Evidence Sessions view --

(defun arxana-browser--evidence-sessions-format ()
  "Column format for evidence sessions view."
  [("Session" 38 t)
   ("Entries" 8 t)
   ("Types" 30 nil)
   ("Latest" 20 t)])

(defun arxana-browser--evidence-sessions-row (item)
  "Row for evidence session ITEM."
  (let ((session-id (or (plist-get item :session-id) "(no session)"))
        (count (or (plist-get item :entry-count) 0))
        (types (or (plist-get item :type-summary) ""))
        (latest (or (plist-get item :latest-at) "")))
    (vector (arxana-lab--truncate session-id 37)
            (format "%d" count)
            (arxana-lab--truncate types 29)
            (arxana-lab--truncate latest 19))))

(defun arxana-browser--evidence-sessions-items ()
  "Fetch evidence and group by session-id."
  (condition-case err
      (let* ((response (arxana-browser--evidence-fetch
                        (list (cons "limit" "500"))))
             (entries (or (plist-get response :entries) '()))
             (groups (make-hash-table :test 'equal)))
        ;; Group entries by session-id
        (dolist (e entries)
          (let* ((sid (or (plist-get e :evidence/session-id) "(no session)"))
                 (existing (gethash sid groups)))
            (puthash sid (cons e existing) groups)))
        ;; Build session summary items
        (let ((items '()))
          (maphash
           (lambda (sid group-entries)
             (let* ((count (length group-entries))
                    (types (seq-uniq (mapcar (lambda (e)
                                              (format "%s" (or (plist-get e :evidence/type) "?")))
                                            group-entries)))
                    (latest (car (seq-sort (lambda (a b)
                                            (string> (or (plist-get a :evidence/at) "")
                                                     (or (plist-get b :evidence/at) "")))
                                          group-entries))))
               (push (list :type 'evidence-session
                           :session-id sid
                           :entry-count count
                           :type-summary (mapconcat #'identity types ", ")
                           :latest-at (or (plist-get latest :evidence/at) ""))
                     items)))
           groups)
          (if items
              (seq-sort (lambda (a b)
                          (string> (or (plist-get a :latest-at) "")
                                   (or (plist-get b :latest-at) "")))
                        items)
            (list (list :type 'info
                        :label "No evidence sessions"
                        :description "No session-tagged evidence found")))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch evidence"
                 :description (format "Error: %s" (error-message-string err)))))))

;; -- Evidence visit actions --

(defun arxana-browser-evidence-open-entry (item)
  "Open a single evidence ITEM in the detail viewer."
  (arxana-lab-open-evidence-entry item))

(defun arxana-browser-evidence-open-session (item)
  "Open evidence timeline filtered by session from ITEM."
  (let* ((session-id (plist-get item :session-id))
         (response (arxana-browser--evidence-fetch
                    (list (cons "session-id" session-id)
                          (cons "limit" "200"))))
         (entries (plist-get response :entries)))
    (if entries
        (arxana-lab-open-evidence-timeline entries session-id)
      (user-error "No evidence entries for session %s" session-id))))

(provide 'arxana-browser-lab)
;;; arxana-browser-lab.el ends here
