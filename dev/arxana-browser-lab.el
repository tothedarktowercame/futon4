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
              :label "Tensions"
              :description "Discrepancies between ideal and actual (devmap gaps)"
              :view 'tensions)
        (list :type 'lab-menu
              :label "Devmaps"
              :description "Architectural prototypes (wiring diagrams, exotypes)"
              :view 'devmaps)
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
      ('tension-entry
       (arxana-browser-tension-open-entry item))
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

(defvar-local arxana-browser--evidence-filter nil
  "Current evidence filter parameters (alist of query params).")

;; -- Evidence Timeline view --

(defun arxana-browser--evidence-timeline-format ()
  "Column format for evidence timeline view."
  [("Time" 18 t)
   ("Type" 7 t)
   ("Author" 12 t)
   ("↳" 2 nil)
   ("Preview" 30 nil)
   ("Subject" 0 nil)])

(defun arxana-browser--evidence-timeline-row (item)
  "Row for evidence timeline ITEM."
  (let* ((at (or (plist-get item :evidence/at) ""))
         (etype (plist-get item :evidence/type))
         (author (or (plist-get item :evidence/author) ""))
         (body (plist-get item :evidence/body))
         (reply-to (plist-get item :evidence/in-reply-to))
         (subject (plist-get item :evidence/subject))
         (type-label (arxana-lab--evidence-type-label etype))
         (type-face (arxana-lab--evidence-type-face etype))
         (type-display (if type-face
                           (propertize type-label 'face type-face)
                         type-label))
         (preview (arxana-lab--evidence-body-preview body etype)))
    (vector (arxana-lab--truncate at 17)
            type-display
            (arxana-lab--truncate author 11)
            (if reply-to "↳" "")
            (arxana-lab--truncate preview 29)
            (if (and (listp subject) (plist-get subject :ref/type))
                (format "%s:%s"
                        (or (plist-get subject :ref/type) "?")
                        (or (plist-get subject :ref/id) "?"))
              ""))))

(defun arxana-browser--evidence-timeline-items ()
  "Fetch and return evidence timeline items.
Applies `arxana-browser--evidence-filter' when set."
  (condition-case err
      (let* ((params (append (list (cons "limit" "100"))
                             (or arxana-browser--evidence-filter '())))
             (response (arxana-browser--evidence-fetch params))
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

;; =============================================================================
;; Evidence link navigation
;; =============================================================================

(defun arxana-browser--evidence-fetch-single (evidence-id)
  "Fetch a single evidence entry by EVIDENCE-ID from Futon1a."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (base (string-remove-suffix "/" arxana-lab-futon1-server))
         (url (concat base "/evidence/" (url-hexify-string evidence-id)))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        nil
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (payload (arxana-lab--parse-json body)))
          (kill-buffer buffer)
          (plist-get payload :entry))))))

(defun arxana-browser-lab--browse-evidence-entry (evidence-id)
  "Fetch evidence entry by EVIDENCE-ID and display in detail viewer."
  (let ((entry (arxana-browser--evidence-fetch-single evidence-id)))
    (if entry
        (arxana-lab-open-evidence-entry entry)
      (user-error "Could not fetch evidence entry: %s" evidence-id))))

(defun arxana-browser-lab--browse-evidence-chain (evidence-id)
  "Fetch and display the reply chain for EVIDENCE-ID."
  (interactive
   (list (or (get-text-property (point) 'arxana-evidence-id)
             (read-string "Evidence ID: "))))
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (base (string-remove-suffix "/" arxana-lab-futon1-server))
         (url (concat base "/evidence/" (url-hexify-string evidence-id) "/chain"))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        (user-error "Could not fetch chain for %s" evidence-id)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (payload (arxana-lab--parse-json body))
               (entries (plist-get payload :entries)))
          (kill-buffer buffer)
          (if entries
              (arxana-lab-open-evidence-timeline entries (format "chain:%s" evidence-id))
            (user-error "No chain entries for %s" evidence-id)))))))

;; Wire evidence link navigation at load time
(setq arxana-lab-evidence-browser-function #'arxana-browser-lab--browse-evidence-entry)

;; =============================================================================
;; Evidence filtering
;; =============================================================================

(defun arxana-browser-evidence-filter-by-type ()
  "Filter evidence timeline by type."
  (interactive)
  (let* ((types '("(all)" "pattern-selection" "pattern-outcome" "reflection"
                  "gate-traversal" "coordination" "forum-post" "mode-transition"
                  "presence-event" "correction" "conjecture"))
         (choice (completing-read "Filter by type: " types nil t)))
    (setq arxana-browser--evidence-filter
          (if (string= choice "(all)")
              nil
            (list (cons "type" choice))))
    (arxana-browser--render)))

(defun arxana-browser-evidence-filter-by-author ()
  "Filter evidence timeline by author."
  (interactive)
  (let ((author (read-string "Filter by author: ")))
    (setq arxana-browser--evidence-filter
          (if (string-empty-p author)
              nil
            (list (cons "author" author))))
    (arxana-browser--render)))

(defun arxana-browser-evidence-clear-filter ()
  "Clear evidence timeline filter."
  (interactive)
  (setq arxana-browser--evidence-filter nil)
  (arxana-browser--render))

;; =============================================================================
;; Futon3c server configuration
;; =============================================================================

(defcustom arxana-lab-futon3c-server
  (or (getenv "FUTON3C_API_BASE")
      "http://localhost:7070/api/alpha")
  "Futon3c API base URL for mission control, tensions, and portfolio data."
  :type 'string
  :group 'arxana-lab-sessions)

(defun arxana-browser--futon3c-fetch (endpoint)
  "Fetch ENDPOINT from the futon3c server. Returns parsed JSON plist."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (base (string-remove-suffix "/" arxana-lab-futon3c-server))
         (url (concat base endpoint))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        nil
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (payload (arxana-lab--parse-json body)))
          (kill-buffer buffer)
          payload)))))

;; =============================================================================
;; Tension browser view
;; =============================================================================

(defface arxana-lab-tension-uncovered-face
  '((t :foreground "#e06c75"))
  "Face for uncovered-component tensions."
  :group 'arxana-lab)

(defface arxana-lab-tension-blocked-face
  '((t :foreground "#e5c07b"))
  "Face for blocked-mission tensions."
  :group 'arxana-lab)

(defface arxana-lab-tension-structural-face
  '((t :foreground "#c678dd"))
  "Face for structural-invalid tensions."
  :group 'arxana-lab)

(defun arxana-browser--tension-type-face (tension-type)
  "Return face for TENSION-TYPE keyword or string."
  (let ((tt (if (keywordp tension-type)
                (substring (symbol-name tension-type) 1)
              (or tension-type ""))))
    (cond
     ((string= tt "uncovered-component") 'arxana-lab-tension-uncovered-face)
     ((string= tt "blocked-mission") 'arxana-lab-tension-blocked-face)
     ((string= tt "structural-invalid") 'arxana-lab-tension-structural-face)
     (t 'default))))

(defun arxana-browser--tension-type-label (tension-type)
  "Short label for TENSION-TYPE."
  (let ((tt (if (keywordp tension-type)
                (substring (symbol-name tension-type) 1)
              (or tension-type ""))))
    (cond
     ((string= tt "uncovered-component") "UNCOVER")
     ((string= tt "blocked-mission") "BLOCKED")
     ((string= tt "structural-invalid") "STRUCT")
     (t (upcase (arxana-lab--truncate tt 7))))))

(defun arxana-browser--tensions-format ()
  "Column format for tensions view."
  [("Type" 8 t)
   ("Devmap" 22 t)
   ("Component" 20 t)
   ("Cov%" 5 t)
   ("Summary" 0 nil)])

(defun arxana-browser--tensions-row (item)
  "Row for tension ITEM."
  (let* ((ttype (or (plist-get item :tension/type) ""))
         (devmap (or (plist-get item :tension/devmap) ""))
         (component (or (plist-get item :tension/component) ""))
         (coverage (plist-get item :tension/coverage-pct))
         (summary (or (plist-get item :tension/summary) ""))
         (label (arxana-browser--tension-type-label ttype))
         (face (arxana-browser--tension-type-face ttype)))
    (vector (propertize label 'face face)
            (arxana-lab--truncate (if (keywordp devmap)
                                      (substring (symbol-name devmap) 1)
                                    (format "%s" devmap))
                                  21)
            (arxana-lab--truncate (if (keywordp component)
                                      (substring (symbol-name component) 1)
                                    (format "%s" component))
                                  19)
            (if coverage (format "%.0f" (* 100.0 coverage)) "")
            (arxana-lab--truncate summary 60))))

(defun arxana-browser--tensions-items ()
  "Fetch tensions from futon3c Mission Control."
  (condition-case err
      (let* ((payload (arxana-browser--futon3c-fetch "/mc/tensions"))
             (tensions (plist-get payload :tensions)))
        (if tensions
            (mapcar (lambda (t-entry)
                      (append (list :type 'tension-entry) t-entry))
                    tensions)
          (list (list :type 'info
                      :label "No tensions detected"
                      :description "All devmap components are covered"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch tensions"
                 :description (format "Error: %s" (error-message-string err)))))))

(defun arxana-browser-tension-open-entry (item)
  "Open detail view for a tension ITEM."
  (let* ((buf-name "*Tension Detail*")
         (buf (get-buffer-create buf-name))
         (ttype (plist-get item :tension/type))
         (devmap (plist-get item :tension/devmap))
         (component (plist-get item :tension/component))
         (mission (plist-get item :tension/mission))
         (blocked-by (plist-get item :tension/blocked-by))
         (coverage (plist-get item :tension/coverage-pct))
         (detected (plist-get item :tension/detected-at))
         (summary (plist-get item :tension/summary)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "* Tension: " (or (format "%s" summary) "?") "\n\n")
        (insert (format "- Type :: %s\n" (or ttype "?")))
        (when devmap
          (insert (format "- Devmap :: %s\n" devmap)))
        (when component
          (insert (format "- Component :: %s\n" component)))
        (when mission
          (insert (format "- Mission :: %s\n" mission)))
        (when blocked-by
          (insert (format "- Blocked by :: %s\n" blocked-by)))
        (when coverage
          (insert (format "- Coverage :: %.0f%%\n" (* 100.0 coverage))))
        (when detected
          (insert (format "- Detected :: %s\n" detected)))
        (insert "\n** Actions\n\n")
        (insert "- [ ] Propose mission to address this tension\n")
        (insert "- [ ] Investigate related evidence\n")
        (insert "- [ ] Browse devmap prototype\n")
        (view-mode 1)))
    (display-buffer buf)))

;; =============================================================================
;; Devmap browser view
;; =============================================================================

(defface arxana-lab-devmap-active-face
  '((t :foreground "#61afef"))
  "Face for active devmaps."
  :group 'arxana-lab)

(defface arxana-lab-devmap-complete-face
  '((t :foreground "#98c379"))
  "Face for complete devmaps."
  :group 'arxana-lab)

(defun arxana-browser--devmaps-format ()
  "Column format for devmaps view."
  [("State" 9 t)
   ("Devmap" 28 t)
   ("Comps" 6 t)
   ("Edges" 6 t)
   ("I/O" 6 nil)
   ("Valid" 6 nil)])

(defun arxana-browser--devmaps-row (item)
  "Row for devmap ITEM."
  (let* ((dm-id (plist-get item :devmap/id))
         (name (if (keywordp dm-id)
                   (substring (symbol-name dm-id) 1)
                 (format "%s" dm-id)))
         (state (plist-get item :devmap/state))
         (state-str (if (keywordp state)
                        (substring (symbol-name state) 1)
                      (format "%s" state)))
         (face (cond
                ((string= state-str "complete") 'arxana-lab-devmap-complete-face)
                ((string= state-str "active") 'arxana-lab-devmap-active-face)
                (t 'default)))
         (comp-count (or (plist-get item :devmap/component-count) 0))
         (edge-count (or (plist-get item :devmap/edge-count) 0))
         (in-count (or (plist-get item :devmap/input-count) 0))
         (out-count (or (plist-get item :devmap/output-count) 0))
         (valid (plist-get item :devmap/all-valid)))
    (vector (propertize (upcase state-str) 'face face)
            name
            (format "%d" comp-count)
            (format "%d" edge-count)
            (format "%d/%d" in-count out-count)
            (if valid "ok" "FAIL"))))

(defun arxana-browser--devmaps-items ()
  "Fetch devmap summaries from futon3c Mission Control."
  (condition-case err
      (let ((devmaps (arxana-browser--fetch-devmap-summaries)))
        (if devmaps
            (mapcar (lambda (dm)
                      (append (list :type 'devmap-entry) dm))
                    devmaps)
          (list (list :type 'info
                      :label "No devmaps found"
                      :description "futon5/data/missions/ may be empty"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch devmaps"
                 :description (format "Error: %s" (error-message-string err)))))))

(defun arxana-browser-devmap-open-entry (item)
  "Open detail view for a devmap ITEM."
  (let* ((buf-name "*Devmap Detail*")
         (dm-id (plist-get item :devmap/id))
         (name (if (keywordp dm-id)
                   (substring (symbol-name dm-id) 1)
                 (format "%s" dm-id)))
         (state (plist-get item :devmap/state))
         (components (plist-get item :devmap/components))
         (comp-count (or (plist-get item :devmap/component-count) 0))
         (edge-count (or (plist-get item :devmap/edge-count) 0))
         (in-count (or (plist-get item :devmap/input-count) 0))
         (out-count (or (plist-get item :devmap/output-count) 0))
         (valid (plist-get item :devmap/all-valid))
         (failed (plist-get item :devmap/failed-checks))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Devmap: %s\n" name))
        (insert (make-string 60 ?=) "\n\n")
        (insert (format "  State:       %s\n" (if (keywordp state)
                                                   (substring (symbol-name state) 1)
                                                 state)))
        (insert (format "  Components:  %d\n" comp-count))
        (insert (format "  Edges:       %d\n" edge-count))
        (insert (format "  Inputs:      %d\n" in-count))
        (insert (format "  Outputs:     %d\n" out-count))
        (insert (format "  Valid:       %s\n" (if valid "yes" "NO")))
        (when (and failed (> (length failed) 0))
          (insert (format "  Failures:    %s\n" failed)))
        (insert "\nComponents:\n")
        (insert (make-string 40 ?-) "\n")
        (dolist (c components)
          (let ((cid (plist-get c :component/id))
                (cname (plist-get c :component/name)))
            (insert (format "  %s  %s\n"
                            (if (keywordp cid)
                                (substring (symbol-name cid) 1)
                              (format "%s" cid))
                            (or cname "")))))
        (insert "\n[i] Ingest as hyperedge  [q] Quit\n")
        (goto-char (point-min))
        (local-set-key (kbd "q") #'quit-window)
        (local-set-key (kbd "i")
                       (lambda ()
                         (interactive)
                         (let ((resp (arxana-browser-ingest-devmap-as-hyperedge item)))
                           (message (if resp "Devmap ingested as hyperedge" "Ingestion failed")))))
        (view-mode 1)))
    (display-buffer buf)))

;; =============================================================================
;; Narrative trail view (per-mission evidence story)
;; =============================================================================

(defun arxana-browser--narrative-trail-format ()
  "Column format for narrative trail view."
  [("Time" 18 t)
   ("Type" 7 t)
   ("Author" 12 t)
   ("Gates" 8 nil)
   ("Preview" 0 nil)])

(defun arxana-browser--narrative-trail-row (item)
  "Row for narrative trail ITEM."
  (let* ((at (or (plist-get item :evidence/at) ""))
         (etype (plist-get item :evidence/type))
         (author (or (plist-get item :evidence/author) ""))
         (body (plist-get item :evidence/body))
         (gates (plist-get body :mission/gates))
         (type-label (arxana-lab--evidence-type-label etype))
         (type-face (arxana-lab--evidence-type-face etype))
         (type-display (if type-face
                           (propertize type-label 'face type-face)
                         type-label))
         (preview (arxana-lab--evidence-body-preview body etype))
         (gates-str (if (and gates (plist-get gates :checked))
                        (format "%d/%d"
                                (plist-get gates :checked)
                                (plist-get gates :total))
                      "")))
    (vector (arxana-lab--truncate at 17)
            type-display
            (arxana-lab--truncate author 11)
            gates-str
            (arxana-lab--truncate preview 50))))

(defun arxana-browser--narrative-trail-items (mission-id)
  "Fetch evidence entries for MISSION-ID as a narrative trail."
  (condition-case err
      (let* ((params (list (cons "subject-id" mission-id)
                           (cons "limit" "200")))
             (response (arxana-browser--evidence-fetch params))
             (entries (plist-get response :entries)))
        (if entries
            (mapcar (lambda (e)
                      (append (list :type 'evidence-entry) e))
                    entries)
          (list (list :type 'info
                      :label (format "No evidence for mission %s" mission-id)
                      :description "Backfill may not have been run yet"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch narrative trail"
                 :description (format "Error: %s" (error-message-string err)))))))

(defun arxana-browser-open-narrative-trail (mission-id)
  "Open a narrative trail for MISSION-ID in the browser."
  (interactive (list (read-string "Mission ID: ")))
  (let ((buffer (get-buffer-create arxana-browser--buffer)))
    (with-current-buffer buffer
      (setq arxana-browser--stack
            (cons (list :view 'narrative-trail
                        :label (format "Trail: %s" mission-id)
                        :mission-id mission-id)
                  arxana-browser--stack))))
  (arxana-browser--render))

;; =============================================================================
;; Tension → Hyperedge ingestion bridge
;; =============================================================================

(defun arxana-browser-ingest-tension-as-hyperedge (tension)
  "Create an Arxana hyperedge from a TENSION entry.
Posts to futon1a via `arxana-store--post-hyperedge'.
Returns the response or nil on error."
  (let* ((ttype (plist-get tension :tension/type))
         (devmap (plist-get tension :tension/devmap))
         (component (plist-get tension :tension/component))
         (mission (plist-get tension :tension/mission))
         (summary (plist-get tension :tension/summary))
         (detected (plist-get tension :tension/detected-at))
         (hx-type (format "tension/%s"
                          (if (keywordp ttype)
                              (substring (symbol-name ttype) 1)
                            (format "%s" ttype))))
         (endpoints (delq nil
                          (list (when devmap
                                  (format "devmap:%s"
                                          (if (keywordp devmap)
                                              (substring (symbol-name devmap) 1)
                                            devmap)))
                                (when component
                                  (format "component:%s/%s"
                                          (if (keywordp devmap)
                                              (substring (symbol-name devmap) 1)
                                            devmap)
                                          (if (keywordp component)
                                              (substring (symbol-name component) 1)
                                            component)))
                                (when mission
                                  (format "mission:%s" mission)))))
         (props (delq nil
                      (list (when summary (cons 'summary summary))
                            (when detected (cons 'detected-at detected))))))
    (arxana-store--post-hyperedge "tension" hx-type endpoints props)))

(defun arxana-browser-ingest-all-tensions ()
  "Fetch all tensions from futon3c and ingest as Arxana hyperedges.
Returns a summary plist with :created and :failed counts."
  (interactive)
  (let* ((payload (arxana-browser--futon3c-fetch "/mc/tensions"))
         (tensions (or (plist-get payload :tensions) '()))
         (created 0)
         (failed 0))
    (dolist (t-entry tensions)
      (condition-case _err
          (let ((resp (arxana-browser-ingest-tension-as-hyperedge t-entry)))
            (if resp
                (setq created (1+ created))
              (setq failed (1+ failed))))
        (error (setq failed (1+ failed)))))
    (let ((msg (format "Tension ingestion: %d created, %d failed (of %d)"
                       created failed (length tensions))))
      (when (called-interactively-p 'interactive)
        (message msg))
      (list :created created :failed failed :total (length tensions)))))

;; =============================================================================
;; Devmap → Hyperedge ingestion bridge
;; =============================================================================

(defun arxana-browser--futon3c-post (endpoint payload)
  "POST PAYLOAD to ENDPOINT on the futon3c server. Returns parsed JSON plist."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Accept" . "application/json")
                                      ("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (base (string-remove-suffix "/" arxana-lab-futon3c-server))
         (url (concat base endpoint))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        nil
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (result (arxana-lab--parse-json body)))
          (kill-buffer buffer)
          result)))))

(defun arxana-browser--fetch-devmap-summaries ()
  "Fetch devmap summaries from futon3c via mission-control review.
Returns a list of devmap plists."
  (let* ((result (arxana-browser--futon3c-post
                  "/mission-control"
                  '((action . "review"))))
         (lr (plist-get result :last-result))
         (review (plist-get lr :result)))
    (plist-get review :portfolio/devmap-summaries)))

(defun arxana-browser-ingest-devmap-as-hyperedge (devmap)
  "Create an Arxana hyperedge from a DEVMAP summary.
Posts to futon1a via `arxana-store--post-hyperedge'.
Returns the response or nil on error."
  (let* ((dm-id (plist-get devmap :devmap/id))
         (dm-name (if (keywordp dm-id)
                      (substring (symbol-name dm-id) 1)
                    (format "%s" dm-id)))
         (state (plist-get devmap :devmap/state))
         (comp-count (plist-get devmap :devmap/component-count))
         (edge-count (plist-get devmap :devmap/edge-count))
         (input-count (plist-get devmap :devmap/input-count))
         (output-count (plist-get devmap :devmap/output-count))
         (all-valid (plist-get devmap :devmap/all-valid))
         (components (plist-get devmap :devmap/components))
         ;; Build endpoints: devmap ID + each component
         (endpoints (cons (format "devmap:%s" dm-name)
                          (mapcar (lambda (c)
                                    (let ((cid (plist-get c :component/id)))
                                      (format "component:%s/%s"
                                              dm-name
                                              (if (keywordp cid)
                                                  (substring (symbol-name cid) 1)
                                                (format "%s" cid)))))
                                  components)))
         (props (list (cons 'state (if (keywordp state)
                                       (substring (symbol-name state) 1)
                                     (format "%s" state)))
                      (cons 'component-count comp-count)
                      (cons 'edge-count edge-count)
                      (cons 'input-count input-count)
                      (cons 'output-count output-count)
                      (cons 'all-valid (if all-valid "true" "false")))))
    (arxana-store--post-hyperedge "devmap" "devmap/prototype" endpoints props)))

(defun arxana-browser-ingest-all-devmaps ()
  "Fetch all devmap summaries from futon3c and ingest as Arxana hyperedges.
Returns a summary plist with :created and :failed counts."
  (interactive)
  (let* ((devmaps (arxana-browser--fetch-devmap-summaries))
         (created 0)
         (failed 0))
    (dolist (dm devmaps)
      (condition-case _err
          (let ((resp (arxana-browser-ingest-devmap-as-hyperedge dm)))
            (if resp
                (setq created (1+ created))
              (setq failed (1+ failed))))
        (error (setq failed (1+ failed)))))
    (let ((msg (format "Devmap ingestion: %d created, %d failed (of %d)"
                       created failed (length devmaps))))
      (when (called-interactively-p 'interactive)
        (message msg))
      (list :created created :failed failed :total (length devmaps)))))

;; =============================================================================
;; Reflection grounding — about-var relations
;; =============================================================================

(defun arxana-browser--resolve-var (ns-name var-name)
  "Resolve a Clojure var via futon3c reflection API.
Returns a plist with :reflection/ns, :reflection/file, :reflection/line, etc.
or nil if not found."
  (let* ((endpoint (format "/reflect/var/%s/%s"
                           (url-hexify-string ns-name)
                           (url-hexify-string var-name)))
         (payload (arxana-browser--futon3c-fetch endpoint)))
    (when (and payload (plist-get payload :ok))
      (plist-get payload :envelope))))

(defun arxana-browser-ground-claim-to-var (claim-id ns-name var-name)
  "Create an about-var hyperedge linking CLAIM-ID to var NS-NAME/VAR-NAME.
Resolves the var via reflection API and stores the reflection envelope
as hyperedge props. Returns the response or nil."
  (interactive
   (list (read-string "Claim/evidence ID: ")
         (read-string "Namespace: ")
         (read-string "Var name: ")))
  (let ((envelope (arxana-browser--resolve-var ns-name var-name)))
    (unless envelope
      (user-error "Could not resolve var %s/%s" ns-name var-name))
    (let* ((var-id (format "var:%s/%s" ns-name var-name))
           (props (delq nil
                        (list (cons 'reflection/ns
                                    (format "%s" (or (plist-get envelope :reflection/ns) "")))
                              (cons 'reflection/symbol
                                    (format "%s" (or (plist-get envelope :reflection/symbol) "")))
                              (cons 'reflection/file
                                    (or (plist-get envelope :reflection/file) ""))
                              (cons 'reflection/line
                                    (plist-get envelope :reflection/line))
                              (cons 'reflection/arglists
                                    (format "%s" (or (plist-get envelope :reflection/arglists) "")))
                              (when (plist-get envelope :reflection/doc)
                                (cons 'reflection/doc
                                      (plist-get envelope :reflection/doc)))
                              (cons 'resolved-at
                                    (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))))
           (resp (arxana-store--post-hyperedge
                  "about-var" "reflection/about-var"
                  (list claim-id var-id) props)))
      (when (called-interactively-p 'interactive)
        (message "Grounded %s → %s/%s (line %s)"
                 claim-id ns-name var-name
                 (or (plist-get envelope :reflection/line) "?")))
      resp)))

(defun arxana-browser-verify-reflection-snapshot (hyperedge)
  "Re-resolve a var from an about-var HYPEREDGE and check for staleness.
HYPEREDGE is an alist with :hx/props containing reflection/* fields.
Returns :ok, :stale (signature changed), or :missing (var gone)."
  (let* ((props (cdr (assq :hx/props hyperedge)))
         (ns-name (or (cdr (assq :reflection/ns props))
                      (cdr (assq 'reflection/ns props)) ""))
         (var-name (or (cdr (assq :reflection/symbol props))
                       (cdr (assq 'reflection/symbol props)) ""))
         (old-line (or (cdr (assq :reflection/line props))
                       (cdr (assq 'reflection/line props))))
         (old-arglists (or (cdr (assq :reflection/arglists props))
                           (cdr (assq 'reflection/arglists props)))))
    (let ((envelope (arxana-browser--resolve-var
                     (format "%s" ns-name) (format "%s" var-name))))
      (cond
       ((not envelope) :missing)
       ((or (not (equal old-line (plist-get envelope :reflection/line)))
            (not (equal old-arglists
                        (format "%s" (or (plist-get envelope :reflection/arglists) "")))))
        :stale)
       (t :ok)))))

(provide 'arxana-browser-lab)
;;; arxana-browser-lab.el ends here
