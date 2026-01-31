;;; arxana-browser-lab.el --- Lab browser views -*- lexical-binding: t; -*-

;;; Commentary:
;; Lab browsing helpers for the Arxana browser.
;; Includes active session viewing via Claude stream and archived session browsing.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

(require 'arxana-lab)

(defvar arxana-browser--stack)
(defvar arxana-browser--context)
(defvar arxana-browser--buffer)

(declare-function arxana-browser--item-at-point "arxana-browser-core")
(declare-function arxana-browser--render "arxana-browser-core")
(declare-function fuclient-claude-stream-connect "fuclient-claude-stream")
(declare-function arxana-browser-patterns--ensure-frame "arxana-browser-patterns")

(defgroup arxana-lab-sessions nil
  "Lab session browsing."
  :group 'arxana)

(defcustom arxana-lab-sessions-server
  (or (getenv "FUTON3_SERVER") "http://localhost:5050")
  "Futon3 HTTP server URL for fetching session lists."
  :type 'string
  :group 'arxana-lab-sessions)

(defcustom arxana-lab-sessions-request-timeout 10
  "Timeout in seconds for session list requests."
  :type 'integer
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

(defun arxana-lab--fetch-sessions (endpoint)
  "Fetch sessions from ENDPOINT (e.g., /fulab/lab/sessions/active)."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (url (concat (string-remove-suffix "/" arxana-lab-sessions-server) endpoint))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (unless buffer
      (user-error "Failed to fetch sessions from %s" url))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "\n\n" nil 'move)
      (let ((body (buffer-substring-no-properties (point) (point-max))))
        (kill-buffer buffer)
        (arxana-lab--parse-json body)))))

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
              :label "Active Sessions"
              :description "Currently running Claude Code sessions"
              :view 'lab-sessions-active)
        (list :type 'lab-menu
              :label "Recent Sessions"
              :description "All sessions in ~/.claude/projects/"
              :view 'lab-sessions-recent)
        (list :type 'lab-menu
              :label "Archived Sessions"
              :description "Exported sessions in lab/raw"
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
   ("Size" 8 nil)
   ("Active" 6 nil)
   ("Modified" 20 t)])

(defun arxana-browser--lab-sessions-active-row (item)
  "Row for active session ITEM."
  (let ((id (or (plist-get item :id) ""))
        (project (or (plist-get item :project) ""))
        (size (format "%dkb" (or (plist-get item :size-kb) 0)))
        (active (if (plist-get item :active) "●" ""))
        (modified (or (plist-get item :modified) "")))
    (vector (arxana-lab--truncate id 36)
            (arxana-lab--truncate project 24)
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
                      :description "Start a Claude Code session to see it here"))))
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
                      :description "No Claude Code sessions in ~/.claude/projects/"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch sessions"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Archived Sessions view
;; =============================================================================

(defun arxana-browser--lab-sessions-archived-format ()
  "Column format for archived sessions view."
  [("Session ID" 40 t)
   ("Size" 8 nil)
   ("Modified" 20 t)])

(defun arxana-browser--lab-sessions-archived-row (item)
  "Row for archived session ITEM."
  (let ((id (or (plist-get item :id) ""))
        (size (format "%dkb" (or (plist-get item :size-kb) 0)))
        (modified (or (plist-get item :modified) "")))
    (vector (arxana-lab--truncate id 38)
            size
            (arxana-lab--truncate modified 19))))

(defun arxana-browser--lab-sessions-archived-items ()
  "Fetch and return archived session items."
  (condition-case err
      (let* ((response (arxana-lab--fetch-sessions "/fulab/lab/sessions/archived"))
             (sessions (plist-get response :sessions)))
        (if sessions
            (mapcar (lambda (s)
                      (append (list :type 'lab-session-archived) s))
                    sessions)
          (list (list :type 'info
                      :label "No archived sessions"
                      :description "Export sessions to lab/raw to see them here"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch sessions"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Session actions
;; =============================================================================

(defun arxana-browser-lab-view-session (item)
  "View (stream) the session ITEM."
  (let ((path (plist-get item :path)))
    (unless path
      (user-error "No path for session"))
    (if (featurep 'fuclient-claude-stream)
        (fuclient-claude-stream-connect path)
      (if (require 'fuclient-claude-stream nil t)
          (fuclient-claude-stream-connect path)
        (user-error "fuclient-claude-stream not available")))))

(defun arxana-browser-lab-open-session (item)
  "Open a lab session ITEM - dispatch based on type."
  (let ((type (plist-get item :type)))
    (pcase type
      ('lab-session-active
       (arxana-browser-lab-view-session item))
      ('lab-session-archived
       ;; For archived, could open the JSON or view if converted
       (let ((path (plist-get item :path)))
         (if path
             (find-file path)
           (user-error "No path for session"))))
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

(provide 'arxana-browser-lab)
;;; arxana-browser-lab.el ends here
