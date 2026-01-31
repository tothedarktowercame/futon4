;;; arxana-browser-forum.el --- Forum browser views -*- lexical-binding: t; -*-

;;; Commentary:
;; Forum integration for the Arxana browser. Uses WebSockets for live updates.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-util)

(defvar arxana-browser--buffer)
(defvar arxana-browser--stack)
(defvar arxana-browser--context)

(declare-function arxana-browser--render "arxana-browser-core")
(declare-function arxana-browser--ensure-context "arxana-browser-core")

(defgroup arxana-forum nil
  "Forum integration for Arxana."
  :group 'arxana)

(defcustom arxana-forum-server
  (or (getenv "FORUM_SERVER") "http://localhost:5050")
  "Forum HTTP base URL."
  :type 'string
  :group 'arxana-forum)

(defcustom arxana-forum-ws-server nil
  "Forum WebSocket base URL.
When nil, derive from `arxana-forum-server`."
  :type '(choice (const :tag "Derived" nil)
                 string)
  :group 'arxana-forum)

(defcustom arxana-forum-author
  (or (getenv "FORUM_AUTHOR") (user-login-name))
  "Default author name for forum posts."
  :type 'string
  :group 'arxana-forum)

(defcustom arxana-forum-request-timeout 10
  "Number of seconds to wait for forum HTTP responses."
  :type 'integer
  :group 'arxana-forum)

(defcustom arxana-forum-thread-limit 100
  "Maximum number of threads to fetch for the forum view."
  :type 'integer
  :group 'arxana-forum)

(defvar-local arxana-forum--thread-id nil)
(defvar-local arxana-forum--thread-title nil)
(defvar-local arxana-forum--posts nil)
(defvar-local arxana-forum--post-ids nil)
(defvar-local arxana-forum--ws nil)
(defvar-local arxana-forum--ws-thread nil)

(defvar arxana-forum--compose-buffer "*Arxana Forum Compose*")

(defun arxana-forum--require-websocket ()
  (unless (featurep 'websocket)
    (require 'websocket nil t))
  (unless (featurep 'websocket)
    (user-error "websocket.el is required for forum live updates")))

(defun arxana-forum--normalize-base (base)
  (string-remove-suffix "/" (or base "")))

(defun arxana-forum--derive-ws-server ()
  (let ((base (arxana-forum--normalize-base arxana-forum-server)))
    (cond
     ((string-prefix-p "wss://" base) base)
     ((string-prefix-p "ws://" base) base)
     ((string-prefix-p "https://" base)
      (concat "wss://" (substring base (length "https://")) "/forum/stream/ws"))
     ((string-prefix-p "http://" base)
      (concat "ws://" (substring base (length "http://")) "/forum/stream/ws"))
     (t (concat "ws://" base "/forum/stream/ws")))))

(defun arxana-forum--ws-base ()
  (arxana-forum--normalize-base (or arxana-forum-ws-server
                                    (arxana-forum--derive-ws-server))))

(defun arxana-forum--ws-url (&optional thread-id)
  (let ((base (arxana-forum--ws-base)))
    (if (and thread-id (not (string-empty-p thread-id)))
        (concat base (if (string-match-p "\\?" base) "&" "?")
                "thread-id=" (url-hexify-string thread-id))
      base)))

(defun arxana-forum--parse-json (body)
  (when (and body (stringp body) (not (string-empty-p body)))
    (condition-case _err
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

(defun arxana-forum--get (plist key)
  (or (plist-get plist key)
      (when (keywordp key)
        (plist-get plist (intern (substring (symbol-name key) 1))))))

(defun arxana-forum--request (method path &optional data)
  (let* ((url-request-method method)
         (url-request-extra-headers
          (append '(("Accept" . "application/json"))
                  (when data '(("Content-Type" . "application/json")))))
         (url-request-data (when data
                             (encode-coding-string (json-encode data) 'utf-8)))
         (url (concat (arxana-forum--normalize-base arxana-forum-server) path))
         (buffer (url-retrieve-synchronously url t t arxana-forum-request-timeout)))
    (unless buffer
      (user-error "Forum request failed: %s %s" method url))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "\n\n" nil 'move)
      (let ((body (buffer-substring-no-properties (point) (point-max))))
        (kill-buffer buffer)
        (arxana-forum--parse-json body)))))

(defun arxana-forum--truncate (text max-len)
  (let ((value (or text "")))
    (if (> (length value) max-len)
        (concat (substring value 0 (max 0 (- max-len 3))) "...")
      value)))

(defun arxana-forum--format-tags (tags)
  (when tags
    (mapconcat (lambda (tag)
                 (if (keywordp tag)
                     (substring (symbol-name tag) 1)
                   (format "%s" tag)))
               tags
               ", ")))

(defun arxana-forum--thread-summary (thread)
  (let ((title (or (arxana-forum--get thread :thread/title) ""))
        (author (or (arxana-forum--get thread :thread/author) ""))
        (updated (or (arxana-forum--get thread :thread/updated) ""))
        (count (or (arxana-forum--get thread :thread/post-count) 0))
        (tags (arxana-forum--format-tags (arxana-forum--get thread :thread/tags)))
        (pinned? (arxana-forum--get thread :thread/pinned?)))
    (list :type 'forum-thread
          :thread-id (arxana-forum--get thread :thread/id)
          :label title
          :author author
          :updated updated
          :count count
          :tags tags
          :pinned? pinned?)))

(defun arxana-forum--post-summary (post)
  (let ((author (or (arxana-forum--get post :post/author) ""))
        (timestamp (or (arxana-forum--get post :post/timestamp) ""))
        (claim (or (arxana-forum--get post :post/claim-type) "step"))
        (pattern (arxana-forum--get post :post/pattern-applied))
        (body (or (arxana-forum--get post :post/body) "")))
    (list :type 'forum-post
          :post-id (arxana-forum--get post :post/id)
          :author author
          :timestamp timestamp
          :claim-type claim
          :pattern pattern
          :body body)))

(defun arxana-forum--fetch-threads ()
  (let* ((response (arxana-forum--request "GET"
                                          (format "/forum/threads?limit=%d"
                                                  arxana-forum-thread-limit)))
         (threads (arxana-forum--get response :threads)))
    (mapcar #'arxana-forum--thread-summary (or threads '()))))

(defun arxana-forum--fetch-thread (thread-id)
  (let* ((response (arxana-forum--request "GET"
                                          (format "/forum/thread/%s" thread-id)))
         (thread (arxana-forum--get response :thread))
         (posts (arxana-forum--get response :posts)))
    (list thread posts)))

(defun arxana-forum--set-thread (thread-id)
  (let* ((result (arxana-forum--fetch-thread thread-id))
         (thread (car result))
         (posts (cadr result)))
    (setq arxana-forum--thread-id thread-id
          arxana-forum--thread-title (arxana-forum--get thread :thread/title)
          arxana-forum--posts (mapcar #'arxana-forum--post-summary (or posts '()))
          arxana-forum--post-ids (mapcar (lambda (p) (arxana-forum--get p :post-id))
                                         arxana-forum--posts))
    thread))

(defun arxana-forum--maybe-render ()
  (when (and (boundp 'arxana-browser--context)
             (let ((context (or arxana-browser--context (car arxana-browser--stack))))
               (and context
                    (eq (plist-get context :view) 'forum-thread)
                    (equal (plist-get context :thread-id) arxana-forum--thread-id))))
    (arxana-browser--render)))

(defun arxana-forum--ingest-post (post)
  (let* ((thread-id (arxana-forum--get post :post/thread-id))
         (post-id (arxana-forum--get post :post/id)))
    (when (and thread-id post-id
               (equal thread-id arxana-forum--thread-id)
               (not (member post-id arxana-forum--post-ids)))
      (push post-id arxana-forum--post-ids)
      (setq arxana-forum--posts
            (sort (append arxana-forum--posts
                          (list (arxana-forum--post-summary post)))
                  (lambda (a b)
                    (string< (or (plist-get a :timestamp) "")
                             (or (plist-get b :timestamp) "")))))
      (arxana-forum--maybe-render))))

(defun arxana-forum--handle-message (payload)
  (let* ((msg (arxana-forum--parse-json payload))
         (type (arxana-forum--get msg :type)))
    (pcase type
      ("init"
       (dolist (post (arxana-forum--get msg :recent-posts))
         (arxana-forum--ingest-post post)))
      ("post-created"
       (let ((post (arxana-forum--get msg :post)))
         (when post
           (arxana-forum--ingest-post post))))
      (_ nil))))

(defun arxana-forum--connect (thread-id)
  (arxana-forum--require-websocket)
  (when (and arxana-forum--ws
             (not (equal arxana-forum--ws-thread thread-id)))
    (ignore-errors (websocket-close arxana-forum--ws))
    (setq arxana-forum--ws nil
          arxana-forum--ws-thread nil))
  (unless arxana-forum--ws
    (let ((url (arxana-forum--ws-url thread-id)))
      (setq arxana-forum--ws-thread thread-id
            arxana-forum--ws
            (websocket-open
             url
             :on-message (lambda (_ws frame)
                           (let ((payload (websocket-frame-payload frame)))
                             (when (stringp payload)
                               (arxana-forum--handle-message payload))))
             :on-close (lambda (_ws) (setq arxana-forum--ws nil))
             :on-error (lambda (_ws err)
                         (message "[arxana-forum] websocket error: %s" err)))))))

(defun arxana-forum-disconnect ()
  "Disconnect any active forum WebSocket."
  (interactive)
  (when arxana-forum--ws
    (ignore-errors (websocket-close arxana-forum--ws)))
  (setq arxana-forum--ws nil
        arxana-forum--ws-thread nil))

;; =============================================================================
;; Browser integration
;; =============================================================================

(defun arxana-browser--forum-format ()
  [("Thread" 12 t)
   ("Title" 45 t)
   ("Author" 12 t)
   ("Posts" 7 nil)
   ("Updated" 20 t)
   ("Tags" 0 nil)])

(defun arxana-browser--forum-row (item)
  (let ((thread-id (or (plist-get item :thread-id) ""))
        (title (or (plist-get item :label) ""))
        (author (or (plist-get item :author) ""))
        (count (number-to-string (or (plist-get item :count) 0)))
        (updated (or (plist-get item :updated) ""))
        (tags (or (plist-get item :tags) ""))
        (pinned? (plist-get item :pinned?)))
    (vector (if pinned? (concat "*" thread-id) thread-id)
            (arxana-forum--truncate title 44)
            (arxana-forum--truncate author 12)
            count
            (arxana-forum--truncate updated 20)
            tags)))

(defun arxana-browser--forum-thread-format ()
  [("Author" 12 t)
   ("Type" 10 t)
   ("Time" 20 t)
   ("Body" 0 nil)])

(defun arxana-browser--forum-thread-row (item)
  (let ((author (or (plist-get item :author) ""))
        (claim (or (plist-get item :claim-type) "step"))
        (timestamp (or (plist-get item :timestamp) ""))
        (pattern (plist-get item :pattern))
        (body (or (plist-get item :body) "")))
    (vector (arxana-forum--truncate author 12)
            (arxana-forum--truncate
             (if pattern
                 (format "%s/%s" claim pattern)
               (format "%s" claim))
             10)
            (arxana-forum--truncate timestamp 20)
            (arxana-forum--truncate body 120))))

(defun arxana-browser--forum-items ()
  (condition-case err
      (arxana-forum--fetch-threads)
    (error
     (list (list :type 'info
                 :message (format "Forum unavailable: %s" (error-message-string err))
                 :label "Forum error"
                 :description "Check arxana-forum-server")))))

(defun arxana-browser--forum-thread-items (context)
  (let ((thread-id (plist-get context :thread-id)))
    (unless (and thread-id (equal thread-id arxana-forum--thread-id))
      (arxana-forum--set-thread thread-id))
    (arxana-forum--connect thread-id)
    (or arxana-forum--posts '())))

(defun arxana-forum-open-thread (item)
  "Open a forum thread ITEM in the Arxana browser."
  (let ((thread-id (plist-get item :thread-id))
        (label (or (plist-get item :label) (plist-get item :thread-id))))
    (unless thread-id
      (user-error "No thread id found"))
    (arxana-forum--set-thread thread-id)
    (setq arxana-browser--stack
          (cons (list :view 'forum-thread
                      :label label
                      :thread-id thread-id)
                arxana-browser--stack))
    (arxana-browser--render)))

;; =============================================================================
;; Compose / reply
;; =============================================================================

(defvar arxana-forum-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'arxana-forum-compose-send)
    (define-key map (kbd "C-c C-k") #'arxana-forum-compose-abort)
    map)
  "Keymap for `arxana-forum-compose-mode'.")

(define-derived-mode arxana-forum-compose-mode text-mode "Arxana-Forum-Compose"
  "Mode for composing forum replies."
  (setq-local header-line-format "C-c C-c to send, C-c C-k to cancel"))

(defvar-local arxana-forum--compose-thread-id nil)

(defun arxana-forum-compose (&optional thread-id)
  "Compose a reply to THREAD-ID."
  (interactive)
  (let ((thread-id (or thread-id
                       arxana-forum--thread-id
                       (read-string "Thread id: "))))
    (let ((buf (get-buffer-create arxana-forum--compose-buffer)))
      (with-current-buffer buf
        (erase-buffer)
        (arxana-forum-compose-mode)
        (setq arxana-forum--compose-thread-id thread-id))
      (pop-to-buffer buf)
      (message "Compose reply for %s" thread-id))))

(defun arxana-forum-compose-for-current-thread ()
  "Compose a reply for the currently viewed forum thread."
  (interactive)
  (arxana-browser--ensure-context)
  (let* ((context (or arxana-browser--context (car arxana-browser--stack)))
         (thread-id (plist-get context :thread-id)))
    (unless (and context (eq (plist-get context :view) 'forum-thread) thread-id)
      (user-error "Not currently viewing a forum thread"))
    (arxana-forum-compose thread-id)))

(defun arxana-forum-compose-send ()
  "Send the current compose buffer as a forum reply."
  (interactive)
  (let ((thread-id arxana-forum--compose-thread-id)
        (body (string-trim (buffer-string))))
    (unless (and thread-id (not (string-empty-p body)))
      (user-error "Thread id or body missing"))
    (arxana-forum--request
     "POST"
     (format "/forum/thread/%s/reply" thread-id)
     (list :author arxana-forum-author
           :body body))
    (let ((buf (current-buffer)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))
    (message "Posted to %s" thread-id)))

(defun arxana-forum-compose-abort ()
  "Abort composing a forum reply."
  (interactive)
  (let ((buf (current-buffer)))
    (when (buffer-live-p buf)
      (kill-buffer buf))
    (message "Compose aborted")))

(provide 'arxana-browser-forum)
;;; arxana-browser-forum.el ends here
