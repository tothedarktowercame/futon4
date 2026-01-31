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

(defcustom arxana-forum-stream-auto-reconnect t
  "When non-nil, reconnect forum WebSocket after disconnects."
  :type 'boolean
  :group 'arxana-forum)

(defcustom arxana-forum-stream-reconnect-delay 3
  "Seconds to wait before reconnecting to the forum WebSocket."
  :type 'integer
  :group 'arxana-forum)

(defcustom arxana-forum-stream-auto-focus nil
  "When non-nil, focus the forum stream buffer on connect."
  :type 'boolean
  :group 'arxana-forum)

(defcustom arxana-forum-stream-show-status-messages nil
  "When non-nil, append connection status messages to the stream buffer."
  :type 'boolean
  :group 'arxana-forum)

(defvar arxana-forum-stream--buffer-prefix "*Arxana Forum*")
(defvar arxana-forum-stream--websocket nil)
(defvar arxana-forum-stream--current-thread-id nil)
(defvar arxana-forum-stream--ping-timer nil)
(defvar arxana-forum-stream--reconnect-timer nil)

(defvar-local arxana-forum-stream--thread-id nil)
(defvar-local arxana-forum-stream--post-ids nil)
(defvar-local arxana-forum-stream--depths nil)
(defvar-local arxana-forum-stream--post-map nil)

(defvar arxana-forum--compose-buffer "*Arxana Forum Compose*")

(defface arxana-forum-stream-author-face
  '((t :inherit font-lock-keyword-face))
  "Face for forum author names."
  :group 'arxana-forum)

(defface arxana-forum-stream-timestamp-face
  '((t :inherit font-lock-comment-face))
  "Face for forum timestamps."
  :group 'arxana-forum)

(defface arxana-forum-stream-meta-face
  '((t :inherit font-lock-doc-face))
  "Face for forum metadata."
  :group 'arxana-forum)

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

(defun arxana-forum--get (obj key)
  (let* ((key-sym (and (keywordp key)
                       (intern (substring (symbol-name key) 1))))
         (key-str (cond
                   ((keywordp key) (substring (symbol-name key) 1))
                   ((symbolp key) (symbol-name key))
                   ((stringp key) key)
                   (t nil))))
    (or (and (listp obj) (plist-member obj key) (plist-get obj key))
        (and key-sym (listp obj) (plist-member obj key-sym) (plist-get obj key-sym))
        (and key-str (listp obj) (plist-member obj key-str) (plist-get obj key-str))
        (and (listp obj) (consp (car-safe obj))
             (or (cdr (assoc key obj))
                 (and key-sym (cdr (assoc key-sym obj)))
                 (and key-str (cdr (assoc key-str obj)))))
        nil)))

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

;; =============================================================================
;; Forum stream (append-only thread view)
;; =============================================================================

(defun arxana-forum-stream--buffer-name (thread-id)
  (format "%s %s*" arxana-forum-stream--buffer-prefix thread-id))

(defun arxana-forum-stream--get-buffer (thread-id)
  "Get or create the forum stream buffer for THREAD-ID."
  (let ((buf (get-buffer-create (arxana-forum-stream--buffer-name thread-id))))
    (with-current-buffer buf
      (unless (eq major-mode 'arxana-forum-stream-mode)
        (arxana-forum-stream-mode))
      (setq-local arxana-forum-stream--thread-id thread-id)
      (unless (hash-table-p arxana-forum-stream--post-ids)
        (setq-local arxana-forum-stream--post-ids (make-hash-table :test 'equal)))
      (unless (hash-table-p arxana-forum-stream--depths)
        (setq-local arxana-forum-stream--depths (make-hash-table :test 'equal)))
      (unless (hash-table-p arxana-forum-stream--post-map)
        (setq-local arxana-forum-stream--post-map (make-hash-table :test 'equal))))
    buf))

(defun arxana-forum-stream--format-time (timestamp)
  (if (and timestamp (> (length timestamp) 19))
      (substring timestamp 11 19)
    (or timestamp "")))

(defun arxana-forum-stream--indent (depth)
  (make-string (* (max 0 depth) 2) ?\s))

(defun arxana-forum-stream--format-header (post depth)
  (let* ((timestamp (arxana-forum--get post :post/timestamp))
         (author (or (arxana-forum--get post :post/author) ""))
         (claim (or (arxana-forum--get post :post/claim-type) "step"))
         (pattern (arxana-forum--get post :post/pattern-applied))
         (reply-to (arxana-forum--get post :post/in-reply-to))
         (indent (arxana-forum-stream--indent depth))
         (meta (if pattern
                   (format "%s/%s" claim pattern)
                 (format "%s" claim)))
         (reply (when reply-to (format " reply %s" reply-to))))
    (concat indent
            (propertize (format "[%s] " (arxana-forum-stream--format-time timestamp))
                        'face 'arxana-forum-stream-timestamp-face)
            (propertize author 'face 'arxana-forum-stream-author-face)
            " "
            (propertize (format "(%s)" meta) 'face 'arxana-forum-stream-meta-face)
            (when reply (propertize reply 'face 'arxana-forum-stream-meta-face))
            "\n")))

(defun arxana-forum-stream--format-body (post depth)
  (let* ((body (or (arxana-forum--get post :post/body) ""))
         (indent (arxana-forum-stream--indent (1+ depth)))
         (lines (split-string body "\n" nil)))
    (mapconcat (lambda (line)
                 (concat indent line))
               lines
               "\n")))

(defun arxana-forum-stream--format-post (post depth)
  (concat (arxana-forum-stream--format-header post depth)
          (arxana-forum-stream--format-body post depth)
          "\n\n"))

(defun arxana-forum-stream--append (thread-id text)
  (let ((buf (arxana-forum-stream--get-buffer thread-id)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (at-end (= (point) (point-max))))
        (save-excursion
          (goto-char (point-max))
          (insert text))
        (when at-end
          (goto-char (point-max)))))))

(defun arxana-forum-stream--compute-depths (posts)
  (let ((post-map (make-hash-table :test 'equal))
        (depths (make-hash-table :test 'equal)))
    (dolist (post posts)
      (let ((pid (arxana-forum--get post :post/id)))
        (when pid
          (puthash pid post post-map))))
    (cl-labels ((depth (pid)
                       (or (gethash pid depths)
                           (let* ((post (gethash pid post-map))
                                  (parent (and post (arxana-forum--get post :post/in-reply-to)))
                                  (d (if (and parent (gethash parent post-map))
                                         (1+ (depth parent))
                                       0)))
                             (puthash pid d depths)
                             d))))
      (dolist (post posts)
        (let ((pid (arxana-forum--get post :post/id)))
          (when pid (depth pid)))))
    (list post-map depths)))

(defun arxana-forum-stream--ingest-post (thread-id post)
  (let* ((post-id (arxana-forum--get post :post/id))
         (reply-to (arxana-forum--get post :post/in-reply-to))
         (buf (arxana-forum-stream--get-buffer thread-id)))
    (when post-id
      (with-current-buffer buf
        (unless (gethash post-id arxana-forum-stream--post-ids)
          (puthash post-id t arxana-forum-stream--post-ids)
          (puthash post-id post arxana-forum-stream--post-map)
          (let* ((parent-depth (and reply-to (gethash reply-to arxana-forum-stream--depths)))
                 (depth (if parent-depth (1+ parent-depth) 0)))
            (puthash post-id depth arxana-forum-stream--depths)
            (arxana-forum-stream--append thread-id
                                         (arxana-forum-stream--format-post post depth))))))))

(defun arxana-forum-stream--on-message (_ws frame)
  (let* ((payload (websocket-frame-text frame))
         (msg (arxana-forum--parse-json payload))
         (type (arxana-forum--get msg :type)))
    (pcase type
      ("init"
       (dolist (post (arxana-forum--get msg :recent-posts))
         (when post
           (arxana-forum-stream--ingest-post arxana-forum-stream--current-thread-id post))))
      ("post-created"
       (let ((post (arxana-forum--get msg :post)))
         (when post
           (let ((thread-id (arxana-forum--get post :post/thread-id)))
             (when (and thread-id (equal thread-id arxana-forum-stream--current-thread-id))
               (arxana-forum-stream--ingest-post thread-id post))))))
      ("pong" nil)
      (_ nil))))

(defun arxana-forum-stream--on-close (_ws)
  (when arxana-forum-stream-show-status-messages
    (arxana-forum-stream--append arxana-forum-stream--current-thread-id
                                 "\n--- Disconnected ---\n"))
  (when arxana-forum-stream--ping-timer
    (cancel-timer arxana-forum-stream--ping-timer)
    (setq arxana-forum-stream--ping-timer nil))
  (setq arxana-forum-stream--websocket nil)
  (when (and arxana-forum-stream-auto-reconnect
             arxana-forum-stream--current-thread-id)
    (arxana-forum-stream--schedule-reconnect)))

(defun arxana-forum-stream--on-error (_ws err)
  (if arxana-forum-stream-show-status-messages
      (arxana-forum-stream--append arxana-forum-stream--current-thread-id
                                   (format "\n--- WebSocket error: %s ---\n" err))
    (message "[arxana-forum] websocket error: %s" err)))

(defun arxana-forum-stream--schedule-reconnect ()
  (when arxana-forum-stream--reconnect-timer
    (cancel-timer arxana-forum-stream--reconnect-timer))
  (setq arxana-forum-stream--reconnect-timer
        (run-at-time arxana-forum-stream-reconnect-delay nil
                     (lambda ()
                       (setq arxana-forum-stream--reconnect-timer nil)
                       (when arxana-forum-stream--current-thread-id
                         (arxana-forum-stream-connect arxana-forum-stream--current-thread-id))))))

(defun arxana-forum-stream--start-ping-timer ()
  (when arxana-forum-stream--ping-timer
    (cancel-timer arxana-forum-stream--ping-timer))
  (setq arxana-forum-stream--ping-timer
        (run-with-timer
         10 10
         (lambda ()
           (when (and arxana-forum-stream--websocket
                      (websocket-openp arxana-forum-stream--websocket))
             (websocket-send-text arxana-forum-stream--websocket
                                  (json-encode (list :type "ping"))))))))

(defun arxana-forum-stream--bootstrap (thread-id)
  (let* ((result (arxana-forum--fetch-thread thread-id))
         (thread (car result))
         (posts (cadr result))
         (title (or (arxana-forum--get thread :thread/title) thread-id))
         (sorted (sort (copy-sequence (or posts '()))
                       (lambda (a b)
                         (string< (or (arxana-forum--get a :post/timestamp) "")
                                  (or (arxana-forum--get b :post/timestamp) ""))))))
    (with-current-buffer (arxana-forum-stream--get-buffer thread-id)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Thread %s — %s\n\n" thread-id title)
                            'face 'arxana-forum-stream-meta-face)))
      (let* ((computed (arxana-forum-stream--compute-depths sorted))
             (post-map (car computed))
             (depths (cadr computed)))
        (setq-local arxana-forum-stream--post-map post-map)
        (setq-local arxana-forum-stream--depths depths)
        (clrhash arxana-forum-stream--post-ids)
        (dolist (post sorted)
          (arxana-forum-stream--ingest-post thread-id post))))))

(defun arxana-forum-stream-disconnect ()
  "Disconnect the forum WebSocket stream."
  (interactive)
  (when arxana-forum-stream--reconnect-timer
    (cancel-timer arxana-forum-stream--reconnect-timer)
    (setq arxana-forum-stream--reconnect-timer nil))
  (when arxana-forum-stream--ping-timer
    (cancel-timer arxana-forum-stream--ping-timer)
    (setq arxana-forum-stream--ping-timer nil))
  (let ((auto-reconnect arxana-forum-stream-auto-reconnect))
    (setq arxana-forum-stream-auto-reconnect nil)
    (when (and arxana-forum-stream--websocket
               (websocket-openp arxana-forum-stream--websocket))
      (websocket-close arxana-forum-stream--websocket))
    (setq arxana-forum-stream-auto-reconnect auto-reconnect))
  (setq arxana-forum-stream--websocket nil))

(defun arxana-forum-stream-connect (thread-id)
  "Open an append-only forum stream for THREAD-ID."
  (interactive (list (read-string "Thread id: " arxana-forum-stream--current-thread-id)))
  (arxana-forum--require-websocket)
  (when (and arxana-forum-stream--websocket
             (websocket-openp arxana-forum-stream--websocket))
    (arxana-forum-stream-disconnect))
  (setq arxana-forum-stream--current-thread-id thread-id)
  (arxana-forum-stream--bootstrap thread-id)
  (let ((url (arxana-forum--ws-url thread-id)))
    (setq arxana-forum-stream--websocket
          (websocket-open url
                          :on-message #'arxana-forum-stream--on-message
                          :on-close #'arxana-forum-stream--on-close
                          :on-error #'arxana-forum-stream--on-error)))
  (arxana-forum-stream--start-ping-timer)
  (let ((buf (arxana-forum-stream--get-buffer thread-id)))
    (if (or arxana-forum-stream-auto-focus
            (called-interactively-p 'interactive))
        (pop-to-buffer buf)
      (display-buffer buf))))

(defun arxana-forum-stream-reconnect ()
  "Reconnect the current forum stream."
  (interactive)
  (if arxana-forum-stream--current-thread-id
      (arxana-forum-stream-connect arxana-forum-stream--current-thread-id)
    (call-interactively #'arxana-forum-stream-connect)))

(defun arxana-forum-stream-clear ()
  "Clear the forum stream buffer."
  (interactive)
  (let ((thread-id arxana-forum-stream--current-thread-id))
    (when thread-id
      (with-current-buffer (arxana-forum-stream--get-buffer thread-id)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (propertize (format "Thread %s\n\n" thread-id)
                              'face 'arxana-forum-stream-meta-face)))))))

(defvar arxana-forum-stream-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'arxana-forum-stream-reconnect)
    (define-key map (kbd "c") #'arxana-forum-stream-clear)
    (define-key map (kbd "q") #'arxana-forum-stream-disconnect)
    (define-key map (kbd "C-c C-f") #'arxana-forum-compose-for-current-thread)
    map)
  "Keymap for `arxana-forum-stream-mode'.")

(define-derived-mode arxana-forum-stream-mode special-mode "Arxana-Forum"
  "Mode for streaming forum threads."
  (setq-local truncate-lines nil))

;; =============================================================================
;; Browser integration
;; =============================================================================

(defun arxana-browser--forum-format ()
  [("Thread" 12 t)
   ("Title" 55 t)
   ("Author" 12 t)
   ("Posts" 5 nil)
   ("Updated" 17 t)])

(defun arxana-browser--forum-row (item)
  (let ((thread-id (or (plist-get item :thread-id) ""))
        (title (or (plist-get item :label) ""))
        (author (or (plist-get item :author) ""))
        (count (number-to-string (or (plist-get item :count) 0)))
        (updated (or (plist-get item :updated) ""))
        (pinned? (plist-get item :pinned?)))
    (vector (if pinned? (concat "*" thread-id) thread-id)
            (arxana-forum--truncate title 55)
            (arxana-forum--truncate author 12)
            count
            (arxana-forum--truncate updated 17))))

(defun arxana-browser--forum-items ()
  (condition-case err
      (arxana-forum--fetch-threads)
    (error
     (list (list :type 'info
                 :message (format "Forum unavailable: %s" (error-message-string err))
                 :label "Forum error"
                 :description "Check arxana-forum-server")))))

(defun arxana-forum-open-thread (item)
  "Open a forum thread ITEM in the forum stream buffer."
  (let* ((thread-id (or (plist-get item :thread-id)
                        (arxana-forum--get item :thread/id)
                        (arxana-forum--get item :thread-id)))
         (label (or (plist-get item :label)
                    (arxana-forum--get item :thread/title)
                    (arxana-forum--get item :thread/id)
                    thread-id)))
    (unless (and thread-id (not (string-empty-p (format "%s" thread-id))))
      (user-error "No thread id found"))
    (arxana-forum-stream-connect (format "%s" thread-id))
    (message "Opened forum thread %s (%s)" thread-id label)))

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
  (let ((thread-id nil))
    (cond
     ((and (boundp 'arxana-forum-stream--thread-id)
           arxana-forum-stream--thread-id)
      (setq thread-id arxana-forum-stream--thread-id))
     ((and (fboundp 'tabulated-list-get-id))
      (let ((item (tabulated-list-get-id)))
        (when (and item (eq (plist-get item :type) 'forum-thread))
          (setq thread-id (plist-get item :thread-id))))))
    (unless thread-id
      (user-error "No forum thread selected"))
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
