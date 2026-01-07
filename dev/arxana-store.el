;;; arxana-store.el --- Futon storage bridge helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin wrappers around the Futon1 HTTP API. These helpers sit on top of
;; the historical `futon4-*` functions so callers have a consistent,
;; testable interface for ensuring articles, storing scholia, and fetching
;; data back from the server.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pp)
(require 'subr-x)
(require 'url)
(require 'url-http)

(defvar futon4-enable-sync nil
  "Non-nil enables Futon sync operations.")

(defvar futon4-base-url nil
  "Base URL for the Futon API (e.g., http://localhost:8080).")

(defgroup arxana-store nil
  "Futon storage bridge settings."
  :group 'arxana)

(defcustom arxana-store-default-profile nil
  "Optional profile name to send via the X-Profile header."
  :type '(choice (const :tag "Default" nil)
                 string)
  :group 'arxana-store)

(defcustom arxana-store-request-timeout 10
  "Number of seconds to wait for Futon HTTP responses."
  :type 'integer
  :group 'arxana-store)

(defconst arxana-store--snapshot-scopes '("all" "latest")
  "Valid snapshot scope identifiers for Futon/XTDB backups.")

(defvar arxana-store-last-error nil
  "Most recent storage error (plist with :reason, :detail, :context).")

(defvar arxana-store-last-request nil
  "Most recent request metadata (plist with :method, :target, :payload, :query).")

(defvar arxana-store-last-failure nil
  "Most recent failed request (plist with :request and :error).")

(defvar arxana-store-last-response nil
  "Most recent JSON body returned by `arxana-store--request'.")

(defvar arxana-store-invariants-log-file
  (expand-file-name "../data/logs/qa-invariants.log"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Log file for invariant failures returned by Futon.")

(defun arxana-store-clear-error ()
  "Clear `arxana-store-last-error'."
  (setq arxana-store-last-error nil
        arxana-store-last-failure nil))

(defun arxana-store-sync-enabled-p ()
  "Return non-nil when Futon sync is enabled."
  (and (boundp 'futon4-enable-sync)
       futon4-enable-sync))

(defcustom arxana-store-auto-start-server nil
  "When non-nil, automatically start the Futon server when enabling sync."
  :type 'boolean
  :group 'arxana)

(defun arxana-store--start-server ()
  "Start a Futon server if a known helper is available."
  (cond
   ((fboundp 'futon4-start-server) (futon4-start-server))
   ((fboundp 'tatami-start-server) (tatami-start-server))
   (t nil)))

(defun arxana-store-ensure-sync (&optional prompt)
  "Enable Futon sync, optionally prompting and starting the server."
  (if (arxana-store-sync-enabled-p)
      t
    (let ((prompt (or prompt "Futon sync is disabled. Enable it now? ")))
      (when (yes-or-no-p prompt)
        (setq futon4-enable-sync t)
        (when (or arxana-store-auto-start-server
                  (yes-or-no-p "Start the Futon server now? "))
          (unless (arxana-store--start-server)
            (message "No Futon server start helper found.")))
        t))))

(defun arxana-store--record-error (reason detail &optional context)
  "Remember an error REASON/DETAIL/CONTEXT and log it."
  (setq arxana-store-last-error (list :reason reason
                                      :detail detail
                                      :context context)
        arxana-store-last-failure (list :request arxana-store-last-request
                                        :error arxana-store-last-error))
  (message "[arxana-store] %s" detail)
  nil)

(defun arxana-store--base-url ()
  (or (and (boundp 'futon4-base-url) futon4-base-url)
      (error "futon4-base-url is not set")))

(defun arxana-store--normalize-base (base)
  (let* ((base (string-remove-suffix "/" (or base ""))))
    (cond
     ((string-match-p "/api\\(/alpha\\)?\\'" base) base)
     ((string-match-p "/api/%ce%b1\\'" base) base)
     ((string-match-p "/api/%CE%B1\\'" base) base)
     (t (concat base "/api")))))

(defun arxana-store--build-url (path &optional query)
  (let ((base (arxana-store--normalize-base (arxana-store--base-url)))
        (path (if (stringp path) path "")))
    (concat base
            path
            (if (and query (> (length query) 0))
                (concat "?" query)
              ""))))

(defun arxana-store--encode-segment (value)
  (url-hexify-string (or value "")))

(defun arxana-store--query-string (params)
  (let ((pairs
         (delq nil
               (mapcar (lambda (kv)
                         (when (cdr kv)
                           (format "%s=%s"
                                   (car kv)
                                   (arxana-store--encode-segment (format "%s" (cdr kv))))))
                       params))))
    (when pairs
      (string-join pairs "&"))))

(defun arxana-store--default-headers (payload-p)
  (append (list '("Accept" . "application/json"))
          (when payload-p '(("Content-Type" . "application/json")))
          (when arxana-store-default-profile
            (list (cons "X-Profile" arxana-store-default-profile)))))

(defun arxana-store--canonical-path (path)
  (if (and path (fboundp 'futon4--canonical-path))
      (futon4--canonical-path path)
    path))

(defun arxana-store--normalize-json-body (body)
  "Ensure BODY is UTF-8 decoded so emoji/sigils are not mojibake."
  (when body
    (if (multibyte-string-p body)
        body
      (condition-case _err
          (decode-coding-string body 'utf-8)
        (error body)))))

(defun arxana-store--log-invariants-failure (body)
  "Append BODY to the invariants log when Futon reports a failure."
  (when (and (listp body)
             (string= (alist-get :error body) "Model invariants failed"))
    (condition-case _err
        (let* ((log-file arxana-store-invariants-log-file)
               (dir (file-name-directory log-file))
               (timestamp (format-time-string "%Y-%m-%d %H:%M:%S %z"))
               (request arxana-store-last-request))
          (make-directory dir t)
          (with-temp-buffer
            (insert "* " timestamp " Model invariants failed\n")
            (when request
              (insert "- request: "
                      (or (plist-get request :method) "?")
                      " "
                      (or (plist-get request :target) "?")
                      "\n"))
            (insert "- response:\n")
            (insert "#+begin_src elisp\n")
            (pp body (current-buffer))
            (insert "#+end_src\n\n")
            (append-to-file (point-min) (point-max) log-file)))
      (error nil))))

(defun arxana-store--request (method path &optional payload query)
  "Fire METHOD PATH against Futon and return the parsed JSON body.
Optional PAYLOAD is JSON encoded for POST requests. QUERY is an
already encoded query string (without the leading ?)."
  (if (not (arxana-store-sync-enabled-p))
      (arxana-store--record-error 'disabled "Futon sync disabled" method)
    (unless (stringp path)
      (cl-return-from arxana-store--request
        (arxana-store--record-error 'invalid "Missing request path" path)))
    (let* ((coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8)
           (url-request-method method)
           (url-request-data (when payload
                               (encode-coding-string (json-encode payload) 'utf-8)))
           (url-request-extra-headers (arxana-store--default-headers payload))
           (target (arxana-store--build-url path query))
           (fallback-target (when (and (stringp target)
                                       (string-match-p "://localhost\\b" target))
                              (replace-regexp-in-string "://localhost\\b"
                                                        "://127.0.0.1"
                                                        target))))
      (setq arxana-store-last-request (list :method method
                                            :target target
                                            :payload payload
                                            :query query))
      (condition-case err
          (let ((buf (url-retrieve-synchronously target nil nil arxana-store-request-timeout)))
            (if (not (buffer-live-p buf))
                (let ((retry nil))
                  (when buf (kill-buffer buf))
                  (when fallback-target
                    (setq retry (url-retrieve-synchronously fallback-target nil nil
                                                            arxana-store-request-timeout)))
                  (if (buffer-live-p retry)
                      (setq buf retry)
                    (arxana-store--record-error
                     'connection
                     (format "No response buffer (target %s)" target)
                     arxana-store-last-request)))
              (with-current-buffer buf
                (goto-char (point-min))
                (if (not (re-search-forward "\r?\n\r?\n" nil t))
                    (progn
                      (kill-buffer buf)
                      (arxana-store--record-error 'protocol "Malformed HTTP response" target))
                  (let* ((json-object-type 'alist)
                         (json-array-type 'list)
                         (json-key-type 'keyword)
                         (body (arxana-store--normalize-json-body
                                (buffer-substring-no-properties (point) (point-max))))
                         (body (and body
                                    (let ((parsed nil))
                                      (when (fboundp 'json-parse-string)
                                        (setq parsed
                                              (or (ignore-errors
                                                    (json-parse-string body
                                                                       :object-type 'alist
                                                                       :array-type 'list
                                                                       :object-key-type 'keyword
                                                                       :null-object nil
                                                                       :false-object nil))
                                                  (ignore-errors
                                                    (json-parse-string body
                                                                       :object-type 'alist
                                                                       :array-type 'list
                                                                       :key-type 'keyword
                                                                       :null-object nil
                                                                       :false-object nil)))))
                                      (or parsed
                                          (ignore-errors (json-read-from-string body)))))))
                    (kill-buffer buf)
                    (if body
                        (progn
                          (setq arxana-store-last-error nil
                                arxana-store-last-response body)
                          (arxana-store--log-invariants-failure body)
                          body)
                      (arxana-store--record-error 'protocol "Failed to parse Futon JSON" target)))))))
        (quit
         (arxana-store--record-error 'quit "Request aborted" arxana-store-last-request)
         (signal 'quit nil))
        (error
         (arxana-store--record-error 'request err target))))))

(defun arxana-store--request-async (method path callback &optional payload query)
  "Fire METHOD PATH against Futon and invoke CALLBACK with (RESPONSE STATUS)."
  (if (not (arxana-store-sync-enabled-p))
      (progn
        (arxana-store--record-error 'disabled "Futon sync disabled" method)
        (when (functionp callback)
          (funcall callback nil (list :error 'disabled))))
    (unless (stringp path)
      (arxana-store--record-error 'invalid "Missing request path" path)
      (cl-return-from arxana-store--request-async nil))
    (let* ((coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8)
           (url-request-method method)
           (url-request-data (when payload
                               (encode-coding-string (json-encode payload) 'utf-8)))
           (url-request-extra-headers (arxana-store--default-headers payload))
           (target (arxana-store--build-url path query)))
      (setq arxana-store-last-request (list :method method
                                            :target target
                                            :payload payload
                                            :query query))
      (url-retrieve
       target
       (lambda (status)
         (let ((err (plist-get status :error))
               (result nil))
           (when (and (not err) (buffer-live-p (current-buffer)))
             (goto-char (point-min))
             (when (re-search-forward "\r?\n\r?\n" nil t)
               (let* ((json-object-type 'alist)
                      (json-array-type 'list)
                      (json-key-type 'keyword)
                      (body (arxana-store--normalize-json-body
                             (buffer-substring-no-properties (point) (point-max))))
                      (parsed (or (and (fboundp 'json-parse-string)
                                       (or (ignore-errors
                                             (json-parse-string body
                                                                :object-type 'alist
                                                                :array-type 'list
                                                                :object-key-type 'keyword
                                                                :null-object nil
                                                                :false-object nil))
                                           (ignore-errors
                                             (json-parse-string body
                                                                :object-type 'alist
                                                                :array-type 'list
                                                                :key-type 'keyword
                                                                :null-object nil
                                                                :false-object nil))))
                                  (ignore-errors (json-read-from-string body)))))
                 (setq result parsed))))
           (when (buffer-live-p (current-buffer))
             (kill-buffer (current-buffer)))
           (when (functionp callback)
             (funcall callback result status))))
       nil t))))

(defun arxana-store--normalize-snapshot-scope (scope context)
  "Return the canonical snapshot SCOPE string or record an error for CONTEXT."
  (let ((value (cond
                ((null scope) "all")
                ((stringp scope) scope)
                ((symbolp scope) (symbol-name scope))
                (t scope))))
    (if (member value arxana-store--snapshot-scopes)
        value
      (arxana-store--record-error
       'invalid
       (format "Unknown snapshot scope %S (expected %s)"
               scope arxana-store--snapshot-scopes)
       context))))

(defun arxana-store--snapshot-scope-prompt (&optional prompt default)
  "Prompt user for a snapshot scope using PROMPT and DEFAULT."
  (let* ((prompt (or prompt "Snapshot scope (all/latest): "))
         (default (or default "all"))
         (choice (completing-read prompt arxana-store--snapshot-scopes
                                  nil t nil nil default)))
    (intern choice)))

(cl-defun arxana-store-ensure-article (&key name path spine props)
  "Ensure Futon has an article entity NAME, returning its canonical id."
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'article))
   ((not name)
    (arxana-store--record-error 'invalid "Article name is required" 'article))
   (t
    (let* ((canonical (when path (futon4--canonical-path path)))
           (id (futon4--article-id-for name canonical)))
      (futon4-ensure-article-entity id name canonical spine nil props)
      id))))

(cl-defun arxana-store-ensure-entity (&key id name type source entity/source external-id seen-count pinned? last-seen props media/sha256)
  "Ensure Futon has an entity named NAME of TYPE, returning the response.
ID, SOURCE, EXTERNAL-ID, SEEN-COUNT, PINNED?, and LAST-SEEN mirror the
payload accepted by Futon's `/entity` endpoint. TYPE may be a keyword or
string. MEDIA/SHA256 is stored at the top level when provided. Signals a
user error when NAME is missing."
  (unless name
    (user-error "Entity name is required"))
  (let* ((payload (delq nil (list (cons 'name name)
                                  (when type
                                    (cons 'type (if (keywordp type)
                                                    (symbol-name type)
                                                  type)))
                                  (when id (cons 'id id))
                                  (when source (cons 'source source))
                                  (when entity/source (cons 'entity/source entity/source))
                                  (when external-id (cons 'external-id external-id))
                                  (when seen-count (cons 'seen-count seen-count))
                                  (when (not (null pinned?))
                                    (cons 'pinned? (and pinned? t)))
                                  (when last-seen (cons 'last-seen last-seen))
                                  (when media/sha256 (cons 'media/sha256 media/sha256))
                                  (when props (cons 'props props))))))
    (arxana-store--request "POST" "/entity" payload)))

(defun arxana-store--relation-payload (src-id dst-id label extra-props &optional type)
  (let ((props (cons (cons 'label (or label "")) (or extra-props '()))))
    (delq nil
          (list (cons 'type (or type "arxana/scholium"))
                (cons 'src src-id)
                (cons 'dst dst-id)
                (when props (cons 'props props))))))

(defun arxana-store--post-relation (src-id dst-id &optional label extra-props type)
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'relation))
   ((or (null src-id) (null dst-id))
    (arxana-store--record-error 'invalid "Provide source and target ids" 'relation))
   (t
    (let ((payload (arxana-store--relation-payload src-id dst-id label extra-props type)))
      (arxana-store--request "POST" "/relation" payload)))))

(defun arxana-store--nema-simple-wrapper (src dst label &optional callback)
  "Post a simple scholium relation and invoke CALLBACK with the response."
  (let ((resp (arxana-store--post-relation src dst label)))
    (when (and callback resp)
      (funcall callback resp))
    resp))

(defun arxana-store--install-relation-shim ()
  "Install legacy relation entry points on top of the store helpers."
  (unless (fboundp 'futon4-store-nema-simple)
    (defalias 'futon4-store-nema-simple #'arxana-store--nema-simple-wrapper))
  (unless (fboundp 'futon4-store-hyperedge)
    (defalias 'futon4-store-hyperedge #'arxana-store--hyperedge-wrapper)))

(defun futon4--sync-about-links (source links)
  "Sync ABOUT LINKS for SOURCE using the legacy Futon shim."
  (let ((source-id (cond
                    ((fboundp 'futon4-lookup-article-id)
                     (futon4-lookup-article-id source))
                    ((fboundp 'futon4--article-id-for)
                     (futon4--article-id-for source))
                    (t nil))))
    (dolist (link links)
      (let* ((target (car link))
             (extras (cdr link))
             (target-id (cond
                         ((fboundp 'futon4--article-id-for)
                          (futon4--article-id-for target))
                         ((fboundp 'futon4-lookup-article-id)
                          (futon4-lookup-article-id target))
                         (t nil)))
             (label (if extras
                        (mapconcat #'prin1-to-string extras " | ")
                      "")))
        (when (and source-id target-id (fboundp 'futon4-store-nema-simple))
          (futon4-store-nema-simple source-id target-id label nil))))))

(cl-defun arxana-store-create-relation (&key src dst label props type)
  (unless (and src dst)
    (user-error "Provide both :src and :dst ids"))
  (arxana-store--post-relation src dst label props type))

(defun arxana-store-create-relations-batch (relations)
  "Persist RELATIONS in a single request.
RELATIONS is a list of relation payloads matching the /relation format."
  (unless (and (listp relations) relations)
    (user-error "Provide a non-empty relations list"))
  (arxana-store--request "POST" "/relations/batch"
                         (list (cons 'relations relations))))

(defun arxana-store-create-relations-batch-async (relations callback)
  "Persist RELATIONS asynchronously and invoke CALLBACK with (RESPONSE STATUS)."
  (unless (and (listp relations) relations)
    (user-error "Provide a non-empty relations list"))
  (arxana-store--request-async "POST" "/relations/batch" callback
                               (list (cons 'relations relations))))

(defun arxana-store--stringify (value)
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun arxana-store--hyperedge-payload (type hx-type endpoints props)
  (let ((clean (delq nil endpoints)))
    (delq nil
          (list (when type (cons 'type (arxana-store--stringify type)))
                (when hx-type (cons 'hx/type (arxana-store--stringify hx-type)))
                (cons 'hx/endpoints clean)
                (when props (cons 'props props))))))

(defun arxana-store--post-hyperedge (type hx-type endpoints &optional props)
  (when (fboundp 'arxana-data-constraints-validate-hyperedge)
    (arxana-data-constraints-validate-hyperedge hx-type endpoints))
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'hyperedge))
   ((or (not hx-type) (not endpoints))
    (arxana-store--record-error 'invalid "Provide hx/type and endpoints" 'hyperedge))
   (t
    (let ((payload (arxana-store--hyperedge-payload type hx-type endpoints props)))
      (arxana-store--request "POST" "/hyperedge" payload)))))

(defun arxana-store--hyperedge-wrapper (type hx-type endpoints props &optional callback)
  "Post a hyperedge and invoke CALLBACK with the response."
  (let ((resp (arxana-store--post-hyperedge type hx-type endpoints props)))
    (when (and callback resp)
      (funcall callback resp))
    resp))

(defun arxana-store-upsert-scholium (source target &optional label)
  (interactive
   (list (read-string "Source article: " (or (and (boundp 'name-of-current-article)
                                                  name-of-current-article)
                                              (buffer-name)))
         (read-string "Target article: ")
         (read-string "Label (optional): " nil nil "")))
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'upsert-scholium))
   ((or (null source) (null target))
    (arxana-store--record-error 'invalid "Provide both source and target names" 'upsert-scholium))
   (t
    (let* ((src-id (and (fboundp 'futon4--article-id-for)
                        (futon4--article-id-for source)))
           (dst-id (and (fboundp 'futon4--article-id-for)
                        (futon4--article-id-for target))))
      (unless (and src-id dst-id)
        (cl-return-from arxana-store-upsert-scholium
          (arxana-store--record-error 'invalid "Could not derive scholium ids" 'upsert-scholium)))
      (let ((response (arxana-store--post-relation src-id dst-id label)))
        (when response
          (when (called-interactively-p 'interactive)
            (message "Stored scholium %s → %s" src-id dst-id))
          (list :src src-id :dst dst-id :label label)))))))

(defun arxana-store-fetch-entity (id &optional version as-of)
  (interactive
   (list (read-string "Entity id: " "")
         (read-string "Version (uuid or blank): ")
         (let ((val (read-string "As-of (ms since epoch, blank for latest): ")))
           (when (and val (> (length val) 0)) val))))
  (unless id
    (cl-return-from arxana-store-fetch-entity
      (arxana-store--record-error 'invalid "Missing entity id" 'fetch-entity)))
  (let ((query (arxana-store--query-string
                (delq nil (list (when version (cons "version" version))
                                (when as-of (cons "as-of" as-of)))))))
    (arxana-store--request "GET"
                           (format "/entity/%s" (arxana-store--encode-segment id))
                           nil query)))

(defun arxana-store-entity-history (id &optional limit)
  (interactive (list (read-string "Entity id: " "") (read-number "Limit: " 10)))
  (unless id
    (cl-return-from arxana-store-entity-history
      (arxana-store--record-error 'invalid "Missing entity id" 'entity-history)))
  (let ((query (arxana-store--query-string (list (cons "limit" (or limit 10))))))
    (arxana-store--request "GET"
                           (format "/entities/history/%s" (arxana-store--encode-segment id))
                           nil query)))

(defun arxana-store-ego (name &optional limit)
  (interactive
   (list (read-string "Ego name: " (or (and (boundp 'name-of-current-article)
                                             name-of-current-article)
                                        (buffer-name)))
         (read-number "Limit: " 15)))
  (let ((target-name (or name (and (boundp 'name-of-current-article)
                                   name-of-current-article))))
    (unless target-name
      (cl-return-from arxana-store-ego
        (arxana-store--record-error 'invalid "Missing ego name" 'ego)))
    (let ((query (arxana-store--query-string (when limit (list (cons "limit" limit)))))
          (encoded (arxana-store--encode-segment target-name)))
      (arxana-store--request "GET" (format "/ego/%s" encoded) nil query))))

(defun arxana-store-ego-async (name callback &optional limit)
  "Fetch ego data for NAME and invoke CALLBACK with (RESPONSE STATUS)."
  (unless (arxana-store-sync-enabled-p)
    (arxana-store--record-error 'disabled "Futon sync disabled" 'ego)
    (cl-return-from arxana-store-ego-async nil))
  (unless (and name (> (length name) 0))
    (arxana-store--record-error 'invalid "Missing ego name" 'ego)
    (cl-return-from arxana-store-ego-async nil))
  (let* ((query (arxana-store--query-string (when limit (list (cons "limit" limit)))))
         (encoded (arxana-store--encode-segment name))
         (target (arxana-store--build-url (format "/ego/%s" encoded) query)))
    (url-retrieve
     target
     (lambda (status)
       (let ((err (plist-get status :error))
             (result nil))
         (when (and (not err) (buffer-live-p (current-buffer)))
           (goto-char (point-min))
           (when (re-search-forward "\r?\n\r?\n" nil t)
             (let* ((json-object-type 'alist)
                    (json-array-type 'list)
                    (json-key-type 'keyword)
                    (body (arxana-store--normalize-json-body
                           (buffer-substring-no-properties (point) (point-max))))
                    (parsed (or (and (fboundp 'json-parse-string)
                                     (or (ignore-errors
                                           (json-parse-string body
                                                              :object-type 'alist
                                                              :array-type 'list
                                                              :object-key-type 'keyword
                                                              :null-object nil
                                                              :false-object nil))
                                         (ignore-errors
                                           (json-parse-string body
                                                              :object-type 'alist
                                                              :array-type 'list
                                                              :key-type 'keyword
                                                              :null-object nil
                                                              :false-object nil))))
                                (ignore-errors (json-read-from-string body)))))
               (setq result parsed))))
         (when (buffer-live-p (current-buffer))
           (kill-buffer (current-buffer)))
         (when (functionp callback)
           (funcall callback result status))))
     nil t)))

(defun arxana-store-cooccur (name &optional limit)
  (interactive (list (read-string "Cooccur entity: " (or (and (boundp 'name-of-current-article)
                                                        name-of-current-article)
                                                   (buffer-name)))
                     (read-number "Limit: " 10)))
  (let ((target-name name))
    (if (not (and target-name (> (length target-name) 0)))
        (arxana-store--record-error 'invalid "Missing cooccur name" 'cooccur)
      (let* ((query (arxana-store--query-string (when limit (list (cons "limit" limit)))))
             (encoded (arxana-store--encode-segment target-name)))
        (arxana-store--request "GET" (format "/cooccur/%s" encoded) nil query)))))

(defun arxana-store-tail (&optional limit)
  (interactive (list (read-number "Tail limit: " 5)))
  (let* ((limit (or limit 5))
         (query (arxana-store--query-string (when limit (list (cons "limit" limit)))))
         (body (arxana-store--request "GET" "/tail" nil query)))
    (when (and body (called-interactively-p 'interactive))
      (message "Fetched /tail (%d relations)" limit))
    body))

(defun arxana-store-types ()
  "Fetch the type registry from Futon."
  (interactive)
  (arxana-store--request "GET" "/types"))

;;;###autoload
(defun arxana-store-ping (&optional limit)
  "Probe the Futon API and report diagnostics in a buffer."
  (interactive "P")
  (let* ((limit (if (numberp limit) limit 1))
         (query (arxana-store--query-string (when limit (list (cons "limit" limit)))))
         (target (arxana-store--build-url "/tail" query))
         (response (arxana-store--request "GET" "/tail" nil query))
         (buffer (get-buffer-create "*Arxana Store Ping*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Sync enabled: %s\n" (if (arxana-store-sync-enabled-p) "yes" "no")))
        (insert (format "Base URL: %s\n" (arxana-store--base-url)))
        (insert (format "Target: %s\n" target))
        (insert (format "Timeout: %ss\n" arxana-store-request-timeout))
        (insert (format "Proxy: %S\n" url-proxy-services))
        (insert "\nLast request:\n")
        (pp arxana-store-last-request (current-buffer))
        (insert "\n\nLast error:\n")
        (pp arxana-store-last-error (current-buffer))
        (insert "\n\nLast failure:\n")
        (pp arxana-store-last-failure (current-buffer))
        (insert "\n\nResponse:\n")
        (if response
            (pp response (current-buffer))
          (insert "(nil)\n"))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun arxana-store-last-request-report ()
  "Show the last request/error without issuing a new HTTP call."
  (interactive)
  (let ((buffer (get-buffer-create "*Arxana Store Last Request*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Sync enabled: %s\n" (if (arxana-store-sync-enabled-p) "yes" "no")))
        (insert (format "Base URL: %s\n" (arxana-store--base-url)))
        (insert (format "Timeout: %ss\n" arxana-store-request-timeout))
        (insert (format "Proxy: %S\n" url-proxy-services))
        (insert "\nLast request:\n")
        (pp arxana-store-last-request (current-buffer))
        (insert "\n\nLast error:\n")
        (pp arxana-store-last-error (current-buffer))
        (insert "\n\nLast failure:\n")
        (pp arxana-store-last-failure (current-buffer))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

(defun arxana-store-save-snapshot (&optional scope label)
  "Save an XTDB snapshot with SCOPE and optional LABEL."
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'snapshot))
   (t
    (let ((scope (arxana-store--normalize-snapshot-scope scope 'snapshot)))
      (when scope
        (arxana-store--request "POST" "/snapshot"
                               (delq nil (list (cons 'scope scope)
                                               (when label (cons 'label label))))))))))

(defun arxana-store-restore-snapshot (&optional snapshot-id scope)
  "Restore XTDB snapshot SNAPSHOT-ID for SCOPE."
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'snapshot))
   (t
    (let ((scope (arxana-store--normalize-snapshot-scope scope 'snapshot)))
      (when scope
        (arxana-store--request "POST" "/snapshot/restore"
                               (delq nil (list (cons 'action "restore")
                                               (cons 'scope scope)
                                               (when snapshot-id
                                                 (cons 'snapshot/id snapshot-id))))))))))

(provide 'arxana-store)

;;; arxana-store.el ends here
