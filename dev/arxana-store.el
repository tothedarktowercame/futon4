;;; arxana-store.el --- Futon storage bridge helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin wrappers around the Futon1 HTTP API. These helpers sit on top of
;; the historical `futon4-*` functions so callers have a consistent,
;; testable interface for ensuring articles, storing scholia, and fetching
;; data back from the server.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)

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

(defvar arxana-store-last-response nil
  "Most recent JSON body returned by `arxana-store--request'.")

(defun arxana-store-clear-error ()
  "Clear `arxana-store-last-error'."
  (setq arxana-store-last-error nil))

(defun arxana-store-sync-enabled-p ()
  "Return non-nil when Futon sync is enabled."
  (and (boundp 'futon4-enable-sync)
       futon4-enable-sync))

(defun arxana-store--record-error (reason detail &optional context)
  "Remember an error REASON/DETAIL/CONTEXT and log it."
  (setq arxana-store-last-error (list :reason reason
                                      :detail detail
                                      :context context))
  (message "[arxana-store] %s" detail)
  nil)

(defun arxana-store--base-url ()
  (or (and (boundp 'futon4-base-url) futon4-base-url)
      (error "futon4-base-url is not set")))

(defun arxana-store--build-url (path &optional query)
  (concat (arxana-store--base-url)
          path
          (if (and query (> (length query) 0))
              (concat "?" query)
            "")))

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

(defun arxana-store--request (method path &optional payload query)
  "Fire METHOD PATH against Futon and return the parsed JSON body.
Optional PAYLOAD is JSON encoded for POST requests. QUERY is an
already encoded query string (without the leading ?)."
  (if (not (arxana-store-sync-enabled-p))
      (arxana-store--record-error 'disabled "Futon sync disabled" method)
    (let* ((url-request-method method)
           (url-request-data (when payload
                               (encode-coding-string (json-encode payload) 'utf-8)))
           (url-request-extra-headers (arxana-store--default-headers payload))
           (target (arxana-store--build-url path query)))
      (condition-case err
          (let ((buf (url-retrieve-synchronously target nil nil arxana-store-request-timeout)))
            (if (not (buffer-live-p buf))
                (progn
                  (when buf (kill-buffer buf))
                  (arxana-store--record-error 'connection "No response buffer" target))
              (with-current-buffer buf
                (goto-char (point-min))
                (if (not (re-search-forward "\r?\n\r?\n" nil t))
                    (progn
                      (kill-buffer buf)
                      (arxana-store--record-error 'protocol "Malformed HTTP response" target))
                  (let* ((json-object-type 'alist)
                         (json-array-type 'list)
                         (json-key-type 'keyword)
                         (body (ignore-errors (json-read))))
                    (kill-buffer buf)
                    (if body
                        (progn
                          (setq arxana-store-last-error nil
                                arxana-store-last-response body)
                          body)
                      (arxana-store--record-error 'protocol "Failed to parse Futon JSON" target)))))))
        (error
         (arxana-store--record-error 'request err target))))))

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

(cl-defun arxana-store-ensure-entity (&key id name type source external-id seen-count pinned? last-seen)
  "Ensure Futon has an entity named NAME of TYPE, returning the response.
ID, SOURCE, EXTERNAL-ID, SEEN-COUNT, PINNED?, and LAST-SEEN mirror the
payload accepted by Futon's `/entity` endpoint. TYPE may be a keyword or
string. Signals a user error when NAME is missing."
  (unless name
    (user-error "Entity name is required"))
  (let* ((payload (delq nil (list (cons 'name name)
                                  (when type
                                    (cons 'type (if (keywordp type)
                                                    (symbol-name type)
                                                  type)))
                                  (when id (cons 'id id))
                                  (when source (cons 'source source))
                                  (when external-id (cons 'external-id external-id))
                                  (when seen-count (cons 'seen-count seen-count))
                                  (when (not (null pinned?))
                                    (cons 'pinned? (and pinned? t)))
                                  (when last-seen (cons 'last-seen last-seen))))))
    (arxana-store--request "POST" "/entity" payload)))

(defun arxana-store--relation-payload (src-id dst-id label extra-props)
  (let ((props (cons (cons 'label (or label "")) (or extra-props '()))))
    (delq nil
          (list (cons 'type "arxana/scholium")
                (cons 'src src-id)
                (cons 'dst dst-id)
                (when props (cons 'props props))))))

(defun arxana-store--post-relation (src-id dst-id &optional label extra-props)
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'relation))
   ((or (null src-id) (null dst-id))
    (arxana-store--record-error 'invalid "Provide source and target ids" 'relation))
   (t
    (let ((payload (arxana-store--relation-payload src-id dst-id label extra-props)))
      (arxana-store--request "POST" "/relation" payload)))))

(cl-defun arxana-store-create-relation (&key src dst label props)
  (unless (and src dst)
    (user-error "Provide both :src and :dst ids"))
  (arxana-store--post-relation src dst label props))

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
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'hyperedge))
   ((or (not hx-type) (not endpoints))
    (arxana-store--record-error 'invalid "Provide hx/type and endpoints" 'hyperedge))
   (t
    (let ((payload (arxana-store--hyperedge-payload type hx-type endpoints props)))
      (arxana-store--request "POST" "/hyperedge" payload)))))

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

(provide 'arxana-store)

;;; arxana-store.el ends here
