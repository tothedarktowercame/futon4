;;; arxana-store.el --- Futon storage bridge helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Thin wrappers around the Futon1 HTTP API.  These helpers sit on top of
;; the historical futon4-* functions so callers have a consistent, testable
;; interface for ensuring articles, storing scholia, and fetching data back
;; from the server.

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
          (when payload-p
            '(("Content-Type" . "application/json")))
          (when arxana-store-default-profile
            (list (cons "X-Profile" arxana-store-default-profile)))))

(defun arxana-store--canonical-path (path)
  (if (and path (fboundp 'futon4--canonical-path))
      (futon4--canonical-path path)
    path))

(defun arxana-store--request (method path &optional payload query)
  "Fire METHOD PATH against Futon and return the parsed JSON body.
Optional PAYLOAD is JSON encoded for POST requests.  QUERY is an
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
payload accepted by Futon's `/entity` endpoint.  TYPE may be a keyword or
string.  Signals a user error when NAME is missing."
  (unless (and name (not (string-empty-p name)))
    (user-error "Entity name is required"))
  (let* ((payload (delq nil
                        (list (when id (cons 'id id))
                              (cons 'name name)
                              (when type
                                (cons 'type (if (symbolp type)
                                                (symbol-name type)
                                              type)))
                              (when source (cons 'source source))
                              (when external-id (cons 'external-id external-id))
                              (when seen-count (cons 'seen-count seen-count))
                              (when (not (null pinned?))
                                (cons 'pinned? (and pinned? t)))
                              (when last-seen (cons 'last-seen last-seen))))))
    (arxana-store--request "POST" "/entity" payload)))

(defun arxana-store--relation-payload (src-id dst-id label extra-props)
  "Build the Futon relation payload for SRC-ID, DST-ID, LABEL, and EXTRA-PROPS."
  (let ((props (cons (cons 'label (or label "")) (or extra-props '()))))
    (delq nil
          (list (cons 'type "arxana/scholium")
                (cons 'src src-id)
                (cons 'dst dst-id)
                (when props
                  (cons 'props props))))))

(defun arxana-store--post-relation (src-id dst-id &optional label extra-props)
  "POST a scholium relation between SRC-ID and DST-ID to Futon1.
LABEL defaults to the empty string.  EXTRA-PROPS lets callers add
additional Futon props.  Returns the parsed Futon response or nil
when sync is disabled or the request fails."
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'relation))
   ((or (null src-id) (null dst-id))
    (arxana-store--record-error 'invalid "Provide source and target ids" 'relation))
   (t
    (let ((payload (arxana-store--relation-payload src-id dst-id label extra-props)))
      (arxana-store--request "POST" "/relation" payload)))))

(defun arxana-store--stringify (value)
  "Return VALUE as a string for JSON payloads."
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun arxana-store--hyperedge-payload (type hx-type endpoints props)
  "Build the Futon hyperedge payload body."
  (let ((clean-endpoints (delq nil endpoints)))
    (delq nil
          (list (when type
                  (cons 'type (arxana-store--stringify type)))
                (when hx-type
                  (cons 'hx/type (arxana-store--stringify hx-type)))
                (cons 'hx/endpoints clean-endpoints)
                (when props (cons 'props props))))))

(defun arxana-store--post-hyperedge (type hx-type endpoints &optional props)
  "POST a multi-end hyperedge described by TYPE, HX-TYPE, and ENDPOINTS."
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'hyperedge))
   ((null hx-type)
    (arxana-store--record-error 'invalid "Provide an :hx/type" 'hyperedge))
   ((or (null endpoints) (not (listp endpoints)))
    (arxana-store--record-error 'invalid "Provide at least one endpoint" 'hyperedge))
   (t
    (let ((payload (arxana-store--hyperedge-payload type hx-type endpoints props)))
      (arxana-store--request "POST" "/hyperedge" payload)))))

(defun arxana-store-post-hyperedge (type hx-type endpoints &optional props)
  "Public wrapper for posting multi-end hyperedges to Futon."
  (arxana-store--post-hyperedge type hx-type endpoints props))

(defun arxana-store--snapshot-payload (scope &optional label)
  "Build the payload used for snapshot save/restore calls."
  (delq nil (list (cons 'scope scope)
                  (when (and label (not (string-empty-p label)))
                    (cons 'label label)))))

(defun arxana-store-save-snapshot (&optional scope label)
  "Request a Futon/XTDB snapshot with SCOPE (`all' or `latest').
LABEL is an optional hint recorded server-side for bookkeeping.
Returns the Futon response body or nil when sync is disabled." 
  (interactive
   (list (arxana-store--snapshot-scope-prompt "Snapshot scope (all/latest): " "all")
         (read-string "Snapshot label (optional): " nil nil "")))
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'snapshot-save))
   (t
    (let ((scope-str (arxana-store--normalize-snapshot-scope scope 'snapshot-save)))
      (when scope-str
        (let ((payload (arxana-store--snapshot-payload scope-str label)))
          (arxana-store--request "POST" "/snapshot" payload)))))))

(defun arxana-store-restore-snapshot (&optional snapshot-id scope)
  "Restore the Futon/XTDB store from SNAPSHOT-ID with SCOPE semantics.
When SNAPSHOT-ID is nil, Futon restores the most recent snapshot.
SCOPE mirrors `arxana-store-save-snapshot' (`all' or `latest')."
  (interactive
   (list (let ((id (read-string "Snapshot id (leave blank for latest server snapshot): ")))
           (unless (string-empty-p id) id))
         (arxana-store--snapshot-scope-prompt "Restore scope (all/latest): " "all")))
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'snapshot-restore))
   (t
    (let ((scope-str (arxana-store--normalize-snapshot-scope scope 'snapshot-restore)))
      (when scope-str
        (let ((payload (delq nil (list (cons 'action "restore")
                                       (cons 'scope scope-str)
                                       (when snapshot-id (cons 'snapshot/id snapshot-id))))))
          (arxana-store--request "POST" "/snapshot/restore" payload)))))))

(defun arxana-store--snapshot-id-from-response (response)
  "Try to extract a snapshot id from RESPONSE alist." 
  (let* ((snapshot (or (alist-get :snapshot response)
                       (alist-get 'snapshot response)))
         (id (or (and snapshot
                      (or (alist-get :id snapshot)
                          (alist-get 'id snapshot)))
                 (alist-get :snapshot/id response)
                 (alist-get 'snapshot/id response)
                 (alist-get :id response)
                 (alist-get 'id response))))
    id))

(cl-defun arxana-store-ensure-article (&key name path spine id props)
  "Ensure Futon has an entity for NAME (or PATH).
Returns the Futon id when successful."
  (interactive
   (list :name (or (and (boundp 'name-of-current-article)
                        name-of-current-article)
                   (buffer-name))
         :path (or (and (buffer-file-name)
                        (fboundp 'futon4--canonical-path)
                        (futon4--canonical-path (buffer-file-name)))
                   (buffer-file-name))
         :spine (and current-prefix-arg t)))
  (cond
   ((not (arxana-store-sync-enabled-p))
    (arxana-store--record-error 'disabled "Futon sync disabled" 'ensure-article))
   ((and (null name) (null path) (null id))
    (arxana-store--record-error 'invalid "Provide NAME, PATH, or ID" 'ensure-article))
   (t
    (let* ((canonical-path (arxana-store--canonical-path path))
           (article-id (or id
                           (and (fboundp 'futon4--article-id-for)
                                (futon4--article-id-for name canonical-path)))))
      (unless article-id
        (cl-return-from arxana-store-ensure-article
          (arxana-store--record-error 'invalid "Could not derive article id" 'ensure-article)))
      (condition-case err
          (progn
            (when (fboundp 'futon4-ensure-article-entity)
              (when (and (fboundp 'arxana-article--remember-path) canonical-path)
                (arxana-article--remember-path name canonical-path))
              (futon4-ensure-article-entity article-id (or name canonical-path article-id)
                                            canonical-path spine nil props))
            (when (called-interactively-p 'interactive)
              (message "Ensured Futon entity %s" article-id))
            article-id)
        (error
         (arxana-store--record-error 'elisp err 'ensure-article)))))))

(defun arxana-store-upsert-scholium (source target &optional label)
  "Ensure SOURCE → TARGET scholium relation exists.  LABEL is optional."
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
      (condition-case err
          (progn
            (let ((response (arxana-store--post-relation src-id dst-id label)))
              (when (called-interactively-p 'interactive)
                (if response
                    (message "Stored scholium %s → %s" src-id dst-id)
                  (message "Failed to store scholium %s → %s" src-id dst-id)))
              (when response
                (list :src src-id :dst dst-id :label label))))
        (error
         (arxana-store--record-error 'elisp err 'upsert-scholium)))))))

(defun arxana-store-fetch-entity (id &optional version as-of)
  "Fetch entity ID from Futon. Optional VERSION UUID and AS-OF timestamp (ms)."
  (interactive
   (list (read-string "Entity id: "
                      (or (and (boundp 'name-of-current-article)
                               (fboundp 'futon4-lookup-article-id)
                               (futon4-lookup-article-id name-of-current-article))
                          ""))
         (read-string "Version (uuid or blank): ")
         (let ((val (read-string "As-of (ms since epoch, blank for latest): ")))
           (when (and val (> (length val) 0))
             val))))
  (unless id
    (cl-return-from arxana-store-fetch-entity
      (arxana-store--record-error 'invalid "Missing entity id" 'fetch-entity)))
  (let ((query (arxana-store--query-string
                (delq nil (list (when version (cons "version" version))
                                (when as-of (cons "as-of" as-of)))))))
    (let ((body (arxana-store--request "GET"
                                       (format "/entity/%s" (arxana-store--encode-segment id))
                                       nil query)))
      (when (and body (called-interactively-p 'interactive))
        (message "Fetched entity %s" id))
      body)))

(defun arxana-store-entity-history (id &optional limit)
  "Return history entries for entity ID. LIMIT defaults to 10."
  (interactive
   (list (read-string "Entity id: "
                      (or (and (boundp 'name-of-current-article)
                               (fboundp 'futon4-lookup-article-id)
                               (futon4-lookup-article-id name-of-current-article))
                          ""))
         (let ((val (read-number "Limit: " 10))) val)))
  (unless id
    (cl-return-from arxana-store-entity-history
      (arxana-store--record-error 'invalid "Missing entity id" 'entity-history)))
  (let ((query (arxana-store--query-string
                (list (cons "limit" (or limit 10))))))
    (let ((body (arxana-store--request "GET"
                                       (format "/entities/history/%s"
                                               (arxana-store--encode-segment id))
                                       nil query)))
      (when (and body (called-interactively-p 'interactive))
        (message "Fetched history for %s" id))
      body)))

(defun arxana-store-ego (name &optional limit)
  "Return Futon /ego data for NAME (defaults to current article).
LIMIT controls how many links Futon should return."
  (interactive
   (list (read-string "Ego name: "
                      (or (and (boundp 'name-of-current-article)
                               name-of-current-article)
                          (buffer-name)))
         (read-number "Limit: " 15)))
  (let* ((target-name (or name (and (boundp 'name-of-current-article)
                                    name-of-current-article))))
    (unless target-name
      (cl-return-from arxana-store-ego
        (arxana-store--record-error 'invalid "Missing ego name" 'ego)))
    (let ((query (arxana-store--query-string
                  (when limit (list (cons "limit" limit)))))
          (encoded (arxana-store--encode-segment target-name)))
      (let ((body (arxana-store--request "GET"
                                         (format "/ego/%s" encoded)
                                         nil query)))
        (when (and body (called-interactively-p 'interactive))
          (message "Fetched /ego for %s" target-name))
        body))))

(defun arxana-store-cooccur (name &optional limit)
  "Return Futon /cooccur data for NAME.
LIMIT controls how many co-occurring entities Futon should return."
  (interactive
   (list (read-string "Cooccur entity: "
                      (or (and (boundp 'name-of-current-article)
                               name-of-current-article)
                          (buffer-name)))
         (read-number "Limit: " 10)))
  (let ((target-name name))
    (if (not (and target-name (> (length target-name) 0)))
        (arxana-store--record-error 'invalid "Missing cooccur name" 'cooccur)
      (let* ((query (arxana-store--query-string
                     (when limit (list (cons "limit" limit)))))
             (encoded (arxana-store--encode-segment target-name))
             (body (arxana-store--request "GET"
                                          (format "/cooccur/%s" encoded)
                                          nil query)))
        (when (and body (called-interactively-p 'interactive))
          (message "Fetched /cooccur for %s" target-name))
        body))))

(defun arxana-store-tail (&optional limit)
  "Return Futon /tail data. LIMIT defaults to 5 relations."
  (interactive (list (read-number "Tail limit: " 5)))
  (let* ((limit (or limit 5))
         (query (arxana-store--query-string (when limit (list (cons "limit" limit)))))
         (body (arxana-store--request "GET" "/tail" nil query)))
    (when (and body (called-interactively-p 'interactive))
      (message "Fetched /tail (%d relations)" limit))
    body))

(defun arxana-store--nema-simple-wrapper (src-id dst-id &optional label cb)
  "Compatibility shim so legacy callers hit `arxana-store--post-relation'."
  (let ((body (arxana-store--post-relation src-id dst-id label)))
    (when (and body cb)
      (funcall cb body))
    body))

(defun arxana-store--hyperedge-wrapper (type hx-type endpoints &optional props cb)
  "Compatibility shim for `futon4-store-hyperedge'."
  (let ((body (arxana-store--post-hyperedge type hx-type endpoints props)))
    (when (and body cb)
      (funcall cb body))
    body))

(defun arxana-store--install-relation-shim ()
  "Ensure `futon4-store-nema-simple' routes through the store helpers."
  (when (fboundp 'futon4-store-nema-simple)
    (fset 'futon4-store-nema-simple #'arxana-store--nema-simple-wrapper)))

(defun arxana-store--install-hyperedge-shim ()
  "Ensure `futon4-store-hyperedge' routes through the store helpers."
  (when (fboundp 'futon4-store-hyperedge)
    (fset 'futon4-store-hyperedge #'arxana-store--hyperedge-wrapper)))

(arxana-store--install-relation-shim)
(arxana-store--install-hyperedge-shim)

(with-eval-after-load 'arxana-tangled
  (arxana-store--install-relation-shim)
  (arxana-store--install-hyperedge-shim))

(provide 'arxana-store)

;;; arxana-store.el ends here
