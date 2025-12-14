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
                                  (cons 'type (if (keywordp type)
                                                  (symbol-name type)
                                                type))
                                  (when id (cons 'id id))
                                  (when source (cons 'source source))
                                  (when external-id (cons 'external-id external-id))
                                  (when seen-count (cons 'seen-count seen-count))
                                  (when pinned? (cons 'pinned pinned?))
                                  (when last-seen (cons 'last-seen last-seen))))))
    (arxana-store--request "POST" "/entity" payload)))

;;; (rest of file omitted for brevity)
