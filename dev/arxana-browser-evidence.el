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

(declare-function arxana-browser--render "arxana-browser-core")

(defgroup arxana-evidence nil
  "Evidence landscape browsing for Arxana."
  :group 'arxana)

(defcustom arxana-evidence-server
  (or (getenv "FUTON3C_SERVER")
      (getenv "FUTON1_API_BASE")
      (getenv "STACK_HUD_FUTON1_API_BASE")
      "http://localhost:7070")
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

(defun arxana-evidence--normalize-base (base)
  (let ((value (string-remove-suffix "/" (or base ""))))
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
    (with-current-buffer buffer
      (goto-char (point-min))
      (let ((status (or (and (boundp 'url-http-response-status) url-http-response-status) 0)))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (parsed (arxana-evidence--parse-json body)))
          (kill-buffer buffer)
          (if (and (>= status 200) (< status 300))
              parsed
            (user-error "Evidence API error %s (%s)" status url)))))))

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

(defun arxana-browser--evidence-session-items (context)
  (let* ((session-id (plist-get context :session-id))
         (entries (or (plist-get context :entries)
                      (when (and (stringp session-id) (not (string= session-id "(none)")))
                        (arxana-evidence--fetch-evidence
                         `(("session-id" . ,session-id)
                           ("limit" . ,arxana-evidence-thread-limit)))))))
    (if (and entries (listp entries) (> (length entries) 0))
        (mapcar #'arxana-evidence--entry->item entries)
      (list (list :type 'info
                  :label "No evidence in session"
                  :description (format "Session %s has no visible entries." (or session-id "(none)")))))))

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

(provide 'arxana-browser-evidence)
;;; arxana-browser-evidence.el ends here
