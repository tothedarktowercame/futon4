;;; arxana-docbook-remote.el --- Docbook remote helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Remote API helpers for docbook entries and TOC operations.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'url)

(declare-function arxana-docbook--locate-books-root "arxana-docbook-core")
(declare-function arxana-docbook--filesystem-available-p "arxana-docbook-core" (&optional book))

(defvar arxana-docbook--storage-probe nil)
(defvar arxana-docbook--filesystem-probe nil)

(defcustom arxana-docbook-remote-enabled t
  "When non-nil, prefer the Futon API (futon4-base-url) for doc browsing when sync is enabled."
  :type 'boolean
  :group 'arxana-docbook)
(defcustom arxana-docbook-remote-delete-path-format "/docs/%s/doc/%s"
  "Format string for docbook delete endpoint (book, doc-id)."
  :type 'string
  :group 'arxana-docbook)
(defcustom arxana-docbook-remote-delete-method "DELETE"
  "HTTP method used when deleting docbook entries from the remote API."
  :type 'string
  :group 'arxana-docbook)
(defvar arxana-docbook-last-delete-errors nil
  "Details from the last failed remote docbook delete attempt.")
(defcustom arxana-docbook-remote-delete-toc-path-format "/docs/%s/toc/%s"
  "Format string for docbook TOC delete endpoint (book, doc-id)."
  :type 'string
  :group 'arxana-docbook)
(defcustom arxana-docbook-remote-order-path-format "/docs/%s/contents/order"
  "Format string for docbook TOC order endpoint (book)."
  :type 'string
  :group 'arxana-docbook)
(defcustom arxana-docbook-remote-order-method "POST"
  "HTTP method used when updating remote docbook TOC order."
  :type 'string
  :group 'arxana-docbook)
(defun arxana-docbook--remote-base-url ()
  (when (and (boundp 'futon4-base-url) futon4-base-url)
    (replace-regexp-in-string "/+$" "" futon4-base-url)))
(defun arxana-docbook--sync-enabled-p ()
  (and (boundp 'futon4-enable-sync)
       futon4-enable-sync))
(defun arxana-docbook--remote-enabled-p ()
  (and arxana-docbook-remote-enabled
       (arxana-docbook--sync-enabled-p)
       (arxana-docbook--remote-base-url)))
(defun arxana-docbook--remote-available-p (&optional book)
  (let* ((probe (or arxana-docbook--storage-probe
                    (arxana-docbook--probe-storage book)))
         (status (plist-get probe :status)))
    (when (and (eq status :empty)
               (arxana-docbook--remote-enabled-p))
      (setq probe (arxana-docbook--probe-storage book))
      (setq arxana-docbook--storage-probe probe)
      (setq status (plist-get probe :status)))
    (memq status '(:ok :empty))))
(defun arxana-docbook--http-response (path &optional method)
  "Return a plist with :status, :data, and :error for PATH."
  (let ((base (arxana-docbook--remote-base-url))
        (method (or method "GET")))
    (when base
      (let* ((url-request-method method)
             (url (concat base path))
             (buf (url-retrieve-synchronously url t t 5)))
        (when (buffer-live-p buf)
          (unwind-protect
              (with-current-buffer buf
                (let ((status (and (boundp 'url-http-response-status)
                                   url-http-response-status)))
                  (goto-char (point-min))
                  (if (not (re-search-forward "\n\n" nil t))
                      (list :status status :error "No HTTP body")
                    (decode-coding-region (point) (point-max) 'utf-8-unix t)
                    (let ((json-object-type 'plist)
                          (json-array-type 'list)
                          (json-key-type 'keyword))
                      (condition-case err
                          (list :status status :data (json-read))
                        (error (list :status status
                                     :error (error-message-string err))))))))
            (kill-buffer buf)))))))
(defun arxana-docbook--probe-storage (&optional book)
  "Probe the remote docbook API for BOOK and return a status plist."
  (let ((book (or book "futon4")))
    (if (not (arxana-docbook--remote-enabled-p))
        (list :status :disabled)
      (let* ((path (format "/docs/%s/toc" book))
             (resp (arxana-docbook--http-response path))
             (status (plist-get resp :status))
             (data (plist-get resp :data)))
        (cond
         ((null resp) (list :status :unreachable :error "No response"))
         ((and status (/= status 200))
          (list :status :unreachable
                :status-code status
                :error (plist-get resp :error)))
         ((not data) (list :status :unreachable :error "No data"))
         (t (let* ((headings (plist-get data :headings))
                   (count (length headings))
                   (state (if (> count 0) :ok :empty)))
              (list :status state
                    :headings count
                    :contents data))))))))
(defun arxana-docbook--probe-filesystem (&optional book)
  "Probe the filesystem docbook root for BOOK and return a status plist."
  (let* ((root (arxana-docbook--locate-books-root))
         (path (cond
                ((and root book) (expand-file-name book root))
                (root root))))
    (cond
     ((not root) (list :status :missing))
     ((not (file-directory-p path)) (list :status :missing :path path))
     (t (list :status :ok :path path)))))
(defun arxana-docbook--data-source (&optional book)
  (let ((remote (or arxana-docbook--storage-probe
                    (arxana-docbook--probe-storage book)))
        (filesystem (or arxana-docbook--filesystem-probe
                        (arxana-docbook--probe-filesystem book))))
    (cond
     ((memq (plist-get remote :status) '(:ok :empty)) :storage)
     ((eq (plist-get filesystem :status) :ok) :filesystem)
     (t :state))))
(defun arxana-docbook--source-label (&optional book source)
  (let* ((source (or source (arxana-docbook--data-source book)))
         (storage-status (plist-get (or arxana-docbook--storage-probe
                                        (arxana-docbook--probe-storage book))
                                    :status))
         (filesystem-status (plist-get (or arxana-docbook--filesystem-probe
                                           (arxana-docbook--probe-filesystem book))
                                       :status))
         (text (pcase source
                 (:storage (pcase storage-status
                             (:ok "Storage (synced)")
                             (:empty "Storage (empty)")
                             (:disabled "Storage (disabled)")
                             (:unreachable "Storage (unreachable)")
                             (_ "Storage (unknown)")))
                 (:filesystem (pcase filesystem-status
                                 (:ok "Filesystem (scratch)")
                                 (:missing "Filesystem (missing)")
                                 (_ "Filesystem (unknown)")))
                 (_ "State (ephemeral)")))
         (face (pcase source
                 (:storage (pcase storage-status
                             ((or :ok :empty) 'arxana-docbook-source-green)
                             (_ 'arxana-docbook-source-red)))
                 (:filesystem (pcase filesystem-status
                                 (:ok 'arxana-docbook-source-amber)
                                 (_ 'arxana-docbook-source-red)))
                 (_ 'arxana-docbook-source-red))))
    (propertize (format "Source: %s" text) 'face face)))
(defun arxana-docbook--source-brief (&optional book source)
  (let* ((source (or source (arxana-docbook--data-source book)))
         (storage-status (plist-get (or arxana-docbook--storage-probe
                                        (arxana-docbook--probe-storage book))
                                    :status))
         (filesystem-status (plist-get (or arxana-docbook--filesystem-probe
                                           (arxana-docbook--probe-filesystem book))
                                       :status))
         (label (pcase source
                  (:storage (if (memq storage-status '(:ok :empty)) "G" "R"))
                  (:filesystem (if (eq filesystem-status :ok) "A" "R"))
                  (_ "R")))
         (face (pcase source
                 (:storage (if (memq storage-status '(:ok :empty))
                               'arxana-docbook-source-green
                             'arxana-docbook-source-red))
                 (:filesystem (if (eq filesystem-status :ok)
                                  'arxana-docbook-source-amber
                                'arxana-docbook-source-red))
                 (_ 'arxana-docbook-source-red))))
    (propertize (format "Src:%s" label) 'face face)))
(defun arxana-docbook--probe-summary (&optional book)
  (let* ((storage (or arxana-docbook--storage-probe
                      (arxana-docbook--probe-storage book)))
         (filesystem (or arxana-docbook--filesystem-probe
                         (arxana-docbook--probe-filesystem book)))
         (storage-label (pcase (plist-get storage :status)
                          (:ok (format "Storage ok (%d)" (or (plist-get storage :headings) 0)))
                          (:empty "Storage empty")
                          (:disabled "Storage disabled")
                          (:unreachable "Storage unreachable")
                          (_ "Storage unknown")))
         (filesystem-label (pcase (plist-get filesystem :status)
                             (:ok "FS ok")
                             (:missing "FS missing")
                             (_ "FS unknown"))))
    (format "%s | %s" storage-label filesystem-label)))
(defun arxana-docbook--http-json (path)
  (let* ((resp (arxana-docbook--http-response path))
         (status (plist-get resp :status))
         (data (plist-get resp :data)))
    (when (and resp (or (null status) (= status 200)))
      data)))
(defun arxana-docbook--http-json-request (path method payload)
  "Return a plist with :status, :data, and :error for PATH using METHOD and PAYLOAD."
  (let ((base (arxana-docbook--remote-base-url)))
    (when base
      (let* ((url-request-method (or method "POST"))
             (url-request-extra-headers '(("Content-Type" . "application/json")))
             (url-request-data (and payload (encode-coding-string (json-encode payload) 'utf-8)))
             (url (concat base path))
             (buf (url-retrieve-synchronously url t t 5)))
        (when (buffer-live-p buf)
          (unwind-protect
              (with-current-buffer buf
                (let ((status (and (boundp 'url-http-response-status)
                                   url-http-response-status)))
                  (goto-char (point-min))
                  (if (not (re-search-forward "\n\n" nil t))
                      (list :status status :error "No HTTP body")
                    (decode-coding-region (point) (point-max) 'utf-8-unix t)
                    (let ((json-object-type 'plist)
                          (json-array-type 'list)
                          (json-key-type 'keyword))
                      (condition-case err
                          (list :status status :data (json-read))
                        (error (list :status status
                                     :error (error-message-string err))))))))
            (kill-buffer buf)))))))
(defun arxana-docbook--remote-update-toc-order (book order)
  "Update remote TOC order for BOOK using ORDER (list of doc-ids)."
  (when (and book (listp order) (arxana-docbook--remote-enabled-p))
    (let* ((path (format arxana-docbook-remote-order-path-format book))
           (payload (list (cons "order" order)))
           (resp (arxana-docbook--http-json-request path arxana-docbook-remote-order-method payload))
           (status (plist-get resp :status)))
      (if (and (numberp status) (<= 200 status 299))
          (list :status :ok :response resp)
        (list :status :error :response resp
              :error (or (plist-get resp :error)
                         (and (plist-get resp :data)
                              (plist-get (plist-get resp :data) :error))
                         status))))))
(defun arxana-docbook--remote-delete-doc (book doc-id)
  "Delete DOC-ID from the remote docbook store."
  (when (and book doc-id (arxana-docbook--remote-enabled-p))
    (let* ((path (format arxana-docbook-remote-delete-path-format book doc-id))
           (resp (arxana-docbook--http-response path arxana-docbook-remote-delete-method))
           (status (plist-get resp :status)))
      (if (and (numberp status) (<= 200 status 299))
          (list :status :ok :response resp)
        (list :status :error :response resp
              :error (or (plist-get resp :error)
                         (and (plist-get resp :data)
                              (plist-get (plist-get resp :data) :error))
                         status))))))
(defun arxana-docbook--remote-delete-toc (book doc-id &optional cascade)
  "Delete DOC-ID from the remote docbook TOC.
When CASCADE is non-nil, request that entries are deleted too."
  (when (and book doc-id (arxana-docbook--remote-enabled-p))
    (let* ((path (format arxana-docbook-remote-delete-toc-path-format book doc-id))
           (path (if cascade (concat path "?cascade=true") path))
           (resp (arxana-docbook--http-response path arxana-docbook-remote-delete-method))
           (status (plist-get resp :status)))
      (if (and (numberp status) (<= 200 status 299))
          (list :status :ok :response resp)
        (list :status :error :response resp
              :error (or (plist-get resp :error)
                         (and (plist-get resp :data)
                              (plist-get (plist-get resp :data) :error))
                         status))))))
(defun arxana-docbook--normalize-remote-entry (entry)
  (let* ((entry-id (or (plist-get entry :doc/entry-id)
                       (plist-get entry :doc_entry_id)
                       (plist-get entry :doc/entry_id)))
         (raw-summary (or (plist-get entry :doc/summary)
                          (plist-get entry :doc/body)
                          (plist-get entry :doc/context)
                          (plist-get entry :doc/delta)))
         (summary (when raw-summary
                    (replace-regexp-in-string
                     "[[:space:]\n]+" " "
                     (string-trim raw-summary)))))
    (when entry-id
      (list :doc-id (or (plist-get entry :doc/id) (plist-get entry :doc-id))
            :entry-id entry-id
            :book (or (plist-get entry :doc/book) (plist-get entry :doc-book))
            :version (plist-get entry :doc/version)
            :timestamp (plist-get entry :doc/timestamp)
            :files (plist-get entry :doc/files)
            :summary summary
            :summary-raw raw-summary
            :status (plist-get entry :doc/status)
            :heading (plist-get entry :doc/heading)
            :function-name (or (plist-get entry :doc/function-name)
                               (plist-get entry :doc/function_name))
            :source-path (or (plist-get entry :doc/source-path)
                             (plist-get entry :doc/source_path))
            :entry entry))))
(defun arxana-docbook--remote-contents (book)
  (when-let* ((data (or (arxana-docbook--http-json (format "/docs/%s/toc" book))
                        (arxana-docbook--http-json (format "/docs/%s/contents" book))))
              (headings (plist-get data :headings)))
    (mapcar (lambda (h)
              (list :type 'docbook-heading
                    :doc-id (or (plist-get h :doc/id) (plist-get h :doc-id))
                    :title (or (plist-get h :doc/title) (plist-get h :title))
                    :outline (or (plist-get h :doc/outline_path) (plist-get h :outline_path))
                    :path_string (or (plist-get h :doc/path_string) (plist-get h :path_string))
                    :level (or (plist-get h :doc/level) (plist-get h :level))
                    :book book
                    :latest (plist-get h :doc/latest)))
            (seq-filter (lambda (h)
                          (or (plist-get h :doc/outline_path)
                              (plist-get h :outline_path)
                              (plist-get h :doc/level)
                              (plist-get h :level)))
                        headings))))
(defun arxana-docbook--remote-heading (book doc-id)
  (when-let* ((data (arxana-docbook--http-json (format "/docs/%s/heading/%s" book doc-id)))
              (entries (plist-get data :doc/entries)))
    (delq nil (mapcar #'arxana-docbook--normalize-remote-entry entries))))
(defun arxana-docbook--remote-recent (book)
  (when-let* ((data (arxana-docbook--http-json (format "/docs/%s/recent" book)))
              (entries (plist-get data :entries)))
    (delq nil (mapcar #'arxana-docbook--normalize-remote-entry entries))))

(provide 'arxana-docbook-remote)
;;; arxana-docbook-remote.el ends here
