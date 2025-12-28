;;; arxana-docbook.el --- Futon doc book browser -*- lexical-binding: t; -*-

;;; Commentary:
;; Browse filesystem-backed doc book entries (pilot) inside Emacs. Entries are
;; stored under dev/logs/books/<book>/raw/*.json with matching stubs in
;; dev/logs/books/<book>/stubs/*.org. This view keeps a separation between the
;; reading buffer and the source files (a “yad”/hand separation), but still lets
;; you jump to the underlying artifacts when needed.
;;
;; TODO(org-sync): Mirror doc book browser into XTDB docs once the UI/fields
;; stabilize.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'tabulated-list)
(require 'org)
(require 'url)

(defgroup arxana-docbook nil
  "Browse doc book entries for Futon systems."
  :group 'arxana)

(defface arxana-docbook-source-green
  '((t :foreground "ForestGreen" :weight bold))
  "Face for storage-backed docbook sources."
  :group 'arxana-docbook)

(defface arxana-docbook-source-amber
  '((t :foreground "DarkGoldenrod" :weight bold))
  "Face for filesystem-backed docbook sources."
  :group 'arxana-docbook)

(defface arxana-docbook-source-red
  '((t :foreground "IndianRed" :weight bold))
  "Face for state-only docbook sources."
  :group 'arxana-docbook)

(defface arxana-docbook-new-content
  '((t :background "#2b2b2b"))
  "Face for lab-draft additions in docbook views."
  :group 'arxana-docbook)

(defcustom arxana-docbook-books-root nil
  "Root directory containing doc book folders (e.g., dev/logs/books)."
  :type '(choice (const :tag "Auto-detect" nil)
                 directory)
  :group 'arxana-docbook)

(defcustom arxana-docbook-remote-enabled t
  "When non-nil, prefer the Futon API (futon4-base-url) for doc browsing when sync is enabled."
  :type 'boolean
  :group 'arxana-docbook)

(defcustom arxana-docbook-remote-preserve-order t
  "When non-nil, trust remote TOC order as returned by /docs/:book/contents."
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

(defcustom arxana-docbook-org-export-directory nil
  "Directory for docbook Org exports (nil uses dev/logs/books/<book>/export)."
  :type '(choice (const :tag "Auto" nil)
                 directory)
  :group 'arxana-docbook)

(defcustom arxana-docbook-write-filesystem-toc nil
  "When non-nil, allow writing toc.json as a filesystem byproduct."
  :type 'boolean
  :group 'arxana-docbook)

(defvar-local arxana-docbook--book nil
  "Current doc book identifier (e.g., futon4) for the browser buffer.")

(defvar-local arxana-docbook--source :filesystem
  "Current doc book data source (:storage, :filesystem, :state).")

(defvar-local arxana-docbook--storage-probe nil
  "Cached probe data for the remote docbook source.")

(defvar-local arxana-docbook--filesystem-probe nil
  "Cached probe data for the filesystem docbook source.")

(defvar-local arxana-docbook--entry-book nil
  "Book name for the current docbook entry buffer.")

(defvar-local arxana-docbook--entry-doc-id nil
  "Doc id for the current docbook entry buffer.")

(defvar-local arxana-docbook--entry-entry-id nil
  "Entry id for the current docbook entry buffer.")

(defvar-local arxana-docbook--entry-toc nil
  "Cached TOC list for the current docbook entry buffer.")

(defvar-local arxana-docbook--return-buffer nil
  "Buffer to return to when exiting a docbook entry view.")

(defvar-local arxana-docbook--return-doc-id nil
  "Doc id to select when returning to the browser.")

(defvar-local arxana-docbook--return-entry-id nil
  "Entry id to select when returning to the browser.")


(defconst arxana-docbook--entry-buffer "*Arxana Docbook*"
  "Buffer name for the main docbook entry view.")

(defconst arxana-docbook--source-buffer "*Arxana Docbook Source*"
  "Buffer name for the docbook source view.")

(defun arxana-docbook--cleanup-entry-buffers ()
  (dolist (buf (buffer-list))
    (when (string-match-p "^\\*Arxana Docbook\\*<" (buffer-name buf))
      (kill-buffer buf))))

(defun arxana-docbook--frame-window-for-buffer (buffer)
  (seq-find (lambda (win)
              (eq (window-buffer win) buffer))
            (window-list (selected-frame) 'no-mini)))

(defun arxana-docbook--ensure-two-up (doc-buffer &optional source-buffer)
  (let* ((frame (selected-frame))
         (doc-window (or (arxana-docbook--frame-window-for-buffer doc-buffer)
                         (selected-window)))
         (source-buffer (or source-buffer
                            (get-buffer-create arxana-docbook--source-buffer)))
         (source-window (or (arxana-docbook--frame-window-for-buffer source-buffer)
                            (window-in-direction 'right doc-window)
                            (split-window doc-window nil 'right))))
    (set-window-dedicated-p doc-window nil)
    (set-window-dedicated-p source-window nil)
    (set-window-buffer doc-window doc-buffer)
    (set-window-buffer source-window source-buffer)
    (set-window-dedicated-p doc-window t)
    (set-window-dedicated-p source-window t)
    (select-window doc-window)
    (dolist (win (window-list frame 'no-mini))
      (when (and (not (eq win doc-window))
                 (eq (window-buffer win) doc-buffer))
        (set-window-buffer win (get-buffer-create arxana-docbook--source-buffer))))))

(defun arxana-docbook--locate-books-root ()
  (or arxana-docbook-books-root
      (let* ((base (or load-file-name buffer-file-name default-directory))
             (root (and base (locate-dominating-file base "dev/logs/books"))))
        (when root
          (expand-file-name "dev/logs/books" root)))))

(defun arxana-docbook--repo-root ()
  (let* ((base (or load-file-name buffer-file-name))
         (root (and base (locate-dominating-file base "dev"))))
    (or root
        (when-let* ((books (arxana-docbook--locate-books-root)))
          (expand-file-name "../../.." books))
        (locate-dominating-file default-directory "dev/logs/books")
        (locate-dominating-file default-directory "dev")
        default-directory)))

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

(defun arxana-docbook--filesystem-available-p (&optional book)
  (let* ((root (arxana-docbook--locate-books-root))
         (path (cond
                ((and root book) (expand-file-name book root))
                (root root))))
    (and path (file-directory-p path))))

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

(defun arxana-docbook--header-line ()
  (let ((label (arxana-docbook--source-label arxana-docbook--book arxana-docbook--source))
        (summary (arxana-docbook--probe-summary arxana-docbook--book)))
    (format "Doc book %s — %s — %s"
            (or arxana-docbook--book "unknown")
            label
            summary)))

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

(defun arxana-docbook--toc-remove-doc-id (book doc-id)
  "Remove DOC-ID from the filesystem toc.json for BOOK.
Return non-nil when the toc file was updated."
  (let* ((path (arxana-docbook--toc-path book))
         (json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword))
    (when (and path (file-readable-p path))
      (let* ((toc (json-read-file path))
             (filtered (and (listp toc)
                            (seq-filter (lambda (heading)
                                          (not (equal doc-id
                                                      (or (plist-get heading :doc_id)
                                                          (plist-get heading :doc-id)))))
                                        toc))))
        (when (and (listp filtered) (/= (length filtered) (length toc)))
          (let ((json-encoding-pretty-print t))
            (with-temp-file path
              (insert (json-encode filtered))
              (insert "\n")))
          t)))))

(defun arxana-docbook--filesystem-delete-doc (book doc-id)
  "Delete filesystem docbook entries for DOC-ID and return deleted paths."
  (let* ((entries (ignore-errors (arxana-docbook-entries book)))
         (matches (seq-filter (lambda (entry)
                                (equal doc-id (plist-get entry :doc-id)))
                              (or entries '())))
         (deleted '()))
    (dolist (entry matches)
      (let* ((raw (plist-get entry :raw-path))
             (run-id (plist-get entry :run-id))
             (stub (or (plist-get entry :stub-path)
                       (and raw run-id
                            (expand-file-name (format "../stubs/%s.org" run-id)
                                              (file-name-directory raw))))))
        (when (and raw (file-exists-p raw))
          (delete-file raw)
          (push raw deleted))
        (when (and stub (file-exists-p stub))
          (delete-file stub)
          (push stub deleted))))
    (when (and matches (arxana-docbook--filesystem-available-p book))
      (arxana-docbook--toc-remove-doc-id book doc-id))
    (nreverse deleted)))

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

(defun arxana-docbook--available-books ()
  (let* ((root (arxana-docbook--locate-books-root)))
    (when (and root (file-directory-p root))
      (seq-sort #'string<
                (seq-filter
                 (lambda (entry)
                   (and (not (member entry '("." "..")))
                        (file-directory-p (expand-file-name entry root))))
                 (directory-files root))))))

(defun arxana-docbook--read-json (path)
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-key-type 'keyword))
    (condition-case err
        (json-read-file path)
      (error
       (message "[arxana-docbook] Failed to read %s: %s" path (error-message-string err))
       nil))))

(defun arxana-docbook--entry-from-json (book path)
  (let* ((payload (arxana-docbook--read-json path))
         (run-id (or (plist-get payload :run_id)
                     (file-name-base path)))
         (stub (expand-file-name (format "stubs/%s.org" run-id)
                                 (file-name-directory (directory-file-name (file-name-directory path)))))
         (files (plist-get payload :files_touched))
         (outline (plist-get payload :outline_path)))
    (when payload
      (list :book book
            :doc-id (plist-get payload :doc_id)
            :run-id run-id
            :version (plist-get payload :version)
            :timestamp (plist-get payload :timestamp)
            :summary (plist-get payload :agent_summary)
            :outline outline
            :files (and (listp files) files)
            :raw-path path
            :stub-path (and (file-readable-p stub) stub)))))

(defun arxana-docbook--entry-version (entry)
  (or (plist-get entry :version)
      (plist-get entry :doc/version)))

(defun arxana-docbook--lab-draft-p (entry)
  (string= (arxana-docbook--entry-version entry) "lab-draft"))

(defun arxana-docbook--entries-for-doc (book doc-id)
  (let* ((remote (when (arxana-docbook--remote-available-p book)
                   (ignore-errors (arxana-docbook--remote-heading book doc-id))))
         (local (ignore-errors (arxana-docbook-entries book)))
         (entries (append remote local)))
    (seq-filter (lambda (entry)
                  (equal doc-id (plist-get entry :doc-id)))
                entries)))

(defun arxana-docbook--latest-non-lab (entries)
  (let ((filtered (seq-filter (lambda (entry) (not (arxana-docbook--lab-draft-p entry)))
                              entries)))
    (car (seq-sort (lambda (a b)
                     (string> (or (plist-get a :timestamp) "")
                              (or (plist-get b :timestamp) "")))
                   filtered))))

(defun arxana-docbook--lab-drafts (entries)
  (seq-filter #'arxana-docbook--lab-draft-p entries))

(defun arxana-docbook--session-id-from-run (run-id)
  (when (and run-id (stringp run-id))
    (car (split-string run-id "-" t))))

(defun arxana-docbook--read-stub (entry)
  (let ((stub (plist-get entry :stub-path)))
    (when (and stub (file-readable-p stub))
      (with-temp-buffer
        (let ((coding-system-for-read 'utf-8-unix))
          (insert-file-contents stub))
        (buffer-string)))))

(defun arxana-docbook--strip-stub-header (text)
  (let* ((lines (split-string (or text "") "\n"))
         (start (cl-position-if (lambda (line)
                                  (string-prefix-p "*" line))
                                lines)))
    (if start
        (string-join (nthcdr start lines) "\n")
      (or text ""))))

(defun arxana-docbook--strip-org-metadata (text)
  (let* ((lines (split-string (or text "") "\n"))
         (start (cl-position-if-not (lambda (line)
                                      (string-match-p "^#\\+" line))
                                    lines)))
    (if start
        (string-join (nthcdr start lines) "\n")
      (or text ""))))

(defun arxana-docbook--strip-org-code-blocks (text)
  (let ((lines (split-string (or text "") "\n"))
        (result '())
        (skip nil))
    (dolist (line lines)
      (cond
       ((string-match-p "^#\\+BEGIN_SRC" line)
        (setq skip t))
       ((string-match-p "^#\\+END_SRC" line)
        (setq skip nil))
       ((not skip)
        (push line result))))
    (string-join (nreverse result) "\n")))

(defun arxana-docbook--entry-content (entry)
  (let ((stub (arxana-docbook--read-stub entry)))
    (cond
     (stub (arxana-docbook--strip-org-metadata
            (arxana-docbook--strip-org-code-blocks
             (arxana-docbook--strip-stub-header stub))))
     ((plist-get entry :summary-raw)
      (arxana-docbook--strip-org-metadata
       (arxana-docbook--strip-org-code-blocks (plist-get entry :summary-raw))))
     ((plist-get entry :summary)
      (arxana-docbook--strip-org-metadata
       (arxana-docbook--strip-org-code-blocks (plist-get entry :summary))))
     (t ""))))

(defun arxana-docbook--entry-raw-text (entry)
  (or (arxana-docbook--read-stub entry)
      (plist-get entry :summary-raw)
      (plist-get entry :summary)
      ""))

(defun arxana-docbook--entry-function-name (entry)
  (or (plist-get entry :function-name)
      (plist-get entry :doc/function-name)
      (when-let* ((raw (plist-get entry :entry)))
        (or (plist-get raw :doc/function-name)
            (plist-get raw :doc/function_name)))))

(defun arxana-docbook--entry-source-path (entry)
  "Return a source file path hinted by ENTRY content, if any."
  (let* ((raw (plist-get entry :entry))
         (source (or (plist-get entry :source-path)
                     (plist-get entry :doc/source-path)
                     (and raw (or (plist-get raw :doc/source-path)
                                  (plist-get raw :doc/source_path))))))
    (or source
        (let ((text (arxana-docbook--entry-raw-text entry)))
          (or (when (string-match ":tangle[[:space:]]+\\([^[:space:]]+\\)" text)
                (match-string 1 text))
              (when (string-match "^#\\+INCLUDE:[[:space:]]+\"\\([^\"]+\\)\"" text)
                (match-string 1 text))
              (when (string-match "code>[[:space:]]+\\([^[:space:]]+\\)" text)
                (match-string 1 text)))))))

;; TODO(org-sync): Track docbook function browsing/jump UI in XTDB docs (see org-sync-tracker).
(defun arxana-docbook--jump-to-function (buffer name)
  (when (and buffer name)
    (with-current-buffer buffer
      (goto-char (point-min))
      (let* ((pattern (format "^(\\(cl-\\)?def\\(un\\|macro\\|subst\\|alias\\|generic\\|method\\)\\s-+%s\\_>"
                              (regexp-quote name)))
             (found (re-search-forward pattern nil t)))
        (when found
          (beginning-of-line))))))

(defun arxana-docbook--source-link-line (entry)
  (when-let* ((path (arxana-docbook--entry-source-path entry)))
    (format "- Source: [[file:%s][%s]]\n" path path)))

(defun arxana-docbook--demote-org (text)
  (replace-regexp-in-string "^\\(\\*+\\) " "*\\1 " (or text "")))

(defun arxana-docbook--demote-org-by (text steps)
  (let ((out (or text "")))
    (dotimes (_ (max 0 steps))
      (setq out (arxana-docbook--demote-org out)))
    out))

(defun arxana-docbook--export-context ()
  "Return plist with :host-root and :book inferred from the current buffer file."
  (when buffer-file-name
    (when (string-match "\\(.*\\)/dev/logs/books/\\([^/]+\\)/" buffer-file-name)
      (list :host-root (match-string 1 buffer-file-name)
            :book (match-string 2 buffer-file-name)))))

(defun arxana-docbook--resolve-include-path (path base-dir)
  "Resolve PATH for #+INCLUDE relative to BASE-DIR and known repo roots."
  (let* ((base-dir (or base-dir default-directory))
         (repo-root (file-name-as-directory (arxana-docbook--repo-root)))
         (ctx (arxana-docbook--export-context))
         (host-root (plist-get ctx :host-root))
         (book (plist-get ctx :book))
         (candidates
          (delq nil
                (list
                 (when (file-name-absolute-p path) path)
                 (expand-file-name path base-dir)
                 (and (string-prefix-p "dev/" path)
                      (expand-file-name path repo-root))
                 (and host-root book (string-prefix-p "dev/" path)
                      (expand-file-name path
                                        (file-name-as-directory
                                         (expand-file-name book host-root))))))))
    (or (seq-find #'file-readable-p candidates)
        path)))

(defun arxana-docbook--rewrite-include-paths (text base-dir)
  "Rewrite relative Org include paths in TEXT to absolute paths under BASE-DIR."
  (replace-regexp-in-string
   "^#\\+INCLUDE:[ \t]+\"\\([^\"]+\\)\"\\(.*\\)$"
   (lambda (line)
     (if (string-match "^#\\+INCLUDE:[ \t]+\"\\([^\"]+\\)\"\\(.*\\)$" line)
         (let* ((path (match-string 1 line))
                (rest (match-string 2 line))
                (abs (arxana-docbook--resolve-include-path path base-dir)))
           (format "#+INCLUDE: \"%s\"%s" abs rest))
       line))
   (or text "") t t))

(defun arxana-docbook--entries-by-doc-id (entries)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (when-let* ((doc-id (plist-get entry :doc-id)))
        (puthash doc-id (cons entry (gethash doc-id table)) table)))
    table))

(defun arxana-docbook--heading-title (heading)
  (or (plist-get heading :title)
      (plist-get heading :doc/title)
      (plist-get heading :doc_title)
      (plist-get heading :doc-id)))

(defun arxana-docbook--heading-outline (heading)
  (or (plist-get heading :outline)
      (plist-get heading :doc/outline_path)
      (plist-get heading :outline_path)))

(defun arxana-docbook--heading-path-string (heading)
  (or (plist-get heading :path_string)
      (plist-get heading :doc/path_string)
      (plist-get heading :path-string)))

(defun arxana-docbook--toc-doc-id (heading)
  (or (plist-get heading :doc-id)
      (plist-get heading :doc_id)
      (plist-get heading :doc/id)))

(defun arxana-docbook--heading-level (heading)
  (or (plist-get heading :level)
      (let ((outline (arxana-docbook--heading-outline heading)))
        (and (listp outline) (length outline)))
      1))

(defun arxana-docbook--outline-key (heading)
  (or (arxana-docbook--heading-outline heading)
      (let ((path (arxana-docbook--heading-path-string heading)))
        (and path (split-string path " / " t)))))

(defun arxana-docbook--outline-less-p (a b)
  "Return non-nil when A should sort before B by outline path."
  (let* ((oa (or (arxana-docbook--outline-key a) '()))
         (ob (or (arxana-docbook--outline-key b) '()))
         (len-a (length oa))
         (len-b (length ob))
         (min-len (min len-a len-b))
         (idx 0)
         (result nil)
         (done nil))
    (while (and (not done) (< idx min-len))
      (let* ((seg-a (format "%s" (nth idx oa)))
             (seg-b (format "%s" (nth idx ob))))
        (cond
         ((string< seg-a seg-b)
          (setq result t done t))
         ((string< seg-b seg-a)
          (setq result nil done t))
         (t (setq idx (1+ idx))))))
    (unless done
      (cond
       ((/= len-a len-b)
        (setq result (< len-a len-b)))
       (t
        (let* ((id-a (format "%s" (arxana-docbook--toc-doc-id a)))
               (id-b (format "%s" (arxana-docbook--toc-doc-id b))))
          (setq result (string< id-a id-b))))))
    result))

(defun arxana-docbook--normalize-remote-toc (toc)
  "Return a TOC list for remote sources, sorted when configured."
  (cond
   ((not (listp toc)) toc)
   (arxana-docbook-remote-preserve-order toc)
   (t (sort (copy-sequence toc) #'arxana-docbook--outline-less-p))))

(defun arxana-docbook--order-headings (headings order)
  (let ((by-id (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (ordered '()))
    (dolist (heading headings)
      (when-let* ((doc-id (arxana-docbook--toc-doc-id heading)))
        (puthash doc-id heading by-id)))
    (dolist (doc-id order)
      (let ((heading (gethash doc-id by-id)))
        (when heading
          (puthash doc-id t seen)
          (push heading ordered))))
    (dolist (heading headings)
      (let ((doc-id (arxana-docbook--toc-doc-id heading)))
        (when (and doc-id (not (gethash doc-id seen)))
          (push heading ordered))))
    (nreverse ordered)))

(defun arxana-docbook--lab-addition-label (session-id)
  (if session-id
      (format "Lab addition (%s)" session-id)
    "Lab addition"))

(defun arxana-docbook--descendant-headings (book heading)
  (let* ((toc (arxana-docbook--toc-for-view book))
         (outline (arxana-docbook--heading-outline heading)))
    (when (and outline (listp outline))
      (seq-filter
       (lambda (candidate)
         (let ((child (arxana-docbook--heading-outline candidate)))
           (and (listp child)
                (> (length child) (length outline))
                (equal outline (seq-take child (length outline))))))
       toc))))

(defun arxana-docbook--first-descendant-entry (book heading)
  (catch 'found
    (dolist (child (arxana-docbook--descendant-headings book heading))
      (let* ((doc-id (arxana-docbook--toc-doc-id child))
             (entries (and doc-id (arxana-docbook--entries-for-doc book doc-id)))
             (entry (and entries (arxana-docbook--latest-non-lab entries)))
             (content (and entry (arxana-docbook--entry-content entry))))
        (when (and content (not (string-empty-p (string-trim content))))
          (throw 'found (list :heading child :entry entry :content content))))))
  nil)

(defun arxana-docbook--render-merged-heading (book heading entries)
  (let* ((doc-id (plist-get heading :doc-id))
         (title (arxana-docbook--heading-title heading))
         (entries (or entries '()))
         (base-entry (or (arxana-docbook--latest-non-lab entries)
                         (car entries)))
         (lab-entries (arxana-docbook--lab-drafts entries))
         (remote-entries (when (and (not base-entry)
                                    (arxana-docbook--remote-available-p book))
                           (ignore-errors (arxana-docbook--remote-heading book doc-id))))
         (merged (append entries remote-entries))
         (base-entry (or base-entry
                         (arxana-docbook--latest-non-lab merged)
                         (car merged)))
         (lab-entries (or lab-entries (arxana-docbook--lab-drafts merged))))
    (insert (format "* %s\n" title))
    (if base-entry
        (let* ((content (arxana-docbook--entry-content base-entry))
               (trimmed (string-trim (or content "")))
               (source-line (arxana-docbook--source-link-line base-entry)))
          (if (string-empty-p trimmed)
              (if-let* ((desc (arxana-docbook--first-descendant-entry book heading))
                        (child-title (arxana-docbook--heading-title (plist-get desc :heading)))
                        (child-content (plist-get desc :content))
                        (child-entry (plist-get desc :entry))
                        (child-source (arxana-docbook--source-link-line child-entry)))
                  (progn
                    (insert (format "** %s\n" child-title))
                    (when child-source
                      (insert child-source))
                    (insert (arxana-docbook--demote-org child-content) "\n"))
                (insert "- (no entry content yet)\n"))
            (when source-line
              (insert source-line))
            (insert (arxana-docbook--demote-org content) "\n")))
      (insert "- (no base entry yet)\n"))
    (when (and lab-entries (listp lab-entries))
      (dolist (lab lab-entries)
        (let* ((start (point))
               (run-id (plist-get lab :run-id))
               (session-id (arxana-docbook--session-id-from-run run-id))
               (lab-body (arxana-docbook--entry-content lab)))
          (insert (format "\n- %s\n"
                          (arxana-docbook--lab-addition-label session-id)))
          (insert (arxana-docbook--demote-org lab-body) "\n")
          (let ((end (point)))
            (add-text-properties start end
                                 (list 'face 'arxana-docbook-new-content
                                       'arxana-new-content t
                                       'line-prefix (propertize "> " 'face 'arxana-docbook-new-content)
                                       'wrap-prefix (propertize "> " 'face 'arxana-docbook-new-content)))))))
    (insert "\n")))

(defun arxana-docbook--export-heading (book heading)
  (let* ((doc-id (arxana-docbook--toc-doc-id heading))
         (title (arxana-docbook--heading-title heading))
         (level (max 1 (or (arxana-docbook--heading-level heading) 1)))
         (stars (make-string level ?*))
         (entries (and doc-id (arxana-docbook--entries-for-doc book doc-id)))
         (base-entry (and entries (arxana-docbook--latest-non-lab entries)))
         (content (and base-entry (arxana-docbook--entry-content base-entry)))
         (trimmed (string-trim (or content "")))
         (source-line (and base-entry (arxana-docbook--source-link-line base-entry))))
    (insert (format "%s %s\n" stars (or title doc-id "")))
    (if (and trimmed (not (string-empty-p trimmed)))
        (progn
          (when source-line
            (insert source-line))
          (let* ((root (arxana-docbook--repo-root))
                 (content (arxana-docbook--rewrite-include-paths content root)))
            (insert (arxana-docbook--demote-org-by content 1) "\n")))
      (if-let* ((desc (arxana-docbook--first-descendant-entry book heading))
                (child-title (arxana-docbook--heading-title (plist-get desc :heading)))
                (child-content (plist-get desc :content))
                (child-entry (plist-get desc :entry))
                (child-source (arxana-docbook--source-link-line child-entry)))
          (progn
            (insert (format "%s %s\n" (make-string (1+ level) ?*) child-title))
            (when child-source
              (insert child-source))
            (let* ((root (arxana-docbook--repo-root))
                   (child-content (arxana-docbook--rewrite-include-paths child-content root)))
              (insert (arxana-docbook--demote-org-by child-content 2) "\n")))
        (insert "- (no entry content yet)\n")))
    (insert "\n")))

(defun arxana-docbook-export-org-book (&optional book output-file order)
  "Export docbook BOOK into a single Org file at OUTPUT-FILE.
ORDER, when provided, reorders TOC headings by doc-id."
  (interactive)
  (let* ((book (or book arxana-docbook--book
                   (car (arxana-docbook--available-books))
                   "futon4"))
         (default (arxana-docbook--default-org-export-path book))
         (output-file (or output-file
                          (read-file-name "Export Org file: "
                                          (file-name-directory default)
                                          default nil
                                          (file-name-nondirectory default)))))
    (let* ((toc (arxana-docbook--toc-for-view book))
           (headings (if (and order (listp order))
                         (arxana-docbook--order-headings toc order)
                       toc)))
      (with-temp-buffer
        (insert (format "#+TITLE: Docbook %s\n" book))
        (insert (format "#+DATE: %s\n\n" (format-time-string "%Y-%m-%d")))
        (dolist (heading headings)
          (when heading
            (arxana-docbook--export-heading book heading)))
        (write-region (point-min) (point-max) output-file))
      (message "Wrote docbook Org export to %s" output-file)
      output-file)))

(defun arxana-docbook--export-org-to-pdf (org-file &optional pdf-file)
  "Export ORG-FILE to PDF, returning the PDF path."
  (require 'ox-latex)
  (let* ((org-file (expand-file-name org-file))
         (default-directory (file-name-directory org-file))
         (pdf-file (or pdf-file (concat (file-name-sans-extension org-file) ".pdf")))
         (org-export-show-temporary-export-buffer nil)
         (org-export-in-background nil)
         (org-export-with-broken-links 'mark))
    (with-current-buffer (find-file-noselect org-file)
      (let ((org-export-before-parsing-functions
             (cons #'arxana-docbook--normalize-include-paths
                   org-export-before-parsing-functions))
            (exported (org-latex-export-to-pdf)))
        (unless exported
          (error "Org LaTeX export did not produce a PDF"))
        (setq exported (expand-file-name exported))
        (setq pdf-file (expand-file-name pdf-file))
        (unless (string= exported pdf-file)
          (copy-file exported pdf-file t))
        pdf-file))))

(defun arxana-docbook--normalize-include-paths (&optional backend)
  "Rewrite #+INCLUDE paths to absolute paths when possible."
  (ignore backend)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^#\\+INCLUDE: \"\\([^\"]+\\)\"" nil t)
      (let* ((raw (match-string 1))
             (resolved (arxana-docbook--resolve-include-path raw default-directory)))
        (when (and resolved (not (string= raw resolved)))
          (replace-match resolved t t nil 1))))))

(defun arxana-docbook-export-pdf-book (&optional book org-file pdf-file order)
  "Export docbook BOOK to Org (ORG-FILE) and then to PDF (PDF-FILE).
ORDER, when provided, reorders TOC headings by doc-id."
  (interactive)
  (let* ((book (or book arxana-docbook--book
                   (car (arxana-docbook--available-books))
                   "futon4"))
         (org-file (or org-file (arxana-docbook--default-org-export-path book)))
         (pdf-file (or pdf-file (concat (file-name-sans-extension org-file) ".pdf")))
         (org-path (arxana-docbook-export-org-book book org-file order)))
    (arxana-docbook--export-org-to-pdf org-path pdf-file)
    (message "Wrote docbook PDF export to %s" pdf-file)
    pdf-file))

(defun arxana-docbook-open-book (&optional book)
  "Open a compiled docbook view for BOOK using filesystem entries."
  (interactive)
  (let* ((book (or book arxana-docbook--book
                   (car (arxana-docbook--available-books))))
         (toc (arxana-docbook--toc-for-view book))
         (entries (or (arxana-docbook-entries book) '()))
         (by-doc (arxana-docbook--entries-by-doc-id entries))
         (toc-docs (mapcar (lambda (h) (plist-get h :doc-id)) toc))
         (extra-docs (seq-filter (lambda (doc-id) (not (member doc-id toc-docs)))
                                 (hash-table-keys by-doc)))
         (buf (get-buffer-create (format "*DocBook Book:%s*" book))))
    (arxana-docbook--cleanup-entry-buffers)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: DocBook %s\n\n" book))
        (dolist (heading toc)
          (let* ((doc-id (plist-get heading :doc-id))
                 (entries (gethash doc-id by-doc)))
            (arxana-docbook--render-merged-heading book heading entries)))
        (when (and extra-docs (listp extra-docs))
          (insert "* Lab additions\n\n")
          (dolist (doc-id extra-docs)
            (let* ((entries (gethash doc-id by-doc))
                   (title (or (and entries
                                   (plist-get (car entries) :outline)
                                   (car (plist-get (car entries) :outline)))
                              doc-id)))
              (arxana-docbook--render-merged-heading book
                                                     (list :doc-id doc-id :title title)
                                                     entries))))
        (goto-char (point-min))
        (org-show-all)
        (visual-line-mode 1)
        (view-mode 1)))
    (pop-to-buffer buf)))

(defun arxana-docbook-open-section-context (&optional book doc-id &rest args)
  "Open a contextual docbook view around DOC-ID for BOOK (prev/current/next)."
  (interactive)
  (let* ((toc-index (car args))
         (book (or book arxana-docbook--book
                   (car (arxana-docbook--available-books))))
         (toc (arxana-docbook--toc-for-view book))
         (entries (or (arxana-docbook-entries book) '()))
         (by-doc (arxana-docbook--entries-by-doc-id entries))
         (doc-ids (mapcar #'arxana-docbook--toc-doc-id toc))
         (doc-id (or doc-id (car doc-ids)))
         (idx (or toc-index
                  (cl-position doc-id doc-ids :test #'equal)))
         (window-ids (delq nil (if idx
                                   (list (and (> idx 0) (nth (1- idx) doc-ids))
                                         doc-id
                                         (and (< idx (1- (length doc-ids))) (nth (1+ idx) doc-ids)))
                                 (list doc-id))))
         (buf (get-buffer-create (format "*DocBook Section:%s/%s*" book doc-id))))
    (unless doc-id
      (user-error "No doc-id available for section context"))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: DocBook %s (context)\n\n" book))
        (dolist (did window-ids)
          (let* ((heading (seq-find (lambda (h) (equal (arxana-docbook--toc-doc-id h) did)) toc))
                 (entries (or (gethash did by-doc)
                              (arxana-docbook--entries-for-doc book did)))
                 (title (or (and heading (arxana-docbook--heading-title heading))
                            (and entries
                                 (plist-get (car entries) :outline)
                                 (car (plist-get (car entries) :outline)))
                            did))
                 (heading (or heading (list :doc-id did :title title))))
            (arxana-docbook--render-merged-heading book heading entries)))
        (goto-char (point-min))
        (org-show-all)
        (visual-line-mode 1)
        (view-mode 1)))
    (pop-to-buffer buf)))

(defun arxana-docbook--entries-for (book)
  (let* ((root (arxana-docbook--locate-books-root))
         (book-dir (and root (expand-file-name book root)))
         (raw-dir (and book-dir (expand-file-name "raw" book-dir))))
    (when (and raw-dir (file-directory-p raw-dir))
      (let ((entries (delq nil
                           (mapcar (lambda (path)
                                     (when (string-match-p "\\.json\\'" path)
                                       (arxana-docbook--entry-from-json book path)))
                                   (directory-files raw-dir t nil t)))))
        (seq-sort
         (lambda (a b)
           (string> (or (plist-get a :timestamp) "")
                    (or (plist-get b :timestamp) "")))
         entries)))))

(defun arxana-docbook--tabulated-entries (entries)
  (mapcar
   (lambda (entry)
     (let* ((doc-id (or (plist-get entry :doc-id) ""))
            (version (or (plist-get entry :version) ""))
            (timestamp (or (plist-get entry :timestamp) ""))
            (files (plist-get entry :files))
            (summary (or (plist-get entry :summary) "")))
       (list entry
             (vector doc-id
                     version
                     timestamp
                     (format "%d" (length (or files '())))
                     (truncate-string-to-width summary 96 nil nil t)))))
   entries))

(defun arxana-docbook--refresh ()
  (let* ((book arxana-docbook--book)
         (storage (arxana-docbook--probe-storage book))
         (filesystem (arxana-docbook--probe-filesystem book))
         (source (arxana-docbook--data-source book))
         (entries (pcase source
                    (:storage (if (memq (plist-get storage :status) '(:ok :empty))
                                  (or (arxana-docbook--remote-recent book) '())
                                '()))
                    (:filesystem (if (eq (plist-get filesystem :status) :ok)
                                     (or (arxana-docbook--entries-for book) '())
                                   '()))
                    (_ '()))))
    (setq arxana-docbook--storage-probe storage)
    (setq arxana-docbook--filesystem-probe filesystem)
    (setq arxana-docbook--source source)
    (setq tabulated-list-entries (arxana-docbook--tabulated-entries entries))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (force-mode-line-update)))

(defun arxana-docbook--goto-entry (doc-id &optional entry-id)
  "Move point to DOC-ID (or ENTRY-ID) in the current docbook browser buffer."
  (when (and (or doc-id entry-id) (derived-mode-p 'arxana-docbook-mode))
    (goto-char (point-min))
    (forward-line 1)
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (let ((entry (tabulated-list-get-id)))
          (when (and entry
                     (or (and entry-id (equal entry-id (plist-get entry :entry-id)))
                         (and doc-id (equal doc-id (plist-get entry :doc-id)))))
            (setq found t))
          (unless found
            (forward-line 1))))
      found)))

(defun arxana-docbook--toc-path (book)
  (let* ((root (arxana-docbook--locate-books-root))
         (dir (and root (expand-file-name book root))))
    (and dir (expand-file-name "toc.json" dir))))

(defun arxana-docbook--export-dir (book)
  (or arxana-docbook-org-export-directory
      (let* ((root (arxana-docbook--locate-books-root))
             (dir (and root (expand-file-name book root))))
        (when dir
          (setq dir (expand-file-name "export" dir))
          (make-directory dir t)
          dir))))

(defun arxana-docbook--default-org-export-path (book)
  (let ((dir (arxana-docbook--export-dir book)))
    (if dir
        (expand-file-name (format "%s.org" book) dir)
      (expand-file-name (format "docbook-%s.org" book)
                        (arxana-docbook--repo-root)))))

(defun arxana-docbook--toc (book)
  "Return list of heading plists for BOOK from toc.json."
  (let* ((path (arxana-docbook--toc-path book))
         (json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword))
    (when (and path (file-readable-p path))
      (condition-case err
          (json-read-file path)
        (error
         (message "[arxana-docbook] Failed to read toc %s: %s" path (error-message-string err))
         nil)))))

(defun arxana-docbook--toc-write-order (book order)
  "Rewrite toc.json for BOOK to match ORDER (list of doc-ids).
Return non-nil when the file was updated."
  (let* ((path (arxana-docbook--toc-path book))
         (json-object-type 'plist)
         (json-array-type 'list)
         (json-key-type 'keyword))
    (when (and path (file-readable-p path))
      (let* ((toc (json-read-file path))
             (ordered (and (listp toc) (arxana-docbook--order-headings toc order))))
        (when (and ordered (/= (length ordered) (length toc)))
          (message "[arxana-docbook] TOC order missing %d headings"
                   (- (length toc) (length ordered))))
        (when (and ordered (listp toc))
          (let ((json-encoding-pretty-print t))
            (with-temp-file path
              (insert (json-encode ordered))
              (insert "\n")))
          t)))))

(defun arxana-docbook--toc-write-headings (book headings &optional order)
  "Write HEADINGS as toc.json for BOOK, reordering by ORDER when provided."
  (let* ((path (or (arxana-docbook--toc-path book)
                   (expand-file-name (format "dev/logs/books/%s/toc.json" book)
                                     (arxana-docbook--repo-root))))
         (dir (and path (file-name-directory path))))
    (unless (and dir (file-directory-p dir))
      (when dir
        (make-directory dir t)))
    (unless (and headings (listp headings))
      (error "No toc headings available to write"))
    (let* ((ordered (if (and order (listp order))
                        (arxana-docbook--order-headings headings order)
                      headings))
           (json-encoding-pretty-print t))
      (with-temp-file path
        (insert (json-encode ordered))
        (insert "\n")))
    t))

(defun arxana-docbook--toc-for-view (book)
  (let ((remote (when (arxana-docbook--remote-available-p book)
                  (ignore-errors (arxana-docbook--remote-contents book)))))
    (cond
     (remote (arxana-docbook--normalize-remote-toc remote))
     (t (or (arxana-docbook--toc book) '())))))

(defun arxana-docbook--heading-for-doc-id (book doc-id)
  (when doc-id
    (seq-find (lambda (heading)
                (equal doc-id (arxana-docbook--toc-doc-id heading)))
              (arxana-docbook--toc-for-view book))))

(defun arxana-docbook--dedupe-preserve-order (items)
  (let ((seen (make-hash-table :test 'equal))
        (result '()))
    (dolist (item items)
      (unless (gethash item seen)
        (puthash item t seen)
        (push item result)))
    (nreverse result)))

(defun arxana-docbook--entry-doc-ids (book)
  (let* ((toc (arxana-docbook--toc-for-view book))
         (toc-ids (delq nil (mapcar #'arxana-docbook--toc-doc-id toc))))
    (if toc-ids
        (arxana-docbook--dedupe-preserve-order toc-ids)
      (arxana-docbook--dedupe-preserve-order
       (delq nil (mapcar (lambda (entry) (plist-get entry :doc-id))
                         (arxana-docbook--entries-for book)))))))

(defun arxana-docbook--render-entry (entry &optional return-buffer return-entry-id)
  (let* ((book (plist-get entry :book))
         (doc-id (plist-get entry :doc-id))
         (buf (get-buffer-create arxana-docbook--entry-buffer))
         (entries (arxana-docbook--entries-for-doc book doc-id))
         (heading (arxana-docbook--heading-for-doc-id book doc-id))
         (base-title (or (plist-get entry :title)
                         (and heading (arxana-docbook--heading-title heading))
                         doc-id))
         (function-name (arxana-docbook--entry-function-name entry))
         (title (if function-name
                    (format "%s — %s" function-name base-title)
                  base-title)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (let ((doc-ids (arxana-docbook--entry-doc-ids book)))
          (unless (member doc-id doc-ids)
            (setq doc-ids (append doc-ids (list doc-id))))
          (setq arxana-docbook--entry-book book
                arxana-docbook--entry-doc-id doc-id
                arxana-docbook--entry-entry-id (plist-get entry :entry-id)
                arxana-docbook--entry-toc doc-ids
                arxana-docbook--return-buffer return-buffer
                arxana-docbook--return-doc-id doc-id
                arxana-docbook--return-entry-id return-entry-id))
        (insert (format "#+TITLE: %s\n\n" (or title doc-id "")))
        (arxana-docbook--render-merged-heading book
                                               (list :doc-id doc-id :title base-title)
                                               entries)
        (goto-char (point-min))
        (org-show-all)
        (visual-line-mode 1)
        (view-mode 1)
        (arxana-docbook-entry-mode 1)
        (setq-local minor-mode-overriding-map-alist
                    `((arxana-docbook-entry-mode . ,arxana-docbook-entry-mode-map)))))
    (let* ((source (arxana-docbook--entry-source-path entry))
           (source-path (and source (expand-file-name source (arxana-docbook--repo-root))))
           (source-buf (when (and source-path (file-readable-p source-path))
                         (find-file-noselect source-path))))
      (arxana-docbook--ensure-two-up buf source-buf)
      (when (and function-name source-buf)
        (arxana-docbook--jump-to-function source-buf function-name)))))

(defun arxana-docbook--entry-next-doc-id ()
  (let* ((doc-id arxana-docbook--entry-doc-id)
         (doc-ids (or arxana-docbook--entry-toc '()))
         (idx (cl-position doc-id doc-ids :test #'equal)))
    (when (and idx (< (1+ idx) (length doc-ids)))
      (nth (1+ idx) doc-ids))))

(defun arxana-docbook--entry-prev-doc-id ()
  (let* ((doc-id arxana-docbook--entry-doc-id)
         (doc-ids (or arxana-docbook--entry-toc '()))
         (idx (cl-position doc-id doc-ids :test #'equal)))
    (when (and idx (> idx 0))
      (nth (1- idx) doc-ids))))

(defun arxana-docbook-next-entry ()
  "Open the next docbook entry in TOC order."
  (interactive)
  (let* ((book arxana-docbook--entry-book)
         (next-id (arxana-docbook--entry-next-doc-id)))
    (unless (and book next-id)
      (user-error "No next docbook entry"))
    (let* ((entries (arxana-docbook--entries-for-doc book next-id))
           (entry (or (arxana-docbook--latest-non-lab entries)
                      (car entries)
                      (list :book book :doc-id next-id))))
      (arxana-docbook--render-entry entry))))

(defun arxana-docbook-prev-entry ()
  "Open the previous docbook entry in TOC order."
  (interactive)
  (let* ((book arxana-docbook--entry-book)
         (prev-id (arxana-docbook--entry-prev-doc-id)))
    (unless (and book prev-id)
      (user-error "No previous docbook entry"))
    (let* ((entries (arxana-docbook--entries-for-doc book prev-id))
           (entry (or (arxana-docbook--latest-non-lab entries)
                      (car entries)
                      (list :book book :doc-id prev-id))))
      (arxana-docbook--render-entry entry))))

(defun arxana-docbook-open-uri (uri)
  "Open a docbook URI like docbook://BOOK/DOC-ID[/ENTRY-ID]."
  (interactive "sDocbook URI: ")
  (let* ((clean (string-trim uri))
         (parts (split-string (string-remove-prefix "docbook://" clean) "/" t))
         (book (nth 0 parts))
         (doc-id (nth 1 parts))
         (entry-id (nth 2 parts)))
    (unless (and book doc-id)
      (user-error "Invalid docbook URI: %s" uri))
    (let* ((entries (arxana-docbook--entries-for-doc book doc-id))
           (entry (or (and entry-id
                           (seq-find (lambda (e)
                                       (equal entry-id (plist-get e :entry-id)))
                                     entries))
                      (arxana-docbook--latest-non-lab entries)
                      (car entries)
                      (list :book book :doc-id doc-id))))
      (arxana-docbook--render-entry entry))))

(defun arxana-docbook--view-raw (entry)
  (let* ((buf (get-buffer-create (format "*DocBook raw:%s/%s*" (plist-get entry :book) (plist-get entry :run-id))))
         (path (plist-get entry :raw-path)))
    (unless (and path (file-readable-p path))
      (user-error "No readable raw JSON for this entry"))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents path)
        (goto-char (point-min))
        (js-mode))
      (view-mode 1))
    (pop-to-buffer buf)))

(defun arxana-docbook-open-entry ()
  "Open the doc book stub/summary at point in a read-only view buffer."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry
      (user-error "No entry at point"))
    (arxana-docbook--render-entry entry (current-buffer) (plist-get entry :entry-id))))

(defun arxana-docbook-open-raw ()
  "Open the raw JSON log for the entry at point."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry
      (user-error "No entry at point"))
    (arxana-docbook--view-raw entry)))

;;;###autoload
(defun arxana-docbook-entries (book)
  "Return a list of doc book entries for BOOK."
  (arxana-docbook--entries-for book))

;;;###autoload
(defun arxana-docbook-open-entry-object (entry)
  "Open ENTRY (plist) in a read-only buffer."
  (arxana-docbook--render-entry entry (current-buffer) (plist-get entry :entry-id)))

;;;###autoload
(defun arxana-docbook-open-entry-raw (entry)
  "Open the raw JSON backing ENTRY (plist)."
  (arxana-docbook--view-raw entry))

(defvar arxana-docbook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'arxana-docbook-open-entry)
    (define-key map (kbd "o") #'arxana-docbook-open-entry)
    (define-key map (kbd "v") #'arxana-docbook-open-raw)
    map)
  "Keymap for `arxana-docbook-mode'.")

 (defun arxana-docbook-copy-location ()
  "Copy a location identifier for the current docbook entry."
  (interactive)
  (let* ((book arxana-docbook--entry-book)
         (doc-id arxana-docbook--entry-doc-id)
         (entry-id arxana-docbook--entry-entry-id)
         (location (cond
                    ((and book doc-id entry-id)
                     (format "docbook://%s/%s/%s" book doc-id entry-id))
                    ((and book doc-id)
                     (format "docbook://%s/%s" book doc-id))
                    (t "docbook://unknown"))))
    (kill-new location)
    (message "Copied %s" location)))

(defvar arxana-docbook-entry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'arxana-docbook-next-entry)
    (define-key map (kbd "p") #'arxana-docbook-prev-entry)
    (define-key map (kbd "b") #'arxana-docbook-return-to-browser)
    (define-key map (kbd "y") #'arxana-docbook-copy-location)
    map)
  "Keymap for `arxana-docbook-entry-mode'.")

(define-minor-mode arxana-docbook-entry-mode
  "Minor mode for docbook entry buffers."
  :keymap arxana-docbook-entry-mode-map)

(define-derived-mode arxana-docbook-mode tabulated-list-mode "ArxDocBook"
  "Browser for filesystem-backed doc book entries."
  (setq tabulated-list-format [("Doc" 26 t)
                               ("Version" 18 t)
                               ("When" 20 t)
                               ("Files" 8 t)
                               ("Summary" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq header-line-format '(:eval (arxana-docbook--header-line)))
  (tabulated-list-init-header))

;;;###autoload
;;;###autoload
(defun arxana-docbook-browse (&optional book)
  "Browse doc book entries for BOOK (default: futon4)."
  (interactive)
  (let* ((books (or (arxana-docbook--available-books) '("futon4")))
         (book (or book
                   (and (cdr books)
                        (completing-read "Doc book: " books nil t nil nil (car books)))
                   (car books))))
    (unless book
      (user-error "No doc books found under dev/logs/books"))
    (let ((buf (get-buffer-create (format "*DocBook:%s*" book))))
      (with-current-buffer buf
        (arxana-docbook-mode)
        (setq arxana-docbook--book book)
        (arxana-docbook--refresh))
      (pop-to-buffer buf))))

(defun arxana-docbook-return-to-browser ()
  "Return to the docbook browser and highlight the current entry."
  (interactive)
  (let* ((book arxana-docbook--entry-book)
         (doc-id arxana-docbook--return-doc-id)
         (entry-id arxana-docbook--return-entry-id)
         (return-buffer arxana-docbook--return-buffer))
    (unless (and book doc-id)
      (user-error "No current docbook entry to return from"))
    (cond
     ((and (buffer-live-p return-buffer)
           (with-current-buffer return-buffer
             (derived-mode-p 'arxana-browser-mode)))
      (with-current-buffer return-buffer
        (when (fboundp 'arxana-browser--render)
          (arxana-browser--render))
        (when (fboundp 'arxana-browser--goto-doc-id)
          (arxana-browser--goto-doc-id doc-id))))
     (t
      (let ((buf (get-buffer-create (format "*DocBook:%s*" book))))
        (with-current-buffer buf
          (arxana-docbook-mode)
          (setq arxana-docbook--book book)
          (arxana-docbook--refresh)
          (arxana-docbook--goto-entry doc-id entry-id))
        (setq return-buffer buf))))
    (when (buffer-live-p return-buffer)
      (pop-to-buffer return-buffer))))

;;;###autoload
(defun arxana-docbook-browse-futon4 ()
  "Shortcut to browse the futon4 doc book."
  (interactive)
  (arxana-docbook-browse "futon4"))

(provide 'arxana-docbook)

;;; arxana-docbook.el ends here
