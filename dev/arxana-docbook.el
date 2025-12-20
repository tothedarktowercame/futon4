;;; arxana-docbook.el --- Futon doc book browser -*- lexical-binding: t; -*-

;;; Commentary:
;; Browse filesystem-backed doc book entries (pilot) inside Emacs. Entries are
;; stored under dev/logs/books/<book>/raw/*.json with matching stubs in
;; dev/logs/books/<book>/stubs/*.org. This view keeps a separation between the
;; reading buffer and the source files (a “yad”/hand separation), but still lets
;; you jump to the underlying artifacts when needed.
;;
;; TODO(org-sync): Mirror doc book browser into spine2.org once the UI/fields
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

(defvar-local arxana-docbook--book nil
  "Current doc book identifier (e.g., futon4) for the browser buffer.")

(defvar-local arxana-docbook--source :filesystem
  "Current doc book data source (:storage, :filesystem, :state).")

(defun arxana-docbook--locate-books-root ()
  (or arxana-docbook-books-root
      (let* ((base (or load-file-name buffer-file-name default-directory))
             (root (and base (locate-dominating-file base "dev/logs/books"))))
        (when root
          (expand-file-name "dev/logs/books" root)))))

(defun arxana-docbook--remote-base-url ()
  (when (and (boundp 'futon4-base-url) futon4-base-url)
    (replace-regexp-in-string "/+$" "" futon4-base-url)))

(defun arxana-docbook--sync-enabled-p ()
  (and (boundp 'futon4-enable-sync)
       futon4-enable-sync))

(defun arxana-docbook--remote-available-p ()
  (and arxana-docbook-remote-enabled
       (arxana-docbook--sync-enabled-p)
       (arxana-docbook--remote-base-url)))

(defun arxana-docbook--filesystem-available-p (&optional book)
  (let* ((root (arxana-docbook--locate-books-root))
         (path (cond
                ((and root book) (expand-file-name book root))
                (root root))))
    (and path (file-directory-p path))))

(defun arxana-docbook--data-source (&optional book)
  (cond
   ((arxana-docbook--remote-available-p) :storage)
   ((arxana-docbook--filesystem-available-p book) :filesystem)
   (t :state)))

(defun arxana-docbook--source-label (&optional book source)
  (let* ((source (or source (arxana-docbook--data-source book)))
         (text (pcase source
                 (:storage "Storage (synced)")
                 (:filesystem "Filesystem (scratch)")
                 (_ "State (ephemeral)")))
         (face (pcase source
                 (:storage 'arxana-docbook-source-green)
                 (:filesystem 'arxana-docbook-source-amber)
                 (_ 'arxana-docbook-source-red))))
    (propertize (format "Source: %s" text) 'face face)))

(defun arxana-docbook--source-brief (&optional book source)
  (let* ((source (or source (arxana-docbook--data-source book)))
         (label (pcase source
                  (:storage "G")
                  (:filesystem "A")
                  (_ "R")))
         (face (pcase source
                 (:storage 'arxana-docbook-source-green)
                 (:filesystem 'arxana-docbook-source-amber)
                 (_ 'arxana-docbook-source-red))))
    (propertize (format "Src:%s" label) 'face face)))

(defun arxana-docbook--header-line ()
  (let ((label (arxana-docbook--source-label arxana-docbook--book arxana-docbook--source)))
    (format "Doc book %s — %s"
            (or arxana-docbook--book "unknown")
            label)))

(defun arxana-docbook--http-json (path)
  (let ((base (arxana-docbook--remote-base-url)))
    (when base
      (let* ((url-request-method "GET")
             (url (concat base path))
             (buf (url-retrieve-synchronously url t t 5)))
        (when (buffer-live-p buf)
          (unwind-protect
              (with-current-buffer buf
                (goto-char (point-min))
                (when (re-search-forward "\n\n" nil t)
                  (let ((json-object-type 'plist)
                        (json-array-type 'list)
                        (json-key-type 'keyword))
                    (json-read))))
            (kill-buffer buf)))))))

(defun arxana-docbook--normalize-remote-entry (entry)
  (list :doc-id (or (plist-get entry :doc/id) (plist-get entry :doc-id))
        :book (or (plist-get entry :doc/book) (plist-get entry :doc-book))
        :version (plist-get entry :doc/version)
        :timestamp (plist-get entry :doc/timestamp)
        :files (plist-get entry :doc/files)
        :summary (or (plist-get entry :doc/summary)
                     (plist-get entry :doc/context)
                     (plist-get entry :doc/delta))
        :status (plist-get entry :doc/status)
        :heading (plist-get entry :doc/heading)
        :entry entry))

(defun arxana-docbook--remote-contents (book)
  (when-let* ((data (arxana-docbook--http-json (format "/docs/%s/contents" book)))
              (headings (plist-get data :headings)))
    (mapcar (lambda (h)
              (list :type 'docbook-heading
                    :doc-id (or (plist-get h :doc/id) (plist-get h :doc-id))
                    :title (or (plist-get h :doc/title) (plist-get h :title))
                    :outline (plist-get h :doc/outline_path)
                    :path_string (plist-get h :doc/path_string)
                    :level (plist-get h :doc/level)
                    :book book
                    :latest (plist-get h :doc/latest)))
            headings)))

(defun arxana-docbook--remote-heading (book doc-id)
  (when-let* ((data (arxana-docbook--http-json (format "/docs/%s/heading/%s" book doc-id)))
              (entries (plist-get data :doc/entries)))
    (mapcar #'arxana-docbook--normalize-remote-entry entries)))

(defun arxana-docbook--remote-recent (book)
  (when-let* ((data (arxana-docbook--http-json (format "/docs/%s/recent" book)))
              (entries (plist-get data :entries)))
    (mapcar #'arxana-docbook--normalize-remote-entry entries)))

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
  (let* ((remote (when (arxana-docbook--remote-available-p)
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

(defun arxana-docbook--entry-content (entry)
  (let ((stub (arxana-docbook--read-stub entry)))
    (cond
     (stub (arxana-docbook--strip-stub-header stub))
     ((plist-get entry :summary) (plist-get entry :summary))
     (t ""))))

(defun arxana-docbook--demote-org (text)
  (replace-regexp-in-string "^\\(\\*+\\) " "*\\1 " (or text "")))

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

(defun arxana-docbook--toc-doc-id (heading)
  (or (plist-get heading :doc-id)
      (plist-get heading :doc_id)
      (plist-get heading :doc/id)))

(defun arxana-docbook--lab-addition-label (session-id)
  (if session-id
      (format "Lab addition (%s)" session-id)
    "Lab addition"))

(defun arxana-docbook--render-merged-heading (book heading entries)
  (let* ((doc-id (plist-get heading :doc-id))
         (title (arxana-docbook--heading-title heading))
         (entries (or entries '()))
         (base-entry (or (arxana-docbook--latest-non-lab entries)
                         (car entries)))
         (lab-entries (arxana-docbook--lab-drafts entries))
         (remote-entries (when (and (not base-entry)
                                    (arxana-docbook--remote-available-p))
                           (ignore-errors (arxana-docbook--remote-heading book doc-id))))
         (merged (append entries remote-entries))
         (base-entry (or base-entry
                         (arxana-docbook--latest-non-lab merged)
                         (car merged)))
         (lab-entries (or lab-entries (arxana-docbook--lab-drafts merged))))
    (insert (format "* %s\n" title))
    (if base-entry
        (insert (arxana-docbook--demote-org (arxana-docbook--entry-content base-entry)) "\n")
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
         (source (arxana-docbook--data-source book))
         (entries (pcase source
                    (:storage (or (arxana-docbook--remote-recent book) '()))
                    (:filesystem (or (arxana-docbook--entries-for book) '()))
                    (_ '()))))
    (setq arxana-docbook--source source)
    (setq tabulated-list-entries (arxana-docbook--tabulated-entries entries))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (force-mode-line-update)))

(defun arxana-docbook--toc-path (book)
  (let* ((root (arxana-docbook--locate-books-root))
         (dir (and root (expand-file-name book root))))
    (and dir (expand-file-name "toc.json" dir))))

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

(defun arxana-docbook--toc-for-view (book)
  (or (when (arxana-docbook--remote-available-p)
        (ignore-errors (arxana-docbook--remote-contents book)))
      (arxana-docbook--toc book)
      '()))

(defun arxana-docbook--render-entry (entry)
  (let* ((book (plist-get entry :book))
         (doc-id (plist-get entry :doc-id))
         (buf (get-buffer-create (format "*DocBook:%s/%s*" book doc-id)))
         (entries (arxana-docbook--entries-for-doc book doc-id))
         (title (or (plist-get entry :title) doc-id)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: %s\n\n" (or title doc-id "")))
        (arxana-docbook--render-merged-heading book
                                               (list :doc-id doc-id :title title)
                                               entries)
        (goto-char (point-min))
        (org-show-all)
        (view-mode 1)))
    (pop-to-buffer buf)))

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
    (arxana-docbook--render-entry entry)))

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
  (arxana-docbook--render-entry entry))

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

;;;###autoload
;;;###autoload
(defun arxana-docbook-browse-futon4 ()
  "Shortcut to browse the futon4 doc book."
  (interactive)
  (arxana-docbook-browse "futon4"))

(provide 'arxana-docbook)

;;; arxana-docbook.el ends here
