;;; arxana-docbook-checkout.el --- Checkout docbook entries -*- lexical-binding: t; -*-

;;; Commentary:
;; Fetch remote docbook entries and write stubs into the repo checkout.

;;; Code:

(require 'cl-lib)
(require 'arxana-docbook-core)
(require 'arxana-docbook-remote)
(require 'arxana-docbook-toc)
(require 'arxana-store)
(require 'subr-x)

(declare-function arxana-docbook--available-books "arxana-docbook-core")
(declare-function arxana-docbook--entries-for-doc "arxana-docbook-core" (book doc-id))
(declare-function arxana-docbook--latest-non-lab "arxana-docbook-core" (entries))
(declare-function arxana-docbook--locate-books-root "arxana-docbook-core")
(declare-function arxana-docbook--remote-available-p "arxana-docbook-remote" (&optional book))
(declare-function arxana-docbook--remote-contents "arxana-docbook-remote" (book))
(declare-function arxana-docbook--remote-heading "arxana-docbook-remote" (book doc-id))
(declare-function arxana-docbook--normalize-remote-entry "arxana-docbook-remote" (entry))
(declare-function arxana-docbook--toc-write-headings "arxana-docbook-toc" (book headings &optional order))
(declare-function arxana-docbook--heading-title "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--heading-outline "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--toc-doc-id "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--heading-path-string "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--heading-for-doc-id "arxana-docbook-toc" (book doc-id))
(declare-function arxana-store--request "arxana-store" (method path &optional payload query))
(declare-function arxana-store-sync-enabled-p "arxana-store")

(defconst arxana-docbook--stub-clean-face
  '((t :foreground "ForestGreen" :weight bold))
  "Face used for clean docbook stub headers.")
(defconst arxana-docbook--stub-dirty-face
  '((t :foreground "orange" :weight bold))
  "Face used for dirty docbook stub headers.")

(defvar-local arxana-docbook--stub-doc-id nil
  "Doc id for the current docbook stub buffer.")
(defvar-local arxana-docbook--stub-entry-id nil
  "Entry id for the current docbook stub buffer.")
(defvar-local arxana-docbook--stub-book nil
  "Book id for the current docbook stub buffer.")
(defvar-local arxana-docbook--stub-last-sync-error nil
  "Last sync error for the current docbook stub buffer.")

(defun arxana-docbook--stub-metadata-from-buffer ()
  (arxana-docbook--stub-metadata
   (buffer-substring-no-properties (point-min) (point-max))))

(defun arxana-docbook--stub-synced-at ()
  "Return the last sync time for this stub buffer, if recorded."
  (let* ((meta (arxana-docbook--stub-metadata-from-buffer))
         (raw (cdr (assoc "SYNCED_AT" meta))))
    (and raw (ignore-errors (date-to-time raw)))))

(defun arxana-docbook--stub-set-property (key value)
  "Set property KEY to VALUE in the stub's property drawer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^:PROPERTIES:" nil t)
        (let ((start (point)))
          (if (re-search-forward (format "^:%s:[ \t]*\\(.*\\)$" key) nil t)
              (replace-match value t t nil 1)
            (goto-char start)
            (insert (format ":%s: %s\n" key value))))
      (goto-char (point-min))
      (insert ":PROPERTIES:\n"
              (format ":%s: %s\n" key value)
              ":END:\n\n"))))

(defun arxana-docbook--stub-record-sync ()
  "Record the current time as the last successful sync."
  (let ((stamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t)))
    (arxana-docbook--stub-set-property "SYNCED_AT" stamp)
    (save-buffer)
    (set-buffer-modified-p nil)))

(defconst arxana-docbook--sync-fs-skew-seconds 1
  "Allowed filesystem mtime skew (seconds) before marking a stub as fs-dirty.")

(defun arxana-docbook--stub-fs-dirty-p (synced-at file-mtime)
  "Return non-nil when FILE-MTIME is meaningfully newer than SYNCED-AT."
  (and synced-at
       file-mtime
       (time-less-p (time-add synced-at (seconds-to-time arxana-docbook--sync-fs-skew-seconds))
                    file-mtime)))

(defun arxana-docbook--face-safe (face)
  "Return FACE when valid, otherwise `default`."
  (if (or (null face) (eq face t))
      'default
    face))

(defun arxana-docbook--stub-header-line ()
  (let* ((dirty (buffer-modified-p))
         (synced-at (arxana-docbook--stub-synced-at))
         (file-mtime (and buffer-file-name
                          (file-attribute-modification-time
                           (file-attributes buffer-file-name))))
         (fs-dirty (arxana-docbook--stub-fs-dirty-p synced-at file-mtime))
         (status (cond
                  (dirty "Docbook stub [dirty]")
                  (fs-dirty "Docbook stub [fs-dirty]")
                  ((not synced-at) "Docbook stub [unsynced]")
                  (t "Docbook stub [clean]")))
         (sync (cond
                ((not (arxana-store-sync-enabled-p)) " [sync disabled]")
                (arxana-docbook--stub-last-sync-error " [sync error]")
                (t "")))
         (face (if (or dirty arxana-docbook--stub-last-sync-error)
                   arxana-docbook--stub-dirty-face
                 arxana-docbook--stub-clean-face))
         (meta (arxana-docbook--stub-metadata-from-buffer))
         (meta-doc-id (cdr (assoc "DOC_ID" meta)))
         (doc-id (or arxana-docbook--stub-doc-id meta-doc-id "unknown")))
    (propertize (format "%s - %s%s - C-c C-s saves + syncs"
                        status doc-id sync)
                'face (arxana-docbook--face-safe face))))

(defun arxana-docbook--update-stub-header (&rest _args)
  (setq-local header-line-format
              '(:eval (arxana-docbook--stub-header-line))))

(defvar arxana-docbook-stub-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'arxana-docbook-stub-save-sync)
    map)
  "Keymap for `arxana-docbook-stub-mode'.")

(defvar-local arxana-docbook--emulation-map-alist nil
  "Emulation map entry for `arxana-docbook-stub-mode`.")
(defvar-local arxana-docbook--override-map nil
  "Override map entry for `arxana-docbook-stub-mode`.")

(define-minor-mode arxana-docbook-stub-mode
  "Minor mode for editing docbook stub files."
  :keymap arxana-docbook-stub-mode-map
  (if arxana-docbook-stub-mode
      (progn
        (setq-local arxana-docbook--emulation-map-alist
                    (list (cons 'arxana-docbook-stub-mode arxana-docbook-stub-mode-map)))
        (setq-local emulation-mode-map-alists
                    (cons arxana-docbook--emulation-map-alist
                          emulation-mode-map-alists))
        (setq-local arxana-docbook--override-map
                    (cons 'arxana-docbook-stub-mode arxana-docbook-stub-mode-map))
        (setq-local minor-mode-overriding-map-alist
                    (cons arxana-docbook--override-map
                          minor-mode-overriding-map-alist))
        (add-hook 'after-change-functions #'arxana-docbook--update-stub-header nil t)
        (add-hook 'after-save-hook #'arxana-docbook--update-stub-header nil t)
        (arxana-docbook--update-stub-header))
    (remove-hook 'after-change-functions #'arxana-docbook--update-stub-header t)
    (remove-hook 'after-save-hook #'arxana-docbook--update-stub-header t)
    (when arxana-docbook--emulation-map-alist
      (setq-local emulation-mode-map-alists
                  (delq arxana-docbook--emulation-map-alist
                        emulation-mode-map-alists))
      (setq-local arxana-docbook--emulation-map-alist nil))
    (when arxana-docbook--override-map
      (setq-local minor-mode-overriding-map-alist
                  (delq arxana-docbook--override-map
                        minor-mode-overriding-map-alist))
      (setq-local arxana-docbook--override-map nil))))

(defun arxana-docbook--maybe-enable-stub-mode ()
  "Enable `arxana-docbook-stub-mode` for docbook stub files."
  (when (and buffer-file-name
             (string-suffix-p ".org" buffer-file-name))
    (let* ((root (arxana-docbook--locate-books-root))
           (root (and root (file-name-as-directory (expand-file-name root))))
           (abs (expand-file-name buffer-file-name))
           (in-root (and root (string-prefix-p root abs)))
           (docbook-marker nil))
      (save-excursion
        (goto-char (point-min))
        (setq docbook-marker (re-search-forward "^:DOC_ID:" nil t)))
      (when (and docbook-marker (or in-root (not root)))
        (arxana-docbook-stub-mode 1)))))

(add-hook 'org-mode-hook #'arxana-docbook--maybe-enable-stub-mode)

(defun arxana-docbook--stub-book-from-path (path)
  (when path
    (let* ((root (arxana-docbook--locate-books-root))
           (abs (expand-file-name path))
           (root (and root (file-name-as-directory (expand-file-name root)))))
      (when (and root (string-prefix-p root abs))
        (let* ((relative (string-remove-prefix root abs))
               (parts (split-string relative "/" t)))
          (car parts))))))

(defun arxana-docbook--stub-metadata (text)
  (let ((lines (split-string (or text "") "\n"))
        (metadata '())
        (in-props nil))
    (dolist (line lines)
      (cond
       ((string-match-p "^:PROPERTIES:" line) (setq in-props t))
       ((and in-props (string-match-p "^:END:" line)) (setq in-props nil))
       ((and in-props (string-match "^:\\([A-Z0-9_-]+\\):[[:space:]]*\\(.*\\)$" line))
        (push (cons (match-string 1 line)
                    (string-trim (match-string 2 line)))
              metadata))
       ((string-match "^#\\+TITLE:[[:space:]]*\\(.*\\)$" line)
        (push (cons "TITLE" (string-trim (match-string 1 line))) metadata))))
    metadata))

(defun arxana-docbook--stub-sections (text)
  (let ((lines (split-string (or text "") "\n"))
        (current nil)
        (acc '())
        (sections (make-hash-table :test 'equal)))
    (dolist (line lines)
      (if (string-match "^\\*+\\s-+\\(.+\\)$" line)
          (progn
            (when current
              (puthash (downcase (string-trim current))
                       (string-trim (string-join (nreverse acc) "\n"))
                       sections))
            (setq current (match-string 1 line))
            (setq acc '()))
        (when current
          (push line acc))))
    (when current
      (puthash (downcase (string-trim current))
               (string-trim (string-join (nreverse acc) "\n"))
               sections))
    sections))

(defun arxana-docbook--stub-section (sections name)
  (gethash (downcase name) sections))

(defun arxana-docbook--stub-payload-from-buffer ()
  (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
         (metadata (arxana-docbook--stub-metadata text))
         (sections (arxana-docbook--stub-sections text))
         (doc-id (or (cdr (assoc "DOC_ID" metadata))
                     arxana-docbook--stub-doc-id))
         (version (or (cdr (assoc "VERSION" metadata)) "unversioned"))
         (replaces (cdr (assoc "REPLACES" metadata)))
         (title (cdr (assoc "TITLE" metadata)))
         (context (arxana-docbook--stub-section sections "context"))
         (delta (arxana-docbook--stub-section sections "delta"))
         (verification (arxana-docbook--stub-section sections "verification"))
         (book (or arxana-docbook--stub-book
                   (arxana-docbook--stub-book-from-path buffer-file-name)))
         (entry-id (or (cdr (assoc "ENTRY_ID" metadata))
                       arxana-docbook--stub-entry-id
                       (and buffer-file-name (file-name-base buffer-file-name))))
         (heading (and book doc-id (arxana-docbook--heading-for-doc-id book doc-id)))
         (outline (and heading (arxana-docbook--heading-outline heading)))
         (path-string (or (and heading (arxana-docbook--heading-path-string heading))
                          (and outline (string-join outline " / "))
                          title)))
    (unless (and book entry-id)
      (error "Docbook stub missing book or entry id"))
    (unless (or doc-id outline)
      (error "Docbook stub missing doc id and outline"))
    (let ((pairs (list (cons "book_id" book)
                       (cons "doc_id" doc-id)
                       (cons "entry_id" entry-id)
                       (cons "version" version)
                       (cons "timestamp" (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))
                       (cons "agent_summary" (or context ""))
                       (cons "outline_path" outline)
                       (cons "path_string" path-string)
                       (cons "context" context)
                       (cons "delta" delta)
                       (cons "verification" verification)
                       (cons "replaces" (and replaces (not (string-empty-p replaces)) replaces))))
          (payload nil))
      (dolist (pair pairs)
        (when (cdr pair)
          (push pair payload)))
      (nreverse payload))))

(defun arxana-docbook--sync-stub-buffer ()
  (unless (arxana-store-sync-enabled-p)
    (setq-local arxana-docbook--stub-last-sync-error "Sync disabled")
    (message "XTDB sync skipped (sync disabled).")
    (arxana-docbook--update-stub-header)
    (cl-return-from arxana-docbook--sync-stub-buffer nil))
  (let* ((payload (arxana-docbook--stub-payload-from-buffer))
         (book (cdr (assoc "book_id" payload)))
         (path (format "/docs/%s/entry" book))
         (resp (arxana-store--request "POST" path payload)))
    (if resp
        (progn
          (setq-local arxana-docbook--stub-last-sync-error nil)
          (arxana-docbook--stub-record-sync)
          (message "Docbook sync ok.")
          t)
      (setq-local arxana-docbook--stub-last-sync-error
                  (or (and (boundp 'arxana-store-last-error)
                           (plist-get arxana-store-last-error :detail))
                      "XTDB sync failed"))
      (message "Docbook sync failed: %s" arxana-docbook--stub-last-sync-error)
      nil))
  (arxana-docbook--update-stub-header))

(defun arxana-docbook-stub-save-sync ()
  "Save the current docbook stub and sync it to XTDB."
  (interactive)
  (save-buffer)
  (arxana-docbook--sync-stub-buffer))

(defun arxana-docbook--book-dir (book)
  (let ((root (arxana-docbook--locate-books-root)))
    (and root book (expand-file-name book root))))

(defun arxana-docbook--ensure-book-dirs (book)
  (let* ((book-dir (arxana-docbook--book-dir book)))
    (unless book-dir
      (error "No docbook root for %s" book))
    (dolist (dir (list book-dir))
      (unless (file-directory-p dir)
        (make-directory dir t)))
    (list :book book-dir)))

(defun arxana-docbook--entry-paths (book entry-id)
  (let* ((dirs (arxana-docbook--ensure-book-dirs book))
         (stub (expand-file-name (format "%s.org" entry-id) (plist-get dirs :book))))
    (list :stub stub)))

(defun arxana-docbook--checkout-file-mtime (path)
  "Return PATH's mtime without relying on core helpers."
  (when (and path (file-exists-p path))
    (file-attribute-modification-time (file-attributes path))))

(defun arxana-docbook--checkout-normalize-timestamp (value)
  "Normalize VALUE into an Emacs time value."
  (cond
   ((null value) nil)
   ((consp value) value)
   ((numberp value)
    (if (> value 1000000000000)
        (seconds-to-time (/ value 1000.0))
      (seconds-to-time value)))
   ((stringp value)
    (ignore-errors (date-to-time value)))
   (t nil)))

(defun arxana-docbook--entry-local-mtime (paths)
  (arxana-docbook--checkout-file-mtime (plist-get paths :stub)))

(defun arxana-docbook--entry-local-newer-p (paths entry)
  (let ((local (arxana-docbook--entry-local-mtime paths))
        (remote (arxana-docbook--checkout-normalize-timestamp (plist-get entry :timestamp))))
    (and local
         (or (not remote)
             (time-less-p remote local)))))

(defun arxana-docbook--normalize-latest-entry (value)
  (cond
   ((and (listp value) (plist-get value :entry-id)) value)
   (t (arxana-docbook--normalize-remote-entry value))))

(defun arxana-docbook--latest-entry-for-heading (book heading)
  (let* ((latest (plist-get heading :latest))
         (entry (and latest (arxana-docbook--normalize-latest-entry latest))))
    (or entry
        (when-let* ((doc-id (arxana-docbook--toc-doc-id heading))
                    (entries (arxana-docbook--remote-heading book doc-id)))
          (or (arxana-docbook--latest-non-lab entries)
              (car entries))))))

(defun arxana-docbook--entry-org-block (label text)
  (when (and text (not (string-empty-p (string-trim text))))
    (format "* %s\n%s\n\n" label (string-trim text))))

(defun arxana-docbook--entry-org-body (entry)
  (let* ((raw (plist-get entry :entry))
         (context (or (plist-get raw :doc/context)
                      (plist-get raw :doc/summary)
                      (plist-get raw :doc/body)
                      (plist-get raw :doc/context_text)))
         (delta (or (plist-get raw :doc/delta)
                    (plist-get raw :doc/changes)))
         (verification (or (plist-get raw :doc/verification)
                           (plist-get raw :doc/tests)))
         (summary (or context
                      (plist-get entry :summary-raw)
                      (plist-get entry :summary)))
         (parts (delq nil (list (arxana-docbook--entry-org-block "Context" summary)
                                (arxana-docbook--entry-org-block "Delta" delta)
                                (arxana-docbook--entry-org-block "Verification" verification)))))
    (if parts
        (string-join parts "")
      "* Context\n- (no summary yet)\n\n")))

(defun arxana-docbook--entry-payload (book heading entry)
  (let* ((raw (plist-get entry :entry))
         (doc-id (or (plist-get entry :doc-id)
                     (and raw (or (plist-get raw :doc/id) (plist-get raw :doc-id)))
                     (arxana-docbook--toc-doc-id heading)))
         (run-id (or (plist-get entry :entry-id)
                     (and raw (plist-get raw :doc/entry-id))))
         (outline (or (arxana-docbook--heading-outline heading)
                      (and raw (plist-get raw :doc/outline_path))))
         (summary (or (plist-get entry :summary)
                      (plist-get entry :summary-raw)
                      (and raw (plist-get raw :doc/summary))))
         (files (or (plist-get entry :files)
                    (and raw (plist-get raw :doc/files))))
         (links (and raw (or (plist-get raw :doc/links-to)
                             (plist-get raw :doc/links_to))))
         (replaces (and raw (or (plist-get raw :doc/replaces)
                                (plist-get raw :doc/replaces_entry)))))
    (unless (and doc-id run-id)
      (error "Remote entry missing doc-id or entry-id"))
    (let ((pairs (list (cons "doc_id" doc-id)
                       (cons "run_id" run-id)
                       (cons "book_id" (or (plist-get entry :book) book))
                       (cons "version" (plist-get entry :version))
                       (cons "timestamp" (plist-get entry :timestamp))
                       (cons "agent_summary" summary)
                       (cons "outline_path" outline)
                       (cons "files_touched" files)
                       (cons "links_to" links)
                       (cons "replaces" replaces)))
          (payload nil))
      (dolist (pair pairs)
        (when (cdr pair)
          (push pair payload)))
      (nreverse payload))))

(defun arxana-docbook--entry-stub-text (heading entry)
  (let* ((entry-id (plist-get entry :entry-id))
         (title (or (arxana-docbook--heading-title heading)
                    (plist-get entry :doc-id)
                    entry-id))
         (doc-id (or (plist-get entry :doc-id) "unknown"))
         (version (or (plist-get entry :version) "unversioned"))
         (outline (or (arxana-docbook--heading-outline heading)
                      (plist-get entry :outline)))
         (path-string (or (arxana-docbook--heading-path-string heading)
                          (and outline (string-join outline " / ")))))
    (concat (format "#+TITLE: %s\n" title)
            ":PROPERTIES:\n"
            (format ":DOC_ID: %s\n" doc-id)
            (format ":ENTRY_ID: %s\n" (or entry-id ""))
            (format ":VERSION: %s\n" version)
            (format ":OUTLINE_PATH: %s\n" (or path-string ""))
            (format ":PATH_STRING: %s\n" (or path-string ""))
            (format ":REPLACES: %s\n"
                    (or (plist-get (plist-get entry :entry) :doc/replaces) ""))
            ":END:\n\n"
            (arxana-docbook--entry-org-body entry))))

(defun arxana-docbook--entry-file-id (entry)
  "Return the filename stem to use for ENTRY."
  (or (plist-get entry :doc-id)
      (plist-get entry :entry-id)))

(defun arxana-docbook--read-file-text (path)
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents path))
      (buffer-string))))

(defun arxana-docbook--entry-files-differ-p (paths stub-text)
  (let* ((stub-path (plist-get paths :stub))
         (stub-local (arxana-docbook--read-file-text stub-path))
         (stub-diff (and stub-path (not (string= (or stub-local "") (or stub-text ""))))))
    (list :stub stub-diff)))

(defun arxana-docbook--diff-path (path text &optional label)
  (when (and path text (file-readable-p path))
    (let ((tmp (make-temp-file "arxana-docbook-review-" nil ".org")))
      (unwind-protect
          (progn
            (with-temp-file tmp
              (insert text))
            (let ((buf (diff-no-select path tmp "-u" t)))
              (with-current-buffer buf
                (setq-local buffer-read-only t)
                (when label
                  (setq-local buffer-display-time (format "Docbook review: %s" label))))
              (pop-to-buffer buf)))
        (when (file-exists-p tmp)
          (delete-file tmp))))))

(defun arxana-docbook--review-entry-overwrite (paths stub-text local-newer)
  (let* ((stub-path (plist-get paths :stub))
         (prompt (format "Remote docbook entry differs%s. Overwrite local? "
                         (if local-newer " (local newer)" "")))
         (choice nil))
    (while (not (memq choice '(?y ?n)))
      (setq choice (read-char-choice
                    (concat prompt "[y]es [n]o [d]iff")
                    '(?y ?n ?d)))
      (when (eq choice ?d)
        (when (plist-get (arxana-docbook--entry-files-differ-p paths stub-text) :stub)
          (arxana-docbook--diff-path stub-path stub-text "stub")))
      (unless (memq choice '(?y ?n))
        (setq choice nil)))
    (eq choice ?y)))

(defun arxana-docbook--write-entry-files (book heading entry &optional force review)
  (let* ((entry-id (arxana-docbook--entry-file-id entry))
         (paths (and entry-id (arxana-docbook--entry-paths book entry-id))))
    (unless entry-id
      (error "Entry missing entry-id"))
    (let* ((local-newer (arxana-docbook--entry-local-newer-p paths entry))
           (stub-text (arxana-docbook--entry-stub-text heading entry))
           (diffs (arxana-docbook--entry-files-differ-p paths stub-text))
           (changed (plist-get diffs :stub))
           (should-write (cond
                          (force t)
                          (review (or (not changed)
                                      (arxana-docbook--review-entry-overwrite
                                       paths stub-text local-newer)))
                          ((and (not review) local-newer) nil)
                          (t t))))
      (if (not should-write)
          :skipped
        (with-temp-file (plist-get paths :stub)
          (insert stub-text))
        :written))))

;;;###autoload
(defun arxana-docbook-checkout-book (&optional book force review)
  "Fetch remote docbook entries for BOOK and write them to the filesystem.
When FORCE is non-nil, overwrite local files even if they appear newer.
When REVIEW is non-nil, prompt before overwriting differing local files."
  (interactive
   (list (let* ((default (or (and (boundp 'arxana-docbook--book)
                                  arxana-docbook--book)
                             (car (arxana-docbook--available-books))
                             "futon4")))
           (read-string "Doc book: " default))
         current-prefix-arg
         nil))
  (unless (arxana-docbook--remote-available-p book)
    (user-error "Docbook remote is unavailable; enable sync first"))
  (let* ((headings (or (arxana-docbook--remote-contents book) '()))
         (written 0)
         (skipped 0)
         (missing 0))
    (unless headings
      (user-error "No remote docbook headings found for %s" book))
    (arxana-docbook--toc-write-headings book headings)
    (dolist (heading headings)
        (let* ((entry (arxana-docbook--latest-entry-for-heading book heading))
               (result (and entry
                            (arxana-docbook--write-entry-files book heading entry force review))))
          (pcase result
            (:written (cl-incf written))
            (:skipped (cl-incf skipped))
            (_ (cl-incf missing)))))
    (message "Docbook checkout: %d written, %d skipped, %d missing" written skipped missing)
    (list :written written :skipped skipped :missing missing)))

;;;###autoload
(defun arxana-docbook-checkout-book-review (&optional book)
  "Fetch remote docbook entries for BOOK and review differences before overwrite."
  (interactive)
  (arxana-docbook-checkout-book book nil t))

;;;###autoload
(defun arxana-docbook-open-stub (&optional entry)
  "Open the stub file for ENTRY, enabling `arxana-docbook-stub-mode`."
  (interactive)
  (let* ((entry (or entry
                    (when (and (boundp 'arxana-docbook--entry-book)
                               arxana-docbook--entry-book
                               (boundp 'arxana-docbook--entry-doc-id)
                               arxana-docbook--entry-doc-id)
                      (let* ((book arxana-docbook--entry-book)
                             (doc-id arxana-docbook--entry-doc-id)
                             (entry-id arxana-docbook--entry-entry-id)
                             (entries (arxana-docbook--entries-for-doc book doc-id)))
                        (or (and entry-id
                                 (cl-find-if (lambda (item)
                                               (equal entry-id (plist-get item :entry-id)))
                                             entries))
                            (arxana-docbook--latest-non-lab entries))))))
         (stub (and entry (plist-get entry :stub-path))))
    (unless stub
      (user-error "No local stub found (run arxana-docbook-checkout-book first)"))
    (let ((buffer (find-file stub)))
      (with-current-buffer buffer
        (setq-local arxana-docbook--stub-doc-id (plist-get entry :doc-id))
        (setq-local arxana-docbook--stub-entry-id (plist-get entry :entry-id))
        (setq-local arxana-docbook--stub-book
                    (or (plist-get entry :book)
                        (arxana-docbook--stub-book-from-path stub)))
        (setq-local arxana-docbook--stub-last-sync-error nil)
        (arxana-docbook-stub-mode 1))
      (pop-to-buffer buffer))))

(provide 'arxana-docbook-checkout)
;;; arxana-docbook-checkout.el ends here
