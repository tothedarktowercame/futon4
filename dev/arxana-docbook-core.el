;;; arxana-docbook-core.el --- Docbook core helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Core parsing and filesystem helpers for docbook entries.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)

(declare-function arxana-docbook--remote-available-p "arxana-docbook-remote" (&optional book))
(declare-function arxana-docbook--remote-heading "arxana-docbook-remote" (book doc-id))

(defgroup arxana-docbook nil
  "Browse doc book entries for Futon systems."
  :group 'arxana)
(defcustom arxana-docbook-books-root nil
  "Root directory containing doc book folders (e.g., docs/docbook)."
  :type '(choice (const :tag "Auto-detect" nil)
                 directory)
  :group 'arxana-docbook)
(defun arxana-docbook--locate-books-root ()
  (or arxana-docbook-books-root
      (let* ((base (or load-file-name buffer-file-name default-directory))
             (repo-root (and base (locate-dominating-file base "dev")))
             (working-root (and repo-root
                                (expand-file-name "docs/docbook" repo-root))))
        (cond
         ((and working-root (file-directory-p working-root)) working-root)
         (repo-root (expand-file-name "docs/docbook" repo-root))))))
(defun arxana-docbook--repo-root ()
  (let* ((base (or load-file-name buffer-file-name))
         (lib (and (not base) (locate-library "arxana-docbook-core")))
         (base (or base lib))
         (root (and base (locate-dominating-file base "dev"))))
    (or root
        (when-let* ((books (arxana-docbook--locate-books-root)))
          (expand-file-name "../../.." books))
        (locate-dominating-file default-directory "dev")
        default-directory)))
(defun arxana-docbook--filesystem-available-p (&optional book)
  (let* ((root (arxana-docbook--locate-books-root))
         (path (cond
                ((and root book) (expand-file-name book root))
                (root root))))
    (and path (file-directory-p path))))
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

(defun arxana-docbook--normalize-timestamp (value)
  "Coerce VALUE into a time value when possible."
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

(defun arxana-docbook--file-mtime (path)
  (when (and path (file-exists-p path))
    (file-attribute-modification-time (file-attributes path))))
(defun arxana-docbook--entry-mtime (entry)
  "Return the mtime for ENTRY's stub file when available."
  (arxana-docbook--file-mtime (plist-get entry :stub-path)))
(defun arxana-docbook--entry-from-json (book path)
  (let* ((payload (arxana-docbook--read-json path))
         (run-id (or (plist-get payload :run_id)
                     (file-name-base path)))
         (stub (expand-file-name (format "../%s.org" run-id)
                                 (file-name-directory (directory-file-name path))))
         (files (plist-get payload :files_touched))
         (outline (plist-get payload :outline_path)))
    (when payload
      (list :book book
            :doc-id (plist-get payload :doc_id)
            :run-id run-id
            :entry-id run-id
            :version (plist-get payload :version)
            :timestamp (plist-get payload :timestamp)
            :summary (plist-get payload :agent_summary)
            :outline outline
            :files (and (listp files) files)
            :raw-path path
            :stub-path (and (file-readable-p stub) stub)))))
(defun arxana-docbook--entry-from-stub (book path)
  (let* ((text (when (file-readable-p path)
                 (with-temp-buffer
                   (let ((coding-system-for-read 'utf-8-unix))
                     (insert-file-contents path))
                   (buffer-string))))
         (lines (and text (split-string text "\n")))
         (title nil)
         (props (make-hash-table :test 'equal))
         (in-props nil)
         (mtime (arxana-docbook--file-mtime path))
         (timestamp (and mtime (format-time-string "%Y-%m-%dT%H:%M:%SZ" mtime t))))
    (when text
      (dolist (line lines)
        (cond
         ((string-prefix-p "#+TITLE:" line)
          (setq title (string-trim (substring line (length "#+TITLE:")))))
         ((string= ":PROPERTIES:" line)
          (setq in-props t))
         ((string= ":END:" line)
          (setq in-props nil))
         (in-props
          (when (string-match "^:\\([A-Z0-9_]+\\):[ \t]*\\(.*\\)$" line)
            (puthash (match-string 1 line) (string-trim (match-string 2 line)) props))))))
    (when text
      (let* ((entry-id (file-name-base path))
             (entry-id (or (gethash "ENTRY_ID" props) entry-id))
             (doc-id entry-id)
             (declared (gethash "DOC_ID" props))
             (version (gethash "VERSION" props))
             (outline (gethash "OUTLINE_PATH" props))
             (path-string (gethash "PATH_STRING" props)))
        (when (and declared (not (string= declared entry-id)))
          (message "[arxana-docbook] Ignoring DOC_ID %s (expected %s) in %s"
                   declared entry-id (file-name-nondirectory path)))
        (list :book book
              :doc-id doc-id
              :entry-id entry-id
              :run-id entry-id
              :version version
              :timestamp timestamp
              :outline (and outline (split-string outline " / " t))
              :path-string path-string
              :title title
              :stub-path path)))))

(defun arxana-docbook--stub-path-for-doc (book doc-id)
  "Return the stub path for DOC-ID in BOOK when it exists."
  (let* ((root (arxana-docbook--locate-books-root))
         (book-dir (and root book (expand-file-name book root)))
         (path (and book-dir doc-id
                    (expand-file-name (format "%s.org" doc-id) book-dir))))
    (and path (file-readable-p path) path)))
(defun arxana-docbook--entry-version (entry)
  (or (plist-get entry :version)
      (plist-get entry :doc/version)))
(defun arxana-docbook--lab-draft-p (entry)
  (string= (arxana-docbook--entry-version entry) "lab-draft"))
(defun arxana-docbook--entries-for-doc (book doc-id)
  (let* ((remote (when (arxana-docbook--remote-available-p book)
                   (ignore-errors (arxana-docbook--remote-heading book doc-id))))
         (local (ignore-errors (arxana-docbook-entries book)))
         (local-index (make-hash-table :test 'equal))
         (entries nil))
    (dolist (entry local)
      (when-let* ((entry-id (plist-get entry :entry-id))
                  (mtime (arxana-docbook--entry-mtime entry)))
        (puthash entry-id mtime local-index)))
    (dolist (entry remote)
      (let* ((entry-id (plist-get entry :entry-id))
             (local-mtime (and entry-id (gethash entry-id local-index)))
             (remote-time (arxana-docbook--normalize-timestamp (plist-get entry :timestamp))))
        (when (or (not local-mtime)
                  (not remote-time)
                  (time-less-p local-mtime remote-time))
          (push entry entries))))
    (setq entries (append (nreverse entries) local))
    (let* ((filtered (seq-filter (lambda (entry)
                                   (equal doc-id (plist-get entry :doc-id)))
                                 entries)))
      (mapcar (lambda (entry)
                (if (plist-get entry :stub-path)
                    entry
                  (let ((stub (arxana-docbook--stub-path-for-doc book doc-id)))
                    (if stub
                        (plist-put (copy-sequence entry) :stub-path stub)
                      entry))))
              filtered))))
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
(defun arxana-docbook--maybe-fix-mojibake (text)
  "Best-effort repair for UTF-8 text mis-decoded as Latin-1/Windows-1252."
  (if (stringp text)
      (let* ((raw (if (multibyte-string-p text)
                      text
                    (decode-coding-string text 'latin-1)))
             (count (lambda (s)
                      (let ((idx 0)
                            (hits 0)
                            (len (length s)))
                        (while (< idx len)
                          (let ((ch (aref s idx)))
                            (when (or (= ch #x00e2)
                                      (= ch #x00c2)
                                      (= ch #x00c3))
                              (setq hits (1+ hits))))
                          (setq idx (1+ idx)))
                        hits)))
             (before (funcall count raw))
             (converted (condition-case nil
                            (decode-coding-string (encode-coding-string raw 'latin-1) 'utf-8)
                          (error raw)))
             (after (funcall count converted)))
        (if (and (> before 0) (< after before))
            converted
          text))
    text))
(defun arxana-docbook--entry-content (entry)
  (let* ((stub (arxana-docbook--read-stub entry))
         (keep-src (and stub
                        (or (string-match-p "^:KEEP_SRC:" stub)
                            (string-match-p "^#\\+PROPERTY:.*KEEP_SRC" stub))))
         (stub-placeholder (and stub
                                (string-match-p "\\(no summary yet\\)" stub))))
    (cond
     ((and stub (not stub-placeholder))
      (arxana-docbook--maybe-fix-mojibake
       (arxana-docbook--strip-org-metadata
        (if keep-src
            (arxana-docbook--strip-stub-header stub)
          (arxana-docbook--strip-org-code-blocks
           (arxana-docbook--strip-stub-header stub))))))
     ((plist-get entry :summary-raw)
      (arxana-docbook--maybe-fix-mojibake
       (arxana-docbook--strip-org-metadata
       (if keep-src
           (plist-get entry :summary-raw)
         (arxana-docbook--strip-org-code-blocks (plist-get entry :summary-raw))))))
     ((plist-get entry :summary)
      (arxana-docbook--maybe-fix-mojibake
       (arxana-docbook--strip-org-metadata
       (if keep-src
           (plist-get entry :summary)
         (arxana-docbook--strip-org-code-blocks (plist-get entry :summary))))))
     (t ""))))
(defun arxana-docbook--entry-raw-text (entry)
  (arxana-docbook--maybe-fix-mojibake
   (or (arxana-docbook--read-stub entry)
       (plist-get entry :summary-raw)
       (plist-get entry :summary)
       "")))
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
(defun arxana-docbook--source-link-line (entry)
  (when-let* ((path (arxana-docbook--entry-source-path entry)))
    (format "- Source: [[file:%s][%s]]\n" path path)))
(defun arxana-docbook--entries-by-doc-id (entries)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (when-let* ((doc-id (plist-get entry :doc-id)))
        (puthash doc-id (cons entry (gethash doc-id table)) table)))
    table))
(defun arxana-docbook--entries-for (book)
  (let* ((root (arxana-docbook--locate-books-root))
         (book-dir (and root (expand-file-name book root)))
         (stub-dir book-dir)
         (raw-dir (and book-dir (expand-file-name "raw" book-dir)))
         (entries '()))
    (when (and stub-dir (file-directory-p stub-dir))
      (setq entries
            (append entries
                    (delq nil
                          (mapcar (lambda (path)
                                    (when (string-match-p "\\.org\\'" path)
                                      (arxana-docbook--entry-from-stub book path)))
                                  (directory-files stub-dir t nil t))))))
    (when (and raw-dir (file-directory-p raw-dir))
      (setq entries
            (append entries
                    (delq nil
                          (mapcar (lambda (path)
                                    (when (string-match-p "\\.json\\'" path)
                                      (arxana-docbook--entry-from-json book path)))
                                  (directory-files raw-dir t nil t))))))
    (when entries
      (seq-sort
       (lambda (a b)
         (string> (or (plist-get a :timestamp) "")
                  (or (plist-get b :timestamp) "")))
       entries))))
(defun arxana-docbook-entries (book)
  "Return a list of doc book entries for BOOK."
  (arxana-docbook--entries-for book))

(provide 'arxana-docbook-core)
;;; arxana-docbook-core.el ends here
