;;; arxana-lab.el --- Lab notebook browser -*- lexical-binding: t; -*-

;;; Commentary:
;; Browse lab notebook entries staged under lab/ (raw + stubs + traces).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'org)

(defgroup arxana-lab nil
  "Browse lab notebook entries."
  :group 'arxana)

(defface arxana-lab-assistant-face
  '((t :background "#1e242b"))
  "Face for assistant timeline blocks."
  :group 'arxana-lab)

(defface arxana-lab-psr-face
  '((t :background "#2b1e24" :foreground "#e0a0c0"))
  "Face for PSR (pattern selection) blocks."
  :group 'arxana-lab)

(defface arxana-lab-pur-face
  '((t :background "#1e2b24" :foreground "#a0e0c0"))
  "Face for PUR (pattern use) blocks."
  :group 'arxana-lab)

(defface arxana-lab-aif-face
  '((t :background "#24241e" :foreground "#e0e0a0"))
  "Face for AIF summary blocks."
  :group 'arxana-lab)

(defface arxana-lab-code-face
  '((t :background "#1e1e2b" :foreground "#a0c0e0"))
  "Face for code edit blocks."
  :group 'arxana-lab)

(defface arxana-lab-hyperlink-face
  '((t :foreground "dodger blue" :underline t))
  "Face for clickable hyperlinks."
  :group 'arxana-lab)

(defcustom arxana-lab-root nil
  "Root directory containing the lab notebook (e.g., <repo>/lab)."
  :type '(choice (const :tag "Auto-detect" nil)
                 directory)
  :group 'arxana-lab)

(defun arxana-lab--locate-root ()
  (or arxana-lab-root
      (let* ((base (or load-file-name buffer-file-name default-directory))
             (root (and base (locate-dominating-file base "lab"))))
        (when root
          (expand-file-name "lab" root)))))

(defun arxana-lab--read-json (path)
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-key-type 'keyword))
    (condition-case err
        (json-read-file path)
      (error
       (message "[arxana-lab] Failed to read %s: %s" path (error-message-string err))
       nil))))

(defun arxana-lab--raw-files (root)
  (let ((dir (and root (expand-file-name "raw" root))))
    (when (and dir (file-directory-p dir))
      (seq-filter (lambda (path)
                    (string-match-p "\\.json\\'" path))
                  (directory-files dir t nil t)))))

(defun arxana-lab--file-items (root subdir kind)
  (let ((dir (and root (expand-file-name subdir root))))
    (when (and dir (file-directory-p dir))
      (mapcar (lambda (path)
                (let* ((attrs (file-attributes path))
                       (mtime (file-attribute-modification-time attrs)))
                  (list :type 'lab-file
                        :kind kind
                        :label (file-name-nondirectory path)
                        :path path
                        :modified (when mtime
                                    (format-time-string "%Y-%m-%d %H:%M" mtime)))))
              (seq-filter #'file-regular-p
                          (directory-files dir t "^[^.]" t))))))

(defun arxana-lab--entry-from-raw (root path)
  (let* ((payload (arxana-lab--read-json path))
         (session-id (or (plist-get payload :lab/session-id)
                         (file-name-base path)))
         (ts-start (plist-get payload :lab/timestamp-start))
         (ts-end (plist-get payload :lab/timestamp-end))
         (files (plist-get payload :lab/files-touched))
         (trace-rel (or (plist-get payload :lab/trace-path)
                        (format "lab/trace/%s.org" session-id)))
         (draft-rel (or (plist-get payload :lab/doc-draft-path)
                        (format "lab/doc-drafts/%s.json" session-id)))
         (stub (and root (expand-file-name "stubs" root)
                    (expand-file-name (format "%s.org" session-id)
                                      (expand-file-name "stubs" root))))
         (trace (and root trace-rel
                     (expand-file-name trace-rel (file-name-directory root))))
         (draft (and root draft-rel
                     (expand-file-name draft-rel (file-name-directory root)))))
    (when payload
      (list :type 'lab-entry
            :session-id session-id
            :timestamp-start ts-start
            :timestamp-end ts-end
            :files (and (listp files) files)
            :raw-path path
            :stub-path (and stub (file-readable-p stub) stub)
            :trace-path (and trace (file-readable-p trace) trace)
            :draft-path (and draft (file-readable-p draft) draft)))))

(defun arxana-lab-entries ()
  "Return lab notebook entries from lab/raw."
  (let* ((root (arxana-lab--locate-root))
         (paths (or (arxana-lab--raw-files root) '()))
         (entries (delq nil (mapcar (lambda (p) (arxana-lab--entry-from-raw root p)) paths))))
    (seq-sort
     (lambda (a b)
       (string> (or (plist-get a :timestamp-start) "")
                (or (plist-get b :timestamp-start) "")))
     entries)))

(defun arxana-lab-file-items (kind)
  "Return lab file entries for KIND (raw, stubs, drafts)."
  (let ((root (arxana-lab--locate-root)))
    (pcase kind
      ('raw (arxana-lab--file-items root "raw" "raw"))
      ('stubs (arxana-lab--file-items root "stubs" "stubs"))
      ('drafts (arxana-lab--file-items root "doc-drafts" "drafts"))
      (_ nil))))

(defun arxana-lab--open-file (path title mode)
  (unless (and path (file-readable-p path))
    (user-error "No readable %s file" title))
  (let ((buf (get-buffer-create (format "*Lab:%s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-file-contents path)
        (goto-char (point-min))
        (funcall mode))
      (view-mode 1))
    (pop-to-buffer buf)))

(defun arxana-lab--extract-org (text)
  (if (string-match "```org\\s-*\\([\\s\\S]*?\\)```" text)
      (string-trim (match-string 1 text))
    (string-trim text)))

(defun arxana-lab--strip-org-fences (text)
  (let* ((lines (split-string (string-trim text) "\n"))
         (head (car lines))
         (tail (car (last lines)))
         (body lines))
    (when (and head (string-match-p "\\`\\s-*```org\\s-*\\'" head))
      (setq body (cdr body)))
    (when (and tail (string-match-p "\\`\\s-*```\\s-*\\'" tail))
      (setq body (butlast body)))
    (string-join body "\n")))

(defun arxana-lab--extract-org-block (text)
  (when (string-match "```org\\s-*\\([\\s\\S]*?\\)```" text)
    (string-trim (match-string 1 text))))

(defun arxana-lab--summary-from-json (text)
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'string))
    (condition-case _err
        (let* ((payload (or (ignore-errors
                              (json-parse-string text :object-type 'alist :array-type 'list))
                            (json-read-from-string text)))
               (choices (or (alist-get "choices" payload) '()))
               (choice (car choices))
               (message (and choice (alist-get "message" choice)))
               (content (and message (alist-get "content" message))))
          (when (stringp content)
            (arxana-lab--extract-org content)))
      (error nil))))

(defun arxana-lab--header-lines (text)
  (let ((lines (split-string text "\n"))
        (out '()))
    (while (and lines (string-prefix-p "#+" (car lines)))
      (push (car lines) out)
      (setq lines (cdr lines)))
    (string-join (nreverse out) "\n")))

(defun arxana-lab--json-substring (text)
  (let ((start (string-match "{\"id\"\\|{\"object\"\\|{\"choices\"" text)))
    (when start
      (substring text start))))

(defun arxana-lab--decode-json-escapes (text)
  (let ((out text))
    (setq out (replace-regexp-in-string "\\\\\\\\" "\\\\" out))
    (setq out (replace-regexp-in-string "\\\\\"" "\"" out))
    (setq out (replace-regexp-in-string "\\\\/" "/" out))
    (setq out (replace-regexp-in-string "\\\\n" "\n" out))
    (setq out (replace-regexp-in-string "\\\\r" "\r" out))
    (setq out (replace-regexp-in-string "\\\\t" "\t" out))
    out))

(defun arxana-lab--extract-org-fallback (text)
  (when (string-match "```org\\s-*\\([\\s\\S]*?\\)```" text)
    (let ((body (match-string 1 text)))
      (string-trim (arxana-lab--decode-json-escapes body)))))

(defun arxana-lab--extract-org-from-decoded (text)
  (let ((decoded (arxana-lab--decode-json-escapes text)))
    (arxana-lab--extract-org-block decoded)))

(defun arxana-lab--extract-json-string-value (text key)
  (let* ((pattern (format "\"%s\"\\s-*:\\s-*\"" (regexp-quote key)))
         (start (string-match pattern text)))
    (when start
      (let ((i (+ start (length (match-string 0 text))))
            (len (length text))
            (out (get-buffer-create " *lab-json-string*"))
            (escape nil)
            (done nil))
        (with-current-buffer out
          (erase-buffer))
        (while (and (< i len) (not done))
          (let ((ch (aref text i)))
            (cond
             (escape
              (with-current-buffer out (insert ch))
              (setq escape nil))
             ((= ch ?\\)
              (with-current-buffer out (insert ch))
              (setq escape t))
             ((= ch ?\")
              (setq done t))
             (t
              (with-current-buffer out (insert ch)))))
          (setq i (1+ i)))
        (with-current-buffer out
          (let ((raw (buffer-string)))
            (kill-buffer out)
            (arxana-lab--decode-json-escapes raw)))))))

(defun arxana-lab-open-stub-viewer (path)
  "Open a readable lab stub viewer for PATH."
  (let* ((raw (with-temp-buffer
                (insert-file-contents path)
                (buffer-string)))
          (org-block (arxana-lab--extract-org-block raw))
          (header (arxana-lab--header-lines raw))
          (json-part (or (arxana-lab--json-substring raw) raw))
          (parsed (arxana-lab--summary-from-json json-part))
          (fallback (arxana-lab--extract-org-fallback json-part))
          (decoded-block (arxana-lab--extract-org-from-decoded json-part))
          (content (arxana-lab--extract-json-string-value json-part "content"))
         (summary-info
          (cond
           (parsed (list :source :parsed :text parsed))
           (fallback (list :source :fallback :text fallback))
           (decoded-block (list :source :decoded :text decoded-block))
           (content (list :source :content :text (or (arxana-lab--extract-org-block content)
                                                     (arxana-lab--extract-org content))))
           (org-block (list :source :org-block :text org-block))
           (t (list :source :raw :text raw))))
         (summary (plist-get summary-info :text))
         (buf (get-buffer-create "*Lab:stub*")))
    (when (eq (plist-get summary-info :source) :raw)
      (message "[lab-stub] Unable to extract org content; showing raw JSON. parsed=%s fallback=%s decoded=%s org-block=%s json-start=%s"
               (if parsed "yes" "no")
               (if fallback "yes" "no")
               (if decoded-block "yes" "no")
               (if org-block "yes" "no")
               (if json-part "yes" "no")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (when (and header (not (string-empty-p header)))
          (insert header "\n\n"))
        (insert (arxana-lab--strip-org-fences (arxana-lab--extract-org summary)))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buf)))

(defun arxana-lab--insert-kv (label value)
  (insert (format "- %s: %s\n" label (or value ""))))

(defun arxana-lab--normalize-message (msg role)
  (list :id (or (plist-get msg :id) "")
        :timestamp (or (plist-get msg :timestamp) "")
        :text (or (plist-get msg :text) "")
        :role role))

(defun arxana-lab--insert-message-list (title messages)
  (when (and messages (listp messages))
    (insert (format "* %s (%d)\n" title (length messages)))
    (dolist (msg messages)
      (let ((id (or (plist-get msg :id) ""))
            (ts (or (plist-get msg :timestamp) ""))
            (text (or (plist-get msg :text) "")))
        (insert (format "** msg:%s\n" id))
        (arxana-lab--insert-kv "Timestamp" ts)
        (insert "\n")
        (insert text)
        (unless (string-suffix-p "\n" text)
          (insert "\n"))
        (insert "\n")))))

;; =============================================================================
;; Hyperlink support
;; =============================================================================

(defvar arxana-lab-pattern-browser-function nil
  "Function to browse a pattern by ID. Called with pattern-id string.")

(defvar arxana-lab-code-browser-function #'find-file-other-window
  "Function to browse code. Called with file path string.")

(defun arxana-lab--make-pattern-link (pattern-id)
  "Create a clickable link for PATTERN-ID."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]
      (lambda () (interactive)
        (arxana-lab--browse-pattern pattern-id)))
    (define-key map (kbd "RET")
      (lambda () (interactive)
        (arxana-lab--browse-pattern pattern-id)))
    (propertize (format "[[pattern:%s]]" pattern-id)
                'face 'arxana-lab-hyperlink-face
                'mouse-face 'highlight
                'keymap map
                'help-echo (format "Browse pattern: %s" pattern-id)
                'arxana-pattern-id pattern-id)))

(defun arxana-lab--make-code-link (file-path &optional function-name)
  "Create a clickable link for FILE-PATH, optionally to FUNCTION-NAME."
  (let ((map (make-sparse-keymap))
        (display (if function-name
                     (format "[[code:%s::%s]]" file-path function-name)
                   (format "[[code:%s]]" file-path))))
    (define-key map [mouse-1]
      (lambda () (interactive)
        (arxana-lab--browse-code file-path function-name)))
    (define-key map (kbd "RET")
      (lambda () (interactive)
        (arxana-lab--browse-code file-path function-name)))
    (propertize display
                'face 'arxana-lab-hyperlink-face
                'mouse-face 'highlight
                'keymap map
                'help-echo (format "Open: %s%s" file-path
                                   (if function-name (format " at %s" function-name) ""))
                'arxana-file-path file-path
                'arxana-function function-name)))

(defun arxana-lab--browse-pattern (pattern-id)
  "Browse PATTERN-ID using configured browser function."
  (if arxana-lab-pattern-browser-function
      (funcall arxana-lab-pattern-browser-function pattern-id)
    (message "Pattern: %s (set arxana-lab-pattern-browser-function to enable navigation)"
             pattern-id)))

(defun arxana-lab--browse-code (file-path &optional function-name)
  "Browse FILE-PATH, optionally jumping to FUNCTION-NAME."
  (let ((full-path (if (file-name-absolute-p file-path)
                       file-path
                     ;; Try to find relative to common roots
                     (or (locate-file file-path (list default-directory
                                                       (expand-file-name "~/code/futon3/")
                                                       (expand-file-name "~/code/")))
                         file-path))))
    (when (and full-path (file-exists-p full-path))
      (funcall arxana-lab-code-browser-function full-path)
      (when function-name
        (goto-char (point-min))
        (search-forward function-name nil t)))))

;; =============================================================================
;; Enhanced timeline for MUSN events
;; =============================================================================

(defun arxana-lab--role-face (role)
  "Return face for event ROLE."
  (pcase role
    ("psr" 'arxana-lab-psr-face)
    ("pur" 'arxana-lab-pur-face)
    ("aif" 'arxana-lab-aif-face)
    ("system" 'arxana-lab-code-face)
    ("assistant" 'arxana-lab-assistant-face)
    (_ nil)))

(defun arxana-lab--insert-event-links (event)
  "Insert hyperlinks for pattern-id and file references in EVENT."
  (when-let ((pattern-id (plist-get event :pattern-id)))
    (insert "  Pattern: " (arxana-lab--make-pattern-link pattern-id) "\n"))
  (when-let ((file (plist-get event :file)))
    (let ((func (plist-get event :function)))
      (insert "  File: " (arxana-lab--make-code-link file func) "\n")))
  ;; Candidates (for PSR)
  (when-let ((candidates (plist-get event :candidates)))
    (insert "  Candidates: ")
    (insert (mapconcat #'arxana-lab--make-pattern-link candidates ", "))
    (insert "\n"))
  ;; Anchors (for PUR)
  (when-let ((anchors (plist-get event :anchors)))
    (dolist (anchor anchors)
      (when-let ((ref (plist-get anchor :anchor/ref)))
        (when-let ((file (plist-get ref :file)))
          (insert "  Anchor: " (arxana-lab--make-code-link file (plist-get ref :fn)) "\n"))))))

(defun arxana-lab--insert-musn-timeline (all-events)
  "Insert timeline view for MUSN ALL-EVENTS with rich formatting."
  (insert (format "* Event Timeline (%d events)\n\n" (length all-events)))
  (dolist (event all-events)
    (let* ((start (point))
           (id (or (plist-get event :id) "?"))
           (ts (or (plist-get event :timestamp) ""))
           (role (or (plist-get event :role) "system"))
           (event-type (or (plist-get event :event-type) "unknown"))
           (text (or (plist-get event :text) ""))
           (face (arxana-lab--role-face role)))
      ;; Header
      (insert (format "** [%s] %s\n" (upcase role) id))
      (insert (format "   :PROPERTIES:\n   :TIMESTAMP: %s\n   :EVENT-TYPE: %s\n   :END:\n\n"
                      ts event-type))
      ;; Hyperlinks
      (arxana-lab--insert-event-links event)
      ;; Text content
      (insert "\n")
      (insert text)
      (unless (string-suffix-p "\n" text)
        (insert "\n"))
      (insert "\n")
      ;; Apply face overlay
      (when face
        (let ((ov (make-overlay start (point))))
          (overlay-put ov 'face face)
          (overlay-put ov 'priority 1)
          (overlay-put ov 'evaporate t))))))

(defun arxana-lab--insert-message-timeline (user-messages assistant-messages)
  (let* ((users (mapcar (lambda (msg) (arxana-lab--normalize-message msg "user"))
                        (or user-messages '())))
         (assistants (mapcar (lambda (msg) (arxana-lab--normalize-message msg "assistant"))
                             (or assistant-messages '())))
         (all (append users assistants))
         (sorted (seq-sort (lambda (a b)
                             (string< (plist-get a :timestamp)
                                      (plist-get b :timestamp)))
                           all)))
    (insert (format "* Timeline (%d)\n" (length sorted)))
    (dolist (msg sorted)
      (let ((start (point))
            (id (plist-get msg :id))
            (ts (plist-get msg :timestamp))
            (role (plist-get msg :role))
            (text (plist-get msg :text)))
        (insert (format "** %s msg:%s\n" role id))
        (arxana-lab--insert-kv "Timestamp" ts)
        (insert "\n")
        (insert text)
        (unless (string-suffix-p "\n" text)
          (insert "\n"))
        (insert "\n")
        (when (string= role "assistant")
          (let ((ov (make-overlay start (point))))
            (overlay-put ov 'face 'arxana-lab-assistant-face)
            (overlay-put ov 'priority 1)
            (overlay-put ov 'evaporate t)))))))

(defun arxana-lab-open-raw-payload (payload)
  "Open a readable lab notebook view for PAYLOAD."
  (unless payload
    (user-error "Unable to read lab payload"))
  (let* ((session (or (plist-get payload :lab/session-id) ""))
         (agent (plist-get payload :lab/agent))
         (source (plist-get payload :lab/source))
         (ts-start (plist-get payload :lab/timestamp-start))
         (ts-end (plist-get payload :lab/timestamp-end))
         (files (or (plist-get payload :lab/files-touched) '()))
         (trace (plist-get payload :lab/trace-path))
         (users (plist-get payload :lab/user-messages))
         (assistants (plist-get payload :lab/assistant-messages))
         ;; MUSN-specific fields
         (all-events (plist-get payload :lab/all-events))
         (has-psr (plist-get payload :lab/has-psr))
         (has-pur (plist-get payload :lab/has-pur))
         (has-aif (plist-get payload :lab/has-aif))
         (is-musn (string= source "musn/futon3"))
         (buf (get-buffer-create (format "*Lab:%s*" session))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: Lab Session %s\n" session))
        (when agent
          (insert (format "#+SUBTITLE: Agent: %s\n" agent)))
        (insert "\n")
        ;; Summary
        (insert "* Summary\n")
        (arxana-lab--insert-kv "Session" session)
        (when agent (arxana-lab--insert-kv "Agent" agent))
        (when source (arxana-lab--insert-kv "Source" source))
        (arxana-lab--insert-kv "Start" ts-start)
        (arxana-lab--insert-kv "End" ts-end)
        (arxana-lab--insert-kv "Trace" trace)
        ;; MUSN indicators
        (when is-musn
          (insert "\n** Pattern Activity\n")
          (arxana-lab--insert-kv "Has PSR" (if has-psr "yes" "no"))
          (arxana-lab--insert-kv "Has PUR" (if has-pur "yes" "no"))
          (arxana-lab--insert-kv "Has AIF" (if has-aif "yes" "no")))
        ;; Files with hyperlinks
        (when files
          (insert "\n* Files Touched\n")
          (dolist (file files)
            (insert "- " (arxana-lab--make-code-link file) "\n")))
        (insert "\n")
        ;; Timeline - use MUSN timeline if available
        (if (and is-musn all-events)
            (arxana-lab--insert-musn-timeline all-events)
          (arxana-lab--insert-message-timeline users assistants))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buf)))

(defun arxana-lab-open-raw-viewer (path)
  "Open a readable lab notebook view for the raw JSON PATH."
  (let ((payload (arxana-lab--read-json path)))
    (arxana-lab-open-raw-payload payload)))

;;;###autoload
(defun arxana-lab-open-entry-object (entry)
  "Open lab stub for ENTRY."
  (interactive)
  (arxana-lab-open-stub-viewer (plist-get entry :stub-path)))

;;;###autoload
(defun arxana-lab-open-trace-object (entry)
  "Open lab trace for ENTRY."
  (interactive)
  (arxana-lab--open-file (plist-get entry :trace-path) "trace" #'org-mode))

;;;###autoload
(defun arxana-lab-open-raw-object (entry)
  "Open raw JSON for ENTRY."
  (interactive)
  (arxana-lab-open-raw-viewer (plist-get entry :raw-path)))

;;;###autoload
(defun arxana-lab-open-draft-object (entry)
  "Open doc draft JSON for ENTRY."
  (interactive)
  (arxana-lab--open-file (plist-get entry :draft-path) "draft" #'js-mode))

(defun arxana-lab-open-file-entry (entry)
  "Open a lab file ENTRY from `arxana-lab-file-items'."
  (let* ((kind (plist-get entry :kind))
         (path (plist-get entry :path))
         (mode (if (string= kind "stubs") #'org-mode #'js-mode)))
    (if (string= kind "raw")
        (arxana-lab-open-raw-viewer path)
      (if (string= kind "stubs")
          (arxana-lab-open-stub-viewer path)
        (arxana-lab--open-file path kind mode)))))

;; =============================================================================
;; Integration setup
;; =============================================================================

(defun arxana-lab-setup-hyperlinks ()
  "Set up hyperlink handlers for Lab viewer.
Call this after loading arxana-browser-patterns."
  (interactive)
  ;; Pattern browser - use arxana-browser-patterns if available
  (when (fboundp 'arxana-browser-pattern-show)
    (setq arxana-lab-pattern-browser-function
          (lambda (pattern-id)
            (arxana-browser-pattern-show pattern-id))))
  ;; Code browser - default find-file is usually fine
  (setq arxana-lab-code-browser-function #'find-file-other-window)
  (message "Lab hyperlinks configured"))

;;;###autoload
(defun arxana-lab-browse-musn-session (session-id)
  "Browse a MUSN session by SESSION-ID from the Lab."
  (interactive "sSession ID: ")
  (arxana-lab-browse-raw-session session-id))

;;;###autoload
(defun arxana-lab-browse-raw-session (session-id)
  "Browse any raw session by SESSION-ID from the Lab."
  (interactive "sSession ID: ")
  (let* ((root (arxana-lab--locate-root))
         (path (and root (expand-file-name (format "raw/%s.json" session-id) root))))
    (if (and path (file-exists-p path))
        (arxana-lab-open-raw-viewer path)
      (user-error "Session not found in lab/raw: %s" session-id))))

;; =============================================================================
;; Claude Code live refresh
;; =============================================================================

(defvar arxana-lab-claude-jsonl-path nil
  "Path to Claude Code JSONL file for live refresh.")

(defvar arxana-lab-claude-import-script
  "/home/joe/code/futon4/scripts/claude-jsonl-to-lab.clj"
  "Path to Claude Code JSONL import script.")

(defun arxana-lab-refresh-claude-session ()
  "Re-import the current Claude Code session from JSONL and refresh view."
  (interactive)
  (unless arxana-lab-claude-jsonl-path
    (setq arxana-lab-claude-jsonl-path
          (read-file-name "Claude JSONL file: " "~/.claude/projects/")))
  (let* ((session-id (or (and (string-match "\\*Lab:\\([^*]+\\)\\*" (buffer-name))
                              (match-string 1 (buffer-name)))
                         "current-futon3-session"))
         (cmd (format "cd /home/joe/code/futon4 && bb %s %s %s"
                      (shell-quote-argument arxana-lab-claude-import-script)
                      (shell-quote-argument arxana-lab-claude-jsonl-path)
                      (shell-quote-argument session-id))))
    (message "Refreshing from JSONL...")
    (shell-command cmd)
    (arxana-lab-browse-raw-session session-id)
    (message "Refreshed %s" session-id)))

(defun arxana-lab-set-claude-jsonl (path)
  "Set the Claude JSONL PATH for live refresh."
  (interactive "fClaude JSONL file: ")
  (setq arxana-lab-claude-jsonl-path path)
  (message "Claude JSONL set to: %s" path))

;; Add refresh binding to view-mode in Lab buffers
(defun arxana-lab--setup-refresh-key ()
  "Set up 'g' key for refresh in Lab buffers."
  (when (string-prefix-p "*Lab:" (buffer-name))
    (local-set-key (kbd "g") #'arxana-lab-refresh-claude-session)))

(add-hook 'view-mode-hook #'arxana-lab--setup-refresh-key)

(provide 'arxana-lab)

;;; arxana-lab.el ends here
