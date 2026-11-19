;;; mojira-mission-patterns.el --- Async mission patterns  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)
(defvar url-http-response-status)
(require 'button)
(require 'subr-x)

(defgroup mojira-mission-patterns nil "Clocked mission patterns." :group 'applications)
(defcustom mojira-mission-patterns-base-url "http://localhost:7070"
  "Agency URL." :type 'string)
(defvar-local mojira-mission-patterns--agent nil)
(defvar-local mojira-mission-patterns--generation nil)
(defvar-local mojira-mission-patterns--mission nil)
(defvar-local mojira-mission-patterns--scopes nil)
(defvar-local mojira-mission-patterns--sessions nil)

(declare-function arxana-browser-patterns--pattern-file "arxana-browser-patterns" (name))
(declare-function mission-mode-minor-mode "mission-mode" (&optional arg))
(declare-function mission-mode-refresh "mission-mode" ())
(defvar mission-mode-minor-mode)
(defvar mission-mode-scope-types)

(defcustom mojira-mission-patterns-code-root "~/code/"
  "Root whose repos hold holes/missions/ documents." :type 'directory)

(defun mojira-mission-patterns--mission-file (mission)
  "MISSION's document, found by one glob per layout (no recursive walk)."
  (let ((root (file-name-as-directory (expand-file-name mojira-mission-patterns-code-root))))
    (cl-some (lambda (pat) (car (file-expand-wildcards (concat root pat))))
             (list (concat "*/holes/missions/" mission ".md")
                   (concat "*/holes/" mission ".md")))))

(defun mojira-mission-patterns--open-document (mission)
  "Show MISSION's document in another window with mission-mode on.
mission-mode badges its pattern scopes (violet) once its async fetch lands."
  (let ((file (mojira-mission-patterns--mission-file mission)))
    (if (not file)
        (message "※ m p: no document for %s" mission)
      (let ((buf (find-file-noselect file)))
        (with-current-buffer buf
          (when (or (fboundp 'mission-mode-minor-mode)
                    (require 'mission-mode nil t))
            ;; Patterns only: one substrate read per binder, ~0.1 s, not every
            ;; scope. An already-open document is switched and refreshed too
            ;; (Joe 2026-11-19 saw no change when it was left alone).
            (setq-local mission-mode-scope-types '("pattern" "psr" "pur"))
            (if (bound-and-true-p mission-mode-minor-mode)
                (mission-mode-refresh)
              (mission-mode-minor-mode 1))))
        (display-buffer buf '((display-buffer-reuse-window
                               display-buffer-use-some-window)
                              (inhibit-same-window . t)))))))

(defun mojira-mission-patterns--agent ()
  "Current REPL's local id, else the most recently used REPL's id."
  (cl-loop for b in (cons (current-buffer) (delq (current-buffer) (buffer-list)))
           thereis
           (with-current-buffer b
             (cl-loop for var in '(claude-repl-agent-id codex-repl-agency-agent-id
                                  zai-repl-agent-id kimi-repl-agent-id)
                      when (and (local-variable-p var) (stringp (symbol-value var)))
                      return (symbol-value var)))))

(defun mojira-mission-patterns--fetch (path callback)
  "Fetch PATH asynchronously; call CALLBACK with (JSON ERROR)."
  (condition-case err
      (url-retrieve
       (concat mojira-mission-patterns-base-url path)
       (lambda (status)
         (let (data failure)
           (unwind-protect
               (condition-case e
                   (if (plist-get status :error)
                       (setq failure (format "%s" (plist-get status :error)))
                     (unless (and (boundp 'url-http-response-status)
                                  (numberp url-http-response-status)
                                  (<= 200 url-http-response-status 299))
                       (error "HTTP %s" url-http-response-status))
                     (goto-char (point-min))
                     (unless (re-search-forward "\r?\n\r?\n" nil t) (error "Missing HTTP body"))
                     (let ((json-object-type 'alist) (json-array-type 'list)
                           (json-key-type 'symbol) (json-false nil))
                       (setq data (json-read))))
                 (error (setq failure (error-message-string e))))
             (kill-buffer (current-buffer)))
           (funcall callback data failure))) nil t t)
    (error (funcall callback nil (error-message-string err)))))

(defun mojira-mission-patterns--open (id)
  "Open a local pattern file; never invoke a synchronous network opener."
  (condition-case err
      (let ((file (and (fboundp 'arxana-browser-patterns--pattern-file)
                       (arxana-browser-patterns--pattern-file id))))
        (unless file
          (let ((candidate (expand-file-name (concat id ".flexiarg") "~/code/futon3/library/")))
            (when (and (file-in-directory-p candidate "~/code/futon3/library/")
                       (file-exists-p candidate))
              (setq file candidate))))
        (if file (find-file-other-window file) (message "No local pattern file for %s" id)))
    (error (message "Pattern: %s" (error-message-string err)))))

(defun mojira-mission-patterns--scope-pattern (scope)
  "The pattern id a document SCOPE cites (pattern_id), not its scope id."
  (or (alist-get 'pattern_id scope) (alist-get 'id scope)))

(defun mojira-mission-patterns--line (id text face)
  "Insert clickable TEXT for ID with FACE."
  (insert-text-button text 'face face 'follow-link t
                      'action (lambda (_) (mojira-mission-patterns--open id)))
  (insert "\n"))

(defun mojira-mission-patterns--render ()
  "Render current responses; nil means still loading."
  (let* ((inhibit-read-only t)
         (scopes (alist-get 'scopes mojira-mission-patterns--scopes))
         (patterns (alist-get 'patterns mojira-mission-patterns--sessions))
         (ids (mapcar #'mojira-mission-patterns--scope-pattern scopes))
         (both (lambda (p) (member (alist-get 'id p) ids))))
    (erase-buffer)
    (insert (format "%s · %s\n\ng refresh · q quit · RET open local pattern\n★ both sections · document-only dim · session-only used bold\n\n"
                    (or mojira-mission-patterns--mission "Mission patterns")
                    mojira-mission-patterns--agent))
    (insert "In the mission document\n")
    (cond ((null mojira-mission-patterns--scopes) (insert "loading…\n"))
          ((alist-get 'error mojira-mission-patterns--scopes)
           (insert (format "Error: %s\n" (alist-get 'error mojira-mission-patterns--scopes))))
          ((null scopes) (insert "No document patterns.\n")))
    (dolist (s scopes)
      (let* ((id (mojira-mission-patterns--scope-pattern s))
             (shared (cl-find id patterns :key (lambda (p) (alist-get 'id p)) :test #'equal))
             (used (and shared (> (or (alist-get 'occurs shared) 0) 0))))
        (mojira-mission-patterns--line
         id (format "%s%s [%s] · %s%s" (if shared "★ " "")
                    (or (alist-get 'title s) id) (alist-get 'type s) id
                    (if used "" " · not used in sessions")) (if used 'default 'shadow))
        (insert (propertize
                 (concat "  " (truncate-string-to-width
                               (replace-regexp-in-string "[\n\r]+" " " (or (alist-get 'passage s) ""))
                               (max 10 (- (window-body-width (get-buffer-window (current-buffer))) 3))) "\n")
                 'face 'shadow))))
    (insert (format "\nIn sessions (%s turns read)\n" (or (alist-get 'turns_read mojira-mission-patterns--sessions) "…")))
    (cond ((null mojira-mission-patterns--sessions) (insert "loading…\n"))
          ((alist-get 'error mojira-mission-patterns--sessions)
           (insert (format "Error: %s\n" (alist-get 'error mojira-mission-patterns--sessions))))
          ((alist-get 'unindexed mojira-mission-patterns--sessions) (insert "Mission turn index unavailable.\n"))
          ((null patterns) (insert "No session patterns.\n")))
    (dolist (p (append (cl-remove-if-not both patterns) (cl-remove-if both patterns)))
      (let ((shared (funcall both p)) (id (alist-get 'id p)))
        (mojira-mission-patterns--line
         id (format "%s%s · used %s · proposed %s · %s sessions"
                    (if shared "★ " "") id (alist-get 'occurs p) (alist-get 'proposed p)
                    (length (alist-get 'sessions p)))
         (if (and (not shared) (> (or (alist-get 'occurs p) 0) 0)) 'bold 'default))))
    (goto-char (point-min))))

(defun mojira-mission-patterns-refresh (&optional open-document)
  "Refresh the retained agent's clock and both sections asynchronously.
With OPEN-DOCUMENT, also show the mission document in mission-mode."
  (interactive)
  (let ((buffer (current-buffer)) (token (list t))
        (agent mojira-mission-patterns--agent))
    (setq mojira-mission-patterns--generation token)
    (let ((inhibit-read-only t)) (erase-buffer) (insert "loading…\n"))
    (if (not agent)
        (let ((inhibit-read-only t)) (erase-buffer) (insert "No REPL agent found.\n"))
      (mojira-mission-patterns--fetch
       (concat "/api/alpha/agent-clock?agent-id=" (url-hexify-string agent))
       (lambda (clock err)
         (when (and (buffer-live-p buffer)
                    (eq token (buffer-local-value 'mojira-mission-patterns--generation buffer)))
           (with-current-buffer buffer
             (let ((mission (alist-get 'mission-id clock)))
               (if (or err (not (stringp mission)) (string-empty-p mission))
                   (let ((inhibit-read-only t))
                     (erase-buffer) (insert (if err (concat "Error: " err "\n") "Agent is not clocked into a mission.\n")))
                 (setq mojira-mission-patterns--mission mission
                       mojira-mission-patterns--scopes nil mojira-mission-patterns--sessions nil)
                 (when open-document (mojira-mission-patterns--open-document mission))
                 (mojira-mission-patterns--render)
                 (dolist (spec '(("mission-pattern-scopes" . mojira-mission-patterns--scopes)
                                 ("mission-session-patterns" . mojira-mission-patterns--sessions)))
                   (let ((target (cdr spec)))
                     (mojira-mission-patterns--fetch
                      (concat "/api/alpha/circus/" (car spec) "?mission=" (url-hexify-string mission) "&n=200")
                      (lambda (data failure)
                        (when (and (buffer-live-p buffer)
                                   (eq token (buffer-local-value 'mojira-mission-patterns--generation buffer)))
                          (with-current-buffer buffer
                            (set target (if failure `((error . ,failure)) (or data '((empty . t)))))
                            (mojira-mission-patterns--render))))))))))))))))

(defvar mojira-mission-patterns-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'mojira-mission-patterns-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'push-button)
    map))
(define-derived-mode mojira-mission-patterns-mode special-mode "Mission patterns"
  "Read-only asynchronous mission pattern view.")

;;;###autoload
(defun mojira-mission-patterns ()
  "Open the clocked mission's patterns: its document in mission-mode (pattern
scopes badged) plus a right side window listing document and session patterns."
  (interactive)
  (let ((agent (mojira-mission-patterns--agent))
        (buffer (get-buffer-create "*※ m p*")))
    (with-current-buffer buffer
      (mojira-mission-patterns-mode)
      (setq mojira-mission-patterns--agent agent))
    (display-buffer-in-side-window buffer '((side . right) (window-width . 0.35)))
    (with-current-buffer buffer (mojira-mission-patterns-refresh t))))

(provide 'mojira-mission-patterns)
;;; mojira-mission-patterns.el ends here
