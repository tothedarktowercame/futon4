;;; mojira-related-sessions.el --- Sessions sharing a session's patterns -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'button)
(require 'seq)
(require 'mojira-mission-patterns)
(require 'subr-x)

(defgroup mojira-related-sessions nil "Sessions related by shared patterns." :group 'applications)
(defcustom mojira-related-sessions-limit 10
  "Maximum number of related sessions shown by default."
  :type 'integer)

(defvar-local mojira-related-sessions--agent nil)
(defvar-local mojira-related-sessions--generation nil)
(defvar-local mojira-related-sessions--session nil)
(defvar-local mojira-related-sessions--limit nil)
(defvar-local mojira-related-sessions--total nil)
(defvar-local mojira-related-sessions--data nil)

(defun mojira-related-sessions--current-session ()
  "Return the real session id of the REPL buffer invoking the command."
  (when (and (boundp 'agent-chat--session-id)
             (stringp agent-chat--session-id)
             (not (string-empty-p agent-chat--session-id))
             (not (equal agent-chat--session-id "pending")))
    agent-chat--session-id))

(defun mojira-related-sessions--session-buffer (session)
  "Return an open REPL buffer for SESSION, if one exists here."
  (or (and (fboundp 'claude-repl-find-buffer-by-session-id)
           (claude-repl-find-buffer-by-session-id session))
      (seq-find
       (lambda (buffer)
         (and (buffer-live-p buffer)
              (with-current-buffer buffer
                (and (boundp 'agent-chat--session-id)
                     (equal agent-chat--session-id session)))))
       (buffer-list))))

(defun mojira-related-sessions--open-session (session)
  "Pop to SESSION's local REPL, or explain that it is not open here."
  (if-let* ((buffer (mojira-related-sessions--session-buffer session)))
      (pop-to-buffer buffer)
    (message "※ s x p: session %s is not open in this Emacs" session)))

(defun mojira-related-sessions--open-pattern (pattern)
  (mojira-mission-patterns--open pattern))

(defun mojira-related-sessions--short-session (session)
  (if (> (length session) 18)
      (concat (substring session 0 7) "…" (substring session -8))
    session))

(defun mojira-related-sessions--session-button (session)
  (insert-text-button (mojira-related-sessions--short-session session)
                      'face 'link 'follow-link t
                      'mojira-session session
                      'action (lambda (button)
                               (mojira-related-sessions--open-session
                                (button-get button 'mojira-session)))))

(defun mojira-related-sessions--pattern-button (pattern)
  (insert-text-button (format "    %s" pattern)
                      'face 'link 'follow-link t
                      'mojira-pattern pattern
                      'action (lambda (button)
                               (mojira-related-sessions--open-pattern
                                (button-get button 'mojira-pattern))))
  (insert "\n"))

(defun mojira-related-sessions--render ()
  "Render the current related-sessions response."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "※ s x p · %s (%d patterns) · top %d%s\n\n"
                    (or mojira-related-sessions--session "Session")
                    (length (or (alist-get 'patterns mojira-related-sessions--data) nil))
                    (or mojira-related-sessions--limit mojira-related-sessions-limit)
                    (if mojira-related-sessions--total
                        (format " of %d" mojira-related-sessions--total)
                      "")))
    (insert "g refresh · q quit · RET open session or pattern\n\n")
    (cond
     ((null mojira-related-sessions--data) (insert "loading…\n"))
     ((alist-get 'error mojira-related-sessions--data)
      (insert (format "Error: %s\n" (alist-get 'error mojira-related-sessions--data))))
     ((null (alist-get 'related mojira-related-sessions--data))
      (insert "No related sessions.\n"))
     (t
      (dolist (related (alist-get 'related mojira-related-sessions--data))
        (let ((session (alist-get 'session related))
              (shared (alist-get 'shared related)))
          ;; the count first: a narrow side window cuts the end of the line (Joe 2026-11-21)
          (insert (format "%.2f  %d shared pattern%s  " (or (alist-get 'score related) 0.0)
                          (length shared) (if (= 1 (length shared)) "" "s")))
          (mojira-related-sessions--session-button session)
          (insert (format "  %s  %s  %s\n"
                          (or (alist-get 'agent related) "—")
                          (or (alist-get 'mission related) "—")
                          (or (alist-get 'latest_at related) "—")))
          (dolist (pattern shared)
            (mojira-related-sessions--pattern-button pattern))))))
    (goto-char (point-min))))

(defun mojira-related-sessions-refresh (&optional limit)
  "Refresh sessions related to the invoking REPL's session asynchronously."
  (interactive)
  (let ((buffer (current-buffer)) (token (list t))
        (session mojira-related-sessions--session)
        (limit (or limit mojira-related-sessions--limit mojira-related-sessions-limit)))
    (setq mojira-related-sessions--generation token
          mojira-related-sessions--limit limit
          mojira-related-sessions--total nil)
    (let ((inhibit-read-only t)) (erase-buffer) (insert "loading…\n"))
    (if (not session)
        (let ((inhibit-read-only t))
          (erase-buffer) (insert "No current REPL session found.\n"))
      (mojira-mission-patterns--fetch
       (format "/api/alpha/circus/session-related-sessions?session=%s&since-hours=24&n=%d"
               (url-hexify-string session) (1+ limit))
       (lambda (data failure)
         (when (and (buffer-live-p buffer)
                    (eq token (buffer-local-value
                               'mojira-related-sessions--generation buffer)))
           (with-current-buffer buffer
             (if failure
                 (setq mojira-related-sessions--data `((error . ,failure))
                       mojira-related-sessions--total nil)
               (let* ((payload (copy-alist (or data '())))
                      (related (or (alist-get 'related payload) nil))
                      (more (> (length related) limit)))
                 (setf (alist-get 'related payload)
                       (if more (seq-take related limit) related))
                 (setq mojira-related-sessions--data payload
                       mojira-related-sessions--total
                       (and more (length related)))))
             (mojira-related-sessions--render))))))))

(defvar mojira-related-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'mojira-related-sessions-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'push-button)
    map))

(define-derived-mode mojira-related-sessions-mode special-mode "Related sessions"
  "Read-only asynchronous view of sessions sharing a session's patterns.")

;;;###autoload
(defun mojira-related-sessions (&optional prefix)
  "Show sessions sharing patterns with the current REPL session."
  (interactive "P")
  (let ((agent (mojira-mission-patterns--agent))
        (session (mojira-related-sessions--current-session))
        (buffer (get-buffer-create "*※ s x p*"))
        (limit (if prefix (prefix-numeric-value prefix) mojira-related-sessions-limit)))
    (with-current-buffer buffer
      (mojira-related-sessions-mode)
      (setq mojira-related-sessions--agent agent
            mojira-related-sessions--session session
            mojira-related-sessions--limit limit))
    (display-buffer-in-side-window buffer '((side . right) (window-width . 0.35)))
    (with-current-buffer buffer (mojira-related-sessions-refresh limit))))

(provide 'mojira-related-sessions)
;;; mojira-related-sessions.el ends here
