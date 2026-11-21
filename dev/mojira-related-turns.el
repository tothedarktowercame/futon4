;;; mojira-related-turns.el --- Turns sharing a turn's patterns -*- lexical-binding: t; -*-

(require 'button)
(require 'cl-lib)
(require 'mojira-mission-patterns)
(require 'seq)
(require 'subr-x)

(defgroup mojira-related-turns nil "Turns related by shared patterns." :group 'applications)
(defcustom mojira-related-turns-limit 10
  "Maximum number of related turns shown by default."
  :type 'integer)

(defvar-local mojira-related-turns--agent nil)
(defvar-local mojira-related-turns--generation nil)
(defvar-local mojira-related-turns--turn nil)
(defvar-local mojira-related-turns--session nil)
(defvar-local mojira-related-turns--limit nil)
(defvar-local mojira-related-turns--total nil)
(defvar-local mojira-related-turns--data nil)

(defun mojira-related-turns--region-entry (entry)
  "Return ENTRY's (BEG . END) region when its markers are live."
  (let ((region (cddr entry)))
    (when (and (markerp (car region)) (markerp (cdr region))
               (marker-position (car region)) (marker-position (cdr region)))
      region)))

(defun mojira-related-turns--turn-at-point ()
  "Return the turn id whose captured text contains point, or nil."
  (when (boundp 'session-mode--turn-text-regions)
    (seq-some
     (lambda (entry)
       (let ((region (mojira-related-turns--region-entry entry)))
         (and region (<= (car region) (point)) (< (point) (cdr region))
              (car entry))))
     session-mode--turn-text-regions)))

(defun mojira-related-turns--newest-turn ()
  "Return the newest captured turn id in the current REPL buffer."
  (when (boundp 'session-mode--turn-text-regions)
    (seq-some (lambda (entry)
                (and (mojira-related-turns--region-entry entry) (car entry)))
              session-mode--turn-text-regions)))

(defun mojira-related-turns--open (turn)
  "Move point to TURN in its open REPL buffer, or report that it is absent."
  (let* ((target-session mojira-related-turns--session)
        (buffer (seq-find
                 (lambda (candidate)
                   (and (buffer-live-p candidate)
                        (with-current-buffer candidate
                          (and (boundp 'agent-chat--session-id)
                               (equal agent-chat--session-id
                                      target-session)))))
                 (buffer-list))))
    (if (not buffer)
        (message "※ t x p: turn %s is not open in this Emacs" turn)
      (with-current-buffer buffer
        (let ((entry (seq-find (lambda (candidate) (equal (car candidate) turn))
                               (and (boundp 'session-mode--turn-text-regions)
                                    session-mode--turn-text-regions))))
          (if-let* ((region (and entry (mojira-related-turns--region-entry entry))))
              (progn
                (goto-char (car region))
                (message "※ t x p: moved to turn %s" turn))
            (message "※ t x p: turn %s has no captured text here" turn)))))))

(defun mojira-related-turns--open-pattern (pattern)
  (mojira-mission-patterns--open pattern))

(defun mojira-related-turns--short-id (turn)
  (if (> (length turn) 24)
      (concat (substring turn 0 10) "…" (substring turn -11))
    turn))

(defun mojira-related-turns--pattern-button (pattern)
  (insert-text-button (format "    %s" pattern)
                      'face 'link 'follow-link t
                      'mojira-pattern pattern
                      'action (lambda (button)
                               (mojira-related-turns--open-pattern
                                (button-get button 'mojira-pattern))))
  (insert "\n"))

(defun mojira-related-turns--turn-button (turn)
  (insert-text-button (mojira-related-turns--short-id turn)
                      'face 'link 'follow-link t
                      'mojira-turn turn
                      'action (lambda (button)
                               (mojira-related-turns--open
                                (button-get button 'mojira-turn)))))

(defun mojira-related-turns--render ()
  "Render the current related-turns response."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "※ t x p · %s (%d patterns) · top %d%s\n\n"
                    (or mojira-related-turns--turn "Turn")
                    (length (or (alist-get 'patterns mojira-related-turns--data) nil))
                    (or mojira-related-turns--limit mojira-related-turns-limit)
                    (if mojira-related-turns--total
                        (format " of %d" mojira-related-turns--total)
                      "")))
    (insert "g refresh · q quit · RET open turn or pattern\n\n")
    (cond
     ((null mojira-related-turns--data) (insert "loading…\n"))
     ((alist-get 'error mojira-related-turns--data)
      (insert (format "Error: %s\n" (alist-get 'error mojira-related-turns--data))))
     ((null (alist-get 'related mojira-related-turns--data))
      (insert "No related turns.\n"))
     (t
      (dolist (related (alist-get 'related mojira-related-turns--data))
        (let ((turn (alist-get 'turn related))
              (shared (alist-get 'shared related))
              (excerpt (truncate-string-to-width
                        (replace-regexp-in-string
                         "[\n\r]+" " " (string-trim (or (alist-get 'excerpt related) "")))
                        120 nil nil "…")))
          (insert (format "%.2f  " (or (alist-get 'score related) 0.0)))
          (mojira-related-turns--turn-button turn)
          (insert (format "  %s  %s  %s  %s  (%d shared patterns)\n"
                          (or (alist-get 'agent related)
                              (alist-get 'speaker related) "—")
                          (or (alist-get 'mission related) "—")
                          (or (alist-get 'created_at related) "—")
                          excerpt
                          (length shared)))
          (dolist (pattern shared)
            (mojira-related-turns--pattern-button pattern))))))
    (goto-char (point-min))))

(defun mojira-related-turns-refresh (&optional limit)
  "Refresh turns related to the turn selected in the invoking REPL."
  (interactive)
  (let ((buffer (current-buffer)) (token (list t))
        (turn mojira-related-turns--turn)
        (limit (or limit mojira-related-turns--limit mojira-related-turns-limit)))
    (setq mojira-related-turns--generation token
          mojira-related-turns--limit limit
          mojira-related-turns--total nil)
    (let ((inhibit-read-only t)) (erase-buffer) (insert "loading…\n"))
    (if (not turn)
        (let ((inhibit-read-only t))
          (erase-buffer) (insert "No current turn found.\n"))
      (mojira-mission-patterns--fetch
       (format "/api/alpha/circus/turn-related-turns?turn=%s&n=%d"
               (url-hexify-string turn) (1+ limit))
       (lambda (data failure)
         (when (and (buffer-live-p buffer)
                    (eq token (buffer-local-value
                               'mojira-related-turns--generation buffer)))
           (with-current-buffer buffer
             (if failure
                 (setq mojira-related-turns--data `((error . ,failure))
                       mojira-related-turns--total nil)
               (let* ((payload (copy-alist (or data '())))
                      (related (or (alist-get 'related payload) nil))
                      (more (> (length related) limit)))
                 (setf (alist-get 'related payload)
                       (if more (seq-take related limit) related))
                 (setq mojira-related-turns--data payload
                       mojira-related-turns--total
                       (and more (length related)))))
             (mojira-related-turns--render))))))))

(defvar mojira-related-turns-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'mojira-related-turns-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'push-button)
    map))

(define-derived-mode mojira-related-turns-mode special-mode "Related turns"
  "Read-only asynchronous view of turns sharing a turn's patterns.")

;;;###autoload
(defun mojira-related-turns (&optional prefix)
  "Show turns sharing patterns with the turn at point in the current REPL."
  (interactive "P")
  (let* ((turn-at-point (mojira-related-turns--turn-at-point))
         (turn (or turn-at-point (mojira-related-turns--newest-turn)))
         (agent (mojira-mission-patterns--agent))
         (session (and (boundp 'agent-chat--session-id) agent-chat--session-id))
         (buffer (get-buffer-create "*※ t x p*"))
         (limit (if prefix (prefix-numeric-value prefix) mojira-related-turns-limit)))
    (with-current-buffer buffer
      (mojira-related-turns-mode)
      (setq mojira-related-turns--agent agent
            mojira-related-turns--turn turn
            mojira-related-turns--session session
            mojira-related-turns--limit limit))
    (unless turn-at-point
      (message "※ t x p: no turn at point; using newest turn %s" (or turn "<none>")))
    (display-buffer-in-side-window buffer '((side . right) (window-width . 0.35)))
    (with-current-buffer buffer (mojira-related-turns-refresh limit))))

(provide 'mojira-related-turns)
;;; mojira-related-turns.el ends here
