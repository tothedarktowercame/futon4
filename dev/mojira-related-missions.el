;;; mojira-related-missions.el --- Missions sharing the clocked mission's patterns -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'button)
(require 'mojira-mission-patterns)
(require 'subr-x)

(defgroup mojira-related-missions nil "Missions related by shared patterns." :group 'applications)
(defcustom mojira-related-missions-limit 10
  "Maximum number of related missions requested."
  :type 'integer)

(defvar-local mojira-related-missions--agent nil)
(defvar-local mojira-related-missions--generation nil)
(defvar-local mojira-related-missions--mission nil)
(defvar-local mojira-related-missions--data nil)
(defvar-local mojira-related-missions--limit nil)
(defvar-local mojira-related-missions--total nil)

(defun mojira-related-missions--open-mission (mission)
  "Open MISSION without restricting mission-mode to particular scope types."
  (let ((file (mojira-mission-patterns--mission-file mission)))
    (if (not file)
        (message "※ m x p: no document for %s" mission)
      (let ((buf (find-file-noselect file)))
        (with-current-buffer buf
          (when (or (fboundp 'mission-mode-minor-mode)
                    (require 'mission-mode nil t))
            ;; This view deliberately leaves all mission scope types visible.
            (setq-local mission-mode-scope-types nil)
            (if (bound-and-true-p mission-mode-minor-mode)
                (mission-mode-refresh)
              (mission-mode-minor-mode 1))))
        (display-buffer buf '((display-buffer-reuse-window
                               display-buffer-use-some-window)
                              (inhibit-same-window . t)))))))

(defun mojira-related-missions--open-pattern (pattern)
  "Open PATTERN using the same local pattern opener as the mission-patterns view."
  (mojira-mission-patterns--open pattern))

(defun mojira-related-missions--mission-button (mission)
  (insert-text-button mission
                      'face 'link 'follow-link t
                      'mojira-mission mission
                      'action (lambda (button)
                               (mojira-related-missions--open-mission
                                (button-get button 'mojira-mission)))))

(defun mojira-related-missions--pattern-button (pattern)
  (insert-text-button (format "    %s" pattern)
                      'face 'link 'follow-link t
                      'mojira-pattern pattern
                      'action (lambda (button)
                               (mojira-related-missions--open-pattern
                                (button-get button 'mojira-pattern))))
  (insert "\n"))

(defun mojira-related-missions--render ()
  "Render the current related-missions response."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "※ m x p · %s (%d patterns) · top %d%s\n\n"
                    (or mojira-related-missions--mission "Mission")
                    (length (or (alist-get 'patterns mojira-related-missions--data) nil))
                    (or mojira-related-missions--limit mojira-related-missions-limit)
                    (if mojira-related-missions--total
                        ;; only limit+1 rows are fetched: we know more exist, not how many
                        " (more exist; C-u N ※ m x p for more)"
                      "")))
    (insert "g refresh · q quit · RET open mission or pattern\n\n")
    (cond
     ((null mojira-related-missions--data) (insert "loading…\n"))
     ((alist-get 'error mojira-related-missions--data)
      (insert (format "Error: %s\n" (alist-get 'error mojira-related-missions--data))))
     ((null (alist-get 'related mojira-related-missions--data))
      (insert "No related missions.\n"))
     (t
      (dolist (related (alist-get 'related mojira-related-missions--data))
        (let ((mission (alist-get 'mission related))
              (shared (alist-get 'shared related)))
          (insert (format "%.2f  " (or (alist-get 'score related) 0.0)))
          (mojira-related-missions--mission-button mission)
          (insert (format "  (%d shared patterns)\n" (length shared)))
          (dolist (pattern shared)
            (mojira-related-missions--pattern-button pattern))))))
    (goto-char (point-min))))

(defun mojira-related-missions-refresh (&optional limit)
  "Refresh the clocked mission and its related missions asynchronously."
  (interactive)
  (let ((buffer (current-buffer)) (token (list t))
        (agent mojira-related-missions--agent)
        (limit (or limit mojira-related-missions--limit mojira-related-missions-limit)))
    (setq mojira-related-missions--generation token)
    (setq mojira-related-missions--limit limit
          mojira-related-missions--total nil)
    (let ((inhibit-read-only t)) (erase-buffer) (insert "loading…\n"))
    (if (not agent)
        (let ((inhibit-read-only t)) (erase-buffer) (insert "No REPL agent found.\n"))
      (mojira-mission-patterns--fetch
       (concat "/api/alpha/agent-clock?agent-id=" (url-hexify-string agent))
       (lambda (clock err)
         (when (and (buffer-live-p buffer)
                    (eq token (buffer-local-value 'mojira-related-missions--generation buffer)))
           (with-current-buffer buffer
             (let ((mission (alist-get 'mission-id clock)))
               (if (or err (not (stringp mission)) (string-empty-p mission))
                   (let ((inhibit-read-only t))
                     (erase-buffer)
                     (insert (if err (concat "Error: " err "\n")
                               "Agent is not clocked into a mission.\n")))
                 (setq mojira-related-missions--mission mission
                       mojira-related-missions--data nil
                       mojira-related-missions--total nil)
                 (mojira-related-missions--render)
                 (mojira-mission-patterns--fetch
                  (format "/api/alpha/circus/mission-related-missions?mission=%s&n=%d"
                          (url-hexify-string mission) (1+ limit))
                  (lambda (data failure)
                    (when (and (buffer-live-p buffer)
                               (eq token (buffer-local-value
                                          'mojira-related-missions--generation buffer)))
                      (with-current-buffer buffer
                        (if failure
                            (progn
                              (setq mojira-related-missions--data `((error . ,failure))
                                    mojira-related-missions--total nil))
                          (let* ((payload (copy-alist (or data '())))
                                 (related (or (alist-get 'related payload) nil))
                                 (more (> (length related) limit)))
                            (setf (alist-get 'related payload)
                                  (if more (seq-take related limit) related))
                            (setq mojira-related-missions--data payload
                                  mojira-related-missions--total
                                  (and more (length related)))))
                        (mojira-related-missions--render))))))))))))))

(defvar mojira-related-missions-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'mojira-related-missions-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'push-button)
    map))

(define-derived-mode mojira-related-missions-mode special-mode "Related missions"
  "Read-only asynchronous view of missions sharing the clocked mission's patterns.")

;;;###autoload
(defun mojira-related-missions (&optional prefix)
  "Show missions that share patterns with the clocked mission."
  (interactive "P")
  (let ((agent (mojira-mission-patterns--agent))
        (buffer (get-buffer-create "*※ m x p*"))
        (limit (if prefix (prefix-numeric-value prefix) mojira-related-missions-limit)))
    (with-current-buffer buffer
      (mojira-related-missions-mode)
      (setq mojira-related-missions--agent agent
            mojira-related-missions--limit limit))
    (display-buffer-in-side-window buffer '((side . right) (window-width . 0.35)))
    (with-current-buffer buffer (mojira-related-missions-refresh limit))))

(provide 'mojira-related-missions)
;;; mojira-related-missions.el ends here
