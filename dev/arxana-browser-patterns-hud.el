;;; arxana-browser-patterns-hud.el --- Pattern backlinks HUD -*- lexical-binding: t; -*-

;;; Commentary:
;; Render a Stack-style HUD listing Org TODOs linked to the current pattern.

;;; Code:

(require 'subr-x)
(require 'hud-service)
(require 'arxana-browser-patterns)
(require 'arxana-ui nil t)

(defgroup arxana-browser-patterns-hud nil
  "HUD display for Arxana pattern backlinks."
  :group 'arxana)

(defcustom arxana-browser-patterns-hud-buffer-name "*Arxana Pattern HUD*"
  "Buffer name for the pattern backlinks HUD."
  :type 'string
  :group 'arxana-browser-patterns-hud)

(defcustom arxana-browser-patterns-hud-auto nil
  "When non-nil, render the pattern HUD when backlinks are refreshed."
  :type 'boolean
  :group 'arxana-browser-patterns-hud)

(defcustom arxana-browser-patterns-hud-window-width 0.33
  "Width of the pattern HUD side window."
  :type 'number
  :group 'arxana-browser-patterns-hud)

(defcustom arxana-browser-patterns-hud-max-items 20
  "Maximum number of backlinks to render in the HUD."
  :type 'integer
  :group 'arxana-browser-patterns-hud)

(defvar-local arxana-browser-patterns-hud--pattern nil)
(defvar-local arxana-browser-patterns-hud--backlinks nil)
(defvar-local arxana-browser-patterns-hud--current-entry nil)
(defvar arxana-browser-patterns-hud--window nil)

(defun arxana-browser-patterns-hud--insert-backlink (entry indent)
  (let ((label (arxana-browser-patterns--backlink-label entry)))
    (when label
      (let ((line-start (point)))
        (insert (format "%s- %s" indent label))
        (let ((line-end (point)))
          (make-text-button
           line-start line-end
           'action #'arxana-browser-patterns-open-backlink
           'follow-link t
           'help-echo "Open Org task"
           'keymap arxana-browser-patterns--backlink-keymap
           'arxana-org-id (plist-get entry :org-id)
           'arxana-org-file (plist-get entry :org-file)
           'arxana-hud-kind "org-task"
           'arxana-task-label label
           'arxana-task-entity (plist-get entry :entity-id)))
        (insert "\n")
        (let ((todo (plist-get entry :org/todo))
              (heading (plist-get entry :org/heading))
              (outline (plist-get entry :org/outline))
              (scheduled (plist-get entry :org/scheduled))
              (org-file (plist-get entry :org/file))
              (org-id (or (plist-get entry :org/id) (plist-get entry :org-id))))
          (when (and todo (stringp todo) (> (length todo) 0))
            (insert (format "%s  state: %s\n" indent todo)))
          (when (and scheduled (stringp scheduled) (> (length scheduled) 0))
            (insert (format "%s  scheduled: %s\n" indent scheduled)))
          (when (and outline (stringp outline) (> (length outline) 0))
            (insert (format "%s  outline: %s\n" indent outline)))
          (when (and org-file (stringp org-file) (> (length org-file) 0))
            (insert (format "%s  file: %s\n" indent (file-name-nondirectory org-file))))
          (when (and org-id (stringp org-id) (> (length org-id) 0))
            (insert (format "%s  id: %s\n" indent org-id))))))))

(defun arxana-browser-patterns-hud--render (pattern-name backlinks current-entry)
  (let ((indent "  ")
        (total (length backlinks)))
    (insert (format "Pattern HUD\n\nPattern: %s\n\n" pattern-name))
    (insert (format "Linked TODOs: %d\n\n" total))
    (if current-entry
        (progn
          (insert "Focused TODO:\n")
          (arxana-browser-patterns-hud--insert-backlink current-entry indent)
          (insert "\n"))
      (insert "Focused TODO: (none)\n\nHover a TODO line in the pattern to see details.\n")))
  (setq-local arxana-browser-patterns-hud--pattern pattern-name)
  (setq-local arxana-browser-patterns-hud--backlinks backlinks)
  (setq-local arxana-browser-patterns-hud--current-entry current-entry))

(defun arxana-browser-patterns-hud-render (pattern-name backlinks &optional current-entry)
  "Render HUD for PATTERN-NAME using BACKLINKS."
  (futon-hud-service-render
   arxana-browser-patterns-hud-buffer-name
   (lambda (_buf _win)
     (arxana-browser-patterns-hud--render pattern-name backlinks current-entry))
   (list :mode 'special-mode
         :truncate-lines t
         :side 'right
         :window-width arxana-browser-patterns-hud-window-width))
  (setq arxana-browser-patterns-hud--window
        (get-buffer-window arxana-browser-patterns-hud-buffer-name nil))
  (when (fboundp 'arxana-ui-mark-managed)
    (with-current-buffer arxana-browser-patterns-hud-buffer-name
      (arxana-ui-mark-managed "Arxana Pattern HUD")))
  (when (fboundp 'arxana-window-constraints-validate-patterns-hud)
    (arxana-window-constraints-validate-patterns-hud
     arxana-browser-patterns-hud--window)))

(defun arxana-browser-patterns-hud-refresh (&optional pattern-name)
  "Fetch backlinks for PATTERN-NAME and render them in the HUD."
  (interactive)
  (let* ((name (or pattern-name
                   arxana-browser-patterns-hud--pattern
                   (and (boundp 'arxana-browser-patterns--pattern)
                        (plist-get arxana-browser-patterns--pattern :name))
                   (and (buffer-file-name)
                        (arxana-patterns-ingest--derive-name-from-path (buffer-file-name))))))
    (unless (and name (not (string-empty-p name)))
      (user-error "No pattern name available"))
    (if (fboundp 'arxana-store-ego-async)
        (arxana-store-ego-async
         name
         (lambda (resp status)
           (if (plist-get status :error)
               (message "HUD backlinks fetch failed: %s" (plist-get status :error))
             (let ((backlinks (arxana-browser-patterns--pattern-backlinks-from-response resp)))
               (arxana-browser-patterns-hud-render name backlinks))))
         arxana-browser-patterns-ego-limit)
      (let ((backlinks (arxana-browser-patterns--pattern-backlinks name)))
        (arxana-browser-patterns-hud-render name backlinks)))))

(defun arxana-browser-patterns-hud-hide ()
  "Hide the pattern HUD window if it is visible."
  (when (window-live-p arxana-browser-patterns-hud--window)
    (let ((win arxana-browser-patterns-hud--window))
      (set-window-parameter win 'futon-hud-owner nil)
      (set-window-dedicated-p win nil)
      (delete-window win)))
  (setq arxana-browser-patterns-hud--window nil))

(defun arxana-browser-patterns-hud--entry-at-point ()
  (get-text-property (point) 'arxana-hud-entry))

(defun arxana-browser-patterns-hud--pattern-at-point ()
  (get-text-property (point) 'arxana-hud-pattern))

(defun arxana-browser-patterns-hud--post-command ()
  (let ((entry (arxana-browser-patterns-hud--entry-at-point)))
    (if entry
        (unless (equal entry arxana-browser-patterns-hud--current-entry)
          (setq arxana-browser-patterns-hud--current-entry entry)
          (let ((pattern (or (arxana-browser-patterns-hud--pattern-at-point)
                             arxana-browser-patterns-hud--pattern
                             (and (boundp 'arxana-browser-patterns--backlinks-pattern)
                                  arxana-browser-patterns--backlinks-pattern)))
                (backlinks (or arxana-browser-patterns-hud--backlinks
                               (and (boundp 'arxana-browser-patterns--backlinks-cache)
                                    arxana-browser-patterns--backlinks-cache))))
            (arxana-browser-patterns-hud-render pattern backlinks entry)))
      (when arxana-browser-patterns-hud--current-entry
        (setq arxana-browser-patterns-hud--current-entry nil)
        (arxana-browser-patterns-hud-hide)))))

(define-minor-mode arxana-browser-patterns-hud-follow-mode
  "Show the pattern HUD only when point is on a linked TODO."
  :lighter " Pattern-HUD"
  (if arxana-browser-patterns-hud-follow-mode
      (add-hook 'post-command-hook #'arxana-browser-patterns-hud--post-command nil t)
    (remove-hook 'post-command-hook #'arxana-browser-patterns-hud--post-command t)
    (arxana-browser-patterns-hud-hide)))

(provide 'arxana-browser-patterns-hud)

;;; arxana-browser-patterns-hud.el ends here
