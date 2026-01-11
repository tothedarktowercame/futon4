;;; arxana-ui.el --- Shared UI helpers for Arxana -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared helpers for header styling and focal/ancillary state.

;;; Code:

(require 'subr-x)
(require 'seq)

(defgroup arxana-ui nil
  "Customization group for shared Arxana UI helpers."
  :group 'arxana)

(defface arxana-ui-header-focal
  '((t :background "#0f172a" :foreground "#f8fafc" :weight bold))
  "Face for focal Arxana headers."
  :group 'arxana-ui)

(defface arxana-ui-header-ancillary
  '((t :background "#3b0764" :foreground "#f8fafc" :weight bold))
  "Face for ancillary Arxana headers."
  :group 'arxana-ui)

(defvar-local arxana-ui-managed nil
  "Non-nil when buffer participates in Arxana header handling.")
(defvar-local arxana-ui--base-header-line nil)
(defvar-local arxana-ui--label nil)
(defvar-local arxana-ui--focal nil)
(defvar-local arxana-ui-return-buffer nil
  "Buffer to return to when using the left-at-point-min shortcut.")
(defvar-local arxana-ui-return-window-config nil
  "Window configuration to restore when using the left-at-point-min shortcut.")

(defun arxana-ui-left-or-return ()
  "Move left or return when at point-min."
  (interactive)
  (if (> (point) (point-min))
      (backward-char)
    (cond
     ((and arxana-ui-return-window-config
           (window-configuration-p arxana-ui-return-window-config))
      (set-window-configuration arxana-ui-return-window-config))
     ((buffer-live-p arxana-ui-return-buffer)
      (set-window-buffer (selected-window) arxana-ui-return-buffer))
     ((and (fboundp 'arxana-browser--up)
           (eq major-mode 'arxana-browser-mode))
      (arxana-browser--up))
     ((buffer-live-p (get-buffer "*Arxana Browser*"))
      (set-window-buffer (selected-window) "*Arxana Browser*"))
     (t (message "No return target available")))))

(defvar arxana-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") #'arxana-ui-left-or-return)
    map)
  "Keymap for `arxana-ui-mode'.")

(define-minor-mode arxana-ui-mode
  "Minor mode for shared Arxana UI behavior."
  :keymap arxana-ui-mode-map)

(defun arxana-ui--format-header ()
  (let* ((base (or arxana-ui--base-header-line ""))
         (raw (string-trim (format-mode-line base)))
         (text (if (string-empty-p raw)
                   (or arxana-ui--label (buffer-name))
                 raw))
         (face (if arxana-ui--focal
                   'arxana-ui-header-focal
                 'arxana-ui-header-ancillary)))
    (propertize (format " %s " text) 'face face)))

;;;###autoload
(defun arxana-ui-mark-managed (&optional label)
  "Enable Arxana header styling for the current buffer."
  (setq-local arxana-ui-managed t)
  (setq-local arxana-ui--label label)
  (when (null arxana-ui--base-header-line)
    (setq-local arxana-ui--base-header-line header-line-format))
  (setq-local header-line-format '(:eval (arxana-ui--format-header)))
  (arxana-ui-mode 1)
  (arxana-ui-refresh))

(defun arxana-ui--leftmost-window (frame)
  (let ((wins (window-list frame 'no-mini)))
    (car (sort (copy-sequence wins)
               (lambda (a b)
                 (< (car (window-edges a))
                    (car (window-edges b))))))))

;;;###autoload
(defun arxana-ui-refresh ()
  "Recompute focal/ancillary header styling in all frames."
  (dolist (frame (frame-list))
    (let* ((wins (window-list frame 'no-mini))
           (managed (seq-filter (lambda (win)
                                  (with-current-buffer (window-buffer win)
                                    arxana-ui-managed))
                                wins))
           (min-left (when managed
                       (apply #'min (mapcar (lambda (win)
                                              (car (window-edges win)))
                                            managed))))
           (selected (selected-window))
           (focal (cond
                   ((and (memq selected managed)
                         (= (car (window-edges selected)) min-left))
                    selected)
                   (managed
                    (seq-find (lambda (win)
                                (= (car (window-edges win)) min-left))
                              managed))
                   (t nil))))
      (dolist (win wins)
        (let ((buf (window-buffer win)))
          (with-current-buffer buf
            (when arxana-ui-managed
              (setq arxana-ui--focal (eq win focal))
              (force-mode-line-update))))))
      (when (fboundp 'arxana-window-constraints-validate-ui-focal)
        (arxana-window-constraints-validate-ui-focal frame))))

(add-hook 'window-configuration-change-hook #'arxana-ui-refresh)

(provide 'arxana-ui)
;;; arxana-ui.el ends here
