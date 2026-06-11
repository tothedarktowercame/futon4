;;; arxana-essays-twoup.el --- Section outline ↔ content, side by side -*- lexical-binding: t; -*-

;; Joe, 2026-06-11 (submission-day infrastructure): when the cursor rests
;; on a section row in the Essays outline, the OTHER window shows that
;; section's content from the source markdown. Model = the source file;
;; view = outline + content window; controller = cursor motion.
;;
;; Discipline: never steals focus, never moves the operator's point in the
;; content buffer beyond the section jump they asked for by looking, and
;; reuses the existing window when one is already showing the source.
;; Enable with M-x arxana-essays-twoup-mode in the *Arxana Browser*.

;;; Code:

(require 'arxana-browser-essays)

(defface arxana-essays-twoup-section-face
  '((t :background "#2d2a4a" :extend t))
  "Tint for the section currently mirrored in the content window.")

(defvar-local arxana-essays-twoup--last-key nil)
(defvar arxana-essays-twoup--overlay nil)

(defcustom arxana-essays-twoup-surplus-buffers
  '("*Arxana Essay Compiled Notes*")
  "Buffers whose windows are surplus to the two-up layout and get closed."
  :type '(repeat string)
  :group 'arxana-browser)

(defun arxana-essays-twoup--right-window (buf)
  "Window directly RIGHT of the outline, showing BUF.
The Arxana two-up invariant (Joe, 2026-06-11): basic material on the
left — here, the outline — annotations on the right — here, the raised
section content. Surplus windows (e.g. Compiled Notes) are removed from
the frame so the pair reads as a pair."
  (let* ((outline-win (or (get-buffer-window "*Arxana Browser*" t)
                          (get-buffer-window (current-buffer) t)
                          (selected-window))))
    ;; clear surplus windows on this frame
    (dolist (w (window-list (window-frame outline-win)))
      (when (and (not (eq w outline-win))
                 (member (buffer-name (window-buffer w))
                         arxana-essays-twoup-surplus-buffers)
                 (not (eq w (frame-root-window))))
        (ignore-errors (delete-window w))))
    (let* ((right (window-in-direction 'right outline-win))
           (win (cond
                 ((and right (eq (window-buffer right) buf)) right)
                 (right (set-window-buffer right buf) right)
                 (t (let ((new (split-window outline-win nil 'right)))
                      (set-window-buffer new buf)
                      new)))))
      ;; exactly one pair: any OTHER window showing BUF is a duplicate
      (dolist (w (window-list (window-frame outline-win)))
        (when (and (eq (window-buffer w) buf)
                   (not (eq w win))
                   (not (eq w outline-win)))
          (ignore-errors (delete-window w))))
      win)))

(defun arxana-essays-twoup--row-section ()
  "Return (ESSAY-ID . SECTION-NAME) for the row at point, or nil."
  (let ((id (get-text-property (line-beginning-position) 'tabulated-list-id)))
    (when (and (listp id) (eq (plist-get id :type) 'essays-section))
      (cons (plist-get id :essay-id) (plist-get id :label)))))

(defun arxana-essays-twoup--show (essay-id section-name)
  "Display SECTION-NAME of ESSAY-ID's source in the other window."
  (let* ((cat (arxana-browser-essays--catalog-spec essay-id))
         (source (and cat (arxana-browser-essays--cat-source-file cat))))
    (when (and source (file-readable-p source))
      (let* ((buf (find-file-noselect source))
             (win (arxana-essays-twoup--right-window buf)))
        (when win
          (with-current-buffer buf
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward
                     (concat "^## +" (regexp-quote section-name) " *$") nil t)
                (let* ((start (line-beginning-position))
                       (end (save-excursion
                              (if (re-search-forward "^## " nil t)
                                  (line-beginning-position)
                                (point-max)))))
                  (set-window-point win start)
                  (with-selected-window win (recenter 1))
                  (when arxana-essays-twoup--overlay
                    (delete-overlay arxana-essays-twoup--overlay))
                  (setq arxana-essays-twoup--overlay
                        (make-overlay start end buf))
                  (overlay-put arxana-essays-twoup--overlay 'face
                               'arxana-essays-twoup-section-face))))))
        win))))

(defun arxana-essays-twoup--post-command ()
  (let ((row (arxana-essays-twoup--row-section)))
    (when (and row (not (equal row arxana-essays-twoup--last-key)))
      (setq arxana-essays-twoup--last-key row)
      (ignore-errors
        (arxana-essays-twoup--show (car row) (cdr row))))))

;;;###autoload
(define-minor-mode arxana-essays-twoup-mode
  "Mirror the essay section at point into the other window."
  :lighter " 2up"
  (if arxana-essays-twoup-mode
      (add-hook 'post-command-hook #'arxana-essays-twoup--post-command nil t)
    (remove-hook 'post-command-hook #'arxana-essays-twoup--post-command t)
    (when arxana-essays-twoup--overlay
      (delete-overlay arxana-essays-twoup--overlay)
      (setq arxana-essays-twoup--overlay nil))))

(provide 'arxana-essays-twoup)
;;; arxana-essays-twoup.el ends here
