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
             (win (or (get-buffer-window buf t)
                      (display-buffer buf '(display-buffer-use-some-window
                                            (inhibit-same-window . t))))))
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
