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
        ;; The layout invariant is CHECKED, not just enforced — the Reazon
        ;; window-constraints discipline, restored to the Essays build.
        (when (fboundp 'arxana-window-constraints-validate-essays-two-up)
          (arxana-window-constraints-validate-essays-two-up
           (get-buffer "*Arxana Browser*") buf
           (and win (window-frame win))))
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

;;;; RET descends: focus the item, re-form the pair one level down --------

;; Joe, spoken 2026-06-11: RET always FOCUSES the item under the cursor —
;; a new two-up of [the section (basic material, LEFT) | its annotations
;; (RIGHT)]. Cursor-over already auto-raises, so RET need not raise; and
;; the old three-pane reading view (delete-other-windows + a Compiled
;; Notes side window) breaks the pair, so in twoup-mode RET never goes
;; there.

(defvar arxana-essays-twoup--level1 nil
  "Saved (BROWSER-BUF . SOURCE-BUF) for ascending back to level 1.")

(defun arxana-essays-twoup--section-span (source-buf section-name)
  "Return (START . END) of SECTION-NAME's ## span in SOURCE-BUF."
  (with-current-buffer source-buf
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (concat "^## +" (regexp-quote section-name) " *$") nil t)
        (cons (line-beginning-position)
              (save-excursion
                (if (re-search-forward "^## " nil t)
                    (line-beginning-position)
                  (point-max))))))))

(defun arxana-essays-twoup-descend ()
  "Focus the section at point: section content LEFT, its annotations RIGHT."
  (interactive)
  (let ((row (arxana-essays-twoup--row-section)))
    (if (null row)
        ;; not a section row — defer to the browser's ordinary visit
        (call-interactively #'arxana-browser--visit)
      (let* ((essay-id (car row))
             (section-name (cdr row))
             (browser-buf (current-buffer))
             (cat (arxana-browser-essays--catalog-spec essay-id))
             (source (and cat (arxana-browser-essays--cat-source-file cat)))
             (source-buf (and source (find-file-noselect source)))
             (span (and source-buf
                        (arxana-essays-twoup--section-span source-buf section-name))))
        (if (null span)
            (user-error "Cannot locate section %s in source" section-name)
          (setq arxana-essays-twoup--level1 (cons browser-buf source-buf))
          (let* ((sec-buf-name (format "§ %s — %s" section-name
                                       (file-name-nondirectory source)))
                 (sec-buf (or (get-buffer sec-buf-name)
                              (with-current-buffer source-buf
                                (let ((ib (make-indirect-buffer
                                           source-buf sec-buf-name t)))
                                  ib)))))
            (with-current-buffer sec-buf
              (narrow-to-region (car span) (cdr span))
              (local-set-key (kbd "q") #'arxana-essays-twoup-ascend)
              (local-set-key (kbd "b") #'arxana-essays-twoup-ascend))
            ;; drill the browser into the annotations view for this section
            (with-current-buffer browser-buf
              (arxana-browser--visit))
            ;; geometry: section LEFT, annotations (browser) RIGHT
            (let* ((left-win (or (get-buffer-window browser-buf t)
                                 (selected-window)))
                   (frame (window-frame left-win))
                   (right-win (or (window-in-direction 'right left-win)
                                  (split-window left-win nil 'right))))
              (set-window-buffer left-win sec-buf)
              (set-window-buffer right-win browser-buf)
              ;; surplus discipline at level 2 as well
              (dolist (w (window-list frame))
                (when (and (not (memq w (list left-win right-win)))
                           (member (buffer-name (window-buffer w))
                                   arxana-essays-twoup-surplus-buffers))
                  (ignore-errors (delete-window w))))
              (select-window left-win)
              (when (fboundp 'arxana-window-constraints-validate-essays-two-up)
                (arxana-window-constraints-validate-essays-two-up
                 sec-buf browser-buf frame)))))))))

(defun arxana-essays-twoup-ascend ()
  "Return to level 1: outline LEFT, raised source RIGHT."
  (interactive)
  (when arxana-essays-twoup--level1
    (let* ((browser-buf (car arxana-essays-twoup--level1))
           (source-buf (cdr arxana-essays-twoup--level1))
           (left-win (selected-window))
           (frame (window-frame left-win))
           (right-win (or (window-in-direction 'right left-win)
                          (window-in-direction 'left left-win))))
      ;; whichever window the section view holds becomes the outline again
      (with-current-buffer browser-buf
        (when (fboundp 'arxana-browser--back) (ignore-errors (arxana-browser--back))))
      (let* ((wins (sort (window-list frame 'nomini)
                         (lambda (a b) (< (nth 0 (window-edges a))
                                          (nth 0 (window-edges b))))))
             (lw (car wins))
             (rw (cadr wins)))
        (when (and lw rw)
          (set-window-buffer lw browser-buf)
          (set-window-buffer rw source-buf)
          (select-window lw))))))

(defvar arxana-essays-twoup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'arxana-essays-twoup-descend)
    map)
  "Keymap active in `arxana-essays-twoup-mode' (RET descends).")

(unless (assq 'arxana-essays-twoup-mode minor-mode-map-alist)
  (push (cons 'arxana-essays-twoup-mode arxana-essays-twoup-mode-map)
        minor-mode-map-alist))
