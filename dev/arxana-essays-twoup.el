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
  '("*Arxana Essay Compiled Notes*"
    ;; The legacy three-pane reading view: a stale *Arxana Essay* render
    ;; also SHADOWS the outline's annotation counts (open-section-buffer-state
    ;; reads its overlays in place of the manifest), and saving it would
    ;; retract whatever its overlays lost track of. In twoup-mode both are
    ;; surplus; Joe flagged the stale Notes window live 2026-06-11.
    "*Arxana Essay*"
    "*Arxana Essay Notes*")
  "Buffers whose windows are surplus to the two-up layout and get closed."
  :type '(repeat string)
  :group 'arxana-browser)

(defun arxana-essays-twoup--solo (buf keep-win)
  "Enforce the Arxana solo invariant for BUF: KEEP-WIN is its ONE window.
Every other window showing BUF — on any frame — is deleted; when such a
window is its frame's only window, it is switched to another buffer
instead (never delete a frame). Joe, 2026-06-11: \"there should be
exactly one of each Arxana-associated buffer at a time.\""
  (when (buffer-live-p buf)
    (dolist (w (get-buffer-window-list buf 'nomini t))
      (unless (eq w keep-win)
        (if (eq w (frame-root-window (window-frame w)))
            (set-window-buffer w (other-buffer buf (window-frame w)))
          (ignore-errors (delete-window w)))))))

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
      ;; solo invariant, frame-global: one window for the source, one for
      ;; the outline
      (arxana-essays-twoup--solo buf win)
      (arxana-essays-twoup--solo (window-buffer outline-win) outline-win)
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
        (with-current-buffer buf
          (arxana-essays-twoup-content-mode 1))
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

;; `arxana-browser--render' resets the major mode on every navigation,
;; which kills buffer-local minor modes — the cursor-raise silently died
;; after the first RET (caught live by Joe, 2026-06-11). Mark the mode
;; variable and its hook permanent-local so the two-up survives renders.
(put 'arxana-essays-twoup-mode 'permanent-local t)
(put 'arxana-essays-twoup--post-command 'permanent-local-hook t)

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
             (item (get-text-property (line-beginning-position)
                                      'tabulated-list-id))
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
              (arxana-essays-twoup-content-mode 1)
              (local-set-key (kbd "q") #'arxana-essays-twoup-ascend)
              (local-set-key (kbd "b") #'arxana-essays-twoup-ascend))
            ;; Drill the browser into the annotations view for this section
            ;; by pushing the context DIRECTLY (the item plist already is
            ;; one: :view essays-section + :essay-id + :section-id).
            ;; `arxana-browser--visit' must NOT be used here: for
            ;; essays-section rows it dispatches to the three-pane reading
            ;; view (core.el:1993), whose delete-other-windows + side
            ;; window destroys the pair before we can arrange it.
            (with-current-buffer browser-buf
              (setq arxana-browser--stack (cons item arxana-browser--stack))
              (arxana-browser--render))
            ;; geometry: section LEFT, annotations (browser) RIGHT
            (let* ((left-win (or (get-buffer-window browser-buf t)
                                 (selected-window)))
                   (frame (window-frame left-win))
                   (right-win (or (window-in-direction 'right left-win)
                                  (split-window left-win nil 'right))))
              (set-window-buffer left-win sec-buf)
              (set-window-buffer right-win browser-buf)
              ;; Solo invariant, frame-global (idempotent under repeat
              ;; RET): each pair buffer in exactly one window; surplus
              ;; buffers evicted from this frame.
              (arxana-essays-twoup--solo sec-buf left-win)
              (arxana-essays-twoup--solo browser-buf right-win)
              (dolist (w (window-list frame))
                (when (and (not (memq w (list left-win right-win)))
                           (member (buffer-name (window-buffer w))
                                   arxana-essays-twoup-surplus-buffers))
                  (ignore-errors (delete-window w))))
              (select-window left-win)
              (when (fboundp 'arxana-window-constraints-validate-essays-two-up)
                (arxana-window-constraints-validate-essays-two-up
                 sec-buf browser-buf frame)))))))))

(defvar arxana-essays-twoup--in-ascend nil
  "Non-nil while ascend itself calls `arxana-browser--up' (advice guard).")

(defun arxana-essays-twoup-ascend ()
  "Go BACK to the previous two-up display: outline LEFT, raised source RIGHT.
The Arxana back invariant (Joe, 2026-06-11, oft-repeated): back means
the previous two-up DISPLAY — the pair re-forms — never a bare view pop
that strands the window layout."
  (interactive)
  (when arxana-essays-twoup--level1
    (let* ((browser-buf (car arxana-essays-twoup--level1))
           (source-buf (cdr arxana-essays-twoup--level1))
           (frame (window-frame (selected-window))))
      ;; pop the browser only when it is genuinely one level down;
      ;; a stray ascend must not pop past the outline
      (with-current-buffer browser-buf
        (when (and (fboundp 'arxana-browser--up)
                   (eq (plist-get (car arxana-browser--stack) :view)
                       'essays-section))
          (ignore-errors
            (let ((arxana-essays-twoup--in-ascend t))
              (arxana-browser--up)))))
      (let* ((wins (sort (window-list frame 'nomini)
                         (lambda (a b) (< (nth 0 (window-edges a))
                                          (nth 0 (window-edges b))))))
             (lw (car wins))
             (rw (cadr wins)))
        (when (and lw rw)
          (set-window-buffer lw browser-buf)
          (set-window-buffer rw source-buf)
          (arxana-essays-twoup--solo browser-buf lw)
          (arxana-essays-twoup--solo source-buf rw)
          (select-window lw)
          (when (fboundp 'arxana-window-constraints-validate-essays-two-up)
            (arxana-window-constraints-validate-essays-two-up
             browser-buf source-buf frame)))))))

(defun arxana-essays-twoup--up-restores-pair (orig &rest args)
  "Around `arxana-browser--up': back from the annotations view re-forms
the previous two-up display instead of popping the view in place."
  (if (and (not arxana-essays-twoup--in-ascend)
           (bound-and-true-p arxana-essays-twoup-mode)
           (eq (plist-get (car arxana-browser--stack) :view) 'essays-section)
           arxana-essays-twoup--level1)
      (arxana-essays-twoup-ascend)
    (apply orig args)))

(advice-add 'arxana-browser--up :around #'arxana-essays-twoup--up-restores-pair)

(defun arxana-essays-twoup-left-or-back ()
  "Move left; at `point-min', go BACK to the previous two-up display.
In a content buffer the left arrow keeps its cursor meaning — except at
point-min, where no leftward motion exists and <left> means back."
  (interactive)
  (if (bobp)
      (arxana-essays-twoup-ascend)
    (left-char 1)))

(defvar arxana-essays-twoup-content-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<left>") #'arxana-essays-twoup-left-or-back)
    map)
  "Keymap for content buffers raised into a two-up pair.")

(define-minor-mode arxana-essays-twoup-content-mode
  "Back-navigation bindings for a two-up content buffer."
  :lighter ""
  :keymap arxana-essays-twoup-content-mode-map)

(defvar arxana-essays-twoup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'arxana-essays-twoup-descend)
    map)
  "Keymap active in `arxana-essays-twoup-mode' (RET descends).")

(unless (assq 'arxana-essays-twoup-mode minor-mode-map-alist)
  (push (cons 'arxana-essays-twoup-mode arxana-essays-twoup-mode-map)
        minor-mode-map-alist))
