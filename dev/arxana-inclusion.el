;;; arxana-inclusion.el --- Guard legacy include/transclude helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Ensures include/transclude commands only run when the referenced article
;; has been registered, preventing silent “nil” insertions.

;;; Code:

(require 'subr-x)
(require 'arxana-article nil t)

(defface arxana-inclusion-marker-face
  '((t :inherit shadow))
  "Face used for inclusion/transclusion markers."
  :group 'arxana)

(defcustom arxana-inclusion-show-markers t
  "When non-nil, insert start/end markers around included text."
  :type 'boolean
  :group 'arxana)

(defcustom arxana-transclusion-default-mode 'bidi
  "Default mode for new transclusions."
  :type '(choice (const :tag "Bidirectional" bidi)
                 (const :tag "Mirror (source -> target)" mirror))
  :group 'arxana)

(defcustom arxana-transclusion-mirror-read-only t
  "When non-nil, mirror transclusions are read-only in target buffers."
  :type 'boolean
  :group 'arxana)

(defconst arxana-inclusion--wrapper-version 4
  "Version counter for inclusion wrappers.")

(defun arxana-inclusion--base-include (name)
  "Insert the plain text of article NAME without markers."
  (let ((article (and (fboundp 'get-article) (get-article name))))
    (when (and article (fboundp 'sch-plain-text))
      (insert (sch-plain-text article)))))

(defun arxana-inclusion--base-transclude (name)
  "Insert the transcluded text of article NAME without markers."
  (let* ((article (and (fboundp 'get-article) (get-article name)))
         (target (and (boundp 'name-of-current-article) name-of-current-article)))
    (when (and target (fboundp 'get-article))
      (arxana-inclusion--require-article target "transclude-article" #'get-article))
    (when (and article (fboundp 'sch-plain-text))
      (insert (sch-plain-text article)))))

(defvar arxana-inclusion--transclusion-index (make-hash-table :test 'equal)
  "Hash table mapping article names to transclusion overlays.")

(defvar-local arxana-inclusion--transclusion-overlays nil)
(defvar-local arxana-inclusion--transclusion-syncing nil)
(defvar arxana-inclusion--wrap-active nil)

(defvar arxana-inclusion--article-buffers (make-hash-table :test 'equal)
  "Hash table mapping article names to their source buffers.")

(defun arxana-inclusion--canonical-article-name (name)
  "Return the canonical article name matching NAME, if any."
  (let ((names (arxana-inclusion--article-names)))
    (or (and (member name names) name)
        (and name
             (seq-find (lambda (candidate)
                         (string= (downcase candidate) (downcase name)))
                       names)))))

(defun arxana-inclusion--find-article-buffer (name)
  "Find a live buffer whose `name-of-current-article' matches NAME."
  (seq-find
   (lambda (buf)
     (with-current-buffer buf
       (and (boundp 'name-of-current-article)
            (equal name-of-current-article name))))
   (buffer-list)))

(defun arxana-inclusion--article-names ()
  "Return the list of known article names."
  (when (fboundp 'turn-article-table-into-names)
    (turn-article-table-into-names)))

(defun arxana-inclusion--read-article (prompt)
  "Read an article name with PROMPT, offering completion when possible."
  (let ((names (arxana-inclusion--article-names)))
    (if names
        (minibuffer-with-setup-hook
            (lambda ()
              (local-set-key (kbd "TAB") #'minibuffer-complete)
              (local-set-key (kbd "C-i") #'minibuffer-complete))
          (completing-read prompt names nil t nil nil (car names)))
      (read-string prompt))))

(defun arxana-inclusion--insert-marker (label kind)
  "Insert a marker line for LABEL of KIND (include/transclude)."
  (let* ((tag (format "%s" kind))
         (text (format "[[%s: %s]]" tag label)))
    (unless (bolp)
      (insert "\n"))
    (insert (propertize text
                        'face 'arxana-inclusion-marker-face
                        'arxana-inclusion-marker t))
    (insert "\n")))

(defun arxana-inclusion--insert-end-marker (kind)
  "Insert an end marker for KIND (include/transclude)."
  (arxana-inclusion--insert-marker (format "end %s" kind) kind))

(defun arxana-inclusion--clear-transclusion-overlays ()
  "Remove transclusion overlays from the current buffer."
  (when arxana-inclusion--transclusion-overlays
    (mapc #'delete-overlay arxana-inclusion--transclusion-overlays)
    (setq arxana-inclusion--transclusion-overlays nil)))

(defun arxana-inclusion--register-transclusion (name overlay)
  "Register OVERLAY as a transclusion of NAME."
  (let ((existing (gethash name arxana-inclusion--transclusion-index)))
    (puthash name (cons overlay existing) arxana-inclusion--transclusion-index)))

(defun arxana-inclusion--prune-transclusions (name)
  "Prune dead overlays for NAME."
  (let ((existing (gethash name arxana-inclusion--transclusion-index)))
    (when existing
      (puthash name
               (cl-remove-if-not
                (lambda (ov)
                  (and (overlay-buffer ov)
                       (let ((s (overlay-start ov))
                             (e (overlay-end ov)))
                         (and s e (> e s)))))
                existing)
               arxana-inclusion--transclusion-index))))

(defun arxana-inclusion--make-transclusion-overlay (beg end name &optional mode)
  "Create a transclusion overlay for NAME spanning BEG..END."
  (let* ((use-mode (or mode arxana-transclusion-default-mode))
         (ov (make-overlay beg end)))
    (overlay-put ov 'arxana-transclusion name)
    (overlay-put ov 'arxana-transclusion-mode use-mode)
    (overlay-put ov 'help-echo
                 (format "Transclusion of %s (%s)" name use-mode))
    (when (and (eq use-mode 'mirror) arxana-transclusion-mirror-read-only)
      (overlay-put ov 'read-only t))
    (arxana-inclusion--register-transclusion name ov)
    (push ov arxana-inclusion--transclusion-overlays)
    (add-hook 'after-change-functions #'arxana-inclusion--transclusion-after-change nil t)
    ov))

(defun arxana-inclusion--transclusion-mode-from-arg (arg)
  "Return transclusion mode based on ARG and defaults."
  (if arg 'mirror arxana-transclusion-default-mode))

(defun arxana-inclusion--transclusion-at-point ()
  "Return the transclusion overlay at point, if any."
  (seq-find (lambda (ov)
              (and (overlay-get ov 'arxana-transclusion)
                   (let ((s (overlay-start ov))
                         (e (overlay-end ov)))
                     (and s e (> e s)))))
            (overlays-at (point))))

(defun arxana-inclusion--update-article-text (name text)
  "Update the article NAME with TEXT in the in-memory table."
  (when (and (fboundp 'get-article) name)
    (let ((entry (get-article name)))
      (when entry
        (puthash name (plist-put entry :text text) arxana-article--table)
        t))))

(defun arxana-inclusion--sync-source-buffer (name text)
  "Replace the contents of NAME's source buffer with TEXT."
  (let ((buf (or (gethash name arxana-inclusion--article-buffers)
                 (arxana-inclusion--find-article-buffer name))))
    (when (buffer-live-p buf)
      (puthash name buf arxana-inclusion--article-buffers)
      (with-current-buffer buf
        (add-hook 'after-change-functions #'arxana-inclusion--source-after-change nil t)
        (let ((arxana-inclusion--transclusion-syncing t)
              (inhibit-read-only t))
          (erase-buffer)
          (insert text))))))

(defun arxana-inclusion--commit-overlay (overlay &optional skip-buffer)
  "Commit OVERLAY back into its source article, skipping SKIP-BUFFER."
  (let* ((raw-name (overlay-get overlay 'arxana-transclusion))
         (name (or (arxana-inclusion--canonical-article-name raw-name)
                   raw-name))
         (text (buffer-substring-no-properties
                (overlay-start overlay) (overlay-end overlay))))
    (when (and name raw-name (not (string= name raw-name)))
      (overlay-put overlay 'arxana-transclusion name))
    (when (and name (not arxana-inclusion--transclusion-syncing))
      (let ((arxana-inclusion--transclusion-syncing t))
        (if (arxana-inclusion--update-article-text name text)
            (progn
              (arxana-inclusion--sync-source-buffer name text)
              (arxana-inclusion--refresh-transclusions name skip-buffer))
          (message "No article named '%s' for transclusion update" name))))))

(defun arxana-inclusion--refresh-transclusions (name &optional skip-buffer)
  "Refresh all transclusions for NAME, skipping SKIP-BUFFER."
  (arxana-inclusion--prune-transclusions name)
  (let* ((overlays (gethash name arxana-inclusion--transclusion-index))
         (article (and (fboundp 'get-article) (get-article name)))
         (text (and article (fboundp 'sch-plain-text) (sch-plain-text article))))
    (when text
      (dolist (ov overlays)
        (let ((buf (overlay-buffer ov)))
          (when (and (buffer-live-p buf)
                     (not (eq buf skip-buffer)))
            (with-current-buffer buf
              (let ((arxana-inclusion--transclusion-syncing t)
                    (inhibit-read-only t)
                    (start (overlay-start ov))
                    (end (overlay-end ov)))
                (when (and start end)
                  (save-excursion
                    (goto-char start)
                    (delete-region start end)
                    (let ((new-start (point)))
                      (insert text)
                      (move-overlay ov new-start (point)))))))))))))

(defun arxana-inclusion--transclusion-after-change (beg end _len)
  "Propagate edits inside transclusions."
  (unless arxana-inclusion--transclusion-syncing
    (let ((ovs (overlays-in beg end))
          (handled nil))
      (dolist (ov ovs)
        (when (and (not handled)
                   (overlay-get ov 'arxana-transclusion)
                   (not (eq (overlay-get ov 'arxana-transclusion-mode) 'mirror))
                   (let ((s (overlay-start ov))
                         (e (overlay-end ov)))
                     (and s e (> e s)
                          (or (and (>= beg s) (< beg e))
                              (and (>= end s) (<= end e))))))
          (setq handled t)
          (arxana-inclusion--commit-overlay ov (current-buffer)))))))

(defun arxana-inclusion--track-article-buffer (&optional name)
  "Track the current buffer as the source for article NAME."
  (let ((article (or name (and (boundp 'name-of-current-article)
                               name-of-current-article))))
    (when article
      (puthash article (current-buffer) arxana-inclusion--article-buffers)
      (add-hook 'after-change-functions #'arxana-inclusion--source-after-change nil t))))

(defun arxana-inclusion--source-after-change (_beg _end _len)
  "Propagate source article edits to transclusions."
  (when (and (boundp 'name-of-current-article) name-of-current-article)
    (unless arxana-inclusion--transclusion-syncing
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (let ((arxana-inclusion--transclusion-syncing t))
          (arxana-inclusion--update-article-text name-of-current-article text)
          (arxana-inclusion--refresh-transclusions name-of-current-article
                                                   (current-buffer)))))))

(defun arxana-transclusion-refresh-at-point ()
  "Refresh the transclusion at point from its source article."
  (interactive)
  (let* ((ov (arxana-inclusion--transclusion-at-point))
         (name (and ov (overlay-get ov 'arxana-transclusion))))
    (unless (and ov name)
      (user-error "No transclusion at point"))
    (let* ((article (and (fboundp 'get-article) (get-article name)))
           (text (and article (fboundp 'sch-plain-text)
                      (sch-plain-text article))))
      (unless text
        (user-error "No text for article '%s'" name))
      (let ((inhibit-read-only t)
            (start (overlay-start ov))
            (end (overlay-end ov)))
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (let ((new-start (point)))
            (insert text)
            (move-overlay ov new-start (point)))))
      (message "Transclusion refreshed from %s" name))))

(defun arxana-transclusion-refresh-buffer ()
  "Refresh all transclusions in the current buffer."
  (interactive)
  (let ((count 0))
    (dolist (ov (copy-sequence arxana-inclusion--transclusion-overlays))
      (when (overlay-buffer ov)
        (goto-char (overlay-start ov))
        (ignore-errors
          (arxana-transclusion-refresh-at-point)
          (setq count (1+ count)))))
    (message "Refreshed %d transclusion(s)" count)))

(defun arxana-transclusion-commit-at-point ()
  "Commit the transclusion at point back into its source article."
  (interactive)
  (let* ((ov (arxana-inclusion--transclusion-at-point))
         (name (and ov (overlay-get ov 'arxana-transclusion)))
         (mode (and ov (overlay-get ov 'arxana-transclusion-mode))))
    (unless (and ov name)
      (user-error "No transclusion at point"))
    (when (eq mode 'mirror)
      (user-error "Mirror transclusion is read-only; use a bidi transclusion to commit edits"))
    (let ((text (buffer-substring-no-properties
                 (overlay-start ov) (overlay-end ov))))
      (unless (arxana-inclusion--update-article-text name text)
        (user-error "No article named '%s'" name))
      (message "Transclusion committed to %s" name))))

(defun arxana-inclusion--kind-for-caller (caller)
  "Return the marker kind for CALLER."
  (pcase caller
    ("include-article" "include")
    ("transclude-article" "transclude")
    (_ caller)))

(defun arxana-inclusion--require-article (name caller lookup)
  "Return the article NAME using LOOKUP or raise a helpful CALLER error."
  (when (fboundp 'arxana-data-constraints-validate-inclusion)
    (arxana-data-constraints-validate-inclusion name caller))
  (let ((article (and name (funcall lookup name))))
    (unless article
      (user-error "%s: unknown article '%s'. Run `make-current-buffer-into-article' in that buffer first."
                 caller name))
    article))

(defun arxana-inclusion--wrap (orig caller)
  "Return a wrapper for ORIG that checks existence before inserting."
  (lambda (name &optional mode-arg &rest _args)
    (interactive (list (arxana-inclusion--read-article
                        (format "%s article: "
                                (capitalize (arxana-inclusion--kind-for-caller caller))))
                       current-prefix-arg))
    (let ((resolved (or (arxana-inclusion--canonical-article-name name) name)))
      (if arxana-inclusion--wrap-active
          (let ((arxana-inclusion-show-markers nil))
            (apply orig (list resolved)))
        (let ((arxana-inclusion--wrap-active t))
          (when (fboundp 'get-article)
            (arxana-inclusion--require-article resolved caller #'get-article))
          (let* ((kind (arxana-inclusion--kind-for-caller caller))
                 (mode (arxana-inclusion--transclusion-mode-from-arg mode-arg))
                 (start nil)
                 (end nil))
            (when arxana-inclusion-show-markers
              (arxana-inclusion--insert-marker resolved kind))
            (setq start (point))
            (let ((arxana-inclusion-show-markers nil))
              (apply orig (list resolved)))
            (setq end (point))
            (when (and (string= kind "transclude")
                       (> end start))
              (arxana-inclusion--make-transclusion-overlay start end resolved mode))
            (when arxana-inclusion-show-markers
              (arxana-inclusion--insert-end-marker kind))))))))

(defun arxana-inclusion--wrap-command (symbol caller)
  "Wrap command SYMBOL with inclusion guards and completion."
  (let ((version (get symbol 'arxana-inclusion-wrapper-version)))
    (when (or (not version)
              (< version arxana-inclusion--wrapper-version))
      (when (fboundp symbol)
        (put symbol 'arxana-inclusion-wrapped nil)
        (put symbol 'arxana-inclusion--wrapped-orig nil)
        (put symbol 'arxana-inclusion--wrapped-fn nil)
        (let* ((base (cond
                      ((eq symbol 'include-article)
                       #'arxana-inclusion--base-include)
                      ((eq symbol 'transclude-article)
                       #'arxana-inclusion--base-transclude)
                      (t
                       (or (get symbol 'arxana-inclusion--base-orig)
                           (symbol-function symbol)))))
               (wrapped (arxana-inclusion--wrap base caller)))
          (put symbol 'arxana-inclusion--base-orig base)
          (put symbol 'arxana-inclusion--wrapped-fn wrapped)
          (fset symbol wrapped)
          (put symbol 'arxana-inclusion-wrapper-version
               arxana-inclusion--wrapper-version))))))

(unless (fboundp 'include-article)
  (defun include-article (name)
    "Insert the plain text of article NAME into the current buffer."
    (interactive (list (arxana-inclusion--read-article "Include article: ")))
    (let ((article (arxana-inclusion--require-article name "include-article" #'get-article)))
      (when (fboundp 'sch-plain-text)
        (insert (sch-plain-text article)))
      article)))

(unless (fboundp 'transclude-article)
  (defun transclude-article (name)
    "Insert the transcluded text of article NAME into the current buffer."
    (interactive (list (arxana-inclusion--read-article "Transclude article: ")))
    (let* ((article (arxana-inclusion--require-article name "transclude-article" #'get-article))
           (target (and (boundp 'name-of-current-article) name-of-current-article)))
      (when target
        (arxana-inclusion--require-article target "transclude-article" #'get-article))
      (when (fboundp 'sch-plain-text)
        (insert (sch-plain-text article)))
      article)))

(arxana-inclusion--wrap-command 'include-article "include-article")
(arxana-inclusion--wrap-command 'transclude-article "transclude-article")

(when (fboundp 'make-current-buffer-into-article)
  (advice-add 'make-current-buffer-into-article :after
              (lambda (name &rest _)
                (arxana-inclusion--track-article-buffer name))))

(with-eval-after-load 'arxana-tangled
  (arxana-inclusion--wrap-command 'include-article "include-article")
  (arxana-inclusion--wrap-command 'transclude-article "transclude-article"))

(provide 'arxana-inclusion)

;;; arxana-inclusion.el ends here
