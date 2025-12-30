;;; arxana-flexiarg-collection.el --- Flexiarg collection editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers for editing flexiarg collections and syncing changes to XTDB.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'outline)

(require 'arxana-store)
(require 'arxana-patterns-ingest)

(defvar arxana-flexiarg-collection-mode-map nil
  "Keymap for `arxana-flexiarg-collection-mode'.")

(defcustom arxana-flexiarg-sync-on-save t
  "When non-nil, saving a flexiarg collection syncs to XTDB if enabled."
  :type 'boolean
  :group 'arxana-patterns)

(defun arxana-flexiarg--file-mtime (file)
  (when (and file (file-exists-p file))
    (file-attribute-modification-time (file-attributes file))))

(defun arxana-flexiarg--read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun arxana-flexiarg--find-contrib-path ()
  "Return a plausible futon3 contrib path containing flexiarg.el, or nil."
  (let* ((root (if (boundp 'arxana-root-directory)
                   arxana-root-directory
                 (file-name-directory (or load-file-name buffer-file-name default-directory))))
         (candidate (expand-file-name "../futon3/contrib" root)))
    (when (file-directory-p candidate)
      candidate)))

(defun arxana-flexiarg--ensure-parent-map ()
  (when (and (boundp 'arxana-flexiarg-collection-mode-map)
             (keymapp arxana-flexiarg-collection-mode-map)
             (boundp 'flexiarg-mode-map)
             (keymapp flexiarg-mode-map))
    (set-keymap-parent arxana-flexiarg-collection-mode-map flexiarg-mode-map)))

(defun arxana-flexiarg--ensure-flexiarg ()
  "Ensure flexiarg is available and wired into the collection mode."
  (unless (featurep 'flexiarg)
    (let ((loaded (require 'flexiarg nil t)))
      (unless loaded
        (let ((candidate (arxana-flexiarg--find-contrib-path)))
          (when (and candidate (not (member candidate load-path)))
            (add-to-list 'load-path candidate)
            (setq loaded (require 'flexiarg nil t)))))
      (unless loaded
        (user-error "flexiarg.el not found; add futon3/contrib to load-path"))))
  (arxana-flexiarg--ensure-parent-map))

(with-eval-after-load 'flexiarg
  (arxana-flexiarg--ensure-parent-map))

(defvar-local arxana-flexiarg--segments nil
  "List of tracked flexiarg segments in the current buffer.")

(defun arxana-flexiarg--collect-segments-from-buffer ()
  "Rebuild segment metadata from existing `arxana-source-file` text properties."
  (let ((pos (point-min))
        (segments nil))
    (while (< pos (point-max))
      (let ((file (get-text-property pos 'arxana-source-file)))
        (if (stringp file)
            (let ((end (or (next-single-property-change pos 'arxana-source-file nil (point-max))
                           (point-max))))
              (push (arxana-flexiarg--segment pos end file) segments)
              (setq pos end))
          (setq pos (or (next-single-property-change pos 'arxana-source-file nil (point-max))
                        (point-max))))))
    (nreverse segments)))

(defvar-local arxana-flexiarg--metadata-overlays nil
  "Overlays covering metadata lines so they can be hidden or styled.")
(defvar-local arxana-flexiarg--context-overlays nil
  "Overlays showing placeholders for hidden context/IF blocks.")

(defconst arxana-flexiarg--metadata-placeholder
  (propertize "@ metadata (hidden)" 'face '(:inherit shadow :underline t)))
(defconst arxana-flexiarg--context-placeholder-face
  '(:inherit shadow :slant italic))
(defconst arxana-flexiarg--hidden-token 'arxana-flexiarg-hidden)
(defconst arxana-flexiarg--clause-placeholder
  (propertize "+ clause (hidden)" 'face '(:inherit shadow :slant italic)))
(defconst arxana-flexiarg--header-clean-face
  '(:inherit mode-line))
(defconst arxana-flexiarg--header-dirty-face
  '(:inherit mode-line :foreground "orange"))
(defconst arxana-flexiarg--header-error-face
  '(:inherit mode-line :foreground "tomato"))

(defun arxana-flexiarg--hide-range (start end)
  "Mark text between START and END as hidden using the flexiarg token."
  (when (and start end (< start end))
    (let ((inhibit-modification-hooks t))
      (add-text-properties start end `(invisible ,arxana-flexiarg--hidden-token)))))

(defun arxana-flexiarg--show-range (start end)
  "Remove the flexiarg hidden token between START and END."
  (when (and start end (< start end))
    (let ((pos start)
          (inhibit-modification-hooks t))
      (while (< pos end)
        (let* ((next (next-single-property-change pos 'invisible nil end))
               (val (get-text-property pos 'invisible)))
          (when (eq val arxana-flexiarg--hidden-token)
            (remove-text-properties pos (or next end) '(invisible nil)))
          (setq pos (or next end)))))))

(defun arxana-flexiarg--metadata--prevent-edit (_ov after _beg _end &optional _len)
  (unless (or after
              arxana-flexiarg--metadata-visible
              arxana-flexiarg--inhibit-guards)
    (user-error "Metadata blocks are hidden; use C-c C-m to show them")))

(defun arxana-flexiarg--context--prevent-edit (_ov after _beg _end &optional _len)
  (unless (or after arxana-flexiarg--inhibit-guards)
    (user-error "Hidden context blocks cannot be edited; use C-c C-o")))

(defun arxana-flexiarg--metadata-overlay-set-hidden (ov hidden)
  (let ((start (overlay-start ov))
        (end (overlay-end ov)))
    (when (and start end)
      (if hidden
          (progn
            (arxana-flexiarg--hide-range start end)
            (overlay-put ov 'before-string (concat arxana-flexiarg--metadata-placeholder "\n"))
            (overlay-put ov 'modification-hooks '(arxana-flexiarg--metadata--prevent-edit)))
        (arxana-flexiarg--show-range start end)
        (overlay-put ov 'before-string nil)
        (overlay-put ov 'modification-hooks nil)))))

(defun arxana-flexiarg--metadata-set-visibility (visible)
  (dolist (ov arxana-flexiarg--metadata-overlays)
    (arxana-flexiarg--metadata-overlay-set-hidden ov (not visible))))

(defun arxana-flexiarg--clear-metadata-overlays ()
  (when arxana-flexiarg--metadata-overlays
    (dolist (ov arxana-flexiarg--metadata-overlays)
      (arxana-flexiarg--show-range (overlay-start ov) (overlay-end ov))
      (delete-overlay ov))
    (setq arxana-flexiarg--metadata-overlays nil)))

(defun arxana-flexiarg--clear-context-overlays ()
  (when arxana-flexiarg--context-overlays
    (dolist (ov arxana-flexiarg--context-overlays)
      (arxana-flexiarg--show-range (overlay-start ov) (overlay-end ov))
      (delete-overlay ov))
    (setq arxana-flexiarg--context-overlays nil)))

(defvar arxana-flexiarg--metadata-visible nil)
(defvar-local arxana-flexiarg--fold-restore-metadata nil
  "When non-nil, restore metadata visibility after leaving a folded view.")

(defun arxana-flexiarg-toggle-metadata (&optional state)
  "Toggle visibility of @metadata sections at the top of each flexiarg."
  (interactive)
  (setq arxana-flexiarg--metadata-visible
        (if (null state) (not arxana-flexiarg--metadata-visible) state))
  (arxana-flexiarg--metadata-set-visibility arxana-flexiarg--metadata-visible)
  (message (if arxana-flexiarg--metadata-visible
               "Flexiarg metadata is now visible"
             "Flexiarg metadata is hidden")))

(defun arxana-flexiarg--annotate-metadata (start end)
  "Create overlays marking metadata lines between START and END."
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (looking-at "^;;;"))
      (forward-line 1))
    (while (and (< (point) end)
                (looking-at "^\\s-*$"))
      (forward-line 1))
    (while (and (< (point) end)
                (looking-at "^@[^\n]*"))
      (let ((block-start (point)))
        (while (and (< (point) end)
                    (looking-at "^@[^\n]*"))
          (forward-line 1))
        (let ((block-end (point))
              (ov (make-overlay block-start (point))))
          (overlay-put ov 'priority 1000)
          (arxana-flexiarg--metadata-overlay-set-hidden ov t)
          (push ov arxana-flexiarg--metadata-overlays))))
    (when (and (< (point) end)
               (looking-at "^\\s-*$"))
      (forward-line 1))))

(defun arxana-flexiarg--segment (start end file)
  (list :start start
        :end end
        :file file
        :mtime (arxana-flexiarg--file-mtime file)
        :dirty nil))

(defun arxana-flexiarg--segment-bounds (segment)
  "Return START . END for SEGMENT, recalculating via text properties if needed."
  (let* ((file (plist-get segment :file))
         (start (plist-get segment :start))
         (end (plist-get segment :end)))
    (when (or (null start)
              (null end)
              (< start (point-min))
              (< end (point-min))
              (> start (point-max))
              (> end (point-max))
              (not (equal (get-text-property start 'arxana-source-file) file))
              (not (equal (get-text-property (max (1- end) start) 'arxana-source-file) file)))
      (let ((pos (text-property-any (point-min) (point-max) 'arxana-source-file file)))
        (when pos
          (setq start pos
                end (or (next-single-property-change pos 'arxana-source-file nil (point-max))
                        (point-max)))
          (plist-put segment :start start)
          (plist-put segment :end end))))
    (when (and start end (<= start end))
      (cons start end))))

(defun arxana-flexiarg--prepare-buffer (files)
  "Insert FILES into the current buffer and mark their regions."
  (setq arxana-flexiarg--segments nil)
  (arxana-flexiarg--clear-metadata-overlays)
  (arxana-flexiarg--clear-context-overlays)
  (let ((inhibit-read-only t)
        (arxana-flexiarg--inhibit-guards t))
    (erase-buffer)
    (setq buffer-file-name nil)
    (dolist (file files)
      (let ((header-start (point)))
        (insert (format ";;; %s\n\n" file))
        (add-text-properties header-start (point)
                             (list 'face '(:inherit font-lock-comment-face
                                                    :weight bold)
                                   'arxana-source-file file))
        (let ((body-start (point)))
          (insert (arxana-flexiarg--read-file file))
          (unless (bolp)
            (insert "\n"))
          (let ((body-end (point)))
            (add-text-properties body-start body-end
                                 (list 'arxana-source-file file
                                       'arxana-source-start 0))
            (add-face-text-property body-start body-end
                                    'arxana-flexiarg-region
                                    t)
            (arxana-flexiarg--annotate-metadata body-start body-end)
            (push (arxana-flexiarg--segment body-start body-end file)
                  arxana-flexiarg--segments))))
        (insert "\n\n")))
  (setq arxana-flexiarg--segments (nreverse arxana-flexiarg--segments))
  (setq arxana-flexiarg--metadata-visible nil)
  (arxana-flexiarg--metadata-set-visibility nil))

(defvar-local arxana-flexiarg--inhibit-guards nil
  "When non-nil, skip edit guard checks inside flexiarg collection buffers.")

(defun arxana-flexiarg--segment-for-region (start end)
  (let ((file-start (get-text-property start 'arxana-source-file))
        (file-end (get-text-property (max (1- end) start) 'arxana-source-file)))
    (when (and (stringp file-start)
               (stringp file-end)
               (string= file-start file-end))
      file-start)))

(defun arxana-flexiarg--file-at-change (beg end)
  (or (get-text-property beg 'arxana-source-file)
      (get-text-property (max (1- end) beg) 'arxana-source-file)
      (let ((prev (previous-single-property-change beg 'arxana-source-file nil (point-min))))
        (and prev (get-text-property (max (1- prev) (point-min))
                                     'arxana-source-file)))
      (let ((next (next-single-property-change end 'arxana-source-file nil (point-max))))
        (and next (get-text-property next 'arxana-source-file)))))

(defun arxana-flexiarg--before-change (beg end)
  "Disallow edits spanning multiple source files."
  (unless arxana-flexiarg--inhibit-guards
    (let ((file (or (arxana-flexiarg--segment-for-region beg end)
                    (arxana-flexiarg--file-at-change beg end))))
      (unless file
        (user-error "Edits must stay within a single source flexiarg file")))))

(defun arxana-flexiarg--after-change (beg end length)
  (let* ((file (or (get-text-property beg 'arxana-source-file)
                   (get-text-property (max (1- end) beg) 'arxana-source-file)
                   (let ((prev (previous-single-property-change beg 'arxana-source-file nil (point-min))))
                     (and prev (get-text-property (max (1- prev) (point-min))
                                                  'arxana-source-file)))
                   (let ((next (next-single-property-change end 'arxana-source-file nil (point-max))))
                     (and next (get-text-property next 'arxana-source-file)))))
         (delta (- (- end beg) length)))
    (when file
      (dolist (seg arxana-flexiarg--segments)
        (when (string= (plist-get seg :file) file)
          (plist-put seg :dirty t))))
    (unless arxana-flexiarg--inhibit-guards
      (when file
        (save-excursion
          (dolist (seg arxana-flexiarg--segments)
            (when (string= (plist-get seg :file) file)
              (when (/= delta 0)
                (let ((seg-start (plist-get seg :start))
                      (seg-end (plist-get seg :end)))
                  (cond
                   ((<= beg seg-start)
                    (plist-put seg :start (+ seg-start delta))
                    (plist-put seg :end (+ seg-end delta)))
                   ((and (> beg seg-start) (< beg seg-end))
                    (plist-put seg :end (+ seg-end delta)))))))))))))

(defun arxana-flexiarg--sync-files (files)
  "Sync FILES to XTDB using the ingest pipeline when enabled."
  (when (and arxana-flexiarg-sync-on-save files)
    (let ((targets (delete-dups (delq nil files)))
          (skip nil))
      (if (not (arxana-store-sync-enabled-p))
          (progn
            (setq-local arxana-flexiarg--last-sync-errors nil)
            (when (derived-mode-p 'arxana-flexiarg-collection-mode)
              (arxana-flexiarg--update-header-state))
            (when (bound-and-true-p arxana-flexiarg-file-mode)
              (arxana-flexiarg--update-file-header-state))
            (message "XTDB sync skipped (sync disabled)."))
        (let ((synced 0)
              (failed nil))
          (setq-local arxana-flexiarg--last-sync-errors nil)
          (unless (arxana-store-tail 1)
            (setq-local arxana-flexiarg--last-sync-errors
                        (mapcar (lambda (file)
                                  (cons file "Futon API unreachable; check futon4-base-url."))
                                targets))
            (when (derived-mode-p 'arxana-flexiarg-collection-mode)
              (arxana-flexiarg--update-header-state))
            (when (bound-and-true-p arxana-flexiarg-file-mode)
              (arxana-flexiarg--update-file-header-state))
            (message "XTDB sync skipped (server unreachable).")
            (setq skip t))
          (unless skip
            (setq-local arxana-flexiarg--last-sync-errors nil)
            (dolist (file targets)
              (condition-case err
                  (progn
                    (arxana-patterns-ingest--ingest-file file)
                    (cl-incf synced))
                (error
                 (push (cons file (error-message-string err)) failed))))
            (setq-local arxana-flexiarg--last-sync-errors (nreverse failed))
            (when (derived-mode-p 'arxana-flexiarg-collection-mode)
              (arxana-flexiarg--update-header-state))
            (when (bound-and-true-p arxana-flexiarg-file-mode)
              (arxana-flexiarg--update-file-header-state))
            (when (or (> synced 0) failed)
              (message "XTDB sync: %d ok, %d failed" synced (length failed)))
            (when failed
              (message "XTDB sync failures: %s"
                       (string-join (mapcar #'car failed) ", ")))))))))

(defun arxana-flexiarg--save ()
  "Write the current flexiarg buffer back to its source files."
  (interactive)
  (unless arxana-flexiarg--segments
    (user-error "This buffer is not tracking any flexiarg files"))
  (let ((written 0)
        (written-files nil)
        (stale nil))
    (dolist (segment arxana-flexiarg--segments)
      (when (plist-get segment :dirty)
        (let* ((file (plist-get segment :file))
               (saved (plist-get segment :mtime))
               (current (arxana-flexiarg--file-mtime file)))
          (when (and saved current (time-less-p saved current))
            (push file stale)))))
    (when (and stale
               (not (yes-or-no-p (format "Overwrite %d file(s) changed on disk? "
                                         (length (delete-dups stale))))))
      (user-error "Save cancelled"))
    (dolist (segment arxana-flexiarg--segments)
      (when (plist-get segment :dirty)
        (let* ((file (plist-get segment :file))
               (bounds (arxana-flexiarg--segment-bounds segment)))
          (unless bounds
            (user-error "Could not locate buffer region for %s" file))
          (let* ((start (car bounds))
                 (end (cdr bounds))
                 (text (buffer-substring-no-properties start end)))
            (with-temp-file file
              (insert text))
            (plist-put segment :mtime (arxana-flexiarg--file-mtime file))
            (plist-put segment :dirty nil)
            (push file written-files)
            (cl-incf written)))))
    (set-buffer-modified-p nil)
    (arxana-flexiarg--update-header-state)
    (arxana-flexiarg--sync-files written-files)
    (arxana-flexiarg--update-header-state)
    (force-mode-line-update)
    (setq written-files (nreverse written-files))
    (if (> written 0)
        (message "Saved flexiarg files: %s"
                 (string-join (mapcar #'file-name-nondirectory written-files) ", "))
      (message "No flexiarg changes to save"))))

(defun arxana-flexiarg--revert ()
  "Revert the flexiarg collection buffer to the last saved state."
  (interactive)
  (unless arxana-flexiarg--segments
    (user-error "This buffer is not tracking any flexiarg files"))
  (when (yes-or-no-p "Discard all edits and reload from disk? ")
    (let ((files (mapcar (lambda (seg) (plist-get seg :file))
                         arxana-flexiarg--segments)))
      (arxana-flexiarg--prepare-buffer files)
      (goto-char (point-min))
      (message "Reloaded %d flexiarg files" (length files)))))

(defun arxana-flexiarg-reconnect-collection ()
  "Reconnect the current buffer to its flexiarg source file segments."
  (interactive)
  (unless (derived-mode-p 'arxana-flexiarg-collection-mode)
    (user-error "Only available in flexiarg collection buffers"))
  (let ((segments (arxana-flexiarg--collect-segments-from-buffer)))
    (unless segments
      (user-error "No flexiarg segments detected in the current buffer"))
    (setq arxana-flexiarg--segments segments)
    (arxana-flexiarg--clear-metadata-overlays)
    (arxana-flexiarg--clear-context-overlays)
    (dolist (seg arxana-flexiarg--segments)
      (arxana-flexiarg--annotate-metadata (plist-get seg :start) (plist-get seg :end)))
    (setq arxana-flexiarg--metadata-visible nil)
    (arxana-flexiarg--metadata-set-visibility nil)
    (arxana-flexiarg--update-header-state)
    (message "Reconnected %d flexiarg files" (length arxana-flexiarg--segments))))

(defun arxana-flexiarg-show-sync-errors ()
  "Display the errors from the last flexiarg sync attempt."
  (interactive)
  (unless arxana-flexiarg--last-sync-errors
    (user-error "No sync errors recorded"))
  (let ((buffer (get-buffer-create "*Arxana Flexiarg Sync Errors*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Flexiarg sync errors:\n\n")
        (dolist (entry arxana-flexiarg--last-sync-errors)
          (insert (format "- %s: %s\n" (car entry) (cdr entry))))
        (goto-char (point-min))
        (view-mode 1)))
    (pop-to-buffer buffer)))

(defun arxana-flexiarg--update-header-state ()
  "Update header-line to reflect dirty state without editing buffer content."
  (let* ((dirty (buffer-modified-p))
         (errors (and (boundp 'arxana-flexiarg--last-sync-errors)
                      arxana-flexiarg--last-sync-errors))
         (face (cond
                (errors arxana-flexiarg--header-error-face)
                (dirty arxana-flexiarg--header-dirty-face)
                (t arxana-flexiarg--header-clean-face)))
         (suffix (concat
                  (if dirty " [unsaved edits]" "")
                  (if errors (format " [xtdb errors: %d]" (length errors)) ""))))
    (setq-local header-line-format
                (propertize
                 (concat "C-c C-s saves changes back to the original flexiarg files." suffix)
                 'face face)))
  (force-mode-line-update))

(setq arxana-flexiarg-collection-mode-map
      (let ((map (make-sparse-keymap)))
        (when (and (boundp 'flexiarg-mode-map) (keymapp flexiarg-mode-map))
          (set-keymap-parent map flexiarg-mode-map))
        (define-key map (kbd "C-c C-s") #'arxana-flexiarg--save)
        (define-key map (kbd "C-c C-k") #'arxana-flexiarg--revert)
        (define-key map (kbd "C-c C-r") #'arxana-flexiarg-reconnect-collection)
        (define-key map (kbd "C-c C-m") #'arxana-flexiarg-toggle-metadata)
        (define-key map (kbd "C-c C-e") #'arxana-flexiarg-show-sync-errors)
        (define-key map (kbd "C-c C-o") #'arxana-flexiarg-show-conclusions)
        (define-key map (kbd "TAB") #'outline-cycle)
        (define-key map (kbd "<backtab>") #'arxana-flexiarg-cycle-buffer)
        (define-key map (kbd "<left>") #'arxana-flexiarg-maybe-exit)
        (define-key map (kbd "S-TAB") #'arxana-flexiarg-cycle-buffer)
        (define-key map (kbd "<S-tab>") #'arxana-flexiarg-cycle-buffer)
        (define-key map (kbd "<S-iso-lefttab>") #'arxana-flexiarg-cycle-buffer)
        map))

(defvar arxana-flexiarg-file-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'arxana-flexiarg-file-save-sync)
    (define-key map (kbd "C-c C-e") #'arxana-flexiarg-show-sync-errors)
    (define-key map (kbd "<left>") #'arxana-flexiarg-maybe-exit)
    map)
  "Keymap for `arxana-flexiarg-file-mode'.")

(defvar-local arxana-flexiarg--cycle-state 'show-all)
(defvar-local arxana-flexiarg--last-sync-errors nil
  "List of (FILE . MESSAGE) pairs from the last XTDB sync attempt.")

(defun arxana-flexiarg-maybe-exit ()
  "Exit to the previous buffer when at point-min; otherwise move left."
  (interactive)
  (if (<= (point) (point-min))
      (quit-window)
    (backward-char)))

(define-minor-mode arxana-flexiarg-file-mode
  "Minor mode for flexiarg files with Arxana save/sync helpers."
  :lighter " Arxana-Flexiarg"
  :keymap arxana-flexiarg-file-mode-map
  (when arxana-flexiarg-file-mode
    (add-hook 'after-change-functions #'arxana-flexiarg--update-file-header-state nil t)
    (add-hook 'post-command-hook #'arxana-flexiarg--update-file-header-state nil t)
    (add-hook 'after-save-hook #'arxana-flexiarg--update-file-header-state nil t)
    (arxana-flexiarg--update-file-header-state)))

(defun arxana-flexiarg--update-file-header-state (&rest _args)
  "Update header-line for flexiarg file buffers."
  (let* ((dirty (buffer-modified-p))
         (errors (and (boundp 'arxana-flexiarg--last-sync-errors)
                      arxana-flexiarg--last-sync-errors))
         (face (cond
                (errors arxana-flexiarg--header-error-face)
                (dirty arxana-flexiarg--header-dirty-face)
                (t arxana-flexiarg--header-clean-face)))
         (suffix (concat
                  (if dirty " [unsaved edits]" "")
                  (if errors (format " [xtdb errors: %d]" (length errors)) ""))))
    (setq-local header-line-format
                (propertize
                 (concat "C-c C-s saves to filesystem and syncs to XTDB." suffix)
                 'face face)))
  (force-mode-line-update))

(defun arxana-flexiarg-file-save-sync ()
  "Save the current flexiarg file and sync it to XTDB when enabled."
  (interactive)
  (unless buffer-file-name
    (user-error "Current buffer is not visiting a file"))
  (save-buffer)
  (arxana-flexiarg--sync-files (list buffer-file-name))
  (arxana-flexiarg--update-file-header-state)
  (message "Saved %s" (file-name-nondirectory buffer-file-name)))

(define-derived-mode arxana-flexiarg-collection-mode flexiarg-mode "Flexiarg-Collection"
  "Edit multiple flexiarg files with structural tracking."
  (setq buffer-read-only nil)
  (add-hook 'before-change-functions #'arxana-flexiarg--before-change nil t)
  (add-hook 'after-change-functions #'arxana-flexiarg--after-change nil t)
  (add-hook 'after-change-functions (lambda (&rest _) (arxana-flexiarg--update-header-state)) nil t)
  (add-hook 'post-command-hook #'arxana-flexiarg--update-header-state nil t)
  (add-hook 'after-save-hook #'arxana-flexiarg--update-header-state nil t)
  (setq arxana-flexiarg--cycle-state 'show-all)
  (setq-local outline-regexp "^;;; .*$\\|^@[^\n]*\\|^![^\n]*\\|^\\s-+[+?].*$")
  (setq-local outline-level
              (lambda ()
                (cond
                 ((looking-at "^;;;") 0)
                 ((looking-at "^@") 1)
                 ((looking-at "^!") 1)
                 ((looking-at "^\\s-+[+?]") 1)
                 (t 1000))))
  (let ((map (copy-keymap outline-minor-mode-map)))
    (define-key map (kbd "<backtab>") #'arxana-flexiarg-cycle-buffer)
    (define-key map (kbd "S-TAB") #'arxana-flexiarg-cycle-buffer)
    (define-key map (kbd "<S-tab>") #'arxana-flexiarg-cycle-buffer)
    (setq-local outline-minor-mode-map map))
  (outline-minor-mode 1)
  (font-lock-add-keywords
   nil
   '((";;; \\(.*\\)$" 0 '(:inherit font-lock-comment-face :weight bold))))
  (arxana-flexiarg--update-header-state))

(defun arxana-flexiarg--ensure-outline-invisibility ()
  (dolist (token (list 'outline arxana-flexiarg--hidden-token))
    (unless (memq token buffer-invisibility-spec)
      (add-to-invisibility-spec token))))

(defun arxana-flexiarg--show-headings ()
  (arxana-flexiarg--ensure-outline-invisibility)
  (outline-show-all)
  (outline-hide-sublevels 1))

(defun arxana-flexiarg--show-conclusions ()
  (arxana-flexiarg--show-headings)
  (arxana-flexiarg--clear-context-overlays)
  (when arxana-flexiarg--metadata-visible
    (setq arxana-flexiarg--fold-restore-metadata t))
  (arxana-flexiarg--metadata-set-visibility nil)
  (setq arxana-flexiarg--metadata-visible nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^!" nil t)
      (outline-show-entry)
      (save-excursion
        (forward-line 1)
        (while (and (not (eobp))
                    (not (looking-at "^;;;\\|^!\\|^\\s-*[+?!]")))
          (outline-show-entry)
          (forward-line 1)))))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\([ \t]*\\)\\([+?].*\\)$" nil t)
      (let* ((indent (match-string 1))
             (label (string-trim (match-string 2)))
             (bol (match-beginning 0))
             (body-start (line-beginning-position 2))
             (end (save-excursion
                    (goto-char body-start)
                    (while (and (not (eobp))
                                (or (looking-at "^[ \t]+[^+?!]")
                                    (looking-at "^[ \t]*$")))
                      (forward-line 1))
                    (point)))
             (placeholder (concat indent
                                  (propertize (format "%s (hidden)" label)
                                              'face arxana-flexiarg--context-placeholder-face)
                                  "\n"))
             (ov (make-overlay bol end)))
        (arxana-flexiarg--hide-range bol end)
        (overlay-put ov 'arxana-flexiarg-context-label label)
        (overlay-put ov 'display placeholder)
        (overlay-put ov 'priority 500)
        (overlay-put ov 'intangible t)
        (overlay-put ov 'modification-hooks '(arxana-flexiarg--context--prevent-edit))
        (push ov arxana-flexiarg--context-overlays)))))

(defun arxana-flexiarg-show-conclusions ()
  "Fold the current buffer so only `!` conclusion headers remain visible."
  (interactive)
  (unless (derived-mode-p 'arxana-flexiarg-collection-mode)
    (user-error "Only available in flexiarg collection buffers"))
  (arxana-flexiarg--show-conclusions)
  (setq arxana-flexiarg--cycle-state 'conclusions)
  (message "Showing only conclusion lines"))

(defun arxana-flexiarg-cycle-buffer ()
  "Cycle folding states for flexiarg buffers.\nOverview keeps `!` lines visible even at the tightest fold."
  (interactive)
  (unless (derived-mode-p 'arxana-flexiarg-collection-mode)
    (user-error "Only available in flexiarg collection buffers"))
  (if (eq arxana-flexiarg--cycle-state 'conclusions)
      (progn
        (outline-show-all)
        (arxana-flexiarg--clear-context-overlays)
        (when arxana-flexiarg--fold-restore-metadata
          (setq arxana-flexiarg--fold-restore-metadata nil)
          (arxana-flexiarg--metadata-set-visibility t)
          (setq arxana-flexiarg--metadata-visible t))
        (setq arxana-flexiarg--cycle-state 'show-all)
        (message "Showing entire flexiarg collection"))
    (arxana-flexiarg-show-conclusions)))

(provide 'arxana-flexiarg-collection)
;;; arxana-flexiarg-collection.el ends here
