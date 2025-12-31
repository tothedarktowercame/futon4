;;; arxana-browser-patterns.el --- Pattern browser views -*- lexical-binding: t; -*-

;;; Commentary:
;; Pattern browsing and editing helpers for the Arxana browser.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'org)
(require 'org-element)
(require 'tabulated-list)

(require 'arxana-browser-core)
(require 'arxana-store)
(require 'arxana-flexiarg-collection)
(require 'arxana-patterns-ingest)

(defvar flexiarg-mode-map (make-sparse-keymap))
(declare-function arxana-browser--click-path "arxana-browser-core")
(declare-function arxana-browser--item-at-point "arxana-browser-core")
(declare-function arxana-browser--render "arxana-browser-core")

(defvar arxana-browser--buffer)
(defvar arxana-browser--stack)
(defvar arxana-browser--context)

(defvar arxana-flexiarg-collection-mode-map
  (let ((map (make-sparse-keymap)))
    (when (keymapp flexiarg-mode-map)
      (set-keymap-parent map flexiarg-mode-map))
    (define-key map (kbd "C-c C-s") #'arxana-flexiarg--save)
    (define-key map (kbd "C-c C-k") #'arxana-flexiarg--revert)
    (define-key map (kbd "C-c C-m") #'arxana-flexiarg-toggle-metadata)
    (define-key map (kbd "C-c C-o") #'arxana-flexiarg-show-conclusions)
    (define-key map (kbd "TAB") #'outline-cycle)
    (define-key map (kbd "<backtab>") #'arxana-flexiarg-cycle-buffer)
    (define-key map (kbd "S-TAB") #'arxana-flexiarg-cycle-buffer)
    (define-key map (kbd "<S-tab>") #'arxana-flexiarg-cycle-buffer)
    (define-key map (kbd "<S-iso-lefttab>") #'arxana-flexiarg-cycle-buffer)
    map)
  "Keymap for `arxana-flexiarg-collection-mode'.")

(defvar arxana-browser-patterns-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'arxana-browser-patterns-save)
    (define-key map (kbd "g") #'arxana-browser-patterns-refresh-buffer)
    map)
  "Keymap for `arxana-browser-patterns-view-mode'.")

(defun arxana-browser-patterns--update-header-state ()
  "Update header-line to reflect dirty state without editing buffer content."
  (let* ((dirty (buffer-modified-p))
         (face (if dirty
                   arxana-browser-patterns--header-dirty-face
                 arxana-browser-patterns--header-clean-face))
         (suffix (if dirty " [unsaved edits]" "")))
    (setq-local header-line-format
                (propertize
                 (concat "C-c C-s to sync changes; g to refetch from Futon." suffix)
                 'face face))))

(defgroup arxana-patterns nil
  "Utilities for browsing Futon pattern entities in Emacs."
  :group 'arxana)

(defcustom arxana-browser-patterns-ego-limit 64
  "Number of neighbor links to request when fetching Futon pattern data."
  :type 'integer
  :group 'arxana-patterns)

(defcustom arxana-browser-patterns-language-ego-limit 256
  "Neighbor limit to use when listing pattern languages and their members."
  :type 'integer
  :group 'arxana-patterns)

(defcustom arxana-browser-patterns-library-root nil
  "Path to the Futon3 pattern library checkout.
When nil the browser attempts to locate a \"futon3/library\" directory
relative to the current buffer or this file."
  :type '(choice (const :tag "Auto-detect" nil)
                 directory)
  :group 'arxana-patterns)

(defcustom arxana-browser-patterns-prefer-filesystem t
  "When non-nil, open local flexiarg files if they are newer than XTDB."
  :type 'boolean
  :group 'arxana-patterns)

(defcustom arxana-browser-patterns-collection-roots-file
  (locate-user-emacs-file "arxana-collection-roots.el")
  "File used to remember additional collection roots across Emacs sessions.
Set to nil to disable persistence."
  :type '(choice (const :tag "Do not persist" nil)
                 file)
  :group 'arxana-patterns)

(defvar-local arxana-browser-patterns--pattern nil
  "Buffer-local plist describing the currently loaded pattern.")

(defconst arxana-browser-patterns--summary-begin "#+BEGIN_SUMMARY")

(defconst arxana-browser-patterns--summary-end "#+END_SUMMARY")

(defconst arxana-browser-patterns--header-clean-face
  '(:inherit mode-line))
(defconst arxana-browser-patterns--header-dirty-face
  '(:inherit mode-line :foreground "orange"))

(defun arxana-browser-patterns--play-click ()
  (let ((path (arxana-browser--click-path)))
    (when (and arxana-browser-enable-click
               path
               (display-graphic-p)
               (fboundp 'play-sound))
      (ignore-errors
        (let ((player (executable-find "pw-play"))
              (volume (max 0.0 (min 1.0 arxana-browser-click-volume))))
          (if player
              (start-process "arxana-click" nil player
                             "--volume" (format "%.3f" volume)
                             "--media-role" "event"
                             path)
            (play-sound `(sound :file ,path :volume ,volume))))))))

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

(defun arxana-browser-patterns--locate-library-root ()
  (let ((explicit arxana-browser-patterns-library-root))
    (cond
     ((and explicit (file-directory-p explicit)) (expand-file-name explicit))
     (t
      (let* ((current (or load-file-name buffer-file-name default-directory))
             (root (locate-dominating-file current "futon3")))
        (when root
          (let ((candidate (expand-file-name "futon3/library" root)))
            (and (file-directory-p candidate) candidate))))))))

(defun arxana-browser-patterns--file-mtime (file)
  (when (and file (file-exists-p file))
    (file-attribute-modification-time (file-attributes file))))

(defun arxana-browser-patterns--pattern-file (name)
  "Return the flexiarg file path for NAME if it exists."
  (when (and name (not (string-empty-p name)))
    (let* ((relative (concat name ".flexiarg"))
           (roots (delq nil (append (arxana-browser-patterns--collection-root-paths)
                                    (list (arxana-browser-patterns--locate-library-root))
                                    (list default-directory)))))
      (cl-loop for root in roots
               for candidate = (expand-file-name relative root)
               when (file-exists-p candidate)
               return candidate))))

(defun arxana-browser-patterns--normalize-last-seen (value)
  "Coerce VALUE into a time value (seconds) when possible."
  (cond
   ((null value) nil)
   ((consp value) value)
   ((numberp value)
    (if (> value 1000000000000)
        (seconds-to-time (/ value 1000.0))
      (seconds-to-time value)))
   ((stringp value)
    (ignore-errors (date-to-time value)))
   (t nil)))

(defun arxana-browser-patterns--filesystem-newer-p (file xtdb-last-seen)
  (let ((file-time (arxana-browser-patterns--file-mtime file))
        (xtdb-time (arxana-browser-patterns--normalize-last-seen xtdb-last-seen)))
    (and file-time
         (or (not xtdb-time)
             (time-less-p xtdb-time file-time)))))

(defun arxana-browser-patterns--library-directories (&optional root)
  (let ((root (or root (arxana-browser-patterns--locate-library-root))))
    (when root
      (seq-sort #'string<
                (seq-filter
                 (lambda (entry)
                   (let ((full (expand-file-name entry root)))
                     (and (file-directory-p full)
                          (not (member entry '("." ".."))))))
                 (directory-files root))))))

(defun arxana-browser-patterns--flexiarg-files-in-directory (dir)
  (when (and dir (file-directory-p dir))
    (seq-sort #'string<
              (seq-filter #'file-regular-p
                          (directory-files dir t "\\.flexiarg\\'")))))

(defun arxana-browser-patterns--flexiarg-files-for (library &optional root)
  (let* ((root (or root (arxana-browser-patterns--locate-library-root)))
         (dir (and root (expand-file-name library root))))
    (arxana-browser-patterns--flexiarg-files-in-directory dir)))

(defun arxana-browser-patterns--normalize-path (path)
  (when path
    (file-name-as-directory (expand-file-name path))))

(defun arxana-browser-patterns--library-metadata-path (directory)
  (expand-file-name arxana-browser-patterns--library-metadata-file
                    (file-name-as-directory directory)))

(defun arxana-browser-patterns--write-library-metadata (directory language-name language-title status)
  "Persist LANGUAGE-NAME metadata for DIRECTORY so the browser can detect imports.
LANGUAGE-TITLE and STATUS are optional strings recorded for display."
  (when (and directory language-name)
    (let ((path (arxana-browser-patterns--library-metadata-path directory))
          (payload (list :language-name language-name
                         :language-title language-title
                         :status status
                         :directory (arxana-browser-patterns--normalize-path directory)
                         :updated-at (float-time (current-time)))))
      (with-temp-file path
        (let ((print-length nil)
              (print-level nil))
          (prin1 payload (current-buffer)))))))

(defun arxana-browser-patterns--read-library-metadata (directory)
  "Return the metadata plist stored for DIRECTORY, or nil."
  (let ((path (arxana-browser-patterns--library-metadata-path directory)))
    (when (file-readable-p path)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-min))
            (read (current-buffer)))
        (error nil)))))

(defun arxana-browser-patterns--load-persisted-collection-roots ()
  (unless arxana-browser-patterns--persisted-collection-roots-loaded
    (setq arxana-browser-patterns--persisted-collection-roots-loaded t)
    (let* ((path arxana-browser-patterns-collection-roots-file)
           (raw (when (and path (file-readable-p path))
                  (condition-case nil
                      (with-temp-buffer
                        (insert-file-contents path)
                        (goto-char (point-min))
                        (read (current-buffer)))
                    (error nil)))))
      (setq arxana-browser-patterns--persisted-collection-roots
            (mapcar #'arxana-browser-patterns--normalize-path (delq nil raw)))))
  (delq nil arxana-browser-patterns--persisted-collection-roots))

(defun arxana-browser-patterns--persist-collection-roots ()
  (let ((path arxana-browser-patterns-collection-roots-file))
    (when path
      (make-directory (file-name-directory path) t)
      (with-temp-file path
        (let ((print-length nil)
              (print-level nil))
          (prin1 (delq nil arxana-browser-patterns--persisted-collection-roots)
                 (current-buffer)))))))

(defun arxana-browser-patterns--remember-collection-root (directory)
  (let ((path (arxana-browser-patterns--normalize-path directory)))
    (when path
      (arxana-browser-patterns--load-persisted-collection-roots)
      (unless (member path arxana-browser-patterns--persisted-collection-roots)
        (setq arxana-browser-patterns--persisted-collection-roots
              (cons path arxana-browser-patterns--persisted-collection-roots))
        (arxana-browser-patterns--persist-collection-roots)))))

(defun arxana-browser-patterns--collection-root-paths ()
  "Return a de-duplicated list of active collection roots."
  (let* ((canonical (arxana-browser-patterns--locate-library-root))
         (persisted (arxana-browser-patterns--load-persisted-collection-roots))
         (roots (append (list canonical)
                        persisted
                        arxana-browser-patterns--additional-collection-roots)))
    (seq-uniq (delq nil (mapcar #'arxana-browser-patterns--normalize-path roots)))))

(defun arxana-browser-patterns--collection-directories (root)
  "Return directories under ROOT that contain `.flexiarg` files.
When ROOT itself has flexiarg files, include it as `\".\"`."
  (let ((dirs (or (arxana-browser-patterns--library-directories root) '())))
    (if (arxana-browser-patterns--flexiarg-files-in-directory root)
        (cons "." dirs)
      dirs)))

(defun arxana-browser-patterns--collection-from-directory (directory)
  "Return a collection plist for DIRECTORY."
  (let* ((dir (arxana-browser-patterns--normalize-path directory))
         (files (arxana-browser-patterns--flexiarg-files-in-directory dir)))
    (unless files
      (user-error "No .flexiarg files found in %s" dir))
    (let ((label (file-name-nondirectory (directory-file-name dir))))
      (list :type 'collection
            :label label
            :title label
            :files files
            :directory dir
            :source (if (= (length files) 1)
                        (car files)
                      (format "%s (%d files)" label (length files)))
            :status "local"
            :count (length files)
            :root (file-name-directory (directory-file-name dir))))))

(defun arxana-browser-patterns--primary-order-for-file (file)
  (when (and file (file-readable-p file))
    (let* ((entries (arxana-browser-patterns--parse-flexiarg file))
           (first (car entries)))
      (plist-get first :order))))

(defun arxana-browser-patterns--sort-files-by-order (files)
  (let ((copy (copy-sequence files)))
    (sort copy
          (lambda (a b)
            (let ((oa (or (arxana-browser-patterns--primary-order-for-file a)
                          most-positive-fixnum))
                  (ob (or (arxana-browser-patterns--primary-order-for-file b)
                          most-positive-fixnum)))
              (if (/= oa ob)
                  (< oa ob)
                (string< a b)))))))

(defun arxana-browser-patterns--register-collection-root (directory &optional quiet)
  (let ((path (arxana-browser-patterns--normalize-path directory)))
    (when path
      (unless (member path arxana-browser-patterns--additional-collection-roots)
        (push path arxana-browser-patterns--additional-collection-roots))
      (arxana-browser-patterns--remember-collection-root path)
      (unless quiet
        (message "Added collection root %s" path))
      path)))

(defun arxana-browser-patterns--read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun arxana-browser-patterns--insert-flexiarg-section (file)
  "Insert an Org heading that embeds FILE."
  (let ((title (file-name-nondirectory file)))
    (insert (format "* %s\n" title))
    (insert ":PROPERTIES:\n")
    (insert (format ":FILE: %s\n" file))
    (insert ":END:\n\n")
    (insert "#+BEGIN_SRC flexiarg\n")
    (insert (arxana-browser-patterns--read-file file))
    (unless (bolp)
      (insert "\n"))
    (insert "#+END_SRC\n\n")))

(defun arxana-browser-patterns--friendly-classification (value)
  (cond
   ((null value) nil)
   ((stringp value)
    (if (string-match "/\\([^/]+\\)\\'" value)
        (match-string 1 value)
      value))
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun arxana-browser-patterns--status-label (status &optional cached)
  (let ((base (or (arxana-browser-patterns--friendly-classification status)
                  "imported")))
    (if cached
        (format "%s (cached)" base)
      (format "imported/%s" base))))

(defun arxana-browser-patterns--imported-status-label (language)
  (arxana-browser-patterns--status-label (plist-get language :status)))

(defun arxana-browser-patterns--metadata-status-label (metadata)
  (arxana-browser-patterns--status-label (plist-get metadata :status) t))

(defvar-local arxana-flexiarg--segments nil
  "List of tracked flexiarg segments in the current buffer.")

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
            (put-text-property pos (or next end) 'invisible nil))
          (setq pos (or next end)))))))

(defun arxana-flexiarg--metadata--prevent-edit (_ov after _beg _end &optional _len)
  (unless (or after
              arxana-flexiarg--metadata-visible
              arxana-flexiarg--inhibit-guards)
    (user-error "Metadata is read-only; toggle visibility with C-c C-m")))

(defun arxana-flexiarg--context--prevent-edit (_ov after _beg _end &optional _len)
  (unless (or after arxana-flexiarg--inhibit-guards)
    (user-error "Hidden block is read-only; expand the view to edit it")))

(defun arxana-flexiarg--metadata-overlay-set-hidden (ov hidden)
  (let ((start (overlay-start ov))
        (end (overlay-end ov)))
    (when (and start end)
      (if hidden
          (progn
            (arxana-flexiarg--hide-range start end)
            (overlay-put ov 'before-string (concat arxana-flexiarg--metadata-placeholder "\n"))
            (overlay-put ov 'intangible t)
            (overlay-put ov 'priority 1000)
            (overlay-put ov 'modification-hooks '(arxana-flexiarg--metadata--prevent-edit)))
        (arxana-flexiarg--show-range start end)
        (overlay-put ov 'before-string nil)
        (overlay-put ov 'priority nil)
        (overlay-put ov 'intangible nil)
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
  (list :start start :end end :file file))

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

(defun arxana-browser-patterns--flexiarg-set-order (file order)
  "Ensure FILE declares `@order ORDER` in its metadata."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((meta-end (save-excursion
                      (goto-char (point-min))
                      (while (looking-at "^@[^\n]*$")
                        (forward-line 1))
                      (point))))
      (goto-char (point-min))
      (if (re-search-forward "^@order\\s-+.*$" meta-end t)
          (replace-match (format "@order %d" order) t t)
        (goto-char meta-end)
        (unless (bolp)
          (insert "\n"))
        (insert (format "@order %d\n" order))
        (setq meta-end (point))))
    (write-region (point-min) (point-max) file nil 'silent)))

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
                                                    :weight bold)))
        (let ((body-start (point)))
          (insert (arxana-browser-patterns--read-file file))
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

(defun arxana-flexiarg--before-change (beg end)
  "Disallow edits spanning multiple source files."
  (unless arxana-flexiarg--inhibit-guards
    (let ((file (arxana-flexiarg--segment-for-region beg end)))
      (unless file
        (user-error "Edits must stay within a single source flexiarg file")))))

(defun arxana-flexiarg--after-change (beg end length)
  (unless arxana-flexiarg--inhibit-guards
    (let* ((file (get-text-property beg 'arxana-source-file))
           (delta (- (- end beg) length)))
      (when (and file (/= delta 0))
        (save-excursion
          (dolist (seg arxana-flexiarg--segments)
            (when (string= (plist-get seg :file) file)
              (let ((seg-start (plist-get seg :start))
                    (seg-end (plist-get seg :end)))
                (cond
                 ((<= beg seg-start)
                  (plist-put seg :start (+ seg-start delta))
                  (plist-put seg :end (+ seg-end delta)))
                 ((and (> beg seg-start) (< beg seg-end))
                  (plist-put seg :end (+ seg-end delta))))))))))))

(defun arxana-flexiarg--save ()
  "Write the current flexiarg buffer back to its source files."
  (interactive)
  (unless arxana-flexiarg--segments
    (user-error "This buffer is not tracking any flexiarg files"))
  (let ((written 0))
    (dolist (segment arxana-flexiarg--segments)
      (let* ((file (plist-get segment :file))
             (bounds (arxana-flexiarg--segment-bounds segment)))
        (unless bounds
          (user-error "Could not locate buffer region for %s" file))
        (let* ((start (car bounds))
               (end (cdr bounds))
               (text (buffer-substring-no-properties start end)))
          (with-temp-file file
            (insert text))
          (cl-incf written))))
    (message "Saved %d flexiarg files" written)))

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

(defvar-local arxana-flexiarg--cycle-state 'show-all)

(define-derived-mode arxana-flexiarg-collection-mode flexiarg-mode "Flexiarg-Collection"
  "Edit multiple flexiarg files with structural tracking."
  (setq buffer-read-only nil)
  (add-hook 'before-change-functions #'arxana-flexiarg--before-change nil t)
  (add-hook 'after-change-functions #'arxana-flexiarg--after-change nil t)
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
  (setq-local header-line-format "C-c C-s saves changes back to the original flexiarg files."))

(defun arxana-flexiarg--ensure-outline-invisibility ()
  (dolist (token (list 'outline arxana-flexiarg--hidden-token))
    (unless (member token buffer-invisibility-spec)
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
  "Cycle folding states for flexiarg buffers.
Overview keeps `!` lines visible even at the tightest fold."
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

(defun arxana-browser-patterns--language-index-by-path (language-rows)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (row language-rows table)
      (when-let* ((raw (plist-get row :import-path))
                  (path (arxana-browser-patterns--normalize-path raw)))
        (puthash path row table)))))

(defun arxana-browser-patterns--parse-flexiarg (file)
  (let ((lines (split-string (with-temp-buffer
                               (insert-file-contents file)
                               (buffer-string))
                             "\n" t))
        (current nil)
        (results nil))
    (dolist (line lines)
      (cond
       ((string-match "^@\\(arg\\|flexiarg\\|name\\)\\s-+\\(.+\\)$" line)
        (when current
          (push current results))
        (setq current (list :name (string-trim (match-string 2 line)))))
       ((and current (string-match "^@title\\s-+\\(.+\\)$" line))
        (setq current (plist-put current :title (string-trim (match-string 1 line)))))
       ((and current (string-match "^@order\\s-+\\([0-9]+\\)$" line))
        (setq current (plist-put current :order (string-to-number (match-string 1 line)))))))
    (when current
      (push current results))
    (let ((final (nreverse results)))
      (if final
          final
        (list
         (list :name (arxana-browser-patterns-ingest--derive-name-from-path file)
               :order nil))))))

(defun arxana-browser-patterns--filesystem-collection-items (&optional language-index)
  (let (items)
    (dolist (root (arxana-browser-patterns--collection-root-paths))
      (when (file-directory-p root)
        (dolist (dir (arxana-browser-patterns--collection-directories root))
          (let* ((actual-dir (if (string= dir ".")
                                 root
                               (expand-file-name dir root)))
                 (files (arxana-browser-patterns--flexiarg-files-in-directory actual-dir)))
            (when files
              (let* ((count (length files))
                     (abs-dir (arxana-browser-patterns--normalize-path actual-dir))
                     (language (and language-index
                                    abs-dir
                                    (gethash abs-dir language-index)))
                     (metadata (arxana-browser-patterns--read-library-metadata abs-dir)))
                (when (and language (not metadata))
                  (arxana-browser-patterns--write-library-metadata abs-dir
                                                           (plist-get language :name)
                                                           (plist-get language :title)
                                                           (plist-get language :status))
                  (setq metadata (arxana-browser-patterns--read-library-metadata abs-dir)))
                (let* ((status (cond
                                (language (arxana-browser-patterns--imported-status-label language))
                                (metadata (arxana-browser-patterns--metadata-status-label metadata))
                                (t "local")))
                       (language-name (or (plist-get language :name)
                                          (plist-get metadata :language-name)))
                       (label (if (string= dir ".")
                                  (file-name-nondirectory (directory-file-name abs-dir))
                                dir)))
                  (push (append (list :type 'collection
                                      :label label
                                      :title label
                                      :files files
                                      :directory abs-dir
                                      :source (if (= count 1)
                                                  (file-relative-name (car files) root)
                                                (format "%s (%d files)" label count))
                                      :status status
                                      :count count
                                      :root root)
                                (when language
                                  (list :language language))
                                (when language-name
                                  (list :language-name language-name))
                                (when metadata
                                  (list :metadata metadata)))
                        items))))))))
    (nreverse items)))

(defun arxana-browser-patterns-edit-collection (&optional collection)
  "Edit every `.flexiarg` file in COLLECTION inside a single flexiarg buffer.
When called interactively with point on a collection row inside the browser,
use that entry; otherwise prompt for a directory."
  (interactive)
  (let* ((collection
          (or collection
              (and (derived-mode-p 'arxana-browser-patterns-browser-mode)
                   (let ((item (arxana-browser--item-at-point)))
                     (and item (eq (plist-get item :type) 'collection) item)))
              (arxana-browser-patterns--collection-from-directory
               (read-directory-name "Collection directory: " nil nil t))))
         (files (arxana-browser-patterns--sort-files-by-order
                 (plist-get collection :files))))
    (unless files
      (user-error "No `.flexiarg` files found for %s" (plist-get collection :label)))
    (arxana-flexiarg--ensure-flexiarg)
    (let* ((label (plist-get collection :label))
           (buffer (get-buffer-create (format "*Flexiarg Collection: %s*" label))))
      (with-current-buffer buffer
        (arxana-flexiarg-collection-mode)
        (arxana-flexiarg--prepare-buffer files)
        (goto-char (point-min)))
      (pop-to-buffer buffer)
      buffer)))

(define-minor-mode arxana-browser-patterns-view-mode
  "Minor mode for pattern editing buffers."
  :lighter " Pattern"
  :keymap arxana-browser-patterns-view-mode-map
  (when arxana-browser-patterns-view-mode
    (add-hook 'after-change-functions #'arxana-browser-patterns--update-header-state nil t)
    (arxana-browser-patterns--update-header-state)))

(defun arxana-browser-patterns--ensure-sync ()
  (unless (arxana-store-ensure-sync)
    (user-error "Futon sync is disabled; enable futon4-enable-sync first")))

(defun arxana-browser-patterns--alist (key alist)
  (alist-get key alist))

(defun arxana-browser-patterns--alist-like-p (value)
  (and (listp value)
       (let ((first (car-safe value)))
         (and first (consp first)))))

(defun arxana-browser-patterns--entity-value (entity &rest keys)
  "Return the first matching value in ENTITY for the provided :entity/* KEYS."
  (when (arxana-browser-patterns--alist-like-p entity)
    (seq-some (lambda (key)
                (let ((cell (assoc key entity)))
                  (when cell (cdr cell))))
              keys)))

(defun arxana-browser-patterns--entity-from-version (entity)
  "Return the entity payload stored inside ENTITY's version data, if any."
  (when (arxana-browser-patterns--alist-like-p entity)
    (let* ((version (arxana-browser-patterns--alist :version entity))
           (data (and (arxana-browser-patterns--alist-like-p version)
                      (arxana-browser-patterns--alist :data version)))
           (payload (and (arxana-browser-patterns--alist-like-p data)
                         (arxana-browser-patterns--alist :entity data))))
      (and (arxana-browser-patterns--alist-like-p payload) payload))))

(defun arxana-browser-patterns--find-entity (tree target-id)
  "Locate the entity with TARGET-ID inside TREE (direct or linked payloads)."
  (let ((match nil))
    (cl-labels ((entity-matches-p (entity)
                  (let ((id (arxana-browser-patterns--entity-value entity :id :entity/id)))
                    (and id target-id (equal id target-id))))
                (walk (node)
                  (when (and node (not match))
                    (cond
                     ((arxana-browser-patterns--alist-like-p node)
                      (when (entity-matches-p node)
                        (setq match node))
                      (dolist (pair node)
                        (when (and (consp pair) (not match))
                          (walk (cdr pair)))))
                     ((listp node)
                      (dolist (item node)
                        (walk item)))))))
      (walk tree)
      match)))

(defun arxana-browser-patterns--pattern-entity (ego)
  (arxana-browser-patterns--alist :entity ego))

(defun arxana-browser-patterns--resolve-entity-by-name (name)
  "Return the Futon entity alist for NAME via `/ego`."
  (let* ((ego-response (arxana-store-ego name 1))
         (ego (and ego-response (arxana-browser-patterns--alist :ego ego-response))))
    (and ego (arxana-browser-patterns--alist :entity ego))))

(defun arxana-browser-patterns--relation-text (value)
  "Return VALUE as a normalized relation string without the leading colon."
  (let ((text (cond
               ((keywordp value) (symbol-name value))
               ((symbolp value) (symbol-name value))
               ((stringp value) value)
               (t nil))))
    (when text
      (if (and (> (length text) 0)
               (eq (aref text 0) ?:))
          (substring text 1)
        text))))

(defun arxana-browser-patterns--relation-match-p (value target)
  "Return non-nil when VALUE (keyword/string) matches TARGET (string/keyword)."
  (let ((lhs (arxana-browser-patterns--relation-text value))
        (rhs (arxana-browser-patterns--relation-text target)))
    (and lhs rhs (string= lhs rhs))))

(defun arxana-browser-patterns--ego-outgoing (ego)
  "Return normalized outgoing link list for EGO nodes."
  (when (arxana-browser-patterns--alist-like-p ego)
    (or (arxana-browser-patterns--alist :outgoing ego)
        (let ((links (arxana-browser-patterns--alist :links ego)))
          (and (arxana-browser-patterns--alist-like-p links)
               (arxana-browser-patterns--alist :outgoing links))))))

(defun arxana-browser-patterns--link-prop (link key)
  "Return LINK property identified by KEY (keyword/string)."
  (let* ((props (or (arxana-browser-patterns--alist :props link)
                    (arxana-browser-patterns--alist 'props link)
                    (arxana-browser-patterns--alist :properties link)
                    (arxana-browser-patterns--alist 'properties link)))
         (target (arxana-browser-patterns--relation-text key)))
    (when (and props target)
      (let ((match (seq-find (lambda (entry)
                               (and (consp entry)
                                    (let ((key-text (arxana-browser-patterns--relation-text (car entry))))
                                      (and key-text (string= key-text target)))))
                             props)))
        (cdr match)))))

(defun arxana-browser-patterns--component-links (ego)
  (when (arxana-browser-patterns--alist-like-p ego)
    (let ((outgoing (arxana-browser-patterns--ego-outgoing ego)))
      (seq-filter
       (lambda (entry)
         (arxana-browser-patterns--relation-match-p
          (arxana-browser-patterns--alist :relation entry)
          ":pattern/includes"))
       outgoing))))

(defun arxana-browser-patterns--component-slug-base (name)
  (when (and name (string-match "\\`\\(.+\\)/[0-9]+-[^/]+\\'" name))
    (match-string 1 name)))

(defun arxana-browser-patterns--component-link-name (link)
  (let ((entity (arxana-browser-patterns--alist :entity link)))
    (or (arxana-browser-patterns--entity-value entity :name :entity/name)
        (arxana-browser-patterns--entity-value entity :ident :entity/ident))))

(defun arxana-browser-patterns--lookup-component-by-name (component-name)
  (let ((base (arxana-browser-patterns--component-slug-base component-name)))
    (when base
      (let* ((ego-response (arxana-store-ego base arxana-browser-patterns-ego-limit))
             (ego (and ego-response (arxana-browser-patterns--alist :ego ego-response)))
             (links (and (arxana-browser-patterns--alist-like-p ego)
                         (arxana-browser-patterns--component-links ego)))
             (matching (and links
                            (cl-find-if (lambda (link)
                                          (string= (arxana-browser-patterns--component-link-name link)
                                                   component-name))
                                        links))))
        (when matching
          (arxana-browser-patterns--fetch-component matching))))))

(defun arxana-browser-patterns--lookup-component-by-prefix (component-name)
  (let ((base (arxana-browser-patterns--component-slug-base component-name)))
    (when base
      (let* ((ego-response (arxana-store-ego base arxana-browser-patterns-ego-limit))
             (ego (and ego-response (arxana-browser-patterns--alist :ego ego-response)))
             (links (and (arxana-browser-patterns--alist-like-p ego)
                        (arxana-browser-patterns--component-links ego)))
             (matching (and links
                            (cl-find-if (lambda (link)
                                          (let ((name (arxana-browser-patterns--component-link-name link)))
                                            (and name (string-prefix-p name component-name))))
                                        links))))
        (when matching
          (arxana-browser-patterns--fetch-component matching))))))

(defun arxana-browser-patterns--extract-summary ()
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^" (regexp-quote arxana-browser-patterns--summary-begin) "\\s-*$") nil t)
          (let ((start (progn (forward-line 1) (point))))
            (if (re-search-forward (concat "^" (regexp-quote arxana-browser-patterns--summary-end) "\\s-*$") nil t)
                (string-trim (buffer-substring-no-properties start (match-beginning 0)))
              ""))
        ""))))

(defun arxana-browser-patterns--read-header-field (label)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (rx (format "^#\\+%s: \\(.*\\)$" (regexp-quote label))))
      (when (re-search-forward rx nil t)
        (string-trim (match-string 1))))))

(defun arxana-browser-patterns--component-name-info (name)
  (if (and name (string-match "/\\([0-9]+\\)-\\([^/]+\\)$" name))
      (list :order (string-to-number (match-string 1 name))
            :kind (match-string 2 name))
    (list :order 0 :kind (or name "component"))))

(defun arxana-browser-patterns--component-parent-id (component-name)
  (when component-name
    (let* ((ego-response (ignore-errors (arxana-store-ego component-name arxana-browser-patterns-ego-limit)))
           (ego (and ego-response (arxana-browser-patterns--alist :ego ego-response)))
           (incoming (and ego (arxana-browser-patterns--alist :incoming ego)))
           (parent-link (and incoming
                             (cl-find-if (lambda (entry)
                                           (arxana-browser-patterns--relation-match-p
                                            (arxana-browser-patterns--alist :relation entry)
                                            ":pattern/component-parent"))
                                         incoming))))
      (when parent-link
        (let* ((entity (arxana-browser-patterns--alist :entity parent-link)))
          (or (arxana-browser-patterns--alist :entity/id entity)
              (arxana-browser-patterns--alist :id entity)))))))

(defun arxana-browser-patterns--fetch-entity-source (entity-id)
  (when entity-id
    (let* ((response (ignore-errors (arxana-store-fetch-entity entity-id)))
           (entity (and response (arxana-browser-patterns--alist :entity response)))
           (linked (and response (arxana-browser-patterns--find-entity response entity-id)))
           (version-entity (or (arxana-browser-patterns--entity-from-version entity)
                               (arxana-browser-patterns--entity-from-version linked)))
           (candidates (delq nil (list entity version-entity linked))))
      (cl-labels ((value (&rest keys)
                    (seq-some (lambda (candidate)
                                (apply #'arxana-browser-patterns--entity-value candidate keys))
                              candidates)))
        (when (or candidates (plist-get (car candidates) :id))
          (list :source (or (value :source :entity/source) "")
                :external-id (value :external-id :entity/external-id)
                :name (value :name :entity/name)
                :id (or (value :id :entity/id) entity-id)))))))

(defun arxana-browser-patterns--fetch-component (link)
  (let* ((entity (arxana-browser-patterns--alist :entity link))
         (component-id (or (arxana-browser-patterns--alist :entity/id entity)
                           (arxana-browser-patterns--alist :id entity)))
         (component-name (or (arxana-browser-patterns--alist :entity/name entity)
                             (arxana-browser-patterns--alist :name entity)))
         (details (arxana-browser-patterns--fetch-entity-source component-id))
         (order-info (arxana-browser-patterns--component-name-info component-name))
         (parent-id (arxana-browser-patterns--component-parent-id component-name)))
    (when component-id
      (list :id component-id
            :name (or component-name (plist-get details :name) "component")
            :text (or (plist-get details :source) "")
            :order (plist-get order-info :order)
            :kind (plist-get order-info :kind)
            :parent-id parent-id))))

(defun arxana-browser-patterns--compute-levels (components)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (component components)
      (puthash (plist-get component :id)
               (copy-sequence component)
               table))
    (cl-labels ((level-of (component-id)
                  (let ((entry (gethash component-id table)))
                    (if (not entry)
                        0
                      (let ((existing (plist-get entry :level)))
                        (if existing
                            existing
                          (let* ((parent-id (plist-get entry :parent-id))
                                 (computed (if parent-id
                                               (1+ (level-of parent-id))
                                             0))
                                 (updated (plist-put entry :level computed)))
                            (puthash component-id updated table)
                            computed)))))))
      (mapcar (lambda (component)
                (let ((id (plist-get component :id)))
                  (level-of id)
                  (gethash id table)))
              components))))

(defun arxana-browser-patterns--fetch-pattern-data (name)
  (arxana-browser-patterns--ensure-sync)
  (let* ((ego-response (arxana-store-ego name arxana-browser-patterns-ego-limit))
         (ego (and ego-response (arxana-browser-patterns--alist :ego ego-response)))
         (entity (and ego (arxana-browser-patterns--pattern-entity ego)))
         (pattern-id (or (and entity (arxana-browser-patterns--alist :id entity))
                         (arxana-browser-patterns--alist :entity/id entity)))
         (pattern-details (arxana-browser-patterns--fetch-entity-source pattern-id))
         (summary (or (plist-get pattern-details :source) ""))
         (title (or (plist-get pattern-details :external-id) name))
         (component-links (arxana-browser-patterns--component-links ego))
         (component-entries (delq nil (mapcar #'arxana-browser-patterns--fetch-component
                                              component-links)))
         (leveled-components (arxana-browser-patterns--compute-levels component-entries))
         (components (cl-sort (copy-sequence leveled-components)
                              #'< :key (lambda (comp)
                                         (or (plist-get comp :order) 0)))))
    (unless pattern-id
      (user-error "Pattern %s was not found in Futon" name))
    (list :id pattern-id
          :name name
          :title title
          :summary summary
          :components components)))

(defun arxana-browser-patterns--insert-summary (summary)
  (insert arxana-browser-patterns--summary-begin "\n")
  (insert (string-trim (or summary "")) "\n")
  (insert arxana-browser-patterns--summary-end "\n\n"))

(defun arxana-browser-patterns--insert-component (component)
  (let* ((level (max 0 (or (plist-get component :level) 0)))
         (stars (make-string (max 1 (1+ level)) ?*))
         (label (capitalize (or (plist-get component :kind) "component")))
         (name (plist-get component :name))
         (component-id (plist-get component :id))
         (text (string-trim (or (plist-get component :text) ""))))
    (insert (format "%s %s\n" stars label))
    (insert ":PROPERTIES:\n")
    (insert (format ":COMPONENT-ID: %s\n" component-id))
    (insert (format ":COMPONENT-NAME: %s\n" name))
    (insert (format ":COMPONENT-KIND: %s\n" (plist-get component :kind)))
    (insert (format ":COMPONENT-ORDER: %s\n" (or (plist-get component :order) 0)))
    (insert (format ":COMPONENT-PARENT: %s\n"
                    (or (plist-get component :parent-id) "")))
    (insert ":END:\n\n")
    (insert text "\n\n")))

(defun arxana-browser-patterns--render-pattern (pattern)
  (let* ((name (plist-get pattern :name))
         (buffer (get-buffer-create (format "*Arxana Pattern: %s*" name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: Pattern %s\n" name))
        (insert (format "#+PATTERN: %s\n" name))
        (insert (format "#+PATTERN-ID: %s\n" (plist-get pattern :id)))
        (insert (format "#+PATTERN-TITLE: %s\n\n" (plist-get pattern :title)))
        (arxana-browser-patterns--insert-summary (plist-get pattern :summary))
        (dolist (component (plist-get pattern :components))
          (arxana-browser-patterns--insert-component component))
        (goto-char (point-min))
        (org-mode)
        (arxana-browser-patterns-view-mode 1)
        (setq-local arxana-browser-patterns--pattern pattern)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun arxana-browser-patterns-open-filesystem (&optional name-or-path)
  "Open the flexiarg file for NAME-OR-PATH in a regular buffer."
  (interactive (list (read-string "Pattern name or path: "
                                  (thing-at-point 'symbol t))))
  (let* ((path (if (and name-or-path (file-exists-p name-or-path))
                   name-or-path
                 (arxana-browser-patterns--pattern-file name-or-path))))
    (unless path
      (user-error "No flexiarg file found for %s" name-or-path))
    (arxana-flexiarg--ensure-flexiarg)
    (let ((buffer (find-file path)))
      (with-current-buffer buffer
        (let ((was-modified (buffer-modified-p)))
          (when (fboundp 'flexiarg-mode)
            (flexiarg-mode))
          (arxana-flexiarg-file-mode 1)
          (unless was-modified
            (setq-local arxana-flexiarg--last-sync-errors nil)
            (set-buffer-modified-p nil)
            (arxana-flexiarg--update-file-header-state))))
      (pop-to-buffer buffer)
      buffer)))

;;;###autoload
(defun arxana-browser-patterns-open (name)
  "Fetch the Futon pattern NAME and render it in an Org buffer."
  (interactive (list (read-string "Pattern name: " (thing-at-point 'symbol t))))
  (let* ((pattern (arxana-browser-patterns--fetch-pattern-data name))
         (file (arxana-browser-patterns--pattern-file name)))
    (if (and arxana-browser-patterns-prefer-filesystem
             (arxana-browser-patterns--filesystem-newer-p file
                                                         (plist-get pattern :last-seen)))
        (progn
          (message "Opening local flexiarg; it is newer than XTDB.")
          (arxana-browser-patterns-open-filesystem file))
      (arxana-browser-patterns--render-pattern pattern))))

(defun arxana-browser-patterns-inspect-entity (name)
  "Show the Futon source text for entity NAME (pattern or component)."
  (interactive (list (read-string "Entity name: " (thing-at-point 'symbol t))))
  (arxana-browser-patterns--ensure-sync)
  (let* ((entity (arxana-browser-patterns--resolve-entity-by-name name))
         (component (and (not entity)
                          (or (arxana-browser-patterns--lookup-component-by-name name)
                              (arxana-browser-patterns--lookup-component-by-prefix name))))
         (direct-id (and (not (or entity component))
                         (arxana-browser-patterns--fetch-entity-source name)))
         (details (cond
                    (entity
                     (let ((entity-id (or (arxana-browser-patterns--entity-value entity :id :entity/id)
                                          (arxana-browser-patterns--entity-value entity :ident :entity/ident))))
                       (unless entity-id
                         (user-error "Entity %s not found" name))
                       (arxana-browser-patterns--fetch-entity-source entity-id)))
                    (component component)
                    (direct-id direct-id)
                    (t nil))))
    (unless details
      (user-error "Entity %s was not found" name))
    (let* ((text (string-trim (or (plist-get details :text)
                                  (plist-get details :summary)
                                  (plist-get details :source)
                                  "")))
           (buffer (get-buffer-create "*Arxana Pattern Snippet*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Name: %s\n" (or (plist-get details :name) name)))
          (insert (format "Id: %s\n" (or (plist-get details :id) "?")))
          (when-let ((title (plist-get details :external-id)))
            (insert (format "Title: %s\n" title)))
          (when-let ((order (plist-get details :order)))
            (insert (format "Order: %s\n" order)))
          (when-let ((kind (plist-get details :kind)))
            (insert (format "Kind: %s\n" kind)))
          (insert "\n")
          (insert text)
          (goto-char (point-min))
          (view-mode 1)))
      (pop-to-buffer buffer))))

(defun arxana-browser-patterns-refresh-buffer ()
  "Re-fetch the current pattern from Futon and replace the buffer contents."
  (interactive)
  (unless (and (boundp 'arxana-browser-patterns--pattern)
               arxana-browser-patterns--pattern)
    (user-error "No pattern is loaded in this buffer"))
  (arxana-browser-patterns-open (plist-get arxana-browser-patterns--pattern :name)))

(defun arxana-browser-patterns--collect-components ()
  (let (results)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\*+ " nil t)
        (let ((component-id (org-entry-get (point) "COMPONENT-ID")))
          (when component-id
            (let* ((component-name (org-entry-get (point) "COMPONENT-NAME"))
                   (begin (save-excursion
                            (org-back-to-heading t)
                            (forward-line)
                            (while (looking-at "^[ \\t]*$\|^[ \\t]*:\\|^[ \\t]*#")
                              (forward-line))
                            (point)))
                   (end (save-excursion
                          (org-end-of-subtree t t)
                          (point)))
                   (text (if (and begin end)
                             (string-trim (buffer-substring-no-properties begin end))
                           "")))
              (push (list :id component-id
                          :name component-name
                          :text text)
                    results))))))
    (nreverse results)))

(defun arxana-browser-patterns-save ()
  "Sync the current pattern buffer back to Futon.
Only existing components are updated; new headings without component ids
are ignored for now."
  (interactive)
  (arxana-browser-patterns--ensure-sync)
  (unless (and (boundp 'arxana-browser-patterns--pattern)
               arxana-browser-patterns--pattern)
    (user-error "No pattern metadata found in this buffer"))
  (save-excursion
    (widen)
    (let* ((pattern-id (plist-get arxana-browser-patterns--pattern :id))
           (pattern-name (or (plist-get arxana-browser-patterns--pattern :name)
                             (arxana-browser-patterns--read-header-field "PATTERN")))
           (pattern-title (arxana-browser-patterns--read-header-field "PATTERN-TITLE"))
           (summary (arxana-browser-patterns--extract-summary))
           (components (arxana-browser-patterns--collect-components)))
      (arxana-store-ensure-entity :id pattern-id
                                  :name pattern-name
                                  :type "pattern/library"
                                  :source summary
                                  :external-id pattern-title)
      (dolist (component components)
        (let ((cid (plist-get component :id))
              (cname (plist-get component :name))
              (ctext (plist-get component :text)))
          (when (and cid cname)
            (arxana-store-ensure-entity :id cid
                                        :name cname
                                        :type "pattern/component"
                                        :source ctext))))
      (message "Synced %s (%d components)" pattern-name (length components)))))

(defun arxana-browser-patterns--browser-pattern-row (item)
  (let* ((order (plist-get item :order))
         (name (or (plist-get item :label) ""))
         (title (or (plist-get item :title) ""))
         (file (plist-get item :file)))
    (vector (if order (number-to-string order) "-")
            name
            (if file
                (file-name-nondirectory file)
              title))))

(defalias 'arxana-browser-patterns--menu-items #'arxana-browser--menu-items)

(defalias 'arxana-browser-patterns--code-items #'arxana-browser--code-items)

(defun arxana-browser-patterns--ensure-frame ()
  (or (seq-find (lambda (frame)
                  (and (frame-live-p frame)
                       (equal (frame-parameter frame 'name) arxana-browser-frame-name)))
                (frame-list))
      (let ((frame (make-frame `((name . ,arxana-browser-frame-name)))))
        (set-frame-parameter frame 'arxana-frame t)
        (when arxana-browser-frame-fullscreen
          (set-frame-parameter frame 'fullscreen 'fullboth))
        frame)))

(defun arxana-browser-patterns--normalize-order (value)
  (cond
   ((numberp value) value)
   ((stringp value)
    (when (string-match-p "\\`[0-9]+\\'" value)
      (string-to-number value)))
   (t nil)))

(defun arxana-browser-patterns--language-pattern-entry (link)
  (when (arxana-browser-patterns--relation-match-p
         (arxana-browser-patterns--alist :relation link)
         arxana-browser-patterns-ingest-language-relation)
    (let* ((entity (arxana-browser-patterns--alist :entity link))
           (pattern-id (arxana-browser-patterns--entity-value entity :id :entity/id))
           (name (or (arxana-browser-patterns--entity-value entity :name :entity/name)
                     (arxana-browser-patterns--entity-value entity :ident :entity/ident)))
           (title (or (arxana-browser-patterns--entity-value entity :external-id :entity/external-id)
                      (arxana-browser-patterns--entity-value entity :name :entity/name)))
           (order (or (arxana-browser-patterns--link-prop link :order)
                      (arxana-browser-patterns--link-prop link 'order))))
      (when name
        (list :type 'pattern
              :pattern-id pattern-id
              :label name
              :title title
              :order (arxana-browser-patterns--normalize-order order))))))

(defun arxana-browser-patterns--language-pattern-items (language-name)
  (arxana-browser-patterns--ensure-sync)
  (let* ((ego-response (ignore-errors (arxana-store-ego language-name arxana-browser-patterns-language-ego-limit)))
         (ego (and ego-response (arxana-browser-patterns--alist :ego ego-response)))
         (outgoing (arxana-browser-patterns--ego-outgoing ego))
         (items (delq nil (mapcar #'arxana-browser-patterns--language-pattern-entry outgoing))))
    (seq-sort (lambda (a b)
                (let ((oa (or (plist-get a :order) most-positive-fixnum))
                      (ob (or (plist-get b :order) most-positive-fixnum)))
                  (if (/= oa ob)
                      (< oa ob)
                    (string< (or (plist-get a :label) "")
                             (or (plist-get b :label) "")))))
              items)))

(defun arxana-browser-patterns--filesystem-pattern-items (library)
  (when library
    (let* ((files (or (plist-get library :files)
                      (arxana-browser-patterns--flexiarg-files-for (plist-get library :label))))
           (results nil))
      (dolist (file files)
        (dolist (entry (arxana-browser-patterns--parse-flexiarg file))
          (let ((order (plist-get entry :order)))
            (push (list :type 'pattern
                        :label (plist-get entry :name)
                        :title (plist-get entry :title)
                        :file file
                        :order order)
                  results))))
      (setq results
            (sort results
                  (lambda (a b)
                    (let ((oa (or (plist-get a :order) most-positive-fixnum))
                          (ob (or (plist-get b :order) most-positive-fixnum)))
                      (if (/= oa ob)
                          (< oa ob)
                        (string< (or (plist-get a :label) "")
                                 (or (plist-get b :label) "")))))))
      (let ((counter 1))
        (dolist (entry results)
          (setf (plist-get entry :order) counter)
          (setq counter (1+ counter))))
      results)))

(defun arxana-browser-patterns--browser-pattern-items (language)
  (when language
    (pcase (plist-get language :type)
      ('language
       (arxana-browser-patterns--language-pattern-items (plist-get language :label)))
      ('collection
       (arxana-browser-patterns--filesystem-pattern-items language))
      (_ nil))))

(defun arxana-browser-patterns--move-entry (entries old-index new-index)
  (let* ((len (length entries))
         (new-index (max 0 (min new-index (1- len))))
         (elem (nth old-index entries))
         (rest (cl-remove elem entries :count 1 :test #'eq)))
    (append (cl-subseq rest 0 new-index)
            (list elem)
            (nthcdr new-index rest))))

(defun arxana-browser-patterns--flexiarg-apply-order (items)
  (let ((n 1))
    (dolist (entry items)
      (let ((file (plist-get entry :file)))
        (when file
          (arxana-browser-patterns--flexiarg-set-order file n)))
      (setq n (1+ n)))))

(defun arxana-browser-patterns--language-entity-id (language)
  (or (plist-get language :entity-id)
      (let* ((name (plist-get language :label))
             (entity (and name (arxana-browser-patterns--resolve-entity-by-name name))))
        (and entity (arxana-browser-patterns--entity-value entity :id :entity/id)))))

(defun arxana-browser-patterns--pattern-entry-id (entry)
  (or (plist-get entry :pattern-id)
      (let* ((name (plist-get entry :label))
             (entity (and name (arxana-browser-patterns--resolve-entity-by-name name))))
        (and entity (arxana-browser-patterns--entity-value entity :id :entity/id)))))

(defun arxana-browser-patterns--language-apply-order (language items)
  (arxana-browser-patterns--ensure-sync)
  (let ((language-id (arxana-browser-patterns--language-entity-id language)))
    (unless language-id
      (user-error "Could not determine Futon id for %s" (plist-get language :label)))
    (let ((n 1)
          (updated 0))
      (dolist (entry items)
        (let ((pattern-id (arxana-browser-patterns--pattern-entry-id entry)))
          (unless pattern-id
            (user-error "Could not determine Futon id for pattern %s" (plist-get entry :label)))
          (arxana-store-create-relation :src language-id
                                        :dst pattern-id
                                        :label arxana-browser-patterns-ingest-language-relation
                                        :props (list (cons 'order n)))
          (setq updated (1+ updated)))
        (setq n (1+ n)))
      (message "Updated %d relations for %s" updated (plist-get language :label)))))

(defun arxana-browser-patterns--slugify (text)
  (when text
    (let* ((lower (downcase text))
           (clean (replace-regexp-in-string "[^a-z0-9]+" "-" lower)))
      (replace-regexp-in-string "-+" "-" (string-trim clean "-")))))

(defun arxana-browser-patterns--library-default-language (library)
  (let* ((label (or (plist-get library :label) "library"))
         (title (or (plist-get library :title) label))
         (slug (or (arxana-browser-patterns--slugify label) "library")))
    (list :name (format "pattern-language/%s" slug)
          :title title)))

(defun arxana-browser-patterns-add-collection-root (directory)
  "Add DIRECTORY as an ad-hoc pattern collection root."
  (interactive "DCollection directory: ")
  (let ((path (arxana-browser-patterns--normalize-path directory)))
    (unless (file-directory-p path)
      (user-error "Directory %s does not exist" path))
    (arxana-browser-patterns--register-collection-root path)
    (when (get-buffer arxana-browser--buffer)
      (arxana-browser--render))))

(defun arxana-browser-patterns--import-library (library &optional prompt)
  (arxana-browser-patterns--ensure-sync)
  (let ((dir (or (plist-get library :directory)
                 (let* ((label (plist-get library :label))
                        (files (and label (arxana-browser-patterns--flexiarg-files-for label))))
                   (and files (file-name-directory (car files)))))))
    (unless (and dir (file-directory-p dir))
      (user-error "Library directory %s is missing" dir))
    (let* ((defaults (arxana-browser-patterns--library-default-language library))
           (label (or (plist-get library :label)
                      (file-name-nondirectory (directory-file-name dir))))
           (language-name (if prompt
                              (read-string "Pattern language name: " (plist-get defaults :name))
                            (plist-get defaults :name)))
           (language-title (if prompt
                               (read-string "Pattern language title: " (plist-get defaults :title))
                             (plist-get defaults :title)))
           (language-status
            (if prompt
                (let* ((default-status (arxana-browser-patterns-ingest--language-status-name dir nil))
                       (input (string-trim
                               (read-string (format "Pattern language status (default %s): "
                                                    default-status)
                                            nil nil default-status))))
                  (if (string-empty-p input)
                      default-status
                    input))
              (arxana-browser-patterns-ingest--language-status-name dir nil))))
      (unless (and language-name (not (string-empty-p language-name)))
        (user-error "Pattern language name is required to import"))
      (arxana-browser-patterns-ingest-directory dir language-name language-title language-status)
      (arxana-browser-patterns--register-collection-root dir t)
      (arxana-browser-patterns--write-library-metadata dir language-name language-title language-status)
      (message "Imported %s into %s" label language-name)
      language-name)))

(defun arxana-browser-patterns-import-all-collections ()
  "Ingest every flexiarg collection under known roots."
  (interactive)
  (unless (arxana-store-sync-enabled-p)
    (user-error "Futon sync is disabled; enable futon4-enable-sync first"))
  (let ((collections (arxana-browser-patterns--filesystem-collection-items)))
    (unless collections
      (user-error "No collections were found under the active roots"))
    (when (yes-or-no-p (format "Import %d collections into Futon?" (length collections)))
      (let ((imported 0))
        (dolist (collection collections)
          (when (arxana-browser-patterns--import-library collection)
            (setq imported (1+ imported))))
        (message "Imported %d collections" imported)
        (with-current-buffer (get-buffer-create arxana-browser--buffer)
          (setq arxana-browser--stack nil
                arxana-browser--context nil))
        (arxana-browser--render)))))

(defalias 'arxana-browser-patterns-import-all-libraries #'arxana-browser-patterns-import-all-collections)

(provide 'arxana-browser-patterns)
;;; arxana-browser-patterns.el ends here
