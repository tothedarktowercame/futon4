;;; arxana-patterns.el --- Futon pattern importer/editor -*- lexical-binding: t; -*-

;;; Commentary:
;; Fetch pattern-library entries from Futon1 (patterns ingested from Futon3) and
;; render them as editable Org buffers.  Each buffer exposes the pattern summary
;; and component passages so Emacs users can review and update pattern text
;; without dropping into the Futon CLI.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'org)
(require 'org-element)
(require 'tabulated-list)

(require 'arxana-store)
(require 'arxana-patterns-ingest)
(require 'arxana-docbook)
(require 'arxana-lab)
(require 'arxana-media)

(defvar flexiarg-mode-map nil)

(defgroup arxana-patterns nil
  "Utilities for browsing Futon pattern entities in Emacs."
  :group 'arxana)

(defcustom arxana-patterns-ego-limit 64
  "Number of neighbor links to request when fetching Futon pattern data."
  :type 'integer
  :group 'arxana-patterns)

(defcustom arxana-patterns-language-ego-limit 256
  "Neighbor limit to use when listing pattern languages and their members."
  :type 'integer
  :group 'arxana-patterns)

(defcustom arxana-patterns-library-root nil
  "Path to the Futon3 pattern library checkout.
When nil the browser attempts to locate a \"futon3/library\" directory
relative to the current buffer or this file."
  :type '(choice (const :tag "Auto-detect" nil)
                 directory)
  :group 'arxana-patterns)

(defcustom arxana-patterns-collection-roots-file
  (locate-user-emacs-file "arxana-collection-roots.el")
  "File used to remember additional collection roots across Emacs sessions.
Set to nil to disable persistence."
  :type '(choice (const :tag "Do not persist" nil)
                 file)
  :group 'arxana-patterns)

(defvar-local arxana-patterns--pattern nil
  "Buffer-local plist describing the currently loaded pattern.")

(defconst arxana-patterns--summary-begin "#+BEGIN_SUMMARY")
(defconst arxana-patterns--summary-end "#+END_SUMMARY")

(defface arxana-patterns-browser-highlight
  '((t :inherit hl-line :background "#61CE3C"))
  "Face used to highlight the active row in the pattern browser."
  :group 'arxana-patterns)

(defface arxana-patterns-docbook-latest-face
  '((t :foreground "#7fdc7f"))
  "Face used for docbook headings with recent entries."
  :group 'arxana-patterns)

(defface arxana-patterns-docbook-empty-face
  '((t :foreground "#d7b46a"))
  "Face used for docbook headings without recent entries."
  :group 'arxana-patterns)

(defface arxana-patterns-docbook-unindexed-face
  '((t :foreground "#e3a86e"))
  "Face used for docbook headings missing from the TOC."
  :group 'arxana-patterns)

(defconst arxana-patterns--browser-click-default
  (let* ((dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (expand-file-name "../resources/sounds/arxana-click.wav" dir))
  "Default click sound distributed with Futon4.")

(defcustom arxana-patterns-browser-enable-click t
  "When non-nil, play a quiet click while moving within the pattern browser."
  :type 'boolean
  :group 'arxana-patterns)

(defcustom arxana-patterns-browser-click-sound arxana-patterns--browser-click-default
  "Audio file used for the pattern browser navigation click.
Set to nil to disable the bundled sound without turning off clicks entirely."
  :type '(choice (const :tag "No sound" nil)
                 file)
  :group 'arxana-patterns)

(defcustom arxana-patterns-browser-click-volume 0.25
  "Volume multiplier (0.0–1.0) for the navigation click."
  :type 'number
  :group 'arxana-patterns)

(defcustom arxana-patterns-browser-wheel-step 1
  "Number of entries to move per wheel or trackpad gesture."
  :type 'integer
  :group 'arxana-patterns)

(defcustom arxana-patterns-docbook-top-order
  '("Overview"
    "Quickstart"
    "Recent changes (futon4, pilot)"
    "Storage bridge"
    "Pattern workflows"
    "Browsing & relations"
    "Org imports / exports & snapshots"
    "Inclusion / derivation UX"
    "Article lifecycle"
    "Compatibility & test support"
    "Contributor guide (embedded)"
    "QA checklist"
    "Known limitations")
  "Preferred ordering for top-level docbook headings in the contents view."
  :type '(repeat string)
  :group 'arxana-patterns)

(defun arxana-patterns--browser-click-path ()
  (let ((path arxana-patterns-browser-click-sound))
    (when (and path (file-readable-p path))
      path)))

(defun arxana-patterns--play-click ()
  (let ((path (arxana-patterns--browser-click-path)))
    (when (and arxana-patterns-browser-enable-click
               path
               (display-graphic-p)
               (fboundp 'play-sound))
      (ignore-errors
        (play-sound `(sound :file ,path :volume ,(max 0.0 (min 1.0 arxana-patterns-browser-click-volume))))))))

(if (boundp 'org-src-lang-modes)
    (add-to-list 'org-src-lang-modes '("flexiarg" . flexiarg) t)
  (with-eval-after-load 'org
    (add-to-list 'org-src-lang-modes '("flexiarg" . flexiarg) t)))

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

(with-eval-after-load 'flexiarg
  (arxana-flexiarg--ensure-parent-map))

(defun arxana-patterns--locate-library-root ()
  (let ((explicit arxana-patterns-library-root))
    (cond
     ((and explicit (file-directory-p explicit)) (expand-file-name explicit))
     (t
      (let* ((current (or load-file-name buffer-file-name default-directory))
             (root (locate-dominating-file current "futon3")))
        (when root
          (let ((candidate (expand-file-name "futon3/library" root)))
            (and (file-directory-p candidate) candidate))))))))

(defun arxana-patterns--library-directories (&optional root)
  (let ((root (or root (arxana-patterns--locate-library-root))))
    (when root
      (seq-sort #'string<
                (seq-filter
                 (lambda (entry)
                   (let ((full (expand-file-name entry root)))
                     (and (file-directory-p full)
                          (not (member entry '("." ".."))))))
                 (directory-files root))))))

(defun arxana-patterns--flexiarg-files-in-directory (dir)
  (when (and dir (file-directory-p dir))
    (seq-sort #'string<
              (seq-filter #'file-regular-p
                          (directory-files dir t "\\.flexiarg\\'")))))

(defun arxana-patterns--flexiarg-files-for (library &optional root)
  (let* ((root (or root (arxana-patterns--locate-library-root)))
         (dir (and root (expand-file-name library root))))
    (arxana-patterns--flexiarg-files-in-directory dir)))

(defun arxana-patterns--normalize-path (path)
  (when path
    (file-name-as-directory (expand-file-name path))))

(defvar arxana-patterns--additional-collection-roots nil
  "Session-local list of ad-hoc pattern collection directories.")

(defconst arxana-patterns--library-metadata-file ".arxana-language"
  "Marker file recording Futon language details for a library directory.")

(defun arxana-patterns--library-metadata-path (directory)
  (expand-file-name arxana-patterns--library-metadata-file
                    (file-name-as-directory directory)))

(defun arxana-patterns--write-library-metadata (directory language-name language-title status)
  "Persist LANGUAGE-NAME metadata for DIRECTORY so the browser can detect imports.
LANGUAGE-TITLE and STATUS are optional strings recorded for display."
  (when (and directory language-name)
    (let ((path (arxana-patterns--library-metadata-path directory))
          (payload (list :language-name language-name
                         :language-title language-title
                         :status status
                         :directory (arxana-patterns--normalize-path directory)
                         :updated-at (float-time (current-time)))))
      (with-temp-file path
        (let ((print-length nil)
              (print-level nil))
          (prin1 payload (current-buffer)))))))

(defun arxana-patterns--read-library-metadata (directory)
  "Return the metadata plist stored for DIRECTORY, or nil."
  (let ((path (arxana-patterns--library-metadata-path directory)))
    (when (file-readable-p path)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-min))
            (read (current-buffer)))
        (error nil)))))

(defvar arxana-patterns--persisted-collection-roots-loaded nil)
(defvar arxana-patterns--persisted-collection-roots nil)

(defun arxana-patterns--load-persisted-collection-roots ()
  (unless arxana-patterns--persisted-collection-roots-loaded
    (setq arxana-patterns--persisted-collection-roots-loaded t)
    (let* ((path arxana-patterns-collection-roots-file)
           (raw (when (and path (file-readable-p path))
                  (condition-case nil
                      (with-temp-buffer
                        (insert-file-contents path)
                        (goto-char (point-min))
                        (read (current-buffer)))
                    (error nil)))))
      (setq arxana-patterns--persisted-collection-roots
            (mapcar #'arxana-patterns--normalize-path (delq nil raw)))))
  (delq nil arxana-patterns--persisted-collection-roots))

(defun arxana-patterns--persist-collection-roots ()
  (let ((path arxana-patterns-collection-roots-file))
    (when path
      (make-directory (file-name-directory path) t)
      (with-temp-file path
        (let ((print-length nil)
              (print-level nil))
          (prin1 (delq nil arxana-patterns--persisted-collection-roots)
                 (current-buffer)))))))

(defun arxana-patterns--remember-collection-root (directory)
  (let ((path (arxana-patterns--normalize-path directory)))
    (when path
      (arxana-patterns--load-persisted-collection-roots)
      (unless (member path arxana-patterns--persisted-collection-roots)
        (setq arxana-patterns--persisted-collection-roots
              (cons path arxana-patterns--persisted-collection-roots))
        (arxana-patterns--persist-collection-roots)))))

(defun arxana-patterns--collection-root-paths ()
  "Return a de-duplicated list of active collection roots."
  (let* ((canonical (arxana-patterns--locate-library-root))
         (persisted (arxana-patterns--load-persisted-collection-roots))
         (roots (append (list canonical)
                        persisted
                        arxana-patterns--additional-collection-roots)))
    (seq-uniq (delq nil (mapcar #'arxana-patterns--normalize-path roots)))))

(defun arxana-patterns--collection-directories (root)
  "Return directories under ROOT that contain `.flexiarg` files.
When ROOT itself has flexiarg files, include it as `\".\"`."
  (let ((dirs (or (arxana-patterns--library-directories root) '())))
    (if (arxana-patterns--flexiarg-files-in-directory root)
        (cons "." dirs)
      dirs)))

(defun arxana-patterns--collection-from-directory (directory)
  "Return a collection plist for DIRECTORY."
  (let* ((dir (arxana-patterns--normalize-path directory))
         (files (arxana-patterns--flexiarg-files-in-directory dir)))
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

(defun arxana-patterns--primary-order-for-file (file)
  (when (and file (file-readable-p file))
    (let* ((entries (arxana-patterns--parse-flexiarg file))
           (first (car entries)))
      (plist-get first :order))))

(defun arxana-patterns--sort-files-by-order (files)
  (let ((copy (copy-sequence files)))
    (sort copy
          (lambda (a b)
            (let ((oa (or (arxana-patterns--primary-order-for-file a)
                          most-positive-fixnum))
                  (ob (or (arxana-patterns--primary-order-for-file b)
                          most-positive-fixnum)))
              (if (/= oa ob)
                  (< oa ob)
                (string< a b)))))))

(defun arxana-patterns--register-collection-root (directory &optional quiet)
  (let ((path (arxana-patterns--normalize-path directory)))
    (when path
      (unless (member path arxana-patterns--additional-collection-roots)
        (push path arxana-patterns--additional-collection-roots))
      (arxana-patterns--remember-collection-root path)
      (unless quiet
        (message "Added collection root %s" path))
      path)))

(defun arxana-patterns--read-file (file)
  "Return the contents of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun arxana-patterns--insert-flexiarg-section (file)
  "Insert an Org heading that embeds FILE."
  (let ((title (file-name-nondirectory file)))
    (insert (format "* %s\n" title))
    (insert ":PROPERTIES:\n")
    (insert (format ":FILE: %s\n" file))
    (insert ":END:\n\n")
    (insert "#+BEGIN_SRC flexiarg\n")
    (insert (arxana-patterns--read-file file))
    (unless (bolp)
      (insert "\n"))
    (insert "#+END_SRC\n\n")))

(defun arxana-patterns--friendly-classification (value)
  (cond
   ((null value) nil)
   ((stringp value)
    (if (string-match "/\\([^/]+\\)\\'" value)
        (match-string 1 value)
      value))
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun arxana-patterns--status-label (status &optional cached)
  (let ((base (or (arxana-patterns--friendly-classification status)
                  "imported")))
    (if cached
        (format "%s (cached)" base)
      (format "imported/%s" base))))

(defun arxana-patterns--imported-status-label (language)
  (arxana-patterns--status-label (plist-get language :status)))

(defun arxana-patterns--metadata-status-label (metadata)
  (arxana-patterns--status-label (plist-get metadata :status) t))

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

(defun arxana-patterns--flexiarg-set-order (file order)
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
          (insert (arxana-patterns--read-file file))
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

(defun arxana-patterns--language-index-by-path (language-rows)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (row language-rows table)
      (when-let* ((raw (plist-get row :import-path))
                  (path (arxana-patterns--normalize-path raw)))
        (puthash path row table)))))

(defun arxana-patterns--parse-flexiarg (file)
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
         (list :name (arxana-patterns-ingest--derive-name-from-path file)
               :order nil))))))

(defun arxana-patterns--filesystem-collection-items (&optional language-index)
  (let (items)
    (dolist (root (arxana-patterns--collection-root-paths))
      (when (file-directory-p root)
        (dolist (dir (arxana-patterns--collection-directories root))
          (let* ((actual-dir (if (string= dir ".")
                                 root
                               (expand-file-name dir root)))
                 (files (arxana-patterns--flexiarg-files-in-directory actual-dir)))
            (when files
              (let* ((count (length files))
                     (abs-dir (arxana-patterns--normalize-path actual-dir))
                     (language (and language-index
                                    abs-dir
                                    (gethash abs-dir language-index)))
                     (metadata (arxana-patterns--read-library-metadata abs-dir)))
                (when (and language (not metadata))
                  (arxana-patterns--write-library-metadata abs-dir
                                                           (plist-get language :name)
                                                           (plist-get language :title)
                                                           (plist-get language :status))
                  (setq metadata (arxana-patterns--read-library-metadata abs-dir)))
                (let* ((status (cond
                                (language (arxana-patterns--imported-status-label language))
                                (metadata (arxana-patterns--metadata-status-label metadata))
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

(defun arxana-patterns-edit-collection (&optional collection)
  "Edit every `.flexiarg` file in COLLECTION inside a single flexiarg buffer.
When called interactively with point on a collection row inside the browser,
use that entry; otherwise prompt for a directory."
  (interactive)
  (let* ((collection
          (or collection
              (and (derived-mode-p 'arxana-patterns-browser-mode)
                   (let ((item (arxana-patterns--browser-item-at-point)))
                     (and item (eq (plist-get item :type) 'collection) item)))
              (arxana-patterns--collection-from-directory
               (read-directory-name "Collection directory: " nil nil t))))
         (files (arxana-patterns--sort-files-by-order
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

(defvar arxana-patterns-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'arxana-patterns-save)
    (define-key map (kbd "g") #'arxana-patterns-refresh-buffer)
    map)
  "Keymap for `arxana-patterns-view-mode'.")

(define-minor-mode arxana-patterns-view-mode
  "Minor mode for pattern editing buffers."
  :lighter " Pattern"
  :keymap arxana-patterns-view-mode-map
  (when arxana-patterns-view-mode
    (setq header-line-format "C-c C-s to sync changes; g to refetch from Futon")))

(defun arxana-patterns--ensure-sync ()
  (unless (arxana-store-sync-enabled-p)
    (user-error "Futon sync is disabled; enable futon4-enable-sync first")))

(defun arxana-patterns--alist (key alist)
  (alist-get key alist))

(defun arxana-patterns--alist-like-p (value)
  (and (listp value)
       (let ((first (car-safe value)))
         (and first (consp first)))))

(defun arxana-patterns--entity-value (entity &rest keys)
  "Return the first matching value in ENTITY for the provided :entity/* KEYS."
  (when (arxana-patterns--alist-like-p entity)
    (seq-some (lambda (key)
                (let ((cell (assoc key entity)))
                  (when cell (cdr cell))))
              keys)))

(defun arxana-patterns--entity-from-version (entity)
  "Return the entity payload stored inside ENTITY's version data, if any."
  (when (arxana-patterns--alist-like-p entity)
    (let* ((version (arxana-patterns--alist :version entity))
           (data (and (arxana-patterns--alist-like-p version)
                      (arxana-patterns--alist :data version)))
           (payload (and (arxana-patterns--alist-like-p data)
                         (arxana-patterns--alist :entity data))))
      (and (arxana-patterns--alist-like-p payload) payload))))

(defun arxana-patterns--find-entity (tree target-id)
  "Locate the entity with TARGET-ID inside TREE (direct or linked payloads)."
  (let ((match nil))
    (cl-labels ((entity-matches-p (entity)
                  (let ((id (arxana-patterns--entity-value entity :id :entity/id)))
                    (and id target-id (equal id target-id))))
                (walk (node)
                  (when (and node (not match))
                    (cond
                     ((arxana-patterns--alist-like-p node)
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

(defun arxana-patterns--pattern-entity (ego)
  (arxana-patterns--alist :entity ego))

(defun arxana-patterns--resolve-entity-by-name (name)
  "Return the Futon entity alist for NAME via `/ego`."
  (let* ((ego-response (arxana-store-ego name 1))
         (ego (and ego-response (arxana-patterns--alist :ego ego-response))))
    (and ego (arxana-patterns--alist :entity ego))))

(defun arxana-patterns--relation-text (value)
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

(defun arxana-patterns--relation-match-p (value target)
  "Return non-nil when VALUE (keyword/string) matches TARGET (string/keyword)."
  (let ((lhs (arxana-patterns--relation-text value))
        (rhs (arxana-patterns--relation-text target)))
    (and lhs rhs (string= lhs rhs))))

(defun arxana-patterns--ego-outgoing (ego)
  "Return normalized outgoing link list for EGO nodes."
  (when (arxana-patterns--alist-like-p ego)
    (or (arxana-patterns--alist :outgoing ego)
        (let ((links (arxana-patterns--alist :links ego)))
          (and (arxana-patterns--alist-like-p links)
               (arxana-patterns--alist :outgoing links))))))

(defun arxana-patterns--link-prop (link key)
  "Return LINK property identified by KEY (keyword/string)."
  (let* ((props (or (arxana-patterns--alist :props link)
                    (arxana-patterns--alist 'props link)
                    (arxana-patterns--alist :properties link)
                    (arxana-patterns--alist 'properties link)))
         (target (arxana-patterns--relation-text key)))
    (when (and props target)
      (let ((match (seq-find (lambda (entry)
                               (and (consp entry)
                                    (let ((key-text (arxana-patterns--relation-text (car entry))))
                                      (and key-text (string= key-text target)))))
                             props)))
        (cdr match)))))

(defun arxana-patterns--component-links (ego)
  (when (arxana-patterns--alist-like-p ego)
    (let ((outgoing (arxana-patterns--ego-outgoing ego)))
      (seq-filter
       (lambda (entry)
         (arxana-patterns--relation-match-p
          (arxana-patterns--alist :relation entry)
          ":pattern/includes"))
       outgoing))))

(defun arxana-patterns--component-slug-base (name)
  (when (and name (string-match "\\`\\(.+\\)/[0-9]+-[^/]+\\'" name))
    (match-string 1 name)))

(defun arxana-patterns--component-link-name (link)
  (let ((entity (arxana-patterns--alist :entity link)))
    (or (arxana-patterns--entity-value entity :name :entity/name)
        (arxana-patterns--entity-value entity :ident :entity/ident))))

(defun arxana-patterns--lookup-component-by-name (component-name)
  (let ((base (arxana-patterns--component-slug-base component-name)))
    (when base
      (let* ((ego-response (arxana-store-ego base arxana-patterns-ego-limit))
             (ego (and ego-response (arxana-patterns--alist :ego ego-response)))
             (links (and (arxana-patterns--alist-like-p ego)
                         (arxana-patterns--component-links ego)))
             (matching (and links
                            (cl-find-if (lambda (link)
                                          (string= (arxana-patterns--component-link-name link)
                                                   component-name))
                                        links))))
        (when matching
          (arxana-patterns--fetch-component matching))))))

(defun arxana-patterns--lookup-component-by-prefix (component-name)
  (let ((base (arxana-patterns--component-slug-base component-name)))
    (when base
      (let* ((ego-response (arxana-store-ego base arxana-patterns-ego-limit))
             (ego (and ego-response (arxana-patterns--alist :ego ego-response)))
             (links (and (arxana-patterns--alist-like-p ego)
                        (arxana-patterns--component-links ego)))
             (matching (and links
                            (cl-find-if (lambda (link)
                                          (let ((name (arxana-patterns--component-link-name link)))
                                            (and name (string-prefix-p name component-name))))
                                        links))))
        (when matching
          (arxana-patterns--fetch-component matching))))))

(defun arxana-patterns--extract-summary ()
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^" (regexp-quote arxana-patterns--summary-begin) "\\s-*$") nil t)
          (let ((start (progn (forward-line 1) (point))))
            (if (re-search-forward (concat "^" (regexp-quote arxana-patterns--summary-end) "\\s-*$") nil t)
                (string-trim (buffer-substring-no-properties start (match-beginning 0)))
              ""))
        ""))))

(defun arxana-patterns--read-header-field (label)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t)
          (rx (format "^#\\+%s: \\(.*\\)$" (regexp-quote label))))
      (when (re-search-forward rx nil t)
        (string-trim (match-string 1))))))

(defun arxana-patterns--component-name-info (name)
  (if (and name (string-match "/\\([0-9]+\\)-\\([^/]+\\)$" name))
      (list :order (string-to-number (match-string 1 name))
            :kind (match-string 2 name))
    (list :order 0 :kind (or name "component"))))

(defun arxana-patterns--component-parent-id (component-name)
  (when component-name
    (let* ((ego-response (ignore-errors (arxana-store-ego component-name arxana-patterns-ego-limit)))
           (ego (and ego-response (arxana-patterns--alist :ego ego-response)))
           (incoming (and ego (arxana-patterns--alist :incoming ego)))
           (parent-link (and incoming
                             (cl-find-if (lambda (entry)
                                           (arxana-patterns--relation-match-p
                                            (arxana-patterns--alist :relation entry)
                                            ":pattern/component-parent"))
                                         incoming))))
      (when parent-link
        (let* ((entity (arxana-patterns--alist :entity parent-link)))
          (or (arxana-patterns--alist :entity/id entity)
              (arxana-patterns--alist :id entity)))))))

(defun arxana-patterns--fetch-entity-source (entity-id)
  (when entity-id
    (let* ((response (ignore-errors (arxana-store-fetch-entity entity-id)))
           (entity (and response (arxana-patterns--alist :entity response)))
           (linked (and response (arxana-patterns--find-entity response entity-id)))
           (version-entity (or (arxana-patterns--entity-from-version entity)
                               (arxana-patterns--entity-from-version linked)))
           (candidates (delq nil (list entity version-entity linked))))
      (cl-labels ((value (&rest keys)
                    (seq-some (lambda (candidate)
                                (apply #'arxana-patterns--entity-value candidate keys))
                              candidates)))
        (when (or candidates (plist-get (car candidates) :id))
          (list :source (or (value :source :entity/source) "")
                :external-id (value :external-id :entity/external-id)
                :name (value :name :entity/name)
                :id (or (value :id :entity/id) entity-id)))))))

(defun arxana-patterns--fetch-component (link)
  (let* ((entity (arxana-patterns--alist :entity link))
         (component-id (or (arxana-patterns--alist :entity/id entity)
                           (arxana-patterns--alist :id entity)))
         (component-name (or (arxana-patterns--alist :entity/name entity)
                             (arxana-patterns--alist :name entity)))
         (details (arxana-patterns--fetch-entity-source component-id))
         (order-info (arxana-patterns--component-name-info component-name))
         (parent-id (arxana-patterns--component-parent-id component-name)))
    (when component-id
      (list :id component-id
            :name (or component-name (plist-get details :name) "component")
            :text (or (plist-get details :source) "")
            :order (plist-get order-info :order)
            :kind (plist-get order-info :kind)
            :parent-id parent-id))))

(defun arxana-patterns--compute-levels (components)
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

(defun arxana-patterns--fetch-pattern-data (name)
  (arxana-patterns--ensure-sync)
  (let* ((ego-response (arxana-store-ego name arxana-patterns-ego-limit))
         (ego (and ego-response (arxana-patterns--alist :ego ego-response)))
         (entity (and ego (arxana-patterns--pattern-entity ego)))
         (pattern-id (or (and entity (arxana-patterns--alist :id entity))
                         (arxana-patterns--alist :entity/id entity)))
         (pattern-details (arxana-patterns--fetch-entity-source pattern-id))
         (summary (or (plist-get pattern-details :source) ""))
         (title (or (plist-get pattern-details :external-id) name))
         (component-links (arxana-patterns--component-links ego))
         (component-entries (delq nil (mapcar #'arxana-patterns--fetch-component
                                              component-links)))
         (leveled-components (arxana-patterns--compute-levels component-entries))
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

(defun arxana-patterns--insert-summary (summary)
  (insert arxana-patterns--summary-begin "\n")
  (insert (string-trim (or summary "")) "\n")
  (insert arxana-patterns--summary-end "\n\n"))

(defun arxana-patterns--insert-component (component)
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

(defun arxana-patterns--render-pattern (pattern)
  (let* ((name (plist-get pattern :name))
         (buffer (get-buffer-create (format "*Arxana Pattern: %s*" name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "#+TITLE: Pattern %s\n" name))
        (insert (format "#+PATTERN: %s\n" name))
        (insert (format "#+PATTERN-ID: %s\n" (plist-get pattern :id)))
        (insert (format "#+PATTERN-TITLE: %s\n\n" (plist-get pattern :title)))
        (arxana-patterns--insert-summary (plist-get pattern :summary))
        (dolist (component (plist-get pattern :components))
          (arxana-patterns--insert-component component))
        (goto-char (point-min))
        (org-mode)
        (arxana-patterns-view-mode 1)
        (setq-local arxana-patterns--pattern pattern)))
    (pop-to-buffer buffer)))

;;;###autoload
(defun arxana-patterns-open (name)
  "Fetch the Futon pattern NAME and render it in an Org buffer."
  (interactive (list (read-string "Pattern name: " (thing-at-point 'symbol t))))
  (let ((pattern (arxana-patterns--fetch-pattern-data name)))
    (arxana-patterns--render-pattern pattern)))

;;;###autoload
(defun arxana-patterns-inspect-entity (name)
  "Show the Futon source text for entity NAME (pattern or component)."
  (interactive (list (read-string "Entity name: " (thing-at-point 'symbol t))))
  (arxana-patterns--ensure-sync)
  (let* ((entity (arxana-patterns--resolve-entity-by-name name))
         (component (and (not entity)
                          (or (arxana-patterns--lookup-component-by-name name)
                              (arxana-patterns--lookup-component-by-prefix name))))
         (direct-id (and (not (or entity component))
                         (arxana-patterns--fetch-entity-source name)))
         (details (cond
                    (entity
                     (let ((entity-id (or (arxana-patterns--entity-value entity :id :entity/id)
                                          (arxana-patterns--entity-value entity :ident :entity/ident))))
                       (unless entity-id
                         (user-error "Entity %s not found" name))
                       (arxana-patterns--fetch-entity-source entity-id)))
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

(defun arxana-patterns-refresh-buffer ()
  "Re-fetch the current pattern from Futon and replace the buffer contents."
  (interactive)
  (unless (and (boundp 'arxana-patterns--pattern)
               arxana-patterns--pattern)
    (user-error "No pattern is loaded in this buffer"))
  (arxana-patterns-open (plist-get arxana-patterns--pattern :name)))

(defun arxana-patterns--collect-components ()
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

(defun arxana-patterns-save ()
  "Sync the current pattern buffer back to Futon.
Only existing components are updated; new headings without component ids
are ignored for now."
  (interactive)
  (arxana-patterns--ensure-sync)
  (unless (and (boundp 'arxana-patterns--pattern)
               arxana-patterns--pattern)
    (user-error "No pattern metadata found in this buffer"))
  (save-excursion
    (widen)
    (let* ((pattern-id (plist-get arxana-patterns--pattern :id))
           (pattern-name (or (plist-get arxana-patterns--pattern :name)
                             (arxana-patterns--read-header-field "PATTERN")))
           (pattern-title (arxana-patterns--read-header-field "PATTERN-TITLE"))
           (summary (arxana-patterns--extract-summary))
           (components (arxana-patterns--collect-components)))
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

(defvar arxana-patterns--browser-buffer "*Arxana Browser*")
(defvar-local arxana-patterns--browser-stack nil)
(put 'arxana-patterns--browser-stack 'permanent-local t)
(defvar-local arxana-patterns--browser-context nil)
(put 'arxana-patterns--browser-context 'permanent-local t)
(defvar-local arxana-patterns--browser--last-row 1)
(defvar-local arxana-patterns--docbook-contents-order nil
  "Alist of docbook order overrides keyed by book.")
(defvar-local arxana-patterns--docbook-contents-cache nil
  "Cached docbook contents items for the current browser view.")
(defvar-local arxana-patterns--docbook-contents-book nil
  "Book id for the cached docbook contents items.")

(defun arxana-patterns--browser-ensure-context ()
  (unless arxana-patterns--browser-stack
    (when arxana-patterns--browser-context
      (setq arxana-patterns--browser-stack
            (list arxana-patterns--browser-context)))))

(defun arxana-patterns--browser-root-format ()
  [("Type" 10 t)
   ("Name" 40 t)
   ("Source/Path" 50 t)
   ("Status" 16 t)
   ("Items" 7 nil)])

(defun arxana-patterns--browser-pattern-format ()
  [("Order" 8 t)
   ("Pattern" 40 t)
   ("Title" 50 t)])

(defun arxana-patterns--browser-menu-format ()
  [("Menu" 20 t)
   ("Description" 70 nil)])

(defun arxana-patterns--browser-info-format ()
  [("Name" 25 t)
   ("Details" 65 nil)])

(defun arxana-patterns--docbook-format ()
  [("Doc" 28 t)
   ("Version" 18 t)
   ("When" 20 t)
   ("Files" 7 t)
   ("Summary" 0 nil)])

(defun arxana-patterns--lab-format ()
  [("Session" 36 t)
   ("Start" 20 t)
   ("Files" 7 t)
   ("Focus" 0 nil)])

(defun arxana-patterns--lab-file-format ()
  [("File" 32 t)
   ("When" 17 t)
   ("Kind" 8 t)
   ("Path" 0 nil)])

(defun arxana-patterns--docbook-contents-format ()
  [("Heading" 60 t)
   ("Path" 0 nil)
   ("Id" 6 t)
   ("Src" 4 t)
   ("C/D" 10 t)])


(defun arxana-patterns--docbook-lines-in-string (text)
  (length (split-string (string-trim (or text "")) "\n" t)))

(defun arxana-patterns--docbook-file-line-count (path)
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (count-lines (point-min) (point-max)))))

(defun arxana-patterns--docbook-ratio-format (loc lod)
  (cond
   ((or (null loc) (null lod)) "")
   ((<= lod 0) (if (> loc 0) "inf" "0"))
   (t
    (let ((ratio (/ (float loc) (float lod))))
      (cond
       ((>= ratio 10000) (format "%.2e" ratio))
       ((>= ratio 1000) (format "%.1e" ratio))
       ((>= ratio 100) (format "%.0f" ratio))
       ((>= ratio 10) (format "%.1f" ratio))
       (t (format "%.2f" ratio)))))))

(defun arxana-patterns--docbook-entry-source-path (entry)
  (when entry
    (arxana-docbook--entry-source-path entry)))

(defun arxana-patterns--docbook-entry-doc-lines (entry)
  (when entry
    (arxana-patterns--docbook-lines-in-string
     (arxana-docbook--entry-content entry))))

(defun arxana-patterns--docbook-entry-loc (entry)
  (let* ((source (arxana-patterns--docbook-entry-source-path entry))
         (root (arxana-docbook--repo-root))
         (path (and source (expand-file-name source root))))
    (arxana-patterns--docbook-file-line-count path)))

(defun arxana-patterns--docbook-entry-coverage (entry)
  (if (arxana-patterns--docbook-entry-source-path entry) "Y" ""))

(defun arxana-patterns--docbook-entry-ratio (entry)
  (let ((loc (arxana-patterns--docbook-entry-loc entry))
        (lod (arxana-patterns--docbook-entry-doc-lines entry)))
    (arxana-patterns--docbook-ratio-format loc lod)))

(defun arxana-patterns--docbook-top-key (item)
  (or (car (plist-get item :outline))
      (car (split-string (or (plist-get item :path_string) "") " / " t))
      (plist-get item :title)
      ""))

(defun arxana-patterns--docbook-group-order (items)
  (let ((order '()))
    (dolist (item items)
      (let ((key (arxana-patterns--docbook-top-key item)))
        (when (and key (not (member key order)))
          (setq order (append order (list key))))))
    order))

(defun arxana-patterns--docbook-group-items (items)
  (let* ((order (arxana-patterns--docbook-group-order items))
         (preferred (seq-filter (lambda (key) (member key order))
                                arxana-patterns-docbook-top-order))
         (order (append preferred (seq-filter (lambda (key) (not (member key preferred)))
                                              order))))
    (sort (copy-sequence items)
          (lambda (a b)
            (let* ((ga (cl-position (arxana-patterns--docbook-top-key a)
                                    order
                                    :test #'equal))
                   (gb (cl-position (arxana-patterns--docbook-top-key b)
                                    order
                                    :test #'equal))
                   (la (or (plist-get a :level) 99))
                   (lb (or (plist-get b :level) 99))
                   (ia (or (plist-get a :toc-index) 0))
                   (ib (or (plist-get b :toc-index) 0)))
              (cond
               ((and ga gb (/= ga gb)) (< ga gb))
               ((/= la lb) (< la lb))
               (t (< ia ib))))))))

(defun arxana-patterns--docbook-contents-order-get (book)
  (cdr (assoc book arxana-patterns--docbook-contents-order)))

(defun arxana-patterns--docbook-contents-order-set (book order)
  (setf (alist-get book arxana-patterns--docbook-contents-order nil nil #'equal)
        order))

(defun arxana-patterns--docbook-contents-order-items (items order)
  (let ((by-id (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (ordered '()))
    (dolist (item (arxana-patterns--docbook-contents-unique-items items))
      (let ((doc-id (plist-get item :doc-id)))
        (when doc-id
          (puthash doc-id item by-id))))
    (dolist (doc-id order)
      (let ((item (gethash doc-id by-id)))
        (when item
          (puthash doc-id t seen)
          (push item ordered))))
    (setq ordered (nreverse ordered))
    (dolist (item (arxana-patterns--docbook-contents-unique-items items))
      (let ((doc-id (plist-get item :doc-id)))
        (when (and doc-id (not (gethash doc-id seen)))
          (push item ordered))))
    (nreverse ordered)))

(defun arxana-patterns--docbook-contents-normalize-order (order items)
  "Return ORDER filtered to ITEMS, appending any missing ids."
  (let ((present (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (normalized '())
        (missing '()))
    (dolist (item items)
      (when-let* ((doc-id (plist-get item :doc-id)))
        (puthash doc-id t present)))
    (dolist (doc-id order)
      (when (and doc-id (gethash doc-id present))
        (puthash doc-id t seen)
        (push doc-id normalized)))
    (setq normalized (nreverse normalized))
    (dolist (item items)
      (when-let* ((doc-id (plist-get item :doc-id)))
        (unless (gethash doc-id seen)
          (push doc-id missing))))
    (append normalized (nreverse missing))))

(defun arxana-patterns--docbook-contents-ensure-order (book items)
  (let* ((order (arxana-patterns--docbook-contents-order-get book))
         (default-order (mapcar (lambda (item) (plist-get item :doc-id))
                                (arxana-patterns--docbook-contents-unique-items items)))
         (default-order (delq nil default-order)))
    (unless order
      (setq order default-order)
      (arxana-patterns--docbook-contents-order-set book order))
    order))

(defun arxana-patterns--docbook-contents-item-map (items)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (item (arxana-patterns--docbook-contents-unique-items items))
      (when-let* ((doc-id (plist-get item :doc-id)))
        (puthash doc-id item table)))
    table))

(defun arxana-patterns--browser-root-row (item)
  (let* ((type (capitalize (symbol-name (or (plist-get item :type) 'unknown))))
         (name (or (plist-get item :title)
                   (plist-get item :label)
                   ""))
         (source (or (plist-get item :source) "-"))
         (status (or (plist-get item :status) "-"))
         (count (number-to-string (or (plist-get item :count) 0))))
    (vector type name source status count)))

(defun arxana-patterns--browser-pattern-row (item)
  (let* ((order (plist-get item :order))
         (name (or (plist-get item :label) ""))
         (title (or (plist-get item :title) ""))
         (file (plist-get item :file)))
    (vector (if order (number-to-string order) "-")
            name
            (if file
                (file-name-nondirectory file)
              title))))

(defun arxana-patterns--browser-menu-row (item)
  (vector (or (plist-get item :label) "")
          (or (plist-get item :description) "")))

(defun arxana-patterns--browser-info-row (item)
  (vector (or (plist-get item :label) "")
          (or (plist-get item :description) "")))

(defun arxana-patterns--docbook-short-id (doc-id)
  (let* ((suffix (and doc-id (car (last (split-string doc-id "-" t))))))
    (and suffix (substring suffix 0 (min 5 (length suffix))))))

(defun arxana-patterns--docbook-row-label (item)
  (let* ((doc-id (plist-get item :doc-id))
         (title (or (plist-get item :label) ""))
         (short-id (arxana-patterns--docbook-short-id doc-id)))
    (cond
     ((and short-id (not (string-empty-p title)))
      (format "[%s] %s" short-id title))
     (short-id (format "[%s]" short-id))
     ((not (string-empty-p title)) title)
     (doc-id doc-id)
     (t ""))))

(defun arxana-patterns--docbook-row (item)
  (let ((label (arxana-patterns--docbook-row-label item)))
    (vector label
            (or (plist-get item :version) "")
            (or (plist-get item :timestamp) "")
            (format "%d" (length (or (plist-get item :files) '())))
            (or (plist-get item :summary)
                (plist-get item :description)
                ""))))

(defun arxana-patterns--docbook-contents-row (item)
  (let* ((level (or (plist-get item :level) 1))
         (title (or (plist-get item :title) ""))
         (doc-id (plist-get item :doc-id))
         (short-id (arxana-patterns--docbook-short-id doc-id))
         (short-col (if short-id (format "%5s" short-id) ""))
         (coverage (or (plist-get item :coverage) ""))
         (ratio (or (plist-get item :ratio) ""))
         ;; ASCII markers only; non-ASCII char literals can break load in some Emacs builds.
         (indent (make-string (max 0 (1- level)) ?.))
         (latest (plist-get item :latest))
         (virtual (plist-get item :virtual))
         (marker (cond
                  (virtual (propertize "!" 'face 'arxana-patterns-docbook-unindexed-face))
                  (latest (propertize "*" 'face 'arxana-patterns-docbook-latest-face))
                  (t (propertize "." 'face 'arxana-patterns-docbook-empty-face)))))
    (vector (format "%s %s%s"
                    marker
                    (if (string-empty-p indent)
                        ""
                      (format "%s " indent))
                    title)
            (or (plist-get item :path_string) "")
            short-col
            coverage
            ratio)))


(defun arxana-patterns--lab-row (item)
  (let* ((session (or (plist-get item :session-id) ""))
         (start (or (plist-get item :timestamp-start) ""))
         (files (or (plist-get item :files) '()))
         (focus (or (car files) "")))
    (vector session
            start
            (format "%d" (length files))
            focus)))

(defun arxana-patterns--lab-file-row (item)
  (let* ((label (or (plist-get item :label) ""))
         (modified (or (plist-get item :modified) ""))
         (kind (or (plist-get item :kind) ""))
         (path (or (plist-get item :path) "")))
    (vector label modified kind path)))

(defun arxana-patterns--browser-header-line (context total)
  (cond
   ((not context)
    (format "Futon4 browser menu (%d entries). RET/right selects, LEFT/b returns."
            total))
   ((eq (plist-get context :view) 'patterns)
    (let ((base (format "Pattern languages & collections (%d entries). RET/right opens, LEFT/b backs up, I imports, E/e edit collections, +/- reorder, A adds a root, g refreshes."
                        total)))
      (if (arxana-store-sync-enabled-p)
          base
        (concat base " [sync disabled: showing filesystem only]"))))
   ((eq (plist-get context :view) 'code)
    "Code browser — wire Futon1 source entities here. LEFT/b returns.")
   ((eq (plist-get context :view) 'media)
    "Media library — pick All tracks, a status, or Projects to drill into recorder projects. LEFT/b returns.")
   ((eq (plist-get context :view) 'media-projects)
    "Media projects — select a recorder project to list its tracks. LEFT/b returns.")
   ((eq (plist-get context :view) 'media-publications)
    "Media publications — select an EP folder to browse its exported tracks. LEFT/b returns.")
   ((eq (plist-get context :view) 'media-publication)
    "Publication tracks — RET plays, p plays, s stops. LEFT/b returns.")
   ((eq (plist-get context :view) 'docbook)
    (format "Doc books — select a book, then Contents or Recent. %s. LEFT/b returns."
            (arxana-docbook--source-brief)))
   ((eq (plist-get context :view) 'docbook-book)
    (format "Doc book views — pick Contents or Recent. %s. LEFT/b returns."
            (arxana-docbook--source-brief (plist-get context :book))))
   ((eq (plist-get context :view) 'docbook-contents)
    (format "Doc book contents — RET opens heading; %s. LEFT/b returns."
            (arxana-docbook--source-brief (plist-get context :book))))
   ((eq (plist-get context :view) 'docbook-section)
    (format "Doc book section — RET opens entry; %s. LEFT/b returns."
            (arxana-docbook--source-brief (plist-get context :book))))
   ((eq (plist-get context :view) 'docbook-recent)
    (format "Doc book recent entries — RET opens entry; %s. LEFT/b returns."
            (arxana-docbook--source-brief (plist-get context :book))))
   ((eq (plist-get context :view) 'lab)
    "Lab notebook — RET stub; v trace, r raw, d draft. LEFT/b returns.")
   ((eq (plist-get context :view) 'lab-files)
    (format "Lab files (%s) — RET opens file. LEFT/b returns."
            (or (plist-get context :label) "lab")))
   ((plist-get context :media-filter)
    (let* ((label (or (plist-get context :label) "Tracks"))
           (count (plist-get context :count)))
      (format "%s — %s. LEFT/b returns."
              label
              (if (numberp count)
                  (format "%d track%s" count (if (= count 1) "" "s"))
                (format "%d entries" total)))))
   (t
    (let ((title (or (plist-get context :title) (plist-get context :label))))
      (format "%s — RET/right opens pattern, LEFT/b returns." title)))))

(defun arxana-patterns--browser-root-items ()
  (let* ((language-rows (when (arxana-store-sync-enabled-p)
                          (arxana-patterns-ingest-language-rows)))
         (language-index (and language-rows
                              (arxana-patterns--language-index-by-path language-rows)))
         (language-items
          (mapcar (lambda (row)
                    (let* ((import-path (plist-get row :import-path))
                           (source (or import-path
                                       (arxana-patterns--friendly-classification
                                        (plist-get row :source)))))
                      (list :type 'language
                            :label (plist-get row :name)
                            :title (or (plist-get row :title) (plist-get row :name))
                            :entity-id (plist-get row :id)
                            :source source
                            :status (or (arxana-patterns--friendly-classification
                                         (plist-get row :status))
                                        "-")
                            :count (plist-get row :count)
                            :import-path import-path)))
                  (or language-rows '())))
         (collection-items (or (arxana-patterns--filesystem-collection-items language-index) '())))
    (append language-items collection-items)))

(defun arxana-patterns--menu-items ()
  (list (list :type 'menu
              :label "Patterns"
              :description "Browse pattern languages and flexiarg collections."
              :view 'patterns)
        (list :type 'menu
              :label "Code"
              :description "Upcoming Arxana code browser (imports pending)."
              :view 'code)
        (list :type 'menu
              :label "Media"
              :description "Zoom/Napster media library prototype."
              :view 'media)
        (list :type 'menu
              :label "Docs"
              :description "Doc books (XTDB-backed, futon4)."
              :view 'docbook)
        (list :type 'menu
              :label "Lab"
              :description "Lab notebook sessions staged under lab/."
              :view 'lab)))

(defun arxana-patterns--code-items ()
  (list (list :type 'info
              :label "Arxana source"
              :description "Hook up Futon1 code entities to browse modules here.")
        (list :type 'info
              :label "Import status"
              :description "No code catalogs detected yet.")))

(defun arxana-patterns--media-items ()
  (let* ((entries (or (arxana-media--entries) '()))
         (total (length entries)))
    (cond
     ((zerop total)
      (list (list :type 'info
                  :label "No Zoom catalog detected"
                  :description (if (and arxana-media-index-path
                                        (not (file-readable-p arxana-media-index-path)))
                                   (format "Expected catalog at %s" arxana-media-index-path)
                                 "Run zoom_sync.py to populate data/zoom_sync_index.json."))))
     (t
      (let ((items (list (list :type 'media-category
                               :label "All tracks"
                               :description (format "%d total recording%s"
                                                    total (if (= total 1) "" "s"))
                               :media-filter 'all
                               :count total))))
        (setq items (append items (arxana-media--status-items entries)))
        (let* ((pub-root (file-name-as-directory (expand-file-name arxana-media-publications-root)))
               (pub-dirs (or (arxana-media--publication-directories) '()))
               (pub-count (length pub-dirs)))
          (setq items
                (append items
                        (list (list :type 'menu
                                    :label "EPs"
                                    :description (format "%d publication%s — %s"
                                                         pub-count
                                                         (if (= pub-count 1) "" "s")
                                                         pub-root)
                                    :view 'media-publications)))))
        (let ((projects (arxana-media--project-items entries)))
          (when projects
            (setq items
                  (append items
                          (list (list :type 'media-projects
                                      :label "Projects"
                  :description "Recording folders from the Zoom R4."
                  :view 'media-projects
                  :count (length projects)))))))
        items)))))

(defun arxana-patterns--docbook-books ()
  (let ((books (or (arxana-docbook--available-books) '("futon4"))))
    (mapcar (lambda (book)
              (list :type 'docbook-book
                    :label (capitalize book)
                    :book book
                    :description (format "Doc book (%s)."
                                         (arxana-docbook--source-brief book))))
            books)))

(defun arxana-patterns--docbook-unavailable-message (&optional book)
  (let* ((book (or book "futon4"))
         (probe (arxana-docbook--probe-summary book)))
    (format "Doc book %s unavailable. %s" book probe)))

(defun arxana-patterns--docbook-entry-label (entry)
  (or (plist-get entry :title)
      (when-let* ((heading (plist-get entry :heading)))
        (or (plist-get heading :doc/title)
            (plist-get heading :doc/path_string)))
      (plist-get entry :doc-id)))


(defun arxana-patterns--docbook-items (&optional book)
  (let* ((book (or book "futon4"))
         (entries (or (when (arxana-docbook--remote-available-p book)
                        (ignore-errors (arxana-docbook--remote-recent book)))
                      (ignore-errors (arxana-docbook-entries book)))))
    (if (and entries (listp entries))
        (mapcar (lambda (entry)
                  (list :type 'docbook-entry
                        :doc-id (plist-get entry :doc-id)
                        :label (arxana-patterns--docbook-entry-label entry)
                        :version (plist-get entry :version)
                        :timestamp (plist-get entry :timestamp)
                        :files (plist-get entry :files)
                        :summary (plist-get entry :summary)
                        :entry entry))
                entries)
      (list (list :type 'info
                  :label "No doc book entries detected"
                  :description (arxana-patterns--docbook-unavailable-message book))))))

(defun arxana-patterns--docbook-book-items (book)
  (list (list :type 'docbook-contents-root
              :label "Contents"
              :description "Browse spine outline mirrored into doc book."
              :book book)
        (list :type 'docbook-recent
              :label "Recent"
              :description "List recent doc book entries."
              :book book)))

(defun arxana-patterns--docbook-contents-items (book)
  (let* ((remote (when (arxana-docbook--remote-available-p book)
                   (ignore-errors (arxana-docbook--remote-contents book))))
         (local-toc (or (arxana-docbook--toc book) '()))
         (toc (or remote local-toc '()))
         (local-entries (when (and (not remote) (arxana-docbook--filesystem-available-p book))
                          (ignore-errors (arxana-docbook-entries book))))
         (recent-entries (when (and remote (arxana-docbook--remote-available-p book))
                           (ignore-errors (arxana-docbook--remote-recent book))))
         (entries-for-metrics (or recent-entries local-entries '()))
         (entry-map (let ((table (make-hash-table :test 'equal)))
                      (dolist (entry entries-for-metrics)
                        (when-let* ((doc-id (plist-get entry :doc-id)))
                          (let* ((existing (gethash doc-id table))
                                 (current-ts (plist-get entry :timestamp))
                                 (existing-ts (plist-get existing :timestamp)))
                            (when (or (null existing)
                                      (string> (or current-ts "") (or existing-ts "")))
                              (puthash doc-id entry table)))))
                      table))
         (latest-docs (when (and local-entries (listp local-entries))
                        (let ((table (make-hash-table :test 'equal)))
                          (dolist (entry local-entries)
                            (when-let* ((doc-id (plist-get entry :doc-id)))
                              (puthash doc-id t table)))
                          table)))
         (remote-map (when (and remote (listp remote))
                       (let ((table (make-hash-table :test 'equal)))
                         (dolist (h remote)
                           (let ((doc-id (or (plist-get h :doc-id) (plist-get h :doc_id))))
                             (when doc-id
                               (puthash doc-id h table))))
                         table)))
         (toc-items
          (if toc
              (cl-loop for h in toc
                         for idx from 0
                         collect
                         (let* ((doc-id (or (plist-get h :doc-id) (plist-get h :doc_id)))
                                (remote-h (and remote-map doc-id (gethash doc-id remote-map)))
                                (path-string (or (plist-get h :path_string) (plist-get h :path-string)))
                                (latest (or (and remote-h (plist-get remote-h :latest))
                                            (plist-get h :latest)
                                            (and latest-docs (gethash doc-id latest-docs))))
                                (entry (and doc-id (gethash doc-id entry-map)))
                                (coverage (and entry (arxana-patterns--docbook-entry-coverage entry)))
                                (ratio (and entry (arxana-patterns--docbook-entry-ratio entry))))
                           (list :type 'docbook-heading
                                 :doc-id doc-id
                                 :title (or (plist-get h :title) doc-id)
                                 :outline (or (plist-get h :outline) (plist-get h :outline_path))
                                 :path_string path-string
                                 :level (plist-get h :level)
                                 :latest latest
                                 :book book
                                 :coverage coverage
                                 :ratio ratio
                                 :toc-index idx)))
            '()))
         (toc-items (arxana-patterns--docbook-group-items toc-items))
         (order (arxana-patterns--docbook-contents-ensure-order book toc-items))
         (toc-items (arxana-patterns--docbook-contents-order-items toc-items order))
         (toc-docs (let ((table (make-hash-table :test 'equal)))
                     (dolist (item toc-items)
                       (when-let* ((doc-id (plist-get item :doc-id)))
                         (puthash doc-id t table)))
                     table))
         (entry-headings
          (cond
           (remote
            (delq nil
                  (mapcar (lambda (entry)
                            (let* ((heading (plist-get entry :heading))
                                   (doc-id (or (plist-get heading :doc/id)
                                               (plist-get heading :doc-id))))
                              (when doc-id
                                (list :doc-id doc-id
                                      :title (or (plist-get heading :doc/title)
                                                 (plist-get heading :doc/path_string)
                                                 doc-id)
                                      :outline (plist-get heading :doc/outline_path)
                                      :path_string (plist-get heading :doc/path_string)
                                      :level (plist-get heading :doc/level)))))
                          (or recent-entries '()))))
           (local-entries
            (delq nil
                  (mapcar (lambda (entry)
                            (when-let* ((doc-id (plist-get entry :doc-id)))
                              (let* ((outline (plist-get entry :outline))
                                     (path-string (or (plist-get entry :path_string)
                                                      (and outline (string-join outline " / ")))))
                                (list :doc-id doc-id
                                      :title (or (and outline (car outline)) doc-id)
                                      :outline outline
                                      :path_string path-string
                                      :level (and outline (length outline))))))
                          local-entries)))
           (t '())))
         (virtual-items
          (delq nil
                (mapcar
                 (lambda (h)
                   (let ((doc-id (plist-get h :doc-id)))
                     (when (and doc-id (not (gethash doc-id toc-docs)))
                       (let* ((title (or (plist-get h :title) doc-id))
                              (outline (or (plist-get h :outline)
                                           (list "Lab additions" title)))
                              (path-str (or (plist-get h :path_string)
                                            (string-join outline " / "))))
                         (list :type 'docbook-heading
                               :doc-id doc-id
                               :title (format "%s (unindexed)" title)
                               :outline outline
                               :path_string path-str
                               :level (or (plist-get h :level) 1)
                               :latest t
                               :virtual t
                               :book book
                               :coverage (and (gethash doc-id entry-map)
                                              (arxana-patterns--docbook-entry-coverage
                                               (gethash doc-id entry-map)))
                               :ratio (and (gethash doc-id entry-map)
                                           (arxana-patterns--docbook-entry-ratio
                                            (gethash doc-id entry-map))))))))
                 entry-headings))))
    (setq arxana-patterns--docbook-contents-cache toc-items)
    (setq arxana-patterns--docbook-contents-book book)
    (cond
     ((or toc-items virtual-items) (append toc-items virtual-items))
     (t (list (list :type 'info
                    :label "No TOC found"
                    :description (arxana-patterns--docbook-unavailable-message book))))
  )))

(defun arxana-patterns--docbook-contents-context ()
  (let ((context (car arxana-patterns--browser-stack)))
    (when (and context (eq (plist-get context :view) 'docbook-contents))
      context)))

(defun arxana-patterns--docbook-contents-assert ()
  (unless (arxana-patterns--docbook-contents-context)
    (user-error "Not in a docbook contents view")))

(defun arxana-patterns--docbook-contents-current-item ()
  (arxana-patterns--docbook-contents-assert)
  (let ((item (tabulated-list-get-id)))
    (unless (and item (plist-get item :doc-id))
      (user-error "No docbook heading at point"))
    item))

(defun arxana-patterns--docbook-contents-items-live ()
  "Return the current docbook contents items."
  (or arxana-patterns--docbook-contents-cache
      (delq nil
            (mapcar (lambda (entry)
                      (let ((item (car-safe entry)))
                        (when (and item (plist-get item :doc-id))
                          item)))
                    (or tabulated-list-entries '())))))

(defun arxana-patterns--docbook-contents-unique-items (items)
  "Return ITEMS de-duplicated by :doc-id, preserving first occurrence."
  (let ((seen (make-hash-table :test 'equal))
        (unique '()))
    (dolist (item items)
      (let ((doc-id (plist-get item :doc-id)))
        (when (and doc-id (not (gethash doc-id seen)))
          (puthash doc-id t seen)
          (push item unique))))
    (nreverse unique)))

(defun arxana-patterns--docbook-contents-current-order (items)
  (delq nil (mapcar (lambda (item) (plist-get item :doc-id))
                    (arxana-patterns--docbook-contents-unique-items items))))

(defun arxana-patterns--docbook-contents-item-swap (order idx-a idx-b)
  (let ((copy (copy-sequence order)))
    (let ((val-a (nth idx-a copy))
          (val-b (nth idx-b copy)))
      (setf (nth idx-a copy) val-b)
      (setf (nth idx-b copy) val-a))
    copy))

(defun arxana-patterns--docbook-contents-reorder (new-order doc-id)
  (let* ((context (arxana-patterns--docbook-contents-context))
         (book (plist-get context :book)))
    (arxana-patterns--docbook-contents-order-set book new-order)
    (arxana-patterns--browser-render)
    (when (fboundp 'arxana-patterns--browser-goto-doc-id)
      (arxana-patterns--browser-goto-doc-id doc-id))))

(defun arxana-patterns-docbook-move-item-up ()
  "Move the current docbook heading up one row."
  (interactive)
  (let* ((item (arxana-patterns--docbook-contents-current-item))
         (doc-id (plist-get item :doc-id))
         (context (arxana-patterns--docbook-contents-context))
         (book (plist-get context :book))
         (items (arxana-patterns--docbook-contents-items-live))
         (order (arxana-patterns--docbook-contents-current-order items)))
    (unless order
      (user-error "No ordering information available"))
    (let* ((idx (cl-position doc-id order :test #'equal))
           (prev-idx (and idx (> idx 0) (1- idx))))
      (unless idx
        (user-error "No ordering entry for this heading"))
      (unless prev-idx
        (user-error "Already at top"))
      (arxana-patterns--docbook-contents-reorder
       (arxana-patterns--docbook-contents-item-swap order idx prev-idx)
       doc-id))))

(defun arxana-patterns-docbook-move-item-down ()
  "Move the current docbook heading down one row."
  (interactive)
  (let* ((item (arxana-patterns--docbook-contents-current-item))
         (doc-id (plist-get item :doc-id))
         (context (arxana-patterns--docbook-contents-context))
         (book (plist-get context :book))
         (items (arxana-patterns--docbook-contents-items-live))
         (order (arxana-patterns--docbook-contents-current-order items)))
    (unless order
      (user-error "No ordering information available"))
    (let* ((idx (cl-position doc-id order :test #'equal))
           (next-idx (and idx (< idx (1- (length order))) (1+ idx))))
      (unless idx
        (user-error "No ordering entry for this heading"))
      (unless next-idx
        (user-error "Already at bottom"))
      (arxana-patterns--docbook-contents-reorder
       (arxana-patterns--docbook-contents-item-swap order idx next-idx)
       doc-id))))

(defun arxana-patterns--docbook-contents-section-blocks (items)
  "Return section blocks based on display order in ITEMS."
  (let ((blocks '())
        (current-key nil)
        (current-block '()))
    (dolist (item (arxana-patterns--docbook-contents-unique-items items))
      (let* ((doc-id (plist-get item :doc-id))
             (key (arxana-patterns--docbook-top-key item)))
        (if (and current-key (equal key current-key))
            (push doc-id current-block)
          (when current-block
            (push (cons current-key (nreverse current-block)) blocks))
          (setq current-key key)
          (setq current-block (list doc-id)))))
    (when current-block
      (push (cons current-key (nreverse current-block)) blocks))
    (nreverse blocks)))

(defun arxana-patterns-docbook-move-section-up ()
  "Move the current docbook section up by one section."
  (interactive)
  (let* ((item (arxana-patterns--docbook-contents-current-item))
         (doc-id (plist-get item :doc-id))
         (context (arxana-patterns--docbook-contents-context))
         (book (plist-get context :book))
         (items (arxana-patterns--docbook-contents-items-live))
         (order (arxana-patterns--docbook-contents-current-order items))
         (blocks (arxana-patterns--docbook-contents-section-blocks items))
         (key (arxana-patterns--docbook-top-key item))
         (idx (cl-position key (mapcar #'car blocks) :test #'equal)))
    (unless idx
      (user-error "No section ordering entry for this heading"))
    (unless (and idx (> idx 0))
      (user-error "Already at top"))
    (let* ((copy (copy-sequence blocks))
           (prev (nth (1- idx) copy))
           (cur (nth idx copy)))
      (setf (nth (1- idx) copy) cur)
      (setf (nth idx copy) prev)
      (arxana-patterns--docbook-contents-reorder
       (apply #'append (mapcar #'cdr copy))
       doc-id))))

(defun arxana-patterns-docbook-move-section-down ()
  "Move the current docbook section down by one section."
  (interactive)
  (let* ((item (arxana-patterns--docbook-contents-current-item))
         (doc-id (plist-get item :doc-id))
         (context (arxana-patterns--docbook-contents-context))
         (book (plist-get context :book))
         (items (arxana-patterns--docbook-contents-items-live))
         (order (arxana-patterns--docbook-contents-current-order items))
         (blocks (arxana-patterns--docbook-contents-section-blocks items))
         (key (arxana-patterns--docbook-top-key item))
         (idx (cl-position key (mapcar #'car blocks) :test #'equal)))
    (unless idx
      (user-error "No section ordering entry for this heading"))
    (unless (and idx (< idx (1- (length blocks))))
      (user-error "Already at bottom"))
    (let* ((copy (copy-sequence blocks))
           (next (nth (1+ idx) copy))
           (cur (nth idx copy)))
      (setf (nth (1+ idx) copy) cur)
      (setf (nth idx copy) next)
      (arxana-patterns--docbook-contents-reorder
       (apply #'append (mapcar #'cdr copy))
       doc-id))))
  

(defun arxana-patterns--docbook-section-items (book heading)
  (let* ((outline (plist-get heading :outline))
         (doc-id (plist-get heading :doc-id))
         (entries (or (when (arxana-docbook--remote-available-p book)
                        (ignore-errors (arxana-docbook--remote-heading book doc-id)))
                      (ignore-errors (arxana-docbook-entries book)))))
    (if (and entries (listp entries))
        (let* ((matches
                (seq-filter
                 (lambda (entry)
                   (or (and outline (equal outline (plist-get entry :outline)))
                       (and doc-id (equal doc-id (plist-get entry :doc-id)))))
                 entries)))
          (if matches
              (mapcar (lambda (entry)
                        (list :type 'docbook-entry
                              :doc-id (plist-get entry :doc-id)
                              :label (arxana-patterns--docbook-entry-label entry)
                              :version (plist-get entry :version)
                              :timestamp (plist-get entry :timestamp)
                              :files (plist-get entry :files)
                              :summary (plist-get entry :summary)
                              :entry entry))
                      matches)
            (list (list :type 'info
                        :label "No entries yet"
                        :summary "Add a stub/raw entry for this heading to browse it here."
                        :description "Add a stub/raw entry for this heading to browse it here."))))
      (list (list :type 'info
                  :label "No doc book entries detected"
                  :summary (arxana-patterns--docbook-unavailable-message book)
                  :description (arxana-patterns--docbook-unavailable-message book))))))

(defun arxana-patterns--lab-items ()
  (let ((entries (or (arxana-lab-entries) '())))
    (if (and entries (listp entries))
        entries
      (list (list :type 'info
                  :label "No lab entries detected"
                  :description "Run dev/lab-export.clj to populate lab/raw.")))))

(defun arxana-patterns-browse-lab-files (kind)
  "Open the lab files browser for KIND (raw, stubs, drafts)."
  (interactive
   (list (intern (completing-read "Lab files: " '("raw" "stubs" "drafts") nil t))))
  (let* ((kind (if (symbolp kind) kind (intern kind)))
         (label (pcase kind
                  ('raw "Raw")
                  ('stubs "Stubs")
                  ('drafts "Drafts")
                  (_ (capitalize (format "%s" kind))))))
    (with-current-buffer (get-buffer-create arxana-patterns--browser-buffer)
      (setq arxana-patterns--browser-stack (list (list :view 'lab-files
                                                       :kind kind
                                                       :label label)))
      (setq arxana-patterns--browser-context nil))
    (arxana-patterns--browser-render)))

(defcustom arxana-patterns-frame-name "Arxana"
  "Name of the frame used for Arxana browse windows."
  :type 'string
  :group 'arxana)

(defcustom arxana-patterns-frame-fullscreen nil
  "When non-nil, make the Arxana browse frame fullscreen."
  :type 'boolean
  :group 'arxana)

(defun arxana-patterns--ensure-frame ()
  (or (seq-find (lambda (frame)
                  (and (frame-live-p frame)
                       (equal (frame-parameter frame 'name) arxana-patterns-frame-name)))
                (frame-list))
      (let ((frame (make-frame `((name . ,arxana-patterns-frame-name)))))
        (set-frame-parameter frame 'arxana-frame t)
        (when arxana-patterns-frame-fullscreen
          (set-frame-parameter frame 'fullscreen 'fullboth))
        frame)))

(defun arxana-patterns-browse-lab-files-other-frame (kind)
  "Open the lab files browser for KIND in the Arxana frame."
  (interactive
   (list (intern (completing-read "Lab files: " '("raw" "stubs" "drafts") nil t))))
  (let ((frame (arxana-patterns--ensure-frame)))
    (with-selected-frame frame
      (let ((display-buffer-overriding-action
             '((display-buffer-same-window))))
        (arxana-patterns-browse-lab-files kind))
      (select-frame-set-input-focus frame))))

(defalias 'arxana-patterns-browse-lab-files-other-window
  #'arxana-patterns-browse-lab-files-other-frame)

(defun arxana-patterns--normalize-order (value)
  (cond
   ((numberp value) value)
   ((stringp value)
    (when (string-match-p "\\`[0-9]+\\'" value)
      (string-to-number value)))
   (t nil)))

(defun arxana-patterns--language-pattern-entry (link)
  (when (arxana-patterns--relation-match-p
         (arxana-patterns--alist :relation link)
         arxana-patterns-ingest-language-relation)
    (let* ((entity (arxana-patterns--alist :entity link))
           (pattern-id (arxana-patterns--entity-value entity :id :entity/id))
           (name (or (arxana-patterns--entity-value entity :name :entity/name)
                     (arxana-patterns--entity-value entity :ident :entity/ident)))
           (title (or (arxana-patterns--entity-value entity :external-id :entity/external-id)
                      (arxana-patterns--entity-value entity :name :entity/name)))
           (order (or (arxana-patterns--link-prop link :order)
                      (arxana-patterns--link-prop link 'order))))
      (when name
        (list :type 'pattern
              :pattern-id pattern-id
              :label name
              :title title
              :order (arxana-patterns--normalize-order order))))))

(defun arxana-patterns--language-pattern-items (language-name)
  (arxana-patterns--ensure-sync)
  (let* ((ego-response (ignore-errors (arxana-store-ego language-name arxana-patterns-language-ego-limit)))
         (ego (and ego-response (arxana-patterns--alist :ego ego-response)))
         (outgoing (arxana-patterns--ego-outgoing ego))
         (items (delq nil (mapcar #'arxana-patterns--language-pattern-entry outgoing))))
    (seq-sort (lambda (a b)
                (let ((oa (or (plist-get a :order) most-positive-fixnum))
                      (ob (or (plist-get b :order) most-positive-fixnum)))
                  (if (/= oa ob)
                      (< oa ob)
                    (string< (or (plist-get a :label) "")
                             (or (plist-get b :label) "")))))
              items)))

(defun arxana-patterns--filesystem-pattern-items (library)
  (when library
    (let* ((files (or (plist-get library :files)
                      (arxana-patterns--flexiarg-files-for (plist-get library :label))))
           (results nil))
      (dolist (file files)
        (dolist (entry (arxana-patterns--parse-flexiarg file))
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

(defun arxana-patterns--browser-pattern-items (language)
  (when language
    (pcase (plist-get language :type)
      ('language
       (arxana-patterns--language-pattern-items (plist-get language :label)))
      ('collection
       (arxana-patterns--filesystem-pattern-items language))
      (_ nil))))

(defun arxana-patterns--move-entry (entries old-index new-index)
  (let* ((len (length entries))
         (new-index (max 0 (min new-index (1- len))))
         (elem (nth old-index entries))
         (rest (cl-remove elem entries :count 1 :test #'eq)))
    (append (cl-subseq rest 0 new-index)
            (list elem)
            (nthcdr new-index rest))))

(defun arxana-patterns--browser-goto-label (label)
  (goto-char (point-min))
  (forward-line 1)
  (catch 'found
    (while (not (eobp))
      (let ((entry (tabulated-list-get-id)))
        (when (and entry (string= (plist-get entry :label) label))
          (throw 'found t)))
      (forward-line 1))))

(defun arxana-patterns--flexiarg-apply-order (items)
  (let ((n 1))
    (dolist (entry items)
      (let ((file (plist-get entry :file)))
        (when file
          (arxana-patterns--flexiarg-set-order file n)))
      (setq n (1+ n)))))

(defun arxana-patterns--language-entity-id (language)
  (or (plist-get language :entity-id)
      (let* ((name (plist-get language :label))
             (entity (and name (arxana-patterns--resolve-entity-by-name name))))
        (and entity (arxana-patterns--entity-value entity :id :entity/id)))))

(defun arxana-patterns--pattern-entry-id (entry)
  (or (plist-get entry :pattern-id)
      (let* ((name (plist-get entry :label))
             (entity (and name (arxana-patterns--resolve-entity-by-name name))))
        (and entity (arxana-patterns--entity-value entity :id :entity/id)))))

(defun arxana-patterns--language-apply-order (language items)
  (arxana-patterns--ensure-sync)
  (let ((language-id (arxana-patterns--language-entity-id language)))
    (unless language-id
      (user-error "Could not determine Futon id for %s" (plist-get language :label)))
    (let ((n 1)
          (updated 0))
      (dolist (entry items)
        (let ((pattern-id (arxana-patterns--pattern-entry-id entry)))
          (unless pattern-id
            (user-error "Could not determine Futon id for pattern %s" (plist-get entry :label)))
          (arxana-store-create-relation :src language-id
                                        :dst pattern-id
                                        :label arxana-patterns-ingest-language-relation
                                        :props (list (cons 'order n)))
          (setq updated (1+ updated)))
        (setq n (1+ n)))
      (message "Updated %d relations for %s" updated (plist-get language :label)))))

(defun arxana-patterns--browser-move-pattern (delta)
  (arxana-patterns--browser-ensure-context)
  (let ((context (car arxana-patterns--browser-stack))
        (entry (tabulated-list-get-id)))
    (unless context
      (user-error "Reordering only works after selecting a collection or language"))
    (unless (and entry (eq (plist-get entry :type) 'pattern))
      (user-error "Place point on a pattern entry to reorder"))
    (let* ((label (plist-get entry :label))
           (file (plist-get entry :file))
           (items (arxana-patterns--browser-pattern-items context))
           (index (cl-position-if (lambda (item)
                                    (and (string= (plist-get item :label) label)
                                         (equal (plist-get item :file) file)))
                                  items))
           (new-index (and index (+ index delta))))
      (unless index
        (user-error "Could not locate this pattern in the collection"))
      (when (and new-index (>= new-index 0)
                 (< new-index (length items)))
        (setq items (arxana-patterns--move-entry items index new-index))
        (pcase (plist-get context :type)
          ('collection
           (arxana-patterns--flexiarg-apply-order items))
          ('language
           (arxana-patterns--language-apply-order context items))
          (_
           (user-error "Reordering is not supported for %s entries"
                       (plist-get context :type))))
        (arxana-patterns--browser-render)
        (arxana-patterns--browser-goto-label label)
        (message "Moved %s to position %d" label (1+ new-index))))))

(defun arxana-patterns--browser-move-pattern-up ()
  (interactive)
  (arxana-patterns--browser-move-pattern -1))

(defun arxana-patterns--browser-move-pattern-down ()
  (interactive)
  (arxana-patterns--browser-move-pattern 1))

(defun arxana-patterns--slugify (text)
  (when text
    (let* ((lower (downcase text))
           (clean (replace-regexp-in-string "[^a-z0-9]+" "-" lower)))
      (replace-regexp-in-string "-+" "-" (string-trim clean "-")))))

(defun arxana-patterns--library-default-language (library)
  (let* ((label (or (plist-get library :label) "library"))
         (title (or (plist-get library :title) label))
         (slug (or (arxana-patterns--slugify label) "library")))
    (list :name (format "pattern-language/%s" slug)
          :title title)))

(defun arxana-patterns-add-collection-root (directory)
  "Add DIRECTORY as an ad-hoc pattern collection root."
  (interactive "DCollection directory: ")
  (let ((path (arxana-patterns--normalize-path directory)))
    (unless (file-directory-p path)
      (user-error "Directory %s does not exist" path))
    (arxana-patterns--register-collection-root path)
    (when (get-buffer arxana-patterns--browser-buffer)
      (arxana-patterns--browser-render))))

(defun arxana-patterns--import-library (library &optional prompt)
  (arxana-patterns--ensure-sync)
  (let ((dir (or (plist-get library :directory)
                 (let* ((label (plist-get library :label))
                        (files (and label (arxana-patterns--flexiarg-files-for label))))
                   (and files (file-name-directory (car files)))))))
    (unless (and dir (file-directory-p dir))
      (user-error "Library directory %s is missing" dir))
    (let* ((defaults (arxana-patterns--library-default-language library))
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
                (let* ((default-status (arxana-patterns-ingest--language-status-name dir nil))
                       (input (string-trim
                               (read-string (format "Pattern language status (default %s): "
                                                    default-status)
                                            nil nil default-status))))
                  (if (string-empty-p input)
                      default-status
                    input))
              (arxana-patterns-ingest--language-status-name dir nil))))
      (unless (and language-name (not (string-empty-p language-name)))
        (user-error "Pattern language name is required to import"))
      (arxana-patterns-ingest-directory dir language-name language-title language-status)
      (arxana-patterns--register-collection-root dir t)
      (arxana-patterns--write-library-metadata dir language-name language-title language-status)
      (message "Imported %s into %s" label language-name)
      language-name)))

(defun arxana-patterns--browser-current-items ()
  (arxana-patterns--browser-ensure-context)
  (let ((context (car arxana-patterns--browser-stack)))
    (cond
     ((not context)
      (arxana-patterns--menu-items))
     ((plist-get context :media-filter)
      (arxana-media--track-items (plist-get context :media-filter)))
     ((plist-get context :view)
      (pcase (plist-get context :view)
        ('patterns (arxana-patterns--browser-root-items))
        ('code (arxana-patterns--code-items))
        ('media (arxana-patterns--media-items))
        ('docbook (arxana-patterns--docbook-books))
        ('docbook-book (arxana-patterns--docbook-book-items (plist-get context :book)))
        ('docbook-contents (arxana-patterns--docbook-contents-items (plist-get context :book)))
        ('docbook-section (arxana-patterns--docbook-section-items (plist-get context :book) context))
        ('docbook-recent (arxana-patterns--docbook-items (plist-get context :book)))
        ('lab (arxana-patterns--lab-items))
        ('lab-files (arxana-lab-file-items (plist-get context :kind)))
        ('media-projects (arxana-media--project-items (or (arxana-media--entries) '())))
        ('media-publications (arxana-media--publications-items))
        ('media-publication (arxana-media--publication-track-items (plist-get context :publication-path)))
        (_ (arxana-patterns--menu-items))))
     (t
      (arxana-patterns--browser-pattern-items context)))))

(defun arxana-patterns--browser--row-count ()
  (length (or tabulated-list-entries '())))

(defun arxana-patterns--browser--current-row ()
  (if (derived-mode-p 'tabulated-list-mode)
      (let ((row 0)
            (pos (line-beginning-position)))
        (save-excursion
          (goto-char (point-min))
          (while (< (point) pos)
            (when (tabulated-list-get-id)
              (setq row (1+ row)))
            (forward-line 1)))
        row)
    arxana-patterns--browser--last-row))

(defun arxana-patterns--browser--goto-first-entry ()
  (goto-char (point-min))
  (while (and (not (eobp)) (null (tabulated-list-get-id)))
    (forward-line 1))
  (not (eobp)))

(defun arxana-patterns--browser--goto-row (row)
  (let* ((count (arxana-patterns--browser--row-count))
         (old arxana-patterns--browser--last-row)
         (new-row arxana-patterns--browser--last-row))
    (if (<= count 0)
        (progn
          (setq arxana-patterns--browser--last-row 0)
          (goto-char (point-min)))
      (setq new-row (max 0 (min row (1- count))))
      (when (arxana-patterns--browser--goto-first-entry)
        (let ((steps new-row))
          (while (> steps 0)
            (forward-line 1)
            (while (and (not (eobp)) (null (tabulated-list-get-id)))
              (forward-line 1))
            (setq steps (1- steps)))))
      (setq arxana-patterns--browser--last-row new-row)
      (beginning-of-line)
      (when (and (> count 0) (/= old new-row))
        (arxana-patterns--play-click)))))

(defun arxana-patterns--browser-goto-doc-id (doc-id)
  "Move point to the first browser row matching DOC-ID."
  (when (and doc-id (derived-mode-p 'arxana-patterns-browser-mode))
    (let ((row 0)
          (found nil))
      (dolist (entry (or tabulated-list-entries '()))
        (when (and (not found)
                   (equal doc-id (plist-get (car entry) :doc-id)))
          (setq found row))
        (setq row (1+ row)))
      (when found
        (setq arxana-patterns--browser--last-row found)
        (arxana-patterns--browser--goto-row found)
        t))))

(defun arxana-patterns--browser-move-selection (delta)
  (let* ((count (arxana-patterns--browser--row-count))
         (current arxana-patterns--browser--last-row)
         (target (if (> count 0)
                     (max 0 (min (1- count) (+ current delta)))
                   0)))
    (arxana-patterns--browser--goto-row target)))

(defun arxana-patterns--browser-wheel-steps (_event)
  (max 1 (or arxana-patterns-browser-wheel-step 1)))

(defun arxana-patterns--browser-wheel-down (event)
  (interactive "e")
  (arxana-patterns--browser-move-selection (arxana-patterns--browser-wheel-steps event)))

(defun arxana-patterns--browser-wheel-up (event)
  (interactive "e")
  (arxana-patterns--browser-move-selection (- (arxana-patterns--browser-wheel-steps event))))

(defun arxana-patterns--browser-next-line (&optional _event)
  (interactive)
  (arxana-patterns--browser-move-selection 1))

(defun arxana-patterns--browser-previous-line (&optional _event)
  (interactive)
  (arxana-patterns--browser-move-selection -1))

(defun arxana-patterns--browser--tabulated-entries (context items)
  (let ((row-fn
         (cond
         ((not context) #'arxana-patterns--browser-menu-row)
         ((plist-get context :media-filter) #'arxana-media--track-row)
         ((plist-get context :view)
          (pcase (plist-get context :view)
            ('patterns #'arxana-patterns--browser-root-row)
            ('code #'arxana-patterns--browser-info-row)
            ('media #'arxana-patterns--browser-info-row)
            ('docbook #'arxana-patterns--browser-info-row)
            ('docbook-book #'arxana-patterns--browser-info-row)
            ('docbook-contents #'arxana-patterns--docbook-contents-row)
            ('docbook-section #'arxana-patterns--docbook-row)
            ('docbook-recent #'arxana-patterns--docbook-row)
            ('lab #'arxana-patterns--lab-row)
            ('lab-files #'arxana-patterns--lab-file-row)
            ('media-projects #'arxana-patterns--browser-info-row)
            ('media-publications #'arxana-patterns--browser-info-row)
            ('media-publication #'arxana-media--publication-track-row)
            (_ #'arxana-patterns--browser-menu-row)))
        (t #'arxana-patterns--browser-pattern-row))))
    (mapcar (lambda (entry)
              (list entry (funcall row-fn entry)))
            items)))

(defun arxana-patterns--browser-item-at-point ()
  (tabulated-list-get-id))

(defun arxana-patterns--browser-render ()
  (let ((buffer (get-buffer-create arxana-patterns--browser-buffer)))
    (with-current-buffer buffer
      (let ((desired-row (if (derived-mode-p 'arxana-patterns-browser-mode)
                             (arxana-patterns--browser--current-row)
                           arxana-patterns--browser--last-row)))
        (arxana-patterns--browser-ensure-context)
        (let* ((context (car arxana-patterns--browser-stack))
               (items (arxana-patterns--browser-current-items))
               (format (cond
                        ((not context) (arxana-patterns--browser-menu-format))
                        ((plist-get context :media-filter)
                         (arxana-media--track-format))
                        ((plist-get context :view)
                         (pcase (plist-get context :view)
                            ('patterns (arxana-patterns--browser-root-format))
                            ('code (arxana-patterns--browser-info-format))
                            ('media (arxana-patterns--browser-info-format))
                            ('docbook (arxana-patterns--browser-info-format))
                            ('docbook-book (arxana-patterns--browser-info-format))
                            ('docbook-contents (arxana-patterns--docbook-contents-format))
                            ('docbook-section (arxana-patterns--docbook-format))
                            ('docbook-recent (arxana-patterns--docbook-format))
                            ('lab (arxana-patterns--lab-format))
                            ('lab-files (arxana-patterns--lab-file-format))
                            ('media-projects (arxana-patterns--browser-info-format))
                            ('media-publications (arxana-patterns--browser-info-format))
                            ('media-publication (arxana-media--publication-track-format))
                            (_ (arxana-patterns--browser-menu-format))))
                        ((eq (plist-get context :type) 'language)
                         (arxana-patterns--browser-pattern-format))
                        ((eq (plist-get context :type) 'collection)
                         (arxana-patterns--browser-pattern-format))
                        (t (arxana-patterns--browser-root-format))))
               (entries (arxana-patterns--browser--tabulated-entries context items)))
          (setq arxana-patterns--browser-context context)
          (let ((inhibit-read-only t))
            (arxana-patterns-browser-mode)
            (setq tabulated-list-format format
                  tabulated-list-entries entries
                  tabulated-list-use-header-line t)
            (setq header-line-format (arxana-patterns--browser-header-line context (length items)))
            (tabulated-list-init-header)
            (tabulated-list-print t)
            (let* ((count (arxana-patterns--browser--row-count))
                   (clamped (if (> count 0)
                                (max 0 (min desired-row (1- count)))
                              0)))
              (arxana-patterns--browser--goto-row clamped))))))
      (display-buffer buffer)))

(defun arxana-patterns--browser-visit ()
  (interactive)
  (let ((item (arxana-patterns--browser-item-at-point)))
    (unless item
      (user-error "No entry on this line"))
    (pcase (plist-get item :type)
      ('menu
       (let ((view (plist-get item :view)))
         (if (not view)
             (message "No view associated with this entry")
           (setq arxana-patterns--browser-stack
                 (cons item arxana-patterns--browser-stack))
           (arxana-patterns--browser-render))))
      ('language
       (setq arxana-patterns--browser-stack
             (cons item arxana-patterns--browser-stack))
       (arxana-patterns--browser-render))
      ('collection
       (setq arxana-patterns--browser-stack
             (cons item arxana-patterns--browser-stack))
       (arxana-patterns--browser-render))
      ('pattern
       (arxana-patterns-open (plist-get item :label)))
      ('media-publication
       (let ((path (plist-get item :path)))
         (unless (and path (file-directory-p path))
           (user-error "Publication path missing or not a directory"))
         (setq arxana-patterns--browser-stack
               (cons (list :view 'media-publication
                           :label (plist-get item :label)
                           :publication-path path)
                     arxana-patterns--browser-stack))
         (arxana-patterns--browser-render)))
      ('media-publication-track
       (arxana-media-play-at-point))
      ((or 'media-category 'media-project)
       (let ((filter (plist-get item :media-filter)))
         (if (not filter)
             (message "No tracks associated with %s" (plist-get item :label))
           (setq arxana-patterns--browser-stack
                 (cons item arxana-patterns--browser-stack))
           (arxana-patterns--browser-render))))
      ('media-projects
       (setq arxana-patterns--browser-stack
             (cons (list :view 'media-projects
                         :label (plist-get item :label))
                   arxana-patterns--browser-stack))
       (arxana-patterns--browser-render))
      ('media-track
      (let* ((entry (plist-get item :entry))
             (title (or (plist-get entry :title)
                        (plist-get entry :base_name)
                        (plist-get entry :sha256))))
        (message "Track: %s (%s)" title (plist-get entry :status))))
      ('docbook-book
       (setq arxana-patterns--browser-stack
             (cons (list :view 'docbook-book
                         :label (plist-get item :label)
                         :book (plist-get item :book))
                   arxana-patterns--browser-stack))
       (arxana-patterns--browser-render))
      ('docbook-contents-root
       (setq arxana-patterns--browser-stack
             (cons (list :view 'docbook-contents
                         :label (plist-get item :label)
                         :book (plist-get item :book))
                   arxana-patterns--browser-stack))
       (arxana-patterns--browser-render))
      ('docbook-heading
       (setq arxana-patterns--browser-stack
             (cons (plist-put (copy-sequence item) :view 'docbook-section)
                   arxana-patterns--browser-stack))
       (arxana-patterns--browser-render))
      ('docbook-recent
       (setq arxana-patterns--browser-stack
             (cons (list :view 'docbook-recent
                         :label (plist-get item :label)
                         :book (plist-get item :book))
                   arxana-patterns--browser-stack))
       (arxana-patterns--browser-render))
      ('docbook-entry
       (let ((entry (plist-get item :entry)))
         (arxana-docbook-open-entry-object entry)))
      ('lab-entry
       (arxana-lab-open-entry-object item))
      ('lab-file
       (arxana-lab-open-file-entry item))
      ('info
       (message "%s" (or (plist-get item :message)
                         "Nothing to open here yet")))
      (_
       (user-error "Don't know how to open %S entries" (plist-get item :type))))))

(defun arxana-patterns--lab-entry-at-point ()
  (let ((item (tabulated-list-get-id)))
    (when (and item (eq (plist-get item :type) 'lab-entry))
      item)))

(defun arxana-patterns--lab-open-trace ()
  (interactive)
  (let ((entry (arxana-patterns--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-trace-object entry)))

(defun arxana-patterns--lab-open-raw ()
  (interactive)
  (let ((entry (arxana-patterns--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-raw-object entry)))

(defun arxana-patterns--lab-open-draft ()
  (interactive)
  (let ((entry (arxana-patterns--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-draft-object entry)))

(defun arxana-patterns--docbook-heading-at-point ()
  (let ((item (arxana-patterns--browser-item-at-point)))
    (when (and item (eq (plist-get item :type) 'docbook-heading))
      item)))

(defun arxana-patterns-docbook-open-book ()
  "Open a compiled docbook view for the current book."
  (interactive)
  (let* ((item (arxana-patterns--browser-item-at-point))
         (book (or (plist-get item :book) "futon4")))
    (arxana-docbook-open-book book)))

(defun arxana-patterns-docbook-open-section-context ()
  "Open a contextual docbook view around the heading at point."
  (interactive)
  (let ((heading (arxana-patterns--docbook-heading-at-point)))
    (unless heading
      (user-error "No docbook heading at point"))
    (arxana-docbook-open-section-context (plist-get heading :book)
                                         (plist-get heading :doc-id)
                                         (plist-get heading :toc-index))))

(defun arxana-patterns--docbook-location (item)
  (let* ((book (or (plist-get item :book) "futon4"))
         (doc-id (plist-get item :doc-id))
         (entry (plist-get item :entry))
         (entry-id (and entry (plist-get entry :entry-id))))
    (cond
     ((and book doc-id entry-id) (format "docbook://%s/%s/%s" book doc-id entry-id))
     ((and book doc-id) (format "docbook://%s/%s" book doc-id))
     (t nil))))

(defun arxana-patterns--browser-copy-location ()
  "Copy a location identifier for the current browser item."
  (interactive)
  (let* ((item (arxana-patterns--browser-item-at-point))
         (location (cond
                    ((and item (memq (plist-get item :type) '(docbook-entry docbook-heading)))
                     (arxana-patterns--docbook-location item))
                    (t nil))))
    (unless location
      (user-error "No location available for this item"))
    (kill-new location)
    (message "Copied %s" location)))

(defun arxana-patterns--browser-up ()
  (interactive)
  (arxana-patterns--browser-ensure-context)
  (cond
   ((not arxana-patterns--browser-stack)
    (if arxana-patterns--browser-context
        (progn
          (setq arxana-patterns--browser-context nil)
          (arxana-patterns--browser-render))
      (message "Already at top level")))
   (t
    (setq arxana-patterns--browser-stack (cdr arxana-patterns--browser-stack))
    (when (null arxana-patterns--browser-stack)
      (setq arxana-patterns--browser-context nil))
    (arxana-patterns--browser-render))))

(defun arxana-patterns--browser-refresh ()
  (interactive)
  (arxana-patterns--browser-render))

(defun arxana-patterns--browser-import-library ()
  "Ingest the flexiarg collection at point into Futon."
  (interactive)
  (let ((item (arxana-patterns--browser-item-at-point)))
    (unless (and item (eq (plist-get item :type) 'collection))
      (user-error "Place point on a collection entry to import"))
    (unless (arxana-store-sync-enabled-p)
      (user-error "Futon sync is disabled; enable futon4-enable-sync first"))
    (arxana-patterns--import-library item 'prompt)
    (setq arxana-patterns--browser-stack nil
          arxana-patterns--browser-context nil)
    (arxana-patterns--browser-render)))

(defun arxana-patterns--browser-edit-collection ()
  "Open an Org buffer for editing every pattern inside the collection at point."
  (interactive)
  (let ((item (arxana-patterns--browser-item-at-point)))
    (unless (and item (eq (plist-get item :type) 'collection))
      (user-error "Place point on a collection entry to edit"))
    (arxana-patterns-edit-collection item)))

(defun arxana-patterns--browser-edit-current-context ()
  "Edit the collection represented by the current browser context.
Useful after drilling into a collection so you can start editing without
returning to the top-level list."
  (interactive)
  (arxana-patterns--browser-ensure-context)
  (let ((context (car arxana-patterns--browser-stack)))
    (unless (and context (eq (plist-get context :type) 'collection))
      (user-error "Not currently viewing a collection"))
    (arxana-patterns-edit-collection context)))

(defun arxana-patterns-import-all-collections ()
  "Ingest every flexiarg collection under known roots."
  (interactive)
  (unless (arxana-store-sync-enabled-p)
    (user-error "Futon sync is disabled; enable futon4-enable-sync first"))
  (let ((collections (arxana-patterns--filesystem-collection-items)))
    (unless collections
      (user-error "No collections were found under the active roots"))
    (when (yes-or-no-p (format "Import %d collections into Futon?" (length collections)))
      (let ((imported 0))
        (dolist (collection collections)
          (when (arxana-patterns--import-library collection)
            (setq imported (1+ imported))))
        (message "Imported %d collections" imported)
        (with-current-buffer (get-buffer-create arxana-patterns--browser-buffer)
          (setq arxana-patterns--browser-stack nil
                arxana-patterns--browser-context nil))
        (arxana-patterns--browser-render)))))

(defalias 'arxana-patterns-import-all-libraries #'arxana-patterns-import-all-collections)

(defvar arxana-patterns-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'arxana-patterns--browser-visit)
    (define-key map (kbd "<right>") #'arxana-patterns--browser-visit)
    (define-key map (kbd "<left>") #'arxana-patterns--browser-up)
    (define-key map [wheel-left] #'arxana-patterns--browser-up)
    (define-key map [wheel-right] #'arxana-patterns--browser-visit)
    (define-key map [double-wheel-left] #'arxana-patterns--browser-up)
    (define-key map [double-wheel-right] #'arxana-patterns--browser-visit)
    (define-key map [triple-wheel-left] #'arxana-patterns--browser-up)
    (define-key map [triple-wheel-right] #'arxana-patterns--browser-visit)
    (define-key map [mouse-6] #'arxana-patterns--browser-up)
    (define-key map [mouse-7] #'arxana-patterns--browser-visit)
    (define-key map [wheel-down] #'arxana-patterns--browser-wheel-down)
    (define-key map [wheel-up] #'arxana-patterns--browser-wheel-up)
    (define-key map [double-wheel-down] #'arxana-patterns--browser-wheel-down)
    (define-key map [double-wheel-up] #'arxana-patterns--browser-wheel-up)
    (define-key map [triple-wheel-down] #'arxana-patterns--browser-wheel-down)
    (define-key map [triple-wheel-up] #'arxana-patterns--browser-wheel-up)
    (define-key map [mouse-4] #'arxana-patterns--browser-wheel-up)
    (define-key map [mouse-5] #'arxana-patterns--browser-wheel-down)
    (define-key map (kbd "b") #'arxana-patterns--browser-up)
    (define-key map (kbd "g") #'arxana-patterns--browser-refresh)
    (define-key map (kbd "I") #'arxana-patterns--browser-import-library)
    (define-key map (kbd "E") #'arxana-patterns--browser-edit-collection)
    (define-key map (kbd "e") #'arxana-patterns--browser-edit-current-context)
    (define-key map (kbd "+") #'arxana-patterns--browser-move-pattern-up)
    (define-key map (kbd "-") #'arxana-patterns--browser-move-pattern-down)
    (define-key map (kbd "A") #'arxana-patterns-add-collection-root)
    (define-key map (kbd "t") #'arxana-media-retitle-at-point)
    (define-key map (kbd "p") #'arxana-media-play-at-point)
    (define-key map (kbd "s") #'arxana-media-stop-playback)
    (define-key map (kbd "o") #'arxana-media-toggle-autoplay-next)
    (define-key map (kbd "v") #'arxana-patterns--lab-open-trace)
    (define-key map (kbd "r") #'arxana-patterns--lab-open-raw)
    (define-key map (kbd "d") #'arxana-patterns--lab-open-draft)
    (define-key map (kbd "O") #'arxana-patterns-docbook-open-book)
    (define-key map (kbd "C") #'arxana-patterns-docbook-open-section-context)
    (define-key map (kbd "M-<up>") #'arxana-patterns-docbook-move-item-up)
    (define-key map (kbd "M-<down>") #'arxana-patterns-docbook-move-item-down)
    (define-key map (kbd "C-M-<up>") #'arxana-patterns-docbook-move-section-up)
    (define-key map (kbd "C-M-<down>") #'arxana-patterns-docbook-move-section-down)
    (define-key map (kbd "m") #'arxana-media-toggle-mark-at-point)
    (define-key map (kbd "U") #'arxana-media-unmark-all)
    (define-key map (kbd "P") #'arxana-media-publish-marked)
    (define-key map (kbd "u") #'arxana-media-set-publication-url)
    (define-key map (kbd "w") #'arxana-media-open-publication-url)
    (define-key map (kbd "y") #'arxana-patterns--browser-copy-location)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode arxana-patterns-browser-mode tabulated-list-mode "Arxana-Browse"
  "Mode for browsing Futon pattern libraries."
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key nil)
  (setq-local hl-line-face 'arxana-patterns-browser-highlight)
  (hl-line-mode 1))

;;;###autoload
(defun arxana-patterns-browse ()
  "Open the pattern library browser buffer."
  (interactive)
  (with-current-buffer (get-buffer-create arxana-patterns--browser-buffer)
    (setq arxana-patterns--browser-stack nil
          arxana-patterns--browser-context nil))
  (arxana-patterns--browser-render))

(defalias 'arxana-browse #'arxana-patterns-browse)

(provide 'arxana-patterns)
;;; arxana-patterns.el ends here
