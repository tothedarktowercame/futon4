;;; arxana-media.el --- Futon media browser helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Media catalog browsing, playback, retitling, tagging, and publication helpers
;; used by the Futon Emacs browser.
;;
;; TODO(org-sync): Track this module in dev/org-sync-tracker.org for mirroring
;; into the literate sources.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'browse-url)
(require 'arxana-store)

(defvar arxana-patterns--browser-context)
(defvar arxana-patterns--browser-stack)
(declare-function arxana-patterns--browser-item-at-point "arxana-patterns")
(declare-function arxana-patterns--browser-render "arxana-patterns")

(defgroup arxana-media nil
  "Media browsing, playback, and publication helpers."
  :group 'arxana)

(defun arxana-media--locate-default-index ()
  (let* ((base (or load-file-name buffer-file-name default-directory))
         (dir (and base (file-name-directory base)))
         (root (and dir (locate-dominating-file dir "futon0"))))
    (cond
     (root
      (expand-file-name "futon0/data/zoom_sync_index.json" root))
     ((and dir
           (file-readable-p (expand-file-name "../futon0/data/zoom_sync_index.json" dir)))
      (expand-file-name "../futon0/data/zoom_sync_index.json" dir))
     ((file-readable-p (expand-file-name "~/code/storage/zoomr4/meta/zoom_sync_index.json"))
      (expand-file-name "~/code/storage/zoomr4/meta/zoom_sync_index.json"))
     ((file-readable-p (expand-file-name "~/code/futon0/data/zoom_sync_index.json"))
      (expand-file-name "~/code/futon0/data/zoom_sync_index.json"))
     (t nil))))

(defcustom arxana-media-index-path (arxana-media--locate-default-index)
  "Path to the futon0 Zoom catalog JSON produced by zoom_sync.py."
  :type '(choice (const :tag "Auto-detect" nil)
                 file)
  :group 'arxana-media)

(defvar arxana-media--catalog nil)
(defvar arxana-media--catalog-mtime nil)

(defun arxana-media--locate-zoom-sync-script ()
  (let* ((base (or load-file-name buffer-file-name default-directory))
         (dir (and base (file-name-directory base)))
         (root (and dir (locate-dominating-file dir "futon0"))))
    (cond
     (root
      (let ((candidate (expand-file-name "futon0/scripts/zoom_sync.py" root)))
        (and (file-readable-p candidate) candidate)))
     (t nil))))

(defcustom arxana-media-zoom-sync-script (arxana-media--locate-zoom-sync-script)
  "Path to futon0/scripts/zoom_sync.py used for catalog mutations."
  :type '(choice (const :tag "Auto-detect" nil)
                 file)
  :group 'arxana-media)

(defun arxana-media--locate-player ()
  (or (executable-find "mpv")
      (executable-find "ffplay")
      (executable-find "afplay")))

(defcustom arxana-media-player-program (arxana-media--locate-player)
  "External program used to play media tracks."
  :type '(choice (const :tag "Auto-detect" nil)
                 string)
  :group 'arxana-media)

(defcustom arxana-media-player-extra-args nil
  "Extra args appended to `arxana-media-player-program` invocations."
  :type '(repeat string)
  :group 'arxana-media)

(defcustom arxana-media-autoplay-next nil
  "When non-nil, automatically play the next track after playback finishes."
  :type 'boolean
  :group 'arxana-media)

(defvar arxana-media--playback-process nil)
(defvar arxana-media--playback-stop-requested nil)
(defvar arxana-media--playback-queue nil)
(defvar arxana-media--playback-index nil)
(defvar arxana-media--playback-token 0)

(defcustom arxana-media-publications-root (expand-file-name "~/code/storage/publications/")
  "Directory used as a holding place for publication exports."
  :type 'directory
  :group 'arxana-media)

(defcustom arxana-media-misc-root (expand-file-name "~/code/storage/misc-audio/")
  "Directory containing misc audio working sets (non-Zoom sources)."
  :type 'directory
  :group 'arxana-media)

(defcustom arxana-media-publication-tag-prefix "publication:"
  "Prefix used when tagging catalog entries for publications."
  :type 'string
  :group 'arxana-media)

(defvar arxana-media--marked (make-hash-table :test 'equal))
(defvar arxana-media--lyrics-cache (make-hash-table :test 'equal))
(defvar arxana-media--misc-sha-cache (make-hash-table :test 'equal))
(defcustom arxana-media-publication-metadata-file "publication.json"
  "Filename used to store per-publication metadata inside an EP directory."
  :type 'string
  :group 'arxana-media)

(defun arxana-media--publication-audio-file-p (path)
  (and (stringp path)
       (string-match-p "\\.\\(mp3\\|wav\\|flac\\|ogg\\|m4a\\)\\'" (downcase path))))

(defun arxana-media--file-sha256 (path)
  "Return the SHA256 of PATH contents."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (secure-hash 'sha256 (current-buffer))))

(defun arxana-media--misc-sha256 (path)
  "Return the SHA256 for PATH with a small cache keyed by mtime."
  (let* ((attrs (and path (file-attributes path)))
         (mtime (and attrs (file-attribute-modification-time attrs)))
         (cached (and path (gethash path arxana-media--misc-sha-cache))))
    (if (and cached (equal (car cached) mtime))
        (cdr cached)
      (let ((sha (arxana-media--file-sha256 path)))
        (when path
          (puthash path (cons mtime sha) arxana-media--misc-sha-cache))
        sha))))

(defun arxana-media--track-title (item)
  (let ((type (plist-get item :type)))
    (cond
     ((eq type 'media-track)
      (let ((entry (plist-get item :entry)))
        (or (plist-get entry :title)
            (plist-get entry :base_name)
            (plist-get entry :sha256)
            "track")))
     ((memq type '(media-publication-track media-misc-track))
      (or (plist-get item :label)
          (and (plist-get item :path) (file-name-base (plist-get item :path)))
          "track"))
     (t "track"))))

(defun arxana-media--track-sha (item)
  (pcase (plist-get item :type)
    ('media-track
     (let ((entry (plist-get item :entry)))
       (or (plist-get item :sha256)
           (plist-get entry :sha256))))
    ('media-misc-track
     (let ((path (plist-get item :path)))
       (when (and path (file-readable-p path))
         (arxana-media--misc-sha256 path))))
    (_ nil)))

(defun arxana-media--track-entity-id (item)
  "Return a stable XTDB entity id for ITEM."
  (pcase (plist-get item :type)
    ('media-track
     (let* ((entry (plist-get item :entry))
            (sha (plist-get entry :sha256)))
       (unless sha
         (user-error "Track entry has no :sha256"))
       (format "arxana/media/zoom/%s" sha)))
    ('media-misc-track
     (let* ((path (plist-get item :path)))
       (unless (and path (file-readable-p path))
         (user-error "No readable media file found"))
       (format "arxana/media/misc/%s" (arxana-media--misc-sha256 path))))
    (_
     (user-error "Unsupported media item for lyrics"))))

(defun arxana-media--lyrics-entity-id (item)
  "Return the lyrics entity id for ITEM."
  (pcase (plist-get item :type)
    ('media-track
     (let ((sha (arxana-media--track-sha item)))
       (unless sha
         (user-error "Track entry has no :sha256"))
       (format "arxana/media-lyrics/zoom/%s" sha)))
    ('media-misc-track
     (let ((sha (arxana-media--track-sha item)))
       (unless sha
         (user-error "No readable media file found"))
       (format "arxana/media-lyrics/misc/%s" sha)))
    (_
     (user-error "Unsupported media item for lyrics"))))

(defun arxana-media--entity-source (entity)
  (let* ((source (and (listp entity)
                      (or (alist-get :source entity)
                          (alist-get 'source entity)
                          (alist-get :entity/source entity))))
         (version (and (listp entity) (alist-get :version entity)))
         (data (and (listp version) (alist-get :data version)))
         (data-source (and (listp data)
                           (or (alist-get :source data)
                               (alist-get 'source data)))))
    (or (and (stringp source)
             (not (string-empty-p source))
             (not (string= source "external"))
             source)
        data-source
        source
        "")))

(defun arxana-media--maybe-fix-utf8 (text)
  "Fix common UTF-8-as-Latin-1 mojibake when needed."
  (if (and (stringp text)
           (or (string-match-p "[\x80-\x9f]" text)
               (string-match-p "[Ãâ]" text)))
      (condition-case nil
          (decode-coding-string (string-make-unibyte text) 'utf-8)
        (error text))
    text))

(defun arxana-media--fetch-lyrics (entity-id)
  (let* ((response (ignore-errors (arxana-store-fetch-entity entity-id)))
         (entity (and (listp response) (alist-get :entity response)))
         (lyrics (arxana-media--entity-source entity)))
    (arxana-media--maybe-fix-utf8 (or lyrics ""))))

(defvar-local arxana-media--lyrics-context nil)

(defun arxana-media--lyrics-context (item)
  (let* ((type (plist-get item :type))
         (entry (plist-get item :entry))
         (path (plist-get item :path))
         (title (arxana-media--track-title item))
         (entity-id (arxana-media--track-entity-id item))
         (lyrics-id (arxana-media--lyrics-entity-id item))
         (source (if (eq type 'media-track) "zoom" "misc")))
    (list :item item
          :title title
          :path path
          :entity-id entity-id
          :lyrics-entity-id lyrics-id
          :source source
          :entry entry)))

(defvar arxana-media-lyrics-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") #'arxana-media-lyrics-save)
    map)
  "Keymap for `arxana-media-lyrics-mode'.")

(define-derived-mode arxana-media-lyrics-mode text-mode "Lyrics"
  "Major mode for editing media lyrics."
  (setq header-line-format "C-c C-s to save lyrics to XTDB"))

(defun arxana-media-lyrics-save ()
  "Save lyrics in the current buffer to XTDB."
  (interactive)
  (unless (arxana-store-ensure-sync)
    (user-error "Futon sync is disabled; enable futon4-enable-sync first"))
  (unless (and (listp arxana-media--lyrics-context)
               (plist-get arxana-media--lyrics-context :entity-id))
    (user-error "No lyrics context available"))
  (let* ((context arxana-media--lyrics-context)
         (entity-id (plist-get context :entity-id))
         (lyrics-id (plist-get context :lyrics-entity-id))
         (title (plist-get context :title))
         (path (plist-get context :path))
         (source (plist-get context :source))
         (entry (plist-get context :entry))
         (lyrics (string-trim-right (buffer-string)))
         (lyrics-name (format "%s (lyrics)" title)))
    (arxana-store-ensure-entity :id entity-id
                                :name title
                                :type "arxana/media-track"
                                :external-id entity-id)
    (arxana-store-ensure-entity :id lyrics-id
                                :name lyrics-name
                                :type "arxana/media-lyrics"
                                :external-id lyrics-id
                                :source lyrics)
    (arxana-store-create-relation :src entity-id
                                  :dst lyrics-id
                                  :label ":media/lyrics")
    (puthash lyrics-id (not (string-empty-p lyrics)) arxana-media--lyrics-cache)
    (message "Saved lyrics for %s" title)))

(defun arxana-media-edit-lyrics-at-point ()
  "Open a buffer to edit lyrics for the current media track."
  (interactive)
  (let* ((item (arxana-patterns--browser-item-at-point)))
    (unless (and item (memq (plist-get item :type) '(media-track media-misc-track)))
      (user-error "No media track at point"))
    (let* ((context (arxana-media--lyrics-context item))
           (title (plist-get context :title))
           (lyrics-id (plist-get context :lyrics-entity-id))
           (buffer (get-buffer-create (format "*Arxana Lyrics: %s*" title))))
      (with-current-buffer buffer
        (arxana-media-lyrics-mode)
        (setq-local arxana-media--lyrics-context context)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (arxana-media--fetch-lyrics lyrics-id))
          (goto-char (point-min))))
      (pop-to-buffer buffer))))

(defun arxana-media--publication-metadata-path (directory)
  (expand-file-name arxana-media-publication-metadata-file
                    (file-name-as-directory directory)))

(defun arxana-media--read-publication-metadata (directory)
  (let ((path (arxana-media--publication-metadata-path directory)))
    (when (file-readable-p path)
      (condition-case _err
          (let ((json-object-type 'plist)
                (json-array-type 'list)
                (json-key-type 'keyword))
            (json-read-file path))
        (error nil)))))

(defun arxana-media--write-publication-metadata (directory name url)
  (let* ((path (arxana-media--publication-metadata-path directory))
         (payload (list :name name
                        :url url
                        :updated_at (float-time (current-time)))))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert (json-encode payload))
      (insert "\n"))))

(defun arxana-media--publication-directories ()
  (let ((root (file-name-as-directory (expand-file-name arxana-media-publications-root))))
    (when (file-directory-p root)
      (seq-sort
       #'string<
       (seq-filter
        (lambda (path)
          (and (file-directory-p path)
               (not (member (file-name-nondirectory (directory-file-name path)) '("." "..")))))
        (directory-files root t nil t))))))

(defun arxana-media--misc-directories ()
  (let ((root (file-name-as-directory (expand-file-name arxana-media-misc-root))))
    (when (file-directory-p root)
      (seq-sort
       #'string<
       (seq-filter
        (lambda (path)
          (and (file-directory-p path)
               (not (member (file-name-nondirectory (directory-file-name path)) '("." "..")))))
        (directory-files root t nil t))))))

(defun arxana-media--publication-audio-files (directory)
  (when (file-directory-p directory)
    (seq-sort
     #'string<
     (seq-filter
      #'arxana-media--publication-audio-file-p
      (directory-files (file-name-as-directory directory) t nil t)))))

(defun arxana-media--publications-items ()
  (let ((dirs (or (arxana-media--publication-directories) '())))
    (if (null dirs)
        (list (list :type 'info
                    :label "No publications yet"
                    :description (format "Nothing under %s" (expand-file-name arxana-media-publications-root))
                    :message "Use P in the track list to publish marked tracks."))
      (mapcar (lambda (dir)
                (let* ((name (file-name-nondirectory (directory-file-name dir)))
                       (count (length (arxana-media--publication-audio-files dir)))
                       (meta (arxana-media--read-publication-metadata dir))
                       (url (and (listp meta) (plist-get meta :url))))
                  (list :type 'media-publication
                        :label name
                        :path dir
                        :url url
                        :count count
                        :description (format "%d track%s%s"
                                             count (if (= count 1) "" "s")
                                             (if (and url (stringp url) (not (string-empty-p url)))
                                                 (format " — %s" url)
                                               "")))))
              dirs))))

(defun arxana-media--publication-track-items (directory)
  (let* ((files (or (arxana-media--publication-audio-files directory) '())))
    (if (null files)
        (list (list :type 'info
                    :label "Empty publication"
                    :description "No audio files found in this directory."))
      (mapcar (lambda (path)
                (list :type 'media-publication-track
                      :label (file-name-base path)
                      :path path))
              files))))

(defun arxana-media--misc-items ()
  (let ((dirs (or (arxana-media--misc-directories) '())))
    (if (null dirs)
        (list (list :type 'info
                    :label "No misc audio yet"
                    :description (format "Nothing under %s" (expand-file-name arxana-media-misc-root))))
      (mapcar (lambda (dir)
                (let* ((name (file-name-nondirectory (directory-file-name dir)))
                       (count (length (arxana-media--publication-audio-files dir))))
                  (list :type 'media-misc-folder
                        :label name
                        :path dir
                        :count count
                        :description (format "%d track%s"
                                             count (if (= count 1) "" "s")))))
              dirs))))

(defun arxana-media--misc-track-items (directory)
  (let* ((files (or (arxana-media--publication-audio-files directory) '())))
    (if (null files)
        (list (list :type 'info
                    :label "Empty folder"
                    :description "No audio files found in this directory."))
      (mapcar (lambda (path)
                (list :type 'media-misc-track
                      :label (file-name-base path)
                      :path path))
              files))))

(defun arxana-media--publication-track-format ()
  [("Title" 48 t)
   ("File" 34 t)])

(defun arxana-media--publication-track-row (item)
  (let ((path (plist-get item :path)))
    (vector (or (plist-get item :label) "")
            (if (and path (stringp path))
                (file-name-nondirectory path)
              ""))))

(defun arxana-media--catalog-path ()
  (or arxana-media-index-path (arxana-media--locate-default-index)))

(defun arxana-media--guess-recorder-root (entry)
  "Return the recorder mount root for ENTRY, if it can be inferred."
  (let ((source (plist-get entry :source)))
    (when (and source (stringp source))
      (let ((pos (string-match "/R4_Project/" source)))
        (when pos
          (substring source 0 pos))))))

(defun arxana-media--track-play-path (entry)
  (let ((candidates (list (plist-get entry :mp3)
                          (plist-get entry :copied_to)
                          (plist-get entry :source))))
    (seq-find (lambda (path)
                (and path
                     (stringp path)
                     (file-readable-p path)))
              candidates)))

(defun arxana-media-stop-playback ()
  (interactive)
  (when (process-live-p arxana-media--playback-process)
    (setq arxana-media--playback-stop-requested t)
    (ignore-errors (kill-process arxana-media--playback-process)))
  (setq arxana-media--playback-process nil)
  (message "Playback stopped"))

(defun arxana-media-toggle-autoplay-next ()
  (interactive)
  (setq arxana-media-autoplay-next (not arxana-media-autoplay-next))
  (message "Autoplay next: %s" (if arxana-media-autoplay-next "on" "off")))

(defun arxana-media--current-playback-context ()
  (or arxana-patterns--browser-context
      (car arxana-patterns--browser-stack)))

(defun arxana-media--playback-queue-for-item (item)
  (let* ((type (plist-get item :type))
         (context (arxana-media--current-playback-context)))
    (cond
     ((eq type 'media-track)
      (let ((filter (and context (plist-get context :media-filter))))
        (when filter
          (arxana-media--track-items filter))))
     ((eq type 'media-publication-track)
      (let ((path (and context (plist-get context :publication-path))))
        (when path
          (arxana-media--publication-track-items path))))
     ((eq type 'media-misc-track)
      (let ((path (and context (plist-get context :misc-path))))
        (when path
          (arxana-media--misc-track-items path))))
     (t nil))))

(defun arxana-media--playback-index-for-item (queue item)
  (let ((needle-type (plist-get item :type)))
    (cl-position-if
     (lambda (candidate)
       (and (eq (plist-get candidate :type) needle-type)
            (cond
             ((eq needle-type 'media-track)
              (string= (or (plist-get candidate :sha256)
                           (plist-get (plist-get candidate :entry) :sha256)
                           "")
                       (or (plist-get item :sha256)
                           (plist-get (plist-get item :entry) :sha256)
                           "")))
             ((eq needle-type 'media-publication-track)
              (string= (or (plist-get candidate :path) "")
                       (or (plist-get item :path) "")))
             ((eq needle-type 'media-misc-track)
              (string= (or (plist-get candidate :path) "")
                       (or (plist-get item :path) "")))
             (t nil))))
     queue)))

(defun arxana-media--start-playback (path title &optional token)
  (let ((player (or arxana-media-player-program
                    (arxana-media--locate-player))))
    (unless player
      (user-error "No player found (install mpv/ffplay/afplay or customize arxana-media-player-program)"))
    (unless (and path (stringp path) (file-readable-p path))
      (user-error "No readable media file found for %s" title))
    (setq arxana-media--playback-stop-requested nil)
    (let* ((args
            (append
             (cond
              ((string-match-p "ffplay\\'" player) (list "-autoexit" "-nodisp"))
              ((string-match-p "mpv\\'" player) (list "--no-video" "--really-quiet"))
              (t nil))
             arxana-media-player-extra-args
             (list path)))
           (buffer (get-buffer-create "*arxana-media-play*"))
           (proc (apply #'start-process "arxana-media-play" buffer player args))
           (sentinel-token (or token arxana-media--playback-token)))
      (setq arxana-media--playback-process proc)
      (set-process-query-on-exit-flag proc nil)
      (set-process-sentinel
       proc
       (lambda (process _event)
         (when (and (eq (process-status process) 'exit)
                    (equal (process-exit-status process) 0)
                    arxana-media-autoplay-next
                    (not arxana-media--playback-stop-requested)
                    (equal sentinel-token arxana-media--playback-token))
           (arxana-media-play-next))))
      (message "Playing: %s" title))))

(defun arxana-media-play-next ()
  (interactive)
  (unless (and (listp arxana-media--playback-queue)
               (numberp arxana-media--playback-index))
    (user-error "No active playback queue"))
  (let ((next (1+ arxana-media--playback-index)))
    (unless (< next (length arxana-media--playback-queue))
      (message "End of queue")
      (setq arxana-media--playback-process nil)
      (cl-return-from arxana-media-play-next))
    (setq arxana-media--playback-index next)
    (let* ((item (nth next arxana-media--playback-queue))
           (type (plist-get item :type))
           (entry (plist-get item :entry))
           (sha (and entry (plist-get entry :sha256)))
           (title (cond
                   ((eq type 'media-publication-track)
                    (or (plist-get item :label)
                        (and (plist-get item :path) (file-name-base (plist-get item :path)))
                        "track"))
                   ((eq type 'media-misc-track)
                    (or (plist-get item :label)
                        (and (plist-get item :path) (file-name-base (plist-get item :path)))
                        "track"))
                   (t
                    (or (plist-get entry :title)
                        (plist-get entry :base_name)
                        sha
                        "track"))))
           (path (cond
                  ((eq type 'media-publication-track) (plist-get item :path))
                  ((eq type 'media-misc-track) (plist-get item :path))
                  (t (arxana-media--track-play-path entry)))))
      (when (process-live-p arxana-media--playback-process)
        (setq arxana-media--playback-stop-requested t)
        (ignore-errors (kill-process arxana-media--playback-process)))
      (arxana-media--start-playback path title arxana-media--playback-token))))

(defun arxana-media-play-at-point ()
  "Play the current media track using `arxana-media-player-program`."
  (interactive)
  (let* ((item (arxana-patterns--browser-item-at-point)))
    (unless (and item (memq (plist-get item :type) '(media-track media-publication-track media-misc-track)))
      (user-error "No playable track at point"))
    (let* ((type (plist-get item :type))
           (entry (plist-get item :entry))
           (sha (and entry (plist-get entry :sha256)))
           (title (cond
                   ((eq type 'media-publication-track)
                    (or (plist-get item :label) (and (plist-get item :path) (file-name-base (plist-get item :path))) "track"))
                   ((eq type 'media-misc-track)
                    (or (plist-get item :label) (and (plist-get item :path) (file-name-base (plist-get item :path))) "track"))
                   (t
                    (or (plist-get entry :title)
                        (plist-get entry :base_name)
                        sha
                        "track"))))
           (path (cond
                  ((eq type 'media-publication-track) (plist-get item :path))
                  ((eq type 'media-misc-track) (plist-get item :path))
                  (t (arxana-media--track-play-path entry)))))
      (when (process-live-p arxana-media--playback-process)
        (arxana-media-stop-playback))
      (setq arxana-media--playback-token (1+ arxana-media--playback-token))
      (setq arxana-media--playback-queue (arxana-media--playback-queue-for-item item))
      (setq arxana-media--playback-index
            (or (and arxana-media--playback-queue
                     (arxana-media--playback-index-for-item arxana-media--playback-queue item))
                0))
      (arxana-media--start-playback path title arxana-media--playback-token))))

(defun arxana-media--zoom-sync-args (&rest extra)
  (let* ((script (or arxana-media-zoom-sync-script
                     (arxana-media--locate-zoom-sync-script)))
         (log (arxana-media--catalog-path)))
    (unless (and script (file-readable-p script))
      (user-error "zoom_sync.py not found (customize arxana-media-zoom-sync-script)"))
    (unless (and log (file-readable-p log))
      (user-error "Media catalog JSON not readable (customize arxana-media-index-path)"))
    (append (list "python3" script "--log" log) extra)))

(defun arxana-media--retitle-track (entry title)
  (let* ((sha (plist-get entry :sha256))
         (status (downcase (or (plist-get entry :status) "hold")))
         (root (arxana-media--guess-recorder-root entry))
         (args (append (arxana-media--zoom-sync-args
                        "--title" (format "%s=%s" sha title)
                        "--title-manifest-status" "hold")
                       (when root (list "--source" root))))
         (buf (get-buffer-create "*arxana-media*"))
         (exit-code
          (progn
            (with-current-buffer buf
              (let ((inhibit-read-only t))
                (erase-buffer)))
            (apply #'process-file (car args) nil buf nil (cdr args)))))
    (unless (equal exit-code 0)
      (user-error "zoom_sync.py failed (see *arxana-media* buffer)"))
    (when (not (string= status "hold"))
      (message "[arxana-media] Note: retitled non-hold entry %s" sha))))

(defun arxana-media--entry-time (entry)
  (let ((timestamp (or (plist-get entry :recorded_at)
                       (plist-get entry :ingested_at))))
    (when (and timestamp (stringp timestamp) (> (length timestamp) 0))
      (ignore-errors
        (float-time (date-to-time timestamp))))))

(defun arxana-media--derive-project (entry)
  "Return a plist with project data from ENTRY :source path when possible."
  (let ((source (plist-get entry :source)))
    (when (and source (stringp source))
      (let* ((parts (split-string source "/" t))
             (index 0)
             folder project-path)
        (dolist (part parts)
          (when (and (null folder)
                     (string-match-p "\\`[0-9]\\{8\\}_[0-9]\\{3\\}\\'" part))
            (setq folder part)
            (let ((parent (and (> index 0) (nth (1- index) parts))))
              (setq project-path (if (and parent
                                          (string-match-p "\\`r4_project" (downcase parent)))
                                     (concat parent "/" part)
                                   part))))
          (setq index (1+ index)))
        (when folder
          (list :project-path project-path
                :project-folder folder))))))

(defun arxana-media--format-recorded (entry)
  "Return a friendly timestamp for ENTRY plists from the media catalog."
  (let ((recorded (plist-get entry :recorded_at)))
    (cond
     ((and recorded (stringp recorded) (> (length recorded) 0))
      (condition-case nil
          (format-time-string "%Y-%m-%d %H:%M" (date-to-time recorded))
        (error
         (if (> (length recorded) 16)
             (substring recorded 0 16)
           recorded))))
     ((plist-get entry :recorded_date))
     (t "?"))))

(defalias 'arxana-media-format-recorded #'arxana-media--format-recorded)

(defun arxana-media--normalize-entry (entry)
  (let* ((status (downcase (or (plist-get entry :status) "hold")))
         (derived (arxana-media--derive-project entry))
         (project-folder (or (plist-get entry :project_folder)
                             (plist-get derived :project-folder)))
         (project-path (or (plist-get entry :project_path)
                           (plist-get derived :project-path)))
         (recorder-project (or (plist-get entry :recorder_project)
                               project-folder
                               "Unknown project"))
         (project (or recorder-project
                      project-folder
                      "Unknown project"))
         (title (or (plist-get entry :title)
                    (plist-get entry :base_name)
                    (plist-get entry :sha256))))
    (setq entry (plist-put entry :status status))
    (when project-path
      (setq entry (plist-put entry :project_path project-path)))
    (when project-folder
      (setq entry (plist-put entry :project_folder project-folder)))
    (setq entry (plist-put entry :recorder_project recorder-project))
    (setq entry (plist-put entry :title title))
    entry))

(defun arxana-media--read-catalog (path)
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        (json-key-type 'keyword))
    (condition-case err
        (let* ((data (json-read-file path))
               (entries (plist-get data :entries)))
          (mapcar #'arxana-media--normalize-entry entries))
      (error
       (message "[arxana-media] Failed to read %s: %s" path err)
       nil))))

(defun arxana-media--entries ()
  (let ((path (arxana-media--catalog-path)))
    (if (and path (file-readable-p path))
        (let* ((attrs (file-attributes path))
               (mtime (file-attribute-modification-time attrs)))
          (unless (and arxana-media--catalog
                       arxana-media--catalog-mtime
                       (equal mtime arxana-media--catalog-mtime))
            (setq arxana-media--catalog (arxana-media--read-catalog path)
                  arxana-media--catalog-mtime mtime))
          arxana-media--catalog)
      (setq arxana-media--catalog nil
            arxana-media--catalog-mtime nil)
      (when path
        (message "[arxana-media] Catalog not readable at %s" path))
      nil)))

(defun arxana-media--status-items (entries)
  (let ((counts (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let ((status (or (plist-get entry :status) "hold")))
        (puthash status (1+ (gethash status counts 0)) counts)))
    (let* ((order '(("hold" . "Hold (Zoom + disk)")
                    ("archive" . "Archive (disk only)")
                    ("trash" . "Trash (ready to delete)")))
           (items nil))
      (dolist (entry order)
        (let* ((status (car entry))
               (label (cdr entry))
               (count (gethash status counts 0)))
          (when (> count 0)
            (push (list :type 'media-category
                        :label label
                        :description (format "%d track%s" count (if (= count 1) "" "s"))
                        :media-filter (cons 'status status)
                        :count count)
                  items)
            (remhash status counts))))
      (maphash (lambda (status count)
                 (push (list :type 'media-category
                             :label (capitalize status)
                             :description (format "%d track%s" count (if (= count 1) "" "s"))
                             :media-filter (cons 'status status)
                             :count count)
                       items))
               counts)
      (nreverse items))))

(defun arxana-media--project-items (entries)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (entry entries)
      (let* ((project (or (plist-get entry :recorder_project)
                          (plist-get entry :project_folder)
                          "Unknown project"))
             (time (or (arxana-media--entry-time entry) 0))
             (bucket (gethash project table)))
        (if bucket
            (progn
              (setf (aref bucket 0) (1+ (aref bucket 0)))
              (when (> time (aref bucket 1))
                (setf (aref bucket 1) time)
                (setf (aref bucket 2) (or (plist-get entry :recorded_at) "unknown"))))
          (puthash project
                   (vector 1 time (or (plist-get entry :recorded_at) "unknown"))
                   table))))
    (let (items)
      (maphash
       (lambda (project vec)
         (let* ((count (aref vec 0))
                (latest (aref vec 2)))
           (push (list :type 'media-project
                       :label project
                       :description (format "%d track%s — latest %s"
                                            count (if (= count 1) "" "s") latest)
                       :media-filter (cons 'project project)
                       :count count
                       :latest-time (aref vec 1))
                 items)))
       table)
      (seq-sort-by (lambda (item)
                     (- (or (plist-get item :latest-time) 0.0)))
                   #'<
                   items))))

(defun arxana-media--filter-entries (entries filter)
  (pcase filter
    ('all entries)
    (`(status . ,status)
     (seq-filter (lambda (entry)
                   (string= (or (plist-get entry :status) "hold") status))
                 entries))
    (`(project . ,project)
     (seq-filter (lambda (entry)
                   (string= (or (plist-get entry :recorder_project)
                                (plist-get entry :project_folder)
                                "")
                            project))
                 entries))
    (_ entries)))

(defun arxana-media--track-items (filter)
  (let* ((entries (or (arxana-media--entries) '()))
         (selected (arxana-media--filter-entries entries filter))
         (sorted (seq-sort (lambda (a b)
                             (> (or (arxana-media--entry-time a) 0)
                                (or (arxana-media--entry-time b) 0)))
                           selected)))
    (mapcar (lambda (entry)
              (list :type 'media-track
                    :entry entry
                    :sha256 (plist-get entry :sha256)
                    :label (or (plist-get entry :title)
                               (plist-get entry :base_name)
                               (plist-get entry :sha256))
                    :status (plist-get entry :status)
                    :project (or (plist-get entry :recorder_project)
                                 (plist-get entry :project_folder)
                                 "Unknown project")))
            sorted)))

(defun arxana-media--track-format ()
  [(" " 1 nil)
   ("Ly" 2 nil)
   ("Recorded" 18 t)
   ("Status" 8 t)
   ("Title" 40 t)
   ("Project" 20 t)])

(defun arxana-media--marked-p (sha)
  (and sha (gethash sha arxana-media--marked)))

(defun arxana-media--lyrics-present-p (lyrics-id)
  (let ((cached (gethash lyrics-id arxana-media--lyrics-cache 'unset)))
    (cond
     ((not (eq cached 'unset)) (eq cached t))
     ((not (arxana-store-sync-enabled-p)) nil)
     (t
      (let* ((response (ignore-errors (arxana-store-fetch-entity lyrics-id)))
             (entity (and (listp response) (alist-get :entity response)))
             (lyrics (arxana-media--entity-source entity))
             (present (and (stringp lyrics) (not (string-empty-p lyrics)))))
        (puthash lyrics-id (if present t 'none) arxana-media--lyrics-cache)
        present)))))

(defun arxana-media--lyrics-indicator (item)
  (condition-case nil
      (let ((lyrics-id (arxana-media--lyrics-entity-id item)))
        (if (and lyrics-id (arxana-media--lyrics-present-p lyrics-id)) "L" " "))
    (error " ")))

(defun arxana-media--track-row (item)
  (let* ((entry (plist-get item :entry))
         (sha (or (plist-get item :sha256) (plist-get entry :sha256)))
         (status (capitalize (or (plist-get entry :status) "-")))
         (project (or (plist-get entry :recorder_project)
                      (plist-get entry :project_folder)
                      "-"))
         (title (or (plist-get entry :title)
                    (plist-get entry :base_name)
                    (plist-get entry :sha256)))
         (recorded (arxana-media--format-recorded entry)))
    (vector (if (arxana-media--marked-p sha) "*" " ")
            (arxana-media--lyrics-indicator item)
            recorded status title project)))

(defun arxana-media--misc-track-format ()
  [("Ly" 2 nil)
   ("Title" 48 t)
   ("File" 34 t)])

(defun arxana-media--misc-track-row (item)
  (let ((path (plist-get item :path)))
    (vector (arxana-media--lyrics-indicator item)
            (or (plist-get item :label) "")
            (if (and path (stringp path))
                (file-name-nondirectory path)
              ""))))

(defun arxana-media-toggle-mark-at-point ()
  (interactive)
  (let* ((item (arxana-patterns--browser-item-at-point)))
    (unless (and item (eq (plist-get item :type) 'media-track))
      (user-error "No media track at point"))
    (let* ((sha (or (plist-get item :sha256)
                    (plist-get (plist-get item :entry) :sha256))))
      (unless sha
        (user-error "Track entry has no :sha256"))
      (if (arxana-media--marked-p sha)
          (remhash sha arxana-media--marked)
        (puthash sha t arxana-media--marked))
      (arxana-patterns--browser-render))))

(defun arxana-media-unmark-all ()
  (interactive)
  (clrhash arxana-media--marked)
  (arxana-patterns--browser-render)
  (message "Unmarked all tracks"))

(defun arxana-media--slug (value)
  (let* ((raw (downcase (string-trim (or value ""))))
         (clean (replace-regexp-in-string "[^a-z0-9]+" "-" raw))
         (trimmed (replace-regexp-in-string "\\`-+\\|-+\\'" "" clean)))
    (if (string-empty-p trimmed) "untitled" trimmed)))

(defun arxana-media--marked-tracks ()
  (let* ((entries (or (arxana-media--entries) '()))
         (want (make-hash-table :test 'equal)))
    (maphash (lambda (sha _flag) (puthash sha t want)) arxana-media--marked)
    (seq-filter (lambda (entry)
                  (gethash (plist-get entry :sha256) want))
                entries)))

(defun arxana-media--track-export-path (entry)
  (let ((candidates (list (plist-get entry :mp3)
                          (plist-get entry :copied_to)
                          (plist-get entry :source))))
    (seq-find (lambda (path)
                (and path
                     (stringp path)
                     (file-readable-p path)))
              candidates)))

(defun arxana-media--tag-entries (entries tag)
  (let ((args (apply #'append
                     (mapcar (lambda (entry)
                               (let ((sha (plist-get entry :sha256)))
                                 (unless sha
                                   (user-error "Entry missing :sha256"))
                                 (list "--tag" (format "%s=%s" sha tag))))
                             entries))))
    (let* ((cmd (apply #'arxana-media--zoom-sync-args args))
           (buf (get-buffer-create "*arxana-media*"))
           (exit-code
            (progn
              (with-current-buffer buf
                (let ((inhibit-read-only t))
                  (erase-buffer)))
              (apply #'process-file (car cmd) nil buf nil (cdr cmd)))))
      (unless (equal exit-code 0)
        (user-error "zoom_sync.py failed (see *arxana-media* buffer)")))))

(defun arxana-media-publish-marked (name &optional url)
  "Tag marked tracks for publication NAME and copy audio into the holding root.
When URL is provided, write it to the publication metadata."
  (interactive
   (let* ((pub-name (read-string "Publication name: "))
          (pub-url (read-string "Publication URL (optional): ")))
     (list pub-name pub-url)))
  (let* ((entries (arxana-media--marked-tracks)))
    (unless entries
      (user-error "No marked tracks"))
    (let* ((slug (arxana-media--slug name))
           (tag (concat arxana-media-publication-tag-prefix slug))
           (dest-dir (file-name-as-directory (expand-file-name slug arxana-media-publications-root))))
      (make-directory dest-dir t)
      (dolist (entry entries)
        (let* ((path (arxana-media--track-export-path entry))
               (title (or (plist-get entry :title)
                          (plist-get entry :base_name)
                          (plist-get entry :sha256)))
               (dest (and path (expand-file-name (file-name-nondirectory path) dest-dir))))
          (unless path
            (user-error "No readable media file found for %s" title))
          (copy-file path dest t)))
      (when (and url (stringp url) (not (string-empty-p url)))
        (arxana-media--write-publication-metadata dest-dir name url))
      (arxana-media--tag-entries entries tag)
      (setq arxana-media--catalog nil
            arxana-media--catalog-mtime nil)
      (message "Published %d track(s) to %s (tag %s)" (length entries) dest-dir tag)
      (arxana-patterns--browser-render))))

(defun arxana-media--publication-at-point ()
  (let ((item (arxana-patterns--browser-item-at-point)))
    (unless (and item (eq (plist-get item :type) 'media-publication))
      (user-error "No publication at point"))
    item))

(defun arxana-media-set-publication-url ()
  "Set or update the URL metadata for the publication at point."
  (interactive)
  (let* ((item (arxana-media--publication-at-point))
         (dir (plist-get item :path))
         (meta (and dir (arxana-media--read-publication-metadata dir)))
         (current (and meta (plist-get meta :url)))
         (label (plist-get item :label))
         (url (string-trim (read-string "Publication URL: " current))))
    (when (string-empty-p url)
      (user-error "URL cannot be empty"))
    (arxana-media--write-publication-metadata dir (or label "") url)
    (message "Updated URL for %s" label)
    (arxana-patterns--browser-render)))

(defun arxana-media-open-publication-url ()
  "Open the publication URL at point, if present."
  (interactive)
  (let* ((item (arxana-media--publication-at-point))
         (dir (plist-get item :path))
         (meta (and dir (arxana-media--read-publication-metadata dir)))
         (url (and meta (plist-get meta :url))))
    (if (and url (not (string-empty-p url)))
        (browse-url url)
      (user-error "No URL recorded for this publication"))))

(defun arxana-media-retitle-at-point ()
  "Retitle the current media track or misc audio file."
  (interactive)
  (let* ((item (arxana-patterns--browser-item-at-point)))
    (unless (and item (memq (plist-get item :type) '(media-track media-misc-track)))
      (user-error "No media track at point"))
    (pcase (plist-get item :type)
      ('media-track
       (let* ((entry (plist-get item :entry))
              (sha (plist-get entry :sha256))
              (status (downcase (or (plist-get entry :status) "hold")))
              (current (or (plist-get entry :title)
                           (plist-get entry :base_name)
                           "")))
         (unless sha
           (user-error "Track entry has no :sha256"))
         (unless (string= status "hold")
           (user-error "Only hold items can be retitled on the Zoom (status is %s)" status))
         (let ((title (string-trim (read-string "New title: " current))))
           (when (string-empty-p title)
             (user-error "Title cannot be empty"))
           (arxana-media--retitle-track entry title)
           (setq arxana-media--catalog nil
                 arxana-media--catalog-mtime nil)
           (arxana-patterns--browser-render)
           (message "Retitled %s" title))))
      ('media-misc-track
       (let* ((path (plist-get item :path))
              (current (or (plist-get item :label)
                           (and path (file-name-base path))
                           "")))
         (unless (and path (file-readable-p path))
           (user-error "No readable media file found"))
         (let* ((title (string-trim (read-string "New title: " current))))
           (when (string-empty-p title)
             (user-error "Title cannot be empty"))
           (let* ((dir (file-name-directory path))
                  (ext (or (file-name-extension path t) ""))
                  (slug (arxana-media--slug title))
                  (dest (expand-file-name (concat slug ext) dir)))
             (when (string= path dest)
               (user-error "Title matches existing filename"))
             (rename-file path dest)
             (arxana-patterns--browser-render)
             (message "Retitled %s" title))))))))

(defun arxana-media-delete-at-point ()
  "Hard-delete the current misc audio file from disk."
  (interactive)
  (let* ((item (arxana-patterns--browser-item-at-point)))
    (unless (and item (eq (plist-get item :type) 'media-misc-track))
      (user-error "Hard delete is only available for misc audio tracks"))
    (let* ((path (plist-get item :path))
           (label (or (plist-get item :label)
                      (and path (file-name-base path))
                      "track")))
      (unless (and path (file-exists-p path))
        (user-error "No file found for %s" label))
      (when (yes-or-no-p (format "Delete %s? " label))
        (delete-file path)
        (arxana-patterns--browser-render)
        (message "Deleted %s" label)))))

(provide 'arxana-media)

;;; arxana-media.el ends here
