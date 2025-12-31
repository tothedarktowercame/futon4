;;; arxana-media.el --- Futon media browser helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Media catalog browsing, playback, retitling, tagging, and publication helpers
;; used by the Futon Emacs browser.
;;
;; TODO(org-sync): Track this module in dev/docs-backlog.org for mirroring
;; into the literate sources.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'browse-url)
(require 'arxana-browser-marks)
(require 'arxana-store)

(defvar arxana-browser--context)
(defvar arxana-browser--stack)
(declare-function arxana-browser--item-at-point "arxana-browser-core")
(declare-function arxana-browser--current-items "arxana-browser-core")
(declare-function arxana-browser--render "arxana-browser-core")

(defgroup arxana-media nil
  "Media browsing, playback, and publication helpers."
  :group 'arxana)

(defconst arxana-media--source-root
  (let* ((source (or load-file-name buffer-file-name))
         (dir (and source (file-name-directory source))))
    (and dir (file-name-directory (directory-file-name dir))))
  "Filesystem root inferred from the location of this file.")

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

(defcustom arxana-media-zoom-root
  (or (getenv "ZOOM_SYNC_DEST")
      (expand-file-name "~/code/storage/zoomr4/"))
  "Root directory for Zoom R4 media files."
  :type 'directory
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
(defvar arxana-media--playback-player nil)

(defun arxana-media--playback-player-kind ()
  (cond
   ((and arxana-media--playback-player
         (string-match-p "ffplay\\'" arxana-media--playback-player)) 'ffplay)
   ((and arxana-media--playback-player
         (string-match-p "mpv\\'" arxana-media--playback-player)) 'mpv)
   (t nil)))

(defcustom arxana-media-publications-root (expand-file-name "~/code/storage/publications/")
  "Directory used as a holding place for publication exports."
  :type 'directory
  :group 'arxana-media)

(defcustom arxana-media-misc-root (expand-file-name "~/code/storage/misc-audio/")
  "Directory containing misc audio working sets (non-Zoom sources)."
  :type 'directory
  :group 'arxana-media)

(defcustom arxana-media-ep-staging-root
  (expand-file-name "ep-staging/" arxana-media-misc-root)
  "Directory containing EP-in-progress folders."
  :type 'directory
  :group 'arxana-media)

(defcustom arxana-media-bounce-profiles
  '(("vocal+banjo+bass+harp"
     :script "scripts/bounce_vocal_banjo_bass_harp.sh"
     :instruments ("vocal" "banjo" "bass" "harmonica"))
    ("vocal-forward+banjo+bass+harp"
     :script "scripts/bounce_vocal_banjo_bass_harp_vocal_forward.sh"
     :instruments ("vocal" "banjo" "bass" "harmonica"))
    ("vocal+bass+pbass+accordion" :script "scripts/bounce_vocal_bass_pbass_accordion.sh")
    ("vocal-forward+bass+pbass+accordion" :script "scripts/bounce_vocal_bass_pbass_accordion_vocal_forward.sh"))
  "Profiles for multi-track bounce workflows.
Each entry is (PROFILE-NAME :script PATH [:instruments (\"vocal\" ...)])."
  :type '(repeat sexp)
  :group 'arxana-media)

(defcustom arxana-media-bounce-instrument-aliases
  '(("harmonic" . "harmonica")
    ("harp" . "harmonica")
    ("vocals" . "vocal")
    ("vox" . "vocal"))
  "Aliases used when matching bounce instruments from filenames."
  :type '(repeat (cons (string :tag "Alias")
                       (string :tag "Canonical")))
  :group 'arxana-media)

(defcustom arxana-media-bounce-output-root nil
  "Default directory for bounced mixes.
When nil, use a \"bounces\" directory under `arxana-media-misc-root`."
  :type '(choice (const :tag "Use misc-audio bounces" nil)
                 directory)
  :group 'arxana-media)

(defcustom arxana-media-publication-tag-prefix "publication:"
  "Prefix used when tagging catalog entries for publications."
  :type 'string
  :group 'arxana-media)

(defvar-local arxana-media--marked (make-hash-table :test 'equal))
(defvar arxana-media--lyrics-cache (make-hash-table :test 'equal))
(defvar arxana-media--misc-sha-cache (make-hash-table :test 'equal))
(defvar arxana-media--duration-cache (make-hash-table :test 'equal))
(defcustom arxana-media-publication-metadata-file "publication.json"
  "Filename used to store per-publication metadata inside an EP directory."
  :type 'string
  :group 'arxana-media)

(defcustom arxana-media-duration-program (or (executable-find "ffprobe") nil)
  "Program used to probe audio duration (e.g., ffprobe)."
  :type '(choice (const :tag "None" nil)
                 string)
  :group 'arxana-media)

(defcustom arxana-media-audacity-program (or (executable-find "audacity") nil)
  "Program used to open audio files in Audacity."
  :type '(choice (const :tag "None" nil)
                 string)
  :group 'arxana-media)

(defcustom arxana-media-enable-audacity t
  "When non-nil, enable Audacity integration."
  :type 'boolean
  :group 'arxana-media)

(defcustom arxana-media-audacity-script-pipe-dir "/tmp"
  "Directory where Audacity mod-script-pipe creates its named pipes."
  :type 'directory
  :group 'arxana-media)

(defcustom arxana-media-audacity-use-script-pipe t
  "When non-nil, use Audacity's mod-script-pipe to import tracks into one project."
  :type 'boolean
  :group 'arxana-media)

(defcustom arxana-media-audacity-startup-wait 1.0
  "Seconds to wait after launching Audacity before checking script pipes."
  :type 'number
  :group 'arxana-media)

(defcustom arxana-media-audacity-always-launch t
  "When non-nil, launch Audacity before using script pipes."
  :type 'boolean
  :group 'arxana-media)

(defcustom arxana-media-audacity-send-timeout 0.2
  "Seconds to wait for Audacity script-pipe writes to complete."
  :type 'number
  :group 'arxana-media)

(defcustom arxana-media-audacity-fallback-to-args t
  "When non-nil, fall back to launching Audacity with file arguments."
  :type 'boolean
  :group 'arxana-media)

(defcustom arxana-media-misc-hash-notify-threshold (* 10 1024 1024)
  "File size (bytes) above which misc hashing emits a status message."
  :type 'integer
  :group 'arxana-media)

(defcustom arxana-media-lyrics-auto-hash-limit (* 25 1024 1024)
  "Max file size (bytes) to auto-hash for lyrics indicators."
  :type 'integer
  :group 'arxana-media)

(defface arxana-media-lyrics-serif
  '((t :inherit nil :family "TeX Gyre Termes"))
  "Serif face for chorded lyric lines."
  :group 'arxana-media)

(defface arxana-media-lyrics-serif-bold
  '((t :inherit nil :family "TeX Gyre Termes" :weight bold))
  "Serif bold face for chorded lyric lines."
  :group 'arxana-media)

(defface arxana-media-lyrics-serif-italic
  '((t :inherit nil :family "TeX Gyre Termes" :slant italic))
  "Serif italic face for chorded lyric lines."
  :group 'arxana-media)

(defface arxana-media-lyrics-serif-bold-italic
  '((t :inherit nil :family "TeX Gyre Termes" :weight bold :slant italic))
  "Serif bold italic face for chorded lyric lines."
  :group 'arxana-media)

(defface arxana-media-lyrics-sans
  '((t :inherit nil :family "TeX Gyre Heros"))
  "Sans face for chorded lyric lines."
  :group 'arxana-media)

(defface arxana-media-lyrics-sans-bold
  '((t :inherit nil :family "TeX Gyre Heros" :weight bold))
  "Sans bold face for chorded lyric lines."
  :group 'arxana-media)

(defface arxana-media-lyrics-sans-bold-italic
  '((t :inherit nil :family "TeX Gyre Heros" :weight bold :slant italic))
  "Sans bold italic face for chorded lyric lines."
  :group 'arxana-media)

(defface arxana-media-lyrics-script
  '((t :inherit nil :family "Allura"))
  "Script face for chorded lyric lines."
  :group 'arxana-media)

(defface arxana-media-lyrics-script-bold
  '((t :inherit nil :family "Parisienne" :weight bold))
  "Script bold face for chorded lyric lines."
  :group 'arxana-media)

(defface arxana-media-lyrics-blackletter
  '((t :inherit nil :family "UnifrakturMaguntia"))
  "Blackletter face for chorded lyric lines."
  :group 'arxana-media)

(defface arxana-media-lyrics-blackletter-bold
  '((t :inherit nil :family "UnifrakturMaguntia" :weight bold))
  "Blackletter bold face for chorded lyric lines."
  :group 'arxana-media)

(defface arxana-media-lyrics-mono
  '((t :inherit nil :family "Iosevka Term"))
  "Monospace face for chorded lyric lines."
  :group 'arxana-media)

(defcustom arxana-media-lyrics-chord-faces
  '(("A" . arxana-media-lyrics-blackletter)
    ("A#" . arxana-media-lyrics-blackletter-bold)
    ("B" . arxana-media-lyrics-mono)
    ("C" . arxana-media-lyrics-serif-bold)
    ("C#" . arxana-media-lyrics-serif-italic)
    ("D" . arxana-media-lyrics-serif-bold-italic)
    ("E" . arxana-media-lyrics-sans-bold)
    ("F" . arxana-media-lyrics-sans)
    ("F#" . arxana-media-lyrics-sans-bold-italic)
    ("G" . arxana-media-lyrics-script)
    ("G#" . arxana-media-lyrics-script-bold))
  "Chord-to-face mapping for lyrics display."
  :type '(repeat (cons (string :tag "Chord")
                       (face :tag "Face")))
  :group 'arxana-media)

(defconst arxana-media--lyrics-chord-regexp
  "^[[:space:]]*\\[\\([^]\n]+\\)\\][[:space:]]*\\(.*\\)"
  "Regex for chord-prefixed lyric lines.")

(defun arxana-media--normalize-chord (value)
  (save-match-data
    (let ((normalized (upcase (string-trim value))))
      (setq normalized (replace-regexp-in-string "\u266f" "#" normalized))
      (setq normalized (replace-regexp-in-string "\u266d" "b" normalized))
      (when (string-match "\\`\\([A-G][#b]?\\)" normalized)
        (match-string 1 normalized)))))

(defun arxana-media--lyrics-face-for-chord (chord)
  (when chord
    (cdr (assoc (arxana-media--normalize-chord chord)
                arxana-media-lyrics-chord-faces))))

(defun arxana-media--lyrics-chord-matcher (limit)
  (when (re-search-forward arxana-media--lyrics-chord-regexp limit t)
    (let ((face (arxana-media--lyrics-face-for-chord
                 (match-string-no-properties 1))))
      (when face
        (put-text-property (match-beginning 2) (match-end 2)
                           'font-lock-face face)))
    t))

(defun arxana-media--lyrics-apply-chord-faces (start end)
  "Apply chord-aware faces between START and END."
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (< (point) end)
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        (remove-text-properties line-start line-end
                                '(face nil font-lock-face nil))
        (let ((line (buffer-substring-no-properties line-start line-end)))
          (when (string-match arxana-media--lyrics-chord-regexp line)
            (let* ((chord (match-string 1 line))
                   (body-start (match-beginning 2))
                   (body-end (match-end 2))
                   (face (arxana-media--lyrics-face-for-chord chord)))
              (when (and face body-start body-end)
                (add-text-properties (+ line-start body-start)
                                     (+ line-start body-end)
                                     (list 'face face))))))
        (forward-line 1)))))

(defun arxana-media--lyrics-refresh-region (start end &optional _len)
  "Refresh chord-aware faces around a change from START to END."
  (arxana-media--lyrics-apply-chord-faces start end))

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
         (cached (and path (gethash path arxana-media--misc-sha-cache)))
         (size (and attrs (file-attribute-size attrs)))
         (notify (and size (>= size arxana-media-misc-hash-notify-threshold))))
    (if (and cached (equal (car cached) mtime))
        (cdr cached)
      (when notify
        (message "[arxana-media] Hashing %s (%.1f MB)..."
                 (file-name-nondirectory path)
                 (/ size 1048576.0)))
      (let ((sha (arxana-media--file-sha256 path)))
        (when path
          (puthash path (cons mtime sha) arxana-media--misc-sha-cache))
        (when notify
          (message "[arxana-media] Hashing %s...done"
                   (file-name-nondirectory path)))
        sha))))

(defun arxana-media--maybe-cache-misc-sha (path)
  (when (and path (file-readable-p path))
    (let* ((attrs (file-attributes path))
           (size (and attrs (file-attribute-size attrs)))
           (cached (gethash path arxana-media--misc-sha-cache)))
      (cond
       (cached (cdr cached))
       ((and size (<= size arxana-media-lyrics-auto-hash-limit))
        (arxana-media--misc-sha256 path))
       (t nil)))))

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
    ('media-publication-track
     (let ((path (plist-get item :path)))
       (when (and path (file-readable-p path))
         (arxana-media--misc-sha256 path))))
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
    ('media-publication-track
     (let* ((path (plist-get item :path)))
       (unless (and path (file-readable-p path))
         (user-error "No readable media file found"))
       (format "arxana/media/misc/%s" (arxana-media--misc-sha256 path))))
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
    ('media-publication-track
     (let ((sha (arxana-media--track-sha item)))
       (unless sha
         (user-error "No readable media file found"))
       (format "arxana/media-lyrics/misc/%s" sha)))
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
  (setq header-line-format "C-c C-s to save lyrics to XTDB")
  (add-hook 'after-change-functions #'arxana-media--lyrics-refresh-region nil t)
  (arxana-media--lyrics-apply-chord-faces (point-min) (point-max)))

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
    (when (fboundp 'arxana-browser--render)
      (arxana-browser--render))
    (message "Saved lyrics for %s" title)))

(defun arxana-media-edit-lyrics-at-point ()
  "Open a buffer to edit lyrics for the current media track."
  (interactive)
  (let* ((item (arxana-browser--item-at-point)))
    (unless (and item (memq (plist-get item :type)
                            '(media-track media-misc-track media-publication-track)))
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
          (goto-char (point-min))
          (arxana-media--lyrics-apply-chord-faces (point-min) (point-max))))
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

(defun arxana-media--ep-staging-directories ()
  (let ((root (file-name-as-directory (expand-file-name arxana-media-ep-staging-root))))
    (when (file-directory-p root)
      (seq-sort
       #'string<
       (seq-filter
        (lambda (path)
          (and (file-directory-p path)
               (not (member (file-name-nondirectory (directory-file-name path)) '("." "..")))))
        (directory-files root t nil t))))))

(defun arxana-media--ep-staging-items ()
  (let ((dirs (or (arxana-media--ep-staging-directories) '())))
    (if (null dirs)
        (list (list :type 'info
                    :label "No EPs in staging"
                    :description (format "Nothing under %s"
                                         (expand-file-name arxana-media-ep-staging-root))))
      (mapcar (lambda (dir)
                (let* ((name (file-name-nondirectory (directory-file-name dir)))
                       (count (length (arxana-media--publication-audio-files dir))))
                  (list :type 'media-ep-staging
                        :label name
                        :path dir
                        :count count
                        :description (format "%d track%s"
                                             count (if (= count 1) "" "s")))))
              dirs))))

(defun arxana-media--prompt-ep-directory ()
  (let* ((dirs (or (arxana-media--ep-staging-directories) '()))
         (names (mapcar (lambda (dir)
                          (file-name-nondirectory (directory-file-name dir)))
                        dirs))
         (create-label "Create new...")
         (choice (if names
                     (completing-read "EP in progress (staging): "
                                      (append names (list create-label)) nil t)
                   create-label)))
    (if (string= choice create-label)
        (let* ((name (string-trim (read-string "New EP name: "))))
          (when (string-empty-p name)
            (user-error "EP name cannot be empty"))
          (let* ((slug (arxana-media--slug name))
                 (dest (file-name-as-directory
                        (expand-file-name slug arxana-media-ep-staging-root))))
            (make-directory dest t)
            (arxana-media--write-publication-metadata dest name "")
            dest))
      (file-name-as-directory (expand-file-name choice arxana-media-ep-staging-root)))))

(defun arxana-media--misc-directories ()
  (let ((root (file-name-as-directory (expand-file-name arxana-media-misc-root))))
    (when (file-directory-p root)
      (seq-sort
       #'string<
       (seq-filter
        (lambda (path)
          (and (file-directory-p path)
               (not (member (file-name-nondirectory (directory-file-name path)) '("." "..")))
               (not (and arxana-media-ep-staging-root
                         (string= (file-name-as-directory (expand-file-name path))
                                  (file-name-as-directory (expand-file-name arxana-media-ep-staging-root)))))))
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
  [(" " 1 nil)
   ("Ly" 2 nil)
   ("Title" 40 t)
   ("Len" 6 nil)
   ("File" 34 t)])

(defun arxana-media--publication-track-row (item)
  (let ((path (plist-get item :path)))
    (vector (if (arxana-media--marked-p path) "*" " ")
            (arxana-media--lyrics-indicator item)
            (or (plist-get item :label) "")
            (or (arxana-media--format-duration
                 (arxana-media--duration-seconds path))
                "")
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
    (seq-find #'identity
              (mapcar #'arxana-media--resolve-zoom-path candidates))))

(defun arxana-media--zoom-candidate-paths (path root catalog-root parent)
  (let* ((prefixes '("media/zoomr4/" "media/zoomr4\\" "zoomr4/" "zoomr4\\"))
         (stripped (cl-loop for prefix in prefixes
                            when (string-prefix-p prefix path)
                            return (substring path (length prefix))))
         (bases (delq nil (list root catalog-root parent)))
         (candidates (mapcar (lambda (base) (expand-file-name path base)) bases)))
    (when stripped
      (setq candidates
            (append candidates
                    (mapcar (lambda (base) (expand-file-name stripped base)) bases))))
    candidates))

(defun arxana-media--resolve-zoom-path (path)
  (when (and path (stringp path))
    (cond
     ((and (file-name-absolute-p path) (file-readable-p path))
      path)
     ((file-name-absolute-p path)
      nil)
     (t
      (let* ((root (and arxana-media-zoom-root
                        (file-name-as-directory (expand-file-name arxana-media-zoom-root))))
             (parent (and root (file-name-directory (directory-file-name root))))
             (catalog (arxana-media--catalog-path))
             (catalog-root (and catalog
                                (file-name-directory (directory-file-name (file-name-directory catalog)))))
             (candidates (arxana-media--zoom-candidate-paths path root catalog-root parent)))
        (seq-find #'file-readable-p candidates))))))

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
  (or arxana-browser--context
      (car arxana-browser--stack)))

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
              ((string-match-p "mpv\\'" player)
               (list "--no-video"
                     "--really-quiet"))
              (t nil))
             arxana-media-player-extra-args
             (list path)))
           (buffer (get-buffer-create "*arxana-media-play*"))
           (use-pty (and player (string-match-p "\\(ffplay\\|mpv\\)\\'" player)))
           (proc (let ((process-connection-type use-pty))
                   (apply #'start-process "arxana-media-play" buffer player args)))
           (sentinel-token (or token arxana-media--playback-token)))
      (setq arxana-media--playback-process proc)
      (setq arxana-media--playback-player player)
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

(defun arxana-media--playback-send (payload &optional label)
  (let ((proc arxana-media--playback-process))
    (cond
     ((not (and proc (process-live-p proc)))
      (user-error "No active playback process"))
     ((not (arxana-media--playback-player-kind))
      (user-error "Playback controls are only available for ffplay/mpv"))
     (t
      (let ((kind (arxana-media--playback-player-kind)))
        (process-send-string proc (if (and (eq kind 'ffplay)
                                           (not (string-suffix-p "\n" payload)))
                                      (concat payload "\n")
                                    payload)))
      (when label
        (message "%s" label))))))

(defun arxana-media-playback-pause-toggle ()
  "Toggle pause for active ffplay playback."
  (interactive)
  (pcase (arxana-media--playback-player-kind)
    ('ffplay (arxana-media--playback-send "p" "Toggle pause"))
    ('mpv (arxana-media--playback-send " " "Toggle pause"))
    (_ (arxana-media--playback-send " " "Toggle pause"))))

(defun arxana-media-playback-seek-back-10 ()
  "Seek backward 10 seconds in ffplay."
  (interactive)
  (arxana-media--playback-send "\e[D" "Seek -10s"))

(defun arxana-media-playback-seek-forward-10 ()
  "Seek forward 10 seconds in ffplay."
  (interactive)
  (arxana-media--playback-send "\e[C" "Seek +10s"))

(defun arxana-media-playback-seek-back-30 ()
  "Seek backward 30 seconds in ffplay."
  (interactive)
  (dotimes (_ 3)
    (arxana-media--playback-send "\e[D"))
  (message "Seek -30s"))

(defun arxana-media-playback-seek-forward-30 ()
  "Seek forward 30 seconds in ffplay."
  (interactive)
  (dotimes (_ 3)
    (arxana-media--playback-send "\e[C"))
  (message "Seek +30s"))

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
  (let* ((item (arxana-browser--item-at-point)))
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

(defun arxana-media--items ()
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
        (let* ((ep-root (file-name-as-directory (expand-file-name arxana-media-ep-staging-root)))
               (ep-dirs (or (arxana-media--ep-staging-directories) '()))
               (ep-count (length ep-dirs)))
          (setq items
                (append items
                        (list (list :type 'menu
                                    :label "EP staging"
                                    :description (format "%d EP%s — %s"
                                                         ep-count
                                                         (if (= ep-count 1) "" "s")
                                                         ep-root)
                                    :view 'media-ep-staging)))))
        (let* ((misc-root (and (boundp 'arxana-media-misc-root)
                               arxana-media-misc-root))
               (misc-root (and misc-root (file-name-as-directory (expand-file-name misc-root))))
               (misc-ready (and misc-root (fboundp 'arxana-media--misc-directories)))
               (misc-dirs (if misc-ready (or (arxana-media--misc-directories) '()) '()))
               (misc-count (length misc-dirs))
               (misc-description (cond
                                  (misc-ready
                                   (format "%d folder%s — %s"
                                           misc-count
                                           (if (= misc-count 1) "" "s")
                                           misc-root))
                                  (misc-root
                                   "Reload arxana-media to enable misc audio.")
                                  (t
                                   "Set arxana-media-misc-root to enable misc audio."))))
          (setq items
                (append items
                        (list (list :type 'menu
                                    :label "Misc Audio"
                                    :description misc-description
                                    :view (and misc-ready 'media-misc))))))
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
   ("Len" 6 nil)
   ("Title" 40 t)
   ("Project" 20 t)])

(defun arxana-media--marked-p (key)
  (and key (gethash key arxana-media--marked)))

(defun arxana-media--mark-key-for-item (item)
  (pcase (plist-get item :type)
    ('media-track
     (or (plist-get item :sha256)
         (plist-get (plist-get item :entry) :sha256)))
    ('media-publication-track
     (plist-get item :path))
    ('media-misc-track
     (plist-get item :path))
    (_ nil)))

(defun arxana-media--duration-seconds (path)
  "Return duration in seconds for PATH, or nil."
  (let ((program (if (boundp 'arxana-media-duration-program)
                     arxana-media-duration-program
                   (executable-find "ffprobe"))))
    (when (and path (stringp path)
               program
               (file-readable-p path))
      (let* ((attrs (file-attributes path))
             (mtime (and attrs (file-attribute-modification-time attrs)))
             (cached (gethash path arxana-media--duration-cache))
             (cached-mtime (car-safe cached))
             (cached-seconds (cdr-safe cached)))
        (if (and cached (equal cached-mtime mtime))
            cached-seconds
          (with-temp-buffer
            (let ((exit-code (process-file program nil t nil
                                           "-v" "error"
                                           "-show_entries" "format=duration"
                                           "-of" "default=noprint_wrappers=1:nokey=1"
                                           path)))
              (when (and (equal exit-code 0)
                         (> (buffer-size) 0))
                (let* ((text (string-trim (buffer-string)))
                       (seconds (ignore-errors (string-to-number text))))
                  (when (and seconds (> seconds 0))
                    (puthash path (cons mtime seconds) arxana-media--duration-cache)
                    seconds))))))))))

(defun arxana-media--format-duration (seconds)
  (when (and seconds (numberp seconds))
    (let* ((total (floor seconds))
           (mins (/ total 60))
           (secs (% total 60))
           (hours (/ mins 60))
           (mins-rem (% mins 60)))
      (if (> hours 0)
          (format "%d:%02d:%02d" hours mins-rem secs)
        (format "%d:%02d" mins secs)))))

(defun arxana-media--entry-duration (entry)
  (let ((path (arxana-media--track-play-path entry)))
    (arxana-media--duration-seconds path)))

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
      (pcase (plist-get item :type)
        ('media-publication-track
         (let* ((path (plist-get item :path))
                (sha (arxana-media--maybe-cache-misc-sha path))
                (lyrics-id (and sha (format "arxana/media-lyrics/misc/%s" sha))))
           (if (and lyrics-id (arxana-media--lyrics-present-p lyrics-id)) "L" " ")))
        ('media-misc-track
         (let* ((path (plist-get item :path))
                (sha (arxana-media--maybe-cache-misc-sha path))
                (lyrics-id (and sha (format "arxana/media-lyrics/misc/%s" sha))))
           (if (and lyrics-id (arxana-media--lyrics-present-p lyrics-id)) "L" " ")))
        (_
         (let ((lyrics-id (arxana-media--lyrics-entity-id item)))
           (if (and lyrics-id (arxana-media--lyrics-present-p lyrics-id)) "L" " "))))
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
         (recorded (arxana-media--format-recorded entry))
         (duration (arxana-media--format-duration
                    (arxana-media--entry-duration entry))))
    (vector (if (arxana-media--marked-p sha) "*" " ")
            (arxana-media--lyrics-indicator item)
            recorded status duration title project)))

(defun arxana-media--misc-track-format ()
  [(" " 1 nil)
   ("Ly" 2 nil)
   ("Title" 40 t)
   ("Len" 6 nil)
   ("File" 34 t)])

(defun arxana-media--misc-track-row (item)
  (let ((path (plist-get item :path)))
    (vector (if (arxana-media--marked-p path) "*" " ")
            (arxana-media--lyrics-indicator item)
            (or (plist-get item :label) "")
            (or (arxana-media--format-duration
                 (arxana-media--duration-seconds path))
                "")
            (if (and path (stringp path))
                (file-name-nondirectory path)
              ""))))

(defun arxana-media-toggle-mark-at-point ()
  (interactive)
  (let* ((item (arxana-browser--item-at-point)))
    (unless (and item (memq (plist-get item :type)
                            '(media-track media-misc-track media-publication-track)))
      (user-error "No media track at point"))
    (let* ((key (arxana-media--mark-key-for-item item)))
      (unless key
        (user-error "Track entry has no markable key"))
      (if (arxana-media--marked-p key)
          (remhash key arxana-media--marked)
        (puthash key t arxana-media--marked))
      (arxana-browser--render))))

(defun arxana-media-unmark-all ()
  (interactive)
  (clrhash arxana-media--marked)
  (arxana-browser--render)
  (message "Unmarked all tracks"))

(defun arxana-media--marked-items-in-context ()
  (arxana-browser-marks-items-in-context
   arxana-media--marked
   #'arxana-media--mark-key-for-item))

(defun arxana-media--marked-misc-items-in-context ()
  (arxana-browser-marks-items-in-context
   arxana-media--marked
   #'arxana-media--mark-key-for-item
   (lambda (item)
     (eq (plist-get item :type) 'media-misc-track))))

(defun arxana-media--slug (value)
  (let* ((raw (downcase (string-trim (or value ""))))
         (clean (replace-regexp-in-string "[^a-z0-9]+" "-" raw))
         (trimmed (replace-regexp-in-string "\\`-+\\|-+\\'" "" clean)))
    (if (string-empty-p trimmed) "untitled" trimmed)))

(defun arxana-media--marked-track-entries-in-context ()
  (let ((items (arxana-media--marked-items-in-context))
        (entries nil))
    (dolist (item items)
      (when (eq (plist-get item :type) 'media-track)
        (let ((entry (plist-get item :entry)))
          (when entry (push entry entries)))))
    (nreverse entries)))

(defun arxana-media--status-prompt (&optional default)
  (let* ((choices '("hold" "archive" "trash"))
         (default (or default "hold")))
    (completing-read "Set status (hold/archive/trash): " choices nil t nil nil default)))

(defun arxana-media--apply-status (entries status)
  (let* ((normalized (downcase status))
         (assignments (delq nil
                            (mapcar (lambda (entry)
                                      (let ((sha (plist-get entry :sha256)))
                                        (when sha (format "%s=%s" sha normalized))))
                                    entries))))
    (when assignments
      (let* ((args (append (arxana-media--zoom-sync-args)
                           (apply #'append (mapcar (lambda (spec) (list "--status" spec))
                                                   assignments))))
             (buf (get-buffer-create "*arxana-media*"))
             (exit-code
              (progn
                (with-current-buffer buf
                  (let ((inhibit-read-only t))
                    (erase-buffer)))
                (apply #'process-file (car args) nil buf nil (cdr args)))))
        (unless (equal exit-code 0)
          (user-error "zoom_sync.py failed (see *arxana-media* buffer)"))
        (setq arxana-media--catalog nil
              arxana-media--catalog-mtime nil)
        (message "Updated status for %d recording%s"
                 (length assignments)
                 (if (= (length assignments) 1) "" "s"))))))

(defun arxana-media-set-status-marked (status)
  "Set STATUS for all marked Zoom tracks (or the track at point if none marked)."
  (interactive (list (arxana-media--status-prompt)))
  (let* ((visible-marked (arxana-media--marked-track-entries-in-context))
         (entries nil))
    (when (and visible-marked (> (length visible-marked) 0))
      (unless (yes-or-no-p (format "Apply status to %d marked track%s? "
                                   (length visible-marked)
                                   (if (= (length visible-marked) 1) "" "s")))
        (setq visible-marked nil))
      (setq entries visible-marked))
    (unless entries
      (let ((item (arxana-browser--item-at-point)))
        (unless (and item (eq (plist-get item :type) 'media-track))
          (user-error "No marked tracks or media track at point"))
        (setq entries (list (plist-get item :entry)))))
    (arxana-media--apply-status entries status)
    (clrhash arxana-media--marked)
    (arxana-browser--render)))

(defun arxana-media--item-play-path (item)
  (pcase (plist-get item :type)
    ('media-track
     (arxana-media--track-play-path (plist-get item :entry)))
    ('media-misc-track
     (plist-get item :path))
    ('media-publication-track
     (plist-get item :path))
    (_ nil)))

(defun arxana-media-open-in-audacity ()
  "Open marked media tracks (or the track at point) in Audacity."
  (interactive)
  (unless arxana-media-enable-audacity
    (user-error "Audacity integration is disabled"))
  (unless arxana-media-audacity-program
    (user-error "Audacity not found (customize arxana-media-audacity-program)"))
  (let* ((marked (seq-filter (lambda (item)
                               (memq (plist-get item :type)
                                     '(media-track media-misc-track media-publication-track)))
                             (arxana-media--marked-items-in-context)))
         (items nil)
         (paths nil))
    (if (and marked (> (length marked) 0))
        (setq items marked)
      (let ((item (arxana-browser--item-at-point)))
        (unless (and item (memq (plist-get item :type)
                                '(media-track media-misc-track media-publication-track)))
          (user-error "No media track at point"))
        (setq items (list item))))
    (dolist (item items)
      (let ((path (arxana-media--item-play-path item)))
        (when (and path (file-readable-p path))
          (push path paths))))
    (setq paths (nreverse paths))
    (unless paths
      (user-error "No readable media files for Audacity"))
    (let ((result (and arxana-media-audacity-use-script-pipe
                       (arxana-media--audacity-import-via-pipe paths))))
      (cond
       ((eq result t)
        (message "Imported %d track%s into Audacity" (length paths) (if (= (length paths) 1) "" "s")))
       ((eq result :no-pipes)
        (message "Audacity script pipes not available; enable mod-script-pipe and restart Audacity."))
       ((eq result :send-failed)
        (message "Audacity script pipes not responding; enable mod-script-pipe and restart Audacity.")))
      (cond
       ((eq result t) nil)
       (arxana-media-audacity-fallback-to-args
        (apply #'start-process "arxana-audacity" nil arxana-media-audacity-program paths)
        (message "Opening %d track%s in Audacity" (length paths) (if (= (length paths) 1) "" "s")))
       (t
        (user-error "Audacity script pipes unavailable; see messages for setup steps"))))))

(defun arxana-media--audacity-pipe-paths ()
  (let* ((dir (file-name-as-directory (expand-file-name arxana-media-audacity-script-pipe-dir)))
         (candidates (list (user-login-name) (number-to-string (user-uid))))
         (stem (or (seq-find (lambda (candidate)
                               (let ((to (expand-file-name (format "audacity_script_pipe.to.%s" candidate) dir))
                                     (from (expand-file-name (format "audacity_script_pipe.from.%s" candidate) dir)))
                                 (and (file-exists-p to) (file-exists-p from))))
                             candidates)
                   (car candidates)))
         (to (expand-file-name (format "audacity_script_pipe.to.%s" stem) dir))
         (from (expand-file-name (format "audacity_script_pipe.from.%s" stem) dir)))
    (list :to to :from from)))

(defun arxana-media--audacity-pipes-ready-p (paths)
  (and (file-exists-p (plist-get paths :to))
       (file-exists-p (plist-get paths :from))))

(defun arxana-media--audacity-send (pipe message)
  (when (and pipe (file-exists-p pipe))
    (let ((proc (make-process :name "arxana-audacity-pipe"
                              :buffer nil
                              :command (list "sh" "-c" (format "cat > %s" (shell-quote-argument pipe)))
                              :noquery t))
          (deadline (+ (float-time) arxana-media-audacity-send-timeout)))
      (process-send-string proc (concat message "\n"))
      (process-send-eof proc)
      (while (and (process-live-p proc)
                  (< (float-time) deadline))
        (accept-process-output proc 0.02))
      (when (process-live-p proc)
        (delete-process proc)
        nil))))

(defun arxana-media--audacity-read-response (_pipe)
  nil)

(defun arxana-media--audacity-import-via-pipe (paths)
  "Try importing PATHS into a single Audacity project via mod-script-pipe."
  (let* ((pipes (arxana-media--audacity-pipe-paths))
         (to (plist-get pipes :to))
         (from (plist-get pipes :from)))
    (when arxana-media-audacity-always-launch
      (apply #'start-process "arxana-audacity" nil arxana-media-audacity-program nil)
      (sleep-for arxana-media-audacity-startup-wait))
    (unless (arxana-media--audacity-pipes-ready-p pipes)
      (apply #'start-process "arxana-audacity" nil arxana-media-audacity-program nil)
      (sleep-for arxana-media-audacity-startup-wait))
    (if (not (arxana-media--audacity-pipes-ready-p pipes))
        :no-pipes
      (if (and (arxana-media--audacity-send to "New:")
               (let ((ok t))
                 (dolist (path paths)
                   (setq ok (and ok
                                 (arxana-media--audacity-send
                                  to
                                  (format "Import2: Filename=\"%s\"" (expand-file-name path))))))
                 (when ok
                   (arxana-media--audacity-read-response from))
                 ok))
          t
        :send-failed))))

(defun arxana-media--track-export-path (entry)
  (let ((candidates (list (plist-get entry :mp3)
                          (plist-get entry :copied_to)
                          (plist-get entry :source))))
    (seq-find #'identity
              (mapcar #'arxana-media--resolve-zoom-path candidates))))

(defun arxana-media--project-root ()
  (let* ((base (or (and load-file-name (file-name-directory load-file-name))
                   (and buffer-file-name (file-name-directory buffer-file-name))
                   default-directory)))
    (or (locate-dominating-file base ".git")
        base)))

(defun arxana-media--script-path (path)
  (if (file-name-absolute-p path)
      path
    (let* ((base (or load-file-name buffer-file-name default-directory))
           (dir (and base (file-name-directory base)))
           (local-root (and dir (file-name-directory (directory-file-name dir))))
           (candidates (delq nil (list (and arxana-media--source-root
                                           (expand-file-name path arxana-media--source-root))
                                       (and local-root (expand-file-name path local-root))
                                       (expand-file-name path (arxana-media--project-root))))))
      (or (seq-find #'file-exists-p candidates)
          (car candidates)
          path))))

(defun arxana-media--bounce-profile-choice ()
  (let* ((choices (mapcar #'car arxana-media-bounce-profiles))
         (choice (completing-read "Bounce profile: " choices nil t)))
    (assoc choice arxana-media-bounce-profiles)))

(defun arxana-media--bounce-output-root ()
  (file-name-as-directory
   (expand-file-name
    (or arxana-media-bounce-output-root
        (expand-file-name "bounces" arxana-media-misc-root)))))

(defun arxana-media--bounce-input-path (entry)
  (let ((candidates (delq nil (list (plist-get entry :source)
                                    (plist-get entry :mp3)
                                    (plist-get entry :copied_to))))
        (paths nil))
    (dolist (candidate candidates)
      (when (and candidate (stringp candidate))
        (let ((resolved (arxana-media--resolve-zoom-path candidate)))
          (when resolved (push resolved paths)))
        (when (string-match-p "\\.mp3\\'" (downcase candidate))
          (let* ((wav (concat (file-name-sans-extension candidate) ".wav"))
                 (wav-res (arxana-media--resolve-zoom-path wav)))
            (when wav-res (push wav-res paths))))
        (when (string-match-p "/mp3/" candidate)
          (let* ((wav (replace-regexp-in-string "/mp3/" "/wav/" candidate))
                 (wav (if (string-match-p "\\.mp3\\'" (downcase wav))
                          (concat (file-name-sans-extension wav) ".wav")
                        wav))
                 (wav-res (arxana-media--resolve-zoom-path wav)))
            (when wav-res (push wav-res paths))))))
    (seq-find (lambda (path)
                (and path (file-readable-p path)))
              (nreverse paths))))

(defun arxana-media--bounce-entry-tokens (entry &optional input-path)
  (let* ((candidates (delq nil (list (plist-get entry :title)
                                     (plist-get entry :base_name)
                                     (plist-get entry :source)
                                     (plist-get entry :name)
                                     (and input-path
                                          (file-name-nondirectory input-path)))))
         (tokens (make-hash-table :test 'equal)))
    (dolist (candidate candidates)
      (when (stringp candidate)
        (dolist (part (split-string (downcase candidate) "[^a-z0-9]+" t))
          (puthash part t tokens))))
    (let (values)
      (maphash (lambda (token _flag) (push token values)) tokens)
      values)))

(defun arxana-media--bounce-entry-instruments (entry instruments aliases &optional input-path)
  (let ((tokens (arxana-media--bounce-entry-tokens entry input-path))
        (matches nil))
    (dolist (instrument instruments)
      (when (member instrument tokens)
        (push instrument matches)))
    (dolist (alias aliases)
      (let ((alias-name (car alias))
            (canonical (cdr alias)))
        (when (and (member alias-name tokens)
                   (member canonical instruments))
          (push canonical matches))))
    (cl-delete-duplicates matches :test #'string=)))

(defun arxana-media-bounce-marked ()
  "Bounce marked tracks using a selected profile."
  (interactive)
  (let ((entries (arxana-media--marked-track-entries-in-context)))
    (unless entries
      (user-error "No marked tracks for bounce"))
    (unless (= (length entries) 4)
      (user-error "Bounce expects 4 tracks (got %d)" (length entries)))
    (let* ((profile (arxana-media--bounce-profile-choice))
           (script-rel (plist-get (cdr profile) :script))
           (script (arxana-media--script-path script-rel))
           (instrument-order (plist-get (cdr profile) :instruments))
           (instrument-aliases (or (plist-get (cdr profile) :instrument-aliases)
                                   arxana-media-bounce-instrument-aliases)))
      (unless (and script (file-exists-p script) (file-executable-p script))
        (user-error "Bounce script not executable: %s" script))
      (let ((entry-data nil))
        (dolist (entry entries)
          (let ((path (arxana-media--bounce-input-path entry)))
            (unless path
              (user-error "No readable input for %s"
                          (or (plist-get entry :title)
                              (plist-get entry :recorded)
                              (plist-get entry :name)
                              "track")))
            (push (list :entry entry :path path) entry-data)))
        (setq entry-data (nreverse entry-data))
      (when instrument-order
        (let ((assignments (make-hash-table :test 'equal))
              (unmatched nil)
              (ambiguous nil)
              (duplicates nil))
          (dolist (data entry-data)
            (let* ((entry (plist-get data :entry))
                   (path (plist-get data :path))
                   (matches (arxana-media--bounce-entry-instruments
                             entry instrument-order instrument-aliases path))
                   (label (or (and path (file-name-nondirectory path))
                              (plist-get entry :title)
                              (plist-get entry :base_name)
                              (plist-get entry :source)
                              (plist-get entry :name)
                              "track")))
              (cond
               ((null matches)
                (push label unmatched))
               ((> (length matches) 1)
                (push (cons label matches) ambiguous))
               (t
                (let ((instrument (car matches)))
                  (if (gethash instrument assignments)
                      (push instrument duplicates)
                    (puthash instrument data assignments)))))))
          (when duplicates
            (user-error "Multiple tracks matched: %s"
                        (mapconcat #'identity (cl-delete-duplicates duplicates :test #'string=) ", ")))
          (when ambiguous
            (user-error "Ambiguous instrument tags: %s"
                        (mapconcat (lambda (pair)
                                     (format "%s -> %s"
                                             (car pair)
                                             (mapconcat #'identity (cdr pair) "/")))
                                   (nreverse ambiguous) "; ")))
          (when unmatched
            (user-error "No instrument tag found in: %s"
                        (mapconcat #'identity (nreverse unmatched) ", ")))
          (let (missing)
            (dolist (instrument instrument-order)
              (unless (gethash instrument assignments)
                (push instrument missing)))
            (when missing
              (user-error "Missing instrument tracks: %s"
                          (mapconcat #'identity (nreverse missing) ", "))))
          (setq entry-data
                (mapcar (lambda (instrument)
                          (gethash instrument assignments))
                        instrument-order))))
        (let ((inputs nil))
          (dolist (data entry-data)
            (push (plist-get data :path) inputs))
          (setq inputs (nreverse inputs))
          (let* ((out-root (arxana-media--bounce-output-root))
                 (_ (unless (file-directory-p out-root)
                      (user-error "Bounce output directory missing: %s" out-root)))
                 (default-path (expand-file-name "mix.wav" out-root))
                 (raw (read-file-name "Output wav: " out-root default-path nil
                                      (file-name-nondirectory default-path)))
                 (raw (replace-regexp-in-string "\\`[[:space:]\n\r\t]+\\|[[:space:]\n\r\t]+\\'" "" raw))
                 (out (expand-file-name raw))
                 (out (if (string-match-p "\\.wav\\'" (downcase out))
                          out
                        (concat out ".wav")))
                 (mp3 (when (y-or-n-p "Also create mp3? ")
                        (concat (file-name-sans-extension out) ".mp3")))
                 (buffer (get-buffer-create "*arxana-media-bounce*"))
                 (args (append inputs (list out) (when mp3 (list mp3)))))
            (unless (file-directory-p (file-name-directory out))
              (user-error "Output directory missing: %s" (file-name-directory out)))
            (with-current-buffer buffer
              (let ((inhibit-read-only t))
                (erase-buffer)))
            (let ((exit-code (apply #'process-file script nil buffer t args)))
              (unless (equal exit-code 0)
                (user-error "Bounce failed (see %s)" (buffer-name buffer))))
            (message "Bounce complete: %s" out)))))))

(defun arxana-media-bounce-or-up ()
  "Bounce marked tracks in media views, otherwise go up."
  (interactive)
  (let* ((context arxana-browser--context)
         (media-context (or (plist-get context :media-filter)
                            (memq (plist-get context :view)
                                  '(media-projects media-publications media-publication))
                            (eq (plist-get context :type) 'media-category))))
    (if (and media-context (arxana-media--marked-track-entries-in-context))
        (arxana-media-bounce-marked)
      (arxana-browser--up))))

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
  (let* ((entries (arxana-media--marked-track-entries-in-context)))
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
      (arxana-browser--render))))

(defun arxana-media--publication-at-point ()
  (let ((item (arxana-browser--item-at-point)))
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
    (arxana-browser--render)))

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

(defun arxana-media-move-misc-to-ep-at-point ()
  "Stage the current track (or marked tracks) into an EP-in-progress directory."
  (interactive)
  (let* ((marked (arxana-media--marked-items-in-context))
         (items (if (and marked (> (length marked) 0))
                    marked
                  (let ((item (arxana-browser--item-at-point)))
                    (unless (and item (memq (plist-get item :type)
                                            '(media-track media-misc-track media-publication-track)))
                      (user-error "No media track at point"))
                    (list item)))))
    (when (and (> (length items) 1)
               (not (yes-or-no-p (format "Stage %d tracks into an EP? " (length items)))))
      (user-error "Canceled"))
    (let ((dest-dir (arxana-media--prompt-ep-directory))
          (moved 0)
          (copied 0))
      (dolist (item items)
        (pcase (plist-get item :type)
          ('media-track
           (let* ((entry (plist-get item :entry))
                  (title (or (plist-get entry :title)
                             (plist-get entry :base_name)
                             (plist-get entry :sha256)
                             "track"))
                  (path (arxana-media--track-export-path entry)))
             (unless path
               (user-error "No readable media file found for %s" title))
             (let* ((ext (or (file-name-extension path t) ""))
                    (slug (arxana-media--slug title))
                    (dest (expand-file-name (concat slug ext) dest-dir)))
               (when (file-exists-p dest)
                 (user-error "Destination already exists: %s" dest))
               (copy-file path dest t)
               (setq copied (1+ copied)))))
          ((or 'media-misc-track 'media-publication-track)
           (let* ((path (plist-get item :path))
                  (label (or (plist-get item :label)
                             (and path (file-name-base path))
                             "track")))
             (unless (and path (file-readable-p path))
               (user-error "No readable media file found for %s" label))
             (let ((dest (expand-file-name (file-name-nondirectory path) dest-dir)))
               (when (file-exists-p dest)
                 (user-error "Destination already exists: %s" dest))
               (rename-file path dest)
               (setq moved (1+ moved))
               (remhash path arxana-media--misc-sha-cache))))
          (_ (user-error "Unsupported media item for staging"))))
      (dolist (item items)
        (let ((key (arxana-media--mark-key-for-item item)))
          (when key (remhash key arxana-media--marked))))
      (arxana-browser--render)
      (message "Staged %d track%s to %s (%d moved, %d copied)"
               (+ moved copied)
               (if (= (+ moved copied) 1) "" "s")
               dest-dir
               moved
               copied))))
(defun arxana-media-retitle-at-point ()
  "Retitle the current media track or misc audio file."
  (interactive)
  (let* ((item (arxana-browser--item-at-point)))
    (unless (and item (memq (plist-get item :type)
                            '(media-track media-misc-track media-publication-track)))
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
           (arxana-browser--render)
           (message "Retitled %s" title))))
      ((or 'media-misc-track 'media-publication-track)
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
             (arxana-browser--render)
             (message "Retitled %s" title))))))))

(defun arxana-media-delete-at-point ()
  "Hard-delete the current misc audio file from disk."
  (interactive)
  (let* ((item (arxana-browser--item-at-point)))
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
        (arxana-browser--render)
        (message "Deleted %s" label)))))

(provide 'arxana-media)

;;; arxana-media.el ends here
