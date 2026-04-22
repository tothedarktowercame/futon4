;;; arxana-browser-vsatarcs.el --- VSATARCS reader for VSAT-shaped stories -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight Arxana-native reader for VSAT-shaped anthology Markdown.
;; The initial source is file-backed `## Scene: Title | anchor' content,
;; matching futon5a's story drafts.  The parsed shape deliberately mirrors
;; future `vsat-story' / `vsat-scene' entities: story metadata, scene anchors,
;; scene body text, and intra-story links.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'arxana-ui nil t)

(defgroup arxana-vsatarcs nil
  "VSATARCS reading support for VSAT-shaped stories."
  :group 'arxana)

(defcustom arxana-vsatarcs-story-directories
  (list (expand-file-name "~/code/futon5a/holes/stories"))
  "Directories scanned for VSAT-shaped Markdown stories."
  :type '(repeat directory)
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-landing-story "leaf-start-here.md"
  "Filename of the landing-page story used by `arxana-vsatarcs-up'."
  :type 'string
  :group 'arxana-vsatarcs)

(defvar arxana-vsatarcs--buffer "*VSATARCS*")

(defvar-local arxana-vsatarcs--story nil)
(defvar-local arxana-vsatarcs--source-file nil)
(defvar-local arxana-vsatarcs--current-anchor nil)

(defun arxana-vsatarcs--slugify (text)
  "Return a conservative link anchor for TEXT."
  (let* ((value (downcase (string-trim (or text ""))))
         (value (replace-regexp-in-string "[^[:alnum:] -]" "" value))
         (value (replace-regexp-in-string "[[:space:]]+" "-" value))
         (value (replace-regexp-in-string "-+" "-" value)))
    (string-trim value "-+" "-+")))

(defun arxana-vsatarcs--scene-heading-p (line)
  "Return parsed scene heading fields from LINE, or nil."
  (when (string-match
         "^##+[[:space:]]+Scene:[[:space:]]+\\(.+?\\)\\(?:[[:space:]]+|[[:space:]]*\\([[:alnum:] -]+\\)\\)?[[:space:]]*$"
         line)
    (let* ((title (string-trim (match-string 1 line)))
           (anchor (string-trim (or (match-string 2 line) ""))))
      (list :title title
            :anchor (if (string-empty-p anchor)
                        (arxana-vsatarcs--slugify title)
                      (arxana-vsatarcs--slugify anchor))))))

(defun arxana-vsatarcs--finalize-scene (scene body-lines index)
  "Return SCENE with BODY-LINES and INDEX, or nil when SCENE is nil."
  (when scene
    (let* ((raw-body (string-trim (string-join (nreverse body-lines) "\n")))
           (opening (string-match-p "(opening scene)" raw-body))
           (body (replace-regexp-in-string
                  "^[[:space:]]*\\*(opening scene)\\*[[:space:]]*\n?" ""
                  raw-body))
           (body (arxana-vsatarcs--normalize-folded-links body)))
      (append scene
              (list :index index
                    :opening opening
                    :body (string-trim body)
                    :links (arxana-vsatarcs--extract-links body))))))

(defun arxana-vsatarcs--normalize-folded-links (text)
  "Normalize common hard-wrapped Markdown link targets in TEXT."
  (replace-regexp-in-string "(\\([^)\n]+-\\)\n\\([^)\n]+\\))"
                            "(\\1\\2)"
                            (or text "")))

(defun arxana-vsatarcs--extract-links (text)
  "Extract Markdown links from TEXT as plists with `:text' and `:target'."
  (let ((normalized (arxana-vsatarcs--normalize-folded-links text))
        links)
    (with-temp-buffer
      (insert normalized)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([^]\n]+\\)\\](\\([^) \t\n]+\\))" nil t)
        (push (list :text (match-string 1)
                    :target (arxana-vsatarcs--slugify (match-string 2)))
              links)))
    (nreverse links)))

(defun arxana-vsatarcs-parse-string (text &optional source-file)
  "Parse VSAT-shaped anthology Markdown TEXT.

Return a plist with `:title', `:source-file', `:metadata', and `:scenes'."
  (let ((lines (split-string (or text "") "\n"))
        title
        metadata-lines
        scenes
        current
        body-lines
        (scene-index 0)
        (before-scenes t))
    (dolist (line lines)
      (let ((heading (arxana-vsatarcs--scene-heading-p line)))
        (cond
         (heading
          (when current
            (push (arxana-vsatarcs--finalize-scene current body-lines scene-index)
                  scenes)
            (setq scene-index (1+ scene-index)))
          (setq before-scenes nil
                current heading
                body-lines nil))
         (current
          (push line body-lines))
         (before-scenes
          (cond
           ((and (not title)
                 (string-match "^#[[:space:]]+\\(.+\\)[[:space:]]*$" line))
            (setq title (string-trim (match-string 1 line))))
           ((not (string-match-p "^---[[:space:]]*$" line))
            (push line metadata-lines)))))))
    (when current
      (push (arxana-vsatarcs--finalize-scene current body-lines scene-index)
            scenes))
    (list :title (or title (and source-file (file-name-base source-file)) "Untitled VSATARCS Story")
          :source-file source-file
          :metadata (string-trim (string-join (nreverse metadata-lines) "\n"))
          :scenes (nreverse (delq nil scenes)))))

(defun arxana-vsatarcs-parse-file (path)
  "Parse VSAT-shaped anthology Markdown at PATH."
  (let ((expanded (expand-file-name path)))
    (with-temp-buffer
      (insert-file-contents expanded)
      (arxana-vsatarcs-parse-string
       (buffer-substring-no-properties (point-min) (point-max))
       expanded))))

(defun arxana-vsatarcs--scenes (story)
  (or (plist-get story :scenes) '()))

(defun arxana-vsatarcs--scene-by-anchor (story anchor)
  (let ((target (arxana-vsatarcs--slugify anchor)))
    (cl-find-if (lambda (scene)
                  (string= (plist-get scene :anchor) target))
                (arxana-vsatarcs--scenes story))))

(defun arxana-vsatarcs--opening-scene (story)
  (or (cl-find-if (lambda (scene) (plist-get scene :opening))
                  (arxana-vsatarcs--scenes story))
      (car (arxana-vsatarcs--scenes story))))

(defun arxana-vsatarcs--current-scene ()
  (or (and arxana-vsatarcs--story
           arxana-vsatarcs--current-anchor
           (arxana-vsatarcs--scene-by-anchor arxana-vsatarcs--story
                                             arxana-vsatarcs--current-anchor))
      (and arxana-vsatarcs--story
           (arxana-vsatarcs--opening-scene arxana-vsatarcs--story))))

(defun arxana-vsatarcs--insert-button (label action &optional help)
  (insert-text-button label
                      'follow-link t
                      'help-echo help
                      'action action))

(defun arxana-vsatarcs--insert-scene-link (story scene &optional label)
  (let ((anchor (plist-get scene :anchor)))
    (arxana-vsatarcs--insert-button
     (or label (plist-get scene :title))
     (lambda (_button)
       (arxana-vsatarcs-render story anchor))
     (format "Go to %s" anchor))))

(defun arxana-vsatarcs--insert-markdown-line (line)
  "Insert LINE, turning simple Markdown links into VSATARCS buttons."
  (let ((start 0))
    (while (string-match "\\[\\([^]\n]+\\)\\](\\([^) \t\n]+\\))" line start)
      (insert (substring line start (match-beginning 0)))
      (let ((label (match-string 1 line))
            (target (arxana-vsatarcs--slugify (match-string 2 line))))
        (arxana-vsatarcs--insert-button
         label
         (lambda (_button)
           (arxana-vsatarcs-goto target))
         (format "Go to %s" target)))
      (setq start (match-end 0)))
    (insert (substring line start))
    (insert "\n")))

(defun arxana-vsatarcs--insert-markdown (text)
  "Insert story TEXT with intra-story links buttonized."
  (dolist (line (split-string (or text "") "\n"))
    (arxana-vsatarcs--insert-markdown-line line)))

(defun arxana-vsatarcs--scene-at-index (story index)
  (nth index (arxana-vsatarcs--scenes story)))

(defun arxana-vsatarcs--remember-return-target (return-buffer return-config)
  "Store RETURN-BUFFER and RETURN-CONFIG for left-at-start navigation."
  (when (and (boundp 'arxana-ui-return-buffer)
             (buffer-live-p return-buffer))
    (setq-local arxana-ui-return-buffer return-buffer))
  (when (and (boundp 'arxana-ui-return-window-config)
             (window-configuration-p return-config))
    (setq-local arxana-ui-return-window-config return-config)))

(defun arxana-vsatarcs-left-or-return ()
  "Move left; at point-min, return via the shared Arxana UI target."
  (interactive)
  (if (> (point) (point-min))
      (backward-char)
    (if (fboundp 'arxana-ui-left-or-return)
        (arxana-ui-left-or-return)
      (message "No return target available"))))

(defun arxana-vsatarcs-render (story &optional anchor return-buffer return-config)
  "Render STORY at ANCHOR in the VSATARCS reader buffer.
RETURN-BUFFER and RETURN-CONFIG, when non-nil, are used by
`arxana-vsatarcs-left-or-return' at point-min."
  (let* ((target-buffer (get-buffer-create arxana-vsatarcs--buffer))
         (existing-return-buffer
          (with-current-buffer target-buffer
            (and (boundp 'arxana-ui-return-buffer)
                 arxana-ui-return-buffer)))
         (existing-return-config
          (with-current-buffer target-buffer
            (and (boundp 'arxana-ui-return-window-config)
                 arxana-ui-return-window-config)))
         (scene (or (and anchor (arxana-vsatarcs--scene-by-anchor story anchor))
                    (arxana-vsatarcs--opening-scene story)))
         (scenes (arxana-vsatarcs--scenes story))
         (index (or (plist-get scene :index) 0))
         (previous (arxana-vsatarcs--scene-at-index story (1- index)))
         (next (arxana-vsatarcs--scene-at-index story (1+ index))))
    (unless scene
      (user-error "Story has no scenes"))
    (with-current-buffer target-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (arxana-vsatarcs-mode)
        (setq arxana-vsatarcs--story story
              arxana-vsatarcs--source-file (plist-get story :source-file)
              arxana-vsatarcs--current-anchor (plist-get scene :anchor))
        (arxana-vsatarcs--remember-return-target
         (or return-buffer existing-return-buffer)
         (or return-config existing-return-config))
        (when (fboundp 'arxana-ui-mark-managed)
          (arxana-ui-mark-managed "VSATARCS"))
        (insert (plist-get story :title) "\n")
        (insert (make-string (length (plist-get story :title)) ?=) "\n\n")
        (insert (format "Scene %d/%d: %s%s\n"
                        (1+ index)
                        (length scenes)
                        (plist-get scene :title)
                        (if (plist-get scene :opening) " (opening)" "")))
        (when arxana-vsatarcs--source-file
          (insert (format "Source: %s\n" arxana-vsatarcs--source-file)))
        (insert "\n")
        (when previous
          (arxana-vsatarcs--insert-scene-link story previous "< previous"))
        (when (and previous next)
          (insert "   "))
        (when next
          (arxana-vsatarcs--insert-scene-link story next "next >"))
        (insert "\n\n")
        (insert "Scenes: ")
        (cl-loop for s in scenes
                 for i from 0
                 do (progn
                      (when (> i 0) (insert " · "))
                      (if (eq s scene)
                          (insert (propertize (plist-get s :title) 'face 'bold))
                        (arxana-vsatarcs--insert-scene-link story s))))
        (insert "\n\n")
        (insert (make-string 72 ?-) "\n\n")
        (arxana-vsatarcs--insert-markdown (plist-get scene :body))
        (goto-char (point-min)))
      (display-buffer (current-buffer)))))

(defun arxana-vsatarcs--find-story-file (name)
  "Return an absolute path for a VSATARCS story matching NAME, or nil.
NAME may or may not end in \".md\"; story directories are searched in
order for a matching readable file."
  (let ((filename (if (string-suffix-p ".md" name) name (concat name ".md"))))
    (cl-some (lambda (dir)
               (let ((path (expand-file-name filename (expand-file-name dir))))
                 (and (file-readable-p path) path)))
             arxana-vsatarcs-story-directories)))

(defun arxana-vsatarcs-goto (anchor)
  "Navigate VSATARCS to ANCHOR.

ANCHOR is tried first as a scene within the current story; if no such
scene exists it is tried as a cross-story reference, opening the file
\"<anchor>.md\" from `arxana-vsatarcs-story-directories'."
  (interactive
   (list (completing-read "Scene: "
                          (mapcar (lambda (scene) (plist-get scene :anchor))
                                  (arxana-vsatarcs--scenes arxana-vsatarcs--story))
                          nil t)))
  (unless arxana-vsatarcs--story
    (user-error "No VSATARCS story is active"))
  (cond
   ((arxana-vsatarcs--scene-by-anchor arxana-vsatarcs--story anchor)
    (arxana-vsatarcs-render arxana-vsatarcs--story anchor))
   ((arxana-vsatarcs--find-story-file anchor)
    (arxana-vsatarcs-open-file
     (arxana-vsatarcs--find-story-file anchor)
     (and (boundp 'arxana-ui-return-buffer) arxana-ui-return-buffer)
     (and (boundp 'arxana-ui-return-window-config)
          arxana-ui-return-window-config)))
   (t
    (user-error "Unknown VSATARCS target: %s" anchor))))

(defun arxana-vsatarcs-up ()
  "Jump to the VSATARCS landing page (`arxana-vsatarcs-landing-story').
The landing story is expected to live in one of the directories in
`arxana-vsatarcs-story-directories'."
  (interactive)
  (let ((path (arxana-vsatarcs--find-story-file arxana-vsatarcs-landing-story)))
    (unless path
      (user-error "No VSATARCS landing story (%s) found in story directories"
                  arxana-vsatarcs-landing-story))
    (arxana-vsatarcs-open-file
     path
     (and (boundp 'arxana-ui-return-buffer) arxana-ui-return-buffer)
     (and (boundp 'arxana-ui-return-window-config)
          arxana-ui-return-window-config))))

(defun arxana-vsatarcs-next-scene ()
  "Move to the next scene in the current VSATARCS story."
  (interactive)
  (let* ((scene (arxana-vsatarcs--current-scene))
         (next (and scene
                    (arxana-vsatarcs--scene-at-index arxana-vsatarcs--story
                                                     (1+ (plist-get scene :index))))))
    (unless next
      (user-error "No next VSATARCS scene"))
    (arxana-vsatarcs-render arxana-vsatarcs--story (plist-get next :anchor))))

(defun arxana-vsatarcs-previous-scene ()
  "Move to the previous scene in the current VSATARCS story."
  (interactive)
  (let* ((scene (arxana-vsatarcs--current-scene))
         (previous (and scene
                        (arxana-vsatarcs--scene-at-index arxana-vsatarcs--story
                                                         (1- (plist-get scene :index))))))
    (unless previous
      (user-error "No previous VSATARCS scene"))
    (arxana-vsatarcs-render arxana-vsatarcs--story (plist-get previous :anchor))))

(defun arxana-vsatarcs-reload ()
  "Reload the current VSATARCS story from disk."
  (interactive)
  (unless arxana-vsatarcs--source-file
    (user-error "Current VSATARCS story has no source file"))
  (let ((anchor arxana-vsatarcs--current-anchor))
    (arxana-vsatarcs-render
     (arxana-vsatarcs-parse-file arxana-vsatarcs--source-file)
     anchor)))

(defun arxana-vsatarcs-open-file (path &optional return-buffer return-config)
  "Open VSAT-shaped anthology Markdown PATH in the VSATARCS reader.
RETURN-BUFFER and RETURN-CONFIG override the current buffer/window
configuration as the left-at-start return target."
  (interactive "fVSATARCS story: ")
  (arxana-vsatarcs-render
   (arxana-vsatarcs-parse-file path)
   nil
   (or return-buffer (current-buffer))
   (or return-config (current-window-configuration))))

(defun arxana-vsatarcs-open-example ()
  "Open the futon5a Stack Geometry anthology example."
  (interactive)
  (let ((path (expand-file-name "~/code/futon5a/holes/stories/leaf-6-4-4.md")))
    (unless (file-readable-p path)
      (user-error "Example story not found: %s" path))
    (arxana-vsatarcs-open-file path)))

(defvar arxana-vsatarcs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'arxana-vsatarcs-next-scene)
    (define-key map (kbd "p") #'arxana-vsatarcs-previous-scene)
    (define-key map (kbd "g") #'arxana-vsatarcs-reload)
    (define-key map (kbd "o") #'arxana-vsatarcs-goto)
    (define-key map (kbd "u") #'arxana-vsatarcs-up)
    (define-key map (kbd "<left>") #'arxana-vsatarcs-left-or-return)
    map)
  "Keymap for `arxana-vsatarcs-mode'.")

(define-derived-mode arxana-vsatarcs-mode special-mode "VSATARCS"
  "Major mode for reading VSAT-shaped stories."
  (setq-local truncate-lines nil))

(defun arxana-browser-vsatarcs--story-paths ()
  "Return readable VSATARCS source paths."
  (let (paths)
    (dolist (directory arxana-vsatarcs-story-directories)
      (let ((expanded (expand-file-name directory)))
        (when (file-directory-p expanded)
          (setq paths
                (append paths
                        (directory-files expanded t "\\.md\\'"))))))
    (delete-dups (sort paths #'string<))))

(defun arxana-browser-vsatarcs-format ()
  "Return tabulated format for VSATARCS story sources."
  [("Story" 36 t)
   ("Scenes" 7 t)
   ("Opening" 26 t)
   ("Path" 0 nil)])

(defun arxana-browser-vsatarcs-row (item)
  "Return a tabulated row for VSATARCS ITEM."
  (vector (or (plist-get item :label) "")
          (format "%s" (or (plist-get item :scene-count) 0))
          (or (plist-get item :opening) "-")
          (or (plist-get item :path) "")))

(defun arxana-browser-vsatarcs-items ()
  "Return browser rows for VSATARCS story sources."
  (let ((paths (arxana-browser-vsatarcs--story-paths)))
    (if paths
        (mapcar
         (lambda (path)
           (condition-case err
               (let* ((story (arxana-vsatarcs-parse-file path))
                      (opening (arxana-vsatarcs--opening-scene story)))
                 (list :type 'vsatarcs-story
                       :label (plist-get story :title)
                       :path path
                       :scene-count (length (arxana-vsatarcs--scenes story))
                       :opening (and opening (plist-get opening :title))))
             (error
              (list :type 'vsatarcs-story
                    :label (file-name-base path)
                    :path path
                    :scene-count 0
                    :opening (format "parse error: %s" (error-message-string err))))))
         paths)
      (list (list :type 'info
                  :label "No VSATARCS stories"
                  :description "Add Markdown stories to arxana-vsatarcs-story-directories.")))))

(defun arxana-browser-vsatarcs-open (item)
  "Open VSATARCS browser ITEM."
  (let ((path (plist-get item :path))
        (return-buffer (current-buffer))
        (return-config (current-window-configuration)))
    (unless (and path (file-readable-p path))
      (user-error "VSATARCS source is missing: %s" path))
    (arxana-vsatarcs-open-file path return-buffer return-config)))

(provide 'arxana-browser-vsatarcs)
;;; arxana-browser-vsatarcs.el ends here
