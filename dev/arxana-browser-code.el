;;; arxana-browser-code.el --- Code browser helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Browse code files and show related docbook entries side-by-side.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Link persistence support
(declare-function arxana-links-make-strategy "arxana-links")
(declare-function arxana-links-persist-strategy "arxana-links")
(declare-function arxana-links-find-strategy "arxana-links")
(declare-function arxana-store-sync-enabled-p "arxana-store")

(declare-function arxana-docbook-entries "arxana-docbook-core" (book))
(declare-function arxana-docbook--entry-source-path "arxana-docbook-core" (entry))
(declare-function arxana-docbook--entry-raw-text "arxana-docbook-core" (entry))
(declare-function arxana-docbook--entry-content "arxana-docbook-core" (entry))

(defgroup arxana-browser-code nil
  "Code browser helpers for Arxana."
  :group 'arxana)

(defcustom arxana-browser-code-docbook "futon4"
  "Docbook name to search for code-related entries."
  :type 'string
  :group 'arxana-browser-code)

(defcustom arxana-browser-code-file-regexp
  "\\.\\(el\\|clj\\|cljc\\|cljs\\|edn\\)\\'"
  "Regexp used to collect code files."
  :type 'regexp
  :group 'arxana-browser-code)

(defcustom arxana-browser-code-roots nil
  "List of directories to scan for code files.
When nil, defaults to <repo>/dev and <repo>/test."
  :type '(repeat directory)
  :group 'arxana-browser-code)

(defcustom arxana-browser-code-docs-buffer "*Arxana Code Docs*"
  "Buffer name used for the docbook split view."
  :type 'string
  :group 'arxana-browser-code)

(defcustom arxana-browser-code-docs-side 'right
  "Side for the docs window (passed to `display-buffer-in-side-window`)."
  :type '(choice (const :tag "Right" right)
                 (const :tag "Left" left)
                 (const :tag "Bottom" bottom))
  :group 'arxana-browser-code)

(defcustom arxana-browser-code-docs-width 0.45
  "Width (fraction) for the docs side window."
  :type 'number
  :group 'arxana-browser-code)

(defcustom arxana-browser-code-frame-name "Arxana Code"
  "Frame name used for the code + docs split."
  :type 'string
  :group 'arxana-browser-code)

(defcustom arxana-browser-code-browser-frame-name "Arxana Browser"
  "Frame name used to hold the Arxana browser."
  :type 'string
  :group 'arxana-browser-code)

(defvar arxana-browser-code--symbol-cache (make-hash-table :test 'equal))
(defconst arxana-browser-code--symbol-cache-version 2
  "Bump to invalidate cached symbol lists.")

(defvar arxana-browser-code--persistence-manifest
  '("Persist code-symbol -> doc paragraph anchors."
    "Persist file -> doc entry associations (docbook metadata)."
    "Persist backlink index for doc paragraphs -> code definitions.")
  "Notes for future persisted code-docs linkage.")

(defvar arxana-browser-code--active-strategy nil
  "The currently active link strategy for code-docs browsing.
This is loaded from Futon1 on first use, or created and persisted if none exists.")

(defconst arxana-browser-code--def-patterns
  '("defun" "defmacro" "defsubst" "defvar" "defvar-local"
    "defcustom" "defconst" "define-derived-mode" "defn" "defn-")
  "Patterns that define linkable symbols in code files.")

(defun arxana-browser-code-ensure-strategy ()
  "Ensure the code-docs link strategy exists and is persisted.
Returns the active strategy, or nil if persistence is unavailable."
  (or arxana-browser-code--active-strategy
      (when (fboundp 'arxana-store-sync-enabled-p)
        ;; Try to load existing strategy
        (let ((existing (and (fboundp 'arxana-links-find-strategy)
                             (arxana-links-find-strategy "futon4"))))
          (if existing
              (setq arxana-browser-code--active-strategy existing)
            ;; Create and persist new strategy
            (when (and (fboundp 'arxana-links-make-strategy)
                       (fboundp 'arxana-links-persist-strategy)
                       (arxana-store-sync-enabled-p))
              (let* ((roots (arxana-browser-code--resolve-roots))
                     (scope (list :repo "futon4"
                                  :code-roots (mapcar
                                               (lambda (r)
                                                 (file-relative-name r (arxana-browser-code--repo-root)))
                                               roots)
                                  :docbook arxana-browser-code-docbook))
                     (finders (list (list :type :symbol-as-term
                                          :def-patterns arxana-browser-code--def-patterns
                                          :auto-link? t)
                                    (list :type :filename-mention
                                          :auto-link? t)))
                     (strategy (arxana-links-make-strategy :scope scope :finders finders)))
                (when (arxana-links-persist-strategy strategy)
                  (message "[arxana-browser-code] Created link strategy: %s"
                           (plist-get strategy :xt/id))
                  (setq arxana-browser-code--active-strategy strategy))))))
        arxana-browser-code--active-strategy)))

(defvar arxana-browser-code-docs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'arxana-browser-code-docs-activate)
    (define-key map (kbd "C-m") #'arxana-browser-code-docs-activate)
    (define-key map (kbd "C-c C-n") #'arxana-browser-code-docs-cycle-paragraph)
    map)
  "Keymap for `arxana-browser-code-docs-mode'.")

(defvar arxana-browser-code-docs-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'arxana-browser-code-docs-activate)
    (define-key map (kbd "RET") #'arxana-browser-code-docs-activate)
    map)
  "Keymap for doc links in `arxana-browser-code-docs-mode'.")

(defface arxana-browser-code-docs-file-link-face
  '((t :inherit link :slant italic))
  "Face for file links in code docs buffers."
  :group 'arxana-browser-code)

(defface arxana-browser-code-docs-highlight-face
  '((t :inherit highlight))
  "Face for the current doc paragraph highlight."
  :group 'arxana-browser-code)

(defcustom arxana-browser-code-sync-docs t
  "When non-nil, keep doc highlights in sync with code point."
  :type 'boolean
  :group 'arxana-browser-code)

(defvar-local arxana-browser-code--doc-symbol-map nil)
(defvar-local arxana-browser-code--doc-source-path nil)
(defvar-local arxana-browser-code--doc-highlight-overlay nil)
(defvar-local arxana-browser-code--doc-last-symbol nil)
(defvar-local arxana-browser-code--doc-cycle-cache nil)

(defvar arxana-browser-code-sync-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") #'arxana-browser-code-cycle-docs-paragraph)
    map)
  "Keymap for `arxana-browser-code-sync-mode'.")

(define-minor-mode arxana-browser-code-docs-mode
  "Minor mode for Arxana code docs buffers."
  :lighter " CodeDocs"
  :keymap arxana-browser-code-docs-mode-map)

(defun arxana-browser-code--repo-root ()
  (let* ((base (or load-file-name buffer-file-name default-directory))
         (root (and base (locate-dominating-file base "dev"))))
    (or root default-directory)))

(defun arxana-browser-code--resolve-roots ()
  (let* ((root (arxana-browser-code--repo-root))
         (defaults (list (expand-file-name "dev" root)
                         (expand-file-name "test" root)))
         (candidates (or arxana-browser-code-roots defaults)))
    (delq nil
          (mapcar (lambda (path)
                    (let ((expanded (expand-file-name path root)))
                      (when (file-directory-p expanded)
                        expanded)))
                  candidates))))

(defun arxana-browser-code--relative-path (path)
  (let ((root (arxana-browser-code--repo-root)))
    (if (and root (string-prefix-p (file-name-as-directory root) path))
        (string-remove-prefix (file-name-as-directory root) path)
      path)))

(defun arxana-browser-code--file-items ()
  (let ((roots (arxana-browser-code--resolve-roots))
        (items '()))
    (dolist (root roots)
      (dolist (path (directory-files-recursively root arxana-browser-code-file-regexp))
        (push (list :type 'code-file
                    :label (file-name-nondirectory path)
                    :description (arxana-browser-code--relative-path path)
                    :path path)
              items)))
    (setq items (sort items (lambda (a b)
                              (string< (plist-get a :description)
                                       (plist-get b :description)))))
    items))

(defun arxana-browser-code-items ()
  "Return items for the code browser view."
  (let ((items (arxana-browser-code--file-items)))
    (if items
        items
      (list (list :type 'info
                  :label "No code files found"
                  :description "Set arxana-browser-code-roots to enable scanning.")))))

(defun arxana-browser-code-format ()
  [("Name" 28 t)
   ("Path" 80 t)])

(defun arxana-browser-code-row (item)
  (let ((label (or (plist-get item :label) ""))
        (desc (or (plist-get item :description) "")))
    (vector label desc)))

(defun arxana-browser-code--entry-title (entry)
  (or (plist-get entry :title)
      (plist-get entry :doc/title)
      (plist-get entry :doc-id)
      "Doc entry"))

(defun arxana-browser-code--normalize-path (path)
  (when path
    (let ((expanded (expand-file-name path (arxana-browser-code--repo-root))))
      (if (file-exists-p expanded)
          (file-truename expanded)
        expanded))))

(defun arxana-browser-code--file-symbols (path)
  (let* ((attrs (and path (file-attributes path)))
         (mtime (and attrs (file-attribute-modification-time attrs)))
         (cached (and path (gethash path arxana-browser-code--symbol-cache))))
    (if (and cached
             (equal (car cached) mtime)
             (equal (cadr cached) arxana-browser-code--symbol-cache-version))
        (caddr cached)
      (let ((symbols '())
            (seen (make-hash-table :test 'equal)))
        (when (and path (file-readable-p path))
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-min))
            (while (re-search-forward
                    "^[[:space:]]*(\\(defun\\|defmacro\\|defsubst\\|defvar\\|defvar-local\\|defcustom\\|defconst\\|define-derived-mode\\|defn\\|defn-\\)\\s-+\\([^[:space:]\n]+\\)"
                    nil t)
              (let ((sym (match-string 2)))
                (when (and sym (not (string-empty-p sym))
                           (not (gethash sym seen)))
                  (puthash sym t seen)
                  (push sym symbols))))))
        (setq symbols (nreverse symbols))
        (when path
          (puthash path (list mtime arxana-browser-code--symbol-cache-version symbols)
                   arxana-browser-code--symbol-cache))
        symbols))))

(defun arxana-browser-code--doc-text (entry)
  (let ((text (and entry (arxana-docbook--entry-raw-text entry))))
    (or text "")))

(defun arxana-browser-code--text-contains-symbol-p (text symbol)
  (when (and text symbol)
    (string-match-p (concat "\\_<" (regexp-quote symbol) "\\_>") text)))

(defun arxana-browser-code--entry-matches-path-p (entry target)
  (let* ((source (and entry (arxana-docbook--entry-source-path entry)))
         (source-path (arxana-browser-code--normalize-path source)))
    (or (and source-path (string= source-path target))
        (and source (string-suffix-p source target))
        (and source (string= (file-name-nondirectory source)
                             (file-name-nondirectory target))))))

(defun arxana-browser-code--entry-matches-symbols-p (entry symbols filename)
  (let ((text (arxana-browser-code--doc-text entry))
        (matched nil))
    (when (and text filename
               (string-match-p (regexp-quote filename) text))
      (setq matched t))
    (when (and (not matched) (listp symbols))
      (dolist (sym symbols)
        (when (and sym (arxana-browser-code--text-contains-symbol-p text sym))
          (setq matched t))))
    matched))

(defun arxana-browser-code--docbook-matches (path)
  (let* ((target (arxana-browser-code--normalize-path path))
         (symbols (and target (arxana-browser-code--file-symbols target)))
         (filename (and target (file-name-nondirectory target)))
         (matches '()))
    (when (and target (fboundp 'arxana-docbook-entries))
      (dolist (entry (arxana-docbook-entries arxana-browser-code-docbook))
        (when (or (arxana-browser-code--entry-matches-path-p entry target)
                  (arxana-browser-code--entry-matches-symbols-p entry symbols filename))
          (push entry matches))))
    (nreverse matches)))

(defun arxana-browser-code--defun-regexp (symbol)
  (concat "^[[:space:]]*(\\(defun\\|defmacro\\|defsubst\\|defvar\\|defvar-local\\|defcustom\\|defconst\\|define-derived-mode\\|defn\\|defn-\\)\\s-+"
          (regexp-quote symbol)
          "\\_>"))

(defun arxana-browser-code--symbol-in-file-p (symbol path)
  (when (and symbol path (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (re-search-forward (arxana-browser-code--defun-regexp symbol) nil t))))

(defun arxana-browser-code--find-symbol-path (symbol)
  (let* ((rg (executable-find "rg"))
         (root (arxana-browser-code--repo-root)))
    (when (and rg root symbol)
      (let* ((pattern (arxana-browser-code--defun-regexp symbol))
             (default-directory root)
             (args (list rg "-n" "-g" "*.el" "-g" "*.clj" "-g" "*.cljc" "-g" "*.cljs" pattern "dev" "test"))
             (lines (ignore-errors (apply #'process-lines args))))
        (when (and lines (car lines))
          (let ((path (car (split-string (car lines) ":"))))
            (when (and path (file-exists-p path))
              (expand-file-name path root))))))))

(defun arxana-browser-code--jump-to-symbol (buffer symbol)
  (when (and buffer symbol)
    (with-current-buffer buffer
      (goto-char (point-min))
      (when (re-search-forward (arxana-browser-code--defun-regexp symbol) nil t)
        (goto-char (match-beginning 0))
        (let ((win (get-buffer-window buffer t)))
          (when win
            (with-selected-window win
              (recenter))))))))

(defun arxana-browser-code--open-path (path)
  (unless (and path (file-readable-p path))
    (user-error "No readable file for %s" path))
  ;; Ensure link strategy is loaded/created on first use
  (arxana-browser-code-ensure-strategy)
  (arxana-browser-code--show-browser-frame)
  (let* ((browser-buf (get-buffer "*Arxana Browser*"))
         (browser-frame (and browser-buf (arxana-browser-code--frame-showing-buffer browser-buf)))
         (code-frame (or (arxana-browser-code--frame-by-name arxana-browser-code-frame-name)
                         (and (not (eq (selected-frame) browser-frame))
                              (selected-frame))
                         (arxana-browser-code--ensure-frame arxana-browser-code-frame-name)))
         (docs-buf (arxana-browser-code--render-docs path
                                                     (arxana-browser-code--docbook-matches path)))
         (code-buf (find-file-noselect path)))
    (with-selected-frame code-frame
      (let* ((windows (window-list code-frame 'no-mini))
             (side (seq-find (lambda (w)
                               (window-parameter w 'window-side))
                             windows))
             (docs-win (get-buffer-window docs-buf code-frame))
             (main (or (seq-find (lambda (w)
                                   (and (not (window-parameter w 'window-side))
                                        (not (eq w docs-win))))
                                 windows)
                       (and (not (eq (selected-window) docs-win))
                            (selected-window))
                       (car windows))))
        (select-window main)
        (if docs-win
            (dolist (win windows)
              (when (and (window-live-p win)
                         (not (eq win docs-win))
                         (not (eq win main)))
                (delete-window win)))
          (when (not side)
            (delete-other-windows main)))
        (set-window-buffer (selected-window) code-buf)
        (cond
         (docs-win
          (set-window-buffer docs-win docs-buf))
         (side
          (set-window-buffer side docs-buf))
         (t
         (display-buffer-in-side-window
           docs-buf
           (list (cons 'side arxana-browser-code-docs-side)
                 (cons 'window-width arxana-browser-code-docs-width)
                 (cons 'frame code-frame))))))
      (with-current-buffer code-buf
        (when arxana-browser-code-sync-docs
          (arxana-browser-code-sync-mode 1)))
    code-buf)))

(defun arxana-browser-code--open-symbol (symbol path)
  (let ((code-buf (arxana-browser-code--open-path path)))
    (arxana-browser-code--jump-to-symbol code-buf symbol)))

(defun arxana-browser-code--link-props-at-point ()
  (or (and (get-text-property (point) 'arxana-symbol)
           (list :symbol (get-text-property (point) 'arxana-symbol)
                 :path (get-text-property (point) 'arxana-path)))
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'arxana-symbol)
           (list :symbol (get-text-property (1- (point)) 'arxana-symbol)
                 :path (get-text-property (1- (point)) 'arxana-path)))))

(defun arxana-browser-code-docs-activate ()
  "Activate the link at point, if any."
  (interactive)
  (let* ((props (arxana-browser-code--link-props-at-point))
         (symbol (plist-get props :symbol))
         (path (plist-get props :path)))
    (cond
     ((and symbol path)
      (arxana-browser-code--open-symbol symbol path))
     (path
      (arxana-browser-code--open-path path))
     ((fboundp 'org-open-at-point)
      (org-open-at-point)))))

(defun arxana-browser-code-docs-cycle-paragraph (activate)
  "Cycle to the next code symbol link in the current paragraph.
With prefix ACTIVATE, open the symbol after cycling."
  (interactive "P")
  (let* ((bounds (bounds-of-thing-at-point 'paragraph))
         (start (car bounds))
         (end (cdr bounds)))
    (unless bounds
      (user-error "No paragraph at point"))
    (let ((positions '()))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let ((pos (text-property-any (point) end 'arxana-symbol nil)))
            (if (not pos)
                (goto-char end)
              (push pos positions)
              (goto-char (1+ pos))))))
      (setq positions (nreverse (delete-dups positions)))
      (unless positions
        (user-error "No code links in this paragraph"))
      (let* ((cache arxana-browser-code--doc-cycle-cache)
             (reuse (and cache
                         (equal (plist-get cache :bounds) bounds)
                         (equal (plist-get cache :positions) positions)))
             (index (if reuse (plist-get cache :index) -1))
             (next (mod (1+ index) (length positions)))
             (pos (nth next positions)))
        (setq arxana-browser-code--doc-cycle-cache
              (list :bounds bounds :positions positions :index next))
        (goto-char pos)
        (when activate
          (arxana-browser-code-docs-activate))))))

(defun arxana-browser-code-cycle-docs-paragraph (activate)
  "Cycle doc links in the docs buffer from a code buffer.
With prefix ACTIVATE, open the symbol after cycling."
  (interactive "P")
  (let ((docs-buf (get-buffer arxana-browser-code-docs-buffer)))
    (unless docs-buf
      (user-error "No docs buffer available"))
    (with-current-buffer docs-buf
      (arxana-browser-code-docs-cycle-paragraph activate))
    (let ((win (get-buffer-window docs-buf t)))
      (when win
        (with-selected-window win
          (recenter))))))

(defun arxana-browser-code--linkify-docs (path entries)
  (let* ((symbols (and path (arxana-browser-code--file-symbols path)))
         (filename (and path (file-name-nondirectory path))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "arxana-[A-Za-z0-9-]+" nil t)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (symbol (match-string 0))
               (target (cond
                        ((member symbol symbols) path)
                        ((arxana-browser-code--find-symbol-path symbol))
                        ((and filename (string= symbol filename)) path)
                        (t nil))))
          (when target
            (add-text-properties
             start end
             (list 'arxana-symbol symbol
                   'arxana-path target
                   'face 'link
                   'font-lock-face 'link
                   'keymap arxana-browser-code-docs-link-map
                   'mouse-face 'highlight
                   'help-echo "Open symbol")))))
    (when filename
      (save-excursion
        (goto-char (point-min))
        (while (search-forward filename nil t)
          (add-text-properties
           (match-beginning 0) (match-end 0)
           (list 'arxana-path path
                 'face 'arxana-browser-code-docs-file-link-face
                 'font-lock-face 'arxana-browser-code-docs-file-link-face
                 'keymap arxana-browser-code-docs-link-map
                 'mouse-face 'highlight
                 'help-echo "Open file"))))
    (when (and symbols path)
      (dolist (symbol symbols)
        (save-excursion
          (goto-char (point-min))
          (while (search-forward symbol nil t)
            (add-text-properties
             (match-beginning 0) (match-end 0)
             (list 'arxana-symbol symbol
                   'arxana-path path
                   'face 'link
                   'font-lock-face 'link
                   'keymap arxana-browser-code-docs-link-map
                   'mouse-face 'highlight
                   'help-echo "Open symbol")))))))
)))

(defun arxana-browser-code--render-docs (path entries)
  (let ((buf (get-buffer-create arxana-browser-code-docs-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: Docs for %s\n\n" (file-name-nondirectory path)))
        (insert (format "- File: %s\n\n" (arxana-browser-code--relative-path path)))
        (let (preview-start)
          (if entries
              (progn
                (insert "* Matches\n")
                (dolist (entry entries)
                  (let* ((doc-id (plist-get entry :doc-id))
                         (title (arxana-browser-code--entry-title entry))
                         (uri (and doc-id
                                   (format "docbook://%s/%s" arxana-browser-code-docbook doc-id))))
                    (insert (format "- %s\n" title))
                    (when uri
                      (insert (format "  - %s\n" uri)))))
                (let* ((entry (car entries))
                       (title (arxana-browser-code--entry-title entry))
                       (doc-id (plist-get entry :doc-id))
                       (uri (and doc-id
                                 (format "docbook://%s/%s" arxana-browser-code-docbook doc-id)))
                       (body (ignore-errors (arxana-docbook--entry-content entry))))
                  (insert "\n* Doc Preview\n")
                  (insert (format "** %s\n" title))
                  (when uri
                    (insert (format "- %s\n\n" uri)))
                  (setq preview-start (point))
                  (if (and body (> (length (string-trim body)) 0))
                      (insert body "\n")
                    (insert "(No content available)\n"))))
            (insert "* Matches\n- (none)\n"))
          (setq arxana-browser-code--doc-symbol-map
                (arxana-browser-code--index-docs path preview-start))
          (setq arxana-browser-code--doc-source-path path)
          (setq arxana-browser-code--doc-last-symbol nil)))
      (arxana-browser-code--linkify-docs path entries)
      (goto-char (point-min))
      (view-mode 1)
      (arxana-browser-code-docs-mode 1))
    buf))

(defun arxana-browser-code--index-docs (path preview-start)
  (let ((symbols (and path (arxana-browser-code--file-symbols path)))
        (map '()))
    (when (and preview-start symbols)
      (dolist (symbol symbols)
        (save-excursion
          (goto-char preview-start)
          (when (search-forward symbol nil t)
            (let ((bounds (bounds-of-thing-at-point 'paragraph)))
              (when bounds
                (push (cons symbol bounds) map)))))))
    (nreverse map)))

(defun arxana-browser-code--current-def-symbol ()
  (save-excursion
    (when (ignore-errors (beginning-of-defun) t)
      (when (looking-at
             "^[[:space:]]*(\\(defun\\|defmacro\\|defsubst\\|defvar\\|defvar-local\\|defcustom\\|defconst\\|define-derived-mode\\|defn\\|defn-\\)\\s-+\\([^[:space:]\n]+\\)")
        (match-string 2)))))

(defun arxana-browser-code--sync-docs ()
  (when (and arxana-browser-code-sync-docs
             buffer-file-name)
    (let* ((path (arxana-browser-code--normalize-path buffer-file-name))
           (symbol (arxana-browser-code--current-def-symbol))
           (docs-buf (get-buffer arxana-browser-code-docs-buffer)))
      (when docs-buf
        (with-current-buffer docs-buf
          (when (and arxana-browser-code--doc-source-path
                     (string= (arxana-browser-code--normalize-path arxana-browser-code--doc-source-path)
                              path))
            (let ((bounds (and symbol
                               (cdr (assoc symbol arxana-browser-code--doc-symbol-map)))))
              (if (not bounds)
                  (when (overlayp arxana-browser-code--doc-highlight-overlay)
                    (delete-overlay arxana-browser-code--doc-highlight-overlay)
                    (setq arxana-browser-code--doc-highlight-overlay nil)
                    (setq arxana-browser-code--doc-last-symbol nil))
                (unless (overlayp arxana-browser-code--doc-highlight-overlay)
                  (setq arxana-browser-code--doc-highlight-overlay
                        (make-overlay (car bounds) (cdr bounds)))
                  (overlay-put arxana-browser-code--doc-highlight-overlay
                               'face 'arxana-browser-code-docs-highlight-face))
                (move-overlay arxana-browser-code--doc-highlight-overlay
                              (car bounds) (cdr bounds))
                (setq arxana-browser-code--doc-last-symbol symbol)
                (let ((win (get-buffer-window docs-buf t)))
                  (when win
                    (with-selected-window win
                      (goto-char (car bounds))
                      (recenter))))))))))))

(define-minor-mode arxana-browser-code-sync-mode
  "Sync code point with the docs buffer."
  :lighter " CodeSync"
  :keymap arxana-browser-code-sync-mode-map
  (if arxana-browser-code-sync-mode
      (add-hook 'post-command-hook #'arxana-browser-code--sync-docs nil t)
    (remove-hook 'post-command-hook #'arxana-browser-code--sync-docs t)))

(defun arxana-browser-code--display-docs (path)
  (let ((buf (arxana-browser-code--render-docs path
                                               (arxana-browser-code--docbook-matches path))))
    (display-buffer-in-side-window
     buf
     (list (cons 'side arxana-browser-code-docs-side)
           (cons 'window-width arxana-browser-code-docs-width)))))

(defun arxana-browser-code--frame-by-name (name)
  (seq-find (lambda (frame)
              (equal (frame-parameter frame 'name) name))
            (frame-list)))

(defun arxana-browser-code--frame-showing-buffer (buffer)
  (seq-find (lambda (frame)
              (get-buffer-window buffer frame))
            (frame-list)))

(defun arxana-browser-code--ensure-frame (name)
  (or (arxana-browser-code--frame-by-name name)
      (make-frame (list (cons 'name name)))))

(defun arxana-browser-code--show-browser-frame ()
  (let* ((browser-buf (get-buffer "*Arxana Browser*"))
         (existing (and browser-buf
                        (or (arxana-browser-code--frame-showing-buffer browser-buf)
                            (arxana-browser-code--frame-by-name arxana-browser-code-browser-frame-name)
                            (arxana-browser-code--frame-by-name "*Arxana Browser*")))))
    (when browser-buf
      (let ((frame (or existing
                       (arxana-browser-code--ensure-frame arxana-browser-code-browser-frame-name))))
        (with-selected-frame frame
          (unless (get-buffer-window browser-buf frame)
            (set-window-buffer (selected-window) browser-buf)))))))

(defun arxana-browser-code-normalize-frames ()
  "Normalize frames so browser and code/docs are separated."
  (interactive)
  (let ((browser-buf (get-buffer "*Arxana Browser*")))
    (when browser-buf
      (arxana-browser-code--show-browser-frame)))
  (let* ((code-frame (or (arxana-browser-code--frame-by-name arxana-browser-code-frame-name)
                         (selected-frame)))
         (docs-buf (get-buffer arxana-browser-code-docs-buffer)))
    (when (and docs-buf (frame-live-p code-frame))
      (with-selected-frame code-frame
        (when (not (get-buffer-window docs-buf code-frame))
          (display-buffer-in-side-window
           docs-buf
           (list (cons 'side arxana-browser-code-docs-side)
                 (cons 'window-width arxana-browser-code-docs-width)
                 (cons 'frame code-frame))))))))

(defun arxana-browser-code-open (item)
  "Open code ITEM and show related docbook entries."
  (arxana-browser-code--open-path (plist-get item :path)))

;;;###autoload
(defun arxana-browser-code-strategy-status ()
  "Display the current link strategy for code-docs browsing."
  (interactive)
  (let ((strategy (arxana-browser-code-ensure-strategy)))
    (if strategy
        (message "Active strategy: %s (repo: %s, docbook: %s)"
                 (plist-get strategy :xt/id)
                 (plist-get (plist-get strategy :scope) :repo)
                 (plist-get (plist-get strategy :scope) :docbook))
      (message "No active strategy (Futon sync may be disabled)"))))

;;;###autoload
(defun arxana-browser-code-reset-strategy ()
  "Reset the cached strategy, forcing reload on next use."
  (interactive)
  (setq arxana-browser-code--active-strategy nil)
  (message "Strategy cache cleared"))

(provide 'arxana-browser-code)

;;; arxana-browser-code.el ends here
