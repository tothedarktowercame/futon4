;;; arxana-docbook-ui.el --- Docbook UI helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; UI helpers and interactive commands for docbook browsing.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'org)
(require 'arxana-ui nil t)

(declare-function arxana-docbook--entries-for "arxana-docbook-core" (book))
(declare-function arxana-docbook--entries-for-doc "arxana-docbook-core" (book doc-id))
(declare-function arxana-docbook--entry-content "arxana-docbook-core" (entry))
(declare-function arxana-docbook--available-books "arxana-docbook-core")
(declare-function arxana-docbook-entries "arxana-docbook-core" (book))
(declare-function arxana-docbook--entries-by-doc-id "arxana-docbook-core" (entries))
(declare-function arxana-browser-browse "arxana-browser")
(declare-function arxana-browser--render "arxana-browser-core")
(declare-function arxana-links-load-strategies "arxana-links" (&optional type-filter))
(declare-function arxana-links-edit-surface-form-at-point "arxana-links" ())
(declare-function arxana-docbook--entry-doc-ids "arxana-docbook-toc" (book))
(declare-function arxana-docbook--entry-source-path "arxana-docbook-core" (entry))
(declare-function arxana-docbook--entry-version "arxana-docbook-core" (entry))
(declare-function arxana-docbook--heading-for-doc-id "arxana-docbook-toc" (book doc-id))
(declare-function arxana-docbook--heading-level "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--heading-outline "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--heading-path-string "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--heading-title "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--lab-draft-p "arxana-docbook-core" (entry))
(declare-function arxana-docbook--lab-drafts "arxana-docbook-core" (entries))
(declare-function arxana-docbook--latest-non-lab "arxana-docbook-core" (entries))
(declare-function arxana-docbook--normalize-remote-entry "arxana-docbook-remote" (entry))
(declare-function arxana-docbook--read-stub "arxana-docbook-core" (entry))
(declare-function arxana-docbook--remote-available-p "arxana-docbook-remote" (&optional book))
(declare-function arxana-docbook--probe-filesystem "arxana-docbook-remote" (&optional book))
(declare-function arxana-docbook--probe-storage "arxana-docbook-remote" (&optional book))
(declare-function arxana-docbook--remote-heading "arxana-docbook-remote" (book doc-id))
(declare-function arxana-docbook--remote-contents "arxana-docbook-remote" (book))
(declare-function arxana-docbook--remote-recent "arxana-docbook-remote" (book))
(declare-function arxana-docbook--source-brief "arxana-docbook-remote" (&optional book source))
(declare-function arxana-docbook--source-label "arxana-docbook-remote" (&optional book source))
(declare-function arxana-docbook--toc-doc-id "arxana-docbook-toc" (heading))
(declare-function arxana-docbook--toc-for-view "arxana-docbook-toc" (book))
(declare-function arxana-docbook--toc-write-headings "arxana-docbook-toc" (book headings &optional order))
(declare-function arxana-docbook--toc-write-order "arxana-docbook-toc" (book order))
(declare-function arxana-docbook--toc-path "arxana-docbook-toc" (book))
(declare-function arxana-docbook--probe-summary "arxana-docbook-remote" (&optional book))
(declare-function arxana-docbook--data-source "arxana-docbook-remote" (&optional book))
(declare-function arxana-docbook--filesystem-available-p "arxana-docbook-core" (&optional book))
(declare-function arxana-docbook--repo-root "arxana-docbook-core")
(declare-function arxana-docbook--toc-remove-doc-id "arxana-docbook-toc" (book doc-id))
(declare-function arxana-docbook--filesystem-delete-doc "arxana-docbook-toc" (book doc-id))
(declare-function arxana-docbook--remote-delete-toc "arxana-docbook-remote" (book doc-id &optional cascade))
(declare-function arxana-docbook--remote-delete-doc "arxana-docbook-remote" (book doc-id))
(declare-function arxana-docbook--demote-org "arxana-docbook-export" (text))
(declare-function arxana-docbook--remote-enabled-p "arxana-docbook-remote" ())
(declare-function arxana-docbook--entry-raw-text "arxana-docbook-core" (entry))
(declare-function arxana-docbook--entry-function-name "arxana-docbook-core" (entry))
(declare-function arxana-docbook--entry-mtime "arxana-docbook-core" (entry))
(declare-function arxana-links-load-voiced-links "arxana-links" (&optional strategy-id))
(declare-function arxana-links-load-surface-forms "arxana-links" (&optional concept-id))
(declare-function arxana-scholium-show-for-doc "arxana-scholium" (target-doc &optional source-buffer scholia))
(declare-function arxana-links-load-scholia-for-doc "arxana-links" (doc-name))
(declare-function arxana-docbook--normalize-timestamp "arxana-docbook-core" (value))
(declare-function arxana-docbook--strip-org-metadata "arxana-docbook-core" (text))
(declare-function arxana-docbook--strip-org-code-blocks "arxana-docbook-core" (text))
(declare-function arxana-docbook--strip-stub-header "arxana-docbook-core" (text))
(declare-function arxana-docbook--session-id-from-run "arxana-docbook-core" (run-id))
(declare-function arxana-docbook-open-stub "arxana-docbook-checkout" (&optional entry))
(declare-function arxana-browser-code--file-symbols "arxana-browser-code" (path))
(declare-function arxana-browser-code--find-symbol-path "arxana-browser-code" (symbol))
(declare-function arxana-browser-code--open-symbol "arxana-browser-code" (symbol path))
(declare-function arxana-browser-code--open-path "arxana-browser-code" (path))

(defface arxana-docbook-source-green
  '((t :foreground "ForestGreen" :weight bold))
  "Face for storage-backed docbook sources."
  :group 'arxana-docbook)
(defface arxana-docbook-source-amber
  '((t :foreground "DarkGoldenrod" :weight bold))
  "Face for filesystem-backed docbook sources."
  :group 'arxana-docbook)
(defface arxana-docbook-source-red
  '((t :foreground "IndianRed" :weight bold))
  "Face for state-only docbook sources."
  :group 'arxana-docbook)

(defface arxana-docbook-symbol-link-face
  '((t :inherit link :weight bold :underline t))
  "Face for symbol links in docbook entry buffers."
  :group 'arxana-docbook)

(defface arxana-docbook-surface-link-face
  '((t :inherit link :underline t))
  "Face for surface form links in docbook entry buffers."
  :group 'arxana-docbook)

(defvar arxana-docbook-symbol-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'arxana-docbook-open-symbol-at-point)
    (define-key map (kbd "RET") #'arxana-docbook-open-symbol-at-point)
    map)
  "Keymap for symbol links in docbook entry buffers.")

(defvar arxana-docbook-surface-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'arxana-links-edit-surface-form-at-point)
    (define-key map (kbd "RET") #'arxana-links-edit-surface-form-at-point)
    map)
  "Keymap for surface form links in docbook entry buffers.")

(defun arxana-docbook--entry-title-text (entry)
  (or (plist-get entry :title)
      (plist-get entry :doc/title)
      (when-let* ((raw (plist-get entry :entry)))
        (or (plist-get raw :doc/title)
            (plist-get raw :doc/title_string)))))

(defun arxana-docbook--entry-files (entry)
  (or (plist-get entry :files)
      (when-let* ((raw (plist-get entry :entry)))
        (or (plist-get raw :doc/files)
            (plist-get raw :doc/files_list)
            (plist-get raw :doc/files-list)))))

(defun arxana-docbook--guess-source-path (title)
  "Return a source path derived from TITLE, if any."
  (when (and title (stringp title))
    (let ((case-fold-search nil))
      (cond
       ((string-match "\\`\\(dev\\|test\\)/[^[:space:]]+\\.[a-z]+\\'" title)
        (match-string 0 title))
       ((string-match "\\(dev/[^[:space:]]+\\.[a-z]+\\)" title)
        (match-string 1 title))
       (t nil)))))

(defun arxana-docbook-open-symbol-at-point ()
  "Open the symbol link at point, if any."
  (interactive)
  (let* ((symbol (or (get-text-property (point) 'arxana-symbol)
                     (and (> (point) (point-min))
                          (get-text-property (1- (point)) 'arxana-symbol))))
         (path (or (get-text-property (point) 'arxana-path)
                   (and (> (point) (point-min))
                        (get-text-property (1- (point)) 'arxana-path)))))
    (cond
     ((and symbol path (fboundp 'arxana-browser-code--open-symbol))
      (arxana-browser-code--open-symbol symbol path))
     (path
      (if (fboundp 'arxana-browser-code--open-path)
          (arxana-browser-code--open-path path)
        (find-file path)))
     (t
      (user-error "No symbol link at point")))))

(defun arxana-docbook--symbol-link-face ()
  (if (facep 'arxana-docbook-symbol-link-face)
      'arxana-docbook-symbol-link-face
    'link))

(defun arxana-docbook--ensure-browser-code ()
  "Ensure arxana-browser-code helpers are available."
  (or (featurep 'arxana-browser-code)
      (require 'arxana-browser-code nil t)
      (let* ((repo (arxana-docbook--repo-root))
             (path (and repo (expand-file-name "dev/arxana-browser-code.el" repo))))
        (when (and path (file-readable-p path))
          (load path t t)
          (featurep 'arxana-browser-code)))))

(defvar-local arxana-docbook--symbol-overlays nil
  "Overlays used for symbol links in docbook entry buffers.")

(defvar-local arxana-docbook--surface-overlays nil
  "Overlays used for surface form links in docbook entry buffers.")

(defvar-local arxana-docbook--surface-tooltip-last nil
  "Last surface form id shown in a tooltip.")

(defun arxana-docbook--clear-symbol-overlays ()
  (when arxana-docbook--symbol-overlays
    (dolist (overlay arxana-docbook--symbol-overlays)
      (delete-overlay overlay))
    (setq arxana-docbook--symbol-overlays nil)))

(defun arxana-docbook--clear-surface-overlays ()
  (when arxana-docbook--surface-overlays
    (dolist (overlay arxana-docbook--surface-overlays)
      (delete-overlay overlay))
    (setq arxana-docbook--surface-overlays nil)))

(defun arxana-docbook--surface-tooltip (&rest _args)
  "Show a tooltip for the surface form at point, when available."
  (let* ((form-id (get-text-property (point) 'arxana-surface-form-id))
         (concept-id (get-text-property (point) 'arxana-concept-id)))
    (when (and form-id concept-id
               (not (equal form-id arxana-docbook--surface-tooltip-last)))
      (setq arxana-docbook--surface-tooltip-last form-id)
      (if (fboundp 'tooltip-show)
          (tooltip-show (format "Surface form → %s" concept-id))
        (message "Surface form → %s" concept-id)))))

(defun arxana-docbook--repo-root-for-entry (entry)
  "Return repo root for ENTRY, preferring the current docbook book."
  (let* ((book (or (plist-get entry :book) arxana-docbook--entry-book))
         (books-root (arxana-docbook--locate-books-root)))
    (or (when (and book books-root)
          (let* ((book-root (expand-file-name book books-root))
                 (repo-root (expand-file-name "../../.." book-root)))
            (when (file-directory-p repo-root)
              repo-root)))
        (arxana-docbook--repo-root))))

(defun arxana-docbook--linkify-symbols (entry &optional title)
  "Add symbol links inside the current docbook entry buffer for ENTRY."
  (arxana-docbook--ensure-browser-code)
  (arxana-docbook--clear-symbol-overlays)
  (let* ((source (and entry (arxana-docbook--entry-source-path entry)))
         (title (or title (and entry (arxana-docbook--entry-title-text entry))))
         (files (and entry (arxana-docbook--entry-files entry)))
         (source (or source
                     (and files (car-safe files))
                     (arxana-docbook--guess-source-path title)))
         (repo (arxana-docbook--repo-root-for-entry entry))
         (path (and source repo (expand-file-name source repo)))
         (symbols (and path (fboundp 'arxana-browser-code--file-symbols)
                       (arxana-browser-code--file-symbols path)))
         (face (arxana-docbook--symbol-link-face)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "arxana-[A-Za-z0-9-]+" nil t)
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               (symbol (match-string 0))
               (target (cond
                        ((and symbols (member symbol symbols)) path)
                        ((fboundp 'arxana-browser-code--find-symbol-path)
                         (arxana-browser-code--find-symbol-path symbol))
                        (t nil))))
          (when target
            (add-text-properties
             start end
             (list 'arxana-symbol symbol
                   'arxana-path target
                   'face face
                   'font-lock-face face))
            (let ((overlay (make-overlay start end)))
              (overlay-put overlay 'arxana-docbook-symbol t)
              (overlay-put overlay 'arxana-symbol symbol)
              (overlay-put overlay 'arxana-path target)
              (overlay-put overlay 'face face)
              (overlay-put overlay 'keymap arxana-docbook-symbol-link-map)
              (overlay-put overlay 'mouse-face 'highlight)
              (overlay-put overlay 'help-echo "Open symbol")
              (push overlay arxana-docbook--symbol-overlays))))))))

(defun arxana-docbook--surface-form-strategy-enabled-p (book)
  (when (fboundp 'arxana-links-load-strategies)
    (let* ((strategies (arxana-links-load-strategies))
           (match nil))
      (dolist (strategy strategies)
        (when (and (not match)
                   (let* ((scope (plist-get strategy :scope))
                          (docbook (and scope (plist-get scope :docbook))))
                     (or (not docbook)
                         (equal docbook book))))
          (let ((finders (plist-get strategy :finders)))
            (when (and (listp finders)
                       (seq-some (lambda (finder)
                                   (let* ((value (plist-get finder :type))
                                          (key-name "surface-form-hybrid"))
                                     (or (eq value :surface-form-hybrid)
                                         (equal value key-name)
                                         (equal value ":surface-form-hybrid"))))
                                 finders))
              (setq match t)))))
      match)))

(defun arxana-docbook--word-boundary-ok-p (start end)
  (let ((before (char-before start))
        (after (char-after end)))
    (and (or (not before)
             (not (string-match-p "\\w" (string before))))
         (or (not after)
             (not (string-match-p "\\w" (string after)))))))

(defun arxana-docbook--linkify-surface-forms (entry)
  "Add surface form links inside the current docbook entry buffer for ENTRY."
  (arxana-docbook--clear-surface-overlays)
  (let* ((book (or (plist-get entry :book) arxana-docbook--entry-book))
         (enabled (arxana-docbook--surface-form-strategy-enabled-p book)))
    (when (and enabled
               (fboundp 'arxana-links-load-surface-forms))
      (let* ((forms (arxana-links-load-surface-forms))
             (forms (cl-remove-if-not
                     (lambda (form)
                       (and (plist-get form :surface)
                            (plist-get form :concept-id)))
                     forms))
             (forms (seq-sort-by
                     (lambda (form)
                       (length (plist-get form :surface)))
                     #'>
                     forms))
             (face 'arxana-docbook-surface-link-face))
        (when forms
          (save-excursion
            (dolist (form forms)
              (let* ((surface (plist-get form :surface))
                     (form-id (plist-get form :xt/id))
                     (concept-id (plist-get form :concept-id)))
                (goto-char (point-min))
                (while (search-forward surface nil t)
                  (let ((start (- (point) (length surface)))
                        (end (point)))
                    (when (and (arxana-docbook--word-boundary-ok-p start end)
                               (not (and (fboundp 'org-in-src-block-p)
                                         (org-in-src-block-p)))
                               (not (get-text-property start 'arxana-symbol))
                               (not (get-text-property start 'arxana-surface-form-id)))
                      (add-text-properties
                       start end
                       (list 'arxana-surface-form-id form-id
                             'arxana-concept-id concept-id
                             'face face
                             'font-lock-face face
                             'help-echo (format "Surface form → %s" concept-id)
                             'cursor-sensor-functions
                             (list #'arxana-docbook--surface-tooltip)))
                      (let ((overlay (make-overlay start end)))
                        (overlay-put overlay 'arxana-docbook-surface t)
                        (overlay-put overlay 'arxana-surface-form-id form-id)
                        (overlay-put overlay 'arxana-concept-id concept-id)
                        (overlay-put overlay 'face face)
                        (overlay-put overlay 'keymap arxana-docbook-surface-link-map)
                        (overlay-put overlay 'mouse-face 'highlight)
                        (overlay-put overlay 'help-echo (format "Surface form → %s" concept-id))
                        (overlay-put overlay 'cursor-sensor-functions
                                     (list #'arxana-docbook--surface-tooltip))
                        (push overlay arxana-docbook--surface-overlays)))))))))))))

(defun arxana-docbook--voiced-links-for-entry (entry)
  (when (and (fboundp 'arxana-links-load-voiced-links)
             (fboundp 'arxana-store-sync-enabled-p)
             (arxana-store-sync-enabled-p))
    (let* ((book (or (plist-get entry :book) arxana-docbook--entry-book))
           (doc-id (or (plist-get entry :doc-id) arxana-docbook--entry-doc-id))
           (links (arxana-links-load-voiced-links)))
      (cl-remove-if-not
       (lambda (link)
         (let* ((target (plist-get link :target))
                (target-book (plist-get target :docbook))
                (target-doc (plist-get target :doc-id)))
           (and (equal target-book book)
                (equal target-doc doc-id))))
       links))))

(defun arxana-docbook--resolve-voiced-source-path (entry source)
  (let* ((repo (arxana-docbook--repo-root-for-entry entry))
         (file (plist-get source :file)))
    (cond
     ((and file (file-name-absolute-p file) (file-readable-p file)) file)
     ((and repo file
           (file-readable-p (expand-file-name file repo)))
      (expand-file-name file repo))
     ((and repo file (string-prefix-p "futon4/" file)
           (file-readable-p (expand-file-name (string-remove-prefix "futon4/" file) repo)))
      (expand-file-name (string-remove-prefix "futon4/" file) repo))
     (t nil))))

(defun arxana-docbook--render-voiced-links (entry)
  (let ((links (arxana-docbook--voiced-links-for-entry entry)))
    (when links
      (insert "\n* Voiced links\n")
      (dolist (link links)
        (let* ((source (plist-get link :source))
               (symbol (plist-get source :symbol))
               (path (arxana-docbook--resolve-voiced-source-path entry source))
               (label (or symbol "(unknown symbol)"))
               (file-label (or (plist-get source :file) "(unknown file)"))
               (line-start (point)))
          (insert (format "- %s (%s)\n" label file-label))
          (when (and symbol path)
            (let* ((line-end (line-end-position))
                   (sym-start (save-excursion
                                (goto-char line-start)
                                (search-forward label line-end t)
                                (- (point) (length label))))
                   (sym-end (and sym-start (+ sym-start (length label))))
                   (face (arxana-docbook--symbol-link-face)))
              (when (and sym-start sym-end)
                          (add-text-properties
                           sym-start sym-end
                           (list 'arxana-symbol symbol
                                 'arxana-path path
                                 'face face
                                 'font-lock-face face))))))))))

(defun arxana-docbook--edn-escape (text)
  (replace-regexp-in-string
   "\\\\" "\\\\\\\\"
   (replace-regexp-in-string "\"" "\\\"" text t t) t t))

(defun arxana-docbook--plist-p (value)
  (and (listp value)
       (keywordp (car value))
       (cl-evenp (length value))))

(defun arxana-docbook--edn-value (value)
  (cond
   ((stringp value) (format "\"%s\"" (arxana-docbook--edn-escape value)))
   ((keywordp value) (format ":%s" (substring (symbol-name value) 0)))
   ((symbolp value) (symbol-name value))
   ((eq value t) "true")
   ((null value) "nil")
   ((numberp value) (format "%s" value))
   ((arxana-docbook--plist-p value)
    (arxana-docbook--edn-map value))
   ((listp value)
    (format "[%s]" (mapconcat #'arxana-docbook--edn-value value " ")))
   (t (format "%S" value))))

(defun arxana-docbook--edn-map (plist)
  (let (pairs)
    (while plist
      (let ((key (pop plist))
            (val (pop plist)))
        (when key
          (push (cons (substring (symbol-name key) 1)
                      (arxana-docbook--edn-value val))
                pairs))))
    (format "{%s}"
            (mapconcat
             (lambda (pair)
               (format ":%s %s" (car pair) (cdr pair)))
             (nreverse pairs)
             ", "))))

(defun arxana-docbook--edn-map-pretty (plist)
  "Return a pretty-printed EDN map string from PLIST."
  (let ((pairs '())
        (max-key 0))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist)))
        (when key
          (let* ((k (substring (symbol-name key) 1))
                 (v (arxana-docbook--edn-value val)))
            (setq max-key (max max-key (length k)))
            (push (cons k v) pairs)))))
    (setq pairs (nreverse pairs))
    (if (null pairs)
        "{}"
      (concat "{\n"
              (mapconcat
               (lambda (pair)
                 (format "  :%s%s %s"
                         (car pair)
                         (make-string (- max-key (length (car pair))) ? )
                         (cdr pair)))
               pairs
               "\n")
              "\n}"))))

(defun arxana-docbook--surface-form-score (form)
  (let* ((context (plist-get form :context))
         (doc (and context (plist-get context :doc))))
    (cond
     ((and (stringp doc) (string-prefix-p "docbook://" doc)) 3)
     ((and (stringp doc) (string-match-p "/" doc)) 2)
     ((and (stringp doc) (not (string-prefix-p "*" doc))) 1)
     (t 0))))

(defun arxana-docbook--select-surface-form (concept-id form-id)
  (let* ((forms (if form-id
                    (cl-remove-if-not
                     (lambda (form)
                       (equal (plist-get form :xt/id) form-id))
                     (arxana-links-load-surface-forms))
                  (and concept-id
                       (arxana-links-load-surface-forms concept-id)))))
    (when (and forms (listp forms))
      (car (seq-sort-by #'arxana-docbook--surface-form-score #'>
                        (copy-sequence forms))))))

(defun arxana-docbook--render-link-examples (entry)
  "Render live UI examples for links in the current buffer."
  (when (and (fboundp 'arxana-links-load-voiced-links)
             (fboundp 'arxana-links-load-surface-forms))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+BEGIN_ARXANA_LINK_EXAMPLES\\s-*$" nil t)
        (let ((block-start (line-beginning-position)))
          (when (re-search-forward "^#\\+END_ARXANA_LINK_EXAMPLES\\s-*$" nil t)
            (let* ((block-end (line-end-position))
                   (raw (buffer-substring-no-properties block-start block-end))
                   (concept-id (when (string-match "^:concept-id:[ \t]*\\(.+\\)$" raw)
                                 (string-trim (match-string 1 raw))))
                   (surface-id (when (string-match "^:surface-form-id:[ \t]*\\(.+\\)$" raw)
                                 (string-trim (match-string 1 raw))))
                   (voiced-id (when (string-match "^:voiced-link-id:[ \t]*\\(.+\\)$" raw)
                                (string-trim (match-string 1 raw))))
                   (surface (arxana-docbook--select-surface-form concept-id surface-id))
                   (voiced (and voiced-id
                                (car (cl-remove-if-not
                                      (lambda (link)
                                        (equal (plist-get link :xt/id) voiced-id))
                                      (arxana-links-load-voiced-links)))))
                   (surface-edn (and surface
                                     (arxana-docbook--edn-map-pretty
                                      (list :xt/id (plist-get surface :xt/id)
                                            :type (plist-get surface :type)
                                            :concept-id (plist-get surface :concept-id)
                                            :concept-label (plist-get surface :concept-label)
                                            :surface (plist-get surface :surface)
                                            :term (plist-get surface :term)
                                            :gloss (plist-get surface :gloss)
                                            :source (plist-get surface :source)
                                            :context (plist-get surface :context)))))
                   (voiced-edn (and voiced
                                    (arxana-docbook--edn-map-pretty
                                     (list :xt/id (plist-get voiced :xt/id)
                                           :type (plist-get voiced :type)
                                           :name (plist-get voiced :name)
                                           :source (plist-get voiced :source)
                                           :target (plist-get voiced :target)))))
                   (face (arxana-docbook--symbol-link-face)))
              (delete-region block-start (min (point-max) (1+ block-end)))
              (goto-char block-start)
              (insert "\n* Examples (inspectable)\n")
              (insert "** EDN\n")
              (insert "*** Surface form\n")
              (if surface-edn
                  (insert (format "#+begin_src edn\n%s\n#+end_src\n"
                                  surface-edn))
                (insert "- (no matching surface form)\n"))
              (insert "\n*** Voiced link\n")
              (if voiced-edn
                  (insert (format "#+begin_src edn\n%s\n#+end_src\n"
                                  voiced-edn))
                (insert "- (no matching voiced link)\n"))
              (insert "\n** Live UI\n")
              (insert "*** Surface form\n")
              (if surface
                  (let ((line-start (point)))
                    (insert (format "- \"%s\" -> %s (source: %s)\n"
                                    (plist-get surface :surface)
                                    (plist-get surface :concept-id)
                                    (plist-get surface :source)))
                    (add-text-properties
                     line-start (line-end-position -1)
                     (list 'arxana-surface-form-id (plist-get surface :xt/id))))
                (insert "- (no matching surface form)\n"))
              (insert "\n*** Voiced link\n")
              (if voiced
                  (let* ((source (plist-get voiced :source))
                         (symbol (plist-get source :symbol))
                         (path (arxana-docbook--resolve-voiced-source-path entry source))
                         (label (or symbol "(unknown symbol)"))
                         (line-start (point)))
                    (insert (format "- %s (%s)\n" label (or (plist-get source :file) "(unknown file)")))
                    (when (and symbol path)
                      (let* ((line-end (line-end-position))
                             (sym-start (save-excursion
                                          (goto-char line-start)
                                          (search-forward label line-end t)
                                          (- (point) (length label))))
                             (sym-end (and sym-start (+ sym-start (length label)))))
                        (when (and sym-start sym-end)
                          (add-text-properties
                           sym-start sym-end
                           (list 'arxana-symbol symbol
                                 'arxana-path path
                                 'face face
                                 'font-lock-face face))))))
                (insert "- (no matching voiced link)\n")))))))))

(defface arxana-docbook-new-content
  '((t :background "#2b2b2b"))
  "Face for lab-draft additions in docbook views."
  :group 'arxana-docbook)
(defface arxana-docbook-entry-dirty-face
  '((t :foreground "orange"))
  "Face for dirty docbook entry headers."
  :group 'arxana-docbook)

(defcustom arxana-docbook-open-uri-prefer-render t
  "When non-nil, open docbook URIs in the rendered entry view."
  :type 'boolean
  :group 'arxana-docbook)
(defvar-local arxana-docbook--book nil
  "Current doc book identifier (e.g., futon4) for the browser buffer.")
(defvar-local arxana-docbook--source :filesystem
  "Current doc book data source (:storage, :filesystem, :state).")
(defvar-local arxana-docbook--storage-probe nil
  "Cached probe data for the remote docbook source.")
(defvar-local arxana-docbook--filesystem-probe nil
  "Cached probe data for the filesystem docbook source.")
(defvar-local arxana-docbook--entry-book nil
  "Book name for the current docbook entry buffer.")
(defvar-local arxana-docbook--entry-doc-id nil
  "Doc id for the current docbook entry buffer.")
(defvar-local arxana-docbook--entry-entry-id nil
  "Entry id for the current docbook entry buffer.")
(defvar-local arxana-docbook--entry-current nil
  "Entry plist for the current docbook entry buffer.")
(defvar-local arxana-docbook--entry-toc nil
  "Cached TOC list for the current docbook entry buffer.")
(defvar-local arxana-docbook--return-buffer nil
  "Buffer to return to when exiting a docbook entry view.")
(defvar-local arxana-docbook--return-doc-id nil
  "Doc id to select when returning to the browser.")
(defvar-local arxana-docbook--return-entry-id nil
  "Entry id to select when returning to the browser.")
(defconst arxana-docbook--entry-buffer "*Arxana Docbook*"
  "Buffer name for the main docbook entry view.")
(defconst arxana-docbook--source-buffer "*Arxana Docbook Source*"
  "Buffer name for the docbook source view.")
(defun arxana-docbook--refresh-linked-code-docs ()
  "Refresh related code docs after saving a docbook entry."
  (when (and (featurep 'arxana-browser-code)
             (boundp 'arxana-browser-code-docs-buffer))
    (let ((buf (get-buffer arxana-browser-code-docs-buffer)))
      (when buf
        (with-current-buffer buf
          (when (and arxana-browser-code--doc-entry
                     arxana-docbook--entry-doc-id
                     (equal (plist-get arxana-browser-code--doc-entry :doc-id)
                            arxana-docbook--entry-doc-id))
            (when (fboundp 'arxana-browser-code-docs-refresh)
              (arxana-browser-code-docs-refresh))))))))
(defun arxana-docbook--cleanup-entry-buffers ()
  (dolist (buf (buffer-list))
    (when (string-match-p "^\\*Arxana Docbook\\*<" (buffer-name buf))
      (kill-buffer buf))))
(defun arxana-docbook--frame-window-for-buffer (buffer)
  (seq-find (lambda (win)
              (eq (window-buffer win) buffer))
            (window-list (selected-frame) 'no-mini)))
(defun arxana-docbook--ensure-two-up (doc-buffer &optional source-buffer)
  (let* ((frame (selected-frame))
         (doc-window (or (arxana-docbook--frame-window-for-buffer doc-buffer)
                         (selected-window)))
         (source-buffer (or source-buffer
                            (get-buffer-create arxana-docbook--source-buffer)))
         (source-window (or (arxana-docbook--frame-window-for-buffer source-buffer)
                            (window-in-direction 'right doc-window)
                            (split-window doc-window nil 'right))))
    (set-window-dedicated-p doc-window nil)
    (set-window-dedicated-p source-window nil)
    (set-window-buffer doc-window doc-buffer)
    (set-window-buffer source-window source-buffer)
    (with-current-buffer source-buffer
      (when (fboundp 'arxana-ui-mark-managed)
        (arxana-ui-mark-managed "Arxana Docbook Source")))
    (set-window-dedicated-p doc-window t)
    (set-window-dedicated-p source-window t)
    (select-window doc-window)
    (dolist (win (window-list frame 'no-mini))
      (when (and (not (eq win doc-window))
                 (eq (window-buffer win) doc-buffer))
        (set-window-buffer win (get-buffer-create arxana-docbook--source-buffer)))))
    (when (fboundp 'arxana-window-constraints-validate-docbook-two-up)
      (arxana-window-constraints-validate-docbook-two-up
       doc-buffer source-buffer frame)))

(defun arxana-docbook--ensure-browser-left (browser-buffer doc-buffer)
  (let* ((frame (selected-frame))
         (browser-window (or (arxana-docbook--frame-window-for-buffer browser-buffer)
                             (selected-window)))
         (doc-window nil))
    (set-window-dedicated-p browser-window nil)
    (set-window-buffer browser-window browser-buffer)
    (delete-other-windows browser-window)
    (setq doc-window (or (window-in-direction 'right browser-window)
                         (split-window browser-window nil 'right)))
    (set-window-dedicated-p doc-window nil)
    (set-window-buffer doc-window doc-buffer)
    (set-window-dedicated-p doc-window t)
    (select-window doc-window)
    (dolist (win (window-list frame 'no-mini))
      (when (and (not (eq win doc-window))
                 (eq (window-buffer win) doc-buffer))
        (set-window-buffer win browser-buffer)))
    (when (fboundp 'arxana-window-constraints-validate-docbook-browser-left)
      (arxana-window-constraints-validate-docbook-browser-left
       browser-buffer doc-buffer frame))))

(defun arxana-docbook--browser-buffer ()
  (let ((buf (get-buffer "*Arxana Browser*")))
    (when (buffer-live-p buf)
      buf)))
(defun arxana-docbook--header-line ()
  (let ((label (arxana-docbook--source-label arxana-docbook--book arxana-docbook--source))
        (summary (arxana-docbook--probe-summary arxana-docbook--book)))
    (format "Doc book %s — %s — %s"
            (or arxana-docbook--book "unknown")
            label
            summary)))

(defun arxana-docbook--entry-dirty-p (entry)
  (or (buffer-modified-p)
      (let* ((stub (and entry (plist-get entry :stub-path)))
             (synced-at (and stub (arxana-docbook--stub-synced-at-from-path stub)))
             (file-mtime (and stub (file-attribute-modification-time
                                    (file-attributes stub))))
             (fs-dirty (and synced-at
                            file-mtime
                            (time-less-p (time-add synced-at
                                                   (seconds-to-time arxana-docbook--entry-fs-skew-seconds))
                                         file-mtime))))
        (if synced-at
            fs-dirty
          (let ((local (and entry (arxana-docbook--entry-mtime entry)))
                (remote (arxana-docbook--normalize-timestamp (plist-get entry :timestamp))))
            (and local
                 (or (not remote)
                     (time-less-p remote local))))))))

(defconst arxana-docbook--entry-fs-skew-seconds 1
  "Allowed filesystem mtime skew (seconds) before marking an entry dirty.")

(defun arxana-docbook--stub-synced-at-from-path (path)
  "Return SYNCED_AT timestamp from stub PATH when present."
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (let ((coding-system-for-read 'utf-8-unix))
        (insert-file-contents path))
      (goto-char (point-min))
      (when (re-search-forward "^:SYNCED_AT:[ \t]*\\(.*\\)$" nil t)
        (ignore-errors (date-to-time (string-trim (match-string 1))))))))

(defun arxana-docbook--entry-header-line ()
  (let* ((dirty (arxana-docbook--entry-dirty-p arxana-docbook--entry-current))
         (suffix (if dirty " [dirty]" ""))
         (text (format "Docbook entry%s — M-n/M-p next/prev; M-y copies link."
                       suffix)))
    (if dirty
        (propertize text 'face 'arxana-docbook-entry-dirty-face)
      text)))

(defun arxana-docbook--entry-update-header (&rest _args)
  (force-mode-line-update))
(defun arxana-docbook--jump-to-function (buffer name)
  (when (and buffer name)
    (with-current-buffer buffer
      (goto-char (point-min))
      (let* ((pattern (format "^(\\(cl-\\)?def\\(un\\|macro\\|subst\\|alias\\|generic\\|method\\)\\s-+%s\\_>"
                              (regexp-quote name)))
             (found (re-search-forward pattern nil t)))
        (when found
          (beginning-of-line))))))
(defun arxana-docbook--lab-addition-label (session-id)
  (if session-id
      (format "Lab addition (%s)" session-id)
    "Lab addition"))
(defun arxana-docbook--descendant-headings (book heading)
  (let* ((toc (arxana-docbook--toc-for-view book))
         (outline (arxana-docbook--heading-outline heading)))
    (when (and outline (listp outline))
      (seq-filter
       (lambda (candidate)
         (let ((child (arxana-docbook--heading-outline candidate)))
           (and (listp child)
                (> (length child) (length outline))
                (equal outline (seq-take child (length outline))))))
       toc))))
(defun arxana-docbook--first-descendant-entry (book heading)
  (catch 'found
    (dolist (child (arxana-docbook--descendant-headings book heading))
      (let* ((doc-id (arxana-docbook--toc-doc-id child))
             (entries (and doc-id (arxana-docbook--entries-for-doc book doc-id)))
             (entry (and entries (arxana-docbook--latest-non-lab entries)))
             (content (and entry (arxana-docbook--entry-content entry))))
        (when (and content (not (string-empty-p (string-trim content))))
          (throw 'found (list :heading child :entry entry :content content))))))
  nil)
(defun arxana-docbook--render-merged-heading (book heading entries)
  (let* ((doc-id (plist-get heading :doc-id))
         (title (arxana-docbook--heading-title heading))
         (entries (or entries '()))
         (base-entry (or (arxana-docbook--latest-non-lab entries)
                         (car entries)))
         (lab-entries (arxana-docbook--lab-drafts entries))
         (remote-entries (when (and (not base-entry)
                                    (arxana-docbook--remote-available-p book))
                           (ignore-errors (arxana-docbook--remote-heading book doc-id))))
         (merged (append entries remote-entries))
         (base-entry (or base-entry
                         (arxana-docbook--latest-non-lab merged)
                         (car merged)))
         (lab-entries (or lab-entries (arxana-docbook--lab-drafts merged))))
    (insert (format "* %s\n" title))
    (if base-entry
        (let* ((content (arxana-docbook--entry-content base-entry))
               (trimmed (string-trim (or content "")))
               (source-line (arxana-docbook--source-link-line base-entry)))
          (if (string-empty-p trimmed)
              (if-let* ((desc (arxana-docbook--first-descendant-entry book heading))
                        (child-title (arxana-docbook--heading-title (plist-get desc :heading)))
                        (child-content (plist-get desc :content))
                        (child-entry (plist-get desc :entry))
                        (child-source (arxana-docbook--source-link-line child-entry)))
                  (progn
                    (insert (format "** %s\n" child-title))
                    (when child-source
                      (insert child-source))
                    (insert (arxana-docbook--demote-org child-content) "\n"))
                (insert "- (no entry content yet)\n"))
            (when source-line
              (insert source-line))
            (insert (arxana-docbook--demote-org content) "\n")))
      (insert "- (no base entry yet)\n"))
    (when (and lab-entries (listp lab-entries))
      (dolist (lab lab-entries)
        (let* ((start (point))
               (run-id (plist-get lab :run-id))
               (session-id (arxana-docbook--session-id-from-run run-id))
               (lab-body (arxana-docbook--entry-content lab)))
          (insert (format "\n- %s\n"
                          (arxana-docbook--lab-addition-label session-id)))
          (insert (arxana-docbook--demote-org lab-body) "\n")
          (let ((end (point)))
            (add-text-properties start end
                                 (list 'face 'arxana-docbook-new-content
                                       'arxana-new-content t
                                       'line-prefix (propertize "> " 'face 'arxana-docbook-new-content)
                                       'wrap-prefix (propertize "> " 'face 'arxana-docbook-new-content)))))))
    (insert "\n")))
(defun arxana-docbook-open-book (&optional book)
  "Open a compiled docbook view for BOOK using filesystem entries."
  (interactive)
  (let* ((book (or book arxana-docbook--book
                   (car (arxana-docbook--available-books))))
         (toc (arxana-docbook--toc-for-view book))
         (entries (or (arxana-docbook-entries book) '()))
         (by-doc (arxana-docbook--entries-by-doc-id entries))
         (toc-docs (mapcar (lambda (h) (plist-get h :doc-id)) toc))
         (extra-docs (seq-filter (lambda (doc-id) (not (member doc-id toc-docs)))
                                 (hash-table-keys by-doc)))
         (buf (get-buffer-create (format "*DocBook Book:%s*" book))))
    (arxana-docbook--cleanup-entry-buffers)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: DocBook %s\n\n" book))
        (dolist (heading toc)
          (let* ((doc-id (plist-get heading :doc-id))
                 (entries (gethash doc-id by-doc)))
            (arxana-docbook--render-merged-heading book heading entries)))
        (when (and extra-docs (listp extra-docs))
          (insert "* Lab additions\n\n")
          (dolist (doc-id extra-docs)
            (let* ((entries (gethash doc-id by-doc))
                   (title (or (and entries
                                   (plist-get (car entries) :outline)
                                   (car (plist-get (car entries) :outline)))
                              doc-id)))
              (arxana-docbook--render-merged-heading book
                                                     (list :doc-id doc-id :title title)
                                                     entries))))
        (goto-char (point-min))
        (org-show-all)
        (visual-line-mode 1)
        (view-mode 1)))
    (let* ((return-browser (and return-buffer
                                (buffer-live-p return-buffer)
                                (with-current-buffer return-buffer
                                  (derived-mode-p 'arxana-browser-mode))
                                return-buffer))
           (existing-browser (unless return-browser
                               (let ((candidate (arxana-docbook--browser-buffer)))
                                 (when (arxana-docbook--frame-window-for-buffer candidate)
                                   candidate))))
           (browser (or return-browser existing-browser)))
      (if browser
          (arxana-docbook--ensure-browser-left browser buf)
        (pop-to-buffer buf)))))
(defun arxana-docbook-open-section-context (&optional book doc-id &rest args)
  "Open a contextual docbook view around DOC-ID for BOOK (prev/current/next)."
  (interactive)
  (let* ((toc-index (car args))
         (book (or book arxana-docbook--book
                   (car (arxana-docbook--available-books))))
         (toc (arxana-docbook--toc-for-view book))
         (entries (or (arxana-docbook-entries book) '()))
         (by-doc (arxana-docbook--entries-by-doc-id entries))
         (doc-ids (mapcar #'arxana-docbook--toc-doc-id toc))
         (doc-id (or doc-id (car doc-ids)))
         (idx (or toc-index
                  (cl-position doc-id doc-ids :test #'equal)))
         (window-ids (delq nil (if idx
                                   (list (and (> idx 0) (nth (1- idx) doc-ids))
                                         doc-id
                                         (and (< idx (1- (length doc-ids))) (nth (1+ idx) doc-ids)))
                                 (list doc-id))))
         (buf (get-buffer-create (format "*DocBook Section:%s/%s*" book doc-id))))
    (unless doc-id
      (user-error "No doc-id available for section context"))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert (format "#+TITLE: DocBook %s (context)\n\n" book))
        (dolist (did window-ids)
          (let* ((heading (seq-find (lambda (h) (equal (arxana-docbook--toc-doc-id h) did)) toc))
                 (entries (or (gethash did by-doc)
                              (arxana-docbook--entries-for-doc book did)))
                 (title (or (and heading (arxana-docbook--heading-title heading))
                            (and entries
                                 (plist-get (car entries) :outline)
                                 (car (plist-get (car entries) :outline)))
                            did))
                 (heading (or heading (list :doc-id did :title title))))
            (arxana-docbook--render-merged-heading book heading entries)))
        (goto-char (point-min))
        (org-show-all)
        (visual-line-mode 1)
        (view-mode 1)))
    (pop-to-buffer buf)))
(defun arxana-docbook--tabulated-entries (entries)
  (mapcar
   (lambda (entry)
     (let* ((doc-id (or (plist-get entry :doc-id) ""))
            (version (or (plist-get entry :version) ""))
            (timestamp (or (plist-get entry :timestamp) ""))
            (files (plist-get entry :files))
            (summary (or (plist-get entry :summary) "")))
       (list entry
             (vector doc-id
                     version
                     timestamp
                     (format "%d" (length (or files '())))
                     (truncate-string-to-width summary 96 nil nil t)))))
   entries))
(defun arxana-docbook--refresh ()
  (let* ((book arxana-docbook--book)
         (storage (arxana-docbook--probe-storage book))
         (filesystem (arxana-docbook--probe-filesystem book))
         (source (arxana-docbook--data-source book))
         (entries (pcase source
                    (:storage (if (memq (plist-get storage :status) '(:ok :empty))
                                  (or (arxana-docbook--remote-recent book) '())
                                '()))
                    (:filesystem (if (eq (plist-get filesystem :status) :ok)
                                     (or (arxana-docbook--entries-for book) '())
                                   '()))
                    (_ '()))))
    (setq arxana-docbook--storage-probe storage)
    (setq arxana-docbook--filesystem-probe filesystem)
    (setq arxana-docbook--source source)
    (setq tabulated-list-entries (arxana-docbook--tabulated-entries entries))
    (tabulated-list-init-header)
    (tabulated-list-print t)
    (force-mode-line-update)))
(defun arxana-docbook--goto-entry (doc-id &optional entry-id)
  "Move point to DOC-ID (or ENTRY-ID) in the current docbook browser buffer."
  (when (and (or doc-id entry-id) (derived-mode-p 'arxana-docbook-mode))
    (goto-char (point-min))
    (forward-line 1)
    (let ((found nil))
      (while (and (not found) (not (eobp)))
        (let ((entry (tabulated-list-get-id)))
          (when (and entry
                     (or (and entry-id (equal entry-id (plist-get entry :entry-id)))
                         (and doc-id (equal doc-id (plist-get entry :doc-id)))))
            (setq found t))
          (unless found
            (forward-line 1))))
      found)))
(defun arxana-docbook--render-entry (entry &optional return-buffer return-entry-id)
  (let* ((book (plist-get entry :book))
         (doc-id (plist-get entry :doc-id))
         (buf (get-buffer-create arxana-docbook--entry-buffer))
         (entries (arxana-docbook--entries-for-doc book doc-id))
         (heading (arxana-docbook--heading-for-doc-id book doc-id))
         (base-title (or (plist-get entry :title)
                         (and heading (arxana-docbook--heading-title heading))
                         doc-id))
         (function-name (arxana-docbook--entry-function-name entry))
         (title (if function-name
                    (format "%s — %s" function-name base-title)
                  base-title)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (let ((doc-ids (arxana-docbook--entry-doc-ids book)))
          (unless (member doc-id doc-ids)
            (setq doc-ids (append doc-ids (list doc-id))))
          (setq arxana-docbook--entry-book book
                arxana-docbook--entry-doc-id doc-id
                arxana-docbook--entry-entry-id (plist-get entry :entry-id)
                arxana-docbook--entry-current entry
                arxana-docbook--entry-toc doc-ids
                arxana-docbook--return-buffer return-buffer
                arxana-docbook--return-doc-id doc-id
                arxana-docbook--return-entry-id return-entry-id))
        (insert (format "#+TITLE: %s\n\n" (or title doc-id "")))
        (arxana-docbook--render-merged-heading book
                                               (list :doc-id doc-id :title base-title)
                                               entries)
        (arxana-docbook--render-voiced-links entry)
        (arxana-docbook--render-link-examples entry)
        (arxana-docbook--linkify-symbols entry base-title)
        (arxana-docbook--linkify-surface-forms entry)
        (goto-char (point-min))
        (org-show-all)
        (visual-line-mode 1)
        (when (fboundp 'cursor-sensor-mode)
          (cursor-sensor-mode 1))
        (read-only-mode 1)
        (arxana-docbook-entry-mode 1)
        (setq-local header-line-format '(:eval (arxana-docbook--entry-header-line)))
        (setq-local minor-mode-overriding-map-alist
                    `((arxana-docbook-entry-mode . ,arxana-docbook-entry-mode-map)))
        (when (fboundp 'arxana-ui-mark-managed)
          (arxana-ui-mark-managed "Arxana Docbook"))))
    (pop-to-buffer buf)))
(defun arxana-docbook--entry-next-doc-id ()
  (let* ((doc-id arxana-docbook--entry-doc-id)
         (doc-ids (or arxana-docbook--entry-toc '()))
         (idx (cl-position doc-id doc-ids :test #'equal)))
    (when (and idx (< (1+ idx) (length doc-ids)))
      (nth (1+ idx) doc-ids))))
(defun arxana-docbook--entry-prev-doc-id ()
  (let* ((doc-id arxana-docbook--entry-doc-id)
         (doc-ids (or arxana-docbook--entry-toc '()))
         (idx (cl-position doc-id doc-ids :test #'equal)))
    (when (and idx (> idx 0))
      (nth (1- idx) doc-ids))))
(defun arxana-docbook-next-entry ()
  "Open the next docbook entry in TOC order."
  (interactive)
  (let* ((book arxana-docbook--entry-book)
         (next-id (arxana-docbook--entry-next-doc-id)))
    (unless (and book next-id)
      (user-error "No next docbook entry"))
    (let* ((entries (arxana-docbook--entries-for-doc book next-id))
           (entry (or (arxana-docbook--latest-non-lab entries)
                      (car entries)
                      (list :book book :doc-id next-id))))
      (arxana-docbook--render-entry entry))))
(defun arxana-docbook-prev-entry ()
  "Open the previous docbook entry in TOC order."
  (interactive)
  (let* ((book arxana-docbook--entry-book)
         (prev-id (arxana-docbook--entry-prev-doc-id)))
    (unless (and book prev-id)
      (user-error "No previous docbook entry"))
    (let* ((entries (arxana-docbook--entries-for-doc book prev-id))
           (entry (or (arxana-docbook--latest-non-lab entries)
                      (car entries)
                      (list :book book :doc-id prev-id))))
      (arxana-docbook--render-entry entry))))
(defun arxana-docbook-open-uri (uri)
  "Open a docbook URI like docbook://BOOK/DOC-ID[/ENTRY-ID]."
  (interactive "sDocbook URI: ")
  (let* ((clean (string-trim uri))
         (is-docbook (string-prefix-p "docbook://" clean))
         (is-view (string-prefix-p "arxana://view/" clean)))
    (when is-view
      (let* ((view-name (string-remove-prefix "arxana://view/" clean))
             (view (intern view-name))
             (book (when (string-prefix-p "docbook" view-name) "futon4")))
        (unless (require 'arxana-browser nil t)
          (user-error "Arxana browser is unavailable; load arxana-browser"))
        (arxana-browser-browse)
        (with-current-buffer (get-buffer arxana-browser--buffer)
          (setq arxana-browser--stack (list (list :view view :book book :label view-name))
                arxana-browser--context (car arxana-browser--stack))
          (arxana-browser--render))
        (when (fboundp 'arxana-ui-refresh)
          (arxana-ui-refresh))
        (cl-return-from arxana-docbook-open-uri nil)))
    (unless is-docbook
      (user-error "Invalid docbook URI: %s" uri))
    (let* ((parts (split-string (string-remove-prefix "docbook://" clean) "/" t))
           (book (nth 0 parts))
           (doc-id (nth 1 parts))
           (entry-id (nth 2 parts)))
      (unless (and book doc-id)
        (user-error "Invalid docbook URI: %s" uri))
    (let* ((root (arxana-docbook--locate-books-root))
           (stub-path (and root doc-id
                           (expand-file-name (format "%s.org" doc-id)
                                             (expand-file-name book root))))
           (entries (arxana-docbook--entries-for-doc book doc-id))
           (local-stubs (seq-filter (lambda (e) (plist-get e :stub-path)) entries))
           (pick-from (lambda (pool)
                        (or (and entry-id
                                 (seq-find (lambda (e)
                                             (equal entry-id (plist-get e :entry-id)))
                                           pool))
                            (arxana-docbook--latest-non-lab pool)
                            (car pool))))
           (entry (or (funcall pick-from local-stubs)
                      (funcall pick-from entries)
                      (list :book book :doc-id doc-id))))
        (if arxana-docbook-open-uri-prefer-render
            (arxana-docbook--render-entry entry)
          (if (and stub-path (file-readable-p stub-path))
              (arxana-docbook-open-stub (plist-put (copy-sequence entry) :stub-path stub-path))
            (if (and (plist-get entry :stub-path)
                     (file-readable-p (plist-get entry :stub-path)))
                (arxana-docbook-open-stub entry)
              (arxana-docbook--render-entry entry))))))))
(defun arxana-docbook--view-raw (_entry)
  (user-error "Raw JSON cache is disabled; open the stub instead"))
(defun arxana-docbook-open-entry ()
  "Open the doc book stub/summary at point in a read-only view buffer."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry
      (user-error "No entry at point"))
    (if (and (plist-get entry :stub-path)
             (file-readable-p (plist-get entry :stub-path)))
        (arxana-docbook-open-stub entry)
      (arxana-docbook--render-entry entry (current-buffer) (plist-get entry :entry-id)))))
(defun arxana-docbook-open-raw ()
  "Open the raw JSON log for the entry at point."
  (interactive)
  (let ((entry (tabulated-list-get-id)))
    (unless entry
      (user-error "No entry at point"))
    (arxana-docbook--view-raw entry)))
(defun arxana-docbook-open-entry-object (entry)
  "Open ENTRY (plist) in a read-only buffer."
  (if (and (plist-get entry :stub-path)
           (file-readable-p (plist-get entry :stub-path)))
      (arxana-docbook-open-stub entry)
    (arxana-docbook--render-entry entry (current-buffer) (plist-get entry :entry-id))))
(defun arxana-docbook-open-entry-raw (entry)
  "Open the raw JSON backing ENTRY (plist)."
  (arxana-docbook--view-raw entry))

(defun arxana-docbook--merge-scholia (primary fallback)
  "Merge scholia lists, deduping by :xt/id when available."
  (let ((seen (make-hash-table :test 'equal))
        (out nil))
    (dolist (scholium (append primary fallback))
      (let ((sch-id (or (plist-get scholium :xt/id) (plist-get scholium :name))))
        (unless (gethash sch-id seen)
          (puthash sch-id t seen)
          (push scholium out))))
    (nreverse out)))

;;;###autoload
(defun arxana-docbook-show-scholia ()
  "Display scholia for the current docbook entry."
  (interactive)
  (let ((load-prefer-newer t))
    (require 'arxana-scholium)
    (require 'arxana-links))
  (unless (fboundp 'arxana-scholium-show-for-doc)
    (user-error "arxana-scholium-show-for-doc not loaded; re-run M-x arxana-load"))
  (let ((book arxana-docbook--entry-book)
        (doc-id arxana-docbook--entry-doc-id))
    (unless (and book doc-id)
      (user-error "No docbook entry active"))
    (let* ((target-doc (format "docbook://%s/%s" book doc-id))
           (buffer-doc (buffer-name (current-buffer)))
           (primary (arxana-links-load-scholia-for-doc target-doc))
           (fallback (unless (equal buffer-doc target-doc)
                       (arxana-links-load-scholia-for-doc buffer-doc)))
           (scholia (arxana-docbook--merge-scholia primary fallback)))
      (arxana-scholium-show-for-doc target-doc (current-buffer) scholia))))

(defun arxana-docbook-copy-location ()
  "Copy a location identifier for the current docbook entry."
  (interactive)
  (let* ((book arxana-docbook--entry-book)
         (doc-id arxana-docbook--entry-doc-id)
         (entry-id arxana-docbook--entry-entry-id)
         (location (cond
                    ((and book doc-id entry-id)
                     (format "docbook://%s/%s/%s" book doc-id entry-id))
                    ((and book doc-id)
                     (format "docbook://%s/%s" book doc-id))
                    (t "docbook://unknown"))))
    (kill-new location)
    (message "Copied %s" location)))

(defun arxana-docbook-left-or-return ()
  "Move left or return to the docbook browser when at the buffer start."
  (interactive)
  (if (<= (point) (point-min))
      (arxana-docbook-return-to-browser)
    (backward-char 1)))

(defun arxana-docbook--capture-line-context ()
  (let ((line (string-trim (buffer-substring-no-properties
                            (line-beginning-position)
                            (line-end-position)))))
    (when (and line (> (length line) 6))
      line)))

(defun arxana-docbook-open-stub-at-point ()
  "Open the stub for the current entry and try to preserve point."
  (interactive)
  (let ((context (arxana-docbook--capture-line-context)))
    (arxana-docbook-open-stub arxana-docbook--entry-current)
    (when context
      (goto-char (point-min))
      (when (search-forward context nil t)
        (goto-char (match-beginning 0))
        (beginning-of-line)))))

(defvar arxana-docbook-entry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'arxana-docbook-next-entry)
    (define-key map (kbd "M-p") #'arxana-docbook-prev-entry)
    (define-key map (kbd "M-y") #'arxana-docbook-copy-location)
    (define-key map (kbd "<left>") #'arxana-docbook-left-or-return)
    (define-key map (kbd "C-c C-s") #'arxana-docbook-open-stub-at-point)
    (define-key map (kbd "e") #'arxana-docbook-open-stub-at-point)
    map)
  "Keymap for `arxana-docbook-entry-mode'.")

(define-minor-mode arxana-docbook-entry-mode
  "Minor mode for docbook entry buffers."
  :keymap arxana-docbook-entry-mode-map
  (if arxana-docbook-entry-mode
      (progn
        (setq-local header-line-format '(:eval (arxana-docbook--entry-header-line)))
        (let ((maps (copy-sequence minor-mode-overriding-map-alist)))
          (setf (alist-get 'arxana-docbook-entry-mode maps)
                arxana-docbook-entry-mode-map)
          (setq-local minor-mode-overriding-map-alist maps))
        (add-hook 'after-change-functions #'arxana-docbook--entry-update-header nil t)
        (add-hook 'after-save-hook #'arxana-docbook--entry-update-header nil t)
        (add-hook 'after-save-hook #'arxana-docbook--refresh-linked-code-docs nil t)
        (arxana-docbook--entry-update-header))
    (remove-hook 'after-change-functions #'arxana-docbook--entry-update-header t)
    (remove-hook 'after-save-hook #'arxana-docbook--entry-update-header t)
    (remove-hook 'after-save-hook #'arxana-docbook--refresh-linked-code-docs t)
    (when (assoc 'arxana-docbook-entry-mode minor-mode-overriding-map-alist)
      (setq-local minor-mode-overriding-map-alist
                  (assq-delete-all 'arxana-docbook-entry-mode
                                   minor-mode-overriding-map-alist)))
    (kill-local-variable 'header-line-format)))

(define-derived-mode arxana-docbook-mode tabulated-list-mode "ArxDocBook"
  "Browser for filesystem-backed doc book entries."
  (setq tabulated-list-format [("Doc" 26 t)
                               ("Version" 18 t)
                               ("When" 20 t)
                               ("Files" 8 t)
                               ("Summary" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq header-line-format '(:eval (arxana-docbook--header-line)))
  (tabulated-list-init-header)
  (when (fboundp 'arxana-ui-mark-managed)
    (arxana-ui-mark-managed "Arxana Docbook")))

(defvar arxana-docbook-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'arxana-docbook-open-entry)
    (define-key map (kbd "o") #'arxana-docbook-open-entry)
    map)
  "Keymap for `arxana-docbook-mode'.")
(defun arxana-docbook-browse (&optional book)
  "Browse doc book entries for BOOK (default: futon4)."
  (interactive)
  (let* ((books (or (arxana-docbook--available-books) '("futon4")))
         (book (or book
                   (and (cdr books)
                        (completing-read "Doc book: " books nil t nil nil (car books)))
                   (car books))))
    (unless book
      (user-error "No doc books found (set arxana-docbook-books-root if needed)"))
    (let ((buf (get-buffer-create (format "*DocBook:%s*" book))))
      (with-current-buffer buf
        (arxana-docbook-mode)
        (setq arxana-docbook--book book)
        (arxana-docbook--refresh))
      (pop-to-buffer buf))))
(defun arxana-docbook-return-to-browser ()
  "Return to the docbook browser and highlight the current entry."
  (interactive)
  (let* ((book arxana-docbook--entry-book)
         (doc-id arxana-docbook--return-doc-id)
         (entry-id arxana-docbook--return-entry-id)
         (return-buffer arxana-docbook--return-buffer))
    (unless (and book doc-id)
      (user-error "No current docbook entry to return from"))
    (cond
     ((and (buffer-live-p return-buffer)
           (with-current-buffer return-buffer
             (derived-mode-p 'arxana-browser-mode)))
      (with-current-buffer return-buffer
        (when (fboundp 'arxana-browser--render)
          (arxana-browser--render))
        (when (fboundp 'arxana-browser--goto-doc-id)
          (arxana-browser--goto-doc-id doc-id))))
     (t
      (let ((buf (get-buffer-create (format "*DocBook:%s*" book))))
        (with-current-buffer buf
          (arxana-docbook-mode)
          (setq arxana-docbook--book book)
          (arxana-docbook--refresh)
          (arxana-docbook--goto-entry doc-id entry-id))
        (setq return-buffer buf))))
    (when (buffer-live-p return-buffer)
      (pop-to-buffer return-buffer))))
(defun arxana-docbook-browse-futon4 ()
  "Shortcut to browse the futon4 doc book."
  (interactive)
  (arxana-docbook-browse "futon4"))

(provide 'arxana-docbook-ui)
;;; arxana-docbook-ui.el ends here
