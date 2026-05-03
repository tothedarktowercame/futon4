;;; arxana-browser-essays-wikibooks.el --- Import Wikibooks pages as essays -*- lexical-binding: t; -*-

;;; Commentary:
;; Imports Wikibooks pages into the Essays subsystem as local markdown-ish
;; source files plus generated manifest files.  This gives Arxana a browseable
;; local surface for wiki-backed books without making MediaWiki the canonical
;; editor.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-util)

(require 'arxana-browser-essays)

(defgroup arxana-browser-essays-wikibooks nil
  "Wikibooks import support for Arxana Essays."
  :group 'arxana-browser-essays)

(defconst arxana-browser-essays-wikibooks--file
  (or load-file-name (buffer-file-name))
  "Absolute path to this module when loaded from disk.")

(defconst arxana-browser-essays-wikibooks--repo-root
  (when arxana-browser-essays-wikibooks--file
    (expand-file-name ".."
                      (file-name-directory
                       arxana-browser-essays-wikibooks--file)))
  "Repository root inferred from the module location.")

(defcustom arxana-browser-essays-wikibooks-site-base-url
  "https://en.wikibooks.org"
  "Base URL for the Wikibooks instance to import from."
  :type 'string
  :group 'arxana-browser-essays-wikibooks)

(defcustom arxana-browser-essays-wikibooks-data-directory
  (expand-file-name "data/essays/wikibooks"
                    (or arxana-browser-essays-wikibooks--repo-root
                        default-directory))
  "Directory holding imported Wikibooks essay sources and manifests."
  :type 'directory
  :group 'arxana-browser-essays-wikibooks)

(defcustom arxana-browser-essays-wikibooks-request-timeout 30
  "Timeout in seconds for Wikibooks fetches."
  :type 'integer
  :group 'arxana-browser-essays-wikibooks)

(defcustom arxana-browser-essays-wikibooks-auto-sync t
  "When non-nil, discover generated Wikibooks manifests on refresh/load."
  :type 'boolean
  :group 'arxana-browser-essays-wikibooks)

(defconst arxana-browser-essays-wikibooks-peeragogy-root-title
  "Peeragogy Handbook"
  "Root page title for the Peeragogy Handbook import.")

(defun arxana-browser-essays-wikibooks--slugify (text)
  "Return a filesystem-safe slug for TEXT."
  (let ((slug (downcase (or text ""))))
    (setq slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
    (string-trim slug "-+" "-+")))

(defun arxana-browser-essays-wikibooks--book-directory (root-title)
  "Return the local directory holding imported files for ROOT-TITLE."
  (expand-file-name (arxana-browser-essays-wikibooks--slugify root-title)
                    arxana-browser-essays-wikibooks-data-directory))

(defun arxana-browser-essays-wikibooks--page-basename (page-title)
  "Return a stable local basename for PAGE-TITLE."
  (let ((page (replace-regexp-in-string "/" "--" page-title)))
    (arxana-browser-essays-wikibooks--slugify page)))

(defun arxana-browser-essays-wikibooks--page-raw-url (page-title)
  "Return the MediaWiki raw URL for PAGE-TITLE."
  (format "%s/w/index.php?title=%s&action=raw"
          (string-remove-suffix "/"
                                arxana-browser-essays-wikibooks-site-base-url)
          (url-hexify-string page-title)))

(defun arxana-browser-essays-wikibooks--page-web-url (page-title)
  "Return the public page URL for PAGE-TITLE."
  (format "%s/wiki/%s"
          (string-remove-suffix "/"
                                arxana-browser-essays-wikibooks-site-base-url)
          (url-hexify-string page-title)))

(defun arxana-browser-essays-wikibooks--fetch-raw (page-title)
  "Fetch raw wikitext for PAGE-TITLE."
  (let ((buf (url-retrieve-synchronously
              (arxana-browser-essays-wikibooks--page-raw-url page-title)
              t t
              arxana-browser-essays-wikibooks-request-timeout)))
    (unless (buffer-live-p buf)
      (error "[wikibooks] Failed to fetch %s" page-title))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (unless (search-forward "\n\n" nil t)
            (error "[wikibooks] Malformed HTTP response for %s" page-title))
          (buffer-substring-no-properties (point) (point-max)))
      (kill-buffer buf))))

(defun arxana-browser-essays-wikibooks--extract-book-pages (root-title raw-root)
  "Return ordered unique subpage titles linked from RAW-ROOT for ROOT-TITLE."
  (let ((prefixes (list (concat root-title "/")
                        (concat root-title " V1.0/")))
        (seen (make-hash-table :test 'equal))
        pages)
    (with-temp-buffer
      (insert raw-root)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^]|]+\\)\\(?:|[^]]*\\)?\\]\\]" nil t)
        (let ((target (string-trim (match-string 1))))
          (when (and (seq-some (lambda (prefix)
                                 (string-prefix-p prefix target))
                               prefixes)
                     (not (gethash target seen)))
            (puthash target t seen)
            (push target pages)))))
    (nreverse pages)))

(defun arxana-browser-essays-wikibooks--last-pipe-field (text)
  "Return the final `|' field in TEXT, trimmed."
  (let* ((parts (split-string text "|" t "[[:space:]\n]+"))
         (last (car (last parts))))
    (string-trim (or last ""))))

(defun arxana-browser-essays-wikibooks--strip-simple-templates (text)
  "Remove simple one-line templates from TEXT."
  (let ((out (or text "")))
    (while (string-match "{{[^{}\n]*}}" out)
      (setq out (replace-match "" t t out)))
    out))

(defun arxana-browser-essays-wikibooks--strip-ref-blocks (text)
  "Remove `<ref ...>...</ref>' blocks from TEXT."
  (with-temp-buffer
    (insert (or text ""))
    (goto-char (point-min))
    (while (search-forward "<ref" nil t)
      (let ((start (match-beginning 0)))
        (if (search-forward "</ref>" nil t)
            (delete-region start (point))
          (goto-char start)
          (when (search-forward ">" nil t)
            (delete-region start (point))))))
    (buffer-string)))

(defun arxana-browser-essays-wikibooks--decode-html-entities (text)
  "Decode common HTML entities in TEXT."
  (let ((out (or text "")))
    (setq out (replace-regexp-in-string "&quot;" "\"" out t t))
    (setq out (replace-regexp-in-string "&apos;" "'" out t t))
    (setq out (replace-regexp-in-string "&#0*39;" "'" out t t))
    (setq out (replace-regexp-in-string "&amp;" "&" out t t))
    (setq out (replace-regexp-in-string "&nbsp;" " " out t t))
    (setq out (replace-regexp-in-string "&lt;" "<" out t t))
    (setq out (replace-regexp-in-string "&gt;" ">" out t t))
    (with-temp-buffer
      (insert out)
      (goto-char (point-min))
      (while (re-search-forward "&#\\([0-9]+\\);" nil t)
        (replace-match
         (string (string-to-number (match-string 1)))
         t t))
      (goto-char (point-min))
      (while (re-search-forward "&#x\\([0-9A-Fa-f]+\\);" nil t)
        (replace-match
         (string (string-to-number (match-string 1) 16))
         t t))
      (buffer-string))))

(defun arxana-browser-essays-wikibooks--wikitext-to-markdown (page-title raw)
  "Convert RAW wikitext for PAGE-TITLE into markdown-ish text."
  (let ((text (or raw "")))
    (setq text (replace-regexp-in-string "\r\n?" "\n" text))
    (setq text (arxana-browser-essays-wikibooks--strip-ref-blocks text))
    (setq text (arxana-browser-essays-wikibooks--decode-html-entities text))
    (setq text (replace-regexp-in-string "<blockquote>" "\n> " text t t))
    (setq text (replace-regexp-in-string "</blockquote>" "\n" text t t))
    (setq text
          (replace-regexp-in-string
           "\\[\\[File:\\([^]]+\\)\\]\\]"
           (lambda (_match)
             (let ((caption
                    (arxana-browser-essays-wikibooks--last-pipe-field
                     (match-string 1 text))))
               (if (string-empty-p caption)
                   "> [Media]\n"
                 (format "> [Media] %s\n" caption))))
           text t t))
    (setq text (replace-regexp-in-string "<ref[^>]*/>" "" text))
    (setq text (replace-regexp-in-string "{{ISBN|\\([^}]+\\)}}" "ISBN \\1" text))
    (setq text (arxana-browser-essays-wikibooks--strip-simple-templates text))
    (setq text
          (replace-regexp-in-string
           "\\[\\(https?://[^] \n]+\\)[[:space:]]+\\([^]]+\\)\\]"
           "[\\2](\\1)" text t))
    (setq text
          (replace-regexp-in-string
           "\\[\\(https?://[^] \n]+\\)\\]"
           "<\\1>" text t))
    (setq text
          (replace-regexp-in-string
           "\\[\\[\\([^]|]+\\)|\\([^]]+\\)\\]\\]"
           "\\2" text t))
    (setq text
          (replace-regexp-in-string
           "\\[\\[\\([^]]+\\)\\]\\]"
           "\\1" text t))
    (setq text
          (replace-regexp-in-string
           "^=+\\s-*\\(.+?\\)\\s-*=+$"
           "## \\1" text t))
    (setq text
          (replace-regexp-in-string
           "^:#\\s-*\\([^#].*\\)$"
           "1. \\1" text t))
    (setq text
          (replace-regexp-in-string
           "^#\\s-*\\([^#].*\\)$"
           "1. \\1" text t))
    (setq text (replace-regexp-in-string "^:\\*\\s-*" "- " text t))
    (setq text (replace-regexp-in-string "^:\\s-*" "" text t))
    (setq text
          (replace-regexp-in-string
           "^__[A-Z][A-Z0-9_]*__$" "" text t))
    (setq text (replace-regexp-in-string "'''''\\(.+?\\)'''''" "***\\1***" text t))
    (setq text (replace-regexp-in-string "'''\\(.+?\\)'''" "**\\1**" text t))
    (setq text (replace-regexp-in-string "''\\(.+?\\)''" "*\\1*" text t))
    (setq text (replace-regexp-in-string "<[^>\n]+>" "" text t))
    (setq text
          (replace-regexp-in-string
           "^\\[\\[[[:alpha:]-]+:[^]]+\\]\\]$" "" text t))
    (setq text
          (replace-regexp-in-string
           "^Category:[^\n]*$" "" text t))
    (setq text (replace-regexp-in-string "\n\\{3,\\}" "\n\n" text))
    (setq text (string-trim text))
    (let ((body text))
      (cond
       ((string-match-p "^## " body)
        (let ((first (string-match "^## " body)))
          (if (and first (> first 0)
                   (not (string-empty-p
                         (string-trim (substring body 0 first)))))
              (setq body (concat "## Overview\n\n"
                                 (string-trim (substring body 0 first))
                                 "\n\n"
                                 (string-trim (substring body first))))
            (setq body (string-trim body)))))
       (t
        (setq body (concat "## Overview\n\n" body))))
      (concat "# " page-title "\n\n" body "\n"))))

(defun arxana-browser-essays-wikibooks--markdown-headings (markdown)
  "Return the ordered list of `##' headings found in MARKDOWN."
  (let (headings)
    (with-temp-buffer
      (insert (or markdown ""))
      (goto-char (point-min))
      (while (re-search-forward "^##[[:space:]]+\\(.+\\)$" nil t)
        (push (string-trim (match-string 1)) headings)))
    (nreverse headings)))

(defun arxana-browser-essays-wikibooks--manifest-symbol (page-title)
  "Return the generated manifest symbol for PAGE-TITLE."
  (intern
   (format "arxana-browser-essays-wikibooks-%s-manifest"
           (arxana-browser-essays-wikibooks--page-basename page-title))))

(defun arxana-browser-essays-wikibooks--essay-id (page-title)
  "Return the essay id for PAGE-TITLE."
  (format "arxana/essay/wikibooks/%s"
          (arxana-browser-essays-wikibooks--page-basename page-title)))

(defun arxana-browser-essays-wikibooks--book-essay-id (root-title)
  "Return the book-level essay id for ROOT-TITLE."
  (format "arxana/essay/wikibooks/book/%s"
          (arxana-browser-essays-wikibooks--slugify root-title)))

(defun arxana-browser-essays-wikibooks--section-id (essay-id index heading)
  "Return a stable section id for ESSAY-ID, INDEX, and HEADING."
  (format "%s/section/%d-%s"
          essay-id
          index
          (arxana-browser-essays-wikibooks--slugify heading)))

(defun arxana-browser-essays-wikibooks--chapter-label (root-title page-title)
  "Return a succinct chapter label for PAGE-TITLE under ROOT-TITLE."
  (or (and (string-prefix-p (concat root-title "/") page-title)
           (string-remove-prefix (concat root-title "/") page-title))
      (and (string-prefix-p (concat root-title " V1.0/") page-title)
           (string-remove-prefix (concat root-title " V1.0/") page-title))
      page-title))

(defun arxana-browser-essays-wikibooks--build-manifest (page-title markdown source-file)
  "Build an essays manifest for PAGE-TITLE from MARKDOWN and SOURCE-FILE."
  (let* ((essay-id (arxana-browser-essays-wikibooks--essay-id page-title))
         (headings (arxana-browser-essays-wikibooks--markdown-headings markdown))
         (url (arxana-browser-essays-wikibooks--page-web-url page-title))
         (sections
          (cl-loop for heading in headings
                   for index from 0
                   collect
                   (list :id (arxana-browser-essays-wikibooks--section-id
                              essay-id index heading)
                         :name heading
                         :type "arxana/essay-section"
                         :props `((index . ,index)
                                  (heading-level . 2)
                                  (heading-text . ,heading))))))
    `(:version 1
      :essay (:id ,essay-id
              :name ,page-title
              :type "arxana/essay"
              :source-file ,source-file
              :props ((source . "wikibooks")
                      (wikibooks-title . ,page-title)
                      (wikibooks-url . ,url)
                      (imported . ,(format-time-string "%FT%TZ" (current-time)))))
      :sections ,sections
      :annotations ())))

(defun arxana-browser-essays-wikibooks--write-manifest-file (path symbol manifest)
  "Write MANIFEST under SYMBOL to PATH."
  (make-directory (file-name-directory path) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file path
      (insert (format ";;; %s --- Generated Wikibooks essay manifest -*- lexical-binding: t; -*-\n\n"
                      (file-name-nondirectory path)))
      (insert ";;; Code:\n\n")
      (let ((print-level nil)
            (print-length nil))
        (pp `(defconst ,symbol ',manifest) (current-buffer)))
      (insert "\n(provide '"
              (symbol-name symbol)
              ")\n"))))

(defun arxana-browser-essays-wikibooks--manifest-file (page-title root-title)
  "Return the manifest file path for PAGE-TITLE under ROOT-TITLE."
  (expand-file-name
   (format "%s-manifest.el"
           (arxana-browser-essays-wikibooks--page-basename page-title))
   (arxana-browser-essays-wikibooks--book-directory root-title)))

(defun arxana-browser-essays-wikibooks--book-manifest-file (root-title)
  "Return the manifest file path for the imported book ROOT-TITLE."
  (expand-file-name
   (format "%s-book-manifest.el"
           (arxana-browser-essays-wikibooks--slugify root-title))
   (arxana-browser-essays-wikibooks--book-directory root-title)))

(defun arxana-browser-essays-wikibooks--annotations-file-for-manifest-file (path)
  "Return the sidecar annotations file path corresponding to manifest PATH."
  (when (and path (string-suffix-p "-manifest.el" path))
    (concat (string-remove-suffix "-manifest.el" path)
            "-annotations.el")))

(defun arxana-browser-essays-wikibooks--source-file (page-title root-title)
  "Return the wikitext source path for PAGE-TITLE under ROOT-TITLE.
The canonical source is now MediaWiki wikitext, fetched directly from
Wikibooks and stored under the book directory's `wikitext/' subdir.
Markdown imports (former canonical) live in `import-archive/'."
  (expand-file-name
   (format "wikitext/%s.mw"
           (arxana-browser-essays-wikibooks--page-basename page-title))
   (arxana-browser-essays-wikibooks--book-directory root-title)))

(defun arxana-browser-essays-wikibooks--book-manifest-symbol (root-title)
  "Return the manifest symbol for the imported book ROOT-TITLE."
  (intern
   (format "arxana-browser-essays-wikibooks-%s-book-manifest"
           (arxana-browser-essays-wikibooks--slugify root-title))))

(defun arxana-browser-essays-wikibooks--expected-import-paths (root-title pages)
  "Return the set of generated file paths expected for ROOT-TITLE and PAGES."
  (append
   (mapcan
    (lambda (page-title)
      (list (arxana-browser-essays-wikibooks--source-file page-title root-title)
            (arxana-browser-essays-wikibooks--manifest-file page-title root-title)))
    pages)
   (list (arxana-browser-essays-wikibooks--book-manifest-file root-title))))

(defun arxana-browser-essays-wikibooks--prune-stale-import-files (root-title pages)
  "Delete generated files under ROOT-TITLE that are not expected for PAGES.
Considers only top-level files; the canonical wikitext under
`wikitext/' and archived imports under `import-archive/' are out of
scope (left to the wikitext-corpus refresh script and the operator)."
  (let* ((dir (arxana-browser-essays-wikibooks--book-directory root-title))
         (expected (mapcar #'expand-file-name
                           (arxana-browser-essays-wikibooks--expected-import-paths
                            root-title pages))))
    (when (file-directory-p dir)
      (dolist (path (directory-files dir t "\\`[^.]"))
        (when (and (file-regular-p path)
                   (or (string-suffix-p ".mw" path)
                       (string-suffix-p "-manifest.el" path))
                   (not (member (expand-file-name path) expected)))
          (delete-file path))))))

(defun arxana-browser-essays-wikibooks--first-manifest-symbol (path)
  "Return the first manifest symbol defined in PATH."
  (let (symbol)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (condition-case nil
          (while (and (not symbol) (not (eobp)))
            (let ((form (read (current-buffer))))
              (when (and (listp form)
                         (memq (car form) '(defconst defvar defcustom))
                         (symbolp (cadr form)))
                (setq symbol (cadr form)))))
        (end-of-file nil)))
    symbol))

(defun arxana-browser-essays-wikibooks--first-defconst-value (path)
  "Load PATH and return the first top-level variable value it defines."
  (let ((symbol (arxana-browser-essays-wikibooks--first-manifest-symbol path)))
    (when symbol
      (load path nil t)
      (and (boundp symbol) (symbol-value symbol)))))

(defun arxana-browser-essays-wikibooks--manifest-files ()
  "Return discovered generated Wikibooks manifest files."
  (if (file-directory-p arxana-browser-essays-wikibooks-data-directory)
      (sort
       (directory-files-recursively
        arxana-browser-essays-wikibooks-data-directory
        "-manifest\\.el\\'")
       #'string<)
    nil))

(defun arxana-browser-essays-wikibooks--merge-annotations (base extras)
  "Return BASE annotations merged with EXTRAS, deduplicated by `:id'."
  (let ((seen (make-hash-table :test 'equal))
        merged)
    (dolist (ann (append base extras))
      (let ((ann-id (plist-get ann :id)))
        (unless (or (null ann-id) (gethash ann-id seen))
          (puthash ann-id t seen)
          (push ann merged))))
    (nreverse merged)))

(defun arxana-browser-essays-wikibooks--merge-annotation-sidecar (path symbol manifest)
  "Merge sidecar annotations for manifest at PATH into SYMBOL/MANIFEST."
  (let* ((annotations-path
          (arxana-browser-essays-wikibooks--annotations-file-for-manifest-file
           path))
         (sidecar (and annotations-path
                       (file-readable-p annotations-path)
                       (arxana-browser-essays-wikibooks--first-defconst-value
                        annotations-path))))
    (if (not sidecar)
        manifest
      (let* ((existing (plist-get manifest :annotations))
             (extra (cond
                     ((and (listp sidecar) (plist-get sidecar :annotations))
                      (plist-get sidecar :annotations))
                     ((listp sidecar) sidecar)
                     (t nil)))
             (merged (plist-put (copy-tree manifest)
                                :annotations
                                (arxana-browser-essays-wikibooks--merge-annotations
                                 existing extra))))
        (when (and symbol (boundp symbol))
          (set symbol merged))
        merged))))

(defun arxana-browser-essays-wikibooks--catalog-from-manifest-file (path)
  "Return a catalog plist derived from the manifest file at PATH."
  (let ((symbol (arxana-browser-essays-wikibooks--first-manifest-symbol path)))
    (when symbol
      (load path nil t)
      (let* ((manifest (and (boundp symbol) (symbol-value symbol)))
             (manifest (and manifest
                            (arxana-browser-essays-wikibooks--merge-annotation-sidecar
                             path symbol manifest)))
             (essay (and (listp manifest) (plist-get manifest :essay)))
             (essay-id (and essay (plist-get essay :id)))
             (name (and essay (plist-get essay :name)))
             (source-rel (and essay (plist-get essay :source-file)))
             (essay-props (and essay (plist-get essay :props)))
             (source (and source-rel
                          (expand-file-name source-rel
                                            (file-name-directory path)))))
        (when (and essay-id
                   (eq (alist-get 'wikibooks-catalog essay-props) t))
          (list :id (intern (arxana-browser-essays-wikibooks--slugify essay-id))
                :label name
                :description "Imported from Wikibooks."
                :essay-id essay-id
                :manifest-symbol symbol
                :source-file source))))))

(defun arxana-browser-essays-wikibooks--merge-catalogs (base extras)
  "Return BASE merged with EXTRAS, deduplicated by `:essay-id'."
  (let ((seen (make-hash-table :test 'equal))
        merged)
    (dolist (cat (append base extras))
      (let ((essay-id (plist-get cat :essay-id)))
        (unless (or (null essay-id) (gethash essay-id seen))
          (puthash essay-id t seen)
          (push cat merged))))
    (nreverse merged)))

(defun arxana-browser-essays-wikibooks--catalog-owned-p (cat)
  "Return non-nil when CAT is owned by the Wikibooks importer."
  (let ((essay-id (plist-get cat :essay-id)))
    (and (stringp essay-id)
         (string-prefix-p "arxana/essay/wikibooks/" essay-id))))

(defun arxana-browser-essays-wikibooks--managed-manifest-file-p (path)
  "Return non-nil when PATH lives under the Wikibooks data directory."
  (let ((abs-path (and path (expand-file-name path)))
        (abs-root (and arxana-browser-essays-wikibooks-data-directory
                       (expand-file-name
                        arxana-browser-essays-wikibooks-data-directory))))
    (and (stringp abs-path)
         (stringp abs-root)
         (string-prefix-p
          (file-name-as-directory abs-root)
          abs-path))))

(defun arxana-browser-essays-wikibooks-sync-registrations ()
  "Discover generated Wikibooks essays and register them with the browser."
  (interactive)
  (when arxana-browser-essays-wikibooks-auto-sync
    (let* ((files (arxana-browser-essays-wikibooks--manifest-files))
           (catalogs (delq nil
                           (mapcar
                            #'arxana-browser-essays-wikibooks--catalog-from-manifest-file
                            files))))
      (setq arxana-browser-essays-manifest-files
            (delete-dups
             (append
              (seq-remove
               #'arxana-browser-essays-wikibooks--managed-manifest-file-p
               arxana-browser-essays-manifest-files)
              files)))
      (setq arxana-browser-essays-catalogs
            (arxana-browser-essays-wikibooks--merge-catalogs
             (seq-remove
              #'arxana-browser-essays-wikibooks--catalog-owned-p
              arxana-browser-essays-catalogs)
             catalogs))))
  (when (called-interactively-p 'interactive)
    (message "[wikibooks] Registered %d manifest(s)"
             (length (arxana-browser-essays-wikibooks--manifest-files)))))

(defun arxana-browser-essays-wikibooks--refresh-advice (&rest _)
  "Advice: resync discovered manifest registrations after essay refresh."
  (arxana-browser-essays-wikibooks-sync-registrations))

(defun arxana-browser-essays-wikibooks--import-page (root-title page-title)
  "Import PAGE-TITLE from Wikibooks under the book ROOT-TITLE.
Returns plist with `:page', `:source-file', and `:manifest-file'."
  (let* ((raw (arxana-browser-essays-wikibooks--fetch-raw page-title))
         (markdown (arxana-browser-essays-wikibooks--wikitext-to-markdown
                    page-title raw))
         (source-path (arxana-browser-essays-wikibooks--source-file
                       page-title root-title))
         (manifest-path (arxana-browser-essays-wikibooks--manifest-file
                         page-title root-title))
         (manifest-rel (file-name-nondirectory source-path))
         (manifest (arxana-browser-essays-wikibooks--build-manifest
                    page-title markdown manifest-rel))
         (symbol (arxana-browser-essays-wikibooks--manifest-symbol page-title)))
    (make-directory (file-name-directory source-path) t)
    (let ((coding-system-for-write 'utf-8-unix))
      (with-temp-file source-path
        (insert markdown)))
    (arxana-browser-essays-wikibooks--write-manifest-file
     manifest-path symbol manifest)
    (list :page page-title
          :source-file source-path
          :manifest-file manifest-path)))

(defun arxana-browser-essays-wikibooks--build-book-manifest (root-title imported-pages)
  "Build a book-level essays manifest for ROOT-TITLE from IMPORTED-PAGES."
  (let* ((essay-id (arxana-browser-essays-wikibooks--book-essay-id root-title))
         (root-entry (seq-find
                      (lambda (entry)
                        (equal (plist-get entry :page) root-title))
                      imported-pages))
         (root-source (and root-entry
                           (file-name-nondirectory
                            (plist-get root-entry :source-file))))
         (chapters (seq-remove
                    (lambda (entry)
                      (equal (plist-get entry :page) root-title))
                    imported-pages))
         (sections
          (cl-loop for entry in chapters
                   for index from 0
                   for page-title = (plist-get entry :page)
                   for label = (arxana-browser-essays-wikibooks--chapter-label
                                root-title page-title)
                   for source-file = (plist-get entry :source-file)
                   collect
                   (list :id (arxana-browser-essays-wikibooks--section-id
                              essay-id index label)
                         :name label
                         :type "arxana/essay-section"
                         :props `((index . ,index)
                                  (heading-level . 1)
                                  (heading-text . ,label)
                                  (page-title . ,page-title)
                                  (source-file . ,source-file)
                                  (render-whole-file . t))))))
    `(:version 1
      :essay (:id ,essay-id
              :name ,root-title
              :type "arxana/essay"
              :source-file ,root-source
              :props ((source . "wikibooks")
                      (wikibooks-root-title . ,root-title)
                      (wikibooks-url . ,(arxana-browser-essays-wikibooks--page-web-url
                                         root-title))
                      (wikibooks-catalog . t)
                      (imported . ,(format-time-string "%FT%TZ" (current-time)))))
      :sections ,sections
      :annotations ())))

(defun arxana-browser-essays-wikibooks--whole-file-text (source-file)
  "Return SOURCE-FILE content prepared for full-file essay rendering."
  (unless (and source-file (file-readable-p source-file))
    (error "[wikibooks] Source file not readable: %s" source-file))
  (with-temp-buffer
    (insert-file-contents source-file)
    (goto-char (point-min))
    (when (looking-at "^# .+\n+")
      (replace-match ""))
    (let ((text (string-trim (buffer-string))))
      (if (string-match-p "^## " text)
          (concat text "\n")
        (concat "## Overview\n\n" text "\n")))))

(defun arxana-browser-essays-wikibooks--section-entry (manifest section-id)
  "Return the section plist for SECTION-ID in MANIFEST."
  (seq-find
   (lambda (section)
     (string= (plist-get section :id) section-id))
   (plist-get manifest :sections)))

(defun arxana-browser-essays-wikibooks--open-book-section (essay-id section-id section-name)
  "Open imported Wikibooks chapter SECTION-ID from ESSAY-ID as a full file."
  (let* ((manifest (arxana-browser-essays--manifest-for essay-id))
         (section (arxana-browser-essays-wikibooks--section-entry manifest section-id))
         (props (and section (plist-get section :props)))
         (source-file (and props (alist-get 'source-file props)))
         (page-title (and props (alist-get 'page-title props)))
         (section-text (arxana-browser-essays-wikibooks--whole-file-text source-file))
         (annotations (arxana-browser-essays--annotations-for-section
                       manifest section-id))
         (text-buf (arxana-browser-essays--render-section-text
                    essay-id section-id section-name
                    source-file page-title
                    section-text annotations))
         (notes-buf (arxana-browser-essays--render-section-notes
                     section-name annotations)))
    (arxana-browser-essays--display-section-buffers text-buf notes-buf)))

(defun arxana-browser-essays-wikibooks--open-section-advice
    (orig-fn essay-id section-id section-name)
  "Open Wikibooks imported chapter views as whole files when configured."
  (let* ((manifest (arxana-browser-essays--manifest-for essay-id))
         (section (and manifest
                       (arxana-browser-essays-wikibooks--section-entry
                        manifest section-id)))
         (props (and section (plist-get section :props))))
    (if (eq (alist-get 'render-whole-file props) t)
        (arxana-browser-essays-wikibooks--open-book-section
         essay-id section-id section-name)
      (funcall orig-fn essay-id section-id section-name))))

;;;###autoload
(defun arxana-browser-essays-wikibooks-import-book (root-title)
  "Import the Wikibooks book rooted at ROOT-TITLE into local essay files."
  (interactive "sWikibooks root page title: ")
  (let* ((raw-root (arxana-browser-essays-wikibooks--fetch-raw root-title))
         (pages (cons root-title
                      (arxana-browser-essays-wikibooks--extract-book-pages
                       root-title raw-root)))
         imported)
    (arxana-browser-essays-wikibooks--prune-stale-import-files root-title pages)
    (dolist (page pages)
      (push (arxana-browser-essays-wikibooks--import-page root-title page)
            imported))
    (setq imported (nreverse imported))
    (arxana-browser-essays-wikibooks--write-manifest-file
     (arxana-browser-essays-wikibooks--book-manifest-file root-title)
     (arxana-browser-essays-wikibooks--book-manifest-symbol root-title)
     (arxana-browser-essays-wikibooks--build-book-manifest root-title imported))
    (arxana-browser-essays-wikibooks-sync-registrations)
    (arxana-browser-essays-refresh)
    (arxana-browser-essays-wikibooks-sync-registrations)
    (message "[wikibooks] Imported %d page(s) for %s"
             (length imported) root-title)
    imported))

;;;###autoload
(defun arxana-browser-essays-wikibooks-import-peeragogy-handbook ()
  "Import the Peeragogy Handbook from Wikibooks into local essays."
  (interactive)
  (arxana-browser-essays-wikibooks-import-book
   arxana-browser-essays-wikibooks-peeragogy-root-title))

(advice-add 'arxana-browser-essays-refresh :after
            #'arxana-browser-essays-wikibooks--refresh-advice)
(advice-add 'arxana-browser-essays--open-section :around
            #'arxana-browser-essays-wikibooks--open-section-advice)

(arxana-browser-essays-wikibooks-sync-registrations)

(provide 'arxana-browser-essays-wikibooks)

;;; arxana-browser-essays-wikibooks.el ends here
