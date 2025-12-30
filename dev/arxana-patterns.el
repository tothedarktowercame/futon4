;;; arxana-patterns.el --- Futon pattern importer/editor -*- lexical-binding: t; -*-

;;; Commentary:
;; Fetch pattern-library entries from Futon1 (patterns ingested from Futon3) and
;; render them as editable Org buffers.  Each buffer exposes the pattern summary
;; and component passages so Emacs users can review and update pattern text
;; without dropping into the Futon CLI.  

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'org)
(require 'org-element)
(require 'tabulated-list)

(require 'arxana-store)
(require 'arxana-patterns-ingest)
(require 'arxana-flexiarg-collection)
(require 'arxana-browser-patterns)
(require 'arxana-browser-core)
(require 'arxana-browser-lab)
(require 'arxana-browser-docbook)
(require 'arxana-lab)
(require 'tabulated-list)
(require 'arxana-media)
(require 'arxana-browser-marks)

(defalias 'arxana-patterns--ensure-tabulated-list #'arxana-browser--ensure-tabulated-list)
(defalias 'arxana-patterns--tabulated-list-guard #'arxana-browser--tabulated-list-guard)

(unless (advice-member-p #'arxana-patterns--tabulated-list-guard 'tabulated-list-mode)
  (advice-add 'tabulated-list-mode :around #'arxana-patterns--tabulated-list-guard))

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

(defcustom arxana-patterns-prefer-filesystem t
  "When non-nil, open local flexiarg files if they are newer than XTDB."
  :type 'boolean
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

(defalias 'arxana-patterns-open-filesystem #'arxana-browser-patterns-open-filesystem)
(defalias 'arxana-patterns-open #'arxana-browser-patterns-open)
(defalias 'arxana-patterns-inspect-entity #'arxana-browser-patterns-inspect-entity)
(defalias 'arxana-patterns-refresh-buffer #'arxana-browser-patterns-refresh-buffer)
(defalias 'arxana-patterns-save #'arxana-browser-patterns-save)
(defalias 'arxana-patterns-edit-collection #'arxana-browser-patterns-edit-collection)
(defalias 'arxana-patterns-add-collection-root #'arxana-browser-patterns-add-collection-root)
(defalias 'arxana-patterns-import-all-collections #'arxana-browser-patterns-import-all-collections)
(defvaralias 'arxana-patterns-prefer-filesystem 'arxana-browser-patterns-prefer-filesystem)
(defvaralias 'arxana-patterns-view-mode-map 'arxana-browser-patterns-view-mode-map)
(defalias 'arxana-patterns-view-mode #'arxana-browser-patterns-view-mode)

;;; Compatibility aliases for browser-patterns helpers.
(defalias 'arxana-patterns--update-header-state #'arxana-browser-patterns--update-header-state)
(defalias 'arxana-patterns--play-click #'arxana-browser-patterns--play-click)
(defalias 'arxana-patterns--locate-library-root #'arxana-browser-patterns--locate-library-root)
(defalias 'arxana-patterns--file-mtime #'arxana-browser-patterns--file-mtime)
(defalias 'arxana-patterns--pattern-file #'arxana-browser-patterns--pattern-file)
(defalias 'arxana-patterns--normalize-last-seen #'arxana-browser-patterns--normalize-last-seen)
(defalias 'arxana-patterns--filesystem-newer-p #'arxana-browser-patterns--filesystem-newer-p)
(defalias 'arxana-patterns--library-directories #'arxana-browser-patterns--library-directories)
(defalias 'arxana-patterns--flexiarg-files-in-directory #'arxana-browser-patterns--flexiarg-files-in-directory)
(defalias 'arxana-patterns--flexiarg-files-for #'arxana-browser-patterns--flexiarg-files-for)
(defalias 'arxana-patterns--normalize-path #'arxana-browser-patterns--normalize-path)
(defalias 'arxana-patterns--library-metadata-path #'arxana-browser-patterns--library-metadata-path)
(defalias 'arxana-patterns--write-library-metadata #'arxana-browser-patterns--write-library-metadata)
(defalias 'arxana-patterns--read-library-metadata #'arxana-browser-patterns--read-library-metadata)
(defalias 'arxana-patterns--load-persisted-collection-roots #'arxana-browser-patterns--load-persisted-collection-roots)
(defalias 'arxana-patterns--persist-collection-roots #'arxana-browser-patterns--persist-collection-roots)
(defalias 'arxana-patterns--remember-collection-root #'arxana-browser-patterns--remember-collection-root)
(defalias 'arxana-patterns--collection-root-paths #'arxana-browser-patterns--collection-root-paths)
(defalias 'arxana-patterns--collection-directories #'arxana-browser-patterns--collection-directories)
(defalias 'arxana-patterns--collection-from-directory #'arxana-browser-patterns--collection-from-directory)
(defalias 'arxana-patterns--primary-order-for-file #'arxana-browser-patterns--primary-order-for-file)
(defalias 'arxana-patterns--sort-files-by-order #'arxana-browser-patterns--sort-files-by-order)
(defalias 'arxana-patterns--register-collection-root #'arxana-browser-patterns--register-collection-root)
(defalias 'arxana-patterns--read-file #'arxana-browser-patterns--read-file)
(defalias 'arxana-patterns--insert-flexiarg-section #'arxana-browser-patterns--insert-flexiarg-section)
(defalias 'arxana-patterns--friendly-classification #'arxana-browser-patterns--friendly-classification)
(defalias 'arxana-patterns--status-label #'arxana-browser-patterns--status-label)
(defalias 'arxana-patterns--imported-status-label #'arxana-browser-patterns--imported-status-label)
(defalias 'arxana-patterns--metadata-status-label #'arxana-browser-patterns--metadata-status-label)
(defalias 'arxana-patterns--flexiarg-set-order #'arxana-browser-patterns--flexiarg-set-order)
(defalias 'arxana-patterns--language-index-by-path #'arxana-browser-patterns--language-index-by-path)
(defalias 'arxana-patterns--parse-flexiarg #'arxana-browser-patterns--parse-flexiarg)
(defalias 'arxana-patterns--filesystem-collection-items #'arxana-browser-patterns--filesystem-collection-items)
(defalias 'arxana-patterns-edit-collection #'arxana-browser-patterns-edit-collection)
(defalias 'arxana-patterns--ensure-sync #'arxana-browser-patterns--ensure-sync)
(defalias 'arxana-patterns--alist #'arxana-browser-patterns--alist)
(defalias 'arxana-patterns--alist-like-p #'arxana-browser-patterns--alist-like-p)
(defalias 'arxana-patterns--entity-value #'arxana-browser-patterns--entity-value)
(defalias 'arxana-patterns--entity-from-version #'arxana-browser-patterns--entity-from-version)
(defalias 'arxana-patterns--find-entity #'arxana-browser-patterns--find-entity)
(defalias 'arxana-patterns--pattern-entity #'arxana-browser-patterns--pattern-entity)
(defalias 'arxana-patterns--resolve-entity-by-name #'arxana-browser-patterns--resolve-entity-by-name)
(defalias 'arxana-patterns--relation-text #'arxana-browser-patterns--relation-text)
(defalias 'arxana-patterns--relation-match-p #'arxana-browser-patterns--relation-match-p)
(defalias 'arxana-patterns--ego-outgoing #'arxana-browser-patterns--ego-outgoing)
(defalias 'arxana-patterns--link-prop #'arxana-browser-patterns--link-prop)
(defalias 'arxana-patterns--component-links #'arxana-browser-patterns--component-links)
(defalias 'arxana-patterns--component-slug-base #'arxana-browser-patterns--component-slug-base)
(defalias 'arxana-patterns--component-link-name #'arxana-browser-patterns--component-link-name)
(defalias 'arxana-patterns--lookup-component-by-name #'arxana-browser-patterns--lookup-component-by-name)
(defalias 'arxana-patterns--lookup-component-by-prefix #'arxana-browser-patterns--lookup-component-by-prefix)
(defalias 'arxana-patterns--extract-summary #'arxana-browser-patterns--extract-summary)
(defalias 'arxana-patterns--read-header-field #'arxana-browser-patterns--read-header-field)
(defalias 'arxana-patterns--component-name-info #'arxana-browser-patterns--component-name-info)
(defalias 'arxana-patterns--component-parent-id #'arxana-browser-patterns--component-parent-id)
(defalias 'arxana-patterns--fetch-entity-source #'arxana-browser-patterns--fetch-entity-source)
(defalias 'arxana-patterns--fetch-component #'arxana-browser-patterns--fetch-component)
(defalias 'arxana-patterns--compute-levels #'arxana-browser-patterns--compute-levels)
(defalias 'arxana-patterns--fetch-pattern-data #'arxana-browser-patterns--fetch-pattern-data)
(defalias 'arxana-patterns--insert-summary #'arxana-browser-patterns--insert-summary)
(defalias 'arxana-patterns--insert-component #'arxana-browser-patterns--insert-component)
(defalias 'arxana-patterns--render-pattern #'arxana-browser-patterns--render-pattern)
(defalias 'arxana-patterns-open-filesystem #'arxana-browser-patterns-open-filesystem)
(defalias 'arxana-patterns-open #'arxana-browser-patterns-open)
(defalias 'arxana-patterns-inspect-entity #'arxana-browser-patterns-inspect-entity)
(defalias 'arxana-patterns-refresh-buffer #'arxana-browser-patterns-refresh-buffer)
(defalias 'arxana-patterns--collect-components #'arxana-browser-patterns--collect-components)
(defalias 'arxana-patterns-save #'arxana-browser-patterns-save)
(defalias 'arxana-patterns--browser-pattern-row #'arxana-browser-patterns--browser-pattern-row)
(defalias 'arxana-patterns--menu-items #'arxana-browser-patterns--menu-items)
(defalias 'arxana-patterns--code-items #'arxana-browser-patterns--code-items)
(defalias 'arxana-patterns--ensure-frame #'arxana-browser-patterns--ensure-frame)
(defalias 'arxana-patterns--normalize-order #'arxana-browser-patterns--normalize-order)
(defalias 'arxana-patterns--language-pattern-entry #'arxana-browser-patterns--language-pattern-entry)
(defalias 'arxana-patterns--language-pattern-items #'arxana-browser-patterns--language-pattern-items)
(defalias 'arxana-patterns--filesystem-pattern-items #'arxana-browser-patterns--filesystem-pattern-items)
(defalias 'arxana-patterns--browser-pattern-items #'arxana-browser-patterns--browser-pattern-items)
(defalias 'arxana-patterns--move-entry #'arxana-browser-patterns--move-entry)
(defalias 'arxana-patterns--flexiarg-apply-order #'arxana-browser-patterns--flexiarg-apply-order)
(defalias 'arxana-patterns--language-entity-id #'arxana-browser-patterns--language-entity-id)
(defalias 'arxana-patterns--pattern-entry-id #'arxana-browser-patterns--pattern-entry-id)
(defalias 'arxana-patterns--language-apply-order #'arxana-browser-patterns--language-apply-order)
(defalias 'arxana-patterns--slugify #'arxana-browser-patterns--slugify)
(defalias 'arxana-patterns--library-default-language #'arxana-browser-patterns--library-default-language)
(defalias 'arxana-patterns-add-collection-root #'arxana-browser-patterns-add-collection-root)
(defalias 'arxana-patterns--import-library #'arxana-browser-patterns--import-library)
(defalias 'arxana-patterns-import-all-collections #'arxana-browser-patterns-import-all-collections)

;;; Compatibility aliases for browser-core helpers.
(defvaralias 'arxana-patterns--browser-buffer 'arxana-browser--buffer)
(defvaralias 'arxana-patterns--browser-stack 'arxana-browser--stack)
(defvaralias 'arxana-patterns--browser-context 'arxana-browser--context)
(defvaralias 'arxana-patterns--browser--last-row 'arxana-browser--last-row)
(defvaralias 'arxana-patterns--browser-click-default 'arxana-browser-click-default)
(defvaralias 'arxana-patterns-browser-enable-click 'arxana-browser-enable-click)
(defvaralias 'arxana-patterns-browser-click-sound 'arxana-browser-click-sound)
(defvaralias 'arxana-patterns-browser-click-volume 'arxana-browser-click-volume)
(defvaralias 'arxana-patterns-browser-wheel-step 'arxana-browser-wheel-step)
(defvaralias 'arxana-patterns-browser-mode-map 'arxana-browser-mode-map)
(defalias 'arxana-patterns-browser-mode #'arxana-browser-mode)
(defalias 'arxana-patterns--browser-click-path #'arxana-browser--click-path)
(defalias 'arxana-patterns--browser-ensure-context #'arxana-browser--ensure-context)
(defalias 'arxana-patterns--browser-root-format #'arxana-browser--root-format)
(defalias 'arxana-patterns--browser-pattern-format #'arxana-browser--pattern-format)
(defalias 'arxana-patterns--browser-menu-format #'arxana-browser--menu-format)
(defalias 'arxana-patterns--browser-info-format #'arxana-browser--info-format)
(defalias 'arxana-patterns--browser-root-row #'arxana-browser--root-row)
(defalias 'arxana-patterns--browser-menu-row #'arxana-browser--menu-row)
(defalias 'arxana-patterns--browser-info-row #'arxana-browser--info-row)
(defalias 'arxana-patterns--browser-header-line #'arxana-browser--header-line)
(defalias 'arxana-patterns--browser-decorate-header-line #'arxana-browser--decorate-header-line)
(defalias 'arxana-patterns--browser-root-items #'arxana-browser--root-items)
(defalias 'arxana-patterns--browser-move-pattern #'arxana-browser--move-pattern)
(defalias 'arxana-patterns--browser-move-pattern-up #'arxana-browser--move-pattern-up)
(defalias 'arxana-patterns--browser-move-pattern-down #'arxana-browser--move-pattern-down)
(defalias 'arxana-patterns--browser-current-items #'arxana-browser--current-items)
(defalias 'arxana-patterns--browser--row-count #'arxana-browser--row-count)
(defalias 'arxana-patterns--browser-row-count #'arxana-browser--row-count)
(defalias 'arxana-patterns--browser--current-row #'arxana-browser--current-row)
(defalias 'arxana-patterns--browser-current-row #'arxana-browser--current-row)
(defalias 'arxana-patterns--browser--goto-first-entry #'arxana-browser--goto-first-entry)
(defalias 'arxana-patterns--browser-goto-first-entry #'arxana-browser--goto-first-entry)
(defalias 'arxana-patterns--browser--goto-row #'arxana-browser--goto-row)
(defalias 'arxana-patterns--browser-goto-row #'arxana-browser--goto-row)
(defalias 'arxana-patterns--browser-goto-doc-id #'arxana-browser--goto-doc-id)
(defalias 'arxana-patterns--browser-goto-label #'arxana-browser--goto-label)
(defalias 'arxana-patterns--browser-move-selection #'arxana-browser--move-selection)
(defalias 'arxana-patterns--browser-wheel-steps #'arxana-browser--wheel-steps)
(defalias 'arxana-patterns--browser-wheel-down #'arxana-browser--wheel-down)
(defalias 'arxana-patterns--browser-wheel-up #'arxana-browser--wheel-up)
(defalias 'arxana-patterns--browser-next-line #'arxana-browser--next-line)
(defalias 'arxana-patterns--browser-previous-line #'arxana-browser--previous-line)
(defalias 'arxana-patterns--browser--tabulated-entries #'arxana-browser--tabulated-entries)
(defalias 'arxana-patterns--browser-tabulated-entries #'arxana-browser--tabulated-entries)
(defalias 'arxana-patterns--browser-item-at-point #'arxana-browser--item-at-point)
(defalias 'arxana-patterns--browser-render #'arxana-browser--render)
(defalias 'arxana-patterns--browser-visit #'arxana-browser--visit)
(defalias 'arxana-patterns--pattern-location #'arxana-browser--pattern-location)
(defalias 'arxana-patterns--collection-location #'arxana-browser--collection-location)
(defalias 'arxana-patterns--language-location #'arxana-browser--language-location)
(defalias 'arxana-patterns--browser-copy-location #'arxana-browser--copy-location)
(defalias 'arxana-patterns--browser-copy-current-location #'arxana-browser--copy-current-location)
(defalias 'arxana-patterns--browser-up #'arxana-browser--up)
(defalias 'arxana-patterns--browser-refresh #'arxana-browser--refresh)
(defalias 'arxana-patterns--browser-import-library #'arxana-browser--import-library)
(defalias 'arxana-patterns--browser-edit-collection #'arxana-browser--edit-collection)
(defalias 'arxana-patterns--browser-edit-current-context #'arxana-browser--edit-current-context)
(defalias 'arxana-patterns--browser-stage-to-ep #'arxana-browser--stage-to-ep)
(defalias 'arxana-patterns--browser-toggle-mark #'arxana-browser--toggle-mark)
(defalias 'arxana-patterns--browser-remove-marked #'arxana-browser--remove-marked)
(defalias 'arxana-patterns--browser-hard-delete-marked #'arxana-browser--hard-delete-marked)
(defalias 'arxana-patterns--browser-delete-marked #'arxana-browser--delete-marked)

;;; Compatibility aliases for browser-lab helpers.
(defalias 'arxana-patterns--lab-format #'arxana-browser--lab-format)
(defalias 'arxana-patterns--lab-file-format #'arxana-browser--lab-file-format)
(defalias 'arxana-patterns--lab-row #'arxana-browser--lab-row)
(defalias 'arxana-patterns--lab-file-row #'arxana-browser--lab-file-row)
(defalias 'arxana-patterns--lab-items #'arxana-browser--lab-items)
(defalias 'arxana-patterns-browse-lab-files #'arxana-browser-lab-browse-files)
(defalias 'arxana-patterns-browse-lab-files-other-frame
  #'arxana-browser-lab-browse-files-other-frame)
(defalias 'arxana-patterns-browse-lab-files-other-window
  #'arxana-browser-lab-browse-files-other-frame)
(defalias 'arxana-patterns--lab-entry-at-point #'arxana-browser--lab-entry-at-point)
(defalias 'arxana-patterns--lab-open-trace #'arxana-browser--lab-open-trace)
(defalias 'arxana-patterns--lab-open-raw #'arxana-browser--lab-open-raw)
(defalias 'arxana-patterns--lab-open-draft #'arxana-browser--lab-open-draft)

;;; Compatibility aliases for media helpers.
(defalias 'arxana-patterns--media-items #'arxana-media--items)
(defalias 'arxana-patterns--media-entries #'arxana-media--entries)
(defalias 'arxana-patterns--media-track-items #'arxana-media--track-items)
(defalias 'arxana-patterns--media-project-items #'arxana-media--project-items)
(defalias 'arxana-patterns--media-publications-items #'arxana-media--publications-items)
(defalias 'arxana-patterns--media-publication-track-items #'arxana-media--publication-track-items)
(defalias 'arxana-patterns--media-ep-staging-items #'arxana-media--ep-staging-items)
(defalias 'arxana-patterns--media-misc-items #'arxana-media--misc-items)
(defalias 'arxana-patterns--media-misc-track-items #'arxana-media--misc-track-items)
(defalias 'arxana-patterns--media-track-row #'arxana-media--track-row)
(defalias 'arxana-patterns--media-publication-track-row #'arxana-media--publication-track-row)
(defalias 'arxana-patterns--media-misc-track-row #'arxana-media--misc-track-row)
(defalias 'arxana-patterns--media-track-format #'arxana-media--track-format)
(defalias 'arxana-patterns--media-publication-track-format #'arxana-media--publication-track-format)
(defalias 'arxana-patterns--media-misc-track-format #'arxana-media--misc-track-format)
(defalias 'arxana-patterns-media-bounce-or-up #'arxana-media-bounce-or-up)
(defalias 'arxana-patterns-media-retitle-at-point #'arxana-media-retitle-at-point)
(defalias 'arxana-patterns-media-play-at-point #'arxana-media-play-at-point)
(defalias 'arxana-patterns-media-stop-playback #'arxana-media-stop-playback)
(defalias 'arxana-patterns-media-toggle-autoplay-next #'arxana-media-toggle-autoplay-next)
(defalias 'arxana-patterns-media-edit-lyrics-at-point #'arxana-media-edit-lyrics-at-point)
(defalias 'arxana-patterns-media-open-in-audacity #'arxana-media-open-in-audacity)
(defalias 'arxana-patterns-media-playback-pause-toggle #'arxana-media-playback-pause-toggle)
(defalias 'arxana-patterns-media-playback-seek-back-10 #'arxana-media-playback-seek-back-10)
(defalias 'arxana-patterns-media-playback-seek-forward-10 #'arxana-media-playback-seek-forward-10)
(defalias 'arxana-patterns-media-playback-seek-back-30 #'arxana-media-playback-seek-back-30)
(defalias 'arxana-patterns-media-playback-seek-forward-30 #'arxana-media-playback-seek-forward-30)
(defalias 'arxana-patterns-media-unmark-all #'arxana-media-unmark-all)
(defalias 'arxana-patterns-media-set-status-marked #'arxana-media-set-status-marked)
(defalias 'arxana-patterns-media-publish-marked #'arxana-media-publish-marked)
(defalias 'arxana-patterns-media-set-publication-url #'arxana-media-set-publication-url)
(defalias 'arxana-patterns-media-open-publication-url #'arxana-media-open-publication-url)
(defalias 'arxana-patterns-media-stage-to-ep #'arxana-media-stage-to-ep)
(defalias 'arxana-patterns-media-delete-at-point #'arxana-media-delete-at-point)
(defalias 'arxana-patterns-media-move-misc-to-ep-at-point #'arxana-media-move-misc-to-ep-at-point)
(defalias 'arxana-patterns-media-toggle-mark-at-point #'arxana-media-toggle-mark-at-point)

;;; Compatibility aliases for docbook helpers.
(define-obsolete-face-alias 'arxana-patterns-docbook-latest-face
  'arxana-browser-docbook-latest-face "2025-12-30")
(define-obsolete-face-alias 'arxana-patterns-docbook-empty-face
  'arxana-browser-docbook-empty-face "2025-12-30")
(define-obsolete-face-alias 'arxana-patterns-docbook-unindexed-face
  'arxana-browser-docbook-unindexed-face "2025-12-30")
(define-obsolete-face-alias 'arxana-patterns-docbook-marked-face
  'arxana-browser-docbook-marked-face "2025-12-30")
(defvaralias 'arxana-patterns-docbook-top-order 'arxana-browser-docbook-top-order)
(defvaralias 'arxana-patterns-docbook-prefer-toc-order
  'arxana-browser-docbook-prefer-toc-order)
(defvar-local arxana-patterns--docbook-contents-order nil
  "Obsolete; use `arxana-browser--docbook-contents-order'.")
(defvar-local arxana-patterns--docbook-contents-order-source nil
  "Obsolete; use `arxana-browser--docbook-contents-order-source'.")
(defvar-local arxana-patterns--docbook-contents-synced-order nil
  "Obsolete; use `arxana-browser--docbook-contents-synced-order'.")
(defvar-local arxana-patterns--docbook-contents-toc nil
  "Obsolete; use `arxana-browser--docbook-contents-toc'.")
(defvar-local arxana-patterns--docbook-contents-view-items nil
  "Obsolete; use `arxana-browser--docbook-contents-view-items'.")
(defvar-local arxana-patterns--docbook-contents-book nil
  "Obsolete; use `arxana-browser--docbook-contents-book'.")
(defvar-local arxana-patterns--docbook-contents-marked nil
  "Obsolete; use `arxana-browser--docbook-contents-marked'.")
(defvar-local arxana-patterns--docbook-contents-removed nil
  "Obsolete; use `arxana-browser--docbook-contents-removed'.")
(defvar-local arxana-patterns--docbook-contents-hidden nil
  "Obsolete; use `arxana-browser--docbook-contents-hidden'.")
(make-obsolete-variable 'arxana-patterns--docbook-contents-order
                        'arxana-browser--docbook-contents-order "2025-12-30")
(make-obsolete-variable 'arxana-patterns--docbook-contents-order-source
                        'arxana-browser--docbook-contents-order-source "2025-12-30")
(make-obsolete-variable 'arxana-patterns--docbook-contents-synced-order
                        'arxana-browser--docbook-contents-synced-order "2025-12-30")
(make-obsolete-variable 'arxana-patterns--docbook-contents-toc
                        'arxana-browser--docbook-contents-toc "2025-12-30")
(make-obsolete-variable 'arxana-patterns--docbook-contents-view-items
                        'arxana-browser--docbook-contents-view-items "2025-12-30")
(make-obsolete-variable 'arxana-patterns--docbook-contents-book
                        'arxana-browser--docbook-contents-book "2025-12-30")
(make-obsolete-variable 'arxana-patterns--docbook-contents-marked
                        'arxana-browser--docbook-contents-marked "2025-12-30")
(make-obsolete-variable 'arxana-patterns--docbook-contents-removed
                        'arxana-browser--docbook-contents-removed "2025-12-30")
(make-obsolete-variable 'arxana-patterns--docbook-contents-hidden
                        'arxana-browser--docbook-contents-hidden "2025-12-30")
(defalias 'arxana-patterns--docbook-format #'arxana-browser--docbook-format)
(defalias 'arxana-patterns--docbook-contents-format
  #'arxana-browser--docbook-contents-format)
(defalias 'arxana-patterns--docbook-lines-in-string
  #'arxana-browser--docbook-lines-in-string)
(defalias 'arxana-patterns--docbook-file-line-count
  #'arxana-browser--docbook-file-line-count)
(defalias 'arxana-patterns--docbook-ratio-format
  #'arxana-browser--docbook-ratio-format)
(defalias 'arxana-patterns--docbook-entry-source-path
  #'arxana-browser--docbook-entry-source-path)
(defalias 'arxana-patterns--docbook-entry-doc-lines
  #'arxana-browser--docbook-entry-doc-lines)
(defalias 'arxana-patterns--docbook-entry-loc
  #'arxana-browser--docbook-entry-loc)
(defalias 'arxana-patterns--docbook-entry-coverage
  #'arxana-browser--docbook-entry-coverage)
(defalias 'arxana-patterns--docbook-entry-ratio
  #'arxana-browser--docbook-entry-ratio)
(defalias 'arxana-patterns--docbook-top-key
  #'arxana-browser--docbook-top-key)
(defalias 'arxana-patterns--docbook-group-order
  #'arxana-browser--docbook-group-order)
(defalias 'arxana-patterns--docbook-group-items
  #'arxana-browser--docbook-group-items)
(defalias 'arxana-patterns--docbook-contents-order-get
  #'arxana-browser--docbook-contents-order-get)
(defalias 'arxana-patterns--docbook-contents-order-set
  #'arxana-browser--docbook-contents-order-set)
(defalias 'arxana-patterns--docbook-contents-order-source-get
  #'arxana-browser--docbook-contents-order-source-get)
(defalias 'arxana-patterns--docbook-contents-order-source-set
  #'arxana-browser--docbook-contents-order-source-set)
(defalias 'arxana-patterns--docbook-contents-dirty-p
  #'arxana-browser--docbook-contents-dirty-p)
(defalias 'arxana-patterns--docbook-entry-local-mtime
  #'arxana-browser--docbook-entry-local-mtime)
(defalias 'arxana-patterns--docbook-latest-remote-entry
  #'arxana-browser--docbook-latest-remote-entry)
(defalias 'arxana-patterns--docbook-entry-dirty-p
  #'arxana-browser--docbook-entry-dirty-p)
(defalias 'arxana-patterns--docbook-sync-order
  #'arxana-browser--docbook-sync-order)
(defalias 'arxana-patterns--docbook-contents-order-items
  #'arxana-browser--docbook-contents-order-items)
(defalias 'arxana-patterns--docbook-contents-normalize-order
  #'arxana-browser--docbook-contents-normalize-order)
(defalias 'arxana-patterns--docbook-contents-removed-get
  #'arxana-browser--docbook-contents-removed-get)
(defalias 'arxana-patterns--docbook-contents-removed-set
  #'arxana-browser--docbook-contents-removed-set)
(defalias 'arxana-patterns--docbook-contents-removed-p
  #'arxana-browser--docbook-contents-removed-p)
(defalias 'arxana-patterns--docbook-contents-marked-p
  #'arxana-browser--docbook-contents-marked-p)
(defalias 'arxana-patterns--docbook-contents-mark-key
  #'arxana-browser--docbook-contents-mark-key)
(defalias 'arxana-patterns-docbook-toggle-mark-at-point
  #'arxana-browser-docbook-toggle-mark-at-point)
(defalias 'arxana-patterns-docbook-remove-marked
  #'arxana-browser-docbook-remove-marked)
(defalias 'arxana-patterns--docbook-contents-refresh-local
  #'arxana-browser--docbook-contents-refresh-local)
(defalias 'arxana-patterns--docbook-contents-update-line
  #'arxana-browser--docbook-contents-update-line)
(defalias 'arxana-patterns-docbook-hard-delete-marked
  #'arxana-browser-docbook-hard-delete-marked)
(defalias 'arxana-patterns--docbook-contents-ensure-order
  #'arxana-browser--docbook-contents-ensure-order)
(defalias 'arxana-patterns--docbook-contents-item-map
  #'arxana-browser--docbook-contents-item-map)
(defalias 'arxana-patterns--docbook-short-id
  #'arxana-browser--docbook-short-id)
(defalias 'arxana-patterns--docbook-row-label
  #'arxana-browser--docbook-row-label)
(defalias 'arxana-patterns--docbook-row
  #'arxana-browser--docbook-row)
(defalias 'arxana-patterns--docbook-contents-row
  #'arxana-browser--docbook-contents-row)
(defalias 'arxana-patterns--docbook-books
  #'arxana-browser--docbook-books)
(defalias 'arxana-patterns--docbook-unavailable-message
  #'arxana-browser--docbook-unavailable-message)
(defalias 'arxana-patterns--docbook-entry-label
  #'arxana-browser--docbook-entry-label)
(defalias 'arxana-patterns--docbook-items
  #'arxana-browser--docbook-items)
(defalias 'arxana-patterns--docbook-book-items
  #'arxana-browser--docbook-book-items)
(defalias 'arxana-patterns--docbook-contents-items
  #'arxana-browser--docbook-contents-items)
(defalias 'arxana-patterns--docbook-contents-context
  #'arxana-browser--docbook-contents-context)
(defalias 'arxana-patterns--docbook-contents-assert
  #'arxana-browser--docbook-contents-assert)
(defalias 'arxana-patterns--docbook-contents-current-item
  #'arxana-browser--docbook-contents-current-item)
(defalias 'arxana-patterns--docbook-contents-items-live
  #'arxana-browser--docbook-contents-items-live)
(defalias 'arxana-patterns--docbook-contents-unique-items
  #'arxana-browser--docbook-contents-unique-items)
(defalias 'arxana-patterns--docbook-contents-current-order
  #'arxana-browser--docbook-contents-current-order)
(defalias 'arxana-patterns--docbook-contents-item-swap
  #'arxana-browser--docbook-contents-item-swap)
(defalias 'arxana-patterns--docbook-contents-reorder
  #'arxana-browser--docbook-contents-reorder)
(defalias 'arxana-patterns-docbook-move-item-up
  #'arxana-browser-docbook-move-item-up)
(defalias 'arxana-patterns-docbook-move-item-down
  #'arxana-browser-docbook-move-item-down)
(defalias 'arxana-patterns--docbook-contents-section-blocks
  #'arxana-browser--docbook-contents-section-blocks)
(defalias 'arxana-patterns-docbook-move-section-up
  #'arxana-browser-docbook-move-section-up)
(defalias 'arxana-patterns-docbook-move-section-down
  #'arxana-browser-docbook-move-section-down)
(defalias 'arxana-patterns--docbook-contents-top-level-p
  #'arxana-browser--docbook-contents-top-level-p)
(defalias 'arxana-patterns-docbook-move-top
  #'arxana-browser-docbook-move-top)
(defalias 'arxana-patterns-docbook-move-bottom
  #'arxana-browser-docbook-move-bottom)
(defalias 'arxana-patterns--docbook-section-items
  #'arxana-browser--docbook-section-items)
(defalias 'arxana-patterns--docbook-heading-at-point
  #'arxana-browser--docbook-heading-at-point)
(defalias 'arxana-patterns-docbook-open-book
  #'arxana-browser-docbook-open-book)
(defalias 'arxana-patterns-docbook-open-section-context
  #'arxana-browser-docbook-open-section-context)
(defalias 'arxana-patterns--docbook-location
  #'arxana-browser--docbook-location)
(defalias 'arxana-patterns-docbook-export-org
  #'arxana-browser-docbook-export-org)
(defalias 'arxana-patterns-docbook-sync-order
  #'arxana-browser-docbook-sync-order)
(defalias 'arxana-patterns-docbook-export-pdf
  #'arxana-browser-docbook-export-pdf)


(if (boundp 'org-src-lang-modes)
    (add-to-list 'org-src-lang-modes '("flexiarg" . flexiarg) t)
  (with-eval-after-load 'org
    (add-to-list 'org-src-lang-modes '("flexiarg" . flexiarg) t)))

(defvar arxana-patterns--additional-collection-roots nil
  "Session-local list of ad-hoc pattern collection directories.")

(defconst arxana-patterns--library-metadata-file ".arxana-language"
  "Marker file recording Futon language details for a library directory.")

(defvar arxana-patterns--persisted-collection-roots-loaded nil)
(defvar arxana-patterns--persisted-collection-roots nil)

(defalias 'arxana-patterns-import-all-libraries #'arxana-patterns-import-all-collections)

(provide 'arxana-patterns)
;;; arxana-patterns.el ends here
