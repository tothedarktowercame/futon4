;;; arxana-browser-essays.el --- Annotated Edition support for essays -*- lexical-binding: t; -*-

;;; Commentary:
;; Annotated-edition support for essays (working papers and similar long-form
;; prose), companion to `arxana-browser-songs.el'.  The storage model is the
;; same (XTDB entities + annotation hyperedges, per README-songs.md); the
;; shape differs in two respects:
;;
;;   (1) Source endpoints reference existing pattern-library entities by
;;       their `:name' (the flexiarg `@flexiarg` value), rather than being
;;       upserted by this module.  If a pattern is not yet in the store,
;;       the importer calls `arxana-patterns-ingest-file' on the canonical
;;       .flexiarg path, using a configurable library root.
;;
;;   (2) The annotated side is a hierarchy: essay → essay-section → passage.
;;       The manifest declares the essay entity and its section entities;
;;       the importer upserts both before creating annotation hyperedges.
;;
;; Manifest format: Elisp plist (see `arxana-browser-essays-ukrn-wp-manifest'
;; in ~/npt/working-paper/annotations.el for the reference example).  Keys:
;;
;;   :version      — manifest schema version (currently 1).
;;   :essay        — plist with :id, :name, :type, :source-file, :props.
;;   :sections     — list of plists; each with :id, :name, :type, :props.
;;   :annotations  — list of plists; each with :id, :hx-type, :annotated,
;;                   :source, :note, :labels.
;;     :annotated  is a plist with :entity-id and :passage.
;;     :source     is a plist with :pattern-name (resolves to entity-id)
;;                 and :passage.
;;
;; Annotation `:hx-type' conventions (mirrored from the Songs demo):
;;   annotation/grounds        — annotated rests on source claim
;;   annotation/instantiates   — annotated is canonical form of source T/C/C
;;   annotation/exhibits       — annotated is computational instance source names
;;   annotation/engages-hole   — annotated engages a content gap in source

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'arxana-store)
(require 'arxana-patterns-ingest)

(defgroup arxana-browser-essays nil
  "Annotated-edition support for essays."
  :group 'arxana)

(defcustom arxana-browser-essays-manifest-files
  (list (expand-file-name "~/npt/working-paper/annotations.el"))
  "Elisp manifest files to load when importing essay annotations.
Each file must define a defconst whose value is a manifest plist (see
commentary above).  The importer can accept a manifest symbol or value
directly if a file-based flow is not wanted."
  :type '(repeat file)
  :group 'arxana-browser-essays)

(defcustom arxana-browser-essays-pattern-library-root
  (expand-file-name "~/code/futon3/library")
  "Directory under which pattern `.flexiarg' files are located.
Used to resolve missing pattern entities (looked up by pattern-name +
`.flexiarg' suffix) so they can be ingested on demand."
  :type 'directory
  :group 'arxana-browser-essays)

;;; ─────────────────────────────────────────────────────────────
;;; Manifest loading
;;; ─────────────────────────────────────────────────────────────

(defun arxana-browser-essays--load-manifest-file (path)
  "Load PATH (Elisp file) and return the first `defconst' value it defines."
  (let ((symbol nil))
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
        (end-of-file nil))
      (when symbol
        (load path nil t)
        (symbol-value symbol)))))

(defun arxana-browser-essays--all-manifests ()
  "Return the list of manifest plists loaded from the configured files."
  (delq nil
        (mapcar #'arxana-browser-essays--load-manifest-file
                arxana-browser-essays-manifest-files)))

;;;###autoload
(defun arxana-browser-essays-refresh ()
  "Force-reload manifest files and (re-)apply the catalog defcustom value.
Run after editing `annotations.el' or `arxana-browser-essays.el' to pick
up the latest content without restarting Emacs."
  (interactive)
  ;; Re-load each manifest file; defconst will re-evaluate.
  (dolist (path arxana-browser-essays-manifest-files)
    (when (file-readable-p path)
      (load path nil t)))
  ;; Re-apply the catalog defcustom value from this file's source.  Without
  ;; this, defcustom keeps its previously-loaded value even after eval-buffer.
  (custom-reevaluate-setting 'arxana-browser-essays-catalogs)
  (when (called-interactively-p 'interactive)
    (message "Refreshed %d manifest(s); %d catalog(s)"
             (length arxana-browser-essays-manifest-files)
             (length arxana-browser-essays-catalogs))))

;;; ─────────────────────────────────────────────────────────────
;;; Pattern resolution
;;; ─────────────────────────────────────────────────────────────

;; Response-shape helpers (mirror arxana-browser-songs internals so we
;; don't depend on private symbols there).  See arxana-browser-songs.el
;; for the same patterns.

(defun arxana-browser-essays--unwrap-entities (response)
  "Navigate RESPONSE and return the contained list of entity alists."
  (let ((entities (and (listp response)
                       (or (ignore-errors (alist-get :entities response))
                           (ignore-errors (alist-get :items response))
                           response))))
    (cond
     ((null entities) nil)
     ((vectorp entities) (append entities nil))
     ((and (listp entities)
           (listp (car entities))
           (ignore-errors (alist-get :entity (car entities))))
      (mapcar (lambda (item) (or (alist-get :entity item) item)) entities))
     ((listp entities) entities)
     (t nil))))

(defun arxana-browser-essays--entity-id (entity)
  "Extract the canonical id from ENTITY (alist), trying common key variants."
  (and (listp entity)
       (or (alist-get :entity/id entity)
           (alist-get :xt/id entity)
           (alist-get :entity/external-id entity)
           (alist-get :id entity))))

(defun arxana-browser-essays--entity-name (entity)
  "Extract the canonical name from ENTITY (alist).
Pattern entities use `:ident' / `:entity/ident'; media entities use
`:name' / `:entity/name'.  Try both schemas."
  (and (listp entity)
       (or (alist-get :entity/ident entity)
           (alist-get :ident entity)
           (alist-get :entity/name entity)
           (alist-get :name entity))))

(defun arxana-browser-essays--pattern-entity-id (pattern-name)
  "Return the Futon entity-id for PATTERN-NAME, or nil.
PATTERN-NAME is the flexiarg `@flexiarg' value (e.g. \"ukrns/ARGUMENT\").
Filters by name client-side because the latest-entities endpoint only
accepts :type and :limit."
  (let* ((response (arxana-store-fetch-entities-latest
                    :type "pattern/library" :limit 2000))
         (entities (arxana-browser-essays--unwrap-entities response))
         (match (seq-find
                 (lambda (e)
                   (let ((n (arxana-browser-essays--entity-name e)))
                     (and (stringp n) (string= n pattern-name))))
                 entities)))
    (when match
      (arxana-browser-essays--entity-id match))))

(defun arxana-browser-essays--ingest-pattern (pattern-name)
  "Ingest the pattern file corresponding to PATTERN-NAME and return the result.
Searches under `arxana-browser-essays-pattern-library-root'.  Returns the
plist `(:name NAME :id ID)' from `arxana-patterns-ingest-file'."
  (let* ((path (expand-file-name
                (concat pattern-name ".flexiarg")
                arxana-browser-essays-pattern-library-root)))
    (unless (file-readable-p path)
      (user-error "Pattern file not found: %s" path))
    (message "Ingesting pattern: %s" path)
    (arxana-patterns-ingest-file path)))

(defun arxana-browser-essays--resolve-pattern (pattern-name)
  "Return entity-id for PATTERN-NAME, ingesting from disk if absent.
Prefers the id returned directly by the ingest helper over a second
round-trip through `--pattern-entity-id', which avoids the limit problem
when the store contains many `pattern/library' entities."
  (or (arxana-browser-essays--pattern-entity-id pattern-name)
      (let ((result (arxana-browser-essays--ingest-pattern pattern-name)))
        (or (plist-get result :id)
            (arxana-browser-essays--pattern-entity-id pattern-name)))
      (user-error "Unable to resolve pattern: %s" pattern-name)))

;;; ─────────────────────────────────────────────────────────────
;;; Entity + annotation import
;;; ─────────────────────────────────────────────────────────────

(defun arxana-browser-essays--ensure-essay (essay dry-run)
  "Upsert ESSAY entity unless DRY-RUN."
  (if dry-run
      (message "[dry-run] ensure-essay %s" (plist-get essay :id))
    (arxana-store-ensure-entity
     :id (plist-get essay :id)
     :name (plist-get essay :name)
     :type (plist-get essay :type)
     :props (plist-get essay :props))))

(defun arxana-browser-essays--ensure-section (section dry-run)
  "Upsert SECTION entity unless DRY-RUN."
  (if dry-run
      (message "[dry-run] ensure-section %s" (plist-get section :id))
    (arxana-store-ensure-entity
     :id (plist-get section :id)
     :name (plist-get section :name)
     :type (plist-get section :type)
     :props (plist-get section :props))))

(defun arxana-browser-essays--create-annotation (annotation dry-run)
  "Create hyperedge for ANNOTATION unless DRY-RUN.
Resolves :source :pattern-name to an entity-id; ingests the pattern file
on demand if the entity is not yet in the store."
  (let* ((annotated (plist-get annotation :annotated))
         (source (plist-get annotation :source))
         (pattern-name (plist-get source :pattern-name))
         (pattern-id (if dry-run
                         (format "<pattern:%s>" pattern-name)
                       (arxana-browser-essays--resolve-pattern pattern-name)))
         (endpoints
          `(((:role . "annotated")
             (:entity-id . ,(plist-get annotated :entity-id))
             (:passage . ,(plist-get annotated :passage)))
            ((:role . "source")
             (:entity-id . ,pattern-id)
             (:passage . ,(plist-get source :passage))))))
    (if dry-run
        (message "[dry-run] %s -> %s" (plist-get annotation :id) pattern-id)
      (arxana-store-create-hyperedge
       :id (plist-get annotation :id)
       :type "arxana/annotation"
       :hx-type (plist-get annotation :hx-type)
       :endpoints endpoints
       :props (delq nil
                    (list (cons 'note (plist-get annotation :note))
                          (when (plist-get annotation :retracted)
                            (cons 'retracted t))))
       :labels (let ((base (plist-get annotation :labels)))
                 (if (plist-get annotation :retracted)
                     (append base (list "annotation/retracted"))
                   base))))))

(defun arxana-browser-essays--import-manifest (manifest dry-run)
  "Import essay, sections, and annotations from MANIFEST plist.
When DRY-RUN is non-nil, log intentions without writing.  Returns a
plist summary: :essay, :section-count, :annotation-count."
  (let* ((essay (plist-get manifest :essay))
         (sections (plist-get manifest :sections))
         (annotations (plist-get manifest :annotations)))
    (arxana-browser-essays--ensure-essay essay dry-run)
    (dolist (section sections)
      (arxana-browser-essays--ensure-section section dry-run))
    (dolist (annotation annotations)
      (arxana-browser-essays--create-annotation annotation dry-run))
    (list :essay (plist-get essay :id)
          :section-count (length sections)
          :annotation-count (length annotations))))

;;;###autoload
(defun arxana-browser-essays-import (&optional dry-run)
  "Import all configured Essay manifests into XTDB.
With prefix argument DRY-RUN, print what would be imported without
making any changes."
  (interactive "P")
  (unless dry-run
    (unless (arxana-store-ensure-sync)
      (user-error "Futon sync is disabled; enable futon4-enable-sync first")))
  (let ((manifests (arxana-browser-essays--all-manifests))
        (summaries nil))
    (unless manifests
      (user-error "No Essay manifests found in %s"
                  arxana-browser-essays-manifest-files))
    (dolist (manifest manifests)
      (push (arxana-browser-essays--import-manifest manifest dry-run)
            summaries))
    (setq summaries (nreverse summaries))
    (when (called-interactively-p 'interactive)
      (dolist (s summaries)
        (message "%s Essay %s: %d section%s, %d annotation%s"
                 (if dry-run "Prepared" "Imported")
                 (plist-get s :essay)
                 (plist-get s :section-count)
                 (if (= (plist-get s :section-count) 1) "" "s")
                 (plist-get s :annotation-count)
                 (if (= (plist-get s :annotation-count) 1) "" "s"))))
    summaries))

;;; ─────────────────────────────────────────────────────────────
;;; Browser catalog (Essays in M-x arxana-browse)
;;; ─────────────────────────────────────────────────────────────

(defcustom arxana-browser-essays-catalogs
  '((:id ukrn-wp
     :label "UKRN Working Paper"
     :description "Annotated edition of the UKRN WP draft v5."
     :essay-id "arxana/essay/ukrn-wp"
     :manifest-symbol arxana-browser-essays-ukrn-wp-manifest
     :source-file "/home/joe/npt/working-paper/UKRN_WP_draft_v5.md"))
  "Catalog definitions surfaced under the Essays browser menu.
Each entry is a plist with :id, :label, :description, :essay-id,
:manifest-symbol (the defconst holding the manifest), and :source-file
(the markdown file the essay reads from)."
  :type 'sexp
  :group 'arxana-browser-essays)

(defcustom arxana-browser-essays-text-buffer "*Arxana Essay*"
  "Buffer name used for the essay text in the reading view."
  :type 'string
  :group 'arxana-browser-essays)

(defcustom arxana-browser-essays-notes-buffer "*Arxana Essay Notes*"
  "Buffer name used for annotation notes in the reading view."
  :type 'string
  :group 'arxana-browser-essays)

(defcustom arxana-browser-essays-notes-side 'right
  "Side for the annotation notes window."
  :type '(choice (const left) (const right))
  :group 'arxana-browser-essays)

(defcustom arxana-browser-essays-notes-width 0.45
  "Width for the annotation notes side window (fraction of frame)."
  :type 'number
  :group 'arxana-browser-essays)

(defface arxana-browser-essays-annotation-face
  '((t :background "#eef6ff"))
  "Face for annotated passages in the essay text."
  :group 'arxana-browser-essays)

(defface arxana-browser-essays-marker-face
  '((t :foreground "#2563eb" :weight bold))
  "Face for inline annotation markers like [1], [2]."
  :group 'arxana-browser-essays)

(defface arxana-browser-essays-active-face
  '((t :background "#dbeafe"))
  "Face for the currently-active passage / note (point-tracked)."
  :group 'arxana-browser-essays)

(defface arxana-browser-essays-pattern-link-face
  '((t :inherit link))
  "Face for design-pattern hyperlinks in `*Arxana Essay Notes*'."
  :group 'arxana-browser-essays)

(defface arxana-browser-essays-comment-face
  '((t :background "#fef3c7"))
  "Face for author-comment passages (Google-Docs-style sticky notes)."
  :group 'arxana-browser-essays)

(defface arxana-browser-essays-comment-marker-face
  '((t :foreground "#92400e" :weight bold))
  "Face for the inline marker on comment annotations."
  :group 'arxana-browser-essays)

(defun arxana-browser-essays--catalog-spec (essay-id)
  "Return the catalog spec whose :essay-id matches ESSAY-ID."
  (seq-find
   (lambda (cat) (string= (plist-get cat :essay-id) essay-id))
   arxana-browser-essays-catalogs))

(defun arxana-browser-essays--manifest-for (essay-id)
  "Return the loaded manifest plist for ESSAY-ID, or nil."
  (let ((cat (arxana-browser-essays--catalog-spec essay-id)))
    (when cat
      (or (and (boundp (plist-get cat :manifest-symbol))
               (symbol-value (plist-get cat :manifest-symbol)))
          (progn
            ;; Lazy-load from the configured manifest files if not already bound.
            (arxana-browser-essays--all-manifests)
            (and (boundp (plist-get cat :manifest-symbol))
                 (symbol-value (plist-get cat :manifest-symbol))))))))

(defun arxana-browser-essays--annotation-retracted-p (ann)
  "Return non-nil if ANN is marked retracted in the manifest.
Retracted annotations remain in the manifest as hypergraph citizens
(they carry provenance and may be re-attached later) but are filtered
out of the render and overlay passes."
  (plist-get ann :retracted))

(defun arxana-browser-essays--annotations-for-section (manifest section-id &optional include-retracted)
  "Return live annotations from MANIFEST targeting SECTION-ID.
Retracted annotations (marked `:retracted t') are excluded unless
INCLUDE-RETRACTED is non-nil."
  (let ((all (seq-filter
              (lambda (ann)
                (string= (plist-get (plist-get ann :annotated) :entity-id)
                         section-id))
              (plist-get manifest :annotations))))
    (if include-retracted
        all
      (seq-remove #'arxana-browser-essays--annotation-retracted-p all))))

;;;###autoload
(defun arxana-browser-essays-menu-items ()
  "Return top-level Essays menu items, one per registered essay catalog."
  (mapcar
   (lambda (cat)
     (let* ((manifest (arxana-browser-essays--manifest-for
                       (plist-get cat :essay-id)))
            (sections (and manifest (plist-get manifest :sections)))
            (annotations (and manifest (plist-get manifest :annotations)))
            (count-sections (length sections))
            (count-annotations (length annotations)))
       (list :type 'essays-essay
             :label (plist-get cat :label)
             :description (format "%s (%d section%s, %d annotation%s)"
                                  (plist-get cat :description)
                                  count-sections
                                  (if (= count-sections 1) "" "s")
                                  count-annotations
                                  (if (= count-annotations 1) "" "s"))
             :view 'essays-essay
             :essay-id (plist-get cat :essay-id))))
   arxana-browser-essays-catalogs))

(defun arxana-browser-essays-format (&optional context)
  "Return tabulated-list format for the Essays browser CONTEXT."
  (pcase (and context (plist-get context :view))
    ('essays-section [("Annotation" 36 t) ("Type" 22 t) ("Source pattern" 0 nil)])
    (_ [("Section / Essay" 56 t) ("Annotations" 12 t)])))

(defun arxana-browser-essays-row (item)
  "Return a tabulated-list row for ITEM."
  (pcase (plist-get item :type)
    ('essays-annotation
     (vector (or (plist-get item :label) "")
             (or (plist-get item :hx-type-short) "")
             (or (plist-get item :pattern-name) "")))
    (_
     (vector (or (plist-get item :label) "")
             (format "%s" (or (plist-get item :annotation-count) 0))))))

(defun arxana-browser-essays-items (context)
  "Return browser items for CONTEXT, dispatched by view."
  (pcase (plist-get context :view)
    ('essays-home
     (arxana-browser-essays-menu-items))

    ('essays-essay
     (let* ((essay-id (plist-get context :essay-id))
            (manifest (arxana-browser-essays--manifest-for essay-id))
            (sections (and manifest (plist-get manifest :sections))))
       (if sections
           (mapcar
            (lambda (section)
              (let* ((sid (plist-get section :id))
                     (all (arxana-browser-essays--annotations-for-section
                           manifest sid t))
                     (live (seq-remove
                            #'arxana-browser-essays--annotation-retracted-p all))
                     (retracted (- (length all) (length live)))
                     (count-str (if (> retracted 0)
                                    (format "%d(%d)" (length live) retracted)
                                  (format "%d" (length live)))))
                (list :type 'essays-section
                      :label (plist-get section :name)
                      :description
                      (if (> retracted 0)
                          (format "%d live annotation%s, %d retracted"
                                  (length live)
                                  (if (= (length live) 1) "" "s")
                                  retracted)
                        (format "%d annotation%s"
                                (length live)
                                (if (= (length live) 1) "" "s")))
                      :view 'essays-section
                      :essay-id essay-id
                      :section-id sid
                      :annotation-count count-str)))
            sections)
         (list (list :type 'info
                     :label "No sections in manifest"
                     :description "Run M-x arxana-browser-essays-import.")))))

    ('essays-section
     (let* ((essay-id (plist-get context :essay-id))
            (section-id (plist-get context :section-id))
            (manifest (arxana-browser-essays--manifest-for essay-id))
            (annotations (and manifest
                              (arxana-browser-essays--annotations-for-section
                               manifest section-id))))
       (if annotations
           (mapcar
            (lambda (ann)
              (let* ((annotated (plist-get ann :annotated))
                     (source (plist-get ann :source))
                     (hx (or (plist-get ann :hx-type) ""))
                     (hx-short (replace-regexp-in-string
                                "^annotation/" "" hx)))
                (list :type 'essays-annotation
                      :label (plist-get annotated :passage)
                      :description (plist-get ann :note)
                      :hx-type hx
                      :hx-type-short hx-short
                      :pattern-name (plist-get source :pattern-name)
                      :source-passage (plist-get source :passage)
                      :annotation-id (plist-get ann :id))))
            annotations)
         (list (list :type 'info
                     :label "No annotations on this section yet"
                     :description "Manifest has no entries targeting this section.")))))

    (_ nil)))

;;; ─────────────────────────────────────────────────────────────
;;; Section text extraction
;;; ─────────────────────────────────────────────────────────────

(defun arxana-browser-essays--catalog-source-file (essay-id)
  "Return the source-file path for the catalog matching ESSAY-ID.
Falls back to the manifest's :essay :source-file (resolved against
~/npt/working-paper/) if the catalog spec lacks :source-file."
  (let* ((cat (arxana-browser-essays--catalog-spec essay-id))
         (cat-path (and cat (plist-get cat :source-file))))
    (or cat-path
        (let* ((manifest (arxana-browser-essays--manifest-for essay-id))
               (essay (and manifest (plist-get manifest :essay)))
               (rel (and essay (plist-get essay :source-file))))
          (and rel (expand-file-name rel "~/npt/working-paper/"))))))

(defun arxana-browser-essays--section-heading-text (manifest section-id)
  "Return the heading-text prop for SECTION-ID in MANIFEST."
  (let ((sec (seq-find
              (lambda (s) (string= (plist-get s :id) section-id))
              (plist-get manifest :sections))))
    (and sec (alist-get 'heading-text (plist-get sec :props)))))

(defun arxana-browser-essays--extract-section-text (source-file heading-text)
  "Read SOURCE-FILE and return text under the markdown `## HEADING-TEXT`.
Allows an optional `N.` number prefix between `##` and HEADING-TEXT, so a
heading-text of \"The synthesis...\" matches `## 5. The synthesis...`.
Returns text from the `##` line up to (but not including) the next `## ` line."
  (when (and source-file (file-readable-p source-file) heading-text)
    (with-temp-buffer
      (insert-file-contents source-file)
      (goto-char (point-min))
      (let ((heading-re (format "^##[[:space:]]+\\(?:[0-9]+\\.[[:space:]]+\\)?%s"
                                (regexp-quote heading-text))))
        (when (re-search-forward heading-re nil t)
          (let ((start (match-beginning 0))
                (end (or (and (re-search-forward "^## " nil t)
                              (match-beginning 0))
                         (point-max))))
            (buffer-substring-no-properties start end)))))))

(defun arxana-browser-essays--locate-passage-bounds (passage)
  "Search for PASSAGE in the current buffer; return (BEG . END) or nil.
Falls back to a fuzzy first-N-chars search if exact match not found."
  (when (and passage (stringp passage) (not (string-empty-p passage)))
    (save-excursion
      (goto-char (point-min))
      (cond
       ((search-forward passage nil t)
        (cons (match-beginning 0) (match-end 0)))
       ;; Fuzzy: try first 60 chars (passages with ellipses or quoting issues).
       ((let ((short (substring passage 0 (min 60 (length passage)))))
          (goto-char (point-min))
          (when (search-forward short nil t)
            (cons (match-beginning 0)
                  (min (point-max) (+ (match-beginning 0) (length passage)))))))))))

(defun arxana-browser-essays--longest-prefix-match (passage lengths)
  "Return (POS . LEN) for the longest prefix of PASSAGE of one of LENGTHS found.
LENGTHS is a descending list of candidate lengths.  POS is the buffer
position of the prefix match's beginning; LEN is the prefix length that
matched.  Returns nil if no listed prefix length matches."
  (let ((result nil))
    (dolist (n lengths)
      (when (and (null result) (<= n (length passage)))
        (let* ((prefix (substring passage 0 n))
               (pos (save-excursion
                      (goto-char (point-min))
                      (and (search-forward prefix nil t)
                           (match-beginning 0)))))
          (when pos (setq result (cons pos n))))))
    result))

(defun arxana-browser-essays--longest-suffix-match (passage lengths)
  "Return (POS . LEN) for the longest suffix of PASSAGE of one of LENGTHS found.
POS is the buffer position at the suffix match's end; LEN is the suffix
length that matched.  Returns nil if no listed suffix length matches."
  (let ((result nil)
        (plen (length passage)))
    (dolist (n lengths)
      (when (and (null result) (<= n plen))
        (let* ((suffix (substring passage (- plen n)))
               (pos (save-excursion
                      (goto-char (point-min))
                      (and (search-forward suffix nil t)
                           (match-end 0)))))
          (when pos (setq result (cons pos n))))))
    result))

(defun arxana-browser-essays--locate-passage-bounds-lax (passage)
  "Locate PASSAGE in the current buffer with graded fallbacks.
Returns a plist (:bounds (BEG . END) :quality SYM) where SYM is one of
`exact', `bracket-PL-SL' (PL=matched prefix length, SL=suffix length),
`prefix-PL', or `suffix-SL'; or nil if no reasonable anchor was found.

Strategy: try exact match first.  Otherwise walk a descending length
ladder (60/40/20/10/5) to find the longest matching prefix and longest
matching suffix of PASSAGE in the buffer.  If both match with a
reasonable span (< 2× passage length), bracket them.  Otherwise fall
back to one-sided anchoring, expanding forward/back by passage length
and snapping to the nearest whitespace boundary to avoid mid-word
anchors.

For non-`exact' quality, the caller is expected to rewrite the manifest
to the buffer text between BEG and END so that subsequent renders
anchor exactly."
  (when (and passage (stringp passage) (not (string-empty-p passage)))
    (let ((len (length passage))
          (ladder '(60 40 20 10 5)))
      (save-excursion
        (goto-char (point-min))
        (if (search-forward passage nil t)
            (list :bounds (cons (match-beginning 0) (match-end 0))
                  :quality 'exact)
          (let* ((pm (arxana-browser-essays--longest-prefix-match passage ladder))
                 (sm (arxana-browser-essays--longest-suffix-match passage ladder)))
            (cond
             ((and pm sm
                   (< (car pm) (car sm))
                   (< (- (car sm) (car pm)) (* 2 len)))
              (list :bounds (cons (car pm) (car sm))
                    :quality (intern (format "bracket-%d-%d" (cdr pm) (cdr sm)))))
             (pm
              (let* ((start (car pm))
                     (end-guess (min (point-max) (+ start len)))
                     (end (save-excursion
                            (goto-char end-guess)
                            (skip-chars-forward "^\n \t" (min (point-max)
                                                                (+ end-guess 30)))
                            (point))))
                (list :bounds (cons start end)
                      :quality (intern (format "prefix-%d" (cdr pm))))))
             (sm
              (let* ((end (car sm))
                     (start-guess (max (point-min) (- end len)))
                     (start (save-excursion
                              (goto-char start-guess)
                              (skip-chars-backward "^\n \t"
                                                   (max (point-min)
                                                        (- start-guess 30)))
                              (skip-chars-forward " \t")
                              (point))))
                (list :bounds (cons start end)
                      :quality (intern (format "suffix-%d" (cdr sm)))))))))))))

(defun arxana-browser-essays--manifest-file-for-annotation (ann-id)
  "Return the path to the manifest file containing ANN-ID, or nil."
  (seq-find
   (lambda (path)
     (and (file-readable-p path)
          (with-temp-buffer
            (insert-file-contents path)
            (goto-char (point-min))
            (search-forward (format ":id %S" ann-id) nil t))))
   arxana-browser-essays-manifest-files))

(defun arxana-browser-essays--rewrite-manifest-passage (manifest-file ann-id new-passage)
  "In MANIFEST-FILE, rewrite the annotation with ANN-ID to use NEW-PASSAGE.
Returns the previous passage if the file was changed, nil if the passage
already equalled NEW-PASSAGE.  Raises on id-not-found or malformed form.
Textual edit inside the annotation's S-expression: locates `:id \"ID\"',
climbs to the enclosing form, and replaces the single `:passage' string
within that form.  Preserves comments and all other formatting."
  (let ((previous nil)
        (changed nil))
    (with-temp-buffer
      (insert-file-contents manifest-file)
      (goto-char (point-min))
      (unless (search-forward (format ":id %S" ann-id) nil t)
        (error "[essays] Annotation id %s not found in %s"
               ann-id manifest-file))
      (let ((id-pos (match-beginning 0)))
        (goto-char id-pos)
        (backward-up-list)
        (let ((form-start (point))
              (form-end (save-excursion (forward-sexp) (point))))
          (goto-char form-start)
          (unless (re-search-forward ":passage[[:space:]]+" form-end t)
            (error "[essays] No :passage in annotation %s" ann-id))
          (let* ((str-start (point))
                 (str-end (progn (forward-sexp) (point)))
                 (current (read (buffer-substring str-start str-end))))
            (setq previous current)
            (unless (string= current new-passage)
              (delete-region str-start str-end)
              (goto-char str-start)
              (prin1 new-passage (current-buffer))
              (setq changed t)))))
      (when changed
        (write-region (point-min) (point-max) manifest-file nil 'silent)))
    (and changed previous)))

;;; ─────────────────────────────────────────────────────────────
;;; Reading-view rendering
;;; ─────────────────────────────────────────────────────────────

;; Buffer-local indices and overlays for the cross-buffer sync.
(defvar-local arxana-browser-essays--note-index nil
  "Hash table mapping annotation-id -> (start-marker . end-marker) in notes buffer.")
(defvar-local arxana-browser-essays--source-index nil
  "Hash table mapping annotation-id -> (start . end) in text buffer.")
(defvar-local arxana-browser-essays--note-highlight-overlay nil
  "Overlay highlighting the currently-active note in notes buffer.")
(defvar-local arxana-browser-essays--text-highlight-overlay nil
  "Overlay highlighting the currently-active passage in text buffer.")
(defvar-local arxana-browser-essays--last-active-id nil
  "Most recently activated annotation-id (across buffers).")

;; Buffer-locals used by in-place edit mode (see
;; `arxana-browser-essays-edit-mode' further down).  Set by the renderer
;; so the edit/save path knows which slice of which source file it is
;; editing.
(defvar-local arxana-browser-essays--source-file nil
  "Absolute path to the markdown file backing the current text buffer.")
(defvar-local arxana-browser-essays--heading-text nil
  "Heading text of the current section (matches the markdown `## ' line).")
(defvar-local arxana-browser-essays--essay-id nil
  "Essay id for the current text buffer; used on re-render after save.")
(defvar-local arxana-browser-essays--section-id nil
  "Section id for the current text buffer; used on re-render after save.")
(defvar-local arxana-browser-essays--section-name nil
  "Display name of the current section; used on re-render after save.")
(defvar-local arxana-browser-essays--content-start nil
  "Marker at the start of section content (past the \"Section: …\" header).
Everything from this marker to `point-max' is the user-editable region;
anything before it is the injected read-view banner.")
(defvar-local arxana-browser-essays--unresolved nil
  "List of annotation plists whose :passage could not be located on render.
Populated by `--render-section-text' after exhausting the lax fuzzy
search ladder; diagnostic only — the renderer just logs the count.")

(defun arxana-browser-essays--annotation-id-at-point ()
  "Return the annotation-id of the overlay at point in current buffer, or nil."
  (let ((ov (seq-find
             (lambda (o) (overlay-get o 'arxana-essay-annotation-id))
             (overlays-at (point)))))
    (and ov (overlay-get ov 'arxana-essay-annotation-id))))

(defun arxana-browser-essays--annotation-id-in-notes-at-point ()
  "Return the annotation-id of the notes entry containing point, or nil."
  (get-text-property (point) 'arxana-essay-annotation-id))

(defun arxana-browser-essays--highlight-bounds-in-buffer (buf bounds overlay-var)
  "Move/create OVERLAY-VAR in BUF to BOUNDS and recenter the visible window."
  (when (and (buffer-live-p buf) bounds (consp bounds))
    (with-current-buffer buf
      (unless (overlayp (symbol-value overlay-var))
        (set overlay-var (make-overlay (car bounds) (cdr bounds)))
        (overlay-put (symbol-value overlay-var) 'face
                     'arxana-browser-essays-active-face)
        (overlay-put (symbol-value overlay-var) 'priority 30))
      (move-overlay (symbol-value overlay-var) (car bounds) (cdr bounds))
      (let ((win (get-buffer-window buf t)))
        (when (window-live-p win)
          (with-selected-window win
            (goto-char (car bounds))
            (recenter)))))))

(defun arxana-browser-essays--clear-highlight-in-buffer (buf overlay-var)
  "Delete OVERLAY-VAR overlay in BUF if it exists."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (overlayp (symbol-value overlay-var))
        (delete-overlay (symbol-value overlay-var))
        (set overlay-var nil)))))

(defun arxana-browser-essays--reconcile-overlays ()
  "Reconcile annotation overlays with text-property runs of the same id.
Render time writes `arxana-essay-annotation-id' and
`arxana-essay-annotation-index' as text properties on each annotated
passage, mirroring the overlay.  Kill and yank carry the text
properties through the kill ring natively; the overlay does not
follow.  This pass finds any contiguous text-property run whose
overlay is absent or mis-placed and creates or moves the overlay to
match, so a cut-paste of an annotated region inside the buffer keeps
its face + `[N]' marker."
  (save-excursion
    (let ((runs (make-hash-table :test 'equal))
          (indices (make-hash-table :test 'equal))
          (pos (point-min)))
      (while (< pos (point-max))
        (let* ((id (get-text-property pos 'arxana-essay-annotation-id))
               (idx (get-text-property pos 'arxana-essay-annotation-index))
               (end (next-single-property-change
                     pos 'arxana-essay-annotation-id nil (point-max))))
          (when (and id (not (gethash id runs)))
            (puthash id (cons pos end) runs)
            (when idx (puthash id idx indices)))
          (setq pos end)))
      (let ((existing (make-hash-table :test 'equal)))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (let ((id (overlay-get ov 'arxana-essay-annotation-id)))
            (when id (puthash id ov existing))))
        (maphash
         (lambda (id bounds)
           (let* ((beg (car bounds))
                  (end (cdr bounds))
                  (ov (gethash id existing))
                  (idx (gethash id indices)))
             (cond
              ((and ov
                    (= (overlay-start ov) beg)
                    (= (overlay-end ov) end))
               nil)
              (ov
               (move-overlay ov beg end))
              (t
               (let ((new (make-overlay beg end)))
                 (overlay-put new 'face 'arxana-browser-essays-annotation-face)
                 (overlay-put new 'arxana-essay-annotation-id id)
                 (overlay-put new 'priority 10)
                 (overlay-put new 'evaporate t)
                 (when idx
                   (overlay-put new 'after-string
                                (propertize
                                 (format " [%d]" idx)
                                 'face 'arxana-browser-essays-marker-face))))))))
         runs)))))

(defun arxana-browser-essays--retracted-ids-in-this-section ()
  "Return annotation-ids in the manifest for this section with no live overlay.
`Dead' here means the id appears in the loaded manifest under the
current section but no buffer overlay carries that id — either because
the overlay evaporated (`evaporate t' on region delete) or because the
render loop couldn't anchor it.  Used to decorate the notes buffer in
real time (strikethrough) so Joe can see which annotations will be
retracted on the next save."
  (let* ((essay-id arxana-browser-essays--essay-id)
         (section-id arxana-browser-essays--section-id)
         (manifest (and essay-id
                        (arxana-browser-essays--manifest-for essay-id)))
         (section-anns (and manifest
                            (arxana-browser-essays--annotations-for-section
                             manifest section-id)))
         (expected-ids (mapcar (lambda (a) (plist-get a :id)) section-anns))
         (live-ids (let (acc)
                     (dolist (ov (overlays-in (point-min) (point-max)))
                       (let ((id (overlay-get ov 'arxana-essay-annotation-id)))
                         (when (and id
                                    (> (overlay-end ov) (overlay-start ov)))
                           (push id acc))))
                     acc)))
    (cl-set-difference expected-ids live-ids :test #'equal)))

(defun arxana-browser-essays--refresh-notes-retractions ()
  "Apply strikethrough to notes-buffer entries whose overlay has died.
Called from the text buffer's post-command hook.  Uses
`--retracted-ids-in-this-section' to find annotations with no live
overlay and decorates the corresponding notes entry with a
`arxana-essays-retracted' overlay.  Clears previous decorations each
pass so resurrecting an annotation (undo) unmarks it.

Also clears the point-tracking active-highlight overlay in the text
buffer when it sits on a span whose annotation has died — prevents a
stranded highlight where the annotation used to live."
  (let* ((notes-buf (get-buffer arxana-browser-essays-notes-buffer))
         (dead-ids (arxana-browser-essays--retracted-ids-in-this-section)))
    ;; Text-buffer: drop the active-highlight if its annotation is gone.
    (when (and dead-ids
               (overlayp arxana-browser-essays--text-highlight-overlay))
      (let* ((hov arxana-browser-essays--text-highlight-overlay)
             (hov-start (overlay-start hov))
             (hov-end (overlay-end hov))
             (overlaps-live
              (and hov-start hov-end
                   (seq-some
                    (lambda (ov)
                      (and (overlay-get ov 'arxana-essay-annotation-id)
                           (> (overlay-end ov) (overlay-start ov))
                           (<= (overlay-start ov) hov-start)
                           (<= hov-end (overlay-end ov))))
                    (overlays-in hov-start hov-end)))))
        (unless overlaps-live
          (delete-overlay hov)
          (setq arxana-browser-essays--text-highlight-overlay nil))))
    ;; Notes-buffer: mark retracted entries with strikethrough.
    (when (and notes-buf (buffer-live-p notes-buf))
      (with-current-buffer notes-buf
        (remove-overlays (point-min) (point-max)
                         'arxana-essays-retracted t)
        (when (hash-table-p arxana-browser-essays--note-index)
          (dolist (id dead-ids)
            (let* ((bounds (gethash id arxana-browser-essays--note-index))
                   (beg (and bounds (marker-position (car bounds))))
                   (end (and bounds (marker-position (cdr bounds)))))
              (when (and beg end (< beg end))
                (let ((ov (make-overlay beg end)))
                  (overlay-put ov 'face
                               '(:strike-through t :foreground "#888"))
                  (overlay-put ov 'arxana-essays-retracted t)
                  (overlay-put ov 'priority 20)
                  (overlay-put ov 'help-echo
                               "Retracted — removed on next save"))))))))))

(defun arxana-browser-essays--sync-notes-from-text ()
  "Post-command-hook for text buffer: highlight matching note from point."
  (let ((id (arxana-browser-essays--annotation-id-at-point))
        (notes-buf (get-buffer arxana-browser-essays-notes-buffer)))
    (unless (equal id arxana-browser-essays--last-active-id)
      (setq arxana-browser-essays--last-active-id id)
      (if (and id notes-buf)
          (with-current-buffer notes-buf
            (when (hash-table-p arxana-browser-essays--note-index)
              (let ((bounds (gethash id arxana-browser-essays--note-index)))
                (if bounds
                    (arxana-browser-essays--highlight-bounds-in-buffer
                     notes-buf
                     (cons (marker-position (car bounds))
                           (marker-position (cdr bounds)))
                     'arxana-browser-essays--note-highlight-overlay)
                  (arxana-browser-essays--clear-highlight-in-buffer
                   notes-buf 'arxana-browser-essays--note-highlight-overlay)))))
        (when notes-buf
          (arxana-browser-essays--clear-highlight-in-buffer
           notes-buf 'arxana-browser-essays--note-highlight-overlay))))))

(defun arxana-browser-essays--sync-text-from-notes ()
  "Post-command-hook for notes buffer: highlight matching text passage from point."
  (let ((id (arxana-browser-essays--annotation-id-in-notes-at-point))
        (text-buf (get-buffer arxana-browser-essays-text-buffer)))
    (unless (equal id arxana-browser-essays--last-active-id)
      (setq arxana-browser-essays--last-active-id id)
      (if (and id text-buf)
          (with-current-buffer text-buf
            (when (hash-table-p arxana-browser-essays--source-index)
              (let ((bounds (gethash id arxana-browser-essays--source-index)))
                (if bounds
                    (arxana-browser-essays--highlight-bounds-in-buffer
                     text-buf bounds
                     'arxana-browser-essays--text-highlight-overlay)
                  (arxana-browser-essays--clear-highlight-in-buffer
                   text-buf 'arxana-browser-essays--text-highlight-overlay)))))
        (when text-buf
          (arxana-browser-essays--clear-highlight-in-buffer
           text-buf 'arxana-browser-essays--text-highlight-overlay))))))

(defun arxana-browser-essays-left-or-return ()
  "Move point left; at point-min, return to the prior window layout.
Mirrors `arxana-browser-songs-left-or-return'.  Restores the window
configuration captured at `arxana-browser-essays-open' time, falling
through to `arxana-ui-left-or-return' if no local return is set."
  (interactive)
  (cond
   ((> (point) (point-min))
    (backward-char))
   ((and (boundp 'arxana-ui-return-window-config)
         (window-configuration-p arxana-ui-return-window-config))
    (set-window-configuration arxana-ui-return-window-config))
   ((and (boundp 'arxana-ui-return-buffer)
         (buffer-live-p arxana-ui-return-buffer))
    (set-window-buffer (selected-window) arxana-ui-return-buffer))
   ((fboundp 'arxana-ui-left-or-return)
    (arxana-ui-left-or-return))
   (t (message "No return target available"))))

(defvar arxana-browser-essays-text-sync-mode-map nil
  "Keymap for `arxana-browser-essays-text-sync-mode'.")

(setq arxana-browser-essays-text-sync-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<left>") #'arxana-browser-essays-left-or-return)
        map))

(define-minor-mode arxana-browser-essays-text-sync-mode
  "Minor mode in *Arxana Essay* that syncs notes-buffer highlight on point move."
  :init-value nil
  :keymap arxana-browser-essays-text-sync-mode-map
  (cond
   (arxana-browser-essays-text-sync-mode
    (add-hook 'post-command-hook
              #'arxana-browser-essays--reconcile-overlays nil t)
    (add-hook 'post-command-hook
              #'arxana-browser-essays--sync-notes-from-text nil t)
    (add-hook 'post-command-hook
              #'arxana-browser-essays--refresh-notes-retractions nil t))
   (t
    (remove-hook 'post-command-hook
                 #'arxana-browser-essays--reconcile-overlays t)
    (remove-hook 'post-command-hook
                 #'arxana-browser-essays--sync-notes-from-text t)
    (remove-hook 'post-command-hook
                 #'arxana-browser-essays--refresh-notes-retractions t))))

(define-minor-mode arxana-browser-essays-notes-sync-mode
  "Minor mode in *Arxana Essay Notes* that syncs text-buffer highlight on point move."
  :init-value nil
  (if arxana-browser-essays-notes-sync-mode
      (add-hook 'post-command-hook
                #'arxana-browser-essays--sync-text-from-notes nil t)
    (remove-hook 'post-command-hook
                 #'arxana-browser-essays--sync-text-from-notes t)))

;;; ─────────────────────────────────────────────────────────────
;;; In-place edit mode (pathway 1: essay text edits)
;;; ─────────────────────────────────────────────────────────────

(defun arxana-browser-essays-delete-region-or-backward-char (&optional arg)
  "Delete active region if any, else delete char backward.
Stand-in for `delete-selection-mode' behaviour within the essay edit
buffer — Joe's global setup runs with that mode off, but within an
essay the region-delete-on-typing intuition is what the edit flow
expects (yank-over or selection-delete cleanly collapses annotation
overlays, and the save pipeline retracts the dead ones)."
  (interactive "p")
  (cond
   ((and (region-active-p) (not buffer-read-only))
    (delete-region (region-beginning) (region-end)))
   (t
    (backward-delete-char (or arg 1)))))

(defun arxana-browser-essays-delete-region-or-forward-char (&optional arg)
  "Delete active region if any, else delete char forward.
Partner to `arxana-browser-essays-delete-region-or-backward-char' for
<deletechar> / C-d within the edit buffer."
  (interactive "p")
  (cond
   ((and (region-active-p) (not buffer-read-only))
    (delete-region (region-beginning) (region-end)))
   (t
    (delete-char (or arg 1)))))

(defvar arxana-browser-essays-edit-mode-map nil
  "Keymap active when `arxana-browser-essays-edit-mode' is on.")

;; Build (and rebuild on reload) the edit-mode keymap via `setq' so that
;; adding new bindings during development actually takes effect — a plain
;; `defvar' with an initialiser skips re-initialisation when the variable
;; is already bound, and the old keymap silently persists.
(setq arxana-browser-essays-edit-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c C-c") #'arxana-browser-essays-save-section)
        (define-key map (kbd "C-c C-s") #'arxana-browser-essays-save-section)
        (define-key map (kbd "C-c C-k") #'arxana-browser-essays-abort-edit)
        (define-key map (kbd "DEL")
                    #'arxana-browser-essays-delete-region-or-backward-char)
        (define-key map (kbd "<backspace>")
                    #'arxana-browser-essays-delete-region-or-backward-char)
        (define-key map (kbd "<deletechar>")
                    #'arxana-browser-essays-delete-region-or-forward-char)
        (define-key map (kbd "C-d")
                    #'arxana-browser-essays-delete-region-or-forward-char)
        (define-key map (kbd "C-c c")
                    #'arxana-browser-essays-add-comment)
        map))

(define-minor-mode arxana-browser-essays-edit-mode
  "Toggle in-place editing of the annotated essay text buffer.

When enabled, `view-mode' is disabled and the buffer becomes writable.
Annotation overlays move with edits (insertion stays inside the overlay,
deletions shrink it).  Use the following keys to end the edit session:

  \\[arxana-browser-essays-save-section]  save section back to the source file
  \\[arxana-browser-essays-abort-edit]    abort edits and re-render from disk

The injected `Section: …' header at the top of the buffer is not written
back to disk — only the region from the `## HEADING' line onward is
replaced in the source file."
  :init-value nil
  :lighter " Essay-Edit"
  :keymap arxana-browser-essays-edit-mode-map
  (cond
   (arxana-browser-essays-edit-mode
    (unless arxana-browser-essays--source-file
      (setq arxana-browser-essays-edit-mode nil)
      (user-error
       "No :source-file on this buffer; reopen via the browser first"))
    (when (bound-and-true-p view-mode)
      (view-mode -1))
    (setq buffer-read-only nil))
   (t
    (setq buffer-read-only t)
    (unless (bound-and-true-p view-mode)
      (view-mode 1)))))

(defun arxana-browser-essays--editable-content ()
  "Return the current editable slice of the text buffer, or signal.
The slice runs from `arxana-browser-essays--content-start' to
`point-max'.  The first non-blank line must be a `## ' markdown heading
matching `arxana-browser-essays--heading-text', else we refuse to save."
  (unless (and arxana-browser-essays--content-start
               (marker-position arxana-browser-essays--content-start))
    (user-error "No content-start marker; cannot determine editable region"))
  (let* ((content (buffer-substring-no-properties
                   arxana-browser-essays--content-start (point-max)))
         (heading arxana-browser-essays--heading-text)
         (heading-re (format "\\`[[:space:]\n]*##[[:space:]]+\\(?:[0-9]+\\.[[:space:]]+\\)?%s"
                             (regexp-quote heading))))
    (unless (string-match-p heading-re content)
      (user-error
       "Edit region no longer starts with `## %s'; aborting save" heading))
    content))

(defun arxana-browser-essays--replace-section-in-file (source-file heading-text new-content)
  "In SOURCE-FILE, replace the section with HEADING-TEXT by NEW-CONTENT.
Section boundaries match `--extract-section-text': from the `##' line
matching HEADING-TEXT (with optional `N.' prefix) up to the next `## '
heading or end of file.  Writes the file back on success.  Returns a
plist: (:changed BOOL :backup PATH) where BACKUP is the pre-edit file
saved to SOURCE-FILE~arxana~."
  (unless (and source-file (file-writable-p source-file))
    (user-error "Source file not writable: %s" source-file))
  (let* ((backup (concat source-file "~arxana~"))
         (original (with-temp-buffer
                     (insert-file-contents source-file)
                     (buffer-string)))
         (heading-re (format "^##[[:space:]]+\\(?:[0-9]+\\.[[:space:]]+\\)?%s"
                             (regexp-quote heading-text))))
    (copy-file source-file backup t)
    (with-temp-buffer
      (insert original)
      (goto-char (point-min))
      (unless (re-search-forward heading-re nil t)
        (user-error "Could not find heading `## %s' in %s"
                    heading-text source-file))
      (let ((start (match-beginning 0))
            (end (or (and (save-excursion
                            (goto-char (match-end 0))
                            (re-search-forward "^## " nil t))
                          (match-beginning 0))
                     (point-max))))
        (delete-region start end)
        (goto-char start)
        (insert new-content)
        ;; Ensure trailing newline before the next section (or EOF).
        (unless (or (eobp) (eq (char-before) ?\n))
          (insert "\n")))
      (write-region (point-min) (point-max) source-file nil 'silent))
    (list :changed (not (string= original
                                  (with-temp-buffer
                                    (insert-file-contents source-file)
                                    (buffer-string))))
          :backup backup)))

(defun arxana-browser-essays--append-manifest-annotation (manifest-file annotation)
  "Append ANNOTATION (a plist) to the `:annotations' list in MANIFEST-FILE.
Locates the `:annotations' keyword, walks forward to the enclosing
parenthesised list, and inserts ANNOTATION just before the closing
paren as a new element.  Pretty-prints the inserted form with a blank
line of separation to match the file's hand-authored structure."
  (unless (file-writable-p manifest-file)
    (user-error "Manifest not writable: %s" manifest-file))
  (with-temp-buffer
    (insert-file-contents manifest-file)
    (goto-char (point-min))
    (unless (re-search-forward ":annotations" nil t)
      (user-error "No :annotations keyword in %s" manifest-file))
    ;; Advance to the opening paren of the annotations list.
    (skip-chars-forward " \t\n")
    (unless (looking-at "(")
      (user-error "Expected `(' after :annotations in %s" manifest-file))
    (let ((list-start (point))
          (list-end (save-excursion (forward-sexp) (point))))
      (goto-char list-end)
      (backward-char)                          ; position at close-paren
      (skip-chars-backward " \t\n")
      (insert "\n\n     ")
      (let ((pp-escape-newlines nil))
        (insert (pp-to-string annotation)))
      ;; pp often adds trailing newline; tidy indentation before close.
      (skip-chars-backward " \t\n")
      (insert "\n    ")
      (ignore list-start))
    (write-region (point-min) (point-max) manifest-file nil 'silent))
  t)

(defun arxana-browser-essays--mark-manifest-annotation-retracted (manifest-file ann-id)
  "Mark annotation ANN-ID as retracted in MANIFEST-FILE (`:retracted t').
Retracted annotations remain in the manifest — they are hypergraph
citizens with provenance — but render and count paths filter them out
of live views.  Returns t if the file was changed; nil if the
annotation was already marked retracted or wasn't found.

Preserves all other fields and the file's hand-authored structure."
  (let ((changed nil))
    (with-temp-buffer
      (insert-file-contents manifest-file)
      (goto-char (point-min))
      (when (search-forward (format ":id %S" ann-id) nil t)
        (goto-char (match-beginning 0))
        (backward-up-list)
        (let ((form-start (point))
              (form-end (save-excursion (forward-sexp) (point))))
          (goto-char form-start)
          (unless (re-search-forward ":retracted[[:space:]]+t" form-end t)
            ;; Insert ` :retracted t` just inside the form's closing
            ;; paren, on its own line with the same indent as :id.
            (goto-char form-end)
            (backward-char)                     ; position at close-paren
            (skip-chars-backward " \t\n")
            (insert "\n      :retracted t")
            (setq changed t))))
      (when changed
        (write-region (point-min) (point-max) manifest-file nil 'silent)))
    changed))

(defun arxana-browser-essays--dead-overlay-ids ()
  "Return annotation-ids whose overlays in the current buffer have zero length.
A zero-length overlay is the fingerprint of an annotated passage that
was deleted or wholly overwritten (e.g. by a region-replacing yank).
Used by the save pipeline to retract such annotations automatically."
  (let (dead)
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((id (overlay-get ov 'arxana-essay-annotation-id)))
        (when (and id
                   (= (overlay-start ov) (overlay-end ov)))
          (push id dead))))
    (nreverse dead)))

;;;###autoload
(defun arxana-browser-essays-cleanup-stale-overlays ()
  "Remove zero-length annotation overlays and orphan marker overlays.
Two sources of visual debris:
  1. Zero-length annotation overlays (id present, span empty) left over
     after a region-delete / yank-over.  Under `evaporate' these clean
     themselves up on further buffer modification, but an undo or a
     transient state can leave them visible.
  2. Orphan marker overlays — zero-width overlays carrying only an
     `after-string' `[N]' marker, usually left from a prior render
     (the legacy two-overlay-per-annotation scheme).

Does not modify the manifest or touch XTDB — it only cleans display."
  (interactive)
  (let ((removed 0))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((id (overlay-get ov 'arxana-essay-annotation-id))
            (after (overlay-get ov 'after-string))
            (len (- (overlay-end ov) (overlay-start ov))))
        (when (or (and id (= len 0))
                  (and (null id) after (= len 0)))
          (delete-overlay ov)
          (cl-incf removed))))
    (when (called-interactively-p 'interactive)
      (message "[essays] Removed %d stale overlay(s)" removed))
    removed))

(defun arxana-browser-essays--backup-manifest-files ()
  "Copy each configured manifest file to FILE~arxana~.
Returns an alist ((PATH . BACKUP-PATH) ...) for the files actually
backed up (readable, size > 0).  Called before any sync that may
mutate manifests, so the backup always reflects the pre-save state."
  (delq nil
        (mapcar
         (lambda (path)
           (when (and (file-readable-p path)
                      (> (file-attribute-size (file-attributes path)) 0))
             (let ((backup (concat path "~arxana~")))
               (copy-file path backup t)
               (cons path backup))))
         arxana-browser-essays-manifest-files)))

(defun arxana-browser-essays--sync-overlays-to-manifest ()
  "Walk annotation overlays in the current text buffer and sync text to manifest.
For each annotation overlay whose current text differs from the
manifest's stored `:passage' for that id, rewrites the manifest file in
place.  Returns a list of annotation-ids whose passages changed (in
render order).  Called from `arxana-browser-essays-save-section' before
the markdown writeback so that live edits inside overlaid regions flow
into the manifest automatically."
  (let ((updated-ids nil)
        (manifest-ann-by-id
         (let ((h (make-hash-table :test 'equal)))
           (dolist (m (arxana-browser-essays--all-manifests))
             (dolist (a (plist-get m :annotations))
               (puthash (plist-get a :id) a h)))
           h)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((id (overlay-get ov 'arxana-essay-annotation-id)))
        (when id
          (let* ((ann (gethash id manifest-ann-by-id))
                 (stored (and ann (plist-get (plist-get ann :annotated) :passage)))
                 (current (buffer-substring-no-properties
                           (overlay-start ov) (overlay-end ov)))
                 (path (and ann
                            (arxana-browser-essays--manifest-file-for-annotation id))))
            (when (and stored path (not (string= stored current)))
              (arxana-browser-essays--rewrite-manifest-passage path id current)
              (push id updated-ids)
              (message "[essays] Synced %s: manifest passage updated (%d→%d chars)"
                       id (length stored) (length current)))))))
    (when updated-ids
      (arxana-browser-essays-refresh))
    (nreverse updated-ids)))

(defun arxana-browser-essays--sync-annotations-to-xtdb (ann-ids)
  "Upsert the annotations in ANN-IDS to XTDB (futon1a).
Re-reads the manifest post-rewrite so the upserted passages match the
Elisp file state.  For each annotation, ensures the parent section
entity exists, then calls `--create-annotation' (which upserts the
annotation hyperedge).  XTDB records each upsert as a new transaction,
giving us versioned history for future checkpoint/restore.

Errors during individual annotation upserts are logged and non-fatal —
a failed XTDB write should not block the markdown save, since the
manifest file remains authoritative for the next render."
  (when ann-ids
    (cond
     ((not (arxana-store-sync-enabled-p))
      (message "[essays] XTDB sync skipped (futon sync disabled)"))
     (t
      (let ((by-id (let ((h (make-hash-table :test 'equal)))
                     (dolist (m (arxana-browser-essays--all-manifests))
                       (dolist (a (plist-get m :annotations))
                         (puthash (plist-get a :id) a h)))
                     h))
            (section-by-id
             (let ((h (make-hash-table :test 'equal)))
               (dolist (m (arxana-browser-essays--all-manifests))
                 (dolist (s (plist-get m :sections))
                   (puthash (plist-get s :id) s h)))
               h))
            (sections-ensured (make-hash-table :test 'equal))
            (ok 0)
            (fail 0))
        (dolist (id ann-ids)
          (let* ((ann (gethash id by-id))
                 (sec-id (and ann
                              (plist-get (plist-get ann :annotated)
                                         :entity-id)))
                 (sec (and sec-id (gethash sec-id section-by-id))))
            (cond
             ((null ann)
              (message "[essays] XTDB sync: %s not in manifest" id)
              (cl-incf fail))
             ;; Comments have no pattern source — skip XTDB upsert for
             ;; now (deferred in the mission doc).  Manifest-only.
             ((string= (plist-get ann :hx-type) "annotation/comment")
              nil)
             (t
              (when (and sec (not (gethash sec-id sections-ensured)))
                (condition-case err
                    (progn
                      (arxana-browser-essays--ensure-section sec nil)
                      (puthash sec-id t sections-ensured))
                  (error
                   (message "[essays] XTDB sync: ensure-section %s: %s"
                            sec-id (error-message-string err)))))
              (condition-case err
                  (progn
                    (arxana-browser-essays--create-annotation ann nil)
                    (cl-incf ok))
                (error
                 (message "[essays] XTDB sync: %s failed: %s"
                          id (error-message-string err))
                 (cl-incf fail)))))))
        (message "[essays] XTDB sync: %d ok / %d fail" ok fail)
        (list :ok ok :fail fail))))))

(defun arxana-browser-essays-save-section ()
  "Save the current *Arxana Essay* buffer section back to its source file.
Before writing, syncs each live annotation overlay's current text back
to the manifest (so inline edits inside overlaid passages persist as
updated `:passage' values).  Then replaces the section in
`arxana-browser-essays--source-file' bounded by the heading
`## HEADING-TEXT' with the current editable content, and re-opens the
section to refresh overlays — which also triggers the render-time lax
re-anchor pass for any annotation whose bounds are now stale.  Leaves a
`~arxana~' backup next to the source file."
  (interactive)
  (unless arxana-browser-essays-edit-mode
    (user-error "Not in `arxana-browser-essays-edit-mode'"))
  (let* ((content (arxana-browser-essays--editable-content))
         (source arxana-browser-essays--source-file)
         (heading arxana-browser-essays--heading-text)
         (essay-id arxana-browser-essays--essay-id)
         (section-id arxana-browser-essays--section-id)
         (section-name arxana-browser-essays--section-name)
         (saved-point (point))
         (saved-window-start (window-start))
         ;; Snapshot manifests BEFORE the sync pass — overlay-to-passage
         ;; rewriting can move annotations around in unexpected ways when
         ;; edits cross paragraph boundaries; the ~arxana~ sibling gives
         ;; us a pre-save state to roll back to via
         ;; `arxana-browser-essays-undo-last-save'.
         (_manifest-backups (arxana-browser-essays--backup-manifest-files))
         ;; Identify annotations that no longer have a live overlay in
         ;; this section.  `evaporate t' deletes the overlay outright
         ;; when its span collapses, so we can't rely on zero-length
         ;; overlays — instead we diff the manifest (live entries only)
         ;; against current overlays to catch both zero-length AND
         ;; already-evaporated cases.
         (dead-ids (arxana-browser-essays--retracted-ids-in-this-section))
         (retracted
          (let ((n 0))
            (dolist (id dead-ids)
              (let ((path (arxana-browser-essays--manifest-file-for-annotation id)))
                (when path
                  (when (arxana-browser-essays--mark-manifest-annotation-retracted
                         path id)
                    (cl-incf n)
                    (message "[essays] Marked %s retracted (overlay was empty)" id)))))
            (when (> n 0)
              (arxana-browser-essays-refresh))
            n))
         (updated-ids (arxana-browser-essays--sync-overlays-to-manifest))
         (synced (length updated-ids))
         ;; Push both the passage-updated and the newly-retracted
         ;; annotations to XTDB so the retraction flag is persisted as a
         ;; hyperedge label/prop (not just a manifest-local marker).
         (xtdb (arxana-browser-essays--sync-annotations-to-xtdb
                (delete-dups (append updated-ids dead-ids))))
         (result (arxana-browser-essays--replace-section-in-file
                  source heading content)))
    (when (and essay-id section-id section-name)
      (arxana-browser-essays--open-section essay-id section-id section-name)
      (let* ((buf (get-buffer arxana-browser-essays-text-buffer))
             (win (and buf (get-buffer-window buf t))))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            ;; `--render-section-text' re-enables `view-mode' and makes
            ;; the buffer read-only.  Restore the edit-mode writable
            ;; state so Joe can keep typing after C-c C-c.
            (when (bound-and-true-p view-mode) (view-mode -1))
            (setq buffer-read-only nil)
            (setq-local arxana-browser-essays-edit-mode t)
            (force-mode-line-update)
            (goto-char (min saved-point (point-max)))
            (when (window-live-p win)
              (set-window-point win (point))
              (set-window-start
               win (min saved-window-start (point-max))))))))
    (message "Saved section %s → %s%s%s%s%s"
             section-name
             (abbreviate-file-name source)
             (if (plist-get result :changed) "" " (no change)")
             (if (> synced 0) (format " [%d passage(s) synced]" synced) "")
             (if (> retracted 0) (format " [%d retracted]" retracted) "")
             (if xtdb
                 (format " [XTDB: %d ok, %d fail]"
                         (plist-get xtdb :ok)
                         (plist-get xtdb :fail))
               ""))))

;;;###autoload
(defun arxana-browser-essays-add-comment (start end text)
  "Attach an author-comment annotation to the region START..END.
TEXT is the comment body.  The comment is inserted into the current
section's manifest as an annotation with `hx-type \"annotation/comment\"',
no pattern source, and the comment body as `:note'.  The manifest is
reloaded and the section re-rendered so the comment appears both as a
yellow-tinted overlay in the text buffer and a `💬' entry in the notes
buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end)
             (read-string "Comment: "))
     (user-error "Select a region to comment on first")))
  (unless (and arxana-browser-essays--section-id
               arxana-browser-essays--source-file)
    (user-error "Not in an essay text buffer"))
  (let* ((cs (or arxana-browser-essays--content-start (point-min)))
         (passage-start (max start (marker-position cs)))
         (passage-end (max passage-start end))
         (passage (buffer-substring-no-properties passage-start passage-end))
         (section-id arxana-browser-essays--section-id)
         (essay-id arxana-browser-essays--essay-id)
         (slug (arxana-browser-essays--section-id-slug section-id))
         (ts (format-time-string "%Y%m%d%H%M%S"))
         (id (format "hx:wp:v5:comment-%s-%s" slug ts))
         (annotation
          (list :id id
                :hx-type "annotation/comment"
                :annotated (list :entity-id section-id :passage passage)
                :source (list :pattern-name nil :passage nil)
                :note text))
         (manifest-file (car arxana-browser-essays-manifest-files)))
    (unless (and passage (not (string-empty-p passage)))
      (user-error "Selected region is empty"))
    (arxana-browser-essays--append-manifest-annotation manifest-file annotation)
    (arxana-browser-essays-refresh)
    (when (and essay-id section-id arxana-browser-essays--section-name)
      (arxana-browser-essays--open-section
       essay-id section-id arxana-browser-essays--section-name))
    (message "[essays] Added comment %s (%d chars passage)" id
             (length passage))
    id))

(defun arxana-browser-essays-undo-last-save ()
  "Restore the markdown source-file and manifest(s) from `~arxana~' backups.
The save pipeline writes a `.~arxana~' sibling next to every mutated
file (markdown source and each configured manifest) immediately before
the save.  This command copies those backups back over the live files,
reloads the manifests, and re-opens the current section.

Only one level of undo — repeated C-c C-c saves overwrite the backups."
  (interactive)
  (let* ((source arxana-browser-essays--source-file)
         (md-backup (and source (concat source "~arxana~")))
         (restored 0))
    (when (and md-backup (file-readable-p md-backup))
      (copy-file md-backup source t)
      (cl-incf restored))
    (dolist (path arxana-browser-essays-manifest-files)
      (let ((bk (concat path "~arxana~")))
        (when (file-readable-p bk)
          (copy-file bk path t)
          (cl-incf restored))))
    (if (zerop restored)
        (user-error "No ~arxana~ backups found to restore")
      (arxana-browser-essays-refresh)
      (when (and arxana-browser-essays--essay-id
                 arxana-browser-essays--section-id
                 arxana-browser-essays--section-name)
        (arxana-browser-essays--open-section
         arxana-browser-essays--essay-id
         arxana-browser-essays--section-id
         arxana-browser-essays--section-name))
      (message "Restored %d file(s) from ~arxana~ backups" restored))))

(defun arxana-browser-essays-abort-edit ()
  "Abort the current edit session and re-render the section from disk."
  (interactive)
  (unless arxana-browser-essays-edit-mode
    (user-error "Not in `arxana-browser-essays-edit-mode'"))
  (let ((essay-id arxana-browser-essays--essay-id)
        (section-id arxana-browser-essays--section-id)
        (section-name arxana-browser-essays--section-name))
    (arxana-browser-essays-edit-mode -1)
    (when (and essay-id section-id section-name)
      (arxana-browser-essays--open-section essay-id section-id section-name))
    (message "Edit aborted; re-rendered from disk")))

(defun arxana-browser-essays--render-section-text
    (essay-id section-id section-name source-file heading-text
              section-text annotations)
  "Render SECTION-NAME / SECTION-TEXT into the text buffer with overlays.
Builds a buffer-local source-index mapping annotation-id to (start . end)
bounds, used by the notes buffer's sync hook.  Also records ESSAY-ID,
SECTION-ID, SOURCE-FILE, and HEADING-TEXT as buffer-locals so the
in-place edit command (`arxana-browser-essays-edit-mode') can write
changes back to disk and re-render."
  (let ((buf (get-buffer-create arxana-browser-essays-text-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            ;; Capture edit-mode state pre-render so we can restore it
            ;; after `view-mode 1' re-asserts read-only.  Without this,
            ;; any re-open (from save, from an external trigger, etc.)
            ;; silently locks the buffer, which breaks the "save as you
            ;; go" edit workflow.
            (was-editing arxana-browser-essays-edit-mode))
        (remove-overlays (point-min) (point-max))
        (erase-buffer)
        (setq-local arxana-browser-essays--source-index
                    (make-hash-table :test 'equal))
        (setq-local arxana-browser-essays--text-highlight-overlay nil)
        (setq-local arxana-browser-essays--last-active-id nil)
        (setq-local arxana-browser-essays--essay-id essay-id)
        (setq-local arxana-browser-essays--section-id section-id)
        (setq-local arxana-browser-essays--section-name section-name)
        (setq-local arxana-browser-essays--source-file source-file)
        (setq-local arxana-browser-essays--heading-text heading-text)
        (insert (format "Section: %s\n\n" section-name))
        (setq-local arxana-browser-essays--content-start (point-marker))
        (set-marker-insertion-type arxana-browser-essays--content-start nil)
        (insert section-text)
        (goto-char (point-min))
        (setq-local arxana-browser-essays--unresolved nil)
        (let ((index 0)
              (auto-rewrites nil))
          (dolist (ann annotations)
            (cl-incf index)
            (let* ((ann-id (plist-get ann :id))
                   (passage (plist-get (plist-get ann :annotated) :passage))
                   (result (arxana-browser-essays--locate-passage-bounds-lax passage))
                   (bounds (plist-get result :bounds))
                   (quality (plist-get result :quality)))
              (if (not bounds)
                  (push (list :index index :annotation ann)
                        arxana-browser-essays--unresolved)
                (when (and quality (not (eq quality 'exact)))
                  (push (list :id ann-id
                              :quality quality
                              :old passage
                              :new (buffer-substring-no-properties
                                    (car bounds) (cdr bounds)))
                        auto-rewrites))
                (puthash ann-id bounds
                         arxana-browser-essays--source-index)
                (let* ((is-comment (string= (plist-get ann :hx-type)
                                             "annotation/comment"))
                       (passage-face (if is-comment
                                         'arxana-browser-essays-comment-face
                                       'arxana-browser-essays-annotation-face))
                       (marker-face (if is-comment
                                        'arxana-browser-essays-comment-marker-face
                                      'arxana-browser-essays-marker-face))
                       (marker-label (if is-comment
                                         (format " 💬%d" index)
                                       (format " [%d]" index))))
                  (let ((ov (make-overlay (car bounds) (cdr bounds))))
                  (overlay-put ov 'face passage-face)
                  (overlay-put ov 'arxana-essay-annotation-id ann-id)
                  (overlay-put ov 'priority 10)
                  ;; Single overlay carries both the passage face and the
                  ;; inline marker.  `evaporate' deletes the overlay (and
                  ;; thus the marker) the moment the region becomes empty
                  ;; — e.g. when the user yanks over or deletes the whole
                  ;; annotated span.  Prevents hanging [N] markers.
                  (overlay-put ov 'evaporate t)
                  (overlay-put ov 'help-echo
                               (format "[%d] %s%s" index
                                       (plist-get (plist-get ann :source)
                                                  :pattern-name)
                                       (if (eq quality 'exact) ""
                                         (format " (re-anchored: %s)"
                                                 quality))))
                  (overlay-put ov 'after-string
                               (propertize marker-label 'face marker-face))
                  ;; Mirror the annotation id + marker index as text
                  ;; properties so kill/yank carries them natively
                  ;; through the kill ring.  The post-command
                  ;; reconcile sweep re-creates the overlay at the
                  ;; yank target.  `arxana-essay-annotation-id' is not
                  ;; in `yank-excluded-properties', so it survives.
                  (with-silent-modifications
                    (put-text-property (car bounds) (cdr bounds)
                                       'arxana-essay-annotation-id ann-id)
                    (put-text-property (car bounds) (cdr bounds)
                                       'arxana-essay-annotation-index index)))))))
          ;; Persist any auto-re-anchored passages back to the manifest.
          (when auto-rewrites
            (dolist (rw (nreverse auto-rewrites))
              (let ((path (arxana-browser-essays--manifest-file-for-annotation
                           (plist-get rw :id))))
                (when path
                  (arxana-browser-essays--rewrite-manifest-passage
                   path (plist-get rw :id) (plist-get rw :new))
                  (message "[essays] Re-anchored %s (%s): manifest updated"
                           (plist-get rw :id) (plist-get rw :quality)))))
            (arxana-browser-essays-refresh)))
        (setq-local arxana-browser-essays--unresolved
                    (nreverse arxana-browser-essays--unresolved))
        (goto-char (point-min))
        (view-mode 1)
        (setq-local truncate-lines nil)
        (arxana-browser-essays-text-sync-mode 1)
        ;; Restore edit-mode if it was active before the re-render, so a
        ;; save or open-section call in the middle of an editing pass
        ;; doesn't silently lock the buffer.
        (when was-editing
          (arxana-browser-essays-edit-mode 1))))
    buf))

(defun arxana-browser-essays--render-section-notes (section-name annotations)
  "Render notes for ANNOTATIONS into the notes buffer.
Builds a buffer-local note-index mapping annotation-id to (start-marker
. end-marker) bounds, used by the text buffer's sync hook.  Each entry
also carries an `arxana-essay-annotation-id' text property so the notes
buffer's own sync hook can find the id at point."
  (let ((buf (get-buffer-create arxana-browser-essays-notes-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (setq-local truncate-lines nil)
        (setq-local arxana-browser-essays--note-index
                    (make-hash-table :test 'equal))
        (setq-local arxana-browser-essays--note-highlight-overlay nil)
        (setq-local arxana-browser-essays--last-active-id nil)
        (insert (format "#+TITLE: Notes for %s\n\n" section-name))
        (if (null annotations)
            (insert "- (no annotations on this section)\n")
          (let ((index 0))
            (dolist (ann annotations)
              (cl-incf index)
              (let* ((entry-start (point))
                     (source (plist-get ann :source))
                     (pattern (plist-get source :pattern-name))
                     (source-passage (plist-get source :passage))
                     (note (plist-get ann :note))
                     (hx-type (plist-get ann :hx-type))
                     (hx-short (replace-regexp-in-string
                                "^annotation/" "" (or hx-type "")))
                     (id (plist-get ann :id)))
                (let ((is-comment (string= hx-type "annotation/comment")))
                  (cond
                   (is-comment
                    (insert (format "* 💬%d Author comment\n" index))
                    (when note
                      (insert (format "  %s\n" note))))
                   (t
                    (insert (format "* [%d] " index))
                    (if pattern
                        (insert
                         (propertize
                          pattern
                          'arxana-essays-pattern-name pattern
                          'face 'arxana-browser-essays-pattern-link-face
                          'font-lock-face 'arxana-browser-essays-pattern-link-face
                          'mouse-face 'highlight
                          'follow-link t
                          'keymap arxana-browser-essays-notes-link-map
                          'help-echo
                          (format "RET / mouse-1: open pattern %s" pattern)))
                      (insert "(no pattern)"))
                    (insert (format " (%s)\n" hx-short))
                    (when source-passage
                      (insert (format "  %s\n" source-passage)))
                    (when note
                      (insert (format "  - Gloss: %s\n" note)))))
                  (insert "\n"))
                ;; Tag the whole entry with the annotation-id so the
                ;; notes-side sync hook can find which annotation point is on.
                (put-text-property entry-start (point)
                                   'arxana-essay-annotation-id id)
                ;; Index for the text-side sync hook to look up.
                (puthash id
                         (cons (copy-marker entry-start)
                               (copy-marker (point)))
                         arxana-browser-essays--note-index)))))
        (goto-char (point-min))
        (view-mode 1)
        (arxana-browser-essays-notes-sync-mode 1)
        (arxana-browser-essays-notes-mode 1)))
    buf))

(defun arxana-browser-essays--display-section-buffers (text-buf notes-buf)
  "Display TEXT-BUF on the main window and NOTES-BUF in a side window.
Selects a normal (non-side) window first so `delete-other-windows' is
legal — important when called while the pattern three-pane layout is
active (notes in a side window)."
  (let ((main-win (or (seq-find (lambda (w)
                                  (null (window-parameter w 'window-side)))
                                (window-list nil 'nomini))
                      (selected-window))))
    (select-window main-win)
    (delete-other-windows)
    (set-window-buffer (selected-window) text-buf)
    (display-buffer-in-side-window
     notes-buf
     `((side . ,arxana-browser-essays-notes-side)
       (slot . 0)
       (window-width . ,arxana-browser-essays-notes-width)
       (preserve-size . (t . nil))))))

(defun arxana-browser-essays--open-section (essay-id section-id section-name)
  "Open the reading view for SECTION-ID in essay ESSAY-ID."
  (let* ((manifest (arxana-browser-essays--manifest-for essay-id))
         (source-file (arxana-browser-essays--catalog-source-file essay-id))
         (heading-text (arxana-browser-essays--section-heading-text
                        manifest section-id))
         (section-text (or (arxana-browser-essays--extract-section-text
                            source-file heading-text)
                           (format "(Could not extract section text from %s.\n See heading: %s)\n"
                                   (or source-file "<no source-file in catalog>")
                                   (or heading-text "<no heading-text in section props>"))))
         (annotations (arxana-browser-essays--annotations-for-section
                       manifest section-id))
         (text-buf (arxana-browser-essays--render-section-text
                    essay-id section-id section-name
                    source-file heading-text
                    section-text annotations))
         (notes-buf (arxana-browser-essays--render-section-notes
                     section-name annotations)))
    (arxana-browser-essays--display-section-buffers text-buf notes-buf)))

;;;###autoload
(defun arxana-browser-essays-audit-passages ()
  "Audit every annotation in every loaded essay manifest.
For each annotation, attempts to locate `:annotated :passage' in the
corresponding section's source text.  Reports passages that could not
be found, by section.  Useful after editing annotations.el to verify
the highlight overlays will actually attach in the reading view."
  (interactive)
  (let ((report-buf (get-buffer-create "*Arxana Essays Audit*"))
        (total 0)
        (found 0)
        (missing nil))
    (with-current-buffer report-buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Arxana Essays — passage audit\n")
        (insert "==============================\n\n")
        (dolist (manifest (arxana-browser-essays--all-manifests))
          (let* ((essay (plist-get manifest :essay))
                 (essay-id (plist-get essay :id))
                 (source-file (arxana-browser-essays--catalog-source-file
                               essay-id))
                 (sections (plist-get manifest :sections))
                 (annotations (plist-get manifest :annotations)))
            (insert (format "Essay: %s\n" (plist-get essay :name)))
            (insert (format "Source: %s\n\n" source-file))
            (dolist (section sections)
              (let* ((sid (plist-get section :id))
                     (sname (plist-get section :name))
                     (heading-text (alist-get 'heading-text
                                              (plist-get section :props)))
                     (text (arxana-browser-essays--extract-section-text
                            source-file heading-text))
                     (anns (arxana-browser-essays--annotations-for-section
                            manifest sid))
                     (section-missing nil))
                (insert (format "## %s\n" sname))
                (cond
                 ((null text)
                  (insert (format "  ! Section text could not be extracted (heading: %s)\n\n"
                                  heading-text)))
                 (t
                  (dolist (ann anns)
                    (cl-incf total)
                    (let ((passage (plist-get (plist-get ann :annotated)
                                              :passage)))
                      (if (with-temp-buffer
                            (insert text)
                            (goto-char (point-min))
                            (search-forward passage nil t))
                          (cl-incf found)
                        (push (list :section sname
                                    :id (plist-get ann :id)
                                    :passage passage)
                              section-missing))))
                  (if section-missing
                      (progn
                        (dolist (m (nreverse section-missing))
                          (push m missing)
                          (insert (format "  ! MISS  %s\n        %s\n"
                                          (plist-get m :id)
                                          (plist-get m :passage))))
                        (insert "\n"))
                    (insert (format "  ✓ all %d annotations matched\n\n"
                                    (length anns))))))))))
        (insert "==============================\n")
        (insert (format "Total: %d annotations\n" total))
        (insert (format "Matched: %d\n" found))
        (insert (format "Missed:  %d\n" (length missing)))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer report-buf)
    (when (called-interactively-p 'interactive)
      (message "Audit: %d/%d matched (%d missed)" found total (length missing)))
    missing))

(defun arxana-browser-essays-open (item)
  "Open browser ITEM.
For 'essays-section items, opens the side-by-side reading view (text +
notes) and captures the pre-open window configuration so that
`<left>' at point-min can return to it (Arxana-UI convention)."
  (pcase (plist-get item :type)
    ('essays-section
     (let ((return-buffer (current-buffer))
           (return-config (current-window-configuration)))
       (arxana-browser-essays--open-section
        (plist-get item :essay-id)
        (plist-get item :section-id)
        (plist-get item :label))
       (dolist (buf (list (get-buffer arxana-browser-essays-text-buffer)
                          (get-buffer arxana-browser-essays-notes-buffer)))
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq-local arxana-ui-return-buffer return-buffer)
             (setq-local arxana-ui-return-window-config return-config))))))
    ('essays-annotation
     (let ((buf (get-buffer-create "*Arxana Essay Annotation*")))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (format "Annotation: %s\n" (plist-get item :annotation-id)))
           (insert (format "Type: %s\n\n" (plist-get item :hx-type)))
           (insert "Annotated passage:\n")
           (insert (format "  %s\n\n" (plist-get item :label)))
           (insert (format "Source pattern: %s\n" (plist-get item :pattern-name)))
           (insert "Source passage:\n")
           (insert (format "  %s\n\n" (plist-get item :source-passage)))
           (insert "Gloss:\n")
           (insert (format "  %s\n" (or (plist-get item :description) ""))))
         (goto-char (point-min))
         (special-mode))
       (display-buffer buf)))
    (_
     (message "[essays] Drill in further: RET on a section to open the reading view."))))

;;; ─────────────────────────────────────────────────────────────
;;; Pattern-name hyperlinks in the notes buffer (federated-wiki style)
;;; ─────────────────────────────────────────────────────────────

;; Important: `org-unfontify-region' strips `keymap' and `mouse-face'
;; text properties on every font-lock pass in an org buffer (see
;; `org.el' :: `org-unfontify-region').  We therefore cannot rely on
;; text-property keymaps here — bindings live on a buffer-local
;; minor-mode keymap (`arxana-browser-essays-notes-mode-map'), and the
;; activation function dispatches on the surviving text property
;; `arxana-essays-pattern-name'.  The `follow-link' property still
;; works for mouse-1 because Emacs consults it regardless of
;; `mouse-face'.
(defvar arxana-browser-essays-notes-link-map
  (let ((map (make-sparse-keymap)))
    ;; Retained for legacy reference / future non-org contexts.  Not
    ;; attached as a text-property keymap — see comment above.
    (define-key map (kbd "RET") #'arxana-browser-essays-follow-pattern-at-point)
    (define-key map [mouse-1] #'arxana-browser-essays-follow-pattern-at-point)
    (define-key map [mouse-2] #'arxana-browser-essays-follow-pattern-at-point)
    map)
  "Keymap for pattern-name activation (also used by notes-mode).")

(defvar arxana-browser-essays-notes-mode-map nil
  "Keymap for `arxana-browser-essays-notes-mode'.
Dispatches RET / mouse-1 based on the `arxana-essays-pattern-name' text
property at point; falls through to org bindings when no such property
is present.  `<left>' at point-min returns to the prior layout.")

(setq arxana-browser-essays-notes-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "RET") #'arxana-browser-essays-follow-pattern-at-point)
        (define-key map [mouse-1] #'arxana-browser-essays-follow-pattern-at-point)
        (define-key map [mouse-2] #'arxana-browser-essays-follow-pattern-at-point)
        (define-key map (kbd "<left>") #'arxana-browser-essays-left-or-return)
        map))

(define-minor-mode arxana-browser-essays-notes-mode
  "Minor mode in `*Arxana Essay Notes*' enabling pattern-link activation.
Bound in the notes buffer after render.  Activates the pattern-name
link under point via RET or mouse; passes through to default bindings
when no pattern is at point."
  :init-value nil
  :lighter nil
  :keymap arxana-browser-essays-notes-mode-map)

(defvar-local arxana-browser-essays--restore-config nil
  "Window configuration saved before a pattern was opened from notes.
Set buffer-local on the opened pattern buffer so killing it restores
the previous two-pane layout.")

(defun arxana-browser-essays--restore-on-kill ()
  "Kill-buffer-hook on a pattern buffer opened from notes.
Restores the window configuration saved just before the pattern was
opened.  No-op if no saved configuration is attached."
  (let ((config arxana-browser-essays--restore-config))
    (setq arxana-browser-essays--restore-config nil)
    (when (and config (window-configuration-p config))
      ;; Defer to post-kill so the pattern buffer's window is already
      ;; gone; otherwise `set-window-configuration' may fail on the
      ;; soon-to-die window.
      (run-at-time 0 nil #'set-window-configuration config))))

(defun arxana-browser-essays-follow-pattern-at-point (&optional event)
  "Open the design pattern named at point in a right-side pane.
The essay text window is dropped (scrolled off-screen left); the notes
buffer takes the left slot; the pattern buffer takes the right slot.
Killing the pattern buffer restores the prior two-pane layout.

Reads the pattern name from the `arxana-essays-pattern-name' text
property at point.  When no property is found, falls through to
`org-return' so the minor-mode binding does not hijack ordinary
navigation in the notes buffer."
  (interactive (list last-nonmenu-event))
  (when (and event (mouse-event-p event))
    (mouse-set-point event))
  (let ((name (get-text-property (point) 'arxana-essays-pattern-name)))
    (if (null name)
        (cond
         ((and event (mouse-event-p event)) nil)
         ((fboundp 'org-return) (call-interactively #'org-return))
         (t (call-interactively #'newline)))
      (require 'arxana-browser-patterns nil t)
      (unless (fboundp 'arxana-browser-patterns-open)
        (user-error "arxana-browser-patterns-open is not available"))
      (let* ((notes-buf (current-buffer))
             (saved-config (current-window-configuration))
             (buffers-before (buffer-list))
             ;; `arxana-browser-patterns-open' dispatches to either
             ;; `--render-pattern' (creates `*Arxana Pattern: NAME*') or
             ;; `-open-filesystem' (opens `NAME.flexiarg' via find-file).
             ;; Capture whichever buffer it put us in, falling back to
             ;; the newest buffer created during the call.
             (pattern-buf
              (save-window-excursion
                (arxana-browser-patterns-open name)
                (or (current-buffer)
                    (seq-find (lambda (b) (not (memq b buffers-before)))
                              (buffer-list))))))
        (unless (buffer-live-p pattern-buf)
          (user-error "Could not locate pattern buffer for %s" name))
        ;; The notes buffer is typically displayed as a side window;
        ;; `delete-other-windows' refuses to leave a side window as the
        ;; only window.  Jump to a normal window (the essay text one)
        ;; first, then blow away the rest and rebuild.
        (let ((text-win (get-buffer-window
                         arxana-browser-essays-text-buffer t)))
          (when (window-live-p text-win)
            (select-window text-win)))
        (delete-other-windows)
        (set-window-buffer (selected-window) notes-buf)
        (let ((right (split-window-right)))
          (set-window-buffer right pattern-buf)
          (select-window right))
        (with-current-buffer pattern-buf
          (setq-local arxana-browser-essays--restore-config saved-config)
          (add-hook 'kill-buffer-hook
                    #'arxana-browser-essays--restore-on-kill nil t))
        (message "Pattern %s — kill buffer to return to essay layout" name)))))

;;; ─────────────────────────────────────────────────────────────
;;; Named checkpoints in XTDB (save / list / restore)
;;; ─────────────────────────────────────────────────────────────

(defun arxana-browser-essays--section-id-slug (section-id)
  "Return the final path segment of SECTION-ID for use in checkpoint ids."
  (car (last (split-string section-id "/"))))

;;;###autoload
(defun arxana-browser-essays-checkpoint-save (label)
  "Save a named checkpoint of the current section to XTDB.
Captures the markdown text of the section (read from disk) plus every
annotation's current `:passage' as known to the manifest, persisting
them as an entity of type `arxana/checkpoint' with LABEL as its name.
Restorable via `arxana-browser-essays-checkpoint-restore'.

Use this at milestones — the save pipeline already captures every C-c
C-c as an XTDB transaction, but those transactions are anonymous;
checkpoints are the named, human-meaningful restore targets."
  (interactive (list (read-string "Checkpoint label: ")))
  (let* ((essay-id arxana-browser-essays--essay-id)
         (section-id arxana-browser-essays--section-id)
         (section-name arxana-browser-essays--section-name)
         (heading arxana-browser-essays--heading-text)
         (source-file arxana-browser-essays--source-file))
    (unless (and essay-id section-id section-name source-file)
      (user-error "Not in an essay text buffer with section metadata"))
    (unless (arxana-store-ensure-sync)
      (user-error "Futon sync disabled"))
    (let* ((section-text
            (or (arxana-browser-essays--extract-section-text
                 source-file heading)
                (user-error "Could not read section text from %s" source-file)))
           (manifest (arxana-browser-essays--manifest-for essay-id))
           (section-anns
            (arxana-browser-essays--annotations-for-section manifest section-id))
           (ann-snapshot
            (mapcar (lambda (a)
                      (list :id (plist-get a :id)
                            :passage (plist-get (plist-get a :annotated)
                                                :passage)))
                    section-anns))
           (ts (format-time-string "%Y%m%dT%H%M%S"))
           (slug (arxana-browser-essays--section-id-slug section-id))
           (id (format "arxana/checkpoint/%s/%s" slug ts)))
      (arxana-store-ensure-entity
       :id id
       :name label
       :type "arxana/checkpoint"
       :props `((essay-id . ,essay-id)
                (section-id . ,section-id)
                (section-name . ,section-name)
                (heading-text . ,heading)
                (source-file . ,source-file)
                (tx-time-iso . ,(format-time-string "%FT%T%z"))
                (section-text . ,section-text)
                (annotation-count . ,(length section-anns))
                (annotations . ,ann-snapshot)))
      (message "[essays] Checkpoint saved: %s [%s]" label id)
      id)))

(defun arxana-browser-essays--checkpoints-for-section (section-id)
  "Return checkpoint entities matching SECTION-ID."
  (let* ((response (arxana-store-fetch-entities-latest
                    :type "arxana/checkpoint" :limit 200))
         (entities (arxana-browser-essays--unwrap-entities response)))
    (seq-filter
     (lambda (e)
       (string= section-id
                (or (alist-get :section-id e)
                    (let ((p (alist-get :props e)))
                      (and (listp p) (alist-get :section-id p))))))
     entities)))

;;;###autoload
(defun arxana-browser-essays-checkpoint-list ()
  "List checkpoints for the current section in a dedicated buffer.
Each entry shows: timestamp — label — annotation count — id.  Press RET
on an entry to restore that checkpoint."
  (interactive)
  (let* ((section-id arxana-browser-essays--section-id)
         (section-name arxana-browser-essays--section-name))
    (unless section-id
      (user-error "Not in an essay text buffer"))
    (unless (arxana-store-ensure-sync)
      (user-error "Futon sync disabled"))
    (let* ((entries (arxana-browser-essays--checkpoints-for-section section-id))
           (buf (get-buffer-create "*Arxana Essay Checkpoints*")))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Checkpoints for: %s\n" (or section-name section-id)))
          (insert (make-string 60 ?─) "\n\n")
          (if (null entries)
              (insert "(no checkpoints yet — M-x arxana-browser-essays-checkpoint-save)\n")
            (dolist (e entries)
              (let* ((id (alist-get :id e))
                     (label (alist-get :name e))
                     (p (alist-get :props e))
                     (tx (or (and (listp p) (alist-get :tx-time-iso p)) "?"))
                     (n  (or (and (listp p) (alist-get :annotation-count p)) "?"))
                     (line-start (point)))
                (insert (format "  %s  %s  (%s annotations)\n    %s\n\n"
                                tx label n id))
                (put-text-property line-start (point)
                                   'arxana-essays-checkpoint-id id))))
          (goto-char (point-min))
          (special-mode)
          (local-set-key (kbd "RET")
                         #'arxana-browser-essays-checkpoint-restore-at-point)))
      (display-buffer buf))))

(defun arxana-browser-essays-checkpoint-restore-at-point ()
  "Restore the checkpoint whose entry contains point in the list buffer."
  (interactive)
  (let ((id (get-text-property (point) 'arxana-essays-checkpoint-id)))
    (if id
        (arxana-browser-essays-checkpoint-restore id)
      (message "No checkpoint at point"))))

;;;###autoload
(defun arxana-browser-essays-checkpoint-restore (checkpoint-id)
  "Restore section text and annotations from CHECKPOINT-ID.
Writes the checkpoint's `section-text' back to the markdown file (with
an `~arxana~' backup), rewrites each stored annotation's `:passage' in
the manifest, and upserts the hyperedges to XTDB — each upsert is a new
transaction, so the restore itself is also visible in XTDB history.
Re-renders the section."
  (interactive (list (read-string "Checkpoint id: ")))
  (unless (arxana-store-ensure-sync)
    (user-error "Futon sync disabled"))
  (let* ((response (arxana-store-fetch-entity checkpoint-id))
         (entity (and (listp response) (alist-get :entity response)))
         (props (and entity (alist-get :props entity))))
    (unless props
      (user-error "Checkpoint %s not found" checkpoint-id))
    (let* ((section-id   (alist-get :section-id props))
           (heading      (alist-get :heading-text props))
           (source-file  (alist-get :source-file props))
           (section-text (alist-get :section-text props))
           (annotations  (alist-get :annotations props))
           (essay-id     (alist-get :essay-id props))
           (section-name (alist-get :section-name props)))
      (arxana-browser-essays--backup-manifest-files)
      (when (and source-file (file-writable-p source-file))
        (copy-file source-file (concat source-file "~arxana~") t))
      (when (and source-file heading section-text)
        (arxana-browser-essays--replace-section-in-file
         source-file heading section-text))
      (dolist (ann annotations)
        (let* ((id (plist-get ann :id))
               (passage (plist-get ann :passage))
               (path (arxana-browser-essays--manifest-file-for-annotation id)))
          (when (and id passage path)
            (arxana-browser-essays--rewrite-manifest-passage path id passage))))
      (arxana-browser-essays-refresh)
      (arxana-browser-essays--sync-annotations-to-xtdb
       (delq nil (mapcar (lambda (a) (plist-get a :id)) annotations)))
      (when (and essay-id section-id section-name)
        (arxana-browser-essays--open-section essay-id section-id section-name))
      (message "[essays] Restored checkpoint %s" checkpoint-id))))

(provide 'arxana-browser-essays)
;;; arxana-browser-essays.el ends here
