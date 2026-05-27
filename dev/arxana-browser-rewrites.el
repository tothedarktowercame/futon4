;;; arxana-browser-rewrites.el --- Three-up rewrite review surface -*- lexical-binding: t; -*-

;;; Commentary:
;; A read-only 3-up review surface for typed rewrites recorded in an
;; annotations .edn (the canonical hypergraph for an Arxana Essay).
;;
;; The three panels:
;;
;;   *Arxana Rewrite A*       *Arxana Rewrite Rule*      *Arxana Rewrite B*
;;   (Version A — :before)    (the f:(A,B)->C trace)     (Version B — :after)
;;
;; Each panel renders from a single annotation entry's :closure block.
;; A is the pre-rewrite span in the paper's prose; Rule is the typed
;; transformation (pattern + composition + augmentation + invariant-delta
;; + verification); B is the post-rewrite span.
;;
;; Convention follows arxana-browser-code.el (the 2-up Code<->Docs surface):
;; dedicated frame, buffer naming `*Arxana <Name>*`, custom group with
;; configurable layout, mode-map for actions.
;;
;; Status (2026-05-14): minimal first prototype.
;; - Reads annotations-v12.edn via a small built-in EDN reader.
;; - Renders one annotation by :id.
;; - Action keys are stubbed (print messages) — accept/reject/edit/defer/promote.
;; - No write-back to the .edn yet; that lands once review judgments are
;;   trustworthy.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

;; -----------------------------------------------------------------
;; Customisation
;; -----------------------------------------------------------------

(defgroup arxana-browser-rewrites nil
  "Three-up review surface for typed rewrites."
  :group 'arxana)

(defcustom arxana-browser-rewrites-annotations-file nil
  "Obsolete one-shot override for the default corpus's annotations file.

If non-nil, the next default-corpus load uses this path once, then clears
the variable.  Prefer `arxana-browser-rewrites-corpora'."
  :type '(choice
          (const :tag "Use corpus registry default" nil)
          file)
  :group 'arxana-browser-rewrites)

(make-obsolete-variable 'arxana-browser-rewrites-annotations-file
                        'arxana-browser-rewrites-corpora
                        "a future major release")

(defcustom arxana-browser-rewrites-corpora
  '((:id :ukrn-wp-v12
     :name "npt UKRN WP v12"
     :annotations-file "/home/joe/npt/working-paper/annotations-v12.edn"
     :default? t)
    (:id :side-a
     :name "Hyperreal Side A"
     :annotations-file "/home/joe/npt/applications/hyperreal-director-side-a/annotations.el"
     :default? nil)
    (:id :side-b
     :name "Hyperreal Side B"
     :annotations-file "/home/joe/npt/applications/hyperreal-director-side-b/annotations.el"
     :default? nil)
    (:id :vsatarcs-alignment-completeness
     :name "VSATARCS alignment completeness (R-criteria)"
     :annotations-file "/home/joe/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn"
     :default? nil)
    ;; (:id :futon-stack
    ;;  :name "Futon-Stack Descriptive Essay"
    ;;  :annotations-file "/home/joe/.../annotations.edn"
    ;;  :default? nil)
    )
  "Corpus registry for the RewriteReview surface.

Each entry is a plist with `:id', `:name', `:annotations-file', and
`:default?'.  Exactly one live entry should normally carry `:default? t'."
  :type '(repeat sexp)
  :group 'arxana-browser-rewrites)

(defcustom arxana-browser-rewrites-frame-name "Arxana Rewrites"
  "Dedicated frame name for the 3-up review surface."
  :type 'string
  :group 'arxana-browser-rewrites)

(defcustom arxana-browser-rewrites-a-buffer "*Arxana Rewrite A*"
  "Buffer name for the left panel (Version A, pre-rewrite span)."
  :type 'string
  :group 'arxana-browser-rewrites)

(defcustom arxana-browser-rewrites-rule-buffer "*Arxana Rewrite Rule*"
  "Buffer name for the centre panel (the rewriting rule)."
  :type 'string
  :group 'arxana-browser-rewrites)

(defcustom arxana-browser-rewrites-b-buffer "*Arxana Rewrite B*"
  "Buffer name for the right panel (Version B, post-rewrite span)."
  :type 'string
  :group 'arxana-browser-rewrites)

(defcustom arxana-browser-rewrites-wrap-column 88
  "Wrap column for prose rendering in the side panels."
  :type 'integer
  :group 'arxana-browser-rewrites)

;; -----------------------------------------------------------------
;; State
;; -----------------------------------------------------------------

(defvar arxana-browser-rewrites--annotations nil
  "Cached list of annotations from the loaded .edn (list of plists).")

(defvar-local arxana-browser-rewrites--current-corpus nil
  "Active corpus id for the current RewriteReview panel buffer.")

(defvar-local arxana-browser-rewrites--current-id nil
  "The :id of the annotation currently displayed.")

(defvar arxana-browser-rewrites--ordered-ids nil
  "Ordered list of closed-annotation :ids for navigation.")

(defun arxana-browser-rewrites--corpus-by-id (corpus-id)
  "Return the corpus plist whose `:id' equals CORPUS-ID."
  (cl-find-if (lambda (corpus)
                (eq (plist-get corpus :id) corpus-id))
              arxana-browser-rewrites-corpora))

(defun arxana-browser-rewrites--default-corpus ()
  "Return the registry entry marked as default corpus."
  (or (cl-find-if (lambda (corpus) (plist-get corpus :default?))
                  arxana-browser-rewrites-corpora)
      (car arxana-browser-rewrites-corpora)))

(defun arxana-browser-rewrites--default-corpus-id ()
  "Return the id of the default corpus."
  (plist-get (arxana-browser-rewrites--default-corpus) :id))

(defun arxana-browser-rewrites--current-corpus-id ()
  "Return the active corpus id for the current command context."
  (or arxana-browser-rewrites--current-corpus
      (arxana-browser-rewrites--default-corpus-id)))

(defun arxana-browser-rewrites--resolve-corpus (&optional corpus-id)
  "Return the effective corpus plist for CORPUS-ID.

When the obsolete `arxana-browser-rewrites-annotations-file' override is
set and the selected corpus is the default corpus, use that path once."
  (let* ((target-id (or corpus-id
                        (arxana-browser-rewrites--current-corpus-id)))
         (corpus (copy-sequence
                  (or (arxana-browser-rewrites--corpus-by-id target-id)
                      (user-error "Unknown RewriteReview corpus: %s" target-id)))))
    (when (and arxana-browser-rewrites-annotations-file
               (eq (plist-get corpus :id)
                   (arxana-browser-rewrites--default-corpus-id)))
      (plist-put corpus :annotations-file
                 arxana-browser-rewrites-annotations-file)
      (setq arxana-browser-rewrites-annotations-file nil))
    corpus))

(defun arxana-browser-rewrites--corpus-suffix (corpus-id)
  "Return a buffer-name suffix for CORPUS-ID, or nil for the default corpus."
  (unless (eq corpus-id (arxana-browser-rewrites--default-corpus-id))
    (format " [%s]" (arxana-browser-rewrites--display-name corpus-id))))

(defun arxana-browser-rewrites--buffer-name (base corpus-id panel)
  "Return the panel buffer name for BASE, CORPUS-ID, and PANEL.

BASE is the default-corpus buffer name; PANEL is the trailing label
already present in BASE (for example \"A\" or \"Rule\")."
  (let ((suffix (arxana-browser-rewrites--corpus-suffix corpus-id)))
    (if suffix
        (format "*Arxana Rewrite%s %s*" suffix panel)
      base)))

(defun arxana-browser-rewrites--set-buffer-context (buffer corpus-id id)
  "Record CORPUS-ID and annotation ID in BUFFER's buffer-local state."
  (with-current-buffer buffer
    (setq-local arxana-browser-rewrites--current-corpus corpus-id)
    (setq-local arxana-browser-rewrites--current-id id)))

;; -----------------------------------------------------------------
;; Minimal EDN reader
;;
;; Handles the subset our annotations file uses: maps {}, vectors [],
;; lists (), strings "...", namespaced keywords :foo/bar, numbers,
;; nil/true/false, line comments. Maps become plists keyed by keyword
;; (lookup via `plist-get`). Commas are treated as whitespace per EDN.
;; -----------------------------------------------------------------

(defun arxana-browser-rewrites--edn-skip-ws ()
  "Skip whitespace, commas, and EDN line comments at point."
  (let ((continue t))
    (while continue
      (skip-chars-forward " \t\n\r,")
      (if (eq (char-after) ?\;)
          (forward-line 1)
        (setq continue nil)))))

(defun arxana-browser-rewrites--edn-read-string ()
  "Read an EDN string starting at the opening double-quote."
  (forward-char 1) ; skip opening "
  (let ((start (point)))
    (while (and (not (eobp)) (not (eq (char-after) ?\")))
      (if (eq (char-after) ?\\)
          (forward-char 2)
        (forward-char 1)))
    (let ((raw (buffer-substring-no-properties start (point))))
      (forward-char 1) ; skip closing "
      (setq raw (replace-regexp-in-string "\\\\\"" "\"" raw))
      (setq raw (replace-regexp-in-string "\\\\n" "\n" raw))
      (setq raw (replace-regexp-in-string "\\\\t" "\t" raw))
      (setq raw (replace-regexp-in-string "\\\\\\\\" "\\\\" raw))
      raw)))

(defun arxana-browser-rewrites--edn-read-keyword ()
  "Read an EDN keyword `:foo` or `:ns/name` at point."
  (forward-char 1) ; skip :
  (let ((start (point)))
    ;; `-` placed at the very end of the char-set is literal, not a range.
    (skip-chars-forward "a-zA-Z0-9_/.?!*+:-")
    (intern (concat ":" (buffer-substring-no-properties start (point))))))

(defun arxana-browser-rewrites--edn-read-number ()
  "Read a numeric literal at point."
  (let ((start (point)))
    ;; `-` at the end is literal; previously `+-e` was being parsed as a range
    ;; from `+` (43) to `e` (101), consuming brackets and colons.
    (skip-chars-forward "0-9.+eE-")
    (string-to-number (buffer-substring-no-properties start (point)))))

(defun arxana-browser-rewrites--edn-read-symbol ()
  "Read a bare symbol at point (catch-all)."
  (let ((start (point)))
    ;; `-` at the end is literal.
    (skip-chars-forward "a-zA-Z0-9_/.?!*+-")
    (intern (buffer-substring-no-properties start (point)))))

(defun arxana-browser-rewrites--edn-read-tagged-or-set ()
  "Handle `#' dispatch: `#{...}' as a list, `#tag form' as the form (tag discarded).
The discriminator after `#' is either `{' for a set or a tag symbol
(e.g. `inst', `uuid').  The tagged-value path consumes the tag and the
following form, returning only the form; this preserves tag-bearing
values' content without modelling tags as elisp values yet."
  (forward-char 1) ; skip #
  (cond
   ((eq (char-after) ?\{)
    ;; #{...} — set; read as list since order is irrelevant for our consumers.
    (forward-char 1) ; skip {
    (let (elts)
      (arxana-browser-rewrites--edn-skip-ws)
      (while (not (eq (char-after) ?\}))
        (push (arxana-browser-rewrites--edn-read-form) elts)
        (arxana-browser-rewrites--edn-skip-ws))
      (forward-char 1) ; skip }
      (nreverse elts)))
   (t
    ;; #tag form — read and discard the tag, return the form
    (arxana-browser-rewrites--edn-read-symbol)
    (arxana-browser-rewrites--edn-read-form))))

(defun arxana-browser-rewrites--edn-read-form ()
  "Read one EDN form at point."
  (arxana-browser-rewrites--edn-skip-ws)
  (cond
   ((eobp) nil)
   ((eq (char-after) ?\{) (arxana-browser-rewrites--edn-read-map))
   ((eq (char-after) ?\[) (arxana-browser-rewrites--edn-read-vector))
   ((eq (char-after) ?\() (arxana-browser-rewrites--edn-read-list))
   ((eq (char-after) ?\#) (arxana-browser-rewrites--edn-read-tagged-or-set))
   ((eq (char-after) ?\") (arxana-browser-rewrites--edn-read-string))
   ((eq (char-after) ?\:) (arxana-browser-rewrites--edn-read-keyword))
   ((looking-at "nil\\b") (forward-char 3) nil)
   ((looking-at "true\\b") (forward-char 4) t)
   ((looking-at "false\\b") (forward-char 5) nil)
   ((looking-at "[-+]?[0-9]") (arxana-browser-rewrites--edn-read-number))
   (t (arxana-browser-rewrites--edn-read-symbol))))

(defun arxana-browser-rewrites--edn-read-map ()
  "Read an EDN map at point into a plist."
  (forward-char 1) ; skip {
  (let (acc)
    (arxana-browser-rewrites--edn-skip-ws)
    (while (not (eq (char-after) ?\}))
      (let ((k (arxana-browser-rewrites--edn-read-form))
            (v (arxana-browser-rewrites--edn-read-form)))
        ;; Push k then v so nreverse produces a (k v k v ...) plist
        ;; usable directly with `plist-get`.
        (push k acc)
        (push v acc))
      (arxana-browser-rewrites--edn-skip-ws))
    (forward-char 1) ; skip }
    (nreverse acc)))

(defun arxana-browser-rewrites--edn-read-vector ()
  "Read an EDN vector at point into an elisp vector."
  (forward-char 1) ; skip [
  (let (elts)
    (arxana-browser-rewrites--edn-skip-ws)
    (while (not (eq (char-after) ?\]))
      (push (arxana-browser-rewrites--edn-read-form) elts)
      (arxana-browser-rewrites--edn-skip-ws))
    (forward-char 1) ; skip ]
    (apply #'vector (nreverse elts))))

(defun arxana-browser-rewrites--edn-read-list ()
  "Read an EDN list at point."
  (forward-char 1) ; skip (
  (let (elts)
    (arxana-browser-rewrites--edn-skip-ws)
    (while (not (eq (char-after) ?\)))
      (push (arxana-browser-rewrites--edn-read-form) elts)
      (arxana-browser-rewrites--edn-skip-ws))
    (forward-char 1) ; skip )
    (nreverse elts)))

(defun arxana-browser-rewrites--read-edn-file (path)
  "Read EDN file at PATH; return the top-level form."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (arxana-browser-rewrites--edn-read-form)))

;; -----------------------------------------------------------------
;; Annotation loading & lookup
;; -----------------------------------------------------------------

(defun arxana-browser-rewrites-load (&optional corpus-id)
  "Load (or reload) annotations for CORPUS-ID.

Interactively, reload the current panel's corpus, or the default corpus when
called outside RewriteReview buffers."
  (interactive)
  (let* ((corpus (arxana-browser-rewrites--resolve-corpus corpus-id))
         (resolved-id (plist-get corpus :id))
         (path (plist-get corpus :annotations-file)))
    (if (string-suffix-p ".el" path)
        (progn
          (message
           "corpus %s uses .el format which is not yet supported by arxana-browser-rewrites; use arxana-browser-essays.el for those"
           (plist-get corpus :name))
          nil)
      (let* ((data (arxana-browser-rewrites--read-edn-file path))
             (anns (append (plist-get data :annotations) nil)))
        (setq arxana-browser-rewrites--annotations anns)
        (setq arxana-browser-rewrites--ordered-ids
              (mapcar (lambda (a) (plist-get a :id))
                      (cl-remove-if-not
                       (lambda (a) (eq (plist-get a :status) :closed))
                       anns)))
        (setq arxana-browser-rewrites--current-corpus resolved-id)
        (message "Loaded %d annotations from %s [%s] (%d closed)"
                 (length anns)
                 path
                 (plist-get corpus :name)
                 (length arxana-browser-rewrites--ordered-ids))
        anns))))

(defun arxana-browser-rewrites--find (id)
  "Return the annotation plist whose :id equals ID."
  (cl-find-if (lambda (a) (equal (plist-get a :id) id))
              arxana-browser-rewrites--annotations))

;; -----------------------------------------------------------------
;; Panel rendering
;; -----------------------------------------------------------------

(defface arxana-browser-rewrites-header-face
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for panel headers."
  :group 'arxana-browser-rewrites)

(defface arxana-browser-rewrites-field-face
  '((t :inherit font-lock-type-face :weight bold))
  "Face for field labels in the rule panel."
  :group 'arxana-browser-rewrites)

(defface arxana-browser-rewrites-diff-add-face
  '((t :inherit diff-added))
  "Face for the post-rewrite (B) span (blob fallback only)."
  :group 'arxana-browser-rewrites)

(defface arxana-browser-rewrites-diff-del-face
  '((t :inherit diff-removed))
  "Face for the pre-rewrite (A) span (blob fallback only)."
  :group 'arxana-browser-rewrites)

;; Per-entity fate faces (the physicalized sheaf).
;; KEEP and KEEP-WITH-GLUE-ADJUSTMENT entities use neutral text so the
;; eye can scan structural correspondence between A and B.  DROP / REPLACE
;; entities are coloured by fate, and new (ADDed) entities on B side too.

(defface arxana-browser-rewrites-entity-id-face
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for entity ids in panel headers." :group 'arxana-browser-rewrites)

(defface arxana-browser-rewrites-entity-type-face
  '((t :inherit font-lock-type-face))
  "Face for entity type labels." :group 'arxana-browser-rewrites)

(defface arxana-browser-rewrites-entity-keep-face
  '((t :inherit default))
  "Face for KEEP entities (unchanged across A and B)."
  :group 'arxana-browser-rewrites)

(defface arxana-browser-rewrites-entity-drop-face
  '((t :inherit shadow :strike-through t))
  "Face for DROP entities (struck through)."
  :group 'arxana-browser-rewrites)

(defface arxana-browser-rewrites-entity-replace-face
  '((t :inherit font-lock-warning-face))
  "Face for REPLACE-source entities (about to be replaced on B side)."
  :group 'arxana-browser-rewrites)

(defface arxana-browser-rewrites-entity-add-face
  '((t :inherit diff-added))
  "Face for ADDED entities on B side."
  :group 'arxana-browser-rewrites)

(defface arxana-browser-rewrites-entity-glue-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for glue-adjustment markers and notes."
  :group 'arxana-browser-rewrites)

(defface arxana-browser-rewrites-entity-placeholder-face
  '((t :inherit font-lock-comment-face))
  "Face for placeholder lines (e.g., '[e2 dropped]' on B side)."
  :group 'arxana-browser-rewrites)

(defun arxana-browser-rewrites--fill-paragraph-region (start end)
  "Soft-fill prose between START and END using the configured wrap column."
  (let ((fill-column arxana-browser-rewrites-wrap-column))
    (fill-region start end nil t)))

(defun arxana-browser-rewrites--insert-header (text)
  "Insert TEXT as a panel header."
  (let ((start (point)))
    (insert text "\n")
    (insert (make-string (length text) ?=) "\n\n")
    (add-text-properties start (point)
                         '(face arxana-browser-rewrites-header-face))))

(defun arxana-browser-rewrites--insert-field (label value)
  "Insert a labelled field (label in field-face)."
  (let ((start (point)))
    (insert label)
    (add-text-properties start (point)
                         '(face arxana-browser-rewrites-field-face))
    (insert ": " (if (stringp value) value (format "%S" value)) "\n")))

(defun arxana-browser-rewrites--fate->face (fate)
  "Map a fate keyword to an Emacs face symbol for entity body rendering."
  (pcase fate
    (:keep                       'arxana-browser-rewrites-entity-keep-face)
    (:keep-with-glue-adjustment  'arxana-browser-rewrites-entity-keep-face)
    (:drop                       'arxana-browser-rewrites-entity-drop-face)
    (:replace                    'arxana-browser-rewrites-entity-replace-face)
    (_                           'arxana-browser-rewrites-entity-keep-face)))

(defun arxana-browser-rewrites--glue-for (entity-id glue-adjustments)
  "Return the glue-adjustment plist whose :on equals ENTITY-ID, if any."
  (when glue-adjustments
    (cl-loop for g across glue-adjustments
             when (eq (plist-get g :on) entity-id)
             return g)))

(defun arxana-browser-rewrites--op-for (entity-id operations)
  "Return the operation plist targeting ENTITY-ID, if any."
  (when operations
    (cl-loop for op across operations
             when (eq (plist-get op :target) entity-id)
             return op)))

(defun arxana-browser-rewrites--display-name (sym)
  "Render keyword/symbol SYM as a display string without the leading `:`."
  (let ((s (if (symbolp sym) (symbol-name sym) (format "%s" sym))))
    (if (and (stringp s) (string-prefix-p ":" s)) (substring s 1) s)))

(defun arxana-browser-rewrites--insert-entity-header (id type fate-or-tag &optional extra)
  "Insert a single-line header for an entity block."
  (insert "[")
  (let ((id-start (point)))
    (insert (arxana-browser-rewrites--display-name (or id "?")))
    (add-text-properties id-start (point)
                         '(face arxana-browser-rewrites-entity-id-face)))
  (insert "] ")
  (let ((ty-start (point)))
    (insert (arxana-browser-rewrites--display-name (or type "")))
    (add-text-properties ty-start (point)
                         '(face arxana-browser-rewrites-entity-type-face)))
  (when fate-or-tag
    (insert (format "  %s" fate-or-tag)))
  (when extra
    (insert " ")
    (let ((extra-start (point)))
      (insert extra)
      (add-text-properties extra-start (point)
                           '(face arxana-browser-rewrites-entity-glue-face))))
  (insert "\n"))

(defun arxana-browser-rewrites--insert-entity-body (text face)
  "Insert ENTITY body text at point with FACE; soft-fill to wrap column."
  (let ((start (point)))
    (insert "  " (or text "") "\n")
    (add-text-properties start (point) `(face ,face))
    (arxana-browser-rewrites--fill-paragraph-region start (point))))

(defun arxana-browser-rewrites--insert-placeholder (text)
  "Insert a placeholder line (e.g. '[e2 dropped]') with placeholder face."
  (let ((start (point)))
    (insert "  " text "\n")
    (add-text-properties start (point)
                         '(face arxana-browser-rewrites-entity-placeholder-face))))

(defun arxana-browser-rewrites--render-a (annotation closure)
  "Render the Version A (pre-rewrite) panel from ANNOTATION + CLOSURE.
If :entities-before is present, render entity-by-entity (physicalized sheaf);
otherwise fall back to a single blob with diff-removed face."
  (let* ((corpus-id (arxana-browser-rewrites--current-corpus-id))
         (buf (get-buffer-create
               (arxana-browser-rewrites--buffer-name
                arxana-browser-rewrites-a-buffer corpus-id "A")))
         (before (plist-get closure :before))
         (entities (plist-get closure :entities-before))
         (operations (plist-get closure :operations))
         (glue (plist-get closure :glue-adjustments))
         (section (plist-get
                   (aref (plist-get annotation :endpoints) 0)
                   :section-id)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (arxana-browser-rewrites--insert-header "VERSION A (pre-rewrite)")
      (arxana-browser-rewrites--insert-field "Section" section)
      (arxana-browser-rewrites--insert-field "Annotation" (plist-get annotation :id))
      (insert "\n")
      (cond
       (entities
        ;; Physicalized sheaf: render each entity as a block with fate-coloured body.
        (cl-loop for e across entities do
                 (let* ((id (plist-get e :id))
                        (typ (plist-get e :type))
                        (fate (plist-get e :fate))
                        (span (plist-get e :span))
                        (op (arxana-browser-rewrites--op-for id operations))
                        (op-name (and op (plist-get op :op)))
                        (g (arxana-browser-rewrites--glue-for id glue))
                        (fate-tag
                         (cond ((eq fate :drop) "DROP")
                               ((eq fate :replace) "REPLACE")
                               ((eq fate :keep-with-glue-adjustment)
                                (if g
                                    (format "keep (glue → %s)" (plist-get g :effect))
                                  "keep (glue)"))
                               (t "keep"))))
                   (arxana-browser-rewrites--insert-entity-header
                    id typ fate-tag
                    (when op-name (format "← op=%s" op-name)))
                   (arxana-browser-rewrites--insert-entity-body
                    span
                    (arxana-browser-rewrites--fate->face fate))
                   (insert "\n"))))
       (t
        ;; Blob fallback (annotations without entity decomposition).
        (let ((start (point)))
          (insert (or before ""))
          (add-text-properties start (point)
                               '(face arxana-browser-rewrites-diff-del-face))
          (arxana-browser-rewrites--fill-paragraph-region start (point)))
        (insert "\n\n")))
      (arxana-browser-rewrites-mode)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (arxana-browser-rewrites--set-buffer-context
     buf corpus-id (plist-get annotation :id))
    buf))

(defun arxana-browser-rewrites--render-rule (annotation closure)
  "Render the centre RULE panel (the f:(A,B)->C trace)."
  (let* ((corpus-id (arxana-browser-rewrites--current-corpus-id))
         (buf (get-buffer-create
               (arxana-browser-rewrites--buffer-name
                arxana-browser-rewrites-rule-buffer corpus-id "Rule")))
        (pattern (plist-get closure :pattern-name))
        (composition-id (plist-get closure :composition-id))
        (composition-text (plist-get closure :composition-text))
        (augmentation (plist-get closure :augmentation))
        (delta (plist-get closure :invariant-delta))
        (verification (plist-get closure :verification))
        (enables (plist-get closure :enables))
        (decided-by (plist-get closure :decided-by))
        (timestamp (plist-get closure :timestamp))
        (severity (plist-get annotation :severity))
        (rewrite-trace (plist-get closure :rewrite-trace)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (arxana-browser-rewrites--insert-header "REWRITING RULE — f:(A,B)→C")
      (arxana-browser-rewrites--insert-field "Annotation" (plist-get annotation :id))
      (arxana-browser-rewrites--insert-field "Severity" severity)
      (insert "\n")
      (arxana-browser-rewrites--insert-header "A (diagnostic)")
      (let ((notes (plist-get annotation :note)))
        (when (stringp notes)
          (let ((start (point)))
            (insert notes "\n")
            (arxana-browser-rewrites--fill-paragraph-region start (point)))))
      (insert "\n")
      (arxana-browser-rewrites--insert-header "B (rewrite plan)")
      (arxana-browser-rewrites--insert-field "Pattern" pattern)
      (arxana-browser-rewrites--insert-field "Composition-id" composition-id)
      (when composition-text
        (let ((start (point)))
          (insert "Composition: " composition-text "\n")
          (arxana-browser-rewrites--fill-paragraph-region start (point))))
      (when augmentation
        (insert "Augmentation:\n")
        (let ((pl augmentation))
          (while pl
            (insert (format "  %S = %s\n" (car pl)
                            (let ((v (cadr pl)))
                              (if (stringp v) v (format "%S" v)))))
            (setq pl (cddr pl)))))
      (insert "\n")
      ;; Sheaf-of-rewrite section: entities, operations, glue-adjustments.
      (let ((entities (plist-get closure :entities-before))
            (operations (plist-get closure :operations))
            (glue (plist-get closure :glue-adjustments)))
        (when (or entities operations glue)
          (arxana-browser-rewrites--insert-header "Local sections (sheaf-of-rewrite)")
          (when entities
            (insert "Entities (typed open cover of the :before paragraph):\n")
            (cl-loop for e across entities do
                     (let ((id (plist-get e :id))
                           (typ (plist-get e :type))
                           (fate (plist-get e :fate))
                           (span (plist-get e :span)))
                       (insert (format "  %s [%s] %s\n     %s\n"
                                       (or id "?")
                                       (or typ "?")
                                       (or fate "?")
                                       (if (and (stringp span)
                                                (> (length span) 90))
                                           (concat (substring span 0 87) "...")
                                         (or span ""))))))
            (insert "\n"))
          (when operations
            (insert "Operations (local sections):\n")
            (cl-loop for op across operations do
                     (let ((op-type (plist-get op :op))
                           (target (plist-get op :target))
                           (rationale (plist-get op :rationale))
                           (with-entities (plist-get op :with-entities))
                           (pattern-ref (plist-get op :pattern-ref)))
                       (insert (format "  %s %s\n     rationale: %s\n"
                                       (or op-type "?")
                                       (or target "?")
                                       (or rationale "")))
                       (when pattern-ref
                         (insert (format "     pattern-ref: %s\n" pattern-ref)))
                       (when with-entities
                         (insert "     with-entities:\n")
                         (cl-loop for n across with-entities do
                                  (let ((nid (plist-get n :id))
                                        (nty (plist-get n :type))
                                        (faug (plist-get n :from-augmentation))
                                        (txt (plist-get n :text)))
                                    (insert (format "       %s [%s] from-augmentation=%s\n         %s\n"
                                                    (or nid "?")
                                                    (or nty "?")
                                                    (or faug "?")
                                                    (if (and (stringp txt)
                                                             (> (length txt) 80))
                                                        (concat (substring txt 0 77) "...")
                                                      (or txt "")))))))))
            (insert "\n"))
          (when glue
            (insert "Glue-adjustments (compatibility on overlaps):\n")
            (cl-loop for g across glue do
                     (let ((effect (plist-get g :effect))
                           (on (plist-get g :on))
                           (caused-by (plist-get g :caused-by))
                           (note (plist-get g :note)))
                       (insert (format "  %s on %s (caused-by %s)\n"
                                       (or effect "?")
                                       (or on "?")
                                       (or caused-by "?")))
                       (when note
                         (let ((start (point)))
                           (insert (format "    %s\n" note))
                           (arxana-browser-rewrites--fill-paragraph-region start (point))))))
            (insert "\n"))))
      (arxana-browser-rewrites--insert-header "C (verified output)")
      (when delta
        (insert "Invariant-delta:\n")
        (let ((pl delta))
          (while pl
            (insert (format "  %S : %S\n" (car pl) (cadr pl)))
            (setq pl (cddr pl)))))
      (when verification
        (insert "Verification:\n")
        (let ((pl verification))
          (while pl
            (insert (format "  %S = %s\n" (car pl)
                            (let ((v (cadr pl)))
                              (if (stringp v) v (format "%S" v)))))
            (setq pl (cddr pl)))))
      (when enables
        (insert "\nEnables (one-step-lookahead integration points):\n")
        (cl-loop for e across enables do
                 (let ((point (plist-get e :point))
                       (desc (plist-get e :description))
                       (status (plist-get e :status))
                       (blocker (plist-get e :blocker)))
                   (insert (format "  %s\n" (or point "?")))
                   (when desc
                     (let ((start (point)))
                       (insert (format "    description: %s\n" desc))
                       (arxana-browser-rewrites--fill-paragraph-region start (point))))
                   (when status
                     (insert (format "    status: %s\n" status)))
                   (when blocker
                     (let ((start (point)))
                       (insert (format "    blocker: %s\n"
                                       (if (stringp blocker)
                                           blocker
                                         (format "%S" blocker))))
                       (arxana-browser-rewrites--fill-paragraph-region start (point)))))))
      (insert "\n")
      (arxana-browser-rewrites--insert-field "Decided-by" decided-by)
      (arxana-browser-rewrites--insert-field "Timestamp" timestamp)
      (when rewrite-trace
        (insert "\n")
        (let ((start (point)))
          (insert "Trace: " rewrite-trace "\n")
          (arxana-browser-rewrites--fill-paragraph-region start (point))))
      (insert "\n")
      (arxana-browser-rewrites--insert-header "Actions")
      (insert "  a  accept       r  reject       e  edit\n")
      (insert "  d  defer        p  promote      n  next       b  back\n")
      (insert "  q  quit         g  reload       RET  open pattern flexiarg\n")
      (arxana-browser-rewrites-mode)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (arxana-browser-rewrites--set-buffer-context
     buf corpus-id (plist-get annotation :id))
    buf))

(defun arxana-browser-rewrites--render-b (annotation closure)
  "Render the Version B (post-rewrite) panel.
If :entities-before is present, walk it and emit operation-aware blocks:
KEEP entities show their span (with glue-adjustment markers); DROP shows
a placeholder; REPLACE shows a placeholder followed by ADDED entities."
  (let* ((corpus-id (arxana-browser-rewrites--current-corpus-id))
         (buf (get-buffer-create
               (arxana-browser-rewrites--buffer-name
                arxana-browser-rewrites-b-buffer corpus-id "B")))
         (after (plist-get closure :after))
         (entities (plist-get closure :entities-before))
         (operations (plist-get closure :operations))
         (glue (plist-get closure :glue-adjustments))
         (section (plist-get
                   (aref (plist-get annotation :endpoints) 0)
                   :section-id)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (arxana-browser-rewrites--insert-header "VERSION B (post-rewrite)")
      (arxana-browser-rewrites--insert-field "Section" section)
      (arxana-browser-rewrites--insert-field "Annotation" (plist-get annotation :id))
      (insert "\n")
      (cond
       (entities
        (cl-loop for e across entities do
                 (let* ((id (plist-get e :id))
                        (typ (plist-get e :type))
                        (fate (plist-get e :fate))
                        (span (plist-get e :span))
                        (op (arxana-browser-rewrites--op-for id operations))
                        (g (arxana-browser-rewrites--glue-for id glue)))
                   (cond
                    ;; Dropped: no body, just a placeholder line.
                    ((eq fate :drop)
                     (arxana-browser-rewrites--insert-entity-header
                      id typ "(dropped)" nil)
                     (arxana-browser-rewrites--insert-placeholder
                      (format "↳ removed; rationale: %s"
                              (or (and op (plist-get op :rationale))
                                  "—")))
                     (insert "\n"))
                    ;; Replaced: placeholder line + ADDED entity blocks immediately below.
                    ((eq fate :replace)
                     (arxana-browser-rewrites--insert-entity-header
                      id typ "(replaced)" nil)
                     (arxana-browser-rewrites--insert-placeholder
                      (format "↳ replaced; rationale: %s"
                              (or (and op (plist-get op :rationale))
                                  "—")))
                     (when (and op (plist-get op :with-entities))
                       (cl-loop for n across (plist-get op :with-entities) do
                                (let ((nid (plist-get n :id))
                                      (nty (plist-get n :type))
                                      (faug (plist-get n :from-augmentation))
                                      (txt (plist-get n :text)))
                                  (arxana-browser-rewrites--insert-entity-header
                                   nid nty "ADD"
                                   (when faug
                                     (format "← augmentation.%s"
                                             (substring (symbol-name faug) 1))))
                                  (arxana-browser-rewrites--insert-entity-body
                                   txt 'arxana-browser-rewrites-entity-add-face))))
                     (insert "\n"))
                    ;; Kept (possibly with glue-adjustment).
                    (t
                     (let ((fate-tag
                            (cond ((eq fate :keep-with-glue-adjustment)
                                   (if g
                                       (format "kept (glue → %s)" (plist-get g :effect))
                                     "kept (glue)"))
                                  (t "kept"))))
                       (arxana-browser-rewrites--insert-entity-header
                        id typ fate-tag nil))
                     (arxana-browser-rewrites--insert-entity-body
                      span 'arxana-browser-rewrites-entity-keep-face)
                     (when g
                       (arxana-browser-rewrites--insert-placeholder
                        (format "  glue: %s — caused-by %s"
                                (plist-get g :effect)
                                (plist-get g :caused-by))))
                     (insert "\n"))))))
       (t
        ;; Blob fallback.
        (let ((start (point)))
          (if (stringp after)
              (insert after)
            (insert "(no :after recorded — possibly a deletion-only rewrite)"))
          (add-text-properties start (point)
                               '(face arxana-browser-rewrites-diff-add-face))
          (arxana-browser-rewrites--fill-paragraph-region start (point)))
        (insert "\n\n")))
      (arxana-browser-rewrites-mode)
      (setq buffer-read-only t)
      (goto-char (point-min)))
    (arxana-browser-rewrites--set-buffer-context
     buf corpus-id (plist-get annotation :id))
    buf))

;; -----------------------------------------------------------------
;; 3-up frame layout
;; -----------------------------------------------------------------

(defun arxana-browser-rewrites--ensure-frame ()
  "Return (creating if needed) the dedicated rewrites frame."
  (let ((existing (cl-find-if
                   (lambda (f)
                     (equal (frame-parameter f 'name)
                            arxana-browser-rewrites-frame-name))
                   (frame-list))))
    (or existing
        (make-frame
         (list (cons 'name arxana-browser-rewrites-frame-name))))))

(defun arxana-browser-rewrites--display-3up (a-buf rule-buf b-buf)
  "Lay A-BUF, RULE-BUF, B-BUF side-by-side in the rewrites frame."
  (let ((frame (arxana-browser-rewrites--ensure-frame)))
    (select-frame frame)
    (delete-other-windows)
    (switch-to-buffer a-buf)
    (let* ((width (frame-width frame))
           (third (max 30 (/ width 3)))
           (w-a (selected-window))
           (w-rule (split-window-right third))
           (w-b nil))
      (select-window w-rule)
      (switch-to-buffer rule-buf)
      (setq w-b (split-window-right (max 30 (/ width 3))))
      (select-window w-b)
      (switch-to-buffer b-buf)
      (select-window w-a))))

;; -----------------------------------------------------------------
;; Entry point + navigation
;; -----------------------------------------------------------------

(defun arxana-browser-rewrites--show-id (id)
  "Render the annotation with :id ID across the three panels."
  (let* ((ann (arxana-browser-rewrites--find id))
         (closure (and ann (plist-get ann :closure))))
    (cond
     ((null ann)
      (user-error "No annotation with :id %s" id))
     ((null closure)
      (user-error "Annotation %s has no :closure (status %s — open?)"
                  id (plist-get ann :status)))
     (t
      (let ((a-buf (arxana-browser-rewrites--render-a ann closure))
            (rule-buf (arxana-browser-rewrites--render-rule ann closure))
            (b-buf (arxana-browser-rewrites--render-b ann closure)))
        (arxana-browser-rewrites--display-3up a-buf rule-buf b-buf)
        (setq arxana-browser-rewrites--current-id id)
        (message "Reviewing rewrite: %s" id))))))

;;;###autoload
(defun arxana-browser-rewrites-open (&optional id corpus-id)
  "Open the 3-up rewrite review surface for annotation with :id ID.
If ID is nil, prompt with the list of closed annotations.
Always re-reads the .edn so external edits are picked up.

Annotation ids are strings as stored in the .edn (e.g.
\"hx:wp:v12:s1-1-local-self-eval-parked\")."
  (interactive)
  (let ((corpus (arxana-browser-rewrites--resolve-corpus
                 (or corpus-id
                     (arxana-browser-rewrites--current-corpus-id)))))
    (when (arxana-browser-rewrites-load (plist-get corpus :id))
      (let ((id (or id
                    (completing-read
                     (format "Annotation id (%s): " (plist-get corpus :name))
                     arxana-browser-rewrites--ordered-ids
                     nil t))))
        (arxana-browser-rewrites--show-id id)))))

(defun arxana-browser-rewrites-select-corpus ()
  "Prompt for a corpus and switch the RewriteReview surface to it."
  (interactive)
  (let* ((choices (mapcar (lambda (corpus)
                            (cons (plist-get corpus :name)
                                  (plist-get corpus :id)))
                          arxana-browser-rewrites-corpora))
         (selected-name
          (completing-read "Corpus: " (mapcar #'car choices) nil t))
         (selected-id (cdr (assoc selected-name choices))))
    (arxana-browser-rewrites-open nil selected-id)))

(defun arxana-browser-rewrites-next ()
  "Advance to the next closed annotation."
  (interactive)
  (when (arxana-browser-rewrites-load
         (arxana-browser-rewrites--current-corpus-id))
    (let* ((ids arxana-browser-rewrites--ordered-ids)
           (pos (cl-position arxana-browser-rewrites--current-id ids :test #'equal))
           (next-id (and pos (nth (1+ pos) ids))))
      (if next-id
          (arxana-browser-rewrites--show-id next-id)
        (message "No further closed annotations.")))))

(defun arxana-browser-rewrites-previous ()
  "Step back to the previous closed annotation."
  (interactive)
  (when (arxana-browser-rewrites-load
         (arxana-browser-rewrites--current-corpus-id))
    (let* ((ids arxana-browser-rewrites--ordered-ids)
           (pos (cl-position arxana-browser-rewrites--current-id ids :test #'equal))
           (prev-id (and pos (> pos 0) (nth (1- pos) ids))))
      (if prev-id
          (arxana-browser-rewrites--show-id prev-id)
        (message "No previous closed annotation.")))))

;; -----------------------------------------------------------------
;; Action stubs
;; -----------------------------------------------------------------

(defun arxana-browser-rewrites-accept ()
  "Stub: accept the current rewrite. Will write-back to the .edn once wired."
  (interactive)
  (message "[stub] accept %s — write-back not yet implemented"
           arxana-browser-rewrites--current-id))

(defun arxana-browser-rewrites-reject ()
  "Stub: reject the current rewrite."
  (interactive)
  (message "[stub] reject %s — revert not yet implemented"
           arxana-browser-rewrites--current-id))

(defun arxana-browser-rewrites-edit ()
  "Stub: edit the closure block for the current rewrite."
  (interactive)
  (message "[stub] edit %s — edit-closure form not yet implemented"
           arxana-browser-rewrites--current-id))

(defun arxana-browser-rewrites-defer ()
  "Stub: defer the current rewrite review."
  (interactive)
  (message "[stub] defer %s" arxana-browser-rewrites--current-id))

(defun arxana-browser-rewrites-promote ()
  "Stub: capture a finding from the current rewrite for promotion."
  (interactive)
  (message "[stub] promote finding from %s — promotion form not yet implemented"
           arxana-browser-rewrites--current-id))

(defun arxana-browser-rewrites-open-pattern ()
  "Open the cited pattern's flexiarg in another window."
  (interactive)
  (when (arxana-browser-rewrites-load
         (arxana-browser-rewrites--current-corpus-id))
    (let* ((ann (arxana-browser-rewrites--find arxana-browser-rewrites--current-id))
           (closure (and ann (plist-get ann :closure)))
           (pattern (and closure (plist-get closure :pattern-name)))
           (library-root "/home/joe/code/futon3/library/"))
      (if (stringp pattern)
          (let ((path (concat library-root pattern ".flexiarg")))
            (if (file-exists-p path)
                (find-file-other-window path)
              (user-error "Pattern file not found: %s" path)))
        (user-error "No pattern-name on current annotation's closure")))))

;; -----------------------------------------------------------------
;; Major mode for the panels
;; -----------------------------------------------------------------

(defvar arxana-browser-rewrites-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'arxana-browser-rewrites-accept)
    (define-key map (kbd "r") #'arxana-browser-rewrites-reject)
    (define-key map (kbd "e") #'arxana-browser-rewrites-edit)
    (define-key map (kbd "d") #'arxana-browser-rewrites-defer)
    (define-key map (kbd "p") #'arxana-browser-rewrites-promote)
    (define-key map (kbd "n") #'arxana-browser-rewrites-next)
    (define-key map (kbd "b") #'arxana-browser-rewrites-previous)
    (define-key map (kbd "g") #'arxana-browser-rewrites-load)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "RET") #'arxana-browser-rewrites-open-pattern)
    map)
  "Keymap for `arxana-browser-rewrites-mode'.")

(define-derived-mode arxana-browser-rewrites-mode special-mode
  "RewriteReview"
  "Major mode for the Arxana 3-up rewrite review panels."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(provide 'arxana-browser-rewrites)

;;; arxana-browser-rewrites.el ends here
