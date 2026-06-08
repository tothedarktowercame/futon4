;;; arxana-browser-essays-compiled.el --- Compiled-view (whole-essay) surface for Arxana Essays  -*- lexical-binding: t; -*-

;; Author: claude-8 for Joe
;; Date: 2026-05-20
;; Status: Phase 1 of the compiled-view feature.  Read-only.
;; Companion handoff: ~/code/algorithms/arxana-essays-compiled-view-handoff.md
;;
;; Phase 1 purpose: open the entire .md prose of an essay in a single buffer
;; with annotation overlays placed at their :passage anchors, so cross-section
;; diagnostics (e.g. scope-mush-naming spanning 3 endpoints; section-bridge-missing
;; spanning 4 endpoints) are visible at once.  Read-only — no save-back yet
;; (that's Phase 2).
;;
;; Companion to arxana-browser-essays.el (per-section view); this surface does
;; NOT replace that one — they coexist.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'arxana-browser-essays)
(require 'arxana-browser-rewrites)  ; for --read-edn-file

(defgroup arxana-browser-essays-compiled nil
  "Compiled-view (whole-essay) surface for Arxana Essays."
  :group 'arxana-browser-essays)

(defcustom arxana-browser-essays-compiled-buffer-format "*Arxana Essay (compiled): %s*"
  "Buffer-name format string for compiled-view buffers.
%s is replaced with the essay's :label."
  :type 'string
  :group 'arxana-browser-essays-compiled)

(defcustom arxana-browser-essays-compiled-notes-buffer "*Arxana Essay Compiled Notes*"
  "Buffer name for the side-pane annotation notes in the compiled view."
  :type 'string
  :group 'arxana-browser-essays-compiled)

(defcustom arxana-browser-essays-compiled-notes-width 0.42
  "Fractional width of the notes side window."
  :type 'number
  :group 'arxana-browser-essays-compiled)

(defface arxana-browser-essays-compiled-multi-anchor-face
  '((t :background "#fde68a"))
  "Face for annotation anchors that belong to a multi-endpoint annotation
(e.g. cross-section diagnostics)."
  :group 'arxana-browser-essays-compiled)

(defface arxana-browser-essays-compiled-aif-witness-face
  '((t :background "#dcfce7"))
  "Face for AIF² invariant-witness annotations (typically protect-during-rewrites)."
  :group 'arxana-browser-essays-compiled)

(defface arxana-browser-essays-compiled-orphan-face
  '((t :inherit shadow :slant italic))
  "Face for annotation rows whose :passage could not be located in the prose."
  :group 'arxana-browser-essays-compiled)

(defface arxana-browser-essays-compiled-resolved-face
  '((t :foreground "#15803d" :weight bold))
  "Face for the resolution badge on annotations with editorial-log entries."
  :group 'arxana-browser-essays-compiled)

(defface arxana-browser-essays-compiled-resolved-anchor-face
  '((t :underline t))
  "Face for prose overlays whose annotation has been resolved but whose
anchor is still present in the prose.  Underline-only — no background
highlight — distinguishes resolved-anchored (historical editorial link)
from unresolved-anchored (live concern to act on), without erasing the
anchor altogether (the way :status :retracted would)."
  :group 'arxana-browser-essays-compiled)

(defface arxana-browser-essays-compiled-resolved-rationale-face
  '((t :foreground "#15803d" :slant italic))
  "Face for the rationale text rendered under a resolved annotation."
  :group 'arxana-browser-essays-compiled)

(defface arxana-invisible-ink
  '((t :foreground "#fde047"))
  "Face for compiled-view text marked as \"invisible ink\" — present in
the source but absent from the rendered output (e.g. a two-pager PDF).
Light-yellow foreground gives a ghosted, hard-to-read appearance that
signals \"this prose didn't make the cut\" without removing it from
view.  Applied as the trailing face in a face list so an underlying
annotation overlay's background still shows through."
  :group 'arxana-browser-essays-compiled)

(defcustom arxana-browser-essays-compiled-invisible-ink-hx-types
  '("annotation/twopager-cut"
    "annotation/twopager-scaffold-stripped"
    "annotation/invisible-ink")
  "Annotation `:hx-type' strings whose annotated passages should display
as invisible ink in the compiled view.  Extend this list (or apply the
\"invisible-ink\" label to an annotation) to mark additional content as
present-in-source-but-absent-from-output."
  :type '(repeat string)
  :group 'arxana-browser-essays-compiled)

(defun arxana-browser-essays-compiled--invisible-ink-p (ann)
  "Return non-nil if ANN should be rendered as invisible ink.
Matches when the annotation's `:hx-type' is in
`arxana-browser-essays-compiled-invisible-ink-hx-types', OR the
annotation carries the \"invisible-ink\" string in its `:labels'."
  (let ((hx (plist-get ann :hx-type))
        (labels (plist-get ann :labels)))
    (or (and hx (member hx arxana-browser-essays-compiled-invisible-ink-hx-types))
        (and labels (member "invisible-ink" labels)))))

;; -----------------------------------------------------------------
;; Persistence: user-annotations sidecar
;; -----------------------------------------------------------------
;;
;; `arxana-invisible-ink-region' mutates the in-memory annotation list
;; (`--annotations').  Without disk persistence those changes vanish on
;; buffer kill or Emacs restart.  The sidecar pattern:
;;
;;   - Path: <source-file-dir>/<source-file-basename>.user-annotations.el
;;   - Shape: a single sexp = plist with two fields
;;       :new-annotations  — list of annotation plists not in the canonical
;;                            manifest (each with :id starting "hx:invisible-ink:")
;;       :label-additions  — alist (ann-id . (added-labels)) extending
;;                            canonical annotations' :labels in-memory
;;   - Lifecycle: written on every `arxana-invisible-ink-region' call;
;;     read at `arxana-browser-essays-open-compiled' time and merged
;;     into the annotation list before overlays are placed.
;;
;; The main `<essay>-annotations.el' manifest is never modified — Joe's
;; hand-written notes inside the canonical `:annotations' list are safe.
;; To revert, delete the sidecar.

(defvar-local arxana-browser-essays-compiled--user-annotation-changes nil
  "Buffer-local plist tracking operator-added invisible-ink changes.
Keys: :new-annotations (list), :label-additions (alist of ann-id → labels-added).
Persisted via `--write-user-annotations-sidecar' on every change.")

(defun arxana-browser-essays-compiled--user-annotations-sidecar-path (source-file)
  "Return the sidecar path for SOURCE-FILE, or nil if SOURCE-FILE is unusable.
The sidecar lives next to the source .md, named `<basename>.user-annotations.el'."
  (when (and source-file (stringp source-file))
    (expand-file-name
     (concat (file-name-base source-file) ".user-annotations.el")
     (file-name-directory source-file))))

(defun arxana-browser-essays-compiled--read-user-annotations-sidecar (path)
  "Read sidecar at PATH and return its plist payload, or nil if unreadable / malformed."
  (when (and path (file-readable-p path))
    (condition-case _
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          ;; Skip leading comments / whitespace before the data sexp.
          (while (and (not (eobp))
                      (looking-at "[[:space:]]*\\(;.*\n\\|\n\\)"))
            (forward-line 1))
          (read (current-buffer)))
      (error nil))))

(defun arxana-browser-essays-compiled--write-user-annotations-sidecar (path payload)
  "Write PAYLOAD plist to sidecar PATH with a friendly header.
PAYLOAD must be a plist with :new-annotations and :label-additions fields."
  (when path
    (with-temp-buffer
      (insert
       (format ";;; %s --- Operator-added invisible-ink annotations -*- lexical-binding: t; -*-\n"
               (file-name-nondirectory path)))
      (insert ";;\n")
      (insert ";; Auto-generated by `arxana-invisible-ink-region'.\n")
      (insert ";; Loaded by `arxana-browser-essays-open-compiled' and merged into\n")
      (insert ";; the in-memory annotations for this essay's compiled view.\n")
      (insert ";;\n")
      (insert ";; Hand-editable but Arxana will overwrite this file on the next\n")
      (insert ";; invisible-ink mark.  To revert: delete this file.\n")
      (insert ";;\n\n")
      (let ((print-length nil)
            (print-level nil)
            (print-escape-newlines t)
            (print-escape-control-characters t))
        (pp payload (current-buffer)))
      (insert "\n")
      (write-region (point-min) (point-max) path nil 'silent))))

(defun arxana-browser-essays-compiled--apply-user-annotations (anns sidecar-payload)
  "Return ANNS augmented with SIDECAR-PAYLOAD's new annotations + label additions.
Non-destructive on ANNS: new annotations are appended; canonical annotations
gain extra labels via fresh plist-put.  Returns the merged list."
  (let ((new-anns (plist-get sidecar-payload :new-annotations))
        (label-adds (plist-get sidecar-payload :label-additions))
        (result (mapcar (lambda (a) (copy-sequence a)) anns)))
    (dolist (entry label-adds)
      (let* ((id (car entry))
             (added (cdr entry))
             (ann (cl-find-if (lambda (a) (equal (plist-get a :id) id)) result)))
        (when ann
          (let ((existing (copy-sequence (plist-get ann :labels))))
            (dolist (lbl added)
              (unless (member lbl existing)
                (setq existing (append existing (list lbl)))))
            (plist-put ann :labels existing)))))
    (append result (mapcar (lambda (a) (copy-sequence a)) new-anns))))

(defun arxana-browser-essays-compiled--persist-user-annotations ()
  "Write the current buffer-local `--user-annotation-changes' to its sidecar.
Returns the sidecar path written, or nil if no source-file is bound."
  (let ((path (arxana-browser-essays-compiled--user-annotations-sidecar-path
               arxana-browser-essays-compiled--source-file)))
    (when path
      (arxana-browser-essays-compiled--write-user-annotations-sidecar
       path arxana-browser-essays-compiled--user-annotation-changes)
      path)))

;; -----------------------------------------------------------------
;; Buffer-local state
;; -----------------------------------------------------------------

(defvar-local arxana-browser-essays-compiled--essay-id nil)
(defvar-local arxana-browser-essays-compiled--source-file nil)
(defvar-local arxana-browser-essays-compiled--annotations nil
  "Annotation plists for the current compiled buffer.")
(defvar-local arxana-browser-essays-compiled--ordered-positions nil
  "List of (POSITION . ANN-ID) pairs sorted by POSITION; basis for n/p navigation.")
(defvar-local arxana-browser-essays-compiled--last-followed-id nil
  "Last annotation :id followed by the post-command cursor sync; nil if none.")
(defvar arxana-browser-essays-compiled--notes-active-overlay nil
  "Buffer-local-in-notes-buffer overlay tracking the currently-highlighted entry.")

;; -----------------------------------------------------------------
;; Annotation source resolution
;; -----------------------------------------------------------------

(defun arxana-browser-essays-compiled--edn-annotation-essay-id (edn)
  "Return the :essay-id an annotations EDN is pinned to, via a cheap head scan.
Deliberately does NOT use the full EDN reader: that reader is hand-rolled and
can loop on non-annotation .edn (e.g. a large `.aif.edn' graph) that happens to
sit in the same directory.  Only the first 4 KB are scanned for the literal key."
  (when (and edn (file-readable-p edn))
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents edn nil 0 4096)
        (goto-char (point-min))
        (when (re-search-forward ":essay-id[[:space:]]+\"\\([^\"]+\\)\"" nil t)
          (match-string 1))))))

(defun arxana-browser-essays-compiled--edn-sibling-path (source-file &optional essay-id)
  "Return the annotations .edn for SOURCE-FILE, version-aware via ESSAY-ID.

A multi-version essay family shares ONE directory, so a single
`annotations.edn' would bleed one version's annotations onto the prose of
another (e.g. v1's eightfold-path set showing over v5's form prose).
Resolution order:
  1. `<source-basename>.annotations.edn' sibling (per-version, predictable);
  2. any `*.edn' in the directory whose `:essay-id' equals ESSAY-ID — e.g. a
     `coherence-annotations-<ver>.edn' produced by a later pass;
  3. the shared `annotations.edn', but ONLY when it is not pinned to a
     DIFFERENT essay-id (so v1's set never bleeds onto v5)."
  (when (and source-file (stringp source-file))
    (let* ((dir (file-name-directory source-file))
           (per-version (expand-file-name
                         (concat (file-name-base source-file) ".annotations.edn")
                         dir)))
      (cond
       ((file-readable-p per-version) per-version)
       ((when essay-id
          (seq-find (lambda (edn)
                      (equal essay-id
                             (arxana-browser-essays-compiled--edn-annotation-essay-id edn)))
                    (ignore-errors (directory-files dir t "\\.edn\\'")))))
       (t (let ((shared (expand-file-name "annotations.edn" dir)))
            (when (and (file-readable-p shared)
                       (let ((sid (arxana-browser-essays-compiled--edn-annotation-essay-id shared)))
                         (or (null sid) (null essay-id) (equal sid essay-id))))
              shared)))))))

(defun arxana-browser-essays-compiled--load-annotations (essay-id source-file)
  "Return a list of annotation plists for ESSAY-ID.

Preference order: sibling annotations.edn beside SOURCE-FILE; otherwise the
.el manifest's :annotations field.  Annotations from .edn are returned
verbatim (plists); .el manifest annotations are returned as-is (also plists,
but with a different field shape — see arxana-browser-essays.el).

If the editorial module is loaded
\(`arxana-vsatarcs-editorial-decorate-annotations' available), each
annotation is additionally augmented with `:resolution-state' etc. from
the sibling editorial-log.edn.  This is the address-then-orphan-cycle
plumbing: even an annotation whose passage no longer matches the prose
(orphan) carries its resolution state to the notes pane, so the
operator can see at a glance that it has been actioned."
  (let* ((edn-path (arxana-browser-essays-compiled--edn-sibling-path source-file essay-id))
         (anns
          (cond
           (edn-path
            (let* ((data (arxana-browser-rewrites--read-edn-file edn-path))
                   (anns (plist-get data :annotations)))
              (append anns nil)))
           (t
            (let ((manifest (arxana-browser-essays--manifest-for essay-id)))
              (append (plist-get manifest :annotations) nil)))))
         (anns (if (and edn-path
                        (fboundp 'arxana-vsatarcs-editorial-decorate-annotations))
                   (arxana-vsatarcs-editorial-decorate-annotations anns edn-path)
                 anns))
         ;; Merge in the operator's invisible-ink sidecar so user-added
         ;; markings survive across sessions.  The sidecar lives next to
         ;; the source .md; see `--user-annotations-sidecar-path'.
         (sidecar-path
          (arxana-browser-essays-compiled--user-annotations-sidecar-path source-file))
         (sidecar-payload
          (arxana-browser-essays-compiled--read-user-annotations-sidecar sidecar-path)))
    (if sidecar-payload
        (arxana-browser-essays-compiled--apply-user-annotations anns sidecar-payload)
      anns)))

;; -----------------------------------------------------------------
;; Endpoint passage extraction (handles .edn-shape and .el-shape)
;; -----------------------------------------------------------------

(defun arxana-browser-essays-compiled--annotation-passages (ann)
  "Return a list of annotated passage strings declared by ANN.

Supports both schemas:
  - .edn:  :endpoints [{:role :annotated :passage \"...\"} ...]
  - .el:   :annotated (:entity-id ... :passage \"...\")"
  (mapcar #'car (arxana-browser-essays-compiled--annotated-endpoint-pairs ann)))

(defun arxana-browser-essays-compiled--annotated-endpoint-pairs (ann)
  "Return ((PASSAGE . ENDPOINT-INDEX) ...) for ANN's annotated endpoints.

ENDPOINT-INDEX is the 0-based position in the .edn `:endpoints' vector
\(or -1 for `.el'-shape annotations whose single `:annotated' field has
no analogous index).  Used by the passage-sync path to address the right
endpoint in `annotations.edn' when rewriting on save."
  (let (pairs)
    ;; .edn shape — walk endpoints with index in the FULL vector
    (let ((eps (plist-get ann :endpoints)))
      (when eps
        (let ((i 0))
          (dolist (ep (append eps nil))
            (when (eq (plist-get ep :role) :annotated)
              (let ((p (plist-get ep :passage)))
                (when (and p (stringp p) (not (string-empty-p p)))
                  (push (cons p i) pairs))))
            (setq i (1+ i))))))
    ;; .el shape — single passage; use -1 to mark non-indexed
    (let ((annotated (plist-get ann :annotated)))
      (when annotated
        (let ((p (plist-get annotated :passage)))
          (when (and p (stringp p) (not (string-empty-p p)))
            (push (cons p -1) pairs)))))
    (nreverse pairs)))

(defun arxana-browser-essays-compiled--annotation-source-pattern (ann)
  "Return the source pattern-name string for ANN, or nil."
  (let ((eps (plist-get ann :endpoints)))
    (when eps
      (cl-some (lambda (ep)
                 (and (eq (plist-get ep :role) :source)
                      (plist-get ep :pattern-name)))
               (append eps nil))))
  (or (let ((src (plist-get ann :source)))
        (and src (plist-get src :pattern-name)))))

;; -----------------------------------------------------------------
;; Overlay placement
;; -----------------------------------------------------------------

(defun arxana-browser-essays-compiled--passage-face (ann passages-for-this-ann)
  "Return the face to use for an annotation overlay.

PASSAGES-FOR-THIS-ANN is the count of :annotated passages on this annotation
(>= 2 → multi-anchor, gets the distinct face).

If the annotation carries a resolution-state in the anchor-releasing set
\(:addressed, :addressed-differently, :rejected), the overlay uses the
resolved-anchor face (underline only) — historical link preserved without
the live-highlight background.

If `--invisible-ink-p' is true for ANN, the result is a face LIST that
composes the base face with `arxana-invisible-ink' as the trailing
override; this gives the prose a faded foreground while preserving the
base annotation background."
  (let* ((hx-type (plist-get ann :hx-type))
         (res-state (plist-get ann :resolution-state))
         (base
          (cond
           ((memq res-state arxana-browser-essays-compiled--anchor-releasing-resolutions)
            'arxana-browser-essays-compiled-resolved-anchor-face)
           ((or (eq hx-type :aif/invariant-witness)
                (eq hx-type :aif/role)
                (eq hx-type :aif/boundary)
                (eq hx-type :aif/timescale))
            'arxana-browser-essays-compiled-aif-witness-face)
           ((>= passages-for-this-ann 2)
            'arxana-browser-essays-compiled-multi-anchor-face)
           ((eq hx-type :annotation/comment)
            'arxana-browser-essays-comment-face)
           (t
            'arxana-browser-essays-annotation-face))))
    (if (arxana-browser-essays-compiled--invisible-ink-p ann)
        (list base 'arxana-invisible-ink)
      base)))

(defun arxana-browser-essays-compiled--locate-near-suffix (passage)
  "Locate PASSAGE in the current buffer via suffix-first bracketing.

Strategy: find the longest suffix of PASSAGE that matches in the buffer
\(60/40/20/10/5 ladder), then search BACKWARD from the suffix start for
the longest prefix of PASSAGE within 2 × passage-length chars before the
suffix.  Bracket between them.

This avoids the wrong-place hazard that affects the symmetric
`arxana-browser-essays--locate-passage-bounds-lax': forward prefix search
returns the FIRST occurrence of any matching prefix length, which for
short ladder rungs (e.g. 5 chars) can match elsewhere in the document.
Searching backward from a high-confidence suffix anchor constrains the
prefix to its actual neighbourhood.

Returns (BEG . END . QUALITY) where QUALITY is `near-suffix-PL-SL', or nil."
  (when (and passage (stringp passage) (not (string-empty-p passage)))
    (let* ((plen (length passage))
           (ladder '(60 40 20 10 5))
           (suffix-match
            (cl-some
             (lambda (n)
               (when (>= plen n)
                 (let ((suff (substring passage (- plen n))))
                   (save-excursion
                     (goto-char (point-min))
                     (when (search-forward suff nil t)
                       (cons (match-beginning 0) n))))))
             ladder)))
      (when suffix-match
        (let* ((suff-pos (car suffix-match))
               (suff-len (cdr suffix-match))
               (suff-end (+ suff-pos suff-len))
               (back-limit (max (point-min) (- suff-pos (* 2 plen))))
               (prefix-match
                (cl-some
                 (lambda (n)
                   (when (>= plen n)
                     (let ((pref (substring passage 0 n)))
                       (save-excursion
                         (goto-char suff-pos)
                         (when (search-backward pref back-limit t)
                           (cons (match-beginning 0) n))))))
                 ladder)))
          (when (and prefix-match
                     (< (car prefix-match) suff-pos))
            (list (car prefix-match)
                  suff-end
                  (intern (format "near-suffix-%d-%d"
                                  (cdr prefix-match)
                                  suff-len)))))))))

(defun arxana-browser-essays-compiled--place-overlay (ann passage face &optional endpoint-index)
  "Search for PASSAGE in the current buffer and place an overlay tagged with ANN.
Tries exact match first; on miss, falls back to a suffix-first lax bracket
\(see `arxana-browser-essays-compiled--locate-near-suffix') so small edits
inside an annotated passage don't drop the overlay.  The overlay's
`arxana-essays-compiled-quality' property records the match quality
\(`exact' or `near-suffix-PL-SL').
Return the overlay if placed, else nil."
  (save-excursion
    (goto-char (point-min))
    (let* ((exact (search-forward passage nil t))
           (near  (and (not exact)
                       (save-excursion
                         (arxana-browser-essays-compiled--locate-near-suffix
                          passage))))
           (bounds (cond
                    (exact (cons (match-beginning 0) (match-end 0)))
                    (near  (cons (nth 0 near) (nth 1 near)))))
           (quality (cond
                     (exact 'exact)
                     (near  (nth 2 near)))))
      (when bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               ;; Default marker advance (nil/nil), no `evaporate'.
               ;; Mid-overlay character edits still grow/shrink the overlay
               ;; (Emacs marker default: inserts BETWEEN markers shift the
               ;; end forward).  Wholesale delete-and-retype collapses the
               ;; overlay to zero length → orphan in notes pane → operator
               ;; re-anchors via `arxana-browser-essays-compiled-reanchor-region'
               ;; or resolves the annotation.  Adjacent inserts (at overlay
               ;; end) do NOT grow the overlay, so an unrelated annotation
               ;; can't absorb prose just because it started where text was
               ;; deleted.
               (ov (make-overlay beg end))
               (note (plist-get ann :note))
               (ann-id (plist-get ann :id))
               (severity (plist-get ann :severity))
               (status (plist-get ann :status)))
          (overlay-put ov 'face face)
          (overlay-put ov 'arxana-essays-compiled-ann-id ann-id)
          (overlay-put ov 'arxana-essays-compiled-endpoint-index
                       (or endpoint-index -1))
          (overlay-put ov 'arxana-essays-compiled-position beg)
          (overlay-put ov 'arxana-essays-compiled-quality quality)
          ;; Mirror ann-id and endpoint-index as text properties — but
          ;; only on positions where the property is currently nil.  This
          ;; preserves inner-annotation text-property anchors when an
          ;; outer wrapping annotation (e.g. from invisible-ink-region)
          ;; covers the same range; the wrapping annotation fills the
          ;; gaps between nested overlays without clobbering them.  Kill /
          ;; yank still carries the property natively through the kill
          ;; ring; `--reconcile-overlays-from-properties' (registered on
          ;; post-command-hook) creates a fresh overlay at any yank
          ;; target whose properties say it belongs to an annotation.
          (with-silent-modifications
            (let ((pos beg))
              (while (< pos end)
                (let* ((current (get-text-property
                                 pos 'arxana-essays-compiled-ann-id))
                       (next-change
                        (or (next-single-property-change
                             pos 'arxana-essays-compiled-ann-id nil end)
                            end)))
                  (unless current
                    (put-text-property
                     pos next-change 'arxana-essays-compiled-ann-id ann-id)
                    (put-text-property
                     pos next-change 'arxana-essays-compiled-endpoint-index
                     (or endpoint-index -1)))
                  (setq pos next-change)))))
          (overlay-put ov 'help-echo
                       (format "[%s | %s | severity=%s status=%s | anchor=%s]\n%s"
                               ann-id
                               (or (plist-get ann :hx-type) :unknown)
                               (or severity "n/a")
                               (or status "open")
                               quality
                             (if (and note (> (length note) 240))
                                 (concat (substring note 0 240) "...")
                               (or note ""))))
          ov)))))

(defconst arxana-browser-essays-compiled--anchor-releasing-resolutions
  '(:addressed :addressed-differently :rejected)
  "Resolution-state values that release the annotation's anchor.
When the editorial log carries one of these states for an annotation, the
compiled view does NOT place an overlay — the annotation has been decided,
its overlay role is spent.  `:deferred' (or no log entry) keeps the
overlay so the annotation continues to surface.")

(defun arxana-browser-essays-compiled--place-all-overlays (anns)
  "Place overlays for ANNS in the current buffer; return ordered-positions list.
Annotations whose passages cannot be located are skipped here (still rendered
in the notes buffer with the orphan face).  Annotations whose editorial-log
resolution-state is anchor-releasing (see
`arxana-browser-essays-compiled--anchor-releasing-resolutions') are also
skipped — the address-then-orphan cycle releases the overlay on resolution
so the notes pane shows orphan + resolved-badge, the canonical 'this was
actioned' signal."
  (let ((positions nil))
    (dolist (ann anns)
      (let* ((pairs (arxana-browser-essays-compiled--annotated-endpoint-pairs ann))
             (n (length pairs))
             (face (arxana-browser-essays-compiled--passage-face ann n)))
        (dolist (pair pairs)
          (let ((ov (arxana-browser-essays-compiled--place-overlay
                     ann (car pair) face (cdr pair))))
            (when ov
              (push (cons (overlay-start ov) (plist-get ann :id)) positions))))))
    (sort positions (lambda (a b) (< (car a) (car b))))))

;; -----------------------------------------------------------------
;; Notes side pane
;; -----------------------------------------------------------------

(defun arxana-browser-essays-compiled--render-notes (anns text-buffer)
  "Render ANNS into the notes buffer, with cross-references to TEXT-BUFFER overlays.
Annotations whose passages were not located in the prose are shown with the
orphan face."
  (let ((buf (get-buffer-create arxana-browser-essays-compiled-notes-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Arxana Essay Compiled Notes\n"
                            'face 'arxana-browser-rewrites-header-face))
        (insert (format "Essay: %s\nAnnotations: %d  |  text buffer: %s\n\n"
                        (or (buffer-name text-buffer) "?")
                        (length anns)
                        (or (buffer-name text-buffer) "?")))
        (insert "Keys in text buffer: n/p navigate, RET show note, q quit.\n\n")
        (dolist (ann anns)
          (let* ((entry-start (point))
                 (id (plist-get ann :id))
                 (hx (plist-get ann :hx-type))
                 (sev (plist-get ann :severity))
                 (status (plist-get ann :status))
                 (note (plist-get ann :note))
                 (pattern (arxana-browser-essays-compiled--annotation-source-pattern ann))
                 (passages (arxana-browser-essays-compiled--annotation-passages ann))
                 ;; Count overlays actually placed for this annotation (includes
                 ;; lax-anchored matches), excluding zero-length overlays —
                 ;; without `evaporate', a fully-deleted overlay collapses to
                 ;; zero length but persists.  Zero-length = effective orphan.
                 (located-count
                  (with-current-buffer text-buffer
                    (let ((n 0))
                      (dolist (ov (overlays-in (point-min) (point-max)))
                        (when (and (equal (overlay-get ov 'arxana-essays-compiled-ann-id) id)
                                   (> (overlay-end ov) (overlay-start ov)))
                          (cl-incf n)))
                      n)))
                 (orphan? (and (> (length passages) 0)
                               (= located-count 0))))
            (insert (propertize (format "── %s ──\n" id)
                                'face (if orphan?
                                          'arxana-browser-essays-compiled-orphan-face
                                        'arxana-browser-rewrites-field-face)))
            (insert (format "  type: %s   severity: %s   status: %s   anchors: %d/%d\n"
                            (or hx :unknown)
                            (or sev "n/a")
                            (or status "open")
                            located-count (length passages)))
            (when pattern
              (insert (format "  pattern: %s\n" pattern)))
            (let ((res-state (plist-get ann :resolution-state)))
              (when res-state
                (insert "  ")
                (insert (propertize
                         (format "↪ resolved %s in %s (%s)\n"
                                 res-state
                                 (or (plist-get ann :resolution-log-id) "?")
                                 (or (plist-get ann :resolution-timestamp) "?"))
                         'face 'arxana-browser-essays-compiled-resolved-face))
                (let ((rat (plist-get ann :resolution-rationale)))
                  (when (and rat (not (string-empty-p rat)))
                    (insert "    ")
                    (insert (propertize (format "\"%s\"\n" rat)
                                        'face
                                        'arxana-browser-essays-compiled-resolved-rationale-face))))))
            (when note
              (insert "\n")
              (let ((start (point)))
                (insert note)
                (when orphan?
                  (add-text-properties
                   start (point)
                   '(face arxana-browser-essays-compiled-orphan-face)))))
            (insert "\n\n")
            ;; Tag the whole block with the annotation id so a post-command
            ;; hook in this notes buffer can ask "which annotation is under
            ;; point?" via `get-text-property' and drive the reverse sync
            ;; (notes cursor → text buffer backlink highlight + scroll).
            (add-text-properties
             entry-start (point)
             (list 'arxana-essays-compiled-ann-id id))))
        (goto-char (point-min))
        (special-mode)
        ;; Pair this notes buffer with its companion text buffer and arm
        ;; the reverse-sync hook.  Buffer-local state survives `special-mode'
        ;; setup because we set it AFTER the mode call.
        (setq-local arxana-browser-essays-compiled--notes-text-buffer
                    text-buffer)
        (setq-local arxana-browser-essays-compiled--notes-last-followed-id
                    nil)
        (add-hook 'post-command-hook
                  #'arxana-browser-essays-compiled--notes-post-command-sync
                  nil t))
      buf)))

;; -----------------------------------------------------------------
;; Compiled-view mode + navigation
;; -----------------------------------------------------------------

(defvar arxana-browser-essays-compiled-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'arxana-browser-essays-compiled-next-annotation)
    (define-key map (kbd "p") #'arxana-browser-essays-compiled-previous-annotation)
    (define-key map (kbd "RET") #'arxana-browser-essays-compiled-show-note)
    (define-key map (kbd "g") #'arxana-browser-essays-compiled-refresh)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "C-c C-c") #'arxana-browser-essays-compiled-save)
    (define-key map (kbd "C-c C-k") #'arxana-browser-essays-compiled-abort-edit)
    map)
  "Keymap for `arxana-browser-essays-compiled-mode'.")

(define-derived-mode arxana-browser-essays-compiled-mode special-mode "Arxana-Compiled"
  "Major mode for the compiled-view of an Arxana Essay.

Read-only.  Annotations appear as colour-coded overlays:
  - yellow  (multi-anchor)   — annotation has 2+ endpoints, likely cross-section
  - green   (aif-witness)    — :hx-type :aif/* — protect during rewrites
  - amber   (comment)        — :hx-type :annotation/comment (single anchor)
  - blue    (default)        — anything else

Cursor on an overlay automatically highlights and parallax-recenters the
matching note in the side pane (notes-window line = essay-buffer line of point).

Keys: n/p navigate; RET show full note in the side pane; g refresh; q quit."
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  (setq buffer-read-only t)
  (add-hook 'post-command-hook
            #'arxana-browser-essays-compiled--post-command-sync nil t)
  (add-hook 'post-command-hook
            #'arxana-browser-essays-compiled--reconcile-overlays-from-properties
            nil t))

(defun arxana-browser-essays-compiled--annotation-at-point ()
  "Return the annotation :id at point in the current compiled buffer, or nil.
When multiple annotation overlays overlap at point, prefer the one with
the narrowest span — the most-specific annotation for this location.
Without this tiebreaker `overlays-at' returns overlapping overlays in
unspecified order, so the selected annotation flickers between same-start
neighbours; narrowest-wins gives the user the per-passage annotation in
preference to a cross-section diagnostic that happens to anchor here."
  (let* ((ovs (cl-remove-if-not
               (lambda (ov) (overlay-get ov 'arxana-essays-compiled-ann-id))
               (overlays-at (point))))
         (narrowest (car (sort ovs
                               (lambda (a b)
                                 (< (- (overlay-end a) (overlay-start a))
                                    (- (overlay-end b) (overlay-start b))))))))
    (and narrowest (overlay-get narrowest 'arxana-essays-compiled-ann-id))))

(defun arxana-browser-essays-compiled-next-annotation ()
  "Move point to the next annotation overlay in this buffer."
  (interactive)
  (let* ((pt (point))
         (positions arxana-browser-essays-compiled--ordered-positions)
         (next (cl-find-if (lambda (p) (> (car p) pt)) positions)))
    (if next
        (progn
          (goto-char (car next))
          (arxana-browser-essays-compiled--echo-current))
      (message "No further annotations."))))

(defun arxana-browser-essays-compiled-previous-annotation ()
  "Move point to the previous annotation overlay in this buffer."
  (interactive)
  (let* ((pt (point))
         (positions (reverse arxana-browser-essays-compiled--ordered-positions))
         (prev (cl-find-if (lambda (p) (< (car p) pt)) positions)))
    (if prev
        (progn
          (goto-char (car prev))
          (arxana-browser-essays-compiled--echo-current))
      (message "No previous annotations."))))

(defun arxana-browser-essays-compiled--echo-current ()
  "Echo a one-line summary of the annotation at point."
  (let ((id (arxana-browser-essays-compiled--annotation-at-point)))
    (when id
      (let* ((ann (cl-find-if (lambda (a) (equal (plist-get a :id) id))
                              arxana-browser-essays-compiled--annotations))
             (hx (plist-get ann :hx-type))
             (sev (plist-get ann :severity))
             (pattern (arxana-browser-essays-compiled--annotation-source-pattern ann)))
        (message "[%s] %s | severity=%s%s"
                 id (or hx :?) (or sev "n/a")
                 (if pattern (format " | pattern=%s" pattern) ""))))))

(defun arxana-browser-essays-compiled--notes-entry-bounds (id)
  "Return (BEG . END) of the notes-buffer entry for ID, or nil."
  (let ((notes (get-buffer arxana-browser-essays-compiled-notes-buffer)))
    (when notes
      (with-current-buffer notes
        (save-excursion
          (goto-char (point-min))
          (when (search-forward (format "── %s ──" id) nil t)
            (let ((beg (line-beginning-position))
                  (end (save-excursion
                         (forward-line 1)
                         (if (re-search-forward "^── " nil t)
                             (line-beginning-position)
                           (point-max)))))
              (cons beg end))))))))

(defun arxana-browser-essays-compiled--clear-notes-active ()
  "Remove the active-entry overlay in the notes buffer, if any."
  (when (overlayp arxana-browser-essays-compiled--notes-active-overlay)
    (delete-overlay arxana-browser-essays-compiled--notes-active-overlay)
    (setq arxana-browser-essays-compiled--notes-active-overlay nil)))

(defun arxana-browser-essays-compiled--current-screen-line ()
  "Return the screen-row of point in the selected window (0 = top visible line).

`post-command-hook' runs BEFORE the upcoming redisplay, so `window-start'
may be stale when point has moved off-screen during the command.  If
point isn't currently visible, pre-emptively recenter — mirroring what
the natural redisplay would do under default `scroll-conservatively'=0 —
so `window-start' reflects the post-scroll state before we count.

Falls back to 0 if `count-screen-lines' signals an error (defensive)."
  (when (eq (window-buffer (selected-window)) (current-buffer))
    (unless (pos-visible-in-window-p (point))
      (ignore-errors (recenter))))
  (or (ignore-errors
        (count-screen-lines (window-start) (point)))
      0))

(defun arxana-browser-essays-compiled--recenter-target-at-row (target-row)
  "In the selected window, recenter so point sits at TARGET-ROW screen rows
from the window top.  TARGET-ROW is clamped to the window body height; if
it would land off-screen the recenter snaps to the nearest valid row.
Used in both sync directions to put the followed annotation at the same
window-row as the source cursor — the side-by-side reading position."
  (let* ((body (window-body-height (selected-window)))
         (row (max 0 (min target-row (max 0 (1- body))))))
    (recenter row)))

(defun arxana-browser-essays-compiled--paint-notes-active-entry (id)
  "Paint the active-entry overlay over the notes-buffer entry for ID.
Clears any previous active-entry overlay first.  Returns the new overlay
or nil if no entry was found.  Caller is responsible for ensuring the
notes buffer is current (via `with-current-buffer') if called from
outside it."
  (let ((bounds (arxana-browser-essays-compiled--notes-entry-bounds id)))
    (when bounds
      (let ((notes (get-buffer arxana-browser-essays-compiled-notes-buffer)))
        (when notes
          (with-current-buffer notes
            (arxana-browser-essays-compiled--clear-notes-active)
            (let ((ov (make-overlay (car bounds) (cdr bounds))))
              (overlay-put ov 'face 'arxana-browser-essays-active-face)
              (overlay-put ov 'priority 10)
              (setq arxana-browser-essays-compiled--notes-active-overlay ov)
              ov)))))))

(defun arxana-browser-essays-compiled--highlight-notes-entry (id source-screen-row)
  "Highlight the notes entry for ID; recenter the notes window so the entry
sits at SOURCE-SCREEN-ROW screen rows from the top — aligning it
side-by-side with point in the companion essay window."
  (let ((bounds (arxana-browser-essays-compiled--notes-entry-bounds id))
        (notes (get-buffer arxana-browser-essays-compiled-notes-buffer)))
    (when (and bounds notes)
      (arxana-browser-essays-compiled--paint-notes-active-entry id)
      ;; Restrict to the current frame: the notes buffer may be displayed
      ;; in multiple frames (e.g. a duplicate in a side surface); we want
      ;; the one in the operator's current frame, where their cursor just
      ;; moved.  `t' (all frames) caught the wrong window.
      (let ((win (or (get-buffer-window notes nil)
                     (get-buffer-window notes 0))))
        (when (window-live-p win)
          (with-selected-window win
            (goto-char (car bounds))
            (arxana-browser-essays-compiled--recenter-target-at-row
             source-screen-row)))))))

(defun arxana-browser-essays-compiled--prune-orphan-overlays ()
  "Delete annotation overlays whose range doesn't overlap any text-property
run carrying a matching ann-id.  Such orphans arise from undo edge cases
(marker positions collapsed to point-min) and from kill operations that
erase the property run while a zero-length overlay marker remains behind.

Overlays placed via `arxana-invisible-ink-region' (quality
`manual-region') are EXEMPT from pruning: when they wrap nested
annotations they intentionally have no matching text-property backing,
since setting the property would overwrite the inner annotations' ann-id
text-property at the same positions.  The overlay alone carries the
visual; the sidecar carries the persistence.  Returns the count deleted."
  (let ((deleted 0))
    (dolist (ov (cl-remove-if-not
                 (lambda (ov) (overlay-get ov 'arxana-essays-compiled-ann-id))
                 (overlays-in (point-min) (point-max))))
      (let* ((id (overlay-get ov 'arxana-essays-compiled-ann-id))
             (quality (overlay-get ov 'arxana-essays-compiled-quality))
             (beg (overlay-start ov))
             (end (overlay-end ov))
             (manual? (eq quality 'manual-region))
             (overlaps-property
              (cond
               ((= beg end)
                ;; Zero-length overlay: check the property AT the position.
                (equal (get-text-property beg 'arxana-essays-compiled-ann-id) id))
               (t
                (catch 'found
                  (let ((pos beg))
                    (while (< pos end)
                      (when (equal (get-text-property pos 'arxana-essays-compiled-ann-id) id)
                        (throw 'found t))
                      (setq pos (or (next-single-property-change
                                     pos 'arxana-essays-compiled-ann-id nil end)
                                    end))))
                  nil)))))
        (unless (or manual? overlaps-property)
          (delete-overlay ov)
          (cl-incf deleted))))
    deleted))

(defun arxana-browser-essays-compiled--cleanup-redundant-overlays ()
  "Delete annotation overlays whose range is contained within another
annotation overlay's range with the SAME `arxana-essays-compiled-ann-id'.
For pairs with identical ranges keeps the largest-by-id-then-arbitrary;
non-overlapping overlays for the same id (e.g. multi-anchor annotations,
yanked-elsewhere passages) are preserved.  Returns the count deleted."
  (let* ((annotated
          (cl-remove-if-not
           (lambda (ov) (overlay-get ov 'arxana-essays-compiled-ann-id))
           (overlays-in (point-min) (point-max))))
         (groups (make-hash-table :test 'equal))
         (deleted 0))
    (dolist (ov annotated)
      (let ((id (overlay-get ov 'arxana-essays-compiled-ann-id)))
        (push ov (gethash id groups))))
    (maphash
     (lambda (_id ovs)
       (when (> (length ovs) 1)
         (let* ((sorted (sort (copy-sequence ovs)
                              (lambda (a b)
                                (> (- (overlay-end a) (overlay-start a))
                                   (- (overlay-end b) (overlay-start b))))))
                kept-spans)
           (dolist (ov sorted)
             (let ((b (overlay-start ov))
                   (e (overlay-end ov)))
               (if (cl-some (lambda (k)
                              (and (<= (car k) b) (>= (cdr k) e)))
                            kept-spans)
                   (progn (delete-overlay ov) (cl-incf deleted))
                 (push (cons b e) kept-spans)))))))
     groups)
    deleted))

;;;###autoload
(defun arxana-browser-essays-compiled-cleanup-overlays ()
  "Interactive entry point: delete redundant annotation overlays in the
current compiled-view buffer.  See
`arxana-browser-essays-compiled--cleanup-redundant-overlays'."
  (interactive)
  (unless (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (user-error "Not in a compiled-view buffer"))
  (let ((n (arxana-browser-essays-compiled--cleanup-redundant-overlays)))
    (message "[compiled] Deleted %d redundant overlay%s"
             n (if (= n 1) "" "s"))))

(defun arxana-browser-essays-compiled--reconcile-overlays-from-properties ()
  "Scan buffer for `arxana-essays-compiled-ann-id' text-property runs and
ensure each has exactly one matching overlay covering it.

Render time stamps the property on every annotated passage (mirroring
the overlay).  Kill / yank carry the property through the kill ring; the
overlay does not follow.  This sweep, registered on post-command-hook,
finds each contiguous property run and:

  - If a contained overlay already covers it: no-op.
  - If overlays with that id overlap but none covers: RESIZE the largest
    overlapping one to the property run, delete the others (prevents
    duplicate accumulation when text deletions shrink an overlay below
    the property-run span and the reconciler would otherwise add a new
    overlay on top of the shrunken one).
  - If no overlay with that id overlaps: create one (the yank-elsewhere
    path).

Effect: yanking a marked-up region elsewhere re-creates the visible
markup at the yank target without operator action, and text edits that
narrow an existing overlay don't leave behind duplicate overlays."
  (when (derived-mode-p 'arxana-browser-essays-compiled-mode)
    ;; Sweep orphan + redundant overlays before the property walk so any
    ;; resize-then-delete-other-overlapping result is clean.  Orphans
    ;; first (zero-length stragglers from undo / kill edge cases),
    ;; redundants second (contained-within duplicates from past edits).
    (arxana-browser-essays-compiled--prune-orphan-overlays)
    (arxana-browser-essays-compiled--cleanup-redundant-overlays)
    (save-excursion
      (let ((pos (point-min))
            (anns arxana-browser-essays-compiled--annotations))
        (while (< pos (point-max))
          (let ((id (get-text-property pos 'arxana-essays-compiled-ann-id)))
            (cond
             (id
              (let* ((idx (get-text-property
                           pos 'arxana-essays-compiled-endpoint-index))
                     (end (or (next-single-property-change
                               pos 'arxana-essays-compiled-ann-id nil
                               (point-max))
                              (point-max)))
                     (matching-overlaps
                      (cl-remove-if-not
                       (lambda (ov)
                         (and (equal (overlay-get
                                      ov 'arxana-essays-compiled-ann-id)
                                     id)
                              (< (overlay-start ov) end)
                              (> (overlay-end ov) pos)))
                       (overlays-in pos end)))
                     (covering
                      (cl-find-if
                       (lambda (ov)
                         (and (<= (overlay-start ov) pos)
                              (>= (overlay-end ov) end)))
                       matching-overlaps)))
                (cond
                 (covering nil)  ; already covered
                 (matching-overlaps
                  ;; Resize the largest overlapping overlay to the run;
                  ;; delete the others to avoid stacking duplicates.
                  (let ((keep (car (sort (copy-sequence matching-overlaps)
                                         (lambda (a b)
                                           (> (- (overlay-end a) (overlay-start a))
                                              (- (overlay-end b) (overlay-start b))))))))
                    (move-overlay keep pos end)
                    (dolist (ov matching-overlaps)
                      (unless (eq ov keep)
                        (delete-overlay ov)))))
                 (t
                  ;; No overlay for this property run — create one.
                  (let* ((ann (cl-find-if
                               (lambda (a) (equal (plist-get a :id) id))
                               anns))
                         (pairs (and ann
                                     (arxana-browser-essays-compiled--annotated-endpoint-pairs
                                      ann)))
                         (face (and ann
                                    (arxana-browser-essays-compiled--passage-face
                                     ann (length pairs)))))
                    (when (and ann face)
                      (let ((ov (make-overlay pos end)))
                        (overlay-put ov 'face face)
                        (overlay-put ov 'arxana-essays-compiled-ann-id id)
                        (overlay-put ov 'arxana-essays-compiled-endpoint-index
                                     (or idx -1))
                        (overlay-put ov 'arxana-essays-compiled-position pos)
                        (overlay-put ov 'arxana-essays-compiled-quality
                                     'yanked-from-property)
                        (overlay-put ov 'help-echo
                                     (format "[%s | yanked-from-property]"
                                             id)))))))
                (setq pos end)))
             (t
              (setq pos (or (next-single-property-change
                             pos 'arxana-essays-compiled-ann-id nil
                             (point-max))
                            (point-max)))))))))))

(defun arxana-browser-essays-compiled--post-command-sync ()
  "Sync the notes-buffer highlight + recenter to the annotation at point.
Runs in `post-command-hook' inside compiled buffers; cheap no-op when the
annotation under point hasn't changed since the last call."
  (when (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (let ((id (arxana-browser-essays-compiled--annotation-at-point))
          (source-row (arxana-browser-essays-compiled--current-screen-line)))
      (cond
       ((and id (not (equal id arxana-browser-essays-compiled--last-followed-id)))
        (setq arxana-browser-essays-compiled--last-followed-id id)
        (arxana-browser-essays-compiled--highlight-notes-entry id source-row))
       ((and (null id) arxana-browser-essays-compiled--last-followed-id)
        (setq arxana-browser-essays-compiled--last-followed-id nil)
        (let ((notes (get-buffer arxana-browser-essays-compiled-notes-buffer)))
          (when notes
            (with-current-buffer notes
              (arxana-browser-essays-compiled--clear-notes-active)))))
       ;; When the same annotation stays under point but the screen row
       ;; changes (point moved within a multi-line overlay), keep the
       ;; side-by-side alignment fresh.
       ((and id (equal id arxana-browser-essays-compiled--last-followed-id))
        (let ((notes (get-buffer arxana-browser-essays-compiled-notes-buffer)))
          (when notes
            (let ((win (or (get-buffer-window notes nil)
                           (get-buffer-window notes 0)))
                  (bounds (arxana-browser-essays-compiled--notes-entry-bounds id)))
              (when (and (window-live-p win) bounds)
                (with-selected-window win
                  (goto-char (car bounds))
                  (arxana-browser-essays-compiled--recenter-target-at-row
                   source-row)))))))))))

(defun arxana-browser-essays-compiled-show-note ()
  "In the notes side pane, scroll to the note for the annotation at point."
  (interactive)
  (let ((id (arxana-browser-essays-compiled--annotation-at-point)))
    (if (not id)
        (message "No annotation at point.")
      (arxana-browser-essays-compiled--highlight-notes-entry
       id (arxana-browser-essays-compiled--current-screen-line)))))

(defun arxana-browser-essays-compiled-refresh ()
  "Refresh overlays + notes pane against current buffer content.

Preserves the buffer's text (so unsaved edits survive); only the
annotation overlays and notes pane are rebuilt.  For a full re-read
from disk (which discards unsaved edits), use
\\[arxana-browser-essays-open-compiled] instead, or
\\[arxana-browser-essays-compiled-abort-edit] in edit mode."
  (interactive)
  (when arxana-browser-essays-compiled--essay-id
    (arxana-browser-essays-compiled--refresh-overlays-in-place)))

;; -----------------------------------------------------------------
;; Entry point
;; -----------------------------------------------------------------

;;;###autoload
(defun arxana-browser-essays-open-compiled (essay-id)
  "Open the whole-essay compiled view for ESSAY-ID.

Interactively, prompts for an essay from the current catalog."
  (interactive
   (list
    (let* ((cats (arxana-browser-essays--catalogs))
           (choices (mapcar (lambda (cat)
                              (cons (or (plist-get cat :label)
                                        (plist-get cat :essay-id))
                                    (plist-get cat :essay-id)))
                            cats))
           (chosen (completing-read "Compiled view of essay: "
                                    (mapcar #'car choices) nil t)))
      (cdr (assoc chosen choices)))))
  (let* ((cat (arxana-browser-essays--catalog-spec essay-id))
         (label (or (and cat (plist-get cat :label)) essay-id))
         (source-file (and cat (plist-get cat :source-file))))
    (unless (and source-file (file-readable-p source-file))
      (user-error "Compiled view: no readable source-file for essay %s" essay-id))
    (let* ((buf-name (format arxana-browser-essays-compiled-buffer-format label))
           (buf (get-buffer-create buf-name))
           (anns (arxana-browser-essays-compiled--load-annotations essay-id source-file)))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert-file-contents source-file)
          (goto-char (point-min)))
        ;; `erase-buffer' does not remove overlays.  Without explicit
        ;; deletion, every refresh (g / open-compiled) accumulates a new
        ;; set on top of the prior, producing N×passages overlays after N
        ;; refreshes.  Clear annotation overlays before re-placing.
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (overlay-get ov 'arxana-essays-compiled-ann-id)
            (delete-overlay ov)))
        (arxana-browser-essays-compiled-mode)
        (setq-local arxana-browser-essays-compiled--essay-id essay-id)
        (setq-local arxana-browser-essays-compiled--source-file source-file)
        (setq-local arxana-browser-essays-compiled--annotations anns)
        ;; Initialise the user-annotation-changes plist from the sidecar
        ;; (or a fresh empty payload if none).  `--load-annotations'
        ;; already merged the sidecar into ANNS; we mirror the same data
        ;; into the buffer-local tracker so further changes from
        ;; `arxana-invisible-ink-region' know what's already persisted
        ;; and accumulate on top.
        (setq-local arxana-browser-essays-compiled--user-annotation-changes
                    (or (arxana-browser-essays-compiled--read-user-annotations-sidecar
                         (arxana-browser-essays-compiled--user-annotations-sidecar-path
                          source-file))
                        '(:new-annotations nil :label-additions nil)))
        (setq-local arxana-browser-essays-compiled--ordered-positions
                    (arxana-browser-essays-compiled--place-all-overlays anns)))
      (arxana-browser-essays-compiled--render-notes anns buf)
      ;; Layout: compiled buffer left, notes side window right.
      (delete-other-windows)
      (switch-to-buffer buf)
      (let ((notes (get-buffer arxana-browser-essays-compiled-notes-buffer)))
        (when notes
          (display-buffer-in-side-window
           notes
           `((side . right)
             (window-width . ,arxana-browser-essays-compiled-notes-width)))))
      (message "Compiled view: %d annotations (%d anchored, %d orphan)"
               (length anns)
               (length arxana-browser-essays-compiled--ordered-positions)
               (- (length anns)
                  (length (cl-delete-duplicates
                           (mapcar #'cdr
                                   arxana-browser-essays-compiled--ordered-positions)
                           :test #'equal))))
      buf)))

;; -----------------------------------------------------------------
;; Edit + save-back  (Arxana-as-compiler; cf. futon-theory/wyrd)
;; -----------------------------------------------------------------
;;
;; The compiled view is one regime — whole-essay-with-cross-section-context —
;; among several Arxana provides for the same underlying material.  Per the
;; wyrd pattern, the chrome must let the operator turn between regimes;
;; collapsing the compiled view into "mere reading" destroys the cross-
;; section editing affordance that motivated its existence in the first
;; place.
;;
;; The compiled buffer is loaded by `insert-file-contents' from the source
;; .md; its text content stays in 1:1 correspondence with disk.  Save writes
;; the buffer back to .md whole-file (no section-bounded regex, unlike the
;; section-by-section view's `arxana-browser-essays-save-section').
;;
;; v0 does NOT sync overlay text back to annotation `:passage' fields.
;; Annotations whose passages were rewritten render as orphan in the notes
;; pane — same regime as the lax re-anchor the section-view already
;; documents.  Future log entries (editorial-log.edn) can record explicit
;; retraction or repassage; or v1 of this save-back can add the passage
;; sync if operator workflow demands it.

(defvar-local arxana-browser-essays-compiled--edit-mode nil
  "Buffer-local: non-nil when the compiled view is editable + armed for `C-c C-c'.
Toggled via `arxana-browser-essays-compiled-edit-mode-toggle'.")

(defvar arxana-browser-essays-compiled-edit-map
  (let ((map (make-sparse-keymap)))
    ;; No parent — printable chars fall through to global-map's
    ;; `self-insert-command'.  Inheriting from the compiled-mode-map (whose
    ;; ultimate parent is `special-mode-map' with `suppress-keymap' applied)
    ;; would re-shadow self-insert with `undefined' — the bug this map fixes.
    (define-key map (kbd "C-c C-c") #'arxana-browser-essays-compiled-save)
    (define-key map (kbd "C-c C-k") #'arxana-browser-essays-compiled-abort-edit)
    (define-key map (kbd "C-S-y") #'arxana-browser-essays-compiled-yank-without-annotation)
    map)
  "Local keymap installed in the compiled view while edit mode is on.")

;; 🥨 reaches the essays hydra even in edit mode — the whole point of a dedicated
;; key (`?' must stay self-inserting here).  Standalone `define-key' (not inside
;; the defvar) so it re-applies on every reload, since `defvar' won't re-init an
;; already-bound map.
(define-key arxana-browser-essays-compiled-edit-map
            (kbd "🥨") #'arxana-browser-essays-hydra/body)

(defun arxana-browser-essays-compiled-edit-mode-toggle ()
  "Toggle edit mode in the compiled view.
ENABLE: disables read-only; arms `C-c C-c' (save) and `C-c C-k' (abort).
DISABLE: re-enables read-only (no implicit save; pending edits stay in
the buffer until save or abort)."
  (interactive)
  (unless (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (user-error "Not in a compiled-view buffer"))
  (cond
   ((not arxana-browser-essays-compiled--edit-mode)
    (setq buffer-read-only nil)
    (setq-local arxana-browser-essays-compiled--edit-mode t)
    (use-local-map arxana-browser-essays-compiled-edit-map)
    (force-mode-line-update)
    (message "[compiled] Edit mode ENABLED — C-c C-c saves to %s; C-c C-k aborts"
             (abbreviate-file-name
              (or arxana-browser-essays-compiled--source-file "<no-source>"))))
   (t
    (setq-local arxana-browser-essays-compiled--edit-mode nil)
    (setq buffer-read-only t)
    (use-local-map arxana-browser-essays-compiled-mode-map)
    (force-mode-line-update)
    (message "[compiled] Edit mode DISABLED (buffer read-only)"))))

;; -----------------------------------------------------------------
;; History / snapshots — per-save versioning under <essay>/.history/
;; -----------------------------------------------------------------
;;
;; Each compiled-view save copies the current `.md` and (sibling)
;; `annotations.edn' into a timestamped snapshot under
;; `<essay-dir>/.history/' BEFORE the new write.  That gives an N-level,
;; on-disk history independent of git, the `.~arxana~' single-level
;; sibling backup, and the section-view's XTDB transactions.
;;
;; Restore is operator-driven:
;;  - `arxana-browser-essays-compiled-history-list': open a buffer
;;    listing snapshots, with one chord per row to restore that snapshot.
;;  - `arxana-browser-essays-compiled-restore-last-save': revert to the
;;    most recent snapshot (the state immediately before the most recent
;;    save) without prompting.

(defun arxana-browser-essays-compiled--snapshot-dir (essay-dir)
  "Return the .history directory path under ESSAY-DIR."
  (expand-file-name ".history" essay-dir))

(defun arxana-browser-essays-compiled--snapshot-timestamp ()
  "Return a filesystem-safe ISO-ish timestamp string for snapshot names."
  (format-time-string "%Y-%m-%dT%H-%M-%S"))

(defun arxana-browser-essays-compiled--make-snapshot (src-md edn-path)
  "Copy current SRC-MD and EDN-PATH (if present) into the .history dir.
Returns the timestamp used, or nil if no snapshot was taken (e.g. no
readable source file)."
  (when (and src-md (file-readable-p src-md))
    (let* ((essay-dir (file-name-directory src-md))
           (hist-dir (arxana-browser-essays-compiled--snapshot-dir essay-dir))
           (ts (arxana-browser-essays-compiled--snapshot-timestamp)))
      (unless (file-directory-p hist-dir)
        (make-directory hist-dir t))
      (copy-file src-md
                 (expand-file-name
                  (format "%s--%s" ts (file-name-nondirectory src-md))
                  hist-dir)
                 t)
      (when (and edn-path (file-readable-p edn-path))
        (copy-file edn-path
                   (expand-file-name
                    (format "%s--%s" ts (file-name-nondirectory edn-path))
                    hist-dir)
                   t))
      ts)))

(defun arxana-browser-essays-compiled--snapshot-pairs (hist-dir)
  "Return ((TIMESTAMP MD-PATH EDN-PATH-or-nil) ...) from HIST-DIR, newest first."
  (when (file-directory-p hist-dir)
    (let* ((files (directory-files hist-dir nil "^[0-9][0-9][0-9][0-9]-.*--" t))
           (groups (make-hash-table :test 'equal)))
      (dolist (f files)
        (when (string-match "\\`\\([^-]+-[^-]+-[^T]+T[^-]+-[^-]+-[^-]+\\)--\\(.+\\)\\'" f)
          (let* ((ts   (match-string 1 f))
                 (rest (match-string 2 f))
                 (full (expand-file-name f hist-dir))
                 (cell (or (gethash ts groups) (list nil nil))))
            (cond
             ((string-suffix-p ".md" rest)
              (setf (nth 0 cell) full))
             ((string-suffix-p ".edn" rest)
              (setf (nth 1 cell) full)))
            (puthash ts cell groups))))
      (let (result)
        (maphash (lambda (ts cell)
                   (push (list ts (nth 0 cell) (nth 1 cell)) result))
                 groups)
        (sort result (lambda (a b) (string> (car a) (car b))))))))

(defun arxana-browser-essays-compiled-history-list ()
  "List snapshots of the current essay; press RET on a row to restore that snapshot."
  (interactive)
  (unless (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (user-error "Not in a compiled-view buffer"))
  (let* ((src arxana-browser-essays-compiled--source-file)
         (essay-dir (file-name-directory src))
         (hist-dir (arxana-browser-essays-compiled--snapshot-dir essay-dir))
         (pairs (arxana-browser-essays-compiled--snapshot-pairs hist-dir))
         (essay-id arxana-browser-essays-compiled--essay-id)
         (buf (get-buffer-create
               (format "*Arxana Essay History: %s*"
                       (file-name-nondirectory
                        (directory-file-name essay-dir))))))
    (unless pairs
      (user-error "No snapshots yet under %s" hist-dir))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Snapshots under %s\n%d entries (newest first)\n\n"
                        (abbreviate-file-name hist-dir)
                        (length pairs)))
        (insert "RET on a row to restore that snapshot to disk + buffer\n")
        (insert "q to quit\n\n")
        (dolist (entry pairs)
          (let* ((ts  (nth 0 entry))
                 (md  (nth 1 entry))
                 (edn (nth 2 entry))
                 (start (point)))
            (insert (format "%s   .md=%s   .edn=%s\n"
                            ts
                            (if md "yes" "—")
                            (if edn "yes" "—")))
            (put-text-property start (1- (point))
                               'arxana-snapshot-entry entry)))
        (goto-char (point-min)))
      (use-local-map (let ((m (copy-keymap special-mode-map)))
                       (define-key m (kbd "RET")
                                   #'arxana-browser-essays-compiled--history-restore-at-point)
                       (define-key m (kbd "q") #'quit-window)
                       m))
      (setq-local arxana-browser-essays-compiled--history-essay-id essay-id)
      (setq-local arxana-browser-essays-compiled--history-src-md src)
      (special-mode))
    (display-buffer buf)))

(defvar-local arxana-browser-essays-compiled--history-essay-id nil)
(defvar-local arxana-browser-essays-compiled--history-src-md nil)

(defun arxana-browser-essays-compiled--history-restore-at-point ()
  "Restore the snapshot whose row is at point in the history list buffer."
  (interactive)
  (let ((entry (get-text-property (point) 'arxana-snapshot-entry))
        (essay-id arxana-browser-essays-compiled--history-essay-id)
        (src-md arxana-browser-essays-compiled--history-src-md))
    (unless entry
      (user-error "No snapshot at point"))
    (unless (yes-or-no-p (format "Restore snapshot %s? (will overwrite current .md and .edn) "
                                 (nth 0 entry)))
      (user-error "Restore cancelled"))
    (let ((snap-md  (nth 1 entry))
          (snap-edn (nth 2 entry))
          (edn-path (and src-md (arxana-browser-essays-compiled--edn-sibling-path src-md))))
      (when (and snap-md src-md (file-readable-p snap-md))
        (copy-file snap-md src-md t))
      (when (and snap-edn edn-path (file-readable-p snap-edn))
        (copy-file snap-edn edn-path t))
      ;; Re-open the compiled view so the buffer reflects restored disk state.
      (when essay-id
        (arxana-browser-essays-open-compiled essay-id))
      (message "[compiled] Restored snapshot %s" (nth 0 entry)))))

(defun arxana-browser-essays-compiled-restore-last-save ()
  "Revert to the most recent snapshot (state immediately before the most recent save).
Asks for confirmation; overwrites current .md and .edn from the snapshot."
  (interactive)
  (unless (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (user-error "Not in a compiled-view buffer"))
  (let* ((src arxana-browser-essays-compiled--source-file)
         (essay-dir (file-name-directory src))
         (hist-dir (arxana-browser-essays-compiled--snapshot-dir essay-dir))
         (pairs (arxana-browser-essays-compiled--snapshot-pairs hist-dir))
         (most-recent (car pairs))
         (essay-id arxana-browser-essays-compiled--essay-id))
    (unless most-recent
      (user-error "No snapshots yet under %s" hist-dir))
    (unless (yes-or-no-p
             (format "Restore most recent snapshot (%s)? "
                     (nth 0 most-recent)))
      (user-error "Restore cancelled"))
    (let ((snap-md  (nth 1 most-recent))
          (snap-edn (nth 2 most-recent))
          (edn-path (arxana-browser-essays-compiled--edn-sibling-path src)))
      (when (and snap-md (file-readable-p snap-md))
        (copy-file snap-md src t))
      (when (and snap-edn edn-path (file-readable-p snap-edn))
        (copy-file snap-edn edn-path t))
      (arxana-browser-essays-open-compiled essay-id)
      (message "[compiled] Restored from snapshot %s" (nth 0 most-recent)))))

(defun arxana-browser-essays-compiled--edn-syntax-table ()
  "Return a syntax table where { } [ ] are paren-syntax, for EDN navigation."
  (let ((st (make-syntax-table emacs-lisp-mode-syntax-table)))
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    st))

(defun arxana-browser-essays-compiled--rewrite-edn-endpoint-passage
    (edn-path ann-id endpoint-index new-passage)
  "In EDN-PATH, replace the ENDPOINT-INDEX-th endpoint's :passage of ANN-ID.

Surgical text edit: locates the annotation by `:id', enters its
`:endpoints' vector, skips to the ENDPOINT-INDEXth endpoint map, and
rewrites its `:passage' string.  Preserves all other content (comments,
formatting, sibling endpoints, other annotations).

Returns t if file was changed, nil if NEW-PASSAGE already matched, or
the symbol `not-applicable' if the target endpoint has no `:passage'
field (e.g. `:role :source' pattern endpoint).

Uses a syntax table where {, }, [, ] are paren-syntax so the elisp
`forward-sexp' / `down-list' navigators handle EDN structure correctly."
  (let ((result nil))
    (with-temp-buffer
      (insert-file-contents edn-path)
      (with-syntax-table (arxana-browser-essays-compiled--edn-syntax-table)
        (goto-char (point-min))
        (unless (search-forward (format ":id \"%s\"" ann-id) nil t)
          (error "[compiled-sync] Annotation %s not found in %s"
                 ann-id edn-path))
        (backward-up-list)
        (let ((ann-start (point))
              (ann-end (save-excursion (forward-sexp) (point))))
          (goto-char ann-start)
          (unless (re-search-forward ":endpoints[[:space:]\n]+\\[" ann-end t)
            (error "[compiled-sync] No :endpoints vector for %s" ann-id))
          (goto-char (1- (point)))  ; back onto the [
          (let ((vec-end (save-excursion (forward-sexp) (point))))
            (down-list)             ; into the vector
            (let ((i 0))
              (while (< i endpoint-index)
                (forward-sexp)
                (setq i (1+ i))))
            ;; Skip whitespace to the start of the target endpoint map.
            (skip-chars-forward " \t\n")
            (when (> (point) vec-end)
              (error "[compiled-sync] Endpoint %d out of range for %s"
                     endpoint-index ann-id))
            (let ((ep-start (point))
                  (ep-end (save-excursion (forward-sexp) (point))))
              (goto-char ep-start)
              (cond
               ((re-search-forward ":passage[[:space:]\n]+" ep-end t)
                (let* ((str-start (point))
                       (str-end   (progn (forward-sexp) (point)))
                       (current (read (buffer-substring str-start str-end))))
                  (cond
                   ((string= current new-passage)
                    (setq result nil))
                   (t
                    (delete-region str-start str-end)
                    (goto-char str-start)
                    (prin1 new-passage (current-buffer))
                    (setq result t)))))
               (t
                (setq result 'not-applicable)))))))
      (when (eq result t)
        (write-region (point-min) (point-max) edn-path nil 'silent)))
    result))

(defun arxana-browser-essays-compiled--sync-overlays-to-edn ()
  "Walk annotation overlays in the current buffer; sync changed text to .edn.

For each overlay with a recognised endpoint-index (>= 0), compare current
overlay text to the annotation's stored `:passage' in the sibling
`annotations.edn'.  If they differ, rewrite the .edn passage in place.

Returns a list of (ANN-ID . ENDPOINT-INDEX) tuples that were synced.

Called from `arxana-browser-essays-compiled-save' before the .md
writeback, so the new prose and the new manifest passages are written
together, and the next overlay refresh anchors against the updated text."
  (let ((synced nil)
        (edn-path (arxana-browser-essays-compiled--edn-sibling-path
                   arxana-browser-essays-compiled--source-file)))
    (when edn-path
      (dolist (ov (overlays-in (point-min) (point-max)))
        (let ((id  (overlay-get ov 'arxana-essays-compiled-ann-id))
              (idx (overlay-get ov 'arxana-essays-compiled-endpoint-index)))
          (when (and id (numberp idx) (>= idx 0))
            (let* ((beg (overlay-start ov))
                   (end (overlay-end ov))
                   (current (and beg end (> end beg)
                                 (buffer-substring-no-properties beg end))))
              (when (and current (not (string-empty-p current)))
                (let ((res (condition-case e
                               (arxana-browser-essays-compiled--rewrite-edn-endpoint-passage
                                edn-path id idx current)
                             (error (message "[compiled-sync] %S" e) nil))))
                  (when (eq res t)
                    (push (cons id idx) synced)))))))))
    (nreverse synced)))

(defun arxana-browser-essays-compiled--refresh-overlays-in-place ()
  "Reconcile annotation overlays against the loaded annotation set.

The buffer is the authoritative ground truth for current anchors.
Existing overlays are PRESERVED — their span is where the operator has
established this annotation in the prose, regardless of what
`annotations.edn' says.  Reconciliation:

  - For each (ann-id, endpoint-index) in the loaded annotation set that
    has NO existing overlay, attempt placement via the stored :passage
    \(exact match, then lax near-suffix).
  - For existing overlays, refresh the face only (resolution-state may
    have changed since last render).
  - Overlays whose (ann-id, endpoint-index) is no longer in the loaded
    set are deleted.

This replaces the prior delete-all-then-re-search behaviour, which lost
the operator's in-flight anchors any time the prose drifted from the
stored :passage without an intervening save+sync.  For a forced re-anchor
from .edn, use `arxana-browser-essays-open-compiled' (full re-open)."
  (let* ((essay-id arxana-browser-essays-compiled--essay-id)
         (src arxana-browser-essays-compiled--source-file)
         (anns (and essay-id
                    (arxana-browser-essays-compiled--load-annotations
                     essay-id src)))
         (existing (make-hash-table :test 'equal))
         (expected (make-hash-table :test 'equal)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((id (overlay-get ov 'arxana-essays-compiled-ann-id))
            (idx (overlay-get ov 'arxana-essays-compiled-endpoint-index)))
        (when id
          (puthash (cons id (or idx -1)) ov existing))))
    (dolist (ann anns)
      (let* ((pairs (arxana-browser-essays-compiled--annotated-endpoint-pairs ann))
             (face (arxana-browser-essays-compiled--passage-face ann (length pairs))))
        (dolist (pair pairs)
          (let* ((passage (car pair))
                 (idx (cdr pair))
                 (key (cons (plist-get ann :id) idx))
                 (existing-ov (gethash key existing)))
            (puthash key t expected)
            (cond
             ((and existing-ov (overlay-buffer existing-ov))
              (overlay-put existing-ov 'face face))
             (t
              (arxana-browser-essays-compiled--place-overlay
               ann passage face idx)))))))
    (maphash (lambda (key ov)
               (unless (gethash key expected)
                 (when (overlay-buffer ov)
                   (delete-overlay ov))))
             existing)
    (setq-local arxana-browser-essays-compiled--annotations anns)
    (setq-local arxana-browser-essays-compiled--ordered-positions
                (let (positions)
                  (dolist (ov (overlays-in (point-min) (point-max)))
                    (let ((id (overlay-get ov 'arxana-essays-compiled-ann-id)))
                      (when id
                        (push (cons (overlay-start ov) id) positions))))
                  (sort positions (lambda (a b) (< (car a) (car b))))))
    (arxana-browser-essays-compiled--render-notes anns (current-buffer))))

(defun arxana-browser-essays-compiled-save ()
  "Save edits from the compiled-view buffer back to its source .md file.

Counterpart of `arxana-browser-essays-save-section', whole-file scope
instead of section-bounded.  Steps:
  1. Backup manifest files (`.~arxana~' siblings).
  2. Backup source .md (`.~arxana~' sibling).
  3. Write the buffer text to source .md.
  4. Reload manifests + invalidate XTDB catalog cache.
  5. Refresh overlays + notes pane in-place (preserves window layout).
  6. Re-arm edit mode so the operator can keep typing post-save.

Whole-essay edit-back is the compile→change→decompile affordance the
compiled view exists to provide; cf. `futon-theory/wyrd' (rotation
between regimes; do not collapse into one)."
  (interactive)
  (unless (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (user-error "Not in a compiled-view buffer"))
  (unless arxana-browser-essays-compiled--edit-mode
    (user-error "Not in compiled-view edit mode (hydra `e' to toggle)"))
  (let* ((src arxana-browser-essays-compiled--source-file)
         (saved-point (point))
         (content (buffer-substring-no-properties (point-min) (point-max))))
    (unless (and src (file-writable-p (file-name-directory src)))
      (user-error "Source file not writable: %s" src))
    (when (string-empty-p (string-trim content))
      (user-error "Buffer content is empty — refusing to save"))
    (when (fboundp 'arxana-browser-essays--backup-manifest-files)
      (arxana-browser-essays--backup-manifest-files))
    ;; Take a timestamped pre-save snapshot of the current on-disk state
    ;; (.md and sibling annotations.edn) under <essay-dir>/.history/.
    ;; This is the N-level history operator-facing — restore via
    ;; `arxana-browser-essays-compiled-history-list' /
    ;; `arxana-browser-essays-compiled-restore-last-save'.
    (let ((edn-path (arxana-browser-essays-compiled--edn-sibling-path src)))
      (arxana-browser-essays-compiled--make-snapshot src edn-path))
    ;; Backup the .edn alongside the .md so v1 passage-sync is recoverable.
    (let* ((edn-path (arxana-browser-essays-compiled--edn-sibling-path src))
           (edn-bk (and edn-path (concat edn-path "~arxana~"))))
      (when (and edn-path (file-readable-p edn-path))
        (copy-file edn-path edn-bk t)))
    (let ((bk (concat src "~arxana~")))
      (when (file-readable-p src)
        (copy-file src bk t)))
    ;; v1 passage-sync: for each overlay whose text was edited inside the
    ;; annotated span, rewrite the .edn passage so the next refresh anchors
    ;; against the new prose.  Distinguishes edits-within-anchor (synced)
    ;; from true deletions (overlay evaporated → not synced → orphan).
    (let ((synced (arxana-browser-essays-compiled--sync-overlays-to-edn)))
      (let ((coding-system-for-write 'utf-8))
        (write-region content nil src nil 'silent))
      (when (fboundp 'arxana-browser-essays--reload-manifests-and-cache)
        (arxana-browser-essays--reload-manifests-and-cache))
      (arxana-browser-essays-compiled--refresh-overlays-in-place)
      (goto-char (min saved-point (point-max)))
      (message "[compiled] Saved → %s  (backup: %s.~arxana~)%s"
               (abbreviate-file-name src)
               (file-name-nondirectory src)
               (if synced
                   (format "  [%d passage(s) synced]" (length synced))
                 "")))))

(defun arxana-browser-essays-compiled-rebuild-overlays ()
  "Delete all annotation overlays and re-place from `annotations.edn'.

Useful when overlays have drifted from their semantic anchors (e.g. after
a sequence of edits caused marker-tracking to over-extend an overlay).
Does NOT re-read the source-file or change window layout; the buffer's
prose is untouched.  Overlays whose stored `:passage' no longer matches
the prose orphan correctly."
  (interactive)
  (unless (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (user-error "Not in a compiled-view buffer"))
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (overlay-get ov 'arxana-essays-compiled-ann-id)
      (delete-overlay ov)))
  (let* ((essay-id arxana-browser-essays-compiled--essay-id)
         (src arxana-browser-essays-compiled--source-file)
         (anns (and essay-id
                    (arxana-browser-essays-compiled--load-annotations
                     essay-id src))))
    (setq-local arxana-browser-essays-compiled--annotations anns)
    (setq-local arxana-browser-essays-compiled--ordered-positions
                (arxana-browser-essays-compiled--place-all-overlays anns))
    (arxana-browser-essays-compiled--render-notes anns (current-buffer)))
  (message "[compiled] Rebuilt overlays from annotations.edn"))

(defun arxana-browser-essays-compiled-reanchor-region ()
  "Re-anchor an annotation endpoint to the active region in the prose.

Use when an annotation has orphaned because its `:passage' no longer matches
the prose (typically after a whole-span replace that killed the overlay).
Prompts for the annotation id; if it has multiple `:annotated' endpoints,
prompts for which one.  Rewrites the endpoint's `:passage' in
`annotations.edn' to the selected region's text, then refreshes overlays
in place so the annotation re-anchors at the selection.

The .edn is backed up via `~arxana~' sibling before the write."
  (interactive)
  (unless (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (user-error "Not in a compiled-view buffer"))
  (unless (use-region-p)
    (user-error "Select the new anchor region first"))
  (let* ((new-passage (buffer-substring-no-properties
                       (region-beginning) (region-end)))
         (anns arxana-browser-essays-compiled--annotations)
         (id-choices
          (mapcar (lambda (a) (plist-get a :id)) anns))
         (chosen-id (completing-read "Re-anchor annotation: " id-choices nil t))
         (ann (cl-find-if (lambda (a)
                            (string= (plist-get a :id) chosen-id))
                          anns))
         (pairs (arxana-browser-essays-compiled--annotated-endpoint-pairs ann))
         (idx
          (cond
           ((= (length pairs) 1) (cdr (car pairs)))
           (t
            (let* ((options
                    (mapcar (lambda (pair)
                              (cons (format "idx=%d: %s"
                                            (cdr pair)
                                            (if (> (length (car pair)) 60)
                                                (concat (substring (car pair) 0 60) "...")
                                              (car pair)))
                                    (cdr pair)))
                            pairs))
                   (chosen-label (completing-read "Endpoint: "
                                                  (mapcar #'car options) nil t)))
              (cdr (assoc chosen-label options))))))
         (edn-path (arxana-browser-essays-compiled--edn-sibling-path
                    arxana-browser-essays-compiled--source-file)))
    (unless edn-path
      (user-error "No sibling annotations.edn found"))
    (let ((bk (concat edn-path "~arxana~")))
      (when (file-readable-p edn-path)
        (copy-file edn-path bk t)))
    (let ((result (arxana-browser-essays-compiled--rewrite-edn-endpoint-passage
                   edn-path chosen-id idx new-passage)))
      (cond
       ((eq result t)
        (arxana-browser-essays-compiled-refresh)
        (message "[compiled] Re-anchored %s endpoint %d (%d chars)"
                 chosen-id idx (length new-passage)))
       ((null result)
        (message "[compiled] No change — :passage already matched"))
       (t
        (message "[compiled] Re-anchor not applicable: %S" result))))))

(defun arxana-browser-essays-compiled-abort-edit ()
  "Abort the current compiled-view edit session.
Re-reads the source .md into the buffer in-place, discarding unsaved
edits; source .md is unchanged.  Preserves window layout (does not
re-open via `arxana-browser-essays-open-compiled')."
  (interactive)
  (unless (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (user-error "Not in a compiled-view buffer"))
  (unless arxana-browser-essays-compiled--edit-mode
    (user-error "Not in compiled-view edit mode"))
  (let ((src arxana-browser-essays-compiled--source-file))
    (unless (and src (file-readable-p src))
      (user-error "Source file not readable: %s" src))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert-file-contents src)
      (goto-char (point-min)))
    (arxana-browser-essays-compiled--refresh-overlays-in-place)
    (setq-local arxana-browser-essays-compiled--edit-mode nil)
    (setq buffer-read-only t)
    (use-local-map arxana-browser-essays-compiled-mode-map)
    (force-mode-line-update)
    (message "[compiled] Aborted edits; re-read from %s"
             (abbreviate-file-name src))))

;; -----------------------------------------------------------------
;; Hydra — '?' menu over Arxana Essays surfaces
;; -----------------------------------------------------------------

(require 'hydra)

(defun arxana-browser-essays-compiled--current-essay-id ()
  "Best-effort: return the essay-id associated with the current buffer.
Checks the compiled buffer's local var, then the per-section view's, then
falls back to prompting via the catalog completer."
  (or arxana-browser-essays-compiled--essay-id
      (and (boundp 'arxana-browser-essays--essay-id)
           arxana-browser-essays--essay-id)
      nil))

(defun arxana-browser-essays-compiled-open-current ()
  "Open the compiled view for the current buffer's essay (compiled or per-section),
or prompt if there isn't one in context."
  (interactive)
  (let ((id (arxana-browser-essays-compiled--current-essay-id)))
    (if id
        (arxana-browser-essays-open-compiled id)
      (call-interactively #'arxana-browser-essays-open-compiled))))

(defun arxana-browser-essays-compiled-open-rewrites-for-current ()
  "Open the Arxana Rewrites 3-up surface for the current essay's registered corpus.
The mapping is heuristic: a corpus is considered matching if its
`:annotations-file` lives under the same directory as the essay's
source-file (or if its `:id` slug appears in the essay-id)."
  (interactive)
  (let* ((id (arxana-browser-essays-compiled--current-essay-id))
         (cat (and id (arxana-browser-essays--catalog-spec id)))
         (source-dir (and cat (file-name-directory (plist-get cat :source-file))))
         (corpus (and source-dir
                      (cl-find-if
                       (lambda (c)
                         (let ((af (plist-get c :annotations-file)))
                           (and af (string-prefix-p source-dir af))))
                       (and (boundp 'arxana-browser-rewrites-corpora)
                            arxana-browser-rewrites-corpora)))))
    (cond
     ((not (fboundp 'arxana-browser-rewrites-open))
      (user-error "arxana-browser-rewrites not loaded"))
     (corpus
      (let ((arxana-browser-rewrites-default-corpus-id (plist-get corpus :id)))
        (arxana-browser-rewrites-open)))
     (t
      (call-interactively #'arxana-browser-rewrites-open)))))

;; -----------------------------------------------------------------
;; PDF export (pandoc) — bound to the essays hydra as `p'.
;; -----------------------------------------------------------------

(defcustom arxana-browser-essays-compiled-pdf-engine "xelatex"
  "LaTeX engine pandoc uses for `arxana-browser-essays-compiled-export-pdf'."
  :type 'string :group 'arxana-browser-essays-compiled)

(defcustom arxana-browser-essays-compiled-pdf-mainfont "DejaVu Serif"
  "Main font for PDF export — needs Unicode coverage (£ ≈ · → em-dash)."
  :type 'string :group 'arxana-browser-essays-compiled)

(defcustom arxana-browser-essays-compiled-pdf-papersize "a4"
  "Paper size for `arxana-browser-essays-compiled-export-pdf'."
  :type 'string :group 'arxana-browser-essays-compiled)

(defun arxana-browser-essays-compiled-export-pdf (&optional source-file)
  "Export the current essay's source markdown to a PDF via pandoc.
From a compiled view, uses the buffer-local
`arxana-browser-essays-compiled--source-file'; otherwise prompts for a
markdown file (or pass SOURCE-FILE non-interactively).  Renders with
`arxana-browser-essays-compiled-pdf-engine' on
`arxana-browser-essays-compiled-pdf-papersize' using
`arxana-browser-essays-compiled-pdf-mainfont'.  Writes <base>.pdf beside
the source and returns its path; on failure pops the pandoc log."
  (interactive)
  (let* ((src (or source-file
                  arxana-browser-essays-compiled--source-file
                  (read-file-name "Markdown to export: " nil nil t)))
         (src (and src (expand-file-name src))))
    (unless (and src (file-readable-p src))
      (user-error "No readable source markdown: %S" src))
    (let* ((out (concat (file-name-sans-extension src) ".pdf"))
           (pandoc (or (executable-find "pandoc")
                       (user-error "pandoc not found on `exec-path'")))
           (args (list src "-o" out
                       (format "--pdf-engine=%s"
                               arxana-browser-essays-compiled-pdf-engine)
                       "-V" (format "mainfont=%s"
                                    arxana-browser-essays-compiled-pdf-mainfont)
                       "-V" (format "papersize=%s"
                                    arxana-browser-essays-compiled-pdf-papersize)
                       "-V" "geometry:margin=1in"
                       "-V" "fontsize=11pt"
                       "-V" "colorlinks=true"))
           (buf (get-buffer-create "*arxana-pdf-export*")))
      (with-current-buffer buf (erase-buffer))
      (let ((status (apply #'call-process pandoc nil buf nil args)))
        (if (and (eql status 0) (file-readable-p out))
            (progn (message "[essays] PDF exported (%s): %s"
                            arxana-browser-essays-compiled-pdf-papersize out)
                   out)
          (progn (display-buffer buf)
                 (user-error "pandoc failed (status %s); see *arxana-pdf-export*"
                             status)))))))

(defhydra arxana-browser-essays-hydra (:color blue :hint nil)
  "
 Arxana Essays — ? menu
 ─────────────────────
 _c_ompiled view (this essay or pick)         _o_ open essays-home
 _R_ewrites 3-up surface (for this corpus)    _r_ refresh manifests
 _e_dit current section                       _s_ave section (in edit-mode)
 _a_udit passages (manifest vs prose)         _i_mport (XTDB)
 _C_omment: add text comment at point         _x_ export markdown
 _h_ checkpoint save                          _l_ checkpoint list
 _p_ export PDF (A4)                          _?_ describe-mode
 _q_ quit
"
  ("c" arxana-browser-essays-compiled-open-current)
  ("o" (lambda () (interactive)
         (if (fboundp 'arxana-browser-essays-open)
             (call-interactively #'arxana-browser-essays-open)
           (find-file "~/code/futon4/dev/arxana-browser-essays.el"))))
  ("R" arxana-browser-essays-compiled-open-rewrites-for-current)
  ("r" arxana-browser-essays-refresh)
  ("e" (lambda () (interactive)
         (cond
          ((derived-mode-p 'arxana-browser-essays-compiled-mode)
           (arxana-browser-essays-compiled-edit-mode-toggle))
          ((fboundp 'arxana-browser-essays-edit-mode)
           (arxana-browser-essays-edit-mode 1))
          (t (message "Edit mode not available in this buffer.")))))
  ("s" (lambda () (interactive)
         (if (fboundp 'arxana-browser-essays-save-section)
             (call-interactively #'arxana-browser-essays-save-section)
           (message "Save-section not available in this buffer."))))
  ("a" arxana-browser-essays-audit-passages)
  ("i" arxana-browser-essays-import)
  ("C" (lambda () (interactive)
         (if (fboundp 'arxana-browser-essays-add-comment)
             (call-interactively #'arxana-browser-essays-add-comment)
           (message "add-comment not available."))))
  ("x" (lambda () (interactive)
         (if (fboundp 'arxana-browser-essays-export-markdown)
             (call-interactively #'arxana-browser-essays-export-markdown)
           (message "export-markdown not available."))))
  ("p" arxana-browser-essays-compiled-export-pdf)
  ("h" (lambda () (interactive)
         (if (fboundp 'arxana-browser-essays-checkpoint-save)
             (call-interactively #'arxana-browser-essays-checkpoint-save)
           (message "checkpoint-save not available."))))
  ("l" (lambda () (interactive)
         (if (fboundp 'arxana-browser-essays-checkpoint-list)
             (arxana-browser-essays-checkpoint-list)
           (message "checkpoint-list not available."))))
  ("?" describe-mode)
  ("q" nil "quit"))

;; Bind '?' in the compiled-view mode (we own this keymap).
(define-key arxana-browser-essays-compiled-mode-map
            (kbd "?") #'arxana-browser-essays-hydra/body)
;; 🥨 (U+1F968): a dedicated hydra key that ALSO works in edit mode, where
;; `?' must remain self-inserting.  Bound wherever `?' opens the essays hydra.
(define-key arxana-browser-essays-compiled-mode-map
            (kbd "🥨") #'arxana-browser-essays-hydra/body)

;; Bind '?' in the per-section reading view's minor-mode keymap, if available.
;; Hooked off the existing arxana-browser-essays-text-sync-mode-map so it
;; appears whenever the reading minor-mode is on (i.e. read mode, not edit
;; mode — '?' should stay self-insert during edits).
(when (boundp 'arxana-browser-essays-text-sync-mode-map)
  (define-key arxana-browser-essays-text-sync-mode-map
              (kbd "?") #'arxana-browser-essays-hydra/body)
  (define-key arxana-browser-essays-text-sync-mode-map
              (kbd "🥨") #'arxana-browser-essays-hydra/body))

;; Notes buffers: special-mode binds '?' to describe-mode by default.  In
;; both the per-section *Arxana Essay Notes* and the compiled *Arxana Essay
;; Compiled Notes*, route '?' to the hydra (describe-mode still reachable
;; via the hydra body).
(defun arxana-browser-essays-compiled--install-notes-key ()
  "Hook function: in any notes buffer, bind local '?' to the essays hydra."
  (when (and (derived-mode-p 'special-mode)
             (or (string= (buffer-name) arxana-browser-essays-compiled-notes-buffer)
                 (and (boundp 'arxana-browser-essays-notes-buffer)
                      (string= (buffer-name) arxana-browser-essays-notes-buffer))))
    (use-local-map (copy-keymap (current-local-map)))
    (local-set-key (kbd "?") #'arxana-browser-essays-hydra/body)
    (local-set-key (kbd "🥨") #'arxana-browser-essays-hydra/body)))

;; Install the hook so future notes-buffer setups pick it up.
(add-hook 'special-mode-hook #'arxana-browser-essays-compiled--install-notes-key)

;; Arxana Browser '?': when the browser is in an essays view, route to the
;; essays hydra instead of the generic browser/media hydra.  Other views
;; (media, docbook, evidence, lab, patterns, ...) fall through to the
;; original `arxana-browser-help'.

(defun arxana-browser-essays-compiled--browser-help-advice (orig-fn &rest args)
  "Dispatch `arxana-browser-help' to the essays hydra in essays views."
  (let ((view (and (boundp 'arxana-browser--context)
                   (plist-get arxana-browser--context :view))))
    (cond
     ((memq view '(essays-home essays-essay essays-section))
      (arxana-browser-essays-hydra/body))
     (t (apply orig-fn args)))))

(when (fboundp 'arxana-browser-help)
  (advice-add 'arxana-browser-help
              :around #'arxana-browser-essays-compiled--browser-help-advice))

;; Apply immediately to any existing notes buffers in the current session.
(dolist (bname (list arxana-browser-essays-compiled-notes-buffer
                     (and (boundp 'arxana-browser-essays-notes-buffer)
                          arxana-browser-essays-notes-buffer)))
  (when (and bname (get-buffer bname))
    (with-current-buffer (get-buffer bname)
      (arxana-browser-essays-compiled--install-notes-key))))

;; -----------------------------------------------------------------
;; Notes → text reverse sync (backlink highlight)
;; -----------------------------------------------------------------
;;
;; When the cursor enters an annotation entry in the *Arxana Essay
;; Compiled Notes* side pane, the corresponding anchor in the compiled
;; text buffer is (a) raised into view by scrolling the text window and
;; (b) painted with a high-priority transient overlay so the operator
;; can see WHERE in the essay this note is anchored.  Complements the
;; existing text → notes sync (`--post-command-sync`); the two run
;; independently.  The text-side active overlay uses a distinct face
;; from the always-on annotation overlay so the operator can tell
;; "currently followed" from "anchored here".

(defface arxana-browser-essays-compiled-active-anchor-face
  '((t :background "#fda4af" :foreground "#7f1d1d" :weight bold))
  "Face for the currently-followed annotation anchor in the compiled
text buffer.  Applied transiently when the cursor sits on the
corresponding entry in the notes side pane; cleared when the cursor
moves off.  Higher priority than the always-on annotation overlay so
the followed anchor stands out against the constellation of all anchors."
  :group 'arxana-browser-essays-compiled)

(defvar-local arxana-browser-essays-compiled--notes-text-buffer nil
  "Buffer-local in the notes buffer: the companion compiled text buffer.")

(defvar-local arxana-browser-essays-compiled--notes-last-followed-id nil
  "Buffer-local in the notes buffer: last :id whose backlink was applied.")

(defvar-local arxana-browser-essays-compiled--text-active-anchor-overlay nil
  "Buffer-local in the text buffer: overlay marking the currently-followed
backlink, or nil.")

(defun arxana-browser-essays-compiled--clear-text-active-anchor (text-buffer)
  "Delete the active-anchor overlay in TEXT-BUFFER, if any."
  (when (buffer-live-p text-buffer)
    (with-current-buffer text-buffer
      (when (overlayp arxana-browser-essays-compiled--text-active-anchor-overlay)
        (delete-overlay arxana-browser-essays-compiled--text-active-anchor-overlay)
        (setq arxana-browser-essays-compiled--text-active-anchor-overlay nil)))))

(defun arxana-browser-essays-compiled--text-overlay-for-id (text-buffer id)
  "Return the leftmost annotation overlay in TEXT-BUFFER for ID, or nil.
Multi-anchor annotations have multiple overlays; pick the lowest BEG so
the backlink scrolls to the first occurrence in document order."
  (when (buffer-live-p text-buffer)
    (with-current-buffer text-buffer
      (let (best)
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (and (equal (overlay-get ov 'arxana-essays-compiled-ann-id) id)
                     (> (overlay-end ov) (overlay-start ov)))
            (when (or (null best)
                      (< (overlay-start ov) (overlay-start best)))
              (setq best ov))))
        best))))

(defun arxana-browser-essays-compiled--highlight-text-anchor (text-buffer id source-screen-row)
  "Apply the transient active-anchor overlay in TEXT-BUFFER for annotation ID,
and recenter TEXT-BUFFER's window so the anchor sits at SOURCE-SCREEN-ROW
screen rows from the window top — aligning it side-by-side with point in
the companion notes window.  Returns t if an anchor was found and
highlighted, nil if the annotation is orphan."
  (arxana-browser-essays-compiled--clear-text-active-anchor text-buffer)
  (let ((ov (arxana-browser-essays-compiled--text-overlay-for-id text-buffer id)))
    (when ov
      (with-current-buffer text-buffer
        (let* ((beg (overlay-start ov))
               (end (overlay-end ov))
               (active (make-overlay beg end)))
          (overlay-put active 'face
                       'arxana-browser-essays-compiled-active-anchor-face)
          (overlay-put active 'priority 100)
          (overlay-put active 'arxana-essays-compiled-active-anchor t)
          (setq arxana-browser-essays-compiled--text-active-anchor-overlay
                active))
        ;; Restrict to the current frame: text-buffer may be displayed
        ;; in multiple frames; we want the one in the operator's current
        ;; frame, where they just moved the notes cursor.
        (let ((win (or (get-buffer-window text-buffer nil)
                       (get-buffer-window text-buffer 0))))
          (when (window-live-p win)
            (let ((beg (overlay-start ov)))
              ;; Move window-point WITHOUT entering the window: keeps the
              ;; text buffer's point alone (so the text→notes sync isn't
              ;; perturbed by the reverse sync touching point), while still
              ;; bringing the anchor into view.
              (set-window-point win beg)
              (with-selected-window win
                (arxana-browser-essays-compiled--recenter-target-at-row
                 source-screen-row))))))
      t)))

(defun arxana-browser-essays-compiled--notes-id-at-point ()
  "Return the `arxana-essays-compiled-ann-id' text property at point, or nil."
  (get-text-property (point) 'arxana-essays-compiled-ann-id))

(defun arxana-browser-essays-compiled--notes-post-command-sync ()
  "In the notes buffer, sync the text buffer's backlink highlight + scroll.
Runs in `post-command-hook' inside the notes buffer; no-ops when the
annotation under point is unchanged since the last call.

Also keeps the notes-side active-entry overlay in step with point in
the notes buffer.  Without this, the active-entry overlay would only
be repainted on essay→notes syncs, leaving stale highlights when the
operator moves the cursor in the notes pane itself."
  (let ((text-buffer arxana-browser-essays-compiled--notes-text-buffer)
        (id (arxana-browser-essays-compiled--notes-id-at-point))
        (source-row (arxana-browser-essays-compiled--current-screen-line)))
    (when (buffer-live-p text-buffer)
      (cond
       ((and id
             (not (equal id arxana-browser-essays-compiled--notes-last-followed-id)))
        (setq arxana-browser-essays-compiled--notes-last-followed-id id)
        (arxana-browser-essays-compiled--paint-notes-active-entry id)
        (unless (arxana-browser-essays-compiled--highlight-text-anchor
                 text-buffer id source-row)
          (message "[compiled-backlink] No anchor located for %s (orphan)"
                   id)))
       ((and (null id)
             arxana-browser-essays-compiled--notes-last-followed-id)
        (setq arxana-browser-essays-compiled--notes-last-followed-id nil)
        (arxana-browser-essays-compiled--clear-notes-active)
        (arxana-browser-essays-compiled--clear-text-active-anchor
         text-buffer))
       ;; Same annotation under point but the notes cursor row changed
       ;; (e.g. scrolling within a long note) — keep alignment fresh.
       ((and id
             (equal id arxana-browser-essays-compiled--notes-last-followed-id)
             (overlayp arxana-browser-essays-compiled--text-active-anchor-overlay))
        (let ((win (or (get-buffer-window text-buffer nil)
                       (get-buffer-window text-buffer 0)))
              (ov arxana-browser-essays-compiled--text-active-anchor-overlay))
          (when (and (window-live-p win) (overlay-buffer ov))
            (set-window-point win (overlay-start ov))
            (with-selected-window win
              (arxana-browser-essays-compiled--recenter-target-at-row
               source-row)))))))))

;; -----------------------------------------------------------------
;; Interactive invisible-ink marking
;; -----------------------------------------------------------------
;;
;; `arxana-invisible-ink-region' lets the operator mark an arbitrary
;; region in the compiled view as invisible ink.  Two cases:
;;
;;   - Region overlaps existing annotation overlays: each affected
;;     annotation gains the "invisible-ink" label, and its overlays are
;;     repainted with the composed (base + invisible-ink) face.  This is
;;     the "correspondence is maintained" path: the annotation itself
;;     now asserts that this passage is invisible ink, so the marking
;;     survives any subsequent overlay refresh that consults the
;;     manifest.
;;
;;   - Region has no overlapping annotation: a fresh annotation of
;;     `:hx-type "annotation/invisible-ink"' is pushed into the buffer's
;;     in-memory `--annotations' list, and an overlay is placed.  This
;;     keeps the marking discoverable as a first-class annotation (it
;;     will appear in the notes pane after the next
;;     `arxana-browser-essays-compiled-refresh').
;;
;; Persistence to disk is out of scope for v1; the markings live in the
;; buffer-local manifest until the buffer is closed.  Joe can run
;; `arxana-browser-essays-compiled-refresh' to re-render the notes pane
;; including newly-created invisible-ink annotations.

(defun arxana-browser-essays-compiled--repaint-overlays-for-ann (id)
  "Recompute and apply the overlay face for every overlay tagged with ID
in the current buffer.  Used after an annotation's labels change (e.g.
gaining \"invisible-ink\") so its overlays reflect the new face."
  (let ((ann (cl-find-if (lambda (a) (equal (plist-get a :id) id))
                         arxana-browser-essays-compiled--annotations)))
    (when ann
      (let* ((pairs (arxana-browser-essays-compiled--annotated-endpoint-pairs ann))
             (face (arxana-browser-essays-compiled--passage-face ann (length pairs))))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (equal (overlay-get ov 'arxana-essays-compiled-ann-id) id)
            (overlay-put ov 'face face)))))))

;;;###autoload
(defun arxana-invisible-ink-region (beg end)
  "Mark the region BEG..END in the compiled view as invisible ink.

A fresh annotation of `:hx-type \"annotation/invisible-ink\"' is added
to the in-memory manifest covering the entire selection, regardless of
whether the region already overlaps existing annotations.  This means
nested invisible-ink markings work: you can mark a verbatim slice
invisible, then later mark its surrounding paragraph invisible — both
annotations co-exist and together cover the whole outer range.

In addition, if the region overlaps existing annotations (other than
ad-hoc invisible-ink ones), each gains the \"invisible-ink\" label so
the data model records that this passage has been marked invisible
even outside the wrapping ad-hoc annotation's scope.

Both effects are persisted to a sibling
`<essay-basename>.user-annotations.el' sidecar so the markings survive
across buffer kills and Emacs restarts.  Re-opening the compiled view
loads the sidecar and re-applies the changes.  The canonical
`<essay>-annotations.el' manifest is never modified."
  (interactive "r")
  (unless (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (user-error "Not in a compiled-view buffer"))
  (unless (use-region-p)
    (user-error "No active region"))
  (let* ((overlapping-ann-ids
          (cl-remove-duplicates
           (delq nil
                 (mapcar (lambda (ov)
                           (overlay-get ov 'arxana-essays-compiled-ann-id))
                         (overlays-in beg end)))
           :test 'equal))
         (changes arxana-browser-essays-compiled--user-annotation-changes)
         (tagged-count 0))
    ;; --- (1) Tag any overlapping existing annotations with "invisible-ink" ---
    (dolist (id overlapping-ann-ids)
      (let ((ann (cl-find-if (lambda (a) (equal (plist-get a :id) id))
                             arxana-browser-essays-compiled--annotations)))
        (when ann
          (let ((labels (plist-get ann :labels)))
            (unless (member "invisible-ink" labels)
              (setq ann (plist-put ann :labels
                                   (cons "invisible-ink" labels)))
              (cl-incf tagged-count)))
          ;; Record the label addition in the sidecar tracker (skip the
          ;; sidecar-created hx:invisible-ink:... annotations — their
          ;; labels are already persisted as part of the :new-annotations
          ;; payload).
          (unless (string-prefix-p "hx:invisible-ink:" id)
            (let* ((existing-additions (plist-get changes :label-additions))
                   (entry (assoc id existing-additions)))
              (cond
               (entry
                (unless (member "invisible-ink" (cdr entry))
                  (setcdr entry (append (cdr entry) (list "invisible-ink")))))
               (t
                (setq changes
                      (plist-put changes
                                 :label-additions
                                 (cons (cons id (list "invisible-ink"))
                                       existing-additions)))))))
          (arxana-browser-essays-compiled--repaint-overlays-for-ann id))))
    ;; --- (2) Always create a new wrapping ad-hoc annotation for [beg, end] ---
    (let* ((id (format "hx:invisible-ink:%s"
                       (format-time-string "%Y%m%dT%H%M%S")))
           (passage (buffer-substring-no-properties beg end))
           (manifest (and arxana-browser-essays-compiled--essay-id
                          (arxana-browser-essays--manifest-for
                           arxana-browser-essays-compiled--essay-id)))
           (sections (and manifest (plist-get manifest :sections)))
           (section-id (or (plist-get (car sections) :id)
                           arxana-browser-essays-compiled--essay-id
                           "ad-hoc"))
           (ann (list :id id
                      :hx-type "annotation/invisible-ink"
                      :annotated (list :entity-id section-id
                                       :passage passage)
                      :source (list :pattern-name nil :passage nil)
                      :note (format "Invisible-ink region marked %s%s"
                                    (format-time-string "%Y-%m-%d %H:%M")
                                    (if overlapping-ann-ids
                                        (format "; wraps %d nested annotation%s"
                                                (length overlapping-ann-ids)
                                                (if (= (length overlapping-ann-ids) 1) "" "s"))
                                      ""))
                      :labels '("invisible-ink"))))
      (push ann arxana-browser-essays-compiled--annotations)
      (setq changes
            (plist-put changes
                       :new-annotations
                       (cons ann (plist-get changes :new-annotations))))
      (setq arxana-browser-essays-compiled--user-annotation-changes changes)
      (let ((ov (make-overlay beg end)))
        ;; Lower priority than nested annotations so the inner overlay's
        ;; face (which may carry hx-type-specific styling) still wins at
        ;; nested positions.  Both apply invisible-ink, so the gaps
        ;; between nested overlays are correctly faded by this wrapper.
        (overlay-put ov 'face 'arxana-invisible-ink)
        (overlay-put ov 'priority -1)
        (overlay-put ov 'arxana-essays-compiled-ann-id id)
        (overlay-put ov 'arxana-essays-compiled-endpoint-index -1)
        (overlay-put ov 'arxana-essays-compiled-position beg)
        (overlay-put ov 'arxana-essays-compiled-quality 'manual-region)
        (overlay-put ov 'help-echo
                     (format "[%s | annotation/invisible-ink | manual-region]" id)))
      ;; Text-property tagging would mask any nested annotation's
      ;; ann-id property at the same positions.  Skip property tagging
      ;; for wrapping ad-hoc annotations so nested annotations remain
      ;; addressable via `--annotation-at-point' and the reconciler.
      ;; The overlay alone (with low priority) carries the visual.
      (when (zerop (length overlapping-ann-ids))
        (with-silent-modifications
          (put-text-property beg end 'arxana-essays-compiled-ann-id id)
          (put-text-property beg end 'arxana-essays-compiled-endpoint-index -1)))
      (arxana-browser-essays-compiled--persist-user-annotations)
      (cond
       (overlapping-ann-ids
        (message "[invisible-ink] Created %s wrapping %d nested annotation%s; %d gained \"invisible-ink\" label; persisted to %s"
                 id
                 (length overlapping-ann-ids)
                 (if (= (length overlapping-ann-ids) 1) "" "s")
                 tagged-count
                 (file-name-nondirectory
                  (arxana-browser-essays-compiled--user-annotations-sidecar-path
                   arxana-browser-essays-compiled--source-file))))
       (t
        (message "[invisible-ink] Created %s on region [%d-%d]; persisted to %s"
                 id beg end
                 (file-name-nondirectory
                  (arxana-browser-essays-compiled--user-annotations-sidecar-path
                   arxana-browser-essays-compiled--source-file))))))))

;;;###autoload
(defun arxana-invisible-ink-persist ()
  "Re-write the current buffer's user-annotations sidecar to disk.
Use to force-save the in-memory `--user-annotation-changes' if you
manipulated them directly (Arxana already writes the sidecar on every
`arxana-invisible-ink-region' call, so manual persistence is rarely
needed)."
  (interactive)
  (unless (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (user-error "Not in a compiled-view buffer"))
  (let ((path (arxana-browser-essays-compiled--persist-user-annotations)))
    (if path
        (message "[invisible-ink] Persisted to %s" (abbreviate-file-name path))
      (message "[invisible-ink] No source-file bound; nothing to persist"))))

;; -----------------------------------------------------------------
;; Yank without annotation (C-S-y in compiled-view edit mode)
;; -----------------------------------------------------------------
;;
;; The compiled view stamps `arxana-essays-compiled-ann-id' as a text
;; property on every annotated passage, mirroring the overlay.  Kill /
;; yank carries that property through the kill ring; the
;; `--reconcile-overlays-from-properties' post-command sweep then
;; recreates an overlay at the yank target — useful when you're
;; deliberately moving an annotated passage, awkward when you just
;; want a plain copy of some prose.
;;
;; `arxana-browser-essays-compiled-yank-without-annotation' strips
;; the Arxana annotation properties from the yanked text immediately
;; after insertion, so no overlay is reconstructed and the pasted text
;; is plain — no invisible-ink face, no backlink to the original
;; annotation's notes entry.  Bound to C-S-y in the edit map.
;;
;; Caveat: if you yank INSIDE an existing annotation overlay, Emacs
;; marker semantics extend that overlay across the inserted text;
;; this command can't undo that without splitting the surrounding
;; overlay.  To paste truly outside any annotation, position point
;; outside existing overlays first.

;;;###autoload
(defun arxana-browser-essays-compiled-yank-without-annotation ()
  "Yank like `yank' but strip Arxana annotation text properties from the
inserted text.  After yank, the reconcile sweep sees no
`arxana-essays-compiled-ann-id' in the yanked region, so no new
overlay is created and the pasted prose appears plain (no invisible-ink
face, no backlink to the original annotation's notes entry)."
  (interactive)
  (let ((start (point)))
    (yank)
    (with-silent-modifications
      (remove-list-of-text-properties
       start (point)
       '(arxana-essays-compiled-ann-id
         arxana-essays-compiled-endpoint-index)))))

(provide 'arxana-browser-essays-compiled)
;;; arxana-browser-essays-compiled.el ends here
