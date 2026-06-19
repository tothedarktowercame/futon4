;;; arxana-vsatarcs-editorial.el --- Stage 1 of M-editorial-assistant: editorial-log substrate -*- lexical-binding: t; -*-

;; Author: claude-10 for Joe (2026-05-21)
;; Status: Stage 1 v0. Schema + a single resolve-comment command. Future
;; commands (add-comment, reject-comment, show-history) are designed for
;; but not implemented in v0; the schema accommodates them.

;;; Commentary:
;;
;; M-editorial-assistant Stage 1.  The editorial log captures operator
;; resolutions of reviewer-comments on essays.  Per the mission HEAD,
;; the log is a per-essay sidecar (`editorial-log.edn') living next to
;; the essay's `annotations.edn' in the canonical home
;; `~/code/futonN/essays/<slug>/'.  Append-only; multiple log entries
;; per annotation supported (stack-as-thread per Q5).
;;
;; This module separates concerns from `annotations.edn' (hand-authored
;; survey-pass artefact, comments + formatting matter) by writing only
;; into the sibling file.  The Codex projector for M-essay-corpus-substrate
;; D2 picks up both files from the same directory.
;;
;; Schema (editorial-log.edn):
;;
;;   {:version       1
;;    :essay-id      "arxana/essay/<slug>-v1"
;;    :editorial-log
;;    [{:id            "log-NNN"
;;      :annotation-id "<hx-id>"        ; the annotation being resolved
;;      :timestamp     "ISO-8601"
;;      :author        :operator        ; or :peripheral when Stage 2 lands
;;      :resolution    :addressed
;;                   | :addressed-differently   ; rationale required
;;                   | :rejected                ; rationale required
;;                   | :deferred                ; rationale required
;;      :rationale     "string"
;;      :edit          {:before "..." :after "..."}  ; or nil (intent recorded; prose deferred)
;;     ...]}
;;
;; The derived "current resolution state" for an annotation is the
;; latest log entry's :resolution (or :pending if no entries exist).
;; Computed on read; not stored on the annotation itself, so the log
;; stays the single source of truth.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'arxana-browser-rewrites)         ; --read-edn-file
(require 'arxana-browser-essays-compiled)  ; --annotation-at-point + buffer-locals

(defvar arxana-vsatarcs-editorial--resolution-choices
  '((:addressed             . "Addressed exactly as reviewer suggested")
    (:addressed-differently . "Addressed differently — rationale required")
    (:rejected              . "Rejected — rationale required")
    (:deferred              . "Deferred — rationale required"))
  "Allowed :resolution values + completion-prompt labels.")

;; -----------------------------------------------------------------
;; Path resolution
;; -----------------------------------------------------------------

(defun arxana-vsatarcs-editorial--log-path (annotations-path)
  "Return the sibling editorial-log.edn path for ANNOTATIONS-PATH."
  (expand-file-name "editorial-log.edn"
                    (file-name-directory annotations-path)))

(defun arxana-vsatarcs-editorial--essay-annotations-path ()
  "Return the annotations.edn path for the current compiled-view buffer."
  (unless (derived-mode-p 'arxana-browser-essays-compiled-mode)
    (user-error "Not in an Arxana compiled-view buffer"))
  (let ((src arxana-browser-essays-compiled--source-file))
    (unless src
      (user-error "Compiled view has no source-file local"))
    (let ((sibling (expand-file-name "annotations.edn"
                                     (file-name-directory src))))
      (unless (file-readable-p sibling)
        (user-error "No sibling annotations.edn at %s" sibling))
      sibling)))

;; -----------------------------------------------------------------
;; Read / append helpers
;; -----------------------------------------------------------------

(defun arxana-vsatarcs-editorial--read-log (log-path)
  "Read editorial-log.edn at LOG-PATH; return data plist or nil if absent."
  (when (file-readable-p log-path)
    (arxana-browser-rewrites--read-edn-file log-path)))

(defun arxana-vsatarcs-editorial--existing-entries (log-data)
  "Return the :editorial-log vector entries from LOG-DATA as a list."
  (append (and log-data (plist-get log-data :editorial-log)) nil))

(defun arxana-vsatarcs-editorial--next-log-id (existing-entries)
  "Return the next log-NNN id given EXISTING-ENTRIES."
  (format "log-%03d" (1+ (length existing-entries))))

(defun arxana-vsatarcs-editorial--timestamp ()
  "Return current time as an ISO-8601 string."
  (format-time-string "%Y-%m-%dT%H:%M:%S%z"))

(defun arxana-vsatarcs-editorial--edn-escape (s)
  "Escape S for embedding inside an EDN double-quoted string."
  (replace-regexp-in-string
   "\"" "\\\\\""
   (replace-regexp-in-string "\\\\" "\\\\\\\\" s)))

(defun arxana-vsatarcs-editorial--render-entry (entry)
  "Render a log ENTRY plist as a multi-line EDN map string.
ENTRY may omit :edit, in which case it renders as nil; future log
entries can append before/after pairs when prose is written."
  (let* ((id   (plist-get entry :id))
         (ann  (plist-get entry :annotation-id))
         (ts   (plist-get entry :timestamp))
         (auth (plist-get entry :author))
         (res  (plist-get entry :resolution))
         (rat  (plist-get entry :rationale))
         (edit (plist-get entry :edit)))
    (concat
     "  {:id            \"" id "\"\n"
     "   :annotation-id \"" ann "\"\n"
     "   :timestamp     \"" ts "\"\n"
     "   :author        " (format "%s" auth) "\n"
     "   :resolution    " (format "%s" res) "\n"
     "   :rationale     \""
     (arxana-vsatarcs-editorial--edn-escape (or rat "")) "\"\n"
     "   :edit          " (if edit (format "%S" edit) "nil") "}")))

(defun arxana-vsatarcs-editorial--write-fresh (log-path entry essay-id)
  "Create a new LOG-PATH containing the schema header + first ENTRY for ESSAY-ID."
  (with-temp-buffer
    (insert ";; editorial-log.edn --- M-editorial-assistant per-essay resolution log\n")
    (insert ";; Schema: see arxana-vsatarcs-editorial.el commentary.\n")
    (insert ";; Append-only; multiple entries per annotation = stack-as-thread (Q5).\n;;\n")
    (insert "{:version       1\n")
    (insert " :essay-id      \"" essay-id "\"\n")
    (insert " :editorial-log\n [\n")
    (insert (arxana-vsatarcs-editorial--render-entry entry))
    (insert "\n ]}\n")
    (write-region (point-min) (point-max) log-path nil 'silent)))

(defun arxana-vsatarcs-editorial--append-existing (log-path entry)
  "Append ENTRY to an existing editorial-log.edn at LOG-PATH.
Walks the EDN syntactically to find the closing ] of :editorial-log."
  (with-temp-buffer
    (insert-file-contents log-path)
    (goto-char (point-min))
    (unless (re-search-forward ":editorial-log[\n\t ]*\\[" nil t)
      (user-error "editorial-log.edn at %s is missing the :editorial-log vector" log-path))
    ;; Walk to matching closing ]
    (let ((depth 1))
      (while (and (> depth 0) (not (eobp)))
        (cond
         ((looking-at "\\[") (cl-incf depth) (forward-char 1))
         ((looking-at "\\]") (cl-decf depth)
          (unless (zerop depth) (forward-char 1)))
         ((looking-at "\"")
          ;; skip string literal to avoid mis-counting brackets inside it
          (forward-char 1)
          (while (and (not (eobp))
                      (not (looking-at "\"")))
            (when (looking-at "\\\\") (forward-char 1))
            (forward-char 1))
          (when (looking-at "\"") (forward-char 1)))
         (t (forward-char 1))))
      (when (> depth 0)
        (user-error "Unbalanced brackets in %s" log-path)))
    ;; point is at the closing ]; insert before it
    (insert "\n" (arxana-vsatarcs-editorial--render-entry entry) "\n ")
    (write-region (point-min) (point-max) log-path nil 'silent)))

(defun arxana-vsatarcs-editorial--append-entry (log-path entry essay-id)
  "Append ENTRY to editorial-log.edn at LOG-PATH; create if missing."
  (if (file-readable-p log-path)
      (arxana-vsatarcs-editorial--append-existing log-path entry)
    (arxana-vsatarcs-editorial--write-fresh log-path entry essay-id))
  log-path)

;; -----------------------------------------------------------------
;; Public command
;; -----------------------------------------------------------------

;;;###autoload
(defun arxana-vsatarcs-editorial-resolve-comment ()
  "Record an operator resolution against the annotation at point.

Prompts for resolution kind + rationale; appends a log entry to
editorial-log.edn next to the current essay's annotations.edn.
The :edit field is left nil at recording-time — capture the actual
prose change as a follow-up log entry once written (stack-as-thread)."
  (interactive)
  (let ((ann-id (arxana-browser-essays-compiled--annotation-at-point)))
    (unless ann-id
      (user-error "No annotation at point"))
    (let* ((annotations-path (arxana-vsatarcs-editorial--essay-annotations-path))
           (log-path (arxana-vsatarcs-editorial--log-path annotations-path))
           (annotations-data
            (arxana-browser-rewrites--read-edn-file annotations-path))
           (essay-id (plist-get annotations-data :essay-id))
           (res-label
            (completing-read
             (format "Resolution for %s: " ann-id)
             (mapcar #'cdr arxana-vsatarcs-editorial--resolution-choices)
             nil t))
           (resolution
            (car (cl-find-if
                  (lambda (pair) (string= (cdr pair) res-label))
                  arxana-vsatarcs-editorial--resolution-choices)))
           (rationale
            (let ((r (read-string
                      (if (eq resolution :addressed)
                          "Rationale (optional for :addressed): "
                        (format "Rationale (required for %s): " resolution)))))
              (when (and (not (eq resolution :addressed))
                         (string-empty-p (string-trim r)))
                (user-error "Rationale required for %s" resolution))
              r))
           (existing-entries
            (arxana-vsatarcs-editorial--existing-entries
             (arxana-vsatarcs-editorial--read-log log-path)))
           (entry (list :id (arxana-vsatarcs-editorial--next-log-id existing-entries)
                        :annotation-id ann-id
                        :timestamp (arxana-vsatarcs-editorial--timestamp)
                        :author :operator
                        :resolution resolution
                        :rationale rationale)))
      (arxana-vsatarcs-editorial--append-entry log-path entry essay-id)
      ;; Auto-refresh the compiled view so the new resolution state lands
      ;; in the notes pane (anchor-release + green badge) without an
      ;; explicit `g'.  Safe no-op outside compiled-view buffers.
      (when (and (derived-mode-p 'arxana-browser-essays-compiled-mode)
                 (fboundp 'arxana-browser-essays-compiled-refresh))
        (arxana-browser-essays-compiled-refresh))
      (message "Recorded %s (%s) against %s in %s"
               (plist-get entry :id) resolution ann-id
               (file-name-nondirectory log-path)))))

;; -----------------------------------------------------------------
;; Annotation decoration (address-then-orphan cycle)
;; -----------------------------------------------------------------

(defun arxana-vsatarcs-editorial--latest-by-annotation (entries)
  "Hash-table mapping annotation-id → latest log entry from ENTRIES.
Stack-as-thread (Q5): if multiple entries exist for the same annotation,
the last one wins."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (e entries)
      (let ((aid (plist-get e :annotation-id)))
        (when aid
          (puthash aid e h))))
    h))

(defun arxana-vsatarcs-editorial-decorate-annotations (anns annotations-path)
  "Return ANNS with each annotation possibly augmented by resolution state.
ANNOTATIONS-PATH points at the annotations.edn whose sibling
editorial-log.edn we consult.  Annotations with a log entry get
:resolution-state, :resolution-rationale, :resolution-timestamp, and
:resolution-log-id attached (latest entry wins).  Annotations without a
log entry are returned unchanged.

This is the address-then-orphan-cycle hook: compiled.el calls this
function (via `fboundp' check) when loading annotations, so the notes
pane can render whether each annotation has been actioned."
  (let* ((log-path (arxana-vsatarcs-editorial--log-path annotations-path))
         (log-data (arxana-vsatarcs-editorial--read-log log-path))
         (entries (arxana-vsatarcs-editorial--existing-entries log-data))
         (by-aid (arxana-vsatarcs-editorial--latest-by-annotation entries)))
    (mapcar
     (lambda (ann)
       (let* ((id (plist-get ann :id))
              (entry (gethash id by-aid)))
         (if entry
             (append ann
                     (list :resolution-state    (plist-get entry :resolution)
                           :resolution-rationale (plist-get entry :rationale)
                           :resolution-timestamp (plist-get entry :timestamp)
                           :resolution-log-id    (plist-get entry :id)))
           ann)))
     anns)))

;; -----------------------------------------------------------------
;; Show-history changelog command
;; -----------------------------------------------------------------

(defvar arxana-vsatarcs-editorial-history-buffer-format
  "*Arxana Editorial History: %s*"
  "Buffer-name format for the per-essay editorial changelog.")

(defface arxana-vsatarcs-editorial-history-header-face
  '((t :weight bold :foreground "#1e40af"))
  "Face for the changelog header line.")

(defface arxana-vsatarcs-editorial-history-entry-face
  '((t :foreground "#075985"))
  "Face for entry separator lines in the changelog.")

(defface arxana-vsatarcs-editorial-history-rationale-face
  '((t :slant italic :foreground "#374151"))
  "Face for the rationale string in the changelog.")

;;;###autoload
(defun arxana-vsatarcs-editorial-show-history ()
  "Render the editorial log for the current essay as a changelog buffer.

Call from a compiled-view buffer; reads the sibling editorial-log.edn
and renders entries in reverse-chronological order (latest first) with
resolution-kind counts in the header."
  (interactive)
  (let* ((annotations-path (arxana-vsatarcs-editorial--essay-annotations-path))
         (log-path (arxana-vsatarcs-editorial--log-path annotations-path))
         (log-data (arxana-vsatarcs-editorial--read-log log-path))
         (essay-id (or (and log-data (plist-get log-data :essay-id))
                       "<unknown>"))
         (entries (arxana-vsatarcs-editorial--existing-entries log-data))
         (essay-label (file-name-nondirectory
                       (directory-file-name
                        (file-name-directory annotations-path))))
         (buf (get-buffer-create
               (format arxana-vsatarcs-editorial-history-buffer-format
                       essay-label))))
    (unless entries
      (user-error "No editorial-log.edn entries yet at %s" log-path))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "Editorial History\n"
                            'face 'arxana-vsatarcs-editorial-history-header-face))
        (insert (format "Essay: %s\n" essay-id))
        (insert (format "Log:   %s\n" (abbreviate-file-name log-path)))
        (insert (format "Total: %d entr%s\n"
                        (length entries)
                        (if (= 1 (length entries)) "y" "ies")))
        (let ((counts (make-hash-table :test 'equal)))
          (dolist (e entries)
            (let ((k (plist-get e :resolution)))
              (puthash k (1+ (gethash k counts 0)) counts)))
          (insert "By resolution: ")
          (let (parts)
            (maphash (lambda (k v) (push (format "%s=%d" k v) parts)) counts)
            (insert (mapconcat #'identity (nreverse parts) "  "))
            (insert "\n\n")))
        (dolist (e (reverse entries))
          (let* ((log-id (plist-get e :id))
                 (ts     (plist-get e :timestamp))
                 (res    (plist-get e :resolution))
                 (aid    (plist-get e :annotation-id))
                 (rat    (plist-get e :rationale))
                 (edit   (plist-get e :edit))
                 (auth   (plist-get e :author)))
            (insert (propertize
                     (format "── %s   %s   [%s]   author=%s ───────────\n"
                             log-id (or ts "?") res (or auth :operator))
                     'face 'arxana-vsatarcs-editorial-history-entry-face))
            (insert (format "   annotation: %s\n" aid))
            (when (and rat (not (string-empty-p rat)))
              (insert "   rationale:  ")
              (let ((start (point)))
                (insert (format "\"%s\"\n" rat))
                (add-text-properties
                 start (point)
                 '(face arxana-vsatarcs-editorial-history-rationale-face))))
            (when edit
              (insert (format "   edit:       %S\n" edit)))
            (insert "\n")))
        (goto-char (point-min)))
      (special-mode))
    (display-buffer buf)))

(provide 'arxana-vsatarcs-editorial)
;;; arxana-vsatarcs-editorial.el ends here
