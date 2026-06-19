;;; arxana-vsatarcs-lifting-queue.el --- Non-lifted-story queue for VSATARCS reader chrome -*- lexical-binding: t; -*-

;;; Commentary:
;; Surfaces the set of stories at `~/code/futon5a/holes/stories/' that
;; are NOT yet referenced by `stack-annotations.edn :sections[]'.  The
;; queue is operator-meaningful state: each non-lifted story is a
;; candidate `:lift-story' / `:stack-annotations-upsert' action waiting
;; for AIF to score + dispatch (read-half today; write-half lands in
;; M-vsatarcs-writer as a v0.6.x action-class candidate per claude-2's
;; §1 source-list).
;;
;; **Why a queue and not bulk-lift:** the cheap one-off would be to
;; add stub `:sections[]' entries for every non-lifted story.  But
;; that loses the operator-meaningful dispatch signal — the *fact*
;; that a story sits unlifted is itself diagnostic information AIF
;; can score (story-age vs lift-priority vs mission-relevance).
;; Per Joe directive 2026-05-20: the queue should be visible and AIF
;; should tell us to dispatch.  This module ships the visibility half;
;; the dispatch half lands on the writer side.
;;
;; **Proposed-payload synthesis:** for each non-lifted story, the
;; module derives a candidate `:sections[]' entry shape:
;;
;;   - `:proposed-id'   ← `arxana/stack/futon-v1/<kind>/<slug>'
;;     where `<kind>' is inferred from filename pattern
;;     (`leaf-*' → `leaf'; `devmap-*' → `devmap'; otherwise `unknown')
;;     and `<slug>' is the basename suffix.
;;   - `:proposed-name' ← first `# Title' line in the file.
;;   - `:proposed-ref'  ← `futon5a/holes/stories/<basename>'.
;;   - `:mtime'         ← file modification time (for age-based
;;     prioritisation).
;;
;; The writer side consumes this payload directly when proposing a
;; `:lift-story' action; the consent-gate validates against the
;; admissibility predicate "proposed-id not already present in
;; `:sections[]'".  Symmetric to v0.5.22's safety-property family.
;;
;; Contract: contributes engagement-time signal to R10 (the queue's
;; depth + age distribution become observable signals) and lays the
;; read-surface for the future `:stack-annotations-upsert' action class.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'arxana-browser-rewrites) ; shared EDN reader

(defgroup arxana-vsatarcs-lifting-queue nil
  "Non-lifted-story queue surface for the VSATARCS reader."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-lifting-queue-stories-directory
  (expand-file-name "~/code/futon5a/holes/stories/")
  "Directory containing VSATARCS-rendered story files.
Files matching `*.md' (excluding `*.aif.md' companions) are
candidates for lifting; the queue reports which are not yet
referenced by `stack-annotations.edn'."
  :type 'directory
  :group 'arxana-vsatarcs-lifting-queue)

(defcustom arxana-vsatarcs-lifting-queue-stack-annotations-file
  (expand-file-name "~/code/futon5a/holes/stack-annotations.edn")
  "Path to the canonical stack-annotations.edn.
Read on every snapshot call; no in-memory cache."
  :type 'file
  :group 'arxana-vsatarcs-lifting-queue)

(defcustom arxana-vsatarcs-lifting-queue-stack-id "arxana/stack/futon-v1"
  "Stack id prefix used to construct proposed entity ids."
  :type 'string
  :group 'arxana-vsatarcs-lifting-queue)

(defun arxana-vsatarcs-lifting-queue--story-files ()
  "Return absolute paths to all candidate story `*.md' files.
Filters out `*.aif.md' companions (which are sibling annotation files,
not story-prose files) and the README, if present."
  (when (file-directory-p arxana-vsatarcs-lifting-queue-stories-directory)
    (let ((dir arxana-vsatarcs-lifting-queue-stories-directory))
      (cl-remove-if
       (lambda (p)
         (or (string-suffix-p ".aif.md" p)
             (string= "README.md" (file-name-nondirectory p))))
       (directory-files dir t "\\.md\\'")))))

(defun arxana-vsatarcs-lifting-queue--read-stack-annotations ()
  "Return the parsed stack-annotations data, or nil if unreadable."
  (when (file-readable-p
         arxana-vsatarcs-lifting-queue-stack-annotations-file)
    (arxana-browser-rewrites--read-edn-file
     arxana-vsatarcs-lifting-queue-stack-annotations-file)))

(defun arxana-vsatarcs-lifting-queue--lifted-basenames (data)
  "Return the set of story basenames referenced by DATA's `:sections[]'.
Each section entry has `:ref' = path like
`futon5a/holes/stories/leaf-2.md'; the basename is the lifted-story
key the chrome's `section-for-story-basename' uses."
  (let* ((sections (plist-get data (intern ":sections")))
         (refs (when sections
                 (mapcar (lambda (s)
                           (plist-get s (intern ":ref")))
                         (append sections nil)))))
    (delq nil (mapcar (lambda (r)
                        (and (stringp r) (file-name-nondirectory r)))
                      refs))))

(defun arxana-vsatarcs-lifting-queue--infer-kind (basename)
  "Infer the `:kind' keyword for a story BASENAME (a string like `leaf-2.md').
Returns `:leaf' / `:devmap' / `:frame' / `:unknown' based on prefix."
  (cond
   ((string-prefix-p "leaf-" basename) :leaf)
   ((string-prefix-p "devmap-" basename) :devmap)
   ((string-prefix-p "frame-" basename) :frame)
   ((string-prefix-p "war-machine-" basename) :war-machine)
   ((string-prefix-p "strategic-" basename) :strategic)
   (t :unknown)))

(defun arxana-vsatarcs-lifting-queue--basename-slug (basename)
  "Return the `id-slug' portion of BASENAME (drops prefix + `.md').
Examples: `leaf-2.md' → `2'; `leaf-6-4-5.md' → `6-4-5';
`devmap-futon3.md' → `futon3'; `war-machine-lucid-scenes.md' →
`lucid-scenes'."
  (let* ((noext (replace-regexp-in-string "\\.md\\'" "" basename))
         (kind (arxana-vsatarcs-lifting-queue--infer-kind basename))
         (prefix (pcase kind
                   (:leaf "leaf-")
                   (:devmap "devmap-")
                   (:frame "frame-")
                   (:war-machine "war-machine-")
                   (:strategic "strategic-")
                   (_ ""))))
    (if (string-prefix-p prefix noext)
        (substring noext (length prefix))
      noext)))

(defun arxana-vsatarcs-lifting-queue--proposed-id (basename)
  "Construct the proposed entity-id for an unlifted story BASENAME."
  (let ((kind (arxana-vsatarcs-lifting-queue--infer-kind basename))
        (slug (arxana-vsatarcs-lifting-queue--basename-slug basename)))
    (format "%s/%s/%s"
            arxana-vsatarcs-lifting-queue-stack-id
            (substring (symbol-name kind) 1)
            slug)))

(defun arxana-vsatarcs-lifting-queue--proposed-name (path)
  "Read the first `# Title' line from PATH (for proposed `:name' field).
Returns the title text trimmed of `# ' prefix; nil if no title found."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path nil 0 4096)
      (goto-char (point-min))
      (when (re-search-forward "^# +\\(.+\\)$" nil t)
        (string-trim (match-string 1))))))

(defun arxana-vsatarcs-lifting-queue--mtime (path)
  "Return PATH's mtime as ISO-8601 string, or nil."
  (when (file-readable-p path)
    (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                        (nth 5 (file-attributes path)) t)))

(defun arxana-vsatarcs-lifting-queue--summarise-unlifted (path)
  "Build a payload plist for an unlifted story at PATH."
  (let ((basename (file-name-nondirectory path)))
    (list :story-basename basename
          :story-path path
          :kind (arxana-vsatarcs-lifting-queue--infer-kind basename)
          :proposed-id (arxana-vsatarcs-lifting-queue--proposed-id basename)
          :proposed-name (arxana-vsatarcs-lifting-queue--proposed-name path)
          :proposed-ref (concat "futon5a/holes/stories/" basename)
          :mtime (arxana-vsatarcs-lifting-queue--mtime path))))

(defun arxana-vsatarcs-lifting-queue-snapshot ()
  "Return the lifting-queue snapshot.

The snapshot is a plist:

  (:stack-loaded?    <t when stack-annotations.edn was readable>
   :stories-dir      <absolute path of source directory>
   :total-stories    <integer — all candidate stories>
   :lifted-count     <integer — stories already in :sections[]>
   :unlifted-count   <integer — stories awaiting dispatch>
   :unlifted         (<payload-plist> ... sorted by mtime descending)
   :kind-counts      ((<kind> . <count>) ...)
   :digest-line      <one-line operator-facing summary>)

Sort order is mtime-descending so the most recently-authored
unlifted stories surface at the top of the queue."
  (let* ((all (arxana-vsatarcs-lifting-queue--story-files))
         (data (arxana-vsatarcs-lifting-queue--read-stack-annotations))
         (lifted (arxana-vsatarcs-lifting-queue--lifted-basenames data))
         (unlifted-paths (cl-remove-if
                          (lambda (p)
                            (member (file-name-nondirectory p) lifted))
                          all))
         (unlifted-summaries (mapcar
                              #'arxana-vsatarcs-lifting-queue--summarise-unlifted
                              unlifted-paths))
         (sorted (sort (copy-sequence unlifted-summaries)
                       (lambda (a b)
                         (let ((ma (or (plist-get a :mtime) ""))
                               (mb (or (plist-get b :mtime) "")))
                           (string> ma mb)))))
         (kind-counts (arxana-vsatarcs-lifting-queue--kind-counts sorted)))
    (list :stack-loaded? (not (null data))
          :stories-dir arxana-vsatarcs-lifting-queue-stories-directory
          :total-stories (length all)
          :lifted-count (- (length all) (length unlifted-paths))
          :unlifted-count (length unlifted-paths)
          :unlifted sorted
          :kind-counts kind-counts
          :digest-line
          (format "%d/%d lifted; %d in queue (by kind: %s)"
                  (- (length all) (length unlifted-paths))
                  (length all)
                  (length unlifted-paths)
                  (mapconcat
                   (lambda (c)
                     (format "%s=%d"
                             (substring (symbol-name (car c)) 1)
                             (cdr c)))
                   (cl-remove-if (lambda (c) (zerop (cdr c)))
                                 kind-counts)
                   " ")))))

(defun arxana-vsatarcs-lifting-queue--kind-counts (summaries)
  "Return an alist (kind . count) over SUMMARIES."
  (let ((counts (mapcar (lambda (k) (cons k 0))
                        '(:leaf :devmap :frame
                                :war-machine :strategic :unknown))))
    (dolist (s summaries)
      (let* ((k (plist-get s :kind))
             (cell (assoc k counts)))
        (when cell
          (setcdr cell (1+ (cdr cell))))))
    counts))

(provide 'arxana-vsatarcs-lifting-queue)
;;; arxana-vsatarcs-lifting-queue.el ends here
