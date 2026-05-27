;;; arxana-vsatarcs-cluster.el --- Mission-cluster overview for VSATARCS reader chrome -*- lexical-binding: t; -*-

;;; Commentary:
;; Reader-criteria V-COV closure (Q8 of
;; `~/code/futon2/docs/vsatarcs-reader-criteria.md').  Aggregates
;; per-mission status from the futon-stack-as-Hyperreal-business
;; mission cluster (M-war-machine-aif-completion +
;; M-stack-essay-code-alignment + M-stack-morphogenetic-rewrite by
;; default) into a single overview block the reader chrome surfaces
;; alongside the other chrome blocks.  Operator sees `where are we
;; in the cluster' without opening multiple docs.
;;
;; Per-mission extraction:
;;
;;   - **Status: ...** header line (first occurrence; the
;;     mission-level disposition the author last wrote)
;;
;;   - Lifecycle stage: highest-numbered `## N. <STAGE>' heading
;;     where STAGE ∈ {IDENTIFY, MAP, DERIVE, DOCUMENT}.  The mission
;;     scaffold convention is `## 1. IDENTIFY' through `## 4.
;;     DOCUMENT' in order; the highest one with content is the
;;     mission's current stage.
;;
;;   - Checkpoints: every `### Checkpoint N — title' is counted; a
;;     checkpoint is `:complete' when an immediately-following bold
;;     `**Status: COMPLETE...' marker is found, otherwise `:open'.
;;
;; The module reads mission files on every snapshot (no in-memory
;; cache); operator-meaningful changes appear immediately when
;; either side ships a new checkpoint or moves the mission status
;; line.
;;
;; The reader-criteria doc names four cluster members; only three
;; have mission files (`M-war-machine-aif-completion',
;; `M-stack-essay-code-alignment', `M-stack-morphogenetic-rewrite').
;; The fourth — `M-stack-essay' — is the essay-side work this
;; VSATARCs apparatus IS; it does not have its own mission file and
;; is documented in the audit row note rather than parsed as a
;; mission.
;;
;; Contract: contributes to V-COV (operator can see all cluster
;; members' status in one place — the deepest comprehension dividend
;; per claude-2's recommendation).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup arxana-vsatarcs-cluster nil
  "Mission-cluster overview for the VSATARCS reader surface."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-cluster-mission-files
  (mapcar #'expand-file-name
          '("~/code/futon7/holes/M-war-machine-aif-completion.md"
            "~/code/futon7/holes/M-stack-essay-code-alignment.md"
            "~/code/futon7/holes/M-stack-morphogenetic-rewrite.md"))
  "Mission files to include in the cluster overview.
Default: the three named cluster members with mission files.  The
fourth member (`M-stack-essay') has no mission file because the
essay-side work is the VSATARCs apparatus itself; it is documented
in the `:reader-criterion-audit :Q8' field of the alignment .aif.edn
rather than parsed here."
  :type '(repeat file)
  :group 'arxana-vsatarcs-cluster)

(defconst arxana-vsatarcs-cluster-lifecycle-stages
  '(:identify :map :derive :document)
  "Ordered set of lifecycle stages a mission can be in.
Per `~/code/futon5a/holes/mission-lifecycle.md' convention: missions
move IDENTIFY → MAP → DERIVE → DOCUMENT.  The snapshot reports the
highest-numbered stage that has been started.")

(defun arxana-vsatarcs-cluster--basename-no-md (path)
  "Return the file's basename stripped of `.md' suffix."
  (let ((base (file-name-nondirectory (or path ""))))
    (replace-regexp-in-string "\\.md\\'" "" base)))

(defun arxana-vsatarcs-cluster--read-file (path)
  "Return PATH's contents as a string, or nil if unreadable."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun arxana-vsatarcs-cluster--extract-status-line (text)
  "Return the first `**Status: ...**' value in TEXT (excluding the marker), or nil.
The mission scaffold convention places this near the top of the
file; we capture the substring between `**Status:**` (or
`**Status:`) and the next blank line / end-of-string.  The
extracted value may itself contain markdown (links, bold).

The capture group uses `\\(?:.\\|\n\\)+?' to span newlines —
Emacs's `.' does not match `\n' by default."
  (when (and text
             (string-match
              "\\*\\*Status:\\*\\*[[:space:]]*\\(\\(?:.\\|\n\\)+?\\)\\(?:\n\n\\|\\'\\)"
              text))
    (string-trim
     (replace-regexp-in-string
      "\n[[:space:]]*" " " (match-string 1 text)))))

(defun arxana-vsatarcs-cluster--detect-stage (text)
  "Return the highest lifecycle stage marker found in TEXT.
Scans for `## N. STAGE' style headings; returns the bare stage
keyword from `arxana-vsatarcs-cluster-lifecycle-stages' for the
last matching heading (highest section number).  Returns nil when
no recognised stage heading exists."
  (let ((stage nil))
    (when text
      (dolist (s arxana-vsatarcs-cluster-lifecycle-stages)
        (let ((s-name (upcase (substring (symbol-name s) 1))))
          (when (string-match
                 (format "^## [0-9]+\\. %s\\b" (regexp-quote s-name)) text)
            (setq stage s)))))
    stage))

(defun arxana-vsatarcs-cluster--count-checkpoints (text)
  "Return `(:total N :complete K)' for `### Checkpoint' headings in TEXT.
A checkpoint counts as `:complete' when the next non-blank line
after its heading is a `**Status: COMPLETE' marker (case-insensitive
on the COMPLETE token).  Other markers (`IN PROGRESS', `OPEN', or
no marker) keep the checkpoint in the `:total - :complete' open bucket."
  (let ((total 0) (complete 0))
    (when text
      (let ((pos 0))
        (while (string-match
                "^### Checkpoint[[:space:]]+[^\n]*\n"
                text pos)
          (cl-incf total)
          (setq pos (match-end 0))
          ;; Look ahead within the next ~200 chars for a Status: COMPLETE
          ;; marker before the next `### Checkpoint' / `## ' heading.
          (let* ((end (or (string-match "^\\(### Checkpoint\\|## \\)"
                                        text pos)
                          (length text)))
                 (window (substring text pos (min end (length text)))))
            (when (string-match-p
                   "\\*\\*Status:[[:space:]]+COMPLETE\\b" window)
              (cl-incf complete))))))
    (list :total total :complete complete)))

(defun arxana-vsatarcs-cluster--summarise-mission (path)
  "Return an operator-facing plist summary of the mission at PATH.
Output carries `:mission' (basename without `.md'), `:path' (absolute
path read), `:loaded?' (t when readable), `:status-line' (first
`**Status:**' value), `:stage' (highest lifecycle stage detected),
`:checkpoints' (`(:total N :complete K)')."
  (let* ((text (arxana-vsatarcs-cluster--read-file path))
         (loaded? (not (null text))))
    (list :mission (arxana-vsatarcs-cluster--basename-no-md path)
          :path path
          :loaded? loaded?
          :status-line (and text
                            (arxana-vsatarcs-cluster--extract-status-line
                             text))
          :stage (and text
                      (arxana-vsatarcs-cluster--detect-stage text))
          :checkpoints (arxana-vsatarcs-cluster--count-checkpoints text))))

(defun arxana-vsatarcs-cluster-snapshot ()
  "Return the mission-cluster overview snapshot.

The snapshot is a plist:

  (:cluster-loaded?     <t when every configured mission file was readable>
   :missions            (<mission-summary> ...)
   :stage-counts        ((:identify . N) (:map . N) (:derive . N) (:document . N) (nil . N))
   :checkpoints-total   <integer — sum across cluster>
   :checkpoints-complete <integer — sum across cluster>
   :digest-line         <one-line operator-facing summary>)

Missions are returned in `arxana-vsatarcs-cluster-mission-files'
order — operator-meaningful order (typically dependency order: WM
foundational, alignment middle, morphogenetic-rewrite gated)."
  (let* ((summaries (mapcar #'arxana-vsatarcs-cluster--summarise-mission
                            arxana-vsatarcs-cluster-mission-files))
         (loaded? (cl-every (lambda (s) (plist-get s :loaded?)) summaries))
         (stage-counts (arxana-vsatarcs-cluster--stage-counts summaries))
         (cp-total (cl-reduce #'+
                              (mapcar (lambda (s)
                                        (plist-get (plist-get s :checkpoints)
                                                   :total))
                                      summaries)
                              :initial-value 0))
         (cp-complete (cl-reduce #'+
                                 (mapcar (lambda (s)
                                           (plist-get (plist-get s :checkpoints)
                                                      :complete))
                                         summaries)
                                 :initial-value 0)))
    (list :cluster-loaded? loaded?
          :missions summaries
          :stage-counts stage-counts
          :checkpoints-total cp-total
          :checkpoints-complete cp-complete
          :digest-line (arxana-vsatarcs-cluster--digest summaries
                                                        cp-total cp-complete))))

(defun arxana-vsatarcs-cluster--stage-counts (summaries)
  "Return an alist `(stage . count)' over SUMMARIES.
Includes one entry per `arxana-vsatarcs-cluster-lifecycle-stages'
keyword plus a `nil' bucket for missions with no detected stage —
chrome shape is stable across cluster evolution."
  (let ((counts (mapcar (lambda (s) (cons s 0))
                        arxana-vsatarcs-cluster-lifecycle-stages))
        (none 0))
    (dolist (s summaries)
      (let* ((stage (plist-get s :stage))
             (cell (assoc stage counts)))
        (if cell
            (setcdr cell (1+ (cdr cell)))
          (setq none (1+ none)))))
    (append counts (list (cons nil none)))))

(defun arxana-vsatarcs-cluster--digest (summaries cp-total cp-complete)
  "Render a one-line operator-facing digest from SUMMARIES + checkpoint counts."
  (let* ((n (length summaries))
         (loaded (cl-count-if (lambda (s) (plist-get s :loaded?)) summaries))
         (stage-names (mapcar (lambda (s)
                                (let ((st (plist-get s :stage)))
                                  (if st
                                      (substring (symbol-name st) 1)
                                    "no-stage")))
                              summaries)))
    (format "%d/%d missions loaded; stages: %s; checkpoints: %d/%d complete"
            loaded n
            (string-join stage-names " / ")
            cp-complete cp-total)))

(provide 'arxana-vsatarcs-cluster)
;;; arxana-vsatarcs-cluster.el ends here
