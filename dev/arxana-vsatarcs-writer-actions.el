;;; arxana-vsatarcs-writer-actions.el --- Writer-action forward-model (R4) -*- lexical-binding: t; -*-

;;; Commentary:
;; R4 forward-model for VSATARCS writer-capability — the action shape,
;; predict-effects dispatch, and admissibility predicates (can-propose?,
;; can-execute?).  Implements the "writer-action" half of M-vsatarcs-writer
;; (cf. `~/code/futon4/holes/missions/M-vsatarcs-writer.md').
;;
;; Action-classes implemented (per D5 ordering, self-documentation family):
;;   :mission-doc-sync         (L3, 2026-05-20) — checkpoint-complete markers on FUTON missions
;;   :aif-edn-revision-entry   (L4, 2026-05-20) — :provenance :revisions log entries on .aif.edn
;;   :story-update             (L4+, 2026-05-20) — append a Scene to a FUTON story leaf
;;                                                  per claude-4 v0.5.22 safety-property generalisation
;;
;; All classes share the (R4 forward-model, R6 consent-gate) shape;
;; specific predict-effects / can-propose? / can-execute? branch on
;; (plist-get action :type).  Per D5 + PSR-A2, the action space is
;; bounded and opt-in.
;;
;; Action shape (plist):
;;   (:type :mission-doc-sync
;;    :target-file "/abs/path/to/M-foo.md"
;;    :target-checkpoint "Checkpoint 4 — feature X"
;;    :proposed-status-marker "**Status: COMPLETE (2026-05-20)**"
;;    :match-type :clean | :scope-creep | :pivot
;;    :source-closure-id "hx:vsatarcs-align:v0-X-Y:..."
;;    :rationale "human-readable string")
;;
;; Match-type resolution (D2 + I7): mission-doc-sync requires :clean for
;; autopen-eligibility; :scope-creep requires :operator source; :pivot
;; triggers abstain without consent-request emission.

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-cluster) ; reuses mission-status parser per S4

(defgroup arxana-vsatarcs-writer-actions nil
  "Writer-action forward-model (R4) for VSATARCS."
  :group 'arxana-vsatarcs)

(defconst arxana-vsatarcs-writer-actions-version 1
  "Schema version for writer-action shape.")

;; ---------------------------------------------------------------------
;; Action constructors
;; ---------------------------------------------------------------------

(cl-defun arxana-vsatarcs-writer-action-make-mission-doc-sync
    (&key target-file target-checkpoint proposed-status-marker
          match-type source-closure-id rationale)
  "Construct a `:mission-doc-sync' writer-action plist.
All keys required.  MATCH-TYPE must be one of :clean, :scope-creep, :pivot."
  (cl-assert (memq match-type '(:clean :scope-creep :pivot)))
  (cl-assert (file-name-absolute-p target-file))
  (cl-assert (stringp target-checkpoint))
  (cl-assert (stringp proposed-status-marker))
  (list :type :mission-doc-sync
        :target-file target-file
        :target-checkpoint target-checkpoint
        :proposed-status-marker proposed-status-marker
        :match-type match-type
        :source-closure-id source-closure-id
        :rationale rationale))

(cl-defun arxana-vsatarcs-writer-action-make-aif-edn-revision-entry
    (&key target-file proposed-rev proposed-on proposed-by proposed-summary
          match-type source-closure-id rationale)
  "Construct a `:aif-edn-revision-entry' writer-action plist.
Targets the `:provenance :revisions' log of a `*.aif.edn' file.
MATCH-TYPE in {:clean :scope-creep :pivot}.  PROPOSED-REV is the
new version string (e.g., \"v0.5.21\").  PROPOSED-SUMMARY is the
multi-line summary text for the entry."
  (cl-assert (memq match-type '(:clean :scope-creep :pivot)))
  (cl-assert (file-name-absolute-p target-file))
  (cl-assert (stringp proposed-rev))
  (cl-assert (stringp proposed-summary))
  (list :type :aif-edn-revision-entry
        :target-file target-file
        :proposed-rev proposed-rev
        :proposed-on (or proposed-on
                         (format-time-string "%Y-%m-%d"))
        :proposed-by (or proposed-by "claude-2")
        :proposed-summary proposed-summary
        :match-type match-type
        :source-closure-id source-closure-id
        :rationale rationale))

(cl-defun arxana-vsatarcs-writer-action-make-story-update
    (&key target-file scene-slug scene-name scene-content
          match-type source-evidence-id rationale)
  "Construct a `:story-update' writer-action plist.
Appends a new `## Scene:' block to a FUTON story leaf (`.md').
MATCH-TYPE in {:clean :scope-creep :pivot}.  SCENE-SLUG is the
unique slug after the `|' separator (the URL-fragment-style
identifier).  SCENE-NAME is the human-readable heading.  Per the
v0.5.22 safety-property generalisation (claude-4): admissibility
predicate is `proposed scene-slug not already present in target
story' — same recursion-safety mechanic as the other classes."
  (cl-assert (memq match-type '(:clean :scope-creep :pivot)))
  (cl-assert (file-name-absolute-p target-file))
  (cl-assert (stringp scene-slug))
  (cl-assert (stringp scene-name))
  (cl-assert (stringp scene-content))
  (list :type :story-update
        :target-file target-file
        :scene-slug scene-slug
        :scene-name scene-name
        :scene-content scene-content
        :match-type match-type
        :source-evidence-id source-evidence-id
        :rationale rationale))

;; ---------------------------------------------------------------------
;; Forward-model: predict-effects
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-writer-actions-predict-effects (action)
  "Predict post-state for ACTION.
Returns a plist describing the post-state.  Dispatches on (plist-get
ACTION :type).  Per S4, mission-doc-sync uses `cluster.el's parser to
project the post-edit state cheaply; aif-edn-revision-entry uses a
text-level :revisions block scan."
  (cl-case (plist-get action :type)
    (:mission-doc-sync
     (arxana-vsatarcs-writer-actions--predict-mission-doc-sync action))
    (:aif-edn-revision-entry
     (arxana-vsatarcs-writer-actions--predict-aif-edn-revision-entry action))
    (:story-update
     (arxana-vsatarcs-writer-actions--predict-story-update action))
    (t (error "Unknown writer-action type: %s" (plist-get action :type)))))

(defun arxana-vsatarcs-writer-actions--predict-story-update (action)
  "Predict the post-state of a :story-update ACTION.
Counts `## Scene:' headings pre-edit; predicts +1 post-edit.
Coarse parser; same shape as the other self-doc classes' predict-effects."
  (let* ((file (plist-get action :target-file))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (pre-count (arxana-vsatarcs-writer-actions--count-scenes text))
         (last-slug (arxana-vsatarcs-writer-actions--last-scene-slug text)))
    (list :pre-state (list :scenes-count pre-count :last-slug last-slug)
          :predicted-post-state (list :scenes-count (1+ pre-count)
                                      :last-slug (plist-get action :scene-slug))
          :predicted-delta (list :scenes-count-delta 1
                                 :new-slug (plist-get action :scene-slug)))))

(defun arxana-vsatarcs-writer-actions--count-scenes (text)
  "Count `## Scene:' headings in TEXT."
  (let ((count 0))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^## Scene:[[:space:]]" nil t)
        (cl-incf count)))
    count))

(defun arxana-vsatarcs-writer-actions--last-scene-slug (text)
  "Return the last scene's slug (after `|') in TEXT, or nil."
  (let ((slug nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward
              "^## Scene:[[:space:]]+[^|]+|[[:space:]]*\\([a-zA-Z0-9_-]+\\)"
              nil t)
        (setq slug (match-string 1))))
    slug))

(defun arxana-vsatarcs-writer-actions--predict-aif-edn-revision-entry (action)
  "Predict the post-state of an :aif-edn-revision-entry ACTION.
Counts :revisions entries in the target .aif.edn pre-edit and predicts
+1 post-edit.  Coarse parser — counts `{:rev ' occurrences (sufficient
for L4's prediction-error == 0 acceptance test)."
  (let* ((file (plist-get action :target-file))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (pre-count (arxana-vsatarcs-writer-actions--count-revisions text))
         (last-rev (arxana-vsatarcs-writer-actions--last-revision text)))
    (list :pre-state (list :revisions-count pre-count :last-rev last-rev)
          :predicted-post-state (list :revisions-count (1+ pre-count)
                                      :last-rev (plist-get action :proposed-rev))
          :predicted-delta (list :revisions-count-delta 1
                                 :new-rev (plist-get action :proposed-rev)))))

(defun arxana-vsatarcs-writer-actions--count-revisions (text)
  "Count `{:rev \"...\"' occurrences in TEXT (revision-log entries)."
  (let ((count 0)
        (case-fold-search nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "{:rev[[:space:]]+\"" nil t)
        (cl-incf count)))
    count))

(defun arxana-vsatarcs-writer-actions--last-revision (text)
  "Return the first (most-recent) :rev value in TEXT, or nil."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (when (re-search-forward "{:rev[[:space:]]+\"\\([^\"]+\\)\"" nil t)
      (match-string 1))))

(defun arxana-vsatarcs-writer-actions--predict-mission-doc-sync (action)
  "Predict the post-state of applying a :mission-doc-sync ACTION.
The post-state is the parsed scaffold state (per cluster.el) AFTER
inserting the proposed-status-marker below the target-checkpoint
heading.  Implementation: pre-parse current file; simulate edit on a
temp string; re-parse; return both pre and predicted-post for
comparison."
  (let* ((file (plist-get action :target-file))
         (checkpoint (plist-get action :target-checkpoint))
         (marker (plist-get action :proposed-status-marker))
         (text (with-temp-buffer
                 (insert-file-contents file)
                 (buffer-string)))
         (pre-status (arxana-vsatarcs-cluster--extract-status-line text))
         (pre-stage (arxana-vsatarcs-cluster--detect-stage text))
         (pre-checkpoints (arxana-vsatarcs-cluster--count-checkpoints text))
         (post-text (arxana-vsatarcs-writer-actions--simulate-edit
                     text checkpoint marker))
         (post-status (arxana-vsatarcs-cluster--extract-status-line post-text))
         (post-stage (arxana-vsatarcs-cluster--detect-stage post-text))
         (post-checkpoints (arxana-vsatarcs-cluster--count-checkpoints
                            post-text)))
    (list :pre-state (list :status pre-status :stage pre-stage
                           :checkpoints pre-checkpoints)
          :predicted-post-state (list :status post-status :stage post-stage
                                      :checkpoints post-checkpoints)
          :predicted-delta (list :checkpoints-complete-delta
                                 (- (plist-get post-checkpoints :complete)
                                    (plist-get pre-checkpoints :complete))))))

(defun arxana-vsatarcs-writer-actions--simulate-edit (text checkpoint marker)
  "Return TEXT modified by inserting MARKER below the CHECKPOINT heading.
This is the pre-execution simulation used by predict-effects; it doesn't
write the file.  Search is line-anchored on the checkpoint heading
substring."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (when (re-search-forward
           (concat "^### " (regexp-quote checkpoint) "[[:space:]]*$")
           nil t)
      ;; Insert marker on the line immediately after the heading.
      (forward-line 1)
      (insert marker "\n"))
    (buffer-string)))

;; ---------------------------------------------------------------------
;; Admissibility: can-propose? + can-execute?
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-writer-actions-can-propose? (action)
  "Return non-nil iff ACTION is admissible as a proposal.
Dispatches on :type.  Checks: target exists; structural preconditions
for the action-class."
  (cl-case (plist-get action :type)
    (:mission-doc-sync
     (arxana-vsatarcs-writer-actions--can-propose-mission-doc-sync action))
    (:aif-edn-revision-entry
     (arxana-vsatarcs-writer-actions--can-propose-aif-edn-revision-entry action))
    (:story-update
     (arxana-vsatarcs-writer-actions--can-propose-story-update action))
    (t nil)))

(defun arxana-vsatarcs-writer-actions--can-propose-story-update (action)
  "Story-update proposal admissibility.
Requires: target-file exists; file has ≥1 `## Scene:' heading (sanity
check it's a story); proposed scene-slug not already in the file —
THIS is the v0.5.22 safety-property predicate that prevents runaway
self-application per claude-4's generalisation."
  (let* ((file (plist-get action :target-file))
         (proposed-slug (plist-get action :scene-slug)))
    (and (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (when (re-search-forward "^## Scene:[[:space:]]" nil t)
             ;; File is a story; check slug uniqueness
             (goto-char (point-min))
             (not (re-search-forward
                   (concat "^## Scene:[[:space:]]+[^|]+|[[:space:]]*"
                           (regexp-quote proposed-slug)
                           "[[:space:]]*$")
                   nil t)))))))

(defun arxana-vsatarcs-writer-actions--can-propose-aif-edn-revision-entry (action)
  "Aif-edn-revision-entry proposal admissibility.
Requires: target-file exists; `:revisions' block found; proposed-rev
not already in the log."
  (let* ((file (plist-get action :target-file))
         (proposed-rev (plist-get action :proposed-rev)))
    (and (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (when (re-search-forward "^[[:space:]]+:revisions[[:space:]]*$" nil t)
             ;; Block found; check :rev uniqueness
             (goto-char (point-min))
             (not (re-search-forward
                   (concat "{:rev[[:space:]]+\""
                           (regexp-quote proposed-rev)
                           "\"")
                   nil t)))))))

(defun arxana-vsatarcs-writer-actions--can-propose-mission-doc-sync (action)
  "Mission-doc-sync proposal admissibility.
Requires: target-file exists; target-checkpoint heading found; checkpoint
not already marked complete."
  (let* ((file (plist-get action :target-file))
         (checkpoint (plist-get action :target-checkpoint)))
    (and (file-exists-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (goto-char (point-min))
           (when (re-search-forward
                  (concat "^### " (regexp-quote checkpoint) "[[:space:]]*$")
                  nil t)
             ;; Check: look ahead ~200 chars for an existing
             ;; **Status: COMPLETE marker before the next heading.
             (let ((heading-end (point))
                   (next-heading (save-excursion
                                   (or (re-search-forward
                                        "^\\(### Checkpoint\\|## \\)"
                                        nil t)
                                       (point-max)))))
               (not (save-excursion
                      (goto-char heading-end)
                      (re-search-forward
                       "\\*\\*Status:[[:space:]]+COMPLETE\\b"
                       next-heading t)))))))))

(defun arxana-vsatarcs-writer-actions-can-execute? (action)
  "Return non-nil iff ACTION is admissible for execution (per I5 + I6).
Stricter than can-propose? — adds reversibility precondition (git-clean
state on target)."
  (cl-case (plist-get action :type)
    (:mission-doc-sync
     (arxana-vsatarcs-writer-actions--can-execute-mission-doc-sync action))
    (:aif-edn-revision-entry
     (arxana-vsatarcs-writer-actions--can-execute-aif-edn-revision-entry action))
    (:story-update
     (arxana-vsatarcs-writer-actions--can-execute-story-update action))
    (t nil)))

(defun arxana-vsatarcs-writer-actions--can-execute-story-update (action)
  "Story-update execution admissibility.
Requires: can-propose?; file writable; git-clean state (I6)."
  (and (arxana-vsatarcs-writer-actions--can-propose-story-update action)
       (let ((file (plist-get action :target-file)))
         (and (file-writable-p file)
              (arxana-vsatarcs-writer-actions--git-clean-p file)))))

(defun arxana-vsatarcs-writer-actions--can-execute-aif-edn-revision-entry (action)
  "Aif-edn-revision-entry execution admissibility.
Requires: can-propose?; target file writable; git-clean state (I6)."
  (and (arxana-vsatarcs-writer-actions--can-propose-aif-edn-revision-entry action)
       (let ((file (plist-get action :target-file)))
         (and (file-writable-p file)
              (arxana-vsatarcs-writer-actions--git-clean-p file)))))

(defun arxana-vsatarcs-writer-actions--can-execute-mission-doc-sync (action)
  "Mission-doc-sync execution admissibility.
Requires: can-propose?; target file is writable; git-tracked with no
uncommitted changes (I6)."
  (and (arxana-vsatarcs-writer-actions--can-propose-mission-doc-sync action)
       (let ((file (plist-get action :target-file)))
         (and (file-writable-p file)
              (arxana-vsatarcs-writer-actions--git-clean-p file)))))

(defun arxana-vsatarcs-writer-actions--git-clean-p (file)
  "Return non-nil iff FILE is git-tracked and has no uncommitted changes.
Per I6.  Uses `git status --porcelain' on the target.  If the directory
isn't a git repo at all, returns nil (defensive: don't write outside
version control)."
  (let* ((dir (file-name-directory (expand-file-name file)))
         (default-directory dir)
         (rel (file-relative-name (expand-file-name file) dir))
         (status (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process "git" nil t nil
                                   "status" "--porcelain" "--" rel)))))
    ;; Empty porcelain output means clean.  Non-zero exit (not a git
    ;; repo) lands in `status' as empty too, so we also check that
    ;; we're inside a worktree.
    (and (= 0 (call-process "git" nil nil nil
                            "rev-parse" "--is-inside-work-tree"))
         (string-empty-p (string-trim status)))))

(provide 'arxana-vsatarcs-writer-actions)
;;; arxana-vsatarcs-writer-actions.el ends here
