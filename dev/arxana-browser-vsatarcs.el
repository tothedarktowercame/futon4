;;; arxana-browser-vsatarcs.el --- VSATARCS reader for VSAT-shaped stories -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight Arxana-native reader for VSAT-shaped anthology Markdown.
;; The initial source is file-backed `## Scene: Title | anchor' content,
;; matching futon5a's story drafts.  The parsed shape deliberately mirrors
;; future `vsat-story' / `vsat-scene' entities: story metadata, scene anchors,
;; scene body text, and intra-story links.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'subr-x)
(require 'arxana-ui nil t)
(require 'arxana-vsatarcs-belief)
(require 'arxana-vsatarcs-anticipation)
(require 'arxana-vsatarcs-sorrys)
(require 'arxana-vsatarcs-bilateral)
(require 'arxana-vsatarcs-r-criteria-wm)
(require 'arxana-vsatarcs-wm-decision)
(require 'arxana-vsatarcs-wm-recent)
(require 'arxana-vsatarcs-cluster)
(require 'arxana-vsatarcs-xtdb-clicks)
(require 'arxana-vsatarcs-lifting-queue)
(require 'arxana-vsatarcs-essay-revision-queue)

(defgroup arxana-vsatarcs nil
  "VSATARCS reading support for VSAT-shaped stories."
  :group 'arxana)

(defcustom arxana-vsatarcs-story-directories
  (list (expand-file-name "~/code/futon5a/holes/stories"))
  "Directories scanned for VSAT-shaped Markdown stories."
  :type '(repeat directory)
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-landing-story "leaf-start-here.md"
  "Filename of the landing-page story used by `arxana-vsatarcs-up'."
  :type 'string
  :group 'arxana-vsatarcs)

(defvar arxana-vsatarcs--buffer "*VSATARCS*")

(defvar-local arxana-vsatarcs--story nil)
(defvar-local arxana-vsatarcs--source-file nil)
(defvar-local arxana-vsatarcs--current-anchor nil)

(defun arxana-vsatarcs--slugify (text)
  "Return a conservative link anchor for TEXT."
  (let* ((value (downcase (string-trim (or text ""))))
         (value (replace-regexp-in-string "[^[:alnum:] -]" "" value))
         (value (replace-regexp-in-string "[[:space:]]+" "-" value))
         (value (replace-regexp-in-string "-+" "-" value)))
    (string-trim value "-+" "-+")))

(defun arxana-vsatarcs--scene-heading-p (line)
  "Return parsed scene heading fields from LINE, or nil."
  (when (string-match
         "^##+[[:space:]]+Scene:[[:space:]]+\\(.+?\\)\\(?:[[:space:]]+|[[:space:]]*\\([[:alnum:] -]+\\)\\)?[[:space:]]*$"
         line)
    (let* ((title (string-trim (match-string 1 line)))
           (anchor (string-trim (or (match-string 2 line) ""))))
      (list :title title
            :anchor (if (string-empty-p anchor)
                        (arxana-vsatarcs--slugify title)
                      (arxana-vsatarcs--slugify anchor))))))

(defun arxana-vsatarcs--finalize-scene (scene body-lines index)
  "Return SCENE with BODY-LINES and INDEX, or nil when SCENE is nil."
  (when scene
    (let* ((raw-body (string-trim (string-join (nreverse body-lines) "\n")))
           (opening (string-match-p "(opening scene)" raw-body))
           (body (replace-regexp-in-string
                  "^[[:space:]]*\\*(opening scene)\\*[[:space:]]*\n?" ""
                  raw-body))
           (body (arxana-vsatarcs--normalize-folded-links body)))
      (append scene
              (list :index index
                    :opening opening
                    :body (string-trim body)
                    :links (arxana-vsatarcs--extract-links body))))))

(defun arxana-vsatarcs--normalize-folded-links (text)
  "Normalize common hard-wrapped Markdown link targets in TEXT."
  (replace-regexp-in-string "(\\([^)\n]+-\\)\n\\([^)\n]+\\))"
                            "(\\1\\2)"
                            (or text "")))

(defun arxana-vsatarcs--extract-links (text)
  "Extract Markdown links from TEXT as plists with `:text' and `:target'."
  (let ((normalized (arxana-vsatarcs--normalize-folded-links text))
        links)
    (with-temp-buffer
      (insert normalized)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([^]\n]+\\)\\](\\([^) \t\n]+\\))" nil t)
        (push (list :text (match-string 1)
                    :target (arxana-vsatarcs--slugify (match-string 2)))
              links)))
    (nreverse links)))

(defun arxana-vsatarcs-parse-string (text &optional source-file)
  "Parse VSAT-shaped anthology Markdown TEXT.

Return a plist with `:title', `:source-file', `:metadata', and `:scenes'."
  (let ((lines (split-string (or text "") "\n"))
        title
        metadata-lines
        scenes
        current
        body-lines
        (scene-index 0)
        (before-scenes t))
    (dolist (line lines)
      (let ((heading (arxana-vsatarcs--scene-heading-p line)))
        (cond
         (heading
          (when current
            (push (arxana-vsatarcs--finalize-scene current body-lines scene-index)
                  scenes)
            (setq scene-index (1+ scene-index)))
          (setq before-scenes nil
                current heading
                body-lines nil))
         (current
          (push line body-lines))
         (before-scenes
          (cond
           ((and (not title)
                 (string-match "^#[[:space:]]+\\(.+\\)[[:space:]]*$" line))
            (setq title (string-trim (match-string 1 line))))
           ((not (string-match-p "^---[[:space:]]*$" line))
            (push line metadata-lines)))))))
    (when current
      (push (arxana-vsatarcs--finalize-scene current body-lines scene-index)
            scenes))
    (list :title (or title (and source-file (file-name-base source-file)) "Untitled VSATARCS Story")
          :source-file source-file
          :metadata (string-trim (string-join (nreverse metadata-lines) "\n"))
          :scenes (nreverse (delq nil scenes)))))

(defun arxana-vsatarcs-parse-file (path)
  "Parse VSAT-shaped anthology Markdown at PATH."
  (let ((expanded (expand-file-name path)))
    (with-temp-buffer
      (insert-file-contents expanded)
      (arxana-vsatarcs-parse-string
       (buffer-substring-no-properties (point-min) (point-max))
       expanded))))

(defun arxana-vsatarcs--scenes (story)
  (or (plist-get story :scenes) '()))

(defun arxana-vsatarcs--scene-by-anchor (story anchor)
  (let ((target (arxana-vsatarcs--slugify anchor)))
    (cl-find-if (lambda (scene)
                  (string= (plist-get scene :anchor) target))
                (arxana-vsatarcs--scenes story))))

(defun arxana-vsatarcs--opening-scene (story)
  (or (cl-find-if (lambda (scene) (plist-get scene :opening))
                  (arxana-vsatarcs--scenes story))
      (car (arxana-vsatarcs--scenes story))))

(defun arxana-vsatarcs--current-scene ()
  (or (and arxana-vsatarcs--story
           arxana-vsatarcs--current-anchor
           (arxana-vsatarcs--scene-by-anchor arxana-vsatarcs--story
                                             arxana-vsatarcs--current-anchor))
      (and arxana-vsatarcs--story
           (arxana-vsatarcs--opening-scene arxana-vsatarcs--story))))

(defun arxana-vsatarcs--insert-button (label action &optional help)
  (insert-text-button label
                      'follow-link t
                      'help-echo help
                      'action action))

(defun arxana-vsatarcs--insert-scene-link (story scene &optional label)
  (let ((anchor (plist-get scene :anchor)))
    (arxana-vsatarcs--insert-button
     (or label (plist-get scene :title))
     (lambda (_button)
       (arxana-vsatarcs-render story anchor))
     (format "Go to %s" anchor))))

(defun arxana-vsatarcs--insert-markdown-line (line)
  "Insert LINE, turning simple Markdown links into VSATARCS buttons."
  (let ((start 0))
    (while (string-match "\\[\\([^]\n]+\\)\\](\\([^) \t\n]+\\))" line start)
      (insert (substring line start (match-beginning 0)))
      (let ((label (match-string 1 line))
            (target (arxana-vsatarcs--slugify (match-string 2 line))))
        (arxana-vsatarcs--insert-button
         label
         (lambda (_button)
           (arxana-vsatarcs-goto target))
         (format "Go to %s" target)))
      (setq start (match-end 0)))
    (insert (substring line start))
    (insert "\n")))

(defun arxana-vsatarcs--insert-markdown (text)
  "Insert story TEXT with intra-story links buttonized."
  (dolist (line (split-string (or text "") "\n"))
    (arxana-vsatarcs--insert-markdown-line line)))

(defun arxana-vsatarcs--scene-at-index (story index)
  (nth index (arxana-vsatarcs--scenes story)))

(defun arxana-vsatarcs--remember-return-target (return-buffer return-config)
  "Store RETURN-BUFFER and RETURN-CONFIG for left-at-start navigation."
  (when (and (boundp 'arxana-ui-return-buffer)
             (buffer-live-p return-buffer))
    (setq-local arxana-ui-return-buffer return-buffer))
  (when (and (boundp 'arxana-ui-return-window-config)
             (window-configuration-p return-config))
    (setq-local arxana-ui-return-window-config return-config)))

(defun arxana-vsatarcs-left-or-return ()
  "Move left; at point-min, return via the shared Arxana UI target."
  (interactive)
  (if (> (point) (point-min))
      (backward-char)
    (if (fboundp 'arxana-ui-left-or-return)
        (arxana-ui-left-or-return)
      (message "No return target available"))))

(defun arxana-vsatarcs-render (story &optional anchor return-buffer return-config)
  "Render STORY at ANCHOR in the VSATARCS reader buffer.
RETURN-BUFFER and RETURN-CONFIG, when non-nil, are used by
`arxana-vsatarcs-left-or-return' at point-min."
  (let* ((target-buffer (get-buffer-create arxana-vsatarcs--buffer))
         (existing-return-buffer
          (with-current-buffer target-buffer
            (and (boundp 'arxana-ui-return-buffer)
                 arxana-ui-return-buffer)))
         (existing-return-config
          (with-current-buffer target-buffer
            (and (boundp 'arxana-ui-return-window-config)
                 arxana-ui-return-window-config)))
         (scene (or (and anchor (arxana-vsatarcs--scene-by-anchor story anchor))
                    (arxana-vsatarcs--opening-scene story)))
         (scenes (arxana-vsatarcs--scenes story))
         (index (or (plist-get scene :index) 0))
         (previous (arxana-vsatarcs--scene-at-index story (1- index)))
         (next (arxana-vsatarcs--scene-at-index story (1+ index))))
    (unless scene
      (user-error "Story has no scenes"))
    (with-current-buffer target-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (arxana-vsatarcs-mode)
        (setq arxana-vsatarcs--story story
              arxana-vsatarcs--source-file (plist-get story :source-file)
              arxana-vsatarcs--current-anchor (plist-get scene :anchor))
        (arxana-vsatarcs--remember-return-target
         (or return-buffer existing-return-buffer)
         (or return-config existing-return-config))
        (when (fboundp 'arxana-ui-mark-managed)
          (arxana-ui-mark-managed "VSATARCS"))
        (insert (plist-get story :title) "\n")
        (insert (make-string (length (plist-get story :title)) ?=) "\n\n")
        (insert (format "Scene %d/%d: %s%s\n"
                        (1+ index)
                        (length scenes)
                        (plist-get scene :title)
                        (if (plist-get scene :opening) " (opening)" "")))
        (when arxana-vsatarcs--source-file
          (insert (format "Source: %s\n" arxana-vsatarcs--source-file)))
        (insert "\n")
        (when previous
          (arxana-vsatarcs--insert-scene-link story previous "< previous"))
        (when (and previous next)
          (insert "   "))
        (when next
          (arxana-vsatarcs--insert-scene-link story next "next >"))
        (insert "\n\n")
        (insert "Scenes: ")
        (cl-loop for s in scenes
                 for i from 0
                 do (progn
                      (when (> i 0) (insert " · "))
                      (if (eq s scene)
                          (insert (propertize (plist-get s :title) 'face 'bold))
                        (arxana-vsatarcs--insert-scene-link story s))))
        (insert "\n\n")
        (arxana-vsatarcs--insert-belief-snapshot)
        (arxana-vsatarcs--insert-anticipation-snapshot)
        (arxana-vsatarcs--insert-sorrys-snapshot)
        (arxana-vsatarcs--insert-bilateral-snapshot)
        (arxana-vsatarcs--insert-wm-r-criteria-snapshot)
        (arxana-vsatarcs--insert-wm-decision-snapshot)
        (arxana-vsatarcs--insert-wm-recent-snapshot)
        (arxana-vsatarcs--insert-cluster-snapshot)
        (arxana-vsatarcs--insert-xtdb-clicks-snapshot)
        (arxana-vsatarcs--insert-lifting-queue-snapshot)
        (arxana-vsatarcs--insert-essay-revision-queue-snapshot)
        (insert (make-string 72 ?-) "\n\n")
        (arxana-vsatarcs--insert-markdown (plist-get scene :body))
        (goto-char (point-min)))
      (display-buffer (current-buffer)))))

(defun arxana-vsatarcs--insert-belief-snapshot ()
  "Insert the global VSATARCS belief snapshot block.
The block surfaces R1 (per the alignment contract
`futon4/docs/vsatarcs-alignment-completeness.md') in the reader
chrome: every tracked entity is listed with its most-likely status
and entropy (in nats).  When no entity is tracked yet, a brief
explanatory line is inserted instead.  Story-scoped filtering
(showing only entities the current story references) is a v0.3
move that couples to R2."
  (let ((snapshot (arxana-vsatarcs-belief-snapshot)))
    (insert "Belief snapshot (global; entity → status, entropy in nats)\n")
    (if (null snapshot)
        (insert "  (no entities tracked yet — ingest M-INC events via "
                "`arxana-vsatarcs-belief-ingest-events')\n\n")
      (dolist (row snapshot)
        (insert (format "  %s → %s   (H = %.3f)\n"
                        (car row) (or (nth 1 row) "?") (nth 2 row))))
      (insert "\n"))))

(defun arxana-vsatarcs--insert-anticipation-snapshot ()
  "Insert the anticipation snapshot block (reader-criterion Q3).
Reads `~/code/calendar/events.edn' on every call via
`arxana-vsatarcs-anticipation-snapshot' and lists each in-horizon
event with days-until, firing prior, mission anchor, and per-event
time-pressure.  When no events.edn exists or no events fall in the
horizon, an explanatory line is inserted instead."
  (let* ((snap (arxana-vsatarcs-anticipation-snapshot))
         (events (plist-get snap :events))
         (horizon (plist-get snap :horizon-days))
         (tp (plist-get snap :horizon-time-pressure)))
    (insert (format "Anticipation snapshot (horizon %dd; aggregate tp = %.2f)\n"
                    horizon tp))
    (cond
     ((not (plist-get snap :events-loaded?))
      (insert "  (events file not readable: "
              arxana-vsatarcs-anticipation-events-file ")\n\n"))
     ((null events)
      (insert "  (no events in horizon)\n\n"))
     (t
      (dolist (e events)
        (insert (format "  %s  in %.1fd  p=%.2f  tp=%.2f%s\n"
                        (or (plist-get e :id) "?")
                        (or (plist-get e :days-until) 0.0)
                        (or (plist-get e :p-fires) 0.0)
                        (or (plist-get e :time-pressure) 0.0)
                        (if-let ((m (plist-get e :mission)))
                            (format "  %s" m) ""))))
      (insert "\n")))))

(defun arxana-vsatarcs--render-id (id)
  "Stringify a `:mu-post' entity ID for chrome output."
  (cond ((null id) "")
        ((stringp id) id)
        ((symbolp id) (symbol-name id))
        (t (format "%s" id))))

(defun arxana-vsatarcs--insert-xtdb-clicks-snapshot ()
  "Insert the XTDB-clicks snapshot block (R10 engagement-time surface).
Queries futon1a XTDB via `arxana-vsatarcs-xtdb-clicks-snapshot' for the
configured stream types (watcher-event + invoke-job by default) and
renders per-stream digest + the last N records.  Defensive across
server-down / type-slow / stream-empty cases — chrome layout is stable
even when XTDB is unreachable."
  (let* ((snap (arxana-vsatarcs-xtdb-clicks-snapshot))
         (streams (plist-get snap :streams)))
    (insert (format "XTDB clicks (%s)\n" (plist-get snap :digest-line)))
    (if (null streams)
        (insert "  (no streams configured)\n\n")
      (dolist (s streams)
        (insert
         (format "  stream %s (%s):\n"
                 (let ((k (plist-get s :stream-subclass)))
                   (if (keywordp k)
                       (substring (symbol-name k) 1)
                     (format "%s" k)))
                 (if (plist-get s :stream-loaded?)
                     (format "%d records returned"
                             (length (or (plist-get s :records) nil)))
                   "unavailable — futon1a server unreachable or type slow")))
        (let ((records (plist-get s :records)))
          (when records
            (dolist (r records)
              (let* ((ts (plist-get r :ts))
                     (short-ts (cond
                                ((null ts) "?")
                                ((numberp ts)
                                 (format-time-string
                                  "%H:%M:%S"
                                  (seconds-to-time (/ ts 1000.0)) t))
                                (t (format "%s" ts)))))
                (insert (format "    %s  source=%-12s repo=%-12s cycle=%s\n"
                                short-ts
                                (or (plist-get r :source) "?")
                                (or (plist-get r :repo) "?")
                                (or (plist-get r :cycle) "?"))))))))
      (insert "\n"))))

(defun arxana-vsatarcs--insert-essay-revision-queue-snapshot ()
  "Insert the essay-revision-queue block (corpus-niche dispatch surface).
Per Joe directive 2026-05-20 (revision-as-niche-creation framing): each
essay in `~/code/futon7a/' is an entity in the corpus-niche; cross-essay
coherence is its free energy; revision is action.  Surfaces top N
candidates by G-proxy (combining staleness, cross-link-density, and
named-stale-pattern hits) for operator inspection + future
`:essay-revise' action-class dispatch."
  (let* ((snap (arxana-vsatarcs-essay-revision-queue-snapshot))
         (essays (plist-get snap :essays))
         (n-show 5))
    (insert (format "Essay revision queue (%s)\n"
                    (plist-get snap :digest-line)))
    (cond
     ((not (plist-get snap :corpus-loaded?))
      (insert "  (corpus directory not readable)\n\n"))
     ((null essays)
      (insert "  (corpus empty)\n\n"))
     (t
      (let ((shown (cl-subseq essays 0 (min n-show (length essays)))))
        (dolist (e shown)
          (insert (format "  G=%-8.3f  age=%4.0fd  xlink=%-2d  stale=%-2d  %s%s\n"
                          (or (plist-get e :G-proxy) 0.0)
                          (or (plist-get e :days-since-mtime) 0.0)
                          (or (plist-get e :cross-link-density) 0)
                          (or (plist-get e :stale-pattern-hits) 0)
                          (or (plist-get e :essay-basename) "?")
                          (if (plist-get e :has-pkd-epigraph?) "  [PKD]" ""))))
        (when (> (length essays) n-show)
          (insert (format "  ... %d more queued\n"
                          (- (length essays) n-show))))
        (insert "\n"))))))

(defun arxana-vsatarcs--insert-lifting-queue-snapshot ()
  "Insert the lifting-queue block (non-lifted-story dispatch surface).
Per Joe directive 2026-05-20: non-lifted stories become a queue AIF can
dispatch — read-half here; write-half lands as a v0.6.x action class
in M-vsatarcs-writer.  Surfaces top N queue entries (mtime-newest first)
with their proposed `:sections[]' payload for operator inspection."
  (let* ((snap (arxana-vsatarcs-lifting-queue-snapshot))
         (queue (plist-get snap :unlifted))
         (n-show 5))
    (insert (format "Lifting queue (%s)\n" (plist-get snap :digest-line)))
    (cond
     ((not (plist-get snap :stack-loaded?))
      (insert "  (stack-annotations.edn not readable)\n\n"))
     ((null queue)
      (insert "  (queue empty — every story is lifted)\n\n"))
     (t
      (let ((shown (cl-subseq queue 0 (min n-show (length queue)))))
        (dolist (q shown)
          (insert (format "  [%-12s] %s  →  %s\n"
                          (let ((k (plist-get q :kind)))
                            (if (keywordp k)
                                (substring (symbol-name k) 1) "?"))
                          (or (plist-get q :story-basename) "?")
                          (or (plist-get q :proposed-id) "?"))))
        (when (> (length queue) n-show)
          (insert (format "  ... %d more queued\n"
                          (- (length queue) n-show))))
        (insert "\n"))))))

(defun arxana-vsatarcs--insert-cluster-snapshot ()
  "Insert the mission-cluster overview block (reader-criterion Q8).
Surfaces per-mission lifecycle stage and checkpoint progress for the
futon-stack-as-Hyperreal-business cluster.  The block is compact —
the operator sees `where are we in the cluster' without opening
multiple docs.  Aggregate digest line summarises stage distribution
and total checkpoint completion."
  (let* ((snap (arxana-vsatarcs-cluster-snapshot))
         (missions (plist-get snap :missions)))
    (insert (format "Mission cluster (%s)\n"
                    (plist-get snap :digest-line)))
    (if (null missions)
        (insert "  (no cluster missions configured)\n\n")
      (dolist (m missions)
        (let* ((cps (plist-get m :checkpoints))
               (total (plist-get cps :total))
               (done (plist-get cps :complete)))
          (insert (format "  %-40s  %-10s  %d/%d cps%s\n"
                          (or (plist-get m :mission) "?")
                          (if-let ((s (plist-get m :stage)))
                              (substring (symbol-name s) 1)
                            "(no-stage)")
                          done total
                          (if (plist-get m :loaded?) "" "  [unreadable]")))))
      (insert "\n"))))

(defun arxana-vsatarcs--insert-wm-recent-snapshot ()
  "Insert the recent-trace + belief-drift block (reader-criteria Q5+Q6).
Reads the last N records from today's WM trace via
`arxana-vsatarcs-wm-recent-snapshot' and renders two subsections:
recent-trace (per-record digest of timestamp + mode + decision +
G-total + time-pressure + µ-shift) and top-K-moved entities (drift
trajectory across the window)."
  (let* ((snap (arxana-vsatarcs-wm-recent-snapshot))
         (recent (plist-get snap :recent))
         (moved (plist-get snap :top-k-moved)))
    (insert (format "WM recent trace (%s · %s)\n"
                    (plist-get snap :trace-date)
                    (plist-get snap :digest-line)))
    (cond
     ((not (plist-get snap :trace-loaded?))
      (insert "  (trace file not readable)\n\n"))
     ((null recent)
      (insert "  (no records in trace)\n\n"))
     (t
      (dolist (r recent)
        (arxana-vsatarcs--insert-recent-record r))
      (insert (format "  drift across window (top-%d-most-moved):\n"
                      arxana-vsatarcs-wm-recent-top-k-moved))
      (if (null moved)
          (insert "    (window too short or quiescent)\n")
        (dolist (m moved)
          (insert (format "    %s  max-abs-diff=%.4f\n"
                          (arxana-vsatarcs--render-id
                           (plist-get m :id))
                          (or (plist-get m :max-abs-diff) 0.0)))))
      (insert "\n")))))

(defun arxana-vsatarcs--insert-recent-record (r)
  "Insert one recent-trace record line into the current buffer."
  (let* ((ts (plist-get r :timestamp))
         (short-ts (if (and ts (>= (length ts) 19))
                       (substring ts 11 19)
                     (or ts "?")))
         (tp (plist-get r :time-pressure))
         (tp-str (if tp (format " tp=%.3f" tp) "")))
    (insert (format "  %s  %-12s  %s %s  G=%.3f%s  µ=%d\n"
                    short-ts
                    (or (plist-get r :mode) "?")
                    (or (plist-get r :decision-action) "?")
                    (arxana-vsatarcs--render-id
                     (plist-get r :decision-target))
                    (or (plist-get r :G-total) 0.0)
                    tp-str
                    (or (plist-get r :mu-shift-count) 0)))))

(defun arxana-vsatarcs--insert-wm-decision-snapshot ()
  "Insert the WM-decision snapshot block (reader-criterion Q2).
Reads today's WM trace via `arxana-vsatarcs-wm-decision-snapshot' and
renders the latest record's chosen action + top-K alternatives + the
composition section (time-pressure, horizon-steps, µ-shift count)
that explains why this G-total.  Per claude-2's handoff 2026-05-20
recommendation, the chrome shows decision + close-runners-up
together so operator-comprehension is supported.  When no trace
exists / record is empty / decision field absent, surfaces the
appropriate digest line."
  (let* ((snap (arxana-vsatarcs-wm-decision-snapshot))
         (s (plist-get snap :summary)))
    (insert (format "WM decision (%s · %s)\n"
                    (plist-get snap :trace-date)
                    (plist-get snap :digest-line)))
    (when (plist-get snap :decision-present?)
      (insert (format "  rationale: %s\n"
                      (or (plist-get s :rationale) "(none)")))
      (let ((alts (plist-get s :top-k)))
        (when (and alts (> (length alts) 1))
          (insert "  alternatives:\n")
          (dolist (a (cdr alts))      ; skip rank 1 (the chosen action)
            (insert (format "    rank %s: %s %s  (G=%.3f)\n"
                            (or (plist-get a :rank) "?")
                            (or (plist-get a :action-type) "?")
                            (let ((t- (plist-get a :target)))
                              (cond
                               ((null t-) "")
                               ((symbolp t-) (symbol-name t-))
                               (t (format "%s" t-))))
                            (or (plist-get a :G-total) 0.0))))))
      (insert "  composition:\n")
      (when-let ((tp (plist-get s :chosen-time-pressure)))
        (insert (format "    time-pressure: %.3f\n" tp)))
      (when-let ((h (plist-get s :horizon-steps)))
        (insert (format "    horizon-steps: %d\n" h)))
      (insert (format "    µ-shift: %d entities (R3d global update fired)\n"
                      (or (plist-get s :mu-shift-count) 0)))
      (when-let ((gap (plist-get s :gap-report)))
        (insert (format "    gap-report: %S\n" gap))))
    (insert "\n")))

(defun arxana-vsatarcs--insert-wm-r-criteria-snapshot ()
  "Insert the WM-side R-criteria status row (reader-criterion Q1).
Reads `~/code/futon2/docs/futon-aif-completeness.md' on every call
and renders a compact one-line-per-criterion status strip.  Each row
shows the criterion key, status (`:satisfied' / `:n-a' / `:not-satisfied'
/ `:unknown'), `as of vX.Y' anchor when present, and the human-readable
name.  When the contract file is unreadable, an explanatory line is
inserted instead."
  (let* ((snap (arxana-vsatarcs-r-criteria-wm-snapshot))
         (by-key (plist-get snap :by-key)))
    (insert (format "WM R-criteria (%s)\n"
                    (plist-get snap :summary-line)))
    (if (not (plist-get snap :contract-loaded?))
        (insert "  (WM contract not readable: "
                arxana-vsatarcs-r-criteria-wm-file ")\n\n")
      (dolist (k arxana-vsatarcs-r-criteria-wm-rs)
        (let ((row (plist-get by-key k)))
          (insert (format "  %-4s  %-15s  %-7s  %s\n"
                          k
                          (or (plist-get row :status) "?")
                          (or (plist-get row :as-of) "-")
                          (or (plist-get row :name) "(missing)")))))
      (insert "\n"))))

(defun arxana-vsatarcs--insert-bilateral-snapshot ()
  "Insert the bilateral-evidence snapshot block (reader-criterion Q7).
Reads the local `.aif.edn' on every call via
`arxana-vsatarcs-bilateral-snapshot' and lists cross-side
correspondence records (vsatarcs ↔ wm pairs grouped by
`:evidence-kind').  When no `.aif.edn' exists or the
`:bilateral-evidence' block is empty, an explanatory line is
inserted instead.  A ★ marker on a row indicates the entry carries
`:protocol-witnesses' (cross-side coordination audit trail captured
in the source)."
  (let* ((snap (arxana-vsatarcs-bilateral-snapshot))
         (entries (plist-get snap :entries))
         (total (plist-get snap :total))
         (kc (plist-get snap :kind-counts))
         (wc (plist-get snap :witness-count)))
    (insert (format "Bilateral evidence (total %d; with protocol-witnesses: %d; by kind: %s)\n"
                    total wc
                    (mapconcat (lambda (c)
                                 (format "%s=%d"
                                         (if (keywordp (car c))
                                             (substring (symbol-name (car c)) 1)
                                           (car c))
                                         (cdr c)))
                               (cl-remove-if (lambda (c) (zerop (cdr c))) kc)
                               " ")))
    (cond
     ((not (plist-get snap :block-loaded?))
      (insert "  (aif file not readable: "
              arxana-vsatarcs-bilateral-aif-file ")\n\n"))
     ((null entries)
      (insert "  (no bilateral-evidence entries)\n\n"))
     (t
      (dolist (e entries)
        (insert (format "  %s%s  [%s]  %s\n"
                        (or (plist-get e :principle) "?")
                        (if (plist-get e :has-protocol-witnesses?) " ★" "")
                        (or (plist-get e :evidence-kind) "?")
                        (or (plist-get e :landed) "?"))))
      (insert "\n")))))

(defun arxana-vsatarcs--insert-sorrys-snapshot ()
  "Insert the sorry-registry snapshot block (reader-criterion Q4).
Reads `~/code/futon2/data/sorrys.edn' on every call via
`arxana-vsatarcs-sorrys-snapshot' and lists the total + per-kind
distribution + per-status distribution.  Full per-sorry titles are
elided to the first 12 entries to keep the chrome compact; operators
can read the full registry in source.

Sorry mining from agent interactions is the work of
M-a-sorry-enterprise (futon5a/holes/missions/M-a-sorry-enterprise.md);
the event vocabulary that would populate future entries automatically
comes from M-interest-network-coupling step (b).  Both are referenced
in the module commentary."
  (let* ((scene (arxana-vsatarcs--current-scene))
         (scene-body (and scene (plist-get scene :body)))
         (snap (if scene-body
                   (arxana-vsatarcs-sorrys-snapshot-for-text scene-body)
                 (arxana-vsatarcs-sorrys-snapshot)))
         (sorrys (plist-get snap :sorrys))
         (total (plist-get snap :total))
         (kc (plist-get snap :kind-counts))
         (sc (plist-get snap :status-counts))
         (scoped? (plist-get snap :scoped?))
         (scope-missions (plist-get snap :scope-missions)))
    (insert (format "Sorry registry (total %d%s; by kind: %s; by status: %s)\n"
                    total
                    (if scoped?
                        (format "; story-scoped to %s"
                                (mapconcat #'identity scope-missions ", "))
                      "")
                    (mapconcat (lambda (c)
                                 (format "%s=%d" (car c) (cdr c)))
                               (cl-remove-if (lambda (c) (zerop (cdr c))) kc)
                               " ")
                    (mapconcat (lambda (c)
                                 (format "%s=%d" (car c) (cdr c)))
                               (cl-remove-if (lambda (c) (zerop (cdr c))) sc)
                               " ")))
    (cond
     ((not (plist-get snap :registry-loaded?))
      (insert "  (registry file not readable: "
              arxana-vsatarcs-sorrys-file ")\n\n"))
     ((null sorrys)
      (insert "  (registry empty)\n\n"))
     (t
      (let ((shown (cl-subseq sorrys 0 (min 12 (length sorrys)))))
        (dolist (s shown)
          (insert (format "  [%s] %s\n"
                          (or (plist-get s :kind) "?")
                          (or (plist-get s :title) "(no title)"))))
        (when (> (length sorrys) (length shown))
          (insert (format "  ... %d more in source\n"
                          (- (length sorrys) (length shown)))))
        (insert "\n"))))))

(defun arxana-vsatarcs--find-story-file (name)
  "Return an absolute path for a VSATARCS story matching NAME, or nil.
NAME may or may not end in \".md\"; story directories are searched in
order for a matching readable file."
  (let ((filename (if (string-suffix-p ".md" name) name (concat name ".md"))))
    (cl-some (lambda (dir)
               (let ((path (expand-file-name filename (expand-file-name dir))))
                 (and (file-readable-p path) path)))
             arxana-vsatarcs-story-directories)))

(defun arxana-vsatarcs-goto (anchor)
  "Navigate VSATARCS to ANCHOR.

ANCHOR is tried first as a scene within the current story; if no such
scene exists it is tried as a cross-story reference, opening the file
\"<anchor>.md\" from `arxana-vsatarcs-story-directories'."
  (interactive
   (list (completing-read "Scene: "
                          (mapcar (lambda (scene) (plist-get scene :anchor))
                                  (arxana-vsatarcs--scenes arxana-vsatarcs--story))
                          nil t)))
  (unless arxana-vsatarcs--story
    (user-error "No VSATARCS story is active"))
  (cond
   ((arxana-vsatarcs--scene-by-anchor arxana-vsatarcs--story anchor)
    (arxana-vsatarcs-render arxana-vsatarcs--story anchor))
   ((arxana-vsatarcs--find-story-file anchor)
    (arxana-vsatarcs-open-file
     (arxana-vsatarcs--find-story-file anchor)
     (and (boundp 'arxana-ui-return-buffer) arxana-ui-return-buffer)
     (and (boundp 'arxana-ui-return-window-config)
          arxana-ui-return-window-config)))
   (t
    (user-error "Unknown VSATARCS target: %s" anchor))))

(defun arxana-vsatarcs-up ()
  "Jump to the VSATARCS landing page (`arxana-vsatarcs-landing-story').
The landing story is expected to live in one of the directories in
`arxana-vsatarcs-story-directories'."
  (interactive)
  (let ((path (arxana-vsatarcs--find-story-file arxana-vsatarcs-landing-story)))
    (unless path
      (user-error "No VSATARCS landing story (%s) found in story directories"
                  arxana-vsatarcs-landing-story))
    (arxana-vsatarcs-open-file
     path
     (and (boundp 'arxana-ui-return-buffer) arxana-ui-return-buffer)
     (and (boundp 'arxana-ui-return-window-config)
          arxana-ui-return-window-config))))

(defun arxana-vsatarcs-next-scene ()
  "Move to the next scene in the current VSATARCS story."
  (interactive)
  (let* ((scene (arxana-vsatarcs--current-scene))
         (next (and scene
                    (arxana-vsatarcs--scene-at-index arxana-vsatarcs--story
                                                     (1+ (plist-get scene :index))))))
    (unless next
      (user-error "No next VSATARCS scene"))
    (arxana-vsatarcs-render arxana-vsatarcs--story (plist-get next :anchor))))

(defun arxana-vsatarcs-previous-scene ()
  "Move to the previous scene in the current VSATARCS story."
  (interactive)
  (let* ((scene (arxana-vsatarcs--current-scene))
         (previous (and scene
                        (arxana-vsatarcs--scene-at-index arxana-vsatarcs--story
                                                         (1- (plist-get scene :index))))))
    (unless previous
      (user-error "No previous VSATARCS scene"))
    (arxana-vsatarcs-render arxana-vsatarcs--story (plist-get previous :anchor))))

(defun arxana-vsatarcs-reload ()
  "Reload the current VSATARCS story from disk."
  (interactive)
  (unless arxana-vsatarcs--source-file
    (user-error "Current VSATARCS story has no source file"))
  (let ((anchor arxana-vsatarcs--current-anchor))
    (arxana-vsatarcs-render
     (arxana-vsatarcs-parse-file arxana-vsatarcs--source-file)
     anchor)))

(defun arxana-vsatarcs-open-file (path &optional return-buffer return-config)
  "Open VSAT-shaped anthology Markdown PATH in the VSATARCS reader.
RETURN-BUFFER and RETURN-CONFIG override the current buffer/window
configuration as the left-at-start return target."
  (interactive "fVSATARCS story: ")
  (arxana-vsatarcs-render
   (arxana-vsatarcs-parse-file path)
   nil
   (or return-buffer (current-buffer))
   (or return-config (current-window-configuration))))

(defun arxana-vsatarcs-open-example ()
  "Open the futon5a Stack Geometry anthology example."
  (interactive)
  (let ((path (expand-file-name "~/code/futon5a/holes/stories/leaf-6-4-4.md")))
    (unless (file-readable-p path)
      (user-error "Example story not found: %s" path))
    (arxana-vsatarcs-open-file path)))

(defvar arxana-vsatarcs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'arxana-vsatarcs-next-scene)
    (define-key map (kbd "p") #'arxana-vsatarcs-previous-scene)
    (define-key map (kbd "g") #'arxana-vsatarcs-reload)
    (define-key map (kbd "o") #'arxana-vsatarcs-goto)
    (define-key map (kbd "u") #'arxana-vsatarcs-up)
    (define-key map (kbd "<left>") #'arxana-vsatarcs-left-or-return)
    ;; R1 belief surface — operator-triggerable belief management
    (define-key map (kbd "B") #'arxana-vsatarcs-belief-bootstrap-and-redisplay)
    (define-key map (kbd "R") #'arxana-vsatarcs-belief-reset-and-redisplay)
    (define-key map (kbd "i") #'arxana-vsatarcs-belief-ingest-interactive)
    map)
  "Keymap for `arxana-vsatarcs-mode'.

Reader navigation: `n' / `p' next/previous scene; `g' reload from disk;
`o' jump to scene anchor; `u' to landing story; `<left>' back-or-return.

R1 belief surface: `B' bootstrap-from-canonical-source-and-persist; `R'
reset-in-memory-with-confirm; `i' ingest events from minibuffer.")

(define-derived-mode arxana-vsatarcs-mode special-mode "VSATARCS"
  "Major mode for reading VSAT-shaped stories."
  (setq-local truncate-lines nil))

(defun arxana-browser-vsatarcs--story-paths ()
  "Return readable VSATARCS source paths.
Files matching `*.aif.md' are excluded — they are AIF+ annotation overlays
on stories, not stories themselves.  See `README-vsatarcs.md' for the
`*.md' / `*.aif.md' / `*.aif.edn' naming convention."
  (let (paths)
    (dolist (directory arxana-vsatarcs-story-directories)
      (let ((expanded (expand-file-name directory)))
        (when (file-directory-p expanded)
          (setq paths
                (append paths
                        (directory-files expanded t "\\.md\\'"))))))
    (delete-dups
     (sort (cl-remove-if (lambda (p)
                           (string-suffix-p ".aif.md" p))
                         paths)
           #'string<))))

(defun arxana-browser-vsatarcs-format ()
  "Return tabulated format for VSATARCS story sources."
  [("Story" 36 t)
   ("Scenes" 7 t)
   ("Opening" 26 t)
   ("Path" 0 nil)])

(defun arxana-browser-vsatarcs-row (item)
  "Return a tabulated row for VSATARCS ITEM."
  (vector (or (plist-get item :label) "")
          (format "%s" (or (plist-get item :scene-count) 0))
          (or (plist-get item :opening) "-")
          (or (plist-get item :path) "")))

(defun arxana-browser-vsatarcs-items ()
  "Return browser rows for VSATARCS story sources."
  (let ((paths (arxana-browser-vsatarcs--story-paths)))
    (if paths
        (mapcar
         (lambda (path)
           (condition-case err
               (let* ((story (arxana-vsatarcs-parse-file path))
                      (opening (arxana-vsatarcs--opening-scene story)))
                 (list :type 'vsatarcs-story
                       :label (plist-get story :title)
                       :path path
                       :scene-count (length (arxana-vsatarcs--scenes story))
                       :opening (and opening (plist-get opening :title))))
             (error
              (list :type 'vsatarcs-story
                    :label (file-name-base path)
                    :path path
                    :scene-count 0
                    :opening (format "parse error: %s" (error-message-string err))))))
         paths)
      (list (list :type 'info
                  :label "No VSATARCS stories"
                  :description "Add Markdown stories to arxana-vsatarcs-story-directories.")))))

(defun arxana-browser-vsatarcs-open (item)
  "Open VSATARCS browser ITEM."
  (let ((path (plist-get item :path))
        (return-buffer (current-buffer))
        (return-config (current-window-configuration)))
    (unless (and path (file-readable-p path))
      (user-error "VSATARCS source is missing: %s" path))
    (arxana-vsatarcs-open-file path return-buffer return-config)))

(provide 'arxana-browser-vsatarcs)
;;; arxana-browser-vsatarcs.el ends here
