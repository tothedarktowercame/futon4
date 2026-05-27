;;; arxana-vsatarcs-observation.el --- R2 observation channels for VSATARCS -*- lexical-binding: t; -*-

;;; Commentary:
;; Observation channel schema for the VSATARCS reader surface — R2 of
;; the standard AIF completeness contract
;; (`futon4/docs/vsatarcs-alignment-completeness.md').
;;
;; The schema mirrors the shape of `futon2.aif.observation' on the WM
;; side (claude-2): a `defconst' enumerating channel keywords, one pure
;; per-channel function returning a value in [0, 1], an `observe' entry
;; that builds the channel-map from inputs, and a `sense-to-vector'
;; projection in declared channel order.  The CONTENT differs: where
;; the WM observes stack-fitness, VSATARCS observes properties of its
;; essay corpus + canonical projection source.
;;
;; Channels (v0.3.0, 2026-05-18):
;;
;;   :story-coverage              — fraction of `stack-annotations.edn'
;;                                  `:sections[]' whose `:ref' matches
;;                                  a story file present in the
;;                                  configured story directories
;;   :lift-freshness              — exp(-mean-delta-days / scale) of
;;                                  the gap between recorded
;;                                  `:source-mtime' and filesystem
;;                                  mtime; 1.0 = perfectly fresh
;;   :annotation-overlay-presence — fraction of stories with a sibling
;;                                  `<name>.aif.edn'
;;   :scene-density               — mean scenes per story, divided by
;;                                  `arxana-vsatarcs-observation-scene-density-max'
;;                                  and clipped to [0, 1]
;;   :link-density                — mean markdown links per scene,
;;                                  divided by
;;                                  `arxana-vsatarcs-observation-link-density-max'
;;                                  and clipped to [0, 1]
;;
;; Contract: contributes to R2 (observation channel schema).  Cross-maps
;; to F1 (explicit fitness state) and F9 (feed-readable annotation
;; graph) at stack scope.

;;; Code:

(require 'cl-lib)
(require 'arxana-browser-vsatarcs)
(require 'arxana-browser-rewrites) ; shared EDN reader

(defgroup arxana-vsatarcs-observation nil
  "Observation channel schema for the VSATARCS reader surface (R2)."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-observation-stack-annotations-path
  (expand-file-name "~/code/futon5a/holes/stack-annotations.edn")
  "Canonical projection source for the observation channels.
Same default as `arxana-vsatarcs-belief-stack-annotations-path' but
kept separate so the observer can target a different source for tests
or for shadow runs."
  :type 'file
  :group 'arxana-vsatarcs-observation)

(defcustom arxana-vsatarcs-observation-freshness-scale-days 30.0
  "Scale parameter for `:lift-freshness' channel.
Freshness is `exp(-mean-delta-days / scale)'.  Default 30 days: a
one-month-old lift yields ~0.37; a one-week-old lift yields ~0.79."
  :type 'number
  :group 'arxana-vsatarcs-observation)

(defcustom arxana-vsatarcs-observation-scene-density-max 20.0
  "Normalisation ceiling for `:scene-density' channel.
Mean scenes per story above this value clip to 1.0."
  :type 'number
  :group 'arxana-vsatarcs-observation)

(defcustom arxana-vsatarcs-observation-link-density-max 5.0
  "Normalisation ceiling for `:link-density' channel.
Mean links per scene above this value clip to 1.0."
  :type 'number
  :group 'arxana-vsatarcs-observation)

(defconst arxana-vsatarcs-observation-channels
  '(:story-coverage
    :lift-freshness
    :annotation-overlay-presence
    :scene-density
    :link-density)
  "Ordered list of VSATARCS observation channel keywords.
Schema is stable: any change here is an R2-shape change, requires a
contract-doc bump and a new closure annotation.")

;; ---------------------------------------------------------------------
;; Internal helpers
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-observation--read-sections (&optional path)
  "Return `:sections[]' from PATH or
`arxana-vsatarcs-observation-stack-annotations-path' as a list.
Returns nil if the file is unreadable.  Uses the shared EDN reader
which tolerates `#inst' and `#{}'.  EDN vectors come back as elisp
vectors; this helper coerces to a list for `dolist'-friendly use."
  (let ((source (or path arxana-vsatarcs-observation-stack-annotations-path)))
    (when (file-readable-p source)
      (let* ((data (arxana-browser-rewrites--read-edn-file source))
             (sections (plist-get data :sections)))
        (cond
         ((null sections) nil)
         ((vectorp sections) (append sections nil))
         (t sections))))))

(defun arxana-vsatarcs-observation--story-paths ()
  "Return list of `.md' story file paths from configured story directories.
Filters out `*.aif.md' overlays per the VSATARCS convention."
  (let (paths)
    (dolist (directory arxana-vsatarcs-story-directories)
      (let ((expanded (expand-file-name directory)))
        (when (file-directory-p expanded)
          (setq paths
                (append paths
                        (cl-remove-if (lambda (p) (string-suffix-p ".aif.md" p))
                                      (directory-files expanded t "\\.md\\'")))))))
    (delete-dups (sort paths #'string<))))

(defun arxana-vsatarcs-observation--story-basename-set ()
  "Return a hash-set of story basenames present on disk (with `.md' suffix)."
  (let ((set (make-hash-table :test 'equal)))
    (dolist (path (arxana-vsatarcs-observation--story-paths))
      (puthash (file-name-nondirectory path) t set))
    set))

(defun arxana-vsatarcs-observation--ref-basename (ref)
  "Return the basename of a section's `:ref' field, or nil."
  (when (and ref (stringp ref))
    (file-name-nondirectory ref)))

(defun arxana-vsatarcs-observation--clip (x)
  "Clip X to [0, 1]."
  (max 0.0 (min 1.0 (float x))))

;; ---------------------------------------------------------------------
;; Per-channel pure functions (each returns a value in [0, 1])
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-observation-channel-story-coverage (sections present-set)
  "Compute `:story-coverage' from SECTIONS and PRESENT-SET.
SECTIONS is the `:sections[]' list from `stack-annotations.edn'.
PRESENT-SET is a hash-set of story basenames on disk.  Returns the
fraction of sections whose `:ref' basename is present (in [0, 1]).
Returns 0.0 when there are no sections."
  (if (null sections) 0.0
    (let ((n 0) (k 0))
      (dolist (s sections)
        (let ((ref (arxana-vsatarcs-observation--ref-basename
                    (plist-get s :ref))))
          (when ref
            (cl-incf n)
            (when (gethash ref present-set) (cl-incf k)))))
      (if (zerop n) 0.0
        (arxana-vsatarcs-observation--clip (/ (float k) (float n)))))))

(defun arxana-vsatarcs-observation--mtime-to-days-ago (mtime now)
  "Return number of days from MTIME to NOW.  MTIME and NOW are floats."
  (/ (- now mtime) 86400.0))

(defun arxana-vsatarcs-observation-channel-lift-freshness (sections present-set
                                                                    &optional now)
  "Compute `:lift-freshness' from SECTIONS and PRESENT-SET.
For each section whose `:ref' is on disk, compares the on-disk
filesystem mtime to the recorded `:source-mtime' on the section.
Returns `exp(-mean-delta-days / scale)' in [0, 1].  Returns 1.0 when
no comparable section is found (vacuously fresh).  NOW defaults to
`float-time'.

Source-mtime values that aren't parseable by `parse-iso8601-time-string'
or `date-to-time' (or that are nil) are skipped — they contribute
no signal but don't penalise."
  (let* ((scale (or arxana-vsatarcs-observation-freshness-scale-days 30.0))
         (now (or now (float-time)))
         (deltas nil))
    (dolist (s sections)
      (let* ((ref-base (arxana-vsatarcs-observation--ref-basename
                        (plist-get s :ref)))
             (source-mtime (plist-get s :source-mtime))
             (on-disk-path (and ref-base (gethash ref-base present-set)
                                (cl-some (lambda (dir)
                                           (let ((p (expand-file-name
                                                     ref-base (expand-file-name dir))))
                                             (and (file-readable-p p) p)))
                                         arxana-vsatarcs-story-directories))))
        (when (and on-disk-path source-mtime (stringp source-mtime))
          (let* ((recorded (ignore-errors (float-time (date-to-time source-mtime))))
                 (actual (ignore-errors
                           (float-time (nth 5 (file-attributes on-disk-path))))))
            (when (and recorded actual)
              (push (abs (arxana-vsatarcs-observation--mtime-to-days-ago
                          recorded actual))
                    deltas))))))
    (if (null deltas) 1.0
      (let* ((mean-delta (/ (apply #'+ deltas) (float (length deltas))))
             (freshness (exp (- (/ mean-delta scale)))))
        (arxana-vsatarcs-observation--clip freshness)))))

(defun arxana-vsatarcs-observation-channel-annotation-overlay-presence ()
  "Compute `:annotation-overlay-presence' from configured story directories.
Returns the fraction of story `.md' files that have a sibling
`<name>.aif.edn' file.  Returns 0.0 when no stories are found."
  (let ((paths (arxana-vsatarcs-observation--story-paths)))
    (if (null paths) 0.0
      (let* ((with-overlay
              (cl-count-if
               (lambda (p)
                 (let* ((dir (file-name-directory p))
                        (base (file-name-base p))
                        (overlay (expand-file-name (concat base ".aif.edn") dir)))
                   (file-readable-p overlay)))
               paths)))
        (arxana-vsatarcs-observation--clip (/ (float with-overlay)
                                              (float (length paths))))))))

(defun arxana-vsatarcs-observation--parse-story-or-nil (path)
  "Return parsed VSATARCS story plist for PATH, or nil on error."
  (condition-case nil
      (arxana-vsatarcs-parse-file path)
    (error nil)))

(defun arxana-vsatarcs-observation-channel-scene-density (&optional stories)
  "Compute `:scene-density' from STORIES (or auto-parse from disk).
Mean scenes per story divided by
`arxana-vsatarcs-observation-scene-density-max', clipped to [0, 1].
Returns 0.0 when no stories are found."
  (let* ((stories
          (or stories
              (delq nil (mapcar #'arxana-vsatarcs-observation--parse-story-or-nil
                                (arxana-vsatarcs-observation--story-paths)))))
         (n (length stories)))
    (if (zerop n) 0.0
      (let* ((total-scenes
              (apply #'+ (mapcar (lambda (s)
                                   (length (plist-get s :scenes)))
                                 stories)))
             (mean (/ (float total-scenes) (float n))))
        (arxana-vsatarcs-observation--clip
         (/ mean (float arxana-vsatarcs-observation-scene-density-max)))))))

(defun arxana-vsatarcs-observation-channel-link-density (&optional stories)
  "Compute `:link-density' from STORIES (or auto-parse from disk).
Mean markdown links per scene divided by
`arxana-vsatarcs-observation-link-density-max', clipped to [0, 1].
Returns 0.0 when no scenes are found."
  (let* ((stories
          (or stories
              (delq nil (mapcar #'arxana-vsatarcs-observation--parse-story-or-nil
                                (arxana-vsatarcs-observation--story-paths)))))
         (all-scenes
          (apply #'append (mapcar (lambda (s) (plist-get s :scenes)) stories)))
         (n (length all-scenes)))
    (if (zerop n) 0.0
      (let* ((total-links
              (apply #'+ (mapcar (lambda (sc)
                                   (length (plist-get sc :links)))
                                 all-scenes)))
             (mean (/ (float total-links) (float n))))
        (arxana-vsatarcs-observation--clip
         (/ mean (float arxana-vsatarcs-observation-link-density-max)))))))

;; ---------------------------------------------------------------------
;; Public entry points: observe + sense-to-vector
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-observe (&optional path)
  "Return an observation map of the current VSATARCS surface.
PATH overrides `arxana-vsatarcs-observation-stack-annotations-path'.
Channels are computed in `arxana-vsatarcs-observation-channels' order;
each returns a value in [0, 1].  Result shape: a plist of
(channel-keyword value)* pairs."
  (let* ((sections (arxana-vsatarcs-observation--read-sections path))
         (present (arxana-vsatarcs-observation--story-basename-set))
         (stories (delq nil
                        (mapcar #'arxana-vsatarcs-observation--parse-story-or-nil
                                (arxana-vsatarcs-observation--story-paths)))))
    (list :story-coverage
          (arxana-vsatarcs-observation-channel-story-coverage sections present)
          :lift-freshness
          (arxana-vsatarcs-observation-channel-lift-freshness sections present)
          :annotation-overlay-presence
          (arxana-vsatarcs-observation-channel-annotation-overlay-presence)
          :scene-density
          (arxana-vsatarcs-observation-channel-scene-density stories)
          :link-density
          (arxana-vsatarcs-observation-channel-link-density stories))))

(defun arxana-vsatarcs-sense-to-vector (observation)
  "Project OBSERVATION (plist) to a vector in declared channel order."
  (apply #'vector
         (mapcar (lambda (ch) (or (plist-get observation ch) 0.0))
                 arxana-vsatarcs-observation-channels)))

(provide 'arxana-vsatarcs-observation)
;;; arxana-vsatarcs-observation.el ends here
