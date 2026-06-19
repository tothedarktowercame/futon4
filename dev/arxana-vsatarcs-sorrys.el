;;; arxana-vsatarcs-sorrys.el --- Sorry registry block for VSATARCS reader chrome -*- lexical-binding: t; -*-

;;; Commentary:
;; Reader-criteria V-COV closure (Q4 of
;; `~/code/futon2/docs/vsatarcs-reader-criteria.md').  Reads the
;; hand-curated sorry registry at `~/code/futon2/data/sorrys.edn'
;; (schema v2 with `:kind' field per Joe 2026-05-18) and exposes a
;; snapshot the reader chrome can surface alongside the belief and
;; anticipation snapshots.
;;
;; The shape is operator-facing: each sorry carries its id, title,
;; status, kind, raised-at date, and related-missions list.  The
;; snapshot also exposes per-`:kind' counts (`:meta',
;; `:prototyping-forward', `:technical-debt', `:decision-debt',
;; `:external-dependency') so a reader can see the distribution at a
;; glance.
;;
;; The module does not maintain mutable state — every call re-reads the
;; canonical EDN and recomputes.  Same discipline as the anticipation
;; module: sorrys.edn is the source of truth and any rendered surface is
;; a downstream projection.
;;
;; Story-scoped filtering (showing only sorries whose
;; `:related-missions' overlap with the current story's mission
;; references) is a v0.3 move that couples to V-COM (a reader looking at
;; one story only sees the relevant sorries); the v1 surface is a global
;; list grouped by kind.
;;
;; Cross-reference to upstream mission for SORRY mining: the registry
;; itself is hand-curated, but the mining vocabulary that would populate
;; future entries automatically comes from M-a-sorry-enterprise
;; (`~/code/futon5a/holes/missions/M-a-sorry-enterprise.md', mines
;; per-turn pattern retrieval from agent interactions into Strategic
;; SORRY pressure signals) and the M-INC event vocabulary from
;; M-interest-network-coupling (`~/code/futon4/holes/missions/
;; M-interest-network-coupling.md', defines `state/spawned',
;; `state/addressed', `state/foreclosed', etc.).  When M-INC step (b)
;; commits, sorrys in this registry will gain typed-event provenance
;; and this module's reader-projection will couple to mining output.
;;
;; Contract: contributes to V-COV (coverage — reader can see the
;; sorry registry distribution).

;;; Code:

(require 'cl-lib)
(require 'arxana-browser-rewrites) ; shared EDN reader

(defgroup arxana-vsatarcs-sorrys nil
  "Sorry-registry snapshot for the VSATARCS reader surface."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-sorrys-file
  ;; R-A.1 (M-war-machine-first-outing): registry relocated data/ → resources/
  ;; (tracked). Prefer resources/, fall back to data/ for transition-safety.
  (let ((res (expand-file-name "~/code/futon2/resources/sorrys.edn"))
        (old (expand-file-name "~/code/futon2/data/sorrys.edn")))
    (if (file-exists-p res) res old))
  "Path to the hand-curated sorry registry.
Read on every snapshot call; no in-memory cache is held."
  :type 'file
  :group 'arxana-vsatarcs-sorrys)

(defconst arxana-vsatarcs-sorrys-known-kinds
  '(:meta :prototyping-forward :technical-debt :decision-debt :external-dependency)
  "Closed set of `:kind' values the registry schema v2 declares.
Used by `arxana-vsatarcs-sorrys-snapshot' to render a stable
distribution row even when some kinds are absent from the current
registry.  Sorrys carrying an unknown `:kind' are grouped under
the symbol `unknown' in the distribution.")

(defun arxana-vsatarcs-sorrys--strip-leading-colon (sym)
  "Return symbol SYM with any leading `:' stripped from its name.
The shared EDN reader interns keywords as symbols whose name retains
the leading `:'; helper for normalising `:status' and `:kind' values
to bare keywords for stable comparisons across surfaces."
  (when sym
    (let ((s (symbol-name sym)))
      (if (string-prefix-p ":" s)
          (intern (concat ":" (substring s 1)))
        (intern (concat ":" s))))))

(defun arxana-vsatarcs-sorrys--plist-get (sorry key)
  "Return KEY from SORRY (a plist as returned by the shared EDN reader).
KEY is the bare keyword form (`:id', `:kind', etc.); the reader
interns keys as symbols whose names retain the leading `:'."
  (plist-get sorry (intern (symbol-name key))))

(defun arxana-vsatarcs-sorrys--load ()
  "Return the raw `:sorrys' vector from the registry file, or nil.
Returns nil silently when the file is unreadable; callers should
treat nil as \"no registry block to render\"."
  (when (file-readable-p arxana-vsatarcs-sorrys-file)
    (let* ((data (arxana-browser-rewrites--read-edn-file
                  arxana-vsatarcs-sorrys-file))
           (sorrys (plist-get data (intern ":sorrys"))))
      (and sorrys (append sorrys nil)))))

(defun arxana-vsatarcs-sorrys--summarise (sorry)
  "Return an operator-facing plist summary of SORRY.
The output plist carries `:id', `:title', `:status', `:kind',
`:raised-at', `:related-missions'.  `:status' and `:kind' are
normalised to bare keywords; `:related-missions' is a list of
strings (the mission-file basenames)."
  (let ((kind (arxana-vsatarcs-sorrys--plist-get sorry :kind))
        (status (arxana-vsatarcs-sorrys--plist-get sorry :status))
        (missions (arxana-vsatarcs-sorrys--plist-get sorry :related-missions)))
    (list :id            (arxana-vsatarcs-sorrys--plist-get sorry :id)
          :title         (arxana-vsatarcs-sorrys--plist-get sorry :title)
          :status        (and status
                              (arxana-vsatarcs-sorrys--strip-leading-colon status))
          :kind          (and kind
                              (arxana-vsatarcs-sorrys--strip-leading-colon kind))
          :raised-at     (arxana-vsatarcs-sorrys--plist-get sorry :raised-at)
          :related-missions (and missions
                                 (if (vectorp missions)
                                     (append missions nil)
                                   missions)))))

(defun arxana-vsatarcs-sorrys--kind-counts (summaries)
  "Return an alist of (kind . count) over SUMMARIES.
Counts cover all `arxana-vsatarcs-sorrys-known-kinds' plus `unknown'
(absent kinds map to 0 so the row shape is stable across snapshots)."
  (let ((counts (mapcar (lambda (k) (cons k 0))
                        arxana-vsatarcs-sorrys-known-kinds))
        (unknown 0))
    (dolist (s summaries)
      (let* ((k (plist-get s :kind))
             (cell (assoc k counts)))
        (if cell
            (setcdr cell (1+ (cdr cell)))
          (setq unknown (1+ unknown)))))
    (append counts (list (cons 'unknown unknown)))))

(defun arxana-vsatarcs-sorrys--status-counts (summaries)
  "Return an alist of (status . count) over SUMMARIES.
Open / addressed / foreclosed are the schema-declared statuses."
  (let ((open 0) (addressed 0) (foreclosed 0) (other 0))
    (dolist (s summaries)
      (pcase (plist-get s :status)
        (:open (cl-incf open))
        (:addressed (cl-incf addressed))
        (:foreclosed (cl-incf foreclosed))
        (_ (cl-incf other))))
    (list (cons :open open)
          (cons :addressed addressed)
          (cons :foreclosed foreclosed)
          (cons 'other other))))

(defun arxana-vsatarcs-sorrys-snapshot ()
  "Return the sorry-registry snapshot.
The snapshot is a plist:

  (:registry-loaded?    <t or nil>
   :registry-path       <absolute path read>
   :total               <integer>
   :kind-counts         ((<kind> . <count>) ...)
   :status-counts       ((<status> . <count>) ...)
   :sorrys              (<summary-plist> ...))

Sorrys are returned in registry order (preserving the file's
canonical sequence — operator-meaningful, not alphabetised)."
  (let* ((raw (arxana-vsatarcs-sorrys--load))
         (summaries (mapcar #'arxana-vsatarcs-sorrys--summarise (or raw nil))))
    (list :registry-loaded? (not (null raw))
          :registry-path arxana-vsatarcs-sorrys-file
          :total (length summaries)
          :kind-counts (arxana-vsatarcs-sorrys--kind-counts summaries)
          :status-counts (arxana-vsatarcs-sorrys--status-counts summaries)
          :sorrys summaries)))

(defun arxana-vsatarcs-sorrys-by-mission (mission)
  "Return summaries of sorrys whose `:related-missions' contains MISSION.
MISSION is a string (mission-file basename, e.g.
`\"M-war-machine-aif-completion\"').  Used for story-scoped filtering
when a story carries a mission reference."
  (let ((summaries (plist-get (arxana-vsatarcs-sorrys-snapshot) :sorrys)))
    (cl-remove-if-not
     (lambda (s)
       (cl-find mission (plist-get s :related-missions) :test #'string=))
     summaries)))

(defconst arxana-vsatarcs-sorrys--mission-regexp
  "\\bM-[a-zA-Z][a-zA-Z0-9-]+\\b"
  "Regexp matching mission-file basenames (without the `.md' suffix).
Mission convention: `M-' prefix + kebab-case identifier (e.g.
`M-war-machine-aif-completion', `M-stack-essay-code-alignment').")

(defun arxana-vsatarcs-sorrys-missions-mentioned-in (text)
  "Return a deduplicated list of mission basenames referenced in TEXT.
Scans TEXT for `M-[name]'-shape tokens; returns the unique set in
first-occurrence order so chrome callers can highlight the
primary-mentioned mission first."
  (when (stringp text)
    (let (matches seen)
      (let ((pos 0))
        (while (string-match arxana-vsatarcs-sorrys--mission-regexp text pos)
          (let ((m (match-string 0 text)))
            (unless (member m seen)
              (push m matches)
              (push m seen)))
          (setq pos (match-end 0))))
      (nreverse matches))))

(defun arxana-vsatarcs-sorrys-snapshot-for-text (text)
  "Return a story-scoped sorry snapshot keyed off mission refs in TEXT.

When TEXT mentions one or more `M-*' missions, returns a snapshot
plist mirroring `arxana-vsatarcs-sorrys-snapshot' but with
`:sorrys' filtered to entries whose `:related-missions' overlap
with the mentioned set.  The snapshot adds two extra fields:

  :scoped?           t (always, when this entry-point is used)
  :scope-missions    (list of mentioned-mission basenames, primary first)

When TEXT has no mission references OR the filter result is empty,
falls back to the global snapshot with `:scoped? nil' so the chrome
always renders something operator-meaningful."
  (let* ((missions (arxana-vsatarcs-sorrys-missions-mentioned-in text))
         (global (arxana-vsatarcs-sorrys-snapshot)))
    (if (null missions)
        (append global (list :scoped? nil :scope-missions nil))
      (let* ((filtered (cl-remove-if-not
                        (lambda (s)
                          (cl-intersection
                           (plist-get s :related-missions)
                           missions
                           :test #'string=))
                        (plist-get global :sorrys)))
             (kind-counts (arxana-vsatarcs-sorrys--kind-counts filtered))
             (status-counts (arxana-vsatarcs-sorrys--status-counts filtered)))
        (if (null filtered)
            (append global (list :scoped? nil :scope-missions missions))
          (list :registry-loaded? (plist-get global :registry-loaded?)
                :registry-path (plist-get global :registry-path)
                :total (length filtered)
                :kind-counts kind-counts
                :status-counts status-counts
                :sorrys filtered
                :scoped? t
                :scope-missions missions))))))

(provide 'arxana-vsatarcs-sorrys)
;;; arxana-vsatarcs-sorrys.el ends here
