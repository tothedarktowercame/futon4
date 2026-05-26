;;; arxana-vsatarcs-essay-revision-queue.el --- Essay revision queue surface -*- lexical-binding: t; -*-

;;; Commentary:
;; First in-vivo exercise of the R1-R11 + Q1-Q8 apparatus after v0.5.27
;; closure (Joe directive 2026-05-20): the futon7a essay corpus becomes
;; a typed substrate the apparatus reads, scores, and queues for
;; `:essay-revise' action-class dispatch.  Read-half here; write-half
;; lands on claude-2's M-vsatarcs-writer as a new action class
;; (`:essay-revise' or `:essay-niche-revision') extending the
;; self-documentation family per v0.5.22's safety-property
;; generalisation.
;;
;; **Revision-as-niche-creation framing** (Joe 2026-05-20): each essay
;; is an entity in a corpus-niche; cross-essay coherence is the niche's
;; free energy; revision is action.  The new Operator's Foreword
;; (authored via the eoi-engine simulated run with claude-8) changed
;; the corpus's free-energy landscape — surrounding essays gained
;; demand for revision to fit the new piece.  This module surfaces
;; that demand as an AIF-readable surface: per-essay staleness +
;; cross-link-density + named-stale-pattern signals, scored via the
;; existing `arxana-vsatarcs-efe' composition to rank revisions by
;; G-total.
;;
;; **Per-essay metrics**:
;;   :mtime              — file modification time (epoch)
;;   :days-since-mtime   — age in days from now
;;   :size-bytes         — byte size of the rendered HTML
;;   :cross-link-density — count of intra-futon7a `<a href="...html">' refs
;;   :stale-pattern-hits — count of regex hits for known stale-era markers
;;   :has-pkd-epigraph?  — distinguished signal for the About Us anchor
;;
;; **EFE scoring** (via `arxana-vsatarcs-efe-compute' once `:essay-revise'
;; is wired into the dispatch; today the module emits the per-essay
;; signals + a hand-tuned coarse score; EFE wiring lands as part of
;; the next claude-2 / claude-4 coordination cycle):
;;   :G-pragmatic ∝ -(staleness × cross-link-density × stale-pattern-hits)
;;   :G-epistemic ∝ -(recency-of-corpus-changes — similar pattern to other classes)
;;
;; Contract: contributes to R10's engagement-time surface (essay
;; corpus is now part of the apparatus's observable substrate);
;; provides the read-side payload for a future `:essay-revise'
;; action class.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup arxana-vsatarcs-essay-revision-queue nil
  "Essay revision queue surface for the VSATARCS reader."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-essay-revision-queue-corpus-directory
  (expand-file-name "~/code/futon7a/")
  "Directory containing essay HTML files to track.
Files matching `*.html' under this directory are candidates; the
exclude list (`-excluded-basenames') filters compliance / legal /
non-authored files."
  :type 'directory
  :group 'arxana-vsatarcs-essay-revision-queue)

(defcustom arxana-vsatarcs-essay-revision-queue-excluded-basenames
  '("Code-of-Conduct.html"
    "Anti-Bribery-Anti-Corruption.html"
    "Anti-Slavery-2022.html"
    "Human-Rights-Policy.html"
    "Real-Living-Wage.html"
    "policies.html"
    "vsatarcs.html")  ; the regenerated reader; not a hand-authored essay
  "Files to exclude from the revision queue.
Compliance / legal essays have different revision cadence (legal
review, not authored prose); the regenerated reader (vsatarcs.html)
is apparatus-output, not corpus-prose.  Operator can extend this
list freely."
  :type '(repeat string)
  :group 'arxana-vsatarcs-essay-revision-queue)

(defconst arxana-vsatarcs-essay-revision-queue--stale-patterns
  '(;; Pre-LLM-era AI funding-round URLs (TechCrunch shape):
    "techcrunch\\.com/[0-9]\\{4\\}/[0-9]\\{2\\}/.*\\(seed\\|series-[a-c]\\|raises\\|funding\\|valuation\\)"
    ;; Specific pre-LLM-era AI companies frequently cited as "current":
    "\\b\\(Codota\\|DeepCode\\|Andela\\)\\b"
    ;; Forward-looking claims with explicit years that have passed:
    "by 20\\(19\\|20\\|21\\|22\\)\\b"
    ;; Funding rounds from 2018-2022 era:
    "\\$[0-9]+[mM]\\b.*\\(Series A\\|Series B\\|seed round\\)")
  "Regex patterns indicating likely stale-era content.
Pattern hits contribute to the staleness signal in addition to mtime.
Patterns are conservative (false-negatives over false-positives);
adding patterns is a closure-worthy schema move so the test count
guards the set.")

(defconst arxana-vsatarcs-essay-revision-queue--pkd-marker
  "Philip K\\. Dick\\|Phillip K\\. Dick\\|Disneyland officials\\|How to Build a Universe"
  "Regex marker for the PKD epigraph anchor.
About Us carries this today; the new Operator's Foreword may add
a second instance for the niche-creation pair.  Operator-visible
in the queue snapshot's `:has-pkd-epigraph?' per-essay flag.")

(defun arxana-vsatarcs-essay-revision-queue--essay-files ()
  "Return absolute paths to all candidate essay files.
Filters via `-excluded-basenames'."
  (when (file-directory-p arxana-vsatarcs-essay-revision-queue-corpus-directory)
    (let ((dir arxana-vsatarcs-essay-revision-queue-corpus-directory))
      (cl-remove-if
       (lambda (p)
         (member (file-name-nondirectory p)
                 arxana-vsatarcs-essay-revision-queue-excluded-basenames))
       (directory-files dir t "\\.html\\'")))))

(defun arxana-vsatarcs-essay-revision-queue--read-essay (path)
  "Return PATH's contents as a string, or nil if unreadable.
Files capped at 200KB read since most futon7a essays are <50KB; large
HTML files (the regen'd vsatarcs.html at 466KB) are excluded above
but the cap provides defensive bound."
  (when (file-readable-p path)
    (with-temp-buffer
      (insert-file-contents path nil 0 200000)
      (buffer-string))))

(defun arxana-vsatarcs-essay-revision-queue--days-since-mtime (path)
  "Return days since PATH's mtime as a float."
  (let ((mtime (nth 5 (file-attributes path))))
    (when mtime
      (/ (- (float-time) (float-time mtime)) 86400.0))))

(defun arxana-vsatarcs-essay-revision-queue--cross-link-count (text)
  "Return the count of `<a href=\"*.html\">' references in TEXT.
Approximates corpus-internal coupling — higher = more cross-essay
links = revision touches more reader paths.  External URLs and
non-html targets are excluded."
  (when (stringp text)
    (let ((count 0) (pos 0))
      (while (string-match
              "<a[[:space:]]+[^>]*href=\"\\([^\"]+\\.html\\)\""
              text pos)
        (let ((href (match-string 1 text)))
          ;; Count only intra-corpus links (relative or same-host).
          (when (or (not (string-match-p "://" href))
                    (string-match-p "hyperreal\\.enterprises" href))
            (cl-incf count)))
        (setq pos (match-end 0)))
      count)))

(defun arxana-vsatarcs-essay-revision-queue--stale-pattern-hits (text)
  "Return the count of stale-pattern hits in TEXT across all known patterns."
  (when (stringp text)
    (let ((hits 0))
      (dolist (pat arxana-vsatarcs-essay-revision-queue--stale-patterns)
        (let ((pos 0))
          (while (string-match pat text pos)
            (cl-incf hits)
            (setq pos (match-end 0)))))
      hits)))

(defun arxana-vsatarcs-essay-revision-queue--has-pkd-marker? (text)
  "Return non-nil when TEXT contains a PKD-anchor marker."
  (and (stringp text)
       (string-match-p
        arxana-vsatarcs-essay-revision-queue--pkd-marker text)))

(defun arxana-vsatarcs-essay-revision-queue--summarise-essay (path)
  "Compute per-essay metrics for PATH; return a payload plist."
  (let* ((text (arxana-vsatarcs-essay-revision-queue--read-essay path))
         (basename (file-name-nondirectory path))
         (days (arxana-vsatarcs-essay-revision-queue--days-since-mtime path))
         (size (and (file-readable-p path)
                    (nth 7 (file-attributes path))))
         (xlinks (arxana-vsatarcs-essay-revision-queue--cross-link-count text))
         (stale-hits (arxana-vsatarcs-essay-revision-queue--stale-pattern-hits
                      text))
         (pkd? (arxana-vsatarcs-essay-revision-queue--has-pkd-marker? text)))
    (list :essay-basename basename
          :essay-path path
          :days-since-mtime (or days 0.0)
          :size-bytes (or size 0)
          :cross-link-density (or xlinks 0)
          :stale-pattern-hits (or stale-hits 0)
          :has-pkd-epigraph? pkd?
          ;; G-shape proxy combining the three signals.  Lower G = more
          ;; preferred (more demand for revision).  Two-term composition:
          ;; the dominant term scales with stale-pattern-hits (the strong
          ;; revision signal); the baseline term gives essays without
          ;; stale-hits a non-zero G based on age × cross-link-density
          ;; (so well-linked landing pages don't sit at G=0 forever just
          ;; because they don't trip a specific pattern).  When EFE
          ;; wiring lands in `arxana-vsatarcs-efe.el', this proxy gets
          ;; replaced by a per-class dispatch; for now it's an honest
          ;; first cut that surfaces the highest-priority revisions
          ;; correctly (faq-shaped essays dominate via the stale-hits
          ;; term; index/landing essays surface via the baseline term).
          :G-proxy (- (+ (* (or stale-hits 0)
                            (1+ (or xlinks 0))
                            (1+ (/ (or days 0.0) 30.0))
                            0.1)
                         (* (or xlinks 0)
                            (/ (or days 0.0) 30.0)
                            0.01))))))

(defun arxana-vsatarcs-essay-revision-queue-snapshot ()
  "Return the essay-revision-queue snapshot.

The snapshot is a plist:

  (:corpus-loaded?   <t when the corpus directory was readable>
   :corpus-dir       <absolute directory path read>
   :total-essays     <integer — candidate essays after exclusions>
   :pkd-anchored     <integer — essays carrying the PKD epigraph marker>
   :essays           (<payload-plist> ... sorted by G-proxy ascending)
   :digest-line      <one-line operator-facing summary>)

Sort is G-proxy-ascending so the highest-priority revision candidates
(largest staleness × cross-link-density × pattern-hits) surface
first.  Operator reads top-3 / top-5 for the immediate niche-creation
queue."
  (let* ((files (arxana-vsatarcs-essay-revision-queue--essay-files))
         (loaded? (and files (file-directory-p
                              arxana-vsatarcs-essay-revision-queue-corpus-directory)))
         (summaries (mapcar
                     #'arxana-vsatarcs-essay-revision-queue--summarise-essay
                     (or files nil)))
         (sorted (sort (copy-sequence summaries)
                       (lambda (a b)
                         (< (plist-get a :G-proxy)
                            (plist-get b :G-proxy)))))
         (pkd-count (cl-count-if (lambda (s) (plist-get s :has-pkd-epigraph?))
                                 summaries)))
    (list :corpus-loaded? loaded?
          :corpus-dir arxana-vsatarcs-essay-revision-queue-corpus-directory
          :total-essays (length files)
          :pkd-anchored pkd-count
          :essays sorted
          :digest-line
          (format "%d essays in corpus; %d PKD-anchored; top-priority: %s (G=%.3f)"
                  (length files) pkd-count
                  (or (and sorted (plist-get (car sorted) :essay-basename))
                      "?")
                  (or (and sorted (plist-get (car sorted) :G-proxy)) 0.0)))))

(provide 'arxana-vsatarcs-essay-revision-queue)
;;; arxana-vsatarcs-essay-revision-queue.el ends here
