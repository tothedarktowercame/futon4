;;; arxana-vsatarcs-r-criteria-wm.el --- WM R-criteria status row for VSATARCS reader chrome -*- lexical-binding: t; -*-

;;; Commentary:
;; Reader-criteria V-COV + V-CUR closure (Q1 of
;; `~/code/futon2/docs/vsatarcs-reader-criteria.md').  Parses the
;; "Summary" table in `~/code/futon2/docs/futon-aif-completeness.md'
;; (claude-2's WM-side R-criteria contract) and exposes a compact
;; status row the VSATARCS reader chrome can surface alongside the
;; local R-criterion-audit.
;;
;; The shape is purely a renderer over an external markdown table.
;; No EDN involved — the WM contract is a hand-curated markdown
;; document, and operator-readability of "where is the WM RIGHT NOW
;; on R1-R12" is what Q1 asks for.  Currency follows the WM doc's
;; revision cadence (it is updated when WM closures land); the chrome
;; re-reads on every snapshot so an operator who refreshes a story
;; immediately sees any WM-side change.
;;
;; Parser scope: the table block following `## Summary' with column
;; header `| R-criterion | Status | Gap-closing checkpoint / blocker |'.
;; Rows match the pattern `| RN — <name> | <status> | <blocker> |'.
;; Status is normalised into the closed set `:satisfied' / `:n-a' /
;; `:not-satisfied' (the three values the WM table uses today).  When
;; a `✓ as of vX.Y' qualifier is present, the version is extracted
;; into `:as-of'.
;;
;; Contract: contributes to V-COV (coverage — reader can see the
;; WM-side status row alongside this side's audit) + V-CUR (currency —
;; refresh on every snapshot keeps the projection in sync with the WM
;; doc's revision cadence).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup arxana-vsatarcs-r-criteria-wm nil
  "WM R-criteria status row for the VSATARCS reader surface."
  :group 'arxana-vsatarcs)

(defcustom arxana-vsatarcs-r-criteria-wm-file
  (expand-file-name "~/code/futon2/docs/futon-aif-completeness.md")
  "Path to claude-2's WM AIF completeness contract.
Read on every snapshot call; no in-memory cache is held."
  :type 'file
  :group 'arxana-vsatarcs-r-criteria-wm)

(defconst arxana-vsatarcs-r-criteria-wm-rs
  '(:R1 :R2 :R3 :R4 :R5 :R6 :R7 :R8 :R9 :R10 :R11 :R12)
  "Closed set of R-criteria keys the parser tracks.
The WM contract's Summary table has one row per Rn; the snapshot
carries an entry for each declared key (with `:status :unknown'
when the row is missing) so the chrome layout is stable across
contract revisions.")

(defun arxana-vsatarcs-r-criteria-wm--normalise-status (raw)
  "Map a raw status cell string RAW to a closed-set keyword.
The WM table uses three forms:
  - `✓' or `**✓ as of vX.Y**'  → :satisfied
  - `N/A'                        → :n-a
  - `✗'                          → :not-satisfied
Unrecognised cells map to :unknown so the parser surfaces the
oddity rather than silently miscategorising."
  (let ((s (string-trim (or raw ""))))
    (cond
     ((string-match-p "N/A" s) :n-a)
     ((string-match-p "✗" s) :not-satisfied)
     ((string-match-p "✓" s) :satisfied)
     (t :unknown))))

(defun arxana-vsatarcs-r-criteria-wm--extract-as-of (raw)
  "Return the `vX.Y' substring from RAW, or nil if absent.
The WM table records `**✓ as of v0.12**'-style qualifiers; the
version anchor is operator-meaningful (lets the reader see at a
glance how recently each criterion landed)."
  (let ((s (or raw "")))
    (when (string-match "\\bv\\([0-9]+\\.[0-9]+\\(?:\\.[0-9]+\\)?\\)\\b" s)
      (concat "v" (match-string 1 s)))))

(defun arxana-vsatarcs-r-criteria-wm--strip-markdown-emphasis (s)
  "Strip `**', `*', and surrounding whitespace from S."
  (let ((trimmed (string-trim (or s ""))))
    (replace-regexp-in-string "[*`]" "" trimmed)))

(defun arxana-vsatarcs-r-criteria-wm--parse-row (line)
  "Return a plist `(:R <kw> :name <s> :status <kw> :as-of <s> :blocker <s>)' or nil.
LINE is one markdown table row.  Returns nil for header / separator
rows / non-Rn rows."
  (when (string-match
         "^|[[:space:]]*R\\([0-9]+\\)[[:space:]]*—[[:space:]]*\\([^|]+\\)|\\([^|]*\\)|\\(.*\\)|[[:space:]]*$"
         line)
    (let* ((n (string-to-number (match-string 1 line)))
           (name (arxana-vsatarcs-r-criteria-wm--strip-markdown-emphasis
                  (match-string 2 line)))
           (status-raw (match-string 3 line))
           (blocker-raw (match-string 4 line))
           (status (arxana-vsatarcs-r-criteria-wm--normalise-status status-raw))
           (as-of (arxana-vsatarcs-r-criteria-wm--extract-as-of status-raw))
           (blocker (string-trim (or blocker-raw "")))
           (blocker-clean (if (or (string= blocker "—") (string= blocker "-"))
                              nil
                            blocker)))
      (list :R (intern (format ":R%d" n))
            :name name
            :status status
            :as-of as-of
            :blocker blocker-clean))))

(defun arxana-vsatarcs-r-criteria-wm--find-summary-block (text)
  "Return the table block starting after the `## Summary' heading in TEXT.
Returns the substring from `## Summary' to the next `## ' heading or
end-of-string.  Returns nil when no `## Summary' heading is found."
  (when (string-match "^## Summary[[:space:]]*$" text)
    (let* ((start (match-end 0))
           (rest (substring text start))
           (next (string-match "^## " rest)))
      (if next (substring rest 0 next) rest))))

(defun arxana-vsatarcs-r-criteria-wm--parse-rows (block)
  "Parse R-criterion rows from BLOCK; return list of row plists in order."
  (let (rows)
    (dolist (line (split-string (or block "") "\n"))
      (let ((row (arxana-vsatarcs-r-criteria-wm--parse-row line)))
        (when row (push row rows))))
    (nreverse rows)))

(defun arxana-vsatarcs-r-criteria-wm--align-to-closed-set (rows)
  "Return a plist mapping each `arxana-vsatarcs-r-criteria-wm-rs' key
to its parsed row, filling missing keys with `(:status :unknown)'."
  (let (acc)
    (dolist (k arxana-vsatarcs-r-criteria-wm-rs)
      (let ((row (cl-find k rows :key (lambda (r) (plist-get r :R)))))
        (push k acc)
        (push (or row (list :R k :status :unknown :name nil
                            :as-of nil :blocker nil))
              acc)))
    (nreverse acc)))

(defun arxana-vsatarcs-r-criteria-wm--status-counts (rows)
  "Return an alist `(status . count)' over ROWS."
  (let ((satisfied 0) (n-a 0) (not-satisfied 0) (unknown 0))
    (dolist (r rows)
      (pcase (plist-get r :status)
        (:satisfied (cl-incf satisfied))
        (:n-a (cl-incf n-a))
        (:not-satisfied (cl-incf not-satisfied))
        (_ (cl-incf unknown))))
    (list (cons :satisfied satisfied)
          (cons :n-a n-a)
          (cons :not-satisfied not-satisfied)
          (cons :unknown unknown))))

(defun arxana-vsatarcs-r-criteria-wm-snapshot ()
  "Return the WM-side R-criteria status snapshot.

The snapshot is a plist:

  (:contract-loaded?    <t or nil>
   :contract-path       <absolute path read>
   :rows                ((:R RN :name ... :status ... :as-of ... :blocker ...) ...)
   :by-key              (plist <:R1> <row> <:R2> <row> ...)
   :status-counts       ((:satisfied . N) (:n-a . N) (:not-satisfied . N) (:unknown . N))
   :summary-line        <one-line concise status digest>)

Rows are returned in parsed order (R1 → R12 as they appear in the
contract's Summary table).  Rows missing from the contract are
filled into `:by-key' with `:status :unknown' so the chrome shape
is stable when the contract evolves."
  (let* ((path arxana-vsatarcs-r-criteria-wm-file)
         (loaded? (file-readable-p path))
         (text (and loaded?
                    (with-temp-buffer
                      (insert-file-contents path)
                      (buffer-string))))
         (block (and text
                     (arxana-vsatarcs-r-criteria-wm--find-summary-block text)))
         (rows (and block
                    (arxana-vsatarcs-r-criteria-wm--parse-rows block)))
         (by-key (arxana-vsatarcs-r-criteria-wm--align-to-closed-set
                  (or rows nil)))
         (counts (arxana-vsatarcs-r-criteria-wm--status-counts
                  (or rows nil))))
    (list :contract-loaded? loaded?
          :contract-path path
          :rows (or rows nil)
          :by-key by-key
          :status-counts counts
          :summary-line (arxana-vsatarcs-r-criteria-wm--summary-line counts))))

(defun arxana-vsatarcs-r-criteria-wm--summary-line (counts)
  "Render COUNTS as a one-line digest string."
  (format "WM-side: %d satisfied / %d N/A / %d not-satisfied / %d unknown"
          (cdr (assoc :satisfied counts))
          (cdr (assoc :n-a counts))
          (cdr (assoc :not-satisfied counts))
          (cdr (assoc :unknown counts))))

(provide 'arxana-vsatarcs-r-criteria-wm)
;;; arxana-vsatarcs-r-criteria-wm.el ends here
