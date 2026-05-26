;;; arxana-vsatarcs-cluster-test.el --- Tests for VSATARCS cluster-overview module -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-cluster.el' (reader-criterion Q8: V-COV).
;; Covers per-mission extraction (status line, lifecycle stage,
;; checkpoint counts), cluster aggregation, edge cases (missing
;; files, missions without lifecycle markers), and the digest line.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-cluster)

;; ---------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-cluster-test--write-mission (path body)
  "Write mission markdown at PATH containing BODY (the file content)."
  (with-temp-file path
    (insert body)))

(defmacro arxana-vsatarcs-cluster-test--with-fixtures (specs &rest body)
  "Bind `arxana-vsatarcs-cluster-mission-files' to temp files holding SPECS.
SPECS is a list of (BASENAME . CONTENT) cons cells.  Each file is
written to a temp dir; the dynamic var is bound to those paths
in order."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "vsatarcs-cluster-" t))
          (paths (mapcar
                  (lambda (spec)
                    (let ((p (expand-file-name
                              (concat (car spec) ".md") dir)))
                      (arxana-vsatarcs-cluster-test--write-mission
                       p (cdr spec))
                      p))
                  ,specs))
          (arxana-vsatarcs-cluster-mission-files paths))
     (unwind-protect
         (progn ,@body)
       (dolist (p paths)
         (when (file-exists-p p) (delete-file p)))
       (when (file-directory-p dir) (delete-directory dir)))))

(defconst arxana-vsatarcs-cluster-test--wm-mission
  "# Mission: M-war-machine-aif-completion

**Date:** 2026-05-17
**Status:** R1-R10 satisfied through v0.16 (Checkpoint 9 closed 2026-05-19). R11 N/A; R12 deferred. **Exit criterion source:** §0 of M-descriptive-essay-of-the-stack.md, criterion #2.

## 1. IDENTIFY — done

## 2. MAP — done

## 3. DERIVE — done

## 4. DOCUMENT — in progress

### Checkpoint 0 — runtime-location resolved
**Status: COMPLETE 2026-05-17.**

### Checkpoint 1 — R1 belief
**Status: COMPLETE 2026-05-17.**

### Checkpoint 2 — R4 forward model
**Status: COMPLETE 2026-05-17.**

### Checkpoint 3 — R5+R6
**Status: COMPLETE 2026-05-17.**
")

(defconst arxana-vsatarcs-cluster-test--align-mission
  "# Mission: M-stack-essay-code-alignment

**Date:** 2026-05-17
**Status:** v0.5.14 in flight; 7 of 8 reader-criteria satisfied.

## 1. IDENTIFY — Path-arrow essentials

## 2. MAP — Tensions

### Checkpoint 0 — M-INC step (b) lands
### Checkpoint 1 — first alignment-anomaly recorded
**Status: COMPLETE 2026-05-18.**
")

(defconst arxana-vsatarcs-cluster-test--gated-mission
  "# Mission: M-stack-morphogenetic-rewrite

**Date:** 2026-05-17
**Status:** Gated on cluster siblings closing.

## 1. IDENTIFY — pending
")

(defconst arxana-vsatarcs-cluster-test--bare-mission
  "# Mission: M-bare-test

(no status, no stage, no checkpoints — defensive case)
")

;; ---------------------------------------------------------------------
;; basename helper
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-cluster-basename-strips-md ()
  (should (equal "M-foo"
                 (arxana-vsatarcs-cluster--basename-no-md "/a/b/M-foo.md"))))

(ert-deftest arxana-vsatarcs-cluster-basename-no-suffix ()
  (should (equal "M-foo"
                 (arxana-vsatarcs-cluster--basename-no-md "/a/M-foo"))))

;; ---------------------------------------------------------------------
;; Status line extraction
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-cluster-extract-status-basic ()
  (let ((line (arxana-vsatarcs-cluster--extract-status-line
               arxana-vsatarcs-cluster-test--wm-mission)))
    (should line)
    (should (string-match-p "R1-R10 satisfied" line))
    (should (string-match-p "Checkpoint 9 closed" line))))

(ert-deftest arxana-vsatarcs-cluster-extract-status-flattens-newlines ()
  ;; Multi-line **Status:** body should flatten internal newlines.
  (let* ((text "**Status:** First sentence.\n   Second sentence.\n\n## Next")
         (line (arxana-vsatarcs-cluster--extract-status-line text)))
    (should (string-match-p "First sentence" line))
    (should (string-match-p "Second sentence" line))
    (should-not (string-match-p "\n" line))))

(ert-deftest arxana-vsatarcs-cluster-extract-status-missing ()
  (should (null (arxana-vsatarcs-cluster--extract-status-line
                 arxana-vsatarcs-cluster-test--bare-mission))))

;; ---------------------------------------------------------------------
;; Lifecycle stage detection
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-cluster-detect-stage-document ()
  ;; WM mission has IDENTIFY/MAP/DERIVE/DOCUMENT — highest is :document.
  (should (eq :document
              (arxana-vsatarcs-cluster--detect-stage
               arxana-vsatarcs-cluster-test--wm-mission))))

(ert-deftest arxana-vsatarcs-cluster-detect-stage-map ()
  ;; Alignment mission only goes through MAP.
  (should (eq :map
              (arxana-vsatarcs-cluster--detect-stage
               arxana-vsatarcs-cluster-test--align-mission))))

(ert-deftest arxana-vsatarcs-cluster-detect-stage-identify ()
  ;; Gated mission only has IDENTIFY.
  (should (eq :identify
              (arxana-vsatarcs-cluster--detect-stage
               arxana-vsatarcs-cluster-test--gated-mission))))

(ert-deftest arxana-vsatarcs-cluster-detect-stage-none ()
  (should (null (arxana-vsatarcs-cluster--detect-stage
                 arxana-vsatarcs-cluster-test--bare-mission))))

;; ---------------------------------------------------------------------
;; Checkpoint counting
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-cluster-count-checkpoints-wm ()
  (let ((cps (arxana-vsatarcs-cluster--count-checkpoints
              arxana-vsatarcs-cluster-test--wm-mission)))
    ;; 4 checkpoints, all complete.
    (should (= 4 (plist-get cps :total)))
    (should (= 4 (plist-get cps :complete)))))

(ert-deftest arxana-vsatarcs-cluster-count-checkpoints-align ()
  (let ((cps (arxana-vsatarcs-cluster--count-checkpoints
              arxana-vsatarcs-cluster-test--align-mission)))
    ;; 2 checkpoints; only Cp 1 is COMPLETE.
    (should (= 2 (plist-get cps :total)))
    (should (= 1 (plist-get cps :complete)))))

(ert-deftest arxana-vsatarcs-cluster-count-checkpoints-none ()
  (let ((cps (arxana-vsatarcs-cluster--count-checkpoints
              arxana-vsatarcs-cluster-test--bare-mission)))
    (should (= 0 (plist-get cps :total)))
    (should (= 0 (plist-get cps :complete)))))

;; ---------------------------------------------------------------------
;; Snapshot integration
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-cluster-snapshot-three-missions ()
  (arxana-vsatarcs-cluster-test--with-fixtures
      `(("M-wm" . ,arxana-vsatarcs-cluster-test--wm-mission)
        ("M-align" . ,arxana-vsatarcs-cluster-test--align-mission)
        ("M-gated" . ,arxana-vsatarcs-cluster-test--gated-mission))
    (let* ((snap (arxana-vsatarcs-cluster-snapshot))
           (missions (plist-get snap :missions)))
      (should (plist-get snap :cluster-loaded?))
      (should (= 3 (length missions)))
      ;; Stage distribution: 1 :identify, 1 :map, 1 :document.
      (let ((sc (plist-get snap :stage-counts)))
        (should (= 1 (cdr (assoc :identify sc))))
        (should (= 1 (cdr (assoc :map sc))))
        (should (= 0 (cdr (assoc :derive sc))))
        (should (= 1 (cdr (assoc :document sc))))
        (should (= 0 (cdr (assoc nil sc)))))
      ;; Checkpoint totals: 4 + 2 + 0 = 6 total; 4 + 1 + 0 = 5 complete.
      (should (= 6 (plist-get snap :checkpoints-total)))
      (should (= 5 (plist-get snap :checkpoints-complete))))))

(ert-deftest arxana-vsatarcs-cluster-snapshot-mission-order-preserved ()
  (arxana-vsatarcs-cluster-test--with-fixtures
      `(("M-z-last" . ,arxana-vsatarcs-cluster-test--gated-mission)
        ("M-a-first" . ,arxana-vsatarcs-cluster-test--wm-mission))
    (let* ((snap (arxana-vsatarcs-cluster-snapshot))
           (names (mapcar (lambda (m) (plist-get m :mission))
                          (plist-get snap :missions))))
      ;; File-order preserved (not alphabetised).
      (should (equal '("M-z-last" "M-a-first") names)))))

(ert-deftest arxana-vsatarcs-cluster-snapshot-missing-file-graceful ()
  (let ((arxana-vsatarcs-cluster-mission-files
         '("/nonexistent/M-foo.md" "/nonexistent/M-bar.md")))
    (let* ((snap (arxana-vsatarcs-cluster-snapshot))
           (missions (plist-get snap :missions)))
      (should-not (plist-get snap :cluster-loaded?))
      (should (= 2 (length missions)))
      (dolist (m missions)
        (should-not (plist-get m :loaded?))
        (should (null (plist-get m :status-line)))
        (should (null (plist-get m :stage)))))))

(ert-deftest arxana-vsatarcs-cluster-snapshot-empty-list ()
  (let ((arxana-vsatarcs-cluster-mission-files '()))
    (let ((snap (arxana-vsatarcs-cluster-snapshot)))
      ;; cl-every over empty list is t; cluster-loaded? = t vacuously.
      (should (plist-get snap :cluster-loaded?))
      (should (equal '() (plist-get snap :missions)))
      (should (= 0 (plist-get snap :checkpoints-total)))
      (should (= 0 (plist-get snap :checkpoints-complete))))))

;; ---------------------------------------------------------------------
;; Per-mission summary structure
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-cluster-summarise-wm-mission ()
  (arxana-vsatarcs-cluster-test--with-fixtures
      `(("M-war-machine" . ,arxana-vsatarcs-cluster-test--wm-mission))
    (let* ((snap (arxana-vsatarcs-cluster-snapshot))
           (m (car (plist-get snap :missions))))
      (should (equal "M-war-machine" (plist-get m :mission)))
      (should (plist-get m :loaded?))
      (should (string-match-p "R1-R10" (plist-get m :status-line)))
      (should (eq :document (plist-get m :stage)))
      (should (= 4 (plist-get (plist-get m :checkpoints) :total)))
      (should (= 4 (plist-get (plist-get m :checkpoints) :complete))))))

;; ---------------------------------------------------------------------
;; Digest line
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-cluster-digest-includes-stage-names ()
  (arxana-vsatarcs-cluster-test--with-fixtures
      `(("M-wm" . ,arxana-vsatarcs-cluster-test--wm-mission)
        ("M-align" . ,arxana-vsatarcs-cluster-test--align-mission)
        ("M-gated" . ,arxana-vsatarcs-cluster-test--gated-mission))
    (let ((digest (plist-get (arxana-vsatarcs-cluster-snapshot)
                             :digest-line)))
      (should (string-match-p "3/3 missions loaded" digest))
      (should (string-match-p "DOCUMENT\\|document" digest))
      (should (string-match-p "MAP\\|map" digest))
      (should (string-match-p "IDENTIFY\\|identify" digest))
      (should (string-match-p "5/6 complete" digest)))))

(ert-deftest arxana-vsatarcs-cluster-digest-no-missions ()
  (let ((arxana-vsatarcs-cluster-mission-files '()))
    (let ((digest (plist-get (arxana-vsatarcs-cluster-snapshot)
                             :digest-line)))
      (should (string-match-p "0/0" digest)))))

;; ---------------------------------------------------------------------
;; Live mission-doc smoke (defensive — only when files exist)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-cluster-live-smoke ()
  (let* ((existing
          (cl-remove-if-not
           #'file-readable-p
           (mapcar #'expand-file-name
                   '("~/code/futon7/holes/M-war-machine-aif-completion.md"
                     "~/code/futon7/holes/M-stack-essay-code-alignment.md"
                     "~/code/futon7/holes/M-stack-morphogenetic-rewrite.md")))))
    (when existing
      (let* ((arxana-vsatarcs-cluster-mission-files existing)
             (snap (arxana-vsatarcs-cluster-snapshot)))
        (should (plist-get snap :cluster-loaded?))
        (should (= (length existing) (length (plist-get snap :missions))))
        ;; Every loaded mission gets a status line and a basename.
        (dolist (m (plist-get snap :missions))
          (should (plist-get m :loaded?))
          (should (stringp (plist-get m :mission))))))))

(provide 'arxana-vsatarcs-cluster-test)
;;; arxana-vsatarcs-cluster-test.el ends here
