;;; arxana-vsatarcs-r-criteria-wm-test.el --- Tests for WM R-criteria parser -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-r-criteria-wm.el' (reader-criterion Q1:
;; V-COV + V-CUR).  Covers markdown table parsing, status
;; normalisation, version extraction, the closed-set alignment, and
;; the by-key/status-counts apparatus.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-r-criteria-wm)

;; ---------------------------------------------------------------------
;; Status normalisation
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-r-criteria-wm-normalise-checkmark ()
  (should (eq :satisfied
              (arxana-vsatarcs-r-criteria-wm--normalise-status "✓"))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-normalise-checkmark-with-version ()
  (should (eq :satisfied
              (arxana-vsatarcs-r-criteria-wm--normalise-status
               "**✓ as of v0.12**"))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-normalise-na ()
  (should (eq :n-a
              (arxana-vsatarcs-r-criteria-wm--normalise-status "N/A"))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-normalise-cross ()
  (should (eq :not-satisfied
              (arxana-vsatarcs-r-criteria-wm--normalise-status "✗"))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-normalise-empty-is-unknown ()
  (should (eq :unknown
              (arxana-vsatarcs-r-criteria-wm--normalise-status ""))))

;; ---------------------------------------------------------------------
;; Version extraction
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-r-criteria-wm-extract-as-of-simple ()
  (should (equal "v0.12"
                 (arxana-vsatarcs-r-criteria-wm--extract-as-of
                  "**✓ as of v0.12**"))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-extract-as-of-three-part ()
  (should (equal "v0.5.1"
                 (arxana-vsatarcs-r-criteria-wm--extract-as-of
                  "**✓ as of v0.5.1**"))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-extract-as-of-absent ()
  (should (null (arxana-vsatarcs-r-criteria-wm--extract-as-of "✓"))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-extract-as-of-na ()
  (should (null (arxana-vsatarcs-r-criteria-wm--extract-as-of "N/A"))))

;; ---------------------------------------------------------------------
;; Row parsing
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-r-criteria-wm-parse-row-basic ()
  (let ((row (arxana-vsatarcs-r-criteria-wm--parse-row
              "| R1 — Explicit belief state | **✓ as of v0.2** | — (`futon2.aif.belief`) |")))
    (should row)
    (should (eq :R1 (plist-get row :R)))
    (should (equal "Explicit belief state" (plist-get row :name)))
    (should (eq :satisfied (plist-get row :status)))
    (should (equal "v0.2" (plist-get row :as-of)))
    ;; Single `—' becomes nil per the parser's blocker-clean rule.
    (should (string-match-p "futon2.aif.belief" (plist-get row :blocker)))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-parse-row-na ()
  (let ((row (arxana-vsatarcs-r-criteria-wm--parse-row
              "| R11 — Hierarchical composition | N/A | Single observer at this scope |")))
    (should (eq :R11 (plist-get row :R)))
    (should (eq :n-a (plist-get row :status)))
    (should (null (plist-get row :as-of)))
    (should (equal "Single observer at this scope" (plist-get row :blocker)))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-parse-row-cross ()
  (let ((row (arxana-vsatarcs-r-criteria-wm--parse-row
              "| R12 — Dual-loop hyperparameter inference | ✗ | Deferred per §3.1 |")))
    (should (eq :R12 (plist-get row :R)))
    (should (eq :not-satisfied (plist-get row :status)))
    (should (string-match-p "Deferred" (plist-get row :blocker)))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-parse-row-em-dash-blocker ()
  (let ((row (arxana-vsatarcs-r-criteria-wm--parse-row
              "| R2 — Observation channel schema | ✓ | — |")))
    ;; Bare `—' blocker should become nil (clean cell).
    (should (null (plist-get row :blocker)))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-parse-row-rejects-header ()
  (should (null (arxana-vsatarcs-r-criteria-wm--parse-row
                 "| R-criterion | Status | Gap-closing checkpoint / blocker |"))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-parse-row-rejects-separator ()
  (should (null (arxana-vsatarcs-r-criteria-wm--parse-row "|---|---|---|"))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-parse-row-rejects-non-r ()
  (should (null (arxana-vsatarcs-r-criteria-wm--parse-row
                 "| F1 — Explicit fitness state | ✓ | — |"))))

;; ---------------------------------------------------------------------
;; Find-summary-block
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-r-criteria-wm-find-summary-block-basic ()
  (let* ((text "# Doc\n\n## Why\n\nSome prose.\n\n## Summary\n\n| R1 — X | ✓ | — |\n\n## Next\n\nMore prose.")
         (block (arxana-vsatarcs-r-criteria-wm--find-summary-block text)))
    (should block)
    (should (string-match-p "R1 — X" block))
    (should-not (string-match-p "Next" block))
    (should-not (string-match-p "## Summary" block))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-find-summary-block-eof ()
  ;; If no later `## ' heading, block extends to end-of-text.
  (let* ((text "## Summary\n\n| R1 — X | ✓ | — |\n")
         (block (arxana-vsatarcs-r-criteria-wm--find-summary-block text)))
    (should (string-match-p "R1 — X" block))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-find-summary-block-missing ()
  (should (null (arxana-vsatarcs-r-criteria-wm--find-summary-block
                 "# Doc\n\nNo summary heading here.\n"))))

;; ---------------------------------------------------------------------
;; Snapshot integration
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-r-criteria-wm-test--write-fixture (path)
  "Write a minimal contract fixture at PATH with a Summary table."
  (with-temp-file path
    (insert "# Test contract\n\n"
            "## Why\n\nProse.\n\n"
            "## Summary\n\n"
            "| R-criterion | Status | Gap-closing checkpoint / blocker |\n"
            "|---|---|---|\n"
            "| R1 — Explicit belief state | **✓ as of v0.2** | — (`futon2.aif.belief`) |\n"
            "| R2 — Observation channel schema | ✓ | — |\n"
            "| R3 — Predictive-coding belief update | **✓ as of v0.11** | 4 of 14 channels |\n"
            "| R4 — Predictive forward model | **✓ as of v0.3** | — |\n"
            "| R5 — Principled EFE terms | **✓ as of v0.4** | — |\n"
            "| R6 — Softmax + abstain | **✓ as of v0.5** | — |\n"
            "| R7 — Adaptive precision | **✓ as of v0.12** | — |\n"
            "| R8 — Per-tick trace | **✓ as of v0.7** | — |\n"
            "| R9 — Named validation properties | **✓ as of v0.7** | — |\n"
            "| R10 — Live operation | **✓ as of v0.8** (scheduled-execution-ready) | — |\n"
            "| R11 — Hierarchical composition | N/A | Single observer at this scope |\n"
            "| R12 — Dual-loop hyperparameter inference | ✗ | Deferred per §3.1 |\n"
            "\n## Next section\n\nMore prose.\n")))

(defmacro arxana-vsatarcs-r-criteria-wm-test--with-fixture (&rest body)
  "Bind contract file to a temp fixture; eval BODY."
  `(let* ((tmp (make-temp-file "vsatarcs-r-criteria-wm-" nil ".md"))
          (arxana-vsatarcs-r-criteria-wm-file tmp))
     (unwind-protect
         (progn
           (arxana-vsatarcs-r-criteria-wm-test--write-fixture tmp)
           ,@body)
       (when (file-exists-p tmp) (delete-file tmp)))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-snapshot-missing-file-graceful ()
  (let ((arxana-vsatarcs-r-criteria-wm-file "/nonexistent/path.md"))
    (let ((snap (arxana-vsatarcs-r-criteria-wm-snapshot)))
      (should (not (plist-get snap :contract-loaded?)))
      (should (equal '() (plist-get snap :rows)))
      ;; All 12 keys present in by-key with :unknown status.
      (dolist (k arxana-vsatarcs-r-criteria-wm-rs)
        (let ((row (plist-get (plist-get snap :by-key) k)))
          (should row)
          (should (eq :unknown (plist-get row :status))))))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-snapshot-full-table ()
  (arxana-vsatarcs-r-criteria-wm-test--with-fixture
   (let* ((snap (arxana-vsatarcs-r-criteria-wm-snapshot))
          (rows (plist-get snap :rows))
          (by-key (plist-get snap :by-key)))
     (should (plist-get snap :contract-loaded?))
     (should (= 12 (length rows)))
     (should (eq :satisfied (plist-get (plist-get by-key :R1) :status)))
     (should (equal "v0.2" (plist-get (plist-get by-key :R1) :as-of)))
     (should (eq :satisfied (plist-get (plist-get by-key :R3) :status)))
     (should (equal "v0.11" (plist-get (plist-get by-key :R3) :as-of)))
     (should (eq :n-a (plist-get (plist-get by-key :R11) :status)))
     (should (eq :not-satisfied (plist-get (plist-get by-key :R12) :status))))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-snapshot-status-counts ()
  (arxana-vsatarcs-r-criteria-wm-test--with-fixture
   (let* ((snap (arxana-vsatarcs-r-criteria-wm-snapshot))
          (counts (plist-get snap :status-counts)))
     ;; R1-R10 satisfied (10); R11 N/A (1); R12 not-satisfied (1).
     (should (= 10 (cdr (assoc :satisfied counts))))
     (should (= 1 (cdr (assoc :n-a counts))))
     (should (= 1 (cdr (assoc :not-satisfied counts))))
     (should (= 0 (cdr (assoc :unknown counts)))))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-snapshot-summary-line ()
  (arxana-vsatarcs-r-criteria-wm-test--with-fixture
   (let ((line (plist-get (arxana-vsatarcs-r-criteria-wm-snapshot)
                          :summary-line)))
     (should (string-match-p "10 satisfied" line))
     (should (string-match-p "1 N/A" line))
     (should (string-match-p "1 not-satisfied" line)))))

(ert-deftest arxana-vsatarcs-r-criteria-wm-snapshot-rows-in-order ()
  (arxana-vsatarcs-r-criteria-wm-test--with-fixture
   (let* ((snap (arxana-vsatarcs-r-criteria-wm-snapshot))
          (keys (mapcar (lambda (r) (plist-get r :R)) (plist-get snap :rows))))
     (should (equal '(:R1 :R2 :R3 :R4 :R5 :R6 :R7 :R8 :R9 :R10 :R11 :R12)
                    keys)))))

;; ---------------------------------------------------------------------
;; Live WM contract smoke (defensive — only when file exists)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-r-criteria-wm-live-smoke ()
  (when (file-readable-p
         (expand-file-name "~/code/futon2/docs/futon-aif-completeness.md"))
    (let* ((arxana-vsatarcs-r-criteria-wm-file
            (expand-file-name "~/code/futon2/docs/futon-aif-completeness.md"))
           (snap (arxana-vsatarcs-r-criteria-wm-snapshot))
           (counts (plist-get snap :status-counts)))
      (should (plist-get snap :contract-loaded?))
      ;; Per the doc as of v0.16: R1-R10 satisfied, R11 N/A, R12 not-satisfied.
      (should (>= (cdr (assoc :satisfied counts)) 9))
      (should (= 1 (cdr (assoc :n-a counts))))
      (should (>= (cdr (assoc :not-satisfied counts)) 1)))))

(provide 'arxana-vsatarcs-r-criteria-wm-test)
;;; arxana-vsatarcs-r-criteria-wm-test.el ends here
