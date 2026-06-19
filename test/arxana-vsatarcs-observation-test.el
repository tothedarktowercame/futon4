;;; arxana-vsatarcs-observation-test.el --- Tests for VSATARCS R2 observation -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-observation.el' (R2 of the standard AIF
;; completeness contract).  Schema-stability tests + per-channel tests
;; + observe/sense-to-vector roundtrip.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-observation)

;; ---------------------------------------------------------------------
;; Schema-stability tests (R2 declares a stable channel set)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-observation-channels-are-keywords ()
  (should (cl-every #'keywordp arxana-vsatarcs-observation-channels)))

(ert-deftest arxana-vsatarcs-observation-channels-have-no-duplicates ()
  (should (= (length arxana-vsatarcs-observation-channels)
             (length (delete-dups (copy-sequence
                                   arxana-vsatarcs-observation-channels))))))

(ert-deftest arxana-vsatarcs-observation-channels-count-stable ()
  ;; v0.3.0 declares 5 channels; growing the set is an R2-shape change
  ;; (closure annotation + contract bump).  This test guards the count.
  (should (= 5 (length arxana-vsatarcs-observation-channels))))

(ert-deftest arxana-vsatarcs-observation-channels-named ()
  (should (equal arxana-vsatarcs-observation-channels
                 '(:story-coverage
                   :lift-freshness
                   :annotation-overlay-presence
                   :scene-density
                   :link-density))))

;; ---------------------------------------------------------------------
;; Per-channel tests using fixtures + temp directories
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-observation-test--make-story-dir ()
  "Create a temp dir + two minimal scene-form stories + one .aif.edn overlay.
Returns the directory path."
  (let* ((dir (make-temp-file "vsatarcs-obs-fixture-" t))
         (s1 (expand-file-name "story-1.md" dir))
         (s2 (expand-file-name "story-2.md" dir))
         (a1 (expand-file-name "story-1.aif.edn" dir)))
    (with-temp-file s1
      (insert "# Story One\n\n"
              "## Scene: Opening | one\n\n*(opening scene)*\n\nBody [link](other)\n\n"
              "## Scene: Two | two\n\nBody two\n"))
    (with-temp-file s2
      (insert "# Story Two\n\n"
              "## Scene: Only | only\n\n*(opening scene)*\n\nBody only\n"))
    (with-temp-file a1
      (insert "{:annotations []}"))
    dir))

(defmacro arxana-vsatarcs-observation-test--with-story-dir (dir-var &rest body)
  "Bind DIR-VAR to a fresh fixture story directory; run BODY."
  (declare (indent 1))
  `(let ((,dir-var (arxana-vsatarcs-observation-test--make-story-dir))
         (arxana-vsatarcs-story-directories nil))
     (setq arxana-vsatarcs-story-directories (list ,dir-var))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory ,dir-var t)))))

(ert-deftest arxana-vsatarcs-observation-annotation-overlay-presence-half ()
  "Two stories, one overlay → 0.5."
  (arxana-vsatarcs-observation-test--with-story-dir _
    (let ((v (arxana-vsatarcs-observation-channel-annotation-overlay-presence)))
      (should (< (abs (- v 0.5)) 1e-9)))))

(ert-deftest arxana-vsatarcs-observation-annotation-overlay-presence-zero-when-empty ()
  (let ((arxana-vsatarcs-story-directories
         (list (make-temp-file "vsatarcs-obs-empty-" t))))
    (unwind-protect
        (should (= 0.0 (arxana-vsatarcs-observation-channel-annotation-overlay-presence)))
      (ignore-errors (delete-directory (car arxana-vsatarcs-story-directories) t)))))

(ert-deftest arxana-vsatarcs-observation-scene-density-respects-max ()
  ;; Two stories: 2 scenes + 1 scene = mean 1.5; max 20 → ~0.075.
  (arxana-vsatarcs-observation-test--with-story-dir _
    (let ((v (arxana-vsatarcs-observation-channel-scene-density))
          (arxana-vsatarcs-observation-scene-density-max 20.0))
      (should (and (>= v 0.0) (<= v 1.0)))
      (should (< (abs (- v (/ 1.5 20.0))) 1e-9)))))

(ert-deftest arxana-vsatarcs-observation-scene-density-clips-to-1 ()
  (arxana-vsatarcs-observation-test--with-story-dir _
    (let ((arxana-vsatarcs-observation-scene-density-max 0.5))
      (should (= 1.0 (arxana-vsatarcs-observation-channel-scene-density))))))

(ert-deftest arxana-vsatarcs-observation-link-density-bounded ()
  (arxana-vsatarcs-observation-test--with-story-dir _
    (let ((v (arxana-vsatarcs-observation-channel-link-density)))
      (should (and (>= v 0.0) (<= v 1.0))))))

(ert-deftest arxana-vsatarcs-observation-story-coverage-zero-when-no-sections ()
  (let ((set (make-hash-table :test 'equal)))
    (should (= 0.0 (arxana-vsatarcs-observation-channel-story-coverage nil set)))))

(ert-deftest arxana-vsatarcs-observation-story-coverage-one-when-all-present ()
  (let ((sections '((:id "a" :ref "dir/foo.md")
                    (:id "b" :ref "dir/bar.md")))
        (set (make-hash-table :test 'equal)))
    (puthash "foo.md" t set)
    (puthash "bar.md" t set)
    (should (= 1.0 (arxana-vsatarcs-observation-channel-story-coverage
                    sections set)))))

(ert-deftest arxana-vsatarcs-observation-story-coverage-half ()
  (let ((sections '((:id "a" :ref "dir/foo.md")
                    (:id "b" :ref "dir/bar.md")))
        (set (make-hash-table :test 'equal)))
    (puthash "foo.md" t set)
    (should (< (abs (- (arxana-vsatarcs-observation-channel-story-coverage
                        sections set)
                       0.5))
               1e-9))))

(ert-deftest arxana-vsatarcs-observation-story-coverage-ignores-refless-sections ()
  ;; Sections without :ref are excluded from the denominator entirely.
  (let ((sections '((:id "a" :ref "dir/foo.md")
                    (:id "b")  ; no :ref
                    (:id "c" :ref "dir/bar.md")))
        (set (make-hash-table :test 'equal)))
    (puthash "foo.md" t set)
    (should (< (abs (- (arxana-vsatarcs-observation-channel-story-coverage
                        sections set)
                       0.5))
               1e-9))))

(ert-deftest arxana-vsatarcs-observation-lift-freshness-vacuous-when-no-data ()
  (let ((set (make-hash-table :test 'equal)))
    (should (= 1.0 (arxana-vsatarcs-observation-channel-lift-freshness
                    nil set)))))

(ert-deftest arxana-vsatarcs-observation-lift-freshness-decays-with-staleness ()
  ;; Synthesise a section pointing at a fixture file; control mtimes
  ;; explicitly to verify the decay formula.
  (arxana-vsatarcs-observation-test--with-story-dir dir
    (let* ((path (expand-file-name "story-1.md" dir))
           ;; Set file mtime to a known time (T = 1000000000.0 ≈ Sept 2001)
           (recorded-time-str (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                                  (seconds-to-time 1000000000.0)
                                                  t))
           (now 1002592000.0) ; ~30 days later
           (sections (list (list :id "x" :ref "dir/story-1.md"
                                 :source-mtime recorded-time-str)))
           (set (make-hash-table :test 'equal)))
      (puthash "story-1.md" t set)
      ;; Touch the file to 1000000000.0 + small delta so on-disk ≈ recorded
      (set-file-times path (seconds-to-time 1000000000.0))
      (let ((v (arxana-vsatarcs-observation-channel-lift-freshness
                sections set now)))
        ;; on-disk mtime matches recorded mtime → delta ≈ 0 → freshness ≈ 1
        (should (> v 0.99))
        (should (<= v 1.0))))))

;; ---------------------------------------------------------------------
;; observe + sense-to-vector
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-observe-returns-plist-with-all-channels ()
  (arxana-vsatarcs-observation-test--with-story-dir _
    ;; No stack-annotations path; the only sections-dependent channel
    ;; falls back to 0.0, but the plist still carries all 5 keys.
    (let* ((arxana-vsatarcs-observation-stack-annotations-path
            (expand-file-name "no-such-file.edn" temporary-file-directory))
           (obs (arxana-vsatarcs-observe)))
      (dolist (ch arxana-vsatarcs-observation-channels)
        (should (plist-member obs ch))))))

(ert-deftest arxana-vsatarcs-observe-values-in-unit-interval ()
  (arxana-vsatarcs-observation-test--with-story-dir _
    (let* ((arxana-vsatarcs-observation-stack-annotations-path
            (expand-file-name "no-such-file.edn" temporary-file-directory))
           (obs (arxana-vsatarcs-observe)))
      (dolist (ch arxana-vsatarcs-observation-channels)
        (let ((v (plist-get obs ch)))
          (should (numberp v))
          (should (and (>= v 0.0) (<= v 1.0))))))))

(ert-deftest arxana-vsatarcs-sense-to-vector-respects-declared-order ()
  (let* ((obs '(:story-coverage 0.1
                :lift-freshness 0.2
                :annotation-overlay-presence 0.3
                :scene-density 0.4
                :link-density 0.5))
         (v (arxana-vsatarcs-sense-to-vector obs)))
    (should (equal v [0.1 0.2 0.3 0.4 0.5]))))

(ert-deftest arxana-vsatarcs-sense-to-vector-defaults-missing-to-zero ()
  (let ((v (arxana-vsatarcs-sense-to-vector '(:story-coverage 0.5))))
    (should (= 0.5 (aref v 0)))
    (dolist (i '(1 2 3 4))
      (should (= 0.0 (aref v i))))))

(provide 'arxana-vsatarcs-observation-test)
;;; arxana-vsatarcs-observation-test.el ends here
