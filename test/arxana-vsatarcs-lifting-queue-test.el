;;; arxana-vsatarcs-lifting-queue-test.el --- Tests for lifting-queue surface -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-lifting-queue.el'.  Covers:
;;
;;   - --infer-kind discrimination per filename prefix
;;   - --basename-slug suffix extraction
;;   - --proposed-id construction
;;   - Lifted-basename extraction from a minimal stack-annotations fixture
;;   - Full snapshot with temp story dir + temp stack-annotations
;;   - Edge cases: empty dir; stack-annotations unreadable; all stories lifted

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-lifting-queue)

;; ---------------------------------------------------------------------
;; Fixtures
;; ---------------------------------------------------------------------

(defmacro arxana-vsatarcs-lifting-queue-test--with-fixture
    (stories stack-annotations-edn &rest body)
  "Create a temp stories dir + stack-annotations file; eval BODY.
STORIES is a list of (BASENAME . CONTENT) pairs written into the
temp stories dir.  STACK-ANNOTATIONS-EDN is the content for the
temp stack-annotations.edn (a string).  Both customs are rebound
for the body's duration."
  (declare (indent 2))
  `(let* ((dir (make-temp-file "vsatarcs-lq-stories-" t))
          (saf (make-temp-file "vsatarcs-lq-sa-" nil ".edn"))
          (arxana-vsatarcs-lifting-queue-stories-directory
           (file-name-as-directory dir))
          (arxana-vsatarcs-lifting-queue-stack-annotations-file saf))
     (unwind-protect
         (progn
           (dolist (s ,stories)
             (with-temp-file (expand-file-name (car s) dir)
               (insert (cdr s))))
           (when ,stack-annotations-edn
             (with-temp-file saf (insert ,stack-annotations-edn)))
           ,@body)
       (dolist (f (directory-files dir t "\\.md\\'"))
         (when (file-exists-p f) (delete-file f)))
       (when (file-exists-p saf) (delete-file saf))
       (ignore-errors (delete-directory dir)))))

;; ---------------------------------------------------------------------
;; --infer-kind
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-lifting-queue-infer-kind-leaf ()
  (should (eq :leaf
              (arxana-vsatarcs-lifting-queue--infer-kind "leaf-2.md"))))

(ert-deftest arxana-vsatarcs-lifting-queue-infer-kind-devmap ()
  (should (eq :devmap
              (arxana-vsatarcs-lifting-queue--infer-kind "devmap-futon3.md"))))

(ert-deftest arxana-vsatarcs-lifting-queue-infer-kind-frame ()
  (should (eq :frame
              (arxana-vsatarcs-lifting-queue--infer-kind "frame-foo.md"))))

(ert-deftest arxana-vsatarcs-lifting-queue-infer-kind-war-machine ()
  (should (eq :war-machine
              (arxana-vsatarcs-lifting-queue--infer-kind
               "war-machine-lucid-scenes.md"))))

(ert-deftest arxana-vsatarcs-lifting-queue-infer-kind-unknown ()
  (should (eq :unknown
              (arxana-vsatarcs-lifting-queue--infer-kind "random.md"))))

;; ---------------------------------------------------------------------
;; --basename-slug
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-lifting-queue-slug-simple-leaf ()
  (should (equal "2"
                 (arxana-vsatarcs-lifting-queue--basename-slug "leaf-2.md"))))

(ert-deftest arxana-vsatarcs-lifting-queue-slug-nested-leaf ()
  (should (equal "6-4-5"
                 (arxana-vsatarcs-lifting-queue--basename-slug
                  "leaf-6-4-5.md"))))

(ert-deftest arxana-vsatarcs-lifting-queue-slug-devmap ()
  (should (equal "futon3"
                 (arxana-vsatarcs-lifting-queue--basename-slug
                  "devmap-futon3.md"))))

(ert-deftest arxana-vsatarcs-lifting-queue-slug-unknown-keeps-full ()
  (should (equal "random"
                 (arxana-vsatarcs-lifting-queue--basename-slug "random.md"))))

;; ---------------------------------------------------------------------
;; --proposed-id
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-lifting-queue-proposed-id-leaf ()
  (should (equal "arxana/stack/futon-v1/leaf/2"
                 (arxana-vsatarcs-lifting-queue--proposed-id "leaf-2.md"))))

(ert-deftest arxana-vsatarcs-lifting-queue-proposed-id-devmap ()
  (should (equal "arxana/stack/futon-v1/devmap/futon3"
                 (arxana-vsatarcs-lifting-queue--proposed-id
                  "devmap-futon3.md"))))

(ert-deftest arxana-vsatarcs-lifting-queue-proposed-id-respects-custom-stack-id ()
  (let ((arxana-vsatarcs-lifting-queue-stack-id "my/stack"))
    (should (equal "my/stack/leaf/2"
                   (arxana-vsatarcs-lifting-queue--proposed-id "leaf-2.md")))))

;; ---------------------------------------------------------------------
;; --lifted-basenames
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-lifting-queue-lifted-basenames-extraction ()
  (let* ((sample
          (read "(:sections [(:id \"a\" :ref \"futon5a/holes/stories/leaf-2.md\")
                              (:id \"b\" :ref \"futon5a/holes/stories/devmap-futon3.md\")])"))
         ;; Convert the elisp form into the plist shape the shared
         ;; EDN reader would produce (keys are symbols-with-leading-colon).
         (data (list (intern ":sections")
                     (vector (list (intern ":id") "a"
                                   (intern ":ref")
                                   "futon5a/holes/stories/leaf-2.md")
                             (list (intern ":id") "b"
                                   (intern ":ref")
                                   "futon5a/holes/stories/devmap-futon3.md")))))
    (let ((bases (arxana-vsatarcs-lifting-queue--lifted-basenames data)))
      (should (member "leaf-2.md" bases))
      (should (member "devmap-futon3.md" bases))
      (should (= 2 (length bases))))))

(ert-deftest arxana-vsatarcs-lifting-queue-lifted-basenames-empty-data ()
  (should (null (arxana-vsatarcs-lifting-queue--lifted-basenames nil))))

;; ---------------------------------------------------------------------
;; Snapshot integration
;; ---------------------------------------------------------------------

(defconst arxana-vsatarcs-lifting-queue-test--stack-annotations-with-leaf-2
  (concat
   "{:schema-version 1\n"
   " :stack \"futon\"\n"
   " :sections [{:id \"arxana/stack/futon-v1/leaf/2\""
   " :kind :leaf"
   " :name \"Sample\""
   " :ref \"futon5a/holes/stories/leaf-2.md\"}]}\n"))

(ert-deftest arxana-vsatarcs-lifting-queue-snapshot-all-unlifted-when-empty-sa ()
  (arxana-vsatarcs-lifting-queue-test--with-fixture
      '(("leaf-2.md" . "# Sample Leaf 2\n")
        ("devmap-futon3.md" . "# Devmap Three\n"))
      "{:schema-version 1 :sections []}"
    (let ((snap (arxana-vsatarcs-lifting-queue-snapshot)))
      (should (plist-get snap :stack-loaded?))
      (should (= 2 (plist-get snap :total-stories)))
      (should (= 0 (plist-get snap :lifted-count)))
      (should (= 2 (plist-get snap :unlifted-count))))))

(ert-deftest arxana-vsatarcs-lifting-queue-snapshot-filters-lifted ()
  (arxana-vsatarcs-lifting-queue-test--with-fixture
      '(("leaf-2.md" . "# Sample Leaf 2\n")
        ("devmap-futon3.md" . "# Devmap Three\n"))
      arxana-vsatarcs-lifting-queue-test--stack-annotations-with-leaf-2
    (let* ((snap (arxana-vsatarcs-lifting-queue-snapshot))
           (unlifted (plist-get snap :unlifted)))
      (should (= 2 (plist-get snap :total-stories)))
      (should (= 1 (plist-get snap :lifted-count)))
      (should (= 1 (plist-get snap :unlifted-count)))
      ;; The unlifted one is the devmap.
      (should (equal "devmap-futon3.md"
                     (plist-get (car unlifted) :story-basename))))))

(ert-deftest arxana-vsatarcs-lifting-queue-snapshot-payload-shape ()
  (arxana-vsatarcs-lifting-queue-test--with-fixture
      '(("leaf-5-7.md" . "# Sample Title Here\n\nProse."))
      "{:schema-version 1 :sections []}"
    (let* ((snap (arxana-vsatarcs-lifting-queue-snapshot))
           (s (car (plist-get snap :unlifted))))
      (should (equal "leaf-5-7.md" (plist-get s :story-basename)))
      (should (eq :leaf (plist-get s :kind)))
      (should (equal "arxana/stack/futon-v1/leaf/5-7"
                     (plist-get s :proposed-id)))
      (should (equal "Sample Title Here" (plist-get s :proposed-name)))
      (should (equal "futon5a/holes/stories/leaf-5-7.md"
                     (plist-get s :proposed-ref)))
      (should (stringp (plist-get s :mtime))))))

(ert-deftest arxana-vsatarcs-lifting-queue-snapshot-stack-unreadable ()
  (let ((arxana-vsatarcs-lifting-queue-stack-annotations-file
         "/nonexistent.edn")
        (arxana-vsatarcs-lifting-queue-stories-directory
         "/nonexistent-dir/"))
    (let ((snap (arxana-vsatarcs-lifting-queue-snapshot)))
      (should-not (plist-get snap :stack-loaded?))
      (should (= 0 (plist-get snap :total-stories)))
      (should (= 0 (plist-get snap :unlifted-count))))))

(ert-deftest arxana-vsatarcs-lifting-queue-snapshot-excludes-aif-md ()
  ;; `.aif.md` companion files are NOT candidate story files.
  (arxana-vsatarcs-lifting-queue-test--with-fixture
      '(("leaf-2.md" . "# Leaf 2\n")
        ("leaf-2.aif.md" . "# Annotation companion\n"))
      "{:schema-version 1 :sections []}"
    (let ((snap (arxana-vsatarcs-lifting-queue-snapshot)))
      ;; Only leaf-2.md counts as a candidate; leaf-2.aif.md excluded.
      (should (= 1 (plist-get snap :total-stories))))))

(ert-deftest arxana-vsatarcs-lifting-queue-snapshot-kind-counts ()
  (arxana-vsatarcs-lifting-queue-test--with-fixture
      '(("leaf-a.md" . "# A\n")
        ("leaf-b.md" . "# B\n")
        ("devmap-x.md" . "# X\n")
        ("war-machine-y.md" . "# Y\n"))
      "{:schema-version 1 :sections []}"
    (let ((counts (plist-get (arxana-vsatarcs-lifting-queue-snapshot)
                             :kind-counts)))
      (should (= 2 (cdr (assoc :leaf counts))))
      (should (= 1 (cdr (assoc :devmap counts))))
      (should (= 1 (cdr (assoc :war-machine counts)))))))

(ert-deftest arxana-vsatarcs-lifting-queue-snapshot-digest-line ()
  (arxana-vsatarcs-lifting-queue-test--with-fixture
      '(("leaf-a.md" . "# A\n")
        ("devmap-b.md" . "# B\n"))
      arxana-vsatarcs-lifting-queue-test--stack-annotations-with-leaf-2
    (let ((line (plist-get (arxana-vsatarcs-lifting-queue-snapshot)
                           :digest-line)))
      (should (string-match-p "0/2 lifted" line))
      (should (string-match-p "2 in queue" line))
      (should (string-match-p "leaf=1" line))
      (should (string-match-p "devmap=1" line)))))

;; ---------------------------------------------------------------------
;; Live smoke (defensive — only when source files exist)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-lifting-queue-live-smoke ()
  (when (and (file-directory-p
              (expand-file-name "~/code/futon5a/holes/stories/"))
             (file-readable-p
              (expand-file-name "~/code/futon5a/holes/stack-annotations.edn")))
    (let ((snap (arxana-vsatarcs-lifting-queue-snapshot)))
      (should (plist-get snap :stack-loaded?))
      (should (> (plist-get snap :total-stories) 0))
      ;; Per Joe's observation today: 46 of 48 unlifted as of 2026-05-20.
      ;; Don't pin the exact number; just check the gap is real.
      (should (> (plist-get snap :unlifted-count) 0)))))

(provide 'arxana-vsatarcs-lifting-queue-test)
;;; arxana-vsatarcs-lifting-queue-test.el ends here
