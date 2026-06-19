;;; arxana-vsatarcs-sorrys-test.el --- Tests for VSATARCS sorry-registry module -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-sorrys.el' (reader-criterion Q4: V-COV).
;; Covers EDN reading from a temp registry file, summary normalisation
;; (status/kind strip-leading-colon), kind-counts + status-counts
;; aggregation, by-mission filter, and the empty-file degenerate path.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-sorrys)

;; ---------------------------------------------------------------------
;; Test fixtures
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-sorrys-test--write (path body-edn)
  "Write a registry file at PATH wrapping BODY-EDN in :sorrys vector."
  (with-temp-file path
    (insert "{:schema-version 1\n :sorrys [" body-edn "]}\n")))

(defmacro arxana-vsatarcs-sorrys-test--with-registry (body-edn &rest body)
  "Bind the registry file to a temp path holding BODY-EDN; eval BODY."
  (declare (indent 1))
  `(let* ((tmp (make-temp-file "vsatarcs-sorrys-" nil ".edn"))
          (arxana-vsatarcs-sorrys-file tmp))
     (unwind-protect
         (progn
           (arxana-vsatarcs-sorrys-test--write tmp ,body-edn)
           ,@body)
       (when (file-exists-p tmp) (delete-file tmp)))))

(defconst arxana-vsatarcs-sorrys-test--meta
  (concat
   "{:id :sorry/meta-one\n"
   " :title \"Meta sorry\"\n"
   " :raised-at \"2026-05-17\"\n"
   " :status :open\n"
   " :kind :meta\n"
   " :related-missions [\"M-war-machine-aif-completion\"]}\n"))

(defconst arxana-vsatarcs-sorrys-test--pf-1
  (concat
   "{:id :sorry/pf-channel-1\n"
   " :title \"Channel 1 likelihood\"\n"
   " :raised-at \"2026-05-19\"\n"
   " :status :open\n"
   " :kind :prototyping-forward\n"
   " :related-missions [\"M-war-machine-aif-completion\"]}\n"))

(defconst arxana-vsatarcs-sorrys-test--pf-2
  (concat
   "{:id :sorry/pf-channel-2\n"
   " :title \"Channel 2 likelihood\"\n"
   " :raised-at \"2026-05-19\"\n"
   " :status :addressed\n"
   " :kind :prototyping-forward\n"
   " :related-missions [\"M-war-machine-aif-completion\" \"M-stack-essay-code-alignment\"]}\n"))

(defconst arxana-vsatarcs-sorrys-test--td-1
  (concat
   "{:id :sorry/td-thing\n"
   " :title \"Technical-debt sample\"\n"
   " :raised-at \"2026-05-15\"\n"
   " :status :foreclosed\n"
   " :kind :technical-debt\n"
   " :related-missions [\"M-other\"]}\n"))

(defconst arxana-vsatarcs-sorrys-test--no-kind
  (concat
   "{:id :sorry/uncategorised\n"
   " :title \"No kind annotated\"\n"
   " :raised-at \"2026-05-10\"\n"
   " :status :open\n"
   " :related-missions [\"M-foo\"]}\n"))

;; ---------------------------------------------------------------------
;; Schema-stability tests
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-sorrys-known-kinds-are-keywords ()
  (should (cl-every #'keywordp arxana-vsatarcs-sorrys-known-kinds)))

(ert-deftest arxana-vsatarcs-sorrys-known-kinds-no-duplicates ()
  (should (= (length arxana-vsatarcs-sorrys-known-kinds)
             (length (delete-dups (copy-sequence
                                   arxana-vsatarcs-sorrys-known-kinds))))))

(ert-deftest arxana-vsatarcs-sorrys-known-kinds-match-schema ()
  ;; v2 schema declares exactly these 5 kinds.
  (should (equal arxana-vsatarcs-sorrys-known-kinds
                 '(:meta :prototyping-forward :technical-debt
                         :decision-debt :external-dependency))))

;; ---------------------------------------------------------------------
;; Snapshot integration
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-sorrys-snapshot-empty-file-graceful ()
  (let ((arxana-vsatarcs-sorrys-file "/nonexistent/path.edn"))
    (let ((snap (arxana-vsatarcs-sorrys-snapshot)))
      (should (not (plist-get snap :registry-loaded?)))
      (should (= 0 (plist-get snap :total)))
      (should (equal '() (plist-get snap :sorrys)))
      ;; kind-counts row is stable even when empty
      (should (= (1+ (length arxana-vsatarcs-sorrys-known-kinds))
                 (length (plist-get snap :kind-counts)))))))

(ert-deftest arxana-vsatarcs-sorrys-snapshot-single-meta ()
  (arxana-vsatarcs-sorrys-test--with-registry
      arxana-vsatarcs-sorrys-test--meta
    (let* ((snap (arxana-vsatarcs-sorrys-snapshot))
           (sorrys (plist-get snap :sorrys)))
      (should (plist-get snap :registry-loaded?))
      (should (= 1 (plist-get snap :total)))
      (let ((s (car sorrys)))
        ;; Note: shared EDN reader returns the bare-name symbol for keywords
        ;; (e.g., `:sorry/meta-one' becomes a symbol). Just compare names.
        (should (string= ":sorry/meta-one" (symbol-name (plist-get s :id))))
        (should (equal "Meta sorry" (plist-get s :title)))
        (should (equal :open (plist-get s :status)))
        (should (equal :meta (plist-get s :kind)))
        (should (equal "2026-05-17" (plist-get s :raised-at)))
        (should (equal '("M-war-machine-aif-completion")
                       (plist-get s :related-missions)))))))

(ert-deftest arxana-vsatarcs-sorrys-snapshot-preserves-order ()
  (arxana-vsatarcs-sorrys-test--with-registry
      (concat arxana-vsatarcs-sorrys-test--meta
              arxana-vsatarcs-sorrys-test--pf-1
              arxana-vsatarcs-sorrys-test--pf-2)
    (let* ((snap (arxana-vsatarcs-sorrys-snapshot))
           (ids (mapcar (lambda (s) (symbol-name (plist-get s :id)))
                        (plist-get snap :sorrys))))
      ;; Registry order is meaningful; do not alphabetise.
      (should (equal '(":sorry/meta-one"
                       ":sorry/pf-channel-1"
                       ":sorry/pf-channel-2")
                     ids)))))

(ert-deftest arxana-vsatarcs-sorrys-kind-counts-distribution ()
  (arxana-vsatarcs-sorrys-test--with-registry
      (concat arxana-vsatarcs-sorrys-test--meta
              arxana-vsatarcs-sorrys-test--pf-1
              arxana-vsatarcs-sorrys-test--pf-2
              arxana-vsatarcs-sorrys-test--td-1)
    (let* ((snap (arxana-vsatarcs-sorrys-snapshot))
           (counts (plist-get snap :kind-counts)))
      (should (= 1 (cdr (assoc :meta counts))))
      (should (= 2 (cdr (assoc :prototyping-forward counts))))
      (should (= 1 (cdr (assoc :technical-debt counts))))
      (should (= 0 (cdr (assoc :decision-debt counts))))
      (should (= 0 (cdr (assoc :external-dependency counts))))
      (should (= 0 (cdr (assoc 'unknown counts)))))))

(ert-deftest arxana-vsatarcs-sorrys-kind-counts-unknown-bucket ()
  (arxana-vsatarcs-sorrys-test--with-registry
      arxana-vsatarcs-sorrys-test--no-kind
    (let* ((snap (arxana-vsatarcs-sorrys-snapshot))
           (counts (plist-get snap :kind-counts)))
      ;; A sorry without :kind falls into the `unknown' bucket.
      (should (= 1 (cdr (assoc 'unknown counts))))
      (should (= 0 (cdr (assoc :meta counts)))))))

(ert-deftest arxana-vsatarcs-sorrys-status-counts ()
  (arxana-vsatarcs-sorrys-test--with-registry
      (concat arxana-vsatarcs-sorrys-test--meta       ; open
              arxana-vsatarcs-sorrys-test--pf-1       ; open
              arxana-vsatarcs-sorrys-test--pf-2       ; addressed
              arxana-vsatarcs-sorrys-test--td-1)      ; foreclosed
    (let* ((snap (arxana-vsatarcs-sorrys-snapshot))
           (counts (plist-get snap :status-counts)))
      (should (= 2 (cdr (assoc :open counts))))
      (should (= 1 (cdr (assoc :addressed counts))))
      (should (= 1 (cdr (assoc :foreclosed counts))))
      (should (= 0 (cdr (assoc 'other counts)))))))

(ert-deftest arxana-vsatarcs-sorrys-kind-row-shape-stable-when-empty ()
  ;; Even with one sorry of a single kind, the row carries entries for
  ;; every declared kind plus the `unknown' bucket — V-COM property.
  (arxana-vsatarcs-sorrys-test--with-registry
      arxana-vsatarcs-sorrys-test--meta
    (let* ((snap (arxana-vsatarcs-sorrys-snapshot))
           (counts (plist-get snap :kind-counts)))
      (dolist (k arxana-vsatarcs-sorrys-known-kinds)
        (should (assoc k counts)))
      (should (assoc 'unknown counts)))))

;; ---------------------------------------------------------------------
;; By-mission filter
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-sorrys-by-mission-single-match ()
  (arxana-vsatarcs-sorrys-test--with-registry
      (concat arxana-vsatarcs-sorrys-test--meta
              arxana-vsatarcs-sorrys-test--pf-1
              arxana-vsatarcs-sorrys-test--td-1)
    (let ((wm (arxana-vsatarcs-sorrys-by-mission "M-war-machine-aif-completion"))
          (other (arxana-vsatarcs-sorrys-by-mission "M-other")))
      (should (= 2 (length wm)))
      (should (= 1 (length other)))
      (should (equal "M-other"
                     (car (plist-get (car other) :related-missions)))))))

(ert-deftest arxana-vsatarcs-sorrys-by-mission-cross-listed ()
  ;; pf-channel-2 lists both WM and stack-essay-code-alignment.
  (arxana-vsatarcs-sorrys-test--with-registry
      (concat arxana-vsatarcs-sorrys-test--meta
              arxana-vsatarcs-sorrys-test--pf-2)
    (let ((wm (arxana-vsatarcs-sorrys-by-mission "M-war-machine-aif-completion"))
          (align (arxana-vsatarcs-sorrys-by-mission "M-stack-essay-code-alignment")))
      (should (= 2 (length wm)))  ; both reference WM
      (should (= 1 (length align))))))

(ert-deftest arxana-vsatarcs-sorrys-by-mission-no-match ()
  (arxana-vsatarcs-sorrys-test--with-registry
      arxana-vsatarcs-sorrys-test--meta
    (should (null (arxana-vsatarcs-sorrys-by-mission "M-nonexistent")))))

;; ---------------------------------------------------------------------
;; Defensive: normalisation of keyword statuses/kinds
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-sorrys-normalises-status-keyword ()
  (arxana-vsatarcs-sorrys-test--with-registry
      arxana-vsatarcs-sorrys-test--meta
    (let* ((snap (arxana-vsatarcs-sorrys-snapshot))
           (s (car (plist-get snap :sorrys))))
      ;; Status is a bare keyword (no double-prefixed leading colon).
      (should (keywordp (plist-get s :status))))))

(ert-deftest arxana-vsatarcs-sorrys-normalises-kind-keyword ()
  (arxana-vsatarcs-sorrys-test--with-registry
      arxana-vsatarcs-sorrys-test--meta
    (let* ((snap (arxana-vsatarcs-sorrys-snapshot))
           (s (car (plist-get snap :sorrys))))
      (should (keywordp (plist-get s :kind))))))

;; ---------------------------------------------------------------------
;; Mission-mention scanning (story-scoped filtering helper)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-sorrys-missions-in-empty-text ()
  (should (null (arxana-vsatarcs-sorrys-missions-mentioned-in ""))))

(ert-deftest arxana-vsatarcs-sorrys-missions-in-nil-text ()
  (should (null (arxana-vsatarcs-sorrys-missions-mentioned-in nil))))

(ert-deftest arxana-vsatarcs-sorrys-missions-in-no-match ()
  (should (null (arxana-vsatarcs-sorrys-missions-mentioned-in
                 "Just some prose with no mission refs."))))

(ert-deftest arxana-vsatarcs-sorrys-missions-in-single ()
  (let ((refs (arxana-vsatarcs-sorrys-missions-mentioned-in
               "See M-war-machine-aif-completion for context.")))
    (should (equal '("M-war-machine-aif-completion") refs))))

(ert-deftest arxana-vsatarcs-sorrys-missions-in-multiple-deduped ()
  ;; Duplicates collapse; first-occurrence order is preserved.
  (let ((refs (arxana-vsatarcs-sorrys-missions-mentioned-in
               "M-stack-essay-code-alignment relates to M-war-machine-aif-completion;
                see also M-stack-essay-code-alignment §2.")))
    (should (equal '("M-stack-essay-code-alignment"
                     "M-war-machine-aif-completion")
                   refs))))

(ert-deftest arxana-vsatarcs-sorrys-missions-in-rejects-non-mission ()
  ;; `M-INC' is not a mission file basename per the convention.  But
  ;; our regex would naively match it.  Document the limitation rather
  ;; than over-engineer: convention is `M-[kebab-name]'; M-INC violates
  ;; but matches.  This test guards the current behaviour so a future
  ;; tightening is intentional.
  (let ((refs (arxana-vsatarcs-sorrys-missions-mentioned-in
               "M-INC step (b) is pending.")))
    (should (equal '("M-INC") refs))))

;; ---------------------------------------------------------------------
;; Story-scoped snapshot
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-sorrys-snapshot-for-text-no-mention ()
  ;; No mission mention → falls back to global snapshot with :scoped? nil.
  (arxana-vsatarcs-sorrys-test--with-registry
      (concat arxana-vsatarcs-sorrys-test--meta
              arxana-vsatarcs-sorrys-test--pf-1)
    (let ((snap (arxana-vsatarcs-sorrys-snapshot-for-text
                 "Some story body with no mission refs.")))
      (should (= 2 (plist-get snap :total)))
      (should-not (plist-get snap :scoped?))
      (should (null (plist-get snap :scope-missions))))))

(ert-deftest arxana-vsatarcs-sorrys-snapshot-for-text-with-mention ()
  ;; Mission mention matching the fixture's :related-missions → filter
  ;; activates; :scoped? = t.
  (arxana-vsatarcs-sorrys-test--with-registry
      (concat arxana-vsatarcs-sorrys-test--meta
              arxana-vsatarcs-sorrys-test--pf-1
              arxana-vsatarcs-sorrys-test--td-1)  ;; references M-other
    (let ((snap (arxana-vsatarcs-sorrys-snapshot-for-text
                 "Discussing M-war-machine-aif-completion progress.")))
      (should (plist-get snap :scoped?))
      (should (equal '("M-war-machine-aif-completion")
                     (plist-get snap :scope-missions)))
      ;; meta + pf-1 reference WM; td-1 references M-other → filtered out.
      (should (= 2 (plist-get snap :total))))))

(ert-deftest arxana-vsatarcs-sorrys-snapshot-for-text-no-overlap-fallback ()
  ;; Mission mentioned but no sorrys reference it → fall back to
  ;; global with :scoped? nil and :scope-missions documented.
  (arxana-vsatarcs-sorrys-test--with-registry
      arxana-vsatarcs-sorrys-test--meta
    (let ((snap (arxana-vsatarcs-sorrys-snapshot-for-text
                 "Discussing M-non-existent-mission.")))
      (should-not (plist-get snap :scoped?))
      (should (equal '("M-non-existent-mission")
                     (plist-get snap :scope-missions)))
      ;; Falls back to global → meta sorry still surfaces.
      (should (= 1 (plist-get snap :total))))))

(ert-deftest arxana-vsatarcs-sorrys-snapshot-for-text-multi-mission ()
  ;; Multiple missions mentioned in text → union filter (any matching
  ;; sorry passes).  pf-2 cross-references both WM and alignment.
  (arxana-vsatarcs-sorrys-test--with-registry
      (concat arxana-vsatarcs-sorrys-test--meta
              arxana-vsatarcs-sorrys-test--pf-2)
    (let ((snap (arxana-vsatarcs-sorrys-snapshot-for-text
                 "Cross-cutting concern across M-war-machine-aif-completion
                  and M-stack-essay-code-alignment.")))
      (should (plist-get snap :scoped?))
      (should (= 2 (length (plist-get snap :scope-missions))))
      (should (= 2 (plist-get snap :total))))))

(provide 'arxana-vsatarcs-sorrys-test)
;;; arxana-vsatarcs-sorrys-test.el ends here
