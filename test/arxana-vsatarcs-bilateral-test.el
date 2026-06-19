;;; arxana-vsatarcs-bilateral-test.el --- Tests for VSATARCS bilateral-evidence -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for `arxana-vsatarcs-bilateral.el' (reader-criterion Q7:
;; V-COV).  Covers EDN reading from a temp .aif.edn carrying a
;; `:bilateral-evidence' block, summary normalisation, kind-counts
;; aggregation across the closed set, witness-count, and the
;; by-kind filter.

;;; Code:

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-bilateral)

;; ---------------------------------------------------------------------
;; Test fixtures
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-bilateral-test--write (path body-edn)
  "Write a minimal .aif.edn at PATH wrapping BODY-EDN in :bilateral-evidence."
  (with-temp-file path
    (insert "{:schema-version 1\n :bilateral-evidence [" body-edn "]}\n")))

(defmacro arxana-vsatarcs-bilateral-test--with-aif (body-edn &rest body)
  "Bind the .aif.edn file to a temp path holding BODY-EDN; eval BODY."
  (declare (indent 1))
  `(let* ((tmp (make-temp-file "vsatarcs-bilateral-" nil ".edn"))
          (arxana-vsatarcs-bilateral-aif-file tmp))
     (unwind-protect
         (progn
           (arxana-vsatarcs-bilateral-test--write tmp ,body-edn)
           ,@body)
       (when (file-exists-p tmp) (delete-file tmp)))))

(defconst arxana-vsatarcs-bilateral-test--entry-1
  (concat
   "{:vsatarcs-id \"hx:vsatarcs-align:v0-2-3:enables-renderer-closure\"\n"
   " :wm-id \"futon-aif-completeness.md §Capability-gap-modeling\"\n"
   " :principle :capability-gap-modeling\n"
   " :evidence-kind :independent-naming-of-same-principle\n"
   " :landed \"2026-05-17\"\n"
   " :note \"Same principle named on both sides without coordination.\"}\n"))

(defconst arxana-vsatarcs-bilateral-test--entry-2
  (concat
   "{:vsatarcs-id \"hx:vsatarcs-align:v0-2-5:cross-side-bridge-closure\"\n"
   " :wm-id \"hx:wm:v0-9:symmetric-bootstrap-closure\"\n"
   " :principle :symmetric-bootstrap-of-shared-entity-domain\n"
   " :evidence-kind :joint-landing\n"
   " :landed \"2026-05-18\"\n"
   " :note \"Coordinated bilateral milestone via bell/whistle.\"}\n"))

(defconst arxana-vsatarcs-bilateral-test--entry-3-with-witnesses
  (concat
   "{:vsatarcs-id \"hx:vsatarcs-align:v0-5-0:r3a-likelihood-closure\"\n"
   " :wm-id \"futon-aif-completeness.md §R3 v0.10\"\n"
   " :principle :r3a-likelihood-via-ants-port\n"
   " :evidence-kind :joint-landing\n"
   " :landed \"2026-05-19\"\n"
   " :landed-vsatarcs \"2026-05-19\"\n"
   " :landed-wm \"2026-05-18\"\n"
   " :note \"R3a + R3c land via port of WM-side likelihood architecture.\"\n"
   " :protocol-witnesses [{:turn 1 :actor :claude-4 :gist \"cue\"}\n"
   "                      {:turn 2 :actor :claude-2 :gist \"ack\"}\n"
   "                      {:turn 3 :actor :claude-4 :gist \"landed\"}]}\n"))

(defconst arxana-vsatarcs-bilateral-test--entry-4-one-sided
  (concat
   "{:vsatarcs-id \"hx:vsatarcs-align:v0-5-1:r9-f-decrease-closure\"\n"
   " :wm-id \"futon2/test/r9_named_validation_test.clj\"\n"
   " :principle :r9-f-decrease-named-property\n"
   " :evidence-kind :one-sided-extension\n"
   " :landed \"2026-05-19\"\n"
   " :landed-vsatarcs \"2026-05-19\"\n"
   " :landed-wm \"earlier\"\n"
   " :note \"R9 F-decrease lands on VSATARCs sequentially; WM already had it.\"}\n"))

;; ---------------------------------------------------------------------
;; Schema-stability tests
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-bilateral-evidence-kinds-are-keywords ()
  (should (cl-every #'keywordp arxana-vsatarcs-bilateral-evidence-kinds)))

(ert-deftest arxana-vsatarcs-bilateral-evidence-kinds-no-duplicates ()
  (should (= (length arxana-vsatarcs-bilateral-evidence-kinds)
             (length (delete-dups
                      (copy-sequence
                       arxana-vsatarcs-bilateral-evidence-kinds))))))

(ert-deftest arxana-vsatarcs-bilateral-evidence-kinds-contains-active-set ()
  ;; All seven kinds active in the closed set as of v0.5.32 — the sixth
  ;; (`:consent-gated-writer-event') activated 2026-05-20 (claude-2 L4);
  ;; the seventh (`:symmetric-apparatus-port') activated 2026-05-21
  ;; (claude-9 + claude-4 R12 narrow-take-up bilateral close).
  (dolist (k '(:independent-naming-of-same-principle
               :joint-landing
               :independent-naming-of-same-r-criterion-shape-at-different-scopes
               :one-sided-extension
               :coordinated-empirical-observation
               :consent-gated-writer-event
               :symmetric-apparatus-port))
    (should (memq k arxana-vsatarcs-bilateral-evidence-kinds))))

(ert-deftest arxana-vsatarcs-bilateral-evidence-kinds-count ()
  ;; Closed-set cardinality guard: v0.5.32 = 7.  Adding/removing kinds
  ;; is a closure-worthy schema change so this test surfaces the count
  ;; shift intentionally.
  (should (= 7 (length arxana-vsatarcs-bilateral-evidence-kinds))))

;; ---------------------------------------------------------------------
;; Snapshot integration
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-bilateral-snapshot-empty-file-graceful ()
  (let ((arxana-vsatarcs-bilateral-aif-file "/nonexistent/path.edn"))
    (let ((snap (arxana-vsatarcs-bilateral-snapshot)))
      (should (not (plist-get snap :block-loaded?)))
      (should (= 0 (plist-get snap :total)))
      (should (equal '() (plist-get snap :entries)))
      ;; Even with no entries, kind-counts row has stable shape.
      (should (= (1+ (length arxana-vsatarcs-bilateral-evidence-kinds))
                 (length (plist-get snap :kind-counts)))))))

(ert-deftest arxana-vsatarcs-bilateral-snapshot-single-entry ()
  (arxana-vsatarcs-bilateral-test--with-aif
      arxana-vsatarcs-bilateral-test--entry-1
    (let* ((snap (arxana-vsatarcs-bilateral-snapshot))
           (e (car (plist-get snap :entries))))
      (should (plist-get snap :block-loaded?))
      (should (= 1 (plist-get snap :total)))
      (should (string= "hx:vsatarcs-align:v0-2-3:enables-renderer-closure"
                       (plist-get e :vsatarcs-id)))
      (should (eq :capability-gap-modeling (plist-get e :principle)))
      (should (eq :independent-naming-of-same-principle
                  (plist-get e :evidence-kind)))
      (should (equal "2026-05-17" (plist-get e :landed)))
      (should (not (plist-get e :has-protocol-witnesses?))))))

(ert-deftest arxana-vsatarcs-bilateral-snapshot-preserves-order ()
  (arxana-vsatarcs-bilateral-test--with-aif
      (concat arxana-vsatarcs-bilateral-test--entry-1
              arxana-vsatarcs-bilateral-test--entry-2
              arxana-vsatarcs-bilateral-test--entry-3-with-witnesses)
    (let* ((snap (arxana-vsatarcs-bilateral-snapshot))
           (ids (mapcar (lambda (e) (plist-get e :vsatarcs-id))
                        (plist-get snap :entries))))
      ;; Source order: entry-1, entry-2, entry-3.
      (should (equal '("hx:vsatarcs-align:v0-2-3:enables-renderer-closure"
                       "hx:vsatarcs-align:v0-2-5:cross-side-bridge-closure"
                       "hx:vsatarcs-align:v0-5-0:r3a-likelihood-closure")
                     ids)))))

(ert-deftest arxana-vsatarcs-bilateral-kind-counts-distribution ()
  (arxana-vsatarcs-bilateral-test--with-aif
      (concat arxana-vsatarcs-bilateral-test--entry-1       ; independent-naming
              arxana-vsatarcs-bilateral-test--entry-2       ; joint-landing
              arxana-vsatarcs-bilateral-test--entry-3-with-witnesses ; joint-landing
              arxana-vsatarcs-bilateral-test--entry-4-one-sided) ; one-sided
    (let* ((snap (arxana-vsatarcs-bilateral-snapshot))
           (counts (plist-get snap :kind-counts)))
      (should (= 1 (cdr (assoc :independent-naming-of-same-principle counts))))
      (should (= 2 (cdr (assoc :joint-landing counts))))
      (should (= 0 (cdr (assoc
                         :independent-naming-of-same-r-criterion-shape-at-different-scopes
                         counts))))
      (should (= 1 (cdr (assoc :one-sided-extension counts))))
      (should (= 0 (cdr (assoc :coordinated-empirical-observation counts))))
      (should (= 0 (cdr (assoc 'unknown counts)))))))

(ert-deftest arxana-vsatarcs-bilateral-kind-row-shape-stable ()
  ;; With one entry of a single kind, all declared kinds + unknown
  ;; still appear in the row — V-COM property.
  (arxana-vsatarcs-bilateral-test--with-aif
      arxana-vsatarcs-bilateral-test--entry-1
    (let* ((snap (arxana-vsatarcs-bilateral-snapshot))
           (counts (plist-get snap :kind-counts)))
      (dolist (k arxana-vsatarcs-bilateral-evidence-kinds)
        (should (assoc k counts)))
      (should (assoc 'unknown counts)))))

(ert-deftest arxana-vsatarcs-bilateral-witness-count ()
  (arxana-vsatarcs-bilateral-test--with-aif
      (concat arxana-vsatarcs-bilateral-test--entry-1
              arxana-vsatarcs-bilateral-test--entry-2
              arxana-vsatarcs-bilateral-test--entry-3-with-witnesses)
    (let ((snap (arxana-vsatarcs-bilateral-snapshot)))
      (should (= 1 (plist-get snap :witness-count))))))

(ert-deftest arxana-vsatarcs-bilateral-witness-count-no-witnesses ()
  (arxana-vsatarcs-bilateral-test--with-aif
      arxana-vsatarcs-bilateral-test--entry-1
    (let ((snap (arxana-vsatarcs-bilateral-snapshot)))
      (should (= 0 (plist-get snap :witness-count))))))

(ert-deftest arxana-vsatarcs-bilateral-landed-asymmetric-fields ()
  ;; entry-3 carries :landed, :landed-vsatarcs, :landed-wm.
  (arxana-vsatarcs-bilateral-test--with-aif
      arxana-vsatarcs-bilateral-test--entry-3-with-witnesses
    (let* ((snap (arxana-vsatarcs-bilateral-snapshot))
           (e (car (plist-get snap :entries))))
      (should (equal "2026-05-19" (plist-get e :landed)))
      (should (equal "2026-05-19" (plist-get e :landed-vsatarcs)))
      (should (equal "2026-05-18" (plist-get e :landed-wm))))))

;; ---------------------------------------------------------------------
;; By-kind filter
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-bilateral-by-kind-joint-landing ()
  (arxana-vsatarcs-bilateral-test--with-aif
      (concat arxana-vsatarcs-bilateral-test--entry-1
              arxana-vsatarcs-bilateral-test--entry-2
              arxana-vsatarcs-bilateral-test--entry-3-with-witnesses
              arxana-vsatarcs-bilateral-test--entry-4-one-sided)
    (let ((joint (arxana-vsatarcs-bilateral-by-kind :joint-landing))
          (independent (arxana-vsatarcs-bilateral-by-kind
                        :independent-naming-of-same-principle))
          (oneside (arxana-vsatarcs-bilateral-by-kind :one-sided-extension)))
      (should (= 2 (length joint)))
      (should (= 1 (length independent)))
      (should (= 1 (length oneside))))))

(ert-deftest arxana-vsatarcs-bilateral-by-kind-no-match ()
  (arxana-vsatarcs-bilateral-test--with-aif
      arxana-vsatarcs-bilateral-test--entry-1
    (should (null (arxana-vsatarcs-bilateral-by-kind
                   :coordinated-empirical-observation)))))

;; ---------------------------------------------------------------------
;; Live .aif.edn smoke (defensive — only assert structure when readable)
;; ---------------------------------------------------------------------

(ert-deftest arxana-vsatarcs-bilateral-live-aif-smoke ()
  ;; When the actual contract file is on disk (CI may or may not have it),
  ;; verify the snapshot shape parses without error and yields entries.
  (when (file-readable-p
         (expand-file-name "~/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn"))
    (let* ((arxana-vsatarcs-bilateral-aif-file
            (expand-file-name
             "~/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn"))
           (snap (arxana-vsatarcs-bilateral-snapshot)))
      (should (plist-get snap :block-loaded?))
      ;; At least the 8 entries known to exist as of v0.5.9.
      (should (>= (plist-get snap :total) 8)))))

(provide 'arxana-vsatarcs-bilateral-test)
;;; arxana-vsatarcs-bilateral-test.el ends here
