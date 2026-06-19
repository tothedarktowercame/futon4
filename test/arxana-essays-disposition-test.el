;;; arxana-essays-disposition-test.el --- Native annotation-disposition + section ops -*- lexical-binding: t; -*-

;; Exercises the Arxana Essays editing loop end-to-end against a throwaway
;; fixture essay with the same structural shape as a real paper: a source
;; markdown, an authoritative annotations.edn (vector of annotations +
;; sections), and a derived .el manifest.  Steps through the operations that
;; were previously done by hand / via external python:
;;
;;   resolve-with-discharge  → :closure t :discharge-procedure "..."
;;   kill (not-relevant)     → :retracted t :retraction-kind :not-relevant
;;   re-home an annotation   → :section-id rewritten to another section
;;   remove a section        → section map dropped from :sections vector
;;   regenerate the .el      → derived manifest re-synced from the .edn
;;
;; Every assertion is on the post-write .edn parsed back via the project's
;; own EDN reader, so the test proves the textual writers produce
;; parseable, semantically-correct EDN — the thing the python work-arounds
;; were standing in for.

(require 'ert)
(require 'cl-lib)
(require 'arxana-test-support)

(add-to-list 'load-path (expand-file-name "dev" default-directory))

(arxana-test--ensure-tangled-loaded)

;; The essays browser soft-loads its compiled-view surface, which hard-
;; requires `hydra' (a UI-only dependency).  This test exercises the EDN
;; section/annotation writers, not the hydra UI, so provide a minimal
;; `hydra' stub when the package is absent (batch/CI) to let the module
;; load.  `defhydra' just needs to be a no-op-ish macro here.
(unless (require 'hydra nil t)
  (defmacro defhydra (name body &rest heads)
    "Test stub for `defhydra' — defines a no-op `NAME/body' command."
    (ignore body heads)
    `(defun ,(intern (format "%s/body" name)) () (interactive) nil))
  (provide 'hydra))

(require 'arxana-browser-essays)
(require 'arxana-browser-rewrites)

(defconst arxana-essays-disposition-test--eid
  "arxana/essay/demo-disposition-fixture")

(defun arxana-essays-disposition-test--sid (slug)
  (concat arxana-essays-disposition-test--eid "/section/" slug))

(defun arxana-essays-disposition-test--write-fixture (dir)
  "Write a fixture essay (source .md + annotations.edn) under DIR.
Returns the path to the annotations.edn."
  (let ((md (expand-file-name "demo.md" dir))
        (edn (expand-file-name "annotations.edn" dir)))
    (with-temp-file md
      (insert "# Demo paper\n\n"
              "## Abstract\n\nThis is the abstract. It addresses questions.\n\n"
              "## Research questions\n\nThe questions are as follows.\n\n"
              "## Methods\n\nWe did the methods, carefully and well.\n\n"
              "## Summary\n\nIn summary, the work is done.\n"))
    (with-temp-file edn
      (insert ";; demo fixture --- authoritative annotation layer\n\n"
              "{:version 2\n"
              " :essay-id \"" arxana-essays-disposition-test--eid "\"\n"
              " :source-file \"demo.md\"\n\n"
              " :sections\n"
              " [{:id \"" (arxana-essays-disposition-test--sid "abstract")
              "\" :name \"Abstract\" :index [1] :heading-level 2}\n"
              "  {:id \"" (arxana-essays-disposition-test--sid "research-questions")
              "\" :name \"Research questions\" :index [2] :heading-level 2}\n"
              "  {:id \"" (arxana-essays-disposition-test--sid "methods")
              "\" :name \"Methods\" :index [3] :heading-level 2}\n"
              "  {:id \"" (arxana-essays-disposition-test--sid "summary")
              "\" :name \"Summary\" :index [4] :heading-level 2}]\n\n"
              " :annotations\n"
              " [{:id \"hx:demo:rqs-early\"\n"
              "   :hx-type :writing/critique\n"
              "   :section-id \"" (arxana-essays-disposition-test--sid "research-questions") "\"\n"
              "   :passage \"The questions are as follows.\"\n"
              "   :patterns [\"plos-npt-with-small-n/research-questions-stated-early\"]\n"
              "   :severity :medium :status :open :polarity :fit\n"
              "   :suggestion \"Move RQs into the abstract.\"}\n\n"
              "  {:id \"hx:demo:methods-fix\"\n"
              "   :hx-type :writing/critique\n"
              "   :section-id \"" (arxana-essays-disposition-test--sid "methods") "\"\n"
              "   :passage \"We did the methods, carefully and well.\"\n"
              "   :patterns [\"plos-npt-with-small-n/site-case-data-display\"]\n"
              "   :severity :high :status :open :polarity :friction\n"
              "   :suggestion \"Add a study-map table.\"}\n\n"
              "  {:id \"hx:demo:bogus\"\n"
              "   :hx-type :writing/critique\n"
              "   :section-id \"" (arxana-essays-disposition-test--sid "summary") "\"\n"
              "   :passage \"In summary, the work is done.\"\n"
              "   :patterns [\"plos-npt-with-small-n/abstract-n-method-frame\"]\n"
              "   :severity :low :status :open :polarity :friction\n"
              "   :suggestion \"This one is not relevant.\"}]}\n"))
    edn))

(defun arxana-essays-disposition-test--ann (edn id)
  "Return annotation plist with :id ID from EDN file, parsed natively."
  (let* ((data (arxana-browser-rewrites--read-edn-file edn))
         (anns (append (plist-get data :annotations) nil)))
    (cl-find-if (lambda (a) (string= (plist-get a :id) id)) anns)))

(defun arxana-essays-disposition-test--section-ids (edn)
  (let* ((data (arxana-browser-rewrites--read-edn-file edn))
         (secs (append (plist-get data :sections) nil)))
    (mapcar (lambda (s) (plist-get s :id)) secs)))

(ert-deftest arxana-essays-disposition-resolve-with-discharge ()
  "resolve writes :closure + :discharge-procedure, parseably."
  (let ((dir (make-temp-file "arxana-disp" t)))
    (unwind-protect
        (let ((edn (arxana-essays-disposition-test--write-fixture dir)))
          (should (arxana-browser-essays--mark-manifest-annotation-closed
                   edn "hx:demo:methods-fix" "2026-01-01T00:00:00+0000"
                   "Added the study-map table."))
          (let ((a (arxana-essays-disposition-test--ann edn "hx:demo:methods-fix")))
            (should (eq t (plist-get a :closure)))
            (should (string= "Added the study-map table."
                             (plist-get a :discharge-procedure))))
          ;; idempotent: second resolve is a no-op
          (should-not (arxana-browser-essays--mark-manifest-annotation-closed
                       edn "hx:demo:methods-fix" "x" "y")))
      (delete-directory dir t))))

(ert-deftest arxana-essays-disposition-kill-not-relevant ()
  "kill writes :retracted + :retraction-kind, parseably."
  (let ((dir (make-temp-file "arxana-disp" t)))
    (unwind-protect
        (let ((edn (arxana-essays-disposition-test--write-fixture dir)))
          (should (arxana-browser-essays--mark-manifest-annotation-killed
                   edn "hx:demo:bogus" :not-relevant "2026-01-01T00:00:00+0000"))
          (let ((a (arxana-essays-disposition-test--ann edn "hx:demo:bogus")))
            (should (eq t (plist-get a :retracted)))
            (should (eq :not-relevant (plist-get a :retraction-kind)))))
      (delete-directory dir t))))

(ert-deftest arxana-essays-disposition-rehome-and-remove-section ()
  "Re-home an annotation to another section, then drop the now-empty section.
This is the exact flow used to move the RQs into the Abstract and delete
the standalone Research questions section."
  (let ((dir (make-temp-file "arxana-disp" t)))
    (unwind-protect
        (let ((edn (arxana-essays-disposition-test--write-fixture dir))
              (rq-sec (arxana-essays-disposition-test--sid "research-questions"))
              (abs-sec (arxana-essays-disposition-test--sid "abstract")))
          ;; re-home rqs-early from research-questions -> abstract, new passage
          (should (arxana-browser-essays--rehome-edn-annotation
                   edn "hx:demo:rqs-early" abs-sec "It addresses questions."))
          (let ((a (arxana-essays-disposition-test--ann edn "hx:demo:rqs-early")))
            (should (string= abs-sec (plist-get a :section-id)))
            (should (string= "It addresses questions." (plist-get a :passage))))
          ;; now remove the research-questions section
          (should (member rq-sec (arxana-essays-disposition-test--section-ids edn)))
          (should (arxana-browser-essays--remove-edn-section edn rq-sec))
          (should-not (member rq-sec (arxana-essays-disposition-test--section-ids edn)))
          ;; other sections survive; annotation count unchanged (re-homed, not deleted)
          (should (= 3 (length (arxana-essays-disposition-test--section-ids edn))))
          (let* ((data (arxana-browser-rewrites--read-edn-file edn)))
            (should (= 3 (length (append (plist-get data :annotations) nil)))))
          ;; removing a non-existent section is a no-op
          (should-not (arxana-browser-essays--remove-edn-section edn "no/such/section")))
      (delete-directory dir t))))

(ert-deftest arxana-essays-disposition-regenerate-el ()
  "Regenerated .el reflects post-surgery sections and parses as elisp."
  (let ((dir (make-temp-file "arxana-disp" t)))
    (unwind-protect
        (let ((edn (arxana-essays-disposition-test--write-fixture dir))
              (el  (expand-file-name "annotations.el" dir)))
          (arxana-browser-essays--remove-edn-section
           edn (arxana-essays-disposition-test--sid "research-questions"))
          (arxana-browser-essays--regenerate-el-from-edn edn el "Demo paper")
          (should (file-exists-p el))
          ;; loads as valid elisp and defines exactly one defconst
          (with-temp-buffer
            (insert-file-contents el)
            (goto-char (point-min))
            (let ((forms nil))
              (ignore-errors
                (while t (push (read (current-buffer)) forms)))
              (let ((defconsts (cl-remove-if-not
                                (lambda (f) (and (listp f) (eq (car f) 'defconst)))
                                forms)))
                (should (= 1 (length defconsts)))
                (let ((manifest (cadr (caddr (car defconsts)))))
                  (should-not (plist-get manifest :annotations))))))
          ;; section dropped from the .el too; abstract retained
          (with-temp-buffer
            (insert-file-contents el)
            (should-not (search-forward "section/research-questions" nil t))
            (goto-char (point-min))
            (should (search-forward "section/abstract" nil t))))
      (delete-directory dir t))))

(ert-deftest arxana-essays-disposition-rename-section-heading-in-edn ()
  "Rename a section heading in the authoritative .edn (no heading-text prop).
The .edn section shape carries the heading in `:name' with NO
`(heading-text . ...)' prop (that's .el-only).  The rewrite must update
`:name' and not error on the missing heading-text prop — the gap behind
heading-rename-and-save silently failing to update the .edn."
  (let ((dir (make-temp-file "arxana-disp" t)))
    (unwind-protect
        (let ((edn (arxana-essays-disposition-test--write-fixture dir))
              (sec (arxana-essays-disposition-test--sid "methods")))
          ;; --manifest-file-for-section must find the .edn, not just .el
          (let ((arxana-browser-essays-manifest-files
                 (list (expand-file-name "annotations.el" dir))))
            ;; (the .el need not exist; the resolver also probes sibling .edn)
            (should (string= edn
                             (arxana-browser-essays--manifest-file-for-section sec))))
          ;; rewrite :name; new-heading provided but no heading-text prop present
          (should (arxana-browser-essays--rewrite-manifest-section-heading
                   edn sec "Methods and analysis" "Methods and analysis"))
          (let* ((data (arxana-browser-rewrites--read-edn-file edn))
                 (secs (append (plist-get data :sections) nil))
                 (m (cl-find-if (lambda (s) (string= (plist-get s :id) sec)) secs)))
            (should (string= "Methods and analysis" (plist-get m :name))))
          ;; still parses as valid EDN
          (should (arxana-browser-rewrites--read-edn-file edn)))
      (delete-directory dir t))))

(provide 'arxana-essays-disposition-test)
;;; arxana-essays-disposition-test.el ends here
