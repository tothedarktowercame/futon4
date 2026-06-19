;;; arxana-browser-essays-wikibooks-test.el --- Tests for Wikibooks essay import -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'arxana-browser-essays)
(require 'arxana-browser-essays-wikibooks)

(ert-deftest arxana-browser-essays-wikibooks-extract-book-pages-keeps-peeragogy-order ()
  (let* ((raw "
:#[[Peeragogy Handbook/Preface to the 3rd Edition|Preface]]
:# [[Peeragogy Handbook/Foreword|Foreword]]
:#[[Peeragogy Handbook V1.0/Overview|Overview]]
[[Wikibooks:Peeragogy|Project page]]
:# [[Peeragogy Handbook/Foreword|Foreword]]
")
         (pages (arxana-browser-essays-wikibooks--extract-book-pages
                 "Peeragogy Handbook" raw)))
    (should (equal '("Peeragogy Handbook/Preface to the 3rd Edition"
                     "Peeragogy Handbook/Foreword"
                     "Peeragogy Handbook V1.0/Overview")
                   pages))))

(ert-deftest arxana-browser-essays-wikibooks-converts-basic-wikitext-to-markdown ()
  (let* ((raw "Lead paragraph with [[w:peer learning|peer learning]].

== Notes ==
# This page was adapted from the [https://peeragogy.org/foreword HTML version].
[[Category:Book:Peeragogy Handbook]]
")
         (md (arxana-browser-essays-wikibooks--wikitext-to-markdown
              "Peeragogy Handbook/Foreword" raw)))
    (should (string-match-p "^# Peeragogy Handbook/Foreword\n\n## Overview\n\nLead paragraph with peer learning\\." md))
    (should (string-match-p "\n## Notes\n" md))
    (should (string-match-p "1\\. This page was adapted from the \\[HTML version\\](https://peeragogy\\.org/foreword)\\." md))
    (should-not (string-match-p "Category:" md))))

(ert-deftest arxana-browser-essays-wikibooks-decodes-entities-and-cleans-blockquotes ()
  (let* ((raw "__NOTOC__\n=Heartbeat=\n<blockquote>''&quot;If overstimulation interferes with our ability to 'think.'&quot;'' – ''Future Shock''</blockquote>\n")
         (md (arxana-browser-essays-wikibooks--wikitext-to-markdown
              "Peeragogy Handbook V1.0/Patterns" raw)))
    (should-not (string-match-p "__NOTOC__" md))
    (should-not (string-match-p "&quot;" md))
    (should (string-match-p "> \\*\"If overstimulation interferes with our ability to 'think\\.'\"\\* – \\*Future Shock\\*" md))))

(ert-deftest arxana-browser-essays-wikibooks-build-manifest-from-markdown-headings ()
  (let* ((markdown "# Demo\n\n## Overview\n\nAlpha.\n\n## Notes\n\nBeta.\n")
         (manifest (arxana-browser-essays-wikibooks--build-manifest
                    "Peeragogy Handbook/Foreword"
                    markdown
                    "peeragogy-handbook--foreword.md"))
         (essay (plist-get manifest :essay))
         (sections (plist-get manifest :sections)))
    (should (equal "Peeragogy Handbook/Foreword"
                   (plist-get essay :name)))
    (should (equal "peeragogy-handbook--foreword.md"
                   (plist-get essay :source-file)))
    (should (= 2 (length sections)))
    (should (equal "Overview"
                   (alist-get 'heading-text
                              (plist-get (car sections) :props))))
    (should (equal "Notes"
                   (alist-get 'heading-text
                              (plist-get (cadr sections) :props))))))

(ert-deftest arxana-browser-essays-wikibooks-build-book-manifest-uses-chapters-as-whole-files ()
  (let* ((root-title "Peeragogy Handbook")
         (imported-pages
          (list (list :page root-title
                      :source-file "/tmp/peeragogy-handbook.md")
                (list :page "Peeragogy Handbook/Foreword"
                      :source-file "/tmp/peeragogy-handbook--foreword.md")
                (list :page "Peeragogy Handbook/Introduction"
                      :source-file "/tmp/peeragogy-handbook--introduction.md")))
         (manifest (arxana-browser-essays-wikibooks--build-book-manifest
                    root-title imported-pages))
         (essay (plist-get manifest :essay))
         (sections (plist-get manifest :sections))
         (props (plist-get (car sections) :props)))
    (should (equal "Peeragogy Handbook" (plist-get essay :name)))
    (should (equal "peeragogy-handbook.md" (plist-get essay :source-file)))
    (should (eq t (alist-get 'wikibooks-catalog (plist-get essay :props))))
    (should (= 2 (length sections)))
    (should (equal "Foreword" (plist-get (car sections) :name)))
    (should (equal "Peeragogy Handbook/Foreword"
                   (alist-get 'page-title props)))
    (should (equal "/tmp/peeragogy-handbook--foreword.md"
                   (alist-get 'source-file props)))
    (should (eq t (alist-get 'render-whole-file props)))))

(ert-deftest arxana-browser-essays-wikibooks-whole-file-text-drops-page-title ()
  (let ((path (make-temp-file "arxana-wikibooks-whole-file-" nil ".md")))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert "# Peeragogy Handbook/Foreword\n\n## Overview\n\nAlpha.\n"))
          (should (equal "## Overview\n\nAlpha.\n"
                         (arxana-browser-essays-wikibooks--whole-file-text path))))
      (delete-file path))))

(ert-deftest arxana-browser-essays-wikibooks-merges-annotation-sidecar-into-book-manifest ()
  (let* ((tmpdir (make-temp-file "arxana-wikibooks-" t))
         (arxana-browser-essays-wikibooks-data-directory tmpdir)
         (page-title "Peeragogy Handbook/Foreword")
         (book-manifest (arxana-browser-essays-wikibooks--build-book-manifest
                         "Peeragogy Handbook"
                         (list (list :page "Peeragogy Handbook"
                                     :source-file "/tmp/peeragogy-handbook.md")
                               (list :page page-title
                                     :source-file "/tmp/peeragogy-handbook--foreword.md"))))
         (book-symbol (arxana-browser-essays-wikibooks--book-manifest-symbol
                       "Peeragogy Handbook"))
         (book-path (expand-file-name "peeragogy-handbook-book-manifest.el"
                                      tmpdir))
         (annotations-path
          (arxana-browser-essays-wikibooks--annotations-file-for-manifest-file
           book-path))
         (sidecar-symbol 'peeragogy-handbook-book-annotations)
         (catalog nil))
    (unwind-protect
        (progn
          (arxana-browser-essays-wikibooks--write-manifest-file
           book-path book-symbol book-manifest)
          (with-temp-file annotations-path
            (insert ";;; sidecar annotations\n\n")
            (let ((annotations
                   '((:id "hx:test:book"
                      :hx-type "annotation/grounds"
                      :annotated (:entity-id "arxana/essay/wikibooks/book/peeragogy-handbook/section/0-foreword"
                                  :passage "Alpha")
                      :source (:pattern-name "writing-coherence/meta-lede"
                               :passage "! conclusion: test")
                      :note "Test annotation"
                      :labels ("annotation/test")))))
              (pp `(defconst ,sidecar-symbol ',annotations)
                  (current-buffer))))
          (setq catalog
                (arxana-browser-essays-wikibooks--catalog-from-manifest-file
                 book-path))
          (should (equal "Peeragogy Handbook"
                         (plist-get catalog :label)))
          (should (= 1 (length (plist-get (symbol-value book-symbol) :annotations))))
          (should (equal "hx:test:book"
                         (plist-get (car (plist-get (symbol-value book-symbol)
                                                    :annotations))
                                    :id))))
      (delete-directory tmpdir t))))

(ert-deftest arxana-browser-essays-wikibooks-prune-keeps-annotation-sidecars ()
  (let* ((tmpdir (make-temp-file "arxana-wikibooks-" t))
         (arxana-browser-essays-wikibooks-data-directory tmpdir)
         (root-title "Peeragogy Handbook")
         (dir (arxana-browser-essays-wikibooks--book-directory root-title))
         (stale-md (expand-file-name "stale.md" dir))
         (sidecar (expand-file-name "peeragogy-handbook-book-annotations.el" dir)))
    (unwind-protect
        (progn
          (make-directory dir t)
          (with-temp-file stale-md
            (insert "stale"))
          (with-temp-file sidecar
            (insert "sidecar"))
          (arxana-browser-essays-wikibooks--prune-stale-import-files
           root-title
           '("Peeragogy Handbook"))
          (should-not (file-exists-p stale-md))
          (should (file-exists-p sidecar)))
      (delete-directory tmpdir t))))

(ert-deftest arxana-browser-essays-wikibooks-sync-registrations-discovers-book-catalog-only ()
  (let* ((tmpdir (make-temp-file "arxana-wikibooks-" t))
         (arxana-browser-essays-wikibooks-data-directory tmpdir)
         (arxana-browser-essays-manifest-files nil)
         (arxana-browser-essays-catalogs nil)
         (page-title "Peeragogy Handbook/Foreword")
         (page-manifest (arxana-browser-essays-wikibooks--build-manifest
                         page-title
                         "# Demo\n\n## Overview\n\nText.\n"
                         "peeragogy-handbook--foreword.md"))
         (page-symbol (arxana-browser-essays-wikibooks--manifest-symbol page-title))
         (page-path (expand-file-name "peeragogy-handbook--foreword-manifest.el"
                                      tmpdir))
         (book-manifest (arxana-browser-essays-wikibooks--build-book-manifest
                         "Peeragogy Handbook"
                         (list (list :page "Peeragogy Handbook"
                                     :source-file "/tmp/peeragogy-handbook.md")
                               (list :page page-title
                                     :source-file "/tmp/peeragogy-handbook--foreword.md"))))
         (book-symbol (arxana-browser-essays-wikibooks--book-manifest-symbol
                       "Peeragogy Handbook"))
         (book-path (expand-file-name "peeragogy-handbook-book-manifest.el"
                                      tmpdir)))
    (unwind-protect
        (progn
          (arxana-browser-essays-wikibooks--write-manifest-file
           page-path page-symbol page-manifest)
          (arxana-browser-essays-wikibooks--write-manifest-file
           book-path book-symbol book-manifest)
          (arxana-browser-essays-wikibooks-sync-registrations)
          (should (member page-path arxana-browser-essays-manifest-files))
          (should (member book-path arxana-browser-essays-manifest-files))
          (should (= 1 (length arxana-browser-essays-catalogs)))
          (should (equal (plist-get (car arxana-browser-essays-catalogs) :essay-id)
                         "arxana/essay/wikibooks/book/peeragogy-handbook"))
          (should (equal (plist-get (car arxana-browser-essays-catalogs) :source-file)
                         (expand-file-name "peeragogy-handbook.md" tmpdir))))
      (delete-directory tmpdir t))))

(provide 'arxana-browser-essays-wikibooks-test)

;;; arxana-browser-essays-wikibooks-test.el ends here
