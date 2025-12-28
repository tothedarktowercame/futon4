;;; arxana-docbook-export-test.el --- Docbook export tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'json)

(load-file (expand-file-name "dev/arxana-docbook.el" default-directory))
(require 'arxana-docbook)

(ert-deftest arxana-docbook-export-org-rewrites-includes ()
  (let* ((repo-root (arxana-docbook--repo-root))
         (include-target (expand-file-name "dev/arxana-browser-browse.el" repo-root)))
    (unless (file-readable-p include-target)
      (ert-skip (format "Missing include target: %s" include-target)))
    (let* ((tmp (make-temp-file "arxana-docbook-export" t))
           (books-root (expand-file-name "books" tmp))
           (book "futon4")
           (book-dir (expand-file-name book books-root))
           (raw-dir (expand-file-name "raw" book-dir))
           (stubs-dir (expand-file-name "stubs" book-dir))
           (export-dir (expand-file-name "export" book-dir))
           (toc-path (expand-file-name "toc.json" book-dir))
           (doc-id "futon4-test-doc")
           (run-id "futon4-test-run")
           (stub-path (expand-file-name (format "%s.org" run-id) stubs-dir))
           (raw-path (expand-file-name (format "%s.json" run-id) raw-dir))
           (output-file (expand-file-name "futon4.org" export-dir)))
      (make-directory raw-dir t)
      (make-directory stubs-dir t)
      (make-directory export-dir t)
      (with-temp-file toc-path
        (insert (json-encode (list (list :doc_id doc-id
                                         :title "Test"
                                         :outline_path (list "Test")
                                         :path_string "Test"
                                         :level 1)))
                "\n"))
      (with-temp-file raw-path
        (insert (json-encode (list :doc_id doc-id
                                   :version "test"
                                   :timestamp "2025-01-01T00:00:00Z"
                                   :outline_path (list "Test")
                                   :agent_summary "summary"))
                "\n"))
      (with-temp-file stub-path
        (insert "* Test\n#+INCLUDE: \"dev/arxana-browser-browse.el\"\n\nBody\n"))
      (let ((arxana-docbook-books-root books-root)
            (arxana-docbook-remote-enabled nil)
            (futon4-enable-sync nil))
        (arxana-docbook-export-org-book book output-file nil)
        (with-temp-buffer
          (insert-file-contents output-file)
          (goto-char (point-min))
          (should (search-forward (format "#+INCLUDE: \"%s\"" include-target) nil t)))))))

(provide 'arxana-docbook-export-test)
;;; arxana-docbook-export-test.el ends here
