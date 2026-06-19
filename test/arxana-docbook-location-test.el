;;; arxana-docbook-location-test.el --- Tests for docbook filesystem location -*- lexical-binding: t; -*-

;;; Commentary:
;; Ensure docbook browsing resolves to docs/docbook after the migration.

;;; Code:

(require 'ert)
(require 'seq)

(defun arxana-docbook-location-test--canonical-dir (path)
  (directory-file-name (file-truename (expand-file-name path))))

(let* ((base (or load-file-name buffer-file-name))
       (root (and base
                  (expand-file-name ".." (file-name-directory base)))))
  (load-file (expand-file-name "dev/arxana-docbook-core.el" root))
  (load-file (expand-file-name "dev/arxana-docbook-toc.el" root)))

(ert-deftest arxana-docbook-location-resolves-docbook-root ()
  (let* ((arxana-docbook-books-root nil)
         (repo-root (arxana-docbook--repo-root))
         (expected (expand-file-name "docs/docbook" repo-root))
         (actual (arxana-docbook--locate-books-root)))
    (should (equal expected actual))
    (should (file-directory-p actual))))

(ert-deftest arxana-docbook-location-futon4-toc-and-entries ()
  (let* ((arxana-docbook-books-root nil)
         (arxana-docbook-remote-enabled nil)
         (book "futon4")
         (root (arxana-docbook--locate-books-root))
         (toc-path (expand-file-name (format "%s/toc.json" book) root))
         (toc (arxana-docbook--toc book))
         (entries (arxana-docbook-entries book)))
    (should (file-readable-p toc-path))
    (should (listp toc))
    (should (seq-some (lambda (heading) (plist-get heading :doc-id)) toc))
    (should (listp entries))
    (should (seq-some (lambda (entry) (plist-get entry :doc-id)) entries))))

(ert-deftest arxana-docbook-location-ignores-system-dev-root ()
  (let ((arxana-docbook-books-root nil)
        (load-file-name nil)
        (buffer-file-name nil)
        (default-directory "/home/joe/code/"))
    (should (equal (arxana-docbook-location-test--canonical-dir "/home/joe/code/futon4/")
                   (arxana-docbook-location-test--canonical-dir
                    (arxana-docbook--repo-root))))
    (should (equal (arxana-docbook-location-test--canonical-dir
                    "/home/joe/code/futon4/docs/docbook")
                   (arxana-docbook-location-test--canonical-dir
                    (arxana-docbook--locate-books-root))))))

(provide 'arxana-docbook-location-test)
;;; arxana-docbook-location-test.el ends here
