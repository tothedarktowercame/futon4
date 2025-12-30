;;; arxana-export-test.el --- Tests for Org exporter -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'arxana-test-support)

(add-to-list 'load-path (expand-file-name "dev" default-directory))

(arxana-test--ensure-tangled-loaded)

(require 'arxana-articles-export)

(ert-deftest arxana-export-org-directory-writes-files ()
  (let ((futon4-enable-sync nil)
        (dir (make-temp-file "arxana-export" t)))
    (unwind-protect
        (cl-letf (((symbol-function 'arxana-export--labels-for)
                   (lambda (&rest _) '("demo-label")))
                  ((symbol-function 'arxana-export--articles)
                   (lambda ()
                     (list (list "Demo"
                                 "Body text"
                                 '(("Target" (passage ("Demo" 0 42)) (transclusion-of "Src")))
                                 nil
                                 nil)))))
          (let ((dest (arxana-export-org-directory dir)))
            (should (file-exists-p (expand-file-name "MANIFEST.org" dest)))
            (let* ((org-files (directory-files dest t "\\.org\\'"))
                   (article-path (cl-find-if (lambda (path)
                                               (not (string-match-p "MANIFEST\\.org$" path)))
                                             org-files)))
              (should article-path)
              (with-temp-buffer
                (insert-file-contents article-path)
                (should (search-forward "#+TITLE: Demo" nil t))))
            (with-temp-buffer
              (insert-file-contents (expand-file-name "MANIFEST.org" dest))
              (should (search-forward "| Name | File | ID | Labels | Links |" nil t))
              (should (search-forward "* Article Index" nil t))
              (should (search-forward "- Labels: demo-label" nil t))
              (should (search-forward "[transclusion] Target" nil t))
              (should (search-forward "* Label Index" nil t)))))
      (delete-directory dir t))))

(ert-deftest arxana-export-org-directory-calls-snapshot-when-enabled ()
  (let ((futon4-enable-sync t)
        (dir (make-temp-file "arxana-export" t))
        (snapshot-called nil)
        (scope-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'arxana-export--articles)
                   (lambda ()
                     (list (list "Demo" "Body" nil nil nil))))
                  ((symbol-function 'arxana-store-sync-enabled-p)
                   (lambda () t))
                  ((symbol-function 'arxana-store--snapshot-scope-prompt)
                   (lambda (&rest _)
                     (setq scope-called t)
                     'all))
                  ((symbol-function 'read-string)
                   (lambda (&rest _) ""))
                  ((symbol-function 'arxana-store-save-snapshot)
                   (lambda (&rest _)
                     (setq snapshot-called t)
                     '((:snapshot/id . "snap-1"))))
                  ((symbol-function 'arxana-store--snapshot-id-from-response)
                   (lambda (&rest _) "snap-1")))
          (arxana-export-org-directory dir)
          (should snapshot-called)
          (should scope-called)
          (with-temp-buffer
            (insert-file-contents (expand-file-name "MANIFEST.org" dir))
            (should (search-forward "snap-1" nil t))
            (should (search-forward "- Hyperedges: 0" nil t))))
      (delete-directory dir t))))

(provide 'arxana-export-test)
;;; arxana-export-test.el ends here
