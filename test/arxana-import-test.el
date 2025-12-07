;;; arxana-import-test.el --- Tests for Org import helpers -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "arxana/dev" default-directory))

(let ((tangled (expand-file-name "arxana/arxana-tangled.el" default-directory)))
  (unless (featurep 'arxana-tangled)
    (load-file tangled)))

(require 'arxana-import)

(ert-deftest arxana-import-org-file-registers-article ()
  (let* ((temp (make-temp-file "arxana-import" nil ".org" "#+TITLE: Demo\nBody text"))
         (called nil)
         (stored nil))
    (unwind-protect
        (cl-letf (((symbol-function 'scholium)
                   (lambda (name text &optional _about _type _book)
                     (setq stored (list name text))))
                  ((symbol-function 'arxana-store-ensure-article)
                   (lambda (&rest args)
                     (setq called args)
                     'ok)))
          (let ((name (arxana-import-org-file temp)))
            (should (equal name "Demo"))
            (should (equal stored '("Demo" "#+TITLE: Demo\nBody text")))
            (should called)))
      (delete-file temp))))

(ert-deftest arxana-import-org-directory-imports-multiple-files ()
  (let* ((dir (make-temp-file "arxana-import-dir" t))
         (files (list (expand-file-name "one.org" dir)
                      (expand-file-name "two.org" dir)))
         (names '()))
    (unwind-protect
        (progn
          (dolist (file files)
            (with-temp-file file
              (insert (format "#+TITLE: %s\nBody" (file-name-base file)))))
          (cl-letf (((symbol-function 'scholium)
                     (lambda (name &rest _)
                       (push name names)))
                    ((symbol-function 'arxana-store-ensure-article)
                     (lambda (&rest _) 'ok)))
            (should (= (arxana-import-org-directory dir) 2))
            (should (= (length names) 2))
            (should (member "one" names))
            (should (member "two" names))))
      (delete-directory dir t))))

(provide 'arxana-import-test)
;;; arxana-import-test.el ends here
