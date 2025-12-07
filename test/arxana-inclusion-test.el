;;; arxana-inclusion-test.el --- Tests for inclusion/transclusion guards -*- lexical-binding: t; -*-

(require 'ert)
(require 'arxana-inclusion)

(ert-deftest arxana-include-errors-when-article-missing ()
  (let ((name-of-current-article "Target"))
    (cl-letf (((symbol-function 'get-article) (lambda (_name) nil)))
      (should-error (include-article "Missing") :type 'user-error))))

(ert-deftest arxana-include-inserts-source-text ()
  (let ((name-of-current-article "Target"))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-article)
                 (lambda (name)
                   (when (string= name "Demo") 'demo)))
                ((symbol-function 'sch-plain-text)
                 (lambda (_article) "DATA"))
                ((symbol-function 'scholium)
                 (lambda (&rest _)))
                ((symbol-function 'genref) (lambda () 'ref)))
        (include-article "Demo")
        (should (string= (buffer-string) "DATA"))))))

(ert-deftest arxana-transclude-errors-when-article-missing ()
  (let ((name-of-current-article "Target"))
    (cl-letf (((symbol-function 'get-article) (lambda (_name) nil)))
      (should-error (transclude-article "Missing") :type 'user-error))))

(ert-deftest arxana-transclude-inserts-source-text ()
  (let ((name-of-current-article "Target"))
    (with-temp-buffer
      (cl-letf (((symbol-function 'get-article)
                 (lambda (name)
                   (pcase name
                     ("Target" '(twd ("orig")))
                     ("Demo" 'demo))))
                ((symbol-function 'sch-plain-text)
                 (lambda (_article) "TRANS"))
                ((symbol-function 'scholium)
                 (lambda (&rest _)))
                ((symbol-function 'genref) (lambda () 'ref)))
        (transclude-article "Demo")
        (should (string= (buffer-string) "TRANS"))))))

(provide 'arxana-inclusion-test)
;;; arxana-inclusion-test.el ends here
