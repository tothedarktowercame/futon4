;;; arxana-article-test.el --- Tests for article lifecycle -*- lexical-binding: t; -*-

(require 'ert)
(require 'arxana-article)

(defvar futon4-enable-sync)
(defvar modified-type-labels)

(ert-deftest arxana-article-metadata-envelope ()
  (let ((modified-type-labels '((nil . plain-label))))
    (cl-letf (((symbol-function 'scholium-text)
               (lambda (entry) (nth 1 entry)))
              ((symbol-function 'metadata-article)
               (lambda (_name)
                 (list 'meta
                       '((backlinks . (("Src" 1)))
                         (links . (("Dst"))))
                       nil 'meta 'system)))
              ((symbol-function 'get-article)
               (lambda (_label)
                 (list 'plain '(Alpha) nil nil nil)))
              ((symbol-function 'arxana-article--labels-for)
               (lambda (_name) '(plain-label))))
      (should (equal '((metadata (backlinks . (("Src" 1)))
                                 (links . (("Dst")))
                                 (labels . (plain-label))))
                     (arxana-article--metadata-envelope 'Alpha))))))

(provide 'arxana-article-test)
;;; arxana-article-test.el ends here
