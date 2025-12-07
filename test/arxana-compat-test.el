;;; arxana-compat-test.el --- Tests for article table shim -*- lexical-binding: t; -*-

(require 'ert)
(require 'arxana-compat)

(ert-deftest arxana-compat-prefers-article-table ()
  (let ((article-table (make-hash-table :test 'equal)))
    (puthash "Compat" '("TEXT" nil nil nil) article-table)
    (should (equal (cdr (get-article "Compat")) '("TEXT" nil nil nil)))))

(ert-deftest arxana-compat-falls-back-to-legacy ()
  (let ((article-table (make-hash-table :test 'equal)))
    (puthash "Compat" '("TEXT" nil nil nil) article-table)
    (should-not (get-article "Missing"))))

(provide 'arxana-compat-test)
;;; arxana-compat-test.el ends here
