;;; arxana-bootstrap-test.el --- Harness smoke tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'arxana-bootstrap)

(ert-deftest arxana-bootstrap-root-exists ()
  "Repo root should resolve to a directory."
  (should (file-directory-p arxana-root-directory)))

(ert-deftest arxana-bootstrap-loads-dev ()
  "Loading dev modules should succeed."
  (should (arxana-load)))

(ert-deftest arxana-bootstrap-load-ignores-legacy-arg ()
  "Legacy prefix arguments should not change dev loading."
  (should (arxana-load t)))

(provide 'arxana-bootstrap-test)
;;; arxana-bootstrap-test.el ends here
