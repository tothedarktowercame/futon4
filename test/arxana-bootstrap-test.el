;;; arxana-bootstrap-test.el --- Harness smoke tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'arxana-bootstrap)

(ert-deftest arxana-bootstrap-locates-spine ()
  "Spine file should exist inside the repo."
  (should (file-exists-p (arxana--spine-file))))

(ert-deftest arxana-bootstrap-loads-spine ()
  "Loading the spine should define helper functions for tangling."
  (arxana-load-spine)
  (should (fboundp 'arxana-tangle-spine-concat)))

(ert-deftest arxana-bootstrap-builds-tangled ()
  "Full build regenerates the tangled sources without error."
  (let ((output (arxana-build t)))
    (should (and (stringp output)
                 (file-exists-p output)))))

(provide 'arxana-bootstrap-test)
;;; arxana-bootstrap-test.el ends here
