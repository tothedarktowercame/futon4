;;; arxana-saving-test.el --- Snapshot save/restore integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "arxana/dev" default-directory))

(require 'arxana-store)

(let ((tangled (expand-file-name "arxana/arxana-tangled.el" default-directory)))
  (unless (featurep 'arxana-tangled)
    (load-file tangled)))

(ert-deftest arxana-saving-prefers-snapshot-when-synced ()
  (let ((futon4-enable-sync t)
        (captured nil))
    (cl-letf (((symbol-function 'arxana-store-save-snapshot)
               (lambda (scope label)
                 (setq captured (list scope label))
                 '((:snapshot/id . "snap-1")))))
      (save-all-scholia "notes" 'latest)
      (should (equal captured '(latest "notes"))))))

(ert-deftest arxana-saving-restores-snapshot-when-synced ()
  (let ((futon4-enable-sync t)
        (captured nil))
    (cl-letf (((symbol-function 'arxana-store-restore-snapshot)
               (lambda (snapshot scope)
                 (setq captured (list snapshot scope))
                 '((:snapshot/id . "snap-1")))))
      (read-scholia-file "snap-1" 'all)
      (should (equal captured '("snap-1" all))))))

(provide 'arxana-saving-test)
;;; arxana-saving-test.el ends here
