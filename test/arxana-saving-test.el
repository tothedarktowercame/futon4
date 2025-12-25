;;; arxana-saving-test.el --- Snapshot save/restore integration -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'arxana-test-support)

(add-to-list 'load-path (expand-file-name "dev" default-directory))

(require 'arxana-store)

(arxana-test--ensure-tangled-loaded)

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
