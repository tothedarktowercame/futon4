;;; arxana-vsatarcs-ledger-test.el --- Tests for Arxana Ledger -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-vsatarcs-ledger)

(defconst arxana-ledger-test--fixture
  "{:ledger
 [{:item/id \"202504-01\" :item/invoice nil :item/status :unbilled-draft
   :item/invoiced? false :item/paid? false :item/hours 1 :item/rate-gbp 75.0}
  {:item/id \"202504-02\" :item/invoice nil :item/status :unbilled-draft
   :item/invoiced? false :item/paid? false :item/hours 2 :item/rate-gbp 75.0}
  {:item/id \"R\" :item/invoice \"202503\" :item/status :invoice-ready
   :item/invoiced? false :item/paid? false :item/hours 1 :item/rate-gbp 75.0}
  {:item/id \"I\" :item/invoice \"202502\" :item/status :invoiced
   :item/invoiced? true :item/paid? false :item/hours 1 :item/rate-gbp 75.0}
  {:item/id \"H\" :item/invoice \"202501\" :item/status :billed-paid
   :item/invoiced? true :item/paid? true :item/hours 1 :item/rate-gbp 75.0}]}"
  "Minimal ledger fixture covering each lifecycle stratum.")

(defmacro arxana-ledger-test--with-ledger (&rest body)
  "Run BODY with a temporary ledger fixture."
  `(let ((tmp (make-temp-file "arxana-ledger-" nil ".edn"
                              arxana-ledger-test--fixture)))
     (unwind-protect
         (let ((arxana-ledger-file tmp))
           ,@body)
       (delete-file tmp))))

(defun arxana-ledger-test--item (id)
  "Return fixture item ID from the current temporary ledger."
  (let ((items (arxana-ledger--items (arxana-ledger--read))))
    (cl-find id items :key (lambda (it) (plist-get it :item/id)) :test #'equal)))

(ert-deftest arxana-ledger-mode-map-binds-ledger-help ()
  (should (eq #'arxana-ledger-help
              (lookup-key arxana-ledger-mode-map (kbd "?"))))
  (should (eq #'arxana-ledger-ready-current-invoice
              (lookup-key arxana-ledger-mode-map (kbd "r"))))
  (should (eq #'arxana-ledger-mark-invoice-sent
              (lookup-key arxana-ledger-mode-map (kbd "i"))))
  (should (eq #'arxana-ledger-mark-invoice-historical
              (lookup-key arxana-ledger-mode-map (kbd "h"))))
  (should (eq #'arxana-ledger-open-draft-invoice
              (lookup-key arxana-ledger-mode-map (kbd "O"))))
  (should (eq #'arxana-ledger-mark-invoice-sent
              (lookup-key arxana-ledger-mode-map (kbd "I")))))

(ert-deftest arxana-ledger-status-choices-cover-pipeline ()
  (dolist (status '(":unbilled-draft" ":invoice-ready" ":invoiced"
                    ":billed-paid" ":speculative" ":archived"))
    (should (member status arxana-ledger--status-choices))))

(ert-deftest arxana-ledger-mark-invoice-sent-updates-ready-items ()
  (arxana-ledger-test--with-ledger
   (let ((refreshed nil))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
               ((symbol-function 'arxana-ledger-refresh)
                (lambda () (setq refreshed t))))
       (arxana-ledger-mark-invoice-sent "202503")
       (let ((ready (arxana-ledger-test--item "R"))
             (current (arxana-ledger-test--item "202504-01")))
         (should refreshed)
         (should (eq :invoiced (plist-get ready :item/status)))
         (should (eq t (plist-get ready :item/invoiced?)))
         (should (eq nil (plist-get ready :item/paid?)))
         (should (eq :unbilled-draft (plist-get current :item/status))))))))

(ert-deftest arxana-ledger-ready-current-invoice-stages-all-current-items ()
  (arxana-ledger-test--with-ledger
   (let ((generated nil)
         (refreshed nil))
     (cl-letf (((symbol-function 'arxana-ledger--generate-draft-invoice)
                (lambda (inv-id) (setq generated inv-id)))
               ((symbol-function 'arxana-ledger-refresh)
                (lambda () (setq refreshed t))))
       (arxana-ledger-ready-current-invoice "202504")
       (let ((a (arxana-ledger-test--item "202504-01"))
             (b (arxana-ledger-test--item "202504-02"))
             (ready (arxana-ledger-test--item "R")))
         (should (equal "202504" generated))
         (should refreshed)
         (dolist (it (list a b))
           (should (equal "202504" (plist-get it :item/invoice)))
           (should (eq :invoice-ready (plist-get it :item/status)))
           (should (eq nil (plist-get it :item/invoiced?)))
           (should (eq nil (plist-get it :item/paid?))))
         (should (eq :invoice-ready (plist-get ready :item/status))))))))

(ert-deftest arxana-ledger-mark-invoice-historical-updates-invoiced-items ()
  (arxana-ledger-test--with-ledger
   (let ((refreshed nil))
     (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
               ((symbol-function 'arxana-ledger-refresh)
                (lambda () (setq refreshed t))))
       (arxana-ledger-mark-invoice-historical "202502")
       (let ((it (arxana-ledger-test--item "I"))
             (historical (arxana-ledger-test--item "H")))
         (should refreshed)
         (should (eq :billed-paid (plist-get it :item/status)))
         (should (eq t (plist-get it :item/invoiced?)))
         (should (eq t (plist-get it :item/paid?)))
         (should (eq :billed-paid (plist-get historical :item/status))))))))

(ert-deftest arxana-ledger-read-invoice-id-for-status-uses-chooser ()
  (arxana-ledger-test--with-ledger
   (let ((seen nil))
     (cl-letf (((symbol-function 'completing-read)
                (lambda (prompt choices &rest _)
                  (setq seen (list prompt choices))
                  "202503")))
       (should (equal "202503"
                      (arxana-ledger--read-invoice-id-for-status
                       :invoice-ready "Record Invoice Ready as sent: ")))
       (should (equal "Record Invoice Ready as sent: " (car seen)))
       (should (equal '("202503") (cadr seen)))))))

(ert-deftest arxana-ledger-help-hydra-refreshes-on-reload ()
  (unwind-protect
      (progn
        (eval '(defun arxana-ledger-help-hydra/body () :old))
        (eval
         '(defmacro defhydra (&rest _args)
            '(defun arxana-ledger-help-hydra/body ()
               (interactive)
               :new)))
        (cl-letf (((symbol-function 'require) (lambda (&rest _) nil)))
          (should (arxana-ledger--ensure-help-hydra))
          (should (eq :new (arxana-ledger-help-hydra/body)))))
    (ignore-errors (fmakunbound 'arxana-ledger-help-hydra/body))
    (ignore-errors (fmakunbound 'defhydra))))

(provide 'arxana-vsatarcs-ledger-test)
;;; arxana-vsatarcs-ledger-test.el ends here
