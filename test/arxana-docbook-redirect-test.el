;;; arxana-docbook-redirect-test.el --- Tests for docbook redirects -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'arxana-browser-core)
(require 'arxana-browser-docbook)

(ert-deftest arxana-docbook-redirects-single-entry ()
  (let ((opened nil)
        (rendered nil))
    (with-temp-buffer
      (arxana-browser-mode)
      (let* ((heading (list :type 'docbook-heading
                            :doc-id "doc-1"
                            :title "Quickstart"
                            :level 1
                            :book "futon4"))
             (entry (list :doc-id "doc-1" :entry-id "run-1")))
        (setq tabulated-list-format (arxana-browser--docbook-contents-format))
        (setq tabulated-list-entries
              (list (list heading (arxana-browser--docbook-contents-row heading))))
        (tabulated-list-init-header)
        (tabulated-list-print t)
        (goto-char (point-min))
        (while (and (not (eobp)) (null (tabulated-list-get-id)))
          (forward-line 1))
        (let ((stack (list (list :view 'docbook-contents :book "futon4"))))
          (setq arxana-browser--stack stack)
          (cl-letf (((symbol-function 'arxana-browser--docbook-section-items)
                     (lambda (&rest _args)
                       (list (list :type 'docbook-entry :entry entry))))
                    ((symbol-function 'arxana-docbook-open-entry-object)
                     (lambda (arg) (setq opened arg)))
                    ((symbol-function 'arxana-browser--render)
                     (lambda () (setq rendered t))))
            (arxana-browser--visit)
            (should (equal opened entry))
            (should (not rendered))
            (should (equal arxana-browser--stack stack))))))))

(ert-deftest arxana-docbook-redirects-single-entry-render ()
  (let ((opened nil)
        (stack (list (list :view 'docbook-section :book "futon4")
                     (list :view 'docbook-contents :book "futon4"))))
    (with-temp-buffer
      (setq arxana-browser--stack stack)
      (cl-letf (((symbol-function 'arxana-browser--current-items)
                 (lambda ()
                   (list (list :type 'docbook-entry
                               :entry (list :doc-id "doc-1" :entry-id "run-1")))))
                ((symbol-function 'arxana-docbook-open-entry-object)
                 (lambda (arg) (setq opened arg)))
                ((symbol-function 'display-buffer)
                 (lambda (&rest _args) nil)))
        (arxana-browser--render)
        (should opened)
        (should (equal (plist-get opened :entry-id) "run-1"))
        (should (equal arxana-browser--stack (cdr stack)))))))

(provide 'arxana-docbook-redirect-test)
;;; arxana-docbook-redirect-test.el ends here
