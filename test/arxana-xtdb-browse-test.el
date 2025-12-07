;;; arxana-xtdb-browse-test.el --- Tests for XTDB browser -*- lexical-binding: t; -*-

(require 'ert)
(require 'arxana-xtdb-browse)

(ert-deftest arxana-xtdb-extracts-tail-rows ()
  (let* ((row '((:src . ((:name . "Alpha") (:type . :article) (:ident . "id-alpha")))
                (:dst . ((:name . "Beta") (:type . :article) (:ident . "id-beta")))
                (:type . :hx/links)
                (:last-seen . 123)))
         (sample `((:command . "/tail") (:tail . ((:relations . (,row))))))
         (rows (arxana-xtdb--tail-rows sample)))
    (should (= (length rows) 1))
    (should (eq (alist-get :type (car rows)) :hx/links))))

(ert-deftest arxana-xtdb-rows-ignore-metadata ()
  (let ((rows (arxana-xtdb--tail-rows '((:command . "/tail")
                                        (:limit . 5)
                                        (:relations . nil)))))
    (should (null rows))))

(ert-deftest arxana-xtdb-converts-rows-into-entries ()
  (let* ((rows (list '((:src . ((:name . "Alpha") (:type . :article) (:ident . "id-alpha")))
                      (:dst . ((:name . "Beta") (:type . :article) (:ident . "id-beta")))
                      (:type . :hx/links)
                      (:last-seen . 42))))
         (entries (arxana-xtdb--rows->entries rows))
         (entry (car entries))
         (vector (cadr entry)))
    (should (= (length entries) 1))
    (should (string-match-p "Alpha" (aref vector 2)))
    (should (string-match-p "Beta" (aref vector 3)))))

(ert-deftest arxana-xtdb-entity-labels ()
  (should (string-match-p "Alpha"
                          (arxana-xtdb--entity-label '((:name . "Alpha")))))
  (should (string-match-p "Alpha"
                          (arxana-xtdb--entity-label '((:entity/name . "Alpha"))))))

(provide 'arxana-xtdb-browse-test)
;;; arxana-xtdb-browse-test.el ends here
