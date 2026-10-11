;;; arxana-browser-missions-test.el --- Live mission census tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path (expand-file-name "../dev" (file-name-directory load-file-name)))
(require 'arxana-browser-core)
(require 'arxana-browser-missions)

(defconst arxana-browser-missions-test--snapshot
  '(:missions ((:mission/id "M-a" :mission/repo "futon2"
                :mission/status "ready" :mission/title "A")
               (:mission/id "M-b" :mission/repo "futon3"
                :mission/status "complete" :mission/title "B")
               (:mission/id "M-c" :mission/repo "futon3"
                :mission/status "derive" :mission/title "C"))
    :declared-count 3))

(ert-deftest arxana-missions-portfolio-shows-live-census-first ()
  (cl-letf (((symbol-function 'arxana-missions--fetch-snapshot)
             (lambda () arxana-browser-missions-test--snapshot)))
    (let* ((items (arxana-browser--missions-portfolio-items))
           (census (car items)))
      (should (eq 'info (plist-get census :type)))
      (should (equal "Live META field: 3 M/E/T/A tasks (2 kinds)"
                     (plist-get census :label)))
      (should (plist-get census :census-coherent))
      (should (= 4 (length items))))))

(ert-deftest arxana-missions-census-surfaces-endpoint-mismatch ()
  (let* ((snapshot (plist-put (copy-tree arxana-browser-missions-test--snapshot)
                              :declared-count 98))
         (item (arxana-missions--census-item snapshot)))
    (should-not (plist-get item :census-coherent))
    (should (string-match-p "declared 98, returned 3"
                            (plist-get item :description)))))

(ert-deftest arxana-home-is-the-meta-work-field ()
  (let ((missions (cl-find-if
                   (lambda (item) (equal "META" (plist-get item :label)))
                   (arxana-browser--menu-items))))
    (should missions)
    (should (string-match-p "outer-policy order" (plist-get missions :description)))
    (should-not (string-match-p "98 missions" (plist-get missions :description)))))

(provide 'arxana-browser-missions-test)
;;; arxana-browser-missions-test.el ends here
