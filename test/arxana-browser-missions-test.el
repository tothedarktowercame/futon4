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

(ert-deftest arxana-missions-portfolio-renders-info-instead-of-question-marks ()
  (should (equal ["!" "" "Missions unavailable" "Error: projection absent"]
                 (arxana-browser--missions-portfolio-row
                  '(:type info :label "Missions unavailable"
                    :description "Error: projection absent")))))

(ert-deftest arxana-mission-views-register-their-keymap ()
  (dolist (view '(missions-portfolio missions-by-status missions-status-group))
    (should (eq arxana-browser-missions--view-map
                (gethash view arxana-browser-view-keymaps)))))

(ert-deftest arxana-home-is-the-meta-work-field ()
  (let ((missions (cl-find-if
                   (lambda (item) (equal "META" (plist-get item :label)))
                   (arxana-browser--menu-items))))
    (should missions)
    (should (string-match-p "outer-policy order" (plist-get missions :description)))
    (should-not (string-match-p "98 missions" (plist-get missions :description)))))

(ert-deftest arxana-work-candidates-lead-with-meta-id ()
  (let* ((work '(:mission/id "E-dynamic-queries"
                 :mission/repo "excursion"
                 :mission/status "eligible"
                 :mission/path "/tmp/E-dynamic-queries.md"))
         (candidate (car (arxana-missions--work-candidates (list work)))))
    (should (string-prefix-p "E-dynamic-queries  [eligible · excursion]"
                             (car candidate)))
    (should (eq work (cdr candidate)))))

(ert-deftest arxana-work-id-at-point-prefers-containing-id ()
  (with-temp-buffer
    (insert "See M-other-work, then E-dynamic-queries for the operative charter.")
    (search-backward "dynamic")
    (should (equal "E-dynamic-queries"
                   (arxana-missions--work-id-at-point)))))

(ert-deftest arxana-work-id-at-point-uses-nearest-surrounding-id ()
  (with-temp-buffer
    (insert "M-left-work some nearby prose E-right-work")
    (search-backward "nearby")
    (should (equal "M-left-work"
                   (arxana-missions--work-id-at-point)))))

(ert-deftest futon-find-work-opens-the-meta-source-without-scanning ()
  (let* ((path (make-temp-file "futon-find-work-" nil ".md"))
         (work (list :mission/id "M-shared-memory-control-build-test"
                     :mission/repo "mission" :mission/status "ready"
                     :mission/path path))
         opened)
    (unwind-protect
        (cl-letf (((symbol-function 'arxana-missions--fetch-work-inventory)
                   (lambda () (list work)))
                  ((symbol-function 'completing-read)
                   (lambda (_prompt candidates &rest _)
                     (caar candidates)))
                  ((symbol-function 'find-file)
                   (lambda (chosen) (setq opened chosen))))
          (futon-find-work)
          (should (equal path opened)))
      (delete-file path))))

(ert-deftest futon-find-work-defaults-to-id-around-point ()
  (let* ((path (make-temp-file "futon-find-work-default-" nil ".md"))
         (work (list :mission/id "E-dynamic-queries" :mission/repo "excursion"
                     :mission/status "known" :mission/path path))
         seen-default)
    (unwind-protect
        (with-temp-buffer
          (insert "Continue with E-dynamic-queries next.")
          (search-backward "dynamic")
          (cl-letf (((symbol-function 'arxana-missions--fetch-work-inventory)
                     (lambda () (list work)))
                    ((symbol-function 'completing-read)
                     (lambda (_prompt candidates _predicate _require-match
                                      _initial _hist default)
                       (setq seen-default default)
                       (caar candidates)))
                    ((symbol-function 'find-file) #'ignore))
            (futon-find-work)
            (should (string-prefix-p "E-dynamic-queries  [" seen-default))))
      (delete-file path))))

(ert-deftest arxana-substrate-work-row-keeps-canonical-provenance-path ()
  (cl-letf (((symbol-function 'arxana-missions--substrate-json)
             (lambda (_)
               '(:entities
                 ((:entity/external-id "M-shared-memory-control-build-test"
                   :entity/name "mission|M-shared-memory-control-build-test"
                   :entity/type "mission"
                   :entity/props
                   (:provenance/repo "futon3c"
                    :provenance/path "/canonical/M-shared-memory-control-build-test.md"
                    :mission/status-class "open")))))))
    (let ((row (car (arxana-missions--substrate-work-rows "mission"))))
      (should (equal "M-shared-memory-control-build-test"
                     (plist-get row :mission/id)))
      (should (equal "/canonical/M-shared-memory-control-build-test.md"
                     (plist-get row :mission/path)))
      (should (equal "open" (plist-get row :mission/status))))))

(ert-deftest arxana-missing-inventory-path-resolves-from-exact-scope-entity ()
  (let ((work '(:mission/id "E-70B-on-raw-control-arm"
                :mission/repo "excursion" :mission/path nil)))
    (cl-letf (((symbol-function 'arxana-missions--substrate-json)
               (lambda (path)
                 (should (string-suffix-p
                          "/api/alpha/entity/E-70B-on-raw-control-arm" path))
                 '(:entity (:props
                            (:mission/path
                             "/home/joe/code/futon6/holes/excursions/E-70B-on-raw-control-arm.md"))))))
      (should
       (equal "/home/joe/code/futon6/holes/excursions/E-70B-on-raw-control-arm.md"
              (arxana-missions--resolve-work-path work))))))

(ert-deftest arxana-present-inventory-path-needs-no-second-read ()
  (let ((work '(:mission/id "E-ready" :mission/path "/canonical/E-ready.md")))
    (cl-letf (((symbol-function 'arxana-missions--substrate-json)
               (lambda (_) (ert-fail "unexpected substrate lookup"))))
      (should (equal "/canonical/E-ready.md"
                     (arxana-missions--resolve-work-path work))))))

(provide 'arxana-browser-missions-test)
;;; arxana-browser-missions-test.el ends here
