;;; arxana-field-desk-test.el --- Tests for Morning Brief Field Desk -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "../dev" (file-name-directory load-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'arxana-field-desk)
(require 'arxana-browser-core)

(defconst arxana-field-desk-test--feature-item
  "{:attempt-id \"attempt-feature\"
 :queued-at \"2026-07-18T10:00:00Z\"
 :outcome :grounded-change
 :commit \"abc123\"
 :achievement {:tier :fully-grounded
               :build {:validation {:review-job \"review-1\"
                                    :artifact-binding {:repo \"/repo\" :commit \"abc123\"}}}}
 :qa-targets {:selection {:policy {:mission-path \"/tmp/M-feature.md\"}}}
 :feature-card {:built \"A navigable feature desk\"
                :want-coverage [\"operator acceptance\" \"refresh behavior\"]
                :matches-intent? true
                :things-to-try [\"M-x arxana-field-desk\" \"press v\"]
                :fold-ref \"fold/feature\"
                :proof-ref \"proof/feature\"}}")

(defconst arxana-field-desk-test--failed-item
  "{:attempt-id \"attempt-failed\"
 :queued-at \"2026-07-18T11:00:00Z\"
 :outcome :build-failed
 :achievement {:tier :none :build nil}
 :failure {:kind :build-failed}}")

(defconst arxana-field-desk-test--full-item
  "{:attempt-id \"attempt-full\"
 :queued-at \"2026-07-18T12:00:00Z\"
 :outcome :grounded-change
 :commit \"def456\"
 :achievement {:tier :fully-grounded :build {:commits [\"def456\"]}}
 :feature-card {:built \"Already accepted\"
                :want-coverage []
                :matches-intent? true
                :things-to-try []}}")

(defmacro arxana-field-desk-test--with-store (&rest body)
  `(let* ((root (make-temp-file "arxana-field-desk-" t))
          (items-dir (expand-file-name "items" root))
          (reviews-dir (expand-file-name "reviews" root)))
     (make-directory items-dir t)
     (make-directory reviews-dir t)
     (with-temp-file (expand-file-name "attempt-feature.edn" items-dir)
       (insert arxana-field-desk-test--feature-item))
     (with-temp-file (expand-file-name "attempt-failed.edn" items-dir)
       (insert arxana-field-desk-test--failed-item))
     (with-temp-file (expand-file-name "attempt-full.edn" items-dir)
       (insert arxana-field-desk-test--full-item))
     (with-temp-file (expand-file-name "failed-selection.edn" reviews-dir)
       (insert "{:morning-brief/review-id \"r-failed-selection\"
 :attempt-id \"attempt-failed\" :objective :selection-quality
 :answer :no :note \"A better target existed\" :reviewer \"joe\"}"))
     (dolist (pair '((:feature-verdict :accept-feature)
                     (:selection-quality :yes)
                     (:substantive-achievement :yes)
                     (:evidence-sufficiency :sufficient)))
       (with-temp-file
           (expand-file-name (format "full-%s.edn" (substring (symbol-name (car pair)) 1))
                             reviews-dir)
         (insert (format "{:morning-brief/review-id \"r-%s\"
 :attempt-id \"attempt-full\" :objective %s :answer %s
 :note \"reviewed\" :reviewer \"joe\"}"
                         (substring (symbol-name (car pair)) 1)
                         (car pair) (cadr pair)))))
     (unwind-protect
         (let ((arxana-field-desk-root root)
               (arxana-field-desk-auto-refresh-seconds nil))
           ,@body)
       (when (get-buffer arxana-field-desk--buffer)
         (kill-buffer arxana-field-desk--buffer))
       (delete-directory root t))))

(defun arxana-field-desk-test--item (attempt-id)
  (cl-find attempt-id (arxana-field-desk--items)
           :key (lambda (item) (plist-get item :attempt-id))
           :test #'equal))

(ert-deftest arxana-field-desk-strata-are-an-exclusive-partition ()
  (arxana-field-desk-test--with-store
   (let ((items (arxana-field-desk--items))
         (reviews (arxana-field-desk--reviews)))
     (should (equal '("attempt-feature")
                    (mapcar (lambda (item) (plist-get item :attempt-id))
                            (arxana-field-desk--items-at :pending items reviews))))
     (should (equal '("attempt-failed")
                    (mapcar (lambda (item) (plist-get item :attempt-id))
                            (arxana-field-desk--items-at :partial items reviews))))
     (should (equal '("attempt-full")
                    (mapcar (lambda (item) (plist-get item :attempt-id))
                            (arxana-field-desk--items-at :full items reviews))))
     (should (= 3 (apply #'+
                         (mapcar (lambda (stratum)
                                   (length (arxana-field-desk--items-at
                                            stratum items reviews)))
                                 '(:pending :partial :full))))))))

(ert-deftest arxana-field-desk-applicability-mirrors-morning-brief ()
  (arxana-field-desk-test--with-store
   (should (equal '(:feature-verdict :selection-quality
                    :substantive-achievement :evidence-sufficiency)
                  (arxana-field-desk--item-objectives
                   (arxana-field-desk-test--item "attempt-feature"))))
   (should (equal '(:selection-quality :substantive-achievement
                    :machine-response)
                  (arxana-field-desk--item-objectives
                   (arxana-field-desk-test--item "attempt-failed"))))))

(ert-deftest arxana-field-desk-sheet-is-feature-first-and-shows-answers ()
  (arxana-field-desk-test--with-store
   (with-temp-buffer
     (arxana-field-desk--insert-sheet
      (arxana-field-desk-test--item "attempt-feature")
      (arxana-field-desk--reviews))
     (let* ((text (buffer-string))
            (feature (string-match "THE FEATURE" text))
            (evidence (string-match "EVIDENCE LINKS" text))
            (verdict (string-match "VERDICT" text))
            (appendix (string-match "APPENDIX — DECISION QA" text)))
       (should (string-match-p "A navigable feature desk" text))
       (should (string-match-p "M-x arxana-field-desk" text))
       (should (< feature evidence))
       (should (< evidence verdict))
       (should (< verdict appendix))
       (should (string-match-p "\\[unanswered\\] Accept the built feature" text))))))

(ert-deftest arxana-field-desk-failed-build-renders-honest-feature-gap ()
  (arxana-field-desk-test--with-store
   (with-temp-buffer
     (arxana-field-desk--insert-sheet
      (arxana-field-desk-test--item "attempt-failed")
      (arxana-field-desk--reviews))
     (should (string-match-p "Build-time gap: no valid feature card"
                             (buffer-string)))
     (should (string-match-p "\\[no\\] Was this the best available"
                             (buffer-string))))))

(ert-deftest arxana-field-desk-qa-command-is-shell-free-argv ()
  (should
   (equal '("clojure" "-M:wm-full-loop" "qa" "attempt-feature"
            "feature-verdict" "accept-with-follow-ups"
            "Works; retain a refresh follow-up" "joe")
          (arxana-field-desk--qa-command
           "attempt-feature" :feature-verdict :accept-with-follow-ups
           "Works; retain a refresh follow-up" "joe"))))

(ert-deftest arxana-field-desk-is-wired-into-the-browser-contract ()
  (arxana-field-desk-test--with-store
   (should (cl-find 'field-desk (arxana-browser--menu-items)
                    :key (lambda (item) (plist-get item :view))))
   (let ((arxana-browser--stack (list '(:view field-desk))))
     (should (= 3 (length (arxana-browser--current-items))))
     (should (cl-every
              (lambda (item) (eq 'field-desk-stratum (plist-get item :type)))
              (arxana-browser--current-items))))))

(provide 'arxana-field-desk-test)
;;; arxana-field-desk-test.el ends here
