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
          (reviews-dir (expand-file-name "reviews" root))
          (addenda-dir (expand-file-name "addenda" root))
          (incidents-root (make-temp-file "arxana-jvm-incidents-" t)))
     (make-directory items-dir t)
     (make-directory reviews-dir t)
     (make-directory addenda-dir t)
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
     ;; Filenames deliberately oppose timestamps: rendering follows :created-at.
     (with-temp-file (expand-file-name "a-later-name.edn" addenda-dir)
       (insert "{:morning-brief/addendum-id \"mba-second\"
 :attempt-id \"attempt-feature\" :kind :repro :title \"Exercise it\"
 :body \"Press n -> a compose buffer opens.\" :author \"joe\"
 :created-at \"2026-07-18T12:00:00Z\"}"))
     (with-temp-file (expand-file-name "z-earlier-name.edn" addenda-dir)
       (insert "{:morning-brief/addendum-id \"mba-first\"
 :attempt-id \"attempt-feature\" :kind :why-built :title \"Review gap\"
 :body \"We built this so claims can be reproduced.\" :author \"machine\"
 :created-at \"2026-07-18T11:00:00Z\"}"))
     ;; Filenames deliberately oppose timestamps: incident lists sort by :at.
     (with-temp-file (expand-file-name "a-newer.edn" incidents-root)
       (insert "{:jvm-incident/id \"jvi-new\" :at \"2026-07-18T14:00:00Z\"
 :thread \"invoke-worker-2\" :ex-class \"java.lang.OutOfMemoryError\"
 :message \"Java heap space while invoking a very large agent response\"
 :ex-data nil :stack [\"futon3c.agency.Invoke.run(Invoke.java:42)\"
                      \"java.lang.Thread.run(Thread.java:840)\"]
 :heap {:used-mb 1530 :max-mb 1536}
 :runtime {:pid 4242 :uptime-ms 90000}
 :jvm-incident/schema-version 1}"))
     (with-temp-file (expand-file-name "z-older.edn" incidents-root)
       (insert "{:jvm-incident/id \"jvi-old\" :at \"2026-07-18T13:00:00Z\"
 :thread \"worker-1\" :ex-class \"java.lang.IllegalStateException\"
 :message \"older\" :stack [\"example.Old.run(Old.java:1)\"]
 :heap {:used-mb 100 :max-mb 1536}
 :runtime {:pid 4242 :uptime-ms 80000}}"))
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
               (arxana-field-desk-jvm-incidents-root incidents-root)
               (arxana-field-desk-auto-refresh-seconds nil))
           ,@body)
       (when (get-buffer arxana-field-desk--buffer)
         (kill-buffer arxana-field-desk--buffer))
       (delete-directory root t)
       (delete-directory incidents-root t))))

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
     (let* ((case-fold-search nil)
            (text (buffer-string))
            (feature (string-match "THE FEATURE" text))
            (built (string-match "WHAT WAS BUILT" text))
            (notebook (string-match "NOTEBOOK — why built / how to reproduce" text))
            (validation (string-match "VALIDATION & INDEPENDENT REVIEW" text))
            (evidence (string-match "EVIDENCE LINKS" text))
            (verdict (string-match "^VERDICT$" text))
            (appendix (string-match "APPENDIX — DECISION QA" text)))
       (should (string-match-p "A navigable feature desk" text))
       (should (string-match-p "M-x arxana-field-desk" text))
       (should (< feature built))
       (should (< built notebook))
       (should (< notebook validation))
       (should (< validation evidence))
       (should (< evidence verdict))
       (should (< verdict appendix))
       (should (string-match-p "\\[unanswered\\] Accept the built feature" text))))))

(ert-deftest arxana-field-desk-failed-build-renders-honest-feature-gap ()
  (arxana-field-desk-test--with-store
   (with-temp-buffer
     (arxana-field-desk--insert-sheet
      (arxana-field-desk-test--item "attempt-failed")
      (arxana-field-desk--reviews))
     (should (string-match-p "No feature card: the author did not record a feature claim"
                             (buffer-string)))
     (should (string-match-p "\\[no\\] Was this the best available"
                             (buffer-string))))))

(ert-deftest arxana-field-desk-notebook-renders-addenda-in-created-order ()
  (arxana-field-desk-test--with-store
   (with-temp-buffer
     (arxana-field-desk--insert-notebook
      (arxana-field-desk-test--item "attempt-feature"))
     (let* ((case-fold-search nil)
            (text (buffer-string))
            (first (string-match "why-built · Review gap · machine · 2026-07-18" text))
            (second (string-match "repro · Exercise it · joe · 2026-07-18" text)))
       (should first)
       (should second)
       (should (< first second))
       (should (string-match-p
                "  We built this so claims can be reproduced" text))))))

(ert-deftest arxana-field-desk-notebook-empty-state-is-actionable ()
  (arxana-field-desk-test--with-store
   (with-temp-buffer
     (arxana-field-desk--insert-notebook
      (arxana-field-desk-test--item "attempt-full"))
     (should (string-match-p
              "No repro notes yet — press n to add what you tried and observed."
              (buffer-string))))))

(ert-deftest arxana-field-desk-review-post-payload-is-canonical ()
  (should
   (equal '(("attempt-id" . "attempt-feature")
            ("objective" . "feature-verdict")
            ("answer" . "accept-with-follow-ups")
            ("note" . "Works; retain a refresh follow-up")
            ("reviewer" . "joe"))
          (arxana-field-desk--review-payload
           "attempt-feature" :feature-verdict :accept-with-follow-ups
           "Works; retain a refresh follow-up" "joe")))
  (let ((arxana-field-desk-endpoint "http://127.0.0.1:7070/"))
    (should (equal
             "http://127.0.0.1:7070/api/alpha/morning-brief/review"
             (arxana-field-desk--review-url)))))

(ert-deftest arxana-field-desk-addendum-post-payload-is-canonical ()
  (should
   (equal '(("attempt-id" . "attempt-feature")
            ("kind" . "repro")
            ("title" . "Exercise it")
            ("body" . "Press n -> a compose buffer opens")
            ("author" . "joe"))
          (arxana-field-desk--addendum-payload
           "attempt-feature" :repro "Exercise it"
           "Press n -> a compose buffer opens" "joe")))
  (should (equal
           "http://127.0.0.1:7070/api/alpha/morning-brief/addendum"
           (arxana-field-desk--addendum-url))))

(ert-deftest arxana-field-desk-is-wired-into-the-browser-contract ()
  (arxana-field-desk-test--with-store
   (should (cl-find 'field-desk (arxana-browser--menu-items)
                    :key (lambda (item) (plist-get item :view))))
   (let ((arxana-browser--stack (list '(:view field-desk))))
     (let ((rows (arxana-browser--current-items)))
       (should (= 4 (length rows)))
       (should (= 3 (cl-count 'field-desk-stratum rows
                              :key (lambda (item) (plist-get item :type)))))
       (should (eq 'field-desk-incident-stratum
                   (plist-get (car (last rows)) :type)))))))

(ert-deftest arxana-field-desk-jvm-stratum-counts-durable-records ()
  (arxana-field-desk-test--with-store
   (let ((row (car (last (arxana-field-desk--home-items)))))
     (should (equal "JVM incidents (2)" (plist-get row :label)))
     (should (eq 'field-desk-incident-stratum (plist-get row :type))))))

(ert-deftest arxana-field-desk-jvm-incidents-are-newest-first ()
  (arxana-field-desk-test--with-store
   (should (equal '("jvi-new" "jvi-old")
                  (mapcar (lambda (incident)
                            (plist-get incident :jvm-incident/id))
                          (arxana-field-desk--jvm-incidents))))))

(ert-deftest arxana-field-desk-jvm-sheet-renders-stack-verbatim ()
  (arxana-field-desk-test--with-store
   (with-temp-buffer
     (arxana-field-desk--insert-incident-sheet
      (car (arxana-field-desk--jvm-incidents)))
     (let ((text (buffer-string)))
       (should (string-match-p "java.lang.OutOfMemoryError" text))
       (should (string-match-p "used MB[[:space:]]+1530" text))
       (should (string-match-p
                "  futon3c.agency.Invoke.run(Invoke.java:42)" text))
       (should (string-match-p
                "  java.lang.Thread.run(Thread.java:840)" text))))))

(provide 'arxana-field-desk-test)
;;; arxana-field-desk-test.el ends here
