;;; arxana-cold-outbox-test.el --- Tests for cold outbox porcelain -*- lexical-binding: t; -*-

(setq load-prefer-newer t)
(add-to-list 'load-path (expand-file-name "../dev" (file-name-directory load-file-name)))

(require 'ert)
(require 'cl-lib)
(require 'arxana-cold-outbox)

(defconst arxana-cold-outbox-test--staged
  "{:schema/version 1
 :draft/id \"draft-1\"
 :draft/status :staged
 :paths {:staging-dir \"__DIR__\" :draft-md \"draft.md\" :context-md \"context.md\"}
 :source/scan {:axis \"main\" :date \"2026-06-16\" :scan-path \"/tmp/scan.edn\"
               :brief-path \"/tmp/brief.md\" :brief-item \"METR/eval-analysis-public\"}
 :source/daily-item {:kind :github-repo :full-name \"METR/eval-analysis-public\"}
 :interest-match {:adapter \"futon2.aif.adapters.interest-network\"
                  :territory-label \"held-space analytic practice\"
                  :score 0.72
                  :because [\"daily brief names METR\"]}
 :target {:target/id \"org:metr\" :target/type :org
          :name \"METR\" :org \"METR\" :person nil
          :contact-uri nil :public-url \"https://metr.org\"
          :relationship-path {:class-candidate :cold-scan-lead
                              :basis :only-known-path-is-cold-email
                              :notes \"Cold is relational, not topical.\"}}
 :routing {:lead-class-candidate :cold-scan-lead
           :plants-thesis-candidate :T2.3-cold
           :cold-relational-test {:operator-confirmed? nil}}
 :eoi {:flash \"eoi-outward\"}
 :strawman {:kind :merged-pr :url \"https://example.test/pr\" :title \"Merged PR warrant\"}
 :review {:state :needs-operator-review
          :john-hancock-send {:send-authorized? nil
                              :send-channel nil
                              :send-witness nil}}
 :send-projection {:event :outreach-sent
                   :plants-thesis :T2.3-cold
                   :lead-class :cold-scan-lead
                   :lead \"org:metr\"
                   :sent-at nil
                   :send-witness nil
                   :witness-sources [{:kind :daily-scan :path \"/tmp/scan.edn\"}]
                   :outcome nil
                   :projection {:from nil :to nil :redact [] :flatten [] :substitute [] :add []}}}"
  "Minimal staged.edn fixture covering send-gate rendering and transitions.")

(defmacro arxana-cold-outbox-test--with-fixture (&rest body)
  "Run BODY with a temporary staged outbox fixture."
  `(let* ((root (make-temp-file "arxana-cold-outbox-" t))
          (dir (expand-file-name "draft-1" root))
          (staged (expand-file-name "staged.edn" dir))
          (draft-md (expand-file-name "draft.md" dir)))
     (make-directory dir t)
     (with-temp-file staged
       (insert (replace-regexp-in-string
                "__DIR__" dir arxana-cold-outbox-test--staged
                t t)))
     (with-temp-file draft-md
       (insert "Hello METR,\n\nThis is the drafted body.\n"))
     (unwind-protect
         (let ((arxana-cold-outbox-root root))
           ,@body)
       (delete-directory root t))))

(defun arxana-cold-outbox-test--draft ()
  "Return the single fixture draft."
  (car (arxana-cold-outbox--drafts)))

(ert-deftest arxana-cold-outbox-read-and-classify-staged ()
  (arxana-cold-outbox-test--with-fixture
   (let ((draft (arxana-cold-outbox-test--draft)))
     (should draft)
     (should (equal "draft-1" (arxana-cold-outbox--id draft)))
     (should (eq :staged (arxana-cold-outbox--stage draft))))))

(ert-deftest arxana-cold-outbox-render-draft-shows-gate-material ()
  (arxana-cold-outbox-test--with-fixture
   (arxana-cold-outbox--render-draft "draft-1")
   (with-current-buffer arxana-cold-outbox--buffer
     (let ((txt (buffer-string)))
       (should (string-match-p "METR" txt))
       (should (string-match-p "Merged PR warrant" txt))
       (should (string-match-p "This is the drafted body" txt))
       (should (string-match-p "Cold is relational, not topical" txt))))))

(ert-deftest arxana-cold-outbox-mark-reviewed-surgical-edit ()
  (arxana-cold-outbox-test--with-fixture
   (let ((draft (arxana-cold-outbox-test--draft)))
     (arxana-cold-outbox--mark-reviewed-record draft)
     (let ((updated (arxana-cold-outbox-test--draft)))
       (should (eq :reviewed (plist-get (arxana-cold-outbox--data updated)
                                        :draft/status)))))))

(ert-deftest arxana-cold-outbox-constructs-send-event-and-command ()
  (arxana-cold-outbox-test--with-fixture
   (let* ((draft (arxana-cold-outbox-test--draft))
          (event (arxana-cold-outbox--outreach-event
                  draft "msg-id-123" "2026-06-16T12:00:00+0100"))
          (args (arxana-cold-outbox--intake-command "/tmp/event.edn")))
     (should (eq :outreach-sent (plist-get event :event)))
     (should (eq :T2.3-cold (plist-get event :plants-thesis)))
     (should (eq :cold-scan-lead (plist-get event :lead-class)))
     (should (equal "msg-id-123" (plist-get event :send-witness)))
     (should (equal "2026-06-16T12:00:00+0100" (plist-get event :sent-at)))
     (should (equal "bb" (car args)))
     (should (equal arxana-cold-outbox-pudding-prover (cadr args)))
     (should (equal "intake!" (caddr args)))
     (should (equal "/tmp/event.edn" (cadddr args))))))

(provide 'arxana-cold-outbox-test)
;;; arxana-cold-outbox-test.el ends here
