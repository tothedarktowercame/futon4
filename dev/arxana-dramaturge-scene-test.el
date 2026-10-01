;;; arxana-dramaturge-scene-test.el --- ERT for pure scene helpers -*- lexical-binding: t; -*-

;; Run:
;;   emacs -Q --batch -L /home/joe/code/futon4/dev \
;;     -l arxana-dramaturge.el -l arxana-dramaturge-scene.el \
;;     -l arxana-dramaturge-scene-test.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'arxana-dramaturge-scene)

(ert-deftest scene-read-ndjson-parses-lines-in-order ()
  (let ((events (arxana-dramaturge-scene--read-ndjson
                 "/home/joe/code/futon3c/emacs/scenes/streams/stub-hello.ndjson")))
    (should (= 3 (length events)))
    (should (equal "assistant" (plist-get (nth 0 events) :type)))
    (should (equal "success" (plist-get (nth 2 events) :subtype)))))

(ert-deftest scene-format-report-renders-statuses ()
  (let* ((results (list (list :name 'ok :pass? t :elapsed-ms 1 :doc "d"
                              :steps (list (list :status 'pass :form '(key "r"))))
                        (list :name 'bad :pass? nil :elapsed-ms 2 :doc "d"
                              :steps (list (list :status 'timed-out
                                                 :form '(await p :timeout 1)
                                                 :detail "still nil")))))
         (report (arxana-dramaturge-scene--format-report results)))
    (should (string-match-p "scene ok: PASS" report))
    (should (string-match-p "scene bad: FAIL" report))
    (should (string-match-p "timed-out" report))))

(ert-deftest scene-expected-fail-accounting ()
  (should (arxana-dramaturge-scene--counts-as-pass '(:pass? nil) t))
  (should-not (arxana-dramaturge-scene--counts-as-pass '(:pass? t) t))
  (should (arxana-dramaturge-scene--counts-as-pass '(:pass? t) nil))
  (should-not (arxana-dramaturge-scene--counts-as-pass '(:pass? nil) nil)))

(ert-deftest scene-key-step-fails-on-unbound-key ()
  "The key step must fail, not silently no-op, on an undefined key —
the shape of the r/R reload bug."
  (let ((arxana-dramaturge-scene--steps nil)
        (arxana-dramaturge-scene--buffer (get-buffer-create "*scene-ert*")))
    (with-current-buffer arxana-dramaturge-scene--buffer
      (fundamental-mode))
    (arxana-dramaturge-scene--key "C-x M-z never-bound")
    (should (eq 'fail (plist-get (car arxana-dramaturge-scene--steps) :status)))
    (kill-buffer "*scene-ert*")))

(ert-deftest scene-await-timeout-status ()
  (let ((arxana-dramaturge-scene--steps nil))
    (arxana-dramaturge-scene--await (lambda () nil) 0.3)
    (should (eq 'timed-out (plist-get (car arxana-dramaturge-scene--steps) :status)))))

(ert-deftest scene-type-inserts-space ()
  "Regression: (kbd \" \") is the empty sequence; typing a space must
still insert it (futon4 dev, 2026-09-30)."
  (let ((arxana-dramaturge-scene--steps nil)
        (arxana-dramaturge-scene--buffer (get-buffer-create "*scene-ert-spc*")))
    (with-current-buffer arxana-dramaturge-scene--buffer
      (erase-buffer) (fundamental-mode))
    (arxana-dramaturge-scene--type "a b")
    (should (equal "a b" (with-current-buffer arxana-dramaturge-scene--buffer
                           (buffer-string))))
    (kill-buffer "*scene-ert-spc*")))

;;; arxana-dramaturge-scene-test.el ends here
