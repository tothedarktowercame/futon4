;;; mojira-related-turns-test.el --- Offline related-turn tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'mojira-related-turns)
(require 'mojira-dispatch)

(ert-deftest mojira-related-turns-render-fixture-order-and-buttons ()
  (with-temp-buffer
    (mojira-related-turns-mode)
    (setq mojira-related-turns--turn "turn-current"
          mojira-related-turns--data
          '((turn . "turn-current") (session . "session-current")
            (patterns "p/one")
            (related
             ((turn . "turn-related") (created_at . "2026-11-21T04:00:00Z")
              (speaker . "codex-4") (mission . "M-shared")
              (excerpt . "A related turn excerpt")
              (shared "p/one") (score . 2.5))))
          mojira-related-turns--limit 10)
    (mojira-related-turns--render)
    (goto-char (point-min))
    (should (search-forward "※ t x p · turn-current (1 patterns) · top 10" nil t))
    (should (search-forward "2.50  1 shared pattern  turn-related  codex-4  M-shared  2026-11-21T04:00:00Z  A related turn excerpt" nil t))
    (should (search-forward "    p/one" nil t))
    (should (button-at (progn (goto-char (point-min))
                              (search-forward "turn-related" nil t)
                              (- (point) (length "turn-related")))))
    (should (button-at (progn (goto-char (point-min))
                              (search-forward "    p/one" nil t)
                              (- (point) (length "    p/one")))))))

(ert-deftest mojira-related-turns-async-limit-and-empty-state ()
  (with-temp-buffer
    (mojira-related-turns-mode)
    (setq mojira-related-turns--turn "turn-current")
    (let (requests)
      (cl-letf (((symbol-function 'mojira-mission-patterns--fetch)
                 (lambda (path callback) (push (cons path callback) requests))))
        (mojira-related-turns-refresh 2)
        (should (string-match-p "turn-related-turns\\?turn=turn-current&n=3"
                                (car (car requests))))
        (funcall (cdr (car requests))
                 '((turn . "turn-current") (session . "session-current") (patterns "p")
                   (related ((turn . "turn-1") (shared "p") (score . 3.0))
                            ((turn . "turn-2") (shared "p") (score . 2.0))
                            ((turn . "turn-3") (shared "p") (score . 1.0))))
                 nil)
        (should (string-match-p "top 2 of 3" (buffer-string)))
        (should (string-match-p "turn-2" (buffer-string)))
        (should-not (string-match-p "turn-3" (buffer-string)))
        (setq mojira-related-turns--data
              '((turn . "turn-current") (patterns) (related)))
        (mojira-related-turns--render)
        (should (string-match-p "No related turns\\." (buffer-string)))))))

(ert-deftest mojira-related-turns-point-selection-falls-back-to-newest ()
  (with-temp-buffer
    (insert "old\nnew\n")
    (let ((old-beg (copy-marker (point-min)))
          (old-end (copy-marker (+ (point-min) 4)))
          (new-beg (copy-marker (+ (point-min) 4)))
          (new-end (copy-marker (point-max))))
      (setq-local session-mode--turn-text-regions
                  (list (cons "turn-new" (cons "agent" (cons new-beg new-end)))
                        (cons "turn-old" (cons "operator" (cons old-beg old-end)))))
      (goto-char 6)
      (should (equal "turn-new" (mojira-related-turns--turn-at-point)))
      (goto-char (point-max))
      (should (equal "turn-new" (mojira-related-turns--newest-turn))))))

(ert-deftest mojira-related-turns-dispatch-does-not-clash-with-t-kind ()
  (let ((mojira-codes-file "/home/joe/code/futon4/dev/mojira-codes.json") called)
    (mojira--load-table)
    (should (member "t x p" (mapcar #'car mojira-shortcodes)))
    (should (member "t" (mapcar #'car mojira-kinds)))
    (cl-letf (((symbol-function 'mojira-related-turns)
               (lambda () (interactive) (setq called t))))
      (mojira-dispatch "t x p")
      (should called))))

(ert-deftest mojira-related-turns-opens-captured-turn-in-open-repl ()
  (let ((buffer (generate-new-buffer " *turn-related*"))
        (target (generate-new-buffer " *turn-target*")))
    (unwind-protect
        (progn
          (with-current-buffer target
            (setq-local agent-chat--session-id "session-current")
            (insert "turn text")
            (setq-local session-mode--turn-text-regions
                        (list (cons "turn-related"
                                    (cons "agent"
                                          (cons (copy-marker (point-min))
                                                (copy-marker (point-max))))))))
          (with-current-buffer buffer
            (setq-local mojira-related-turns--session "session-current"))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (&rest _) (ert-fail "must move point without selecting a window"))))
            (with-current-buffer buffer
              (mojira-related-turns--open "turn-related")))
          (with-current-buffer target
            (should (= (point-min) (point))))))
      (kill-buffer buffer)
      (kill-buffer target)))
