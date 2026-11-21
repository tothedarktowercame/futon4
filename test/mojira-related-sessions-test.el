;;; mojira-related-sessions-test.el --- Offline related-session tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'mojira-related-sessions)
(require 'mojira-dispatch)

(ert-deftest mojira-related-sessions-render-fixture-order-and-buttons ()
  (with-temp-buffer
    (mojira-related-sessions-mode)
    (setq mojira-related-sessions--session "session-current"
          mojira-related-sessions--data
          '((session . "session-current")
            (patterns "p/one" "p/two")
            (related
             ((session . "session-related") (agent . "codex-4")
              (mission . "M-shared") (latest_at . "2026-11-21T04:00:00Z")
              (latest_turn . "turn-7") (shared "p/one") (score . 2.5))))
          mojira-related-sessions--limit 10)
    (mojira-related-sessions--render)
    (goto-char (point-min))
    (should (search-forward "※ s x p · session-current (2 patterns) · top 10" nil t))
    (should (search-forward "2.50  1 shared pattern  session-related  codex-4  M-shared  2026-11-21T04:00:00Z" nil t))
    (should (search-forward "    p/one" nil t))
    (should (button-at (progn (goto-char (point-min))
                              (search-forward "session-related" nil t)
                              (- (point) (length "session-related")))))
    (should (button-at (progn (goto-char (point-min))
                              (search-forward "    p/one" nil t)
                              (- (point) (length "    p/one")))))))

(ert-deftest mojira-related-sessions-async-limit-and-empty-state ()
  (with-temp-buffer
    (mojira-related-sessions-mode)
    (setq mojira-related-sessions--session "session-current")
    (let (requests)
      (cl-letf (((symbol-function 'mojira-mission-patterns--fetch)
                 (lambda (path callback) (push (cons path callback) requests))))
        (mojira-related-sessions-refresh 2)
        (should (string-match-p "session-related-sessions\\?session=session-current&since-hours=24&n=3"
                                (car (car requests))))
        (funcall (cdr (car requests))
                 '((session . "session-current") (patterns "p")
                   (related ((session . "s-1") (shared "p") (score . 3.0))
                            ((session . "s-2") (shared "p") (score . 2.0))
                            ((session . "s-3") (shared "p") (score . 1.0))))
                 nil)
        (should (string-match-p "top 2 of 3" (buffer-string)))
        (should (string-match-p "s-2" (buffer-string)))
        (should-not (string-match-p "s-3" (buffer-string)))
        (setq mojira-related-sessions--data
              '((session . "session-current") (patterns) (related)))
        (mojira-related-sessions--render)
        (should (string-match-p "No related sessions\\." (buffer-string)))))))

(ert-deftest mojira-related-sessions-dispatch-does-not-clash-with-s-kind ()
  (let ((mojira-codes-file "/home/joe/code/futon4/dev/mojira-codes.json") called)
    (mojira--load-table)
    (should (member "s x p" (mapcar #'car mojira-shortcodes)))
    (should (member "s" (mapcar #'car mojira-kinds)))
    (cl-letf (((symbol-function 'mojira-related-sessions)
               (lambda () (interactive) (setq called t))))
      (mojira-dispatch "s x p")
      (should called))))

(ert-deftest mojira-related-sessions-opens-local-session-buffer ()
  (let ((buffer (generate-new-buffer " *session-related*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local agent-chat--session-id "session-related"))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (candidate) (should (eq candidate buffer))))
                    ((symbol-function 'claude-repl-find-buffer-by-session-id)
                     (lambda (_) nil)))
            (mojira-related-sessions--open-session "session-related")))
      (kill-buffer buffer))))
