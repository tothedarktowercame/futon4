;;; mojira-mission-patterns-test.el --- Offline tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'mojira-mission-patterns)

(ert-deftest mojira-mission-patterns-render-fixture ()
  (with-temp-buffer
    (mojira-mission-patterns-mode)
    (setq mojira-mission-patterns--agent "invented-1"
          mojira-mission-patterns--mission "M-invented"
          mojira-mission-patterns--scopes
          '((scopes . (((id . "m/pattern/p-shared") (pattern_id . "p-shared") (title . "Shared") (type . "pattern") (passage . "Example passage"))
                       ((id . "p-doc") (title . "Document") (type . "psr")))))
          mojira-mission-patterns--sessions
          '((turns_read . 3) (patterns . (((id . "p-only") (occurs . 8) (proposed . 9) (sessions . ("s1")))
                                        ((id . "p-shared") (occurs . 1) (proposed . 2) (sessions . ("s1" "s2")))))))
    (mojira-mission-patterns--render)
    (should (search-forward "In the mission document" nil t))
    (should (search-forward "★ Shared [pattern] · p-shared" nil t))
    (should (search-forward "p-doc · not used in sessions" nil t))
    (should (eq 'shadow (get-text-property (1- (point)) 'face)))
    (should (search-forward "In sessions (3 turns read)" nil t))
    (should (search-forward "★ p-shared · used 1 · proposed 2 · 2 sessions" nil t))
    (should (search-forward "p-only · used 8" nil t))
    (should (eq 'bold (get-text-property (1- (point)) 'face)))))

(ert-deftest mojira-mission-patterns-unclocked-and-error ()
  (dolist (reply '((nil nil "not clocked") (nil "refused" "Error: refused")))
    (with-temp-buffer
      (mojira-mission-patterns-mode)
      (setq mojira-mission-patterns--agent "invented-1")
      (cl-letf (((symbol-function 'mojira-mission-patterns--fetch)
                 (lambda (_path cb) (funcall cb (car reply) (cadr reply)))))
        (mojira-mission-patterns-refresh)
        (should (string-match-p (nth 2 reply) (buffer-string)))))))

(ert-deftest mojira-mission-patterns-network-is-callback-only ()
  (let (requests)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) (ert-fail "synchronous network")))
              ((symbol-function 'url-retrieve)
               (lambda (url cb &rest _)
                 (should (functionp cb))
                 (push (cons url cb) requests))))
      (with-temp-buffer
        (mojira-mission-patterns-mode)
        (setq mojira-mission-patterns--agent "invented-1")
        (mojira-mission-patterns-refresh)
        (should (equal (buffer-string) "loading…\n"))
        (should (= 1 (length requests)))
        (let ((cb (cdar requests)))
          (with-temp-buffer
            (setq-local url-http-response-status 200)
            (insert "HTTP/1.1 200 OK\r\n\r\n{\"mission-id\":\"M-invented\"}")
            (funcall cb nil)))
        ;; Both requests queued before either finishes.
        (should (= 3 (length requests)))
        (should (string-match-p "mission-session-patterns" (caar requests)))
        (should (string-match-p "mission-pattern-scopes" (car (cadr requests))))))))

(ert-deftest mojira-mission-patterns-stale-reply-ignored ()
  (with-temp-buffer
    (mojira-mission-patterns-mode)
    (setq mojira-mission-patterns--agent "invented-1")
    (let (callbacks)
      (cl-letf (((symbol-function 'mojira-mission-patterns--fetch)
                 (lambda (_path cb) (push cb callbacks))))
        (mojira-mission-patterns-refresh)
        (mojira-mission-patterns-refresh)
        (funcall (cadr callbacks) nil "old error")
        (should (equal "loading…\n" (buffer-string)))
        (funcall (car callbacks) nil "new error")
        (should (string-match-p "new error" (buffer-string)))))))

(ert-deftest mojira-mission-patterns-both-completions-update-target-buffer ()
  (with-temp-buffer
    (mojira-mission-patterns-mode)
    (setq mojira-mission-patterns--agent "invented-1")
    (let ((target (current-buffer)) callbacks)
      (cl-letf (((symbol-function 'mojira-mission-patterns--fetch)
                 (lambda (path cb) (push (cons path cb) callbacks))))
        (mojira-mission-patterns-refresh)
        (funcall (cdar callbacks) '((mission-id . "M-fixture")) nil)
        (let ((sessions (cdar callbacks)) (scopes (cdr (cadr callbacks))))
          (with-temp-buffer
            (funcall sessions '((turns_read . 2) (patterns . nil)) nil)
            (funcall scopes nil "scope failure"))
          (with-current-buffer target
            (should (string-match-p "In sessions (2 turns read)" (buffer-string)))
            (should (string-match-p "Error: scope failure" (buffer-string)))))))))

(ert-deftest mojira-mission-patterns-local-agent-selection ()
  (let ((a (generate-new-buffer " *invented-repl*"))
        (other (generate-new-buffer " *invented-editor*")))
    (unwind-protect
        (progn
          (with-current-buffer a
            (set (make-local-variable 'codex-repl-agency-agent-id) "codex-invented")
            (should (equal "codex-invented" (mojira-mission-patterns--agent))))
          (with-current-buffer other
            (cl-letf (((symbol-function 'buffer-list) (lambda (&rest _) (list other a))))
              (should (equal "codex-invented" (mojira-mission-patterns--agent))))))
      (kill-buffer a) (kill-buffer other))))

(ert-deftest mojira-mission-patterns-finds-and-opens-the-mission-document ()
  (let* ((root (make-temp-file "mmp-root" t))
         (mojira-mission-patterns-code-root root)
         (file (expand-file-name "repo-a/holes/missions/M-invented.md" root))
         (enabled nil))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert "# M-invented\n"))
    (make-directory (expand-file-name "repo-b/holes" root) t)
    (with-temp-file (expand-file-name "repo-b/holes/E-invented.md" root) (insert "# E\n"))
    (unwind-protect
        (progn
          (should (equal file (mojira-mission-patterns--mission-file "M-invented")))
          (should (string-suffix-p "repo-b/holes/E-invented.md"
                                   (mojira-mission-patterns--mission-file "E-invented")))
          (should-not (mojira-mission-patterns--mission-file "M-absent"))
          (cl-letf (((symbol-function 'mission-mode-minor-mode)
                     (lambda (&optional _)
                       (should (equal '("pattern" "psr" "pur") mission-mode-scope-types))
                       (setq enabled (current-buffer)))))
            (mojira-mission-patterns--open-document "M-invented"))
          (should (buffer-live-p enabled))
          (should (equal file (buffer-file-name enabled))))
      (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
      (delete-directory root t))))
