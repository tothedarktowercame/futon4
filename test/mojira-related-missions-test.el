;;; mojira-related-missions-test.el --- Offline related-missions tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'mojira-related-missions)
(require 'mojira-dispatch)

(ert-deftest mojira-related-missions-render-fixture-in-order-and-buttons ()
  (with-temp-buffer
    (mojira-related-missions-mode)
    (setq mojira-related-missions--mission "M-arxana-together"
          mojira-related-missions--data
          '((mission . "M-arxana-together")
            (patterns "agency/self-attribution" "sidecar/append-only-semantic-audit")
            (related
             ((mission . "M-first") (shared "agency/self-attribution") (score . 2.5))
             ((mission . "M-second") (shared "sidecar/append-only-semantic-audit" "futon-theory/single-source-of-truth") (score . 1.0)))))
    (mojira-related-missions--render)
    (goto-char (point-min))
    (should (search-forward "※ m x p · M-arxana-together (2 patterns)" nil t))
    (should (search-forward "2.50  M-first  (1 shared patterns)" nil t))
    (should (search-forward "    agency/self-attribution" nil t))
    (should (search-forward "1.00  M-second  (2 shared patterns)" nil t))
    (should (search-forward "    sidecar/append-only-semantic-audit" nil t))
    (let (buttons)
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (when (button-at (point))
            (let ((button (button-at (point))))
              (push button buttons)
              (goto-char (button-end button))))
          (unless (button-at (point)) (forward-char 1))))
      (setq buttons (nreverse buttons))
      (should (= 5 (length buttons)))
      (should (equal "M-first" (button-get (nth 0 buttons) 'mojira-mission)))
      (should (equal "agency/self-attribution" (button-get (nth 1 buttons) 'mojira-pattern))))))

(ert-deftest mojira-related-missions-empty-related-is-readable ()
  (with-temp-buffer
    (mojira-related-missions-mode)
    (setq mojira-related-missions--mission "M-empty"
          mojira-related-missions--data '((mission . "M-empty") (patterns)))
    (mojira-related-missions--render)
    (should (string-match-p "No related missions\\." (buffer-string)))))

(ert-deftest mojira-related-missions-endpoint-error-is-text ()
  (with-temp-buffer
    (mojira-related-missions-mode)
    (setq mojira-related-missions--mission "M-error"
          mojira-related-missions--data '((error . "connection refused")))
    (mojira-related-missions--render)
    (should (string-match-p "Error: connection refused" (buffer-string)))))

(ert-deftest mojira-related-missions-async-fetch-and-side-window ()
  (let (callbacks displayed)
    (cl-letf (((symbol-function 'display-buffer-in-side-window)
               (lambda (buffer _alist) (setq displayed buffer)))
              ((symbol-function 'mojira-mission-patterns--fetch)
               (lambda (_path callback) (push callback callbacks))))
      (cl-letf (((symbol-function 'mojira-mission-patterns--agent)
                 (lambda () "agent-1")))
        (mojira-related-missions)
        (let ((buffer (get-buffer "*※ m x p*")))
          (unwind-protect
              (with-current-buffer buffer
                (should (equal "loading…\n" (buffer-string)))
                (funcall (car callbacks) '((mission-id . "M-fixture")) nil)
                (should (= 2 (length callbacks)))
                (funcall (car callbacks)
                         '((mission . "M-fixture") (patterns "p")
                           (related ((mission . "M-other") (shared "p") (score . 3.0)))) nil)
                (should (equal buffer displayed))
                (should (string-match-p "M-other" (buffer-string))))
            (when (buffer-live-p buffer) (kill-buffer buffer))))))))

(ert-deftest mojira-related-missions-dispatch-code-does-not-clash-with-m-p ()
  (let ((mojira-codes-file "/home/joe/code/futon4/dev/mojira-codes.json")
        called)
    (mojira--load-table)
    (let ((codes (mapcar #'car mojira-shortcodes)))
      (should (member "m p" codes))
      (should (member "m x p" codes))
      (should-not (string-prefix-p "m p " "m x p")))
    (cl-letf (((symbol-function 'mojira-related-missions)
               (lambda () (interactive) (setq called t))))
      (mojira-dispatch "m x p")
      (should called))))

(ert-deftest mojira-related-missions-opens-mission-without-type-filter ()
  (let* ((root (make-temp-file "mrm-root" t))
         (mojira-mission-patterns-code-root root)
         (file (expand-file-name "repo/holes/missions/M-other.md" root))
         (activated nil))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert "# M-other\n"))
    (unwind-protect
        (cl-letf (((symbol-function 'mission-mode-minor-mode)
                   (lambda (&optional _) (setq activated mission-mode-scope-types)))
                  ((symbol-function 'display-buffer)
                   (lambda (&rest _) nil)))
          (mojira-related-missions--open-mission "M-other")
          (should (null activated)))
      (when (get-file-buffer file) (kill-buffer (get-file-buffer file)))
      (delete-directory root t))))
