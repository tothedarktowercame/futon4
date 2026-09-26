;;; arxana-check-parens-test.el --- Tests for check-parens CLI parser -*- lexical-binding: t; -*-

(require 'ert)
(load-file (expand-file-name "../dev/check-parens.el"
                             (file-name-directory load-file-name)))

(ert-deftest arxana-check-parens-parse-cli-args-parses-flags-and-files ()
  (let ((command-line-args-left
         '("emacs" "-Q" "--"
           "--json"
           "--no-defaults"
           "--strategy=read"
           "--context" "4"
           "dev/a.el"
           "test/b.el")))
    (let ((opts (arxana-check-parens--parse-cli-args)))
      (should (equal (plist-get opts :files) '("dev/a.el" "test/b.el")))
      (should (equal (plist-get opts :strategy) "read"))
      (should (eq (plist-get opts :json) t))
      (should (eq (plist-get opts :no-defaults) t))
      (should (= (plist-get opts :context) 4)))))

(ert-deftest arxana-check-parens-parse-cli-args-ignores-pre-separator ()
  (let ((command-line-args-left
         '("--json" "dev/ignored.el" "--" "--strategy" "both" "dev/used.el")))
    (let ((opts (arxana-check-parens--parse-cli-args)))
      (should (equal (plist-get opts :files) '("dev/used.el")))
      (should (equal (plist-get opts :strategy) "both"))
      (should-not (plist-get opts :json)))))

(ert-deftest arxana-check-parens-parse-cli-args-ignores-unknown-flags ()
  (let ((command-line-args-left
         '("--" "--bogus" "--context=2" "dev/file.el")))
    (let ((opts (arxana-check-parens--parse-cli-args)))
      (should (equal (plist-get opts :files) '("dev/file.el")))
      (should (= (plist-get opts :context) 2)))))

;; With no `--` separator at all, every argument is a candidate: the caller
;; cannot have meant "ignore all of these", and treating them as pre-separator
;; noise is what made the bare `-l check-parens.el FILE' invocation check
;; nothing (claude-5, 2026-09-23).
(ert-deftest arxana-check-parens-parse-cli-args-without-separator-takes-files ()
  (let ((command-line-args-left '("dev/one.el" "dev/two.el")))
    (let ((opts (arxana-check-parens--parse-cli-args)))
      (should (equal (plist-get opts :files) '("dev/one.el" "dev/two.el"))))))

;; The bad case the gate is named for, run the way it is actually typed.
;; `emacs --batch -l check-parens.el FILE' used to load the definitions, visit
;; FILE, print nothing and exit 0 for ANY file — so an unbalanced defun cleared
;; it. A subprocess is the only honest way to test this: the defect was in what
;; loading the file does, not in any function it defines.
(ert-deftest arxana-check-parens-bare-batch-invocation-catches-unbalanced ()
  (let* ((dir (make-temp-file "check-parens-test" t))
         (bad (expand-file-name "bad.el" dir))
         (good (expand-file-name "good.el" dir))
         (script (expand-file-name "dev/check-parens.el"
                                   (locate-dominating-file
                                    (or load-file-name buffer-file-name default-directory)
                                    "dev"))))
    (unwind-protect
        (progn
          (with-temp-file bad
            (insert "(defun f ()\n  (let ((a 1)\n        (b 2)\n    (+ a b)))\n"))
          (with-temp-file good
            (insert "(defun f ()\n  (let ((a 1)\n        (b 2))\n    (+ a b)))\n"))
          (should (= 1 (call-process (expand-file-name invocation-name
                                                       invocation-directory)
                                     nil nil nil "--batch" "-l" script bad)))
          (should (= 0 (call-process (expand-file-name invocation-name
                                                       invocation-directory)
                                     nil nil nil "--batch" "-l" script good))))
      (delete-directory dir t))))

(provide 'arxana-check-parens-test)
;;; arxana-check-parens-test.el ends here

;; claude-5, 2026-09-23, on kimi-2's report. 609bede made the read-scan run
;; even when the parens balance — right for Emacs Lisp, and it turned the gate
;; into a hard failure on ordinary CLOJURE, because the elisp reader cannot
;; read `#{}', `#""' or `#()' and signals `Invalid read syntax: "#"'. Worse,
;; a read-kind problem carries :form-start-line and no :line, so the printer
;; printed NOTHING and the CLI still exited 1: futon2 full_loop_runner.clj and
;; futon3c runner_service.clj both failed in total silence while being
;; perfectly readable. Clojure and EDN now get a syntax-state scan that finds
;; what the reader scan was there for — an unterminated string, which balances
;; fine and still breaks the file.
(defun arxana-check-parens-test--run-on (content ext)
  "Write CONTENT to a temp file with EXT, run the gate on it, return (code . output)."
  (let* ((dir (make-temp-file "cp-lang" t))
         (file (expand-file-name (concat "t." ext) dir))
         (script (expand-file-name "dev/check-parens.el"
                                   (locate-dominating-file
                                    (or load-file-name buffer-file-name default-directory)
                                    "dev")))
         (out (generate-new-buffer " *cp*")))
    (unwind-protect
        (progn
          (with-temp-file file (insert content))
          (cons (call-process (expand-file-name invocation-name invocation-directory)
                              nil out nil "--batch" "-l" script file)
                (with-current-buffer out (buffer-string))))
      (kill-buffer out)
      (delete-directory dir t))))

(ert-deftest arxana-check-parens-clojure-reader-macros-are-not-failures ()
  (dolist (src '("(ns a.b)\n(def s #{:a :b})\n"
                 "(ns a.b)\n(def r #\"[0-9]+\")\n"
                 "(ns a.b)\n(def f #(inc %))\n"))
    (let ((r (arxana-check-parens-test--run-on src "clj")))
      (should (= 0 (car r)))
      (should (string-match-p "OK" (cdr r))))))

(ert-deftest arxana-check-parens-unterminated-string-fails-in-both-languages ()
  ;; parens balance in each; only the open string is wrong
  (let ((clj (arxana-check-parens-test--run-on "(ns a.b)\n(def s \"open\n(def t 1)\n" "clj"))
        (el (arxana-check-parens-test--run-on "(defun f ()\n  (message \"open\n  1)\n" "el")))
    (should (= 1 (car clj)))
    (should (string-match-p "Unterminated string" (cdr clj)))
    (should (= 1 (car el)))))

(ert-deftest arxana-check-parens-never-exits-non-zero-in-silence ()
  ;; the failure mode that hid the Clojure breakage: exit 1, no output
  (let ((r (arxana-check-parens-test--run-on "(ns a.b)\n(def s \"open\n" "clj")))
    (should (= 1 (car r)))
    (should (string-match-p "[^ \t\n]" (cdr r)))
    ;; and it says where
    (should (string-match-p ":[0-9]+:" (cdr r)))))

;; claude-5, 2026-09-24. Handing a markdown file to the tool used to be silent;
;; once the batch auto-run landed it became a LOUD wrong failure — an ordinary
;; `# heading' read as `Invalid read syntax: "#"'. Skip what this tool cannot
;; read, say which, and keep checking the rest.
(ert-deftest arxana-check-parens-skips-non-lisp-targets ()
  (let* ((dir (make-temp-file "cp-skip" t))
         (md (expand-file-name "note.md" dir))
         (good (expand-file-name "good.clj" dir))
         (bad (expand-file-name "bad.clj" dir))
         (script (expand-file-name "dev/check-parens.el"
                                   (locate-dominating-file
                                    (or load-file-name buffer-file-name default-directory)
                                    "dev")))
         (emacs (expand-file-name invocation-name invocation-directory))
         (run (lambda (&rest args)
                (let ((out (generate-new-buffer " *cp*")))
                  (unwind-protect
                      (cons (apply #'call-process emacs nil out nil
                                   "--batch" "-l" script args)
                            (with-current-buffer out (buffer-string)))
                    (kill-buffer out))))))
    (unwind-protect
        (progn
          (with-temp-file md (insert "# A heading\n\nProse with #{braces} in it.\n"))
          (with-temp-file good (insert "(ns a.b)\n(def s #{:a})\n"))
          (with-temp-file bad (insert "(ns a.b)\n(def s \"open\n"))
          ;; markdown alone is not a pass and not a parse error: nothing to check
          (let ((r (funcall run md)))
            (should (= 2 (car r)))
            (should (string-match-p "No Lisp or Clojure files" (cdr r))))
          ;; mixed with a good Clojure file: skip is announced, result is OK
          (let ((r (funcall run md good)))
            (should (= 0 (car r)))
            (should (string-match-p "skipped" (cdr r)))
            (should (string-match-p "OK" (cdr r))))
          ;; mixed with a broken one: the break still fails the run
          (let ((r (funcall run md bad)))
            (should (= 1 (car r)))
            (should (string-match-p "Unterminated string" (cdr r)))))
      (delete-directory dir t))))
