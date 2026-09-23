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
