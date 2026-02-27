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

(provide 'arxana-check-parens-test)
;;; arxana-check-parens-test.el ends here
