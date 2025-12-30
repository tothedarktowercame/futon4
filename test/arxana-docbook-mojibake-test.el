;;; arxana-docbook-mojibake-test.el --- Tests for docbook mojibake repair -*- lexical-binding: t; -*-

;;; Commentary:
;; Non-interactive test for UTF-8 text mis-decoded as Latin-1/Windows-1252.

;;; Code:

(require 'ert)

(let* ((base (or load-file-name buffer-file-name default-directory))
       (root (or (locate-dominating-file base "dev") base)))
  (add-to-list 'load-path (expand-file-name "dev" root))
  (load-file (expand-file-name "dev/arxana-docbook-core.el" root)))

(ert-deftest arxana-docbook-mojibake-fix-arrow ()
  (let* ((mojibake (string ?\xE2 ?\x86 ?\x92))
         (expected "\u2192")
         (actual (arxana-docbook--maybe-fix-mojibake mojibake)))
    (should (string= expected actual))))

(provide 'arxana-docbook-mojibake-test)
;;; arxana-docbook-mojibake-test.el ends here
