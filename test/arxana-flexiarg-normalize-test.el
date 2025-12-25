;;; arxana-flexiarg-normalize-test.el --- Tests for flexiarg normalizer -*- lexical-binding: t; -*-

(require 'ert)
(load-file (expand-file-name "../dev/arxana-flexiarg-normalize.el"
                             (file-name-directory load-file-name)))

(defun arxana-flexiarg-normalize-test--normalize-string (text)
  (with-temp-buffer
    (insert text)
    (arxana-flexiarg--normalize-buffer)
    (buffer-string)))

(ert-deftest arxana-flexiarg-normalize-transforms-if-blocks ()
  (let* ((input "@flexiarg test/pattern\n@title Demo\n\nIF: Alpha\nHOWEVER: Beta\nTHEN: Gamma\nBECAUSE: Delta")
         (output (arxana-flexiarg-normalize-test--normalize-string input)))
    (should (string-match-p "! conclusion:" output))
    (should (string-match-p "\\+ IF:" output))
    (should (string-match-p "Gamma" output))))

(ert-deftest arxana-flexiarg-normalize-no-op-when-canonical ()
  (let ((input "@flexiarg demo\n\n! conclusion:\n  Something\n\n  + IF:\n    clause"))
    (with-temp-buffer
      (insert input)
      (should-not (arxana-flexiarg--normalize-buffer)))))

(ert-deftest arxana-flexiarg-normalize-directory-collects-stats ()
  (let* ((tmp (make-temp-file "flexiarg" t))
         (file (expand-file-name "demo.flexiarg" tmp)))
    (write-region "@flexiarg demo\n\nIF: Test" nil file)
    (arxana-flexiarg-normalize-directory tmp)
    (should (string-match-p "! conclusion" (with-temp-buffer
                                               (insert-file-contents file)
                                               (buffer-string))))))

(ert-deftest arxana-flexiarg-normalize-rewrites-alias-only ()
  (let* ((input "@flexiarg demo\n\n! instantiated-by: Example")
         (output (arxana-flexiarg-normalize-test--normalize-string input)))
    (should (string-match-p "! conclusion:" output))))

(provide 'arxana-flexiarg-normalize-test)
