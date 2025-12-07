;;; arxana-inclusion-batch-test.el --- Batch regression for include/transclude -*- lexical-binding: t; -*-

(require 'ert)
(require 'arxana-inclusion)

(defun arxana-inclusion-test--with-temp-articles (body)
  (let ((buf-a (get-buffer-create " *Include A*"))
        (buf-b (get-buffer-create " *Include B*")))
    (with-current-buffer buf-a
      (erase-buffer)
      (insert "AAA"))
    (with-current-buffer buf-b
      (erase-buffer)
      (insert "BBB"))
    (unwind-protect
        (progn
          (with-current-buffer buf-a
            (let ((futon4-enable-sync nil))
              (make-current-buffer-into-article "Include-A")))
          (with-current-buffer buf-b
            (let ((futon4-enable-sync nil))
              (make-current-buffer-into-article "Include-B")))
          (funcall body buf-a buf-b))
      (when (buffer-live-p buf-a) (kill-buffer buf-a))
      (when (buffer-live-p buf-b) (kill-buffer buf-b)))))

(ert-deftest arxana-include-article-copies-text ()
  (arxana-inclusion-test--with-temp-articles
   (lambda (target _source)
     (message "names=%S" (turn-article-table-into-names))
     (with-current-buffer target
       (goto-char (point-max))
       (include-article "Include-B")
       (should (string-match-p "BBB" (buffer-string)))))))

(ert-deftest arxana-transclude-article-copies-text ()
  (arxana-inclusion-test--with-temp-articles
   (lambda (target _source)
     (message "names=%S" (turn-article-table-into-names))
     (with-current-buffer target
       (goto-char (point-max))
       (transclude-article "Include-B")
       (should (string-match-p "BBB" (buffer-string)))))))

(provide 'arxana-inclusion-batch-test)
;;; arxana-inclusion-batch-test.el ends here
