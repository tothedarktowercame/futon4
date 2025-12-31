;;; arxana-docbook-checkout-test.el --- Tests for docbook checkout -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(require 'arxana-docbook-checkout)

(defun arxana-docbook-checkout-test--heading (doc-id title entry)
  (list :type 'docbook-heading
        :doc-id doc-id
        :title title
        :outline (list title)
        :path_string title
        :level 1
        :latest entry))

(defun arxana-docbook-checkout-test--remote-entry (doc-id entry-id timestamp)
  (list :doc/id doc-id
        :doc/entry-id entry-id
        :doc/book "futon4"
        :doc/version "v1"
        :doc/timestamp timestamp
        :doc/summary "Doc summary"
        :doc/context "Context block"
        :doc/delta "Delta block"
        :doc/files '("dev/example.el")))

(ert-deftest arxana-docbook-checkout-writes-files ()
  (let* ((root (make-temp-file "docbook-checkout" t))
         (arxana-docbook-books-root root)
         (heading (arxana-docbook-checkout-test--heading
                   "doc-1" "Doc One"
                   (arxana-docbook-checkout-test--remote-entry
                    "doc-1" "run-1" "2024-01-01T00:00:00Z"))))
    (cl-letf (((symbol-function 'arxana-docbook--remote-available-p)
               (lambda (&rest _args) t))
              ((symbol-function 'arxana-docbook--remote-contents)
               (lambda (_book) (list heading)))
              ((symbol-function 'arxana-docbook--remote-heading)
               (lambda (&rest _args) nil)))
      (arxana-docbook-checkout-book "futon4" nil)
      (let* ((book-dir (expand-file-name "futon4" root))
             (raw (expand-file-name "raw/run-1.json" book-dir))
             (stub (expand-file-name "stubs/run-1.org" book-dir))
             (toc (expand-file-name "toc.json" book-dir)))
        (should (file-readable-p raw))
        (should (file-readable-p stub))
        (should (file-readable-p toc))
        (let* ((json-object-type 'plist)
               (json-array-type 'list)
               (json-key-type 'keyword)
               (payload (json-read-file raw)))
          (should (equal (plist-get payload :doc_id) "doc-1"))
          (should (equal (plist-get payload :run_id) "run-1"))
          (should (equal (plist-get payload :version) "v1")))
        (with-temp-buffer
          (insert-file-contents stub)
          (should (string-match-p "#\\+TITLE: Doc One" (buffer-string)))
          (should (string-match-p "\\* Context" (buffer-string)))
          (should (string-match-p "Context block" (buffer-string))))))))

(ert-deftest arxana-docbook-checkout-skips-newer-local ()
  (let* ((root (make-temp-file "docbook-checkout" t))
         (arxana-docbook-books-root root)
         (heading (arxana-docbook-checkout-test--heading
                   "doc-1" "Doc One"
                   (arxana-docbook-checkout-test--remote-entry
                    "doc-1" "run-1" "2020-01-01T00:00:00Z"))))
    (let* ((book-dir (expand-file-name "futon4" root))
           (raw (expand-file-name "raw/run-1.json" book-dir))
           (stub (expand-file-name "stubs/run-1.org" book-dir)))
      (make-directory (file-name-directory raw) t)
      (make-directory (file-name-directory stub) t)
      (with-temp-file raw
        (insert "{\"doc_id\":\"doc-1\",\"run_id\":\"run-1\",\"version\":\"local\"}\n"))
      (with-temp-file stub
        (insert "#+TITLE: Local stub\n"))
      (set-file-times raw (current-time))
      (set-file-times stub (current-time))
      (cl-letf (((symbol-function 'arxana-docbook--remote-available-p)
                 (lambda (&rest _args) t))
                ((symbol-function 'arxana-docbook--remote-contents)
                 (lambda (_book) (list heading)))
                ((symbol-function 'arxana-docbook--remote-heading)
                 (lambda (&rest _args) nil)))
        (let* ((before (with-temp-buffer
                         (insert-file-contents raw)
                         (buffer-string)))
               (result (arxana-docbook-checkout-book "futon4" nil))
               (after (with-temp-buffer
                        (insert-file-contents raw)
                        (buffer-string))))
          (should (equal before after))
          (should (equal (plist-get result :skipped) 1)))))))

(provide 'arxana-docbook-checkout-test)
;;; arxana-docbook-checkout-test.el ends here
