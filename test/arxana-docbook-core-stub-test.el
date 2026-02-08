;;; arxana-docbook-core-stub-test.el --- Tests for stub parsing -*- lexical-binding: t; -*-

;;; Commentary:
;; Stub files record both a DOC_ID (logical document id) and an ENTRY_ID
;; (format-specific id; often DOC_ID::org). Ensure we don't conflate them.

;;; Code:

(require 'ert)

(let* ((base (or load-file-name buffer-file-name default-directory))
       (root (or (locate-dominating-file base "dev") base)))
  (add-to-list 'load-path (expand-file-name "dev" root))
  (load-file (expand-file-name "dev/arxana-docbook-core.el" root)))

(ert-deftest arxana-docbook-core-stub-parses-doc-and-entry-ids ()
  (let* ((root (make-temp-file "docbook-stub" t))
         (book "futon4")
         (path (expand-file-name "futon4-aaaa1111bbbb.org" root)))
    (with-temp-file path
      (insert "#+TITLE: Example\n"
              ":PROPERTIES:\n"
              ":DOC_ID: futon4-aaaa1111bbbb\n"
              ":ENTRY_ID: futon4-aaaa1111bbbb::org\n"
              ":VERSION: org\n"
              ":OUTLINE_PATH: Example\n"
              ":PATH_STRING: Example\n"
              ":END:\n\n"
              "* Context\n"
              "Hello\n"))
    (let ((entry (arxana-docbook--entry-from-stub book path)))
      (should (equal (plist-get entry :doc-id) "futon4-aaaa1111bbbb"))
      (should (equal (plist-get entry :entry-id) "futon4-aaaa1111bbbb::org"))
      (should (equal (plist-get entry :run-id) "futon4-aaaa1111bbbb::org")))))

(ert-deftest arxana-docbook-core-stub-derives-doc-id-from-entry-id ()
  (let* ((root (make-temp-file "docbook-stub" t))
         (book "futon4")
         (path (expand-file-name "futon4-cccc2222dddd.org" root)))
    (with-temp-file path
      (insert "#+TITLE: Example\n"
              ":PROPERTIES:\n"
              ":ENTRY_ID: futon4-cccc2222dddd::org\n"
              ":VERSION: org\n"
              ":END:\n\n"
              "* Context\n"
              "Hello\n"))
    (let ((entry (arxana-docbook--entry-from-stub book path)))
      (should (equal (plist-get entry :doc-id) "futon4-cccc2222dddd"))
      (should (equal (plist-get entry :entry-id) "futon4-cccc2222dddd::org")))))

(provide 'arxana-docbook-core-stub-test)
;;; arxana-docbook-core-stub-test.el ends here

