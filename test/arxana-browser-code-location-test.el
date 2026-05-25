;;; arxana-browser-code-location-test.el --- Tests for code/docs root resolution -*- lexical-binding: t; -*-

;;; Commentary:
;; Guard against `/dev` being mistaken for a Futon repo root.

;;; Code:

(require 'ert)

(defun arxana-browser-code-location-test--canonical-dir (path)
  (directory-file-name (file-truename (expand-file-name path))))

(let* ((base (or load-file-name buffer-file-name))
       (root (and base
                  (expand-file-name ".." (file-name-directory base)))))
  (load-file (expand-file-name "dev/arxana-docbook-core.el" root))
  (load-file (expand-file-name "dev/arxana-docbook.el" root))
  (load-file (expand-file-name "dev/arxana-browser-code.el" root))
  (load-file (expand-file-name "dev/arxana-browser.el" root))
  (load-file (expand-file-name "dev/arxana-browser-core.el" root))
  (load-file (expand-file-name "dev/arxana-docbook-ui.el" root)))

(ert-deftest arxana-browser-code-location-resolves-repo-root-outside-repo-default-directory ()
  (let ((arxana-docbook-books-root nil)
        (arxana-browser-code-docbook "futon4")
        (load-file-name nil)
        (buffer-file-name nil)
        (default-directory "/home/joe/code/"))
    (should (equal (arxana-browser-code-location-test--canonical-dir
                    "/home/joe/code/futon4/")
                   (arxana-browser-code-location-test--canonical-dir
                    (arxana-browser-code--repo-root))))
    (should (equal (list (expand-file-name "/home/joe/code/futon4/dev")
                         (expand-file-name "/home/joe/code/futon4/test"))
                   (arxana-browser-code--resolve-roots)))))

(ert-deftest arxana-browser-code-location-finds-doc-match-outside-repo-default-directory ()
  (let ((arxana-docbook-books-root nil)
        (arxana-browser-code-docbook "futon4")
        (arxana-browser-code--docbook-entry-cache nil)
        (arxana-browser-code--docbook-index-cache nil)
        (arxana-browser-code--docbook-match-cache (make-hash-table :test 'equal))
        (load-file-name nil)
        (buffer-file-name nil)
        (default-directory "/home/joe/code/"))
    (should (equal "path"
                   (arxana-browser-code--docbook-match-label
                    "/home/joe/code/futon4/dev/arxana-data-constraints.el")))
    (should (> (length (arxana-browser-code--docbook-matches
                        "/home/joe/code/futon4/dev/arxana-data-constraints.el"))
               0))))

(ert-deftest arxana-docbook-open-uri-code-route-anchors-futon4-docbook ()
  (let ((buf (generate-new-buffer " *arxana-code-route*"))
        (set-docbook nil)
        (arxana-browser--buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'arxana-browser-browse)
                   (lambda ()
                     (setq arxana-browser--buffer (buffer-name buf))))
                  ((symbol-function 'arxana-browser--render) #'ignore)
                  ((symbol-function 'arxana-ui-refresh) #'ignore)
                  ((symbol-function 'arxana-browser-code-set-docbook)
                   (lambda (book)
                     (setq set-docbook book))))
          (arxana-docbook-open-uri "arxana://view/code")
          (with-current-buffer buf
            (should (equal "futon4" set-docbook))
            (should (equal 'code (plist-get (car arxana-browser--stack) :view)))
            (should (equal "futon4" (plist-get (car arxana-browser--stack) :docbook)))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(provide 'arxana-browser-code-location-test)
;;; arxana-browser-code-location-test.el ends here
