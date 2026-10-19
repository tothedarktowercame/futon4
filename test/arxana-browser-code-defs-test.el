;;; arxana-browser-code-defs-test.el --- Clojure definition recognition -*- lexical-binding: t; -*-

(require 'ert)

(defconst arxana-browser-code-defs-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(load-file (expand-file-name "dev/arxana-browser-code.el"
                             arxana-browser-code-defs-test--root))

(defconst arxana-browser-code-defs-test--elisp-oracle
  '("name-of-current-article" "arxana-browse--stringify" "arxana-browse--ensure"
    "arxana-browse-open-catalog" "arxana-browse-open-labels" "arxana-browse-follow-link"
    "arxana-browse-follow-article-about" "arxana-browse-follow-scholium-about"
    "arxana-browse-history-back" "arxana-browse-history-forward"
    "arxana-browse--parent-context" "arxana-browse-open-parent"
    "arxana-browse-parent-next" "arxana-browse-parent-previous"))

(defconst arxana-browser-code-defs-test--fixture
  "(defn run-opportunity! [] :ok)\n(defn- ^:private ^Private helper [] :ok)\n(def ^:dynamic *launch-route-check!* true)\n(def ^{:doc \"x\"} ^:dynamic table {})\n(defmulti dispatch :kind)\n(defmethod dispatch :k [_] :ok)\n(defprotocol Port (open! [this]))\n;; (defn commented-out [] :no)\n\"(defn in-a-string [])\"\n")

(ert-deftest arxana-browser-code-defs-recognizes-clojure-forms-and-metadata ()
  (let* ((dir (make-temp-file "arxana-clj-defs-" t))
         (path (expand-file-name "fixture.clj" dir))
         (arxana-browser-code--symbol-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file path (insert arxana-browser-code-defs-test--fixture))
          ;; Commented and non-leading string definitions are ignored because the
          ;; shared regexp requires a top-level opening paren at line start.
          (should (equal '("run-opportunity!" "helper" "*launch-route-check!*"
                           "table" "dispatch" "Port")
                         (arxana-browser-code--file-symbols path)))
          (with-temp-buffer
            (insert-file-contents path)
            (emacs-lisp-mode)
            (goto-char (point-min))
            (search-forward "*launch-route-check!*")
            (let ((bounds (arxana-browser-code--find-defun-bounds "*launch-route-check!*")))
              (should bounds)
              (should (equal "(def ^:dynamic *launch-route-check!* true)"
                             (string-trim-right
                              (buffer-substring-no-properties (car bounds) (cdr bounds)))))
              (goto-char (1+ (car bounds)))
              (should (equal "*launch-route-check!*"
                             (arxana-browser-code--current-def-symbol)))))
          (should (equal arxana-browser-code-defs-test--elisp-oracle
                         (arxana-browser-code--file-symbols
                          (expand-file-name "dev/arxana-browser-browse.el"
                                            arxana-browser-code-defs-test--root)))))
      (delete-directory dir t))))

(provide 'arxana-browser-code-defs-test)
;;; arxana-browser-code-defs-test.el ends here
