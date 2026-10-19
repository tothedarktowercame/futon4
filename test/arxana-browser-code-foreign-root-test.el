;;; arxana-browser-code-foreign-root-test.el --- Foreign Clojure roots -*- lexical-binding: t; -*-
(require 'ert)
(let ((root (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))
  (load-file (expand-file-name "dev/arxana-docbook-core.el" root))
  (load-file (expand-file-name "dev/arxana-browser-code.el" root)))
(ert-deftest arxana-browser-code-foreign-root-def-doc-items ()
  (let* ((dir (make-temp-file "arxana-foreign-" t)) (root (expand-file-name "src/futon2/aif" dir))
         (path (expand-file-name "meta_self_heal_production.clj" root))
         (entry (list :doc-id "foreign" :version "def-doc" :summary-raw
                      ":SOURCE_PATH: src/futon2/aif/meta_self_heal_production.clj\n- route!: route\n- production-readiness: ready\n- *seam*: seam\n- not-in-file!: bad\n")))
    (unwind-protect
        (progn (make-directory root t)
               (with-temp-file path (insert "(defn route! [] nil)\n(defn production-readiness [] nil)\n(def ^:dynamic *seam* nil)\n"))
               (let ((arxana-browser-code-roots (list root))
                     (arxana-browser-code--docbook-entry-cache nil) (arxana-browser-code--docbook-index-cache nil)
                     (arxana-browser-code--docbook-match-cache (make-hash-table :test 'equal)))
                 (cl-letf (((symbol-function 'arxana-browser-code--docbook-entries) (lambda () (list entry))))
                   (should (equal (list entry) (arxana-browser-code--docbook-matches path)))
                   (should (equal '("route!" "production-readiness" "*seam*")
                                  (mapcar (lambda (x) (plist-get x :symbol))
                                          (arxana-browser-code--entry-context-items entry)))))))
      (delete-directory dir t))))
(ert-deftest arxana-browser-code-def-doc-without-source-keeps-prefix-rule ()
  ;; Bad case from review: 8 of 32 futon4 def-doc entries have no SOURCE_PATH;
  ;; their bullets for `require' or `with-temp-buffer' must not become links.
  (let ((entry (list :doc-id "legacy" :version "def-doc" :summary-raw
                     "- arxana-foo: x\n- require: y\n- with-temp-buffer: z\n")))
    (should (equal '("arxana-foo") (arxana-browser-code--entry-def-doc-symbols entry)))))
(provide 'arxana-browser-code-foreign-root-test)
