;;; arxana-adjacency-test.el --- Tests for scholium adjacency syncing -*- lexical-binding: t; -*-

(require 'ert)
(require 'arxana-store)
(require 'arxana-test-support)

(add-to-list 'load-path (expand-file-name "dev" default-directory))

(arxana-test--ensure-tangled-loaded)

(arxana-store--install-relation-shim)

(ignore-errors (require 'arxana-article))

(ert-deftest arxana-adjacency-syncs-hyperedges ()
  (let ((futon4-enable-sync t)
        (calls nil))
    (cl-letf (((symbol-function 'futon4-lookup-article-id)
               (lambda (name)
                 (when (string= name "Source") "id-source")))
              ((symbol-function 'futon4--article-id-for)
               (lambda (name &optional _path)
                 (pcase name
                   ("Source" "id-source")
                   ("Target" "id-target")
                   ("Transcribed" "id-transcribed"))))
              ((symbol-function 'futon4-store-nema-simple)
               (lambda (src dst label &optional _cb)
                 (push (list src dst label) calls))))
      (futon4--sync-about-links
       "Source"
       '(("Target" (passage ("Source" 0 42)) (transclusion-of "Transcribed"))))
      (should (equal calls '(("id-source" "id-target"
                             "(passage (\"Source\" 0 42)) | (transclusion-of \"Transcribed\")")))))))

(ert-deftest arxana-adjacency-falls-back-to-relations ()
  (let ((futon4-enable-sync t)
        (relation-calls nil))
    (cl-letf (((symbol-function 'futon4-lookup-article-id)
               (lambda (name)
                 (when (string= name "Source") "id-source")))
              ((symbol-function 'futon4--article-id-for)
               (lambda (name &optional _path)
                 (pcase name
                   ("Source" "id-source")
                   ("Target" "id-target"))))
              ((symbol-function 'futon4-store-nema-simple)
               (lambda (src dst label &optional _cb)
                 (push (list src dst label) relation-calls))))
      (futon4--sync-about-links "Source" '(("Target")))
      (should (equal relation-calls '(("id-source" "id-target" "")))))))

(ert-deftest arxana-adjacency-skips-missing-targets ()
  (let ((futon4-enable-sync t)
        (relation-called nil))
    (cl-letf (((symbol-function 'futon4-lookup-article-id)
               (lambda (name)
                 (when (string= name "Source") "id-source")))
              ((symbol-function 'futon4--article-id-for)
               (lambda (name &optional _path)
                 (when (string= name "Source") "id-source")))
              ((symbol-function 'futon4-store-nema-simple)
               (lambda (&rest _)
                 (setq relation-called t))))
      (futon4--sync-about-links "Source" '(("Unknown" (passage 0 42))))
      (should-not relation-called))))

(provide 'arxana-adjacency-test)
;;; arxana-adjacency-test.el ends here
