;;; arxana-essays-diachronic-test.el --- Tests for essay live round seeding -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-browser-essays)

(defconst arxana-essays-diachronic-test--hyperreal-source
  "/home/joe/npt/applications/hyperreal-director-side-a/hyperreal-director-side-a-v1.md")

(defconst arxana-essays-diachronic-test--hyperreal-manifest
  "/home/joe/npt/applications/hyperreal-director-side-a/annotations.el")

(defconst arxana-essays-diachronic-test--hyperreal-edn
  "/home/joe/npt/applications/hyperreal-director-side-a/eoi-annotations.edn")

(defconst arxana-essays-diachronic-test--hyperreal-seed-id
  "hyperreal-director-side-a-v1")

(defconst arxana-essays-diachronic-test--hyperreal-essay-id
  "arxana/essay/hyperreal-director-side-a-v1")

(defun arxana-essays-diachronic-test--file-text (path)
  "Return literal text from PATH."
  (with-temp-buffer
    (insert-file-contents-literally path)
    (buffer-string)))

(defun arxana-essays-diachronic-test--copy-hyperreal-seed (dir)
  "Copy the Hyperreal golden seed files into DIR and return a seed plist."
  (make-directory dir t)
  (let ((md (expand-file-name
             (file-name-nondirectory
              arxana-essays-diachronic-test--hyperreal-source)
             dir))
        (manifest (expand-file-name
                   (file-name-nondirectory
                    arxana-essays-diachronic-test--hyperreal-manifest)
                   dir))
        (edn (expand-file-name
              (file-name-nondirectory
               arxana-essays-diachronic-test--hyperreal-edn)
              dir)))
    (copy-file arxana-essays-diachronic-test--hyperreal-source md t)
    (copy-file arxana-essays-diachronic-test--hyperreal-manifest manifest t)
    (copy-file arxana-essays-diachronic-test--hyperreal-edn edn t)
    (list :seed-id arxana-essays-diachronic-test--hyperreal-seed-id
          :essay-id arxana-essays-diachronic-test--hyperreal-essay-id
          :md md
          :manifest manifest
          :edn edn)))

(ert-deftest arxana-essays-diachronic-mint-live-round-copies-seed-immutably ()
  (let* ((root (make-temp-file "arxana-essays-diachronic-" t))
         (seed-dir (expand-file-name "seed" root))
         (rounds-root (expand-file-name "rounds" root))
         (seed (arxana-essays-diachronic-test--copy-hyperreal-seed seed-dir))
         (seed-md (plist-get seed :md))
         (seed-manifest (plist-get seed :manifest))
         (md-before (arxana-browser-essays--file-sha256 seed-md))
         (manifest-before (arxana-browser-essays--file-sha256 seed-manifest))
         (md-text (arxana-essays-diachronic-test--file-text seed-md))
         (manifest-text
          (arxana-essays-diachronic-test--file-text seed-manifest))
         (store-called nil))
    (unwind-protect
        (let ((arxana-browser-essays-diachronic-seeds (list seed))
              (arxana-browser-essays-rounds-root rounds-root))
          (cl-letf (((symbol-function 'arxana-store-sync-enabled-p)
                     (lambda () nil))
                    ((symbol-function 'arxana-store-fetch-entity)
                     (lambda (&rest _)
                       (setq store-called t)
                       (error "XTDB fetch should be skipped when sync is off")))
                    ((symbol-function 'arxana-store-ensure-entity)
                     (lambda (&rest _)
                       (setq store-called t)
                       (error "XTDB upsert should be skipped when sync is off"))))
            (let ((round-a
                   (arxana-browser-essays--mint-live-round
                    arxana-essays-diachronic-test--hyperreal-seed-id))
                  (round-b
                   (arxana-browser-essays--mint-live-round
                    arxana-essays-diachronic-test--hyperreal-seed-id)))
              (should-not store-called)
              (should (stringp (plist-get round-a :round-id)))
              (should (stringp (plist-get round-a :minted-at)))
              (should (equal arxana-essays-diachronic-test--hyperreal-seed-id
                             (plist-get round-a :seed-id)))
              (should (equal arxana-essays-diachronic-test--hyperreal-essay-id
                             (plist-get round-a :essay-id)))
              (should (file-directory-p (plist-get round-a :round-dir)))
              (should (file-exists-p (plist-get round-a :md)))
              (should (file-exists-p (plist-get round-a :manifest)))
              (should (file-exists-p (plist-get round-a :edn)))
              (should (not (equal (plist-get round-a :round-id)
                                  (plist-get round-b :round-id))))
              (should (equal md-before
                             (arxana-browser-essays--file-sha256 seed-md)))
              (should (equal manifest-before
                             (arxana-browser-essays--file-sha256
                              seed-manifest)))
              (should (equal md-before
                             (plist-get round-a :seed-md-sha256)))
              (should (equal manifest-before
                             (plist-get round-a :seed-manifest-sha256)))
              (should (equal md-text
                             (arxana-essays-diachronic-test--file-text
                              (plist-get round-a :md))))
              (should (equal manifest-text
                             (arxana-essays-diachronic-test--file-text
                              (plist-get round-a :manifest))))
              (should-not (string-prefix-p
                           (file-name-as-directory seed-dir)
                           (file-name-as-directory
                            (plist-get round-a :round-dir)))))))
      (delete-directory root t))))

(provide 'arxana-essays-diachronic-test)

;;; arxana-essays-diachronic-test.el ends here
