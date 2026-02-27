;;; arxana-media-lyrics-test.el --- Tests for chorded lyric faces -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root (file-name-directory (directory-file-name test-dir)))
       (dev (expand-file-name "dev" root)))
  (add-to-list 'load-path dev)
  (load-file (expand-file-name "dev/arxana-media.el" root)))
(require 'arxana-media)

(defconst arxana-media-lyrics-test--sample
  (concat
   "[A\u266f] Generally alone and the stars shone brighter, except in the city\n"
   "[the] red glow takes over\n"
   "[C] there's a direct connection between every stimulus and every action\n"
   "[F\u266f] recall when machines were wonderful\n"
   "[E] especially including the knee-jerk reactions\n"
   "[F] We have to keep the [F] fragments italic\n"
   "          [B] the childhood home\n"
   "[F\u266f] other machines strong sailing along\n"
   "          [C\u266f] The current detritus\n"
   "[D] to enable anyone who can build with blocks to say something\n"
   "[D] 'cause you hear some words that start to resonate\n"
   "[A\u266f] Here we grow extinct among the concrete barriers\n"
   "[G\u266f] Would it be reasonable to assume that you like interesting activities?\n"))

(ert-deftest arxana-media-lyrics-apply-chord-faces ()
  (with-temp-buffer
    (insert arxana-media-lyrics-test--sample)
    (arxana-media--lyrics-apply-chord-faces (point-min) (point-max))
    (goto-char (point-min))
    (let ((seen nil))
      (while (re-search-forward arxana-media--lyrics-chord-regexp nil t)
        (let* ((chord (match-string-no-properties 1))
               (face (arxana-media--lyrics-face-for-chord chord))
               (body-start (match-beginning 2))
               (body-face (and body-start (get-text-property body-start 'face))))
          (push chord seen)
          (if face
              (should (eq body-face face))
            (should (null body-face)))
          (should (null (get-text-property (match-beginning 1) 'face)))))
      (dolist (chord '("A\u266f" "C" "F\u266f" "E" "F" "B" "C\u266f" "D" "G\u266f" "the"))
        (should (member chord seen))))))

(ert-deftest arxana-media-lyrics-apply-chord-faces-inline-segments ()
  (with-temp-buffer
    (insert "[C] alpha [F] beta [G\u266f] gamma")
    (arxana-media--lyrics-apply-chord-faces (point-min) (point-max))
    (goto-char (point-min))
    (search-forward "alpha")
    (should (eq (get-text-property (match-beginning 0) 'face)
                (arxana-media--lyrics-face-for-chord "C")))
    (search-forward "beta")
    (should (eq (get-text-property (match-beginning 0) 'face)
                (arxana-media--lyrics-face-for-chord "F")))
    (search-forward "gamma")
    (should (eq (get-text-property (match-beginning 0) 'face)
                (arxana-media--lyrics-face-for-chord "G\u266f")))
    (goto-char (point-min))
    (re-search-forward "\\[F\\]")
    (should (null (get-text-property (match-beginning 0) 'face)))))

(ert-deftest arxana-media-lyrics-f-chord-is-italic ()
  (should (eq (arxana-media--lyrics-face-for-chord "F")
              'arxana-media-lyrics-sans-italic)))

(ert-deftest arxana-media-lyrics-f-chord-migrates-stale-sans-map ()
  (let ((arxana-media-lyrics-chord-faces
         '(("F" . arxana-media-lyrics-sans))))
    (should (eq (arxana-media--lyrics-face-for-chord "F")
                'arxana-media-lyrics-sans-italic))))

(ert-deftest arxana-media-lualatex-candidates-prefer-configured-then-fixed-then-found ()
  (let ((fixed "/usr/local/texlive/2025/bin/x86_64-linux/lualatex")
        (preferred "/custom/lualatex")
        (found "/usr/bin/lualatex"))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (bin) (and (string= bin "lualatex") found)))
              ((symbol-function 'file-executable-p)
               (lambda (path) (member path (list preferred fixed found)))))
      (should (equal (arxana-media--lualatex-candidates preferred)
                     (list preferred fixed found))))))

(ert-deftest arxana-media-resolve-lualatex-picks-first-ready-candidate ()
  (cl-letf (((symbol-function 'arxana-media--lualatex-candidates)
             (lambda (&optional _preferred)
               '("/bad/lualatex" "/good/lualatex")))
            ((symbol-function 'arxana-media--lualatex-ready-p)
             (lambda (path) (string= path "/good/lualatex"))))
    (should (equal (arxana-media--resolve-lualatex)
                   "/good/lualatex"))))

(ert-deftest arxana-media-publication-cover-detects-cover1-jpg ()
  (let ((dir (make-temp-file "arxana-pub-cover-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "cover1.jpg" dir)
            (insert "x"))
          (should (equal (arxana-media--publication-cover dir "ep9")
                         "ep9/cover1.jpg")))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest arxana-media-publication-cover-nil-when-missing ()
  (let ((dir (make-temp-file "arxana-pub-cover-none-" t)))
    (unwind-protect
        (should (null (arxana-media--publication-cover dir "ep9")))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest arxana-media-lyrics-quick-hash ()
  (let* ((slice arxana-media-lyrics-quick-hash-bytes)
         (head (make-string slice ?H))
         (tail (make-string slice ?T))
         (middle (make-string 1000 ?M))
         (content (concat head middle tail))
         (file (make-temp-file "arxana-quick-hash-")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert content)
            (write-region (point-min) (point-max) file nil 'silent))
          (let* ((size (nth 7 (file-attributes file)))
                 (prefix (encode-coding-string (format "%d:" size) 'utf-8))
                 (expected (with-temp-buffer
                             (set-buffer-multibyte nil)
                             (insert prefix)
                             (insert head)
                             (insert tail)
                             (secure-hash 'sha256 (current-buffer))))
                 (actual (arxana-media--file-quick-hash file)))
            (should (equal expected actual))))
      (ignore-errors (delete-file file)))))

(defun arxana-media-lyrics-test--with-temp-track (fn)
  (let ((file (make-temp-file "arxana-lyrics-track-")))
    (unwind-protect
        (progn
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert "track-bytes")
            (write-region (point-min) (point-max) file nil 'silent))
          (funcall fn file))
      (ignore-errors (delete-file file)))))

(defun arxana-media-lyrics-test--context (path)
  (list :entity-id "track-1"
        :lyrics-entity-id "lyrics-1"
        :item (list :type 'media-publication-track :path path)
        :title "Test Track"
        :path path
        :source "misc"))

(ert-deftest arxana-media-lyrics-save-logs-and-errors-on-store-failure ()
  (arxana-media-lyrics-test--with-temp-track
   (lambda (path)
     (let* ((journal (make-temp-file "lyrics-journal-"))
            (arxana-media-lyrics-journal-file journal)
            (arxana-media--lyrics-context (arxana-media-lyrics-test--context path))
            (arxana-media--lyrics-cache (make-hash-table :test 'equal)))
       (with-temp-buffer
         (insert "lyrics text")
         (cl-letf (((symbol-function 'arxana-store-ensure-sync) (lambda (&rest _) t))
                   ((symbol-function 'arxana-media--track-sha) (lambda (&rest _) "sha-1"))
                   ((symbol-function 'arxana-store-upsert-media-lyrics) (lambda (&rest _) '((:error . "boom")))))
           (should-error (arxana-media-lyrics-save))))
       (should (file-exists-p journal))
       (with-temp-buffer
         (insert-file-contents journal)
         (should (string-match-p "lyrics text" (buffer-string))))))))

(ert-deftest arxana-media-lyrics-save-errors-on-verification-mismatch ()
  (arxana-media-lyrics-test--with-temp-track
   (lambda (path)
     (let* ((journal (make-temp-file "lyrics-journal-"))
            (arxana-media-lyrics-journal-file journal)
            (arxana-media--lyrics-context (arxana-media-lyrics-test--context path))
            (arxana-media--lyrics-cache (make-hash-table :test 'equal)))
       (with-temp-buffer
         (insert "lyrics text")
         (cl-letf (((symbol-function 'arxana-store-ensure-sync) (lambda (&rest _) t))
                   ((symbol-function 'arxana-media--track-sha) (lambda (&rest _) "sha-1"))
                   ((symbol-function 'arxana-store-upsert-media-lyrics) (lambda (&rest _) '((:ok? . t))))
                   ((symbol-function 'arxana-store-fetch-entity)
                    (lambda (&rest _) '((:entity . ((:media/sha256 . "sha-1")
                                                    (:source . "different")))))))
           (should-error (arxana-media-lyrics-save))))
       (should (file-exists-p journal))))))

(ert-deftest arxana-media-lyrics-save-succeeds-when-verified ()
  (arxana-media-lyrics-test--with-temp-track
   (lambda (path)
     (let* ((journal (make-temp-file "lyrics-journal-"))
            (arxana-media-lyrics-journal-file journal)
            (arxana-media--lyrics-context (arxana-media-lyrics-test--context path))
            (arxana-media--lyrics-cache (make-hash-table :test 'equal)))
       (with-temp-buffer
         (insert "lyrics text")
         (cl-letf (((symbol-function 'arxana-store-ensure-sync) (lambda (&rest _) t))
                   ((symbol-function 'arxana-media--track-sha) (lambda (&rest _) "sha-1"))
                   ((symbol-function 'arxana-store-upsert-media-lyrics) (lambda (&rest _) '((:ok? . t))))
                   ((symbol-function 'arxana-store-fetch-entity)
                    (lambda (&rest _) '((:entity . ((:media/sha256 . "sha-1")
                                                    (:source . "lyrics text")))))))
           (should (condition-case nil
                       (progn (arxana-media-lyrics-save) t)
                     (error nil)))))
       (should (file-exists-p journal))))))

(ert-deftest arxana-media-lyrics-present-retries-after-transient-down ()
  (let ((arxana-media--lyrics-cache (make-hash-table :test 'equal))
        (arxana-store-last-error nil)
        (calls 0))
    (cl-letf (((symbol-function 'arxana-store-sync-enabled-p) (lambda () t))
              ((symbol-function 'arxana-store-clear-error)
               (lambda () (setq arxana-store-last-error nil)))
              ((symbol-function 'arxana-store-fetch-entity)
               (lambda (_id)
                 (setq calls (1+ calls))
                 (if (= calls 1)
                     (progn
                       (setq arxana-store-last-error '((:reason . connection)))
                       nil)
                   '((:entity . ((:source . "lyrics text")))))))
              ((symbol-function 'arxana-media--entity-source)
               (lambda (entity)
                 (or (and (listp entity) (alist-get :source entity)) ""))))
      (should (eq (arxana-media--lyrics-present-p "lyrics-1") 'unknown))
      (should (eq (gethash "lyrics-1" arxana-media--lyrics-cache 'unset) 'unset))
      (should (eq (arxana-media--lyrics-present-p "lyrics-1") t))
      (should (= calls 2))
      (should (eq (gethash "lyrics-1" arxana-media--lyrics-cache) t)))))

(ert-deftest arxana-media-lyrics-present-does-not-cache-negative ()
  (let ((arxana-media--lyrics-cache (make-hash-table :test 'equal))
        (calls 0))
    (cl-letf (((symbol-function 'arxana-store-sync-enabled-p) (lambda () t))
              ((symbol-function 'arxana-store-clear-error) (lambda () nil))
              ((symbol-function 'arxana-store-fetch-entity)
               (lambda (_id)
                 (setq calls (1+ calls))
                 '((:entity . ((:source . ""))))))
              ((symbol-function 'arxana-media--entity-source)
               (lambda (entity)
                 (or (and (listp entity) (alist-get :source entity)) ""))))
      (should-not (arxana-media--lyrics-present-p "lyrics-empty"))
      (should (eq (gethash "lyrics-empty" arxana-media--lyrics-cache 'unset) 'unset))
      (should-not (arxana-media--lyrics-present-p "lyrics-empty"))
      (should (= calls 2)))))

(ert-deftest arxana-media-lyrics-present-uses-health-guard-when-down ()
  (let ((arxana-media--lyrics-cache (make-hash-table :test 'equal))
        (fetch-calls 0))
    (cl-letf (((symbol-function 'arxana-store-sync-enabled-p) (lambda () t))
              ((symbol-function 'arxana-store-remote-status) (lambda (&optional _force) :down))
              ((symbol-function 'arxana-store-fetch-entity)
               (lambda (&rest _)
                 (setq fetch-calls (1+ fetch-calls))
                 nil)))
      (should (eq (arxana-media--lyrics-present-p "lyrics-down") 'unknown))
      (should (= fetch-calls 0))
      (should (eq (gethash "lyrics-down" arxana-media--lyrics-cache 'unset) 'unset)))))

(ert-deftest arxana-media-assert-store-ok-reports-missing-penholder ()
  (let ((response '((:error (:layer . 3) (:reason . missing-penholder) (:context)))))
    (let ((message-text
           (condition-case err
               (progn
                 (arxana-media--assert-store-ok response "Saving lyrics bundle")
                 nil)
             (error (error-message-string err)))))
      (should (string-match-p "missing-penholder" (or message-text "")))
      (should (string-match-p "arxana-store-default-penholder" (or message-text ""))))))

(ert-deftest arxana-media-catalog-path-falls-back-from-stale-config ()
  (let* ((stale (expand-file-name "/tmp/does-not-exist-zoom-sync-index.json"))
         (fallback-file (make-temp-file "zoom-sync-index-" nil ".json" "{\"entries\":[]}")))
    (unwind-protect
        (let ((arxana-media-index-path stale))
          (cl-letf (((symbol-function 'arxana-media--locate-default-index)
                     (lambda () fallback-file)))
            (should (equal (arxana-media--catalog-path) fallback-file))))
      (ignore-errors (delete-file fallback-file)))))

(ert-deftest arxana-media-locate-default-index-skips-unreadable-root ()
  (let ((storage-candidate "/mock/storage/zoom_sync_index.json"))
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (&rest _) "/tmp/"))
              ((symbol-function 'arxana-media--readable-path)
               (lambda (path)
                 (cond
                  ((not (stringp path)) nil)
                  ((string-match-p "/tmp/futon0/data/zoom_sync_index\\.json\\'" path) nil)
                  ((string-match-p "code/storage/zoomr4/meta/zoom_sync_index\\.json\\'" path)
                   storage-candidate)
                  (t nil)))))
      (should (equal (arxana-media--locate-default-index) storage-candidate)))))

(ert-deftest arxana-media-lyrics-text->latex-body-preserves-line-structure ()
  (let ((text "[C] alpha\n\n[F] beta"))
    (let ((body (arxana-media--lyrics-text->latex-body text)))
      (should (string-match-p "\\\\noindent " body))
      (should (string-match-p "\\\\vspace{0.6em}\\\\par" body))
      (should (string-match-p "alpha" body))
      (should (string-match-p "beta" body)))))

(ert-deftest arxana-media-publication-lyrics-sections-collects-lyrics-only ()
  (let ((root (make-temp-file "arxana-chapbook-root-" t)))
    (unwind-protect
        (let* ((ep1 (expand-file-name "ep1" root))
               (ep2 (expand-file-name "ep2" root)))
          (make-directory ep1 t)
          (make-directory ep2 t)
          (with-temp-file (expand-file-name "publication.json" ep1)
            (insert "{\"name\":\"EP1\",\"url\":\"\"}\n"))
          (with-temp-file (expand-file-name "tracks.json" ep1)
            (insert (concat
                     "{\"updated_at\":1,\"tracks\":["
                     "{\"file\":\"a.mp3\",\"title\":\"A\",\"lyrics\":\"[C] one\"},"
                     "{\"file\":\"b.mp3\",\"title\":\"B\",\"lyrics\":\"\"}"
                     "]}\n")))
          (with-temp-file (expand-file-name "tracks.json" ep2)
            (insert (concat
                     "{\"updated_at\":1,\"tracks\":["
                     "{\"file\":\"c.mp3\",\"title\":\"C\",\"lyrics\":\"   \"}"
                     "]}\n")))
          (let ((arxana-media-publications-root root))
            (let ((sections (arxana-media--publication-lyrics-sections)))
              (should (= (length sections) 1))
              (should (equal (plist-get (car sections) :title) "EP1"))
              (should (= (length (plist-get (car sections) :tracks)) 1))
              (should (equal (plist-get (car (plist-get (car sections) :tracks)) :title) "A")))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest arxana-media-lyrics-clean-track-title-removes-kebab-and-page-prefix ()
  (should (equal (arxana-media--lyrics-clean-track-title "page-1-this-is-my-mother")
                 "this is my mother"))
  (should (equal (arxana-media--lyrics-clean-track-title "salt")
                 "salt"))
  (should (equal (arxana-media--lyrics-clean-track-title "page-6-don-t-stake-too-much")
                 "don't stake too much"))
  (should (equal (arxana-media--lyrics-clean-track-title " page-10-a_b-c ")
                 "a b c")))

(ert-deftest arxana-media-publication-lyrics-tracks-flattens-sections ()
  (let ((sections (list (list :title "EP1"
                              :tracks (list (list :title "a" :lyrics "x")
                                            (list :title "b" :lyrics "y")))
                        (list :title "EP2"
                              :tracks (list (list :title "c" :lyrics "z"))))))
    (cl-letf (((symbol-function 'arxana-media--publication-lyrics-sections)
               (lambda (&optional _directories) sections)))
      (let ((tracks (arxana-media--publication-lyrics-tracks)))
        (should (= (length tracks) 3))
        (should (equal (mapcar (lambda (track) (plist-get track :title)) tracks)
                       '("a" "b" "c")))))))

(ert-deftest arxana-media-publication-lyrics-bulk-data-uses-existing-titles-and-store-lyrics ()
  (let ((dir "/tmp/epx")
        (calls nil))
    (cl-letf (((symbol-function 'arxana-media--publication-directories)
               (lambda () (list dir)))
              ((symbol-function 'arxana-media--publication-display-name)
               (lambda (_dir) "EPX"))
              ((symbol-function 'arxana-media--publication-audio-files)
               (lambda (_dir) (list "/tmp/epx/salt.mp3")))
              ((symbol-function 'arxana-media--read-publication-tracks)
               (lambda (_dir)
                 '(:tracks [(:file "salt.mp3" :title "salt custom")])) )
              ((symbol-function 'arxana-media--misc-sha256)
               (lambda (_path) "sha1"))
              ((symbol-function 'arxana-media--fetch-lyrics-for-id-strict)
               (lambda (lyrics-id)
                 (push lyrics-id calls)
                 "lyrics text")))
      (let* ((payload (arxana-media--publication-lyrics-bulk-data))
             (pubs (append (plist-get payload :publications) nil))
             (pub (car pubs))
             (tracks (append (plist-get pub :tracks) nil))
             (track (car tracks)))
        (should (equal (plist-get payload :source) "futon1a"))
        (should (= (length pubs) 1))
        (should (equal (plist-get pub :slug) "epx"))
        (should (= (length tracks) 1))
        (should (equal (plist-get track :file) "salt.mp3"))
        (should (equal (plist-get track :title) "salt custom"))
        (should (equal (plist-get track :lyrics) "lyrics text"))
        (should (equal (car calls) "arxana/media-lyrics/misc/sha1"))))))

(ert-deftest arxana-media-apply-publication-lyrics-bulk-json-writes-tracks ()
  (let ((root (make-temp-file "arxana-bulk-apply-" t)))
    (unwind-protect
        (let* ((arxana-media-publications-root root)
               (ep1 (expand-file-name "ep1" root))
               (bulk (expand-file-name "bulk.json" root)))
          (make-directory ep1 t)
          (with-temp-file bulk
            (insert (json-encode
                     '(:updated_at 1
                       :source "futon1a"
                       :publications [(:slug "ep1"
                                       :tracks [(:file "a.mp3"
                                                 :title "A"
                                                 :lyrics "L")])])))
            (insert "\n"))
          (arxana-media-apply-publication-lyrics-bulk-json bulk)
          (let* ((tracks (arxana-media--read-publication-tracks ep1))
                 (items (append (plist-get tracks :tracks) nil))
                 (track (car items)))
            (should (= (length items) 1))
            (should (equal (plist-get track :file) "a.mp3"))
            (should (equal (plist-get track :title) "A"))
            (should (equal (plist-get track :lyrics) "L"))))
      (ignore-errors (delete-directory root t)))))
