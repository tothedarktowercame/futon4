;;; arxana-media-lyrics-test.el --- Tests for chorded lyric faces -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)

(let* ((test-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root (file-name-directory (directory-file-name test-dir)))
       (dev (expand-file-name "dev" root)))
  (add-to-list 'load-path dev)
  (load-file (expand-file-name "dev/arxana-media.el" root))
  (load-file (expand-file-name "dev/arxana-browser-core.el" root)))
(require 'arxana-media)
(require 'arxana-browser-core)

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

(ert-deftest arxana-media-misc-track-items-sort-newest-first ()
  (let ((dir (make-temp-file "arxana-misc-sort-" t)))
    (unwind-protect
        (let ((older (expand-file-name "older.wav" dir))
              (newer (expand-file-name "newer.wav" dir))
              (middle (expand-file-name "middle.wav" dir)))
          (dolist (file (list older newer middle))
            (with-temp-file file
              (insert "x")))
          (set-file-times older (date-to-time "2026-03-19 09:00"))
          (set-file-times middle (date-to-time "2026-03-20 09:00"))
          (set-file-times newer (date-to-time "2026-03-21 09:00"))
          (should (equal (mapcar (lambda (item)
                                   (file-name-nondirectory (plist-get item :path)))
                                 (arxana-media--misc-track-items dir))
                         '("newer.wav" "middle.wav" "older.wav"))))
      (ignore-errors (delete-directory dir t)))))

(ert-deftest arxana-media-misc-track-row-includes-date-column ()
  (let ((file (make-temp-file "arxana-misc-row-" nil ".wav")))
    (unwind-protect
        (progn
          (set-file-times file (date-to-time "2026-03-21 14:15"))
          (let ((row (arxana-media--misc-track-row
                      (list :type 'media-misc-track
                            :label "demo"
                            :path file))))
            (should (= (length row) 6))
            (should (equal (aref row 2) "2026-03-21 14:15"))
            (should (equal (aref row 3) "demo"))))
      (ignore-errors (delete-file file)))))

(ert-deftest arxana-media-remove-from-ep-staging-at-point-removes-only-current-placement ()
  (let* ((root (make-temp-file "arxana-ep-remove-" t))
         (ep3 (expand-file-name "ep3" root))
         (ep4 (expand-file-name "ep4" root))
         (ep3-file (expand-file-name "remote-control.mp3" ep3))
         (ep4-file (expand-file-name "remote-control.mp3" ep4))
         (other-file (expand-file-name "yellow-sister.mp3" ep4))
         (item (list :type 'media-publication-track
                     :label "remote control"
                     :path ep4-file))
         (arxana-browser--stack (list (list :view 'media-ep-staging-ep
                                            :label "EP4"
                                            :ep-staging-path ep4)))
         (arxana-browser--context nil)
         (arxana-media--marked (make-hash-table :test 'equal))
         (arxana-media--misc-sha-cache (make-hash-table :test 'equal))
         (arxana-media--duration-cache (make-hash-table :test 'equal))
         (render-calls 0))
    (unwind-protect
        (progn
          (make-directory ep3 t)
          (make-directory ep4 t)
          (dolist (file (list ep3-file ep4-file other-file))
            (with-temp-file file
              (insert "audio")))
          (arxana-media--write-publication-tracks
           ep4
           (list (list :file "remote-control.mp3" :title "remote control")
                 (list :file "yellow-sister.mp3" :title "yellow sister")))
          (puthash ep4-file t arxana-media--marked)
          (puthash ep4-file '(cached) arxana-media--misc-sha-cache)
          (puthash ep4-file '(cached) arxana-media--duration-cache)
          (cl-letf (((symbol-function 'arxana-browser--item-at-point)
                     (lambda () item))
                    ((symbol-function 'arxana-browser--render)
                     (lambda () (setq render-calls (1+ render-calls)))))
            (arxana-media-remove-from-ep-staging-at-point))
          (should (file-exists-p ep3-file))
          (should-not (file-exists-p ep4-file))
          (should (file-exists-p other-file))
          (should-not (gethash ep4-file arxana-media--marked))
          (should-not (gethash ep4-file arxana-media--misc-sha-cache))
          (should-not (gethash ep4-file arxana-media--duration-cache))
          (should (= render-calls 1))
          (should (equal (plist-get (arxana-media--read-publication-tracks ep4) :tracks)
                         (list (list :file "yellow-sister.mp3"
                                     :title "yellow sister")))))
      (ignore-errors (delete-directory root t))))

(ert-deftest arxana-browser-remove-marked-delegates-to-ep-staging-removal ()
  (let ((arxana-browser--stack (list (list :view 'media-ep-staging-ep
                                           :ep-staging-path "/tmp/ep4/")))
        (called 0))
    (cl-letf (((symbol-function 'arxana-browser--ensure-context) (lambda () t))
              ((symbol-function 'arxana-media-remove-from-ep-staging-at-point)
               (lambda () (setq called (1+ called)))))
      (arxana-browser--remove-marked))
    (should (= called 1))))

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

(ert-deftest arxana-media-publish-marked-publication-track-upserts-lyrics-bundle ()
  (let* ((root (make-temp-file "arxana-publish-root-" t))
         (source (expand-file-name "adding-a-voice.wav" root))
         (arxana-media-publications-root root)
         (arxana-media--lyrics-cache (make-hash-table :test 'equal))
         (upsert-calls nil)
         (render-called nil))
    (unwind-protect
        (progn
          (with-temp-file source
            (set-buffer-multibyte nil)
            (insert "track-bytes"))
          (cl-letf (((symbol-function 'arxana-media--marked-items-in-context)
                     (lambda ()
                       (list (list :type 'media-publication-track
                                   :path source
                                   :label "Adding a Voice"))))
                    ((symbol-function 'arxana-media--marked-track-entries-in-context)
                     (lambda () nil))
                    ((symbol-function 'arxana-media--slug)
                     (lambda (_) "ep4"))
                    ((symbol-function 'arxana-store-sync-enabled-p)
                     (lambda () t))
                    ((symbol-function 'arxana-media--fetch-lyrics)
                     (lambda (id)
                       (and (equal id "source-lyrics")
                            "lyrics text\n")))
                    ((symbol-function 'arxana-media--lyrics-entity-id)
                     (lambda (item)
                       (if (equal (plist-get item :path) source)
                           "source-lyrics"
                         "dest-lyrics")))
                    ((symbol-function 'arxana-media--track-entity-id)
                     (lambda (item)
                       (if (equal (plist-get item :path) source)
                           "source-track"
                         "dest-track")))
                    ((symbol-function 'arxana-media--track-sha)
                     (lambda (&rest _) (make-string 64 ?a)))
                    ((symbol-function 'arxana-media--store-track-entity)
                     (lambda (&rest _) t))
                    ((symbol-function 'arxana-store-upsert-media-lyrics)
                     (lambda (&rest args)
                       (push args upsert-calls)
                       '((:ok? . t))))
                    ((symbol-function 'arxana-store-create-relation)
                     (lambda (&rest _)
                       (ert-fail "publish-marked should not call arxana-store-create-relation")))
                    ((symbol-function 'arxana-store-ensure-entity)
                     (lambda (&rest _)
                       (ert-fail "publish-marked should not call arxana-store-ensure-entity for lyrics")))
                    ((symbol-function 'arxana-media--write-publications-index)
                     (lambda () nil))
                    ((symbol-function 'arxana-browser--render)
                     (lambda () (setq render-called t))))
            (arxana-media-publish-marked "EP4")))
      (ignore-errors (delete-directory root t)))
    (should render-called)
    (should (= (length upsert-calls) 1))
    (let ((relation (plist-get (car upsert-calls) :relation)))
      (should (equal (alist-get 'type relation) ":media/lyrics"))
      (should (equal (alist-get 'src relation) "dest-track"))
      (should (equal (alist-get 'dst relation) "dest-lyrics")))
    (should (gethash "dest-lyrics" arxana-media--lyrics-cache))))

(ert-deftest arxana-media-write-publications-index-embeds-data-for-file-scheme ()
  (let* ((root (make-temp-file "arxana-publications-root-" t))
         (ep4 (expand-file-name "ep4" root))
         (arxana-media-publications-root root))
    (unwind-protect
        (progn
          (make-directory ep4 t)
          (with-temp-file (expand-file-name "publication.json" ep4)
            (insert "{\"name\":\"EP4\",\"url\":\"https://example.test/ep4\"}\n"))
          (with-temp-file (expand-file-name "tracks.json" ep4)
            (insert "{\"tracks\":[{\"file\":\"adding-a-voice.wav\",\"title\":\"Adding a Voice\",\"lyrics\":\"la\"}]}\n"))
          (arxana-media--write-publications-index)
          (let ((index-html (with-temp-buffer
                              (insert-file-contents (expand-file-name "index.html" root))
                              (buffer-string)))
                (player-html (with-temp-buffer
                               (insert-file-contents (expand-file-name "player.html" root))
                               (buffer-string))))
            (should (string-match-p "publications-data" index-html))
            (should (string-match-p "\"slug\":\"ep4\"" index-html))
            (should (string-match-p "publications-player-data" player-html))
            (should (string-match-p "\"Adding a Voice\"" player-html))
            (should (string-match-p "\"tracks\":\\[{\"file\":\"adding-a-voice.wav\",\"title\":\"Adding a Voice\"" player-html))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest arxana-media-read-publication-tracks-reconciles-renamed-audio-files ()
  (let* ((root (make-temp-file "arxana-publications-root-" t))
         (ep4 (expand-file-name "ep4" root)))
    (unwind-protect
        (progn
          (make-directory ep4 t)
          (with-temp-file (expand-file-name "adding-a-voice.mp3" ep4)
            (insert "audio"))
          (with-temp-file (expand-file-name "no-longer-alone.mp3" ep4)
            (insert "audio"))
          (with-temp-file (expand-file-name "tracks.json" ep4)
            (insert "{\"tracks\":[{\"file\":\"adding-a-voice.wav\",\"title\":\"Adding a Voice\"},{\"file\":\"no-longer-alone.wav.mp3\",\"title\":\"No Longer Alone\"}]}\n"))
          (let* ((payload (arxana-media--read-publication-tracks ep4))
                 (tracks (append (plist-get payload :tracks) nil))
                 (track (car tracks))
                 (track2 (cadr tracks)))
            (should (equal (plist-get track :file) "adding-a-voice.mp3"))
            (should (equal (plist-get track2 :file) "no-longer-alone.mp3")))
          (let* ((payload2 (with-temp-buffer
                             (insert-file-contents (expand-file-name "tracks.json" ep4))
                             (let ((json-object-type 'plist)
                                   (json-array-type 'list)
                                   (json-key-type 'keyword))
                               (json-read-from-string (buffer-string)))))
                 (tracks2 (append (plist-get payload2 :tracks) nil))
                 (track3 (car tracks2))
                 (track4 (cadr tracks2)))
            (should (equal (plist-get track3 :file) "adding-a-voice.mp3"))
            (should (equal (plist-get track4 :file) "no-longer-alone.mp3"))))
      (ignore-errors (delete-directory root t)))))

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

(ert-deftest arxana-media-toggle-mark-at-point-updates-row-in-place ()
  (let ((arxana-media--marked (make-hash-table :test 'equal))
        (set-col-calls nil)
        (render-called nil)
        (item '(:type media-misc-track :path "/tmp/track-a.wav")))
    (cl-letf (((symbol-function 'arxana-browser--item-at-point)
               (lambda () item))
              ((symbol-function 'tabulated-list-set-col)
               (lambda (col desc &optional change-entry-data)
                 (push (list col desc change-entry-data) set-col-calls)))
              ((symbol-function 'arxana-browser--render)
               (lambda () (setq render-called t))))
      (arxana-media-toggle-mark-at-point)
      (arxana-media-toggle-mark-at-point))
    (should (equal (nreverse set-col-calls)
                   '((0 "*" t) (0 " " t))))
    (should-not render-called)
    (should-not (gethash "/tmp/track-a.wav" arxana-media--marked))))

(ert-deftest arxana-media-toggle-mark-at-point-falls-back-to-render ()
  (let ((arxana-media--marked (make-hash-table :test 'equal))
        (render-calls 0)
        (item '(:type media-misc-track :path "/tmp/track-b.wav")))
    (cl-letf (((symbol-function 'arxana-browser--item-at-point)
               (lambda () item))
              ((symbol-function 'tabulated-list-set-col)
               (lambda (&rest _) (error "no row")))
              ((symbol-function 'arxana-browser--render)
               (lambda () (setq render-calls (1+ render-calls)))))
      (arxana-media-toggle-mark-at-point))
    (should (= render-calls 1))
    (should (gethash "/tmp/track-b.wav" arxana-media--marked))))

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

(ert-deftest arxana-media-retitle-track-preflights-store-before-zoom-sync ()
  (let ((entry '(:sha256 "sha-1" :status "hold" :title "old title"))
        (ensure-calls 0)
        (process-called nil))
    (cl-letf (((symbol-function 'arxana-store-sync-enabled-p) (lambda () t))
              ((symbol-function 'arxana-store-ensure-entity)
               (lambda (&rest _args)
                 (setq ensure-calls (1+ ensure-calls))
                 '((:error (:layer . 3)
                           (:reason . forbidden)
                           (:context (:penholder . "joe"))))))
              ((symbol-function 'arxana-media--zoom-sync-args)
               (lambda (&rest args)
                 (append '("python3" "/tmp/mock-zoom-sync.py") args)))
              ((symbol-function 'process-file)
               (lambda (&rest _args)
                 (setq process-called t)
                 0)))
      (should-error (arxana-media--retitle-track entry "new title"))
      (should (= ensure-calls 1))
      (should-not process-called))))

(ert-deftest arxana-media-bounce-profile-vocal-guitar-piano-bass-present ()
  (let* ((profile (assoc "vocal+guitar+piano+bass" arxana-media--bounce-profiles-default))
         (props (cdr profile)))
    (should profile)
    (should (equal (plist-get props :script)
                   "scripts/bounce_vocal_guitar_piano_bass.sh"))
    (should (equal (plist-get props :instruments)
                   '("vocal" "guitar" "piano" "bass")))))

(ert-deftest arxana-media-bounce-profile-vocal-forward-drum-sharp-bass-guitar-present ()
  (let* ((profile (assoc "vocal-forward+drum-sharp+bass+guitar"
                         arxana-media--bounce-profiles-default))
         (props (cdr profile)))
    (should profile)
    (should (equal (plist-get props :script)
                   "scripts/bounce_vocal_drum_bass_guitar_vocal_forward_sharp_drums.sh"))
    (should (equal (plist-get props :instruments)
                   '("vocal" "drum" "bass" "guitar")))))

(ert-deftest arxana-media-bounce-entry-instruments-matches-drum-aliases ()
  (should (equal (arxana-media--bounce-entry-instruments
                  '(:title "take drums")
                  '("vocal" "drum" "bass" "guitar")
                  arxana-media-bounce-instrument-aliases)
                 '("drum"))))

(ert-deftest arxana-media-bounce-profile-vocal-piano-bass-harmonica-present ()
  (let* ((profile (assoc "vocal+piano+bass+harmonica" arxana-media--bounce-profiles-default))
         (props (cdr profile)))
    (should profile)
    (should (equal (plist-get props :script)
                   "scripts/bounce_vocal_piano_bass_harmonica.sh"))
    (should (equal (plist-get props :instruments)
                   '("vocal" "piano" "bass" "harmonica")))))

(ert-deftest arxana-media-bounce-profile-vocal-piano-accordion-banjo-present ()
  (let* ((profile (assoc "vocal+piano+accordion+banjo" arxana-media--bounce-profiles-default))
         (props (cdr profile)))
    (should profile)
    (should (equal (plist-get props :script)
                   "scripts/bounce_vocal_piano_accordion_banjo.sh"))
    (should (equal (plist-get props :instruments)
                   '("vocal" "piano" "accordion" "banjo")))))

(ert-deftest arxana-media-bounce-profile-vocal-vocal2-vocoder-present ()
  (let* ((profile (assoc "vocal+vocal2+vocoder" arxana-media--bounce-profiles-default))
         (props (cdr profile)))
    (should profile)
    (should (equal (plist-get props :script)
                   "scripts/bounce_vocal_vocal2_vocoder.sh"))
    (should (equal (plist-get props :instruments)
                   '("vocal" "vocal2")))))

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
               (bulk (expand-file-name "bulk.json" root))
               (index-calls 0))
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
          (cl-letf (((symbol-function 'arxana-media--write-publications-index)
                     (lambda () (setq index-calls (1+ index-calls)))))
            (arxana-media-apply-publication-lyrics-bulk-json bulk))
          (let* ((tracks (arxana-media--read-publication-tracks ep1))
                 (items (append (plist-get tracks :tracks) nil))
                 (track (car items)))
            (should (= (length items) 1))
            (should (equal (plist-get track :file) "a.mp3"))
            (should (equal (plist-get track :title) "A"))
            (should (equal (plist-get track :lyrics) "L"))
            (should (= index-calls 1))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest arxana-media-refresh-publication-lyrics-at-point-updates-current-publication ()
  (let* ((root (make-temp-file "arxana-refresh-publication-" t))
         (ep4 (expand-file-name "ep4" root))
         (arxana-media-publications-root root)
         (render-calls 0))
    (unwind-protect
        (progn
          (make-directory ep4 t)
          (with-temp-file (expand-file-name "tracks.json" ep4)
            (insert "{\"tracks\":[{\"file\":\"a.mp3\",\"title\":\"Old\",\"lyrics\":\"old lyrics\"}]}\n"))
          (cl-letf (((symbol-function 'arxana-store-ensure-sync) (lambda () t))
                    ((symbol-function 'arxana-browser--item-at-point)
                     (lambda () (list :type 'media-publication :label "EP4" :path ep4)))
                    ((symbol-function 'arxana-media--publication-lyrics-bulk-data)
                     (lambda (directories &optional _fetch-fn)
                       (should (equal directories (list ep4)))
                       '(:updated_at 1
                         :source "futon1a"
                         :publications [(:slug "ep4"
                                         :tracks [(:file "a.mp3"
                                                   :title "Old"
                                                   :lyrics "new lyrics")])])))
                    ((symbol-function 'arxana-browser--render)
                     (lambda () (setq render-calls (1+ render-calls))))
                    ((symbol-function 'arxana-media--write-publications-index)
                     (lambda () nil)))
            (arxana-media-refresh-publication-lyrics-at-point))
          (let* ((tracks (arxana-media--read-publication-tracks ep4))
                 (items (append (plist-get tracks :tracks) nil))
                 (track (car items)))
            (should (= render-calls 1))
            (should (= (length items) 1))
            (should (equal (plist-get track :file) "a.mp3"))
            (should (equal (plist-get track :lyrics) "new lyrics"))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest arxana-media-publication-at-point-falls-back-from-track-row-context ()
  (let ((arxana-browser--context '(:view media-publication
                                   :label "EP4"
                                   :publication-path "/tmp/ep4"))
        (arxana-browser--stack nil))
    (cl-letf (((symbol-function 'arxana-browser--item-at-point)
               (lambda ()
                 '(:type media-publication-track
                   :path "/tmp/ep4/adding-a-voice.mp3"))))
      (let ((item (arxana-media--publication-at-point)))
        (should (eq (plist-get item :type) 'media-publication))
        (should (equal (plist-get item :label) "EP4"))
        (should (equal (plist-get item :path) "/tmp/ep4"))))))

(ert-deftest arxana-media-publication-track-items-carry-lyrics-indicator-from-metadata ()
  (let* ((root (make-temp-file "arxana-publication-lyrics-" t))
         (ep4 (expand-file-name "ep4" root))
         (audio (expand-file-name "adding-a-voice.mp3" ep4))
         (arxana-media--lyrics-cache (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (make-directory ep4 t)
          (with-temp-file audio
            (set-buffer-multibyte nil)
            (insert "audio"))
          (arxana-media--write-publication-tracks
           ep4
           (list (list :file "adding-a-voice.mp3"
                       :title "Adding a Voice"
                       :lyrics "lyric line"))))
          (let* ((items (arxana-media--publication-track-items ep4))
                 (item (car items))
                 (track (plist-get item :publication-track)))
            (should (= (length items) 1))
            (should (equal (plist-get item :label) "Adding a Voice"))
            (should (equal (plist-get track :lyrics) "lyric line"))
            (cl-letf (((symbol-function 'arxana-media--lyrics-present-p)
                       (lambda (&rest _)
                         (ert-fail "publication lyric indicator should use publication metadata first"))))
              (should (equal (arxana-media--lyrics-indicator item) "L")))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest arxana-media-write-publication-track-lyrics-updates-tracks-json ()
  (let* ((root (make-temp-file "arxana-publication-write-" t))
         (ep4 (expand-file-name "ep4" root))
         (audio (expand-file-name "adding-a-voice.mp3" ep4)))
    (unwind-protect
        (progn
          (make-directory ep4 t)
          (with-temp-file audio
            (set-buffer-multibyte nil)
            (insert "audio"))
          (arxana-media--write-publication-tracks
           ep4
           (list (list :file "adding-a-voice.mp3"
                       :title "Adding a Voice"
                       :lyrics "old lyrics")))
          (cl-letf (((symbol-function 'arxana-media--write-publications-index)
                     (lambda () nil)))
            (should (arxana-media--write-publication-track-lyrics
                     (list :type 'media-publication-track
                           :path audio)
                     "new lyrics")))
          (let* ((data (arxana-media--read-publication-tracks ep4))
                 (tracks (append (plist-get data :tracks) nil))
                 (track (car tracks)))
            (should (= (length tracks) 1))
            (should (equal (plist-get track :lyrics) "new lyrics"))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest arxana-media-publication-source-item-resolves-staging-counterpart ()
  (let* ((root (make-temp-file "arxana-publication-source-" t))
         (arxana-media-ep-staging-root (expand-file-name "ep-staging" root))
         (publication-root (expand-file-name "publications" root))
         (staging-ep (expand-file-name "ep4" arxana-media-ep-staging-root))
         (publication-ep (expand-file-name "ep4" publication-root))
         (staging-audio (expand-file-name "adding-a-voice.wav" staging-ep))
         (publication-audio (expand-file-name "adding-a-voice.mp3" publication-ep)))
    (unwind-protect
        (progn
          (make-directory staging-ep t)
          (make-directory publication-ep t)
          (with-temp-file staging-audio
            (insert "staging"))
          (with-temp-file publication-audio
            (insert "publication"))
          (let ((source-item
                 (arxana-media--publication-source-item
                  (list :type 'media-publication-track :path publication-audio))))
            (should (equal (plist-get source-item :path) staging-audio))
            (should (equal (plist-get source-item :label) "adding-a-voice"))))
      (ignore-errors (delete-directory root t)))))

(ert-deftest arxana-media-lyrics-save-from-publication-uses-staging-source-and-mirrors-json ()
  (let* ((root (make-temp-file "arxana-publication-save-" t))
         (arxana-media-ep-staging-root (expand-file-name "ep-staging" root))
         (publication-root (expand-file-name "publications" root))
         (staging-ep (expand-file-name "ep4" arxana-media-ep-staging-root))
         (publication-ep (expand-file-name "ep4" publication-root))
         (staging-audio (expand-file-name "adding-a-voice.wav" staging-ep))
         (publication-audio (expand-file-name "adding-a-voice.mp3" publication-ep))
         (upsert-item nil)
         (written-lyrics nil)
         (arxana-media--lyrics-cache (make-hash-table :test 'equal))
         (arxana-media--lyrics-context nil))
    (unwind-protect
        (progn
          (make-directory staging-ep t)
          (make-directory publication-ep t)
          (with-temp-file staging-audio
            (insert "staging"))
          (with-temp-file publication-audio
            (insert "publication"))
          (arxana-media--write-publication-tracks
           publication-ep
           (list (list :file "adding-a-voice.mp3"
                       :title "Adding a Voice"
                       :lyrics "")))
          (setq arxana-media--lyrics-context
                (arxana-media--lyrics-context
                 (list :type 'media-publication-track
                       :label "Adding a Voice"
                       :path publication-audio)))
          (with-temp-buffer
            (insert "fresh lyrics")
            (cl-letf (((symbol-function 'arxana-store-ensure-sync) (lambda () t))
                      ((symbol-function 'arxana-media--journal-lyrics) (lambda (&rest _) nil))
                      ((symbol-function 'arxana-media--track-sha)
                       (lambda (item)
                         (if (equal (plist-get item :path) staging-audio)
                             "staging-sha"
                           "publication-sha")))
                      ((symbol-function 'arxana-media--upsert-track-lyrics-bundle)
                       (lambda (item _title _entity-id _lyrics-id _sha lyrics _context)
                         (setq upsert-item item)
                         (setq written-lyrics lyrics)))
                      ((symbol-function 'arxana-media--verify-lyrics-write) (lambda (&rest _) t))
                      ((symbol-function 'arxana-browser--render) (lambda () nil))
                      ((symbol-function 'arxana-media--write-publications-index) (lambda () nil)))
              (arxana-media-lyrics-save)))
          (should (equal (plist-get upsert-item :path) staging-audio))
          (should (equal written-lyrics "fresh lyrics"))
          (let* ((data (arxana-media--read-publication-tracks publication-ep))
                 (tracks (append (plist-get data :tracks) nil))
                 (track (car tracks)))
            (should (equal (plist-get track :lyrics) "fresh lyrics"))))
      (ignore-errors (delete-directory root t)))))
