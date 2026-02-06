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
      (dolist (chord '("A\u266f" "C" "F\u266f" "E" "B" "C\u266f" "D" "G\u266f" "the"))
        (should (member chord seen))))))

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
