;;; arxana-browser-essays-test.el --- Tests for essays browser -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-browser-core)
(require 'arxana-browser-essays)

(ert-deftest arxana-browser-essays-menu-items-count-live-section-annotations ()
  (let ((arxana-browser-essays-catalogs
         '((:label "Essay Alpha"
            :description "Annotated essay"
            :essay-id "essay:alpha")))
        (manifest
         '(:essay (:id "essay:alpha")
           :sections ((:id "section:1" :name "Section One")
                      (:id "section:2" :name "Section Two"))
           :annotations ((:id "ann:1"
                          :annotated (:entity-id "section:1"))
                         (:id "ann:2"
                          :annotated (:entity-id "section:1")
                          :retracted t)
                         (:id "ann:3"
                          :annotated (:entity-id "section:2"))
                         (:id "ann:essay"
                          :annotated (:entity-id "essay:alpha"))))))
    (cl-letf (((symbol-function 'arxana-browser-essays--manifest-for)
               (lambda (_essay-id)
                 manifest)))
      (let ((item (car (arxana-browser-essays-menu-items))))
        (should (= 2 (plist-get item :annotation-count)))
        (should (equal "Annotated essay (2 sections, 2 annotations)"
                       (plist-get item :description)))))))

(ert-deftest arxana-browser-essays-items-use-live-open-section-counts ()
  (let* ((manifest
          '(:essay (:id "essay:alpha")
            :sections ((:id "section:1" :name "Section One"))
            :annotations ((:id "ann:1"
                           :annotated (:entity-id "section:1"))
                          (:id "ann:2"
                           :annotated (:entity-id "section:1")))))
         (buf (get-buffer-create arxana-browser-essays-text-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (erase-buffer)
            (insert "## 1. Section One\n\nOne Two\n")
            (setq-local arxana-browser-essays--essay-id "essay:alpha")
            (setq-local arxana-browser-essays--section-id "section:1")
            (setq-local arxana-browser-essays--heading-text "Section One")
            (let ((ov (make-overlay 20 23)))
              (overlay-put ov 'arxana-essay-annotation-id "ann:1")))
          (cl-letf (((symbol-function 'arxana-browser-essays--manifest-for)
                     (lambda (_essay-id)
                       manifest)))
            (let ((item (car (arxana-browser-essays-items
                              '(:view essays-essay :essay-id "essay:alpha")))))
              (should (equal "1(0+1)" (plist-get item :annotation-count)))
              (should (equal "1 live annotation, 1 pending retraction"
                             (plist-get item :description))))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest arxana-browser-copy-location-supports-essay-items ()
  (let (copied)
    (cl-letf (((symbol-function 'arxana-browser--item-at-point)
               (lambda ()
                 '(:type essays-essay
                   :essay-id "essay:alpha/beta")))
              ((symbol-function 'kill-new)
               (lambda (text)
                 (setq copied text)))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (arxana-browser--copy-location))
    (should (equal "arxana://essay/essay%3Aalpha%2Fbeta" copied))))

(ert-deftest arxana-browser-copy-current-location-uses-essay-context ()
  (let ((arxana-browser--context
         '(:view essays-section
           :essay-id "essay:alpha"
           :section-id "section/2"))
        (arxana-browser--stack nil)
        copied)
    (cl-letf (((symbol-function 'kill-new)
               (lambda (text)
                 (setq copied text)))
              ((symbol-function 'message)
               (lambda (&rest _args) nil)))
      (arxana-browser--copy-current-location))
    (should (equal "arxana://essay/essay%3Aalpha/section/section%2F2" copied))))

(ert-deftest arxana-browser-essays-reconcile-refreshes-source-index-after-deletion ()
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf
                (arxana-browser-essays--render-section-text
                 "essay:alpha" "section:1" "Section One"
                 nil nil
                 "prefix target suffix\n"
                 '((:id "ann:1"
                    :hx-type "annotation/grounds"
                    :annotated (:passage "target")
                    :source (:pattern-name "pattern/demo")))))
          (with-current-buffer buf
            (let* ((old-bounds (gethash "ann:1" arxana-browser-essays--source-index))
                   (old-start (car old-bounds))
                   (old-end (cdr old-bounds)))
              (should (consp old-bounds))
              (let ((inhibit-read-only t))
                (goto-char (point-min))
                (search-forward "prefix ")
                (delete-region (match-beginning 0) (match-end 0)))
              (arxana-browser-essays--reconcile-overlays)
              (let* ((new-bounds (gethash "ann:1" arxana-browser-essays--source-index))
                     (ov (seq-find
                          (lambda (overlay)
                            (equal "ann:1"
                                   (overlay-get overlay 'arxana-essay-annotation-id)))
                          (overlays-in (point-min) (point-max)))))
                (should (consp new-bounds))
                (should (overlayp ov))
                (should (< (car new-bounds) old-start))
                (should (< (cdr new-bounds) old-end))
                (should (equal new-bounds
                               (cons (overlay-start ov)
                                     (overlay-end ov))))))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest arxana-browser-essays-spell-style-replacement-preserves-annotation-properties ()
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf
                (arxana-browser-essays--render-section-text
                 "essay:alpha" "section:1" "Section One"
                 nil nil
                 "At this early stage in modeling dependencies.\n"
                 '((:id "ann:spell"
                    :hx-type "annotation/grounds"
                    :annotated (:passage "modeling")
                    :source (:pattern-name "pattern/demo")))))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (goto-char (point-min))
              (search-forward "modeling")
              (replace-match "modelling" nil t))
            (goto-char (point-min))
            (search-forward "modelling")
            (backward-char 1)
            (should (equal "ann:spell"
                           (get-text-property (point) 'arxana-essay-annotation-id)))
            (should (= 1
                       (get-text-property (point) 'arxana-essay-annotation-index)))
            (should (seq-find
                     (lambda (overlay)
                       (equal "ann:spell"
                              (overlay-get overlay 'arxana-essay-annotation-id)))
                     (overlays-at (point))))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest arxana-browser-essays-ensure-text-sync-hooks-restores-after-change-hook ()
  (with-temp-buffer
    (setq-local arxana-browser-essays-text-sync-mode t)
    (setq-local post-command-hook nil)
    (setq-local after-change-functions nil)
    (arxana-browser-essays--ensure-text-sync-hooks)
    (should (memq #'arxana-browser-essays--reconcile-overlays post-command-hook))
    (should (memq #'arxana-browser-essays--sync-notes-from-text post-command-hook))
    (should (memq #'arxana-browser-essays--refresh-notes-retractions post-command-hook))
    (should (memq #'arxana-browser-essays--restore-annotation-properties-after-change
                  after-change-functions))))

(ert-deftest arxana-browser-essays-reconcile-self-heals-undo-style-catastrophic-detach ()
  (with-temp-buffer
    (insert "Section: Demo\n\nShared scaffold evidence\n")
    (setq-local arxana-browser-essays--essay-id "essay:alpha")
    (setq-local arxana-browser-essays--section-id "section:1")
    (setq-local arxana-browser-essays-text-sync-mode t)
    (setq-local arxana-browser-essays--source-index
                (make-hash-table :test 'equal))
    (setq-local this-command 'undo)
    (cl-letf (((symbol-function 'arxana-browser-essays--manifest-for)
               (lambda (_essay-id)
                 '(:sections ((:id "section:1"))
                   :annotations ((:id "ann:1"
                                  :hx-type "annotation/grounds"
                                  :annotated (:entity-id "section:1"
                                              :passage "Shared scaffold evidence")
                                  :source (:pattern-name "demo")))))))
      (arxana-browser-essays--reconcile-overlays)
      (let ((bounds (gethash "ann:1" arxana-browser-essays--source-index)))
        (should bounds)
        (should (equal "ann:1"
                       (get-text-property (car bounds)
                                          'arxana-essay-annotation-id)))
        (should (seq-some
                 (lambda (ov)
                   (string= (overlay-get ov 'arxana-essay-annotation-id) "ann:1"))
                 (overlays-in (car bounds) (cdr bounds))))))))

(ert-deftest arxana-browser-essays-export-markdown-copies-source-file ()
  (let* ((source (make-temp-file "arxana-essay-source-" nil ".md"))
         (dest (make-temp-file "arxana-essay-export-" nil ".md"))
         (arxana-browser-essays-catalogs
          `((:label "Essay Alpha"
             :essay-id "essay:alpha"
             :source-file ,source))))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "# Title\n\n## 1. Section\n\nHello export.\n"))
          (with-temp-file dest
            (insert "stale"))
          (cl-letf (((symbol-function 'message)
                     (lambda (&rest _args) nil)))
            (should (equal dest
                           (arxana-browser-essays-export-markdown
                            dest "essay:alpha"))))
          (should (equal "# Title\n\n## 1. Section\n\nHello export.\n"
                         (with-temp-buffer
                           (insert-file-contents dest)
                           (buffer-string)))))
      (ignore-errors (delete-file source))
      (ignore-errors (delete-file dest)))))

(ert-deftest arxana-browser-essays-export-markdown-refuses-dirty-live-buffer ()
  (let* ((source (make-temp-file "arxana-essay-source-" nil ".md"))
         (dest (make-temp-file "arxana-essay-export-" nil ".md"))
         (arxana-browser-essays-catalogs
          `((:label "Essay Alpha"
             :essay-id "essay:alpha"
             :source-file ,source)))
         (buf (get-buffer-create arxana-browser-essays-text-buffer)))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "# Title\n\n## 1. Section\n\nHello export.\n"))
          (with-current-buffer buf
            (erase-buffer)
            (insert "Unsaved live edit")
            (setq-local arxana-browser-essays--essay-id "essay:alpha")
            (set-buffer-modified-p t))
          (with-current-buffer buf
            (should-error
             (arxana-browser-essays-export-markdown dest "essay:alpha")
             :type 'user-error)))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (ignore-errors (delete-file source))
      (ignore-errors (delete-file dest)))))

(ert-deftest arxana-browser-essays-render-section-text-clears-modified-flag ()
  (let ((buf (get-buffer-create arxana-browser-essays-text-buffer)))
    (unwind-protect
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert "stale")
          (set-buffer-modified-p t)
          (arxana-browser-essays--render-section-text
           "essay:alpha"
           "section:1"
           "§1. Demo"
           "/tmp/demo.md"
           "Demo"
           "Body\n"
           nil)
          (should-not (buffer-modified-p)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest arxana-browser-essays-rewrite-manifest-section-heading-updates-fields ()
  (let ((manifest-file (make-temp-file "arxana-essay-manifest-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file manifest-file
            (insert "(defconst test-essay-manifest\n")
            (insert "  '(:version 1\n")
            (insert "    :sections\n")
            (insert "    ((:id \"section:1\"\n")
            (insert "      :name \"§1. Old Title\"\n")
            (insert "      :type \"arxana/essay-section\"\n")
            (insert "      :props ((index . 1)\n")
            (insert "              (heading-text . \"Old Title\"))))))\n"))
          (should (arxana-browser-essays--rewrite-manifest-section-heading
                   manifest-file "section:1" "New Title" "§1. New Title"))
          (with-temp-buffer
            (insert-file-contents manifest-file)
            (let ((text (buffer-string)))
              (should (string-match-p
                       (regexp-quote ":name \"§1. New Title\"")
                       text))
              (should (string-match-p
                       (regexp-quote "(heading-text . \"New Title\")")
                       text)))))
      (ignore-errors (delete-file manifest-file)))))

(ert-deftest arxana-browser-essays-retraction-preview-still-shows-live-deletions-during-heading-rename ()
  (with-temp-buffer
    (insert "Section: Demo\n\n## 1. New Title\n\nBody\n")
    (setq-local arxana-browser-essays-edit-mode t)
    (setq-local arxana-browser-essays--essay-id "essay:alpha")
    (setq-local arxana-browser-essays--section-id "section:1")
    (setq-local arxana-browser-essays--heading-text "Old Title")
    (setq-local arxana-browser-essays--content-start
                (copy-marker (save-excursion
                               (goto-char (point-min))
                               (search-forward "\n\n")
                               (point))))
    (cl-letf (((symbol-function 'arxana-browser-essays--manifest-for)
               (lambda (_essay-id)
                 '(:sections ((:id "section:1"))
                   :annotations ((:id "ann:1"
                                  :annotated (:entity-id "section:1")))))))
      (should (equal '("ann:1")
                     (arxana-browser-essays--retracted-ids-in-this-section))))))

(ert-deftest arxana-browser-essays-editable-content-recovers-from-banner-start ()
  (with-temp-buffer
    (insert "Section: Demo\n\n## 1. New Title\n\nBody\n")
    (setq-local arxana-browser-essays--content-start (copy-marker (point-min)))
    (should (equal "## 1. New Title\n\nBody\n"
                   (arxana-browser-essays--editable-content)))))

(ert-deftest arxana-browser-essays-save-section-allows-confirmed-heading-change ()
  (let* ((source (make-temp-file "arxana-essay-source-" nil ".md"))
         (manifest-file (make-temp-file "arxana-essay-manifest-" nil ".el"))
         (buf (get-buffer-create arxana-browser-essays-text-buffer))
         (prompted nil))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "# Title\n\n## 1. Old Title\n\nBody text.\n"))
          (with-temp-file manifest-file
            (insert "(defconst test-essay-manifest\n")
            (insert "  '(:version 1\n")
            (insert "    :sections\n")
            (insert "    ((:id \"section:1\"\n")
            (insert "      :name \"§1. Old Title\"\n")
            (insert "      :type \"arxana/essay-section\"\n")
            (insert "      :props ((index . 1)\n")
            (insert "              (heading-text . \"Old Title\"))))\n")
            (insert "    :annotations ()))\n"))
          (with-current-buffer buf
            (erase-buffer)
            (insert "Section: §1. Old Title\n\n## 1. New Title\n\nBody text.\n")
            (goto-char (point-max))
            (setq-local arxana-browser-essays-edit-mode t)
            (setq-local arxana-browser-essays--source-file source)
            (setq-local arxana-browser-essays--essay-id "essay:alpha")
            (setq-local arxana-browser-essays--section-id "section:1")
            (setq-local arxana-browser-essays--section-name "§1. Old Title")
            (setq-local arxana-browser-essays--heading-text "Old Title")
            (setq-local arxana-browser-essays--content-start
                        (copy-marker (save-excursion
                                       (goto-char (point-min))
                                       (search-forward "\n\n")
                                       (point)))))
          (with-current-buffer buf
            (cl-letf (((symbol-function 'y-or-n-p)
                       (lambda (prompt)
                         (setq prompted prompt)
                         t))
                      ((symbol-function 'window-start)
                       (lambda (&optional _window) 1))
                      ((symbol-function 'message)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'arxana-browser-essays--backup-manifest-files)
                       (lambda () nil))
                      ((symbol-function 'arxana-browser-essays--retracted-ids-in-this-section)
                       (lambda () nil))
                      ((symbol-function 'arxana-browser-essays--sync-overlays-to-manifest)
                       (lambda () nil))
                      ((symbol-function 'arxana-browser-essays--sync-annotations-to-xtdb)
                       (lambda (_ids) nil))
                      ((symbol-function 'arxana-browser-essays-refresh)
                       (lambda () nil))
                      ((symbol-function 'arxana-browser-essays--manifest-for)
                       (lambda (_essay-id)
                         '(:sections ((:id "section:1"
                                      :name "§1. Old Title"
                                      :props ((heading-text . "Old Title"))))
                           :annotations ())))
                      ((symbol-function 'arxana-browser-essays--manifest-file-for-section)
                       (lambda (_section-id) manifest-file))
                      ((symbol-function 'arxana-browser-essays--open-section)
                       (lambda (&rest _args) nil)))
              (arxana-browser-essays-save-section)))
          (should (string-match-p "Overwrite and change title"
                                  (or prompted "")))
          (with-temp-buffer
            (insert-file-contents source)
            (should (string-match-p
                     (regexp-quote "## 1. New Title")
                     (buffer-string))))
          (with-temp-buffer
            (insert-file-contents manifest-file)
            (let ((text (buffer-string)))
              (should (string-match-p
                       (regexp-quote ":name \"§1. New Title\"")
                       text))
              (should (string-match-p
                       (regexp-quote "(heading-text . \"New Title\")")
                       text)))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (ignore-errors (delete-file source))
      (ignore-errors (delete-file manifest-file)))))

;;; arxana-browser-essays-test.el ends here
