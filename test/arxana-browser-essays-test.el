;;; arxana-browser-essays-test.el --- Tests for essays browser -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-browser-core)
(require 'arxana-docbook-ui)
(require 'arxana-browser-essays)

(defconst arxana-browser-essays-test--hyperreal-source
  "/home/joe/npt/applications/hyperreal-director-side-a/hyperreal-director-side-a-v1.md")

(defconst arxana-browser-essays-test--hyperreal-manifest
  "/home/joe/npt/applications/hyperreal-director-side-a/annotations.el")

(defconst arxana-browser-essays-test--hyperreal-essay-id
  "arxana/essay/hyperreal-director-side-a-v1")

(defconst arxana-browser-essays-test--hyperreal-section-clarity
  "arxana/essay/hyperreal-director-side-a-v1/section/6-clarity")

(defconst arxana-browser-essays-test--hyperreal-ann-right-effort
  "arxana/anno/hyperreal-side-a-v1/tension-right-effort-indicators")

(defun arxana-browser-essays-test--face-has-p (face target)
  "Return non-nil when FACE includes TARGET."
  (cond
   ((eq face target) t)
   ((listp face) (memq target face))
   (t nil)))

(defun arxana-browser-essays-test--copy-tree (source-dir dest-dir)
  "Recursively copy SOURCE-DIR into DEST-DIR."
  (make-directory dest-dir t)
  (dolist (entry (directory-files source-dir t directory-files-no-dot-files-regexp))
    (let ((dest (expand-file-name (file-name-nondirectory entry) dest-dir)))
      (if (file-directory-p entry)
          (arxana-browser-essays-test--copy-tree entry dest)
        (copy-file entry dest t)))))

(defun arxana-browser-essays-test--make-hyperreal-rounds ()
  "Return copied Hyperreal golden/live test rounds in a temp directory.
The result plist includes `:root', `:golden-dir', `:live-dir',
`:golden-source', `:golden-manifest', `:live-source', and
`:live-manifest'.  All writes stay inside the copied live round."
  (let* ((root (make-temp-file "arxana-hyperreal-rounds-" t))
         (golden-dir (expand-file-name "golden" root))
         (live-dir (expand-file-name "live" root))
         (source-dir (file-name-directory
                      arxana-browser-essays-test--hyperreal-source)))
    (arxana-browser-essays-test--copy-tree source-dir golden-dir)
    (arxana-browser-essays-test--copy-tree golden-dir live-dir)
    (list :root root
          :golden-dir golden-dir
          :live-dir live-dir
          :golden-source
          (expand-file-name
           (file-name-nondirectory arxana-browser-essays-test--hyperreal-source)
           golden-dir)
          :golden-manifest
          (expand-file-name
           (file-name-nondirectory arxana-browser-essays-test--hyperreal-manifest)
           golden-dir)
          :live-source
          (expand-file-name
           (file-name-nondirectory arxana-browser-essays-test--hyperreal-source)
           live-dir)
          :live-manifest
          (expand-file-name
           (file-name-nondirectory arxana-browser-essays-test--hyperreal-manifest)
           live-dir))))

(defun arxana-browser-essays-test--hyperreal-catalog (rounds)
  "Return a catalog plist pointing at the live Hyperreal ROUNDs copy."
  `((:essay-id ,arxana-browser-essays-test--hyperreal-essay-id
     :manifest-file ,(plist-get rounds :live-manifest)
     :source-file ,(plist-get rounds :live-source))))

(defun arxana-browser-essays-test--manifest-annotation-by-id (manifest ann-id)
  "Return the annotation with ANN-ID from MANIFEST, or nil."
  (seq-find (lambda (ann) (equal ann-id (plist-get ann :id)))
            (plist-get manifest :annotations)))

(defun arxana-browser-essays-test--rewrite-file-text (path transform)
  "Replace PATH contents with the result of calling TRANSFORM on the text."
  (let (text)
    (with-temp-buffer
      (insert-file-contents path)
      (setq text (buffer-string)))
    (with-temp-file path
      (insert (funcall transform text)))))

(ert-deftest arxana-browser-essays-hyperreal-round-copy-loads-real-seed ()
  (let* ((rounds (arxana-browser-essays-test--make-hyperreal-rounds))
         (arxana-browser-essays-manifest-files
          (list (plist-get rounds :live-manifest)))
         (arxana-browser-essays-catalogs
          `((:essay-id "arxana/essay/hyperreal-director-side-a-v1"
             :manifest-file ,(plist-get rounds :live-manifest))))
         (arxana-browser-essays--xtdb-catalog-cache
          arxana-browser-essays--xtdb-catalog-cache-unset))
    (unwind-protect
        (cl-letf (((symbol-function 'arxana-store-sync-enabled-p)
                   (lambda () nil)))
          (let* ((manifest (arxana-browser-essays--load-manifest-file
                            (plist-get rounds :live-manifest)))
                 (essay-id (plist-get (plist-get manifest :essay) :id))
                 (summary (arxana-browser-essays--annotation-summary manifest)))
            (should (equal "arxana/essay/hyperreal-director-side-a-v1" essay-id))
            (should (= 6 (length (plist-get manifest :sections))))
            (should (= 6 (plist-get summary :live)))
            (should (= 0 (plist-get summary :retracted)))
            (should (equal (plist-get rounds :live-source)
                           (arxana-browser-essays--catalog-source-file essay-id)))))
      (delete-directory (plist-get rounds :root) t))))

(ert-deftest arxana-browser-essays-hyperreal-import-persists-copied-paths ()
  (let* ((rounds (arxana-browser-essays-test--make-hyperreal-rounds))
         (arxana-browser-essays-manifest-files
          (list (plist-get rounds :live-manifest)))
         (arxana-browser-essays-catalogs nil)
         (essay-call nil)
         (section-calls nil)
         (hyperedges nil))
    (unwind-protect
        (cl-letf (((symbol-function 'arxana-store-ensure-sync)
                   (lambda () t))
                  ((symbol-function 'arxana-store-ensure-entity)
                   (lambda (&rest args)
                     (if (string= (plist-get args :type) "arxana/essay")
                         (setq essay-call args)
                       (push args section-calls))
                     args))
                  ((symbol-function 'arxana-store-create-hyperedge)
                   (lambda (&rest args)
                     (push args hyperedges)
                     args))
                  ((symbol-function 'arxana-browser-essays--resolve-pattern)
                   (lambda (pattern-name)
                     (format "pattern:%s" pattern-name))))
          (let ((summaries (arxana-browser-essays-import)))
            (should (= 1 (length summaries)))
            (should (equal (plist-get rounds :live-manifest)
                           (alist-get 'manifest-file
                                      (plist-get essay-call :props))))
            (should (equal (plist-get rounds :live-source)
                           (alist-get 'source-file
                                      (plist-get essay-call :props))))
            (should-not (equal arxana-browser-essays-test--hyperreal-manifest
                               (alist-get 'manifest-file
                                          (plist-get essay-call :props))))
            (should-not (equal arxana-browser-essays-test--hyperreal-source
                               (alist-get 'source-file
                                          (plist-get essay-call :props))))
            (should (= 6 (length section-calls)))
            (should (= 6 (length hyperedges)))))
      (delete-directory (plist-get rounds :root) t))))

(ert-deftest arxana-browser-essays-hyperreal-live-round-save-does-not-touch-seed ()
  (let* ((rounds (arxana-browser-essays-test--make-hyperreal-rounds))
         (live-manifest (plist-get rounds :live-manifest))
         (live-source (plist-get rounds :live-source))
         (golden-source (plist-get rounds :golden-source))
         (original-source arxana-browser-essays-test--hyperreal-source)
         (live-manifest-plist
          (arxana-browser-essays--load-manifest-file live-manifest))
         (essay-id (plist-get (plist-get live-manifest-plist :essay) :id))
         (section-id "arxana/essay/hyperreal-director-side-a-v1/section/4-introspection")
         (section-name (arxana-browser-essays--section-name live-manifest-plist section-id))
         (heading-text (arxana-browser-essays--section-heading-text
                        live-manifest-plist section-id))
         (section-text (arxana-browser-essays--extract-section-text
                        live-source heading-text))
         (original-live-text (with-temp-buffer
                               (insert-file-contents live-source)
                               (buffer-string)))
         (original-golden-text (with-temp-buffer
                                 (insert-file-contents golden-source)
                                 (buffer-string)))
         (original-real-text (with-temp-buffer
                               (insert-file-contents original-source)
                               (buffer-string)))
         (edited-section
          (replace-regexp-in-string
           "Starling Bank\nbalance, yearly accounts with HMRC, and historical invoices\nsubmitted\\."
           "Starling Bank\nbalance, yearly accounts with HMRC, historical invoices\nsubmitted, and explicit operational snapshots."
           section-text t t))
         (buf (get-buffer-create arxana-browser-essays-text-buffer)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (erase-buffer)
            (insert (format "Section: %s\n\n%s" section-name edited-section))
            (goto-char (point-max))
            (setq-local arxana-browser-essays-edit-mode t)
            (setq-local arxana-browser-essays--source-file live-source)
            (setq-local arxana-browser-essays--essay-id essay-id)
            (setq-local arxana-browser-essays--section-id section-id)
            (setq-local arxana-browser-essays--section-name section-name)
            (setq-local arxana-browser-essays--heading-text heading-text)
            (setq-local arxana-browser-essays--content-start
                        (copy-marker (save-excursion
                                       (goto-char (point-min))
                                       (search-forward "\n\n")
                                       (point)))))
          (with-current-buffer buf
            (cl-letf (((symbol-function 'window-start)
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
                       (lambda (_essay-id) live-manifest-plist))
                      ((symbol-function 'arxana-browser-essays--manifest-file-for-section)
                       (lambda (_section-id) live-manifest))
                      ((symbol-function 'arxana-browser-essays--open-section)
                       (lambda (&rest _args) nil)))
              (arxana-browser-essays-save-section)))
          (should-not (equal original-live-text
                             (with-temp-buffer
                               (insert-file-contents live-source)
                               (buffer-string))))
          (should (equal original-golden-text
                         (with-temp-buffer
                           (insert-file-contents golden-source)
                           (buffer-string))))
          (should (equal original-real-text
                         (with-temp-buffer
                           (insert-file-contents original-source)
                           (buffer-string)))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (delete-directory (plist-get rounds :root) t))))

(ert-deftest arxana-browser-essays-hyperreal-preserved-anchor-replay-keeps-manifest ()
  (let* ((rounds (arxana-browser-essays-test--make-hyperreal-rounds))
         (arxana-browser-essays-manifest-files
          (list (plist-get rounds :live-manifest)))
         (arxana-browser-essays-catalogs
          (arxana-browser-essays-test--hyperreal-catalog rounds))
         (manifest-before
          (arxana-browser-essays--load-manifest-file (plist-get rounds :live-manifest)))
         (section-id arxana-browser-essays-test--hyperreal-section-clarity)
         (ann-id arxana-browser-essays-test--hyperreal-ann-right-effort)
         (heading-text (arxana-browser-essays--section-heading-text manifest-before section-id))
         (section-name (arxana-browser-essays--section-name manifest-before section-id))
         (source-file (plist-get rounds :live-source))
         (manifest-text-before (with-temp-buffer
                                 (insert-file-contents (plist-get rounds :live-manifest))
                                 (buffer-string)))
         (buf nil))
    (unwind-protect
        (progn
          (arxana-browser-essays-test--rewrite-file-text
           source-file
           (lambda (text)
             (replace-regexp-in-string
              "Am I doing something here that matters\\? So, can we\nget indicators"
              "Am I doing something here that matters? That question stays active. So, can we\nget indicators"
              text t t)))
          (setq buf
                (arxana-browser-essays--render-section-text
                 arxana-browser-essays-test--hyperreal-essay-id
                 section-id
                 section-name
                 source-file
                 heading-text
                 (arxana-browser-essays--extract-section-text source-file heading-text)
                 (arxana-browser-essays--annotations-for-section manifest-before section-id)))
          (with-current-buffer buf
            (should-not arxana-browser-essays--unresolved)
            (should (gethash ann-id arxana-browser-essays--source-index)))
          (should (equal manifest-text-before
                         (with-temp-buffer
                           (insert-file-contents (plist-get rounds :live-manifest))
                           (buffer-string)))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (delete-directory (plist-get rounds :root) t))))

(ert-deftest arxana-browser-essays-hyperreal-forced-reanchor-rewrites-manifest ()
  (let* ((rounds (arxana-browser-essays-test--make-hyperreal-rounds))
         (arxana-browser-essays-manifest-files
          (list (plist-get rounds :live-manifest)))
         (arxana-browser-essays-catalogs
          (arxana-browser-essays-test--hyperreal-catalog rounds))
         (manifest-before
          (arxana-browser-essays--load-manifest-file (plist-get rounds :live-manifest)))
         (section-id arxana-browser-essays-test--hyperreal-section-clarity)
         (ann-id arxana-browser-essays-test--hyperreal-ann-right-effort)
         (ann-before (arxana-browser-essays-test--manifest-annotation-by-id
                      manifest-before ann-id))
         (old-passage (plist-get (plist-get ann-before :annotated) :passage))
         (replacement-passage
          "Can we get indicators of improvement\n*other than* or as a *proxy for* unit economics, given that consultancy\ntrading volume is currently low?")
         (heading-text (arxana-browser-essays--section-heading-text manifest-before section-id))
         (section-name (arxana-browser-essays--section-name manifest-before section-id))
         (source-file (plist-get rounds :live-source))
         (buf nil)
         (new-passage nil))
    (unwind-protect
        (progn
          (arxana-browser-essays-test--rewrite-file-text
           source-file
           (lambda (text)
             (replace-regexp-in-string
              (regexp-quote old-passage)
              replacement-passage
              text t t)))
          (setq buf
                (arxana-browser-essays--render-section-text
                 arxana-browser-essays-test--hyperreal-essay-id
                 section-id
                 section-name
                 source-file
                 heading-text
                 (arxana-browser-essays--extract-section-text source-file heading-text)
                 (arxana-browser-essays--annotations-for-section manifest-before section-id)))
          (with-current-buffer buf
            (let ((bounds (gethash ann-id arxana-browser-essays--source-index)))
              (should bounds)
              (setq new-passage
                    (buffer-substring-no-properties (car bounds) (cdr bounds)))))
          (let* ((manifest-after
                  (arxana-browser-essays--load-manifest-file (plist-get rounds :live-manifest)))
                 (ann-after (arxana-browser-essays-test--manifest-annotation-by-id
                             manifest-after ann-id))
                 (stored-passage (plist-get (plist-get ann-after :annotated) :passage)))
            (should-not (equal old-passage stored-passage))
            (should (equal new-passage stored-passage))
            (should (equal replacement-passage stored-passage))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (delete-directory (plist-get rounds :root) t))))

(ert-deftest arxana-browser-essays-hyperreal-substrate-removal-retracts-in-live-round ()
  (let* ((rounds (arxana-browser-essays-test--make-hyperreal-rounds))
         (arxana-browser-essays-manifest-files
          (list (plist-get rounds :live-manifest)))
         (arxana-browser-essays-catalogs
          (arxana-browser-essays-test--hyperreal-catalog rounds))
         (manifest-before
          (arxana-browser-essays--load-manifest-file (plist-get rounds :live-manifest)))
         (golden-before (with-temp-buffer
                          (insert-file-contents (plist-get rounds :golden-manifest))
                          (buffer-string)))
         (real-before (with-temp-buffer
                        (insert-file-contents arxana-browser-essays-test--hyperreal-manifest)
                        (buffer-string)))
         (section-id arxana-browser-essays-test--hyperreal-section-clarity)
         (ann-id arxana-browser-essays-test--hyperreal-ann-right-effort)
         (heading-text (arxana-browser-essays--section-heading-text manifest-before section-id))
         (section-name (arxana-browser-essays--section-name manifest-before section-id))
         (source-file (plist-get rounds :live-source))
         (buf nil))
    (unwind-protect
        (progn
          (setq buf
                (arxana-browser-essays--render-section-text
                 arxana-browser-essays-test--hyperreal-essay-id
                 section-id
                 section-name
                 source-file
                 heading-text
                 (arxana-browser-essays--extract-section-text source-file heading-text)
                 (arxana-browser-essays--annotations-for-section manifest-before section-id)))
          (with-current-buffer buf
            (let ((inhibit-read-only t)
                  (bounds (gethash ann-id arxana-browser-essays--source-index)))
              (should bounds)
              (setq-local arxana-browser-essays-edit-mode t)
              (goto-char (car bounds))
              (delete-region (car bounds) (cdr bounds)))
            (cl-letf (((symbol-function 'window-start)
                       (lambda (&optional _window) 1))
                      ((symbol-function 'message)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'arxana-browser-essays--backup-manifest-files)
                       (lambda () nil))
                      ((symbol-function 'arxana-browser-essays--sync-overlays-to-manifest)
                       (lambda () nil))
                      ((symbol-function 'arxana-browser-essays--sync-annotations-to-xtdb)
                       (lambda (_ids) nil))
                      ((symbol-function 'arxana-browser-essays--open-section)
                       (lambda (&rest _args) nil)))
              (arxana-browser-essays-save-section)))
          (let* ((manifest-after
                  (arxana-browser-essays--load-manifest-file (plist-get rounds :live-manifest)))
                 (ann-after (arxana-browser-essays-test--manifest-annotation-by-id
                             manifest-after ann-id)))
            (should (plist-get ann-after :retracted)))
          (should (equal golden-before
                         (with-temp-buffer
                           (insert-file-contents (plist-get rounds :golden-manifest))
                           (buffer-string))))
          (should (equal real-before
                         (with-temp-buffer
                           (insert-file-contents arxana-browser-essays-test--hyperreal-manifest)
                           (buffer-string)))))
      (when (buffer-live-p buf)
        (kill-buffer buf))
      (delete-directory (plist-get rounds :root) t))))

(ert-deftest arxana-browser-essays-hyperreal-retraction-survives-reopen ()
  (let* ((rounds (arxana-browser-essays-test--make-hyperreal-rounds))
         (arxana-browser-essays-manifest-files
          (list (plist-get rounds :live-manifest)))
         (arxana-browser-essays-catalogs
          (arxana-browser-essays-test--hyperreal-catalog rounds))
         (manifest-before
          (arxana-browser-essays--load-manifest-file (plist-get rounds :live-manifest)))
         (section-id arxana-browser-essays-test--hyperreal-section-clarity)
         (ann-id arxana-browser-essays-test--hyperreal-ann-right-effort)
         (heading-text (arxana-browser-essays--section-heading-text manifest-before section-id))
         (section-name (arxana-browser-essays--section-name manifest-before section-id))
         (source-file (plist-get rounds :live-source))
         (text-buf nil)
         (notes-buf nil))
    (unwind-protect
        (progn
          (setq text-buf
                (arxana-browser-essays--render-section-text
                 arxana-browser-essays-test--hyperreal-essay-id
                 section-id
                 section-name
                 source-file
                 heading-text
                 (arxana-browser-essays--extract-section-text source-file heading-text)
                 (arxana-browser-essays--annotations-for-section manifest-before section-id)))
          (with-current-buffer text-buf
            (let ((inhibit-read-only t)
                  (bounds (gethash ann-id arxana-browser-essays--source-index)))
              (should bounds)
              (setq-local arxana-browser-essays-edit-mode t)
              (goto-char (car bounds))
              (delete-region (car bounds) (cdr bounds)))
            (cl-letf (((symbol-function 'window-start)
                       (lambda (&optional _window) 1))
                      ((symbol-function 'message)
                       (lambda (&rest _args) nil))
                      ((symbol-function 'arxana-browser-essays--backup-manifest-files)
                       (lambda () nil))
                      ((symbol-function 'arxana-browser-essays--sync-overlays-to-manifest)
                       (lambda () nil))
                      ((symbol-function 'arxana-browser-essays--sync-annotations-to-xtdb)
                       (lambda (_ids) nil))
                      ((symbol-function 'arxana-browser-essays--display-section-buffers)
                       (lambda (_text _notes) nil)))
              (arxana-browser-essays-save-section)
              (arxana-browser-essays--open-section
               arxana-browser-essays-test--hyperreal-essay-id
               section-id
               section-name)))
          (setq notes-buf (get-buffer arxana-browser-essays-notes-buffer))
          (with-current-buffer notes-buf
            (goto-char (point-min))
            (should (search-forward "[retracted]" nil t))
            (should (equal ann-id
                           (get-text-property (match-beginning 0)
                                              'arxana-essay-annotation-id)))))
      (when (buffer-live-p text-buf)
        (kill-buffer text-buf))
      (when (buffer-live-p notes-buf)
        (kill-buffer notes-buf))
      (delete-directory (plist-get rounds :root) t))))

(ert-deftest arxana-browser-essays-menu-items-count-live-section-annotations ()
  (let ((arxana-browser-essays-catalogs
         '((:label "Essay Alpha"
            :description "Annotated essay"
            :essay-id "essay:alpha")))
        (arxana-browser-essays--xtdb-catalog-cache
         arxana-browser-essays--xtdb-catalog-cache-unset)
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
                 manifest))
              ((symbol-function 'arxana-store-sync-enabled-p)
               (lambda () nil)))
      (let ((item (car (arxana-browser-essays-menu-items))))
        (should (= 2 (plist-get item :annotation-count)))
        (should (equal "Annotated essay (2 sections, 2 annotations)"
                       (plist-get item :description)))))))

(ert-deftest arxana-browser-essays-render-section-notes-keeps-retracted-visible ()
  (let ((buf nil))
    (unwind-protect
        (progn
          (setq buf
                (arxana-browser-essays--render-section-notes
                 "Section One"
                 '((:id "ann:1"
                    :hx-type "annotation/grounds"
                    :annotated (:entity-id "section:1" :passage "Live")
                    :source (:pattern-name "pattern/live"
                             :passage "! conclusion: live"))
                   (:id "ann:2"
                    :hx-type "annotation/grounds"
                    :annotated (:entity-id "section:1" :passage "Retracted")
                    :source (:pattern-name "pattern/retracted"
                             :passage "! conclusion: retracted")
                    :retracted t
                    :retraction-kind manual
                    :retracted-at "2026-05-14"))))
          (with-current-buffer buf
            (goto-char (point-min))
            (should (search-forward "[retracted: manual @ 2026-05-14]" nil t))
            (let ((face (get-text-property (match-beginning 0) 'face)))
              (should (arxana-browser-essays-test--face-has-p
                       face
                       'arxana-browser-essays-retracted-face)))
            (should (search-forward "pattern/retracted" nil t))))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest arxana-browser-essays-open-section-passes-retracted-annotations-to-notes ()
  (let* ((manifest
          '(:essay (:id "essay:alpha")
            :sections ((:id "section:1"
                        :name "Section One"
                        :props ((heading-text . "Section One"))))
            :annotations ((:id "ann:1"
                           :hx-type "annotation/grounds"
                           :annotated (:entity-id "section:1" :passage "Live")
                           :source (:pattern-name "pattern/live"
                                    :passage "! conclusion: live"))
                          (:id "ann:2"
                           :hx-type "annotation/grounds"
                           :annotated (:entity-id "section:1" :passage "Retracted")
                           :source (:pattern-name "pattern/retracted"
                                    :passage "! conclusion: retracted")
                           :retracted t))))
         (notes-annotations nil)
         (text-annotations nil))
    (cl-letf (((symbol-function 'arxana-browser-essays--manifest-for)
               (lambda (_essay-id) manifest))
              ((symbol-function 'arxana-browser-essays--catalog-source-file)
               (lambda (_essay-id) "/tmp/demo.md"))
              ((symbol-function 'arxana-browser-essays--section-heading-text)
               (lambda (_manifest _section-id) "Section One"))
              ((symbol-function 'arxana-browser-essays--extract-section-text)
               (lambda (&rest _args) "## 1. Section One\n\nBody\n"))
              ((symbol-function 'arxana-browser-essays--render-section-text)
               (lambda (&rest args)
                 (setq text-annotations (nth 6 args))
                 (get-buffer-create " *essays-open-section-text*")))
              ((symbol-function 'arxana-browser-essays--render-section-notes)
               (lambda (_section-name annotations)
                 (setq notes-annotations annotations)
                 (get-buffer-create " *essays-open-section-notes*")))
              ((symbol-function 'arxana-browser-essays--display-section-buffers)
               (lambda (&rest _args) nil)))
      (arxana-browser-essays--open-section "essay:alpha" "section:1" "Section One"))
    (should (= 1 (length text-annotations)))
    (should (= 2 (length notes-annotations)))
    (should-not (seq-some #'arxana-browser-essays--annotation-retracted-p
                          text-annotations))
    (should (seq-some #'arxana-browser-essays--annotation-retracted-p
                      notes-annotations))))

(ert-deftest arxana-browser-essays-ensure-essay-persists-registry-props ()
  (let* ((tmpdir (make-temp-file "arxana-essay-registry-" t))
         (manifest-file (expand-file-name "annotations.el" tmpdir))
         (essay '(:id "essay:alpha"
                  :name "Essay Alpha"
                  :type "arxana/essay"
                  :source-file "essay.md"
                  :props ((version . "v1"))))
         (arxana-browser-essays-catalogs
          '((:essay-id "essay:alpha"
             :label "Annotated Essay Alpha"
             :description "Imported essay")))
         captured)
    (unwind-protect
        (cl-letf (((symbol-function 'arxana-store-ensure-entity)
                   (lambda (&rest args)
                     (setq captured args))))
          (arxana-browser-essays--ensure-essay essay manifest-file nil)
          (let ((props (plist-get captured :props)))
            (should (equal "essay:alpha" (plist-get captured :id)))
            (should (equal "Annotated Essay Alpha" (alist-get 'label props)))
            (should (equal "Imported essay" (alist-get 'description props)))
            (should (equal manifest-file (alist-get 'manifest-file props)))
            (should (equal (expand-file-name "essay.md" tmpdir)
                           (alist-get 'source-file props)))
            (should (equal "v1" (alist-get 'version props)))))
      (delete-directory tmpdir t))))

(ert-deftest arxana-browser-essays-manifest-for-loads-via-manifest-file ()
  (let* ((tmpdir (make-temp-file "arxana-essay-manifest-load-" t))
         (manifest-file (expand-file-name "annotations.el" tmpdir))
         (arxana-browser-essays-catalogs
          `((:essay-id "essay:alpha"
             :manifest-file ,manifest-file))))
    (unwind-protect
        (progn
          (with-temp-file manifest-file
            (insert "(defconst test-essay-manifest\n")
            (insert "  '(:essay (:id \"essay:alpha\"\n")
            (insert "            :name \"Essay Alpha\"\n")
            (insert "            :source-file \"essay.md\")\n")
            (insert "    :sections ()\n")
            (insert "    :annotations ()))\n"))
          (let ((arxana-browser-essays--xtdb-catalog-cache
                 arxana-browser-essays--xtdb-catalog-cache-unset))
            (cl-letf (((symbol-function 'arxana-store-sync-enabled-p)
                       (lambda () nil)))
              (let ((manifest (arxana-browser-essays--manifest-for "essay:alpha")))
                (should (equal "essay:alpha"
                               (plist-get (plist-get manifest :essay) :id)))))))
      (delete-directory tmpdir t))))

(ert-deftest arxana-browser-essays-catalog-source-file-resolves-relative-to-manifest ()
  (let* ((tmpdir (make-temp-file "arxana-essay-source-resolve-" t))
         (manifest-file (expand-file-name "annotations.el" tmpdir))
         (source-file (expand-file-name "essay.md" tmpdir))
         (arxana-browser-essays-catalogs
          `((:essay-id "essay:alpha"
             :manifest-file ,manifest-file))))
    (unwind-protect
        (progn
          (with-temp-file manifest-file
            (insert "(defconst test-essay-manifest\n")
            (insert "  '(:essay (:id \"essay:alpha\"\n")
            (insert "            :name \"Essay Alpha\"\n")
            (insert "            :source-file \"essay.md\")\n")
            (insert "    :sections ()\n")
            (insert "    :annotations ()))\n"))
          (with-temp-file source-file
            (insert "# Essay\n"))
          (let ((arxana-browser-essays--xtdb-catalog-cache
                 arxana-browser-essays--xtdb-catalog-cache-unset))
            (cl-letf (((symbol-function 'arxana-store-sync-enabled-p)
                       (lambda () nil)))
              (let ((resolved
                     (arxana-browser-essays--catalog-source-file
                      "essay:alpha")))
                (should (equal source-file resolved))))))
      (delete-directory tmpdir t))))

(ert-deftest arxana-browser-essays-xtdb-catalog-merges-before-augmentations ()
  (let* ((arxana-browser-essays-catalogs
          '((:essay-id "essay:alpha"
             :label "Fallback Alpha"
             :description "Fallback"
             :manifest-file "/tmp/alpha-override/annotations.el"
             :source-file "/tmp/alpha-override/essay.md")
            (:essay-id "essay:beta"
             :label "Fallback Beta")))
         (arxana-browser-essays--xtdb-catalog-cache
          arxana-browser-essays--xtdb-catalog-cache-unset)
         (response
          (list
           (cons :entities
                 (list
                 '((:id . "essay:alpha")
                   (:entity/name . "Essay Alpha")
                    (:props (:label . "XTDB Alpha")
                            (:description . "Projected")
                            (:manifest-file . "/xtdb/alpha/annotations.el")
                            (:source-file . "/xtdb/alpha/essay.md"))))))))
    (cl-letf (((symbol-function 'arxana-store-sync-enabled-p)
               (lambda () t))
              ((symbol-function 'arxana-store-fetch-entities-latest)
               (lambda (&rest _args)
                 response)))
      (let ((catalogs (arxana-browser-essays--catalogs)))
        (should (= 2 (length catalogs)))
        (should (equal "XTDB Alpha"
                       (plist-get (car catalogs) :label)))
        (should (equal "/tmp/alpha-override/annotations.el"
                       (plist-get (car catalogs) :manifest-file)))
        (should (equal "/tmp/alpha-override/essay.md"
                       (plist-get (car catalogs) :source-file)))
        (should (equal "essay:beta"
                       (plist-get (cadr catalogs) :essay-id)))))))

(ert-deftest arxana-browser-essays-refresh-clears-xtdb-catalog-cache ()
  (let ((arxana-browser-essays-manifest-files nil)
        (arxana-browser-essays-catalogs nil)
        (arxana-browser-essays--xtdb-catalog-cache '((:essay-id "cached"))))
    (cl-letf (((symbol-function 'custom-reevaluate-setting)
               (lambda (&rest _args) nil))
              ((symbol-function 'require)
               (lambda (&rest _args) nil)))
      (arxana-browser-essays-refresh)
      (should (eq arxana-browser-essays--xtdb-catalog-cache
                  arxana-browser-essays--xtdb-catalog-cache-unset)))))

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

(ert-deftest arxana-docbook-open-uri-opens-essay-section-routes ()
  (let ((arxana-browser--stack nil)
        (arxana-browser--context nil)
        (opened nil)
        (browser-buf (get-buffer-create arxana-browser--buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'arxana-browser-browse)
                   (lambda ()
                     browser-buf))
                  ((symbol-function 'arxana-browser-essays--manifest-for)
                   (lambda (_essay-id)
                     '(:sections ((:id "section/2" :name "Section Two")))))
                  ((symbol-function 'arxana-browser-essays--section-name)
                   (lambda (_manifest section-id)
                     (if (equal section-id "section/2")
                         "Section Two"
                       section-id)))
                  ((symbol-function 'arxana-browser-essays-open)
                   (lambda (item)
                     (setq opened item)))
                  ((symbol-function 'arxana-ui-refresh)
                   (lambda () nil)))
          (arxana-docbook-open-uri
           "arxana://essay/essay%3Aalpha/section/section%2F2")
          (should (equal 'essays-section (plist-get opened :type)))
          (should (equal "essay:alpha" (plist-get opened :essay-id)))
          (should (equal "section/2" (plist-get opened :section-id)))
          (should (equal "Section Two" (plist-get opened :label))))
      (when (buffer-live-p browser-buf)
        (kill-buffer browser-buf)))))

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
