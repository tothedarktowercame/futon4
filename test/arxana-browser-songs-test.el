;;; arxana-browser-songs-test.el --- Tests for songs browser -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-browser-core)
(require 'arxana-browser-songs)

(ert-deftest arxana-browser-core-menu-includes-songs ()
  (should
   (member "Songs"
           (mapcar (lambda (item) (plist-get item :label))
                   (arxana-browser--menu-items)))))

(ert-deftest arxana-browser-songs-menu-items-show-counts ()
  (cl-letf (((symbol-function 'arxana-store-sync-enabled-p) (lambda () t))
            ((symbol-function 'arxana-store-fetch-entities-latest)
             (lambda (&rest args)
               (pcase (plist-get args :type)
                 ("arxana/media-lyrics"
                  (list
                   (cons :entities
                         (list
                          '((:entity/id . "lyrics-2")
                            (:entity/name . "Beta")
                            (:entity/type . "arxana/media-lyrics")
                            (:entity/source . "b"))
                          '((:entity/id . "lyrics-1")
                            (:entity/name . "Alpha")
                            (:entity/type . "arxana/media-lyrics")
                            (:entity/source . "a"))))))
                 (_ '((:entities . nil)))))))
    (let* ((items (arxana-browser-songs-menu-items))
           (chapbook (seq-find (lambda (item)
                                 (equal (plist-get item :label) "Chapbook lyrics"))
                               items))
           (suite (seq-find (lambda (item)
                              (equal (plist-get item :label) "Suite songs"))
                            items)))
      (should (eq (plist-get chapbook :type) 'songs-catalog))
      (should (string-match-p "^2 entries\\." (plist-get chapbook :description)))
      (should (string-match-p "^0 entries\\." (plist-get suite :description))))))

(ert-deftest arxana-browser-songs-items-build-song-entities ()
  (cl-letf (((symbol-function 'arxana-store-sync-enabled-p) (lambda () t))
            ((symbol-function 'arxana-store-fetch-entities-latest)
             (lambda (&rest _)
               (list
                (cons :entities
                      (list
                       '((:entity/id . "lyrics-2")
                         (:entity/name . "Beta")
                         (:entity/type . "arxana/media-lyrics")
                         (:entity/source . "line one\nline two\n"))
                       '((:entity/id . "lyrics-1")
                         (:entity/name . "Alpha")
                         (:entity/type . "arxana/media-lyrics")
                         (:entity/source . "solo line"))))))))
    (let* ((items (arxana-browser-songs-items '(:view songs-chapbook)))
           (first (car items))
           (second (cadr items)))
      (should (= 2 (length items)))
      (should (eq (plist-get first :type) 'song-entity))
      (should (equal "Alpha" (plist-get first :label)))
      (should (equal "lyrics-1" (plist-get first :entity-id)))
      (should (= 1 (plist-get first :line-count)))
      (should (equal "Beta" (plist-get second :label)))
      (should (= 2 (plist-get second :line-count))))))

(ert-deftest arxana-browser-songs-open-renders-entity-text ()
  (let ((buffer-name "*Arxana Song*"))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'arxana-store-fetch-entity)
                     (lambda (_id)
                       '((:entity . ((:entity/id . "lyrics-1")
                                     (:entity/name . "Alpha")
                                     (:entity/type . "arxana/media-lyrics")
                                     (:entity/source . "line one\nline two"))))))
                    ((symbol-function 'arxana-store-fetch-hyperedges)
                     (lambda (&rest _) '((:hyperedges . nil))))
                    ((symbol-function 'display-buffer-in-side-window)
                     (lambda (&rest _) nil)))
            (arxana-browser-songs-open '(:entity-id "lyrics-1"))
            (with-current-buffer buffer-name
              (let ((content (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "Title: Alpha" content))
                (should (string-match-p "Entity: lyrics-1" content))
                (should (string-match-p "Lines: 2" content))
                (should (string-match-p "line one" content))))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest arxana-browser-songs-render-notes-keeps-note-index-after-org-mode ()
  (let* ((entity '((:entity/id . "song:abi")
                   (:entity/name . "Abi")
                   (:entity/type . "arxana/song")))
         (sections
          '((:title "Supported By Chapbook"
             :items ((:kind support
                      :hyperedge-id "hx:support"
                      :target-id "doc:no-longer-alone"
                      :target-name "no-longer-alone (lyrics)"
                      :current-passage "line 7: Heavy, the sleep of butterflies"
                      :target-passage "the butterfly craves release to the open sky"
                      :note "Against heaviness, release"
                      :marker-label "[1]")))
            (:title "Open Questions"
             :items ((:kind open-question
                      :hyperedge-id "hx:question"
                      :passage "line 7: Heavy, the sleep of butterflies"
                      :question "Why heavy?"
                      :marker-label "[2]")))))
         (buffer-name "*Arxana Song Notes*"))
    (unwind-protect
        (with-current-buffer (arxana-browser-songs--render-notes entity sections)
          (should (hash-table-p arxana-browser-songs--note-index))
          (let ((bounds (gethash "hx:support" arxana-browser-songs--note-index)))
            (should (consp bounds))
            (should (markerp (car bounds)))
            (should (markerp (cdr bounds))))
          (goto-char (point-min))
          (should (search-forward "[1] no-longer-alone (lyrics)" nil t))
          (beginning-of-line)
          (should (equal 'target (get-text-property (point) 'arxana-song-jump-kind)))
          (should (equal "doc:no-longer-alone"
                         (get-text-property (point) 'arxana-song-target-id)))
          (should (search-forward "the butterfly craves release to the open sky" nil t))
          (beginning-of-line)
          (should (equal 'arxana-browser-songs-note-quote-face
                         (get-text-property (point) 'face)))
          (should (equal 'arxana-browser-songs-note-quote-face
                         (get-text-property (point) 'font-lock-face)))
          (should (equal 'target (get-text-property (point) 'arxana-song-jump-kind)))
          (should (equal "doc:no-longer-alone"
                         (get-text-property (point) 'arxana-song-target-id))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest arxana-browser-songs-text-syncs-active-note-from-point ()
  (let ((song-buffer "*Arxana Song*")
        (notes-buffer "*Arxana Song Notes*"))
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'arxana-store-fetch-entity)
                (lambda (id)
                  (pcase id
                    ("song:abi"
                     '((:entity . ((:entity/id . "song:abi")
                                   (:entity/name . "Abi")
                                   (:entity/type . "arxana/song")
                                   (:entity/source . "Blue carnations, in the soil\nHeavy, the sleep of butterflies")))))
                    ("doc:no-longer-alone"
                     '((:entity . ((:entity/id . "doc:no-longer-alone")
                                   (:entity/name . "no-longer-alone (lyrics)")
                                   (:entity/type . "arxana/media-lyrics")
                                   (:entity/source . "the butterfly craves release to the open sky")))))
                    (_ '((:entity . nil))))))
               ((symbol-function 'arxana-store-fetch-hyperedges)
                (lambda (&rest _)
                  '((:hyperedges
                     . (((:hx/id . "hx:support")
                         (:hx/type . :annotation/supports)
                         (:hx/props . ((:note . "Against heaviness, release")))
                         (:hx/ends . (((:role . :annotated)
                                       (:entity-id . "song:abi")
                                       (:passage . "line 2: Heavy, the sleep of butterflies"))
                                      ((:role . :source)
                                       (:entity-id . "doc:no-longer-alone")
                                       (:passage . "the butterfly craves release to the open sky"))))))))))
               ((symbol-function 'display-buffer-in-side-window)
                (lambda (&rest _) nil))
               ((symbol-function 'display-buffer)
                (lambda (&rest _) nil)))
            (arxana-browser-songs-open '(:entity-id "song:abi"))
            (with-current-buffer song-buffer
              (goto-char (point-min))
              (search-forward "Heavy, the sleep of butterflies")
              (goto-char (match-beginning 0))
              (arxana-browser-songs--sync-note-from-point))
            (with-current-buffer notes-buffer
              (should (overlayp arxana-browser-songs--note-highlight-overlay))
              (should (equal "hx:support"
                             (get-text-property
                              (overlay-start arxana-browser-songs--note-highlight-overlay)
                              'arxana-song-hyperedge-id))))))
      (when (get-buffer song-buffer)
        (kill-buffer song-buffer))
      (when (get-buffer notes-buffer)
        (kill-buffer notes-buffer)))))

(ert-deftest arxana-browser-songs-notes-sync-source-from-gloss-line ()
  (let ((song-buffer "*Arxana Song*")
        (notes-buffer "*Arxana Song Notes*"))
    (unwind-protect
        (progn
          (cl-letf
              (((symbol-function 'arxana-store-fetch-entity)
                (lambda (id)
                  (pcase id
                    ("song:abi"
                     '((:entity . ((:entity/id . "song:abi")
                                   (:entity/name . "Abi")
                                   (:entity/type . "arxana/song")
                                   (:entity/source . "Blue carnations, in the soil\nHeavy, the sleep of butterflies")))))
                    ("doc:no-longer-alone"
                     '((:entity . ((:entity/id . "doc:no-longer-alone")
                                   (:entity/name . "no-longer-alone (lyrics)")
                                   (:entity/type . "arxana/media-lyrics")
                                   (:entity/source . "the butterfly craves release to the open sky")))))
                    (_ '((:entity . nil))))))
               ((symbol-function 'arxana-store-fetch-hyperedges)
                (lambda (&rest _)
                  '((:hyperedges
                     . (((:hx/id . "hx:support")
                         (:hx/type . :annotation/supports)
                         (:hx/props . ((:note . "Against heaviness, release")))
                         (:hx/ends . (((:role . :annotated)
                                       (:entity-id . "song:abi")
                                       (:passage . "line 2: Heavy, the sleep of butterflies"))
                                      ((:role . :source)
                                       (:entity-id . "doc:no-longer-alone")
                                       (:passage . "the butterfly craves release to the open sky"))))))))))
               ((symbol-function 'display-buffer-in-side-window)
                (lambda (&rest _) nil))
               ((symbol-function 'display-buffer)
                (lambda (&rest _) nil))
               ((symbol-function 'recenter)
                (lambda (&rest _) nil)))
            (arxana-browser-songs-open '(:entity-id "song:abi"))
            (with-current-buffer notes-buffer
              (goto-char (point-min))
              (search-forward "Gloss: Against heaviness, release")
              (goto-char (match-beginning 0))
              (arxana-browser-songs--sync-source-from-point))
            (with-current-buffer song-buffer
              (should (overlayp arxana-browser-songs--active-overlay))
              (should (equal "Heavy, the sleep of butterflies"
                             (buffer-substring-no-properties
                              (overlay-start arxana-browser-songs--active-overlay)
                              (overlay-end arxana-browser-songs--active-overlay)))))))
      (when (get-buffer song-buffer)
        (kill-buffer song-buffer))
      (when (get-buffer notes-buffer)
        (kill-buffer notes-buffer)))))

(ert-deftest arxana-browser-songs-overlay-hyperedge-prefers-narrowest-span ()
  (with-temp-buffer
    (insert "abcdef")
    (let ((wide (make-overlay 1 6))
          (narrow (make-overlay 3 4)))
      (overlay-put wide 'arxana-song-hyperedge-id "wide")
      (overlay-put narrow 'arxana-song-hyperedge-id "narrow")
      (should (equal "narrow"
                     (arxana-browser-songs--overlay-hyperedge-id-at 3))))))

(ert-deftest arxana-browser-songs-render-entity-clears-stale-overlays ()
  (let* ((entity '((:entity/id . "song:abi")
                   (:entity/name . "Abi")
                   (:entity/type . "arxana/song")
                   (:entity/source . "Blue carnations, in the soil")))
         (sections nil)
         (buffer-name "*Arxana Song*"))
    (unwind-protect
        (with-current-buffer (get-buffer-create buffer-name)
          (make-overlay (point-min) (point-min))
          (should (< 0 (length (overlays-in (point-min) (point-max)))))
          (arxana-browser-songs--render-entity entity sections)
          (should (= 0 (length (overlays-in (point-min) (point-min))))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest arxana-browser-songs-mode-maps-bind-highlight-toggle ()
  (should (eq #'arxana-browser-songs-left-or-return
              (lookup-key arxana-browser-songs-text-mode-map (kbd "<left>"))))
  (should (eq #'arxana-browser-songs-left-or-return
              (lookup-key arxana-browser-songs-notes-mode-map (kbd "<left>"))))
  (should (eq #'arxana-browser-songs-toggle-highlight-scope
              (lookup-key arxana-browser-songs-text-mode-map (kbd "h"))))
  (should (eq #'arxana-browser-songs-toggle-highlight-scope
              (lookup-key arxana-browser-songs-notes-mode-map (kbd "h")))))

(ert-deftest arxana-browser-songs-left-at-point-min-returns-to-browser ()
  (let ((browser-buf (get-buffer-create "*Arxana Browser*"))
        (song-buffer "*Arxana Song*")
        (notes-buffer "*Arxana Song Notes*"))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer browser-buf)
          (cl-letf (((symbol-function 'arxana-store-fetch-entity)
                     (lambda (_id)
                       '((:entity . ((:entity/id . "lyrics-1")
                                     (:entity/name . "Alpha")
                                     (:entity/type . "arxana/media-lyrics")
                                     (:entity/source . "line one\nline two"))))))
                    ((symbol-function 'arxana-store-fetch-hyperedges)
                     (lambda (&rest _) '((:hyperedges . nil))))
                    ((symbol-function 'display-buffer-in-side-window)
                     (lambda (&rest _) nil)))
            (arxana-browser-songs-open '(:entity-id "lyrics-1"))
            (with-current-buffer song-buffer
              (should (eq browser-buf arxana-ui-return-buffer))
              (should (window-configuration-p arxana-ui-return-window-config))
              (goto-char (point-min))
              (arxana-ui-left-or-return))
            (should (eq browser-buf (window-buffer (selected-window))))))
      (when (get-buffer browser-buf)
        (kill-buffer browser-buf))
      (when (get-buffer song-buffer)
        (kill-buffer song-buffer))
      (when (get-buffer notes-buffer)
        (kill-buffer notes-buffer)))))

(ert-deftest arxana-browser-songs-left-at-point-min-returns-from-linked-doc-to-source-song ()
  (let ((browser-buf (get-buffer-create "*Arxana Browser*"))
        (song-buffer "*Arxana Song*")
        (notes-buffer "*Arxana Song Notes*"))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer browser-buf)
          (cl-letf
              (((symbol-function 'arxana-store-fetch-entity)
                (lambda (id)
                  (pcase id
                    ("song:abi"
                     '((:entity . ((:entity/id . "song:abi")
                                   (:entity/name . "Abi")
                                   (:entity/type . "arxana/song")
                                   (:entity/source . "Blue carnations, in the soil\nHeavy, the sleep of butterflies")))))
                    ("doc:no-longer-alone"
                     '((:entity . ((:entity/id . "doc:no-longer-alone")
                                   (:entity/name . "no-longer-alone (lyrics)")
                                   (:entity/type . "arxana/media-lyrics")
                                   (:entity/source . "the butterfly craves release to the open sky")))))
                    (_ '((:entity . nil))))))
               ((symbol-function 'arxana-store-fetch-hyperedges)
                (lambda (&rest args)
                  (pcase (plist-get args :end)
                    ("song:abi"
                     '((:hyperedges
                        . (((:hx/id . "hx:support")
                            (:hx/type . :annotation/supports)
                            (:hx/props . ((:note . "Against heaviness, release")))
                            (:hx/ends . (((:role . :annotated)
                                          (:entity-id . "song:abi")
                                          (:passage . "line 2: Heavy, the sleep of butterflies"))
                                         ((:role . :source)
                                          (:entity-id . "doc:no-longer-alone")
                                          (:passage . "the butterfly craves release to the open sky")))))))))
                    ("doc:no-longer-alone"
                     '((:hyperedges
                        . (((:hx/id . "hx:support")
                            (:hx/type . :annotation/supports)
                            (:hx/props . ((:note . "Against heaviness, release")))
                            (:hx/ends . (((:role . :annotated)
                                          (:entity-id . "song:abi")
                                          (:passage . "line 2: Heavy, the sleep of butterflies"))
                                         ((:role . :source)
                                          (:entity-id . "doc:no-longer-alone")
                                          (:passage . "the butterfly craves release to the open sky")))))))))
                    (_ '((:hyperedges . nil))))))
               ((symbol-function 'display-buffer-in-side-window)
                (lambda (&rest _) nil))
               ((symbol-function 'display-buffer)
                (lambda (&rest _) nil))
               ((symbol-function 'recenter)
                (lambda (&rest _) nil)))
            (arxana-browser-songs-open '(:entity-id "song:abi"))
            (with-current-buffer notes-buffer
              (goto-char (point-min))
              (search-forward "the butterfly craves release to the open sky")
              (beginning-of-line)
              (arxana-browser-songs-notes-activate))
            (with-current-buffer song-buffer
              (should (string-match-p (regexp-quote "Title: no-longer-alone (lyrics)")
                                      (buffer-substring-no-properties (point-min) (point-max))))
              (should (equal "song:abi" (plist-get arxana-browser-songs--return-item :entity-id)))
              (goto-char (point-min))
              (arxana-browser-songs-left-or-return)
              (should (string-match-p "Title: Abi"
                                      (buffer-substring-no-properties (point-min) (point-max))))
              (goto-char (point-min))
              (arxana-browser-songs-left-or-return))
            (should (eq browser-buf (window-buffer (selected-window))))))
      (when (get-buffer browser-buf)
        (kill-buffer browser-buf))
      (when (get-buffer song-buffer)
        (kill-buffer song-buffer))
      (when (get-buffer notes-buffer)
        (kill-buffer notes-buffer)))))

(ert-deftest arxana-browser-songs-unwrap-entities-ignores-top-level-metadata ()
  (let* ((response '((:profile . "default")
                     (:type . "arxana/media-lyrics")
                     (:entities . (((:id . "lyrics-1")
                                    (:name . "Alpha")
                                    (:type . :arxana/media-lyrics)
                                    (:source . "one"))))))
         (entities (arxana-browser-songs--unwrap-entities response)))
    (should (= 1 (length entities)))
    (should (equal "lyrics-1" (arxana-browser-songs--entity-id (car entities))))))

(ert-deftest arxana-browser-songs-parse-suite-file-prefers-itpar-and-song-envs ()
  (let ((file (make-temp-file "arxana-suite-" nil ".tex"
                              (mapconcat
                               #'identity
                               '("\\section*{\\huge \\book The Sea}"
                                 "\\section{Salty Moon}"
                                 "\\textbf{C F7}"
                                 "This is ignored"
                                 "\\begin{itpar}"
                                 "Blue carnations"
                                 "Kiss me in the moonlight"
                                 "\\end{itpar}"
                                 "\\section*{\\huge \\book A House}"
                                 "\\section{Last coffee before the end of the world}"
                                 "\\begin{song}{title={}}"
                                 "\\begin{verse}"
                                 "And the ^{Cm7}sun forever ^{Gm}grows \\\\"
                                 "I'm not ^{Go}afraid"
                                 "\\end{verse}"
                                 "\\end{song}"
                                 "\\appendix"
                                 "\\section{Inventory}"
                                 "Ignore me")
                               "\n"))))
    (unwind-protect
        (let* ((songs (arxana-browser-songs-parse-suite-file file))
               (first (car songs))
               (second (cadr songs)))
          (should (= 2 (length songs)))
          (should (equal "Salty Moon" (plist-get first :name)))
          (should (equal "The Sea" (plist-get first :movement)))
          (should (string-match-p "Blue carnations" (plist-get first :source)))
          (should-not (string-match-p "This is ignored" (plist-get first :source)))
          (should (equal "Last coffee before the end of the world" (plist-get second :name)))
          (should (string-match-p "And the sun forever grows" (plist-get second :source)))
          (should (string-match-p "I'm not afraid" (plist-get second :source))))
      (ignore-errors (delete-file file)))))

(ert-deftest arxana-browser-songs-annotation-sections-handle-supports-and-backlinks ()
  (let* ((song '((:id . "song:abi")
                 (:name . "Abi")
                 (:type . :arxana/song)
                 (:source . "Heavy, the sleep of butterflies")))
         (doc '((:id . "doc:no-longer-alone")
                (:name . "no-longer-alone (lyrics)")
                (:type . :arxana/media-lyrics)
                (:source . "the butterfly craves release to the open sky")))
         (support-edge
          '((:hx/id . "hx:support")
            (:hx/type . :annotation/supports)
            (:hx/props . ((:note . "Against heaviness, release")))
            (:hx/ends . (((:role . :annotated)
                          (:entity-id . "song:abi")
                          (:passage . "line 7"))
                         ((:role . :source)
                          (:entity-id . "doc:no-longer-alone")
                          (:passage . "butterfly"))))))
         (question-edge
          '((:hx/id . "hx:question")
            (:hx/type . :annotation/open-question)
            (:hx/props . ((:question . "Why heavy?")))
            (:hx/ends . (((:role . :annotated)
                          (:entity-id . "song:abi")
                          (:passage . "line 7")))))))
    (cl-letf (((symbol-function 'arxana-store-fetch-hyperedges)
               (lambda (&rest args)
                 (pcase (plist-get args :end)
                   ("song:abi" `((:hyperedges . (,support-edge ,question-edge))))
                   ("doc:no-longer-alone" `((:hyperedges . (,support-edge))))
                   (_ '((:hyperedges . nil))))))
              ((symbol-function 'arxana-store-fetch-entity)
               (lambda (id)
                 (pcase id
                   ("song:abi" `((:entity . ,song)))
                   ("doc:no-longer-alone" `((:entity . ,doc)))
                   (_ '((:entity . nil)))))))
      (let* ((song-sections (arxana-browser-songs--annotation-sections song))
             (supports (plist-get (car song-sections) :items))
             (questions (plist-get (cadr song-sections) :items))
             (doc-sections (arxana-browser-songs--annotation-sections doc))
             (backlinks (plist-get (car doc-sections) :items)))
        (should (equal "Supported By Chapbook" (plist-get (car song-sections) :title)))
        (should (= 1 (length supports)))
        (should (equal "doc:no-longer-alone" (plist-get (car supports) :target-id)))
        (should (equal "Against heaviness, release" (plist-get (car supports) :note)))
        (should (equal "Open Questions" (plist-get (cadr song-sections) :title)))
        (should (= 1 (length questions)))
        (should (equal "Why heavy?" (plist-get (car questions) :question)))
        (should (equal "Backlinks To Suite Songs" (plist-get (car doc-sections) :title)))
        (should (= 1 (length backlinks)))
        (should (equal "song:abi" (plist-get (car backlinks) :target-id)))))))

(ert-deftest arxana-browser-songs-sort-sections-orders-by-passage-before-labeling ()
  (let* ((sections
          '((:title "Supported By Chapbook"
             :items ((:kind support
                      :hyperedge-id "hx:memory"
                      :current-passage "lines 15-18: One day the sea will carry us away from our memories")
                     (:kind support
                      :hyperedge-id "hx:nocturne"
                      :current-passage "lines 5-8: The mist is wrapped around your shoulders")
                     (:kind support
                      :hyperedge-id "hx:blue"
                      :current-passage "lines 1-4: Blue carnations, in the soil")
                     (:kind support
                      :hyperedge-id "hx:butterfly"
                      :current-passage "line 7: Heavy, the sleep of butterflies")))))
         (sorted (arxana-browser-songs--sort-sections sections))
         (labeled (arxana-browser-songs--label-sections sorted))
         (items (plist-get (car labeled) :items)))
    (should (equal '("hx:blue" "hx:butterfly" "hx:nocturne" "hx:memory")
                   (mapcar (lambda (entry) (plist-get entry :hyperedge-id)) items)))
    (should (equal '("[1]" "[2]" "[3]" "[4]")
                   (mapcar (lambda (entry) (plist-get entry :marker-label)) items)))))

(ert-deftest arxana-browser-songs-import-demo-annotations-dry-run ()
  (should (= (length arxana-browser-songs-demo-annotations)
             (length (arxana-browser-songs-import-demo-annotations t)))))

;;; arxana-browser-songs-test.el ends here
