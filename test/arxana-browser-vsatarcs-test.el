;;; arxana-browser-vsatarcs-test.el --- Tests for VSATARCS reader -*- lexical-binding: t; -*-

(setq load-prefer-newer t)

(require 'ert)
(require 'cl-lib)
(require 'arxana-browser-core)
(require 'arxana-browser-vsatarcs)

(defconst arxana-vsatarcs-test--story
  "# Demo Story

**Leaf:** test

---

## Scene: Overview | overview

*(opening scene)*

Start here.

[Go next](next-scene)

## Scene: Next Scene | next-scene

Continue here.

[Back](overview)
")

(ert-deftest arxana-vsatarcs-parse-string-finds-scenes ()
  (let* ((story (arxana-vsatarcs-parse-string arxana-vsatarcs-test--story))
         (scenes (plist-get story :scenes))
         (opening (arxana-vsatarcs--opening-scene story)))
    (should (equal (plist-get story :title) "Demo Story"))
    (should (= (length scenes) 2))
    (should (equal (plist-get opening :anchor) "overview"))
    (should (plist-get opening :opening))
    (should (equal (plist-get (cadr scenes) :title) "Next Scene"))))

(ert-deftest arxana-vsatarcs-parse-string-extracts-links ()
  (let* ((story (arxana-vsatarcs-parse-string arxana-vsatarcs-test--story))
         (scene (arxana-vsatarcs--scene-by-anchor story "overview"))
         (links (plist-get scene :links)))
    (should (= (length links) 1))
    (should (equal (plist-get (car links) :text) "Go next"))
    (should (equal (plist-get (car links) :target) "next-scene"))))

(ert-deftest arxana-vsatarcs-parse-string-normalizes-folded-links ()
  (let* ((story (arxana-vsatarcs-parse-string
                 "# Folded

## Scene: One | one

[Folded target](next-
scene)

## Scene: Next Scene | next-scene

Done.
"))
         (scene (arxana-vsatarcs--scene-by-anchor story "one"))
         (link (car (plist-get scene :links))))
    (should (equal (plist-get link :target) "next-scene"))
    (should (string-match-p "(next-scene)" (plist-get scene :body)))))

(ert-deftest arxana-vsatarcs-render-button-navigation ()
  (let ((story (arxana-vsatarcs-parse-string arxana-vsatarcs-test--story))
        (buffer-name arxana-vsatarcs--buffer))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
            (arxana-vsatarcs-render story "overview"))
          (with-current-buffer buffer-name
            (should (search-forward "Scene 1/2: Overview" nil t))
            (arxana-vsatarcs-goto "next-scene")
            (goto-char (point-min))
            (should (search-forward "Scene 2/2: Next Scene" nil t))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name)))))

(ert-deftest arxana-browser-vsatarcs-mode-map-binds-left-return ()
  (should (eq #'arxana-vsatarcs-left-or-return
              (lookup-key arxana-vsatarcs-mode-map (kbd "<left>")))))

(ert-deftest arxana-browser-vsatarcs-left-at-point-min-returns-to-browser ()
  (let ((browser-buf (get-buffer-create "*Arxana Browser*"))
        (vsatarcs-buf arxana-vsatarcs--buffer)
        (directory (make-temp-file "arxana-vsatarcs-" t)))
    (unwind-protect
        (save-window-excursion
          (let ((path (expand-file-name "demo.md" directory)))
            (with-temp-file path
              (insert arxana-vsatarcs-test--story))
            (switch-to-buffer browser-buf)
            (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
              (arxana-browser-vsatarcs-open (list :path path)))
            (with-current-buffer vsatarcs-buf
              (should (eq browser-buf arxana-ui-return-buffer))
              (should (window-configuration-p arxana-ui-return-window-config)))
            (switch-to-buffer vsatarcs-buf)
            (goto-char (point-min))
            (arxana-vsatarcs-left-or-return)
            (should (eq browser-buf (window-buffer (selected-window))))))
      (when (get-buffer browser-buf)
        (kill-buffer browser-buf))
      (when (get-buffer vsatarcs-buf)
        (kill-buffer vsatarcs-buf))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest arxana-browser-core-menu-includes-vsatarcs ()
  (let ((labels (mapcar (lambda (item) (plist-get item :label))
                        (arxana-browser--menu-items))))
    (should (member "VSATARCS" labels))))

(ert-deftest arxana-browser-vsatarcs-items-read-directory ()
  (let* ((directory (make-temp-file "arxana-vsatarcs-" t))
         (path (expand-file-name "demo.md" directory))
         (arxana-vsatarcs-story-directories (list directory)))
    (unwind-protect
        (progn
          (with-temp-file path
            (insert arxana-vsatarcs-test--story))
          (let ((item (car (arxana-browser-vsatarcs-items))))
            (should (eq (plist-get item :type) 'vsatarcs-story))
            (should (equal (plist-get item :label) "Demo Story"))
            (should (= (plist-get item :scene-count) 2))
            (should (equal (plist-get item :opening) "Overview"))))
      (ignore-errors (delete-directory directory t)))))

(ert-deftest arxana-vsatarcs-render-shows-belief-snapshot-empty ()
  "Reader chrome surfaces an empty-belief notice when no entities are tracked."
  (arxana-vsatarcs-belief-reset)
  (let ((story (arxana-vsatarcs-parse-string arxana-vsatarcs-test--story))
        (buffer-name arxana-vsatarcs--buffer))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
            (arxana-vsatarcs-render story "overview"))
          (with-current-buffer buffer-name
            (goto-char (point-min))
            (should (search-forward "Belief snapshot" nil t))
            (goto-char (point-min))
            (should (search-forward "no entities tracked yet" nil t))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (arxana-vsatarcs-belief-reset))))

(ert-deftest arxana-vsatarcs-render-shows-belief-snapshot-populated ()
  "Reader chrome lists tracked entity ids + most-likely status."
  (arxana-vsatarcs-belief-reset)
  (arxana-vsatarcs-belief-ingest-events
   '((:entity-id "arxana/stack/futon-v1/leaf/2" :type :strengthened :weight 3.0)))
  (let ((story (arxana-vsatarcs-parse-string arxana-vsatarcs-test--story))
        (buffer-name arxana-vsatarcs--buffer))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'display-buffer) (lambda (&rest _) nil)))
            (arxana-vsatarcs-render story "overview"))
          (with-current-buffer buffer-name
            (goto-char (point-min))
            (should (search-forward "Belief snapshot" nil t))
            (goto-char (point-min))
            (should (search-forward "arxana/stack/futon-v1/leaf/2" nil t))
            (goto-char (point-min))
            (should (search-forward "strengthened" nil t))))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (arxana-vsatarcs-belief-reset))))

(provide 'arxana-browser-vsatarcs-test)
;;; arxana-browser-vsatarcs-test.el ends here
