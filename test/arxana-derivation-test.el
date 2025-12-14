;;; arxana-derivation-test.el --- Tests for derivation previews -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'arxana-derivation)

(unless (fboundp 'scholium-name)
  (defun scholium-name (sch) (car sch)))

(unless (fboundp 'scholium-about)
  (defun scholium-about (sch) (nth 2 sch)))

(unless (fboundp 'scholium-type)
  (defun scholium-type (sch) (nth 3 sch)))

(cl-defun arxana-derivation-test--scholium (&key name about type)
  "Return a minimal scholium struct for preview tests."
  (list name nil about type nil))

(ert-deftest arxana-derivation-build-items-detects-transclusion ()
  (let ((buffer (get-buffer-create " *arxana-preview*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (insert "Included text\nMore text")
            (put-text-property 1 14 'scholia '((transclusion-of "Source"))))
          (let* ((sch (arxana-derivation-test--scholium
                       :name '(derives-from "Target" "Source" 1)
                       :about '((passage ("Target" 1 14)))
                       :type 'derives-from))
                 (ids '(("Source" . "id-src")
                        ("Target" . "id-target"))))
            (cl-letf (((symbol-function 'futon4-lookup-article-id)
                       (lambda (name)
                         (cdr (assoc name ids)))))
              (let ((items (arxana-derivation--build-items (list sch)
                                                           buffer
                                                           "Target")))
                (should (= (length items) 1))
                (let ((item (car items)))
                  (should (equal (plist-get item :source) "Source"))
                  (should (equal (plist-get item :futon-source) "id-src"))
                  (should (eq (plist-get item :kind) :transclusion))
                  (should (string-match-p "Included" (plist-get item :snippet))))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest arxana-derivation-rendering-adds-toggle-buttons ()
  (let ((arxana-derivation-preview-state (make-hash-table :test 'equal)))
    (with-temp-buffer
      (setq-local arxana-derivation-preview-items
                  (list (list :key "Target::demo"
                              :source "Source"
                              :length 5
                              :kind :inclusion
                              :snippet "Preview text"
                              :futon-source "id-src")))
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (arxana-derivation--render-previews))
      (goto-char (point-min))
      (should (search-forward "Derivation previews" nil t))
      (should (search-forward "[+]" nil t))
      (arxana-derivation-preview-toggle "Target::demo")
      (goto-char (point-min))
      (should (search-forward "[-]" nil t))
      (should (search-forward "Preview text" nil t)))))

(ert-deftest arxana-derivation-highlights-install-overlays ()
  (with-temp-buffer
    (insert "Included text")
    (setq-local arxana-derivation-preview-items
                (list (list :kind :inclusion :begin 1 :end 8)))
    (let ((arxana-derivation-highlight-inclusions t)
          (arxana-derivation-highlight-transclusions nil)
          (arxana-derivation-highlight-identifications nil))
      (arxana-derivation--apply-highlights (current-buffer))
      (should (= (length arxana-derivation-highlight-overlays) 1))
      (let ((ov (car arxana-derivation-highlight-overlays)))
        (should (eq (overlay-get ov 'face) 'arxana-derivation-inclusion-face))
        (should (= (overlay-start ov) 1))
        (should (= (overlay-end ov) 8))))))

(ert-deftest arxana-derivation-toggle-refreshes-highlights ()
  (with-temp-buffer
    (insert "Included transclusion")
    (setq-local arxana-derivation-preview-items
                (list (list :kind :inclusion :begin 1 :end 9)))
    (let ((arxana-derivation-highlight-inclusions t)
          (arxana-derivation-highlight-transclusions t)
          (arxana-derivation-highlight-identifications nil))
      (arxana-derivation--apply-highlights (current-buffer))
      (should (= (length arxana-derivation-highlight-overlays) 1))
      (arxana-derivation-toggle-inclusion-highlights)
      (should-not arxana-derivation-highlight-inclusions)
      (should (null arxana-derivation-highlight-overlays)))))

(ert-deftest arxana-derivation-highlight-only-commands ()
  (with-temp-buffer
    (insert "ABCDEFGHIJ")
    (setq-local arxana-derivation-preview-items
                (list (list :kind :inclusion :begin 1 :end 2)
                      (list :kind :transclusion :begin 3 :end 4)
                      (list :kind :identification :begin 5 :end 6)))
    (let ((arxana-derivation-highlight-inclusions t)
          (arxana-derivation-highlight-transclusions t)
          (arxana-derivation-highlight-identifications t))
      (arxana-derivation--apply-highlights (current-buffer))
      (should (= (length arxana-derivation-highlight-overlays) 3))
      (arxana-derivation-highlight-only-transclusions)
      (should-not arxana-derivation-highlight-inclusions)
      (should arxana-derivation-highlight-transclusions)
      (should-not arxana-derivation-highlight-identifications)
      (should (= (length arxana-derivation-highlight-overlays) 1))
      (let ((ov (car arxana-derivation-highlight-overlays)))
        (should (= (overlay-start ov) 3))
        (should (= (overlay-end ov) 4)))
      (arxana-derivation-highlight-only-inclusions)
      (should arxana-derivation-highlight-inclusions)
      (should-not arxana-derivation-highlight-transclusions)
      (should-not arxana-derivation-highlight-identifications)
      (should (= (length arxana-derivation-highlight-overlays) 1))
      (arxana-derivation-highlight-only-identifications)
      (should-not arxana-derivation-highlight-inclusions)
      (should-not arxana-derivation-highlight-transclusions)
      (should arxana-derivation-highlight-identifications)
      (should (= (length arxana-derivation-highlight-overlays) 1)))))

(provide 'arxana-derivation-test)
;;; arxana-derivation-test.el ends here
