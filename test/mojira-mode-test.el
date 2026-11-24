;;; mojira-mode-test.el --- ERT tests for mojira-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests run against a small fixture glossary JSON, not the real file.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mojira-mode)

(defconst mojira-test-fixture-json
  "{\"entities\": [
     {\"slug\": \"war-machine\", \"name\": \"War Machine\",
      \"forms\": [\"War Machine\", \"WM\"],
      \"what\": \"The agency's execution engine. It reads turns.\",
      \"where\": \"futon2/src/futon2/wm.clj\", \"grounded\": \"grounded\",
      \"store-attested\": true, \"producers\": [\"joe\"], \"relations\": \"top\",
      \"status\": \"active\", \"grounding-probes\": []},
     {\"slug\": \"xiang\", \"name\": \"Elephant xiang\",
      \"forms\": [\"xiang\"],
      \"what\": \"A reader agent. It never sleeps.\",
      \"where\": \"notes/xiang.md\", \"grounded\": \"notional\",
      \"store-attested\": false, \"producers\": [\"joe\"], \"relations\": \"\",
      \"status\": \"active\", \"grounding-probes\": []},
     {\"slug\": \"magnet-a\", \"name\": \"Magnet A\",
      \"forms\": [\"magnet\"], \"what\": \"A.\", \"where\": \"\",
      \"grounded\": \"no-where\", \"store-attested\": true,
      \"producers\": [], \"relations\": \"\", \"status\": \"\", \"grounding-probes\": []},
     {\"slug\": \"magnet-b\", \"name\": \"Magnet B\",
      \"forms\": [\"magnet\"], \"what\": \"B.\", \"where\": \"\",
      \"grounded\": \"no-where\", \"store-attested\": true,
      \"producers\": [], \"relations\": \"\", \"status\": \"\", \"grounding-probes\": []}],
   \"forms\": {\"war\": \"war-machine\", \"war machine\": \"war-machine\",
              \"wm\": \"war-machine\", \"xiang\": \"xiang\", \"magnet\": null},
   \"form-conflicts\": [{\"form\": \"magnet\", \"slugs\": [\"magnet-a\", \"magnet-b\"]}],
   \"possible-duplicates\": [{\"form\": \"magnet\", \"slugs\": [\"magnet-a\", \"magnet-b\"]}],
   \"vocab-comparison\": {}, \"sessionista-introductions\": []}")

(defun mojira-test--with-buffer (text mode-fn)
  "Run MODE-FN in a fresh buffer containing TEXT; return the buffer."
  (let ((file (make-temp-file "mojira-fixture" nil ".json"))
        (buf (generate-new-buffer " *mojira-test*")))
    (with-temp-file file (insert mojira-test-fixture-json))
    (setq mojira--state nil)
    (with-current-buffer buf
      (insert text)
      (goto-char (point-min))
      (let ((mojira-glossary-file file))
        (set (make-local-variable 'mojira-glossary-file) file)
        (funcall mode-fn)
        (font-lock-mode 1)
        (font-lock-ensure)))
    buf))

(defun mojira-test--faces-at (buffer needle offset)
  "Face list of the char at NEEDLE+OFFSET in BUFFER, or nil."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let* ((i (search-forward needle nil t))
           (f (and i (get-text-property (- i offset) 'face))))
      (and f (if (listp f) f (list f))))))

(defun mojira-test--help-echo-at (buffer needle offset)
  "Help-echo of the char at NEEDLE+OFFSET in BUFFER, or nil."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((i (search-forward needle nil t)))
      (and i (get-text-property (- i offset) 'help-echo)))))

(defun mojira-test--setup (text)
  "Fontified fresh buffer with TEXT and the fixture glossary."
  (mojira-test--with-buffer
   text
   (lambda () (mojira-mode 1))))

(ert-deftest mojira-highlight-wm-not-wmc ()
  (let ((buf (mojira-test--setup "the WM ran and WMC too")))
    (unwind-protect
        (progn
          (should (memq 'mojira-entity
                        (mojira-test--faces-at buf "WM" 1)))
          (should-not (memq 'mojira-entity
                            (mojira-test--faces-at buf "WMC" 2))))
      (kill-buffer buf))))

(ert-deftest mojira-highlight-cjk ()
  ;; Uses a CJK fixture variant: 象 must highlight as itself.
  (let ((file (make-temp-file "mojira-cjk" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"entities\":[{\"slug\":\"xiang\",\"name\":\"Elephant\","
                    "\"forms\":[\"象\"],\"what\":\"A reader.\",\"where\":\"x.md\","
                    "\"grounded\":\"notional\",\"store-attested\":false,"
                    "\"producers\":[],\"relations\":\"\",\"status\":\"\","
                    "\"grounding-probes\":[]}],"
                    "\"forms\":{\"象\":\"xiang\"},\"form-conflicts\":[],"
                    "\"possible-duplicates\":[],"
                    "\"vocab-comparison\":{},\"sessionista-introductions\":[]}"))
          (setq mojira--state nil)
          (with-temp-buffer
            (insert "象 reads turns")
            (goto-char (point-min))
            (let ((mojira-glossary-file file))
              (set (make-local-variable 'mojira-glossary-file) file)
              (mojira-mode 1)
              (font-lock-mode 1)
              (font-lock-ensure)
              (should (memq 'mojira-entity
                            (get-text-property 1 'face)))))
          (setq mojira--state nil))
      (setq mojira--state nil))))

(ert-deftest mojira-longest-form-wins ()
  ;; "Machine" alone is not a form: it can only be highlighted if the
  ;; longer form "War Machine" matched as a whole.
  (let ((buf (mojira-test--setup "the War Machine hums")))
    (unwind-protect
        (progn
          (should (memq 'mojira-entity
                        (mojira-test--faces-at buf "Machine" 1)))
          (should (memq 'mojira-entity
                        (mojira-test--faces-at buf "War" 1))))
      (kill-buffer buf))))

(ert-deftest mojira-ambiguous-face ()
  (let ((buf (mojira-test--setup "the magnet hums")))
    (unwind-protect
        (let ((echo (mojira-test--help-echo-at buf "magnet" 1)))
          (should (memq 'mojira-entity-ambiguous
                        (mojira-test--faces-at buf "magnet" 1)))
          (should (and echo (string-match "magnet-a" echo)))
          (should (and echo (string-match "magnet-b" echo))))
      (kill-buffer buf))))

(ert-deftest mojira-help-echo-file-content-only ()
  (let ((buf (mojira-test--setup "xiang reads turns")))
    (unwind-protect
        (let ((echo (mojira-test--help-echo-at buf "xiang" 1)))
          (should (and echo (string-match "file content only" echo)))
          (should (and echo (string-match "xiang" echo))))
      (kill-buffer buf))))

(ert-deftest mojira-help-echo-stored-no-flag ()
  (let ((buf (mojira-test--setup "the WM ran")))
    (unwind-protect
        (let ((echo (mojira-test--help-echo-at buf "WM" 1)))
          (should (and echo (string-match "War Machine" echo)))
          (should (not (and echo (string-match "file content only" echo)))))
      (kill-buffer buf))))

(ert-deftest mojira-case-insensitive ()
  (let ((buf (mojira-test--setup "the Wm ran and the wM too")))
    (unwind-protect
        (progn
          (should (memq 'mojira-entity
                        (mojira-test--faces-at buf "Wm" 1)))
          (should (memq 'mojira-entity
                        (mojira-test--faces-at buf "wM" 1))))
      (kill-buffer buf))))

(ert-deftest mojira-describe-buffer ()
  (let ((buf (mojira-test--setup "the magnet hums")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (search-forward "magnet")
          (goto-char (match-beginning 0))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (b &optional _a _s) b)))
            (mojira-describe-entity-at-point))
          (let ((help (get-buffer "*Mojira*")))
            (should help)
            (with-current-buffer help
              (should (string-match "magnet-a" (buffer-string)))
              (should (string-match "magnet-b" (buffer-string)))
              (should (string-match "possible duplicates" (buffer-string))))
            (kill-buffer help)))
      (kill-buffer buf))))

;;; mojira-mode-test.el ends here
