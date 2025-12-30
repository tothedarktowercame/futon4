;;; arxana-test-support.el --- Shared helpers for ERT suites -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides utilities to locate/load the legacy combined Arxana bundle inside
;; the dev/test harness, regardless of whether we're running from the repo
;; root or a subdirectory.

;;; Code:

(require 'cl-lib)
(require 'arxana-saving nil t)

(defun arxana-test--locate-tangled ()
  "Return the best-effort path to the legacy `arxana-tangled.el' bundle."
  (let* ((root (or (and (boundp 'arxana-root-directory)
                        arxana-root-directory)
                   default-directory))
         (candidates (list (expand-file-name "arxana-tangled.el" root))))
    (cl-loop for path in candidates
             when (file-exists-p path)
             return path)))

(defun arxana-test--ensure-tangled-loaded ()
  "Load the legacy bundle if it is not already resident."
  (unless (featurep 'arxana-tangled)
    (let ((tangled (arxana-test--locate-tangled)))
      (if tangled
          (load-file tangled)
        (message "arxana-tangled.el not found; continuing with dev modules")))))

(provide 'arxana-test-support)

;;; arxana-test-support.el ends here
