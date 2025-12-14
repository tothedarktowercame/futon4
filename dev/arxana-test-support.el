;;; arxana-test-support.el --- Shared helpers for ERT suites -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides utilities to locate/load the tangled Arxana sources inside
;; the dev/test harness, regardless of whether we're running from the
;; repo root or from within the arxana/ subtree.

;;; Code:

(require 'cl-lib)

(defun arxana-test--locate-tangled ()
  "Return the best-effort path to `arxana-tangled.el'."
  (let* ((root (or (and (boundp 'arxana-root-directory)
                        arxana-root-directory)
                   default-directory))
         (candidates (list (expand-file-name "arxana/arxana-tangled.el" root)
                           (expand-file-name "arxana-tangled.el" root))))
    (cl-loop for path in candidates
             when (file-exists-p path)
             return path)))

(defun arxana-test--ensure-tangled-loaded ()
  "Load the tangled sources if they are not already resident."
  (unless (featurep 'arxana-tangled)
    (let ((tangled (arxana-test--locate-tangled)))
      (unless tangled
        (error "Cannot find arxana-tangled.el; run arxana-build first"))
      (load-file tangled))))

(provide 'arxana-test-support)

;;; arxana-test-support.el ends here
