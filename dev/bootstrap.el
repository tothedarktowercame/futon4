;;; bootstrap.el --- Build / load harness for Arxana  -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides entry points for loading the Arxana sources from `dev/`.
;; The current workflow loads the dev modules directly.

;;; Code:

(defconst arxana-minimum-emacs-version "28.1"
  "Smallest supported Emacs version for the revived Arxana client.")

(defconst arxana--bootstrap-file (or load-file-name (buffer-file-name))
  "Absolute path to this bootstrap file when loaded from disk.")

(defconst arxana-root-directory
  (let* ((base (cond
                (arxana--bootstrap-file
                 (expand-file-name ".."
                                   (file-name-directory arxana--bootstrap-file)))
                ((bound-and-true-p default-directory)
                 (expand-file-name "arxana" default-directory))
                (t default-directory))))
    (file-name-as-directory base))
  "Root directory containing the Arxana repository.")

(add-to-list 'load-path (expand-file-name "dev" arxana-root-directory))

;; TODO(org-sync): Remove legacy arxana-allow-tangle toggle once no callers remain.
(defcustom arxana-allow-tangle nil
  "Legacy toggle; ignored in the current workflow."
  :type 'boolean
  :group 'arxana)

(defun arxana--ensure-version ()
  "Throw an error unless the running Emacs meets the minimum version."
  (when (version< emacs-version arxana-minimum-emacs-version)
    (error "Arxana requires Emacs %s or newer (current: %s)"
           arxana-minimum-emacs-version emacs-version))
  t)

(defun arxana--ensure-dependencies ()
  "Require runtime packages needed for Org parsing and networking."
  (dolist (feature '(org org-macs json url url-http cl-lib seq))
    (require feature))
  ;; Align is nice-to-have; tolerate absence (e.g., minimal builds).
  (ignore-errors (require 'align))
  t)

(defun arxana-load-dev ()
  "Load the current `dev/` modules for interactive work."
  (arxana--ensure-version)
  (arxana--ensure-dependencies)
  ;; Prefer dev/staging implementations for interactive work during Phase 1.
  (dolist (dev-file '("dev/arxana-docbook.el"
                      "dev/arxana-lab.el"
                      "dev/arxana-patterns.el"))
    (let ((path (expand-file-name dev-file arxana-root-directory)))
      (when (file-readable-p path)
        (load path nil t))))
  (ignore-errors (require 'arxana-store))
  (ignore-errors (require 'arxana-article))
  (ignore-errors (require 'arxana-scholium))
  (ignore-errors (require 'arxana-relations))
  (ignore-errors (require 'arxana-browser))
  (ignore-errors (require 'arxana-derivation))
  (ignore-errors (require 'arxana-saving))
  (ignore-errors (require 'arxana-inclusion))
  (ignore-errors (require 'arxana-import))
  (ignore-errors (require 'arxana-export))
  (ignore-errors (require 'arxana-compat))
  (ignore-errors (require 'arxana-xtdb-browse))
  (ignore-errors (require 'arxana-patterns))
  (ignore-errors (require 'arxana-docbook))
  (ignore-errors (require 'arxana-lab))
  (message "Loaded dev/ modules")
  t)

(defun arxana-load (&optional _unused)
  "Load the current `dev/` modules."
  (interactive "P")
  (when arxana-allow-tangle
    (message "arxana-allow-tangle is set, but this toggle is ignored"))
  (arxana-load-dev))

(defun arxana-batch-load ()
  "Batch entry point: load `dev/` sources, then exit."
  (arxana-load))

(defalias 'arxana-build #'arxana-load)
(defalias 'arxana-batch-build #'arxana-batch-load)

(provide 'arxana-bootstrap)

;;; bootstrap.el ends here
