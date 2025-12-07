;;; bootstrap.el --- Build / load harness for Arxana  -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides repeatable entry points for tangling and loading the literate
;; Arxana sources from `spine.org`.  Use `arxana-build` interactively or invoke
;; `arxana-batch-build` via `emacs --batch` for CI / scripting.

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
  "Root directory containing `spine.org` and the Org sources.")

(add-to-list 'load-path (expand-file-name "dev" arxana-root-directory))

(defun arxana--ensure-version ()
  "Throw an error unless the running Emacs meets the minimum version."
  (when (version< emacs-version arxana-minimum-emacs-version)
    (error "Arxana requires Emacs %s or newer (current: %s)"
           arxana-minimum-emacs-version emacs-version))
  t)

(defun arxana--ensure-dependencies ()
  "Require runtime packages needed for tangling and networking."
  (dolist (feature '(org ob-tangle ob-core ob-exp org-macs json url url-http cl-lib seq))
    (require feature))
  ;; Align is nice-to-have; tolerate absence (e.g., minimal builds).
  (ignore-errors (require 'align))
  t)

(defun arxana--spine-file ()
  "Return the absolute path to `spine.org` within the repo."
  (expand-file-name "spine.org" arxana-root-directory))

(defun arxana--tangle-output ()
  "Return the absolute path to `arxana-tangled.el`."
  (expand-file-name "arxana-tangled.el" arxana-root-directory))

(defun arxana-load-spine ()
  "Load `spine.org` so helper functions such as `arxana-tangle-spine-concat` exist."
  (let ((spine (arxana--spine-file)))
    (unless (file-exists-p spine)
      (error "Cannot find spine file at %s" spine))
    (arxana--ensure-dependencies)
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-load-file spine))))

(defun arxana-build (&optional force-tangle)
  "Ensure the tangled Elisp exists and is loaded.
With FORCE-TANGLE (or a \[universal-argument]), always regenerate the file."
  (interactive "P")
  (arxana--ensure-version)
  (arxana-load-spine)
  (let* ((spine (arxana--spine-file))
         (output (arxana--tangle-output))
         (sources (cons spine
                        (when (fboundp 'arxana-spine-files)
                          (arxana-spine-files spine))))
         (needs-tangle (or force-tangle
                           (not (file-exists-p output))
                           (seq-some (lambda (src)
                                       (or (not (file-exists-p src))
                                           (file-newer-than-file-p src output)))
                                     sources))))
    (when needs-tangle
      (message "Tangling Arxana sources from %s" spine)
      (arxana-tangle-spine-concat spine))
    (load-file output)
    (unless (featurep 'arxana-tangled)
      (provide 'arxana-tangled))
    (ignore-errors (require 'arxana-store))
    (ignore-errors (require 'arxana-article))
    (ignore-errors (require 'arxana-scholium))
    (ignore-errors (require 'arxana-relations))
    (ignore-errors (require 'arxana-browse))
    (ignore-errors (require 'arxana-derivation))
    (ignore-errors (require 'arxana-inclusion))
    (ignore-errors (require 'arxana-import))
    (ignore-errors (require 'arxana-export))
    (ignore-errors (require 'arxana-compat))
    (ignore-errors (require 'arxana-xtdb-browse))
    (ignore-errors (require 'arxana-patterns))
    (message "Loaded %s" output)
    output))

(defun arxana-batch-build ()
  "Batch entry point: regenerate and load the tangled sources, then exit."
  (let ((output (arxana-build t)))
    (message "Regenerated %s" output)))

(provide 'arxana-bootstrap)

;;; bootstrap.el ends here
