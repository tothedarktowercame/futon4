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
(let ((reazon-root (expand-file-name "dev/vendor/reazon" arxana-root-directory)))
  (when (file-directory-p reazon-root)
    (add-to-list 'load-path reazon-root)))

;; TODO(org-sync): Remove legacy arxana-allow-tangle toggle once no callers remain.
(defcustom arxana-allow-tangle nil
  "Legacy toggle; ignored in the current workflow."
  :type 'boolean
  :group 'arxana)

(defcustom arxana-load-dev-profile nil
  "When non-nil, log per-module load times for `arxana-load-dev`."
  :type 'boolean
  :group 'arxana)

(defcustom arxana-load-dev-profile-threshold 0.05
  "Minimum seconds to report module load timings."
  :type 'number
  :group 'arxana)

(defvar arxana--load-profile nil
  "Internal accumulator for `arxana-load-dev` profiling.")

(defvar arxana-load-dev-last-profile nil
  "Last profiling data captured by `arxana-load-dev`.")

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

(defun arxana--profile-load (label thunk)
  "Run THUNK and record elapsed time under LABEL."
  (let ((start (float-time)))
    (prog1 (funcall thunk)
      (push (cons label (- (float-time) start)) arxana--load-profile))))

(defun arxana--report-load-profile (total)
  "Report `arxana--load-profile` timings with TOTAL seconds."
  (setq arxana-load-dev-last-profile (nreverse arxana--load-profile))
  (when arxana-load-dev-profile
    (let* ((sorted (sort (copy-sequence arxana-load-dev-last-profile)
                         (lambda (a b) (> (cdr a) (cdr b)))))
           (slow (seq-filter (lambda (entry)
                               (>= (cdr entry) arxana-load-dev-profile-threshold))
                             sorted))
           (snippets (mapcar (lambda (entry)
                               (format "%s=%.3fs" (car entry) (cdr entry)))
                             slow)))
      (message "Arxana load profile: total %.3fs%s"
               total
               (if snippets
                   (concat " slow: " (string-join snippets ", "))
                 "")))))

(defun arxana-load-dev ()
  "Load the current `dev/` modules for interactive work."
  (arxana--ensure-version)
  (arxana--ensure-dependencies)
  (setq load-prefer-newer t)
  (setq arxana--load-profile nil)
  (let ((total-start (float-time)))
  (let ((books-root (expand-file-name "docs/docbook" arxana-root-directory)))
    (when (file-directory-p books-root)
      (setq arxana-docbook-books-root books-root)))
  ;; Prefer dev/staging implementations for interactive work during Phase 1.
  (dolist (dev-file '("dev/arxana-docbook.el"
                      "dev/arxana-docbook-ui.el"
                      "dev/arxana-links.el"
                      "dev/arxana-org-links.el"
                      "dev/arxana-data-constraints.el"
                      "dev/arxana-window-constraints.el"
                      "dev/arxana-scholium.el"
                      "dev/arxana-lab.el"
                      "dev/arxana-patterns.el"))
    (let ((path (expand-file-name dev-file arxana-root-directory)))
      (when (file-readable-p path)
        (arxana--profile-load
         (format "load %s" dev-file)
         (lambda ()
           (load path nil t))))))
  (arxana--profile-load "require arxana-store" (lambda () (ignore-errors (require 'arxana-store))))
  (arxana--profile-load "require arxana-links" (lambda () (ignore-errors (require 'arxana-links))))
  (arxana--profile-load "require arxana-org-links" (lambda () (ignore-errors (require 'arxana-org-links))))
  (arxana--profile-load "require arxana-article" (lambda () (ignore-errors (require 'arxana-article))))
  (arxana--profile-load "require arxana-scholium" (lambda () (ignore-errors (require 'arxana-scholium))))
  (arxana--profile-load "require arxana-relations" (lambda () (ignore-errors (require 'arxana-relations))))
  (arxana--profile-load "require arxana-browser" (lambda () (ignore-errors (require 'arxana-browser))))
  (arxana--profile-load "require arxana-derivation" (lambda () (ignore-errors (require 'arxana-derivation))))
  (arxana--profile-load "require arxana-saving" (lambda () (ignore-errors (require 'arxana-saving))))
  (arxana--profile-load "require arxana-inclusion" (lambda () (ignore-errors (require 'arxana-inclusion))))
  (arxana--profile-load "require arxana-import" (lambda () (ignore-errors (require 'arxana-import))))
  (arxana--profile-load "require arxana-articles-export" (lambda () (ignore-errors (require 'arxana-articles-export))))
  (arxana--profile-load "require arxana-compat" (lambda () (ignore-errors (require 'arxana-compat))))
  (arxana--profile-load "require arxana-xtdb-browse" (lambda () (ignore-errors (require 'arxana-xtdb-browse))))
  (arxana--profile-load "require arxana-patterns" (lambda () (ignore-errors (require 'arxana-patterns))))
  (arxana--profile-load "require arxana-docbook" (lambda () (ignore-errors (require 'arxana-docbook))))
  (arxana--profile-load "require arxana-docbook-ui" (lambda () (ignore-errors (require 'arxana-docbook-ui))))
  (arxana--profile-load "require arxana-lab" (lambda () (ignore-errors (require 'arxana-lab))))
  (arxana--report-load-profile (- (float-time) total-start)))
  (message "Loaded dev/ modules")
  t)

(defun arxana-load (&optional _unused)
  "Load the current `dev/` modules."
  (interactive "P")
  (when arxana-allow-tangle
    (message "arxana-allow-tangle is set, but this toggle is ignored"))
  (arxana-load-dev))

(defun arxana-reload-harder ()
  "Unload and reload dev modules to ensure updated definitions take effect."
  (interactive)
  (let* ((preserved (arxana--snapshot-variable-values arxana-reload-preserved-variables))
         (dev-files '("dev/arxana-docbook.el"
                      "dev/arxana-docbook-ui.el"
                      "dev/arxana-docbook-checkout.el"
                      "dev/arxana-docbook-remote.el"
                      "dev/arxana-lab.el"
                      "dev/arxana-patterns.el"
                      "dev/arxana-store.el"
                      "dev/arxana-links.el"
                      "dev/arxana-org-links.el"
                      "dev/arxana-article.el"
                      "dev/arxana-scholium.el"
                      "dev/arxana-relations.el"
                      "dev/arxana-browser.el"
                      "dev/arxana-derivation.el"
                      "dev/arxana-saving.el"
                      "dev/arxana-inclusion.el"
                      "dev/arxana-import.el"
                      "dev/arxana-articles-export.el"
                      "dev/arxana-compat.el"
                      "dev/arxana-xtdb-browse.el"
                      "dev/arxana-media.el"
                      "dev/arxana-browser-evidence.el"
                      "dev/arxana-browser-core.el"))
         (features '(arxana-docbook arxana-docbook-ui arxana-docbook-checkout
                     arxana-docbook-remote arxana-lab arxana-patterns
                     arxana-store arxana-links arxana-article arxana-scholium
                     arxana-org-links arxana-relations arxana-browser arxana-derivation
                     arxana-saving arxana-inclusion arxana-import
                     arxana-articles-export arxana-compat arxana-xtdb-browse
                     arxana-media arxana-browser-evidence arxana-browser-core)))
    (dolist (feat features)
      (when (featurep feat)
        (ignore-errors (unload-feature feat t))))
    (dolist (dev-file dev-files)
      (let ((path (expand-file-name dev-file arxana-root-directory)))
        (when (file-readable-p path)
          (load-file path))))
    (arxana-load-dev)
    (arxana--restore-variable-values preserved)
    (message "Reloaded dev/ modules (hard, preserved runtime settings)")))

(defvar arxana--hot-reload-pending-render nil)
(defvar arxana--hot-reload-pending-load nil)
(defcustom arxana-hot-reload-defer-dev t
  "When non-nil, defer `arxana-load-dev` until hot reload batch completion."
  :type 'boolean
  :group 'arxana)

(defun arxana--render-browsers ()
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'arxana-browser-mode)
        (ignore-errors (arxana-browser--render))))))

(defun arxana--apply-pending-load ()
  (when arxana--hot-reload-pending-load
    (setq arxana--hot-reload-pending-load nil)
    (arxana-load-dev)))

(defun arxana-reload-after-hot-reload (&optional file)
  "Refresh Arxana modules after a hot reload of FILE.
Designed for integration with futon0/contrib hot reload hooks."
  (let* ((root (and (boundp 'arxana-root-directory) arxana-root-directory))
         (root* (and root (file-truename root)))
         (file* (and file (file-truename file)))
         (under-root (and root* file* (string-prefix-p root* file*))))
    (when (or (null file) under-root)
      (if (and arxana-hot-reload-defer-dev
               (boundp 'my-chatgpt-shell--hot-reload-batch-p)
               my-chatgpt-shell--hot-reload-batch-p)
          (setq arxana--hot-reload-pending-load t)
        (arxana-load-dev))
      (if (and (boundp 'my-chatgpt-shell--hot-reload-batch-p)
               my-chatgpt-shell--hot-reload-batch-p)
          (setq arxana--hot-reload-pending-render t)
        (arxana--render-browsers))
      (message "Arxana hot reload applied."))))

(defun arxana-reload-after-hot-reload-batch ()
  "Refresh Arxana browser buffers after a batch hot reload."
  (arxana--apply-pending-load)
  (when arxana--hot-reload-pending-render
    (setq arxana--hot-reload-pending-render nil)
    (arxana--render-browsers)))

(defun arxana-batch-load ()
  "Batch entry point: load `dev/` sources, then exit."
  (arxana-load))

(defalias 'arxana-build #'arxana-load)
(defalias 'arxana-batch-build #'arxana-batch-load)

(defconst arxana-reload-preserved-variables
  '(futon4-enable-sync
    futon4-base-url
    arxana-store-default-penholder
    arxana-store-default-profile
    arxana-media-index-path)
  "Runtime variables preserved across `arxana-reload-harder`.")

(defun arxana--snapshot-variable-values (variables)
  "Return VALUES for currently bound VARIABLES."
  (let (snapshot)
    (dolist (var variables (nreverse snapshot))
      (when (boundp var)
        (push (cons var (symbol-value var)) snapshot)))))

(defun arxana--restore-variable-values (snapshot)
  "Restore variable values from SNAPSHOT."
  (dolist (entry snapshot)
    (set (car entry) (cdr entry))))

(provide 'arxana-bootstrap)

;;; bootstrap.el ends here
