;;; arxana-import.el --- Org → XTDB import helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Utility functions that read Org files/directories and push their
;; contents into Arxana's article graph (and, via the existing store
;; hooks, into XTDB).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function scholium "arxana-tangled" (name text &optional about type book))
(declare-function sch-book "arxana-tangled" () nil)
(declare-function arxana-store-ensure-article "arxana-store" (&rest _))
(declare-function futon4--canonical-path "arxana-tangled" (path))

(defun arxana-import--read-file-contents (path)
  "Return the contents of PATH as a string."
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun arxana-import--org-title-from-string (text)
  "Extract a #+TITLE from TEXT when present."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((case-fold-search t))
      (when (re-search-forward "^#\\+TITLE:[[:space:]]*" nil t)
        (let ((start (point)))
          (end-of-line)
          (string-trim (buffer-substring-no-properties start (point))))))))

(defun arxana-import--derive-name (path text)
  "Pick a reasonable article name from PATH and TEXT."
  (or (arxana-import--org-title-from-string text)
      (file-name-base path)
      (file-name-nondirectory path)))

(defun arxana-import--canonical-path (path)
  (if (fboundp 'futon4--canonical-path)
      (futon4--canonical-path path)
    path))

(defun arxana-import-org-file (path)
  "Import the Org file at PATH into the Arxana article table."
  (interactive "fOrg file: ")
  (let* ((text (arxana-import--read-file-contents path))
         (name (arxana-import--derive-name path text)))
    (when (string-empty-p (or name ""))
      (error "Could not derive article name from %s" path))
    (scholium name text nil '(org) (sch-book))
    (when (fboundp 'arxana-store-ensure-article)
      (arxana-store-ensure-article :name name :path (arxana-import--canonical-path path)))
    (message "Imported %s" name)
    name))

(defun arxana-import--org-files-recursively (directory)
  "Return Org files under DIRECTORY recursively."
  (let ((results nil))
    (dolist (entry (directory-files directory t directory-files-no-dot-files-regexp))
      (cond
       ((file-directory-p entry)
        (setq results (nconc results (arxana-import--org-files-recursively entry))))
       ((and (file-regular-p entry)
             (string-match-p "\\.org\\'" entry))
        (push entry results))))
    results))

(defun arxana-import--org-files-in-directory (directory recursive)
  "Return Org files under DIRECTORY; RECURSIVE when non-nil."
  (let ((dir (file-name-as-directory (expand-file-name directory))))
    (if recursive
        (arxana-import--org-files-recursively dir)
      (directory-files dir t "\\.org\\'" t))))

(defun arxana-import-org-directory (directory &optional recursive)
  "Import all Org files under DIRECTORY.  With RECURSIVE, walk subdirs."
  (interactive "DImport Org directory: \nP")
  (let* ((files (arxana-import--org-files-in-directory directory recursive))
         (count 0))
    (dolist (file files)
      (arxana-import-org-file file)
      (setq count (1+ count)))
    (message "Imported %d Org file%s" count (if (= count 1) "" "s"))
    count))

(provide 'arxana-import)

;;; arxana-import.el ends here
