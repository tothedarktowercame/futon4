;;; arxana-apm.el --- APM Prelim Tutor porcelain for Arxana  -*- lexical-binding: t; -*-

;; Provides M-x apm-cheatsheet to project an APM problem's Arxana graph
;; to a two-column LaTeX cheatsheet and compile it.

;;; Code:

(defvar apm-script-dir
  (expand-file-name "~/code/storage/apm/")
  "Directory containing APM scripts.")

(defvar apm-output-dir
  (expand-file-name "~/code/storage/canary/")
  "Directory for cheatsheet output.")

(defvar apm-problem-ids nil
  "Cached list of APM problem IDs (loaded from manifest).")

(defun apm--load-problem-ids ()
  "Load problem IDs from manifest.edn (cached)."
  (or apm-problem-ids
      (setq apm-problem-ids
            (with-temp-buffer
              (insert-file-contents
               (expand-file-name "manifest.edn" apm-script-dir))
              (let (ids)
                (goto-char (point-min))
                (while (re-search-forward ":id \"\\([^\"]+\\)\"" nil t)
                  (push (concat "apm-" (match-string 1)) ids))
                (nreverse ids))))))

(defun apm--current-problem-id ()
  "Try to detect current APM problem ID from context.
Checks buffer name, then ego view, then prompts."
  (or (when (string-match "apm-[a-z][0-9]" (buffer-name))
        (match-string 0 (buffer-name)))
      (when (and (boundp 'arxana-ego--current-name)
                 (string-match "^apm-" (or arxana-ego--current-name "")))
        arxana-ego--current-name)
      nil))

;;;###autoload
(defun apm-cheatsheet (problem-id)
  "Project an APM problem's Arxana graph to a two-column LaTeX cheatsheet.

Reads entities and relations from XTDB, assembles exposition + discipline
annotations with ArSE and PlanetMath cross-references, compiles to PDF."
  (interactive
   (list (completing-read "APM problem: "
                          (apm--load-problem-ids)
                          nil nil
                          (apm--current-problem-id))))
  (let* ((base (replace-regexp-in-string "^apm-" "" problem-id))
         (tex-file (expand-file-name (concat base ".tex") apm-output-dir))
         (pdf-file (expand-file-name (concat base ".pdf") apm-output-dir))
         (script (expand-file-name "project-cheatsheet.py" apm-script-dir))
         (default-directory apm-output-dir))
    (message "Projecting %s from Arxana..." problem-id)
    ;; Generate TeX
    (let ((result (shell-command-to-string
                   (format "python3 %s %s 2>&1" script problem-id))))
      (message "%s" result))
    ;; Compile
    (if (file-exists-p tex-file)
        (progn
          (message "Compiling %s..." tex-file)
          (shell-command
           (format "cd %s && lualatex -interaction=nonstopmode %s"
                   (shell-quote-argument apm-output-dir)
                   (shell-quote-argument (file-name-nondirectory tex-file))))
          (if (file-exists-p pdf-file)
              (progn
                (message "Opening %s" pdf-file)
                (find-file pdf-file))
            (message "Compilation failed — check %s"
                     (expand-file-name (concat base ".log") apm-output-dir))))
      (message "TeX generation failed for %s" problem-id))))

;;;###autoload
(defun apm-browse (problem-id)
  "Browse an APM problem in Arxana's ego view."
  (interactive
   (list (completing-read "APM problem: "
                          (apm--load-problem-ids)
                          nil nil
                          (apm--current-problem-id))))
  (arxana-store-ego problem-id))

;;;###autoload
(defun apm-cheatsheet-from-ego ()
  "Generate cheatsheet for the problem currently shown in ego view."
  (interactive)
  (let ((id (apm--current-problem-id)))
    (if id
        (apm-cheatsheet id)
      (call-interactively #'apm-cheatsheet))))

(provide 'arxana-apm)

;;; arxana-apm.el ends here
