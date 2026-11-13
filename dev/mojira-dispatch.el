;;; mojira-dispatch.el --- ※: a "do anything" prompt that reads Mojira shortcodes  -*- lexical-binding: t; -*-

;; Joe, 2026-11-13: "※ should behave in Emacs like M-: except instead of saying \"Eval: \" it
;; should say \"※ \" and instead of accepting Lisp it should accept Mojira shortcodes."
;; Design note: futon4/holes/missions/M-mojira.md, "A governed ~/code makes keyed access
;; possible".  Mojira itself is a code-server extension; this is its Emacs entry point.
;;
;; Shortcodes (first draft):
;;   p                  the Arxana browser's Patterns view (arxana://view/patterns)
;;   RET (empty)        the Arxana browser home (what ※ used to do)
;;   <n> [<kind>]       a futonN repo and a kind of thing in it (0, 1b, 2, 3c, 4, 5, 6, 7, ...);
;;   r <name> [<kind>]  any other repo by name.  Repos come from the ~/code allowlist.
;;                      <kind>: d docs and READMEs, h holes, m missions, t technotes,
;;                      s scripts; none = the repo itself.   e.g. "6 d", "3c m", "0".
;;   arxana://...       open an Arxana URI directly
;;   ?                  this list
;;
;; A code that is complete and is not the start of a longer code acts at once, with no RET
;; ("p", "6 d", "?"); otherwise finish with RET ("6", "r p4ng", an arxana:// URI).
;;
;; Keys: <f9> and ※.  (<f9> used to insert ※; ※ used to open the Arxana browser home,
;; which is now "※ RET".)

(require 'subr-x)

(defvar mojira-allowlist-file
  (expand-file-name "~/code/futon0/system/zone/code-allowlist/allowlist.tsv")
  "The ~/code allowlist (name, kind, remote): the source of repo shortcodes.")

(defvar mojira-code-dir (expand-file-name "~/code/"))

(defvar mojira-shortcodes
  '(("p" . (lambda () (mojira--open-uri "arxana://view/patterns"))))
  "Fixed shortcodes: (CODE . FUNCTION).  Repo shortcodes are derived from the allowlist.")

(defvar mojira-kinds
  '(("d" "docs and READMEs" docs)
    ("h" "holes" "holes")
    ("m" "missions" "holes/missions")
    ("t" "technotes" "holes/technotes" "technotes" "docs/technotes")
    ("s" "scripts" "scripts"))
  "KEY, label, then the paths tried in order (or the symbol `docs' for README* + docs/).")

(defvar mojira-history nil)

(defun mojira--repos ()
  "Alist of (KEY . NAME) for every repo on the allowlist."
  (when (file-exists-p mojira-allowlist-file)
    (with-temp-buffer
      (insert-file-contents mojira-allowlist-file)
      (let (out)
        (dolist (line (split-string (buffer-string) "\n" t))
          (let* ((cols (split-string line "\t"))
                 (name (car cols)))
            (when (equal (nth 1 cols) "repo")
              (push (cons (concat "r " name) name) out)
              (when (string-match "\\`futon\\([0-9][0-9a-z]*\\)\\'" name)
                (push (cons (match-string 1 name) name) out)))))
        out))))

(defun mojira--open-uri (uri)
  (unless (fboundp 'arxana-docbook-open-uri)
    (require 'arxana-docbook-ui nil t))
  (if (fboundp 'arxana-docbook-open-uri)
      (arxana-docbook-open-uri uri)
    (user-error "Arxana is not loaded; cannot open %s" uri)))

(defun mojira--open-kind (root kind)
  (let* ((spec (cdr (assoc kind mojira-kinds)))
         (paths (cdr spec)))
    (unless spec (user-error "Unknown kind %s (try ?)" kind))
    (if (eq (car paths) 'docs)
        (let ((files (append (file-expand-wildcards (expand-file-name "README*" root))
                             (and (file-directory-p (expand-file-name "docs" root))
                                  (list (expand-file-name "docs" root))))))
          (if files
              (dired (cons root (mapcar (lambda (f) (file-relative-name f root)) files)))
            (dired root)))
      (let ((dir (seq-find #'file-directory-p
                           (mapcar (lambda (p) (expand-file-name p root)) paths))))
        (if dir (dired dir) (user-error "%s has no %s" root (car spec)))))))

(defun mojira--all-codes ()
  "Every complete shortcode: fixed codes, ?, repo keys, and repo keys with a kind."
  (let ((codes (cons "?" (mapcar #'car mojira-shortcodes))))
    (dolist (r (mojira--repos))
      (push (car r) codes)
      (dolist (k mojira-kinds) (push (concat (car r) " " (car k)) codes)))
    codes))

(defvar mojira--codes nil)

(defun mojira--maybe-exit ()
  "Exit the ※ prompt as soon as its contents are a complete code that starts no longer one."
  (let ((s (minibuffer-contents)))
    (when (and (member s mojira--codes)
               (not (seq-some (lambda (c) (and (not (equal c s)) (string-prefix-p s c)))
                              mojira--codes)))
      (exit-minibuffer))))

(defun mojira--read ()
  (setq mojira--codes (mojira--all-codes))
  (minibuffer-with-setup-hook
      (lambda () (add-hook 'post-command-hook #'mojira--maybe-exit nil t))
    (read-string "※ " nil 'mojira-history)))

(defun mojira-help ()
  "Show the shortcodes."
  (interactive)
  (with-help-window "*※ shortcodes*"
    (princ "※ shortcodes\n\n  p        Arxana Patterns view\n  RET      Arxana browser home\n")
    (princ "  arxana://...  open an Arxana URI\n\n  <n> [<kind>]       e.g. 6 d, 3c m, 0 RET\n  r <name> [<kind>]  other repos, e.g. r p4ng RET\n\n  kinds:\n")
    (dolist (k mojira-kinds) (princ (format "    %s  %s\n" (car k) (cadr k))))
    (princ "\n  repos:\n")
    (dolist (r (sort (mojira--repos) (lambda (a b) (string< (car a) (car b)))))
      (unless (string-prefix-p "r " (car r)) (princ (format "    %-4s %s\n" (car r) (cdr r)))))))

(defun mojira-dispatch (code)
  "Read a Mojira shortcode at the ※ prompt and act on it."
  (interactive (list (mojira--read)))
  (let* ((code (string-trim code))
         (words (split-string code " " t))
         (fixed (cdr (assoc code mojira-shortcodes)))
         (named (equal (car words) "r"))
         (key (if named (string-join (seq-take words 2) " ") (car words)))
         (kind (nth (if named 2 1) words))
         (repo (and key (cdr (assoc key (mojira--repos))))))
    (cond
     ((string-empty-p code)
      (if (fboundp 'arxana-browser-home) (arxana-browser-home) (mojira--open-uri "arxana://view/home")))
     ((equal code "?") (mojira-help))
     (fixed (funcall fixed))
     ((string-prefix-p "arxana://" code) (mojira--open-uri code))
     (repo
      (let ((root (expand-file-name repo mojira-code-dir)))
        (if kind (mojira--open-kind root kind) (dired root))))
     (t (user-error "※ %s: unknown shortcode (try ?)" code)))))

(global-set-key (kbd "<f9>") #'mojira-dispatch)
(global-set-key (kbd "※") #'mojira-dispatch)

(provide 'mojira-dispatch)
;;; mojira-dispatch.el ends here
