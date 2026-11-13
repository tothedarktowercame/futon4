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
;;   c 9 / x 9 / z 9 / k 9   the REPL buffer of claude-9 / codex-9 / zai-9 / kimi-9 (any number
;;                      with an open buffer; c 1 waits for RET while claude-1x buffers exist)
;;   c l / x l / ...    the last REPL of that kind: the one used most recently, other than
;;                      the buffer you are in ("xl" types as "x l" and acts at once)
;;   <n> l, r <name> l  the last file you visited in that repo, by any route
;;   l                  the last buffer (the one before this)
;;   "l" is a modal: it means "last" in whatever context the code before it names.
;;   arxana://...       open an Arxana URI directly
;;   ?                  this list
;;
;; A code that is complete and is not the start of a longer code acts at once, with no RET
;; ("p", "6 d", "?"); otherwise finish with RET ("6", "r p4ng", an arxana:// URI).
;;
;; Keys: <f9> and ※.  (<f9> used to insert ※; ※ used to open the Arxana browser home,
;; which is now "※ RET".)

(require 'subr-x)
(require 'json)

;; The fixed codes, kinds and agent letters are shared with code-server Mojira (its : prompt):
;; one table, futon4/dev/mojira-codes.json, so a code added there applies in both places.
(defvar mojira-codes-file
  (expand-file-name "mojira-codes.json"
                    (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "The shared shortcode table (futon4/dev/mojira-codes.json).")

(defun mojira--table ()
  "The shared table as an alist, or nil if it cannot be read."
  (ignore-errors
    (let ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'symbol))
      (json-read-file mojira-codes-file))))

(defvar mojira-allowlist-file
  (expand-file-name "~/code/futon0/system/zone/code-allowlist/allowlist.tsv")
  "The ~/code allowlist (name, kind, remote): the source of repo shortcodes.")

(defvar mojira-code-dir (expand-file-name "~/code/"))

(defvar mojira-shortcodes nil
  "Fixed shortcodes: (CODE . FUNCTION), from the shared table (`mojira--load-table').
Repo shortcodes are derived from the allowlist.")

(defvar mojira-kinds
  '(("d" "docs and READMEs" docs)
    ("h" "holes" "holes")
    ("m" "missions" "holes/missions")
    ("t" "technotes" "holes/technotes" "technotes" "docs/technotes")
    ("s" "scripts" "scripts"))
  "KEY, label, then the paths tried in order (or the symbol `docs' for README* + docs/).
Replaced from the shared table when it can be read.")

(defvar mojira-agent-modals '(("l" . "last"))
  "Agent modals: \"c l\" is the last claude REPL.  From the shared table when readable.")

(defun mojira--load-table ()
  "Set the fixed codes, kinds, agent letters and modals from the shared table."
  (let ((tb (mojira--table)))
    (when tb
      (setq mojira-shortcodes
            (mapcar (lambda (c)
                      (let ((code (alist-get 'code c))
                            (label (alist-get 'label c))
                            (uri (alist-get 'uri (alist-get 'emacs c))))
                        (cons code
                              (if uri
                                  (lambda () (mojira--open-uri uri))
                                (lambda ()
                                  (user-error "※ %s (%s) works in code-server Mojira only" code label))))))
                    (alist-get 'codes tb)))
      (setq mojira-kinds
            (mapcar (lambda (k)
                      (append (list (alist-get 'key k) (alist-get 'label k))
                              (if (eq (alist-get 'docs k) t) '(docs) (alist-get 'paths k))))
                    (alist-get 'kinds tb)))
      (setq mojira-agent-kinds
            (mapcar (lambda (a) (cons (alist-get 'key a) (alist-get 'kind a))) (alist-get 'agents tb)))
      (setq mojira-agent-modals
            (mapcar (lambda (m) (cons (alist-get 'key m) (alist-get 'label m))) (alist-get 'agent_modals tb))))))

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

(defvar mojira-agent-kinds '(("c" . "claude") ("x" . "codex") ("z" . "zai") ("k" . "kimi"))
  "Agent shortcode letter -> agent kind; REPL buffers are named *KIND-repl:KIND-N*.
Replaced from the shared table when it can be read.")

(defun mojira--agents ()
  "Alist of (CODE . BUFFER-NAME) for every open agent REPL buffer, e.g. (\"c 9\" . ...).
Also \"c l\" (and so on): the most recently used REPL of that kind other than the current
buffer, as `buffer-list' orders them."
  (let (out last)
    (dolist (b (buffer-list))
      (let ((n (buffer-name b)))
        (dolist (k mojira-agent-kinds)
          (when (string-match (format "\\`\\*%s-repl:%s-\\([0-9]+\\)\\*\\'" (cdr k) (cdr k)) n)
            (push (cons (concat (car k) " " (match-string 1 n)) n) out)
            (when (and (not (eq b (current-buffer))) (not (assoc (car k) last)))
              (push (cons (car k) n) last))))))
    (when (assoc "l" mojira-agent-modals)
      (dolist (l last) (push (cons (concat (car l) " l") (cdr l)) out)))
    out))

(defun mojira--all-codes ()
  "Every complete shortcode: fixed codes, ?, l, repo keys, repo keys with a kind or l."
  (let ((codes (append (list "?") (mapcar #'car mojira-shortcodes) (mapcar #'car (mojira--agents)))))
    (when (assoc "l" mojira-agent-modals) (push "l" codes))
    (dolist (r (mojira--repos))
      (push (car r) codes)
      (when (assoc "l" mojira-agent-modals) (push (concat (car r) " l") codes))
      (dolist (k mojira-kinds) (push (concat (car r) " " (car k)) codes)))
    codes))

(defun mojira--last-file-in (root)
  "The file under ROOT visited most recently, by any route: a file buffer other than the
current one, in `buffer-list' (most recently used) order, else the newest `recentf' entry."
  (let ((root (file-name-as-directory (expand-file-name root))))
    (or (seq-some (lambda (b)
                    (let ((f (buffer-file-name b)))
                      (and f (not (eq b (current-buffer))) (string-prefix-p root (expand-file-name f)) f)))
                  (buffer-list))
        (and (boundp 'recentf-list)
             (seq-find (lambda (f) (string-prefix-p root (expand-file-name f))) recentf-list)))))

(defvar mojira--codes nil)

(defun mojira--maybe-exit ()
  "After a typed character: exit the ※ prompt if its contents are a complete code that starts
no longer one; otherwise insert a space if every code continuing the contents continues with
a space (so \"c\" becomes \"c \", \"6\" becomes \"6 \", but \"1\" stays, since \"1b\" exists)."
  (when (eq this-command 'self-insert-command)
    ;; A character that fits no code as typed, but would after a space ("1b" + "d"): add the space.
    (let* ((s (minibuffer-contents))
           (spaced (and (> (length s) 1) (not (string-match-p " \\'" (substring s 0 -1)))
                        (concat (substring s 0 -1) " " (substring s -1)))))
      (when (and spaced
                 (not (seq-some (lambda (c) (string-prefix-p s c)) mojira--codes))
                 (seq-some (lambda (c) (string-prefix-p spaced c)) mojira--codes))
        (delete-char -1)
        (insert " " (substring s -1))))
    (let* ((s (minibuffer-contents))
           (longer (seq-filter (lambda (c) (and (not (equal c s)) (string-prefix-p s c)))
                               mojira--codes)))
      (cond
       ((and (member s mojira--codes) (null longer)) (exit-minibuffer))
       ((and longer (not (string-suffix-p " " s))
             (seq-every-p (lambda (c) (string-prefix-p (concat s " ") c)) longer))
        (insert " ")
        (mojira--hint (concat s " ")))
       (t (mojira--hint s))))))

(defun mojira--hint (s)
  "Show, after the ※ prompt's input S, what can come next: RET if S is a code, then the
possible next characters."
  (let* ((next (delete-dups
                (delq nil (mapcar (lambda (c)
                                    (and (string-prefix-p s c) (> (length c) (length s))
                                         (let ((ch (substring c (length s) (1+ (length s)))))
                                           (if (equal ch " ") nil ch))))
                                  mojira--codes))))
         (parts (append (and (member (string-trim-right s) mojira--codes)
                             (list (format "RET %s" (string-trim-right s))))
                        (and next (list (string-join (sort next #'string<) " "))))))
    (when parts
      (minibuffer-message "  [%s]" (string-join parts " | ")))))

(defun mojira--read ()
  (mojira--load-table)
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
    (princ "\n  agents: c N claude, x N codex, z N zai, k N kimi (open REPL buffers)\n")
    (princ "          c l, x l, ... the last REPL of that kind (other than this buffer)\n")
    (princ "\n  l: last, in the context before it.  6 l  the last file you visited in futon6;\n")
    (princ "     l alone  the last buffer\n")
    (princ (format "\n  shared with code-server Mojira (its : prompt): %s\n" mojira-codes-file))
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
     ((equal code "l") (switch-to-buffer (other-buffer (current-buffer) t)))
     ((assoc code (mojira--agents)) (pop-to-buffer (cdr (assoc code (mojira--agents)))))
     ((string-match "\\`[cxzk] \\([0-9]+\\|l\\)\\'" code)
      (user-error "※ %s: no open REPL buffer for that agent" code))
     (fixed (funcall fixed))
     ((string-prefix-p "arxana://" code) (mojira--open-uri code))
     ((and repo (equal kind "l"))
      (let ((f (mojira--last-file-in (expand-file-name repo mojira-code-dir))))
        (if f (find-file f) (user-error "※ %s: no file visited in %s yet" code repo))))
     (repo
      (let ((root (expand-file-name repo mojira-code-dir)))
        (if kind (mojira--open-kind root kind) (dired root))))
     (t (user-error "※ %s: unknown shortcode (try ?)" code)))))

(mojira--load-table)

(global-set-key (kbd "<f9>") #'mojira-dispatch)
(global-set-key (kbd "※") #'mojira-dispatch)

(provide 'mojira-dispatch)
;;; mojira-dispatch.el ends here
