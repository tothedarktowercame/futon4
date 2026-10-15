;;; arxana-browser-issues.el --- Personal kanban over GitHub issues -*- lexical-binding: t; -*-

;;; Commentary:
;; A small kanban board over the GitHub issues and PRs that involve the
;; operator (`arxana-issues-login'): Rob's electric-samurai/mfuton-share
;; plus the tothedarktowercame/futon* repos.  GitHub has no board for
;; these, so the lanes are local: GitHub supplies the cards, and the lane
;; each card sits in is kept in `arxana-issues-board-file'.
;;
;; One `gh api graphql' call (async) fetches several searches at once
;; (involves / assigned / mentioned / the whole mfuton-share tracker /
;; recently closed).  The result is cached in `arxana-issues-cache-file'
;; so the board opens instantly; `g' re-fetches.
;;
;; Cards that nobody has placed get a default lane:
;;   closed on GitHub                          -> Done
;;   assigned to someone else, not me          -> Waiting
;;   touched by someone else (author, assignee,
;;   last comment) or in mfuton-share          -> Inbox
;;   otherwise (my own notes-to-self)          -> Backlog
;; A ● marks an open card where someone else (last commenter, or the
;; author if there are no comments) acted after I last opened it on the
;; board, so Rob's new issues and comments stand out.
;;
;; Entry points: M-x arxana-issues-board, or "Issues" on the browser home.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup arxana-browser-issues nil
  "Personal kanban board over GitHub issues."
  :group 'arxana)

(defcustom arxana-issues-login "tothedarktowercame"
  "GitHub login whose issues the board tracks."
  :type 'string
  :group 'arxana-browser-issues)

(defcustom arxana-issues-owners '("tothedarktowercame" "electric-samurai")
  "GitHub owners searched (each becomes a `user:' qualifier)."
  :type '(repeat string)
  :group 'arxana-browser-issues)

(defcustom arxana-issues-shared-repos '("electric-samurai/mfuton-share")
  "Repos whose whole open tracker is on the board, involved or not."
  :type '(repeat string)
  :group 'arxana-browser-issues)

(defcustom arxana-issues-repo-regexp
  "\\`\\(electric-samurai/mfuton-share\\|tothedarktowercame/futon[^/]*\\)\\'"
  "Only cards whose repo (owner/name) matches this regexp are shown."
  :type 'regexp
  :group 'arxana-browser-issues)

(defcustom arxana-issues-closed-days 30
  "Closed cards updated within this many days are fetched for Done."
  :type 'integer
  :group 'arxana-browser-issues)

(defcustom arxana-issues-board-file
  (expand-file-name "~/notes/issues-board/board.eld")
  "Local lane/seen state, an alist printed with `prin1'."
  :type 'file
  :group 'arxana-browser-issues)

(defcustom arxana-issues-cache-file
  (expand-file-name "~/notes/issues-board/cache.json")
  "Last GitHub fetch, raw GraphQL JSON."
  :type 'file
  :group 'arxana-browser-issues)

(defconst arxana-issues--lanes
  '((inbox   "Inbox"   "new or untriaged; someone else touched it")
    (next    "Next"    "picked to do soon")
    (doing   "Doing"   "in hand")
    (waiting "Waiting" "ball is in someone else's court")
    (backlog "Backlog" "my own notes-to-self; collapsed by default")
    (done    "Done"    "closed on GitHub, or parked as done"))
  "Lane id, title, blurb — in board order.")

(defvar arxana-issues--buffer "*Arxana Issues*")

(defvar arxana-issues--fetching nil
  "Non-nil while a `gh' fetch is running.")

(defvar arxana-issues--collapsed '(backlog)
  "Lanes rendered as a single header line.")

;; ------------------------------------------------------------ fetch ----

(defconst arxana-issues--fragment
  "fragment F on Issue { __typename number title url state updatedAt createdAt
  repository { nameWithOwner } author { login }
  assignees(first: 10) { nodes { login } } labels(first: 10) { nodes { name } }
  comments(last: 1) { totalCount nodes { author { login } createdAt } } }
fragment P on PullRequest { __typename number title url state updatedAt createdAt
  repository { nameWithOwner } author { login }
  assignees(first: 10) { nodes { login } } labels(first: 10) { nodes { name } }
  comments(last: 1) { totalCount nodes { author { login } createdAt } } }")

(defun arxana-issues--searches ()
  "Return (ALIAS . SEARCH-QUERY) pairs for one GraphQL call."
  (let* ((me arxana-issues-login)
         (users (mapconcat (lambda (o) (concat "user:" o)) arxana-issues-owners " "))
         (since (format-time-string
                 "%Y-%m-%d"
                 (time-subtract nil (days-to-time arxana-issues-closed-days)) t)))
    (append
     `(("involves" . ,(format "involves:%s %s is:open" me users))
       ("assigned" . ,(format "assignee:%s %s is:open" me users))
       ("mentioned" . ,(format "mentions:%s %s is:open" me users))
       ("closed" . ,(format "involves:%s %s is:closed updated:>=%s" me users since)))
     (cl-loop for repo in arxana-issues-shared-repos
              for i from 0
              collect (cons (format "shared%d" i)
                            (format "repo:%s is:open" repo))))))

(defun arxana-issues--graphql ()
  (concat
   "query {\n"
   (mapconcat (lambda (s)
                (format "  %s: search(type: ISSUE, first: 100, query: %S) { nodes { ...F ...P } }"
                        (car s) (cdr s)))
              (arxana-issues--searches) "\n")
   "\n}\n" arxana-issues--fragment))

(defun arxana-issues-fetch (&optional then)
  "Fetch cards from GitHub asynchronously; call THEN with no args on success."
  (interactive)
  (if arxana-issues--fetching
      (message "Arxana issues: fetch already running")
    (let ((out (generate-new-buffer " *arxana-issues-gh*")))
      (setq arxana-issues--fetching t)
      (message "Arxana issues: fetching from GitHub…")
      (make-process
       :name "arxana-issues-gh"
       :buffer out
       :noquery t
       :connection-type 'pipe
       :command (list "gh" "api" "graphql" "-f"
                      (concat "query=" (arxana-issues--graphql)))
       :sentinel
       (lambda (proc _event)
         (unless (process-live-p proc)
           (setq arxana-issues--fetching nil)
           (let ((buf (process-buffer proc)))
             (unwind-protect
                 (if (and (eq (process-status proc) 'exit)
                          (zerop (process-exit-status proc)))
                     (let ((json (with-current-buffer buf (buffer-string))))
                       (make-directory (file-name-directory arxana-issues-cache-file) t)
                       (with-temp-file arxana-issues-cache-file (insert json))
                       (message "Arxana issues: fetched %d card(s)"
                                (length (arxana-issues--cards)))
                       (when then (funcall then)))
                   (message "Arxana issues: gh failed: %s"
                            (string-trim (with-current-buffer buf
                                           (buffer-substring (point-min)
                                                             (min (point-max) 400))))))
               (kill-buffer buf)))))))))

;; ------------------------------------------------------------- data ----

(defun arxana-issues--read-cache ()
  "Return the cached GraphQL `data' object as a hash table, or nil."
  (when (file-readable-p arxana-issues-cache-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents arxana-issues-cache-file)
        (gethash "data" (json-parse-buffer :object-type 'hash-table
                                           :array-type 'list
                                           :null-object nil))))))

(defun arxana-issues--logins (node key)
  (delq nil (mapcar (lambda (n) (and n (gethash "login" n)))
                    (gethash "nodes" (or (gethash key node) (make-hash-table))))))

(defun arxana-issues--node->card (node reasons)
  (let* ((repo (gethash "nameWithOwner" (gethash "repository" node)))
         (num (gethash "number" node))
         (author (let ((a (gethash "author" node))) (and a (gethash "login" a))))
         (comments (gethash "comments" node))
         (last (car (gethash "nodes" comments)))
         (last-by (and last (gethash "author" last)
                       (gethash "login" (gethash "author" last)))))
    (list :key (format "%s#%d" repo num)
          :repo repo
          :number num
          :kind (if (equal (gethash "__typename" node) "PullRequest") "PR" "issue")
          :title (gethash "title" node)
          :url (gethash "url" node)
          :state (gethash "state" node)
          :updated (gethash "updatedAt" node)
          :author author
          :assignees (arxana-issues--logins node "assignees")
          :labels (delq nil (mapcar (lambda (l) (and l (gethash "name" l)))
                                    (gethash "nodes" (gethash "labels" node))))
          :comment-count (gethash "totalCount" comments)
          :last-comment-by last-by
          :last-comment-at (and last (gethash "createdAt" last))
          :reasons reasons)))

(defun arxana-issues--cards ()
  "Return the deduplicated card plists from the cache, newest first."
  (let ((data (arxana-issues--read-cache))
        (by-key (make-hash-table :test #'equal))
        order)
    (when data
      (maphash
       (lambda (alias result)
         (let ((reason (cond ((string-prefix-p "shared" alias) "shared")
                             ((equal alias "closed") nil)
                             (t alias))))
           (dolist (node (gethash "nodes" result))
             ;; Empty objects are search hits of other types (discussions).
             (when (and node (gethash "number" node))
               (let* ((repo (gethash "nameWithOwner" (gethash "repository" node)))
                      (key (format "%s#%d" repo (gethash "number" node)))
                      (old (gethash key by-key)))
                 (when (string-match-p arxana-issues-repo-regexp repo)
                   (if old
                       (when (and reason (not (member reason (plist-get old :reasons))))
                         (puthash key (plist-put old :reasons
                                                 (append (plist-get old :reasons)
                                                         (list reason)))
                                  by-key))
                     (push key order)
                     (puthash key (arxana-issues--node->card
                                   node (and reason (list reason)))
                              by-key))))))))
       data))
    (sort (mapcar (lambda (k) (gethash k by-key)) order)
          (lambda (a b) (string> (plist-get a :updated) (plist-get b :updated))))))

;; ------------------------------------------------------------ board ----

(defun arxana-issues--board ()
  "Return the board alist: KEY -> plist (:lane SYM :seen ISO)."
  (when (file-readable-p arxana-issues-board-file)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents arxana-issues-board-file)
        (read (current-buffer))))))

(defun arxana-issues--save-board (board)
  (make-directory (file-name-directory arxana-issues-board-file) t)
  (with-temp-file arxana-issues-board-file
    (insert ";; Arxana issues board: card key -> (:lane LANE :seen UPDATED-AT)\n")
    (let ((print-length nil) (print-level nil))
      (pp board (current-buffer)))))

(defun arxana-issues--board-update (key prop value)
  (let* ((board (arxana-issues--board))
         (cell (assoc key board)))
    (if cell
        (setcdr cell (plist-put (cdr cell) prop value))
      (push (cons key (list prop value)) board))
    (arxana-issues--save-board board)))

(defun arxana-issues--others-touched-p (card)
  (let ((me arxana-issues-login))
    (or (member "shared" (plist-get card :reasons))
        (and (plist-get card :author) (not (equal (plist-get card :author) me)))
        (cl-some (lambda (l) (not (equal l me))) (plist-get card :assignees))
        (and (plist-get card :last-comment-by)
             (not (equal (plist-get card :last-comment-by) me))))))

(defun arxana-issues--default-lane (card)
  (let ((me arxana-issues-login)
        (assignees (plist-get card :assignees)))
    (cond ((not (equal (plist-get card :state) "OPEN")) 'done)
          ((and assignees (not (member me assignees))) 'waiting)
          ((arxana-issues--others-touched-p card) 'inbox)
          (t 'backlog))))

(defun arxana-issues--lane (card board)
  "Lane for CARD.  A card closed on GitHub is Done whatever its saved lane."
  (let ((saved (plist-get (cdr (assoc (plist-get card :key) board)) :lane)))
    (if (and saved (equal (plist-get card :state) "OPEN"))
        saved
      (arxana-issues--default-lane card))))

(defun arxana-issues--unseen-p (card board)
  "Non-nil when someone else acted on open CARD since I last opened it.
\"Someone else\" means the last commenter, or the author when there are
no comments; my own comments never raise the mark."
  (let ((seen (plist-get (cdr (assoc (plist-get card :key) board)) :seen))
        (actor (or (plist-get card :last-comment-by) (plist-get card :author))))
    (and (equal (plist-get card :state) "OPEN")
         actor (not (equal actor arxana-issues-login))
         (or (null seen) (string> (plist-get card :updated) seen)))))

(defun arxana-issues--lane-cards (lane cards board)
  (cl-remove-if-not (lambda (c) (eq (arxana-issues--lane c board) lane)) cards))

;; ----------------------------------------------------------- render ----

(defun arxana-issues--ago (iso)
  (if (not iso) ""
    (let ((s (float-time (time-subtract nil (date-to-time iso)))))
      (cond ((< s 3600) (format "%dm" (/ s 60)))
            ((< s 86400) (format "%dh" (/ s 3600)))
            (t (format "%dd" (/ s 86400)))))))

(defun arxana-issues--short-repo (repo)
  (replace-regexp-in-string "\\`[^/]+/" "" repo))

(defun arxana-issues--insert-card (card board)
  (let* ((start (point))
         (unseen (arxana-issues--unseen-p card board))
         (who (plist-get card :last-comment-by))
         (tags (append (cl-remove "involves" (plist-get card :reasons) :test #'equal)
                       (plist-get card :labels))))
    (insert (if unseen (propertize "●" 'face 'warning) " ")
            " "
            (propertize (format "%-20s" (format "%s#%d"
                                               (arxana-issues--short-repo (plist-get card :repo))
                                               (plist-get card :number)))
                        'face 'font-lock-constant-face)
            (if (equal (plist-get card :kind) "PR") (propertize "PR " 'face 'font-lock-type-face) "")
            (plist-get card :title)
            (propertize
             (concat "  "
                     (format "%s ago" (arxana-issues--ago (plist-get card :updated)))
                     (if who (format " · last: %s" who) "")
                     (if tags (format " · %s" (string-join tags ",")) ""))
             'face 'shadow)
            "\n")
    (put-text-property start (point) 'arxana-issues-card card)))

(defun arxana-issues--render ()
  (let* ((cards (arxana-issues--cards))
         (board (arxana-issues--board))
         (buf (get-buffer-create arxana-issues--buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'arxana-issues-mode) (arxana-issues-mode))
      (let ((inhibit-read-only t)
            (line (line-number-at-pos))
            (col (current-column)))
        (erase-buffer)
        (insert (propertize "Arxana Issues — kanban over GitHub" 'face 'bold) "\n")
        (insert (propertize
                 (format "%s involved · %s  (fetched %s)\n"
                         arxana-issues-login
                         (string-join arxana-issues-shared-repos ", ")
                         (if (file-exists-p arxana-issues-cache-file)
                             (format-time-string
                              "%m-%d %H:%M"
                              (file-attribute-modification-time
                               (file-attributes arxana-issues-cache-file)))
                           "never — press g"))
                 'face 'shadow))
        (insert (propertize "RET view · o browser · m move · </> shift lane · TAB fold · g fetch · n/p cards\n\n"
                            'face 'shadow))
        (dolist (spec arxana-issues--lanes)
          (let* ((lane (car spec))
                 (in (arxana-issues--lane-cards lane cards board))
                 (unseen (cl-count-if (lambda (c) (arxana-issues--unseen-p c board)) in))
                 (folded (memq lane arxana-issues--collapsed))
                 (start (point)))
            (insert (propertize (format "%s %s (%d%s)"
                                        (if folded "▸" "▾")
                                        (nth 1 spec) (length in)
                                        (if (> unseen 0) (format ", %d ●" unseen) ""))
                                'face 'font-lock-keyword-face)
                    (propertize (format "  — %s\n" (nth 2 spec)) 'face 'shadow))
            (put-text-property start (point) 'arxana-issues-lane lane)
            (unless folded
              (dolist (c in) (arxana-issues--insert-card c board))
              (when (null in) (insert (propertize "   (empty)\n" 'face 'shadow))))
            (insert "\n")))
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col)))
    buf))

(defun arxana-issues--card-at-point ()
  (or (get-text-property (point) 'arxana-issues-card)
      (get-text-property (line-beginning-position) 'arxana-issues-card)))

(defun arxana-issues--require-card ()
  (or (arxana-issues--card-at-point) (user-error "No card on this line")))

;; ---------------------------------------------------------- commands ----

(defun arxana-issues-refresh ()
  "Re-fetch from GitHub, then re-render."
  (interactive)
  (arxana-issues-fetch
   (lambda ()
     (when (get-buffer arxana-issues--buffer) (arxana-issues--render)))))

(defun arxana-issues-move (lane)
  "Move the card at point to LANE."
  (interactive
   (list (intern (completing-read
                  "Lane: "
                  (mapcar (lambda (s) (symbol-name (car s))) arxana-issues--lanes)
                  nil t))))
  (let ((card (arxana-issues--require-card)))
    (arxana-issues--board-update (plist-get card :key) :lane lane)
    (when (memq lane arxana-issues--collapsed)
      (setq arxana-issues--collapsed (delq lane arxana-issues--collapsed)))
    (arxana-issues--render)
    (arxana-issues--goto-card (plist-get card :key))
    (message "%s → %s" (plist-get card :key) lane)))

(defun arxana-issues--goto-card (key)
  "Put point on the card whose key is KEY, if it is shown."
  (let ((pos (point-min)) found)
    (while (and (not found) (setq pos (next-single-property-change
                                       pos 'arxana-issues-card)))
      (when (equal (plist-get (get-text-property pos 'arxana-issues-card) :key) key)
        (setq found pos)))
    (when found (goto-char found))))

(defun arxana-issues--shift (delta)
  (let* ((card (arxana-issues--require-card))
         (ids (mapcar #'car arxana-issues--lanes))
         (cur (arxana-issues--lane card (arxana-issues--board)))
         (i (max 0 (min (1- (length ids)) (+ delta (cl-position cur ids))))))
    (arxana-issues-move (nth i ids))))

(defun arxana-issues-shift-right ()
  "Move the card at point one lane to the right."
  (interactive)
  (arxana-issues--shift 1))

(defun arxana-issues-shift-left ()
  "Move the card at point one lane to the left."
  (interactive)
  (arxana-issues--shift -1))

(defun arxana-issues-mark-seen ()
  "Clear the ● on the card at point without opening it."
  (interactive)
  (let ((card (arxana-issues--require-card)))
    (arxana-issues--board-update (plist-get card :key) :seen (plist-get card :updated))
    (arxana-issues--render)))

(defun arxana-issues-browse-url ()
  "Open the card at point in a web browser, and mark it seen."
  (interactive)
  (let ((card (arxana-issues--require-card)))
    (arxana-issues--board-update (plist-get card :key) :seen (plist-get card :updated))
    (browse-url (plist-get card :url))
    (arxana-issues--render)))

(defun arxana-issues--insert-thread (data)
  "Insert the issue or PR DATA (parsed `gh ... view --json') as plain text."
  (let ((login (lambda (o) (and o (gethash "login" o)))))
    (insert (propertize (or (gethash "title" data) "") 'face 'bold) "\n"
            (propertize (format "%s · opened by %s %s · assigned: %s\n%s\n"
                                (gethash "state" data)
                                (funcall login (gethash "author" data))
                                (substring (or (gethash "createdAt" data) "") 0 10)
                                (or (mapconcat login (gethash "assignees" data) ", ") "")
                                (gethash "url" data))
                        'face 'shadow)
            "\n" (string-trim (or (gethash "body" data) "")) "\n")
    (dolist (c (gethash "comments" data))
      (insert "\n" (propertize (format "── %s · %s\n"
                                       (funcall login (gethash "author" c))
                                       (gethash "createdAt" c))
                               'face 'font-lock-keyword-face)
              (string-trim (or (gethash "body" c) "")) "\n"))
    (when (null (gethash "comments" data))
      (insert "\n" (propertize "(no comments)\n" 'face 'shadow)))))

(defun arxana-issues-view ()
  "Show the card at point (body and comments, via `gh') and mark it seen."
  (interactive)
  (let* ((card (arxana-issues--require-card))
         (buf (get-buffer-create (format "*gh %s*" (plist-get card :key))))
         (out (generate-new-buffer " *arxana-issues-view*")))
    (arxana-issues--board-update (plist-get card :key) :seen (plist-get card :updated))
    (arxana-issues--render)
    (with-current-buffer buf
      (special-mode)
      (let ((inhibit-read-only t)) (erase-buffer) (insert "Loading…\n")))
    ;; Use --json: without a TTY, `gh issue view --comments' prints only
    ;; the comments, so an issue with none came back empty.
    (make-process
     :name "arxana-issues-view" :buffer out :noquery t :connection-type 'pipe
     :command (list "gh" (if (equal (plist-get card :kind) "PR") "pr" "issue")
                    "view" (number-to-string (plist-get card :number))
                    "-R" (plist-get card :repo)
                    "--json" "title,state,author,assignees,body,comments,url,createdAt")
     :sentinel
     (lambda (proc _e)
       (unless (process-live-p proc)
         (let ((text (with-current-buffer out (buffer-string))))
           (kill-buffer out)
           (when (buffer-live-p buf)
             (with-current-buffer buf
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (condition-case err
                     (arxana-issues--insert-thread
                      (json-parse-string text :object-type 'hash-table
                                         :array-type 'list :null-object nil))
                   (error (insert (format "gh failed (%s):\n%s"
                                          (error-message-string err) text))))
                 (goto-char (point-min)))))))))
    (pop-to-buffer buf)))

(defun arxana-issues-toggle-lane ()
  "Fold or unfold the lane whose header is at point."
  (interactive)
  (let ((lane (get-text-property (line-beginning-position) 'arxana-issues-lane)))
    (unless lane (user-error "Not on a lane header"))
    (setq arxana-issues--collapsed
          (if (memq lane arxana-issues--collapsed)
              (delq lane arxana-issues--collapsed)
            (cons lane arxana-issues--collapsed)))
    (arxana-issues--render)))

(defun arxana-issues--next-prop (prop dir)
  "Move to the next line (DIR 1) or previous line (DIR -1) carrying PROP."
  (let ((pos (save-excursion
               (forward-line dir)
               (while (and (not (get-text-property (point) prop))
                           (zerop (forward-line dir))
                           (not (eobp))))
               (and (get-text-property (point) prop) (point)))))
    (if pos (goto-char pos) (message "No more cards"))))

(defun arxana-issues-next-card ()
  (interactive)
  (arxana-issues--next-prop 'arxana-issues-card 1))

(defun arxana-issues-prev-card ()
  (interactive)
  (arxana-issues--next-prop 'arxana-issues-card -1))

(defun arxana-issues-ret ()
  "View the card at point, or fold the lane if on a header."
  (interactive)
  (if (get-text-property (line-beginning-position) 'arxana-issues-lane)
      (arxana-issues-toggle-lane)
    (arxana-issues-view)))

(defvar arxana-issues-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'arxana-issues-ret)
    (define-key m (kbd "TAB") #'arxana-issues-toggle-lane)
    (define-key m "o" #'arxana-issues-browse-url)
    (define-key m "m" #'arxana-issues-move)
    (define-key m ">" #'arxana-issues-shift-right)
    (define-key m "<" #'arxana-issues-shift-left)
    (define-key m "s" #'arxana-issues-mark-seen)
    (define-key m "g" #'arxana-issues-refresh)
    (define-key m "n" #'arxana-issues-next-card)
    (define-key m "p" #'arxana-issues-prev-card)
    m)
  "Keymap for `arxana-issues-mode'.")

(define-derived-mode arxana-issues-mode special-mode "Arxana-Issues"
  "Kanban board over GitHub issues.
\\{arxana-issues-mode-map}"
  (setq truncate-lines t))

;;;###autoload
(defun arxana-issues-board (&optional lane)
  "Open the issues kanban board; jump to LANE when given.
Fetches from GitHub first if there is no cache yet."
  (interactive)
  (let ((show (lambda ()
                (let ((buf (arxana-issues--render)))
                  (pop-to-buffer buf)
                  (when lane
                    (setq arxana-issues--collapsed (delq lane arxana-issues--collapsed))
                    (arxana-issues--render)
                    (goto-char (point-min))
                    (let ((pos (text-property-any (point-min) (point-max)
                                                  'arxana-issues-lane lane)))
                      (when pos (goto-char pos))))))))
    (if (file-exists-p arxana-issues-cache-file)
        (funcall show)
      (arxana-issues-fetch show))))

;; -------------------------------------------------- home integration ----

(defun arxana-issues--home-items ()
  "One browser-home row per lane, with counts."
  (let ((cards (arxana-issues--cards))
        (board (arxana-issues--board)))
    (mapcar
     (lambda (spec)
       (let* ((in (arxana-issues--lane-cards (car spec) cards board))
              (unseen (cl-count-if (lambda (c) (arxana-issues--unseen-p c board)) in)))
         (list :type 'issues-lane
               :label (nth 1 spec)
               :lane (car spec)
               :description (format "%d card(s)%s — %s" (length in)
                                    (if (> unseen 0) (format ", %d updated" unseen) "")
                                    (nth 2 spec)))))
     arxana-issues--lanes)))

(defun arxana-issues-open-lane (item)
  "Open the board at the lane named by browser-home ITEM."
  (arxana-issues-board (plist-get item :lane)))

(provide 'arxana-browser-issues)
;;; arxana-browser-issues.el ends here
