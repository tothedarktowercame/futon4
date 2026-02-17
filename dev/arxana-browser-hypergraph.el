;;; arxana-browser-hypergraph.el --- Local hypergraph viewer -*- lexical-binding: t; -*-

;;; Commentary:
;; Browse local hypergraph JSON exports from the Arxana browser.

;;; Code:

(require 'json)
(require 'browse-url)
(require 'org)
(require 'subr-x)

(defgroup arxana-browser-hypergraph nil
  "Local hypergraph browser helpers for Arxana."
  :group 'arxana)

(defcustom arxana-browser-hypergraph-preview-limit 8
  "How many nodes and edges to preview in the detail buffer."
  :type 'integer
  :group 'arxana-browser-hypergraph)

(defcustom arxana-browser-hypergraph-lens-item-limit 6
  "How many items per lens section to render in summary view."
  :type 'integer
  :group 'arxana-browser-hypergraph)

(defcustom arxana-browser-hypergraph-open-style 'summary
  "How to open hypergraph entries."
  :type '(choice (const :tag "Summary buffer" summary)
                 (const :tag "Arxana glasses buffer" arxana)
                 (const :tag "HTML atlas" html)
                 (const :tag "Both summary and HTML" both))
  :group 'arxana-browser-hypergraph)

(defcustom arxana-browser-hypergraph-export-directory
  (expand-file-name "arxana-hypergraph" temporary-file-directory)
  "Directory where generated hypergraph HTML views are written."
  :type 'directory
  :group 'arxana-browser-hypergraph)

(defun arxana-browser-hypergraph--repo-root ()
  "Return the Futon4 checkout root."
  (if (boundp 'arxana-root-directory)
      arxana-root-directory
    (expand-file-name ".."
                      (file-name-directory (or load-file-name
                                               buffer-file-name
                                               default-directory)))))

(defcustom arxana-browser-hypergraph-sources
  (list (expand-file-name "../futon6/data/first-proof/thread-633512-hypergraph.json"
                          (arxana-browser-hypergraph--repo-root)))
  "List of hypergraph JSON files shown in the Arxana browser."
  :type '(repeat file)
  :group 'arxana-browser-hypergraph)

(defface arxana-browser-hypergraph-expression-face
  '((t :background "#d9fbe0" :box (:line-width 1 :color "#34a853")))
  "Face for expression surface overlays."
  :group 'arxana-browser-hypergraph)

(defface arxana-browser-hypergraph-mention-face
  '((t :background "#fff7cc" :box (:line-width 1 :color "#d97706")))
  "Face for mention overlays."
  :group 'arxana-browser-hypergraph)

(defface arxana-browser-hypergraph-discourse-face
  '((t :underline (:style wave :color "#8b5cf6")))
  "Face for discourse overlays."
  :group 'arxana-browser-hypergraph)

(defface arxana-browser-hypergraph-scope-face
  '((t :background "#dbeafe" :box (:line-width 1 :color "#2563eb")))
  "Face for scope opener overlays."
  :group 'arxana-browser-hypergraph)

(defface arxana-browser-hypergraph-subexpr-operator-face
  '((t :underline (:style line :color "#0ea5e9")))
  "Face for LaTeX operator token overlays inside expressions."
  :group 'arxana-browser-hypergraph)

(defface arxana-browser-hypergraph-subexpr-variable-face
  '((t :underline (:style line :color "#d946ef")))
  "Face for variable token overlays inside expressions."
  :group 'arxana-browser-hypergraph)

(defface arxana-browser-hypergraph-peer-face
  '((t :background "#dbeafe" :box (:line-width 1 :color "#2563eb")))
  "Face for peer-highlight overlays."
  :group 'arxana-browser-hypergraph)

(defface arxana-browser-hypergraph-peer-similar-face
  '((t :background "#f5d0fe" :box (:line-width 1 :color "#d946ef")))
  "Face for peer highlights representing similar (not strict identity) roles."
  :group 'arxana-browser-hypergraph)

(defface arxana-browser-hypergraph-question-face
  '((t :inherit org-level-1 :weight bold))
  "Face for question headings in Arxana hypergraph glasses buffers."
  :group 'arxana-browser-hypergraph)

(defface arxana-browser-hypergraph-answer-face
  '((t :inherit org-level-2 :weight bold))
  "Face for answer headings in Arxana hypergraph glasses buffers."
  :group 'arxana-browser-hypergraph)

(defface arxana-browser-hypergraph-comment-face
  '((t :inherit org-level-3 :weight bold))
  "Face for comment headings in Arxana hypergraph glasses buffers."
  :group 'arxana-browser-hypergraph)

(defvar arxana-browser-hypergraph-expression-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'arxana-browser-hypergraph-describe-at-point)
    (define-key map (kbd "RET") #'arxana-browser-hypergraph-describe-at-point)
    map)
  "Keymap used by expression overlays in Arxana glasses buffers.")

(defvar arxana-browser-hypergraph-mention-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'arxana-browser-hypergraph-describe-at-point)
    (define-key map (kbd "RET") #'arxana-browser-hypergraph-describe-at-point)
    map)
  "Keymap used by mention overlays in Arxana glasses buffers.")

(defvar arxana-browser-hypergraph-glasses-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "RET") #'arxana-browser-hypergraph-describe-at-point)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `arxana-browser-hypergraph-glasses-mode'.")

(defvar-local arxana-browser-hypergraph--peer-overlays nil
  "Peer highlight overlays for the current glasses buffer.")

(defvar-local arxana-browser-hypergraph--active-peer-key nil
  "Current peer key under point in the current glasses buffer.")

(define-derived-mode arxana-browser-hypergraph-glasses-mode org-mode "Arxana-Hypergraph"
  "Arxana-native hypergraph glasses buffer."
  (setq-local truncate-lines nil)
  (setq-local arxana-browser-hypergraph--peer-overlays nil)
  (setq-local arxana-browser-hypergraph--active-peer-key nil)
  (add-hook 'post-command-hook
            #'arxana-browser-hypergraph--peer-highlight-at-point
            nil
            t))

(defun arxana-browser-hypergraph--as-string (value)
  (cond
   ((null value) "")
   ((stringp value) value)
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun arxana-browser-hypergraph--safe-filename (value)
  (let* ((raw (downcase (arxana-browser-hypergraph--as-string value)))
         (normalized (replace-regexp-in-string "[^a-z0-9]+" "-" raw))
         (trimmed (replace-regexp-in-string "\\`-+\\|-+\\'" "" normalized)))
    (if (string-empty-p trimmed) "hypergraph" trimmed)))

(defun arxana-browser-hypergraph--escape-html (value)
  (let ((text (arxana-browser-hypergraph--as-string value)))
    (setq text (replace-regexp-in-string "&" "&amp;" text t t))
    (setq text (replace-regexp-in-string "<" "&lt;" text t t))
    (setq text (replace-regexp-in-string ">" "&gt;" text t t))
    (setq text (replace-regexp-in-string "\"" "&quot;" text t t))
    text))

(defun arxana-browser-hypergraph--get (data key)
  (let* ((sym (if (keywordp key)
                  (intern (substring (symbol-name key) 1))
                key))
         (keyw (if (keywordp key)
                   key
                 (intern (concat ":" (symbol-name key)))))
         (txt (symbol-name sym)))
    (cond
     ((hash-table-p data)
      (or (gethash sym data)
          (gethash keyw data)
          (gethash txt data)))
     ((and (listp data) (keywordp (car-safe data)))
      (or (plist-get data keyw)
          (plist-get data sym)))
     ((listp data)
      (or (alist-get sym data nil nil #'equal)
          (alist-get keyw data nil nil #'equal)
          (alist-get txt data nil nil #'equal)))
     (t nil))))

(defun arxana-browser-hypergraph--safe-list (value)
  (if (listp value) value nil))

(defun arxana-browser-hypergraph--read-json-file (path)
  (if (fboundp 'json-parse-string)
      (with-temp-buffer
        (insert-file-contents path)
        (json-parse-string (buffer-string)
                           :object-type 'alist
                           :array-type 'list
                           :null-object nil
                           :false-object nil))
    (let ((json-object-type 'alist)
          (json-array-type 'list)
          (json-false nil)
          (json-null nil))
      (json-read-file path))))

(defun arxana-browser-hypergraph--raw-json-text (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun arxana-browser-hypergraph--companion-path (path suffix)
  "Return a companion JSON PATH for SUFFIX or nil when unavailable."
  (when (and (stringp path)
             (string-match-p "-hypergraph\\.json\\'" path))
    (let ((candidate (replace-regexp-in-string
                      "-hypergraph\\.json\\'"
                      (format "-%s.json" suffix)
                      path)))
      (and (file-readable-p candidate) candidate))))

(defun arxana-browser-hypergraph--nodes (payload)
  (arxana-browser-hypergraph--safe-list
   (arxana-browser-hypergraph--get payload 'nodes)))

(defun arxana-browser-hypergraph--edges (payload)
  (or (arxana-browser-hypergraph--safe-list
       (arxana-browser-hypergraph--get payload 'edges))
      (arxana-browser-hypergraph--safe-list
       (arxana-browser-hypergraph--get payload 'hyperedges))
      '()))

(defun arxana-browser-hypergraph--histogram (items key)
  (let ((table (make-hash-table :test 'equal))
        rows)
    (dolist (item items)
      (let ((label (or (arxana-browser-hypergraph--as-string
                        (arxana-browser-hypergraph--get item key))
                       "")))
        (when (string-empty-p label)
          (setq label "unknown"))
        (puthash label (1+ (gethash label table 0)) table)))
    (maphash (lambda (name count)
               (push (cons name count) rows))
             table)
    (sort rows (lambda (a b)
                 (if (= (cdr a) (cdr b))
                     (string< (car a) (car b))
                   (> (cdr a) (cdr b)))))))

(defun arxana-browser-hypergraph--node-preview (node)
  (let* ((attrs (arxana-browser-hypergraph--get node 'attrs))
         (preview (or (arxana-browser-hypergraph--get attrs 'latex)
                      (arxana-browser-hypergraph--get attrs 'text)
                      (arxana-browser-hypergraph--get attrs 'match)))
         (text (string-trim (arxana-browser-hypergraph--as-string preview))))
    (if (string-empty-p text) "-" (truncate-string-to-width text 72 nil nil t))))

(defun arxana-browser-hypergraph--edge-preview (edge)
  (let ((ends (arxana-browser-hypergraph--safe-list
               (arxana-browser-hypergraph--get edge 'ends))))
    (if ends
        (truncate-string-to-width
         (mapconcat (lambda (end)
                      (arxana-browser-hypergraph--as-string end))
                    ends
                    ", ")
         72 nil nil t)
      "-")))

(defun arxana-browser-hypergraph--source-label (path)
  (let ((name (file-name-nondirectory path)))
    (if (string-empty-p name) path name)))

(defun arxana-browser-hypergraph--source-item (source)
  (let* ((path (expand-file-name source))
         (item (list :type 'hypergraph-source
                     :label (arxana-browser-hypergraph--source-label path)
                     :path path
                     :thread "-"
                     :nodes 0
                     :edges 0
                     :status "missing")))
    (if (not (file-readable-p path))
        item
      (condition-case err
          (let* ((payload (arxana-browser-hypergraph--read-json-file path))
                 (nodes (arxana-browser-hypergraph--nodes payload))
                 (edges (arxana-browser-hypergraph--edges payload))
                 (thread (arxana-browser-hypergraph--get payload 'thread_id)))
            (setq item (plist-put item :status "ok"))
            (setq item (plist-put item :thread
                                  (if thread
                                      (arxana-browser-hypergraph--as-string thread)
                                    "-")))
            (setq item (plist-put item :nodes (length nodes)))
            (setq item (plist-put item :edges (length edges)))
            (setq item (plist-put item :payload payload))
            item)
        (error
         (plist-put (plist-put item :status "error")
                    :error (error-message-string err)))))))

(defun arxana-browser-hypergraph-items ()
  "Return configured local hypergraph sources."
  (if (not (listp arxana-browser-hypergraph-sources))
      (list (list :type 'info
                  :label "Hypergraph sources missing"
                  :description "Set arxana-browser-hypergraph-sources to a list of JSON files."))
    (let ((items (mapcar #'arxana-browser-hypergraph--source-item
                         arxana-browser-hypergraph-sources)))
      (if items
          items
        (list (list :type 'info
                    :label "No hypergraph sources configured"
                    :description "Customize arxana-browser-hypergraph-sources."))))))

(defun arxana-browser-hypergraph-format ()
  [("Dataset" 30 t)
   ("Thread" 10 t)
   ("Nodes" 7 t)
   ("Edges" 7 t)
   ("Status" 10 t)
   ("Path" 0 t)])

(defun arxana-browser-hypergraph-row (item)
  (vector (or (plist-get item :label) "")
          (or (plist-get item :thread) "-")
          (number-to-string (or (plist-get item :nodes) 0))
          (number-to-string (or (plist-get item :edges) 0))
          (or (plist-get item :status) "")
          (or (plist-get item :path) "")))

(defun arxana-browser-hypergraph--insert-histogram (title rows)
  (insert (format "%s\n" title))
  (if rows
      (dolist (row rows)
        (insert (format "  %-24s %d\n" (car row) (cdr row))))
    (insert "  (none)\n"))
  (insert "\n"))

(defun arxana-browser-hypergraph--insert-previews (title rows render)
  (insert (format "%s\n" title))
  (if rows
      (let ((limit (max 1 arxana-browser-hypergraph-preview-limit))
            (count 0))
        (dolist (row rows)
          (when (< count limit)
            (insert (funcall render row))
            (setq count (1+ count)))))
    (insert "  (none)\n"))
  (insert "\n"))

(defun arxana-browser-hypergraph--hash-push (table key value)
  (puthash key (cons value (gethash key table)) table))

(defun arxana-browser-hypergraph--node-by-id (nodes)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (node nodes table)
      (puthash (arxana-browser-hypergraph--as-string
                (arxana-browser-hypergraph--get node 'id))
               node
               table))))

(defun arxana-browser-hypergraph--post-nodes (nodes)
  (let (posts)
    (dolist (node nodes (nreverse posts))
      (when (string= (arxana-browser-hypergraph--as-string
                      (arxana-browser-hypergraph--get node 'type))
                     "post")
        (push node posts)))))

(defun arxana-browser-hypergraph--post-rank (post)
  (let ((subtype (arxana-browser-hypergraph--as-string
                  (arxana-browser-hypergraph--get post 'subtype))))
    (cond
     ((string= subtype "question") 0)
     ((string= subtype "answer") 1)
     ((string= subtype "comment") 2)
     (t 3))))

(defun arxana-browser-hypergraph--bucket-by-post (edges node-by-id type)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (edge edges table)
      (when (string= (arxana-browser-hypergraph--as-string
                      (arxana-browser-hypergraph--get edge 'type))
                     type)
        (let ((ends (arxana-browser-hypergraph--safe-list
                     (arxana-browser-hypergraph--get edge 'ends))))
          (cond
           ((and (string= type "scope") (>= (length ends) 2))
            (let ((post-id (arxana-browser-hypergraph--as-string (nth 1 ends)))
                  (scope-id (arxana-browser-hypergraph--as-string (nth 0 ends))))
              (arxana-browser-hypergraph--hash-push
               table
               post-id
               (list :edge edge :node (gethash scope-id node-by-id)))))
           ((and (member type '("discourse" "categorical")) (>= (length ends) 1))
            (arxana-browser-hypergraph--hash-push
             table
             (arxana-browser-hypergraph--as-string (nth 0 ends))
             edge))
           ((and (string= type "surface") (>= (length ends) 2))
            (let ((post-id (arxana-browser-hypergraph--as-string (nth 1 ends)))
                  (expr-id (arxana-browser-hypergraph--as-string (nth 0 ends))))
              (arxana-browser-hypergraph--hash-push
               table
               post-id
               (list :edge edge :node (gethash expr-id node-by-id)))))
           ((and (string= type "mention") (>= (length ends) 2))
            (let ((post-id (arxana-browser-hypergraph--as-string (nth 0 ends)))
                  (node-id (arxana-browser-hypergraph--as-string (nth 1 ends))))
               (arxana-browser-hypergraph--hash-push
                table
                post-id
                (list :edge edge :node (gethash node-id node-by-id)))))))))))

(defun arxana-browser-hypergraph--iatc-buckets (edges)
  (let ((incoming (make-hash-table :test 'equal))
        (outgoing (make-hash-table :test 'equal)))
    (dolist (edge edges)
      (when (string= (arxana-browser-hypergraph--as-string
                      (arxana-browser-hypergraph--get edge 'type))
                     "iatc")
        (let ((ends (arxana-browser-hypergraph--safe-list
                     (arxana-browser-hypergraph--get edge 'ends))))
          (when (>= (length ends) 2)
            (let* ((src (arxana-browser-hypergraph--as-string (nth 0 ends)))
                   (dst (arxana-browser-hypergraph--as-string (nth 1 ends)))
                   (attrs (arxana-browser-hypergraph--get edge 'attrs))
                   (act (arxana-browser-hypergraph--as-string
                         (or (arxana-browser-hypergraph--get attrs 'act) "link")))
                   (row (list :src src :dst dst :act act :edge edge)))
              (arxana-browser-hypergraph--hash-push incoming dst row)
              (arxana-browser-hypergraph--hash-push outgoing src row))))))
    (list incoming outgoing)))

(defun arxana-browser-hypergraph--tail-token (value)
  (let* ((text (arxana-browser-hypergraph--as-string value))
         (parts (split-string text "/" t)))
    (if parts
        (car (last parts))
      text)))

(defun arxana-browser-hypergraph--tex-lite (value)
  (let ((text (arxana-browser-hypergraph--as-string value))
        (pairs '(("\\forall" . "∀")
                 ("\\exists" . "∃")
                 ("\\to" . "→")
                 ("\\rightarrow" . "→")
                 ("\\Rightarrow" . "⇒")
                 ("\\implies" . "⇒")
                 ("\\land" . "∧")
                 ("\\wedge" . "∧")
                 ("\\lor" . "∨")
                 ("\\vee" . "∨")
                 ("\\neg" . "¬")
                 ("\\in" . "∈")
                 ("\\subseteq" . "⊆")
                 ("\\subset" . "⊂")
                 ("\\cup" . "∪")
                 ("\\cap" . "∩")
                 ("\\times" . "×")
                 ("\\cdot" . "·")
                 ("\\leq" . "≤")
                 ("\\geq" . "≥")
                 ("\\neq" . "≠")
                 ("\\alpha" . "α")
                 ("\\beta" . "β")
                 ("\\gamma" . "γ")
                 ("\\delta" . "δ")
                 ("\\epsilon" . "ε")
                 ("\\lambda" . "λ")
                 ("\\mu" . "μ")
                 ("\\pi" . "π")
                 ("\\sigma" . "σ"))))
    (dolist (pair pairs)
      (setq text (string-replace (car pair) (cdr pair) text)))
    (setq text (string-replace "\\left" "" text))
    (setq text (string-replace "\\right" "" text))
    (setq text (replace-regexp-in-string "[{}]" "" text t))
    (setq text (replace-regexp-in-string "[[:space:]\n\r]+" " " text t))
    (string-trim text)))

(defun arxana-browser-hypergraph--scope-symbol (scope-node)
  (let* ((attrs (arxana-browser-hypergraph--get scope-node 'attrs))
         (ends (arxana-browser-hypergraph--safe-list
                (arxana-browser-hypergraph--get attrs 'ends)))
         (sym nil)
         (desc nil)
         (prm nil))
    (dolist (entry ends)
      (let ((role (arxana-browser-hypergraph--as-string
                   (arxana-browser-hypergraph--get entry 'role))))
        (cond
         ((and (not sym) (member role '("symbol" "variable")))
          (setq sym entry))
         ((and (not desc) (member role '("description" "domain")))
          (setq desc entry))
         ((and (not prm) (string= role "parameters"))
          (setq prm entry)))))
    (let ((text ""))
      (when sym
        (setq text (concat text
                           (or (arxana-browser-hypergraph--get sym 'latex)
                               (arxana-browser-hypergraph--get sym 'text)
                               ""))))
      (when desc
        (setq text (concat text
                           ": "
                           (or (arxana-browser-hypergraph--get desc 'latex)
                               (arxana-browser-hypergraph--get desc 'text)
                               ""))))
      (when prm
        (setq text (concat text
                           " ("
                           (arxana-browser-hypergraph--as-string
                            (arxana-browser-hypergraph--get prm 'text))
                           ")")))
      (string-trim text))))

(defun arxana-browser-hypergraph--take (items count)
  (let ((limit (max 0 count))
        (out nil)
        (index 0))
    (dolist (item items (nreverse out))
      (when (< index limit)
        (push item out)
        (setq index (1+ index))))))

(defun arxana-browser-hypergraph--insert-thread-lenses (nodes edges)
  (let* ((limit (max 1 arxana-browser-hypergraph-lens-item-limit))
         (node-by-id (arxana-browser-hypergraph--node-by-id nodes))
         (posts (arxana-browser-hypergraph--post-nodes nodes))
         (scope-by-post (arxana-browser-hypergraph--bucket-by-post edges node-by-id "scope"))
         (discourse-by-post (arxana-browser-hypergraph--bucket-by-post edges node-by-id "discourse"))
         (categorical-by-post (arxana-browser-hypergraph--bucket-by-post edges node-by-id "categorical"))
         (surface-by-post (arxana-browser-hypergraph--bucket-by-post edges node-by-id "surface"))
         (mention-by-post (arxana-browser-hypergraph--bucket-by-post edges node-by-id "mention"))
         (iatc-parts (arxana-browser-hypergraph--iatc-buckets edges))
         (iatc-incoming (nth 0 iatc-parts))
         (iatc-outgoing (nth 1 iatc-parts)))
    (insert "Thread Lenses (Emacs Native)\n")
    (if (not posts)
        (insert "  (no post nodes)\n\n")
      (setq posts (sort posts
                        (lambda (a b)
                          (let ((ra (arxana-browser-hypergraph--post-rank a))
                                (rb (arxana-browser-hypergraph--post-rank b)))
                            (if (= ra rb)
                                (string<
                                 (arxana-browser-hypergraph--as-string
                                  (arxana-browser-hypergraph--get a 'id))
                                 (arxana-browser-hypergraph--as-string
                                  (arxana-browser-hypergraph--get b 'id)))
                              (< ra rb))))))
      (dolist (post posts)
        (let* ((post-id (arxana-browser-hypergraph--as-string
                         (arxana-browser-hypergraph--get post 'id)))
               (subtype (arxana-browser-hypergraph--as-string
                         (or (arxana-browser-hypergraph--get post 'subtype)
                             "post")))
               (post-attrs (arxana-browser-hypergraph--get post 'attrs))
               (score (arxana-browser-hypergraph--get post-attrs 'score))
               (accepted (arxana-browser-hypergraph--get post-attrs 'is_accepted))
               (categories (reverse (gethash post-id categorical-by-post)))
               (scopes (arxana-browser-hypergraph--take
                        (reverse (gethash post-id scope-by-post))
                        limit))
               (discourses (arxana-browser-hypergraph--take
                            (reverse (gethash post-id discourse-by-post))
                            limit))
               (surfaces (arxana-browser-hypergraph--take
                          (reverse (gethash post-id surface-by-post))
                          limit))
               (mentions (arxana-browser-hypergraph--take
                          (reverse (gethash post-id mention-by-post))
                          limit))
               (incoming (arxana-browser-hypergraph--take
                          (reverse (gethash post-id iatc-incoming))
                          limit))
               (outgoing (arxana-browser-hypergraph--take
                          (reverse (gethash post-id iatc-outgoing))
                          limit)))
          (insert (format "  %s [%s]%s%s\n"
                          post-id
                          subtype
                          (if score (format " score=%s" score) "")
                          (if accepted " accepted" "")))
          (if categories
              (let (chunks)
                (dolist (edge categories)
                  (let* ((attrs (arxana-browser-hypergraph--get edge 'attrs))
                         (concept (string-remove-prefix
                                   "cat/"
                                   (arxana-browser-hypergraph--as-string
                                    (or (arxana-browser-hypergraph--get attrs 'concept)
                                        "cat"))))
                         (cscore (arxana-browser-hypergraph--get attrs 'score)))
                    (push (if cscore
                              (format "%s@%s" concept cscore)
                            concept)
                          chunks)))
                (insert (format "    categories: %s\n"
                                (mapconcat #'identity (nreverse chunks) ", "))))
            (insert "    categories: none\n"))
          (if scopes
              (dolist (row scopes)
                (let* ((edge (plist-get row :edge))
                       (scope-node (plist-get row :node))
                       (attrs (arxana-browser-hypergraph--get edge 'attrs))
                       (role (arxana-browser-hypergraph--tail-token
                              (or (arxana-browser-hypergraph--get attrs 'binding_type)
                                  (arxana-browser-hypergraph--get scope-node 'subtype)
                                  "scope")))
                       (symbol (or (arxana-browser-hypergraph--scope-symbol scope-node)
                                   (arxana-browser-hypergraph--as-string
                                    (arxana-browser-hypergraph--get
                                     (arxana-browser-hypergraph--get scope-node 'attrs)
                                     'match)))))
                  (insert (format "    scope %-10s %s\n" role symbol))))
            (insert "    scope: none\n"))
          (if discourses
              (dolist (edge discourses)
                (let* ((attrs (arxana-browser-hypergraph--get edge 'attrs))
                       (role (arxana-browser-hypergraph--as-string
                              (or (arxana-browser-hypergraph--get attrs 'role) "disc")))
                       (dtype (arxana-browser-hypergraph--tail-token
                               (arxana-browser-hypergraph--get attrs 'dtype)))
                       (match (arxana-browser-hypergraph--as-string
                               (arxana-browser-hypergraph--get attrs 'match))))
                  (insert (format "    discourse %-10s %s [%s]\n" role match dtype))))
            (insert "    discourse: none\n"))
          (if surfaces
              (let (exprs)
                (dolist (row surfaces)
                  (let* ((node (plist-get row :node))
                         (attrs (arxana-browser-hypergraph--get node 'attrs))
                         (latex (or (arxana-browser-hypergraph--get attrs 'latex)
                                    (arxana-browser-hypergraph--get node 'id)))
                         (sexp (arxana-browser-hypergraph--as-string
                                (arxana-browser-hypergraph--get attrs 'sexp)))
                         (rendered (arxana-browser-hypergraph--tex-lite latex)))
                    (push (if (string-empty-p sexp)
                              rendered
                            (format "%s [s-expr]" rendered))
                          exprs)))
                (insert (format "    expressions: %s\n"
                                (mapconcat #'identity (nreverse exprs) " | "))))
            (insert "    expressions: none\n"))
          (if mentions
              (let (mention-labels)
                (dolist (row mentions)
                  (let* ((node (plist-get row :node))
                         (attrs (arxana-browser-hypergraph--get node 'attrs))
                         (label (or (arxana-browser-hypergraph--get attrs 'name)
                                    (arxana-browser-hypergraph--get node 'subtype)
                                    (arxana-browser-hypergraph--get node 'id))))
                    (push (arxana-browser-hypergraph--as-string label) mention-labels)))
                (insert (format "    mentions: %s\n"
                                (mapconcat #'identity (nreverse mention-labels) ", "))))
            (insert "    mentions: none\n"))
          (if (or incoming outgoing)
              (progn
                (if incoming
                    (dolist (row incoming)
                      (insert (format "    iatc-in  %s -> %s (%s)\n"
                                      (plist-get row :src)
                                      post-id
                                      (arxana-browser-hypergraph--tail-token
                                       (plist-get row :act)))))
                  (insert "    iatc-in: none\n"))
                (if outgoing
                    (dolist (row outgoing)
                      (insert (format "    iatc-out %s -> %s (%s)\n"
                                      post-id
                                      (plist-get row :dst)
                                      (arxana-browser-hypergraph--tail-token
                                       (plist-get row :act)))))
                  (insert "    iatc-out: none\n")))
            (insert "    iatc: none\n"))
          (insert "\n"))))
    (insert "\n")))

(defun arxana-browser-hypergraph--point-prop (prop)
  (or (get-text-property (point) prop)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) prop))))

(defun arxana-browser-hypergraph-describe-at-point ()
  "Describe the hypergraph overlay at point."
  (interactive)
  (let ((kind (arxana-browser-hypergraph--point-prop 'arxana-hypergraph-kind)))
    (pcase kind
      ('expression
       (let ((latex (or (arxana-browser-hypergraph--point-prop 'arxana-hypergraph-latex) ""))
             (sexp (or (arxana-browser-hypergraph--point-prop 'arxana-hypergraph-sexp) "")))
         (if (string-empty-p sexp)
             (message "Expression: %s" latex)
           (message "Expression: %s | s-expr: %s" latex sexp))))
      ('mention
       (let ((label (or (arxana-browser-hypergraph--point-prop 'arxana-hypergraph-label) "")))
         (message "Mention: %s" label)))
      ('discourse
       (let ((match (or (arxana-browser-hypergraph--point-prop 'arxana-hypergraph-label) "")))
         (message "Discourse: %s" match)))
      ('scope
       (let ((role (or (arxana-browser-hypergraph--point-prop 'arxana-hypergraph-label) "")))
         (message "Scope opener: %s" role)))
      ('subexpr-operator
       (let ((token (or (arxana-browser-hypergraph--point-prop 'arxana-hypergraph-label) "")))
         (message "Operator token: %s (same symbol)" token)))
      ('subexpr-variable
       (let ((token (or (arxana-browser-hypergraph--point-prop 'arxana-hypergraph-label) "")))
         (message "Variable token: %s (similar syntactic role)" token)))
      (_
       (org-open-at-point)))))

(defun arxana-browser-hypergraph--clear-peer-overlays ()
  (when arxana-browser-hypergraph--peer-overlays
    (dolist (overlay arxana-browser-hypergraph--peer-overlays)
      (delete-overlay overlay))
    (setq arxana-browser-hypergraph--peer-overlays nil)))

(defun arxana-browser-hypergraph--highlight-peer-key (peer-key)
  (let ((pos (point-min)))
    (while (< pos (point-max))
      (let* ((next (or (next-single-property-change pos 'arxana-peer-key nil (point-max))
                       (point-max)))
             (value (get-text-property pos 'arxana-peer-key)))
        (when (and value (equal value peer-key) (< pos next))
          (let ((overlay (make-overlay pos next)))
            (overlay-put overlay 'face
                         (or (get-text-property pos 'arxana-peer-face)
                             'arxana-browser-hypergraph-peer-face))
            (overlay-put overlay 'priority 900)
            (push overlay arxana-browser-hypergraph--peer-overlays)))
        (setq pos (if (> next pos) next (1+ pos)))))))

(defun arxana-browser-hypergraph--peer-highlight-at-point ()
  (let ((peer-key (arxana-browser-hypergraph--point-prop 'arxana-peer-key)))
    (unless (equal peer-key arxana-browser-hypergraph--active-peer-key)
      (setq arxana-browser-hypergraph--active-peer-key peer-key)
      (arxana-browser-hypergraph--clear-peer-overlays)
      (when peer-key
        (arxana-browser-hypergraph--highlight-peer-key peer-key)))))

(defun arxana-browser-hypergraph--normalize-peer-token (value)
  (let* ((text (downcase (arxana-browser-hypergraph--as-string value)))
         (text (string-remove-prefix "$" text))
         (text (string-remove-suffix "$" text))
         (text (replace-regexp-in-string "[[:space:]\n\r]+" "" text)))
    (if (string-empty-p text) "-" text)))

(defun arxana-browser-hypergraph--decode-html-entities (text)
  (let ((pairs '(("&nbsp;" . " ")
                 ("&amp;" . "&")
                 ("&lt;" . "<")
                 ("&gt;" . ">")
                 ("&quot;" . "\"")
                 ("&#39;" . "'"))))
    (dolist (pair pairs text)
      (setq text (string-replace (car pair) (cdr pair) text)))))

(defun arxana-browser-hypergraph--html-to-text (html)
  (let ((text (arxana-browser-hypergraph--as-string html))
        (case-fold-search t))
    (setq text (replace-regexp-in-string "<\\s-*br\\s-*/?\\s-*>" "\n" text t))
    (setq text (replace-regexp-in-string "</\\s-*p\\s-*>" "\n\n" text t))
    (setq text (replace-regexp-in-string "<\\s-*li[^>]*>" "- " text t))
    (setq text (replace-regexp-in-string "</\\s-*li\\s-*>" "\n" text t))
    (setq text (replace-regexp-in-string "<[^>]+>" "" text t))
    (setq text (arxana-browser-hypergraph--decode-html-entities text))
    (setq text (replace-regexp-in-string "\n\\{3,\\}" "\n\n" text t))
    (string-trim text)))

(defun arxana-browser-hypergraph--raw-thread-answer-html (raw-thread answer-id)
  (let ((answers (arxana-browser-hypergraph--safe-list
                  (arxana-browser-hypergraph--get raw-thread 'answers)))
        (out nil))
    (dolist (answer answers out)
      (let ((id (arxana-browser-hypergraph--as-string
                 (arxana-browser-hypergraph--get answer 'id))))
        (when (and (not out) (string= id answer-id))
          (setq out (arxana-browser-hypergraph--as-string
                     (arxana-browser-hypergraph--get answer 'body_html))))))))

(defun arxana-browser-hypergraph--raw-thread-comment-text (raw-thread comment-id)
  (let ((comments-q (arxana-browser-hypergraph--safe-list
                     (arxana-browser-hypergraph--get raw-thread 'comments_q)))
        (comments-a (arxana-browser-hypergraph--get raw-thread 'comments_a))
        (out nil))
    (catch 'found
      (dolist (comment comments-q)
        (let ((id (arxana-browser-hypergraph--as-string
                   (arxana-browser-hypergraph--get comment 'id))))
          (when (string= id comment-id)
            (setq out (arxana-browser-hypergraph--as-string
                       (arxana-browser-hypergraph--get comment 'text)))
            (throw 'found out))))
      (when (listp comments-a)
        (dolist (bucket comments-a)
          (let ((rows (arxana-browser-hypergraph--safe-list (cdr-safe bucket))))
            (dolist (comment rows)
              (let ((id (arxana-browser-hypergraph--as-string
                         (arxana-browser-hypergraph--get comment 'id))))
                (when (string= id comment-id)
                  (setq out (arxana-browser-hypergraph--as-string
                             (arxana-browser-hypergraph--get comment 'text)))
                  (throw 'found out)))))))
      out)))

(defun arxana-browser-hypergraph--raw-thread-post-body (raw-thread post-id)
  (when raw-thread
    (cond
     ((string-prefix-p "q-" post-id)
      (arxana-browser-hypergraph--html-to-text
       (arxana-browser-hypergraph--get
        (arxana-browser-hypergraph--get raw-thread 'question)
        'body_html)))
     ((string-prefix-p "a-" post-id)
      (arxana-browser-hypergraph--html-to-text
       (arxana-browser-hypergraph--raw-thread-answer-html raw-thread
                                                          (string-remove-prefix "a-" post-id))))
     ((string-prefix-p "c-" post-id)
      (arxana-browser-hypergraph--as-string
       (arxana-browser-hypergraph--raw-thread-comment-text raw-thread
                                                           (string-remove-prefix "c-" post-id))))
     (t nil))))

(defun arxana-browser-hypergraph--post-face (subtype)
  (cond
   ((string= subtype "question") 'arxana-browser-hypergraph-question-face)
   ((string= subtype "answer") 'arxana-browser-hypergraph-answer-face)
   ((string= subtype "comment") 'arxana-browser-hypergraph-comment-face)
   (t 'org-level-1)))

(defun arxana-browser-hypergraph--post< (a b)
  (let* ((ra (arxana-browser-hypergraph--post-rank a))
         (rb (arxana-browser-hypergraph--post-rank b))
         (ia (arxana-browser-hypergraph--as-string
              (arxana-browser-hypergraph--get a 'id)))
         (ib (arxana-browser-hypergraph--as-string
              (arxana-browser-hypergraph--get b 'id))))
    (if (= ra rb)
        (string< ia ib)
      (< ra rb))))

(defun arxana-browser-hypergraph--ordered-posts (posts)
  (let ((question nil)
        (answers nil)
        (comments nil)
        (children (make-hash-table :test 'equal))
        (out nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (post posts)
      (let ((subtype (arxana-browser-hypergraph--as-string
                      (arxana-browser-hypergraph--get post 'subtype))))
        (cond
         ((string= subtype "question") (setq question post))
         ((string= subtype "answer") (push post answers))
         ((string= subtype "comment") (push post comments)))))
    (setq answers (sort answers #'arxana-browser-hypergraph--post<))
    (setq comments (sort comments #'arxana-browser-hypergraph--post<))
    (dolist (comment comments)
      (let* ((attrs (arxana-browser-hypergraph--get comment 'attrs))
             (parent (arxana-browser-hypergraph--as-string
                      (arxana-browser-hypergraph--get attrs 'parent)))
             (existing (gethash parent children)))
        (puthash parent (append existing (list comment)) children)))
    (when question
      (push question out)
      (puthash (arxana-browser-hypergraph--as-string
                (arxana-browser-hypergraph--get question 'id))
               t
               seen)
      (dolist (comment (gethash (arxana-browser-hypergraph--as-string
                                 (arxana-browser-hypergraph--get question 'id))
                                children))
        (push comment out)
        (puthash (arxana-browser-hypergraph--as-string
                  (arxana-browser-hypergraph--get comment 'id))
                 t
                 seen)))
    (dolist (answer answers)
      (let ((answer-id (arxana-browser-hypergraph--as-string
                        (arxana-browser-hypergraph--get answer 'id))))
        (push answer out)
        (puthash answer-id t seen)
        (dolist (comment (gethash answer-id children))
          (push comment out)
          (puthash (arxana-browser-hypergraph--as-string
                    (arxana-browser-hypergraph--get comment 'id))
                   t
                   seen))))
    (dolist (comment comments)
      (let ((comment-id (arxana-browser-hypergraph--as-string
                         (arxana-browser-hypergraph--get comment 'id))))
        (unless (gethash comment-id seen)
          (push comment out))))
    (nreverse out)))

(defun arxana-browser-hypergraph--mark-next (start end needle props cursor)
  (when (and (stringp needle) (not (string-empty-p needle)))
    (save-excursion
      (goto-char (or (gethash needle cursor) start))
      (let ((found nil)
            (simple (string-match-p "\\`[[:alnum:]_]+\\'" needle)))
        (while (and (not found) (search-forward needle end t))
          (let ((mstart (- (point) (length needle)))
                (mend (point)))
            (unless (or (text-property-not-all mstart mend 'arxana-hypergraph-kind nil)
                        (and simple
                             (let ((before (char-before mstart))
                                   (after (char-after mend)))
                               (or (and before (eq (char-syntax before) ?w))
                                   (and after (eq (char-syntax after) ?w))))))
              (add-text-properties mstart mend props)
              (setq found t)))
          (puthash needle (point) cursor))
        found))))

(defun arxana-browser-hypergraph--mark-all (start end needle props)
  (when (and (stringp needle) (not (string-empty-p needle)))
    (save-excursion
      (goto-char start)
      (let ((simple (string-match-p "\\`[[:alnum:]_]+\\'" needle)))
      (while (search-forward needle end t)
        (let ((mstart (- (point) (length needle)))
              (mend (point)))
          (unless (or (text-property-not-all mstart mend 'arxana-hypergraph-kind nil)
                      (and simple
                           (let ((before (char-before mstart))
                                 (after (char-after mend)))
                             (or (and before (eq (char-syntax before) ?w))
                                 (and after (eq (char-syntax after) ?w))))))
            (add-text-properties mstart mend props))))))))

(defun arxana-browser-hypergraph--expression-candidates (row)
  (let* ((edge (plist-get row :edge))
         (node (plist-get row :node))
         (edge-attrs (arxana-browser-hypergraph--get edge 'attrs))
         (node-attrs (arxana-browser-hypergraph--get node 'attrs))
         (surface (arxana-browser-hypergraph--as-string
                   (arxana-browser-hypergraph--get edge-attrs 'surface)))
         (latex (arxana-browser-hypergraph--as-string
                 (or (arxana-browser-hypergraph--get node-attrs 'latex)
                     (arxana-browser-hypergraph--get edge-attrs 'latex))))
         (out nil))
    (when (not (string-empty-p surface))
      (push surface out))
    (when (not (string-empty-p latex))
      (push latex out)
      (push (format "$%s$" latex) out)
      (push (format "$$%s$$" latex) out))
    (delete-dups (nreverse out))))

(defun arxana-browser-hypergraph--scope-opener-candidates (row)
  (let* ((edge (plist-get row :edge))
         (node (plist-get row :node))
         (attrs (arxana-browser-hypergraph--get edge 'attrs))
         (role (downcase
                (arxana-browser-hypergraph--as-string
                 (or (arxana-browser-hypergraph--get attrs 'binding_type)
                     (arxana-browser-hypergraph--get node 'subtype)
                     "scope"))))
         (match (arxana-browser-hypergraph--as-string
                 (arxana-browser-hypergraph--get
                  (arxana-browser-hypergraph--get node 'attrs)
                  'match))))
    (delete-dups
     (delq nil
           (append
            (cond
             ((string-match-p "let" role) '("Let" "let"))
             ((or (string-match-p "universal" role)
                  (string-match-p "forall" role))
              '("for all" "For all"))
             ((string-match-p "where" role) '("where" "Where"))
             (t nil))
            (when (not (string-empty-p match))
              (let ((token (car (split-string match "[[:space:]\n\r]+" t))))
                (when (and token (not (string-empty-p token)))
                  (list token)))))))))

(defun arxana-browser-hypergraph--math-letter-p (ch)
  (or (and (>= ch ?A) (<= ch ?Z))
      (and (>= ch ?a) (<= ch ?z))))

(defun arxana-browser-hypergraph--annotate-subexpressions-in-region (start end)
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\\\\[[:alpha:]]+" end t)
      (let* ((mstart (match-beginning 0))
             (mend (match-end 0))
             (token (buffer-substring-no-properties mstart mend)))
        (add-text-properties
         mstart mend
         (list 'arxana-hypergraph-kind 'subexpr-operator
               'arxana-hypergraph-id token
               'arxana-hypergraph-label token
               'arxana-peer-key (format "subexpr:op:%s" token)
               'arxana-peer-face 'arxana-browser-hypergraph-subexpr-operator-face
               'mouse-face 'highlight
               'help-echo (format "Operator token %s (same symbol)" token)
               'keymap arxana-browser-hypergraph-expression-map)))))
  (let ((pos start))
    (while (< pos end)
      (let ((ch (char-after pos)))
        (when (and ch
                   (arxana-browser-hypergraph--math-letter-p ch))
          (let* ((before (char-before pos))
                 (after (char-after (1+ pos)))
                 (prev-word (and before (eq (char-syntax before) ?w)))
                 (next-word (and after (eq (char-syntax after) ?w)))
                 (escaped (eq before ?\\)))
            (when (and (not prev-word)
                       (not next-word)
                       (not escaped))
              (let* ((token (buffer-substring-no-properties pos (1+ pos)))
                     (norm (downcase token)))
                (add-text-properties
                 pos (1+ pos)
                 (list 'arxana-hypergraph-kind 'subexpr-variable
                       'arxana-hypergraph-id token
                       'arxana-hypergraph-label token
                       'arxana-peer-key (format "subexpr:var:%s" norm)
                       'arxana-peer-face 'arxana-browser-hypergraph-subexpr-variable-face
                       'mouse-face 'highlight
                       'help-echo (format "Variable token %s (similar role)" token)
                       'keymap arxana-browser-hypergraph-expression-map)))))))
      (setq pos (1+ pos)))))

(defun arxana-browser-hypergraph--annotate-subexpressions-for-post (start end)
  (let ((pos start))
    (while (< pos end)
      (let* ((next (or (next-single-property-change pos 'arxana-hypergraph-kind nil end)
                       end))
             (kind (get-text-property pos 'arxana-hypergraph-kind)))
        (when (and (eq kind 'expression) (< pos next))
          (arxana-browser-hypergraph--annotate-subexpressions-in-region pos next))
        (setq pos (if (> next pos) next (1+ pos)))))))

(defun arxana-browser-hypergraph--annotate-post-region (start end post-id scope-by-post surface-by-post mention-by-post discourse-by-post)
  (let ((expr-cursor (make-hash-table :test 'equal))
        (scope-cursor (make-hash-table :test 'equal))
        (mention-cursor (make-hash-table :test 'equal))
        (disc-cursor (make-hash-table :test 'equal))
        (expr-latex-map (make-hash-table :test 'equal)))
    (dolist (row (or (gethash post-id surface-by-post) '()))
      (let* ((node (plist-get row :node))
             (attrs (arxana-browser-hypergraph--get node 'attrs))
             (node-id (arxana-browser-hypergraph--as-string
                       (arxana-browser-hypergraph--get node 'id)))
             (latex (arxana-browser-hypergraph--as-string
                     (or (arxana-browser-hypergraph--get attrs 'latex) node-id)))
             (sexp (arxana-browser-hypergraph--as-string
                    (arxana-browser-hypergraph--get attrs 'sexp)))
             (peer-key (format "expr:%s"
                               (arxana-browser-hypergraph--normalize-peer-token latex)))
             (props (list 'arxana-hypergraph-kind 'expression
                          'arxana-hypergraph-id node-id
                          'arxana-hypergraph-latex latex
                          'arxana-hypergraph-sexp sexp
                          'arxana-peer-key peer-key
                          'arxana-peer-face 'arxana-browser-hypergraph-peer-face
                          'face 'arxana-browser-hypergraph-expression-face
                          'font-lock-face 'arxana-browser-hypergraph-expression-face
                          'mouse-face 'highlight
                          'help-echo (if (string-empty-p sexp)
                                         (format "Expression: %s" latex)
                                       (format "Expression: %s\ns-expr: %s" latex sexp))
                          'keymap arxana-browser-hypergraph-expression-map))
             (found nil))
        (unless (or (string-empty-p latex)
                    (gethash latex expr-latex-map))
          (puthash latex props expr-latex-map))
        (dolist (needle (arxana-browser-hypergraph--expression-candidates row))
          (unless found
            (setq found (arxana-browser-hypergraph--mark-next
                         start end needle props expr-cursor))))))
    (maphash
     (lambda (latex props)
       (dolist (needle (delete-dups
                        (list latex
                              (format "$%s$" latex)
                              (format "$$%s$$" latex))))
         (arxana-browser-hypergraph--mark-all start end needle props)))
     expr-latex-map)
    (dolist (row (or (gethash post-id scope-by-post) '()))
      (let* ((edge (plist-get row :edge))
             (attrs (arxana-browser-hypergraph--get edge 'attrs))
             (role (arxana-browser-hypergraph--tail-token
                    (or (arxana-browser-hypergraph--get attrs 'binding_type)
                        "scope")))
             (props (list 'arxana-hypergraph-kind 'scope
                          'arxana-hypergraph-id role
                          'arxana-hypergraph-label role
                          'face 'arxana-browser-hypergraph-scope-face
                          'font-lock-face 'arxana-browser-hypergraph-scope-face
                          'mouse-face 'highlight
                          'help-echo (format "Scope opener: %s" role))))
        (dolist (needle (arxana-browser-hypergraph--scope-opener-candidates row))
          (arxana-browser-hypergraph--mark-next
           start end needle props scope-cursor))))
    (dolist (row (or (gethash post-id mention-by-post) '()))
      (let* ((edge (plist-get row :edge))
             (node (plist-get row :node))
             (edge-attrs (arxana-browser-hypergraph--get edge 'attrs))
             (node-attrs (arxana-browser-hypergraph--get node 'attrs))
             (surface (arxana-browser-hypergraph--as-string
                       (arxana-browser-hypergraph--get edge-attrs 'surface)))
             (term-id (arxana-browser-hypergraph--as-string
                       (arxana-browser-hypergraph--get node 'id)))
             (label (arxana-browser-hypergraph--as-string
                     (or (arxana-browser-hypergraph--get node-attrs 'name)
                         (arxana-browser-hypergraph--get node 'subtype)
                         term-id)))
             (peer-key (format "term:%s"
                               (arxana-browser-hypergraph--normalize-peer-token
                                (if (string-empty-p term-id) label term-id)))))
        (arxana-browser-hypergraph--mark-next
         start end surface
         (list 'arxana-hypergraph-kind 'mention
               'arxana-hypergraph-id term-id
               'arxana-hypergraph-label label
               'arxana-peer-key peer-key
               'arxana-peer-face 'arxana-browser-hypergraph-peer-face
               'face 'arxana-browser-hypergraph-mention-face
               'font-lock-face 'arxana-browser-hypergraph-mention-face
               'mouse-face 'highlight
               'help-echo (format "Mention: %s" label)
               'keymap arxana-browser-hypergraph-mention-map)
         mention-cursor)))
    (dolist (edge (or (gethash post-id discourse-by-post) '()))
      (let* ((attrs (arxana-browser-hypergraph--get edge 'attrs))
             (match (arxana-browser-hypergraph--as-string
                     (arxana-browser-hypergraph--get attrs 'match)))
             (role (arxana-browser-hypergraph--as-string
                    (arxana-browser-hypergraph--get attrs 'role))))
        (arxana-browser-hypergraph--mark-next
         start end match
         (list 'arxana-hypergraph-kind 'discourse
               'arxana-hypergraph-id role
               'arxana-hypergraph-label match
               'face 'arxana-browser-hypergraph-discourse-face
               'font-lock-face 'arxana-browser-hypergraph-discourse-face
               'help-echo (format "Discourse %s: %s" role match))
         disc-cursor)))
    (arxana-browser-hypergraph--annotate-subexpressions-for-post start end)))

(defun arxana-browser-hypergraph--insert-post-block (post raw-thread scope-by-post categorical-by-post surface-by-post mention-by-post discourse-by-post)
  (let* ((post-id (arxana-browser-hypergraph--as-string
                   (arxana-browser-hypergraph--get post 'id)))
         (subtype (arxana-browser-hypergraph--as-string
                   (or (arxana-browser-hypergraph--get post 'subtype) "post")))
         (attrs (arxana-browser-hypergraph--get post 'attrs))
         (score (arxana-browser-hypergraph--get attrs 'score))
         (accepted (arxana-browser-hypergraph--get attrs 'is_accepted))
         (parent (arxana-browser-hypergraph--as-string
                  (arxana-browser-hypergraph--get attrs 'parent)))
         (categories (or (gethash post-id categorical-by-post) '()))
         (scopes (or (gethash post-id scope-by-post) '()))
         (heading-start (point))
         (body (or (arxana-browser-hypergraph--raw-thread-post-body raw-thread post-id)
                   "(No post body available.)")))
    (insert (format "* %s [%s]%s%s\n"
                    post-id
                    subtype
                    (if score (format " score=%s" score) "")
                    (if accepted " accepted" "")))
    (add-text-properties heading-start (point)
                         (list 'face (arxana-browser-hypergraph--post-face subtype)))
    (unless (or (string-empty-p parent)
                (string= parent "null"))
      (insert (format "- parent :: %s\n" parent)))
    (if categories
        (let (labels)
          (dolist (edge categories)
            (let* ((cat-attrs (arxana-browser-hypergraph--get edge 'attrs))
                   (concept (arxana-browser-hypergraph--as-string
                             (or (arxana-browser-hypergraph--get cat-attrs 'concept)
                                 "cat"))))
              (push (string-remove-prefix "cat/" concept) labels)))
          (insert (format "- categories :: %s\n"
                          (mapconcat #'identity (nreverse labels) ", "))))
      (insert "- categories :: none\n"))
    (if scopes
        (dolist (row scopes)
          (let* ((edge (plist-get row :edge))
                 (scope-node (plist-get row :node))
                 (edge-attrs (arxana-browser-hypergraph--get edge 'attrs))
                 (role (arxana-browser-hypergraph--tail-token
                        (or (arxana-browser-hypergraph--get edge-attrs 'binding_type)
                            (arxana-browser-hypergraph--get scope-node 'subtype)
                            "scope")))
                 (symbol (or (arxana-browser-hypergraph--scope-symbol scope-node)
                             (arxana-browser-hypergraph--as-string
                              (arxana-browser-hypergraph--get
                               (arxana-browser-hypergraph--get scope-node 'attrs)
                               'match)))))
            (insert (format "- scope %s :: %s\n" role symbol))))
      (insert "- scope :: none\n"))
    (insert "\n")
    (let ((body-start (point)))
      (insert body "\n\n")
      (arxana-browser-hypergraph--annotate-post-region
       body-start (point) post-id scope-by-post
       surface-by-post mention-by-post discourse-by-post))))

(defun arxana-browser-hypergraph-open-arxana (item)
  "Open ITEM as an Arxana-native document buffer with overlays."
  (unless (eq (plist-get item :type) 'hypergraph-source)
    (user-error "Not a hypergraph source entry"))
  (let* ((path (plist-get item :path))
         (payload (or (plist-get item :payload)
                      (and (file-readable-p path)
                           (ignore-errors
                             (arxana-browser-hypergraph--read-json-file path)))))
         (raw-thread-path (arxana-browser-hypergraph--companion-path path "raw"))
         (raw-thread (and raw-thread-path
                          (ignore-errors
                            (arxana-browser-hypergraph--read-json-file raw-thread-path))))
         (nodes (arxana-browser-hypergraph--nodes payload))
         (edges (arxana-browser-hypergraph--edges payload))
         (thread (arxana-browser-hypergraph--as-string
                  (arxana-browser-hypergraph--get payload 'thread_id)))
         (node-by-id (arxana-browser-hypergraph--node-by-id nodes))
         (posts (arxana-browser-hypergraph--ordered-posts
                 (arxana-browser-hypergraph--post-nodes nodes)))
         (scope-by-post (arxana-browser-hypergraph--bucket-by-post edges node-by-id "scope"))
         (categorical-by-post (arxana-browser-hypergraph--bucket-by-post edges node-by-id "categorical"))
         (surface-by-post (arxana-browser-hypergraph--bucket-by-post edges node-by-id "surface"))
         (mention-by-post (arxana-browser-hypergraph--bucket-by-post edges node-by-id "mention"))
         (discourse-by-post (arxana-browser-hypergraph--bucket-by-post edges node-by-id "discourse"))
         (buf (get-buffer-create "*Arxana Hypergraph Glasses*")))
    (unless payload
      (user-error "Cannot read hypergraph payload: %s" (or path "<nil>")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (arxana-browser-hypergraph-glasses-mode)
        (insert (format "#+TITLE: Hypergraph Glasses — Thread %s\n\n"
                        (if (string-empty-p thread) "-" thread)))
        (insert (format "- Dataset :: %s\n" (or (plist-get item :label) "?")))
        (insert (format "- Path :: %s\n" (or path "-")))
        (insert (format "- Nodes :: %d\n" (length nodes)))
        (insert (format "- Edges :: %d\n" (length edges)))
        (if raw-thread
            (insert (format "- Companion raw :: %s\n\n" raw-thread-path))
          (insert "- Companion raw :: missing (-raw.json not found)\n\n"))
        (if posts
            (dolist (post posts)
              (arxana-browser-hypergraph--insert-post-block
               post raw-thread scope-by-post categorical-by-post
               surface-by-post mention-by-post discourse-by-post))
          (insert "* (no post nodes)\n"))
        (goto-char (point-min))
        (org-show-all)
        (visual-line-mode 1)
        (read-only-mode 1)
        (view-mode 1)))
    (display-buffer buf)))

(defun arxana-browser-hypergraph--html-document (item raw-json raw-thread-json)
  (let* ((label (arxana-browser-hypergraph--escape-html
                 (or (plist-get item :label) "hypergraph")))
         (path (arxana-browser-hypergraph--escape-html
                (or (plist-get item :path) "-")))
         (thread (arxana-browser-hypergraph--escape-html
                  (or (plist-get item :thread) "-"))))
    (concat
     "<!doctype html>\n"
     "<html lang=\"en\"><head><meta charset=\"utf-8\">"
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
     "<script>window.MathJax={tex:{inlineMath:[['$','$'],['\\\\(','\\\\)']],displayMath:[['$$','$$'],['\\\\[','\\\\]']]},options:{skipHtmlTags:['script','noscript','style','textarea','pre','code']},startup:{typeset:false}};</script>\n"
     "<script src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\" async></script>\n"
     "<title>Arxana Hypergraph Atlas</title>\n"
     "<style>\n"
     ":root{--bg:#f3f6fb;--ink:#0f172a;--muted:#475569;--card:#ffffff;--line:#cbd5e1;--accent:#0ea5e9}\n"
     "*{box-sizing:border-box}\n"
     "body{margin:0;font-family:\"Iosevka Aile\",\"IBM Plex Sans\",\"Segoe UI\",sans-serif;background:linear-gradient(155deg,#eef4ff 0,#f8fbff 55%,#e9f5ff 100%);color:var(--ink)}\n"
     ".page{max-width:1320px;margin:0 auto;padding:20px 18px 36px}\n"
     ".hero{background:var(--card);border:1px solid var(--line);border-radius:14px;padding:18px 20px 14px;box-shadow:0 10px 30px rgba(2,8,23,.08)}\n"
     ".title{font-size:1.3rem;font-weight:700;margin:0 0 4px}\n"
     ".meta{font-size:.82rem;color:var(--muted);display:flex;gap:14px;flex-wrap:wrap}\n"
     ".cards{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:10px;margin-top:14px}\n"
     ".card{background:#f8fbff;border:1px solid #dbe8f7;border-radius:10px;padding:10px 12px}\n"
     ".card .k{font-size:.68rem;color:#64748b;text-transform:uppercase;letter-spacing:.06em}\n"
     ".card .v{font-size:1.2rem;font-weight:700;margin-top:2px}\n"
     ".main{display:grid;grid-template-columns:1.2fr 1fr;gap:12px;margin-top:12px}\n"
     ".panel{background:var(--card);border:1px solid var(--line);border-radius:12px;padding:12px;box-shadow:0 8px 20px rgba(15,23,42,.06)}\n"
     ".panel h2{font-size:.9rem;margin:0 0 8px;text-transform:uppercase;letter-spacing:.06em;color:#334155}\n"
     ".controls{display:flex;gap:8px;flex-wrap:wrap;margin-bottom:8px}\n"
     "input[type='search']{flex:1;min-width:160px;padding:8px 10px;border:1px solid #c7d7eb;border-radius:8px;background:#f8fbff}\n"
     ".chips{display:flex;flex-wrap:wrap;gap:6px}\n"
     ".chip{font-size:.72rem;border:1px solid #c8daee;background:#f4f9ff;border-radius:999px;padding:3px 9px;cursor:pointer;color:#1e293b}\n"
     ".chip.active{background:#0ea5e9;color:white;border-color:#0284c7}\n"
     ".viz{border:1px solid #d8e3f1;border-radius:10px;background:linear-gradient(180deg,#fbfdff,#f4f8ff);height:440px;overflow:hidden}\n"
     ".viz svg{width:100%;height:100%;display:block}\n"
     ".edge{stroke:#94a3b8;stroke-width:1.2;opacity:.65}\n"
     ".edge.active{stroke:#f97316;stroke-width:2.1;opacity:.95}\n"
     ".node{stroke:#ffffff;stroke-width:1.8;cursor:pointer}\n"
     ".node.active{stroke:#0f172a;stroke-width:2.8}\n"
     ".node-label{font-size:10px;fill:#1e293b;pointer-events:none}\n"
     ".split{display:grid;grid-template-columns:1fr 1fr;gap:10px;margin-top:10px}\n"
     ".table-wrap{border:1px solid #d9e4f2;border-radius:10px;overflow:auto;max-height:260px;background:white}\n"
     "table{width:100%;border-collapse:collapse;font-size:.76rem}\n"
     "th{position:sticky;top:0;background:#eff6ff;padding:6px 8px;text-align:left;border-bottom:1px solid #d5e4f5}\n"
     "td{padding:6px 8px;border-bottom:1px solid #eef2f7;vertical-align:top}\n"
     "tr{cursor:pointer}\n"
     "tr:hover td{background:#f8fbff}\n"
     "tr.active td{background:#e0f2fe}\n"
     ".detail{margin-top:10px;border:1px solid #d9e4f2;border-radius:10px;background:#0f172a;color:#e2e8f0;overflow:auto;max-height:250px}\n"
     ".detail pre{margin:0;padding:10px 12px;font-size:.73rem;line-height:1.45;white-space:pre-wrap;word-break:break-word}\n"
     ".hint{font-size:.72rem;color:#64748b;margin-top:7px}\n"
     ".lenses{margin-top:12px}\n"
     ".lenses-grid{display:grid;grid-template-columns:1fr 1fr;gap:10px}\n"
     ".lens-card{background:#fff;border:1px solid #d9e4f2;border-radius:10px;padding:11px 12px;box-shadow:0 6px 18px rgba(15,23,42,.06)}\n"
     ".lens-card.active{outline:2px solid #0ea5e9}\n"
     ".post-question{border-left:4px solid #2563eb}\n"
     ".post-answer{border-left:4px solid #16a34a}\n"
     ".post-comment{border-left:4px solid #64748b}\n"
     ".lens-head{display:flex;align-items:center;gap:8px;flex-wrap:wrap}\n"
     ".post-id{font-size:.72rem;font-weight:700;padding:2px 7px;border-radius:3px;background:#0f172a;color:#fff}\n"
     ".post-kind{font-size:.67rem;text-transform:uppercase;letter-spacing:.04em;color:#475569}\n"
     ".kind-question{background:#dbeafe;color:#1d4ed8;border:1px solid #93c5fd;border-radius:999px;padding:2px 8px}\n"
     ".kind-answer{background:#dcfce7;color:#166534;border:1px solid #86efac;border-radius:999px;padding:2px 8px}\n"
     ".kind-comment{background:#f1f5f9;color:#334155;border:1px solid #cbd5e1;border-radius:999px;padding:2px 8px}\n"
     ".score{font-size:.72rem;color:#64748b}\n"
     ".badge{display:inline-block;font-size:.62rem;padding:2px 6px;border-radius:999px;border:1px solid #c7d2fe;background:#e0e7ff;color:#312e81;margin-right:4px}\n"
     ".badge.cat{border-color:#f59e0b;background:#fef3c7;color:#92400e}\n"
     ".badge.iatc{border-color:#d8b4fe;background:#f3e8ff;color:#6b21a8}\n"
     ".iatc-assert{border-color:#93c5fd;background:#dbeafe;color:#1e40af}\n"
     ".iatc-clarify{border-color:#d8b4fe;background:#f3e8ff;color:#7c3aed}\n"
     ".iatc-exemplify{border-color:#fcd34d;background:#fef3c7;color:#92400e}\n"
     ".iatc-reference{border-color:#a5b4fc;background:#e0e7ff;color:#4338ca}\n"
     ".iatc-challenge{border-color:#fca5a5;background:#fee2e2;color:#b91c1c}\n"
     ".iatc-query{border-color:#f9a8d4;background:#fce7f3;color:#be185d}\n"
     ".iatc-agree{border-color:#86efac;background:#dcfce7;color:#166534}\n"
     ".sec{margin-top:7px;padding-top:6px;border-top:1px dashed #dbe5f3}\n"
     ".sec h4{margin:0 0 4px;font-size:.68rem;letter-spacing:.04em;text-transform:uppercase;color:#475569}\n"
     ".scope-row{display:flex;gap:7px;align-items:flex-start;font-size:.73rem;margin-bottom:4px}\n"
     ".scope-role{font-size:.62rem;padding:1px 6px;border-radius:3px;background:#dbeafe;color:#1d4ed8;text-transform:uppercase;letter-spacing:.03em;white-space:nowrap}\n"
     ".scope-let{background:#dbeafe;color:#1e40af}\n"
     ".scope-universal{background:#f3e8ff;color:#7c3aed}\n"
     ".scope-where{background:#cffafe;color:#0f766e}\n"
     ".disc-item{font-size:.73rem;margin:0 0 4px}\n"
     ".disc-role{display:inline-block;font-size:.62rem;padding:1px 6px;border-radius:3px;margin-right:5px;text-transform:uppercase;letter-spacing:.03em}\n"
     ".disc-role-wire{background:#f3e8ff;color:#7c3aed;border:1px dotted #a855f7}\n"
     ".disc-role-label{background:#fff7ed;color:#c2410c;border:1px solid #f97316}\n"
     ".disc-role-port{background:#ccfbf1;color:#0f766e;border:1px double #14b8a6}\n"
     ".expr-chip{display:inline-flex;gap:7px;align-items:center;font-size:.72rem;font-family:\"Iosevka Aile\",\"IBM Plex Mono\",monospace;background:#ecfdf5;border:1px solid #86efac;color:#166534;border-radius:999px;padding:2px 8px;margin:0 6px 6px 0;cursor:pointer;max-width:100%}\n"
     ".expr-chip:hover{background:#dcfce7}\n"
     ".expr-tex{font-family:\"STIX Two Math\",\"Cambria Math\",\"Iosevka Aile\",serif;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;max-width:220px}\n"
     ".expr-sexp-mini{font-size:.63rem;color:#065f46;background:#d1fae5;border:1px solid #6ee7b7;border-radius:999px;padding:1px 6px;white-space:nowrap}\n"
     ".mention{display:inline-block;font-size:.68rem;background:#fff7ed;border:1px solid #fed7aa;color:#9a3412;border-radius:3px;padding:1px 5px;margin:0 4px 4px 0}\n"
     ".empty{font-size:.73rem;color:#64748b;font-style:italic}\n"
     "#sexp-tip{position:absolute;z-index:999;display:none;font-family:\"Iosevka Aile\",\"IBM Plex Mono\",monospace;font-size:.74rem;background:#0f172a;color:#e2e8f0;padding:8px 10px;border-radius:6px;max-width:540px;line-height:1.4;box-shadow:0 8px 24px rgba(2,6,23,.45)}\n"
     ".glasses{margin-top:12px}\n"
     ".glass-card{background:#fff;border:1px solid #d9e4f2;border-radius:10px;padding:14px 16px;margin-bottom:10px}\n"
     ".glass-question{border-left:4px solid #2563eb}\n"
     ".glass-answer{border-left:4px solid #16a34a}\n"
     ".glass-comment{border-left:4px solid #94a3b8;background:#f8fafc;padding:10px 12px}\n"
     ".glass-header{display:flex;align-items:center;gap:8px;flex-wrap:wrap;margin-bottom:8px}\n"
     ".glass-label{font-size:.66rem;font-weight:700;text-transform:uppercase;letter-spacing:.05em;padding:2px 8px;border-radius:999px}\n"
     ".glass-label.question{background:#dbeafe;color:#1d4ed8}\n"
     ".glass-label.answer{background:#dcfce7;color:#166534}\n"
     ".glass-label.comment{background:#e2e8f0;color:#334155}\n"
     ".glass-body p{margin:.52rem 0;font-size:.95rem;line-height:1.55}\n"
     ".glass-body blockquote{border-left:3px solid #c7d2fe;background:#f5f3ff;margin:.65rem 0;padding:6px 10px}\n"
     ".glass-meta{font-size:.72rem;color:#64748b}\n"
     ".glass-cats{margin:6px 0}\n"
     ".glass-cats .cat-badge{display:inline-block;font-size:.62rem;border-radius:999px;padding:2px 7px;margin-right:4px;border:1px solid #f59e0b;background:#fef3c7;color:#92400e;text-transform:uppercase;letter-spacing:.03em}\n"
     ".scope-map-inline{background:#eff6ff;border:1px solid #bfdbfe;border-radius:8px;padding:8px 10px;margin-top:8px}\n"
     ".scope-map-inline h4{font-size:.65rem;text-transform:uppercase;letter-spacing:.05em;color:#1d4ed8;margin:0 0 6px}\n"
     ".scope-mini{font-size:.74rem;margin-bottom:3px}\n"
     ".disc-mark{padding:0 2px;border-radius:2px}\n"
     ".disc-mark.disc-role-wire{background:#f3e8ff;border-bottom:2px dotted #a855f7}\n"
     ".disc-mark.disc-role-label{background:#fff7ed;border-bottom:2px solid #f97316}\n"
     ".disc-mark.disc-role-port{background:#ccfbf1;border-bottom:2px double #14b8a6}\n"
     ".term-mention{background:rgba(253,224,71,.28);border-bottom:1.5px solid #ca8a04;padding:0 1px;border-radius:2px}\n"
     ".expr-surface{background:rgba(34,197,94,.11);border:1px solid #86efac;border-radius:3px;padding:0 2px;cursor:pointer;transition:background .12s,border-color .12s}\n"
     ".expr-surface:hover{background:rgba(34,197,94,.2)}\n"
     ".expr-surface.peer,.term-mention.peer{outline:2px solid #0ea5e9;outline-offset:1px}\n"
     "@media (max-width:1080px){.main{grid-template-columns:1fr}.split{grid-template-columns:1fr}.cards{grid-template-columns:repeat(3,minmax(0,1fr))}.lenses-grid{grid-template-columns:1fr}}\n"
     "</style></head><body>\n"
     "<div class=\"page\">\n"
     "<section class=\"hero\">\n"
     "<h1 class=\"title\">Arxana Hypergraph Atlas</h1>\n"
     "<div class=\"meta\"><span><strong>Dataset:</strong> " label "</span><span><strong>Thread:</strong> " thread "</span><span><strong>Path:</strong> " path "</span></div>\n"
     "<div class=\"cards\" id=\"stats\"></div>\n"
     "</section>\n"
     "<section class=\"main\">\n"
     "<section class=\"panel\"><h2>Topology</h2><div class=\"controls\"><input id=\"search\" type=\"search\" placeholder=\"Filter by id, type, attrs...\"></div><div class=\"chips\" id=\"types\"></div><div class=\"viz\" id=\"viz\"></div><div class=\"hint\">Click nodes or edges to inspect structure.</div></section>\n"
     "<section class=\"panel\"><h2>Inspector</h2><div class=\"split\"><div><h2 style=\"margin:0 0 6px\">Nodes</h2><div class=\"table-wrap\"><table id=\"nodes-table\"><thead><tr><th>id</th><th>type</th><th>preview</th></tr></thead><tbody></tbody></table></div></div><div><h2 style=\"margin:0 0 6px\">Edges</h2><div class=\"table-wrap\"><table id=\"edges-table\"><thead><tr><th>type</th><th>ends</th></tr></thead><tbody></tbody></table></div></div></div><div class=\"detail\"><pre id=\"detail\">Select an item.</pre></div></section>\n"
     "</section>\n"
     "<section class=\"panel lenses\"><h2>Thread Lenses</h2><div class=\"hint\">Scope/discourse/categorical/iatc/expression layers over post nodes.</div><div class=\"lenses-grid\" id=\"lenses\"></div></section>\n"
     "<section class=\"panel glasses\"><h2>Hypergraph Glasses</h2><div class=\"hint\">Original thread content with hypergraph overlays. Hover expressions for s-expr and cross-highlight identical objects.</div><div id=\"glasses\"></div></section>\n"
     "</div>\n"
     "<div id=\"sexp-tip\"></div>\n"
     "<script>\n"
     "const RAW_JSON = " (json-encode raw-json) ";\n"
     "const RAW_THREAD_JSON = " (if raw-thread-json
                                   (json-encode raw-thread-json)
                                 "null") ";\n"
     "const DATA = JSON.parse(RAW_JSON);\n"
     "const THREAD = RAW_THREAD_JSON ? JSON.parse(RAW_THREAD_JSON) : null;\n"
     "const nodes = Array.isArray(DATA.nodes) ? DATA.nodes : [];\n"
     "const edges = Array.isArray(DATA.edges) ? DATA.edges : (Array.isArray(DATA.hyperedges) ? DATA.hyperedges : []);\n"
     "const nodeById = Object.fromEntries(nodes.map(n=>[String(n.id),n]));\n"
     "const postNodes = nodes.filter(n=>String(n.type)==='post');\n"
     "const palette = ['#0ea5e9','#22c55e','#eab308','#a855f7','#f97316','#ef4444','#14b8a6','#6366f1','#f43f5e','#10b981'];\n"
     "const state = { search: '', activeTypes: new Set(), selectedNode: null, selectedEdge: null };\n"
     "function h(s){let x=0;const t=String(s||'');for(let i=0;i<t.length;i++)x=((x<<5)-x)+t.charCodeAt(i);return Math.abs(x)}\n"
     "function colorFor(kind){return palette[h(kind)%palette.length]}\n"
     "function text(v){if(v==null)return '';if(typeof v==='string')return v;return JSON.stringify(v)}\n"
     "function esc(v){return text(v).replaceAll('&','&amp;').replaceAll('<','&lt;').replaceAll('>','&gt;').replaceAll('\"','&quot;')}\n"
     "function preview(n){const a=n&&n.attrs?n.attrs:{};return text(a.latex||a.text||a.match||a.surface||a).slice(0,86)}\n"
     "function nodeTypes(){const m={};nodes.forEach(n=>{const t=text(n.type||'unknown')||'unknown';m[t]=(m[t]||0)+1});return Object.entries(m).sort((a,b)=>b[1]-a[1])}\n"
     "function matchesNode(n){const t=text(n.type||'unknown');if(state.activeTypes.size && !state.activeTypes.has(t))return false;const q=state.search.trim().toLowerCase();if(!q)return true;const blob=(text(n.id)+' '+t+' '+text(n.subtype)+' '+text(n.attrs)).toLowerCase();return blob.includes(q)}\n"
     "function filtered(){const fn=nodes.filter(matchesNode);const ids=new Set(fn.map(n=>n.id));const fe=edges.filter(e=>{const ends=Array.isArray(e.ends)?e.ends:[];return ends.some(id=>ids.has(id))});return {fn,fe,ids}}\n"
     "function renderStats(fn,fe){const byType=nodeTypes();const html=[['nodes',fn.length],['edges',fe.length],['node types',byType.length]].map(c=>`<div class='card'><div class='k'>${c[0]}</div><div class='v'>${c[1]}</div></div>`).join('');document.getElementById('stats').innerHTML=html}\n"
     "function renderTypeChips(){const box=document.getElementById('types');box.innerHTML='';nodeTypes().forEach(([t,c])=>{const b=document.createElement('button');b.className='chip'+(state.activeTypes.has(t)?' active':'');b.textContent=`${t} (${c})`;b.onclick=()=>{if(state.activeTypes.has(t))state.activeTypes.delete(t);else state.activeTypes.add(t);render()};box.appendChild(b)})}\n"
     "function layout(fn){const groups={};fn.forEach(n=>{const t=text(n.type||'unknown')||'unknown';(groups[t]=groups[t]||[]).push(n)});const types=Object.keys(groups);const out={};const cx=430,cy=220,base=70,step=52;types.forEach((t,ti)=>{const arr=groups[t];const r=base+ti*step;arr.forEach((n,ni)=>{const a=((Math.PI*2)*ni/Math.max(arr.length,1))+(ti*0.37);out[n.id]={x:cx+Math.cos(a)*r,y:cy+Math.sin(a)*r,t}})});return out}\n"
     "function renderGraph(fn,fe){const pos=layout(fn);const svgNS='http://www.w3.org/2000/svg';const root=document.getElementById('viz');root.innerHTML='';const svg=document.createElementNS(svgNS,'svg');svg.setAttribute('viewBox','0 0 860 440');root.appendChild(svg);fe.forEach((e,i)=>{const ends=Array.isArray(e.ends)?e.ends:[];if(ends.length<2)return;const a=pos[ends[0]],b=pos[ends[1]];if(!a||!b)return;const line=document.createElementNS(svgNS,'line');line.setAttribute('x1',a.x);line.setAttribute('y1',a.y);line.setAttribute('x2',b.x);line.setAttribute('y2',b.y);line.setAttribute('class','edge'+(state.selectedEdge===i?' active':''));line.setAttribute('stroke',colorFor(e.type||'edge'));line.addEventListener('click',()=>{state.selectedEdge=i;state.selectedNode=null;showDetail('edge',e);render()});svg.appendChild(line)});fn.forEach(n=>{const p=pos[n.id];if(!p)return;const g=document.createElementNS(svgNS,'g');const c=document.createElementNS(svgNS,'circle');c.setAttribute('cx',p.x);c.setAttribute('cy',p.y);c.setAttribute('r',8);c.setAttribute('fill',colorFor(n.type||'unknown'));c.setAttribute('class','node'+(state.selectedNode===n.id?' active':''));c.addEventListener('click',()=>{state.selectedNode=n.id;state.selectedEdge=null;showDetail('node',n);render()});const l=document.createElementNS(svgNS,'text');l.setAttribute('x',p.x+10);l.setAttribute('y',p.y+3);l.setAttribute('class','node-label');l.textContent=text(n.id).slice(0,24);g.appendChild(c);g.appendChild(l);svg.appendChild(g)})}\n"
     "function renderNodesTable(fn){const body=document.querySelector('#nodes-table tbody');body.innerHTML='';fn.forEach(n=>{const tr=document.createElement('tr');if(state.selectedNode===n.id)tr.classList.add('active');tr.innerHTML=`<td>${text(n.id)}</td><td>${text(n.type||'')}</td><td>${preview(n)}</td>`;tr.onclick=()=>{state.selectedNode=n.id;state.selectedEdge=null;showDetail('node',n);render()};body.appendChild(tr)})}\n"
     "function renderEdgesTable(fe){const body=document.querySelector('#edges-table tbody');body.innerHTML='';fe.forEach((e,i)=>{const tr=document.createElement('tr');if(state.selectedEdge===i)tr.classList.add('active');const ends=Array.isArray(e.ends)?e.ends:[];tr.innerHTML=`<td>${text(e.type||'edge')}</td><td>${ends.map(text).join(' , ')}</td>`;tr.onclick=()=>{state.selectedEdge=i;state.selectedNode=null;showDetail('edge',e);render()};body.appendChild(tr)})}\n"
     "function showDetail(kind,obj){document.getElementById('detail').textContent=JSON.stringify({kind,data:obj},null,2)}\n"
     "function bucketBy(type){const out=new Map();edges.filter(e=>String(e.type)===type).forEach(e=>{const ends=Array.isArray(e.ends)?e.ends:[];if(type==='scope'&&ends.length>=2){const post=String(ends[1]);const arr=out.get(post)||[];arr.push({edge:e,node:nodeById[String(ends[0])]});out.set(post,arr)}else if((type==='discourse'||type==='categorical')&&ends.length>=1){const post=String(ends[0]);const arr=out.get(post)||[];arr.push(e);out.set(post,arr)}else if(type==='surface'&&ends.length>=2){const post=String(ends[1]);const arr=out.get(post)||[];arr.push({edge:e,node:nodeById[String(ends[0])]});out.set(post,arr)}else if(type==='mention'&&ends.length>=2){const post=String(ends[0]);const arr=out.get(post)||[];arr.push({edge:e,node:nodeById[String(ends[1])]});out.set(post,arr)}});return out}\n"
     "const scopeByPost=bucketBy('scope');\n"
     "const discourseByPost=bucketBy('discourse');\n"
     "const categoricalByPost=bucketBy('categorical');\n"
     "const surfaceByPost=bucketBy('surface');\n"
     "const mentionByPost=bucketBy('mention');\n"
     "const iatcEdges=edges.filter(e=>String(e.type)==='iatc').map(e=>({edge:e,src:String((e.ends||[])[0]||''),dst:String((e.ends||[])[1]||''),act:text(e.attrs&&e.attrs.act||'link')}));\n"
     "const iatcClassByAct={assert:'iatc-assert',clarify:'iatc-clarify',exemplify:'iatc-exemplify',reference:'iatc-reference',challenge:'iatc-challenge',query:'iatc-query',agree:'iatc-agree'};\n"
     "function postRank(p){const s=text(p.subtype||'');if(s==='question')return 0;if(s==='answer')return 1;if(s==='comment')return 2;return 3}\n"
     "function postClass(p){const s=text(p.subtype||'');if(s==='question')return 'post-question';if(s==='answer')return 'post-answer';if(s==='comment')return 'post-comment';return ''}\n"
     "function postKindClass(p){const s=text(p.subtype||'').toLowerCase();if(s==='question')return 'kind-question';if(s==='answer')return 'kind-answer';if(s==='comment')return 'kind-comment';return ''}\n"
     "function scopeSymbol(row){const ends=((row.node||{}).attrs||{}).ends||[];const sym=ends.find(x=>x.role==='symbol'||x.role==='variable');const desc=ends.find(x=>x.role==='description'||x.role==='domain');const prm=ends.find(x=>x.role==='parameters');const a=[];if(sym)a.push(sym.latex||sym.text||'');if(desc)a.push(': '+(desc.latex||desc.text||''));if(prm)a.push(' ('+(prm.text||'')+')');return a.join('')}\n"
     "function texLite(raw){let out=text(raw||'');const pairs=[['\\\\forall','∀'],['\\\\exists','∃'],['\\\\to','→'],['\\\\rightarrow','→'],['\\\\Rightarrow','⇒'],['\\\\implies','⇒'],['\\\\land','∧'],['\\\\wedge','∧'],['\\\\lor','∨'],['\\\\vee','∨'],['\\\\neg','¬'],['\\\\in','∈'],['\\\\subseteq','⊆'],['\\\\subset','⊂'],['\\\\cup','∪'],['\\\\cap','∩'],['\\\\times','×'],['\\\\cdot','·'],['\\\\leq','≤'],['\\\\geq','≥'],['\\\\neq','≠'],['\\\\alpha','α'],['\\\\beta','β'],['\\\\gamma','γ'],['\\\\delta','δ'],['\\\\epsilon','ε'],['\\\\lambda','λ'],['\\\\mu','μ'],['\\\\pi','π'],['\\\\sigma','σ']];pairs.forEach(([src,dst])=>{out=out.split(src).join(dst)});out=out.split('\\\\left').join('');out=out.split('\\\\right').join('');out=out.replace(/[{}]/g,'');out=out.replace(/\\s+/g,' ').trim();return out}\n"
     "function scopeRoleClass(row){const raw=text((row.edge&&row.edge.attrs&&row.edge.attrs.binding_type)||(row.node&&row.node.subtype)||'').toLowerCase();if(raw.includes('let'))return 'scope-let';if(raw.includes('universal')||raw.includes('forall'))return 'scope-universal';if(raw.includes('where'))return 'scope-where';return ''}\n"
     "function discourseRoleClass(edge){const role=text(edge&&edge.attrs&&edge.attrs.role||'').toLowerCase();if(role.includes('wire'))return 'disc-role-wire';if(role.includes('label'))return 'disc-role-label';if(role.includes('port'))return 'disc-role-port';return ''}\n"
     "function iatcActClass(act){const key=text(act||'').toLowerCase().replace(/[^a-z0-9]+/g,'-').replace(/^-+|-+$/g,'');return iatcClassByAct[key]||''}\n"
     "function renderExpressionChip(surface){const n=surface.node||{};const a=n.attrs||{};const lx=text(a.latex||n.id||'expr');const sx=text(a.sexp||'');const shown=texLite(lx)||lx;const sexpPill=sx?\"<span class='expr-sexp-mini'>s-expr</span>\":'';return `<span class='expr-chip' data-sexp='${esc(sx)}'><span class='expr-tex'>${esc(shown)}</span>${sexpPill}</span>`}\n"
     "function renderLenses(){const q=state.search.trim().toLowerCase();const posts=[...postNodes].sort((a,b)=>postRank(a)-postRank(b)||text(a.id).localeCompare(text(b.id)));let html='';posts.forEach(p=>{const pid=text(p.id);const attrs=p.attrs||{};const blob=(pid+' '+text(p.subtype)+' '+text(attrs)).toLowerCase();if(q&&!blob.includes(q))return;const scopes=scopeByPost.get(pid)||[];const disc=discourseByPost.get(pid)||[];const cats=categoricalByPost.get(pid)||[];const surfs=surfaceByPost.get(pid)||[];const ments=mentionByPost.get(pid)||[];const incoming=iatcEdges.filter(x=>x.dst===pid);const outgoing=iatcEdges.filter(x=>x.src===pid);const score=(attrs.score!=null)?`<span class='score'>score ${esc(attrs.score)}</span>`:'';const accepted=(attrs.is_accepted)?`<span class='badge'>accepted</span>`:'';const catHtml=cats.map(c=>`<span class='badge cat'>${esc((c.attrs&&c.attrs.concept||'cat').replace('cat/',''))} ${(c.attrs&&c.attrs.score!=null)?esc(c.attrs.score):''}</span>`).join('');const scopeHtml=scopes.length?scopes.map(s=>`<div class='scope-row'><span class='scope-role ${scopeRoleClass(s)}'>${esc((s.edge.attrs&&s.edge.attrs.binding_type||s.node&&s.node.subtype||'scope').split('/').pop())}</span><span>${esc(scopeSymbol(s)||((s.node&&s.node.attrs&&s.node.attrs.match)||''))}</span></div>`).join(''):`<div class='empty'>none</div>`;const discHtml=disc.length?disc.map(d=>`<div class='disc-item'><span class='disc-role ${discourseRoleClass(d)}'>${esc((d.attrs&&d.attrs.role)||'disc')}</span> <span>${esc((d.attrs&&d.attrs.match)||'')}</span> <span class='score'>${esc(((d.attrs&&d.attrs.dtype)||'').split('/').pop())}</span></div>`).join(''):`<div class='empty'>none</div>`;const exprHtml=surfs.length?surfs.slice(0,28).map(renderExpressionChip).join(''):`<div class='empty'>none</div>`;const mentionHtml=ments.length?ments.slice(0,22).map(m=>{const n=m.node||{};const a=n.attrs||{};const label=text((a.name)||(n.subtype)||(n.id));return `<span class='mention'>${esc(label)}</span>`}).join(''):`<div class='empty'>none</div>`;const iatcHtml=(incoming.length||outgoing.length)?`<div>${incoming.slice(0,8).map(x=>`<span class='badge iatc ${iatcActClass(x.act)}'>${esc(x.src)} → ${esc(pid)} : ${esc(x.act)}</span>`).join('')}</div><div style='margin-top:4px'>${outgoing.slice(0,8).map(x=>`<span class='badge iatc ${iatcActClass(x.act)}'>${esc(pid)} → ${esc(x.dst)} : ${esc(x.act)}</span>`).join('')}</div>`:`<div class='empty'>none</div>`;html+=`<article class='lens-card ${postClass(p)} ${(state.selectedNode===pid)?'active':''}'><div class='lens-head'><span class='post-id' data-focus-node='${esc(pid)}'>${esc(pid)}</span><span class='post-kind ${postKindClass(p)}'>${esc(p.subtype||'post')}</span>${score}${accepted}</div>${catHtml?`<div class='sec'>${catHtml}</div>`:''}<div class='sec'><h4>Scope Map</h4>${scopeHtml}</div><div class='sec'><h4>Discourse</h4>${discHtml}</div><div class='sec'><h4>Expression Surfaces</h4>${exprHtml}</div><div class='sec'><h4>Mentions</h4>${mentionHtml}</div><div class='sec'><h4>IATC</h4>${iatcHtml}</div></article>`});document.getElementById('lenses').innerHTML=html||\"<div class='empty'>No posts match the current filter.</div>\";document.querySelectorAll('[data-focus-node]').forEach(el=>el.addEventListener('click',()=>{state.selectedNode=el.getAttribute('data-focus-node');state.selectedEdge=null;showDetail('node',nodeById[state.selectedNode]||{id:state.selectedNode});render()}));attachSexpTips()}\n"
     "const threadCommentMap=(()=>{const out={};if(!THREAD)return out;(THREAD.comments_q||[]).forEach(c=>{out['c-'+text(c.id)]=c});Object.values(THREAD.comments_a||{}).forEach(arr=>{(arr||[]).forEach(c=>{out['c-'+text(c.id)]=c})});return out})();\n"
     "function normKey(v){return text(v).toLowerCase().replace(/\\s+/g,' ').trim()}\n"
     "function commentChildren(parentId){return postNodes.filter(n=>text(n.subtype)==='comment'&&text(n.attrs&&n.attrs.parent)==text(parentId)).sort((a,b)=>text(a.id).localeCompare(text(b.id)))}\n"
     "function threadBodyHtml(postId){if(!THREAD)return '';if(postId.startsWith('q-'))return text(THREAD.question&&THREAD.question.body_html||'');if(postId.startsWith('a-')){const id=Number(postId.slice(2));const row=(THREAD.answers||[]).find(a=>Number(a.id)===id);return text(row&&row.body_html||'')}if(postId.startsWith('c-')){const c=threadCommentMap[postId];return c?`<p>${esc(c.text||'')}</p>`:''}return ''}\n"
     "function replaceFirstText(root,needle,mk){const q=text(needle);if(!q)return false;const ql=q.toLowerCase();const walker=document.createTreeWalker(root,NodeFilter.SHOW_TEXT);let n;while((n=walker.nextNode())){const p=n.parentElement;if(!p)continue;if(p.closest('script,style,code,pre,.expr-surface'))continue;const src=n.nodeValue||'';const low=src.toLowerCase();const idx=low.indexOf(ql);if(idx<0)continue;const frag=document.createDocumentFragment();if(idx>0)frag.appendChild(document.createTextNode(src.slice(0,idx)));frag.appendChild(mk(src.slice(idx,idx+q.length)));if(idx+q.length<src.length)frag.appendChild(document.createTextNode(src.slice(idx+q.length)));n.parentNode.replaceChild(frag,n);return true}return false}\n"
     "function wrapMathSurfaces(root,postId){const surfaces=(surfaceByPost.get(postId)||[]).slice().sort((a,b)=>(Number(a.edge&&a.edge.attrs&&a.edge.attrs.position||0)-Number(b.edge&&b.edge.attrs&&b.edge.attrs.position||0)));let i=0;const textNodes=[];const walker=document.createTreeWalker(root,NodeFilter.SHOW_TEXT);let n;while((n=walker.nextNode()))textNodes.push(n);textNodes.forEach(node=>{const src=node.nodeValue||'';if(src.indexOf('$')<0)return;const rx=/\\$\\$[^$]+\\$\\$|\\$[^$]+\\$/g;let m,last=0,changed=false;const frag=document.createDocumentFragment();while((m=rx.exec(src))!==null){changed=true;if(m.index>last)frag.appendChild(document.createTextNode(src.slice(last,m.index)));const token=m[0];const span=document.createElement('span');span.className='expr-surface';const row=surfaces[i++]||null;if(row&&row.node){const a=row.node.attrs||{};const latex=text(a.latex||token.replace(/^\\$+|\\$+$/g,''));span.dataset.exprId=text(row.node.id||'');span.dataset.eqKey=normKey(latex);span.dataset.sexp=text(a.sexp||'');span.dataset.latex=latex}span.textContent=token;frag.appendChild(span);last=rx.lastIndex}if(!changed)return;if(last<src.length)frag.appendChild(document.createTextNode(src.slice(last)));node.parentNode.replaceChild(frag,node)})}\n"
     "function applyMentionMarks(root,postId){(mentionByPost.get(postId)||[]).forEach(row=>{const surface=text(row.edge&&row.edge.attrs&&row.edge.attrs.surface||'');if(!surface)return;const termId=text(row.node&&row.node.id||'');replaceFirstText(root,surface,(hit)=>{const s=document.createElement('span');s.className='term-mention';s.dataset.termId=termId;s.textContent=hit;return s})})}\n"
     "function applyDiscourseMarks(root,postId){(discourseByPost.get(postId)||[]).forEach(edge=>{const a=edge.attrs||{};const match=text(a.match||'');if(!match)return;const roleCls=discourseRoleClass(edge);replaceFirstText(root,match,(hit)=>{const s=document.createElement('span');s.className=`disc-mark ${roleCls}`;s.textContent=hit;return s})})}\n"
     "function scopePanel(postId){const scopes=scopeByPost.get(postId)||[];if(!scopes.length)return '';const rows=scopes.map(s=>{const label=esc((s.edge.attrs&&s.edge.attrs.binding_type||s.node&&s.node.subtype||'scope').split('/').pop());const body=esc(scopeSymbol(s)||((s.node&&s.node.attrs&&s.node.attrs.match)||''));return `<div class='scope-mini'><strong>${label}</strong> ${body}</div>`}).join('');return `<div class='scope-map-inline'><h4>Scope Map</h4>${rows}</div>`}\n"
     "function categoryPanel(postId){const cats=categoricalByPost.get(postId)||[];if(!cats.length)return '';return `<div class='glass-cats'>${cats.map(c=>`<span class='cat-badge'>${esc((c.attrs&&c.attrs.concept||'cat').replace('cat/',''))}</span>`).join('')}</div>`}\n"
     "function annotatePostBody(postId){const raw=threadBodyHtml(postId);if(!raw)return '';const host=document.createElement('div');host.innerHTML=raw;wrapMathSurfaces(host,postId);applyMentionMarks(host,postId);applyDiscourseMarks(host,postId);return host.innerHTML}\n"
     "function renderGlassCard(postNode){const pid=text(postNode.id);const subtype=text(postNode.subtype||'post');const attrs=postNode.attrs||{};const score=(attrs.score!=null)?`<span class='glass-meta'>score ${esc(attrs.score)}</span>`:'';const accepted=attrs.is_accepted?`<span class='glass-meta'>accepted</span>`:'';const labelClass=subtype==='question'?'question':(subtype==='answer'?'answer':'comment');const body=annotatePostBody(pid)||`<p class='empty'>No raw thread body found for ${esc(pid)}</p>`;return `<article class='glass-card glass-${labelClass}'><div class='glass-header'><span class='glass-label ${labelClass}'>${esc(subtype||'post')}</span><span class='post-id' data-focus-node='${esc(pid)}'>${esc(pid)}</span>${score}${accepted}</div>${categoryPanel(pid)}<div class='glass-body'>${body}</div>${scopePanel(pid)}</article>`}\n"
     "function bindGlassesInteractions(){document.querySelectorAll('.expr-surface').forEach(el=>{el.onmouseenter=()=>{const expr=el.dataset.exprId||'';const eq=el.dataset.eqKey||'';document.querySelectorAll('.expr-surface').forEach(x=>{if((expr&&x.dataset.exprId===expr)||(eq&&x.dataset.eqKey===eq))x.classList.add('peer')});const sexp=el.dataset.sexp||'';if(sexp){sexpTip.textContent=sexp;sexpTip.style.display='block';const r=el.getBoundingClientRect();let left=r.left+window.scrollX;const maxLeft=window.scrollX+window.innerWidth-sexpTip.offsetWidth-10;left=Math.max(window.scrollX+8,Math.min(left,maxLeft));sexpTip.style.left=left+'px';sexpTip.style.top=(r.bottom+window.scrollY+6)+'px'}};el.onmouseleave=()=>{document.querySelectorAll('.expr-surface.peer').forEach(x=>x.classList.remove('peer'));sexpTip.style.display='none'}});document.querySelectorAll('.term-mention').forEach(el=>{el.onmouseenter=()=>{const term=el.dataset.termId||'';if(!term)return;document.querySelectorAll('.term-mention').forEach(x=>{if(x.dataset.termId===term)x.classList.add('peer')})};el.onmouseleave=()=>{document.querySelectorAll('.term-mention.peer').forEach(x=>x.classList.remove('peer'))}})}\n"
     "function renderGlasses(){const root=document.getElementById('glasses');if(!root)return;if(!THREAD){root.innerHTML=\"<div class='empty'>No companion raw thread JSON found. Expected a sibling file ending in -raw.json.</div>\";return}const question=postNodes.find(n=>text(n.subtype)==='question');const answers=postNodes.filter(n=>text(n.subtype)==='answer').sort((a,b)=>text(a.id).localeCompare(text(b.id)));let html='';if(question){html+=renderGlassCard(question);commentChildren(text(question.id)).forEach(c=>{html+=renderGlassCard(c)})}answers.forEach(a=>{html+=renderGlassCard(a);commentChildren(text(a.id)).forEach(c=>{html+=renderGlassCard(c)})});root.innerHTML=html||\"<div class='empty'>No post nodes available.</div>\";bindGlassesInteractions();if(window.MathJax&&MathJax.typesetPromise){MathJax.typesetPromise([root]).catch(()=>{})}}\n"
     "const sexpTip=document.getElementById('sexp-tip');\n"
     "function attachSexpTips(){document.querySelectorAll('.expr-chip').forEach(chip=>{chip.onmouseenter=(evt)=>{const sexp=chip.getAttribute('data-sexp');if(!sexp)return;sexpTip.textContent=sexp;sexpTip.style.display='block';const r=chip.getBoundingClientRect();let left=r.left+window.scrollX;const maxLeft=window.scrollX+window.innerWidth-sexpTip.offsetWidth-10;left=Math.max(window.scrollX+8,Math.min(left,maxLeft));sexpTip.style.left=left+'px';sexpTip.style.top=(r.bottom+window.scrollY+6)+'px'};chip.onmouseleave=()=>{sexpTip.style.display='none'}})}\n"
     "function render(){const {fn,fe}=filtered();renderStats(fn,fe);renderTypeChips();renderGraph(fn,fe);renderNodesTable(fn);renderEdgesTable(fe);if(state.selectedNode && !fn.some(n=>n.id===state.selectedNode) && !postNodes.some(n=>String(n.id)===String(state.selectedNode)))state.selectedNode=null;if(state.selectedEdge!=null && state.selectedEdge>=fe.length)state.selectedEdge=null;renderLenses();renderGlasses()}\n"
     "document.getElementById('search').addEventListener('input',e=>{state.search=e.target.value||'';render()});\n"
     "render();\n"
     "</script></body></html>\n")))

(defun arxana-browser-hypergraph-write-html (item)
  "Write ITEM as an interactive HTML atlas and return the output path."
  (unless (eq (plist-get item :type) 'hypergraph-source)
    (user-error "Not a hypergraph source entry"))
  (let* ((path (plist-get item :path))
         (raw-json (and path (file-readable-p path)
                        (arxana-browser-hypergraph--raw-json-text path)))
         (raw-thread-path (arxana-browser-hypergraph--companion-path path "raw"))
         (raw-thread-json (and raw-thread-path
                               (arxana-browser-hypergraph--raw-json-text raw-thread-path))))
    (unless raw-json
      (user-error "Cannot read hypergraph JSON file: %s" (or path "<nil>")))
    (let* ((base (arxana-browser-hypergraph--safe-filename
                  (or (plist-get item :label)
                      (file-name-nondirectory path))))
           (hash (substring (md5 raw-json) 0 10))
           (filename (format "%s-%s.html" base hash))
           (out (expand-file-name filename arxana-browser-hypergraph-export-directory))
           (html (arxana-browser-hypergraph--html-document item raw-json raw-thread-json)))
      (make-directory (file-name-directory out) t)
      (with-temp-file out
        (insert html))
      out)))

(defun arxana-browser-hypergraph-open-html (item)
  "Generate and open ITEM as an interactive HTML atlas."
  (let ((path (arxana-browser-hypergraph-write-html item)))
    (browse-url-of-file path)
    path))

(defun arxana-browser-hypergraph-open-summary (item)
  "Show ITEM details in a read-only hypergraph summary buffer."
  (unless (eq (plist-get item :type) 'hypergraph-source)
    (user-error "Not a hypergraph source entry"))
  (let* ((path (plist-get item :path))
         (payload (or (plist-get item :payload)
                      (and (file-readable-p path)
                           (ignore-errors
                             (arxana-browser-hypergraph--read-json-file path)))))
         (nodes (arxana-browser-hypergraph--nodes payload))
         (edges (arxana-browser-hypergraph--edges payload))
         (thread (arxana-browser-hypergraph--get payload 'thread_id))
         (node-types (arxana-browser-hypergraph--histogram nodes 'type))
         (edge-types (arxana-browser-hypergraph--histogram edges 'type))
         (buf (get-buffer-create "*Arxana Hypergraph*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Dataset: %s\n" (or (plist-get item :label) "?")))
        (insert (format "Path: %s\n" (or path "-")))
        (insert (format "Status: %s\n" (or (plist-get item :status) "unknown")))
        (when (plist-get item :error)
          (insert (format "Error: %s\n" (plist-get item :error))))
        (if (not payload)
            (insert "\nCould not parse this hypergraph JSON file.\n")
          (insert (format "Thread ID: %s\n"
                          (or (arxana-browser-hypergraph--as-string thread) "-")))
          (insert (format "Nodes: %d\n" (length nodes)))
          (insert (format "Edges: %d\n\n" (length edges)))
          (arxana-browser-hypergraph--insert-histogram "Node Types" node-types)
          (arxana-browser-hypergraph--insert-histogram "Edge Types" edge-types)
          (arxana-browser-hypergraph--insert-previews
           "Node Preview"
           nodes
           (lambda (node)
             (let ((id (arxana-browser-hypergraph--as-string
                        (arxana-browser-hypergraph--get node 'id)))
                   (type (arxana-browser-hypergraph--as-string
                          (arxana-browser-hypergraph--get node 'type))))
               (format "  %-20s %-14s %s\n"
                       (truncate-string-to-width id 20 nil nil t)
                       (truncate-string-to-width type 14 nil nil t)
                       (arxana-browser-hypergraph--node-preview node)))))
          (arxana-browser-hypergraph--insert-previews
           "Edge Preview"
           edges
           (lambda (edge)
             (let* ((type (arxana-browser-hypergraph--as-string
                           (arxana-browser-hypergraph--get edge 'type)))
                    (ends (arxana-browser-hypergraph--safe-list
                           (arxana-browser-hypergraph--get edge 'ends))))
               (format "  %-14s (%d) %s\n"
                       (truncate-string-to-width type 14 nil nil t)
                       (length ends)
                       (arxana-browser-hypergraph--edge-preview edge)))))
          (arxana-browser-hypergraph--insert-thread-lenses nodes edges))
        (goto-char (point-min))
        (view-mode 1)))
    (display-buffer buf)))

(defun arxana-browser-hypergraph-open (item)
  "Open ITEM using `arxana-browser-hypergraph-open-style'."
  (pcase arxana-browser-hypergraph-open-style
    ('summary
     (arxana-browser-hypergraph-open-summary item))
    ('arxana
     (arxana-browser-hypergraph-open-arxana item))
    ('html
     (condition-case err
         (arxana-browser-hypergraph-open-html item)
       (error
        (message "Hypergraph HTML view failed (%s); showing summary"
                 (error-message-string err))
        (arxana-browser-hypergraph-open-summary item))))
    ('both
     (arxana-browser-hypergraph-open-summary item)
     (condition-case err
         (arxana-browser-hypergraph-open-html item)
       (error
        (message "Hypergraph HTML view failed: %s" (error-message-string err)))))
    (_
     (arxana-browser-hypergraph-open-summary item))))

(provide 'arxana-browser-hypergraph)

;;; arxana-browser-hypergraph.el ends here
