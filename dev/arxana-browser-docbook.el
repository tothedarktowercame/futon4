;;; arxana-browser-docbook.el --- Docbook browser views -*- lexical-binding: t; -*-

;;; Commentary:
;; Docbook browsing, ordering, and syncing for the Arxana browser.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'json)
(require 'tabulated-list)

(require 'arxana-docbook)
(require 'arxana-browser-marks)

(defgroup arxana-browser-docbook nil
  "Docbook browsing helpers for the Arxana browser."
  :group 'arxana)

(defvar arxana-browser--stack)
(defvar arxana-browser--context)

(declare-function arxana-browser--render "arxana-browser-core")
(declare-function arxana-browser--goto-doc-id "arxana-browser-core" (doc-id))
(declare-function arxana-browser--current-row "arxana-browser-core")
(declare-function arxana-browser--goto-row "arxana-browser-core" (row))
(declare-function arxana-browser--item-at-point "arxana-browser-core")
(declare-function arxana-browser--ensure-context "arxana-browser-core")
(declare-function arxana-docbook--entry-mtime "arxana-docbook-core" (entry))
(declare-function arxana-docbook--normalize-timestamp "arxana-docbook-core" (value))
(declare-function arxana-docbook--normalize-remote-entry "arxana-docbook-remote" (entry))

(defface arxana-browser-docbook-latest-face
  '((t :foreground "#7fdc7f"))
  "Face used for docbook headings with recent entries."
  :group 'arxana-browser-docbook)

(defface arxana-browser-docbook-empty-face
  '((t :foreground "#d7b46a"))
  "Face used for docbook headings without recent entries."
  :group 'arxana-browser-docbook)

(defface arxana-browser-docbook-unindexed-face
  '((t :foreground "#e3a86e"))
  "Face used for docbook headings missing from the TOC."
  :group 'arxana-browser-docbook)

(defface arxana-browser-docbook-marked-face
  '((t :foreground "#f0d85c"))
  "Face used for marked docbook headings."
  :group 'arxana-browser-docbook)

(defface arxana-browser-docbook-dirty-face
  '((t :foreground "orange"))
  "Face used for docbook headings with local changes."
  :group 'arxana-browser-docbook)

(defcustom arxana-browser-docbook-top-order
  '("Overview"
    "Quickstart"
    "Recent changes (futon4, pilot)"
    "Storage bridge"
    "Pattern workflows"
    "Browsing & relations"
    "Org imports / exports & snapshots"
    "Inclusion / derivation UX"
    "Article lifecycle"
    "Compatibility & test support"
    "Contributor guide (embedded)"
    "QA checklist"
    "Known limitations")
  "Preferred ordering for top-level docbook headings in the contents view."
  :type '(repeat string)
  :group 'arxana-browser-docbook)

(defcustom arxana-browser-docbook-prefer-toc-order t
  "When non-nil, preserve TOC order for docbook contents views and exports."
  :type 'boolean
  :group 'arxana-browser-docbook)

(defvar-local arxana-browser--docbook-contents-order nil
  "Alist of docbook order overrides keyed by book.")

(defvar-local arxana-browser--docbook-contents-order-source nil
  "Alist of docbook order sources keyed by book.")

(defvar-local arxana-browser--docbook-contents-synced-order nil
  "Cached doc-id order last synced from/to remote storage.")

(defvar-local arxana-browser--docbook-contents-toc nil
  "Cached TOC heading list for the current contents view.")

(defvar-local arxana-browser--docbook-contents-view-items nil
  "Docbook contents items for the current browser view.")

(defvar-local arxana-browser--docbook-contents-book nil
  "Book id for the current docbook contents items.")

(defvar-local arxana-browser--docbook-contents-marked (make-hash-table :test 'equal))

(defvar-local arxana-browser--docbook-contents-removed nil
  "Alist of removed docbook doc-ids keyed by book.")

(defvar-local arxana-browser--docbook-contents-hidden nil
  "Obsolete: use `arxana-browser--docbook-contents-removed' instead.")

(put 'arxana-browser--docbook-contents-marked 'permanent-local t)

(put 'arxana-browser--docbook-contents-removed 'permanent-local t)

(put 'arxana-browser--docbook-contents-hidden 'permanent-local t)

(put 'arxana-browser--docbook-contents-order-source 'permanent-local t)

(put 'arxana-browser--docbook-contents-synced-order 'permanent-local t)

(put 'arxana-browser--docbook-contents-toc 'permanent-local t)

(make-obsolete-variable 'arxana-browser--docbook-contents-hidden
                        'arxana-browser--docbook-contents-removed
                        "2025-12-27")

(defun arxana-browser--docbook-format ()
  [("Doc" 28 t)
   ("Version" 18 t)
   ("When" 20 t)
   ("Files" 7 t)
   ("Summary" 0 nil)])

(defun arxana-browser--docbook-contents-format ()
  [("Heading" 60 t)
   ("Path" 0 nil)
   ("Id" 6 t)
   ("Src" 4 t)
   ("C/D" 10 t)])

(defun arxana-browser--docbook-lines-in-string (text)
  (length (split-string (string-trim (or text "")) "\n" t)))

(defun arxana-browser--docbook-file-line-count (path)
  (when (and path (file-readable-p path))
    (with-temp-buffer
      (insert-file-contents path)
      (count-lines (point-min) (point-max)))))

(defun arxana-browser--docbook-ratio-format (loc lod)
  (cond
   ((or (null loc) (null lod)) "")
   ((<= lod 0) (if (> loc 0) "inf" "0"))
   (t
    (let ((ratio (/ (float loc) (float lod))))
      (cond
       ((>= ratio 10000) (format "%.2e" ratio))
       ((>= ratio 1000) (format "%.1e" ratio))
       ((>= ratio 100) (format "%.0f" ratio))
       ((>= ratio 10) (format "%.1f" ratio))
       (t (format "%.2f" ratio)))))))

(defun arxana-browser--docbook-entry-source-path (entry)
  (when entry
    (arxana-docbook--entry-source-path entry)))

(defun arxana-browser--docbook-entry-doc-lines (entry)
  (when entry
    (arxana-browser--docbook-lines-in-string
     (arxana-docbook--entry-content entry))))

(defun arxana-browser--docbook-entry-loc (entry)
  (let* ((source (arxana-browser--docbook-entry-source-path entry))
         (root (arxana-docbook--repo-root))
         (path (and source (expand-file-name source root))))
    (arxana-browser--docbook-file-line-count path)))

(defun arxana-browser--docbook-entry-coverage (entry)
  (if (arxana-browser--docbook-entry-source-path entry) "Y" ""))

(defun arxana-browser--docbook-entry-ratio (entry)
  (let ((loc (arxana-browser--docbook-entry-loc entry))
        (lod (arxana-browser--docbook-entry-doc-lines entry)))
    (arxana-browser--docbook-ratio-format loc lod)))

(defun arxana-browser--docbook-top-key (item)
  (or (car (plist-get item :outline))
      (car (split-string (or (plist-get item :path_string) "") " / " t))
      (plist-get item :title)
      ""))

(defun arxana-browser--docbook-group-order (items)
  (let ((order '()))
    (dolist (item items)
      (let ((key (arxana-browser--docbook-top-key item)))
        (when (and key (not (member key order)))
          (setq order (append order (list key))))))
    order))

(defun arxana-browser--docbook-group-items (items)
  (let* ((order (arxana-browser--docbook-group-order items))
         (preferred (seq-filter (lambda (key) (member key order))
                                arxana-browser-docbook-top-order))
         (order (append preferred (seq-filter (lambda (key) (not (member key preferred)))
                                              order))))
    (sort (copy-sequence items)
          (lambda (a b)
            (let* ((ga (cl-position (arxana-browser--docbook-top-key a)
                                    order
                                    :test #'equal))
                   (gb (cl-position (arxana-browser--docbook-top-key b)
                                    order
                                    :test #'equal))
                   (la (or (plist-get a :level) 99))
                   (lb (or (plist-get b :level) 99))
                   (ia (or (plist-get a :toc-index) 0))
                   (ib (or (plist-get b :toc-index) 0)))
              (cond
               ((and ga gb (/= ga gb)) (< ga gb))
               ((/= la lb) (< la lb))
               (t (< ia ib))))))))

(defun arxana-browser--docbook-contents-order-get (book)
  (cdr (assoc book arxana-browser--docbook-contents-order)))

(defun arxana-browser--docbook-contents-order-set (book order)
  (setf (alist-get book arxana-browser--docbook-contents-order nil nil #'equal)
        order))

(defun arxana-browser--docbook-contents-order-source-get (book)
  (cdr (assoc book arxana-browser--docbook-contents-order-source)))

(defun arxana-browser--docbook-contents-order-source-set (book source)
  (setf (alist-get book arxana-browser--docbook-contents-order-source nil nil #'equal)
        source))

(defun arxana-browser--docbook-contents-dirty-p (book)
  (let* ((items (or arxana-browser--docbook-contents-view-items
                    (arxana-browser--docbook-contents-items-live)))
         (current (and (listp items)
                       (arxana-browser--docbook-contents-current-order items)))
         (synced arxana-browser--docbook-contents-synced-order)
         (order-dirty (and (listp current)
                           (listp synced)
                           (not (equal (delq nil current) (delq nil synced)))))
         (entry-dirty (and (listp items)
                           (seq-find (lambda (item)
                                       (plist-get item :dirty))
                                     items))))
    (or order-dirty entry-dirty)))

(defun arxana-browser--docbook-entry-local-mtime (entry)
  (or (and entry (arxana-docbook--entry-mtime entry)) nil))

(defun arxana-browser--docbook-latest-remote-entry (heading)
  (let* ((latest (plist-get heading :latest)))
    (cond
     ((and (listp latest) (plist-get latest :entry-id)) latest)
     ((listp latest) (arxana-docbook--normalize-remote-entry latest))
     (t nil))))

(defun arxana-browser--docbook-entry-dirty-p (local-mtime remote-entry)
  (let ((remote-time (arxana-docbook--normalize-timestamp
                      (plist-get remote-entry :timestamp))))
    (and local-mtime
         (or (not remote-time)
             (time-less-p remote-time local-mtime)))))

(defun arxana-browser--docbook-sync-order (book order &optional prompt)
  "Sync ORDER for BOOK to remote and/or filesystem toc.json."
  ;; TODO(docbook): treat toc.json as an optional byproduct; remove once remote order persistence is sufficient.
  (when (and prompt (not (yes-or-no-p "Sync current order before export? ")))
    (cl-return-from arxana-browser--docbook-sync-order nil))
  (let ((remote-ok nil)
        (local-ok nil)
        (remote-err nil)
        (local-err nil))
    (when (arxana-docbook--remote-available-p book)
      (let ((resp (arxana-docbook--remote-update-toc-order book order)))
        (if (and resp (eq (plist-get resp :status) :ok))
            (setq remote-ok t)
          (setq remote-err (plist-get resp :error)))))
    (condition-case err
        (when (and (boundp 'arxana-docbook-write-filesystem-toc)
                   arxana-docbook-write-filesystem-toc
                   (or (arxana-docbook--filesystem-available-p book)
                       arxana-browser--docbook-contents-toc))
          (if arxana-browser--docbook-contents-toc
              (arxana-docbook--toc-write-headings
               book arxana-browser--docbook-contents-toc order)
            (arxana-docbook--toc-write-order book order))
          (setq local-ok t))
      (error
       (setq local-err (error-message-string err))))
    (cond
     ((or remote-ok local-ok)
      (arxana-browser--docbook-contents-order-source-set book :toc)
      (setq arxana-browser--docbook-contents-synced-order order)
      (let ((context (car arxana-browser--stack)))
        (when (and context
                   (eq (plist-get context :view) 'docbook-contents)
                   (equal (plist-get context :book) book))
          (arxana-browser--render)))
      (message "Synced docbook order (remote=%s local=%s)"
               (if remote-ok "ok" "no")
               (if local-ok "ok"
                 (if (and (boundp 'arxana-docbook-write-filesystem-toc)
                          arxana-docbook-write-filesystem-toc)
                     "no"
                   "off")))
      t)
     (t
      (message "Docbook order sync failed (remote=%s local=%s)"
               (or remote-err "n/a")
               (or local-err "n/a"))
      nil))))

(defun arxana-browser--docbook-contents-order-items (items order)
  (let ((by-id (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (ordered '()))
    (dolist (item (arxana-browser--docbook-contents-unique-items items))
      (let ((doc-id (plist-get item :doc-id)))
        (when doc-id
          (puthash doc-id item by-id))))
    (dolist (doc-id order)
      (let ((item (gethash doc-id by-id)))
        (when item
          (puthash doc-id t seen)
          (push item ordered))))
    (dolist (item (arxana-browser--docbook-contents-unique-items items))
      (let ((doc-id (plist-get item :doc-id)))
        (when (and doc-id (not (gethash doc-id seen)))
          (push item ordered))))
    (nreverse ordered)))

(defun arxana-browser--docbook-contents-normalize-order (order items)
  "Return ORDER filtered to ITEMS, appending any missing ids."
  (let ((present (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal))
        (normalized '())
        (missing '()))
    (dolist (item items)
      (when-let* ((doc-id (plist-get item :doc-id)))
        (puthash doc-id t present)))
    (dolist (doc-id order)
      (when (and doc-id (gethash doc-id present))
        (puthash doc-id t seen)
        (push doc-id normalized)))
    (setq normalized (nreverse normalized))
    (dolist (item items)
      (when-let* ((doc-id (plist-get item :doc-id)))
        (unless (gethash doc-id seen)
          (push doc-id missing))))
    (append normalized (nreverse missing))))

(defun arxana-browser--docbook-contents-removed-get (book)
  "Return removed doc-ids for BOOK, migrating obsolete storage."
  (let* ((removed (cdr (assoc book arxana-browser--docbook-contents-removed)))
         (hidden (cdr (assoc book arxana-browser--docbook-contents-hidden)))
         (merged (cl-delete-duplicates (append removed hidden) :test #'equal)))
    (when hidden
      (setf (alist-get book arxana-browser--docbook-contents-removed nil nil #'equal)
            merged)
      (setf (alist-get book arxana-browser--docbook-contents-hidden nil nil #'equal)
            nil))
    merged))

(defun arxana-browser--docbook-contents-removed-set (book removed)
  (setf (alist-get book arxana-browser--docbook-contents-removed nil nil #'equal)
        removed)
  (setf (alist-get book arxana-browser--docbook-contents-hidden nil nil #'equal)
        nil))

(defun arxana-browser--docbook-contents-removed-p (book doc-id)
  (and doc-id (member doc-id (arxana-browser--docbook-contents-removed-get book))))

(defun arxana-browser--docbook-contents-marked-p (doc-id)
  (and doc-id
       (hash-table-p arxana-browser--docbook-contents-marked)
       (gethash doc-id arxana-browser--docbook-contents-marked)))

(defun arxana-browser--docbook-contents-mark-key (item)
  (when (and item (eq (plist-get item :type) 'docbook-heading))
    (plist-get item :doc-id)))

(defun arxana-browser-docbook-toggle-mark-at-point ()
  "Toggle mark for the docbook heading at point."
  (interactive)
  (arxana-browser--docbook-contents-assert)
  (let ((item (arxana-browser--docbook-contents-current-item)))
    (let ((doc-id (plist-get item :doc-id)))
      (unless doc-id
        (user-error "Docbook heading missing doc-id"))
      (if (gethash doc-id arxana-browser--docbook-contents-marked)
          (remhash doc-id arxana-browser--docbook-contents-marked)
        (puthash doc-id t arxana-browser--docbook-contents-marked))
      (arxana-browser--docbook-contents-update-line item))))

(defun arxana-browser-docbook-remove-marked ()
  "Remove marked docbook headings from the contents view."
  (interactive)
  (arxana-browser--docbook-contents-assert)
  (let* ((context (arxana-browser--docbook-contents-context))
         (book (plist-get context :book))
         (marked (arxana-browser-marks-items-in-context
                  arxana-browser--docbook-contents-marked
                  #'arxana-browser--docbook-contents-mark-key
                  (lambda (item) (eq (plist-get item :type) 'docbook-heading))))
         (doc-ids (delq nil (mapcar (lambda (item) (plist-get item :doc-id)) marked))))
    (unless doc-ids
      (user-error "No marked docbook headings"))
    (let* ((removed (or (arxana-browser--docbook-contents-removed-get book) '()))
           (merged (cl-delete-duplicates (append removed doc-ids) :test #'equal)))
      (arxana-browser--docbook-contents-removed-set book merged)
      (dolist (doc-id doc-ids)
        (remhash doc-id arxana-browser--docbook-contents-marked)))
    (arxana-browser--render)))

(defun arxana-browser--docbook-contents-refresh-local ()
  "Refresh the docbook contents buffer without reloading remote data."
  (let* ((context (arxana-browser--docbook-contents-context)))
    (when context
      (let* ((items (mapcar #'car (or tabulated-list-entries '())))
             (items (seq-filter
                     (lambda (item)
                       (not (arxana-browser--docbook-contents-removed-p
                             (plist-get context :book)
                             (plist-get item :doc-id))))
                     items))
             (entries (mapcar (lambda (item)
                                (list item (arxana-browser--docbook-contents-row item)))
                              items))
             (desired-row (arxana-browser--current-row)))
        (let ((inhibit-read-only t))
          (setq tabulated-list-entries entries)
          (tabulated-list-print t)
          (arxana-browser--goto-row desired-row))))))

(defun arxana-browser--docbook-contents-update-line (item)
  "Update the current docbook contents line for ITEM."
  (let* ((doc-id (plist-get item :doc-id))
         (row (arxana-browser--docbook-contents-row item))
         (updated (list item row)))
    (setq tabulated-list-entries
          (mapcar (lambda (entry)
                    (if (and (car entry)
                             (equal doc-id (plist-get (car entry) :doc-id)))
                        updated
                      entry))
                  (or tabulated-list-entries '())))
    (let ((inhibit-read-only t))
      (beginning-of-line)
      (delete-region (line-beginning-position)
                     (min (point-max) (1+ (line-end-position))))
      (tabulated-list-print-entry item row)
      (beginning-of-line))))

(defun arxana-browser-docbook-hard-delete-marked ()
  "Hard delete marked/removed docbook headings from storage."
  (interactive)
  (arxana-browser--docbook-contents-assert)
  (let* ((context (arxana-browser--docbook-contents-context))
         (book (plist-get context :book))
         (marked (arxana-browser-marks-items-in-context
                  arxana-browser--docbook-contents-marked
                  #'arxana-browser--docbook-contents-mark-key
                  (lambda (item) (eq (plist-get item :type) 'docbook-heading))))
         (marked-ids (delq nil (mapcar (lambda (item) (plist-get item :doc-id)) marked)))
         (removed-ids (or (arxana-browser--docbook-contents-removed-get book) '()))
         (doc-ids (cl-delete-duplicates (append marked-ids removed-ids) :test #'equal)))
    (unless doc-ids
      (user-error "No marked or removed docbook headings"))
    (let ((filesystem-count 0)
          (remote-ok 0)
          (remote-failed 0)
          (remote-errors '())
          (deleted-ids '())
          (removed (or (arxana-browser--docbook-contents-removed-get book) '())))
      (setq arxana-docbook-last-delete-errors nil)
      (dolist (doc-id doc-ids)
        (let ((deleted (arxana-docbook--filesystem-delete-doc book doc-id)))
          (setq filesystem-count (+ filesystem-count (length deleted))))
        (when (arxana-docbook--remote-available-p book)
          (let ((resp (arxana-docbook--remote-delete-toc book doc-id t)))
            (if (and resp (eq (plist-get resp :status) :ok))
                (progn
                  (setq remote-ok (1+ remote-ok))
                  (push doc-id deleted-ids))
              (setq remote-failed (1+ remote-failed))
              (push (list :doc-id doc-id
                          :error (plist-get resp :error)
                          :response (plist-get resp :response))
                    remote-errors)))))
      (let* ((order (arxana-browser--docbook-contents-order-get book))
             (order (and order (cl-remove-if (lambda (doc-id) (member doc-id doc-ids)) order))))
        (when order
          (arxana-browser--docbook-contents-order-set book order)))
      (setq removed (cl-remove-if (lambda (doc-id) (member doc-id doc-ids)) removed))
      (arxana-browser--docbook-contents-removed-set book removed)
      (dolist (doc-id doc-ids)
        (remhash doc-id arxana-browser--docbook-contents-marked))
      (setq arxana-browser--docbook-contents-view-items nil)
      (setq arxana-browser--docbook-contents-book nil)
      (arxana-browser--render)
      (setq arxana-docbook-last-delete-errors (nreverse remote-errors))
      (if remote-errors
          (message "Hard deleted %d entries (filesystem %d, remote ok %d, remote failed %d; details in arxana-docbook-last-delete-errors)"
                   (length doc-ids) filesystem-count remote-ok remote-failed)
        (message "Hard deleted %d entries (filesystem %d, remote ok %d)"
                 (length doc-ids) filesystem-count remote-ok)))))

(defun arxana-browser--docbook-contents-ensure-order (book items)
  (let* ((unique (arxana-browser--docbook-contents-unique-items items))
         (order (arxana-browser--docbook-contents-order-get book))
         (source (arxana-browser--docbook-contents-order-source-get book))
         (default-order (mapcar (lambda (item) (plist-get item :doc-id)) unique))
         (default-order (delq nil default-order)))
    (cond
     ((null order)
      (setq order default-order)
      (arxana-browser--docbook-contents-order-set book order)
      (arxana-browser--docbook-contents-order-source-set book :toc))
     ((and (not (eq source :manual))
           (not (equal order default-order)))
      (setq order default-order)
      (arxana-browser--docbook-contents-order-set book order)
      (arxana-browser--docbook-contents-order-source-set book :toc))
     ((null source)
      (arxana-browser--docbook-contents-order-source-set book :toc)))
    (let ((normalized (arxana-browser--docbook-contents-normalize-order order unique)))
      (unless (equal normalized order)
        (arxana-browser--docbook-contents-order-set book normalized))
      normalized)))

(defun arxana-browser--docbook-contents-item-map (items)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (item (arxana-browser--docbook-contents-unique-items items))
      (when-let* ((doc-id (plist-get item :doc-id)))
        (puthash doc-id item table)))
    table))

(defun arxana-browser--docbook-short-id (doc-id)
  (let* ((suffix (and doc-id (car (last (split-string doc-id "-" t))))))
    (and suffix (substring suffix 0 (min 5 (length suffix))))))

(defun arxana-browser--docbook-row-label (item)
  (let* ((doc-id (plist-get item :doc-id))
         (title (or (plist-get item :label) ""))
         (short-id (arxana-browser--docbook-short-id doc-id)))
    (cond
     ((and short-id (not (string-empty-p title)))
      (format "[%s] %s" short-id title))
     (short-id (format "[%s]" short-id))
     ((not (string-empty-p title)) title)
     (doc-id doc-id)
     (t ""))))

(defun arxana-browser--docbook-row (item)
  (let ((label (arxana-browser--docbook-row-label item)))
    (vector label
            (or (plist-get item :version) "")
            (or (plist-get item :timestamp) "")
            (format "%d" (length (or (plist-get item :files) '())))
            (or (plist-get item :summary)
                (plist-get item :description)
                ""))))

(defun arxana-browser--docbook-contents-row (item)
  (let* ((level (or (plist-get item :level) 1))
         (title (or (plist-get item :title) ""))
         (doc-id (plist-get item :doc-id))
         (short-id (arxana-browser--docbook-short-id doc-id))
         (short-col (if short-id (format "%5s" short-id) ""))
         (coverage (or (plist-get item :coverage) ""))
         (ratio (or (plist-get item :ratio) ""))
         ;; ASCII markers only; non-ASCII char literals can break load in some Emacs builds.
         (indent (make-string (max 0 (1- level)) ?.))
         (latest (plist-get item :latest))
         (virtual (plist-get item :virtual))
         (dirty (plist-get item :dirty))
         (marked (arxana-browser--docbook-contents-marked-p doc-id))
         (marker (cond
                  (marked (propertize "M" 'face 'arxana-browser-docbook-marked-face))
                  (virtual (propertize "!" 'face 'arxana-browser-docbook-unindexed-face))
                  (latest (propertize "*" 'face 'arxana-browser-docbook-latest-face))
                  (t (propertize "." 'face 'arxana-browser-docbook-empty-face)))))
    (when (and dirty (stringp marker))
      (add-face-text-property 0 (length marker)
                              'arxana-browser-docbook-dirty-face
                              t marker))
    (vector (format "%s %s%s"
                    marker
                    (if (string-empty-p indent)
                        ""
                      (format "%s " indent))
                    title)
            (or (plist-get item :path_string) "")
            short-col
            coverage
            ratio)))

(defun arxana-browser--docbook-books ()
  (let ((books (or (arxana-docbook--available-books) '("futon4"))))
    (mapcar (lambda (book)
              (list :type 'docbook-book
                    :label (capitalize book)
                    :book book
                    :description (format "Doc book (%s)."
                                         (arxana-docbook--source-brief book))))
            books)))

(defun arxana-browser--docbook-unavailable-message (&optional book)
  (let* ((book (or book "futon4"))
         (probe (arxana-docbook--probe-summary book)))
    (format "Doc book %s unavailable. %s" book probe)))

(defun arxana-browser--docbook-entry-label (entry)
  (or (plist-get entry :title)
      (when-let* ((heading (plist-get entry :heading)))
        (or (plist-get heading :doc/title)
            (plist-get heading :doc/path_string)))
      (plist-get entry :doc-id)))

(defun arxana-browser--docbook-items (&optional book)
  (let* ((book (or book "futon4"))
         (entries (or (when (arxana-docbook--remote-available-p book)
                        (ignore-errors (arxana-docbook--remote-recent book)))
                      (ignore-errors (arxana-docbook-entries book)))))
    (if (and entries (listp entries))
        (mapcar (lambda (entry)
                  (list :type 'docbook-entry
                        :doc-id (plist-get entry :doc-id)
                        :label (arxana-browser--docbook-entry-label entry)
                        :version (plist-get entry :version)
                        :timestamp (plist-get entry :timestamp)
                        :files (plist-get entry :files)
                        :summary (plist-get entry :summary)
                        :entry entry))
                entries)
      (list (list :type 'info
                  :label "No doc book entries detected"
                  :description (arxana-browser--docbook-unavailable-message book))))))

(defun arxana-browser--docbook-book-items (book)
  (list (list :type 'docbook-contents-root
              :label "Contents"
              :description "Browse spine outline mirrored into doc book."
              :book book)
        (list :type 'docbook-recent
              :label "Recent"
              :description "List recent doc book entries."
              :book book)))

(defun arxana-browser--docbook-contents-items (book)
  (let* ((remote-raw (when (arxana-docbook--remote-available-p book)
                       (ignore-errors (arxana-docbook--remote-contents book))))
         (remote (and remote-raw (arxana-docbook--normalize-remote-toc remote-raw)))
         (local-toc (or (arxana-docbook--toc book) '()))
         (toc (or remote local-toc '()))
         (toc-order (delq nil (mapcar #'arxana-docbook--toc-doc-id toc)))
         (local-entries (when (and (not remote) (arxana-docbook--filesystem-available-p book))
                          (ignore-errors (arxana-docbook-entries book))))
         (recent-entries (when (and remote (arxana-docbook--remote-available-p book))
                           (ignore-errors (arxana-docbook--remote-recent book))))
         (entries-for-metrics (or recent-entries local-entries '()))
         (entry-map (let ((table (make-hash-table :test 'equal)))
                      (dolist (entry entries-for-metrics)
                        (when-let* ((doc-id (plist-get entry :doc-id)))
                          (let* ((existing (gethash doc-id table))
                                 (current-ts (plist-get entry :timestamp))
                                 (existing-ts (plist-get existing :timestamp)))
                            (when (or (null existing)
                                      (string> (or current-ts "") (or existing-ts "")))
                              (puthash doc-id entry table)))))
                      table))
         (latest-docs (when (and local-entries (listp local-entries))
                        (let ((table (make-hash-table :test 'equal)))
                          (dolist (entry local-entries)
                            (when-let* ((doc-id (plist-get entry :doc-id)))
                              (puthash doc-id t table)))
                          table)))
         (local-time-map (let ((table (make-hash-table :test 'equal)))
                           (dolist (entry (or local-entries '()))
                             (when-let* ((doc-id (plist-get entry :doc-id))
                                         (mtime (arxana-browser--docbook-entry-local-mtime entry)))
                               (let ((existing (gethash doc-id table)))
                                 (when (or (not existing)
                                           (time-less-p existing mtime))
                                   (puthash doc-id mtime table)))))
                           table))
         (remote-map (when (and remote (listp remote))
                       (let ((table (make-hash-table :test 'equal)))
                         (dolist (h remote)
                           (let ((doc-id (or (plist-get h :doc-id) (plist-get h :doc_id))))
                             (when doc-id
                               (puthash doc-id h table))))
                         table)))
         (toc-items
          (if toc
              (cl-loop for h in toc
                         for idx from 0
                         collect
                         (let* ((doc-id (or (plist-get h :doc-id) (plist-get h :doc_id)))
                                (remote-h (and remote-map doc-id (gethash doc-id remote-map)))
                                (path-string (or (plist-get h :path_string) (plist-get h :path-string)))
                                (latest (or (and remote-h (plist-get remote-h :latest))
                                            (plist-get h :latest)
                                            (and latest-docs (gethash doc-id latest-docs))))
                                (remote-entry (and remote-h
                                                   (arxana-browser--docbook-latest-remote-entry remote-h)))
                                (entry (and doc-id (gethash doc-id entry-map)))
                                (local-mtime (and doc-id (gethash doc-id local-time-map)))
                                (dirty (arxana-browser--docbook-entry-dirty-p local-mtime remote-entry))
                                (coverage (and entry (arxana-browser--docbook-entry-coverage entry)))
                                (ratio (and entry (arxana-browser--docbook-entry-ratio entry))))
                           (list :type 'docbook-heading
                                 :doc-id doc-id
                                 :title (or (plist-get h :title) doc-id)
                                 :outline (or (plist-get h :outline) (plist-get h :outline_path))
                                 :path_string path-string
                                 :level (plist-get h :level)
                                 :latest latest
                                 :dirty dirty
                                 :book book
                                 :coverage coverage
                                 :ratio ratio
                                 :toc-index idx)))
            '()))
         (toc-items (if (and toc-items arxana-browser-docbook-prefer-toc-order)
                        toc-items
                      (arxana-browser--docbook-group-items toc-items)))
         (order (arxana-browser--docbook-contents-ensure-order book toc-items))
         (toc-items (arxana-browser--docbook-contents-order-items toc-items order))
         (toc-items (seq-filter (lambda (item)
                                  (not (arxana-browser--docbook-contents-removed-p
                                        book (plist-get item :doc-id))))
                                toc-items))
         (toc-docs (let ((table (make-hash-table :test 'equal)))
                     (dolist (item toc-items)
                       (when-let* ((doc-id (plist-get item :doc-id)))
                         (puthash doc-id t table)))
                     table))
         (entry-headings
          (cond
           (remote
            (delq nil
                  (mapcar (lambda (entry)
                            (let* ((heading (plist-get entry :heading))
                                   (doc-id (or (plist-get heading :doc/id)
                                               (plist-get heading :doc-id))))
                              (when doc-id
                                (list :doc-id doc-id
                                      :title (or (plist-get heading :doc/title)
                                                 (plist-get heading :doc/path_string)
                                                 doc-id)
                                      :outline (plist-get heading :doc/outline_path)
                                      :path_string (plist-get heading :doc/path_string)
                                      :level (plist-get heading :doc/level)))))
                          (or recent-entries '()))))
           (local-entries
            (delq nil
                  (mapcar (lambda (entry)
                            (when-let* ((doc-id (plist-get entry :doc-id)))
                              (let* ((outline (plist-get entry :outline))
                                     (path-string (or (plist-get entry :path_string)
                                                      (and outline (string-join outline " / ")))))
                                (list :doc-id doc-id
                                      :title (or (and outline (car outline)) doc-id)
                                      :outline outline
                                      :path_string path-string
                                      :level (and outline (length outline))))))
                          local-entries)))
           (t '())))
         (virtual-items
          (seq-filter
           (lambda (item)
             (not (arxana-browser--docbook-contents-removed-p
                   book (plist-get item :doc-id))))
           (delq nil
                 (mapcar
                  (lambda (h)
                    (let ((doc-id (plist-get h :doc-id)))
                      (when (and doc-id (not (gethash doc-id toc-docs)))
                        (let* ((title (or (plist-get h :title) doc-id))
                               (outline (or (plist-get h :outline)
                                            (list "Lab additions" title)))
                               (path-str (or (plist-get h :path_string)
                                             (string-join outline " / "))))
                          (list :type 'docbook-heading
                                :doc-id doc-id
                                :title (format "%s (unindexed)" title)
                                :outline outline
                               :path_string path-str
                               :level (or (plist-get h :level) 1)
                               :latest t
                                :dirty (and (gethash doc-id local-time-map) t)
                               :virtual t
                               :book book
                               :coverage (and (gethash doc-id entry-map)
                                              (arxana-browser--docbook-entry-coverage
                                               (gethash doc-id entry-map)))
                                :ratio (and (gethash doc-id entry-map)
                                            (arxana-browser--docbook-entry-ratio
                                             (gethash doc-id entry-map))))))))
                  entry-headings)))))
    (when (or (not (equal arxana-browser--docbook-contents-book book))
              (null arxana-browser--docbook-contents-synced-order))
      (setq arxana-browser--docbook-contents-order-source nil)
      (setq arxana-browser--docbook-contents-synced-order
            (or toc-order order)))
    (setq arxana-browser--docbook-contents-toc toc)
    (setq arxana-browser--docbook-contents-view-items toc-items)
    (setq arxana-browser--docbook-contents-book book)
    (cond
     ((or toc-items virtual-items) (append toc-items virtual-items))
     (t (list (list :type 'info
                    :label "No TOC found"
                    :description (arxana-browser--docbook-unavailable-message book)))))))

(defun arxana-browser--docbook-contents-context ()
  (let ((context (car arxana-browser--stack)))
    (when (and context (eq (plist-get context :view) 'docbook-contents))
      context)))

(defun arxana-browser--docbook-contents-assert ()
  (unless (arxana-browser--docbook-contents-context)
    (user-error "Not in a docbook contents view")))

(defun arxana-browser--docbook-contents-current-item ()
  (arxana-browser--docbook-contents-assert)
  (let ((item (tabulated-list-get-id)))
    (unless (and item (plist-get item :doc-id))
      (user-error "No docbook heading at point"))
    item))

(defun arxana-browser--docbook-contents-items-live ()
  "Return the current docbook contents items."
  (or arxana-browser--docbook-contents-view-items
      (delq nil
            (mapcar (lambda (entry)
                      (let ((item (car-safe entry)))
                        (when (and item (plist-get item :doc-id))
                          item)))
                    (or tabulated-list-entries '())))))

(defun arxana-browser--docbook-contents-unique-items (items)
  "Return ITEMS de-duplicated by :doc-id, preserving first occurrence."
  (let ((seen (make-hash-table :test 'equal))
        (unique '()))
    (dolist (item items)
      (let ((doc-id (plist-get item :doc-id)))
        (when (and doc-id (not (gethash doc-id seen)))
          (puthash doc-id t seen)
          (push item unique))))
    (nreverse unique)))

(defun arxana-browser--docbook-contents-current-order (items)
  (delq nil (mapcar (lambda (item) (plist-get item :doc-id))
                    (arxana-browser--docbook-contents-unique-items items))))

(defun arxana-browser--docbook-contents-item-swap (order idx-a idx-b)
  (let ((copy (copy-sequence order)))
    (let ((val-a (nth idx-a copy))
          (val-b (nth idx-b copy)))
      (setf (nth idx-a copy) val-b)
      (setf (nth idx-b copy) val-a))
    copy))

(defun arxana-browser--docbook-contents-reorder (new-order doc-id)
  (let* ((context (arxana-browser--docbook-contents-context))
         (book (plist-get context :book)))
    (arxana-browser--docbook-contents-order-set book new-order)
    (arxana-browser--docbook-contents-order-source-set book :manual)
    (when arxana-browser--docbook-contents-view-items
      (setq arxana-browser--docbook-contents-view-items
            (arxana-browser--docbook-contents-order-items
             arxana-browser--docbook-contents-view-items new-order)))
    (arxana-browser--render)
    (when (fboundp 'arxana-browser--goto-doc-id)
      (arxana-browser--goto-doc-id doc-id))))

(defun arxana-browser-docbook-move-item-up ()
  "Move the current docbook heading up one row."
  (interactive)
  (let* ((item (arxana-browser--docbook-contents-current-item))
         (doc-id (plist-get item :doc-id))
         (context (arxana-browser--docbook-contents-context))
         (book (plist-get context :book))
         (items (arxana-browser--docbook-contents-items-live))
         (order (arxana-browser--docbook-contents-current-order items)))
    (unless order
      (user-error "No ordering information available"))
    (let* ((idx (cl-position doc-id order :test #'equal))
           (prev-idx (and idx (> idx 0) (1- idx))))
      (unless idx
        (user-error "No ordering entry for this heading"))
      (unless prev-idx
        (user-error "Already at top"))
      (arxana-browser--docbook-contents-reorder
       (arxana-browser--docbook-contents-item-swap order idx prev-idx)
       doc-id))))

(defun arxana-browser-docbook-move-item-down ()
  "Move the current docbook heading down one row."
  (interactive)
  (let* ((item (arxana-browser--docbook-contents-current-item))
         (doc-id (plist-get item :doc-id))
         (context (arxana-browser--docbook-contents-context))
         (book (plist-get context :book))
         (items (arxana-browser--docbook-contents-items-live))
         (order (arxana-browser--docbook-contents-current-order items)))
    (unless order
      (user-error "No ordering information available"))
    (let* ((idx (cl-position doc-id order :test #'equal))
           (next-idx (and idx (< idx (1- (length order))) (1+ idx))))
      (unless idx
        (user-error "No ordering entry for this heading"))
      (unless next-idx
        (user-error "Already at bottom"))
      (arxana-browser--docbook-contents-reorder
       (arxana-browser--docbook-contents-item-swap order idx next-idx)
       doc-id))))

(defun arxana-browser--docbook-contents-section-blocks (items)
  "Return section blocks based on display order in ITEMS."
  (let ((blocks '())
        (current-key nil)
        (current-block '()))
    (dolist (item (arxana-browser--docbook-contents-unique-items items))
      (let* ((doc-id (plist-get item :doc-id))
             (key (arxana-browser--docbook-top-key item)))
        (if (and current-key (equal key current-key))
            (push doc-id current-block)
          (when current-block
            (push (cons current-key (nreverse current-block)) blocks))
          (setq current-key key)
          (setq current-block (list doc-id)))))
    (when current-block
      (push (cons current-key (nreverse current-block)) blocks))
    (nreverse blocks)))

(defun arxana-browser-docbook-move-section-up ()
  "Move the current docbook section up by one section."
  (interactive)
  (let* ((item (arxana-browser--docbook-contents-current-item))
         (doc-id (plist-get item :doc-id))
         (context (arxana-browser--docbook-contents-context))
         (book (plist-get context :book))
         (items (arxana-browser--docbook-contents-items-live))
         (order (arxana-browser--docbook-contents-current-order items))
         (blocks (arxana-browser--docbook-contents-section-blocks items))
         (key (arxana-browser--docbook-top-key item))
         (idx (cl-position key (mapcar #'car blocks) :test #'equal)))
    (unless idx
      (user-error "No section ordering entry for this heading"))
    (unless (and idx (> idx 0))
      (user-error "Already at top"))
    (let* ((copy (copy-sequence blocks))
           (prev (nth (1- idx) copy))
           (cur (nth idx copy)))
      (setf (nth (1- idx) copy) cur)
      (setf (nth idx copy) prev)
      (arxana-browser--docbook-contents-reorder
       (apply #'append (mapcar #'cdr copy))
       doc-id))))

(defun arxana-browser-docbook-move-section-down ()
  "Move the current docbook section down by one section."
  (interactive)
  (let* ((item (arxana-browser--docbook-contents-current-item))
         (doc-id (plist-get item :doc-id))
         (context (arxana-browser--docbook-contents-context))
         (book (plist-get context :book))
         (items (arxana-browser--docbook-contents-items-live))
         (order (arxana-browser--docbook-contents-current-order items))
         (blocks (arxana-browser--docbook-contents-section-blocks items))
         (key (arxana-browser--docbook-top-key item))
         (idx (cl-position key (mapcar #'car blocks) :test #'equal)))
    (unless idx
      (user-error "No section ordering entry for this heading"))
    (unless (and idx (< idx (1- (length blocks))))
      (user-error "Already at bottom"))
    (let* ((copy (copy-sequence blocks))
           (next (nth (1+ idx) copy))
           (cur (nth idx copy)))
      (setf (nth (1+ idx) copy) cur)
      (setf (nth idx copy) next)
      (arxana-browser--docbook-contents-reorder
       (apply #'append (mapcar #'cdr copy))
       doc-id))))

(defun arxana-browser--docbook-contents-top-level-p (item)
  (let ((level (or (plist-get item :level) 1)))
    (<= level 1)))

(defun arxana-browser-docbook-move-top ()
  "Move the current docbook section or heading to the top."
  (interactive)
  (let* ((item (arxana-browser--docbook-contents-current-item))
         (doc-id (plist-get item :doc-id))
         (items (arxana-browser--docbook-contents-items-live))
         (order (arxana-browser--docbook-contents-current-order items)))
    (unless order
      (user-error "No ordering information available"))
    (if (arxana-browser--docbook-contents-top-level-p item)
        (let* ((blocks (arxana-browser--docbook-contents-section-blocks items))
               (keys (mapcar #'car blocks))
               (key (arxana-browser--docbook-top-key item))
               (idx (cl-position key keys :test #'equal)))
          (unless idx
            (user-error "No section ordering entry for this heading"))
          (unless (> idx 0)
            (user-error "Already at top"))
          (let* ((block (nth idx blocks))
                 (head (cl-subseq blocks 0 idx))
                 (tail (nthcdr (1+ idx) blocks))
                 (copy (cons block (append head tail))))
            (arxana-browser--docbook-contents-reorder
             (apply #'append (mapcar #'cdr copy))
             doc-id)))
      (let* ((idx (cl-position doc-id order :test #'equal)))
        (unless idx
          (user-error "No ordering entry for this heading"))
        (unless (> idx 0)
          (user-error "Already at top"))
        (let ((copy (cons doc-id (cl-remove doc-id order :test #'equal))))
          (arxana-browser--docbook-contents-reorder copy doc-id))))))

(defun arxana-browser-docbook-move-bottom ()
  "Move the current docbook section or heading to the bottom."
  (interactive)
  (let* ((item (arxana-browser--docbook-contents-current-item))
         (doc-id (plist-get item :doc-id))
         (items (arxana-browser--docbook-contents-items-live))
         (order (arxana-browser--docbook-contents-current-order items)))
    (unless order
      (user-error "No ordering information available"))
    (if (arxana-browser--docbook-contents-top-level-p item)
        (let* ((blocks (arxana-browser--docbook-contents-section-blocks items))
               (keys (mapcar #'car blocks))
               (key (arxana-browser--docbook-top-key item))
               (idx (cl-position key keys :test #'equal)))
          (unless idx
            (user-error "No section ordering entry for this heading"))
          (unless (< idx (1- (length blocks)))
            (user-error "Already at bottom"))
          (let* ((block (nth idx blocks))
                 (head (cl-subseq blocks 0 idx))
                 (tail (nthcdr (1+ idx) blocks))
                 (copy (append head tail (list block))))
            (arxana-browser--docbook-contents-reorder
             (apply #'append (mapcar #'cdr copy))
             doc-id)))
      (let* ((idx (cl-position doc-id order :test #'equal)))
        (unless idx
          (user-error "No ordering entry for this heading"))
        (unless (< idx (1- (length order)))
          (user-error "Already at bottom"))
        (let ((copy (append (cl-remove doc-id order :test #'equal) (list doc-id))))
          (arxana-browser--docbook-contents-reorder copy doc-id))))))

(defun arxana-browser--docbook-section-items (book heading)
  (let* ((outline (plist-get heading :outline))
         (doc-id (plist-get heading :doc-id))
         (entries (or (when (arxana-docbook--remote-available-p book)
                        (ignore-errors (arxana-docbook--remote-heading book doc-id)))
                      (ignore-errors (arxana-docbook-entries book)))))
    (if (and entries (listp entries))
        (let* ((matches
                (seq-filter
                 (lambda (entry)
                   (or (and outline (equal outline (plist-get entry :outline)))
                       (and doc-id (equal doc-id (plist-get entry :doc-id)))))
                 entries)))
          (if matches
              (mapcar (lambda (entry)
                        (list :type 'docbook-entry
                              :doc-id (plist-get entry :doc-id)
                              :label (arxana-browser--docbook-entry-label entry)
                              :version (plist-get entry :version)
                              :timestamp (plist-get entry :timestamp)
                              :files (plist-get entry :files)
                              :summary (plist-get entry :summary)
                              :entry entry))
                      matches)
            (list (list :type 'info
                        :label "No entries yet"
                        :summary "Add a stub/raw entry for this heading to browse it here."
                        :description "Add a stub/raw entry for this heading to browse it here."))))
      (list (list :type 'info
                  :label "No doc book entries detected"
                  :summary (arxana-browser--docbook-unavailable-message book)
                  :description (arxana-browser--docbook-unavailable-message book))))))

(defun arxana-browser--docbook-heading-at-point ()
  (let ((item (arxana-browser--item-at-point)))
    (when (and item (eq (plist-get item :type) 'docbook-heading))
      item)))

(defun arxana-browser-docbook-open-book ()
  "Open a compiled docbook view for the current book."
  (interactive)
  (let* ((item (arxana-browser--item-at-point))
         (book (or (plist-get item :book) "futon4")))
    (arxana-docbook-open-book book)))

(defun arxana-browser-docbook-open-section-context ()
  "Open a contextual docbook view around the heading at point."
  (interactive)
  (let ((heading (arxana-browser--docbook-heading-at-point)))
    (unless heading
      (user-error "No docbook heading at point"))
    (arxana-docbook-open-section-context (plist-get heading :book)
                                         (plist-get heading :doc-id)
                                         (plist-get heading :toc-index))))

(defun arxana-browser--docbook-location (item)
  (let* ((book (or (plist-get item :book) "futon4"))
         (doc-id (plist-get item :doc-id))
         (entry (plist-get item :entry))
         (entry-id (and entry (plist-get entry :entry-id))))
    (cond
     ((and book doc-id entry-id) (format "docbook://%s/%s/%s" book doc-id entry-id))
     ((and book doc-id) (format "docbook://%s/%s" book doc-id))
     (t nil))))

(defun arxana-browser-docbook-export-org ()
  "Export the current docbook contents view to a single Org file."
  (interactive)
  (arxana-browser--ensure-context)
  (let* ((context (car arxana-browser--stack))
         (book (plist-get context :book)))
    (unless (and context (eq (plist-get context :view) 'docbook-contents))
      (user-error "Export only works in a docbook contents view"))
    (let* ((order (arxana-browser--docbook-contents-order-get book))
           (source (arxana-browser--docbook-contents-order-source-get book))
           (dirty (arxana-browser--docbook-contents-dirty-p book))
           (order (and (eq source :manual) (listp order) (delq nil order)))
           (needs-sync (and order dirty)))
      (when needs-sync
        (arxana-browser--docbook-sync-order book order t))
      (arxana-docbook-export-org-book book nil order))))

(defun arxana-browser-docbook-sync-order ()
  "Sync the current docbook contents order to remote storage and/or toc.json."
  (interactive)
  (arxana-browser--ensure-context)
  (let* ((context (car arxana-browser--stack))
         (book (plist-get context :book)))
    (unless (and context (eq (plist-get context :view) 'docbook-contents))
      (user-error "Sync only works in a docbook contents view"))
    (if (not (arxana-browser--docbook-contents-dirty-p book))
        (message "Docbook order already synced")
      (let* ((source (arxana-browser--docbook-contents-order-source-get book))
             (order (arxana-browser--docbook-contents-order-get book))
             (items (arxana-browser--docbook-contents-items-live))
             (current (arxana-browser--docbook-contents-current-order items))
             (synced arxana-browser--docbook-contents-synced-order)
             (candidate (cond
                         ((and (eq source :manual) (listp order)) order)
                         ((listp current) current)
                         (t order)))
             (candidate (or candidate current))
             (filtered (if (and (listp synced) synced)
                           (seq-filter (lambda (doc-id) (member doc-id synced))
                                       (delq nil candidate))
                         (delq nil candidate))))
        (unless (and filtered (listp filtered) filtered)
          (user-error "No order available to sync"))
        (unless (arxana-browser--docbook-sync-order book filtered nil)
          (user-error "Docbook order sync failed"))))))

(defun arxana-browser-docbook-export-pdf ()
  "Export the current docbook contents view to Org, then to PDF."
  (interactive)
  (arxana-browser--ensure-context)
  (let* ((context (car arxana-browser--stack))
         (book (plist-get context :book)))
    (unless (and context (eq (plist-get context :view) 'docbook-contents))
      (user-error "Export only works in a docbook contents view"))
    (let* ((order (arxana-browser--docbook-contents-order-get book))
           (source (arxana-browser--docbook-contents-order-source-get book))
           (dirty (arxana-browser--docbook-contents-dirty-p book))
           (order (and (eq source :manual) (listp order) (delq nil order)))
           (needs-sync (and order dirty)))
      (when needs-sync
        (arxana-browser--docbook-sync-order book order t))
      (arxana-docbook-export-pdf-book book nil nil order))))

(provide 'arxana-browser-docbook)
;;; arxana-browser-docbook.el ends here
