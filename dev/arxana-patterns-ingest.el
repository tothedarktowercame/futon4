;;; arxana-patterns-ingest.el --- Import flexiarg pattern libraries -*- lexical-binding: t; -*-

;;; Commentary:
;; Read pattern `.flexiarg` files from a directory, convert each entry
;; into Futon pattern/component entities, and wire them together using
;; `:pattern/includes` relations.  An optional pattern-language entity
;; can also be created so callers can browse the ordered collection via
;; Futon's `/ego` endpoint.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'arxana-store)

(defgroup arxana-patterns-ingest nil
  "Settings for ingesting flexiarg pattern libraries."
  :group 'arxana)

(defcustom arxana-patterns-ingest-ego-limit 128
  "How many links to request when fetching `/ego` data during ingest."
  :type 'integer
  :group 'arxana-patterns-ingest)

(defcustom arxana-patterns-ingest-language-relation ":pattern-language/includes"
  "Relation label used when wiring languages to their member patterns."
  :type 'string
  :group 'arxana-patterns-ingest)

(defcustom arxana-patterns-ingest-language-source-relation ":language/source"
  "Relation label used when tagging pattern languages with their source."
  :type 'string
  :group 'arxana-patterns-ingest)

(defcustom arxana-patterns-ingest-language-status-relation ":language/status"
  "Relation label used when tagging pattern languages with their status."
  :type 'string
  :group 'arxana-patterns-ingest)

(defcustom arxana-patterns-ingest-language-catalog-name "pattern-language/catalog"
  "Entity name that collects every known pattern language."
  :type 'string
  :group 'arxana-patterns-ingest)

(defcustom arxana-patterns-ingest-language-catalog-relation ":language/catalog"
  "Relation label used when linking languages into the catalog."
  :type 'string
  :group 'arxana-patterns-ingest)

(defcustom arxana-patterns-ingest-default-ad-hoc-status "pattern-language/status/draft"
  "Default status entity name for ad-hoc pattern languages."
  :type 'string
  :group 'arxana-patterns-ingest)

(defcustom arxana-patterns-ingest-default-canonical-status "pattern-language/status/published"
  "Default status entity name for canonical pattern languages (futon3/library)."
  :type 'string
  :group 'arxana-patterns-ingest)

(defun arxana-patterns-ingest--flexiarg-files (directory)
  "Return absolute `.flexiarg` paths under DIRECTORY sorted by mtime (desc)."
  (let* ((dir (file-name-as-directory (expand-file-name directory)))
         (candidates (directory-files dir t "\\.flexiarg\\'" t)))
    (seq-sort (lambda (a b)
                (let ((ta (nth 5 (file-attributes a 'string)))
                      (tb (nth 5 (file-attributes b 'string))))
                  (time-less-p tb ta)))
              candidates)))

(defun arxana-patterns-ingest--canonical-directory-p (directory)
  "Return non-nil when DIRECTORY appears to live under futon3/library."
  (let ((path (file-name-as-directory (expand-file-name directory))))
    (string-match-p (regexp-quote "/futon3/library/") path)))

(defun arxana-patterns-ingest--language-source-name (directory)
  "Return the classification entity name for DIRECTORY's source."
  (if (arxana-patterns-ingest--canonical-directory-p directory)
      "pattern-language/source/futon3-library"
    "pattern-language/source/ad-hoc"))

(defun arxana-patterns-ingest--language-status-name (directory explicit)
  "Return the classification entity for language status.
When EXPLICIT is non-nil, return it; otherwise derive from DIRECTORY."
  (cond
   ((and explicit (not (string-empty-p explicit))) explicit)
   ((arxana-patterns-ingest--canonical-directory-p directory)
    arxana-patterns-ingest-default-canonical-status)
   (t arxana-patterns-ingest-default-ad-hoc-status)))

(defun arxana-patterns-ingest--trim-empty-lines (lines)
  "Remove blank lines from the start/end of LINES."
  (let ((result lines))
    (while (and result (string-match-p "\\`[[:space:]]*\\'" (car result)))
      (setq result (cdr result)))
    (let ((rev (nreverse result)))
      (while (and rev (string-match-p "\\`[[:space:]]*\\'" (car rev)))
        (setq rev (cdr rev)))
      (nreverse rev))))

(defun arxana-patterns-ingest--slugify (text)
  "Return a lowercase slug derived from TEXT."
  (let* ((lower (downcase (or text "component")))
         (clean (replace-regexp-in-string "[^a-z0-9]+" "-" lower))
         (trimmed (string-trim clean "-" "-")))
    (if (string-empty-p trimmed)
        "component"
      trimmed)))

(defun arxana-patterns-ingest--extract-id (node)
  "Recursively locate an :id/:entity/id entry inside NODE."
  (cond
   ((null node) nil)
   ((and (consp node) (keywordp (car node))
         (memq (car node) '(:id :entity/id)))
    (cdr node))
   ((consp node)
    (or (arxana-patterns-ingest--extract-id (car node))
        (arxana-patterns-ingest--extract-id (cdr node))))
   (t nil)))

(defun arxana-patterns-ingest--lookup-id (name)
  "Fetch NAME via `/ego` and return the associated entity id."
  (when name
    (let* ((response (ignore-errors (arxana-store-ego name 1)))
           (ego (and response (alist-get :ego response)))
           (entity (and ego (alist-get :entity ego))))
      (arxana-patterns-ingest--extract-id entity))))

(defun arxana-patterns-ingest--relation-text (value)
  (let ((raw (cond
              ((keywordp value) (symbol-name value))
              ((symbolp value) (symbol-name value))
              ((stringp value) value)
              (t nil))))
    (when raw
      (if (and (> (length raw) 0) (eq (aref raw 0) ?:))
          (substring raw 1)
        raw))))

(defun arxana-patterns-ingest--relation-match-p (value target)
  "Return non-nil when VALUE and TARGET represent the same relation label."
  (let ((lhs (arxana-patterns-ingest--relation-text value))
        (rhs (arxana-patterns-ingest--relation-text target)))
    (and lhs rhs (string= lhs rhs))))

(defun arxana-patterns-ingest--existing-targets (name relation)
  "Return a hash table of entity ids linked from NAME via RELATION."
  (let* ((response (ignore-errors (arxana-store-ego name arxana-patterns-ingest-ego-limit)))
         (ego (and response (alist-get :ego response)))
         (links (and ego (or (alist-get :outgoing ego)
                             (let ((l (alist-get :links ego)))
                               (and l (alist-get :outgoing l))))))
         (table (make-hash-table :test 'equal)))
    (dolist (link links)
      (when (arxana-patterns-ingest--relation-match-p (alist-get :relation link) relation)
        (let* ((entity (alist-get :entity link))
               (id (arxana-patterns-ingest--extract-id entity)))
          (when id
            (puthash id t table)))))
    table))

(defun arxana-patterns-ingest--ensure-tag (language-name language-id relation target-name target-type)
  "Ensure LANGUAGE-ID points at TARGET-NAME via RELATION."
  (when (and language-name language-id relation target-name)
    (let* ((target-response (arxana-store-ensure-entity :name target-name
                                                        :type target-type
                                                        :source (format "%s classification" relation)
                                                        :external-id target-name))
           (target-id (or (arxana-patterns-ingest--extract-id target-response)
                          (arxana-patterns-ingest--lookup-id target-name)))
           (existing (arxana-patterns-ingest--existing-targets language-name relation)))
      (when (and target-id (not (gethash target-id existing)))
        (arxana-store-create-relation :src language-id
                                      :dst target-id
                                      :label relation)
        (puthash target-id t existing)))))

(defun arxana-patterns-ingest--ensure-catalog-link (language-name language-id)
  "Ensure LANGUAGE-ID is reachable from the central catalog."
  (when (and language-name language-id)
    (let* ((catalog-name arxana-patterns-ingest-language-catalog-name)
           (response (arxana-store-ensure-entity :name catalog-name
                                                 :type "pattern/language-catalog"
                                                 :source "Pattern languages"))
           (catalog-id (or (arxana-patterns-ingest--extract-id response)
                           (arxana-patterns-ingest--lookup-id catalog-name)))
           (existing (when catalog-id
                       (arxana-patterns-ingest--existing-targets
                        catalog-name arxana-patterns-ingest-language-catalog-relation))))
      (when (and catalog-id existing (not (gethash language-id existing)))
        (arxana-store-create-relation :src catalog-id
                                      :dst language-id
                                      :label arxana-patterns-ingest-language-catalog-relation)
        (puthash language-id t existing)))))

(defun arxana-patterns-ingest--ego-outgoing (ego)
  (or (alist-get :outgoing ego)
      (let ((links (alist-get :links ego)))
        (and links (alist-get :outgoing links)))))

(defun arxana-patterns-ingest--link-target-name (link)
  (let* ((entity (alist-get :entity link)))
    (or (alist-get :name entity)
        (alist-get :entity/name entity)
        (alist-get :ident entity)
        (alist-get :entity/ident entity))))

(defun arxana-patterns-ingest--language-metadata (language-name)
  "Return plist of metadata for LANGUAGE-NAME using `/ego`."
  (let* ((response (ignore-errors (arxana-store-ego language-name arxana-patterns-ingest-ego-limit)))
         (ego (and response (alist-get :ego response)))
         (entity (and ego (alist-get :entity ego)))
         (title (or (alist-get :external-id entity)
                    (alist-get :entity/external-id entity)
                    language-name))
         (outgoing (arxana-patterns-ingest--ego-outgoing ego))
         (source nil)
         (status nil)
         (count 0))
    (dolist (link outgoing)
      (let ((rel (alist-get :relation link)))
        (cond
         ((arxana-patterns-ingest--relation-match-p rel arxana-patterns-ingest-language-relation)
          (setq count (1+ count)))
         ((and (not source)
               (arxana-patterns-ingest--relation-match-p rel arxana-patterns-ingest-language-source-relation))
          (setq source (arxana-patterns-ingest--link-target-name link)))
         ((and (not status)
               (arxana-patterns-ingest--relation-match-p rel arxana-patterns-ingest-language-status-relation))
          (setq status (arxana-patterns-ingest--link-target-name link))))))
    (list :name language-name
          :title title
          :source source
          :status status
          :count count)))

;;;###autoload
(defun arxana-patterns-list-languages ()
  "Display a summary of known pattern languages and their status."
  (interactive)
  (unless (arxana-store-sync-enabled-p)
    (user-error "Futon sync is disabled; enable futon4-enable-sync first"))
  (let* ((catalog arxana-patterns-ingest-language-catalog-name)
         (response (ignore-errors (arxana-store-ego catalog arxana-patterns-ingest-ego-limit)))
         (ego (and response (alist-get :ego response)))
         (outgoing (and ego (arxana-patterns-ingest--ego-outgoing ego)))
         (language-names (delq nil
                               (mapcar (lambda (link)
                                         (when (arxana-patterns-ingest--relation-match-p
                                                (alist-get :relation link)
                                                arxana-patterns-ingest-language-catalog-relation)
                                           (arxana-patterns-ingest--link-target-name link)))
                                       outgoing))))
    (if (seq-empty-p language-names)
        (message "No pattern languages are registered yet")
      (let* ((rows (mapcar #'arxana-patterns-ingest--language-metadata language-names))
             (buffer (get-buffer-create "*Arxana Pattern Languages*")))
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "%-32s %-24s %-24s %s\n"
                            "Language" "Source" "Status" "Patterns"))
            (insert (make-string 90 ?-) "\n")
            (dolist (row rows)
              (insert (format "%-32s %-24s %-24s %d\n"
                              (plist-get row :name)
                              (or (plist-get row :source) "?")
                              (or (plist-get row :status) "?")
                              (plist-get row :count))))
            (goto-char (point-min))
            (special-mode)))
        (display-buffer buffer)))))

(defun arxana-patterns-ingest--section (label lines)
  (let* ((clean-lines (arxana-patterns-ingest--trim-empty-lines (nreverse lines)))
         (text (string-trim-right (mapconcat #'identity clean-lines "\n"))))
    (list :label label
          :slug (arxana-patterns-ingest--slugify label)
          :text text)))

(defun arxana-patterns-ingest--parse-flexiarg (path)
  "Parse PATH and return a plist describing the pattern contents."
  (with-temp-buffer
    (insert-file-contents path)
    (goto-char (point-min))
    (let ((meta nil)
          (sections nil)
          (current nil))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                    (line-end-position))))
          (cond
           ((string-match "^@\\([^[:space:]]+\\)\\s-+\\(.*\\)$" line)
            (let ((key (intern (concat ":" (downcase (match-string 1 line)))))
                  (value (string-trim (match-string 2 line))))
              (setq meta (plist-put meta key value))))
           ((string-match "^[[:space:]]*\([!+]\)\\s-*\\([^:]+\\):?\\s*$" line)
            (when current
              (push (arxana-patterns-ingest--section (plist-get current :label)
                                                     (plist-get current :lines))
                    sections))
            (setq current (list :label (string-trim (match-string 2 line))
                                :lines nil)))
           ((and current)
            (setf (plist-get current :lines)
                  (cons line (plist-get current :lines))))))
        (forward-line 1))
      (when current
        (push (arxana-patterns-ingest--section (plist-get current :label)
                                               (plist-get current :lines))
              sections))
      (let ((name (plist-get meta :arg)))
        (unless (and name (not (string-empty-p name)))
          (user-error "Missing @arg in %s" path))
        (let* ((title (plist-get meta :title))
               (ordered (nreverse sections))
               (first (car ordered)))
          (list :name name
                :title title
                :summary (or (plist-get first :text) "")
                :components ordered
                :meta meta))))))

(defun arxana-patterns-ingest--component-name (pattern index component)
  (format "%s/%02d-%s" pattern index (plist-get component :slug)))

(defun arxana-patterns-ingest--ensure-pattern (pattern)
  (let* ((response (arxana-store-ensure-entity :name (plist-get pattern :name)
                                               :type "pattern/library"
                                               :source (plist-get pattern :summary)
                                               :external-id (plist-get pattern :title)))
         (id (or (arxana-patterns-ingest--extract-id response)
                 (arxana-patterns-ingest--lookup-id (plist-get pattern :name)))))
    id))

(defun arxana-patterns-ingest--ensure-component (pattern-id pattern-name component index existing)
  (let* ((component-name (arxana-patterns-ingest--component-name pattern-name index component))
         (response (arxana-store-ensure-entity :name component-name
                                               :type "pattern/component"
                                               :source (plist-get component :text)
                                               :external-id (plist-get component :label)))
         (component-id (or (arxana-patterns-ingest--extract-id response)
                           (arxana-patterns-ingest--lookup-id component-name))))
    (when (and pattern-id component-id
               (or (not existing) (not (gethash component-id existing))))
      (arxana-store-create-relation :src pattern-id
                                    :dst component-id
                                    :label ":pattern/includes")
      (when existing
        (puthash component-id t existing)))
    component-id))

(defun arxana-patterns-ingest--ingest-file (path)
  "Ingest a single flexiarg PATH, returning plist with :name and :id."
  (let* ((data (arxana-patterns-ingest--parse-flexiarg path))
         (pattern-id (arxana-patterns-ingest--ensure-pattern data))
         (existing (arxana-patterns-ingest--existing-targets (plist-get data :name)
                                                             ":pattern/includes"))
         (index 1))
    (dolist (component (plist-get data :components))
      (arxana-patterns-ingest--ensure-component pattern-id (plist-get data :name)
                                                component index existing)
      (setq index (1+ index)))
    (list :name (plist-get data :name)
          :id pattern-id)))

(defun arxana-patterns-ingest--ensure-language (language-name language-title patterns directory language-status)
  "Ensure LANGUAGE-NAME exists and links to PATTERNS in order."
  (let* ((summary (format "Imported from %s" directory))
         (response (arxana-store-ensure-entity :name language-name
                                              :type "pattern/language"
                                              :source summary
               :external-id (or language-title language-name)))
         (language-id (or (arxana-patterns-ingest--extract-id response)
                          (arxana-patterns-ingest--lookup-id language-name)))
         (existing (when language-id
                     (or (arxana-patterns-ingest--existing-targets
                          language-name arxana-patterns-ingest-language-relation)
                         (make-hash-table :test 'equal))))
         (source-name (arxana-patterns-ingest--language-source-name directory))
         (status-name (arxana-patterns-ingest--language-status-name directory language-status)))
    (when (and language-id existing)
      (cl-loop for pattern in patterns
               for order from 1
               for pid = (plist-get pattern :id)
               when (and pid (not (gethash pid existing)))
               do (progn
                    (arxana-store-create-relation :src language-id
                                                  :dst pid
                                                  :label arxana-patterns-ingest-language-relation
                                                  :props (list (cons 'order order)))
                    (puthash pid t existing))))
      (arxana-patterns-ingest--ensure-tag language-name language-id
                                          arxana-patterns-ingest-language-source-relation
                                          source-name "pattern/language-source")
      (arxana-patterns-ingest--ensure-tag language-name language-id
                                          arxana-patterns-ingest-language-status-relation
                                          status-name "pattern/language-status")
      (arxana-patterns-ingest--ensure-catalog-link language-name language-id)))

;;;###autoload
(defun arxana-patterns-ingest-directory (directory &optional language-name language-title language-status)
  "Ingest `.flexiarg` files under DIRECTORY.
When LANGUAGE-NAME is non-nil, ensure a pattern-language entity links to
all imported patterns using reverse modification time order. LANGUAGE-TITLE
becomes the entity's external id. LANGUAGE-STATUS lets callers override the
auto-detected status classification."
  (interactive
   (let* ((dir (read-directory-name "Flexiarg directory: " nil nil t))
          (raw-name (string-trim (read-string "Pattern language name (blank to skip): ")))
          (name (unless (string-empty-p raw-name) raw-name))
          (title (when name
                   (let ((val (string-trim (read-string "Pattern language title (optional): "))))
                     (unless (string-empty-p val) val))))
          (status (when name
                    (let* ((default (arxana-patterns-ingest--language-status-name dir nil))
                           (prompt (format "Pattern language status (default %s): " default))
                           (val (string-trim (read-string prompt nil nil default))))
                      (unless (string-empty-p val) val)))))
     (list dir name title status)))
  (unless (arxana-store-sync-enabled-p)
    (user-error "Futon sync is disabled; enable futon4-enable-sync first"))
  (let* ((dir directory)
         (files (arxana-patterns-ingest--flexiarg-files dir)))
    (unless files
      (user-error "No .flexiarg files found in %s" dir))
    (let ((results nil))
      (dolist (file files)
        (push (arxana-patterns-ingest--ingest-file file) results))
      (setq results (nreverse results))
      (when language-name
        (arxana-patterns-ingest--ensure-language language-name language-title
                                                 results dir language-status))
      (message "Ingested %d patterns from %s" (length results) dir)
      (when language-name
        (message "Language %s id %s" language-name
                 (or (arxana-patterns-ingest--extract-id
                      (arxana-store-ensure-entity :name language-name))
                     (arxana-patterns-ingest--lookup-id language-name))))
      results)))

(provide 'arxana-patterns-ingest)

;;; arxana-patterns-ingest.el ends here
