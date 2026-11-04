;;; arxana-browser-missions.el --- Mission Control browser views -*- lexical-binding: t; -*-

;;; Commentary:
;; META work-field browsing for Arxana, backed by the exact War Machine
;; outer-policy preview at GET /api/alpha/wm/meta-preview.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'url)
(require 'url-http)

(declare-function arxana-browser--render "arxana-browser-core")
(defvar arxana-browser--stack)

(defcustom arxana-mission-control-server "http://localhost:7070"
  "Base URL (host:port only, no path suffix) for the Mission Control HTTP
server.  Distinct from `arxana-evidence-server' — Mission Control runs in
futon3c (default port 7070), whereas the evidence server runs in futon1a
(default port 7071).  Mission inventory queries are issued against
`<server>/api/alpha/wm/meta-preview'."
  :type 'string
  :group 'arxana-missions)

(defcustom arxana-missions-meta-endpoint "/api/alpha/wm/meta-latest"
  "Path of the META field endpoint.  The default serves the latest click's
retained outer-task-selection receipt without recomputing it.  Set it to
\"/api/alpha/wm/meta-preview\" to recompute the field live (slow)."
  :type 'string
  :group 'arxana-missions)

(defcustom arxana-missions-substrate-server "http://localhost:7073"
  "Base URL for substrate-2's entity API.
`futon-find-work' reads the durable work census here; it does not depend on a
recent War Machine selection receipt being present."
  :type 'string
  :group 'arxana-missions)

(defun arxana-missions-browse-live-preview ()
  "Open the META field recomputed live by meta-preview (blocks ~90 s)."
  (interactive)
  (let ((arxana-missions-meta-endpoint "/api/alpha/wm/meta-preview"))
    (setq arxana-browser--stack
          (cons (list :view 'missions-portfolio :label "META (live preview)")
                arxana-browser--stack))
    (arxana-browser--render)))

(defun arxana-missions--server ()
  "Return the Mission Control server base URL."
  arxana-mission-control-server)

(defun arxana-missions--name (value)
  "Render keyword, symbol, or JSON string VALUE without inventing semantics."
  (cond ((keywordp value) (substring (symbol-name value) 1))
        ((symbolp value) (symbol-name value))
        ((stringp value) value)
        (t (format "%s" value))))

(defun arxana-missions--excluded-status (row)
  "Status cell for excluded ROW: \"x \" plus the reason's family.
The family is the part of the reason before \"/\" (\"requisition\",
\"repair-finding\", \"pipeline\"), or the whole reason when it has none."
  (let ((reason (arxana-missions--name (plist-get row :ineligible-reason))))
    (concat "x " (car (split-string (string-remove-prefix ":" reason) "/")))))

(defun arxana-missions--plist-keys (plist)
  "Return the keys of PLIST in their retained order."
  (let (keys)
    (while plist
      (push (car plist) keys)
      (setq plist (cddr plist)))
    (nreverse keys)))

(defun arxana-missions--fetch-snapshot ()
  "Fetch the live Mission Control snapshot, including census metadata."
  (let* ((base (replace-regexp-in-string "/\\'" "" (arxana-missions--server)))
         (url (concat base arxana-missions-meta-endpoint))
         (url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         ;; meta-latest reads the latest click's retained receipt (under 1 s);
         ;; meta-preview recomputes the field (~85 s on 2026-10-25).
         (buffer (url-retrieve-synchronously
                  url t t (if (string-suffix-p "meta-preview" url) 90 15))))
    (unless buffer
      (user-error "Mission inventory request failed"))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "\n\n" nil 'move)
      (let* ((body (buffer-substring-no-properties (point) (point-max)))
             (parsed (condition-case nil
                         (if (fboundp 'json-parse-string)
                             (json-parse-string body
                                               :object-type 'plist
                                               :array-type 'list
                                               :null-object nil
                                               :false-object nil)
                           (let ((json-object-type 'plist)
                                 (json-array-type 'list)
                                 (json-false nil)
                                 (json-null nil))
                             (json-read-from-string body)))
                       (error nil))))
        (kill-buffer buffer)
        (unless (listp parsed)
          (user-error "Mission inventory response was not an object"))
        (let* ((receipt (plist-get parsed :receipt))
               (_ (when-let* ((absence (plist-get receipt :absent)))
                    (user-error "META projection unavailable: %s"
                                (arxana-missions--name absence))))
               (support (plist-get receipt :support))
               (excluded (plist-get receipt :excluded))
               (selection (plist-get (plist-get receipt :policy) :meta-selection))
               (ranking (plist-get selection :ranking))
               (by-id (make-hash-table :test 'equal))
               rows)
          (dolist (row support) (puthash (plist-get row :id) row by-id))
          ;; A retained click receipt carries only the selected policy, not a
          ;; ranking: list the chosen target first, then the other eligible rows.
          (unless ranking
            (let ((chosen-id (plist-get (plist-get selection :selected) :target-id)))
              (dolist (task (append (seq-filter (lambda (r) (equal (plist-get r :id) chosen-id)) support)
                                    (seq-remove (lambda (r) (equal (plist-get r :id) chosen-id)) support)))
                (push (list :mission/id (plist-get task :id)
                            :mission/repo (arxana-missions--name (plist-get task :kind))
                            :mission/status (if (equal (plist-get task :id) chosen-id) "chosen" "eligible")
                            :mission/title (if (equal (plist-get task :id) chosen-id)
                                               (format "selected policy %s; G %s"
                                                       (plist-get (plist-get selection :selected) :policy-id)
                                                       (plist-get (plist-get selection :selected) :G))
                                             "")
                            :mission/path (plist-get (plist-get task :source) :path))
                      rows))))
          (dolist (ranked ranking)
            (let* ((id (plist-get ranked :id))
                   (task (gethash id by-id)))
              (push (list :mission/id id :mission/repo (arxana-missions--name (plist-get task :kind))
                          :mission/status (format "#%s" (plist-get ranked :rank))
                          :mission/title (format "%s; pairwise losses %s"
                                                 (mapconcat #'arxana-missions--name
                                                            (arxana-missions--plist-keys
                                                             (plist-get ranked :channels)) ", ")
                                                 (plist-get ranked :pairwise-losses))
                          :mission/path (plist-get (plist-get task :source) :path))
                    rows)))
          ;; The reason family goes in the Status column so it stays visible
          ;; on a narrow frame; the full reason stays in Title (Joe, 2026-11-04).
          (dolist (row excluded)
            (push (list :mission/id (plist-get row :id)
                        :mission/repo (arxana-missions--name (plist-get row :kind))
                        :mission/status (arxana-missions--excluded-status row)
                        :mission/title (format "%s%s"
                                               (arxana-missions--name (plist-get row :ineligible-reason))
                                               (if-let* ((stage (plist-get row :exclusion-stage)))
                                                   (format " (%s stage)" (arxana-missions--name stage))
                                                 ""))
                        :mission/path (plist-get (plist-get row :source) :path)) rows))
          (list :missions (nreverse rows)
                :declared-count (+ (length support) (length excluded))
                :selection selection
                :source (plist-get parsed :source)
                :g-frontier (plist-get parsed :g-frontier)))))))

(defun arxana-missions--fetch ()
  "Fetch mission inventory from Mission Control. Returns mission maps."
  (plist-get (arxana-missions--fetch-snapshot) :missions))

(defun arxana-missions--substrate-json (path)
  "GET PATH from substrate-2 and return its JSON plist."
  (let* ((url-request-method "GET")
         ;; url.el writes its own Accept header after `url-request-extra-headers';
         ;; bind the controlling variable or substrate correctly serves EDN.
         (url-mime-accept-string "application/json")
         (base (string-remove-suffix "/" arxana-missions-substrate-server))
         (buffer (url-retrieve-synchronously (concat base path) t t 15)))
    (unless buffer (user-error "Substrate work inventory request failed"))
    (with-current-buffer buffer
      (unwind-protect
          (progn
            (goto-char (point-min))
            ;; `url-retrieve-synchronously' normalises the header separator in
            ;; its response buffer; use the same boundary as the META reader.
            (unless (re-search-forward "\n\n" nil t)
              (user-error "Malformed substrate work inventory response"))
            (json-parse-string (buffer-substring-no-properties (point) (point-max))
                               :object-type 'plist :array-type 'list
                               :null-object nil :false-object nil))
        (kill-buffer buffer)))))

(defun arxana-missions--repo-from-entity-name (name)
  "Recover the checkout name from substrate entity NAME."
  (when (and (stringp name) (string-match "\\`\\([^/]+\\)/" name))
    (replace-regexp-in-string "-d[0-9]*\\'" "" (match-string 1 name))))

(defun arxana-missions--conventional-work-path (id repo)
  "Resolve work ID under REPO using declaration locations, without searching."
  (when (and (stringp id) (stringp repo))
    (let* ((root (expand-file-name repo "/home/joe/code/"))
           (dirs (pcase (substring id 0 (min 2 (length id)))
                   ("M-" '("holes/missions" "holes"))
                   ("E-" '("holes/excursions" "analysis/excursions" "holes"))
                   ("T-" '("holes/tickets" "holes"))
                   ("A-" '("holes/assignments" "holes"))
                   (_ '("holes")))))
      (seq-find #'file-exists-p
                (mapcar (lambda (dir)
                          (expand-file-name (concat id ".md")
                                            (expand-file-name dir root)))
                        dirs)))))

(defun arxana-missions--substrate-work-kind (entity-type)
  "Return a display kind for substrate ENTITY-TYPE."
  (car (split-string (or entity-type "work") "/")))

(defun arxana-missions--substrate-work-rows (entity-type)
  "Read one ENTITY-TYPE population from substrate-2 as META-shaped rows."
  (let* ((path (format "/api/alpha/entities?type=%s&limit=1000&include-total=false"
                       (url-hexify-string entity-type)))
         (entities (plist-get (arxana-missions--substrate-json path) :entities)))
    (delq
     nil
     (mapcar
      (lambda (entity)
        (let* ((id (plist-get entity :entity/external-id))
               (props (plist-get entity :entity/props))
               (repo (or (plist-get props :provenance/repo)
                         (arxana-missions--repo-from-entity-name
                          (plist-get entity :entity/name))))
               (path (or (plist-get props :provenance/path)
                         (arxana-missions--conventional-work-path id repo)))
               (kind (arxana-missions--substrate-work-kind entity-type))
               (status (or (plist-get props :mission/status-class)
                           (plist-get props :mission/status)
                           (plist-get props :excursion/status)
                           "known")))
          (when (and (stringp id) (string-match-p "\\`[META]-" id))
            (list :mission/id id :mission/repo kind
                  :mission/status status :mission/path path))))
      entities))))

(defun arxana-missions--fetch-work-inventory ()
  "Return the known substrate-2 M/E/T/A work population.
Types not yet populated simply contribute no rows."
  (let ((types '("mission" "excursion/doc" "ticket/doc" "assignment/doc")))
    (sort (delete-dups (mapcan #'arxana-missions--substrate-work-rows types))
          (lambda (a b) (string< (plist-get a :mission/id)
                                 (plist-get b :mission/id))))))

(defun arxana-missions--resolve-work-path (work)
  "Return WORK's canonical path, resolving a missing path by exact ID.
The inventory projections are intentionally compact and older excursion rows
do not carry provenance.  Their canonical scope entity does carry
`mission/path', so consult that one record only after the operator selects it."
  (or (plist-get work :mission/path)
      (let* ((id (plist-get work :mission/id))
             (response (and id
                            (arxana-missions--substrate-json
                             (format "/api/alpha/entity/%s"
                                     (url-hexify-string id)))))
             (props (plist-get (plist-get response :entity) :props)))
        (or (plist-get props :provenance/path)
            (plist-get props :mission/path)
            (plist-get props :excursion/path)))))

(defun arxana-missions--work-candidates (missions)
  "Return completion alist for MISSIONS from the META work field.
The ID leads each display string so ordinary completion styles can narrow on
the name exactly as a Projectile file command does.  Status and kind remain
visible context; the associated value is the unmodified META row."
  (mapcar
   (lambda (work)
     (let ((id (or (plist-get work :mission/id) "?"))
           (status (or (plist-get work :mission/status) "?"))
           (kind (or (plist-get work :mission/repo) "?")))
       (cons (format "%s  [%s · %s]" id status kind) work)))
   missions))

(defun arxana-missions--work-id-at-point (&optional radius)
  "Return the nearest M/E/T/A identifier around point.
Search RADIUS characters on either side (default 500).  An identifier that
contains point wins; otherwise choose the one with the nearest boundary."
  (let* ((origin (point))
         (radius (or radius 500))
         (start (max (point-min) (- origin radius)))
         (end (min (point-max) (+ origin radius)))
         best best-distance)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\_<[META]-[[:alnum:]_.-]+\\_>" end t)
        (let* ((beg (match-beginning 0))
               (finish (match-end 0))
               (distance (cond ((< origin beg) (- beg origin))
                               ((> origin finish) (- origin finish))
                               (t 0))))
          (when (or (null best-distance) (< distance best-distance))
            (setq best (match-string-no-properties 0)
                  best-distance distance)))))
    best))

;;;###autoload
(defun futon-find-work ()
  "Find a declared work item from Arxana's live META field.

Completion covers the same M/E/T/A population shown by META and opens the
canonical source path retained in substrate, rather than searching for text
references to the work ID."
  (interactive)
  (let* ((near-id (arxana-missions--work-id-at-point))
         (candidates (arxana-missions--work-candidates
                      (arxana-missions--fetch-work-inventory)))
         (_ (unless candidates (user-error "META contains no work items")))
         (near-choice (and near-id
                           (car (seq-find
                                 (lambda (candidate)
                                   (string-prefix-p (concat near-id "  [")
                                                    (car candidate)))
                                 candidates))))
         (choice (completing-read "Find work: " candidates nil t
                                  (unless near-choice near-id) nil near-choice))
         (work (cdr (assoc choice candidates)))
         (path (arxana-missions--resolve-work-path work)))
    (unless (and (stringp path) (not (string-empty-p path)))
      (user-error "META has no source path for %s"
                  (plist-get work :mission/id)))
    (unless (file-exists-p path)
      (user-error "META source does not exist locally: %s" path))
    (find-file path)))

(defalias 'arxana-find-work #'futon-find-work)

(defun arxana-missions--census-item (snapshot)
  "Build a visible census/provenance row from live SNAPSHOT."
  (let* ((missions (plist-get snapshot :missions))
         (actual-count (length missions))
         (declared-count (plist-get snapshot :declared-count))
         (repos (delete-dups
                 (delq nil (mapcar (lambda (mission)
                                     (plist-get mission :mission/repo))
                                   missions))))
         (coherent (or (null declared-count) (= declared-count actual-count))))
    (list :type 'info
          :label (format "Live META field: %d M/E/T/A tasks (%d kinds)"
                         actual-count (length repos))
          :description (if coherent
                           (let ((source (plist-get snapshot :source)))
                             (if source
                                 (format "Source: latest click receipt, %s %s (written %s)"
                                         (plist-get source :tree) (plist-get source :attempt)
                                         (plist-get source :written-at))
                               (format "Source: GET %s; ordering is the WM receipt"
                                       arxana-missions-meta-endpoint)))
                         (format "Census mismatch: endpoint declared %s, returned %d"
                                 declared-count actual-count))
          :census-coherent coherent
          :mission-count actual-count
          :repo-count (length repos))))

(defun arxana-missions--g-frontier-item (snapshot)
  "An info row naming every candidate the click scored with G, or nil."
  (when-let* ((f (plist-get snapshot :g-frontier))
              (entries (plist-get f :entries)))
    (list :type 'info
          :label (format "G frontier: %d scored of the eligible field (horizon %s)"
                         (length entries) (plist-get f :horizon))
          :description
          (mapconcat (lambda (e)
                       (format "#%s %s G=%s" (or (plist-get e :rank) "-")
                               (plist-get e :target-id)
                               (if (numberp (plist-get e :G))
                                   (format "%.4f" (plist-get e :G))
                                 (or (plist-get e :reason) "?"))))
                     entries "; "))))

(defun arxana-missions--status-sort-key (status)
  "Sort key for mission status — active first, archived/nonstarter last.
Slots `stale-in-progress' (derived server-side when an in-progress
mission's source file is older than 7 days) just below the actively-
in-progress band so untouched work surfaces visibly. Slots `archived'
just above `nonstarter' so closed-but-not-failed work sits at the
bottom of the active range."
  (cond
   ((member status '("in-progress" "testing")) 0)
   ((string= status "stale-in-progress") 1)
   ((member status '("ready" "identify" "map" "derive")) 2)
   ((string= status "blocked") 3)
   ((string= status "deferred") 4)
   ((string= status "complete") 5)
   ((string= status "archived") 6)
   ((string= status "nonstarter") 7)
   (t 4)))

;; =============================================================================
;; Menu items (top-level entry from evidence-home or main menu)
;; =============================================================================

(defun arxana-browser-missions-menu-items ()
  "Return menu items for the missions section."
  (list (list :type 'menu
              :label "Mission Portfolio"
              :description "Cross-repo mission inventory from Mission Control."
              :view 'missions-portfolio)
        (list :type 'menu
              :label "Missions by Status"
              :description "Group missions by lifecycle status."
              :view 'missions-by-status)))

;; =============================================================================
;; Portfolio view — grouped by repo
;; =============================================================================

(defun arxana-browser--missions-portfolio-format ()
  [("Status" 15 t)
   ("Repo" 10 t)
   ("Mission" 40 t)
   ("Title" 0 nil)])

(defun arxana-browser--missions-portfolio-row (item)
  (if (eq (plist-get item :type) 'info)
      (vector "!" "" (or (plist-get item :label) "")
              (or (plist-get item :description) ""))
    (vector (or (plist-get item :status) "?")
            (or (plist-get item :repo) "?")
            (or (plist-get item :mission-id) "?")
            (or (plist-get item :title) ""))))

(defun arxana-browser--missions-portfolio-items ()
  "Fetch and render the full mission portfolio."
  (condition-case err
      (let* ((snapshot (arxana-missions--fetch-snapshot))
             (missions (plist-get snapshot :missions)))
        (if missions
            (append (delq nil (list (arxana-missions--census-item snapshot)
                                    (arxana-missions--g-frontier-item snapshot)))
                  (mapcar (lambda (m)
                            (list :type 'mission-entry
                            :mission-id (or (plist-get m :mission/id) "?")
                            :status (or (plist-get m :mission/status) "?")
                            :repo (or (plist-get m :mission/repo) "?")
                            :title (or (plist-get m :mission/title) "")
                            :path (plist-get m :mission/path)
                            :label (or (plist-get m :mission/id) "?")
                            :description (format "%s — %s"
                                                 (or (plist-get m :mission/status) "?")
                                                 (or (plist-get m :mission/title) ""))))
                          missions))
          (list (list :type 'info
                      :label "No missions found"
                      :description "Mission scan returned empty."))))
    (error
     (list (list :type 'info
                 :label "Missions unavailable"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; By-status view — grouped by status
;; =============================================================================

(defun arxana-browser--missions-by-status-format ()
  [("Count" 6 t)
   ("Status" 15 t)
   ("Missions" 0 nil)])

(defun arxana-browser--missions-by-status-row (item)
  (vector (or (plist-get item :count-str) "")
          (or (plist-get item :status) "?")
          (or (plist-get item :mission-list) "")))

(defun arxana-browser--missions-by-status-items ()
  "Group missions by status."
  (condition-case err
      (let* ((missions (arxana-missions--fetch))
             (by-status (make-hash-table :test 'equal)))
        (dolist (m missions)
          (let* ((status (or (plist-get m :mission/status) "unknown"))
                 (existing (gethash status by-status)))
            (puthash status (cons m existing) by-status)))
        (let (rows)
          (maphash
           (lambda (status group)
             (push (list :type 'missions-status-group
                         :status status
                         :count-str (format "%d" (length group))
                         :missions group
                         :mission-list (mapconcat
                                        (lambda (m) (or (plist-get m :mission/id) "?"))
                                        (seq-take group 5) ", ")
                         :label (format "%s (%d)" status (length group))
                         :description (format "Click to browse %d %s missions"
                                              (length group) status))
                   rows))
           by-status)
          (sort rows (lambda (a b)
                       (< (arxana-missions--status-sort-key (plist-get a :status))
                          (arxana-missions--status-sort-key (plist-get b :status)))))))
    (error
     (list (list :type 'info
                 :label "Missions unavailable"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Status group drill-down
;; =============================================================================

(defun arxana-browser--missions-status-group-items (context)
  "Show missions within a status group."
  (let ((missions (plist-get context :missions)))
    (if missions
        (mapcar (lambda (m)
                  (list :type 'mission-entry
                        :mission-id (or (plist-get m :mission/id) "?")
                        :status (or (plist-get m :mission/status) "?")
                        :repo (or (plist-get m :mission/repo) "?")
                        :title (or (plist-get m :mission/title) "")
                        :path (plist-get m :mission/path)
                        :label (or (plist-get m :mission/id) "?")
                        :description (format "[%s] %s"
                                             (or (plist-get m :mission/repo) "?")
                                             (or (plist-get m :mission/title) ""))))
                missions)
      (list (list :type 'info :label "Empty group" :description "No missions in this status.")))))

(defun arxana-browser-missions-menu ()
  "Show the key summary for Mission Control browser views."
  (interactive)
  (message "META: RET inspect mission · g refresh · q back · ? this menu"))

(defvar arxana-browser-missions--view-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" #'arxana-browser-missions-menu)
    map)
  "Keymap laid over Mission Control browser listings.")

(defun arxana-browser-missions--register-view-keymaps ()
  "Register the keymap owned by each Mission Control view."
  (when (fboundp 'arxana-browser-register-view-keymap)
    (dolist (view '(missions-portfolio missions-by-status missions-status-group))
      (arxana-browser-register-view-keymap
       view arxana-browser-missions--view-map))))

(with-eval-after-load 'arxana-browser-core
  (arxana-browser-missions--register-view-keymaps))
(arxana-browser-missions--register-view-keymaps)

;; =============================================================================
;; Open handlers
;; =============================================================================

(defun arxana-missions--fetch-wants (id path)
  "Fetch the read-only wants view for mission ID at absolute PATH."
  (let* ((base (replace-regexp-in-string "/\\'" "" (arxana-missions--server)))
         (url (format "%s/api/alpha/wm/mission-wants?id=%s&path=%s" base
                      (url-hexify-string id) (url-hexify-string path)))
         (url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (buffer (url-retrieve-synchronously url t t 60)))
    (unless buffer (user-error "Mission wants request failed"))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "\n\n" nil 'move)
      (prog1 (json-parse-string (buffer-substring-no-properties (point) (point-max))
                                :object-type 'plist :array-type 'list
                                :null-object nil :false-object nil)
        (kill-buffer buffer)))))

(defun arxana-missions--insert-want-rows (title rows fn)
  "Insert section TITLE listing ROWS, each rendered by FN."
  (insert (format "\n%s (%d)\n" title (length rows)))
  (if rows (dolist (row rows) (insert "  " (funcall fn row) "\n"))
    (insert "  none\n")))

(defun arxana-missions-show-wants (id path)
  "Show what the WM would see as wants selecting mission ID (doc at PATH)."
  (let* ((resp (arxana-missions--fetch-wants id path))
         (view (plist-get resp :view))
         (route (plist-get view :route))
         (buf (get-buffer-create (format "*WM wants: %s*" id))))
    (unless (plist-get resp :ok)
      (user-error "Mission wants: %s %s" (plist-get resp :error)
                  (or (plist-get resp :message) "")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "WM wants for %s  (read-only; nothing observed)\n" id))
        (insert (format "route: %s%s\n" (plist-get route :kind)
                        (if-let* ((a (plist-get route :seam-absent)))
                            (format "  (no seam document: %s)" (plist-get a :absence)) "")))
        (insert-button (format "source: %s" path)
                       'action (lambda (_) (find-file-other-window path)))
        (insert "\n")
        (arxana-missions--insert-want-rows
         "Admitted wants" (plist-get view :admitted)
         (lambda (w) (format "[%s] %s" (or (plist-get w :locator-class)
                                            "no locator: assembly refuses the target")
                             (plist-get w :stated))))
        (arxana-missions--insert-want-rows
         "Waiting" (plist-get view :waiting)
         (lambda (w) (format "[%s] %s" (plist-get w :reason) (plist-get w :stated))))
        (arxana-missions--insert-want-rows
         "Out of view" (plist-get view :out-of-view)
         (lambda (w) (format "[%s] %s" (plist-get w :reason) (or (plist-get w :token) ""))))
        (arxana-missions--insert-want-rows
         "Coverage questions" (plist-get view :coverage-questions)
         (lambda (q) (format "%S" q)))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun arxana-browser-missions-open-entry (item)
  "Open a mission entry: show its WM wants view, or its file if no id."
  (let ((path (plist-get item :path))
        (id (plist-get item :mission-id)))
    (cond ((and id path (stringp path) (file-exists-p path))
           (arxana-missions-show-wants id path))
          ((and path (stringp path) (file-exists-p path))
           (find-file-other-window path))
          (t (message "Mission: %s [%s] — %s" (or id "?")
                      (or (plist-get item :status) "?")
                      (or (plist-get item :title) ""))))))

(defun arxana-browser-missions-open-status-group (item)
  "Drill into a status group."
  (setq arxana-browser--stack
        (cons (list :view 'missions-status-group
                    :label (format "Missions: %s" (plist-get item :status))
                    :status (plist-get item :status)
                    :missions (plist-get item :missions))
              arxana-browser--stack))
  (arxana-browser--render))

(provide 'arxana-browser-missions)
;;; arxana-browser-missions.el ends here
