;;; arxana-browser-missions.el --- Mission Control browser views -*- lexical-binding: t; -*-

;;; Commentary:
;; Mission portfolio browsing for Arxana.
;; Provides inventory view grouped by repo and status,
;; backed by GET /api/alpha/missions.

;;; Code:

(require 'cl-lib)
(require 'json)
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
`<server>/api/alpha/missions'."
  :type 'string
  :group 'arxana-missions)

(defun arxana-missions--server ()
  "Return the Mission Control server base URL."
  arxana-mission-control-server)

(defun arxana-missions--fetch ()
  "Fetch mission inventory from Agency. Returns list of mission maps."
  (let* ((base (replace-regexp-in-string "/\\'" "" (arxana-missions--server)))
         (url (concat base "/api/alpha/missions"))
         (url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (buffer (url-retrieve-synchronously url t t 15)))
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
        (or (plist-get parsed :missions) '())))))

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
  (vector (or (plist-get item :status) "?")
          (or (plist-get item :repo) "?")
          (or (plist-get item :mission-id) "?")
          (or (plist-get item :title) "")))

(defun arxana-browser--missions-portfolio-items ()
  "Fetch and render the full mission portfolio."
  (condition-case err
      (let* ((missions (arxana-missions--fetch))
             (sorted (sort (copy-sequence missions)
                           (lambda (a b)
                             (let ((sa (arxana-missions--status-sort-key
                                        (or (plist-get a :mission/status) "")))
                                   (sb (arxana-missions--status-sort-key
                                        (or (plist-get b :mission/status) ""))))
                               (if (= sa sb)
                                   (string< (or (plist-get a :mission/repo) "")
                                            (or (plist-get b :mission/repo) ""))
                                 (< sa sb)))))))
        (if sorted
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
                    sorted)
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

;; =============================================================================
;; Open handlers
;; =============================================================================

(defun arxana-browser-missions-open-entry (item)
  "Open a mission entry — navigate to its file if available."
  (let ((path (plist-get item :path)))
    (if (and path (stringp path) (file-exists-p path))
        (find-file-other-window path)
      (message "Mission: %s [%s] — %s"
               (or (plist-get item :mission-id) "?")
               (or (plist-get item :status) "?")
               (or (plist-get item :title) "")))))

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
