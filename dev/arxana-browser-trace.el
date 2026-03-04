;;; arxana-browser-trace.el --- Trace browser for self-representing stack -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive drill-down views for the 6-gate trace chain.
;; Entry point: trace-home menu → trace-devmaps → trace-tensions → trace-gates.
;; Data comes from futon3c Mission Control: GET /mc/trace

;;; Code:

(require 'cl-lib)

;; ---------------------------------------------------------------------------
;; Faces
;; ---------------------------------------------------------------------------

(defface arxana-trace-pass-face
  '((t :foreground "#98c379"))
  "Face for passing trace gates."
  :group 'arxana-lab)

(defface arxana-trace-gap-face
  '((t :foreground "#e06c75"))
  "Face for gap trace gates."
  :group 'arxana-lab)

(defface arxana-trace-blocked-face
  '((t :foreground "#e5c07b"))
  "Face for blocked trace gates."
  :group 'arxana-lab)

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(defcustom arxana-trace-source-roots
  '("/home/joe/code/futon3c/src/"
    "/home/joe/code/futon1a/src/")
  "Directories to search when resolving classpath-relative source paths."
  :type '(repeat string)
  :group 'arxana-lab)

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defun arxana-trace--keyword-str (val)
  "Convert VAL to a plain string, stripping leading colon if keyword."
  (cond
   ((keywordp val) (substring (symbol-name val) 1))
   ((stringp val) val)
   ((null val) "")
   (t (format "%s" val))))

(defun arxana-trace--truncate (text max-len)
  "Truncate TEXT to MAX-LEN characters."
  (let ((value (or text "")))
    (if (> (length value) max-len)
        (concat (substring value 0 (max 0 (- max-len 1))) "…")
      value)))

(defun arxana-trace--fetch (endpoint)
  "Fetch ENDPOINT from futon3c. Returns parsed JSON plist."
  (arxana-browser--futon3c-fetch endpoint))

(defun arxana-trace--gate-indicator (status)
  "Return a propertized indicator string for gate STATUS."
  (let ((s (arxana-trace--keyword-str status)))
    (cond
     ((string= s "pass")
      (propertize "PASS" 'face 'arxana-trace-pass-face))
     ((string= s "gap")
      (propertize "間" 'face 'arxana-trace-gap-face))
     ((string= s "blocked")
      (propertize "関" 'face 'arxana-trace-blocked-face))
     (t s))))

(defun arxana-trace--gate-indicators (gates)
  "Build a 6-gate indicator string from GATES list."
  (mapconcat
   (lambda (g)
     (arxana-trace--gate-indicator (plist-get g :status)))
   gates
   " "))

(defun arxana-trace--gate-summary (gate)
  "Extract a short summary string from a GATE plist."
  (let* ((name (arxana-trace--keyword-str (plist-get gate :gate)))
         (status (arxana-trace--keyword-str (plist-get gate :status)))
         (data (plist-get gate :data)))
    (cond
     ((string= name "devmap")
      (if data
          (format "%s (%d comps, %d edges)"
                  (arxana-trace--keyword-str (plist-get data :state))
                  (or (plist-get data :component-count) 0)
                  (or (plist-get data :edge-count) 0))
        ""))
     ((string= name "component")
      (or (plist-get data :name) (arxana-trace--keyword-str (plist-get data :id))))
     ((string= name "coverage")
      (let ((missions (plist-get data :missions)))
        (if (and missions (> (length missions) 0))
            (format "%d missions" (length missions))
          (format "%s" (or (arxana-trace--keyword-str (plist-get data :tension-type)) "no coverage")))))
     ((string= name "evidence")
      (let ((missions (plist-get data :missions)))
        (if missions
            (format "%d missions" (length missions))
          "none")))
     ((string= name "reflection")
      (if (and data (plist-get data :ns))
          (let ((ns (arxana-trace--keyword-str (plist-get data :ns)))
                (vars (plist-get data :vars)))
            (format "%s (%d vars)" ns (length (or vars '()))))
        (if (and data (plist-get data :candidate-namespaces))
            (format "%d candidates" (length (plist-get data :candidate-namespaces)))
          "no ns")))
     ((string= name "source")
      (if data
          (or (plist-get data :file) "resolved")
        (if (string= status "blocked") "blocked" "")))
     (t ""))))

(defun arxana-trace--resolve-source-path (classpath-relative)
  "Resolve CLASSPATH-RELATIVE against `arxana-trace-source-roots`."
  (when classpath-relative
    (cl-loop for root in arxana-trace-source-roots
             for candidate = (expand-file-name classpath-relative root)
             when (file-exists-p candidate) return candidate)))

;; ---------------------------------------------------------------------------
;; View 1: trace-home (entry point menu)
;; ---------------------------------------------------------------------------

(defun arxana-browser-trace-home-items ()
  "Items for the trace home menu."
  (list (list :type 'menu
              :label "Trace by Devmap"
              :description "Overview: devmaps with tension counts and gate summaries."
              :view 'trace-devmaps)
        (list :type 'menu
              :label "All Tensions (traced)"
              :description "Flat list of all tensions with 6-gate chain status."
              :view 'trace-all-tensions)
        (list :type 'menu
              :label "All Components (full landscape)"
              :description "Trace every component in every devmap — covered and uncovered."
              :view 'trace-all-components)))

(defun arxana-browser-trace-home-format ()
  "Column format for trace-home view."
  (arxana-browser--menu-format))

(defun arxana-browser-trace-home-row (item)
  "Row for trace-home menu ITEM."
  (arxana-browser--menu-row item))

;; ---------------------------------------------------------------------------
;; View 2: trace-devmaps (grouped overview)
;; ---------------------------------------------------------------------------

(defun arxana-browser-trace-devmaps-items ()
  "Fetch trace data, group by devmap, return items."
  (condition-case err
      (let* ((payload (arxana-trace--fetch "/mc/trace"))
             (paths (plist-get payload :paths)))
        (if (not paths)
            (list (list :type 'info
                        :label "No trace paths"
                        :description "The trace API returned no paths."))
          ;; Group paths by devmap
          (let ((groups (make-hash-table :test 'equal)))
            (dolist (p paths)
              (let* ((tension (plist-get p :tension))
                     (devmap (arxana-trace--keyword-str
                              (plist-get tension :tension/devmap))))
                (puthash devmap (cons p (gethash devmap groups nil)) groups)))
            (let (result)
              (maphash
               (lambda (devmap dm-paths)
                 (let* ((total (length dm-paths))
                        (complete (cl-count-if
                                   (lambda (p) (eq (plist-get p :complete?) t))
                                   dm-paths))
                        (gaps (cl-count-if
                               (lambda (p) (plist-get p :gap-at))
                               dm-paths))
                        (blocked (cl-count-if
                                  (lambda (p) (plist-get p :blocked-at))
                                  dm-paths)))
                   (push (list :type 'trace-devmap
                               :label devmap
                               :description (format "%d tensions, %d complete" total complete)
                               :view 'trace-tensions
                               :devmap devmap
                               :paths (reverse dm-paths)
                               :tension-count total
                               :complete-count complete
                               :gap-count gaps
                               :blocked-count blocked)
                         result)))
               groups)
              (or (nreverse result)
                  (list (list :type 'info
                              :label "No devmaps"
                              :description "Trace returned paths but no devmaps found.")))))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch trace"
                 :description (format "Error: %s" (error-message-string err)))))))

(defun arxana-browser-trace-devmaps-format ()
  "Column format for trace-devmaps view."
  [("Devmap" 28 t)
   ("Tensions" 9 t)
   ("Complete" 9 t)
   ("Gaps" 5 t)
   ("Blocked" 8 t)])

(defun arxana-browser-trace-devmaps-row (item)
  "Row for trace-devmap ITEM."
  (let ((devmap (or (plist-get item :label) ""))
        (tensions (or (plist-get item :tension-count) 0))
        (complete (or (plist-get item :complete-count) 0))
        (gaps (or (plist-get item :gap-count) 0))
        (blocked (or (plist-get item :blocked-count) 0)))
    (vector (arxana-trace--truncate devmap 27)
            (format "%d" tensions)
            (propertize (format "%d" complete)
                        'face (if (= complete tensions)
                                  'arxana-trace-pass-face
                                'default))
            (propertize (format "%d" gaps)
                        'face (if (> gaps 0)
                                  'arxana-trace-gap-face
                                'default))
            (propertize (format "%d" blocked)
                        'face (if (> blocked 0)
                                  'arxana-trace-blocked-face
                                'default)))))

;; ---------------------------------------------------------------------------
;; View 2b: trace-all-tensions (flat list, fetched fresh)
;; ---------------------------------------------------------------------------

(defun arxana-browser-trace-all-tensions-items ()
  "Fetch trace data and return flat tension items."
  (condition-case err
      (let* ((payload (arxana-trace--fetch "/mc/trace"))
             (paths (plist-get payload :paths)))
        (if (not paths)
            (list (list :type 'info
                        :label "No trace paths"
                        :description "The trace API returned no paths."))
          (mapcar
           (lambda (p)
             (let* ((tension (plist-get p :tension))
                    (gates (plist-get p :gates))
                    (component (arxana-trace--keyword-str
                                (plist-get tension :tension/component)))
                    (devmap (arxana-trace--keyword-str
                             (plist-get tension :tension/devmap)))
                    (ttype (arxana-trace--keyword-str
                            (plist-get tension :tension/type))))
               (list :type 'trace-tension
                     :label component
                     :description (format "%s/%s" devmap component)
                     :tension tension
                     :gates gates
                     :component component
                     :devmap devmap
                     :tension-type ttype
                     :complete (plist-get p :complete?)
                     :gap-at (arxana-trace--keyword-str (plist-get p :gap-at))
                     :blocked-at (arxana-trace--keyword-str (plist-get p :blocked-at)))))
           paths)))
    (error
     (list (list :type 'info
                 :label "Failed to fetch trace"
                 :description (format "Error: %s" (error-message-string err)))))))

;; ---------------------------------------------------------------------------
;; View 2c: trace-all-components (full landscape, fetched from /mc/trace-all)
;; ---------------------------------------------------------------------------

(defun arxana-browser-trace-all-components-items ()
  "Fetch trace-all data (every component in every devmap), group by devmap."
  (condition-case err
      (let* ((payload (arxana-trace--fetch "/mc/trace-all"))
             (paths (plist-get payload :paths)))
        (if (not paths)
            (list (list :type 'info
                        :label "No components"
                        :description "The trace-all API returned no paths."))
          ;; Group paths by devmap
          (let ((groups (make-hash-table :test 'equal)))
            (dolist (p paths)
              (let* ((tension (plist-get p :tension))
                     (devmap (arxana-trace--keyword-str
                              (plist-get tension :tension/devmap))))
                (puthash devmap (cons p (gethash devmap groups nil)) groups)))
            (let (result)
              (maphash
               (lambda (devmap dm-paths)
                 (let* ((total (length dm-paths))
                        (complete (cl-count-if
                                   (lambda (p) (eq (plist-get p :complete?) t))
                                   dm-paths))
                        (gaps (cl-count-if
                               (lambda (p) (plist-get p :gap-at))
                               dm-paths))
                        (blocked (cl-count-if
                                  (lambda (p) (plist-get p :blocked-at))
                                  dm-paths)))
                   (push (list :type 'trace-devmap
                               :label devmap
                               :description (format "%d components, %d complete" total complete)
                               :view 'trace-tensions
                               :devmap devmap
                               :paths (reverse dm-paths)
                               :tension-count total
                               :complete-count complete
                               :gap-count gaps
                               :blocked-count blocked)
                         result)))
               groups)
              (or (sort (nreverse result)
                        (lambda (a b)
                          (string< (plist-get a :label) (plist-get b :label))))
                  (list (list :type 'info
                              :label "No devmaps"
                              :description "No devmaps found.")))))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch trace-all"
                 :description (format "Error: %s" (error-message-string err)))))))

(defun arxana-browser-trace-all-components-format ()
  "Column format for trace-all-components view."
  [("Devmap" 28 t)
   ("Comps" 6 t)
   ("Traced" 7 t)
   ("Gaps" 5 t)
   ("Blocked" 8 t)])

(defun arxana-browser-trace-all-components-row (item)
  "Row for trace-all-components ITEM."
  (let ((devmap (or (plist-get item :label) ""))
        (total (or (plist-get item :tension-count) 0))
        (complete (or (plist-get item :complete-count) 0))
        (gaps (or (plist-get item :gap-count) 0))
        (blocked (or (plist-get item :blocked-count) 0)))
    (vector (arxana-trace--truncate devmap 27)
            (format "%d" total)
            (propertize (format "%d" complete)
                        'face (if (= complete total)
                                  'arxana-trace-pass-face
                                'default))
            (propertize (format "%d" gaps)
                        'face (if (> gaps 0)
                                  'arxana-trace-gap-face
                                'default))
            (propertize (format "%d" blocked)
                        'face (if (> blocked 0)
                                  'arxana-trace-blocked-face
                                'default)))))

;; ---------------------------------------------------------------------------
;; View 3: trace-tensions (tensions within one devmap)
;; ---------------------------------------------------------------------------

(defun arxana-browser-trace-tensions-items (context)
  "Build tension items from CONTEXT :paths (no additional API call)."
  (let ((paths (plist-get context :paths)))
    (if (not paths)
        (list (list :type 'info
                    :label "No paths"
                    :description "No trace paths found in this devmap context."))
      (mapcar
       (lambda (p)
         (let* ((tension (plist-get p :tension))
                (gates (plist-get p :gates))
                (component (arxana-trace--keyword-str
                            (plist-get tension :tension/component)))
                (ttype (arxana-trace--keyword-str
                        (plist-get tension :tension/type))))
           (list :type 'trace-tension
                 :label component
                 :description (format "%s: %s" component ttype)
                 :tension tension
                 :gates gates
                 :component component
                 :tension-type ttype
                 :complete (plist-get p :complete?)
                 :gap-at (arxana-trace--keyword-str (plist-get p :gap-at))
                 :blocked-at (arxana-trace--keyword-str (plist-get p :blocked-at)))))
       paths))))

(defun arxana-browser-trace-tensions-format ()
  "Column format for trace-tensions view."
  [("Component" 18 t)
   ("Type" 10 t)
   ("Gates" 35 nil)
   ("Gap-At" 12 t)
   ("Blocked-At" 12 t)])

(defun arxana-browser-trace-tensions-row (item)
  "Row for trace-tension ITEM."
  (let ((component (or (plist-get item :component) ""))
        (ttype (or (plist-get item :tension-type) ""))
        (gates (plist-get item :gates))
        (gap-at (or (plist-get item :gap-at) ""))
        (blocked-at (or (plist-get item :blocked-at) "")))
    (vector (arxana-trace--truncate component 17)
            (arxana-trace--truncate ttype 9)
            (if gates (arxana-trace--gate-indicators gates) "")
            (propertize gap-at 'face (if (not (string-empty-p gap-at))
                                         'arxana-trace-gap-face
                                       'default))
            (propertize blocked-at 'face (if (not (string-empty-p blocked-at))
                                             'arxana-trace-blocked-face
                                           'default)))))

;; ---------------------------------------------------------------------------
;; View 4: trace-gates (6-gate chain for one tension)
;; ---------------------------------------------------------------------------

(defun arxana-browser-trace-gates-items (context)
  "Build gate items from CONTEXT :gates."
  (let ((gates (plist-get context :gates))
        (idx 0)
        result)
    (if (not gates)
        (list (list :type 'info
                    :label "No gates"
                    :description "No gate data found in this tension context."))
      (dolist (g gates (nreverse result))
        (let* ((gate-name (arxana-trace--keyword-str (plist-get g :gate)))
               (status (arxana-trace--keyword-str (plist-get g :status)))
               (summary (arxana-trace--gate-summary g)))
          (push (list :type 'trace-gate
                      :label gate-name
                      :description summary
                      :gate-name gate-name
                      :gate-status status
                      :gate-data (plist-get g :data)
                      :gate-index idx
                      :tension (plist-get context :tension))
                result)
          (setq idx (1+ idx)))))))

(defun arxana-browser-trace-gates-format ()
  "Column format for trace-gates view."
  [("#" 3 nil)
   ("Gate" 14 nil)
   ("Status" 8 nil)
   ("Summary" 0 nil)])

(defun arxana-browser-trace-gates-row (item)
  "Row for trace-gate ITEM."
  (let ((idx (or (plist-get item :gate-index) 0))
        (gate (or (plist-get item :gate-name) ""))
        (status (or (plist-get item :gate-status) ""))
        (summary (or (plist-get item :description) "")))
    (vector (format "%d" idx)
            gate
            (arxana-trace--gate-indicator status)
            (arxana-trace--truncate summary 60))))

;; ---------------------------------------------------------------------------
;; Visit actions
;; ---------------------------------------------------------------------------

(defun arxana-browser-trace-visit-devmap (item)
  "Push trace-tensions view for devmap ITEM."
  (setq arxana-browser--stack
        (cons (list :view 'trace-tensions
                    :label (plist-get item :label)
                    :paths (plist-get item :paths))
              arxana-browser--stack))
  (arxana-browser--render))

(defun arxana-browser-trace-visit-tension (item)
  "Push trace-gates view for tension ITEM."
  (setq arxana-browser--stack
        (cons (list :view 'trace-gates
                    :label (plist-get item :label)
                    :gates (plist-get item :gates)
                    :tension (plist-get item :tension))
              arxana-browser--stack))
  (arxana-browser--render))

(defun arxana-browser-trace-visit-gate (item)
  "Open detail for gate ITEM.
For reflection gates that pass, fetch and display the reflection envelope.
For source gates that pass, open the file at the line."
  (let ((gate-name (plist-get item :gate-name))
        (status (plist-get item :gate-status))
        (data (plist-get item :gate-data)))
    (cond
     ;; Reflection gate with a namespace — fetch reflection envelope
     ((and (string= gate-name "reflection")
           (string= status "pass")
           data
           (plist-get data :ns))
      (let* ((ns (arxana-trace--keyword-str (plist-get data :ns)))
             (vars (plist-get data :vars)))
        (if (and vars (> (length vars) 0))
            ;; Fetch first var's reflection
            (let* ((first-var (arxana-trace--keyword-str (car vars)))
                   (endpoint (format "/reflect/var/%s/%s" ns first-var)))
              (arxana-trace--show-reflection-envelope endpoint ns first-var))
          (message "Reflection gate passed but no vars found"))))
     ;; Source gate with file data — open file
     ((and (string= gate-name "source")
           (string= status "pass")
           data)
      (let* ((file (plist-get data :file))
             (line (plist-get data :line))
             (resolved (or (and file (file-exists-p file) file)
                           (arxana-trace--resolve-source-path file))))
        (if resolved
            (progn
              (find-file-other-window resolved)
              (when (and line (numberp line) (> line 0))
                (goto-char (point-min))
                (forward-line (1- line))))
          (message "Source file not found: %s" (or file "nil")))))
     ;; Evidence gate — open the mission file if path is present
     ((and (string= gate-name "evidence")
           data)
      (let* ((missions (plist-get data :missions))
             (first-mission (car missions))
             (path (and first-mission (plist-get first-mission :path))))
        (if (and path (file-exists-p path))
            (find-file-other-window path)
          (message "Evidence gate: %d missions, no local path"
                   (length (or missions '()))))))
     (t
      (message "Gate %s [%s]: %s"
               (or gate-name "?")
               (or status "?")
               (or (plist-get item :description) "no detail"))))))

;; ---------------------------------------------------------------------------
;; Reflection envelope display
;; ---------------------------------------------------------------------------

(defun arxana-trace--show-reflection-envelope (endpoint ns var-name)
  "Fetch ENDPOINT and display reflection envelope for NS/VAR-NAME."
  (condition-case err
      (let* ((payload (arxana-trace--fetch endpoint))
             (buf-name "*Trace: Reflection*")
             (buf (get-buffer-create buf-name)))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (org-mode)
            (insert (format "* Reflection: %s/%s\n\n" ns var-name))
            (when payload
              (let ((file (plist-get payload :file))
                    (line (plist-get payload :line))
                    (arglists (plist-get payload :arglists))
                    (doc (plist-get payload :doc))
                    (resolved-at (plist-get payload :resolved-at)))
                (insert (format "- Namespace :: %s\n" ns))
                (insert (format "- Symbol :: %s\n" var-name))
                (when file
                  (insert (format "- File :: %s\n" file)))
                (when line
                  (insert (format "- Line :: %s\n" line)))
                (when arglists
                  (insert (format "- Arglists :: %s\n" arglists)))
                (when doc
                  (insert (format "\n** Docstring\n\n%s\n" doc)))
                (when resolved-at
                  (insert (format "\n- Resolved at :: %s\n" resolved-at)))
                ;; Resolve for 'o' keybinding
                (let* ((resolved (or (and file (file-exists-p file) file)
                                     (arxana-trace--resolve-source-path file)))
                       (target-line line))
                  (when resolved
                    (insert (format "\n** Actions\n\n"))
                    (insert (format "Press =o= to open %s" resolved))
                    (when target-line
                      (insert (format " at line %s" target-line)))
                    (insert "\n")))))
            (goto-char (point-min))
            (local-set-key (kbd "q") #'quit-window)
            (when payload
              (let* ((file (plist-get payload :file))
                     (line (plist-get payload :line))
                     (resolved (or (and file (file-exists-p file) file)
                                   (arxana-trace--resolve-source-path file))))
                (when resolved
                  (local-set-key (kbd "o")
                                 (lambda ()
                                   (interactive)
                                   (find-file-other-window resolved)
                                   (when (and line (numberp line) (> line 0))
                                     (goto-char (point-min))
                                     (forward-line (1- line))))))))
            (view-mode 1)))
        (display-buffer buf))
    (error
     (message "Failed to fetch reflection: %s" (error-message-string err)))))

(provide 'arxana-browser-trace)

;;; arxana-browser-trace.el ends here
