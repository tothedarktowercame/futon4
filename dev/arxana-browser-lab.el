;;; arxana-browser-lab.el --- Lab browser views -*- lexical-binding: t; -*-

;;; Commentary:
;; Lab browsing helpers for the Arxana browser.
;; Includes active session viewing via Claude stream and archived session browsing.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-parse)
(require 'url-http)

(require 'arxana-lab)

(defvar arxana-browser--stack)
(defvar arxana-browser--context)
(defvar arxana-browser--buffer)

(declare-function arxana-browser--item-at-point "arxana-browser-core")
(declare-function arxana-browser--render "arxana-browser-core")
(declare-function fuclient-claude-stream-connect "fuclient-claude-stream")
(declare-function arxana-browser-patterns--ensure-frame "arxana-browser-patterns")
(declare-function arxana-lab-open-raw-payload "arxana-lab" (payload))
(declare-function arxana-browser-code--open-path "arxana-browser-code" (path))

(defgroup arxana-lab-sessions nil
  "Lab session browsing."
  :group 'arxana)

(defcustom arxana-lab-sessions-server
  (or (getenv "FUTON3_SERVER") "http://localhost:5050")
  "Futon3 HTTP server URL for fetching session lists."
  :type 'string
  :group 'arxana-lab-sessions)

(defcustom arxana-lab-sessions-servers nil
  "List of Futon3 HTTP server URLs to query for session lists.
When nil, uses `arxana-lab-sessions-server`."
  :type '(repeat string)
  :group 'arxana-lab-sessions)

(defcustom arxana-lab-sessions-request-timeout 10
  "Timeout in seconds for session list requests."
  :type 'integer
  :group 'arxana-lab-sessions)

(defcustom arxana-lab-futon1-server
  (or (getenv "FUTON1_API_BASE")
      (getenv "STACK_HUD_FUTON1_API_BASE")
      "http://localhost:7071/api/alpha")
  "Futon1 API base URL for archived lab sessions."
  :type 'string
  :group 'arxana-lab-sessions)

(defun arxana-lab--parse-json (body)
  "Parse JSON BODY into plist."
  (when (and body (stringp body) (not (string-empty-p body)))
    (condition-case nil
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

(defun arxana-lab--session-servers ()
  (let ((servers arxana-lab-sessions-servers))
    (cond
     ((and (listp servers) (seq servers)) servers)
     ((stringp servers) (list servers))
     ((and arxana-lab-sessions-server (stringp arxana-lab-sessions-server))
      (list arxana-lab-sessions-server))
     (t nil))))

(defun arxana-lab--server-host (server)
  (condition-case _err
      (let* ((url (url-generic-parse-url server))
             (host (url-host url))
             (port (url-port url)))
        (cond
         ((and host port) (format "%s:%s" host port))
         (host host)
         (t server)))
    (error server)))

(defun arxana-lab--merge-sessions (sessions)
  (seq-sort (lambda (a b)
              (string> (or (plist-get a :modified) "")
                       (or (plist-get b :modified) "")))
            sessions))

(defcustom arxana-lab-use-plain-websocket t
  "Use plain ws:// instead of wss:// for lab-ws connections.
Set to t as workaround for Emacs 31 TLS/nginx WebSocket issues.
Set to nil to use wss:// through nginx SSL termination."
  :type 'boolean
  :group 'arxana-lab-sessions)

(defun arxana-lab--server->ws (server)
  "Convert HTTP server URL to WebSocket URL for lab-ws.
If `arxana-lab-use-plain-websocket' is t, always use ws://5056 (direct).
Otherwise, HTTP 5050 -> ws 5056, HTTPS 5051 -> wss 5057 (nginx SSL)."
  (let* ((base (string-remove-suffix "/" (or server "")))
         ;; Extract host from URL
         (host (replace-regexp-in-string
                "^\\(https?\\|wss?\\)://" ""
                (replace-regexp-in-string ":[0-9]+.*$" "" base))))
    (if arxana-lab-use-plain-websocket
        ;; Direct connection to lab-ws on 5056 (no SSL)
        (format "ws://%s:5056" host)
      ;; SSL path through nginx
      (let* ((is-ssl (or (string-prefix-p "https://" base)
                         (string-prefix-p "wss://" base)))
             (with-port (if is-ssl
                            (replace-regexp-in-string ":5051\\b" ":5057" base)
                          (replace-regexp-in-string ":5050\\b" ":5056" base))))
        (cond
         ((string-prefix-p "wss://" with-port) with-port)
         ((string-prefix-p "ws://" with-port) with-port)
         ((string-prefix-p "https://" with-port)
          (concat "wss://" (string-remove-prefix "https://" with-port)))
         ((string-prefix-p "http://" with-port)
          (concat "ws://" (string-remove-prefix "http://" with-port)))
         (t with-port))))))

(defun arxana-lab--fetch-sessions (endpoint)
  "Fetch sessions from ENDPOINT (e.g., /fulab/lab/sessions/active)."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (servers (arxana-lab--session-servers))
         (merged '()))
    (unless servers
      (user-error "No lab session servers configured"))
    (dolist (server servers)
      (let* ((url (concat (string-remove-suffix "/" server) endpoint))
             (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
        (if (not buffer)
            (message "[arxana-lab] Failed to fetch sessions from %s" url)
          (with-current-buffer buffer
            (goto-char (point-min))
            (re-search-forward "\n\n" nil 'move)
            (let* ((body (buffer-substring-no-properties (point) (point-max)))
                   (payload (arxana-lab--parse-json body))
                   (sessions (plist-get payload :sessions))
                   (host (arxana-lab--server-host server)))
              (kill-buffer buffer)
              (when (listp sessions)
                (setq merged
                      (append merged
                              (mapcar (lambda (s)
                                        (append (if (listp s) s (list :id s))
                                                (list :server server :host host)))
                                      sessions)))))))))
    (list :ok t :sessions (arxana-lab--merge-sessions merged))))

(defun arxana-lab--fetch-futon1 (endpoint)
  "Fetch lab sessions from Futon1 ENDPOINT (e.g., /lab/sessions)."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (base (string-remove-suffix "/" arxana-lab-futon1-server))
         (url (concat base endpoint))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        (list :ok nil :entries nil)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (payload (arxana-lab--parse-json body)))
          (kill-buffer buffer)
          payload)))))

(defun arxana-lab--fetch-futon1-session (session-id)
  "Fetch a single Futon1 lab session by SESSION-ID."
  (arxana-lab--fetch-futon1 (format "/lab/session/%s" session-id)))

(defun arxana-lab--truncate (text max-len)
  "Truncate TEXT to MAX-LEN characters."
  (let ((value (or text "")))
    (if (> (length value) max-len)
        (concat (substring value 0 (max 0 (- max-len 3))) "...")
      value)))

;; =============================================================================
;; Lab menu items (Active Sessions / Archived Sessions)
;; =============================================================================

(defun arxana-browser--lab-menu-items ()
  "Return the Lab sub-menu items."
  (list (list :type 'lab-menu
              :label "Evidence Timeline"
              :description "Evidence landscape entries (PSRs, PURs, PARs)"
              :view 'evidence-timeline)
        (list :type 'lab-menu
              :label "Evidence by Session"
              :description "Evidence grouped by session ID"
              :view 'evidence-sessions)
        (list :type 'lab-menu
              :label "Open REPL Sessions"
              :description "Evidence-backed semantic summaries for open Codex/Claude buffers"
              :view 'evidence-open-sessions)
        (list :type 'lab-menu
              :label "Active Sessions"
              :description "Currently running Codex/Claude sessions"
              :view 'lab-sessions-active)
        (list :type 'lab-menu
              :label "Recent Sessions"
              :description "All sessions in ~/.claude/projects/ and ~/.codex/sessions/"
              :view 'lab-sessions-recent)
        (list :type 'lab-menu
              :label "Raw Lab Logs"
              :description "Exported sessions in lab/raw"
              :view 'lab-sessions-raw)
        (list :type 'lab-menu
              :label "Archived Sessions"
              :description "Persisted lab sessions in Futon1"
              :view 'lab-sessions-archived)
        (list :type 'lab-menu
              :label "Tensions"
              :description "Discrepancies between ideal and actual (devmap gaps)"
              :view 'tensions)
        (list :type 'lab-menu
              :label "Devmaps"
              :description "Architectural prototypes (wiring diagrams, exotypes)"
              :view 'devmaps)
        (list :type 'lab-menu
              :label "Lab Files"
              :description "Browse raw/stubs/drafts files"
              :view 'lab)))

(defun arxana-browser--lab-menu-format ()
  "Format for Lab menu view."
  [("Option" 20 t)
   ("Description" 0 nil)])

(defun arxana-browser--lab-menu-row (item)
  "Row for Lab menu ITEM."
  (vector (or (plist-get item :label) "")
          (or (plist-get item :description) "")))

;; =============================================================================
;; Invariants browser
;; =============================================================================

(defcustom arxana-invariants-structural-law-inventory-path
  "/home/joe/code/futon3c/docs/structural-law-inventory.sexp"
  "Path to the structural-law inventory registry seed."
  :type 'file
  :group 'arxana-browser)

(defun arxana-browser--invariants-menu-items ()
  "Return the Invariants sub-menu items."
  (list
   (list :type 'invariants-menu
         :label "Live Invariants"
         :description "Working invariant families from futon-stack-invariant-model.edn — the checks the live system actually runs."
         :view 'operational-families)
   (list :type 'invariants-menu
         :label "Live Violations"
         :description "Concrete active violation instances emitted when those live invariants fail."
         :view 'violations)
   (list :type 'invariants-menu
         :label "Candidate Queue"
         :description "Unwired invariant pressure from structural-law-inventory.sexp."
         :view 'candidate-invariants)
   (list :type 'invariants-menu
         :label "Invariant Guide"
         :description "How the five layers (I0-I4) work, who owns what, and how invariants mature."
         :view 'invariant-guide)
   (list :type 'info
         :label "Reading order"
         :description "Start with Live Invariants, then inspect Live Violations, then use Candidate Queue for growth pressure.")))

(defun arxana-browser--mi0-docs-code-report ()
  "Return a report on Docs->Code correspondence for live invariant families."
  (let* ((model (arxana-browser--read-invariant-model))
         (inventory (ignore-errors (arxana-browser--read-structural-law-inventory)))
         (live-families (and model
                             (seq-filter #'arxana-browser--live-invariant-family-p
                                         (arxana-browser--edn-extract-families model))))
         (entries nil))
    (dolist (family live-families)
      (let* ((family-form (and inventory
                               (arxana-browser--operational-family-definition-form
                                inventory
                                (plist-get family :id))))
             (implemented-in (plist-get (cdr family-form) :implemented-in))
             (doc (arxana-browser--invariant-family-doc family))
             (missing nil))
        (unless (and doc (plist-get doc :what))
          (push "missing family overview" missing))
        (unless (plist-get doc :referenced-entities)
          (push "missing referenced-entities section" missing))
        (dolist (path implemented-in)
          (unless (arxana-browser--invariant-code-doc family path)
            (push (format "%s lacks docs note" path) missing))
          (unless (arxana-browser--invariant-code-symbol family path)
            (push (format "%s lacks symbol target" path) missing)))
        (push (list :family family
                    :implemented-in implemented-in
                    :missing (nreverse missing))
              entries)))
    (let* ((entries (nreverse entries))
           (total (length entries))
           (complete (seq-count (lambda (entry)
                                  (null (plist-get entry :missing)))
                                entries))
           (incomplete (seq-filter (lambda (entry)
                                     (plist-get entry :missing))
                                   entries)))
      (list :total total
            :complete complete
            :incomplete incomplete
            :entries entries))))

(defun arxana-browser--mi0-live-violation-actionability-report ()
  "Return a first-pass actionability report for live violation hyperedges."
  (condition-case err
      (let* ((items (arxana-browser--violations-items))
             (violations (seq-filter (lambda (item)
                                       (eq (or (plist-get item :type)
                                               (cdr (assq :type item)))
                                           'violation-entry))
                                     items))
             (entries nil))
        (dolist (item violations)
          (let* ((props (arxana-browser--violation-props item))
                 (endpoints (arxana-browser--violation-endpoints item))
                 (entity-endpoints (if (and endpoints
                                           (stringp (car endpoints))
                                           (string-prefix-p "inv:" (car endpoints)))
                                      (cdr endpoints)
                                    endpoints))
                 (missing nil))
            (unless (arxana-browser--field props :family)
              (push "missing family anchor" missing))
            (unless (arxana-browser--field props :rule)
              (push "missing rule key" missing))
            (unless entity-endpoints
              (push "missing violating endpoints" missing))
            (unless (arxana-browser--field props :actionability)
              (push "missing actionability" missing))
            (unless (arxana-browser--field props :obligation-id)
              (push "missing obligation id" missing))
            (unless (arxana-browser--field props :detected-at)
              (push "missing detected-at" missing))
            (unless (arxana-browser--field props :emitted-by)
              (push "missing emitted-by provenance" missing))
            (unless (arxana-browser--field props :last-checked)
              (push "missing last-checked" missing))
            (push (list :item item
                        :missing (nreverse missing))
                  entries)))
        (let* ((entries (nreverse entries))
               (total (length entries))
               (complete (seq-count (lambda (entry)
                                      (null (plist-get entry :missing)))
                                    entries))
               (incomplete (seq-filter (lambda (entry)
                                         (plist-get entry :missing))
                                       entries)))
          (list :total total
                :complete complete
                :incomplete incomplete
                :entries entries)))
    (error
     (list :total 0
           :complete 0
           :incomplete nil
           :entries nil
           :error (error-message-string err)))))

(defun arxana-browser--invariant-guide-items ()
  "Return items for the invariant guide view."
  (let* ((mi0 (arxana-browser--mi0-docs-code-report))
         (total (plist-get mi0 :total))
         (complete (plist-get mi0 :complete))
         (incomplete (plist-get mi0 :incomplete))
         (violation-mi0 (arxana-browser--mi0-live-violation-actionability-report))
         (v-total (plist-get violation-mi0 :total))
         (v-complete (plist-get violation-mi0 :complete))
         (v-incomplete (plist-get violation-mi0 :incomplete))
         (v-error (plist-get violation-mi0 :error)))
    (append
     (list
      (list :type 'info :label "MI0: CORE METAINVARIANTS"
            :description "Metainvariants about the invariant surfaces themselves. These are the conditions that make the invariant browser inhabitable rather than merely descriptive.")
      (list :type 'info :label "  Docs->Code correspondence"
            :description (if (= total complete)
                             (format "%d/%d live invariant families currently have family docs, referenced-entity glosses, and symbol-level code targets for every implemented-in path." complete total)
                           (format "%d/%d live invariant families currently satisfy full Docs->Code correspondence." complete total)))
      (list :type 'info :label "  Live-violation actionability"
            :description (cond
                          (v-error
                           (format "Could not evaluate live-violation-actionability: %s" v-error))
                          ((zerop v-total)
                           "0/0 live violations currently have viable hyperedge annotations.")
                          ((= v-total v-complete)
                           (format "%d/%d live violations currently have viable hyperedge annotations." v-complete v-total))
                          (t
                           (format "%d/%d live violations currently have viable hyperedge annotations." v-complete v-total)))))
     (mapcar
      (lambda (entry)
        (let ((family (plist-get entry :family)))
          (list :type 'info
                :label (format "  Missing: %s" (or (plist-get family :name) (plist-get family :id)))
                :description (string-join (plist-get entry :missing) "; "))))
      incomplete)
     (mapcar
      (lambda (entry)
        (let* ((item (plist-get entry :item))
               (rule (arxana-browser--violation-rule item)))
          (list :type 'info
                :label (format "  Violation gap: %s" rule)
                :description (string-join (plist-get entry :missing) "; "))))
      v-incomplete)
     (list
      (list :type 'info :label ""
            :description "")
      (list :type 'info :label "THE FIVE INVARIANT LAYERS"
            :description "From substrate (I0) to self-governance (I4). Each layer depends on all layers below it.")
      (list :type 'info :label ""
            :description "")
      (list :type 'info :label "I0: Data persists durably"
            :description "Owner: futon1a. 3 operational families. The substrate — if this breaks, nothing works.")
      (list :type 'info :label "  Startup contracts"
            :description "Startup requires explicit policy; underspecified → loud failure.")
      (list :type 'info :label "  Authorization & identity"
            :description "Write authority and identity uniqueness enforced before durable write.")
      (list :type 'info :label "  Layered error hierarchy"
            :description "Failures surface at the layer that caused them, with stable context.")
      (list :type 'info :label ""
            :description "")
      (list :type 'info :label "I1: State transitions are valid"
            :description "Owner: futon3c operational core, plus a futon3b bypassable gate pipeline family. Legal states, legal transitions.")
      (list :type 'info :label "  Phase ordering"
            :description "States advance in valid order in the live futon3c mission/proof/tickle surfaces.")
      (list :type 'info :label "  Gate pipeline phase ordering (bypassable)"
            :description "futon3b's G5->G0 gate ordering exists in code and tests, but is not part of the current live invariant aggregate.")
      (list :type 'info :label "  Status discipline"
            :description "Status labels are legal and evidence-consistent.")
      (list :type 'info :label "  Existence"
            :description "Referenced entities actually exist.")
      (list :type 'info :label "  Dependency satisfaction"
            :description "Completed things backed by completed prerequisites.")
      (list :type 'info :label ""
            :description "")
      (list :type 'info :label "I2: Failures are visible"
            :description "Mixed ownership. 1 operational (graph-symmetry), 2 candidate. Observability layer.")
      (list :type 'info :label "  Graph symmetry (operational)"
            :description "If A points to B, the inverse relation exists. Checked in portfolio/agency/proof logic.")
      (list :type 'info :label "  Failure locality (candidate)"
            :description "Failures surface near source layer. Strong in futon1a/3b, candidate elsewhere.")
      (list :type 'info :label "  Human-visible inspectability (candidate)"
            :description "Operator can tell what's going on without folklore.")
      (list :type 'info :label ""
            :description "")
      (list :type 'info :label "I3: Work is structured"
            :description "1 operational (required-outputs), 3 candidate. Where outputs land.")
      (list :type 'info :label "  Required outputs (operational)"
            :description "Each phase produces its required artifacts.")
      (list :type 'info :label "  Atomic inspectable units (candidate)"
            :description "Work in bounded units. Shape is real but live work routes around it.")
      (list :type 'info :label "  Artifact custody (candidate)"
            :description "Outputs land where the stack expects them.")
      (list :type 'info :label "  Repo role clarity (candidate)"
            :description "A repo says what it is, and its root matches.")
      (list :type 'info :label ""
            :description "")
      (list :type 'info :label "I4: The system governs itself"
            :description "All 4 families candidate. The frontier — where the stack needs to grow.")
      (list :type 'info :label "  Peripheral custody (candidate)"
            :description "Sessions carry enough structure to prevent drift.")
      (list :type 'info :label "  Budgeted action selection (candidate)"
            :description "Action constrained by budget, not priority alone.")
      (list :type 'info :label "  Archaeology control (candidate)"
            :description "Latent work doesn't accumulate as invisible debt.")
      (list :type 'info :label "  Cross-store agreement (candidate)"
            :description "Mirrors across stores agree about identity and continuity.")
      (list :type 'info :label ""
            :description "")
      (list :type 'info :label "INVARIANT LIFECYCLE"
            :description "candidate → wired (violation detectable) → operational (enforced) → structural property")
      (list :type 'info :label "  candidate"
            :description "Law-shaped pressure recurs but no enforcement. Named in structural-law-inventory.sexp.")
      (list :type 'info :label "  wired"
            :description "Check exists in *_logic.clj. Violations produce obligation records.")
      (list :type 'info :label "  operational"
            :description "Check runs in production. Violations surfaced as live hyperedges.")
      (list :type 'info :label "  structural"
            :description "Never violated. Property of the system. (Aspirational for most.)")
      (list :type 'info :label ""
            :description "")
      (list :type 'info :label "SOURCE OF TRUTH"
            :description "futon3c/docs/structural-law-inventory.sexp → futon4/futon-stack-invariant-model.edn")))))

(defun arxana-browser--invariants-menu-format ()
  "Format for the Invariants menu view."
  [("Option" 20 t)
   ("Description" 0 nil)])

(defun arxana-browser--invariants-menu-row (item)
  "Row for Invariants menu ITEM."
  (vector (or (plist-get item :label) "")
          (or (plist-get item :description) "")))

(defun arxana-browser--read-structural-law-inventory ()
  "Read all top-level forms from the structural-law inventory file."
  (when (file-readable-p arxana-invariants-structural-law-inventory-path)
    (with-temp-buffer
      (insert-file-contents arxana-invariants-structural-law-inventory-path)
      (goto-char (point-min))
      (let (forms)
        (condition-case nil
            (while t
              (push (read (current-buffer)) forms))
          (end-of-file nil))
        (nreverse forms)))))

(defun arxana-browser--sexp-find-first (tree head)
  "Return the first subform in TREE whose car is HEAD."
  (cond
   ((not (consp tree)) nil)
   ((eq (car tree) head) tree)
   (t (or (arxana-browser--sexp-find-first (car tree) head)
          (arxana-browser--sexp-find-first (cdr tree) head)))))

(defun arxana-browser--sexp-find-all (tree head)
  "Return all subforms in TREE whose car is HEAD."
  (cond
   ((not (consp tree)) nil)
   ((eq (car tree) head)
    (cons tree
          (arxana-browser--sexp-find-all (cdr tree) head)))
   (t (append (arxana-browser--sexp-find-all (car tree) head)
              (arxana-browser--sexp-find-all (cdr tree) head)))))

(defun arxana-browser--candidate-family-forms (inventory)
  "Extract candidate family forms from INVENTORY."
  (let* ((watchlist (arxana-browser--sexp-find-first inventory 'candidate-family-watchlist))
         (body (cdr-safe watchlist))
         (entries (if (and (= (length body) 1)
                           (listp (car body)))
                      (car body)
                    body)))
    (seq-filter (lambda (entry)
                  (and (consp entry)
                       (eq (car entry) 'family)))
                entries)))

(defun arxana-browser--candidate-family-definition-forms (inventory)
  "Extract family forms from the top-level candidate-families section."
  (let* ((section (arxana-browser--sexp-find-first inventory 'candidate-families))
         (body (cdr-safe section))
         (entries (if (and (= (length body) 1)
                           (listp (car body)))
                      (car body)
                    body)))
    (seq-filter (lambda (entry)
                  (and (consp entry)
                       (eq (car entry) 'family)))
                entries)))

(defun arxana-browser--operational-family-definition-forms (inventory)
  "Extract family forms from the top-level operational-families section."
  (let* ((section (arxana-browser--sexp-find-first inventory 'operational-families))
         (body (cdr-safe section))
         (entries (if (and (= (length body) 1)
                           (listp (car body)))
                      (car body)
                    body)))
    (seq-filter (lambda (entry)
                  (and (consp entry)
                       (eq (car entry) 'family)))
                entries)))

(defun arxana-browser--operational-family-symbol-from-model-id (model-id)
  "Convert MODEL-ID like :F-startup-contracts to the inventory symbol."
  (let ((base (replace-regexp-in-string "^:F-" "" (or model-id ""))))
    (intern
     (if (string= base "authorization-and-identity")
         "authorization-and-identity-discipline"
       base))))

(defun arxana-browser--operational-family-definition-form (inventory model-id)
  "Find the operational family form in INVENTORY matching MODEL-ID."
  (let ((family-sym (arxana-browser--operational-family-symbol-from-model-id model-id)))
    (seq-find (lambda (entry)
                (eq (plist-get (cdr entry) :id) family-sym))
              (arxana-browser--operational-family-definition-forms inventory))))

(defun arxana-browser--resolve-invariant-code-path (path)
  "Resolve inventory shorthand PATH to an on-disk source file."
  (let* ((code-root "/home/joe/code")
         (repo-src-prefixed (string-match-p "^futon[0-9a-z-]+/src/" path))
         (repo-prefixed (string-match-p "^futon[0-9a-z-]+/" path))
         (repo (and repo-prefixed (car (split-string path "/" t))))
         (candidates (delq
                      nil
                      (list (when repo-src-prefixed
                              (expand-file-name path code-root))
                            (expand-file-name path code-root)
                            (when repo
                              (expand-file-name (concat "src/" path)
                                                (expand-file-name repo code-root)))
                            (expand-file-name (concat "src/futon3c/" path)
                                              (expand-file-name "futon3c" code-root))))))
    (seq-find #'file-readable-p candidates)))

(defun arxana-browser--format-repos-display (repos)
  "Render REPOS from the EDN model as a compact display string."
  (let* ((tokens (split-string (or repos "") "[[:space:]\n]+" t))
         (clean (mapcar (lambda (token)
                          (let ((repo (replace-regexp-in-string "^:R-" "" token)))
                            (if (string-match "^futon\\([0-9][a-z]?\\)$" repo)
                                (match-string 1 repo)
                              repo)))
                        tokens)))
    (if clean
        (string-join clean ", ")
      "-")))

(defun arxana-browser--repos-list (repos)
  "Return REPOS from the EDN model as a normalized list."
  (mapcar (lambda (token)
            (replace-regexp-in-string "^:R-" "" token))
          (split-string (or repos "") "[[:space:]\n]+" t)))

(defun arxana-browser--implemented-root (path)
  "Infer the repo root owning shorthand implementation PATH."
  (cond
   ((string-match-p "^futon[0-9a-z-]+/" path)
    (car (split-string path "/" t)))
   (t "futon3c")))

(defun arxana-browser--implemented-roots-display (paths)
  "Render implementation repo roots for shorthand PATHS."
  (let ((roots (delete-dups (mapcar #'arxana-browser--implemented-root paths))))
    (if roots
        (string-join roots ", ")
      "-")))

(defvar arxana-browser--invariant-family-docs nil
  "Curated family docs used by invariant detail pages.")

(setq arxana-browser--invariant-family-docs
      '((":F-existence"
     :what "Ensure that structural references point at real entities before downstream logic trusts or acts on them."
     :referenced-entities
     ("mission cycle blocker references -> obligation ids that must exist"
      "proof ledger references -> proof items and dependencies that must exist"
      "page / job targets -> registered agents that must exist"
      "registry-backed mirrors -> actors named consistently enough to be looked up")
     :code-docs
     (("agency/logic.clj" :note "Runs the registry-side invariant pass that catches missing or inconsistent agency-backed references before they are treated as live structure."
       :symbol "check-registry")
      ("peripheral/proof_logic.clj" :note "Reject proof ledger references that point at missing proof items."
       :symbol "query-dangling-refs")
      ("peripheral/mission_backend.clj" :note "Reject mission transitions or blocker claims that point at missing obligations."
       :symbol "tool-cycle-begin")))
    (":F-phase-ordering"
     :what "Ensure that live futon3c cycles advance through their declared phase order rather than skipping required prefixes."
     :referenced-entities
     ("mission cycle current/completed phase prefixes"
      "proof mode transitions after the falsification boundary"
      "watchdog scan -> page -> escalate progression")
     :code-docs
     (("agents/tickle_logic.clj" :note "Checks that watchdog escalation does not jump ahead of the scan/page chain."
       :symbol "query-watchdog-escalations-without-pages")
      ("peripheral/proof_logic.clj" :note "Checks proof-mode progression against the required preconditions."
       :symbol "query-mode-violations")
      ("peripheral/mission_backend.clj" :note "Checks mission-cycle phase prefixes against the declared order."
       :symbol "validate-phase-advance")))
    (":F-gate-pipeline-phase-ordering"
     :what "Ensure that futon3b's gate pipeline traverses G5->G0 monotonically and stops at the first failing gate."
     :referenced-entities
     ("gate ids inside the proof-path"
      "task submissions entering at G5"
      "proof-path persistence after a gate rejection")
     :code-docs
     (("futon3b/src/futon3/gate/pipeline.clj" :note "Composes the ordered G5->G0 traversal and stops after the first failing gate."
       :symbol "run")
      ("futon3b/src/futon3b/bootstrap.clj" :note "Builds and submits live inputs through the gate pipeline so the ordering can actually be exercised."
       :symbol "submit!")))
    (":F-required-outputs"
     :what "Ensure that each phase or transition emits the artifacts its downstream consumers require."
     :referenced-entities
     ("proof cycle phase outputs"
      "mission cycle phase-data keys"
      "shape declarations that name required fields")
     :code-docs
     (("peripheral/proof_logic.clj" :note "Finds proof cycles that advanced without recording required phase outputs."
       :symbol "query-missing-phase-outputs")
      ("peripheral/mission_backend.clj" :note "Rejects mission phase advances when required outputs are missing."
       :symbol "validate-phase-advance")
      ("peripheral/mission_shapes.clj" :note "Defines and validates the shape vocabulary that mission phase outputs must satisfy."
       :symbol "validate")))
    (":F-status-discipline"
     :what "Ensure that status/state labels stay within the declared vocabularies and remain compatible with the evidence class that produced them."
     :referenced-entities
     ("proof item statuses"
      "mission obligation and cycle result statuses"
      "agent invocation / watchdog state labels")
     :code-docs
     (("agents/tickle_logic.clj" :note "Projects live watchdog and page state into explicit fact sets rather than leaving it as informal event interpretation."
       :symbol "build-live-db")
      ("agency/logic.clj" :note "Checks agency-side invocation status facts for structurally suspicious states."
       :symbol "query-invoking-without-idle-path")
      ("peripheral/proof_logic.clj" :note "Rejects proof states that carry invalid or incompatible status values."
       :symbol "query-invalid-statuses")
      ("peripheral/mission_backend.clj" :note "Checks proposed mission status transitions before they become durable state."
       :symbol "tool-status-validate")
      ("peripheral/mission_shapes.clj" :note "Defines the allowed mission status vocabulary and transition rules."
       :symbol "valid-status-transition?")))
    (":F-dependency-satisfaction"
     :what "Ensure that a thing only claims completion when the prerequisites it depends on have also been satisfied."
     :referenced-entities
     ("mission blocker chains"
      "proof dependencies and their statuses")
     :code-docs
     (("portfolio/logic.clj" :note "Grounds mission adjacency and completion reasoning in explicit blocker satisfaction."
       :symbol "deps-complete?")
      ("peripheral/proof_logic.clj" :note "Finds proved proof items whose dependencies are not yet proved."
       :symbol "query-proved-with-unproved-deps")))
    (":F-startup-contracts"
     :what "Ensure startup fails loudly unless the declared policy surface is explicit enough to support safe operation."
     :referenced-entities
     ("allowed penholder policy"
      "compat penholder participation"
      "startup-time config required to create the live system")
     :code-docs
     (("futon1a/system.clj" :note "Rejects underspecified startup policy before the system boots into live service."
       :symbol "validate-start-policy!")))
    (":F-layered-error-hierarchy"
     :what "Ensure failures surface from the layer that caused them, with stable layer/status/context metadata preserved into the API surface."
     :referenced-entities
     ("layered write-pipeline stages"
      "error maps carrying layer/status/reason/context"
      "HTTP response projection of those layer errors")
     :code-docs
     (("futon1a/core/pipeline.clj" :note "Runs the layered write path in strict order and stops at the failing layer."
       :symbol "run-write!")
      ("futon1a/api/errors.clj" :note "Projects layer-tagged errors into stable HTTP response shapes."
       :symbol "error->response")))
    (":F-authorization-and-identity"
     :what "Ensure authority and durable external identity constraints are enforced before writes commit."
     :referenced-entities
     ("penholder and tooling authority"
      "external-id/source identity pairs"
      "entity/relation records that will be written durably")
     :code-docs
     (("futon1a/auth/penholder.clj" :note "Rejects writes whose penholder or tooling identity is not explicitly allowed."
       :symbol "authorize!")
      ("futon1a/core/identity.clj" :note "Rejects invalid UUID/external-id identity claims and conflicting durable mappings."
       :symbol "validate-identity")
      ("futon1a/core/entity.clj" :note "Rejects malformed entity/relation records before they enter the write path."
       :symbol "validate-entity")
      ("futon1a/core/pipeline.clj" :note "Enforces the ordering between authorization, entity integrity, identity checks, and durable write."
       :symbol "run-write!")))
    (":F-graph-symmetry"
     :what "Ensure paired structural relations stay mutually coherent rather than drifting into one-sided graphs."
     :referenced-entities
     ("proof unlock/dependency pairs"
      "declared peripheral hop topologies"
      "mission dependency graph projections")
     :code-docs
     (("portfolio/logic.clj" :note "This is the symbol that materializes mission blocker edges into the portfolio fact DB. It is substrate for symmetry reasoning rather than a dedicated symmetry checker."
       :symbol "build-db")
      ("agents/tickle_logic.clj" :note "Checks that escalation relations are backed by the corresponding earlier page relation rather than drifting into one-sided event graphs."
       :symbol "query-orphan-escalations")
      ("agency/logic.clj" :note "Finds peripheral topology asymmetries between declared exits and allowed entries."
       :symbol "query-entry-exit-asymmetry")
      ("peripheral/proof_logic.clj" :note "Finds unlock/depends-on mismatches in proof graphs."
       :symbol "query-asymmetric-edges")))
    (":F-archaeology-control"
     :what "Ensure latent-work artifacts (autostashes, deferred-stub probe registrations, open pipeline-tracer items, ...) do not accumulate as invisible operational debt. The family is the subsumption-witness shape: A is obsolete relative to a stronger record P; each artifact-class is a sibling invariant under namespace `obsolescence-recognition/<artifact-class>`. See futon3/library/invariant-coherence/{shape-first-identify,subsumption-witness,protocol-family-naming}.flexiarg for the methodology and naming convention. M-archaeology-control 2026-04-29."
     :referenced-entities
     ("git stashes per ~/code/futon* repo (canonical record: HEAD-reachable commit graph)"
      "deferred-stub registrations in family-check-fns (canonical record: inventory :status :operational)"
      "open :pipeline-tracer-item evidence entries (canonical record: matching :pipeline-tracer-closed entry)"
      "(deferred siblings: branch-merged, test-skip, conditional-todo, dead-shim — see M-archaeology-control scope-out)")
     :code-docs
     (("futon3c/src/futon3c/logic/archaeology.clj" :note "Three sibling check-fn factories, all returning the standard probe-result shape. `register-archaeology-control-taps!` wires them as probe-tap implementations."
       :symbol "I-obsolescence-recognition")
      ("futon3c/src/futon3c/logic/archaeology.clj" :note "obsolescence-recognition/autostash: enumerate `git stash list`; subsumption test = `git stash show -p N | git apply --reverse --check` clean against HEAD."
       :symbol "check-autostash-obsolescence")
      ("futon3c/src/futon3c/logic/archaeology.clj" :note "obsolescence-recognition/deferred-stub: walk `family-check-fns`; for each fn returning `:deferred? true`, cross-reference the structural-law inventory; flag when inventory shows `:status :operational`."
       :symbol "check-deferred-stub-obsolescence")
      ("futon3c/src/futon3c/logic/archaeology.clj" :note "obsolescence-recognition/pipeline-tracer: query open vs closed `:pipeline-tracer-item` evidence by tag; flag closed-or-past-target tracers."
       :symbol "check-pipeline-tracer-obsolescence")
      ("futon3c/scripts/check-autostash-obsolescence.sh" :note "Pre-commit hook for the autostash slice (strong-mode binding). Installed via symlink across all 14 ~/code/futon* repos as of 2026-04-29; refuses commit when subsumed stashes remain. Bypass: FUTON3C_SKIP_AUTOSTASH_CHECK=1."
       :symbol "main")
      ("futon3c/test/futon3c/logic/archaeology_test.clj" :note "12 deftests / 21 assertions covering empty-input, real-stash-subsumption, no-inventory cross-check, deferred-stub flagging, closed-track flagging, past-target flagging, fresh-future-not-flagged, and the registrar."
       :symbol "register-installs-three-taps")))))

(defun arxana-browser--invariant-family-doc (item)
  "Return the curated doc plist for invariant family ITEM, if any."
  (alist-get (plist-get item :id)
             arxana-browser--invariant-family-docs
             nil nil #'equal))

(defun arxana-browser--normalize-invariant-code-doc-spec (spec)
  "Normalize a curated invariant code doc SPEC into a plist."
  (cond
   ((stringp spec)
    (list :note spec))
   ((and (listp spec) (keywordp (car spec)))
    spec)
   (t nil)))

(defun arxana-browser--invariant-code-doc (item path)
  "Return a short explanation for PATH within invariant family ITEM."
  (let* ((doc (arxana-browser--invariant-family-doc item))
         (code-docs (plist-get doc :code-docs))
         (spec (cdr (assoc path code-docs))))
    (plist-get (arxana-browser--normalize-invariant-code-doc-spec spec) :note)))

(defun arxana-browser--invariant-code-symbol (item path)
  "Return the implementing symbol for PATH within invariant family ITEM, if any."
  (let* ((doc (arxana-browser--invariant-family-doc item))
         (code-docs (plist-get doc :code-docs))
         (spec (cdr (assoc path code-docs))))
    (plist-get (arxana-browser--normalize-invariant-code-doc-spec spec) :symbol)))

(defvar arxana-browser-invariant-family-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'arxana-browser-invariant-family-open-code-at-point)
    (define-key map (kbd "RET") #'arxana-browser-invariant-family-open-code-at-point)
    map)
  "Keymap for code-pointer blocks in invariant family buffers.")

(defvar-local arxana-browser--invariant-code-targets nil)
(defvar-local arxana-browser--invariant-last-target nil)
(defvar-local arxana-browser--invariant-doc-highlight-overlay nil)
(defvar-local arxana-browser--invariant-source-buffer nil)

(defun arxana-browser--jump-to-code-symbol (buffer symbol)
  "Move point in BUFFER to the defun for SYMBOL, if present."
  (when (and buffer symbol)
    (with-current-buffer buffer
      (goto-char (point-min))
      (when (re-search-forward
             (format "^[[:space:]]*(\\(defun\\|defmacro\\|defsubst\\|defvar\\|defvar-local\\|defcustom\\|defconst\\|define-derived-mode\\|define-minor-mode\\|defn\\|defn-\\)\\s-+%s\\_>"
                     (regexp-quote symbol))
             nil t)
        (goto-char (match-beginning 0))
        (let ((win (get-buffer-window buffer t)))
          (when win
            (with-selected-window win
              (recenter))))))))

(defun arxana-browser--current-code-def-symbol ()
  "Return the current defun symbol name in the current code buffer, if any."
  (save-excursion
    (when (ignore-errors
            (end-of-line)
            (beginning-of-defun)
            t)
      (when (looking-at
             "^[[:space:]]*(\\(defun\\|defmacro\\|defsubst\\|defvar\\|defvar-local\\|defcustom\\|defconst\\|define-derived-mode\\|define-minor-mode\\|defn\\|defn-\\)\\s-+\\([^[:space:]\n()]+\\)")
        (match-string-no-properties 2)))))

(defun arxana-browser--highlight-invariant-doc-target (target)
  "Highlight TARGET in the current invariant family buffer."
  (if (not target)
      (when (overlayp arxana-browser--invariant-doc-highlight-overlay)
        (delete-overlay arxana-browser--invariant-doc-highlight-overlay)
        (setq arxana-browser--invariant-doc-highlight-overlay nil))
    (unless (overlayp arxana-browser--invariant-doc-highlight-overlay)
      (setq arxana-browser--invariant-doc-highlight-overlay
            (make-overlay (plist-get target :start) (plist-get target :end)))
      (overlay-put arxana-browser--invariant-doc-highlight-overlay
                   'face 'highlight))
    (move-overlay arxana-browser--invariant-doc-highlight-overlay
                  (plist-get target :start)
                  (plist-get target :end))))

(defun arxana-browser--invariant-doc-target-at-point ()
  "Return the current invariant code target at point, if any."
  (or (get-text-property (point) 'arxana-invariant-target)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'arxana-invariant-target))))

(defun arxana-browser--find-invariant-doc-target (path &optional symbol)
  "Find an invariant doc target by PATH and optional SYMBOL in the current buffer."
  (let* ((full-path (and path (expand-file-name path)))
         (exact (seq-find
                 (lambda (entry)
                   (and (plist-get entry :resolved)
                        (equal (expand-file-name (plist-get entry :resolved)) full-path)
                        symbol
                        (equal (plist-get entry :symbol) symbol)))
                 arxana-browser--invariant-code-targets)))
    (or exact
        (seq-find
         (lambda (entry)
           (and (plist-get entry :resolved)
                (equal (expand-file-name (plist-get entry :resolved)) full-path)))
         arxana-browser--invariant-code-targets))))

(defun arxana-browser--sync-invariant-docs-from-code ()
  "Drive invariant-family highlighting from the current code buffer."
  (if (not (buffer-live-p arxana-browser--invariant-source-buffer))
      (remove-hook 'post-command-hook #'arxana-browser--sync-invariant-docs-from-code t)
    (let* ((path buffer-file-name)
           (symbol (arxana-browser--current-code-def-symbol))
           (source arxana-browser--invariant-source-buffer)
           (target (and path
                        (with-current-buffer source
                          (arxana-browser--find-invariant-doc-target path symbol)))))
      (when (buffer-live-p source)
        (with-current-buffer source
          (arxana-browser--highlight-invariant-doc-target target)
          (setq-local arxana-browser--invariant-last-target target))
        (let ((win (get-buffer-window source t)))
          (when (and win target)
            (set-window-point win (plist-get target :start))
            (with-selected-window win
              (recenter))))))))

(defun arxana-browser--open-invariant-code-target (target source-buffer)
  "Open code TARGET and attach reverse sync back to SOURCE-BUFFER."
  (let ((resolved (plist-get target :resolved))
        (symbol (plist-get target :symbol)))
    (unless (and resolved (file-readable-p resolved))
      (user-error "No readable file for %s" (or resolved (plist-get target :path) "?")))
    (let ((code-buf (if (fboundp 'arxana-browser-code--open-path)
                        (arxana-browser-code--open-path resolved)
                      (find-file-noselect resolved))))
      (arxana-browser--jump-to-code-symbol code-buf symbol)
      (when (buffer-live-p source-buffer)
        (with-current-buffer source-buffer
          (arxana-browser--highlight-invariant-doc-target target)
          (setq-local arxana-browser--invariant-last-target target))
        (with-current-buffer code-buf
          (setq-local arxana-browser--invariant-source-buffer source-buffer)
          (add-hook 'post-command-hook #'arxana-browser--sync-invariant-docs-from-code nil t)))
      code-buf)))

(defun arxana-browser-invariant-family-open-code-at-point ()
  "Open the invariant-family code target at point."
  (interactive)
  (let ((target (arxana-browser--invariant-doc-target-at-point)))
    (unless target
      (user-error "No invariant code target at point"))
    (arxana-browser--open-invariant-code-target target (current-buffer))))

(defun arxana-browser--sync-code-from-invariant-docs ()
  "Drive code opening/highlighting from the current invariant-family buffer."
  (let ((target (arxana-browser--invariant-doc-target-at-point)))
    (unless (equal target arxana-browser--invariant-last-target)
      (setq-local arxana-browser--invariant-last-target target)
      (arxana-browser--highlight-invariant-doc-target target)
      (when target
        (arxana-browser--open-invariant-code-target target (current-buffer))))))

(define-minor-mode arxana-browser-invariant-family-sync-mode
  "Keep invariant-family doc blocks and code buffers in sync."
  :lighter " InvSync"
  (if arxana-browser-invariant-family-sync-mode
      (add-hook 'post-command-hook #'arxana-browser--sync-code-from-invariant-docs nil t)
    (remove-hook 'post-command-hook #'arxana-browser--sync-code-from-invariant-docs t)))

(defun arxana-browser--insert-invariant-code-pointer-block (item path)
  "Insert a documented code-pointer block for ITEM and PATH.
Returns the target plist recorded for docs/code sync."
  (let* ((resolved (arxana-browser--resolve-invariant-code-path path))
         (note (arxana-browser--invariant-code-doc item path))
         (symbol (arxana-browser--invariant-code-symbol item path))
         (start (point)))
    (insert (format "- %s\n" path))
    (when note
      (insert (format "  %s\n" note)))
    (when symbol
      (insert (format "  Target :: %s\n" symbol)))
    (when resolved
      (insert (format "  File :: %s\n" resolved)))
    (let* ((end (point))
           (target (list :path path
                         :resolved resolved
                         :symbol symbol
                         :start start
                         :end end)))
      (add-text-properties
       start end
       (list 'arxana-invariant-target target
             'arxana-path resolved
             'arxana-symbol symbol
             'keymap arxana-browser-invariant-family-link-map
             'mouse-face 'highlight
             'follow-link t
             'help-echo (if symbol
                            (format "Docs -> Code: open %s in %s" symbol path)
                          (format "Docs -> Code: open %s" path))))
      (add-text-properties
       (+ start 2) (+ start 2 (length path))
       (list 'face 'link
             'font-lock-face 'link))
      target)))

(defun arxana-browser--repo-seed-devmap-forms (inventory)
  "Extract all devmap forms from the inventory file.
The current registry file has drifted into a multi-form shape, so we gather
all `devmap` forms rather than assuming a single well-nested repo-seeds block."
  (arxana-browser--sexp-find-all inventory 'devmap))

(defun arxana-browser--candidate-invariant-entry-item
    (invariant-id family-id source status summary origin)
  "Build a concrete candidate invariant browser item."
  (list :type 'candidate-invariant-entry
        :label (or invariant-id "?")
        :invariant (or invariant-id "?")
        :family (or family-id "?")
        :source (or source "?")
        :status (or status "candidate")
        :origin origin
        :description (or summary "")
        :note (or summary "")))

(defconst arxana-browser-stack-priority-queue-path
  "/home/joe/code/futon5a/data/stack-stereolithography-priority-queue.json"
  "Generated machine-readable priority queue used to sort candidate invariants.")

(defun arxana-browser--candidate-priority-map ()
  "Return a map from candidate item identity to priority metadata."
  (let ((table (make-hash-table :test 'equal)))
    (when (file-readable-p arxana-browser-stack-priority-queue-path)
      (let* ((payload (arxana-lab--parse-json
                       (with-temp-buffer
                         (insert-file-contents arxana-browser-stack-priority-queue-path)
                         (buffer-string))))
             (runs (plist-get payload :runs)))
        (cl-loop for run in runs
                 for rank from 1 do
          (let* ((score (plist-get run :priority/score))
                 (raw-source (or (plist-get run :run/source) 'unknown))
                 (source (cond
                          ((symbolp raw-source) (symbol-name raw-source))
                          ((stringp raw-source) raw-source)
                          (t "unknown")))
                 (family-id (plist-get run :family/id))
                 (invariant-id (plist-get run :invariant/id))
                 (family (cond
                          ((keywordp family-id) (substring (symbol-name family-id) 1))
                          ((stringp family-id) family-id)
                          (t "")))
                 (invariant (cond
                             ((keywordp invariant-id) (substring (symbol-name invariant-id) 1))
                             ((stringp invariant-id) invariant-id)
                             (t "")))
                 (key (format "%s|%s|%s" family invariant source)))
            (puthash key (list :priority-rank rank :priority-score score) table)))))
    table))

(defvar arxana-browser--candidate-family-layer-map
  '(("human-visible-inspectability" . ":I2")
    ("failure-locality" . ":I2")
    ("atomic-inspectable-units" . ":I3")
    ("artifact-custody" . ":I3")
    ("repo-role-clarity" . ":I3")
    ("interaction-evidence-continuity" . ":I3")
    ("peripheral-custody" . ":I4")
    ("budgeted-action-selection" . ":I4")
    ("archaeology-control" . ":I4")
    ("cross-store-agreement" . ":I4")
    ("strategic-closure-specification" . ":I4"))
  "Heuristic layer assignments for candidate invariant families.")

(defun arxana-browser--candidate-family-layer (family-id)
  "Return the invariant layer id for candidate FAMILY-ID."
  (or (cdr (assoc family-id arxana-browser--candidate-family-layer-map))
      "?"))

(defun arxana-browser--candidate-lane (item)
  "Return the queue lane label for candidate invariant ITEM."
  (let ((status (or (plist-get item :status) "candidate"))
        (origin (plist-get item :origin)))
    (cond
     ((member status '("operational-but-bypassable"
                       "candidate-with-live-exemplar"
                       "operational-when-enabled"))
      "promote-next")
     ((eq origin 'family-definition) "family-watchlist")
     (t "repo-pressure"))))

(defun arxana-browser--candidate-lane-rank (lane)
  "Return sort rank for candidate queue LANE."
  (cond
   ((equal lane "promote-next") 0)
   ((equal lane "family-watchlist") 1)
   ((equal lane "repo-pressure") 2)
   (t 9)))

(defun arxana-browser--candidate-lane-description (lane count)
  "Return a section description for candidate queue LANE with COUNT items."
  (let ((item-word (if (= count 1) "item" "items"))
        (touch-word (if (= count 1) "touches" "touch")))
    (pcase lane
    ("promote-next"
     (format "%d %s already %s live structure or partial wiring. These are the clearest promotion candidates." count item-word touch-word))
    ("family-watchlist"
     (format "%d canonical law-shaped pressures from the structural-law inventory. Read these as the intended queue substrate, not repo-specific anecdotes." count))
    ("repo-pressure"
     (format "%d repo-seeded pressures and exemplars. These are useful evidence, but they need family-level consolidation before they become inhabitable policy." count))
    (_ (format "%d candidate %s." count item-word)))))

(defun arxana-browser--candidate-lane-display (lane)
  "Return a short human-readable display label for queue LANE."
  (pcase lane
    ("promote-next" "promote")
    ("family-watchlist" "watchlist")
    ("repo-pressure" "repo")
    (_ lane)))

(defun arxana-browser--candidate-enrich-item (item family-meta priority-map)
  "Attach queue metadata to ITEM using FAMILY-META."
  (let* ((family (plist-get item :family))
         (meta (cdr (assoc family family-meta)))
         (lane (arxana-browser--candidate-lane item))
         (layer (arxana-browser--candidate-family-layer family))
         (key (format "%s|%s|%s"
                      family
                      (or (plist-get item :invariant) "")
                      (symbol-name (plist-get item :origin))))
         (priority (gethash key priority-map)))
    (append item
            (list :lane lane
                  :lane-rank (arxana-browser--candidate-lane-rank lane)
                  :layer layer
                  :family-summary (plist-get meta :summary)
                  :priority-rank (plist-get priority :priority-rank)
                  :priority-score (plist-get priority :priority-score)))))

(defun arxana-browser--candidate-section-items (items)
  "Insert queue section headers around candidate invariant ITEMS."
  (let ((sorted (sort (copy-sequence items)
                      (lambda (a b)
                        (let ((a-priority (or (plist-get a :priority-rank) 99999))
                              (b-priority (or (plist-get b :priority-rank) 99999))
                              (a-rank (plist-get a :lane-rank))
                              (b-rank (plist-get b :lane-rank))
                              (a-layer (or (plist-get a :layer) ""))
                              (b-layer (or (plist-get b :layer) ""))
                              (a-family (or (plist-get a :family) ""))
                              (b-family (or (plist-get b :family) ""))
                              (a-inv (or (plist-get a :invariant) ""))
                              (b-inv (or (plist-get b :invariant) "")))
                          (cond
                           ((/= a-priority b-priority) (< a-priority b-priority))
                           ((/= a-rank b-rank) (< a-rank b-rank))
                           ((not (equal a-layer b-layer)) (string< a-layer b-layer))
                           ((not (equal a-family b-family)) (string< a-family b-family))
                           (t (string< a-inv b-inv)))))))
        (current-lane nil)
        (current-layer nil)
        (result nil))
    (dolist (item sorted)
      (let ((lane (plist-get item :lane))
            (layer (plist-get item :layer)))
        (unless (equal lane current-lane)
          (setq current-lane lane
                current-layer nil)
          (let ((count (seq-count (lambda (entry)
                                    (equal (plist-get entry :lane) lane))
                                  sorted)))
            (push (list :type 'info
                        :label (upcase (replace-regexp-in-string "-" " " lane))
                        :description (arxana-browser--candidate-lane-description lane count))
                  result)))
        (unless (equal layer current-layer)
          (setq current-layer layer)
          (let ((layer-info (seq-find (lambda (entry)
                                        (equal (plist-get entry :id) layer))
                                      arxana-browser--invariant-layers)))
            (push (list :type 'info
                        :label (format "  %s: %s"
                                       (or layer "?")
                                       (or (and layer-info (plist-get layer-info :name)) ""))
                        :description (or (and layer-info (plist-get layer-info :question))
                                         ""))
                  result)))
        (push item result)))
    (nreverse result)))

(defun arxana-browser--candidate-invariant-items-from-family-form (family-form)
  "Flatten candidate invariants from FAMILY-FORM."
  (let* ((plist (cdr family-form))
         (family-id (plist-get plist :id))
         (family-name (and family-id (symbol-name family-id)))
         (status (let ((raw (plist-get plist :status)))
                   (if raw (symbol-name raw) "candidate")))
         (source (format "family/%s" (or family-name "?")))
         (invariants (seq-filter (lambda (entry)
                                   (and (consp entry)
                                        (eq (car entry) 'invariant)))
                                 (plist-get plist :candidate-invariants))))
    (mapcar
     (lambda (entry)
       (let* ((entry-plist (cdr entry))
              (invariant-id (let ((raw (plist-get entry-plist :id)))
                              (and raw (symbol-name raw))))
              (summary (plist-get entry-plist :summary)))
         (arxana-browser--candidate-invariant-entry-item
          invariant-id family-name source status summary 'family-definition)))
     invariants)))

(defun arxana-browser--candidate-invariant-items-from-devmap-form (devmap-form)
  "Flatten candidate invariants from repo-seed DEVMAP-FORM."
  (let* ((plist (cdr devmap-form))
         (devmap-id (plist-get plist :id))
         (source (format "repo/%s" (if devmap-id (symbol-name devmap-id) "?")))
         (invariants (seq-filter (lambda (entry)
                                   (and (consp entry)
                                        (eq (car entry) 'invariant)))
                                 (plist-get plist :candidate-invariants))))
    (mapcar
     (lambda (entry)
       (let* ((entry-plist (cdr entry))
              (invariant-id (let ((raw (plist-get entry-plist :id)))
                              (and raw (symbol-name raw))))
              (family-id (let ((raw (plist-get entry-plist :family)))
                           (and raw (symbol-name raw))))
              (status (let ((raw (plist-get entry-plist :status)))
                        (if raw (symbol-name raw) "candidate")))
              (summary (plist-get entry-plist :summary)))
         (arxana-browser--candidate-invariant-entry-item
          invariant-id family-id source status summary 'repo-seed)))
     invariants)))

(defcustom arxana-browser-tracer-default-target-date "2026-05-06"
  "Default target date (ISO YYYY-MM-DD) for pipeline-tracer items.
Days-to-target drives the tracer's `:priority-rank' so it floats among
peers as the deadline approaches."
  :type 'string
  :group 'arxana-browser)

(defvar arxana-browser--invariant-queue-tracer-items
  `(;; M-invariant-queue-extend pipeline-tracer track-ids (work items
   ;; emitted by `futon3c.logic.tracer/default-tracers`). Lane:
   ;; promote-next — these are concrete next-actions touching the
   ;; apparatus that just landed.
    ;; Pipeline-tracer items live in family-watchlist (the busy lane,
    ;; 54+ items) so they intersperse meaningfully. Target dates are
    ;; staggered by track character — smaller/concrete items earlier
    ;; (closer deadline = lower priority-rank = higher visibility),
    ;; larger/blocked items later. As work moves, the tracers shift.
    (:invariant "track-4-2-snapshot-as-evidence"
     :family "evidence-per-turn"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/tracer.clj"
     :note "Snapshot-as-evidence convention. Target 2026-05-02 (small)."
     :target-date "2026-05-02"
     :tracer? t :tracer-kind "pipeline-tracer")
    (:invariant "track-4-3-arxana-view-columns"
     :family "family-canary"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon4/dev/arxana-browser-*.el"
     :note "Arxana view: last-fire-at columns. Target 2026-05-04."
     :target-date "2026-05-04"
     :tracer? t :tracer-kind "pipeline-tracer")
    (:invariant "track-3-write-class-scoping"
     :family "single-boundary"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/docs/boundary-pattern.md"
     :note "Write-class generalization sub-mission stubs. Target 2026-05-06."
     :target-date "2026-05-06"
     :tracer? t :tracer-kind "pipeline-tracer")
    (:invariant "track-1-substrate-2-lift"
     :family "substrate-2-phase-1"
     :lane "family-watchlist"
     :layer "I0"
     :source "M-live-geometric-stack/tests"
     :note "Substrate-2 phase-1 lift (≥3 of 6 — concrete work). Target 2026-05-09."
     :target-date "2026-05-09"
     :tracer? t :tracer-kind "pipeline-tracer")
    (:invariant "track-2-war-machine-aif-lift"
     :family "war-machine-aif"
     :lane "family-watchlist"
     :layer "I0"
     :source "M-war-machine"
     :note "War-Machine AIF lift (blocked on M-war-machine). Target 2026-05-13."
     :target-date "2026-05-13"
     :tracer? t :tracer-kind "pipeline-tracer")
    (:invariant "track-5-vsatarcs"
     :family "vsatarcs"
     :lane "family-watchlist"
     :layer "I0"
     :source "M-stack-geometry-anthology"
     :note "VSATARCS narrative coherence (parked, slow tracer). Target 2026-05-20."
     :target-date "2026-05-20"
     :tracer? t :tracer-kind "pipeline-tracer")
    ;; Substrate-2 phase-1 deferred-stubs (M-live-geometric-stack source).
    ;; Lane: family-watchlist — canonical cross-stack pressure pending lift.
    (:invariant "substrate-2/L1-stable-id"
     :family "substrate-2-phase-1"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Lift via probe check-fn. (Track 1 source.)"
     :tracer? t :tracer-kind "deferred-stub")
    (:invariant "substrate-2/L1-idempotency"
     :family "substrate-2-phase-1"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Lift via probe check-fn. (Track 1 source.)"
     :tracer? t :tracer-kind "deferred-stub")
    (:invariant "substrate-2/L2-endpoint-resolution"
     :family "substrate-2-phase-1"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Lift via probe check-fn. (Track 1 source.)"
     :tracer? t :tracer-kind "deferred-stub")
    (:invariant "substrate-2/L2-vocab-target-resolution"
     :family "substrate-2-phase-1"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Lift via probe check-fn. (Track 1 source.)"
     :tracer? t :tracer-kind "deferred-stub")
    (:invariant "substrate-2/L2-counter-ratchet"
     :family "substrate-2-phase-1"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Lift via probe check-fn. (Track 1 source.)"
     :tracer? t :tracer-kind "deferred-stub")
    (:invariant "substrate-2/regression-vs-bb-v0_5"
     :family "substrate-2-phase-1"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Lift via probe check-fn. (Track 1 source.)"
     :tracer? t :tracer-kind "deferred-stub")
    ;; War-Machine AIF deferred-stubs (futon5a/holes/holistic-argument-aif2.edn)
    (:invariant "war-machine/boundary-integrity"
     :family "war-machine-aif"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Conceptual check in M-war-machine. (Track 2.)"
     :tracer? t :tracer-kind "deferred-stub")
    (:invariant "war-machine/observation-action-asymmetry"
     :family "war-machine-aif"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Conceptual check in M-war-machine. (Track 2.)"
     :tracer? t :tracer-kind "deferred-stub")
    (:invariant "war-machine/timescale-separation"
     :family "war-machine-aif"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Conceptual check in M-war-machine. (Track 2.)"
     :tracer? t :tracer-kind "deferred-stub")
    (:invariant "war-machine/preference-exogeneity"
     :family "war-machine-aif"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Conceptual check in M-war-machine. (Track 2.)"
     :tracer? t :tracer-kind "deferred-stub")
    (:invariant "war-machine/model-adequacy"
     :family "war-machine-aif"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Conceptual check in M-war-machine. (Track 2.)"
     :tracer? t :tracer-kind "deferred-stub")
    (:invariant "war-machine/compositional-closure"
     :family "war-machine-aif"
     :lane "family-watchlist"
     :layer "I0"
     :source "futon3c/logic/probe_taps.clj"
     :note "Deferred-stub. Conceptual check in M-war-machine. (Track 2.)"
     :tracer? t :tracer-kind "deferred-stub"))
  "Pipeline-tracer items interspersed into the Candidate Queue view.

Two kinds, distributed across existing lanes by character (NOT bunched
into a special tracer lane — that defeats their purpose as tracers):

  - pipeline-tracer (lane promote-next): work items from
    `futon3c.logic.tracer/default-tracers' (M-invariant-queue-extend
    Tracks 4.2, 4.3, 3, 1, 2, 5). Priority = days-to-target so they
    float among peers as the deadline approaches.

  - deferred-stub (lane family-watchlist): invariant families
    registered as inactive in `register-deferred-taps!' (substrate-2
    phase-1 + War-Machine AIF). Mid-range fixed priority so they sit
    naturally amongst the other watchlisted families.

Rendered with `arxana-browser-tracer-family-face' (hot pink) so they
stand out *visually* but their *position* is meaningful — they sit
where their character-and-priority places them, and as queue items
move past them (or they past others), pipeline flow becomes legible.")

(defun arxana-browser--tracer-priority-rank (item)
  "Compute a meaningful `:priority-rank' for tracer ITEM.

For pipeline-tracer items: days from now until `:target-date' (closer
deadline = lower number = higher priority). Past deadline yields a
negative rank, floating it above peers.

For deferred-stub items: a band per source-family so they intersperse
meaningfully — substrate-2 (concrete tests on disk) ranks 45-50 right
after the priority-mapped tier; war-machine (blocked, abstract) ranks
70-75 deeper in the tail."
  (let* ((kind (plist-get item :tracer-kind))
         (target (plist-get item :target-date))
         (inv (or (plist-get item :invariant) "")))
    (cond
     ((and (equal kind "pipeline-tracer")
           (stringp target)
           (>= (length target) 10))
      (condition-case _
          (let* ((target-time (date-to-time (concat (substring target 0 10)
                                                    "T00:00:00Z")))
                 (delta (float-time (time-subtract target-time (current-time)))))
            (truncate (/ delta (* 60 60 24))))
        (error 30)))
     ;; Deferred-stubs: per-family band, with within-band offset by id
     ;; so the six items within each band don't collapse to one rank.
     ((string-prefix-p "substrate-2/" inv)
      (+ 45 (mod (sxhash inv) 6)))
     ((string-prefix-p "war-machine/" inv)
      (+ 70 (mod (sxhash inv) 6)))
     (t 60))))

(defun arxana-browser--tracer-priority-score (item)
  "Compute a `:priority-score' for tracer ITEM.

Mirrors `:priority-rank' inversely so higher score = more urgent (the
opposite convention from rank). Used only for the displayed P column."
  (let ((rank (arxana-browser--tracer-priority-rank item)))
    (max 0 (- 100 rank))))

(defun arxana-browser--candidate-invariants-items ()
  "Return concrete candidate invariant queue items from the registry seed."
  (let* ((inventory (ignore-errors (arxana-browser--read-structural-law-inventory)))
         (priority-map (arxana-browser--candidate-priority-map))
         (family-meta (and inventory
                           (mapcar (lambda (family-form)
                                     (let* ((plist (cdr family-form))
                                            (id (plist-get plist :id)))
                                       (cons (and id (symbol-name id))
                                             (list :summary (plist-get plist :summary)
                                                   :status (plist-get plist :status)))))
                                   (arxana-browser--candidate-family-definition-forms inventory))))
         (family-items (and inventory
                            (mapcan #'arxana-browser--candidate-invariant-items-from-family-form
                                    (arxana-browser--candidate-family-definition-forms inventory))))
         (devmap-items (and inventory
                            (mapcan #'arxana-browser--candidate-invariant-items-from-devmap-form
                                    (arxana-browser--repo-seed-devmap-forms inventory))))
         (regular-items (mapcar (lambda (item)
                                  (arxana-browser--candidate-enrich-item item family-meta priority-map))
                                (append family-items devmap-items)))
         ;; Pre-enrich tracer items with sort fields so they survive
         ;; `arxana-browser--candidate-section-items'. Priority is
         ;; meaningful (days-to-target for pipeline-tracers, mid-range
         ;; for deferred-stubs), so tracers intersperse with regular
         ;; lane peers rather than bunching at top or bottom.
         (tracer-items (mapcar (lambda (item)
                                 (append (copy-sequence item)
                                         (list :lane-rank
                                               (arxana-browser--candidate-lane-rank
                                                (plist-get item :lane))
                                               :priority-rank
                                               (arxana-browser--tracer-priority-rank item)
                                               :priority-score
                                               (arxana-browser--tracer-priority-score item))))
                               arxana-browser--invariant-queue-tracer-items))
         (items (append tracer-items regular-items)))
    (if items
        (arxana-browser--candidate-section-items items)
      (list (list :type 'info
                  :label "No candidate invariant queue"
                  :description "Could not read candidate-family-watchlist from structural-law-inventory.sexp.")))))

(defun arxana-browser--candidate-invariants-format ()
  "Column format for the candidate invariant queue."
  [("#" 4 t)
   ("P" 5 t)
   ("Invariant" 28 t)
   ("Lane" 15 t)
   ("Layer" 6 t)
   ("Family" 24 t)
   ("Source" 14 t)
   ("Note" 0 nil)])

(defun arxana-browser--candidate-invariants-row (item)
  "Row for candidate invariant ITEM.
Tracer items (`:tracer? t') render with the hot-pink tracer face so
they stand out against regular candidates."
  (if (eq (plist-get item :type) 'info)
      (vector
       (or (plist-get item :label) "")
       ""
       ""
       ""
       ""
       ""
       ""
       (arxana-lab--truncate (or (plist-get item :description) "") 110))
    (let* ((tracer? (plist-get item :tracer?))
           (paint (lambda (s)
                    (if (and tracer? (stringp s) (not (string-empty-p s)))
                        (propertize s 'face 'arxana-browser-tracer-family-face)
                      s))))
      (vector
       (funcall paint
                (or (and (plist-get item :priority-rank)
                         (number-to-string (plist-get item :priority-rank)))
                    ""))
       (funcall paint
                (or (and (plist-get item :priority-score)
                         (number-to-string (plist-get item :priority-score)))
                    ""))
       (funcall paint (arxana-lab--truncate (or (plist-get item :invariant) "") 27))
       (funcall paint (arxana-lab--truncate
                       (arxana-browser--candidate-lane-display (or (plist-get item :lane) ""))
                       14))
       (funcall paint (or (plist-get item :layer) "?"))
       (funcall paint (arxana-lab--truncate (or (plist-get item :family) "") 23))
       (funcall paint (arxana-lab--truncate (or (plist-get item :source) "") 13))
       (funcall paint (arxana-lab--truncate (or (plist-get item :note) "") 90))))))

(defun arxana-browser-candidate-invariant-open-entry (item)
  "Open detail view for candidate invariant ITEM."
  (let ((buf (get-buffer-create "*Invariant Candidate*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "* Candidate Invariant: " (or (plist-get item :invariant) "?") "\n\n")
        (insert (format "- Family :: %s\n" (or (plist-get item :family) "?")))
        (insert (format "- Lane :: %s\n"
                        (arxana-browser--candidate-lane-display
                         (or (plist-get item :lane) "?"))))
        (insert (format "- Layer :: %s\n" (or (plist-get item :layer) "?")))
        (insert (format "- Priority Rank :: %s\n" (or (plist-get item :priority-rank) "?")))
        (insert (format "- Priority Score :: %s\n" (or (plist-get item :priority-score) "?")))
        (insert (format "- Source :: %s\n" (or (plist-get item :source) "?")))
        (insert (format "- Status :: %s\n" (or (plist-get item :status) "?")))
        (insert (format "- Origin :: %s\n" (or (plist-get item :origin) "?")))
        (when-let ((family-summary (plist-get item :family-summary)))
          (insert (format "- Family Summary :: %s\n" family-summary)))
        (insert "\n** Note\n\n")
        (insert (or (plist-get item :note) ""))
        (insert "\n\n** Interpretation\n\n")
        (insert "This is a candidate invariant from the structural-law inventory,\n")
        (insert "not a live violation. It names a concrete law-shaped pressure that\n")
        (insert "may later become an always-on invariant, gate, or obligation source.\n")
        (insert "\n** Queue Reading\n\n")
        (pcase (plist-get item :lane)
          ("promote-next"
           (insert "This item is close to the live path already. Treat it as a promotion or\n")
           (insert "anti-bypass candidate rather than a distant aspiration.\n"))
          ("family-watchlist"
           (insert "This item belongs to the canonical family watchlist. Read it as the shape of\n")
           (insert "policy the stack wants, even if no repo has wired it yet.\n"))
          ("repo-pressure"
           (insert "This item is repo-seeded pressure or an exemplar. It is useful because it names\n")
           (insert "real friction, but it still needs consolidation at the family level.\n")))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

;; =============================================================================
;; Operational Families view — wired invariants from the EDN model
;; =============================================================================

(defcustom arxana-invariants-model-path
  "/home/joe/code/futon4/futon-stack-invariant-model.edn"
  "Path to the futon-stack-invariant-model.edn file."
  :type 'file
  :group 'arxana-browser)

(defun arxana-browser--read-invariant-model ()
  "Read invariant families from futon-stack-invariant-model.edn.
Uses line-by-line extraction since EDN maps can't be parsed by `read'."
  (when (file-readable-p arxana-invariants-model-path)
    (with-temp-buffer
      (insert-file-contents arxana-invariants-model-path)
      (buffer-string))))

(defun arxana-browser--edn-extract-families (edn-text)
  "Extract family entries from EDN-TEXT using per-field line matching.
Finds each {:id :F-... block and extracts fields individually."
  (let ((families nil)
        (pos 0)
        (next-pos 0))
    ;; Find each family block by its :id line
    (while (string-match ":id +:F-\\([a-z-]+\\)" edn-text pos)
      (let* ((id (concat ":F-" (match-string 1 edn-text)))
             (block-start (match-beginning 0))
             (block-end (or (string-match ":id +:F-" edn-text (1+ block-start))
                            (length edn-text)))
             (block (substring edn-text block-start block-end))
             (name (when (string-match ":name +\"\\([^\"]+\\)\"" block)
                     (match-string 1 block)))
             (layer (when (string-match ":layer +\\(:I[0-9]+\\)" block)
                      (match-string 1 block)))
             (status (when (string-match ":status +:\\([a-z-]+\\)" block)
                       (concat ":" (match-string 1 block))))
             (question (when (string-match ":question +\"\\([^\"]+\\)\"" block)
                         (match-string 1 block)))
             (repos (when (string-match ":repos +#?{?\\[?\\([^]}]+\\)" block)
                      (match-string 1 block)))
             (inv-count (when (string-match ":invariant-count +\\([0-9]+\\)" block)
                          (string-to-number (match-string 1 block))))
             (cand-invs (when (string-match ":candidate-invariants +\\[\\([^]]+\\)\\]" block)
                          (match-string 1 block)))
             (cand-count (if cand-invs
                             (length (split-string cand-invs ":" t " +"))
                           0))
             (note (when (string-match ":note +\"\\([^\"]+\\)\"" block)
                     (match-string 1 block))))
        (push (list :type 'operational-family-entry
                    :id id
                    :name (or name id)
                    :layer (or layer "?")
                    :status (or status ":unknown")
                    :question (or question "")
                    :repos (or repos "")
                    :invariant-count (or inv-count cand-count 0)
                    :candidate-invariants (or cand-invs "")
                    :note (or note "")
                    :label (or name id))
              families)
        (setq next-pos block-end))
      (setq pos (max (1+ pos) next-pos)))
    (nreverse families)))

(defvar arxana-browser--invariant-layers
  '((:id ":I0" :name "Data persists durably"
     :question "Can I trust that what I wrote is still there, uncorrupted, and attributable?"
     :owner "futon1a (substrate)" :note "If this doesn't hold, nothing above it matters.")
    (:id ":I1" :name "State transitions are valid"
     :question "Is the system in a legal state, and did it get there legally?"
     :owner "futon3b, futon3c (coordination)")
    (:id ":I2" :name "Failures are visible"
     :question "When something goes wrong, can I tell what and where?"
     :owner "futon1a (operational) + futon3c, futon4, futon6 (candidate)")
    (:id ":I3" :name "Work is structured"
     :question "Do outputs land where they belong, and can I find them?"
     :owner "futon3c (required-outputs operational), rest candidate across stack")
    (:id ":I4" :name "The system governs itself"
     :question "Can the system sustain without folklore, heroics, or manual archaeology?"
     :owner "all candidate — the frontier"))
  "The five invariant layers, from substrate to self-governance.")

(defun arxana-browser--live-invariant-family-p (family)
  "Return non-nil when FAMILY belongs in the Live Invariants view."
  (member (plist-get family :status)
          '(":operational"
            ":operational-but-bypassable"
            ":operational-when-enabled")))

(defun arxana-browser--operational-families-items ()
  "Return items for the operational families view.
Reads from futon-stack-invariant-model.edn and shows all families
grouped by layer with header rows."
  (let* ((model (arxana-browser--read-invariant-model))
         (families (and model
                        (seq-filter #'arxana-browser--live-invariant-family-p
                                    (arxana-browser--edn-extract-families model)))))
    (if families
        ;; Group by layer, insert layer headers
        (let ((sorted (sort (copy-sequence families)
                            (lambda (a b)
                              (string< (or (plist-get a :layer) "")
                                       (or (plist-get b :layer) "")))))
              (current-layer nil)
              (result nil))
          (dolist (fam sorted)
            (let ((layer (plist-get fam :layer)))
              (unless (equal layer current-layer)
                (setq current-layer layer)
                ;; Insert layer header
                (let ((layer-info (seq-find (lambda (l) (equal (plist-get l :id) layer))
                                            arxana-browser--invariant-layers)))
                  (push (list :type 'info
                              :label (format "%s: %s"
                                             (or layer "?")
                                             (or (and layer-info (plist-get layer-info :name)) ""))
                              :description (or (and layer-info (plist-get layer-info :owner)) ""))
                        result)))
              (push fam result)))
          (nreverse result))
      (list (list :type 'info
                  :label "No invariant model"
                  :description "Could not read futon-stack-invariant-model.edn.")))))

(defun arxana-browser--operational-families-format ()
  "Column format for the operational families view."
  [("Family" 28 t)
   ("Layer" 6 t)
   ("Status" 12 t)
   ("Repos" 10 t)
   ("Checks" 6 t)
   ("Question" 0 nil)])

(defun arxana-browser--operational-families-row (item)
  "Row for operational family ITEM."
  (let ((status (or (plist-get item :status) "?")))
    (vector
     (arxana-lab--truncate (or (plist-get item :name) "") 27)
     (or (plist-get item :layer) "?")
     (propertize status
                 'face (cond
                        ((string= status ":operational")
                         'arxana-violation-auto-fixable-face)
                        ((string= status ":operational-but-bypassable")
                         'arxana-violation-needs-review-face)
                        ((string= status ":candidate")
                         'arxana-violation-needs-review-face)
                        (t 'default)))
     (arxana-lab--truncate
      (arxana-browser--format-repos-display (plist-get item :repos))
      9)
     (let ((n (plist-get item :invariant-count)))
       (if (and n (> n 0)) (number-to-string n) "-"))
     (arxana-lab--truncate (or (plist-get item :question) "") 80))))

(defun arxana-browser-operational-family-location (item)
  "Return an Arxana URI for operational family ITEM."
  (let* ((raw-id (or (plist-get item :id) ""))
         (token (replace-regexp-in-string "^:F-" "" raw-id)))
    (when (not (string-empty-p token))
      (format "arxana://invariants/family/%s"
              (url-hexify-string token)))))

(defun arxana-browser-operational-family-open-entry (item)
  "Open detail view for operational family ITEM."
  (let* ((buf (get-buffer-create "*Invariant Family*"))
         (inventory (ignore-errors (arxana-browser--read-structural-law-inventory)))
         (family-form (and inventory
                           (arxana-browser--operational-family-definition-form
                            inventory
                            (plist-get item :id))))
         (implemented-in (plist-get (cdr family-form) :implemented-in))
         (location (arxana-browser-operational-family-location item))
         (code-targets nil))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "* Invariant Family: " (or (plist-get item :name) "?") "\n\n")
        (insert (format "- ID :: %s\n" (or (plist-get item :id) "?")))
        (when location
          (insert (format "- Location :: %s\n" location)))
        (insert (format "- Layer :: %s\n" (or (plist-get item :layer) "?")))
        (insert (format "- Status :: %s\n" (or (plist-get item :status) "?")))
        (insert (format "- Question :: %s\n" (or (plist-get item :question) "?")))
        (insert (format "- Repos :: %s\n"
                        (arxana-browser--format-repos-display
                         (plist-get item :repos))))
        (let ((doc (arxana-browser--invariant-family-doc item)))
          (when doc
            (insert "\n** What This Family Tries To Do\n\n")
            (insert (plist-get doc :what) "\n")
            (let ((refs (plist-get doc :referenced-entities)))
              (when refs
                (insert "\n** Referenced Entities\n\n")
                (dolist (ref refs)
                  (insert (format "- %s\n" ref)))))))
        (when implemented-in
          (insert (format "- Implementation roots :: %s\n"
                          (arxana-browser--implemented-roots-display implemented-in)))
          (let* ((rendered-repos (sort (delete-dups (arxana-browser--repos-list
                                                     (plist-get item :repos)))
                                       #'string<))
                 (implementation-roots (sort (delete-dups (mapcar #'arxana-browser--implemented-root
                                                                   implemented-in))
                                             #'string<)))
            (unless (equal rendered-repos implementation-roots)
              (insert (format "- Inhabitation note :: rendered repo participation (%s) does not match current code-pointer roots (%s)\n"
                              (string-join rendered-repos ", ")
                              (string-join implementation-roots ", "))))))        
        (let ((n (plist-get item :invariant-count)))
          (when (and n (> n 0))
            (insert (format "- Invariant count :: %d\n" n))))
        (when implemented-in
          (insert "\n** Code Pointers\n\n")
          (dolist (path implemented-in)
            (push (arxana-browser--insert-invariant-code-pointer-block item path)
                  code-targets)
            (insert "\n")))
        (let ((cands (plist-get item :candidate-invariants)))
          (when (and cands (not (string-empty-p cands)))
            (insert "\n** Candidate Invariants\n\n")
            (dolist (inv (split-string cands ":" t " +"))
              (insert (format "- %s\n" inv)))))
        (let ((note (plist-get item :note)))
          (when (and note (not (string-empty-p note)))
            (insert "\n** Note\n\n" note "\n")))
        (let ((status (or (plist-get item :status) "")))
          (insert "\n** Interpretation\n\n")
          (cond
           ((string= status ":operational")
            (insert "This family has wired invariant checks that run in production.\n")
            (insert "Violations are detected and surfaced as live violation hyperedges.\n"))
           ((string= status ":operational-but-bypassable")
            (insert "This family has real code and tests, but normal live stack behavior can still route around it.\n")
            (insert "Treat it as implemented but not fully inhabited on the current live path.\n"))
           ((string= status ":candidate")
            (insert "This family has identified structural pressures but no operational checks.\n")
            (insert "The invariants are law-shaped tendencies, not yet enforced.\n")
            (insert "Wiring these checks would promote them from candidate to operational.\n"))))
        (setq-local arxana-browser--invariant-code-targets (nreverse code-targets))
        (setq-local arxana-browser--invariant-last-target nil)
        (setq-local arxana-browser--invariant-source-buffer nil)
        (goto-char (point-min))
        (arxana-browser-invariant-family-sync-mode 1)))
    (pop-to-buffer buf)))

;; =============================================================================
;; Active Sessions view
;; =============================================================================

(defun arxana-browser--lab-sessions-active-format ()
  "Column format for active sessions view."
  [("Session ID" 38 t)
   ("Project" 25 t)
   ("Host" 22 t)
   ("Size" 8 nil)
   ("Active" 6 nil)
   ("Modified" 20 t)])

(defun arxana-browser--lab-sessions-active-row (item)
  "Row for active session ITEM."
  (let ((id (or (plist-get item :id) ""))
        (project (or (plist-get item :project) ""))
        (host (or (plist-get item :host) ""))
        (size (format "%dkb" (or (plist-get item :size-kb) 0)))
        (active (if (plist-get item :active) "●" ""))
        (modified (or (plist-get item :modified) "")))
    (vector (arxana-lab--truncate id 36)
            (arxana-lab--truncate project 24)
            (arxana-lab--truncate host 21)
            size
            active
            (arxana-lab--truncate modified 19))))

(defun arxana-browser--lab-sessions-active-items ()
  "Fetch and return active session items (only truly active ones)."
  (condition-case err
      (let* ((response (arxana-lab--fetch-sessions "/fulab/lab/sessions/active"))
             (sessions (plist-get response :sessions))
             ;; Filter to only active sessions
             (active-sessions (seq-filter (lambda (s) (plist-get s :active)) sessions)))
        (if active-sessions
            (mapcar (lambda (s)
                      (append (list :type 'lab-session-active) s))
                    active-sessions)
          (list (list :type 'info
                      :label "No active sessions"
                      :description "Start a Codex or Claude session to see it here"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch sessions"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Recent Sessions view (all sessions, not just active)
;; =============================================================================

(defun arxana-browser--lab-sessions-recent-items ()
  "Fetch and return all recent session items."
  (condition-case err
      (let* ((response (arxana-lab--fetch-sessions "/fulab/lab/sessions/active"))
             (sessions (plist-get response :sessions)))
        (if sessions
            (mapcar (lambda (s)
                      (append (list :type 'lab-session-active) s))
                    sessions)
          (list (list :type 'info
                      :label "No sessions found"
                      :description "No Codex/Claude sessions detected on configured hosts"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch sessions"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Raw Lab Logs view (lab/raw)
;; =============================================================================

(defun arxana-browser--lab-sessions-raw-format ()
  "Column format for raw lab logs view."
  [("Session ID" 40 t)
   ("Host" 22 t)
   ("Size" 8 nil)
   ("Modified" 20 t)])

(defun arxana-browser--lab-sessions-raw-row (item)
  "Row for raw lab log ITEM."
  (let ((id (or (plist-get item :id) ""))
        (host (or (plist-get item :host) ""))
        (size (format "%dkb" (or (plist-get item :size-kb) 0)))
        (modified (or (plist-get item :modified) "")))
    (vector (arxana-lab--truncate id 38)
            (arxana-lab--truncate host 21)
            size
            (arxana-lab--truncate modified 19))))

(defun arxana-browser--lab-sessions-raw-items ()
  "Fetch and return raw lab log items."
  (condition-case err
      (let* ((response (arxana-lab--fetch-sessions "/fulab/lab/sessions/archived"))
             (sessions (plist-get response :sessions)))
        (if sessions
            (mapcar (lambda (s)
                      (append (list :type 'lab-session-raw) s))
                    sessions)
          (list (list :type 'info
                      :label "No raw lab logs"
                      :description "Export sessions to lab/raw to see them here"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch raw logs"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Archived Sessions view (Futon1)
;; =============================================================================

(defun arxana-browser--lab-sessions-archived-format ()
  "Column format for archived sessions view."
  [("Session ID" 40 t)
   ("Start" 20 t)
   ("End" 20 t)])

(defun arxana-browser--lab-sessions-archived-row (item)
  "Row for archived session ITEM."
  (let ((id (or (plist-get item :id) ""))
        (start (or (plist-get item :timestamp-start) ""))
        (end (or (plist-get item :timestamp-end) "")))
    (vector (arxana-lab--truncate id 38)
            (arxana-lab--truncate start 19)
            (arxana-lab--truncate end 19))))

(defun arxana-browser--lab-sessions-archived-items ()
  "Fetch and return archived session items from Futon1."
  (condition-case err
      (let* ((response (arxana-lab--fetch-futon1 "/lab/sessions"))
             (entries (plist-get response :entries)))
        (if entries
            (mapcar (lambda (s)
                      (append (list :type 'lab-session-archived) s))
                    entries)
          (list (list :type 'info
                      :label "No archived sessions"
                      :description "No Futon1 lab sessions found"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch archived sessions"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Session actions
;; =============================================================================

(defun arxana-browser-lab-view-session (item)
  "View (stream) the session ITEM."
  (let ((path (plist-get item :path)))
    (unless path
      (user-error "No path for session"))
    (let ((server (arxana-lab--server->ws
                   (or (plist-get item :server) arxana-lab-sessions-server))))
      ;; Set server globally so reconnects work
      (setq fuclient-claude-stream-server server)
      (if (featurep 'fuclient-claude-stream)
          (fuclient-claude-stream-connect path)
        (if (require 'fuclient-claude-stream nil t)
            (fuclient-claude-stream-connect path)
          (user-error "fuclient-claude-stream not available"))))))

(defun arxana-browser-lab-open-session (item)
  "Open a lab session ITEM - dispatch based on type."
  (let ((type (plist-get item :type)))
    (pcase type
      ('lab-session-active
       (arxana-browser-lab-view-session item))
      ('lab-session-raw
       (let ((path (plist-get item :path)))
         (if path
             (arxana-lab-open-raw-viewer path)
           (user-error "No path for session"))))
      ('lab-session-archived
       (let* ((session-id (plist-get item :id))
              (payload (arxana-lab--fetch-futon1-session session-id))
              (doc (plist-get payload :doc)))
         (if doc
             (arxana-lab-open-raw-payload doc)
           (user-error "No Futon1 data for session"))))
      ('lab-menu
       ;; Navigate to sub-view
       (let ((view (plist-get item :view))
             (label (plist-get item :label)))
         (setq arxana-browser--stack
               (cons (list :view view :label label)
                     arxana-browser--stack))
         (arxana-browser--render)))
      ('tension-entry
       (arxana-browser-tension-open-entry item))
      (_
       (user-error "Unknown session type: %s" type)))))

;; =============================================================================
;; Original Lab entries (for Lab Files sub-view)
;; =============================================================================

(defun arxana-browser--lab-format ()
  [("Session" 36 t)
   ("Start" 20 t)
   ("Files" 7 t)
   ("Focus" 0 nil)])

(defun arxana-browser--lab-file-format ()
  [("File" 32 t)
   ("When" 17 t)
   ("Kind" 8 t)
   ("Path" 0 nil)])

(defun arxana-browser--lab-row (item)
  (let* ((session (or (plist-get item :session-id) ""))
         (start (or (plist-get item :timestamp-start) ""))
         (files (or (plist-get item :files) '()))
         (focus (or (car files) "")))
    (vector session
            start
            (format "%d" (length files))
            focus)))

(defun arxana-browser--lab-file-row (item)
  (let* ((label (or (plist-get item :label) ""))
         (modified (or (plist-get item :modified) ""))
         (kind (or (plist-get item :kind) ""))
         (path (or (plist-get item :path) "")))
    (vector label modified kind path)))

(defun arxana-browser--lab-items ()
  (let ((entries (or (arxana-lab-entries) '())))
    (if (and entries (listp entries))
        entries
      (list (list :type 'info
                  :label "No lab entries detected"
                  :description "Run dev/lab-export.clj to populate lab/raw.")))))

(defun arxana-browser-lab-browse-files (kind)
  "Open the lab files browser for KIND (raw, stubs, drafts)."
  (interactive
   (list (intern (completing-read "Lab files: " '("raw" "stubs" "drafts") nil t))))
  (let* ((kind (if (symbolp kind) kind (intern kind)))
         (label (pcase kind
                  ('raw "Raw")
                  ('stubs "Stubs")
                  ('drafts "Drafts")
                  (_ (capitalize (format "%s" kind))))))
    (with-current-buffer (get-buffer-create arxana-browser--buffer)
      (setq arxana-browser--stack (list (list :view 'lab-files
                                                       :kind kind
                                                       :label label)))
      (setq arxana-browser--context nil))
    (arxana-browser--render)))

(defun arxana-browser-lab-browse-files-other-frame (kind)
  "Open the lab files browser for KIND in the Arxana frame."
  (interactive
   (list (intern (completing-read "Lab files: " '("raw" "stubs" "drafts") nil t))))
  (let ((frame (arxana-browser-patterns--ensure-frame)))
    (with-selected-frame frame
      (let ((display-buffer-overriding-action
             '((display-buffer-same-window))))
        (arxana-browser-lab-browse-files kind))
      (select-frame-set-input-focus frame))))

(defalias 'arxana-browser-lab-browse-files-other-window
  #'arxana-browser-lab-browse-files-other-frame)

(defun arxana-browser--lab-entry-at-point ()
  (let ((item (tabulated-list-get-id)))
    (when (and item (eq (plist-get item :type) 'lab-entry))
      item)))

(defun arxana-browser--lab-open-trace ()
  (interactive)
  (let ((entry (arxana-browser--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-trace-object entry)))

(defun arxana-browser--lab-open-raw ()
  (interactive)
  (let ((entry (arxana-browser--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-raw-object entry)))

(defun arxana-browser--lab-open-draft ()
  (interactive)
  (let ((entry (arxana-browser--lab-entry-at-point)))
    (unless entry
      (user-error "No lab entry at point"))
    (arxana-lab-open-draft-object entry)))

;; =============================================================================
;; Evidence landscape views
;; =============================================================================

(defun arxana-browser--evidence-fetch (params)
  "Fetch evidence entries from Futon1a. PARAMS is an alist of query params."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (base (string-remove-suffix "/" arxana-lab-futon1-server))
         (query-string (mapconcat (lambda (pair)
                                    (format "%s=%s"
                                            (url-hexify-string (car pair))
                                            (url-hexify-string (cdr pair))))
                                  (seq-filter #'cdr params) "&"))
         (url (if (string-empty-p query-string)
                  (concat base "/evidence")
                (concat base "/evidence?" query-string)))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        (list :entries nil)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (payload (arxana-lab--parse-json body)))
          (kill-buffer buffer)
          (or payload (list :entries nil)))))))

(defvar-local arxana-browser--evidence-filter nil
  "Current evidence filter parameters (alist of query params).")

;; -- Evidence Timeline view --

(defun arxana-browser--evidence-timeline-format ()
  "Column format for evidence timeline view."
  [("Time" 18 t)
   ("Type" 7 t)
   ("Author" 12 t)
   ("↳" 2 nil)
   ("Preview" 30 nil)
   ("Subject" 0 nil)])

(defun arxana-browser--evidence-timeline-row (item)
  "Row for evidence timeline ITEM."
  (let* ((at (or (plist-get item :evidence/at) ""))
         (etype (plist-get item :evidence/type))
         (author (or (plist-get item :evidence/author) ""))
         (body (plist-get item :evidence/body))
         (reply-to (plist-get item :evidence/in-reply-to))
         (subject (plist-get item :evidence/subject))
         (type-label (arxana-lab--evidence-type-label etype))
         (type-face (arxana-lab--evidence-type-face etype))
         (type-display (if type-face
                           (propertize type-label 'face type-face)
                         type-label))
         (preview (arxana-lab--evidence-body-preview body etype)))
    (vector (arxana-lab--truncate at 17)
            type-display
            (arxana-lab--truncate author 11)
            (if reply-to "↳" "")
            (arxana-lab--truncate preview 29)
            (if (and (listp subject) (plist-get subject :ref/type))
                (format "%s:%s"
                        (or (plist-get subject :ref/type) "?")
                        (or (plist-get subject :ref/id) "?"))
              ""))))

(defun arxana-browser--evidence-timeline-items ()
  "Fetch and return evidence timeline items.
Applies `arxana-browser--evidence-filter' when set."
  (condition-case err
      (let* ((params (append (list (cons "limit" "100"))
                             (or arxana-browser--evidence-filter '())))
             (response (arxana-browser--evidence-fetch params))
             (entries (plist-get response :entries)))
        (if entries
            (mapcar (lambda (e)
                      (append (list :type 'evidence-entry) e))
                    entries)
          (list (list :type 'info
                      :label "No evidence entries"
                      :description "Append evidence via futon3c to see entries here"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch evidence"
                 :description (format "Error: %s" (error-message-string err)))))))

;; -- Evidence Sessions view --

(defun arxana-browser--evidence-sessions-format ()
  "Column format for evidence sessions view."
  [("Session" 38 t)
   ("Entries" 8 t)
   ("Types" 30 nil)
   ("Latest" 20 t)])

(defun arxana-browser--evidence-sessions-row (item)
  "Row for evidence session ITEM."
  (let ((session-id (or (plist-get item :session-id) "(no session)"))
        (count (or (plist-get item :entry-count) 0))
        (types (or (plist-get item :type-summary) ""))
        (latest (or (plist-get item :latest-at) "")))
    (vector (arxana-lab--truncate session-id 37)
            (format "%d" count)
            (arxana-lab--truncate types 29)
            (arxana-lab--truncate latest 19))))

(defun arxana-browser--evidence-sessions-items ()
  "Fetch evidence and group by session-id."
  (condition-case err
      (let* ((response (arxana-browser--evidence-fetch
                        (list (cons "limit" "500"))))
             (entries (or (plist-get response :entries) '()))
             (groups (make-hash-table :test 'equal)))
        ;; Group entries by session-id
        (dolist (e entries)
          (let* ((sid (or (plist-get e :evidence/session-id) "(no session)"))
                 (existing (gethash sid groups)))
            (puthash sid (cons e existing) groups)))
        ;; Build session summary items
        (let ((items '()))
          (maphash
           (lambda (sid group-entries)
             (let* ((count (length group-entries))
                    (types (seq-uniq (mapcar (lambda (e)
                                              (format "%s" (or (plist-get e :evidence/type) "?")))
                                            group-entries)))
                    (latest (car (seq-sort (lambda (a b)
                                            (string> (or (plist-get a :evidence/at) "")
                                                     (or (plist-get b :evidence/at) "")))
                                          group-entries))))
               (push (list :type 'evidence-session
                           :session-id sid
                           :entry-count count
                           :type-summary (mapconcat #'identity types ", ")
                           :latest-at (or (plist-get latest :evidence/at) ""))
                     items)))
           groups)
          (if items
              (seq-sort (lambda (a b)
                          (string> (or (plist-get a :latest-at) "")
                                   (or (plist-get b :latest-at) "")))
                        items)
            (list (list :type 'info
                        :label "No evidence sessions"
                        :description "No session-tagged evidence found")))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch evidence"
                 :description (format "Error: %s" (error-message-string err)))))))

;; -- Evidence visit actions --

(defun arxana-browser-evidence-open-entry (item)
  "Open a single evidence ITEM in the detail viewer."
  (arxana-lab-open-evidence-entry item))

(defun arxana-browser-evidence-open-session (item)
  "Open evidence timeline filtered by session from ITEM."
  (let* ((session-id (plist-get item :session-id))
         (response (arxana-browser--evidence-fetch
                    (list (cons "session-id" session-id)
                          (cons "limit" "200"))))
         (entries (plist-get response :entries)))
    (if entries
        (arxana-lab-open-evidence-timeline entries session-id)
      (user-error "No evidence entries for session %s" session-id))))

;; =============================================================================
;; Evidence link navigation
;; =============================================================================

(defun arxana-browser--evidence-fetch-single (evidence-id)
  "Fetch a single evidence entry by EVIDENCE-ID from Futon1a."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (base (string-remove-suffix "/" arxana-lab-futon1-server))
         (url (concat base "/evidence/" (url-hexify-string evidence-id)))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        nil
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (payload (arxana-lab--parse-json body)))
          (kill-buffer buffer)
          (plist-get payload :entry))))))

(defun arxana-browser-lab--browse-evidence-entry (evidence-id)
  "Fetch evidence entry by EVIDENCE-ID and display in detail viewer."
  (let ((entry (arxana-browser--evidence-fetch-single evidence-id)))
    (if entry
        (arxana-lab-open-evidence-entry entry)
      (user-error "Could not fetch evidence entry: %s" evidence-id))))

(defun arxana-browser-lab--browse-evidence-chain (evidence-id)
  "Fetch and display the reply chain for EVIDENCE-ID."
  (interactive
   (list (or (get-text-property (point) 'arxana-evidence-id)
             (read-string "Evidence ID: "))))
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (base (string-remove-suffix "/" arxana-lab-futon1-server))
         (url (concat base "/evidence/" (url-hexify-string evidence-id) "/chain"))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        (user-error "Could not fetch chain for %s" evidence-id)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (payload (arxana-lab--parse-json body))
               (entries (plist-get payload :entries)))
          (kill-buffer buffer)
          (if entries
              (arxana-lab-open-evidence-timeline entries (format "chain:%s" evidence-id))
            (user-error "No chain entries for %s" evidence-id)))))))

;; Wire evidence link navigation at load time
(setq arxana-lab-evidence-browser-function #'arxana-browser-lab--browse-evidence-entry)

;; =============================================================================
;; Evidence filtering
;; =============================================================================

(defun arxana-browser-evidence-filter-by-type ()
  "Filter evidence timeline by type."
  (interactive)
  (let* ((types '("(all)" "pattern-selection" "pattern-outcome" "reflection"
                  "gate-traversal" "coordination" "forum-post" "mode-transition"
                  "presence-event" "correction" "conjecture"))
         (choice (completing-read "Filter by type: " types nil t)))
    (setq arxana-browser--evidence-filter
          (if (string= choice "(all)")
              nil
            (list (cons "type" choice))))
    (arxana-browser--render)))

(defun arxana-browser-evidence-filter-by-author ()
  "Filter evidence timeline by author."
  (interactive)
  (let ((author (read-string "Filter by author: ")))
    (setq arxana-browser--evidence-filter
          (if (string-empty-p author)
              nil
            (list (cons "author" author))))
    (arxana-browser--render)))

(defun arxana-browser-evidence-clear-filter ()
  "Clear evidence timeline filter."
  (interactive)
  (setq arxana-browser--evidence-filter nil)
  (arxana-browser--render))

;; =============================================================================
;; Futon3c server configuration
;; =============================================================================

(defcustom arxana-lab-futon3c-server
  (or (getenv "FUTON3C_API_BASE")
      "http://localhost:7070/api/alpha")
  "Futon3c API base URL for mission control, tensions, and portfolio data."
  :type 'string
  :group 'arxana-lab-sessions)

(defun arxana-browser--futon3c-fetch (endpoint)
  "Fetch ENDPOINT from the futon3c server. Returns parsed JSON plist."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (base (string-remove-suffix "/" arxana-lab-futon3c-server))
         (url (concat base endpoint))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        nil
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (payload (arxana-lab--parse-json body)))
          (kill-buffer buffer)
          payload)))))

;; =============================================================================
;; Tension browser view
;; =============================================================================

(defface arxana-lab-tension-uncovered-face
  '((t :foreground "#e06c75"))
  "Face for uncovered-component tensions."
  :group 'arxana-lab)

(defface arxana-lab-tension-blocked-face
  '((t :foreground "#e5c07b"))
  "Face for blocked-mission tensions."
  :group 'arxana-lab)

(defface arxana-lab-tension-structural-face
  '((t :foreground "#c678dd"))
  "Face for structural-invalid tensions."
  :group 'arxana-lab)

(defun arxana-browser--tension-type-face (tension-type)
  "Return face for TENSION-TYPE keyword or string."
  (let ((tt (if (keywordp tension-type)
                (substring (symbol-name tension-type) 1)
              (or tension-type ""))))
    (cond
     ((string= tt "uncovered-component") 'arxana-lab-tension-uncovered-face)
     ((string= tt "blocked-mission") 'arxana-lab-tension-blocked-face)
     ((string= tt "structural-invalid") 'arxana-lab-tension-structural-face)
     (t 'default))))

(defun arxana-browser--tension-type-label (tension-type)
  "Short label for TENSION-TYPE."
  (let ((tt (if (keywordp tension-type)
                (substring (symbol-name tension-type) 1)
              (or tension-type ""))))
    (cond
     ((string= tt "uncovered-component") "UNCOVER")
     ((string= tt "blocked-mission") "BLOCKED")
     ((string= tt "structural-invalid") "STRUCT")
     (t (upcase (arxana-lab--truncate tt 7))))))

(defun arxana-browser--tensions-format ()
  "Column format for tensions view."
  [("Type" 8 t)
   ("Devmap" 22 t)
   ("Component" 20 t)
   ("Cov%" 5 t)
   ("Summary" 0 nil)])

(defun arxana-browser--tensions-row (item)
  "Row for tension ITEM."
  (let* ((ttype (or (plist-get item :tension/type) ""))
         (devmap (or (plist-get item :tension/devmap) ""))
         (component (or (plist-get item :tension/component) ""))
         (coverage (plist-get item :tension/coverage-pct))
         (summary (or (plist-get item :tension/summary) ""))
         (label (arxana-browser--tension-type-label ttype))
         (face (arxana-browser--tension-type-face ttype)))
    (vector (propertize label 'face face)
            (arxana-lab--truncate (if (keywordp devmap)
                                      (substring (symbol-name devmap) 1)
                                    (format "%s" devmap))
                                  21)
            (arxana-lab--truncate (if (keywordp component)
                                      (substring (symbol-name component) 1)
                                    (format "%s" component))
                                  19)
            (if coverage (format "%.0f" (* 100.0 coverage)) "")
            (arxana-lab--truncate summary 60))))

(defun arxana-browser--tensions-items ()
  "Fetch tensions from futon3c Mission Control."
  (condition-case err
      (let* ((payload (arxana-browser--futon3c-fetch "/mc/tensions"))
             (tensions (plist-get payload :tensions)))
        (if tensions
            (mapcar (lambda (t-entry)
                      (append (list :type 'tension-entry) t-entry))
                    tensions)
          (list (list :type 'info
                      :label "No tensions detected"
                      :description "All devmap components are covered"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch tensions"
                 :description (format "Error: %s" (error-message-string err)))))))

(defun arxana-browser-tension-open-entry (item)
  "Open detail view for a tension ITEM."
  (let* ((buf-name "*Tension Detail*")
         (buf (get-buffer-create buf-name))
         (ttype (plist-get item :tension/type))
         (devmap (plist-get item :tension/devmap))
         (component (plist-get item :tension/component))
         (mission (plist-get item :tension/mission))
         (blocked-by (plist-get item :tension/blocked-by))
         (coverage (plist-get item :tension/coverage-pct))
         (detected (plist-get item :tension/detected-at))
         (summary (plist-get item :tension/summary)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "* Tension: " (or (format "%s" summary) "?") "\n\n")
        (insert (format "- Type :: %s\n" (or ttype "?")))
        (when devmap
          (insert (format "- Devmap :: %s\n" devmap)))
        (when component
          (insert (format "- Component :: %s\n" component)))
        (when mission
          (insert (format "- Mission :: %s\n" mission)))
        (when blocked-by
          (insert (format "- Blocked by :: %s\n" blocked-by)))
        (when coverage
          (insert (format "- Coverage :: %.0f%%\n" (* 100.0 coverage))))
        (when detected
          (insert (format "- Detected :: %s\n" detected)))
        (insert "\n** Actions\n\n")
        (insert "- [ ] Propose mission to address this tension\n")
        (insert "- [ ] Investigate related evidence\n")
        (insert "- [ ] Browse devmap prototype\n")
        (view-mode 1)))
    (display-buffer buf)))

;; =============================================================================
;; Devmap browser view
;; =============================================================================

(defface arxana-lab-devmap-active-face
  '((t :foreground "#61afef"))
  "Face for active devmaps."
  :group 'arxana-lab)

(defface arxana-lab-devmap-complete-face
  '((t :foreground "#98c379"))
  "Face for complete devmaps."
  :group 'arxana-lab)

(defun arxana-browser--devmaps-format ()
  "Column format for devmaps view."
  [("State" 9 t)
   ("Devmap" 28 t)
   ("Comps" 6 t)
   ("Edges" 6 t)
   ("I/O" 6 nil)
   ("Valid" 6 nil)])

(defun arxana-browser--devmaps-row (item)
  "Row for devmap ITEM."
  (let* ((dm-id (plist-get item :devmap/id))
         (name (if (keywordp dm-id)
                   (substring (symbol-name dm-id) 1)
                 (format "%s" dm-id)))
         (state (plist-get item :devmap/state))
         (state-str (if (keywordp state)
                        (substring (symbol-name state) 1)
                      (format "%s" state)))
         (face (cond
                ((string= state-str "complete") 'arxana-lab-devmap-complete-face)
                ((string= state-str "active") 'arxana-lab-devmap-active-face)
                (t 'default)))
         (comp-count (or (plist-get item :devmap/component-count) 0))
         (edge-count (or (plist-get item :devmap/edge-count) 0))
         (in-count (or (plist-get item :devmap/input-count) 0))
         (out-count (or (plist-get item :devmap/output-count) 0))
         (valid (plist-get item :devmap/all-valid)))
    (vector (propertize (upcase state-str) 'face face)
            name
            (format "%d" comp-count)
            (format "%d" edge-count)
            (format "%d/%d" in-count out-count)
            (if valid "ok" "FAIL"))))

(defun arxana-browser--devmaps-items ()
  "Fetch devmap summaries from futon3c Mission Control."
  (condition-case err
      (let ((devmaps (arxana-browser--fetch-devmap-summaries)))
        (if devmaps
            (mapcar (lambda (dm)
                      (append (list :type 'devmap-entry) dm))
                    devmaps)
          (list (list :type 'info
                      :label "No devmaps found"
                      :description "futon5/data/missions/ may be empty"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch devmaps"
                 :description (format "Error: %s" (error-message-string err)))))))

(defun arxana-browser-devmap-open-entry (item)
  "Open detail view for a devmap ITEM."
  (let* ((buf-name "*Devmap Detail*")
         (dm-id (plist-get item :devmap/id))
         (name (if (keywordp dm-id)
                   (substring (symbol-name dm-id) 1)
                 (format "%s" dm-id)))
         (state (plist-get item :devmap/state))
         (components (plist-get item :devmap/components))
         (comp-count (or (plist-get item :devmap/component-count) 0))
         (edge-count (or (plist-get item :devmap/edge-count) 0))
         (in-count (or (plist-get item :devmap/input-count) 0))
         (out-count (or (plist-get item :devmap/output-count) 0))
         (valid (plist-get item :devmap/all-valid))
         (failed (plist-get item :devmap/failed-checks))
         (buf (get-buffer-create buf-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Devmap: %s\n" name))
        (insert (make-string 60 ?=) "\n\n")
        (insert (format "  State:       %s\n" (if (keywordp state)
                                                   (substring (symbol-name state) 1)
                                                 state)))
        (insert (format "  Components:  %d\n" comp-count))
        (insert (format "  Edges:       %d\n" edge-count))
        (insert (format "  Inputs:      %d\n" in-count))
        (insert (format "  Outputs:     %d\n" out-count))
        (insert (format "  Valid:       %s\n" (if valid "yes" "NO")))
        (when (and failed (> (length failed) 0))
          (insert (format "  Failures:    %s\n" failed)))
        (insert "\nComponents:\n")
        (insert (make-string 40 ?-) "\n")
        (dolist (c components)
          (let ((cid (plist-get c :component/id))
                (cname (plist-get c :component/name)))
            (insert (format "  %s  %s\n"
                            (if (keywordp cid)
                                (substring (symbol-name cid) 1)
                              (format "%s" cid))
                            (or cname "")))))
        (insert "\n[i] Ingest as hyperedge  [q] Quit\n")
        (goto-char (point-min))
        (local-set-key (kbd "q") #'quit-window)
        (local-set-key (kbd "i")
                       (lambda ()
                         (interactive)
                         (let ((resp (arxana-browser-ingest-devmap-as-hyperedge item)))
                           (message (if resp "Devmap ingested as hyperedge" "Ingestion failed")))))
        (view-mode 1)))
    (display-buffer buf)))

;; =============================================================================
;; Narrative trail view (per-mission evidence story)
;; =============================================================================

(defun arxana-browser--narrative-trail-format ()
  "Column format for narrative trail view."
  [("Time" 18 t)
   ("Type" 7 t)
   ("Author" 12 t)
   ("Gates" 8 nil)
   ("Preview" 0 nil)])

(defun arxana-browser--narrative-trail-row (item)
  "Row for narrative trail ITEM."
  (let* ((at (or (plist-get item :evidence/at) ""))
         (etype (plist-get item :evidence/type))
         (author (or (plist-get item :evidence/author) ""))
         (body (plist-get item :evidence/body))
         (gates (plist-get body :mission/gates))
         (type-label (arxana-lab--evidence-type-label etype))
         (type-face (arxana-lab--evidence-type-face etype))
         (type-display (if type-face
                           (propertize type-label 'face type-face)
                         type-label))
         (preview (arxana-lab--evidence-body-preview body etype))
         (gates-str (if (and gates (plist-get gates :checked))
                        (format "%d/%d"
                                (plist-get gates :checked)
                                (plist-get gates :total))
                      "")))
    (vector (arxana-lab--truncate at 17)
            type-display
            (arxana-lab--truncate author 11)
            gates-str
            (arxana-lab--truncate preview 50))))

(defun arxana-browser--narrative-trail-items (mission-id)
  "Fetch evidence entries for MISSION-ID as a narrative trail."
  (condition-case err
      (let* ((params (list (cons "subject-id" mission-id)
                           (cons "limit" "200")))
             (response (arxana-browser--evidence-fetch params))
             (entries (plist-get response :entries)))
        (if entries
            (mapcar (lambda (e)
                      (append (list :type 'evidence-entry) e))
                    entries)
          (list (list :type 'info
                      :label (format "No evidence for mission %s" mission-id)
                      :description "Backfill may not have been run yet"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch narrative trail"
                 :description (format "Error: %s" (error-message-string err)))))))

(defun arxana-browser-open-narrative-trail (mission-id)
  "Open a narrative trail for MISSION-ID in the browser."
  (interactive (list (read-string "Mission ID: ")))
  (let ((buffer (get-buffer-create arxana-browser--buffer)))
    (with-current-buffer buffer
      (setq arxana-browser--stack
            (cons (list :view 'narrative-trail
                        :label (format "Trail: %s" mission-id)
                        :mission-id mission-id)
                  arxana-browser--stack))))
  (arxana-browser--render))

;; =============================================================================
;; Tension → Hyperedge ingestion bridge
;; =============================================================================

(defun arxana-browser-ingest-tension-as-hyperedge (tension)
  "Create an Arxana hyperedge from a TENSION entry.
Posts to futon1a via `arxana-store--post-hyperedge'.
Returns the response or nil on error."
  (let* ((ttype (plist-get tension :tension/type))
         (devmap (plist-get tension :tension/devmap))
         (component (plist-get tension :tension/component))
         (mission (plist-get tension :tension/mission))
         (summary (plist-get tension :tension/summary))
         (detected (plist-get tension :tension/detected-at))
         (hx-type (format "tension/%s"
                          (if (keywordp ttype)
                              (substring (symbol-name ttype) 1)
                            (format "%s" ttype))))
         (endpoints (delq nil
                          (list (when devmap
                                  (format "devmap:%s"
                                          (if (keywordp devmap)
                                              (substring (symbol-name devmap) 1)
                                            devmap)))
                                (when component
                                  (format "component:%s/%s"
                                          (if (keywordp devmap)
                                              (substring (symbol-name devmap) 1)
                                            devmap)
                                          (if (keywordp component)
                                              (substring (symbol-name component) 1)
                                            component)))
                                (when mission
                                  (format "mission:%s" mission)))))
         (props (delq nil
                      (list (when summary (cons 'summary summary))
                            (when detected (cons 'detected-at detected))))))
    (arxana-store--post-hyperedge "tension" hx-type endpoints props)))

(defun arxana-browser-ingest-all-tensions ()
  "Fetch all tensions from futon3c and ingest as Arxana hyperedges.
Returns a summary plist with :created and :failed counts."
  (interactive)
  (let* ((payload (arxana-browser--futon3c-fetch "/mc/tensions"))
         (tensions (or (plist-get payload :tensions) '()))
         (created 0)
         (failed 0))
    (dolist (t-entry tensions)
      (condition-case _err
          (let ((resp (arxana-browser-ingest-tension-as-hyperedge t-entry)))
            (if resp
                (setq created (1+ created))
              (setq failed (1+ failed))))
        (error (setq failed (1+ failed)))))
    (let ((msg (format "Tension ingestion: %d created, %d failed (of %d)"
                       created failed (length tensions))))
      (when (called-interactively-p 'interactive)
        (message msg))
      (list :created created :failed failed :total (length tensions)))))

;; =============================================================================
;; Devmap → Hyperedge ingestion bridge
;; =============================================================================

(defun arxana-browser--futon3c-post (endpoint payload)
  "POST PAYLOAD to ENDPOINT on the futon3c server. Returns parsed JSON plist."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Accept" . "application/json")
                                      ("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (base (string-remove-suffix "/" arxana-lab-futon3c-server))
         (url (concat base endpoint))
         (buffer (url-retrieve-synchronously url t t arxana-lab-sessions-request-timeout)))
    (if (not buffer)
        nil
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (result (arxana-lab--parse-json body)))
          (kill-buffer buffer)
          result)))))

(defun arxana-browser--fetch-devmap-summaries ()
  "Fetch devmap summaries from futon3c via mission-control review.
Returns a list of devmap plists."
  (let* ((result (arxana-browser--futon3c-post
                  "/mission-control"
                  '((action . "review"))))
         (lr (plist-get result :last-result))
         (review (plist-get lr :result)))
    (plist-get review :portfolio/devmap-summaries)))

(defun arxana-browser-ingest-devmap-as-hyperedge (devmap)
  "Create an Arxana hyperedge from a DEVMAP summary.
Posts to futon1a via `arxana-store--post-hyperedge'.
Returns the response or nil on error."
  (let* ((dm-id (plist-get devmap :devmap/id))
         (dm-name (if (keywordp dm-id)
                      (substring (symbol-name dm-id) 1)
                    (format "%s" dm-id)))
         (state (plist-get devmap :devmap/state))
         (comp-count (plist-get devmap :devmap/component-count))
         (edge-count (plist-get devmap :devmap/edge-count))
         (input-count (plist-get devmap :devmap/input-count))
         (output-count (plist-get devmap :devmap/output-count))
         (all-valid (plist-get devmap :devmap/all-valid))
         (components (plist-get devmap :devmap/components))
         ;; Build endpoints: devmap ID + each component
         (endpoints (cons (format "devmap:%s" dm-name)
                          (mapcar (lambda (c)
                                    (let ((cid (plist-get c :component/id)))
                                      (format "component:%s/%s"
                                              dm-name
                                              (if (keywordp cid)
                                                  (substring (symbol-name cid) 1)
                                                (format "%s" cid)))))
                                  components)))
         (props (list (cons 'state (if (keywordp state)
                                       (substring (symbol-name state) 1)
                                     (format "%s" state)))
                      (cons 'component-count comp-count)
                      (cons 'edge-count edge-count)
                      (cons 'input-count input-count)
                      (cons 'output-count output-count)
                      (cons 'all-valid (if all-valid "true" "false")))))
    (arxana-store--post-hyperedge "devmap" "devmap/prototype" endpoints props)))

(defun arxana-browser-ingest-all-devmaps ()
  "Fetch all devmap summaries from futon3c and ingest as Arxana hyperedges.
Returns a summary plist with :created and :failed counts."
  (interactive)
  (let* ((devmaps (arxana-browser--fetch-devmap-summaries))
         (created 0)
         (failed 0))
    (dolist (dm devmaps)
      (condition-case _err
          (let ((resp (arxana-browser-ingest-devmap-as-hyperedge dm)))
            (if resp
                (setq created (1+ created))
              (setq failed (1+ failed))))
        (error (setq failed (1+ failed)))))
    (let ((msg (format "Devmap ingestion: %d created, %d failed (of %d)"
                       created failed (length devmaps))))
      (when (called-interactively-p 'interactive)
        (message msg))
      (list :created created :failed failed :total (length devmaps)))))

;; =============================================================================
;; Reflection grounding — about-var relations
;; =============================================================================

(defun arxana-browser--resolve-var (ns-name var-name)
  "Resolve a Clojure var via futon3c reflection API.
Returns a plist with :reflection/ns, :reflection/file, :reflection/line, etc.
or nil if not found."
  (let* ((endpoint (format "/reflect/var/%s/%s"
                           (url-hexify-string ns-name)
                           (url-hexify-string var-name)))
         (payload (arxana-browser--futon3c-fetch endpoint)))
    (when (and payload (plist-get payload :ok))
      (plist-get payload :envelope))))

(defun arxana-browser-ground-claim-to-var (claim-id ns-name var-name)
  "Create an about-var hyperedge linking CLAIM-ID to var NS-NAME/VAR-NAME.
Resolves the var via reflection API and stores the reflection envelope
as hyperedge props. Returns the response or nil."
  (interactive
   (list (read-string "Claim/evidence ID: ")
         (read-string "Namespace: ")
         (read-string "Var name: ")))
  (let ((envelope (arxana-browser--resolve-var ns-name var-name)))
    (unless envelope
      (user-error "Could not resolve var %s/%s" ns-name var-name))
    (let* ((var-id (format "var:%s/%s" ns-name var-name))
           (props (delq nil
                        (list (cons 'reflection/ns
                                    (format "%s" (or (plist-get envelope :reflection/ns) "")))
                              (cons 'reflection/symbol
                                    (format "%s" (or (plist-get envelope :reflection/symbol) "")))
                              (cons 'reflection/file
                                    (or (plist-get envelope :reflection/file) ""))
                              (cons 'reflection/line
                                    (plist-get envelope :reflection/line))
                              (cons 'reflection/arglists
                                    (format "%s" (or (plist-get envelope :reflection/arglists) "")))
                              (when (plist-get envelope :reflection/doc)
                                (cons 'reflection/doc
                                      (plist-get envelope :reflection/doc)))
                              (cons 'resolved-at
                                    (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)))))
           (resp (arxana-store--post-hyperedge
                  "about-var" "reflection/about-var"
                  (list claim-id var-id) props)))
      (when (called-interactively-p 'interactive)
        (message "Grounded %s → %s/%s (line %s)"
                 claim-id ns-name var-name
                 (or (plist-get envelope :reflection/line) "?")))
      resp)))

(defun arxana-browser-verify-reflection-snapshot (hyperedge)
  "Re-resolve a var from an about-var HYPEREDGE and check for staleness.
HYPEREDGE is an alist with :hx/props containing reflection/* fields.
Returns :ok, :stale (signature changed), or :missing (var gone)."
  (let* ((props (cdr (assq :hx/props hyperedge)))
         (ns-name (or (cdr (assq :reflection/ns props))
                      (cdr (assq 'reflection/ns props)) ""))
         (var-name (or (cdr (assq :reflection/symbol props))
                       (cdr (assq 'reflection/symbol props)) ""))
         (old-line (or (cdr (assq :reflection/line props))
                       (cdr (assq 'reflection/line props))))
         (old-arglists (or (cdr (assq :reflection/arglists props))
                           (cdr (assq 'reflection/arglists props)))))
    (let ((envelope (arxana-browser--resolve-var
                     (format "%s" ns-name) (format "%s" var-name))))
      (cond
       ((not envelope) :missing)
       ((or (not (equal old-line (plist-get envelope :reflection/line)))
            (not (equal old-arglists
                        (format "%s" (or (plist-get envelope :reflection/arglists) "")))))
        :stale)
       (t :ok)))))

;;; --- Invariant Violations View ---

(defface arxana-violation-inv1-face
  '((t :foreground "#e5c07b" :weight bold))
  "Face for INV-1 (undocumented) violations."
  :group 'arxana-browser)

(defface arxana-violation-inv2-face
  '((t :foreground "#e06c75" :weight bold))
  "Face for INV-2 (uncovered) violations."
  :group 'arxana-browser)

(defface arxana-violation-inv3-face
  '((t :foreground "#61afef" :weight bold))
  "Face for INV-3 (orphan) violations."
  :group 'arxana-browser)

(defface arxana-violation-auto-fixable-face
  '((t :foreground "#98c379" :weight bold))
  "Face for auto-fixable structural-law violations."
  :group 'arxana-browser)

(defface arxana-violation-needs-review-face
  '((t :foreground "#e5c07b" :weight bold))
  "Face for needs-review structural-law violations."
  :group 'arxana-browser)

(defface arxana-violation-informational-face
  '((t :foreground "#61afef"))
  "Face for informational structural-law violations."
  :group 'arxana-browser)

(defface arxana-browser-tracer-family-face
  '((t :foreground "hot pink" :weight bold))
  "Face for pipeline-tracer items in the Invariant Queue.

These are work items (M-invariant-queue-extend tracks plus
substrate-2 / war-machine deferred-stub families) being watched as
pipeline-flow signals. Highlighted hot pink so they stand out
against regular candidate invariants."
  :group 'arxana-browser)

(defun arxana-browser--violations-format ()
  "Column format for violations view."
  [("Rule" 28 t)
   ("Domain" 12 t)
   ("Entity" 36 t)
   ("Action" 0 nil)])

(defun arxana-browser--field (obj key)
  "Return KEY from OBJ, handling both plists and alists."
  (cond
   ((null obj) nil)
   ((and (listp obj) (keywordp (car obj)))
    (plist-get obj key))
   ((listp obj)
    (alist-get key obj nil nil #'eq))
   (t nil)))

(defun arxana-browser--field-present-p (obj key)
  "Return non-nil when OBJ explicitly contains KEY."
  (cond
   ((null obj) nil)
   ((and (listp obj) (keywordp (car obj)))
    (plist-member obj key))
   ((listp obj)
    (assq key obj))
   (t nil)))

(defun arxana-browser--violation-props (item)
  "Return the props plist for violation ITEM."
  (or (arxana-browser--field item :hx/props) '()))

(defun arxana-browser--violation-endpoints (item)
  "Return the endpoint vector for violation ITEM."
  (or (arxana-browser--field item :hx/endpoints) '()))

(defun arxana-browser--violation-rule (item)
  "Return a display rule for violation ITEM."
  (let* ((props (arxana-browser--violation-props item))
         (rule (arxana-browser--field props :rule))
         (family (arxana-browser--field props :family))
         (legacy (arxana-browser--field props :invariant))
         (endpoints (arxana-browser--violation-endpoints item))
         (inv-endpoint (car endpoints)))
    (cond
     (legacy (format "%s" legacy))
     ((and family rule) (format "%s/%s" family rule))
     (rule (format "%s" rule))
     ((and (stringp inv-endpoint) (string-prefix-p "inv:" inv-endpoint))
      (substring inv-endpoint 4))
     (t (format "%s" (or (arxana-browser--field item :hx/type) "?"))))))

(defun arxana-browser--violation-domain (item)
  "Return a display domain for violation ITEM."
  (let* ((props (arxana-browser--violation-props item))
         (domain (arxana-browser--field props :domain)))
    (if domain
        (format "%s" domain)
      "legacy")))

(defun arxana-browser--violation-entity (item)
  "Return a display entity string for violation ITEM."
  (let* ((endpoints (arxana-browser--violation-endpoints item))
         (entity-eps (if (and endpoints
                              (stringp (car endpoints))
                              (string-prefix-p "inv:" (car endpoints)))
                         (cdr endpoints)
                       endpoints)))
    (if entity-eps
        (mapconcat (lambda (x) (format "%s" x)) entity-eps ", ")
      "")))

(defun arxana-browser--violation-action (item)
  "Return an action/resolution string for violation ITEM."
  (let* ((props (arxana-browser--violation-props item))
         (label (arxana-browser--field props :label))
         (resolution (arxana-browser--field props :resolution))
         (message (arxana-browser--field props :message))
         (actionability (arxana-browser--field props :actionability)))
    (cond
     (resolution (format "%s" resolution))
     (label (format "%s" label))
     (message (format "%s" message))
     (actionability (format "%s" actionability))
     (t ""))))

(defun arxana-browser--violation-active-p (item)
  "Return non-nil when violation ITEM should be shown as live."
  (let* ((props (arxana-browser--violation-props item))
         (state (arxana-browser--field props :state))
         (active (arxana-browser--field props :active)))
    (not (or (equal state "cleared")
             (equal active "false")
             (equal active :false)))))

(defun arxana-browser--violation-face (inv)
  "Return face for invariant INV string."
  (cond
   ((string-match-p "INV-1" inv) 'arxana-violation-inv1-face)
   ((string-match-p "INV-2" inv) 'arxana-violation-inv2-face)
   ((string-match-p "INV-3" inv) 'arxana-violation-inv3-face)
   ((string-match-p "INV-4" inv) 'arxana-violation-inv2-face)  ; reuse red for math
   (t 'default)))

(defun arxana-browser--violation-display-face (item)
  "Return the preferred face for violation ITEM."
  (let* ((props (arxana-browser--violation-props item))
         (actionability (let ((raw (arxana-browser--field props :actionability)))
                          (and raw (format "%s" raw))))
         (rule (arxana-browser--violation-rule item)))
    (cond
     ((equal actionability "auto-fixable") 'arxana-violation-auto-fixable-face)
     ((equal actionability "needs-review") 'arxana-violation-needs-review-face)
     ((equal actionability "informational") 'arxana-violation-informational-face)
     (t (arxana-browser--violation-face rule)))))

(defun arxana-browser--violations-row (item)
  "Row for violation ITEM."
  (let* ((rule (arxana-browser--violation-rule item))
         (domain (arxana-browser--violation-domain item))
         (entity (arxana-browser--violation-entity item))
         (action (arxana-browser--violation-action item))
         (face (arxana-browser--violation-display-face item)))
    (vector (propertize rule 'face face)
            (arxana-lab--truncate domain 11)
            (arxana-lab--truncate entity 35)
            (arxana-lab--truncate action 60))))

(defun arxana-browser--coerce-hyperedges (resp)
  "Return a hyperedge list from RESP."
  (cond
   ((arxana-browser--field-present-p resp :hyperedges)
    (arxana-browser--field resp :hyperedges))
   ((and (listp resp) (listp (car resp)))
    resp)
   (t nil)))

(defun arxana-browser--tag-violation-entry (hx)
  "Attach `:type violation-entry` to HX, preserving alist/plist shape."
  (if (and (listp hx) (keywordp (car hx)))
      (append (list :type 'violation-entry) hx)
    (append (list (cons :type 'violation-entry)) hx)))

(defun arxana-browser--violations-items ()
  "Fetch invariant violations from futon1a hyperedge store."
  (let ((all-violations nil)
        (seen (make-hash-table :test 'equal)))
    ;; Prefer the generic invariant/violation shape, but keep older
    ;; per-invariant hyperedges visible until everything is ported.
    (dolist (inv-type '("invariant/violation"
                        "invariant/undocumented-entry-point"
                        "invariant/uncovered-component"
                        "invariant/orphan-namespace"
                        "invariant/ungrounded-definition"))
      (let* ((resp (ignore-errors
                     (arxana-store-fetch-hyperedges :type inv-type :limit 200)))
             (hxs (arxana-browser--coerce-hyperedges resp)))
        (dolist (hx hxs)
          (let ((hx-id (or (arxana-browser--field hx :hx/id)
                           (format "%s" hx))))
            (unless (or (gethash hx-id seen)
                        (not (arxana-browser--violation-active-p hx)))
              (puthash hx-id t seen)
              (push (arxana-browser--tag-violation-entry hx) all-violations))))))
    (if all-violations
        (nreverse all-violations)
      (list (list :type 'info
                  :label "No violations"
                  :description "All invariants passed — store may need refresh (run ingest-three-columns.py --invariants or emit structural-law hyperedges).")))))

(defun arxana-browser-violation-open-entry (item)
  "Open detail view for a violation ITEM."
  (let* ((buf-name "*Violation Detail*")
         (buf (get-buffer-create buf-name))
         (props (arxana-browser--violation-props item))
         (inv (arxana-browser--violation-rule item))
         (summary (or (arxana-browser--field props :summary)
                      (arxana-browser--field props :message)
                      "?"))
         (resolution (or (arxana-browser--field props :resolution)
                         (arxana-browser--field props :label)
                         (arxana-browser--field props :actionability)
                         "?"))
         (domain (arxana-browser--violation-domain item))
         (family (or (let ((raw (arxana-browser--field props :family)))
                       (and raw (format "%s" raw)))
                     "?"))
         (endpoints (arxana-browser--violation-endpoints item))
         (entity (or (arxana-browser--violation-entity item) "?"))
         (hx-id (or (arxana-browser--field item :hx/id) "?")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (org-mode)
        (insert "* Violation: " summary "\n\n")
        (insert (format "- Invariant :: %s\n" inv))
        (insert (format "- Family :: %s\n" family))
        (insert (format "- Domain :: %s\n" domain))
        (insert (format "- Entity :: %s\n" entity))
        (insert (format "- Hyperedge ID :: %s\n" hx-id))
        (insert (format "- Resolution :: %s\n" resolution))
        (when endpoints
          (insert (format "- Endpoints :: %s\n"
                          (mapconcat (lambda (x) (format "%s" x))
                                     endpoints
                                     ", "))))
        (insert "\n** Context\n\n")
        (cond
         ((string-match-p "INV-1" inv)
          (insert "This namespace has no docstring. Add a ns docstring or mark\n")
          (insert "the namespace as internal (non-entry-point).\n"))
         ((string-match-p "INV-2" inv)
          (insert "This devmap component has no covering mission. Create a mission\n")
          (insert "that addresses this component, or annotate coverage explicitly.\n"))
         ((string-match-p "INV-3" inv)
          (insert "This namespace is not required by any other namespace in the\n")
          (insert "futon ecosystem. It may be dead code, or its integration point\n")
          (insert "may be missing.\n"))
         ((string-match-p "INV-4" inv)
          (insert "This scope (definition/binding) is never referenced by an iatc\n")
          (insert "(argumentation) edge. Add an iatc edge with act=reference to\n")
          (insert "ground this definition in the argumentative structure.\n"))
         ((string= family "existence")
          (insert "A required structural entity or relation is missing. The\n")
          (insert "violation endpoints identify the canonical invariant plus the\n")
          (insert "runtime objects that failed to satisfy it.\n"))
         ((string= family "required-outputs")
          (insert "A phase or workflow transition happened without producing a\n")
          (insert "required output artifact. Repair the missing output before\n")
          (insert "treating the cycle as complete.\n"))
         ((string= family "phase-ordering")
          (insert "This workflow advanced out of its declared order. The repair is\n")
          (insert "to realign runtime state with the canonical phase sequence.\n"))
         ((string= family "cross-store-agreement")
          (insert "Two live stores disagree about the same runtime fact. Treat this\n")
          (insert "as a review item until the responsible surfaces are reconciled.\n"))
         (t
          (insert "This violation uses the generic invariant/violation shape.\n")
          (insert "Inspect the rule, family, and endpoints above to determine the\n")
          (insert "relevant repair path.\n")))
        (insert "\n** Actions\n\n")
        (insert "- Press =q= to close this buffer\n")
        (when (string-match-p "^ns:" entity)
          (let ((ns-name (replace-regexp-in-string "^ns:" "" entity)))
            (insert (format "- Open namespace: M-x cider-find-ns RET %s RET\n" ns-name))))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(provide 'arxana-browser-lab)
;;; arxana-browser-lab.el ends here
