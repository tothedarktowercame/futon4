;;; arxana-vsatarcs-sales.el --- Arxana Sales pipeline view -*- lexical-binding: t; -*-

;;; Commentary:
;; The Arxana Sales pipeline: a navigable view over `~/code/sales/sales.edn',
;; the DUAL of the Ledger (`arxana-vsatarcs-ledger.el').  Where the Ledger runs
;; work → billing, this runs INTERACTIONS → conversion: clients are inhabitants
;; moving through stages (Prospect → Engaged → Proposed → Agreed → Recurring,
;; or Churned), each with an amortized JOURNEY of dated interaction-events and a
;; ROLODEX card (the human/relationship layer: how-we-met, contacts, notes).
;;
;; Models the sales↔client coupling as a MUTUALISM (futon5/docs/
;; {client,sales}-terminal-vocabulary.md): the operator may observe/invite the
;; client's consent-gated terminals (:agree/:pay/:refer) but never fire them.
;;
;; Reads the canonical EDN live on every call (no cache) via the shared in-tree
;; reader (`arxana-browser-rewrites'); same discipline as the Ledger.

;;; Code:

(require 'cl-lib)
(require 'arxana-browser-rewrites) ; shared EDN reader

(defgroup arxana-vsatarcs-sales nil
  "Arxana Sales pipeline view over the sales/client journey record."
  :group 'arxana-vsatarcs)

(defcustom arxana-sales-file (expand-file-name "~/code/sales/sales.edn")
  "Path to the canonical sales pipeline EDN.  Read live on every call."
  :type 'file
  :group 'arxana-vsatarcs-sales)

(defvar arxana-sales--buffer "*Arxana Sales*")

(defconst arxana-sales--stages
  '((:prospect  :prospect  "Prospect"  "identified, not yet engaged")
    (:engaged   :engaged   "Engaged"   "in conversation / taking meetings")
    (:proposed  :proposed  "Proposed"  "a scoped, priced proposition is on the table")
    (:agreed    :agreed    "Agreed"    "go-ahead given; not yet a paying cycle")
    (:recurring :recurring "Recurring" "established, paying client")
    (:churned   :churned   "Churned"   "went cold / declined (legitimate)"))
  "Entry-point → (stage title blurb), in pipeline order so clients are seen
moving through acquisition.  Dual of the Ledger's invoice strata.")

;; ---------------------------------------------------------------- data ----

(defun arxana-sales--read ()
  "Read the sales EDN; return its plist, or nil if unreadable."
  (when (file-readable-p arxana-sales-file)
    (ignore-errors (arxana-browser-rewrites--read-edn-file arxana-sales-file))))

(defun arxana-sales--clients (data) (append (plist-get data :clients) nil))
(defun arxana-sales--journey (data) (append (plist-get data :journey) nil))
(defun arxana-sales--rolodex (data) (append (plist-get data :rolodex) nil))
(defun arxana-sales--demos (data) (append (plist-get data :demos) nil))

(defun arxana-sales--clients-at (clients stage)
  (cl-remove-if-not (lambda (c) (eq (plist-get c :client/stage) stage)) clients))

(defun arxana-sales--events-for (journey client-id)
  "Events for CLIENT-ID, in time-linear order (ISO-string lexical sort)."
  (sort (cl-remove-if-not (lambda (e) (eq (plist-get e :event/client) client-id)) journey)
        (lambda (a b) (string< (or (plist-get a :event/date) "9999")
                               (or (plist-get b :event/date) "9999")))))

(defun arxana-sales--card-for (rolodex client-id)
  (cl-find-if (lambda (c) (eq (plist-get c :card/client) client-id)) rolodex))

(defun arxana-sales--date-str (d)
  (cond ((null d) "—")
        ((and (stringp d) (>= (length d) 10)) (substring d 0 10))
        (t (format "%s" d))))

(defun arxana-sales--sym-str (x)
  "Render a keyword/symbol/string X without the leading colon."
  (cond ((keywordp x) (substring (symbol-name x) 1))
        ((symbolp x) (symbol-name x))
        (t (format "%s" x))))

;; ----------------------------------------------- native edit (surgical) ----
;; Find the target map by an anchor literal (e.g. ":card/id :eric-white-brookes")
;; and replace only the named field's value sexp; comments + #inst dates survive.

(defun arxana-sales--set-field! (anchor key value-str)
  "In `arxana-sales-file', within the map containing ANCHOR, set KEY to literal
VALUE-STR (inserting KEY after ANCHOR if absent).  Targeted; preserves comments."
  (with-temp-buffer
    (insert-file-contents arxana-sales-file)
    (modify-syntax-entry ?\{ "(}")
    (modify-syntax-entry ?\} "){")
    (modify-syntax-entry ?\[ "(]")
    (modify-syntax-entry ?\] ")[")
    (goto-char (point-min))
    (unless (search-forward anchor nil t)
      (error "Sales: anchor not found: %s" anchor))
    (let* ((apt (point))
           (mapbeg (save-excursion (goto-char apt) (backward-up-list) (point)))
           (mapend (save-excursion (goto-char mapbeg) (forward-sexp) (point))))
      (save-restriction
        (narrow-to-region mapbeg mapend)
        (goto-char (point-min))
        (if (re-search-forward (concat (regexp-quote key) "[ \t\n]+") nil t)
            (let ((vbeg (point)))
              (forward-sexp)
              (delete-region vbeg (point))
              (goto-char vbeg)
              (insert value-str))
          (goto-char (point-min))
          (search-forward anchor)
          (insert (format "\n   %s %s" key value-str)))))
    (write-region (point-min) (point-max) arxana-sales-file nil 'quiet)))

(defun arxana-sales--edn-string (s)
  (concat "\"" (replace-regexp-in-string "\\([\"\\\\]\\)" "\\\\\\1" s) "\""))

;; -------------------------------------------------------------- render ----

(defun arxana-sales--button (label action &optional help)
  (insert-text-button label 'follow-link t 'help-echo help 'action action))

(define-derived-mode arxana-sales-mode special-mode "Arxana-Sales"
  "Major mode for the Arxana Sales pipeline view.
\\{arxana-sales-mode-map}")

(defvar-local arxana-sales--refresh-fn nil
  "Thunk re-rendering the current Arxana Sales view (bound to `g').")

(defun arxana-sales-refresh ()
  "Re-read sales.edn and re-render the current view."
  (interactive)
  (if arxana-sales--refresh-fn (funcall arxana-sales--refresh-fn) (arxana-sales-browse)))

(defun arxana-sales-edit-file ()
  "Open the canonical sales EDN for editing.  Press `g' to re-read after save."
  (interactive)
  (find-file arxana-sales-file))

(defun arxana-sales--render-frame (body-fn &optional refresh-fn)
  (let ((buf (get-buffer-create arxana-sales--buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'arxana-sales-mode) (arxana-sales-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall body-fn)
        (goto-char (point-min)))
      (when refresh-fn (setq arxana-sales--refresh-fn refresh-fn)))
    (pop-to-buffer buf)))

;; --- at-point dispatch -----------------------------------------------------

(defun arxana-sales--prop-at-point (prop)
  (or (get-text-property (point) prop)
      (and (> (point) (point-min)) (get-text-property (1- (point)) prop))))

;; --- client journey frame --------------------------------------------------

(defun arxana-sales--insert-event (e)
  "Insert one journey event line."
  (let ((term (plist-get e :event/sales-terminal))
        (resp (plist-get e :event/client-response))
        (cost (plist-get e :event/cost-hours))
        (stage (plist-get e :event/stage-after)))
    (insert (format "  • %s" (arxana-sales--date-str (plist-get e :event/date))))
    (when term  (insert (format "  sales:%s" (arxana-sales--sym-str term))))
    (when resp  (insert (format "  → client:%s" (arxana-sales--sym-str resp))))
    (when cost  (insert (format "  (%sh)" cost)))
    (when stage (insert (format "  ⇒ %s" (arxana-sales--sym-str stage))))
    (insert "\n")
    (when (plist-get e :event/content)
      (insert (format "      %s\n" (plist-get e :event/content))))
    (when (plist-get e :event/interest-match)
      (insert (format "      match: %s\n" (plist-get e :event/interest-match))))
    (insert "\n")))

(defun arxana-sales--referral-glyph (status)
  (pcase status (:live "●") (:latent "○") (:withdrawn "✗") (_ "·")))

(defun arxana-sales--insert-card (card)
  "Insert a Rolodex CARD (tagged for at-point editing)."
  (let ((beg (point))
        (kind (plist-get card :card/kind)))
    (insert (format "  ┌─ Rolodex: %s" (or (plist-get card :card/name) "?")))
    (let ((org (plist-get card :card/org)))
      (when (and (stringp org) (not (string-empty-p org))) (insert (format " · %s" org))))
    (when kind (insert (format "  [%s]" (arxana-sales--sym-str kind))))
    (when (plist-get card :card/in-pipeline?) (insert "  ☑ in pipeline"))
    (insert "\n")
    (dolist (f '((:card/role           "role")
                 (:card/email          "email")
                 (:card/phone          "phone")
                 (:card/first-contact  "first-contact")
                 (:card/referred-by    "referred-by")
                 (:card/how-we-met     "how-we-met")
                 (:card/relationship   "relationship")
                 (:card/friendly-interest "friendly-interest")
                 (:card/notes          "notes")))
      (let ((v (plist-get card (car f))))
        (insert (format "  │ %-17s %s\n" (cadr f)
                        (if (and (stringp v) (string-empty-p v)) "—" (or v "—"))))))
    (let ((terr (append (plist-get card :card/interest-territories) nil)))
      (when terr (insert (format "  │ %-17s %s\n" "territories"
                                 (mapconcat #'identity terr ", ")))))
    (let ((refs (append (plist-get card :card/referrals) nil)))
      (when refs
        (insert "  │ referrals (edges = lead sources):\n")
        (dolist (r refs)
          (insert (format "  │   %s %s%s\n"
                          (arxana-sales--referral-glyph (plist-get r :status))
                          (or (plist-get r :to) "?")
                          (let ((rs (plist-get r :reason)))
                            (if rs (format "  [%s]" (arxana-sales--sym-str rs)) ""))))
          (when (plist-get r :note)
            (insert (format "  │       %s\n" (plist-get r :note)))))))
    (insert "  └─ (e: edit a field)\n\n")
    (add-text-properties beg (point) (list 'arxana-sales-card card))))

(defun arxana-sales--render-client (client-id)
  "Render one client: Rolodex card header + amortized journey timeline."
  (let* ((data (arxana-sales--read))
         (client (cl-find-if (lambda (c) (eq (plist-get c :client/id) client-id))
                             (arxana-sales--clients data)))
         (events (arxana-sales--events-for (arxana-sales--journey data) client-id))
         (card (arxana-sales--card-for (arxana-sales--rolodex data) client-id)))
    (arxana-sales--render-frame
     (lambda ()
       (insert (format "Arxana Sales — %s\n\n" (or (plist-get client :client/name)
                                                   (arxana-sales--sym-str client-id))))
       (arxana-sales--button "[← entry points]" (lambda (_) (arxana-sales-browse)))
       (insert "\n\n")
       (when client
         (insert (format "  stage: %s    engagement: %s\n"
                         (arxana-sales--sym-str (plist-get client :client/stage))
                         (arxana-sales--sym-str (plist-get client :client/engagement))))
         (when (plist-get client :client/mutualism)
           (insert (format "  mutualism: %s\n" (plist-get client :client/mutualism))))
         (when (plist-get client :client/note)
           (insert (format "  note: %s\n" (plist-get client :client/note))))
         (insert "\n"))
       (when card (arxana-sales--insert-card card))
       (insert (format "  Journey (%d events, time-linear):\n\n" (length events)))
       (if (null events)
           (insert "    (none yet)\n")
         (dolist (e events) (arxana-sales--insert-event e)))
       (add-text-properties (point-min) (point-max) (list 'arxana-sales-client client-id)))
     (lambda () (arxana-sales--render-client client-id)))))

;; --- stage frame -----------------------------------------------------------

(defun arxana-sales--render-stage (stage)
  "List clients at STAGE; each opens its journey on RET."
  (let* ((spec (assq stage arxana-sales--stages))
         (data (arxana-sales--read))
         (clients (arxana-sales--clients-at (arxana-sales--clients data) stage)))
    (arxana-sales--render-frame
     (lambda ()
       (insert (format "Arxana Sales — %s  (%s)\n\n" (nth 2 spec) (nth 3 spec)))
       (arxana-sales--button "[← entry points]" (lambda (_) (arxana-sales-browse)))
       (insert "\n\n")
       (if (null clients)
           (insert "  (none)\n")
         (dolist (c clients)
           (let ((id (plist-get c :client/id)))
             (arxana-sales--button
              (format "▸ %s" (or (plist-get c :client/name) (arxana-sales--sym-str id)))
              (let ((i id)) (lambda (_) (arxana-sales--render-client i)))
              "Open this client's journey")
             (insert (format "   — %s\n" (arxana-sales--sym-str (plist-get c :client/engagement))))))))
     (lambda () (arxana-sales--render-stage stage)))))

;; --- rolodex frame ---------------------------------------------------------

(defun arxana-sales--render-rolodex ()
  "List all Rolodex cards (clients + pre-clients)."
  (let* ((data (arxana-sales--read))
         (cards (arxana-sales--rolodex data)))
    (arxana-sales--render-frame
     (lambda ()
       (insert "Arxana Sales — Rolodex\n\n")
       (arxana-sales--button "[← entry points]" (lambda (_) (arxana-sales-browse)))
       (insert "    ")
       (arxana-sales--button "[+ add card]" (lambda (_) (arxana-sales-add-card)))
       (insert "\n\n")
       (if (null cards)
           (insert "  (none)\n")
         (dolist (c cards)
           (let ((cl (plist-get c :card/client)))
             (if cl
                 (arxana-sales--button
                  (format "▸ %s" (or (plist-get c :card/name) "?"))
                  (let ((i cl)) (lambda (_) (arxana-sales--render-client i)))
                  "Open this client's journey")
               (insert (format "▸ %s" (or (plist-get c :card/name) "?"))))
             (insert (format "   — %s%s\n"
                             (or (plist-get c :card/org) "")
                             (if cl "  (client)" "  (pre-client)")))
             (arxana-sales--insert-card c)))))
     #'arxana-sales--render-rolodex)))

;; --- leads frame (Rolodex → Prospect feed) ---------------------------------

(defun arxana-sales--render-leads ()
  "Harvest lead sources: live/latent referral edges across all cards, plus
contacts not yet in the pipeline.  Each can be promoted into a Prospect."
  (let* ((data (arxana-sales--read))
         (cards (arxana-sales--rolodex data)))
    (arxana-sales--render-frame
     (lambda ()
       (insert "Arxana Sales — Leads  (Rolodex → Prospect feed)\n")
       (insert "The lead-gen gradient: the network's edges become pipeline prospects.\n\n")
       (arxana-sales--button "[← entry points]" (lambda (_) (arxana-sales-browse)))
       (insert "\n\n")
       ;; 1) referral edges (live ● / latent ○) across all cards
       (insert "Referral edges — candidate leads via connectors:\n\n")
       (let ((any nil))
         (dolist (card cards)
           (let ((cname (or (plist-get card :card/name) "?"))
                 (trust (plist-get card :card/relationship)))
             (dolist (r (append (plist-get card :card/referrals) nil))
               (when (memq (plist-get r :status) '(:live :latent))
                 (setq any t)
                 (insert (format "  %s %s\n"
                                 (arxana-sales--referral-glyph (plist-get r :status))
                                 (or (plist-get r :to) "?")))
                 (insert (format "      via %s%s\n" cname
                                 (if trust (format "  —  %s" trust) "")))
                 (when (plist-get r :note)
                   (insert (format "      %s\n" (plist-get r :note))))
                 (arxana-sales--button "      [promote → Prospect]"
                                       (let ((cn cname) (e r))
                                         (lambda (_) (arxana-sales-promote-edge cn e)))
                                       "Create a Prospect card + client from this referral")
                 (insert "\n\n")))))
         (unless any (insert "  (no live/latent edges)\n\n")))
       ;; 2) contacts not yet in the pipeline — promote directly
       (insert "Contacts not yet in the pipeline — promote directly:\n\n")
       (let ((any nil))
         (dolist (card cards)
           (unless (or (plist-get card :card/in-pipeline?) (plist-get card :card/client))
             (setq any t)
             (let ((beg (point)))
               (insert (format "  ▸ %s%s\n" (or (plist-get card :card/name) "?")
                               (let ((org (plist-get card :card/org)))
                                 (if (and (stringp org) (not (string-empty-p org)))
                                     (format "  ·  %s" org) ""))))
               (arxana-sales--button "      [promote → Prospect]"
                                     (let ((c card)) (lambda (_) (arxana-sales-promote-card c)))
                                     "Tick in-pipeline + add a :prospect client (or press p)")
               (insert "\n\n")
               (add-text-properties beg (point) (list 'arxana-sales-card card)))))
         (unless any (insert "  (all carded contacts are already in the pipeline)\n"))))
     #'arxana-sales--render-leads)))

;; --- demos frame (scan → demo → call, indexed) -----------------------------

(defun arxana-sales--demo-glyph (outcome)
  (pcase outcome
    (:landed "✓") (:no-interest "✗") (:built-not-shown "⚠")
    (:pending "⋯") (:idea "○") (_ "·")))

(defconst arxana-sales--demo-built '(:landed :no-interest :built-not-shown)
  "Outcomes that mean a demo was actually built (cost was spent).")

(defun arxana-sales--render-demos ()
  "Index the demos: each carries a dual warrant (eoi-new ⊗ territory-match),
a build cost, and an outcome.  Shows the depositing ratio (landed / built)
and the hermit risk (built but never shown)."
  (let* ((data (arxana-sales--read))
         (demos (arxana-sales--demos data))
         (built (cl-remove-if-not
                 (lambda (d) (memq (plist-get d :demo/outcome) arxana-sales--demo-built)) demos))
         (landed (cl-remove-if-not (lambda (d) (eq (plist-get d :demo/outcome) :landed)) demos))
         (hermit (cl-remove-if-not (lambda (d) (eq (plist-get d :demo/outcome) :built-not-shown)) demos))
         (cost (apply #'+ 0.0 (delq nil (mapcar (lambda (d) (plist-get d :demo/build-cost-hours)) demos)))))
    (arxana-sales--render-frame
     (lambda ()
       (insert "Arxana Sales — Demos  (scan → demo → call)\n")
       (insert "A demo is the :demonstrate hinge. Dual warrant = eoi-new ⊗ territory-match\n")
       (insert "→ positive EV even at low conversion (platform-value floor is non-zero).\n\n")
       (arxana-sales--button "[← entry points]" (lambda (_) (arxana-sales-browse)))
       (insert "\n\n")
       (insert (format "  Built: %d   Landed: %d   Depositing ratio: %s   Hermit (built, not shown): %d   Σcost: %.1fh\n\n"
                       (length built) (length landed)
                       (if (> (length built) 0)
                           (format "%.0f%%" (* 100.0 (/ (float (length landed)) (length built))))
                         "—")
                       (length hermit) cost))
       (if (null demos)
           (insert "  (none)\n")
         (dolist (d demos)
           (let* ((w (plist-get d :demo/warrant))
                  (terr (append (plist-get w :territory-match) nil))
                  (c (plist-get d :demo/build-cost-hours)))
             (insert (format "  %s %s\n" (arxana-sales--demo-glyph (plist-get d :demo/outcome))
                             (or (plist-get d :demo/title) "?")))
             (insert (format "      for: %s   outcome: %s%s\n"
                             (arxana-sales--sym-str (plist-get d :demo/for))
                             (arxana-sales--sym-str (plist-get d :demo/outcome))
                             (if c (format "   cost: %sh" c) "")))
             (when (plist-get w :eoi-new)
               (insert (format "      eoi-new: %s\n" (plist-get w :eoi-new))))
             (when terr
               (insert (format "      territory-match: %s\n" (mapconcat #'identity terr ", "))))
             (when (plist-get d :demo/note)
               (insert (format "      %s\n" (plist-get d :demo/note))))
             (insert "\n")))))
     #'arxana-sales--render-demos)))

;; --- editing commands ------------------------------------------------------

(defconst arxana-sales--card-fields
  '(":card/role" ":card/email" ":card/phone" ":card/first-contact"
    ":card/referred-by" ":card/how-we-met" ":card/relationship"
    ":card/friendly-interest" ":card/notes")
  "String-valued Rolodex card fields offered for editing.
\(Structured :card/referrals edges are edited via E: raw EDN for now.)")

(defun arxana-sales-edit-card-field ()
  "Edit a field on the Rolodex card at point (in-place, comment-preserving)."
  (interactive)
  (let* ((card (arxana-sales--prop-at-point 'arxana-sales-card))
         (id (and card (plist-get card :card/id))))
    (unless id (user-error "No Rolodex card at point"))
    (let* ((field (completing-read (format "Field for %s: " (arxana-sales--sym-str id))
                                   arxana-sales--card-fields nil t))
           (cur (plist-get card (intern field)))
           (new (read-string (format "%s: " field) (if (stringp cur) cur ""))))
      (arxana-sales--set-field! (format ":card/id %s" id) field
                                (arxana-sales--edn-string new))
      (message "%s %s updated" (arxana-sales--sym-str id) field)
      (arxana-sales-refresh))))

(defun arxana-sales-set-stage-at-point ()
  "Set the pipeline stage of the client at point."
  (interactive)
  (let ((id (arxana-sales--prop-at-point 'arxana-sales-client)))
    (unless id (user-error "No client at point"))
    (let ((stage (completing-read (format "Stage for %s: " (arxana-sales--sym-str id))
                                  (mapcar (lambda (s) (concat ":" (symbol-name (nth 1 s))))
                                          arxana-sales--stages)
                                  nil t)))
      (arxana-sales--set-field! (format ":client/id %s" id) ":client/stage" stage)
      (message "%s → %s" (arxana-sales--sym-str id) stage)
      (arxana-sales-refresh))))

(defun arxana-sales--insert-after-vector! (vec-key text)
  "Insert TEXT just inside the EDN vector named VEC-KEY (e.g. \":clients\")."
  (with-temp-buffer
    (insert-file-contents arxana-sales-file)
    (goto-char (point-min))
    (unless (re-search-forward (concat (regexp-quote vec-key) "[ \t\n]*\\[") nil t)
      (error "Sales: vector %s not found" vec-key))
    (insert text)
    (write-region (point-min) (point-max) arxana-sales-file nil 'quiet)))

(defun arxana-sales-add-card ()
  "Add a blank Rolodex card (prompts id + name); appends to :rolodex."
  (interactive)
  (let* ((id (read-string "New card id (kebab, no colon): "))
         (name (read-string "Name: ")))
    (when (string-empty-p id) (user-error "Need an id"))
    (arxana-sales--insert-after-vector! ":rolodex"
      (format "\n  {:card/id :%s :card/client nil\n   :card/name %s :card/org \"\" :card/role \"\"\n   :card/email \"\" :card/phone \"\" :card/links []\n   :card/first-contact \"\" :card/referred-by \"\"\n   :card/how-we-met \"\" :card/interest-territories [] :card/notes \"\"}\n"
              id (arxana-sales--edn-string name)))
    (message "Added card :%s" id)
    (arxana-sales--render-rolodex)))

;; --- Rolodex → Prospect: promote a card / a referral edge into the pipeline -

(defun arxana-sales--client-id-exists-p (data id)
  "Non-nil if DATA already has a client with :client/id ID (a keyword)."
  (cl-some (lambda (c) (eq (plist-get c :client/id) id)) (arxana-sales--clients data)))

(defun arxana-sales-promote-card (card)
  "Promote CARD into the pipeline as a Prospect: tick :card/in-pipeline?, link
:card/client, and append a :prospect client entry if none exists."
  (let* ((id (plist-get card :card/id))
         (idstr (arxana-sales--sym-str id))
         (name (or (plist-get card :card/name) idstr)))
    (unless id (user-error "Card has no id"))
    (when (y-or-n-p (format "Promote %s into the pipeline as a Prospect? " name))
      (unless (arxana-sales--client-id-exists-p (arxana-sales--read) id)
        (arxana-sales--insert-after-vector! ":clients"
          (format "\n  {:client/id :%s :client/name %s :client/stage :prospect :client/engagement :tbd}\n"
                  idstr (arxana-sales--edn-string name))))
      (arxana-sales--set-field! (format ":card/id :%s" idstr) ":card/in-pipeline?" "true")
      (arxana-sales--set-field! (format ":card/id :%s" idstr) ":card/client" (format ":%s" idstr))
      (message "Promoted %s → Prospect" name)
      (arxana-sales--render-leads))))

(defun arxana-sales-promote-card-at-point ()
  "Promote the Rolodex card at point into the pipeline as a Prospect."
  (interactive)
  (let ((card (arxana-sales--prop-at-point 'arxana-sales-card)))
    (unless card (user-error "No Rolodex card at point"))
    (arxana-sales-promote-card card)))

(defun arxana-sales-promote-edge (connector-name edge)
  "Promote a referral EDGE (a `:card/referrals' entry) emitted by CONNECTOR-NAME
into the pipeline: create a Prospect card (referred-by the connector) + a
:prospect client.  Prompts for the new contact's name + id."
  (let* ((to (or (plist-get edge :to) ""))
         (seed (if (string-prefix-p "(" to) "" to))
         (name (read-string "Prospect name: " seed))
         (id (read-string "Prospect id (kebab, no colon): ")))
    (when (or (string-empty-p name) (string-empty-p id))
      (user-error "Need a name and an id"))
    (arxana-sales--insert-after-vector! ":rolodex"
      (format "\n  {:card/id :%s :card/client :%s :card/kind :prospect :card/in-pipeline? true\n   :card/name %s :card/org \"\" :card/role \"\"\n   :card/email \"\" :card/phone \"\" :card/links []\n   :card/first-contact \"\" :card/referred-by %s\n   :card/how-we-met \"\" :card/relationship \"\" :card/friendly-interest \"\"\n   :card/interest-territories [] :card/notes \"\"}\n"
              id id (arxana-sales--edn-string name) (arxana-sales--edn-string connector-name)))
    (arxana-sales--insert-after-vector! ":clients"
      (format "\n  {:client/id :%s :client/name %s :client/stage :prospect :client/engagement :tbd}\n"
              id (arxana-sales--edn-string name)))
    (message "Promoted referral via %s → Prospect: %s" connector-name name)
    (arxana-sales--render-leads)))

(defvar arxana-sales-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "g" #'arxana-sales-refresh)
    (define-key m "e" #'arxana-sales-edit-card-field)
    (define-key m "s" #'arxana-sales-set-stage-at-point)
    (define-key m "+" #'arxana-sales-add-card)
    (define-key m "p" #'arxana-sales-promote-card-at-point)
    (define-key m "L" #'arxana-sales--render-leads)
    (define-key m "D" #'arxana-sales--render-demos)
    (define-key m "E" #'arxana-sales-edit-file)
    m)
  "Keymap for `arxana-sales-mode'.")

;;;###autoload
(defun arxana-sales-browse ()
  "Open the Arxana Sales pipeline with stage entry points + Rolodex."
  (interactive)
  (let* ((data (arxana-sales--read))
         (clients (arxana-sales--clients data)))
    (arxana-sales--render-frame
     (lambda ()
       (if (null data)
           (insert (format "Arxana Sales — cannot read %s\n" arxana-sales-file))
         (insert "Arxana Sales — pipeline (dual of the Ledger)\n")
         (insert (format "Source: %s\n\n" arxana-sales-file))
         (dolist (spec arxana-sales--stages)
           (let ((sub (arxana-sales--clients-at clients (nth 0 spec))))
             (arxana-sales--button
              (format "▸ %s" (nth 2 spec))
              (let ((s (nth 0 spec))) (lambda (_) (arxana-sales--render-stage s)))
              (nth 3 spec))
             (insert (format "   — %d client(s)  (%s)\n" (length sub) (nth 3 spec)))))
         (insert "\n")
         (arxana-sales--button "▸ Rolodex"
                               (lambda (_) (arxana-sales--render-rolodex))
                               "All contact cards (clients + pre-clients)")
         (insert (format "   — %d card(s)\n" (length (arxana-sales--rolodex data))))
         (arxana-sales--button "▸ Leads  (Rolodex → Prospect)"
                               (lambda (_) (arxana-sales--render-leads))
                               "Referral edges + un-promoted contacts → pipeline prospects")
         (insert "   — the lead-gen gradient\n")
         (arxana-sales--button "▸ Demos  (scan → demo → call)"
                               (lambda (_) (arxana-sales--render-demos))
                               "Dual-warranted demos; depositing ratio")
         (insert (format "   — %d indexed\n\n" (length (arxana-sales--demos data))))
         (arxana-sales--button "[edit sales.edn]" (lambda (_) (arxana-sales-edit-file))
                               "Open the sales EDN to edit; g to refresh after save")
         (insert "   — card: e=edit field +=add p=promote  ·  client: s=set stage  ·  L=leads  g=refresh  E=raw EDN\n")))
     #'arxana-sales-browse)))

;; ---------------------------------------------------- home integration ----

(defun arxana-sales--home-items ()
  "Return the stage entries + a Rolodex entry for the browser home `sales' view."
  (let* ((data (arxana-sales--read))
         (clients (arxana-sales--clients data)))
    (append
     (mapcar
      (lambda (spec)
        (let ((sub (arxana-sales--clients-at clients (nth 0 spec))))
          (list :type 'sales-stage
                :label (nth 2 spec)
                :stage (nth 0 spec)
                :description (format "%d client(s) — %s" (length sub) (nth 3 spec)))))
      arxana-sales--stages)
     (list (list :type 'sales-rolodex
                 :label "Rolodex"
                 :description (format "%d contact card(s)" (length (arxana-sales--rolodex data))))
           (list :type 'sales-leads
                 :label "Leads (Rolodex → Prospect)"
                 :description "Referral edges + un-promoted contacts → pipeline prospects")
           (list :type 'sales-demos
                 :label "Demos (scan → demo → call)"
                 :description (format "%d dual-warranted demos; depositing ratio"
                                      (length (arxana-sales--demos data))))))))

(defun arxana-sales-open-stage (item)
  "Open the stage / Rolodex / Leads / Demos named by ITEM (a `sales-*' home item)."
  (pcase (plist-get item :type)
    ('sales-rolodex (arxana-sales--render-rolodex))
    ('sales-leads   (arxana-sales--render-leads))
    ('sales-demos   (arxana-sales--render-demos))
    (_              (arxana-sales--render-stage (plist-get item :stage)))))

(provide 'arxana-vsatarcs-sales)
;;; arxana-vsatarcs-sales.el ends here
