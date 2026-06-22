;;; arxana-vsatarcs-ledger.el --- Arxana Ledger view (Historical/Current/Future) -*- lexical-binding: t; -*-

;;; Commentary:
;; The Arxana Ledger: a navigable view over `~/code/ledger/ledger.edn' with
;; three entry points — Historical (billed+paid), Current (unbilled drafts =
;; the basis of the next invoice), and Future (antedated speculations).  Each
;; item renders as a frame.
;;
;; Reads the canonical EDN live on every call (no cache); the ledger is the
;; source of truth and this view is a downstream projection — same discipline
;; as `arxana-vsatarcs-sorrys.el'.  Reuses the in-tree EDN reader from
;; `arxana-browser-rewrites' (it discards `#inst' tags → ISO string, and
;; interns namespaced keywords like `:item/id'), so no parseedn dependency.
;; A real `#inst' → Emacs-time adapter can be added later if sorting/arith on
;; dates is wanted; v0 keeps the ISO string (lexical order = chronological).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'arxana-browser-rewrites) ; shared EDN reader

(defgroup arxana-vsatarcs-ledger nil
  "Arxana Ledger view over the billable-hours ledger."
  :group 'arxana-vsatarcs)

(defcustom arxana-ledger-file (expand-file-name "~/code/ledger/ledger.edn")
  "Path to the canonical billable-hours ledger.  Read live on every call."
  :type 'file
  :group 'arxana-vsatarcs-ledger)

(defvar arxana-ledger--buffer "*Arxana Ledger*")

(defconst arxana-ledger--strata
  '((:current       :unbilled-draft "Current"       "drafted, not yet invoiced (this billing cycle)")
    (:invoice-ready :invoice-ready  "Invoice Ready" "staged for the next invoice; draft .docx generated")
    (:invoiced      :invoiced       "Invoiced"      "on an issued invoice, awaiting payment")
    (:historical    :billed-paid    "Historical"    "billed + paid (by invoice)")
    (:archived      :archived       "Archived"      "older, set aside")
    (:future        :speculative    "Future"        "antedated speculations"))
  "Entry-point → (status title blurb), in time-linear pipeline order so items
are seen moving through the system: Current (work done) → Invoice Ready
(staged + draft .docx) → Invoiced (issued, awaiting pay) → Historical (paid,
by invoice) → Archived (set aside); Future holds forward speculations.
Echoes the media EP flow (staging → released EPs).")

(defconst arxana-ledger--status-choices
  '(":unbilled-draft" ":invoice-ready" ":invoiced" ":billed-paid" ":speculative" ":archived")
  "EDN status literals accepted by manual ledger status editing.")

;; ---------------------------------------------------------------- data ----

(defun arxana-ledger--read ()
  "Read the ledger EDN; return its plist, or nil if unreadable."
  (when (file-readable-p arxana-ledger-file)
    (ignore-errors (arxana-browser-rewrites--read-edn-file arxana-ledger-file))))

(defun arxana-ledger--items (data)
  "Ledger items of DATA as a list (`:ledger' parses to a vector)."
  (append (plist-get data :ledger) nil))

(defun arxana-ledger--by-status (items status)
  (cl-remove-if-not (lambda (it) (eq (plist-get it :item/status) status)) items))

(defun arxana-ledger--amount (it)
  "Billable £ for IT: explicit amount, else prior-predictive, else (hours|hint)×rate."
  (or (plist-get it :item/amount-gbp)
      (let ((pp (plist-get it :item/prior-predictive)))
        (and pp (plist-get pp :amount-gbp)))
      (let ((h (or (plist-get it :item/hours) (plist-get it :item/hours-hint)))
            (r (or (plist-get it :item/rate-gbp) 75.0)))
        (and h (* h r)))))

(defun arxana-ledger--sum (items key)
  (apply #'+ 0.0 (delq nil (mapcar key items))))

(defun arxana-ledger--date-str (d)
  "Show D (ISO string from `#inst', or nil) as YYYY-MM-DD."
  (cond ((null d) "—")
        ((and (stringp d) (>= (length d) 10)) (substring d 0 10))
        (t (format "%s" d))))

(defun arxana-ledger--truncate (s n)
  (if (and (stringp s) (> (length s) n)) (concat (substring s 0 n) "…") s))

;; ------------------------------------------------------------- render ----

(defun arxana-ledger--button (label action &optional help)
  (insert-text-button label 'follow-link t 'help-echo help 'action action))

(defun arxana-ledger-edit-file ()
  "Open the canonical ledger EDN for editing.
After saving, press `g' to re-read."
  (interactive)
  (find-file arxana-ledger-file))

;; ----------------------------------------------- native edit / move ----
;; Surgical write-back: find the item's map by :item/id and replace only the
;; targeted field's value via sexp navigation, leaving all comments and
;; formatting intact (no full re-serialisation). The WM Clojure side reads
;; the same file unchanged.

(defun arxana-ledger--set-field! (id key value-str)
  "In `arxana-ledger-file', set KEY (e.g. \":item/hours\") to literal
VALUE-STR for the item whose :item/id is ID.  Targeted edit; preserves
comments.  Signals an error if the item is not found."
  (with-temp-buffer
    (insert-file-contents arxana-ledger-file)
    ;; treat EDN brackets as sexp delimiters so forward-sexp works
    (modify-syntax-entry ?\{ "(}")
    (modify-syntax-entry ?\} "){")
    (modify-syntax-entry ?\[ "(]")
    (modify-syntax-entry ?\] ")[")
    (goto-char (point-min))
    (unless (search-forward (format ":item/id \"%s\"" id) nil t)
      (error "Ledger: item %s not found" id))
    (let* ((idpt (point))
           (mapbeg (save-excursion (goto-char idpt) (backward-up-list) (point)))
           (mapend (save-excursion (goto-char mapbeg) (forward-sexp) (point))))
      (save-restriction
        (narrow-to-region mapbeg mapend)
        (goto-char (point-min))
        (if (re-search-forward (concat (regexp-quote key) "[ \t\n]+") nil t)
            (let ((vbeg (point)))            ; replace the existing value sexp
              (forward-sexp)
              (delete-region vbeg (point))
              (goto-char vbeg)
              (insert value-str))
          (goto-char (point-min))            ; key absent → insert after :item/id
          (search-forward (format ":item/id \"%s\"" id))
          (insert (format "\n   %s %s" key value-str)))))
    (write-region (point-min) (point-max) arxana-ledger-file nil 'quiet)))

(defun arxana-ledger--item-at-point ()
  "Return the ledger item plist tagged at point (or just before)."
  (or (get-text-property (point) 'arxana-ledger-item)
      (and (> (point) (point-min))
           (get-text-property (1- (point)) 'arxana-ledger-item))))

(defun arxana-ledger-refresh ()
  "Re-read the ledger and re-render the current view."
  (interactive)
  (if arxana-ledger--refresh-fn (funcall arxana-ledger--refresh-fn)
    (arxana-ledger-browse)))

(defun arxana-ledger-set-hours-at-point ()
  "Set hours on the item at point; recompute amount = hours × rate."
  (interactive)
  (let* ((it (arxana-ledger--item-at-point))
         (id (plist-get it :item/id)))
    (unless id (user-error "No ledger item at point"))
    (let* ((rate (or (plist-get it :item/rate-gbp) 75.0))
           (h (read-number (format "Hours for %s (rate £%s): " id rate)
                           (or (plist-get it :item/hours) 0))))
      (arxana-ledger--set-field! id ":item/hours" (number-to-string h))
      (arxana-ledger--set-field! id ":item/amount-gbp" (number-to-string (* h rate)))
      (message "%s: %sh = £%s" id h (* h rate))
      (arxana-ledger-refresh))))

(defun arxana-ledger--edn-string (s)
  "Render S as an EDN string literal (escaping \\ and \")."
  (concat "\"" (replace-regexp-in-string "\\([\"\\\\]\\)" "\\\\\\1" s) "\""))

(defun arxana-ledger-set-description-at-point ()
  "Edit the description of the item at point (in-place, comment-preserving)."
  (interactive)
  (let* ((it (arxana-ledger--item-at-point))
         (id (plist-get it :item/id)))
    (unless id (user-error "No ledger item at point"))
    (let ((new (read-string (format "Description for %s: " id)
                            (or (plist-get it :item/description) ""))))
      (arxana-ledger--set-field! id ":item/description" (arxana-ledger--edn-string new))
      (message "%s description updated" id)
      (arxana-ledger-refresh))))

(defun arxana-ledger-set-status-at-point ()
  "Set the status (move between strata) of the item at point."
  (interactive)
  (let* ((it (arxana-ledger--item-at-point))
         (id (plist-get it :item/id)))
    (unless id (user-error "No ledger item at point"))
    (let ((status (completing-read
                   (format "Status for %s: " id)
                   arxana-ledger--status-choices
                   nil t)))
      (arxana-ledger--set-field! id ":item/status" status)
      (message "%s → %s" id status)
      (arxana-ledger-refresh))))

(defun arxana-ledger-archive-at-point ()
  "Move the item at point to the Archived stratum."
  (interactive)
  (let* ((it (arxana-ledger--item-at-point))
         (id (plist-get it :item/id)))
    (unless id (user-error "No ledger item at point"))
    (when (y-or-n-p (format "Archive %s? " id))
      (arxana-ledger--set-field! id ":item/status" ":archived")
      (message "%s archived" id)
      (arxana-ledger-refresh))))

(defun arxana-ledger--generate-draft-invoice (inv-id)
  "Generate a standard draft invoice .docx for INV-ID.
Delegates to ~/code/ledger/print_invoice.bb, which uses the configured
reference .docx template when present."
  (let ((script (expand-file-name "~/code/ledger/print_invoice.bb")))
    (with-temp-buffer
      (let ((status (call-process "bb" nil t nil script inv-id))
            (out (buffer-string)))
        (unless (zerop status)
          (user-error "Invoice generation failed (%s): %s" status (string-trim out)))
        (message "Print Invoice: %s" (string-trim out))
        out))))

(defun arxana-ledger--current-invoice-id-default ()
  "Infer the next invoice id from Current item ids, falling back to 202504."
  (let* ((current (arxana-ledger--by-status
                   (arxana-ledger--items (arxana-ledger--read)) :unbilled-draft))
         (ids (delq nil
                    (mapcar (lambda (it)
                              (when-let ((id (plist-get it :item/id)))
                                (and (string-match "\\`\\([0-9]+\\)-" id)
                                     (match-string 1 id))))
                            current))))
    (or (car ids) "202504")))

(defun arxana-ledger-ready-current-invoice (inv-id)
  "Move all Current items into Invoice Ready under INV-ID and generate the docx.
This is the main `r' lifecycle transition. It intentionally stages all Current
items as one invoice for now."
  (interactive (list (read-string "Ready Current as invoice id: "
                                  (arxana-ledger--current-invoice-id-default))))
  (let ((current (arxana-ledger--by-status
                  (arxana-ledger--items (arxana-ledger--read)) :unbilled-draft)))
    (unless current
      (user-error "No Current item(s) to ready"))
    (arxana-ledger--generate-draft-invoice inv-id)
    (dolist (it current)
      (let ((id (plist-get it :item/id)))
        (arxana-ledger--set-field! id ":item/invoice" (format "\"%s\"" inv-id))
        (arxana-ledger--set-field! id ":item/status" ":invoice-ready")
        (arxana-ledger--set-field! id ":item/invoiced?" "false")
        (arxana-ledger--set-field! id ":item/paid?" "false")))
    (message "Readied %d Current item(s) into Invoice Ready (invoice %s)"
             (length current) inv-id)
    (arxana-ledger-refresh)))

(defun arxana-ledger-print-invoice (inv-id)
  "Generate a draft invoice .docx for INV-ID, then optionally ready Current.
Prefer `arxana-ledger-ready-current-invoice' (`r') for the normal lifecycle
transition."
  (interactive (list (read-string "Print invoice id: "
                                  (arxana-ledger--current-invoice-id-default))))
  (arxana-ledger--generate-draft-invoice inv-id)
  (let ((current (arxana-ledger--by-status
                  (arxana-ledger--items (arxana-ledger--read)) :unbilled-draft)))
    (when (and current
               (y-or-n-p (format "Move %d Current item(s) → Invoice Ready (invoice %s)? "
                                 (length current) inv-id)))
      (dolist (it current)
        (let ((id (plist-get it :item/id)))
          (arxana-ledger--set-field! id ":item/invoice" (format "\"%s\"" inv-id))
          (arxana-ledger--set-field! id ":item/status" ":invoice-ready")))
      (message "Staged %d item(s) into Invoice Ready (invoice %s)" (length current) inv-id)))
  (arxana-ledger-refresh))

(defun arxana-ledger--read-invoice-id-for-status (status prompt)
  "Read an invoice id from ledger items currently in STATUS."
  (let* ((items (arxana-ledger--items (arxana-ledger--read)))
         (ids (cl-remove "unassigned"
                         (arxana-ledger--invoice-ids-of items status)
                         :test #'equal))
         (point-invoice (plist-get (arxana-ledger--item-at-point) :item/invoice))
         (default (or (and point-invoice (member point-invoice ids) point-invoice)
                      (car ids))))
    (unless ids
      (user-error "No invoice(s) in %s" status))
    (completing-read prompt ids nil t nil nil default)))

(defun arxana-ledger--invoice-id-default (&optional status)
  "Return a useful invoice id default.
Prefer the item at point, then invoices in STATUS, then the next known draft id."
  (or (plist-get (arxana-ledger--item-at-point) :item/invoice)
      (car (arxana-ledger--invoice-ids-of
            (arxana-ledger--items (arxana-ledger--read))
            (or status :invoice-ready)))
      "202504"))

(defun arxana-ledger-open-draft-invoice (inv-id)
  "Open the draft invoice .docx for INV-ID.
Use `arxana-ledger-print-invoice' first if no draft exists yet."
  (interactive (list (read-string "Open draft invoice id: "
                                  (arxana-ledger--invoice-id-default))))
  (arxana-ledger-open-docx (arxana-ledger--draft-docx-path inv-id)))

(defun arxana-ledger-mark-invoice-sent (inv-id)
  "Record that INV-ID was sent/issued.
This moves matching `:invoice-ready' items to `:invoiced' and sets
`:item/invoiced?' true. It does not send email; it records the external
send once the operator has actually sent the draft invoice."
  (interactive (list (arxana-ledger--read-invoice-id-for-status
                      :invoice-ready "Record Invoice Ready as sent: ")))
  (let* ((items (arxana-ledger--items (arxana-ledger--read)))
         (ready (cl-remove-if-not
                 (lambda (it)
                   (and (eq (plist-get it :item/status) :invoice-ready)
                        (equal inv-id (plist-get it :item/invoice))))
                 items)))
    (unless ready
      (user-error "No Invoice Ready item(s) for invoice %s" inv-id))
    (when (y-or-n-p (format "Record invoice %s as sent/issued for %d item(s)? "
                            inv-id (length ready)))
      (dolist (it ready)
        (let ((id (plist-get it :item/id)))
          (arxana-ledger--set-field! id ":item/status" ":invoiced")
          (arxana-ledger--set-field! id ":item/invoiced?" "true")
          (arxana-ledger--set-field! id ":item/paid?" "false")))
      (message "Invoice %s recorded as sent/issued (%d item(s))"
               inv-id (length ready))
      (arxana-ledger-refresh))))

(defun arxana-ledger-mark-invoice-historical (inv-id)
  "Record that INV-ID was paid and move it from Invoiced to Historical."
  (interactive (list (arxana-ledger--read-invoice-id-for-status
                      :invoiced "Move Invoiced to Historical: ")))
  (let* ((items (arxana-ledger--items (arxana-ledger--read)))
         (invoiced (cl-remove-if-not
                    (lambda (it)
                      (and (eq (plist-get it :item/status) :invoiced)
                           (equal inv-id (plist-get it :item/invoice))))
                    items)))
    (unless invoiced
      (user-error "No Invoiced item(s) for invoice %s" inv-id))
    (when (y-or-n-p (format "Record invoice %s as paid/Historical for %d item(s)? "
                            inv-id (length invoiced)))
      (dolist (it invoiced)
        (let ((id (plist-get it :item/id)))
          (arxana-ledger--set-field! id ":item/status" ":billed-paid")
          (arxana-ledger--set-field! id ":item/invoiced?" "true")
          (arxana-ledger--set-field! id ":item/paid?" "true")))
      (message "Invoice %s moved to Historical (%d item(s))"
               inv-id (length invoiced))
      (arxana-ledger-refresh))))

(defun arxana-ledger-show-bindings-help ()
  "Show ledger-specific bindings and lifecycle help."
  (interactive)
  (with-help-window "*Arxana Ledger Help*"
    (with-current-buffer standard-output
      (insert "Arxana Ledger\n")
      (insert "=============\n\n")
      (insert "Workflow\n")
      (insert "--------\n")
      (insert "Current -> r generates the standard draft .docx and moves all Current items to Invoice Ready.\n")
      (insert "Invoice Ready -> i records the chosen invoice as sent/issued and moves it to Invoiced.\n")
      (insert "Invoiced -> h records the chosen invoice as paid and moves it to Historical.\n\n")
      (insert "Bindings\n")
      (insert "--------\n")
      (insert " r  ready all Current items as one generated invoice\n")
      (insert " i  record selected Invoice Ready invoice as sent/issued\n")
      (insert " h  record selected Invoiced invoice as paid/Historical\n")
      (insert " g  refresh\n")
      (insert " e  edit hours at point\n")
      (insert " d  edit description at point\n")
      (insert " s  set status at point\n")
      (insert " a  archive item at point\n")
      (insert " P  print/regenerate draft invoice; optionally stage Current -> Invoice Ready\n")
      (insert " O  open draft invoice .docx\n")
      (insert " E  open raw ledger.edn\n")
      (insert " ?  this help\n"))))

(defun arxana-ledger--ensure-help-hydra ()
  "Define or refresh the ledger help Hydra when Hydra is installed."
  (when (or (fboundp 'defhydra)
            (require 'hydra nil t))
    ;; Reloading this file in a live Emacs should update the menu immediately.
    (when (fboundp 'arxana-ledger-help-hydra/body)
      (fmakunbound 'arxana-ledger-help-hydra/body))
    (eval
     '(defhydra arxana-ledger-help-hydra (:hint nil :foreign-keys run)
        "
Arxana Ledger
  _r_: ready Current -> Invoice Ready         _i_: sent/issued -> Invoiced
  _h_: paid -> Historical                     _O_: open draft .docx
  _P_: print/regenerate                       _g_: refresh
  _e_: hours   _d_: description   _s_: status   _a_: archive
  _E_: raw EDN  _?_: full help
  _q_: quit
"
        ("r" arxana-ledger-ready-current-invoice nil :exit t)
        ("i" arxana-ledger-mark-invoice-sent nil :exit t)
        ("h" arxana-ledger-mark-invoice-historical nil :exit t)
        ("P" arxana-ledger-print-invoice nil :exit t)
        ("O" arxana-ledger-open-draft-invoice nil :exit t)
        ("g" arxana-ledger-refresh nil)
        ("e" arxana-ledger-set-hours-at-point nil :exit t)
        ("d" arxana-ledger-set-description-at-point nil :exit t)
        ("s" arxana-ledger-set-status-at-point nil :exit t)
        ("a" arxana-ledger-archive-at-point nil :exit t)
        ("E" arxana-ledger-edit-file nil :exit t)
        ("?" arxana-ledger-show-bindings-help "full help" :exit t)
        ("q" nil "quit" :exit t))))
  (fboundp 'arxana-ledger-help-hydra/body))

(defun arxana-ledger-help ()
  "Open ledger bindings help, preferring a ledger-specific Hydra."
  (interactive)
  (if (arxana-ledger--ensure-help-hydra)
      (arxana-ledger-help-hydra/body)
    (arxana-ledger-show-bindings-help)))

(defvar arxana-ledger-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "g" #'arxana-ledger-refresh)
    (define-key m "e" #'arxana-ledger-set-hours-at-point)
    (define-key m "d" #'arxana-ledger-set-description-at-point)
    (define-key m "s" #'arxana-ledger-set-status-at-point)
    (define-key m "a" #'arxana-ledger-archive-at-point)
    (define-key m "r" #'arxana-ledger-ready-current-invoice)
    (define-key m "i" #'arxana-ledger-mark-invoice-sent)
    (define-key m "h" #'arxana-ledger-mark-invoice-historical)
    (define-key m "P" #'arxana-ledger-print-invoice)
    (define-key m "O" #'arxana-ledger-open-draft-invoice)
    (define-key m "I" #'arxana-ledger-mark-invoice-sent)
    (define-key m "E" #'arxana-ledger-edit-file)
    (define-key m "?" #'arxana-ledger-help)
    m)
  "Keymap for `arxana-ledger-mode'.")

;; Reassert key bindings on reload; `defvar' preserves an existing keymap.
(define-key arxana-ledger-mode-map "r" #'arxana-ledger-ready-current-invoice)
(define-key arxana-ledger-mode-map "i" #'arxana-ledger-mark-invoice-sent)
(define-key arxana-ledger-mode-map "h" #'arxana-ledger-mark-invoice-historical)
(define-key arxana-ledger-mode-map "P" #'arxana-ledger-print-invoice)
(define-key arxana-ledger-mode-map "O" #'arxana-ledger-open-draft-invoice)
(define-key arxana-ledger-mode-map "I" #'arxana-ledger-mark-invoice-sent)
(define-key arxana-ledger-mode-map "?" #'arxana-ledger-help)

(define-derived-mode arxana-ledger-mode special-mode "Arxana-Ledger"
  "Major mode for the Arxana Ledger view.
\\{arxana-ledger-mode-map}")

(defvar-local arxana-ledger--refresh-fn nil
  "Thunk re-rendering the current Arxana Ledger view (bound to `g').")

(defun arxana-ledger--render-frame (body-fn &optional refresh-fn)
  (let ((buf (get-buffer-create arxana-ledger--buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'arxana-ledger-mode) (arxana-ledger-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall body-fn)
        (goto-char (point-min)))
      (when refresh-fn (setq arxana-ledger--refresh-fn refresh-fn)))
    (pop-to-buffer buf)))

(defun arxana-ledger--insert-item (it)
  "Insert one ledger item as a frame (tagged with its id/plist for editing)."
  (let* ((beg (point))
         (id (plist-get it :item/id))
         (date (plist-get it :item/date))
         (hours (plist-get it :item/hours))
         (hint (plist-get it :item/hours-hint))
         (amt (arxana-ledger--amount it))
         (inv (plist-get it :item/invoice))
         (client (plist-get it :item/client))
         (desc (plist-get it :item/description)))
    (insert (format "  • %s  [%s]" (or id "?") (arxana-ledger--date-str date)))
    (cond
     (hours (insert (format "  %sh = £%s" hours (or amt "?"))))
     (hint  (insert (format "  ~%sh hint ≈ £%s" hint (or amt "?"))))
     (amt   (insert (format "  £%s" amt))))
    (when inv (insert (format "  (inv %s)" inv)))
    (insert "\n")
    (when client (insert (format "      client: %s\n" client)))
    (when (eq (plist-get it :item/status) :speculative)
      (let ((pp (plist-get it :item/prior-predictive))
            (conds (append (plist-get it :item/conditions) nil)))
        (when pp
          (insert (format "      prior-predictive: £%s by %s (p=%s)\n"
                          (plist-get pp :amount-gbp)
                          (arxana-ledger--date-str (plist-get pp :by))
                          (plist-get pp :p))))
        (dolist (c conds) (insert (format "      ⟶ needs: %s\n" c)))))
    (when desc (insert (format "      %s\n" (arxana-ledger--truncate desc 160))))
    (insert "\n")
    (add-text-properties beg (point)
                         (list 'arxana-ledger-item it 'arxana-ledger-item-id id))))

(defun arxana-ledger--invoices (items)
  "Distinct invoice ids among the billed+paid ITEMS, in first-seen order."
  (let (order)
    (dolist (it (arxana-ledger--by-status items :billed-paid))
      (let ((inv (plist-get it :item/invoice)))
        (when (and inv (not (member inv order))) (push inv order))))
    (nreverse order)))

(defun arxana-ledger--invoice-items (items inv)
  "Billed+paid ITEMS belonging to invoice INV."
  (cl-remove-if-not (lambda (it) (equal inv (plist-get it :item/invoice)))
                    (arxana-ledger--by-status items :billed-paid)))

(defun arxana-ledger--draft-docx-path (inv-id)
  "Path of the draft .docx print_invoice.bb writes for INV-ID."
  (expand-file-name
   (format "~/code/invoices/VSAT proof of concept project Invoice #%s (draft).docx" inv-id)))

(defun arxana-ledger-open-docx (path)
  "Open PATH in the external viewer (LibreOffice via xdg-open)."
  (if (file-exists-p path)
      (call-process "xdg-open" nil 0 nil path)
    (user-error "No draft .docx yet (press P to generate): %s" path)))

(defun arxana-ledger--invoice-ids-of (items status)
  "Distinct :item/invoice ids among ITEMS of STATUS (first-seen order)."
  (let (order)
    (dolist (it (arxana-ledger--by-status items status))
      (let ((inv (or (plist-get it :item/invoice) "unassigned")))
        (unless (member inv order) (push inv order))))
    (nreverse order)))

(defun arxana-ledger--render-invoice (inv)
  "Render the work-items for invoice INV (Historical subsection)."
  (let* ((items (arxana-ledger--items (arxana-ledger--read)))
         (sub (arxana-ledger--invoice-items items inv)))
    (arxana-ledger--render-frame
     (lambda ()
       (insert (format "Arxana Ledger — Historical — VSAT POC %s\n\n" inv))
       (arxana-ledger--button "[← Historical]" (lambda (_) (arxana-ledger--render-stratum :historical)))
       (insert "    ")
       (arxana-ledger--button "[← entry points]" (lambda (_) (arxana-ledger-browse)))
       (insert "\n\n")
       (dolist (it sub) (arxana-ledger--insert-item it))
       (insert (format "  ── %s: %d item(s); £%.2f ──\n"
                       inv (length sub) (arxana-ledger--sum sub #'arxana-ledger--amount))))
     (lambda () (arxana-ledger--render-invoice inv)))))

(defun arxana-ledger--render-stratum (stratum)
  "Render STRATUM.  `:historical' groups by invoice (subsections); the others
are flat item lists."
  (let* ((spec (assq stratum arxana-ledger--strata))
         (status (nth 1 spec))
         (items (arxana-ledger--items (arxana-ledger--read))))
    (cond
     ((eq stratum :historical)
      (arxana-ledger--render-frame
       (lambda ()
         (insert "Arxana Ledger — Historical (by invoice)\n\n")
         (arxana-ledger--button "[← entry points]" (lambda (_) (arxana-ledger-browse)))
         (insert "\n\n")
         (dolist (inv (arxana-ledger--invoices items))
           (let* ((sub (arxana-ledger--invoice-items items inv))
                  (tot (arxana-ledger--sum sub #'arxana-ledger--amount))
                  (paid? (cl-some (lambda (it) (plist-get it :item/paid?)) sub)))
             (arxana-ledger--button
              (format "▸ VSAT POC %s" inv)
              (let ((i inv)) (lambda (_) (arxana-ledger--render-invoice i)))
              (format "Open invoice %s" inv))
             (insert (format "   — %d item(s), £%.2f%s\n"
                             (length sub) tot (if paid? " · paid" "")))))
         (insert "\n"))
       (lambda () (arxana-ledger--render-stratum :historical))))
     ((eq stratum :invoice-ready)
      (arxana-ledger--render-frame
       (lambda ()
         (insert "Arxana Ledger — Invoice Ready\n\n")
         (arxana-ledger--button "[← entry points]" (lambda (_) (arxana-ledger-browse)))
         (insert "\n\n")
         (let ((ready (arxana-ledger--by-status items :invoice-ready)))
           (if (null ready)
               (insert "  (none — stage Current items here with P: print invoice)\n")
             (dolist (inv (arxana-ledger--invoice-ids-of items :invoice-ready))
               (let* ((sub (cl-remove-if-not
                            (lambda (it) (equal inv (or (plist-get it :item/invoice) "unassigned")))
                            ready))
                      (tot (arxana-ledger--sum sub #'arxana-ledger--amount))
                      (docx (arxana-ledger--draft-docx-path inv)))
                 (insert (format "Invoice #%s — %d item(s), £%.2f\n" inv (length sub) tot))
                 (arxana-ledger--button
                  (if (file-exists-p docx) "[open draft .docx ↗]"
                    "[draft .docx not generated — press P]")
                  (let ((p docx)) (lambda (_) (arxana-ledger-open-docx p)))
                  "Open the draft invoice in LibreOffice (external viewer)")
                 (insert "\n\n")
                 (dolist (it sub) (arxana-ledger--insert-item it))
                 (insert (format "  ── %s: £%.2f ──\n\n" inv tot)))))))
       (lambda () (arxana-ledger--render-stratum :invoice-ready))))
     (t
      (let ((sub (sort (arxana-ledger--by-status items status)
                       (lambda (a b) (string< (or (plist-get a :item/date) "9999")
                                              (or (plist-get b :item/date) "9999"))))))
        (arxana-ledger--render-frame
         (lambda ()
           (insert (format "Arxana Ledger — %s  (%s)\n\n" (nth 2 spec) (nth 3 spec)))
           (arxana-ledger--button "[← entry points]" (lambda (_) (arxana-ledger-browse)))
           (insert "\n\n")
           (if (null sub)
               (insert "  (none)\n")
             (dolist (it sub) (arxana-ledger--insert-item it))
             (insert (format "  ── %d item(s); £%.2f total ──\n"
                             (length sub)
                             (arxana-ledger--sum sub #'arxana-ledger--amount)))))
         (let ((s stratum)) (lambda () (arxana-ledger--render-stratum s)))))))))

;;;###autoload
(defun arxana-ledger-browse ()
  "Open the Arxana Ledger with Historical / Current / Future entry points."
  (interactive)
  (let* ((data (arxana-ledger--read))
         (items (arxana-ledger--items data)))
    (arxana-ledger--render-frame
     (lambda ()
       (if (null data)
           (insert (format "Arxana Ledger — cannot read %s\n" arxana-ledger-file))
         (insert "Arxana Ledger\n")
         (insert (format "Source: %s\n\n" arxana-ledger-file))
         (dolist (spec arxana-ledger--strata)
           (let* ((stratum (nth 0 spec))
                  (sub (arxana-ledger--by-status items (nth 1 spec))))
             (arxana-ledger--button
              (format "▸ %s" (nth 2 spec))
              (let ((s stratum)) (lambda (_) (arxana-ledger--render-stratum s)))
              (nth 3 spec))
             (insert (format "   — %d item(s), £%.2f  (%s)\n"
                             (length sub)
                             (arxana-ledger--sum sub #'arxana-ledger--amount)
                             (nth 3 spec)))))
         (insert "\n")
         (let ((paid (arxana-ledger--by-status items :billed-paid)))
           (insert (format "Paid to date: £%.2f / %sh across %d items.\n"
                           (arxana-ledger--sum paid #'arxana-ledger--amount)
                           (arxana-ledger--sum paid (lambda (it) (plist-get it :item/hours)))
                           (length paid))))
         (insert "\n")
         (arxana-ledger--button "[edit ledger.edn]"
                                (lambda (_) (arxana-ledger-edit-file))
                                "Open the ledger EDN to edit; g to refresh after save")
         (insert "   — lifecycle: r=ready i=sent h=historical  ·  item: e=hours d=desc s=status a=archive  ·  O=open P=print  ·  ?=help\n")))
   #'arxana-ledger-browse)))

;; ---------------------------------------------------- home integration ----
;; Wired into arxana-browser-core's home as the 'ledger view: three stratum
;; rows (rendered via the default menu-row), each opening its frame on RET.

(defun arxana-ledger--home-items ()
  "Return the three stratum entries for the browser home `ledger' view."
  (let ((items (arxana-ledger--items (arxana-ledger--read))))
    (mapcar
     (lambda (spec)
       (let* ((sub (arxana-ledger--by-status items (nth 1 spec)))
              (tot (arxana-ledger--sum sub #'arxana-ledger--amount)))
         (list :type 'ledger-stratum
               :label (nth 2 spec)
               :stratum (nth 0 spec)
               :description (format "%d item(s), £%.2f — %s"
                                    (length sub) tot (nth 3 spec)))))
     arxana-ledger--strata)))

(defun arxana-ledger-open-stratum (item)
  "Open the stratum named by ITEM (a `ledger-stratum' home item)."
  (arxana-ledger--render-stratum (plist-get item :stratum)))

;; Arxana Browser '?': the browser's ledger view lists STRATA (bulk), not
;; individual items, so the per-item edit keys (e/d/s/a) of the standalone
;; ledger hydra don't apply there — those operate on the item at point, which
;; only exists in the `*Arxana Ledger*' frames reached by RET.  Define a
;; bulk/invoice-only variant and route the ledger view to it; other views fall
;; through to the original `arxana-browser-help'.  Mirrors the essays module's
;; `arxana-browser-essays-compiled--browser-help-advice'.

(defun arxana-ledger--ensure-browser-help-hydra ()
  "Define the bulk/invoice-only ledger hydra for the Arxana Browser view."
  (when (or (fboundp 'defhydra) (require 'hydra nil t))
    (unless (fboundp 'arxana-ledger-browser-help-hydra/body)
      (eval
       '(defhydra arxana-ledger-browser-help-hydra (:hint nil :foreign-keys run)
          "
Arxana Ledger (bulk / invoice ops)
  _r_: ready Current -> Invoice Ready    _i_: issue -> Invoiced
  _h_: mark paid -> Historical           _P_: print/regenerate draft
  _O_: open draft .docx                  _g_: refresh
  _E_: raw EDN      (RET a stratum for per-item edits)
  _?_: full help    _q_: quit
"
          ("r" arxana-ledger-ready-current-invoice nil :exit t)
          ("i" arxana-ledger-mark-invoice-sent nil :exit t)
          ("h" arxana-ledger-mark-invoice-historical nil :exit t)
          ("P" arxana-ledger-print-invoice nil :exit t)
          ("O" arxana-ledger-open-draft-invoice nil :exit t)
          ("g" arxana-browser--refresh nil)
          ("E" arxana-ledger-edit-file nil :exit t)
          ("?" arxana-ledger-show-bindings-help "full help" :exit t)
          ("q" nil "quit" :exit t)))))
  (fboundp 'arxana-ledger-browser-help-hydra/body))

(defun arxana-ledger-browser-help ()
  "Show the bulk/invoice ledger hydra (for the Arxana Browser ledger view)."
  (interactive)
  (if (arxana-ledger--ensure-browser-help-hydra)
      (arxana-ledger-browser-help-hydra/body)
    (arxana-ledger-show-bindings-help)))

(defun arxana-ledger--browser-help-advice (orig-fn &rest args)
  "Dispatch `arxana-browser-help' to the bulk ledger hydra in the ledger view."
  (let ((view (and (boundp 'arxana-browser--context)
                   (plist-get arxana-browser--context :view))))
    (if (eq view 'ledger)
        (arxana-ledger-browser-help)
      (apply orig-fn args))))

(when (fboundp 'arxana-browser-help)
  (advice-add 'arxana-browser-help
              :around #'arxana-ledger--browser-help-advice))

(provide 'arxana-vsatarcs-ledger)
;;; arxana-vsatarcs-ledger.el ends here
