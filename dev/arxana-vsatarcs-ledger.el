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
  '((:current    :unbilled-draft "Current"    "drafted, not yet invoiced (this billing cycle)")
    (:invoiced   :invoiced       "Invoiced"   "on an issued invoice, awaiting payment")
    (:historical :billed-paid    "Historical" "billed + paid (by invoice)")
    (:archived   :archived       "Archived"   "older, set aside")
    (:future     :speculative    "Future"     "antedated speculations"))
  "Entry-point → (status title blurb), in time-linear pipeline order so items
are seen moving through the system: Current (work done) → Invoiced (issued,
awaiting pay) → Historical (paid, by invoice) → Archived (set aside); Future
holds forward speculations.  Echoes the media EP flow (staging → released EPs).")

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
                   '(":unbilled-draft" ":billed-paid" ":speculative" ":archived")
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

(defun arxana-ledger-print-invoice (inv-id)
  "Generate a draft invoice .docx for INV-ID (default 202504) via
~/code/ledger/print_invoice.bb (Markdown → pandoc --reference-doc=template).
Writes to ~/code/invoices/."
  (interactive (list (read-string "Print invoice id: " "202504")))
  (let ((out (shell-command-to-string
              (format "bb %s %s"
                      (shell-quote-argument (expand-file-name "~/code/ledger/print_invoice.bb"))
                      (shell-quote-argument inv-id)))))
    (message "Print Invoice: %s" (string-trim out))))

(defvar arxana-ledger-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "g" #'arxana-ledger-refresh)
    (define-key m "e" #'arxana-ledger-set-hours-at-point)
    (define-key m "d" #'arxana-ledger-set-description-at-point)
    (define-key m "s" #'arxana-ledger-set-status-at-point)
    (define-key m "a" #'arxana-ledger-archive-at-point)
    (define-key m "P" #'arxana-ledger-print-invoice)
    (define-key m "E" #'arxana-ledger-edit-file)
    m)
  "Keymap for `arxana-ledger-mode'.")

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
    (if (eq stratum :historical)
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
         (lambda () (arxana-ledger--render-stratum :historical)))
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
         (let ((s stratum)) (lambda () (arxana-ledger--render-stratum s))))))))

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
         (insert "   — item: e=hours d=desc s=status a=archive  ·  P=print invoice  ·  g=refresh  E=raw EDN\n")))
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

(provide 'arxana-vsatarcs-ledger)
;;; arxana-vsatarcs-ledger.el ends here
