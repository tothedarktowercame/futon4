;;; arxana-cold-outbox.el --- Cold-chain staged outbox view -*- lexical-binding: t; -*-

;;; Commentary:
;; Navigable porcelain over the cold-chain kit-outbox staging directories.
;; This mirrors `arxana-vsatarcs-sales.el': live EDN reads via
;; `arxana-browser-rewrites', `special-mode' rendering, text-button navigation,
;; surgical comment-preserving EDN edits, a keymap, and browser home entry
;; helpers.
;;
;; The load-bearing transition is OUTBOX -> SENT MAIL.  The SMTP path is
;; consent-gated, uses `smtpmail' with operator configuration, captures the real
;; Message-ID, and records that as :send-witness through `bb pudding-prover.bb
;; intake!'.  If SMTP is unconfigured, the command falls back to the previous
;; external-send witness prompt.  If intake rejects, the staged draft is not
;; flipped to :sent.

;;; Code:

(eval-and-compile
  (let ((dir (or (and load-file-name (file-name-directory load-file-name))
                 (and buffer-file-name (file-name-directory buffer-file-name))
                 default-directory)))
    (add-to-list 'load-path dir)))

(require 'cl-lib)
(require 'message)
(require 'smtpmail)
(require 'subr-x)
(require 'arxana-browser-rewrites)

(defgroup arxana-cold-outbox nil
  "Arxana cold-chain staged outbox review surface."
  :group 'arxana)

(defcustom arxana-cold-outbox-root
  "/home/joe/code/futon7/data/outbox/staged"
  "Root directory containing one staged outbox directory per draft.
Each child directory may contain `staged.edn', `draft.md', and `context.md'."
  :type 'directory
  :group 'arxana-cold-outbox)

(defcustom arxana-cold-outbox-pudding-prover
  "/home/joe/code/futon7/holes/pudding-prover.bb"
  "Babashka pudding-prover script used for kit-intake `intake!'."
  :type 'file
  :group 'arxana-cold-outbox)

(defcustom arxana-cold-outbox-from-address
  "joseph.corneli@hyperreal.enterprises"
  "From address used when sending staged EOIs through SMTP."
  :type 'string
  :group 'arxana-cold-outbox)

(defcustom arxana-cold-outbox-smtp-server
  ""
  "SMTP server for staged EOI sends.
When empty, `arxana-cold-outbox-send' falls back to external-send witness
recording and does not attempt SMTP."
  :type 'string
  :group 'arxana-cold-outbox)

(defcustom arxana-cold-outbox-smtp-service
  587
  "SMTP service/port for staged EOI sends."
  :type 'integer
  :group 'arxana-cold-outbox)

(defcustom arxana-cold-outbox-smtp-user
  "joseph.corneli@hyperreal.enterprises"
  "SMTP username for staged EOI sends.
Passwords are resolved by `smtpmail' and auth-source; this module never reads or
stores them."
  :type 'string
  :group 'arxana-cold-outbox)

(defcustom arxana-cold-outbox-smtp-stream-type
  'starttls
  "SMTP stream type for staged EOI sends, usually `starttls' or `ssl'."
  :type '(choice (const :tag "STARTTLS" starttls)
                 (const :tag "SSL" ssl)
                 (symbol :tag "Other"))
  :group 'arxana-cold-outbox)

(defvar arxana-cold-outbox--buffer "*Arxana Cold Outbox*")
(defvar arxana-cold-outbox--smtp-send-fn #'arxana-cold-outbox--smtp-send-default
  "Function called to send a composed message buffer.
It receives the message buffer and must return the sent Message-ID string.")
(defvar arxana-cold-outbox--intake-fn #'arxana-cold-outbox--call-intake
  "Function called to submit the temporary outreach event file to kit-intake.")

(defconst arxana-cold-outbox--stages
  '((:staged   :staged   "Staged"   "drafted + staged in the outbox; awaiting operator review")
    (:reviewed :reviewed "Reviewed" "operator has read the send-gate surface; cleared to send")
    (:sent     :sent     "Sent"     "operator sent it; :send-witness captured, kit-intake recorded")
    (:replied  :replied  "Replied"  "the world answered")
    (:silent   :silent   "Silent"   "outcome window passed, no reply"))
  "Outbox lifecycle stages in order.  Dual of the Sales pipeline stages.")

;; ---------------------------------------------------------------- data ----

(defun arxana-cold-outbox--read-staged-file (path)
  "Read one staged EDN file at PATH; return (:file PATH :data DATA)."
  (list :file path
        :data (arxana-browser-rewrites--read-edn-file path)))

(defun arxana-cold-outbox--staged-files ()
  "Return all readable staged.edn files below `arxana-cold-outbox-root'."
  (when (file-directory-p arxana-cold-outbox-root)
    (sort
     (cl-remove-if-not
      #'file-readable-p
      (directory-files-recursively arxana-cold-outbox-root "\\`staged\\.edn\\'"))
     #'string<)))

(defun arxana-cold-outbox--drafts ()
  "Read every staged draft live from disk."
  (delq nil
        (mapcar (lambda (path)
                  (ignore-errors (arxana-cold-outbox--read-staged-file path)))
                (arxana-cold-outbox--staged-files))))

(defun arxana-cold-outbox--data (draft) (plist-get draft :data))
(defun arxana-cold-outbox--file (draft) (plist-get draft :file))

(defun arxana-cold-outbox--id (draft)
  (plist-get (arxana-cold-outbox--data draft) :draft/id))

(defun arxana-cold-outbox--sym-str (x)
  "Render a keyword/symbol/string X without the leading colon."
  (cond ((keywordp x) (substring (symbol-name x) 1))
        ((symbolp x) (symbol-name x))
        ((null x) "—")
        (t (format "%s" x))))

(defun arxana-cold-outbox--stage (draft)
  "Return the outbox lifecycle stage for DRAFT.
When :draft/status is :sent, :send-projection :outcome derives :replied or
:silent where applicable."
  (let* ((m (arxana-cold-outbox--data draft))
         (status (plist-get m :draft/status))
         (send (plist-get m :send-projection))
         (outcome (plist-get send :outcome)))
    (cond ((and (eq status :sent) (eq outcome :reply)) :replied)
          ((and (eq status :sent) (memq outcome '(:silent :silence-after-window))) :silent)
          (t status))))

(defun arxana-cold-outbox--drafts-at (drafts stage)
  (cl-remove-if-not (lambda (d) (eq (arxana-cold-outbox--stage d) stage)) drafts))

(defun arxana-cold-outbox--plist-get-in (plist keys)
  "Return value in nested PLIST at KEYS."
  (let ((cur plist))
    (dolist (key keys cur)
      (setq cur (and (listp cur) (plist-get cur key))))))

(defun arxana-cold-outbox--path (draft key)
  "Resolve DRAFT path entry KEY against its staging directory."
  (let* ((m (arxana-cold-outbox--data draft))
         (paths (plist-get m :paths))
         (raw (plist-get paths key))
         (dir (or (plist-get paths :staging-dir)
                  (file-name-directory (arxana-cold-outbox--file draft)))))
    (cond ((not (stringp raw)) nil)
          ((file-name-absolute-p raw) raw)
          (t (expand-file-name raw dir)))))

;; ----------------------------------------------- native edit (surgical) ----
;; Find the target map by an anchor literal (\":draft/id \\\"...\\\"\") and
;; replace only the named field's value sexp; comments outside that value survive.

(defun arxana-cold-outbox--set-field! (file anchor key value-str)
  "In FILE, within the map containing ANCHOR, set KEY to literal VALUE-STR.
If KEY is absent, insert it after ANCHOR.  Targeted; preserves comments."
  (with-temp-buffer
    (insert-file-contents file)
    (modify-syntax-entry ?\{ "(}")
    (modify-syntax-entry ?\} "){")
    (modify-syntax-entry ?\[ "(]")
    (modify-syntax-entry ?\] ")[")
    (goto-char (point-min))
    (unless (search-forward anchor nil t)
      (error "Cold outbox: anchor not found: %s" anchor))
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
          (insert (format "\n %s %s" key value-str)))))
    (write-region (point-min) (point-max) file nil 'quiet)))

(defun arxana-cold-outbox--edn-string (s)
  (concat "\"" (replace-regexp-in-string "\\([\"\\\\]\\)" "\\\\\\1" s) "\""))

(defun arxana-cold-outbox--edn (x)
  "Render X as a small EDN literal covering the staged outbox schema."
  (cond
   ((null x) "nil")
   ((eq x t) "true")
   ((eq x :false) "false")
   ((keywordp x) (concat ":" (substring (symbol-name x) 1)))
   ((symbolp x) (symbol-name x))
   ((stringp x) (arxana-cold-outbox--edn-string x))
   ((numberp x) (number-to-string x))
   ((vectorp x)
    (concat "[" (mapconcat #'arxana-cold-outbox--edn (append x nil) " ") "]"))
   ((and (listp x) (keywordp (car x)))
    (let (pairs)
      (while x
        (push (format "%s %s"
                      (arxana-cold-outbox--edn (pop x))
                      (arxana-cold-outbox--edn (pop x)))
              pairs))
      (concat "{" (mapconcat #'identity (nreverse pairs) " ") "}")))
   ((listp x)
    (concat "[" (mapconcat #'arxana-cold-outbox--edn x " ") "]"))
   (t (arxana-cold-outbox--edn-string (format "%s" x)))))

;; -------------------------------------------------------------- render ----

(defun arxana-cold-outbox--button (label action &optional help)
  (insert-text-button label 'follow-link t 'help-echo help 'action action))

(define-derived-mode arxana-cold-outbox-mode special-mode "Cold-Outbox"
  "Major mode for the Arxana cold-chain staged outbox.
\\{arxana-cold-outbox-mode-map}")

(defvar-local arxana-cold-outbox--refresh-fn nil
  "Thunk re-rendering the current cold outbox view.")

(defun arxana-cold-outbox-refresh ()
  "Re-read staged EDN files and re-render the current view."
  (interactive)
  (if arxana-cold-outbox--refresh-fn
      (funcall arxana-cold-outbox--refresh-fn)
    (arxana-cold-outbox-browse)))

(defun arxana-cold-outbox--render-frame (body-fn &optional refresh-fn)
  (let ((buf (get-buffer-create arxana-cold-outbox--buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'arxana-cold-outbox-mode) (arxana-cold-outbox-mode))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (funcall body-fn)
        (goto-char (point-min)))
      (when refresh-fn (setq arxana-cold-outbox--refresh-fn refresh-fn)))
    (pop-to-buffer buf)))

(defun arxana-cold-outbox--prop-at-point (prop)
  (or (get-text-property (point) prop)
      (and (> (point) (point-min)) (get-text-property (1- (point)) prop))))

(defun arxana-cold-outbox--insert-kv (label value)
  (insert (format "  %-22s %s\n" label
                  (cond ((and (stringp value) (string-empty-p value)) "—")
                        ((null value) "—")
                        ((keywordp value) (arxana-cold-outbox--sym-str value))
                        (t (format "%s" value))))))

(defun arxana-cold-outbox--insert-target (target)
  (insert "Target\n")
  (arxana-cold-outbox--insert-kv "name" (plist-get target :name))
  (arxana-cold-outbox--insert-kv "org" (plist-get target :org))
  (arxana-cold-outbox--insert-kv "person" (plist-get target :person))
  (arxana-cold-outbox--insert-kv
   "contact-uri"
   (or (plist-get target :contact-uri) "<operator must add addressee>"))
  (arxana-cold-outbox--insert-kv "public-url" (plist-get target :public-url))
  (let ((rp (plist-get target :relationship-path)))
    (arxana-cold-outbox--insert-kv "relationship-class" (plist-get rp :class-candidate))
    (arxana-cold-outbox--insert-kv "relationship-basis" (plist-get rp :basis))
    (when (plist-get rp :notes)
      (insert (format "  relational-cold note  %s\n" (plist-get rp :notes)))))
  (insert "\n"))

(defun arxana-cold-outbox--insert-strawman (strawman)
  (insert "Strawman / warrant artifact\n")
  (if (not strawman)
      (insert "  —\n\n")
    (arxana-cold-outbox--insert-kv "kind" (plist-get strawman :kind))
    (arxana-cold-outbox--insert-kv "title" (plist-get strawman :title))
    (arxana-cold-outbox--insert-kv "url" (plist-get strawman :url))
    (arxana-cold-outbox--insert-kv "path" (plist-get strawman :path))
    (insert "\n")))

(defun arxana-cold-outbox--insert-body (draft)
  (insert "Drafted body\n")
  (let ((path (arxana-cold-outbox--path draft :draft-md)))
    (cond ((and path (file-readable-p path))
           (insert "────────────────────────────────────────────────────────────\n")
           (insert (with-temp-buffer (insert-file-contents path) (buffer-string)))
           (unless (bolp) (insert "\n"))
           (insert "────────────────────────────────────────────────────────────\n\n"))
          (t
           (insert "  <no body yet — run eoi-new>\n\n")))))

(defun arxana-cold-outbox--insert-provenance (m)
  (insert "Provenance\n")
  (let ((scan (plist-get m :source/scan))
        (match (plist-get m :interest-match)))
    (arxana-cold-outbox--insert-kv "scan axis" (plist-get scan :axis))
    (arxana-cold-outbox--insert-kv "scan date" (plist-get scan :date))
    (arxana-cold-outbox--insert-kv "scan path" (plist-get scan :scan-path))
    (arxana-cold-outbox--insert-kv "brief item" (plist-get scan :brief-item))
    (arxana-cold-outbox--insert-kv "match score" (plist-get match :score))
    (arxana-cold-outbox--insert-kv "match territory" (plist-get match :territory-label))
    (let ((because (append (plist-get match :because) nil)))
      (when because
        (insert "  because\n")
        (dolist (b because) (insert (format "    • %s\n" b))))))
  (insert "\n"))

(defun arxana-cold-outbox--insert-routing (m)
  (let ((routing (plist-get m :routing)))
    (insert "Routing\n")
    (arxana-cold-outbox--insert-kv "lead-class-candidate" (plist-get routing :lead-class-candidate))
    (arxana-cold-outbox--insert-kv "plants-thesis-candidate" (plist-get routing :plants-thesis-candidate))
    (arxana-cold-outbox--insert-kv
     "operator-confirmed?"
     (arxana-cold-outbox--plist-get-in routing '(:cold-relational-test :operator-confirmed?)))
    (insert "\n")))

(defun arxana-cold-outbox--insert-review (m)
  (let ((review (plist-get m :review)))
    (insert "Review / send gate\n")
    (arxana-cold-outbox--insert-kv "draft/status" (plist-get m :draft/status))
    (arxana-cold-outbox--insert-kv "review state" (plist-get review :state))
    (let ((jh (plist-get review :john-hancock-send)))
      (arxana-cold-outbox--insert-kv "send-authorized?" (plist-get jh :send-authorized?))
      (arxana-cold-outbox--insert-kv "send-channel" (plist-get jh :send-channel))
      (arxana-cold-outbox--insert-kv "send-witness" (plist-get jh :send-witness)))
    (insert "\n")))

(defun arxana-cold-outbox--render-draft (draft-id)
  "Render one staged draft send-gate surface."
  (let* ((draft (cl-find-if (lambda (d) (equal (arxana-cold-outbox--id d) draft-id))
                            (arxana-cold-outbox--drafts)))
         (m (and draft (arxana-cold-outbox--data draft))))
    (arxana-cold-outbox--render-frame
     (lambda ()
       (if (not draft)
           (insert (format "Cold Outbox — missing draft %s\n" draft-id))
         (let ((beg (point)))
           (insert (format "Cold Outbox — send gate: %s\n" draft-id))
           (insert (format "Source: %s\n\n" (arxana-cold-outbox--file draft)))
           (arxana-cold-outbox--button "[← entry points]" (lambda (_) (arxana-cold-outbox-browse)))
           (insert "    ")
           (arxana-cold-outbox--button "[raw EDN]" (lambda (_) (find-file (arxana-cold-outbox--file draft))))
           (insert "\n\n")
           (arxana-cold-outbox--insert-target (plist-get m :target))
           (arxana-cold-outbox--insert-strawman (plist-get m :strawman))
           (arxana-cold-outbox--insert-body draft)
           (arxana-cold-outbox--insert-provenance m)
           (arxana-cold-outbox--insert-routing m)
           (arxana-cold-outbox--insert-review m)
           (insert "Commands: r=mark reviewed  S=record witnessed send via kit-intake  E=raw EDN  g=refresh\n")
           (add-text-properties beg (point) (list 'arxana-cold-outbox-draft draft)))))
     (lambda () (arxana-cold-outbox--render-draft draft-id)))))

(defun arxana-cold-outbox--render-stage (stage)
  "List drafts at STAGE; each opens its send-gate surface."
  (let* ((spec (assq stage arxana-cold-outbox--stages))
         (drafts (arxana-cold-outbox--drafts-at (arxana-cold-outbox--drafts) stage)))
    (arxana-cold-outbox--render-frame
     (lambda ()
       (insert (format "Cold Outbox — %s  (%s)\n\n" (nth 2 spec) (nth 3 spec)))
       (arxana-cold-outbox--button "[← entry points]" (lambda (_) (arxana-cold-outbox-browse)))
       (insert "\n\n")
       (if (null drafts)
           (insert "  (none)\n")
         (dolist (d drafts)
           (let ((id (arxana-cold-outbox--id d))
                 (target (plist-get (arxana-cold-outbox--data d) :target)))
             (arxana-cold-outbox--button
              (format "▸ %s" id)
              (let ((i id)) (lambda (_) (arxana-cold-outbox--render-draft i)))
              "Open this draft's send gate")
             (insert (format "   — %s\n" (or (plist-get target :name) "?")))))))
     (lambda () (arxana-cold-outbox--render-stage stage)))))

(defun arxana-cold-outbox--render-all ()
  "Render a flat list of all staged outbox drafts."
  (let ((drafts (arxana-cold-outbox--drafts)))
    (arxana-cold-outbox--render-frame
     (lambda ()
       (insert "Cold Outbox — all drafts\n\n")
       (arxana-cold-outbox--button "[← entry points]" (lambda (_) (arxana-cold-outbox-browse)))
       (insert "\n\n")
       (if (null drafts)
           (insert "  (none)\n")
         (dolist (d drafts)
           (let* ((id (arxana-cold-outbox--id d))
                  (m (arxana-cold-outbox--data d))
                  (target (plist-get m :target)))
             (arxana-cold-outbox--button
              (format "▸ %s" id)
              (let ((i id)) (lambda (_) (arxana-cold-outbox--render-draft i)))
              "Open this draft's send gate")
             (insert (format "   — %s  [%s]\n"
                             (or (plist-get target :name) "?")
                             (arxana-cold-outbox--sym-str (arxana-cold-outbox--stage d))))))))
     #'arxana-cold-outbox--render-all)))

;; ----------------------------------------------------------- transitions ----

(defun arxana-cold-outbox--anchor (draft)
  (format ":draft/id %s" (arxana-cold-outbox--edn-string (arxana-cold-outbox--id draft))))

(defun arxana-cold-outbox--set-draft-field! (draft key value)
  "Set top-level KEY in DRAFT's staged.edn to EDN VALUE."
  (arxana-cold-outbox--set-field!
   (arxana-cold-outbox--file draft)
   (arxana-cold-outbox--anchor draft)
   key
   (arxana-cold-outbox--edn value)))

(defun arxana-cold-outbox--mark-reviewed-record (draft)
  "Flip DRAFT from :staged to :reviewed."
  (let ((status (plist-get (arxana-cold-outbox--data draft) :draft/status)))
    (unless (eq status :staged)
      (user-error "Draft is %s, not staged" (arxana-cold-outbox--sym-str status)))
    (arxana-cold-outbox--set-draft-field! draft ":draft/status" :reviewed)))

(defun arxana-cold-outbox-mark-reviewed ()
  "Mark the draft at point as reviewed."
  (interactive)
  (let ((draft (arxana-cold-outbox--prop-at-point 'arxana-cold-outbox-draft)))
    (unless draft (user-error "No cold outbox draft at point"))
    (arxana-cold-outbox--mark-reviewed-record draft)
    (message "%s → :reviewed" (arxana-cold-outbox--id draft))
    (arxana-cold-outbox-refresh)))

(defun arxana-cold-outbox--iso-now ()
  (format-time-string "%FT%T%z"))

(defun arxana-cold-outbox--outreach-event (draft send-witness &optional sent-at)
  "Construct the :outreach-sent event for DRAFT with SEND-WITNESS."
  (let* ((m (arxana-cold-outbox--data draft))
         (projection (copy-sequence (or (plist-get m :send-projection) nil)))
         (sent (or sent-at (arxana-cold-outbox--iso-now))))
    (plist-put projection :event :outreach-sent)
    (plist-put projection :sent-at sent)
    (plist-put projection :send-witness send-witness)
    projection))

(defun arxana-cold-outbox--intake-command (event-file)
  "Return the kit-intake shell command args for EVENT-FILE."
  (list "bb" arxana-cold-outbox-pudding-prover "intake!" event-file))

(defun arxana-cold-outbox--call-intake (event-file)
  "Run kit-intake for EVENT-FILE.  Return (EXIT-CODE . OUTPUT)."
  (let* ((buf (get-buffer-create "*Arxana Cold Outbox Intake*"))
         (args (cdr (arxana-cold-outbox--intake-command event-file))))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer)))
    (let ((exit (apply #'call-process "bb" nil buf nil args)))
      (cons exit (with-current-buffer buf (buffer-string))))))

(defun arxana-cold-outbox--smtp-configured-p ()
  "Return non-nil when SMTP is configured for direct staged EOI sends."
  (not (string-empty-p (string-trim (or arxana-cold-outbox-smtp-server "")))))

(defun arxana-cold-outbox--from-domain ()
  "Return local SMTP domain derived from `arxana-cold-outbox-from-address'."
  (or (cadr (split-string arxana-cold-outbox-from-address "@"))
      "localhost"))

(defun arxana-cold-outbox--draft-body (draft)
  "Return DRAFT body text from draft.md, or raise a user error."
  (let ((path (arxana-cold-outbox--path draft :draft-md)))
    (unless (and path (file-readable-p path))
      (user-error "No draft body readable; run eoi-new before sending"))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun arxana-cold-outbox--default-subject (draft)
  "Return the configured or inferred subject for DRAFT."
  (let* ((m (arxana-cold-outbox--data draft))
         (subject (plist-get m :subject))
         (strawman (plist-get m :strawman))
         (title (plist-get strawman :title))
         (target (plist-get m :target))
         (name (plist-get target :name)))
    (or subject
        (and title (format "Re: %s" title))
        (and name (format "Re: %s" name))
        "Re: staged EOI")))

(defun arxana-cold-outbox--required-address (draft)
  "Return DRAFT's target contact URI or prompt for a required address."
  (let* ((m (arxana-cold-outbox--data draft))
         (target (plist-get m :target))
         (contact (plist-get target :contact-uri))
         (addr (string-trim
                (or contact
                    (read-string "To address (required): ")))))
    (when (string-empty-p addr)
      (user-error "To address is required; refusing to fabricate one"))
    addr))

(defun arxana-cold-outbox--required-subject (draft)
  "Return DRAFT subject, prompting with an inferred default when absent."
  (let* ((m (arxana-cold-outbox--data draft))
         (configured (plist-get m :subject))
         (subject (if configured
                      configured
                    (read-string "Subject: " (arxana-cold-outbox--default-subject draft)))))
    (setq subject (string-trim subject))
    (when (string-empty-p subject)
      (user-error "Subject is required"))
    subject))

(defun arxana-cold-outbox--compose-message-buffer (draft to subject)
  "Return a preview buffer containing DRAFT as an SMTP message to TO."
  (let* ((message-user-fqdn (arxana-cold-outbox--from-domain))
         (buf (generate-new-buffer "*Arxana Cold Outbox Message Preview*"))
         (body (arxana-cold-outbox--draft-body draft))
         (message-id (message-make-message-id)))
    (with-current-buffer buf
      (message-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "From: %s\n" arxana-cold-outbox-from-address))
        (insert (format "To: %s\n" to))
        (insert (format "Subject: %s\n" subject))
        (insert (format "Message-ID: %s\n" message-id))
        (insert "\n")
        (insert body)
        (unless (bolp) (insert "\n"))
        (goto-char (point-min))))
    buf))

(defun arxana-cold-outbox--message-id (buffer)
  "Return the Message-ID header from BUFFER."
  (with-current-buffer buffer
    (string-trim (or (message-fetch-field "Message-ID") ""))))

(defun arxana-cold-outbox--smtp-send-default (buffer)
  "Send composed message BUFFER through `smtpmail'; return its Message-ID."
  (with-current-buffer buffer
    (let ((message-id (arxana-cold-outbox--message-id buffer))
          (send-mail-function #'smtpmail-send-it)
          (message-send-mail-function #'smtpmail-send-it)
          (smtpmail-smtp-server arxana-cold-outbox-smtp-server)
          (smtpmail-smtp-service arxana-cold-outbox-smtp-service)
          (smtpmail-smtp-user arxana-cold-outbox-smtp-user)
          (smtpmail-stream-type arxana-cold-outbox-smtp-stream-type)
          (smtpmail-local-domain (arxana-cold-outbox--from-domain))
          (user-mail-address arxana-cold-outbox-from-address))
      (unless (string-empty-p message-id)
        (smtpmail-send-it)
        message-id))))

(defun arxana-cold-outbox--replace-review-after-send (draft send-witness)
  "Return DRAFT :review value updated with SEND-WITNESS authorization."
  (let* ((review (copy-sequence (or (plist-get (arxana-cold-outbox--data draft) :review) nil)))
         (jh (copy-sequence (or (plist-get review :john-hancock-send) nil))))
    (plist-put review :state :sent)
    (plist-put jh :send-authorized? t)
    (plist-put jh :send-channel (if (arxana-cold-outbox--smtp-configured-p) :smtp :external))
    (plist-put jh :send-witness send-witness)
    (plist-put review :john-hancock-send jh)
    review))

(defun arxana-cold-outbox--record-send! (draft send-witness sent-at event)
  "Persist successful send transition for DRAFT."
  (arxana-cold-outbox--set-draft-field! draft ":draft/status" :sent)
  (arxana-cold-outbox--set-draft-field!
   draft ":review" (arxana-cold-outbox--replace-review-after-send draft send-witness))
  (arxana-cold-outbox--set-draft-field!
   draft ":send-projection" event)
  (message "%s → :sent at %s" (arxana-cold-outbox--id draft) sent-at))

(defun arxana-cold-outbox--intake-and-record! (draft send-witness sent-at event)
  "Submit EVENT, and on success record DRAFT as sent with SEND-WITNESS."
  (let ((tmp (make-temp-file "arxana-outreach-sent-" nil ".edn")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert (arxana-cold-outbox--edn event) "\n"))
          (let* ((result (funcall arxana-cold-outbox--intake-fn tmp))
                 (exit (car result))
                 (output (cdr result)))
            (if (zerop exit)
                (progn
                  (arxana-cold-outbox--record-send! draft send-witness sent-at event)
                  (message "kit-intake accepted: %s" (string-trim output))
                  (arxana-cold-outbox-refresh))
              (with-current-buffer (get-buffer-create "*Arxana Cold Outbox Intake*")
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert output)
                  (goto-char (point-max))
                  (insert (format "\nkit-intake rejected with exit %s\n" exit))))
              (pop-to-buffer "*Arxana Cold Outbox Intake*")
              (user-error "kit-intake rejected; draft not marked sent"))))
      (ignore-errors (delete-file tmp)))))

(defun arxana-cold-outbox--record-external-send (draft)
  "Fallback path: prompt for external send witness, then route through intake."
  (unless (y-or-n-p "Operator confirms this was sent in mail/client? ")
    (user-error "Send recording aborted"))
  (let ((witness (string-trim (read-string "send-witness (Message-ID/receipt/sent ISO): "))))
    (when (string-empty-p witness)
      (user-error "send-witness is required; refusing to fabricate one"))
    (let* ((sent-at (arxana-cold-outbox--iso-now))
           (event (arxana-cold-outbox--outreach-event draft witness sent-at)))
      (arxana-cold-outbox--intake-and-record! draft witness sent-at event))))

(defun arxana-cold-outbox--send-via-smtp (draft)
  "Compose, preview, SMTP-send DRAFT, then route its Message-ID through intake."
  (let* ((to (arxana-cold-outbox--required-address draft))
         (subject (arxana-cold-outbox--required-subject draft))
         (buffer (arxana-cold-outbox--compose-message-buffer draft to subject)))
    (unwind-protect
        (progn
          (pop-to-buffer buffer)
          (unless (y-or-n-p
                   (format "Send this now via SMTP as %s? "
                           arxana-cold-outbox-from-address))
            (user-error "SMTP send aborted"))
          (let ((message-id (string-trim
                             (or (funcall arxana-cold-outbox--smtp-send-fn buffer) ""))))
            (when (string-empty-p message-id)
              (user-error "SMTP send returned no Message-ID; draft not marked sent"))
            (let* ((sent-at (arxana-cold-outbox--iso-now))
                   (event (arxana-cold-outbox--outreach-event draft message-id sent-at)))
              (arxana-cold-outbox--intake-and-record! draft message-id sent-at event))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun arxana-cold-outbox-send ()
  "Send or record a witnessed send for the draft at point through kit-intake."
  (interactive)
  (let* ((draft (arxana-cold-outbox--prop-at-point 'arxana-cold-outbox-draft))
         (status (and draft (plist-get (arxana-cold-outbox--data draft) :draft/status))))
    (unless draft (user-error "No cold outbox draft at point"))
    (unless (memq status '(:staged :reviewed))
      (user-error "Draft is %s; only staged/reviewed drafts can be sent"
                  (arxana-cold-outbox--sym-str status)))
    (if (arxana-cold-outbox--smtp-configured-p)
        (arxana-cold-outbox--send-via-smtp draft)
      (arxana-cold-outbox--record-external-send draft))))

(defun arxana-cold-outbox-edit-raw ()
  "Open the staged.edn file for the draft at point, or the outbox root."
  (interactive)
  (let ((draft (arxana-cold-outbox--prop-at-point 'arxana-cold-outbox-draft)))
    (if draft
        (find-file (arxana-cold-outbox--file draft))
      (dired arxana-cold-outbox-root))))

(defvar arxana-cold-outbox-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "g" #'arxana-cold-outbox-refresh)
    (define-key m "r" #'arxana-cold-outbox-mark-reviewed)
    (define-key m "S" #'arxana-cold-outbox-send)
    (define-key m "E" #'arxana-cold-outbox-edit-raw)
    m)
  "Keymap for `arxana-cold-outbox-mode'.")

;;;###autoload
(defun arxana-cold-outbox-browse ()
  "Open the cold-chain staged outbox with stage entry points + all drafts."
  (interactive)
  (let ((drafts (arxana-cold-outbox--drafts)))
    (arxana-cold-outbox--render-frame
     (lambda ()
       (insert "Cold Outbox — staged EOI send gates\n")
       (insert (format "Source: %s\n\n" arxana-cold-outbox-root))
       (dolist (spec arxana-cold-outbox--stages)
         (let ((sub (arxana-cold-outbox--drafts-at drafts (nth 0 spec))))
           (arxana-cold-outbox--button
            (format "▸ %s" (nth 2 spec))
            (let ((s (nth 0 spec))) (lambda (_) (arxana-cold-outbox--render-stage s)))
            (nth 3 spec))
           (insert (format "   — %d draft(s)  (%s)\n" (length sub) (nth 3 spec)))))
       (insert "\n")
       (arxana-cold-outbox--button "▸ All drafts"
                                   (lambda (_) (arxana-cold-outbox--render-all))
                                   "Flat list of staged outbox drafts")
       (insert (format "   — %d total\n\n" (length drafts)))
       (insert "Keys: RET/buttons=open  r=reviewed  S=record witnessed send  E=raw EDN  g=refresh\n"))
     #'arxana-cold-outbox-browse)))

;; ---------------------------------------------------- home integration ----

(defun arxana-cold-outbox--home-items ()
  "Return stage entries + an all-drafts entry for the browser home outbox view."
  (let ((drafts (arxana-cold-outbox--drafts)))
    (append
     (mapcar
      (lambda (spec)
        (let ((sub (arxana-cold-outbox--drafts-at drafts (nth 0 spec))))
          (list :type 'cold-outbox-stage
                :label (nth 2 spec)
                :stage (nth 0 spec)
                :description (format "%d draft(s) — %s" (length sub) (nth 3 spec)))))
      arxana-cold-outbox--stages)
     (list (list :type 'cold-outbox-all
                 :label "All cold outbox drafts"
                 :description (format "%d staged outbox draft(s)" (length drafts)))))))

(defun arxana-cold-outbox-open-stage (item)
  "Open the stage or flat-list named by ITEM."
  (pcase (plist-get item :type)
    ('cold-outbox-all (arxana-cold-outbox--render-all))
    (_                (arxana-cold-outbox--render-stage (plist-get item :stage)))))

(provide 'arxana-cold-outbox)
;;; arxana-cold-outbox.el ends here
