;;; mojira-mode.el --- Highlight substrate-2 glossary entities -*- lexical-binding: t; -*-

;;; Commentary:
;; Buffer-local minor mode that highlights the named entities of the
;; substrate-2 glossary (~/storage/lexicon/glossary.json, produced by
;; futon3c/scripts/entity_glossary.py).  All forms are compiled into a
;; single longest-first regexp once per load; highlighting is done
;; through font-lock with a matcher function, so jit-lock keeps it fast
;; in large chat buffers and redisplay never touches the JSON file.

;;; Code:

(require 'json)
(require 'cl-lib)

(defgroup mojira nil
  "Highlight glossary entities in buffers."
  :group 'convenience)

(defcustom mojira-glossary-file "~/storage/lexicon/glossary.json"
  "Path to the glossary JSON read by `mojira-mode-reload'."
  :type 'file
  :group 'mojira)

(defface mojira-entity
  '((t :underline t))
  "Face for a form that resolves to one glossary entity.")

(defface mojira-entity-ambiguous
  '((t :underline (:style wave :color "dark orange")))
  "Face for a form shared by several glossary entities.")

(defvar mojira--state nil
  "Cached glossary: plist with :regexp, :by-form, :entities, :partners.")

(defun mojira--first-sentence (what)
  "First sentence of WHAT, or WHAT itself."
  (if (or (null what) (string-empty-p what))
      ""
    (let ((i (string-match "[.!?\u3002]" what)))
      (if i (substring what 0 (min (1+ i) (length what))) what))))

(defun mojira--build-regexp (forms)
  "Regexp matching any of FORMS, longest form first, whole-symbol."
  (concat "\\_<\\(?:"
          (mapconcat #'regexp-quote
                     (sort forms (lambda (a b) (> (length a) (length b))))
                     "\\|")
          "\\)\\_>"))

(defun mojira--load ()
  "Read `mojira-glossary-file' once and build the highlight state."
  (let* ((data (json-read-file (expand-file-name mojira-glossary-file)))
         (forms (append (cdr (assq 'forms data)) nil))
         (conflicts (append (cdr (assq 'form-conflicts data)) nil))
         (entities (append (cdr (assq 'entities data)) nil))
         (dups (append (cdr (assq 'possible-duplicates data)) nil))
         (by-form (make-hash-table :test #'equal))
         (ent (make-hash-table :test #'equal))
         (partners (make-hash-table :test #'equal)))
    (dolist (f forms)
      ;; json.el's default object type gives alist keys as symbols.
      (let ((key (if (symbolp (car f)) (symbol-name (car f)) (car f))))
        (when (and (stringp key) (not (string-empty-p key)))
          (puthash key (list (cdr f)) by-form))))
    (dolist (c conflicts)
      (puthash (cdr (assq 'form c)) (append (cdr (assq 'slugs c)) nil) by-form))
    (dolist (e entities)
      (puthash (cdr (assq 'slug e)) e ent))
    (dolist (d dups)
      (dolist (s (append (cdr (assq 'slugs d)) nil))
        (dolist (o (append (cdr (assq 'slugs d)) nil))
          (unless (equal s o)
            (cl-pushnew o (gethash s partners) :test #'equal)))))
    (list :regexp (mojira--build-regexp (hash-table-keys by-form))
          :by-form by-form
          :entities ent
          :partners partners)))

(defun mojira--ensure-state ()
  "Load the glossary if not already cached."
  (unless mojira--state
    (setq mojira--state (mojira--load))))

(defun mojira-mode-reload ()
  "Re-read `mojira-glossary-file' and refontify mojira buffers."
  (interactive)
  (setq mojira--state (mojira--load))
  (dolist (b (buffer-list))
    (when (buffer-local-value 'mojira-mode b)
      (with-current-buffer b (font-lock-flush)))))

(defun mojira--slugs-for (form)
  "Candidate slugs for lowercased FORM."
  (and mojira--state
       (gethash form (plist-get mojira--state :by-form))))

(defun mojira--help-echo (form slugs)
  "Tooltip text for FORM with candidate SLUGS."
  (if (or (null slugs) (/= 1 (length slugs)))
      (format "Ambiguous form \"%s\"; candidates: %s"
              form (or slugs '(none)))
    (let* ((e (gethash (car slugs) (plist-get mojira--state :entities)))
           (attested (cdr (assq 'store-attested e))))
      (format "%s (%s)\n%s\nwhere: %s\ngrounded: %s%s"
              (cdr (assq 'name e))
              (car slugs)
              (mojira--first-sentence (cdr (assq 'what e)))
              (or (cdr (assq 'where e)) "")
              (or (cdr (assq 'grounded e)) "")
              (if (memq attested '(nil :false :json-false))
                  "\nfile content only" "")))))

(defvar mojira--last-form nil
  "Lowercased form matched by the last `mojira--matcher' call.")

(defun mojira--matcher (limit)
  "Font-lock matcher: find the next glossary form before LIMIT."
  (let ((regexp (and mojira--state (plist-get mojira--state :regexp))))
    (when regexp
      (let ((case-fold-search t))
        (when (re-search-forward regexp limit t)
          (let* ((form (downcase (match-string 0)))
                 (slugs (mojira--slugs-for form))
                 (beg (match-beginning 0))
                 (end (match-end 0)))
            ;; Set properties inside save-match-data: help-echo does
            ;; string-match, which would clobber our match data.
            (save-match-data
              (put-text-property beg end 'mojira-form form)
              (put-text-property
               beg end 'help-echo (mojira--help-echo form slugs)))
            (setq mojira--last-form form)
            t))))))

(defun mojira--match-face ()
  "Face for the form just matched by `mojira--matcher'."
  (let* ((form mojira--last-form)
         (slugs (and form (mojira--slugs-for form))))
    (if (and slugs (= 1 (length slugs)))
        'mojira-entity
      'mojira-entity-ambiguous)))

(defvar mojira--keywords
  '((mojira--matcher (0 (mojira--match-face) prepend))))

(defun mojira--insert-field (label value)
  (insert label ": ")
  (insert (cond ((stringp value) value)
                ((vectorp value) (mapconcat #'identity (append value nil) ", "))
                ((consp value) (mapconcat #'identity value ", "))
                ((null value) "")
                (t (format "%s" value))))
  (insert "\n"))

(defun mojira--insert-entity (slug)
  "Insert the full glossary entry for SLUG at point."
  (let ((e (gethash slug (plist-get mojira--state :entities))))
    (if (not e)
        (insert "no entry for " slug "\n")
      (insert (format "%s (%s)\n" (cdr (assq 'name e)) slug))
      (mojira--insert-field "forms" (cdr (assq 'forms e)))
      (insert "what: " (or (cdr (assq 'what e)) "") "\n")
      (insert "where: " (or (cdr (assq 'where e)) "") "\n")
      (insert "relations: " (or (cdr (assq 'relations e)) "") "\n")
      (mojira--insert-field "producers" (cdr (assq 'producers e)))
      (insert "grounded: " (or (cdr (assq 'grounded e)) "") "\n")
      (insert "status: " (or (cdr (assq 'status e)) "") "\n")
      (insert "store-attested: "
              (if (memq (cdr (assq 'store-attested e)) '(nil :false :json-false))
                  "no (file content only)"
                "yes")
              "\n")
      (mojira--insert-field "possible duplicates"
                            (gethash slug (plist-get mojira--state :partners))))))

(defun mojira-describe-entity-at-point ()
  "Show the full glossary entry for the entity form at point."
  (interactive)
  (let ((form (or (get-text-property (point) 'mojira-form)
                  (and (> (point) (point-min))
                       (get-text-property (1- (point)) 'mojira-form)))))
    (if (not form)
        (message "No mojira form at point")
      (let* ((slugs (mojira--slugs-for form))
             (buf (get-buffer-create "*Mojira*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (dolist (slug (or slugs '(none)))
              (mojira--insert-entity (format "%s" slug))
              (insert "\n"))
            (goto-char (point-min))))
        (pop-to-buffer buf)))))

;;;###autoload
(define-minor-mode mojira-mode
  "Highlight substrate-2 glossary entities in this buffer.
Forms resolving to one entity get the `mojira-entity' face; forms
shared by several get `mojira-entity-ambiguous'.  Use
\\[mojira-describe-entity-at-point] for the full entry and
\\[mojira-mode-reload] to re-read the JSON."
  :lighter " Mojira"
  (cond (mojira-mode
         (mojira--ensure-state)
         (font-lock-add-keywords nil mojira--keywords)
         (font-lock-flush))
        (t
         (font-lock-remove-keywords nil mojira--keywords)
         (font-lock-flush))))

(provide 'mojira-mode)
;;; mojira-mode.el ends here
