;;; arxana-xtdb-browse.el --- XTDB browser for Futon datasets -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides an Emacs UI for peeking into Futon/XTDB data.  Uses `/tail`
;; to list recent relations and lets users drill into entities or
;; open the existing relation buffers.

;;; Code:

(require 'seq)
(require 'tabulated-list)
(require 'subr-x)
(require 'arxana-store)
(require 'arxana-relations)

(defgroup arxana-xtdb-browse nil
  "Interactive browser for Futon/XTDB contents."
  :group 'arxana)

(defcustom arxana-xtdb-browse-default-limit 25
  "Number of relations to request when browsing XTDB."
  :type 'integer
  :group 'arxana-xtdb-browse)

(defvar arxana-xtdb-browse-buffer "*Arxana XTDB Browser*")

(defvar-local arxana-xtdb--current-limit arxana-xtdb-browse-default-limit)
(defvar-local arxana-xtdb--tail-response nil)

(defun arxana-xtdb--entity-name (entity)
  (or (alist-get :name entity)
      (alist-get :entity/name entity)
      "<unknown>"))

(defun arxana-xtdb--entity-type (entity)
  (or (alist-get :type entity)
      (alist-get :entity/type entity)))

(defun arxana-xtdb--entity-ident (entity)
  (or (alist-get :ident entity)
      (alist-get :entity/id entity)))

(defun arxana-xtdb--entity-label (entity)
  (let ((name (arxana-xtdb--entity-name entity))
        (type (arxana-xtdb--entity-type entity)))
    (if type
        (format "%s (%s)" name type)
      name)))

(defun arxana-xtdb--relation-row-p (row)
  (and (listp row)
       (consp (car row))
       (or (alist-get :src row)
           (alist-get :dst row))))

(defun arxana-xtdb--tail-rows (body)
  "Normalize BODY (from `/tail`) into a list of relation alists."
  (let* ((section (or (alist-get :tail body)
                      body))
         (raw (or (alist-get :relations section)
                  (alist-get :links section)
                  (alist-get :items section)
                  section)))
    (when (not (listp raw))
      (setq raw nil))
    (seq-filter #'arxana-xtdb--relation-row-p raw)))

(defun arxana-xtdb--rows->entries (rows)
  "Return tabulated-list entries derived from ROWS."
  (let ((index 0))
    (mapcar
     (lambda (row)
       (setq index (1+ index))
       (let* ((src (alist-get :src row))
              (dst (alist-get :dst row))
              (relation (alist-get :type row))
              (last-seen (or (alist-get :last-seen row)
                             (alist-get :hx/last-seen row)
                             ""))
              (vector (vector (format "%s" last-seen)
                              (format "%s" relation)
                              (arxana-xtdb--entity-label src)
                              (arxana-xtdb--entity-label dst))))
         (list index vector row)))
     rows)))

(defun arxana-xtdb--describe-entity (entity)
  "Display ENTITY data using `arxana-store-fetch-entity'."
  (let* ((ident (arxana-xtdb--entity-ident entity))
         (name (arxana-xtdb--entity-name entity)))
    (unless ident
      (user-error "No Futon id recorded for %s" name))
    (let ((data (arxana-store-fetch-entity ident)))
      (unless data
        (user-error "No Futon data for %s" name))
      (with-current-buffer (get-buffer-create "*Arxana Entity*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Entity: %s\nId: %s\n\n" name ident))
          (pp data (current-buffer))
          (goto-char (point-min))
          (view-mode 1))
        (display-buffer (current-buffer))))))

(defun arxana-xtdb--entry-at-point ()
  (or (get-text-property (point) 'tabulated-list-entry)
      (user-error "No XTDB relation on this line")))

(defun arxana-xtdb--entity-at-point (slot)
  (let* ((entry (arxana-xtdb--entry-at-point))
         (relation (nth 2 entry)))
    (alist-get slot relation)))

(defun arxana-xtdb-browse-open-source ()
  "Open the entity detail buffer for the source at point."
  (interactive)
  (arxana-xtdb--describe-entity (arxana-xtdb--entity-at-point :src)))

(defun arxana-xtdb-browse-open-target ()
  "Open the entity detail buffer for the target at point."
  (interactive)
  (arxana-xtdb--describe-entity (arxana-xtdb--entity-at-point :dst)))

(defun arxana-xtdb-browse-show-ego ()
  "Display `/ego` view for the entity at point."
  (interactive)
  (let* ((entity (or (arxana-xtdb--entity-at-point :src)
                     (arxana-xtdb--entity-at-point :dst)))
         (name (arxana-xtdb--entity-name entity)))
    (arxana-relations-show-ego name arxana-xtdb--current-limit)))

(defun arxana-xtdb-browse-balances ()
  (interactive)
  (message "Not yet implemented"))

(defun arxana-xtdb--populate (rows)
  (setq tabulated-list-entries (arxana-xtdb--rows->entries rows))
  (tabulated-list-print t))

(defun arxana-xtdb-refresh (&optional limit)
  "Refresh the XTDB browser contents with LIMIT rows."
  (interactive)
  (let* ((limit (or limit arxana-xtdb--current-limit))
         (body (arxana-store-tail limit)))
    (setq arxana-xtdb--current-limit limit
          arxana-xtdb--tail-response body)
    (let ((rows (arxana-xtdb--tail-rows (or body '()))))
      (if rows
          (progn
            (arxana-xtdb--populate rows)
            (message "Fetched %d relations" (length rows)))
        (setq tabulated-list-entries nil)
        (tabulated-list-print t)
        (message "No relations returned from Futon")))))

(defun arxana-xtdb-browse-change-limit (limit)
  "Set a new LIMIT for `/tail` results and refresh."
  (interactive (list (read-number "Tail limit: " arxana-xtdb--current-limit)))
  (arxana-xtdb-refresh limit))

(defun arxana-xtdb-browse-inspect-raw ()
  "Display the raw `/tail` payload for debugging."
  (interactive)
  (unless arxana-xtdb--tail-response
    (user-error "No XTDB payload captured yet"))
  (with-current-buffer (get-buffer-create "*Arxana XTDB Raw*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (pp arxana-xtdb--tail-response (current-buffer))
      (goto-char (point-min))
      (view-mode 1))
    (display-buffer (current-buffer))))

(defvar arxana-xtdb-browse-mode-map
  (let ((map (copy-keymap tabulated-list-mode-map)))
    (define-key map (kbd "g") #'arxana-xtdb-refresh)
    (define-key map (kbd "l") #'arxana-xtdb-browse-change-limit)
    (define-key map (kbd "RET") #'arxana-xtdb-browse-show-ego)
    (define-key map (kbd "s") #'arxana-xtdb-browse-open-source)
    (define-key map (kbd "t") #'arxana-xtdb-browse-open-target)
    (define-key map (kbd "R") #'arxana-xtdb-browse-inspect-raw)
    map)
  "Keymap for `arxana-xtdb-browse-mode'.")

(define-derived-mode arxana-xtdb-browse-mode tabulated-list-mode "XTDB"
  "Major mode for browsing Futon/XTDB relations."
  (setq tabulated-list-format [("Seen" 18 t)
                               ("Relation" 18 t)
                               ("Source" 25 t)
                               ("Target" 25 t)]
        tabulated-list-padding 2
        tabulated-list-sort-key (cons "Seen" nil))
  (tabulated-list-init-header))

;;;###autoload
(defun arxana-xtdb-browse (&optional limit)
  "List recent Futon relations using a tabulated UI.
When LIMIT is provided (or via prefix), request that many rows."
  (interactive (list (when current-prefix-arg
                       (read-number "Tail limit: " arxana-xtdb-browse-default-limit))))
  (unless (arxana-store-sync-enabled-p)
    (user-error "Futon sync is disabled; enable it to browse XTDB"))
  (let ((buffer (get-buffer-create arxana-xtdb-browse-buffer)))
    (with-current-buffer buffer
      (arxana-xtdb-browse-mode)
      (arxana-xtdb-refresh (or limit arxana-xtdb-browse-default-limit)))
    (display-buffer buffer)))

(provide 'arxana-xtdb-browse)

;;; arxana-xtdb-browse.el ends here
