;;; arxana-org-links.el --- Persist Org task pattern links -*- lexical-binding: t; -*-

;;; Commentary:
;; Persist Org task -> pattern associations into Futon/XTDB as relations.
;; This uses :PATTERN:, :PATTERN1:, :PATTERN2: properties on Org headings.

;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-id)

(declare-function arxana-store-create-relations-batch "arxana-store" (&rest args))
(declare-function arxana-store-create-relations-batch-async "arxana-store" (&rest args))
(declare-function arxana-store-sync-enabled-p "arxana-store" ())
(declare-function arxana-store-ego "arxana-store" (&rest args))

(defgroup arxana-org-links nil
  "Org task link persistence for Arxana."
  :group 'arxana
  :prefix "arxana-org-links-")

(defcustom arxana-org-links-relation-type "arxana/pattern-link"
  "Relation type used for Org task -> pattern associations."
  :type 'string
  :group 'arxana-org-links)

(defcustom arxana-org-links-task-entity-type "org/task"
  "Entity type used for Org task nodes."
  :type 'string
  :group 'arxana-org-links)

(defcustom arxana-org-links-pattern-entity-type "arxana/pattern"
  "Entity type used for pattern nodes."
  :type 'string
  :group 'arxana-org-links)

(defcustom arxana-org-links-pattern-property-regexp "^PATTERN\\([0-9]+\\)?$"
  "Regexp for Org properties that encode pattern associations."
  :type 'regexp
  :group 'arxana-org-links)

(defcustom arxana-org-links-batch-size 50
  "Number of relations to send per batch."
  :type 'integer
  :group 'arxana-org-links)

(defcustom arxana-org-links-chunk-size 25
  "Number of Org headings processed per async tick."
  :type 'integer
  :group 'arxana-org-links)

(defcustom arxana-org-links-idle-delay 0.05
  "Idle delay between async processing ticks."
  :type 'number
  :group 'arxana-org-links)

(defcustom arxana-org-links-hover-delay 0.2
  "Seconds to wait before opening a hovered pattern."
  :type 'number
  :group 'arxana-org-links)

(defcustom arxana-org-links-hover-window-width 0.45
  "Width of the ancillary window used for pattern hover previews."
  :type 'number
  :group 'arxana-org-links)

(defcustom arxana-org-links-enable-hover t
  "When non-nil, enable pattern hover previews in Org buffers."
  :type 'boolean
  :group 'arxana-org-links)

(defvar arxana-org-links--pattern-prop-regexp
  "^\\s-*:PATTERN[0-9]*:\\s-*\\(.+\\)$")

(defvar-local arxana-org-links--hover-timer nil)
(defvar-local arxana-org-links--hover-pattern nil)
(defvar-local arxana-org-links--hover-window nil)
(defvar-local arxana-org-links--pattern-overlays nil)

(defvar arxana-org-links--pattern-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'arxana-org-links-open-pattern-at-point)
    (define-key map [mouse-1] #'arxana-org-links-open-pattern-at-point)
    map)
  "Keymap for pattern links in Org buffers.")

(defun arxana-org-links--string-trim (value)
  "Trim whitespace from VALUE."
  (replace-regexp-in-string "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" "" (or value "")))

(defun arxana-org-links--split-patterns (value)
  "Split VALUE into pattern ids."
  (let ((raw (arxana-org-links--string-trim value)))
    (when (and raw (> (length raw) 0))
      (mapcar #'arxana-org-links--string-trim
              (split-string raw "," t "[ \t\n\r]+")))))

(defun arxana-org-links--entry-pattern-ids ()
  "Return pattern ids for the current Org entry."
  (let* ((props (org-entry-properties nil 'standard))
         (out nil))
    (dolist (pair props (nreverse out))
      (let ((key (car pair))
            (val (cdr pair)))
        (when (and key (string-match-p arxana-org-links-pattern-property-regexp key))
          (dolist (pattern (arxana-org-links--split-patterns val))
            (when (and pattern (> (length pattern) 0))
              (push pattern out))))))))

(defun arxana-org-links--entry-task-id ()
  "Ensure and return the Org ID for the current entry."
  (or (org-entry-get nil "ID")
      (org-id-get-create)))

(defun arxana-org-links--task-entity-id (org-id)
  "Return the XTDB entity id for ORG-ID."
  (format "org-task:%s" org-id))

(defun arxana-org-links--pattern-entity-id (pattern-id)
  "Return the XTDB entity id for PATTERN-ID."
  (format "pattern:%s" pattern-id))

(defun arxana-org-links--entry-outline ()
  "Return a string outline path for the current Org entry."
  (let ((path (org-get-outline-path t t)))
    (mapconcat #'identity path " / ")))

(defun arxana-org-links--relation-spec (task-id org-id org-file todo heading outline scheduled pattern-id)
  "Build a relation spec for TASK-ID, HEADING, and PATTERN-ID."
  (let* ((clean-heading (arxana-org-links--string-trim heading))
         (todo-prefix (and (stringp todo) (> (length todo) 0) (concat todo " ")))
         (task-label (concat (or todo-prefix "") clean-heading))
         (task-name (if (> (length task-label) 0) task-label task-id))
         (pattern-entity (arxana-org-links--pattern-entity-id pattern-id))
         (props (delq nil
                      (list (cons 'org/id org-id)
                            (cons 'org/file org-file)
                            (cons 'org/todo todo)
                            (cons 'org/heading clean-heading)
                            (cons 'org/outline outline)
                            (cons 'org/scheduled scheduled)))))
    (list (cons 'type arxana-org-links-relation-type)
          (cons 'src (list (cons 'id task-id)
                           (cons 'name task-name)
                           (cons 'type arxana-org-links-task-entity-type)))
          (cons 'dst (list (cons 'id pattern-entity)
                           (cons 'name pattern-id)
                           (cons 'type arxana-org-links-pattern-entity-type)))
          (cons 'props props))))

(defun arxana-org-links--collect-entry-relations ()
  "Return relation specs for the current Org entry."
  (let* ((patterns (arxana-org-links--entry-pattern-ids))
         (org-id (arxana-org-links--entry-task-id))
         (task-id (arxana-org-links--task-entity-id org-id))
         (heading (or (nth 4 (org-heading-components)) ""))
         (todo (or (org-get-todo-state) ""))
         (outline (arxana-org-links--entry-outline))
         (org-file (or (buffer-file-name) ""))
         (scheduled-time (org-get-scheduled-time nil))
         (scheduled (when scheduled-time
                      (format-time-string "%Y-%m-%d" scheduled-time))))
    (mapcar (lambda (pattern-id)
              (arxana-org-links--relation-spec task-id org-id org-file todo heading
                                               outline scheduled pattern-id))
            patterns)))

(defun arxana-org-links--persist-relations (relations)
  "Persist RELATIONS in a single request."
  (unless (arxana-store-sync-enabled-p)
    (user-error "Futon sync disabled; enable sync before persisting Org links"))
  (when relations
    (arxana-store-create-relations-batch relations)))

(defun arxana-org-links--persist-relations-async (relations callback)
  "Persist RELATIONS asynchronously and invoke CALLBACK with (RESPONSE STATUS)."
  (unless (arxana-store-sync-enabled-p)
    (user-error "Futon sync disabled; enable sync before persisting Org links"))
  (when relations
    (arxana-store-create-relations-batch-async relations callback)))

(defun arxana-org-links--relation-type-string (value)
  "Normalize relation type VALUE into a string."
  (cond
   ((keywordp value) (substring (symbol-name value) 1))
   ((symbolp value) (symbol-name value))
   ((stringp value) value)
   (t "")))

(defun arxana-org-links--extract-backlink-rows (response)
  "Return incoming rows matching the pattern-link relation type."
  (let* ((ego (or (alist-get :ego response) response))
         (incoming (or (alist-get :incoming ego) '()))
         (target arxana-org-links-relation-type)
         (matched nil))
    (dolist (row incoming (nreverse matched))
      (let* ((rel (alist-get :relation row))
             (rel-type (arxana-org-links--relation-type-string rel)))
        (when (string= rel-type target)
          (push row matched))))))

;;;###autoload
(defun arxana-org-links-show-pattern-backlinks (pattern-id)
  "Show Org task backlinks for PATTERN-ID using XTDB."
  (interactive (list (read-string "Pattern id: ")))
  (let* ((resp (arxana-store-ego pattern-id))
         (rows (and resp (arxana-org-links--extract-backlink-rows resp)))
         (buffer (get-buffer-create "*Arxana Pattern Backlinks*")))
    (with-current-buffer buffer
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "Pattern: %s\n" pattern-id))
      (if (not (and rows (> (length rows) 0)))
          (insert "  (no backlinks found)\n")
        (dolist (row rows)
          (let* ((entity (alist-get :entity row))
                 (name (or (alist-get :name entity)
                           (alist-get :entity/name entity)
                           (alist-get :entity/id entity)
                           "?"))
                 (eid (or (alist-get :entity/id entity)
                          (alist-get :id entity)
                          "")))
            (insert (format "  - %s%s\n" name
                            (if (and eid (> (length eid) 0))
                                (format " (%s)" eid)
                              ""))))))
      (goto-char (point-min))
      (read-only-mode 1))
    (display-buffer buffer)))

;;;###autoload
(defun arxana-org-links-persist-entry-patterns ()
  "Persist pattern associations for the current Org entry."
  (interactive)
  (let ((relations (arxana-org-links--collect-entry-relations)))
    (if relations
        (progn
          (arxana-org-links--persist-relations relations)
          (message "Persisted %d pattern link(s)" (length relations)))
      (message "No PATTERN properties found on this entry"))))

;;;###autoload
(defun arxana-org-links-persist-buffer-patterns-sync ()
  "Persist pattern associations for entries in the current Org buffer (sync)."
  (let ((relations nil)
        (count 0))
    (org-map-entries
     (lambda ()
       (let ((entry-relations (arxana-org-links--collect-entry-relations)))
         (when entry-relations
           (setq relations (nconc relations entry-relations))
           (setq count (+ count (length entry-relations)))))))
    (if relations
        (progn
          (arxana-org-links--persist-relations relations)
          (message "Persisted %d pattern link(s) in buffer" count))
      (message "No PATTERN properties found in buffer"))))

(defun arxana-org-links-persist-buffer-patterns-async ()
  "Persist pattern associations for entries in the current Org buffer (async)."
  (unless (derived-mode-p 'org-mode)
    (user-error "Current buffer is not an Org buffer"))
  (let* ((buffer (current-buffer))
         (pos (point-min))
         (done nil)
         (pending nil)
         (processed 0)
         (relation-count 0)
         (sent 0)
         (in-flight 0)
         (flush-fn nil)
         (step-fn nil))
    (message "Persisting Org pattern links…")
    (setq flush-fn
          (lambda ()
            (when (and pending (listp pending))
              (let ((batch pending))
                (setq pending nil)
                (setq in-flight (1+ in-flight))
                (setq sent (+ sent (length batch)))
                (arxana-org-links--persist-relations-async
                 batch
                 (lambda (_resp status)
                   (setq in-flight (max 0 (1- in-flight)))
                   (when (plist-get status :error)
                     (message "Persist batch failed: %s" (plist-get status :error)))
                   (when (and done (= in-flight 0) (null pending))
                     (message "Persisted %d pattern link(s) in buffer" relation-count))))))))
    (setq step-fn
          (lambda ()
            (when (buffer-live-p buffer)
              (with-current-buffer buffer
                (save-excursion
                  (goto-char pos)
                  (catch 'done
                    (dotimes (_ arxana-org-links-chunk-size)
                      (when (eobp)
                        (setq done t)
                        (throw 'done nil))
                      (unless (org-at-heading-p)
                        (outline-next-heading))
                      (when (org-at-heading-p)
                        (let ((entry-relations (arxana-org-links--collect-entry-relations)))
                          (when entry-relations
                            (setq pending (nconc pending entry-relations))
                            (setq relation-count (+ relation-count (length entry-relations))))))
                      (setq processed (1+ processed))
                      (outline-next-heading)))
                  (setq pos (point)))))
              (when (and pending (>= (length pending) arxana-org-links-batch-size))
                (funcall flush-fn))
              (if done
                  (progn
                    (funcall flush-fn)
                    (when (and (= in-flight 0) (null pending))
                      (message "Persisted %d pattern link(s) in buffer" relation-count)))
                (message "Persisting links… %d headings scanned, %d relations"
                         processed relation-count)
                (run-with-idle-timer arxana-org-links-idle-delay nil step-fn))))
    (run-with-idle-timer arxana-org-links-idle-delay nil step-fn)))

;;;###autoload
(defun arxana-org-links-persist-buffer-patterns (&optional sync)
  "Persist pattern associations for entries in the current Org buffer.
With SYNC non-nil, run the blocking version."
  (interactive "P")
  (if sync
      (arxana-org-links-persist-buffer-patterns-sync)
    (arxana-org-links-persist-buffer-patterns-async)))

;;; Pattern hover support

(defun arxana-org-links--pattern-at-pos (pos)
  (get-text-property pos 'arxana-pattern-id))

(defun arxana-org-links-open-pattern-at-point (&optional _event)
  "Open the pattern referenced at point."
  (interactive)
  (let ((pattern (arxana-org-links--pattern-at-pos (point))))
    (unless (and pattern (stringp pattern))
      (user-error "No pattern at point"))
    (arxana-org-links--show-pattern pattern)))

(defun arxana-org-links--show-pattern (pattern)
  (require 'arxana-browser-patterns nil t)
  (unless (fboundp 'arxana-browser-patterns-open)
    (user-error "Pattern browser is not available"))
  (let* ((origin (selected-window))
         (buffer (format "*Arxana Pattern: %s*" pattern))
         (display-buffer-alist
          (cons
           `(,(regexp-quote buffer)
             (display-buffer-in-side-window)
             (side . right)
             (slot . 0)
             (window-width . ,arxana-org-links-hover-window-width)
             (window-parameters . ((no-delete-other-windows . t))))
           display-buffer-alist)))
    (save-selected-window
      (arxana-browser-patterns-open pattern))
    (setq arxana-org-links--hover-window
          (get-buffer-window buffer t))
    (when (fboundp 'arxana-window-constraints-validate-org-hover)
      (arxana-window-constraints-validate-org-hover
       arxana-org-links--hover-window))
    (when (window-live-p origin)
      (select-window origin))))

(defun arxana-org-links--schedule-hover (pattern)
  (when (timerp arxana-org-links--hover-timer)
    (cancel-timer arxana-org-links--hover-timer))
  (setq arxana-org-links--hover-timer
        (run-with-idle-timer
         arxana-org-links-hover-delay nil
         (lambda ()
           (when (and pattern (stringp pattern))
             (setq arxana-org-links--hover-pattern pattern)
             (arxana-org-links--show-pattern pattern))))))

(defun arxana-org-links--line-pattern ()
  (let ((line (buffer-substring-no-properties
               (line-beginning-position)
               (line-end-position))))
    (when (string-match arxana-org-links--pattern-prop-regexp line)
      (arxana-org-links--string-trim (match-string 1 line)))))

(defun arxana-org-links--maybe-propify-line ()
  (when arxana-org-links--pattern-overlays
    (mapc #'delete-overlay arxana-org-links--pattern-overlays)
    (setq arxana-org-links--pattern-overlays nil))
  (let ((pattern (arxana-org-links--line-pattern)))
    (when pattern
      (save-excursion
        (beginning-of-line)
        (when (re-search-forward arxana-org-links--pattern-prop-regexp
                                 (line-end-position) t)
          (let ((start (match-beginning 1))
                (end (match-end 1))
                (line-start (line-beginning-position))
                (line-end (line-end-position)))
            (add-text-properties
             start end
             (list 'arxana-pattern-id pattern
                   'mouse-face 'highlight
                   'help-echo (format "Open pattern: %s" pattern)
                   'keymap arxana-org-links--pattern-keymap
                   'face '(org-property-value underline)
                   'font-lock-face '(org-property-value underline)))
            (add-text-properties
             line-start line-end
             (list 'arxana-pattern-id pattern
                   'mouse-face 'highlight
                   'help-echo (format "Open pattern: %s" pattern)
                   'keymap arxana-org-links--pattern-keymap))
            (let ((next-end (min (point-max) (1+ line-end))))
              (when (> next-end line-end)
                (add-text-properties
                 line-end next-end
                 (list 'arxana-pattern-id pattern
                       'mouse-face 'highlight
                       'help-echo (format "Open pattern: %s" pattern)
                       'keymap arxana-org-links--pattern-keymap))))
            (add-face-text-property start end 'link t)
            (when (and start end (> end start))
              (let ((ov (make-overlay start end nil t nil)))
                (overlay-put ov 'arxana-pattern-overlay t)
                (overlay-put ov 'face 'underline)
                (overlay-put ov 'mouse-face 'highlight)
                (overlay-put ov 'help-echo (format "Open pattern: %s" pattern))
                (overlay-put ov 'keymap arxana-org-links--pattern-keymap)
                (overlay-put ov 'priority 999)
                (push ov arxana-org-links--pattern-overlays)))))))))

(defun arxana-org-links--maybe-hover ()
  (let* ((mouse (mouse-position))
         (frame (and (consp mouse) (car mouse)))
         (xy (and (consp mouse) (consp (cdr mouse)) (cdr mouse)))
         (x (and (consp xy) (car xy)))
         (y (and (consp xy) (cdr xy)))
         (posn (and frame x y (posn-at-x-y x y frame)))
         (win (and posn (posn-window posn)))
         (pos (and posn (posn-point posn))))
    (if (and pos win (eq win (selected-window)))
        (let ((pattern (arxana-org-links--pattern-at-pos pos)))
          (when (and pattern (not (equal pattern arxana-org-links--hover-pattern)))
            (arxana-org-links--schedule-hover pattern)))
      (arxana-org-links--maybe-propify-line)
      (let ((pattern (or (arxana-org-links--pattern-at-pos (point))
                         (arxana-org-links--line-pattern))))
        (if (and pattern (not (equal pattern arxana-org-links--hover-pattern)))
            (arxana-org-links--schedule-hover pattern)
          (setq arxana-org-links--hover-pattern nil))))))

(defun arxana-org-links--fontify-patterns (limit)
  (when (re-search-forward arxana-org-links--pattern-prop-regexp limit t)
    (let* ((start (match-beginning 1))
           (end (match-end 1))
           (pattern (arxana-org-links--string-trim (match-string 1))))
      (when (and pattern (> (length pattern) 0))
        (add-text-properties
         start end
         (list 'arxana-pattern-id pattern
               'mouse-face 'highlight
               'help-echo (format "Open pattern: %s" pattern)
               'keymap arxana-org-links--pattern-keymap))
        (add-face-text-property start end 'link t))
      t)))

(define-minor-mode arxana-org-links-pattern-hover-mode
  "Enable hover previews for PATTERN properties in Org buffers."
  :lighter " Arxana-Hover"
  (if arxana-org-links-pattern-hover-mode
      (progn
        (font-lock-add-keywords nil '((arxana-org-links--fontify-patterns)) 'append)
        (font-lock-flush)
        (add-hook 'post-command-hook #'arxana-org-links--maybe-hover nil t))
    (remove-hook 'post-command-hook #'arxana-org-links--maybe-hover t)
    (font-lock-remove-keywords nil '((arxana-org-links--fontify-patterns)))
    (font-lock-flush)))

(defun arxana-org-links--maybe-enable-hover ()
  (when (and arxana-org-links-enable-hover (derived-mode-p 'org-mode))
    (arxana-org-links-pattern-hover-mode 1)))

(add-hook 'org-mode-hook #'arxana-org-links--maybe-enable-hover)

(provide 'arxana-org-links)

;;; arxana-org-links.el ends here
