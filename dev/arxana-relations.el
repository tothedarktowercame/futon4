;;; arxana-relations.el --- Relation browsing helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive commands that render Futon relation data (/ego, /cooccur, /tail)
;; inside Emacs so users no longer need to shell out to curl.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'arxana-store)

(declare-function futon4-lookup-article-id "arxana-tangled" (name))
(declare-function futon4--article-id-for "arxana-tangled" (name &optional path))

(defgroup arxana-relations nil
  "Buffers for browsing Futon relation data."
  :group 'arxana)

(defconst arxana-relations-buffer-name "*Arxana Relations*")

(defvar-local arxana-relations-context nil
  "Plist describing the last Futon render in this buffer.")

(defvar arxana-relations-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'arxana-relations-refresh)
    (define-key map (kbd "p") #'arxana-relations-provenance)
    (define-key map (kbd "y") #'arxana-relations-copy-id)
    map)
  "Keymap for `arxana-relations-buffer-mode'.")

(define-minor-mode arxana-relations-buffer-mode
  "Minor mode that adds refresh/provenance commands to relation buffers."
  :lighter " ArxRel"
  :keymap arxana-relations-buffer-mode-map
  (if arxana-relations-buffer-mode
      (setq-local revert-buffer-function #'arxana-relations--revert-buffer)
    (kill-local-variable 'revert-buffer-function)))

(defun arxana-relations--type-label (type)
  (cond
   ((keywordp type) (substring (symbol-name type) 1))
   ((symbolp type) (symbol-name type))
   ((stringp type) type)
   (t (format "%s" type))))

(defun arxana-relations--name-with-type (name type)
  (if (and name type)
      (format "%s (%s)" name (arxana-relations--type-label type))
    (or name "?")))

(defun arxana-relations--entity-name (entity)
  (or (alist-get :entity/name entity)
      (alist-get :name entity)))

(defun arxana-relations--entity-type (entity)
  (or (alist-get :entity/type entity)
      (alist-get :type entity)))

(defun arxana-relations--relation-label (rel)
  (arxana-relations--type-label (or rel "?")))

(defun arxana-relations--normalize-direction (dir)
  (cond
   ((keywordp dir) dir)
   ((stringp dir) (intern (concat ":" (downcase dir))))
   (t dir)))

(defun arxana-relations--insert-line (fmt &rest args)
  (insert (apply #'format (concat fmt "\n") args)))

(defun arxana-relations--insert-section (title lines)
  (arxana-relations--insert-line "%s" title)
  (if (seq-empty-p lines)
      (arxana-relations--insert-line "  (none)")
    (dolist (line lines)
      (arxana-relations--insert-line "  - %s" line))))

(defun arxana-relations--render-ego (body)
  (let* ((section (or (alist-get :ego body) body))
         (focal (or (alist-get :entity section) section))
         (links (or (alist-get :links section)
                    (alist-get :neighbors section)
                    (and (listp section) section)))
         (focus-name (arxana-relations--name-with-type (or (alist-get :name focal)
                                                           (arxana-relations--entity-name focal))
                                                       (or (alist-get :type focal)
                                                           (arxana-relations--entity-type focal))))
         (format-link (lambda (link)
                        (let* ((neighbor (or (alist-get :entity link)
                                             (alist-get :dst link)
                                             (alist-get :src link)))
                               (neighbor-name (arxana-relations--name-with-type
                                               (arxana-relations--entity-name neighbor)
                                               (arxana-relations--entity-type neighbor)))
                               (direction (arxana-relations--normalize-direction
                                           (alist-get :direction link)))
                               (relation (arxana-relations--relation-label
                                          (or (alist-get :relation link)
                                              (alist-get :type link)))))
                          (if (eq direction :in)
                              (format "%s —%s→ %s" neighbor-name relation focus-name)
                            (format "%s —%s→ %s" focus-name relation neighbor-name)))))
         (outgoing (seq-filter (lambda (link)
                                 (eq (arxana-relations--normalize-direction
                                      (alist-get :direction link))
                                     :out))
                               links))
         (incoming (seq-filter (lambda (link)
                                 (eq (arxana-relations--normalize-direction
                                      (alist-get :direction link))
                                     :in))
                               links))
         (others (seq-remove (lambda (link)
                               (memq link (append outgoing incoming)))
                             links)))
    (arxana-relations--insert-line "Entity: %s" (or focus-name "?"))
    (arxana-relations--insert-section "Outgoing:" (mapcar format-link outgoing))
    (arxana-relations--insert-section "Incoming:" (mapcar format-link incoming))
    (when (seq-some #'identity others)
      (arxana-relations--insert-section "Other links:" (mapcar format-link others)))))

(defun arxana-relations--render-cooccur (body)
  (let* ((section (or (alist-get :cooccur body) body))
         (entity (or (alist-get :entity section) section))
         (rows (or (alist-get :rows section)
                   (alist-get :entities section)
                   (alist-get :items section)
                   (alist-get :cooccurrences section)
                   (and (listp section) section)))
         (header (arxana-relations--name-with-type (arxana-relations--entity-name entity)
                                                   (arxana-relations--entity-type entity)))
         (lines (mapcar (lambda (row)
                          (let ((name (arxana-relations--name-with-type (alist-get :name row)
                                                                         (alist-get :type row)))
                                (count (or (alist-get :count row)
                                           (alist-get :weight row))))
                            (if count
                                (format "%s (%s)" name count)
                              name)))
                        rows)))
    (arxana-relations--insert-line "Co-occurrences with %s:" (or header "?"))
    (if (seq-empty-p lines)
        (arxana-relations--insert-line "  (none)")
      (dolist (line lines)
        (arxana-relations--insert-line "  - %s" line)))))

(defun arxana-relations--confidence-string (value)
  (when (numberp value)
    (format "conf %.2f" (float value))))

(defun arxana-relations--render-tail (body)
  (let* ((section (or (alist-get :tail body) body))
         (rows (or (alist-get :relations section)
                   (alist-get :links section)
                   (alist-get :items section)
                   (and (listp section) section)))
         (format-row (lambda (row)
                       (let* ((src (or (alist-get :src row) (alist-get :source row)))
                              (dst (or (alist-get :dst row) (alist-get :target row)))
                              (relation (arxana-relations--relation-label (alist-get :type row)))
                              (confidence (arxana-relations--confidence-string
                                           (alist-get :confidence row)))
                              (last-seen (alist-get :last-seen row))
                              (extras (delq nil (list confidence
                                                      (when last-seen
                                                        (format "seen %s" last-seen))))))
                         (concat (format "%s —%s→ %s"
                                         (arxana-relations--name-with-type
                                          (arxana-relations--entity-name src)
                                          (arxana-relations--entity-type src))
                                         relation
                                         (arxana-relations--name-with-type
                                          (arxana-relations--entity-name dst)
                                          (arxana-relations--entity-type dst)))
                                 (when extras
                                   (format " (%s)" (string-join extras ", "))))))))
    (arxana-relations--insert-section "Recent relations:" (mapcar format-row rows))))

(defun arxana-relations--render-fallback (body)
  (arxana-relations--insert-line "Raw response:")
  (pp body (current-buffer)))

(defun arxana-relations--article-id (name)
  (or (and name (fboundp 'futon4-lookup-article-id)
           (futon4-lookup-article-id name))
      (and name (fboundp 'futon4--article-id-for)
           (futon4--article-id-for name))))

(defun arxana-relations--prepare-context (context)
  (when context
    (let ((ctx (copy-sequence context)))
      (when-let ((name (plist-get ctx :name)))
        (setq ctx (plist-put ctx :id (arxana-relations--article-id name))))
      ctx)))

(defun arxana-relations--finalize-context (context title renderer)
  (when context
    (let ((ctx (arxana-relations--prepare-context context)))
      (setq ctx (plist-put ctx :title title))
      (setq ctx (plist-put ctx :renderer renderer))
      (setq ctx (plist-put ctx :fetched-at (current-time)))
      ctx)))

(defun arxana-relations--base-context (context)
  (let ((copy (copy-sequence context)))
    (dolist (key '(:title :renderer :fetched-at))
      (cl-remf copy key))
    copy))

(defun arxana-relations--header-line (context)
  (when context
    (let* ((endpoint (plist-get context :endpoint))
           (limit (plist-get context :limit))
           (name (plist-get context :name))
           (id (plist-get context :id))
           (fetched (plist-get context :fetched-at))
           (label (if endpoint (upcase (symbol-name endpoint)) "REL"))
           (timestamp (and fetched (format-time-string "%Y-%m-%d %H:%M" fetched))))
      (string-join
       (delq nil
             (list
              (if name
                  (format "[%s • %s]" label name)
                (format "[%s]" label))
              (format "Futon: %s" (or id "<unsynced>"))
              (format "Limit: %s" (or limit "–"))
              (when timestamp (format "Fetched %s" timestamp))
              "g refresh · p provenance · y copy id"))
       "   "))))

(defun arxana-relations--ensure-context ()
  (unless (bound-and-true-p arxana-relations-buffer-mode)
    (user-error "Not inside an Arxana relations buffer"))
  (unless arxana-relations-context
    (user-error "No Futon context recorded for this buffer"))
  arxana-relations-context)

(defun arxana-relations--revert-buffer (_ignore-auto _noconfirm)
  (arxana-relations-refresh))

(defun arxana-relations-refresh ()
  "Re-fetch the Futon payload for the current relations buffer."
  (interactive)
  (let* ((ctx (arxana-relations--ensure-context))
         (fetch (plist-get ctx :fetch))
         (renderer (plist-get ctx :renderer))
         (title (plist-get ctx :title))
         (base (arxana-relations--base-context ctx)))
    (unless fetch
      (user-error "This buffer cannot be refreshed (no fetch function recorded)"))
    (let* ((updated-base (arxana-relations--prepare-context base))
           (body (funcall fetch)))
      (arxana-relations--with-buffer title renderer body updated-base))))

(defun arxana-relations-provenance ()
  "Display the Futon endpoint + entity id backing the current buffer."
  (interactive)
  (let* ((ctx (arxana-relations--ensure-context))
         (endpoint (plist-get ctx :endpoint))
         (name (plist-get ctx :name))
         (limit (plist-get ctx :limit))
         (id (plist-get ctx :id))
         (fetched (plist-get ctx :fetched-at))
         (timestamp (and fetched (format-time-string "%Y-%m-%d %H:%M:%S" fetched))))
    (message "[%s] %s (limit %s) id=%s%s"
             (if endpoint (upcase (symbol-name endpoint)) "REL")
             (or name "recent tail")
             (or limit "–")
             (or id "<unsynced>")
             (if timestamp (format " fetched %s" timestamp) ""))))

(defun arxana-relations-copy-id ()
  "Copy the Futon entity id associated with the current buffer."
  (interactive)
  (let* ((ctx (arxana-relations--ensure-context))
         (id (plist-get ctx :id)))
    (unless id
      (user-error "This buffer is not tied to a Futon entity id"))
    (kill-new id)
    (message "Copied Futon id: %s" id)))

(defun arxana-relations--with-buffer (title renderer body context)
  (let ((buffer (get-buffer-create arxana-relations-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s\n\n" title))
        (if body
            (condition-case err
                (funcall renderer body)
              (error
               (arxana-relations--insert-line "Unable to format response (%s)." err)
               (arxana-relations--render-fallback body)))
          (arxana-relations--insert-line "No data returned from Futon."))
        (goto-char (point-min))
        (view-mode 1)
        (arxana-relations-buffer-mode 1)
        (if context
            (progn
              (setq-local arxana-relations-context
                          (arxana-relations--finalize-context context title renderer))
              (setq header-line-format
                    (arxana-relations--header-line arxana-relations-context)))
          (setq-local arxana-relations-context nil)
          (setq header-line-format nil))))
    (display-buffer buffer)))

;;;###autoload
(defun arxana-relations-show-ego (name &optional limit)
  "Display Futon /ego data for NAME with optional LIMIT."
  (interactive
   (list (read-string "Ego entity: "
                      (or (and (boundp 'name-of-current-article)
                               name-of-current-article)
                          (buffer-name)))
         (read-number "Limit: " 15)))
  (let ((body (arxana-store-ego name limit)))
    (arxana-relations--with-buffer (format "Ego: %s" name)
                                   #'arxana-relations--render-ego
                                   body
                                   (list :endpoint 'ego
                                         :name name
                                         :limit limit
                                         :fetch (lambda () (arxana-store-ego name limit))))))

;;;###autoload
(defun arxana-relations-show-cooccur (name &optional limit)
  "Display Futon /cooccur data for NAME with optional LIMIT."
  (interactive
   (list (read-string "Cooccur entity: "
                      (or (and (boundp 'name-of-current-article)
                               name-of-current-article)
                          (buffer-name)))
         (read-number "Limit: " 10)))
  (let ((body (arxana-store-cooccur name limit)))
    (arxana-relations--with-buffer (format "Co-occur: %s" name)
                                   #'arxana-relations--render-cooccur
                                   body
                                   (list :endpoint 'cooccur
                                         :name name
                                         :limit limit
                                         :fetch (lambda ()
                                                  (arxana-store-cooccur name limit))))))

;;;###autoload
(defun arxana-relations-show-tail (&optional limit)
  "Display Futon /tail data, showing LIMIT relations (default 5)."
  (interactive (list (read-number "Tail limit: " 5)))
  (let ((body (arxana-store-tail limit)))
    (arxana-relations--with-buffer (format "Tail (last %s relations)" (or limit 5))
                                   #'arxana-relations--render-tail
                                   body
                                   (list :endpoint 'tail
                                         :limit limit
                                         :fetch (lambda () (arxana-store-tail limit))))))

(provide 'arxana-relations)

;;; arxana-relations.el ends here
