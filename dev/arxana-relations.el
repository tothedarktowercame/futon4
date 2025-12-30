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

(defun arxana-relations--get (key data)
  (let ((value
         (cond
          ((and (consp data)
                (keywordp (car data))
                (not (listp (cdr data))))
           (alist-get key (list data)))
          ((and (listp data) (consp (car data)))
           (alist-get key data))
          ((listp data)
           (plist-get data key))
          (t nil))))
    (if (and (listp value)
             (= (length value) 1)
             (let ((inner (car value)))
               (or (not (listp inner))
                   (and (listp inner) (keywordp (car inner))))))
        (car value)
      value)))

(defun arxana-relations--entity-name (entity)
  (or (arxana-relations--get :entity/name entity)
      (arxana-relations--get :name entity)))

(defun arxana-relations--entity-type (entity)
  (or (arxana-relations--get :entity/type entity)
      (arxana-relations--get :type entity)))

(defun arxana-relations--insert-line (fmt &rest args)
  (insert (apply #'format (concat fmt "\n") args)))

(defun arxana-relations--insert-section (title lines)
  (arxana-relations--insert-line "%s" title)
  (if (seq-empty-p lines)
      (arxana-relations--insert-line "  (none)")
    (dolist (line lines)
      (arxana-relations--insert-line "  - %s" line))))

(defun arxana-relations--render-ego (body)
  (let* ((ego (arxana-relations--get :ego body))
         (entity (and ego (arxana-relations--get :entity ego)))
         (name (arxana-relations--entity-name entity))
         (links (arxana-relations--get :links ego))
         (outgoing '())
         (incoming '()))
    (dolist (link links)
      (let* ((direction (arxana-relations--get :direction link))
             (rel (arxana-relations--get :relation link))
             (target (arxana-relations--get :entity link))
             (label (arxana-relations--name-with-type
                     (arxana-relations--entity-name target)
                     (arxana-relations--entity-type target))))
        (when label
          (if (eq direction :out)
              (push (format "%s via %s" label (arxana-relations--type-label rel)) outgoing)
            (when (eq direction :in)
              (push (format "%s via %s" label (arxana-relations--type-label rel)) incoming))))))
    (when name
      (arxana-relations--insert-line "Ego: %s" name)
      (arxana-relations--insert-line ""))
    (arxana-relations--insert-section "Outgoing" (nreverse outgoing))
    (arxana-relations--insert-section "Incoming" (nreverse incoming))))

(defun arxana-relations--render-cooccur (body)
  (let* ((cooccur (arxana-relations--get :cooccur body))
         (rows (arxana-relations--get :rows cooccur))
         (lines (mapcar (lambda (row)
                          (format "%s (%s)"
                                  (or (arxana-relations--get :name row) "?")
                                  (or (arxana-relations--get :count row) 0)))
                        rows)))
    (arxana-relations--insert-section "Co-occurrences" lines)))

(defun arxana-relations--render-tail (body)
  (let* ((rels (arxana-relations--get :relations body))
         (lines (mapcar (lambda (rel)
                          (let* ((src (arxana-relations--get :src rel))
                                 (dst (arxana-relations--get :dst rel))
                                 (src-name (arxana-relations--entity-name src))
                                 (dst-name (arxana-relations--entity-name dst)))
                            (format "%s → %s" (or src-name "?") (or dst-name "?"))))
                        rels)))
    (arxana-relations--insert-section "Recent relations" lines)))

(defun arxana-relations--with-buffer (title renderer body &optional context)
  (let ((buf (get-buffer-create arxana-relations-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (arxana-relations-buffer-mode 1)
        (setq arxana-relations-context context)
        (when title
          (arxana-relations--insert-line "%s" title)
          (arxana-relations--insert-line ""))
        (if (functionp renderer)
            (funcall renderer body)
          (arxana-relations--render-fallback body))
        (goto-char (point-min))))
    (display-buffer buf)))

(defun arxana-relations--revert-buffer (_ignore-auto _noconfirm)
  (arxana-relations-refresh))

(defun arxana-relations-show-ego (name &optional limit)
  "Render ego relations for NAME."
  (interactive (list (read-string "Ego name: ")))
  (let* ((limit (or limit 10))
         (body (arxana-store-ego name limit))
         (futon-id (and (fboundp 'futon4-lookup-article-id)
                        (futon4-lookup-article-id name)))
         (context (list :endpoint :ego :name name :limit limit :id futon-id)))
    (arxana-relations--with-buffer "Ego" #'arxana-relations--render-ego body context)
    (with-current-buffer arxana-relations-buffer-name
      (setq header-line-format (if futon-id
                                   (format "Futon: %s" futon-id)
                                 "Futon: (unknown)")))))

(defun arxana-relations-refresh ()
  "Refresh the current relations buffer."
  (interactive)
  (let* ((context arxana-relations-context)
         (endpoint (plist-get context :endpoint)))
    (pcase endpoint
      (:ego (arxana-relations-show-ego (plist-get context :name)
                                      (plist-get context :limit)))
      (_ (message "No refresh handler for %s" endpoint)))))

(defun arxana-relations-copy-id ()
  "Copy the current Futon id to the kill ring."
  (interactive)
  (let ((futon-id (plist-get arxana-relations-context :id)))
    (if futon-id
        (progn
          (kill-new futon-id)
          (message "Copied %s" futon-id))
      (user-error "No Futon id available"))))

(defun arxana-relations--render-fallback (body)
  (arxana-relations--insert-line "Raw response:")
  (pp body (current-buffer)))

;; ... remaining functions from dev/arxana-relations.el ...

(provide 'arxana-relations)

;;; arxana-relations.el ends here
