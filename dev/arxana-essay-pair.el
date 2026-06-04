;;; arxana-essay-pair.el --- Side-by-side compiled essay pair view -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'arxana-store)
(require 'arxana-browser-essays nil t)
(require 'arxana-browser-essays-compiled nil t)

(defgroup arxana-essay-pair nil
  "Side-by-side compiled essay pair views."
  :group 'arxana)

(defcustom arxana-essay-pair-right-width 0.45
  "Fractional width used for the right essay pane."
  :type 'number
  :group 'arxana-essay-pair)

(defface arxana-essay-pair-own-left-face
  '((t (:inherit highlight :extend t)))
  "Face for the left essay's own annotations."
  :group 'arxana-essay-pair)

(defface arxana-essay-pair-own-right-face
  '((t (:inherit lazy-highlight :extend t)))
  "Face for the right essay's own annotations."
  :group 'arxana-essay-pair)

(defface arxana-essay-pair-bind-face
  '((t (:inherit match :extend t)))
  "Face for cross-version projection binds."
  :group 'arxana-essay-pair)

(defface arxana-essay-pair-orphan-face
  '((t (:inherit warning :extend t)))
  "Face for source-only projection orphans."
  :group 'arxana-essay-pair)

(defface arxana-essay-pair-new-slot-face
  '((t (:inherit success :extend t)))
  "Face for target-only new slots."
  :group 'arxana-essay-pair)

(defface arxana-essay-pair-active-face
  '((t (:inherit isearch :extend t)))
  "Face for the currently selected projection."
  :group 'arxana-essay-pair)

(defvar arxana-essay-pair-last-summary nil
  "Summary plist from the most recent `arxana-essay-side-by-side' render.")

(defvar arxana-essay-pair--projection-overlays nil)
(defvar arxana-essay-pair--active-overlays nil)

(defvar-local arxana-essay-pair--essay-id nil)
(defvar-local arxana-essay-pair--other-buffer nil)
(defvar-local arxana-essay-pair--last-active-projection nil)

(defconst arxana-essay-pair--source-fallbacks
  '(("arxana/essay/anthropic-fellows-2026-v1" .
     "/home/joe/code/futon5a/essays/anthropic-fellows-2026/anthropic-fellows-2026-v1.md")
    ("arxana/essay/anthropic-fellows-2026-form-fill-v2" .
     "/home/joe/code/futon5a/essays/anthropic-fellows-2026/anthropic-fellows-2026-form-fill-v2.md")
    ("arxana/essay/glasgow-cogito-neurotech-RA-formal-application-v1" .
     "/home/joe/code/futon5a/essays/glasgow-cogito-neurotech-RA-formal-application/glasgow-cogito-neurotech-RA-formal-application-v1.md")
    ("arxana/essay/glasgow-cogito-cover-letter-final" .
     "/home/joe/code/futon5a/essays/glasgow-cogito-neurotech-RA-formal-application/glasgow-cogito-neurotech-RA-cover-letter-final.md")))

(defconst arxana-essay-pair--projection-types
  '("arxana/eoi-projection-bind" "arxana/eoi-orphan" "arxana/eoi-new-slot"))

(defvar arxana-essay-pair-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'arxana-essay-pair-activate-at-point)
    (define-key map [mouse-1] #'arxana-essay-pair-mouse-activate)
    (define-key map (kbd "g") #'arxana-essay-pair-refresh)
    map))

(define-derived-mode arxana-essay-pair-mode text-mode "Arxana Essay Pair"
  "Mode for compiled side-by-side essay-pair panes."
  (setq buffer-read-only t)
  (setq-local truncate-lines nil)
  (visual-line-mode 1)
  (add-hook 'post-command-hook #'arxana-essay-pair--post-command nil t))

(defun arxana-essay-pair--get (obj key)
  (cond
   ((null obj) nil)
   ((and (listp obj) (plist-member obj key)) (plist-get obj key))
   ((and (listp obj) (assq key obj)) (cdr (assq key obj)))
   ((and (keywordp key) (listp obj)
         (assq (intern (substring (symbol-name key) 1)) obj))
    (cdr (assq (intern (substring (symbol-name key) 1)) obj)))
   (t nil)))

(defun arxana-essay-pair--string (value)
  (cond
   ((null value) nil)
   ((stringp value) value)
   ((keywordp value) (substring (symbol-name value) 1))
   ((symbolp value) (symbol-name value))
   (t (format "%s" value))))

(defun arxana-essay-pair--props (hx)
  (or (arxana-essay-pair--get hx :hx/props)
      (arxana-essay-pair--get hx :props)))

(defun arxana-essay-pair--prop (hx key)
  (let ((props (arxana-essay-pair--props hx)))
    (or (arxana-essay-pair--get props key)
        (arxana-essay-pair--get props (intern (format ":%s" key))))))

(defun arxana-essay-pair--hx-id (hx)
  (arxana-essay-pair--get hx :hx/id))

(defun arxana-essay-pair--hx-type (hx)
  (arxana-essay-pair--get hx :hx/type))

(defun arxana-essay-pair--hx-ends (hx)
  (or (arxana-essay-pair--get hx :hx/ends)
      (arxana-essay-pair--get hx :hx/endpoints)))

(defun arxana-essay-pair--response-hyperedges (response)
  (let ((h (arxana-essay-pair--get response :hyperedges)))
    (cond
     ((and (listp response) (arxana-essay-pair--get response :hx/id)) (list response))
     ((listp h) h)
     (t nil))))

(defun arxana-essay-pair--entity-id (entity-response)
  (or (arxana-essay-pair--get entity-response :id)
      (arxana-essay-pair--get (arxana-essay-pair--get entity-response :entity) :id)))

(defun arxana-essay-pair--entity-name (entity-response)
  (or (arxana-essay-pair--get entity-response :name)
      (arxana-essay-pair--get (arxana-essay-pair--get entity-response :entity) :name)))

(defun arxana-essay-pair--entity-props (entity-response)
  (or (arxana-essay-pair--get entity-response :props)
      (arxana-essay-pair--get (arxana-essay-pair--get entity-response :entity) :props)))

(defun arxana-essay-pair--essay-source-file (essay-id)
  (let* ((entity (ignore-errors (arxana-store-fetch-entity essay-id)))
         (props (arxana-essay-pair--entity-props entity))
         (remote (arxana-essay-pair--get props :source-file))
         (remote-file (and remote (expand-file-name remote)))
         (fallback (cdr (assoc essay-id arxana-essay-pair--source-fallbacks))))
    (cond
     ((and remote-file (file-readable-p remote-file)) remote-file)
     ((and fallback (file-readable-p fallback)) fallback)
     ((and (fboundp 'arxana-browser-essays--resolve-source-file)
           (ignore-errors (arxana-browser-essays--resolve-source-file essay-id))))
     (t (user-error "No readable source file for %s (store source-file=%S)" essay-id remote)))))

(defun arxana-essay-pair--entity-heading-candidates (entity-id)
  (let* ((entity (ignore-errors (arxana-store-fetch-entity entity-id)))
         (name (arxana-essay-pair--entity-name entity))
         (props (arxana-essay-pair--entity-props entity))
         (heading (arxana-essay-pair--get props :heading-text))
         (slot (and (string-match "/slot/\\([^/]+\\)\\'" entity-id)
                    (match-string 1 entity-id)))
         (section (and (string-match "/section/\\([^/]+\\)\\'" entity-id)
                       (match-string 1 entity-id)))
         (slot-title (and slot
                          (mapconcat #'capitalize (split-string slot "-" t) " ")))
         (slot-fallback
          (pcase slot
            ("why-interested" "Why interested")
            ("research-areas" "Research areas excited")
            ("stream" "Streams — top choice")
            ("relevant-background" "Relevant background")
            ("accept-ft-pct" "Accept a full-time offer")
            ("continue-pct" "Continue in the stream")
            ("references" "References")
            ("logistics" "Workspace")
            ("anything-else" "Anything else")
            (_ nil))))
    (delq nil (list name heading slot-fallback slot-title slot section))))

(defun arxana-essay-pair--find-text-bounds (text)
  (when (and text (stringp text) (not (string-empty-p text)))
    (save-excursion
      (goto-char (point-min))
      (or (when (search-forward text nil t)
            (cons (match-beginning 0) (match-end 0)))
          (when (and (fboundp 'arxana-browser-essays-compiled--locate-near-suffix)
                     (> (length text) 12))
            (let ((near (arxana-browser-essays-compiled--locate-near-suffix text)))
              (when near (cons (nth 0 near) (nth 1 near)))))))))

(defun arxana-essay-pair--place-text-overlay (text face &rest props)
  (let ((bounds (arxana-essay-pair--find-text-bounds text)))
    (when bounds
      (let ((ov (make-overlay (car bounds) (cdr bounds))))
        (overlay-put ov 'face face)
        (overlay-put ov 'help-echo (plist-get props :help-echo))
        (while props
          (overlay-put ov (pop props) (pop props)))
        ov))))

(defun arxana-essay-pair--place-anchor-overlay (entity-id passage face &rest props)
  (let ((ov nil))
    (setq ov (and passage
                  (apply #'arxana-essay-pair--place-text-overlay passage face props)))
    (unless ov
      (cl-dolist (candidate (arxana-essay-pair--entity-heading-candidates entity-id))
        (setq ov (apply #'arxana-essay-pair--place-text-overlay candidate face props))
        (when ov (cl-return ov))))
    ov))

(defun arxana-essay-pair--own-annotations (essay-id source-file)
  (if (and (fboundp 'arxana-browser-essays-compiled--load-annotations)
           source-file)
      (ignore-errors
        (arxana-browser-essays-compiled--load-annotations essay-id source-file))
    nil))

(defun arxana-essay-pair--place-own-annotations (annotations face)
  (let ((count 0))
    (dolist (ann annotations)
      (let* ((passages (and (fboundp 'arxana-browser-essays-compiled--annotation-passages)
                            (arxana-browser-essays-compiled--annotation-passages ann)))
             (id (or (plist-get ann :id) (plist-get ann :label)))
             (note (or (plist-get ann :note) (plist-get ann :summary) "")))
        (dolist (passage passages)
          (when (arxana-essay-pair--place-text-overlay
                 passage face
                 'arxana-essay-pair-layer 'own
                 'help-echo (format "Own annotation %s\n%s" (or id "") note))
            (setq count (1+ count))))))
    count))

(defun arxana-essay-pair--fetch-projection-hyperedges (left-id right-id)
  (let (out)
    (dolist (type arxana-essay-pair--projection-types)
      (dolist (hx (arxana-essay-pair--response-hyperedges
                   (arxana-store-fetch-hyperedges :type type :limit 500)))
        (let ((ids (mapcar (lambda (ep) (arxana-essay-pair--get ep :entity-id))
                           (arxana-essay-pair--hx-ends hx))))
          (when (and (cl-some (lambda (id) (and id (string-prefix-p left-id id))) ids)
                     (cl-some (lambda (id) (and id (string-prefix-p right-id id))) ids))
            (push hx out))
          (when (and (string= type "arxana/eoi-orphan")
                     (cl-some (lambda (id) (and id (string-prefix-p left-id id))) ids))
            (push hx out))
          (when (and (string= type "arxana/eoi-new-slot")
                     (cl-some (lambda (id) (and id (string-prefix-p right-id id))) ids))
            (push hx out)))))
    (delete-dups (nreverse out))))

(defun arxana-essay-pair--end-by-role (hx role)
  (cl-find-if (lambda (ep) (string= (arxana-essay-pair--string (arxana-essay-pair--get ep :role)) role))
              (arxana-essay-pair--hx-ends hx)))

(defun arxana-essay-pair--end-for-essay (hx essay-id &optional role)
  (cl-find-if (lambda (ep)
                (and (let ((id (arxana-essay-pair--get ep :entity-id)))
                       (and id (string-prefix-p essay-id id)))
                     (or (not role)
                         (string= (arxana-essay-pair--string
                                   (arxana-essay-pair--get ep :role))
                                  role))))
              (arxana-essay-pair--hx-ends hx)))

(defun arxana-essay-pair--projection-face (hx)
  (pcase (arxana-essay-pair--hx-type hx)
    ("arxana/eoi-orphan" 'arxana-essay-pair-orphan-face)
    ("arxana/eoi-new-slot" 'arxana-essay-pair-new-slot-face)
    (_ 'arxana-essay-pair-bind-face)))

(defun arxana-essay-pair--projection-summary (hx)
  (let ((type (arxana-essay-pair--hx-type hx))
        (move (or (arxana-essay-pair--prop hx :move)
                  (arxana-essay-pair--prop hx :orphan-kind)
                  (arxana-essay-pair--prop hx :n-class)))
        (note (or (arxana-essay-pair--prop hx :note) "")))
    (string-trim (format "%s %s — %s" type (or move "") note))))

(defun arxana-essay-pair--remember-projection-overlay (pid ov)
  (when (and pid ov)
    (puthash pid (cons ov (gethash pid arxana-essay-pair--projection-overlays))
             arxana-essay-pair--projection-overlays)))

(defun arxana-essay-pair--place-projections (hxs side essay-id)
  (let ((count 0))
    (dolist (hx hxs)
      (let* ((pid (arxana-essay-pair--hx-id hx))
             (type (arxana-essay-pair--hx-type hx))
             (endpoint (cond
                        ((eq side 'left) (or (arxana-essay-pair--end-for-essay hx essay-id "source")
                                             (arxana-essay-pair--end-for-essay hx essay-id)))
                        ((eq side 'right) (or (arxana-essay-pair--end-for-essay hx essay-id "annotated")
                                              (arxana-essay-pair--end-for-essay hx essay-id))))))
        (when endpoint
          (let* ((entity-id (arxana-essay-pair--get endpoint :entity-id))
                 (passage (arxana-essay-pair--get endpoint :passage))
                 (help (arxana-essay-pair--projection-summary hx))
                 (face (arxana-essay-pair--projection-face hx))
                 (ov (arxana-essay-pair--place-anchor-overlay
                      entity-id passage face
                      'arxana-essay-pair-layer 'projection
                      'arxana-essay-pair-projection-id pid
                      'arxana-essay-pair-projection-type type
                      'help-echo help)))
            (when ov
              (arxana-essay-pair--remember-projection-overlay pid ov)
              (setq count (1+ count)))))))
    count))

(defun arxana-essay-pair--clear-active ()
  (mapc #'delete-overlay arxana-essay-pair--active-overlays)
  (setq arxana-essay-pair--active-overlays nil))

(defun arxana-essay-pair--highlight-projection (pid)
  (unless (equal pid arxana-essay-pair--last-active-projection)
    (setq arxana-essay-pair--last-active-projection pid)
    (arxana-essay-pair--clear-active)
    (when pid
      (dolist (ov (gethash pid arxana-essay-pair--projection-overlays))
        (when (overlay-buffer ov)
          (with-current-buffer (overlay-buffer ov)
            (let ((active (make-overlay (overlay-start ov) (overlay-end ov))))
              (overlay-put active 'face 'arxana-essay-pair-active-face)
              (overlay-put active 'priority 1000)
              (push active arxana-essay-pair--active-overlays)))))
      (let ((ovs (gethash pid arxana-essay-pair--projection-overlays)))
        (when ovs
          (message "[essay-pair] %s" (or (overlay-get (car ovs) 'help-echo) pid)))))))

(defun arxana-essay-pair--projection-id-at-point ()
  (cl-some (lambda (ov) (overlay-get ov 'arxana-essay-pair-projection-id))
           (overlays-at (point))))

(defun arxana-essay-pair--post-command ()
  (when (derived-mode-p 'arxana-essay-pair-mode)
    (arxana-essay-pair--highlight-projection
     (arxana-essay-pair--projection-id-at-point))))

(defun arxana-essay-pair-activate-at-point ()
  "Highlight the projection at point and jump to its counterpart when present."
  (interactive)
  (let* ((pid (arxana-essay-pair--projection-id-at-point))
         (ovs (and pid (gethash pid arxana-essay-pair--projection-overlays)))
         (other (cl-find-if (lambda (ov) (not (eq (overlay-buffer ov) (current-buffer)))) ovs)))
    (unless pid (user-error "No projection annotation at point"))
    (arxana-essay-pair--highlight-projection pid)
    (when other
      (pop-to-buffer (overlay-buffer other))
      (goto-char (overlay-start other)))))

(defun arxana-essay-pair-mouse-activate (event)
  "Mouse-activate projection at EVENT."
  (interactive "e")
  (mouse-set-point event)
  (arxana-essay-pair-activate-at-point))

(defun arxana-essay-pair-refresh ()
  "Re-render the current side-by-side essay pair."
  (interactive)
  (let ((summary arxana-essay-pair-last-summary))
    (unless summary (user-error "No essay-pair summary available"))
    (arxana-essay-side-by-side (plist-get summary :provenance-id)
                               (and (not (plist-get summary :provenance-id))
                                    (plist-get summary :right-id)))))

(defun arxana-essay-pair--provenance-pair (edge-id)
  (let* ((hx (arxana-store-fetch-hyperedge edge-id))
         (draft (or (arxana-essay-pair--end-by-role hx "draft")
                    (arxana-essay-pair--end-by-role hx "source")
                    (arxana-essay-pair--end-by-role hx "left")))
         (final (or (arxana-essay-pair--end-by-role hx "final")
                    (arxana-essay-pair--end-by-role hx "target")
                    (arxana-essay-pair--end-by-role hx "right")
                    (arxana-essay-pair--end-by-role hx "annotated"))))
    (unless (and draft final)
      (user-error "Could not resolve draft/final endpoints from %s" edge-id))
    (list :left-id (arxana-essay-pair--get draft :entity-id)
          :right-id (arxana-essay-pair--get final :entity-id)
          :provenance-id edge-id
          :provenance hx)))

(defun arxana-essay-pair--render-header (side essay-id provenance hxs)
  (let* ((props (and provenance (arxana-essay-pair--props provenance)))
         (redactions (arxana-essay-pair--get props :redactions))
         (deployed (arxana-essay-pair--get props :deployed))
         (orphan (arxana-essay-pair--get props :orphan)))
    (insert (propertize (format "Arxana essay-pair %s pane\nEssay: %s\n" side essay-id)
                        'face 'bold))
    (when provenance
      (insert (format "Provenance: %s\n" (arxana-essay-pair--hx-id provenance)))
      (when redactions (insert (format "Redactions: %s\n" redactions)))
      (when deployed (insert (format "Deployed: %s\n" deployed)))
      (when orphan (insert (format "Orphan: %s\n" orphan))))
    (insert (format "Projection layer: %d hyperedges. RET/click a highlighted anchor to jump.\n\n---\n\n"
                    (length hxs)))))

(defun arxana-essay-pair--render-buffer (essay-id source-file side provenance hxs)
  (let* ((name (format "*Arxana Essay Pair %s: %s*" side essay-id))
         (buffer (get-buffer-create name))
         (own-anns (arxana-essay-pair--own-annotations essay-id source-file))
         own-count proj-count)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (arxana-essay-pair--render-header side essay-id provenance hxs)
        (insert-file-contents source-file))
      (goto-char (point-min))
      (arxana-essay-pair-mode)
      (setq-local arxana-essay-pair--essay-id essay-id)
      (setq own-count
            (arxana-essay-pair--place-own-annotations
             own-anns (if (eq side 'left)
                          'arxana-essay-pair-own-left-face
                        'arxana-essay-pair-own-right-face)))
      (setq proj-count (arxana-essay-pair--place-projections hxs side essay-id)))
    (list :buffer buffer :own-count own-count :projection-count proj-count)))

;;;###autoload
(defun arxana-essay-side-by-side (&optional provenance-or-left right)
  "Render two compiled essays side by side.
With one argument, PROVENANCE-OR-LEFT is a provenance hyperedge id whose
endpoints resolve the draft/final pair.  With two arguments, it is the left
essay id and RIGHT is the right essay id."
  (interactive
   (if current-prefix-arg
       (list (read-string "Left essay id: ")
             (read-string "Right essay id: "))
     (list (read-string "Provenance edge id: "
                        "arxana/hyperedge/fellows-eoi-projection-v1-to-v2")
           nil)))
  (unless (arxana-store-ensure-sync)
    (user-error "Futon sync is disabled; set futon4-enable-sync/futon4-base-url"))
  (let* ((pair (if right
                   (list :left-id provenance-or-left :right-id right)
                 (arxana-essay-pair--provenance-pair provenance-or-left)))
         (left-id (plist-get pair :left-id))
         (right-id (plist-get pair :right-id))
         (provenance (plist-get pair :provenance))
         (left-source (arxana-essay-pair--essay-source-file left-id))
         (right-source (arxana-essay-pair--essay-source-file right-id))
         (hxs (arxana-essay-pair--fetch-projection-hyperedges left-id right-id)))
    (setq arxana-essay-pair--projection-overlays (make-hash-table :test 'equal))
    (arxana-essay-pair--clear-active)
    (let* ((left (arxana-essay-pair--render-buffer left-id left-source 'left provenance hxs))
           (right-pane (arxana-essay-pair--render-buffer right-id right-source 'right provenance hxs))
           (left-buffer (plist-get left :buffer))
           (right-buffer (plist-get right-pane :buffer)))
      (with-current-buffer left-buffer
        (setq-local arxana-essay-pair--other-buffer right-buffer))
      (with-current-buffer right-buffer
        (setq-local arxana-essay-pair--other-buffer left-buffer))
      (delete-other-windows)
      (switch-to-buffer left-buffer)
      (let ((right-window (split-window-right
                           (max 20 (floor (* (frame-width)
                                             arxana-essay-pair-right-width))))))
        (set-window-buffer right-window right-buffer))
      (setq arxana-essay-pair-last-summary
            (list :left-id left-id
                  :right-id right-id
                  :provenance-id (plist-get pair :provenance-id)
                  :left-source left-source
                  :right-source right-source
                  :left-own-annotations (plist-get left :own-count)
                  :right-own-annotations (plist-get right-pane :own-count)
                  :projection-hyperedges (length hxs)
                  :left-projection-overlays (plist-get left :projection-count)
                  :right-projection-overlays (plist-get right-pane :projection-count)
                  :left-buffer (buffer-name left-buffer)
                  :right-buffer (buffer-name right-buffer)))
      (message "[essay-pair] %s ⇄ %s: own %d/%d, projection %d hxs, overlays %d/%d"
               left-id right-id
               (plist-get left :own-count)
               (plist-get right-pane :own-count)
               (length hxs)
               (plist-get left :projection-count)
               (plist-get right-pane :projection-count))
      arxana-essay-pair-last-summary)))

(provide 'arxana-essay-pair)
;;; arxana-essay-pair.el ends here
