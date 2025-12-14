;;; arxana-derivation.el --- Inclusion/derivation previews -*- lexical-binding: t; -*-

;;; Commentary:
;; Renders inline previews for inclusion / derivation scholia inside the
;; "Scholia Display" buffer.  We harvest the non-printing `derives-from`
;; scholia after `mark-things-up` runs, capture the passages they reference,
;; and surface them via a collapsible section that lives in
;; `scholia-display-extras-hook`.  This gives Part VI readers a quick way to
;; audit which passages were pulled in via inclusion, transclusion, or
;; identification without scanning the source article by hand.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup arxana-derivation nil
  "Preview helpers for inclusion / derivation scholia."
  :group 'arxana)

(defface arxana-derivation-inclusion-face
  '((t :inherit highlight :background "#052"))
  "Face used to highlight included passages."
  :group 'arxana-derivation)

(defface arxana-derivation-transclusion-face
  '((t :inherit highlight :background "#205"))
  "Face used to highlight transcluded passages."
  :group 'arxana-derivation)

(defface arxana-derivation-identification-face
  '((t :inherit highlight :background "#440"))
  "Face used to highlight identification passages."
  :group 'arxana-derivation)

(defcustom arxana-derivation-highlight-inclusions t
  "When non-nil, highlight inclusion passages in Scholium Display buffers."
  :type 'boolean
  :group 'arxana-derivation)

(defcustom arxana-derivation-highlight-transclusions t
  "When non-nil, highlight transclusion passages in Scholium Display buffers."
  :type 'boolean
  :group 'arxana-derivation)

(defcustom arxana-derivation-highlight-identifications nil
  "When non-nil, highlight identification passages in Scholium Display buffers."
  :type 'boolean
  :group 'arxana-derivation)

(defcustom arxana-derivation-preview-max-chars 320
  "Maximum number of characters to show when an excerpt is expanded."
  :type 'integer
  :group 'arxana-derivation)

(defcustom arxana-derivation-preview-max-lines 4
  "Maximum number of logical lines to include in the excerpt preview."
  :type 'integer
  :group 'arxana-derivation)

(defcustom arxana-derivation-preview-default-state 'collapsed
  "Initial expansion state for newly discovered derivation previews."
  :type '(choice (const :tag "Collapsed" collapsed)
                 (const :tag "Expanded" expanded))
  :group 'arxana-derivation)

(defvar arxana-derivation-preview-state (make-hash-table :test 'equal)
  "Hash map of scholium preview keys -> `expanded' or `collapsed'.")

(defvar-local arxana-derivation-preview-items nil
  "List of preview item plists captured for the current display buffer.")

(defvar-local arxana-derivation-preview--block nil
  "Cons of markers bounding the rendered preview block.")

(defvar-local arxana-derivation-highlight-overlays nil
  "Overlays tracking inclusion/transclusion highlights in this buffer.")

(declare-function scholium-name "arxana-tangled" (scholium))
(declare-function scholium-about "arxana-tangled" (scholium))
(declare-function scholium-type "arxana-tangled" (scholium))
(declare-function get-rendering-target-buffer "arxana-tangled" nil)

(defun arxana-derivation--normalize-name (name)
  "Return a string representation of NAME."
  (cond
   ((null name) nil)
   ((stringp name) name)
   (t (format "%s" name))))

(defun arxana-derivation--lookup-id (name)
  "Return the Futon id for NAME when the bridge is loaded."
  (when (and name (fboundp 'futon4-lookup-article-id))
    (ignore-errors (futon4-lookup-article-id name))))

(defun arxana-derivation--passage-range (about target-name)
  "Extract the BEG . END passage range from ABOUT for TARGET-NAME."
  (let ((target (arxana-derivation--normalize-name target-name)))
    (catch 'range
      (dolist (entry about)
        (when (and (listp entry)
                   (eq (car entry) 'passage))
          (let* ((payload (cadr entry))
                 (parts (if (and (listp payload)
                                 (= (length payload) 1)
                                 (listp (car payload)))
                            (car payload)
                          payload))
                 (article (arxana-derivation--normalize-name (car-safe parts)))
                 (beg (nth 1 parts))
                 (end (nth 2 parts)))
            (when (and beg end
                       (or (null target)
                           (and article (string= article target))))
              (throw 'range (cons beg end))))))
      nil)))

(defun arxana-derivation--region-has (buffer beg end key)
  "Return t when text between BEG and END in BUFFER has scholia KEY."
  (let ((found nil))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((pos beg))
          (while (and (not found) (< pos end))
            (let ((value (get-text-property pos 'scholia)))
              (when (and (listp value)
                         (seq-some (lambda (entry)
                                     (and (listp entry)
                                          (eq (car entry) key)))
                                   value))
                (setq found t)))
            (setq pos (next-single-property-change pos 'scholia nil (1+ pos)))))))
    found))

(defun arxana-derivation--classify (buffer beg end)
  "Return a keyword describing the derivative mode for BEG..END."
  (cond
   ((arxana-derivation--region-has buffer beg end 'transclusion-of) :transclusion)
   ((arxana-derivation--region-has buffer beg end 'identifies-with) :identification)
   (t :inclusion)))

(defun arxana-derivation--cleanup-snippet (text)
  "Trim TEXT down to the configured preview size."
  (let* ((lines (seq-take
                 (seq-remove #'string-empty-p
                             (mapcar #'string-trim (split-string text "\n")))
                 arxana-derivation-preview-max-lines))
         (joined (string-join lines "\n")))
    (if (> (length joined) arxana-derivation-preview-max-chars)
        (concat (substring joined 0 arxana-derivation-preview-max-chars) "...")
      joined)))

(defun arxana-derivation--excerpt (buffer beg end)
  "Return the preview snippet for BEG..END inside BUFFER."
  (when (and buffer (buffer-live-p buffer) beg end)
    (with-current-buffer buffer
      (let* ((bounded-beg (max (point-min) beg))
             (bounded-end (min (point-max) end))
             (raw (buffer-substring-no-properties bounded-beg bounded-end)))
        (arxana-derivation--cleanup-snippet raw)))))

(defun arxana-derivation--preview-key (scholium target)
  "Build a preview key for SCHOLIUM scoped to TARGET."
  (format "%s::%S" (arxana-derivation--normalize-name target)
          (scholium-name scholium)))

(defun arxana-derivation--build-items (scholia buffer target-name)
  "Return preview plists for SCHOLIA rendered inside BUFFER for TARGET-NAME."
  (let (items)
    (dolist (sch scholia)
      (when (eq (scholium-type sch) 'derives-from)
        (let* ((range (arxana-derivation--passage-range (scholium-about sch) target-name))
               (beg (car-safe range))
               (end (cdr-safe range))
               (source (arxana-derivation--normalize-name (nth 2 (scholium-name sch))))
               (snippet (and beg end (arxana-derivation--excerpt buffer beg end))))
          (when (and beg end source snippet)
            (let ((item (list :key (arxana-derivation--preview-key sch target-name)
                              :source source
                              :target (arxana-derivation--normalize-name target-name)
                              :begin beg
                              :end end
                              :length (max 0 (- end beg))
                              :snippet snippet
                              :kind (arxana-derivation--classify buffer beg end)
                              :futon-source (arxana-derivation--lookup-id source)
                              :futon-target (arxana-derivation--lookup-id target-name))))
              (push item items))))))
    (nreverse items)))

(defun arxana-derivation--collect-previews ()
  "Populate `arxana-derivation-preview-items' for the current scholia render."
  (let* ((buffer (get-rendering-target-buffer))
         (items (arxana-derivation--build-items (bound-and-true-p raw-scholia)
                                                buffer
                                                (bound-and-true-p name-of-current-article))))
    (setq arxana-derivation-preview-items items)
    (setq arxana-derivation-preview--block nil)
    (arxana-derivation--apply-highlights buffer)))

(defun arxana-derivation--clear-highlight-overlays ()
  "Remove any highlight overlays installed in the current buffer."
  (when arxana-derivation-highlight-overlays
    (mapc #'delete-overlay arxana-derivation-highlight-overlays)
    (setq arxana-derivation-highlight-overlays nil)))

(defun arxana-derivation--highlight-face (kind)
  "Return the face to use for KIND."
  (pcase kind
    (:transclusion 'arxana-derivation-transclusion-face)
    (:identification 'arxana-derivation-identification-face)
    (:inclusion 'arxana-derivation-inclusion-face)
    (_ nil)))

(defun arxana-derivation--highlight-enabled-p (kind)
  "Return non-nil when KIND should be highlighted."
  (pcase kind
    (:transclusion arxana-derivation-highlight-transclusions)
    (:identification arxana-derivation-highlight-identifications)
    (:inclusion arxana-derivation-highlight-inclusions)
    (_ nil)))

(defun arxana-derivation--apply-highlights (buffer)
  "Install highlight overlays for BUFFER using cached preview items."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (arxana-derivation--clear-highlight-overlays)
      (dolist (item arxana-derivation-preview-items)
        (let* ((kind (plist-get item :kind))
               (face (arxana-derivation--highlight-face kind))
               (begin (plist-get item :begin))
               (end (plist-get item :end)))
          (when (and face
                     (arxana-derivation--highlight-enabled-p kind)
                     (numberp begin) (numberp end) (< begin end))
            (let* ((bounded-beg (max (point-min) begin))
                   (bounded-end (min (point-max) end))
                   (overlay (make-overlay bounded-beg bounded-end nil t t)))
              (overlay-put overlay 'face face)
              (overlay-put overlay 'priority 100)
              (overlay-put overlay 'arxana-derivation-kind kind)
              (push overlay arxana-derivation-highlight-overlays))))))))

(defun arxana-derivation-refresh-highlights ()
  "Rebuild highlight overlays for the current Scholium Display buffer."
  (interactive)
  (unless arxana-derivation-preview-items
    (user-error "No derivation metadata recorded for this buffer"))
  (arxana-derivation--apply-highlights (current-buffer))
  (message "Arxana derivation highlights refreshed."))

(defun arxana-derivation--state (key)
  "Return the stored expansion state for KEY, falling back to the default."
  (or (gethash key arxana-derivation-preview-state)
      arxana-derivation-preview-default-state))

(defun arxana-derivation--set-state (key state)
  "Record expansion STATE for preview KEY."
  (puthash key state arxana-derivation-preview-state))

(defun arxana-derivation--kind-label (kind)
  "Return a human-friendly label for KIND."
  (pcase kind
    (:transclusion "transclusion")
    (:identification "identification")
    (:inclusion "inclusion")
    (_ "derivation")))

(define-button-type 'arxana-derivation-toggle
  'follow-link t
  'help-echo "Toggle preview (RET or mouse-1)"
  'action (lambda (button)
            (arxana-derivation-preview-toggle
             (button-get button 'arxana-derivation-key))))

(defun arxana-derivation--insert-toggle (key expanded)
  "Insert a disclosure button for KEY using EXPANDED state."
  (let ((start (point)))
    (insert (if (eq expanded 'expanded) "[-]" "[+]"))
    (make-text-button start (point)
                      :type 'arxana-derivation-toggle
                      'arxana-derivation-key key)))

(defun arxana-derivation--insert-item (item)
  "Insert a single preview ITEM at point."
  (let* ((key (plist-get item :key))
         (expanded (arxana-derivation--state key))
         (source (plist-get item :source))
         (length (plist-get item :length))
         (kind (arxana-derivation--kind-label (plist-get item :kind)))
         (futon-id (or (plist-get item :futon-source) "<unsynced>")))
    (arxana-derivation--insert-toggle key expanded)
    (insert (format " %s -- %s (%s chars) [id: %s]\n"
                    source kind length futon-id))
    (when (eq expanded 'expanded)
      (let ((snippet (plist-get item :snippet)))
        (when (and snippet (not (string-empty-p snippet)))
          (dolist (line (split-string snippet "\n"))
            (insert (format "    %s\n" line))))))))

(defun arxana-derivation--delete-block ()
  "Remove the previously rendered preview block, if any."
  (when (and arxana-derivation-preview--block
             (markerp (car arxana-derivation-preview--block))
             (markerp (cdr arxana-derivation-preview--block)))
    (let ((beg (marker-position (car arxana-derivation-preview--block)))
          (end (marker-position (cdr arxana-derivation-preview--block))))
      (when (and beg end)
        (delete-region beg end)))
    (set-marker (car arxana-derivation-preview--block) nil)
    (set-marker (cdr arxana-derivation-preview--block) nil)
    (setq arxana-derivation-preview--block nil)))

(defun arxana-derivation--render-previews ()
  "Insert the derivation preview block at point."
  (arxana-derivation--delete-block)
  (when (and arxana-derivation-preview-items
             (not (seq-empty-p arxana-derivation-preview-items)))
    (let ((beg (point)))
      (unless (bolp)
        (insert "\n"))
      (insert (format "Derivation previews (%d):\n"
                      (length arxana-derivation-preview-items)))
      (dolist (item arxana-derivation-preview-items)
        (arxana-derivation--insert-item item))
      (insert "\n")
      (setq arxana-derivation-preview--block
            (cons (copy-marker beg t) (copy-marker (point) t))))))

(defun arxana-derivation-preview-refresh ()
  "Re-render the preview block in the current buffer."
  (interactive)
  (let ((was-view-mode (and (boundp 'view-mode) view-mode)))
    (when was-view-mode
      (view-mode -1))
    (unwind-protect
        (let ((inhibit-read-only t))
          (save-excursion
            (if (and arxana-derivation-preview--block
                     (markerp (car arxana-derivation-preview--block)))
                (goto-char (marker-position (car arxana-derivation-preview--block)))
              (goto-char (point-max)))
            (arxana-derivation--render-previews)))
      (when was-view-mode
        (view-mode 1)))))

(defun arxana-derivation-preview-toggle (key)
  "Toggle the expansion state for KEY and refresh the preview block."
  (interactive)
  (let* ((current (arxana-derivation--state key))
         (next (if (eq current 'expanded) 'collapsed 'expanded)))
    (arxana-derivation--set-state key next)
    (arxana-derivation-preview-refresh)))

(defun arxana-derivation-toggle-inclusion-highlights ()
  "Toggle highlighting for inclusion passages and refresh the buffer."
  (interactive)
  (setq arxana-derivation-highlight-inclusions
        (not arxana-derivation-highlight-inclusions))
  (arxana-derivation-refresh-highlights)
  (message "Inclusion highlighting %s"
           (if arxana-derivation-highlight-inclusions "enabled" "disabled")))

(defun arxana-derivation-toggle-transclusion-highlights ()
  "Toggle highlighting for transclusion passages and refresh the buffer."
  (interactive)
  (setq arxana-derivation-highlight-transclusions
        (not arxana-derivation-highlight-transclusions))
  (arxana-derivation-refresh-highlights)
  (message "Transclusion highlighting %s"
           (if arxana-derivation-highlight-transclusions "enabled" "disabled")))

(defun arxana-derivation-toggle-identification-highlights ()
  "Toggle highlighting for identification passages and refresh the buffer."
  (interactive)
  (setq arxana-derivation-highlight-identifications
        (not arxana-derivation-highlight-identifications))
  (arxana-derivation-refresh-highlights)
  (message "Identification highlighting %s"
           (if arxana-derivation-highlight-identifications "enabled" "disabled")))

(defun arxana-derivation--highlight-only (kind)
  "Enable highlights for KIND and disable the other derivative modes."
  (pcase kind
    (:inclusion
     (setq arxana-derivation-highlight-inclusions t
           arxana-derivation-highlight-transclusions nil
           arxana-derivation-highlight-identifications nil)
     (arxana-derivation-refresh-highlights)
     (message "Showing inclusion highlights only"))
    (:transclusion
     (setq arxana-derivation-highlight-inclusions nil
           arxana-derivation-highlight-transclusions t
           arxana-derivation-highlight-identifications nil)
     (arxana-derivation-refresh-highlights)
     (message "Showing transclusion highlights only"))
    (:identification
     (setq arxana-derivation-highlight-inclusions nil
           arxana-derivation-highlight-transclusions nil
           arxana-derivation-highlight-identifications t)
     (arxana-derivation-refresh-highlights)
     (message "Showing identification highlights only"))))

(defun arxana-derivation-highlight-only-inclusions ()
  "Disable other derivative highlights and focus on inclusions."
  (interactive)
  (arxana-derivation--highlight-only :inclusion))

(defun arxana-derivation-highlight-only-transclusions ()
  "Disable other derivative highlights and focus on transclusions."
  (interactive)
  (arxana-derivation--highlight-only :transclusion))

(defun arxana-derivation-highlight-only-identifications ()
  "Disable other derivative highlights and focus on identifications."
  (interactive)
  (arxana-derivation--highlight-only :identification))

(defun arxana-derivation--render-hook ()
  "Hook entry point for `scholia-display-extras-hook'."
  (arxana-derivation--render-previews))

(with-eval-after-load 'arxana-tangled
  (add-hook 'mark-things-up-hook #'arxana-derivation--collect-previews t)
  (add-hook 'scholia-display-extras-hook #'arxana-derivation--render-hook t))

(provide 'arxana-derivation)

;;; arxana-derivation.el ends here
