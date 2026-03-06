;;; arxana-browser-enrich.el --- Enrichment panel for code browser -*- lexical-binding: t; -*-

;;; Commentary:
;; Extends arxana-browser-code with enrichment data from the futon3c
;; enrichment API (GET /api/alpha/enrich/file?path=...).
;;
;; Shows: mission provenance, pattern provenance, evidence counts,
;; tensions, cross-futon dependencies, and churn/complexity metrics
;; for each symbol in the current file.
;;
;; Usage:
;;   (load "/home/joe/code/futon4/dev/arxana-browser-enrich.el")
;;   M-x arxana-enrich-file   ;; enrich current file
;;   M-x arxana-enrich-symbol ;; enrich symbol at point

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;;; Configuration

(defgroup arxana-enrich nil
  "Enrichment panel for Arxana code browser."
  :group 'arxana-browser-code)

(defcustom arxana-enrich-api-url "http://localhost:7070"
  "Base URL for the futon3c API server."
  :type 'string
  :group 'arxana-enrich)

(defcustom arxana-enrich-buffer "*Arxana Enrichment*"
  "Buffer name for the enrichment panel."
  :type 'string
  :group 'arxana-enrich)

(defcustom arxana-enrich-side 'right
  "Side for the enrichment window."
  :type '(choice (const right) (const left) (const bottom))
  :group 'arxana-enrich)

(defcustom arxana-enrich-width 55
  "Width for the enrichment side window."
  :type 'integer
  :group 'arxana-enrich)

;;; Faces

(defface arxana-enrich-heading
  '((t :weight bold :foreground "#bd93f9" :height 1.1))
  "Face for enrichment section headings."
  :group 'arxana-enrich)

(defface arxana-enrich-mission
  '((t :foreground "#50fa7b"))
  "Face for mission IDs."
  :group 'arxana-enrich)

(defface arxana-enrich-pattern
  '((t :foreground "#8be9fd"))
  "Face for pattern names."
  :group 'arxana-enrich)

(defface arxana-enrich-tension
  '((t :foreground "#ff5555"))
  "Face for tension warnings."
  :group 'arxana-enrich)

(defface arxana-enrich-symbol
  '((t :foreground "#f1fa8c" :weight bold))
  "Face for symbol names."
  :group 'arxana-enrich)

(defface arxana-enrich-count
  '((t :foreground "#ffb86c"))
  "Face for numeric counts."
  :group 'arxana-enrich)

(defface arxana-enrich-dim
  '((t :foreground "#6272a4"))
  "Face for dimmed/secondary text."
  :group 'arxana-enrich)

;;; Internal state

(defvar-local arxana-enrich--data nil
  "Cached enrichment data for the current file.")

(defvar-local arxana-enrich--file nil
  "File path for the current enrichment data.")

;;; API query

(defun arxana-enrich--fetch (path)
  "Fetch enrichment data for PATH from the futon3c API.
Returns parsed JSON as alist, or nil on error."
  (let* ((url (format "%s/api/alpha/enrich/file?path=%s"
                      arxana-enrich-api-url
                      (url-hexify-string path)))
         (url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (buffer (condition-case nil
                     (url-retrieve-synchronously url t t 10)
                   (error nil))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil 'move)
        (let ((result (condition-case nil
                          (json-parse-buffer :object-type 'alist
                                            :null-object nil
                                            :false-object nil)
                        (error nil))))
          (kill-buffer buffer)
          (when (alist-get 'ok result)
            result))))))

;;; Rendering

(defun arxana-enrich--insert-heading (text)
  "Insert a section heading."
  (insert (propertize text 'face 'arxana-enrich-heading) "\n"))

(defun arxana-enrich--insert-list (items face &optional prefix)
  "Insert a list of ITEMS with FACE. Optional PREFIX per line."
  (let ((pfx (or prefix "  • ")))
    (dolist (item items)
      (insert pfx (propertize (format "%s" item) 'face face) "\n"))))

(defun arxana-enrich--render-file-summary (data)
  "Render file-level enrichment summary from DATA."
  (let ((ns (alist-get 'namespace data))
        (layer (alist-get 'enrichment-layer data))
        (var-count (alist-get 'var-count data))
        (missions (alist-get 'missions data))
        (churn (alist-get 'churn data))
        (invariants (alist-get 'invariants data)))
    ;; Header
    (arxana-enrich--insert-heading "── File Enrichment ──")
    (insert (propertize (format "Namespace: %s\n" (or ns "?"))
                        'face 'arxana-enrich-dim))
    (insert (propertize (format "Layer: %s  |  Vars: %s\n"
                                (or layer "?") (or var-count "?"))
                        'face 'arxana-enrich-dim))
    (insert "\n")

    ;; Missions
    (when (and missions (> (length missions) 0))
      (arxana-enrich--insert-heading "Missions")
      (arxana-enrich--insert-list missions 'arxana-enrich-mission)
      (insert "\n"))

    ;; Churn
    (when churn
      (arxana-enrich--insert-heading "Churn / Complexity")
      (let ((commits (alist-get 'commits churn))
            (score (alist-get 'score churn)))
        (when commits
          (insert "  commits: " (propertize (format "%s" commits)
                                            'face 'arxana-enrich-count) "\n"))
        (when score
          (insert "  hotspot score: " (propertize (format "%s" score)
                                                  'face 'arxana-enrich-count) "\n")))
      (insert "\n"))

    ;; Invariant violations
    (when (and invariants (> (length invariants) 0))
      (arxana-enrich--insert-heading "Invariant Violations")
      (dolist (inv invariants)
        (let ((inv-type (alist-get 'invariant inv))
              (summary (alist-get 'summary inv)))
          (insert "  " (propertize (or inv-type "?") 'face 'arxana-enrich-tension)
                  ": " (or summary "") "\n")))
      (insert "\n"))))

(defun arxana-enrich--render-symbols (data)
  "Render per-symbol enrichment from DATA."
  (let ((symbols (alist-get 'symbols data)))
    (when symbols
      (arxana-enrich--insert-heading "── Per-Symbol Enrichment ──")
      (insert "\n")
      ;; symbols is an alist of (symbol-name . enrichment-data)
      (let ((entries (cond
                      ((hash-table-p symbols)
                       (let (pairs)
                         (maphash (lambda (k v) (push (cons k v) pairs)) symbols)
                         (nreverse pairs)))
                      ((listp symbols) symbols)
                      (t nil))))
        (dolist (entry entries)
          (let* ((sym-name (if (symbolp (car entry))
                               (symbol-name (car entry))
                             (format "%s" (car entry))))
                 (info (cdr entry))
                 (missions (alist-get 'missions info))
                 (patterns (alist-get 'patterns info))
                 (evidence-count (alist-get 'evidence-count info))
                 (tensions (alist-get 'tensions info))
                 (has-data (or missions patterns evidence-count tensions)))
            (when has-data
              (insert (propertize sym-name 'face 'arxana-enrich-symbol) "\n")
              (when (and missions (> (length missions) 0))
                (dolist (m missions)
                  (insert "  mission: " (propertize (format "%s" m)
                                                    'face 'arxana-enrich-mission) "\n")))
              (when (and patterns (> (length patterns) 0))
                (dolist (p patterns)
                  (insert "  pattern: " (propertize (format "%s" p)
                                                    'face 'arxana-enrich-pattern) "\n")))
              (when (and evidence-count (> evidence-count 0))
                (insert "  evidence: " (propertize (format "%d entries" evidence-count)
                                                   'face 'arxana-enrich-count) "\n"))
              (when (and tensions (> (length tensions) 0))
                (dolist (t tensions)
                  (insert "  tension: " (propertize (format "%s" t)
                                                    'face 'arxana-enrich-tension) "\n")))
              (insert "\n"))))))))

(defun arxana-enrich--render (data)
  "Render enrichment DATA into the enrichment buffer."
  (let ((buf (get-buffer-create arxana-enrich-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (arxana-enrich--render-file-summary data)
        (arxana-enrich--render-symbols data)
        (goto-char (point-min))
        (special-mode)))
    buf))

(defun arxana-enrich--display (buf)
  "Display enrichment buffer BUF in a side window."
  (display-buffer-in-side-window
   buf `((side . ,arxana-enrich-side)
         (window-width . ,arxana-enrich-width)
         (slot . 2))))

;;; Path resolution

(defun arxana-enrich--buffer-source-path ()
  "Derive a source-relative path from the current buffer's file.
Strips everything up to and including the first src/ component."
  (when-let ((file (buffer-file-name)))
    (if (string-match "/src/" file)
        (substring file (match-beginning 0))
      ;; Fallback: return path relative to ~/code/
      (if (string-match "/home/joe/code/" file)
          (substring file (match-end 0))
        file))))

;;; Commands

;;;###autoload
(defun arxana-enrich-file ()
  "Fetch and display enrichment data for the current file."
  (interactive)
  (let ((path (arxana-enrich--buffer-source-path)))
    (if (not path)
        (message "Cannot determine source path for current buffer")
      (message "Fetching enrichment for %s..." path)
      (let ((data (arxana-enrich--fetch path)))
        (if (not data)
            (message "No enrichment data available (is futon3c/futon1a running?)")
          (setq arxana-enrich--data data)
          (setq arxana-enrich--file path)
          (let ((buf (arxana-enrich--render data)))
            (arxana-enrich--display buf)
            (message "Enrichment: %s (layer %s, %s vars)"
                     (or (alist-get 'namespace data) "?")
                     (or (alist-get 'enrichment-layer data) "?")
                     (or (alist-get 'var-count data) "?")
                     )))))))

;;;###autoload
(defun arxana-enrich-symbol ()
  "Show enrichment for the symbol at point.
If enrichment data hasn't been fetched yet, fetches it first."
  (interactive)
  (unless arxana-enrich--data
    (arxana-enrich-file))
  (when arxana-enrich--data
    (let* ((sym (thing-at-point 'symbol t))
           (symbols (alist-get 'symbols arxana-enrich--data))
           (info (when (and sym symbols)
                   (cond
                    ((hash-table-p symbols) (gethash (intern sym) symbols))
                    ((listp symbols) (alist-get (intern sym) symbols))))))
      (if info
          (message "Enrichment for %s: missions=%s patterns=%s evidence=%s"
                   sym
                   (or (alist-get 'missions info) "none")
                   (or (alist-get 'patterns info) "none")
                   (or (alist-get 'evidence-count info) 0))
        (message "No enrichment data for symbol: %s" sym)))))

;;;###autoload
(defun arxana-enrich-dismiss ()
  "Close the enrichment panel."
  (interactive)
  (when-let ((win (get-buffer-window arxana-enrich-buffer)))
    (delete-window win)))

(provide 'arxana-browser-enrich)
;;; arxana-browser-enrich.el ends here
