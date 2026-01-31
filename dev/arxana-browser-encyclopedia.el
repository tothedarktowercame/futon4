;;; arxana-browser-encyclopedia.el --- Encyclopedia browser views -*- lexical-binding: t; -*-

;;; Commentary:
;; Encyclopedia (PlanetMath etc.) browsing for Arxana.
;; Browse corpuses, entries, and view LaTeX content.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

(defvar arxana-browser--stack)
(defvar arxana-browser--context)
(defvar arxana-browser--buffer)

(declare-function arxana-browser--render "arxana-browser-core")

(defgroup arxana-encyclopedia nil
  "Encyclopedia browsing for Arxana."
  :group 'arxana)

(defcustom arxana-encyclopedia-server
  (or (getenv "FUTON3_SERVER") "http://localhost:5050")
  "Futon3 HTTP server URL for encyclopedia."
  :type 'string
  :group 'arxana-encyclopedia)

(defcustom arxana-encyclopedia-request-timeout 10
  "Timeout in seconds for encyclopedia requests."
  :type 'integer
  :group 'arxana-encyclopedia)

(defun arxana-encyclopedia--parse-json (body)
  "Parse JSON BODY into plist."
  (when (and body (stringp body) (not (string-empty-p body)))
    (condition-case nil
        (if (fboundp 'json-parse-string)
            (json-parse-string body
                               :object-type 'plist
                               :array-type 'list
                               :null-object nil
                               :false-object nil)
          (let ((json-object-type 'plist)
                (json-array-type 'list)
                (json-false nil)
                (json-null nil))
            (json-read-from-string body)))
      (error nil))))

(defun arxana-encyclopedia--fetch (endpoint)
  "Fetch from encyclopedia ENDPOINT."
  (let* ((url-request-method "GET")
         (url-request-extra-headers '(("Accept" . "application/json")))
         (url (concat (string-remove-suffix "/" arxana-encyclopedia-server) endpoint))
         (buffer (url-retrieve-synchronously url t t arxana-encyclopedia-request-timeout)))
    (unless buffer
      (user-error "Failed to fetch from %s" url))
    (with-current-buffer buffer
      (goto-char (point-min))
      (re-search-forward "\n\n" nil 'move)
      (let ((body (buffer-substring-no-properties (point) (point-max))))
        (kill-buffer buffer)
        (arxana-encyclopedia--parse-json body)))))

(defun arxana-encyclopedia--truncate (text max-len)
  "Truncate TEXT to MAX-LEN characters."
  (let ((value (or text "")))
    (if (> (length value) max-len)
        (concat (substring value 0 (max 0 (- max-len 3))) "...")
      value)))

;; =============================================================================
;; Corpus list view
;; =============================================================================

(defun arxana-browser--encyclopedia-format ()
  "Column format for corpus list."
  [("Corpus" 30 t)
   ("Entries" 8 nil)
   ("Source" 15 nil)])

(defun arxana-browser--encyclopedia-row (item)
  "Row for corpus ITEM."
  (let ((id (or (plist-get item :corpus/id) ""))
        (count (or (plist-get item :corpus/count) 0))
        (source (or (plist-get item :corpus/source) "")))
    (vector id (format "%d" count) source)))

(defun arxana-browser--encyclopedia-items ()
  "Fetch and return encyclopedia corpuses."
  (condition-case err
      (let* ((response (arxana-encyclopedia--fetch "/fulab/encyclopedia/corpuses"))
             (corpuses (plist-get response :corpuses)))
        (if corpuses
            (mapcar (lambda (c)
                      (append (list :type 'encyclopedia-corpus) c))
                    corpuses)
          (list (list :type 'info
                      :label "No corpuses found"
                      :description "Parse PlanetMath content first"))))
    (error
     (list (list :type 'info
                 :label "Failed to fetch corpuses"
                 :description (format "Error: %s" (error-message-string err)))))))

;; =============================================================================
;; Entry list view (within a corpus)
;; =============================================================================

(defun arxana-browser--encyclopedia-entries-format ()
  "Column format for entry list."
  [("ID" 35 t)
   ("Title" 40 t)
   ("Type" 12 nil)])

(defun arxana-browser--encyclopedia-entries-row (item)
  "Row for entry ITEM."
  (let ((id (or (plist-get item :entry/id) ""))
        (title (or (plist-get item :entry/title) ""))
        (type (or (plist-get item :entry/type) "")))
    (vector (arxana-encyclopedia--truncate id 34)
            (arxana-encyclopedia--truncate title 39)
            type)))

(defun arxana-browser--encyclopedia-entries-items (context)
  "Fetch entries for corpus in CONTEXT."
  (let ((corpus (plist-get context :corpus)))
    (condition-case err
        (let* ((response (arxana-encyclopedia--fetch
                          (format "/fulab/encyclopedia/%s/entries?limit=200" corpus)))
               (entries (plist-get response :entries)))
          (if entries
              (mapcar (lambda (e)
                        (append (list :type 'encyclopedia-entry :corpus corpus) e))
                      entries)
            (list (list :type 'info
                        :label "No entries"
                        :description "Corpus is empty"))))
      (error
       (list (list :type 'info
                   :label "Failed to fetch entries"
                   :description (format "Error: %s" (error-message-string err))))))))

;; =============================================================================
;; Entry detail view
;; =============================================================================

(defun arxana-browser--encyclopedia-entry-format ()
  "Column format for entry detail (actually not tabular)."
  [("Field" 15 nil)
   ("Value" 0 nil)])

(defun arxana-browser--encyclopedia-entry-row (item)
  "Row for entry detail ITEM."
  (let ((field (or (plist-get item :field) ""))
        (value (or (plist-get item :value) "")))
    (vector field value)))

(defun arxana-browser--encyclopedia-entry-items (context)
  "Fetch full entry details for CONTEXT."
  (let ((corpus (plist-get context :corpus))
        (entry-id (plist-get context :entry-id)))
    (condition-case err
        (let* ((response (arxana-encyclopedia--fetch
                          (format "/fulab/encyclopedia/%s/entry/%s" corpus entry-id)))
               (entry (plist-get response :entry)))
          (if entry
              (list
               (list :type 'info :field "ID" :value (or (plist-get entry :entry/id) ""))
               (list :type 'info :field "Title" :value (or (plist-get entry :entry/title) ""))
               (list :type 'info :field "Type" :value (or (plist-get entry :entry/type) ""))
               (list :type 'info :field "Author" :value (or (plist-get entry :entry/author) ""))
               (list :type 'info :field "Created" :value (or (plist-get entry :entry/created) ""))
               (list :type 'info :field "Related"
                     :value (mapconcat #'identity (plist-get entry :entry/related) ", "))
               (list :type 'info :field "Defines"
                     :value (mapconcat #'identity (plist-get entry :entry/defines) ", "))
               (list :type 'encyclopedia-body :corpus corpus :entry-id entry-id
                     :field "Body" :value (arxana-encyclopedia--truncate
                                           (or (plist-get entry :entry/body) "") 200)))
            (list (list :type 'info :label "Entry not found"))))
      (error
       (list (list :type 'info
                   :label "Failed to fetch entry"
                   :description (format "Error: %s" (error-message-string err))))))))

;; =============================================================================
;; Actions
;; =============================================================================

(defun arxana-browser-encyclopedia-open-corpus (item)
  "Open a corpus ITEM to show its entries."
  (let ((corpus (plist-get item :corpus/id)))
    (setq arxana-browser--stack
          (cons (list :view 'encyclopedia-entries
                      :label corpus
                      :corpus corpus)
                arxana-browser--stack))
    (arxana-browser--render)))

(defun arxana-browser-encyclopedia-open-entry (item)
  "Open an entry ITEM to show its details."
  (let ((corpus (plist-get item :corpus))
        (entry-id (plist-get item :entry/id)))
    (setq arxana-browser--stack
          (cons (list :view 'encyclopedia-entry
                      :label (or (plist-get item :entry/title) entry-id)
                      :corpus corpus
                      :entry-id entry-id)
                arxana-browser--stack))
    (arxana-browser--render)))

(defun arxana-browser-encyclopedia-view-body ()
  "View the full LaTeX body of the current entry in a separate buffer."
  (interactive)
  (let* ((context (or arxana-browser--context (car arxana-browser--stack)))
         (corpus (plist-get context :corpus))
         (entry-id (plist-get context :entry-id)))
    (when (and corpus entry-id)
      (let* ((response (arxana-encyclopedia--fetch
                        (format "/fulab/encyclopedia/%s/entry/%s" corpus entry-id)))
             (entry (plist-get response :entry))
             (body (plist-get entry :entry/body))
             (title (or (plist-get entry :entry/title) entry-id)))
        (when body
          (let ((buf (get-buffer-create (format "*Encyclopedia: %s*" title))))
            (with-current-buffer buf
              (erase-buffer)
              (insert body)
              (when (fboundp 'latex-mode) (latex-mode))
              (goto-char (point-min)))
            (pop-to-buffer buf)))))))

(provide 'arxana-browser-encyclopedia)
;;; arxana-browser-encyclopedia.el ends here
