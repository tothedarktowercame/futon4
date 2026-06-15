;;; arxana-fair.el --- Emacs <-> WebArxana bridge for the demo-gallery fair  -*- lexical-binding: t; -*-

;; Drives the WebArxana per-demo / per-person diagrams from the sales Rolodex,
;; and (once the cljs entity-location patch is rebuilt) lets a node click in the
;; web view jump to the corresponding entry in sales.edn.
;;
;; Diagram naming convention (created by the ingest scripts):
;;   demo-<demo-id>      e.g. demo-grows-at-edges-exotype
;;   person-<card-id>    e.g. person-tim-hosgood
;;   demo-gallery        the big join (all demos x territories x people)

(require 'subr-x)

(defgroup arxana-fair nil
  "Bridge between the sales Rolodex and WebArxana demo diagrams."
  :group 'tools)

(defcustom arxana-fair-webarxana-base "http://localhost:3100"
  "Base URL of the running WebArxana server."
  :type 'string :group 'arxana-fair)

(defcustom arxana-fair-sales-file (expand-file-name "~/code/sales/sales.edn")
  "The sales.edn file backing the Rolodex + demos."
  :type 'string :group 'arxana-fair)

(defun arxana-fair--diagram-url (name)
  (format "%s/wa#/diagram/%s/expanded" arxana-fair-webarxana-base name))

;;;; ---- Emacs -> Web : open the diagram for the demo/card at point ----------

(defun arxana-fair--target-at-point ()
  "Return the WebArxana diagram name for the demo/card at point, or nil.
Works in sales.edn (regexp scan) and in the rendered *Arxana Sales*
buffer (the :card/id text property)."
  (or
   ;; rendered buffer: a Rolodex card carries the :card/id text property
   (let ((c (get-text-property (point) 'arxana-sales-card)))
     (when c
       (let ((cid (plist-get c :card/id)))
         (format "person-%s"
                 (if (keywordp cid) (substring (symbol-name cid) 1) cid)))))
   ;; otherwise: scan backward to the nearest :demo/id or :card/id
   (save-excursion
     (when (re-search-backward
            "\\(:demo/id\\|:card/id\\)[ \t]+:\\([A-Za-z0-9/_-]+\\)" nil t)
       (if (equal (match-string 1) ":demo/id")
           (format "demo-%s" (match-string 2))
         (format "person-%s" (match-string 2)))))))

;;;###autoload
(defun arxana-fair-open-at-point ()
  "Open the WebArxana diagram for the demo or Rolodex card at point."
  (interactive)
  (if-let ((name (arxana-fair--target-at-point)))
      (progn (browse-url (arxana-fair--diagram-url name))
             (message "WebArxana -> %s" name))
    (user-error "No :demo/id or :card/id near point")))

;;;###autoload
(defun arxana-fair-open-gallery ()
  "Open the big demo-gallery join in WebArxana."
  (interactive)
  (browse-url (arxana-fair--diagram-url "demo-gallery")))

;;;; ---- Web -> Emacs : receive arxana://demo|rolodex/<uuid> from a click ----
;; ACTIVE once the cljs entity-location patch (see arxana-fair-CLJS-PATCH below)
;; is rebuilt so demo/person nodes emit these URIs on double-click.

(defun arxana-fair--api-prop (uuid prop)
  "Fetch PROP from the props of WebArxana entity UUID via the local API."
  (let ((url (format "%s/api/futon/entity/%s" arxana-fair-webarxana-base uuid)))
    (with-temp-buffer
      (when (zerop (call-process "curl" nil t nil "-s" url))
        (goto-char (point-min))
        (when (re-search-forward (format "\"%s\":\"\\([^\"]+\\)\"" prop) nil t)
          (match-string 1))))))

(defun arxana-fair--goto-sales (kind id)
  "Open sales.edn and move point to the :demo/id or :card/id ID."
  (find-file arxana-fair-sales-file)
  (goto-char (point-min))
  (when (re-search-forward
         (format "%s[ \t]+:%s\\_>"
                 (if (eq kind 'demo) ":demo/id" ":card/id")
                 (regexp-quote id))
         nil t)
    (beginning-of-line) (recenter)))

(defun arxana-fair-docbook-uri-advice (orig uri &rest args)
  "Handle arxana://demo|rolodex URIs; delegate everything else to ORIG."
  (cond
   ((string-prefix-p "arxana://demo/" uri)
    (let ((uuid (string-remove-prefix "arxana://demo/" uri)))
      (arxana-fair--goto-sales 'demo (or (arxana-fair--api-prop uuid "demo-id") uuid))))
   ((string-prefix-p "arxana://rolodex/" uri)
    (let ((uuid (string-remove-prefix "arxana://rolodex/" uri)))
      ;; person entities resolve by name; fall back to the raw tail
      (arxana-fair--goto-sales 'person uuid)))
   (t (apply orig uri args))))

(with-eval-after-load 'arxana-docbook-ui
  (advice-add 'arxana-docbook-open-uri :around #'arxana-fair-docbook-uri-advice))

(provide 'arxana-fair)
;;; arxana-fair.el ends here
