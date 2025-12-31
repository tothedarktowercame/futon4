;;; arxana-browser-graph.el --- Graph browser helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; List graph types from the Futon API for quick browsing.

;;; Code:

(require 'subr-x)
(require 'arxana-store)
(require 'arxana-relations)

(defgroup arxana-browser-graph nil
  "Graph browser helpers for Arxana."
  :group 'arxana)

(defun arxana-browser-graph--get (key data)
  (if (fboundp 'arxana-relations--get)
      (arxana-relations--get key data)
    (alist-get key data)))

(defun arxana-browser-graph--type-label (kind)
  (cond
   ((keywordp kind) (substring (symbol-name kind) 1))
   ((symbolp kind) (symbol-name kind))
   ((stringp kind) kind)
   (t (format "%s" kind))))

(defun arxana-browser-graph--type-items (body)
  (let ((types (arxana-browser-graph--get :types body))
        (items '()))
    (when (listp types)
      (dolist (pair types)
        (let* ((kind (car pair))
               (entries (cdr pair))
               (kind-label (arxana-browser-graph--type-label kind)))
          (dolist (entry entries)
            (let* ((id (arxana-browser-graph--get :id entry))
                   (parent (arxana-browser-graph--get :parent entry))
                   (inferred (arxana-browser-graph--get :inferred_parent entry)))
              (push (list :type 'graph-type
                          :label (or id "?")
                          :kind kind-label
                          :parent parent
                          :inferred-parent inferred)
                    items))))))
    (sort items (lambda (a b)
                  (string< (plist-get a :label)
                           (plist-get b :label))))))

(defun arxana-browser-graph-items ()
  "Return items for the Graph view."
  (if (fboundp 'arxana-store-types)
      (let ((body (arxana-store-types)))
        (if body
            (let ((items (arxana-browser-graph--type-items body)))
              (if items
                  items
                (list (list :type 'info
                            :label "No types returned"
                            :description "Futon /types response had no entries."))))
          (list (list :type 'info
                      :label "Graph unavailable"
                      :description "No Futon response for /types."))))
    (list (list :type 'info
                :label "Graph unavailable"
                :description "arxana-store-types not available."))))

(defun arxana-browser-graph-format ()
  [ ("Type" 32 t)
    ("Kind" 16 t)
    ("Parent" 36 t)
    ("Inferred" 8 t)])

(defun arxana-browser-graph-row (item)
  (vector (or (plist-get item :label) "")
          (or (plist-get item :kind) "")
          (or (plist-get item :parent) "")
          (if (plist-get item :inferred-parent) "yes" "")))

(defun arxana-browser-graph-open (item)
  "Open a Graph ITEM detail buffer."
  (let* ((id (plist-get item :label))
         (kind (plist-get item :kind))
         (parent (plist-get item :parent))
         (inferred (plist-get item :inferred-parent))
         (buf (get-buffer-create "*Arxana Graph Type*")))
    (if (and id (string= kind "entity") (fboundp 'arxana-relations-show-ego))
        (arxana-relations-show-ego id)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Type: %s\n" (or id "?")))
          (insert (format "Kind: %s\n" (or kind "")))
          (insert (format "Parent: %s\n" (or parent "")))
          (insert (format "Inferred parent: %s\n" (if inferred "yes" "no")))
          (view-mode 1)))
      (display-buffer buf))))

(provide 'arxana-browser-graph)

;;; arxana-browser-graph.el ends here
