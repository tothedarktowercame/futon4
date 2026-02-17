;;; arxana-hypergraph-demo.el --- Standalone hypergraph demo launcher -*- lexical-binding: t; -*-

;;; Commentary:
;; Launch Arxana directly into the local hypergraph viewer without depending on
;; futon0 hot-reload helpers.

;;; Code:

(require 'seq)
(require 'subr-x)

(defconst arxana-hypergraph-demo--dev-root
  (file-name-directory (or load-file-name (buffer-file-name) default-directory))
  "Directory that contains Arxana dev modules.")

(add-to-list 'load-path arxana-hypergraph-demo--dev-root)

(require 'arxana-browser)
(require 'arxana-browser-hypergraph)

(declare-function arxana-browser--render "arxana-browser-core")
(declare-function arxana-browser-hypergraph-open-arxana "arxana-browser-hypergraph" (item))

(defcustom arxana-hypergraph-demo-default-source
  (expand-file-name "../../futon6/data/first-proof/thread-633512-hypergraph.json"
                    arxana-hypergraph-demo--dev-root)
  "Default hypergraph source used by `arxana-hypergraph-demo-open'."
  :type 'file
  :group 'arxana)

(defconst arxana-hypergraph-demo--menu-item
  (list :type 'menu
        :label "Hypergraphs"
        :description "Inspect local hypergraph JSON datasets."
        :view 'hypergraph)
  "Menu context used to render the Hypergraphs browser view.")

;;;###autoload
(defun arxana-hypergraph-demo-open (&optional source)
  "Open Arxana hypergraph HTML view for SOURCE.

If SOURCE is nil, use `arxana-hypergraph-demo-default-source'."
  (interactive)
  (let ((source-path (expand-file-name
                      (or source arxana-hypergraph-demo-default-source))))
    (unless (file-readable-p source-path)
      (user-error "Hypergraph source not readable: %s" source-path))
    (setq arxana-browser-hypergraph-sources (list source-path))
    (setq arxana-browser-hypergraph-open-style 'arxana)
    (arxana-browser-browse)
    (setq arxana-browser--stack (list arxana-hypergraph-demo--menu-item))
    (setq arxana-browser--context nil)
    (arxana-browser--render)
    (let* ((items (arxana-browser-hypergraph-items))
           (item (or (seq-find (lambda (entry)
                                 (string= (expand-file-name source-path)
                                          (expand-file-name
                                           (or (plist-get entry :path) ""))))
                               items)
                     (car items))))
      (when item
        (arxana-browser-hypergraph-open-arxana item)))
    (message "Arxana hypergraph demo loaded: %s" source-path)))

;;;###autoload
(defun arxana-hypergraph-demo-open-default ()
  "Open Arxana hypergraph demo using `arxana-hypergraph-demo-default-source'."
  (interactive)
  (arxana-hypergraph-demo-open arxana-hypergraph-demo-default-source))

(provide 'arxana-hypergraph-demo)

;;; arxana-hypergraph-demo.el ends here
