;;; arxana-docbook.el --- Futon doc book browser -*- lexical-binding: t; -*-

;;; Commentary:
;; Browse filesystem-backed doc book entries (pilot) inside Emacs. Entries are
;; stored under docs/docbook/<book>/*.org (with toc.json for order).
;; This view keeps a separation between the
;; reading buffer and the source files (a “yad”/hand separation), but still lets
;; you jump to the underlying artifacts when needed.
;;
;; TODO(org-sync): Mirror doc book browser into XTDB docs once the UI/fields
;; stabilize.

;;; Code:

(require 'arxana-docbook-core)
(require 'arxana-docbook-checkout)
(require 'arxana-docbook-remote)
(require 'arxana-docbook-toc)
(require 'arxana-docbook-export)
(require 'arxana-docbook-ui)













































































;; TODO(org-sync): Track docbook function browsing/jump UI in XTDB docs (see docs-backlog).















































(provide 'arxana-docbook)

;;; arxana-docbook.el ends here
