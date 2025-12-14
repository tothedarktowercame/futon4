;;; arxana-export.el --- Org exporters for Arxana -*- lexical-binding: t; -*-

;;; Commentary:
;; Helpers that write the current article table back to Org files so
;; XTDB snapshots can be shared as portable bundles.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(require 'arxana-article nil t)

(declare-function arxana-article--labels-for "arxana-article" (name))
(declare-function get-article "arxana-tangled" (name))
(declare-function scholium-name "arxana-tangled" (article))
(declare-function scholium-text "arxana-tangled" (article))
(declare-function scholium-about "arxana-tangled" (article))
(declare-function futon4--article-id-for "arxana-tangled" (name &optional path))
(declare-function link-type-accessor "arxana-tangled" (link type))
(declare-function arxana-store-save-snapshot "arxana-store" (&optional scope label))
(declare-function arxana-store--snapshot-scope-prompt "arxana-store" (&optional prompt default))
(declare-function arxana-store--snapshot-id-from-response "arxana-store" (response))
(declare-function arxana-store-sync-enabled-p "arxana-store" ())
