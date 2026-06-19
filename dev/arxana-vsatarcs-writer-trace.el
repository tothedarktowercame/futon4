;;; arxana-vsatarcs-writer-trace.el --- R8 writer-event record schema -*- lexical-binding: t; -*-

;;; Commentary:
;; Extends the VSATARCS R8 trace with writer-event records — the
;; per-cycle audit log for writer-capability (M-vsatarcs-writer).  Uses
;; the existing `arxana-vsatarcs-trace' I/O primitives; this module
;; only defines the writer-event record shape and a thin emit wrapper.
;;
;; Writer-event record shape (plist):
;;   (:writer-event-id "uuid"
;;    :timestamp "ISO-8601"
;;    :writer-action-class :mission-doc-sync
;;    :proposed-action <action-plist>
;;    :consent-request <consent-request-plist>
;;    :consent-response <consent-response-plist>
;;    :executed-action <execution-plist> ; nil if rejected/ignored
;;    :predicted-post-state <plist>
;;    :observed-post-state <plist>       ; nil if not executed
;;    :prediction-error <float>          ; nil if not executed
;;    :G-breakdown <plist>               ; R-A3; nil until R5 lands
;;    :trace-kind :writer-event)
;;
;; Per D3 + DL4: writer-events are PRIMARY records in writer-trace;
;; bilateral-evidence entries cross-reference them via the 6th
;; :evidence-kind :consent-gated-writer-event.  This module is the
;; canonical home for the record schema.

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-trace)

(defgroup arxana-vsatarcs-writer-trace nil
  "R8 writer-event record schema for VSATARCS writer-capability."
  :group 'arxana-vsatarcs)

(defconst arxana-vsatarcs-writer-trace-version 1
  "Schema version for writer-event records.")

;; ---------------------------------------------------------------------
;; Record constructor
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-writer-trace--gen-id ()
  "Return a fresh writer-event-id string.
Uses a timestamp + random suffix; not a true UUID but unique enough for
single-operator-cadence writer-events."
  (format "we:%s:%04x"
          (format-time-string "%Y%m%dT%H%M%S")
          (random #x10000)))

(cl-defun arxana-vsatarcs-writer-trace-build-record
    (&key proposed-action consent-request consent-response
          executed-action predicted-post-state observed-post-state
          prediction-error g-breakdown)
  "Build a writer-event trace record from the named components.
PROPOSED-ACTION is required.  CONSENT-RESPONSE may be nil if the
record represents a pre-consent abort (e.g., admissibility failure).
EXECUTED-ACTION + OBSERVED-POST-STATE + PREDICTION-ERROR are nil
unless the cycle reached execution."
  (cl-assert proposed-action)
  (list :writer-event-id (arxana-vsatarcs-writer-trace--gen-id)
        :timestamp (arxana-vsatarcs-trace--now-iso)
        :writer-action-class (plist-get proposed-action :type)
        :proposed-action proposed-action
        :consent-request consent-request
        :consent-response consent-response
        :executed-action executed-action
        :predicted-post-state predicted-post-state
        :observed-post-state observed-post-state
        :prediction-error prediction-error
        :G-breakdown g-breakdown
        :trace-kind :writer-event))

;; ---------------------------------------------------------------------
;; Emit
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-writer-trace-emit (record &optional path)
  "Append RECORD to the daily VSATARCS trace file.
Delegates to `arxana-vsatarcs-trace-emit'; writer-event records share
the same daily file as reader-tick records, distinguished by
`:trace-kind :writer-event'.  PATH overrides the default daily file."
  (cl-assert (eq :writer-event (plist-get record :trace-kind)))
  (arxana-vsatarcs-trace-emit record path))

;; ---------------------------------------------------------------------
;; Read filters
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-writer-trace-read-events (&optional date)
  "Return all writer-event records from DATE's trace file.
DATE defaults to today.  Filters `arxana-vsatarcs-trace-read-all' by
`:trace-kind :writer-event'."
  (cl-remove-if-not
   (lambda (r) (eq :writer-event (plist-get r :trace-kind)))
   (arxana-vsatarcs-trace-read-all date)))

(defun arxana-vsatarcs-writer-trace-read-by-class (class &optional date)
  "Return writer-event records of action-class CLASS from DATE's trace.
DATE defaults to today."
  (cl-remove-if-not
   (lambda (r) (eq class (plist-get r :writer-action-class)))
   (arxana-vsatarcs-writer-trace-read-events date)))

(provide 'arxana-vsatarcs-writer-trace)
;;; arxana-vsatarcs-writer-trace.el ends here
