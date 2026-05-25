;;; arxana-vsatarcs-consent.el --- Consent-gate (R6 integral, substitutable) -*- lexical-binding: t; -*-

;;; Commentary:
;; Consent-gate for VSATARCS writer-capability.  Per D1 + PSR-A1 of
;; M-vsatarcs-writer, the consent-gate is implemented as a *function*
;; with a *pluggable response source* — not as a UX-coupled prompt.
;; The architectural promise is that supervised → autonomous migration
;; is a source-swap at one location.
;;
;; Public entry: `arxana-vsatarcs-consent-gate' takes a consent-request
;; plist and a source keyword, returns a consent-response plist.
;;
;; Response sources implemented:
;;   :operator       — Emacs minibuffer prompt; the L3 default
;;   :autopen        — applies registered autopen rules; nil if no rule matches
;;   :cached-policy  — placeholder for L5; currently signals (:response :abstain-for-now)
;;
;; Consent-request shape (plist):
;;   (:request-id "uuid"
;;    :proposed-action <action-plist>
;;    :rationale "human-readable"
;;    :predicted-effects <plist>
;;    :class :mission-doc-sync
;;    :stakes :low | :medium | :high
;;    :reversibility :trivial | :reversible | :hard
;;    :match-type :clean | :scope-creep | :pivot
;;    :timestamp "ISO-8601")
;;
;; Consent-response shape (plist):
;;   (:request-id "uuid"
;;    :response :confirm | :reject | :ignore | :abstain-for-now
;;    :source :operator | :autopen | :cached-policy
;;    :source-id nil | "rule-id" | "policy-id"
;;    :rationale "optional string"
;;    :timestamp "ISO-8601")

;;; Code:

(require 'cl-lib)
(require 'arxana-vsatarcs-trace) ; for --now-iso

(defgroup arxana-vsatarcs-consent nil
  "Consent-gate for VSATARCS writer-capability (R6, substitutable)."
  :group 'arxana-vsatarcs)

(defconst arxana-vsatarcs-consent-version 1
  "Schema version for consent request/response shapes.")

;; ---------------------------------------------------------------------
;; Autopen registry — opt-in per (action-class, match-type) pair (D6)
;; ---------------------------------------------------------------------

(defcustom arxana-vsatarcs-consent-autopen-rules
  '((:mission-doc-sync :clean :reversibility :trivial
                       :rule-id "mission-doc-sync-clean-trivial")
    (:aif-edn-revision-entry :clean :reversibility :trivial
                              :rule-id "aif-edn-revision-entry-clean-trivial")
    (:story-update :clean :reversibility :trivial
                    :rule-id "story-update-clean-trivial"))
  "Registered autopen rules — opt-in per D6.
Each rule is a plist with at minimum :rule-id, :class-match (the
action :type), and conditions on the consent-request shape.  Initially
shipped pairs: (:mission-doc-sync, :clean, :trivial-reversibility) and
(:aif-edn-revision-entry, :clean, :trivial-reversibility) — both
self-documentation-family classes.  All other (class, match-type) pairs
require :operator source."
  :type '(repeat plist)
  :group 'arxana-vsatarcs-consent)

;; ---------------------------------------------------------------------
;; Public entry
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-consent-gate (request source)
  "Emit consent REQUEST and return a response from SOURCE.
SOURCE is one of :operator, :autopen, :cached-policy.  The substitutability
promise (D1) is that the rest of the writer-pipeline doesn't change
based on the source choice."
  (cl-assert (plist-get request :request-id))
  (cl-assert (plist-get request :proposed-action))
  (cl-case source
    (:operator (arxana-vsatarcs-consent--operator request))
    (:autopen (arxana-vsatarcs-consent--autopen request))
    (:cached-policy (arxana-vsatarcs-consent--cached-policy request))
    (t (error "Unknown consent source: %s" source))))

;; ---------------------------------------------------------------------
;; Source implementations
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-consent--operator (request)
  "Operator source — minibuffer prompt for confirm/reject/ignore/abstain.
Surfaces the consent-request fields and returns the operator's chosen
response.  Per the 'autopen rehearsal' success criterion in VERIFY 5.b,
when an autopen rule WOULD have matched, the prompt surfaces that as a
hint without taking the autopen action."
  (let* ((action (plist-get request :proposed-action))
         (rationale (or (plist-get request :rationale) "(no rationale)"))
         (match-type (plist-get request :match-type))
         (stakes (plist-get request :stakes))
         (reversibility (plist-get request :reversibility))
         (autopen-hint (when (arxana-vsatarcs-consent--autopen-would-match request)
                         "  [autopen rule WOULD match in autopen mode]"))
         (prompt (format
                  (concat
                   "Consent request:\n"
                   "  Action: %s  target: %s\n"
                   "  Rationale: %s\n"
                   "  Match-type: %s | Stakes: %s | Reversibility: %s%s\n"
                   "  Response — c=confirm  r=reject  i=ignore  a=abstain-for-now: ")
                  (plist-get action :type)
                  (or (plist-get action :target-checkpoint)
                      (plist-get action :target-file)
                      "(no target)")
                  rationale match-type stakes reversibility
                  (or autopen-hint "")))
         (input (read-char-choice prompt '(?c ?r ?i ?a))))
    (arxana-vsatarcs-consent--build-response
     request
     (cl-case input
       (?c :confirm) (?r :reject) (?i :ignore) (?a :abstain-for-now))
     :operator nil
     (read-from-minibuffer "Optional rationale (empty to skip): "))))

(defun arxana-vsatarcs-consent--autopen (request)
  "Autopen source — apply registered rules; return response or nil.
Returns nil if no autopen rule matches; caller should fall back to
operator source (or :composed source handles that fallback)."
  (let ((rule (arxana-vsatarcs-consent--autopen-find-rule request)))
    (when rule
      (arxana-vsatarcs-consent--build-response
       request :confirm :autopen
       (plist-get rule :rule-id)
       (format "auto-confirmed by rule %s" (plist-get rule :rule-id))))))

(defun arxana-vsatarcs-consent--autopen-would-match (request)
  "Return non-nil iff an autopen rule WOULD match REQUEST.
Used by :operator source to surface the autopen-rehearsal hint without
actually applying the rule."
  (not (null (arxana-vsatarcs-consent--autopen-find-rule request))))

(defun arxana-vsatarcs-consent--autopen-find-rule (request)
  "Return the first autopen rule matching REQUEST, or nil."
  (let ((action (plist-get request :proposed-action))
        (match-type (plist-get request :match-type))
        (reversibility (plist-get request :reversibility)))
    (cl-find-if
     (lambda (rule)
       (and (eq (car rule) (plist-get action :type))
            (eq (cadr rule) match-type)
            (eq (plist-get rule :reversibility) reversibility)))
     arxana-vsatarcs-consent-autopen-rules)))

(defun arxana-vsatarcs-consent--cached-policy (request)
  "Cached-policy source — placeholder for L5 historical lookup.
For L3, always returns :abstain-for-now (operator source would have
handled it)."
  (arxana-vsatarcs-consent--build-response
   request :abstain-for-now :cached-policy "placeholder-L5"
   "cached-policy source not yet implemented (L5 work)"))

;; ---------------------------------------------------------------------
;; Response builder
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-consent--build-response (request response source source-id rationale)
  "Build a consent-response plist."
  (list :request-id (plist-get request :request-id)
        :response response
        :source source
        :source-id source-id
        :rationale (and (stringp rationale)
                        (not (string-empty-p rationale))
                        rationale)
        :timestamp (arxana-vsatarcs-trace--now-iso)))

;; ---------------------------------------------------------------------
;; Request constructor
;; ---------------------------------------------------------------------

(defun arxana-vsatarcs-consent--gen-request-id ()
  "Return a fresh consent-request id."
  (format "cr:%s:%04x"
          (format-time-string "%Y%m%dT%H%M%S")
          (random #x10000)))

(cl-defun arxana-vsatarcs-consent-build-request
    (&key proposed-action rationale predicted-effects class stakes
          reversibility match-type)
  "Build a consent-request plist from its component fields.
Generates a fresh :request-id and :timestamp."
  (cl-assert proposed-action)
  (list :request-id (arxana-vsatarcs-consent--gen-request-id)
        :proposed-action proposed-action
        :rationale rationale
        :predicted-effects predicted-effects
        :class class
        :stakes stakes
        :reversibility reversibility
        :match-type match-type
        :timestamp (arxana-vsatarcs-trace--now-iso)))

(provide 'arxana-vsatarcs-consent)
;;; arxana-vsatarcs-consent.el ends here
