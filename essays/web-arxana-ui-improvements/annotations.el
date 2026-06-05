;;; annotations.el --- Annotated-Edition manifest for the WebArxana UI Improvements mission HEAD -*- lexical-binding: t; -*-

;; Author: Joseph Corneli + Claude (claude-7 running the eoi-engine)
;; Generated: 2026-05-30
;; Scope: HEAD of M-web-arxana-ui-improvements (futon4), authored via a
;;        simulated `eoi new` eoi-mission-head flash (Right-View-only;
;;        round-collapse false).  The mission's Right-View invariant is
;;        "WebArxana provides a coherent visual interface to live
;;        graph-structured information about the stack, both for reading
;;        and writing."
;; Source markdown: /home/joe/code/futon4/holes/missions/M-web-arxana-ui-improvements.md
;;        (the artefact is the mission doc itself; per the Arxana per-repo
;;         convention the source .md stays in holes/missions/ and :source-file
;;         carries the absolute path).
;; Flash profile: eoi-mission-head (inward mission HEAD; clarity-probe
;;        replaces the diagnostic-probe slot; round-collapse false).
;; Engine spec:   /home/joe/code/algorithms/eoi-engine.md
;; Finalisation:  /home/joe/code/algorithms/eoi-finalisation.md
;; Buckets entry: eoi_instances/web-arxana-ui-improvements-HEAD-2026-05-30
;; Institution-object: :node/web-arxana-live-interface (self-authored, constituted)

;;; Commentary:
;; Thin manifest for the Essays importer (arxana-browser-essays.el), per
;; README-essays.md: this file declares the Essay entity and the flat `##`
;; sections ONLY, with an empty `:annotations ()`.  The authoritative
;; annotation layer is the sibling `annotations.edn` (Arxana v2 / EOI
;; finalisation schema) — author and edit annotations there, not here.
;;
;; Sectioning rationale: this is a Right-View-only HEAD, so there is a
;; single path-arrow section (Right View, §2).  The remaining sections are
;; the HEAD lifecycle slots Joe actually filled: what's-already-felt-true
;; (§3, the current-deficiencies diagnosis + the live-graph wish),
;; clarity-gap (§4, the inward clarity-probe), the two address-now
;; commitments (§5 the "live" contract; §6 integration-is-the-proof-point),
;; the carried-forward tensions (§7), and the engine-supplied phase outline
;; (§8).  Anti-glibness and working-economy HEAD slots were not collected
;; (Right-View-only flash) and are carried to IDENTIFY (T6) rather than
;; fabricated — I7 inhabited-mode.
;;
;; Voice-preserving copy-edits applied at assembly (per eoi-engine spec
;; §Assembly invariants — typo corrections a competent copy-editor would
;; make without consulting the author):
;;   - "deficiences" -> "deficiencies" (§3)
;;   - "activited"   -> "activated"    (§3)
;;   - "rosolution"  -> "resolution"   (carried-forward T3 paraphrase only;
;;                                      Joe's verbatim "good scale and
;;                                      resolution" preserved in §3-adjacent
;;                                      quote handling)
;;   - "useable"     -> "usable"       (tension list T4 only)
;; Preserved (Joe-domain / intentional):
;;   - "Web Arxana" (two words) vs "WebArxana" — Joe used both; the §4
;;     clarity-probe verbatim keeps "Web Arxana" as written.
;;   - "(indeed!)" — Joe's own aside in the clarity-probe; preserved.

;;; Code:

(require 'arxana-browser-essays)

(defconst arxana-browser-essays-web-arxana-ui-improvements-manifest
  '(:version 1
    :essay (:id "arxana/essay/web-arxana-ui-improvements-v1"
            :name "WebArxana UI Improvements — Mission HEAD (Right View, 2026-05-30)"
            :type "arxana/essay"
            :source-file "/home/joe/code/futon4/holes/missions/M-web-arxana-ui-improvements.md"
            :props
            ((version . "HEAD-v1")
             (mission . "M-web-arxana-ui-improvements (futon4)")
             (flash . ":eoi-mission-head")
             (round . "inward mission HEAD; Right-View-only")
             (round-collapse . "false")
             (institution-object-ref . ":node/web-arxana-live-interface")
             (eoi-instance-ref . "web-arxana-ui-improvements-HEAD-2026-05-30")
             (composite-arrow . "right-view")
             (clarity-probe . "org-roam-server#35 / what the 'live' contract usefully means; Evidence-Store <-> Mission-structure bridge")
             (tensions-tagged . "T1 address-now (live contract, §5); T2 address-now (integration proof-point, §6); T3 decision-debt (Interest Network feature-vs-filter); T4 deferred (Mission Search approaches); T5 warm-up (Meaning Map); T6 carried (unfilled HEAD slots)")
             (status . "HEAD complete; IDENTIFY pending; loaded into Arxana Essays for annotation")
             (generated . "2026-05-30")))

    :sections
    ((:id "arxana/essay/web-arxana-ui-improvements-v1/section/1-head"
      :name "§1. HEAD"
      :type "arxana/essay-section"
      :props ((index . 1)
              (heading-level . 2)
              (heading-text . "HEAD")
              (path-arrow . nil)))

     (:id "arxana/essay/web-arxana-ui-improvements-v1/section/2-right-view"
      :name "§2. Right View — the invariant"
      :type "arxana/essay-section"
      :props ((index . 2)
              (heading-level . 2)
              (heading-text . "Right View — the invariant")
              (path-arrow . "right-view")))

     (:id "arxana/essay/web-arxana-ui-improvements-v1/section/3-already-felt-true"
      :name "§3. What's already felt to be true"
      :type "arxana/essay-section"
      :props ((index . 3)
              (heading-level . 2)
              (heading-text . "What's already felt to be true")
              (path-arrow . nil)))

     (:id "arxana/essay/web-arxana-ui-improvements-v1/section/4-clarity-gap"
      :name "§4. Clarity-gap — what isn't yet clear"
      :type "arxana/essay-section"
      :props ((index . 4)
              (heading-level . 2)
              (heading-text . "Clarity-gap — what isn't yet clear")
              (path-arrow . nil)))

     (:id "arxana/essay/web-arxana-ui-improvements-v1/section/5-live-contract"
      :name "§5. The \"live\" contract (Emacs <-> browser, both directions)"
      :type "arxana/essay-section"
      :props ((index . 5)
              (heading-level . 2)
              (heading-text . "The \"live\" contract (Emacs ↔ browser, both directions)")
              (path-arrow . nil)
              (tension-id . "T1-live-contract")
              (tag . "address-now")))

     (:id "arxana/essay/web-arxana-ui-improvements-v1/section/6-integration-proof-point"
      :name "§6. Integration is the proof-point, not UI polish"
      :type "arxana/essay-section"
      :props ((index . 6)
              (heading-level . 2)
              (heading-text . "Integration is the proof-point, not UI polish")
              (path-arrow . nil)
              (tension-id . "T2-evidence-store-mission-bridge")
              (tag . "address-now")))

     (:id "arxana/essay/web-arxana-ui-improvements-v1/section/7-carried-forward-tensions"
      :name "§7. Carried-forward tensions"
      :type "arxana/essay-section"
      :props ((index . 7)
              (heading-level . 2)
              (heading-text . "Carried-forward tensions")
              (path-arrow . nil)))

     (:id "arxana/essay/web-arxana-ui-improvements-v1/section/8-phase-outline"
      :name "§8. Phase outline (post-HEAD)"
      :type "arxana/essay-section"
      :props ((index . 8)
              (heading-level . 2)
              (heading-text . "Phase outline (post-HEAD)")
              (path-arrow . nil))))

    :annotations
    ())
  "Annotated-Edition manifest for the WebArxana UI Improvements mission HEAD.")

(provide 'web-arxana-ui-improvements-annotations)
;;; annotations.el ends here
