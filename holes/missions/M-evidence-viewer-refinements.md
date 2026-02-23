# Mission: Evidence Viewer Refinements

**Date:** 2026-02-25  
**Status:** IDENTIFY → MAP  
**Blocked by:** None (WS-backed evidence stream online)  
**Owner:** futon4 (Arxana), partnering with futon3c Mission Control + futon1a evidence store

## Motivation

The WS-backed evidence viewer now mirrors live coordination traffic (see entry `e-daab087c-6141-4071-94f7-f16d848f47ed`). Functionally it works, but the experience is bare-bones: dense tables, no affordances to surface the latest entries, little context for tags/claims, and no cues that link to related artifacts (missions, agents, code).

We need an approachable mission to lift the viewer from "debug dump" to "operator console" so humans (and eventually agents) can treat it as a first-class surface when auditing live work.

## Success Criteria

1. **Responsive frame:** Viewer renders cleanly on laptop + tablet widths; typography/spacing tuned for scanability (no inspector gymnastics required).
2. **Live-tail affordance:** A docked "Live feed" indicator (play/pause + jump-to-latest) that reflects WS activity so users know when new evidence lands.
3. **Entry context cards:** Each evidence row expands into a right-hand card with structured metadata (author, tags, references, linked mission/issue) plus quick links to Mission Control and repo source.
4. **Filter presets:** One-click chips for common queries (e.g., `sigil/coordination`, `agent:codex`, `mission:M-peripheral-gauntlet`) with URL-shareable state.
5. **Theme alignment:** Palette + iconography align with futon4 docbook + Arxana chrome (no mismatched blues/reds); dark-mode parity.
6. **Traceability hooks:** Each UI feature references the underlying evidence schema and emits its own PUR (pattern usage record) so futon3b proof-paths can cite the refinements.

## Scope

### In

- `resources/public/evidence-viewer/**/*` (HTML, CSS, JS for the viewer app)
- `src/futon4/viewer/` (Clojure/CLJS backing the viewer, if applicable)
- `docs/` entries describing UI affordances and Mission Control integration
- Plumbing that reads live WS payloads (already working—tune presentation)

### Out

- Back-end evidence ingestion mechanics (futon1a + futon3c already emit correctly)
- Mission Control query APIs (reuse existing `/evidence` endpoints)
- Rewrite of Arxana desktop app (this is a focused web-viewer update)

## Constraints & Patterns

- **Pattern anchors:** `social/gauntlet/arena`, `social/listener-leases`, `coordination/structured-events-only`, `presentation/legible-defaults`.
- **Evidence-first:** Every refinement ships with before/after screenshots + timestamps recorded as evidence entries referencing this mission.
- **No blocking on futon3x:** Viewer must degrade gracefully if a repo is offline; cached metadata is acceptable for non-critical references.

## Phases

1. **Observe (UI audit)**  
   - Capture current UX pain points (screenshots + narration).  
   - Record baseline metrics (largest contentful paint, WS latency indicator).  
   - Output: Audit doc referenced in PSR.

2. **Propose (wireframes + style guide)**  
   - Low-fidelity wireframes for layout + live-tail indicator.  
   - Palette + typography tokens derived from futon4 docbook theme.  
   - Gate: human review w/ sign-off by futon4 maintainer.

3. **Execute (implementation)**  
   - Update frontend assets; ensure componentized layout (header, filters, stream, detail card).  
   - Introduce WS status widget + toast notifications.  
   - Implement filter chips w/ query-string persistence.

4. **Validate (tests + smoke)**  
   - Browser-based regression (Playwright/Cypress) covering filters, live-tail, and expansion cards.  
   - Manual runbook: open viewer, trigger synthetic evidence (via futon3c dev helper), confirm UI updates.

5. **Integrate (docs + Mission Control link)**  
   - Document new affordances in `docs/README-evidence.md`.  
   - Add Mission Control deep-link logic (e.g., open mission doc from entry card).  
   - Emit PUR summarizing acceptance checklist.

## Acceptance Checklist

| ID | Description | Verification |
|----|-------------|--------------|
| EV-1 | Viewer layout responsive 320px–1400px without horizontal scroll | Lighthouse + manual screenshots |
| EV-2 | Live-tail indicator shows WS connection status + scroll-to-latest control | Automated WS mock test |
| EV-3 | Evidence entry card surfaces author, tags, `:ref/type`, mission links | UI test + real data sample |
| EV-4 | Filter chips persist to URL, deep-linkable | Copy URL, open in fresh browser, state restored |
| EV-5 | Dark/light themes consistent with futon4 palettes | Visual diff or design token snapshot |
| EV-6 | PUR emitted with pointer to docs + code commits | Evidence entry referencing mission + commit hash |

## Evidence Plan

- **PSR:** `social/listener-leases` for live feed reliability; `presentation/legible-defaults` for UX.  
- **Cycle evidence:** Each phase emits an entry tagged `:evidence-viewer` with screenshots or screencasts stored in `resources/viewer-shots/`.  
- **PUR:** On completion, record before/after metrics + user confirmation (Joe pressing ENTER and seeing immediate updates).  
- **Gate linkage:** Results submitted through futon3b `gate-check` referencing Gauntlet Gate 0 (Arena) UX polishing.
