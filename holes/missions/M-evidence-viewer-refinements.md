# Mission: Evidence Viewer Refinements

**Date:** 2026-02-25
**Draft:** 2 (Claude revision of Codex draft 1)
**Status:** IDENTIFY → MAP
**Blocked by:** None (WS-backed evidence stream online)
**Owner:** futon4 (Arxana), partnering with futon3c Mission Control + futon1a evidence store

## Motivation

The WS-backed evidence viewer now mirrors live coordination traffic (see
entry `e-daab087c-6141-4071-94f7-f16d848f47ed`). Functionally it works,
but the experience is bare-bones: dense tables, no affordances to surface
the latest entries, little context for tags/claims, and no cues that link
to related artifacts (missions, agents, code).

We need to lift the viewer from "debug dump" to "operator console" so
humans (and eventually agents) can treat it as a first-class surface when
auditing live work.

## Prerequisites

Before starting refinement work, two infrastructure bugs must be resolved:

1. **Static file directory serving (futon1a):** Requesting
   `/evidence-viewer/` returns `Content-Type: application/octet-stream`
   instead of serving `index.html`. Users must currently use the explicit
   path `/evidence-viewer/index.html`. The static file handler needs to
   serve `index.html` for directory requests.

2. **Valid-time ordering (futon3c, done):** Replicated evidence entries
   were sorted by transaction time (when they arrived on the server),
   not by `:evidence/at` (when they actually happened). Fixed in futon3c
   `xtdb_backend.clj` — `put-and-sync!` now passes `:evidence/at` as
   XTDB valid-time, so entries slot into correct chronological position
   regardless of replication delay. The viewer's sort order must use
   XTDB's valid-time axis (the default), not transaction time.

## Success Criteria

1. **Responsive frame:** Viewer renders cleanly on laptop + tablet widths;
   typography/spacing tuned for scanability (no inspector gymnastics
   required).
2. **Live-tail affordance:** A docked "Live feed" indicator (play/pause +
   jump-to-latest) that reflects WS activity so users know when new
   evidence lands.
3. **Entry context cards:** Each evidence row expands into a card with
   structured metadata (author, tags, references, linked mission/issue)
   plus quick links to Mission Control and repo source.
4. **Filter presets:** One-click chips for common queries (e.g.,
   `type:coordination`, `author:codex`, `tag:replicated`) with
   URL-shareable state via query string.
5. **Theme alignment:** Palette + iconography align with futon4 docbook +
   Arxana chrome; dark-mode parity.

## Scope

### In

- `dev/web/evidence-viewer/` — the actual viewer files:
  - `index.html` (layout + entry point)
  - `evidence.js` (main logic)
  - `evidence-api.js` (API client)
  - `evidence-render.js` (DOM rendering)
- `docs/` entries describing UI affordances and Mission Control integration
- Presentation of live WS payloads (already working — tune rendering)

### Out

- Back-end evidence ingestion mechanics (futon1a + futon3c already handle this)
- Mission Control query APIs (reuse existing `/api/alpha/evidence` endpoints)
- Rewrite of Arxana desktop app (this is a focused web-viewer update)
- Valid-time ordering fix (already landed in futon3c)

## Constraints & Patterns

- **Pattern anchors:** `social/listener-leases` (live feed reliability),
  `coordination/structured-events-only` (typed evidence entries),
  `realtime/surface-map` (viewer as a read-only surface in the surface map).
- **Evidence-first:** Every refinement ships with before/after screenshots
  recorded as evidence entries referencing this mission.
- **No blocking on futon3x:** Viewer must degrade gracefully if a repo is
  offline; cached metadata is acceptable for non-critical references.
- **Chronological ordering:** The viewer must sort entries by `:evidence/at`
  (XTDB valid-time), not by arrival/transaction time. This is critical for
  replicated entries which arrive after a 30-second delay.

## Phases

### Phase 0: Fix static serving (futon1a)

Fix the directory-to-index.html routing in futon1a's static file handler
so `/evidence-viewer/` serves `index.html` with `Content-Type: text/html`.
This is a precondition — users currently need the explicit path.

### Phase 1: Observe (UI audit)

- Capture current UX pain points (screenshots + narration).
- Note: current viewer is 4 static files (~12KB total), no build step.
- Record baseline: what renders, what's missing, what's confusing.
- Output: Audit doc referenced in PSR.

### Phase 2: Propose (layout + affordances)

- Low-fidelity wireframes for layout + live-tail indicator.
- Palette + typography tokens derived from futon4 docbook theme.
- Gate: human review with sign-off.

### Phase 3: Execute (implementation)

- Update frontend assets; componentize layout (header, filters, stream,
  detail card).
- Introduce live-tail indicator (WS connection status + scroll-to-latest).
- Implement filter chips with query-string persistence.

### Phase 4: Validate (smoke test)

- Smoke script: curl the endpoint, trigger synthetic evidence via
  `POST /api/alpha/evidence`, confirm the viewer updates.
- Manual runbook: open viewer in browser, send a message on IRC, confirm
  it appears in the timeline at the correct chronological position.
- No need for Playwright/Cypress — the viewer is 4 static files with no
  build step. A shell script or small Puppeteer script is proportionate.

### Phase 5: Integrate (docs + links)

- Update `docs/README-evidence.md` with new viewer affordances.
- Add Mission Control deep-link logic (e.g., open mission doc from entry
  card).
- Emit PUR summarizing acceptance checklist.

## Acceptance Checklist

| ID | Description | Verification |
|----|-------------|--------------|
| EV-0 | `/evidence-viewer/` serves index.html with correct content-type | `curl -I` returns `text/html` |
| EV-1 | Viewer layout responsive 320px–1400px without horizontal scroll | Manual screenshots at both widths |
| EV-2 | Live-tail indicator shows WS connection status + scroll-to-latest | Manual: disconnect WS, observe indicator change |
| EV-3 | Evidence entry card surfaces author, tags, `:ref/type`, mission links | Manual with real data |
| EV-4 | Filter chips persist to URL, deep-linkable | Copy URL, open in fresh tab, state restored |
| EV-5 | Dark/light themes consistent with futon4 palettes | Visual comparison with Arxana chrome |
| EV-6 | Entries sorted by `:evidence/at` (valid-time), not arrival time | Send replicated entry, confirm position in timeline |

## Evidence Plan

- **PSR:** `social/listener-leases` for live feed reliability;
  `realtime/surface-map` for viewer-as-surface positioning.
- **Cycle evidence:** Each phase emits an entry tagged `:evidence-viewer`
  with screenshots stored in `dev/web/evidence-viewer/shots/`.
- **PUR:** On completion, record before/after metrics + user confirmation
  (Joe reloading the viewer and seeing entries in correct order).
- **Gate linkage:** Results submitted through futon3b `gate-check`.

## Draft History

- **Draft 1** (Codex, dc11725): Initial mission spec.
- **Draft 2** (Claude): Corrected `:in` paths to actual file locations
  (`dev/web/evidence-viewer/` not `resources/public/`). Added Phase 0
  for static serving bug. Added valid-time prerequisite (futon3c fix
  already landed). Removed Playwright/Cypress in favour of proportionate
  smoke testing. Removed criterion 6 (viewer emitting PURs) — the viewer
  is a passive read surface, PURs come from agents doing the work.
  Referenced `realtime/surface-map` pattern. Added EV-0 and EV-6 to
  acceptance checklist.
