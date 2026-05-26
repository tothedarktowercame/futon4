# M-essay-corpus-substrate: Uniform essay import per `~/code/futonN/essays/` convention

**Date:** 2026-05-21 (HEAD authored from operator dialogue; migration completed same day; handoff prepared for Codex).
**Status:** Stage 1 partial-complete. D1 (home convention) + D3 (first-essay migration) landed; D2 (the essay projector in futon3c) is the outstanding work and is the scope of the Codex handoff below.
**Owner:** Joe (operator); claude-10 (Stage 1 author); Codex (D2 implementor — see handoff section).
**Blocked by:** None. Builds on existing watcher + ingest substrate.
**Blocks:** `M-editorial-assistant` Stage 1 — the editorial-log lands on imported essays so `annotations.edn` round-trips to XTDB by default instead of by aspiration. Note: `M-editorial-assistant`'s UI parts (the `M-x arxana-vsatarcs-editorial-*` commands and the chrome block) can proceed in parallel against the migrated essay's sidecar `.edn`; only the WebArxana-visibility completion criterion depends on D2.
**Companion missions:** `M-editorial-assistant` (downstream consumer); `M-essays-edit-cycle` (uses the same per-essay home as its working substrate); `M-self-documenting-stack` (essays are part of the corpus that documents the stack).

## HEAD — Operator-voice anchor

> "How about a pattern whereby `~/code/futonN/essays/` is scanned for each futonN (including items like 7a). For now, we move the Operator's Foreword to futon7a because that's where it's going to be published."
>
> "Essays are more free-form structurally, and, I guess in principle, they can be put anywhere. Still, `~/npt/` is really the *wrong* place for an essay about the futon stack — futon7 or futon7a would be a much more sensible for that particular content. But, even if we eventually allow essays to be scattered all over, wouldn't it make sense for now centralise our content into one place in the file system, and make sure it is processed in a uniform way like patterns are, so that we're not second-guessing the fundamentals?"
>
> — Joe, 2026-05-21 emacs-repl

### What's already felt to be true

Pattern import works. `futon3c.watcher.file-ingest` dispatches by extension to projectors that POST hyperedges to futon1a; `.flexiarg` is the canonical example. Patterns live in `~/code/futon3/library/<topic>/*.flexiarg` — a canonical home with a uniform shape. The substrate runs automatically and is the reason `pattern-library://agent` is structurally checked and up-to-date in XTDB.

Essays do not have this. The Operator's Foreword lives in `~/npt/operator-foreword/` (which is the wrong place — it's a futon7a-publication-bound essay), the catalog points there, and the WebArxana entity view at `arxana://essay/arxana%2Fessay%2Foperator-foreword-v1` shows 0 annotations even though the sidecar `annotations.edn` now carries 16 (15 at original discovery time). Two storage planes, no bridge: Plane A (sidecar `.edn` consumed by `arxana-browser-essays-compiled.el`) and Plane B (XTDB entities consumed by WebArxana) are out of sync because nothing imports A → B.

The failure mode (0 annotations where 16 now exist in the live sidecar; originally 15 at discovery time) is the smoking gun. The fix is not a one-off bridge for this essay; it is the same uniform-projection apparatus patterns already have, applied to essays.

### Anti-glibness discipline

The mission would be superficial if:

- The new home is declared but the projector is never written, so we just move files and the Plane A/B disconnect persists. Discipline: completion requires WebArxana showing 16 annotations for the migrated foreword.
- The projector is written but only handles `operator-foreword.md`; it doesn't generalise across the corpus. Discipline: the projector reads `<slug>.md + annotations.edn` for any subdir under any scanned `~/code/futonN/essays/`, and the first migration is its first real test, not its only test.
- We invent a parallel scan loop instead of extending the watcher. Discipline: the essay projector lives in `futon3c.watcher.projections.essay` and is dispatched by `file_ingest.clj` exactly like `flexiarg` — same shape, same idempotence contract, same POST endpoint.
- The migration breaks references silently. Discipline: audit before move (done — 1 external reference: the eoi flash profile); update after move; re-open the compiled view to confirm no regression.

### Working-economy position

This mission underwrites **M-editorial-assistant** Stage 1. Without it, the editorial log lives as an orphaned sidecar — operator-visible but XTDB-invisible, and the WebArxana surface is broken-by-design. With it, the editorial log is just another field in `annotations.edn` that the same projector imports.

It also unlocks **uniform essay handling across the stack** — futon7a publication targets, futon3 internal essays, future futon-N essays all get the same treatment. The pattern of "canonical home + extension-dispatched projector" is already proven; this mission applies it to a second file family.

Underwriting it: `futon3c.watcher.file-ingest` (the watcher), `arxana-store` (the POST API), `arxana-browser-essays.el` (the catalog reader that already knows about `:source-file` and sibling `annotations.edn`).

### Clarity-gap / carried-forward tensions

- **Where do essays in `~/code/futon7a/` live vs the existing `.html` outputs?** Current futon7a is full of `.html` files (`about.html`, `faq.html`, ...). The convention `~/code/futon7a/essays/<slug>/<slug>.md` keeps sources clearly separated from outputs, but it means the futon7a tree gains a new top-level subdir. Worth confirming this doesn't collide with any existing convention.
- **How are essay hyperedges shaped in XTDB?** Patterns are `pattern/component` entities + `:pattern/includes` relations. Essays should be `arxana/essay` entities (which already exist) + `arxana/annotation` hyperedges with `:role :annotated` / `:role :source` endpoints matching the .edn schema. The annotation projector needs to faithfully preserve the multi-endpoint shape — single-anchor and multi-anchor annotations both project to hyperedges with the same role structure.
- **What about the `arxana-browser-essays-podcasts.el` and `-wikibooks.el` siblings?** They add to `arxana-browser-essays-catalogs` programmatically. The new convention (`~/code/futonN/essays/`) should not break their catalogs; ideally they migrate to the same convention over time, but Stage 1 doesn't require it.
- **Reload vs restart for the projector.** Per `feedback_drawbridge_protocol_reload` and `feedback_reload_safety`, adding a new projection ns to the watcher must be reconstructible-from-disk — a fresh restart of futon3c reads the new projector at startup the same way a Drawbridge `:reload` would. Verify before relying on hot-reload.

### Provenance

Authored 2026-05-21 from emacs-repl dialogue between Joe (operator) and claude-10 (co-author). Sequence: Joe asked to start work on M-editorial-assistant; claude-10 discovered the Plane A/B disconnect (compiled view showed 15 annotations at that moment, WebArxana showed 0); Joe reframed the problem as a missing-essay-importer symptom and proposed the `~/code/futonN/essays/` scan pattern. The operator-voice anchor preserves Joe's two messages verbatim from that turn. The live sidecar now contains 16 annotations after the later survey-bootstrap-meta addition.

## 1. IDENTIFY (sketch — not yet hardened)

### The gap

The futon stack has uniform import for patterns (`.flexiarg` → projector → futon1a) but not for essays. The Operator's Foreword's sidecar `annotations.edn` carries 16 annotations that never reach XTDB; WebArxana's essay entity surface is structurally broken because the upstream pipeline doesn't exist. The same shape would apply to any other essay added to the stack.

### What this mission is

A three-deliverable build:

- **(D1)** Canonical essay-home convention: `~/code/futonN/essays/<slug>/{<slug>.md, annotations.edn}` for each futonN (N includes 7a, 7, 4, 3, 0, …). Scanned uniformly.
- **(D2)** `futon3c.watcher.projections.essay`: projector for the `<slug>.md` + sibling `annotations.edn` pair, dispatched by `file_ingest.clj` on `.md` files matching the essay-home pattern. Posts essay entity + annotation hyperedges to futon1a. Idempotent.
- **(D3)** First-essay migration + verification: move `~/npt/operator-foreword/` to `~/code/futon7a/essays/operator-foreword/`; update the 1 external reference (eoi flash profile); confirm WebArxana entity view shows 16 annotations.

### Completion criteria

1. The projector reads `<slug>.md` + sibling `annotations.edn`, projects to futon1a, and is idempotent.
2. `~/npt/operator-foreword/` no longer exists; its contents live at `~/code/futon7a/essays/operator-foreword/`.
3. `arxana://essay/arxana%2Fessay%2Foperator-foreword-v1` in WebArxana shows 16 annotations (not 0).
4. The compiled view (`arxana-browser-essays-open-compiled` for the foreword) still works against the new path, showing the same 16-annotation count it shows today.
5. One additional essay (any) is added under `~/code/futonN/essays/<slug>/` and verified to project end-to-end — proves the convention generalises, not just the first instance.

## Checkpoints

### HEAD — 2026-05-21

- Operator-voice anchor preserved verbatim from emacs-repl dialogue.
- Pattern-import substrate identified as the analogue (`futon3c.watcher.file-ingest`; flexiarg projector).
- Three deliverables sketched (home convention; projector; migration + verify).
- Reference-audit completed: 1 external ref + 3 self-refs + 1 XTDB entity field to update post-migration.

### Migration complete — 2026-05-21 (later in day)

D1 + D3 landed; D2 outstanding for Codex.

- **D1 (home convention)** — committed to this doc. Convention: `~/code/futonN/essays/<slug>/{<slug>.md, annotations.edn}` for any futonN including 7a. The watcher's scan path generalises across the futon family.
- **D3 (first-essay migration)** — `~/npt/operator-foreword/` → `~/code/futon7a/essays/operator-foreword/`. Four files moved; old dir removed; all five path references updated:
  - `/home/joe/code/futon7a/essays/operator-foreword/load-into-emacs.el` (7 paths)
  - `/home/joe/code/futon7a/essays/operator-foreword/annotations.edn` (2 header paths)
  - `/home/joe/code/futon7a/essays/operator-foreword/annotations.el` (1 header path)
  - `/home/joe/code/futon3/library/peripherals/head-flash/eoi-inward-operator-foreword.edn` (`:first-run-output`)
  - XTDB entity `arxana/essay/operator-foreword-v1` refreshed via re-running `load-into-emacs.el`; `:source-file` now points at the new path.
- **Sweep clean** — `grep -rln "npt/operator-foreword" ~/code/` returns no matches post-migration.
- **Compiled view** (`M-x arxana-browser-essays-open-compiled`) verified working against the new path; 16 annotations rendered.
- **Catalog-cache caveat surfaced.** `arxana-browser-essays--xtdb-catalog-cache` is set on first query and never auto-invalidated. After XTDB writes, operators must run `M-x arxana-browser-essays-refresh` to see the new state. Logged as D2's clarity-gap (see Codex handoff §Known caveats).

### Sibling change — 2026-05-21 (related but not in this mission's scope)

`arxana-browser-essays-compiled.el` — `--annotation-at-point` now selects the **narrowest overlapping overlay** as the tiebreaker when multiple annotations claim the same start-position. Without this, `overlays-at` returned overlapping overlays in unspecified order, so the selected annotation flickered between same-start neighbours (e.g., `hx:of:v1:opening-no-thesis` vs `hx:of:v1:scope-mush-naming` at point 405 in the foreword). Narrowest-wins gives the per-passage annotation precedence over a cross-section diagnostic that happens to anchor here. Pre-existing rendering issue; unblocked the canonical worked example in `M-editorial-assistant`; **not part of this mission's scope** — captured here for handoff context.

## Codex handoff — D2 (the essay projector)

This section follows the futon3c `R11` scope-bounded-handoff pattern. Suitable for transcribing into a GitHub issue per `futon3c/CLAUDE.md` §Codex Handoff Protocol.

### Title

Implement `futon3c.watcher.projections.essay` — auto-import `.md` + `annotations.edn` pairs from `~/code/futonN/essays/<slug>/` into futon1a.

### Goal

Close the Plane A / Plane B disconnect for the essay corpus. Today, sidecar `annotations.edn` files carry the annotation hypergraph (Plane A, consumed by `arxana-browser-essays-compiled.el`) but the corresponding XTDB entities (Plane B, consumed by WebArxana) are not populated — `arxana://essay/arxana%2Fessay%2Foperator-foreword-v1` shows 0 annotations even though 16 exist in the sidecar. D2 fixes this by adding an essay projector dispatched by `futon3c.watcher.file_ingest`, mirroring the existing flexiarg pattern.

### `:in` files (READ-ONLY — reference these; do not modify)

- `/home/joe/code/futon3c/src/futon3c/watcher/file_ingest.clj` — dispatch host; extension-based dispatch + `post-hyperedge!` to futon1a's `/api/alpha/hyperedge`. **The projector plugs in here.**
- `/home/joe/code/futon3c/src/futon3c/watcher/projections/flexiarg.clj` — reference projector. Mirror its shape (read text → parse → return projection map).
- `/home/joe/code/futon4/dev/arxana-browser-essays.el` — Emacs-side parallel; `arxana-browser-essays--import-manifest` is the existing manifest-driven importer. Read it for the entity shape it produces (essay entity + section entities); D2's projector should produce a compatible shape so the two paths can coexist while D2 takes over.
- `/home/joe/code/futon7a/essays/operator-foreword/annotations.edn` — canonical schema reference (schema version 1; 16 annotations across 11 sections; multi-endpoint hyperedges; `:role :annotated` / `:role :source` endpoint structure).
- `/home/joe/code/futon7a/essays/operator-foreword/operator-foreword.md` — first-essay prose; the projector's first real test case.

### `:out` files (CREATE these)

- `/home/joe/code/futon3c/src/futon3c/watcher/projections/essay.clj` — the projector namespace. Provides `collect-file` (mirrors `flexiarg/collect-file`) for projecting a single `.md` + sibling `annotations.edn` pair.
- Test file under `/home/joe/code/futon3c/test/futon3c/watcher/projections/essay_test.clj` — tests covering: parsing the operator-foreword fixture; multi-endpoint annotation projection; idempotence (re-projection produces no duplicates); a second-essay fixture (any small synthetic essay) to prove generality.

### `:patch` files (MODIFY these)

- `/home/joe/code/futon3c/src/futon3c/watcher/file_ingest.clj` — add the essay dispatch branch. The current dispatch is by file extension; essays use `.md` but only inside `<futonN>/essays/<slug>/<slug>.md`. **The path-shape predicate**, not just the extension, must trigger this projector — `.md` files outside the essay-home pattern (mission docs, READMEs) must NOT trigger it. The mission doc dispatch (`holes/missions/M-*.md`) already exists and must remain untouched.

### Input shape (per file event)

For each `<futonN>/essays/<slug>/<slug>.md`:

- Read the .md as prose body for the essay entity.
- Read sibling `annotations.edn` (if present). The .edn carries:
  - `:essay-id` (string, e.g. `arxana/essay/operator-foreword-v1`)
  - `:paper` (filename of the .md)
  - `:sections` (vector of `{:id :name :index :heading-level :aif-region :path-arrow}` maps)
  - `:annotations` (vector of annotations — each has `:id`, `:hx-type`, `:endpoints [{:role :annotated|:source :section-id :passage :pattern-name}]`, `:note`, `:severity`, `:status`, plus optional `:joe-decision-required?`, `:tied-to`, `:invariant`, `:candidate-mechanical-fix?`).
  - `:invariant-audit` (per-invariant rollup; project as metadata)
  - `:rewrite-pass-queue` (operator-classified queue; project as metadata)

### Output (futon1a hyperedge POSTs)

- One `arxana/essay` entity per `.md` (idempotent on `:essay-id`). Props include `:source-file` (absolute path), `:label`, `:description`, plus `:paper`, `:rewrite-pass-status` from the .edn.
- N `arxana/essay-section` entities (one per `:sections` entry; idempotent on the section `:id`).
- For each `:annotations[i]`: one hyperedge of the `:hx-type` value (typically `annotation/comment` or `aif/invariant-witness`), endpoints carrying the .edn's `:role` + `:section-id` + `:passage` (or `:pattern-name`), plus props for `:note :severity :status :joe-decision-required? :tied-to` etc. Multi-endpoint shape preserved verbatim — futon1a's hyperedge endpoint accepts arbitrary-arity endpoints per `file_ingest.clj`'s `post-hyperedge!` helper.
- Hyperedges may reference section entity IDs that are projected by the same file event. Order within the projection: essay first, then sections, then annotations.

### Function signatures (suggested)

```clojure
(ns futon3c.watcher.projections.essay
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def src-exts #{"md"})

(defn essay-home-md?
  "True iff PATH is an .md file under ~/code/futonN/essays/<slug>/<slug>.md.
   Used by file_ingest to gate dispatch — mission docs (holes/missions/M-*.md)
   and other .md files must NOT match."
  [path] ...)

(defn collect-file
  "Project one essay .md + sibling annotations.edn into the projection shape.
   Returns {:essay {...} :sections [...] :annotations [...]} or nil if PATH
   does not match the essay-home shape."
  [path] ...)
```

### Test expectations

1. **Operator-foreword fixture.** Project `/home/joe/code/futon7a/essays/operator-foreword/operator-foreword.md`; assert essay entity + 11 sections + 16 annotation hyperedges; assert `hx:of:v1:scope-mush-naming` projects with 3 `:annotated` endpoints (multi-anchor shape preserved).
2. **Path predicate.** `essay-home-md?` returns true for the operator-foreword path; false for `~/code/futon4/holes/missions/M-essay-corpus-substrate.md`; false for `~/code/futon7a/essays/some-readme.md` (no sibling .edn or wrong subdir depth).
3. **Idempotence.** Re-projecting the same path produces no duplicate entities / hyperedges in futon1a (the ensure-entity / ensure-hyperedge pattern from `flexiarg.clj` and `file_ingest.clj` is the discipline to follow).
4. **Second-essay generality.** A synthetic small essay (test fixture, any N) projects end-to-end with the same code path.
5. `clojure -X:test` passes (per the futon3c project convention).

### Completion criteria (D2)

1. `essay.clj` exists and is dispatched by `file_ingest.clj` on .md files inside the essay-home shape.
2. WebArxana entity view at `arxana://essay/arxana%2Fessay%2Foperator-foreword-v1` shows **16 annotations** post-projection (currently 0).
3. The projector is idempotent (criterion #3 above).
4. The catalog-cache papercut is either fixed (D2 emits a refresh signal Emacs can consume, OR a small Emacs-side hook auto-invalidates the cache on file change) OR explicitly documented as the operator step "`M-x arxana-browser-essays-refresh` after projector runs"; either is acceptable for v0.
5. A second essay under `~/code/futonN/essays/<slug>/` (any N) is added and verified to project end-to-end (criterion #4 of the original mission completion list — proves generality, not just first-essay correctness).

### Known caveats / clarity-gaps (carry-forward from Stage 1)

- **Catalog-cache invalidation gap.** `arxana-browser-essays.el`'s XTDB-projected catalog is cached on first call and never auto-invalidated. The compiled view at `arxana-browser-essays-open-compiled` and WebArxana both read through this cache. After any XTDB write (including D2's projection), operators currently must run `M-x arxana-browser-essays-refresh` to see the new state. Pick one resolution: (a) Emacs filewatcher on `~/code/futonN/essays/` triggers a refresh; (b) D2 writes a sentinel hyperedge Emacs consumes; (c) document as a known operator step. (a) is cleanest if Emacs already has filewatcher infrastructure.
- **`annotations.el` (legacy) coexistence.** The pre-.edn manifest format still exists in some essays; the compiled view falls back to it when no sibling .edn is present. D2 only needs to handle the `.edn` case; the `.el` path can stay as-is for back-compat. New essays should ship `.edn`.
- **Multi-endpoint annotation projection.** The `:endpoints` vector inside an annotation can have 2+ `:role :annotated` entries (e.g. `hx:of:v1:scope-mush-naming` has 3, `hx:of:v1:section-bridge-missing` has 4). The projector must NOT collapse these into separate hyperedges — one annotation = one hyperedge, multi-arity is the point. The `post-hyperedge!` helper accepts arbitrary-arity endpoints already.
- **Reload safety.** Per `feedback_reload_safety`, the new projector ns must be reconstructible-from-disk — a fresh restart of futon3c reads the new code at startup the same way a Drawbridge `:reload` would. Verify before relying on hot-reload. Do not `:reload` `xtdb.api` or any third-party defprotocol during testing (per `feedback_drawbridge_protocol_reload`).
- **Compiled-view tiebreaker.** Sibling fix already landed (2026-05-21) — not a D2 concern but mentioned so Codex isn't surprised by the `--annotation-at-point` change.
- **Repo-wide test gate is currently red outside D2.** On 2026-05-21 Codex verified the watcher-focused suite (`clojure -X:test :patterns '["futon3c.watcher.*"]'`) passes with the new projector, but the full `clojure -X:test` run remains red for unrelated pre-existing issues: missing `/home/joe/code/futon6/data/ct-validation/entities.json`, mfuton-mode expectation drift, wiring-evidence gaps, and blackboard projection failures. Surface this as a stack-health conflict rather than mis-attribute it to the essay projector.

### Next-move (post-D2)

When D2 completes:

1. **Mission close.** WebArxana shows 16 annotations on the operator-foreword; second-essay fixture verified.
2. **Hand back to M-editorial-assistant Stage 1.** The editorial log lives in `annotations.edn` as a new top-level field (`:editorial-log`), projected to futon1a by the same projector D2 just built — so the operator's resolution rationale round-trips to XTDB by construction, not by aspiration.
3. **Migrate other essays opportunistically.** Any essay currently under non-canonical paths (`~/npt/working-paper/`, etc.) can be moved to `~/code/futonN/essays/<slug>/` and picked up by the same projector without further code changes.
