# Mission: Essays Edit Cycle — Re-anchoring Annotated Editions Under Editing

**Date:** 2026-04-20
**Status:** ACTIVE. Read view and section-edit/save writeback are operational
in `arxana-browser-essays.el`; the essay registry now projects from XTDB
rather than depending on volatile Emacs defcustom state. Remaining work is
the broader edit-cycle surface (re-anchoring discipline, flexiarg drift
handling, and richer manifest editing ergonomics).
**Blocked by:** None directly. Builds on the operational Essays
subsystem (read view, importer, manifest format).
**Owner:** futon4 (Essays browser), with ingress points in futon1a (XTDB
writeback) and the source repos for essays under annotation.

## Motivation

The Essays subsystem (`arxana-browser-essays.el`) currently provides a
read-only annotated edition: a markdown essay rendered alongside a
notes pane that highlights pattern-library citations passage by
passage, with cross-buffer point-tracking. The UKRN Working Paper is
the working example, with 53 sentence-level annotations across seven
sections linking to the `equity/` and `ukrns/` flexiarg libraries.

This is the easy half. Real-world use of an annotated edition involves
editing — of the essay text, of the cited flexiargs, and of the
manifest itself. Each edit pathway has its own re-anchoring problem.
None of them is currently handled. The Essays subsystem ships a static
reading view of what is, in practice, a moving target.

The mission is to specify and implement the edit-cycle handling so the
annotated edition stays coherent as its constituent layers evolve under
continued authorship. The original three-layer statement (essay prose,
flexiarg sources, annotation manifest) now has a fourth layer that must
also stay coherent: the registry of essays itself. That registry is now
projected from XTDB entity state rather than editor-local configuration.
The UKRN WP is a useful test bed because all four layers *are* live: the
WP draft is iterating, the flexiarg library is iterating, the manifest
itself is being authored, and the visible registry of essay identities is
part of the day-to-day workflow.

## Annotation lifecycle visibility (added 2026-05-13)

Surfaced by the Anthropic EoI work (a second live test-bed alongside the
UKRN WP): when an operator edits-out the substrate that an annotation was
anchored to, the annotation's overlay evaporates, the annotation transitions
to `:retracted t` in the manifest, and **the notes pane filters it from
view**. The annotation, and the diagnostic-history it carried, silently
disappears.

This is wrong for two reasons that compose:

- **Audit trail.** A reader (future-operator, second reviewer, or
  past-self) should be able to see that a critique was raised AND that the
  edit which evaporated the anchor is what addressed it. Disappearance
  erases the diagnostic record.
- **Argumentation-theoretic.** Editing-out-the-substrate is the same shape
  as Lakatos's **Monster Barring**: when a counterexample threatens a
  theorem, the author redefines the theorem's conditions so the
  counterexample no longer applies. This is a legitimate dialectical move
  *only when the move is visible*. The current UI loses the trace, which
  makes monster-barring indistinguishable from oversight, refutation, or
  silent retreat. That conflation is exactly what Lakatos's framework
  exists to prevent.

Two scoped pieces of work, the second building on the first:

### Persistent retraction visibility (now split to companion mission)

When an annotation transitions to `:retracted t` (whether automatically via
overlay-evaporation at save time, or manually), the notes-pane render keeps
the entry visible with a strikethrough face rather than filtering it out.
Hover / inspect should reveal *what kind* of retraction this was — auto via
substrate-removal, auto via section-deletion, manual, etc. — and *when*
(timestamp on the manifest's `:retracted` transition).

The implementation surface turned out to split in two:

- **Already present:** there is now a *live preview* path in
  `arxana-browser-essays.el` that decorates the notes buffer with
  strikethrough when an annotation's overlay dies during the current edit
  session.
- **Still missing:** that visibility does not survive a save/reopen cycle,
  because the persisted notes render still filters retracted annotations out
  of the section-open path.

That remaining work is now tracked in the companion mission
`M-essays-retraction-visibility.md`. The older handoff at
`~/code/algorithms/arxana-essays-strikethrough-handoff.md` should be read
as historical context, not as a current-state-faithful implementation spec.

### Diachronic argumentation graph per essay (now split to companion mission)

The richer move, surfaced 2026-05-13 in conversation: each Essay could have
a **graph view** analogous to the final-section argument graph in "A First
Proof Sprint" (the futon6 Arxiv monograph). The Essay's graph would be
**diachronic** — it tracks annotation state-transitions over time, so an
operator can see:

- **Which questions / critiques have been raised** (annotation creation
  events).
- **Which have closed**, and by what means: addressed-in-prose,
  monster-barred (substrate removed), folded-into-prose-as-answer,
  explicitly-retracted-as-not-applicable, still-open.
- **The trajectory of the argument** as the essay evolves under editing.

The graph nodes are annotations + section-anchored prose passages. The
edges are state-transitions (raised / addressed / barred / retracted /
re-anchored / pending-retraction). The view is essentially an
argumentation-theoretic visualisation of the essay's revision history,
making rebuttal moves (in the Lakatosian / Toulminian sense) visible at
the same first-class level as the prose itself.

The follow-on turned out to need an explicit dataset and lifecycle spec
before implementation. That spec now lives in the companion mission
`M-essays-diachronic-model.md`.

Implementation surface (revised): XTDB already stores essay entities,
section entities, and annotation hyperedges, and save/restore flows already
produce versioned writes. But a temporal query + graph renderer is not enough
unless the system also distinguishes explicit lifecycle events from derived
current-state projections. The companion mission defines that contract,
including the golden/live dataset discipline and Hyperreal Side A as the
first replayable seed case.

## Execution tracks (2026-05-14)

To get this mission moving as an actual programme of work rather than a
single broad bucket, split it into four near-term tracks:

### Track A — Retraction visibility and lifecycle semantics

Companion mission: `M-essays-retraction-visibility.md`.

Focus:
- Persisted visibility for `:retracted t` annotations after save/reopen.
- Reconciliation of the existing live-preview strikethrough path with the
  persisted notes-render path.
- Decision on whether `:retraction-kind` / `:retracted-at` metadata belongs
  in the same patch or a follow-on.

This is a good Claude companion slice because it is self-contained on the
Emacs UI side and has a narrow test surface.

### Track B — Live registry rollout and migration verification

Focus:
- Re-import configured essay manifests so pre-existing XTDB essay entities
  pick up the new persisted registry props.
- Verify cold-start and save-refresh behavior on the real UKRN / Anthropic /
  Peeragogy essays without manual registration.
- Record the live migration result back into this mission.

This is a good Codex slice because it needs careful environment handling and
should stay close to the registry work already landed.

### Track C — Re-anchoring and flexiarg drift specification

Focus:
- Decide the lossy re-anchor strategy (context-anchor, diff-driven, or
  manual-first).
- Decide the source-passage tracking model for flexiarg edits.
- Turn those decisions into executable sub-missions rather than leaving them
  as open bullets in the parent doc.

### Track D — Diachronic dataset and lifecycle model

Companion mission: `M-essays-diachronic-model.md`.

Focus:
- Define immutable golden seeds and writable live rounds for essay testing.
- Define the XTDB lifecycle-event vocabulary rather than relying on raw
  upsert history alone.
- Specify the projection boundary between manifest flags/current render state
  and durable diachronic history.
- Use Hyperreal Director Side A as the first replayable seed case.

### Cross-agent coordination protocol

Tracks A and B run partly via Claude/Codex coordination. The dispatch
mechanics (bell = async fire-and-forget, whistle = sync blocking,
whistle-stream = NDJSON) are documented in
`~/code/futon3c/README-bells-and-whistles.md`. Track A slices that need to
run in parallel with active Emacs editing should go out via `bell`; Track B
verification passes that block the lifecycle should use `whistle`.

## Theoretical anchoring

- **Arxana scholia-as-entities:** Annotations are first-class, not
  margin comments. The hyperedge model (already in place) supports
  rich edit semantics — versioning, attribution, deletion-as-rewrite —
  in a way that line-anchored annotation systems (PDF margins, web
  highlight tools) cannot.
- **Re-anchoring as a known-hard problem:** The annotation literature
  (Hypothesis, Genius, scholia plumbing in Recogito) treats text-anchor
  drift as a primary failure mode. Strategies range from exact-match
  (fragile) through fuzzy match through anchor-by-context (TextQuoteSelector
  and related W3C Web Annotation specs). Each fails differently; the
  mission needs to specify which failure modes are acceptable for the
  Essays use case.
- **Self-application discipline:** The annotated edition mirrors the
  WP's own self-application stance — the artefact updates as evidence
  accrues. An Essays subsystem that breaks under edit is the WP's own
  argument used against itself; the mission is what closes that gap.

## The four edit pathways

### 1. Essay text edited (annotation passages no longer match exactly)

When the WP markdown is reworded, an annotation's `:annotated :passage`
may no longer be findable in the section text. Two failure modes:

- **Lossless (whitespace, markdown markup, punctuation):** the audit
  function (`arxana-browser-essays-audit-passages`) catches these and
  the manifest can be auto-fixed. The current implementation does this
  as a one-shot batch via the Python script in `/tmp/audit_passages4.py`.
  Lifting this into an interactive Emacs command is straightforward.
- **Lossy (wording changes):** "the gap between outcomes is architectural"
  → "the architectural gap between outcomes" — exact match fails; even
  fuzzy match may not recover the right passage. Re-anchoring options:
  - Anchor-by-context: store N-word neighbourhoods (`prefix` /
    `suffix`) alongside each passage; on miss, search by neighbourhood.
  - Diff-driven: when the source file changes, apply the diff to the
    known annotation positions to predict where they moved.
  - Fall back to manual reconciliation: surface the unresolved
    annotations in a buffer, present likely candidates, let the author
    choose.

### 2. Flexiarg edited (linked source-passage may now be stale)

Each annotation cites a flexiarg's specific field
(`:source :passage "! conclusion: ..."`). Two questions:

- **Tracked by content or by field name?** Currently the passage is a
  literal string. If the flexiarg's `! conclusion` line is reworded,
  the linked passage no longer matches. Switching to field-name
  tracking (`{:field :conclusion}`) would preserve the link but lose
  the specific quote. A hybrid (field name + best-effort quote) seems
  right but needs spec.
- **Snapshot vs live view?** Should the notes pane show the flexiarg
  as it was when the annotation was authored (snapshot), or as it is
  now (live)? Provenance arguments for snapshot; currency arguments for
  live. Probably the right answer is live with a "this annotation was
  authored against version X, current text shown" note.

### 3. Manifest edited from within the browser

The browser is currently read-only. Making it editable means:

- **Inline note editing:** in `*Arxana Essay Notes*`, edit a gloss or
  source passage in place; save writes back to `annotations.el` and
  upserts the hyperedge in XTDB.
- **Add annotation:** select a passage in `*Arxana Essay*`, invoke
  command, prompt for source pattern and gloss, persist.
- **Delete annotation:** at point in either buffer, invoke command;
  remove from manifest and XTDB.
- **Writeback to disk:** the manifest is an Elisp `defconst`. Editing
  it programmatically requires either a structured pretty-printer
  (preferred — preserves the file's hand-authored structure) or a
  full regenerate (loses comments and section organisation). The
  pretty-printer is non-trivial.
- **Writeback to XTDB:** existing `arxana-store-create-hyperedge` and
  related functions handle the write. The harder question is *update
  vs delete-and-recreate* for hyperedges, which depends on Futon1a's
  hyperedge mutation semantics.

### 4. Section structure changes

Adding a new section, renumbering, or merging two sections raises:

- **ID stability vs display numbering:** section IDs in the manifest
  use the form `section/3-cycle-0` — stable across renumbering. The
  display name uses §N — not stable. The mission should clarify
  whether the manifest should auto-update display names from the
  source file's headings or treat them as authored separately.
- **Auto-discovery:** when a new `## N. Heading` is added to the WP,
  should the Essays subsystem auto-create a section entity? Probably
  yes, with the user prompted to confirm; auto-creation without
  confirmation risks polluting the manifest with sections that have no
  annotations yet.

## Sync semantics

When does an edit trigger a re-import / re-anchor?

- **On save?** Source files watched; on save, run the audit and either
  auto-fix or surface unresolved.
- **On demand?** A `M-x arxana-browser-essays-resync` command.
- **Periodically?** Background timer.
- **Multi-author divergence:** if Joe edits the manifest while Codex
  edits the WP, who wins? Probably last-writer-wins at the cell level
  (each annotation is independent), but the mission should specify.

## Source-of-truth registry

The essay catalog itself is part of the edit cycle. If the browser's
home view is sourced from a volatile Emacs `defcustom`, then restart and
reload behavior can sever the link between an essay entity in XTDB and
its source markdown on disk. That is a coherence failure, not a mere UX
annoyance: the manifest still exists, the essay entity still exists, but
the browser can no longer reconstruct the annotated edition.

The structural move is:

- Persist registry metadata needed for reconstruction directly on the
  XTDB essay entity's `:props`: resolved `source-file`, `manifest-file`,
  `label`, and `description`.
- Treat the XTDB essay entities as the authoritative registry and
  project the browser catalog from them.
- Retain a small defcustom augmentation layer only for pre-import
  manifests and explicit operator overrides, merged by `:essay-id`
  without outranking XTDB-backed identity.

This does not eliminate manifest files. It eliminates the category error
where catalog identity lived in editor config while essay identity lived
in XTDB.

## Working example

The UKRN Working Paper is a useful test case because all four edit
pathways are live concurrently:

- The WP draft (`UKRN_WP_draft_v5.md`) iterates as the writing plan
  executes.
- The flexiarg libraries (`/home/joe/code/futon3/library/{equity,ukrns}/`)
  iterate as patterns are revised.
- The manifest (`/home/joe/npt/working-paper/annotations.el`) iterates
  as new annotations are added or revised.
- Section structure may change as the writing plan absorbs new
  sections (annexes, gallery cross-references, candidate sixth pattern
  evolutions).

The mission can use specific known iterations as test cases:
- The §0 architectural-passage edit (already happened: "the difference"
  → "the gap between those outcomes"). Audit caught and fixed it.
- The H3 Year-1 default decision (planned: the [hole: H3] marker will
  be replaced with concrete prose; existing annotation engages-hole
  must follow).
- The MVSG figure caption edit (planned: when the rendered PNG changes
  composition, caption updates).

## Scope

### In scope

- **Audit-and-fix lifecycle** for the lossless edit case (whitespace,
  markup, punctuation). Automate the Python-script path into a
  reliable Emacs command.
- **Anchor-by-context** spec and implementation for the lossy edit
  case. Store prefix/suffix neighbourhoods; fall back to manual
  reconciliation for the unresolvable.
- **Manifest writeback** with structured pretty-printing that
  preserves comments and section organisation.
- **Inline edit commands** in the notes buffer (edit gloss, edit
  passage, delete annotation).
- **Add-annotation command** triggered from a region selection in the
  text buffer.
- **Auto-section-discovery** with confirmation prompt.
- **Re-sync command** with on-save and on-demand triggers.

### Out of scope (for this mission)

- Multi-user real-time sync (deserves its own mission, likely
  inheriting from the WebArxana mission's sync infrastructure).
- Annotation-level provenance / version history (beyond what XTDB
  hyperedge versioning gives for free).
- General-purpose annotation system for other essays beyond UKRN WP
  (UKRN WP is the test case; generalisation comes after).
- Cross-essay annotation queries (e.g. "all annotations citing
  ukrns/ARGUMENT across all essays"). Worth doing later but not core.

### Deferred questions

- Whether annotations should be tracked by content + field name +
  context-anchor as a triple (defence-in-depth) or by one of these
  with the others as fallback.
- Whether the source-flexiarg link should follow the flexiarg's
  current state (live) or snapshot at annotation-authoring time.
- Whether the manifest format should remain Elisp or migrate to
  EDN/JSON (the original sketch was EDN; current is Elisp because
  futon4 has no EDN reader; this mission is a natural point to
  reconsider).
- **Author-comments (Google-Docs-style sticky notes).**
  `arxana-browser-essays-add-comment` (bound to `C-c c` in the edit
  map) creates an annotation with `hx-type "annotation/comment"`, no
  pattern source, and the comment body as `:note`.  It writes to
  `annotations.el` and re-renders (yellow tint + `💬N` marker in the
  text buffer; `💬N Author comment` entry in the notes buffer).
  XTDB sync currently *skips* comments because they have no pattern
  endpoint to pair with the section endpoint.  The hyperedge
  representation is open: single-endpoint scholion, or a constructed
  `arxana/author/<id>` entity as the source endpoint.  Needs a
  decision before comments become durable via checkpoint/restore —
  today they survive in the manifest file only.
- **Cut-yank / copy-paste movement of an annotated passage.**
  The render mirrors each annotation's id onto its text as a text
  property, so in principle kill/yank can carry the id through the
  kill ring and a post-command reconcile sweep can re-attach the
  overlay at the yank target. In practice this is not yet reliable
  enough to treat as supported editing behavior. On 2026-05-14 Joe
  observed that cutting part of an annotated passage and yanking it
  past an unannotated portion of the same section did **not** preserve
  the expected moved annotation behavior. So this is currently a
  front-end defect, not just an open copy-semantics question.
  *Copy* also leaves two runs for the same id: the reconcile currently
  attaches the overlay to the first run only; the second occurrence
  displays as undecorated text. The unresolved question is what the
  semantics should be:
  - Forked annotation (new id, inherited metadata) — makes
    provenance explicit but multiplies the manifest.
  - Silent un-annotation of the copy — matches the "cut is the
    canonical move" stance but is non-obvious.
  - Dual-display (same id, both runs overlaid, markers disambiguated
    `[N/a]` / `[N/b]`) — most faithful to the hypergraph view, but
    complicates the sync-back-to-manifest path (which passage is
    canonical when one is edited?).
  For now, both cut-yank movement and copy-paste of annotated regions
  should be treated as unreliable pending a dedicated front-end fix.

## Exit criteria

- The UKRN WP edit cycle (essay edits, flexiarg edits, manifest edits,
  section edits) can run for a full Cycle-1 writing pass without
  manual annotation-fixing intervention beyond the cases the mission
  explicitly defers to manual reconciliation.
- The audit function and the editing commands are documented in the
  Essays README (to be written as part of the mission).
- A subsequent essay (maybe a futon3c mission report or a futon7
  landscape note) can be annotated using the same machinery without
  copy-paste of the UKRN-specific code.

### Checkpoint 1 — 2026-05-13
**What was done:**
- Replaced direct browser reads of `arxana-browser-essays-catalogs` with a
  merged catalog accessor whose primary input is an XTDB projection of
  `arxana/essay` entities.
- Updated essay import so XTDB essay entities now persist the resolved
  `source-file`, `manifest-file`, `label`, and `description` props needed
  to reconstruct the catalog after Emacs or JVM restart.
- Switched manifest/source resolution to prefer `:manifest-file`, while
  preserving `:manifest-symbol` as a backward-compatible fallback for
  older augmentation entries.
- Added ERT coverage for registry-prop persistence, manifest-file-based
  manifest loading, source-file resolution relative to manifest location,
  XTDB/augmentation merge behavior, and cache invalidation on refresh.

**Test state:** `bash dev/check-parens.sh dev/arxana-browser-essays.el test/arxana-browser-essays-test.el` → OK. `arxana-browser-essays-test.el` → 20 tests, 20 passed, 0 failures.

**Next:** run the live migration path (`arxana-browser-essays-import` against
configured manifests) and verify the cold-start/save-refresh behavior on
real essays against the persisted XTDB store.

### Checkpoint 2 — 2026-05-14
**What was done:**
- Landed the retained-retraction fix for the notes pane: persisted
  `:retracted t` annotations are now rendered on section-open instead of
  being filtered out after save/reopen.
- Unified the retraction presentation path so both live overlay-loss
  preview and persisted notes rendering use the same
  `arxana-browser-essays-retracted-face` strikethrough treatment.
- Added metadata-aware retraction markers in the notes render
  (`[retracted]`, plus optional kind/timestamp when present) without
  widening this patch to a manifest-schema writeback change.
- Added ERT coverage for both the direct notes render behavior and the
  section-open call path that now passes retracted annotations to the
  notes pane while keeping the text render on live annotations only.

**Test state:** `bash dev/check-parens.sh dev/arxana-browser-essays.el test/arxana-browser-essays-test.el` → OK. `arxana-browser-essays-test.el` → 22 tests, 22 passed, 0 failures.

**Live-data note:** real persisted `:retracted t` annotations are present in
the Anthropic Institute Analyst and UKRN manifests, so the reopened-notes
path now has corpus data to exercise. Manual `arxana://view/essays-home`
UI verification against the four live essays was not run in this patch turn.

**Next:** re-sync with the live essay set in Emacs, verify the retained
retraction display on the Hyperreal Side A round-2 annotations, then start
the diachronic argumentation-graph follow-on as a WebArxana-side mission.

### Checkpoint 3 — 2026-05-14
**What was done:**
- Split the diachronic follow-on into a proper companion mission:
  `M-essays-diachronic-model.md`.
- Specified the dataset discipline needed for replayable essay testing:
  immutable golden seeds plus writable live rounds derived from them.
- Recorded the actual current ingest baseline: manifest import, save-time
  sync, and checkpoint restore already write essay state into XTDB, but only
  as current entities/hyperedges plus transaction history.
- Made the missing design boundary explicit: raw XTDB write history is not
  yet a lifecycle model, so the system still needs first-class event types
  and projections for diachronic essay analysis.
- Named Hyperreal Director Side A as the first seeded replay case for this
  work.

**Status:** spec complete, implementation deferred. No code changed in this
checkpoint; this was a mission-structure and data-model clarification pass.

**Next:** define the XTDB document shapes for `dataset-round` and lifecycle
events, then add a seed-to-live-round minting command before starting the
WebArxana graph implementation.

### Checkpoint 4 — 2026-05-14
**What was done:**
- Added the first copy-only Hyperreal replay tests in
  `test/arxana-browser-essays-test.el`, using temp-directory golden/live
  rounds derived from the real Hyperreal Side A seed files.
- Covered three concrete safety/lifecycle cases:
  copied-round manifest/source resolution against the real seed shape,
  import persisting only copied manifest/source paths, and a save-cycle on
  a live-round copy that edits the copied markdown without touching either
  the copied golden seed or the real `~/npt` source file.
- Kept the harness entirely fixture-local: no test writes land in the live
  Hyperreal working files, and XTDB writes are stubbed in the import path.

**Test state:** `bash dev/check-parens.sh test/arxana-browser-essays-test.el` → OK. `arxana-browser-essays-test.el` → 25 tests, 25 passed, 0 failures.

**Next:** add the first explicit replay-style lifecycle assertions on top of
the copied Hyperreal rounds: one preserved-anchor edit, one forced
re-anchor, and one substrate-removal retraction case.

### Checkpoint 5 — 2026-05-14
**What was done:**
- Added the first explicit lifecycle replay assertions on copied Hyperreal
  rounds:
  preserved-anchor edit leaves the manifest passage unchanged,
  forced re-anchor triggers the render-time fuzzy match plus manifest
  passage rewrite,
  and substrate removal during save marks the copied manifest annotation
  `:retracted t`.
- Kept the whole batch fixture-local: the copied live round mutates, while
  the copied golden seed and the real `~/npt` Hyperreal files remain
  unchanged.
- Verified that the re-anchor case is hitting the intended path rather than
  a test-only shortcut: the suite logs a real
  `[essays] Re-anchored ... manifest updated` message from the render path.

**Test state:** `bash dev/check-parens.sh test/arxana-browser-essays-test.el` → OK. `arxana-browser-essays-test.el` → 28 tests, 28 passed, 0 failures.

**Next:** widen the replay harness from single-annotation section-local cases
to multi-annotation and cross-section cases, then start deciding which of
these lifecycle transitions deserve first-class XTDB event entities rather
than inference from upsert history alone.

### Checkpoint 6 — 2026-05-14
**What was done:**
- Added one more copied-Hyperreal replay at the Essays/UI boundary:
  retraction followed by reopen.
- Fixed the persisted-reopen defect that replay exposed. The root cause was
  XTDB-projected `:manifest-file` / `:source-file` values outranking explicit
  runtime working-copy overrides during save/reopen, so copied-round sessions
  could snap back to the wrong files.
- Catalog merging now lets explicit runtime path overrides win while XTDB
  stays authoritative for essay identity and descriptive metadata.
- Fixed the notes renderer to treat missing `:retraction-kind` metadata as
  absent rather than formatting it as the literal string `nil`, so reopened
  retained notes show `[retracted]` instead of `[retracted: nil]`.

**Test state:** `bash dev/check-parens.sh dev/arxana-browser-essays.el test/arxana-browser-essays-test.el` → OK. `arxana-browser-essays-test.el` → 29 tests, 29 passed, 0 failures.

**Decision signal:** this is a good stopping point for broadening the
Arxana Essays test matrix. The copied-round replay harness now covers the
persisted reopen path as well as save-time re-anchor and retraction, so the
next centre of gravity should move to the WebArxana diachronic view rather
than widening more Essays-only edge cases first.
