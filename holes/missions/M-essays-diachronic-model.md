# Mission: Essays Diachronic Model — Golden/Live Dataset Discipline and XTDB Lifecycle Semantics

**Date:** 2026-05-14
**Status:** SPECIFIED, NOT YET IMPLEMENTED
**Parent mission:** `M-essays-edit-cycle.md`
**Owner:** futon4 / WebArxana / futon1a

## Purpose

The Essays subsystem now has a workable read/import/edit loop and a
retained-retraction path, but it still lacks a proper model of the essay as
work-in-progress. XTDB already records versioned writes when essay entities,
sections, and annotation hyperedges are upserted, but raw transaction history
is not yet the same thing as a first-class lifecycle model.

This mission defines the missing contract:

- how a reproducible **golden** essay seed is kept immutable
- how **live** test rounds are minted from that seed
- which essay/annotation state transitions are represented explicitly in XTDB
- which projections are derived from those transitions
- what the first replayable test case is

The first concrete seed case is Hyperreal Director Side A:

- markdown: `/home/joe/npt/applications/hyperreal-director-side-a/hyperreal-director-side-a-v1.md`
- manifest: `/home/joe/npt/applications/hyperreal-director-side-a/annotations.el`

## Current ingest baseline

The current filesystem-to-XTDB ingest path is already operational:

- `arxana-browser-essays-import` loads configured manifest files, upserts the
  essay entity, upserts section entities, and creates annotation hyperedges.
- `arxana-browser-essays-save-section` rewrites manifest passages, marks dead
  overlays retracted, and upserts the affected annotations back into XTDB.
- `arxana-browser-essays-checkpoint-restore` restores section text and
  annotation passages, then re-upserts those annotations to XTDB.

That means XTDB already sees the evolving essay. What it does **not** yet have
is a semantically explicit layer that distinguishes:

- a critique being raised
- a critique being answered in prose
- a critique being removed by substrate deletion
- a critique being re-anchored after prose revision
- a critique being manually withdrawn as inapplicable

If we skip that distinction and rely only on post-hoc interpretation of
generic upserts, the later graph and audit views will be ambiguous.

## Dataset discipline

### Golden dataset

The golden dataset is an immutable seed corpus used for repeatable testing,
replay, and regression verification.

Requirements:

- The source markdown and manifest are treated as read-only seed inputs.
- The seed is versioned by explicit dataset identity, not by "whatever is
  latest in `~/npt`".
- A test round must be able to say "start from golden seed X" and mean one
  exact markdown/manifest pair.
- Re-running the same replay against the same seed must yield the same
  expected lifecycle transitions.

For the first implementation pass, Hyperreal Side A v1 is the canonical seed:

- essay seed id: `hyperreal-director-side-a-v1`
- markdown seed: `hyperreal-director-side-a-v1.md`
- manifest seed: `annotations.el`

### Live dataset

The live dataset is a writable working copy derived from a named golden seed.
It exists so operators can run real edits, retractions, restores, and replay
rounds without mutating the seed itself.

Requirements:

- Every live dataset declares which golden seed it was minted from.
- Live rounds are explicit instances, not silent reuse of the same working
  directory forever.
- A fresh live round can always be recreated from the unchanged golden seed.
- Lifecycle reports and graph views must identify the live round they refer
  to, not just the essay id.

### Seeding invariant

The correct workflow is:

1. choose a golden seed
2. mint a named live round from it
3. import that live round into XTDB
4. perform edits and observe lifecycle transitions
5. discard or archive the round without mutating the seed

Workarounds such as "just copy the current `~/npt` files around by hand and
hope we remember which was the clean one" are invalid.

## XTDB model

### Stable identities

The following identities must remain stable across time:

- `essay`
- `essay-section`
- `annotation`
- `dataset-seed`
- `dataset-round`

The important point is that an annotation is not replaced by a series of
anonymous snapshots. It keeps identity while lifecycle events accumulate.

### Lifecycle events

This mission proposes an explicit event layer in XTDB. Minimum event types:

- `annotation-created`
- `annotation-reanchored`
- `annotation-retracted`
- `annotation-addressed-in-prose`
- `annotation-folded-into-prose`
- `annotation-monster-barred`
- `annotation-reopened`
- `section-rewritten`
- `section-deleted`
- `dataset-round-seeded`

Each event should minimally carry:

- event id
- essay id
- section id when applicable
- annotation id when applicable
- dataset round id
- event kind
- timestamp / XTDB transaction identity
- operator / provenance fields already available in the write path
- enough local state to explain the transition

For example, `annotation-retracted` should be able to say whether the cause
was manual withdrawal, substrate removal, section deletion, or another
recognized reason.

### Projection layer

The UI should not read lifecycle semantics directly from ad hoc manifest flags
when a projection is the real derived view. At minimum we need projections for:

- current notes-pane state
- current section-render state
- open / closed / pending annotation summary
- diachronic argumentation graph per essay
- replay/audit reports for a dataset round

`:retracted t` in the manifest can remain a useful current-state marker, but
it is not the authoritative history model on its own.

## Hyperreal Side A as first replay case

Hyperreal Side A is the right first case because it already has live emergent
tension annotations added on 2026-05-13 and it sits at the overlap between the
Essays mission and the EoI corpus work.

The first replayable scenario should cover:

1. import the golden Hyperreal seed
2. mint a fresh live round
3. perform one prose edit that preserves an annotation anchor
4. perform one prose edit that forces a re-anchor
5. perform one prose edit that removes an annotation substrate entirely
6. save and sync to XTDB
7. verify that the resulting lifecycle projections distinguish those three
   cases rather than collapsing them into generic "annotation changed"

That scenario is small enough to implement, but rich enough to prove whether
the model is truly diachronic.

## Scope

### In scope

- Golden/live dataset discipline for essays under edit.
- XTDB lifecycle-event vocabulary for essay and annotation state transitions.
- Projection requirements for notes-pane, audit, and graph views.
- Hyperreal Side A as the first seeded replay case.

### Out of scope

- Building the final WebArxana graph renderer itself.
- Solving every re-anchoring heuristic in this mission.
- Generalising the seed discipline beyond essays before the Hyperreal case is
  proven.

## Exit criteria

- There is a written contract for golden seed identity and live round identity.
- There is a written XTDB event vocabulary for essay/annotation lifecycle.
- There is a clear statement of which facts are events and which are
  projections.
- Hyperreal Side A is named as the first replayable seed case with a concrete
  verification scenario.
- The parent mission points to this companion mission as the spec source for
  the diachronic follow-on.

## Immediate next steps

1. Add a seeding command or script that mints a live round from the Hyperreal
   golden seed without mutating the source pair in `~/npt`.
2. Define the XTDB document shapes for `dataset-round` and the lifecycle event
   entities.
3. Decide which current save-time transitions can be emitted immediately from
   `arxana-browser-essays-save-section` and which require a larger refactor.
4. Only after that, build the temporal query and WebArxana graph view.
