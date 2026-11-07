# Edit Robustness specification

Date: 2026-11-07

Status: normative draft with a JVM substrate proof; editor-preview transport is
an explicit remaining obligation.

## Purpose

Arxana identifies a logical scope independently of any one byte position or
artifact revision. A content-addressed revision makes one observation
reproducible; it does not freeze the scope or require an editor buffer to keep
the same digest forever. The same portable strategy is evaluated against each
successive snapshot to recover the scope's current location.

This specification corrects the static-fixture interpretation recorded in
`EDIT-ROBUSTNESS-CORRECTION.md`. It deliberately proves the core contract on
throw-away text before another mathematical demonstration is attempted.

## Four distinct objects

1. **Logical scope** — a stable `scope/id`. Copying its text does not silently
   create, select, or merge scope identities.
2. **Strategy revision** — an immutable, digest-identified relocation program.
   The same strategy revision is reused until its actual strategy data changes.
3. **Artifact revision** — immutable, content-addressed canonical bytes for one
   saved or transient snapshot. Every byte change mints a different revision.
4. **Resolution observation** — the qualified result of running one strategy
   revision against one artifact revision. It never rewrites an observation
   made against an earlier revision.

Editor-local moving markers or text properties may improve responsiveness, but
they are presentation/cache mechanisms. They are neither portable scope
identity nor authoritative resolution evidence.

## Normative invariants

- **ER1 — stable identity:** edits do not change `scope/id` merely because its
  bounds, text, or artifact revision changed.
- **ER2 — immutable snapshots:** changed bytes are evaluated under a new
  artifact revision ID. Changed bytes must never be smuggled under an old ID.
- **ER3 — strategy continuity:** an unchanged strategy revision is evaluated
  against successive artifact revisions; clients must not manufacture a new
  strategy merely to update a buffer position.
- **ER4 — cascaded relocation:** exact hint/hash, context/hash, and fuzzy
  context are attempted in that order. A later stage cannot upgrade its
  standing to that of an earlier stage.
- **ER5 — ambiguity preservation:** if a copy produces multiple equally valid
  candidates, all are returned as `ambiguous`; ordering is not selection.
- **ER6 — qualified internal edits:** recovery after the scoped text itself
  changes is usable but `resolved-unverified` until stronger evidence or an
  attributable acceptance exists.
- **ER7 — typed absence:** deletion yields `missing`/orphaned, not a stale
  location and not an arbitrary nearest match.
- **ER8 — historical stability:** resolving a newer revision never mutates or
  invalidates a reproducible observation about an older revision.
- **ER9 — editing is not stale input:** `stale-input` is correct only when
  supplied bytes falsely claim an existing immutable revision ID. Ordinary
  current-buffer edits must instead be registered as a new transient or
  durable revision and resolved normally.
- **ER10 — attributable promotion:** saving or promoting a relocated result
  creates a new immutable revision and evidence/acceptance records without
  rewriting the prior assertion.

## Required edit matrix

| Edit | Required outcome |
|---|---|
| Insert before | Same scope and strategy; shifted verified bounds |
| Insert after | Same scope, strategy, and bounds |
| Move/cut-paste | Context cascade relocates the same scope |
| Edit inside | Fuzzy `resolved-unverified`, visibly qualified |
| Copy | Every candidate returned as `ambiguous`; no first match |
| Delete | Typed `missing`/orphaned result |
| Revisit old revision | Original result and evidence remain reproducible |
| Edit unsaved buffer | Current bytes receive a transient revision and resolve; no refusal solely for differing from the previous digest |
| Save/promote | Durable revision and attributable evidence are appended |

## Throw-away substrate proof

`futon3c.arxana.edit-robustness-test` creates one logical scope, one store, one
artifact provider, and one registered strategy revision. It then ingests seven
distinct text snapshots into that same world: base, insertion before,
insertion after, movement, internal edit, copy, and deletion. The test requires
seven distinct artifact revision IDs while retaining the same strategy
revision, checks every outcome in the matrix, and finally re-resolves the base
revision to prove earlier evidence was not rewritten.

This proves ER1–ER8 at the resolver/artifact-provider boundary. It does **not**
yet prove ER9–ER10 across an editor transport.

## Remaining editor-preview protocol obligation

Before another interactive demo counts as edit-robust, at least one editor
client and the JVM service must demonstrate this sequence:

1. Observe buffer bytes and an editor-local version token.
2. Register those exact bytes as a content-addressed transient artifact
   revision (deduplicating identical bytes is allowed).
3. Resolve the existing strategy revision against that transient revision.
4. Discard out-of-order replies using the editor-local version token, without
   confusing that token with artifact identity.
5. On save or explicit promotion, mint/recognize the durable revision and
   append attributable evidence; retain the prior observations.

The precise endpoint and retention policy are not selected by this substrate
proof. Until this bridge has an acceptance test, a client that refuses merely
because current bytes differ from a fixture is non-conforming, even though the
underlying immutable-revision integrity check remains correct.

## Gate for the next mathematical demo

Mark8-derived mathematical scopes may be used after the editor-preview bridge
passes the same matrix. Their mined alignments remain candidate evidence, not
authority. The demo should add mathematical meaning—such as proof move,
applicability, reason, declaration, and rendered manifestation—without changing
the Edit Robustness contract proved here.
