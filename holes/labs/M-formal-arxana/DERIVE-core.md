# DERIVE-core — the contract shared by F1–F6

Recorded by claude-13, 2026-11-08, from codex-21's answers (round 4, job
invoke-1794099328865-15421-fe14d8f5). codex-21 wrote the mission; these are
its clarifications, in close paraphrase. Joe's direction: settle the common
core before fixture detail, so that F1 does not bend it.

## 1. 'Clusion types

F1's interpretation record is **one 'clusion type over a common envelope**.
Every 'clusion references an immutable, versioned **type declaration**:
type id, roles, arity, endpoint kinds, required dynamics/method fields,
identity projection, convergence policy. `clusion/assert` validates against
the declaration. Fields such as F1's interpretation state are type-specific,
not universal.

## 2. Endpoint kinds

An endpoint may reference: a semantic entity; an artifact revision; a
manifestation/scope; another 'clusion id; an external-reference entity.

- Lean declarations, commits, jobs, agents and the like are wrapped as
  **typed external-reference entities**, never arbitrary strings.
- 'Clusion endpoints are allowed by the schema **from F1**, so the core does
  not change later; F4 supplies the recursion/composition acceptance tests.
- A locator is revision-pinned evidence about a manifestation, not normally a
  bare endpoint.

## 3. Dynamics

Every 'clusion carries an **inert dynamics reference** from the start:
operation/method id, version, parameters. Executable code is never stored as
authority. F4 adds composition and activation tests; D6 capability/admission
is still required before anything executes.

## 4. Projections and charts

| Thing | Durability | Authority |
|---|---|---|
| Projection rule (+ version) | durable, immutable | JVM Arxana (identity, schema) |
| Shared/reproducible chart definition | durable, immutable/versioned | JVM Arxana |
| Deterministic projection output | recomputable; optionally kept as evidence | the declared projector |
| Named shared layout coordinates | durable only when promoted to a chart/layout revision | the projector / promoting user |
| Cursor, viewport, selection, hover, temporary coordinates | client-local presentation state | the user/client |

## 5. Edit proposals

The five-stratum edit-proposal envelope is primarily a **transient
request**. Where audit or execution matters it is preserved through D6
(source act → intent → candidate → decision → receipt). It is not
automatically a 'clusion, though a proposed relation change may contain a
candidate 'clusion.

## 6. Standing

Standing is orthogonal fields, not one ladder:

- **resolution state**: `unique | ambiguous | missing | stale`;
- **evidence**, three orthogonal fields (round 5):
  - `evidence/basis`: `exact | parser | model | human | kernel | composite`
  - `evidence/standing`: `unverified | verified | contradicted | disputed`
  - `evidence/coverage`: `none | partial | complete`
  plus actor/tool, method and version. Basis says how it was attested, not
  how strong it is: human or model evidence can still be unverified or
  disputed;
- **assessment**: a separate attributable record (`candidate | accepted |
  rejected`); nothing implies human acceptance.

The correspondence ladder maps as: `stale` → resolution stale; `ambiguous` →
resolution ambiguous; `unverified` → resolution unique + evidence/standing
unverified; `partial` → resolution as observed + evidence/coverage partial;
`verified` → resolution unique + evidence/standing verified. The old
ladder stays available as a compatibility projection while the internal
contract migrates.

## 7. Existing JVM Arxana records (fidelity: adapt)

Correspondence assertions are a **'clusion subtype**; correspondence
assessments are instances of the general assessment contract. New writes use
`clusion/assert` / `assessment/append`; existing ids and behaviour stay
readable through a compatibility adapter. No permanent second authority.

## 8. Convergence by record type

| Record | Policy |
|---|---|
| Artifact revisions, vocabulary/type declarations, 'clusions, assessments, promoted projection rules | immutable / versioned (append-only, idempotent by id) |
| Artifact heads, uniqueness-governed registries | CAS |
| Personal presentation state | per-user last-writer-wins, or disposable |
| F2 shared working state of one manifestation (and optionally a co-authored layout) | CRDT/OT if F2 shows it is needed; it emits immutable revisions as evidence |

## 9. Fibres

A fibre is a **derived content entity (solution set)**, not a 'clusion. The
map is a 'clusion; membership and provenance are relations. Queries may
compute fibres transiently; where reproducibility matters (F5) a fibre
entity is materialised, pinned to map revision, input/value,
evaluator/theory version and evidence.

## 10. Authorities

| Question | Authority |
|---|---|
| Record validity | JVM Arxana's versioned type/schema registry |
| Atomic persistence | futon1b's conditional route |
| Interpretation acceptance | the attributable human/institutional policy actor; Arxana records, never manufactures, acceptance |
| Projection layout | the registered projection rule/projector; the client/user for unpromoted view state |
| Lean truth (F3/F5) | the Lean 4.29 kernel; Arxana owns only the provenance-bearing correspondence claim |

## 11. DERIVE-F1 checked against this core

Places where DERIVE-F1 (c75e9e6) must follow the core:

1. **Type declaration missing.** F1's interpretation 'clusion needs a
   declared type (roles, arity, endpoint kinds, method fields, identity
   projection, convergence). Increment 1 freezes it alongside the vocabulary
   record, and `clusion/assert` validates against it.
2. **Targets and interpreters as typed external references.** Agents, jobs,
   missions and turns named as targets or interpreters become typed
   external-reference entities, not free strings.
3. **Dynamics reference.** F1's "method/version" becomes the core's inert
   dynamics reference (method id, version, parameters).
4. **Projection rules are durable.** F1's linear, graph and GCC views are
   registered projection rules with versions; their outputs and receipts
   can stay recomputable test evidence.
5. **Evidence fields.** F1's `exact / parser-attested / model-attested /
   human-attested` is `evidence/basis` (§6); F1 records also carry
   `evidence/standing` and `evidence/coverage`. (Settled in round 5.)

## 12. Fidelity matrix (codex-21, round 7, job invoke-1794099593061-15428-4cecb549)

| Existing system | Verdict | What happens to its data and ids |
|---|---|---|
| Legacy Emacs Arxana (futon4/dev/arxana-*.el: voiced links, scholia, surface forms, link strategies) | **adapt** | translated into manifestations, 'clusions and strategy records; legacy ids and bytes kept through aliases/external references, never rewritten in place; all new `arxana/*` writes use the reserved route |
| 象 turn store and readings (turn records, analyses, drafts; `e-xiang-turn-*` evidence) | **preserve** | stays the immutable source of truth; F1 materialises derived Arxana records that cite its evidence ids and revisions; no migration, no change of owner |
| 馬 alignments and reply-mark parsing | **adapt** | mark parsing becomes a versioned interpreter over `intent-vocabulary/v1`; 馬 alignments become model-basis candidate correspondence 'clusions/assessments keeping their ids, reasons and provenance |
| REPL Circus / Giraffe (PURs, pattern outcomes, `pass.edn`) | **preserve** | domain-owned; Arxana references them through typed external-reference entities and 'clusions; no renaming or migration |
| futon1b generic hyperedges in use (e.g. `:memory/assert`) and registered-but-empty types (`:learning-loop/*`, `:has-operator-turn`) | **preserve** | untouched, ids and types kept; formal Arxana may reference but not reinterpret them; empty types are not promoted into the core until real records witness them |
| WebArxana | **drop** from the required runtime | code kept as a read-only donor of interaction ideas; no live data, no id migration |
| D6 compile/admission shapes (futon3c arxana/compile/) | **adapt** | intent / candidate / decision / receipt stay separate records with their semantics and ids; their relations are exposed through common 'clusion types and the reserved persistence path, not collapsed into one generic edge |

**Intent vocabulary presentations.** All three (turnfeed `intent-mark`,
`xiaoxiang-preview.el`, `reply-proforma.md`) are **generated** from
`intent-vocabulary/v1`. Hand-written prose stays around generated regions;
CI refuses drift. (Testing three independent copies would keep the
duplication.) This belongs to F1 increment 1.
