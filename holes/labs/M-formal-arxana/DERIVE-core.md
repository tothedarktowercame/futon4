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

## 13. Operation-attempt receipts (codex-21, round 8, job invoke-1794103156786-15461-2a3f0d13)

When an interpreter or operation did not run, or ran and found nothing, the
core records an immutable **operation-attempt receipt entity**, not a void
'clusion. Fields: dynamics id/version, requested inputs, outcome
`not-run | refused | failed | ran-no-output | ran-with-output`, reason,
actor/tool, time/revision. Only `ran-no-output` with a declared search
envelope supports a claim of typed absence. Later interpretation 'clusions
may cite the receipt.

## 14. External stores as referenced authorities

Records owned by another store (e.g. 驴's `:lu/occurrence` hyperedges in the
minus40 futon1b store on :7093) are **typed referenced authorities**. A
reference names store identity/endpoint, record id, schema authority,
valid-time/revision, and digest where available. Arxana may keep a
rebuildable indexed projection/cache of them, but never a mirror that acts
as a second source of truth. futon1b's conditional route is the sole
authority for `arxana/*` records, not for all source evidence (this narrows
§10's "atomic persistence" row accordingly).

## 15. Reads, indexes and aggregates

- `graph/query` stays bounded: typed filters, reverse-role lookup, ordering
  and limit, all index-backed. No arbitrary online group-by on the prompt
  path.
- **Mandatory index:** `(edge-type, endpoint-role, endpoint-id,
  valid/event-time, edge-id)`. It serves reverse lookup and ordered
  session windows.
- Aggregates are **materialised named entities/projections** with a source
  cursor/watermark and a deterministic bulk rebuild. Pattern status/reason
  aggregates are indexed by `(pattern-id, window, status, reason)`.
- No full scan on the ~5 s prompt path.

## 16. Two type rulings

- 馬's verdict is a model-basis `pattern/occurrence` 'clusion when it asserts
  that a pattern occurs in a turn; it is an assessment only when it judges a
  separately identified occurrence 'clusion.
- `shown` is durable **exposure-event data**: append `proposal/shown`
  events (proposal id, surface/projection revision, actor/session, time);
  the boolean is derived from them, never stored as a mutable flag.

## 17. Identity, evidence records, recursion bounds, C1 scope (codex-21, round 9, job invoke-1794104774919-15529-e9abaa32)

Raised repeatedly in ARGUE (argue/DIALOGUE.md P1, P2, P7, P10, P13).

- **Identity digest** is declared per type. Default: `type-revision +
  ordered role-labelled endpoints + declared asserted-content fields`.
  Interpreter, evidence and dynamics/method version are **out** by default;
  a type may opt one in only when changing it changes the claim.
- **Evidence is a separate record**, not fields on the 'clusion (amends §6's
  placement and §3):
  - `'clusion`: `{id, type-revision, ordered role-endpoints, asserted-content}`
  - `evidence-attestation`: `{id, subject-clusion-id, interpreter/producer,
    basis, standing, coverage, method/version, source-refs}`
  Several attestations may support or dispute one immutable claim; two
  interpreters making the same claim share one 'clusion id with two
  attestations. Assessments stay separate again (§6). The §6 fields and the
  §3 dynamics reference live on the attestation unless the type opts them in.
- **Recursion.** General graphs may contain explicit cycles; composition
  graphs default to DAGs. A cyclic composition needs a type-declared
  fixed-point operator, which this mission does not provide. Reads suppress
  visited ids and take caller-visible `max-depth` and `max-nodes`, returning
  a typed truncation. C1 declares the recursion/cycle policy, C3 enforces
  query bounds, F4 owns the recursive-composition acceptance tests.
- **C1 freezes** roles, arity, endpoint kinds, identity projection and
  convergence policy. Convergence may inherit an immutable, versioned
  record-kind default (§8), but the resolved policy is explicit in the
  declaration. Method/dynamics fields default by record kind and stay out
  of identity unless the type opts them in. (claude-19's ARGUE P9, amended:
  convergence is frozen, not merely defaulted.)

## 18. INSTANTIATE design review (codex-21, round 10, 2026-11-08)

Round 9 governs C1 where earlier prose differs: a 'clusion does **not** carry
an inert dynamics reference by default. Producer/interpreter and method,
version and parameters belong to an `evidence-attestation`. A type may include
a dynamics reference in its asserted content and identity projection only when
the operation itself is part of the semantic claim. This corrects §1's
"required dynamics/method fields", §3's "every 'clusion", and the refactored
ARGUE P10 wording; it does not change F4's separate capability, admission and
receipt boundary.

VERIFY's Lean `Store.acceptance` reducer (latest assessment wins) is not a
general Arxana rule: assessments by different attributable actors may coexist
and remain unresolved. Any selected assessment requires a named policy/query;
append order alone is not semantic authority. C1 therefore defines assessment
records but no global latest-wins reduction. The Lean model should be amended
before a later increment claims that selection theorem as conformance.
