# DERIVE-wiring — system wiring, build order, and stated choices

claude-13, 2026-11-08. Drafted from DERIVE-core.md and DERIVE-F1…F6.md,
which record codex-21's clarifications (rounds 1–9). Nothing here adds
scope; where a line is my reading rather than codex-21's words, it says so.

**Reviewed by codex-21** (2026-11-08, job
invoke-1794104781692-15531-06d61048); its corrections are applied below.

## 1. Wiring diagram (records, operations, authority)

```
 SOURCES (immutable, owned elsewhere)          INTERPRETERS (candidates only)
 ┌─────────────────────────────────┐          ┌──────────────────────────────┐
 │ 象 turn store + turn evidence   │          │ 象 seat (readings)            │
 │   (preserve; futon1b evidence)  │          │ 馬 seat (mark alignments)     │
 │ artifact bytes: exported conv.  │          │ informaliser (F3)             │
 │   .md, Lean files, TeX, HTML    │          │   basis = model; never        │
 │ Giraffe / PUR / pattern records │          │   authority                   │
 └───────────────┬─────────────────┘          └──────────────┬───────────────┘
                 │ evidence ids, revisions                    │ candidate 'clusions
                 ▼                                            ▼
 ┌───────────────────────────────────────────────────────────────────────────┐
 │ JVM ARXANA (Arxana HTTP, default :7084; arxana.v1)    authority: VALIDITY │
 │  type/schema registry  ◄── type declarations, intent-vocabulary/v1        │
 │  write ports:  revision/ingest · clusion/assert · evidence/append ·       │
 │                assessment/append · attempt/append (receipts, C3)         │
 │  read ports:   entity/get · graph/query (bounded) · scope/resolve (F0)    │
 │  evidence adapter: Lean kernel runs → kernel evidence attestations       │
 │  D6 (F4):      action/compile → candidate (non-executable)                │
 │  projectors:   registered projection rules (linear, graph, GCC, SVG, page)│
 └───────────────┬───────────────────────────────────────────┬───────────────┘
                 │ store_http (validated writes)             │ projections (ids)
                 ▼                                           ▼
 ┌───────────────────────────────────┐       ┌────────────────────────────────┐
 │ FUTON1B conditional route (:7073) │       │ PROJECTION CLIENTS             │
 │  authority: ATOMIC PERSISTENCE    │       │  futon4a (Emacs; EWW for HTML) │
 │  arxana/* reserved; CAS heads;    │       │  futon6 VS Code (F2 writer)    │
 │  idempotent message ids;          │       │  HTML/SVG pages (F5, F6)       │
 │  immutable records append-only    │       │  local presentation state      │
 └───────────────────────────────────┘       │  stays local unless PROMOTED   │
                                             └───────────────┬────────────────┘
                                                             │ edits → revision/ingest
 ┌───────────────────────────────────┐                       │ (expected head, F2)
 │ LEAN 4.29 KERNEL (batch, F3/F5)   │  run results → JVM Arxana evidence adapter
 │  authority: LEAN TRUTH            │  → evidence/append → projections
 └───────────────────────────────────┘  (never directly to projection clients)
 ┌───────────────────────────────────┐
 │ ACCEPTANCE (human/institutional   │  → assessment/append (assessment record)
 │  actor; Joe for fixtures)         │  authority: ACCEPTANCE of interpretations
 └───────────────────────────────────┘
 ┌───────────────────────────────────┐
 │ ADMISSION (policy actor; Joe for  │  decision → (bounded op) → receipt
 │  fixtures; F4)                    │  authority: ADMISSION of operations
 └───────────────────────────────────┘
 ┌───────────────────────────────────┐
 │ REFERENCED AUTHORITIES (typed)    │  象 turn store, minus40/驴, Giraffe:
 │  outcomes: unavailable · digest   │  referenced by external-reference entities,
 │  mismatch · schema drift          │  never mirrored; a cache never answers for them
 └───────────────────────────────────┘
```

Trust boundaries:
- **Model output** (象, 馬, informaliser) enters only as candidate
  'clusions with separate evidence attestations (`evidence/basis model`,
  DERIVE-core §17). Acceptance is a separate attributable
  assessment by a human or institutional actor.
- **Kernel evidence** comes from re-running Lean 4.29 on pinned bytes; the
  JVM Arxana evidence adapter records it as attestations. Recorded results
  are a cache, not the gate.
- **Client state** (cursor, viewport, unpromoted coordinates) never reaches
  the store except by explicit promotion to a revision or layout revision.
- **Acceptance and admission are different questions.** Acceptance of an
  interpretation is an assessment record; admission of an operation is an
  admission-decision record (F4). Joe holds both for the fixtures.
- **Execution** happens only after an admission decision (F4); candidates
  carry no credentials.

## 2. Build order (separate from the wiring)

| Step | Builds | Needs | Fixture(s) it unlocks |
|---|---|---|---|
| C1 | type registry + declarations, each freezing roles, arity, endpoint kinds, identity projection, resolved convergence policy and recursion/cycle policy; the evidence-attestation shape (DERIVE-core §17); `intent-vocabulary/v1` and its three generated presentations; golden records | — (F0 done) | all |
| C2 | `clusion/assert`, `evidence/append`, `assessment/append` in JVM Arxana and the futon1b conditional route; namespace/atomicity/idempotency tests | C1 | all |
| C3 | mandatory index `(edge-type, endpoint-role, endpoint-id, valid/event-time, edge-id)` behind `graph/query` (filters, reverse-role, order, limit); bounded recursive reads (visited-id suppression, `max-depth`, `max-nodes`, typed truncation); operation-attempt receipts via `attempt/append`; typed external-store references; materialised aggregates with watermark + deterministic rebuild, **bounded to the named index and the demonstrated Circus queries** — not a general aggregation subsystem, and not a blocker for fixtures that use no aggregates (DERIVE-core §13–§15, §17) | C2 | F1 projections at scale; F4; M-repl-circus as a client |
| F1 | turn materialiser; conversation exporter + ingest; futon4a linear/graph/GCC views; relocation demo; operator assessment | C2 | F2, F4 |
| F3 | correspondence adapter (old ladder as compatibility view); a93J02 manifestations; generated informalisation; mark8 as model attestations; synthetic contradiction (adapter records kernel evidence; an attributable fixture-author assessment marks it contradicted); Lean 4.29 integration check | C2 | — |
| F5 | futon5 Lean 4.29 project (square and cube/power fibre lemmas); geometry types; fibre entity; two-panel HTML/SVG projector opened from futon4a | C2 (futon4a page opening shared with F6) | F6 |
| F2 | write capability in futon4a and the futon6 client; expected-head ingest; three-way rebase per the round-9 contract (canonical bytes, patch identity, overlap classes, retained intent; DERIVE-F2); offline replay | F1 | — |
| F4 | intent producer; D6 chain over F1 turns; composite 'clusion (DAG by default) + futon5 wiring diagram; recursive traversal exercising C3's bounds; one admitted bounded op + refusal | F1, C3 | — |
| F6 | `supports` (and siblings) types; Tufte-style page projector; move-vs-resupport demo | F5 | — |

F1, F3 and F5 can proceed in parallel once C2 is done; C3 can run alongside
them and is needed before any client reads on a latency budget (M-repl-circus's
prompt path) and before F4. (Ordering is my
reading of the "Depends on" lines in each DERIVE-Fn note.)

## 3. Non-obvious choices

1. IF a reply mark is part of what an interpretation is about, HOWEVER a mark
   is something that occurs in a manifestation rather than a relation
   between contents, THEN a mark is an occurrence entity used as evidence by
   the interpretation 'clusion, not a 'clusion, BECAUSE otherwise every
   glyph would carry relation semantics it does not have (codex-21, r2 B).
2. IF futon1b already gives hyperedges a default id, HOWEVER that id sorts
   endpoints and so erases roles (MAP-Q1), THEN a 'clusion id digests its
   type revision, ordered role-labelled endpoints and type-declared
   asserted-content fields (interpreter and method version out by default),
   and roles are validated route data, BECAUSE two 'clusions with the same
   endpoints in different roles are different claims, and two interpreters
   making the same claim make one claim with two attestations (r2 C, r9 Q1).
3. IF plain futon1b hyperedges under a non-`arxana` type would work today,
   HOWEVER that route has last-writer-wins and no Arxana validation, THEN F1
   extends the reserved arxana route with two typed operations, BECAUSE
   evading the namespace gate would give Arxana records two authorities
   (r1 2, r3 2). The operations are `clusion/assert`, `evidence/append` and
   `assessment/append` (r9 Q2).
4. IF standing could be one ladder (as correspondence has), HOWEVER one
   ladder lets "verified" mean both "parsed correctly" and "semantically
   right", THEN standing is orthogonal fields (resolution; evidence basis,
   standing, coverage) and acceptance is a separate assessment, BECAUSE each
   field then has one meaning (r1 5, r4 6, r5 0).
5. IF edit proposals have five strata worth recording, HOWEVER most edits
   are routine and only some need audit, THEN an edit proposal is a
   transient request, preserved through D6 only where audit or execution
   matters, BECAUSE storing every keystroke-level proposal as a record would
   bury the ones that carry decisions (r4 5).
6. IF a fibre could be modelled as a relation, HOWEVER a fibre is a set of
   solutions computed from a map and a value, THEN it is a derived content
   entity, materialised and pinned when reproducibility matters, BECAUSE the
   map is the relation; the fibre is what the map yields (r4 9).
7. IF live co-editing usually suggests a CRDT, HOWEVER futon1b already has a
   tested CAS sequencer, THEN F2 starts with the single sequencer plus
   rebase and decides on a CRDT only by the offline-replay test, BECAUSE a
   CRDT is warranted only when legitimate histories must survive without an
   accepted sequencer (mission F2; r5 4). The decision covers the declared
   workload; lost legitimate history, an unacceptable sequencer or required
   offline convergence reopen it (r9 Q3).
8. IF F3 needs an explicit contradiction, HOWEVER the real a93J02 mismatch
   is one of method, not statement, THEN the real case is kept as
   methodological divergence and a labelled synthetic mutation supplies the
   contradiction, BECAUSE relabelling genuine evidence would falsify it
   (r5 8). The adapter records the Lean run as kernel evidence; an
   attributable fixture-author assessment marks the mutation contradicted
   (r9 Q5).
9. IF the D6 compiler already exists, HOWEVER compiling conversational
   intents needs an intent producer and new intent types, THEN D6 compilation
   lands in F4, not F1, BECAUSE F1 is about representing and projecting the
   intent graph, and F4 is about composing stages (r1 1, r6 1).
10. IF the intent vocabulary could be checked against its three copies,
    HOWEVER checking three hand-kept copies keeps the duplication, THEN the
    three presentations are generated from `intent-vocabulary/v1` with CI
    refusing drift, BECAUSE one source is the only way the copies stay equal
    (r7).
11. IF the F5 lemma could sit beside other Lean work in apm-lean, HOWEVER
    apm-lean is about APM proofs, THEN the unit-circle lemmas get their own
    small Lean 4.29 project under futon5, BECAUSE unrelated theory in apm-lean
    would blur what that repository certifies (r6 4).
12. IF `[0, 2π)` is the obvious second view, HOWEVER it has a cut and is not a
    global continuous chart, THEN it is named an angular projection, BECAUSE
    the geometry vocabulary should not claim a chart where there is none
    (r6 5).
