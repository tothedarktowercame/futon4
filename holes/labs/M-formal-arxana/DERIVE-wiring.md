# DERIVE-wiring — system wiring, build order, and stated choices

claude-13, 2026-11-08. Drafted from DERIVE-core.md and DERIVE-F1…F6.md,
which record codex-21's clarifications (rounds 1–7). Nothing here adds
scope; where a line is my reading rather than codex-21's words, it says so.

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
 │ JVM ARXANA (futon3c :7070, arxana.v1 envelopes)       authority: VALIDITY │
 │  type/schema registry  ◄── type declarations, intent-vocabulary/v1        │
 │  write ports:  revision/ingest · clusion/assert · assessment/append       │
 │  read ports:   entity/get · graph/query · scope/resolve (F0)              │
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
 │ LEAN 4.29 KERNEL (batch, F3/F5)   │                       │
 │  authority: LEAN TRUTH            │──── kernel evidence ──┘ (via JVM Arxana)
 └───────────────────────────────────┘
 ┌───────────────────────────────────┐
 │ ADMISSION (external policy actor; │  decision → (bounded op) → receipt
 │  Joe for fixtures)                │  authority: ACCEPTANCE / ADMISSION
 └───────────────────────────────────┘
```

Trust boundaries:
- **Model output** (象, 馬, informaliser) enters only as candidate records
  with `evidence/basis model`. Acceptance is a separate attributable
  assessment by a human or institutional actor.
- **Kernel evidence** comes from re-running Lean 4.29 on pinned bytes;
  recorded results are a cache, not the gate.
- **Client state** (cursor, viewport, unpromoted coordinates) never reaches
  the store except by explicit promotion to a revision or layout revision.
- **Execution** happens only after an admission decision (F4); candidates
  carry no credentials.

## 2. Build order (separate from the wiring)

| Step | Builds | Needs | Fixture(s) it unlocks |
|---|---|---|---|
| C1 | type registry + declarations; `intent-vocabulary/v1` and its three generated presentations; record shapes, id rules, golden records | — (F0 done) | all |
| C2 | `clusion/assert`, `assessment/append` in JVM Arxana and the futon1b conditional route; namespace/atomicity/idempotency tests | C1 | all |
| F1 | turn materialiser; conversation exporter + ingest; futon4a linear/graph/GCC views; relocation demo; operator assessment | C2 | F2, F4 |
| F3 | correspondence adapter (old ladder as compatibility view); a93J02 manifestations; generated informalisation; mark8 as model assessments; synthetic contradiction; Lean 4.29 integration check | C2 | — |
| F5 | futon5 Lean 4.29 project (square and cube/power fibre lemmas); geometry types; fibre entity; two-panel HTML/SVG projector opened from futon4a | C2 (futon4a page opening shared with F6) | F6 |
| F2 | write capability in futon4a and the futon6 client; expected-head ingest; three-way rebase; offline replay | F1 | — |
| F4 | intent producer; D6 chain over F1 turns; composite 'clusion + futon5 wiring diagram; one admitted bounded op + refusal | F1 | — |
| F6 | `supports` (and siblings) types; Tufte-style page projector; move-vs-resupport demo | F5 | — |

F1, F3 and F5 can proceed in parallel once C2 is done. (Ordering is my
reading of the "Depends on" lines in each DERIVE-Fn note.)

## 3. Non-obvious choices

1. IF a reply mark is part of what an interpretation is about, HOWEVER a mark
   is something that occurs in a manifestation rather than a relation
   between contents, THEN a mark is an occurrence entity used as evidence by
   the interpretation 'clusion, not a 'clusion, BECAUSE otherwise every
   glyph would carry relation semantics it does not have (codex-21, r2 B).
2. IF futon1b already gives hyperedges a default id, HOWEVER that id sorts
   endpoints and so erases roles (MAP-Q1), THEN a 'clusion id digests its
   ordered, role-labelled endpoints, and roles are validated route data,
   BECAUSE two 'clusions with the same endpoints in different roles are
   different claims (r2 C).
3. IF plain futon1b hyperedges under a non-`arxana` type would work today,
   HOWEVER that route has last-writer-wins and no Arxana validation, THEN F1
   extends the reserved arxana route with two typed operations, BECAUSE
   evading the namespace gate would give Arxana records two authorities
   (r1 2, r3 2).
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
   accepted sequencer (mission F2; r5 4).
8. IF F3 needs an explicit contradiction, HOWEVER the real a93J02 mismatch
   is one of method, not statement, THEN the real case is kept as
   methodological divergence and a labelled synthetic mutation supplies the
   contradiction, BECAUSE relabelling genuine evidence would falsify it
   (r5 8).
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
