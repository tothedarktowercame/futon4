# D7 — architecture synthesis

Mission: `M-jvm-arxana`. Phase: DERIVE. Inputs: MAP, D1–D6 and progressive
ARGUE through `dbeb91c`. Status: selected design pending ARGUE checkpoint.

## Selection

Select **A2: a headless JVM Arxana service over futon1b**, with one narrow
store extension: futon1b atomically compares expected head and commits immutable
revision, head, command outcome and journal event. Arxana is the canonical
client interface and owns evaluation/correspondence/protocol compilation;
futon1b remains durable graph and transaction authority.

**D7-DR1.** IF clients need one rapidly evolving semantic protocol; HOWEVER
durable atomicity must hold at the store boundary; THEN separate headless
Arxana from futon1b but add a narrow conditional transaction to futon1b;
BECAUSE this isolates protocol/evaluator evolution without inventing an unsafe
second consistency domain.

**D7-DR2.** IF client evaluation can provide responsive previews; HOWEVER its
evidence is client-attested; THEN only authority-side deterministic evaluation
may create promotable resolution evidence in v1; BECAUSE shared authority
cannot rest on unverified editor claims. Signed external evidence is deferred.

**D7-DR3.** IF interpretation may be produced by humans, models or consumers;
HOWEVER Arxana owns no universal theory of intent; THEN accept durable
provenance-bearing interpretation records under versioned trust profiles and
compile untrusted records only conditionally; BECAUSE provenance/trust is the
common contract while interpretation policy remains replaceable.

**D7-DR4.** IF futon1b's generic write routes remain available to existing
writers; HOWEVER `arxana/*` identity and uniqueness invariants must not be
bypassable; THEN reserve the `arxana/*` type/property namespace at the store
boundary and accept its writes only through the authenticated conditional
transaction route, which validates uniqueness in the same transaction;
BECAUSE Arxana-side validation followed by an open generic write would state an
invariant without enforcing it.

## Comparison matrix

Scores: 3 strong, 2 viable with stated work, 1 conflicts/high risk.

| Criterion | A1 extended futon1b | A2 headless + atomic store route | A3 client evaluators |
|---|---:|---:|---:|
| One authority per question | 3 | 3 | 1 |
| Live-record compatibility | 3 | 3 via D1 adapter | 2 |
| Deterministic resolution | 2, couples store/runtime | 3 | 1, duplicated runtimes |
| Atomic revision refusal | 3 | 3, same narrow store primitive | 1 |
| Artifact/revision access | 2 | 3 via ArtifactProvider | 2, local divergence |
| Subscriptions/replay | 2 | 3 from durable journal | 1 |
| Failure isolation | 1 | 3 | 2 |
| Protocol evolution | 1 | 3 | 1 |
| Second-editor cost | 2 | 3 | 1 |
| Testability | 2 | 3, pure evaluator + adapters | 2 |
| Deployment simplicity | 3 | 2 | 3 |
| Client-attested evidence risk | 3 | 3 | 1 |

A1 is workable but makes unsettled client/evaluator evolution a store release.
A3 remains useful for non-authoritative previews but fails the core invariant
for writes and promotable evidence. A2 wins without choosing the rejected
sequencer/fencing variant: atomicity stays in futon1b.

## Authority assignment

| Question/state | Sole authority |
|---|---|
| Durable entity/hyperedge identity and conditional commit | futon1b |
| Artifact revision bytes/digest | immutable ArtifactProvider governed by Arxana |
| Strategy semantics and resolution result | headless Arxana evaluator |
| Manifestation registry and correspondence assertions | headless Arxana, persisted in futon1b |
| Canonical protocol/schema/semantic digest registry | headless Arxana |
| Command journal/head/outcome | futon1b conditional transaction |
| Subscription projection, cursor retention and resync | headless Arxana over durable journal |
| Intent interpretation | external named interpreter under trust profile; durable record in futon1b |
| Candidate-action compilation | headless Arxana |
| Institutional admission | external policy authority |
| Execution | explicit executor using D3 transaction contract |
| Editor markers, decorations and coordinate conversion | each client, non-authoritative |

Salience and Plexus remain outside v1 and require a witnessed follow-up
contract. Domain scores/verdicts/order evidence stay namespaced.

Futon1b's existing generic entity, hyperedge and batch routes reject creation
or mutation of `arxana/*` typed records and reserved properties. The conditional
route authenticates the Arxana service principal, validates manifestation tuple
uniqueness and alias rules against current durable state, and commits the
record/head/journal atomically. Reads remain available through documented
interfaces. Legacy non-Arxana families keep their present routes; migration
does not retroactively reserve their namespaces.

## Wiring and flow

```text
 Emacs client ─┐                       ┌─ ArtifactProvider (immutable bytes)
 VS Code ext ──┼─ arxana.v1 ─> Headless JVM Arxana
 Oxide pane ───┘                 │ evaluator / correspondence / compiler
                                 │ schema + semantic-digest registry
                                 │ subscription projection
                                 v
                         D1 storage adapter
                                 │
                 reads/batches ──┼── conditional command transaction
                                 v
                  futon1b / XTDB durable graph
             entities · hyperedges · heads · journal · outcomes
                                 │
                                 └── ordered journal events ──> Arxana ──> clients

 external interpreter ─ provenance-bearing intent ─> Arxana compiler
 external policy ─ admission decision ─> executor ─ guarded command ─> futon1b
```

Request flow: client names immutable revisions → Arxana verifies bytes and
computes → optional promotion persists evidence → mutation requires external
admission and expected head → futon1b atomically refuses or commits → Arxana
projects journal event. No success precedes durable outcome.

## Consolidated single-locus invariants

1. For each `(question, artifact revision, semantics version)`, exactly one
   Arxana authority result/digest exists; client projections cannot redefine it.
2. Each artifact revision ID denotes exactly one canonical byte sequence.
3. Each manifestation authority/family/role tuple denotes exactly one stable
   manifestation ID; conflicting definitions are refused.
4. Every resolution/correspondence/action names exact input revisions and
   evidence; durable identity never rests solely on position.
5. Multiple viable candidates remain ambiguous until an attributable
   acceptance; evidence acceptance never equals action admission.
6. Exactly one conditional futon1b transaction wins for a given expected head;
   stale operations preserve recoverable commands and never overwrite.
7. An authoritative promoted resolution was evaluated/replayed by Arxana v1.
8. Unprovenanced intent cannot yield `compiled`; external policy cannot alter
   candidate provenance; receipts cannot rewrite predictions.
9. Domain extensions cannot redefine core IDs, revisions, operations or
   outcomes.
10. Unsupported legacy declarations/capabilities fail explicitly.

Each maps to D1–D6 tripwires and becomes an executable integration assertion.

## Open-obligation disposition

| Obligation | Disposition |
|---|---|
| O-D1b query/index shape | entity + typed hyperedges selected; add query/index test before schema freeze |
| O-D1c salience/Plexus | excluded from v1; workflow audit and separate mission if witnessed |
| O-D2b artifact access | Arxana-owned ArtifactProvider |
| O-D2c acknowledgement provenance | closed in D2: separate immutable actor/policy/reason `resolution-acceptance` record linked to assertion |
| O-D2d non-promoted evidence | closed in D2: ephemeral response only; promotion atomically preserves complete semantic evidence |
| O-D3a atomicity/fencing | store-side conditional transaction selected; sequencer fencing not used |
| O-D3c client evidence | preview only; authority replay required for promotion |
| O-D7b journal/head/retention | journal/head futon1b; cursor retention/resync Arxana |
| O-D7c fixture/assertion authority | Arxana fixture builder + manifestation registry; futon1b persistence |
| O-D7d response digest | Arxana versioned schema/projection registry |
| O-D7e interpretation | external interpreter + Arxana trust profile and conditional fallback |
| O-D7f namespace enforcement | futon1b reserves `arxana/*`; authenticated conditional route atomically validates uniqueness; generic writes refuse |

## Implementation increments

1. **Fixture gate.** Commit the D4 negative rejection test and repaired positive
   manifest/build; no client demonstration proceeds without it.
2. **Atomic store primitive.** Add futon1b conditional command transaction,
   reserved-namespace gate, authenticated service principal, idempotency journal
   and two-writer/crash/bypass tests.
3. **Service spine.** Implement D1 adapter, capabilities handshake,
   ArtifactProvider and canonical schema/digest registry.
4. **Resolve.** Implement D2 evaluator, legacy translation and promotion with
   attributable acceptance.
5. **Correspond.** Implement D4 registry/assertions/queries against fixture.
6. **Project.** Implement D3 subscriptions plus thin Emacs, VS Code and Oxide
   proof clients; run semantic-digest equality transcript.
7. **Compile.** Implement D6 trusted/conditional intent and read-only candidate
   compilation; no production executor required for mission proof.
8. **Integrate.** Run end-to-end acceptance, workflow audit and compatibility
   matrix; publish interfaces for adjacent missions.

Each increment is independently reversible. New record types are additive;
legacy IDs/JSON remain readable; the mission-scope family stays write-frozen
until producer/census repair. Client rollout is capability-negotiated and old
clients continue using existing paths during proof deployment.

## Acceptance plan

- D1 legacy round-trip and capability truth tests.
- D2 golden anchor, duplicate ambiguity and deterministic replay tests.
- D3 two-writer single-winner, incident replay, crash republish and cursor
  resync tests against real futon1b transaction route.
- D4 negative fixture rejection plus positive three-case correspondence.
- D5 Emacs/VS Code/Oxide canonical digest equality and typed refusal parity.
- D6 deterministic candidate, ambiguous/conditional grounding, separate
  admission and immutable receipt tests.
- Cross-cutting: enumerate any conflicting single-locus tuple; zero permitted.
- Attempt direct generic writes of `arxana/*` records and conflicting
  manifestation tuples: every bypass is refused without partial mutation.
- Fidelity: execute every preserved/adapted tripwire and workflow-audit dropped
  salience/Plexus before declaring completion.

## Client proof-presentation profile

Client-side ARGUE review establishes a bounded proof-presentation profile over
the common model; it does not add proof pedagogy to JVM authority.

- `proof/move` is a domain entity type using D1 identity. One move may have
  zero or many revision-pinned D4 manifestations: authored informal proof,
  readable Lean, compiled/generated rendering and separately generated
  informalization. Generated and authored manifestations retain distinct IDs
  and provenance.
- Mark8 mining, declaration/source structure, exact anchors, model generation
  and human review are evidence sources with independent provenance and
  confidence/standing. None becomes authoritative by score alone; the observed
  mark8 yield is candidate evidence, not an alignment oracle.
- D4 `correspondence-assessment` records accepted/rejected judgments. Explicit
  absence assesses an expected role under a bounded search, rather than
  interpreting a missing row. Resolution acceptance, correspondence assessment
  and action admission remain three different judgments.
- Clients may ask for all available manifestations of a move or the next
  absent/ambiguous move using a domain-owned move-order relation. Arxana returns
  facts and assessments; layout, lesson sequence and presentation remain
  client-owned.

Acceptance is layered and must not be collapsed:

1. **Transport:** messages, revisions, refusal and subscription semantics.
2. **Rendering integrity:** the declared source/generated pipeline produced the
   named revision and locators without provenance loss.
3. **Correspondence:** assertions, ambiguity, direction and assessments satisfy
   D4 invariants.
4. **Usefulness:** a reader recovers the argument, explains a move choice,
   navigates successfully or completes a masked exercise.

Only the first three are JVM Arxana conformance. Usefulness is a namespaced
consumer evaluation record linked to move, manifestation, artifact revision,
participant/cohort, instrument and outcome; it may inform later design without
changing common correspondence semantics. The PythonEmit→leantotex-python→
LaTeXML-Oxide proof-of-concept therefore establishes a candidate rendering
path, not readability or pedagogical success.

Profile tripwires: one move returns multiple manifestations without identity
merger; generated output cannot alias authored material; conflicting evidence
and assessments coexist; `next unmatched` requires an explicit domain order;
and a useful/unuseful rating cannot change transport or correspondence
conformance.

## Rollback and failure containment

Disable the Arxana service/client capability flag; no legacy record is deleted.
Conditional route writes are additive and idempotent. Derived assertions may be
recomputed from immutable inputs; journal/outcomes remain audit evidence.
Rollback never rewrites artifact revisions or hides admitted receipts.

## DERIVE exit claim

D1–D7 now specify entities, relations, invariants, flow, interfaces, fidelity,
architecture, decisions and acceptance tests. Implementation can proceed
without deciding authority, identity, consistency or donor behaviour ad hoc.
The D7 ARGUE checkpoint upheld the selection; O-D7f and the omitted O-D2c/d
receipts are dispositioned above. **DERIVE exit: Met.**
