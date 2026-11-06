# M-jvm-arxana — one Arxana authority across Emacs and VS Code

**Date:** 2026-11-06  
**Status:** OPEN — VERIFY (ARGUE completed 2026-11-06)
**Owner:** Joe (operator); unassigned technical driver
**Repos:** `futon4` (Arxana model and Emacs client), `futon1`/XTDB (durable
graph), `futon3c` (scope view, WYSIWYG and prospective clients)  
**Related:** `futon3c/holes/missions/M-repl-circus.md`,
`futon3c/holes/missions/M-ways-of-working.md`,
`futon3c/holes/missions/M-latex-wysiwyg.md`, `M-webarxana`

## Operator anchor

Joe, 2026-11-06:

> Work in a more technical mission, something like M-jvm-arxana that refers
> to the existing Elisp+XTDB hybrid and ports it to what we'd need to have an
> Arxana variant that works across VS Code and Emacs, with the various aligned
> interfaces. Then we can go back to the other missions and build their
> interfaces, possibly beginning a Campaign at that time when the technical
> work is ready.

The implementation may eventually need a CRDT, but that choice does not decide
the core design. The shared contract must admit either revision-checked
single-authority transactions or collaborative merge without changing the
identity, scope, correspondence and action models exposed to clients.

## Gap

Arxana's current logical model and durable graph live across Emacs Lisp and
XTDB. The combination can identify articles, events, plexuses and scope
strategies, but no JVM authority exposes that model as one typed protocol to
multiple clients. Emacs therefore remains both a client and part of the
authority, while the TypeScript scope view, LaTeXML-Oxide WYSIWYG surface and a
prospective VS Code interface each have only part of the picture.

The missing technical waist is a JVM-backed Arxana service that:

- preserves the existing logical entities and typed hyperedges in XTDB;
- resolves cascaded scope strategies against current artifacts;
- relates semantic entities to manifestations in Lean, LaTeX, rendered HTML,
  generated Python, code and prose;
- reports resolution evidence, ambiguity, provenance and temporal standing;
- serves subscriptions and inference-ready action descriptions to Emacs,
  VS Code and other clients through the same contract.

This is a hypertext and correspondence authority, not a reimplementation of
either editor.

## Candidate invariant

For any question about entity identity, graph relation, scope meaning,
manifestation correspondence or action grounding, Emacs and VS Code receive
the same answer from the JVM authority. Editor-local positions, decorations,
markers and view state are disposable projections of that answer.

The shape has at least these instances:

- `shared-authority/entity-identity`
- `shared-authority/scope-resolution`
- `shared-authority/manifestation-correspondence`
- `shared-authority/action-grounding`

## Theoretical anchoring and prior work

The mission is anchored in Arxana's existing identity-through-links model, the
one-logical-hypergraph account in `AGENTS.md`, strategy-based rediscovery in
`dev/arxana-links.el`, and the one-authority-per-question discipline. It also
draws on `futon3/library/storage/graph-memory-contract.flexiarg`: downstream
tools need documented entity, relation and salience shapes rather than private
interpretations of stored records.

The existing implementations are evidence, not a specification to translate
line by line. A later DERIVE phase must include the lifecycle's fidelity
contract: inventory donor capabilities and classify each as preserve, adapt or
drop, with tripwire tests for preserved behaviour.

## Source material for MAP

- `AGENTS.md` — current logical model and storage posture.
- `dev/arxana-links.el` and `dev/arxana-data-constraints.el` — strategy and
  scope shapes in the Emacs implementation.
- `dev/arxana-store.el` and the active futon1/XTDB HTTP routes — storage and
  temporal authority actually available to clients.
- `futon3c/web/scopes/scopes.ts` and its fixtures — portable TypeScript scope
  projection and anchored-text behaviour.
- `futon3c/emacs/latex-wysiwyg.el` and
  `futon3c/holes/missions/M-latex-wysiwyg.md` — Oxide source maps, navigation,
  guarded edits and refusal behaviour.
- `futon3c/holes/missions/M-repl-circus.md` and
  `futon3c/holes/missions/M-ways-of-working.md` — prospective consumers whose
  domain rules remain outside this mission.

## Design hypotheses to test in MAP

The vocabulary and slices below are hypotheses inherited from the design
conversation. They are not DERIVE decisions. MAP may merge, rename or reject
them after checking the live schemas, APIs and clients.

The initial shared vocabulary is deliberately small:

- **Entity** — a stable semantic or operational identity.
- **Hyperedge** — a typed relation with endpoints and provenance.
- **Artifact revision** — a versioned source or derived representation.
- **Manifestation** — an entity as it appears in an artifact.
- **Scope strategy** — a declarative, cascaded procedure for rediscovering a
  manifestation; a resolved range is revision-specific evidence, not identity.
- **Intent** — a typed, revisable reading of a participant act.
- **Candidate action** — grounded target, operation, preconditions, predicted
  effects, warrants, risks and unresolved questions.
- **Admission decision** — admitted, refused, deferred or requiring consent,
  with reasons; institutional policy remains outside this mission.
- **Action receipt** — the attempted operation and resulting revisions,
  entities, evidence and warrants.

XTDB is the durable temporal authority. XTQL supplies exact graph joins,
temporal reconstruction, projections and candidate generation. Ranking,
uncertainty and policy may be supplied later by Giraffe, War Machine or another
consumer without being built into this service.

## Scope

### In

- A JVM API over the existing Arxana/XTDB logical model.
- A documented, tested schema for the core vocabulary above.
- Cascaded scope-strategy resolution with explicit `resolved`, `ambiguous`,
  `missing` and `stale` results and resolution evidence.
- Manifestation and correspondence queries across artifact revisions.
- A subscription protocol for entity, scope, artifact and correspondence
  changes.
- A read-only intent-to-candidate-action compilation path.
- Thin Emacs and VS Code clients proving that the protocol is not tied to
  either editor.
- Compatibility fixtures drawn from the current Emacs Arxana, TypeScript scope
  view and LaTeXML-Oxide WYSIWYG implementation.

### Out

- Recreating Emacs or VS Code editing, redisplay, mode or command semantics.
- Choosing CRDT, OT or single-writer revisioning before a live collaboration
  requirement distinguishes them.
- The complete REPL Circus, Giraffe scoring or War Machine policy.
- FUTON's institutional/admission rules; `M-ways-of-working` remains their
  owner and other users may supply different governance structures.
- Completing the Lean-to-LaTeX EmitPython pipeline.
- Autonomous mutation before the read-only compilation path is inspectable.

## Candidate technical slices (not yet an implementation schedule)

1. **Mirror.** Read representative existing Arxana entities and hyperedges
   through the JVM API without changing their identity or meaning.
2. **Resolve.** Run one cascaded scope strategy in the JVM and return its
   candidates, selected range, revision and evidence.
3. **Project.** Display and navigate the same resolved scope from Emacs and VS
   Code, with equality checked at the service response rather than by pixels.
4. **Correspond.** Connect LaTeX source to one LaTeXML-Oxide HTML
   manifestation; add Lean and generated Python when EmitPython supplies their
   correspondence material.
5. **Compile.** Turn a client intent into a non-executing candidate action with
   grounded targets, warrants, predicted effects and unresolved items.

## First demonstration

Emacs and VS Code open the same LaTeX artifact; VS Code also displays the
existing Oxide-rendered document. Selecting a rendered phrase resolves through
the JVM service to one Arxana entity and scope. Both clients navigate to the
same source manifestation and display the strategy cascade, artifact revision
and provenance used. Asking "what would changing this affect?" returns an
inspectable candidate action and correspondence neighbourhood but performs no
mutation.

This demonstration proves shared authority, strategy resolution,
correspondence and compilation without requiring a headless text editor or a
CRDT.

## Completion conditions

- [ ] Publish the core entity/hyperedge/manifestation/scope/action schema with
  schema-mirroring tests against representative existing XTDB records.
- [ ] Resolve a cascaded strategy in the JVM and prove that an unrelated edit
  before or after its enclosing anchor does not change the intended scope.
- [ ] Show Emacs and VS Code consuming the same entity and scope-resolution
  response for one live artifact revision.
- [ ] Navigate bidirectionally between a LaTeX scope and its Oxide-rendered HTML
  manifestation through JVM-owned correspondence.
- [ ] Compile one client intent into a read-only candidate action whose target,
  provenance, warrants and unresolved fields are all queryable.
- [ ] Record the transaction/concurrency contract independently of its
  implementation and identify the observed requirement that would trigger a
  CRDT decision.
- [ ] Document concrete interfaces that `M-repl-circus`,
  `M-ways-of-working`, Giraffe and War Machine can adopt without moving their
  domain logic into this mission.

## Dependencies and enabling relationships

This mission depends on the current Arxana model and XTDB data remaining
readable, plus a small representative fixture set. It does not depend on the
adjacent missions reaching completion.

It enables a later Campaign only after the projection and correspondence
slices are demonstrated. That Campaign may coordinate domain interfaces and
the broader "action becomes inference" demonstration without making those
missions subordinate to this technical work.

## IDENTIFY lifecycle receipt

- **Motivation:** the Gap section names the absent shared JVM authority and the
  present split across Emacs, XTDB and TypeScript/Oxide clients.
- **Theoretical anchoring:** recorded above; pattern selection remains open and
  may be revised by later evidence.
- **Scope in/out:** recorded above, including the explicit non-goals of editor
  reimplementation, premature concurrency choice and domain-policy ownership.
- **Completion criteria:** seven observable conditions are listed above.
- **Relationships:** dependencies and enabled missions are named without
  making them subordinate.
- **Source material:** concrete donor files and APIs are listed above.
- **Owner and dependencies:** Joe is operator; the technical driver remains an
  ordinary assignable want, not an operator gate.

**IDENTIFY exit: Met.** Joe agreed the plan on 2026-11-06 and then requested a
lifecycle audit and Zai packet planning. The gap is accepted; design hypotheses
remain intentionally unsettled for MAP.

## MAP

MAP surveys facts and makes no architecture decisions. Its required output is
a source-cited inventory, a `ready / missing` table, and concrete answers to
the following questions:

- **Q1 — Logical substrate:** Which entity, hyperedge, provenance, temporal and
  salience shapes are canonical in the live XTDB path, and which exist only in
  Emacs-side conventions?
- **Q2 — Strategies:** What strategy and finder forms exist, how are cascades
  represented and evaluated, and which depend intrinsically on Emacs state?
- **Q3 — Clients:** What protocol shapes and behaviours already exist across
  Emacs, the TypeScript scope view, Oxide WYSIWYG, Marimo, Element and any VS
  Code work?
- **Q4 — Correspondence:** Which stable identities survive the current
  LaTeX-to-HTML path, where are only offsets or quotes available, and what
  evidence can form a first cross-manifestation fixture?
- **Q5 — Consistency:** What concurrent editing actually occurs today, what
  failures have been observed, and which consistency property would require a
  CRDT rather than revision-checked transactions?
- **Q6 — Consumer boundary:** What minimum query and output shapes do the REPL
  Circus, Ways of Working, Giraffe and War Machine demonstrably need, without
  importing their policy into Arxana?

### Zai discovery packets

The first MAP wave is prepared under
`holes/labs/M-jvm-arxana/packets/`. Each packet is read-only discovery, owns a
separate report, and may run concurrently:

1. `P1-live-xtdb-schema.md` — Q1, live logical substrate.
2. `P2-scope-strategy-census.md` — Q2, strategy portability.
3. `P3-client-protocol-census.md` — Q3, existing client interfaces.
4. `P4-correspondence-fixture.md` — Q4, first manifestation fixture.
5. `P5-consistency-requirements.md` — Q5, evidence for or against CRDT.
6. `P6-consumer-interface-census.md` — Q6, adjacent mission requirements.

Dispatched 2026-11-06:

| Packet | Seat | Invoke job |
|---|---|---|
| P1 | `zai-2` | `invoke-1793982291192-13988-cb4d376e` |
| P2 | `zai-3` | `invoke-1793982290931-13985-15c7f953` |
| P3 | `zai-4` | `invoke-1793982291346-13990-a2f3d594` |
| P4 | `zai-5` | `invoke-1793982291108-13987-b49094d3` |
| P5 | `zai-6` | `invoke-1793982291011-13986-48c2c2b4` |
| P6 | `zai-7` | `invoke-1793982291274-13989-2a6a4145` |

Zai seats did not edit this mission, choose the architecture, or begin
implementation. The coordinator accepted and synthesized these reports:

| Packet | Report | Commit | Principal result |
|---|---|---|---|
| P1 | `P1-live-xtdb-schema-report.md` | `13cc65c` | The live authority is futon1b's entity and hyperedge tables; several Emacs fields and proposed records are not live contracts. |
| P2 | `P2-scope-strategy-census-report.md` | `679c5ab` | Finder declarations are stored data without an executor; the anchor re-finder is the only implemented cascade. |
| P3 | `P3-client-protocol-census-report.md` | `9daf23f` | Four useful but mutually unintelligible client protocols exist, and there is no VS Code client. |
| P4 | `P4-correspondence-fixture-report.md` | `f238e96` | The committed Oxide draft8 source/page/scope set is the strongest first correspondence fixture. |
| P5 | `P5-consistency-requirements-report.md` | `4fcbc9d` | No observed workflow presently requires a CRDT; observed failures are stale-revision or single-authority failures. |
| P6 | `P6-consumer-interface-census-report.md` | `6c36a17` | Four adjacent consumers already converge on ID-addressed reads and typed, ID-endpoint hyperedge writes. |

The report paths below are relative to `holes/labs/M-jvm-arxana/`.

### Answers to the MAP questions

**Q1 — Logical substrate.** The live canonical records are futon1b entity
documents (`id`, `name`, `type`, with optional `external-id`, `source` and
`props`) and hyperedge documents (`id`, `type`, non-empty endpoint IDs, with
optional ends, labels, props, confidence and valid time). Hyperedge valid time
is live. Entity as-of/version parameters sent by Emacs are not honoured by the
live service. `seen-count`, `pinned?` and `last-seen` are Emacs-side salience
conventions which the live writer drops; Plexus is prose, not a routed record.
The live store also contains an uncensused `mission-scope` hyperedge family
using `prop/*` attributes whose producer is not yet located. The Emacs client
still defaults to inactive port 7071 while the observed futon1b service is on
7072/7073; `/ego` and `/meta/model` are likewise not live there.

**Q2 — Strategies.** Five finder forms are persisted as JSON plists:
symbol-as-term, filename-mention, embedding-proximity, explicit and
surface-form-hybrid. Nothing dispatches on their types, so there is no
implemented inter-finder ordering, cascade or ambiguity policy. The actual
three-stage cascade is `arxana-links-find-anchor`: exact hint/hash, then
context/hash, then fuzzy before/after matching. Its algorithm and the stored
strategy, link, surface-form and scholium records are portable; buffer capture
and interaction are Emacs-specific. Existing tests show that the anchor
algorithm tolerates unrelated prefix and suffix edits, but `fuzzy-matched` is
not produced and finder-level ambiguity remains undefined.

**Q3 — Clients.** Four working protocols exist: the WYSIWYG WebSocket protocol
uses integer file/line/column positions, typed refusals and a journal; the
TypeScript scope view consumes static codepoint, half-open spans; Marimo agent
chat uses revision-counted SQLite routing and Agency HTTP streaming; Element
polls event-ID-keyed intent marks. They share only a weak vocabulary, with
collisions such as `file` meaning an index in one place and a filename in
another, and they use three unrelated freshness regimes. Existing assets
already demonstrate two-way WYSIWYG navigation, guarded edits, codepoint-span
rendering, agent routing and widget messaging. Missing are a common position
and error vocabulary, common turn identity, live scope transport and any VS
Code client.

**Q4 — Correspondence.** The first fixture should be futon5 commit `245773f`'s
Oxide draft8 set: `sandbox/*.tex`, `page/draft8.html` and
`sandbox/scopes.json`. It supplies a file-index map and 1,630 anchors and
exercises three distinct correspondences: a paragraph point anchor with no DOM
ID, a section range with build-local `S2`, and a mathematics range whose HTML
`alttext` expands `\\sig` to `\\sigma`. It therefore exposes quote-, range-
and transformation-based identity rather than pretending offsets are semantic
IDs. The fixture also reveals a revision defect: the recorded scope hashes
match `.pristine/` sources while the page anchors verify against the live
sandbox sources. Durable manifestation/entity IDs and stable DOM IDs do not
yet exist.

**Q5 — Consistency.** Seven present writers were inventoried, but no legitimate
concurrent multi-writer intent was observed. The two recorded failures, both on
2026-08-08, were an out-of-band test snapshot restore that destroyed browser
edits and an edit from a page roughly 100 minutes stale. These are
single-authority and freshness failures already addressed in the WYSIWYG path
by content-hash checks, typed refusal and journalling. The XTDB API has neither
revision CAS nor a subscription stream, so revision-checked transactions still
need design work; that does not establish a CRDT requirement. The falsifying
observation is now explicit: require a CRDT when two clients make legitimate
concurrent or offline edits that both must survive and rejecting the stale
second edit would destroy unrecoverable work.

**Q6 — Consumer boundary.** REPL Circus, Ways of Working, Giraffe and War
Machine all already witness the same narrow waist: ID-addressed node/edge reads
and batched writes of typed hyperedges whose endpoints are IDs. Their scores,
probe verdicts, want locators and ordering evidence remain domain-namespaced.
Act-attached references, evidence-keyed derived records, owner-linked findings
and ordering edges have matching shapes under different names, but the survey
does not justify standardizing those names yet. Live 驴/狗 metadata,
cross-mission dependency/order edges and per-click selector-reading records
remain consumer-side gaps rather than reasons to put policy in Arxana.

### Ready / missing inventory

| Area | Ready | Missing or unverified |
|---|---|---|
| Durable graph | Live ID-addressed entity/hyperedge reads and typed batch writes; hyperedge valid time | Entity revision CAS/as-of semantics; subscriptions; live Plexus and salience contracts; producer and census support for `mission-scope` edges |
| Location recovery | Portable stored link/strategy forms; tested three-stage anchor recovery | Finder executor, ordering, cascade composition and ambiguity semantics |
| Client surfaces | WYSIWYG navigation/edit refusals/journal; TypeScript span rendering; Marimo routing; Element widget messaging | Shared protocol, position/error/turn vocabulary, live scope transport and VS Code client |
| Manifestations | Committed Lean/LaTeX/HTML-adjacent Oxide fixture with source map and three correspondence classes | Correct revision identity, semantic manifestation IDs, durable DOM identities and generated-Python membership in the fixture |
| Consistency | Content-hash freshness checks, explicit stale refusal and edit journal cover observed failures | Transactional graph revisions and recovery semantics; evidence for CRDT remains absent |
| Adjacent consumers | Empirically shared node/edge query and hyperedge-write waist | Domain records named and governed by their own missions; no cross-domain naming standard is yet warranted |

### Surprises constraining DERIVE

- The live authority is narrower than the Emacs client and design prose imply;
  DERIVE must specify against observed futon1b behaviour, not silently promote
  client fields into canonical storage.
- The strategy layer is missing execution rather than trapped inside Emacs.
  Porting declarations alone would reproduce the present gap.
- The existing clients are exemplars of required behaviours, not variants of
  one latent protocol.
- The strongest correspondence fixture already contains a revision-identity
  contradiction, so revision identity must be part of the model rather than a
  later transport detail.
- CRDT is an admissible future implementation but is not a current
  requirement. DERIVE should preserve the falsifying trigger above.
- The proposed consumer narrow waist is supported by four independent
  consumers, while their inference products are demonstrably domain-specific.

**MAP exit: Met.** All six questions have source-cited answers, the existing
infrastructure and data are inventoried, and the ready/missing table identifies
the exact gaps DERIVE must turn into competing architectures and acceptance
criteria. No architecture or implementation was selected during MAP.

## DERIVE

DERIVE turns the MAP evidence into an implementable design. The earlier
Mirror/Resolve/Project/Correspond/Compile sketch remains useful, but the survey
changes what each slice must prove and adds revision identity as a cross-cutting
concern. This phase produces specifications and tripwire fixtures, not the
production implementation.

### MAP-driven corrections to the earlier sketch

| Earlier slice | Evidence-informed derivation task |
|---|---|
| Mirror | Define a compatibility adapter over the observed futon1b entity/hyperedge records. Do not treat dropped Emacs salience fields, Plexus prose or entity as-of parameters as already canonical. |
| Resolve | Specify and execute the tested three-stage anchor algorithm first; separately define dispatch, ordering, ambiguity and evidence semantics for the five currently inert finder declarations. |
| Project | Design a common location, revision, result and error vocabulary before either client. Emacs supplies donor behaviour; VS Code is a new thin client, not an existing port target. |
| Correspond | Make the Oxide draft8 set the first normative fixture and repair its source-revision contradiction before adding Lean or generated Python. |
| Compile | Emit only the empirically shared ID-addressed graph/action waist. Keep scores, verdicts, wants and ordering evidence in consumer namespaces. |
| Cross-cutting consistency | Carry an artifact revision through resolution, correspondence, edits and receipts. Specify stale refusal and recovery now; introduce CRDT semantics only if the Q5 falsifying observation occurs. |

### Architecture alternatives to derive and compare

DERIVE must draw and evaluate at least these three alternatives against the
same scenarios. Naming an option here does not select it.

1. **Extended futon1b authority.** Add Arxana schema, resolution,
   correspondence and subscription routes directly to the existing graph
   service. This minimizes service boundaries but couples artifact evaluation
   and client protocol evolution to the store.
2. **Headless Arxana service over futon1b.** A separate JVM service owns the
   portable model, strategy executor, correspondence and client protocol while
   using futon1b through a narrow storage adapter. This creates another service
   boundary but keeps graph persistence distinct from artifact computation.
3. **Graph authority with client-hosted evaluators.** Futon1b remains the graph
   authority while Emacs and VS Code evaluate strategies and submit evidence.
   This reuses editor facilities, but must demonstrate how two clients can
   receive the same authoritative answer rather than merely similar local
   answers.

The comparison matrix must score each option for one-authority-per-question,
compatibility with live records, deterministic strategy results, artifact and
revision access, subscription semantics, deployability, failure isolation,
testability, the cost of adding a second editor, and the concrete mechanism
that enforces revision preconditions across read/check/write. An architecture
that merely specifies revision tokens but cannot atomically refuse a stale
write fails the matrix, even though a CRDT is not currently required. Any
hybrid must identify
which option owns each question rather than obscuring ownership behind a new
component boundary.

### Required derivation work products

#### D1 — Canonical model and storage boundary

Design artifact: `holes/labs/M-jvm-arxana/D1-canonical-model.md`.

Specify each entity as source, ingested, derived or authored, its durable
identity and its revision behaviour. The minimum candidate set is graph entity,
hyperedge, artifact, artifact revision, manifestation, scope strategy, scope
resolution, correspondence assertion, intent, candidate action, admission
decision and action receipt. For each, decide whether it is a durable futon1b
record, an immutable derived result, or a protocol-only value.

Specify relation types and arity rather than representing every connection as
an undifferentiated hyperedge. At minimum cover entity-to-manifestation,
artifact-to-revision, strategy-to-finder, resolution-to-revision,
correspondence-across-manifestations, evidence-for-resolution and
intent-to-candidate-action. Record provenance and temporal standing wherever a
derived assertion can become stale.

Deliverables: typed schema, example records mapped to the P1 live census,
migration/compatibility rules for both observed hyperedge families, and a
storage-adapter interface whose tests distinguish live behaviour from proposed
extensions. **Draft complete:** the artifact fixes the shared vocabulary for
D2–D6 and leaves four bounded choices for D7 synthesis.

#### D2 — Strategy evaluator and resolution semantics

Design artifact: `holes/labs/M-jvm-arxana/D2-strategy-evaluator.md`.

Define the finder interface, ordered cascade, short-circuit and accumulation
rules, ambiguity thresholds, revision checks and evidence returned at every
stage. The existing exact/context/fuzzy anchor algorithm is the first reference
implementation and tripwire; the five stored finder forms must each be marked
executable, translated, advisory-only or unsupported rather than presumed to
work.

The result algebra must distinguish at least `resolved`, `ambiguous`,
`missing`, `stale` and `unsupported`, carrying candidates, strategy/finder IDs,
artifact revision, evidence and diagnostics. Results must not turn resolved
offsets into durable identity.

Deliverables: evaluator interface and pseudocode, deterministic ordering rules,
fixture cases for unrelated prefix/suffix edits and genuine ambiguity, and the
authority decision for artifact text access. **Draft complete:** the artifact
defines explicit translation from inert legacy declarations, the result and
evidence algebra, ambiguity-preserving anchor semantics and an immutable
`ArtifactProvider` boundary; D7 retains only the provider's deployment choice.
The progressive ARGUE pass (`6dcdb54`) found the design coherent and added the
requirement that accepting an ambiguous/unverified result be a distinct,
attributable and policy-versioned durable record.

#### D3 — Shared protocol and consistency contract

Design artifact: `holes/labs/M-jvm-arxana/D3-protocol-consistency.md`.

Define versioned request/response envelopes for entity lookup, graph query,
scope resolution, correspondence neighbourhood, subscription and read-only
candidate-action compilation. Define one position vocabulary with explicit
units, indexing base, half-open/closed convention and file identity; do not
reuse the overloaded current `file`, `text` or `agent` fields without qualified
meaning.

Every artifact-dependent message carries a revision identity. Mutation-shaped
requests state an expected revision and receive applied, stale, conflict,
invalid or unsupported outcomes with recovery information. Subscriptions state
ordering, replay/resume, deduplication and resynchronization semantics. The
contract must remain compatible with a later CRDT implementation without
claiming CRDT operations today.

The design must supply an enforcement mechanism, not just an envelope. Because
P1/P5 and D1 establish that futon1b has neither revision CAS nor subscriptions,
D3 must compare at least: a store-side conditional transaction added to
futon1b, a single-writer sequencer in a headless authority with durable
operation log, and the equivalence burden of client-hosted evaluators. Replay
both observed P5 incidents and one interleaving in which two requests name the
same expected revision. A check followed by an unguarded write is not CAS and
does not satisfy the contract.

Deliverables: protocol schemas, error algebra, consistency state machine,
subscription sequence examples, an enforceable revision-precondition design,
and replay tests derived from the two P5 incidents plus the concurrent
interleaving. **Draft complete:** two conforming enforcement mechanisms are
specified for D7 comparison; client-hosted writes are non-conforming.
The progressive ARGUE pass (`98188fb`) confirmed the derivation and made
durable fencing a D7 selection gate; it also requires D7 to decide whether
client-attested evaluator evidence can ever be promoted without authority-side
replay.

#### D4 — Correspondence model and normative fixture

Design artifact: `holes/labs/M-jvm-arxana/D4-correspondence-model.md`.

The current Oxide draft8 source/page/scope set is a contradiction-bearing test
input, not yet a normative fixture: its recorded scope hashes and page anchors
name different source states. Adopt it as the first fixture only after giving
all members one coherent revision identity and adding a tripwire that rejects
the unrepaired set. Model correspondence as a
provenanced assertion between manifestations, capable of representing exact
range, quote/context recovery, build-local DOM anchors and transformed content
such as `\\sig` to `\\sigma`. State when a mapping is directional, partial,
ambiguous or stale.

Deliverables: repaired fixture manifest, stable fixture IDs, expected query
answers for the paragraph/section/mathematics cases, and an extension slot for
Lean and generated Python that does not block the first demonstration.
**Design draft complete:** the original set is a required negative fixture;
the positive fixture is admitted only after deterministic rejection/repair.
The progressive ARGUE pass (`d802654`) added deterministic, authority-namespaced
manifestation minting and made the repaired positive fixture the first required
implementation increment because D5/D6 demonstrations depend on it.

#### D5 — Client projections and fidelity contract

Design artifact: `holes/labs/M-jvm-arxana/D5-client-fidelity.md`.

Specify the smallest Emacs and VS Code surfaces that prove shared authority:
open/select a manifestation, request resolution, display candidates/evidence,
navigate to a returned location, show revision/staleness and inspect a
correspondence neighbourhood. The Oxide document pane is a protocol consumer,
not a privileged source of identity.

Create the required Capability Preservation Matrix from the P2 and P3 donor
censuses. Classify every current behaviour as preserve, adapt or drop, with a
tripwire for each preserved behaviour. It must include anchor rediscovery,
two-way WYSIWYG navigation, typed edit refusal, journalling, codepoint spans,
agent routing and Element messaging, even where the decision is explicitly to
leave a capability outside this mission.

Deliverables: command/view specification for both editors, protocol transcripts
showing identical authority responses, Capability Preservation Matrix and
client-boundary tests. **Design draft complete:** all P2/P3 donor capabilities
have explicit preserve/adapt/drop dispositions and tripwires.
The progressive ARGUE pass (`04ea998`) confirmed those dispositions and fixed
the canonical semantic-response projection used to prove cross-client equality;
request identity, timing, diagnostics and presentation are excluded.

#### D6 — Inference-ready action boundary

Design artifact: `holes/labs/M-jvm-arxana/D6-action-boundary.md`.

Specify a read-only compiler from an intent plus grounded entity/scope inputs
to a candidate action. Its common fields are target IDs, typed operation,
preconditions, predicted effects, provenance/warrants, risks and unresolved
questions. Admission policy, scores, probe verdicts and domain-specific
ordering stay outside the service or in namespaced extension fields.

Deliverables: one end-to-end example using the correspondence fixture, a
queryable receipt shape, and interface notes showing how the four surveyed
consumers attach their own records without changing the common schema.
**Design draft complete:** compilation is deterministic and read-only;
interpretation, admission, execution and receipts remain distinct authorities.
The progressive ARGUE pass (`dbeb91c`) required durable provenance for the
act-to-intent interpretation; unprovenanced/client-attested intent can produce
only a conditional candidate, never a fully compiled one.

#### D7 — Architecture synthesis

Design artifact: `holes/labs/M-jvm-arxana/D7-architecture-synthesis.md`.

Combine D1–D6 into a component and data-flow specification. Draw an exotype or
equivalent wiring diagram showing authority boundaries, ports, durable versus
derived state, revision flow, subscriptions and client projections. Complete
the architecture comparison matrix and select one option with recorded
trade-offs.

Deliverables: selected architecture, wiring diagram, consolidated invariants,
ordered implementation increments, acceptance tests and rollback/compatibility
plan. **Draft complete:** selects headless JVM Arxana over futon1b with atomic
conditional commit remaining at the store boundary. The D7 ARGUE checkpoint
(`37ba0dc`) upheld the selection and required futon1b to reserve `arxana/*`
writes for the authenticated conditional route so uniqueness cannot be bypassed.

### Invariants to make checkable

- Given the same entity/scope question and artifact revision, Emacs and VS Code
  receive the same authority result from exactly one authoritative locus,
  independent of editor-local markers; violations enumerate the conflicting
  `(question, artifact-revision, authority)` records.
- A scope resolution names the exact artifact revision and strategy evidence
  from which it was derived; it is never silently reused against another
  revision.
- Durable identity does not depend solely on a byte, codepoint, line/column or
  DOM position.
- Every derived correspondence or candidate action is traceable to durable
  inputs and may be declared stale without destroying its provenance.
- A stale mutation-shaped request cannot overwrite a newer revision without an
  atomically enforced consistency decision and recoverable evidence.
- Domain-specific consumer fields cannot change the meaning of the common
  entity, endpoint, provenance or revision fields.
- Unsupported legacy declarations fail explicitly rather than appearing to
  have resolved.

Each invariant must acquire an executable assertion, query or fixture test in
the D7 acceptance plan.

### Decision-record discipline

Every non-obvious choice in D1–D7 must use the lifecycle form:
`IF ...; HOWEVER ...; THEN ...; BECAUSE ...`. The first required records are:

- the service boundary and owner of each authoritative question;
- durable record versus derived result for resolutions and correspondences;
- finder ordering and ambiguity semantics;
- revision identity and stale-recovery contract;
- subscription ordering/replay semantics;
- which donor capabilities are preserved, adapted or dropped;
- the boundary between common candidate-action fields and consumer policy;
- the observation that would reopen the CRDT decision.

### Order of work

1. Establish D1's vocabulary and live-storage compatibility constraints; these
   are inputs to every other work product.
2. Draft the three architecture diagrams far enough to assign authority and
   artifact-text access, without selecting one prematurely.
3. Develop D2, D3 and D4 against the same draft8 fixture and revision identity;
   discrepancies feed back into D1.
4. Derive D5 from the shared protocol and complete the donor fidelity matrix.
5. Derive D6 only after the graph and resolution waist is stable.
6. Perform D7 synthesis, choose an architecture, and turn the selected design
   into implementation increments and acceptance tests.

Work products D2–D5 may be researched concurrently after D1 fixes their shared
terms. D7 is intentionally a synthesis task and must not be delegated as four
independent local choices.

### DERIVE exit

**DERIVE exit: Met.** D1–D7 specify the schemas, alternatives, selected
architecture, authority assignments, decision records, fidelity matrix, wiring,
implementation increments and acceptance tests. The progressive ARGUE passes
have dispositioned every material challenge; ARGUE remains open for synthesis
and the planned client-side critique.

### Concurrent ARGUE ownership

By Joe's decision on 2026-11-06, Zai-1 owns an ARGUE shadow while Codex-21
completes DERIVE. Its packet is
`holes/labs/M-jvm-arxana/packets/A1-design-fitness-argument.md`. The shadow
constructs and challenges the argument for JVM Arxana's evolving design and
fitness for purpose; it is not a close analysis of an operator passage.
ARGUE may expose obligations that revise DERIVE, but cannot meet its lifecycle
exit until DERIVE has selected and specified the architecture it is arguing
for.

First-pass argument artifact:
`holes/labs/M-jvm-arxana/A1-design-fitness-argument.md` (commit `65c6eac`). Its
obligation ledger is a live input to DERIVE. In particular, O-D3a makes
revision-precondition enforcement a D7 architecture-selection criterion, and
O-D4a treats the unrepaired draft8 data as a defeater rather than evidence for
the design.

The sequential coordination protocol is
`holes/labs/M-jvm-arxana/ARGUE-DERIVE-loop.md`. Each D1–D7 checkpoint produces
one incremental Zai-1 pass and one typed DERIVE disposition receipt; the
checkpoint table prevents challenges from disappearing between passes.

Client-side ARGUE review after DERIVE adds a bounded proof-presentation profile
to D7: proof moves are domain entities with multiple manifestations;
correspondence assessments are attributable records distinct from resolution
acceptance and action admission; authored/generated provenance remains
separate; and usefulness evaluation is consumer evidence rather than JVM
authority conformance. This clarification does not reopen the selected
architecture or introduce an APM-specific alignment mechanism.

## ARGUE

The canonical synthesis is
`holes/labs/M-jvm-arxana/A2-final-design-fitness-argument.md`. The seven-pass
`A1-design-fitness-argument.md` remains the objection/disposition provenance,
not the final statement.

The principal claim is that headless JVM Arxana over futon1b is fit because it
places semantic questions behind one revision-pinned, evidence-producing
authority while leaving atomic durable mutation at the store boundary. The
alternative designs fail different observed constraints: extending futon1b
with all semantics couples an unsettled protocol to storage, while client-side
evaluation recreates multiple authorities. The selected hybrid names one owner
for every question and reserves `arxana/*` writes so those assignments are
enforceable rather than advisory.

The synthesis includes the lifecycle-required pattern cross-reference,
theoretical coherence and war-room rationale, trade-off summary,
generalization notes and jargon-free argument. It incorporates the
proof-presentation client profile and treats code-server as another client and
deployment shell: useful co-location does not grant it identity, resolution or
write authority.

**ARGUE exit: Met.** The strongest objections have typed dispositions and
materially changed the design. The remaining items are explicit VERIFY and
INSTANTIATE evidence obligations, not unresolved architecture choices. The
fitness argument is understandable in plain language and checkable against
MAP reports, D1–D7 and their tripwires.

## VERIFY

VERIFY uses the two independent client perspectives as its primary contract
gauntlet, supplemented by structural closure and two risk spikes that clients
cannot establish:

1. V1 code-server contract verification (`packets/V1-codeserver-contract-verification.md`).
2. V2 proof-presentation contract verification
   (`packets/V2-proof-presentation-contract-verification.md`).
3. Coordinator-owned specification BOM and wiring/authority closure audit.
4. Atomic expected-head/reserved-namespace spike and D4 fixture-coherence
   rejection/rebuild spike.
5. Completion/fidelity pre-check and decision receipt.

A full theorem or general logic model is a priced decline: the mission needs
typed contract traces, structural closure and targeted empirical evidence, not
a post-hoc formalization of every domain. VERIFY exits only if both clients
trace without contradiction, every wiring input/output has one owner, the two
spikes address the unexercised risks, and all remaining work is properly
INSTANTIATE rather than unresolved DERIVE.

**VERIFY exit: Not met.** Client packets are prepared; reports, structural
audit, spikes and synthesis remain open.
