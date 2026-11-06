# M-jvm-arxana — one Arxana authority across Emacs and VS Code

**Date:** 2026-11-06  
**Status:** OPEN — MAP (IDENTIFY accepted by Joe 2026-11-06)
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

The coordinator synthesizes the six reports into the lifecycle-required
`ready / missing` table. Zai seats do not edit this mission, choose the
architecture, or begin implementation.

**MAP exit: Not started.** Packets are prepared; no survey report has yet been
accepted.
