# M-formal-arxana — Arxana as a recursive hypergraph editor and geometry markup language

**Date:** 2026-11-07  
**Status:** OPEN — IDENTIFY  
**Owner:** Joe (operator); next technical driver to be assigned  
**Repos:** `futon4` (mission, legacy Arxana, design sources), `futon3c`
(JVM authority, 象 and REPL Circus), `futon1`/XTDB (durable hypergraph),
`futon4a` and `futon6` (Emacs and VS Code/code-server projections), `futon5`
(mathematical and rendered manifestations)  
**Related:** `M-jvm-arxana`, `M-self-representing-stack`, `M-象-2000`,
`M-repl-circus`, `M-ways-of-working`, `M-latex-wysiwyg`

## HEAD

### Operator-voice anchor

Joe, 2026-11-07:

> What Arxana is, actually, is a hypergraph editor. Projections into 1D or 2D
> surfaces are convenient for humans, and surely for LLMs as well. The
> underlying situation is that Arxana is a “typesetting system” in which
> everything is composed: not of “the same” letters, but of meaningful
> contents.

Joe connected this to Ted Nelson's transclusion, generalized to **'clusion**:
transclusion is an identity-preserving special case, while 'clusions gather,
transform, relate, and compose arbitrary meaningful contents. Because a
'clusion is itself content, it may participate recursively in further
'clusions.

The final step in the discussion moved from unresolved scalar answers to
geometry. For the square map on the complex unit circle, the fibre over `-1`
is `{i, -i}`: the alternatives are not collapsed into equality but become an
intelligible geometric configuration. Joe's completion criterion was:

> Arxana becomes a markup language for geometry.

### Why this mission, now

Joe, 2026-11-07 (to claude-13), marking this mission high priority:

> What we are trying to do is get the REPL to build a Giant Connected
> Component or GCC which will compile intents into inference-ready actions.
> This is essential to prove out the Apex thesis from Cascade Live. We seem to
> be converging on a working implementation of AIF for software (the War
> Machine), but in order to work coherently with operator intents, we need a
> model of those. Giraffe is a joined up view of that, but Giraffe is
> basically only as good as the rest of the REPL Circus. Lastly, in terms of
> Cascade Live, the primary missing gap is the ethical layer, the cui bono,
> which I have conceptualised as a "fall-through argument" structure. If we
> get it right, Arxana can help hold all of that together; there are further
> applications and demonstrations in mathematics along the way.

The chain of dependencies this sets out:

1. **Apex thesis.** Cascade Live asks whether a community of humans and
   machines can "leave an account of itself structured enough that the work
   can steer itself — so that activity becomes inference rather than motion"
   (`futon2/holes/DESIGN-REQUIREMENTS-apex-2026-09-09.md`). Proving it needs
   operator intent to reach inference as actions, not as prose.
2. **War Machine.** The WM is the AIF-for-software side: it selects and acts
   under a generative model. It can act coherently on what the operator wants
   only if it has a model of operator intents to condition on.
3. **REPL Circus GCC.** The intent model is built from turns, replies,
   missions, patterns and evidence joined into one connected component, and
   compiled into candidate actions the WM can take up. That compilation step
   is what is missing today (Q3 below).
4. **Giraffe.** Giraffe is the joined-up view of that model, so its quality
   is bounded by the Circus records under it (`futon3c/holes/labs/
   giraffe-observation-series/`).
5. **Cui bono.** The main gap remaining in Cascade Live is the ethical layer:
   who the work benefits, stated as a fall-through argument in which each
   claim records what still stands, and where the argument goes next, when it
   fails. That argument is itself a recursive structure of claims, supports
   and defeaters, and needs the same attributable, assessable, non-collapsing
   relations as the rest.

Arxana's part is to be the one graph contract that intents (F1), the GCC
action view (F1), assessable compositions (F4) and argument structure (F6)
all live in, so that these five pieces connect instead of each keeping a
private representation. The mathematical demonstrations (F3, F5) test the
same contract on material where correctness can be checked.

### What is already felt to be true

- The existing Emacs Arxana demonstrates identity through relocatable links,
  rather than identity through fixed buffer positions.
- JVM Arxana now supplies artifact revisions, strategy resolution,
  correspondences, typed evidence, projections, and action compilation.
- 象 already interprets operator and agent turns; the reply proforma's marks
  (`㊯`, `㊢`, and others) provide real, small semantic manifestations.
- REPL Circus already treats turns, replies, missions, patterns, and evidence
  as a prospective Giant Connected Component, but does not yet compile them
  into a complete GCC action surface.
- `M-self-representing-stack` already states the crucial prior definition:
  hyperedges are frozen dynamics, and 'clusion gathers from multiple sources,
  transforms, and composes.
- One stable JVM scope strategy has been proved across seven edited artifact
  revisions, including movement, internal edit, copy ambiguity, and deletion.

### Anti-glibness discipline

This mission must not succeed by redescribing an ordinary labelled graph in
geometric language. Every claimed abstraction must be tested by a progressive
example in which:

1. graph records and their authorities are explicit;
2. at least two projections address the same records;
3. edits distinguish semantic content from coordinates and presentation;
4. ambiguity or contradiction survives without last-writer erasure; and
5. the claimed composition can be queried or executed under a declared
   capability, rather than existing only as prose.

### Working-economy position

The mission supplies the semantic foundation shared by JVM Arxana, 象,
REPL Circus/GCC, mathematical code-to-document correspondence, and Tufte-style
argument layouts. Those systems provide its fixtures, clients, authority, and
evidence. The mission does not replace their domain logic; it states and proves
the common recursive hypergraph contract they can use.

### Carried-forward tensions

- Whether a 'clusion is best formalized as a typed hyperedge, a morphism, a
  process, a relation, or a family admitting all four views.
- Which parts of 'clusion dynamics may be executable, and how capability and
  D6 admission prevent stored structure from becoming arbitrary evaluation.
- Whether collaborative text needs a CRDT/OT adapter, and which graph record
  types need different convergence policies.
- How much mathematical structure the geometry layer commits to: elementary
  incidence and maps first, with categorical/topological generalization earned
  by examples rather than presumed.
- How intent interpretation becomes attributable semantic structure without
  promoting a glyph or model inference directly into authority.

### Provenance

Compiled from Joe's 2026-11-07 Emacs-REPL design conversation with Codex-21,
following the failed static APM demonstration and the Edit Robustness
correction. Immediate design records are:

- `holes/labs/M-jvm-arxana/EDIT-ROBUSTNESS-CORRECTION.md`
- `holes/labs/M-jvm-arxana/EDIT-ROBUSTNESS-SPEC.md`
- `holes/labs/M-jvm-arxana/HYPERGRAPH-EDITOR-MODEL.md`

## 1. IDENTIFY

### Gap

The stack has a working graph store, a JVM Arxana protocol, relocatable scopes,
correspondence assertions, intent analysis, and several projections. It does
not yet have one explicit and proved account of the object these facilities
edit.

Without that account:

- text revision integrity can be mistaken for semantic identity;
- CRDT convergence can be mistaken for semantic agreement;
- projection coordinates can be mistaken for graph content;
- hyperedges can decay into endpoint lists with decorative properties;
- interpreted intents can be mistaken for authoritative acts;
- Lean, LaTeX, diagrams, and prose can be forced into an undefined “same”; and
- geometry remains a visual metaphor rather than a queryable composition.

### Candidate principal invariant

Arxana edits a recursive typed hypergraph whose first-class relations are
**'clusions**: identified, attributable, composable relations or processes with
named input/output roles, topology, provenance, evidence, standing, and a
declared convergence policy. Text, code, mathematics, conversations, diagrams,
and layouts are projections of that graph. Projection coordinates are never
semantic identity unless a declared, evidenced relation makes them content.

### Candidate formal vocabulary

These are hypotheses for MAP and DERIVE, not yet a frozen schema:

- **Content** — a typed semantic object that may be manifested or related.
- **Manifestation** — content as it appears in a revision of an artifact.
- **'Clusion** — a first-class recursive hyperedge with named roles and
  declared dynamics; transclusion is its identity-preserving special case.
- **Projection** — an attributable selection and arrangement of graph content
  on a human- or machine-facing surface.
- **Chart** — a projection supplying coordinates without owning identity.
- **Edit proposal** — a typed request to change manifestation, content,
  relation, projection rule, or presentation state.
- **Fibre/solution set** — all admissible inputs or outputs associated with a
  map and value, including empty, singleton, multiple, or parameterized cases.
- **Standing** — verified, partial, ambiguous, contradictory, unsupported,
  resolved under a named theory/policy, or another declared qualification.
- **Resolution** — join, attributable choice, domain/theory extension, or
  explicit productive non-resolution.

### Shape-first instances

The candidate family is `projection-edit/<instance>`:

- `projection-edit/conversation-intent`
- `projection-edit/source-ast-definition`
- `projection-edit/collaborative-document`
- `projection-edit/cross-manifestation-proof`
- `projection-edit/geometric-fibre`
- `projection-edit/tufte-argument-layout`

The shared shape is: graph content → attributable projection → grounded edit
gesture → typed proposal → authority decision → graph change/refusal →
reprojection.

### Scope in

- A minimal formal schema for contents, recursive 'clusions, roles, topology,
  provenance, standing, convergence policy, and projection provenance.
- A typed edit-proposal envelope distinguishing five strata: manifestation,
  content, relation, projection, and presentation state.
- Operational convergence separated from structural normalization, semantic
  resolution, and productive non-resolution.
- Intent-marked conversation as the first end-to-end proof, joining 象 and
  REPL Circus into a bounded GCC projection.
- Edit Robustness for sequential and collaborative projections.
- Cross-manifestation semantic conflict across Lean, LaTeX, generated formal
  rendering, and generated informalisation.
- A minimal geometry markup vocabulary for objects, maps, fibres, invariants,
  symmetries, charts, and assessments.
- A unit-circle fixture proving geometry independently of a particular visual
  layout.

### Scope out

- A universal ontology of meaning or an unqualified `same-as` relation.
- Arbitrary evaluation of stored code or hyperedge properties.
- A complete theorem prover, computer algebra system, or graphics engine.
- Pixel-perfect layout as semantic authority.
- Selecting a CRDT implementation before the collaborative acceptance case
  demonstrates that one is required.
- Automatically resolving semantic contradiction by overwrite or ranking.
- Comprehensive ports of every legacy Arxana client.

### Progressive proof and demonstration series

Each example must leave a reusable fixture and acceptance record. Later steps
may use earlier contracts but may not silently strengthen their standings.

#### F0 — Edit Robustness substrate (existing proof)

One scope and one strategy survive seven immutable text revisions. Insertions,
movement, internal edit, copying, deletion, and historical replay have typed
outcomes. This proves the resolver substrate, not the editor-preview bridge.

Evidence: futon3c commit `a483b2e0f`; futon4 commits `1704491` and `8ed9567`.

#### F1 — Intent-marked conversation

Use real proforma turns containing `㊯`, `㊢`, and at least one ambiguous or
incorrect mark. Materialize the source turn, interpretation 'clusion, target,
evidence, and standing. Project the same records as:

1. marked linear conversation;
2. an outline or graph view; and
3. a bounded GCC candidate-action view.

Editing prose preserves the act identity where warranted; changing a mark
proposes a new interpretation rather than rewriting history; ambiguity remains
visible. This continues M-象-2000 and M-repl-circus and is the first required
new demonstration.

#### F2 — Collaborative same-artifact editing

Emacs and VS Code/code-server edit one shared document while preserving shared
scope identities and semantic markup. Concurrent non-conflicting edits,
same-range conflict, copy ambiguity, offline replay, and promotion are tested.
A CRDT/OT substrate is warranted only if two legitimate histories must survive
without an accepted sequencer; semantic conflicts remain graph content even
after replicas converge operationally.

#### F3 — Cross-manifestation mathematical meaning

Use a small Mark8-informed APM proof with authored informal prose, readable
Lean, generated formal rendering, and generated informalisation. Demonstrate
verified relocation, partial formalisation, ambiguous alignment, and an
explicit contradiction between authored and generated meaning. No artifact is
merged into another; competing assertions and assessments remain attributable.

#### F4 — Recursive 'clusion composition

Compose interpretation, grounding, correspondence, compilation, and projection
'clusions. Demonstrate that a 'clusion can itself be assessed or contradicted,
and that criticism of one stage does not rewrite its source or unrelated
stages. Executable dynamics remain behind capability and admission boundaries.

#### F5 — Unit-circle geometry markup

Represent \(S^1\), multiplication, conjugation/inversion, and the square map
`z ↦ z²`. Query the fibre over `-1` as `{i, -i}`. Render the same records in at
least two visibly different charts/projections. A semantic change propagates to
both; a coordinate-only change remains local. The graph, not either drawing,
is the authority for objects, maps, fibres, and invariants.

#### F6 — Argument layout

Project a proof or argument into a Tufte-style page whose main claims,
definitions, diagrams, sidenotes, and support relations are graph contents.
Move a diagram in the layout without changing its content identity; separately
change what it supports and require an attributable semantic relation edit.

### Completion criteria

- [ ] MAP answers Q1–Q10 below with cited files, schemas, probes, and fixtures.
- [ ] DERIVE specifies the minimal 'clusion and projection-edit contracts well
  enough to implement without inventing authority decisions during coding.
- [ ] F1 passes end to end on retained real conversation fixtures and produces
  the same record identities in linear, graph, and GCC projections.
- [ ] Editing an F1 manifestation exercises relocation and interpretation
  history instead of refusing solely because bytes changed.
- [ ] Operational convergence is demonstrably distinct from semantic
  agreement; a converged store retains a contradiction fixture.
- [ ] F2 either passes with a declared collaborative substrate or records a
  falsifying observation showing why the single-sequencer model suffices.
- [ ] F3 preserves distinct provenance and typed conflict across at least
  authored informal, Lean, and generated informal manifestations.
- [ ] A 'clusion is recursively addressable and assessable in F4.
- [ ] F5 answers queries for the square map, its invariants, and the two-point
  fibre over `-1` independently of rendering.
- [ ] Two F5 projections preserve graph identity, and semantic versus
  coordinate-only edits have observably different consequences.
- [ ] F6 demonstrates layout as a projection of argument structure rather than
  pixel position as identity.
- [ ] ARGUE explains why this is more than RDF-style annotation and why
  non-resolution is a legitimate result.
- [ ] VERIFY supplies a specification BOM, falsifiers, and retained traces for
  each formal layer used.
- [ ] DOCUMENT gives a new contributor one command/path for each progressive
  demonstration and states what each proves and does not prove.

### Relationships and dependencies

| Mission/system | Relationship |
|---|---|
| M-jvm-arxana | Supplies the implemented authority, artifact, resolve, correspondence, projection, and compile waist |
| M-self-representing-stack | Supplies the prior “hyperedges as frozen dynamics” and 'clusion account |
| M-象-2000 | Supplies intent/move vocabulary, turn evidence, and the pending feature-completeness demo |
| M-repl-circus | Supplies the turn/reply/mission/pattern GCC and its missing final action surface |
| M-ways-of-working | Supplies external admission/governance; remains outside Arxana semantics |
| M-latex-wysiwyg | Supplies guarded edit and rendered-manifestation experience |
| futon1/XTDB | Durable graph authority and temporal reconstruction |
| futon3c Arxana | Headless service and initial executable contracts |
| futon4a/futon6 | Emacs and VS Code/code-server projection clients |
| futon5 / Mark8 / EmitPython | Mathematical manifestations and later fixtures |

### Source material

- `holes/labs/M-jvm-arxana/HYPERGRAPH-EDITOR-MODEL.md` — compiled conceptual
  contract; commits `8100571`, `af657cd`, `234beff`.
- `holes/labs/M-jvm-arxana/EDIT-ROBUSTNESS-SPEC.md` — sequential,
  collaborative, and cross-manifestation edit contract.
- `holes/missions/M-self-representing-stack.md` §Hyperedges as Frozen Dynamics.
- `holes/missions/M-jvm-arxana.md` and its D1–D7, VERIFY, and INSTANTIATE
  records under `holes/labs/M-jvm-arxana/`.
- `futon3c/holes/missions/M-象-2000.md` and
  `futon3c/holes/excursions/E-elephant-2000-feature-completeness-demo.md`.
- `futon3c/holes/missions/M-repl-circus.md`.
- `futon3c/src/futon3c/arxana/` and
  `futon3c/test/futon3c/arxana/edit_robustness_test.clj`.
- `dev/arxana-links.el` — current-buffer anchor cascade and moving scope donor.
- The 2026-11-07 operator/Codex design conversation recorded by the turn
  substrate.

### Owner and handoff

Joe owns the conceptual direction and operator acceptance. The next technical
driver should begin with MAP rather than implementing the illustrative EDN in
the conceptual note. The mission is born-ready: no separate admission gate is
required.

## 2. MAP — survey questions

MAP must produce facts and a ready/missing table; it must not select the final
schema prematurely.

- [ ] **Q1 — Existing 'clusion shapes:** Where do current stores and code
  already represent roles, topology, operations/dynamics, evidence, standing,
  composition, and convergence policy? Which are prose-only?
- [ ] **Q2 — 象 source-to-intent path:** What exact records, IDs, parser/model
  outputs, proforma marks, and assessments currently exist from source turn to
  interpreted move? Where is interpretation authority assigned?
- [ ] **Q3 — REPL Circus/GCC path:** Which turn, reply, pattern, mission,
  capability, candidate-action, and receipt records exist live? What exact
  step from interpreted intent to GCC action surface is missing?
- [ ] **Q4 — Projection provenance:** Which current Emacs, Element, Marimo,
  VS Code, HTML, and graph views can map a rendered fragment back to graph
  records and projection rules?
- [ ] **Q5 — Edit proposal strata:** Which clients currently distinguish
  manifestation, content, relation, projection-rule, and presentation-state
  edits? Where are these silently conflated?
- [ ] **Q6 — Recursive edges:** Can a current hyperedge or assertion be an
  endpoint of another first-class relation without flattening it to opaque
  props? Test actual write and query routes.
- [ ] **Q7 — Convergence policies:** Inventory current first-write-wins,
  append-only, compare-and-set, sequencing, coexistence, and refusal behavior
  by record type. Is any CRDT/OT implementation already suitable?
- [ ] **Q8 — Mathematical fixtures:** Which Mark8/APM and Lean/LaTeX/HTML/
  generated-informal artifacts provide the smallest honest F3 fixture, with
  known partial or contradictory correspondence?
- [ ] **Q9 — Geometry authority:** Which existing Lean, JVM, Clojure, or
  frontend libraries can represent and verify the unit-circle map and fibre
  without making a rendering library the semantic authority?
- [ ] **Q10 — Demonstration surfaces:** What is the smallest maintained path
  for linear, graph/GCC, and geometric projections, including mobile access
  and deterministic replay?

### Required MAP output

| Ready — no new code | Missing — actual work |
|---|---|
| To be filled from Q1–Q10 evidence | To be filled from Q1–Q10 evidence |

### Known evidence, not MAP conclusions

- The JVM Edit Robustness substrate test passes one strategy across seven
  content-addressed revisions.
- Correspondence assertions and assessments already preserve ambiguity and
  provenance in the JVM implementation.
- `M-self-representing-stack` has a prior 'clusion account, but it has not been
  reconciled against the current JVM schemas and action boundary.
- 象 has extensive retained move fixtures and a pending demonstration, but its
  exact readiness for F1 must be re-censused rather than assumed.
- P5 of M-jvm-arxana found no then-current legitimate multi-writer intent; F2
  now supplies the concrete re-arm case of Emacs and VS Code co-editing one
  artifact.

## Later lifecycle skeleton

### 3. DERIVE

- [ ] Specify entity, 'clusion, projection, chart, fibre, edit proposal,
  assessment, and conflict record shapes from MAP evidence.
- [ ] Assign one authority per question and one convergence policy per core
  record/relation type.
- [ ] Write the preserve/adapt/drop fidelity matrix for existing Arxana, 象,
  REPL Circus, and JVM contracts.
- [ ] Define the F1–F6 acceptance fixtures and dependency order.
- [ ] State every non-obvious choice as IF/HOWEVER/THEN/BECAUSE.
- [ ] Determine whether a wiring diagram is required and create it if so.

### 4. ARGUE

- [ ] Cross-reference the DERIVE design against `futon3/library/` and named
  war-room rulings.
- [ ] Argue why recursive contentful 'clusions cannot be reduced to endpoint
  lists, RDF-style predicates, document spans, or one universal merge rule.
- [ ] Give a plain-language account of how finite projections can preserve
  meaningful structure without claiming universal sameness.

### 5. VERIFY

- [ ] Complete the specification BOM for storage, protocol, recursive schema,
  concurrency, interpretation, geometry, and projection fidelity.
- [ ] Spike the riskiest unsupported commitments before general implementation.
- [ ] Record falsifiers and stop-the-line conditions for every demonstration.

### 6. INSTANTIATE

- [ ] Implement only contracts that survived VERIFY, in F1–F6 dependency
  order, retaining one runnable fixture and trace per increment.

### 7. DOCUMENT

- [ ] Publish the progressive demonstration path, conceptual glossary,
  authority boundaries, and a record of what remains intentionally unresolved.
