# Mission: The Three-Column Stack

**Date:** 2026-03-03
**Status:** MAP
**Blocked by:** None (M-self-representing-stack proof of concept complete,
futon1a hyperedge API operational, core.logic foundation in place)
**Owner:** futon4 (Arxana), with dependencies on futon5 (AIF+ formalism,
wiring diagrams), futon3c (Mission Control, core.logic), futon1a (hyperedge
store), futon6 (math content, monograph)

## Motivation

M-self-representing-stack (2026-02-22, re-opened 2026-03-03) demonstrated that
the plumbing works: hyperedges round-trip through XTDB, tensions trace through
6 gates, reflection envelopes ground strategic claims to source code. But it
addressed only one column of three, produced 21 hyperedges (lost to a
non-durable restart, then restored 2026-03-03), and defined no cross-column
invariants.

The futon stack is past a quarter million lines of code. Coherence at this
scale requires structural reasoning — not code review by eyeball, but
machine-checkable invariants that span the full representational surface.
The stack needs to know what it knows, what it doesn't know, and where its
claims don't match reality.

Beyond coherence, the same capability has commercial value. If you can
represent and reason about your own codebase, you can represent and reason
about anyone's codebase. If you can represent a proof-in-progress, you can
represent any structured reasoning process (technical audit, due diligence,
business case analysis). The futon stack is the first and best-instrumented
instance of a general capability.

### Three Columns

The self-representing stack has three representational domains. Each has
entities (things), relations (connections), and process structure (how things
change over time):

**Column 1: Knowledge creation (mathematics / structured reasoning)**

Content: mathematical proofs, definitions, conjectures, discourse structure.
Currently lives in futon6 JSON files (`thread-633512-hypergraph.json`), browsed
by `arxana-browser-hypergraph.el` via local file reads. Rich structure already
extracted: posts (Q/A/comment), scopes (let-bindings, universal quantifiers,
constraints), expressions (LaTeX + sexp), terms (PlanetMath surface forms),
discourse edges (illocutionary acts, adversatives), mention edges, categorical
annotations, surface positions.

The purpose is not encyclopedia display. It's representing reasoning *process*:
"here is where we checked that we are actually using the correct definition
before proceeding deeper into the proof." A proof-in-progress has moves, checks,
backtracking — the same structure as a development sprint or a code review.

**Column 2: Development process (project management)**

Content: missions, devmaps, prototypes, components, coverage, tensions,
evidence trails, portfolio reviews. Lives in futon3c (Mission Control APIs),
futon3/holes (devmap files), futon1a (evidence store). Already partially
navigable via the trace browser (arxana-browser-trace.el) and the tension
export API.

This is the column M-self-representing-stack addressed. The work here is
mostly done — what's missing is persistence (the 21 hyperedges need to survive
restarts) and cross-column links.

**Column 3: Code itself (reflection)**

Content: namespaces, vars, arglists, docstrings, dependency graphs, protocols,
defmethod dispatch, macro/dynamic flags, file/line locations, source forms.
Lives in the running JVM (Clojure reflection), accessible via futon3c's
reflection API (`/reflect/var/:ns/:var`, `/reflect/namespaces`,
`/reflect/deps/:ns`).

M-self-representing-stack treated code as a bottom-out target — the last gate
in a 6-gate trace. But code has its own navigable topology: namespaces contain
vars, vars have dependency graphs, protocols have implementations, multimethods
have dispatch hierarchies. This topology is the code analog of math's scope/
expression/term structure. It should be first-class in the hypergraph, not
just a terminal annotation.

### Theoretical Substrate: AIF+ Wiring Diagrams

The three columns are not three separate databases with cross-links. They
are three *projections* of the same underlying formalism: the AIF+ wiring
diagram calculus defined in futon5 (`chapter0-aif-as-wiring-diagram.md`,
`ct/mission.clj`).

An AIF+ diagram is a typed directed graph with ports (inputs/outputs),
components (boxes), and edges (wires), subject to six structural invariants:

- **I1 (Boundary Integrity):** distinguishable inside/outside
- **I2 (Observation-Action Asymmetry):** system must sense AND act
- **I3 (Timescale Separation):** fast dynamics constrained by slow
- **I4 (Preference Exogeneity):** fast actions cannot rewrite slow preferences
  (wireheading test: no Action→Preference path without Environment)
- **I5 (Model Adequacy):** internal structure tracks external structure
- **I6 (Compositional Closure):** no single point of failure

The futon5 mission validator already checks these on mission diagrams via
graph traversal (`futon5/src/futon5/ct/mission.clj`, 1100+ lines). The
same invariants apply structurally to each column:

| Column | Components | Wires | Ports | I4 check |
|--------|-----------|-------|-------|----------|
| **Code** | Namespaces/modules | Dependency edges, type flows | Entry points (M-x, API, CLI) | Refactoring doesn't rewrite requirements |
| **Math** | Claims, definitions, scopes | Inferential, conflict, preference edges | Premises, conclusions | FALSIFY-before-CONSTRUCT (Proof Peripheral) |
| **Project** | Missions, components | Coverage, evidence, pattern-use edges | Inputs (preconditions), outputs (deliverables) | Already validated by futon5 |

The First Proof Sprint monograph (futon6, Figure 22.1 "The Argument")
demonstrates this concretely: raw git hashes feed evidence ledgers, which
feed post-hoc pattern reconstruction, which feeds an explicit AIF graph.
The monograph's conclusion identifies "the gap between post-hoc annotation
and real-time capture" as a tooling gap, not a conceptual one. This mission
builds the tooling.

**Note on terminology:** The monograph uses "AIF" for Active Inference
Framework throughout (see glossary in `intro-making-of.tex`), but the
concrete node/edge typing (claim, conflict, preference, attacks, supports)
is drawn from the Argument Interchange Framework (Chesñevar et al.). The
two frameworks are complementary — Active Inference provides the
control-theoretic loop (sense→infer→act), Argument Interchange provides the
reasoning structure (claim→attack→support). A future revision of the
monograph should clarify this dual usage.

**Generalization via wiring diagrams:** A wiring diagram with certified
invariants IS a proof of those invariants — the structure itself is the
evidence (no separate "proof object" needed). This means:
- A block of code with known entry points maps to a wiring diagram / flowchart
- A set of definitions and lemmas maps to a wiring diagram
- A set of business processes maps to a wiring diagram
- Cross-column invariants become: "the three projections of the same
  underlying diagram are consistent"

### Cross-Column Invariants: The Actual Product

Individual columns are databases. Cross-column invariants are what make the
system a reasoning surface. Examples:

**Project ↔ Code:**
- Every public entry point (M-x command, API endpoint, CLI subcommand) has a
  docstring
- Every mission references the patterns that were used (PUR trail complete)
- Every devmap component with `:maturity :active` or higher has at least one
  var with a reflection envelope
- No mission claims "complete" if its target vars have unresolved TODOs in
  source

**Math ↔ Math (internal consistency):**
- Every proof step that introduces a definition has a checked reference to
  the definition source
- Every scope binding is used within its scope (no dangling let-bindings)
- Every claimed equivalence has matching categorical annotations on both sides

**Project ↔ Math (capacity development / μάθησις):**
- Mathematical work traces to a capability development goal tracked by
  FutonZero (M-futonzero-mvp) — mathematics is an instance of μάθησις
  (learning), not artifact production
- The Proof Peripheral's discipline gates (FALSIFY-before-CONSTRUCT,
  TryHarder licensing) are satisfied before mathematical results feed back
  into project decisions — we are working on the right problem before we
  work on the problem
- Capability deltas from mathematical practice (task improvement, discipline
  adaptation) are visible in the portfolio as evidence of capacity growth,
  not just mission completion

**Code ↔ Code (internal consistency):**
- Every protocol has at least one implementation in a loaded namespace
- No circular namespace dependencies (or explicitly documented exceptions)
- Every `defmethod` dispatch value corresponds to a known entity type

These invariants are structurally grounded in the AIF+ formalism (I1–I6).
Many can be checked as graph properties of wiring diagrams — e.g., I4
(wireheading) is detectable by path traversal; I6 (compositional closure)
by component removal analysis. On the Clojure side, enforcement uses
core.logic (Layer 2 of M-self-representing-stack's three-layer logic
architecture) and the futon5 mission validator. When an invariant fails, it
emits a tension. The tension browser surfaces it. A human or agent proposes
a fix. The fix emits evidence. The loop closes.

### Generalization Path

The underlying representation is the AIF+ wiring diagram, not a
futon-specific schema. Each column instantiates the same diagram calculus:

- **Any Clojure project:** `var:<ns>/<symbol>` as components, dependency
  edges as wires, entry points as ports → checked against I1–I6
- **Any Java project:** Java reflection (`/reflect/java/:class`) extends the
  same model with classes as components, method signatures as wire types
- **Any structured reasoning process:** Post/scope/expression/term types map
  to claims/definitions/inferences in any domain — the same wiring structure
  that represents a mathematical proof represents a technical audit, a due
  diligence process, or a business case analysis
- **Any project with missions:** `covers`, `evidences`, `about-var` work for
  any project with components and missions — futon5 already validates these
  as wiring diagrams

The futon5 wiring diagrams that regulate cellular automata runs are already
a basic kind of "self-representing" computer program: they specify
information flow (inputs→components→outputs) with typed edges and
structural invariants. This mission generalizes them: given a block of code
with known entry points, map it to a wiring diagram; given a set of
definitions and lemmas, map it to a wiring diagram; given a set of business
processes, map it to a wiring diagram. The AIF+ invariants (I1–I6) then
apply uniformly.

The futon stack is instance #1. Instance #2 is an external codebase or
business process, demonstrating that the representation and reasoning
capabilities generalize. This mission builds the schema and invariant
framework; applying it externally is a follow-on (likely involving futon7,
the Markov blanket / external world boundary).

## Scope In

### Schema (DERIVE)

- Define entity types for all three columns with stable identity patterns
- Define relation types (binary) for within-column and cross-column links
- Define hyperedge types (n-ary) for genuinely multi-endpoint structures
- Specify which entity types are *ingested* (from existing sources) vs
  *derived* (computed by logic rules) vs *authored* (created by humans/agents)

### Data pipeline (VERIFY)

- Re-ingest the 21 proof-of-concept hyperedges with durable persistence
- Port futon6 math JSON to futon1a hyperedges (prove round-trip)
- Keep JSON as backup until round-trip read-back is confirmed
- Ingest code-column entities from reflection API snapshots
- Make ingestion idempotent and repeatable (not one-shot)

### Invariant framework (INSTANTIATE)

- Implement 3–5 cross-column invariants in core.logic
- Wire invariant violations to tension generation
- Surface violations in the trace browser
- Demonstrate the full loop: invariant fails → tension emitted → browsable
  in Arxana → human/agent acts → tension resolves

### Browser integration

- Extend arxana-browser-hypergraph.el to read from futon1a (not just JSON)
- Add code-column browser views (namespace explorer, var detail, dep graph)
- Cross-column navigation: from a math expression to the code that implements
  the concept, from a mission to the vars it claims to address, from a var
  to the missions and proofs that reference it

## Scope Out

- Applying the model to external codebases (follow-on, futon7)
- Web-based visualization (Arxana is Emacs-native)
- Automated mission creation from invariant violations (humans decide)
- Building a full theorem prover (we represent proof *process*, not automate it)
- Replacing Mission Control (MC produces data, this mission makes it navigable
  and checkable)

## Derivation Path

### 1. IDENTIFY (this document)

The mission proposal. Names the three columns, anchors the invariant
framework, scopes the work.

- [x] Name the gap and motivate the mission
- [x] Define the three columns with examples
- [x] Specify cross-column invariants (examples per column pair)
- [x] State the generalization path (futon → any Clojure → any project)
- [x] Scope in/out
- [x] Review with Joe — does the framing match the vision? (approved 2026-03-04)
- [x] Identify any missing columns or invariant categories
  - Math↔Code omitted deliberately: math column represents proof *process*
    (not a math library), so direct math→code links are not yet motivated.
    If this changes (e.g., symbolic computation in Clojure), add it then.
  - Four categories sufficient: Project↔Code, Math↔Math, Project↔Math,
    Code↔Code. Each has 3+ concrete invariant examples.
- [x] Confirm completion criteria are testable and sufficient
  - All 8 criteria are concrete: counts (200 hyperedges, 3 invariants),
    round-trips (JSON→XTDB→read-back), demo loops (violation→tension→resolve),
    documentation deliverables (parameterized schema notes).

### 2. MAP (surveyed 2026-03-04)

Survey the current state of all three columns.

- [x] **Q1 (math column schema):**
  - 82 nodes: 43 expressions, 19 terms, 16 posts, 4 scopes
  - 99 edges: 43 surface, 26 mention, 15 iatc, 8 discourse, 4 scope,
    3 categorical
  - Node types have clear identity patterns: `{a|c|q}-<se_id>` for posts,
    `expr:<parent>:<hash8>` for expressions, `term:<PascalCase>` for terms,
    `<parent>:scope-<seq>` for scopes
  - Expressions carry dual representation: `latex` (presentation) + `sexp`
    (structure) — sexp enables symbolic reasoning
  - Scope subtypes encode proof structure: bind/let, quant/universal,
    quant/existential, constrain/where, env/theorem, assume/consider, etc.
  - Browser (`arxana-browser-hypergraph.el`) has full rendering: 6 faces,
    post-centric lens view, discourse/scope/mention/surface sections per post
  - 105+ additional JSON files in futon6 (showcases, ct-validation golden set,
    nlab-wiring, physics-se-classical)

- [x] **Q2 (code column schema):**
  - 6 reflection endpoints operational at `/api/alpha/reflect/*`:
    - `GET /namespaces` — list + regex filter → `{ns, doc?, file?}`
    - `GET /ns/:ns` — public vars → `{name, arglists, doc, file, line, private?, macro?, dynamic?}`
    - `GET /ns/:ns/full` — public + private vars
    - `GET /var/:ns/:var` — full ReflectionEnvelope (Malli-validated)
    - `GET /deps/:ns` — `{requires, imports, required-by}`
    - `GET /java/:class` — Java class reflection (bases, flags, members)
  - **Gaps:** No protocol listing/enumeration, no defmethod dispatch tracking,
    no multimethod hierarchy, no record/deftype fields, no source form retrieval
  - Protocol flag declared in envelope schema but never populated
  - 35+ tests in core_test.clj, tools wired into explore + mission-control
    peripherals

- [x] **Q3 (project column inventory):**
  - 21 hyperedges in XTDB (restored 2026-03-03)
  - 73 mission backfill evidence entries in evidence store
  - **Re-ingestible without new code (Phase 1):** 9 tensions + 10 devmaps =
    19 hyperedges via existing `arxana-browser-lab.el` functions
  - **Trivial new endpoints (Phase 2):** 10 coverage + 30 doc-audit +
    ~15 actionable + 1 mana = ~56 hyperedges
  - **Needs infrastructure (Phase 3):** 150+ code reflection snapshots
    (bulk var ingestion, requires scheduler)
  - **Path to 200:** Phase 1+2 = ~96 hyperedges (project column alone).
    Remaining ~100 from code reflections or math column ingestion
  - 41 missions scannable across 7 repos, 10 devmap summaries with
    component structure, coverage analysis computable on-the-fly

- [x] **Q4 (core.logic relations):**
  - **Operational (8):** missiono, statuso, blocked-byo, repo-ofo, evidenceo,
    patterno-used, shapeso-defined, mana-fundedo — portfolio adjacency logic
    works end-to-end (adjacent-possible, what-if, critical-path, clusters)
  - **Stubs (6+):** devmapo, devmap-stateo, componento, coverso, hyperedgeo,
    hx-endpointo — facts added but rarely/never queried
  - **Empty (3):** invarianto, implementedo, annotatedo — defined in schema,
    no data source, test-only
  - **Only one tension type derived:** `uncovered-component` via
    `query-derived-tensions`. No invariant violation checking, no cross-column
    linking relations
  - Framework supports the pattern (derive tensions from logic, compare to
    stored hyperedges) but needs invariant population and check functions

- [x] **Q5 (browser migration):**
  - **Data model mismatch:** Browser expects `{nodes: [...], edges: [...]}`
    from JSON file; futon1a returns hyperedges only (no node metadata)
  - **Type system:** JSON uses strings (`"iatc"`), futon1a uses keywords
    (`:link/refers-to`)
  - **Attribute access:** JSON `edge.attrs.act` vs futon1a `:hx/props`
  - **Thin adapter (1 day):** type conversion + props mapping → hyperedge
    visualization works, but no node metadata preview
  - **Full adapter (3–5 days):** entity fetching for node metadata, caching,
    full parity with JSON-based experience

- [x] **Q6 (implicit invariants):**
  - **No conventional CI/linting/hooks** — no GitHub Actions, no clj-kondo,
    no pre-commit hooks. BUT: futon5 already provides a structural checking
    framework far more powerful than linting — the AIF+ diagram validator
    checks 5 structural properties + 3 invariant properties (I3 timescale,
    I4 exogeneity, I6 closure) on wiring diagrams via graph traversal
  - **Rich documented invariants:** futon3c I-1 through I-5 (architectural),
    R1–R10 (wiring contract), futon1a I0–I4 (layer-based), futon5 I1–I6
    (AIF+ structural)
  - **Machine-readable contracts:** `wiring-claims.edn` + `wiring-evidence.edn`
    with commit-scoped evidence; futon5 mission `.edn` files with typed
    ports/components/edges
  - **Test coverage:** futon3c 1,052 tests / 2,954 assertions; futon1a
    157 tests / 387 assertions; futon4 and futon0 have no tests
  - **Candidate formalizations:** the futon5 diagram validator could be
    extended to check cross-column invariants as wiring diagram properties;
    additionally, docstring coverage (grep), no-subprocess-in-transport
    (grep + test exists), no-futon3-deps (grep + test exists), namespace
    hierarchy (regex), error attribution (static analysis)
  - **Naming conventions:** `futon3c.{domain}.{subdomain}`, `verb-noun`
    functions, Malli shape names capitalized, namespace docstrings reference
    invariants

- [x] **MAP summary:**

  **What's ready:**
  - Math column: rich schema with 181 entities (82 nodes + 99 edges),
    browser rendering complete, identity patterns clear
  - Project column: 21 hyperedges persisted, ingestion pipeline exists,
    ~96 more achievable from existing APIs
  - Code column: 6 reflection endpoints operational for ns/var/dep/Java
  - core.logic: portfolio adjacency works, framework for tension derivation
    in place
  - Documented invariants: I-1 through I-5, R1–R10, I0–I4 already specified
    in prose and partially in machine-readable claims

  **What's missing:**
  - Code column gaps: no protocol/defmethod/record reflection endpoints
  - core.logic: invarianto/implementedo have no data source; only one
    tension type (uncovered-component) can be derived
  - Browser: adapter needed for futon1a migration (1–5 days)
  - No conventional CI, but futon5 AIF+ validator provides structural
    checking — needs to be connected to the hypergraph invariant framework
  - Math→futon1a ingestion: not yet attempted
  - Three-column→wiring-diagram mapping not yet formalized (the connection
    between hypergraph entities and AIF+ diagram components/wires/ports)

  **Critical path:**
  1. **DERIVE schema** — entity/relation/hyperedge types for all three columns
     (blocks everything else)
  2. **Math ingestion** — port JSON to futon1a hyperedges (biggest count
     contributor: 181 entities from one thread alone)
  3. **Code reflection bulk snapshot** — ingest ns/var entities from live JVM
     (150+ hyperedges)
  4. **Invariant framework** — populate invarianto, implement check functions,
     wire to tension generation
  5. **Browser adapter** — thin adapter for futon1a reads (unblocks cross-
     column navigation)

### 3. DERIVE

Entity types, relation types, hyperedge types, and invariant rules for all
three columns. IF/HOWEVER/THEN/BECAUSE justifications for design decisions.
This is the core of the mission.

- [ ] Entity type table (all three columns, with identity patterns, source,
  ingested/derived/authored)
- [ ] Binary relation types (within-column and cross-column)
- [ ] Hyperedge types (n-ary connections)
- [ ] Invariant rules (at least 3 cross-column, with core.logic signatures)
- [ ] Data flow: which systems produce which entities, how they reach futon1a
- [ ] Browser view specifications (what views serve each column)

### 4. ARGUE

Synthesis drawing on the AIF+ wiring diagram formalism (futon5), the
theoretical anchoring from M-self-representing-stack (Higgins'
self-discrepancy, reflexivity loop, frozen dynamics, homoiconicity), the
First Proof Sprint monograph (Figure 22.1, "The Argument"), plus the
commercial generalization argument.

- [ ] Why three columns and not two or four — they are the three natural
  projections of an AIF+ diagram (code=implementation, math=reasoning
  structure, project=process flow)
- [ ] Why AIF+ invariants (I1–I6) are the right structural language —
  they apply to any viable system (code, proofs, business processes) and
  are checkable as graph properties of wiring diagrams
- [ ] Why cross-column invariants are the product (not the data) — the
  three projections must be consistent; inconsistency is a tension
- [ ] How wiring diagrams generalize beyond futon: code entry points as
  ports, definitions/lemmas as components, business processes as flows —
  same diagram calculus, same invariant checks
- [ ] The monograph's thesis: "post-hoc → real-time" is a tooling gap, not
  a conceptual one — this mission closes the gap
- [ ] Pattern references (which futon3 patterns inform the design)
- [ ] Clarify AIF dual usage: Active Inference (control loop) vs Argument
  Interchange (reasoning structure) — complementary, not competing

### 5. VERIFY

Data pipeline operational. Round-trip confirmed for all three columns.
At least 3 cross-column invariants implemented and generating tensions.

- [ ] Math JSON → futon1a round-trip (write + read-back confirmed)
- [ ] Code column entities (ns/var/dep snapshots) in futon1a
- [ ] Project column re-ingestion idempotent and repeatable
- [ ] ≥200 hyperedges across all three columns
- [ ] 3+ cross-column invariants in core.logic, generating tensions
- [ ] Invariant violations visible in trace browser or equivalent

### 6. INSTANTIATE

Full loop demonstrated: invariant violation → tension → browsable →
human acts → resolves. At least one instance per column pair (project↔code,
math↔math, project↔math).

- [ ] Project↔Code loop: e.g., "undocumented entry point" → tension → add
  docstring → tension resolves
- [ ] Math↔Math loop: e.g., "definition used without checked reference" →
  tension → add reference → resolves
- [ ] Project↔Math loop: e.g., "math work without capability tracking in
  FutonZero" → tension → register capability goal → resolves; or
  "CONSTRUCT attempted without prior FALSIFY" → tension → add falsification
  step → resolves
- [ ] Write up the demo as a reproducible walkthrough

## Source Material

| Source | What We Take |
|--------|-------------|
| M-self-representing-stack (this repo) | Proof of concept, theoretical anchoring, 21-hyperedge schema |
| `futon5/docs/chapter0-aif-as-wiring-diagram.md` | AIF+ formalism, I1–I6 invariants, diagram calculus |
| `futon5/src/futon5/ct/mission.clj` | Mission diagram validator (structural + invariant checks) |
| `futon5/data/missions/*.edn` | 11+ wiring diagrams (missions, exotypes, agent loops) |
| `futon6/data/first-proof/latex/part4-proof-patterns.tex` | "The Argument" synthesis (Figure 22.1), AIF graph examples |
| `futon6/data/first-proof/thread-633512-hypergraph.json` | Math column entity/edge schema |
| `futon6/data/showcases/hypergraph-showcase.json` | Additional math content samples |
| `futon3c/src/futon3c/peripheral/mission_control_backend.clj` | Project column APIs |
| `futon3c/src/futon3c/reflection/` | Code column reflection API |
| `futon3c/src/futon3c/logic/` | Existing core.logic relations |
| `futon4/dev/arxana-browser-hypergraph.el` | Math column browser (JSON-based) |
| `futon4/dev/arxana-browser-trace.el` | Project column browser (API-based) |
| `futon4/dev/arxana-store.el` | Hyperedge read/write API |
| `futon1a/src/futon1a/api/routes.clj` | Hyperedge HTTP endpoints |

## Relationship to Other Missions

- **M-self-representing-stack** (futon4, re-opened): Proof of concept for this
  mission. This mission subsumes its unfinished work and generalizes it.
- **M-portfolio-inference** (futon3c): Determines what to build next. This
  mission provides the navigable, checkable representation that portfolio
  inference reasons about.
- **M-mission-control** (futon3c, complete): Produces project-column data.
- **M-diagram-composition** (futon5): Defines the pheno-geno-exo composition
  rules for wiring diagrams. The three-column stack's cross-column invariants
  are instances of composition checks (the three projections must be
  consistent under the same AIF+ invariants).
- **futon5 AIF+ formalism** (futon5): Provides the theoretical substrate.
  The I1–I6 invariants, diagram validator, and wiring diagram calculus are
  the structural language for this mission's cross-column invariants. The
  futon5 wiring diagrams that regulate cellular automata runs are a
  precedent for self-representing programs.
- **First Proof Sprint monograph** (futon6): Demonstrates the application of
  AIF+ typing to proof process (Figure 22.1). The monograph's gap
  ("post-hoc → real-time") is what this mission's tooling addresses.
- **futon7** (not yet a mission): External world boundary. The generalization
  of this mission's schema to external codebases and business processes is
  futon7 work.

## Completion Criteria

1. Entity types defined and documented for all three columns
2. At least 200 hyperedges persisted in futon1a (across all three columns),
   surviving restarts
3. Math JSON successfully ported to futon1a with confirmed round-trip
4. Code-column entities (namespaces, vars, deps) represented as first-class
   hypergraph structure
5. At least 3 cross-column invariants implemented in core.logic, generating
   tensions when violated
6. Invariant violations browsable in Arxana (trace browser or equivalent)
7. Full loop demonstrated for at least one invariant: violation → tension →
   browse → act → resolve
8. Schema documented as parameterized (not futon-specific), with notes on
   how it would apply to an external Clojure/Java project
