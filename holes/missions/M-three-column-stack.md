# Mission: The Three-Column Stack

**Date:** 2026-03-03
**Status:** ARGUE
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

- [x] Entity type table (§3.1 — 4 math, 6 project, 4 code entity types
  with identity patterns, attributes, sources, ingestion paths)
- [x] Binary relation types (§3.2 — 6 within-math, 4 within-project,
  3 within-code, 5 cross-column)
- [x] Hyperedge types (§3.3 — 5 existing + 4 new n-ary types)
- [x] Invariant rules (§3.4 — 5 invariants: INV-1 through INV-5, spanning
  Project↔Code, Math↔Math, Code↔Code, Project↔Math, with core.logic
  signatures)
- [x] Data flow (§3.5 — ASCII diagram + ingestion paths table, all
  idempotent)
- [x] Browser view specifications (§3.6 — 5 views: trace, hypergraph,
  namespace explorer, invariant dashboard, cross-column navigator)

#### 3.1 Entity Types

**Math Column (ingested from futon6 JSON)**

| Entity Type | Identity Pattern | Attributes | Source | Count (thread-633512) |
|-------------|-----------------|------------|--------|----------------------|
| `post` | `post:<site>/<se-id>` | subtype (question/answer/comment), score, is_accepted, parent, title, tags | futon6 JSON `nodes` | 16 |
| `expression` | `expr:<parent-id>/<blake2b-8>` | latex, sexp, display | futon6 JSON `nodes` | 43 |
| `scope` | `scope:<parent-id>/<seq>` | subtype (bind/let, quant/universal, constrain/where, env/theorem, assume/consider, …), match, ends (role-labeled bindings) | futon6 JSON `nodes` | 4 |
| `term` | `term:<slug>` (e.g., `term:commutative-diagram`) | surface_forms (array) | futon6 JSON `nodes` | 19 |

**Project Column (ingested from MC APIs + evidence store)**

| Entity Type | Identity Pattern | Attributes | Source | Ingestion |
|-------------|-----------------|------------|--------|-----------|
| `mission` | `mission:<repo>/<id>` | status, date, blocked-by, gates, doc-audit | `build-inventory` (MC backend) | Backfill → evidence store |
| `devmap` | `devmap:<id>` | state, component-count, edge-count, input/output ports, all-valid | `read-all-devmaps` (MC backend) | HX via `arxana-browser-lab.el` |
| `component` | `component:<devmap>/<id>` | name, maturity (from devmap) | Extracted from devmap components | Derived (currently only HX endpoints) |
| `tension` | `tension:<devmap>/<component>/<type>` | type (uncovered/blocked/structural), summary, detected-at, coverage-pct | `build-tension-export` (MC backend) | HX via `arxana-browser-lab.el` |
| `evidence-entry` | `evidence:<id>` (UUID-based) | type, claim-type, author, body, tags, at, pattern-id, session-id | Evidence store (futon3c) | Already persisted |
| `pattern` | `pattern:<namespace>/<name>` | rationale, hotwords, truth (sigil) | Pattern library (futon3/library/) | Authored |

**Code Column (ingested from reflection API snapshots)**

| Entity Type | Identity Pattern | Attributes | Source | Ingestion |
|-------------|-----------------|------------|--------|-----------|
| `namespace` | `ns:<symbol>` | doc, file | `GET /reflect/namespaces` | Snapshot → HX |
| `var` | `var:<ns>/<symbol>` | arglists, doc, file, line, private?, macro?, dynamic? | `GET /reflect/ns/:ns` | Snapshot → HX |
| `var-envelope` | `var:<ns>/<symbol>` (same, richer) | + resolved-at, tag | `GET /reflect/var/:ns/:var` (Malli-validated) | Snapshot → HX |
| `java-class` | `java:<fqcn>` | bases, flags, members | `GET /reflect/java/:class` | Snapshot → HX |

*Not yet queryable (needs new endpoints):* protocol, defmethod/multimethod,
record/deftype. These are scoped out of the first pass — the existing 4
code entity types are sufficient for the initial invariant framework.

**IF** the code column needs protocol/defmethod entities for cross-column
invariants (Code↔Code category: "every protocol has ≥1 implementation"),
**HOWEVER** no reflection endpoints exist for these today,
**THEN** implement protocol/multimethod reflection as a follow-on after the
initial 200-hyperedge milestone is reached,
**BECAUSE** the existing ns/var/dep entities are sufficient for the
Project↔Code invariants (docstring coverage, PUR trail), which are
higher priority.

#### 3.2 Binary Relation Types

**Within Math Column**

| Relation | Endpoints | Attrs | Notes |
|----------|----------|-------|-------|
| `iatc` (illocutionary act) | post → post | act (assert, clarify, exemplify, challenge, query, agree, reference, reform) | Discourse structure — who says what to whom |
| `mention` | post → term | surface (text span) | Term occurrences in posts |
| `surface` | expression → post | position (char offset) | Maps math expressions to their text location |
| `scope-binding` | scope → post | binding_type | Links binding context to the post it scopes |
| `categorical` | post → (self) | concept (cat/equivalence, cat/limit, …), score | Category-theoretic annotations |
| `discourse` | post → (self) | role (wire/port/label), dtype, match | Rhetorical structure signals |

**Within Project Column**

| Relation | Endpoints | Attrs | Notes |
|----------|----------|-------|-------|
| `covers` | mission → component | — | "This mission addresses this devmap component" |
| `evidences` | evidence-entry → mission | claim-type | "This evidence supports/refutes this mission claim" |
| `pattern-use` | mission → pattern | via (PSR/PUR id) | "This mission applied this pattern" |
| `blocked-by` | mission → mission | — | Dependency chain |

**Within Code Column**

| Relation | Endpoints | Attrs | Notes |
|----------|----------|-------|-------|
| `requires` | namespace → namespace | — | Clojure ns requires |
| `imports` | namespace → java-class | — | Clojure ns imports |
| `defines` | namespace → var | — | "This ns contains this var" |

**Cross-Column**

| Relation | Endpoints | Category | Notes |
|----------|----------|----------|-------|
| `about-var` | {mission, tension, component} → var | Project↔Code | "This project entity references this code var" (existing: `reflection/about-var` HX type) |
| `implements` | var → component | Project↔Code | "This var implements this devmap component" |
| `entry-point` | var → {mission, devmap} | Project↔Code | "This var is a public entry point for this system" |
| `capability-goal` | {mission, evidence-entry} → futonzero-goal | Project↔Math | "This work traces to a capability development goal" |
| `discipline-gate` | evidence-entry → {proof-step, scope} | Project↔Math | "This evidence records a Proof Peripheral gate satisfaction" |

#### 3.3 Hyperedge Types (n-ary)

Binary relations (§3.2) use two endpoints. Hyperedges connect 3+
endpoints or have richer internal structure:

| HX Type | Endpoints | Props | Notes |
|---------|----------|-------|-------|
| `tension/uncovered-component` | [devmap, component, ?mission] | summary, detected-at, coverage-pct | Existing — 9 in XTDB |
| `tension/blocked-mission` | [mission, blocker-mission] | summary, detected-at | Existing |
| `tension/structural-invalid` | [devmap, failed-checks…] | summary, detected-at | Existing |
| `devmap/prototype` | [devmap, component₁, component₂, …] | state, component-count, edge-count, all-valid | Existing — 10 in XTDB |
| `reflection/about-var` | [claim-id, var] | reflection envelope fields | Existing — 1 in XTDB |
| `trace/gate-chain` | [tension, gate₁-result, …, gate₆-result] | complete?, blocked-at | NEW: persists the 6-gate trace path |
| `invariant/violation` | [invariant-id, violating-entity₁, …] | rule, message, detected-at | NEW: records an invariant check failure |
| `math/thread` | [post₁, post₂, …, postₙ] | thread-id, site | NEW: groups all posts in a math thread |
| `coverage/analysis` | [devmap, covered-comp₁, …, uncovered-comp₁, …] | total, covered-count, coverage-pct | NEW: snapshot of coverage state |

#### 3.4 Invariant Rules

Five cross-column invariants, with core.logic signatures. Each invariant
is a relation that, when violated, emits a tension of type
`invariant/violation`.

**INV-1: Documented Entry Points** (Project↔Code)

Every public var that serves as an entry point (M-x command, API endpoint,
CLI subcommand) must have a non-nil docstring.

```clojure
(defn query-undocumented-entry-points [db]
  (logic/run* [ns sym]
    (entry-pointo db ns sym)        ;; var is marked as entry point
    (varo db ns sym meta)           ;; var has reflection metadata
    (== (:doc meta) nil)))          ;; docstring is nil
```

Tension emitted: `invariant/violation` with endpoints
`[inv:documented-entry-points, var:<ns>/<sym>]`.

**INV-2: PUR Trail Complete** (Project↔Code)

Every mission with status `:complete` has at least one `pattern-use`
relation (evidence that patterns were selected and outcomes recorded).

```clojure
(defn query-missions-without-pur [db]
  (logic/run* [mid]
    (missiono db mid)
    (statuso db mid :complete)
    (logic/nafc pattern-useo db mid _)))  ;; no pattern-use for this mission
```

Tension emitted: `invariant/violation` with endpoints
`[inv:pur-trail-complete, mission:<repo>/<id>]`.

**INV-3: Definition Reference Checked** (Math↔Math)

Every proof step that introduces a definition (scope with subtype
`bind/let` or `env/theorem`) has at least one `mention` edge linking
to the term being defined.

```clojure
(defn query-ungrounded-definitions [db]
  (logic/run* [scope-id]
    (scopeo db scope-id attrs)
    (membero (:subtype attrs) ["bind/let" "env/theorem"])
    (logic/nafc scope-has-mentiono db scope-id _)))
```

Tension emitted: `invariant/violation` with endpoints
`[inv:definition-reference, scope:<parent>/<seq>]`.

**INV-4: No Circular Namespace Dependencies** (Code↔Code)

The namespace dependency graph (from `requires` relations) must be
acyclic, or cycles must be explicitly documented as exceptions.

```clojure
(defn query-circular-deps [db]
  (logic/run* [ns1 ns2]
    (requireso db ns1 ns2)
    (reachable-viao db ns2 ns1)))    ;; ns2 transitively requires ns1
```

Tension emitted: `invariant/violation` with endpoints
`[inv:no-circular-deps, ns:<ns1>, ns:<ns2>]`.

**INV-5: FALSIFY Before CONSTRUCT** (Project↔Math)

Mathematical work that produces results feeding into project decisions
must have a recorded FALSIFY gate satisfaction (via the Proof Peripheral)
before any CONSTRUCT evidence is recorded.

```clojure
(defn query-unfalsified-constructs [db]
  (logic/run* [eid]
    (evidence-entryo db eid attrs)
    (== (:evidence/type attrs) :proof-construct)
    (logic/nafc has-prior-falsifyo db eid)))
```

Tension emitted: `invariant/violation` with endpoints
`[inv:falsify-before-construct, evidence:<id>]`.

#### 3.5 Data Flow

```
futon6 JSON files ──────────────────────────────┐
  (thread-633512-hypergraph.json, showcases)     │
                                                 │  Math entities
                                                 ▼
                                           ┌──────────┐
futon3c MC APIs ──────────────────────────▶│          │
  GET /mc/tensions                         │          │
  GET /mc/devmaps                          │ futon1a  │
  POST /mc/backfill                        │  XTDB    │
                                           │          │
futon3c Reflection API ───────────────────▶│          │
  GET /reflect/namespaces                  │          │
  GET /reflect/ns/:ns                      └────┬─────┘
  GET /reflect/var/:ns/:var                     │
                                                │ GET /hyperedges
futon3c core.logic ◀────────────────────────────┤
  query-derived-tensions                        │
  invariant checks (INV-1..5)                   │
           │                                    │
           ▼                                    ▼
  tension generation ──────────────▶  arxana-browser (futon4)
  invariant/violation HX                trace, hypergraph views
```

**Ingestion paths:**

| Source | Mechanism | Destination | Idempotent? |
|--------|-----------|-------------|-------------|
| futon6 JSON | New: `arxana-store-ingest-math-thread` (Emacs) | futon1a HX | Yes (stable IDs from content hash) |
| MC tensions | Existing: `arxana-browser-ingest-all-tensions` | futon1a HX | Yes (stable ID: `hx:tension/{endpoints}`) |
| MC devmaps | Existing: `arxana-browser-ingest-all-devmaps` | futon1a HX | Yes (stable ID: `hx:devmap/{endpoints}`) |
| MC backfill | Existing: `POST /mc/backfill` | Evidence store | Yes (duplicate ID check) |
| Reflection snapshots | New: bulk `GET /reflect/ns/:ns` → HX per var | futon1a HX | Yes (stable ID: `var:<ns>/<sym>`) |
| Invariant violations | New: core.logic check → `POST /hyperedge` | futon1a HX | Yes (stable ID from invariant+entity) |

#### 3.6 Browser View Specifications

| View | Column | Data Source | Entry Point | Navigation |
|------|--------|------------|-------------|------------|
| **Trace browser** (existing) | Project | `GET /mc/trace` | Arxana menu → Trace | Devmaps → tensions → 6-gate chain → source |
| **Hypergraph browser** (existing, needs adapter) | Math | futon6 JSON → futon1a | Arxana menu → Encyclopedia | Posts → expressions/scopes/terms/discourse |
| **Namespace explorer** (new) | Code | `GET /reflect/namespaces` + HX | Arxana menu → Code | Namespaces → vars → deps → detail |
| **Invariant dashboard** (new) | Cross-column | `GET /hyperedges?type=invariant/violation` | Arxana menu → Invariants | Violations grouped by rule → entity detail → fix action |
| **Cross-column navigator** (new) | All | HX endpoint traversal | Any entity → related entities | From a var, see missions that reference it; from a mission, see vars it covers; from a math term, see related patterns |

### 4. ARGUE

Synthesis drawing on the AIF+ wiring diagram formalism (futon5), the
theoretical anchoring from M-self-representing-stack (Higgins'
self-discrepancy, reflexivity loop, frozen dynamics, homoiconicity), the
First Proof Sprint monograph (Figure 22.1, "The Argument"), plus the
commercial generalization argument.

- [x] Why three columns and not two or four (§4.1)
- [x] Why AIF+ invariants (I1–I6) are the right structural language (§4.2)
- [x] Why cross-column invariants are the product (not the data) (§4.3)
- [x] How wiring diagrams generalize beyond futon (§4.4)
- [x] The monograph's thesis: "post-hoc → real-time" is a tooling gap (§4.5)
- [x] Pattern cross-reference from futon3/library (§4.6)
- [x] Clarify AIF dual usage (§4.7)
- [x] Plain-language argument (§4.8)

#### 4.1 Why Three Columns and Not Two or Four

**IF** a self-representing system needs to capture its own structure at
multiple levels of abstraction,
**HOWEVER** an undifferentiated graph of "everything about the system"
would be unnavigable and uncheckable,
**THEN** decompose into exactly three columns: what the system *does*
(code), what the system *intends* (project), and what the system *knows*
(knowledge creation),
**BECAUSE** these correspond to the three aspects of any AIF+ wiring
diagram: implementation (the boxes and their internals), process (the
flow of information through the diagram over time), and reasoning (the
argumentative structure that justifies why this diagram and not another).

Two columns would conflate either process with implementation (losing the
ability to ask "does the code match the plan?") or reasoning with process
(losing the ability to ask "are we working on the right problem?"). Four
or more columns would split one of these along an axis that doesn't
correspond to a natural AIF+ projection — e.g., separating "tests" from
"code" creates a column that is derivative of the code column, not
independent of it.

The three-column decomposition also matches the three timescales in the
futon5 pheno-geno-exo composition (`M-diagram-composition.md`):

| Column | Timescale | Composition role |
|--------|-----------|-----------------|
| Code | Fast (social/operational) | Phenotype — the observable behavior |
| Project | Medium (task/sprint) | Genotype — the generative plan |
| Math/Knowledge | Slow (glacial/library) | Exotype — the constraining theory |

This is not coincidental. The AIF+ formalism requires timescale separation
(I3) for viability. A system whose theory changes as fast as its code has
no stable identity; a system whose code never changes relative to its plans
is dead. The three columns instantiate the three timescale bands that I3
requires.

#### 4.2 Why AIF+ Invariants Are the Right Structural Language

**IF** cross-column invariants need to be checkable, not just documented,
**HOWEVER** ad hoc consistency rules (linting, grep patterns, CI scripts)
are brittle and don't compose,
**THEN** ground the invariants in the AIF+ formalism (I1–I6), which
provides a compositional structural language for viability,
**BECAUSE** the AIF+ invariants are graph properties of wiring diagrams
that can be checked by traversal, and the futon5 mission validator
already implements this for mission diagrams (1100+ lines, 5 structural
checks + 3 invariant checks).

The mapping from AIF+ invariants to cross-column checks:

| AIF+ Invariant | Cross-Column Instance | Check Method |
|---------------|----------------------|--------------|
| I1 (Boundary) | Each column has distinct entity types with non-overlapping identity patterns | Schema validation |
| I2 (Obs-Action Asymmetry) | The system both reads its own state (reflection) AND acts on it (tension resolution) | Trace loop completeness |
| I3 (Timescale Separation) | Code changes faster than projects, projects faster than theory | Timestamp ordering on evidence |
| I4 (Preference Exogeneity) | Invariants (slow) are not rewritable by code changes (fast) — no `--no-verify` | INV-5 (FALSIFY before CONSTRUCT); no bypass flags |
| I5 (Model Adequacy) | The hypergraph representation tracks the actual system state | Round-trip verification (VERIFY phase) |
| I6 (Compositional Closure) | Removing any one column degrades but doesn't crash the system | Each column functions independently |

The key insight is **I4 applied to the stack itself**: the invariant rules
(INV-1 through INV-5) are slow-timescale constraints. If the code column
could modify or suppress invariant violations without going through the
project column (i.e., without creating a tension, having a human review
it, and recording evidence of resolution), that would be a wireheading
violation — the system rewriting its own preferences. The
invariant→tension→browse→act→resolve loop is the I4-compliant path.

#### 4.3 Why Cross-Column Invariants Are the Product

**IF** any system can store entities and relations (that's just a database),
**HOWEVER** the value of a self-representing stack is not in the data but
in the ability to reason about structural consistency,
**THEN** the cross-column invariants — not the hyperedges — are the actual
product,
**BECAUSE** invariants are what distinguish a live, self-correcting system
from a static knowledge graph.

The holistic argument sketch (`futon3/holes/holistic-argument-sketch.md`)
identifies a five-step chain: work → proof → patterns → coordination →
understanding → **argument**. The first four steps produce data (code,
evidence, patterns, devmaps). Step 5 — argument — is the claim that the
data is *consistent* and that inconsistencies are *visible*. That claim
is exactly what cross-column invariants provide.

Concretely: 200 hyperedges in XTDB is infrastructure. The statement
"every completed mission has a pattern-use record, and here are the three
that don't" is a product. The first is a database; the second is an
audit finding. The commercial value is in the second.

#### 4.4 How Wiring Diagrams Generalize Beyond Futon

**IF** the three-column schema is parameterized (entity types, relation
types, and invariant rules are not hardcoded to futon),
**HOWEVER** parameterization alone doesn't demonstrate generality,
**THEN** show that the AIF+ wiring diagram mapping works for any system
with entry points, information flow, and structural constraints,
**BECAUSE** a wiring diagram with certified invariants IS a proof of
those invariants — the structure itself is the evidence.

Three generalization levels, each adding a domain:

**Level 1: Any Clojure project.** Replace futon-specific entity IDs with
the target project's namespaces and vars. The reflection API works
unchanged. `var:<ns>/<sym>` is universal. INV-1 (documented entry points)
and INV-4 (no circular deps) apply to any Clojure codebase. The only
futon-specific part is which vars are marked as entry points — and that's
a configuration, not a schema change.

**Level 2: Any Java/JVM project.** The `GET /reflect/java/:class`
endpoint already exists. Classes map to components, methods to wires,
interfaces to ports. The same I1–I6 checks apply: boundary integrity
(package boundaries), timescale separation (API stability vs
implementation churn), compositional closure (no SPOF classes).

**Level 3: Any structured reasoning or business process.** The math
column's post/scope/expression/term types map to any domain where
people make claims, introduce definitions, and draw inferences. A
technical audit has findings (posts), scoping assumptions (scopes),
quantitative evidence (expressions), and domain vocabulary (terms).
A due diligence process has the same structure. The discourse edges
(iatc: assert, challenge, clarify) are universal acts of reasoning.

The futon5 wiring diagrams that regulate cellular automata runs are
the precedent: they specify information flow with typed edges and
structural invariants, and the same diagram calculus transferred from
MetaCA to ants to agent loops. This mission applies the same transfer
to the self-representing stack domain.

#### 4.5 The Monograph's Thesis: Post-Hoc → Real-Time

The First Proof Sprint monograph (futon6, Figure 22.1 "The Argument")
demonstrates the problem and the solution in microcosm:

**Problem:** The proof sprint produced valuable reasoning (layer switches,
obstruction identification, strategy changes), but this reasoning was
reconstructed post-hoc from git commit hashes. The argumentative structure
(claim→conflict→preference→inference) was implicit in the temporal ordering
but not represented explicitly.

**Solution:** Three infrastructure pieces converge: Arxana (typed-edge
graph substrate), the peripheral model (constrained execution environments
where reflection is the only permitted action), and S-expression canonical
form (argument structure and mathematical content coexist). The monograph's
conclusion: "The distance from the present chapter to that system is a
tooling gap, not a conceptual one."

This mission closes the gap by building the tooling:
- The typed-edge graph substrate → futon1a hyperedges with the §3.1 schema
- The constrained execution environment → the Proof Peripheral's
  FALSIFY-before-CONSTRUCT gate (INV-5)
- The canonical form → hyperedge identity patterns with stable IDs
- The real-time capture → ingestion paths (§3.5) that are idempotent and
  repeatable, not one-shot post-hoc reconstruction

The worked example in the monograph (Problem 6 as a 5-node AIF subgraph
with typed edges) is directly representable in the §3.1 schema: each node
is a `post` entity, each edge is an `iatc` relation with an act attribute
(attacks, supports, preference). What the monograph described as future
work, this mission implements.

#### 4.6 Pattern Cross-Reference (Structured Survey)

A systematic search of `futon3/resources/sigils/patterns-index.tsv` (853
patterns, 49 namespaces) against the DERIVE schema identified **82 relevant
patterns** across 14 concern areas. The table below lists the architecturally
significant patterns per concern area with the design element they inform.
Full counts per area follow.

**Cross-column invariants (25 patterns — largest concern area):**

| Pattern | Design Element | Contribution |
|---------|---------------|--------------|
| `devmap-coherence/ifr-f1-dhammavicaya` | INV-1 (documented entry points) | Higher layers cannot inspect "the system model of me" without codified investigation — the invariant enforces this |
| `stack-coherence/evidence-ledger` | INV-2 (PUR trail complete) | "Claims must cite reproducible proofs or they drift into fiction" — exactly what INV-2 checks |
| `code-coherence/dead-code-hygiene` | INV-1 (documented entry points) | Undocumented entry points are the code-hygiene analog of dead code — present but invisible |
| `stack-coherence/commit-intent-alignment` | §3.5 data flow (ingestion paths) | Commits and devmaps must reflect the same movement — cross-column sync |
| `f1/p2` (Invariant Enforcement) | INV-1 through INV-5 | "A deterministic substrate is only trustworthy if invariants are actually enforced" |
| `futon-stack/argument` | §4.3 (invariants as product) | "Coherence requires connection" — the one-line rationale for cross-column invariants |
| `iching/hexagram-61-zhongfu` (Inner Truth) | I4 wireheading test | "Trust emerges when inner state and outward behavior match" — inner=invariants, outer=code |

**Evidence trails & traceability (17 patterns):**

| Pattern | Design Element | Contribution |
|---------|---------------|--------------|
| `agent/trail-enables-return` | Trace browser (§3.6 view 3) | "Trails convert ephemeral navigation into persistent structure" — the trace browser IS this |
| `agent/evidence-over-assertion` | INV-2 (PUR trail) | "Evidence transforms output from 'trust me' to 'check this'" — PUR discipline |
| `musn/use-requires-evidence` | PSR→PUR→evidence chain | "PSR→PUR→evidence is the backbone for logic checking and AIF scoring" |
| `realtime/authoritative-transcript` | XTDB evidence store | All ingestion is durable and append-only; the store is the authoritative record |
| `eight-gates/cai-pluck` (Pluck/Ground) | §3.2 cross-column relations | "Grounding transforms 'trust me' into 'check this'" — cross-column edges are grounding acts |

**Active Inference / AIF (12 patterns):**

| Pattern | Design Element | Contribution |
|---------|---------------|--------------|
| `aif/structured-observation-vector` | Code column entity types (§3.1) | "A stable observation vector is the prerequisite for meaningful precision control" — var envelopes ARE the observation vector |
| `aif/evidence-precision-registry` | Invariant violation severity | "Making precision explicit turns trust into a tunable control surface" |
| `aif/candidate-pattern-action-space` | Tension → resolution action | "Constraining the action space makes scoring meaningful rather than performative" |

**Tension detection & discrepancy (11 patterns):**

| Pattern | Design Element | Contribution |
|---------|---------------|--------------|
| `p4ng/tension-detection` | §3.3 invariant/violation hyperedge | "Making tensions visible is a prerequisite for productive negotiation" — the core principle |
| `realtime/loop-failure-signals` | Invariant violation → tension | A failed invariant IS a loop failure signal; the tension is the signal |
| `realtime/loop-recovery-actions` | Tension → browse → act → resolve | The recovery action: surface violation, let human/agent fix, record fix as evidence |
| `math-informal/parametric-tension-dissolution` | INV-5 (FALSIFY before CONSTRUCT) | "When proof obligations conflict on a parameter, seek a construction that dissolves the tension" |
| `devmap-coherence/prototype-alignment-tension` | §3.6 tension views | "Surfacing tensions and defining readiness windows keep prototypes testable" |

**PSR/PUR discipline (11 patterns):**

| Pattern | Design Element | Contribution |
|---------|---------------|--------------|
| `f3/p1` (PSR/PUR Foundation) | INV-2 (PUR trail complete) | "PSR/PUR discipline is the phenomenological instrument for pattern validation" |
| `musn/selection-before-write` | Pattern→mission linkage | "PSR anchors edits to intent, enabling PUR validation and evidence linking" |
| `realtime/structured-events-only` | §3.3 hyperedge schema | "Stable schema enables accurate parsing, dedupe, and audit trails" |

**Wiring diagrams & composition (7 patterns):**

| Pattern | Design Element | Contribution |
|---------|---------------|--------------|
| `futon-theory/mission-interface-signature` | §3.3 trace/gate-chain hyperedge | "Draw outputs first. Diagram catches gaps tables miss." — validates DERIVE completeness |
| `f5/p2` (Wiring Diagram Semantics) | §4.4 generalization via wiring diagrams | "Wiring diagrams give patterns precise compositional meaning that transfers across domains" |
| `f5/p4` (Wiring Transfer Validation) | §4.5 monograph gap closure | "Proven transfer validates the pattern approach" — MetaCA→ants→self-representing stack |

**Timescale separation / four types (6 patterns):**

| Pattern | Design Element | Contribution |
|---------|---------------|--------------|
| `futon-theory/four-types` | Three-column = pheno/geno/exo projection | "Genotype (internal), phenotype (observable), exotype (connectable), xenotype (transplantable)" — the column decomposition IS the first three types |
| `futon-theory/interface-loop` | Cross-column invariants as interface | "Each layer interface hosts a Baldwin cycle. Interface serves as exotype constraint." — invariants are the interface |
| `futon-theory/local-gain-persistence` | Evidence durability | "Any gain at interface must persist to genotype or be explicitly deleted. No ghost capabilities." |

**Remaining concern areas (counts):**

| Concern Area | Count | Key Patterns |
|-------------|-------|-------------|
| Stack coherence / devmap integrity | 13 | `staleness-scan`, `ready-blocked-triage`, `maturity-evidence-audit` |
| Navigation / browsing | 10 | `f0/p4` (hypertext), `f4/p1` (Arxana), `portal/first-class-query-interface` |
| Schema design / hypergraph | 9 | `f1/p3` (graph schema), `hdm/deep-storage-to-active-graph`, `sidecar/tri-store-separation` |
| Loop closure / feedback | 9 | `p4ng/meta-reflection-loop`, `p4ng/feedback-rhythms`, `realtime/loop-success-signals` |
| Unified stack architecture | 7 | `vsatlas/three-layer-architecture`, `vsatlas/isolarion-drift`, `repository-transition/identity-space` |
| Mission structure | 7 | `futon-theory/mission-scoping`, `futon-theory/mission-dependency`, `p4ng/five-p-rhythm` |
| Self-representation | 2 | `math-informal/the-diagonal-argument`, `futon-theory/four-types` |

**DERIVE revisions prompted by survey:**

1. **`futon-theory/mission-interface-signature`** says "draw outputs first" —
   the §3.3 hyperedge types should be validated against this: what does each
   invariant *output* (not just detect)? Added during VERIFY: each invariant
   should emit a structured violation record, not just a boolean.

2. **`futon-theory/four-types`** confirms the three-column = three-type mapping
   but raises the question: where is the *xenotype* (transplantable) column?
   Answer: the xenotype is the parameterized schema itself (§4.4) — the
   portable structure that transfers to external projects. This is not a fourth
   column but a meta-property of the three-column design.

3. **`futon-theory/local-gain-persistence`** implies INV-2 (PUR trail) should
   also check that evidence entries are *persisted* to futon1a, not just present
   in memory. The VERIFY phase should confirm round-trip for PUR records.

**Gap patterns (should exist, don't yet):**

- **`stack-coherence/cross-column-invariant-registry`** — a canonical list of
  which invariants exist, their status, and last-run results. No existing
  pattern covers this; closest is `evidence-ledger`.
- **`stack-coherence/column-entity-census`** — periodic count of entities per
  column to detect drift (e.g., code column growing while project column stagnates).
- **`aif/wireheading-detection`** — explicit pattern for I4 violations (the
  system rewriting its own preferences). Currently described in this mission's
  §4.2 but not codified as a reusable pattern.

#### 4.7 AIF Dual Usage: Active Inference and Argument Interchange

The futon stack uses "AIF" in two senses that are complementary, not
competing:

**Active Inference Framework** (Friston, Parr & Pezzulo): The
control-theoretic loop — sense → infer → act → evaluate. This is the
engine. It drives the ant simulation (futon2), the agent coordination
(futon3c), the portfolio inference (M-portfolio-inference), and the
proof search strategy (Proof Peripheral). The AIF+ invariants (I1–I6)
are properties of this loop.

**Argument Interchange Framework** (Chesñevar, Reed et al.): The
reasoning structure — claim → attack → support → preference. This is the
content. It describes what the agents *say* as they operate the Active
Inference loop: "I claim X" (node), "this evidence attacks X" (edge),
"I prefer Y over X" (preference). The math column's `iatc` edges
(assert, challenge, clarify, agree) are Argument Interchange types.

The two frameworks compose naturally:
- Active Inference provides the **process** (when to sense, when to act)
- Argument Interchange provides the **structure** (what was claimed, what
  was challenged, what was resolved)
- The self-representing stack captures **both**: the process in the project
  column (missions, evidence, tensions), the structure in the math column
  (posts, discourse edges, scopes)

The First Proof Sprint monograph uses "AIF" for Active Inference
(see glossary in `intro-making-of.tex`) but the concrete node/edge
typing it describes (claim, conflict, preference, attacks, supports)
is Argument Interchange. A future revision of the monograph should
distinguish the two explicitly, as this mission does.

#### 4.8 Plain-Language Argument

We built a system that looks at itself from three angles: the code it's
made of, the plans for what it should become, and the reasoning that
justifies why it's built this way. When any two of these disagree — a
plan says "done" but the code has no documentation, or a proof skips a
step that should have been checked — the system notices and tells you.
Fixing the disagreement produces evidence that the fix happened, which
closes the loop.

This works for our codebase, but the same trick works for any codebase,
any structured reasoning process (a technical audit, a legal review, a
business case), or any project where plans and reality need to stay in
sync. The value isn't the database of facts about the system — it's the
rules that catch when the facts don't add up.

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
| `futon3/holes/holistic-argument-sketch.md` | Stack-level ARGUE seed: S1–S5, A1–A4, potentiality argument |
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

## Relationship to The Holistic Argument

`futon3/holes/holistic-argument-sketch.md` is the seed of a stack-level
ARGUE phase — "what the futon stack is on about, and why it should continue
to exist." It identifies five support relations (S1–S5), four attack
relations (A1–A4), and a five-step chain: work → proof → patterns →
coordination → understanding → **argument** (the missing piece).

This mission addresses several of its gaps directly:

- **S5 (reflexive architecture is rare):** The three-column stack IS the
  reflexive architecture — three projections of the system's own structure,
  checkable against AIF+ invariants. Completing this mission instantiates S5.
- **A4 (the explanation problem):** The three-column decomposition
  (knowledge creation, development process, code) with AIF+ wiring diagrams
  as the unifying substrate is a more legible framing than "Active
  Inference-based reflexive development environment with flexiformal
  proofwork..." — it gives people something concrete to point at.
- **Step 4 → Step 5 (understanding → argument):** The holistic sketch notes
  that M-self-representing-stack delivers step 4 (understanding) but step 5
  (argument) is missing. Cross-column invariants are a form of argument:
  they are structural claims about the system that can be checked, violated,
  and resolved. The invariant→tension→resolve loop is machine-checkable
  argumentation.

What this mission does NOT address (for future work):
- The entelechy framing (Driesch) — what organizing principle guides the
  stack's morphogenesis. The AIF+ invariants (I1–I6) are candidates: they
  constrain what forms are viable. The reverse morphogenesis vocabulary
  (象/香/←, `futon6/holes/handoffs/question-asking-as-reverse-morphogenesis.md`)
  applies: the stack's form (象) is observable in the three columns; the
  entelechy is the ← reading (what constraints make this form stable).
- The Agamben potentiality argument (not-not-doing) — already sketched,
  needs integration with the evidence this mission produces.
- The commercial argument (S4) — this mission builds the infrastructure;
  the commercial case is a follow-on.

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
