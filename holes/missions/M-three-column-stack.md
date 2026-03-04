# Mission: The Three-Column Stack

**Date:** 2026-03-03
**Status:** IDENTIFY
**Blocked by:** None (M-self-representing-stack proof of concept complete,
futon1a hyperedge API operational, core.logic foundation in place)
**Owner:** futon4 (Arxana), with dependencies on futon3c (Mission Control,
core.logic), futon1a (hyperedge store), futon6 (math content)

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

**Project ↔ Math (cross-domain):**
- Every pattern in the library that references a mathematical concept has a
  corresponding term entity with a PlanetMath or equivalent grounding
- Mathematical conjectures referenced in mission docs are tracked as
  open/resolved

**Code ↔ Code (internal consistency):**
- Every protocol has at least one implementation in a loaded namespace
- No circular namespace dependencies (or explicitly documented exceptions)
- Every `defmethod` dispatch value corresponds to a known entity type

These invariants are enforceable via core.logic on the Clojure side (Layer 2
of the three-layer logic architecture from M-self-representing-stack). When an
invariant fails, it emits a tension. The tension browser surfaces it. A human
or agent proposes a fix. The fix emits evidence. The loop closes.

### Generalization Path

The entity types, relation types, and invariant rules are parameterized —
not hardcoded to futon.

- `var:<ns>/<symbol>` works for any Clojure project
- Java reflection (`/reflect/java/:class`) extends the same model to Java
- `covers`, `evidences`, `about-var` work for any project with components
  and missions
- Post/scope/expression/term types work for any structured reasoning process

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
- [ ] Review with Joe — does the framing match the vision?
- [ ] Identify any missing columns or invariant categories
- [ ] Confirm completion criteria are testable and sufficient

### 2. MAP

Survey the current state of all three columns.

- [ ] **Q1 (math column schema):** Full attribute inventory per node/edge type
  in `thread-633512-hypergraph.json`. Identity patterns. Any additional JSON
  files beyond thread-633512. Catalog what `arxana-browser-hypergraph.el`
  currently does with each type.
- [ ] **Q2 (code column schema):** What the reflection API can produce today —
  namespaces, vars, deps, protocols, defmethods. Complete attribute shapes.
  What's queryable vs what needs new endpoints.
- [ ] **Q3 (project column inventory):** 21 hyperedges now persisted in XTDB
  (10 devmap, 9 tension, 1 reflection, 1 test — restored 2026-03-03). 73
  mission backfill entries in evidence store. 9 tension entries via MC API.
  10 devmap summaries. Inventory what can be re-ingested from existing APIs
  without new work.
- [ ] **Q4 (core.logic relations):** Assess `devmapo`, `componento`, `coverso`,
  `hyperedgeo`, `invarianto`, `implementedo` — which are operational vs stubs,
  what query patterns they support.
- [ ] **Q5 (browser migration):** Gap analysis: what does
  `arxana-browser-hypergraph.el` expect from JSON vs what futon1a's
  `GET /hyperedges` returns? What adapter code is needed?
- [ ] **Q6 (implicit invariants):** Survey existing linting rules, test coverage
  requirements, naming conventions, CI checks. These are candidate
  cross-column invariants that could be formalized.
- [ ] **MAP summary:** What's ready, what's missing, what's the critical path.

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

Synthesis drawing on the theoretical anchoring from M-self-representing-stack
(AIF+ organism, Higgins' self-discrepancy, reflexivity loop, frozen dynamics,
homoiconicity) plus the commercial generalization argument.

- [ ] Why three columns and not two or four
- [ ] Why cross-column invariants are the product (not the data)
- [ ] How this generalizes beyond futon (the commercial argument)
- [ ] Pattern references (which futon3 patterns inform the design)

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
- [ ] Project↔Math loop: e.g., "pattern references concept without term
  grounding" → tension → add grounding → resolves
- [ ] Write up the demo as a reproducible walkthrough

## Source Material

| Source | What We Take |
|--------|-------------|
| M-self-representing-stack (this repo) | Proof of concept, theoretical anchoring, 21-hyperedge schema |
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
