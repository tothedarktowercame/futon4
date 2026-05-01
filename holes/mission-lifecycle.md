# Futonic Mission Lifecycle

A futonic mission is a scoped unit of work that moves the stack from a
known state to a better one. Missions follow a 7-phase derivation path
that progresses from naming a gap to demonstrating a working result and
documenting it in the living system.

This document is a reusable reference — not specific to any mission.

## Phases

### 1. IDENTIFY

Name the gap. Why does this mission exist?

- [ ] **Motivation:** What discrepancy between ideal and actual prompted this?
- [ ] **Theoretical anchoring:** What patterns, principles, or prior work
  inform the approach? (AIF+, self-discrepancy, pattern theory, etc.)
- [ ] **Scope in/out:** What's included, what's explicitly deferred?
- [ ] **Completion criteria:** Testable conditions — how will we know it's done?
- [ ] **Relationship to other missions:** What does this depend on, what does
  it enable?
- [ ] **Source material:** Files, APIs, prior work that feed into this mission.
- [ ] **Owner and dependencies:** Which repos are involved, who drives?

The IDENTIFY document IS the mission file (`M-<name>.md`). Everything else
accretes onto it.

**Exit criterion:** A human has read the proposal and agrees the gap is real
and the scope is right.

#### Optional: shape-first IDENTIFY

When the mission's gap is an *invariant* (or a candidate one), IDENTIFY
can have a more formal character. Before progressing to MAP, ask:
**"what shape is this one instance of?"** If the candidate invariant is
naturally one of several siblings under a single protocol-shape, name
the shape, enumerate at least 2-3 plausible instances, and adopt
namespace IDs (`<shape>/<instance>`) in the inventory. If only one
instance ever surfaces, that absence of generalisation is itself a
finding — record it explicitly as `:special-case true`.

This is a methodological move that produces sibling-namespaces of
invariants rather than narrow one-offs, and dogfoods the futon stack's
existing `family → invariants` shape without inventing new structural
elements. Worked example: `M-archaeology-control` IDENTIFY surfaces
the subsumption-witness shape (artifact A obsolete relative to record
P) with siblings `obsolescence-recognition/{autostash, deferred-stub,
pipeline-tracer, branch-merged, ...}`.

Reference patterns:
- `futon3/library/invariant-coherence/shape-first-identify.flexiarg`
- `futon3/library/invariant-coherence/subsumption-witness.flexiarg`
- `futon3/library/invariant-coherence/protocol-family-naming.flexiarg`

### 2. MAP

Survey what exists. Don't design yet — just look.

- [ ] **Inventory existing infrastructure:** What APIs, data stores, browser
  views, logic relations already exist that this mission will use?
- [ ] **Inventory existing data:** What entities, evidence, hyperedges are
  already in the system?
- [ ] **Identify ready vs missing:** Produce a two-column table: "ready (no
  new code needed)" vs "missing (the actual work)."
- [ ] **Answer survey questions:** Each mission defines Q1–Qn questions in its
  MAP section. Answer them with concrete findings, not speculation.
- [ ] **Document surprises:** Anything discovered that changes the scope or
  approach — record it now, before DERIVE locks in the design.

The MAP phase is research. It produces facts, not decisions. Read code, call
APIs, count things, check shapes.

**Exit criterion:** Every MAP question has a concrete answer. The "ready vs
missing" table is complete.

### 3. DERIVE

Design the solution. This is the core intellectual work.

- [ ] **Entity types:** What things will exist in the system? Identity patterns,
  source, ingested/derived/authored.
- [ ] **Relation types:** How do things connect? Binary relations for common
  cases, hyperedges for genuinely n-ary connections.
- [ ] **Invariant rules:** What must be true across the system? Express as
  checkable propositions (core.logic relations, queries, or assertions).
- [ ] **Data flow:** Which systems produce which entities, how they reach
  storage, how they're queried.
- [ ] **IF/HOWEVER/THEN/BECAUSE:** Every non-obvious design decision gets an
  inline justification. "IF X is true, HOWEVER Y complicates it, THEN we
  choose Z, BECAUSE of reason R." This prevents revisiting settled decisions
  and documents the trade-offs for future readers.
- [ ] **View/UI specifications:** What browser views, commands, or interfaces
  will the user interact with?
- [ ] **Wiring diagram (if applicable):** If a futon5 AIF+ exotype diagram
  exists or would be useful, sketch it now. Diagrams settle ports, timescales,
  exogeneity, and closure before code hardens. Not every mission needs one,
  but missions that define components, loops, or multi-repo interfaces benefit
  from having the diagram during DERIVE rather than discovering its absence
  during VERIFY.
- [ ] **Fidelity contract (GF, if applicable):** For port/rebuild missions,
  inventory donor capabilities and produce a Capability Preservation Matrix
  (preserve/adapt/drop) with tripwire tests. For greenfield missions,
  inventory declared spec commitments. See `futon3c/docs/futonic-missions.md`
  §GF for the full template. Not required for pure research or exploration
  missions, but required for any mission that replaces or extends existing
  behavior.
- [ ] **PSR (optional, per non-obvious design choice):** When the design
  selects a pattern from `futon3/library/`, write a Pattern Selection Record
  *before* committing to it. Format and storage in §PSR/PUR Discipline below.
  Optional but strongly encouraged when the choice is non-obvious or the
  pattern is being used in a new way.

**Exit criterion:** Someone could implement the mission from the DERIVE
section alone, without needing to ask clarifying questions.

### 4. ARGUE

Synthesize. Why is this design *right*, not just *workable*?

- [ ] **Pattern cross-reference:** Search `futon3/library/` for patterns
  relevant to the DERIVE design. For each pattern that applies, record:
  which pattern, where it applies in the design, and how. This is a
  structured survey, not a post-hoc decoration — patterns you discover
  here may revise the DERIVE design. PSRs collected during DERIVE feed
  directly into this cross-reference; if PSRs were skipped, this is the
  catch-up moment to write them.
- [ ] **Theoretical coherence:** Does the design serve the theoretical
  anchoring from IDENTIFY? Or has the theory shifted?
- [ ] **Trade-off summary:** What did we give up and why?
- [ ] **Generalization notes:** Does this design work beyond the immediate
  context? What would need to change to apply it elsewhere?
- [ ] **Plain-language argument:** After the technical synthesis, write
  a short (3–5 sentence) version of the argument that uses no jargon.
  If you can't explain it simply, the design may be more complex than
  it needs to be. This also serves as the "elevator pitch" for the
  mission's contribution.

The ARGUE phase produces two things: the technical synthesis (pattern
references, coherence checks, trade-offs) and the plain-language argument.
Both are necessary. The technical synthesis makes the design defensible;
the plain-language version makes it communicable.

**Exit criterion:** The design feels *inevitable* given the constraints,
not merely *possible*. And someone outside the project can understand
what it does and why from the plain-language argument alone.

### 5. VERIFY

Check the architecture against constraints before committing to full
implementation. This phase is primarily about structural and empirical
validation — confirming the design is sound before code hardens.

- [ ] **Structural verification (if wiring diagram exists):** Check the
  architecture against exotype diagrams (`.edn`). Verify: completeness,
  coverage, no orphan inputs, type safety, spec coverage, timescale
  ordering, exogeneity, and compositional closure. If no diagram exists,
  record why (not every mission needs one) and skip this check.
- [ ] **Prototype / spike (if needed):** For missions where structural
  verification alone is insufficient, build a minimal spike to validate
  the riskiest DERIVE commitments. This is not full implementation — it's
  targeted risk reduction.
- [ ] **Completion criteria pre-check:** Review each criterion from IDENTIFY
  and confirm the DERIVE design addresses it. Flag any that the design
  doesn't cover — these need either a DERIVE revision or an explicit
  deferral.
- [ ] **Fidelity check (if GF was produced):** Confirm tripwire tests exist
  for all `preserve` capabilities and compatibility assertions for all
  `adapt` capabilities.
- [ ] **Decision log:** Record any verification-time discoveries that revise
  the DERIVE design, with rationale.
- [ ] **PUR (optional, per pattern application during a spike):** If
  VERIFY-time spikes apply patterns from `futon3/library/`, record a
  Pattern Use Record per application — outcome, prediction error,
  surprises. PURs are how patterns get validated or revised; even
  short ones are valuable.

**Exit criterion:** The design has been checked against available structural
constraints. Any risks that can't be verified statically have been spiked.
DERIVE revisions (if any) are recorded.

### 6. INSTANTIATE

Build the code. By this point, the architecture is fully specified, the
patterns are selected, the argument is written, and the constraints are
checked. Implementation should be the *least creative* phase — if it
requires novel design decisions, the earlier phases were incomplete.

- [ ] **Implement:** Write the code, create the endpoints, wire the views.
- [ ] **Round-trip test:** For each new entity/relation type, confirm that
  write + read-back produces the same data.
- [ ] **Integration test:** Confirm that the new structure works with existing
  infrastructure (browsers, APIs, logic relations).
- [ ] **Completion criteria check:** Go through each criterion from IDENTIFY
  and confirm it's met with evidence (not assertion).
- [ ] **End-to-end demo:** Walk through a realistic scenario that exercises
  the new capability from start to finish.
- [ ] **Loop closure:** If the mission involves a feedback loop (invariant →
  tension → action → resolution), demonstrate the full cycle.
- [ ] **Deferred items:** Explicitly list anything that was discovered during
  VERIFY/INSTANTIATE but belongs to a follow-on mission.
- [ ] **Checkpoint:** Write the final checkpoint in the mission doc with
  commits, what was built, and what remains.
- [ ] **PUR (optional but expected for pattern-driven implementation):**
  This is where the bulk of PURs land. For each pattern selected via
  PSR (or applied without a prior PSR), write a Pattern Use Record after
  the pattern is in place: outcome (success / partial / failure),
  prediction errors, surprises, and any revisions to the pattern itself
  this exercise suggests. Skip only when the implementation is purely
  mechanical (no pattern was actually applied — e.g. a one-line typo
  fix or a rote dependency bump).

**Exit criterion:** Every completion criterion has a concrete demonstration.
A new person (human or agent) could reproduce the demo from the mission doc
without help.

### 7. DOCUMENT

Make the mission's contribution navigable in the living documentation.

- [ ] **Docbook entries:** Create or update docbook entries (futon3x or
  appropriate book) that explain what was built, how to use it, and how it
  connects to prior work. Entries should be self-contained — readable without
  the mission doc.
- [ ] **Cross-references:** Link new entries to existing docbook entries, trace
  browser views, and other navigable surfaces. Use `docbook://` URIs for
  intra-book links.
- [ ] **Browser integration:** If the mission produced new browser views or
  data, verify they appear in `M-x arxana-browse` and are reachable from
  existing navigation paths (menu items, trace paths, etc.).
- [ ] **Deferred-item tickets:** Any deferred items from INSTANTIATE that
  warrant future missions should be noted in the documentation with enough
  context for a new agent to pick them up.

The DOCUMENT phase produces durable, navigable artifacts — not the mission doc
itself (which is the working record), but the documentation that lives in the
system's self-representing surface. The mission doc says what was done and why;
the docbook entries say what exists and how to use it.

**Exit criterion:** Someone browsing the docbook can discover what this mission
built without knowing the mission exists. The documentation is findable via
navigation, not just via grep.

## Mission Lifecycle States

```
IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE → DOCUMENT → COMPLETE
                                                                        ↓
                                                                    RE-OPENED
                                                                    (if gaps found)
```

A mission may also be:
- **BLOCKED:** Waiting on another mission or external dependency.
- **DEFERRED:** Scope was right but timing is wrong.
- **NONSTARTER:** The gap turned out not to be real, or the approach is wrong.

## Conventions

- Mission files live in `<repo>/holes/missions/M-<name>.md`.
- Status line at the top: `**Status:** <PHASE> (date)`.
- Checkpoints are appended (never overwrite prior phases).
- Evidence is emitted to futon1a during VERIFY/INSTANTIATE.
- Completed missions become source material for future missions.
- Each phase accretes — the mission doc grows as phases complete.

## PSR/PUR Discipline

PSR (Pattern Selection Record) and PUR (Pattern Use Record) are the
mechanism by which the pattern library evolves alongside the missions
that consume it. They are **optional per phase** — the per-phase
checklists call out where they typically land — but they are **strongly
encouraged whenever a non-trivial design choice references a pattern**.

### When to write them

| Phase | PSR? | PUR? | Note |
|---|---|---|---|
| IDENTIFY | – | – | Patterns named here are theoretical anchoring, not selection |
| MAP | – | – | MAP is research; no design decisions yet |
| DERIVE | **yes (optional)** | – | Per non-obvious pattern selection |
| ARGUE | catch-up | – | Cross-reference step is the safety net |
| VERIFY | – | **yes** if a spike applied a pattern | |
| INSTANTIATE | – | **yes (expected)** | The bulk of PURs land here |
| DOCUMENT | – | – | PURs become source material for docbook entries |

Skip both only when the work is purely mechanical (typo fix, rote
dependency bump, generated-code regeneration). For everything else, at
least one PUR is the floor.

### Where they live

```
<repo>/holes/labs/<mission-name>/psr/<YYYY-MM-DD>__<phase>__<topic>.md
<repo>/holes/labs/<mission-name>/pur/<YYYY-MM-DD>__<phase>__<topic>.md
```

The labs directory is per-mission; one mission may produce many PSR/PUR
pairs across its phases. Keep filenames sortable by date.

### Format

PSR (write *before* committing to a pattern):

```markdown
# PSR: <short title>
context: <what the situation is, why a pattern is needed>
patterns: <which patterns from futon3/library/ were considered>
decision: <which one was selected, and any sub-choices>
alternatives: <what else was considered, why rejected>
outcome (target): <what success looks like for this application>
confidence: <high / medium / low + rationale>
```

PUR (write *after* applying a pattern):

```markdown
# PUR: <short title matching the PSR if one exists>
pattern (re-confirmed): <which patterns were actually used>
actions taken: <what code/structure changed>
outcome: <success / partial / failure, with evidence>
prediction errors: <where the PSR's expectations differed from reality>
invariants verified: <if applicable, what was checked and how>
connections: <links to other missions, follow-on work, library revisions>
```

### Why optional, not mandatory

PSR/PUR adds friction. For substrate-spanning work, novel pattern
applications, or any decision that future agents would otherwise have
to re-derive ("why was this built this way?"), the friction is bought
back many times over. For trivial work, it's bureaucracy. Mission
authors decide per phase; the lifecycle gives them visible hooks rather
than mandating compliance.

### Exemplar

`futon3/holes/labs/M-live-geometric-stack/{psr,pur}/2026-04-27__phase-1__edge-taxonomy-lift.md`
is the inaugural full-discipline pair after this revision (substrate-2
phase 1, 2026-04-27). Earlier exemplars from the futon1a rebuild are at
`futon3/holes/labs/futon1a/{psr,pur}/`.

## Relationship to Other Processes

- **Portfolio inference** (M-portfolio-inference) decides *which* mission to
  work on next. The mission lifecycle says *how* to work on it.
- **Patterns** (futon3/library) inform DERIVE decisions. Pattern selection
  and use are recorded via PSR/PUR per the discipline above; PURs feed
  back into the library as evidence for pattern revision.
- **Evidence landscape** (futon1a) stores the trail. Mission artifacts become
  queryable evidence.
- **Mission Control** (futon3c) tracks mission status, computes coverage,
  and generates tensions. Tensions can prompt new missions.
