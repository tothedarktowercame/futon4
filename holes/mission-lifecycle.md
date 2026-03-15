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

**Exit criterion:** Someone could implement the mission from the DERIVE
section alone, without needing to ask clarifying questions.

### 4. ARGUE

Synthesize. Why is this design *right*, not just *workable*?

- [ ] **Pattern cross-reference:** Search `futon3/library/` for patterns
  relevant to the DERIVE design. For each pattern that applies, record:
  which pattern, where it applies in the design, and how. This is a
  structured survey, not a post-hoc decoration — patterns you discover
  here may revise the DERIVE design.
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

## Relationship to Other Processes

- **Portfolio inference** (M-portfolio-inference) decides *which* mission to
  work on next. The mission lifecycle says *how* to work on it.
- **Patterns** (futon3/library) inform DERIVE decisions. Pattern use is
  recorded via PSR/PUR during VERIFY.
- **Evidence landscape** (futon1a) stores the trail. Mission artifacts become
  queryable evidence.
- **Mission Control** (futon3c) tracks mission status, computes coverage,
  and generates tensions. Tensions can prompt new missions.
