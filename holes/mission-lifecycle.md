# Futonic Mission Lifecycle

A futonic mission is a scoped unit of work that moves the stack from a
known state to a better one. Missions follow a 6-phase derivation path
that progresses from naming a gap to demonstrating a working result.

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

**Exit criterion:** Someone could implement the mission from the DERIVE
section alone, without needing to ask clarifying questions.

### 4. ARGUE

Synthesize. Why is this design *right*, not just *workable*?

- [ ] **Pattern references:** Which patterns from the library (futon3/library)
  informed the design? How?
- [ ] **Theoretical coherence:** Does the design serve the theoretical
  anchoring from IDENTIFY? Or has the theory shifted?
- [ ] **Trade-off summary:** What did we give up and why?
- [ ] **Generalization notes:** Does this design work beyond the immediate
  context? What would need to change to apply it elsewhere?

The ARGUE phase is short — typically a few paragraphs. Its purpose is to
make the design defensible and to catch misalignments between theory and
implementation before code is written.

**Exit criterion:** The design feels *inevitable* given the constraints,
not merely *possible*.

### 5. VERIFY

Build it and prove it works.

- [ ] **Implement:** Write the code, create the endpoints, wire the views.
- [ ] **Round-trip test:** For each new entity/relation type, confirm that
  write + read-back produces the same data.
- [ ] **Integration test:** Confirm that the new structure works with existing
  infrastructure (browsers, APIs, logic relations).
- [ ] **Completion criteria check:** Go through each criterion from IDENTIFY
  and confirm it's met with evidence (not assertion).
- [ ] **Decision log:** Record any implementation-time decisions that diverged
  from DERIVE, with rationale.

**Exit criterion:** Every completion criterion has a concrete demonstration.

### 6. INSTANTIATE

Demonstrate the full loop in the real system.

- [ ] **End-to-end demo:** Walk through a realistic scenario that exercises
  the new capability from start to finish.
- [ ] **Loop closure:** If the mission involves a feedback loop (invariant →
  tension → action → resolution), demonstrate the full cycle.
- [ ] **Deferred items:** Explicitly list anything that was discovered during
  VERIFY/INSTANTIATE but belongs to a follow-on mission.
- [ ] **Checkpoint:** Write the final checkpoint in the mission doc with
  commits, what was built, and what remains.

**Exit criterion:** A new person (human or agent) could reproduce the demo
from the mission doc without help.

## Mission Lifecycle States

```
IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE → COMPLETE
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
