# Futonic Mission Lifecycle — Institutional Variant

A futonic mission, in the institutional variant, is a scoped unit of *organisational change* that moves an organisation from a known capability state to a better one. Missions may begin with the same optional `HEAD` bootstrap as the agentic-coding lifecycle, then follow the same 7-phase derivation path; the artefacts at each phase are organisational — governance documents, capability portfolios, stakeholder commitments, sequenced rollout plans — rather than code.

This document is a reusable reference. It is the institution-level sibling of `mission-lifecycle.md`. The `HEAD` bootstrap (when used), the 7 phases, and their exit criteria are the same; the inputs and outputs are translated into organisational register.

The variant exists because the agentic-coding lifecycle's MAP, DERIVE, VERIFY, INSTANTIATE, and DOCUMENT phases name code-substrate artefacts (APIs, entity types, browser views, docbook entries) that institutional missions don't have. Renaming each phase's checklist into capability/stakeholder/governance terms makes the lifecycle directly usable by an organisation reading and implementing the mission, not just by an agent writing code.

## When to use this variant

- The mission's substrate is an organisation, not a codebase.
- The mission's artefacts are governance documents, capability commitments, training portfolios, partnership agreements, stakeholder routines.
- Implementation is carried out by named organisational actors (programme managers, training leads, board members) rather than by code agents.
- Hand-off between mission authors and mission implementers is expected — typically at INSTANTIATE.

## Phases

### HEAD (optional bootstrap before IDENTIFY)

Institutional missions can also begin with `HEAD` when the work starts
from a live operator or stakeholder sense that *something important is
here* but the formal gap statement is not yet ready. In the
institutional register, `HEAD` typically captures the initiating voice,
the lived sense of what the organisation is trying to preserve or make
legible, the anti-rhetorical discipline the mission must obey, and the
named tensions that later phases will have to resolve rather than hide.

Use the same `HEAD` discipline as the agentic-coding variant. If the
mission already has a crisp gap statement, skip `HEAD` and begin at
IDENTIFY.

### 1. IDENTIFY

The primary work of IDENTIFY is to *identify a gap* — what the current state cannot do that the desired state requires. Knowing what predecessor programmes achieved is relevant; knowing what they could not establish on their own evidence is more relevant. The IDENTIFY motivation should foreground what is *missing*, not what is carried forward.

The recommended way of stating the gap is as one or more **`sorry`s** (in the Lean sense): proof obligations that the predecessor or current state cannot discharge on its existing evidence. Each `sorry` is a specific claim that the mission has to handle. By VERIFY exit, every named `sorry` has a status:

- *Closed*, with the evidence trail that discharges it recorded in the mission, OR
- *In closure*, with a named action (typically scheduled for INSTANTIATE) that produces the missing evidence, OR
- *Deferred*, with a follow-on mission scoped to address it.

A `sorry` left silent is a mission failure. The Lean-sorry framing makes gap-closure auditable: every gap claim either gets discharged, gets a closure plan, or gets explicitly deferred. None can be quietly dropped.

#### Checklist

- [ ] **The gap.** Stated as one or more `sorry`s. Each `sorry` is a specific proof obligation the predecessor / current state cannot discharge on its existing evidence. State each in plain language; the test is that someone outside the project can read the `sorry` and recognise it as a real gap, not a rhetorical move.
- [ ] **Theoretical anchoring.** What patterns, principles, or prior organisational work inform the approach? Each framing named here is treated as *falsifiable* by the verification work — never as a foundational commitment. *Set aside* verdicts on candidate framings are allowed and are findings in their own right.
- [ ] **Scope in/out.** What's included; what's explicitly deferred to future missions. Closure *plans* for `sorry`s are usually in scope; closure *of* `sorry`s that depend on fresh evidence is usually deferred to a follow-on mission.
- [ ] **Completion criteria.** Testable conditions — how the mission's owners will know it is done. Each `sorry` having a recorded status is one of the standing criteria; verdicts on candidate framings is another.
- [ ] **Relationship to other missions.** What does this depend on (other organisational changes, prior commitments)? What does it enable (downstream capabilities, follow-on missions for deferred `sorry`s)?
- [ ] **Source material.** Reports, evaluations, surveys, prior programmes that feed into this mission. The mission's source material is *not* the mission's output; if a working paper or essay is the mission's *briefing*, name it as such.
- [ ] **Owner and dependencies.** Which parts of the organisation are involved, who drives, who decides? For institutional missions, name both the writing team and the implementing team explicitly; the hand-off contract sits at VERIFY exit.

The mission file (`M-<name>.md`) begins no later than `HEAD`; if `HEAD`
is skipped, IDENTIFY is the first required authored section. Everything
else accretes onto it.

**Exit criterion:** The mission's owners have read the proposal and agree the gap is real, the named `sorry`s are correctly stated, and the scope is right. Where the mission spans multiple organisational units, every named owner has agreed.

#### Anti-pattern: recap-and-reframe IDENTIFY

A common drift is to write IDENTIFY as a recap of what predecessor programmes produced, then reframe those outputs as the mission's recommendations. This obscures the gap. The mission appears to have a starting point (the predecessor outputs) and an ending point (recommendations) but no actual *gap* — and therefore no proof obligations to discharge.

If the IDENTIFY motivation paragraph reads as a summary of prior work, the gap is probably under-identified. Re-write so that the predecessor's *unmet obligations* are the centre, not the predecessor's *outputs*.

A worked example of the recap-and-reframe failure mode and its rewrite is the IDENTIFY draft 1 → draft 2 transition for `M-or-training-as-learning-system` (this directory). Draft 1 led with predecessor outputs and candidate framings; draft 2 leads with four named `sorry`s and treats the candidate framings as items the mission has to verdict on. The contrast between the two drafts is itself a load test of this guidance.

### 2. MAP

Survey what exists. Don't design yet — just look.

- [ ] **Inventory existing capabilities:** What programmes, services, instruments, materials, or skill bases already exist that this mission will use? (For ports from a predecessor programme: what will be inherited?)
- [ ] **Inventory existing relationships:** Which institutions, partners, communities of practice, or stakeholder groups are already engaged? With whom is there standing trust, an established channel, or a prior agreement?
- [ ] **Inventory existing evidence:** What evaluations, surveys, case studies, or operational records bear on the mission? What does the evidence say, and where is it weakest?
- [ ] **Identify ready vs missing:** Produce a two-column table — *ready* (capabilities/relationships/evidence already in place) vs *missing* (the actual organisational work the mission has to do). Cross-reference each "missing" entry to the IDENTIFY `sorry`s where applicable: the missing column is where IDENTIFY's proof obligations acquire concrete substrate. If a `sorry` has no corresponding entry in "missing", either the `sorry` is mis-stated or the survey has not yet found the substrate the closure work needs.
- [ ] **Answer survey questions:** Each mission defines Q1–Qn questions in its MAP section. Answer with concrete findings, not speculation.
- [ ] **Document surprises:** Anything found during MAP that changes scope or approach — record it now, before DERIVE locks in the design.

The MAP phase is research. It produces facts about the organisation's current state, not decisions about the future. Read prior reports, talk to stakeholders, count things, check what the evidence actually says.

**Exit criterion:** Every MAP question has a concrete answer. The "ready vs missing" table is complete.

### 3. DERIVE

Design the response. This is the core intellectual work.

- [ ] **Roles and responsibilities:** What named actors will exist in the design? What does each role do? Who carries which capability?
- [ ] **Coordination structures:** How do roles connect? Where are the standing channels, the meetings, the decision rights? Where are the boundaries between organisational units?
- [ ] **Invariant principles:** What must be true across the organisation for the design to be operational? Express as checkable propositions ("every training session has a feedback artefact attached"; "every commissioned course has a named owner with allocated time"). These are the organisational analogues of code-level invariants.
- [ ] **Resource flow:** Which organisational units produce which outputs (training, evaluation, materials, governance), how do those outputs reach the people who use them, and how does feedback get back to producers?
- [ ] **IF/HOWEVER/THEN/BECAUSE:** Every non-obvious design decision gets an inline justification. "IF X is true of the organisation, HOWEVER Y complicates it, THEN we choose Z, BECAUSE of reason R." This prevents revisiting settled decisions and documents the trade-offs for future readers (including future mission authors picking up where this one left off).
- [ ] **Stakeholder-facing artefacts:** What will partners, participants, funders, or member institutions actually see and interact with? Training-portfolio documents, partnership agreements, evaluation forms, communication routines.
- [ ] **Wiring diagram (if applicable):** Where the mission has architectural structure (e.g., the MVSG framing in the npt working paper), sketch the diagram now. Diagrams settle roles, channels, and boundaries before commitments harden.
- [ ] **Fidelity contract (GF, if applicable):** For missions that *inherit* from a predecessor programme (not greenfield), inventory donor capabilities and produce a Capability Preservation Matrix (preserve / adapt / drop) with named tripwires. The npt WP's "What UKRN-S inherits" / "What UKRN-S can drop" sub-sections are an example of this work.
- [ ] **PSR (optional, per non-obvious pattern selection):** When the design selects a pattern from the relevant pattern library (e.g., `futon3/library/system-coherence/`), write a Pattern Selection Record before committing. Optional but strongly encouraged for non-obvious choices.

**Exit criterion:** A reader could implement the mission from the DERIVE section alone, without needing to ask the mission authors clarifying questions. For institutional missions, this means: the implementing team could pick up the document and start the work without the authors in the room.

### 4. ARGUE

Synthesize. Why is this design *right*, not just *workable*?

- [ ] **Pattern cross-reference:** Search the relevant pattern library for patterns that bear on the DERIVE design. For each pattern that applies, record: which pattern, where it applies in the design, and how. PSRs collected during DERIVE feed directly into this; if PSRs were skipped, this is the catch-up moment.
- [ ] **Theoretical coherence:** Does the design serve the theoretical anchoring named in IDENTIFY? Or has the theory shifted during MAP/DERIVE — and if so, has the shift been recorded?
- [ ] **Trade-off summary:** What did the design give up, and why?
- [ ] **Generalisation notes:** Does the design work beyond the immediate context? What would need to change to apply it to a sibling organisation (other UK reproducibility-network actors, equivalent overseas networks, future successor programmes)?
- [ ] **Plain-language argument:** After the technical synthesis, write a short (3–5 sentence) version of the argument that uses no jargon. If you can't explain it simply, the design may be more complex than it needs to be. The argument should make clear which `sorry`s the design closes and which it defers — at this register, with no rhetoric. This is also the elevator pitch for the mission's contribution.

The ARGUE phase produces two things: the synthesis (pattern references, coherence checks, trade-offs) and the plain-language argument. Both are necessary. The synthesis makes the design defensible; the plain-language version makes it communicable to the institutional readers who will actually implement it.

**Exit criterion:** The design feels *inevitable* given the constraints, not merely *possible*. And someone outside the project — a board member, a funder, an external reviewer — can understand what it does and why from the plain-language argument alone.

### 5. VERIFY

Check the design against constraints before committing to full organisational implementation. This phase is structural and empirical validation — confirming the design is sound before commitments harden.

- [ ] **Structural verification (if wiring diagram exists):** Check the design against the diagram. Are all named roles populated? Are the standing channels actually present in the organisation, or are they being assumed? If a population model or simulation supports the design (e.g., the cyber-ant simulation in the npt WP), check the design's claims against the model's results.
- [ ] **Scenario test (if needed):** For missions where structural verification alone is insufficient, walk through one or two named-stakeholder scenarios in detail. *"What does institutional lead X actually do under this design when situation Y arises?"* This is the institutional analogue of a code spike — targeted risk reduction, not full implementation.
- [ ] **Completion criteria pre-check:** Review each criterion from IDENTIFY and confirm the DERIVE design addresses it. Flag any that the design doesn't cover — these need either a DERIVE revision or an explicit deferral.
- [ ] **`sorry` status check:** For each `sorry` named in IDENTIFY, confirm it has a recorded status — *closed*, *in closure*, or *deferred* — with the closure mechanism named. Sorries left silent are a VERIFY-blocker; either the design must close them, the bootstrap must produce the evidence that does, or a follow-on mission must be scoped.
- [ ] **Candidate-framing verdicts:** For each candidate framing named in IDENTIFY's theoretical anchoring, confirm the design's verdict — *robust*, *robust under stated conditions*, *revised*, *set aside* — with the empirical basis named.
- [ ] **Fidelity check (if GF was produced):** Confirm tripwires exist for all `preserve` capabilities. If a predecessor programme's capability is being inherited, name the operational test that confirms it survived the inheritance.
- [ ] **Decision log:** Record any verification-time discoveries that revise the DERIVE design, with rationale.
- [ ] **PUR (optional, per pattern application during a scenario):** If VERIFY-time scenario walks apply patterns, record a Pattern Use Record per application — outcome, prediction error, surprises. Even short ones are valuable.

**Exit criterion:** The design has been checked against the available structural and empirical constraints. Any organisational risks that can't be verified statically have been scenario-tested. DERIVE revisions (if any) are recorded.

**Hand-off note.** For missions where the writing team is distinct from the implementing team (typical for institutional missions where authors are external to the implementing organisation), VERIFY is the last phase the writers carry. INSTANTIATE and DOCUMENT pass to the implementing organisation. The hand-off contract is: VERIFY closes with the design implementable by a competent reader; INSTANTIATE belongs to the team that owns the substrate.

### 6. INSTANTIATE

Implement. By this point, the design is fully specified, the patterns are selected, the argument is written, and the constraints are checked. Implementation should be the *least creative* phase — if it requires novel design decisions, the earlier phases were incomplete.

- [ ] **Implement:** Stand up the named roles. Convene the named meetings. Publish the named documents. Start the named programmes.
- [ ] **Round-trip test:** For each new role/responsibility/relationship, confirm that the design's intent is realised in the organisational reality (not just on paper).
- [ ] **Integration test:** Confirm that the new structure works with existing organisational infrastructure (other programmes, partner relationships, governance routines). Where there are interfaces with prior commitments, confirm those interfaces hold.
- [ ] **Completion criteria check:** Walk through each IDENTIFY criterion and confirm it's met with evidence (an artefact, a working channel, a recorded outcome) — not assertion.
- [ ] **End-to-end demo:** Walk through a realistic scenario that exercises the new capability from start to finish — typically a single named stakeholder going through the design's full operational cycle.
- [ ] **Loop closure (if applicable):** If the mission involves a feedback loop (evaluation → analysis → service-update → next-evaluation), demonstrate the full cycle.
- [ ] **Deferred items:** Explicitly list anything discovered during VERIFY/INSTANTIATE that belongs to a follow-on mission.
- [ ] **Checkpoint:** Write the final checkpoint in the mission doc with what was built, what remains, and which follow-on missions exist.
- [ ] **PUR (optional but expected for pattern-driven implementation):** This is where the bulk of PURs land. For each pattern selected via PSR (or applied without a prior PSR), write a Pattern Use Record after the pattern is in place: outcome, prediction errors, surprises, library revisions suggested.

**Exit criterion:** Every completion criterion has a concrete demonstration in organisational reality. A new institutional partner could reproduce the operational cycle from the mission doc.

### 7. DOCUMENT

Make the mission's contribution navigable in the organisation's living documentation.

- [ ] **Operational documentation:** Create or update the institution-facing artefacts (training portfolios, programme websites, partner-facing briefings, governance manuals) that explain what was built, how to use it, and how it connects to prior work.
- [ ] **Cross-references:** Link new artefacts to existing organisational documentation — funder reports, board papers, prior programme summaries — so the mission's contribution is discoverable from existing entry points.
- [ ] **Stakeholder accessibility:** If the mission produced new materials, confirm they're reachable from the organisation's standard navigation paths (member-institution intranet, public website, partner mailing list).
- [ ] **Deferred-item tickets:** Any deferred items from INSTANTIATE that warrant future organisational work should be noted with enough context for a future mission author to pick them up.

The DOCUMENT phase produces durable, navigable organisational knowledge — not the mission doc itself (which is the working record), but the documentation that lives in the institution's self-representing surface (annual reports, programme summaries, partner-facing materials). The mission doc says what was done and why; the operational documentation says what exists and how to use it.

**Exit criterion:** Someone navigating the organisation's standard documentation can discover what this mission built without knowing the mission exists. The contribution is findable via routine navigation, not just via insider knowledge.

## Mission Lifecycle States

```
[HEAD] → IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → [hand-off?] → INSTANTIATE → DOCUMENT → COMPLETE
                                                                                               ↓
                                                                                           RE-OPENED
                                                                                           (if gaps found)
```

`HEAD` is optional. When present, it precedes IDENTIFY; when absent, the
mission starts at IDENTIFY.

A mission may also be:

- **BLOCKED:** Waiting on another mission (e.g., a predecessor programme to wind down) or an external dependency (e.g., funding decision, governance approval).
- **DEFERRED:** Scope was right but timing is wrong.
- **NONSTARTER:** The gap turned out not to be real, or the approach is wrong on contact with organisational reality.

## Hand-off pattern

Institutional missions frequently split between two teams: the *writing team* (often external to the institution; subject-matter experts, evaluators, advisors) and the *implementing team* (within the institution; programme managers, training leads, board members).

The hand-off pattern: writing team carries the mission through IDENTIFY → MAP → DERIVE → ARGUE → VERIFY. The mission doc, at VERIFY-close, is the *briefing*. Implementing team takes the briefing and carries INSTANTIATE → DOCUMENT.

The contract: VERIFY closes with the design implementable by a competent reader who is not the author. INSTANTIATE is the implementation pass; DOCUMENT integrates the contribution into the institution's living knowledge.

The implementing team can re-open earlier phases if they discover the briefing is inadequate. That re-opening is itself a finding for the writing team and may produce mission revisions.

## Conventions

- Mission files live in `<repo>/holes/missions/M-<name>.md`.
- Status line at the top records the mission's current phase-state. For
  straightforward missions this can be `**Status:** <PHASE> (date)`;
  for `HEAD`-bootstrapped missions it may be composite, e.g.
  `**Status:** HEAD complete; IDENTIFY pending`.
- Checkpoints are appended (never overwrite prior phases).
- Evidence is captured in the mission doc and any associated lab directories.
- Completed missions become source material for future missions.
- Each phase accretes — the mission doc grows as phases complete.

## PSR/PUR Discipline

Identical to the agentic-coding lifecycle. PSR (Pattern Selection Record) and PUR (Pattern Use Record) are the mechanism by which the pattern library evolves alongside the missions that consume it. Optional per phase, strongly encouraged whenever a non-trivial design choice references a pattern.

The relevant library for institutional missions is typically `futon3/library/system-coherence/` (system-design discipline) and `futon3/library/writing-coherence/` (for mission-doc prose discipline). Patterns from `futon3/library/futon-theory/` and `futon3/library/ants/` may apply where the mission's design uses active-inference framings or pattern-as-attractor claims.

Format and storage convention: same as the agentic-coding variant.

## Relationship to Other Processes

- **Portfolio inference** decides *which* mission to work on next given organisational priorities and dependencies. The mission lifecycle says *how* to work on it.
- **Patterns** inform DERIVE decisions. PSR/PUR feedback loops let pattern libraries evolve under organisational evidence.
- **Evaluation infrastructure** (e.g., evaluation forms, T1 instruments, surveys) stores the trail. Mission artefacts become source material for future missions.

## Differences from the Agentic-Coding Variant

- **IDENTIFY**: gap-first; the *primary* IDENTIFY work is naming `sorry`s — proof obligations the predecessor cannot discharge — not recapping predecessor outputs. The candidate framings carried forward from prior work are explicitly *under verification*, not foundational. (Agentic-coding missions also benefit from this discipline; in institutional work it is essential because the candidate framings have political and rhetorical force that can mask the gap.)
- **MAP**: capabilities / relationships / evidence (vs APIs / data stores / hyperedges). The "missing" column should cross-reference IDENTIFY `sorry`s.
- **DERIVE**: roles / coordination structures / invariant principles / resource flow / stakeholder-facing artefacts (vs entity types / relation types / invariant rules / data flow / view-UI specs).
- **ARGUE**: the plain-language argument explicitly states which `sorry`s the design closes and which it defers — at low register, no rhetoric.
- **VERIFY**: scenario test (vs code spike); explicit `sorry`-status check and candidate-framing verdicts before INSTANTIATE hand-off.
- **INSTANTIATE**: organisational implementation — convene, publish, start (vs build code, round-trip test, integration test).
- **DOCUMENT**: operational documentation in the institution's living surface (vs docbook entries in a self-representing stack).
- **Hand-off pattern**: explicit, often between writing and implementing teams; not the default in agentic-coding missions but the norm in institutional ones.
- **Plain-language argument** in ARGUE matters more: the implementing team are the readers, and they decide whether to commit to the design.

## Worked example

`futon4/holes/missions/M-or-training-as-learning-system.md` (draft 2; draft 1 archived as `.v1.md`) is the inaugural exemplar of this variant. The IDENTIFY draft 1 → draft 2 transition is the load test of the gap-first discipline: draft 1 led with predecessor outputs and candidate framings (recap-and-reframe); draft 2 leads with four named `sorry`s and treats the candidate framings as items the mission has to verdict on. The contrast between the two drafts demonstrates what changes when IDENTIFY is rewritten gap-first, and is the source of the anti-pattern callout in the IDENTIFY section above.
