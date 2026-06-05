# Futonic Campaign Lifecycle

A **Campaign** is a structural tier *above* Mission: a **Temporary Autonomous
Institution** that exists to deliver a **shared standard** which several Missions
must build towards but which **no single Mission can own**. It forms with a
charter, self-governs for its duration, delivers and verifies the shared standard,
and then **dissolves** — leaving its constituent Missions as the durable residue.

This document is the tier-up sibling of `mission-lifecycle.md` and
`mission-lifecycle-institutional.md`. It borrows their 7-phase spine and their
exit-criterion discipline, but the substrate is a *cluster of missions plus the
shared knowledge structure they depend on*, and the register is *institutional*
(roles, governance, charter) with one addition the mission lifecycles don't have:
**the institution is built to end.**

Provenance: Campaign was introduced by Joe (M-action-cost-modelling §3.5,
2026-05-26), passed shape-first IDENTIFY with three instances
(`Campaign-R3-honesty`, `Campaign-the-futon-stack` retroactively,
`Campaign-substrate-completion`), and was gated on a grounding pass
(`E-campaign-spec-grounding`, T10) before any Campaign-scaffolded work commits.
This document is that grounded spec, argued on the live
`Campaign-substrate-completion` instance (see §Worked example), so the T10
pre-commit gate is closeable.

## Why a Campaign, and not just a Mission (the grounding)

Per `futon3/library/structure/what-problem-is-this-actually-solving`:

- **IF** a body of work decomposes into several Missions that all depend on one
  *shared knowledge structure / standard* (a metric, a schema, a contract, a
  substrate) — and that standard must be the *same* for all of them.
- **HOWEVER** Mission-shape can't hold the standard without failing one of two ways:
  - **(a) Absorbed-into-one-Mission:** one Mission builds the standard to *its own*
    needs; the other Missions inherit a mis-fit and each appears successful while
    the joint goal silently slips (proxy-problem substitution).
  - **(b) Left-implicit:** each Mission builds its own version of the standard →
    it **forks**. (Worked failure: the "thin scalar field" forked into three thin
    shadows — aif2's ΔT tie-breaker, M-differentiable-code's would-be continuity,
    M-live-geometric-stack's T — none a real metric. See war-bulletin-10 WR-21.)
- **THEN** lift the work to a Campaign: a temporary institution that owns the
  shared standard's **cross-mission adequacy** — argues *why this standard*,
  **verifies it as fit for all consumers before any of them build to it**, then
  delivers it; the constituent Missions register their dependencies and hold the
  dependent work in escrow until the standard is verified.
- **BECAUSE** the shared standard needs an **ARGUE + VERIFY that has no
  single-Mission owner** — each constituent Mission sees only its own requirement;
  only the Campaign can establish that the standard fits *all* of them. That
  cross-mission adequacy check is the Campaign's reason to exist. Everything else
  (coordination, ordering, escrow) follows from it.

**Campaign vs. War Room.** The War Room (`futon3/holes/war-room.md`) is the
*standing, narrative* cross-futon coordination surface — strategic assessment in
bulletins. A Campaign is the *temporary, structured* institution spun up to
deliver one shared standard, with a lifecycle and a dissolution. A War Bulletin
often supplies a Campaign's charter (see §Worked example); the War Room persists,
the Campaign ends.

## Phases

Each Campaign phase maps to a Mission-spine analogue. Constituent Missions run
their **own** lifecycles independently; the Campaign coordinates, it does not
re-run their phases.

### 1. CHARTER  *(≈ HEAD + IDENTIFY)*

State the joint goal *no single Mission owns*, and the standard to be delivered.

- [ ] **Joint goal / gap.** Often *extracted from a War Bulletin* (the bulletin
  names the crossing point; the charter is its operational form). State as a gap:
  what no constituent Mission can establish on its own.
- [ ] **The shared standard.** Name the knowledge structure to be delivered (the
  metric / schema / contract) — what it is, not yet how it's built.
- [ ] **Joint completion criterion.** The testable condition under which the
  Campaign dissolves — necessarily *cross-mission* (e.g. "standard verified AND at
  least one dependent requirement released-and-consumed"), not "all members done."
- [ ] **Membership.** The constituent Missions, and which is the **keystone** (the
  Mission that builds/delivers the standard) vs. the **paired/dependent** Missions
  (consumers).
- [ ] **Provenance.** How the Campaign was chartered (bulletin, operator memo);
  date and source.

**Exit:** a human agrees the joint goal is real, is genuinely cross-mission, and
the membership is right.

### 2. CONSTITUTION  *(≈ MAP — survey the dependents + set governance)*

Survey the Missions that depend on the Campaign; constitute the institution.

- [ ] **Dependent-mission survey.** For each member, what does it *need from* the
  shared standard, and what does it *contribute*? Read the missions, don't guess.
- [ ] **Roles & decision rights.** Who owns coordination; who ratifies the standard
  (STANDARD-VERIFY); who decides dissolution. (Temporary-institution governance.)
- [ ] **Coordination shape.** `sequential` / `parallel` / `convergent` — the
  topology of how members relate to the keystone.
- [ ] **Ready vs. missing.** Two-column table at Campaign scope: what already
  exists toward the standard vs. what the Campaign must produce.

**Exit:** every dependent mission's requirement-on-the-standard is recorded; the
governance (who ratifies, who dissolves) is named.

### 3. ESCROW  *(≈ MAP, cont. — register the held dependencies)*

Register cross-mission dependencies as **held requirements** in the escrow ledger.

- [ ] **Escrow ledger.** One entry per dependency:
  `{from-mission, on: <the standard / keystone>, requirement, status}`.
- [ ] **Hold the dependent work.** Each paired Mission marks the slice that depends
  on the standard as `held` — *named, not built* (generalises HEAD-as-escrow to
  cross-mission; the requirement doesn't go away, but it isn't built on sand).
- [ ] **Two-step release plan.** Each entry will pass `held → contract-released
  (build-to-verified-spec) → satisfied (consume-live-delivery)`. Name what releases
  each step (STANDARD-VERIFY releases the contract; RUN/DELIVER satisfies it).

**Exit:** every cross-mission dependency is in the ledger with a `held` status and
a named two-step release path.

### 4. STANDARD-ARGUE  *(≈ ARGUE — why this standard, fit for all)*

Argue the shared standard: why *this* knowledge structure, and why it is adequate
for **every** consumer (the check no single Mission can make).

- [ ] **Why-this-standard.** Alternatives considered and rejected; the standard
  feels *inevitable* given all members' requirements, not merely workable for one.
- [ ] **Cross-mission adequacy.** For each member's escrowed requirement, show the
  standard satisfies it. A requirement the standard can't meet is a STANDARD-ARGUE
  blocker, not a member's problem.
- [ ] **Plain-language argument** (no jargon): what the standard is and why all the
  members need this exact one.

**Exit:** the standard is argued adequate for all members; any unmet requirement is
surfaced (revise the standard or re-scope the member).

### 5. STANDARD-VERIFY  *(≈ VERIFY — the Campaign's reason to exist)*

**Verify the shared standard as a design, before any member builds to it.** This is
the load-bearing phase: it produces "the standard to build towards." Distinct from
the keystone Mission's *own* VERIFY (which checks its build is sound) — this checks
the standard is **fit for all consumers** (cross-mission adequacy).

- [ ] **Logic-model-before-code at Campaign tier** (per
  `mission-coherence/logic-model-before-code`): express the standard's invariants
  / interface contract as a checkable model over an abstract trace; conforming
  witness ⇒ 0 violations; one adversarial trace per consumer-requirement ⇒ caught.
- [ ] **Consumer-fit check.** Each member's escrowed requirement is verifiable
  against the standard's contract (not its implementation).
- [ ] **Contract freeze.** On pass, the standard's *contract* is fixed.

**Exit:** the standard's design is verified fit-for-all. **Escrow steps to
`contract-released`** — paired Missions may now resume *designing/building to the
verified spec*, even before the keystone delivers.

### 6. RUN / DELIVER  *(≈ INSTANTIATE — deliver the shared structure)*

The keystone Mission builds and delivers the verified standard; consumers attach.

- [ ] **Keystone delivery.** The keystone Mission runs its own INSTANTIATE and
  ships the standard implementing the verified contract.
- [ ] **Escrow satisfaction.** As each paired Mission consumes the live delivery,
  its ledger entry steps to `satisfied`.
- [ ] **Joint-criterion check.** Confirm the CHARTER joint completion criterion is
  met with evidence (standard delivered + ≥1 requirement satisfied).

**Exit:** the standard is delivered and at least one consumer is live against it;
the joint completion criterion holds.

### 7. DISSOLUTION  *(≈ DOCUMENT + COMPLETE — the institution ends)*

A Campaign is built to end. Dissolve cleanly; leave the Missions as the residue.

- [ ] **Hand back.** Any escrow entries not yet `satisfied` become ordinary
  cross-mission dependencies (or follow-on missions); they are *named*, never
  silently dropped.
- [ ] **Closure record.** The Campaign doc becomes the durable record: what the
  standard was, where it lives, which Missions consumed it.
- [ ] **Dissolve.** Governance is stood down; the Campaign no longer coordinates.
  The keystone + paired Missions continue under their own lifecycles.

**Exit:** the shared standard is in use, the Campaign coordinates nothing further,
and a reader can discover the standard and its consumers from the closure record.

## Campaign States

```
FORMING → CHARTERED → RUNNING → DISSOLVING → DISSOLVED
                         ↓
                      STALLED   (a constituent Mission is BLOCKED; Campaign waits)
                         ↓
                     DISBANDED  (joint goal turned out not real — NONSTARTER analogue)
```

- **STALLED:** a member is BLOCKED (its own dependency/approval); the Campaign holds.
- **DISBANDED:** the shared standard turned out not to be needed / not cross-mission
  after all — record why (a finding; `:special-case true` for the shape).

## The Escrow Ledger (the new first-class element)

The mechanic that makes "pair the Missions, hold the build" real. Each entry:

```
{:from-mission   <M-…>            ; the dependent (consumer) mission
 :on             <the standard | keystone M-…>
 :requirement    "<what it needs from the standard>"
 :status         :held | :contract-released | :satisfied}
```

Two-step release is the point: `STANDARD-VERIFY` flips entries to
`:contract-released` (consumers build to a *verified spec*, unblocked but not
building on sand); `RUN/DELIVER` flips them to `:satisfied` (consumers attach to
the live delivery). This generalises **HEAD-as-escrow** (mission-level, WR-15) to
the **cross-mission** case.

## Conventions

- Campaign files live as `C-<name>.md`. Default home: `futon3c/holes/campaigns/`
  (the coordination layer, where Mission Control lives) — or the keystone Mission's
  repo if more natural. (Convention to confirm with the operator on first use.)
- Status line records phase-state + coordination shape, e.g.
  `**Status:** STANDARD-VERIFY (convergent); 2 escrow entries held`.
- Constituent Missions keep their own `M-<name>.md` and lifecycles; the Campaign
  references them, never duplicates their phase records.
- Checkpoints append; each phase accretes onto the Campaign doc.
- A dissolved Campaign is source material for future Campaigns.

## Backing patterns

`futon3/library/campaign-coherence/`:
- `campaign-as-temporary-institution` — the structural shape + the build-to-end discipline.
- `shared-standard-has-no-single-owner` — the grounding (why Campaign-shape; cross-mission adequacy as the reason to exist).
- `cross-mission-escrow` — the held-requirement primitive + two-step release.

Related: `structure/what-problem-is-this-actually-solving` (grounding),
`mission-coherence/logic-model-before-code` (STANDARD-VERIFY method),
`futon-theory/mission-dependency` + `mission-interface-signature` (the dependencies
escrow registers).

## Worked example — `Campaign-substrate-completion` (the live, chartering instance)

The instance this spec is grounded on (closes `E-campaign-spec-grounding` T10).

- **CHARTER** (extracted from **war-bulletin-10 WR-21**): substrate-2 has a *real
  ground metric* — distance → curvature → attractors — that the dependent Missions
  can build on, replacing the thin scalar field.
- **Shared standard:** the ground metric on substrate-2 (dependency + git
  co-change + semantic distance), from which Ollivier–Ricci curvature, a continuous
  embedding, and Fisher–Rao information geometry derive.
- **Membership:** keystone = `M-substrate-metric` (candidate, defines + builds the
  metric); paired = `M-aif2` (holds the tension-proposer slice — needs curvature as
  principled tension) and `M-differentiable-code` (holds its continuous-data slice);
  possibly `E-codebase-manifold` (the demonstrator).
- **Coordination shape:** convergent (keystone-first; consumers held).
- **Escrow ledger (initial):**
  - `{:from M-aif2, :on metric, :requirement "curvature as the tension-proposer's signal (replaces signed-grad-vs-(1−T))", :status :held}`
  - `{:from M-differentiable-code, :on metric, :requirement "continuous embedding for gradients over the code graph", :status :held}`
- **Joint completion criterion:** the metric is verified fit-for-all (STANDARD-VERIFY)
  **and** at least one paired requirement is released-and-consumed (e.g. aif2's
  tension-proposer reads curvature).
- **STANDARD-VERIFY** is the crux: verify the metric's contract serves *both* aif2
  (curvature/tension) and differentiable-code (continuity) before either commits —
  the cross-mission adequacy check neither Mission could perform alone.

This is the first Campaign. It is *chartered*, not *validated in production*; the
claim is that it passes the shape-first + grounding discipline, with a live joint
goal a single Mission demonstrably could not own (WR-21's three-way fork is the
evidence).
