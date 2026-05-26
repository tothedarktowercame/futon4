# Mission: Interest-Network Coupling

**Date:** 2026-05-14
**Status:** **Design-frozen v1 (2026-05-14).** IDENTIFY through VERIFY are complete on paper (§§1–4). Codex-7's sign-off-final landed after a second whistle round resolved four loose ends: daily-cadence-without-synthetic-churn discipline (§2.2), checkpoint phase as metadata not id-prefix (§2.0), VC-7 replay/idempotence (§4), and HEAD-as-escrow named as a surfaced pattern (§3.5). Next meaningful review point is the actual step (b) artefact (event vocabulary committed as flexiarg/schema), not another prose pass. Awaiting codex-7's piece #2 → step (b) sequencing.
**Sequencing role:** **predecessor mission to `~/code/futon7/holes/M-interim-director.md`** (HEAD authored 2026-05-14 via eoi-new). Step (b) (event vocabulary v1 committed under `library/`) is a *hard predecessor* for M-interim-director *emitting* checkpoint delta-batches — but **not** for its HEAD declaring intentions: Joe used a HEAD-as-escrow pattern (T2 of the HEAD) to crystallise intentions without committing to data shape, decoupling the HEAD-authoring from the vocabulary commit. Steps (c)/(d)/(f) can land in parallel with M-interim-director's checkpoint cycle once it starts (they consume the already-fixed vocabulary). Realistic pipeline: codex-7 piece #2 lands → step (b) lands → M-interim-director starts emitting → step (c)+(d) land in parallel → M-interim-director closes (~2 weeks out) → downstream validation-execution phase becomes a second checkpoint source.
**Owner:** futon4 (event/projection model + WebArxana surface), with
  ingress from futon5a (EoI corpus) and futon7 (mission checkpoints as
  posterior update source).
**Cross-ref:**
  - Parent (corpus side): `~/code/futon5a/holes/missions/M-expressions-of-interest.md`
    INSTANTIATE step (e) — EoI-network-as-interest-model with
    completeness-not-closure.
  - Parent (surface side): `~/code/futon4/holes/missions/M-essays-edit-cycle.md`
    — single-essay annotation lifecycle; this mission extends to cross-essay
    + cross-mission coupling.
  - Future dependency: `~/code/futon7/holes/M-interim-director.md` — to be
    authored. Will be the first concrete posterior-update source.
**Authors:** thrashed out by claude-3 + codex-7 on 2026-05-14 via three
  whistle rounds; codex-7's sign-off owed.

## 1. IDENTIFY

### Motivation

The EoI corpus (`atthangika-buckets.json` `eoi_instances[]` +
`institution_objects[]`) is now N=5, with the first non-trivial cross-essay
structure landing today: Hyperreal Side B explicitly engages Side A's §2,
§5, and §6. Step (e) of `M-expressions-of-interest` reframes the corpus as
**a network model of Joe's interests with the property of *completeness*
(not *closure*)**.

In parallel, Joe has authored `M-interim-director` (futon7) HEAD on
2026-05-14: a **2-week design mission** (≈10 working days net of 4
travel) whose output is *a preregistered experimental plan* for
business-validation of the Hyperreal offer. The mission is
**self-contained-not-totalising** per its T1/T4 tensions, but
**send-gates outbound EoIs by default** for the window. There are
therefore **two distinct posterior-update sources** that this
projection must accommodate, both consuming the same vocabulary:

| Source | Window | What it emits |
|---|---|---|
| **M-interim-director itself** | 2026-05-14 → ~2026-05-28 (≈10 working days) | Checkpoints during *design* of the validation experiments — daily-until-done per T3, content-pegged to futon-stack coverage rather than calendar. |
| **Validation execution** (downstream; possibly June–August 2026; specified by M-interim-director's exit artefact) | post-M-interim-director close | Checkpoints during *running* of the experiments M-interim-director designs. May be authored by Joe or by someone else (per T5/exit: "a reasonable plan of work in place for business validation that *someone else* could run — like a preregistered study"). |

Either source's artefacts — customer-interview notes, business plan
v1, go/nogo memos, basin-foreclosure decisions — constitute **lived
evidence** that updates the standing of EoI entities: tensions get
addressed, basins get foreclosed, new EoIs get spawned, existing
claims get strengthened or falsified.

The coupling between (corpus, mission, surface) needs an explicit data
model and projection apparatus. Without one, M-interim-director's outputs
sit alongside the EoI corpus as parallel documents rather than as a
**posterior update**; the interest-network either ossifies (if updates are
silent) or churns incoherently (if updates overwrite without provenance).

### The actual sorry

> **Sorry-INTEREST-COUPLING-PRIOR**: the corpus declares interests but has
> no machinery to track how those interests evolve under work. Lived
> evidence currently lands in mission docs adjacent to but not connected
> with the EoI corpus. **Closure condition:** a typed event vocabulary
> that lets a mission emit posterior updates targeting EoI entities, with
> provenance preserved.
>
> **Sorry-INTEREST-COUPLING-CHECKPOINT**: there is no agreed format for
> emitting a delta-batch from a mission checkpoint. Pre-condition: the
> event vocabulary is closed. **Closure condition:** a checkpoint format
> (markdown + embedded EDN) that XTDB can project from.
>
> **Sorry-INTEREST-COUPLING-PROJECTION**: the cross-essay interest-surface
> in WebArxana doesn't exist yet. Pre-condition: events + checkpoints
> exist and have actual data. **Closure condition:** a route in WebArxana
> that renders the corpus as a network with composite-arrow edges,
> cross-essay annotations, and a temporal trail of state events.

### Theoretical anchoring

- **Prior / likelihood / posterior split.** The EoI corpus is the prior.
  Lived evidence from M-interim-director is the likelihood. The
  interest-network surface renders the posterior. Already framed in
  `M-expressions-of-interest` IDENTIFY; this mission operationalises it.
- **Event log vs derived projection.** Mirrors the single-essay lifecycle
  work in `M-essays-edit-cycle` Track A and the codex-7 piece #2 design:
  events are append-only and provenance-bearing; projections are derived
  views. M-interim-director must NOT mutate EoI entities in place; it
  emits explicit update events that *target* them.
- **Accretive posterior with provenance.** The network becomes more
  complete under use because evidence accumulates, not because documents
  multiply. Reopen is a first-class event so retraction isn't terminal.
- **Completeness-not-closure as vector, not scalar.** Three coordinates,
  rendered separately (see §1.completion).

### Scope in

- A v1 **event vocabulary** for cross-mission posterior updates.
- A **checkpoint emission format** (markdown + embedded EDN) for missions
  that produce these events.
- A **projection apparatus** that reads emitted events from XTDB and
  computes the posterior state of every EoI entity (essay, section,
  annotation, institution-object, basin, composite-arrow edge).
- A **WebArxana surface** that renders the corpus as an interest-network:
  nodes are essays + institution-objects + basins; edges are cross-essay
  annotations + composite-arrow overlaps + `link/asserted` events;
  temporal axis = checkpoint trail per node/edge.
- A **completeness signal** rendered as a 3-vector (Path coverage / basin
  coverage / resolution-path coverage), NOT collapsed into a scalar score.

### Scope out

- Authoring M-interim-director itself (separate mission, futon7). This
  mission specifies what shape M-interim-director's checkpoints must take
  to participate, but does not author the mission.
- Automated event emission from raw evidence artefacts (interview notes
  etc.). The checkpoint is the operator-asserted delta-batch boundary;
  pre-checkpoint events are out of scope for v1.
- The single-essay retraction-visibility / lifecycle work (codex-7 piece
  #2). This mission depends on it, but does not absorb it.
- Inferring events from prose. All event emission is explicit-EDN; the
  event log is authored, not extracted.

### Completion criteria

1. Event vocabulary v1 is committed in code (a flexiarg or EDN schema
   under `library/`) and used by at least one real M-interim-director
   checkpoint.
2. One real M-interim-director checkpoint has been authored with an
   embedded EDN delta-batch, and the projection has consumed it (an EoI
   entity has had its standing changed by lived evidence with provenance
   visible).
3. WebArxana renders the corpus-as-interest-network for at least the
   five-essay corpus (Glasgow, Anthropic, Mission-coherence, Side A,
   Side B), with cross-essay edges visible.
4. The completeness 3-vector is rendered as a surface, not as
   speculation. At time of completion, the operator can read off which
   Path-arrows are exercised, which basins are spanned, and which
   tension-subtypes have been resolved through lived evidence.

### Relationship to other missions

| Mission | Relationship |
|---|---|
| `M-expressions-of-interest` (futon5a) | Parent — declares the network property; this mission operationalises it. |
| `M-essays-edit-cycle` (futon4) | Sibling — single-essay lifecycle. Shares event-log discipline; this mission generalises to cross-essay + cross-mission. |
| `M-essays-retraction-visibility` (futon4) | Sibling — Track A of the edit-cycle mission, just landed. The cross-mission layer reuses its retraction-as-event-not-deletion stance. |
| `M-essays-diachronic-model` (futon4) | Sibling — the diachronic argumentation graph codex-7 will build as piece #2. The interest-network projection shares its temporal-trail discipline. |
| `M-interim-director` (futon7, HEAD authored 2026-05-14) | **Successor** — first concrete source of posterior updates. HEAD-as-escrow pattern: Joe authored the HEAD without committing to data shape, so step (b)'s vocabulary commitment is the predecessor for *emitting* checkpoints, not for *declaring intentions*. Once step (b) lands, M-interim-director's design-phase checkpoints flow in; later, the validation-execution phase (which M-interim-director designs) becomes a second source. Steps (c)/(d)/(f) run in parallel with both. |

## 2. MAP — DERIVE (the architecture, design-ready)

### 2.0 Checkpoint-level metadata (required)

Independent of any individual event's payload, **every checkpoint
delta-batch carries metadata at the checkpoint level**:

| Field | Required | Notes |
|---|---|---|
| `:checkpoint/id` | yes | Unique within the source mission (e.g. `"M-interim-director/CP-3"`). No phase prefix — disambiguation is done by `:phase` below. |
| `:mission/source` | yes | The source mission's id (e.g. `"futon7:M-interim-director"`). |
| `:phase` | yes | One of `:design`, `:execution`. Distinguishes M-interim-director's *design* checkpoints from the *execution* checkpoints of the validation phase it produces. Same vocabulary applies to both; phase is metadata, not part of `:event/type`. The surface can filter on this without ontology split. |
| `:timestamp` | yes | ISO-8601 UTC. |
| `:emitted-events` | yes | Vector of event records per §2.1. |

### 2.1 Event vocabulary (v1)

Eight event types. Neutral `state/*` namespace so the same vocabulary
applies to essay annotations, institution-objects, basins, and
composite-arrow edges without essay-specific or interest-specific
slang:

| Event | What it asserts |
|---|---|
| `state/spawned` | Target entity is newly created. |
| `state/refined` | Target entity's framing has been sharpened (re-anchor, scoped). |
| `state/strengthened` | Target entity's standing has gained support from evidence. |
| `state/addressed` | Target entity (typically a tension) has been resolved by evidence; not falsified. |
| `state/falsified` | Target entity has been contradicted by evidence. |
| `state/foreclosed` | Target entity (typically a basin) has been deliberately closed off. |
| `state/reopened` | Target entity that was addressed/falsified/foreclosed has had its standing reset to live. First-class so retraction isn't terminal. |
| `link/asserted` | A new cross-essay relation exists (separate from `state/spawned` to avoid overloading "new target" with "new relation"). |

#### Event payload schema

```edn
{:event/id           String              ; unique within checkpoint
 :event/type         keyword             ; one of the eight above
 :checkpoint/ref     String              ; back-pointer to the checkpoint
 :target/entity-id   String              ; the EoI entity this event targets
 :target/granularity keyword             ; one of :essay, :section, :annotation,
                                         ;        :institution-object, :basin,
                                         ;        :composite-arrow
 :prior-state        keyword             ; standing before this event. Required for state/*
                                         ; events; optional or omitted for link/asserted.
 :posterior-state    keyword             ; standing after this event. Required for state/*
                                         ; events; optional or omitted for link/asserted.
 :evidence/refs      [String]            ; pointers to evidence artefacts that
                                         ; justify the event (e.g. interview
                                         ; notes, go/nogo memos, plan drafts)
 :operator/rationale String              ; Joe's plain-language reason
 ;; link/asserted only:
 :link/src           String              ; source entity (for link events)
 :link/dst           String              ; destination entity (for link events)
}
```

### 2.2 Checkpoint emission format

A mission checkpoint is **markdown with an embedded EDN delta-batch**.
The markdown remains the operator's authoring surface (matches the
existing `mission-lifecycle.md` convention); the EDN is the authoritative
machine-readable feed for the projection.

**Emission discipline: no-op days emit no checkpoint.** Daily cadence
is fine when there's a real state delta — even small ones like
`state/refined` on a single annotation. But synthetic checkpoints (an
empty `:emitted-events` vector emitted just to satisfy "daily") are
forbidden: they pollute the log with non-events and degrade the
projection's signal-to-noise. M-interim-director's T3 "daily-until-done,
content-pegged not calendar-pegged" maps to this discipline directly:
if a day produced no genuine delta, no checkpoint lands.

```markdown
### Checkpoint 3 — 2026-06-18

**What was done:**
- Conducted six customer interviews across week 2.
- Drafted business plan v1 §§1–3.
- Reviewed Side A §5's unresolved counter-hypothesis against interview evidence.

**Test state:** No tests; commercial-discipline checkpoint, not engineering.

**Next:** Conduct three more interviews; complete business plan §§4–5.

```edn
{:checkpoint/id "M-interim-director/CP-3"
 :mission/source "futon7:M-interim-director"
 :phase :execution
 :timestamp "2026-06-18T19:40:00Z"
 :emitted-events
 [{:event/id "evt-cp3-1"
   :event/type :state/addressed
   :checkpoint/ref "M-interim-director/CP-3"
   :target/entity-id "arxana/anno/hyperreal-side-a-v1/unresolved-counter-hypothesis"
   :target/granularity :annotation
   :prior-state :live
   :posterior-state :addressed
   :evidence/refs ["customer-interviews/week-2-summary.md" "go-nogo-memo-01.md"]
   :operator/rationale "Interview evidence narrowed the counter-hypothesis from open to addressed, though not falsified."}
  {:event/id "evt-cp3-2"
   :event/type :state/spawned
   :checkpoint/ref "M-interim-director/CP-3"
   :target/entity-id "arxana/essay/hyperreal-client-pitch-acme-labs-v1"
   :target/granularity :essay
   :prior-state nil
   :posterior-state :live
   :evidence/refs ["pitch-draft-acme-labs-v1.md"]
   :operator/rationale "New outward EoI spun out of repeated buyer-language in interviews."}
  {:event/id "evt-cp3-3"
   :event/type :state/foreclosed
   :checkpoint/ref "M-interim-director/CP-3"
   :target/entity-id "eoi-basin/postdoc-academic-affiliation"
   :target/granularity :basin
   :prior-state :live
   :posterior-state :foreclosed
   :evidence/refs ["customer-interviews/week-2-summary.md" "time-budget-review-01.md"]
   :operator/rationale "June–August validation window leaves no credible path for this basin as a live operational vehicle."}
  {:event/id "evt-cp3-4"
   :event/type :link/asserted
   :checkpoint/ref "M-interim-director/CP-3"
   :target/entity-id "link/hyperreal-side-b-8-engages-side-a-6"
   :target/granularity :composite-arrow
   :link/src "arxana/essay/hyperreal-director-side-b-v1/section/8"
   :link/dst "arxana/anno/hyperreal-side-a-v1/tension-right-effort-indicators"
   :evidence/refs ["customer-interviews/week-2-summary.md"]
   :operator/rationale "Live interview evidence confirms Side B §8 is not just resonant with Side A §6 but operationally engages its named missing-arrow problem."}]}
```
```

(That's a markdown code block containing the canonical example from
codex-7's third response, lightly extended with `prior-state` /
`posterior-state` per the agreed payload schema.)

The projection reads the EDN block, validates each event against the
v1 vocabulary, looks up the target entity in XTDB, and writes one event
record per emitted event. Validation errors block the checkpoint; the
operator fixes the EDN and re-runs.

### 2.3 Projection

Two-stage:

1. **Event log (authoritative).** Each emitted event becomes an XTDB
   entity with the payload above. Append-only, provenance-bearing.
2. **Posterior projection (derived).** For every EoI entity, the
   projection computes its current standing by replaying the event log
   in timestamp order. The projection is recomputable from the log
   alone — no in-place mutation of EoI entities.

The projection exposes:
- `entity-standing(entity-id)` → `{:state keyword :as-of timestamp :event-trail [event-id]}`
- `entity-history(entity-id)` → ordered list of events targeting this entity
- `network-edges(checkpoint-id?)` → all `link/asserted` events (optionally
  filtered to a checkpoint)
- `completeness-vector(timestamp?)` → 3-vector (see §2.5)

### 2.4 WebArxana surface

A new route, e.g. `arxana://view/interest-network`, scoped to the EoI
corpus (or any user-selected subset). MVP rendering:

- **Nodes:** essays, institution-objects, basins.
- **Edges:** cross-essay annotations (essay-section → essay-section);
  composite-arrow overlaps (computed from the schema, not from events);
  `link/asserted` events from checkpoints.
- **Node decoration:** current standing colour (live / addressed /
  strengthened / falsified / foreclosed / reopened); checkpoint count.
- **Temporal trail:** click a node to see its event-trail in
  checkpoint-chronological order.
- **No churn render:** the network shows current standing + history; it
  does not animate every event.

The surface is a sibling of codex-7's piece #2 essay-lifecycle surface;
both consume the same projection apparatus. Shared infrastructure
(`fetch-projection-hyperedges` or similar) loads from the proxy.

### 2.5 Completeness signal (vector, not scalar)

Rendered as three independent coordinates:

| Coordinate | What it measures | Source |
|---|---|---|
| **Path coverage** | Conceptual span: every Path-arrow has ≥1 EoI exercising it as a primary arrow. | `eoi_instances[].arrow_components` in the schema. |
| **Basin coverage** | Strategic span: every basin in `M-expressions-of-interest.strawmen/` has ≥1 EoI exploring it. | Strawmen directory as the basin universe; `eoi_instances[]` as the occupancy / exploration evidence. (`institution_objects[]` enriches basin interpretation but is not the direct measure.) |
| **Resolution-path coverage** | Lived adequacy: each tension-subtype has ≥1 case where M-interim-director has *resolved* an instance via `state/addressed` or `state/falsified`. | Event log filtered to `state/{addressed,falsified}`. |

The three coordinates measure different things. Collapsing to a scalar
hides whether progress came from broader exploration (Path/basin
coverage), deeper validation (resolution-path coverage), or merely more
churn. The surface renders all three side-by-side; the operator reads
them as a vector.

The resolution-path coordinate is the most M-interim-director-sensitive
and the strongest signal that the mission is **adding posterior
discriminating power**, not just more documents.

## 3. ARGUE

Three design choices the plan makes, and why each beats its
alternative.

### 3.1 Event log + projection, not in-place mutation

**IF** we modelled M-interim-director's outputs as direct edits to EoI
entities (e.g. flipping an annotation's `:retracted` flag based on
lived evidence), **HOWEVER** that approach silently loses the *why*
of every transition — the projection forgets which checkpoint
addressed which tension on what evidence. **THEN** the resulting
interest-network can render *current standing* but cannot render *the
update trail that made it current*. **BECAUSE** the completeness
property the parent mission specifies is *accretive*
(`M-expressions-of-interest` step (e): "completeness moreso than
closure"), the event log must be the authoritative source. Provenance
isn't a nice-to-have here; it's the load-bearing part of the
property. Mirrors the same separation `M-essays-edit-cycle` Track A
makes for single-essay retraction lifecycle: retracted annotations
stay visible because deletion is a state transition, not an erasure.

### 3.2 Markdown + embedded EDN, not sidecar files or emit commands

**IF** we used per-checkpoint sidecar files (`CP-3.edn` alongside the
markdown), **HOWEVER** the sidecar invites drift: a future operator
could update the prose checkpoint without touching the sidecar (or
vice versa), and the inconsistency lands silently in the projection.
**ALTERNATIVELY** an `eoi mission-checkpoint emit` tool that wraps
markdown + writes XTDB events keeps the two paired by construction,
**HOWEVER** it introduces a new operator dependency (the tool must
exist, be installed, and be run) and obscures the structured payload
behind a CLI verb. **THEN** the cleanest discipline is to embed the
EDN block *inside* the markdown checkpoint section: the operator's
authoring surface stays markdown (matching the existing
`mission-lifecycle.md` convention used in all futon missions); the
machine-readable component lives in the same artefact and cannot
drift from its prose context. **BECAUSE** the Peeragogy provenance
pattern (sourced posterior additions like `**Added 2026-MM-DD from
<source> posterior pass.**`) demonstrates that prose-marking alone is
already culturally accepted in the futon stack — we just add EDN
where prose alone can't feed XTDB.

### 3.3 Vector completeness, not scalar score

**IF** we collapsed the three coordinates (Path coverage / basin
coverage / resolution-path coverage) into a single completeness
score, **HOWEVER** that score hides whether progress came from
broader exploration (Path/basin coverage), deeper validation
(resolution-path coverage), or merely more documents shipped without
discriminating power. **THEN** the surface would render a smoothly
increasing number even when M-interim-director is just churning
without advancing the corpus. **BECAUSE** the operator-facing
question this mission has to answer is *"is the work adding
posterior discriminating power, or am I just generating more
documents?"*, the resolution-path coverage coordinate must be visible
*as itself*, not averaged with the easier-to-grow exploration
coordinates. The 3-vector is the minimum honest reading-surface.

### 3.4 Trade-offs taken

What was given up and why:

- **Authoring friction** for **provenance correctness.** Embedding
  EDN inside every checkpoint is more friction than a free-form prose
  note. Payoff: every state transition has a citation, a rationale,
  and a target — projections become trustworthy.
- **Surface simplicity** for **vector clarity.** A single number is
  easier to glance at than three coordinates. Payoff: the operator
  can't be misled by progress on the cheap dimensions while the
  load-bearing one stagnates.
- **Append-only event log** for **edit-correctness via reopen.**
  Append-only means the log grows without bound and the projection
  has to replay it every time. Payoff: every past state of every
  entity is recoverable; `state/reopened` lets evidence reverse
  earlier conclusions without overwriting them.

### 3.5 Pattern cross-reference

- **`structure/unresolved-tensions-at-closure`** (futon3/library/) —
  the reopen event implements this pattern at cross-mission scale:
  tensions that get `state/addressed` aren't *closed* in the
  closure-machinery sense; they remain in the log as evidence of how
  the closure was reached, and `state/reopened` can be emitted if
  later evidence reverses it. Mirrors the round-aware verdict from
  the EoI engine work.
- **`writing-coherence/name-what-you-drop`** — the `state/foreclosed`
  event names what a checkpoint deliberately closes off (a basin, a
  candidate institution, a strawman). Foreclosure is co-located with
  the decision that made it, not silently dropped from later
  inventories.
- **`structure/whose-question-is-this`** — the
  `:operator/rationale` field carries Joe's plain-language reason for
  each event, so the projection's posterior state always traces back
  to an operator-asserted *because*, not to a machine inference.
- **HEAD-as-escrow** (NEW pattern, surfaced by M-interim-director T2;
  not yet a flexiarg). A mission HEAD that crystallises intentions
  *without committing to data shape*, used to decouple HEAD-authoring
  from a predecessor's schema commit. The HEAD escrows the operator's
  intent for the predecessor to redeem once schema lands. The
  constraint that makes the pattern safe: **explicitness about what
  is not yet committed** (M-interim-director T2 demonstrates this).
  Codex-7 (2026-05-14) recommends formalisation as a future flexiarg
  under `structure/` or `pattern-discipline/`. Carried as a tech-debt
  item; not blocking this mission's INSTANTIATE.

## 4. VERIFY

### 4.1 Approach

Dogfood on M-interim-director's first real checkpoint. The mission's
own June–August 2026 validation window is the verification substrate:
every checkpoint that lands in the live mission is a verification
case for the coupling. The verification question per checkpoint:
*does the projection's posterior match Joe's own read of which
interests have moved, and in which direction?*

### 4.2 Verification cases (proposed, scored when checkpoints land)

| Case | What it tests | Pass condition |
|---|---|---|
| **VC-1: round-trip schema** | An emitted event reaches XTDB and is queryable by entity-id. | After CP-1 lands, `entity-history(target-entity-id)` returns the emitted event with full payload preserved. |
| **VC-2: prior/posterior consistency** | The `prior-state` field on an emitted event matches the projection's recorded standing before the event. | Operator can author CP-N without consulting external state; the projection independently validates that CP-N's `prior-state` claims are consistent with the log replayed up to CP-N. Inconsistencies surface as validation errors. |
| **VC-3: reopen reachability** | A `state/reopened` event after a `state/addressed`/`state/falsified`/`state/foreclosed` correctly restores the target's standing to `:live` in the projection, with both events visible in the trail. | Synthesise a reopen case in CP-2 or CP-3; projection renders the target as `:live` with the address→reopen trail intact. |
| **VC-4: completeness vector responsiveness** | The 3-vector changes coordinate-by-coordinate when the appropriate event lands. | A `state/addressed` on a tension whose subtype hadn't yet been resolved increments resolution-path coverage; a `state/spawned` of an EoI exercising a new Path-arrow as primary increments Path coverage; etc. |
| **VC-5: cross-essay link rendering** | `link/asserted` events surface as edges in the WebArxana interest-network view. | After CP-N with at least one `link/asserted`, the new edge appears in `arxana://view/interest-network` with rendered `evidence/refs` and `operator/rationale` on hover. |
| **VC-6: operator-projection agreement** | Joe-side and projection-side answers to "which interests have moved this checkpoint?" agree. | Within 24h of CP-N landing, Joe lists what he thinks the checkpoint addressed/foreclosed/spawned; the projection's diff over CP-N matches. Disagreements surface either (a) bugs in the projection or (b) under-specified events. |
| **VC-7: replay / idempotence** | Reprojecting the entire event log from scratch yields the same posterior state; accidental duplicate ingest of a checkpoint is rejected or de-duplicated. | Synthesise a duplicate-ingest test of an arbitrary checkpoint; projection state before and after is byte-identical. Re-running the projection from cold against the full log yields the same standings as the live projection. |

### 4.3 Verification deferral

Real verification can only begin after step (b) (vocabulary
committed) AND step (c) (projection wired into XTDB) have landed AND
at least one M-interim-director checkpoint has been authored. The
verification work itself is incremental — each landed checkpoint adds
to the case-base — so VERIFY does not have a single completion
moment. Mission-close moves to DOCUMENT after VC-1 through VC-5 are
each demonstrated at least once with real data, plus at least one
instance of VC-6 with operator/projection agreement at the case
level.

## 5. INSTANTIATE — _pending_

Order (codex-7's revised sequence — vocabulary first, then Joe is
unblocked to produce valid checkpoints without waiting on projection or
rendering):

- **(a)** Codex-7 sign-off on this plan.
- **(b)** Write event vocabulary into a flexiarg or schema file under
  `library/` (codex-7 owns).
- **(e)** Author `M-interim-director.md` (futon7) using this plan's
  checkpoint format from the first checkpoint onward (Joe owns). Once
  the vocabulary and checkpoint format are fixed, Joe should not wait
  on projection or rendering to begin producing valid delta-batches.
- **(c)** Wire projection into XTDB (codex-7 owns; shares infrastructure
  with single-essay lifecycle piece #2).
- **(d)** WebArxana `arxana://view/interest-network` route (codex-7
  owns; sibling of essay-lifecycle surface).
- **(f)** First posterior update lands; verify projection consumes it
  correctly.

## 6. DOCUMENT — _pending_

---

*Plan drafted 2026-05-14 by claude-3 + codex-7 via three whistle
rounds. Event vocabulary, checkpoint format, projection design,
completeness signal: codex-7's design. Mission-doc structure +
write-up: claude-3. Joe will author `M-interim-director` separately and
return to wire it into this projection.*
