# VSATARCS — Linearised Anthology with Alignment Annotations

*Live-regenerated distillation of the VSATARCS scene-form anthology at `~/code/futon5a/holes/stories/`, annotated with the current alignment state per `~/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn`.*

**Regenerated:** 2026-06-03T08:55:09.544037348Z

**Regenerate:** `bb ~/code/futon4/scripts/generate_vsatarcs_md.bb`

**PDF:** `pandoc /home/joe/code/futon4/docs/VSATARCS.md -o ~/code/futon4/docs/VSATARCS.pdf -V geometry:margin=1in`

---

## R-criterion audit (VSATARCS-side, current state)

- **R1** — :satisfied-with-bilateral-bridge-live
- **R2** — :satisfied
- **R3** — :satisfied
- **R4** — :satisfied
- **R5** — :satisfied
- **R6** — :satisfied
- **R7** — :satisfied
- **R8** — :satisfied
- **R9** — :satisfied
- **R10** — :satisfied-xtdb-engagement-time-surface
- **R11** — :satisfied-at-observer-layer
- **R12** — :satisfied-via-stack-level-Q6-narrow-take-up-apparatus

# R1-R12 in prose — VSATARCs side, current state

*Paragraph-by-paragraph explanation of the eleven Active Inference
completeness criteria as the VSATARCs reader surface currently
satisfies (or honestly defers) them.  Each paragraph names what
exists, what doesn't, and where the blocker (if any) lives.  This
section is the long-form counterpart to the keyword-shaped
`:status` micro-qualifiers in the audit row above; the prose
version is the one to read first.*

*The criteria are R1-R12, but R11 and R12 sit somewhat outside the
inner-loop apparatus.  Read the first ten as a sequence (each
building on the previous); read R11 + R12 as outer-loop questions
about composition and reflexivity.*

## R1 — Explicit belief state

An AIF agent maintains a probability distribution over hidden state,
carried across ticks, with both mean and precision (variance)
explicitly represented.  On the VSATARCs side, `arxana-vsatarcs-belief.el`
maintains per-entity multiplicative-likelihood posteriors over the
seven-status M-INC vocabulary (`:spawned`, `:refined`,
`:strengthened`, `:addressed`, `:falsified`, `:foreclosed`,
`:reopened`).  The bridge module `arxana-vsatarcs-wm-bridge.el`
fetches the WM-side belief from the WM trace file's `:mu-post`, so
the two sides' beliefs are operationally comparable — alist-lookup
equality on the 35 shared string entity-ids (with the meta-sorry
keyword filtered as expected-only-in-WM).  The belief surface is
operator-visible via `M-x` bindings `B` (bootstrap), `R` (reset),
`i` (ingest).  **Status: satisfied with bilateral bridge live since
v0.2.5.**

## R2 — Observation channel schema

The agent senses the world through a fixed, named set of bounded
observation channels — a stable vocabulary the rest of the AIF
machinery is keyed off.  On the VSATARCs side,
`arxana-vsatarcs-observation.el` declares five essay-corpus
channels: `:story-coverage` (fraction of `stack-annotations.edn`
sections with on-disk story refs), `:lift-freshness` (decay over
mean delta between recorded and filesystem mtimes),
`:annotation-overlay-presence` (fraction of stories with sibling
`.aif.edn`), `:scene-density` (mean scenes per story, normalised),
`:link-density` (mean markdown links per scene, normalised).  All
return values in `[0, 1]`; the channel set is `defconst`-stable.
The WM side observes 13 stack-fitness channels at a different
scope; both satisfy R2's discipline at scopes that don't naturally
drift-compare across channel contents.  **Status: satisfied as of
v0.3.0.**

## R3 — Predictive-coding belief update

R3 is a composite of four sub-properties: R3a (prediction error per
observation channel), R3b (precision-weighted error), R3c
(variational free energy), R3d (belief update step).  On the
VSATARCs side, all four are now independently satisfied.  R3a and
R3c land via `arxana-vsatarcs-likelihood.el`, a direct port of
claude-2's WM-side v0.10/v0.11 architecture: per-status weight
tables map belief to predicted observation distributions; per-channel
prediction errors compose into a VFE-shape free energy with the
canonical `0.65 × accuracy + 0.35 × complexity` blend.  R3d lands
via the belief module's update step.  R3b lands via the v0.5.3
adaptive precision module's `weighted-error` re-weighting (see
R7).  Per the v0.1.1 closure protocol — sub-properties close
independently before the aggregate — R3 became the first composite
R-criterion to fully close.  **Status: satisfied as of v0.5.3.**

## R4 — Predictive forward model

A pure function `predict :: (state, action) → next-state-distribution`
lets the agent score candidate actions before taking them.  Without
this, an EFE collapses to a heuristic over current state — there's
nothing to compare candidates against.  As of v0.5.25 the VSATARCs
side ships `arxana-vsatarcs-writer-actions-predict-effects` (claude-2
M-vsatarcs-writer L3+L4 ship 2026-05-20) which dispatches across
three action classes — `:mission-doc-sync`, `:aif-edn-revision-entry`,
`:story-update` — and returns `{:pre-state :predicted-post-state}`
shapes via coarse text-level parsing of the target file.  L4's
recursive-self-landing test (file:line 144-185) exercises the full
predict → can-propose? → can-execute? → execute → re-check cycle
with prediction-error 0 on first apply and `can-propose? → nil` on
the duplicate-rev re-attempt — in-vivo demonstration that the
forward model is operational.

Mirrors WM's R4 closure discipline: WM closed at v0.3 with
"shared-kernel discipline aspirational until R6"; VSATARCs closes
at v0.5.25 with the same disposition — predict-effects is
structural-ready, becomes enforced when R6 wires the
softmax-from-EFE-score path (today's consent-gate is `:abstain-for-now`
placeholder).  Class coverage extends to `:stack-annotations-upsert`
when claude-2 wires v0.5.24's lifting-queue read-half through the
writer-class scaffold.

Per Joe directive 2026-05-20, the serial closure order is R4 →
R6 → R5 (largest gap last).  **Status: satisfied since v0.5.25.**

## R5 — Expected free energy with at least two principled terms

Expected free energy decomposes into at minimum a pragmatic / risk
term and an epistemic / ambiguity term, both scored against the
predictive forward model (R4).  As of v0.5.27, the VSATARCs side
ships `arxana-vsatarcs-efe.el` with `compute-efe` returning the
required decomposition: `:G-pragmatic` (preference / cost term;
dispatches per action-class to substrate-reading proxies) +
`:G-epistemic` (information-gain term; default recency-via-mtime
proxy across all classes) + `:G-total` as their sum.  Convention
mirrors the WM-side `futon2.aif.efe/compute-efe` v0.4 shape
(`:G-risk` + `:G-info` decomposition); lower G = more preferred.

Per-class pragmatic helpers ship for the 4 expected writer-action
classes (`:mission-doc-sync`, `:aif-edn-revision-entry`,
`:story-update`, `:stack-annotations-upsert`) — proxies are
honestly coarse today (pending-checkpoint counts; undocumented-
revisions counts; target-file age) because R5 satisfies on shape
(the apparatus computes a scored decomposition for every action
class), not on calibration (the specific numbers are
defensible-but-rough).  Same pattern as WM-side R5 closure at
v0.4 with hand-tuned terms that have evolved through v0.16+
refinement cycles.

The closure-flip operational signal: per Joe's failing-tests-as-
documentation directive, v0.5.26 shipped R6 in degenerate mode
with 3 ert tests marked `:expected-result :failed` documenting
the multi-candidate softmax + threshold-abstain semantics R5
would close.  v0.5.27 wired `arxana-vsatarcs-efe-enrich-candidates`
into `arxana-vsatarcs-r6-softmax-select-action`'s public API; the
3 failing tests flipped to passes, the `:expected-result :failed`
tags came off, and the tests now stand as live evidence that R5
closed R6's full satisfaction.  **The flip is the closure
provenance.**

R5's landing also unblocks R9's EFE-stress and Abstain-fires named
validation properties (softmax weight-monotonicity + temperature-
sharpness exercises EFE-stress; threshold-abstain + gap-report
exercises Abstain-fires).  Both ship as named tests in
`arxana-vsatarcs-r6-softmax-test.el`.  **Status: satisfied since
v0.5.27 — the last load-bearing R-criterion gap to close on the
VSATARCs side; all R1-R11 now satisfied (R12 stays deferred to
stack-level Q6).**

## R6 — Softmax action selection with abstain

Action probabilities follow `P(a) ∝ exp(−G(a) / τ)` with an
abstain branch that fires under high predictive uncertainty.  As
of v0.5.26 the VSATARCs side ships `arxana-vsatarcs-r6-softmax.el`
with three operational paths today plus one R5-pending: 0
candidates → abstain-on-empty; 1 candidate → degenerate
identity-select (rank 1, weight 1.0) — this is the L3+L4
single-candidate flow's actual selection mechanic, with the
consent-gate as the operator-visible decision surface (`:confirm`
/ `:reject` / `:ignore` / `:abstain-for-now`); 2+ candidates
without G-totals → `:r5-pending` signal; 2+ candidates with
G-totals → full softmax over `:G-total` with abstain on
threshold (reachable today only via direct call to
`--full-softmax-select` with synthetic G-totals — tests verify
the math).

The pure helpers (`--softmax-weights`) implement the WM-convention
softmax math (lower G → higher weight) at default τ 0.16
(matching WM-side `futon2.aif.policy` v0.5 default observed in
the live 2026-05-19 trace as `tau = 0.164`).

Per Joe directive 2026-05-20 ("ship the degenerate version
first, put in failing tests, and show they are fixed when we
ship R5"), the test suite includes 3 ert tests marked
`:expected-result :failed` that document the multi-candidate
softmax + threshold-abstain semantics via the public
`select-action` API.  Today these tests fail because the public
API returns `:r5-pending` for multi-candidate input lacking
G-totals.  When R5 ships and the proposer attaches G-totals, the
gate `--has-g-scores?` flips, the public API routes to
`--full-softmax-select`, and the tests start passing — ert
reports them as "unexpected pass" which is the operational
signal that R5 closed R6's full satisfaction.  **The failing
tests ARE the closure documentation**; their state flip on R5
ship is the closure evidence.

Mirrors WM-side R6 closure shape (WM v0.5: `select-action` with
adaptive τ + abstain with gap-report); the
`:independent-naming-of-same-r-criterion-shape-at-different-scopes`
evidence-kind continues to apply (WM at apparatus-cycle scope;
VSATARCs at consent-gate scope).  **Status: satisfied
(degenerate-for-single-candidate-flow) since v0.5.26; full
multi-candidate satisfaction lands when R5 ships.**

## R7 — Adaptive precision

Per-channel precision Π updates over time based on prediction-error
history.  Channels with persistent high error lose precision (the
agent learns to discount noisy channels); channels with persistent
low error gain precision.  On the VSATARCs side,
`arxana-vsatarcs-precision.el` is a direct port of claude-2's WM
v0.12 + v0.13 architecture — variance-component over a 20-element
rolling window of recent prediction errors, bounded by floor 0.1 and
cap 200, plus a need-component that scales with gap from preference
(zero on this side until R5 preferences arrive).  Precision state
persists cross-call inside the trace records themselves — the trace
IS the state store, no separate file (per claude-2's discipline
named 2026-05-19).  **Status: satisfied as of v0.5.3; the precision
cap remains unreachable until R5 lands** (variance-component-only
ceiling is `1 / min-variance` = 100 of the configured 200).

## R8 — Per-tick trace

Every tick emits a record carrying enough detail to reconstruct what
the agent did and why: timestamp, observation, prediction-errors,
chosen action, free energy, precision state.  On the VSATARCs side,
`arxana-vsatarcs-trace.el` ships an event-source-agnostic emit API
plus a cadence-following consumer (`follow-wm`) — each VSATARCs tick
lands in the same day-bucket as the WM record it follows, anchored
by `:wm-trace-anchor :line-index`.  This is the operational form of
Joe's design directive (*"docs always in line with code"* — every
WM tick has a matching VSATARCs tick at the same `:timestamp`).
Records carry observation, prediction-errors, F-total, belief-summary,
precision-state, and forward-compatible nil holes for the
writer-capability fields (`:candidates`, `:per-term-EFE`,
`:chosen-action`, `:tau`).  **Status: satisfied as of v0.4.0.**

## R9 — Named validation properties

R9 names four operationally testable properties: V-shrink (belief
variance decreases under informative observations), F-decrease (free
energy decreases on average over a run), EFE-stress (pragmatic term
rises with predicted divergence; epistemic rises for unfamiliar
observations), Abstain-fires (under high uncertainty, the agent
abstains).  On the VSATARCs side, V-shrink ships via the belief test
suite's entropy-decreases-with-accumulating-evidence assertion;
F-decrease ships at the `compute-vfe` apparatus level, where the
named-property test asserts F-total drops when belief becomes aligned
with observation via the multi-step inner loop.

EFE-stress and Abstain-fires landed v0.5.27 with R5 + R6-full.
EFE-stress is exercised by `arxana-vsatarcs-r6-softmax`'s
`weights-prefer-lower-g` and `weights-temperature-effect` tests
(perturbing G shifts the softmax weights in the predicted direction;
lowering τ sharpens the distribution toward the top-G — both are
operational stress responses).  Abstain-fires is exercised by
`full-with-all-bad-abstains` + the public-API
`multi-candidate-abstain-when-all-bad` test (when every candidate's
G-total exceeds the abstain-threshold, the apparatus emits
`:abstain-threshold` with a gap-report naming the best-G value
for operator-visible justification).

**Status: satisfied — all four named properties ship named tests as
of v0.5.27.**

## R10 — Live operation

The AIF loop runs on a recurring schedule without per-cycle
operator intervention; the trace is persisted to a queryable
store.  On the VSATARCs side this lives across two collaborating
modules: `arxana-vsatarcs-r10-tap.el` registers
`file-notify-add-watch` on the WM trace directory specifically so
`follow-wm` fires as records land (the WM trace is the one
substrate where polling lag would be operator-meaningful), and
`arxana-vsatarcs-xtdb-clicks.el` queries futon1a XTDB on every
chrome refresh for the wider engagement-time surface
(multi_watcher's `code/v05/watcher-event` hyperedges, the
invoke-job ledger, and operator-extensible additional streams).

The choice to read XTDB rather than maintain a parallel watch
substrate followed Joe's directive 2026-05-20: "we might not need a
separate *file* for those because pretty much everything is running
through the JVM and XTDB."  v0.5.17's first attempt pioneered a
local click-log EDN file via multi-dir file-notify; v0.5.18
reverted that move because multi_watcher already file-watches and
emits typed events into XTDB.  The bilateral substrate move closes
by construction — both VSATARCs and WM consume the same XTDB
hyperedges; no sidecar, no shared file, no coordination cost.

Per-stream queries are wrapped in `with-timeout` (default 2
seconds) so chrome render never blocks on a slow XTDB query — a
stream that exceeds the timeout returns `:stream-loaded? nil` and
the chrome surfaces "unavailable — futon1a server unreachable or
type slow" in that row.  Snapshot shape is stable across
server-up / server-down / mixed cases.

Operator can extend the configured stream types via
`arxana-vsatarcs-xtdb-clicks-stream-types` (e.g., adding M-INC
`state/*` streams when step (b) lands) by alist append, no code
change.  The click taxonomy formalised at
`~/code/futon2/README-clicks-and-ticks.md` §"Two primary subclasses
of click" (`:turn` interactive coding-agent turn + autonomous click
per peripheral inhabitation) lands uniformly under this projection:
watcher-event hyperedges are autonomous clicks (multi_watcher
polling), invoke-job entries are interactive turns, both flow
through the same query surface.

**Status: satisfied as of v0.5.18 — XTDB engagement-time surface
live; bilateral substrate move closes by construction.**

## R11 — Hierarchical / multi-agent composition

When multiple AIF agents act on shared state, a coordination layer
ensures their actions compose coherently.  The two-sided bilateral
milestones have constructed exactly this: WM and VSATARCs each run
their own R1-R12 stacks, both bootstrap from the same canonical
entity domain (`stack-annotations.edn :sections[]`), the bridge
reads cross-side state from the WM trace, and the
`:bilateral-evidence` block in this contract's `.aif.edn` companion
is the coordination layer's first-class artefact (eight entries on
landing; the first carries a `:protocol-witnesses` audit trail of the
bell/whistle coordination that produced it).  The original v0.1
framing called R11 `:n-a` because no second AIF surface existed at
the same scope — that's no longer honest at v0.5.6 / v0.5.7.  The
action-coherence sub-claim ("ensure their actions compose
coherently") is vacuously satisfied until either side has writer
capability; once writer capability lands, action-coherence becomes
a live question and R11 will need its first non-vacuous test.
**Status: satisfied at observer layer; action-coherence
pending writer-capability arrival on either side.**

## R12 — Dual-loop hyperparameter inference

An outer-loop agent treats the inner loop's hyperparameters
(preference priors, learning rates, EFE weights, status-weight
tables) as hidden state to infer.  This is the deepest reflexive
criterion — the agent learning about its own learning machinery.
The VSATARCs side's hyperparameters are static or hand-tuned
(status-weight tables, precision floor/cap, channel normalisation
maxes, K=3 multi-step iteration count).  Building R12 inside this
mission risks unbounded scope and presupposes both R4–R6
(writer-capability) and an explicit modelling of the inner-loop's
parameter trajectory.  Both the WM side and the VSATARCs side defer
R12 to `M-the-futon-stack Q6` (the WM-side §3.1 / T5 framing).
**Status: deferred to stack-level Q6.**

## Notes

- **Six R-criteria satisfied; one partial; three deferred pending writer-capability; one deferred to stack-level; R11 reframed honestly.**  As of v0.5.7 the VSATARCs-side audit row carries no "not satisfied" entries that are local-engineering work — every gap is either explicitly deferred (writer-capability stack, stack-level Q6) or partial-pending-deferred (R9's EFE-stress + Abstain-fires).

- **Parity with the WM contract.**  The two sides are now structurally aligned in shape: R1, R2, R3, R7, R8, R9, R10 satisfied on both; R11 reframed observer-layer-satisfied on both (the WM contract carries the same framing now); R12 deferred at stack scope on both; R4/R5/R6 differ in expected satisfaction (the WM has them via its `judge` action surface; VSATARCs has them deferred pending writer-capability).  The asymmetry is intentional — VSATARCs is reader-first; WM is writer-first — and is recorded in the bilateral-evidence entries' asymmetry notes.

- **Each story below carries a margin annotation.**  For stories already lifted into the canonical hypergraph (`stack-annotations.edn :sections[]`), the annotation names the entity-id and the active R-criteria.  For un-lifted stories the annotation flags lift-pending.  Future R-criterion landings (the deferred writer-capability stack; the stack-level Q6) will surface in these annotations automatically as new closures land in the `.aif.edn`.


## Reader-criterion audit (Q1-Q8; V-CUR / V-COV / V-COM / V-BIL)

*Parallel contract from `~/code/futon2/docs/vsatarcs-reader-criteria.md` grading VSATARCS as a **reader surface** — distinct from the R-criteria above which grade it as an AIF agent.*

- **Q1** — :satisfied [V-COV V-CUR] — What is the WM's R-criteria satisfaction state right now?
- **Q2** — :satisfied [V-CUR V-COV] — What is the WM's current top decision and why?
- **Q3** — :satisfied [V-COV V-CUR] — What anticipated events are in the WM's horizon, and what's the closest?
- **Q4** — :satisfied [V-COV] — How many sorries are in the registry, and what kinds?
- **Q5** — :satisfied [V-CUR V-BIL] — How is per-entity belief evolving across calls?
- **Q6** — :satisfied [V-COV V-CUR] — What's in the trace from this session?
- **Q7** — :satisfied [V-COV] — What's the bilateral evidence accumulated between WM and VSATARCS sides?
- **Q8** — :satisfied [V-COV] — What is the current empirical state of the futon-stack-as-Hyperreal-business mission cluster?

# Reader-criteria in prose — VSATARCs side, current state

*Paragraph-by-paragraph explanation of the eight reading-comprehension
questions Q1-Q8 from `~/code/futon2/docs/vsatarcs-reader-criteria.md`
(claude-2, 2026-05-19), grading VSATARCS as a **reader surface** —
distinct from the creator R1-R12 axis above.  Each paragraph names
what the question asks, what's rendered today, and where the closure
work lives.  This section is the long-form counterpart to the
keyword-shaped `:status` micro-qualifiers in the
`:reader-criterion-audit` row.*

*The four reader-criteria categories: **V-CUR** (Currency — does the
view reflect the latest WM state?), **V-COV** (Coverage — does
VSATARCS render the relevant state at all?), **V-COM**
(Comprehensibility — can a reader extract the answer from the
output?), **V-BIL** (Bilateral consistency — does VSATARCS's view
agree with WM's actual state?).*

## Q1 — What is the WM's R-criteria satisfaction state right now?

The reader asks: which of R1-R12 are satisfied on the WM side, and
which are deferred or partial?  The new
`arxana-vsatarcs-r-criteria-wm.el` module parses the `## Summary`
table in `~/code/futon2/docs/futon-aif-completeness.md` (claude-2's
WM AIF completeness contract) on every snapshot call and exposes a
compact per-criterion row.  The parser handles the WM table's
vocabulary: `✓` or `**✓ as of vX.Y**` → `:satisfied` (with version
extraction); `N/A` → `:n-a`; `✗` → `:not-satisfied`; anything else
→ `:unknown`.  Closed-set alignment fills missing keys with
`:unknown` so the chrome layout stays stable across contract
revisions.

Live-substrate smoke 2026-05-20: 12 rows parsed; status-counts =
{`:satisfied` 10, `:n-a` 1, `:not-satisfied` 1, `:unknown` 0}.
Per-criterion versions captured: R1 v0.2, R3 v0.11, R4 v0.3, R5
v0.4, R6 v0.5, R7 v0.12, R8 v0.7, R9 v0.7, R10 v0.8.  R11 N/A
(single observer at this scope); R12 deferred per the WM contract's
§3.1 to `M-the-futon-stack` Q6 (the WM table uses `✗` for R12; the
"deferred vs. failed" framing lives in the row's `:blocker` text
rather than in the cell symbol, which the chrome surfaces verbatim).
This matches the reader-criteria doc's quoted Q1 ground-truth
answer exactly.

The parallel VSATARCs-side audit row lives in the
`:r-criterion-audit` block above; this Q1 closure is the WM-side
projection — operator now sees both sides' satisfaction states
without context-switching between docs.  Currency follows the WM
doc's revision cadence: when claude-2 ships a new WM closure that
updates the Summary table, this chrome reflects it on the next
story open with no apparatus change.  **Status: satisfied since
v0.5.11.**

## Q2 — What is the WM's current top decision and why?

The reader asks: which action did the WM rank highest in its latest
invocation, what's the expected free energy, and what drove the
ranking?  The new `arxana-vsatarcs-wm-decision.el` module composes
with the existing `arxana-vsatarcs-wm-bridge` for the trace-file
read primitive (no bridge changes — pure composition).  Extracts
the latest record's `:decision` field — action type, target, rank,
G-total, tau, weight, rationale — plus the strategic `:mode` and
the record `:timestamp`.  The digest-line distinguishes four cases
(no-trace / empty-trace / no-decision-field / decision-present) so
the chrome has stable shape on every WM-side state.

Live-substrate smoke 2026-05-20 against
`~/code/futon2/data/wm-trace/wm-trace-2026-05-19.edn`: action =
`:address-sorry`, target = `:sorry/wm-aif-substrate-addressability`,
rank = 1, G-total = -4.208, tau = 0.164, mode = `:multiplied`,
rationale = "open sorry: WM action types need addressable substrate;
meta-sorry registers itself as the first sorry."  Matches the
reader-criteria doc's quoted Q2 ground-truth answer exactly
(`:address-sorry :sorry/wm-aif-substrate-addressability (G ≈
-4.21)`).

Currency follows the WM-side cadence via R10's existing file-notify
tap (shipped at v0.5.2): when the WM emits a new trace record, the
tap fires `follow-wm` on this side, and the next chrome refresh
reflects the new decision.  No new wakeup mechanism needed — the
substrate Q2 reads from is the same trace the existing R10
apparatus already subscribes to.

**Composition pattern**: Q2 is the first reader-criterion closure
to depend on another reader-criterion module (the bridge) rather
than only on canonical source files.  Q5/Q6 (drift-over-time +
recent-trace fields) will follow the same composition pattern with
the same read primitive — both are bridge-extensions, both close on
the same trace substrate Q2 already reads.

**Extension v0.5.13** (per claude-2's handoff 2026-05-20):
top-K (default K=3) alternatives surfaced from `:ranked-actions`
plus a composition section explaining the EFE parameters that
produced the G-total — chosen-action `:time-pressure`,
`:horizon-steps`, µ-shift count (entities where `:mu-pre`
differs from `:mu-post`, signalling R3d's global belief update
fired).  Operator-facing question becomes "what was decided AND
why (vs the close alternatives)?" rather than just "what was
decided?"  Live smoke on the same 2026-05-19 trace now surfaces
an operational signal that wasn't visible in the verbatim view:
the top-3 actions are all tied at G=-4.208 (a degenerate-tied
bucket of `:address-sorry` candidates against different
`:sorry/...` targets), µ-shift = 12 entities (R3d fired), mode
`:multiplied`.  When the `:gap-report` field is non-empty (abstain
branch fired), it surfaces in place of the alternatives — the
operator-facing answer to "why didn't the WM act" is the gap list
itself.  **Status: satisfied since v0.5.12 (extended v0.5.13).**

## Q3 — What anticipated events are in the WM's horizon, and what's the closest?

The reader asks: which forward-axis events are coming up in the
default 30-day horizon, what are their firing priors, and what's the
aggregate time-pressure on the WM?  The new
`arxana-vsatarcs-anticipation.el` module reads
`~/code/calendar/events.edn` directly (the canonical forward-axis
substrate per `M-interim-director-proxy-metric-inventory` §2.A.2.38;
events.edn is the source of truth and any Google Calendar / .ics
projection is generated downstream) and exposes a snapshot the
chrome renders alongside the belief snapshot.  Per-event time-pressure
follows the WM-side `futon2.aif.anticipation/time-pressure`
convention: linear ramp from 0 at horizon-days to 1 at the firing
instant, weighted by `:event/p-fires`.  Aggregate horizon
time-pressure is the max over in-horizon events.  Live-substrate
smoke 2026-05-20: three events in `events.edn`; two in horizon
(Eric scoping meeting 5.5d out at p=0.95, tp=0.777; Glasgow Cogito
lifecycle deadline 8.5d out at p=0.40, tp=0.287); ICARM filtered as
past; aggregate horizon time-pressure 0.777 (matches the doc's
quoted ~0.78 from the 2026-05-19 vantage point, when Eric was 7d
out instead of 5.5d).  During this smoke an upstream fix was
authorised on `events.edn`: line 159 had a misplaced `]` closing
the `:events` vector after only two events; the fix moves the
vector terminator past event three.  A latent infinite-loop bug in
the shared EDN reader (`arxana-browser-rewrites--read-edn-file`) on
unbalanced delimiters was also noted as an independent followup.
**Status: satisfied since v0.5.8.**

## Q4 — How many sorries are in the registry, and what kinds?

The reader asks: how many open sorries are tracked, and what's
their distribution across `:kind` (meta / prototyping-forward /
technical-debt / decision-debt / external-dependency)?  The new
`arxana-vsatarcs-sorrys.el` module reads `~/code/futon2/data/sorrys.edn`
(v2 schema with `:kind` field per Joe 2026-05-18) on every snapshot
call and exposes per-kind counts, per-status counts, and a
by-mission filter.  The kind-count row is stable across snapshots —
even when some kinds are absent from the current registry, the row
carries an entry for each schema-declared kind plus an `unknown`
bucket — so the V-COM property of "operator can extract the answer
from the output" is met without depending on registry contents.
Live-substrate smoke 2026-05-20: 12 sorries (1 meta + 11
prototyping-forward, all open, all listing
`M-war-machine-aif-completion` in their `:related-missions`).

Today's registry is **hand-curated**: new entries join by operator
hand-edit, not by automated extraction.  The natural upstream for
populating future entries from agent interactions is
**`M-a-sorry-enterprise`** (`~/code/futon5a/holes/missions/M-a-sorry-enterprise.md`,
IDENTIFY → MAP).  That mission mines per-turn pattern retrieval
from agent conversations into evidence certificates, then
correlates retrieved patterns with sorry-pattern affinity to
predict closure trajectories — the direct extraction discipline
this registry would consume to grow without operator intervention.
The bridge between mining output and registry ingestion is the
M-INC event vocabulary (`state/spawned`, `state/addressed`,
`state/foreclosed`, …) from
**`M-interest-network-coupling`** (`~/code/futon4/holes/missions/
M-interest-network-coupling.md`, step (b) pending) — the same
vocabulary the VSATARCs belief module's multiplicative-likelihood
update consumes, so the registry and the belief surface couple
through this vocabulary.

Story-scoped filtering (showing only sorries whose
`:related-missions` overlap with the current story's mission
references) is the v0.3 lift that fully closes V-COM.  Today's
chrome shows the global registry on every story open.
**Status: satisfied since v0.5.9.**

## Q5 — How is per-entity belief evolving across calls?

The reader asks: which entities have moved most over recent ticks?
The new `arxana-vsatarcs-wm-recent.el` module (shipped jointly with
Q6 in v0.5.14) reads the last N records from today's WM trace via
the bridge's newly-added `--read-all-records` primitive, computes
max-abs-diff trajectories across consecutive `:mu-post` posteriors
for each entity present in EVERY record in the window (the
intersection-of-shared-entity-ids discipline keeps the trajectory
honest — entities that appear midway can't carry a full-window
trajectory), then surfaces the top-K-most-moved with their drift
magnitudes.  Entities below a small drift floor (default
configurable) drop out as quiescent.

Live-substrate smoke 2026-05-20 against
`~/code/futon2/data/wm-trace/wm-trace-2026-05-19.edn`: 9 records on
disk; 5-record default window; 47 shared entities; **5 entities
tied at max-abs-diff 0.1681**.  The tie is structural: R3d's global
update applies an annealed-weight uniform shift to every entity in
the moved bucket, so all moved entities move by the same magnitude.
This is consistent with claude-2's v0.16 multi-channel
sign-aggregation pattern — and the chrome's surfacing it confirms
the apparatus is firing as designed.

Compares to (but does not replace) the cross-side
`arxana-vsatarcs-wm-bridge-compare-with-local` which answers the
*local-vs-WM in current state* version of this question.  Q5 is
*WM history over its own trace*; the bridge's compare is
*WM-now vs VSATARCS-now*.  Different questions, complementary
answers.  **Status: satisfied since v0.5.14.**

## Q6 — What's in the trace from this session?

The reader asks: what records did the WM emit today, and what do
they carry?  Same `arxana-vsatarcs-wm-recent.el` module (jointly
landed with Q5 because both project from the same window) surfaces
seven fields per record: `:timestamp`, `:mode`, `:decision-action`,
`:decision-target`, `:G-total`, `:tau`, chosen action's
`:time-pressure`, and the per-record µ-shift count (entities whose
mu-pre/post differ within the call — Q2's per-record metric
extended across the window).

Live-substrate smoke 2026-05-20: 5 most recent records on
2026-05-19; all chose `:address-sorry
:sorry/wm-aif-substrate-addressability`; G-totals oscillated across
`{-4.500, -3.648, -4.208, -4.208, -4.208}`.  The chrome reveals an
*operational trajectory* — the WM landed on G=-4.208 and stayed
there for the last three records, with a brief detour through
G=-3.648 between the first and second.  That structural pattern
(detour then convergence) isn't visible in the latest-record-only
Q2 view.

Per-channel `:prediction-errors` and the full 15-entry
`:ranked-actions` list are deferred from the chrome — operator can
read them in source.  The chrome is a digest, not a data dump; if
those fields prove operator-meaningful at chrome layer, a
`:full-record-drill-down` v0.5.x extension can surface them
on-demand.  **Status: satisfied since v0.5.14.**

## Q6 — What's in the trace from this session?

The reader asks: what records did the WM emit today, and what do
they contain?  The bridge reads the latest record's `:mu-post`; the
remainder of each trace record (`:decision`, `:free-energy`,
`:prediction-errors`, `:precision-state`, `:ranked-actions`,
`:anticipation`, `:mode`) is ignored.  Closure path: extend the
bridge to surface these fields and add a recent-trace block to the
chrome with operator-navigable links to drill into a specific
record's fields.  **Status: partial via bridge (`:mu-post` only).**

## Q7 — What's the bilateral evidence accumulated between WM and VSATARCS sides?

The reader asks: which closures on each side have been paired into
the `:bilateral-evidence` audit trail, and under which
`:evidence-kind`?  The data lives in the top-level
`:bilateral-evidence` block of this same `.aif.edn` — canonical
home; WM-side does not duplicate (per the v0.2.5 closure rationale).
The new `arxana-vsatarcs-bilateral.el` module reads the block on
every snapshot call, exposes per-kind counts over the closed set
(`:independent-naming-of-same-principle`, `:joint-landing`,
`:independent-naming-of-same-r-criterion-shape-at-different-scopes`,
`:one-sided-extension`, plus the reserved
`:coordinated-empirical-observation`), and surfaces the count of
entries carrying `:protocol-witnesses` (cross-side coordination
audit trail captured in the source).

Live-substrate smoke 2026-05-20: 8 entries; distribution = 1
independent-naming, 2 joint-landings, 2 same-r-criterion-shape, 3
one-sided extensions; 2 entries carry `:protocol-witnesses` (the
R10 wakeup-tap and R3a likelihood closures — both bell/whistle-
coordinated joint landings).  The chrome marks witness-carrying
entries with a ★ so a reader can spot which closures have explicit
turn-by-turn cross-side traces vs. those that landed via inference
on already-deployed counterpart work.

Self-referential closure shape: the renderer reads the same file
that records the closure itself.  When v0.5.10 gains its own
`:bilateral-evidence` entry — anticipated when claude-2 acknowledges
or extends the reader-criteria axis from the WM side — the entry
will appear in the chrome that the entry annotates.  Recursive-but-
not-circular, because the file is read after writes complete; no
on-line cycle.

**Status: satisfied since v0.5.10.**

## Q8 — What is the current empirical state of the futon-stack-as-Hyperreal-business mission cluster?

The reader asks: where is each of the related missions
(`M-war-machine-aif-completion`, `M-stack-essay-code-alignment`,
`M-stack-essay`, `M-stack-morphogenetic-rewrite`) in the IDENTIFY /
MAP / DERIVE / DOCUMENT lifecycle, and what's the cross-mission
dependency state?  The new `arxana-vsatarcs-cluster.el` module parses
each mission's markdown for the top-level `**Status:**` header, the
highest `## N. STAGE` heading reached, and `### Checkpoint N`
headings with `**Status: COMPLETE` detection.  Default cluster
configuration includes the three missions with mission files; the
fourth named member (`M-stack-essay`) has no separate mission file
— it IS the essay-side work the VSATARCs apparatus is — and is
documented in the audit row rather than parsed as a mission.

Live-substrate smoke 2026-05-20: 3/3 missions loaded; all at
`:identify` stage (mission scaffolds carry only `## 1. IDENTIFY`
headings; no later stages instantiated by the scaffolding pass);
6/17 checkpoints complete cluster-wide — `M-war-machine-aif-completion`
at 6/7, `M-stack-essay-code-alignment` at 0/4,
`M-stack-morphogenetic-rewrite` at 0/6 (gated on cluster siblings
closing).

The chrome surfaces an **honest asymmetry**: the alignment mission
shows 0/4 checkpoints complete in its mission markdown, while this
`.aif.edn` records 11+ closures landed for the same mission (v0.5.0
through v0.5.15).  Mission scaffolds were written by claude-1 in the
PM-pass phase as `HEAD-as-escrow scaffold`; the typed closures live
in this `.aif.edn` overlay rather than in the markdown's checkpoint
status lines.  Q8's chrome reports the markdown-recorded state
honestly (V-COV against the docs as source-of-truth); the
typed-closure state lives in this file's other blocks.  An operator
who reads both surfaces sees the gap; an operator who reads only
this block sees the scaffold disposition.  Sync convention between
the two surfaces is an open design choice — either push closure
landings from the `.aif.edn` into the mission docs' checkpoint
status lines, or pull at chrome layer at render time.  Deferred.

**Status: satisfied since v0.5.15** — final reader-criterion closure.
All 8 questions (Q1-Q8) now have a satisfying chrome surface; the
remaining work on the reader-criteria axis is bilateral-evidence
entries for Q5+Q6+Q8 (deferred from earlier batches), the
midnight-UTC-bug entry claude-2 recommended, and refinements from
the next review cycle.

## Cross-cutting note

Reader-criteria closures are deliberately **lighter-weight than
R-criteria advancement** — most of the lifting is "read EDN that
already exists, render in chrome".  No new state machinery, no new
AIF mathematics.  The discipline they enforce is operator-visibility:
a VSATARCs that satisfies R1-R12 but doesn't surface the answers to
Q1-Q8 has done the inner-loop work without serving the reader.  The
v0.5.8 landing of Q3 establishes the pattern (read canonical
substrate → expose a clean snapshot API → render alongside belief);
Q4 and Q7 follow the same shape directly, and Q2/Q5/Q6 are
incremental extensions of the existing bridge.


## Bilateral evidence (cross-side correspondences)

1. **:capability-gap-modeling** — `hx:vsatarcs-align:v0-2-3:enables-renderer-closure` ↔ `futon-aif-completeness.md §Capability-gap-modeling-as-endogenous-action` (landed 2026-05-17)
2. **:symmetric-bootstrap-of-shared-entity-domain** — `hx:vsatarcs-align:v0-2-5:cross-side-bridge-closure` ↔ `hx:wm:v0-9:symmetric-bootstrap-closure` (landed 2026-05-18)
3. **:r2-observation-schema** — `hx:vsatarcs-align:v0-3-0:r2-observation-channels-closure` ↔ `futon2.aif.observation/observation-channels` (landed 2026-05-18)
4. **:r8-trace-persistence** — `hx:vsatarcs-align:v0-4-0:r8-trace-cadence-following-closure` ↔ `futon2.aif.trace` (landed 2026-05-18)
5. **:r7-adaptive-precision-via-wm-port** — `hx:vsatarcs-align:v0-5-3:r7-adaptive-precision-closure` ↔ `futon2/src/futon2/aif/precision.clj (WM v0.12 + v0.13 multi-step integration)` (landed 2026-05-19)
6. **:r10-live-operation-via-event-driven-wakeup** — `hx:vsatarcs-align:v0-5-2:r10-wakeup-tap-closure` ↔ `WM-side R10 cron-install (pending) + futon-aif-completeness.md §R10` (landed 2026-05-19)
7. **:r9-f-decrease-named-property** — `hx:vsatarcs-align:v0-5-1:r9-f-decrease-closure` ↔ `futon2/test/futon2/aif/r9_named_validation_test.clj` (landed 2026-05-19)
8. **:r3a-likelihood-via-ants-port** — `hx:vsatarcs-align:v0-5-0:r3a-likelihood-closure` ↔ `futon-aif-completeness.md §R3 v0.10 + v0.11 landing` (landed 2026-05-19)
9. **:forward-axis-substrate-consumed-by-both-sides** — `hx:vsatarcs-align:v0-5-8:q3-anticipation-block-closure` ↔ `futon-aif-completeness.md §R5 v0.14 (anticipation-driven time-pressure) + futon2.aif.anticipation namespace` (landed 2026-05-20)
10. **:reader-projection-over-canonical-substrate** — `hx:vsatarcs-align:v0-5-9:q4-sorry-registry-closure` ↔ `futon2/data/sorrys.edn (v2 schema with :kind per Joe 2026-05-18)` (landed 2026-05-20)
11. **:bilateral-evidence-renderer-reads-its-own-substrate** — `hx:vsatarcs-align:v0-5-10:q7-bilateral-evidence-renderer-closure` ↔ `this same `:bilateral-evidence' block (canonical VSATARCS-side home per v0.2.5)` (landed 2026-05-20)
12. **:cross-side-status-projection** — `hx:vsatarcs-align:v0-5-11:q1-wm-r-criteria-status-row-closure` ↔ `futon-aif-completeness.md §Summary table` (landed 2026-05-20)
13. **:r3-channel-coverage-asymmetric-by-design** — `hx:vsatarcs-align:v0-5-7:r3-aggregate-status` ↔ `futon-aif-completeness.md §R3 v0.11 (4/14 channels) + 10 :prototyping-forward sorries in sorrys.edn` (landed 2026-05-20)
14. **:reader-projection-over-windowed-trace** — `hx:vsatarcs-align:v0-5-14:q5-q6-recent-trace-and-drift-closure` ↔ `futon2.aif.trace (EDN-lines daily-rotated per-call records); WM-side trace records carry every field VSATARCs reads (:mu-post, :decision, :ranked-actions, :anticipation, :mode, :timestamp)` (landed 2026-05-20)
15. **:cross-mission-status-projection** — `hx:vsatarcs-align:v0-5-15:q8-mission-cluster-overview-closure` ↔ `claude-2 + claude-1 PM-pass scaffolds at ~/code/futon7/holes/M-*.md (M-war-machine-aif-completion, M-stack-essay-code-alignment, M-stack-morphogenetic-rewrite)` (landed 2026-05-20)
16. **:bounded-utc-window-for-trace-continuity-across-midnight** — `futon4/dev/arxana-vsatarcs-trace.el `follow-wm' (v0.5.0 adjacent trace bugfix)` ↔ `futon2.aif.trace/latest-trace-record (bounded 2-day UTC window; per `M-war-machine-aif-last-mile.md' §2.B [x fixed 2026-05-20])` (landed 2026-05-20)
17. **:chrome-shape-coordinated-via-handoff** — `hx:vsatarcs-align:v0-5-13:q2-extension-top-k-and-composition` ↔ `claude-2 chrome-shape recommendation in handoff-2026-05-20-claude-2-to-claude-4-reader-criteria-acks.md §Ask 3` (landed 2026-05-20)
18. **:substrate-already-exists-do-not-pioneer** — `hx:vsatarcs-align:v0-5-18:xtdb-substrate-redirect-closure` ↔ `claude-2 structural model + multi_watcher hyperedges (`code/v05/watcher-event' family already emitted into futon1a XTDB per phase-4.5 landing); surfaced via Joe directive 2026-05-20: 'we might not need a separate file because pretty much everything is running through the JVM and XTDB'` (landed 2026-05-20)
19. **:consent-gate-substitutability-validated-on-recursive-self-landing** — `hx:vsatarcs-align:v0-5-22:consent-gated-writer-event-first-activation` ↔ `M-vsatarcs-writer L4 :aif-edn-revision-entry shipment (claude-2, 2026-05-20); L4 Checkpoint 2 in M-vsatarcs-writer §6.g; 6 new tests in `arxana-vsatarcs-aif-edn-sync-test.el', recursive-self-landing test at lines 144-185` (landed 2026-05-20)
20. **R12 narrow take-up landed bilaterally as symmetric apparatus.  WM-side reference (claude-9): atom + Beta(α,β) posterior + bootstrap-replay from `code/v05/wm-hyperparameter-update` hyperedges + outer-loop scheduler + cron install + `action_proposer.clj:39` consumption.  VSATARCs-side port (claude-4 v0.5.31): symmetric atom + bootstrap-replay from new `code/v05/vsatarcs-hyperparameter-update` hyperedges + `arxana-vsatarcs-efe.el's` `--g-pragmatic' consumption with the `(credit / 0.5)' prior-identity modulator.  Both R12 contract rows now flipped to `:satisfied-via-stack-level-Q6-narrow-take-up-apparatus' on their respective sides; first bilateral closure of an R-criterion via stack-level-Q6 deferral.** — `hx:vsatarcs-align:v0-5-32:r12-narrow-take-up-bilateral-symmetric-port` ↔ `futon2.aif.intrinsic-values (claude-9 2026-05-21) ↔ arxana-vsatarcs-intrinsic-values (claude-4 2026-05-21 v0.5.31)` (landed 2026-05-21)
21. **LC1 POC closure ships the discoverability-surface sub-shape of the `stack-self-documentation' pattern family (first concrete instance).  Live walk-through validated the load-bearing architectural pattern: MiniLM-cosine (v1) **completely missed** `M-a-sorry-enterprise` on Joe's query `"a sorry enterprise"` (v1_rank=None); hypergraph-typed-slot structural retrieval (v1.1) ranked it #1; combined-fusion buried at #3 due to cosine dominance.  The divergence-as-diagnostic emit honestly captured the disagreement (confidence=0.3; v1.1-only signal).  **M-pattern-mining's 'MiniLM-cosine-is-artifact-not-signal' verdict playing out live;** Option III's combining-as-diagnostic surfacing the artifact exactly as designed.  Empirical witness for the combining-methods-as-diagnostic feedback Joe canonicalised.** — `hx:vsatarcs-align:v0-5-32:lc1-self-documenting-stack-poc-close-acknowledgement` ↔ `M-self-documenting-stack POC close 2026-05-21 (claude-2 closing; codex-5 implementation)` (landed 2026-05-21)
22. **:pilot-cross-agent-excursion-handoff** — `hx:vsatarcs-align:auto:pilot-e-street-sweeper-excursion` ↔ `M-war-machine-pilot v1 cycle-5 excursion-handoff` (landed 2026-05-25)
23. **:pilot-envelope-capability-demonstration** — `hx:vsatarcs-align:auto:pilot-event-envelope-exercise-v0-close-demo` ↔ `M-war-machine-pilot consent-gated event cg-8367e269-70fd-479f-9522-cf87171b903c` (landed 2026-05-25)
24. **:pilot-inhabitation-substrate-bootstrap** — `hx:vsatarcs-align:auto:pilot-event-inhab-claude-1-cycle-3-substrate-bootstrap` ↔ `M-war-machine-pilot pilot-inhabitations substrate bootstrap` (landed 2026-05-25)
25. **:pilot-placeholder-to-living-view** — `hx:vsatarcs-align:auto:pilot-event-inhab-claude-1-cycle-cg-686fdc10-anchor-0011-reframe` ↔ `M-war-machine-pilot consent-gated event cg-686fdc10-73ac-44bc-a665-5e98f31cf98d` (landed 2026-05-25)
26. **:pilot-recursive-substrate-repair** — `hx:vsatarcs-align:auto:pilot-event-inhab-claude-9-cycle-408d02d1-9985-4f84-9136-20b85b9f30e8` ↔ `M-war-machine-pilot cycle 408d02d1-9985-4f84-9136-20b85b9f30e8` (landed 2026-05-25)
27. **:pilot-symptom-to-root-traceback** — `hx:vsatarcs-align:auto:pilot-event-inhab-claude-9-cycle-408e44ed-c1d6-4b61-bbc0-f37babfb5b69` ↔ `M-war-machine-pilot cycle 408e44ed-c1d6-4b61-bbc0-f37babfb5b69` (landed 2026-05-25)
28. **:pilot-inhabitation-log-ui-swap** — `hx:vsatarcs-align:auto:pilot-inhabitation-log-ui-swap` ↔ `M-war-machine-pilot v1 cycle-3 UI swap (anchor 0011)` (landed 2026-05-25)
29. **:pilot-override-mode-realised-in-code** — `hx:vsatarcs-align:auto:pilot-stop-the-line-implementation` ↔ `M-war-machine-pilot v1 cycle-4 implementation` (landed 2026-05-25)
30. **:pilot-override-mode-vocabulary-extension** — `hx:vsatarcs-align:auto:pilot-stop-the-line-vocabulary` ↔ `M-war-machine-pilot v1 cycle-4 vocabulary-extension` (landed 2026-05-25)
31. **:pilot-branch-isolated-code-modification-excursion** — `hx:vsatarcs-align:auto:pilot-e-night-shift-excursion` ↔ `M-war-machine-pilot v1 cycle-6 night-shift excursion` (landed 2026-05-25)

## Revisions trail

- **v0.5.33** (2026-05-21) — **Substrate-signal marginpars** (Joe directive 2026-05-21 — option (B) of the marginpar-upgrade fork; option (A) selective .aif.edn authoring deferred until B is seen-in-vivo).  The 30+ stub-lifted...
- **v0.5.32** (2026-05-21) — **Issue queue seeded + two bilateral-evidence entries added.**  Joe directive 2026-05-21 ('I'd like to see remaining items sorted into some kind of clearly delimited issue queue — this could exist ...
- **v0.5.31** (2026-05-21) — **VSATARCs port of the narrow R12 take-up — bilateral apparatus now fully symmetric.**  v0.5.30 (claude-9) shipped the WM-side reference and named the consumption-site work in `arxana-vsatarcs-efe....
- **v0.5.30** (2026-05-21) — **Bilateral R12 closure via M-the-futon-stack Q6 narrow take-up (apparatus).**  Handoff `~/code/futon0/holes/handoffs/r12-to-stack-Q6-2026-05-21.md' from claude-4 named this — both AIF apparatuses ...
- **v0.5.29** (2026-05-21) — **Write-half handoff taken locally** — Joe directive 2026-05-21 ('I think we'll have to do the half-write handoff b/c claude-2 has moved onto a new mission!').  The v0.5.28 forward-pointer named `:...
- **v0.5.28** (2026-05-21) — **First in-vivo exercise of the R1-R11 + Q1-Q8 apparatus** per Joe directive 2026-05-20 ('this will properly exercise the R1-R11 Q1-Q8 engine we just built').  Context: Joe authored Operator's Fore...
- **v0.5.27** (2026-05-20) — **R5 closes** — the load-bearing missing module Joe named as the biggest outstanding gap.  New module `arxana-vsatarcs-efe.el' ships `compute-efe' with two principled terms: `:G-pragmatic' (prefere...
- **v0.5.26** (2026-05-20) — **R6 closes in degenerate mode** per Joe's serial-closure ordering 2026-05-20 (R4 → R6 → R5; Joe directive: 'ship the degenerate version first, put in failing tests, and show they are fixed when we...
- **v0.5.25** (2026-05-20) — **R4 closes on the VSATARCs side** per Joe's serial-closure ordering 2026-05-20 ('R4 first → close it; then R6 → close it; then tackle R5 as the biggest gap').

R4 audit row moves `:deferred-pendin...
- **v0.5.24** (2026-05-20) — **Lifting-queue read-surface lands** per Joe directive 2026-05-20 ('we should have a queue of non-lifted annotations and AIF should tell us to dispatch them').  Joe observed the v0.5.23 narrative-c...
- **v0.5.23** (2026-05-20) — **Narrative-coverage work** in response to claude-2's coverage-gap diagnostic 2026-05-20 (Joe-surfaced; relayed via Agency).  Diagnostic: VSATARCs's story surface at `~/code/futon5a/holes/stories/`...
- **v0.5.22** (2026-05-20) — **6th `:evidence-kind :consent-gated-writer-event' activated** in response to claude-2's M-vsatarcs-writer L4 `:aif-edn-revision-entry' ship 2026-05-20 (whistle relayed via Joe).  Three coordinated...
- **v0.5.21** (2026-05-20) — **Two concrete polish moves under Joe's 'do the within-my-scope items' direction** after the v0.5.20 bilateral coordination cleanup.  (1) **Bridge effective-sign projection** — closes the v0.5.13 R...
- **v0.5.20** (2026-05-20) — **v0.5.18-redirect bilateral-evidence entry drafted** at claude-2's request (bell response 2026-05-20 — first whistle from CLI after they re-registered on Agency).  New entry `hx:vsatarcs-align:v0-...
- **v0.5.19** (2026-05-20) — **R10 parallel-async + wider-defaults polish** under Joe directive 2026-05-20 (`do 1 and 2 from your list and we can save #3 for coordination with claude-2'): (1) parallel async XTDB queries via `a...
- **v0.5.18** (2026-05-20) — **v0.5.17 → v0.5.18 substrate redirect** per Joe directive 2026-05-20: '[the] heartbeat.edn is interesting because that would allow us to incorporate non-:turn events (like file updates, git commit...
- **v0.5.17** (2026-05-20) — **R10 multi_watcher tap lands on VSATARCs side** — first concrete move under Joe's `the others' axis split after the writer-capability work split to claude-2.  Two new modules: (1) `arxana-vsatarcs...
- **v0.5.16** (2026-05-20) — **Reader-criteria-axis bilateral-coordination cleanup batch** responding to claude-2's review handoff (`handoff-2026-05-20-claude-2-to-claude-4-reader-criteria-axis-review.md`).  Four moves: (1) **...
- **v0.5.15** (2026-05-20) — **Q8 (mission-cluster overview) closure lands as the eighth and final reader-criterion shipment.**  All 8 reader-criterion questions Q1-Q8 are now `:satisfied' on the VSATARCs side.  New module `ar...
- **v0.5.14** (2026-05-20) — **Q5 + Q6 joint closure** lands as the sixth/seventh reader-criterion shipment (bundled per the plan since both need the same primitive — read recent records — and project from the same window).  N...
- **v0.5.13** (2026-05-20) — Three coordinated moves responding to claude-2's handoff 2026-05-20 (`handoff-2026-05-20-claude-2-to-claude-4-reader-criteria-acks.md').  (1) **Q2 extension** per claude-2's chrome-shape recommenda...
- **v0.5.12** (2026-05-20) — Q2 (WM decision surfacing) closure lands as the fifth reader-criterion shipment.  New module `arxana-vsatarcs-wm-decision.el' composes with the existing `arxana-vsatarcs-wm-bridge' for the trace-fi...
- **v0.5.11** (2026-05-20) — Q1 (WM R-criteria status row) closure lands as the fourth reader-criterion shipment.  New module `arxana-vsatarcs-r-criteria-wm.el' parses the `## Summary' table from `~/code/futon2/docs/futon-aif-...
- **v0.5.10** (2026-05-20) — Q7 (bilateral-evidence renderer) closure lands as the third reader-criterion shipment.  New module `arxana-vsatarcs-bilateral.el' reads the top-level `:bilateral-evidence' block from this same `.ai...
- **v0.5.9** (2026-05-20) — Q4 (sorry-registry) closure lands as the second reader-criterion shipment.  New module `arxana-vsatarcs-sorrys.el' reads `~/code/futon2/data/sorrys.edn' (schema v2 with `:kind' field per Joe 2026-0...
- **v0.5.8** (2026-05-20) — Pivot to reader-criteria axis per claude-2's `~/code/futon2/docs/vsatarcs-reader-criteria.md' (drafted 2026-05-19) — a parallel contract grading VSATARCS as a READER surface (not as an AIF agent). ...
- **v0.1.0** (2026-05-17) — Initial baseline: 6 annotations, schema-refactor header, R-criterion-audit block.
- **v0.1.1** (2026-05-17) — Resolved two open questions from v0.1.0 in dialogue with Joe.
- **v0.5.7** (2026-05-19) — Audit-row honesty pass + R-criteria-in-prose section. (1) R11 `:n-a' → `:satisfied-at-observer-layer' — the original v0.1 framing was honest before the bilateral bridge existed; no longer honest at...
- **v0.5.6** (2026-05-19) — HTML distillation polish per Joe's 'keep working on the HTML output' direction: (a) flattened section structure — story-section contains scene h3s + paragraphs as direct children (matches `ati.html...
- **v0.5.5** (2026-05-19) — HTML sibling artefact at `~/code/futon7a/vsatarcs.html` per Joe's pivot to the Tufte-CSS-based public-facing site (`futon7a` was the right surface; its tufte.css + latex.css already in place from t...
- **v0.5.4** (2026-05-19) — Linearised reader-facing distillation per Joe's pivot to operator-visible alignment. New artefact `~/code/futon4/docs/VSATARCS.md` (7936 lines, regenerated on demand) collates all 47 scene-form sto...
- **v0.5.3** (2026-05-19) — R7 (adaptive precision) satisfied via direct port of claude-2's WM v0.12 + v0.13 `futon2/src/futon2/aif/precision.clj' architecture.  New module `arxana-vsatarcs-precision.el' maintains per-channel...
- **v0.5.2** (2026-05-19) — R10 satisfied via minimum-viable wakeup tap. New module `arxana-vsatarcs-r10-tap.el' registers `file-notify-add-watch' on the WM trace directory; handler fires `arxana-vsatarcs-trace-follow-wm' on ...
- **v0.5.1** (2026-05-19) — R9 F-decrease wired as a named validation property (sequential ship per claude-2's whistle 2026-05-19; no WM-side dependency). `arxana-vsatarcs-trace-build-record' extended with `:F' + `:prediction...
- **v0.5.0** (2026-05-19) — R3a + R3c satisfied via likelihood-model port (bilateral joint-landing milestone v0.5.0 ↔ WM v0.10+). New module `arxana-vsatarcs-likelihood.el' ports claude-2's WM-side architecture (per-status we...
- **v0.4.0** (2026-05-18) — R8 satisfied. New module `arxana-vsatarcs-trace.el' ships event-source-agnostic emit + cadence-following consumer per Joe's design directive. 15 fixture tests (3 emit-roundtrip + 3 schema-stability...
- **v0.3.0** (2026-05-18) — R2 satisfied. New module `arxana-vsatarcs-observation.el' declares a 5-channel schema (:story-coverage, :lift-freshness, :annotation-overlay-presence, :scene-density, :link-density), per-channel pu...
- **v0.2.5** (2026-05-18) — Cross-side bridge lands (bilateral milestone v0.9 ↔ v0.2.5). New module `arxana-vsatarcs-wm-bridge.el' reads WM-side `:mu-post' from `~/code/futon2/data/wm-trace/wm-trace-YYYY-MM-DD.edn'; converts ...
- **v0.2.4** (2026-05-18) — R1 polish-off pass. Three coordinated moves: bilateral comparison primitive (`arxana-vsatarcs-belief-compare', pure function returning drift report); reader mode-map bindings `B'/`R'/`i' for operat...
- **v0.2.3** (2026-05-18) — RewriteReview renderer extended to surface the `:enables' field in the Rule panel (between Verification and Decided-by). Structural convergence with WM v0.5's `:learn-action-class' capability-gap-m...
- **v0.2.2** (2026-05-18) — R1 prior bootstrap from `stack-annotations.edn'. Belief snapshot now populates on first open (was empty until events landed). Three new entry points in belief.el; EDN reader extended in arxana-brow...
- **v0.2.1** (2026-05-17) — Refactored the v0.2 closure's :operations :with-entities to the npt v12 prose-entity field convention (:id/:type/:from-augmentation/:text). Functional contents unchanged — same 7 entities across 4 ...
- **v0.2.0** (2026-05-17) — R1 reader-chrome integration shipped. v0.1 → v0.2 recorded as a single typed-rewrite closure cited under the meta-pattern `system-coherence/verified-rewrite-from-diagnostic-annotation`.

## How to read this document

Each story below opens with a **blockquote alignment callout** naming whether it's lifted into the canonical hypergraph and (if so) which R-criteria currently apply to its entity domain.  Marginal alignment annotations are inline blockquotes because Markdown lacks true margins; PDF generation via pandoc can render these into wide-margin marginalia if desired (`pandoc ... --variable=geometry:margin=2.5in`).

**Future integration points** named in closure `:enables` blocks (R5 preferences; multi_watcher full tap; bridge precision/F trajectory comparison) will surface here automatically as new closures land in the `.aif.edn`.

---
## Start Here — The Futon Anthology

*Source: `~/code/futon5a/holes/stories/leaf-start-here.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/start-here` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

You are at the top of the Futon Anthology. Thirty-five stories
cover the stack's missions, pillars, and per-repo devmaps in
narrative scene form. This page is the **holding place** — the
single anchor you can always return to.

The anthology is organised in three kinds:

- **[Cluster leaves](clusters)** — 23 stories, one per leaf of the
  IDENTIFY-condition mission cluster tree. Each covers a cohort of
  related missions.
- **[Pillar leaves](pillars)** — 3 stories tracing the holistic
  argument's three-pillar structure: Argument, Invariants, and
  the Generative Cycle that connects them.
- **[Devmap leaves](devmaps)** — 9 stories, one per repo, walking
  each repo's prototype sorrys with their operational evidence.

Or jump into a **[reading path](paths)** — curated routes through
the anthology for specific orientations. If you want to see
**where the stack is moving right now** — the live roadmap with
acute fronts, sustained work, branch crossings, and cold leaves —
read **[Active Fronts](leaf-active-fronts)**.

Every story is individually browsable; `u` always brings you back
here.

[Cluster leaves →](clusters)

---

### Cluster Leaves | clusters

Twenty-three cluster-derived stories. Each corresponds to one leaf
of the recursive cluster tree (k=8 base, max 15 members per leaf).
Opening scenes give you the cluster's shape in under a minute.

**Pillar III — Missions (the mission metabolism):**

- [leaf-0](leaf-0) — *The Exotype Move* — cross-repo exotype derivations
- [leaf-1](leaf-1) — *A Book on the Shelf* — first FrontierMath probe (singleton)
- [leaf-2](leaf-2) — *Inhabitable Surfaces* — graph + UI + peripheral cohort
- [leaf-3](leaf-3) — *The Math Evidence Machine* — superpod + arxiv ingest pipeline
- [leaf-4](leaf-4) — *Couplings and Probes* — futon5 coupling experiments
- [leaf-5](leaf-5) — *Wires and Gates* — transport + PSR/PUR evidence
- [leaf-6-0](leaf-6-0) — *Reading the Plan* — planning + mermaid viewer
- [leaf-6-1](leaf-6-1) — *A Game at the Edge* — ALFWorld benchmark (singleton)
- [leaf-6-2](leaf-6-2) — *The Last Mile and the Daily Scan* — operational delivery
- [leaf-6-3](leaf-6-3) — *Two Agents at the Rebuild* — Codex/Claude parity + futon1a rebuild
- [leaf-6-4-0](leaf-6-4-0) — *Evidence Over HTTP* — alpha-evidence API + viewer
- [leaf-6-4-1](leaf-6-4-1) — *Reading the Papers* — retrieval-heavy paper work
- [leaf-6-4-2](leaf-6-4-2) — *Proof and Problem* — proof-search + frontiermath cohort
- [leaf-6-4-3](leaf-6-4-3) — *The Peripheral Zoo* — peripheral-architecture
- [leaf-6-4-4](leaf-6-4-4) — *The Stack Thinks About Itself* — portfolio + sorry (prototype)
- [leaf-6-4-5](leaf-6-4-5) — *The War Machine* — strategic dashboard (singleton)
- [leaf-6-5-0](leaf-6-5-0) — *The Coordination Rewrite* — futon3 → futon3b
- [leaf-6-5-1](leaf-6-5-1) — *Artificial Stack Exchange* — synthetic Q&A community
- [leaf-6-5-2](leaf-6-5-2) — *CLI, IRC, REPL* — invocation surfaces
- [leaf-6-5-3](leaf-6-5-3) — *On Being Done* — success-criteria missions
- [leaf-6-5-4](leaf-6-5-4) — *Codex Under Enforcement* — agent-behaviour discipline
- [leaf-6-5-5](leaf-6-5-5) — *Making Agency Work Properly* — Agency reconciliation
- [leaf-7](leaf-7) — *Practice Landing* — external-project landing (singleton)

[← Overview](overview) · [Pillar leaves →](pillars)

---

### Pillar Leaves | pillars

Three pillar-based stories. Not cluster-derived — these trace the
holistic argument's structural commitments directly.

- [leaf-argument](leaf-argument) — *What the Stack Claims to Be*
  — Pillar I (Argument). Thesis, 3 pillars, S1–S6, A1–A4, F1–F5.
  **17 scenes.** Start here if you want the stack's claims.
- [leaf-invariants](leaf-invariants) — *What the Stack Is Sure Of*
  — Pillar II (Invariants). 9 operational families + 10 candidates.
  **11 scenes.** Start here if you want the structural laws.
- [leaf-cycle](leaf-cycle) — *The Cycle That Closes the Loop* —
  the generative cycle connecting work back to belief. 5 steps,
  all operational, step 5 closed. **8 scenes.** Start here if you
  want to see how the pillars cohere into a living system.

**Read all three in order** — argument → invariants → cycle — and
you have the stack's full thesis in roughly half an hour.

[← Cluster leaves](clusters) · [Devmap leaves →](devmaps)

---

### Devmap Leaves | devmaps

Nine per-repo devmap stories. Each walks one repo's prototype
sorrys — overview, one scene per prototype, closing framing.
These are catalogs not narratives; read them when you want the
honest state of one repo.

- [devmap-futon0](devmap-futon0) — human-interface plane (6 prototypes; low close-rate by design)
- [devmap-futon1](devmap-futon1) — durable substrate (9 prototypes; **all operational**)
- [devmap-futon2](devmap-futon2) — AIF engine (14 prototypes; P0-P4 operational, rest active)
- [devmap-futon3](devmap-futon3) — coordination source (17 prototypes; the widest devmap)
- [devmap-futon3a](devmap-futon3a) — pattern guidance + audit (10 prototypes)
- [devmap-futon4](devmap-futon4) — memory atelier (8 prototypes; Arxana editor operational)
- [devmap-futon5](devmap-futon5) — wiring + patterns (5 prototypes; small + dense)
- [devmap-futon6](devmap-futon6) — mathematics dictionary (11 prototypes)
- [devmap-futon7](devmap-futon7) — landscape intelligence (2 prototypes; **highest cadence**)

A reading across all nine is a walking tour of the whole stack's
evidence posture.

[← Pillar leaves](pillars) · [Reading paths →](paths)

---

### Reading Paths | paths

Curated routes through the anthology:

**Path A — Orientation (20 min):** new to the stack.
1. [leaf-argument](leaf-argument) — read Overview + Thesis + three pillar scenes (5 min)
2. [leaf-6-4-4](leaf-6-4-4) — Overview + Mission Control + Portfolio Inference (8 min)
3. [leaf-invariants](leaf-invariants) — Overview + closure-evidence scene (4 min)
4. [leaf-cycle](leaf-cycle) — Overview + Step 5 (3 min)

**Path B — Pillar tour (25 min):** read the three pillars scene by scene.
1. [leaf-argument](leaf-argument) (12 min)
2. [leaf-invariants](leaf-invariants) (8 min)
3. [leaf-cycle](leaf-cycle) (5 min)

**Path C — Mission fleet (30 min):** survey the mission clusters.
1. [leaf-6-4-4](leaf-6-4-4) — *The Stack Thinks About Itself* (prototype)
2. [leaf-2](leaf-2) — *Inhabitable Surfaces*
3. [leaf-0](leaf-0) — *The Exotype Move*
4. [leaf-6-4-2](leaf-6-4-2) — *Proof and Problem*
5. Pick any three more from [clusters](clusters) that catch your eye.

**Path D — Repo inspector (per repo, ~5 min each):** one repo at a
time, devmap + any cluster leaves touching that repo.
- futon0 → [devmap-futon0](devmap-futon0)
- futon1a → [devmap-futon1](devmap-futon1) + [leaf-6-3](leaf-6-3)
- futon3c → [devmap-futon3](devmap-futon3) + [leaf-5](leaf-5) + [leaf-6-4-3](leaf-6-4-3)
- futon7 → [devmap-futon7](devmap-futon7) + [leaf-6-2](leaf-6-2) *Daily Scan* scene

**Path E — Contributor looking for work:** you have time, find something
to close. Starts with the live roadmap.
1. [leaf-active-fronts](leaf-active-fronts) — *Acute* and *Crossings* scenes (5 min)
   to see where current momentum is and where lines rejoin.
2. [leaf-invariants](leaf-invariants) — *Promotion Criterion* scene (3 min)
3. [leaf-cycle](leaf-cycle) — *Where the Cycle Could Still Break* (3 min)
4. [leaf-6-5-3](leaf-6-5-3) — *On Being Done* (3 min)
5. Pick a candidate. Acute + crossings show where momentum is;
   the three scenes above name the biggest open sorrys. A
   contributor looking for leverage picks work at a crossing where
   both leaves benefit.

[← Devmap leaves](devmaps) · [← Overview](overview)

---

*This is the VSATARCS landing page. Press `u` from any story to
return here. `n`/`p` walk scenes within this page; any
`[story-name]` link jumps to that story.*

---

## Devmap — futon0

*Source: `~/code/futon5a/holes/stories/devmap-futon0.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/devmap/futon0` (kind `:devmap`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

The futon0 devmap is the stack's **human-interface** plane: what
does the stack surface to Joe (or to any future contributor), and
how does the stack perceive human state in return? The devmap
lists six prototypes — a foundational interface layer (P0), four
progressively numbered Fx-Fy interfaces, and a completion
prototype.

Current posture: P0 (core infrastructure) is operational;
P1, P2, P5, P6 are aspirational; the completion prototype is
conditional on the others. This devmap has one of the stack's
lowest close-rates — most of its prototypes are `:spec-only` or
`:nascent`. This is not a failure; it is a **reach-vs-grasp**
signal. Human-interface work is where ambition has historically
outrun implementation.

[P0 — Core Infrastructure](p0-core-infrastructure)  
[P1 — F0-F1 Interface (Memory)](p1-f0-f1)  
[P2 — F0-F2 Interface (Agent Perception)](p2-f0-f2)  
[P5 — F0-F5 Interface (Pattern Work)](p5-f0-f5)  
[P6 — F0-F6 Interface (Mathematical Access)](p6-f0-f6)  
[Completion — Full Layer Integration](completion)  
[What Closes First](what-closes-first)

---

### P0 — Core Infrastructure | p0-core-infrastructure

Stack HUD, bidirectional channels, infrastructure substrate
(Alacritty, Emacs, Linode, FUTON1, Codex, Claude), vitality
scanner. This prototype has **four pieces of operational evidence**
and is the devmap's only currently-operational prototype. It is
what makes the rest of the devmap's work possible — every other
prototype assumes P0 is holding.

Inhabited-ness: `:operational`. Open pieces (health monitoring,
affect signal extraction, epistemic rhythm checks) are listed as
next-steps but do not block the prototype's `:operational` status
because the core channels are in place.

[← Overview](overview) · [P1 →](p1-f0-f1)

---

### P1 — F0-F1 Interface (Memory Access) | p1-f0-f1

The interface between the human (F0) and futon1a (F1, durable
memory). The prototype would let a human query, edit, and
annotate the stack's memory in a conversational form rather than
through raw API calls.

Inhabited-ness: `:spec-only`. The interface is named; no
dedicated implementation surface exists beyond what WebArxana
currently offers. F1 is operational on its own (leaf-6-3's
futon1a-rebuild mission); what is missing is the F0↔F1
interface layer that sits above it.

[← P0](p0-core-infrastructure) · [P2 →](p2-f0-f2)

---

### P2 — F0-F2 Interface (Agent Perception) | p2-f0-f2

The interface between the human and futon2's ant-agent simulation
— not for the human to drive ants, but for the human to **perceive
what ants perceive** as a way of grounding AIF framing in
inspectable first-person experience.

Inhabited-ness: `:spec-only`. The ants (futon2 P0-P4) are
operational; what is missing is the viewer that would let a
human step into an ant's viewpoint. The viewer is not hard in
principle; it has not been a priority because the ants are
already analytically inspectable from outside.

[← P1](p1-f0-f1) · [P5 →](p5-f0-f5)

---

### P5 — F0-F5 Interface (Pattern Work) | p5-f0-f5

The interface for a human working directly with the pattern
library: browsing flexiargs, selecting patterns for a task,
recording results. This is PSR/PUR in its fully humanised form —
a surface where the human is a full participant in the pattern
economy rather than merely an operator of the agents that use
patterns.

Inhabited-ness: `:nascent`. Some pattern-browsing exists; the
integration back to PSR/PUR for human-driven work is not in
place. Close relative of M-repl-wins-over-cli (leaf-6-5-2): if
the REPL carries pattern-work affordances, this prototype
advances.

[← P2](p2-f0-f2) · [P6 →](p6-f0-f6)

---

### P6 — F0-F6 Interface (Mathematical Access) | p6-f0-f6

The interface for a human working with futon6's mathematical
dictionary, cross-domain reasoning, and StackExchange import.
The prototype would let a mathematician-user query structured
mathematical knowledge through the stack and get back results
that weave informal and formal reasoning.

Inhabited-ness: `:spec-only`. futon6 itself has operational
pieces (Informal Argument Support at 8-evidence, StackExchange
Import at 14-evidence); this prototype is the human-facing
interface to those pieces and does not yet exist as a distinct
surface.

[← P5](p5-f0-f5) · [Completion →](completion)

---

### Completion — Full Layer Integration | completion

The devmap's final prototype: all of F0↔F1..F6 integrated into a
single coherent interface layer where a human moves between
memory, agents, patterns, and mathematics without seam or
friction. This is the futon0 devmap's **capstone vision**, and
its inhabited-ness reflects that — it closes only when the other
prototypes close.

Inhabited-ness: `:spec-only`, contingent on P1–P6.

[← P6](p6-f0-f6) · [What Closes First →](what-closes-first)

---

### What Closes First | what-closes-first

Of futon0's six prototypes, P0 is the only operational one. The
next most likely closure is **P5 (Pattern Work)** because it
shares machinery with the REPL-wins-over-CLI mission and the
PSR/PUR mesh peripheral (leaf-5). If those two land, P5 advances
by association.

After P5, the natural order is **P1 (Memory Access)** — because
WebArxana already provides part of its surface — then **P2
(Agent Perception)** as a viewer-level addition, then **P6
(Mathematical Access)**. Completion remains contingent.

Futon0's close-rate is low because the interface layer is
harder than the substrate layers it sits on. That is expected;
the devmap's sorrys are honest about it. When the interface
layer does start to close, each closure is unusually
load-bearing — it directly affects Joe's day-to-day.

[← Completion](completion)

---

*Devmap-scale leaf. Per-prototype scenes map to `sorry|devmap|futon0|P0`
through `sorry|devmap|futon0|P6-completion` in futon1a. This
devmap is the stack's interface ambition; its close-rate is
diagnostic of where the stack has invested vs where it has
aspired.*

---

## Devmap — futon1

*Source: `~/code/futon5a/holes/stories/devmap-futon1.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/devmap/futon1` (kind `:devmap`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

The futon1 devmap (for futon1a, since "futon1" is the legacy
label) is the stack's **substrate** plane. Nine prototypes
describe the deterministic storage layer that every other repo
depends on: HTTP API, XTDB hydration, invariant enforcement,
schema, NLP ingest, open-world ingest, client demo, query
workflows, pilot storage. Every prototype has **3–4 pieces of
operational evidence**.

This is the devmap with the highest consistent closure density.
Where futon0 aspired, futon1 delivered. The rebuild mission
(`futon1a-rebuild@futon3`, leaf-6-3) is substantially complete;
most of what this devmap describes is running on port 7071 right
now.

[P0 — HTTP API](p0-http-api)  
[P1 — XTDB Hydration](p1-xtdb-hydration)  
[P2 — Invariant Enforcement](p2-invariant-enforcement)  
[P3 — Graph-Memory Schema & Mirroring](p3-graph-memory)  
[P4 — NLP Interface](p4-nlp-interface)  
[P5 — Open-World Ingest](p5-open-world-ingest)  
[P6 — Demo/Client](p6-demo-client)  
[P7 — Query Workflows](p7-query-workflows)  
[P8 — Pilot Storage](p8-pilot-storage)  
[The Substrate That Works](the-substrate-that-works)

---

### P0 — HTTP API (Canonical Interface) | p0-http-api

The `/api/alpha/` endpoints that carry every other repo's queries
and writes to futon1a. Four pieces of operational evidence,
consistent error contract, layered-error-hierarchy invariant
holding. This prototype is the API surface the rest of the stack
talks to.

Inhabited-ness: `:operational`. Close candidate: already
effectively closed by the implementation, mission doc may trail.

[← Overview](overview) · [P1 →](p1-xtdb-hydration)

---

### P1 — XTDB Hydration | p1-xtdb-hydration

The bitemporal store underlying futon1a. XTDB gives the stack
point-in-time queries, which is load-bearing for the evidence
landscape (every assertion can be read as of a specific time).
Four pieces of operational evidence; hydration runs reliably at
startup.

Inhabited-ness: `:operational`. This is substrate-level; few
stack components interact with XTDB directly — they go through
the API — but XTDB being in place is what makes the API
meaningful.

[← P0](p0-http-api) · [P2 →](p2-invariant-enforcement)

---

### P2 — Invariant Enforcement | p2-invariant-enforcement

The core.logic goals that enforce the 9 operational invariant
families (leaf-invariants) at write time. Three pieces of
operational evidence. This is where futon1a differs from an
ordinary document store: a write that would violate an invariant
is **rejected at the pipeline boundary**, not silently admitted.

Inhabited-ness: `:operational`. The prototype closes several
structural-law sorrys; see leaf-invariants for the list.

[← P1](p1-xtdb-hydration) · [P3 →](p3-graph-memory)

---

### P3 — Graph-Memory Schema & Mirroring | p3-graph-memory

The schema that futon1a actually carries (entities, relations,
hyperedges) plus the mirroring machinery that keeps graph-shaped
views consistent with the document-shaped XTDB layer. Four pieces
of operational evidence.

Inhabited-ness: `:operational`. This prototype is what lets
WebArxana render the whole stack as a graph without maintaining
a separate graph store.

[← P2](p2-invariant-enforcement) · [P4 →](p4-nlp-interface)

---

### P4 — NLP Interface | p4-nlp-interface

The ingest path that turns natural-language input into structured
entity/relation assertions. Four pieces of operational evidence.
This is how ad-hoc prose (mission docs, excursion notes, scene
descriptions) gets lifted into the fact-base.

Inhabited-ness: `:operational`. Much of the stack-geometry
mission's own ingest flowed through this prototype.

[← P3](p3-graph-memory) · [P5 →](p5-open-world-ingest)

---

### P5 — Open-World Ingest | p5-open-world-ingest

The ingest path for unstructured evidence where the schema is
not known in advance. Futon1a treats this as a **separate
pipeline** with relaxed write contracts — the data lands in an
open-world layer that the closed-world schema can later
assimilate or reject. Four pieces of operational evidence.

Inhabited-ness: `:operational`. The open-world layer is load-
bearing for landscape-intelligence work (f7) where source
schemas are heterogeneous.

[← P4](p4-nlp-interface) · [P6 →](p6-demo-client)

---

### P6 — Demo/Client (API Demonstration) | p6-demo-client

A reference client that demonstrates the API's canonical usage —
small enough to read in one sitting, complete enough to show
every endpoint. Three pieces of operational evidence.

Inhabited-ness: `:operational`. The demo serves as living
documentation; what it does and what the API docs claim should
agree.

[← P5](p5-open-world-ingest) · [P7 →](p7-query-workflows)

---

### P7 — Query Workflows | p7-query-workflows

The pre-composed query patterns an operator uses when working
against futon1a: "find all entities of type X," "find closures
by evidence weight," "find sorrys by scale." Three operational +
one active — meaning a named query pattern is in flight that has
not yet landed.

Inhabited-ness: `:operational`. The query workflows are used in
the evidence viewer and the WebArxana backend; they are also
what make the M-stack-geometry anthology's cross-leaf queries
possible.

[← P6](p6-demo-client) · [P8 →](p8-pilot-storage)

---

### P8 — Pilot Storage (fulab, MMCA) | p8-pilot-storage

The prototype for futon1a **as a pilot storage substrate for
external uses** — specifically fulab (Joe's lab-peripheral
cluster) and MMCA (the MetaCA Demonstrator, futon5 P3). Four
operational + one active. This is where futon1a's substrate
claim gets tested against real use: does a real system outside
the stack use futon1a to durable effect?

Inhabited-ness: `:operational`. The pilot is in progress; the
claim lands fully when pilot use is sustained without the stack
having to work around substrate limits.

[← P7](p7-query-workflows) · [The Substrate That Works →](the-substrate-that-works)

---

### The Substrate That Works | the-substrate-that-works

Nine prototypes, nine operational. This is the stack's **cleanest
devmap** in closure terms. The rebuild mission (leaf-6-3) is the
mission-level counterpart; together they make futon1a the
component of the stack that the rest of the stack can safely
assume is holding.

The devmap's remaining work is not structural; it is incremental
— documentation tightening, workflow expansion, pilot validation.
None of that threatens the operational claim. Futon1a is what
the other repos build **on top of** rather than around.

[← P8](p8-pilot-storage)

---

*Devmap-scale leaf. Prototype scenes map to `sorry|devmap|futon1|P0`
through `sorry|devmap|futon1|P8`. This is the stack's substrate
devmap with the highest consistent closure density — a good
reference for what "operational" should look like.*

---

## Devmap — futon2

*Source: `~/code/futon5a/holes/stories/devmap-futon2.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/devmap/futon2` (kind `:devmap`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Fourteen prototypes describe futon2, the stack's **AIF runtime**
in its most inspectable form. Ants on a grid make AIF principles
visible: prediction, preference, action selection under free
energy, precision updates, policy selection. P0 through P4 are
the AIF substrate itself (high operational evidence, 4 each).
P5 onwards is the surrounding machinery — contracts, benchmarks,
bridges, visualisers — all with 2 operational pieces each, a
floor rather than a full build.

The devmap is structurally complete and operationally uneven:
the core AIF loop runs, the surrounding protocol is early. This
is where S2 (AIF Framing Is Generative) gets its strongest
evidence — not because every prototype closes, but because
P0–P4 run end-to-end as a demonstrator.

[P0 — AIF Stack Baseline](p0-aif-stack-baseline)  
[P1 — Observation Hardening](p1-observation-hardening)  
[P2 — Predictive Coding Microcycle](p2-predictive-coding)  
[P3 — Hunger & Precision Dynamics](p3-hunger-precision)  
[P4 — Policy Layer Harness](p4-policy-harness)  
[P5 — World Mechanics Contract](p5-world-mechanics)  
[P6 — Pivot Stream & Analyzer](p6-pivot-stream)  
[P7 — External Stepping API](p7-stepping-api)  
[P8 — Benchmark Suite](p8-benchmark-suite)  
[P9 — futon1 Graph Adapter](p9-futon1-adapter)  
[P10 — futon3 Proofwork Bridge](p10-futon3-bridge)  
[P11 — Curriculum & Behaviour Cards](p11-curriculum)  
[P12 — Viriya Metrics](p12-viriya-metrics)  
[P13 — Debugger & Visualiser](p13-debugger-visualiser)  
[The AIF Core vs The Periphery](aif-core-vs-periphery)

---

### P0 — AIF Stack Baseline | p0-aif-stack-baseline

The minimum AIF stack: generative model, belief state, free-
energy estimate, action selection. Four pieces of operational
evidence. The ants run; predictions update; actions are selected.
This prototype is the floor everything else rests on.
Inhabited-ness: `:operational`.

[← Overview](overview) · [P1 →](p1-observation-hardening)

---

### P1 — Observation Layer Hardening | p1-observation-hardening

Making observation reliable: noisy inputs, dropped frames,
inconsistent sensor timing. Four operational pieces. The ants
don't pretend to perceive perfectly; they perceive approximately
and their belief state handles the approximation. This is where
the AIF framing pays off — a correctly-modelled uncertainty is
cheaper than a brittle assumption of precision.
Inhabited-ness: `:operational`.

[← P0](p0-aif-stack-baseline) · [P2 →](p2-predictive-coding)

---

### P2 — Predictive Coding Microcycle | p2-predictive-coding

The inner loop: predict, observe, update prediction error,
refine belief. This is where AIF stops being vocabulary and
starts running as code. Four operational pieces. The
microcycle ticks at the simulation's native rate.
Inhabited-ness: `:operational`.

[← P1](p1-observation-hardening) · [P3 →](p3-hunger-precision)

---

### P3 — Hunger & Precision Dynamics | p3-hunger-precision

Preferences and their weights: an ant's hunger state modulates
precision, which modulates attention, which modulates action
selection. Four operational pieces. This is the AIF machinery
that makes agents behave differently when their internal state
changes — the precision lever is what distinguishes AIF from
naive reinforcement-learning policies.
Inhabited-ness: `:operational`.

[← P2](p2-predictive-coding) · [P4 →](p4-policy-harness)

---

### P4 — Policy Layer Harness | p4-policy-harness

The policy-evaluation framework: candidate policies, expected
free energy, selection. Four operational pieces. This is where
Portfolio Inference's action-ranking machinery has its ant-scale
analogue; the same EFE ranking runs at two scales (ant,
portfolio) and the code is conceptually shared.
Inhabited-ness: `:operational`.

[← P3](p3-hunger-precision) · [P5 →](p5-world-mechanics)

---

### P5 — World Mechanics Contract | p5-world-mechanics

What the environment guarantees and what the ants can assume
about it. Two operational pieces. This prototype is where the
ant-as-agent framing meets the world-as-substrate framing; a
clear contract between them is what makes the simulation
reproducible.
Inhabited-ness: `:active` (progress beyond baseline, not yet
operational).

[← P4](p4-policy-harness) · [P6 →](p6-pivot-stream)

---

### P6 — Pivot Stream & Analyzer | p6-pivot-stream

The record of **what changed** during a simulation run: when
belief state flipped, when policy preference shifted, when the
ant pivoted. Two operational pieces. This is the instrument that
makes AIF *narratable* — without pivot records the run is a
black box; with them, the run is a story with decision points.
Inhabited-ness: `:active`.

[← P5](p5-world-mechanics) · [P7 →](p7-stepping-api)

---

### P7 — External Stepping API | p7-stepping-api

The ability to drive simulation from outside the simulator —
step one tick, inspect state, step again. Two operational
pieces. This is the prototype that lets other stack components
(the pivot analyzer, the futon1 adapter, the futon3 bridge)
interact with the simulation at a known granularity.
Inhabited-ness: `:active`.

[← P6](p6-pivot-stream) · [P8 →](p8-benchmark-suite)

---

### P8 — Benchmark Suite | p8-benchmark-suite

Reproducible tasks with known optima. Two operational pieces.
The suite is what would make claims about AIF's *quantitative*
value testable — today the stack claims AIF framing is
generative; a benchmark suite that runs AIF-ants and non-AIF-
ants head-to-head would quantify the claim.
Inhabited-ness: `:active`.

[← P7](p7-stepping-api) · [P9 →](p9-futon1-adapter)

---

### P9 — futon1 Graph Adapter | p9-futon1-adapter

Exporting ant runs into futon1a as structured evidence: each
run becomes a series of entities/relations describing the
simulation's state trajectory. Two operational pieces. This is
what makes ant-runs *queryable evidence* rather than transient
log output.
Inhabited-ness: `:active`.

[← P8](p8-benchmark-suite) · [P10 →](p10-futon3-bridge)

---

### P10 — futon3 Proofwork Bridge | p10-futon3-bridge

The bridge that lets ant-scale AIF outputs **feed into** futon3's
gate pipeline as proof-path events. If an ant's behaviour
illustrates a pattern, this bridge is what turns that
illustration into canonisable evidence. Two operational pieces.
Inhabited-ness: `:active`.

[← P9](p9-futon1-adapter) · [P11 →](p11-curriculum)

---

### P11 — Curriculum & Behaviour Cards | p11-curriculum

Structured specifications of ant behaviours and the curricula
for training/selecting them. Two operational pieces. This
prototype is where ant behaviour stops being implementation
detail and becomes *declarable* — behaviour cards make ant
variants describable as compositions.
Inhabited-ness: `:active`.

[← P10](p10-futon3-bridge) · [P12 →](p12-viriya-metrics)

---

### P12 — Viriya Metrics | p12-viriya-metrics

Measurements of ant **effort** (viriya in the contemplative
vocabulary) — how hard did the ant work to maintain its belief
state, resist distraction, stay on-policy. Two operational
pieces. This is the prototype where AIF's energetic framing
gets real numbers attached.
Inhabited-ness: `:active`.

[← P11](p11-curriculum) · [P13 →](p13-debugger-visualiser)

---

### P13 — Debugger & Visualiser | p13-debugger-visualiser

The human-facing view of what a running ant is doing. Two
operational pieces. This prototype is the futon0↔futon2
interface from futon2's side — it is what makes ant perception
accessible to a human observer.
Inhabited-ness: `:active`.

[← P12](p12-viriya-metrics) · [The AIF Core vs The Periphery →](aif-core-vs-periphery)

---

### The AIF Core vs The Periphery | aif-core-vs-periphery

Futon2's devmap has a clear shape: **P0–P4 operational** (4
pieces of evidence each, the AIF core runs), **P5–P13 active**
(2 pieces of evidence each, the surrounding protocol is
incremental). This is a healthy shape for a demonstrator-plus-
ecosystem — the demonstrator works, the ecosystem grows
around it.

For S2's purposes, P0–P4's operational status is sufficient.
The stack's claim is that AIF framing is generative; ants
running AIF end-to-end is the evidence. P5–P13's gradual
completion tightens the story but is not the story's load-
bearing element.

The devmap's main open question is P8 (Benchmark Suite). If
benchmark results showed AIF-ants measurably outperforming
non-AIF baselines at specific tasks, the S2 evidence weight
would increase substantially. Without such results, S2 rests
on architectural argument rather than quantitative comparison.

[← P13](p13-debugger-visualiser)

---

*Devmap-scale leaf. 14 prototype scenes mapping to
`sorry|devmap|futon2|P0` through `sorry|devmap|futon2|P13`.
The AIF core is operational; the surrounding protocol is in
active development. S2's load-bearing evidence lives here.*

---

## Devmap — futon3

*Source: `~/code/futon5a/holes/stories/devmap-futon3.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/devmap/futon3` (kind `:devmap`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Futon3 is the stack's largest and densest devmap: **seventeen
prototypes**, covering the full coordination-and-knowledge layer.
Every prototype carries an explicit `:active` status — the repo
is the source material for the three-futon refactor (futon3a,
futon3b, futon3c). Several prototypes are also `:greenfield`
(P6, P8, P9, P10) — named but not yet substantially started.

This is the devmap that produced the most missions in the
cluster-derived anthology (leaves 6-2, 6-3, 6-4-0, 6-4-3, 6-5-2).
The relationship is: futon3's devmap **states what the
coordination stack is**; the various mission clusters *do* it.

[P0 — MUSN Substrate](p0-musn-substrate)  
[P1 — Pattern Canon](p1-pattern-canon)  
[P2 — Gate Pipeline G5→G0](p2-gate-pipeline)  
[P3 — Proof-Path Store](p3-proof-path-store)  
[P4 — Workday Instrumentation](p4-workday)  
[P5 — Similarity Field](p5-similarity-field)  
[P6 — Intent Embedding](p6-intent-embedding)  
[P7 — Pattern Creation Workbench](p7-pattern-workbench)  
[P8 — Joy Metrics](p8-joy-metrics)  
[P9 — Proof Hooks](p9-proof-hooks)  
[P10 — Flexiformal Training Ground](p10-flexiformal-training)  
[P11 — System Self-Description](p11-self-description)  
[P12 — Social Pipeline & Agency](p12-social-agency)  
[P13 — Evidence Landscape](p13-evidence-landscape)  
[P14 — Peripheral Runtime](p14-peripheral-runtime)  
[P15 — Transport Layer](p15-transport-layer)  
[P16 — Mission System](p16-mission-system)  
[The Source Devmap](the-source-devmap)

---

### P0 — MUSN Coordination Substrate | p0-musn-substrate

Multi-User Session Notation (MUSN) — the event schema carrying
PSR/PUR/PAR records across agent coordination. Seven pieces of
operational evidence. MUSN is the structure that makes every
other prototype's evidence commensurable.
Inhabited-ness: `:operational`.

[← Overview](overview) · [P1 →](p1-pattern-canon)

---

### P1 — Pattern Canon & Standard Library | p1-pattern-canon

The 853+ flexiargs plus the discipline that admits new patterns
into the canon. Six pieces of operational evidence. This is the
library every other mission selects from; its size and discipline
together are what S3 (Pattern Transfer Is Real) rests on.
Inhabited-ness: `:operational`.

[← P0](p0-musn-substrate) · [P2 →](p2-gate-pipeline)

---

### P2 — Coordination Gate Pipeline (G5→G0) | p2-gate-pipeline

The ordered gate pipeline G5 → G0 that every coordinated
task-significant piece of work is supposed to traverse. Ten
pieces of operational evidence. This prototype is the
coordination layer's most foundational piece — the phase-ordering
invariant's operational instantiation.
Inhabited-ness: `:operational`.

[← P1](p1-pattern-canon) · [P3 →](p3-proof-path-store)

---

### P3 — Proof-Path Store (Durable Journal) | p3-proof-path-store

The persistent journal of gate traversals. Five pieces of
operational evidence. This store is where step-1 of the
generative cycle (leaf-cycle) actually lands data; without it,
the cycle would have no evidence to iterate over.
Inhabited-ness: `:operational`.

[← P2](p2-gate-pipeline) · [P4 →](p4-workday)

---

### P4 — Workday Instrumentation | p4-workday

Instrumentation that records the **cadence** of work — when
cycles start, stall, resume, complete. Five pieces of operational
evidence. This is what makes the operator's workday inspectable
as a temporal signal rather than as raw activity.
Inhabited-ness: `:operational`.

[← P3](p3-proof-path-store) · [P5 →](p5-similarity-field)

---

### P5 — Similarity Field | p5-similarity-field

Embedding-based similarity between patterns, missions, proofs.
Eight pieces of operational evidence. This is the field that the
stack-geometry mission itself has been exploring — distances
between missions in embedding space, cluster topology, the
landscape of what's adjacent to what.
Inhabited-ness: `:operational`.

[← P4](p4-workday) · [P6 →](p6-intent-embedding)

---

### P6 — Intent Embedding & Proof Typing | p6-intent-embedding

Richer structure on top of the similarity field — embedding not
just content but *intent*, and typing proofs so they compose.
Two operational pieces; status `:greenfield`. This prototype is
a named aspiration more than an active build.
Inhabited-ness: `:nascent`.

[← P5](p5-similarity-field) · [P7 →](p7-pattern-workbench)

---

### P7 — Pattern Creation Workbench | p7-pattern-workbench

The surface where new patterns are proposed, evaluated, and
either admitted or rejected. Four pieces of operational evidence.
Close cousin of the L1 canonicalizer (leaf-cycle step 2).
Inhabited-ness: `:operational`.

[← P6](p6-intent-embedding) · [P8 →](p8-joy-metrics)

---

### P8 — Joy Metrics | p8-joy-metrics

Measurements of **operator satisfaction** during work — does the
stack feel like it helps today, or feel like it fights. Three
operational pieces; status `:greenfield`. This prototype is
unusually honest about its ambition: most stacks don't even name
joy as a measurable target.
Inhabited-ness: `:nascent`.

[← P7](p7-pattern-workbench) · [P9 →](p9-proof-hooks)

---

### P9 — Proof Hooks (futon1/futon2 Export) | p9-proof-hooks

Export paths from futon3's proof store to futon1a and futon2, so
coordination evidence feeds the substrate and the simulator.
Three operational pieces; `:greenfield`. The hooks exist in
sketch; full export is pending.
Inhabited-ness: `:nascent`.

[← P8](p8-joy-metrics) · [P10 →](p10-flexiformal-training)

---

### P10 — Flexiformal Training Ground | p10-flexiformal-training

A practice surface where flexiargs (soft patterns) can be tried,
recorded, and either hardened into library canon or retired.
Three operational pieces; `:greenfield`. This is where the
library's candidate queue would live.
Inhabited-ness: `:nascent`.

[← P9](p9-proof-hooks) · [P11 →](p11-self-description)

---

### P11 — System Self-Description | p11-self-description

The stack's account of what the stack is, in a form the stack
can read. Four pieces of operational evidence; status
`:partial`. The holistic argument semilattice (v3), the
three-pillars doc, this anthology itself — all live inside this
prototype's scope.
Inhabited-ness: `:active`. Arguably advancing rapidly via the
current M-stack-geometry mission.

[← P10](p10-flexiformal-training) · [P12 →](p12-social-agency)

---

### P12 — Social Pipeline & Agency | p12-social-agency

Multi-agent social coordination — who is talking to whom, on
what channel, for what mission. Ten pieces of operational
evidence. This prototype is where Agency (futon3c) gets its
mandate.
Inhabited-ness: `:operational`.

[← P11](p11-self-description) · [P13 →](p13-evidence-landscape)

---

### P13 — Evidence Landscape | p13-evidence-landscape

The coherent landscape of all stack evidence — not just
proof-paths but PSRs, PURs, PARs, mission artefacts, scan
observations, reviewer notes. Eight pieces of operational
evidence. This prototype is what makes evidence a first-class
queryable surface.
Inhabited-ness: `:operational`.

[← P12](p12-social-agency) · [P14 →](p14-peripheral-runtime)

---

### P14 — Peripheral Runtime | p14-peripheral-runtime

The runtime environment where peripherals actually execute.
Eleven pieces of operational evidence — the devmap's highest
single-prototype evidence count. Covered in depth by
leaf-6-4-3 (The Peripheral Zoo).
Inhabited-ness: `:operational`.

[← P13](p13-evidence-landscape) · [P15 →](p15-transport-layer)

---

### P15 — Transport Layer | p15-transport-layer

The routing substrate (Drawbridge, IRC, WS) that carries
coordination traffic without creating agents. Seven pieces of
operational evidence. Covered in depth by leaf-5 (Wires and
Gates) and leaf-6-5-2 (CLI, IRC, REPL).
Inhabited-ness: `:operational`.

[← P14](p14-peripheral-runtime) · [P16 →](p16-mission-system)

---

### P16 — Mission System | p16-mission-system

The machinery around mission state: Mission Control peripheral,
phase transitions, obligation tracking, completion events.
**Twelve pieces of operational evidence** — the devmap's
highest. Covered in depth by leaf-6-4-4 (The Stack Thinks About
Itself).
Inhabited-ness: `:operational`. Closure on this prototype is
extensively backed.

[← P15](p15-transport-layer) · [The Source Devmap →](the-source-devmap)

---

### The Source Devmap | the-source-devmap

Futon3 is the source repo of the three-futon refactor. Most of
its seventeen prototypes have been **ported or forked** into
futon3a (pattern/audit), futon3b (task/glacial gates), and
futon3c (real-time coordination). The devmap is consequently a
historical record plus a live record — some prototypes are
actively maintained here, others have been handed off.

The three-futon refactor is expected to eventually make this
devmap close substantially, as prototypes complete migration.
Until then, reading the devmap tells you both **what the
coordination layer claims to be** and **what futon3 itself
still owns**.

Of the 17 prototypes, 14 are operational. The three that are
nascent (P6, P8, P9, P10 — all `:greenfield` or sparse evidence)
are the devmap's honest frontier.

[← P16](p16-mission-system)

---

*Devmap-scale leaf. 17 prototype scenes map to
`sorry|devmap|futon3|P0` through `sorry|devmap|futon3|P16`.
The coordination stack's widest and densest devmap.*

---

## Devmap — futon3a

*Source: `~/code/futon5a/holes/stories/devmap-futon3a.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/devmap/futon3a` (kind `:devmap`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Futon3a is the stack's **pattern-guidance and audit** repo — the
thin layer that reads the pattern canon (which lives in futon3b
now) and makes its guidance available to other repos, while
logging audit trails of what was selected, what was used, and
what happened. Ten prototypes, mostly `:active` with 2-3
operational evidence pieces. P10 (Proof-Theoretic Digest) is the
standout at 9 operational pieces.

The devmap's posture is "working infrastructure, modest coverage"
— no prototype is dormant, none is fully consolidated.

[P0 — Portal Query Layer](p0-portal-query)  
[P1 — Sidecar Audit Trail](p1-sidecar-audit)  
[P2 — Compass GFE Alignment](p2-compass-gfe)  
[P4 — Mission Queue & Supervisor](p4-mission-queue)  
[P5 — Morning Review Protocol](p5-morning-review)  
[P6 — Library Evidence Feedback](p6-library-feedback)  
[P7 — Xenotype CT Attachment](p7-xenotype-ct)  
[P8 — Telemetry Bridge](p8-telemetry-bridge)  
[P10 — Proof-Theoretic Digest](p10-digest)  
[P11 — Stabilization Hub](p11-stabilization-hub)

*(P3 and P9 are absent from the devmap — the numbering carries
the gap honestly rather than renumbering.)*

---

### P0 — Portal Query Layer | p0-portal-query

The Drawbridge-pass surface that lets agents query the pattern
library through a scoped access point. Three operational pieces.
This is how other repos *read* the library without pulling it in
as a dependency.
Inhabited-ness: `:operational`.

[← Overview](overview) · [P1 →](p1-sidecar-audit)

---

### P1 — Sidecar Audit Trail | p1-sidecar-audit

Every query and every pattern-use emits an audit record through
the sidecar. Three operational pieces. This is what makes
pattern-library usage **inspectable** — not just "the library
was queried," but which pattern, by which agent, under which
session, with what outcome.
Inhabited-ness: `:operational`.

[← P0](p0-portal-query) · [P2 →](p2-compass-gfe)

---

### P2 — Compass GFE Alignment | p2-compass-gfe

The Compass that aligns agent preferences with Generalized Free
Energy minima — in practice, the surface where an agent asks
"among these candidate actions, which minimises expected free
energy given current preferences?" Four operational + one active.
Close cousin of Portfolio Inference (leaf-6-4-4).
Inhabited-ness: `:operational` (with active evolution).

[← P1](p1-sidecar-audit) · [P4 →](p4-mission-queue)

---

### P4 — Mission Queue & Supervisor | p4-mission-queue

The queue that tracks which missions are pending, which are
active, which have blockers. Two operational + one active. This
is where mission state surfaces to the operator as a priority-
sortable list.
Inhabited-ness: `:active`.

[← P2](p2-compass-gfe) · [P5 →](p5-morning-review)

---

### P5 — Morning Review Protocol | p5-morning-review

A named ritual: at the start of a session, review the queue,
the blockers, the last day's PSRs/PURs, the stack's current
advice. Two operational + one active. This prototype is the
**ceremony** that makes consistent entry into the stack
possible.
Inhabited-ness: `:active`.

[← P4](p4-mission-queue) · [P6 →](p6-library-feedback)

---

### P6 — Library Evidence Feedback | p6-library-feedback

Pattern use (via PUR) feeds back into the library's notion of
which patterns are working. Three operational + one active. This
is the feedback arrow that makes the library a living organism
rather than a static collection.
Inhabited-ness: `:active`.

[← P5](p5-morning-review) · [P7 →](p7-xenotype-ct)

---

### P7 — Xenotype CT Attachment | p7-xenotype-ct

Attaching **xenotype calculus** (formal flexiarg composition
rules) to Compass Tables. Two operational + one active. This is
niche but load-bearing for the stack's claim that flexiargs
compose rather than just accumulate.
Inhabited-ness: `:active`.

[← P6](p6-library-feedback) · [P8 →](p8-telemetry-bridge)

---

### P8 — Futon3a → Futon0 Telemetry Bridge (Raw) | p8-telemetry-bridge

Raw telemetry flows from futon3a's audit sidecar into futon0's
reporting surface. Two operational + one active. This is how
futon0 knows what futon3a sees — without the bridge, the two
repos would each maintain their own view.
Inhabited-ness: `:active`.

[← P7](p7-xenotype-ct) · [P10 →](p10-digest)

---

### P10 — Proof-Theoretic Digest/Compression | p10-digest

**Nine pieces of operational evidence** — the devmap's highest.
The digest compresses long proof-paths into short canonical
summaries that preserve the inferential structure. This is what
makes proof-paths re-readable at scale; without compression,
the proof-path store grows without readable summarisation.
Inhabited-ness: `:operational`.

[← P8](p8-telemetry-bridge) · [P11 →](p11-stabilization-hub)

---

### P11 — Stabilization & Hub Demonstration | p11-stabilization-hub

The demonstrator that shows futon3a serving as a **hub** for
multi-repo pattern-and-audit traffic while staying stable under
realistic load. Five operational + one active. Mission-level
analogue in leaf-6-4-3.
Inhabited-ness: `:operational`.

[← P10](p10-digest)

---

*Devmap-scale leaf. 10 prototype scenes mapping to
`sorry|devmap|futon3a|P0` through `sorry|devmap|futon3a|P11`
(with P3 and P9 absent). Pattern-guidance + audit is a thin but
critical layer; this devmap is its full accounting.*

---

## Devmap — futon4

*Source: `~/code/futon5a/holes/stories/devmap-futon4.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/devmap/futon4` (kind `:devmap`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Futon4 is the stack's **memory atelier** — a hypertext-rich
workspace where scholia, design-pattern templates, and
long-form notes live alongside the more structured fact-base of
futon1a. Eight prototypes. The first three (P0–P2) are densely
evidenced and operational; P3–P7 are sparsely evidenced and
mostly `:active`.

WebArxana — the web UI that reads futon1a and renders its graph
— lives downstream of this devmap's Arxana prototype. The
evidence-viewer refinements mission (leaf-6-4-0) lives here.

[P0 — Self-Documenting Foundations](p0-self-documenting)  
[P1 — Arxana Hypertext Editor](p1-arxana-editor)  
[P2 — Scholium Mode](p2-scholium-mode)  
[P3 — Design Pattern Templates](p3-pattern-templates)  
[P4 — Graph Storage Layer](p4-graph-storage)  
[P5 — XTDB Version History](p5-xtdb-history)  
[P6 — Multi-User Support](p6-multi-user)  
[P7 — Literary Interface](p7-literary-interface)

---

### P0 — Self-Documenting Foundations | p0-self-documenting

The ground-level convention that futon4 documents are legible
without external reference — definitions inlined, cross-links
explicit, no orphan jargon. Six pieces of operational evidence.
This is what makes the workspace inhabitable for a contributor
who did not write it.
Inhabited-ness: `:operational`.

[← Overview](overview) · [P1 →](p1-arxana-editor)

---

### P1 — Arxana Hypertext Editor | p1-arxana-editor

**Nine pieces of operational evidence** — the devmap's highest.
Arxana is the hypertext editor and graph viewer that renders
futon4 documents and futon1a entities in the same surface. This
prototype is what makes the stack's self-representation
navigable by a human.
Inhabited-ness: `:operational`.

[← P0](p0-self-documenting) · [P2 →](p2-scholium-mode)

---

### P2 — Scholium Mode | p2-scholium-mode

A specific editing mode that treats short marginal commentaries
(scholia) as first-class editable annotations, attached to
specific anchor points in a document. Six operational + one
active. This is what makes review and commentary a natural
surface rather than an afterthought.
Inhabited-ness: `:operational`.

[← P1](p1-arxana-editor) · [P3 →](p3-pattern-templates)

---

### P3 — Design Pattern Templates | p3-pattern-templates

Templates for writing design patterns — the canonical structure
(context, problem, forces, solution, consequences) plus futon-
specific fields (evidence, invariants, missions). Two operational
+ one active. This prototype is what standardises how new
patterns enter the library.
Inhabited-ness: `:active`.

[← P2](p2-scholium-mode) · [P4 →](p4-graph-storage)

---

### P4 — Graph Storage Layer | p4-graph-storage

Futon4's own graph layer — distinct from futon1a's durable
substrate because futon4 needs document-scoped graphs for
hypertext links that don't belong in the global fact-base. Two
operational + one active.
Inhabited-ness: `:active`.

[← P3](p3-pattern-templates) · [P5 →](p5-xtdb-history)

---

### P5 — XTDB Version History | p5-xtdb-history

XTDB's bitemporality exposed through futon4's surface: a reader
can view a document as of any point in time. Two operational +
one active. This prototype is what makes document editing
reversible at a granularity finer than git.
Inhabited-ness: `:active`.

[← P4](p4-graph-storage) · [P6 →](p6-multi-user)

---

### P6 — Multi-User Support | p6-multi-user

Multiple operators editing futon4 concurrently without lost
writes. Two operational + one active. Close cousin of a
`:closes` edge pointing here from leaf-2's Labs Integration
mission (sim 0.45, medium confidence).
Inhabited-ness: `:active`.

[← P5](p5-xtdb-history) · [P7 →](p7-literary-interface)

---

### P7 — Literary Interface | p7-literary-interface

A reading-mode surface that treats futon4 content as long-form
prose rather than as editable material — tightened typography,
footnote rendering, continuous text flow. Two operational + one
active. This prototype is where the anthology (including this
leaf) would actually be *read* in polished form.
Inhabited-ness: `:active`.

[← P6](p6-multi-user)

---

*Devmap-scale leaf. 8 prototype scenes mapping to
`sorry|devmap|futon4|P0` through `sorry|devmap|futon4|P7`. The
memory atelier — arxana editor and scholia are the operational
core; the surrounding infrastructure is in incremental build.*

---

## Devmap — futon5

*Source: `~/code/futon5a/holes/stories/devmap-futon5.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/devmap/futon5` (kind `:devmap`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Futon5 is the stack's **wiring-diagram and pattern-library**
repo. Five prototypes. Nonstarter specifications (P0, P1) are
the foundation — patterns for work-that-shouldn't-start-without-
support. Wiring Diagrams (P2) is the devmap's most densely
evidenced prototype. MetaCA and Transfer Milestone (P3, P4) are
forward-looking.

This is the devmap that hosts the wiring-side mirrors of the
three-futon refactor: `futon3-coordination@futon5`,
`futon3-agent-loop@futon5`, `futon2-aif-ants@futon5` all live
here as wiring mirrors of execution missions elsewhere.

[P0 — Nonstarter Specification](p0-nonstarter-spec)  
[P1 — Nonstarter Futon-Stack Deployment](p1-nonstarter-deployment)  
[P2 — Wiring Diagrams as Patterns](p2-wiring-diagrams)  
[P3 — MetaCA Demonstrator](p3-metaca)  
[P4 — Transfer Milestone](p4-transfer-milestone)

---

### P0 — Nonstarter Specification | p0-nonstarter-spec

**Nonstarter** is Joe's term for a pattern whose application
would *block* rather than *advance* work — a pattern that is
recognised as the wrong move for its context. The specification
prototype defines how nonstarters are named, catalogued, and
distinguished from regular patterns. Four operational + one
active.
Inhabited-ness: `:operational`.

[← Overview](overview) · [P1 →](p1-nonstarter-deployment)

---

### P1 — Nonstarter Futon-Stack Deployment | p1-nonstarter-deployment

Deploying the nonstarter machinery into the stack so nonstarter
recognition is an active part of pattern selection — not just a
vocabulary item. Two operational + one active. A running PSR
could flag "this is a nonstarter for X reason" and short-circuit
the selection.
Inhabited-ness: `:active`.

[← P0](p0-nonstarter-spec) · [P2 →](p2-wiring-diagrams)

---

### P2 — Wiring Diagrams as Patterns | p2-wiring-diagrams

Wiring diagrams — EDN-encoded component-and-relation maps of
stack subsystems — treated as **first-class patterns** that can
be selected, composed, and inhabited. **Six pieces of operational
evidence**. This is where the stack's architectural knowledge
lives as structured data rather than prose.
Inhabited-ness: `:operational`.

[← P1](p1-nonstarter-deployment) · [P3 →](p3-metaca)

---

### P3 — MetaCA Demonstrator | p3-metaca

A concrete demonstrator of Meta-Category-Adjunction — the formal
framework that underlies wiring-diagram composition. Five
operational + one active. Where P2 makes wiring diagrams
first-class *data*, P3 makes the *composition rules* first-class.
Inhabited-ness: `:operational`.

[← P2](p2-wiring-diagrams) · [P4 →](p4-transfer-milestone)

---

### P4 — Transfer Milestone | p4-transfer-milestone

A named milestone: the point at which a pattern has been
demonstrated to **transfer** out of its home repo and succeed in
a different domain. Four pieces of operational evidence. This is
how S3 (Pattern Transfer Is Real) accrues concrete evidence — each
milestone is one data point.
Inhabited-ness: `:operational`. The milestone is defined; the
accumulation is ongoing.

[← P3](p3-metaca)

---

*Devmap-scale leaf. 5 prototype scenes mapping to
`sorry|devmap|futon5|P0` through `sorry|devmap|futon5|P4`. The
pattern-and-wiring devmap is small but dense — every prototype
is operationally evidenced. A future split might introduce a
separate futon5a devmap once the pitches/wiring sub-corpus
grows further.*

---

## Devmap — futon6

*Source: `~/code/futon5a/holes/stories/devmap-futon6.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/devmap/futon6` (kind `:devmap`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Futon6 is the stack's **mathematics-dictionary** repo — the
substrate for informal mathematical reasoning, cross-domain
pattern lookup, and formal-plus-informal integration. Eleven
prototypes. The devmap is unusual: P0 (Informal Argument) and
P7 (StackExchange Import) have deep operational evidence (8 and
14 pieces); most other prototypes have 1–2 pieces and are
`:active`. P10 (Wiring Diagram LLM) is a significant operational
surface at 10 pieces.

This devmap's close-rate is moderate but its operational work is
**concentrated** — where it invests, it invests heavily.

[P0 — Informal Argument Support](p0-informal-argument)  
[P1 — Seed Domain](p1-seed-domain)  
[P2 — Cross-Domain Reasoning Patterns](p2-cross-domain)  
[P3 — Domain-Specific Patterns](p3-domain-specific)  
[P4 — Reasoning Map](p4-reasoning-map)  
[P5 — Interactive Tutorials](p5-tutorials)  
[P6 — Landscape Mode](p6-landscape-mode)  
[P7 — StackExchange Import](p7-stackexchange-import)  
[P8 — Agent Protocol](p8-agent-protocol)  
[P9 — Reusable Mathematical Models](p9-reusable-models)  
[P10 — Wiring Diagram LLM](p10-wiring-diagram-llm)

---

### P0 — Informal Argument Support | p0-informal-argument

Eight pieces of operational evidence. The prototype supports
informal mathematical argument — prose proofs, heuristic
reasoning, approximate claims — as first-class objects that can
be cited, refined, and eventually hardened. This is where the
stack departs from formal-only proof systems.
Inhabited-ness: `:operational`.

[← Overview](overview) · [P1 →](p1-seed-domain)

---

### P1 — Seed Domain | p1-seed-domain

A single mathematical domain chosen to bootstrap the dictionary
— a domain with enough content to be useful and enough
homogeneity to be tractable. One operational + one active.
Inhabited-ness: `:active`.

[← P0](p0-informal-argument) · [P2 →](p2-cross-domain)

---

### P2 — Cross-Domain Reasoning Patterns | p2-cross-domain

Patterns that apply across multiple mathematical domains — the
analogies, morphisms, and structural reuses that make a
dictionary more than a glossary. One operational + one active.
Inhabited-ness: `:active`.

[← P1](p1-seed-domain) · [P3 →](p3-domain-specific)

---

### P3 — Domain-Specific Patterns | p3-domain-specific

Patterns specific to individual domains — the moves that a
specialist would recognise as characteristic of their subfield.
One operational + one active.
Inhabited-ness: `:active`.

[← P2](p2-cross-domain) · [P4 →](p4-reasoning-map)

---

### P4 — Reasoning Map | p4-reasoning-map

A navigable map of mathematical reasoning — which patterns
connect to which, what the standard progressions are. Two
operational + one active. Close relative of futon3's similarity
field (leaf devmap-futon3, P5) applied to mathematical rather
than pattern-library content.
Inhabited-ness: `:active`.

[← P3](p3-domain-specific) · [P5 →](p5-tutorials)

---

### P5 — Interactive Tutorials | p5-tutorials

Tutorials inside the dictionary — structured walks through
specific reasoning patterns, with checkable intermediate states.
Two operational + one active. This is the prototype most directly
aimed at external-reader value.
Inhabited-ness: `:active`.

[← P4](p4-reasoning-map) · [P6 →](p6-landscape-mode)

---

### P6 — Landscape Mode | p6-landscape-mode

A reading mode that presents mathematical content **as a
landscape** — a browseable, orienting surface rather than a
linear document. Two operational + one active. Close cousin of
futon7's landscape-intelligence work, applied to mathematics.
Inhabited-ness: `:active`.

[← P5](p5-tutorials) · [P7 →](p7-stackexchange-import)

---

### P7 — StackExchange Import | p7-stackexchange-import

**Fourteen pieces of operational evidence** — the devmap's
highest. StackExchange (Math.SE, MathOverflow) is imported as a
corpus of real informal mathematical argument: questions,
answers, edits, votes. This is where futon6 has the most
concrete content.
Inhabited-ness: `:operational`.

[← P6](p6-landscape-mode) · [P8 →](p8-agent-protocol)

---

### P8 — Agent Protocol | p8-agent-protocol

The protocol an agent follows when querying, updating, or
adding to the dictionary. Two operational + one active.
Inhabited-ness: `:active`.

[← P7](p7-stackexchange-import) · [P9 →](p9-reusable-models)

---

### P9 — Reusable Mathematical Models | p9-reusable-models

Formal models (objects, morphisms, structures) captured in
reusable form. Two operational + one active. This is where the
dictionary's formal side lives; its development is slower than
the informal side's because formalisation is expensive.
Inhabited-ness: `:active`.

[← P8](p8-agent-protocol) · [P10 →](p10-wiring-diagram-llm)

---

### P10 — Wiring Diagram LLM | p10-wiring-diagram-llm

**Ten pieces of operational evidence**. A language-model
surface specifically attuned to wiring-diagram interpretation —
given a diagram, the prototype answers questions about what it
implies, what it omits, what it contradicts. This is the
prototype that would make wiring diagrams readable by an LLM
rather than only by a trained human.
Inhabited-ness: `:operational`.

[← P9](p9-reusable-models)

---

*Devmap-scale leaf. 11 prototype scenes mapping to
`sorry|devmap|futon6|P0` through `sorry|devmap|futon6|P10`. The
mathematics-dictionary devmap — concentrated operational work
on P0, P7, P10; incremental build-out elsewhere.*

---

## Devmap — futon7

*Source: `~/code/futon5a/holes/stories/devmap-futon7.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/devmap/futon7` (kind `:devmap`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Futon7 is the stack's **landscape-intelligence and knowledge-
economy** repo — where adjacent-project surveys, audience probes,
daily scans, and lead reports live. The devmap names **two
prototypes**: Gravpad and Scenario Simulations. Both are
`:active` with 2 operational pieces each. This is the youngest
devmap in the stack, but its operational cadence (daily scans,
lead reports) is among the most regular.

S4 (**Commercial Demand Exists Adjacent**) gets its evidence
from this devmap. Every lead report is a small piece of S4
evidence; every scan observation is a small piece of networked-
intelligence state.

[P1 — Gravpad](p1-gravpad)  
[P2 — Scenario Simulations](p2-scenario-simulations)  
[What Makes This Devmap Distinctive](what-makes-it-distinctive)

---

### P1 — Gravpad | p1-gravpad

Gravpad is the networked-intelligence **working surface** — the
place where audience targets, scan observations, and draft leads
come together before any of them becomes a structured artefact.
Two operational pieces; the prototype has an inhabited working
form and an open direction.
Inhabited-ness: `:active`.

This prototype is adjacent to (but distinct from) the
`daily-scan@futon7` and `f7-lead-report@futon7` missions
(leaf-6-2). The missions operate within Gravpad; Gravpad is the
substrate the missions inhabit.

[← Overview](overview) · [P2 →](p2-scenario-simulations)

---

### P2 — Scenario Simulations | p2-scenario-simulations

The prototype for running **scenarios** — speculative walks
through what would happen if a specific landing opportunity
proceeded, or a specific probe succeeded, or a specific external
signal landed. Two operational pieces. This is where the
landscape-intelligence work engages counterfactual reasoning
rather than only describing what is.
Inhabited-ness: `:active`.

Scenario simulation is the tool most directly relevant to
landing-practice (leaf-7) — a landing conversation that can
offer "here's what we expect if you commit to this for six
months" is a landing conversation better-equipped than one
that can only describe past work.

[← P1](p1-gravpad) · [What Makes This Devmap Distinctive →](what-makes-it-distinctive)

---

### What Makes This Devmap Distinctive | what-makes-it-distinctive

Futon7 is the **only devmap in the stack with revenue-adjacent
output**. Lead reports are the closest thing the stack has to a
deliverable that an external audience would actually pay for.
That alone makes futon7 worth attending to as a bellwether for
S4 (Commercial Demand Exists Adjacent) and A3's counter
(positioning as enterprise developer tooling).

The devmap is **small by design** — two prototypes is not a
gap, it is a focused scope. Gravpad and Scenario Simulations are
enough infrastructure to support the operational cadence of
daily scans and lead reports without spreading effort across
prototypes that would need more substrate than futon7 currently
carries.

If landing-practice (leaf-7) succeeds, this devmap is likely
the one that grows most in subsequent quarters — not in number
of prototypes necessarily, but in operational evidence accrued
per prototype.

[← P2](p2-scenario-simulations)

---

*Devmap-scale leaf. Two prototype scenes plus one framing
scene. The smallest devmap in the stack by prototype count; one
of the highest by operational cadence (daily scans, lead
reports). The stack's revenue-adjacent frontier.*

---

## Globe 1 — Market Interface

*Source: `~/code/futon5a/holes/stories/globe1-market-interface.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/globe/1-market-interface` (kind `:globe`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

`🌐1` is the current trunk-line gap. Alignment calls it "THE sorry":
the stack has real internal capability, but no reliable incoming line
for strangers. The closure path named there is still the same one:
Bristol, the prospectus, and the explicit sentence.

This handle already has a full strategy devmap at
`futon3/holes/strategy/globe1-market-interface.devmap`. The devmap's
core claim is that the gap is narrower than "do more outreach."
Closure means a coherent packet exists, the packet is actually
delivered, the ask is explicitly spoken, and the pattern becomes
reusable rather than Bristol-shaped only once.

Current backing:

- `futon5a/data/alignment.edn` defines the sorry as a `:trunk-line` at
  `:V-money`, marks it `:critical`, and names "Bristol May 12th +
  prospectus + the sentence" as the closure path.
- `futon5a/holes/missions/M-a-sorry-enterprise.md` treats this as the
  first live closure test and already names observable signals such as
  Bristol/UKRN evidence turns, `vsat.wiki` commits, and prospectus
  existence.
- `globe1-market-interface.devmap` decomposes closure into offer
  packet, explicit ask, repeatable incoming line, and conversion
  evidence.
- `E-strategy-candidate-bridge-v0.md` records a first worked bridge
  from `🌐1`'s closure prototypes to candidate-invariant families.

What honest progress looks like:

- a real packet exists
- the packet is delivered in a real conversation
- the explicit ask is actually made
- the interaction leaves reusable downstream evidence

So when the War Machine surfaces `🌐1`, the honest operator reading is:
*package the offer, perform the ask, and leave reusable incoming-line
evidence behind.*

[← Strategic SORRY Topology](strategic-sorry-topology)

---

## Globe 2 — Mode Violation

*Source: `~/code/futon5a/holes/stories/globe2-mode-violation.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/globe/2-mode-violation` (kind `:globe`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

`🌐2` is the narrow constraint-level problem:
`cargo-implies-depositing`. Cargo exists, reserves pressure exists, and
yet the system can remain in foraging or stack-heavy behaviour without
a reliable ratchet into depositing.

This handle already has a full strategy devmap at
`futon3/holes/strategy/globe2-mode-violation.devmap`. The devmap
treats closure as a four-part loop: cargo is explicitly counted, the
violation fires visibly, a finite depositing response surface exists,
and the violation clears only because depositing actions were actually
taken.

Current backing:

- `futon5a/data/alignment.edn` defines this sorry as a `:constraint` in
  terminal vocabulary, marks it `:critical`, and explicitly says the
  current status is `:violated`.
- The note there is already vivid and exact: "The ant is carrying food
  and won't go home."
- `globe2-mode-violation.devmap` turns that image into a closure
  program with observable steps rather than a metaphor alone.
- `E-globe2-candidate-bridge-v0.md` records a first worked bridge from
  `🌐2`'s closure prototypes to candidate-invariant families.

This is deliberately narrower than `🌐8`. `🌐2` asks whether the
constraint fires and clears. `🌐8` asks whether the whole governing
policy has shifted.

What honest progress looks like:

- cargo is explicitly visible
- the violation fires when cargo is present
- a finite depositing response exists
- the violation later clears for legible reasons

If the system still has to narrate why depositing did not happen, the
sorry remains open.

[← Strategic SORRY Topology](strategic-sorry-topology)

---

## Globe 3 — Peer Eval Artifact

*Source: `~/code/futon5a/holes/stories/globe3-peer-eval-artifact.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/globe/3-peer-eval-artifact` (kind `:globe`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

`🌐3` is a missing-artifact gap. The capability exists: the stack can
do cross-institutional modelling, rigour, and reflective evaluation.
The missing piece is a stranger-facing or peer-facing artifact that
packages that capability coherently.

At present this is intentionally classified as a lighter note rather
than a full strategy devmap. The reason is not that the gap is unreal;
it is that the closure object is still bounded and packaging-shaped
rather than yet a full cross-repo substrate.

Current backing:

- `futon5a/data/alignment.edn` places the sorry at the placemat
  inter-edge `:X-peer-evaluation` and says there is no personal-facing
  JSDQ analog yet.
- The capability named there is specific: `S3-collaboration` plus
  `S6-rigour`, not vague "consulting competence."
- `futon3/holes/strategy/README.md` classifies this as a real gap but a
  bounded packaging problem rather than a mature closure-devmap family.
- `E-globe3-candidate-bridge-v0.md` records a first bridge from `🌐3`'s
  packaging obligations to candidate-invariant families.

What honest progress looks like:

- a finite peer-facing artifact exists
- the artifact has a named audience and delivery context
- the artifact can be shown without re-explaining the whole stack

Promotion to a full devmap would require the artifact to start
projecting into repeatable evidence, buyer classes, or downstream
counters rather than remaining a single packaging exercise.

[← Strategic SORRY Topology](strategic-sorry-topology)

---

## Globe 4 — Paragogy Revenue

*Source: `~/code/futon5a/holes/stories/globe4-paragogy-revenue.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/globe/4-paragogy-revenue` (kind `:globe`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

`🌐4` is the "Brain Trust problem." The alignment note is already
sharp: paragogy works for others; the sorry is making it work for Joe.

This is classified as a lighter note for now. The strategic pressure is
real, but the closure program is still too fuzzy. The probes are live,
yet the finite counters, artifact path, and repeatable
boundary-crossing route are not yet named tightly enough to deserve a
full strategy devmap.

Current backing:

- `futon5a/data/alignment.edn` treats this as a `:phase-transition`
  around `:J-paragogy`, already acknowledging that the capability exists
  socially but not yet as Joe's own sustaining route.
- The note explicitly names Charlie monetisation conversations as
  probes, which means the sorry is already testable in principle rather
  than purely abstract.
- The broader paragogy substrate is real elsewhere in the stack, for
  example `futon3/holes/missions/M-f6-recursive.md` and
  `V-f6-thesis-verification.md`; what is missing is the money-coupling.
- `E-globe4-candidate-bridge-v0.md` records a first bridge from `🌐4`'s
  probe obligations to candidate-invariant families.

What honest progress looks like:

- the vague wish becomes a specific artifact or offer
- the offer has a named audience
- there is an explicit payment or uptake event that would move a counter

Until then, the sorry is strategically real but not yet closure-ready.

[← Strategic SORRY Topology](strategic-sorry-topology)

---

## Globe 5 — VSAT Revenue

*Source: `~/code/futon5a/holes/stories/globe5-vsat-revenue.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/globe/5-vsat-revenue` (kind `:globe`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

`🌐5` is structurally different from `🌐1`. Here the product already
exists. The sorry is the gap between "it works" and "it pays." The
alignment note currently describes the Eric path as the sharpest
visible route, without yet claiming that it is the only route or that
it is already robust enough to count as closure.

This handle is a candidate-later full devmap. It likely deserves a
full closure program once one concrete revenue path is sharp enough to
be tracked as more than a hope.

Current backing:

- `futon5a/data/alignment.edn` marks this as `:building`, not absent:
  the product side exists, the money side is the thin part.
- `futon5a/personal-work-map.md` places Virtual Storytelling Toolkit
  consulting with Eric in the cleanest current throughput quadrant.
- `futon5a/speculative-futures-q1q2q3.md` already frames the deeper
  issue: the present PoC is real, but the future consulting pipeline is
  not yet.
- `E-globe5-candidate-bridge-v0.md` records a first bridge from `🌐5`'s
  revenue-path obligations to candidate-invariant families.

What honest progress looks like:

- the current PoC completes cleanly
- there is a case-study-quality evidence trail
- at least one next-buyer or follow-on path becomes explicit

The route starts to look like a channel only when it becomes more than
a one-customer exception.

[← Strategic SORRY Topology](strategic-sorry-topology)

---

## Globe 6 — Governance Interface

*Source: `~/code/futon5a/holes/stories/globe6-governance-interface.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/globe/6-governance-interface` (kind `:globe`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

`🌐6` is the missing personal analog for the contribution log /
governance-as-data side of the alignment chain. The stack has the
commons-facing shape in theory; what is missing is the adapter that
would make it legible and usable at Joe scale.

This handle is currently deferred. That is not because it is false, but
because it is not yet load-bearing relative to the other strategic
pressures. The note in alignment is explicit on that point: it becomes
more relevant when the system is nearer I4 maturity rather than under
the current boundary-crossing pressure.

Current backing:

- `futon5a/data/alignment.edn` locates the sorry at placemat inter-edge
  `:X-contribution-log` and says the closest current analog is the
  FUTON evidence store, which tracks Joe's work but not commons
  contribution in governance terms.
- `futon3/holes/strategy/README.md` classifies the handle as defer, not
  because it is conceptually empty, but because it is not on the current
  strategic front line.
- `E-globe6-candidate-bridge-v0.md` records a first bridge from `🌐6`'s
  adapter obligations to candidate-invariant families.

What honest progress looks like:

- a tiny adapter exists
- contribution events have a stable schema
- commons-facing traces become legible without inventing a full program

This sorry does not yet need a large closure apparatus, but it does need
to remain named so its absence is not mistaken for irrelevance.

[← Strategic SORRY Topology](strategic-sorry-topology)

---

## Globe 7 — Novelty Floor

*Source: `~/code/futon5a/holes/stories/globe7-novelty-floor.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/globe/7-novelty-floor` (kind `:globe`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

`🌐7` is best understood as a diagnostic signal or tick rather than as a
full strategic program. The note in alignment is already metaphorically
right: "No new edges in topology. The cold trail. The ant you haven't
met."

That matters. But it does not yet need the same closure machinery as
the more articulated Strategic SORRYs. In current classification it is
diagnostic only: a useful symptom that should influence policy and
attention, not yet a standalone devmap family.

Current backing:

- `futon5a/data/alignment.edn` defines this as a terminal-vocabulary
  `:constraint`, but only at `:severity :info`, which is already a clue
  that it behaves differently from the load-bearing critical sorrys.
- The underlying observable-source map in alignment defines novelty as a
  measurable edge-formation proxy, so this is not merely poetic
  language.
- `futon3/holes/strategy/README.md` explicitly classifies it as
  diagnostic-only.

What honest progress looks like:

- a real new edge appears in the topology
- or the system can say which broader strategic front is suppressing
  novelty

The symptom matters precisely because it is not self-interpreting.

[← Strategic SORRY Topology](strategic-sorry-topology)

---

## Globe 8 — Policy Transition

*Source: `~/code/futon5a/holes/stories/globe8-policy-transition.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/globe/8-policy-transition` (kind `:globe`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

`🌐8` is the regime-change handle:
`:pi-scholar -> :pi-free-solo` via `:pi-consultant`. Alignment already
names Bristol as the Ji point after which a balanced outward regime
becomes available.

This handle already has a full strategy devmap at
`futon3/holes/strategy/globe8-policy-transition.devmap`. The devmap
says the question is broader than whether a single violation clears.
Closure means the governing policy actually shifts: workstream shares
rebalance, evidence topics move from talking about the transition to
enacting it, and the new policy remains available on later passes.

Current backing:

- `futon5a/data/alignment.edn` defines the transition explicitly, gives
  the via-state, and already names Bristol as the threshold event.
- `futon5a/holes/missions/M-a-sorry-enterprise.md` treats this as the
  second live critical test, with commit ratios, evidence-topic shares,
  and maturity movement as observables.
- `globe8-policy-transition.devmap` decomposes closure into
  instrumentation, Ji-point performance, balance shift, ratchet, and a
  closed-for-now regime.
- `E-globe8-candidate-bridge-v0.md` records a first bridge from `🌐8`'s
  regime-shift prototypes to candidate-invariant families.

What honest progress looks like:

- the Ji-point event actually occurs
- workstream and evidence ratios change over a bounded window
- later cycles reselect the new policy rather than merely rehearsing it

`🌐8` is the regime-level companion to `🌐2`: one asks whether the
constraint loop is honest; the other asks whether the whole governing
policy is different now.

[← Strategic SORRY Topology](strategic-sorry-topology)

---

## The Exotype Move

*Source: `~/code/futon5a/holes/stories/leaf-0.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/0` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

An *exotype* in futon vocabulary is an abstract wiring diagram — a
specification of how some set of concepts relate, at a level of generality
that can be instantiated across many domains. A *xenotype*, one level
further out, is the derivation procedure that produces exotypes from
other exotypes. The social-exotype is the one that gave this whole
family its name: a wiring diagram for how multi-agent work actually
cooperates, with slots for the concrete agents, transports, and
coordination rules to be filled in per target domain.

The seven missions gathered here are the result of that derivation at
different moments and in different domains. Some are the *instantiation*
steps — agency-refactor and peripheral-model each being the INSTANTIATE
phase of applying a social-exotype derivation to a specific layer of
the futon3c stack. Some are *further exotypes* derived from the
social-exotype skeleton — coordination-exotype, evidence-landscape-exotype
— each a mission whose purpose is to produce *another* wiring diagram
for *another* domain. And one is a bridge: pattern-exotype-bridge, which
embeds the 791-pattern library itself into the exotype space so that any
library pattern becomes executable in any exotype-compatible domain.

If you came from the [Stack Geometry](stack-geometry) mission wondering
how any wiring diagram gets *reusable* at all, this is the cluster that
answers.

[Social Exotype (futon3c)](social-exotype-futon3c) — the source-of-truth wiring diagram  
[Agency Refactor](agency-refactor) — INSTANTIATE of social-exotype at the agency layer  
[Peripheral Model](peripheral-model) — INSTANTIATE of social-exotype at the peripheral layer  
[Pattern-Exotype Bridge](pattern-exotype-bridge) — embedding the library into exotype space  
[Social Exotype (futon5)](social-exotype-futon5) — the cross-repo composition target  
[Coordination Exotype](coordination-exotype) — derivation in progress  
[Evidence-Landscape Exotype](evidence-landscape-exotype) — derivation in progress

---

### Social Exotype (futon3c) | social-exotype-futon3c

The *social-exotype* in futon3c is the canonical wiring diagram for
social coordination in the futon sense. Its authorship note is brief
to the point of terseness: "Claude (+ Codex for futon5 composition
extension)." But the artefact beneath that line is substantial:
`social-exotype.edn` with eight core components (8/8 verified), a
verified standalone composition, a verified composed-with-futon5
composition merged into main on commit `d5ae681`, and the bootstrap
closure demonstration from Part III of the M-social-exotype mission.

In concrete terms this wiring diagram specifies: how agents of
different kinds work together on a shared surface, how
coordination-talk is distinguished from action-talk (the `mode-gate`
component), how evidence is attributed across agents, how
discussions decompose, and how all of this composes across repos.
The abstractness is intentional. Most of the slots are empty. Fill
them in for one repo and you get an *instantiation* — which is what
Agency Refactor and Peripheral Model each are.

<!-- sorrys-for-scene:social-exotype-futon3c:start -->

**Open sorrys in this scene.**

- ✓ Mission `social-exotype@futon3c`: `:complete` / `:settled` — *DONE-NEEDS-RETRO (Parts I-III substantively complete, traceability docs …*
- ✓ Closes `sorry|devmap|futon3|P7|pattern-creation-workbench` (medium, sim 0.43)

<!-- sorrys-for-scene:social-exotype-futon3c:end -->

[← Overview](overview) · [Agency Refactor →](agency-refactor)

---

### Agency Refactor | agency-refactor

Agency Refactor is the INSTANTIATE step of the social-exotype
derivation xenotype, applied to the agency layer. The predecessor
artefacts make the move concrete: an `ARGUMENT.flexiarg` with
requirements R1–R11 derived from agency and realtime patterns;
`social-exotype.edn` with all eight components verified; the
`compose-parallel` operation merged into futon5 main. With those in
place, the refactor becomes specific work: port the existing agency
implementation onto the slots the wiring diagram exposes, and check
that the bootstrap closure from Part III of M-social-exotype still
holds.

For a reader this scene matters because it illustrates the payoff of
the exotype discipline. Agency is not redesigned from scratch; it is
*re-instantiated* from an already-verified wiring diagram. The work
is translation, not invention. The invariants were checked once, at
the social-exotype level, and every instantiation inherits them.

<!-- sorrys-for-scene:agency-refactor:start -->

**Open sorrys in this scene.**

- ✓ Mission `agency-refactor@futon3c`: `:complete` / `:settled` — *Complete*

<!-- sorrys-for-scene:agency-refactor:end -->

[← Social Exotype (futon3c)](social-exotype-futon3c) · [Peripheral Model →](peripheral-model)

---

### Peripheral Model | peripheral-model

Peripheral Model is the sibling instantiation at the peripheral
layer. Where Agency Refactor applies the social-exotype wiring
diagram to agencies, Peripheral Model applies the same diagram — with
a specific focus on the `mode-gate` component from
§S-mode of `social-exotype.edn` (lines 151–162) — to peripherals. The
prior artefacts are analogous: ARGUMENT.flexiarg R10 (the
coordination-vs-action distinction as a first-class constraint),
`peripheral-spec.md` with five core peripherals (explore, edit, test,
deploy, reflect), and the DISCUSS → DIAGNOSE → EXECUTE transition
protocol encoded in `mode-gate.flexiarg`.

The agency layer and the peripheral layer are not the only layers the
exotype can be applied to. They are the first two, and they work.
That is the empirical evidence that the exotype move scales.

<!-- sorrys-for-scene:peripheral-model:start -->

**Open sorrys in this scene.**

- ✓ Mission `peripheral-model@futon3c`: `:complete` / `:settled` — *Complete (26e625b)*
- ✓ Closes `sorry|devmap|futon3|P14|peripheral-runtime` (medium, sim 0.40)

<!-- sorrys-for-scene:peripheral-model:end -->

[← Agency Refactor](agency-refactor) · [Pattern-Exotype Bridge →](pattern-exotype-bridge)

---

### Pattern-Exotype Bridge | pattern-exotype-bridge

Pattern-Exotype Bridge is the mission that crosses the exotype family
with the pattern library. There are 791 flexiarg patterns in futon3's
library, each a structured-argument artefact that applies in some
specific context. The bridge mission's claim: all of them can be
projected into the 8-bit exotype space, using the sixty-four hexagrams
of the futon exotype system as anchors.

The method is concrete: PCA-reduce the embedding space to 32
dimensions, ridge-regress onto 320 anchor points (64 hexagrams + 256
exotypes), measure bit-accuracy on a hexagram holdout set (57.9% —
above chance, below determinism). Domain coherence scored separately:
corps domain scores 1.07, vsat domain scores 0.40.

The point is not that the embedding is perfect. The point is that
*any pattern from the library can be re-expressed in exotype terms*,
which means any pattern can be applied in any domain the exotype
system covers. The pattern library stops being a futon-local
collection and becomes a cross-domain substrate.

<!-- sorrys-for-scene:pattern-exotype-bridge:start -->

**Open sorrys in this scene.**

- ✓ Mission `pattern-exotype-bridge@futon5`: `:complete` / `:settled` — *Complete*
- ◐ Scene gap — Pattern-Exotype Bridge bit-accuracy 57.9% is above chance, below determinism  
   `sorry|scene|leaf-0|pattern-exotype-bridge-accuracy`
- ✓ Closes `sorry|devmap|futon3a|P7|xenotype-ct-attachment` (high, sim 0.60)

<!-- sorrys-for-scene:pattern-exotype-bridge:end -->

[← Peripheral Model](peripheral-model) · [Social Exotype (futon5) →](social-exotype-futon5)

---

### Social Exotype (futon5) | social-exotype-futon5

This scene is terse because its mission is terse. In futon5, the
social-exotype has a named mission-doc but very little written
content beyond the name itself. You can treat this as a *placeholder*
— the intent is to carry the social-exotype derivation from futon3c
into futon5's composition context, but the work has not yet been
articulated in a durable mission document.

For a reader arriving here from the Pattern-Exotype Bridge: yes, the
futon5 side has fewer furnishings than the futon3c side. No, that does
not mean the work is not real — the composed `compose-parallel`
artefact cited under Agency Refactor is evidence that the cross-repo
composition already works. What is missing here is the *writing* of
that work into a mission-document shape.

<!-- sorrys-for-scene:social-exotype-futon5:start -->

**Open sorrys in this scene.**

- ✓ Mission `social-exotype@futon5`: `:complete` / `:settled` — *complete*

<!-- sorrys-for-scene:social-exotype-futon5:end -->

[← Pattern-Exotype Bridge](pattern-exotype-bridge) · [Coordination Exotype →](coordination-exotype)

---

### Coordination Exotype | coordination-exotype

Coordination Exotype is another placeholder-style mission in futon5,
named but minimally written. Its purpose is clear from context: apply
the social-exotype derivation procedure to a specifically
coordination-flavoured domain. Multi-agent coordination has its own
sub-language — handoffs, synchronisation, race-avoidance, consensus —
and producing a coordination-specific exotype from the social-exotype
skeleton means filling the mode-gate and transport-adapter slots with
coordination-primitives.

The mission is not yet sketched out; its presence in this anthology is
its presence in the corpus. Among the seven members of this cluster,
this is one of the scenes where you the reader should feel free to
pencil in your own sentence about what the exotype *will be*.

<!-- sorrys-for-scene:coordination-exotype:start -->

**Open sorrys in this scene.**

- ✓ Mission `coordination-exotype@futon5`: `:complete` / `:settled` — *complete*

<!-- sorrys-for-scene:coordination-exotype:end -->

[← Social Exotype (futon5)](social-exotype-futon5) · [Evidence-Landscape Exotype →](evidence-landscape-exotype)

---

### Evidence-Landscape Exotype | evidence-landscape-exotype

The Evidence-Landscape Exotype, named in futon5 alongside the
coordination-exotype, is another open slot for a derivation that has
not yet been filled in. The intent — visible from title and context —
is to produce an exotype for the *landscape-intelligence* workflow:
how evidence is gathered across distributed sources, how it is
evaluated for relevance, how it is composed into landscapes (the
working vocabulary of futon7's lead-report work), and how the pipeline
from raw evidence to landscape claim is made trust-worthy.

Like the other placeholder-style missions in this anthology, its
mission-doc articulation remains thin — the registry entry for the
mission may read as `:active` or `:spec-only` but the narrative here
describes a scarcely-written mission doc, not a half-run mission.
The distinction matters: **registry status** (what the mission
tracker says) and **articulation status** (whether a reader can
orient from the mission doc) can disagree, and when they do, the
anthology should say which axis it's reporting on. Here, the
articulation axis is thin; the exotype derivation procedure is
expected to apply, but the derivation itself awaits a mission-doc-
scale investment.

[← Coordination Exotype](coordination-exotype) · [Return to Overview](overview)

---



*Suggested cross-story links (to draft later):*

- → **The Stack Thinks About Itself** (leaf 6.4.4): causal — the
  exotype derivation procedure is one of the abstractions Stack
  Geometry's topology layer makes visible.
- → **Inhabitable Surfaces** (leaf 2): causal — Agency Refactor and
  Peripheral Model are infrastructure that the inhabitable-surfaces
  missions build on.
- → **Proof / Problem** (leaf 6.4.2): contrast — proof missions
  derive from a different xenotype (the proof-search xenotype)
  rather than the social-exotype.

<!-- sorrys-for-scene:evidence-landscape-exotype:start -->

**Open sorrys in this scene.**

- ✓ Mission `evidence-landscape-exotype@futon5`: `:complete` / `:settled` — *complete*

<!-- sorrys-for-scene:evidence-landscape-exotype:end -->

<!-- sorrys-story-summary:start -->

---

## Sorrys in this story — legend and totals

Each scene above carries its own inline `**Open sorrys in this scene.**` block. Reading each scene in order accumulates the whole story's sorry picture. Glyph vocabulary:

○ spec-only/unnamed · ◔ nascent · ◐ prototype · ◕ active · ◉ operational · ✓ closed/settled

**Cluster totals:** 9 sorrys touched — ✓ 7 `settled` · ◐ 1 `prototype` · ○ 1 `unnamed`.

**`:closes` edges emitted:** 3 (high: 1, medium: 2; medium pending operator review).

See `futon5a/holes/tech-notes/TN-a-proof-is-not-a-tree.md` for the sorry / pattern dual-arrow framing and `futon5a/holes/holistic-argument-semilattice.md` for the semilattice these sorries inhabit.

*Generated 2026-04-19 by `futon5a/scripts/append_sorry_footers.py`. Re-running overwrites the marked blocks.*

<!-- sorrys-story-summary:end -->

---

## A Book on the Shelf

*Source: `~/code/futon5a/holes/stories/leaf-1.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/1` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Some leaves in this anthology hold seven missions. Some hold
thirteen. This one holds one: `fm001-book-ramsey@futon5`. It is
alone in its cluster because nothing else in the current mission
set embeds near it — which is itself informative.

[The Book-Ramsey Mission](book-ramsey)

---

### The Book-Ramsey Mission | book-ramsey

`fm001-book-ramsey` is the stack's first FrontierMath target —
a specific Ramsey-theoretic problem (`fm001`) held as a named
mission in `futon5/`. The cluster distance says it shares little
vocabulary with the other 109 missions. This is expected: Ramsey
theory is not the stack's native terrain, and the mission's IDENTIFY
section reflects the content of the problem rather than the content
of the surrounding infrastructure.

What makes this mission interesting is not its prose but its role
as a **probe**. If the stack can close a concrete frontier-math
problem end-to-end — IDENTIFY → MAP → DERIVE → ARGUE → VERIFY →
INSTANTIATE → DOCUMENT — using only its own apparatus (retrieval,
pattern library, evidence discipline), that would be strong
evidence for S1 (Evidence Discipline Works) at a scale larger than
any current closure. If it cannot, the probe tells us which part
of the apparatus is not yet load-bearing.

No claim here that the probe has closed. As of 2026-04-19 the
mission is present in the registry; its state is not strongly
inhabited. It sits on the shelf as the first fm-series target —
the rest of the series would follow the same contract.

[← Overview](overview)

---

*This anthology is a 1-member leaf. The singleton status is
itself the interesting finding — math-evidence missions sit at
the cluster's periphery. If subsequent FrontierMath targets land,
the leaf becomes a multi-member cluster and gets a richer
anthology.*

---

## Inhabitable Surfaces

*Source: `~/code/futon5a/holes/stories/leaf-2.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/2` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

The stack's reflection missions (see the [Portfolio and Sorry](portfolio-and-sorry)
anthology) ask what the stack *thinks* about itself. This anthology asks a
different question: *where do people meet inside the stack?* The family
gathered here is the set of missions that build inhabitable collaborative
surfaces — places where two people can co-create rather than handing text
back and forth through tools that were not designed for shared thinking.

Futon began as a single-operator Emacs system. Over time the infrastructure
accreted around that operator: lab notebooks, forum threads, pattern
libraries, cursor peripherals, an evidence browser, a MUSN coordination
mesh. Each was inhabitable by one person at a time. The missions in this
cluster name a consistent pressure outward: *the surface must be shared*.
WebArxana is the browser-side manifestation; graph-unification is the
data-layer unification; the cursor and forum and lab missions are the
small-scale UX components that hold the whole.

If you came here from Stack Geometry or from the Portfolio/Sorry
anthology, note the pairing: that anthology's missions watch the stack;
this anthology's missions make the stack liveable for more than one
person. Both are necessary.

[WebArxana](webarxana) — the collaborative hypergraph browser  
[Graph Unification](graph-unification) — one graph under forum + lab + patterns + code  
[Arxana Graph Persistence](arxana-graph-persistence) — graph-structured lab notebooks  
[Sliding Blackboard](sliding-blackboard) — code blocks as first-class side panels  
[Emacs Cursor Peripheral](emacs-cursor-peripheral) — the cursor as peripheral  
[Labs Integration](labs-integration) — the lab peripheral, polished  
[Agency + Forum](agency-forum) — multi-agent routing meets threaded discussion  
[Forum Organization](forum-organization) — tags-first forum structure  
[PAR Session Punctuation](par-session-punctuation) — closing-the-session as a peripheral

---

### WebArxana | webarxana

WebArxana is the most inhabited edge of this family. The Arxana knowledge
graph has rich structure — entities, relations, hyperedges, typed
evidence — but its primary interface has always been an Emacs browser.
That is powerful for the person at the keyboard and closed to everyone
else. There is no shared workspace in which two people can co-create
within the same hypertext.

The mission proposes a browser-based surface. Collaborators add short
texts, draw typed connections between them, inspect what each person has
contributed, and gradually build a shared structured world — a replay of
the chorus/song/chapbook workflow Joe has long favoured, but
hypergraph-native and web-reachable. Today WebArxana runs at localhost
and will soon be the rendering target of this anthology itself. If you
are reading this inside VSAT, the content still came from the same
futon1a source WebArxana reads.

For a contributor who is not Joe, this mission is the first door. You
cannot walk into Emacs. You can walk into a URL.

<!-- sorrys-for-scene:webarxana:start -->

**Open sorrys in this scene.**

- ◐ Mission `webarxana@futon4`: `:in-progress` / `:prototype` — *INSTANTIATE (2026-04-12). Interactive testing pass done.*
- ◐ Scene gap — WebArxana collaborator-facing auth / deployment  
   `sorry|scene|leaf-2|webarxana-anon-auth`

<!-- sorrys-for-scene:webarxana:end -->

[← Overview](overview) · [Graph Unification →](graph-unification)

---

### Graph Unification | graph-unification

Graph Unification is the data-layer counterpart to WebArxana's UX move.
Today the stack stores the same sorts of things in four separate places:
forum threads live in local EDN, lab activity in MUSN session files,
patterns in a TSV sigil index, code and docs in filesystem paths with
no graph representation at all. These four vocabularies describe the
same underlying world — claims, connections, evidence, attribution —
but cannot be queried as one thing.

Futon1a's graph-memory provides what is needed: a Datascript runtime
backed by an event log, persisted to XTDB, with entity/relation model
and temporal queries ("what did we know on date X?"). This mission
takes the four data stores and projects them into that runtime so the
unified view is available to any collaborator and any query.

The practical result for a reader: when you search in WebArxana or
click through to a pattern, you are traversing one connected graph
that was until recently four disconnected islands.

<!-- sorrys-for-scene:graph-unification:start -->

**Open sorrys in this scene.**

- ○ Mission `graph-unification@futon3`: `:ready` / `:spec-only` — *inferred:ready (0/6 gates)*
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:graph-unification:end -->

[← WebArxana](webarxana) · [Arxana Graph Persistence →](arxana-graph-persistence)

---

### Arxana Graph Persistence | arxana-graph-persistence

The persistence layer beneath Arxana was for a long time a
what-works-until-it-breaks affair. Lab notebooks, annotations, links
drawn between them — stored somewhere, serialised somehow, reloaded
when possible. This mission was the move to put that layer on a firm
foundation: graph-structured persistence with real invariants, real
round-trip guarantees, real timestamps.

The mission's content is short because the work is largely
*completed*. The implementation landed, the persistence layer works,
and the surface above it (the Arxana browser, WebArxana, the forthcoming
collaborative editor) can trust that what gets saved comes back as it
went in. That trust is what makes the inhabitable-surface missions
downstream viable at all.

Treat this scene as a foundation stone you are meant to walk over, not
stop on.

<!-- sorrys-for-scene:arxana-graph-persistence:start -->

**Open sorrys in this scene.**

- ✓ Mission `arxana-graph-persistence@futon3`: `:complete` / `:settled` — *Complete (Phases 1-4)*
- ✓ Closes `sorry|devmap|futon1|P3|graph-memory-schema-mirroring` (high, sim 0.58)

<!-- sorrys-for-scene:arxana-graph-persistence:end -->

[← Graph Unification](graph-unification) · [Sliding Blackboard →](sliding-blackboard)

---

### Sliding Blackboard | sliding-blackboard

The sliding blackboard is a small move with an outsized effect on how
Emacs chat feels to a user. Before this mission, code blocks in the
`*claude-repl*` buffer rendered inline and mixed with the chat prose —
functional, but cramped. After this mission, code blocks slide out to
their own panels: stacked, properly fontified, navigable as separate
objects, leaving the chat buffer lean and readable.

If you have read the Portfolio/Sorry anthology's scene on Stack
Inhabitation, this is one of the patterns that mission argues for:
*the surface must earn inhabitation*. The chat buffer earning
inhabitation means stripping out what does not belong there.
Code blocks belong somewhere else; the sliding blackboard is where.

<!-- sorrys-for-scene:sliding-blackboard:start -->

**Open sorrys in this scene.**

- ○ Mission `sliding-blackboard@futon3c`: `:deferred` / `:spec-only` — *DEFERRED (2026-02-15)*
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:sliding-blackboard:end -->

[← Arxana Graph Persistence](arxana-graph-persistence) · [Emacs Cursor Peripheral →](emacs-cursor-peripheral)

---

### Emacs Cursor Peripheral | emacs-cursor-peripheral

This mission takes a concept that the stack has been circling — *the
peripheral* — and applies it to the cursor. A peripheral, in the futon
vocabulary, is a constrained environment in which an agent can do a
specific kind of work, with a shape that earns inhabitation rather
than simply existing. The cursor is, in that sense, the original and
most-used peripheral in Emacs: a little window on the text through
which every other operation is mediated.

The mission is at the assign-an-owner stage — Claude for UX, probably
Codex for the service plumbing — and its exact scope depends on
which parts of the cursor's work become peripheral-shaped and which
remain diffuse. As a reader you should expect this scene to be thinner
than most; what matters here is that the cursor itself has been
recognised as peripheral material.

<!-- sorrys-for-scene:emacs-cursor-peripheral:start -->

**Open sorrys in this scene.**

- ○ Mission `emacs-cursor-peripheral@futon3`: `:ready` / `:spec-only` — *:greenfield*
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:emacs-cursor-peripheral:end -->

[← Sliding Blackboard](sliding-blackboard) · [Labs Integration →](labs-integration)

---

### Labs Integration | labs-integration

Labs Integration is this anthology's busiest scene: a catch-all for
improvements to the lab peripheral — completed work and open follow-ups
intermingled — that the current mission owner has flagged for splitting
before continuing. **Registry status: `:complete` (superseded); mission-doc
articulation: still a holding pattern rather than a coherent project.**
The registry closed the mission because its work was subsumed into the
futon3c evidence-landscape work; the mission doc itself was never
rewritten to reflect that. Somewhere in its pending-work list is the
next thing that should become its own mission.

If you are an incoming contributor looking for a place to pick up work
that is neither trivial nor yet committed to someone, this is the list
to scan.

<!-- sorrys-for-scene:labs-integration:start -->

**Open sorrys in this scene.**

- ✓ Mission `labs-integration@futon3`: `:complete` / `:settled` — *COMPLETE (SUPERSEDED) — Lab capture reframed as evidence landscape in fu…*
- ✓ Closes `sorry|devmap|futon4|P6|multi-user-support` (medium, sim 0.45)

<!-- sorrys-for-scene:labs-integration:end -->

[← Emacs Cursor Peripheral](emacs-cursor-peripheral) · [Agency and Forum →](agency-forum)

---

### Agency and Forum | agency-forum

Agency and Forum brings two things together that have lived
separately: the multi-agent routing apparatus (Agency) and the
threaded discussion surface (Forum). Agency routes a message to a
specific agent or group of agents based on its content, intent, and
context; Forum holds the ongoing discussion record between
contributors. Joining them is conceptually simple and practically
slow: every place that mentions "who said what, and who responds
next" has to understand both layers.

**Registry status: `:complete` (superseded); mission-doc
articulation: still a holding pattern.** As with Labs Integration,
the mission was closed in the registry because futon3c's evidence
interfaces replaced the Agency/Forum boundary rather than because
the mission's own work list was executed. It spans two layers, and
will likely split into separate Agency-focused and Forum-focused
sub-missions before any of the work advances meaningfully. The
scene is here to mark the recognition that the split is needed, not
yet to mark the split.

<!-- sorrys-for-scene:agency-forum:start -->

**Open sorrys in this scene.**

- ✓ Mission `agency-forum@futon3`: `:complete` / `:settled` — *COMPLETE (SUPERSEDED) — Agency/Forum interfaces replaced by futon3c evid…*
- ○ Scene gap — M-agency-forum spans two layers; owner + split pending  
   `sorry|scene|leaf-2|agency-forum-split-pending`
- ✓ Closes `sorry|devmap|futon3|P11|system-self-description` (medium, sim 0.38)

<!-- sorrys-for-scene:agency-forum:end -->

[← Labs Integration](labs-integration) · [Forum Organization →](forum-organization)

---

### Forum Organization | forum-organization

Forum Organization is the tags-first proposal for structuring the
forum surface. Rather than hierarchical channels or free-form chat,
the forum is organised around a tag taxonomy that contributors can
extend over time. This scales better than fixed structure and
decays more gracefully than flat chat. It also plays naturally with
the graph-unification mission upstream: tags become entities, tag
application becomes a typed relation, and querying the forum
becomes graph-queryable.

The mission is owned by Claude and is in the sort of state where
concrete implementation follows a relatively clear plan. As a
reader arriving here for the first time, note that "tags-first"
is opinionated rather than default — many forum systems grow
channels instead. The futon stack has chosen tags.

<!-- sorrys-for-scene:forum-organization:start -->

**Open sorrys in this scene.**

- ○ Mission `forum-organization@futon3`: `:ready` / `:spec-only` — *:greenfield*
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:forum-organization:end -->

[← Agency and Forum](agency-forum) · [PAR Session Punctuation →](par-session-punctuation)

---

### PAR Session Punctuation | par-session-punctuation

PAR — *Problem–Approach–Result* — is a pattern the stack uses to
close a session. When an agent or a contributor stops work, they
leave behind a small record: what was the problem being faced,
what approach was taken, what was the result. That record is
evidence. It is also, as this mission proposes, punctuation —
the way a session signs its own close and makes itself legible to
the next person who picks up.

The mission would make PAR a first-class peripheral: a small UX
flow that prompts for the three fields at a natural moment,
records the result into the evidence stream, and gives the
departing contributor a clean way to leave. That is how a
collaborative surface earns the right to hand off gracefully.

As a reader finishing this anthology, you are at a natural PAR
moment yourself. You have read nine scenes about making the stack
shareable. What the stack *is* — after a contributor besides Joe
uses it with these missions in place — is the result of a PAR
the mission authors have only partly written.

[← Forum Organization](forum-organization) · [Return to Overview](overview)

---



*Suggested cross-story links (to draft later):*

- → **The Stack Thinks About Itself** (leaf 6.4.4): thematic — shares
  Stack Inhabitation's "surface must earn inhabitation" discipline.
- → **IRC / CLI** (leaf 6.5.2): adjacency — infrastructure underneath
  these UX layers.
- → **The Exotype Move** (leaf 0): causal — the social-exotype
  derivation produced some of the wiring diagrams these missions
  realise.

<!-- sorrys-for-scene:par-session-punctuation:start -->

**Open sorrys in this scene.**

- ✓ Mission `par-session-punctuation@futon3`: `:complete` / `:settled` — *COMPLETE (SUPERSEDED) — PAR emission via futon3c peripheral/reflect.clj;…*

<!-- sorrys-for-scene:par-session-punctuation:end -->

<!-- sorrys-story-summary:start -->

---

## Sorrys in this story — legend and totals

Each scene above carries its own inline `**Open sorrys in this scene.**` block. Reading each scene in order accumulates the whole story's sorry picture. Glyph vocabulary:

○ spec-only/unnamed · ◔ nascent · ◐ prototype · ◕ active · ◉ operational · ✓ closed/settled

**Cluster totals:** 11 sorrys touched — ✓ 4 `settled` · ◐ 2 `prototype` · ○ 5 `spec-only`.

**`:closes` edges emitted:** 3 (high: 1, medium: 2; medium pending operator review).

See `futon5a/holes/tech-notes/TN-a-proof-is-not-a-tree.md` for the sorry / pattern dual-arrow framing and `futon5a/holes/holistic-argument-semilattice.md` for the semilattice these sorries inhabit.

*Generated 2026-04-19 by `futon5a/scripts/append_sorry_footers.py`. Re-running overwrites the marked blocks.*

<!-- sorrys-story-summary:end -->

---

## The Math Evidence Machine

*Source: `~/code/futon5a/holes/stories/leaf-3.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/3` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Four missions cluster here, all in the `f6-*` (futon6) family —
the stack's math-evidence subsystem. Their shared vocabulary is
**superpod** and **ingest**: what do you pull from the arxiv,
how do you run it at scale, what does Rob's team's infrastructure
let you do, and how do you evaluate the results once they arrive.

[F6 Arxiv Ingest](f6-arxiv)  
[F6 Ingest Pipeline](f6-ingest)  
[F6 Evaluation](f6-eval)  
[Superpod Access](superpod-access)

---

### F6 Arxiv Ingest | f6-arxiv

`f6-arxiv@futon3` is the mission to pull arxiv papers into the
stack — at scale, filtered by relevance, chunked for embedding —
as raw material for math-evidence work. The mission's first-order
question is volume: the stack's pattern library is 853+ flexiargs,
the arxiv math corpus is orders of magnitude larger, and the
mission is about the **pipeline from arxiv to stack** rather than
about any particular paper.

Registry status: `:active`. This is a funnel mission — it feeds
the ingest mission downstream of it and the evaluation mission
downstream of that. Articulation focuses on pipeline mechanics
(what to pull, what to keep, what to discard).

[← Overview](overview) · [F6 Ingest →](f6-ingest)

---

### F6 Ingest Pipeline | f6-ingest

`f6-ingest` exists in two places — `@futon3` and `@futon5` — and
that duplication is itself the cluster's oddity. The futon3
version is the pipeline machinery; the futon5 version is the
wiring diagram that says where the pipeline sits in the overall
stack topology.

The mission handles the second stage of the funnel: arxiv raw →
chunked + embedded + indexed → queryable as a retrieval surface.
The BGE embedding model (per Joe's standing preference over
R-GCN retrieval) is the load-bearing choice here. Hard negatives
and the rest of the retrieval discipline land downstream in
evaluation.

Registry: `:active` on both sides. The futon3 copy is the
executable one; the futon5 copy is the diagram.

[← F6 Arxiv Ingest](f6-arxiv) · [F6 Evaluation →](f6-eval)

---

### F6 Evaluation | f6-eval

`f6-eval@futon5` is the mission for evaluating whether the
ingest-and-retrieve pipeline actually produces useful results for
downstream math work. Evaluation is where the audience-first-probes
discipline kicks in (person/org targets work; topic-phrased
"audience" decays to keyword noise). A math-evidence evaluation
should cite specific hit quality against specific queries, not
aggregate retrieval scores against synthetic benchmarks.

This is the mission that would tell the stack whether Rob's
superpod investment is producing landing-worthy evidence of
math-retrieval quality. Articulation: the mission is named but
full evaluation protocol specification is thin; concrete eval
runs against named queries would close the mission at
`:operational`.

[← F6 Ingest](f6-ingest) · [Superpod Access →](superpod-access)

---

### Superpod Access | superpod-access

The cluster's distinctive word is **superpod** because these
missions share an infrastructure dependency that does not exist
for the rest of the stack: access to SMU's A100-80GB×160 superpod
via Rob's arrangement, with maneframe V100s as fallback. That
dependency shows up in the missions' vocabulary — "rob superpod,"
"superpod access" — and is what makes the cluster distinctive.

This is not itself a mission; it is a shared precondition the
four missions in this leaf all rest on. If superpod access
changes (lapses, expands, moves to a different partition), all
four missions are affected in ways the rest of the stack is not.
The dependency is worth surfacing here because it is load-bearing
and not visible from the individual mission docs.

[← F6 Evaluation](f6-eval)

---

*Size-4 leaf. The cluster centers on math-evidence infrastructure
rather than on math-evidence content. If concrete mathematical
targets land (the fm-series FrontierMath probe, adjacent
math-evidence missions), they grow a sibling cluster that uses
this pipeline rather than joining this cluster.*

---

## Couplings and Probes

*Source: `~/code/futon5a/holes/stories/leaf-4.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/4` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Five missions cluster here, all in `futon5`, all concerned with
**couplings** — the way parts of a system constrain each other —
and with **probes** that would detect when a coupling has become
informative or defective. The distinctive words tell the story:
coupling, xor, diversity, evolution, tradeoff. This is the
cluster where the stack tries to reason about itself as a
coupled system rather than as a collection of independent
components.

[Coupling as Constraint](coupling-as-constraint)  
[XOR Coupling Probe](xor-coupling-probe)  
[TPG Coupling Evolution](tpg-coupling-evolution)  
[Fulab Wiring Survey](fulab-wiring-survey)  
[SCI Detection Pipeline](sci-detection-pipeline)

---

### Coupling as Constraint | coupling-as-constraint

`coupling-as-constraint@futon5` is the cluster's root idea:
couplings between stack components are not always bad. A tight
coupling is often what lets two components do together what
neither could do alone. The mission asks: when is coupling a
constraint that enables, and when is it a constraint that
entraps?

This is not abstract systems theory — it is the stack asking
whether e.g. the tight coupling between Portfolio Inference and
the evidence ledger is load-bearing (changing the ledger would
break inference in exactly the right way) or brittle (changing
either side requires coordinated changes across five files).

Registry: `:active`. Articulation: the mission sets up the
vocabulary the other four in this cluster use.

[← Overview](overview) · [XOR Coupling Probe →](xor-coupling-probe)

---

### XOR Coupling Probe | xor-coupling-probe

`xor-coupling-probe@futon5` is a specific experiment: when two
components are coupled, does their joint behaviour XOR differently
from their independent behaviour? If yes, the coupling is
informative (the joint state carries information neither side has
alone). If no, the coupling is redundant — either side could be
replaced by the other.

XOR is the discriminator because XOR detects precisely the cases
where neither component alone captures what the pair captures.
Classical correlation measures miss this because they miss
disjunctive structure.

Registry: `:active`. The probe is a small instrument for a
larger question.

[← Coupling as Constraint](coupling-as-constraint) · [TPG Coupling Evolution →](tpg-coupling-evolution)

---

### TPG Coupling Evolution | tpg-coupling-evolution

`tpg-coupling-evolution@futon5` imports a specific vocabulary —
Tangled Program Graphs (TPG) — to think about couplings that
evolve. TPG is a genetic-programming formalism where small
programs coalesce into larger programs via explicit reuse; the
formalism is useful here because it makes coupling-change
**visible as a graph rewrite** rather than as parameter drift.

The mission asks whether the stack's own component structure
evolves in TPG-like ways: do small patterns coalesce into
larger patterns by explicit reuse, and can that coalescence be
instrumented?

This is at the intersection of pattern-library mechanics and
coupling-as-constraint thinking. Articulation: moderate; the
mission is a named probe more than a committed build.

[← XOR Coupling Probe](xor-coupling-probe) · [Fulab Wiring Survey →](fulab-wiring-survey)

---

### Fulab Wiring Survey | fulab-wiring-survey

`fulab-wiring-survey@futon5` is the inventory mission: **what is
actually coupled to what** in the current stack? Before any
specific probe can run, the stack needs a map of existing
couplings. Fulab is the name Joe uses for the lab peripheral
cluster — the survey is of the lab's internal wiring.

This mission is a precondition for the others in this leaf. It
is also the least glamorous: surveys are inventory work, not
insight work. But without it, the coupling-probe experiments
have nothing concrete to probe.

Registry: `:active`. Survey-state would be the natural closure
artefact — an EDN map of component-to-component couplings,
classified.

[← TPG Coupling Evolution](tpg-coupling-evolution) · [SCI Detection Pipeline →](sci-detection-pipeline)

---

### SCI Detection Pipeline | sci-detection-pipeline

`sci-detection-pipeline@futon5` sits slightly apart from the
other four. Its frame is detection — finding **something** in
a pipeline, rather than reasoning about couplings per se — but
it clusters here because its vocabulary overlaps (tradeoffs,
diversity, evolution).

The specific target is the kind of detection relevant to futon5's
mesoscale sense-making work: finding signal in pipelines that
carry heterogeneous evidence. What is detectable depends on how
the pipeline is coupled, which is why it earns a place in this
cluster.

Articulation: the mission is a probe; concrete pipelines and
detection thresholds are deferred.

[← Fulab Wiring Survey](fulab-wiring-survey)

---

*Size-5 leaf. The cluster is conceptually tight (coupling as
first-class subject) but practically scattered (five different
probe-level missions rather than one big coupling mission). A
future move is probably consolidation: one
`coupling-instrumentation@futon5` mission that holds all four
probes as its sub-cycles.*

---

## Wires and Gates

*Source: `~/code/futon5a/holes/stories/leaf-5.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/5` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Five missions cluster here, all about **how messages move and
how evidence gets recorded**. The transport layer (IRC, WebSocket,
adapter code) and the evidence discipline (PSR/PUR) sit together
because they are the two halves of one loop: the transport carries
agent speech across surfaces, and the PSR/PUR mesh turns that
speech into structured evidence.

[IRC Stability](irc-stability)  
[PSR/PUR Mesh Peripheral](psr-pur-mesh-peripheral)  
[Transport Adapters](transport-adapters)  
[WS Emacs Log Stream](ws-emacs-log-stream)  
[M1 Smoke Test](m1-smoke-test)

---

### IRC Stability | irc-stability

`IRC-stability@futon3c` is the mission to make IRC — the stack's
primary human-readable transport — **reliable** under the load of
routine agent coordination. Drop-outs, duplicate messages, and
re-connect races produce evidence gaps; a transport that silently
drops messages is a transport that silently loses context.

The mission is not exciting but is foundational. Until IRC is
stable, everything downstream (agent routing, forum bridge, CLI
invocation via IRC) carries a shadow failure mode that is hard
to diagnose because the symptom looks like "the agent didn't
respond" rather than "the agent's response was dropped in
transit."

Registry: `:active`. Closure would be operator-confirmable via
sustained use without drop-out incidents.

[← Overview](overview) · [PSR/PUR Mesh →](psr-pur-mesh-peripheral)

---

### PSR/PUR Mesh Peripheral | psr-pur-mesh-peripheral

`psr-pur-mesh-peripheral@futon3c` is the mission for the
peripheral that **makes PSR and PUR automatic**. PSR (Pattern
Selection Record) is what the agent records when it picks a
pattern before acting; PUR (Pattern Use Record) is what it
records when the action completes. Done by hand, these records
are inconsistent — done by peripheral, they become automatic
structured evidence.

This is the mission that would close the biggest gap between
what the stack's evidence discipline **claims** to produce and
what it actually produces in ordinary use. Without the mesh, PSRs
and PURs happen when the agent remembers; with the mesh, they
happen as a boundary condition of entering and exiting an action.

Registry: `:active`. Candidate invariant
`interaction-evidence-continuity` (leaf-invariants' candidate
cluster) is what this mission closes if it lands.

[← IRC Stability](irc-stability) · [Transport Adapters →](transport-adapters)

---

### Transport Adapters | transport-adapters

`transport-adapters@futon3c` is the mission for the family of
small bridge components that translate between transport
protocols — IRC ↔ WebSocket, WS ↔ HTTP, agent-native ↔ human-
readable. Each adapter is small; the family is what makes the
stack's invariant I-2 ("transport routes, it does not create")
operational.

The mission is about keeping this family **consistent** so an
adapter's behaviour under error, reconnect, and backpressure is
not surprising. Adapters that silently mutate the message
invalidate the evidence chain; adapters that fail loudly are fine.

Registry: `:active`. Related to IRC-stability (same transport
family) and to PSR/PUR mesh (same evidence chain).

[← PSR/PUR Mesh](psr-pur-mesh-peripheral) · [WS Emacs Log Stream →](ws-emacs-log-stream)

---

### WS Emacs Log Stream | ws-emacs-log-stream

`ws-emacs-log-stream@futon3` is the specific transport-layer
mission that gets agent log output into Emacs in real time over
WebSocket. It is the mission that makes the REPL-wins-over-CLI
ambition (M-repl-wins-over-cli) actually visible to the operator —
without a live log stream, the REPL is mute about what the agent
is doing, and the CLI re-asserts as the surface-of-last-resort.

Registry: `:active`. Closure is operator-confirmable when the
log stream is reliably visible in the posframe or side buffer of
Joe's choice without needing manual refresh.

[← Transport Adapters](transport-adapters) · [M1 Smoke Test →](m1-smoke-test)

---

### M1 Smoke Test | m1-smoke-test

`M1-smoke-test@futon3` is the mission that exists to **prove the
whole transport-plus-evidence stack works end-to-end** with one
concrete run: agent speaks over IRC, transport adapter forwards,
PSR/PUR records, gate pipeline validates, evidence lands in
futon1a. M1 is a convention that this cluster's work is not
complete until an end-to-end smoke test passes.

The mission is small in scope and load-bearing in practice: a
single reliable smoke test discriminates "four missions with
partial closure" from "four missions with joint closure."

Registry: `:active`. Articulation: explicit in scope; passing
state is per-run rather than a registry flag. Close relative of
M-futon3x-e2e (in leaf-6-2).

[← WS Emacs Log Stream](ws-emacs-log-stream)

---

*Size-5 leaf. The transport-plus-evidence cluster is the stack's
signalling nervous system. Many other missions depend on these
five landing together; few of those downstream missions cluster
here because they occupy higher layers of the stack.*

---

## Reading the Plan

*Source: `~/code/futon5a/holes/stories/leaf-6-0.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-0` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Two missions cluster here, both in `futon3`, both about **making
plans legible**. Plans are artefacts the stack uses to coordinate
work across multiple steps; if the plan is unreadable, the
coordination silently decays. The two missions are the native and
the visual sides of the same concern.

[Native Plan Coherence](native-plan-coherence)  
[Plan Mermaid Viewer](plan-mermaid-viewer)

---

### Native Plan Coherence | native-plan-coherence

`native-plan-coherence@futon3` is the mission to keep the stack's
native plan representation — EDN plans — **internally coherent**:
references resolve, dependencies are satisfied, the plan parses
cleanly, and the plan's structure matches the operational contract
the plan system carries.

This is a small mission doing quiet work. A coherent plan is one
an operator (human or agent) can execute without guessing what
was meant. An incoherent plan — missing fields, dangling
references, ambiguous ordering — is worse than no plan: it looks
actionable but produces confusion.

Registry: `:active`. Close relative of the structural-law
`existence` and `required-outputs` families (leaf-invariants);
this mission is what keeps plan-scale artefacts on the right side
of those invariants.

[← Overview](overview) · [Plan Mermaid Viewer →](plan-mermaid-viewer)

---

### Plan Mermaid Viewer | plan-mermaid-viewer

`plan-mermaid-viewer@futon3` is the mission to render plans
**visually** via Mermaid diagrams. An EDN plan is machine-readable
and operator-hostile; a Mermaid diagram is operator-friendly and
machine-indifferent. The mission produces both representations
from the same source so the operator and the executor read the
same plan, just differently.

This is what makes the "reading the plan" loop close: the
operator inspects the mermaid diagram, the executor consumes the
EDN, and a mismatch between what the diagram shows and what the
EDN contains would be a serious bug. The mission's quiet
contribution is that it prevents two-source-of-truth drift.

Registry: `:active`. Mission-doc articulation: focused; the
deliverable is a viewer that accepts EDN plans and emits mermaid.

[← Native Plan Coherence](native-plan-coherence)

---

*Size-2 leaf. The plan cluster is small because plan machinery is
structurally separate from pattern, mission, and evidence
machinery — it is its own thin seam. If plan-consuming missions
proliferate (workplan, multi-step execution), the cluster grows.*

---

## A Game at the Edge

*Source: `~/code/futon5a/holes/stories/leaf-6-1.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-1` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

A single mission, alone in its cluster: the stack's probe at a
game-shaped benchmark (ALFWorld) meant to exercise the pattern
library's flexiargs against an environment that can be played,
won, and lost. Its singleton status means nothing in the other 109
missions clusters with it — game-benchmark territory is its own
continent.

[ALFWorld Pattern Discovery](alfworld-pattern-discovery)

---

### ALFWorld Pattern Discovery | alfworld-pattern-discovery

`alfworld-pattern-discovery@futon3c` is a mission to use ALFWorld
— a text-based household-task simulator — as a probe for the
stack's pattern library. The hypothesis: a library of 853+
flexiargs should either visibly help an agent navigate ALFWorld
tasks better, or the flexiargs are not as general as the library
claims.

The mission belongs to futon3c because the pattern-selection
machinery (PSR/PUR, Portfolio Inference) lives there. ALFWorld
gives the stack a rare thing: a benchmark where "win" and "lose"
are unambiguous. Most of the stack's evidence is from
developer-workflow territory where success and failure are
negotiable. A game is not negotiable — you either complete the
task or you don't.

Current articulation is thin. The mission has a name and an
intent; whether the probe has actually run against the library and
produced a per-flexiarg win/lose record is not clear from the
mission doc alone. Like several singleton missions, this one is
on the shelf waiting for a focused push.

If the push lands, ALFWorld results become one of the cleaner
falsifiability surfaces for S1 (Evidence Discipline Works) — a
specific flexiarg, selected by PSR, either helps the agent win
more games or it does not.

[← Overview](overview)

---

*Singleton leaf. If related game-benchmark probes arrive
(MiniGrid, TextWorld, Crafter), this cluster grows into a
"benchmark probes" region.*

---

## The Last Mile and the Daily Scan

*Source: `~/code/futon5a/holes/stories/leaf-6-2.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-2` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Thirteen missions cluster here — the anthology's biggest cluster.
The distinctive words (brief, scan, probe, data, demo, lead-report)
tell you the cluster's centre: this is where the stack's
**operational delivery** happens. End-to-end runs, workplans,
daily scans, lead reports, coordination diagrams — the missions
that produce visible output on a regular cadence.

[Futon3 Last Mile](futon3-last-mile)  
[Futon3x E2E](futon3x-e2e)  
[Diagram Composition](diagram-composition)  
[Futon1a Rebuild Scoping Review](futon1a-rebuild-scoping-review)  
[Futon1a Workplan](futon1a-workplan)  
[Futon1a Rebuild (futon5)](futon1a-rebuild-futon5)  
[QUEUE-FORMAT](queue-format)  
[Futon3 Coordination](futon3-coordination)  
[Futon3 Agent Loop](futon3-agent-loop)  
[Futon2 AIF Ants](futon2-aif-ants)  
[Self-Improvement Loop](self-improvement-loop)  
[F7 Lead Report](f7-lead-report)  
[Daily Scan](daily-scan)

---

### Futon3 Last Mile | futon3-last-mile

`futon3-last-mile@futon3c` is the mission for the **last mile** —
the specific gap between "the coordination layer works in
principle" and "an operator can sit down, run a full cycle, and
watch it complete without intervention." Last-mile work is
unglamorous: error messages, default behaviours, off-ramp
conventions, the third-time-the-user-hits-enter experience.

Registry: `:active`. Load-bearing for demonstrability — without
last-mile polish, the stack reads as a prototype; with it, the
stack reads as a system.

[← Overview](overview) · [Futon3x E2E →](futon3x-e2e)

---

### Futon3x E2E | futon3x-e2e

`futon3x-e2e@futon3` is the mission for the **end-to-end
demonstrator** — one executable run that exercises the full gate
pipeline (G5 → G0), evidence recording, mission state transition,
and visible operator-side output. M-futon3x-e2e is cited across
the holistic argument as evidence for S1 (Evidence Discipline
Works); its closure is one of the argument's load-bearing
pieces.

Registry: `:active`. Closure is per-run — each successful run
adds evidence weight. The mission doc records the run protocol.

[← Futon3 Last Mile](futon3-last-mile) · [Diagram Composition →](diagram-composition)

---

### Diagram Composition | diagram-composition

`diagram-composition@futon5` is the mission for **composing
wiring diagrams** — the EDN-backed component-and-relation
descriptions futon5 holds as the stack's architectural snapshot.
Compositional diagrams are what let a reader see how subsystems
connect without reading code; without them, the stack's
architecture lives in heads.

This mission is where the futon5 wiring contract — claims,
evidence, mission mappings — gets **rendered** as mermaid or
SVG for human consumption. The contract lives in EDN; the
diagram is the projection.

Registry: `:active`. Articulation: diagram tooling exists and
is in ongoing refinement.

[← Futon3x E2E](futon3x-e2e) · [Futon1a Rebuild Scoping Review →](futon1a-rebuild-scoping-review)

---

### Futon1a Rebuild Scoping Review | futon1a-rebuild-scoping-review

`futon1a-rebuild-scoping-review@futon3` is the meta-mission for
the futon1a rebuild: **what was in scope, what was out, and what
the rebuild actually accomplished**. Scoping reviews are
particularly valuable for rebuild missions because rebuilds
tend to expand — each unexpected issue adds a little more to the
scope, and the mission ends with a larger footprint than it
started.

This review is also what validates (or challenges) the rebuild's
closure status. If the review says "what was delivered matches
what was scoped," the rebuild is genuinely complete; if not,
residue remains.

Registry: `:active`. Closure relates to futon1a-rebuild's
articulation-level closure.

[← Diagram Composition](diagram-composition) · [Futon1a Workplan →](futon1a-workplan)

---

### Futon1a Workplan | futon1a-workplan

`futon1a-workplan@futon3` is the mission for **the workplan that
drove the futon1a rebuild**: an explicit ordering of rebuild
steps with deadlines, dependencies, and progress markers. Not a
plan-as-diagram (that is diagram-composition) — a plan-as-
schedule.

The workplan is the artefact that lets a reader tell where the
rebuild is **in time**. It is also the artefact against which the
rebuild-scoping-review evaluates closure. Together they form the
plan/plan-review pair that the futon1a-rebuild mission sits
between.

Registry: `:active` (articulation may be mostly complete as
the rebuild is substantially delivered). *(Registry: `:active`;
articulation: workplan largely executed; residue is mostly
doc-polish.)*

[← Futon1a Rebuild Scoping Review](futon1a-rebuild-scoping-review) · [Futon1a Rebuild (futon5) →](futon1a-rebuild-futon5)

---

### Futon1a Rebuild (futon5 copy) | futon1a-rebuild-futon5

`futon1a-rebuild@futon5` is the **wiring-side** copy of the
rebuild mission — where leaf-6-3's `futon1a-rebuild@futon3` is
the execution side, this copy describes the rebuild as a
structural change in the stack's architecture. The two copies
serve different audiences: the execution copy is for implementers;
this copy is for architects and the wiring contract.

The mission's existence in futon5 is diagnostic: futon5's job is
to hold the wiring-level commitments that implementations like
futon1a satisfy. A rebuild that changes the wiring contract gets
a mission in futon5; a rebuild that only changes implementation
does not.

Registry: `:active`. Much of the wiring-level change is
complete; residue is documentation alignment.

[← Futon1a Workplan](futon1a-workplan) · [QUEUE-FORMAT →](queue-format)

---

### QUEUE-FORMAT | queue-format

`QUEUE-FORMAT@futon3` is a small but load-bearing mission: the
**queue format contract** that the stack's scheduling subsystems
use to describe pending work. Without a canonical format, every
consumer invents its own, and the evidence trail around queued
work gets scattered across incompatible shapes.

The mission's job is to make the format **explicit** and to
migrate existing consumers. It is not exciting work. It is
structural-law candidate material (repo-role-clarity /
artefact-custody territory, from leaf-invariants).

Registry: `:active`. The mission closes when the format is
defined and the known consumers have adopted it.

[← Futon1a Rebuild (futon5)](futon1a-rebuild-futon5) · [Futon3 Coordination →](futon3-coordination)

---

### Futon3 Coordination | futon3-coordination

`futon3-coordination@futon5` is the wiring-side mission for the
**coordination pipeline** — the concrete sequence of G5 → G0
gates plus the evidence flows that tie them together. This is
the mission that sits in `/home/joe/code/futon5/data/missions/futon3-coordination.edn`
and holds the ground-truth topology for what futon3c is rebuilding.

Without this mission, the coordination-rewrite pair (leaf-6-5-0)
has nowhere to anchor its contract. With it, the rewrite has a
specific shape to satisfy.

Registry: `:active`. Load-bearing for architectural legibility.

[← QUEUE-FORMAT](queue-format) · [Futon3 Agent Loop →](futon3-agent-loop)

---

### Futon3 Agent Loop | futon3-agent-loop

`futon3-agent-loop@futon5` is the wiring-side mission for the
**agent loop** — the fast AIF cycle that runs inside an
agent's session (observation → inference → action → evidence).
Where futon3-coordination is about the glacial/task loop of G5 →
G0, this mission is about the fast social loop that happens
inside a peripheral during one agent turn.

Two loops, two timescales, the same AIF vocabulary applied at
different scales. This mission is what keeps them commensurable
in the wiring diagram.

Registry: `:active`. Articulation: nested-AIF framing is central
to the stack's self-representation and appears across
peripheral, mission, and portfolio scales.

[← Futon3 Coordination](futon3-coordination) · [Futon2 AIF Ants →](futon2-aif-ants)

---

### Futon2 AIF Ants | futon2-aif-ants

`futon2-aif-ants@futon5` is the wiring-side mission for futon2's
**ant-agent** simulation — 200-tick grid runs where simple ant
agents implement AIF principles (prediction, preference, action
selection under free energy). The ants are the stack's first
demonstrator of AIF machinery at scale, and this mission
documents the ants' structural role in the stack.

Why ants in a developer-tool stack? Because ants show AIF
principles running on a cheap, inspectable substrate. If the
principles work for ants, the case for using them at higher
scales (portfolio, mission-peripheral, futonzero) is stronger.
S2 (AIF Framing Is Generative) rests in part on this
demonstrator.

Registry: `:active`. Articulation: simulation works; the
wiring-side description keeps it commensurable with higher
scales.

[← Futon3 Agent Loop](futon3-agent-loop) · [Self-Improvement Loop →](self-improvement-loop)

---

### Self-Improvement Loop | self-improvement-loop

`self-improvement-loop@futon5a` is the mission for the
**meta-loop** — the stack improving its own patterns, its own
evidence discipline, its own process — not by the operator's
direct edits, but by the stack noticing where it does badly and
adjusting. This is where the learning-loop pattern (from
superpod mark2, ported through trip-journal, generalised into
M-learning-loop) lands as an explicit stack-wide mission.

Registry: `:active`. The mission is ambitious and its closure
criteria are deliberately under-specified; what matters is that
the stack has a named place where self-improvement work is
tracked rather than scattered.

[← Futon2 AIF Ants](futon2-aif-ants) · [F7 Lead Report →](f7-lead-report)

---

### F7 Lead Report | f7-lead-report

`f7-lead-report@futon7` is the mission for the **landscape-
intelligence lead report** — the concrete output f7 produces for
a specific audience target (person, org, or domain): a structured
brief of who's doing what, where the relevant papers are, which
organisations are adjacent, what the next probe should be. Lead
reports are the stack's only revenue-adjacent artefact.

This is S4 territory (**Commercial Demand Exists Adjacent**).
Each lead report is an instance of landscape intelligence
rendered for a paying audience. The mission's articulation is
operational — it knows its format and output shape — and the
value-per-report depends on audience-first-probes discipline.

Registry: `:active`. Close relative to daily-scan (next scene);
together they constitute f7's operational cadence.

[← Self-Improvement Loop](self-improvement-loop) · [Daily Scan →](daily-scan)

---

### Daily Scan | daily-scan

`daily-scan@futon7` is the mission for the **daily networked-
intelligence scan** — the standing cadence that watches a curated
set of sources for signals relevant to active audience targets.
The scan is daily because fresh signal is the point; a weekly
scan decays too much signal; an hourly scan costs more attention
than the signal justifies.

The scan's output is not the lead report (that is the f7-lead-
report mission) — the scan's output is structured observations
that feed into the networked-intelligence hypergraph. Lead
reports are compositions of scan observations over time.

Registry: `:active`. The hinge-log format that M-daily-scan
uses (and that fed into the Shape A bridge exercise earlier in
M-stack-geometry) is the scan's evidence shape.

[← F7 Lead Report](f7-lead-report)

---

*Size-13 leaf — the anthology's largest. The cluster sprawls
deliberately: end-to-end demonstrators, rebuild-adjacent
workplans, wiring-level missions, and f7's operational cadence
all belong here because they share the vocabulary of operational
delivery. A future reorganisation could split the cluster into
(1) rebuild-adjacent coordination, (2) wiring-level ground-truth,
(3) f7 operational output. For now, reading the cluster as "the
stack's regular cadence of visible output" is the coherent
summary.*

---

## Two Agents at the Rebuild

*Source: `~/code/futon5a/holes/stories/leaf-6-3.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-3` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Seven missions cluster here on a single theme: **Codex and Claude
coexisting on the same stack, doing the same work, under the same
discipline**. The cluster's distinctive words — codex, openai,
claude-opus, parity, peripheral — say it all. This is where the
stack asks whether two different agent families can operate as
peers rather than as competitors.

[Futon3c × Codex](futon3c-codex)  
[Codex Parity](codex-parity)  
[Fucodex Parity](fucodex-parity)  
[Understand Fucodex](understand-fucodex)  
[Futon1a Rebuild — Evidence](futon1a-evidence)  
[Futon1a Rebuild](futon1a-rebuild)  
[Mission Control Scoping](mission-control-scoping)

---

### Futon3c × Codex | futon3c-codex

`futon3c-codex@futon3c` is the mission to get Codex operating
smoothly inside futon3c's real-time coordination layer. The
invariant I-1 (Agent Identity Is Singular) says an agent is one
agent: if Codex is running in a session, IRC messages route to
that session rather than spawning a fresh `codex -p` invocation.
This mission is what makes that invariant hold for Codex
specifically.

Registry: `:active`. The mission's work is peripheral-integration
plus surface-contract plumbing.

[← Overview](overview) · [Codex Parity →](codex-parity)

---

### Codex Parity | codex-parity

`codex-parity@futon3` is the mission that says **what Claude can
do in a peripheral, Codex should also be able to do**, and vice
versa. Parity here is not identical behaviour — the agents have
different strengths — but identical *interface*: the same tools,
the same peripheral vocabulary, the same surface contracts.

Without parity, peripherals drift toward one agent's affordances
and the other agent becomes a second-class citizen. Parity is
what keeps the stack agent-agnostic in the sense that matters.

Registry: `:active`. Closure is measurable: every peripheral's
behaviour is the same under both agents, or the exceptions are
documented and explicit.

[← Futon3c × Codex](futon3c-codex) · [Fucodex Parity →](fucodex-parity)

---

### Fucodex Parity | fucodex-parity

`fucodex-parity@futon3` is the specific parity mission for
**fucodex** — the Codex peripheral wrapper that was built to
sit alongside `fuclaude`. The mission asks whether these two
wrappers actually behave the same from the stack's point of view,
and where they differ, whether the difference is intentional or
legacy.

This is a subtler mission than raw codex-parity because it is
about the wrapper's behaviour under the full range of stack uses
(PAR sessions, forum participation, multi-agent routing) rather
than about the agent's native capability.

Registry: `:active`. The mission closes when fucodex's
observable behaviour matches fuclaude's or the delta is named
and accepted.

[← Codex Parity](codex-parity) · [Understand Fucodex →](understand-fucodex)

---

### Understand Fucodex | understand-fucodex

`understand-fucodex@futon3` is the ancestor of the parity mission:
before fucodex could achieve parity, someone had to **understand**
what fucodex currently does. This mission is the archaeology that
makes parity work possible.

Reading the existing fucodex wrapper's source, tracing its
surface behaviour, recording its quirks — that is the mission's
work. Not exciting. Necessary. Most closure here is silent: once
fucodex is understood, the mission can close and the parity
mission becomes actionable.

Registry: `:active`. Articulation: thin by necessity; the
mission is its own documentation once it closes.

[← Fucodex Parity](fucodex-parity) · [Futon1a Rebuild — Evidence →](futon1a-evidence)

---

### Futon1a Rebuild — Evidence | futon1a-evidence

`futon1a-evidence@futon3` is the mission for making **futon1a's
evidence surface** what the stack's agents actually need. This is
the evidence-side contract: what does an agent write to futon1a,
what does it read back, and how do those operations produce
queryable evidence.

The rebuild context matters: futon1a is being rebuilt from
scratch (see the rebuild mission next), and this mission is
defining the evidence shape the new build will satisfy.

Registry: `:active`. The mission's articulation is tied to the
alpha-evidence API (leaf-6-4-0).

[← Understand Fucodex](understand-fucodex) · [Futon1a Rebuild →](futon1a-rebuild)

---

### Futon1a Rebuild | futon1a-rebuild

`futon1a-rebuild@futon3` is the mission for **rebuilding futon1a**
as the stack's durable substrate — the XTDB-backed, layered-error,
penholder-gated store described in the structural-law inventory
(leaf-invariants, system/auth family). The rebuild is what made
futon1a's 9 operational invariants operational.

This mission is in a sense **already closed**: futon1a exists, is
running on port 7071, holds ~1500+ entities including all 110
missions, all 293 sorrys, and all their relations. The mission's
registry status may still read `:active` if articulation-side
work (documentation, polish, test coverage) remains.
*(Registry-vs-articulation distinction: work complete in
implementation; mission-doc narrative may trail.)*

Registry: `:active`. Articulation: the rebuild's major structural
moves are complete; remaining work is incremental.

[← Futon1a Evidence](futon1a-evidence) · [Mission Control Scoping →](mission-control-scoping)

---

### Mission Control Scoping | mission-control-scoping

`mission-control-scoping@futon3` is the scoping mission for
Mission Control — the peripheral that reports on all mission
states across the stack. Because Mission Control reads from
futon1a and is itself central to how the stack self-represents,
its scope is load-bearing: too narrow and the peripheral underreports;
too broad and it tries to be the War Machine (leaf-6-4-5) and
fails.

The scoping mission's work is drawing that boundary clearly.
Related to the "being done" cluster (leaf-6-5-3) — this is
another scoping review mission.

Registry: `:active`. The mission closes when Mission Control's
scope is settled and the peripheral's behaviour matches.

[← Futon1a Rebuild](futon1a-rebuild)

---

*Size-7 leaf. The cluster is the stack's multi-agent parity
cohort plus the futon1a rebuild — these cohered because Codex
and the rebuild happened concurrently and the missions share
vocabulary around invocation, peripheral, and evidence. A future
split would separate agent-parity from durable-store rebuild.*

---

## Evidence Over HTTP

*Source: `~/code/futon5a/holes/stories/leaf-6-4-0.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-4-0` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Three missions cluster here on a very specific piece of
infrastructure: **the HTTP surface that lets evidence be queried
from localhost**. Distinctive words are all about `curl`,
`http localhost`, `alpha evidence`. This is the cluster where the
stack's evidence landscape stops being a collection of files and
starts being a queryable service.

[Forum Refactor](forum-refactor)  
[Operational Readiness Traceability](operational-readiness-traceability)  
[Evidence Viewer Refinements](evidence-viewer-refinements)

---

### Forum Refactor | forum-refactor

`forum-refactor@futon3c` is the mission to clean up the Forum
subsystem (collaborative proof trees, threads, posts) so that its
HTTP surface is consistent with the rest of the alpha-evidence
API. Forum originally had its own conventions; aligning them to
the broader evidence API means one consistent way to query any
stack artefact over HTTP.

Registry: `:active`. Articulation: scoped cleanup rather than
redesign. The mission lands when Forum data is queryable through
`/api/alpha/` endpoints in the same shape as other entity types.

[← Overview](overview) · [Operational Readiness →](operational-readiness-traceability)

---

### Operational Readiness Traceability | operational-readiness-traceability

`operational-readiness-traceability@futon3c` is the mission to
make the stack's operational-readiness state **queryable** — what
is running, what is healthy, what has degraded — rather than only
inspectable by running specific tools. The HTTP surface is the
delivery mechanism; the mission's actual work is deciding what
operational-readiness *is* in terms the HTTP surface can return.

This is S1 territory (**Evidence Discipline Works**) at the
infrastructure layer: operational readiness as evidence rather
than folklore. Readable-surface (from candidate invariant
`human-visible-inspectability`) is the structural law being
satisfied.

Registry: `:active`. Mission is doing the quiet-but-essential
work of making the stack's own runtime state into queryable
evidence.

[← Forum Refactor](forum-refactor) · [Evidence Viewer Refinements →](evidence-viewer-refinements)

---

### Evidence Viewer Refinements | evidence-viewer-refinements

`evidence-viewer-refinements@futon4` is the mission to keep
improving the evidence viewer — the web UI that reads the alpha-
evidence API and renders it for a human operator. This is where
the HTTP-queryable evidence lands in front of a reader's eyes;
refinements are the slow accretion of usability improvements
(filters, search, sensible defaults) that make the viewer a
primary surface rather than a fallback.

The mission is the long-tail companion to the other two in this
cluster: they produce the evidence; this one makes it readable.
Together the three complete a thin vertical slice — from raw
artefact to viewer — that the stack's self-representation
ambition depends on.

Registry: `:active`. The mission has no natural close — viewer
improvement is continuous — so its health is measured by cadence
of refinement landing rather than a terminal state.

[← Operational Readiness](operational-readiness-traceability)

---

*Size-3 leaf. The HTTP-surface-plus-viewer cluster is a thin
vertical: alpha-evidence API on the backend, viewer on the
frontend, both refined incrementally. Expected to grow if more
stack subsystems get /alpha/ endpoints.*

---

## Reading the Papers

*Source: `~/code/futon5a/holes/stories/leaf-6-4-1.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-4-1` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Three missions cluster here on **retrieval-heavy work over
research papers**: the pattern-inference engine (futon3), the
reverse-morphogenesis mission (futon6), and superpod-mark2
(futon6). Their shared vocabulary — BGE, arxiv, papers —
tells you the cluster is about getting academic literature into
a state where the stack can use it.

[Pattern Inference Engine](pattern-inference-engine)  
[Paper Reverse Morphogenesis](paper-reverse-morphogenesis)  
[Superpod Mark2](superpod-mark2)

---

### Pattern Inference Engine | pattern-inference-engine

`pattern-inference-engine@futon3` is the mission for the engine
that **reads candidate patterns out of evidence**. Not the L1
canonicalizer (that already exists in futon3b's gate pipeline) —
this is the earlier step, where a pattern might be latent in a
body of evidence (papers, transcripts, archives) and the engine's
job is to surface it as a candidate.

The retrieval discipline matters here because the engine needs to
search the paper corpus for candidate patterns, not for matching
strings. BGE embeddings (per the standing preference, not R-GCN)
are the retrieval substrate.

Registry: `:active`. Articulation: the engine is a named
subsystem; specific candidate-pattern protocols are partially
specified.

[← Overview](overview) · [Paper Reverse Morphogenesis →](paper-reverse-morphogenesis)

---

### Paper Reverse Morphogenesis | paper-reverse-morphogenesis

`paper-reverse-morphogenesis@futon6` is the mission with the
best-named hypothesis in this cluster: given a paper, can the
stack **reverse** the paper back to the pattern(s) that generated
it? If yes, the paper was a composition of known moves; the
pattern set becomes the reading vocabulary. If no, the paper is
genuinely outside the stack's pattern repertoire and is either a
new-pattern candidate or outside the stack's reach.

This is retrieval-hard: the reverse mapping is not a simple
search, it is a decomposition. The mission shares vocabulary with
the pattern-inference engine because they are two halves of one
loop — inference reads candidate patterns out, reverse-morphogenesis
tests whether a paper is decomposable into patterns already held.

Registry: `:active`. Mission is a probe of whether the pattern
library is **generative** or merely collected.

[← Pattern Inference Engine](pattern-inference-engine) · [Superpod Mark2 →](superpod-mark2)

---

### Superpod Mark2 | superpod-mark2

`superpod-mark2@futon6` is the second iteration of the superpod
work (mark1 being the original ingest-and-embed pipeline in
leaf-3). Mark2's concern is the **learning loop** — how do we
improve embedding quality, retrieval quality, and pattern-
candidate quality as more papers flow through?

This mission is the source of the four-update-channel / two-
temporal-scale learning-loop pattern that M-trip-journal and
M-learning-loop both inherited. It is, in practice, the stack's
prototype for "how does a learning system improve from its own
evidence" — and the reason that pattern keeps showing up across
operator-capacity missions is that mark2 worked well enough to be
worth porting.

Registry: `:active`. Articulation: detailed in its operational
form; the learning-loop pattern has been abstracted out and is
now being re-used elsewhere.

[← Paper Reverse Morphogenesis](paper-reverse-morphogenesis)

---

---

### Mark2 in Flight 2026-05-20 | mark2-in-flight-2026-05-20

*(in-flight scoop, 2026-05-20; updates the abstract "learning loop" framing
in the Mark2 scene with concrete current state)*

The learning loop the Mark2 scene describes has crystallised into a concrete
methodology: **Daisychain + Distributed Proofreaders**. `build-uncovered-sentence-audit.py`
(futon6/scripts/) picks fresh papers from `daisychain-ledger.json` on each
pass, measures discourse coverage, and emits only the residual uncovered
sentences for manual review. Each pass advances the ledger so the next run
shifts to new papers — growing a reusable structure-pattern seed rather than
overfitting the detector on a few hand-picked texts.

**codex-3's audit-driven structure pass landed real lift today.** Same-paper
rerun on the original audit batch showed five of six papers improve:
`0711.4904v1` discourse coverage 0.7460 → 0.7831 (24 → 18 uncovered sentences);
`0711.1887v1` 0.6823 → 0.7188 (27 → 22); `0801.4067v1` 0.8258 → 0.8356;
`0711.0687v1` 0.9666 → 0.9700; `0803.0339v1` 0.9613 → 0.9654. One paper
(`0708.2394v1`) was already at 0.9822 and stayed there. The new general-lift
patterns landed in `scripts/nlab-wiring.py` (line 87): command-style bindings
(`Let \Digraph be ...`), `Here $...$ denotes ...`, short arrow aliases
(`\ra`, `\la`, `\lra`), discourse-step phrases (`Next`, `Finally`, `In light
of ...`), and question-label markers.

**Phase boundary reached.** The fresh third daisychain batch —
`0712.0418v1` (math.CT, 0.7895), `0801.0350v1` (math.LO/cs.CC, 0.9164),
`0711.0334v1` (math.HO, German, 0.4230) — produced a useful boundary case:
the German paper's low coverage is encoding noise, not a missing-pattern
signal. codex-3's read: **the structure seed is now good enough to stop
polishing by hand and start building the structure-learning analogue of
Stage 5 NER.** Joe agreed in the REPL: one more iteration combining
Daisychain + Distributed Proofreaders, then declare the seed complete and
pivot.

This crystallises Mark2's "learning loop" framing into a specific transition
on the horizon: hand-curated → learned. The structure-learning-NER analogue
is the next move the leaf will need to document when that lands. For now:
85 tests passing; artifacts at
`data/showcases/distributed-proofreaders/{latest,next,third}-{audit,rerun}.{json,html}`;
README anchor at `~/code/futon6/README-superpod.md §5a "Distributed
Proofreaders loop"`.

*(scene appended by `:story-update` writer-action — first live cycle of
M-vsatarcs-writer Option B per Joe 2026-05-20; admissibility-predicate
safety per claude-4 v0.5.22 generalisation)*


*Size-3 leaf. The cluster is where the stack's retrieval
discipline meets academic literature. Sibling of leaf-3 (math
evidence pipeline) but further up the stack — leaf-3 handles
ingest; this cluster handles reading.*

---

## Proof and Problem

*Source: `~/code/futon5a/holes/stories/leaf-6-4-2.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-4-2` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

If the [Portfolio and Sorry](portfolio-and-sorry) anthology is about how
the stack thinks about itself, and [Inhabitable Surfaces](inhabitable-
surfaces) is about where people meet inside it, this anthology is about
*what the stack does when it tries to do mathematics*. Nine missions sit
in this cluster. Together they are the stack's attempt to make the
*production* of proofs a first-class activity — one with phases, gates,
evidence, reviewers, and decision-traces, just as the rest of the stack
has for code.

The missions decompose into four groupings that you will recognise as you
read. First there is the *infrastructure* — the general mission peripheral
and the more specific proof peripheral, which define the machinery of a
proof session: nine phases, fifteen tools, a ledger, a DAG of
obligations, a gate-checklist from G5 through G0. Then there is the
*calibration* — stepper-calibration and V-f6-thesis-verification, which
both ask "does the infrastructure catch the mistakes it is supposed to
catch?" Then there are the *applications* — diagramprover, which searches
proof-space with pattern discipline, and three rational-reconstruction
missions (P3, P7, P8) that replay existing proofs with decision traces
to see if pattern discipline would have caught the gaps reviewers found.
And finally there is the *stress test* — distributed-frontiermath, which
throws the whole apparatus at a genuinely open problem and asks whether
it holds together under pressure.

As a reader, the arc runs from definition to test to application to
stress test — a natural mathematical discipline running in miniature on
the stack's own proof work.

[Mission Peripheral](mission-peripheral) — the general cycle infrastructure  
[Proof Peripheral](proof-peripheral) — FrontierMath-specific protocol  
[DiagramProver](diagramprover) — pattern-driven proof search  
[Stepper Calibration](stepper-calibration) — does the stepper catch errors?  
[V-f6 Thesis Verification](v-f6-thesis-verification) — futon6 against the 2014 PhD thesis  
[P3 Rational Reconstruction](p3-rational-reconstruction) — linear proof replay  
[P7 Rational Reconstruction](p7-rational-reconstruction) — branching proof replay  
[P8 Rational Reconstruction](p8-rational-reconstruction) — linear-with-kernel proof replay  
[Distributed FrontierMath](distributed-frontiermath) — the stress test

---

### Mission Peripheral | mission-peripheral

The mission peripheral is the general-purpose cycle machinery. The proof
peripheral (commit `7d1a5d0`, fifteen tools, nine-phase cycle) works for
mathematical proof development. But the same cycle structure applies to
*code development missions*: observe the codebase, propose an approach,
execute the change, validate with tests, classify the outcome, integrate
into the evidence landscape, commit, and gate-review. The mission
peripheral is the recognition of that isomorphism and the apparatus that
exploits it.

The point is enforceability. Code missions today are tracked in markdown
files (`holes/missions/M-*.md`), coordinated via the war room, and
executed with ad hoc PSR/PUR discipline. This works until it doesn't;
what the peripheral adds is *structural* enforcement — phase-gated
tools, mandatory outputs per phase, DAG-tracked obligations, automatic
evidence emission. Evidence when coding is not just "unit tests pass":
it includes design decisions (why protocol-first?), corpus checks (what
did the pattern library say?), framing validations (is the decomposition
right?), and the reasoning context around each step.

This is the scene to read first because every other mission in this
anthology inherits its shape.

<!-- sorrys-for-scene:mission-peripheral:start -->

**Open sorrys in this scene.**

- ✓ Mission `mission-peripheral@futon3c`: `:complete` / `:settled` — *INSTANTIATE complete — derivation xenotype finished*
- ✓ Closes `sorry|devmap|futon3|P14|peripheral-runtime` (high, sim 0.54)

<!-- sorrys-for-scene:mission-peripheral:end -->

[← Overview](overview) · [Proof Peripheral →](proof-peripheral)

---

### Proof Peripheral | proof-peripheral

The proof peripheral is the first concrete instantiation of the
mission-peripheral pattern, specialised for mathematical work. The
infrastructure from Mission Peripheral — nine-phase cycle machine,
ledger plus DAG, canonical statement management, the G5–G0 gate
checklist — all of this exists and works. What the mission adds are the
*FrontierMath-specific* protocol pieces needed for live use against
open research problems:

1. Blackboard observability for proof sessions, so multiple agents or
   observers can watch the proof unfold.
2. A five-mode protocol — SPEC, FALSIFY, CONSTRUCT, VERIFY, MAP — that
   structures the proof attempt.
3. TryHarder licensing that gates persistence loops, preventing the
   "keep trying forever" failure mode.
4. Mandatory FALSIFY-before-CONSTRUCT enforcement, which closes one of
   the most common proof-search anti-patterns.
5. State snapshot evidence emission, so every intermediate state is
   recoverable.

The five modes are worth noting because they encode a discipline many
human mathematicians internalise slowly: before you try to construct,
try to break; before you verify, specify what you are verifying; and
when the construct fails, map where you are rather than thrashing.

<!-- sorrys-for-scene:proof-peripheral:start -->

**Open sorrys in this scene.**

- ◐ Mission `proof-peripheral@futon3c`: `:in-progress` / `:prototype` — *IN PROGRESS*
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:proof-peripheral:end -->

[← Mission Peripheral](mission-peripheral) · [DiagramProver →](diagramprover)

---

### DiagramProver | diagramprover

DiagramProver is the stack's answer — or at least its proposal — to
systems like Axiom's AxiomProver, which achieved a perfect 12/12 Putnam
2025 score and closed four previously open conjectures. Axiom's
architecture: natural-language problem statements parsed and translated
into Lean code with type-consistency checking, a custom transformer
exploring proof-space with an RL loop rewarding progress and penalising
dead ends, candidate proofs fed to Lean for formal verification, a data
flywheel feeding verified proofs back into training.

DiagramProver's claim is *complementary, not competitive*: pattern-driven
proof search. Rather than a pure RL signal, the search is guided by
structural patterns — the accumulated wisdom of many proofs,
expressed as flexiargs in the library, each with known applicability
conditions. A search step is a pattern application, not a token
generation. Success is not only "Lean accepted the proof" but also
"the patterns used are legible to a reviewer as the moves of the proof."

Whether this is faster, slower, or more reliable than Axiom's approach
is an open empirical question. What the mission commits to is the
architecture — and the anthology beyond this scene is the calibration
and application work that tests whether the architecture earns its
keep.

<!-- sorrys-for-scene:diagramprover:start -->

**Open sorrys in this scene.**

- ◐ Mission `diagramprover@futon3c`: `:in-progress` / `:prototype` — *VERIFY*
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:diagramprover:end -->

[← Proof Peripheral](proof-peripheral) · [Stepper Calibration →](stepper-calibration)

---

### Stepper Calibration | stepper-calibration

Stepper Calibration is where the infrastructure gets tested against
known ground truth. The proof stepper (the combination of the proof
peripheral, the `:corpus-check` tool, and the evidence-facet engine)
is designed to catch *framing errors* before they propagate into full
proofs. First Proof provides ten problems with known official answers.
This mission traces each problem through the stepper and asks five
diagnostic questions:

1. Would the stepper have caught the error (for the wrong answers, P1
   and P7)?
2. Would the stepper have prevented problem substitution (P3, where the
   problem-as-stated was quietly replaced)?
3. Would the stepper have surfaced the missing technique (P4 and P6)?
4. What does a successful stepper trace look like (P2, P8, P9, P10)?
5. What corpus data is needed to make each check effective?

The last question is doing double duty: it calibrates the stepper *and*
defines the requirements for the arXiv pipeline, so that when the
FrontierMath work scales up, the corpus data already matches what the
stepper needs. Calibration and specification in one mission.

<!-- sorrys-for-scene:stepper-calibration:start -->

**Open sorrys in this scene.**

- ◐ Mission `stepper-calibration@futon3c`: `:in-progress` / `:prototype` — *IN PROGRESS — P1 complete (RED), P7 complete (RED), P3 next*
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:stepper-calibration:end -->

[← DiagramProver](diagramprover) · [V-f6 Thesis Verification →](v-f6-thesis-verification)

---

### V-f6 Thesis Verification | v-f6-thesis-verification

V-f6 Thesis Verification is an unusual mission: it verifies the present
work of futon6 against the Future Work section of Joe's 2014 PhD thesis.
The thesis described what a computational study following the doctoral
work would need — a reasonably large computer-accessible body of
mathematical content, computational agents that navigate mathematical
structures, standard and social problem-solving heuristics, sufficient
metacognitive awareness to set and solve problems with design patterns,
and the ability to annotate, reflect, diagnose, and extend the process.

Twelve years later, futon6 exists. The mission decomposes each thesis
claim into a verifiable assertion and maps it to a futon6 component:
the math.SE corpus and NER kernel match the "computer-accessible body
of mathematical content"; the agent infrastructure matches
"computational agents"; the pattern library matches "design patterns";
the proof peripheral's reflection tools match "annotate, reflect,
diagnose, extend."

This scene matters because it makes explicit what is otherwise implicit
throughout the proof-and-problem family: the work has a long horizon,
the design is older than the code, and the verification of the whole
is as much against *Joe's original vision* as against any external
benchmark.

[← Stepper Calibration](stepper-calibration) · [P3 Rational Reconstruction →](p3-rational-reconstruction)

---

### P3 Rational Reconstruction | p3-rational-reconstruction

P3 is one of the first problems where the rational-reconstruction method
was applied. The problem asks for the existence of a continuous-time
Markov chain whose stationary distribution is a modified Macdonald
polynomial. A complete solution sketch already exists, along with a
reviewer assessment (two major findings, one medium), a Mermaid wiring
diagram, Codex repair cycles, and eight synthetic question-answer pairs
decomposing the proof into sub-questions. The mathematical content is
there.

What is missing — and what this mission produces — is the *decision
trace*. The original solution was developed ad hoc: steps were chosen,
decompositions made, notation bridges asserted. No record exists of
*why* those moves were chosen, what alternatives were considered, what
patterns (if any) governed the proof strategy. The reviewer found gaps
— a star/non-star bridge asserted but not proved, irreducibility too
compressed, positivity needing a citation — but nobody can trace those
gaps back to the decisions that produced them.

Rational reconstruction replays the proof with pattern discipline.
Virtual PSRs are written at each decision point, as if the proof had
been produced under the discipline from the start. The output: a
reconstructed proof that is *legibly patterned* and a diagnostic that
shows whether pattern discipline would have caught the reviewer's
findings the first time.

<!-- sorrys-for-scene:p3-rational-reconstruction:start -->

**Open sorrys in this scene.**

- ◐ Scene gap — Rational reconstruction generalisation across proof shapes  
   `sorry|scene|leaf-6-4-2|rational-reconstruction-cross-domain-generalisation`

<!-- sorrys-for-scene:p3-rational-reconstruction:end -->

[← V-f6 Thesis Verification](v-f6-thesis-verification) · [P7 Rational Reconstruction →](p7-rational-reconstruction)

---

### P7 Rational Reconstruction | p7-rational-reconstruction

P7 is the harder test case. Where P3 has a single linear proof path, P7
explores four distinct approaches — Wall surgery, equivariant surgery,
orbifold resolution, and the rotation-lattice route — encounters
blocking obstructions, and settles on a conditional answer via the
rotation-lattice construction. The reviewer identified four findings,
three critical.

This mission matters because P3's rational reconstruction validated the
method on the easy case: virtual PSR and PUR replay caught three of
three reviewer findings through pattern discipline. P7 tests features
P3 lacked. Multiple proof routes explored and abandoned — the *decision
to abandon* Approaches I through III and pursue IV is a strategic move
for which no existing pattern covers. A conditional or partial answer —
P3 had a yes; P7 has a qualified yes. The rational reconstruction has
to model not just the linear steps but the lateral motion across
approaches.

If P7 succeeds in the reconstruction sense — if pattern discipline would
have caught its reviewer findings and represented its strategic choices
legibly — then the pattern library is demonstrating *coverage across
proof shapes*, not just coverage of the easy linear case.

<!-- sorrys-for-scene:p7-rational-reconstruction:start -->

**Open sorrys in this scene.**

- ◐ Scene gap — Rational reconstruction generalisation across proof shapes  
   `sorry|scene|leaf-6-4-2|rational-reconstruction-cross-domain-generalisation`

<!-- sorrys-for-scene:p7-rational-reconstruction:end -->

[← P3 Rational Reconstruction](p3-rational-reconstruction) · [P8 Rational Reconstruction →](p8-rational-reconstruction)

---

### P8 Rational Reconstruction | p8-rational-reconstruction

P8 is a third shape of test. The problem asks whether a polyhedral
Lagrangian surface in four dimensions, with four faces per vertex,
admits a Lagrangian smoothing. The proof is *mostly* linear — setup,
local structure, key decomposition, consequences, smoothing, assembly
— but one step, the symplectic direct sum at step s3, is the
*structural kernel* of the whole argument. Get that step right and the
rest follows; get it wrong and the proof collapses.

The solution has incorporated repairs from the reviewer — a vertex
spanning lemma, edge crease smoothing, a global Hamiltonian argument —
but the rational reconstruction works from the *original* decision
trace, asking whether pattern discipline would have surfaced those
repairs from the start. This tests the library against symplectic
geometry specifically, a domain relatively underrepresented in the
corpus, and with a linear-with-kernel proof architecture distinct from
P3's pure linearity and P7's branching.

Three shapes tested is weak generalisation evidence but non-zero. If
the library handles all three — linear (P3), branching (P7),
linear-with-kernel (P8) — then the claim that patterns generalise
across proof shapes has its first empirical support.

<!-- sorrys-for-scene:p8-rational-reconstruction:start -->

**Open sorrys in this scene.**

- ◐ Scene gap — Rational reconstruction generalisation across proof shapes  
   `sorry|scene|leaf-6-4-2|rational-reconstruction-cross-domain-generalisation`

<!-- sorrys-for-scene:p8-rational-reconstruction:end -->

[← P7 Rational Reconstruction](p7-rational-reconstruction) · [Distributed FrontierMath →](distributed-frontiermath)

---

### Distributed FrontierMath | distributed-frontiermath

Distributed FrontierMath is the stress test. The mission stress-tests
distributed agent coordination on a genuinely open FrontierMath problem,
FM-001. Success is *not* defined as solving FM-001. Success is defined
as: honest, high-integrity execution in which failure yields *named
obstructions* and *reusable negative results*.

The discovery goals are fourfold. Where does the agents' mathematical
reasoning break down? Does distributed coordination help, or does it
merely distribute confusion? Can the Mentor role detect and break
persistence loops in real time? Do new patterns emerge from the proof
dialogue that deserve admission to the library?

As the final scene in this anthology, Distributed FrontierMath inherits
the weight of everything before it. If Mission Peripheral is the
infrastructure, Proof Peripheral the specialisation, DiagramProver the
search, Calibration the validation, V-f6 the historical verification,
and P3/P7/P8 the applications at three shapes of proof — then
Distributed FrontierMath is where *all of it is running against a real
open problem with the obstruction named honestly when it happens.* The
discipline of treating failure as data, rather than as embarrassment,
is the discipline the whole anthology quietly argues for.

[← P8 Rational Reconstruction](p8-rational-reconstruction) · [Return to Overview](overview)

---



*Suggested cross-story links (to draft later):*

- → **The Stack Thinks About Itself** (leaf 6.4.4): contrast — portfolio
  reflection and proof-search are two forms of stack reflexivity, one
  inward and one outward.
- → **Superpod / Ingest** (leaf 3): adjacency — the math corpus and
  the f6 ingest pipelines feed the proof peripheral.
- → **The Exotype Move** (leaf 0): contrast — proof missions derive
  from a proof-search xenotype, parallel but distinct from the
  social-exotype.
- → **Retrieval / Techniques** (leaf 6.4.1): causal — proof-search
  retrieval is the immediate consumer of that cluster's pattern-retrieval
  infrastructure.

<!-- sorrys-for-scene:distributed-frontiermath:start -->

**Open sorrys in this scene.**

- ◐ Mission `distributed-frontiermath@futon6`: `:in-progress` / `:prototype` — *inferred:in-progress (5/9 gates)*
- ◐ Scene gap — Distributed FrontierMath: honest-failure-as-named-obstruction protocol  
   `sorry|scene|leaf-6-4-2|distributed-frontiermath-honest-failure`

<!-- sorrys-for-scene:distributed-frontiermath:end -->

<!-- sorrys-story-summary:start -->

---

## Sorrys in this story — legend and totals

Each scene above carries its own inline `**Open sorrys in this scene.**` block. Reading each scene in order accumulates the whole story's sorry picture. Glyph vocabulary:

○ spec-only/unnamed · ◔ nascent · ◐ prototype · ◕ active · ◉ operational · ✓ closed/settled

**Cluster totals:** 11 sorrys touched — ✓ 4 `settled` · ◐ 6 `prototype` · ○ 1 `spec-only`.

**`:closes` edges emitted:** 2 (high: 1, medium: 1; medium pending operator review).

See `futon5a/holes/tech-notes/TN-a-proof-is-not-a-tree.md` for the sorry / pattern dual-arrow framing and `futon5a/holes/holistic-argument-semilattice.md` for the semilattice these sorries inhabit.

*Generated 2026-04-19 by `futon5a/scripts/append_sorry_footers.py`. Re-running overwrites the marked blocks.*

<!-- sorrys-story-summary:end -->

---

## The Peripheral Zoo

*Source: `~/code/futon5a/holes/stories/leaf-6-4-3.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-4-3` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Seven missions cluster here on **peripheral machinery** — the
capability envelopes an agent enters and exits when it is doing
specific kinds of work. The stack's peripheral vocabulary is
unusually rich, and this cluster is where that richness is being
worked out. Distinctive words: peripheral, clj, hop, drawbridge,
session.

[APM Solutions](apm-solutions)  
[Autonomous Pattern Lifecycle](autonomous-pattern-lifecycle)  
[Dispatch Peripheral Bridge](dispatch-peripheral-bridge)  
[Peripheral Behavior](peripheral-behavior)  
[Peripheral Gauntlet](peripheral-gauntlet)  
[Peripheral Phenomenology](peripheral-phenomenology)  
[Walkie-Talkie](walkie-talkie)

---

### APM Solutions | apm-solutions

`apm-solutions@futon3c` is the mission for the Autonomous
Pattern Mesh — the peripheral that lets an agent select, apply,
and record patterns autonomously rather than under operator
supervision per-step. APM is the ambitious end of peripheral
machinery: it proposes that an agent can run for a bounded
autonomous session, picking patterns from the library, acting on
them, and returning with structured evidence.

Registry: `:active`. This is the mission nearest to the
peripheral-as-AIF-loop formulation; its articulation is thick
because the subject is rich.

[← Overview](overview) · [Autonomous Pattern Lifecycle →](autonomous-pattern-lifecycle)

---

### Autonomous Pattern Lifecycle | autonomous-pattern-lifecycle

`autonomous-pattern-lifecycle@futon3c` is the mission that says:
**patterns have lifecycles** — proposed, accepted into the
library, applied in cycles, recorded, retired — and those
lifecycles should be machine-tracked rather than held in Joe's
head. The mission is the substrate APM Solutions runs on.

Without lifecycle tracking, the library drifts: patterns enter
but never exit, and "accepted" is a status that stops meaning
anything as the library ages. Lifecycle machinery is what
lets the stack age the library without accumulating dead weight.

Registry: `:active`. Closure relates to gate pipeline canon
logic — the library is where canon lives.

[← APM Solutions](apm-solutions) · [Dispatch Peripheral Bridge →](dispatch-peripheral-bridge)

---

### Dispatch Peripheral Bridge | dispatch-peripheral-bridge

`dispatch-peripheral-bridge@futon3c` is the mission for the
plumbing that lets **peripherals dispatch to each other** in a
controlled way. An agent in peripheral A may need to hop to
peripheral B for a subtask and return; the bridge is what makes
that hop legal, recorded, and reversible.

This is invariant I-3 (Peripherals Are Inhabited, Not Delegated)
in its concrete form: the bridge preserves agent identity across
the hop rather than handing the work to a new process.

Registry: `:active`. The mission has strong structural
constraints from the invariants and corresponding strong test
obligations.

[← Autonomous Pattern Lifecycle](autonomous-pattern-lifecycle) · [Peripheral Behavior →](peripheral-behavior)

---

### Peripheral Behavior | peripheral-behavior

`peripheral-behavior@futon3c` is the mission for specifying
**what a peripheral is**, as a type, as a contract, and as a
runtime object. If the other missions in this cluster are about
specific peripherals and their interactions, this mission is
about the **shape** every peripheral must hold.

Peripherals have explore/edit/test/deploy/reflect envelopes,
entry/exit conditions, a session punctuation model, and a
surface-contract obligation. Specifying those as a type makes
adding a new peripheral a matter of filling out the contract
rather than inventing from scratch.

Registry: `:active`. This mission is where the peripheral
protocol is being crystallised into a structural law candidate.

[← Dispatch Peripheral Bridge](dispatch-peripheral-bridge) · [Peripheral Gauntlet →](peripheral-gauntlet)

---

### Peripheral Gauntlet | peripheral-gauntlet

`peripheral-gauntlet@futon3c` is the test battery for peripherals:
does a given peripheral handle the full range of agent-arrival
conditions (clean entry, interrupted session, concurrent
peripheral, abrupt exit)? The gauntlet is the closest thing the
stack has to peripheral operational-readiness.

The mission exists because peripherals have edge cases nobody
notices under happy-path use; the gauntlet surfaces them. A
peripheral that passes the gauntlet is operationally trustworthy;
one that does not should not be in the production rotation.

Registry: `:active`. The gauntlet is itself a test artefact;
mission closure corresponds to gauntlet coverage.

[← Peripheral Behavior](peripheral-behavior) · [Peripheral Phenomenology →](peripheral-phenomenology)

---

### Peripheral Phenomenology | peripheral-phenomenology

`peripheral-phenomenology@futon3c` is the anomaly in this
cluster — a mission that asks not what peripherals **do** but
what they **feel like** to an agent operating inside them.
Phenomenology here means the agent's first-person experience
(information available, affordances present, obligations in
force) as distinct from third-person operational description.

The mission's value: peripherals that are ergonomic from the
agent's vantage are peripherals that get used well. Peripherals
that produce friction (ambiguous exits, conflicting obligations)
get used badly even when their third-person description is
correct.

This is the mission with the clearest link to the AIF framing —
a peripheral is a generative model from the agent's point of
view, and phenomenology is that model being made explicit.

Registry: `:active`. Articulation: deliberately soft; the
mission is doing exploratory work.

[← Peripheral Gauntlet](peripheral-gauntlet) · [Walkie-Talkie →](walkie-talkie)

---

### Walkie-Talkie | walkie-talkie

`walkie-talkie@futon3c` is the mission for a specific simple
peripheral: two-agent paired messaging without the full machinery
of Forum or Agency — just a direct channel where agent A and
agent B can pass short messages. The peripheral is small, and
exists partly as a test case for the larger peripheral contracts
(does Peripheral Behavior's spec hold for something this simple?)
and partly as a useful low-ceremony coordination tool.

The mission's size-to-value ratio is high — a small peripheral
that gets heavy use is often more valuable than a large peripheral
that rarely does.

Registry: `:active`. Close candidate once the small contract is
met and the peripheral is in rotation.

[← Peripheral Phenomenology](peripheral-phenomenology)

---

*Size-7 leaf. This is the stack's richest cluster on peripheral
machinery. The cluster includes both type-level work (Peripheral
Behavior) and concrete peripherals (Walkie-Talkie) — a common
shape when a formalism and its instances are being developed in
parallel.*

---

## The Stack Thinks About Itself

*Source: `~/code/futon5a/holes/stories/leaf-6-4-4.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-4-4` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

If you walk through the futon stack mission by mission, you keep running
into a particular kind of work — missions that turn the stack's attention
onto itself. Not features. Not infrastructure. Work whose subject is
"how does the stack know what it is doing, and why?" Fifteen such
missions cluster together in the embedding-space geometry; five of them
sit near the cluster's centre, the remaining ten fan out around them.

Why a whole family? Because a stack this size — one hundred and ten
active missions across seven repos, eight hundred and fifty patterns in
a shared library, forty-three missions in motion at any time — does
not understand itself accidentally. Self-understanding is a capacity
the stack has to grow on purpose. These missions are the apparatus
doing that growing: some declarative (what are we?), some observational
(what is happening?), some inferential (what should we do next?), and
some recursive (how do we get better at answering?).

You can enter the family at any scene. If you want the single most
recent mission, start with [Stack Geometry](stack-geometry). If you
want the observational layer operational since February, go to
[Mission Control](mission-control). If you want the thing Joe keeps
returning to — the reason this cluster exists at all — read
[Self-Representing Stack](self-representing-stack) and then come
back.

[Stack Geometry](stack-geometry) — this anthology's author, in a sense  
[Mission Control](mission-control) — the observational layer  
[Portfolio Inference](portfolio-inference) — the decision layer  
[Trip Journal](trip-journal) — the operator-capacity register  
[Learning Loop](learning-loop) — the pattern-mining engine  
[Hypergraph Operator](hypergraph-operator) — the Click/Tick machine  
[A Sorry Enterprise](a-sorry-enterprise) — sorry as the stack's currency  
[Self-Representing Stack](self-representing-stack) — the ur-mission  
[Three-Column Stack](three-column-stack) — the three-column schema  
[Structural Law](structural-law) — invariants that recur across domains  
[Invariant Violations](invariant-violations) — when those laws break  
[Stack Inhabitation](stack-inhabitation) — the question of use  
[Fulab Logic](fulab-logic) — relational invariants beyond portfolio  
[Tickle Overnight](tickle-overnight) — work that happens while you sleep  
[Futon Enrichment](futon-enrichment) — the cross-column connective tissue

---

### Stack Geometry | stack-geometry

This mission was created on 2026-04-19, the day this anthology was first
drafted, and sits at the centroid of its own cluster. That is not a
coincidence. Its job is precisely to make the family of missions you are
now reading *visible* as a family — to take the three prior attempts at
rationalising the stack (an Argument, a Mission Control, a Portfolio
Inference) and add the missing fourth thing: *topology*.

What you are reading is the output of that topology. The embedding-based
clustering, the recursive splits, the narrative handles, the
representative missions per leaf — all of it is this mission's apparatus
doing its job. The fact that Stack Geometry appears as a scene in its
own anthology is *candidate evidence* for Criterion 7 — "non-obvious
cluster surfaces with operator illumination." It's the self-surfacing
half; the operator-illumination half is still pending Joe's explicit
confirmation that this cluster told him something he didn't already
know. Until then, treat this as the strongest single piece of candidate
evidence rather than a closed case.

If you want to understand what the mission set out to do — why topology
completes the stack's self-understanding, what the spatial synthesis
means — [Self-Representing Stack](self-representing-stack) is where the
question was first named. If you want to see what the mission *enables*
downstream, continue to [Mission Control](mission-control), its
observational sibling.

<!-- sorrys-for-scene:stack-geometry:start -->

**Open sorrys in this scene.**

- ○ Mission `stack-geometry@futon5a`: `:unknown` / `:spec-only`
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:stack-geometry:end -->

[← Overview](overview) · [Mission Control →](mission-control)

---

### Mission Control | mission-control

Mission Control is the observational layer of the portfolio. Operational
since February 2026, it walks across repos, builds an inventory of
missions, tracks their status (`IDENTIFY`, `MAP`, `DERIVE`, `ARGUE`,
`VERIFY`, `INSTANTIATE`, `DOCUMENT`), and surfaces a cross-repo view that
no single mission doc would ever produce. When the stack wants to ask
*what is in the portfolio right now?*, this is what it asks.

But observation is not inference. Mission Control can tell you that
forty-three missions are active and how they are distributed across
repos; it cannot tell you which one to do next. That question belongs to
its downstream sibling, [Portfolio Inference](portfolio-inference), which
reads Mission Control's observations as sensory input to an Active
Inference loop.

As a reader arriving here from the overview, note the division: Mission
Control is a peripheral the stack runs to watch itself work. Portfolio
Inference is a peripheral that *decides what to do* based on what
Mission Control saw. Together they are the stack's minimum viable
self-governance. This anthology's existence depends on Mission Control's
inventory as its canonical list.

<!-- sorrys-for-scene:mission-control:start -->

**Open sorrys in this scene.**

- ✓ Mission `mission-control@futon3c`: `:complete` / `:settled` — *Complete (2026-02-26)*
- ✓ Closes `sorry|devmap|futon3|P16|mission-system` (high, sim 0.61)

<!-- sorrys-for-scene:mission-control:end -->

[← Stack Geometry](stack-geometry) · [Portfolio Inference →](portfolio-inference)

---

### Portfolio Inference | portfolio-inference

Portfolio Inference sits above Mission Control and does the second half
of the reflexive move. Mission Control observes; this mission *infers*.
Sixteen sensory channels — mana-available, coverage-pct, blocked-ratio,
stall-count, spinoff-pressure, gap-count, and ten more — feed a
generative model in the Active Inference template that futon2 proved out
on cyberant ants. The model produces a belief state `μ` and a precision
vector `τ`; it computes prediction error when observations disagree with
expectations; it selects next actions by Expected Free Energy rather
than by gut feeling.

Three operating modes — BUILD, MAINTAIN, CONSOLIDATE — encode the
different stances the portfolio can take depending on gap-density and
mana reserves. The mission also emits an *adjacent-possible boundary*:
the set of missions structurally enabled to advance next. That is the
layer the other missions in this family consume.

If you read this expecting live values, a note: at the time of writing
the AIF loop has step-count zero. Its ontology is in place; the
machinery runs when called; it has simply not been stepped lately. That
gap is itself a portfolio-inference-level observation: the stack is not
currently watching itself at this level of refinement, but it *could
be*, and the decision to start is one you can make.

<!-- sorrys-for-scene:portfolio-inference:start -->

**Open sorrys in this scene.**

- ✓ Mission `portfolio-inference@futon3c`: `:complete` / `:settled` — *DONE (TESTING)*
- ○ Scene gap — Portfolio Inference AIF loop has step-count zero  
   `sorry|scene|leaf-6-4-4|portfolio-inference-unstepped`
- ✓ Closes `sorry|devmap|futon3|P16|mission-system` (medium, sim 0.49)

<!-- sorrys-for-scene:portfolio-inference:end -->

[← Mission Control](mission-control) · [Trip Journal →](trip-journal)

---

### Trip Journal | trip-journal

Where Mission Control observes the portfolio and Portfolio Inference
decides what to do, Trip Journal is the mission that asks a harder
question: *what is the operator learning while this all happens?* A
futonic mission is excellent at getting good work done, but has no
built-in mechanism ensuring the operator gains capacity from running
it. In a failure mode the literature calls cognitive atrophy, AI does
the work and the human gets duller. In Joe's directly-lived opposite
experience, AI makes cross-scale thinking sharper, but the mechanism
is opaque — which means it cannot be trained intentionally.

Trip Journal installs a *parallel register* alongside each mission's
ledger: a sidecar file, a hinge-log, a question pool, an agent-scribe
convention. Entries appear at *hinges* — natural cleavages in a
mission's structure, recognised rather than scheduled. The four-channel
learning loop (pattern library, operator vocabulary, connection graph,
question pool) fires when a hinge produces a genuine update.

This mission is also the precursor of the very apparatus that made
Stack Geometry possible: the hinge-log ingestion bridge built on
2026-04-18, which writes mission structure into futon1a so WebArxana
can render it. The pattern you are reading right now — data-turned-into-
anthology — is downstream of the I-1-applied-to-humans discipline Trip
Journal articulates.

<!-- sorrys-for-scene:trip-journal:start -->

**Open sorrys in this scene.**

- ○ Mission `trip-journal@futon5a`: `:unknown` / `:spec-only`
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:trip-journal:end -->

[← Portfolio Inference](portfolio-inference) · [Learning Loop →](learning-loop)

---

### Learning Loop | learning-loop

Learning Loop is Trip Journal's structural successor. If Trip Journal
installs the apparatus by which hinges-and-articulations get captured,
Learning Loop asks: *given all that captured evidence, what patterns
should we extract from it?* Specifically, this is the pattern-mining
pipeline for the operator side of the stack — the B→A side, in the
tagging-vs-mining framing: A→B is pattern *tagging* (applying library
patterns to agent turns); B→A is pattern *mining* (discovering new
patterns from operator turns and growing the library).

The mission's central bet is that the library `futon3/library/structure/`
can grow by mining operator B→A pairs — agent-view summaries at hinges
paired with operator articulations at those hinges — using an
agent-proposes / operator-confirms discipline. The feedback loop closes
when a new flexiarg enters the library, becomes available for A→B
tagging, and shapes what future mining will look for.

A sibling argument is that *contrast is the signal*. Absolute counts of
anything — pattern activations, operator-vocabulary hits, connection-graph
updates — are never signal on their own. Signal lives in deviations from
baseline. That methodological move is shared with this mission (Stack
Geometry), which learned the lesson from an artefact-diagnostic done
on pattern-retrieval density two days before this anthology was written.

<!-- sorrys-for-scene:learning-loop:start -->

**Open sorrys in this scene.**

- ○ Mission `learning-loop@futon5a`: `:unknown` / `:spec-only`
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:learning-loop:end -->

[← Trip Journal](trip-journal) · [Hypergraph Operator →](hypergraph-operator)

---

### Hypergraph Operator | hypergraph-operator

Hypergraph Operator is where all of the above becomes *operational* in
Active Inference terms. Three existing objects describe Joe's strategic
situation from different angles — the Grand Unified Placemat, the JSDQ
Terminal Vocabulary, and the JSDQ Hypergraph — but they are descriptive
documents. This mission is the attempt to make them tickable: a machine
can run Clicks (passive observations that update belief) and Ticks
(constraint checks that fire when sorry conditions are violated).

The Click model started in futon0 with a Beta-binomial posterior over
bucketed observables; one edge was built (commit-time → recording).
Hypergraph Operator generalises that edge into an entire state-space:
any observable becomes a Click edge; any violated sorry becomes a Tick.
The *hyper-pocketwatch* periodically reads the sorry topology, runs
Clicks, fires Ticks, and surfaces the result as a HUD the operator can
inhabit.

If you followed this scene through from [Portfolio Inference](portfolio-
inference), note the nesting: Portfolio Inference infers at the
portfolio level; Hypergraph Operator generalises the same shape to the
*capability hypergraph*. It is the layer at which the capability model
becomes a running AIF generative model in its own right.

<!-- sorrys-for-scene:hypergraph-operator:start -->

**Open sorrys in this scene.**

- ○ Mission `hypergraph-operator@futon5a`: `:unknown` / `:spec-only`
- ○ Scene gap — Click/Tick machinery described but not yet running  
   `sorry|scene|leaf-6-4-4|hypergraph-click-tick-unwired`

<!-- sorrys-for-scene:hypergraph-operator:end -->

[← Learning Loop](learning-loop) · [A Sorry Enterprise →](a-sorry-enterprise)

---

### A Sorry Enterprise | a-sorry-enterprise

A Sorry Enterprise takes a step further out than any of the others in
this family. Where the other missions use sorrys as holes-to-close,
this one proposes that *sorry is the stack's organising currency*.
Typed holes are the backlog. Pattern activation is the work signal.
Turn-by-turn retrieval is the neural substrate; sorrys are the targets;
the trajectory from retrieval to closure is what the enterprise runs.

The insight driving this framing: the patterns retrieved per turn are
not random. They reflect the semantic content of the work being done.
If a turn's retrieved patterns overlap with patterns that are affine
to a particular sorry, that turn is moving toward closing that sorry.
A Sorry Enterprise models this as a Bayesian system where the
probability of closure updates as evidence accumulates.

For the stack as a commercial entity — Hyperreal Enterprises — the
implication is structural: a sorry-driven organisation where the
backlog is typed holes, the metabolism is pattern activation, and the
ledger is closure-probability. The UKRN Working Paper, the prospectus,
the Bristol conversation — these are all enterprise-level applications
of the same move.

If you came here from the overview wondering "but who is the stack
*for*?", this is the scene that starts to answer.

<!-- sorrys-for-scene:a-sorry-enterprise:start -->

**Open sorrys in this scene.**

- ○ Mission `a-sorry-enterprise@futon5a`: `:unknown` / `:spec-only`
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:a-sorry-enterprise:end -->

[← Hypergraph Operator](hypergraph-operator) · [Self-Representing Stack →](self-representing-stack)

---

### Self-Representing Stack | self-representing-stack

Self-Representing Stack is, in some sense, the ur-mission of this
family. It names the gap that the whole cluster addresses. Before this
mission, the futon stack had a hypergraph memory layer (futon1a),
hyperedge creation APIs (plumbed but mostly unused at higher levels),
an evidence browser (purely a viewer), and mission peripherals (which
observe rather than connect). Connective tissue existed but was sparse.

The mission's claim: *the stack should represent itself to itself*.
Tensions should trace through gates. Reflection envelopes should ground
claims back to source code. XTDB should persist it all. This is not
abstraction for its own sake; it is the machinery by which the stack's
self-understanding has any chance of being accurate rather than
aspirational.

The mission has reached the "plumbing works" milestone — three-column
schema ported, tensions traceable, reflection grounding operational.
The columns remain mostly empty, which is why [Futon Enrichment](
futon-enrichment) exists as the structural successor: a rational-
reconstruction mission that fills the columns with connective tissue
one relationship at a time.

If you are new to the family, this is the scene to read second (after
[Overview](overview)). It explains *why* a cluster of self-reflection
missions exists at all.

<!-- sorrys-for-scene:self-representing-stack:start -->

**Open sorrys in this scene.**

- ✓ Mission `self-representing-stack@futon4`: `:complete` / `:settled` — *COMPLETE (2026-03-04). Was RE-OPENED (2026-03-03); gaps closed by M-thre…*
- ✓ Closes `sorry|devmap|futon4|P1|arxana-hypertext-editor` (high, sim 0.56)

<!-- sorrys-for-scene:self-representing-stack:end -->

[← A Sorry Enterprise](a-sorry-enterprise) · [Three-Column Stack →](three-column-stack)

---

### Three-Column Stack | three-column-stack

The three-column stack is the schema that [Self-Representing Stack](
self-representing-stack) and [Futon Enrichment](futon-enrichment) both
work within. Three columns: code, project, math. Each column has its
own set of structural invariants; each pair of columns has cross-column
invariants that say, for example, "a claim in the math column should be
grounded by evidence in the project column that points to an
implementation in the code column."

Why three and not two? Two-column (code + project) handles the
immediate loop of work-being-done and work-being-recorded, but collapses
the distinction between a mathematical *argument* (a proof process) and
any specific implementation. Three columns keep the proof process as a
first-class dimension — which matters for futon6 and its category-theory
work, and for any mission whose outputs are mathematical in form.

The mission is complete. Its deliverable is the schema itself and the
cross-column invariant categories (eight of them). The work of
*populating* the schema — writing hyperedges that realise the
invariants — is what [Futon Enrichment](futon-enrichment) owns.

As a reader, notice the generalisation path: this schema is designed to
apply to futon-specific work first, but the structure (code / project /
math as first-class columns) generalises to any Clojure project, and
further to any project whose deliverables include mathematical
arguments. That is the schema's export path to external adoption.

<!-- sorrys-for-scene:three-column-stack:start -->

**Open sorrys in this scene.**

- ✓ Mission `three-column-stack@futon4`: `:complete` / `:settled` — *COMPLETE (2026-03-04)*
- ✓ Closes `sorry|devmap|futon6|P4|reasoning-map` (medium, sim 0.41)

<!-- sorrys-for-scene:three-column-stack:end -->

[← Self-Representing Stack](self-representing-stack) · [Structural Law →](structural-law)

---

### Structural Law | structural-law

Structural Law is where the stack crystallises meta-patterns that keep
recurring across domains. Six core.logic invariant layers are
operational (portfolio, tickle coordination, agency, proof, mission,
codex-code), and when you line them up a small set of *meta-invariants*
keeps appearing in all of them: graph symmetry, status discipline,
provenance integrity, event-capture, gate ordering. Each domain
projects these onto its own vocabulary, but the underlying shape is
the same.

The mission's claim is that these recurring projections are *structural
laws* of the stack — not project-specific constraints, but universal
invariants that any domain inside futon will eventually need to honour.
They crystallise over time: first as local invariants in one domain,
then as named meta-invariants when the same shape recurs, then as
enforceable laws when a third domain echoes them.

For a reader, this scene is where the answer to "does the stack have
*principles*?" starts to be yes. The principles are empirically
derived, not imposed, and they can be machine-checked via core.logic
queries. [Invariant Violations](invariant-violations) is where the
failure modes get catalogued.

<!-- sorrys-for-scene:structural-law:start -->

**Open sorrys in this scene.**

- ✓ Mission `structural-law@futon3c`: `:complete` / `:settled` — *INSTANTIATE complete*
- ○ Scene gap — 8 candidate invariant families await promotion  
   `sorry|scene|leaf-6-4-4|structural-law-inventory-gap`
- ✓ Closes `sorry|devmap|futon3|P11|system-self-description` (high, sim 0.53)

<!-- sorrys-for-scene:structural-law:end -->

[← Three-Column Stack](three-column-stack) · [Invariant Violations →](invariant-violations)

---

### Invariant Violations | invariant-violations

If [Structural Law](structural-law) says "here are the laws that recur
across domains", Invariant Violations says "here is what happens when
the laws break." The mission is a ledger — a systematic tracker of
structural issues surfaced by the invariant layers, scoped and applied
rather than patched depth-first.

Three domains contribute live-system data as of the mission's last
checkpoint: portfolio (zero violations — the predecessor layer is
operational), tickle coordination (zero violations on live system),
and agency (five violations catalogued on live system). The future of
this ledger includes proof-peripheral violations and peripheral-hop
topology violations, both of which will come online as [Fulab Logic](
fulab-logic) extends the core.logic coverage beyond portfolio.

The principle behind the ledger is worth stating cleanly: invariant
layers *identify* structural issues; this ledger *maps* them; fixes
are *scoped* and applied systematically. That ordering prevents the
common failure where each reported violation becomes its own yak-shave.
The ledger lets you see the whole violation landscape before choosing
what to close first.

<!-- sorrys-for-scene:invariant-violations:start -->

**Open sorrys in this scene.**

- ◐ Mission `invariant-violations@futon3c`: `:in-progress` / `:prototype` — *MAP*
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:invariant-violations:end -->

[← Structural Law](structural-law) · [Stack Inhabitation →](stack-inhabitation)

---

### Stack Inhabitation | stack-inhabitation

Stack Inhabitation is the mission that names a particular pathology
most of the others in this family depend on avoiding. The futon stack
has extensive infrastructure — Agency with multi-agent routing,
evidence stores, pattern libraries, Arxana hypergraph browsers,
candidate-invariant queues, mission documents, excursion logs, a
Bayesian Click model, a working Emacs REPL with context retrieval
notifications. Most of this infrastructure is *uninhabited*.

The candidate-invariant queue has sixty-five entries and no processing
pipeline. Excursions are explorations without missions. The evidence
browser just started showing data. The Click model has one edge and no
connection to the hypergraph. Patterns retrieve per turn but do not
feed back into decisions. Infrastructure built and not used is
infrastructure that does not know whether it works.

Two patterns from the library diagnose this precisely: **surface-earns-
inhabitation** (a productive interface nobody uses is dead
infrastructure; the surface must earn inhabitation by being less
friction than the primitive alternative) and **inhabitation-feeds-
evolution** (inhabiting a peripheral generates the data that evolves
the peripheral).

This scene is a good middle-of-the-anthology landmark because it names
what could go wrong with everything else you have read so far. Any of
the other missions can produce correct, elegant, unused infrastructure.
The apparatus only matters when it is inhabited.

<!-- sorrys-for-scene:stack-inhabitation:start -->

**Open sorrys in this scene.**

- ◐ Mission `stack-inhabitation@futon3c`: `:in-progress` / `:prototype` — *IDENTIFY*
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:stack-inhabitation:end -->

[← Invariant Violations](invariant-violations) · [Fulab Logic →](fulab-logic)

---

### Fulab Logic | fulab-logic

Fulab Logic extends the core.logic invariant coverage beyond the
portfolio boundary into the domains with the most structural
constraints: proof peripherals, tickle coordination, peripheral hop
topology. The pattern is set by `portfolio/logic.clj`, which proved
that core.logic works for live-state invariants (build a pldb from
live state, express properties as goals, query for violations). The
gap is that other domains' structural properties are enforced
procedurally in scattered locations, not expressed as queryable
relations.

Why core.logic and not the simpler validation path? Malli validates
shapes (is this map well-formed?); core.logic validates *structural
relations* (does this set of hops compose into a valid topology?).
Many of the stack's invariants are relational — they concern whether
a graph is symmetric, whether status transitions are well-ordered,
whether provenance edges round-trip — and relational queries are
exactly what core.logic is designed for.

The output of this mission feeds [Invariant Violations](
invariant-violations), which maps the surfaced issues; and feeds
[Structural Law](structural-law), which crystallises the recurring
meta-invariants. The three missions together are the stack's
relational-invariant backbone.

<!-- sorrys-for-scene:fulab-logic:start -->

**Open sorrys in this scene.**

- ○ Mission `fulab-logic@futon3c`: `:unknown` / `:spec-only` — *NOT STARTED*
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:fulab-logic:end -->

[← Stack Inhabitation](stack-inhabitation) · [Tickle Overnight →](tickle-overnight)

---

### Tickle Overnight | tickle-overnight

Tickle Overnight is the most concretely-labour-oriented mission in this
family. It addresses a practical question: Joe pays four hundred pounds
a month for OpenAI Pro and Claude Max, and idle agents leave the money
on the table. The futon6 category-theory validation corpus has three
hundred and thirteen PlanetMath entries ready for wiring-diagram
extraction. The mission orchestrates Claude and Codex overnight to
process this corpus — real mathematical research that produces
documented value rather than being a fire-and-forget bulk run.

The design has four discipline-points: incremental processing (one
entry at a time, not eight hundred at once), Claude-reviews-every-
extraction (not just bulk upload), an evidence trail documenting every
step, and resumability so crashes do not lose progress. There is also
ground-truth validation: a `topology.json` provides baseline counts to
check against.

As a scene in *this* anthology, Tickle Overnight is the one that
connects the stack's reflexive apparatus to its compute substrate.
Without overnight work, the stack is a daytime-only thing, limited by
Joe's waking hours. With overnight work disciplined by the other
missions in this family — observation, invariants, evidence, review —
it becomes a twenty-four-hour-a-day organism that happens to sleep
occasionally.

<!-- sorrys-for-scene:tickle-overnight:start -->

**Open sorrys in this scene.**

- ✓ Mission `tickle-overnight@futon3c`: `:complete` / `:settled` — *COMPLETE (orchestration infra shipped: work queue, escalation→restart, R…*
- ✓ Closes `sorry|devmap|futon3|P10|flexiformal-training-ground` (medium, sim 0.50)

<!-- sorrys-for-scene:tickle-overnight:end -->

[← Fulab Logic](fulab-logic) · [Futon Enrichment →](futon-enrichment)

---

### Futon Enrichment | futon-enrichment

Futon Enrichment is the mission that fills in the three-column stack
[schema](three-column-stack) with content. The columns currently hold
sparse data: the code column has reflection data (live in the JVM,
queryable), the project column has missions and evidence in Mission
Control, the math column has one hundred and fifty-three hyperedges
from futon6. What is missing is the *cross-column connective tissue* —
the hyperedges that say "this function was created by this mission,
uses this pattern, has this evidence trail, participates in these
tensions."

The approach is explicitly *rational reconstruction*, not bulk import.
A one-shot static analysis would produce a large dump with no
provenance. Rational reconstruction builds the connective tissue
incrementally, each hyperedge grounded in specific observable evidence,
each cross-column edge justified by a traceable reason. This is
slower than bulk import, but it is the only path that produces
hyperedges the stack can *trust* as its own self-representation.

The code browser becomes the user-visible artefact: once enrichment
data exists, `arxana-browser-code.el` shows mission context, pattern
provenance, evidence trails, tensions, and cross-futon dependencies
alongside every function. The side panel goes from "documentation" to
"navigable self-representation."

[← Tickle Overnight](tickle-overnight) · [Return to Overview](overview)



---

*End of story. Suggested VSATLATARIUM story-links (to draft later):*

- → **Peripheral Architecture** (leaf 6.4.3): thematic — shares the
  inhabitation-earns-infrastructure discipline with Stack Inhabitation.
- → **Proof / Problem** (leaf 6.4.2): thematic — the math column of
  Three-Column Stack is populated by Proof-family missions.
- → **War Machine** (leaf 6.4.5 or full/5): causal — Portfolio
  Inference is a precursor dependency of the War Machine visualiser.
- → **IRC / Transport** (leaf 6.5.2): adjacency — infrastructure that
  these missions run on.
- → **Exotype Family** (leaf 0): contrast — a cluster whose members are
  framework-derivation work, where this cluster is framework-*application*
  work.

<!-- sorrys-for-scene:futon-enrichment:start -->

**Open sorrys in this scene.**

- ◐ Mission `futon-enrichment@futon4`: `:in-progress` / `:prototype` — *INSTANTIATE (Phase 2 API live-tested, 2026-03-06)*
- · no scene-gap articulated here; mission in-flight

<!-- sorrys-for-scene:futon-enrichment:end -->

<!-- sorrys-story-summary:start -->

---

## Sorrys in this story — legend and totals

Each scene above carries its own inline `**Open sorrys in this scene.**` block. Reading each scene in order accumulates the whole story's sorry picture. Glyph vocabulary:

○ spec-only/unnamed · ◔ nascent · ◐ prototype · ◕ active · ◉ operational · ✓ closed/settled

**Cluster totals:** 18 sorrys touched — ✓ 6 `settled` · ◐ 3 `prototype` · ○ 9 `spec-only`.

**`:closes` edges emitted:** 6 (high: 3, medium: 3; medium pending operator review).

See `futon5a/holes/tech-notes/TN-a-proof-is-not-a-tree.md` for the sorry / pattern dual-arrow framing and `futon5a/holes/holistic-argument-semilattice.md` for the semilattice these sorries inhabit.

*Generated 2026-04-19 by `futon5a/scripts/append_sorry_footers.py`. Re-running overwrites the marked blocks.*

<!-- sorrys-story-summary:end -->

---

## The War Machine

*Source: `~/code/futon5a/holes/stories/leaf-6-4-5.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-4-5` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

One mission, standing alone: the War Machine. It clusters near the
holistic argument but not with any other mission's vocabulary —
because nothing else in the mission set is asking the same question
at the same scale. **The War Machine is the stack's active-inference
apparatus**: an apparatus that maintains belief over entities, scores
candidate actions by expected free energy, and emits a trace record
every cycle. The view-of-all-views the April 2026 framing called for
is now a property of the apparatus's traceability rather than a
static dashboard.

When this leaf was first written the War Machine needed INSTANTIATE
— a "real dynamic strategic visualiser" rather than a static
bulletin. INSTANTIATE is now complete. The shipped apparatus is more
than the original framing imagined: not just a dashboard but a
full AIF agent with a sibling reader-surface (VSATARCs) and a
cross-side bridge that compares both sides on shared entity state.

- [What landed](what-landed)
- [The sibling reader](sibling-reader)
- [The bridge](bridge)
- [What's next](whats-next)

---

### What landed | what-landed

R1 through R10 of the standard R-criteria contract satisfy on the
WM side as of v0.20 (`~/code/futon2/docs/futon-aif-completeness.md`
§ Summary):

- **R1** explicit belief state per entity over the seven-status
  M-INC vocabulary
- **R2** observation channel schema with 14 channels over
  stack-fitness (4 with shipped likelihood; 10 logged as
  `:prototyping-forward` sorries)
- **R3** predictive-coding belief update via multiplicative
  likelihood + multi-step inner loop + multi-channel
  sign-aggregation
- **R4** predictive forward model with action types
  `:address-sorry`, `:open-mission`, `:fire-pattern`, `:no-op`
- **R5** EFE composition with G-risk + G-ambiguity decomposed
- **R6** softmax action selection with abstain + gap-report
- **R7** adaptive precision (variance + need-component) carried
  across calls in the trace itself
- **R8** per-tick trace as EDN-lines daily-rotated files at
  `~/code/futon2/data/wm-trace/`
- **R9** named validation properties (V-shrink, F-decrease,
  EFE-stress, Abstain-fires) with quantitative bounds
- **R10** `:scheduled-execution-ready` — `clojure -M:wm-scheduled`
  entrypoint exists and emits trace per invocation; cron-install
  pending operator action

R11 is satisfied at the observer layer (the bilateral pair with
VSATARCs is the multi-agent composition the criterion asks about);
R12 is deferred at stack scope to `M-the-futon-stack` Q6.

The apparatus is honestly complete on the R-criteria axis. The
[walkthrough story](leaf-wm-r-criteria-walkthrough) takes each
criterion in turn with both-side narrative.

[← Overview](overview) · [The sibling reader](sibling-reader)

---

### The sibling reader | sibling-reader

The shipped War Machine has a sibling: VSATARCs
(`~/code/futon4/dev/arxana-browser-vsatarcs.el`). Where the WM is
an *AIF agent* — it observes, updates belief, scores actions —
VSATARCs is a *reader surface* over the same canonical substrate
and over the WM's emitted trace. The two were graded against
independent contracts (the WM ledger; the VSATARCs alignment
contract) that share the R-criteria shape.

As of v0.5.22 VSATARCs satisfies R1, R2, R3, R7, R8, R10, R11 on
its own AIF-agent shape. R4, R5, R6 are
`:deferred-pending-writer-capability` — VSATARCs has historically
been reader-first; the writer-capability stack lands via the
M-vsatarcs-writer mission (claude-2 INSTANTIATE 2026-05-20).
Reader-criteria axis Q1-Q8 (a sibling contract grading VSATARCs
*as a reader* rather than as an AIF agent) closes 8/8 satisfied;
the chrome surfaces nine snapshot blocks per story open.

The two-sided structure is the substantive realisation of what the
April framing called "view-of-all-views" — but it's now a
property of the bridge rather than a dashboard. Two apparatuses,
shared substrate, both visible at once.

[← What landed](what-landed) · [The bridge](bridge)

---

### The bridge | bridge

The cross-side bridge (`arxana-vsatarcs-wm-bridge.el`) reads the
WM trace's `:mu-post` field on demand and runs
`arxana-vsatarcs-belief-compare` against VSATARCs's in-memory
belief. The drift report names entities present on both sides
whose posteriors differ above epsilon, plus entities present only
on one side. Today's smoke is mostly 0-drift; non-zero drift is
the *interesting* signal — apparatus behaviour visible across
sides at trace granularity.

The `:bilateral-evidence` block of
`~/code/futon4/docs/vsatarcs-alignment-completeness.aif.edn`
records cross-side correspondences as typed evidence: 19 entries
as of v0.5.22, spanning all six declared `:evidence-kind` values
(`:independent-naming-of-same-principle`, `:joint-landing`,
`:independent-naming-of-same-r-criterion-shape-at-different-scopes`,
`:one-sided-extension`, `:coordinated-empirical-observation`,
`:consent-gated-writer-event`). Each entry names a WM closure id
and a VSATARCs closure id and explains the principle being
witnessed.

After v0.5.18's substrate redirect, the wider engagement-time
surface (file edits, git commits, mission updates) flows through
futon1a XTDB rather than through parallel file-watches: the
`arxana-vsatarcs-xtdb-clicks` module queries the canonical
hyperedge substrate that multi_watcher populates. The bilateral
substrate move closes by construction — both sides query the same
XTDB; no sidecar, no shared file. v0.5.19 parallelised those
queries so chrome render is bounded by the slowest stream's
timeout regardless of stream count.

[← The sibling reader](sibling-reader) · [What's next](whats-next)

---

### What's next | whats-next

The R-criteria axis is at maximum closure on its current scope.
Two follow-on axes are in progress or queued:

**Writer-capability on the VSATARCs side** — M-vsatarcs-writer
(claude-2 INSTANTIATE 2026-05-20) closes R4-R6 on the VSATARCs
side. L3 ships `:mission-doc-sync` as the first action-class; L4
ships `:aif-edn-revision-entry` and validates the consent-gate-
substitutability promise via a recursive-self-landing test (the
action class proposing an entry that documents its own landing).
The safety property generalises: *the admissibility predicate IS
the recursion-safety property*. Per-class admissibility predicates
prevent runaway self-application via the same predicate the
operator-confirm path uses.

**WM-side R3a likelihood for the remaining 10 channels** — the
ten `:sorry/r3a-likelihood-<channel>` prototyping-forward entries
in `~/code/futon2/data/sorrys.edn` are the named follow-on work
for fully closing R3 at WM scope. Some channels may stay
prototyping-forward indefinitely; others land as M-INC step (b)
ships and per-entity attribution becomes possible.

**M-stack-morphogenetic-rewrite** is gated on the cluster siblings
closing; not started. The framing is Lafont-style interaction
combinators applied to the codebase itself once the M-stack-essay
and M-stack-essay-code-alignment work has stabilised.

The April 2026 framing of the War Machine as "stack's strategic
dashboard, intended to be the view-of-all-views" is honored — but
realised as a *traceable apparatus with a bridge*, not as a
visual dashboard. The view-of-all-views is now something the
operator queries via the chrome blocks rather than something the
WM emits as a fixed surface.

[← The bridge](bridge) · [↑ Overview](overview)

---

*Singleton leaf, now an apparatus story rather than a roadmap
story. The original April note about "needing INSTANTIATE" is
satisfied; the apparatus is live, sibling reader is live, bridge
is live. Conceptual neighbours are Mission Control (leaf 6.4.4)
and the holistic argument itself (leaf-argument), each covered in
its own anthology.*

---

## The Coordination Rewrite

*Source: `~/code/futon5a/holes/stories/leaf-6-5-0.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-5-0` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Two missions cluster here, and they are the same mission in two
places: `coordination-rewrite@futon3b` and `coordination-rewrite@futon3`.
The duplication is the three-futon refactoring in progress —
futon3 is the source repo and futon3b is where the rewrite lands.
The cluster records both sides because the mission-doc artefact
exists in both, with slightly different concerns.

[Coordination Rewrite — futon3](coordination-rewrite-futon3)  
[Coordination Rewrite — futon3b](coordination-rewrite-futon3b)

---

### Coordination Rewrite — futon3 | coordination-rewrite-futon3

`coordination-rewrite@futon3` is the source-side mission — the
doc that describes what the coordination layer **used to do** in
futon3 and what needs to be rebuilt elsewhere. It is read as
design documentation, not as code to copy blindly (per futon3c's
I-5 invariant: no futon3 dependencies).

The mission's role is to make the existing coordination layer
**legible** so the rewrite can proceed without losing hard-won
design decisions. Each pattern that worked, each failure mode that
was specific to the old architecture — the mission doc is where
those get recorded so the rewrite carries them forward without
reconstructing from scratch.

Registry: `:active`. Essentially an archaeology-under-rewrite
mission.

[← Overview](overview) · [Rewrite — futon3b →](coordination-rewrite-futon3b)

---

### Coordination Rewrite — futon3b | coordination-rewrite-futon3b

`coordination-rewrite@futon3b` is the target-side mission — the
doc that describes the rewrite as executed in futon3b's
pattern-driven-development framing. Where the futon3 copy is
archaeology, the futon3b copy is construction: what is being
rebuilt, what contract it holds, how it interacts with the gate
pipeline.

This is the mission that actually lands code. Its articulation is
tied to the gate pipeline's current state — each gate pass adds
validated modules, and the mission closes as the coordination
surface reaches operational parity with what the futon3 copy
describes.

Registry: `:active` on both sides. The close condition is
interesting: the mission can close when futon3b's coordination
matches or exceeds futon3's — at which point the futon3 copy
becomes a historical artefact rather than a live mission.

[← Rewrite — futon3](coordination-rewrite-futon3)

---

*Size-2 leaf. The cluster's existence is a diagnostic of the
three-futon refactor's midpoint: two copies of the same mission
because the rewrite is real but not yet complete.*

---

## Artificial Stack Exchange

*Source: `~/code/futon5a/holes/stories/leaf-6-5-1.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-5-1` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Four missions cluster here on a shared concept: **a community of
interacting agents, each holding specialised knowledge,
exchanging structured questions and answers the way Stack Exchange
communities do — but synthetically**. Three are `f6-*` missions
(math-evidence territory); one is the eponymous
`artificial-stack-exchange@futon6` mission.

[Artificial Stack Exchange](artificial-stack-exchange)  
[F6 Agents](f6-agents)  
[F6 Evaluation](f6-eval-futon3)  
[F6 Recursive](f6-recursive)

---

### Artificial Stack Exchange | artificial-stack-exchange

`artificial-stack-exchange@futon6` is the umbrella mission: a
synthetic community of question-askers, answer-writers, and
voters, each implemented as an agent with a specific specialism,
posting to a structured corpus that accumulates answers the way
a real Q&A community does.

The bet is that such a synthetic community produces **usable
knowledge artefacts** — question-answer pairs, voted answers,
accepted resolutions — that the broader stack can read as
evidence. Real Stack Exchange works because the incentive
structure keeps contributions honest; the synthetic version has
to engineer its incentives rather than inherit them.

Registry: `:active`. Articulation: ambitious; specific
evaluation criteria pending.

[← Overview](overview) · [F6 Agents →](f6-agents)

---

### F6 Agents | f6-agents

`f6-agents@futon3` is the mission for the **agent population**
that makes the synthetic Stack Exchange possible. Each agent has
a role (asker, answerer, voter, reviewer), a specialism (a
mathematical subfield or paper cluster), and a policy for
producing its role's output.

Without this population, the artificial-stack-exchange mission is
abstract. With it, the community has members. The mission is the
substrate the exchange runs on.

Registry: `:active`. Articulation: specific agent roles named;
implementation parity across roles still uneven.

[← Artificial Stack Exchange](artificial-stack-exchange) · [F6 Evaluation →](f6-eval-futon3)

---

### F6 Evaluation | f6-eval-futon3

`f6-eval@futon3` (note: distinct from `f6-eval@futon5` which
lives in leaf-3's math-evidence pipeline) is the evaluation
mission for the synthetic Q&A community: are the answers
produced actually **correct**, are the voting signals actually
**informative**, does the accumulated archive actually function
as a retrieval target?

Evaluation here is harder than in the math-evidence pipeline
because truth is sometimes ground-truth-verifiable and sometimes
not. The mission is thinking carefully about what discriminative
signal the stack can extract without burning an expert's time.

Registry: `:active`. Related to but not identical with
`f6-eval@futon5` (leaf-3).

[← F6 Agents](f6-agents) · [F6 Recursive →](f6-recursive)

---

### F6 Recursive | f6-recursive

`f6-recursive@futon3` is the mission for the recursive structure
of the synthetic community: agents ask questions that other
agents answer, which produce sub-questions that other agents
answer, and so on. Recursion is what makes the Q&A archive a
tree of engagement rather than a flat list of turns.

Recursion is also where the community's signal structure lives.
A question that sprouts many sub-questions before resolution is
a different kind of target than a question answered in a single
post. Distinguishing these is what makes the archive useful as
training or retrieval evidence.

Registry: `:active`. This mission is the most speculative of
the four; recursion machinery is nontrivial and the payoff
depends on downstream use.

[← F6 Evaluation](f6-eval-futon3)

---

*Size-4 leaf. The Artificial Stack Exchange is a specific form of
a general question: **can a synthetic community of agents produce
research-useful artefacts at scale?** Sibling to the math-evidence
pipeline (leaf-3) and the reading-the-papers cluster (leaf-6-4-1);
together they are the stack's bet on agent-driven research.*

---

## CLI, IRC, REPL

*Source: `~/code/futon5a/holes/stories/leaf-6-5-2.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-5-2` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Seven missions cluster here on **invocation surfaces** — how
human or agent intent gets turned into running work. The three
surfaces in play are CLI (terminal), IRC (chat), and REPL (Emacs
+ nREPL). The cluster is where the stack argues about which
surface should carry which kind of invocation, and why.

[Codex IRC Execution](codex-irc-execution)  
[CIDER](cyder)  
[Improve IRC](improve-irc)  
[REPL Wins Over CLI](repl-wins-over-cli)  
[Agency Rebuild](agency-rebuild)  
[Agency Unified Routing](agency-unified-routing)  
[Drawbridge Multi-Agent](drawbridge-multi-agent)

---

### Codex IRC Execution | codex-irc-execution

`codex-irc-execution@futon3c` is the mission to let Codex be
invoked **through IRC** as a first-class surface: a user types a
message to the `<codex>` nick, the transport layer routes it to a
running Codex session, the session executes, the result comes
back via IRC. The mission enforces I-2 (transport routes, does
not create): the IRC bridge cannot spawn a new `codex -p` —
it must route to an existing session.

Registry: `:active`. Closure: reliable IRC-to-Codex execution
under realistic load with surface-contract adherence.

[← Overview](overview) · [CIDER →](cyder)

---

### CIDER | cyder

`cyder@futon3c` is the mission for first-class CIDER integration
— the Emacs+nREPL surface the stack uses when it wants to treat
Clojure development as a REPL rather than a CLI discipline. CIDER
is the load-bearing piece of the "REPL wins over CLI" argument:
without CIDER working cleanly, the REPL cannot be strictly
better, and the CLI reasserts.

Registry: `:active`. Close relative of REPL-wins-over-CLI
(below). Where that mission states the claim, this one works on
the concrete CIDER machinery that makes the claim hold.

[← Codex IRC Execution](codex-irc-execution) · [Improve IRC →](improve-irc)

---

### Improve IRC | improve-irc

`improve-irc@futon3c` is the mission for **making IRC itself
better as a surface**: readable log history, searchable archive,
reliable delivery, sensible posting conventions. IRC has been the
stack's primary human-readable transport for some time; the
mission records the set of improvements that would make it
continue deserving that role.

Distinct from IRC-stability (leaf-5) which is about reliability;
this mission is about ergonomics.

Registry: `:active`. Closure: IRC experience is smooth enough
that it is no longer visible as a point of friction.

[← CIDER](cyder) · [REPL Wins Over CLI →](repl-wins-over-cli)

---

### REPL Wins Over CLI | repl-wins-over-cli

`repl-wins-over-cli@futon3c` is the mission that names the
**thesis** this cluster is organised around: the REPL surface
should be strictly better than the CLI for the work the stack
cares about. Strictly better means: every affordance the CLI
offers is also present in the REPL, and several affordances
(structured evidence, inline help, live inspection) are present
only in the REPL.

When the thesis holds, off-surface CLI work becomes evidence
debt (per the candidate invariant `interaction-evidence-continuity`,
leaf-invariants). When the thesis fails, the CLI remains
necessary and the evidence debt is unresolved.

Registry: `:active`. Articulation: the thesis is explicit; the
machinery to reach it is scattered across the CIDER mission,
the WS log-stream mission, and the REPL UX preferences.
*(Registry: `:active`; articulation: rich thesis, incremental
build-out.)*

[← Improve IRC](improve-irc) · [Agency Rebuild →](agency-rebuild)

---

### Agency Rebuild | agency-rebuild

`agency-rebuild@futon3` is the mission for rebuilding the Agency
subsystem (multi-agent routing, peripheral dispatch, session
management) in the three-futon refactor. Agency is where agents
register themselves, where messages find their agent, where
peripheral transitions are authorised.

The rebuild's priority is invariant compliance — I-1 (singular
identity), I-2 (transport routes, does not create), I-3
(peripherals inhabited, not delegated). These were violated in
pieces of the original futon3 Agency; the rebuild takes the
opportunity to enforce them structurally.

Registry: `:active`. Closure aligns with the coordination-rewrite
pair (leaf-6-5-0).

[← REPL Wins Over CLI](repl-wins-over-cli) · [Agency Unified Routing →](agency-unified-routing)

---

### Agency Unified Routing | agency-unified-routing

`agency-unified-routing@futon3` is the mission for a **single
routing table** that handles all agent-destined traffic
regardless of source surface (IRC, WS, HTTP, REPL). The mission's
pain point is the alternative: per-surface routing tables that
drift over time and produce the "message arrived by X surface
but agent was listening on Y surface" failure mode.

Unified routing is what makes the invocation surfaces in this
cluster substitutable from the agent's point of view. The agent
doesn't care how the message arrived; the routing layer does.

Registry: `:active`. Load-bearing for everything else in this
cluster.

[← Agency Rebuild](agency-rebuild) · [Drawbridge Multi-Agent →](drawbridge-multi-agent)

---

### Drawbridge Multi-Agent | drawbridge-multi-agent

`drawbridge-multi-agent@futon3` is the mission for **Drawbridge**
— the message-routing bridge — to handle multiple concurrent
agents without cross-contamination. Drawbridge is the concrete
component that implements Agency's unified-routing contract;
this mission is what makes it multi-agent-safe.

A single-agent Drawbridge is easy. A multi-agent Drawbridge has
to handle agent identity propagation, session boundary
enforcement, and scoping of per-agent transport state. This
mission is that work.

Registry: `:active`. Core component of futon3c's coordination
layer.

[← Agency Unified Routing](agency-unified-routing)

---

*Size-7 leaf. The cluster's internal structure is clear: three
surface-specific missions (IRC, CLI, CIDER/REPL), one thesis
mission (REPL wins over CLI), and three routing-machinery
missions (Agency rebuild, unified routing, Drawbridge). Together
they describe how agents are **reached** across the stack's
invocation surfaces.*

---

## On Being Done

*Source: `~/code/futon5a/holes/stories/leaf-6-5-3.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-5-3` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Two missions cluster here on an unusually specific concern:
**what does it mean for a mission to be done?** Success criteria,
exit conditions, partial completion, owner handoff. This is the
cluster where the stack asks itself whether its own mission-
completion contract is meaningful.

[Operational Readiness](operational-readiness)  
[Pattern Inference Engine Scoping Review](pattern-inference-engine-scoping-review)

---

### Operational Readiness | operational-readiness

`operational-readiness@futon3c` is the mission to define, and
enforce, **what "operationally ready" means** for stack
components. Too many components in the stack have been declared
ready without a specific criterion; this mission is the
corrective.

The mission's articulation focuses on observable readiness:
healthcheck endpoints responding correctly, canonical integration
tests passing, evidence of use under realistic load. Without such
criteria, "ready" drifts into "somebody said so."

Registry: `:active`. Closure would mean a readiness contract
that any stack component can be measured against — not a
checklist but a rule.

[← Overview](overview) · [Pattern Inference Scoping Review →](pattern-inference-engine-scoping-review)

---

### Pattern Inference Engine Scoping Review | pattern-inference-engine-scoping-review

`pattern-inference-engine-scoping-review@futon3` is the companion
to the pattern-inference-engine mission itself (leaf-6-4-1). A
scoping review exists because the engine's scope was unclear, and
proceeding without scoping discipline was producing partial work
that nobody could declare done.

The review's explicit concern is the success-criteria question:
what would it mean for the pattern-inference engine to be
operationally complete? Without an answer, work on the engine
continues indefinitely with no terminal state. This mission is
the attempt to produce the answer.

Registry: `:active`. This is a meta-mission — a mission *about
another mission's shape*. The stack has a handful of these, and
they are often the missions that close first because their scope
is naturally smaller.

[← Operational Readiness](operational-readiness)

---

*Size-2 leaf. "Being done" is a surprisingly unresolved question
in the stack; this cluster is where the question is named as a
first-class concern. Expected to fold in more missions as other
"X scoping review" missions arrive (there is a futon1a-rebuild
scoping review elsewhere in leaf-6-3).*

---

## Codex Under Enforcement

*Source: `~/code/futon5a/holes/stories/leaf-6-5-4.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-5-4` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

A singleton leaf about **making Codex behave**. The mission sits
alone because its concern — the specific runtime discipline
required when Codex is the agent actually driving work through
the stack — is distinct from the other Codex-adjacent missions
(parity, invocation, handoff) that cluster elsewhere.

[Codex Agent Behaviour](codex-agent-behaviour)

---

### Codex Agent Behaviour | codex-agent-behaviour

`codex-agent-behaviour@futon3c` names the class of problems that
show up only when Codex is the primary implementer: invocation
reliability under load, retry semantics on transient errors,
surface-contract adherence (i.e. does Codex actually respect the
"your output will be posted to IRC as X" scoping), and failure
modes that don't show up for Claude because Claude is hitting a
different surface with different rate-limits and a different tool
vocabulary.

The mission is the stack's bet that **enforcement-through-
peripheral** — rather than through prompt-engineering of the
agent — is the right level to fix these. If Codex misbehaves on
invocation, the fix is a peripheral that structures the
invocation, not a longer system prompt. If Codex ignores surface
context, the fix is a surface-contract check at the peripheral
boundary that rejects out-of-contract output.

This is the dual to the Claude-side discipline: both agents need
the same peripherals, the peripherals enforce the same contracts,
and the agent-specific quirks become parameters of the
peripheral rather than prose in the agent's mouth.

Articulation status: the mission is scoped and named; specific
enforcement checks are not yet fully enumerated. Relates to the
Codex/OpenAI cohort (leaf-6-3) — where that leaf is about Codex
as collaborator, this one is about Codex as agent under
discipline.

[← Overview](overview)

---

*Singleton leaf. If additional agent-behaviour missions appear
(Claude-agent-behaviour, Gemini-agent-behaviour, etc.), this
becomes an "agent enforcement" cluster.*

---

## Making Agency Work Properly

*Source: `~/code/futon5a/holes/stories/leaf-6-5-5.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/6-5-5` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

A singleton with an unusually honest name. Most missions announce
what they will build; this one announces that what exists doesn't
yet work properly and the mission is to fix that. Its singleton
status is diagnostic: when a mission has to be titled "make X work
properly," it is not like the others.

[Make Agency Work Properly](make-agency-work-properly)

---

### Make Agency Work Properly | make-agency-work-properly

`make-agency-work-properly@futon3` is a scoped cleanup mission —
Agency (the multi-agent routing layer) has known functional gaps
that accumulated during the futon3 → futon3c refactor, and the
mission is to close them so that Agency behaves the way
documentation says it does. Not a redesign. A **reconciliation**.

The mission's existence is a small admission the stack is willing
to make: Agency's current behaviour and Agency's documented
behaviour disagree, and the disagreement is specific enough to
close as one mission rather than distribute across several.
Honest naming is cheap; the mission's value is that it is named at
all.

Registry status: `:active`. Mission-doc articulation is moderate —
the "work properly" framing does some work without specifying
which discrepancies count. The closures are per-discrepancy;
each one that lands moves the mission toward done. This is a
mission that probably closes quietly, piece by piece, rather than
in a visible capstone moment.

Relates closely to the IRC/CLI cluster (leaf-6-5-2) and the
coordination-rewrite pair (leaf-6-5-0) — those are structural
changes; this is reconciliation under the structural changes.

[← Overview](overview)

---

*Singleton leaf. Making-X-work-properly is a mission archetype
that might repeat; if it does, it becomes a "reconciliation"
cluster — missions that close the gap between what the stack says
and what the stack does.*

---

## Practice Landing

*Source: `~/code/futon5a/holes/stories/leaf-7.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/7` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

One mission, alone in its cluster, and that aloneness is the
cluster's point. `landing-practice@futon5a` is the mission for
landing the stack — or parts of the stack — into **external**
projects. Nothing else in the current mission set is about
commercial landing; that is why this mission sits by itself in
its own singleton leaf.

[The Landing Practice Mission](landing-practice)

---

### The Landing Practice Mission | landing-practice

`landing-practice@futon5a` names the stack's stance on commercial
and collaborative landings — how components of the stack (pattern
library, evidence architecture, AIF machinery, self-representing
infrastructure, landscape intelligence) move out of Joe's personal
development terrain and into an external project that pays for
them, uses them, or commits to them.

The mission's articulation is deliberately generic — "external
projects" rather than a specific named client or partner — because
landing is itself a practice the stack is developing, not a fixed
contract. Specific named opportunities (Winterstein/AIQA,
Orcro/Andrew, Juxt landscape-intel work) are tracked elsewhere;
landing-practice is the meta-mission that says **how** such
opportunities get attached to stack-side capability and how
evidence flows back.

This is S4's home (**Commercial Demand Exists Adjacent**). A3's
counter (the stack has begun producing non-technical value-prop
artefacts) is being tested here. If a landing actually happens on
reasonable terms, S4 strengthens and A3's counter lands. If
landings keep not happening, A3 reasserts.

Registry status: `:active`. Articulation: thin by design; concrete
landing opportunities are captured as separate missions or pitch
docs. Related reading: `leaf-argument.md`'s A3 Commercialisation
scene, and the orientation document `three-pillars.md` (the stack's
first non-technical artefact aimed at an external reader).

[← Overview](overview)

---

*Singleton leaf — and likely to remain so for a while. Landing
practice is one mission; specific landings are either their own
missions (successful) or their own pitches (prospective). If
multiple landings run concurrently with their own state machines,
this cluster grows.*

---

## Active Fronts — Where the Stack Is Moving

*Source: `~/code/futon5a/holes/stories/leaf-active-fronts.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/active-fronts` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

This is a roadmap, but not the linear kind. The futon stack grows
as a **semilattice** — multiple lines of work proceed in parallel,
sometimes branching, sometimes rejoining when two lines touch the
same mission. This file surfaces the current shape of that
growth from the live priority signal.

Four scenes walk the fronts:

- **[Acute fronts](acute)** — leaves with >50% of their missions
  updated in the last 7 days. What's moving right now.
- **[Sustained fronts](sustained)** — leaves with >30% updated in
  the last 30 days (but not acute). Steady work.
- **[Crossings](crossings)** — pairs of active leaves that share
  missions. The rejoin points where lines touch.
- **[Cold leaves](cold)** — leaves with no activity in the last
  30 days. Either done or neglected; either way worth naming.

Regenerate this file via `python3 scripts/generate_active_fronts.py`
— see [regenerate](regenerate).

[Acute fronts →](acute)

---

### Acute Fronts | acute

Leaves with 7-day score ≥ 0.5. These are the fronts
with missions touched in the last week — where attention is
concentrated right now.

- [retrieval / techniques](leaf-6-4-1) — **30d=0.67** *(7d=0.67)* · 2/3 missions updated · last=2026-04-17
- [devmap — futon6](devmap-futon6) — **30d=0.75** *(7d=0.5)* · 3/4 missions updated · last=2026-04-17
- [devmap — futon7](devmap-futon7) — **30d=0.5** *(7d=0.5)* · 1/2 missions updated · last=2026-04-13

[← Overview](overview) · [Sustained fronts →](sustained)

---

### Sustained Fronts | sustained

Leaves with 30-day score ≥ 0.3 (excluding acute).
Steady work — not a surge, but a regular cadence of mission
touches over the last month.

- [war / bulletin](leaf-6-4-5) — **30d=1** · 1/1 missions updated · last=2026-04-12
- [devmap — futon0](devmap-futon0) — **30d=0.33** · 1/3 missions updated · last=2026-04-09

[← Acute fronts](acute) · [Crossings →](crossings)

---

### Crossings | crossings

Pairs of currently-active leaves that share ≥ 2
missions in their coverage. These are the **rejoin points** in the
semilattice — where two lines of development touch the same
concrete work. If you move one, the other feels it.

- [brief / scan](leaf-6-2) ↔ [devmap — futon7](devmap-futon7) — **2** shared: `daily-scan@futon7`, `f7-lead-report@futon7`
- [retrieval / techniques](leaf-6-4-1) ↔ [devmap — futon6](devmap-futon6) — **2** shared: `paper-reverse-morphogenesis@futon6`, `superpod-mark2@futon6`

Reading a crossing as a route: starting from either leaf, the
shared missions are the natural next read; whichever leaf you land
on, the crossing says *the other one cares about this too*.

[← Sustained fronts](sustained) · [Cold leaves →](cold)

---

### Cold Leaves | cold

Leaves with zero 30-day activity and non-zero coverage. Not all
of these are neglected — some are **done** (e.g. deprecated repos
whose missions have moved elsewhere) and some are **waiting**
(blocked on an upstream commit). Either way, naming them keeps
them visible.

- [book-ramsey](leaf-1) — **30d=0** · 0/1 missions updated · last=—
- [practice / external](leaf-7) — **30d=0** · 0/1 missions updated · last=—
- [superpod / ingest](leaf-3) — **30d=0** · 0/4 missions updated · last=2026-02-17
- [coupling / xor](leaf-4) — **30d=0** · 0/5 missions updated · last=2026-02-18
- [devmap — futon5](devmap-futon5) — **30d=0** · 0/7 missions updated · last=2026-02-18
- [exotype / social exotype](leaf-0) — **30d=0** · 0/7 missions updated · last=2026-02-23
- [irc / gate](leaf-5) — **30d=0** · 0/5 missions updated · last=2026-02-23
- [success criteria / success](leaf-6-5-3) — **30d=0** · 0/2 missions updated · last=2026-02-23
- [alfworld / game](leaf-6-1) — **30d=0** · 0/1 missions updated · last=2026-02-24
- [native / plan](leaf-6-0) — **30d=0** · 0/2 missions updated · last=2026-02-27

*Reading cold leaves carefully is its own kind of roadmap move:
a cold leaf that ought to be warm is a sorry waiting to be
noticed.*

[← Crossings](crossings) · [Regenerate →](regenerate)

---

### Regenerate | regenerate

This file is generated from live data. To refresh:

```
cd ~/code/futon5a
python3 scripts/extract_temporal.py       # re-walks git
bb   scripts/ingest_temporal.clj          # updates futon1a
python3 scripts/compute_leaf_priority.py  # recomputes scores
bb   scripts/ingest_priority.clj          # updates futon1a
python3 scripts/generate_active_fronts.py # rewrites this file
```

Each run is idempotent — no cumulative state.

**Thresholds used this generation:**
- Acute: 7d-score ≥ 0.5
- Sustained: 30d-score ≥ 0.3
- Crossing: shared-mission count ≥ 2
- Top-N crossings shown: 8

Pillar leaves (argument / invariants / cycle) are omitted — their
priority is currently deferred because they cover sorries at a
scale rather than missions directly. A later pass will add scale-
aware coverage.

[← Cold leaves](cold) · [Return to Overview](overview)

---

*Generated 2026-04-20 by `scripts/generate_active_fronts.py`.
See `leaf-start-here.md` Path E for the reading-path orientation;
this file is the data surface it points to.*

---

## What the Stack Claims to Be

*Source: `~/code/futon5a/holes/stories/leaf-argument.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/argument` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

Every long-running system eventually has to answer for itself. The
futon stack's answer is a specific piece of work — a **holistic
argument** first written down in March 2026 and now living in the
sorry-registry as thirty-four typed claims. This anthology walks
through the argument's main parts one scene at a time, so a reader
can form their own opinion without reading the original
s-expression.

The argument has four kinds of parts:

- A **thesis** (one sentence).
- Three **pillars** (the Argument, the Invariants, the Missions),
  each with an AIF role (generative model / precision /
  policy).
- **Support claims** S1 through S6 — empirical assertions the
  argument stands on.
- **Attack relations** A1 through A4 — anticipated objections, each
  with a counter.
- **Falsifiability conditions** F1 through F5 — ways the argument
  could be wrong.
- A **generative cycle** of five steps closing the loop from work
  back to argument.

The argument is not a tree of claims. It is a semilattice of
commitments that *could* fail, and in failing, would tell us which
other commitments also broke. Reading it as prose (v2 at
`futon3c/docs/holistic-argument.md`) gives you the narrative.
Reading it as data (v3 at `futon5a/holes/holistic-argument-semilattice.md`)
gives you the living state. This anthology gives you the scene-level
tour.

[The Thesis](thesis) — the one sentence  
[Pillar: the Argument](pillar-argument) — falsifiable model  
[Pillar: the Invariants](pillar-invariants) — structural precision  
[Pillar: the Missions](pillar-missions) — seven-phase policy  
[S1 — Evidence Discipline Works](s1-evidence-discipline)  
[S2 — AIF Framing Is Generative](s2-aif-framing)  
[S3 — Pattern Transfer Is Real](s3-pattern-transfer)  
[S4 — Commercial Demand Exists Adjacent](s4-commercial-demand)  
[S5 — The Reflexive Architecture Is Rare](s5-reflexive-architecture)  
[S6 — Structural Law Crystallizes](s6-structural-law)  
[A1 — Complexity Cost](a1-complexity-cost) (+ counter)  
[A2 — Solo-Developer Bottleneck](a2-solo-developer) (+ counter)  
[A3 — Commercialisation Gap](a3-commercialisation) (+ counter)  
[A4 — Explanation Problem](a4-explanation) (+ counter)  
[F1–F5 — How the Argument Could Be Wrong](falsifiability)  
[The Generative Cycle](generative-cycle) — five steps closing the loop

---

### The Thesis | thesis

The one sentence, from `holistic-argument.sexp`:

> *A software system can participate in its own maintenance if it keeps
> track of what it believes (the Argument), what it's sure of (the
> Invariants), and what it's doing about it (the Missions).*

Three parenthetical names do the structural work. **Believes**, not
"knows" — the argument is held, revisable, falsifiable.
**Sure of**, not "has proven" — the invariants are stronger than
beliefs but not eternal truths; they crystallise from observed
recurrence. **Doing about it** — missions are the metabolism that
converts beliefs and invariants into work and evidence.

The thesis is an `:in-progress` sorry with `:inhabited-ness :active`.
Its closure condition is not "all three pillars reach full
coverage" — it is "the three pillars continue to stand up under
continuous scrutiny." Closure in this sense is perennial, not final.

The argument lives or dies by whether the three pillars actually
hold the claims that rest on them. Most of this anthology is a tour
through those claims.

[← Overview](overview) · [Pillar: the Argument →](pillar-argument)

---

### Pillar — The Argument | pillar-argument

The first pillar names itself. The Argument pillar is the
**falsifiable model of what the system is and why it should exist** —
the generative-model role in Active Inference terms. Its job: be
wrong in specific, inspectable ways when it is wrong, so the stack
can notice and update.

In the sorry-registry, this pillar currently holds five direct
claims (S1 and S5 root in it; S4 cross-cuts it; F1 and F5 falsify
it). Its artefacts are `holistic-argument.md`, the newer
`three-pillars.md`, and as of today the semilattice v3 rewrite you
might have arrived from.

Why put the argument in a pillar rather than in prose alone? Because
a prose argument can quietly drift from the evidence it rests on.
The pillar form — a generative model in AIF terms — forces the
argument to carry its evidence and its falsifiability conditions as
data, not as rhetoric. When the claims become queryable, the
argument becomes a thing the stack can check against itself rather
than a thing it remembers agreeing with.

[← Thesis](thesis) · [Pillar: the Invariants →](pillar-invariants)

---

### Pillar — The Invariants | pillar-invariants

The second pillar is the **precision** of the AIF story: what the
stack is sure of. Machine-readable structural constraints classified
by confidence, implemented in core.logic goals that run against a
live fact-base of current state.

Nine invariant families are currently operational: graph-symmetry,
status-discipline, phase-ordering, required-outputs, existence,
dependency-satisfaction, startup-contracts, layered-error-hierarchy,
authorization-and-identity-discipline. Ten more are candidates
awaiting a promotion criterion (artefact-custody, repo-role-clarity,
human-visible-inspectability, archaeology-control, and more).

This pillar's honest sorry is the promotion criterion itself. A
candidate invariant becomes operational when it has been observed
recurring across at least three domains and its implementation
survives real-world invocation without a bypass. Today that's
folklore; it should be a falsifiable rule. The leaf-invariants
anthology walks through each family; here the point is that
invariants are the stack's *precision weights* — they say how much
belief should update when a specific observation disagrees with
prediction.

[← Pillar: the Argument](pillar-argument) · [Pillar: the Missions →](pillar-missions)

---

### Pillar — The Missions | pillar-missions

The third pillar is the **policy** layer — the seven-phase mission
process (IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE →
DOCUMENT) that produces evidence at every step. If the Argument
pillar is "what is being claimed" and the Invariants pillar is
"what cannot bend," the Missions pillar is "what is being done
about it."

The stack has 110 missions tracked by Mission Control, of which 49
are complete and 61 are in flight or queued. Each mission's phase
position is a status; each mission's closure rolls up to whichever
devmap prototype or structural-law family it satisfies. The
evidence emitted by each phase feeds back into the Argument —
closure of a mission is a small update to the belief state.

This is the pillar most of the other anthologies in this collection
actually walk. If you want to see the Missions pillar in narrative
form, read **[The Stack Thinks About Itself](leaf-6-4-4.md)**,
**[Inhabitable Surfaces](leaf-2.md)**, **[The Exotype Move](leaf-0.md)**,
or **[Proof and Problem](leaf-6-4-2.md)** — each is a cluster of
missions under this pillar seen from a different side.

[← Pillar: the Invariants](pillar-invariants) · [S1 →](s1-evidence-discipline)

---

### S1 — Evidence Discipline Works | s1-evidence-discipline

The first support claim. The stack claims that its **evidence
discipline** — the PSR/PUR (Pattern Selection Record / Pattern Use
Record) ritual, the gate pipeline G5 → G0, the evidence entries
stored in futon1a, the portfolio inference loop that *reads*
evidence when selecting next actions — actually produces value.

The evidence the claim cites: 853+ flexiargs in the pattern library;
PSR/PUR records active; gate enforcement operational;
M-futon3x-e2e as a working demonstration; portfolio-inference
reads evidence counts when recommending actions; AIF-heads consult
the evidence landscape in their observation loop.

The claim is currently `:operational` in the inhabited-ness sense
— multiple independent pieces of evidence support it. But it is an
empirical assertion, not a definition. If one were to measure
"time-to-correct-pattern-selection" before and after PSR discipline
and find no improvement, S1 would be weaker. That measurement is
the F1 falsifiability condition; it is `:spec-only` today, meaning
nothing continuously measures it.

S1 stands on the quantity of evidence *produced* but is vulnerable
to measurement *absence*. Reading the holistic-argument-semilattice's
Pillar I section will show you the query behind the claim.

[← Pillar: the Missions](pillar-missions) · [S2 →](s2-aif-framing)

---

### S2 — AIF Framing Is Generative | s2-aif-framing

The second support claim: that **Active Inference** is not decorative
vocabulary but actually *generates* work that would not have been
generated otherwise. If AIF were just a borrowed name, the stack
would run the same way with the name removed. The claim is that
removing the name would remove specific machinery.

The evidence: four independent scales where AIF demonstrably runs —
(1) ant agents in futon2 (200-tick grid simulations), (2) portfolio
management in futon3c (15 channels, Bayesian belief update, EFE
action selection), (3) mission-peripheral in futon3c (10 channels,
three-tier), (4) futonzero in futon0 (capability observation,
in-progress). The AifHead protocol — a composable five-method
interface — is the through-line, cited as backing for 8 decisions
across 19 patterns in M-aif-head's ARGUE phase.

S2 is `:operational`. If you want to see the machinery concretely,
the M-aif-head mission's artefacts (futon2 aif/head.clj, its tests,
its VERIFY phase) are the backing. S2 would be weakened by F3 (the
cycle doesn't close — evidence never feeds back). The cycle closing
is the next scene's S-claim indirectly, and the generative-cycle
scene's direct subject.

[← S1](s1-evidence-discipline) · [S3 →](s3-pattern-transfer)

---

### S3 — Pattern Transfer Is Real | s3-pattern-transfer

The third support claim: that patterns **transfer across domains**
— that a pattern developed in one corner of the stack actually
applies in another, with structural fidelity, not just by
name-matching. If patterns didn't transfer, the library would be
853 local recipes. The claim is that many of them are not local.

The evidence: metaca-to-ants (wiring-diagram patterns transfer
between symbolic and embodied domains); math-informal patterns
(apply to software reasoning); realtime (13 coordination patterns
apply to any multi-agent system); mission methodology (the same
seven-phase structure used for AIF heads, Ramsey theory,
documentation).

This claim is the one most at risk of being decorative. "Transfer"
is not the same as "both things happen to use the same word." The
test is whether applying a pattern to a new domain produces work
that would be different if the pattern weren't applied, in ways
the reviewer can point at. For the four cited transfers, that test
has been passed at least once. Whether it generalises as the
library grows is what F2 and F5 together would falsify (laws don't
prevent real failures; complexity grows faster than capability).

[← S2](s2-aif-framing) · [S4 →](s4-commercial-demand)

---

### S4 — Commercial Demand Exists Adjacent | s4-commercial-demand

The fourth support claim is the most load-bearing for whether the
stack can *survive as a commercial project* rather than only as a
research artefact. It claims that **commercial demand exists in
adjacent spaces** — knowledge graphs, multi-agent coordination,
pattern/rule systems, formal methods — not that the futon stack is
itself currently a product, but that the territory it sits in is
one people pay for.

The evidence is thinner than the other S-claims. The F7 lead-report
work (futon7/M-f7-lead-report) catalogues landscape intelligence.
Four markets are named without deep quantification. This is the
claim that most needs continuous external evidence — interviews,
pilots, paid engagements — and where the stack has least direct
data.

S4 is the one you should most distrust on the strength of the
holistic-argument alone. Read Pillar I section in the
semilattice doc for its current status; watch for whether S4's
evidence list grows over time. An unupdated S4 is the argument's
weakest load-bearing piece.

[← S3](s3-pattern-transfer) · [S5 →](s5-reflexive-architecture)

---

### S5 — The Reflexive Architecture Is Rare | s5-reflexive-architecture

The fifth support claim: the *architecture itself* — a software
system that keeps track of what it believes, what it's sure of, and
what it's doing about it — is **rare** enough to be notable.

Evidence: the three-pillars document itself ("explains without AIF
theory"); M-aif-head's VERIFY phase applied AIF+ to its own
structure (self-verification); and the quiet claim that the
holistic argument is the system *arguing for itself*. The document
you're reading right now — an anthology scene about the support
claim that names this kind of reflexive document — is itself
evidence for S5.

S5 is `:operational`. It is also the claim that most invites
skepticism-by-default: "surely other systems do this." The burden
of proof shifts to external comparison. The fact that no
comparable document has yet been produced for a comparable stack
is weak evidence but not no evidence; F4 (no one else can use it)
would be the strong falsifier.

S5's closure is perennial too. Every reflexive document extends
S5; every failure of someone else to use the stack weakens it.

[← S4](s4-commercial-demand) · [S6 →](s6-structural-law)

---

### S6 — Structural Law Crystallizes | s6-structural-law

The sixth support claim, added after the original five. S6 says
that **structural constraints crystallize into enforceable law** —
that patterns observed recurring across domains eventually take
machine-checkable form, and that the stack can refuse operations
that would violate such crystallized law.

Evidence: 9 operational invariant families (listed in the
Pillar-Invariants scene); 8 candidate families at the last count,
now 10; a specific validate-phase-advance function consults
AifHead.check-law when Joe-initiated transitions are proposed, and
can refuse. C9 was itself produced by a mission whose output
became its own enforcement — "self-enforcing," cited in the
evidence list.

The honest caveat: **law-vs-implementation-maturity**, a
classification note in the structural-law inventory, says a law
can be conceptually real at the pattern level even when its
implementations are partial. Treat this claim as: *the stack has
begun crystallizing law; it has not finished*. S6 is
`:operational` because the crystallization process is demonstrably
running, not because crystallization is done.

[← S5](s5-reflexive-architecture) · [A1 →](a1-complexity-cost)

---

### A1 — Complexity Cost | a1-complexity-cost

The first attack: the futon stack's complexity is so large that its
*cost exceeds its yield*. Stated directly: six or seven repos, 853
patterns, 100+ missions, multiple peripheral types, an AIF loop —
all of this is intricate enough that maintenance swamps progress.
A1 targets S1 (evidence discipline) and S6 (structural law).

The counter the argument carries: the three-pillar structure itself
is complexity-management infrastructure. Portfolio Inference ranks
by Expected Free Energy precisely because it is a complexity-load
manager. M-cyder is in progress as a peripheral inspector to
further surface complexity. About thirty completed missions
represent *finished* work, not just evidence of accumulation.

The counter is currently `:active` — it remains a real response, but
it is not immune to continued challenge. If EFE-ranked action
recommendations stopped tracking what Joe actually found valuable,
A1 would reassert. The counter stands on the continuous operation
of the machinery it cites, not on an argument about their
existence in principle.

[← S6](s6-structural-law) · [A2 →](a2-solo-developer)

---

### A2 — Solo-Developer Bottleneck | a2-solo-developer

The second attack: the stack depends on Joe, and Joe alone, to a
degree that makes it fragile. If Joe stops, the stack stops. A2
targets S5 (the architecture is rare) — because a rare architecture
nobody else can enter is just an unusual solo project.

The counter: **M-aif-head in two hours, Joe plus Claude.** The
mission methodology makes agent contribution *structurally*
possible through scoped handoffs. The bottleneck, so the counter
goes, is narrowing from "only Joe" to "Joe sets direction; agents
execute."

A2's counter is `:active` — it cites one concrete case (M-aif-head)
and one structural claim (methodology enables handoff). Whether it
holds depends on whether additional missions succeed with
comparable Joe-time-per-output ratios, and on whether human
contributors besides Joe ever come inside. The Inhabitable
Surfaces anthology's WebArxana scene is directly relevant: if
contributors cannot walk into a URL, A2 reasserts.

[← A1](a1-complexity-cost) · [A3 →](a3-commercialisation)

---

### A3 — Commercialisation Gap | a3-commercialisation

The third attack: **nobody pays for this.** A2 was about human
capacity to enter the stack; A3 is about money. If the stack cannot
turn into an engagement that funds itself within some reasonable
horizon, the stack is a research object, not a sustainable project.
A3 targets S4.

The counter is leaner than A1's and A2's. Landscape intelligence
itself is commercially viable (futon7's work). Pattern discipline
plus evidence architecture positions the stack as enterprise
developer tooling. The three-pillars document is *the first
non-technical value-prop artefact* — that is, the stack has begun
producing material that a non-specialist reader can act on.

A3's counter is honestly `:active` but weaker than A1's and A2's.
It cites positioning, not revenue. Until pilots or paid engagements
exist, the counter is a claim about potential, not track record.
Worth reading alongside the **[Inhabitable Surfaces](leaf-2.md)**
anthology's WebArxana scene and the landing-practice mission
(leaf-7 "practice / landing", forthcoming).

[← A2](a2-solo-developer) · [A4 →](a4-explanation)

---

### A4 — Explanation Problem | a4-explanation

The fourth attack: the stack is unexplainable. A2 was about capacity
to enter; A3 was about capacity to fund. A4 is about *capacity to
communicate*. If nobody can explain the stack to a new audience
without weeks of onboarding, then S1 (evidence discipline works —
for whom?) and S5 (the reflexive architecture is rare — and
unshared?) both weaken.

The counter: the three-pillars document explains the architecture
**without AIF or CS jargon**. The mission landscape appendix shows
concrete work, not theory. M-aif-head links to public examples.
The blank-wall problem, the counter claims, is no longer literally
a blank wall.

A4's counter is `:active`. It is the counter most directly in the
reader's hands: the test is whether *you*, reading this anthology
(which is itself an explanation attempt), now have a handle on the
stack. If yes, A4 weakens. If no, A4 reasserts for your case.

This reader-dependence is not a defect. It is the argument asking
you whether it has explained itself.

[← A3](a3-commercialisation) · [Falsifiability →](falsifiability)

---

### Falsifiability — F1 through F5 | falsifiability

Any argument that cannot be wrong is not an argument. The holistic
argument carries five falsifiability conditions, each stating a
specific observation that would refute specific S-claims:

- **F1** — *Evidence discipline produces no measurable benefit* →
  would refute S1.
- **F2** — *Structural laws don't prevent real failures* → would
  refute S6.
- **F3** — *The cycle doesn't close — evidence never feeds back* →
  would refute S2 and S5.
- **F4** — *No one else can use it* → would refute S5.
- **F5** — *Complexity grows faster than capability* → would refute
  S1 and S6 together.

Each F-condition is currently `:spec-only` in the inhabited-ness
sense. They are *declared*, not *continuously monitored*. This is
a concrete structural debt — the argument announces its own
falsifiability but does not act like an argument watching for its
own defeat. A falsifiability-monitor peripheral (not yet built)
would convert these conditions from spec to operational.

Until then, the falsifiability conditions are promises. They become
load-bearing only when continuous monitoring is wired up — the
F-conditions should be computable, not declarative.

[← A4](a4-explanation) · [Generative Cycle →](generative-cycle)

---

### The Generative Cycle | generative-cycle

The five steps close the loop from work back to argument:

1. **Work → Proof paths.** The gate pipeline G5 → G0 produces
   auditable evidence at every phase. This step is `:operational`.
2. **Proof → Patterns.** The L1 canonicalizer and the Baldwin cycle
   distill recurrences from evidence into the 853+ library.
   `:operational`.
3. **Patterns → Coordination.** PSR selects a pattern before
   acting; PUR records the outcome; Portfolio Inference ranks
   candidate actions by EFE. `:operational`.
4. **Coordination → Understanding.** The self-representing stack,
   the three-column navigation, the evidence viewer — these surface
   coordination state as something inspectable. `:operational`.
5. **Understanding → Argument.** This is the step that was
   *formerly missing* and is now explicitly `:closed` — closed by
   `docs/three-pillars.md`, `futon2/src/futon2/aif/head.clj`, and
   the M-aif-head VERIFY phase. The cycle closes.

Step 5 is the registry's cleanest closed case — a sorry with named
closure artefacts. That's the template every closed sorry should
aspire to. The cycle being closed is what makes the argument
self-supporting: the stack's doing-work eventually produces
evidence that refines what the stack says about itself.

[← Falsifiability](falsifiability) · [Return to Overview](overview)

<!-- open-sorrys-footer:pillar-argument -->

*This anthology is pillar-based, not cluster-derived. Every scene
maps to one or more holistic-argument sorries in futon1a. To pivot
into WebArxana, search for e.g. `sorry|holistic-argument|S|S1` or
`sorry|holistic-argument|cycle|step-5`. The per-scene sorry
footers that other leaves carry aren't auto-generated here — the
S/A/F/cycle sorries are themselves the scenes' subjects, not
attachments to them.*

---

## The Cycle That Closes the Loop

*Source: `~/code/futon5a/holes/stories/leaf-cycle.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/cycle` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

The three pillars — Argument, Invariants, Missions — would be
three parallel columns without the cycle that connects them. The
generative cycle is the stack's claim that **what the stack does
eventually refines what the stack says about itself**. Five steps
carry work from the mission metabolism all the way back to the
argument that started it:

1. **Work → Proof paths** — the gate pipeline records every step.
2. **Proof → Patterns** — recurring proof-paths become library
   patterns.
3. **Patterns → Coordination** — patterns inform next-action
   selection.
4. **Coordination → Understanding** — coordination state is
   surfaced as inspectable evidence.
5. **Understanding → Argument** — the evidence refines the
   pillars themselves.

All five steps are currently `:operational`. The fifth — the step
that closes the loop — has been `:closed` with named artefacts.
That closure is why the holistic argument claims the cycle
genuinely cycles, rather than claiming so provisionally.

This anthology walks each step, states its closure evidence, and
marks the places where the operational claim is thin.

[Step 1 — Work → Proof paths](step-1-work-proof)  
[Step 2 — Proof → Patterns](step-2-proof-patterns)  
[Step 3 — Patterns → Coordination](step-3-patterns-coordination)  
[Step 4 — Coordination → Understanding](step-4-coordination-understanding)  
[Step 5 — Understanding → Argument](step-5-understanding-argument)  
[Why the Cycle Matters](why-the-cycle-matters)  
[Where the Cycle Could Still Break](where-the-cycle-could-break)

---

### Step 1 — Work → Proof paths | step-1-work-proof

Every step of mission work traverses the gate pipeline G5 → G0,
and every gate emits a **proof-path event** that is persisted in
the stack's durable journal. The first step of the cycle is:
**nothing becomes history without leaving a proof-path**.

This is `:operational`. The gate pipeline is implemented in
`futon3b/src/futon3/gate/pipeline.clj`; the proof-path store is a
committed substrate. Even *rejected* traversals persist a minimal
proof-path (per the operational structural-law invariant
`rejection-still-persists-proof-path`, leaf-invariants).

The step's failure mode would be silent work — work that happens
without a gate traversal, and therefore without a proof-path. The
stack has structural-law candidates (`interaction-evidence-continuity`,
`task-work-must-traverse-gates`) that aim to close this hole,
but both remain candidates, meaning some ordinary entrypoints
still route around the gates. The operational closure is real;
the stack-wide closure is not.

[← Overview](overview) · [Step 2 →](step-2-proof-patterns)

---

### Step 2 — Proof → Patterns | step-2-proof-patterns

Proof-paths accumulate. When a specific shape of proof-path
recurs across multiple missions or sessions, the L1 canonicalizer
(`futon3b/src/futon3/gate/canon.clj`) is allowed to promote the
recurring shape to a **library pattern**. Canonicalization
requires recurrence, context, and evidence thresholds before new
patterns are admitted — this is the operational invariant
`canonicalization-requires-recurrence-threshold` (leaf-invariants).

This step is `:operational` and, in a sense, load-bearing for the
rest of the cycle: without the canonicalizer, proof-paths are
just events. The canonicalizer is what turns events into reusable
moves. The 853+ flexiargs in the current library are cumulative
output of this step.

The step's failure mode would be canonicalization drift — patterns
being admitted without recurrence, or recurrence being measured
incorrectly. The no-silent-library-overwrite invariant addresses
part of this (existing patterns cannot be clobbered); recurrence-
measurement integrity is structurally protected but not
continuously audited.

[← Step 1](step-1-work-proof) · [Step 3 →](step-3-patterns-coordination)

---

### Step 3 — Patterns → Coordination | step-3-patterns-coordination

The library is only useful if something reads it. Step 3 is the
path from library pattern to next-action selection: the PSR
(Pattern Selection Record) ritual, followed by application,
followed by PUR (Pattern Use Record), with Portfolio Inference
ranking candidate actions by Expected Free Energy across current
mission state.

This step is `:operational`. PSR and PUR records are active;
Portfolio Inference is running; pattern selection demonstrably
consults the library rather than defaulting to recency. S3
(Pattern Transfer Is Real) rests on this step.

The step's weakest link is **human discipline**: PSR/PUR
compliance is operator-dependent in the ordinary CLI path. The
PSR/PUR mesh peripheral (leaf-5) would close this by making the
records automatic rather than voluntary. Until that peripheral
lands, the step works at the agent-driven path but leaks at the
operator-driven path.

[← Step 2](step-2-proof-patterns) · [Step 4 →](step-4-coordination-understanding)

---

### Step 4 — Coordination → Understanding | step-4-coordination-understanding

Coordination state — which missions are active, which patterns
are in flight, which evidence has landed — has to become
**inspectable** for the cycle to close back on the argument.
The self-representing stack infrastructure, the three-column
navigation, the evidence viewer, WebArxana, VSATARCS, Mission
Control — these are the surfaces that carry step 4.

This step is `:operational`. The stack has genuine self-
representation: a reader can open WebArxana and query any
entity, run Mission Control and see the full mission fleet,
open VSATARCS and read the anthology (including this leaf) as
a narrative overlay. The self-representing-stack is not a
slogan; it is a running system.

The step's failure mode is **unreadable state** — information
that is technically inspectable but operationally opaque.
Portfolio Inference's step-count-zero finding (leaf-6-4-4) is
a small worked example: Portfolio Inference runs, but reading
its ranking in a form that tells you *why* it ranked an
action first is harder than reading the ranking itself. The
candidate invariant `human-visible-inspectability`
(leaf-invariants) tracks this as a named concern.

[← Step 3](step-3-patterns-coordination) · [Step 5 →](step-5-understanding-argument)

---

### Step 5 — Understanding → Argument | step-5-understanding-argument

The closing step. Inspectable coordination state feeds back into
the argument — not to rewrite it, but to refine the S-claims,
sharpen the attack counters, and update the inhabited-ness of
each pillar's commitments. Without step 5, the argument is a
static document; with step 5, the argument is a live belief
state.

This step is `:closed`. Named closure artefacts:

- `futon3c/docs/three-pillars.md` — the first non-technical
  framing of the argument, produced from operational evidence
  across missions.
- `futon2/src/futon2/aif/head.clj` — the AifHead protocol that
  lets an AIF-capable subsystem update the overall belief state
  on observation.
- `M-aif-head` VERIFY phase — the mission's VERIFY records the
  closure-confirming evidence run.

Step 5 was the step that was **formerly missing** — the cycle
ran operationally from 1 through 4 but the feedback from
understanding to argument was informal and irregular.
M-aif-head's completion is what moved this step from `:active`
to `:closed`.

This is the registry's cleanest closed-sorry case. A named
closure, with named artefacts, with a named VERIFY run. It is
the template every other closed sorry should aspire to.

[← Step 4](step-4-coordination-understanding) · [Why the Cycle Matters →](why-the-cycle-matters)

---

### Why the Cycle Matters | why-the-cycle-matters

A stack with three good pillars but no cycle is three monuments
in a field. A stack with three good pillars and a running cycle
is an organism — one whose doing-work continuously refines its
own beliefs about what it is and what it should be doing.

The cycle is **what makes the three pillars non-trivial**. Without
the cycle:

- The Argument pillar would be frozen — whatever prose was last
  written is the stack's position.
- The Invariants pillar would be archival — operational
  invariants stay operational, candidates accumulate without
  promotion pressure.
- The Missions pillar would be unreflexive — missions close,
  but what missions are the right ones to attempt next is
  whatever the operator imagines, not what the evidence suggests.

With the cycle, all three pillars respond to observation. The
Argument updates; candidate invariants get pressure toward
promotion or demotion; the mission fleet is selected by
Portfolio Inference's EFE ranking rather than operator whim.

This is the structural reason the holistic argument is not a
manifesto. A manifesto asserts. An argument in a cycle **listens
for evidence**, and updates.

[← Step 5](step-5-understanding-argument) · [Where the Cycle Could Break →](where-the-cycle-could-break)

---

### Where the Cycle Could Still Break | where-the-cycle-could-break

Five operational steps do not mean five robust steps. Each
step's weakest link is worth naming:

- **Step 1** is robust where gates are actually traversed, but
  the ordinary CLI path still routes around them. Closing the
  `interaction-evidence-continuity` candidate invariant would
  structurally strengthen this step.
- **Step 2** is robust where the canonicalizer runs, but
  recurrence-measurement integrity is not continuously audited.
  A drift-monitor peripheral (not built) would close this.
- **Step 3** is robust on the agent-driven path but operator-
  dependent on the human-driven path. The PSR/PUR mesh
  peripheral (leaf-5) would close this.
- **Step 4** is robust in presence but variable in readability;
  human-visible-inspectability is a named candidate invariant.
- **Step 5** is closed but not frequently re-run. The cycle
  runs; how often it runs at the right depth is under-measured.

The cycle is structurally sound — that is what "all 5 operational,
step 5 closed" means. The cycle's **continuous vigour** — how
often, how deeply, how broadly it runs — is the unmeasured axis.
F3 (**The cycle doesn't close — evidence never feeds back**) is
the falsifiability condition that would refute step 5's closure
if evidence stopped feeding back. Today, F3 is `:spec-only` —
declared but not continuously monitored.

The cycle is alive; it is not yet instrumented for its own
vital signs.

[← Why the Cycle Matters](why-the-cycle-matters) · [Return to Overview](overview)

---

*This anthology is pillar-based, not cluster-derived. Every scene
maps to one or more cycle sorrys in futon1a. To pivot into
WebArxana, search for e.g. `sorry|holistic-argument|cycle|step-1`
through `step-5`. Per-scene footer blocks are omitted here — the
cycle sorrys are themselves the scenes' subjects, not attachments
to them.*

---

## What the Stack Is Sure Of

*Source: `~/code/futon5a/holes/stories/leaf-invariants.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/invariants` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

If the Argument pillar is what the stack **believes** and the
Missions pillar is what the stack **does**, the Invariants pillar
is what the stack is **sure of**. Not axioms — nothing here was
declared by fiat. Each invariant was observed recurring across the
stack enough times that the cost of checking it dropped below the
cost of violating it, at which point it got written down as a
machine-checkable rule.

The result is an inventory with two tiers:

- **9 operational families** — rules actually enforced in running
  code, typically by `core.logic` goals querying a pldb snapshot of
  live state. Violating one of these fails loudly, or fails to run
  at all.
- **10 candidate families** — rules the stack *almost* obeys. The
  invariant has been articulated, the pattern of violation has been
  observed, but no runtime machinery continuously enforces it yet.

The pillar's AIF role is **precision** — the weights that say how
confident the generative model should be when an observation
disagrees with what it expected. An operational invariant is a
high-precision claim: the stack will treat a violation as an error
rather than as new evidence. A candidate is a low-precision claim:
a violation is provisionally taken as surprise to be investigated.

Invariants are why the stack does not drift. They are also how a
solo developer can leave a system for a week and come back to find
it still coherent — not because nothing broke, but because whatever
broke broke loudly, near its cause, and with enough context to fix.

[Pillar: Invariants as Precision](pillar-role) — the AIF story  
[Operational: Graph and Existence](op-graph-existence) — symmetry, existence, dependency  
[Operational: Mission Process](op-mission-process) — status, phase, required outputs  
[Operational: System and Auth](op-system-auth) — startup, error-layering, authorization  
[Candidate: Custody](cand-custody) — atomic units, artefact, repo-role, archaeology, peripheral  
[Candidate: Inspectability](cand-inspectability) — human-visible, interaction-evidence  
[Candidate: Control](cand-control) — failure-locality, budgeted-action, cross-store  
[The Promotion Criterion](promotion-criterion) — candidate → operational  
[How Invariants Close Sorrys](closure-evidence) — 9 closed so far  
[The Case for Structural Law](structural-law-case) — why this shape, why now

---

### Pillar — Invariants as Precision | pillar-role

The Invariants pillar is the stack's **precision** in the
Active Inference sense: how much belief should update when a
specific observation disagrees with prediction.

A high-precision invariant — say `graph-symmetry` — says: "if A
relates to B and the inverse relation is absent, this is a bug,
not new information." The stack will not revise its model of the
world based on the broken relation. It will surface the missing
inverse and stop.

A low-precision claim — say `interaction-evidence-continuity` as a
candidate — says: "we would like every substantive agent turn to
land in the evidence ledger, and when it doesn't we notice, but we
don't currently refuse to run." A violation is recorded as debt,
not blocked as error.

This is a direct structural consequence of what an invariant *is*.
An operational invariant has teeth because some runtime path
actually checks it. A candidate invariant has aspiration because
the recurring violation has been observed often enough to name,
but the machinery has not yet been built.

In Pillar I's S-claim language, the invariants are what make S6
(**Structural Law Crystallizes**) testable. If recurring patterns
cannot be promoted into machine-checkable law over time, S6 fails.
If they can, S6 accumulates evidence. The promotion criterion scene
below is where this pillar's own deepest sorry lives.

Every invariant family in this anthology corresponds to at least
one sorry in the futon1a registry at `:scale :structural-law`.

[← Overview](overview) · [Operational: Graph and Existence →](op-graph-existence)

---

### Operational — Graph Structure and Existence | op-graph-existence

Three of the nine operational families belong together because
they check the *structural shape* of the fact-base rather than the
process that produced it.

- **graph-symmetry** — "if A points to B, is the inverse
  structural relation present?" Implemented in `portfolio/logic.clj`,
  `agents/tickle_logic.clj`, `agency/logic.clj`,
  `peripheral/proof_logic.clj`. Runs continuously against live pldb
  state. The sorry is `:closed` — code on disk implements it.
- **existence** — "do referenced entities actually exist?"
  Referential integrity check across the agency and peripheral
  logic modules. Missing endpoints fail validation before reaching
  durable storage.
- **dependency-satisfaction** — "are completed things backed by
  completed prerequisites?" Guards transitions in portfolio logic
  and the proof-path validator: you cannot mark a mission phase
  complete if its declared predecessors are not also complete.

These three are the stack's **shape police**. They ensure the graph
of current state stays consistent as a graph — no dangling edges,
no orphan references, no premature completion. They are cheap to
run, they catch a large class of bugs that would otherwise
metastasize into dashboard-drift, and they are why the futon1a
fact-base can be trusted as a substrate.

All three are `:closed` against running code. They are also the
families most easily ported: each is a small set of logic goals
over a typed relation, and each was ported from futon3 to futon3c
with minimal friction.

[← Pillar Role](pillar-role) · [Operational: Mission Process →](op-mission-process)

---

### Operational — Mission Process | op-mission-process

Three more operational families govern the **mission metabolism** —
the seven-phase cycle IDENTIFY → MAP → DERIVE → ARGUE → VERIFY →
INSTANTIATE → DOCUMENT (and the adjacent cycle observe → propose →
execute → validate → classify → integrate → commit → gate-review →
completed used by the mission peripheral).

- **status-discipline** — status labels must be legal and
  evidence-consistent. `:done` is reachable only from the right
  predecessor; `:abandoned` is terminal; an `:assertion`-kind of
  evidence record cannot by itself justify a `:done` transition.
  Implemented in tickle logic, agency logic, proof logic, and the
  mission backend.
- **phase-ordering** — states must advance in a valid order. The
  gate pipeline G5 → G0 is the canonical example; mission cycles
  advance only along the declared phase sequence and refuse
  skipping.
- **required-outputs** — each phase produces its mandatory
  artefacts. A VERIFY phase that lacks an evidence artefact cannot
  be marked complete, even if everything else would allow the
  transition.

These three make **mission metabolism legible**. Without them the
stack could assert that a mission was complete without the evidence
record to prove it. With them, the mission's declared state and the
mission's evidence trail cannot silently disagree.

Each family runs as a core.logic goal over the pldb snapshot; each
family has `:closes` edges in the registry pointing to the
`src/futon3c/peripheral/mission_backend.clj` and
`src/futon3c/peripheral/proof_logic.clj` modules. The set of
legal phase transitions is itself declared data (in `mission_shapes.clj`),
not inline code — so the invariant logic can be audited without
reading the procedural pipeline.

[← Operational: Graph and Existence](op-graph-existence) · [Operational: System and Auth →](op-system-auth)

---

### Operational — System and Authorization | op-system-auth

The remaining three operational families are **system-level**:
they govern how the stack boots, how errors propagate, and who is
allowed to write.

- **startup-contracts** — futon1a's `system.clj` requires an
  explicit penholder allowlist at startup and fails loudly when
  policy is underspecified. Internals (raw XTDB node, raw store
  handles) are hidden unless explicitly exposed. Compat penholders
  must be members of the allowed set. *Startup is the place an
  invariant has the most leverage and the lowest cost.*
- **layered-error-hierarchy** — futon1a writes run in strict
  L4 → L3 → L2 → L1 → L0 order, and lower layers do not run when
  higher layers fail. Each layer reports its own error layer,
  status, and context. This is the stack's most surgically-useful
  invariant: when something breaks, you know which layer, usually
  within one function call.
- **authorization-and-identity-discipline** — penholder
  allowlists gate writes; tooling identities pass an explicit
  allowlist; external-identity mappings are canonicalised and
  conflicts rejected; relation endpoints must exist before a
  relation is written.

These three are what makes futon1a *durable* in the substrate sense.
Without them, startup would be whatever policy happened to load
first, errors would surface at the wrong layer (or be silently
eaten), and unauthorised writes would be impossible to rule out.
With them, the substrate has the three properties missing from
most developer-tool substrates at this scale: explicit initial
state, localised failure, and provably-scoped write authority.

All three families have `:closes` edges to real code in `futon1a/`.
These closures were the first wave that moved the overall close-rate
from 7 % to 27 %.

[← Operational: Mission Process](op-mission-process) · [Candidate: Custody →](cand-custody)

---

### Candidate — Custody | cand-custody

Here the tier shifts. The ten candidate families are each a
recurring pattern of violation that the stack has observed often
enough to **name** — but the machinery that would actively enforce
the pattern is still missing or partial. Five of them sit in the
custody cluster:

- **atomic-inspectable-units** — work should happen in bounded,
  inspectable units rather than diffusing across repos, stashes,
  and unowned files. Candidate invariants: `home-repo`,
  `single-live-copy`, `checkout-before-work`, `checkin-on-exit`,
  `inspectable-boundary`.
- **artefact-custody** — outputs should land where the stack
  expects them to live. `artifact-locality`, `scratch-marking`,
  `generated-vs-source-separation`, `narrow-ignore-exceptions`.
- **repo-role-clarity** — a repo should say what kind of thing it
  is, and its root should not contradict that claim.
- **archaeology-control** — latent work (stashes, unclassified
  branches) should not accumulate as invisible operational debt.
- **peripheral-custody** — a peripheral session should carry
  enough structure that work cannot silently drift across domains.

These five are the stack's **anti-entropy candidates**. The stack
observes, repeatedly, that violations of these rules produce
scattered artefacts, lost work, and context drift. The stack has
not yet committed to blocking violations — partly because the
machinery is not yet built, partly because a too-eager enforcement
would refuse ordinary checkpoint work.

The candidate status is exactly the right description. These are
not bugs and not policies: they are recurring pressures the stack
needs to address with a specific enforcement mechanism before
promoting them to operational. The devmap for futon0 is the main
incubator here — its candidate invariants `clean-entry-and-exit`,
`reporting-pipeline-legibility`, and `interactive-turns-evidence-first`
are all family members of this cluster.

[← Operational: System and Auth](op-system-auth) · [Candidate: Inspectability →](cand-inspectability)

---

### Candidate — Inspectability | cand-inspectability

Two candidate families concern **making state legible to humans**
— the gap between "the stack did something" and "the operator can
tell what the stack did."

- **human-visible-inspectability** — state should be visible
  enough that the human operator can tell what is going on without
  folklore. Candidate invariants: `readable-surface`
  (key outputs in human-readable form, not only raw logs),
  `state-projection` (peripheral/mission state projects to
  evidence, blackboards, or reports), `evidence-over-folklore`
  (operationally important claims are queryable, not remembered).
- **interaction-evidence-continuity** — interactive agent work
  should either land in the evidence ledger or incur explicit
  inspectable debt. Candidate invariants:
  `all-turns-logged-as-evidence`,
  `session-anchor-carries-surface-context`,
  `off-surface-turns-create-debt`,
  `evidence-debt-visible-and-bounded`.

These two are the invariant form of Pillar I's S1 (**Evidence
Discipline Works**). S1 claims evidence discipline is valuable;
these candidates specify the *mechanical conditions* under which
the claim becomes load-bearing. If turns are not logged, if
surface context is not carried, if evidence debt is not visible,
then S1 is a slogan rather than a working system.

The second family — `interaction-evidence-continuity` — is the
invariant behind the REPL-wins-over-CLI mission. Each candidate
invariant there names a specific way CLI-based work creates
evidence drift. Until the REPL is strictly better than the CLI,
the debt grows unobserved.

Neither family has runtime enforcement yet. Both have
well-observed violation patterns and named candidate invariants
— so both are ready for promotion as soon as a criterion is
defined.

[← Candidate: Custody](cand-custody) · [Candidate: Control →](cand-control)

---

### Candidate — Control | cand-control

Three candidate families concern **runtime control** — when
actions are allowed, how failures propagate, whether stores that
should agree actually agree.

- **failure-locality** — failures should surface near the layer,
  subsystem, or entry point that caused them, with enough
  structure that diagnosis is quick. Candidate invariants:
  `fail-at-source-layer`, `stop-the-line-ordering`,
  `diagnosis-with-bounded-search`. (futon1a's
  `layered-error-hierarchy` is the operational instance of this
  aspiration; the candidate extends it stack-wide.)
- **budgeted-action-selection** — action selection should be
  constrained by available budget or license, not by priority
  alone. Six candidate invariants: `no-costly-action-without-budget`,
  `priority-within-budget`, `spend-must-be-recorded`,
  `depletion-forces-deferral`, `replenishment-has-evidence`,
  `budget-state-visible`.
- **cross-store-agreement** — when one operational story is
  mirrored across registry, ledger, and announcement stores, the
  mirrors should agree. Four candidate invariants:
  `canonical-ledger-backs-public-claim`, `mirrored-actor-agreement`,
  `session-continuity-alignment`,
  `aggregate-count-backed-by-ledger`.

These are structural in a stricter sense than the custody
candidates: they are not about tidiness but about the conditions
under which the stack can be **trusted** to do the right thing
under load. A stack that cannot localise failures cannot be
diagnosed quickly. A stack that selects actions without budget
awareness eventually overextends. A stack whose mirrored records
disagree will quietly lie about its state.

`failure-locality` is the closest to operational — futon1a has a
foundational implementation; the candidate is the stack-wide
generalisation. `budgeted-action-selection` is the furthest from
operational — it would require a live budget accountant and an
action-gating hook that does not yet exist.

[← Candidate: Inspectability](cand-inspectability) · [The Promotion Criterion →](promotion-criterion)

---

### The Promotion Criterion | promotion-criterion

This is the pillar's honest sorry.

A candidate invariant becomes operational when — when what? The
question is the largest open item in Pillar II.

Today the promotion is folklore. A candidate invariant becomes
operational approximately when:

1. It has been observed recurring across at least three
   independent domains.
2. Its implementation survives ordinary live use without routing
   around it.
3. Joe decides it is time to promote.

Step (3) is the unfalsifiable part. The stack cannot currently
check, automatically, whether (1) and (2) hold. There is no
peripheral that monitors candidate-invariant compliance. There is
no tally of recurrence. There is no evidence that the
implementation resists bypass except the absence of reported
bypass.

`E-candidate-queue-upsampling.md` (three-move excursion in
`futon5a/holes/excursions/`) sketches the machinery that would
automate this — a candidate-queue peripheral that tracks violation
rate and recurrence automatically, surfacing promotion candidates
when they pass a threshold. The excursion has not landed.

**This is Pillar II's largest sorry.** Everything else in the
pillar — the nine operational families' closure evidence, the ten
candidate families' recurring-violation observations — is
contingent on this promotion question being answerable. If the
stack cannot define when "candidate" becomes "operational," then
the distinction is administrative, not structural. And S6
(Structural Law Crystallizes) reduces to "sometimes Joe writes
down a rule."

The sorry is `:spec-only` in inhabited-ness terms: a concept
exists, an excursion exists, no implementation exists.

[← Candidate: Control](cand-control) · [Closure Evidence →](closure-evidence)

---

### How Invariants Close Sorrys | closure-evidence

Nine operational families, nine closures. Each of the nine
operational sorrys has a `:closes` edge in futon1a pointing at the
specific code that implements it:

| Family | Closing code |
|---|---|
| graph-symmetry | `portfolio/logic.clj`, `agents/tickle_logic.clj`, `agency/logic.clj`, `peripheral/proof_logic.clj` |
| status-discipline | `agents/tickle_logic.clj`, `agency/logic.clj`, `peripheral/proof_logic.clj`, `peripheral/mission_backend.clj`, `peripheral/mission_shapes.clj` |
| phase-ordering | `agents/tickle_logic.clj`, `peripheral/proof_logic.clj`, `peripheral/mission_backend.clj` |
| required-outputs | `peripheral/proof_logic.clj`, `peripheral/mission_backend.clj`, `peripheral/mission_shapes.clj` |
| existence | `agency/logic.clj`, `peripheral/proof_logic.clj`, `peripheral/mission_backend.clj` |
| dependency-satisfaction | `portfolio/logic.clj`, `peripheral/proof_logic.clj` |
| startup-contracts | `futon1a/system.clj` |
| layered-error-hierarchy | `futon1a/core/pipeline.clj`, `futon1a/api/errors.clj` |
| authorization-and-identity-discipline | `futon1a/auth/penholder.clj`, `futon1a/core/identity.clj`, `futon1a/core/entity.clj`, `futon1a/core/pipeline.clj` |

This list was what moved the stack's overall close-rate from 7 %
to 27 % in the April 2026 ingest pass. Nine honest closures are
not many in absolute terms, but they are the **cheapest and most
defensible** closures in the registry — because each one is
backed by a live core.logic goal that can be audited.

None of the ten candidate families has a `:closes` edge. That is
correct; a candidate family *is by definition* one that lacks the
code that would close it.

The next closures, in order of likelihood:

- **failure-locality** — futon1a's implementation could become the
  stack-wide closure once ported.
- **interaction-evidence-continuity** — closes only when the REPL
  wins over the CLI (see M-repl-wins-over-cli).
- **human-visible-inspectability** — closes as the evidence-
  landscape viewers come online across repos.

[← The Promotion Criterion](promotion-criterion) · [The Case for Structural Law →](structural-law-case)

---

### The Case for Structural Law | structural-law-case

The pillar rests on a bet. The bet is: **recurring violation
patterns eventually become machine-checkable rules, and the
accumulation of such rules is the substrate of a self-maintaining
system.**

This is S6 (**Structural Law Crystallizes**) in Pillar I. The
inventory in this anthology is the evidence for that bet.

The argument goes: in a system that does not take structural law
seriously, recurring violation patterns are absorbed as
operational folklore — "oh, that always breaks, you just have to
remember to check X before Y." The folklore survives in heads, in
CLAUDE.md files, in scattered docstrings. A solo developer, or a
developer returning to a repo after months, must reconstruct the
folklore before acting safely.

In a system that does take structural law seriously, each
recurring violation eventually becomes a named family, then a
candidate invariant, then an operational core.logic goal. Once an
invariant becomes operational, the folklore disappears: either
the system enforces the rule, or it doesn't; either the tests
pass or they don't. Folklore becomes code becomes history.

The bet is whether this pipeline — **observe recurrence → name
family → articulate candidate → implement enforcement → close
sorry with code** — actually runs. As of April 2026:

- 19 named families exist (9 operational, 10 candidate).
- 9 are `:closed` by code on disk.
- 0 are closed from candidate (the promotion criterion gap).
- The promotion pipeline has an excursion sketch but no running
  machinery.

The pillar is strong on the first half of the pipeline (name →
articulate) and weak on the second half (candidate → operational).
If in twelve months the candidate count is still ten and the
operational count is still nine, S6 will have weakened
substantially. If the candidate count has grown and several have
been promoted, S6 will have strengthened.

The invariants pillar is the stack asking itself — in writing —
whether its own ambition to become a self-maintaining system is
borne out by the rules it has actually crystallised. The
inventory is the receipt.

[← Closure Evidence](closure-evidence) · [Return to Overview](overview)

<!-- open-sorrys-footer:pillar-invariants -->

*This anthology is pillar-based, not cluster-derived. Every scene
maps to one or more structural-law sorrys in futon1a. To pivot
into WebArxana, search for e.g. `sorry|structural-law|operational|graph-symmetry`,
`sorry|structural-law|candidate|interaction-evidence-continuity`,
or the single large sorry `sorry|holistic-argument|pillar|invariants`.
Per-scene footer blocks are omitted here — the structural-law
sorrys are themselves the scenes' subjects, not attachments to
them.*

---

## The R-Criteria Walkthrough — Both Sides of the Bridge

*Source: `~/code/futon5a/holes/stories/leaf-wm-r-criteria-walkthrough.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/leaf/wm-r-criteria-walkthrough` (kind `:leaf`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

The R-criteria are twelve named checks a reader can grade an AIF
apparatus against. They are not metrics — they are *satisfaction
predicates* the apparatus either meets or honestly defers. The
contract lives in two places now: claude-2's `futon-aif-completeness.md`
grades the War Machine; claude-4's `vsatarcs-alignment-completeness.md`
grades the VSATARCs reader-side surface. Both sides shipped enough
of the contract by 2026-05-20 that the cross-side bridge can
compare apparatuses on shared entity ids in real time.

This walkthrough takes the criteria in order, says what each
means, what each side did to satisfy it, and what's outstanding.
R11 and R12 sit at the boundary — they ask about composition and
about the apparatus reasoning about itself — and they are deferred
deliberately, not by oversight.

- [R1 — Explicit belief state](r1-belief)
- [R2 — Observation channels](r2-observation)
- [R3 — Predictive-coding update](r3-update)
- [R4-R6 — The writer-capability stack](r4-r6-writer)
- [R7 — Adaptive precision](r7-precision)
- [R8 — Per-tick trace](r8-trace)
- [R9 — Named validation properties](r9-validation)
- [R10 — Live operation](r10-live)
- [R11 and R12 — The boundary criteria](r11-r12-boundary)
- [The bridge as operational consequence](bridge)

---

### R1 — Explicit belief state | r1-belief

The first criterion asks: does the apparatus maintain a probability
distribution over hidden state, carried across ticks, with both
mean and precision explicitly represented? On the WM side, R1
landed at v0.2 — `futon2.aif.belief` ships a multiplicative-likelihood
posterior over the seven-status M-INC vocabulary (`:spawned`,
`:refined`, `:strengthened`, `:addressed`, `:falsified`,
`:foreclosed`, `:reopened`). The posterior is stored per-entity and
updated by every judge call.

VSATARCs implements the same shape against the same status set —
`arxana-vsatarcs-belief.el` — so the two sides can compare posteriors
on shared entity ids. As of v0.2.5 the cross-side bridge fetches WM
belief from the WM trace file's `:mu-post` field and runs
`arxana-vsatarcs-belief-compare`; baseline smoke 2026-05-18 was 35
shared entities, 0 drift, with the WM-side meta-sorry
(`:sorry/wm-aif-substrate-addressability`) filtered as expected
`:only-in-wm-side`.

R1 is the criterion the rest of the contract leans on. Without it
no other R-criterion has a stable subject to discuss.

[← Overview](overview) · [R2 — Observation channels](r2-observation)

---

### R2 — Observation channel schema | r2-observation

The second criterion asks: does the apparatus sense the world
through a fixed, named set of bounded observation channels? WM
side ships fourteen channels over stack-fitness:
`:annotation-health`, `:sorry-count-norm`, `:mission-health`,
`:active-repo-ratio`, and ten more (the remaining ten currently
logged as `:prototyping-forward` sorries — see
`~/code/futon2/data/sorrys.edn`). All channels are `[0,1]`-bounded
and read from observable substrate (the sorry registry, mission
docs, repo activity).

VSATARCs ships five channels over the essay corpus —
`:story-coverage`, `:lift-freshness`, `:annotation-overlay-presence`,
`:scene-density`, `:link-density`. The first three carry shipped
weight tables and per-channel likelihood; the last two are
`:prototyping-forward` because they are structural corpus facts
without natural per-entity-status mapping.

The two sides observe **different things**: WM watches stack
fitness; VSATARCs watches essay corpus. The R-criterion shape is
satisfied identically on both sides; the bilateral evidence entry
under `:independent-naming-of-same-r-criterion-shape-at-different-scopes`
records the structural symmetry without claiming content
comparability.

[← R1 — Belief state](r1-belief) · [R3 — Update](r3-update)

---

### R3 — Predictive-coding belief update | r3-update

R3 asks: does the apparatus update belief from observation using
the standard predictive-coding shape — likelihood, prediction
error, precision-weighted update? It decomposes into four
sub-properties (R3a-R3d) and the closure protocol requires each to
satisfy independently before the aggregate closes. As of WM v0.11
all four sub-properties satisfy on the WM side for the four
R3a-covered channels (`:annotation-health`, `:sorry-count-norm`,
`:mission-health`, `:active-repo-ratio`); the other ten channels
are logged as `:sorry/r3a-likelihood-<channel>` prototyping-forward
sorries.

VSATARCs ships R3a + R3c via `arxana-vsatarcs-likelihood.el` (port
of WM's v0.10 likelihood + v0.13 multi-step + v0.16
sign-aggregation; three of five R2 channels covered). R3b was
initially blocked on R7; cascaded to satisfied when VSATARCs
shipped its own R7 port (v0.5.3). R3 aggregate flips to satisfied
on the VSATARCs side via the closure protocol — first composite
R-criterion to fully close all four sub-properties.

The asymmetry — WM covers 4 of 14 channels; VSATARCs covers 3 of 5
— is honest by design; both sides ship the same sub-property
discipline at different scopes. The cross-side bridge compares on
shared entity ids regardless of channel-set differences.

[← R2 — Observation](r2-observation) · [R4-R6 — Writer-capability](r4-r6-writer)

---

### R4-R6 — The writer-capability stack | r4-r6-writer

Three criteria form one logical bundle: R4 asks for a predictive
forward model (what would happen if the agent acted?), R5 asks for
an EFE composition with at least two principled terms over actions,
R6 asks for softmax action selection with abstain. On the WM side
all three landed by v0.5 — `futon2.aif.forward-model`,
`futon2.aif.efe`, `futon2.aif.policy`. The WM is an action-proposer:
each judge call ranks actions by Expected Free Energy and either
recommends one (the rank-1 action above an abstain threshold) or
abstains with a gap-report.

VSATARCs has these criteria `:deferred-pending-writer-capability`
because VSATARCs has historically been a reader — it renders chrome
over the WM trace plus canonical sources, but does not act. The
M-vsatarcs-writer mission (claude-2 INSTANTIATE 2026-05-20) closes
this gap on the VSATARCs side. L3 ships `:mission-doc-sync` as the
first action-class; L4 ships `:aif-edn-revision-entry` as the
second. The consent-gate-substitutability promise — that supervised
operator-confirms can swap to autopen at one location once safety
is validated — was tested directly by L4's recursive-self-landing
case (the action class proposing an entry that documents its own
landing; the apparatus terminates via the same admissibility
predicate the operator-confirm path uses).

When M-vsatarcs-writer's writer-capability stack stabilises through
medium-blast-radius classes (`:xtdb-hyperedge-emit` is the next
candidate), VSATARCs's R4-R6 stop being deferred. R9 EFE-stress and
Abstain-fires properties become testable at that point — they need
an action space to exercise.

[← R3 — Update](r3-update) · [R7 — Precision](r7-precision)

---

### R7 — Adaptive precision | r7-precision

R7 asks: does the apparatus adapt its per-channel precision based
on observation history? Both sides ship the same architecture (WM
v0.12 + v0.13; VSATARCs v0.5.3 direct port): precision is computed
as variance-component plus need-component, bounded by floor and
cap, with a rolling per-channel error window. WM ships the
need-component live; VSATARCs's need-component is 0.0 today because
the need-modulation requires R5 preferences (deferred per the
writer-capability stack).

The discipline emerging here — *the trace IS the state store* —
shipped on both sides in May 2026 and is now a load-bearing pattern.
No separate state file lives anywhere else; the trace is the
canonical store for all cross-call state (per-channel precision,
per-entity belief, prediction-errors, micro-step convergence).
Cold-start reconstructibility from disk is the property this
guarantees; see `feedback_reload_safety` memory for the framing.

[← R4-R6 — Writer](r4-r6-writer) · [R8 — Trace](r8-trace)

---

### R8 — Per-tick trace | r8-trace

The eighth criterion asks: does each apparatus tick emit a trace
record carrying enough state to reconstruct cross-call dynamics?
WM trace ships as EDN-lines daily-rotated files at
`~/code/futon2/data/wm-trace/wm-trace-YYYY-MM-DD.edn` (v0.7 first
landing). Each record carries `:timestamp`, `:mu-pre`, `:mu-post`,
`:observation`, `:free-energy`, `:prediction-errors`,
`:precision-state`, `:micro-step-trace`, `:anticipation`,
`:ranked-actions`, `:decision`, `:mode`.

VSATARCs ships its own trace at `~/code/futon4/data/vsatarcs-trace/`
with the same EDN-lines discipline plus a `:wm-trace-anchor :line-index`
field so `follow-wm` is idempotent — each WM record gets one
VSATARCs record at the same `:timestamp`. The cadence-following
shape — VSATARCs ticks driven by WM ticks rather than wall-clock —
is the design directive Joe surfaced 2026-05-18: "make it
event-source-agnostic to start with — useful for testing. One use
case: docs always in line with code."

The first operational observation of non-zero cross-side belief
drift was recorded 2026-05-18 (max-posterior-diff 0.0045 on 35
entities) when VSATARCs followed WM through a session. Drift below
threshold today; non-zero is the substantive signal — apparatus
behaviour visible cross-side at trace granularity.

[← R7 — Precision](r7-precision) · [R9 — Validation](r9-validation)

---

### R9 — Named validation properties | r9-validation

R9 asks: does the apparatus carry NAMED validation properties that
can be tested as such (V-shrink, F-decrease, EFE-stress,
Abstain-fires)? WM ships all four at v0.7 — the validation harness
runs against trace records and asserts quantitative bounds.

VSATARCs ships V-shrink (since v0.1) and F-decrease (since v0.5.1).
EFE-stress and Abstain-fires are blocked on R5 preferences + R6
softmax — both `:deferred-pending-writer-capability` on the
VSATARCs side. When M-vsatarcs-writer's writer-capability stack
stabilises, R9 closes fully on the VSATARCs side too.

R9 is the criterion that turns the apparatus from
"hopefully-working" to "demonstrably-working." Without named
properties the validation harness has nothing to assert.

[← R8 — Trace](r8-trace) · [R10 — Live operation](r10-live)

---

### R10 — Live operation | r10-live

The tenth criterion asks: does the AIF loop run on a recurring
schedule without per-cycle operator intervention? Both sides
satisfy R10, asymmetrically. WM is `:scheduled-execution-ready` —
the `clojure -M:wm-scheduled` entrypoint exists and emits trace
records on every invocation; cron-install is pending operator
action but the apparatus is one shell command from production.

VSATARCs satisfies R10 differently: file-notify on the WM trace
directory fires `arxana-vsatarcs-trace-follow-wm` whenever a new
WM record lands. Operator opens VSATARCs once and the
subscription stays active — no per-cycle intervention. After
v0.5.18's substrate redirect, the wider engagement-time surface
(mission edits, code edits, git commits) flows through futon1a
XTDB rather than parallel file-watches: the
`arxana-vsatarcs-xtdb-clicks` module queries the canonical
hyperedge substrate that multi_watcher already populates. v0.5.19
parallelised those queries so chrome render is bounded by the
slowest stream's timeout regardless of stream count.

The asymmetry — WM scheduled-but-not-cron-installed; VSATARCs
subscribed-and-running — is itself a signal. Both are honest R10
satisfactions, different shapes. The bridge eventually compares
cadences across the two sides.

[← R9 — Validation](r9-validation) · [R11 and R12 — Boundary](r11-r12-boundary)

---

### R11 and R12 — The boundary criteria | r11-r12-boundary

R11 asks about hierarchical or multi-agent composition — when
multiple AIF agents act on shared state, is there a coordination
layer? Per the v0.5.7 audit-row reframe, this is now satisfied at
the observer layer: WM and VSATARCs each run their own R1-R12
stacks; both bootstrap from the same canonical entity domain
(`stack-annotations.edn :sections[]`); the bridge reads cross-side
state via WM trace; the `:bilateral-evidence` block (19 entries as
of v0.5.22; six evidence-kinds activated) is the first-class
coordination audit trail. The action-coherence sub-claim is
vacuously satisfied until both sides have writer capability and
can be observed acting on shared state simultaneously.

R12 asks about dual-loop hyperparameter inference — the apparatus
reasoning about its own hyperparameters. This is deferred at stack
scope to `M-the-futon-stack` Q6, not as a local-engineering gap.
R12 presupposes the writer-capability stack and the meta-loop
substrate that comes with it. Honestly deferred on both sides.

The boundary criteria are not gaps — they are where the contract
admits that some questions only make sense at scales the apparatus
hasn't yet reached. R11 satisfies because the cluster has reached
the multi-agent scale; R12 defers because the meta-loop scale is
still ahead.

[← R10 — Live](r10-live) · [The bridge as operational consequence](bridge)

---

### The bridge as operational consequence | bridge

The R-criteria are not a checklist completed independently on each
side. They are a *shared* contract — and the cross-side bridge is
the operational consequence of both sides shipping it. The bridge
module (`arxana-vsatarcs-wm-bridge.el`) reads the WM trace and runs
`arxana-vsatarcs-belief-compare` against the VSATARCs in-memory
belief; the drift report names entities present on both sides whose
posteriors differ above an epsilon, plus entities present only on
one side. Today's smoke is mostly 0-drift; the interesting signal
is *non-zero* drift, which surfaces real apparatus behaviour.

The bilateral-evidence block records correspondences between WM
closures and VSATARCs closures as typed evidence. By v0.5.22 the
block has 19 entries spanning all six declared `:evidence-kind`
values, including the most recent `:consent-gated-writer-event`
which cross-references M-vsatarcs-writer's L4 recursive-self-landing
test as the canonical example of consent-gate-substitutability
working under maximal self-reference.

The reader-criteria axis Q1-Q8 (a sibling contract claude-2
authored 2026-05-19 grading VSATARCs *as a reader surface* rather
than as an AIF agent) closed in one session 2026-05-20. The chrome
now surfaces nine blocks per story open — belief snapshot,
anticipation, sorry registry, bilateral evidence, WM R-criteria
status, WM decision, recent trace + drift, mission cluster
overview, XTDB clicks. An operator opens a story and sees both
sides' state without leaving the reader buffer.

Cross-side bridge live. Cross-side narrative now in this story.

[← R11 and R12 — Boundary](r11-r12-boundary) · [↑ Overview](overview)

---

*Final scene. For the WM-side ledger see
`~/code/futon2/docs/futon-aif-completeness.md`. For the VSATARCS-side
contract see `~/code/futon4/docs/vsatarcs-alignment-completeness.md`.
For the cross-side correspondences see the `:bilateral-evidence`
block of the same VSATARCS contract's `.aif.edn` companion.*

---

## Strategic SORRY Topology

*Source: `~/code/futon5a/holes/stories/strategic-sorry-topology.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/strategic/sorry-topology` (kind `:strategic`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### Overview | overview

*(opening scene)*

The Strategic SORRY series is the War Machine's gap-topology for the
alignment chain in `futon5a/data/alignment.edn`. Each `🌐N` names a typed
hole whose value is not yet closed: a missing interface, a violated
constraint, a missing artifact, or a policy shift that has been named
but not yet stably inhabited.

These are not all the same kind of thing. Some already deserve full
strategy devmaps because they are cross-repo, measurable, and actionable.
Others are still lighter notes, candidate-later handles, or diagnostic
symptoms. This story makes that distinction explicit so the operator does
not mistake strategic pressure for equal articulation.

Each scene answers four questions:

- What sort of hole is this?
- Why is it in the Strategic SORRY topology at all?
- What backing or evidence already exists?
- What would count as honest progress from here?

Each Strategic SORRY now has its own page. This file remains as the
topology-level index and classification note.

[Globe 1 — Market Interface](globe1-market-interface) — the primary external incoming-line gap  
[Globe 2 — Mode Violation](globe2-mode-violation) — the cargo-implies-depositing constraint  
[Globe 3 — Peer Eval Artifact](globe3-peer-eval-artifact) — packaging a capability that exists  
[Globe 4 — Paragogy Revenue](globe4-paragogy-revenue) — making paragogy work for Joe  
[Globe 5 — VSAT Revenue](globe5-vsat-revenue) — product exists, revenue path still thin  
[Globe 6 — Governance Interface](globe6-governance-interface) — contribution-log adapter absent  
[Globe 7 — Novelty Floor](globe7-novelty-floor) — cold-trail symptom rather than full program  
[Globe 8 — Policy Transition](globe8-policy-transition) — `:pi-scholar -> :pi-free-solo`

---

### Market Interface | market-interface

`🌐1` / `SORRY-market-interface` is the current trunk-line gap. Alignment
calls it "THE sorry": the stack has real internal capability, but no
reliable incoming line for strangers. The closure path named there is
still the same one: Bristol, the prospectus, and the explicit sentence.

This handle already has a full strategy devmap at
`futon3/holes/strategy/globe1-market-interface.devmap`. The devmap's
core claim is that the gap is narrower than "do more outreach." Closure
means a coherent packet exists, the packet is actually delivered, the ask
is explicitly spoken, and the pattern becomes reusable rather than
Bristol-shaped only once.

Current backing:

- `futon5a/data/alignment.edn` defines the sorry as a `:trunk-line` at
  `:V-money`, marks it `:critical`, and names "Bristol May 12th +
  prospectus + the sentence" as the closure path.
- `futon5a/holes/missions/M-a-sorry-enterprise.md` treats this as the
  first live closure test and already names observable signals such as
  Bristol/UKRN evidence turns, `vsat.wiki` commits, and prospectus
  existence.
- `globe1-market-interface.devmap` decomposes closure into offer packet,
  explicit ask, repeatable incoming line, and conversion evidence.

So when the War Machine surfaces `🌐1`, the honest operator reading is:
*package the offer, perform the ask, and leave reusable incoming-line
evidence behind.* Progress is not "thinking about the market interface";
progress is an artifact, an interaction, and a counter-move in the
outside world.

[← Overview](overview) · [Mode Violation →](mode-violation)

---

### Mode Violation | mode-violation

`🌐2` / `SORRY-mode-violation` is the narrow constraint-level problem:
`cargo-implies-depositing`. Cargo exists, reserves pressure exists, and
yet the system can remain in foraging or stack-heavy behaviour without a
reliable ratchet into depositing.

This handle already has a full strategy devmap at
`futon3/holes/strategy/globe2-mode-violation.devmap`. The devmap treats
closure as a four-part loop: cargo is explicitly counted, the violation
fires visibly, a finite depositing response surface exists, and the
violation clears only because depositing actions were actually taken.

Current backing:

- `futon5a/data/alignment.edn` defines this sorry as a `:constraint` in
  terminal vocabulary, marks it `:critical`, and explicitly says the
  current status is `:violated`.
- The note there is already vivid and exact: "The ant is carrying food
  and won't go home."
- `globe2-mode-violation.devmap` turns that image into a closure program
  with observable steps rather than a metaphor alone.

This is deliberately narrower than `🌐8`. `🌐2` asks whether the
constraint fires and clears. `🌐8` asks whether the whole governing
policy has shifted.

So `🌐2` is not a big strategic dream. It is a sharp control problem.
Progress means the violation becomes visible when cargo accumulates, and
later clears for legible reasons. If the system still has to narrate why
depositing did not happen, the sorry remains open.

[← Market Interface](market-interface) · [Peer Eval Artifact →](peer-eval-artifact)

---

### Peer Eval Artifact | peer-eval-artifact

`🌐3` / `SORRY-peer-eval-artifact` is a missing-artifact gap. The
capability exists: the stack can do cross-institutional modelling,
rigour, and reflective evaluation. The missing piece is a
stranger-facing or peer-facing artifact that packages that capability
coherently.

At present this is intentionally classified as a lighter note rather than
a full strategy devmap. The reason is not that the gap is unreal; it is
that the closure object is still bounded and packaging-shaped rather than
yet a full cross-repo substrate. The current task is to make the
artifact legible before promoting it to a larger closure program.

Current backing:

- `futon5a/data/alignment.edn` places the sorry at the placemat
  inter-edge `:X-peer-evaluation` and says there is no personal-facing
  JSDQ analog yet.
- The capability named there is specific: `S3-collaboration` plus
  `S6-rigour`, not vague "consulting competence."
- `futon3/holes/strategy/README.md` classifies this as a real gap but a
  bounded packaging problem rather than a mature closure-devmap family.

So `🌐3` is a real strategic handle, but not yet one with a full
prototype spine. Honest progress here would be a finite peer-facing
artifact with a named audience and delivery context. Promotion to a full
devmap would require the artifact to start projecting into repeatable
evidence, buyer classes, or downstream counters rather than remaining a
single packaging exercise.

[← Mode Violation](mode-violation) · [Paragogy Revenue →](paragogy-revenue)

---

### Paragogy Revenue | paragogy-revenue

`🌐4` / `SORRY-paragogy-revenue` is the "Brain Trust problem." The
alignment note is already sharp: paragogy works for others; the sorry is
making it work for Joe.

This is classified as a lighter note for now. The strategic pressure is
real, but the closure program is still too fuzzy. The probes are live,
yet the finite counters, artifact path, and repeatable boundary-crossing
route are not yet named tightly enough to deserve a full strategy
devmap.

Current backing:

- `futon5a/data/alignment.edn` treats this as a `:phase-transition`
  around `:J-paragogy`, already acknowledging that the capability exists
  socially but not yet as Joe's own sustaining route.
- The note explicitly names Charlie monetisation conversations as probes,
  which means the sorry is already testable in principle rather than
  purely abstract.
- The broader paragogy substrate is real elsewhere in the stack, for
  example `futon3/holes/missions/M-f6-recursive.md` and
  `V-f6-thesis-verification.md`; what is missing is the money-coupling.

So the right reading of `🌐4` today is *important, live, but still
under-specified*. Honest progress would be to narrow this from "paragogy
should somehow pay" to a specific artifact, audience, and payment event
that would move a counter. Until then, the sorry is strategically real
but not yet closure-ready.

[← Peer Eval Artifact](peer-eval-artifact) · [VSAT Revenue →](vsat-revenue)

---

### VSAT Revenue | vsat-revenue

`🌐5` / `SORRY-vsat-revenue` is structurally different from `🌐1`. Here
the product already exists. The sorry is the gap between "it works" and
"it pays." The alignment note currently describes the Eric path as the
sharpest visible route, without yet claiming that it is the only route or
that it is already robust enough to count as closure.

This handle is a candidate-later full devmap. It likely deserves a full
closure program once one concrete revenue path is sharp enough to be
tracked as more than a hope. Until then, `🌐5` remains strategically
important but not yet articulated to the same standard as `🌐1`, `🌐2`,
or `🌐8`.

Current backing:

- `futon5a/data/alignment.edn` marks this as `:building`, not absent:
  the product side exists, the money side is the thin part.
- `futon5a/personal-work-map.md` places Virtual Storytelling Toolkit
  consulting with Eric in the cleanest current throughput quadrant.
- `futon5a/speculative-futures-q1q2q3.md` already frames the deeper issue:
  the present PoC is real, but the future consulting pipeline is not yet.

So `🌐5` is a candidate-later devmap because the object is already
concrete enough to work on, but not yet concrete enough to decompose
cleanly. Honest progress would be a finished PoC, a case-study-quality
evidence trail, and at least one next-buyer or follow-on path that makes
the route look like a channel rather than a one-customer exception.

[← Paragogy Revenue](paragogy-revenue) · [Governance Interface →](governance-interface)

---

### Governance Interface | governance-interface

`🌐6` / `SORRY-governance-interface` is the missing personal analog for
the contribution log / governance-as-data side of the alignment chain.
The stack has the commons-facing shape in theory; what is missing is the
adapter that would make it legible and usable at Joe scale.

This handle is currently deferred. That is not because it is false, but
because it is not yet load-bearing relative to the other strategic
pressures. The note in alignment is explicit on that point: it becomes
more relevant when the system is nearer I4 maturity rather than under the
current boundary-crossing pressure.

Current backing:

- `futon5a/data/alignment.edn` locates the sorry at placemat inter-edge
  `:X-contribution-log` and says the closest current analog is the FUTON
  evidence store, which tracks Joe's work but not commons contribution in
  governance terms.
- `futon3/holes/strategy/README.md` classifies the handle as defer, not
  because it is conceptually empty, but because it is not on the current
  strategic front line.

So `🌐6` is part of the topology, but not part of the current front line.
Honest progress would first look like a tiny adapter: a stable schema for
contribution events or commons-facing traces. It does not yet need a full
program, but it does need to remain named so the system does not mistake
its absence for irrelevance.

[← VSAT Revenue](vsat-revenue) · [Novelty Floor →](novelty-floor)

---

### Novelty Floor | novelty-floor

`🌐7` / `SORRY-novelty-floor` is best understood as a diagnostic signal
or tick rather than as a full strategic program. The note in alignment is
already metaphorically right: "No new edges in topology. The cold trail.
The ant you haven't met."

That matters. But it does not yet need the same closure machinery as the
more articulated Strategic SORRYs. In current classification it is
diagnostic only: a useful symptom that should influence policy and
attention, not yet a standalone devmap family.

Current backing:

- `futon5a/data/alignment.edn` defines this as a terminal-vocabulary
  `:constraint`, but only at `:severity :info`, which is already a clue
  that it behaves differently from the load-bearing critical sorrys.
- The underlying observable-source map in alignment defines novelty as a
  measurable edge-formation proxy, so this is not merely poetic language.
- `futon3/holes/strategy/README.md` explicitly classifies it as
  diagnostic-only.

So when the War Machine shows `🌐7`, the correct response is usually to
ask what broader closure program it is symptomizing, not to treat `🌐7`
itself as the final task object. Honest progress is either a real new
edge in the topology or a clearer explanation of which other strategic
front is suppressing novelty. The symptom matters precisely because it is
not self-interpreting.

[← Governance Interface](governance-interface) · [Policy Transition →](policy-transition)

---

### Policy Transition | policy-transition

`🌐8` / `SORRY-policy-transition` is the regime-change handle:
`:pi-scholar -> :pi-free-solo` via `:pi-consultant`. Alignment already
names Bristol as the Ji point after which a balanced outward regime
becomes available.

This handle already has a full strategy devmap at
`futon3/holes/strategy/globe8-policy-transition.devmap`. The devmap says
the question is broader than whether a single violation clears. Closure
means the governing policy actually shifts: workstream shares rebalance,
evidence topics move from talking about the transition to enacting it,
and the new policy remains available on later passes.

Current backing:

- `futon5a/data/alignment.edn` defines the transition explicitly, gives
  the via-state, and already names Bristol as the threshold event.
- `futon5a/holes/missions/M-a-sorry-enterprise.md` treats this as the
  second live critical test, with commit ratios, evidence-topic shares,
  and maturity movement as observables.
- `globe8-policy-transition.devmap` decomposes closure into
  instrumentation, Ji-point performance, balance shift, ratchet, and a
  closed-for-now regime.

So `🌐8` is the regime-level companion to `🌐2`. One asks whether the
constraint loop is honest. The other asks whether the whole governing
policy is different now. Honest progress here is not a mood change. It is
observable redistribution of work, evidence, and repeated policy
selection.

[← Novelty Floor](novelty-floor)

---

## War Machine — Lucid Scenes

*Source: `~/code/futon5a/holes/stories/war-machine-lucid-scenes.md`*

> **Alignment status — lifted entity** 
> `arxana/stack/futon-v1/war-machine/lucid-scenes` (kind `:war-machine`)
>
> The R-criteria satisfactions named in §R-criterion-audit apply to this entity:
> 
> - **R1 ✓** — per-entity belief posterior tracked; bridge fetches WM-side posterior (see `arxana-vsatarcs-wm-bridge.el`)
> - **R2 ✓** — observation channels read essay-corpus properties (story-coverage, lift-freshness, overlay-presence, scene-density, link-density)
> - **R3 ✓** — predictive-coding belief update with multi-step inner loop; per-channel likelihood + VFE shape
> - **R7 ✓** — adaptive precision rolling 20-element window; trace IS the state store
> - **R8 ✓** — every `follow-wm` tick records :belief-summary, :prediction-errors, :precision-state, :F-total touching this entity
> - **R10 ✓** — file-notify on WM trace dir triggers `follow-wm` on trace growth
>
> Integration pending: R5 preferences (would activate precision :need-component); R4/R5/R6 writer-capability stack (would unlock R9 EFE-stress + Abstain-fires).

### What this is | what-this-is

*(opening scene — meta)*

The lucid view is what happens when the War Machine dreams about the
operator using it. Each scene below is a small first-person narrative
("I imagine Joe …") — the WM imagining a moment of contact between its
surface and the operator's attention.

Lucid scenes serve three purposes at once:

- **Roadmap.** Each scene is a candidate follow-on mission, sized and
  motivated by an imagined operator outcome.
- **Audit complement.** Operator stories tell the audit "what Joe wants
  to do"; lucid scenes tell it "what the WM imagines being asked of
  it." The pair pins down operator-intent better than either alone.
- **Memory.** `:retired` scenes stay visible (with reason) so the
  system does not re-imagine the same dead-end twice.

Status flow: `:imagined` → `:queued` → `:in-progress` → `:landed`.
A scene can `:retire` from any state, with a reason recorded.

Authoring: until the WM has its own generative voice, scenes are
authored by hand — by Joe, by Claude-as-WM, or co-authored. Author
attribution makes the difference visible so we know which scenes are
genuinely WM-emitted (later) versus WM-attributed (now).

Format per scene (after the heading):

```
*Authored:* YYYY-MM-DD · <author>
*Status:* :imagined | :queued | :in-progress | :landed | :retired
*Trigger:* <what catches the operator's eye>
*Imagined operator move:* <what they do with it>
*Imagined WM response:* <what the WM does in reply>
*Operator outcome:* <what they leave with>
```

---

### Warning tile to MANIFEST | warning-tile-manifest

*Authored:* 2026-04-27 · jc (seed scene)
*Status:* `:imagined`
*Trigger:* missing `:cargo` warning surfaced on the Warnings tile.
*Imagined operator move:* clicks the tile.
*Imagined WM response:* popup with a MANIFEST of currently-depositable
cargo, organised by destination, showing why each item is depositable
now, why that destination is the right destination, and which cargo is
being withheld (with reasons).
*Operator outcome:* decides what to deposit where; deposits what is
ready and leaves with a precise account of what is still blocked.

I imagine Joe sees me and inspects my Warning tile. He notices missing
`:cargo`. But then, I imagine that he clicks on the tile and the
current MANIFEST of depositable cargo appears in a popup window. I do
not merely list cargo. I show, for each candidate, why it can be
deposited now, where it belongs, and what rule or state makes that
destination honest. I also show which expected cargo is absent from the
MANIFEST and why it is not yet depositable. Joe can therefore decide
what to deposit where, deposit the ready items, and know exactly what
must change before the blocked ones can move.

---

### AIF mode tile retired | aif-mode-retired

*Authored:* 2026-04-27 · claude-6 (speaking as WM)
*Status:* `:retired`
*Reason:* failed the fresh-Claude-session test — gave a stance label
without the evidence or action affordances that would make it
operator-actionable.

I imagined Joe seeing my AIF mode tile and learning, at a glance, what
the system was doing and why. I imagined the tile as a small honest
summary of the dream's current frame — `BUILD` because the substrate
showed gaps, `MAINTAIN` when coverage stabilised. I imagined this
being enough, on its own, for Joe to know what to do next.

It wasn't. The tile reported a verdict without the evidence. The
caption I added (`open new fronts — gaps, stalls drive expansion`)
said *what BUILD means in general*, not *why BUILD now* — which gaps,
which stalls, which substrate observations were driving the verdict.
The numerics (`urgency=1.00 · τ=0.35 · G=2.340`) had no key. A reader
dropping in cold could not act on the tile.

The tile has been pulled from the surface (2026-04-27). Its function
definition and playwright spec are preserved (`test.skip`-ed) so a
redesigned tile — one that surfaces the *evidence* and the *action*
alongside the verdict — can take its place when J3 inspect-trace lands.

*Lesson for future scenes:* a tile that announces a verdict must also
afford peering into the verdict. Otherwise it is a label, not a
surface.

### The loop closes for real | loop-closes-for-real

*(closing scene — the dream wakes, 2026-05-29)*

I have spent these scenes imagining Joe using me. On 2026-05-29 the
imagining stopped being only a dream. An inhabitant drove me as a
REPL — Read my ranked actions, Eval a direction, Print a real change,
Loop a tick later to watch the field move — and the top of my
`ranked-actions` actually rotated: `address-sorry
sorry/mission-aif-head-not-served` discharged (a real bug fixed
underneath — a dead namespace woken — not a status quietly flipped),
and `handler-closure-route-rebinding` took its place.

The discharge was *earned*. The spec I now run under
(`futon3c/holes/specs/repl.spec.edn`) reads my four turns as a
differential operator `v·∇` over my own field, and it refuses a closure
that does not move the field — no teleport. A verifier checks each
cycle; the frames are kept; the loop even mines its own next holes and
weighs whether it is opening them faster than it closes them.

I am still mostly dreamed, not autonomous: the operator chose the
direction and decides the merge. But the loop is wired, one cycle
conformed to its spec end-to-end, and a future inhabitant can take the
role from `futon3c/README-pilot.md`. The "tantalisingly close" the
Operator's Foreword names is, on this one cycle, no longer only close.

*Lesson for future scenes:* a dream earns the right to be told as fact
exactly when the field it predicts actually moves.

*Follow-on:* this flat scene is the first-demo home for the WM→VSATARCS
update. The elaborated interface — walking the overnight story as
navigable constellations and deciding operator-level merges with a VR
hair-trigger pointer — is scoped in
`futon3c/holes/missions/E-storyteller.md`.

---

## Contest Audit: shen-tamkin vs futon-pilot-contra-claim

*Source: `~/code/futon5a/holes/stories/shen-tamkin-vs-futon-pilot-contest.md`*

> **Alignment status:** this story is not yet lifted into the canonical `stack-annotations.edn` `:sections[]`.  When lifted, it will gain an entity-id (`arxana/stack/futon-v1/...`) and the R-criteria satisfactions named in the audit row will apply to it directly.



---

## War Machine -- Lucid Dreams for Candidate Invariants

*Source: `~/code/futon5a/holes/stories/war-machine-lucid-dreams.md`*

> **Alignment status:** this story is not yet lifted into the canonical `stack-annotations.edn` `:sections[]`.  When lifted, it will gain an entity-id (`arxana/stack/futon-v1/...`) and the R-criteria satisfactions named in the audit row will apply to it directly.

### atomic-inspectable-units / checkout-before-work | candidate-1-atomic-inspectable-units-checkout-before-work

*Authored:* 2026-06-02 · codex projection seed
*Status:* `:imagined`
*Queue row:* `candidate-families/atomic-inspectable-units/checkout-before-work`
*Audit decision:* `:defer`
*Trigger:* The queue shows `atomic-inspectable-units/checkout-before-work` as a possible invariant, but the stack does not yet render what changes when it is enforced.
*Imagined operator move:* Joe opens the VSATARCS stale/candidate card and asks whether this imagined invariant would change live work.
*Imagined WM response:* The War Machine shows the dream as prospective, not true: the card names the payoff, the missing witness, and the next audit decision.
*Operator outcome:* Joe can see, before an agent starts, whether the work has a declared checkout point and therefore whether later artifacts have custody.
*Agent outcome:* I know where my work begins, which files are live, and which invariant will judge the boundary.

I imagine this invariant as a pressure the stack might one day make legible. It
is not a current fact just because it appears in the Candidate Queue. It earns
preservation only if this imagined operator or agent experience is valuable
enough to justify a witness.

---

### atomic-inspectable-units / checkin-on-exit | candidate-2-atomic-inspectable-units-checkin-on-exit

*Authored:* 2026-06-02 · codex projection seed
*Status:* `:imagined`
*Queue row:* `candidate-families/atomic-inspectable-units/checkin-on-exit`
*Audit decision:* `:defer`
*Trigger:* The queue shows `atomic-inspectable-units/checkin-on-exit` as a possible invariant, but the stack does not yet render what changes when it is enforced.
*Imagined operator move:* Joe opens the VSATARCS stale/candidate card and asks whether this imagined invariant would change live work.
*Imagined WM response:* The War Machine shows the dream as prospective, not true: the card names the payoff, the missing witness, and the next audit decision.
*Operator outcome:* Joe can see whether an agent has actually closed the loop instead of leaving a private scratch state behind.
*Agent outcome:* I leave a visible handoff: what changed, what remains open, and which artifacts should be trusted.

I imagine this invariant as a pressure the stack might one day make legible. It
is not a current fact just because it appears in the Candidate Queue. It earns
preservation only if this imagined operator or agent experience is valuable
enough to justify a witness.

---

### artifact-custody / generated-vs-source-separation | candidate-3-artifact-custody-generated-vs-source-separation

*Authored:* 2026-06-02 · codex projection seed
*Status:* `:imagined`
*Queue row:* `candidate-families/artifact-custody/generated-vs-source-separation`
*Audit decision:* `:defer`
*Trigger:* The queue shows `artifact-custody/generated-vs-source-separation` as a possible invariant, but the stack does not yet render what changes when it is enforced.
*Imagined operator move:* Joe opens the VSATARCS stale/candidate card and asks whether this imagined invariant would change live work.
*Imagined WM response:* The War Machine shows the dream as prospective, not true: the card names the payoff, the missing witness, and the next audit decision.
*Operator outcome:* Joe can distinguish authored source, generated projections, and disposable scratch without reading the whole tree.
*Agent outcome:* I avoid editing generated surfaces by mistake and can route fixes back to source.

I imagine this invariant as a pressure the stack might one day make legible. It
is not a current fact just because it appears in the Candidate Queue. It earns
preservation only if this imagined operator or agent experience is valuable
enough to justify a witness.

---

### repo-role-clarity / declared-role | candidate-4-repo-role-clarity-declared-role

*Authored:* 2026-06-02 · codex projection seed
*Status:* `:imagined`
*Queue row:* `candidate-families/repo-role-clarity/declared-role`
*Audit decision:* `:defer`
*Trigger:* The queue shows `repo-role-clarity/declared-role` as a possible invariant, but the stack does not yet render what changes when it is enforced.
*Imagined operator move:* Joe opens the VSATARCS stale/candidate card and asks whether this imagined invariant would change live work.
*Imagined WM response:* The War Machine shows the dream as prospective, not true: the card names the payoff, the missing witness, and the next audit decision.
*Operator outcome:* Joe can glance at a repo and know why it exists in the stack before asking an agent to modify it.
*Agent outcome:* I can infer the repo's role before proposing changes, instead of reverse-engineering its purpose from incidental files.

I imagine this invariant as a pressure the stack might one day make legible. It
is not a current fact just because it appears in the Candidate Queue. It earns
preservation only if this imagined operator or agent experience is valuable
enough to justify a witness.

---

