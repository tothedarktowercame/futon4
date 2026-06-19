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
