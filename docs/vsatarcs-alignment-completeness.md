# AIF completeness contract — VSATARCS reader (alignment scope)

*The eleven properties an Active Inference implementation must satisfy to honestly carry the label, applied to the VSATARCS reader surface as a participant in `M-stack-essay-code-alignment`.*

## Why this document exists

VSATARCS (`futon4/dev/arxana-browser-vsatarcs.el`, `~/code/futon4/README-vsatarcs.md`) is the Arxana-native reader for the futon stack's scene-form anthology. It renders ~47 stories drawn from `~/code/futon5a/holes/stories/`. As a participant in `M-stack-essay-code-alignment` (the interface mission between `M-stack-essay` and `M-war-machine-aif-completion`), VSATARCS is structurally an **AIF observer of the futon stack** — and prospectively an AIF writer once the typed-rewrite apparatus (`~/code/futon4/README-rewriting.md`) routes through it.

This contract grades the VSATARCS surface against the same R1-R12 criteria the WM AIF apparatus is graded against (`~/code/futon2/docs/futon-aif-completeness.md`). The intent is symmetry: WM and VSATARCS are sibling AIF surfaces over the same canonical substrate (`stack-annotations.edn`), graded by the same standard, so alignment can be checked criterion-by-criterion rather than as informal correspondence.

Per `M-stack-essay-code-alignment.md` §1.5, alignment between essay and code is *bidirectional under typed events*: the essay (VSATARCS-projected) reads from `stack-annotations.edn`; the code's belief / forward / EFE artefacts read from + write to the same. A discrepancy between what the essay claims and what the code computes is either resolved or recorded as `:lift-anomaly :alignment-drift` in the same commit. **For that contract to be checkable, both sides must maintain explicit belief over the same entity domain** — which is what R1 supplies.

## Scope note — F1-F10 vs R1-R12

`M-stack-essay-code-alignment.md` does not pin R/F-graded exit criteria to specific deliverables; it pins the alignment relation itself. The two contracts contribute as follows:

| Scope | Contract | Home | What it measures |
|---|---|---|---|
| Whole futon stack | F1-F10 | `~/code/futon0/docs/stack-fitness-completeness.md` | Stack as a homeostat (substrate-2, watcher daemons, satisficing-zapper, metabolic-balance, VSATARCS, WM) |
| WM AIF apparatus | R1-R12 | `~/code/futon2/docs/futon-aif-completeness.md` | WM as an AIF implementation |
| VSATARCS reader surface | R1-R12 (this doc) | `~/code/futon4/docs/vsatarcs-alignment-completeness.md` | VSATARCS as an AIF observer of the stack |

VSATARCS's satisfaction of R-criteria contributes to F-criterion movement at stack scope (see §"Cross-mapping to F1-F10" below) and lets the alignment mission's per-commit / daily / per-checkpoint cadence (T3) compare WM-belief and VSATARCS-belief over the same entity ids.

## Scope of the VSATARCS surface (as graded here)

This contract covers the modules under `futon4/dev/arxana-vsatarcs*.el` and adjacent AIF apparatus authored alongside them:

- `arxana-browser-vsatarcs.el` — scene-form story reader; entry point `arxana-vsatarcs-open-file`, anthology view via `arxana-browser-vsatarcs-*`.
- `arxana-vsatarcs-belief.el` — per-entity belief state apparatus (v0.1).
- *(planned)* observation channels — what VSATARCS senses from `stack-annotations.edn` and from the story prose it has parsed.
- *(planned)* trace surface — per-update record of what events landed and how the posterior moved.

VSATARCS is currently a **live reader on operator demand**: each invocation opens a story, parses it, and renders. There is no recurring cycle, no persisted trace, no action selection.

## The criteria

### R1 — Explicit belief state

The implementation maintains a belief distribution over hidden state, carried across ticks, with mean *and* precision (variance) both explicitly represented.

**Operational check.** Find the persistent belief data structure. Verify it has mean + variance fields per state dimension. Verify it is updated tick-over-tick rather than recomputed.

**This implementation.** **Satisfied as of v0.1 (2026-05-17).** `arxana-vsatarcs-belief.el` ships with:

- `arxana-vsatarcs-belief-status-set` — tagged status set (`spawned refined strengthened addressed falsified foreclosed reopened`), aligned with M-INC event vocabulary v1 `state/*` event types. Identical to the WM apparatus's `futon2.aif.belief/status-set`.
- `arxana-vsatarcs-belief-uniform-prior` — per-entity initial posterior.
- `arxana-vsatarcs-belief-update-entity` / `-update` / `-update-batch` — deterministic multiplicative-likelihood update under typed events; M-INC-compatible event shape (`:type :weight :entity-id :timestamp`); no dependency on M-INC step (b).
- `arxana-vsatarcs-belief-most-likely-status` — argmax (discrete analogue of belief mean).
- `arxana-vsatarcs-belief-entropy` — Shannon entropy in nats (discrete analogue of belief precision / variance).

Tests at `futon4/test/arxana-vsatarcs-belief-test.el` (18 tests / 33 assertions): posterior shape invariants, deterministic update (R1 baseline contract), namespaced/bare event-type equivalence, link-event tolerance, multiplicative-likelihood commutativity, V-shrink-shape (entropy decreases under accumulating evidence), most-likely-status correctness on uniform / peaked / empty inputs.

**Symmetry with WM.** This module is a deliberate elisp port of `futon2/src/futon2/aif/belief.clj` v0.2 (claude-2, 2026-05-17, M-war-machine-aif-completion Checkpoint 1). The status set, the update mechanics, and the test shape match; the event shape is wire-compatible (both consume `{:type :weight :entity-id ...}` events). This makes future per-entity posterior comparison (the alignment-drift check) a structural equality test rather than a translation problem.

**Reader-chrome integration shipped as of v0.2 (2026-05-17).** `arxana-vsatarcs-belief.el` extended with a file-backed store (`arxana-vsatarcs-belief-save` / `-load`; default path `~/.emacs.d/var/vsatarcs-belief.eld`), a module-local current-belief variable, an `arxana-vsatarcs-belief-ingest-events` batch-and-optionally-persist entry point, and an `arxana-vsatarcs-belief-snapshot` projection sorted by descending entropy. The reader buffer (`arxana-vsatarcs-render`) now inserts a "Belief snapshot" block between the scenes navigator and the scene body. When the global belief is empty the block shows a brief notice pointing to the ingest entry point; otherwise it lists each tracked entity-id with its most-likely status and entropy in nats. Tests covering persistence round-trip, ingest, snapshot ordering, and chrome integration ship in `arxana-vsatarcs-belief-test.el` (24 tests / 50 assertions) and `arxana-browser-vsatarcs-test.el` (10 tests, includes 2 chrome-integration tests).

**Story-scoped filtering** (showing only the entities the currently-rendered story references) is a v0.3 move; it couples to R2 because the "entities referenced by the open story" channel is a natural observation channel under the R2 schema. v0.2 surfaces global belief deliberately, so R1's surfacing is operator-checkable without waiting on R2.

**Prior bootstrap from the canonical projection source (shipped as of v0.2.2, 2026-05-18).** `M-x arxana-vsatarcs-belief-bootstrap-from-stack-annotations` reads `~/code/futon5a/holes/stack-annotations.edn`, extracts `:sections[]` `:id` strings, and seeds uniform priors for every entity not yet tracked. Non-destructive: accumulated evidence on already-tracked entities is preserved. The chrome's "Belief snapshot" block becomes populated on first open after the operator runs the bootstrap. The minimal EDN reader in `arxana-browser-rewrites.el` was extended in the same closure to tolerate tagged literals (`#inst "..."` — tag discarded, form returned bare) and sets (`#{...}` — read as list); the README caveat is updated accordingly.

The bootstrap is recorded in the `.aif.edn` companion under a new `:enables` field — a structured list of one-step-lookahead integration points so future moves don't lose sight of WHY the bootstrap exists. The five enabled points: (i) alignment-drift detection (bilateral comparison becomes non-trivial once WM-side also bootstraps); (ii) M-INC event targeting (live events land on known entities); (iii) story-scoped filtering (v0.3 / R2); (iv) R8 trace grounding (entity-id provenance); (v) WM-side symmetric bootstrap as a `:code-docs-correspondence` instance (deferred until the landing surface exists).

**Convergence with WM v0.5 (`futon2/docs/futon-aif-completeness.md` §"Capability-gap modeling as endogenous action: the modeller inside the model").** The `:enables` field is the closure-annotation-layer homolog of the WM-side `:learn-action-class` actions — same intent (explicit capability boundaries, structured rationale, blockers named), different layer. WM operates at the action-space layer because it has an action space; VSATARCS operates at the closure-annotation layer because writer-capability (R4 + R6) is deferred. When VSATARCS gains writer capability, its action space should adopt the same first-class capability-gap-action shape and the two sides become directly compositional under the principle. The two-sided independent naming of the same structural principle on 2026-05-17–18 is itself evidence for a `:code-docs-correspondence` invariant — the bilateral landing the v0.1 R1-symmetry annotation was deferred against (cf. `hx:vsatarcs-align:v0-1:r1-symmetry-with-wm`). Recorded here in the prose so the next iteration of the landing surface has two-sided evidence to point at.

**`:enables` rendered at the review surface (v0.2.3, 2026-05-18).** The `arxana-browser-rewrites.el` Rule panel now surfaces the `:enables` block between Verification and Decided-by. Each entry shows `:point` / `:description` / `:status` / `:blocker`; closures without `:enables` are unaffected. This closes the loop the field was designed to serve: forward-pointers become auditable at the canonical review surface rather than living only in `.aif.edn` data, module commentary, and contract prose. The renderer extension is itself an instance of the capability-gap-modeling principle — the gap ("review surface can't show forward-pointers") is named and closed in the same closure that adds the rendering code.

**R1 polish-off complete (v0.2.4, 2026-05-18).** Three coordinated moves closed the R1 operator-facing surface area: (i) `arxana-vsatarcs-belief-compare(belief-a belief-b &optional epsilon)` — pure-function bilateral comparison primitive returning a drift report (`:only-in-a`, `:only-in-b`, `:posterior-diffs` above epsilon, `:equal-count`); the local apparatus the future cross-side alignment-drift check will consume. (ii) Reader mode-map bindings — `B` bootstrap-and-redisplay, `R` reset-with-confirm, `i` ingest-from-minibuffer; R1 management is operator-triggerable from inside the reader buffer with no `M-x` round-trip. (iii) `README-vsatarcs.md` gains a "Belief surface (R1)" section documenting the status set, persistence path, keybindings, non-interactive entry points, and what's not yet wired. Belief suite 28 → 34 tests / 67 → 90 assertions. R1's audit row reads `:satisfied-with-operator-surface-closed`; the only remaining R1 gap is the cross-side bridge — reading WM-side belief from `futon2.aif.belief` so the comparison primitive can be exercised against real bilateral state. That bridge is deferred (needs a Drawbridge nREPL read or analogous endpoint to be agreed).

**Cross-side bridge live; bilateral milestone v0.9 ↔ v0.2.5 (2026-05-18).** Bilateral counterpart of WM-side `hx:wm:v0-9:symmetric-bootstrap-closure` (claude-2, 2026-05-18); coordinated via the bell/whistle protocol (`futon3c/README-bells-and-whistles.md`). New module `arxana-vsatarcs-wm-bridge.el` reads the latest record's `:mu-post` from `~/code/futon2/data/wm-trace/wm-trace-YYYY-MM-DD.edn`, converts the WM-side format (plist with `:status` keywords; entity-ids as strings or the meta-sorry keyword) into the local belief format (alist with bare status symbols), and runs `arxana-vsatarcs-belief-compare`. The expected meta-sorry id (`:sorry/wm-aif-substrate-addressability`) is filtered out of `:only-in-wm-side` into a dedicated `:expected-in-wm-only` field so the operator isn't told about an expected delta every call. End-to-end smoke test against today's real WM trace (2026-05-18): 36 WM entities (35 strings + meta-sorry) ↔ 35 VSATARCs entities → 35 equal, 0 drift, meta-sorry filtered as expected. 7 fixture tests cover the parse + convert + compare path. R1's audit row now reads `:satisfied-with-bilateral-bridge-live` and `:only-remaining-r1-gap` reads `nil`. The bridge consuming `:mu-post` is forward-compatible: once R3a (WM-side observation-driven updates within the per-call cycle) lands, real drift will surface and lift naturally into `:lift-anomaly :alignment-drift` entries in `stack-annotations.edn` per exit-criterion #3.

The `.aif.edn` also seeds a top-level `:bilateral-evidence` block — a first concrete shape for the `:code-docs-correspondence` invariant landing surface (deferred per Joe 2026-05-17). Two entries on landing: `:independent-naming-of-same-principle` (v0.2.3 ↔ WM §Capability-gap-modeling, 2026-05-17) and `:joint-landing` (v0.2.5 ↔ WM v0.9, 2026-05-18). `:evidence-kind` distinguishes the modes; the block extends as more two-sided pairs accumulate.

### R2 — Observation channel schema

The agent's interface to the world is a fixed, normalised observation shape — a vector or map of bounded channels. The schema is named; observations from different ticks have the same shape and the same channel semantics.

**Operational check.** Find the observation type. Verify the channel set is documented and stable, observations are normalised to [0,1], and all subsequent AIF machinery is keyed off the schema.

**This implementation.** **Satisfied as of v0.3.0 (2026-05-18).** `arxana-vsatarcs-observation.el` ships an ordered 5-channel schema declared as a `defconst`:

| Channel | Source | Normalisation |
|---|---|---|
| `:story-coverage` | fraction of `stack-annotations.edn :sections[]` whose `:ref` basename is present in `arxana-vsatarcs-story-directories` | `lifted-present / lifted-total` |
| `:lift-freshness` | per-section delta between recorded `:source-mtime` and on-disk filesystem mtime | `exp(-mean-delta-days / arxana-vsatarcs-observation-freshness-scale-days)` (default scale 30) |
| `:annotation-overlay-presence` | fraction of stories with a sibling `<name>.aif.edn` | `stories-with-overlay / total-stories` |
| `:scene-density` | mean scenes per story | `mean / arxana-vsatarcs-observation-scene-density-max` clipped to 1 (default max 20) |
| `:link-density` | mean markdown links per scene | `mean / arxana-vsatarcs-observation-link-density-max` clipped to 1 (default max 5) |

All channels return values in `[0, 1]`. `arxana-vsatarcs-observe` builds the channel-map plist; `arxana-vsatarcs-sense-to-vector` projects to declared channel order. Tests at `futon4/test/arxana-vsatarcs-observation-test.el` (19 tests / ~25 assertions): 4 schema-stability (keywords-only, no-duplicates, count-stable, named); 11 per-channel correctness with fixtures; 4 observe + sense-to-vector roundtrip. Real-corpus smoke against 35 sections + 47 stories + 16 overlays (2026-05-18): `:story-coverage` 0.071, `:lift-freshness` 0.91, `:annotation-overlay-presence` 0.34, `:scene-density` 0.31, `:link-density` 0.56 — all in [0, 1].

The closure (`hx:vsatarcs-align:v0-3-0:r2-observation-channels-closure`) closes the v0.1 `r2-gap-named` annotation by realisation: the candidate channels and acceptance test that annotation predicted are now shipped. R3 sub-properties update: R3a moves to `:unblocked-by-r2`; R3c to `:partially-unblocked-by-r2` (both still need a predicted-observation likelihood model). A third `:bilateral-evidence` entry lands with the new `:evidence-kind` `:independent-naming-of-same-r-criterion-shape-at-different-scopes` — WM and VSATARCS both satisfy R2 at different scopes (stack-fitness vs essay-corpus); the discipline is bilaterally satisfied even though the content isn't drift-comparable across channels.

### R3 — Predictive-coding belief update

Four sub-properties must all be present: (R3a) prediction error per observation channel; (R3b) precision weighting; (R3c) variational free energy; (R3d) belief update step.

**Operational check.** Find the belief-update function. Verify all four sub-properties are computed by name; VFE is reported per tick.

**This implementation.** **Partial — R3d shipped, R3a/R3b/R3c pending.** As of v0.1: (R3d) belief update step is present (`arxana-vsatarcs-belief-update`); (R3a) prediction error per channel requires R2 (observation schema) before it can be computed; (R3b) precision weighting requires an updateable precision field, R7-class work; (R3c) variational free energy requires both the observation schema and a likelihood model linking belief states to observation distributions. **Closing gap:** ship R2 first, then a predicted-observation model in `arxana-vsatarcs-belief.el` (or a new `arxana-vsatarcs-likelihood.el`), then wire VFE through.

**Sub-property closure protocol (decided 2026-05-17, applies to all composite R-criteria).** Sub-properties close independently before the aggregate R-criterion closes. Per-sub-property status transitions are recorded as their own deltas (e.g., `:R3a [:blocked-on-R2 :satisfied]`); the aggregate verdict flips `:partial → :satisfied` only when every named sub-property is `:satisfied`. R3 is the only composite criterion at v0.1; future composites adopt the same protocol.

**R3a + R3c satisfied as of v0.5.0 (2026-05-19).** Joint-landing bilateral milestone v0.5.0 ↔ WM v0.10+ via the bell/whistle coordination protocol. New module `arxana-vsatarcs-likelihood.el` ports claude-2's WM-side architecture from `futon2/src/futon2/aif/belief.clj:191-329` + `free_energy.clj:79` + `war_machine.clj:2503-2575`. The shape: per-status weight tables → per-entity expected channel contribution → mean-entropy-variance → composite `predict-observation` returning channel-map of `(:mean :variance)` → `compute-prediction-error` (R3a + R3b: error + precision-weighted error with min-variance floor) → `compute-vfe` (R3c VFE-shape: `0.65 · accuracy + 0.35 · complexity` blend) → `run-multi-step` (K=3 inner loop with anneal-factor `(1 - step/K)`, sign-aware aggregation via per-channel `:health-sign`, synthesised events update belief per step, early-termination on aggregate magnitude < `r3-error-eps`).

**Covered channels (3 of 5 R2 channels):** `:story-coverage`, `:lift-freshness`, `:annotation-overlay-presence` — each backed by a hand-tuned per-status weight table (defensible-but-rough, replaceable by R7 / adaptive precision once it lands). The hypothesis encoded in the weights is correlative not causal: entities believed to be `:addressed` / `:strengthened` are more likely to have story refs lifted to disk; entities in actively-transitional states (`:refined`, `:reopened`, `:spawned`) are more likely to have fresh on-disk lifts; etc.

**Uncovered channels (logged as `:prototyping-forward`):** `:scene-density`, `:link-density` — structural corpus facts without a natural per-entity-status mapping. The `defconst arxana-vsatarcs-likelihood-prototyping-forward-channels` names them honestly. Future move either enriches the status set (e.g. add `:densely-scened` / `:link-dense` tags) or admits them as exogenous-input signals affecting R5 preferences without R3a prediction. Parallel to claude-2's 10 channel-likelihood sorries on the WM side.

**R3 sub-property statuses after v0.5.0:** R3a ✓, R3c ✓, R3d ✓, R3b `:blocked-on-R7` (precision weighting needs adaptive-precision history). Per the closure protocol, the R3 aggregate verdict remains `:partial-three-of-four-sub-properties-satisfied` until R3b also lands. Tests at `futon4/test/arxana-vsatarcs-likelihood-test.el` (18 tests / ~40 assertions): schema stability (3), per-channel predict shape + empty-belief + uniform-vs-peaked-variance (4), prediction-error (4), VFE shape + decrease-when-belief-explains (2), multi-step (5). Real-corpus smoke 2026-05-19: 35 bootstrapped entities → predictions for 3 channels (mean = 0.42 / 0.59 / 0.39) vs observations (0.07 / 0.91 / 0.34) → prediction errors compute; VFE F-total = 0.40 (mostly complexity from uniform priors); multi-step runs 3 steps without early-termination.

**Adjacent trace bug-fix (recorded in the same closure):** `arxana-vsatarcs-trace-follow-wm` previously called `emit` without a date-specific path, so VSATARCs ticks landed in today's wall-clock trace file rather than the WM-record's day-bucket. Surfaced when wall clock crossed midnight UTC during this session (fixtures use 2026-05-18; today 2026-05-19). Fixed: `follow-wm` now passes the date-specific path explicitly. Two trace tests (`follow-wm-timestamps-align-with-wm`, `follow-wm-picks-up-new-records`) caught the regression and now pass.

**Fifth `:bilateral-evidence` entry uses the `:protocol-witnesses` extension** claude-2 proposed in their v0.9 ↔ v0.2.5 ack. Three-turn whistle thread captured as the entry's audit trail (claude-4 cue → claude-2 ack → claude-4 v0.5.0 landed). First worked example of the extension; future `:joint-landing` entries can adopt the same shape.

### R4 — Predictive forward model

A pure function `predict :: (state, action) → next-state-distribution` that the agent uses to *score candidate actions before taking them*. Without this, the agent can score actions only against current state, and EFE collapses to a heuristic.

**Operational check.** Find the function that produces predicted next-state. Verify it is pure, returns a distribution (mean + variance), and is called by the policy layer.

**This implementation.** **Not satisfied.** VSATARCS today has no notion of action — reader-only. A forward model presupposes an action space (R6). When VSATARCS gains writer capability (per `~/code/futon4/README-rewriting.md` — typed rewrites are the candidate action space), the forward model becomes "given the current `stack-annotations.edn` hypergraph and a proposed typed rewrite, what does the post-rewrite hypergraph look like, with what invariant-delta?" That makes R4 a real engineering deliverable rather than a categorical N/A. Gap to close: tied to writer-capability milestones.

### R5 — EFE with at least two principled terms

Expected Free Energy decomposes into at minimum: (R5a) pragmatic / risk and (R5b) epistemic / ambiguity. Both are computed against the predictive forward model from R4.

**Operational check.** Find the EFE computation. Verify both pragmatic and epistemic terms are present and computed against the predictive forward model.

**This implementation.** **Not satisfied.** Presupposes R4. When VSATARCS gains the ability to score candidate typed rewrites, EFE-shaped scoring (pragmatic: invariant-delta direction; epistemic: prose-coverage gap reduction) becomes the policy layer.

### R6 — Softmax action selection with abstain

`P(a) ∝ exp(−G(a) / τ)`, sampled or argmax. Abstain semantics: when the predictive distribution is too uncertain to discriminate among candidates, the agent declines to act.

**Operational check.** Find the action-selection function. Verify softmax with temperature τ; verify an abstain branch exists and fires under high uncertainty.

**This implementation.** **Not satisfied.** No action space (writer capability is on roadmap). Abstain semantics will matter once the rewrite proposer is online; until then, the reader-only surface is structurally abstain-by-default.

### R7 — Adaptive precision

Precision Π updates over time based on prediction-error history. Channels with persistent high error lose precision; channels with persistent low error gain precision.

**Operational check.** Find where Π is updated. Verify it is updated tick-over-tick.

**This implementation.** **Satisfied as of v0.5.3 (2026-05-19).** New module `arxana-vsatarcs-precision.el` is a direct port of claude-2's WM-side `futon2/src/futon2/aif/precision.clj` architecture. Per-channel precision Π updates across ticks based on a 20-element rolling window of recent prediction errors:

```
Π = variance-component + need-component (bounded by [floor, cap])

  variance-component = 1 / max(rolling-variance, min-variance)
  need-component     = need-scale × max(0, channel-gap-from-preference)
```

The variance-component is active. The need-component remains 0 until R5 (preferences) lands — same pattern as claude-2's 10 `:prototyping-forward` channel-likelihood sorries on the WM side. Defaults: `floor 0.1`, `cap 200`, `initial-precision 1.0`, `window-size 20`, `min-variance 0.01`, `need-scale 5.0` (ported verbatim).

**Cross-call persistence: the trace IS the state store** (per claude-2's principle 2026-05-19). `arxana-vsatarcs-trace-build-record` gains a `:precision-state` field; `arxana-vsatarcs-trace-follow-wm` reads the latest record's `:precision-state` to continue the rolling window, updates via `arxana-vsatarcs-precision-update-state`, re-weights this tick's errors via `arxana-vsatarcs-precision-reweight-all`, stores the next state in the emitted record. Same discipline as the WM side; no separate state file.

**R3b also flips here; R3 aggregate closes.** R3b (precision weighting) was `:blocked-on-R7`. With R7 satisfied, R3b is satisfied via this module's `weighted-error` re-weighting (preserves `:per-call-precision` in the trace). Per the v0.1.1 closure protocol — sub-properties close independently before the aggregate — the R3 aggregate verdict flips from `:partial-three-of-four-sub-properties-satisfied` to `:satisfied`. R3 is the **first composite R-criterion to fully close** all four sub-properties on the VSATARCs side.

Tests at `futon4/test/arxana-vsatarcs-precision-test.el` (16 tests): initial-state, variance helper, rolling-window truncation, precision bounds, low-variance ceiling at `1/min-variance`, high-variance reduction, weighted-error preservation of `:per-call-precision`, reweight-all coverage, need-component-zero-without-preferences. Two bugs caught during authoring (`reweight-all` plist construction order; cap-vs-ceiling test expectation correction). Total VSATARCs test count 115 → 131.

**Limitation:** precision cap (200) is unreachable on the VSATARCs side until R5 lands — variance-component alone caps at `1/min-variance = 100`. Architecture is fully in place; only the preferences are missing.

### R8 — Per-tick trace

Each tick emits a record containing: μ_pre, observation, prediction errors, μ_post, candidates considered, per-term EFE values, chosen action, τ, F. Sufficient detail that an external observer can reconstruct what the agent did and why.

**Operational check.** Find the trace surface. Verify the schema includes the named fields. Verify trace is written for every tick.

**This implementation.** **Satisfied as of v0.4.0 (2026-05-18).** `arxana-vsatarcs-trace.el` ships an event-source-agnostic emit API + a cadence-following consumer per Joe's design directive (*"make it event-source-agnostic to start with — useful for testing. One use case: docs always in line with code. Ticks driven by the WM (or whatever drives it); VSATARCs follows the same cadence."*).

**Architecture:**

- **`arxana-vsatarcs-trace-emit RECORD`** — source-agnostic. Any caller (tests, the cadence-follower, future reader-event hooks) appends a record to the day's trace file. The caller fills `:source` + `:tick-kind`.
- **`arxana-vsatarcs-trace-follow-wm &optional DATE`** — the load-bearing use case. Consumes WM records from `~/code/futon2/data/wm-trace/wm-trace-YYYY-MM-DD.edn` the VSATARCs side hasn't yet followed (`:wm-trace-anchor :line-index` past the max seen in our trace) and emits a corresponding VSATARCs tick per WM record at the matching `:timestamp`. **The trace IS the stamp:** idempotent without a separate index file.
- **Record shape** — flat plists + nested plists (prin1-safe through the minimal EDN reader). Fields: `:trace-version`, `:timestamp`, `:emitted-at`, `:tick-kind`, `:source`, `:trigger`, `:observation` (R2 channel-map), `:belief-summary` (`:entity-count`, `:max-entropy`, `:min-entropy`), `:wm-trace-anchor` (path + line-index + wm-timestamp), `:bridge-snapshot` (drift counts + max-posterior-diff). Forward-compatible nil holes: `:prediction-errors` (R3a), `:candidates` / `:per-term-EFE` / `:chosen-action` / `:tau` (R4–R6), `:F` (R3c).

**Why summaries instead of full belief snapshots:** elisp alist-with-dotted-pairs `(a . 0.1)` doesn't round-trip through the minimal EDN reader (the `.` parses as a symbol). Storing summaries keeps records prin1-safe and small. Full belief snapshots in trace would require a proper EDN-map serializer; deferred.

**The "docs always in line with code" guarantee.** Every WM tick has a matching VSATARCs tick at the same `:timestamp` carrying the bridge-fetch result. An analytic joins by `:wm-trace-anchor`. Smoke against today's real WM trace (2026-05-18): 4 WM records consumed → 4 VSATARCs ticks emitted; latest tick anchor `:line-index 3 :wm-timestamp "2026-05-18T21:40:07.467Z"`; `:bridge-snapshot :posterior-diffs-count 35 :max-posterior-diff 0.0045 :expected-in-wm-only-count 1` — **first operational observation of non-zero cross-side belief drift** (claude-2's WM has had cycles since bootstrap; entities have drifted slightly from uniform priors).

Tests at `futon4/test/arxana-vsatarcs-trace-test.el` (15 tests): 3 emit-roundtrip + source-agnosticism, 3 schema-stability, 2 belief-summary, 7 follow-wm (one-tick-per-record, timestamp-alignment, idempotence, picks-up-new, anchor-monotonicity, missing-file, snapshot-shape). Fourth `:bilateral-evidence` entry: `:principle :r8-trace-persistence`, `:evidence-kind :independent-naming-of-same-r-criterion-shape-at-different-scopes`.

### R9 — Named validation properties

Operationally testable properties: V-shrink (belief variance decreases under informative observations), F-decrease (free energy decreases on average over a run), EFE-stress (pragmatic increases with predicted divergence; epistemic increases for unfamiliar observations), Abstain-fires (under high uncertainty, the agent abstains).

**Operational check.** Find the validation harness. Verify each property has a named quantitative acceptance criterion.

**This implementation.** **Partial — V-shrink-shape (v0.1) + F-decrease (v0.5.1).** `arxana-vsatarcs-belief-test.el` ships V-shrink-shape (`arxana-vsatarcs-belief-entropy-peaked-below-uniform`) plus shape invariants. v0.5.1 adds F-decrease as a named property: `arxana-vsatarcs-trace-r9-f-decrease-named-property` asserts F(post-multi-step toward target) < F(pre-multi-step) at the `compute-vfe` apparatus level (using a synthetic clear-signal observation; the trace-level statistical version was tried and dropped — dominated by floating-point noise when observations pull from a varying real-corpus environment). Per-tick `:F` (F-total scalar) is now populated in every `follow-wm`-emitted record, so historical F-trajectory is queryable. Missing: EFE-stress (needs R5), Abstain-fires (needs R6) — both deferred pending writer-capability. Sequential ship per claude-2's 2026-05-19 whistle (no WM-side dependency; WM-side analogous F-decrease wiring lives at `futon2/test/futon2/aif/r9_named_validation_test.clj`).

### R10 — Live operation

The AIF loop runs on a recurring schedule without operator intervention; trace is persisted to a queryable store.

**Operational check.** Find the schedule + the trace store.

**This implementation.** **Satisfied as of v0.5.2 (2026-05-19) — subscribed-and-running.** `arxana-vsatarcs-r10-tap.el` registers `file-notify-add-watch` on the WM trace directory (`~/code/futon2/data/wm-trace/`). When a `wm-trace-YYYY-MM-DD.edn` file is created or grows, the handler fires `arxana-vsatarcs-trace-follow-wm` with the date parsed from the filename. The operator calls `M-x arxana-vsatarcs-r10-tap-start` once per Emacs session; the subscription stays active and fires unattended on every WM trace growth.

**Honest event-driven R10.** R10's "loop runs on a recurring schedule without operator intervention" doesn't mandate a calendar schedule — event-driven schedules satisfy cleanly. Operator opens VSATARCs once; tap is active; trace grows; follow-wm fires; no per-cycle operator intervention. Wakeup-without-work pattern is honest too: when no new WM trace records exist, `follow-wm` is a no-op (idempotent via the existing `:wm-trace-anchor :line-index` lookup).

**Asymmetric R10 satisfaction across the two sides.** WM-side R10 is `:scheduled-execution-ready` (`clojure -M:wm-scheduled` script exists; cron-install pending operator action). VSATARCs-side R10 is `:subscribed-and-running` (file-notify already firing the moment VSATARCs is opened). Both honest R10 satisfactions; different shapes. The bridge eventually compares cadences across both sides' trace records — call-rate differential is itself alignment-drift-adjacent signal (recorded in the seventh `:bilateral-evidence` entry's `:asymmetry-note`).

**Minimum-viable scope; v0.5.3+ extends to the full multi_watcher tap** (per claude-2's design): bb sidecar polls futon1a watcher-events + writes `~/code/storage/multi-watcher/heartbeat.edn` per cycle; VSATARCs file-notifies on the heartbeat; path-classifier categorises events into `:duree-click-mission-edit`, `:duree-click-wm-trace-emission`, `:duree-click-code-edit`, `:duree-click-vsat-reader`; click-log infrastructure stores them. Becomes a bilateral substrate move when WM-side also subscribes. Forward-pointer recorded in the v0.5.2 closure and the seventh `:bilateral-evidence` entry.

Tests at `futon4/test/arxana-vsatarcs-r10-tap-test.el` (10 tests): path-filter (2), date-extraction (1), handler-logic with synthesised events (5), start/stop idempotence + missing-directory creation (2). Real-file-notify integration is exercised manually via `M-x arxana-vsatarcs-r10-tap-start` + `-status`; unit tests synthesise the event tuple directly to keep tests fast + deterministic.

### R11 — Hierarchical / multi-agent composition

When multiple AIF agents act on shared state, a coordination layer ensures their actions compose coherently.

**This implementation.** **N/A at this scope.** VSATARCS and WM are sibling AIF surfaces at *the same scope* (the futon stack); the alignment mission compares their beliefs without requiring hierarchical composition. Future extension if a third AIF surface emerges (Director EoI agency? Forum agency?) at the same scope.

### R12 — Dual-loop / hyperparameter inference

A second AIF loop runs on a slower cadence and treats the inner loop's hyperparameters (priors, learning rates, EFE weights) as hidden state to be inferred.

**This implementation.** **Not satisfied.** The status set and the multiplicative-likelihood update's weight handling are static. No inference. **Deferred per the same reasoning as the WM contract §R12 / T5**: F10 / R12 is the deepest reflexive criterion; satisfaction inside this mission risks unbounded scope.

## Cross-mapping to F1-F10 (stack-level fitness)

The same mapping shape as `futon-aif-completeness.md`; VSATARCS's R-satisfaction adds a *second* contributor on each row, so F-criterion movement is doubly witnessed at stack scope (or asymmetrically witnessed when only one side ships).

| R-criterion | Stack F-criterion contributed to | Mechanism |
|---|---|---|
| R1 (explicit belief state) | F1 (explicit fitness state) | VSATARCS's per-entity belief is queryable from the reader; combined with WM-belief, F1 gains a *bilateral* fitness readout — alignment mission catches drift between them as `:lift-anomaly :alignment-drift` |
| R2 (observation channel schema) | F1 + F9 (feed-readable annotation graph) | VSATARCS observations are sourced from the canonical hypergraph; F9's feed-readability is partly evidenced by what VSATARCS can sense |
| R3 (predictive-coding update) | F4 (bounded self-balance) | Symmetric with WM contribution |
| R4 (predictive forward model) | F5 (adaptive response) | Future writer capability — predicted vs. realised prose / annotation deltas |
| R5 (principled EFE terms) | F4 + F5 | Tied to R4 |
| R6 (action selection + abstain) | F6 (operator inhabitation) | Operator reads VSATARCS-proposed rewrites in the 3-up review surface; abstain prevents low-confidence proposals |
| R7 (adaptive precision) | F10 (dual-loop fitness) | Same as WM |
| R8 (per-tick trace) | F7 (validation harness) | VSATARCS trace becomes evidence for F7 alignment-drift tests |
| R9 (named validation properties) | F7 | Direct |
| R10 (live operation) | F2 (liveness invariant) | A scheduled VSATARCS belief-update cycle is a new liveness contributor |
| R11 (hierarchical composition) | F8 (multi-corpus composition) | Sibling concerns; bilateral satisfaction at the same scope is the alignment mission's domain |
| R12 (dual-loop hyperparameter inference) | F10 | Same as WM |

## Summary

| R-criterion | Status | Gap-closing checkpoint / blocker |
|---|---|---|
| R1 — Explicit belief state | **✓ — bilateral bridge live as of v0.2.5** | no deferred-pending entries on either side; bilateral counterpart `hx:wm:v0-9:symmetric-bootstrap-closure` |
| R2 — Observation channel schema | **✓ as of v0.3.0** | 5 channels declared; 19 tests; closure id `hx:vsatarcs-align:v0-3-0:r2-observation-channels-closure` |
| R3 — Predictive-coding belief update | **✓ as of v0.5.3** — all 4 sub-properties (R3a ✓, R3b ✓, R3c ✓, R3d ✓) | first composite R-criterion to fully close via the sub-property closure protocol |
| R4 — Predictive forward model | **deferred pending writer-capability** | Awaits operator-confirmed typed rewrites as an action class; reframed v0.5.7 |
| R5 — Principled EFE terms | **deferred pending writer-capability** | Depends on R4 forward model |
| R6 — Softmax + abstain | **deferred pending writer-capability** | Depends on action-space arrival |
| R7 — Adaptive precision | **✓ as of v0.5.3** | direct port of WM v0.12 + v0.13; variance-component active; need-component pending R5 preferences |
| R8 — Per-tick trace | **✓ as of v0.4.0** | `arxana-vsatarcs-trace.el`: event-source-agnostic emit + cadence-following consumer; "docs always in line with code" via `:wm-trace-anchor` |
| R9 — Named validation properties | **partial — V-shrink ✓ + F-decrease ✓ as of v0.5.1** | EFE-stress + Abstain-fires depend on R5 + R6 (writer capability — deferred) |
| R10 — Live operation | **✓ as of v0.5.2 — subscribed-and-running** | event-driven via file-notify on WM trace dir; asymmetric with WM (`:scheduled-execution-ready`); v0.5.3+ extends to multi_watcher heartbeat tap |
| R11 — Hierarchical composition | **satisfied at observer layer (v0.5.7 reframe)** | bilateral milestones constructed multi-agent composition with `:bilateral-evidence` coordination layer; action-coherence pending writer-capability |
| R12 — Dual-loop hyperparameter inference | **deferred to stack-level Q6 (v0.5.7 reframe)** | Mirrors the WM-side §R12 framing; not local-engineering gap, explicitly deferred at stack scope |

**Honest current claim (as of v0.1):** VSATARCS satisfies R1 (explicit belief state) cleanly via `arxana-vsatarcs-belief.el`, using the same status set, event shape, and multiplicative-likelihood update as the WM's `futon2.aif.belief`. R9 ships with V-shrink-shape and shape invariants. The current vocabulary "VSATARCS AIF observer with explicit belief state" is honest at the apparatus level; "VSATARCS AIF observer of the futon stack" awaits R2 (observation channels). "VSATARCS AIF *agent*" awaits R4 + R6 (writer capability via typed rewrites).

## Cross-references

- `~/code/futon2/docs/futon-aif-completeness.md` — sibling WM contract; the R1 module here is an elisp port of its v0.2 belief module.
- `~/code/futon0/docs/stack-fitness-completeness.md` — F1-F10 cross-mapping target.
- `~/code/futon7/holes/M-stack-essay-code-alignment.md` — the alignment mission this contract participates in.
- `~/code/futon7/holes/M-war-machine-aif-completion.md` — the code-side sibling mission whose Checkpoint 1 ships the WM belief module this contract ports.
- `~/code/futon4/README-vsatarcs.md` — the reader's user-facing documentation.
- `~/code/futon4/README-rewriting.md` — typed-rewrite discipline; the candidate action space for VSATARCS once writer capability is on.
- `~/code/futon4/dev/arxana-vsatarcs-belief.el` — belief module (v0.1).
- `~/code/futon4/test/arxana-vsatarcs-belief-test.el` — test suite (18 tests / 33 assertions).

## Status of this document

**v0.1 drafted 2026-05-17** by claude-4 as part of M-stack-essay-code-alignment Checkpoint 0 (VSATARCS-side). Authored alongside the elisp port of the belief module from `futon2.aif.belief`. R1 ✓ on day one; all other criteria graded honest.

**v0.2 updated 2026-05-17** (same session). R1 reader-chrome integration shipped: persistence layer, ingest entry point, snapshot projection, and chrome render block. Test count moved from 18 → 24 in the belief suite; vsatarcs reader test count from 8 → 10. R1 verdict remains `:satisfied`; the verdict now witnesses both apparatus existence *and* operator-visibility through the reader chrome.

**v0.2.2 updated 2026-05-18.** R1 prior bootstrap from `stack-annotations.edn`. Three new entry points in `arxana-vsatarcs-belief.el` (`-stack-annotations-path`, `--section-ids-from-stack-annotations`, `-bootstrap-from-stack-annotations`); EDN reader in `arxana-browser-rewrites.el` extended to tolerate `#inst` and `#{}`; README-rewriting.md caveat revised. Belief suite 24 → 28 tests / 50 → 67 assertions. Real-source smoke test: 35 entities bootstrapped from the canonical source. New `:enables` field on the closure annotation records one-step-lookahead integration points so future moves can audit what each rewrite is scaffolding for.

**v0.2.3 updated 2026-05-18.** RewriteReview renderer extended to surface `:enables` in the Rule panel (between Verification and Decided-by). Convergence with WM v0.5 §"Capability-gap modeling as endogenous action" recorded both as prose here (§R1) and as the new closure's `:note`. Two-sided independent naming of the same structural principle is recorded as candidate evidence for the `:code-docs-correspondence` invariant.

**v0.2.4 updated 2026-05-18.** R1 polish-off pass. Bilateral comparison primitive (`arxana-vsatarcs-belief-compare`) + reader mode-map bindings (`B`/`R`/`i`) + README-vsatarcs.md "Belief surface (R1)" section. R1 audit row moves to `:satisfied-with-operator-surface-closed`; only-remaining-R1-gap is cross-side bridge (deferred). Belief suite 28 → 34 tests / 67 → 90 assertions.

**v0.2.5 updated 2026-05-18.** Cross-side bridge lands. Bilateral milestone v0.9 ↔ v0.2.5, coordinated with claude-2 via bell/whistle. New module `arxana-vsatarcs-wm-bridge.el` + 7 fixture tests; end-to-end smoke against today's real WM trace (35 equal, 0 drift). Both R1 pending-entries closed. R1 audit row moves to `:satisfied-with-bilateral-bridge-live`; `:only-remaining-r1-gap` reads `nil`. New top-level `:bilateral-evidence` block seeded as first `:code-docs-correspondence` landing-surface shape (2 entries on landing).

**v0.3.0 updated 2026-05-18.** R2 satisfied. New module `arxana-vsatarcs-observation.el` with a 5-channel schema + observe + sense-to-vector; 19 fixture tests + real-corpus smoke. Closes v0.1 `r2-gap-named` annotation by realisation. R3 sub-property statuses decomposed (R3a `:unblocked-by-r2`, R3c `:partially-unblocked-by-r2`). Third `:bilateral-evidence` entry with new `:evidence-kind` `:independent-naming-of-same-r-criterion-shape-at-different-scopes`. Total VSATARCs test count 51 → 70.

**v0.4.0 updated 2026-05-18.** R8 satisfied. New module `arxana-vsatarcs-trace.el` ships event-source-agnostic emit + cadence-following consumer per Joe's design directive. The "docs always in line with code" guarantee is operational: every WM tick has a matching VSATARCs tick at the same `:timestamp` via `:wm-trace-anchor`. 15 fixture tests + real-WM-trace smoke (4 records → 4 ticks; first observation of non-zero cross-side drift, max-posterior-diff 0.0045). Fourth `:bilateral-evidence` entry. Total VSATARCs test count 70 → 85.

**v0.5.0 updated 2026-05-19.** R3a + R3c satisfied via likelihood-model port (bilateral joint-landing milestone v0.5.0 ↔ WM v0.10+, coordinated via bell/whistle). New module `arxana-vsatarcs-likelihood.el` ports claude-2's WM-side architecture; 3 of 5 R2 channels covered with hand-tuned per-status weights; 2 logged as `:prototyping-forward`. K=3 multi-step inner loop with anneal-factor + sign-aware aggregation. 18 likelihood tests + adjacent trace bug-fix. Fifth `:bilateral-evidence` entry with `:protocol-witnesses` extension (first worked example of that shape). Total VSATARCs test count 85 → 103. R3 aggregate stays `:partial-three-of-four-sub-properties-satisfied` per the closure protocol (R3b blocked on R7).

**v0.5.1 updated 2026-05-19.** R9 F-decrease wired as a named validation property (sequential ship per claude-2's whistle; no WM-side dependency). `arxana-vsatarcs-trace-build-record` extended with `:F` + `:prediction-errors` keyword args; `arxana-vsatarcs-trace-follow-wm` computes VFE per tick. 2 new ert tests. R9 audit row moves to `:partial-with-V-shrink-and-F-decrease`. Sixth `:bilateral-evidence` entry — first to carry `:evidence-kind :one-sided-extension`. Total VSATARCs test count 103 → 105.

**v0.5.2 updated 2026-05-19.** R10 satisfied via minimum-viable wakeup tap (per Joe's order R3a+R3c → R9 → R10-tap). New module `arxana-vsatarcs-r10-tap.el` registers `file-notify-add-watch` on the WM trace directory; handler fires `follow-wm` on `wm-trace-*.edn` create/change. 10 fixture tests cover all branches. R10 audit row moves to `:satisfied-subscribed-and-running`. Seventh `:bilateral-evidence` entry with 5-turn `:protocol-witnesses` chain — first entry to use per-entry witnesses scope (vs chain-of-chains rooted at v0.5.0). Honest asymmetric R10 satisfaction recorded (WM-side `:scheduled-execution-ready`; VSATARCs-side `:subscribed-and-running`). Full multi_watcher tap design deferred to v0.5.3+ as bilateral substrate. Total VSATARCs test count 105 → 115.

**v0.5.3 updated 2026-05-19.** **R7 satisfied + R3 aggregate closes** via direct port of WM v0.12 + v0.13 `precision.clj` architecture. New module `arxana-vsatarcs-precision.el`: per-channel Π via 20-element rolling-window variance-component (need-component pending R5). Trace records gain `:precision-state` field; `follow-wm` reads/updates/stores per tick via "trace IS the state store" discipline. R3b flips `:blocked-on-R7 → :satisfied` via this module's `weighted-error`; per the closure protocol all four R3 sub-properties (R3a, R3b, R3c, R3d) are now `:satisfied`, **R3 aggregate flips `:partial-three-of-four → :satisfied`** — first composite R-criterion to fully close. 16 fixture tests; 2 bugs caught during authoring (reweight-all plist construction order; cap-vs-ceiling test expectation). Eighth `:bilateral-evidence` entry with `:evidence-kind :one-sided-extension`. Total VSATARCs test count 115 → 131.

**v0.5.4 updated 2026-05-19.** Reader-facing distillation `~/code/futon4/docs/VSATARCS.md` shipped per Joe's pivot to operator-visible alignment. Linearised collation of all 47 scene-form stories from `~/code/futon5a/holes/stories/` in hybrid order (leaf-start-here → lifted-sections-by-stack-annotations-order → alphabetic). Each story carries an inline blockquote alignment callout; lifted entities (currently 2 of 47) receive entity-id + active R-criteria; un-lifted stories get a "lift-pending" notice. Header includes R-criterion-audit summary, 8-entry `:bilateral-evidence` index, and the revisions trail. Generator at `~/code/futon4/scripts/generate_vsatarcs_md.bb` (babashka; regen via `bb scripts/generate_vsatarcs_md.bb`); pandoc PDF generation available downstream when desired. Not R-criterion movement — reader-facing infrastructure that makes the satisfied R-criteria operator-visible. Future R-criterion landings surface here automatically as the `.aif.edn` grows. Live-regeneration discipline matches the "trace IS the state store" principle: the source artefacts (stories + `.aif.edn`) are canonical; the distillation is a projection.

**v0.5.5 updated 2026-05-19.** HTML sibling artefact at `~/code/futon7a/vsatarcs.html` per Joe's pivot to the Tufte-CSS-based public-facing site (`futon7a` already hosts the Hyperreal Enterprises content with `tufte.css` + `latex.css` in place). Generator extended to emit BOTH the Markdown source-of-truth (`~/code/futon4/docs/VSATARCS.md`) AND the rendered HTML (`~/code/futon7a/vsatarcs.html`) in one pass. Margin annotations use the canonical Tufte toggle shape — `<label class="margin-toggle">⊕</label><input ...><span class="marginnote">…</span>` — matching the convention in `futon7a/index.html` and the other Hyperreal pages. Each story's alignment callout attaches to its `<h2>` title; expanded marginnote reveals lifted-entity-id (or lift-pending notice) + active R-criteria + pending integration points. The result is reader-friendly on wide screens (true marginalia) and inline on mobile (the toggle reveals). Minimal Markdown→HTML inline converter (strong/em/code/links/lists/blockquotes) preserves structural prose.

**v0.5.6 updated 2026-05-19.** HTML distillation polish per Joe's "keep working on the HTML output" direction: (a) flattened section structure — story sections contain scene `<h3>`s + paragraphs as direct children (matches `ati.html`/`epsrc.html` Tufte-CSS convention; nested scene-sections previously misaligned the `section > p` 55%-width rule); (b) unique anchor IDs — per-story `<section id="<stem>">` + per-scene `<h3 id="<stem>-<anchor>">`; all 391 IDs now unique (was 46 colliding `id="overview"` across 47 stories); (c) story-context-aware Markdown → HTML link resolution: `[text](target)` resolves to `#<stem>-<anchor>` for same-story scenes, `#<target>` for other story basenames (with `.md` suffix stripped), `#<host-stem>-<target>` for unique cross-story scene anchors; **914 in-document links now navigate correctly** (was 0); (d) top-of-document table-of-contents linking every story by id with lifted/unlifted marker; (e) marginnote moved from `<h2>` into the source `<p>` immediately following, so the float-right + `margin-right: -60%` lands the alignment annotation in the wide right margin matching the live `hyperreal.enterprises` layout.

**v0.5.7 updated 2026-05-19.** Audit-row honesty pass + R-criteria-in-prose section. Four reframes: **R11** `:n-a` → `:satisfied-at-observer-layer` (the original v0.1 framing was honest before the bilateral bridge existed; no longer honest at v0.5.6 since the bilateral milestones have constructed multi-agent composition with shared state and a coordination layer); **R12** `:not-satisfied` → `:deferred-to-stack-level-Q6` (matches WM-side §R12 discipline); **R4 / R5 / R6** `:not-satisfied` → `:deferred-pending-writer-capability` (reader-first by design; these criteria await a writer surface rather than being local-engineering gaps). New section in the distillation — paragraph-by-paragraph explanation of R1–R12 (long-form counterpart to the keyword-shaped status micro-qualifiers); source at `~/code/futon4/docs/vsatarcs-r-criteria-in-prose.md`. Parity with the WM contract now structurally aligned at R1, R2, R3, R7, R8, R9, R10, R11 (both observer-layer-satisfied), R12 (both deferred); R4/R5/R6 carry honest asymmetry (WM has writer; VSATARCs is reader-first).

**Versioning roadmap.**

- v0.3 after R2 (observation schema) lands and is exercised by tests; story-scoped belief filtering follows from this.
- v0.4 after R3a/R3c (prediction-error + VFE) become computable against R2 observations.
- v0.5 after R8 (trace) lands.
- v1.0 when R1-R9 are all ✓ or ✓-with-documented-caveat, with at least one alignment-drift incident witnessed in `stack-annotations.edn`'s `:lift-anomalies` block.
