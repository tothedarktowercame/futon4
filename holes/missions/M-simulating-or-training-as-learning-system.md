# M-simulating-or-training-as-learning-system

> **Companion mission to** `M-or-training-as-learning-system.md`. Where the parent mission identified gaps in the *ORP-to-UKRN-S transition* that the working paper could not close on its own evidence, this mission identifies gaps in the *simulation* that supports the working paper's design recommendations and aims at a simulation we can be confident about.

**Mission status:** IDENTIFY drafted 2026-05-07. MAP / DERIVE / ARGUE / VERIFY / INSTANTIATE / DOCUMENT to follow.

**Source material primarily consulted for IDENTIFY:**
- `/home/joe/npt/working-paper/UKRN_WP_draft_v9.md` (current paper)
- `/home/joe/npt/working-paper/argument_v9.sexp` (argument structure with W8 facet-warrant; H7-H9 holes; A7 attack; R3 downgrade; R6 empirical-headline finding)
- The eight prior working-paper drafts `UKRN_WP_draft_v[2-8].md` and the simulation-relevant notebooks under `/home/joe/npt/working-paper/notebooks/`
- The companion mission `/home/joe/code/futon4/holes/missions/M-or-training-as-learning-system.md`
- `/home/joe/npt/working-paper/reachability_corrected_from_orp_survey.md` (the substrate that underwrites the empirical reachability landscape)
- This session's diagnostic findings about single-seed RNG fragility, tautological band-to-outcome mapping, and missing facet structure.

---

## 1. IDENTIFY

### The gap

The parent mission established a design recommendation for UKRN-S resting on a small computational simulation. That simulation has been useful as a stress-testing harness for the design's claims. But under the scrutiny it has now received — RNG-control audit, calibration against an empirically-grounded landscape, attempts to derive defensible parameter contrasts — the simulation has revealed that several of the working paper's load-bearing claims are not yet things the simulation can warrant.

The gap, named directly: *we do not yet have a simulation we are confident about*. We have an instrument that produces trajectories, that responds to parameter choices in some ways, and that has been useful for narrative pedagogy. But the instrument produces findings that vary with sampling seed, derives some of its differentiation from the calibration choices rather than from the dynamics, and is silent on the things the working paper now most wants to claim — facet-level reach, capabilities-coming-online for UKRN-S and for institutions, time-anchored trajectory readings.

This mission's purpose is to construct (or substantially re-construct) a simulation that *can* warrant the working paper's claims, that exposes its commitments rather than hiding them, and whose findings survive multi-seed validation against an empirical landscape that is calibrated rather than self-selected.

### Sorries (in the Lean proof-obligation sense)

The IDENTIFY phase names nine `sorries` — places the current simulation cannot yet speak. Each has a closure obligation that later phases of this mission should discharge.

#### Sorry 1 — Facet-level reach is unmodeled.

The current simulation operates on whole-institution reach (a single scalar D per institution at start, lifted by the AIF agent over 80 ticks). The working paper's Decision 5 (added 2026-05-07) and Finding F-sub-4 prescribe facet-level reach: training topic × audience community × possibly recipient role. Bulk T1 counts and bulk reachability conceal exactly the heterogeneity that the design recommendation now turns on.

Without facet-level reach in the simulation, the design recommendation at the facet level cannot be verified. The simulation is silent on whether 5% reach with strong uptake in one community produces materially different downstream outcomes from 40% reach with weak uptake in another. This is the single largest missing capability in the current model.

**Closure obligation:** the new simulation must operate on faceted reach as its native unit. The new pattern `system-coherence/facet-before-aggregating` is the warrant.

#### Sorry 2 — Capabilities-coming-online for UKRN-S is unmodeled (and a UKRN-S abilities schema does not yet exist).

The working paper describes UKRN-S as a *learning system*: the Evaluator role appointed early; the indicators-track maturing from pilot to detection-at-scale over years 2-3; the Convener arm building OSMI/UUK relationships over time; the Researcher-Advisor role producing successive iterations of verification. Each of these is a *capability* that comes online at a particular point and grows in scope and depth thereafter.

There is an asymmetry in the substrate available for modelling this. The ORP stage left behind a detailed, publicly-available *offerings schema* (`https://www.ukrn.org/training-schema/`) — a structured catalogue of training products, levels, and intended audiences. Nothing equivalent exists for UKRN-S' abilities. The closest available substitute is the working paper's own five-role analysis (Training Manager, Evaluator, Convener, Researcher-Advisor, Institutional Leads — each with its supplies, limitations, and uniquely-supplied function), but that is a *narrative* description of an equippage, not a structured schema of UKRN-S capabilities. It tells us what each role does well in prose; it does not give us a state space over which capabilities can be modelled as entities that come online.

So the current simulation is doubly silent on this. First, it has *static* patterns and a *static* AIF agent — the agent's design configuration is fixed at the start of each run, so capability development is invisible. Second, even if we wanted to add capability dynamics, we don't yet have the schema that would tell us *what the capabilities are* in a form that the simulation could carry. The narrative description in §3 of the working paper is not yet refactored into a schema with capability identifiers, entry conditions, depth or coverage gradients, and observable signs.

This means the simulation cannot test the most distinctive claim of the working paper's design — that UKRN-S' value comes from being a learning system rather than a static service. The simulation is silent on which UKRN-S capabilities matter most for which institutional outcomes, on the order in which capabilities should come online, and on what depth of capability is needed before the system as a whole reaches an operational mode.

**Closure obligation (two-part):**
1. *Construct a UKRN-S abilities schema*, drawing on but going beyond the v9 five-role analysis. This is preparatory work for the simulation but is itself a research output: a structured catalogue of UKRN-S capabilities, with capability identifiers, entry conditions (what brings each capability online), depth/coverage gradients (binary, scaled, or multi-dimensional), observable signs (how an external observer would tell whether the capability is online), and dependencies between capabilities (which require which to function). The ORP training-schema is the natural shape comparison — UKRN-S' equivalent should match that level of structural detail. *This is not solved at IDENTIFY; flagging it as a deliverable for MAP and DERIVE.*
2. *Add a capability-state representation to the simulation*, once the schema exists, with explicit transition rules and observable consequences for the simulation's outputs (delivery viability, architectural sustainability, institutional outcomes).

#### Sorry 3 — Capabilities-coming-online for institutions is unmodeled.

Symmetrically, institutions develop capabilities over time. The working paper's institutional-conditions argument names five candidate signals: role ownership for open research; in-flight evaluation loops; professional-services coordination; leadership support; reward and recognition mechanisms. Each is a binary or scaled capability the institution either has, is developing, or lacks.

The current simulation has 16 NPT factors per institution that are lifted by levers — but the NPT factor scores are continuous and the levers' effect is small per tick. There is no representation of an institutional capability *coming into being* (e.g., "Institution H4 appointed an open-research coordinator at month 6") or persisting once established.

**Closure obligation:** the new simulation must represent institutional capabilities as entities that come online and persist, with explicit entry conditions (what triggers them) and exit conditions (what causes them to lapse). The five candidate signals from the working paper are the natural starting set.

#### Sorry 4 — Time anchoring is absent.

The current simulation runs 80 ticks. Whether 80 ticks corresponds to 1 year of UKRN-S operation, 5 years, or 10 years is unspecified. Without a real-time anchor, the trajectories cannot be read as forecasts; they are descriptive only. This is hole H7 in `argument_v9.sexp`.

The absence of time anchoring matters because the working paper makes time-anchored claims: the bootstrap is a six-month plan; the Evaluator role should be appointed in Q1 2026 with configuration decided by Q2; the next iteration runs after the first T1 wave (currently scheduled). None of these can be checked against simulation output without time calibration.

**Closure obligation:** the new simulation must have explicit time semantics — each tick should correspond to a defined real-time interval — and the calibration should be defended against observable cadence (e.g., a quarterly T1 reporting wave; an annual indicator monitoring run; a 5-year sector cycle).

#### Sorry 5 — Single-seed simulation was treated as evidence.

The original v9 §4 narrative (now revised) included claims like "all-coupled vs no-EV-Convener produces a 0.13 architecture decline" — claims that turned out to be RNG sampling artefacts once the rngs were reset between configurations. This is hole H8 in `argument_v9.sexp` and is a methodological discipline failure: stochastic simulations require multi-seed averaging before per-parameter contrasts can be reported as findings.

**Closure obligation:** the new simulation must be run with multi-seed averaging (≥20 seeds per configuration) as standard. Per-parameter contrasts are reported only with their cross-seed mean and variance. The deferred pattern `system-coherence/single-seed-results-need-multi-seed-validation` will be authored alongside this mission's MAP/DERIVE phases.

#### Sorry 6 — A-axis empirical grounding is uniform-within-band placeholder.

The empirical reachability landscape places institutions on the D axis using corrected ORP reachability bands. The A axis uses uniform spacing within band as a placeholder. The working paper acknowledges this and points at WP02/WP09 (indicators-track) as the source of empirical A — once those indicators mature from pilot scale to per-institution monitoring.

**Closure obligation:** the new simulation should accept empirical A inputs once available, and have a defined fallback for institutions where A is partially observed. Until then, the mission should clarify what kinds of A specifications change the results vs which don't, so we know which findings are robust to A and which depend on the placeholder.

#### Sorry 7 — Tautology between band assignment and outcomes.

The empirical landscape result (R6) says the design lifts higher-reach bands into Multiplied and cannot lift the thin-reach band. But starting D values were derived from band assignment, and the model's dynamics asymmetrically lift A and saturate at threshold. So the only profiles that fail are those whose starting D was placed below the floor by band assignment. The "design works for high-reach, fails for low-reach" headline may be largely a consequence of calibration choices rather than of differential dynamic behaviour. This is attack A7 in `argument_v9.sexp`.

**Closure obligation:** the new simulation must break this tautology — by introducing dynamics that can lift D from below-floor positions under specific design conditions (e.g., ringfenced funder pull plus institutional capability coming online), or by providing a stress test that shows starting position is not the dominant predictor of outcome.

#### Sorry 8 — Per-profile (per-institution) agent is absent.

The current AIF agent acts at the population level: one tension vector chosen per tick, applied to all profiles simultaneously. UKRN-S in reality serves different institutions differently — light-touch consulting for high-capacity institutions; substrate-building for thin-reach institutions; topic-faceted offerings for institutions with specific demand profiles.

A population-level agent cannot represent these differential interventions. It also cannot test the working paper's recommendation that the Evaluator role should be configured differently for different bands of institutional participation.

**Closure obligation:** the new simulation should have at least the option of per-profile or per-band agency — the design intervention can be different for different institutions. This may require restructuring the AIF agent into a hierarchy (population-level prior + per-profile policy refinement), or separating UKRN-S' static service offer from its differentiated delivery.

#### Sorry 9 — Missing institutions leave the population partial.

Four of the 24 institutions in the T1 reporting sheet are missing from the empirical landscape because their survey-side denominators are unknown. Beyond those, the wider UK research-culture ecology includes universities not in the ORP cohort at all. The simulation's results extrapolate from 16 to "the network", but the network is larger and more heterogeneous than the 16 represented.

**Closure obligation:** the new simulation should have a defined extrapolation strategy (or refusal to extrapolate) for institutions outside the calibrated set. As Codex's reachability work matures and additional denominators arrive, the simulation should be re-run with the expanded set.

### What closing the gaps looks like

A simulation we can be confident about is one that satisfies, jointly:

- **Faceted unit of analysis.** Reach, intervention, and outcome are all expressed at the level of (training topic × audience community), not at the level of whole institution. Aggregations are computed but not used as the primary input to recommendations.
- **Capability dynamics for both UKRN-S and institutions.** The state of the system at any tick includes which UKRN-S capabilities are online and at what depth, and which institutional capabilities are online for each institution.
- **Time-anchored runs.** Each tick has a defined real-time interpretation. Forecasts are reported as "by end of year 2" or "by Q2 2027", not as "by tick 40".
- **Multi-seed standard.** All findings reported with cross-seed mean and variance; single-seed runs labeled as illustrative only.
- **Empirically-anchored landscape on both axes.** D from corrected reachability (per facet); A from indicators-track data once available, with placeholder caveats explicit until then.
- **Differentiated agent.** The design intervention can vary by institution, by band, or by facet — at minimum as a tested option.
- **Tautology audit.** Outputs are systematically tested for sensitivity to starting position vs sensitivity to dynamics; findings claim only what survives that test.
- **Defined extrapolation policy** for institutions outside the calibrated set.

A simulation satisfying these properties would be a research-grade artefact, suitable for publication as a methods supplement and capable of supporting the working paper's claims under expert review.

### Theoretical anchoring

The new simulation will preserve the two strongest framings from prior drafts and abandon or demote the others:

- **Active inference (AIF) as the agent model.** The AIF cyber-ant from `ukrn_cyberant_aif.clj` survives because the expected-free-energy framing handles uncertainty, prior preference, and pattern-mediated guidance in a principled way. AIF will likely be retained, possibly extended hierarchically.
- **Pattern-mediated coordination.** The pattern model (CA, PI, SM, CP, VI plus programme-level patterns) survives because patterns are the legible interface between the WP's qualitative findings and quantitative dynamics.
- **NPT-factor population substrate.** The 16-factor scoring of institutions provides the structural detail that lets patterns differentiate between institutions. Survives, possibly with facet-level extension.
- **MVSG / geometric framing.** Demoted to descriptive language only (per R2). No simulation-level commitment.
- **Bayesian scenario analysis** (`ukrn_learning_system.clj`, v4-v8 era). Provides uncertainty quantification that the AIF version doesn't currently expose. Worth re-integrating: the multi-seed AIF runs could feed Bayesian-scenario-style posteriors over per-band outcomes.

The new framing addition needed is **capability-state tracking**: each entity (UKRN-S, each institution, each facet) carries a vector of capability states (online / developing / absent) that evolve under explicit transition rules. This is closer to dynamical-systems modeling than to AIF; the two will need careful composition.

### Scope in

- Constructing the new simulation in Clojure (extending the existing notebook stack).
- Multi-seed validation harness; per-seed result recording; cross-seed aggregation.
- Capability-state representation for UKRN-S and institutions; transition rules.
- Faceted reach as the native input unit; facet-level outcome reporting.
- Time-anchoring calibration; real-cadence interpretation per tick.
- Tautology-audit instrumentation: sensitivity testing of outputs vs starting positions vs dynamics.
- Re-running the v9 paper's headline findings (R6 most important) under the new simulation; reporting which survive and which require qualification.
- Authoring the deferred `system-coherence/single-seed-results-need-multi-seed-validation` pattern.
- A methods-supplement document for the working paper, describing the simulation, its commitments, and the robustness tests it has passed.

### Scope out

- Fundamental departures from active inference as the agent framework (preserved unless evidence forces revision).
- Empirical fieldwork to gather the missing institutional capability data (mission acknowledges this gap; does not undertake the fieldwork).
- Generalisation beyond the UKRN-S use case (the simulation is purpose-built for this specific working paper; reuse is welcome but not designed for).
- New qualitative coding of the focus-group transcripts (the existing role codebook and pattern set are inputs, not subjects of revision).
- Direct intervention in the v9 paper text beyond noting which v9 claims survive and which need qualification.

### Completion criteria

This mission is complete when:

1. The new simulation runs end-to-end on the empirical 16-institution landscape, with multi-seed averaging, faceted reach inputs, and capability-state tracking.
2. Each of the nine sorries above has a documented closure status (resolved / partially resolved / acknowledged as remaining / scope-out).
3. The R6 empirical-headline finding from the parent mission has been re-tested under the new simulation; the result (whatever it is) is reported with cross-seed mean and variance.
4. The pattern `system-coherence/single-seed-results-need-multi-seed-validation` is authored and warranted in `argument_v9.sexp`.
5. A methods supplement document exists, suitable for inclusion as an annex to the working paper or as a standalone technical note.
6. The mission file's INSTANTIATE phase contains an explicit handoff: what UKRN-S' Researcher-Advisor role inherits when the WP's next iteration is run.

### Relationship to other missions

- **Parent:** `M-or-training-as-learning-system.md`. This mission's reason to exist is to discharge the verification debt that the parent mission could not fully discharge with the v9 simulation as it stood.
- **Sibling:** the deferred `single-seed-results-need-multi-seed-validation` pattern authoring is an output of this mission, not a separate one — see Completion Criteria #4.

### Source material — fuller inventory

#### Why the prior eight drafts had simulation gaps

A look back through the working-paper draft history shows three distinct simulation paradigms attempted in succession:

- **v3-v4 (geometric framing).** Closed-triangles-to-tetrahedra; MVSG-as-attractor. The geometry was intuitive but the simulation was static — there were no dynamics, only stage-by-stage scoring. Gap: no time evolution; no uncertainty quantification; framing did not bear weight when later tested (R2 in v9).
- **v4-v8 (Bayesian scenario analysis,** `ukrn_learning_system.clj`**).** Beta priors over institutional context; scenarios as conditional probability calculations. Strong on uncertainty quantification but treated each scenario as a snapshot, not as a trajectory. Gap: no dynamics; no agent.
- **v8 (calibrated phase-space simulation, Figure 5).** Four-stage developmental sequence with explicit lever effects. Brought dynamics in but stages were prescribed (not derived from agent choices). Gap: no agency; no per-tick selection among alternatives; no stochastic sampling.
- **v9 (AIF cyber-ant** `ukrn_cyberant_aif.clj` **+ mission verify** `ukrn_mission_verify.clj`**).** Active inference at population level; expected-free-energy minimisation; pattern-mediated guidance; stochastic candidate sampling. Brought agency in but introduced single-seed fragility and calibration-tautology issues — exactly the gaps this mission addresses.

Each paradigm fixed problems of the previous one and introduced its own. The trajectory has been from static-prescriptive (v3-v4) → static-uncertainty (v4-v8) → dynamic-prescriptive (v8) → dynamic-stochastic (v9). The next paradigm — what this mission must build — is **dynamic-stochastic-faceted-time-anchored-multi-seed**, with explicit capability-state tracking. That is a fifth paradigm, not a refinement of the fourth.

#### Notebook inventory and intended fate under the new simulation

| Notebook | Lines | Role under v9 | Intended fate |
|---|---:|---|---|
| `ukrn_phase_space.clj` | 278 | Population substrate; D/A computation from NPT factors | Retained; possibly extended for facet-level NPT factors |
| `ukrn_cyberant.clj` | 436 | Heuristic cyber-ant (pre-AIF) | Likely retired |
| `ukrn_cyberant_aif.clj` | 715 | AIF agent | Retained; restructured for hierarchical/per-profile agency |
| `ukrn_mission_verify.clj` | 243 | Five-experiment sweeps | Retired in current form; multi-seed wrapper replaces |
| `ukrn_mission_verify_export.clj` | 125 | CSV export for plotting | Retained, extended |
| `ukrn_synth_landscape.clj` | 174 | Synthetic 5×5 grid stress-test | Retained as sensitivity-test fixture |
| `ukrn_reachability_landscape.clj` | 139 | Empirical 16-institution landscape | Retained; faceted version added |
| `ukrn_learning_system.clj` | 1287 | Bayesian scenario analysis (v4-v8 paradigm) | Re-integrated as posterior-aggregation layer over multi-seed AIF runs |
| `ukrn_population_model.clj` | 579 | Population model documentation | Retained |
| `ukrn_working_paper.clj` | 2203 | Paper rendering | Untouched by this mission |
| `ukrn_wp_supplement.clj` | 147 | Supplement rendering | Possibly extended for the methods supplement |

### Owner and dependencies

- **Owner.** Joseph Corneli (Researcher-Advisor role from the parent mission).
- **Dependencies for closure:**
  - WP02/WP09 indicators-track data per institution (Sorry 6) — currently pilot-scale; required for empirical A.
  - Codex's full reachability set (Sorry 9) — currently 16 of 24 institutions covered.
  - First T1 evaluation wave outcome data (parent mission Sorry 1) — feeds calibration of dynamics' speed (Sorry 4).
  - May 12, 2026 UKRN-S vision-day timing constraint: this mission does not need to be complete before that date, but its IDENTIFY framing should inform what is presented and what is held back as "more research is needed". (Today is May 7. This mission's IDENTIFY is the framing input for the away day.)

### Exit criterion

This mission exits the IDENTIFY phase when:

- The nine sorries have been reviewed and either confirmed, refined, or refuted.
- The owner has agreed the scope-in/scope-out boundaries.
- The MAP phase can begin with a concrete substrate inventory of which existing assets serve which closure obligations.

### Notes on this draft

- IDENTIFY drafted directly from session work on 2026-05-07, while the v9 paper, the argument_v9.sexp, and the empirical landscape figure are all freshly in mind.
- The nine sorries are all defensible from the current paper's own commitments and from the audit findings of this session. None are speculative.
- The mission is large — eight or nine sorries with non-trivial closure obligations is more than a fortnight of work. Realistically, closing a subset (Sorries 1, 2, 3, and 5 are the most consequential for the working paper) by the next paper iteration is a defensible target. Sorries 6, 7, 8, 9 may need their own follow-on missions or be folded into longer-term iterations.
- **A note on the parent mission's PUR-pending pattern.** The parent mission's ARGUE phase induced a candidate pattern `equippage-roles-with-tradeoffs`. This mission may induce one or two more candidates: probably a methodology pattern about multi-seed validation, and possibly one about capability-state representation in dynamic simulations.

---

## 2. MAP

### Frame: relative to the working paper stack and prior simulation paradigms

The new simulation does not start from zero. The four prior paradigms (geometric → Bayesian → phase-space → AIF) each left assets behind that may serve the new construction. The MAP question is: for each sorry's closure obligation, which existing asset can serve, which needs adaptation, and which requires fresh authorship?

The new simulation also sits in a particular position in the working-paper stack: it must be capable of supporting the v9 paper's headline claims (especially R6, the empirical landscape result) but with multi-seed and tautology-audit rigor that v9 did not achieve. It need not be the simulation for *all* future iterations, but it must be the simulation for the v9-iteration-2 paper run after fresh T1-wave evidence arrives.

### Input → Process → Output: three triangulating tracks

The new simulation can be triangulated by three tracks, each contributing different closure obligations:

#### Track 1 — Theoretical

- **Active inference (AIF) as agent framework.** Retained from v9. Provides expected-free-energy minimisation, prior preference, ambiguity vs risk decomposition, pattern-EFE adjustment.
- **Pattern-mediated coordination.** Retained: 5 institution-level patterns + 3 programme-level patterns + their tension targets. Patterns are the legible interface between qualitative findings and quantitative dynamics.
- **NPT 16-factor population substrate.** Retained for the institutional-state vector under each profile. Possibly extended to facet-level NPT (per-discipline NPT scores).
- **Capability-state dynamics (NEW).** Required for Sorries 2 and 3. State variable per UKRN-S capability (online / developing / absent) and per institutional capability. Transitions under explicit rules.

The theoretical track contributes to closure of Sorries 2, 3, 7, 8.

#### Track 2 — Empirical

- **T1 reporting sheet** (24 institutions; bulk attendance counts; some quarterly history). The base substrate for any reach claim.
- **Corrected ORP reachability** (16 of 24 institutions; reachability_corrected_from_orp_survey.md). Bands derived from this; D-axis anchor.
- **ORP-survey-2025 data** (`/home/joe/ORP-survey-2025/`). Includes survey-side denominators, possibly with discipline / role breakdowns that could feed faceted denominators.
- **ORP training-schema** (`https://www.ukrn.org/training-schema/`). Public catalogue of offerings; structurally rich; *never used by the simulation to date*. Natural input for facet structure (Sorry 1) and structural reference for UKRN-S abilities schema (Sorry 2).
- **Indicators-track data** (WP02 surveys + WP09 pilots). Currently pilot-scale; sparse per institution. The empirical anchor for A axis (Sorry 6) once mature.
- **Maria's combined post-session + follow-up form.** Pilotable but not yet deployed at scale. The instrument that would bring Sorry 6 closer to closure for trainee-level outcomes.
- **Focus-group transcripts** (T1, T2A, T2B, T3). Already coded in v9-era work; possibly re-codeable for facet signals or capability signals.
- **Codex's reachability extension** (in progress at session time). Will expand the 16-institution coverage as additional survey-side denominators land.

The empirical track contributes to closure of Sorries 1, 6, 9 directly and supports calibration for all others.

#### Track 3 — Methodological

- **Multi-seed validation harness.** Required for Sorry 5. The current `reset-rngs!` mechanism in `ukrn_mission_verify_export.clj` is the seed of this — it can be parameterised over seeds rather than hardcoded.
- **Tautology audit.** Required for Sorry 7. A sensitivity test: for each empirical landscape outcome, vary starting position vs vary dynamics independently; report which is the dominant predictor.
- **Time-anchoring discipline.** Required for Sorry 4. Each tick has a documented real-time interpretation; calibration against observable cadence.
- **Faceted reporting.** Required for Sorry 1. Outputs at the (topic × audience-community) level by default; aggregations only when interpretively defensible.

The methodological track contributes to closure of Sorries 4, 5, 7 directly and disciplines all reporting.

### Inventory: existing capabilities (notebook assets)

| Asset | Lines | What it does | Reusable as-is | Needs adaptation | To retire |
|---|---:|---|:-:|:-:|:-:|
| `ukrn_phase_space.clj` | 278 | NPT-factor → D, A computation; population step dynamics; profile substrate | ✓ |   |   |
| `ukrn_cyberant.clj` | 436 | Heuristic cyber-ant (pre-AIF) |   |   | ✓ (superseded by AIF) |
| `ukrn_cyberant_aif.clj` | 715 | AIF agent; expected-free-energy; pattern-EFE; softmax candidate sampling |   | ✓ (hierarchical extension for Sorry 8) |   |
| `ukrn_mission_verify.clj` | 243 | Five-experiment sweeps (single-seed) |   |   | ✓ (replaced by multi-seed wrapper) |
| `ukrn_mission_verify_export.clj` | 125 | RNG-reset + CSV export |   | ✓ (extend for multi-seed loop) |   |
| `ukrn_synth_landscape.clj` | 174 | Synthetic 5×5 grid stress-test fixture | ✓ (sensitivity-test fixture) |   |   |
| `ukrn_reachability_landscape.clj` | 139 | 16-institution empirical landscape; band placement |   | ✓ (extend to faceted version) |   |
| `ukrn_learning_system.clj` | 1287 | Bayesian scenario analysis; Beta priors over institutional context |   | ✓ (re-integrate as posterior-aggregation layer over multi-seed AIF) |   |
| `ukrn_population_model.clj` | 579 | Population model documentation | ✓ |   |   |
| `ukrn_working_paper.clj` | 2203 | Paper rendering | n/a (not simulation work) |   |   |
| `ukrn_wp_supplement.clj` | 147 | Supplement rendering | n/a |   |   |

### Inventory: existing schemas and theoretical primitives

| Primitive | Where it lives | Closure obligations served |
|---|---|---|
| 16 NPT factors (4 domains × 4 sub-factors) | `assumptions-v2.edn`, `ukrn_phase_space.clj` | Substrate for institutional capabilities (Sorry 3) |
| 5 institution-level patterns (CA, PI, SM, CP, VI) | `ukrn_cyberant.clj` | Substrate for capability transitions; observable signs |
| 3 programme-level patterns | `ukrn_cyberant.clj` | UKRN-S capability candidates (Sorry 2) |
| 12 roles + 61 action codes (ORP role codebook) | `ORP Training Team Personas and Processes(2).xlsx` (referenced in CLAUDE.md memory) | UKRN-S abilities schema basis (Sorry 2.1) |
| 6 tension dimensions | `ukrn_phase_space.clj` | Policy-space inputs to AIF agent |
| 5 levers (publish-scaffold, delivery-spine, cop-infrastructure, evaluation-integration, orca-support) | `assumptions-v2.edn` | The currently-implemented capabilities (Sorry 2 baseline) |
| Pattern → tension → lever → NPT-factor mapping | Distributed across notebooks; partially reconstructed in this session | Required for capability-effect propagation; should be made first-class |
| ORP training-schema (offerings catalogue) | `https://www.ukrn.org/training-schema/` | Facet structure (Sorry 1); structural reference for UKRN-S abilities schema (Sorry 2.1) |
| 5 institutional-condition signals (role ownership, eval loops, prof-services coord, leadership support, reward/recognition) | v9 §3 narrative; slide-deck recommendations | Institutional capability schema starting set (Sorry 3) |
| 5 v9 roles (TM, Evaluator, Convener, Researcher-Advisor, Institutional Leads) | v9 §3 | UKRN-S abilities schema starting set (Sorry 2.1, narrative) |

### Inventory: existing relationships

- **Researcher-Advisor (Joe).** Owns this mission; implements; cross-cutting role from parent mission.
- **Codex (pair partner).** Produced the reachability correction; co-developed `argument_v9.sexp`; in progress on additional reachability data and possibly faceted reach data.
- **UKRN-S team (Elle, Neil, Nic).** Receive simulation outputs as design support; do not directly contribute to simulation construction; their May 12 vision day is a presentation milestone, not a deliverable for this mission.
- **KCL ethics.** Covers T1 instrument; not affected by simulation work.
- **Bristol team (Jacobs et al.).** Authors of WP02/WP09; not directly involved in simulation, but their indicators-track outputs are simulation inputs.

### Ready vs missing — cross-referenced to IDENTIFY sorries

For each sorry, what is *ready* (exists, can use), what *needs adaptation* (exists in some form but requires reshaping), and what is *missing* (must be authored fresh).

| Sorry | Ready | Needs adaptation | Missing |
|---|---|---|---|
| **1. Faceted reach** | T1 sheet (bulk); ORP training-schema (offerings catalogue); ORP-survey-2025 (possibly with discipline tags) | T1 reporting template (add topic and audience tags); reachability calculation (compute per-facet) | Per-facet model dynamics; faceted denominator logic |
| **2. UKRN-S capabilities** | v9 five-role narrative; ORP training-schema as structural reference; 12-role ORP codebook | None directly | UKRN-S abilities schema (preparatory work); capability-state representation; transition rules |
| **3. Institutional capabilities** | NPT factors as proxies; 5 institutional-condition signals from v9; role codebook | NPT-factor scoring extended to capability-state vector | Transition rules for capability online/offline; observable signs |
| **4. Time anchoring** | Bootstrap timeline (6 months); T1 wave cadence (quarterly); indicator monitoring (annual); v9 §3 Q1/Q2 milestones | None | Tick-to-real-time calibration with defended choice; documented in simulation header |
| **5. Multi-seed validation** | `reset-rngs!` mechanism; per-seed CSV export | `ukrn_mission_verify_export.clj` (extend for seed loop); aggregation logic | Multi-seed harness; cross-seed statistical reporting; methodology pattern |
| **6. Empirical A** | Maria's form (pilotable); WP02/WP09 partial data; focus-group transcripts (could re-code for A signals) | None directly until indicators-track matures | Per-institution A from indicators data; fallback specification for partial coverage |
| **7. Tautology break** | Synth landscape fixture; A7 attack documented; sensitivity-test scaffold (manual at session time) | Sensitivity test as automated harness | Outputs systematically tested; refactored dynamics if needed |
| **8. Per-profile agency** | AIF agent at population level; pattern-target structure per profile | AIF agent for hierarchical / per-profile policy | Per-profile or per-band agent dynamics; coupling between population prior and individual policies |
| **9. Missing institutions** | 16 of 24 with reachability; Codex's extension in progress | Reachability landscape (extend as new data lands) | Defined extrapolation policy for institutions outside calibrated set |

### Triangulating the closure obligations

Cross-tabulating the three tracks against the nine sorries makes visible which sorries are theoretical-track-heavy (need new theoretical primitives), empirical-track-heavy (need new data), or methodological-track-heavy (need new harnesses), and which sit at intersections:

| Sorry | Theoretical | Empirical | Methodological |
|---|:-:|:-:|:-:|
| 1. Faceted reach | • | ●● | • |
| 2. UKRN-S capabilities | ●● | ● (training-schema as ref) |   |
| 3. Institutional capabilities | ●● | • |   |
| 4. Time anchoring |   | • | ●● |
| 5. Multi-seed validation |   |   | ●● |
| 6. Empirical A |   | ●● |   |
| 7. Tautology break | • |   | ●● |
| 8. Per-profile agency | ●● |   | • |
| 9. Missing institutions |   | ●● |   |

(●● = primary contribution; • = secondary contribution.)

Implication: Sorries 5 and 7 (methodological) can be closed with mostly-existing infrastructure and discipline. Sorries 1, 6, 9 (empirical-heavy) depend on data we partially have or are awaiting. Sorries 2, 3, 8 (theoretical-heavy) require fresh authorship; these are the most consequential pieces of new modeling work.

### Cross-cutting facet: willingness to pay / strategic-priority alignment

The IDENTIFY phase named the ORP-to-UKRN-S service-model discontinuity (ORP free at the point of use; UKRN-S fee-for-service). MAP needs to surface this as a *substrate* concern, not just a service-design one — because what makes an institution willing to pay for UKRN-S' offer is not the offer's quality in the abstract but its alignment with what that institution is *already trying to do*. Equivalently: UKRN-S needs a Delta — a credible directional claim that engaging with UKRN-S moves the institution further along its own strategic priorities.

University strategic priorities are typically a mix of locally-specific commitments and sector-driven external pressures. Both shape willingness to pay, and only the locally-specific commitments are fully visible in public documents at any given moment.

- **Locally-specific (visible).** Each university publishes some version of its strategic roadmap. As a worked example, Oxford Brookes' "Roadmap 2030 — managers pack" (May 2026) names three workstream pillars — Educational Excellence, Business Engagement and Research, Financial Sustainability — and lists "income generation" and "research balance" among the explicit challenges. The Brookes-shaped Delta UKRN-S could plausibly offer is "open-research-as-research-strengths-amplifier"; the financial-sustainability angle pushes toward commercialisation routes. Each university would have its own such roadmap with its own Delta-shape; the simulation should be agnostic about *which* shape but sensitive to *whether* alignment exists.
- **Sector-driven (partially visible).** REF assessment cycles, UKRI funding-policy shifts, and sector-level reform conversations (UUK 2024 blueprint; Enhancing Research Culture 2022) create pressures that flow into many institutions' priorities simultaneously. The complication: REF and UKRI documents informing the next assessment cycle are not yet publicly available at session time. So sector-driven priorities are *anticipatable but not directly observable* until those documents land. A facetised Delta proposition — "UKRN-S supports preparation for the next REF" — is a credible commercial framing but cannot yet be fully grounded.

This concern is cross-cutting: it touches several sorries and forces refinements in each:

| Sorry | Refinement under willingness-to-pay |
|---|---|
| 1. Faceted reach | Faceting may need a third axis: strategic-priority alignment (besides topic and audience). The same training topic offered to the same audience may have different uptake depending on whether it aligns with the institution's stated priorities. |
| 2. UKRN-S abilities schema | Each UKRN-S capability needs a *value-proposition profile* — which institutional priorities it serves. Not just "what UKRN-S can do" but "what UKRN-S offers as a Delta against which institutional priority". |
| 3. Institutional capabilities | Tracking institutional capabilities is incomplete without tracking which the institution is trying to develop given its roadmap. A capability the institution is *not building* should not be modelled as available simply because the substrate exists. |
| 9. Missing institutions | Non-engaging institutions may be missing not for capacity reasons but for *priority-alignment* reasons. The simulation's extrapolation policy should distinguish "out of capacity" from "out of priority alignment", because the design implications differ. |

### Competitor landscape (substrate for the willingness-to-pay model)

UKRN-S does not operate in an empty market. The willingness-to-pay vector is shaped not only by what UKRN-S offers and what the institution prioritises, but by whether *anyone else* (including the institution's own internal provision) can serve the same priority more cheaply or with better fit. The competitive landscape divides roughly into two clusters:

- **Research development and support providers** (training-adjacent; some overlap with UKRN-S' core offer): Vitae; ARMA; institutions' own in-house provision; Open Research Methods training delivered by the Open University via the Research England Development Fund; the Digital Research Academy; NCCPE; Epigeum; publisher training (SpringerNature). UKRN-S competes here on training quality and breadth, but in-house provision is often the most acute competitor — institutions typically default to internal trainers if internal trainers are available and credible.
- **Consultancy providers** (advisory-adjacent; overlap with UKRN-S' Convener and Researcher-Advisor roles): Research Consulting; Shift Insight; Information Power; MoreBrains; Evidence to Action; Hadfield Consultants. UKRN-S competes here on sector knowledge and theoretical grounding, but these competitors typically have stronger commercial track records and existing client relationships.

UKRN-S' position is hybrid — neither pure training nor pure consultancy — and the simulation should encode that hybrid character explicitly. The simulation's representation of competitors should be at least categorical (per priority category, what alternatives exist and at what relative cost/quality) without claiming finer-grained pricing data we don't have.

### Willingness-to-pay as path integral along trajectories

The above pieces (priority alignment, value-proposition projection, competitor gaps, budget availability) compose into a single architectural construct. Willingness to pay is best modelled not as a static gate but as a *path integral along the institution's trajectory*: at each tick, three conditions must jointly hold for engagement to accumulate value, and the path continues to integrate only while all three hold:

1. **Priority alignment.** UKRN-S' currently-active capabilities map onto the institution's strategic priorities (the Delta projection is positive).
2. **Competitor gap.** Competitors (including the institution's own in-house provision) cannot serve the same priority — or cannot serve it as well, or as cheaply, or with the same sector legitimacy as UKRN-S.
3. **Budget availability.** The institution has finite resources to allocate to external services in a given period; engagement consumes budget; depletion truncates engagement.

If any of the three fails at tick *t*, the path either truncates (engagement ends) or pauses (engagement stalls until the failing condition resolves). If all three hold, one more unit of engagement value accumulates. The integrated value over the trajectory is what the institution and UKRN-S are *jointly* producing — not what UKRN-S delivers in isolation.

This formulation has several useful properties for the simulation:

- It composes cleanly with the AIF agent. Expected free energy is already a quantity computed over a policy (sequence of tensions). The path integral adds a *gating function* over policy time: if willingness-to-pay fails, the policy truncates. This generalises the existing simulation's "run 80 ticks and read final D, A" to "run until disengagement, and read both how-far-it-got and how-quickly-it-disengaged."
- It gives the simulation a natural per-institution exit criterion that depends on dynamics rather than on a fixed tick count. Some institutions may disengage at tick 12 (budget depletion or competitor offer arrives); others may continue through to the planned horizon and beyond.
- It makes the ORP-era "if we deliver, they will come" model visibly inadequate: under the path-integral construction, mere capability-side delivery is not enough. The trajectory either persists or it doesn't, depending on the joint state of UKRN-S, institution, and market.
- It makes some otherwise-implicit findings legible. An institution that disengages early because of a competitor gap is a different finding from one that disengages because of budget depletion or priority drift. The simulation can report these separately.

### Practical implications for the simulation construction

- The simulation should carry a per-institution *strategic-priority vector* (a small set of priority categories, populated from public roadmaps where available and flagged as "unknown" otherwise) and a per-institution *budget* (finite, depleting under engagement, replenishing per institutional cycle).
- The UKRN-S abilities schema (Sorry 2.1) should carry a value-proposition profile per capability — which priorities each capability serves. The Delta is computed at runtime as the projection of UKRN-S' active capabilities onto the institution's priority vector.
- The simulation should carry a *competitor coverage table* (per priority category × competitor cluster) representing whether alternatives exist for that priority. Updated periodically as the market shifts.
- A willingness-to-engage *path integral* replaces the static engagement gate: at each tick, evaluate the three conditions; accumulate value if all hold; truncate or pause otherwise. The trajectory's reportable outcomes are not just final D and A but also *how far the path got before disengagement* and *which condition failed first* when it did.
- The construction is sensitive to the data-availability discontinuity: locally-specific priorities are publishable substrate; sector-driven priorities depend on documents not yet available. The simulation should make explicit which class of priority it is encoding for each institution and tag uncertainty accordingly.

This MAP addition does not introduce a tenth sorry — it sharpens and connects existing ones. But it deserves naming as a cross-cutting concern because without it, the simulation reproduces the ORP-era model of "if we deliver, they will come" — which is exactly the model UKRN-S' fee-for-service mode invalidates. The path-integral formulation is the architectural construct that operationalises the difference.

### Survey questions for MAP

Before DERIVE can begin, the following need owner-level decisions:

1. **Schema construction for UKRN-S abilities (Sorry 2.1).** Owner has flagged this as not-IDENTIFY-work. Is it MAP work or DERIVE work? *Recommendation: a schema sketch in MAP (one paragraph plus a draft list of capability identifiers); full schema authorship in DERIVE.*

2. **New simulation as fresh notebook or extension.** Should the new simulation be a new notebook (`ukrn_simulation_v2.clj`?) or extend `ukrn_cyberant_aif.clj` and `ukrn_mission_verify.clj`? *Recommendation: new notebook(s) — the new architecture differs enough that branching cleanly is more legible than retrofitting.*

3. **Time-anchoring choice.** What real-time interval does each tick represent? *Candidates: 1 month (matches T1 reporting cadence); 1 quarter (matches the existing reporting sheet's columns); 1 week (lets bootstrap-phase events resolve in detail).* *Recommendation: 1 quarter is the most defensible match to existing data cadence.*

4. **Number of seeds for multi-seed validation.** ≥20 was the IDENTIFY-phase placeholder. Does owner have a preference? *Recommendation: 50 seeds for headline findings; 20 for parameter-sensitivity sweeps.*

5. **Empirical anchor: Codex-extension wait or proceed.** Should the new simulation wait for Codex's expanded reachability set or begin with the current 16-institution set and re-run when more data lands? *Recommendation: begin now with 16 institutions; design the extension to be re-runnable when new data lands, treating expanded-set runs as scheduled iterations.*

6. **Bayesian-scenario re-integration.** Should `ukrn_learning_system.clj`'s machinery be re-integrated as a posterior-aggregation layer over multi-seed AIF runs (per IDENTIFY notes)? *Recommendation: yes, but as a separable optional layer — the multi-seed AIF runs should be the primary report; Bayesian aggregation is a sophistication added when the basic harness works.*

7. **Per-profile vs per-band agency.** Should Sorry 8 close with per-profile agents (16 agents, one per institution) or per-band agents (3 agents, one per band)? *Recommendation: per-band first — tractable, matches the band-level recommendation structure of the WP, and exposes whether band-level differential intervention is sufficient. Per-profile is a possible future extension.*

### Surprises

Three observations that emerged from compiling the inventory:

1. **`ukrn_learning_system.clj` is the largest simulation notebook (1287 lines)** and contains substantial uncertainty-quantification machinery (Beta priors, Bayesian network, scenario analysis) that v9 abandoned in the move to AIF. Re-integrating it could be cheaper than authoring fresh uncertainty quantification — a previously-discarded asset that the new simulation may want back.

2. **The 12-role ORP codebook is unused by the simulation.** It informed the v9 §3 narrative (the role-overload analysis, the coordination-complexity gradient) but the simulation has never operationalised the role structure. Yet the role codebook is the closest available structural primitive to a UKRN-S abilities schema (Sorry 2.1). The mission may discover that the role codebook is the *right shape* for the UKRN-S abilities schema, with appropriate adaptation.

3. **The ORP training-schema is also unused by the simulation.** It is a public, structured, externally-validated catalogue of offerings — exactly the structural-richness reference the IDENTIFY phase identified for the UKRN-S abilities schema. The asymmetry the IDENTIFY phase named (ORP has a schema, UKRN-S doesn't) is operationalisable: the *shape* of the ORP schema is the shape the UKRN-S equivalent should match. This may simplify the schema-construction work in DERIVE substantially.

### Notes on this MAP draft

- Drafted 2026-05-07 as continuation of session work.
- The two surprises about under-used assets (Bayesian scenario notebook; ORP role codebook; ORP training-schema) are the most actionable findings of MAP — each suggests a chunk of construction work that may turn out to be re-integration rather than fresh authorship.
- The triangulation table makes visible that Sorries 5 and 7 are the cheapest to close (methodological-only) and Sorries 2, 3, 8 are the most expensive (require fresh theoretical work). This suggests a tactical sequencing for DERIVE: close 5 and 7 first, then 1 and 4 (empirical/methodological), then 2 and 3 (theoretical-heavy), then 8 (advanced). Sorries 6 and 9 are awaiting data.
- Owner-level questions are surfaced explicitly. DERIVE should not begin until those are answered.

---

## 3. DERIVE

### Frame: from MAP inventory + cross-cutting concerns to a single simulation architecture

The MAP phase yielded a substrate inventory, three triangulating tracks, a cross-cutting willingness-to-pay concern (with competitor landscape and path-integral formulation), and seven owner-level questions. DERIVE composes these into a single coherent architecture: what the new simulation actually *is*, decided at component-level granularity, with reasons.

Architectural decisions are presented as `IF / HOWEVER / THEN / BECAUSE` quadruples where the decision is non-obvious. Trivial decisions (use Clojure; extend existing notebook stack) are stated without ceremony.

### Architectural decisions (resolving MAP's seven survey questions)

#### D-A1: UKRN-S abilities schema is a DERIVE deliverable, sketched here

**IF** the UKRN-S abilities schema does not yet exist (Sorry 2.1) and the simulation cannot model capabilities-coming-online without it,
**HOWEVER** schema authorship is a research output in its own right and should not be rushed,
**THEN** sketch the schema in DERIVE with a starting set of capabilities, their entry conditions, depth gradients, observable signs, dependencies, and value-proposition profiles. Full authorship is incremental — additional capabilities, refined gradients, and validated entry conditions will accrete during VERIFY and beyond.
**BECAUSE** a sketch with concrete shape is enough to drive simulation construction; refinement happens against simulation output.

The schema sketch follows below in *UKRN-S abilities schema (sketch)*.

#### D-A2: Fresh notebooks, not retrofitting

**IF** the new simulation differs structurally from v9's (capability-state dynamics; path-integral engagement; multi-seed harness; faceted population),
**HOWEVER** retrofitting `ukrn_cyberant_aif.clj` and `ukrn_mission_verify.clj` would preserve their architectural commitments,
**THEN** author fresh notebooks under the prefix `ukrn_v2_*.clj` with a clean module decomposition. Existing notebooks remain available as references and as test fixtures.
**BECAUSE** the architectural delta is too large for retrofitting to be cheaper than fresh authorship; a clean cut also makes the v9-vs-v2 comparison legible.

Module decomposition (five notebooks):

- `ukrn_v2_substrate.clj` — institutional population (16 → 24+ extensible); per-institution NPT factors; per-institution capability state vector; per-institution strategic-priority vector; per-institution budget; per-institution facet vector. Plus UKRN-S capability state vector and per-capability value-proposition profile. Plus competitor coverage table.
- `ukrn_v2_dynamics.clj` — capability transitions for both UKRN-S and institutions; pattern firing under capability state; lever-to-NPT-factor mapping (extended for facets); hierarchical AIF agent (population prior + per-band policy refinement).
- `ukrn_v2_engagement.clj` — path-integral willingness-to-pay; per-tick evaluation of priority alignment, competitor gap, and budget availability; trajectory truncation/pause/continuation logic; integrated-value computation per institution per facet.
- `ukrn_v2_runner.clj` — multi-seed orchestration; time-anchored simulation loop (tick = 1 quarter); per-seed CSV export; configuration parameter sweep harness.
- `ukrn_v2_analysis.clj` — cross-seed aggregation; sensitivity / tautology audit; optional Bayesian posterior layer over multi-seed outputs; reporting and figure generation.

#### D-A3: Time anchoring — tick = 1 quarter; horizon = 5 years (20 ticks)

**IF** the simulation must support time-anchored claims and the existing data cadence (T1 reporting sheet quarters, indicator monitoring annual, bootstrap-phase 6 months),
**HOWEVER** finer-grained ticks (1 month, 1 week) would let detailed bootstrap events resolve at higher temporal resolution,
**THEN** set tick = 1 quarter; simulation horizon = 20 ticks (5 years), with sub-tick event annotations for finer-grained occurrences (a quarterly T1 reporting wave, an annual indicator monitoring run, a 5-year sector cycle anchor).
**BECAUSE** quarterly matches the existing data substrate (T1 sheet columns) and the working paper's bootstrap milestones (Q1 2026 Evaluator survey; Q2 2026 configuration decision; ~Q3 2026 first T1 wave); any finer cadence introduces calibration uncertainty without empirical grounding.

#### D-A4: Multi-seed regime — 50 for headline; 20 for sensitivity sweeps

**IF** every reportable contrast must survive multi-seed averaging (Sorry 5),
**HOWEVER** seed counts trade off against simulation runtime when parameter sweeps multiply configurations,
**THEN** use 50 seeds for headline findings (e.g., AIF baseline vs drift on the empirical landscape) and 20 seeds for parameter sensitivity sweeps (e.g., per-capability ablation; per-priority weighting variation). Single-seed runs are reserved for development and debugging only and are explicitly labelled "illustrative" if shown.
**BECAUSE** 50 seeds gives a tight enough confidence interval on headline aggregates to defend against the kinds of contrast-collapse the v9 audit revealed; 20 is a defensible compromise for sensitivity work where the effect-size threshold is larger.

#### D-A5: Empirical landscape proceeds with current 16; designed for re-run

**IF** Codex's reachability extension is in progress and may produce 4 additional institutions (or possibly 8 if both denominators-pending and currently-out-of-set institutions are added),
**HOWEVER** waiting for the extension blocks construction work that has well-defined value for the 16 institutions already covered,
**THEN** proceed now with the 16 institutions; design the substrate's institution-set as a parameterised input (count, names anonymised by band-rank tags, reachability values) so that re-running with an extended set is a configuration change rather than re-engineering.
**BECAUSE** parallel progress on data and simulation is the right shape; data extension and simulation extension can both happen incrementally.

#### D-A6: Bayesian aggregation as a separable optional layer

**IF** `ukrn_learning_system.clj`'s Bayesian-scenario machinery offers uncertainty quantification absent from the v9 single-seed runs,
**HOWEVER** the multi-seed harness (D-A4) already produces empirical confidence intervals from cross-seed aggregation, which is the main quantification need,
**THEN** treat the Bayesian layer as separable and optional: the simulation's primary output is multi-seed-averaged numbers with empirical CIs; Bayesian posteriors are a layer that can be added when the basic harness is working and when the question being asked benefits from prior information beyond the seed variation.
**BECAUSE** building both at once over-engineers; multi-seed alone closes Sorry 5; Bayesian aggregation can be added as a second iteration's enhancement.

#### D-A7: Per-band agency first; per-profile reserved for later

**IF** the AIF agent must be differentiated to support different design configurations for different institutional bands (Sorry 8),
**HOWEVER** full per-profile (per-institution) agency multiplies modeling complexity by ~16,
**THEN** implement a hierarchical agent: a population-level prior (set by UKRN-S' overall strategic posture) plus per-band policy refinement (3 bands × possibly 3 facet-aligned variants). Per-profile agency is reserved as a possible future extension, not as a near-term commitment.
**BECAUSE** per-band differentiation matches the granularity of the working paper's recommendations (the v9 paper itself differentiates by reach band, not by institution). Per-profile agency would require per-institution data on specific design preferences that we don't have.

### UKRN-S abilities schema (sketch)

The schema is a structured catalogue of UKRN-S capabilities — what UKRN-S can do — in a form the simulation can carry. Each capability has the following fields:

- **Capability identifier.** A stable kebab-case key.
- **Description.** One-line plain text.
- **Entry conditions.** What brings the capability online. Resource (FTE, time, money), preconditions (other capabilities), or external triggers (data wave; sector event).
- **Depth gradient.** Binary (online/offline), scaled (1-5 by coverage / depth), or multi-dimensional (separate axes for breadth and quality).
- **Observable signs.** How an external observer would know whether the capability is online and at what depth (for ARGUE/VERIFY).
- **Dependencies.** Which other capabilities this requires to function.
- **Value-proposition profile.** Which institutional strategic priorities this capability serves; categorical mapping to a small priority taxonomy.
- **Cost/effort indicator.** Rough — what it takes UKRN-S to bring it online and maintain it.

Starting set, drawn from v9 §3 roles and the 12-role ORP codebook:

| Identifier | Description | Entry conditions | Depth | Dependencies | Priority profile |
|---|---|---|---|---|---|
| `training-delivery` | Sustained training-product delivery at network scale | TM role at ≥1.0 FTE; existing T3 cohort | Scaled 1–5 by quarterly throughput | None | Foundational (all priorities) |
| `cross-institutional-analysis` | Aggregating evidence across institutions for comparative findings | Evaluator role at ≥0.5 FTE; data-handoff channel from TM | Scaled 1–5 by cross-institution coverage | `training-delivery` | REF readiness; indicators-track |
| `sector-translation` | Converting analytical findings into sector-facing commitments | Convener role at any FTE; OSMI/UUK access | Binary (online/offline) | `cross-institutional-analysis` | Sector positioning; reputation |
| `pattern-induction` | Inducing patterns from qualitative + quantitative evidence | Researcher-Advisor at consultancy bandwidth | Scaled 1–5 by pattern-library maturity | `cross-institutional-analysis` | Theoretical legitimacy |
| `verification-harness` | Multi-seed simulation + tautology audit infrastructure | Researcher-Advisor + simulation maintained | Binary (operational/not) | `pattern-induction` | Internal evidence quality |
| `in-flight-evaluation-loops` | Evidence flowing back into design while work is happening | `cross-institutional-analysis` running ≥3 quarters | Scaled by loop-closure rate | `cross-institutional-analysis` | Quality assurance; feedback culture |
| `osmi-uuk-relationships` | Sustained sector-level partnerships | `sector-translation` online ≥4 quarters | Scaled 1–5 by partnership depth | `sector-translation` | Sector positioning |
| `indicators-track-operationalisation` | WP09 pilots scaled to per-institution monitoring | Indicators-track substrate matured (years 2-3); Convener+Evaluator co-ownership | Scaled 1–5 by indicator coverage | `cross-institutional-analysis`; `osmi-uuk-relationships` | REF readiness; commercial proposition |
| `topic-faceted-specialism` | Recognised expertise in a specific topic-audience facet (e.g., AHSS preregistration) | TM + Researcher-Advisor; evidence of specific facet uptake | Binary per facet (online/offline); multi-instance | `pattern-induction` | Differentiated commercial proposition |
| `ref-cycle-alignment` | Translating offerings to current REF assessment expectations | UKRI/REF documents available; Convener + Researcher-Advisor co-authorship | Binary (online/offline); tied to assessment cycle | `sector-translation`; `cross-institutional-analysis` | REF readiness |

Ten capabilities is the starting set; the schema is open to extension. Note that capabilities form a roughly hierarchical structure: `training-delivery` is foundational; analytical and translational capabilities depend on it; specialised capabilities depend on analytical ones.

### Institutional abilities schema (sketch)

Symmetric to the UKRN-S schema, drawn from the v9 five institutional-condition signals plus the role-codebook gradient:

| Identifier | Description | Entry conditions | Depth | Observable signs |
|---|---|---|---|---|
| `role-ownership-or` | Named institutional role with explicit responsibility for open research | Institutional decision; allocated FTE | Scaled 1–3 (no role / role exists / role with budget) | Job description; committee membership; institutional contact list |
| `in-flight-eval-loops-inst` | Local evaluation loops feeding back into local practice | Institutional process; data culture | Binary (online/offline) | Evaluation reports; service-change tracebacks |
| `pro-services-coordination` | Coordination across professional services for open research | Cross-team agreement; senior sponsorship | Scaled 1–3 by integration depth | Joint-team outputs; coordinated calendar |
| `leadership-support` | Senior leadership visibly endorsing open-research priorities | Strategy doc inclusion; named senior sponsor | Binary (visible/not visible) | Strategy text; public statements |
| `reward-recognition` | Open research counted in promotion / appraisal / reward | Policy update; evidence of operation | Scaled 1–3 by integration depth | Promotion criteria; PDR templates; reward case studies |

These five plus the strategic-priority vector (per institution; categorical) constitute the institutional substrate the simulation operates on. The vector and the five capabilities co-evolve under simulation dynamics.

### Path-integral willingness-to-pay (architectural sketch)

For each institution × tick, the path-integral evaluator computes:

1. *Priority-alignment score.* Projection of UKRN-S' currently-active capability set onto the institution's strategic-priority vector, mediated by the value-proposition profile per capability. Result: scalar in [0, 1].
2. *Competitor-gap score.* Lookup in the competitor coverage table: for each priority the institution cares about, is UKRN-S' offering distinguishably better/cheaper/more legitimate than the cluster of competitors who could plausibly serve it? Result: scalar in [0, 1].
3. *Budget-available flag.* Per-institution budget minus engagement cost from prior ticks, compared against per-tick engagement cost. Result: boolean.

Per tick:
- If all three pass thresholds (parameterised; default thresholds chosen for sensible defaults and varied in sensitivity sweeps), engagement value accumulates by 1 unit, budget is debited, capability development progresses.
- If priority alignment fails (UKRN-S no longer serving this institution's priorities), engagement *pauses* — no value accrues, no budget debit, but capability state persists.
- If competitor gap fails (a competitor closes the gap), engagement *truncates* — UKRN-S departs and the institution's trajectory continues without it.
- If budget fails, engagement *truncates* — paywall shutdown.

Reportable outputs per institution per facet:
- Final delivery viability (D)
- Final architectural sustainability (A)
- Total integrated engagement value (the path integral)
- Truncation tick (when engagement ended, or HORIZON if it persisted)
- Truncation cause (priority drift / competitor close / budget exhaustion / no truncation)

This is the substrate that makes the simulation's findings discriminating: an institution that lifted into Multiplied at tick 8 and disengaged at tick 12 because of competitor pressure is a different finding from one that lifted into Multiplied at tick 16 and persisted.

### Closure plans per sorry

Each sorry's closure plan names which architectural component closes it, what additional data or work is needed, and the validation criterion that says the closure has held.

| Sorry | Closes via | Additional work | Validation criterion |
|---|---|---|---|
| 1. Faceted reach | `ukrn_v2_substrate.clj` per-institution facet vector + ORP training-schema as facet ontology + extended T1 reporting tags | T1 reporting template extension (operational, not modeling); facet-level reachability calculation | Multi-seed runs report at facet level; bulk aggregation only when defended |
| 2. UKRN-S capabilities | UKRN-S abilities schema (sketched above) + capability-state representation in `ukrn_v2_substrate.clj` + transition rules in `ukrn_v2_dynamics.clj` | Schema refinement during VERIFY; entry conditions calibrated against observable UKRN-S milestones | Simulation run shows differentiated trajectories for institutions where specific UKRN-S capabilities come online vs not |
| 3. Institutional capabilities | Institutional abilities schema (sketched above) + capability-state representation + transition rules | Schema refinement; calibration against focus-group transcript coding | Simulation shows institutional capability dynamics distinct from NPT-factor dynamics; each capability has explicit entry/exit |
| 4. Time anchoring | D-A3 decision (tick = 1 quarter, 20-tick horizon) | Documentation; cross-reference to data cadence | Simulation header documents tick semantics; outputs labeled with calendar quarters |
| 5. Multi-seed validation | D-A4 decision (50 / 20 seeds) + `ukrn_v2_runner.clj` parameterised seed loop + cross-seed aggregator | Pattern authorship: `single-seed-results-need-multi-seed-validation` | All headline findings reported with cross-seed mean and 95% interval; no single-seed claims |
| 6. Empirical A | Acknowledged dependency on indicators-track data; placeholder uniform-A with explicit caveat; substrate designed to accept empirical-A inputs | Indicators-track data once available; possibly re-coding focus-group transcripts for A signals | When empirical-A lands, swap-in is configuration change; sensitivity audit shows which findings depend on A specification |
| 7. Tautology break | Sensitivity / tautology audit module in `ukrn_v2_analysis.clj` + path-integral construction (engagement gating breaks naive "starting position determines outcome") | Audit harness authoring; per-finding sensitivity reporting | Each headline finding accompanied by sensitivity statement: "robust to ±0.1 starting D"; "depends on starting position" |
| 8. Per-profile agency | D-A7 decision (per-band first); hierarchical AIF agent in `ukrn_v2_dynamics.clj` | Per-band agent specification; reserved per-profile extension | Simulation produces differentiated trajectories per band; per-profile reservation documented |
| 9. Missing institutions | D-A5 decision (proceed with 16; designed for re-run) + parameterised institution-set substrate + extrapolation policy in `ukrn_v2_analysis.clj` | Codex's reachability extension; clear extrapolation refusal text where extrapolation is unwarranted | When extended set lands, configuration change re-runs; outputs distinguish "covered" vs "extrapolated" institutions |

### Pattern Selection Records (PSRs)

Patterns from the libraries that this DERIVE phase explicitly applies:

- `system-coherence/correct-effect-claims-with-relevant-denominators` (W6 in argument_v9.sexp). **Applied to:** all reach claims, all effect claims, all multi-seed reportable outputs. **Disposition:** load-bearing pattern; the simulation's reachability substrate exists because of it.
- `system-coherence/facet-before-aggregating` (W8; new this session). **Applied to:** D-A2 module decomposition (facet vector in substrate); reportable outputs per facet; D5 in argument_v9.sexp. **Disposition:** load-bearing for the simulation's unit of analysis.
- `system-coherence/place-capabilities-at-the-scale-of-their-absence` (W3). **Applied to:** UKRN-S abilities schema construction (Sorry 2); reasoning that cross-institutional analysis sits at network scale, not institutional scale. **Disposition:** structural warrant for which capabilities live where in the schema.
- `system-coherence/turn-design-into-checks` (W4). **Applied to:** observable-signs field per capability; sensitivity audit module; multi-seed reporting requirements. **Disposition:** discipline for the entire simulation's output.
- `system-coherence/separate-configurable-choices-from-load-bearing-couplings` (W5). **Applied to:** distinction between configuration parameters (seed count; threshold settings) and architectural commitments (path-integral construction; capability schema). **Disposition:** documentation discipline.
- `system-coherence/argue-empirically-not-persuasively`. **Applied to:** every reportable claim must derive from simulation behaviour, not from speculative narrative. **Disposition:** governance for VERIFY phase output.
- `system-coherence/bind-open-questions-to-closure-mechanisms` (W2). **Applied to:** each sorry has a closure plan above; each architectural decision has a defended reason; each open uncertainty has a mechanism that would resolve it. **Disposition:** discipline for this DERIVE phase itself.
- New pattern (deferred from session, to be authored alongside this work): `system-coherence/single-seed-results-need-multi-seed-validation`. **Applied to:** D-A4 decision; the `ukrn_v2_runner.clj` design; PSRs become PURs when the pattern is authored and successfully applied.

### Notes on this DERIVE draft

- Drafted 2026-05-07 directly continuing the session's MAP work.
- Seven survey questions answered as IF/HOWEVER/THEN/BECAUSE quadruples (D-A1 through D-A7).
- The UKRN-S abilities schema sketch (10 capabilities) and institutional abilities schema sketch (5 capabilities) are *sketches* — open to refinement during ARGUE and VERIFY. The structural shape is what's being committed to here, not the specific capability list.
- The path-integral willingness-to-pay construction is the most substantive architectural addition to the v9 simulation paradigm. It composes cleanly with AIF (already a path-integral over policies) and gives the simulation the per-institution exit criterion it lacks today.
- Module decomposition into five `ukrn_v2_*` notebooks gives clean concerns; total lines of new code likely 2000–3000 (compared to the 7000 lines in the existing notebook stack).
- This DERIVE makes substantial commitments without yet writing code. The next phase (ARGUE) translates these architectural commitments into testable predictions — what we expect to see when the simulation runs and what would falsify the design's predictions. Then VERIFY runs the simulation.
- The pattern `single-seed-results-need-multi-seed-validation` is now *due*. Authoring it alongside the early `ukrn_v2_runner.clj` work in the next phase makes sense.

---

## 4. ARGUE

### Frame: from architecture to verifiable predictions, with AIF as the theoretical spine

DERIVE made commitments at component level — capability-state dynamics, faceted reach, path-integral willingness-to-pay, hierarchical AIF agent, multi-seed harness, time anchoring, tautology audit. Adding all of this to v9's already non-trivial AIF cyber-ant looks, on its face, like complexification. The risk is that the v2 simulation becomes an arbitrary kludge — a stack of features added because each closed a sorry, without coherence between them.

The ARGUE phase's job is to defend the v2 architecture against that risk by establishing its theoretical grounding. The grounding is *full active inference*: each architectural commitment in DERIVE maps onto, extends, or specialises an established AIF construct. The complexity is principled, not arbitrary — what we have built is what a faithful AIF simulation of the working paper's design *demands*, given the substrate the paper specifies.

This frame also sets up VERIFY: the AIF formalism is operationalised (and uplift-able into futon5's AIF+ machinery) once the architecture is shown to be AIF-consistent. The VERIFY phase can then run the simulation as an active-inference agent rather than as a custom dynamical system whose theoretical status is uncertain.

### Pattern cross-reference: each architectural commitment to its AIF pattern

The AIF library at `/home/joe/code/futon3/library/aif/` provides seven patterns that constitute a *generic AIF agent specification*. Each maps onto specific v2 architectural commitments.

| AIF library pattern | v2 commitment that applies it | How |
|---|---|---|
| `aif/expected-free-energy-scorecard` | EFE decomposition under path-integral engagement | G is computed per term, with separate accounting for priority-alignment risk, ambiguity, competitor-gap, budget pressure, and pattern-EFE — all persisted as named terms, never collapsed to an opaque scalar. The v9 simulation already does this for the basic risk + ambiguity + pattern-EFE decomposition; v2 extends it with engagement-gating terms. |
| `aif/policy-precision-commitment-temperature` | Path-integral willingness-to-pay as commitment-modulating | The path-integral gating is *itself* a precision modulator: when alignment, competitor-gap, and budget all hold, the agent's commitment to its current policy strengthens; when any fail, commitment weakens (pause) or breaks (truncate). The commitment temperature τ becomes state-dependent on the gating outcome rather than time-decay-only. |
| `aif/evidence-precision-registry` | Competitor coverage as precision-weighted observation channel | The competitor-coverage table is, in AIF terms, an evidence precision registry: priorities where competitors can serve get lower precision (UKRN-S' offering on that priority is less reliable as a differential signal); priorities where UKRN-S has unique offer get higher precision. This becomes the formal mechanism for the willingness-to-pay competitor-gap term. |
| `aif/belief-state-operational-hypotheses` | Capability-state vector as belief over operational hypotheses | Each capability's state (online / developing / absent) is an *operational hypothesis* the agent maintains a belief about. The hypothesis updates under observation — observable signs (per the schema's "observable signs" field) are the evidence channels. Capability-coming-online is a belief-update event. |
| `aif/structured-observation-vector` | Faceted reach as structured observation, not scalar | Reach is no longer a scalar input to the simulation; it is a structured observation vector — typed by topic, audience-community, and temporal phase. Each facet is its own channel. The structured-observation pattern is the formal warrant for D5 in argument_v9.sexp. |
| `aif/candidate-pattern-action-space` | UKRN-S abilities schema as bounded action space | Each capability identifier in the UKRN-S abilities schema is a *pattern ID* in AIF terms. The agent's per-tick action selection is "which UKRN-S capability(ies) to bring online or maintain" — a bounded set of pattern IDs determined by the schema and the current state's preconditions. |
| `aif/term-to-channel-traceability` | Multi-seed reporting requires per-term channel disclosure | Every reportable claim in VERIFY must declare which simulation channels (seed range, configuration parameter, time window, facet subset) produced the score it relies on. The term-to-channel traceability pattern is the formal discipline for the multi-seed harness's reporting layer. |

All seven patterns apply. *None are stretched to apply* — each maps cleanly. This is the theoretical-coherence finding: v2's complexity is the AIF complexity made explicit for this domain.

### Theoretical coherence: AIF+ uplift to futon5

The mapping above is sufficient for VERIFY-level operationalisation, but it admits a stronger claim. The futon5 `cyber-ant uplift` formalism (`/home/joe/code/futon5/reference/cyber-ant-uplift.flexiarg`) shows that flexiarg patterns can be *uplifted* into operator specs, then flashed into simulation as `:aif-config` overrides for the `:cyber` species. The pipeline is:

1. Flexiarg patterns describe the behaviours.
2. EDN bindings in `resources/futon5/cyber_ants.edn` link narrative to concrete `:aif` overrides.
3. `cyber_ants.clj` hydrates entries, exposing configs while delegating motif promotion to the generic uplift operator.
4. Simulation depends on this namespace and attaches the generated config to every cyber-agent.
5. The AIF core merges the override into the per-agent active-inference loop.

For the v2 simulation, this enables a stronger version of VERIFY: the architectural commitments documented in DERIVE can be authored as flexiarg patterns (some already exist in the AIF library; some need to be authored for v2-specific commitments — see *PSRs and Pattern Authorship* below), then uplifted into the simulation runner as configurable AIF overrides per institutional band, per facet, or per scenario. This means VERIFY's parameter-sweep work is not a custom orchestration script but a structured uplift-and-run sequence with futon5-managed telemetry.

The implication: VERIFY can borrow futon5's `--aif-weight`, `--aif-guide`, `--aif-mutate` infrastructure (per `futon5/README.md`) rather than re-engineering equivalent parameter sweep machinery. This is non-trivial leverage; it makes VERIFY 100s of lines lighter and benefits from existing futon5 telemetry.

### Predictions the simulation should produce

The v2 simulation's value is in producing predictions that v9's simulation could not, *and that VERIFY can falsify*. The headline predictions:

**P-v2-1 — Path-integral truncations are diagnostic.** Different institutions will disengage at different ticks for different reasons (priority drift, competitor close, budget exhaustion). The truncation distribution is itself a finding: e.g., "most mid-band institutions truncate at tick 8-12 due to competitor closure, not priority drift." If all institutions disengage at the same tick or for the same reason, the path-integral construction adds no information beyond v9's static gate.

**P-v2-2 — Capability-onboarding-order matters.** The order in which UKRN-S' capabilities come online (training-delivery first vs cross-institutional-analysis first vs sector-translation first) shapes which institutions persist in engagement. If institutions are insensitive to capability ordering, this falsifies the "learning system" claim.

**P-v2-3 — Facet-level heterogeneity within reach bands.** Two institutions in the same reach band, but with different topic-audience facet profiles, show measurably different trajectories. If facet structure does not differentiate within bands, then aggregated reachability bands are sufficient and the faceted approach is over-engineered for this question.

**P-v2-4 — v9's collapsed contrasts stay collapsed under multi-seed.** P1 (timing), P2 (architectural prior), P3 (coupling) — which collapsed to identical outcomes under controlled-RNG single-seed runs — should remain collapsed across 50 seeds. If they don't, the v9 audit was wrong and the contrasts were real.

**P-v2-5 — v9's surviving contrasts persist with tighter CIs under multi-seed.** P4 (funder catalysis) and P5 (Evaluator configuration) should retain differentiated outcomes with multi-seed cross-seed CIs that don't span the contrast. If they don't survive, the simulation is too noisy at the parameter resolution implied by v9.

**P-v2-6 — Tautology break: starting position is not the dominant outcome predictor when willingness-to-pay gates engagement.** A thin-reach institution with strong priority alignment, competitor gap, and adequate budget should show distinguishable lift trajectories from a thin-reach institution lacking those. If starting D fully predicts outcome regardless of the path-integral gates, the tautology persists and Sorry 7 is not closed by the path-integral construction alone.

**P-v2-7 — Empirical landscape headline (R6) survives or refines.** Multi-seed runs against the 16-institution empirical landscape produce either the v9 headline (10/16 with design vs 2/16 drift; thin band cannot be lifted) with cross-seed CI, or a refined version (e.g., "thin band is not uniformly unliftable; subset with priority alignment lifts"). Both are publishable findings; the question is which obtains.

Each prediction has a falsifier. P-v2-1 falsifies if truncation distribution is uniform; P-v2-2 if onboarding-order doesn't differentiate; P-v2-3 if within-band facet heterogeneity is below noise; etc. The simulation's value is that *each prediction's falsifier is computable from the multi-seed output*, not narrative.

### Trade-off summary

The v2 architecture trades simplicity for fidelity in three ways. The trade-offs deserve naming so the design's character stays legible.

- **Static dynamics → capability-state dynamics.** v9's static patterns are simpler but cannot represent learning systems; v2's capability transitions are more complex but support the working paper's central claim. Trade-off accepted in DERIVE; ARGUE confirms it is theoretically grounded (operational-hypotheses pattern).
- **Scalar reach → faceted reach.** Bulk reach was the v9 unit; v2 facets by topic and audience-community. The simulation now requires more input data (or default-uniform-with-caveats) but produces actionable per-facet recommendations. Trade-off accepted; structured-observation-vector pattern grounds it.
- **Static engagement → path-integral engagement.** v9 ran 80 ticks; v2's institutions disengage when willingness-to-pay fails. The simulation now requires three additional substrate elements (priority vector, competitor table, budget) but produces per-institution exit dynamics that v9 silently assumed away. Trade-off accepted; policy-precision-commitment-temperature and evidence-precision-registry patterns ground it.

In all three cases, the trade-off is in the direction the working paper's design recommendations point: more fidelity to what UKRN-S' design actually proposes (a learning system; faceted offer; sustainable engagement). The simulation is becoming the right shape for the question.

### Generalisation notes

Two pieces of the v2 architecture are likely to generalise beyond this mission:

- **Path-integral willingness-to-pay** is a pattern broadly applicable to any service-design simulation where engagement is voluntary and resource-constrained. The flexiarg authorship for it is in scope of this mission's PSR work and produces a reusable artefact.
- **Multi-seed validation discipline** generalises trivially. The deferred pattern `system-coherence/single-seed-results-need-multi-seed-validation` is what captures it; both this mission and the parent mission (and any successor) benefit from it being on the books.

By contrast, the *specific* UKRN-S abilities schema and the *specific* facet ontology are domain-specific to the open-research-training context. Their generalisation is by *shape* (other domains will have analogous schemas), not by direct reuse.

### Plain-language argument

For a reader who has not been inside the architectural detail, the v2 simulation's ARGUE-phase claim is this:

> The v9 simulation was useful as an instrument but had three commitments that the working paper's own findings forced us to revise. First, the v9 simulation treated UKRN-S as a static service: capabilities were either there or not, never coming online over time. Second, it treated reach as a single number per institution, hiding exactly the topic-and-audience heterogeneity the working paper now recommends as the unit of analysis. Third, it treated engagement as automatic — once the simulation started, institutions stayed engaged for 80 ticks regardless of whether they were getting value, whether competitors were doing the same job, or whether they had budget.
>
> The v2 simulation fixes all three. It tracks UKRN-S' capabilities as entities that come online over time. It operates on facets — training topic by audience community — as its native unit. And it gates institutional engagement by a path-integral that asks, at every step, whether UKRN-S is serving the institution's priorities, whether competitors aren't already doing it, and whether budget is available. If those three hold, engagement persists; if any fails, the institution disengages.
>
> The v2 architecture is more complex than v9's. The defence against arbitrary complexity is that every architectural commitment maps onto an established active-inference pattern from the AIF library. The complexity is what the formalism demanded, given the working paper's specification. The simulation is, in that sense, more *honest* than v9 — it carries openly what v9 had to assume away.

### Translation to AIF for VERIFY

For VERIFY, the v2 simulation is operationalised as an AIF agent (or hierarchy of AIF agents — population-level prior plus per-band policy refinement, per D-A7) governed by the seven AIF library patterns above. Concretely:

- The substrate (`ukrn_v2_substrate.clj`) populates each agent's *generative model*: priors over capability state, beliefs over institutional state, evidence-precision registry per priority category.
- The dynamics (`ukrn_v2_dynamics.clj`) implements the *active-inference loop*: observe (faceted reach + capability-state evidence + competitor coverage), score candidates (UKRN-S capability action set; G computed per term per scorecard), select (softmax under commitment temperature), enact (lever application; capability transitions).
- The engagement (`ukrn_v2_engagement.clj`) implements the *commitment temperature path-integral*: per-tick gating; truncation/pause/continue logic; integrated value bookkeeping.
- The runner (`ukrn_v2_runner.clj`) orchestrates the AIF loop across seeds and configurations; produces per-seed traces with the term-to-channel traceability discipline.
- The analysis (`ukrn_v2_analysis.clj`) aggregates traces across seeds; produces predictions; reports against the predictions; flags any deviation from AIF-pattern-compliant output.

The futon5 `cyber-ant uplift` machinery makes this substantially lighter to operate. Per the cyber-ant-uplift pattern's compositions: flexiarg patterns describe the behaviours; futon5 EDN binds them to AIF overrides; the futon5 runner attaches the configs and runs telemetry.

If VERIFY uses the futon5 uplift route, it gets `--aif-weight` / `--aif-guide` / `--aif-mutate` parameter-sweep machinery and futon5-managed telemetry. The v2 mission's VERIFY can then focus on substantive prediction-checking rather than infrastructure-building.

### PSRs and Pattern Authorship

Pattern Selection Records for ARGUE (extending DERIVE's PSRs):

- **From the AIF library** — all seven patterns selected and applied (table above). No selection is contested; all map cleanly.
- **From system-coherence library** — the patterns selected in DERIVE remain warranted.

Patterns that the v2 mission's work *generates* and authors:

1. `system-coherence/single-seed-results-need-multi-seed-validation` — *due now*. The deferred pattern. Author alongside `ukrn_v2_runner.clj` construction.
2. `aif/willingness-to-pay-as-commitment-precision` (working title; v2-specific addition to the AIF library) — captures the path-integral construction as an extension to `policy-precision-commitment-temperature`. Authorship driven by VERIFY findings.
3. `aif/capability-state-as-operational-hypothesis-extension` (working title; v2-specific) — captures the capability-state dynamics as an explicit AIF construct. Authorship driven by VERIFY findings.

Items 2 and 3 are pattern *candidates*. They become PURs (Pattern Use Records — successful applications) only if VERIFY confirms their utility. If VERIFY finds the path-integral construction or capability-state dynamics produce no new findings, the candidates do not get authored; the v2 work uses but does not generalise them.

### Notes on this ARGUE draft

- Drafted 2026-05-07 directly continuing DERIVE.
- The AIF cross-reference (table) is the *theoretical-coherence finding*: the v2 architecture's complexity matches the AIF formalism's requirements for this domain. This is the answer to "have we over-engineered?" — no, we have just made what was implicit in v9's AIF interpretation explicit.
- The translation to futon5 AIF+ via cyber-ant-uplift is a strategic move: it gives VERIFY infrastructure leverage (sweep machinery, telemetry) without requiring v2-specific re-engineering.
- The seven predictions (P-v2-1 through P-v2-7) are the falsifiable claims VERIFY runs against. Each has a computable falsifier; none rest on narrative alone.
- The plain-language argument is the version that survives translation to the working paper, the slide deck, or external reviewers. It does not require the AIF formalism to defend; it does require the AIF formalism to *ground*.
- Three pattern candidates flagged for authorship: one due now (single-seed-validation), two emerging from this mission's work (willingness-to-pay-as-commitment-precision; capability-state-as-operational-hypothesis-extension). Authoring the latter two is contingent on VERIFY findings.

---

## 5. VERIFY

### Frame: from architectural commitments to a running simulation

VERIFY for this mission is *construction-and-running-and-reporting*, in that order. Where the parent mission's VERIFY phase ran an existing simulation against predictions, this mission's VERIFY phase first constructs the simulation, then exercises it across multi-seed configurations, then reports results back into the architecture and into the working paper.

This means VERIFY is not a single drafting pass; it is incremental. Each construction step is a discrete deliverable; each multi-seed run is a discrete reporting event. The mission file accretes content as the work proceeds.

### Construction sequence (per D-A2)

Five notebooks, authored in dependency order:

1. **`ukrn_v2_substrate.clj`** — institutional population, capability state vectors, strategic priorities, budgets, facet vectors, UKRN-S abilities schema, competitor coverage. *No dynamics; just data structures and accessors.* **Status (2026-05-07): drafted, loads, smoke-tested.** Time anchor: tick = quarter, horizon = 20 ticks (5y, 2026-Q3 → 2031-Q3 default). 16 institutions (anonymised tags H1-H6 / M1-M4 / T1-T6 by reach within band). 5 institutional capabilities + 10 UKRN-S capabilities. 8 priority categories. 8 × 3 = 24 facet cells per institution. Competitor coverage table populated for two clusters. See decision log V1, V2 below.
2. **`ukrn_v2_dynamics.clj`** — capability transitions for both UKRN-S and institutions; lever-to-NPT-factor mapping (extended for facets); hierarchical AIF agent with population prior + per-band policy refinement. Depends on substrate. **Status (2026-05-07): drafted, loads, smoke-tested over 16 ticks (4 years).** UKRN-S capabilities cascade through dependencies as expected: training-delivery (tick 0) → cross-institutional-analysis (tick 1) → pattern-induction (tick 2) → topic-faceted-specialism + sector-translation (tick 3); plateau at 5 of 10 capabilities online by tick 4 (rest blocked by external conditions or higher-order dependencies). Multi-seed harness explicitly designed for: RNG passed per-step via deterministic `(seed + tick)` rather than global var. Population behaviour at tick 16: 7/16 reach Multiplied (all 6 high-band; 1 of 4 middle; 0 of 6 thin), more conservative than v9's 10/16 because capabilities take time to come online. Two bugs found and fixed during construction (V3, V4 below).
3. **`ukrn_v2_engagement.clj`** — path-integral willingness-to-pay; per-tick gating; trajectory truncation/pause/continuation logic; integrated-value bookkeeping. Depends on substrate and dynamics. **Status (2026-05-07): drafted, loads, smoke-tested with three scenarios.** With default thresholds (and default uniform-0.5 priorities), all 16 institutions sail through 20 ticks with no truncations and integrated-value 19 each — same Multiplied count as dynamics-only (7/16). Gating is responsive when conditions vary: an institution with civic-engagement-only priorities (which no current UKRN-S capability serves) goes :paused with integrated-value 0; an institution under tight-budget thresholds (cost=25/tick, replenish=5) :truncated at tick 5 with :budget-exhaust cause and integrated-value 4. P-v2-1 (truncation diagnosticity) verifiable once priority/competitor/budget heterogeneity is populated across institutions. See V5 below.
4. **`ukrn_v2_runner.clj`** — multi-seed orchestration; per-seed CSV; configuration sweep harness. Depends on the three above. **Status (2026-05-07): drafted, loads, smoke-tested with 5 seeds × 20 ticks.** Two modes (`run-config` for full trajectories, `run-config-finals` for finals only). CSV exports: trajectory CSV (long-format per-tick) and finals CSV (lighter; per-seed end-state). Cross-seed aggregation (`aggregate-finals`, `aggregate-population`) reports mean and 95% empirical CI per institution and per-band. Smoke test: with V6 noise applied, 5-seed CIs span — overall multiplied 5.20 [4–6 of 16]; per-institution mult-fractions in {0%, 20%, 40%, 60%, 80%, 100%}. Defaults: 50 seeds for headline, 20 for sensitivity (per D-A4 / W9). See V6 (noise) and V7 (facet-emphasis bug) below.
5. **`ukrn_v2_analysis.clj`** — cross-seed aggregation; sensitivity / tautology audit; figure generation; reporting. Depends on runner output. **Status (2026-05-07): drafted, loads, smoke-tested.** Three predictions tested in this session (P-v2-1, P-v2-6, P-v2-7); four stubbed for next iteration (P-v2-2, P-v2-3, P-v2-4, P-v2-5). `prediction-status-report` produces a table-style verdict for each prediction with CI-aware status. `headline-baseline`, `tautology-audit`, and `compare-configs` are reusable across future predictions. CSV exports ready for downstream figure rendering. **Headline finding: v2 multi-seed contradicts v9's R6.** v9 reported 10/16 multiplied with design; v2 (50 seeds, default config, default thresholds) reports mean 5.40, CI [4, 7] — v9's value of 10 falls outside the CI. v2 is more conservative because capabilities take time to come online and noise widens variance. This is consistent with the mission's stated purpose: building a simulation we can be confident about, even when its findings are weaker than the prior simulation's.

### Decision log (VERIFY revisions)

*Decisions made during construction that revise upstream commitments are recorded here.*

**V1 (2026-05-07) — Substrate adopts default starting facet vector as zero-reach across all (topic × audience) cells.** Each facet starts at `{:reach 0.0 :uptake 0.0}` rather than carrying inherited reach from bulk reachability. This means the institution's *bulk* reach (from corrected ORP reachability) gives the starting *D* value via band placement, but the *facet* picture starts empty. This is the correct shape: the simulation's job is to model how UKRN-S engagement populates the facet vector over time, starting from a reach-by-facet of zero for engagements that haven't happened yet. The bulk reach informs starting-D (D-axis aggregate position) without pre-loading the facet structure. *Rationale:* bulk historical reach is not the same as forward facet reach; conflating them would smuggle in the v9 tautology by another route.

**V2 (2026-05-07) — Capability state at tick 0 is uniformly zero.** Both UKRN-S' capabilities and each institution's capabilities start at depth 0 (not online). Capabilities come online during simulation under explicit transition rules, never at construction time. *Rationale:* this is what "capabilities-coming-online" means; pre-populating any capability would import unsupported assumption.

**V3 (2026-05-07) — Dynamics bug: nested `:promote` access.** First dynamics smoke test showed zero capability promotions across 4 ticks despite favorable G values. Root cause: `step-band` returns the full *scored* map `{:candidate {:promote ... :emphasize ...} :G ...}`, not the candidate directly. The promotion-collection code was reading `(:promote (val band-entry))` — looking up `:promote` on the outer scored map (which doesn't have that key) and finding nil. Fixed by `(:promote (:candidate (val band-entry)))`. *Pattern lesson:* when functions return scored/wrapped values rather than raw items, every subsequent access must respect the wrapping. This is a common AIF-pipeline bug; worth flagging in any future authorship.

**V4 (2026-05-07) — Substrate must populate NPT factors at construction.** First dynamics smoke test showed institution H1's D collapsing from 0.85 to 0.47 in one tick. Root cause: `make-institution-state` stored only `:starting-D` as metadata, with no actual NPT factors in the institution map. `step-institution` then dissoc'd metadata to extract NPT and got an empty map; `base/d-and-a` defaulted all factors to 0.4 (uniform), giving D = pattern-strength(0.4) ≈ 0.47. Fixed by adding `npt-from-DA` (inverse of pattern-strength) to substrate; institution states now carry actual NPT factor values consistent with their band-derived starting D and a placeholder uniform A=0.5. *Pattern lesson:* substrate must include all state needed by dynamics; metadata-only substrates produce silent defaults under dissoc-style extraction. Use explicit `select-keys` with a known key set instead.

**V5 (2026-05-07) — Path-integral semantics: pause is recoverable; truncate is sticky.** The engagement module distinguishes two failure modes. *Pause* (priority alignment dips below pause threshold) means the institution is not engaged this tick but may re-engage if alignment recovers; integrated-value does not accumulate; NPT/facet state is rolled back. *Truncate* (competitor closes the gap; budget exhausts) is permanent; once truncated, the institution stays truncated for the remainder of the run; truncation tick and cause are recorded. *Rationale:* matches the MAP description of the path integral — engagement either persists (priorities align, competitors haven't closed, budget is fine), pauses (priorities misalign temporarily), or breaks (competitors win or money runs out). The pause/truncate split makes path-integral findings diagnostic per P-v2-1.

**V6 (2026-05-07) — Gaussian noise on NPT factor updates is required for multi-seed variance.** First runner smoke test (5 seeds × 20 ticks) revealed deterministic outcomes across seeds — every seed produced identical Multiplied counts and identical D, A, integrated-value to four decimals. Cause: although `step` constructs a fresh RNG per tick, the per-tick randomness was only consumed by softmax candidate sampling (which shared promoted capability across all 24 facet variants of "promote X") — so action choice across seeds resolved to the same UKRN-S capability, the same lever activations, the same deterministic NPT factor updates. With no stochastic perturbation in the dynamics themselves, multi-seed runs produced zero variance and the multi-seed validation pattern (W9) became inert. *Fix:* added gaussian noise per NPT factor per tick, with sigma=0.012 (matching v9's `base/step`). RNG is threaded explicitly through `step → step-institution → apply-levers-to-npt`. After the fix, smoke test produces 5-seed CIs that span (overall multiplied: 4-6 of 16; per-institution mult% in {0, 20, 40, 60, 80, 100}). *Pattern lesson:* cross-seed validation requires that the dynamics themselves carry noise. Action-selection randomness alone is not enough when the action space collapses to deterministic outcomes across most candidate variants.

**V7 (2026-05-07) — Facet emphasis bug: action wrapper not unpacked.** While threading rng through `step-institution`, surfaced a previously-undetected bug of the same class as V3: `step` was calling `(step-institution inst ukrns-state' (:emphasize action))` where `action` is the scored wrapper map `{:candidate {:emphasize ...} :G ... :priority-alignment ...}`. So `(:emphasize action)` always returned nil, and `update-facets` was always called with `emphasized-facet = nil`, causing the 2.5× emphasis bonus to never fire. The simulation has been running since V3 fix with no facet-emphasis effect — a silent bug that didn't surface in smoke tests because facets were still growing under the baseline coverage from active capabilities. *Fix:* `(:emphasize (:candidate action))`. *Pattern lesson reinforced:* when functions return scored/wrapped values, every subsequent access must respect the wrapping. Worth a future pattern: `unwrap-scored-values-before-access`.

**V8 (2026-05-07) — Substrate stores realised D and A, not target inputs.** Reader-facing trajectory figure for v10 showed visible "rays" — vertical line segments at the start of each trajectory. Investigation: `make-institution-state` was storing `:starting-A = 0.5` (the target input to `npt-from-DA`), but the actual A computed from the synthesised NPT factors was lower (e.g. 0.39 for T6) because shared factors are pinned by `min(u_D, u_A)` and drag the realised A downward when target-D and target-A diverge significantly. So at tick 0, the trajectory plotter read 0.5 (the metadata-stored target); at tick 1, the dynamics computed A from the NPT factors and got 0.39. The 0.108 jump between tick 0 and tick 1 is what rendered as the "ray." *Fix:* `make-institution-state` now computes the realised D and A from the synthesised NPT factors at construction time, and stores them in both `:starting-D` / `:starting-A` (preserved name) and `:current-D` / `:current-A` (explicitly set so the trajectory plotter reads consistent values from tick 0). The original target inputs are preserved as `:target-D` / `:target-A` for documentation. *Pattern lesson:* when synthesising state from a target via an inverse function, store the realised values that the dynamics will actually see, not the target inputs. Otherwise the tick-0/tick-1 transition exposes the inconsistency. Headline numbers unchanged (mean 5.36, CI [4, 7]); this was a bookkeeping fix.

**V9 (2026-05-07) — Decay applies only to disengaged institutions.** First v10 trajectory figure showed high-band institutions (H1, H2, H3) drifting *downward* on D over time — incoherent for institutions actively engaged with UKRN-S. Diagnosis: the simulation was applying decay (regression-to-baseline at rate 0.01 per tick) to every institution per tick, including those receiving continuous reinforcement from UKRN-S' active capabilities. For an :active institution, engagement *is* the reinforcement; applying decay on top represents double-counting (lift representing reinforcement; decay representing absence-of-reinforcement). *Fix:* `apply-levers-to-npt` now reads `:engagement-status` from the institution state and applies decay only when status is not `:active`. So :active institutions only receive lift; :paused / :truncated institutions still decay (regression toward baseline when disengaged). *Pattern lesson:* model-level "regression-to-baseline" terms must respect whether the regression is being counteracted by engagement. The v9 design (no engagement gating) was incoherent; v2's engagement layer makes the correct gating possible.

**V10 (2026-05-07) — Time-condition bug: count ticks since onboarding, not events.** While re-running with V9 fix, noticed that capabilities with time-based dependencies (e.g. `:in-flight-eval-loops-ukrns` which requires `cross-inst-analysis-online-quarters 3`) were never coming online. Diagnosis: `time-condition-met?` was counting onboarding *events* in history (`(count (filter #(= (second %) :cross-institutional-analysis) history))`) rather than counting *ticks since* the single onboarding event. cross-institutional-analysis only onboards once, so `count = 1`, never reaching the required 3. *Fix:* find the first onboarding tick for the prerequisite capability and compute `current-tick - onboarded-tick`. *Effect:* Phase 2 of capability buildout becomes visible (in-flight-eval-loops-ukrns at tick 4 / one-year-after; osmi-uuk-relationships at tick 7 / two-years-after). The trajectory figure now shows real phase transitions at later ticks.

**Combined V9 + V10 effect:** Headline mean shifts from 5.36 to **7.82**; CI tightens from [4, 7] to **[7, 8]**. Per-band: high-band 6/6 (was 4.3/6); middle-band 1.82/4 (was 1.1/4); thin-band 0/6 (unchanged). v9 claim of 10/16 is still outside v2's CI but the contradiction has narrowed substantially. v10 abstract and §1 updated with new numbers and band breakdown.

### Prediction results

*Per-prediction status, populated as multi-seed sweeps complete.*

| Prediction | Status | Cross-seed result | Verdict |
|---|---|---|---|
| P-v2-1 truncation diagnosticity | tested-2026-05-07 | n-truncations=0 under heterogeneous-priority population (20 seeds) | uniform — priority differences not extreme enough; refinement needed |
| P-v2-2 capability-onboarding-order | not-yet-tested | — | requires variants of ext-state schedule |
| P-v2-3 within-band facet heterogeneity | not-yet-tested | — | requires within-band priority diversity calibrated against actual data |
| P-v2-4 v9-collapsed contrasts stay collapsed | not-yet-tested | — | requires v2 implementation of v9 timing/prior/coupling gates |
| P-v2-5 v9-surviving contrasts persist | not-yet-tested | — | requires funder-multiplier and distributed-evaluator-config gates in v2 |
| P-v2-6 tautology break under willingness-to-pay | tested-2026-05-07 | base 5.40 (CI 4-7); ±0.1 perturb → 2.90/7.00 (shifts -2.50/+1.60) | tautology partially broken — outcomes not fully predicted by starting position; dynamics doing some differential work |
| P-v2-7 empirical landscape (R6) survives or refines | tested-2026-05-07 | v2 mean 5.40, CI [4, 7] vs v9 claim 10 | contradicts-v9 — v2 reports a more conservative finding; capabilities-coming-online + multi-seed noise → 5.40 not 10. *v9's R6 was an over-estimate under v2's stricter regime.* |

### Sorry statuses

*Final per VERIFY, populated as construction and runs complete.*

| Sorry | Status | Validation |
|---|---|---|
| 1. Faceted reach | substrate-built; dynamics-faceted | facets carried per institution × topic × audience; emphasis bonus operates (V7); facet-level reporting awaits priority calibration data |
| 2. UKRN-S capabilities | schema-sketched; dynamics-built | 10 capabilities online cascade through dependencies as expected; refinement during VERIFY ongoing |
| 3. Institutional capabilities | schema-sketched; substrate-built | 5 capabilities present in substrate; dynamics not yet activating them (next iteration) |
| 4. Time anchoring | closed (D-A3) | tick = quarter; horizon = 20; 2026-Q3 → 2031-Q3 default |
| 5. Multi-seed validation | closed (D-A4 + W9 pattern) | runner produces CIs; V6 noise added to make multi-seed meaningful |
| 6. Empirical A | acknowledged dependency | placeholder uniform A=0.5; substrate accepts empirical-A inputs when available |
| 7. Tautology break | partially closed (P-v2-6) | sensitivity audit shows shifts -2.5/+1.6 vs perturbation 0.1; tautology partially broken |
| 8. Per-profile agency | closed (D-A7) per-band first | hierarchical agent produces per-band actions; per-profile reserved |
| 9. Missing institutions | closed (D-A5) parameterised | institution set is configuration input; extended-set re-run is configuration change |

### Pattern Use Records (PURs)

*Patterns confirmed as load-bearing by VERIFY findings. PSRs become PURs only when applied successfully against simulation behaviour.*

(none yet — PSRs from DERIVE/ARGUE await VERIFY confirmation)

### Notes on this VERIFY draft

- Begun 2026-05-07 with substrate construction.
- VERIFY accretes; this mission file gains content as construction progresses.
- The frame above is what permits later sessions to pick up exactly where this one left off.

---

## 6. INSTANTIATE

*To be drafted. The INSTANTIATE phase will describe the handoff: how the new simulation enters the working paper's next iteration, who (Researcher-Advisor role) maintains it, and how it is updated when fresh evidence (T1 wave, indicators-track, additional reachability) arrives.*

---

## 7. DOCUMENT

*To be drafted. The DOCUMENT phase will produce: the methods supplement; the new pattern `system-coherence/single-seed-results-need-multi-seed-validation`; possible additional pattern(s) for capability-state representation; any updates to `argument_v9.sexp` reflecting which sorries closed.*
