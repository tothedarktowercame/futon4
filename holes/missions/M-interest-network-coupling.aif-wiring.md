# Wiring diagram — Interest-Network → War-Machine AIF coupling

*Design artifact for the M-interest-network-coupling extension into WM-AIF territory.
Grounded in a read-only mapping pass over `futon2/src/futon2/aif/*` (2026-05-29).
NOT a build. To be ratified by Joe before anything is wired. Line refs are from the
mapping pass; confirmed-at-build-time.*

## 0. Why this exists

"Coupling" must mean more than a read-out surface. It means: the interest network
becomes a **preference source** for the War Machine's active inference, so the WM
prefers policies that *finish what Joe started* and *make a long-standing interest real*.
This diagram answers one question — **how does the AIF model consume interest-network
signals?** — before any risky live wiring.

## 1. The two systems and the substrate bridge (Option B)

```
  INTEREST NETWORK (futon4/futon5a)                WAR MACHINE AIF (futon2)
  ─────────────────────────────────               ────────────────────────
  event log (markdown CP batches)                  judge [scan-data opts]   (war_machine.clj:3099)
        │ replay (interest-event-replay)             │
        ▼                                            ├─ observe → 14-channel obs   (observation.clj:11-34)
  projection:                                        ├─ bootstrap BELIEF            (belief.clj:133-166)
   • entity-standing {id → status}                   │    {entity-id → posterior over
   • network-edges (link/asserted)                   │     #{:spawned :refined :strengthened
   • completeness-vector                             │      :addressed :falsified :foreclosed
   • [morning model] activation, express/acquire,    │      :reopened}}  ← == M-INC VOCAB
     prior-evidence depth (rhizome), degree          ├─ propose candidates {:type :target :weight}
        │                                            ├─ enrich-candidates-with-structural-pressure (:3269)
        ▼                                            ├─ ★ enrich-candidates-with-INTEREST  (NEW seam)
   ════ XTDB SUBSTRATE (futon1a, :7071) ════         ├─ efe/rank-actions   (efe.clj:110-212)
   interest-event entities  ──────────────────────► └─ policy/select-action (softmax on −G-total)
   (Option B: events written as entities;
    WM reads them at cycle time)
```

The substrate (XTDB) is the bridge: Option B writes interest-events as entities; the WM
cycle reads the projection from substrate and biases policy selection. Substrate-residency
earns its risk *here* — because now something (the WM) reads from it.

## 2. The AIF consumption surface (what the code actually offers)

| Surface | Where | Can an external signal set it? |
|---|---|---|
| **Preferences (C vector)** — 14 obs channels → `[lo hi]` healthy ranges | `preferences.clj:8-23` (immutable `def`) | **No seam.** No setter; read as a compile-time var by `free_energy.clj`. Changing it = `def`→atom refactor (higher risk). |
| **Belief** — `{entity-id → posterior over status-set}`; status-set **== M-INC vocab** | `belief.clj:35-40,133-166` | Yes, via **belief events** `{:entity-id :type :weight}` (`belief.clj:69-77`) through the R3 loop. Diffuse (shifts aggregate obs), not per-target. |
| **Candidate action fields** — `:intrinsic-value` (−G-risk), `:structural-pressure-per-action` (−G-total) | `efe.clj:185,190`; enrichment pattern at `war_machine.clj:256-267,3269` | **Yes — the clean seam.** `compute-efe` already honours these; decorate candidates before `rank-actions`. Per-target. |

**EFE** ranks actions by `G-total` ascending; preferences enter G-risk via
`channel-gap(predicted-obs, preferences) × pragmatic-weights`. Per-candidate
`:intrinsic-value` subtracts directly from G-risk; `:structural-pressure-per-action`
subtracts from G-total. Actions are `{:type :target :weight}`,
type ∈ `#{:no-op :address-sorry :open-mission :fire-pattern :learn-action-class}`,
`:target` = a real entity-id (sorry-id / mission-id / belief section-id).

## 3. The two coupling routes

**Route 1 — per-candidate enrichment (RECOMMENDED, low-risk).**
A new adapter produces a per-target bias map; a new enrichment step assocs it onto
candidate actions just after `enrich-candidates-with-structural-pressure`
(`war_machine.clj:~3270`). **No change to `efe.clj`, `preferences.clj`, or
`free_energy.clj`.** Interest signals are inherently per-entity, which matches
per-candidate (per-`:target`) biasing.

**Route 2 — belief events (deeper, M-INC-vocab-aligned).**
Emit interest-network state events as belief events; they flow through R3 and shift
predicted observation, biasing EFE *diffusely* (through aggregate channels, not
per-target). This is the path the belief docstring anticipates. Add it *after* the
entity-id bridge (§5) exists; it complements but does not replace Route 1.

**Recommendation:** Route 1 first (direct, per-interest, no core changes), Route 2 as a
second channel once the id-bridge is solid. Do **not** refactor the C vector to an atom
for v1 — higher risk, and per-target is the right granularity anyway.

## 4. The two preferences, computed from the projection

For a WM candidate action with `:target` t (reconciled to interest entity/territories — §5):

- **"Finish what you started"** → `:intrinsic-value`
  `unfinished(t) = standing(t) ∈ {:live :refined :strengthened :reopened}`
  `boost = w_finish · activation(t) · 1[unfinished(t)]`
  (in-progress + highly activated ⇒ pull G-risk down ⇒ WM prefers acting on it.)

- **"Make a long-standing interest real"** → `:structural-pressure-per-action`
  `latent(t) = prior_evidence_depth(t) [rhizome longevity] · degree(t)`
  `deficit(t) = 1 − actualization(t)  [few expressing essays / not yet :strengthened]`
  `boost = w_real · latent(t) · deficit(t)`
  (deep roots + high connectivity + not-yet-made-real ⇒ urgency to realize it.)

This is the morning's morphogenetic model landing operationally: a candidate action
*expresses* a subset of interests and *acquires* another; the express-set with high
activation drives `:intrinsic-value`, the long-latent under-actualized set drives
`:structural-pressure-per-action`.

## 5. The crux / the main risk: entity-id reconciliation

The two id-spaces do **not** overlap out of the box:

- **WM `:target` space** = sorry-ids, mission-ids, stack-annotation section-ids.
- **Interest-network entity space** = EoI essays (`vsat-poc-...-scenario-c`),
  interest-territories (`peer-learning`, `cross-substrate-invariance`),
  institution-objects (`:node/...`).

A candidate WM action is "address-sorry on S" or "open-mission M" — *not* "express
interest X". So the coupling needs a **bridge map**:

```
wm-target-id  →  #{interest-territory}     ;; which interests this action would express/finish
```

Proposed source (no new authoring if possible): the bipartite graph already tags
**essays → territories**; extend tagging to **missions → territories** (via each
mission's `source_file` / `eoi_instances[].institution_object_ref`) and **sorries →
territories** (via `sorrys.edn`). Then "finish/realize interest X" biases WM actions
whose target expresses X. **This bridge is the new load-bearing artifact; without it the
coupling is unfounded** (a subsumption-claim trap). It must be designed before the build.

## 6. What exists vs what is new (honesty)

| Piece | Status |
|---|---|
| Belief status-set == M-INC vocab | EXISTS (`belief.clj`) |
| Candidate `:intrinsic-value` / `:structural-pressure-per-action` honoured by EFE | EXISTS (`efe.clj:185,190`) |
| Enrichment-before-rank pattern | EXISTS (`war_machine.clj:256-267,3269`) |
| `futon2/aif/adapters/` directory | EXISTS (natural home) |
| Interest-events resident in XTDB (Option B) | NEW (task 4) |
| **entity-id bridge map** (wm-target → territories) | **NEW — the crux (§5)** |
| `futon2.aif.adapters.interest-network` adapter | NEW (produces per-target bias map) |
| enrichment call-site at `war_machine.clj:~3270` | NEW (one `defn` + one threading insert) |
| `w_finish`, `w_real` weights + activation/actualization metrics | NEW (need definition) |

## 7. Open questions for ratification

1. **Route 1 first** (per-candidate), Route 2 (belief events) later — agreed?
2. **The id-bridge (§5):** tag missions+sorries with territories via existing
   annotations — acceptable, or do you want a different reconciliation?
3. **Action scope:** should interest-coupling bias only `:open-mission` /
   `:address-sorry` targets (the "finishable" ones), or all action types?
4. **Weights:** `w_finish` / `w_real` — start hand-set and tune, or learn via the
   existing `intrinsic_values.clj` Beta-posterior outer loop?
5. **Inspectability:** every interest-driven bias should be traceable back to the
   interest signal that caused it (which territory, which standing) — confirm this is a
   hard requirement (it should be, for the AIF to remain interpretable).

## 8. Demonstration result (2026-05-29) — offline, real efe

Harness: `futon2/scripts/futon2/aif/interest_coupling_demo.clj`
(`cd futon2 && clojure -M -m futon2.aif.interest-coupling-demo`). Real `efe/rank-actions`
in a transient JVM; representative WM observation + candidate set; interest bias derived
from the real projection (`interest-network-standing-v1.edn`) via the explicit
M-interim-director bridge (§5). **NOT a live WM cycle** — a mechanism proof on real
interest signals + a representative state. The live XTDB→WM run is the next step.

Bias applied to `:open-mission :m-interim-director`:
- `:intrinsic-value` 0.135 — finish: activation 0.9 × unfinished; from hyperreal + scenario-c `:strengthened`
- `:structural-pressure-per-action` 0.72 — make-real: latent 0.8 × actualization-deficit 0.9

| action | baseline rank | coupled rank | G-total base → coupled |
|---|---|---|---|
| open-mission :m-interim-director | 4 | **1** | −5.1580 → −5.5450 (−0.3870) |
| no-op | 1 | 2 | −5.2000 (unchanged) |
| address-sorry :sorry-r3d-attribution | 2 | 3 | −5.1790 (unchanged) |
| fire-pattern :p-anamnesis | 3 | 4 | −5.1670 (unchanged) |
| open-mission :m-weird-modernism | 5 | 5 | −5.1580 (unchanged) |

Mechanism (fully traceable): ΔG-total = −0.3870 = −0.135 (`:intrinsic-value` → G-risk)
+ −0.252 (0.72 `:structural-pressure-per-action` × 0.35 default weight → G-total). No other
candidate moved in G-total; they only shift rank as M-interim-director climbs.

Weight regime (Q4): with these hand-set weights the coupling **flips the top action**
(no-op → M-interim-director). The flip threshold is small here — no-op leads M-interim-director
by only 0.042 in baseline G-total, and the `:intrinsic-value` term alone (0.135) already
exceeds it, so `w_real` is not the deciding knob. To get the
**re-order-the-tail-without-flipping-the-top** regime, *both* weights must be much smaller
(combined G-total bias < 0.042). Both regimes are reachable; the forward-model effect is real
and fully attributable to the interest signal either way.
