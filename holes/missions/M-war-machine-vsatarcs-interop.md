# M-war-machine-vsatarcs-interop: Operational convergence of WM and VSATARCs

**Date:** 2026-05-21 (HEAD authored from operator dialogue).
**Status:** HEAD. Three point-to-point bridges exist (WM-trace read, XTDB clicks consumer, intrinsic-values hyperedge round-trip); no cross-side coherence audit; mission scopes the convergence.
**Owner:** Joe (operator); claude-4 (HEAD author); IDENTIFY ownership tbd.
**Blocked by:** None directly. Builds on v0.5.31+ bilateral apparatus + v0.5.33 substrate-signal marginpars.
**Companion missions:** `M-stack-essay-code-alignment` (the umbrella that named the VSATARCs reader work); `M-war-machine-aif-last-mile` (WM-side last-mile, claude-9's mission home); `M-self-documenting-stack` (claude-2 closed POC; LC1 surface is one of three the interop must accommodate); `M-editorial-assistant` (claude-10's writer-side surface that this mission must NOT break).
**Issue-queue entry:** `iq-015`.

## HEAD — Operator-voice anchor

> "My only concern right now is that if VSATARCS remains totally filesystem backed, it may end up drifting out of sync with the War Machine and XTDB. Maybe that's not a huge issue but it's not a tiny one either. Actually, I tell you what, I think what we should do is press on with B, and perhaps also A, and then create an M-war-machine-vsatarcs-interop mission that brings the two together operationally."
>
> — Joe, 2026-05-21 emacs-repl (after authorising the v0.5.33 substrate-signal marginpar upgrade)

### What's already felt to be true

The bilateral closures stacked across the last several sessions — claude-2's M-vsatarcs-writer L3/L4/L4+ shipping `:mission-doc-sync` + `:aif-edn-revision-entry` + `:story-update`; claude-4's R-axis polish v0.5.13 → v0.5.27 closing R1-R11; claude-9's R12 narrow-take-up apparatus 2026-05-21; claude-4's symmetric VSATARCs port v0.5.31 — *demonstrate that bilateral coherence is achievable when both sides intentionally ship to a shared shape*. The handoffs in `~/code/futon0/holes/handoffs/` worked. The bilateral-evidence block in `vsatarcs-alignment-completeness.aif.edn` accumulated 21 entries spanning 7 evidence-kinds. Both R12 contract rows flipped on the same day.

**But the coherence is artisanal**. Every cross-side fact is hand-recorded; nothing live-verifies that the bilateral-evidence entries still hold; mission-status references in marginpars are static text rather than live queries; the v0.5.33 substrate-signal marginpars surface filesystem state but not WM state. The drift surface is real even though the apparatus didn't drift today.

### Anti-glibness discipline

The mission would be superficial if:

- "Interop" became a euphemism for "make VSATARCs write into XTDB and call it done." Writing-into-XTDB-doesn't-fix-drift if the bidirectional invariants aren't named. **Discipline:** name the cross-side invariants first (which facts must hold simultaneously across both sides; how violation is detected; what reversal looks like); name the bridges second.
- The mission tried to satisfy all of M-the-futon-stack Q6 (kill-switch + full debug surface + reversal protocol) inside its scope. Q6 stays its own broader mission (per `~/code/futon0/holes/handoffs/r12-to-stack-Q6-2026-05-21.md`'s narrow-subset discipline); this mission is *just* the WM ↔ VSATARCs operational convergence.
- "Live verification" of bilateral-evidence entries became continuous-polling that drowns the apparatus in coherence-check noise. **Discipline:** verification fires on revision boundaries (when `.aif.edn` updates) and on WM trace growth (when new records land), not on a fast tick.
- The mission ignored the three-surface coordination question (WM UI + VSATARCs chrome + LC1 mission-search). iq-009 is already deferred-until-WM-UI-lands. **Discipline:** the interop work must inform iq-009's eventual decision, not pre-empt it.

### Working-economy position

The interop underwrites the **next-iteration substrate** for the issue-queue + bilateral-evidence pattern: instead of agents discovering coherence by narrative-reading revision entries, they query a unified surface. Today an agent landing on the bilateral apparatus has to read `vsatarcs-alignment-completeness.aif.edn` AND `futon2/docs/futon-aif-completeness.md` AND every handoff in `~/code/futon0/holes/handoffs/` AND the WM trace files to know what's true. That cost compounds with every new agent + every new revision; the mission's payoff is a unified-substrate query surface that makes onboarding O(1) instead of O(n) in revisions.

Underwriting it: the v0.5.22 safety-property family + the v0.5.31 bilateral atom-with-bootstrap-replay shape (both sides hydrate from XTDB hyperedges; both sides can write to XTDB; XTDB IS the natural shared substrate that the interop layers atop). No new infrastructure; just a coherence layer over what already exists.

### Clarity-gap / carried-forward tensions

- **Which facts are authoritative on each side**, and which are projections. Today: `.aif.edn` is authoritative for the contract; WM trace files are authoritative for tick history; futon1a XTDB holds hyperedges from both sides + mission-doc records + writer-trace records. Interop must name authoritativeness explicitly so coherence-check direction is unambiguous.
- **Whether the bilateral-evidence block should be live-derived** (rebuilt every revision from XTDB queries) **or stay hand-authored** (current shape; richer prose but no coherence guarantee). Hybrid possible: hand-authored prose + live-derived assertions tested at revision-time.
- **What "mission-status live-query" means concretely**. claude-2's LC1 uses `mission/M-<name>@<repo>` form; VSATARCs uses bare basename (iq-005). Convergence is needed; whether it's done by adopting LC1's form or by adding a translation layer is open.
- **R12 outer-loop bidirectional concern**. WM-side outer-loop runs daily under cron; VSATARCs outer-loop deferred (iq-004). If both run, they read/write disjoint hyperedge types (`code/v05/wm-hyperparameter-update` vs `code/v05/vsatarcs-hyperparameter-update`); no cross-pollution. But if a future R12 variant wants cross-class learning (operator credit flowing between writer-action-classes and WM-action-classes), the schema needs design.
- **Reversal-protocol scope**. The interop touches mutable substrate (XTDB writes via hyperedge POSTs). Reversal — operator emits a correction that the system treats as ground truth — is currently a hand-edit on `.aif.edn` + a re-fetch on XTDB read paths. Operational-reversal-as-first-class-action is Q6 territory; the interop mission tracks the gap.
- **Three-surface composition** (WM UI + VSATARCs chrome + LC1 mission-search). iq-009 is the decision point; this mission's design choices must leave room for whichever architecture wins.

### Provenance

Authored 2026-05-21 from the emacs-repl dialogue between Joe (operator) and claude-4 (co-author). Sequence: Joe authorised v0.5.33 substrate-signal marginpars (option B of the marginpar fork); flagged the drift concern verbatim above; named the mission this is HEAD-authoring. No flash, no interview, no separate intake artifact — operator-voice in this doc is preserved verbatim from that session.

Issue-queue entry `iq-015` created in the same revision (v0.5.33) as the mission HEAD to keep queue-and-mission cross-pointed.

## 1. IDENTIFY (deferred)

Empty at HEAD. IDENTIFY work begins when an agent (claude-4 or fresh session) picks up the mission and starts naming the cross-side invariants concretely.

**Suggested first move at IDENTIFY**: enumerate the cross-side facts that *must* hold simultaneously, grouped by the bridge that carries them:

- **WM-trace-read bridge** (`arxana-vsatarcs-wm-bridge.el`, claude-4 v0.2.5): for each WM-side `:mu-post` entry, the VSATARCs-side mirror exists and matches within tolerance. **Already verified** by `arxana-vsatarcs-belief-compare`; coherence-check is a one-liner over the live state.
- **XTDB-clicks bridge** (`arxana-vsatarcs-xtdb-clicks.el`): for each `code/v05/watcher-event` hyperedge, the corresponding VSATARCs reader-tick is present. **Partially verified** today (the chrome's stream count is the observable; no programmatic assertion).
- **Intrinsic-values bidirectional bridge** (`arxana-vsatarcs-intrinsic-values.el` v0.5.31 + WM-side `futon2.aif.intrinsic-values`): both sides' bootstrap-replays produce the same posteriors when reading the same hyperedge stream. **NOT verified** today (no shared-input test; both sides read different streams).
- **Bilateral-evidence entries**: each `:wm-id` reference resolves to an existing WM-side artifact (event-id, commit, mission-doc anchor, etc.). **NOT verified** today (text-only); claude-2's M-vsatarcs-writer §3 D3 named `:writer-event-id` as the forward-pointer for canonical cross-references.
- **Mission-status references**: claude-2's LC1 form `mission/M-<name>@<repo>` resolves; VSATARCs's bare-basename form maps to a unique resolution. **NOT verified** today; iq-005 names this.

## 2. MAP / DERIVE / ARGUE / VERIFY / INSTANTIATE / DOCUMENT (deferred)

Phase work begins when IDENTIFY lands.

## Checkpoints

### Multi-agent collaboration framing — 2026-05-23 (held, not yet dived into)

Joe (operator) flagged that this mission is **very similar in spirit** to
`~/code/futon7/holes/M-war-machine-frontend-upgrade1.md` (claude-9 owns it):
both descend from `~/code/futon7/holes/M-war-machine-aif-last-mile.md` and
both have to do with making the WM apparatus operationally legible across
its surfaces.  Claude-9's mission is the **WM UI side** (rendering / view
modes / six tour-findings); this mission is the **VSATARCS substrate side**
(WM-trace ↔ XTDB ↔ VSATARCS coherence).  The bar Joe surfaced: "someone is
going to have to do QA of the War Machine UI, but as we do that, I want to
make sure we're reflecting our changes into VSATARCS as well."

That's a paired-discipline shape: when claude-9's UI rendering changes
expose new operator-legible WM state, this mission's coherence-check is
where the change becomes durable on the VSATARCS side; conversely, when
this mission's substrate work names cross-side invariants, claude-9's UI
should be the surface where the operator sees violations.

The pairing's first concrete move is a **multi-agent collaboration over
whistle** (per `~/code/futon3c/README-bells-and-whistles.md`); other agents
may be roped in (claude-2 for M-war-machine-aif-last-mile's salt-down
voice, claude-4 for the v0.5.x bilateral-apparatus history, codex for
specific implementation handoffs) as the work shape clarifies.

#### Foreword v2 annotations as synoptic surface

A v2 annotation pass landed today against
`~/code/futon7a/essays/operator-foreword/annotations.edn` (13 entries):
4 `:foreword/gap-in-foreword`, 3 `:foreword/coverage-check`, 3
`:foreword/reader-prep`, 3 `:foreword/synthetic-question`.  Each
annotation cross-references a specific anthology entity in
`stack-annotations.edn`'s `:sections[]` (e.g.,
`arxana/stack/futon-v1/leaf/wm-r-criteria-walkthrough`,
`arxana/stack/futon-v1/war-machine/lucid-scenes`,
`arxana/stack/futon-v1/devmap/futon6`).  Joe's framing: the foreword
"is like a little eye-at-the-top-of-the-pyramid" — the operator-facing
synoptic surface that the multi-agent collaboration should reflect into.

Four framings of how the foreword annotations and this mission relate:

1. **The annotations are structurally bilateral-evidence rows at the
   prose layer.** Each `:foreword/coverage-check` annotation asserts
   "foreword claims X (foreword-passage endpoint) → anthology
   operationalises X via Y (target endpoint)."  Same shape as the 21
   entries in `vsatarcs-alignment-completeness.aif.edn`'s
   `:bilateral-evidence` vector.  Different layer (prose vs substrate),
   same coherence-check contract.

2. **`:foreword/gap-in-foreword` annotations are negative coherence
   claims** — the anthology has X but the foreword doesn't preview it.
   War Machine, Globes, Pillars, Sorry Topology all named.  Naturally
   typed as `:prototyping-forward` sorries in `futon2/data/sorrys.edn`.

3. **The 3 `:foreword/synthetic-question` annotations are test inputs
   for the unified-substrate query surface this mission wants to build.**
   "Where in VSATARCS does the 'eat your own tail' principle land
   operationally?" — if the unified-substrate query can answer in O(1)
   by pointing at futon6 + futon3, the surface works; if not, both the
   surface is incomplete AND the foreword has a coverage gap.

4. **The foreword IS the operator-facing summary of what this mission
   unifies substrate-side.**  When complete, a reader should get an
   O(1) entry to the apparatus that this mission makes O(1) navigable.
   The foreword's "tantalisingly close" → operational practice bridge
   IS the same bridge this mission builds substrate-side.

#### Concrete moves these framings open (deferred — capture, don't dive)

- **Ingest the foreword's 13 `:target` endpoints into the
  `:bilateral-evidence` vector** of
  `vsatarcs-alignment-completeness.aif.edn`.  The mission's eventual
  coherence-check then verifies those cross-references stay live as
  the anthology evolves.  Drift detection extends from
  substrate-substrate to prose-substrate.

- **Convert `:foreword/gap-in-foreword` annotations to typed sorries**
  in `futon2/data/sorrys.edn` with `:kind :prototyping-forward`.  The
  WM (when it reads sorries reflectively in M-war-machine-aif-last-mile's
  trajectory) surfaces these as next-priorities alongside other prose
  debt.

- **Use the synthetic questions as the first coherence-check fixtures**
  for the unified-substrate query surface.  Each becomes a programmatic
  assertion: `(query-unified-surface "eat-your-own-tail principle") →
  target-section-id futon6`.  If satisfied, surface works + synth
  question `:addressed`.  If not, both gaps surface.

- **Pair UI-QA-discharges with VSATARCS-reflection.** Each item claude-9
  closes on his frontend mission (e.g., R12 narrow-take-up apparatus
  becomes operator-legible in a view mode) is a fact this mission should
  ingest as new bilateral-evidence — and potentially as a new
  cross-reference target in the foreword's v3 annotation pass.

All four are deferred until the pairing is established and the
collaboration shape is clear.

### HEAD — 2026-05-21

- Operator-voice anchor preserved verbatim from emacs-repl dialogue.
- Working-economy position framed: interop underwrites unified-substrate query for the bilateral apparatus (currently artisanally maintained).
- Six clarity-gap tensions surfaced + carried forward to IDENTIFY.
- Five-bridge inventory drafted for IDENTIFY's first move (WM-trace-read, XTDB-clicks, intrinsic-values bidirectional, bilateral-evidence entries, mission-status references).
- Anti-glibness discipline names four traps explicitly (interop-as-euphemism-for-XTDB-write; Q6-scope-creep; live-verification-as-polling-noise; pre-empting-iq-009).
- Issue-queue entry iq-015 created in the same revision (v0.5.33) for queue-and-mission cross-pointing.
- No code shipped this checkpoint; substrate (v0.5.31 R12 ports, v0.5.32 issue-queue, v0.5.33 substrate-signal marginpars) is the inheritance from prior missions.

### Next-move

Open call. Two natural starting points:

- **DERIVE the cross-side coherence schema** — enumerate which facts must hold simultaneously; specify the verification cadence (revision-boundary vs. WM-trace-growth vs. on-demand); specify the violation-reporting surface (chrome block? marginpar overlay? new dedicated buffer?).
- **INSTANTIATE one bridge's coherence-check** — e.g., extend `arxana-vsatarcs-belief-compare`'s already-working pattern to the intrinsic-values bridge as a working second instance; let the schema crystallise from the second case.

Recommended: schema-first (DERIVE), since claude-4's already-shipped bilateral apparatus carries enough variation that the schema benefits from upfront enumeration rather than emerging-from-cases.

### Checkpoint — 2026-05-24 — QA batch 1 (View: stack)

First per-finding cycle executed under the procedural shape Joe set:

- **claude-9 landed Item D** (`Mode ?` → `Mode <inferred>` in HUD; renderer reads `[:judgement :mode]` with aif-stack fallback).  Three-way machine agreement: `wm-trace-2026-05-24.edn :mode` == `/api/alpha/war-machine judgement.mode` == HUD text == `multiplied`.  Verified across all 6 view-modes.
- **Joe visual-QA'd the "View: stack" page** and posted 13 findings (HUD opacity, Loop Health bars-as-mockup, consulting 0% confusion, recommendation-without-action, scan-window date math, EWW fallback, tickbox-sessions-without-affordance, etc.).
- **claude-10 split the batch.**  9 findings → anchored in `~/code/futon5a/data/wm-ui-anchors.edn` (new file, schema captured at file head) with each anchor pointing at the substantive VSATARCS / strategic-vocabulary content the UI element should reference.  4 findings → forwarded to claude-9 as `(UNDOCUMENTED) ISSUE:` items (EWW fallback, 14d window date bug, Play HH:MM indicator, tickbox-sessions + scan-window scrubbing).
- **Bell to claude-9** carried both batches + the 9 anchor IDs.  Procedural shape per anchor: claude-9 makes the UI element reference its anchor (link / tooltip / click-to-explain), at which point the anchor's `:status` flips to `:addressed` and the first concrete addressed case hardens into the bilateral-evidence-row template.
- **Discipline confirmed:** Joe writes closing prose for any foreword/anthology gaps the anchors surface; claude-10 anchors UI elements but does not author closing prose; claude-9 implements the UI cross-references and bells back when addressed.

Anchor file shape (`futon5a/data/wm-ui-anchors.edn`):

- Per anchor: `:id`, `:wm-ui-element {:view :location :text}`, `:anthology-anchor {:section-id :vocab-file :concept :passage-pattern}`, `:legibility-gap`, `:discharge-procedure`, `:severity`, `:status`, `:batch`.
- Highest-severity items this batch: anchor:0008 (Warnings cargo aphorism — `:critical`), anchor:0001 (consulting 0% confusion — `:high`), anchor:0002 + 0009 (Loop Health bars-as-mockup, Recommended Next Move dead-weight — `:high`).
- The file is append-friendly across batches; next QA batch will append `:batch :2026-05-24-batch-2` etc.

The next coherence-check fixture (per the per-item discipline) lands once the FIRST anchor flips to `:addressed` — the addressed case hardens the bilateral-evidence-row shape.  Until then, no premature schema work.
