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
