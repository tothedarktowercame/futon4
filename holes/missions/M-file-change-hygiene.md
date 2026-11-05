# M-file-change-hygiene — make changed files drive bounded upkeep

**Status:** INSTANTIATE — VERIFY preregistration accepted (2026-11-05)

Chartered from Joe's observation that Inbox Zero, warrant currentness, and
Giraffe task refresh share a `file changed` observation, followed by his ruling
that the existing design is DERIVE material and must not bypass a MAP of the
ways earlier Inbox Zero implementations failed.

## IDENTIFY

### Gap

The stack has several mechanisms that notice or react to changed repository
content, but it does not have one durable, content-identified observation that
can feed their distinct authorities. Inbox Zero has repeatedly been built and
activated without holding the intended clean-and-pushed condition. Test
warrants work better because currentness is checked at a handoff gate, while
Giraffe and WM readiness currently pay for refresh through separate paths.

The immediate temptation is to build a shared file-change ledger and attach
consumers. That design is plausible but not yet earned: Inbox Zero's history
contains working components, missing activation, wrong triggers, unowned
queues, incomplete attribution, false completeness, and automatic actions that
did not survive contact with the fleet. This mission must explain those
failures before selecting another mechanism.

### Motivation

Joe's desired working condition is not “a sweep can clean the repositories.” It
is that changed content creates the right bounded upkeep pressure without
requiring a person to rediscover the backlog. A Mission, Excursion, or Ticket
change is an ordinary content change with extra Giraffe hooks; a test-closure
change affects warrant currency; attributable dirty work advances Inbox Zero's
commit water clock. These should share observation without confusing their
remedies or authorities.

### Theoretical anchoring

- `futon3/library/hygiene/`: observe the authority, classify by remedy,
  exempt the in-use, receipt then gate, route to who can act, settle with
  meters.
- `test-registry/fill-forward-not-sweep`: candidate shared hygiene row for
  refreshing demanded work without inventing a completeness obligation.
- Futonic mission lifecycle: MAP is factual survey; DERIVE and ARGUE must not
  become retrospective justification for a design already selected.

### Scope

In scope:

- existing file-change observation, attribution, Inbox Zero, warrant, Giraffe,
  WM readiness, scheduling, health, and replay mechanisms;
- the historical failure record and the current live paths;
- boundaries among observation, priority authority, remedy, and actuation;
- a later multi-repo DERIVE, ARGUE, wiring diagram, and staged build.

Out of scope during MAP:

- implementing the proposed event ledger or consumers;
- enabling automatic commits, pushes, tests, or task contacts;
- treating every save as a logical change or every stale item as demanded work;
- replacing Giraffe's priority authority with a hygiene scheduler.

### Completion criteria

- [ ] Every materially distinct Inbox Zero implementation/activation since the
  Street Sweeper is inventoried with its trigger, authority, consumer,
  terminal action, meter, and observed failure.
- [ ] Current production paths are traced from write/change observation through
  durable state to action or terminal non-action, including disabled and
  host-specific branches.
- [ ] Existing warrant and Giraffe paths are mapped at the same resolution so
  the comparison is evidence-based rather than analogical.
- [ ] A failure taxonomy distinguishes missing observation, wrong authority,
  classification failure, attribution failure, trigger failure, queue without
  consumer, activation/configuration failure, actuation refusal, and missing
  settlement measurement.
- [ ] The ready-versus-missing table and all MAP questions have concrete
  answers with file, commit, log, or checkpoint evidence.
- [ ] DERIVE is revised where MAP contradicts its assumptions.
- [ ] ARGUE contains a failure-by-failure non-recurrence argument; unsupported
  claims cycle back to DERIVE.
- [ ] VERIFY includes adversarial fixtures reproducing each historical failure
  class and a required cross-repo wiring diagram.

### Relationships, repositories, and ownership

Repositories in scope: `futon0` (fleet gate and historical record), `futon3`
(hygiene patterns and earlier Inbox Zero engine), `futon3c` (watcher,
attribution, promotion, sweeper, test registry, Giraffe), `futon2` (WM ticket
readiness consumer), and `futon4` (this mission record and later operator
surface if needed).

This mission depends on the existing registries and watcher being inspectable;
it enables a reliable Inbox Zero facility, event-driven warrant invalidation,
Giraffe fill-forward refresh, and priority-correct WM readiness contact.

### Source material

- `futon0/README-inbox-zero.md`
- `futon3/library/hygiene/README.md` and its cited patterns
- `futon3/library/{inbox-zero,test-registry}/`
- `futon3c/src/futon3c/{watcher,inbox_zero,giraffe}/`
- `futon3c/src/futon3c/test_registry.clj`
- `futon2/src/futon2/aif/{ticket_readiness,full_loop_runtime,outer_observation}.clj`
- `~/water-clock.txt`
- `~/notes/wm-next/tests/RORDER-1-findings.md`
- `~/notes/file-change-hygiene-build-plan.md` (provisional DERIVE seed)

**IDENTIFY exit: Met.** Joe accepted the gap and the shared file-change shape,
then explicitly required a futonic MAP of prior failures before DERIVE.

## MAP

MAP is active. Findings below are an initial evidence index, not design
decisions.

### Survey questions

#### Existing mechanisms and authority

- [ ] **Q1.** Which processes currently observe repository changes, at what
  granularity, and which one—if any—has authoritative before/after content
  identity rather than path/mtime/status alone?
- [ ] **Q2.** What durable records exist for file occurrence, tool attribution,
  session/seat claims, repository state, commit containment, and consumer
  progress? Which survive restart and which are process-local?
- [ ] **Q3.** For each host and launcher, which Inbox Zero components are
  configured, loaded, scheduled, and enabled now? Can health distinguish off,
  idle, caught-up, stuck, and accumulating?
- [ ] **Q4.** Which mechanisms read Git's current/remote authority before
  deciding, and which use cached, unfetched, inferred, or path-list substitutes?

#### Historical failure analysis

- [ ] **Q5.** For every prior Inbox Zero generation, what was its complete
  producer → durable record → consumer → action → settlement path, and at which
  edge did observed cases stop?
- [ ] **Q6.** Which failures were code defects, which were absent wiring or
  default-off activation, which were missing consumers, and which were wrong
  policy even when the mechanism operated as designed?
- [ ] **Q7.** Which successful tests exercised only pure/local behavior while
  the live facility remained unwired, host-incomplete, or unable to see the
  dominant write source?
- [ ] **Q8.** Which proposals created human queues or reminders rather than
  terminal action? Measure production rate, consumption rate, age, decay, and
  eventual disposition where evidence exists.
- [ ] **Q9.** Where did an undifferentiated “dirty” class mix distinct remedies
  (unpushed commits, generated noise, finished work, live tracked output,
  deletions, ambiguous authorship), making safe automation impossible?
- [ ] **Q10.** Where did a local event boundary—especially turn end—exclude
  long-running turns, Codex, Emacs, subprocess output, non-agent writers, or
  repos whose activity never reaches that harness?
- [ ] **Q11.** Where did deduplication, retry, or scheduling manufacture floods,
  repeated no-op plans, or silence that was indistinguishable from success?
- [ ] **Q12.** Which mechanisms cleaned a snapshot but lacked a recurrence
  detector, owned consumer, or meter capable of proving the condition continued
  to hold?

#### Comparison systems

- [ ] **Q13.** Why do warrants currently work better? Trace mint, binding,
  handoff, check, stale classification, mandatory lanes, revalidation incident,
  and closure; identify remaining queue/coverage failures without idealising it.
- [ ] **Q14.** How does Giraffe detect new/changed M/E/T files today, invalidate
  readings, choose refresh scope, publish priority, and expose deferred work?
- [ ] **Q15.** What priority artifact can consumers legitimately read from
  Giraffe, and what decisions must remain local to Inbox Zero, the registry, and
  WM?

#### Counterfactual test of the DERIVE seed

- [ ] **Q16.** For each proposed FC-1–FC-8 slice, which historical failure could
  recur unchanged? Name the missing guard or revise/remove the slice.
- [ ] **Q17.** If the watcher stops, attribution disappears, a host omits an env
  var, a consumer cursor stalls, Git remote reads fail, or a task file changes
  170 siblings at once, what externally visible fact distinguishes failure from
  “nothing to do”?
- [ ] **Q18.** Can the proposed design operate without creating an operator
  queue? For every held item, name the actor, deadline, reroute, and terminal
  disposition.

### Initial failure ledger

These rows are hypotheses grounded in the cited checkpoints. MAP must expand
them into complete traces and verify the present status.

| generation / incident | observed result | candidate failure class | evidence |
|---|---|---|---|
| Street Sweeper, May 2026 | 2,551 proposed patches awaited operator review; sampled later, 0/60 still applied | human queue without consumer; proposal decay | `README-inbox-zero.md`, “Street Sweeper” |
| repeated manual sweeps | repositories became clean, then dirt returned | event rather than facility; recurrence unobserved | “Why fifty sweeps did not hold” |
| undifferentiated dirt | safe mechanical work and judgement-heavy files shared one pile | classification by instances/remedy absent | same section and 2026-08-28 checkpoint |
| `futon-sync` before repair | reported equality from stale refs; missed behind repos | wrong authority / no fetch | 2026-08-19 log and 2026-09-12 checkpoint |
| six-slice Inbox Zero rollout | code merged and tests passed, but zone launcher had no activation exports | implementation without deployment; host enumeration | 2026-08-24 log |
| half activation | watcher fallback path and witness producer env rules disagreed | asymmetric configuration; unattributed silence | 2026-08-24 log |
| turn-end promotion | excluded writers/turns outside the agent-chat boundary | wrong trigger / harness-local observation | 2026-09-16 repair 1 |
| initial activation | 3,263 no-op plans and 84 followups for one seat | unstable dedupe; diagnostic routed as action | 2026-08-26 checkpoint |
| attribution | dominant subprocess/Codex/Emacs writes lacked exact-success witnesses | observation/attribution coverage gap | 2026-08-26; current `inbox_zero/sweeper.clj` namespace contract |
| tracked live run products | cleaning was immediately undone by the running process | missing generated-role remedy; unreachable invariant | 2026-08-28 checkpoint |
| hourly gate | 248 consecutive failures accumulated in journald | receipt/log without owned consumer | 2026-09-12 checkpoint |
| durable Agency inbox | durable files outlived ledger epochs and accumulated without a consumer | durable producer mistaken for facility; incomplete census | 2026-11-04 checkpoint |
| current dirty-pressure lane | reports uncertain ownership and intentionally does not act on dirty files | visible pressure, not yet Inbox Zero actuation | `futon3c/inbox_zero/sweeper.clj` |

### Initial infrastructure inventory

| ready to investigate/use (no new code for MAP) | missing or not yet established |
|---|---|
| Hash-based multi-root watcher dispatches changed paths | one proven canonical content-change receipt contract |
| Git status/unpushed/behind/worktree readers in current sweeper | fleet-wide activation and consumer census |
| Exact-seat witness and claim records for some tool writes | measured coverage by writer kind and host |
| Turn-promotion modes, ledgers, dedupe and followup routing | proof that current consumers drain every produced class |
| Test registry closure hashes, currentness checks and incidents | measured revalidation scheduling/cost under backlog |
| Giraffe readers/pass/provenance/stale-marker namespaces | traced M/E/T change-to-new-pass latency and scope |
| Historical checkpoints with counts and concrete incidents | one normalized failure table with present dispositions |
| Hygiene pattern family | ARGUE mapping from every failure to a non-recurrence property |

### MAP surprises already affecting the seed design

1. Current `inbox_zero/sweeper.clj` states that tool Edit/Write witnesses saw
   only 2 of 238 dirty paths on 2026-09-17, because subprocesses produce most
   dirt. The DERIVE seed's watcher-as-occurrence/tool-as-attribution split is
   directionally compatible, but its expected attribution coverage must be
   measured rather than assumed.
2. The current sweeper already separates four remedy lanes and acts on
   unpushed commits, behind-only repos, and merged worktrees while keeping dirty
   files informational. The mission must preserve those working capabilities;
   it is not designing from an empty system.
3. A “proposal/reminder at ten changes” risks recreating the Street Sweeper's
   dead human queue. DERIVE must specify an owned consumer, deadline/reroute,
   and terminal outcome before retaining that behavior.
4. The lifecycle requires a wiring diagram because this mission defines a
   long-lived, cross-repo, multi-writer machine whose claims depend on its own
   receipts. This is a DERIVE/VERIFY obligation, not optional documentation.

### MAP findings and answers

#### Q1–Q4 — observation, records, activation, authority

There is no single current file-change authority. The multi-root watcher
maintains hash snapshots and dispatches changed paths; the sweeper separately
reads `git status`, upstream commits, behind-only state, and worktrees; tool
activity and Inbox Zero witnesses provide exact attribution for only a minority
of writes. The strongest occurrence observation is therefore filesystem/Git
state, while tool evidence is optional attribution. Mtime and invoke-window
overlap are explicitly diagnostic, not authorship
(`futon3c/inbox_zero/sweeper.clj:10-21`).

Durable artifacts already include watcher snapshots, Inbox Zero witnesses and
claims, promotion state, followup/escalation ledgers, Git commits, test-registry
evidence and subject incidents, and sealed Giraffe run directories. They do not
share one cursor/retention contract. The old promotion state is itself evidence
against casually adding another ledger: on 2026-11-05 it was 645,280,406 bytes,
last modified 2026-11-03, with 2,078 witness files under a 630 MB storage tree.

The live zone process supplies a decisive activation finding. On 2026-11-05:

```
FUTON3C_INBOX_ZERO_ENABLED=false
FUTON3C_INBOX_ZERO_PROMOTION=execute
FUTON3C_INBOX_ZERO_SWEEPER=true
FUTON3C_INBOX_ZERO_THRESHOLD=10
```

Thus the earlier attribution/promotion engine is disabled even though its mode
says execute; the independent sweeper is active every 30 minutes. The sync
timer unit exists but is disabled. “Configured,” “loaded,” “enabled,” “making
progress,” and “caught up” are not one state and must never share one healthy
value.

The current mechanical lanes do read current Git authority. The historical
`futon-sync` failure came from comparing without fetching; the repaired path
fetches. Warrant currency rehashes the declared source/test/load closure and
classifies changed paths as uncommitted or committed
(`test_registry.clj:612-626,1074-1123`). Giraffe recomputes its complete adapter
input and compares its hash with the newest verified run, rather than trusting
path notifications (`giraffe/run.clj:415-454`).

#### Q5–Q12 — why the prior Inbox Zero generations failed

The failures are not one missing feature:

1. **Street Sweeper:** a correct patch producer terminated in an operator
   approval queue. Production continued, proposals decayed, and 0/60 sampled
   packets remained applicable. The missing edge was consumer-to-terminal-act.
2. **Manual sweeps:** each altered one snapshot but installed no recurrence
   detector. A clean tree and an unswept tree were observationally identical.
3. **Undifferentiated dirt:** unpushed commits, generated noise, finished work,
   live tracked output, and ambiguous dirt require different remedies. One safe
   policy could not consume the mixed pile.
4. **Wrong authority:** unfetched refs reported equality while repositories
   were behind.
5. **Implementation without activation:** six tested slices were absent from
   the zone launcher. Another half-activation gave watcher and producer
   different witness-path rules.
6. **Wrong trigger:** turn-end promotion covered one harness, not long turns,
   Codex, Emacs, subprocess output, or other writers.
7. **Coverage mistaken for attribution:** tool witnesses observed 2 of 238
   dirty paths in the measured case; most dirt came from launched processes.
8. **No-op flood:** repeated turn-end checks, unstable keys, and diagnostic
   plans produced 3,263 plans and 84 followups for one seat.
9. **Unreachable cleanliness:** tracked files continuously rewritten by live
   processes became dirty again immediately after commit.
10. **Receipt without consumer:** 248 hourly failures accumulated in journald;
    the later Agency incident likewise accumulated durable files across ledger
    epochs without an owned consumer.
11. **Human reminder as terminal:** items routed to people without bounded
    ownership, deadline, or reroute became a second inbox.
12. **Tests below the live seam:** pure planning, attribution, and dedupe tests
    passed while activation, host configuration, source coverage, and consumer
    liveness remained untested.

The current system is not universally inert. In the two hours inspected on
2026-11-05, the sweeper repeatedly scanned 19 repos, pushed 1–28 commits per
repo in successful passes, ran sync, and attempted worktree retirement. At the
same time it reported approximately 900 dirty paths in futon2, 709 in futon3c,
and 39 in apm-lean as unknown-ownership pressure. Its contract intentionally
separates four remedies: dirty files INFORM; unpushed commits ACT; safe
behind-only repos ACT; merged worktrees ACT
(`inbox_zero/sweeper.clj:23-40`). That is a working partial facility and the
starting point, not something to replace wholesale.

#### Q13 — why warrants work better, and their limit

Warrants are demand-coupled. The author mints a run for committed bounded
scope; the handoff carries it; the reviewer checks actual current hashes,
closure, environment, log, and result parse before deciding whether to rerun.
Failure opens a bounded lane whose successful rerun becomes the next warrant.
This supplies an immediate consumer and reward: the reviewer avoids duplicated
execution only when the receipt remains current.

The registry is deliberately fill-forward, not a scanner. A changed file does
not justify sweeping every warrant. Currentness is decided at lookup/handoff or
for a named revalidation incident. Therefore a global event-driven warrant
revalidation queue in the original DERIVE seed was over-unification. A change
event may accelerate or explain a check, but the existing check remains the
authority and demand boundary.

#### Q14–Q15 — Giraffe change and priority

Giraffe currently loads the open M/E/T population through canonical registry
readers, computes a complete input, runs a deterministic pass, seals its files,
and verifies runs by manifest hashes. New or changed readings are represented
as `:unread-new` or `:reading-source-changed`; changed task bytes alter the
adapter input hash. The WM freshness gate compares a freshly recomputed input
with the newest verified field run.

There is no existing incremental “file changed → affected neighbourhood”
engine that MAP could simply wire. The first implementation should therefore
use file change to schedule/debounce the existing complete Giraffe refresh,
not invent incremental graph invalidation. One batch handles 170 changed
tickets once. The sealed pass remains priority authority. Consumers may read
its order, status, cone, unblocks, rank, and focus relation; they may not ask
Giraffe whether a dirty file is safe to commit or whether a mandatory test lane
may be skipped.

#### Q16–Q18 — counterfactual against the original eight slices

The original plan changes as follows:

- **FC-1/FC-2:** retain a small content-identity reducer, but do not create an
  unbounded second event archive. Persist a compact latest-path index plus a
  bounded append journal with explicit retention and per-consumer cursors.
- **FC-3:** retain domain classification; consume canonical registry readers
  and generated-role declarations, never filename resemblance.
- **FC-4:** replace proposed incremental Giraffe neighbourhood invalidation
  with debounced scheduling of the existing full, sealed Giraffe run. Measure
  batch latency and deferred demand before considering incremental work.
- **FC-5:** remove proactive warrant revalidation as a first build. Join file
  changes only when a named handoff/check/incident demands currentness.
- **FC-6:** a ten-file threshold is not a terminal reminder. For attributable
  in-flight work it is pressure visible to the responsible seat; handoff is the
  logical commit boundary. Unknown ownership remains explicitly unresolved and
  cannot count as achieved Inbox Zero.
- **FC-7:** retain Giraffe-prioritized WM contact, but consume the same sealed
  pass/focus pins as selection. It is a Giraffe projection, not a new hygiene
  priority algorithm.
- **FC-8:** promote this from the final slice to a cross-cutting condition:
  every consumer is born with cursor lag, oldest-unconsumed age, success/failure
  outcome, and a stopped-consumer adversarial test.

The design cannot promise that no human ever judges dirt. It can promise not to
manufacture an unowned human queue: every requested judgement has a named
actor, a reason it cannot be automated, a deadline/reroute, and a terminal
state; unknown ownership remains repo pressure, not a fictional assignment.
This is narrower than “automatic Inbox Zero,” and honest.

### MAP ready versus missing — final

| ready, preserve/reuse | missing, actual work |
|---|---|
| Current sweeper's remedy split and mechanical push/sync/worktree lanes | content-identity change seam shared with domain hooks |
| Multi-root hash watcher | bounded journal, retention, cursor and replay contract |
| Canonical registry readers for M/E/T | debounced task-change trigger into existing Giraffe run |
| Sealed Giraffe pass and complete-input freshness check | shared pass/focus pin supplied to WM readiness ordering |
| Registry closure/currentness checks and incident semantics | optional changed-path explanation at an existing demand boundary |
| Git status and fetched remote authority | logical-batch/handoff water-level projection |
| Uncertain-ownership pressure feed | bounded ownership/judgement lifecycle without false attribution |
| Historical meters and current pass summaries | unified health distinction: off/idle/caught-up/stuck/backlogged |

**MAP exit: Met.** All questions have concrete answers sufficient to revise the
design. The live census prevents three false premises: promotion is not enabled,
the current sweeper is already partially effective, and neither a new global
ledger nor proactive warrant sweep is justified.

## DERIVE

### Entities and relations

1. **Change observation:** canonical repo/worktree/path, operation, before/after
   content identity, Git state, observed-at, and optional attribution receipt.
2. **Latest-path projection:** one current row per worktree/path; repeated saves
   replace provisional identity rather than increasing a counter.
3. **Bounded change journal:** short replay window, retention watermark, and
   per-consumer cursor. It is a delivery hint, never correctness authority;
   every consumer can rebuild from Git, registry rehash, or full Giraffe input.
4. **Domain notice:** a typed projection such as task-definition changed,
   warrant-scope candidate, or Inbox Zero dirty-set delta.
5. **Consumer outcome:** consumed, deferred with demand/reason, failed, or
   terminally not applicable; always tied to input identity.

Relations are binary where possible: observation updates path projection;
notice derives from observation; consumer outcome consumes notice. A logical
work batch may relate several path identities, one seat/session, and one
handoff, so that is the one genuinely n-ary record.

### Invariants

- **O1 Occurrence before attribution:** filesystem/content observation is
  valid without an author; attribution can enrich but never fabricate it.
- **O2 One identity, one level unit:** repeated writes producing the same
  current path identity do not refill the water clock.
- **O3 Authority remains local:** Git decides repository state, registry check
  decides warrant currency, sealed Giraffe pass decides task priority.
- **O4 Persist before notify:** a consumer is never the sole holder of an event;
  level-triggered reconciliation against its authority repairs missed events.
- **O5 Every producer has an owned consumer or no durable output.** Cursor lag
  and oldest-unconsumed age are health, not optional diagnostics. Red beyond a
  bound creates ordinary scheduled work through Giraffe, with closure criteria.
- **O6 Bounded retention and rebuild:** journal size is bounded. A dead cursor
  is dropped after its declared bound, the drop is recorded/red, and revival
  rebuilds from authority rather than blocking retention.
- **O7 Remedy classification precedes actuation:** no generic dirty-file action.
- **O8 Settlement, not save, is the automatic commit boundary:** ten distinct
  dirty paths create pressure, not permission to commit. Interactive work
  settles at handoff; subprocess output settles when its launching job ends or
  at a declared generated-role lifecycle boundary.
- **O9 Demand before refresh:** warrants refresh at handoff/incident; Giraffe
  refreshes after debounced task-change demand; unrelated stale inventories are
  not swept for completeness.
- **O10 Disabled is a first-class state:** a switched-off capability declares
  its re-arm condition and cannot report healthy/caught-up.
- **O11 Stable gain:** measured production must not indefinitely exceed
  demonstrated drain. The loop throttles its producer, applies a safe automatic
  remedy, or schedules capacity/classification work before the pile is unbounded.
- **O12 Unknown is bounded work:** unknown ownership has count and oldest-age
  bounds; classification runs as yield-measured work packages, not one human pile.
- **O13 Holds expire:** every explicit hold has a re-ask/reroute deadline.
- **O14 Ended ownership transfers:** when a seat or job ends, unresolved pressure
  moves within a bound to a named orphan/classification lane; it is not stranded.

### Data flow and slices

1. Add the pure observation/projector contract and bounded journal beside the
   existing watcher. Shadow mode only; periodically reconstruct projections
   from authority and compare them with the event-derived view.
2. Attach per-consumer cursors and health before adding a consumer.
3. Classify M/E/T through canonical readers and debounce changes into one
   existing full Giraffe run, with a maximum-wait deadline under continuous
   change; publish its sealed pass pin and measure full-run cost.
4. Make WM readiness consume the same pass/focus context as selection and
   intersect focus/associated with stale assessments.
5. At warrant check/handoff, use named changed paths to explain scope drift;
   do not enumerate/revalidate all warrants.
6. Project distinct dirty path identities into per-worktree/logical-batch water
   levels. Handoff may run the existing promotion gates; threshold only routes
   owned pressure. Preserve current automatic push/sync/worktree lanes.
7. Expose consumer lag, outcome, residual unknown ownership count/age and
   production/drain rates, action counts, and storage size on health/operator
   surfaces. Declare this health loop itself as a status-known way of working;
   sustained red becomes a prioritised work item rather than passive display.

### IF / HOWEVER / THEN / BECAUSE decisions

- **IF** all three domains react to changed content, **HOWEVER** they have
  different authorities, **THEN** share observation and typed projection but
  not one action queue, **BECAUSE** priority cannot decide safety or currency.
- **IF** an append log enables replay, **HOWEVER** the prior state reached 645
  MB and stopped advancing, **THEN** bound retention and install consumer
  cursors before domain consumers, **BECAUSE** durability without drainage is
  the recorded failure.
- **IF** incremental Giraffe invalidation sounds efficient, **HOWEVER** no such
  verified engine exists and the full input/run is already sealed, **THEN**
  debounce into the existing complete run first, **BECAUSE** correctness and
  batch amortisation are already available.
- **IF** ten dirty paths warrant attention, **HOWEVER** count does not prove
  ownership or commit coherence, **THEN** treat ten as pressure and handoff as
  the commit boundary, **BECAUSE** a threshold is not authority to stage bytes.
- **IF** changed paths can stale warrants, **HOWEVER** the registry is
  constitutionally fill-forward, **THEN** evaluate them only at a named demand
  boundary, **BECAUSE** proactive completeness destroys the warrant economy.
- **IF** events give low-latency routing, **HOWEVER** any watcher, journal, or
  cursor can miss work, **THEN** reconcile periodically from the domain
  authority, **BECAUSE** correctness must be level-triggered and recoverable.
- **IF** red health makes failure visible, **HOWEVER** prior red logs accumulated
  without action, **THEN** red past its bound creates scheduled Giraffe-visible
  work, **BECAUSE** a display is not a consumer.
- **IF** classification can explain dirt, **HOWEVER** some tracked output should
  never have been produced, **THEN** meter classifier yield and prefer stopping
  recurrent dirt at its source, **BECAUSE** a downstream queue can perpetuate waste.

### PSR — Pattern Selection Records

#### PSR-1: `hygiene/receipt-then-gate`

- Pattern chosen: hygiene/receipt-then-gate
- Candidates: direct callback, periodic rescan, durable unbounded queue
- Rationale: Persisted identity plus a consumer cursor permits replay without
  treating notification delivery as completion.
- Confidence: high, conditional on bounded retention and liveness meters.

#### PSR-2: `test-registry/fill-forward-not-sweep`

- Pattern chosen: test-registry/fill-forward-not-sweep
- Candidates: refresh every stale item, round-robin backlog, demand-triggered refresh
- Rationale: The 170-ticket incident and warrant economy both show that stale
  inventory is not itself a mandate to pay every refresh cost.
- Confidence: high for warrants/WM; Giraffe initially batches a complete run
  because that is its existing verified unit.

#### PSR-3: `hygiene/classify-by-remedy-judgement`

- Pattern chosen: hygiene/classify-by-remedy-judgement
- Candidates: universal dirty queue, extension rules, per-domain classification
- Rationale: Fifty sweeps failed because one pile mixed mechanical and
  judgement-bearing work. The current sweeper's four lanes are evidence this
  split works.
- Confidence: high.

#### PSR-4: level-triggered reconciliation (candidate library row)

- Pattern chosen: periodic authority reconciliation plus event hints
- Candidates: event-only watcher, unbounded replay, periodic full polling only
- Rationale: Events give promptness, while Git/registry/Giraffe reconstruction
  makes truncation, missed notification, and dead-consumer recovery correct.
- Confidence: high; no existing named hygiene row was found, so this is a
  candidate rather than a claim about the present library.

#### PSR-5: stop dirt at source (candidate library row)

- Pattern chosen: remove recurrent tracked-output producers before expanding
  classification capacity
- Candidates: classify forever, ignore generated paths, automatic staging
- Rationale: leaked fixtures and live tracked output are producer defects; the
  cheapest correctly classified item is the one no longer emitted.
- Confidence: high for recurrent generated paths, conditional elsewhere.

**DERIVE exit: Met.** The implementation boundary, authorities, invariants,
data flow, and principal trade-offs are specified. MAP caused substantive
revision: no proactive warrant sweep, no first-pass incremental Giraffe engine,
no unbounded ledger, and no automatic commit at the ten-path threshold.

## ARGUE

### Failure-by-failure non-recurrence case

| prior failure | preventing DERIVE invariant | adversarial VERIFY test | live meter | residual risk |
|---|---|---|---|---|
| Street Sweeper dead approval queue | O5/O11/O13; ownership plus stable gain and expiring holds | consumer slower than sustained producer | production/drain rate, lag, oldest age | safe automation may not exist; throttle or schedule bounded classification work |
| snapshot sweep recurs silently | O5 plus periodic/content-triggered observation | stop watcher while files change | watcher heartbeat, projection age | filesystem blind spot outside watched roots |
| mixed dirt cannot act safely | O7 and preserved remedy lanes | generated/live/finished/unpushed fixture mix | counts/outcomes per lane | new class remains unknown until classified |
| stale refs report clean | O3 Git authority | remote advances while local ref stays cached | fetch outcome and remote-read age | remote unavailable; must report unknown, not clean |
| code merged but host disabled | O10 | omit enablement on one host | enabled/off/re-arm state per host | configuration inventory can omit a host; fleet census must expose it |
| watcher/producer half activation | O5 and shared contract | mismatched paths/config | producer and consumer identities/cursors | schema migration mismatch; version gate required |
| turn-end misses other writers | O1/O8 filesystem occurrence plus job settlement | subprocess writes tracked bytes then exits without handoff | observations by source; unsettled jobs | writes outside roots remain unseen |
| exact-attribution blind spot | O1, unknown remains unknown | change with no tool receipt | attributed/unattributed ratio | unknown dirt still needs judgement; mission does not claim otherwise |
| 3,263 no-op flood | O2 plus outcome dedupe | repeated saves and repeated no-op ticks | unique identities, suppressed duplicates | rapidly changing hashes can still create pressure; debounce/window meter |
| live tracked output never settles | O7/O8 plus stop-at-source PSR | process rewrites after proposed commit | recurrent-path and role counts; removals at source | missing role declaration holds action visibly |
| 248 red logs with no consumer | O5; bounded red becomes scheduled work | health remains red past bound with no actor | red age, created work, closure outcome | scheduler itself can fail; its health is status-known and reconciled |
| 645 MB stale durable state | O6 | stalled cursor under sustained writes, then consumer revival | bytes, watermark, cursor drops/rebuilds | authority rebuild can be expensive; meter it |
| mass task changes tax WM clicks | O9 plus Giraffe authority/max wait | 170 unrelated plus continuous changes | full-run cost, debounce age, deferred/contact counts | full run may still be expensive; measure before incremental redesign |
| warrant completeness sweep | O9 and O3 | many stale warrants, one demanded handoff | checks/reruns saved and paid | unseen bad behaviour requires an incident; registry never claimed surveillance |
| unknown ownership becomes Joe's queue | O12/O14 | seat ends leaving 1,600 unattributed paths | unknown count/age, production/drain, classifier yield | judgement remains; packages are bounded and prioritised |

### Pattern cross-reference

- `hygiene/observe-the-authority`: O1/O3 distinguish occurrence, attribution,
  Git state, warrant currency, and task priority rather than promoting one
  convenient proxy.
- `hygiene/classify-by-remedy-judgement`: O7 preserves and extends the current
  four-lane success rather than recreating the mixed pile.
- `hygiene/exempt-the-in-use`: provisional current identities are measured but
  handoff/settlement, not a save, opens actuation.
- `hygiene/receipt-then-gate`: O4/O5 make durable receipt and consumer progress
  separate proof obligations.
- `hygiene/route-to-who-can-act`: threshold pressure names a responsible seat
  only with evidence and carries deadline/reroute; unknown remains repo-wide.
- `hygiene/settle-with-meters`: liveness, lag, storage, cost, residual, and
  action outcomes are born with the loop, not added after failure.
- `test-registry/fill-forward-not-sweep`: O9 prevents stale inventory from
  becoming an unbounded work mandate.
- Candidate `level-triggered reconciliation`: O4/O6 make the bounded journal an
  acceleration hint and make the authorities the recovery path.
- Candidate `stop dirt at source`: recurrent generated dirt is repaired at its
  writer before investment in downstream classification.

The numbered rulings most directly supporting the design are
`@why war-room/wr-4-inhabit-before-building`, because the current partial
facility must be inhabited before another apparatus is built;
`@why war-room/wr-26-a-capability-switched-off-carries-its-re-arm-condition-in-writing-at-the-switch`,
because disabled promotion currently coexists with execute configuration; and
`@why war-room/wr-27-a-loop-is-born-instrumented-for-its-gain`, because this
loop has repeatedly failed through absent consumer/cost evidence.

### Why this is not the same failure again

The proposal is not “put all changed files in a better queue.” It begins by
preserving the mechanisms that already act safely, then supplies a common
content observation only where an existing authority has a named demand. It
refuses three seductive claims: that tool activity proves authorship, that ten
files form a commit, and that stale inventory must be swept. Most importantly,
consumer health and bounded retention precede new durable output. Crucially,
red is not the terminal state: after its bound it creates ordinary prioritised
work, while periodic authority reconciliation detects a broken health path.

This does not solve judgement by automation. Unknown-ownership dirty files
remain visible pressure until a justified classifier or responsible actor can
dispose them. Unlike the former pile, their count and age may not grow without
bound: work is packaged, yield is measured, dead-seat ownership transfers, and
recurrent dirt is stopped at source where possible.

### Trade-offs and generalisation

We give up a single universal scheduler, immediate incremental Giraffe updates,
and the attractive promise that crossing ten files automatically restores
cleanliness. In return we reuse verified authorities, batch mass changes,
avoid proactive warrant costs, and make residual work measurable. The same
shape generalises to other upkeep loops when they share an occurrence but not
an action authority: share the observation, retain local remedy judgement, and
require every durable producer to prove drainage. Stability is a rate claim,
not merely an ownership claim: production must remain within demonstrated drain
or trigger throttling, safe automation, or scheduled capacity work.

### Plain-language argument

We have repeatedly built cleanup machinery that could notice or propose work
but could not prove anyone finished it. This design starts from the parts that
already act safely and gives them a shared signal when file contents change.
Each specialist still decides its own question: Giraffe orders tasks, the test
registry checks tests, and Inbox Zero handles repository state. If a consumer
stops, the backlog becomes scheduled work after a bounded red period; if event
delivery fails, reconciliation with the real authority reconstructs the gap.

### Friendly-review disposition

claude-16 returned **revise**, not ceremonial approval. The revision adds the
missing stability condition (production versus drain), red-to-scheduled-work,
authority reconstruction after journal loss, cursor expiry/rebuild, a maximum
debounce wait, bounded unknown-ownership work, expiring holds, subprocess/job
settlement, and dead-seat transfer. It also records two contrary candidate
patterns: level-triggered reconciliation and stopping dirt at source.

**ARGUE exit: Met.** The revised argument maps every recorded failure to an
invariant, adversarial test, live meter, and residual risk, and incorporates the
friendly review's objections rather than treating visible red as completion.

## VERIFY

VERIFY is a preregistration of what an implementation must make true, not a
claim that the runtime adapter already exists. Joe ruled on 2026-11-05 that the
adapter is properly part of INSTANTIATE and that valid runtime behaviour can be
DOCUMENTed after it exists.

### VERIFY discovery 2026-11-05 — adapting the WM handoff index

The newer non-R-node component Joe recalled exists as
`futon2.aif.handoff-site-index`, backed by
`futon2/resources/wm/handoff-sites.edn`. It is an executable inventory of
producer/consumer sites. Its audit refuses duplicate ids, missing source files
or stable anchors, incomplete contract/transport/status classification,
unresolved cross-repo locations, malformed retention declarations, and agent
reply sites without an explicit representation and decoder. Its own tests also
search for shared handoff callers that escaped the index.

It is a genuine reuse candidate for the **process** BOM row, but not a complete
VERIFY oracle. It proves that an authored seam still exists where declared and
has a named representation contract. It does not prove that the consumer
drains at least as quickly as the producer, that red creates scheduled work,
that a cursor expires, or that a rebuilt projection equals its authority.
Those remain behavioral spike obligations rather than claims the index makes.

The spike declaration at
`holes/labs/M-file-change-hygiene/handoff-sites-spike.edn` reuses the WM schema
without modifying production code. It indexes four already-existing authority
seams: Git status into Inbox Zero, unknown-pressure publication, warrant
closure rehash, and complete Giraffe-input freshness. Running the real WM audit
against the futon3c checkout verifies all four; replacing one stable anchor by
an absent anchor makes the same audit refuse with `:anchor-missing`.

The adaptation suggests two VERIFY layers:

1. Extend the index only when the new observer/projector, bounded journal,
   red-to-work, and job-settlement call sites exist. Require every cross-process
   record to declare producer, consumer, contract, transport, retention, and
   recovery authority. This is forward prototyping, not INSTANTIATE approval.
2. Keep the behavioral obligations outside the structural index: adversarial
   rate, stalled-consumer, cursor-expiry/rebuild, continuous-debounce,
   subprocess-settlement, and sustained-red tests named above.

**VERIFY discovery result:** component found and structurally adaptable. Four
existing seams pass its real audit; it reduces wiring risk but cannot discharge
the loop invariants before a targeted behavioral prototype exists.

### VERIFY correction — the Lean-backed delivery layer

Joe recalled a stronger layer correctly. The closest exact artifact is
`mathlib4/DarkTower/WarMachine/VerifiedAnswerDelivery.lean` (introduced by
mathlib4 `f681ddd342`, 2026-10-29), with a related refusal-preservation model in
`SeamRefusal.lean`. `VerifiedAnswerDelivery.Evidence` carries six checks:
identity/review, source digest, function shape, exact hole coverage, operational
maps, and agreement of the JSON claim. Its central theorem is
`delivered_iff_checked`: `delivered e = true` exactly when the five operational
checks are true. `missing_hole_refused` makes incomplete coverage impossible to
deliver, and `claim_does_not_supply_answers` prevents a JSON assertion from
substituting for the reviewed executable function. The module compiles at the
current mathlib4 HEAD.

This corrects, but does not erase, the limitation above. The production
open-cascade boundary implements the corresponding checks and has a frozen
end-to-end Clojure witness in
`futon2.aif.wm.verified-answer-delivery-test`. However, the generic
`handoff-site-index` contains no Lean theorem/module field, does not invoke a
generated checker, and does not translate an indexed row into
`VerifiedAnswerDelivery.Evidence`. The Lean theorem therefore proves that a
fumbled delivery is impossible **for values admitted by that model**; today the
bridge from every indexed runtime handoff to such an admitted value is authored
and tested, not mechanically enforced by the index.

For this mission the reusable proof architecture is now sharper:

1. The handoff index supplies the complete seam census and source conformance.
2. A generalized Lean admission predicate supplies “accepted implies every
   required check”, with file-change-specific checks for identity, authority
   reconstruction, consumer ownership, settlement boundary, retention/rebuild,
   and red-to-work routing.
3. A small Clojure adapter must construct exactly that evidence record from the
   real boundary result, and a conformance test must prove every indexed
   file-change seam names such an adapter. Without step 3, citing the theorem
   would repeat the written-but-not-wired failure.

**Lean-layer finding:** the proof-backed precedent exists and is stronger than
the Clojure census, but it is specialised to ordinary held-cascade answer
delivery and is not presently a generic backing layer of `handoff-site-index`.
Adapting the composition is a legitimate VERIFY spike; claiming the composition
already exists would be false.

### VERIFY spike 1 — file-change admission theorem

`mathlib4/DarkTower/WarMachine/FileChangeHygieneDelivery.lean` now prototypes
the formal half of the dual argument. Its `Evidence` record names nine boundary
checks derived directly from ARGUE/DERIVE: identity, authority reconstruction,
owned consumer, bounded drainage, settlement, retention-or-rebuild,
red-to-work, expiring holds, and ended-owner transfer.

The proved theorem `admitted_iff_checked` makes admission equivalent to all nine
checks. Separate refusal theorems cover the highest-risk recurrence paths:
missing authority reconstruction, unnamed consumer, unstable drainage,
unsettled handoff, red without scheduled work, and ended ownership without
transfer. The module compiles with no `sorry` at the current mathlib4 HEAD.

The theorem is intentionally conditional. It establishes that a fumbled
handoff cannot be *admitted in the model*; it does not self-certify that runtime
facts were measured. The next VERIFY spike is therefore the Clojure adapter and
conformance test that construct this exact evidence from an indexed seam. Until
that bridge exists, the formal half is coherent but not yet operationally
binding.

### Specification bill of materials

| aspect | VERIFY formalism and level | disposition |
|---|---|---|
| claim | ARGUE failure table plus IF/HOWEVER/THEN/BECAUSE decisions | specified; each non-recurrence claim has a falsifier and meter |
| data / artifact | `FileChangeHygieneDelivery.Evidence` and `admitted_iff_checked` | proved at the admission-shape level; runtime evidence adapter is INSTANTIATE |
| process | WM `handoff-site-index` schema and four-seam spike | mechanically audited; full cross-repo exotype is priced below |
| loop | production/drain, lag, red age, cursor rebuild, and settlement meters | preregistered; empirical admissibility requires the instantiated loop |
| decision | DERIVE decisions and PSR-1 through PSR-5 | specified, including two contrary candidate patterns |

### Priced VERIFY deferrals

- **Full exotype diagram:** the mission triggers the diagram requirement, but a
  conformance-kept diagram cannot honestly name the adapter call sites before
  they exist. Revisit immediately after the first adapter lands; it blocks
  adding a second consumer, not starting the first adapter.
- **Adversarial runtime fixtures:** preregistered cases are watcher/consumer
  stopped, host activation absent, attribution absent, duplicate saves, live
  tracked output, stale remote authority, dead-seat routing, stalled cursor,
  subprocess output without handoff, continuously starved debounce, sustained
  red without a consumer, authority rebuild, and mass task invalidation. They
  land with the adapter and block enabling each corresponding production path.
- **Full Giraffe cost:** measure before choosing debounce and maximum-wait
  values; no production scheduler may be enabled with guessed values.

These are priced rather than silently waived: each has a named revisit trigger
and blocks expansion or activation if unpaid.

### Completion-criteria pre-check

The MAP inventory, live-path tracing, comparison with warrant/Giraffe
authorities, failure taxonomy, ready/missing table, DERIVE revision, and ARGUE
non-recurrence case are present. The original criterion requiring all
adversarial fixtures *during VERIFY* is superseded by Joe's phase ruling above:
VERIFY preregisters the fixtures and admission theorem; INSTANTIATE supplies the
adapter and fixtures before activation. No completion claim is weakened—the
same tests remain gates on the resulting behavior.

### VERIFY decision log

1. Reuse the WM handoff index as the structural seam census.
2. Reuse `VerifiedAnswerDelivery` as the proof-architecture precedent, not as a
   falsely generic runtime bridge.
3. Accept the no-`sorry` file-change admission theorem as the formal
   preregistration.
4. Assign construction of runtime evidence and theorem-to-seam conformance to
   INSTANTIATE.
5. Defer description of observed valid behavior to DOCUMENT, after it exists.

**VERIFY exit: Met.** The architecture has been checked against the existing WM
handoff census and Lean delivery precedent; the file-change admission contract
is proved; risks that require runtime facts are explicitly preregistered with
blocking triggers. Joe accepted this as sufficient VERIFY and assigned the
adapter to INSTANTIATE.

## INSTANTIATE

The first slice is the proof-carrying boundary, not the whole hygiene loop:

1. Define the Clojure representation corresponding exactly to the nine Lean
   evidence fields.
2. Adapt one existing indexed seam end to end and refuse admission when any
   field is absent or false.
3. Extend the handoff index row with the evidence adapter and Lean contract
   identity; add conformance sensitivity for a missing or drifted adapter.
4. Run the preregistered adversarial fixtures for that seam before enabling it.
5. Draw the full cross-repo exotype before adding a second consumer.

### INSTANTIATE checkpoint 1 — one proof-carrying seam

The first slice is live, tested, and committed without enabling a background
observer. `futon3c` commit `5a5ce9bab` adds the runtime evidence adapter and
binds it to `GET /api/alpha/test-registry/currency`. The response carries the
nine checks, their stated basis, the Lean contract identity, and a computed
admission bit; callers cannot provide that bit. Missing, false, or non-Boolean
checks fail closed. A current result whose entry does not equal the requested
entry is also refused.

`futon2` commit `1abe685d1` adds the cross-repository seam to the executable WM
handoff index. Index audit now requires the adapter symbol and Lean contract as
a pair and refuses an invalid symbol, an invalid theorem name, or either field
missing. This closes the particular written-but-not-wired gap identified in
VERIFY: the structural census now names both runtime construction and formal
admission.

The focused gates passed:

- runtime adapter: 3 tests, 36 assertions;
- HTTP boundary: 10 tests, 63 assertions;
- handoff index and mutation sensitivity: 8 tests, 53 assertions;
- changed-file `clj-kondo`: no errors (three pre-existing unresolved-var
  warnings in the large transport namespace, none in the new adapter);
- Lean module: 0 errors, no `sorry`.

The preregistered exotype revisit is paid by
`holes/labs/M-file-change-hygiene/file-change-wiring.edn`. Diagramprover reports
no written-never-read, read-never-written, or multiply-written vertices. The
map distinguishes `:live` from `:planned` boxes: registry authority, evidence
construction, formal admission, and the HTTP consumer are live; observation,
bounded journal, health projection, and red-to-Giraffe routing are not.

**INSTANTIATE exit: In progress.** The first boundary and its full diagram are
stable checkpoints. The exotype prevents an invalid completion claim: the
shared observer/projector, bounded replay/rebuild behavior, and red-to-work loop
still require construction and the remaining preregistered fixtures.

### INSTANTIATE checkpoint 2 — bounded replay contract

`futon3c` commit `357d638a3` adds a pure shadow-mode journal beside the existing
watcher. It maintains one latest level per repository/worktree/path, suppresses
identical content saves, assigns monotone sequence identities, and bounds its
append window independently of consumer progress. A cursor overtaken by the
retention watermark is marked rebuild-required; it cannot acknowledge its way
past the loss, and it resumes only with an explicit authority-rebuild receipt.
Consumers whose finite progress deadline expires are dropped rather than
pinning storage, with a typed receipt naming authority reconstruction as the
recovery path.

Four focused tests (16 assertions) cover repeated-save deduplication, a stalled
cursor under retention pressure, explicit rebuild, deadline expiry/drop, and
monotone bounded acknowledgement. `clj-kondo` reports no errors or warnings.
The exotype marks this box `:prototype`, not `:live`: no serving watcher was
reloaded and no production dispatch has been attached yet.

### INSTANTIATE checkpoint 3 — level-triggered observer

`futon3c` commit `b54b08c3f` connects the bounded journal to the existing
multi-watcher in shadow mode. The journal does not rely on notification
delivery: every watcher cycle reconciles the complete path→content-hash
snapshot against its prior authority level. The first read seeds without
emitting a historical change storm; later reads append only changed and
deleted levels. Thus an event omitted from the direct notification path is
recovered on the next authority comparison.

The serving watcher has not been reloaded and no consumer dispatch is enabled.
The source default creates a 2,048-entry bounded journal, exposes its health in
watcher status, and isolates reconciliation failures from the existing ingest
loop. This advances the observer and journal boxes to `:prototype` while
preserving the exotype's refusal to call them live.

The preregistered missed-event fixture is now executable: seed `h1`, omit any
event call, reconcile an authority snapshot containing `h2`, and observe one
`h2` write plus a contemporaneous deletion. Repeating the same authority level
appends nothing. Focused gates passed: journal 5 tests/22 assertions; watcher
30 tests/103 assertions; changed-file `clj-kondo` clean; check-parens clean.

#### Checkpoint 3 correction after friendly review

Claude-16 found that checkpoint 3 overstated the walk as an authority. The
watcher journal is process-local; after downtime it starts empty and its first
walk seeds quietly, so bytes changed while the watcher was down cannot be
reconstructed from that journal. The same walk also drives ordinary watcher
dispatch, so the original fixture demonstrated the pure reconciliation
function, not a distinct live notification failure.

`futon3c` commit `30266bc81` corrects the contract. The snapshot is now named a
*walk level*, never authority. Every new consumer starts `rebuild-required` and
must provide a fresh Git/registry/Giraffe authority receipt before reading
hints. More changes in one walk than the journal bound collapse into one
level-jump hint and mark consumers for rebuild rather than filling the window
with a useless tail. Reconciliation refusals remain in health; a watcher
running across a hot reload without the new state reports `not-armed` plus the
restart condition.

The restart fixture now discards all journal state, changes and deletes files
during downtime, and proves that the new consumer cannot proceed before its
authority rebuild. A burst-over-bound fixture proves one level jump and rebuild
pressure. Corrected gates: journal 8 tests/32 assertions; watcher 31 tests/104
assertions; clj-kondo and check-parens clean. No serving reload occurred.

### INSTANTIATE checkpoint 4 — demand-bound authority projector

`futon3c` commit `b56db09df` adds the pure demand projector without enabling a
runtime consumer. It treats journal entries only as wake-up hints:

- M/E/T definition paths accumulate one complete Giraffe-input recomputation,
  debounced for batching but forced by a maximum-wait deadline under continuous
  change;
- test-registry currency requests are emitted only for an explicitly named
  active demand whose declared paths intersect the changed paths;
- a level jump requests full Giraffe recomputation and may recheck already
  named warrant demands, but never enumerates the warrant registry.

The projected requests contain no currency verdict. Git, the test registry,
and the sealed Giraffe pass remain the authorities that answer them. Four tests
(14 assertions) cover absence of warrant sweeping, named-path intersection,
continuous-change maximum wait, level-jump rebuilding, and ordinary code
changes not waking Giraffe. clj-kondo and check-parens are clean. The exotype
marks the projector `:prototype`; dispatch remains disabled.

#### Checkpoint 4 correction after friendly review

Claude-6 found two composition defects and one census drift. A rebuild formerly
advanced its cursor to the tail *after* the authority read, losing hints that
arrived during a long Giraffe pass. Rebuild is now two-phase: `begin-rebuild`
pins sequence S, the authority is read, and completion acknowledges only
through S. A rebuild receipt also projects the same full-authority demand as a
level jump; it cannot disappear behind the rebuilt cursor. Finally, M/E/T path
recognition now calls futon2's public mission-registry census predicate rather
than maintaining a permissive regex that admitted nested run snapshots.

The shared census predicate landed as futon2 commit `aeb774dbe`. Its focused
tests pass 28 tests/106 assertions. The composed futon3c tests prove both the
change-during-rebuild race and overflow→rebuild→demand behavior.

### INSTANTIATE checkpoint 5 — first runtime shadow consumer

`futon3c` commit `5e647f108` attaches one in-memory `:giraffe-shadow` consumer
to the watcher cycle. It registers with cursor and rebuild discipline, reads
the newest verified sealed Giraffe run only to establish its authority basis,
feeds bounded hints to the pure projector, and retains bounded demand/outcome
health. It explicitly reports `:dispatch-enabled? false`: it does not run
Giraffe, query the test registry, dispatch work, or reload the serving JVM.

The runtime parameters use a 30-second debounce and five-minute maximum wait,
longer than the observed roughly 160-second Giraffe pass. Hot-reloaded watcher
state without the new atom reports `not-armed` with a restart condition. The
focused suite passes 49 tests/170 assertions across journal, projector, shadow
consumer, and watcher; clj-kondo and check-parens are clean. Adversarial tests
cover restart/unavailable authority, overflow rebuild, a change arriving
during an authority read, continuous change reaching maximum wait, and nested
snapshot paths not waking Giraffe.

### INSTANTIATE checkpoint 6 — durable shadow observations and scoring

Claude-6's checkpoint-5 review found that the shadow call could still abort the
watcher cycle, and that `newest-field-run` was only a historical pass receipt,
not evidence that current Giraffe inputs had been read. `futon3c` commit
`4c1d28fdd` repairs both before adding measurement.

The watcher now isolates every shadow exception and retains a typed refusal in
health; a forced projector failure proves that later cycle work still runs.
Rebuild authority recomputes the canonical missions input and records
`fresh?`, computed-input SHA, sealed-run SHA, and run id. Because a cold
recomputation exceeded 30 seconds in direct measurement, it runs as one
single-flight future: the watcher never waits, and rebuild/demand state remains
pending until the result is available.

Each due Giraffe demand is then fsync-appended to the ignored runtime ledger
`data/file-change-hygiene/giraffe-shadow-demands.jsonl`, including emission
time, trigger basis, changed paths, and the current-versus-sealed authority
reading. Persistence happens before projector acknowledgement; a failed append
leaves the demand pending for retry. The offline scorer reads this ledger and
verified sealed Giraffe run history, classifying changed intervals without a
demand as misses and unchanged intervals with a demand as spurious wakes.

The focused gate passes 74 tests/273 assertions across journal, projector,
shadow consumer, durable ledger/scorer, watcher, and Giraffe run logic;
clj-kondo and check-parens are clean. The serving JVM was not reloaded and the
shadow still performs no Giraffe run, registry query, work dispatch, or click
change.

#### Checkpoint 6 correction after friendly review

Claude-6 demonstrated that the first freshness implementation rebuilt a plain
missions adapter while live clicks mint `missions-excursions` with a readings
corpus. It therefore reported false staleness even when the click's own probe
was green. The run-history scorer was also a poor judge of spurious wakes:
Giraffe normally mints only after staleness, leaving almost no unchanged run
intervals, and emission time misattributed debounced changes.

`futon3c` commit `c05206144` makes the mint self-describing. New manifests
record source, repositories, edges, readings root, selected ids, and retained-
focus option. Both `-main` and freshness call one `build-adapter-result` path;
freshness replays the recorded invocation and refuses to guess for legacy runs.
Tests prove that the combined invocation supplies its reading batches and that
a manifest invocation round-trips to a fresh result.

The asynchronous probe now retains a timestamped result so rebuild and a due
sink can share it. A later demand may reuse it only when the result completed
after the demand's trigger; otherwise one new single-flight probe starts.
Pending is a normal `:held :giraffe-freshness-pending` outcome, not refusal
noise, and absence of a probe never falls back to a synchronous watcher read.
The ledger path is environment-overridable.

Scoring is now preregistered against per-click freshness verdicts rather than
run-to-run differences. It attributes a demand by `first-at-ms`, not delayed
emission: the fixture fresh→stale→fresh with a demand triggered before but
emitted after the stale probe produces zero misses and zero spurious wakes.
Runtime collection of the click verdict history remains the next adapter; the
pure scorer does not claim observations that have not been collected.

Corrected gates pass 77 tests/285 assertions, including the composed
rebuild/due fixture; clj-kondo and check-parens are clean. No serving reload
occurred.
