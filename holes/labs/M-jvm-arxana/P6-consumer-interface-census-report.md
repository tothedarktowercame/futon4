# P6 report — adjacent consumer interface census

Mission: `M-jvm-arxana`. Phase: MAP. Packet: `packets/P6-consumer-interface-census.md`.
Discovery only: no interface is proposed here beyond what a mission, code file or
test already witnesses. No redesign of the REPL Circus, Ways of Working,
Giraffe or the War Machine, and no domain rules moved into Arxana.

## Question (Q6)

What minimum query inputs and outputs do the adjacent missions demonstrably
need from a shared Arxana authority?

## Consumers surveyed

| Consumer | Mission witness | Code/test witnesses |
|---|---|---|
| REPL Circus (驴/象/狗/馬) | `futon3c/holes/missions/M-repl-circus.md` | `futon1b/futon1b_server.clj` (hyperedge routes, `write-hyperedges-batch!` :222), `futon3c/src/futon3c/transport/http.clj` (驴 line), `futon3c/src/futon3c/peripheral/memory_recall.clj:175` (`:memory/assert` reads), `futon3c/src/futon3c/xiang/lu_rerank.clj` |
| Ways of Working | `futon3c/holes/missions/M-ways-of-working.md` | `futon3c/holes/excursions/ways-of-working.json`, `futon3c/scripts/ways-of-working-probe.py`, draw-pile / cascade-standards probe fixtures |
| Giraffe | `futon3c/holes/missions/M-giraffe.md` | `query-uncovered-components`, `trace-tension-path` (used from `futon3c/src/futon3c/portfolio/logic.clj`, `peripheral/mission_control_backend.clj`), cascade `construction.clj:125` (cited in mission) |
| War Machine | `futon3c/holes/missions/M-war-machine.md` | source-material table names `GET /api/alpha/evidence`, `GET /api/alpha/missions`, `futon5a/data/alignment.edn`; selection/action contracts via `futon2/holes/labs/wm-contract/E-outer-selection-landscape.md` and `flight_runner.clj` (cited in M-ways-of-working finding 2) |

## Interface observations (bounded: ≤3 reads, ≤3 writes per consumer)

### 1. REPL Circus

Reads (all witnessed in M-repl-circus.md "Goal" and its code list):

| # | Caller | Target identity | Inputs | Returned facts | Freshness | Witness |
|---|---|---|---|---|---|---|
| R1 | 驴 recall hook | pattern nodes in the GCC | operator turn text (post-hook) | pattern ids + scores | per turn, before the reply | M-repl-circus.md §Goal; `transport/http.clj` `Prompt: pattern ~…` line already delivered to the agent |
| R2 | 狗 recall hook | memories via `:memory/assert` hyperedges | agent reply context | memory ids + scores, each a one-call handle | per agent turn | `peripheral/memory_recall.clj:175` — a working reader today |
| R3 | 馬 / Giraffe | GCC neighbourhood of a node | node id (evidence-id, pattern id, mission id) | adjacent edges, one ring out | on demand; derived, rebuildable | M-repl-circus.md "spreading activation" para |

Writes:

| # | Caller | Target | Payload | Witness |
|---|---|---|---|---|
| W1 | 驴/象/狗/馬/Giraffe derived outputs | typed hyperedges in the GCC, endpoints are ids only | relation type + endpoint ids | M-repl-circus.md "Decided" — `write-hyperedges-batch!` via `/api/alpha/hyperedges` (`futon1b_server.clj:222`, batch route) |
| W2 | 驴/狗 metadata | metadata on the act they belong to (operator turn / agent reply evidence-id) | pattern ids / memory ids + scores | M-repl-circus.md "驴 and 狗 lines"; note: not yet true live (11-05 check found `pattern_id` empty) |
| W3 | scheduler/operational state | separate XTDB documents, not evidence | promise lifecycle, work queues | M-repl-circus.md "Decided" bullet 2 (obligations API named as reader) |

### 2. Ways of Working

| # | Kind | Caller | Inputs | Returned facts | Witness |
|---|---|---|---|---|---|
| R1 | read | probes (`ways-of-working-probe.py`, chained-claim sweep) | joins over turn/reply evidence records (session, timestamps, agent) | turn↔reply chains, bell returns, marks | M-ways-of-working.md HEAD and way table (feedback-reaches, intents-tracked probes); the join "runs over evidence parts 1 and 2" per M-repl-circus.md |
| R2 | read | draw-pile probe | commons-map placements keyed by item id | placement/owner of each work item | M-ways-of-working.md §"One connected graph…" (draw-pile named as reader of `~/notes/commons-map` TSVs) |
| R3 | read | cascade-standards probe (published panel) | mission-scope/pattern walk of the connected cascade | section content + age | M-ways-of-working.md §"Graph cache serves stale…" (walk ~172 s, 30-min age limit — freshness is an explicit requirement) |
| W1 | write | probe verdicts / way declarations | — | `ways-of-working.json` entries: status on/off/failing per way, with commit shas | `futon3c/holes/excursions/ways-of-working.json` (the file exists and is the record) |
| W2 | write | findings routed to owners | — | a finding item linked from the mission | M-ways-of-working.md §Scope ("each sent to its owner", "linked from here") |
| W3 | write | pattern-cascade snapshot publisher | — | published snapshot sections with staleness markers | `scripts/publish-cascade-snapshot.sh` (named in mission header) |

### 3. Giraffe

| # | Kind | Caller | Inputs | Returned facts | Witness |
|---|---|---|---|---|---|
| R1 | read | Giraffe traversal | tension node ids from 馬 | honestly-read tensions (resolved/re-raised/dropped) with quote-checked evidence | M-giraffe.md §"Giraffe in futonic logic"; `query-uncovered-components`, `trace-tension-path` named as the starting operations |
| R2 | read | G scorer | the work pool (M/E/T/A items, ~786 in the 10-25 field) | eligibility, wants with locators, dependencies | M-giraffe.md HEAD (measured counts) and stop-the-line findings 1–3 (read set frozen; all 171 tickets excluded; no dependency input) |
| R3 | read | Giraffe relationship review | pattern cascade (second derivative of 馬 tensions) | proposed tickets, relationship reviews | M-repl-circus.md Circus table, Giraffe row; M-giraffe.md overview draft |
| W1 | write | Giraffe ticket proposal | — | a ticket: attested want, its check (locator), traced dependencies | M-giraffe.md §"Giraffe is the traversal"; candidate-fields list in §Question |
| W2 | write | Giraffe / 馬 derived records | derived store keyed by act evidence-id | tension readings, cards | M-repl-circus.md Circus table (馬, Giraffe output rows) |
| W3 | write | application-order edges between missions | — | cross-mission ordering edge with per-edge evidence | M-giraffe.md §"Two ideas" — "the gap for the backlog is only the outer level: application-order edges between missions, with evidence" |

### 4. War Machine

| # | Kind | Caller | Inputs | Returned facts | Witness |
|---|---|---|---|---|---|
| R1 | read | war-room status synthesis | evidence landscape (whole store) | loop-health per arrow, last-seen timestamps | M-war-machine.md "What the War Machine produces" 1–2; source table `GET /api/alpha/evidence` |
| R2 | read | mission triage signal | mission inventory | active vs abandoned counts, inventory debt | M-war-machine.md item 4; `GET /api/alpha/missions` |
| R3 | read | sorry proximity | sorry topology (`futon5a/data/alignment.edn`) | closure trajectory per critical sorry | M-war-machine.md item 3 and source table |
| W1 | write | selector receipts / reading records | — | what was read and chosen per click (the frozen read set of finding 1 is the defect this write would prevent) | M-giraffe.md stop-the-line finding 1 (9 reading records, 10-16..10-18) |
| W2 | write | want verdicts at reviewed commit | — | wants re-read at the reviewed commit (Definition of Done) | M-giraffe.md Jira table, futon2 `368079c98` cited |
| W3 | write | occurrence squares / runtime certificates | — | `:wm/three-halves-square-v1` records with the IF/HOWEVER/BECAUSE/THEN shape | M-giraffe.md §"Two ideas" (ThreeHalvesBlend.lean runtime, occurrence squares) |

No more than three of each are listed per consumer even where more exist;
these are the ones with the strongest witnesses.

## Domain-specific fields that must stay namespaced outside the common waist

The "waist" here means the minimum shared query surface; these fields are
observed as domain-owned:

- Circus: pattern **activation scores** and recall reasons (驴/狗 lines) —
  owned by the recall machinery, not the authority (M-repl-circus.md).
- Ways of Working: **probe verdicts and status freshness windows** (30-min
  age limit, staleness markers) — probe semantics (ways-of-working.json).
- Giraffe: **want locators, dependency/readiness edges, application order
  with per-edge evidence** — work-pool semantics (M-giraffe.md).
- War Machine: **sorry affinity/trajectory scores, pocketwatch tick rates** —
  sorry-topology and HUD semantics (M-war-machine.md).

## Same shape, different names (recorded, not standardized)

1. **Act-attached pointers**: 驴's "pattern ids + scores on the operator turn"
   vs 狗's "memory ids + scores on the agent reply" — identical shape
   (references attached to an act as metadata), different node types and
   names (M-repl-circus.md, Circus table).
2. **Derived record keyed by act evidence-id**: 象's per-turn reading store vs
   馬's tension records vs Giraffe's relationship reviews — all "derived
   store keyed by the act's evidence-id", three names (M-repl-circus.md
   §Shape).
3. **Owner-linked finding**: Ways-of-Working "finding sent to its owner and
   linked from the mission" vs War Machine repair tickets with a root — both
   an item with a named owner and provenance (M-ways-of-working.md §Scope;
   M-giraffe.md HEAD ticket typing).
4. **Cross-item ordering edge**: Giraffe's "application-order edges between
   missions" vs WM cascade construction order (`construction.clj:125`) — same
   relational shape at different levels (M-giraffe.md §"Two ideas").

## Q6 ready / missing

| Item | Status |
|---|---|
| Consumer mission docs read (Circus, WoW, Giraffe, WM) | ready |
| ≤3 reads / ≤3 writes per consumer with witness | ready (tables above) |
| Code witness for the hyperedge write path | ready (`futon1b_server.clj:222`, batch route) |
| Code witness for a live hyperedge read consumer | ready (`memory_recall.clj:175`) |
| 驴/狗 metadata writes live in evidence | missing (11-05 check: `pattern_id` empty on operator turns — witnessed in M-repl-circus.md) |
| Cross-mission dependency/application-order edges | missing (M-giraffe.md finding 3: "no stage reads readiness") |
| Selector reading records written per click | missing (frozen read set, finding 1) |
| A single mission, code or test witness per proposed interface | ready for every row above; no row rests only on design conversation |
| Standardized common shapes for the four overlaps | intentionally missing (packet forbids standardizing yet) |

## Principal finding

The four consumers already agree, with witnesses, on one narrow waist —
id-addressed node/edge reads (patterns, memories, missions, tensions) plus
id-endpoint hyperedge writes — while each keeps scores, verdicts, locators
and ordering evidence as its own namespaced fields; the two shapes that
recur under different names (act-attached pointers, derived-record-keyed-by-
evidence-id) are recorded here but not merged.
