# Mission: WebArxana UI Improvements

**Date:** 2026-05-30
**Status:** HEAD complete; IDENTIFY complete 2026-05-30; MAP pass complete
enough for DERIVE; DERIVE drafted 2026-05-30
**Owner:** futon4 (Arxana browser + WebArxana web surfaces + data layer),
  with ingress from futon3c (REPL / Evidence Store, Mission structure) and
  the substrate-2 / mission projection.
**Provenance:** HEAD authored 2026-05-30 via a simulated `eoi new`
  (`eoi-mission-head` flash — Right-View-only, the lightest HEAD scaffold;
  round-collapse false). Joe answered in his own register; the engine
  (claude-7, `emacs-claude-repl`) ordered the answers and supplied section
  labels and one orienting italic line per section. Substantive sentences
  below are Joe's verbatim words.
**Schema ref:** `atthangika-buckets.json` →
  `eoi_instances/web-arxana-ui-improvements-HEAD-2026-05-30`;
  institution-object `:node/web-arxana-live-interface` (self-authored,
  constituted).
**Cross-ref:**
  - `~/code/futon4/holes/missions/M-interest-network-coupling.md` — the
    Interest Network visualiser this mission proposes to rescale / demote
    to a filter.
  - `~/code/futon3/holes/missions/M-self-documenting-stack.md` — the LC1
    mission-search UI this mission proposes to make more usable.
  - `~/code/futon4/README-essays.md` — Arxana Essays ingest convention
    (the surface a finished WebArxana view should cohere with).

## HEAD

*The mission's live operator-shape, captured before IDENTIFY hardens it into a tractable gap statement.*

## Right View — the invariant

*The one structural property the finished work is checked against.*

> WebArxana provides a coherent visual interface to live graph-structured information about the stack, both for reading and writing

This would cover the rectification of the UI deficiencies (which are not coherent) as well as the REPL integration (currently non-extant, so the viewer is not truly "live").

## What's already felt to be true

*The current state of the two extensions and the shape of the wished-for graph view, in Joe's words.*

We have built WebArxana with two extensions: Mission Search — e.g. `http://localhost:3100/wa#/mission-search/a%20sorry%20enterprise` — and an Interest Network visualiser `http://localhost:3100/wa#/interest-network` — HOWEVER both of these are deficient in various ways. The Interest Network shows items which are too small to view or navigate clearly, and the mission search is highly cluttered in the out-of-the-box view (although we did add a Confidence slider that allows the user to have some control over how many items are brought into the visualisation). Neither of them is well-integrated with the Graph view which is the central and essential feature of WebArxana, and the Graph view itself is not well integrated with running REPL sessions or missions.

What I'd like would be to have a graph view integrated directly with the running REPL so that we can see, for example, a mission starting to fill in as we move from HEAD to IDENTIFY and so on, and as new patterns are activated in the REPL or in the Mission doc as PSRs.

As for the Interest Network, while it's not a problem to have it viewable in a separate pane as a "feature", it needs to be at a good scale and resolution so that the contents can be read and used, and I somewhat question if it should be a "feature" or possibly a "filter" on the standard Graph view.

Much the same can be said for the Mission Search — it works, but it needs to be made more usable and useful. For that, possibly we'd want some other search approaches, e.g., "recent missions" (search by date) — rather than just searching by name.

Something that we could look into as a warm-up exercise for this mission is creating a 2D visualisation of the joint Mission + Pattern projection as a stand-alone Meaning Map of what's in the Futon stack. If we have such an embedding-based map in mind, the hypergraph-based visualisation might be easier to understand and think about.

## Clarity-gap — what isn't yet clear

*The soft spot Joe wants IDENTIFY to chew on first.*

There's something I'm a bit bemused about which is that I have mentioned `https://github.com/org-roam/org-roam-server/issues/35` in several turns/missions as a potential way to do the Emacs integration but we haven't made that live yet. I'd like IDENTIFY to do a bit of digging around that feature. This isn't just about nagging "why hasn't this landed yet" but about developing some new thinking about what the "live" contract actually looks like (indeed!) — with the further implication that we have to bridge between the Evidence Store (REPL interactions) and Mission structure, which initially doesn't "look like" it's part of Web Arxana at all!

## The "live" contract (Emacs ↔ browser, both directions)

*First `address-now` commitment — what "live" has to mean to be useful.*

I know from earlier experience with Org Roam that it's possible to keep Emacs and the web browser in sync, the question is not just "how do we do that here" but how do we *usefully* do that — this means understanding what information needs to be live-visualised (from Emacs to Firefox) and also understanding how clicking around in the WebArxana nodes can start Emacs-based editing processes.

## Integration is the proof-point, not UI polish

*Second `address-now` commitment — what this mission is really proving out.*

We have laid the groundwork for importing Missions and REPL turns into XTDB, but we haven't integrated them yet — so this mission isn't just about UI polish but about proving out *how* that integration can be done. There are a few bits of prior art like "affective events" that we might want to draw on (regarding mining from REPL turns), and thinking around PSR/PUR in Missions. While limited extension work is needed, the main proof-point will be in the integration.

## Carried-forward tensions

*Named here so IDENTIFY can pick them up without pretending they are already settled. Items marked `address-now` carry Joe's verbatim content in the sections above; the rest are deferred with their disposition.*

- **T1 — the "live" contract** (`address-now`; content in §"The 'live' contract"). IDENTIFY digs into `org-roam/org-roam-server#35` to develop new thinking about what "live" usefully means, both directions (Emacs→Firefox visualisation; WebArxana-node→Emacs editing).
- **T2 — Evidence Store ↔ Mission structure bridge** (`address-now`; content in §"Integration is the proof-point"). Groundwork for importing Missions and REPL turns into XTDB is laid but not integrated; the integration is the proof-point. Prior art: "affective events" (mining REPL turns), PSR/PUR in Missions.
- **T3 — Interest Network scale + feature-vs-filter** (`decision-debt`, deferred to IDENTIFY/DERIVE). Items are too small to read/navigate; needs good scale and resolution; open decision whether it stays a "feature" or becomes a "filter" on the standard Graph view.
- **T4 — Mission Search usability + search approaches** (deferred to IDENTIFY/DERIVE). Works but needs to be more usable and useful; candidate approaches beyond name search, e.g. "recent missions" (search by date).
- **T5 — Meaning Map warm-up** (warm-up exercise; non-blocking). A 2D visualisation of the joint Mission + Pattern projection as a stand-alone embedding-based Meaning Map, on the hypothesis that having it in mind makes the hypergraph visualisation easier to understand and think about.
- **T6 — HEAD slots not yet filled** (carried). The Right-View-only flash did not collect explicit anti-glibness or working-economy answers; IDENTIFY surfaces these rather than the HEAD fabricating them.

## Phase outline (post-HEAD)

*Engine-supplied scaffold per `holes/mission-lifecycle.md`. The substantive desiderata in the sections above are Joe's verbatim words; the phase structure here is connective tissue to be filled by later phases.*

### IDENTIFY

- Name the gap precisely against the Right-View invariant: where the current Graph / Interest Network / Mission Search surfaces fail "coherent" and "live."
- Dig into `org-roam/org-roam-server#35` (T1): what it demonstrates about Emacs↔browser sync, and which direction is the harder half here.
- Scope in/out the Evidence Store ↔ Mission structure bridge (T2) as the load-bearing integration, distinct from extension/polish work.
- Surface the deferred HEAD slots (T6: anti-glibness, working-economy).
- Shape-first option: treat "coherent visual interface to live graph-structured information" as a candidate invariant and ask "what shape is this one instance of?"

### MAP

- Inventory existing infrastructure: the Graph view, Mission Search (`/wa#/mission-search/…`), Interest Network (`/wa#/interest-network`), the Confidence slider, the XTDB import groundwork for Missions + REPL turns, "affective events," PSR/PUR extraction.
- Inventory existing data: what mission/REPL-turn/pattern entities already land in XTDB; what the Interest Network and Mission Search already query.
- Ready vs missing table for the live-coupling and the embedding map.

### DERIVE

- Specify the "live" contract: what is live-visualised Emacs→Firefox, and how a click on a WebArxana node initiates an Emacs-side editing process.
- Specify the Evidence-Store↔Mission projection and how a mission "fills in" HEAD→IDENTIFY→… and lights up patterns/PSRs as they activate.
- Decide Interest Network feature-vs-filter (T3) and Mission Search approaches incl. recent-missions/date (T4); record IF/HOWEVER/THEN/BECAUSE.

### ARGUE / VERIFY / INSTANTIATE / DOCUMENT

- Standard lifecycle. Loop-closure target: a mission visibly filling in, live, as the REPL moves through phases and activates patterns.

### Warm-up exercise (T5; can run alongside IDENTIFY)

- Build a 2D visualisation of the joint Mission + Pattern projection as a stand-alone Meaning Map of what's in the Futon stack, on the hypothesis that an embedding-based map in mind makes the hypergraph visualisation easier to understand and think about.

## 1. IDENTIFY — 2026-05-30 lifecycle checkpoint

### Motivation

The current WebArxana surface already has three pieces that ought to be
one coherent live graph interface:

| Surface | Current role | Gap against the Right-View invariant |
|---|---|---|
| Graph view | Central WebArxana graph canvas, backed by futon1a entities / relations / hyperedges | It is a graph reader/editor for Arxana entities, but it does not yet project live REPL turns, mission phase state, PSRs, PURs, or active mission transitions. |
| Mission Search | Search route at `/wa#/mission-search/...`, backed by the futon3a mission embedding search | It is useful for finding missions, but starts as a separate search-and-results surface, with clutter controlled only partly by confidence / top-k filtering. Its results are not first-class graph filters or graph pins. |
| Interest Network | Separate route at `/wa#/interest-network`, backed by the interest-network posterior projection | It renders the corpus-as-interest-network, but its node scale and readability are still weak, and it is still separate from the standard graph interaction model. |

The discrepancy is therefore not simply "UI polish." The actual gap is:

> **Sorry-WEBARXANA-LIVE-COHERENCE:** WebArxana has graph, mission-search,
> and interest-network surfaces, and futon3c has live REPL / evidence traces,
> but there is no single live graph contract binding them. A mission can move
> from HEAD to IDENTIFY in Emacs without the browser showing that state change
> as a graph event, and a browser node can be clicked without a principled
> mission-aware Emacs edit action being selected.

### The org-roam-server#35 finding

`org-roam/org-roam-server#35` is useful prior art, but not as a turnkey
implementation. The issue demonstrates two directions that matter here:

- Browser-to-Emacs: clicking a node was expected to open the corresponding
  Emacs buffer via `org-protocol`, but on Windows it instead opened a new
  frame and treated the protocol URI as a browser/application path.
- Emacs-to-browser: the reporter notes that org-roam-server's Database and
  Buffer views followed Emacs buffers.

For this mission, the lesson is that "live" must not mean "throw a protocol
URI over the wall." The live contract needs typed intent, acknowledgement, and
failure visibility. A browser click must say what kind of edit/read action is
being requested, against which entity, in which mission/session context; Emacs
must either perform it or return a structured refusal. Conversely, Emacs /
REPL activity must emit graph deltas that the browser can subscribe to or
poll, not merely depend on a browser reload.

Reference inspected: https://github.com/org-roam/org-roam-server/issues/35

### Theoretical anchoring

- **Live-sync source truth** (`futon3/library/exotic/live-sync-source-truth.flexiarg`): liveness must have an explicit source of truth. Here, durable state lives in XTDB / evidence / mission records; browser state is a projection, not the authority.
- **Deep storage to active graph** (`futon3/library/hdm/deep-storage-to-active-graph.flexiarg`): the value of storage is realized when durable records become active graph affordances. Evidence entries and mission phase records should become graph nodes / edges visible during work.
- **State-snapshot witness** (`futon3/library/invariant-coherence/state-snapshot-witness.flexiarg`): not every useful live view is an event stream. Mission phase and REPL session status also need periodic or on-save state snapshots so the browser can ask "what is true now?"
- **Present graph topology, not adjacency lists** (`futon3/library/system-coherence/present-graph-topology-not-adjacency-lists.flexiarg`): the UI should make topology legible: mission phase, evidence chain, pattern activation, and interest-network edges must be seen as graph structure, not just side panels.

### Scope in

- Define a bidirectional live contract between Emacs / REPL and WebArxana:
  Emacs-to-browser graph deltas, browser-to-Emacs typed open/edit intents,
  acknowledgement, and failure states.
- Define the Evidence Store <-> Mission structure bridge for the first live
  projection: a mission phase timeline that can visibly fill in from HEAD to
  IDENTIFY to later phases, with REPL turns and PSR/PUR events attached.
- Rework WebArxana navigation so Mission Search and Interest Network can act
  as graph filters or graph entry points, not only separate feature pages.
- Improve baseline usability for Mission Search and Interest Network where it
  blocks the integration proof: readable scale, clearer initial density, and
  recent/date mission search if the current embedding-only route cannot answer
  the operator's actual lookup need.
- Warm-up, if useful: build a standalone Mission + Pattern Meaning Map as a
  projection spike, but treat it as risk reduction for the main graph contract,
  not as an alternate final surface.

### Scope out

- A full replacement of WebArxana's graph renderer.
- Rewriting futon3c evidence emission. The evidence boundary already exists;
  this mission should consume it or add typed projections, not bypass it.
- Treating browser local UI state as canonical mission state.
- Inferring mission phase transitions solely from prose when an authored or
  machine-readable mission state record can be made available.
- Making Interest Network or Mission Search "prettier" without connecting them
  to the central graph contract.

### Completion criteria

1. A WebArxana route or graph mode can show a concrete mission as a phase
   graph with at least HEAD and IDENTIFY nodes, backed by durable mission or
   evidence data rather than hardcoded demo data.
2. A live or refreshable REPL / evidence thread is attached to that mission
   graph, including at least one user turn, one assistant turn, and one
   evidence id / session id visible or inspectable.
3. Browser-to-Emacs action works for at least one mission node and one
   evidence/session node through a typed `/api/emacs/open` request or successor
   contract, with a visible failure state if Emacs refuses or cannot resolve it.
4. Mission Search can send selected mission results into the central graph
   view as pins, filters, or a graph projection. It must also support at least
   one non-name discovery mode, with "recent missions" as the current leading
   candidate.
5. Interest Network is either integrated as a standard graph filter/projection
   or explicitly retained as a separate feature with a documented reason. In
   either case, node labels must be readable enough for operator navigation in
   the default viewport.
6. The mission doc contains a reproducible end-to-end demo: start from a
   mission in Emacs/REPL, observe it in WebArxana, click from WebArxana back
   into Emacs, and verify the relevant evidence/mission records.

### Relationship to other missions

| Mission / document | Relationship |
|---|---|
| `M-interest-network-coupling.md` | Supplies the current Interest Network projection and event-log discipline; this mission decides whether that projection becomes a graph filter, graph mode, or separate view. |
| `futon7/holes/M-self-documenting-stack.md` | Provides the broader LC1 / self-documenting-stack pressure that Mission Search serves. The path in the HEAD cross-ref was `futon3`, but the file currently lives under `futon7`. |
| `futon3c/holes/specs/repl.spec.edn` | Defines the REPL as READ/EVAL/PRINT/LOOP trace with `data/repl-traces/*.edn`; this mission consumes those traces as live graph material. |
| `futon3c/README-evidence.md` | Documents the evidence entry shape and HTTP API that WebArxana can query or project from. |
| `futon4/docs/tickets/021-emacs-futon-sync-flow.md` | Existing documentation ticket for Emacs -> Futon sync, relevant to the data-flow part of this mission. |
| `futon4/docs/tickets/016-browser-visit-dispatch.md` | Existing browser visit-dispatch doc ticket, relevant to typed browser-to-Emacs edit/open actions. |

### Source material

- `futon4/dev/web/webarxana/src/webarxana/client/core.cljs`
- `futon4/dev/web/webarxana/src/webarxana/client/route.cljs`
- `futon4/dev/web/webarxana/src/webarxana/client/graph.cljs`
- `futon4/dev/web/webarxana/src/webarxana/client/mission_search.cljs`
- `futon4/dev/web/webarxana/src/webarxana/client/interest_network.cljs`
- `futon4/dev/web/webarxana/src/webarxana/client/api.cljs`
- `futon4/dev/web/webarxana/src/webarxana/server/core.clj`
- `futon4/dev/web/webarxana/src/webarxana/server/emacs.clj`
- `futon4/dev/web/webarxana/src/webarxana/server/mission_search.clj`
- `futon4/dev/web/webarxana/src/webarxana/server/interest_network.clj`
- `futon3c/README-evidence.md`
- `futon3c/holes/specs/repl.spec.edn`
- `futon3c/data/repl-traces/*.edn`

### Owner and dependencies

Owner remains futon4 for WebArxana and the browser/server contract. futon3c
is a dependency for evidence and REPL trace shape. futon1a / XTDB is a
dependency for durable entity/evidence reads. futon3/futon7 mission and
pattern records are dependencies for the mission/pattern projection.

### Anti-glibness discipline

The mission is glib if it lands only a better-looking force graph or another
standalone page. Every design and implementation step must preserve the
operator-facing proof point: a durable mission/evidence/pattern state change
appears in WebArxana as graph structure, and a WebArxana gesture can return
to the correct Emacs action without protocol ambiguity.

### Working-economy position

This mission underwrites the stack's working memory. It turns REPL work,
mission lifecycle state, and pattern discipline into a shared visual surface
that Joe can read and act through while work is happening. It is underwritten
by the evidence boundary, mission lifecycle standard, XTDB-backed graph model,
and current WebArxana graph/editor scaffolding.

### Shape-first IDENTIFY

Candidate invariant:

> A live graph workbench must expose the same substrate event as a readable
> graph fact and as an actionable editing affordance.

This is not one special case. Adopt the shape namespace
`live-graph-workbench/*` for the sibling invariants:

| Instance | Meaning |
|---|---|
| `live-graph-workbench/mission-phase` | A mission lifecycle transition is graph-visible and edit-addressable. |
| `live-graph-workbench/repl-turn` | A REPL turn or trace frame is graph-visible and session-addressable. |
| `live-graph-workbench/pattern-activation` | A PSR/PUR/pattern use is graph-visible and pattern-addressable. |
| `live-graph-workbench/projection-filter` | A derived projection such as Mission Search or Interest Network can become a graph filter or entry point without becoming a disconnected UI island. |

This shape keeps the mission from special-casing "make Mission Search nicer"
or "make Interest Network bigger." Those are instances of the same live
workbench invariant.

**IDENTIFY exit status:** complete enough for MAP. Human review is still
required before DERIVE freezes the contract.

## 2. MAP — started 2026-05-30

### Inventory: existing infrastructure

| Infrastructure | Concrete finding |
|---|---|
| WebArxana SPA routing | `client/route.cljs` supports `#/mission-search/<query>`, `#/interest-network`, and graph hashes for pins/focus/type. The route model already has a place for feature pages and graph state, but not for "search result becomes graph filter." |
| Graph view | `client/graph.cljs` renders local Datascript nemas/links from futon1a API reads, supports pins, focus, double-click essay expansion, and some Emacs opening for essay sections. |
| Browser-to-Emacs open path | `client/api.cljs` calls `/api/emacs/open`; `server/emacs.clj` accepts `arxana://`, `docbook://`, absolute paths, and `~/` paths, then shells out to `emacsclient`. This is a real bridge, but it is location-string based rather than a typed action contract. |
| Server routes | `server/core.clj` exposes `/api/emacs/open`, `/api/mission-search`, `/api/mission-search/event`, `/api/interest-network`, `/api/ws`, and `/api/futon/*path`. |
| Mission Search | `client/mission_search.cljs` has query, confidence slider, top-k, agreement-only checkbox, result cards, and a force graph. `server/mission_search.clj` calls `futon.missions/search-missions` using futon3a embeddings and records consumer click events. |
| Interest Network | `client/interest_network.cljs` fetches `/api/interest-network` and renders a force graph with standing colors and completeness 3-vector. `server/interest_network.clj` merges a disk surface, XTDB `interest-event` entities from futon1a, and bucket/strawman coverage. |
| Evidence Store | `futon3c/README-evidence.md` documents `/api/alpha/evidence`, evidence entry shape, session/thread chaining, and laptop-to-server replication. |
| REPL trace | `futon3c/holes/specs/repl.spec.edn` defines READ/EVAL/PRINT/LOOP turn records and canonical `futon3c/data/repl-traces/<run-id>.edn` frames. |
| WebSocket | `server/ws.clj` currently relays presence and arbitrary client messages between WebArxana clients. It is not yet a subscription to evidence or mission graph deltas. |

### Ready vs missing

| Ready: no new code needed for first design pass | Missing: actual work |
|---|---|
| Hash routes for graph, mission search, and interest-network pages. | A graph-mode route or state shape for mission/evidence projections. |
| `/api/emacs/open` bridge to Emacs via `emacsclient`. | Typed browser-to-Emacs action envelope with entity id, action kind, mission/session context, acknowledgement, and refusal reasons. |
| Mission Search endpoint and result click telemetry. | Mission Search result -> central graph pin/filter/projection. |
| Interest Network projection endpoint. | Readable default layout and decision whether it is a graph filter, graph mode, or justified standalone surface. |
| Evidence HTTP API and evidence entry shape. | WebArxana server/client code that queries evidence by mission/session and converts entries to graph nodes/edges. |
| REPL trace frame shape and verifier. | Import/projection path from `data/repl-traces/*.edn` or evidence entries into WebArxana graph state. |
| Existing graph renderer and local Datascript cache. | A normalized WebArxana projection format for mission phase nodes, evidence nodes, and pattern nodes that can coexist with existing nemas/links. |

### MAP questions now open

1. Where is the most canonical machine-readable mission phase state today:
   mission markdown headings, futon3c `data/mission-state/*.edn`, evidence
   snapshots, or a combination?
2. Which API should WebArxana read first for evidence: direct futon3c
   `/api/alpha/evidence`, proxied futon1a entity queries, or a new local
   WebArxana projection endpoint?
3. What is the minimum graph schema for a live mission projection:
   `mission -> phase -> evidence-turn -> pattern`, or a different topology?
4. Does Mission Search's existing `futon.missions/search-missions` result
   contain enough path/date/status metadata to implement "recent missions"
   without a separate index?
5. Does the Interest Network readability problem come mainly from graph scale,
   label policy, force parameters, or the decision to show too many entity
   kinds in one viewport?

### MAP question answers, pass 1

**Q1. Canonical machine-readable mission phase state.**

There is no single canonical source yet; this is a real MAP finding, not a
design choice to be patched over.

Observed candidates:

| Candidate | Evidence | Fitness for this mission |
|---|---|---|
| Mission markdown status/header | `mission_control_backend.clj` parses `**Status:**`, headings, date, owner, cross-refs, code paths, PSRs, PURs, and phase. | Best coverage across existing mission docs, but derived from prose and file metadata. Good for inventory and bootstrapping. |
| Substrate-2 mission-doc hyperedges | `futon.missions/load-missions` prefers substrate-2 `code/v05/mission-doc` hyperedges and maps their props to LC1 records. | Best current search/index source when substrate-2 is live. Carries phase/status/date/path/code paths but not necessarily full lifecycle node detail. |
| futon3c `data/mission-state/*.edn` | Example files carry `:mission/id`, cycles, obligations, current cycle phase, phase data, updated-at. | Best operational cycle state for mission peripheral runs, but currently sparse and not obviously covering all markdown missions. |
| Evidence snapshots | `futon3c.peripheral.mission/state-snapshot` emits mission snapshots on `:mission-save`; `mission->sync-evidence` emits content-hash-stable mission sync snapshots. | Best durable event/snapshot substrate for WebArxana live projection, but requires query/projection wiring and may need backfill/sync coverage. |

Pass-1 answer: WebArxana should read a projection that can merge mission-doc
hyperedge props for inventory, evidence snapshots for change history, and
mission-state EDN only when a mission peripheral cycle is active. DERIVE must
choose the projection boundary; direct UI parsing of markdown would duplicate
the existing parser and violate the no-duplicate-implementation discipline.

**Q2. Evidence API boundary.**

The first implementation should add a local WebArxana projection endpoint that
queries the evidence/source systems server-side, rather than having the browser
call futon3c directly. Reasons:

- WebArxana already proxies futon1a through `server/core.clj` and has local
  projection endpoints for mission search and interest network.
- The browser should receive graph-shaped data, not raw evidence entries whose
  interpretation duplicates server logic.
- The endpoint can later switch from polling to WebSocket deltas without
  changing the graph renderer's data contract.

So the likely DERIVE direction is `/api/mission-live` or
`/api/mission-graph/:mission-id`, returning nodes/edges plus source evidence
ids. This is not yet a final design decision.

**Q3. Minimum live mission graph schema.**

Minimum useful topology for the first demo:

```text
mission
  -> phase node(s): HEAD, IDENTIFY, ...
  -> evidence session(s)
phase
  -> authored section / checkpoint source
evidence session
  -> user turn
  -> assistant turn
turn or phase
  -> pattern record(s): PSR/PUR where present
```

This is enough to satisfy the completion criteria without trying to model the
entire stack. It also leaves room for Interest Network and Mission Search as
projection filters:

- Mission Search selects candidate `mission` nodes.
- Interest Network adds cross-corpus / posterior context around selected
  missions or essays.

**Q4. Mission Search metadata for recent/date search.**

The current mission search index already carries date metadata. The cached
`mission_records.json` sample has `date`, `phase`, `phase_rank`, `status`,
`path`, `home_repo`, `owner`, `summary`, `cross_refs`, and `code_paths`.
`futon.missions/mission-result` returns `:date`, `:phase`, `:status`, `:path`,
and related fields to WebArxana. `structural-search` already sorts partly by
`:phase_rank`, `:date`, and `:title`.

Pass-1 answer: "recent missions" should not need a new index. It likely needs
a new query mode over the existing mission records, probably server-side in
`webarxana.server.mission-search` / `futon.missions`, so the client can ask
for recent/date-ranked missions without pretending it is a text query.

**Q5. Interest Network readability.**

Current evidence points to a combination of scale and label policy:

- `interest_network.cljs` uses a fixed `1100 x 760` viewBox and labels only
  standing nodes, essays, or nodes with degree > 4.
- Territory radii are small: `(+ 7 (* 2.0 sqrt(degree)))`; essay radii start
  at 12. This makes many nodes visually subordinate in a dense force layout.
- Force settings use a moderate repulsion (`-170`) and link distance `70`,
  so dense bipartite structure can compress into a small unreadable mass.
- The surface mixes territory, essay, event-posterior, and ref nodes in one
  viewport, so the feature-vs-filter question is not cosmetic. It may be
  asking one view to do too many jobs.

Pass-1 answer: DERIVE should not only increase node size. It should decide
which entity classes are visible by default, which labels are persistent, and
whether Interest Network is a filtered graph mode entered from central graph
state. Force tuning is necessary but secondary.

### MAP exit status

MAP is complete enough for DERIVE, with one blocked live check recorded below.
2026-05-30 live checks:

- `http://127.0.0.1:7071/api/alpha/entities/latest?type=interest-event&limit=1`
  returned a real `interest-event` entity from `M-interest-network-coupling`.
- `http://127.0.0.1:7071/api/alpha/hyperedges?type=code/v05/mission-doc&limit=1`
  returned a real `code/v05/mission-doc` hyperedge carrying mission id,
  phase, status, path, cross-refs, PSRs/PURs, code paths, and summary.
- `http://127.0.0.1:7070/api/alpha/evidence?limit=3` timed out after 3s from
  this shell. This does not invalidate the evidence-store design, but it does
  mean the WebArxana DERIVE contract must not assume the browser can directly
  read futon3c evidence without a local projection endpoint and failure state.
- Follow-up diagnosis in the same session: `GET /agency/connected` on the
  same server returned a fast 404, proving the server can respond; `GET
  /health`, `GET /api/alpha/evidence/count`, and `GET
  /api/alpha/evidence?tag=codex&limit=1` all timed out. Code inspection shows
  `futon3c.evidence.xtdb-backend/-query` calls `query-all-entries` and then
  filters/sorts in memory, and `handle-evidence-query` applies `author`,
  `session-id`, and `pattern-id` filters after `estore/query*`. That makes
  this a likely evidence-query performance / indexing fix-up, not simply a
  wrong URL.

Remaining concrete checks before INSTANTIATE:

- Confirm whether WebArxana can reach the same evidence store as the current
  Emacs REPL surface without a new auth or network boundary.
- Inspect the current `M-web-arxana-ui-improvements` mission record after the
  next mission-sync/backfill pass, because this mission file was newly added
  in commit `d52287b` and may not yet be in substrate-2.
- Decide whether the first projection consumes mission markdown snapshots,
  evidence snapshots, or both.

### Integration fix-up issues discovered during MAP/DERIVE

| Issue | Evidence | Required investigation |
|---|---|---|
| `F-1 evidence-query-timeout` | `127.0.0.1:7070` responds to unknown routes, but `/health`, `/api/alpha/evidence/count`, and filtered `/api/alpha/evidence` calls timed out at 2-3s. | Check whether XTDB evidence queries are doing full-store scans; add indexed/limited query paths or server-side pagination before WebArxana depends on evidence reads. |
| `F-2 evidence-filter-boundary` | `handle-evidence-query` builds a partial `EvidenceQuery`, then filters `author`, `session-id`, and `pattern-id` after `estore/query*`. | Decide which filters belong in `EvidenceQuery` / backend-level query so `limit=1` and session-scoped reads do not require reading the whole evidence landscape. |
| `F-3 health-coupling` | `/health` also timed out; `handle-health` computes `evidence-count` with `(count (estore/query* evidence-store {}))`, so health is coupled to a full evidence-store scan. | Health should expose degraded evidence state without blocking the whole endpoint; count should use an indexed/count path or a bounded/timeout-protected check. |

## 3. DERIVE — 2026-05-30 draft

### Design target

Build a **live mission graph projection** inside WebArxana, then make Mission
Search and Interest Network feed into that central graph instead of remaining
isolated feature pages.

The first build should prove one loop:

```text
mission doc / substrate-2 mission hyperedge / evidence snapshot
  -> WebArxana mission graph projection
  -> browser click on mission/phase/evidence node
  -> typed Emacs open/edit request
  -> acknowledgement or visible refusal
```

This is a projection and action contract, not a replacement graph renderer.

### Entity types

| Entity | Identity | Source | Notes |
|---|---|---|---|
| Mission | `mission/<mission-id>@<repo>` or existing LC1 mission id | substrate-2 `code/v05/mission-doc` hyperedge props, with fallback to mission records cache | Reuse existing `futon.missions` identity where possible. |
| Mission phase | `mission-phase/<mission-id>/<phase>` | derived from lifecycle headings, mission status/phase, or mission-state cycle phase | HEAD, IDENTIFY, MAP, DERIVE, ARGUE, VERIFY, INSTANTIATE, DOCUMENT. |
| Mission section | `mission-section/<mission-id>/<heading-slug>` | parsed mission doc or substrate props when available | Optional in v1; useful for opening exact headings in Emacs. |
| Evidence session | `evidence-session/<session-id>` | evidence entries or REPL metadata | Represents a REPL/thread context attached to a mission. |
| Evidence turn | `evidence/<evidence-id>` | evidence API/projection | User/assistant turns, snapshots, PSR/PUR/PAR entries. |
| Pattern record | `pattern-record/<evidence-id>` or `pattern/<pattern-id>` | PSR/PUR evidence or mission-embedded PSR/PUR extraction | Kept separate from pattern definition nodes. |
| Projection filter | `projection-filter/<kind>/<id>` | Mission Search or Interest Network selection | A transient or durable node that records how the current graph was assembled. |

### Relation types

| Relation | Source -> target | Meaning |
|---|---|---|
| `mission/has-phase` | Mission -> Mission phase | Lifecycle structure. |
| `mission/current-phase` | Mission -> Mission phase | Current derived phase; at most one current phase per projection snapshot. |
| `mission/has-section` | Mission -> Mission section | Opens exact doc heading when known. |
| `phase/evidenced-by` | Mission phase -> Evidence turn/session/snapshot | A phase transition or phase content has durable evidence. |
| `session/has-turn` | Evidence session -> Evidence turn | Conversation chain projection. |
| `turn/in-reply-to` | Evidence turn -> Evidence turn | Evidence chain order. |
| `turn/activates-pattern` | Evidence turn -> Pattern record | PSR/PUR/PAR or mined pattern application. |
| `search/selected` | Projection filter -> Mission | Mission Search result became a graph selection. |
| `interest/contextualizes` | Projection filter -> Mission/essay/entity | Interest Network adds posterior context around a selected graph focus. |

### Invariant rules

1. **Source authority invariant:** Browser-local graph state is never the
   authority for mission phase, evidence, or pattern status. It is a projection
   of mission records, substrate-2 hyperedges, mission-state EDN, and/or
   evidence entries.
2. **No duplicate parser invariant:** WebArxana must not introduce a second
   mission markdown parser if `mission_control_backend.clj` or substrate-2
   mission-doc hyperedges already carry the needed field. Missing fields should
   be added upstream or in a shared projection layer.
3. **Typed action invariant:** Browser-to-Emacs actions must use an action
   envelope, not a bare ambiguous URI, whenever the target is mission/evidence
   state.
4. **Round-trip witness invariant:** Every action that claims to open/edit a
   live graph node must return `{:ok true ...}` or a structured refusal that
   can be rendered in the UI.
5. **Projection provenance invariant:** Every mission graph node/edge must
   carry enough source metadata to answer "why is this here?" - source path,
   hyperedge id, evidence id, session id, or projection id.
6. **Feature-is-filter invariant:** Mission Search and Interest Network may
   remain as pages only if each can also hand selected results to the central
   graph as a projection/filter. A standalone page with no graph handoff is
   incomplete.

### Data flow

```text
futon1a / substrate-2
  code/v05/mission-doc hyperedges
  interest-event entities
        |
        v
WebArxana server projection endpoints
  /api/mission-graph/:mission-id
  /api/mission-search
  /api/interest-network
        |
        v
WebArxana client local graph cache
  graph page / mission projection mode
        |
        v
/api/emacs/action
  typed open/edit request to emacsclient
```

Evidence has two possible ingestion paths:

```text
Preferred v1:
futon3c evidence API or XTDB-backed evidence store
  -> WebArxana server /api/mission-graph/:mission-id
  -> graph nodes with evidence ids

Fallback v1:
futon3c data/repl-traces/*.edn
  -> WebArxana server reads configured trace dir
  -> graph nodes tagged as file-derived, not evidence-derived
```

The preferred path is semantically cleaner. The fallback is useful only if the
evidence endpoint remains unavailable during implementation, and must be marked
as file-derived so it does not masquerade as canonical evidence.

### IF / HOWEVER / THEN / BECAUSE decisions

**D1 - projection endpoint.**

IF WebArxana needs live mission/evidence graph data, HOWEVER the evidence API
may be slow/unavailable from this process and raw evidence entries are not UI
graph records, THEN add a WebArxana server projection endpoint, BECAUSE the
browser should receive graph-shaped data with provenance and explicit failure
states.

**D2 - mission phase source.**

IF mission phase appears in markdown, substrate-2 mission-doc hyperedges, and
mission-state EDN, HOWEVER duplicating mission parsing in WebArxana would
weaken coherence, THEN use substrate-2 mission-doc hyperedges as the inventory
source and evidence/snapshot records as change-history source, BECAUSE that
keeps WebArxana as a consumer of canonical projections.

**D3 - browser-to-Emacs action.**

IF `/api/emacs/open` already opens `arxana://`, `docbook://`, and file paths,
HOWEVER live mission/evidence nodes require action semantics beyond location
strings, THEN introduce `/api/emacs/action` or extend `/api/emacs/open` with
an action envelope, BECAUSE org-protocol-style URI dispatch is exactly where
the org-roam-server#35 prior art showed ambiguity.

**D4 - Mission Search integration.**

IF Mission Search is already useful and carries date/phase/path metadata,
HOWEVER it is currently a separate results page, THEN add a "send to graph"
path that pins selected results or opens a mission graph projection, BECAUSE
search should select graph context rather than become a competing surface.

**D5 - Interest Network integration.**

IF Interest Network already has a meaningful posterior projection, HOWEVER its
default view is too dense and semantically mixed, THEN make it a filtered graph
mode by default and retain the standalone page only for full-corpus inspection,
BECAUSE operator navigation needs readable local context more than a total
hairball.

**D6 - trace fallback.**

IF REPL trace files are canonical for the REPL spec, HOWEVER evidence entries
are the durable cross-system record, THEN trace-file ingestion may be used as a
fallback/spike but evidence-backed projection remains the completion target,
BECAUSE otherwise the browser would display a parallel record rather than the
stack's evidence landscape.

### View / UI specifications

#### Central graph mission mode

Add a graph mode or route such as:

```text
/wa#/mission/<mission-id>
/wa#/mission/<mission-id>/session/<session-id>
```

Expected view behavior:

- Mission node is pinned/focused.
- Lifecycle phase nodes are arranged in order, not arbitrary force placement.
- Current phase has a clear visual state.
- Evidence/session nodes attach below or beside the relevant phase.
- Pattern records attach to the evidence turn or phase that activated them.
- Each node exposes source provenance in tooltip/card metadata.
- Empty/missing evidence is shown as absence with reason, not hidden.

#### Browser-to-Emacs action envelope

Proposed request shape:

```edn
{:action/type :mission/open-section
 :target/type :mission-phase
 :target/id "mission-phase/M-web-arxana-ui-improvements/derive"
 :mission/id "M-web-arxana-ui-improvements"
 :source/path "/home/joe/code/futon4/holes/missions/M-web-arxana-ui-improvements.md"
 :source/heading "3. DERIVE"
 :session/id nil}
```

Response shape:

```edn
{:ok true
 :action/type :mission/open-section
 :target/id "mission-phase/M-web-arxana-ui-improvements/derive"
 :opened {:kind :file-heading
          :path "/home/joe/code/futon4/holes/missions/M-web-arxana-ui-improvements.md"
          :heading "3. DERIVE"}}
```

Refusal shape:

```edn
{:ok false
 :reason :heading-not-found
 :message "Mission heading was not found in the current file."
 :target/id "mission-phase/M-web-arxana-ui-improvements/derive"}
```

#### Mission Search

- Keep text query.
- Add recent/date mode server-side, using existing `:date` metadata.
- Add result command: "Open graph" or equivalent icon action that routes to
  `#/mission/<mission-id>` or pins the result into graph state.
- Preserve confidence/top-k controls, but avoid making confidence the only
  clutter control.

#### Interest Network

- Add local-context mode: when a mission/essay/entity is focused, show only
  interest-network nodes within an operator-readable radius or selected kinds.
- Increase label legibility for visible nodes; labels should be based on graph
  role and focus context, not only degree threshold.
- Keep full-corpus route as an inspection mode if needed, but document it as
  full-corpus overview, not the main operator workbench.

### Verification methodology

Visual features should be verified in two stages:

1. **Playwright first.** Use automated browser checks to catch blank screens,
   broken routes, JavaScript exceptions, unreadable default density, missing
   labels, failed click actions, and API failure states. For graph views, the
   Playwright check should include a screenshot or canvas/SVG element count so
   "route loaded" cannot pass while the visual surface is empty.
2. **Live QA with Joe.** After Playwright passes, run the surface live with Joe
   because the target property is operator usability: whether the mission
   graph, search handoff, and interest-network filter make the stack easier to
   navigate while actual work is happening. Joe's QA observations should be
   recorded back into this mission as evidence or fix-up issues, not treated as
   informal polish notes.

This methodology is part of the mission design. For visual graph work,
automated tests are necessary but not sufficient; the completion proof needs
both mechanical evidence and operator-readability evidence.

### Fidelity contract

This mission extends existing WebArxana behavior, so these capabilities must
be preserved or adapted:

| Existing capability | Preserve/adapt/drop | Tripwire |
|---|---|---|
| Graph view pins/focus/type hash routing | Preserve | Existing graph hashes still restore pins/focus/type. |
| `#/mission-search/<query>` deep link | Preserve | Existing mission-search URL still runs a query. |
| Mission Search confidence/top-k/agreement controls | Preserve | Controls still affect server query parameters. |
| Mission Search result click telemetry | Preserve | Click event still posts to `/api/mission-search/event`. |
| `/api/emacs/open` for `arxana://`, `docbook://`, and file paths | Preserve | Existing essay/section open still works. |
| Interest Network standalone route | Adapt | Route may remain, but selected context can also be sent to graph. |
| Interest Network completeness 3-vector | Preserve | Full-corpus route still renders the 3-vector when data includes it. |
| WebArxana `/api/futon/*` proxy | Preserve | Existing graph entity/hyperedge reads still work. |

### DERIVE exit status

Drafted, not frozen. Before ARGUE/VERIFY, this design needs a human check on
two choices:

1. Should the typed Emacs action be a new `/api/emacs/action` endpoint, or an
   extension of `/api/emacs/open`?
2. Should the first implementation target a mission graph route first, or the
   Mission Search "send to graph" path first?
