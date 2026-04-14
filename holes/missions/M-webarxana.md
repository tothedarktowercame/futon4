# Mission: WebArxana — Collaborative Hypergraph Surface

**Date:** 2026-04-12
**Status:** INSTANTIATE (2026-04-12). Interactive testing pass done.
Multi-focus canvas, d3-force layout, diagrams, link annotations.
Pending: deployment, some polish items.
**Blocked by:** None (futon1a operational, Arxana Browser operational,
WebArxana v1 prototype committed)
**Owner:** futon4 (WebArxana), with dependencies on futon1a (data store)

## Motivation

The Arxana knowledge graph has rich structure — entities, relations,
hyperedges, evidence — but its primary interface is an Emacs browser.
This is powerful for the person at the keyboard but closed to
collaborators. There is no surface where two people can co-create
within the same hypertext.

The existing collaborative tools (WhatsApp, Google Docs) offer either
ephemeral chat or flat documents. Neither provides structured
annotation, typed connections, or the ability to navigate a growing
web of ideas. The chorus/song/chapbook workflow demonstrates what
Arxana collaboration *could* look like: one person writes source
material, another draws connections and glosses it. But today that
workflow flows through a single person's Emacs.

WebArxana should be a browser-based surface where collaborators can
add short texts, draw connections between them, inspect what each
person has contributed, and gradually build a shared structured world —
a replacement for WhatsApp + Google Docs for private creative
collaboration.

## Theoretical anchoring

- **Scholia-based annotation (Arxana):** Every connection is itself an
  entity. Annotations are first-class, not margin comments.
- **Peer learning (PlanetMath):** Knowledge is co-constructed.
  Authorship is attributed, not dissolved.
- **Pattern languages:** The type system provides a shared vocabulary.
  Collaborators classify their contributions using existing types, not
  ad-hoc tags.
- **Hypergraph editing:** Connections are not limited to binary edges.
  Hyperedges can link passages across multiple entities, carrying
  glosses and roles. The web surface should be a first step toward
  making hypergraph structure inspectable and editable outside Emacs.

## Scope

### In scope

- **Node creation:** Top-level "new node" (default type: article) and
  contextual "+ Adjacent" (default relation: arxana/scholium). Both
  prompt for name and allow picking type/relation from the registered
  type system.
- **Hop-depth controls:** +/- buttons to expand or contract the
  visible neighbourhood (currently fixed at k=3).
- **Author attribution:** Entities record who created them. The UI
  shows authorship.
- **Text-first cards:** The card body should support multi-line text
  entry — enough to write a short passage, not just a label.
- **Activity feed:** A way to see recent additions across the graph,
  so collaborators can see what the other person added (cf. futon1a
  evidence timeline).
- **Hyperedge inspection:** Display hyperedges connected to the
  focused entity (done in v1). Show the gloss/note from hyperedge
  props.

- **Multi-user real-time sync:** Wire the existing WebSocket
  infrastructure so that when one user creates or edits an entity,
  all connected clients see the change without refreshing. XTDB's
  transactional model makes this natural — broadcast after tx commit.
- **Public access / deployment:** Deploy so a collaborator who isn't
  Joe can reach it. This is an exit criterion: the system is only
  useful for collaboration if a collaborator can use it.
- **Author list on entities:** Entities track an ordered list of
  contributors (creator + subsequent editors). Not a single "owner"
  — co-authorship is the norm.
- **Multi-focus canvas:** The graph should support multiple "pinned"
  focus nodes, each bringing their k-neighbourhood into the view.
  This enables the core use case: pulling in clusters from different
  parts of the graph and drawing connections between them. Two entry
  points: (1) the green "+" creates a new node in the canvas, and
  (2) a "pin" or "inject" action (e.g. "\u2192" button) pulls an
  existing node from the library/sidebar into the canvas as a second
  focus. Layout shifts from single-centre radial to multi-centre
  (force-directed or multi-radial). The Connect workflow then works
  across clusters. This is the primary design shift from v1's
  "magnifying glass" model to a "workbench" model.

### Out of scope (deferred)

- **Character-level annotation editor:** The Emacs chorus model
  (passage-level annotation with line-range references). Valuable
  but complex; may warrant its own mission.
- **Inline text editor (codemirror/prosemirror):** A rich editor
  alongside the graph. Explored if the card body proves too limited.
- **Hyperedge *creation* workflow:** Select source node(s), browse to
  sink, save annotation with roles. Procedurally complex; deferred
  but not ruled out.

### Potential sequel: M-webarxana-hypergraph

WebArxana v2 works with binary relations and displays existing
hyperedges. A follow-on mission should extend this to full
hypergraph browsing and interaction:

- **Annotating structures, not just nodes:** Select a subgraph
  (multiple nodes, a cluster, an entire local spread) and create
  a hyperedge that annotates the whole thing — not just one node.
- **Hyperedge creation UI:** A multi-step workflow: select
  source endpoints (with roles and optional passage references),
  browse to sink endpoints, add a gloss, save as a typed
  hyperedge.
- **Hyperedge-as-node:** Hyperedges should be navigable entities
  themselves — clickable, focusable, with their own cards showing
  endpoints, roles, and gloss.

**Architectural constraint for the current mission:** The multi-focus
canvas, the Connect workflow, and the entity/relation model must
not preclude these extensions. Specifically:
- The graph renderer must be able to handle n-ary connections (not
  just binary edges) without a rewrite.
- The Datascript schema should remain compatible with hyperedge
  ingestion (already the case: hyperedges are ingested as links
  with extra metadata).
- The spread/pin model should support selecting multiple nodes as
  a group, even if we don't act on that selection yet.

## Completion criteria

1. A user can create a new freestanding node from the top bar,
   choosing name and type from the registered type system.
2. `+ Adjacent` prompts for name, type (defaulting to "article"),
   and relation type (defaulting to "arxana/scholium"), then creates
   both entities and the link.
3. Hop-depth is adjustable via +/- controls in the UI.
4. Each entity shows its author list. New entities record the
   logged-in user as creator; edits append to the author list.
5. An activity/recent-additions view shows what was added and by whom.
6. When one user creates or edits content, other connected clients
   see the change in real time via WebSocket broadcast.
7. The system is deployed and reachable by a collaborator outside
   localhost.
8. All of the above is covered by Playwright tests.

## Relationship to other missions

- **Depends on:** M-self-representing-stack (Arxana Browser
  infrastructure, complete). futon1a entity/relation/hyperedge APIs.
- **Enables:** Future missions around collaborative annotation,
  hyperedge editing, and pattern-language co-construction.
- **Related:** The Emacs Arxana Browser (arxana-store.el,
  arxana-browser-chorus.el) serves as the reference implementation
  for data access patterns.

## Source material

- `futon4/dev/web/webarxana/` — WebArxana v1 prototype (committed
  2026-04-12, `4e5831f`)
- `futon4/dev/arxana-store.el` — Canonical data access layer
- `futon4/dev/arxana-browser-chorus.el` — Chorus annotation model
- `futon1a/src/futon1a/api/routes.clj` — Backend API
- `futon1a/src/futon1a/compat/futon1_write.clj` — Entity/relation
  write logic (note: `props` field not persisted by entity endpoint)

## Owner and dependencies

- **Primary repo:** futon4
- **Backend:** futon1a (props persistence may need fixing; otherwise
  work within existing API)
- **Driver:** Joe + collaborators via WebArxana itself

---

## MAP

Survey questions and answers:

- Q1: What entity types and relation types are registered in
  futon1a's type system? Which are relevant for collaborative
  writing?
  - **To answer during MAP.** The v1 prototype enumerated types via
    `/api/alpha/types`. Relevant subset for collaboration TBD.

- Q2: How does author attribution currently work?
  - **Answer (from IDENTIFY discussion):** There is no author field
    on entities. We need to add an author list (ordered list of
    contributors: creator + subsequent editors). This is distinct from
    the penholder system (which controls write authorization) and from
    the evidence system (which records AI agent sessions, not human
    co-editing). WebArxana sessions are a *source of* evidence but we
    don't edit existing evidence — history is immutable.

- Q3: What should the activity feed show?
  - **Answer (from IDENTIFY discussion):** The evidence timeline is
    orthogonal — it tracks AI agent work. The WebArxana activity feed
    should show recent entity/relation writes by human collaborators,
    with author attribution. This may be a new lightweight query
    (recent entities by creation date) rather than the evidence API.

- Q4: What are the current limits of the entity write endpoint?
  - **Answer (from prototype):** `props` field is not persisted by
    `futon1a/compat/futon1_write.clj:ensure-entity-doc` (lines 80-85
    only extract name, type, external-id, source). This needs fixing
    in futon1a before we can store author lists, annotation IDs, or
    other structured metadata on entities. **Decision needed:** fix
    in futon1a, or work around via a separate metadata store.

- Q5: How do the existing Emacs browser views handle hop-depth
  control and neighbourhood expansion?
  - **Answer (from IDENTIFY discussion):** The Emacs browser doesn't
    do hop-depth. It composites static views that can be assembled
    from any graph contents. WebArxana's radial ego-graph with
    adjustable k is a new interaction model.

- Q6: What use cases should drive the design? What does a WebArxana
  session look like in practice?
  - **To answer during MAP.** Core use case: two humans co-writing.
    One adds a short text, the other draws connections or responds.
    Think of it as structured conversation where the messages persist
    as nodes in the graph. Adjacent use cases: inspecting what a
    collaborator added, following a thread of connected nodes, adding
    a typed annotation to existing material.

- Q7: What deployment model makes sense for "reachable by a
  collaborator outside localhost"?
  - **To answer during MAP.** Options: tunnel (ngrok/tailscale),
    VPS deployment, or piggyback on existing futon3c Linode
    infrastructure.

---

## DERIVE — Multi-focus canvas ("the spread")

### Analogy

The interaction model resembles a tarot spread. You **pull** entities
from the library onto a canvas. Each arrives with its associations
(ego neighbourhood). You arrange them spatially and then **draw
connections** between entities from different clusters. In tarot, the
reading is ephemeral; in Arxana, the connections you draw are globally
persisted as relations/scholia. The spatial arrangement is session
state; the structural relationships are permanent.

### Entity types

**Spread (session-level, client-side).** A spread is the set of
pinned entities currently on the canvas, their layout positions, and
the hop-depth for each. Spreads are NOT persisted to futon1a — they
are URL-encoded in the hash for shareability and stored in the
client's ui-state for the session. A spread is reconstructed from
its hash.

IF we later want persistent spreads (e.g. "Joe's reading from
Tuesday"), THEN we can promote them to futon1a entities of type
`arxana/spread` with props listing the pinned entity IDs.
HOWEVER this is not needed for the initial implementation.
BECAUSE the hash already captures the spread state, and forcing
persistence adds friction to an exploratory workflow.

**Pinned node.** An entity that has been pulled onto the canvas.
Each pin has:
- `entity-id` — the futon1a entity ID
- `position` — [x, y] on the canvas (client-side, mutable by drag)
- `hop-depth` — how many hops of neighbourhood to show (default 1,
  adjustable per-pin, separate from the global k)

The focused node (whose card is shown on the right) is one of the
pinned nodes. Clicking a node makes it the active card but does not
unpin the others.

### Interaction model

**Pull from library ("\u2192" or "Pin" button):**
When browsing entities in the sidebar, a "Pin" button next to each
entity (or on the search result) adds it to the canvas as a new
pinned node. Its ego is fetched and its neighbourhood rendered.
Position is auto-assigned (avoid overlap with existing pins).

**Green "+" (create new):**
Works as now — creates a local scratch node, shown on canvas. On
Save, it becomes a real entity and a pinned node.

**Connect across clusters:**
Same as now — click Connect on a node, then click a target node
anywhere on the canvas (even in a different cluster). The relation
is created and becomes visible as a link spanning the two clusters.

**Unpin ("\u00d7" or "Remove from spread"):**
Removes a pinned node and its neighbourhood from the canvas. Does
not delete the entity from futon1a.

**Focus (click):**
Clicking any node on the canvas makes it the active card (shown
on the right). The graph stays multi-focus; only the card changes.

### Layout algorithm

The single-centre radial layout is replaced by a multi-centre layout:

**Option A — Multi-radial (simpler):**
Each pinned node gets its own radial layout centred at its position.
Overlapping neighbourhoods merge visually — if a node appears in two
clusters, it gets one position (midpoint or closer to the pin with
more connections to it). Links between clusters are drawn as longer
edges crossing the canvas.

**Option B — Force-directed (more natural):**
Use a force simulation (d3-force or a lightweight equivalent).
Pinned nodes have fixed positions (draggable); their neighbours
are pulled toward them. Cross-cluster links create attractive forces
that pull related clusters closer. This produces more organic
layouts but requires an animation loop.

**Decision:** Start with multi-radial (Option A). It's deterministic,
no animation needed, and works with the existing rendering. If the
layouts become too crowded or overlapping, upgrade to force-directed.
BECAUSE multi-radial is a straightforward extension of the current
code, while force-directed requires a simulation loop, requestAnimationFrame,
and careful performance tuning.

### Hash encoding for spreads

Current hash: `#/type/<type>/focus/<entity-id>`

Extended hash for multi-focus:
`#/pins/<id1>,<id2>,<id3>/focus/<active-id>`

Where `pins` is a comma-separated list of pinned entity IDs, and
`focus` is the one whose card is shown. The `type` segment remains
optional (for sidebar state). Per-pin hop-depth could be encoded
as `<id>:k` (e.g., `<id>:2`) but defaults to 1 if omitted.

### Data flow

1. User pins entity → `fetch-ego` for that entity → ingest into
   Datascript → add to `ui-state.pins` list
2. Graph renders all pinned nodes + their neighbourhoods via
   `neighbourhood` for each pin, merged into one position map
3. User draws connection → `save-relation!` → relation appears as
   a cross-cluster link
4. User unpins → remove from `ui-state.pins` → graph re-renders
   without that cluster (Datascript retains the data but the graph
   filters to pinned nodes' neighbourhoods)

### UI changes needed

1. Replace single `:focus-id` with `:pins [{:id ... :pos [...] :k 1} ...]`
   and `:active-pin` (which pin's card is shown)
2. Add "Pin" button to sidebar entity items and search results
3. Add "Unpin" button to pinned nodes (or to the focus card header)
4. Graph: render merged multi-radial layout from all pins
5. Update hash encoding/decoding for multi-pin state
6. Scratch card "Save" adds the new entity as a pin
7. Connect works across any nodes on the canvas

### IF/HOWEVER/THEN/BECAUSE

IF the multi-radial layout produces overlapping clusters,
HOWEVER the user can adjust per-pin hop-depth to reduce clutter,
THEN the layout remains usable without force-directed simulation,
BECAUSE reducing k from 3 to 1 on crowded pins dramatically reduces
overlap while keeping the essential connections visible.

IF a node appears in multiple pins' neighbourhoods,
HOWEVER it can only have one position on the canvas,
THEN we assign it the position computed by the first pin that
introduced it (stable, deterministic),
BECAUSE attempting to merge positions (midpoint) creates jittery
layouts when pins are added/removed, and the "first wins" rule
is simple and predictable.

### Pattern cross-reference (vsatelier)

Patterns from `futon3/library/vsatelier/` mapped to this design.
Not all apply equally — the current mission is a warm-up for
VSATELIER, not a full implementation.

**Cluster as Agenda Item** — *directly supports*.
The multi-focus canvas IS a cluster viewer. When the user pins
multiple entities and sees their neighbourhoods overlap or form a
bridge, that visual pattern is a candidate "agenda item." In the
current design, the user acts on this by drawing a connection
(creating a scholium). A future extension could let the user
*flag* a cluster as a named discussion topic. For now, the
Connect action is the lightweight equivalent: "I see a pattern
here, I'm recording it as a relation."

**Decision Provenance** — *partially supports*.
Every relation created in WebArxana is persisted with author
attribution (`props.authors`) and can be traced back to the
entities it connects. The spread itself (the set of pins) is the
context in which the decision was made — the hash captures this
context, even if it's not a first-class entity yet. The gap:
there's no explicit "decision" entity type linking the why to
the what. IF we add `arxana/decision` entities that reference
the spread hash and the relations created during a session,
THEN we close this gap. Deferred but architecturally compatible.

**Annotation as Commitment** — *seeds the workflow*.
The scratch card → Save → Connect flow is a lightweight version
of "observation graduates to commitment." The user writes a note
(annotation), saves it as an entity (gives it identity), and
connects it to existing structure (commits to a relationship).
The full pattern would add deadlines, owners, and status tracking
on annotations. This is out of scope but the entity + relation
model can carry those fields in `props` when needed.

**Return Loop** — *architecturally present, not yet surfaced*.
When a user creates a relation between clusters, that relation
becomes visible to future spreads — anyone who pins one of the
connected entities will see the link to the other cluster. This
IS the return loop: actions taken in one session alter the graph
that future sessions explore. The gap: there's no explicit
"outcome" story or "what changed" node. The activity feed (in
scope) would partially surface this — showing what was added
and by whom, so the loop is visible.

**Session as Rehearsal** — *structural fit, not yet scoped*.
The spread model is inherently session-based and low-stakes:
you can pin, connect, unpin, try again. The spread isn't
persisted by default, so it's a safe space for exploration.
A future "rehearsal mode" could load a curated sub-graph and
pose a question. For now, the ephemeral spread is the rehearsal
space.

**Projection Independence** — *partially honoured*.
The Datascript graph model, the futon1a entity/relation types,
and the hash-encoded spread state are all independent of the
SVG rendering. A different frontend (terminal, AR, mobile)
could reconstruct the same spread from the same hash. The gap:
interaction protocols (pin, connect, flag) are embedded in the
ClojureScript UI, not defined as abstract contracts. Acceptable
for now; worth extracting if a second surface is built.

**Federated Cosmoses** — *not in scope, but not blocked*.
The current design is single-instance (one futon1a backend).
Federation would require multi-backend proxy support in the
webarxana server. The entity model doesn't prevent it — entity
IDs are opaque strings, and a federated ID scheme (e.g.,
`instance://entity-id`) could work. Deferred.

---

## ARGUE

### Synthesis from pattern THEN statements

The vsatelier patterns converge on a single workflow arc:
**see structure → flag it → deliberate → decide → record → witness
the consequences.** The WebArxana multi-focus canvas implements the
first three moves of this arc:

1. **See structure:** Pinning entities onto the canvas and expanding
   their neighbourhoods makes structure visible across disconnected
   parts of the graph. This is the *Cluster as Agenda Item* gesture —
   the canvas IS a cluster viewer, and each pin is a hypothesis about
   what's relevant.

2. **Flag it:** Drawing a connection between entities from different
   clusters is a lightweight flag. The user says "these things are
   related" by creating a scholium. This is *Annotation as Commitment*
   in miniature — the annotation (the scholium) is a persisted,
   attributed, typed entity, not a sticky note. It doesn't yet carry
   deadlines or owners, but it carries authorship and a link back to
   the entities that prompted it.

3. **Deliberate:** Two collaborators working the same canvas — one
   adding texts, the other drawing connections — is the *Session as
   Rehearsal* pattern in its simplest form. The spread is ephemeral
   (not persisted by default), so it's a safe space to try ideas.
   The relations they create survive the session; the spread doesn't.

The remaining arc moves (decide, record, witness consequences) are
structurally compatible but not yet built:
- *Decision Provenance* needs an explicit `arxana/decision` entity
  type, which can be added without architectural changes.
- *Return Loop* is already happening at the graph level — relations
  created in one session alter future sessions — but isn't surfaced
  to the user yet. The activity feed would make it visible.
- *Projection Independence* is partially honoured: the data model
  (entities, relations, spreads-as-hash) is surface-independent.
  A second rendering surface could use the same primitives.

### Theoretical coherence

The four theoretical anchors from IDENTIFY map cleanly:

- **Scholia-based annotation:** The scholium is the primary unit of
  work in the multi-focus canvas. Every connection is a first-class
  entity with authorship, type, and provenance. This is preserved.

- **Peer learning (PlanetMath):** Two collaborators work the same
  canvas, each contributing texts and connections. Author attribution
  (`props.authors`) tracks who wrote what. The design explicitly
  supports co-construction, not just co-viewing. This is preserved.

- **Pattern languages:** The type system is the shared vocabulary.
  Collaborators classify entities using registered types, and can
  create new types by naming them. The sidebar organises by type.
  This is preserved, and the multi-focus canvas extends it: you can
  now *see* entities of different types side by side and draw
  connections between them.

- **Hypergraph editing:** The canvas displays hyperedge connections
  (done in v1) and the Connect workflow creates binary relations.
  Full hyperedge *creation* (multi-endpoint, with roles and
  passages) is deferred but structurally unblocked — the entity
  model already supports it. This is partially preserved with a
  clear upgrade path.

No theoretical shift from IDENTIFY. The design serves all four
anchors.

### Trade-off summary

| Gave up | In favour of | Why |
|---------|-------------|-----|
| Persistent spreads | Hash-encoded ephemeral spreads | Reduces friction in exploratory workflow; can add persistence later via `arxana/spread` entity type |
| Force-directed layout | Multi-radial (deterministic) | Simpler implementation, no animation loop; upgrade path is clear |
| Full VSATELIER arc (decide, record, witness) | First three moves (see, flag, deliberate) | The warm-up delivers usable collaboration now; the remaining arc is additive, not architectural |
| Hyperedge creation UI | Binary relation creation only | Binary connect is the 80% case; hyperedge creation is procedurally complex and deferred |
| Character-level annotation | Entity-level annotation | The Emacs chorus model is powerful but complex to port; entity-level scholia are the web-appropriate unit |

### Generalization notes

The multi-focus canvas is not specific to Arxana content. Any
system with typed entities and relations could use this design:
- A research group mapping connections between papers
- A design team linking user stories to technical decisions
- A community mapping relationships between organisations

The requirements: an entity/relation store with ego queries, a
type registry, and a web surface. The vsatelier patterns provide
the deliberation scaffolding. The spread model (pinned entities +
hash-encoded state) is portable across domains.

The main thing that would need to change: the type vocabulary and
the relation types. The interaction model (pin, connect, unpin,
create) is domain-independent.

### Plain-language argument

When two or more people collaborate on ideas, existing tools force a
choice: chat where ideas are ephemeral and linear, or documents where
ideas are flat and the connections between them live only in people's
heads. Physical notecards can be reorganised and grouped, but the
structures that are created are simultaneously static and ephemeral
(the structure is fixed by order or placement, but if the cards are
rearranged, the structure goes away). What's missing is a tool where
the *connections* between ideas are as real as the ideas themselves.
When you link two notes, that link should say who made the connection
and what kind of connection it is. This connection should be
discoverable by anyone navigating the graph in the future — not
buried in a paragraph, lost in a chat scroll, or hidden in a filebox.
At the same time, if someone wants to trace a different set of
connections, or integrate a different set of concepts, they should be
able to do that, too.

We are now in a position to build an easy-to-use tool like this that
works in a standard web browser, because the heavy lifting at the
level of structure already exists in a custom database. The need is
clear: creative collaboration requires both the freedom to explore
and the ability to accumulate.


---

## VERIFY

### Structural verification

No wiring diagram exists for this mission. The architecture is a
straightforward web frontend → proxy → futon1a pipeline. No
exotype diagram is needed.

### Capability cross-reference: Emacs Arxana vs. WebArxana DERIVE

| Capability | Emacs Arxana | WebArxana (v1 prototype) | WebArxana (DERIVE) | Status |
|---|---|---|---|---|
| Browse entities by type | Yes (sidebar catalogs) | Yes (sidebar) | Yes | Preserve |
| View entity content | Yes (buffer) | Yes (focus card) | Yes | Preserve |
| Edit entity content | Yes (buffer edit) | Yes (double-click card) | Yes | Preserve |
| View ego neighbourhood | Yes (static composite) | Yes (radial graph, k-hops) | Yes (multi-pin, per-pin k) | Adapt |
| Create entity | Yes (M-x) | Yes (+, scratch card) | Yes (+ with type picker) | Preserve |
| Create relation | Yes (M-x scholium) | Yes (+ Adjacent, Connect) | Yes (Connect across clusters) | Adapt |
| View hyperedge connections | Yes (chorus annotations) | Yes (auto-fetched) | Yes | Preserve |
| Create hyperedge | Yes (chorus builder) | No | No (sequel) | Defer |
| Author attribution | No | Partial (props.authors on create) | Yes (author list, display) | New |
| Activity feed | No | No | Yes (recent additions view) | New |
| Multi-user real-time | No | No (WS infra exists) | Yes (broadcast on write) | New |
| Deep linking | No | Yes (hash-based) | Yes (multi-pin hash) | New |
| Multi-focus canvas | No | No (single focus) | Yes (pin/unpin, multi-radial) | New |
| Character-level annotation | Yes (chorus passage refs) | No | No (sequel) | Defer |

**Summary:** All preserved capabilities are covered. Two are adapted
(neighbourhood and relation creation gain multi-focus behaviour).
Two are deferred to the sequel (hyperedge creation, character-level
annotation). Four are new capabilities not present in Emacs Arxana.

### Completion criteria pre-check

| # | Criterion | Addressed by DERIVE? | Notes |
|---|-----------|---------------------|-------|
| 1 | Freestanding node creation with type picker | Yes — scratch card + type badge | Already implemented in v1 |
| 2 | + Adjacent with name, type, relation prompt | Yes — creation dialog | Already implemented in v1 |
| 3 | Adjustable hop-depth | Yes — per-pin k in multi-focus | Already implemented globally in v1; per-pin is new |
| 4 | Author list display | Yes — props.authors | Props persistence fixed in futon1a; UI display not yet built |
| 5 | Activity feed | Yes — described as recent-additions view | Not yet built; needs a query (recent entities by date) |
| 6 | Real-time sync | Yes — broadcast after tx commit | WS infra exists; wiring not yet built |
| 7 | Deployed externally | Not architectural — ops task | Needs deployment decision (tunnel, VPS, or Linode) |
| 8 | Playwright tests | Yes — test pattern established | 15 tests in v1; each new feature adds tests |

**Gaps found:** Criteria 4-7 are designed but not yet implemented.
No DERIVE revision needed — these are INSTANTIATE work items.

### Spike: multi-radial layout risk

The riskiest DERIVE commitment is the multi-radial layout with
multiple pinned nodes. Risk: overlapping clusters become unreadable.

**Mitigation already in design:**
- Per-pin hop-depth (reduce k on crowded pins)
- "First pin wins" rule for shared nodes (deterministic, no jitter)
- Unpin to remove a cluster entirely

**Assessment:** The current single-radial code already handles
adaptive ring sizing (min-arc between nodes). Extending to
multiple centres is additive — each pin gets its own radial
layout offset by its canvas position. The merge logic for shared
nodes is the only new complexity, and the "first wins" rule keeps
it simple. No spike needed — the risk is low.

### Architectural constraint check (hypergraph sequel)

- Graph renderer uses SVG `<g>` groups for nodes and `<line>` for
  edges. n-ary hyperedge rendering (e.g. convex hull overlay) can
  be added as a new component type without rewriting existing code.
- Datascript schema has `:link/src` and `:link/dst` as refs.
  Hyperedges with >2 endpoints would need a different schema
  (e.g. `:hx/endpoints` as a collection). This is additive, not
  conflicting.
- The pin model stores entity IDs. Group selection (selecting
  multiple nodes) is a UI state extension, not a data model change.

**No architectural blockers for the sequel.**

### Decision log

No DERIVE revisions needed from VERIFY. All completion criteria
are addressed by the design. The capability cross-reference confirms
no regressions from Emacs Arxana for the capabilities in scope.

---

## Prototype checkpoint (pre-IDENTIFY)

The v1 prototype was built as exploratory work before this mission
was formalised. It established:

- **Architecture:** ClojureScript/Reagent frontend, Clojure/http-kit
  backend proxying to futon1a. Datascript local graph cache.
- **Working features:** Login, type-browsable sidebar, radial
  ego-graph, editable focus cards, hyperedge display, `+ Adjacent`
  (basic), hash-based deep linking.
- **Bugs found and fixed:** Proxy localhost→127.0.0.1, Datascript
  pull pattern for link refs, link label empty-string fallback,
  Reagent re-render after async hyperedge fetch.
- **Data issues found:** futon1a entity endpoint doesn't persist
  `props`; relation endpoint requires map-style `{:id "..."}` for
  non-UUID entity IDs; ego endpoint only returns outgoing relations.
- **Commit:** `4e5831f` "Add WebArxana v1"

## INSTANTIATE checkpoint (2026-04-12)

Built during a single session. Key commits:

- `4e5831f` — WebArxana v1: login, sidebar, radial graph, cards, deep linking
- `565d353` — Creation dialogs, hop-depth controls, type/relation pickers
- `c96ccdd` — Scratch card with scratchpad and connect mode
- `e6887fe` — Auto-populate name from first line on Enter
- `0149c15` — Adaptive ring radius, auto-sized link labels
- `67be5e5` — Deferred server creation (save only on Save)
- `595e505` — Fetch ego on every focus change
- `1c48b82` — Multi-focus canvas: pins, multi-radial, hash encoding
- `4fd8a70` — Author display on focus cards
- `e5e437f` — Activity feed (Recent section in sidebar)
- `8883da2` — Real-time WebSocket sync
- `89245f0` — futon1a: persist entity props through write/read pipeline

**Completion criteria status:**

| # | Criterion | Status |
|---|-----------|--------|
| 1 | Freestanding node creation with type picker | Done (scratch card + editable type badge) |
| 2 | + Adjacent with name, type, relation prompt | Done (creation dialog) |
| 3 | Adjustable hop-depth | Done (global k controls; per-pin k in state, UI pending) |
| 4 | Author list display | Done (shown in focus card header) |
| 5 | Activity feed | Done (Recent section in sidebar) |
| 6 | Real-time sync via WebSocket | Done (broadcast on save) |
| 7 | Deployed externally | Pending (ops decision needed) |
| 8 | Playwright tests | 18 tests passing |

**Remaining for COMPLETE:**
- Deployment (criterion 7)
- Per-pin hop-depth UI controls (minor)
- Persist link annotation edits to futon1a (currently Datascript only)

---

## INSTANTIATE checkpoint 2 (2026-04-12, continued session)

Interactive testing pass with Joe. Major additions:

**Multi-focus canvas:**
- `1c48b82` — Pins state, multi-radial layout, hash encoding
- `3e5b505` — Stacked pin cards on the right (one per pin)
- `4a90c14` — Fix Focus to pin (not just set-focus)
- `42869f3` — d3-force directed layout (replaced radial)
- `1af763f` — Tuned d3-force: free nodes, stronger repulsion
- `96556f5` — Phantom nodes for edge labels (prevent overlap)

**Directional edges and link editing:**
- `4eb8bbd` — Arrowheads showing edge direction
- `a24089c` — Link editor popup (click edge label)
- `e8137cb` — Annotation text field on links (scholium model)
- `0f5f9a0` — Truncate long labels, full text in popup

**Diagrams (persisted spreads):**
- `941111a` — Save Diagram: captures pin set as a `diagram` entity
- `e3e5f56` — Don't pin diagram into its own spread
- `f6060a2` — Filter diagram/includes from graph
- `0739fba` — Expand/compress toggle in sidebar
- `93c44fc` — Compress hides contents, keeps external connections

**Data layer fixes:**
- `54b73b2` — Client ingests incoming relations
- `122bda0` — futon1a: ego returns incoming relations (hot-reloaded)
- `d338c37` — Creation dialog includes text field
- `93aac55` — Save graduates scratch card to pin card
- `32f2f84` — Newest pins at top of card stack

**Completion criteria updated:**

| # | Criterion | Status |
|---|-----------|--------|
| 1 | Freestanding node creation with type picker | Done |
| 2 | + Adjacent with name, type, relation, text | Done |
| 3 | Adjustable hop-depth | Done (global controls) |
| 4 | Author list display | Done |
| 5 | Activity feed | Done (Recent with diagram expand) |
| 6 | Real-time sync via WebSocket | Done |
| 7 | Deployed externally | Pending |
| 8 | Playwright tests | 18 passing |

**Beyond original criteria (emerged during interactive testing):**
- Multi-focus canvas with pins (DERIVE feature, fully built)
- d3-force directed layout
- Directional edges with arrowheads
- Link annotations (scholium text on edges)
- Diagrams: save/expand/compress spreads
- Incoming relations in ego (futon1a change)

**Known issues / polish:**
- Link annotation edits are Datascript-only (not persisted to futon1a)
- Per-pin hop-depth UI controls not yet surfaced
- Some junk test entities in futon1a (see eviction manifest)
- Deployment decision pending (tunnel, VPS, or Linode)
- Dashed "through" edges for compressed diagrams: when a diagram is
  compressed, external nodes connected to hidden contents should show
  a dashed synthetic edge to the diagram node (e.g., Nets → Corks
  becomes Nets ⟿ Essay Ethics when Corks is folded inside)
- Two-user collaboration verified via Playwright (2 contexts, both
  "live", content persists across users). `jac` user added for
  manual testing.

---

## Eviction manifest

Junk entities created during prototype exploration (2026-04-12) when
the database was incorrectly assumed to be empty. These should be
evicted from XTDB when convenient. No general entity delete endpoint
exists in futon1a; eviction requires direct XTDB access.

| Entity ID | Name | Type | Notes |
|-----------|------|------|-------|
| `0c250a46-7a21-432b-9d95-eb40aae71730` | test-nema | article | First test POST |
| `1790ae53-ef33-44db-9e3a-7bd0b74bedc0` | arxana | article | Seed duplicate |
| `89ac6215-8b34-4801-86cd-defc767fba44` | nema | article | Seed duplicate |
| `c9513a39-1de3-49b6-9dd7-d17c7350a144` | scholium | article | Seed duplicate |
| `30d2e9eb-2379-4b90-b30e-4031dd67084d` | peer-learning | article | Seed duplicate |
| `8cfcc678-5735-400b-b386-cd1a19d12d27` | planetmath | article | Seed duplicate |
| `113dae85-442e-4704-9818-8b8b518703a4` | futon | article | Seed duplicate |
| `5bb1c9b2-e092-4d79-95b3-4b464a236af7` | webarxana | article | Seed duplicate |
| `1bc5e179-4392-4aa6-bddf-b09813d01c6d` | prelim-problem | question | Seed; source text dropped |
| `d69503fb-9040-478b-89ba-20c12c8ed9e1` | claim-density | claim | Seed; source text dropped |
| `45c529d2-8d91-4030-b271-0fc963e466b7` | evidence-archimedean | evidence | Seed duplicate |
| `2efe96f8-a345-4ccb-a16b-9c8322220e41` | props-test | article | Props round-trip test |

Also evict the 9 seed relations created between these entities, and
the junk relations created via the non-alpha `/relation` endpoint
(which stored docs with `:src`/`:dst` keys instead of
`:relation/src`/`:relation/dst`, invisible to ego queries).
