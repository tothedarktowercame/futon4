# Mission: WebArxana — Collaborative Hypergraph Surface

**Date:** 2026-04-12
**Status:** IDENTIFY (2026-04-12). Scope revised: real-time sync,
deployment, and author lists moved in-scope.
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

### Out of scope (deferred)

- **Character-level annotation editor:** The Emacs chorus model
  (passage-level annotation with line-range references). Valuable
  but complex; may warrant its own mission.
- **Inline text editor (codemirror/prosemirror):** A rich editor
  alongside the graph. Explored if the card body proves too limited.
- **Hyperedge *creation* workflow:** Select source node(s), browse to
  sink, save annotation with roles. Procedurally complex; deferred
  but not ruled out.

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
