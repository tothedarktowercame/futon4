# Mission: WebArxana — Collaborative Hypergraph Surface

**Date:** 2026-04-12
**Status:** IDENTIFY (2026-04-12)
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

### Out of scope (deferred)

- **Character-level annotation editor:** The Emacs chorus model
  (passage-level annotation with line-range references). Valuable
  but complex; may warrant its own mission.
- **Inline text editor (codemirror/prosemirror):** A rich editor
  alongside the graph. Explored if the card body proves too limited.
- **Hyperedge *creation* workflow:** Select source node(s), browse to
  sink, save annotation with roles. Procedurally complex; deferred
  but not ruled out.
- **Multi-user real-time sync:** The WebSocket infrastructure exists
  but is not yet wired for live co-editing. Current model: refresh to
  see changes.
- **Public access / deployment:** Currently localhost only.

## Completion criteria

1. A user can create a new freestanding node from the top bar,
   choosing name and type from the registered type system.
2. `+ Adjacent` prompts for name, type (defaulting to "article"),
   and relation type (defaulting to "arxana/scholium"), then creates
   both entities and the link.
3. Hop-depth is adjustable via +/- controls in the UI.
4. Each entity shows its author. New entities record the logged-in
   user as author.
5. An activity/recent-additions view shows what was added and by whom.
6. All of the above is covered by Playwright tests.

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
- **Backend:** futon1a (no changes planned; work within existing API)
- **Driver:** Joe + collaborators via WebArxana itself

---

## MAP

Survey questions for the MAP phase:

- Q1: What entity types and relation types are registered in
  futon1a's type system? Which are relevant for collaborative
  writing?
- Q2: How does author attribution currently work? Is there an
  `author` field on entities, or does it need to be added via the
  penholder/evidence system?
- Q3: What does futon1a's evidence timeline look like for recent
  writes? Can it serve as the activity feed, or does WebArxana need
  its own?
- Q4: What are the current limits of the entity write endpoint?
  (Known: `props` not persisted. What else?)
- Q5: How do the existing Emacs browser views handle hop-depth
  control and neighbourhood expansion?

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
