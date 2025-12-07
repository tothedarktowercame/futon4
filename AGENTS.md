Current work in progress is in arxana/latex/org/ directory you can
IGNORE other directories.

## Development Cycle Notes

- Phase 1 (current): revive the Emacs client pragmatically. New store, scholium,
  and relation code lives in `arxana/dev/` + `arxana/test/` so we can iterate
  quickly without reworking the entire spine.
- Phase 2 (after the reanimation milestones): fold the production-ready modules
  back into the literate Org sources so the tangled output remains the single
  source of truth. That refactor will happen once the plan is complete and the
  docs/tests are stable.

* Logical model

Arxana manages a single logical hypergraph. The core primitives are:

- Nema :: Universal node record with stable id, labels, optional endpoints, and payload.
- Article :: A nema labeled :label/article, holding text and core metadata.
- Metadata scholium :: A nema paired 1–1 with an article, caching backlinks, labels, and indexes.
- Event (hyperedge) :: A nema representing a relation or annotation, with :hx/type and N endpoints.
- Plexus :: A nema that names a working set or profile and carries configuration.

Classic Arxana entities, relations, and scholia embed cleanly:

- Entities → article nemas with derived identifiers.
- Relations → 2-end events (:hx/*) between nemas.
- Scholia → events and/or metadata nemas attached to articles.
- Inclusion / clusion / provenance → multi-end events with appropriate :role values.

** Storage substrates

All storage backends implement the same logical model.

*** Current target for storage: EAV in Clojure (Datascript + XTDB) via an HTTP API

Instead of going the triple route, all storage backends in the current
‘beta’ generation converge on an open **entity–attribute–value (EAV)**
model.  Each record—whether an article, event, or plexus—is
represented as a set of attribute–value pairs attached to a unique
entity id.  This design keeps the schema *open*, so new attributes or
link types can appear dynamically without migration.

The canonical implementation uses:

- **Datascript** for in-memory, client-side operation.  
  - Pure Clojure; lightweight and immutable.  
  - Supports live queries, undo/redo, and fast incremental updates.  
  - Ideal for interactive use inside Emacs or JVM clients.

- **XTDB** for durable, time-traveling storage.  
  - Schemaless documents mirror the same EAV keys verbatim.  
  - Transactions are append-only; every version of the graph is queryable.  
  - Perfect for provenance, journaling, and long-term archives.

Together these give Arxana an *open-schema substrate*: a graph database where nemas, events, and
plexuses are all entities, and attributes such as `:hx/type`, `:article/text`, or `:plexus/members`
are just first-class keys.  Queries can run over the generic EAV view, or use higher-level helpers
that expose structured projections (e.g., “articles”, “hyperedges”, “plexuses”).

> **Design posture:** Start open (EAV); specialize only when scale or semantics demand it.  
> Structured views—relational tables, triple stores, materialized indexes—can be layered on
> demand without altering the underlying data model.

This approach keeps computational properties stable: query complexity
and transaction semantics stay identical whether data are viewed as
triples, hash maps, or SQL rows.  It also makes cross-substrate
replication trivial: each backend stores the same `(entity attribute
value)` facts, differing only in query language and persistence model.

** Emacs client (Arxana classic)

The Emacs Lisp implementation in this repository provides:

- Interactive creation and editing of articles and scholia.
- Browsing modes over the in-memory graph.
- Operations for inclusion, derivation, and provenance as events.
- Import/export for specific document formats (historically LaTeX/Org).

Internally, it maintains the logical model in memory and (optionally)
syncs with external substrates (files, SQL backend).
