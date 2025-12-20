Source of truth is `spine2.org`. Edit the literate source and re-tangle into
`dev/` + `test/`. Do not edit generated Elisp directly unless explicitly
requested for an emergency patch.

This project is primarily Lisp. Run `dev/check-parens.sh` (wraps `dev/check-parens.el`)
to quickly detect paren/quote mismatches before or after edits. Example:
`bash dev/check-parens.sh dev/arxana-patterns.el`.

## Development Cycle Notes

- The literate spine (`spine2.org`) is authoritative. Tangled output in `dev/`
  is generated and should not be edited by hand.
- Experimental work must still start in `spine2.org`; mark it clearly so it is
  easy to prune or promote later.

## Literate Emacs Lisp workflow

- Always edit `spine2.org` and re-tangle. Treat `dev/` + `test/` as generated.
- Any new namespace, public function, or data shape should be documented in the
  same literate section that defines it.
- Code meant for experiments only should be clearly marked in `spine2.org`.
- Emacs Lisp should avoid `seq` helpers unless you explicitly `(require 'seq)`
  (prefer plain list functions to prevent `void-function seq` errors).

## Tracking the literate backlog

- `dev/org-sync-tracker.org` is still used to flag any legacy/manual drift that
  needs reconciliation, but new work should already live in `spine2.org`.

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
