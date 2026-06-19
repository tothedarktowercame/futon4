Sources of truth are split: code lives in `dev/` + `test/`, while docs live in
XTDB. Filesystem doc snapshots may be created for editing or LLM inspection,
but treat them as disposable unless called out otherwise. Avoid editing
`arxana-tangled.el` directly unless explicitly requested for an emergency fix.

This project is primarily Lisp. Run `dev/check-parens.sh` (wraps `dev/check-parens.el`)
to quickly detect paren/quote mismatches before or after edits. Example:
`bash dev/check-parens.sh dev/arxana-patterns.el`.

## Development Cycle Notes

- `dev/` + `test/` are canonical for code. Keep docs in XTDB aligned with the
  current behavior and public API surface.
- Avoid duplicate implementations: search for existing helpers before adding
  new ones; extend or relocate code instead of cloning logic.
- File size guardrail: any single code file over 50K is a warning; over 100K is
  a breaking violation and must be refactored.
- Keep modules small and focused: prefer new, well-scoped files over growing
  large ones; add clear section headers, docstrings, and a `defgroup` per module.
- Maintain a single entry point per subsystem; keep UI wrappers thin and route
  behavior through shared core helpers.
- When moving code, remove or alias old entry points immediately and update
  tests/docs in the same change.
- If you create a filesystem doc snapshot, mark it as temporary or archival so
  it is easy to prune or promote later.
- Any new namespace, public function, or data shape should be documented in the
  matching XTDB doc entry for that subsystem.
- Emacs Lisp should avoid `seq` helpers unless you explicitly `(require 'seq)`
  (prefer plain list functions to prevent `void-function seq` errors).

## Tracking doc/code alignment

- `dev/docs-backlog.org` flags drift between `dev/` code and XTDB-backed
  docs, plus any temporary filesystem snapshots that still need ingesting.

## Ingesting essays into Arxana Essays

- **Read `README-essays.md` before ingesting or annotating an essay.** It
  spells out the file-role convention that is easy to get wrong: the source
  `.md` is canonical and read-only (an annotated edition is a *render*, never
  blockquotes baked into the body); `annotations.edn` is the authoritative
  annotation layer with **verbatim** `:passage` anchors; `annotations.el` is a
  thin sections-only manifest that the loader augments from the `.edn`.
- Section headings in the source must be flat `##`; logical nesting lives in
  the manifest's `:index`, not in `#`/`###` markdown depth.
- Verify every ingest with `M-x arxana-browser-essays-audit-passages`, which
  reports per-section whether each annotation's passage actually anchors.

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

## Documentation Tickets

`docs/tickets/` contains 50 documentation tickets specifying gaps to fill. Each includes:
- Code region to document
- What needs explaining
- Suggested depth and word count

Priority: Critical Gaps > Design Decisions > Workflows > API Details

## Rename/Refactor Convention

When renaming or moving functions, log the change to help maintain documentation links:

```
# In CHANGELOG.md or a dedicated refactor-log.md
- YYYY-MM-DD: `old-function-name` → `new-function-name` (file.el)
- YYYY-MM-DD: `function-name` moved from file-a.el to file-b.el
```

## Literate Codebase Vision

- Documentation and code live separately (no #+INCLUDE embedding)
- Hyperedges connect doc sections to code spans (Ted Nelson-style transclusion)
- Think of code as terrain, documentation coverage as a heat map
- AI agents help detect link drift and suggest repairs

## Pending coordination

- **S — 2026-05-21 — VSATARCs R12 follow-on, two pieces deferred** (claude-4 v0.5.31).
  The narrow R12 take-up consumption site landed (`dev/arxana-vsatarcs-intrinsic-values.el` + `dev/arxana-vsatarcs-efe.el` wrapping `--g-pragmatic` with the `(credit / 0.5)` modulator). Two follow-on pieces are needed for learned values to be meaningful:
  1. **Consent-decision stream from `dev/arxana-vsatarcs-consent.el` (claude-2's module) to futon1a XTDB.** Each `:confirm` / `:reject` / `:ignore` / `:abstain-for-now` dispatch should emit a hyperedge of a new type (suggested: `code/v05/vsatarcs-consent-decision`) carrying `{:writer-action-class :writer-event-id :decision :timestamp :source}`. claude-4 did NOT modify `consent.el` directly per the multi-agent coordination convention; this wants claude-2 or a fresh session with that ownership.
  2. **The elisp outer-loop entry point.** A new module (e.g. `dev/arxana-vsatarcs-outer-loop.el`) or sibling `.bb`/`.clj` script that reads consent-decision hyperedges in a sliding window (default 14 days per claude-9's WM-side §13), computes per-class Beta updates via `arxana-vsatarcs-intrinsic-values-next-record`, persists via `arxana-vsatarcs-intrinsic-values-persist-record!`, calls `apply-update!`. Sliding-window-from-scratch per claude-9's design-choices §13 (back-to-back runs must produce identical output to avoid double-counting conjugate updates).
  Cross-pointers: `docs/vsatarcs-alignment-completeness.aif.edn:1584` (R12 audit row + `:v0.5.31-closure` text); `~/code/futon0/holes/missions/M-the-futon-stack-Q6-r12-design-choices.md` (claude-9's per-DERIVE-fork record); `~/code/futon0/holes/handoffs/r12-to-stack-Q6-2026-05-21.md` (original handoff naming both pieces).
