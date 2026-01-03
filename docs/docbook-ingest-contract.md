# Docbook ingest/retrieval contract (futon4 → futon1 XTDB)

This defines how filesystem-backed docbook entries (JSON + stubs) are ingested into futon1’s XTDB and how clients retrieve them. It mirrors the Patterns ingestion style (append-only, explicit lineage) and adds removal semantics.

## Data model (XTDB docs)

- **doc heading** (stable per spine heading)
  - `:doc/id` (string): stable hash of `book + outline_path`.
  - `:doc/book` (string): e.g., `"futon4"`.
  - `:doc/outline_path` (vector of strings): heading path from spine.
  - `:doc/path_string` (string): outline path joined with `" / "`.
  - `:doc/supersedes` (string, optional): prior `:doc/id` if heading moved/renamed.

- **doc entry** (append-only event)
  - `:doc/id` (string): heading id.
  - `:doc/entry-id` (string/uuid): unique per entry (use run-id or generated uuid).
  - `:doc/version` (string): commit/tag describing the code state.
  - `:doc/timestamp` (inst): UTC ISO-8601.
  - `:doc/status` (keyword): `:active` | `:removed` (removed hides the lineage by default).
  - `:doc/replaces` (string, optional): prior `:doc/entry-id` this supersedes.
  - `:doc/merges` (vector of strings, optional): entry-ids merged into this entry.
  - `:doc/links-to` (vector of `:doc/id`, optional): crossrefs/backlinks.
  - `:doc/files` (vector of strings): files touched/scope.
  - `:doc/commit` (string): commit SHA/tag.
  - `:doc/scope` (vector of strings, optional): coarse scopes (module names, etc.).
  - `:doc/tracker-refs` (vector of strings, optional): org-sync tracker ids/notes.
  - `:doc/context` / `:doc/delta` / `:doc/verification` (strings): distilled text blocks.
  - `:doc/toc-version` (string, optional): TOC version this entry was authored against.
  - `:doc/preferred` (boolean, optional): hint to pick this tip when forks exist.

- **doc link** (optional denormalized edge)
  - `:link/src-doc-id`, `:link/dst-doc-id`, `:link/kind` (e.g., `:xref`, `:see-also`).

## Ingest pipeline (filesystem → XTDB)

1. Source: `data/logs/books/<book>/raw/*.json` + stubs for display.
2. For each JSON, derive:
   - heading doc: upsert `:doc/id`, `:doc/book`, `:doc/outline_path`, `:doc/path_string`.
   - entry doc: `:doc/entry-id` = `run_id` or generated; map JSON fields to `:doc/*` keys above.
3. Append-only: never delete; re-ingesting same `:doc/entry-id` is idempotent (same content).
4. Removal: an entry with `:doc/status :removed` marks its lineage non-visible in “latest” view.
5. TOC: ingest `data/logs/books/<book>/toc.json` as `doc heading` docs; link `:doc/toc-version` on entries if present.
6. Transactions: use `xtdb.api/submit!` with `::xtdb/put` for heading + entry docs; optional link docs for crossrefs.

## Retrieval semantics

- **Latest per heading**: for each `:doc/id`, pick the maximal entry by `timestamp/version` where `status = :active` and not `replaced` by another entry; if forks, prefer `:doc/preferred` else max `(timestamp, version)`. If the latest is `:removed`, treat as absent by default.
- **Lineage**: follow `:doc/replaces` / `:doc/merges` / `:doc/supersedes` to show history, including removed entries when explicitly requested.
- **Time travel**: use XTDB bitemporal queries to pull “latest as-of <t>”.
- **By file/scope**: query entries where `:doc/files` contains target or `:doc/scope` matches.
- **Crossrefs**: resolve `:doc/links-to` to current tips (follow `:supersedes` if targets moved).

## Suggested XT queries

- Latest view:
  - Find entries with `:doc/id = ?id`, `:doc/status = :active`; exclude any whose `:doc/entry-id` appears in another entry’s `:doc/replaces`/`:doc/merges`; pick max `(timestamp, version, preferred)`.
- Lineage for heading:
  - Walk from latest backward via `:doc/replaces`/`:doc/merges`; include `:doc/status` for removed markers.
- Contents (TOC):
  - Pull all `doc heading` docs for `:doc/book = ?book`, joined with latest-entry counts/status for badges.
- Recent:
  - Sort active entries by `:doc/timestamp` desc; optionally filter by `:doc/book`.

## Viewer expectations

- Contents tab: show headings with badge (has tip? count? removed?) from latest view + TOC.
- Section view: list entries for a heading (latest first), show lineage indicator; hide removed by default.
- Recent tab: latest active entries, sortable.

## API surface (proposed)

- `/docs/:book/contents` → TOC + tip metadata.
- `/docs/:book/heading/:doc-id` → latest entry + lineage summary.
- `/docs/:book/recent?limit=n` → latest entries.
- `/docs/entries?file=...` → entries touching a file/scope.
- Optional: `/docs/lineage/:doc-id` to render full chain (including removed when `?include_removed=true`).

## Notes on media/patterns parity

- Patterns already use append-only entity/relation docs; this doc contract mirrors that style.
- Media could reuse the same pattern: `media entry` with `status`, `replaces`, `files`, and an optional `:media/id`; removed works the same (latest hides removed chains).
