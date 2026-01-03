# Doc ticket: docbook entry_id format suffix

## Code region
- `dev/arxana-docbook-checkout.el` (stub payload + sync)
- `dev/arxana-docbook-remote.el` (remote entry normalization)
- `dev/arxana-docbook-core.el` (entry/stub parsing)

## What needs documenting
- The difference between `doc_id` (stable doc identity) and `entry_id` (versioned entry)
- Why `entry_id` includes a format suffix like `::org`
- How format suffixes relate to docbook ingestion and export pipelines
- How clients should treat `entry_id` vs `doc_id` when linking or syncing
- Filesystem naming conventions for docbook working sets

## Context
Users see `entry_id` values like `futon4-2c09c5856f2e::org` in API responses and
may assume it is an error. We should document that the suffix is intentional and
represents the payload format/version, while `doc_id` remains stable.

Filesystem snapshots now keep one stub per doc in
`docs/docbook/<book>/<doc_id>.org`. The stub's property drawer records
both `:DOC_ID:` and `:ENTRY_ID:` so the stable identity and the versioned entry
remain visible even though the filename does not include the suffix. This
should be called out explicitly so operators do not expect `::org` on disk, and
so cleanup tools know they can prune older `*::org.org` stubs.

## Suggested depth
Focused: explain the semantics in 1–2 short sections with an example and a
recommended usage pattern.

## Expected word count
200–250 words
