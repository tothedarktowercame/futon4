# Doc ticket: Pattern library ingestion

## Code region
`dev/arxana-patterns-ingest.el`

## What needs documenting
- What ingestion does (flexiarg → Futon entities)
- The `arxana-patterns-ingest-directory` command
- How pattern languages are created/updated
- The `/ego` lookup registration for consistency
- The `arxana-patterns-list-languages` command
- The language rows structure

## Context
Ingestion is how filesystem patterns become queryable in Futon. It's the bridge from static files to live graph.

## Suggested depth
Mid-level: explain the ingestion flow and commands.

## Expected word count
250-300 words
