# Doc ticket: arxana-lab.el - Lab notebook browser

## Code region
`dev/arxana-lab.el` - entire file (~390 lines)

## What needs documenting
- The lab directory structure (`data/logs/lab/raw`, `data/logs/lab/stubs`, `data/logs/lab/trace`, `data/logs/lab/doc-drafts`)
- How lab entries differ from docbook entries
- The session-id based organization
- JSON parsing and message extraction
- The timeline view with user/assistant messages
- How traces, stubs, and drafts relate to each other
- The various viewer functions: stub, raw, trace, draft

## Context
The lab notebook captures AI coding sessions. Users can browse session history, view message timelines, and access raw data. This is separate from docbook but shares some patterns.

## Suggested depth
Mid-level: explain the directory layout and the different views available.

## Expected word count
350-400 words
