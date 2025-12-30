# Doc ticket: arxana-docbook-core.el - Docbook entry parsing

## Code region
`dev/arxana-docbook-core.el` - entire file (~210 lines)

## What needs documenting
- What a "docbook" is in Arxana context (not DocBook XML)
- The books root directory structure (`dev/logs/books/<book>/raw/*.json`)
- How entries are parsed from JSON into plists
- The stub file relationship (`stubs/<run-id>.org`)
- Lab draft detection via version field
- Content extraction: stripping headers, code blocks, metadata
- The `arxana-docbook-entries` public API
- Source path extraction from entry content

## Context
Docbook entries are auto-generated documentation stubs from AI sessions. This module parses them for browsing. It's distinct from the docbook browser UI.

## Suggested depth
Mid-level: explain the data model and directory structure clearly.

## Expected word count
300-350 words
