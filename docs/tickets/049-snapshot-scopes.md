# Doc ticket: Snapshot scopes (all vs. latest)

## Code region
`dev/arxana-store.el:36-37, 195-216` - scope constants and handling

## What needs documenting
- What "all" scope means (complete history)
- What "latest" scope means (current state only)
- When to use each scope
- The prompt behavior for scope selection
- How scope affects snapshot size and restore behavior

## Context
Users need to understand the trade-off between complete history and current-state snapshots.

## Suggested depth
Focused: explain the two scopes and their implications.

## Expected word count
150-200 words
