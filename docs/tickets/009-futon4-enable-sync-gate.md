# Doc ticket: The futon4-enable-sync gating mechanism

## Code region
`dev/arxana-store.el:18-19, 56-59` - `futon4-enable-sync` and `arxana-store-sync-enabled-p`

## What needs documenting
- What `futon4-enable-sync` controls (all network traffic to Futon)
- Why a global gate exists (offline work, testing, gradual adoption)
- The `arxana-store-sync-enabled-p` predicate and where it's checked
- The `arxana-store-ensure-sync` interactive enablement
- The `arxana-store-auto-start-server` option
- How different modules respect (or should respect) this flag

## Context
Every store operation checks this flag. Users need to understand that setting it to nil makes the system work purely locally.

## Suggested depth
Focused: explain the flag's purpose and how to use it.

## Expected word count
200-250 words
