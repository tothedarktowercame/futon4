# Doc ticket: Error handling strategy in arxana-store.el

## Code region
`dev/arxana-store.el:86-94` - `arxana-store--record-error` and related

## What needs documenting
- The design decision to never signal errors back to users
- The `arxana-store-last-error` plist structure (:reason, :detail, :context)
- The `arxana-store-last-failure` request/error pairing
- Why silent failures with logging was chosen over exceptions
- How to diagnose issues using `arxana-store-last-error`
- The ping and last-request-report diagnostic commands

## Context
This is a deliberate design choice that surprises newcomers. Callers expecting exceptions get nil instead. The rationale (graceful degradation) needs explanation.

## Suggested depth
Focused: explain the "why" clearly with examples of how to debug.

## Expected word count
250-300 words
