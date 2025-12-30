# Doc ticket: arxana-store--request - Core HTTP helper

## Code region
`dev/arxana-store.el:135-193` - `arxana-store--request` function

## What needs documenting
- The sync-enabled check at entry
- URL building with base URL, path, and query string
- The localhost → 127.0.0.1 fallback mechanism
- Request metadata tracking (`arxana-store-last-request`)
- HTTP response parsing and JSON extraction
- Error handling: quit, connection, protocol errors
- The timeout mechanism (`arxana-store-request-timeout`)
- Why it returns parsed JSON or nil (never throws)

## Context
This is the single point where all Futon HTTP traffic flows. Understanding it helps debug any sync issues.

## Suggested depth
Mid-level: explain the request flow and error scenarios.

## Expected word count
300-350 words
