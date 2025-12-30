# Doc ticket: Workflow - Registering and syncing an article

## Code region
- `dev/arxana-article.el:41-48`
- `dev/arxana-store.el:218-229`

## What needs documenting
Step-by-step workflow:
1. Open a buffer with content
2. Call `make-current-buffer-into-article` with a name
3. System registers in local hash table
4. If sync enabled, POST to /entity
5. Verify with `arxana-store-ping` or curl

Include:
- Prerequisites (futon4-base-url, futon4-enable-sync)
- Interactive vs. programmatic registration
- What to check if sync fails
- How to verify the entity exists in Futon

## Context
This is the first thing new users need to do. A clear workflow guide is essential.

## Suggested depth
Tutorial: step-by-step with expected outputs.

## Expected word count
250-300 words
