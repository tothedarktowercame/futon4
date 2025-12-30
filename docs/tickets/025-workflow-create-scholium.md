# Doc ticket: Workflow - Creating a scholium

## Code region
- `dev/arxana-scholium.el`
- `dev/arxana-store.el:345-369`

## What needs documenting
Step-by-step workflow:
1. Ensure source and target articles are registered
2. Use `arxana-scholium-compose` or `arxana-store-upsert-scholium`
3. Enter scholium content
4. Publish with C-c C-c
5. Verify the relation exists

Include:
- The difference between compose and upsert
- How to create scholia about regions
- How to view existing scholia (ego query)

## Context
Creating scholia is the core annotation workflow. Users need a clear guide.

## Suggested depth
Tutorial: step-by-step with commands and expected results.

## Expected word count
250-300 words
