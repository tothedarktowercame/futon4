# Doc ticket: How scholia flow through the system

## Code region
- `dev/arxana-scholium.el` - authoring helpers
- `dev/arxana-store.el:345-369` - `arxana-store-upsert-scholium`

## What needs documenting
- What a scholium is (annotation/commentary on another article)
- The `new-scholium-mode` legacy flow
- The `arxana-scholium-compose` modern entry point
- How source and target articles are resolved to IDs
- The `arxana-store-upsert-scholium` API
- The relation type "arxana/scholium"
- How scholia appear in ego/cooccur queries

## Context
Scholia are central to Arxana's annotation model. Users creating commentary need this flow documented.

## Suggested depth
Mid-level: cover the authoring workflow and the storage.

## Expected word count
300-350 words
