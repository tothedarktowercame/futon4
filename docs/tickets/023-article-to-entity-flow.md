# Doc ticket: How articles become Futon entities

## Code region
- `dev/arxana-store.el:218-251` - `arxana-store-ensure-article` and `arxana-store-ensure-entity`
- `dev/arxana-article.el:41-48` - `make-current-buffer-into-article`

## What needs documenting
- The local registration step (hash table entry)
- The remote registration step (POST /entity)
- How article IDs are derived (`futon4--article-id-for`)
- The entity payload structure: name, type, id, path, props
- When registration happens automatically vs. manually
- The spine and props optional fields

## Context
The bridge between "I have a buffer" and "Futon knows about this article" is crucial for understanding the system.

## Suggested depth
Mid-level: explain both local and remote registration.

## Expected word count
250-300 words
