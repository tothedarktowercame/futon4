# Doc ticket: How Emacs to Futon sync works

## Code region
- `dev/arxana-store.el` - the store helpers
- `dev/arxana-article.el` - article registration

## What needs documenting
- The sync flow: local article table → Futon HTTP API → XTDB
- When sync happens (article registration, scholium creation, relation posting)
- The entity/relation/hyperedge endpoints and their roles
- What data is sent vs. kept local
- The canonical path normalization
- How failures are handled (silent, logged to last-error)
- The X-Profile header for multi-profile setups

## Context
Users need a mental model of what flows where. This is the integration story.

## Suggested depth
Overview: draw the data flow picture, then point to specific functions.

## Expected word count
350-400 words
