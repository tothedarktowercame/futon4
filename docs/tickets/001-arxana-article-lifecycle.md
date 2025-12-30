# Doc ticket: arxana-article.el - Article table and lifecycle

## Code region
`dev/arxana-article.el` - entire file (~70 lines)

## What needs documenting
- The in-memory `arxana-article--table` hash table and its role as the local article store
- How `make-current-buffer-into-article` registers a buffer as an article
- The relationship between article names, paths, and the canonical path cache
- Why this module provides fallback definitions (unless fboundp pattern)
- How `get-article` and `sch-plain-text` work together
- The metadata envelope structure and when it's used

## Context
This is the foundation of the Emacs-side article model. Users need to understand this before they can work with scholia, relations, or sync. The current futon4.org mentions article lifecycle but doesn't explain the in-memory table or the registration flow.

## Suggested depth
Mid-level: explain the data structures and the registration flow, with examples of typical usage patterns.

## Expected word count
300-400 words
