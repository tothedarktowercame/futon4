# Doc ticket: arxana-import.el - Org to XTDB import

## Code region
`dev/arxana-import.el` - entire file (~95 lines)

## What needs documenting
- The `arxana-import-org-file` function and its workflow
- How article names are derived from #+TITLE or filename
- The `arxana-import-org-directory` function for batch imports
- How imports connect to the article table and Futon store
- The recursive vs. non-recursive directory scanning

## Context
This is how users get existing Org content into the Arxana graph. It's a key onboarding path but not documented in futon4.org.

## Suggested depth
Light: focus on usage patterns and what happens when you import.

## Expected word count
200-250 words
