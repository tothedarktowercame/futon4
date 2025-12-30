# Doc ticket: arxana-inclusion.el - Include/transclude guards

## Code region
`dev/arxana-inclusion.el` - entire file (~60 lines)

## What needs documenting
- Why these guards exist (preventing silent "nil" insertions)
- The `arxana-inclusion--require-article` function and its error messaging
- How `include-article` and `transclude-article` differ
- The advice wrapping pattern used with `arxana-tangled`
- The user experience when an article hasn't been registered

## Context
Without this module, users would get confusing silent failures when trying to include text from unregistered articles. This is a defensive layer that makes Arxana more user-friendly but isn't mentioned in the current docs.

## Suggested depth
Light: focus on the user-facing behavior and the "why" rather than implementation details.

## Expected word count
200-250 words
