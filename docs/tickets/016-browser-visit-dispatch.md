# Doc ticket: Browser visit dispatch logic

## Code region
`dev/arxana-browser-core.el:584-697` - `arxana-browser--visit`

## What needs documenting
- The item type dispatch (pcase on `:type`)
- How each type is handled: menu, language, collection, pattern, media-*, docbook-*, lab-*, info
- The stack push pattern for drilling down
- How patterns open via `arxana-browser-patterns-open`
- Media playback triggering
- The error case for unknown types
- Why some types push context while others open editors

## Context
This is the central "what happens when you press RET" logic. Understanding the dispatch helps users predict browser behavior.

## Suggested depth
Mid-level: document each item type and its action.

## Expected word count
300-350 words
