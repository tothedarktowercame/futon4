# Doc ticket: Relations ego display

## Code region
`dev/arxana-relations.el:92-115` - `arxana-relations--render-ego`

## What needs documenting
- What "ego" means (the focal entity and its neighbors)
- How links are split into outgoing vs. incoming
- The direction indicator parsing (`:out` vs `:in`)
- Entity name and type formatting
- The relation type label extraction
- How the buffer is structured (Ego header, then sections)

## Context
The ego view is the primary way users explore the graph neighborhood of an article. Understanding its structure helps users navigate relations.

## Suggested depth
Focused: explain the display format and what users can learn from it.

## Expected word count
200-250 words
