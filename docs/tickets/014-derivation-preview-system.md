# Doc ticket: The derivation preview system

## Code region
`dev/arxana-derivation.el` - entire file (~430 lines)

## What needs documenting
- What derivation previews show (passages pulled in via inclusion/transclusion)
- The three custom faces and their colors
- The toggle customizations for each derivation type
- How previews are collected via `mark-things-up-hook`
- The collapsible preview block in Scholia Display
- Highlight overlays on source text
- The toggle/isolate commands for each type
- The preview state hash table for expansion tracking

## Context
This is the visual feedback system that lets users audit which passages were pulled into an article and how. It's mentioned in futon4.org but deserves its own detailed section.

## Suggested depth
Mid-level: explain the user-facing features and how to customize them.

## Expected word count
350-400 words
