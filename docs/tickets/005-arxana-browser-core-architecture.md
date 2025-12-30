# Doc ticket: arxana-browser-core.el - Browser architecture

## Code region
`dev/arxana-browser-core.el` - entire file (~935 lines)

## What needs documenting
- The browser stack model (`arxana-browser--stack`) and context tracking
- How `arxana-browser--visit` dispatches to different item types
- The view system (patterns, code, media, docbook, lab, etc.)
- Row rendering and format definitions for each view type
- Navigation: wheel, keyboard, trackpad support
- The click sound feature and its configuration
- How `arxana-browser--render` rebuilds the tabulated list
- Mark/selection tracking across views

## Context
This is the central navigation hub for the entire system. Users interact with patterns, docbook entries, lab notebooks, and media through this single browser interface. The current docs don't explain the stack-based navigation or how different views work.

## Suggested depth
Deep: this is a large, complex module that orchestrates many subsystems. Include a conceptual overview of the navigation model.

## Expected word count
500-600 words
