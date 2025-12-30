# Doc ticket: Browser stack-based navigation model

## Code region
- `dev/arxana-browser-core.el:161-173` - stack and context variables
- `dev/arxana-browser-core.el:764-778` - `arxana-browser--up`

## What needs documenting
- The `arxana-browser--stack` as a list of context plists
- What a context plist contains (`:view`, `:type`, `:label`, view-specific fields)
- How push/pop works for drill-down and back navigation
- The `arxana-browser--context` relationship to the stack
- The "permanent-local" property and why it's needed
- Keyboard shortcuts: RET/right to drill, left/b to back
- How the header line reflects current context

## Context
Users need to understand the stack metaphor to navigate effectively. It's like an iPod click-wheel interface.

## Suggested depth
Focused: explain the mental model with navigation examples.

## Expected word count
250-300 words
