# Doc ticket: Creating scholia about text regions

## Code region
`dev/arxana-scholium.el:47-51` - `arxana-scholium-compose-from-region`

## What needs documenting
- How to annotate a specific region of text
- The `make-scholium-about-part-of-current-article` underlying function
- How the region bounds are captured
- How this differs from whole-article scholia
- The passage endpoints in the resulting hyperedge

## Context
Region-specific scholia are more precise than whole-article annotations. This feature deserves its own explanation.

## Suggested depth
Focused: explain the region selection and what gets recorded.

## Expected word count
150-200 words
