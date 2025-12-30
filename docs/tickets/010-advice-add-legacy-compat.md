# Doc ticket: The advice-add pattern for legacy compatibility

## Code region
- `dev/arxana-compat.el` - entire file
- `dev/arxana-saving.el:96-98` - advice installation

## What needs documenting
- Why Arxana uses advice-add instead of direct redefinition
- The `with-eval-after-load 'arxana-tangled` pattern
- How `arxana-compat--orig-get-article` preserves the original
- The "modern first, legacy fallback" lookup strategy
- When and why new modules should use this pattern
- Trade-offs: flexibility vs. debugging complexity

## Context
Contributors modifying legacy functions need to understand this pattern to avoid breaking the compatibility layer.

## Suggested depth
Focused: explain the pattern with rationale and guidance for contributors.

## Expected word count
250-300 words
