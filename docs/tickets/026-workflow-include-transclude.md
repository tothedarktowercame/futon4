# Doc ticket: Workflow - Including and transcluding text

## Code region
- `dev/arxana-inclusion.el`
- `dev/arxana-derivation.el`

## What needs documenting
Step-by-step workflow:
1. Register both source and target articles
2. Navigate to where you want to insert content
3. Use `include-article` or `transclude-article`
4. System checks article existence, inserts text
5. Derivation metadata is recorded
6. View derivation previews in Scholia Display

Include:
- The difference between include and transclude
- What happens when source article is missing (the guard)
- How derivation highlighting shows the relationship
- The hyperedge that gets created for provenance

## Context
Inclusion/transclusion is the Ted Nelson feature. Users need to understand the workflow and what artifacts are created.

## Suggested depth
Tutorial: step-by-step with visual explanation of the result.

## Expected word count
300-350 words
