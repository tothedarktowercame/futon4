# Doc ticket: Workflow - Importing from Org

## Code region
`dev/arxana-import.el`

## What needs documenting
Step-by-step workflow:
1. Use `arxana-import-org-file` for single file
2. Or `arxana-import-org-directory` for batch
3. System extracts title (or uses filename)
4. Creates article in local table
5. Syncs to Futon if enabled

Include:
- How article names are derived
- The recursive option for directories
- What happens to existing articles with same name
- Verifying imports worked

## Context
Import is the onboarding path for existing Org content.

## Suggested depth
Tutorial: step-by-step with verification.

## Expected word count
200-250 words
