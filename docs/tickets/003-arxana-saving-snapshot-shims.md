# Doc ticket: arxana-saving.el - Snapshot save/restore shims

## Code region
`dev/arxana-saving.el` - entire file (~100 lines)

## What needs documenting
- How legacy `save-all-scholia` and `read-scholia-file` commands are intercepted
- The `arxana-saving--snapshot-enabled-p` gate and why it matters
- How scope selection works (all vs. latest)
- The around-advice pattern and why it's used for backward compatibility
- The fallback behavior when sync is disabled
- Integration with Futon's `/snapshot` and `/snapshot/restore` endpoints

## Context
This bridges the historical file-based save/restore with modern XTDB snapshots. Contributors need to understand that calling the legacy commands now routes through Futon when sync is enabled.

## Suggested depth
Mid-level: explain the interception pattern and the snapshot flow.

## Expected word count
250-300 words
