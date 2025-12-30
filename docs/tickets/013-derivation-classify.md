# Doc ticket: arxana-derivation--classify - Derivation type detection

## Code region
`dev/arxana-derivation.el:137-142` - `arxana-derivation--classify`

## What needs documenting
- The three derivation types: inclusion, transclusion, identification
- How text properties (`:scholia`) encode derivation metadata
- The priority order: transclusion > identification > inclusion
- How `arxana-derivation--region-has` scans for type markers
- Why default is `:inclusion`

## Context
When rendering derivation previews, the system needs to know what kind of derivative relationship exists. This classification drives highlighting and labeling.

## Suggested depth
Focused: explain the classification logic and what each type means.

## Expected word count
200-250 words
