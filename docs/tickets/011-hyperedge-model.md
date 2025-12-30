# Doc ticket: The hyperedge model and endpoint structure

## Code region
- `dev/arxana-store.el:320-343` - hyperedge payload and posting
- futon4.org lines 324-325 (the `/hyperedge` entry point)

## What needs documenting
- What a hyperedge is vs. a simple relation
- The `:hx/type` and `:hx/endpoints` structure
- Role values: `:role/source`, `:role/target`, `:role/passage`
- How inclusion/transclusion/identification are encoded
- The passage endpoint with article, begin, end
- Props like `label` and `scholium/type`
- When to use hyperedges vs. simple relations

## Context
Hyperedges are the powerful multi-endpoint relations that enable inclusion, transclusion, and provenance tracking. This is core to the Ted Nelson vision but not well explained.

## Suggested depth
Deep: this is a key concept. Include examples of different hyperedge types.

## Expected word count
400-450 words
