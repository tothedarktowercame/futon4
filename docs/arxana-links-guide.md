# Arxana Links Guide

## Purpose

Arxana Links persists the *rules* for hyperlinking (strategies) and the
*signals* around them (surface forms, scholia, embeddings).  Links are
recomputed from strategies so the system remains resilient as code and
docs evolve.

## Prerequisites

- Load dev modules: `M-x arxana-load` or `load-file` on `dev/bootstrap.el`.
- A Futon server if you want persistence.
- `futon4-enable-sync` set to `t` for persistence.

```elisp
(load-file "/path/to/futon4/dev/bootstrap.el")
(setq futon4-enable-sync t)
```

## Concepts

- Tier 0 (Ephemeral): computed links, no persistence.
- Tier 1 (Strategy): persist finder rules; links can be recomputed.
- Tier 2 (Anchored): persist anchored regions for immutable or historic text.

## Common Workflows

### Check status

```elisp
M-x arxana-links-status
```

### Create or load a strategy

```elisp
M-x arxana-links-demo-create-strategy
M-x arxana-browser-code-strategy-status
```

The demo uses the futon4 repo and a definition-based finder.  It is safe
to run multiple times because the strategy ID is deterministic.

### Create a resilient scholium

1. Select a region in a buffer.
2. Run `M-x arxana-links-demo-create-scholium`.
3. Enter the annotation text.

Anchors use content hashes and surrounding context so they survive minor
edits.

### Capture a surface form

1. Select a phrase.
2. Run `M-x arxana-links-capture-surface-form`.
3. Provide the concept ID.

Surface forms inform finders and auto-linking later on.

### Verify anchors

```elisp
M-x arxana-links-demo-verify-anchor
```

This is a local demonstration of the three-step re-finding algorithm
(offset check, context search, fuzzy context).

### Embedding neighbors for patterns

```elisp
M-x arxana-browser-patterns-embedding-status
M-x arxana-browser-patterns-show-neighbors
```

Embedding neighbors require a cached embedding space (persisted as an
embedding cache entity).

## Persistence Entities

- Strategy: `arxana/link-strategy`
- Voiced Link: `arxana/voiced-link`
- Surface Form: `arxana/surface-form`
- Scholium: `arxana/scholium`
- Embedding Cache: `arxana/embedding-cache`

Use `arxana-links-make-*` + `arxana-links-persist-*` to create and save
these records.  Use the `arxana-links-load-*` helpers to retrieve them.
Payloads are stored in Futon entities with the full record serialized
into the `source` field so they round-trip through the existing API.

## Testing

### Unit tests (Emacs only)

```bash
bash dev/run-tests.sh
```

### Integration tests (real Futon server)

The links test suite includes integration tests tagged `:futon` that run
only when a Futon server responds to `/types`.

```bash
bash dev/run-tests.sh
```

Set the base URL with either `futon4-base-url` or `FUTON4_BASE_URL` if the
server is not on `http://localhost:8080`.

When the server is reachable, the tests exercise persistence for:
- strategies
- voiced links
- surface forms
- scholia
- embedding caches
- demo strategy creation

## Quick Reference

- `arxana-links-demo-create-strategy`
- `arxana-links-demo-create-scholium`
- `arxana-links-demo-verify-anchor`
- `arxana-links-capture-surface-form`
- `arxana-links-status`
- `arxana-browser-code-strategy-status`
- `arxana-browser-code-reset-strategy`
