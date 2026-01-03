# Arxana Links Guide

This guide covers the `arxana-links.el` module, which implements Ted Nelson-style
hyperlinks with multiple persistence tiers.

## Overview

Arxana Links provides a flexible linking system that supports:

- **Ephemeral links** (Tier 0): Computed on-the-fly, no persistence
- **Strategy-bound links** (Tier 1): Persist finder rules, links are recomputable
- **Anchored links** (Tier 2): Persist exact regions for immutable content

The key insight is: **persist strategies, not individual links**. This allows links
to survive as code and documentation evolve.

## Prerequisites

1. Futon4 loaded via `dev/bootstrap.el`
2. Futon1 server running (for persistence features)
3. `futon4-enable-sync` set to `t`

```elisp
;; In your init or *scratch*
(load-file "/path/to/futon4/dev/bootstrap.el")
(setq futon4-enable-sync t)
```

## Quick Start

### Check Status

```elisp
M-x arxana-links-status
```

This shows whether sync is enabled and lists any persisted strategies.

### Create a Link Strategy

A strategy declares *how* to find links, not the links themselves:

```elisp
M-x arxana-links-demo-create-strategy
```

This creates a strategy for the futon4 repo that:
- Treats function definitions (defun, defvar, etc.) as linkable terms
- Auto-links when those symbols appear in documentation

### Create a Resilient Scholium

1. Select a region of text in any buffer
2. Run `M-x arxana-links-demo-create-scholium`
3. Enter your annotation text

The scholium is anchored using content hash + surrounding context, so it can
survive minor edits to the document.

### Verify Anchor Resilience

```elisp
M-x arxana-links-demo-verify-anchor
```

This demonstrates that anchors can be re-found even after text is inserted
before them.

### Capture a Surface Form

When you want to record that a phrase refers to a concept:

1. Select the phrase (e.g., "commutative thingy")
2. Run `M-x arxana-links-capture-surface-form`
3. Enter the concept ID (e.g., "abelian-group")

This feeds the open-world ingest system, allowing future auto-linking of
alternative phrasings.

## Data Types

### Link Strategy

Declares finder rules within a scope:

```elisp
(arxana-links-make-strategy
 :scope '(:repo "futon4"
          :code-roots ("dev/" "test/")
          :docbook "futon4")
 :finders '((:type :symbol-as-term :auto-link? t)
            (:type :filename-mention :auto-link? t)))
```

**Finder types:**
- `:symbol-as-term` - Function/variable names become linkable terms
- `:filename-mention` - File names in docs link to files
- `:embedding-proximity` - Vector space neighbors
- `:explicit` - User-created links

### Voiced Link

A promoted link that can have annotations:

```elisp
(arxana-links-make-voiced-link
 :source '(:type :code-symbol
           :file "dev/arxana-store.el"
           :symbol "arxana-store-fetch-entity")
 :target '(:type :doc-paragraph
           :docbook "futon4"
           :doc-id "futon1-xyz")
 :found-by "strategy:futon4-abc"
 :status :confirmed)
```

**Status values:** `:confirmed`, `:suppressed`, `:orphaned`, `:fuzzy-matched`

### Surface Form

Records alternative phrasings for concepts:

```elisp
(arxana-links-make-surface-form
 :concept-id "abelian-group"
 :surface "commutative thingy"
 :context '(:doc "my-notes" :snippet "...the commutative thingy...")
 :source :explicit)
```

### Resilient Anchor

Content-addressable region reference:

```elisp
;; Create from buffer region
(arxana-links-make-anchor (current-buffer) start end)

;; Returns:
;; (:strategy :content-hash
;;  :content-hash "sha256:abc..."
;;  :content-length 47
;;  :context-before "...preceding text..."
;;  :context-after "...following text..."
;;  :hint-offset 1234
;;  :hint-line 42)
```

### Scholium

Annotation attached to a region:

```elisp
(arxana-links-make-scholium
 :target-doc "some-article"
 :anchor <anchor-from-above>
 :content "My annotation"
 :content-type "text/plain")
```

### Embedding Cache

Cached nearest-neighbors from vector spaces:

```elisp
(arxana-links-make-embedding-cache
 :space "patterns-glove"
 :model "glove"
 :dimensions 50
 :neighbors '(("pattern-a" . ((:id "pattern-b" :score 0.95)
                              (:id "pattern-c" :score 0.91)))
              ("pattern-b" . ((:id "pattern-a" :score 0.95))))
 :k 10
 :threshold 0.8)
```

## Anchor Re-finding Algorithm

When loading a scholium, the system tries three strategies to re-find the region:

1. **Exact offset**: Try the hint offset, verify content hash
2. **Context pattern**: Search for context-before + content + context-after
3. **Fuzzy context**: Search for context strings, estimate region

If all strategies fail, the scholium is marked `:orphaned` and can be manually
re-anchored.

## Programming API

### Strategy Management

```elisp
;; Create and persist
(arxana-links-persist-strategy strategy)

;; Load all strategies
(arxana-links-load-strategies)

;; Find strategy for a repo
(arxana-links-find-strategy "futon4")
```

### Voiced Links

```elisp
;; Create and persist
(arxana-links-persist-voiced-link link)

;; Mark as false positive
(arxana-links-suppress-link "link:abc123")

;; Load links (optionally by strategy)
(arxana-links-load-voiced-links "strategy:futon4-xyz")
```

### Scholia

```elisp
;; Create anchor and scholium
(let* ((anchor (arxana-links-make-anchor buf start end))
       (scholium (arxana-links-make-scholium
                  :target-doc "doc-name"
                  :anchor anchor
                  :content "annotation")))
  (arxana-links-persist-scholium scholium))

;; Load scholia for a document
(arxana-links-load-scholia-for-doc "doc-name")

;; Verify a scholium's anchor
(arxana-links-verify-scholium buffer scholium)

;; Re-find an anchor
(arxana-links-find-anchor buffer anchor) ; => (start . end) or nil
```

### Embedding Neighbors

```elisp
;; Compute neighbors from embeddings hash table
(arxana-links-compute-neighbors embeddings-table k threshold)

;; Get neighbors for an item
(arxana-links-get-neighbors "patterns-glove" "pattern-id" 5)
```

## Configuration

```elisp
;; Characters of context to capture around anchored regions
(setq arxana-links-context-chars 30)

;; Default neighbors per item in embedding caches
(setq arxana-links-default-neighbor-k 10)

;; Default minimum similarity for neighbors
(setq arxana-links-default-similarity-threshold 0.8)
```

## Resilient Scholia Integration

The `arxana-scholium.el` module now supports resilient anchoring for region-based
annotations. These scholia survive minor edits using content-hash + context matching.

### Creating a Resilient Scholium

1. Select a region of text
2. Run `M-x arxana-scholium-create-resilient`
3. Enter your annotation

The region is highlighted in green. If Futon sync is enabled, the scholium persists
to XTDB.

### Commands

```elisp
;; Create a resilient scholium on selected region
M-x arxana-scholium-create-resilient

;; Verify all scholia in current buffer (re-find anchors)
M-x arxana-scholium-verify-all

;; View the scholium at point
M-x arxana-scholium-at-point

;; Clear all scholium overlays
M-x arxana-scholium-clear-overlays
```

### Visual Feedback

- **Green**: Anchored (content hash matches)
- **Orange**: Fuzzy-matched (found via context, hash may differ)
- **Red**: Orphaned (anchor not found - needs re-anchoring)

## Code-Docs Browser Integration

The code-docs browser (`arxana-browser-code.el`) automatically creates and persists
a link strategy on first use. This means:

1. First time you open a code file via the browser, a strategy is created
2. The strategy persists to Futon1 (if sync is enabled)
3. On subsequent sessions, the strategy is loaded from Futon1

### Commands

```elisp
;; Check the current strategy
M-x arxana-browser-code-strategy-status

;; Reset the cached strategy (forces reload on next use)
M-x arxana-browser-code-reset-strategy
```

### What Gets Persisted

The code-docs strategy captures:
- Repository: "futon4"
- Code roots: ["dev/" "test/"]
- Docbook: "futon4"
- Finders:
  - `:symbol-as-term` - function/variable definitions become linkable
  - `:filename-mention` - file names in docs link to files

The actual links are computed on-the-fly using this strategy, so they stay
current as code evolves.

## Demo Walkthrough

Here's a complete demo session:

```elisp
;; 1. Load and enable sync
(load-file "dev/bootstrap.el")
(setq futon4-enable-sync t)

;; 2. Check status
(arxana-links-status)

;; 3. Create a strategy (persists finder rules)
(arxana-links-demo-create-strategy)

;; 4. Test anchor resilience (no persistence needed)
(arxana-links-demo-verify-anchor)

;; 5. Create a real scholium
;; - Open a file, select some text
;; - M-x arxana-links-demo-create-scholium
;; - Enter your annotation

;; 6. Capture a surface form
;; - Select text like "HTTP endpoint"
;; - M-x arxana-links-capture-surface-form
;; - Enter concept: "api-route"

;; 7. Verify everything persisted
(arxana-links-status)
```

## Design Notes

### Why Persist Strategies Instead of Links?

Code and documentation are moving targets. If we persist every computed link
with exact line numbers, they break on every edit. Instead:

1. **Strategy declares intent**: "In this repo, function names are linkable terms"
2. **Links are recomputed**: Each session recomputes links from current state
3. **Promoted links are stable**: Only explicitly confirmed links persist

This is like autocomplete: we don't persist every possible completion, just the
rules for generating them.

### Resilient Anchors vs. Brittle Offsets

Traditional anchors break when content is edited. Our anchors survive because:

1. **Content hash**: Identifies the exact text, regardless of position
2. **Context**: Surrounding text helps re-find even if hash fails
3. **Fallback**: Fuzzy matching as last resort, marked for review

This is similar to how Git tracks content, not line numbers.

### Musical Analogy

Think of links like chord progressions:

- **Latent links**: Possible chords in the current key (autocomplete candidates)
- **Voiced links**: Actually played chords (confirmed connections)
- **Move types**: How one thing connects to another (substitution, proximity, etc.)

The system models the *space of possible connections*, not just the connections
themselves.

## Troubleshooting

### "Cannot persist: Futon sync disabled"

```elisp
(setq futon4-enable-sync t)
```

### Strategies/links not loading

Check that Futon1 is running:
```bash
curl http://localhost:8080/api/alpha/tail?limit=1
```

### Anchor not found (orphaned scholium)

The content may have changed too much. Options:
1. Manually re-anchor by selecting new region
2. Check `:context-before` and `:context-after` for clues
3. Use document version pinning for archival content
