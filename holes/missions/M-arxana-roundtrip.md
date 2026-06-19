# Mission: Arxana Round-Trip — Backend Adapters for Editable Hypertext

**Date:** 2026-04-27
**Status:** IDENTIFY. The local Essays subsystem already supports in-place
editing, manifest sync, comments, re-anchoring, and markdown export; no
general round-trip backend adapter exists yet.
**Blocked by:** None for Phase 1. Phase 2 depends on deciding what the
canonical editable surface for mathematics should be.
**Owner:** futon4 (Arxana / Essays), with dependencies on futon1a (XTDB
sync), futon3 pattern libraries, and external publication backends such as
Wikibooks / MediaWiki.

## Motivation

Arxana can already do the hard local part of editing for essays: it opens a
structured source document, overlays annotations as first-class entities, lets
the author edit the prose in place, re-anchors annotations when passages move,
and writes the updated section back to the canonical markdown file.

What it cannot yet do is round-trip that edited material through an external
publication surface without collapsing Arxana's structure into a dead export.

This mission is about building a real adapter boundary:

- Arxana keeps a canonical internal representation.
- A backend renders that representation into a foreign surface.
- The foreign surface can be read back in, diffed, reconciled, and either
  accepted or rejected.
- Drift is surfaced explicitly; it is not silently routed around.

The simplest proving ground is the Peeragogy Handbook on Wikibooks. The source
material is prose-heavy, page-structured, and already close to the existing
Essays workflow. The target backend, MediaWiki, is old but stable: raw
wikitext, page trees, explicit revision ids, explicit edit conflicts, and an
API that makes round-trip boundaries inspectable.

The more ambitious sequel is mathematical writing. We now have evidence that
the stack can parse arXiv eprints into paper-level hypergraphs, but that is
currently a read-side achievement, not a write-side editing cycle. The math
path belongs in the same mission family, but not in the first delivery.

## Theoretical anchoring

- **No workarounds:** A backend adapter is valid only if it preserves the
  invariants of the source system or reports where it cannot. "Export and pray"
  is not a round trip.
- **Arxana scholia-as-entities:** An annotation is not formatting residue. The
  adapter must preserve enough identity that annotations can be re-attached or
  explicitly marked unresolved.
- **Round-trip discipline:** A foreign system is not canonical merely because it
  is public. External edits are inputs to reconcile, not authority that erases
  the source model.
- **Backend pluralism:** Markdown, MediaWiki, and TeX are not rivals; they are
  publication surfaces with different operational affordances. Arxana's job is
  to make those surfaces interoperable without flattening the hypergraph.
- **Hypergraph-native future:** The point is not only to publish documents. The
  deeper goal is an editing model where claims, proofs, references, comments,
  and structural links can all survive format changes because they are carried
  in the graph.

## The gap in the current codebase

The local state of futon4 is uneven in a useful way:

- `dev/arxana-browser-essays.el` already provides the real editing loop:
  section open, in-place edit mode, save-back-to-markdown, manifest sync,
  comment insertion, XTDB sync, and passage auditing.
- `dev/arxana-articles-export.el` is still only a sketch. The key function
  `arxana-export--articles` returns `nil`, so there is no generic export layer
  to piggyback on yet.

This means the right first seam is not "generic Arxana article export." The
right seam is the existing Essays markdown surface. Phase 1 should therefore
attach to the Essays subsystem directly and only later generalize into a wider
backend framework.

## Scope

### Phase 1 — Essays <-> Wikibooks

Build a complete round-trip for essay-like texts using Wikibooks as the
demonstration backend.

#### In scope

- **Backend abstraction for essays:** Define a small adapter protocol with
  operations like `render`, `fetch-remote`, `diff`, `publish`, and
  `import-remote`.
- **MediaWiki renderer:** Convert the canonical essay markdown into wikitext
  suitable for Wikibooks.
- **Page-tree mapping:** Support a root page plus subpages. The Peeragogy
  Handbook root page is a special index page with templates and links; chapter
  content belongs on subpages.
- **Raw fetch + diff:** Read current remote wikitext using MediaWiki's raw page
  surface and compare it with the generated output before publishing.
- **Conflict-safe publish:** Use the MediaWiki edit API with explicit revision
  checks so Arxana can detect concurrent edits instead of overwriting them.
- **Import path:** Pull remote wikitext back into an Arxana-facing review flow.
  For Phase 1 this can be a compare/reconcile buffer rather than a perfect
  parser.
- **Emacs operator commands:** Add commands for previewing rendered wikitext,
  diffing against the live wiki, and publishing one page or a page set.
- **Round-trip evidence:** Store enough metadata locally to know which local
  section last published to which remote page and revision.

#### Out of scope for Phase 1

- Generic support for every Arxana document type.
- Perfect bidirectional parsing of arbitrary hand-edited MediaWiki syntax.
- Fully automatic management of complex root pages that mix prose, templates,
  media embeds, and hand-curated navigation.
- Multi-user collaborative locking across Emacs and Wikibooks in real time.

### Phase 2 — Papers <-> TeX / Hypergraph-Native Math

This arm is explicitly deferred but should shape the Phase 1 abstractions.

#### Intended direction

- Read `.tex` sources and keep them associated with paper-level hypergraphs.
- Support annotations on claims, proofs, equations, and citations as graph
  entities rather than comments trapped in TeX source.
- Explore whether the editable canonical form should remain `.tex`, become a
  normalized intermediate form, or become a hypergraph-native math document
  format with TeX as one backend.

#### Why deferred

The arXiv artifact we now have is read-side evidence of structure extraction,
not yet an editing contract. The archive at
`/home/joe/code/storage/mark2/outbox/results-mfuton-002.tar.gz` contains
paper-level hypergraphs, scopes, relations, and eprint-backed text coverage
for 5000 papers. That is enough to justify the sequel, but not enough to
pretend we already have a coherent write path.

## Wikibooks as the proving backend

Wikibooks is a good first backend because it is simple in the ways that matter:

- It has stable raw page text.
- It already organizes books as page trees.
- It has explicit edit revisions and timestamps.
- It has a documented write API.
- It forces us to confront drift and reconciliation directly.

It is also awkward in exactly the useful ways:

- The root page often contains hand-authored templates and navigation markup.
- Wikitext is lossy relative to richer local structure.
- Remote human edits are likely.

Those awkwardnesses make it a real backend, not a toy.

## Design posture

### Canonical source

For Phase 1, the canonical editable source remains the local essay markdown and
its manifest, as already handled by the Essays subsystem.

### Adapter boundary

Each backend must expose:

1. A rendering function from canonical local form to backend text.
2. A fetch function from backend object id to backend text + revision metadata.
3. A diff/reconcile flow that can surface drift before commit.
4. A publish function that refuses blind overwrite.
5. An import path that records what changed and what could not be mapped.

### Failure semantics

When the backend cannot preserve a structure, the correct action is to surface
the loss:

- unresolved annotation anchor
- unsupported template shape
- ambiguous imported formatting
- edit conflict against newer remote revision

The adapter may offer repair commands. It may not silently erase the mismatch.

## Completion criteria

### Phase 1 complete when:

1. A local essay can be mapped to a page set with stable local-to-remote ids.
2. Arxana can render at least one essay page into MediaWiki text from Emacs.
3. Arxana can fetch the current remote raw text for that page.
4. Arxana can diff generated text against the live Wikibooks page before
   publishing.
5. Arxana can publish via MediaWiki API with conflict checks based on remote
   revision/timestamp state.
6. A remote edit made outside Arxana is detected and surfaced as a reconcile
   event instead of being overwritten.
7. At least one Peeragogy Handbook subpage can complete the full cycle:
   local edit -> render -> preview -> publish -> fetch -> compare.
8. The root page strategy is explicit: either adapter-managed with a restricted
   template, or declared hand-maintained with generated subpage links only.
9. The commands and data shapes are documented in futon4.

### Phase 2 ready to start when:

1. We can point to a canonical editable math surface.
2. We can identify how claim/proof/equation anchors survive local edits.
3. We can state whether TeX is canonical, derivative, or peer to a new
   hypergraph-native math format.

## Relationship to other missions

- **Builds on:** `M-essays-edit-cycle` for the local essay editing lifecycle.
  That mission note is now partly stale relative to the implemented code; this
  mission treats the local edit cycle as operational and extends it outward.
- **Related to:** `M-self-representing-stack`, because backend adapters are part
  of making Arxana's internal structure legible across surfaces.
- **Potential sequel:** a future math-specific mission for TeX round-trip and
  hypergraph-native mathematical editing.

## Source material

- `futon4/dev/arxana-browser-essays.el` — current local essay editor, manifest
  sync path, comments, save-back flow, and markdown export.
- `futon4/dev/arxana-articles-export.el` — sketch export layer; currently not a
  usable generic adapter path.
- `futon4/holes/missions/M-essays-edit-cycle.md` — conceptual predecessor for
  the local editing lifecycle.
- `https://en.wikibooks.org/wiki/Peeragogy_Handbook` — current public root page.
- `https://en.wikibooks.org/wiki/Peeragogy_Handbook/Foreword` — representative
  content subpage.
- MediaWiki raw pages and edit API.
- `/home/joe/code/storage/mark2/outbox/results-mfuton-002.tar.gz` — read-side
  evidence for the future paper/TeX arm.

## MAP

### Q1. What is the exact canonical data shape for a backend-facing essay?

- Current answer: markdown source file + annotations manifest + XTDB ids.
- Need: a small explicit export/import object model so the backend does not
  depend on UI buffer state.

### Q2. What should be the minimal backend protocol?

- Proposed answer: `render`, `fetch-remote`, `diff`, `publish`,
  `import-remote`, `page-map`.
- Need: decide whether protocol lives in a new module or inside the Essays
  subsystem first.

### Q3. How should the Peeragogy root page be handled?

- Likely answer: do not fully regenerate it at first.
- Candidate strategy: adapter manages subpages and can generate a suggested
  root-page index block, but the public root page remains partly hand-curated.

### Q4. What counts as a successful import from MediaWiki?

- Conservative answer: imported text is first-class only after review.
- Need: define what structures are parsed back automatically versus flagged for
  human reconciliation.

### Q5. Where should publish credentials and backend config live?

- Need: a repo-local but non-committed configuration path for wiki endpoint,
  username, bot-password/app-password identity, and page mappings.

### Q6. What does the math sequel actually inherit from Phase 1?

- Candidate answer: the adapter contract, diff/reconcile discipline, stable
  local ids, and explicit failure semantics.
- It should not inherit any assumption that markdown is the canonical local
  source.

### Q7. What does the arXiv artifact already prove?

- It proves the stack can derive paper-level hypergraphs from eprint-backed
  source text at scale.
- It does not yet prove that we can edit those structures and write them back
  coherently to TeX.

## First implementation slice

The first slice should be intentionally narrow:

1. Add a new backend module for MediaWiki/Wikibooks.
2. Support one essay catalog and one or two Peeragogy subpages.
3. Render local markdown to wikitext.
4. Fetch current raw remote text.
5. Diff locally in Emacs.
6. Publish only after explicit review.

If that slice works, generalization can happen with evidence rather than
optimism.
