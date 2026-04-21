# Mission: Essays Edit Cycle — Re-anchoring Annotated Editions Under Editing

**Date:** 2026-04-20
**Status:** STUB. Read view is operational (`arxana-browser-essays.el`,
`/home/joe/npt/working-paper/annotations.el`); editing pathways are not.
**Blocked by:** None directly. Builds on the operational Essays
subsystem (read view, importer, manifest format).
**Owner:** futon4 (Essays browser), with ingress points in futon1a (XTDB
writeback) and the source repos for essays under annotation.

## Motivation

The Essays subsystem (`arxana-browser-essays.el`) currently provides a
read-only annotated edition: a markdown essay rendered alongside a
notes pane that highlights pattern-library citations passage by
passage, with cross-buffer point-tracking. The UKRN Working Paper is
the working example, with 53 sentence-level annotations across seven
sections linking to the `equity/` and `ukrns/` flexiarg libraries.

This is the easy half. Real-world use of an annotated edition involves
editing — of the essay text, of the cited flexiargs, and of the
manifest itself. Each edit pathway has its own re-anchoring problem.
None of them is currently handled. The Essays subsystem ships a static
reading view of what is, in practice, a moving target.

The mission is to specify and implement the edit-cycle handling so the
annotated edition stays coherent as its three constituent layers (essay
prose, flexiarg sources, annotation manifest) evolve under continued
authorship. The UKRN WP is a useful test bed because all three layers
*are* live: the WP draft is iterating, the flexiarg library is
iterating, and the manifest itself is being authored.

## Theoretical anchoring

- **Arxana scholia-as-entities:** Annotations are first-class, not
  margin comments. The hyperedge model (already in place) supports
  rich edit semantics — versioning, attribution, deletion-as-rewrite —
  in a way that line-anchored annotation systems (PDF margins, web
  highlight tools) cannot.
- **Re-anchoring as a known-hard problem:** The annotation literature
  (Hypothesis, Genius, scholia plumbing in Recogito) treats text-anchor
  drift as a primary failure mode. Strategies range from exact-match
  (fragile) through fuzzy match through anchor-by-context (TextQuoteSelector
  and related W3C Web Annotation specs). Each fails differently; the
  mission needs to specify which failure modes are acceptable for the
  Essays use case.
- **Self-application discipline:** The annotated edition mirrors the
  WP's own self-application stance — the artefact updates as evidence
  accrues. An Essays subsystem that breaks under edit is the WP's own
  argument used against itself; the mission is what closes that gap.

## The four edit pathways

### 1. Essay text edited (annotation passages no longer match exactly)

When the WP markdown is reworded, an annotation's `:annotated :passage`
may no longer be findable in the section text. Two failure modes:

- **Lossless (whitespace, markdown markup, punctuation):** the audit
  function (`arxana-browser-essays-audit-passages`) catches these and
  the manifest can be auto-fixed. The current implementation does this
  as a one-shot batch via the Python script in `/tmp/audit_passages4.py`.
  Lifting this into an interactive Emacs command is straightforward.
- **Lossy (wording changes):** "the gap between outcomes is architectural"
  → "the architectural gap between outcomes" — exact match fails; even
  fuzzy match may not recover the right passage. Re-anchoring options:
  - Anchor-by-context: store N-word neighbourhoods (`prefix` /
    `suffix`) alongside each passage; on miss, search by neighbourhood.
  - Diff-driven: when the source file changes, apply the diff to the
    known annotation positions to predict where they moved.
  - Fall back to manual reconciliation: surface the unresolved
    annotations in a buffer, present likely candidates, let the author
    choose.

### 2. Flexiarg edited (linked source-passage may now be stale)

Each annotation cites a flexiarg's specific field
(`:source :passage "! conclusion: ..."`). Two questions:

- **Tracked by content or by field name?** Currently the passage is a
  literal string. If the flexiarg's `! conclusion` line is reworded,
  the linked passage no longer matches. Switching to field-name
  tracking (`{:field :conclusion}`) would preserve the link but lose
  the specific quote. A hybrid (field name + best-effort quote) seems
  right but needs spec.
- **Snapshot vs live view?** Should the notes pane show the flexiarg
  as it was when the annotation was authored (snapshot), or as it is
  now (live)? Provenance arguments for snapshot; currency arguments for
  live. Probably the right answer is live with a "this annotation was
  authored against version X, current text shown" note.

### 3. Manifest edited from within the browser

The browser is currently read-only. Making it editable means:

- **Inline note editing:** in `*Arxana Essay Notes*`, edit a gloss or
  source passage in place; save writes back to `annotations.el` and
  upserts the hyperedge in XTDB.
- **Add annotation:** select a passage in `*Arxana Essay*`, invoke
  command, prompt for source pattern and gloss, persist.
- **Delete annotation:** at point in either buffer, invoke command;
  remove from manifest and XTDB.
- **Writeback to disk:** the manifest is an Elisp `defconst`. Editing
  it programmatically requires either a structured pretty-printer
  (preferred — preserves the file's hand-authored structure) or a
  full regenerate (loses comments and section organisation). The
  pretty-printer is non-trivial.
- **Writeback to XTDB:** existing `arxana-store-create-hyperedge` and
  related functions handle the write. The harder question is *update
  vs delete-and-recreate* for hyperedges, which depends on Futon1a's
  hyperedge mutation semantics.

### 4. Section structure changes

Adding a new section, renumbering, or merging two sections raises:

- **ID stability vs display numbering:** section IDs in the manifest
  use the form `section/3-cycle-0` — stable across renumbering. The
  display name uses §N — not stable. The mission should clarify
  whether the manifest should auto-update display names from the
  source file's headings or treat them as authored separately.
- **Auto-discovery:** when a new `## N. Heading` is added to the WP,
  should the Essays subsystem auto-create a section entity? Probably
  yes, with the user prompted to confirm; auto-creation without
  confirmation risks polluting the manifest with sections that have no
  annotations yet.

## Sync semantics

When does an edit trigger a re-import / re-anchor?

- **On save?** Source files watched; on save, run the audit and either
  auto-fix or surface unresolved.
- **On demand?** A `M-x arxana-browser-essays-resync` command.
- **Periodically?** Background timer.
- **Multi-author divergence:** if Joe edits the manifest while Codex
  edits the WP, who wins? Probably last-writer-wins at the cell level
  (each annotation is independent), but the mission should specify.

## Working example

The UKRN Working Paper is a useful test case because all four edit
pathways are live concurrently:

- The WP draft (`UKRN_WP_draft_v5.md`) iterates as the writing plan
  executes.
- The flexiarg libraries (`/home/joe/code/futon3/library/{equity,ukrns}/`)
  iterate as patterns are revised.
- The manifest (`/home/joe/npt/working-paper/annotations.el`) iterates
  as new annotations are added or revised.
- Section structure may change as the writing plan absorbs new
  sections (annexes, gallery cross-references, candidate sixth pattern
  evolutions).

The mission can use specific known iterations as test cases:
- The §0 architectural-passage edit (already happened: "the difference"
  → "the gap between those outcomes"). Audit caught and fixed it.
- The H3 Year-1 default decision (planned: the [hole: H3] marker will
  be replaced with concrete prose; existing annotation engages-hole
  must follow).
- The MVSG figure caption edit (planned: when the rendered PNG changes
  composition, caption updates).

## Scope

### In scope

- **Audit-and-fix lifecycle** for the lossless edit case (whitespace,
  markup, punctuation). Automate the Python-script path into a
  reliable Emacs command.
- **Anchor-by-context** spec and implementation for the lossy edit
  case. Store prefix/suffix neighbourhoods; fall back to manual
  reconciliation for the unresolvable.
- **Manifest writeback** with structured pretty-printing that
  preserves comments and section organisation.
- **Inline edit commands** in the notes buffer (edit gloss, edit
  passage, delete annotation).
- **Add-annotation command** triggered from a region selection in the
  text buffer.
- **Auto-section-discovery** with confirmation prompt.
- **Re-sync command** with on-save and on-demand triggers.

### Out of scope (for this mission)

- Multi-user real-time sync (deserves its own mission, likely
  inheriting from the WebArxana mission's sync infrastructure).
- Annotation-level provenance / version history (beyond what XTDB
  hyperedge versioning gives for free).
- General-purpose annotation system for other essays beyond UKRN WP
  (UKRN WP is the test case; generalisation comes after).
- Cross-essay annotation queries (e.g. "all annotations citing
  ukrns/ARGUMENT across all essays"). Worth doing later but not core.

### Deferred questions

- Whether annotations should be tracked by content + field name +
  context-anchor as a triple (defence-in-depth) or by one of these
  with the others as fallback.
- Whether the source-flexiarg link should follow the flexiarg's
  current state (live) or snapshot at annotation-authoring time.
- Whether the manifest format should remain Elisp or migrate to
  EDN/JSON (the original sketch was EDN; current is Elisp because
  futon4 has no EDN reader; this mission is a natural point to
  reconsider).
- **Author-comments (Google-Docs-style sticky notes).**
  `arxana-browser-essays-add-comment` (bound to `C-c c` in the edit
  map) creates an annotation with `hx-type "annotation/comment"`, no
  pattern source, and the comment body as `:note`.  It writes to
  `annotations.el` and re-renders (yellow tint + `💬N` marker in the
  text buffer; `💬N Author comment` entry in the notes buffer).
  XTDB sync currently *skips* comments because they have no pattern
  endpoint to pair with the section endpoint.  The hyperedge
  representation is open: single-endpoint scholion, or a constructed
  `arxana/author/<id>` entity as the source endpoint.  Needs a
  decision before comments become durable via checkpoint/restore —
  today they survive in the manifest file only.
- **Copy-paste (as distinct from cut-paste) of an annotated passage.**
  The render now mirrors each annotation's id onto its text as a text
  property, so kill/yank carries the id through the kill ring and a
  post-command reconcile sweep re-attaches the overlay at the yank
  target. This works cleanly for *cut-paste-move* — one text-property
  run per id, overlay follows. *Copy* leaves two runs for the same id:
  the reconcile currently attaches the overlay to the first run only;
  the second occurrence displays as undecorated text. The unresolved
  question is what the semantics should be:
  - Forked annotation (new id, inherited metadata) — makes
    provenance explicit but multiplies the manifest.
  - Silent un-annotation of the copy — matches the "cut is the
    canonical move" stance but is non-obvious.
  - Dual-display (same id, both runs overlaid, markers disambiguated
    `[N/a]` / `[N/b]`) — most faithful to the hypergraph view, but
    complicates the sync-back-to-manifest path (which passage is
    canonical when one is edited?).
  For now, cut-paste-move is the supported workflow; copy-paste of an
  annotated region is undefined.

## Exit criteria

- The UKRN WP edit cycle (essay edits, flexiarg edits, manifest edits,
  section edits) can run for a full Cycle-1 writing pass without
  manual annotation-fixing intervention beyond the cases the mission
  explicitly defers to manual reconciliation.
- The audit function and the editing commands are documented in the
  Essays README (to be written as part of the mission).
- A subsequent essay (maybe a futon3c mission report or a futon7
  landscape note) can be annotated using the same machinery without
  copy-paste of the UKRN-specific code.
