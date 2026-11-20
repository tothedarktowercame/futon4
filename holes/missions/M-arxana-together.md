# M-arxana-together — several people editing one Arxana document, across surfaces

**Status:** OPEN — IDENTIFY. Chartered 2026-11-20 by Joe with claude-5. New research:
the work below has not been done anywhere we know of in this combination.

Joe, 2026-11-20, after his third editorial pass on the Mozilla incubator form with
Murch's revision side by side: "if we could port all of the features to code-server we
would be very close to having a custom Google Docs replacement ... with multiple
active editors, we'd need CRDT/OT after all." Then: "if we use jvm-arxana we can edit
across surfaces, crdt.el in Emacs (possibly customised) and Yjs for Codeserver"; "we'd
need a kind of omni-crdt at the centre ... that merges & replays the two"; "it's
probably easiest to build the central version first and then figure out how to
decentralise it later"; and "we're building and rewriting (immutably!) a hypergraph",
so "we presumably need a more general OT not just the bog standard CRDT".

## What exists (2026-11-20)

From M-arxana-incubator (futon4a, Emacs only, one editor at a time):

- A session that is an append-only log of attributed acts; every edit names its actor.
- Layers on a main line, and forks that branch from a layer and are invisible from the
  head (Murch's revision is the first fork).
- Word-level provenance on every revision: kept, typed, moved, accepted, dropped, and
  the source of each run (Joe, an agent, Murch, the Peeragogy call), shown in colour.
- A read-only view of any layer beside the form; copying from it (`M-w`) keeps the
  source of the words wherever they are pasted.
- Numbered versions of the whole session, kept on every change.
- Anchors that locate text by its words and context, never by file and line
  (DERIVE-2 §11).

From M-jvm-arxana (closed 2026-11-10): Arxana served from the futon3c JVM.

The open problem: two or more people editing at once, from Emacs and from a browser,
with the same provenance, layers and forks.

## Design (proposed 2026-11-20, to be tested)

1. **The hub is jvm-arxana, and it holds the record.** Live text is checkpointed into
   Arxana as attributed acts, as `C-c C-s` does in the form today.
2. **Each surface edits through its own CRDT.** Emacs through `crdt.el`, which already
   syncs a configurable set of text properties (`crdt--enabled-text-properties`), so
   provenance can travel with the text. code-server and the browser through Yjs, where
   the same information becomes text attributes.
3. **The centre is not itself a CRDT.** It is the one authority, so a single ordered
   log of operations suffices. It keeps one replica of each document in each edge CRDT
   and binds them: a change arriving from one side is replayed on the other as an
   ordinary local edit. The hub is the only participant in both, so they converge.
4. **Stable identities.** Every inserted run gets an id, with its attributes (author,
   source, layer). Notes, holes, scopes and links anchor to ids, so they survive edits
   and moves.

### Keep decentralisation cheap (requirements now, not features)

- Ids are unique without asking the hub (editor + counter, e.g. `murch:412`), never a
  hub-assigned sequence number.
- Operations refer to ids ("insert after `joe:88`"), never to positions.
- Each log entry records its author and the entries it followed (causal history), not
  only its place in the hub's order. A later version without a hub then merges logs;
  history is not rewritten.

### The operation algebra (Joe: "a more general OT")

Not one general transform, but a small set of operation types, each with a known
merge rule. Immutability is what makes most of them easy.

| operation | merge rule |
|---|---|
| insert or delete text in a run | a sequence CRDT |
| spans over text: provenance, notes, scopes, holes | anchored at run ids; the published design for concurrent spans is Peritext (Litt, Lim, Kleppmann, van Hardenberg, Ink & Switch, 2022) |
| add a node or a hyperedge | a grow-only set; no conflicts |
| derive or supersede a version | a branch in the derivation tree (DERIVE-2 §11); a later attributed act merges, by a person's judgement |
| move or regroup structure | **open.** Replicated-tree move without cycles: Kleppmann et al., "A highly-available move operation for replicated trees", IEEE TPDS 2021. The graph case, or a rule that a move is a new version plus a supersede, is the research question |

Prior art Joe knows first-hand: Etherpad (Easysync changesets with an attribute pool).
Its attribute pool is the precedent for provenance as per-character attributes.

## Plan (small packets; discovery before building)

1. **Discovery.** Read `crdt.el`'s wire protocol (version 0.3.0, s-expressions over
   TCP) and the options for Yjs from the JVM (a `pycrdt` or Node sidecar owned by
   jvm-arxana; no second JVM). Report the messages the hub must handle on each side.
2. **Proof.** One answer, edited at the same time from Emacs (`crdt.el`) and from a
   browser (Yjs), converging with each author's colour, then checkpointed into Arxana as
   an act whose segments the incubator form renders correctly.
3. Then: all answers; layers and forks as separate documents; a read-only demo for
   Murch and Rob behind its own Caddy basic-auth realm; then editing for them.

## Acceptance checklist

- [ ] A discovery report names every `crdt.el` and Yjs message the hub must handle.
- [ ] Two editors, one in Emacs and one in a browser, type in the same answer at once
      and both see the same text, each run in its author's colour.
- [ ] A checkpoint writes the converged text into Arxana as an attributed act, and the
      incubator form shows it with the right provenance.
- [ ] Ids and log entries meet the decentralisation requirements above (checked by a
      test that rebuilds the document from the log alone).
- [ ] The "move" row of the operation algebra has a stated rule, with a test of
      concurrent moves.
