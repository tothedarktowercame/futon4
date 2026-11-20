# M-arxana-together-apart — editing one Arxana document without a hub

**Status:** NOTED 2026-11-20, not started. Joe, 2026-11-20: "make the server-centred
version and see it working. Then later (if we want) make a peer sync version as a new
research project"; and, of this file, "not to action it just to note it for later".
Written by fable-1 while the research for M-arxana-together was fresh.

**Depends on:** `M-arxana-together.md` (the server-centred version) working first.
**Background:** `M-arxana-together-background.md`; research notes in
`~/storage/research/research_notes/Multi surface CRDT hub for Arxana/`.

## The question

M-arxana-together routes every edit through jvm-arxana. This mission would remove that
requirement: editors work offline or exchange edits directly, and their logs merge
later with the same provenance, layers and forks. Is that worth building, and what has
to change?

## What the server-centred version already leaves in place

These were kept as requirements there so that this mission starts from a usable log:

- Character ids are `editor:counter` with a Lamport counter, unique without the hub.
- Operations name ids, never positions.
- Clients and hub integrate inserts by the same rule (RGA), which converges whatever
  order concurrent entries arrive in, provided each entry arrives after the entries
  it refers to.
- Each log entry records its author and the hub revision the editor had seen.
- The document can be rebuilt from the log alone.

## What the hub does that peers would have to do some other way

| the hub's job today | what replaces it without a hub |
|---|---|
| Delivers entries in one order, so an entry never arrives before the character it is inserted after | Causal delivery: each entry lists the entries it followed (a set of ids, not one hub revision), and a peer holds back an entry until those have arrived. The `parents {:rev k}` field becomes a frontier, as in Eg-walker (Gentle & Kleppmann, EuroSys 2025) |
| Checks that an entry's editor is the connection's identity | Signed entries. Keyhive (Ink & Switch) is the project to read; not researched for the background report |
| Refuses a regroup that would close a cycle | No published rule for nodes with several parents. Options: Kleppmann et al.'s tree move (TPDS 2021: order all moves by timestamp, undo and redo, skip a move that would close a cycle); keeping every claim and computing the grouping as a view that ignores losing claims (Evan Wallace's design); or allowing only "new version plus supersede", which cannot close a cycle |
| Decides who may write | Capabilities carried with the document |
| Holds the one record that is checkpointed into Arxana | Each peer holds a log; a checkpoint names the set of entries it covers |

## Open problems, from the background research

1. **Interleaving.** RGA can interleave two runs typed at the same place when one
   editor types backwards (prepending). FugueMax (Weidner & Kleppmann, 2023) is proved
   not to. Changing the rule later changes how old logs replay, unless the hub-era
   placements are kept as given. Decide before any log is treated as permanent.
2. **Moving a range of text.** Kleppmann's 2020 list-move paper names it as open.
   The incubator records "moved" as provenance on a new insert that points at its
   source ids; that may be enough.
3. **Annotations on a superseded version.** An annotation made on version v while
   someone supersedes v stays on v. This is open in the server-centred version too,
   and gets worse when the two acts can be days apart.
4. **Regrouping in a hypergraph** (the last row of the table above).
5. **Deleted characters accumulate.** With a hub, the log can be compacted at a
   checkpoint everyone has passed. Peers cannot know that everyone has passed it.
6. **What the user sees after a long separation.** Two people who each rewrote a
   paragraph offline get a merged paragraph that neither wrote. The incubator's forks
   (a branch merged by a person's judgement) may be the right answer for long
   separations, with character-level merging only for short ones. Upwelling and
   Patchwork (Ink & Switch, 2023 and 2024) report on this from use.

## Existing software worth evaluating first

Not for the hub version, where they were set aside, but as candidate engines here:

- **Eg-walker / diamond-types** (Rust): event graph with parents; replays any order.
- **Loro** (Rust, with JS bindings): Fugue text, Peritext-style marks, movable tree
  and movable list. Not researched directly.
- **Automerge**: marks, cursors, clone-and-merge branches; Patchwork is built on it.
- **Yjs v14** (release candidate on 2026-11-14): attribution kept as id-keyed maps
  outside the text, and suggestions as a separate document.
- **crdt.el**: dormant since 2024-03; its protocol was read and tested for the
  background report (a non-Emacs client can join a session). Its text properties
  cannot carry provenance that changes after insertion.

None has a maintained JVM binding as of 2026-11. A peer in Emacs or a browser does not
need one; a JVM peer would.

## First step, if this is ever taken up

Discovery only: take a real log from the server-centred version, split it into two
peers' logs at some revision, extend each separately, and merge them with causal
delivery in place of hub order. The result shows which of the six problems above
occur in our own documents and which are theoretical.
