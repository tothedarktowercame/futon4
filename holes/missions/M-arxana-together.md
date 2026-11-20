# M-arxana-together — several people editing one Arxana document, across surfaces

**Status:** OPEN — building the server-centred version (route chosen 2026-11-20). Chartered 2026-11-20 by Joe with claude-5. New research:
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

## Design (revised 2026-11-20 after background research)

The first design of this date bound two CRDTs through the hub: `crdt.el` for Emacs and
Yjs for the browser (commit c11ae42 has that text). The background research
(`M-arxana-together-background.md`) found that the binding itself is where the risks
are: no maintained Yjs engine for the JVM; `crdt.el` re-inserts text under new ids when
a synced property changes and overwrites the author; two id spaces to map; concurrent
inserts ordered by two libraries' tie-breaks; no published convergence argument. Joe,
2026-11-20: "make the server-centred version and see it working. Then later (if we
want) make a peer sync version as a new research project."

1. **The hub is jvm-arxana, and it holds the record.** One merge engine in the futon3c
   JVM. Every edit from every surface goes to the hub, which validates it, appends it
   to one ordered log, and sends it to the other clients. Live text is checkpointed
   into Arxana as attributed acts, as `C-c C-s` does in the form today.
2. **Clients are thin and speak one protocol.** Emacs and the browser each keep the
   document as a list of characters with ids, and apply every log entry by the same
   integration rule as the hub. No `crdt.el`, no Yjs.
3. **Stable identities.** Every character has an id, `editor:counter`, chosen by the
   editor that typed it. An insert carries its attributes (author, source). Notes,
   holes, scopes and links will anchor to ids.
4. **Structure is separate from live text** (Joe, 2026-11-20: the hypergraph "can be
   seen as loosely coupled with CRDT"). Only the text of one document needs real-time
   merging. Adding nodes and hyperedges, superseding a version, and regrouping go
   through the hub as ordinary serialised acts.

### The text protocol

Units are Unicode code points everywhere (not UTF-16 units, not bytes).

**Sites.** The editor part of an id is a site: `<identity>` or
`<identity>.<surface>`, for example `joe.emacs` and `joe.web`. One person's surfaces
are separate sites, so their ids cannot collide. An identity has no dot. The person
shown in colour is the `author` attribute of the insert.

**State.** A document is an ordered list of characters. Each has an id `[editor n]`,
its code point, its attributes, and a deleted flag (deleted characters stay in the
list, so later entries can still name them).

**Operations** name ids, never positions:

- `insert {id: [editor n], after: <id or null for document start>, text, attrs}`.
  Character k of `text` has id `[editor n+k]`; character 0 goes after `after`,
  character k after character k-1.
- `delete {ids: [...]}`. Deleting a deleted character does nothing.

**Counters.** `n` is a Lamport counter: an editor's next counter is one more than the
largest counter it has seen in the document, its own included. Ids are therefore
unique without asking the hub, and an id is always greater than the id it was inserted
after.

**Integration rule** (the RGA rule; Roh et al. 2011). To integrate a character with id
X after id L: start at the place just after L; while the character at that place has
an id greater than X (compare counter, then editor name), step right; insert there.
The hub and every client apply this same rule, so a client can apply its own edit at
once and apply others' entries in whatever order they arrive relative to its own
unacknowledged edits, and all end with the same list. This rule is a sequence CRDT's.
What makes the system server-centred is everything around it: all traffic goes
through the hub, the hub's log is the record, and the hub refuses bad entries.

**Log entry.** `{rev, op, author, parents, at}`: `rev` is the hub's sequence number;
`parents` is the hub revision the editor had applied when it made the edit (what the
editor actually knew), plus by implication the editor's own earlier entries.

**Hub checks.** The id's site belongs to the connection's identity (Etherpad makes
the same check on authorship); the id is unused; `after` and deleted ids exist. A
refused entry is answered with an error and the client reloads the document.

**Conformance.** One fixture file of operation sequences with the expected text and
id list is run by all three implementations (Clojure, Emacs Lisp, JavaScript). The
cases include two editors inserting at the same place at once, an insert after a
character that another editor deleted at the same time, and a delete of the same
character by two editors.

**Transport.** The hub is `futon3c.arxana.together-http`, in the futon3c JVM on
loopback port 7096, with one log file per document under `~/storage/arxana-together/`.
Routes under `/together/v1`: `GET doc/KEY/state` (snapshot as runs, deleted characters
included), `POST doc/KEY/ops`, `GET doc/KEY/events?after=N` (server-sent events, one
log entry each, replayed from N and then live), `GET web/<file>` (the browser client).
The identity is the `X-Together-Editor` header, trusted because the server binds
loopback only; for anyone reaching it through Caddy, Caddy must set that header from
the basic-auth user and discard any value the client sent.

### Requirements kept so a peer version stays possible

- Ids are unique without asking the hub.
- Operations refer to ids, never to positions.
- Each log entry records its author and what the editor had seen.
- The document can be rebuilt from the log alone.

A peer-sync version (no hub, offline edits merged later) is a separate research
project, not part of this mission: `M-arxana-together-apart.md`.

### Structure operations (not real-time)

| operation | rule in the server-centred version |
|---|---|
| insert or delete text | the text protocol above |
| spans over text: provenance, notes, scopes, holes | anchored at character ids. Author and source are fixed at insert and travel with the insert. Spans that change later (notes, holes, scopes) are hub-side records naming a start id and an end id. Published design for concurrent spans: Peritext (Litt, Lim, Kleppmann, van Hardenberg, 2022) |
| add a node or a hyperedge | append; nothing conflicts |
| derive or supersede a version | a branch in the derivation tree (DERIVE-2 §11); a later attributed act merges, by a person's judgement |
| move or regroup structure | a serialised act at the hub. The hub refuses a regroup that would close a cycle and logs the refused claim. Open: an annotation made on a version while that version is superseded stays on the old version |

Prior art Joe knows first-hand: Etherpad (Easysync changesets with an attribute pool),
which also runs one authority with thin clients.

## Plan (small packets, each reviewed before the next)

1. **Engine.** A pure Clojure namespace: state, integration rule, log, rebuild from
   log, hub checks; the conformance fixture and its Clojure runner.
2. **Transport.** The hub endpoint in the futon3c JVM: join a document, receive the
   current state, submit entries, receive others' entries.
3. **Emacs client** in futon4a: a buffer bound to a hub document, each character in
   its author's colour; runs the conformance fixture.
4. **Browser client:** one page doing the same; runs the conformance fixture.
5. **Proof.** One answer of the incubator form edited at once from Emacs and the
   browser, then checkpointed into Arxana as an attributed act that the form renders
   with the right provenance.
6. Then: all answers; layers and forks as separate documents; a read-only demo for
   Murch and Rob behind its own Caddy basic-auth realm; then editing for them; a
   code-server extension.

## Acceptance checklist

- [x] The conformance fixture passes in Clojure, Emacs Lisp and JavaScript.
      (2026-11-20: futon3c bff8826df, b228483a0; futon4a 7ab1037.)
- [ ] Two editors, one in Emacs and one in a browser, type in the same answer at once
      and both see the same text, each run in its author's colour.
      (2026-11-20: shown on a scratch document through the live hub, a batch Emacs as
      joe and headless Chromium as murch; not yet on an answer of the form.)
- [ ] A checkpoint writes the converged text into Arxana as an attributed act, and the
      incubator form shows it with the right provenance.
- [x] A test rebuilds the document from the log alone, and the hub refuses an entry
      whose editor is not the connection's identity.
      (2026-11-20: futon3c together_test.clj and together_http_test.clj.)
- [ ] A regroup that would close a cycle is refused and the refusal is logged.

## Progress

| packet | what | commit | state |
|---|---|---|---|
| 1 | Clojure engine, conformance fixture | futon3c bff8826df | accepted 2026-11-20 |
| 2 | hub state and HTTP transport | futon3c 256c12ea2, a13d9a71b | accepted 2026-11-20; live on 7096 |
| 3a | Emacs Lisp document model | futon4a 7ab1037 | accepted 2026-11-20 |
| 4a | JavaScript document model | futon3c b228483a0 | accepted 2026-11-20 |
| 3b | Emacs buffer binding | futon4a d2eb909 | accepted 2026-11-20 (codex-12 draft; guard design fixed in review) |
| 3c | Emacs network layer | futon4a 2199454 | accepted 2026-11-20 |
| 3d | Emacs live editing (`futon4a-together-open`, `-attach`) | futon4a bc50570 | done 2026-11-20; three-buffer test against a real hub passes |
| 4b | browser binding logic | futon3c d7dd1fdac, 012f1e8cc | accepted 2026-11-20 |
| 4c | browser page | futon3c 33ccd131b, ff06cccfb | accepted 2026-11-20; eight-check two-browser test passes |
| 5 | share one answer of the incubator form; checkpoint as an attributed act | | dispatched; binding gained `futon4a-together-insert-pieces-function` (futon4a 394baec) after codex-12 found a mixed-source paste lost its second source |
