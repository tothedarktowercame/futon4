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

**Display mark.** An insert's `attrs` may carry `mark`: `ok`, `debt` or `bad`.
Clients show marked text in green, amber or red instead of the author's colour.
Other values are ignored. The hub does not interpret it.

**Face.** An insert's `attrs` may carry `face`: `plain`, `heading`, `muted`, `typed`,
`call`, `agent`, `teammate`, `peeragogy` or `struck`. Clients show the text in that
face's colour (the incubator form's colours) instead of the author's. An answer
shared from Emacs carries its Emacs face under `prov.face`, and the page maps those
names to the same set. Other values are ignored.

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
included), `POST doc/KEY/ops` (body `{parents, op}`; with `"alone": true` it is
accepted only while nobody else has written to the document; an insert identical
to one already logged from the same person is answered with that entry's revision), `GET doc/KEY/events?after=N` (server-sent events, one
log entry each, replayed from N and then live), `GET web/<file>` (the browser client), `GET docs` (the list of documents), and for
structure `GET structure/WS/state`, `POST structure/WS/acts` (200 accepted, 409
refused; both are logged), `GET structure/WS/log?after=N`.
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
      (2026-11-20: passes on a fixture session, in ERT and against a throwaway hub:
      a teammate's text saves as accepted from them, Joe's typing as typed, earlier
      text as kept, a paste from two answers as moved from each. Not yet done on the
      real form with a second person.)
- [x] A test rebuilds the document from the log alone, and the hub refuses an entry
      whose editor is not the connection's identity.
      (2026-11-20: futon3c together_test.clj and together_http_test.clj.)
- [x] A regroup that would close a cycle is refused and the refusal is logged.
      (2026-11-20: engine futon3c 4f1eada00, hub routes c0dc64ab4. On the live hub,
      through Caddy, workspace `proof-ws-1120`: regroup b from a to c answered 409
      "cycle", state unchanged, and the refused act is line 6 of
      `~/storage/arxana-together/structure/proof-ws-1120.log.edn`.)

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
| 6 | Caddy block `/edit/` and page URLs relative to the page | futon3c (page.js), /etc/caddy/Caddyfile | done 2026-11-20; typed through Caddy in headless Chromium, text arrived at the hub as joe.web |
| 6a | a repeated insert (retry after a timed-out POST) is acknowledged with its first revision | futon3c 1fc41547d | done 2026-11-20; was a known weakness |
| 6b | seeding is conditional (`"alone": true` on the POST body): of two people sharing the same untouched answer at once, one is refused and reloads | futon3c c409a9da2, futon4a 1b90fc1 | done 2026-11-20; was a known weakness |
| 7 | pure structure engine (`futon3c.arxana.together-structure`): add-node, add-edge, supersede, group, ungroup, regroup; a group or regroup that would close a cycle is refused and the refused act is kept in the log | futon3c 4f1eada00 | accepted 2026-11-20 (codex-12; first sent with an impossible test case, withdrawn on codex-12's objection). Not yet reachable through the hub |
| 8 | list of documents: `GET docs`, and the page at `/edit/` with no `doc` lists them and opens a new key | futon3c 33043dada | accepted 2026-11-20 (codex-13) |
| 9 | structure routes on the hub: `structure/WS/state`, `acts`, `log`; one log file per workspace under `structure/`; refused acts are written | futon3c c0dc64ab4 | accepted 2026-11-20 (codex-12; my acceptance case was wrong, corrected on codex-12's objection); live |
| 10a | several bound regions in one Emacs buffer (`futon4a-together-attach` adds, `-detach` takes an optional connection, `-with-binding`) | futon4a b0117a2 | accepted 2026-11-20 (codex-13) |
| 10b | share the whole form: `futon4a-incubator-together-share-all` binds every answer to its own document; `-share` adds the answer at point; `C-c C-s` detaches all and saves as usual; an unanswered question's placeholder is never seeded | futon4a 6b6c715 | accepted 2026-11-20 (codex-13) on fixture forms and a throwaway hub; not yet tried on the real form |
| 11 | Demo A records for these features, in memory: 13 choices, 12 with a probe that runs the real code | futon3c aa377e644; manifest `~/storage/arxana/design-catalogue/together-manifest-2026-11-20.edn` | built 2026-11-20 (codex-9, finished by fable-1); not written to the store, which needs Joe's go |
| 5 | share one answer of the incubator form (`futon4a-incubator-together-share`); `C-c C-s` saves the converged answer as the usual act | futon4a 394baec, 3ab861f, a30a9f8 | accepted 2026-11-20 on a fixture session; not yet tried on the real form |

## Known weaknesses (2026-11-20)

- The form's answers are hard-wrapped, so a browser editor sees the line breaks.
- Outside access is `https://zone.hyperreal.enterprises/edit/?doc=KEY` (Caddy realm
  `arxana-together`, added 2026-11-20). Caddy sets `X-Together-Editor` from the
  basic-auth user and replaces any value the client sent. Only `joe` has a login;
  a second editor needs one more line in that block.
- The engine and the Emacs model are lists scanned per character: fine for an answer,
  slow for a long document or a large paste. Measured 2026-11-20: writing 18,000
  characters into a document that already held 18,000 took a minute, and after a
  JVM restart the first request for a 16,000-character document replays its log
  for about a minute while holding that document's lock.
- 2026-11-20, found from Joe's "It comes up as all white for me": the document
  list (packet 8) replayed every log on every call under each document's lock,
  two minutes by then; fable-1's own browser checks called it and Joe's page
  waited 9 seconds and more for its document. Fixed in futon3c 1f8d4cd55 (the
  list reads in-memory state). fable-1 had noted the re-read when reviewing
  packet 8 and accepted it.
- Structure operations are served by the hub but no client uses them yet, and
  nothing connects a workspace's nodes to the text documents.


## Demo A: documenting these features and the incubator's (2026-11-20)

Joe, 2026-11-20: "it would be good if you could develop the separate "Demo A" further
with documentation of Arxana, including the real-time features", then "yes, plan the
incubator batch too". Demo A is the catalogue of design choices held as records
(`M-arxana-incubator.md` §6; viewed with `M-x futon4a-catalogue`). The incubator
mission is claude-5's draft; this section is fable-1's plan for two batches and does
not edit that file.

### Batch 1, real-time editing: written to the store 2026-11-20

Joe, 2026-11-20: "you can write back batch 1". Written by
`~/storage/arxana/design-catalogue/write-together.clj` at 15:13Z: 13 choices, 12
checks, 12 links, 12 runs, 49 records, all created; plan and response in
`together-catalogue-2026-11-20.edn` beside it. Read back through
`catalogue-view/read-catalogue`: status ok, 13 `:together/` choices, debt one
`:no-check` (`:together/peer-version-stays-possible`), coverage still passing.

`futon3c.arxana.together-catalogue` (aa377e644). Thirteen choices, keys
`:together/<name>`: convergence, ids-not-positions, log-is-the-record,
authorship-checked, provenance-fixed-at-insert, repeated-insert-acknowledged,
seeding-alone, loopback-identity, structure-is-serialised, regroup-cycle-refused,
emacs-client-conforms, browser-client-conforms, peer-version-stays-possible. The
first twelve each have a probe that runs the real code on a small case; the two
client probes run the Emacs and Node conformance tests as separate processes. The
last has no check and will show as debt. Records for review:
`~/storage/arxana/design-catalogue/together-manifest-2026-11-20.edn`.

### Batch 2, the incubator's editing features: written to the store 2026-11-20

Joe, 2026-11-20: "batch 2 live write is OK". Written by
`~/storage/arxana/design-catalogue/write-incubator.clj`: 22 choices, 20 checks, 20
links, 20 runs, 82 records, all created; log `incubator-catalogue-2026-11-20.edn`.
The catalogue now reads back as 51 choices, 48 enforced, 3 with no check.

### The catalogue as a document on /edit

Joe, 2026-11-20: "if we can get the actual content into demo doc on the /edit surface
that would be great". `futon3c.arxana.catalogue-document/render` (68e2ff1ef) turns
the catalogue view into plain text, one paragraph per line, grouped as real-time
editing, the incubator form, the store and its queries, the catalogue's own rules,
then debt. `~/storage/arxana/design-catalogue/seed-demo-doc.clj` read the live
store and seeded the hub document `demo-a` as author `fable-1`: 18,158 characters,
177 entries. Open `https://zone.hyperreal.enterprises/edit/?doc=demo-a`, or
`M-x futon4a-together-open RET demo-a`. The document is a copy: editing it does
not change the records, and it is not regenerated when the records change.

Joe, the same day: "it shows only authorship colours not the green/amber status that
we had in earlier Emacs Demo A". Added a display mark: an insert's attributes may
include `mark`, one of `ok`, `debt`, `bad`, fixed at insert like the others; the
browser page and the Emacs binding show marked text in green, amber or red in
place of the author's colour, and the page's legend names the marks in use
(futon3c 61ec2a79a, futon4a 574e4ad). `demo-a` was emptied and written again with
each choice's lines marked by its enforcement status: 48 green, 3 amber, rev 356.
A mark does not follow the records: if a check starts failing, the document keeps
its old colour until it is written again.

How this batch was built in memory:

`futon3c.arxana.incubator-catalogue` (codex-9 2f61012a5, 7a3958bc3; fable-1's
review commit after them). 22 choices, 20 with a check, each check running the
named ERT tests in a separate batch Emacs: 20 enforced, 2 debt
(`proposer-is-never-joe` has no test; `undo-is-an-act` is proposed, not true yet).
codex-9 left three rows out because their tests assert less than the planned
sentence; they are in with shorter sentences: `actor-required` (was
actor-on-every-act), `reason-and-slot-required` (the warrant half is untested),
`clips-pinned-to-revisions` (the orphaning half is untested). codex-9 also added
the harness test `incubator-test-harness-refuses-writes-and-store` to futon4a
(e7f65c5, 97783ec, 274acce). Records for review:
`~/storage/arxana/design-catalogue/incubator-manifest-2026-11-20.edn`.
The plan as first written follows.

From a read of futon4a on 2026-11-20 (the code is ahead of the incubator mission
file: forks, numbered versions, the read-only layer view, copying with source and
teammates are in the code and not in that file). Keys `:incubator/<name>`. Each
check is an existing ERT test, run in a separate batch Emacs by one probe function,
which must report at least one test run and none failed. T = `test/futon4a-incubator-test.el`.

| key | kind | statement | source (futon4a/lisp) | check (existing test) |
|---|---|---|---|---|
| actor-on-every-act | constraint | An interactive command records its act as Joe's; a non-interactive call that names no actor is refused. | futon4a-incubator.el, act recording | T `incubator-navigation-source-and-identity-refusal` |
| reason-and-slot-required | constraint | A challenge or warrant with no reason, and a move to an unknown slot, are refused. | futon4a-incubator.el | same test |
| edit-is-one-act | ruling | Committing an edit adds one edit act with its text and provenance; an unchanged commit or a cancel adds none. | futon4a-incubator.el, edit and edit-commit | T `incubator-edit-flows-back-to-the-slot` |
| layers-on-a-main-line | ruling | Versions are layers on a main line; a layer shows the acts of its lineage only. | futon4a-incubator-layers.el | T `…-revision-layer-above-the-call`, `…-proposals-sit-above-joes-pass` |
| forks-invisible-from-head | ruling | A fork branches from a layer; its acts never show on the head of the main line. | futon4a-incubator-layers.el | T `…-murch-layer-is-a-teammates-not-an-agents` |
| proposals-are-not-the-answer | constraint | An edit on a proposal layer or a fork is not counted as Joe's answer unless that layer is shown. | futon4a-incubator.el | the two tests above |
| proposer-is-never-joe | constraint | Proposing a revision as Joe, or with no actor, is refused. | futon4a-incubator-layers.el, propose-revision | none found: debt |
| editorial-save | decision | In an editorial pass, saving writes one edit act by Joe per changed answer; the placeholder of an unanswered question is not saved; leaving without saving writes nothing. | futon4a-incubator-editorial.el | T `…-editorial-pass-tracks-typing-acceptance-and-cut-and-paste` |
| character-provenance | decision | On save each character is classed typed, accepted, agent, kept or moved; text pasted from outside counts as typed. | futon4a-incubator-editorial.el | same test |
| sources-in-colour | ruling | Each source has its own face; agent text is bracketed, a teammate's is not. | futon4a-incubator-provenance.el | T `…-revision-provenance-colours-brackets-and-hud`, `…-expansion-segments-carry-their-own-source`, `…-ret-edit-colours-only-what-changed` |
| layer-view-is-read-only | constraint | Viewing a layer or fork renders a copy; the form's session, act count and layer are unchanged. | futon4a-incubator-layers.el, snapshot | T `…-snapshot-shows-a-fork-without-touching-the-form` |
| copy-keeps-source | decision | Text copied from a teammate's view and pasted in an editorial pass is saved as accepted from that teammate. | futon4a-incubator-editorial.el | T `…-copy-from-murchs-view-keeps-his-colour` |
| numbered-versions | ruling | When the session's content changes, an idle timer writes it as a numbered version named by its hash; a state seen before keeps its old number. | futon4a-incubator-autosave.el | T `…-autosave-numbers-versions-and-sees-in-place-edits`, `…-autosave-round-trip` |
| anchors-by-words | ruling | A mark is located by its words and their context: a unique exact match, else a unique best context match, else nowhere; never by file and line. | futon4a-incubator-holes.el | holes test `…-holes-anchors-survive-edits-and-moves`, `…-never-marks-the-wrong-word` |
| clips-pinned-to-revisions | ruling | A clip is pinned to a revision of its source, stored by content hash; a clip that cannot be relocated is orphaned, never re-split. | call-notes.el | call-notes test `…-revision-is-content-addressed-and-not-rewritten` |
| fuzzy-match-proposes | ruling | A fuzzy match proposes a derivation and never decides one; a speaker passes only through a confirmed derivation. | clip-derivation.el | `clip-derivation-paraphrase-merge-reject-and-cycle` |
| first-cut-once | ruling | An over-limit answer is cut once automatically, and never again after anyone has cut or restored. | futon4a-incubator.el | T `…-first-cut-strikes-to-fit-and-x-restores` |
| load-refused-whole | constraint | A turn-clip load with any text mismatch is refused whole and leaves the session unchanged. | futon4a-incubator-turns.el | turns test `turn-clips-mismatch-refuses-whole-load` |
| export-is-explicit | constraint | Export goes only to a file the user chooses, and quitting with unsaved acts asks first. | futon4a-incubator.el | T `…-explicit-export-and-unsaved-warning` |
| tests-do-not-persist | constraint | The test harness fails any store request or file write, and autosave never starts in batch. | T `with-incubator`; futon4a-incubator-autosave.el | every T test runs under it; a planted write must fail (new small test) |
| shared-answer-saves-as-usual | decision | A shared answer, once detached, is saved by the ordinary editorial save with the same provenance classes. | futon4a-incubator-together.el | `incubator-together-seed-and-save-attribution` |

Not to be recorded as accepted, because the code does not do what the sentence says:

- "The session is an append-only log": acts are appended in normal use, but
  attaching revision segments replaces an act, restoring a version replaces the
  session, and twelve acts were once removed by hand.
  Joe, 2026-11-20: "this reminds me of our Elephant 2000 work ... With Elephant I
  should be able to "undo" a REPL turn. As to whether it is saved anywhere as a
  parallel branch I'm not sure at all. Here I guess it's similar". What Elephant's
  rewind does (futon3c `emacs/turn-stepper.el`, `turn-stepper--revert` and
  `turn-stepper-rewind-apply`): it runs `git revert` on the session's commits, so
  the undo is new commits and the undone ones stay in history, on no branch of
  their own; it cuts the turn's text out of the REPL buffer; and it tells the
  agent the turns are withdrawn. The incubator differs: its three rewrites change
  or drop acts in place, and the earlier state survives only as a numbered version
  if the idle autosave had run. Proposal (fable-1): add the row
  `:incubator/undo-is-an-act`, lifecycle `:proposed`, no check: "An undo is a new
  act that withdraws earlier acts; an act is never changed or removed." It will
  show as debt until the three rewrites are changed to append.
- "A version on every change": versions are taken on an idle tick, so several
  changes can become one version. The table's wording says what the code does.
- futon4a README line 228 says "No decisions are saved automatically", but the idle
  autosave writes to `~/storage`. The README is wrong; fix the README.
- "dropped" is a class in passage-level provenance and in the Python segments, not
  in the character-level classification on save.

Planned in the incubator mission and not built, so not in the batch: a per-act
journal replayed on open; a "From your turns" layer; word-level scopes in the
display; the frame as an open hypergraph checked by DiagramProver.

One hazard found on the way: four tests in T create and kill a buffer named
`*Arxana Incubator*`; run inside Joe's Emacs instead of in batch they would replace
his live form. The probe runs them only in a separate batch process.

### The incubator form as a document on /edit (2026-11-20)

Joe: "Can you bring up the *Arxana Incubator* document with its color-coded contents
in the web version". The hub document `incubator-form` is a copy of his open
`*Arxana Incubator*` buffer (version 17, layer "Your third pass", track changes
shown): 15,964 characters with the form's faces, read from the buffer without
changing it (`~/storage/arxana-together-tools/extract-incubator-buffer.el`) and
written as author `fable-1` (`seed-incubator-form.py` beside it). Inside answers,
the hard line breaks the form puts at the Emacs window's width are joined, so the
browser wraps the text itself. Faces: futon3c 90d706959, futon4a 69206f0.

It is a copy for reading and trying edits: nothing typed in it reaches the form's
session, and it does not follow the form. The way to edit the form's answers with
someone else is still `futon4a-incubator-together-share-all`, which makes one
document per answer; a web page that shows those documents together as one form,
with the questions between them, does not exist yet.
