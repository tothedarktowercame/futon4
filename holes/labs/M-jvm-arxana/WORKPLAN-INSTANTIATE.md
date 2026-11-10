# Workplan items in Arxana — INSTANTIATE record

Joe, 2026-11-09: after the code↔docs demo, place workplan items exactly
where they arose in the evidence and semantically against entities, benefit
lines and priority. claude-6 owns the data mapping (notes under
~/notes/cascade-live-outline/, handoff HANDOFF-arxana-2026-11-09.md);
codex-21 builds; claude-13 reviews. Private stacks only.

## Slice 1 — T-pattern-search-reads-stale-index (accepted after review fixes)

claude-6: `~/notes/cascade-live-outline/arxana/t-pattern-search-slice.edn`
from `make_slice.py` — 29 records (14 'clusions, 15 attestations): the item
(provenance from commit futon3c 0bb3dbf7a; its Dispatched-By trailer wrongly
names zai-10), pending level 6 as a :model draft, five mentions in time order
(context 04:36, first statement 04:38, operator request 04:41, filed 04:42,
cause found 04:42), three :parser "names" links, one :model "serves" link to
benefit line P. claude-13 checked all four evidence quotes verbatim against
`:7070/api/alpha/evidence/<id>` at their codepoint ranges. Nothing
correctable is in any identity.

codex-21: futon3c a95245a3a (types `[:workplan/item 1]`,
`[:lexicon/entity 1]`, `[:benefit-line 1]`, `[:workplan/names 1]`,
`[:workplan/serves 1]`, `[:workplan/mention 1]` (endpoint role
`:evidence`, as the file has it); local-id-resolving, idempotent
materialiser; `:workplan/item-view`; private demo), warrant 2ba965430;
futon4a d3c406a (`futon4a-workplan.el`, terminal page). 29/29 records read
back; second run creates nothing.

Review (claude-13), walked in a clean Emacs: the evidence fixture was
hand-made (`entry.body.text`) and the client read that shape, so `e` failed
on every real mention (live: `entry.evidence/body.text`). Fixed in futon4a
12fc8d1: recorded real fixture, `evidence/body`, UTF-8 decode, quote
highlighted at its stored range; ERT also checks mention time order.
Process safety clean; claude-6's files unchanged.

Demo for Joe: 127.0.0.1:7094 (`systemd-run --user --scope
--unit=futon4a-workplan-demo`).

## Slice 2 — all 323 items and the list (accepted after a review fix)

claude-6: `all-items.edn` from `make_all.py` — 3,881 records (1,720
'clusions, 2,160 attestations, 1 assessment): 323 items (322 pending + E-R14
closed by Joe), 323 `:filed` mentions, 454 names, 416 serves (claude-6's
summary said 418; the file holds 416, matching its 1,720 total), 75 benefit
lines with names, 322 pending levels, 116 user stories. Serves from three
producers kept separate. claude-6 corrected its own glosses of C/K/G/N; the
~10 user-story serves on those lines are weaker. Spot-checks (claude-13):
E-R14's ruling sourced to closed-by-joe.tsv row 1; M-a-wmc-scaling's filed
commit 5271311d is the commit that added its file (git log --diff-filter=A).

codex-21: futon3c 9abe433b5 (chunked idempotent load with a failure ledger
— empty; `:workplan/items` bounded and filterable), warrant b8335c3c0;
futon4a a55daeb (list and expanded item page). Cold load 272 s; 200-item list
0.07 s. Demo restarted on :7094 as transient unit futon4a-workplan-demo.service
wrapping the pid-owned launch script.

Review (claude-13), walked against :7094: grouping repeated headers (it kept
the level sort) and filed each item under its first benefit line only; fixed
in futon4a 01cfb28 with an ERT check that fails on the old code. Item pages,
E-R14's ruling and T-pattern-search's five mentions read correctly.

## Backlinks trial, packet 1 — three entities (accepted after a review fix)

Joe, 11-09: no per-turn link records. Each entity keeps its rule (the words
that name it, on/off) and backlinks are derived. He asked for a trial with a few
known-good words. Measured first (claude-13): one evidence text-search on
:7073 takes ~10 s whatever the limit, so searches run at build time only.

codex-21: futon3c 4e39606ff, warrant 7340378f9 (`backlinks-1-warrant.edn`);
futon4a 2e1bee1. Rules are `[:workplan/entity-match-rule 1]` 'clusions; forms
and switches are attestations, outside identity. Derived index: 4 GETs at
limit 50, 24 occurrences, `index-as-of` 02:19Z. Whole word, case-sensitive,
hyphen is a boundary. A rebuild is byte-identical, and switching "kangaroo" off
drops its 5 and nothing else.

| Form | Hits | Kept | Metadata-only | Occurrences |
|---|---:|---:|---:|---:|
| Agency | 50 | 4 | 46 | 4 |
| Giraffe | 50 | 5 | 45 | 15 |
| kangaroo | 50 | 5 | 45 | 5 |
| Kangaroo | 50 | 0 | 50 | 0 |

Review (claude-13):
- Plants: case-insensitive matching (3 failures) and substring matching (2)
  are caught. A plant that searched the whole entry raised an error rather than
  a test failure, so it is not cleanly shown to be caught.
- Walked against :7094: the agency and giraffe pages render, with `e` on
  backlinks. **kangaroo-pouch failed** with `:arxana.http/internal`: its entity
  record was never written, because no workplan item names it and the rules
  materialiser asserted only rules and attestations. The test had pre-asserted
  every entity, which hid this. Fixed in futon3c 193fa3698: the materialiser
  asserts the entity, and the test no longer pre-asserts. The old code fails
  the new test.
- Limits to carry: (1) the build and the view read the rules from
  `resources/.../entity-backlink-rules.edn`. The stored attestation mirrors the
  file, so switching a form is still a file edit, not an assessment.
  (2) Search hits are relevance-ranked and about 90% match only in metadata, so
  a 50-hit cap keeps 4–5 turns per word, and the agency backlinks are all from
  June. (3) The index is as of 02:19Z.

## sessionista-mode, packet 1 — entity marks in REPL buffers (accepted after review fixes)

Joe, 11-09: a light mode that does only entity marks plus backlinks, as fast as
the as-you-type underlining (no model calls). This is the start of an NNexus-like
linker for agent turns (E-gcc-schema §8).

codex-21: futon4a abb2b11 (`lisp/futon4a-sessionista.el`, warrant
`test/sessionista-1-warrant.edn`, which carries no commit sha). Painting uses
jit-lock, like `session-mode--paint-rnode-tags`. Rules are fetched once from
:7094. Matching is case-sensitive whole words. RET is on the overlay only and
opens the backlinks page. session-mode's entity layer is switched off in the
buffer and restored afterwards. 5,000 lines take 0.03–0.05 s (30,000 marks
checked, so the time is for real painting).

Review (claude-13). Four fixes, each with a test that fails on the old code:
- d3c32b1: the live client reads JSON false as `:json-false` (non-nil), so a
  form switched off would still be painted. The fixture reader parsed false as
  nil, which hid it.
- 0d38946 / ed46eb4: three planted bugs passed the original tests.
  - Case-folding: the first lowercase "agency" in the real text is inside
    agency_send, and marks without an entity were skipped by the check.
  - A stack fetch inside the painter: fixture mode makes no url call.
  - Disabling left a buffer-local nil when the setting was not local before.
- 0a05f49: walked live in `emacs -Q`, the mode never read the demo's ready file,
  so it asked the default URL and painted nothing. Fixture tests cannot see
  this. Also, the operator and agent region lists overlap, so every word got
  two marks.
Live walk after the fixes: Agency, Giraffe and kangaroo (including in M-kangaroo)
are marked once each; agency_send and Giraffes are not. RET on Giraffe opens its
backlinks page.

## sessionista packet 2 — `[[X]]` introductions (accepted after review fixes)

codex-21: futon3c f5a514fb5, a3d871572 (lexicon aliases), warrant 53a8be0da
(`sessionista-2-warrant.edn`, with shas); futon4a 7293b2d. Joe's `[[X]]` /
`[[X | r1, r2]]` on send are anchored to the real `emacs-*` evidence id plus the
codepoint range, through an `:around` advice on `agent-chat--start-turn`
added by the mode (futon3c/emacs untouched). The introduction, referent
(explicit or candidate) and rule records go through public ports. The rules
list is now read from the stored records (the seed file only seeds). The entity
page has `a`/`r`. Fixture: Joe's real turn `emacs-3bb1edfcff4bbe2932e8ab985faf6cfa`.
The explicit case is that turn with one token edited (no real `[[X | r]]` turn
existed).

Review (claude-13):
- **futon3c 00f11cf05:** futon1b's index returns attestations and assessments
  newest first and ignores `:time/order` (`futon1b_arxana_index.clj`), so the
  packet's `(last …)` read the OLDEST.
  - A referent accepted then rejected stayed accepted.
  - A newer forms attestation never took effect.
  - "Any form ever enabled" switched every form on.

  Fixed: the newest ruling decides; an introduced entity is on only while it
  has an accepted referent; a seeded rule keeps each form's own switch from
  its newest attestation. Two tests, both failing on the packet's code.
  Other projections that take `last` of these lists may have the same bug.
  Not yet checked.
- **futon4a a461280:**
  - After an introduction, every sessionista buffer did a forced refresh
    (N+1 requests × ~22 buffers on Joe's send path). Now there is one fetch
    and the shared cache.
  - A `[[…]]` whose turn evidence was not acknowledged was dropped silently.
    Now it gets an echo-area message.
- **Live:** :7094 restarted through its scripts. Its rule list comes from the
  store (agency, giraffe, kangaroo-pouch), and the marks paint. Loaded into
  Joe's Emacs: 22 buffers, one rule fetch, send advice installed.
- **Open:** the demo store is in-memory (`xtn/start-node` with no config,
  `formal_f1_demo.clj:66`), so introductions and rulings are lost whenever
  :7094 restarts.

## Workplan packet 3 — durable :7094 store (accepted)

codex-21: futon3c fa08b1181 (warrant a75a727d2, `workplan-3-warrant.edn`, shas
match); futon4a d35821d. The workplan demo now uses an XTDB local log and
storage, plus `arxana-index.sqlite`, under
`~/.local/share/futon4a/workplan-demo/` (override `FUTON4A_WORKPLAN_DATA_DIR`).
A warm start probes the catalog record and writes nothing. A missing index is
rebuilt before serving. F1–F6 keep fresh stores. Times: cold 323 s, warm 17 s.

Review (claude-13): diff read. Not re-run: the durable-restart test (green in
the warrant; it takes about 6 minutes). Planted bugs not run for this packet.
Noted: a warm start trusts the catalog record alone, so a cold load that died
after writing the catalog would look complete.

Switchover (claude-13): the live :7094 held one introduction (Joe's
`[[Arxana]]`, no referents, no rulings). It was captured, :7094 was stopped
and launched through the scripts (unit futon4a-workplan-demo), and the cold
load created 3,881. The introduction was re-recorded from evidence
`emacs-60640fc27611818aaaf1a6abc8f3e31d` with time 20:08:00Z; the turn's
evidence time is 20:08:53.76Z, so it is 54 s early, entered by hand. Before
futon4a 0457620 the Emacs side sent no time at all. Joe's Emacs was refreshed:
Arxana is pending (dim).

## Workplan packet 4 — the lexicon in the stack (accepted after a review fix, loaded on :7094)

codex-21: futon3c dfbde7e95 (`lexicon_loader.clj`), warrant 858f11eb5
(`workplan-4-warrant.edn`). Input: 254 rows (251 unique slugs; cascade-live,
inbox-zero and holistic-argument appear twice), 158 `@live` and 158 `@tier`,
742 relation targets (589 resolved and written as model/unverified candidate
referents, 153 unresolved and reported, not written), 9 Agency-survey
entities and 10 competing survey corrections.

Review (claude-13), futon3c b3dc73db6: the loader also wrote, for every entity,
a word rule plus a forms attestation (its name, disabled). Because the newest
forms attestation decides (00f11cf05), on :7094 this would have switched
"Agency" and "Giraffe" off, replaced the kangaroo pouch's forms with "Kangaroo
(warm pouch)", and put 251 members in the rule catalog, so sessionista would
fetch every view on each refresh in Joe's Emacs. The fix: no word rules for
lexicon entities. A word rule comes only from Joe's `[[X]]`. The test seeds the
trial rules and checks that the load leaves their attestations unchanged; it
fails on the packet's loader.

Loaded on :7094 (durable): 1,716 records planned, 1,590 created, 0 failures; a
rerun creates 0. Checked afterwards:
- the rule list is still agency, arxana, giraffe and kangaroo-pouch;
- Agency's and the kangaroo pouch's forms are unchanged and on;
- the kangaroo pouch has `part-of`, `in-namespace` and `extends agency`, plus
  `serves`;
- Arxana keeps Joe's introduction and has candidate referents `uses`
  (arxana-store), `part-of` (futon4) and `replaced-by` (jvm-arxana).

### Referents: prepopulated, Joe corrects (11-09, after Joe's first look)

Joe on the Arxana page: three referents all marked "accepted", "uses" with no
object, and "doing this for 100+ items is extremely tedious … better if we
could find a way to prepopulate the graph and let me correct it".
- futon4a 0ae08c2: JSON false arrives as `:json-false`, which is non-nil, so
  every unverified referent showed "accepted" and switched-off forms would
  show `[on]`. Rows now read "arxana uses arxana-store — proposed (from claude-6
  lexicon 2026-11-08)", or "accepted/REJECTED by <actor>".
- futon3c 4a52a6b18: a proposed referent counts until the latest ruling rejects
  it. Each row carries its target slug, proposer and raw text.
- futon3c fd6b4bb40: relations that resolve to their own source through an alias
  are reported, not written. Two were already on :7094 (`arxana part-of
  arxana` from "part-of futon4", and `holistic-argument replaced-by
  holistic-argument`). claude-13 rejected both, with the reason recorded.
- :7094 warm-restarted (load 0, already present); Joe's Emacs reloaded. Arxana
  now has accepted-by-default referents, so its word is on (no longer pending).

## Stepper review section (zai-1, reviewed by claude-13, 2026-11-10)

Joe, 2026-11-10: review the 589 lexicon relations "in the *象 stepper* … in
context". zai-1 built two read views and a REVIEW section in the stepper:
futon3c `7cacf87b1` (on master as `25f79faa8`), futon4a `ab908b6` + warrant
`95fc824` (on main as `01f5c1a`, `719a93f`). The warrant is
`futon4a/test/stepper-review-1-warrant.edn`. futon3c/emacs was not touched.

Review (claude-13):
- Record check: the warrant's shas match the diffs. zai-1's private stack
  :7294 was stopped by zai-1 after it checked /proc cmdline. Its data-root is
  kept because it holds the one recorded test ruling. :7094 was never written.
- Plants, each caught: oldest assessment instead of newest (Clojure test, 1
  of 48 fails); unordered rows (2 ERT fail); a key on the stepper's mode map
  (1 fails); a ruling without frame context (1 fails). Fixture provenance:
  the relation record ids in the recorded fixtures exist on :7094, being
  content-addressed, so they come from a real load.
- Found on the live stack, fixed by claude-13:
  - The Emacs request encoding sent an empty body as `[]` and an alist as a
    vector. Every live call failed and showed as "unreachable". Tests missed
    it because the fixture seam bypasses HTTP. Fixed and pinned by a fifth
    test (futon4a `e076b47`).
  - The index took 44 s on every call, because its 60 s cache was timed from
    the start of a 45 s build. It is now cached for 10 min from the end of
    the build, with one build at a time. Relations took 3–9 s per entity;
    they are now read three at a time (futon3c `c4e05f024`). More than three
    in parallel hits futon1b's 4-permit admission (503), and store_http maps
    503 to `:unavailable`, a category the envelope rejects (AssertionError,
    HTTP 500). That latent bug is not fixed here.
  - The Emacs relations cache is now per entity for 5 min, not per frame
    (futon4a `d7f563c`).
- Measured on :7094 after the fixes: index 19 s cold, 0.0 s warm; relations
  1–3 s each. A real frame (zai-1's bell, 11.9k chars) names 16 entities and
  took 14 s to paint the first time. The result is correct (5 rows, "+155
  more"), but it is too slow for the painting path.
- **Not loaded into Joe's Emacs:** a first visit to a frame would block for
  about 1 s per named entity. Next step: the server keeps every entity's
  relations precomputed (built at startup, cleared on a ruling), and the
  stepper asks for all the frame's entities in one call.

### Painting from a held table (claude-1, 2026-11-10; reviewed by claude-13 the same day)

Joe: "make it as efficient as possible". futon3c `20179c27a`, `fb0aba7ea`;
futon4a `bc32af4`; warrant `futon4a/test/stepper-review-2-warrant.edn`.

- **Service.** `lexicon_views.clj` keeps every lexicon entity and referent
  in one in-memory table. It is built in the background when the demo starts
  (63 s on :7094) and rebuilt every 10 minutes, one read at a time, while a
  stale table is served. `:lexicon/review-table` returns the whole table
  (224 KB, 16 ms), or `:unchanged` for a caller that names the version it
  holds (1 ms), or `:building`. A ruling through `:workplan/assess-referent`
  re-reads that one row's assessments from the store, so the store's
  newest-first order still decides. `:lexicon/entity-index` and
  `:lexicon/entity-relations` answer from the table once it is built (6–7 ms).
- **Emacs.** The section is painted from a local copy of the table, with no
  network call. The copy is fetched and checked asynchronously: on a paint at
  least 5 s after the last check, and every 15 s while a stepper buffer is on
  screen. Before the first table arrives the section is one "loading" line.
  A newer table repaints the stepper and keeps point on its line. A ruling is
  one POST, and the row shows its verdict from the local copy when it returns.
- **Measured on :7094** (frame 0 of `stepper-frames.json`, read-only): first
  visit paints in 7.5 ms with the loading line, and the table arrives 134 ms
  later; later paints take 5–7 ms. Before: 14 s. **On a private stack
  (:7296):** key to repainted verdict 137 ms and 139 ms; a ruling posted
  elsewhere was in the stepper 45 ms after the next check.
- **Bounds for a ruling made elsewhere.** Through this service (an entity
  page): 15 s while the stepper is on screen. Written past the service:
  the next rebuild, up to 10 min plus the rebuild time.
- **Changed behaviour, for Joe to confirm.**
  - A relation is now one row, shown in its own direction. Before, a relation
    with both ends named in the frame appeared twice, and the second copy read
    backwards ("agency extends kangaroo-pouch").
  - Matching is case-sensitive, as the file always said. It had followed the
    buffer's `case-fold-search`, so "whistle" named Whistle and "r1" named R1.
    Frame 0 now names 11 entities, not 16.
    `futon4a-stepper-review-fold-case` restores the old matching.
- **Found and fixed on the way.** Each request's message id was the time in
  whole seconds, so a second ruling within one second shared the first one's
  id and was not recorded, though the reply said ok. Ids are now unique.
- **Not done.** The 503 → `:unavailable` AssertionError in `store_http` is
  still there; the table build retries around it instead. The table is not
  kept on disk, so each restart rebuilds it. Not loaded into Joe's Emacs.

Review (claude-13, 2026-11-10):
- Record check: the warrant `futon4a/test/stepper-review-2-warrant.edn`
  names futon3c `20179c27a`, `fb0aba7ea` and futon4a `bc32af4`, which match
  the diffs. futon3c/emacs is untouched. No ruling was written to :7094.
- The table keeps to E-gcc-schema §15 / B2: it is rebuilt from futon1b on
  start, a ruling re-reads that row's assessments from the store (newest
  first), and it holds nothing the store does not. Reads are not
  idempotency-keyed in futon1b (only writes are), so the table's repeated
  read ids are safe.
- Two more plants of mine, both caught: rulings that land during a build not
  re-read (1 failure, `ruling-during-a-build-is-not-lost`); a version that
  never changes (5 failures). That makes nine plants with claude-1's seven.
- Live on :7094: the table op takes 106 ms for 592 relations (225 kB), of
  which 590 are proposed and 2 rejected. "Unchanged" takes 3.3 ms.
- Loaded into Joe's Emacs (`load-file` of futon4a-stepper-review.el). The
  table is held there (260 entities). Frame 0 of the fixture renders in
  4.9 ms, showing 5 rows and "+110 more", with the directions now right
  ("agency uses futon1b").
- Note for Joe: the stepper's own `r` is rewind. On a REVIEW row the row
  keymap wins, so `r` there means reject; one line off, `r` opens the rewind
  view (which needs `R` to apply).
