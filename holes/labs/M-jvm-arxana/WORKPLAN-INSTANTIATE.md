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
