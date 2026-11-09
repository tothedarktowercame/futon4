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
