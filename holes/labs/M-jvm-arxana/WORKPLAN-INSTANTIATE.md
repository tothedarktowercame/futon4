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
