# T-arxana-direct-reads — Arxana browses futon1b hyperedges live, not via exported files

**Opened:** 2026-09-29 · claude-4, from Joe: the exporter below is a stopgap;
"we both had the conception in mind that Arxana would be able to read directly
from futon1b."

**Status:** open, blocked-on-live-store (re-derived 2026-11-04, enacting
process-coherence/status-refresh-before-work; prior banner "open, parked /
browsing unblocked" was stale). Checked: futon1b :7073 is listening but
returns 404 ("No context found for request") on `/`, `/hyperedges?type=lean/doc-source`,
and `/hyperedge`, so "the client can already fetch them" is not currently
verifiable live; `futon4-base-url`'s docstring still names :7071, which has no
listener (missing item 3 confirmed open); `arxana-store--default-headers`
always prepends `Accept: application/json` (item 4 partly addressed in code,
interactive url.el behavior unverified); body re-read — no in-body ruling
revises the banner. Next move: restore or relocate the hyperedge serving
route on futon1b and fix the stale base-URL default. Full check record:
futon2 `data/wm-d-task-enactment/T-arxana-direct-reads/status-refresh-2026-11-04.edn`
(commit fbe2d376).
Item 3 addressed 2026-11-14 (wm-author, full-loop attempt-001): the
`futon4-base-url` docstring example now names futon1b :7073; the remaining
four completion criteria are untouched and still open — the :7073
`/hyperedges` route still returns 404, so the live-view, endpoint-open,
planted-edge, and interactive-Accept checks remain **Not met.**

Progress 2026-11-07 (wm-author, full-loop attempt-002): the client
half of the route move is done. futon1b's live serving routes are
`/api/alpha/hyperedges` and `/api/alpha/hyperedge/<id>` (the bare
`/hyperedges` and `/hyperedge` paths have no context and 404); restoring
those bare routes needs a futon1b restart, which this attempt does not do.
Instead `arxana-store.el` was relocated to the live routes
(`arxana-store-fetch-hyperedges` → `/alpha/hyperedges`,
`arxana-store-fetch-hyperedge` → `/alpha/hyperedge/<id>`;
`arxana-store--normalize-base` now canonicalizes any `/api/alpha` base back
to the `/api` mount). Live batch verification through the url.el request
path: `arxana-store-fetch-hyperedges :type "lean/doc-source" :limit 2`
returns `first-id=lean/doc-source/borrow-inference-fixpoint`, and
`arxana-store-fetch-hyperedge` of that id returns the edge — no export
step. The same edit binds `url-mime-accept-string` to `application/json`
in `arxana-store--request`, so url.el sends the JSON Accept itself instead
of a losing `Accept: */*` ahead of the explicit header; verified in batch
(the JSON parse previously failed on the EDN reply and now succeeds).
Interactive M-x use is still unexercised, and the M-x hypergraph view,
endpoint-open, and planted-edge checks remain **Not met.**

## What exists

- **Edges are in the store.** 13 `lean/doc-source` hyperedges in futon1b
  (:7073), one per row of `lean-docs-align/registry/counting-immutable-beans.edn`,
  loaded and verified by `lean-docs-align/arxana/load.bb` (commits 8484f6c,
  c9f51d9). This is the first alignment of docs to *external* code in Arxana.
- **The client can already fetch them.** `arxana-store-fetch-hyperedges`
  (`futon4/dev/arxana-store.el`) with `:type "lean/doc-source"` returns all 13,
  and `:end "lean4:<commit>:<file>"` returns the rows for one source file
  (`lean-docs-align/arxana/batch-read.el`).
- **One browser already reads hyperedges live.** `arxana-browser-songs.el`
  calls `arxana-store-fetch-hyperedges` (:1134) and unwraps the response
  (`--unwrap-hyperedges`, :877). That is the pattern to generalise.

## What is missing

1. **The hypergraph browser reads only files.** `arxana-browser-hypergraph.el`
   renders exported `*-hypergraph.json` from disk (`--read-json-file`,
   `--companion-path`). Its edge model (`edges`/`hyperedges`, `ends`) has the
   same shape as the store's, so a live source could feed the same renderer.
   The file exporter is the interim bridge.
2. **Endpoints are not entities.** Endpoints are typed ids
   (`paper:cib2019`, `lean4:<commit>:<file>`, `lcnf-pass:<box-id>`), so no view
   can open an endpoint as a document. Options: resolve typed ids client-side
   (open the file at the commit, the PDF text at the line range), or create
   entity documents for them. **Caution:** futon1b entity PUT replaces the whole
   document, so entity creation needs its own design, not a side effect of a
   browser change.
3. **Base URL default is stale.** `futon4-base-url`'s docstring example is
   futon1a :7071, which no longer answers; hyperedges live in futon1b :7073.
4. **Accept header in batch/`url.el` callers.** `url.el` sends its own
   `Accept: */*` first and futon1b reads only the first Accept header, so it
   replies in EDN and a JSON-expecting caller fails to parse. Batch scripts
   set `url-mime-accept-string` to `application/json`
   (lean-docs-align DISCOVERY.md). Check whether `arxana-store--request`'s
   explicit header (:235) always wins in interactive use.

## Done when

`M-x` (some Arxana hypergraph view) with a hyperedge type, or an endpoint id,
renders the live edges from :7073 with no export step, and following a
`source` endpoint opens the file at the pinned commit and line range.
Acceptance includes a planted case: an edge written after the view was opened
appears on refresh, which a file export cannot do.

## Why it matters beyond browsing

The diagram prover may use these edges as a layer over the code: each edge
ties a published claim to compiler pass boxes, and many claims assert
dependencies between passes (borrow inference feeds RC insertion). A live
read path is what lets that checker and a human look at the same edges.

## Completion criteria

- An Arxana hypergraph view (M-x) with a hyperedge type or endpoint id renders live edges from futon1b with no export step — check: the route answers (curl `/hyperedges?type=lean/doc-source` on the serving port returns entries, not 404) and the view renders them **Not met.**
- Following a `source` endpoint opens the file at the pinned commit and line range — check: interactive open of a `lean4:<commit>:<file>` endpoint in the view **Not met.**
- The planted case: an edge written after the view was opened appears on refresh — check: a dated note here recording the planted-edge refresh **Not met.**
- The stale base-URL default is fixed (`futon4-base-url` docstring/default names the live futon1b port) — check: `/home/joe/code/futon4/dev/arxana-store.el` **Met.**
- Interactive `url.el` callers get JSON when they ask for it (Accept-header precedence verified in interactive use) — check: a dated note here or in lean-docs-align DISCOVERY.md **Not met.**
