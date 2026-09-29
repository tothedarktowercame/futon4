# T-arxana-direct-reads — Arxana browses futon1b hyperedges live, not via exported files

**Opened:** 2026-09-29 · claude-4, from Joe: the exporter below is a stopgap;
"we both had the conception in mind that Arxana would be able to read directly
from futon1b."

**Status:** open, parked. The file exporter ships first (lean-docs-align, the
`lean/doc-source` edges) so browsing is not blocked on this.

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
