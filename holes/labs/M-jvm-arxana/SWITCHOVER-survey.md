# Legacy Emacs Arxana → futon4a switchover survey

Date: 2026-11-09.  This is a read-only survey.  No store or Emacs state was
changed.  The legacy surface is 102 `dev/arxana*.el` files (`rg --files dev |
rg '/arxana[^/]*\.el$|arxana.*\.el$' | wc -l`), rather than the approximate
105 in the request.

## Runtime and protocol baseline

The running daemon is `/usr/local/bin/emacs --fg-daemon --with-profile graph`
(PID found with `ps -eo pid,args | rg '[e]macs.*(daemon|server)'`).  All daemon
observations below came from read-only `emacsclient --eval` calls using only
`featurep`, `boundp`, variable reads, and `where-is-internal`.

The daemon has stale loaded configuration:

| variable | daemon value | source evidence |
|---|---|---|
| `futon4-base-url` | `http://127.0.0.1:7071/api/alpha` | `dev/arxana-store.el:25-31` now documents `:7073`; the loaded value predates that correction |
| `arxana-evidence-server` | `http://localhost:7071/api/alpha` | `dev/arxana-browser-evidence.el:29-42` |
| `arxana-lab-futon1-server` | `http://localhost:7071/api/alpha` | daemon variable read; its evidence browser delegates to the same legacy API |

Port 7071 refused a bounded `curl --connect-timeout 2`; `:7073/api/alpha/types`
answered with `Accept: application/edn`, and `:7070/api/alpha/evidence?limit=1`
answered 200.  Thus changing one base URL cannot repair all clients: graph data
is on 7073 while evidence is on 7070.

The JVM registry inspected at `futon3c/src/futon3c/arxana/schema.clj` exposes
`entity/get`, bounded `graph/query`, revision get/head/ingest, scope resolution,
correspondence, typed clusion/evidence/assessment writes, text editing, and the
versioned F1–F6 projections.  It does **not** expose the legacy generic entity,
relation, DocBook, song-catalog, or tail/ego protocols.

## Feature inventory, runtime use, and futon4a coverage

“Loaded” is the result of `(featurep '...)` in emacs-graph.  A query of
`where-is-internal` found no global binding for the representative entry
commands below; the listed keys are mode-local source bindings.

| group | user entry points and local keys | legacy calls / shapes | loaded? | futon4a coverage |
|---|---|---|---|---|
| Store/browser shell | `arxana-browser-browse` (`dev/arxana-browser.el:24`), `arxana-xtdb-browse`; XTDB mode `g` refresh, `l` limit, `RET` ego, `s`/`t` source/target, `R` raw (`arxana-xtdb-browse.el:139-146`) | `arxana-store.el`: GET `/entities/latest?type&limit`, `/entity/:id`, `/ego/:id`, `/tail`, `/types`, `/alpha/hyperedges?type|end&limit`; POST `/entity`, `/relation`, `/hyperedge` | `arxana-browser`, `arxana-browser-core`, `arxana-store`, `arxana-xtdb-browse` yes | **Partial.** `entity/get` and bounded `graph/query` cover new Arxana records, not legacy generic rows; no generic tail/type/ego projection. |
| Links, surface forms, anchors | `arxana-links-edit-surface-form[-at-point]`, `arxana-links-surface-forms-for-concept`, `arxana-links-demo-promote-link`, `arxana-links-demo-verify-anchor`, `arxana-links-status` (`dev/arxana-links.el`) | POST `/entity` at lines 225/404; GET `/entities/latest` at 334,417,435,625,793,817,864. Types include `arxana/link-strategy` and other `arxana/*`; surface forms and strategies are entity props. | yes | **Partial.** `scope/resolve`, `scope/at-position`, `assertion/promote`, and correspondence operations preserve the core anchor behaviour, but legacy entity shapes need an adapter and the editing UI is absent. |
| Scholia | `make-scholium`, `make-scholium-about-part-of-current-article`, `arxana-scholium-compose[-from-region]`, `...show-for-doc`, `...at-point`, `...edit-at-point`; display `RET`, `t`, left/q; draft `C-c C-c` publish, `C-c C-k` cancel (`dev/arxana-scholium.el:60-67,185-190`) | entity upsert plus POST `/relation` with type `arxana/scholium`, `src`, `dst`, and label/provenance props (`arxana-store.el`) | yes | **Missing as a product feature.** The natural core is a declared scholium clusion plus `clusion/assert`, `evidence/append`, `graph/query`; declaration, migration, compose and projection are absent. |
| DocBook | `arxana-docbook-browse[-futon4]`, `...open-entry-object`, `...open-section-context`, `...open-raw`, `...copy-location`, checkout/review/stub commands; entry keys `RET/o`, `M-n/p`, `M-y`, left, `C-c C-s/e` (`arxana-docbook-ui.el:1504-1513`) | GET `/docs/:doc/toc`, `/contents`, `/heading/:id`, `/recent`; POST `/docs/:doc/entry`; DELETE entry paths (`arxana-docbook-ui.el`, `arxana-docbook-remote.el`) | `arxana-docbook-ui`, checkout and remote yes | **Missing.** Revisions can ingest documents, but there is no DocBook TOC/entry operation, record declaration, or projection. |
| Songs/media annotation | `arxana-browser-songs-import-suite-file`, `...import-demo-annotations`, `...open`, `...toggle-highlight-scope`; text/notes activation commands (`dev/arxana-browser-songs.el:276-1462`) | entity latest/get for `arxana/song`, `arxana/media-track`, `arxana/media-lyrics`; annotations are `/hyperedge` records such as `arxana/annotation`; `/media/lyrics` also exists in `arxana-store.el` | no (`arxana-browser-songs` and browser-media false) | **Missing.** Scope/correspondence primitives are reusable, but no catalog/media types, annotation adapter, synchronized source/notes projection, or import command exists. |
| Evidence timeline and Lab | Evidence timeline/session/thread readers in `dev/arxana-browser-evidence.el`; `arxana-lab-browse-evidence-chain`, session/raw/draft/file viewers in `dev/arxana-lab.el` | GETs under `/evidence` and evidence-id/session queries against `arxana-evidence-server`; Lab also reads local trace files | both yes | **Partial.** `evidence/append`, `graph/query`, and F1/F3/F4 standing views cover typed Arxana evidence. They do not replace the general Agency evidence timeline at `:7070`; a read adapter/projection is needed, not migration of Agency evidence. |
| Hyperedge/three-column views | `arxana-xtdb-browse`, `arxana-hypergraph-demo-open[-default]`; mission documents say `M-x arxana-browse` → Violations (`holes/missions/M-three-column-stack.md:1080-1140`) | generic `/hyperedge`; documented types include `math/iatc` and `cross/project-math-link`; endpoint arrays plus props | XTDB browser yes; hypergraph demo no | **Partial.** Bounded role-aware `graph/query` works for declared clusions only. Legacy generic endpoint lists require a read adapter or typed migration; there is no three-column invariant/violation projection. |
| Remaining browser satellites | Main browser also composes essays, patterns, missions, code, forums, media and relations (`dev/arxana-browser-*.el`, `arxana-relations.el`) | mostly legacy entity latest/get, generic hyperedges and local files | `arxana-relations` yes; loading varies by satellite | **Missing/partial by family.** These should not be silently included in “Arxana”: each needs a separate record/projection decision. |

futon4a itself is loaded together with `futon4a-formal-f1` through `-f6`.
Its current public commands cover capabilities, resolve, correspondence,
subscription, F1 conversation/outline/GCC, F2 collaborative text, F3 maths
correspondence, F4 composites, F5 geometry, and F6 argument layout (files
`futon4a/lisp/futon4a-arxana.el` and `futon4a-formal-f1.el` … `-f6.el`).
That is strong semantic coverage but not feature parity with the legacy
general-purpose browser.

## Live data census (read-only, bounded)

For every `arxana/*` type declared by `GET :7073/api/alpha/types`, I ran:

```
GET /api/alpha/entities/latest?type=<url-encoded-type>&limit=1000
Accept: application/json
```

The sum is **455 entities** across 26 non-empty types: alignment-source 12,
checkpoint 1, chorus 1, constellation-star 35, debug 1, design-pattern 1,
eoi-editorial-layer 1, essay 17, essay-section 128, figure 22,
identity-proposal 8, interest-star 40, link-strategy 1, media-lyrics 26,
media-track 83, metaphor 1, proforma 1, proforma-coverage-cluster 1,
proforma-criterion 30, proforma-gap 4, proforma-gap-cluster 1, raw-interest
12, song 19, stack-node 7, tension 1, venue 1.  These are real legacy data,
but `entity/get` in JVM Arxana addresses the reserved record tables rather
than these generic entities: they need a bounded compatibility adapter or an
explicit, provenance-preserving migration.

| family | bounded live query/result | disposition |
|---|---|---|
| songs/media | `/entities/latest?type=arxana%2Fsong&limit=1000` → 19; media-track → 83; media-lyrics → 26 | exists; adapter/migration required |
| scholia | `/hyperedges?type=arxana%2Fscholium&limit=1000` → 0; same entity query → 0 | no live rows found; declare new type before future writes |
| song annotations | `/hyperedges?type=arxana%2Fannotation&limit=1000` → 0 | no live rows found |
| DocBook | `/docs/futon4/contents` and `/docs/futon3/contents` → 404 | no live DocBook route; underlying historical rows were not enumerable through a bounded family query, so presence is **unchecked** |
| three-column | bounded queries require exact types; the mission names `math/iatc` and `cross/project-math-link`, but a complete canonical type set was not found | data presence **unchecked**; do not infer absence from the two examples |

## Breakage audit

1. **Corrected:** not every source URL points to 7071.  The running daemon's
   `futon4-base-url` does, while current `arxana-store.el:30-31` documents
   7073.  The loaded client is still broken until deliberately reconfigured.
2. **Confirmed:** `arxana-evidence-server` is 7071 in source and daemon;
   evidence is live at `:7070/api/alpha/evidence`.
3. **Confirmed:** daemon `arxana-lab-futon1-server` is 7071.
4. **Confirmed:** scholium publishing traverses `arxana-store`; no 7071
   listener, and generic `arxana/*` writes would now be refused on 7073.
5. **Confirmed route breakage:** both tested `/docs/.../contents` routes on
   7073 are 404.  Historical DocBook data presence remains unchecked.
6. **Corrected:** songs do exist on 7073 (19/83/26 rows above), but the songs
   feature is not loaded and its legacy protocol is not a futon4a protocol.
   Three-column data remains unchecked rather than presumed absent.
7. **Confirmed but retired:** `futon5a/scripts/hinge_log_bridge.clj:44`
   defaults to 7071; its header explicitly says DEPRECATED/DO NOT RUN and
   explains the incompatible identifier convention.
8. **Confirmed:** `curl :3100/` was connection-refused and `GET :7070/wa/`
   returned 404.  WebArxana source still defaults to 7071 at
   `dev/web/webarxana/src/webarxana/server/core.clj:47`.
9. **Confirmed:** documentation disagrees.  For example
   `README-docbook.md:41` uses 7071, while `arxana-store.el` and
   `holes/tickets/T-arxana-direct-reads.md:21-22` name 7073.
10. **Confirmed from code, without a write probe:** `arxana-links.el` POSTs
    `/entity`; `futon1b_gates.clj:79-88` rejects reserved `arxana/*` on generic
    routes, while `futon1b_server.clj:1036-1046` says the authenticated
    `/api/alpha/arxana/command` route is the only writer.  Legacy link writes
    therefore cannot be repaired by changing the port.

## Proposed follow-on packets

1. **Endpoint discovery/read split:** remove 7071 assumptions and add explicit
   graph/evidence discovery plus typed refusal.  No live-store write.
2. **Legacy read adapter:** bounded read-only projection for the 455 generic
   `arxana/*` entities and selected generic hyperedges; prove songs and browser
   navigation before migrating anything.  No live-store write.
3. **Links/surface forms:** declare the minimum typed clusions/entities and
   port the edit/status UI; dry-run migration report first.  **Any migration
   or promotion needs Joe's approval for live :7073 writes.**
4. **Scholium:** declare scholium and thread types, compose/read projection,
   then enable publishing.  **New live writes require Joe's decision.**
5. **Songs/media annotations:** catalog adapter followed by synchronized
   source/notes projection; import remains disabled.  **Imports/migration need
   Joe's decision.**
6. **Evidence/Lab:** point the general evidence reader at 7070 and keep Agency
   evidence external; add an Arxana standing projection rather than copying it.
   No live-store write.
7. **DocBook:** decide whether it is an artifact/revision projection or a
   distinct declared model; only then build TOC/entry operations.  **Data
   migration needs Joe's decision.**
8. **Three-column/invariant views:** first inventory exact live types, then
   adapt generic edges or declare clusions and a violation projection.
   **Migration/remediation writes need Joe's decision.**
9. **Retire legacy UI:** only after command-level parity and recorded data
   decisions; archive WebArxana and the deprecated hinge bridge separately.

The smallest safe first correction is therefore endpoint discovery, not a
bulk replacement of 7071 with 7073.  The first useful parity slice is the
read-only legacy entity adapter; it exposes what must be preserved before any
live write or migration is authorized.
