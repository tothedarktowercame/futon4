# WebArxana constellation rendering pipeline

The "Interest Constellation" diagram (`/wa#/diagram/<name>/expanded`) is a 2D
force-directed node graph. It looks trivial ("draw a graph") but has a long
chain of stages, several of which can *independently* dominate the visible
result. Multiple past attempts to "make it spread / zoom / pan" failed because
each fixed one stage while another re-confined the output — and some fixes
*introduced* the next symptom. This doc is the end-to-end model so that doesn't
keep happening. Read it before changing layout/camera/interaction behaviour.

## The stages (data → pixels)

1. **Load** — `api/expand-diagram!` reads the diagram entity, resets `:pins []`,
   then fires ~35 `pin-entity!` ego-loads **concurrently**. Each one that lands
   calls `state/pin!` (appends one pin) **and bumps `:_render-tick`**. So the
   graph re-renders many times as pins *stream in*, not once at the end.
2. **Neighbourhood** — `state/multi-neighbourhood` expands pins to nemas+links.
3. **Diagram filter** (`graph-svg` `merged-hood`) — hides the diagram apex,
   `diagram/includes` content, neighbour diagrams, etc.
4. **Kind / view-mode filter** — `apply-kind-filter`, `apply-view-mode-filter`.
5. **Layout** — `cached-positions` → `force-layout` (organic / `:organic`,
   `:zoom-lod`) or `layered-layout` (`:layered`, banded `:witnesses`).
6. **Extraction** — pull `[x y]` out of the d3 sim (NaN-guard only; see I-2).
7. **Camera** — `fit-on-new-graph!` sets `!zoom {k x y}`; `zoom-transform-attr`
   renders it as `translate(x,y) scale(k)` on the `g.graph-zoom-layer`.
8. **viewBox** — the `<svg>` has a *fixed* `viewBox "0 0 1200 800"`. This is the
   **camera frame**, NOT the world (see I-1).
9. **Render** — nodes (`node-component`) and links (`link-component`).

## Invariants (the load-bearing ones)

**I-1 — World ≠ viewport.** The world is large: organic spread radius
≈ `175·√N` (`spread` in `force-layout`). The fixed `viewBox 1200×800` is a
*camera* over that world via the zoom-transform `<g>`. The initial fit zooms
*out* (floor `0.08`) to frame the whole cloud; wheel-zoom + drag-pan navigate.
Symptom when violated: the graph is crushed to fit one screen.

**I-2 — Shape emerges from forces, never from a geometric clamp.** Roundness
comes from the radially-symmetric centre gravity (`forceX`/`forceY` toward the
centre). The extraction step does a **NaN-guard only** — no rectangular or
circular clamp. A geometric clamp *pins nodes onto its boundary*: a rectangular
clamp → "landscape A5 sheet"; a circular clamp → "reshaped into a circle"
(especially mid-load when an intermediate pin-count shrinks the radius). Clamps
are safety, not styling.

**I-3 — Incremental load recomputes per pin; seed from the PREVIOUS layout.**
Because pins stream in (stage 1) and `cached-positions` keys on the node/link
set, the sim re-runs on every arrival. Without a stable seed each re-run
phyllotaxis-reshuffles the whole graph → visible churn ("flashed then reshaped").
`cached-positions` passes the prior positions as `seed-positions`; existing
nodes start where they were (only the new node settles in). The seed is *force*
positions, never the pin grid (see I-4).

**I-4 — Seed unbiased, or centrality lies.** A faithful force layout puts
high-betweenness / articulation nodes in the interior (their links pull them
between clusters). That only happens from an unbiased seed. The old code seeded
pins on an 11×10 **grid** → a grid-shaped local minimum that ignored
connectivity (articulation nodes stranded on the rim). Fixes: brand-new nodes
get d3 phyllotaxis (or the prior-centroid during incremental load), never a grid.

**I-5 — The camera cannot reshape geometry.** `fit`/zoom/pan are a *uniform*
`translate + scale`. If the silhouette *changes shape* (not just size/position),
it is the **layout recomputing** (stage 5/6), not the camera. Use this to
localise bugs: shape change ⇒ look at `force-layout`/clamp/seed, not `!zoom`.

**I-6 — Banded/layered modes are separate geometry.** `:witnesses` (banded) and
`:layered` keep the fixed-canvas rectangular layout and the original rectangular
clamp on purpose (their y-bands are absolute). I-1..I-4 are about `:organic`.

## Interaction (clicks & cards)

- **Node clicks must `stopPropagation` on `pointerdown`.** The `<svg>` runs
  drag-pan via `setPointerCapture`; without stopping the node's pointerdown the
  SVG captures the pointer and the browser dispatches the `click` to the SVG,
  not the node — so node clicks were silently lost. Stopping it keeps pan on the
  empty canvas while letting node clicks through (`node-component`).
- **Focus → raise the card.** Clicking calls `state/pin!` → `set-focus!`. The
  focused card only gets an `active-pin` highlight (`card/pin-card-inner`); the
  long rail does not move on its own. `node-component` calls
  `reveal-pinned-card!`, which `scrollIntoView`s the `.focus-card[data-pin-id=…]`
  (the `data-pin-id` anchor lives on the card div).

## Verification discipline

The bugs here lived in the **transient** (incremental load) and in
**interaction** (clicking) — a screenshot of the settled steady state passed
while the live experience was broken. So:

- Test the **load transient**, not just `t = 5s`. The clamp-ring and the churn
  only show *during* streaming.
- Test **clicks** (focus changes + card scrolls into view), not just render.
- Harnesses: `diagram-verify.mjs` (node/edge counts, labels, screenshots),
  ad-hoc Playwright for click/zoom/pan and geometry (aspect ratio, radius
  coefficient-of-variation — a ring has CV≈0, a filled blob ≈0.4).

## File map

| Concern | Location |
|---|---|
| Force sim, spread, NaN-guard, fit | `src/webarxana/client/graph.cljs` `force-layout`, `fit-transform`, `cached-positions` |
| SVG, viewBox, zoom/pan handlers, node click | `graph.cljs` `graph-svg`, `node-component`, `reveal-pinned-card!` |
| Layered/banded layout | `graph.cljs` `layered-layout`, `band-y` |
| Card rail, `active-pin`, `data-pin-id` | `src/webarxana/client/card.cljs` `focus-card`, `pin-card-inner` |
| Pin/focus state | `src/webarxana/client/state.cljs` `pin!`, `set-focus!` |
| Diagram load (streams pins) | `src/webarxana/client/api.cljs` `expand-diagram!`, `pin-entity!` |
| Route parsing | `src/webarxana/client/route.cljs` |

## Build / hot-reload

Two bundles, picked by `wa.html`'s inline bootstrap on the **route**:

- **Static** (default): `npx shadow-cljs compile app` (standalone
  `webarxana/shadow-cljs.edn`, module `:wa`) → `futon4/data/webarxana/public/wa/wa.js`,
  served at **`/wa/wa.js`** by `server/core.clj` `data-asset-response`. Loaded by
  any normal route, e.g. `#/diagram/Interest Constellation/expanded`.
- **Hot-reload** (opt-in): the in-JVM watch (`futon3c.dev.shadow/start!`,
  `M-x start-shadow`; `futon3c/shadow-cljs.edn` build `:webarxana`, asset-path
  `/wa/dev`) → `futon4/data/webarxana/public/wa/dev/main.js`, served at
  **`/wa/dev/main.js`**. Loaded by a `#/dev/...` route, e.g.
  `#/dev/diagram/Interest Constellation/expanded`. `route.cljs` strips the
  leading `dev` segment (and keeps it sticky via `push-hash!`), so the dev route
  behaves identically but hot-reloads CLJS edits live.

**Gotcha that bit us:** both the war-machine and webarxana watch builds emit
module `:main`. If webarxana's asset-path is `/js` it collides with war-machine
at `/js/main.js` (war-machine wins, served from the classpath), so the webarxana
dev bundle is unreachable. Keeping webarxana's dev bundle under the `/wa/dev`
asset-root (served by `data-asset-response`, never the classpath) avoids the
collision. If you change `:webarxana`'s asset-path, update `wa.html`'s picker
(`s.src = dev ? "/wa/dev/main.js" : ...`) to match.
