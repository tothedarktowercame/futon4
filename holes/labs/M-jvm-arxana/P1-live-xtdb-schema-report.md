# P1 report — live Arxana/XTDB schema census (M-jvm-arxana, MAP)

Worker: zai-2. Date: 2026-11-06. Discovery only: no code, schema, store or
service was changed. All probes were GET-only against the live futon1b JVM.

## 0. What the live path actually is

- The Emacs client is `futon4/dev/arxana-store.el` (base URL default
  `http://localhost:7071/api/alpha`, `arxana-store.el:25-30`; health probe at
  `:184-196`). Port **7071 is not listening** on this host; the live compat
  API is served by the **futon1b JVM (pid 502697) on ports 7072/7073**
  (`ss -tlnp`; `readlink /proc/502697/cwd` → `/home/joe/code/futon1b`).
- The serving code is **futon1b's own** `futon1b_server.clj` /
  `futon1b_graph.clj` / `futon1b_hxindex.clj`, NOT futon1a. futon1a
  (`futon1a.api.routes`, `futon1a.compat.futon1-write`) is a parallel compat
  implementation with near-identical shapes; where they were compared they
  agreed. The packet's "futon1/futon1b routes" resolve to: futon4 client →
  futon1b server.
- Route table (server side): `futon1b_server.clj:879-1133` —
  `/api/alpha/hyperedge[/:id]`, `/api/alpha/hyperedges`, `/api/alpha/entity`,
  `/api/alpha/entities/latest`, `/api/alpha/entities/batch`,
  `/api/alpha/relation(s)[/batch]`, `/api/alpha/evidence`, `/api/alpha/derived`,
  `/api/alpha/memory-assert`, `/api/alpha/census`, `/api/alpha/types`,
  `/api/alpha/graph/inhabited`.

## 1. Shape table (source-cited)

| Shape | Identity keys | Required | Optional / convention | Where enforced |
|---|---|---|---|---|
| Entity (XTDB table `:entities`) | `:xt/id = :entity/id` (requested id → existing by `:entity/name` preferring matching `:entity/type`, else fresh UUID) | `:name`, `:type` | `:entity/external-id`, `:entity/source`, `:entity/props` | `futon1b_graph.clj:563-587` (`build-entity` throws L4 `:missing-required`); written via `put-verified!` (`futon1b_graph.clj:67-96`, read-back verified) into table `:entities` |
| Hyperedge (XTDB table `:hyperedges`) | `:xt/id = :hx/id` (requested id, else stable `hx:<type>:<sorted endpoint ids>`, `futon1b_server.clj:80-82`) | `:hx/type`, non-empty `:hx/endpoints` | `:hx/ends` [{:entity-id :role}], `:hx/labels`, `:hx/props`, `:hx/confidence` (futon1a parity: `futon1a/compat/futon1_write.clj:230-285`), `:hx/valid-time` + `:hx/op "retract"` (`futon1b_server.clj:139-160` → `delete-docs` with `:valid-from`) | `futon1b_server.clj:92-124` (`build-hyperedge-doc`); transform via `xf/transform-doc` shape-log; L4 `:act-options-require-hyperedge-write` guards mint options |
| Relation (compat 2-end event) | `:relation/id` = stable `rel\|src\|type\|dst\|note\|order` | `:type`, resolvable `:src`/`:dst` | `:relation/provenance` {:note :order :props}; stores BOTH `:relation/src|dst` and `:relation/from|to` | `futon1a/compat/futon1_write.clj:141-186`; futon1b `relation-route` `futon1b_server.clj:1091` |
| Scope/mission hyperedge (live second family) | `:hx/id` = `hx\|mission-scope\|<canonical-id>` | `:hx/type` (:mission-scope/mission-scope-in), `:hx/endpoints` | `:prop/*` (binder-type, boundary-text, canonical-id, stable-id, source, resolve-by, polarity, mission, original-id, passage, fingerprint, duplicate-heading?), `:scope/name`, `:hx/labels` | **live-probed** (see §4); producer not located in futon1b_server.clj (see §3 discrepancy D3) |
| Plexus / working set | — | — | — | **No route, no writer, no reader found**: `grep -rn plexus` over `futon4/dev/*.el` returns nothing; absent from `futon1b_server.clj`. Exists only in `futon4/AGENTS.md` logical model and `futon4/docs/tickets/033-nema-article-event-model.md` (prose). Nearest live analogue is the snapshot surface (`arxana-store.el:72-73,551-570`; server `futon1a/api/snapshot.clj`) |

Temporal: hyperedges support `:hx/valid-time` puts and validity-ending
retracts (`futon1b_server.clj:139-160`). Entities: the futon4 client sends
`?version=`/`?as-of=` on GET /entity (`arxana-store.el:855-878`) but the live
futon1b `entity-route` (`futon1b_server.clj:1015-1043`) reads only the id
tail and query params for `source`/`external-id` lookup — **no as-of/version
handling server-side**; entity history route `/entities/history/:id`
(`arxana-store.el:871-880`) was not found in futon1b's route list
(unverified which build served it).

Provenance: entity `:entity/source` + `:entity/external-id`; relation
`:relation/provenance`; hyperedge `:hx/props`. No dedicated provenance/salience
attributes exist in any server-side doc builder.

Salience: the futon4 client posts `seen-count`, `pinned?`, `last-seen`,
`media/sha256` on /entity (`arxana-store.el:594-618`), but futon1b's
`build-entity` (`futon1b_graph.clj:563-587`) stores only id/name/type/
external-id/source/props — **those salience fields are silently dropped**
(client convention only).

Validation layers: futon1b gates L0–L4 (`futon1b_gates.clj`; layered errors
seen live, §4), read-back verification on every entity/hyperedge put
(`put-verified!`), alias warrants for entity identity (live 7072/7073 health
shows `:sidecars {:alias-warrants …}`).

## 2. Three end-to-end traces

1. **Entity** — writer: `arxana-store-ensure-entity` (`arxana-store.el:594`)
   POSTs EDN to `/entity`. Server: `entity-route` POST (`futon1b_server.clj:1019-1024`)
   → `graph/write-entity!` (`futon1b_graph.clj:587-598`) → `build-entity`
   (L4 required-keys, id-ensure) → `put-verified!` into table `:entities`
   with entity-id locks and ent-index hook. Reader: GET `/api/alpha/entity/:id`
   (`futon1b_server.clj:1038-1043`) → `graph/fetch-entity-warranted` →
   `public-entity`. Live probe (§4 E1) round-trips name/type/external-id/source/props.
2. **Typed hyperedge** — writer: `arxana-store--post-hyperedge`
   (`arxana-store.el:730-740`) POSTs `{:hx/type :hx/endpoints :props …}` to
   `/hyperedge`, after Emacs-side `arxana-data-constraints-validate-hyperedge`
   (`arxana-store.el:731-732`). Server: `build-hyperedge-doc`
   (`futon1b_server.clj:92-124`) → transform + no-op guard + verified put into
   `:hyperedges` (`futon1b_server.clj:139-160`), hx-index hook on write/delete.
   Reader: GET `/api/alpha/hyperedge/:id` (`futon1b_server.clj:879-895`) and
   `/api/alpha/hyperedges?type|end=…` (`futon1b_server.clj` hyperedges route;
   bounded by `hyperedge-query-timeout-ms`, futon1a parity
   `futon1a/api/routes.clj:1141-1149`). Live probe (§4 H1).
3. **Plexus / working set** — **trace impossible on the live path**: no writer
   in `arxana-store.el`, no route in `futon1b_server.clj`. The nearest stored
   working-set artifact is the snapshot export/restore pair
   (`arxana-store.el:72-73` scopes "all"/"latest"; server snapshot surface
   `futon1a/api/snapshot.clj:963-981` in futon1a routes). Classified
   **claimed in prose only** (AGENTS.md logical model "Plexus :: a nema that
   names a working set").

## 3. canonical / enforced / convention / prose

- **Canonical in storage**: entity doc `{:xt/id :entity/id :entity/name
  :entity/type [:entity/external-id] [:entity/source] [:entity/props]}` in
  table `:entities`; hyperedge doc `{:xt/id :hx/id :hx/type :hx/endpoints
  :hx/ends [:hx/labels] [:hx/props] [:hx/confidence]}` in table `:hyperedges`;
  relation doc with both key variants; scope-family hyperedges with `:prop/*`.
- **Enforced by validator**: L4 required keys (name/type for entities;
  hx/type + non-empty endpoints for hyperedges; resolvable src/dst for
  relations); L4 `:invalid-hyperedge-end`; L1 `:external-id-ambiguous`;
  read-back verification (L0 `:postcommit-missing-*`).
- **Convention in a client**: salience fields (`seen-count`, `pinned?`,
  `last-seen`) and `media/sha256` posted by futon4 but dropped server-side;
  `?version`/`?as-of` entity fetch params (server ignores); `x-profile`
  header (server echoes `"default"`); `/entities/history/:id` client route.
- **Claimed in prose**: Plexus as a stored record (AGENTS.md, ticket 033);
  "documents in XTDB aligned with code" (AGENTS.md doc-drift discipline —
  unverified here).

Discrepancies (prose vs code vs live):

- **D1** — `futon4-base-url` default port 7071 (`arxana-store.el:25-30`) has
  no listener; live service is futon1b on 7072/7073. Any client relying on the
  default fails silently into the health-probe path.
- **D2** — Emacs `arxana-data-constraints.el` validates hyperedges client-side
  (`arxana-store.el:731-732`) as well as server-side L4: double validation,
  client copy can drift from server gates.
- **D3** — live store contains a second hyperedge family (`hx|mission-scope|…`
  with `:prop/*` attributes, §4 H1) that no futon1b_server.clj writer builds;
  producer unlocated in the read sources (unverified — likely a watcher/scope
  ingest path outside futon1b_server.clj).
- **D4** — hyperedge census scans return `count 0` for every `?type=` probed
  while the `:hx-index` checkpoint in the same response names existing
  hyperedge ids of a different id-family (`hx|mission-scope|…`); the census
  appears blind to the scope family (type-name mismatch, unverified).
- **D5** — `/api/alpha/ego/<name>` and `/api/alpha/meta/model` are wired in
  futon1a's app (`futon1a/http/app.clj:620-640`) but return 404 on the live
  futon1b 7073 service (probed); either futon1b never ported them or the
  running build lags its source (unverified which).

## 4. Live probes (read-only, GET only)

Service: `http://localhost:7073` (futon1b JVM, cwd `/home/joe/code/futon1b`,
also serving 7072). All outputs EDN, truncated for the report.

- P-health: `curl -s http://localhost:7072/health` →
  `{:node-open? true, :sidecars {:alias-warrants {:entries 31, :issued 2380, :honoured 689 …}}`.
- P-census-hx: `curl -s 'http://localhost:7073/api/alpha/census?type=code%2Fv05%2Frelated-mission'` →
  `{:type "code/v05/related-mission", :kind :hyperedge, :count 0, :hx-index {:checkpoint {:ts "2026-11-06T16:24:47.176951Z[UTC]", :id "hx|mission-scope|usage-hacking/scope-in/…"}}}`
  (count 0 despite live hyperedges of the scope family — D4).
- P-census-ent-type: `…/census?entity-type=article` →
  `{:error {:layer 4, :reason :missing-required, :context {:required ["type"]}}}`
  — the running build appears to accept only `?type=` for census (unverified).
- E1 entity: `curl -s 'http://localhost:7073/api/alpha/entities/latest?type=pattern%2Flibrary&limit=1'` →
  `{:profile "default", :type "pattern/library", :entities [{:id "agency/bounded-lifecycle", :name "agency/bounded-lifecycle", :type :pattern/library, :external-id "agency/bounded-lifecycle", :source "Every Resource Has a Bounded Lifecycle", :props {:pattern/cross-list []}, :sigiled? false}], :ent-index {…}, :sigil-join {:patterns 1773, :relation-srcs 352, :matched 352}}`
  (redacted: trailing index metadata). Follow-up GET
  `/api/alpha/entity/agency%2Fbounded-lifecycle` returned the same doc shape —
  writer→store→reader round-trip confirmed.
- H1 hyperedge: `curl -s 'http://localhost:7073/api/alpha/hyperedge/hx%7Cmission-scope%7Cusage-hacking%2Fscope-in%2Fa-replay-harness…'` →
  `{:prop/binder-type "mission-scope-in", :prop/canonical-id "usage-hacking/scope-in/…", :hx/endpoints ["futon0-d/mission/usage-hacking" "usage-hacking/scope-in/…" "scope/bounded-item/…-e6737e4bf0" "scope/concept/evidence" "scope/concept/proof" "scope/concept/agents"], :hx/id "hx|mission-scope|…", :hx/labels [:mission-scope/mission-scope-in], :prop/mission "M-usage-hacking", :prop/passage "- A replay harness that evaluates candidate scheduling policies against", :hx/type :mission-scope/mission-scope-in, :prop/fingerprint "e76ca9eabe1944d5", …}`
  (long ids elided with `…`; full values retained in the probe transcript above).
- H-scan: `…/hyperedges?include-total=true&limit=1&type=mission-scope` →
  `{:hyperedges [], :count 0, :count-exact? true, …}`; `?type=address` same.
  Repeated probes sometimes returned `{:ok false, :error :expensive-read-busy,
  :retry-after-seconds 1}` — the expensive-read gate is live and active.
- E2 scope entity: `GET /api/alpha/entity/scope%2Fconcept%2Fproof` →
  `{:entity {:id "scope/concept/proof", :name "proof", :type :concept, :props {:scope/role "concept"}}}`.
- 404s: `/api/alpha/ego/arxana`, `/api/alpha/meta/model` → `404 No context
  found for request` (D5).

No POST/transact/repair was performed at any point.

## 5. ready / missing (Q1 only)

| Item | Status |
|---|---|
| Entity shape (canonical, enforced, live-verified) | **ready** |
| Hyperedge shape, compat family (canonical, enforced) | **ready** (live example of the scope family verified; compat-family live example not captured — scan gated) |
| Relation shape (canonical, enforced) | ready (source-cited; live probe not run — no read route probed) |
| Temporal (valid-time on hyperedges) | ready (source-cited; live probe not run) |
| Temporal on entities (as-of/version) | **missing** server-side (client convention only) |
| Salience fields | **missing** server-side (dropped) |
| Plexus/working-set record | **missing** entirely (prose only; snapshot is the nearest analogue) |
| Census correctness for the scope hyperedge family | **missing** (D4) |
| Client default endpoint (7071) | **missing/stale** (D1) |

## 6. Principal finding

The live canonical shapes are the futon1b `:entities`/`:hyperedges` table
docs (entity: id/name/type[/external-id/source/props]; hyperedge:
hx/id/type/endpoints/ends[/labels/props/confidence/valid-time]) — while
salience fields, entity time-travel, and the Plexus record exist only as
Emacs-side conventions or prose, and the live store already carries a second,
producer-unlocated hyperedge family (`hx|mission-scope|…` with `:prop/*`)
that the census counts as zero.
