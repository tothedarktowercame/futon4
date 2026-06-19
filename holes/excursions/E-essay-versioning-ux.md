# E-essay-versioning-ux

**Owner (implementer):** codex-1 · **Reviewer:** claude-7 · **Opened:** 2026-06-06
**Surface:** WebArxana (futon4) + Arxana store (futon1a).

## Why

eoi-next has been minting essay variants (the Anthropic-Fellows family) with
unsortable labels wired as a *star into v1*, so the operator (Joe) cannot tell
which essay is newest nor walk back through versions. The data has been cleaned
(see below); this excursion delivers the **UI + storage edge** so the version
lineage is legible and navigable.

### Already done (claude-7, do NOT redo)
- The 4 fellows essays now carry head+chain PROPS in the store (verified live):
  `:family "anthropic-fellows-2026"`, `:version` (1..4 int), `:created`,
  `:head` (t on v4 only), `:supersedes` (prev essay-id), `:superseded-by`
  (next essay-id). Sortable `:label` set; v4 (`form-fill-final`) is HEAD.
  Chain: v1 → v2 → v3 → v4.
  - ids: `arxana/essay/anthropic-fellows-2026-v1` (v1),
    `.../anthropic-fellows-2026-form-fill-v2` (v2),
    `.../anthropic-fellows-2026-form-fill-v3` (v3),
    `.../anthropic-fellows-2026-form-fill-final` (v4 HEAD).
- eoi-next's EXIT CRITERION already mints this convention for *future* runs
  (`futon0/scripts/eoi-next`). Out of scope here.

## Operator's spec (verbatim intent)
> `arxana://view/essays-home` will show **one** outbound link per each essay,
> with a new **version #** column. When I click through to the essay outline, I
> should have a link (at the end) to **browse earlier versions** (if any).
> Also: get the **graph-edge content working properly in storage**.

## Track B — STORAGE (do FIRST; it's the foundation) — futon1a

**Root cause (already diagnosed):** `relation-endpoint-id`
(`futon1a/src/futon1a/compat/futon1_write.clj:96-108`) resolves a *string*
src/dst only via `entity-by-name` or `uuid-string?` — never by an existing
`:entity/id`. Our essay-ids ("arxana/essay/...") are neither a name nor a UUID,
so both endpoints resolve to nil → `upsert-relation-doc` throws → the generic
`{:reason :unknown}` 500. (The `(map? v)` branch already accepts `{:id "..."}`
without resolution — that is why bare `{:id}` maps "work" today.)

**Do:**
1. Add an `entity-by-id` helper near `entity-by-name` (same file): xtdb query
   `'{:find [(pull e [...])] :in [id] :where [[e :entity/id id]]}`, return the
   canonical doc (one match expected; if multiple, lexicographically-smallest
   `:entity/id`, mirroring `entity-by-name`'s stability rule).
2. In `relation-endpoint-id`, the `(string? v)` branch: try `entity-by-id`
   BETWEEN the name lookup and the UUID fallback —
   `(or (some-> (entity-by-name node s) :entity/id str)
        (some-> (entity-by-id node s) :entity/id str)
        (when (uuid-string? s) s))`.
   Do not weaken: still require the id to correspond to an existing entity
   (no blind pass-through of arbitrary strings). The `(map? v)` `:id`/`:entity/id`
   path should likewise be tightened to resolve-or-reject if cheap, but do not
   break existing callers — if unsure, leave the map path and only add the
   string-id resolution.
3. Test (futon1a test ns): a relation POSTed with src/dst as entity-id STRINGS
   for two existing entities (a) returns success (not `:unknown`), (b) persists,
   (c) is walkable — appears in `/ego/<name>` of the src entity's name. Cover the
   negative: a string that is neither name, entity-id, nor UUID still rejects.
4. Wire the live supersedes chain for the fellows family (newer
   `--arxana/supersedes--> older`, label "supersedes"): v4→v3, v3→v2, v2→v1.
   Verify each appears in `/ego` of the newer essay's name. (You may use
   `arxana-store-create-relation` from elisp once the server resolves id-strings,
   or POST `/relation` directly.)

**Reload:** futon1a/futon3c run in the serving JVM. **NEVER restart it.** Reload
the changed ns via Drawbridge nREPL (`futon3c/scripts/proof-eval.sh` /
`futon3c/README-drawbridge.md`, nREPL @6768) — first-party ns only, reload only
your edits (see futon3c CLAUDE.md / the never-restart rule). Verify live.

## Track A — UI — WebArxana (futon4/dev/web/webarxana)

**`arxana://view/essays-home` does NOT exist yet.** Build it modeled on the
server-rendered surface precedent
`futon4/dev/web/webarxana/src/webarxana/server/interest_network.clj`
(`arxana://view/interest-network`). Find how that view's route is registered
(likely `server/core.clj`) and register `essays-home` the same way. Prefer a
**server-rendered** surface (no CLJS) to match the precedent; only touch CLJS if
genuinely required.

1. **essays-home surface:** query entities of type `arxana/essay`; group by prop
   `:family`; render ONE row per family = the **head** (prop `:head` truthy;
   fallback: max `:version`). Essays with no `:family` form their own single-row
   family. Each row: a **version # column** (prop `:version`) + the essay
   label/name + exactly **one outbound link** to that essay's outline. (For the
   fellows family this yields one row: "v4 ... ← HEAD", version 4.)
2. **Essay outline + "Browse earlier versions":** locate the click-through
   target of an essays-home row (the essay outline). The web side may not have a
   dedicated essay-outline surface today — essays open via `entity-location`
   `arxana://essay/<id>` / canvas pin (`api.cljs` `browse-and-focus!`,
   `expand-essay!`; `card.cljs`). **First discover and DOCUMENT the actual
   click-through target**, then: at the END of that outline/view, when the essay
   has a `:supersedes` prop (earlier versions exist), render a **"Browse earlier
   versions"** affordance that walks `:supersedes` back (v4→v3→v2→v1) — either an
   inline list of the prior versions (each linking to its own outline) or a link
   to a family-filtered essays view. Newest-first.

**Anchors (from scouting; verify before editing):**
- essays list render / columns: `client/card.cljs:530-567`
- essay query: `client/api.cljs:479-489` (`browse-type!`,
  `GET /entities/latest?type=arxana/essay`)
- entity props accessor: ingested as `:nema/props` (`client/state.cljs:184-200`);
  `client/graph.cljs:32-44` `prop-value`/`numeric-prop`
- essay open click: `client/card.cljs:542` → `api/browse-and-focus!`
  (`client/api.cljs:491-516`); `expand-essay!` `client/api.cljs:230-274`
- intra-app links: `client/api.cljs:14-32` `entity-location`
  (`arxana://essay/<id>`)
- server view precedent: `server/interest_network.clj`; route reg in `server/core.clj`

**CLJS build (only if CLJS touched):** `npx shadow-cljs compile app` from
`futon4/dev/web/webarxana/`. Do NOT start a long-running `watch` (forks a 2nd
JVM — see futon4 CLAUDE.md I-0). futon3c serves the compiled `main.js`.

## Acceptance bar
- `arxana://view/essays-home` renders one row per essay-family (head only) with a
  version # column and a working outline link. Fellows family → exactly one row
  (v4 HEAD, version 4).
- The v4 outline (or its discovered click-through target) shows a "Browse earlier
  versions" affordance at the end; following it surfaces v3, v2, v1.
- `POST /relation` with entity-id-string src/dst SUCCEEDS; the supersedes chain
  edges exist and appear in `/ego` of v4's name.
- All changes reloaded via Drawbridge; serving JVM never restarted; verified live.

## Gates (must clear before belling back)
- clj-kondo clean on changed Clojure (futon1a + webarxana server).
- `futon4/dev/check-parens.el` on changed Clojure/Lisp.
- Tests: the new/existing futon1a relation test green; webarxana server tests
  (e.g. interest_network / mission_search ns) + the arxana essays/diachronic
  tests green. Record counts.
- Live verification of the acceptance bar (curl the surfaces / `/ego`; Playwright
  optional for the rendered page).

## Return
Bell **claude-7** back with: a summary of what changed, the **commit shas**
(futon4 + futon1a), the test counts, and the documented click-through target you
found for the outline affordance.
