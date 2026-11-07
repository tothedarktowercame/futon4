# MAP Q6 — Recursive edges

**Mission:** M-formal-arxana · **Date:** 2026-11-07 · **Surveyor:** zai-9

## Answer (≤15 lines)

- **futon1b hyperedges: YES, already works end-to-end.** A hyperedge id is a
  legal endpoint (bare string or role-map form); the write route accepts it,
  the stored doc preserves it, and `?end=` / type+end queries find edges
  pointing at it. Endpoints are opaque entity-id strings with **no existence
  check** — recursion is free because references are never resolved at write
  time (probed hermetically; see Evidence P1).
- **futon3c JVM assertions: NO, by design.** `validate-endpoint!` requires
  every assertion endpoint to be a *registered manifestation* id; an
  assertion id in `:manifestation/id` is a typed refusal
  `:arxana/manifestation-unknown` (probed, P2). Assertions cannot nest.
- **futon3c assessments: YES, but type-blind.** An assessment's subject is
  just a string under `:subject :assertion/id` with **no existence check**;
  an assessment id (or a ghost id) is accepted and `assessments-for` finds
  it (probed, P2). So assessment→assessment is possible but rides an
  untyped field — nothing distinguishes it from a dangling reference.
- **Emacs (futon4a): neither.** The Emacs client is display-only (renders
  resolve/correspondence results); it mints nothing and has no local link
  store, so it neither enables nor blocks recursion.
- **What breaks:** (1) futon1b end-lookup resolves *uuid-shaped* ends to an
  entity **name** first — non-opaque semantics exist only for entities, hx
  ids pass through untouched; (2) futon1b `uuid-shaped?` name resolution
  would silently re-key an end that happens to look like a uuid; (3) JVM
  assessment nesting has no typed subject, so "assessment of an assessment"
  is indistinguishable from a typo; (4) nothing traverses two levels: no
  query route expands an hx-id endpoint into the inner edge's endpoints.

## Evidence

All probes hermetic (in-memory node / in-memory repository); nothing was
written to live :7073/:7070.

### futon1b hyperedges

- Schema: endpoints are `mapv str` strings, id = `hx:<type>:<sorted-endpoints>`
  — `futon1b/futon1b_arxana.clj:141-163` (`arxana-hyperedge-doc`) and
  `futon1b/futon1b_server.clj:89-118` (`normalize-hyperedge-end` accepts
  `{:entity-id … :role …}` or any value via `(str end)`); no check that an
  endpoint resolves to an entity.
- **P1 (write+query probe, hermetic in-memory XTDB node + loopback server,
  same harness as `futon1b/test_hyperedges_batch.clj`; run
  `clojure -M:node -m q6-recursive-probe`, 2026-11-07, script deleted after
  the run):**
  - `h1 = hx:q6/base:q6/e1` written (endpoint `q6/e1` is a **nonexistent
    entity** — write still `200 ok`, confirming no endpoint existence gate).
  - `h2 = hx:q6/nested:hx:q6/base:q6/e1.q6/e2` with h1's id as a bare
    endpoint: `200`, `ok: true`.
  - `h3` with h1's id inside a role map `{:entity-id h1 :role :subject}`:
    `200`, `ok: true`.
  - `hyperedges-query {:end h1}` → `{h2, h3}`; `{:type "q6/nested" :end h1}`
    → `[h2]`; stored h2 doc keeps `:hx/endpoints ["hx:q6/base:q6/e1" …]` and
    `:hx/ends [{:entity-id "hx:q6/base:q6/e1"} …]`. VERDICT: WORKS.
- Query route: `futon1b/futon1b_graph.clj:1491+` (`hyperedges-query-uncached`)
  — `end` handling at the `end` branch: uuid-shaped ends are resolved to an
  entity *name* (`fetch-entity`) before matching; hx: ids are not
  uuid-shaped and match as strings over `hx/endpoints`.
- Sidecar treats endpoints as opaque rows keyed by (id, position):
  `futon1b/futon1b_hxindex.clj:211-231,353`.
- Implemented-and-exercised for the *entity* case by
  `futon1b/test_hyperedges_batch.clj:35-60` (in-memory node, real HTTP
  route); the *hx-id-as-endpoint* case was implemented-but-unexercised
  until P1 above.

### futon3c JVM correspondence (assertions / assessments)

- Assertion endpoints must be registered manifestations:
  `futon3c/src/futon3c/arxana/correspond.clj:211-220`
  (`validate-endpoint!` throws `:arxana/manifestation-unknown` for an
  unregistered `:manifestation/id`); manifestation ids are minted digests of
  (authority, family, role-key) — `correspond.clj:151-167`. An assertion id
  (`arxana:correspondence:sha256:…`, `correspond.clj:243-255`) is therefore
  never a legal endpoint. **P2 confirmed by probe** (hermetic
  `in-memory-repository`, run as a temporary namespace under
  `clojure -M:test`, 2026-11-07, file deleted after the run): asserting with
  an assertion id in `:manifestation/id` → ExceptionInfo "endpoint names an
  unregistered manifestation", `:type :arxana/manifestation-unknown`.
- Assessments key on a bare string: `correspond.clj:300-318`
  (`validate-assessment!` only checks that `:subject :assertion/id` is
  present — no `get-assertion` existence check); storage/lookup
  `correspond.clj:135-138` (`put-assessment!` / `assessments-for` filter by
  equality on that string). **P2:** an assessment whose subject is another
  assessment's `:assessment/id` is accepted, and `assessments-for` that id
  returns it (1 result); an assessment of a **ghost** id
  `arxana:correspondence:sha256:nope` is also accepted. 2 tests, 5
  assertions, 0 failures.
- Nested assessments *of assertions* (one level) are implemented and
  exercised: `futon3c/test/futon3c/arxana/correspond_test.clj:185-205`
  (`assessment-conflict-coexists-test`, `assessments-for` = 2); the
  assessment-of-assessment and ghost-subject cases were
  implemented-but-unexercised until P2. No test exercises an
  assertion-as-endpoint refusal (grep `manifestation-unknown` over
  `futon3c/test` and `futon3c/src`: only `correspond.clj` itself).
- Usefulness records can name any subject id (opaque `(vals %)` match):
  `correspond.clj:139-141` — a second type-blind reference channel.

### Emacs Arxana links (futon4a)

- `futon4a/lisp/futon4a-arxana.el:1-9`: the client "owns view state … and
  never mints artifact revisions, manifestations, assertions or accepted
  resolutions". Its only relations are read envelopes
  (`:scope/resolve`, `:correspondence/query`, subscriptions) —
  `futon4a-arxana.el:208-260`. Tests exercise rendering and typed refusals
  only (`futon4a/test/futon4a-arxana-test.el:83-86`). No Emacs-side link
  structure exists to recurse through: prose-only in the mission's sense —
  nothing to test.

## Ready — no new code / Missing — actual work

| Ready — no new code | Missing — actual work |
|---|---|
| futon1b: hyperedge-as-endpoint writes, role endpoints, `?end=`/type+end queries (P1) | futon1b: no two-level traversal/expansion query for hx-id endpoints (must be built if 'clusion composition needs to walk into the inner edge) |
| futon1b: endpoints opaque strings, no existence gate (P1: nonexistent entity endpoint accepted) | futon1b: `uuid-shaped?` end→entity-name resolution (`futon1b_graph.clj:1553-1556`) is entity-only semantics; a general endpoint-typing rule is absent |
| futon3c: assertion endpoints typed to manifestations; refusal implemented (P2) | futon3c: assessment subject is an untyped string — typed subject kinds (assertion / assessment / absent) need a schema, plus an existence check or explicit dangling-reference standing |
| futon3c: assessment→assertion references implemented and tested | futon3c: assertion-as-endpoint refusal is unexercised by the committed suite (probe was temporary) |
| futon3c: usefulness records reference any id opaquely | No store distinguishes "endpoint that is an edge" from "endpoint that happens to be a string": no endpoint-kind field anywhere |
| futon4a: client cannot corrupt relations (writes nothing) | Emacs has no first-class link surface at all; a write-capable Emacs relation route is new work if F2 needs it |

## Open doubts

- I did not probe the futon1b **arxana conditional route**
  (`futon1b_arxana.clj:216`) with an hx-id endpoint; it builds through the
  same `arxana-hyperedge-doc` shape, so it should behave identically, but
  the probe used the generic `/api/alpha/hyperedge` route only.
- "Emacs Arxana links" may also mean legacy Arxana semiophores under
  `futon4/dev/arxana-*.el` / `futon4/data/webarxana`; I read those as
  legacy/prose (not part of the live stack) and did not probe them.
- The cognitect test runner under futon3c initially reported 1-of-2 deftests
  for a freshly created namespace until the second deftest was renamed;
  cause unknown (stale var?), worth a note if it recurs.
