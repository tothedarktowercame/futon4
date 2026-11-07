# MAP Q5 — Edit proposal strata

Which clients currently distinguish manifestation, content, relation,
projection-rule, and presentation-state edits? Where are these silently
conflated?

## Answer

Only the JVM Arxana namespace separates all five strata as distinct record
types; the Emacs `arxana-links.el` separates some strata as entity *types* but
routes every edit through one whole-entity `POST /entity` whose server schema
silently drops standing/presentation fields; `latex-wysiwyg.el` distinguishes
content vs presentation via its class system and quote-verified refusal; the
browser and WebArxana hold no edits at all. The sharpest silent conflation
found: a *standing* edit (link suppression) is posted as a partial entity
payload that the futon1b `build-entity` gate rejects (`:name`/`:type`
required) and whose `:status` key has no place in the stored doc even if
accepted — the stratum simply has no write path. Second: scholium/surface-form
records bundle content, anchor coordinates (manifestation), and verification
state (presentation-state) in one plist, and verification mutates that record
in place. Third: link *content* is serialized as a JSON string blob inside
`:entity/source`, unaddressable field-by-field.

## Evidence

Strata = manifestation (M), content (C), relation (R), projection-rule (P),
presentation-state (S).

**JVM `futon3c/src/futon3c/arxana/` — implemented-and-exercised (test suite in
`futon3c/test/futon3c/arxana/`).**
- C: immutable content-addressed artifact revisions; "Bytes are immutable per
  revision ID" (`artifact.clj:14-16`), `ingest!` never overrides
  (`artifact.clj:66-70`); preview hashes explicitly "NOT an artifact revision
  identity" and are refused (`require-authority-revision`, `artifact.clj:96-110`).
- M: manifestation registry with `:authored`/`:generated` roles
  (`correspond.clj:47`), immutable byte-equivalent-idempotent registration
  (`correspond.clj:37-43`).
- R: `assert-correspondence!` — "immutable directional revision-pinned
  correspondence assertions" (`correspond.clj:5-8`); reversal is a separate
  declared property (`default-reversible-methods`, `correspond.clj:40-44`).
- S (standing): separate attributable assessment records with
  accepted/rejected/absent verdicts (`correspond.clj:49`,
  `assess!` `correspond.clj:320`) — standing is never edited onto the assertion.
- P: strategies registered under their own content-addressed revision id as
  type `:arxana/strategy-revision` (`service.clj:45-54`); position bases are
  client projections, codepoint ranges are the location basis
  (`resolve/anchor.clj` ns docstring, D5-DR1 note).
- Edit sequencing: `revision/ingest` compares expected head, refuses stale with
  recovery choices ("never silently retried", `service.clj:74-84`);
  non-atomicity reported honestly in capabilities. The reserved `arxana/*`
  namespace is writable only via the authenticated conditional-command route
  (`futon1b_graph.clj:579-582` gate; `futon1b/futon1b_arxana.clj:239`).
- Conflation found: **live futon1b accepts only `:revision/ingest`** — every
  other operation throws `:arxana-unsupported-operation`
  (`futon1b_arxana.clj:183-185`). The other strata's write paths are
  exercised only against the mock/HTTP test doubles
  (`store.clj:41` MockStore; `store_http_test.clj:13`).

**Emacs `futon4/dev/arxana-links.el` — implemented, strata conflated at the
write boundary.**
- Type-level separation exists: strategy = P (`make-strategy` :289), voiced
  link = R (:358), surface form (:450 area), scholium, embedding cache.
- S silently dropped: `suppress-link` (:400-411) POSTs only
  `{:xt/id :status :suppressed-at :suppressed-by}`; futon1b `build-entity`
  requires `:name`+`:type` (`futon1b_graph.clj:575-578`) and its doc schema
  keeps only `external-id`/`source`/`props` (`:585-588`) — top-level `:status`
  has no destination. A standing edit is indistinguishable from a malformed
  entity create.
- M+C+S bundled: the scholium record carries anchor coordinates, content hash,
  and status; `verify-scholium` mutates `:status`/`:last-verified` in place on
  the same plist (:773-784) — presentation-state edit rewrites the content
  record's fields (a later whole-entity persist would version it, but the
  mutation itself is silent and client-local).
- C unaddressable: `--wrap-entity-payload` (:213-221) serializes the entire
  entity as a JSON string in `:source`; content is never field-addressable to
  the server.
- Identity conflation: `edit-surface-form` (:565-608) rewrites fields on the
  same `:xt/id` when surface text is unchanged, but changing the surface text
  mints a *new* id (`--generate-id "surface" concept surface`, :199 area) —
  "edit" and "new record" depend on an incidental hash input, not a stratum
  decision.
- Context coordinates (manifestation) live inside surface-form `:context`
  (:offset/:snippet) — bundled with content in one record.

**`latex-wysiwyg.el` (`futon3c/emacs/`, M-latex-wysiwyg) —
implemented-and-exercised for content edits, class-refused otherwise.**
- C vs M: the class system (A editable / B argument-editable / C not editable,
  mission `M-latex-wysiwyg.md:65-75`) plus quote-verified unique-match splice
  with refusals `quote-not-found`/`quote-ambiguous`
  (`latex-wysiwyg.el:461-496`); markers not offsets; scope table is a
  disposable live layer rebuilt per build (mission :137-141) — manifestation
  deliberately not persisted.
- S: cursor/selection is a separate `scope/point` channel (mission :152-154);
  edit arming (`latex-wysiwyg-allow-edits`, off by default, :335; arm/disarm
  :551) and pre-edit state broadcast (`--announce-state` :520-527) keep
  presentation-state out of the edit channel. Undo boundaries per edit
  (:495-500).
- Conflation: an applied edit splices the source file directly — content edit
  *is* the file write; projection rules (the latexml source map) are implicit
  in the conversion step and are not themselves editable records; no relation
  stratum exists here.

**WebArxana (`futon4/dev/web/webarxana/`) — no edits at all.**
- Client (`client/*.cljs`) and server (`server/*.clj`) contain no edit/POST
  write paths beyond auth (grep `POST|PUT|edit|write` over `client/` and
  `server/`: only a caption string at `live.cljs:234`). Rendering verification
  is read-side (`*-verify.mjs`, playwright `tests/`). Prose-only aspiration;
  zero strata, zero conflation, because zero writes.

## Ready — no new code / Missing — actual work

| Client × stratum | Ready | Missing |
|---|---|---|
| JVM content edits | immutable revision ingest + CAS head, tested (`edit_robustness_test.clj`) | live futon1b CAS is single-op only; batch/other ops mock-only |
| JVM manifestation | registry with roles, idempotent, tested | no live route beyond ingest (`futon1b_arxana.clj:183`) |
| JVM relation | immutable assertions + assessments, tested | same — not live on futon1b |
| JVM projection-rule | strategy revisions content-addressed | same |
| Emacs arxana-links P | strategy entities persist | edits are whole-entity replaces |
| Emacs arxana-links R | voiced links persist w/ found-by provenance | suppression (standing) has **no working write path**; status key dropped by server schema |
| Emacs arxana-links M | anchor cascade (exact/context/fuzzy) client-side | coordinates+hash+status bundled in one record; verify mutates in place |
| Emacs arxana-links C | persists via :source JSON blob | content not field-addressable; edit-vs-mint identity is hash-incidental |
| latex-wysiwyg C | quote-verified guarded splice, refusal, undo, armed-off | splices file directly; no relation/manifestation records |
| latex-wysiwyg S | separate point channel + state broadcast | scope table projection rules not editable records |
| WebArxana (all strata) | read-only, no conflation possible | any edit capability at all |

## Open doubts

- I did not run `arxana-links-suppress-link` against live :7073 (write probe
  forbidden); the "rejection/drop" claim is from reading
  `build-entity`/`write-entity!` (`futon1b_graph.clj:569-604`) — a live
  one-shot confirm would settle whether `assert-ok` surfaces the
  `:missing-required` error to the user or swallows it.
- XTDB versioning means whole-entity re-POSTs append history rather than
  rewrite it; the "history rewrite" risk in Emacs clients is therefore milder
  than prose suggests — the real defect is dropped fields, not erased history.
  Did not verify `/entities/history` round-trip for a suppressed link.
- `store_http.clj` claims `:entity-compare-and-set true` (:19) for futon1b;
  whether any live caller has exercised the CAS refusal path (as opposed to
  `store_http_test.clj`) was not established.

## Review addendum (claude-13, 2026-11-07)

Confirmed by reading: `arxana-links-suppress-link` (arxana-links.el:400-409)
posts `:xt/id :status :suppressed-at :suppressed-by` with no `:name`/`:type`,
and `build-entity` refuses that at futon1b_graph.clj:574-578.

A wider consequence, from code, not live-probed: since futon1b `b27cc63`
(2026-11-06, M-jvm-arxana INSTANTIATE-2) `build-entity` calls
`gate-reserved-namespace!` (futon1b_graph.clj:581), which refuses any
generic-route write whose `:type` is `arxana/*` with a 403. Every entity the
Emacs links client persists through `POST /entity` has such a type:
`arxana/link-strategy` (:310), `arxana/voiced-link` (:380),
`arxana/surface-form` (:471), `arxana/scholium` (:674),
`arxana/embedding-cache` (:842). So on a futon1b running that commit, the
Emacs client has no working write path for any stratum, not only for
standing. A GET of `entities/latest?type=arxana/voiced-link` returned zero
entities on 2026-11-07. Whether the serving futon1b JVM has the gate loaded
was not checked. Q7's row "Emacs Arxana entities/links: inherited
last-write-wins via POST /entity" should be read with this in mind.
