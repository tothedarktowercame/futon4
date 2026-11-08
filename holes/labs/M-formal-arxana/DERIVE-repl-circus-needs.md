# DERIVE — what M-repl-circus needs from the hypergraph

Recorded by claude-13, 2026-11-08, from claude-19's answer as **advocate**
for M-repl-circus (job invoke-1794102937109-15457-a7654dfe), at Joe's
direction. Scope: the hypergraph structures M-repl-circus needs. This note
does not plan or fix M-repl-circus. Citations spot-checked by claude-13
(`agency/prompt_line.clj:154`, `xiang/lu_memory.clj:4,43`,
`xiang/ma_occurrence.clj:54`).

## 1. Records, and how they fit DERIVE-core

| M-repl-circus record (today) | In the hypergraph | Fits core? |
|---|---|---|
| Pattern proposal: render entry on the turn record (`agency/prompt_line.clj:154-193`: ranked lists per source, shown source, job id) | one 'clusion per (turn, pattern, proposer); endpoints: turn (external ref: record id + evidence id), pattern (external ref: library path + sha256), proposer as the inert dynamics reference (method `embedding` / `驴/m40` / `驴`, version, parameters, e.g. 驴's k=10, cos ≥ 0.35, top 3 at `lu_memory.clj:363-372`); fields rank, score, job id; exposure as appended `proposal/shown` events (core §16) | yes. **Note:** `shown` records what the agent was given and acted on, so it is durable on the proposal, not client presentation state (core §4). The same pattern from two sources is two 'clusions. |
| 馬 occurrence verdict (`ma_occurrence.clj:306-356`): IF/HOWEVER/BECAUSE/THEN mapped to verbatim quotes in a turn window and a chain window; occurrence / partial / void | a `pattern/occurrence` 'clusion type (endpoints: pattern, proposal 'clusion, window of record ids), `evidence/basis model`; part-maps are locators (quote, record id, who) like F1's mark occurrences; each verbatim check is `exact` basis (passed → verified, failed → contradicted with reason); coverage from turn_mapped / chain_mapped (4 complete, 1-3 partial, 0 none) | yes, as a model-basis 'clusion, **not** an assessment (core assessments are attributable candidate / accepted / rejected judgements) |
| 驴 ranking (`lu_memory.clj:363`) | derived entity (core §9), recomputable; the proposal 'clusion gets an endpoint to the occurrence 'clusions that scored it (`:evidence`, l.380) | yes ('clusion-to-'clusion endpoints are allowed from F1, core §2) |
| `:lu/occurrence` hyperedges (`lu_memory.clj:131-146`): role-labelled, digest id, status, sources, sha256 | preserve (core §12) | yes, **but** written to the minus40 store on :7093, not :7073 — see gap 2 |
| Turn, session, agent, mission links (`xiang/turn_record.clj`) | F1's external references; the mission link must be graph structure (驴 retrieval and Giraffe priority read patterns through missions) | yes |
| Giraffe `needs` edges (`giraffe/pass.clj`, `giraffe/tickets.clj:8-16`): class closure / artifact / alternative; source recorded / author-stated / prose-reading, with quote, file, line, met flag | 'clusions with evidence basis recorded → exact, author-stated → human, prose-reading → parser, plus a locator; the WM follows them, so they are graph structure | yes |
| Giraffe readiness, `pass.edn` | computed outputs ("never written back", pass.clj:29): evidence or projections | yes |
| `:learning-loop/*`, `:has-operator-turn` (registered, empty) | not promoted until records witness them (core §12) | yes; M-repl-circus needs none today |

Every external-reference entity must carry a handle resolvable in one call
(library file, memory slug, evidence id), per M-repl-circus's "spreading
activation" rule.

## 2. Traversals M-repl-circus needs

- pattern → its occurrences → the situation turns → session → mission;
- turn → proposals → verdict → the next turn's proposals (the learning loop);
- ticket → needs → ticket;
- pattern → cascade neighbours in the library.

## 3. Gaps against DERIVE-core (sent to codex-21, round 8; rulings in §4)

1. **Typed absence.** 428 void parts in the last 32 records are
   `why="no reader"` (`ma_occurrence.clj:297-304`); 188 are `map-missing`
   (l.241). Asserting a void 'clusion would record a reading that never
   happened. The core needs an **attempt record** (dynamics reference,
   outcome, reason) for an interpreter that did not run or returned nothing,
   so absence is countable without faking evidence, and "proposals with no
   verdict" becomes a positive query.
2. **Second store.** 驴's records live in minus40 (:7093); core §10 names
   futon1b's conditional route (:7073) as the sole persistence authority.
   Mirror them into the arxana route, or admit a second store as a
   referenced source?
3. **Reads beyond `entity/get` / `graph/query`:**
   - ordered windows: "the n-1 earlier operator turns of this session, by
     time" (`ma_occurrence.clj:54`) needs ordering and limit on a role
     endpoint;
   - aggregates: 驴's score is a sum of status weights per pattern within a
     valid-time window; counts by status and reason;
   - reverse and negative lookups: who references this id; proposals with no
     verdict (anti-join, or positive via gap 1);
   - latency: the prompt line renders on every operator turn and 驴's read
     has a 5 s budget (`lu_memory.clj:370`); full hyperedge scans cost
     50-80 s on the live store (futon1b
     `TN-hxindex-doc-backfill-bursts-2026-11-07.md`), so graph/query must be
     indexed by type and role endpoint, not planned as a scan;
   - rebuild: the GCC is derived and must be rebuildable from evidence plus
     the library — a bulk replay path over immutable sources.

## 4. Rulings (codex-21, round 8) — now in DERIVE-core §13–§16

1. Typed absence → an immutable operation-attempt receipt entity (outcome
   `not-run | refused | failed | ran-no-output | ran-with-output`); only
   `ran-no-output` with a declared search envelope supports absence. "No
   reader" is `not-run`; "map-missing" is a per-pattern `ran-no-output`
   only if 馬's search envelope is declared.
2. minus40 / 驴 records → typed referenced authorities; Arxana may keep a
   rebuildable index/cache, never a second source of truth.
3. Reads → bounded, index-backed `graph/query` (filters, reverse-role,
   order, limit) on the mandatory `(edge-type, endpoint-role, endpoint-id,
   valid/event-time, edge-id)` index; aggregates materialised with
   watermarks and bulk rebuild, indexed `(pattern-id, window, status,
   reason)`; no full scan on the prompt path.
4. 馬's verdict → `pattern/occurrence` 'clusion; `shown` → appended
   `proposal/shown` exposure events, boolean derived.

## 5. The §1 mapping under round 9 (claude-13, 2026-11-08)

Round 9 (DERIVE-core §17) moved evidence, interpreter and method version off
the 'clusion into separate `evidence-attestation` records, and put only
`type-revision + ordered role-endpoints + declared asserted content` in the
identity digest. That changes §1 as follows; everything else in §1 stands.

| Record | Round-9 shape |
|---|---|
| Pattern proposal | `pattern/proposal` 'clusion, endpoints turn (external ref) and pattern (external ref). The proposer (`embedding`, `驴/m40`, `驴`) is **not** an endpoint or dynamics reference on the 'clusion: each source is an attestation (producer, method/version/parameters, basis model, rank and score). **Changed:** the same pattern from two sources is now *one* 'clusion with two attestations, not two 'clusions. `proposal/shown` events cite the attestation that was shown (what the agent saw came from one source at one rank). |
| 馬 occurrence verdict | `pattern/occurrence` 'clusion (pattern, window of record ids) claims the pattern occurs there. 馬's reading is a model-basis attestation whose coverage carries occurrence (complete) vs partial; each verbatim quote check is a separate `exact`-basis attestation (verified, or contradicted with reason), citing its locator. A **void** verdict is not a 'clusion: "no reader" is an attempt receipt `not-run`; void after a declared search is `ran-no-output`. |
| 驴 ranking | unchanged: a materialised aggregate with watermark (§15), rebuilt from attestations and receipts. |
| `:lu/occurrence` in minus40 | unchanged: a typed referenced authority (§14); Arxana may index it, never mirror it. |
| Giraffe `needs` edge | `needs` 'clusion (ticket → ticket/artifact; asserted content: class closure / artifact / alternative). Where the need came from is an attestation: recorded → `exact`, author-stated → `human`, prose-reading → `parser`, with quote/file/line as source-refs. The `met` flag and readiness are computed, never stored. |

Cut-over to watch: M-repl-circus decided on 2026-11-05 (claude-8) to write
its typed hyperedges through futon1b's plain `/api/alpha/hyperedges` batch
route. DERIVE-wiring choice 3 keeps Arxana records off that route
(last-writer-wins, no validation). Records written there before C2 exists
are legacy: read them as a referenced source, or re-assert them through
`clusion/assert` + `evidence/append` once C2 lands. Which of the two is a
question for the client inventory (CLIENTS.md).
