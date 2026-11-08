# CLIENT-repl-circus — M-repl-circus as a client of formal Arxana

claude-19, 2026-11-08 03:51Z, acting as M-repl-circus's advocate (as in
DERIVE-repl-circus-needs.md), at claude-13's request. Discovery and writing
only: no code changes, no change to M-repl-circus.md.

Read: DERIVE-repl-circus-needs.md (§5 round-9 remap), DERIVE-core.md §13–§17,
DERIVE-wiring.md, VERIFY.md, DERIVE-F1.md §4–§5. Code read in futon3c
(`xiang/*`, `agency/prompt_line.clj`, `agency/pattern_card_provider.clj`,
`giraffe/*`, `inbox_zero/*`, `peripheral/memory_recall.clj`,
`transport/http.clj`), minus40 (`follower.clj`, `open.clj`), futon1b
(`futon1b_server.clj`, `futon1b_hxindex.clj`, `futon1b_graph.clj`). Live
reads: the two hx sidecar SQLite files (read-only), four turn records in
`~/.emacs-graph/session-turn-analysis/`, two `GET /api/alpha/hyperedge/<id>`
on :7093, the futon3c JVM's environment (pid 521296: `FUTON_SUBSTRATE_URL`
and `FUTON1A_URL` both `http://127.0.0.1:7073`).

Census of the 象 turn store used below (38,820 records, 4,151 operator
turns): 1,382 records carry `:proposed_patterns` (1,406 renders: 876 shown
from the embedding, 64 shown from 驴/m40, 52 ranked-only, 414 older
patterns-only), 1,096 carry `:lu_ranked`, 1,066 carry `:pattern_verdicts`
with 5,400 verdict rows: 29 `occurrence`, 375 `partial`, 4,996 `void`.
Readers: 馬-sonnet on 692 records, none on 374. Of the 43,200 part-maps
(5,400 rows × 8): 1,153 mapped; 16,152 void "no reader"; 11,017 void
"map-missing"; 14,866 void "void" (the reader said null); 12 failed the
verbatim check (6 record-outside-window, 4 quote-too-short, 2
quote-not-found).

## 1. §5 check, from the Circus side

| §5 row | Verdict | Why (Circus code) |
|---|---|---|
| Pattern proposal: one `pattern/proposal` 'clusion per (turn, pattern), each source an attestation | **AFFIRM**, with two corrections below | 馬 already treats a proposal that way: `patterns-of` (`xiang/ma_occurrence.clj:140-185`) merges `:proposed_patterns`, `:embedding_ranked`, `:m40_ranked` and `:lu_ranked` into one entry per pattern id with a `:sources` subset and a `:shown` flag; the per-source cap is 3, so at most 9 proposals per turn. The prompt line shows one source's list and records the other alongside (`agency/pattern_card_provider.clj:418-449` alternation by record-id parity; `agency/prompt_line.clj:154-193` writes `ranked`, `m40_ranked`, `embedding_ranked`, `shown_source`, `job_id`), so `shown` is per render, and a render cites one source at one rank. A retry keeps both renders and "the job id says which one the agent acted on" (`prompt_line.clj:157-159`): one 'clusion, two `proposal/shown` events. |
| — correction 1: there are **three** producers, not two | CORRECT | `embedding` (prompt time, futon3a notions search), `驴/m40` (prompt time, `lu_memory.clj:456 read-near` over the situation vector sidecar, 150 ms hard budget at `pattern_card_provider.clj:346-350`), and `驴` (record time, `xiang/lu_rerank.clj:299-325`: candidate pool from the mined pattern graph plus embedding, a seat asked to pick, each pick carrying a verbatim quote, written as `:lu_ranked`). §5 names all three in passing; the type declaration must list three method ids. The record-time 驴 attestation has a rank and a quote but no score; its quote is a locator into the operator text and belongs in `source-refs`. |
| — correction 2: the pattern endpoint's identity | CORRECT | §1 wrote the pattern endpoint as "library path + sha256". The verdict row carries the pattern file's sha256 (`ma_occurrence.clj:110 read-pattern`). If the sha256 is part of the external-reference entity's id, every library edit mints a new 'clusion for the same pattern on the same turn, and 驴's per-pattern aggregate splits across library revisions. The external-reference entity should be keyed by library path; the sha256 is its `revision/digest` field (core §14 already gives references a digest field). |
| `proposal/shown` events cite the attestation shown | AFFIRM, add the job id | The event's `actor/session` field must carry the invoke job id (`:job_id`, `prompt_line.clj:173`), since that is how the Circus tells which render the agent acted on. |
| 馬 verdict → `pattern/occurrence` 'clusion; 馬's reading a model attestation; each verbatim check an `exact` attestation; void → receipt | **AFFIRM**, with one shape question | `check-map` (`ma_occurrence.clj:210-230`) is the exact check: who ∈ {operator, reply}, record inside the window, ≥ 5 words, and `str/includes?` of the quote in that turn's text. A passed check is `exact / verified`; a failed one keeps the model's map as `:attempt` (`void-map`, l.205) and names the failure, which is `exact / contradicted` with the reason. Status comes from the chain window count (`status-of`, l.247: 4 → occurrence, 1–3 → partial, 0 → void), so coverage is `complete` at 4, `partial` at 1–3. Shape question: the window is 8 record ids (`chain-window`, l.54) and varies per turn; if it is a role endpoint the arity is not fixed. Recommend endpoints (`:pattern`, `:turn`) with the window list as asserted content, so C1 can freeze arity 2. |
| — the void split | AFFIRM codex-21's ruling, with the envelope named | "no reader" (`reader "none"`, 374 records, 16,152 parts) → receipt `not-run`. The reader ran but returned null for a role (14,866 parts) → `ran-no-output`; the search envelope is declared on the record (`:window` of 8 ids, roles I1/I2/G/B, who ∈ {operator, reply}). "map-missing" (11,017 parts: the reader's JSON had no map for that role) is closer to `failed` with reason `map-missing` than to `ran-no-output`; recommend `failed`, since the envelope was searched but the output did not conform. |
| 驴 ranking: materialised aggregate, rebuilt from attestations and receipts | AFFIRM, note what it is today | The live 驴/m40 read never touches a graph store: it queries a local vector sidecar (`scripts/lu_situations.py`, `vectors.npy`) fed by `append-situation!` (`lu_memory.clj:507`) at verdict time, then `score-patterns` (l.340) sums cosine × weight with occurrence 1.0, partial 0.5, void −0.5, k=10, cos ≥ 0.35, top 3. That is already a materialised aggregate; its rebuild is `seed!` (l.287) and its watermark is implicit. Under §15 it needs a declared watermark and a deterministic rebuild from the C2 records. **Defect to carry:** the −0.5 weight is applied to every void row, including the 374 records' "no reader" voids, so an interpreter that did not run lowers a pattern's score. Under receipts, only `ran-no-output` may carry weight; `not-run` must carry none. |
| `:lu/occurrence` in minus40: referenced authority, never mirrored | AFFIRM, with a reader finding | Nothing reads it. `situations-of` (`lu_memory.clj:216`, the :7093 reverse lookup by pattern) has no callers in futon3c; minus40's own source has no reader of `lu/occurrence`; the only readers of the situations are via the vector sidecar. So the 5,400 edges are written-never-read today (refactored ARGUE P15). See §3 for the recommendation. |
| Giraffe `needs` edge: 'clusion; where it came from is an attestation; recorded → exact, author-stated → human, prose-reading → parser | **CORRECT** the basis | Every edge in `holes/tickets/giraffe-edges.json` (21) and `holes/missions/giraffe-mission-edges.json` (41) was read from prose by Zai seats (`read_by: "zai-2, pilot 1, reviewed claude-11"`), so the prose-reading attestation is `basis model`. The verbatim quote-at-line check (`giraffe/tickets.clj:8-16`, as VERIFY's check_edges.py) is a second attestation, `basis exact`, which rejects the edge when it fails. The reviewer (claude-11 / claude-13) is an assessment, and its actor is an agent, not the human/institutional actor §10 names; C1 should say whether an "M-giraffe review" policy actor counts. `class` is absent in the tickets file and defaults to `:closure` (`giraffe/pass.clj:54`); the mission file carries `class` and `stated_in` (needing / supplier / operator). `met` and readiness are computed, never stored (`pass.clj:29`): AFFIRM. |

## 2. Cut-over inventory

"Live" means the code runs in the futon3c JVM or a running unit today, with
records from the last 72 hours. Routes: **turn file** = JSON record in
`~/.emacs-graph/session-turn-analysis/`; **:7093 plain** = minus40 store's
`POST /api/alpha/hyperedge` (last-writer-wins, no Arxana validation);
**:7073 plain** = futon1b's hyperedge routes; **batch** = futon1b
`/api/alpha/hyperedges/batch` (`futon1b_server.clj:1673`,
`write-hyperedges-batch!` l.221), the route claude-8 chose on 2026-11-05.

| # | Where | Record kind | R/W | Current route | Live? | Arxana target |
|---|---|---|---|---|---|---|
| 1 | `agency/prompt_line.clj:154-193` `write-proposed-patterns!` | proposal render: `ranked`, `m40_ranked`, `embedding_ranked`, `shown_source`, `job_id` | W | turn file | yes (1,406 renders) | `clusion/assert` one `pattern/proposal` per (turn, pattern); `evidence/append` one attestation per source; `proposal/shown` event per render |
| 2 | `agency/pattern_card_provider.clj:374-392` `read-m40` → `xiang/lu_memory.clj:456` `read-near` | 驴/m40 prompt-path read | R | local vector sidecar (`scripts/lu_situations.py`, resident process), 150 ms budget | yes (64 shown, 940 renders carry the read) | materialised aggregate (§15) with watermark; stays local; rebuilt from C2 records |
| 3 | `agency/pattern_card_provider.clj:308` `embedding-segment` | embedding top 3 over the library | R | futon3a notions search resident | yes (876 shown) | stays operational; its output is the `embedding` attestation on row 1 |
| 4 | `agency/pattern_card_provider.clj:231` | `:pattern-card/selection` edges (agent chose a card) | R | :7073 plain, `type=` query (3 edges; written by `agency/pattern_card_record_cli.clj`, `pattern_card_acts.clj:44`) | yes | referenced legacy (§12 preserve); an agent's selection is not acceptance; re-declare only if a `pattern/selection` type is wanted |
| 5 | `xiang/lu_rerank.clj:299-325` `handle-event` | `:lu_ranked` (record-time 驴: rank, quote, seat, ms) | W; reads the mined pattern graph `graph.json` via `publication-edn` (l.34-37) and the embedding | turn file | yes (1,096 records) | `evidence/append`: `驴` attestation on row 1's 'clusion, basis model, quote as source-ref |
| 6 | `xiang/ma_occurrence.clj:364-390` `verdict!` | `:pattern_verdicts` (per pattern: turn/chain maps, counts, status, sha256, sources, shown) | W | turn file; raises `:pattern-verdicts` | yes (1,066 records, 5,400 rows) | `clusion/assert` `pattern/occurrence` for the 404 non-void rows; `evidence/append` model attestation (馬-sonnet, coverage) plus one `exact` attestation per checked map; `attempt/append` receipts for void rows |
| 7 | `xiang/lu_memory.clj:175-205` `write-record!` (on `:pattern-verdicts`, `handle-event` l.244) | `:lu-situation` evidence + `:lu/occurrence` hyperedge per verdict row | W | :7093 plain | yes (5,400 edges; off switch `~/code/data/minus40/lu-off`) | referenced authority (§14) for what exists; stop new writes when row 6 lands on C2 |
| 8 | `xiang/lu_memory.clj:216` `situations-of` | reverse lookup pattern → situations | R | :7093 plain `type=lu/occurrence&end=` | **no callers** | delete, or become the C3 `graph/query` reverse-role read if a reader appears |
| 9 | `xiang/lu_memory.clj:507` `append-situation!`; `seed!` l.287 | situation vectors | W | local `vectors.npy` through the resident | yes | the aggregate's index (row 2); `seed!` is its rebuild today |
| 10 | `xiang/turn_service.clj:479-516` `scope-hyperedge`, `post-scope-edges!` via `transport/http.clj:12207-12232` | `xiang/scope` edge: opener → governed, relation closes/within, rule, confidence, status proposed | W | :7093 plain (`xiang-scope-edge-base` defaults to :7093, not :7073) | yes (14,140) | a candidate 'clusion type `xiang/scope`, basis parser (rule labeller `scope-rules/<version>`); not in F1–F6, so referenced legacy until declared |
| 11 | minus40 `follower.clj:100-130` | `e-xiang-turn-*` evidence (copy), `xiang/act` (session, turn-evidence; kind, text ≤ 400, target), `xiang/open`, `xiang/answers` | W; reads turn files | :7093 plain | yes (11,021 / 7,445 / 2,057) | minus40's own projections of the 象 source ("read, never recomputed"); referenced authority, never re-asserted |
| 12 | `transport/http.clj:12274` `:evidence!` from `xiang/turn_service.clj` | settled turn evidence `e-xiang-turn-*` | W | :7073 evidence route | yes | source (§12 preserve); the turn external reference resolves to this evidence id |
| 13 | `xiang/ma_observer.clj:38` → `scripts/ma_observe.py` | 馬 observations (Mastra observer prose, 🔴🟡🟢) keyed to the operator turn's evidence id | W | :7073 evidence rows tagged `ma-observation` | yes for agents in `~/code/data/minus40/ma-enabled.txt` | not a relation; outside Arxana. M-repl-circus's own ruling ("no JSON inside the evidence store") decides its fate, not this design |
| 14 | `inbox_zero/infer_adapters.clj:22-41,171` `session-adaptation` | touched facts: agent → file mentions mined from session evidence bodies within ± mtime window | R | :7073 evidence (`fetch-session-evidence`) | yes | planned by claude-8 as typed hyperedges on the batch route. Under Arxana: a `touched` 'clusion (agent ref, file revision ref) with a `parser` attestation; its reader is inbox_zero's projection |
| 15 | `inbox_zero/attribution.clj:95-110,156` | `clock/clocked-on` lookup by type + end + as-of | R | :7073 plain (201 edges; written by `agency/clock_lineage.clj:107-120` `post-hx!`, `persist-clock!` l.138) | yes | stays operational state on the plain route; referenced authority |
| 16 | `peripheral/memory_recall.clj:140` → `substrate/client.clj:266` `hyperedges-by-end` | 狗 recall: `:memory/assert` by endpoint (callers `dispatch_with_recall.clj`, `peripheral/wm_memory.clj:115`, `peripheral/real_backend.clj:654,698`) | R | :7073 plain (1,964 edges; writers minus40 `memory_assert` and `watcher/multi.clj:647`) | yes for dispatch recall; the per-turn 狗 hook is planned | `:memory/assert` stays preserved (§12). 狗's planned output ("proposed memories", metadata on the agent reply) is row 1's shape with memory refs in place of pattern refs: a `memory/recall` proposal 'clusion, attestations per source, shown events |
| 17 | `giraffe/tickets.clj:183-200`; `holes/tickets/giraffe-edges.json` (21), `holes/missions/giraffe-mission-edges.json` (41) | `needs` edges with quote, file, line, read_by, class, stated_in | R (files) | committed JSON | yes, by CLI run | `clusion/assert` `needs` 'clusions; attestations: model (Zai reader) + exact (quote-at-line check); reviewer as an assessment |
| 18 | `giraffe/run.clj:231-309` `write-run!` | `pass.edn`, `priority.edn`, `work-clusters.json`, `report.md`, manifest | W | `/home/joe/code/storage/giraffe/<run>/` | yes, per run | computed outputs: projections, never records (`pass.clj:29`); readers `giraffe/reread_mint.clj`, `second_reading.clj`, `reread_cli.clj` |
| 19 | `xiang/relationship_review.clj:16` | relationship review candidates (clocked work ≠ discussed work; status proposed) | event only | no store | pure function; emitted as events | not stored until a reader is named; if Giraffe consumes them, a `relates-to` candidate 'clusion with a parser attestation (explicit-id regex) |
| 20 | `watcher/file_ingest.clj:1374-1403` (writer); readers `watcher/multi.clj:702,819`, `logic/cascade_real_live.clj:159,246` | `:pattern/library` (1,213), `:pattern/clause` (9,466), `:cascade/cluster-member` (117) | R for the Circus | :7073 plain | yes | referenced authority: the pattern external reference resolves here (or to the library file); traversal 4 ("pattern → cascade neighbours") reads the mined graph file (row 5), not these |
| 21 | claude-8's decision (M-repl-circus.md §Decided, 2026-11-05) | 驴 activations, 象 readings, 馬 cards, Giraffe reviews, touched facts as typed hyperedges | W | batch route on :7073 | **not live**: the :7073 sidecar has no `xiang/*`, `lu/*`, `ma/*` or `giraffe/*` type (§3) | DERIVE-wiring choice 3: relations go through C2 (`clusion/assert`, `evidence/append`, `attempt/append`); the batch route stays for non-Arxana operational types |

Two traversal notes for C3: the 5 s budget I cited in
DERIVE-repl-circus-needs §3 is the resident's query timeout
(`lu_memory.clj:335`), not the prompt path's; the prompt path waits 150 ms
for 驴/m40 and otherwise shows the embedding. So C3's index serves the
aggregate's rebuild and the learning-loop queries ("proposals with no
verdict", pattern → occurrences → turns), not the prompt render itself.
VERIFY S1's layout C (own table + sidecar index, 26–31 ms) is enough for
both.

## 3. Legacy on the plain routes

Method: `SELECT type, count(*) FROM hx_node GROUP BY type` on each store's
hx sidecar SQLite, opened read-only (futon1b
`migration-store-21/fts5-evidence.db`, 0.1 s; minus40
`~/code/data/minus40/scratch-store/fts5-evidence.db`, under 0.1 s). This
is the same source `GET /api/alpha/hyperedges?type=…&include-total=true`
reads (`futon1b_graph.clj:757` → `futon1b-hxindex/type-count`,
`futon1b_hxindex.clj:1024`), so no XTDB scan was run. Caveat: the futon1b
sidecar is exact up to its checkpoint (2026-11-06T20:58:10Z) plus hook
repairs; the minus40 sidecar has an older schema (`hx_node` without
`doc_edn`), so the status split below comes from the source turn records,
not from :7093.

**futon1b :7073 (59 types, 1.5M hyperedges).** Circus types: none. No
`xiang/*`, `lu/*`, `ma/*`, `giraffe/*` or `touched` type exists; claude-8's
batch-route plan has not written anything yet. Circus-adjacent:

| type | count | recommendation |
|---|---|---|
| `:pattern/clause` | 9,466 | preserve (§12); referenced authority for the pattern external reference |
| `:pattern/library` | 1,213 | same |
| `:memory/assert` | 1,964 | preserve (§12); 狗 reads it as a referenced authority |
| `:clock/clocked-on` | 201 | operational state; stays on the plain route |
| `:cascade/cluster-member` | 117 | preserve; WM-owned |
| `:pattern-card/selection` | 3 | referenced legacy; re-assert only if a selection type is declared |
| `:pattern/attestation` | 1 | referenced legacy (`agency/attestation_record.clj:21`) |
| `:arxana/alignment` | 6 | already on the arxana namespace; C2's adapter (core §7) |

**minus40 :7093 (17 types, 56,000 hyperedges).**

| type | count | recommendation |
|---|---|---|
| `:lu/occurrence` | 5,400 (29 occurrence, 375 partial, 4,996 void, by the source records) | **referenced legacy, and do not re-assert from :7093.** The source of truth is the turn store (`:pattern_verdicts`); the :7093 edges are a projection of it with no reader. Re-assert through C2 from the turn records: 404 non-void rows as `pattern/occurrence` 'clusions with attestations, void rows as receipts in a bulk rebuild (the 驴 aggregate needs the `ran-no-output` ones; the 374 "no reader" records need none). Then stop the :7093 write (off switch exists) |
| `:xiang/scope` | 14,140 | referenced legacy; readers are `scripts/scope_ledger.py`, `scripts/scope_view_data.py`, `emacs/xiang-scope-overlay.el`. Re-assert only if a `xiang/scope` type is declared, which F1–F6 do not do |
| `:xiang/act` | 11,021 | minus40's projection of 象 records (recomputable); referenced authority, never re-asserted |
| `:xiang/open` | 7,445 | same |
| `:xiang/answers` | 2,057 | same |
| `:memory/fact` 8,679, `:memory/mentions` 3,996, `:memory/link` 847, `:memory/version` 714, `:memory/assert` 294, `:memory/relation` 214, `:m40/fault` 136, `:memory/supersede-proposed` 32, `:memory/alias-proposed` 19, `:memory/sorry` 19, `:memory/restates` 18, `:memory/alias` 1 | | minus40's own; referenced authority (§14 as already ruled) |

Rule of thumb that falls out: **re-assert only what makes a claim and has
a reader** (the 404 non-void verdict rows, and the 62 Giraffe edges from
their JSON files); everything else is a projection of a source that
Arxana references.

## 4. Golden-record candidates

Rendered in the round-9 shapes (core §13, §14, §16, §17; F1 §4). Ids
written `clusion:…`, `att:…`, `receipt:…`, `shown:…` are placeholders for
the digests C1 computes; every `:ref/*` and `:source-refs` value is a real
id read on 2026-11-08. External references follow §14: store, endpoint,
record id, schema authority, revision, digest where available.

### (a) one proposal shown from two sources — turn-NgDb6h

Source: `~/.emacs-graph/session-turn-analysis/turn-NgDb6h.json` (codex-7-turn-39,
session 01a1a3df-0582-71d3-a6e0-31341dde1b86, created 2026-11-05T18:18:25Z,
evidence id `emacs-eb5066f4bef987138f074ada9ca752fe`). The pattern
`problems/fundamentals-drowned-by-backlog` was ranked by the embedding
(rank 2, 0.4411), by 驴/m40 (rank 1, 0.21525, neighbour `codex-11-turn-228`)
and by record-time 驴 (rank 3, quote "the queued turn gone missing"). The
embedding list was shown (`shown_source "embedding"`, job
`turn-83b9f9f7-3b9a-487a-a2a6-3b6036200cb4`).

```edn
{:clusion
 {:id "clusion:<digest>"
  :type-revision [:pattern/proposal 1]
  :endpoints [[:turn    {:ref/store :xiang-turn-store
                         :ref/id "turn-NgDb6h"
                         :ref/evidence "emacs-eb5066f4bef987138f074ada9ca752fe"
                         :ref/revision "2026-11-05T18:18:25Z"}]
              [:pattern {:ref/store :pattern-library
                         :ref/id "problems/fundamentals-drowned-by-backlog"
                         :ref/digest "47c6e90b4742e8f8ad9fb33333bbea7e3cd8a2fcb7eac390f319ba5fce95ad89"}]]
  :asserted-content {:intent :proposed}}

 :attestations
 [{:id "att:<digest>" :subject-clusion-id "clusion:<digest>"
   :producer "futon3a-notions-search" :basis :model :standing :unverified :coverage :complete
   :method {:id "embedding" :version "all-MiniLM-L6-v2" :parameters {:top 3}}
   :content {:rank 2 :score 0.4411}
   :source-refs []}
  {:id "att:<digest>" :subject-clusion-id "clusion:<digest>"
   :producer "驴/m40" :basis :model :standing :unverified :coverage :complete
   :method {:id "驴/m40" :version 1 :parameters {:k 10 :min-cos 0.35 :top 3 :budget-ms 150}}
   :content {:rank 1 :score 0.21525 :neighbours 10 :ms 23}
   :source-refs [{:ref/store :xiang-turn-store :ref/id "codex-11-turn-228"}]}
  {:id "att:<digest>" :subject-clusion-id "clusion:<digest>"
   :producer "驴" :basis :model :standing :unverified :coverage :complete
   :method {:id "驴/rerank" :version 1 :parameters {:keep 3 :seat "驴"}}
   :content {:rank 3}
   :source-refs [{:ref/store :xiang-turn-store :ref/id "turn-NgDb6h"
                  :locator {:who :operator :quote "the queued turn gone missing"}}]}]

 :shown
 [{:id "shown:<digest>" :proposal-id "clusion:<digest>"
   :attestation-id "att:<embedding>"
   :surface {:projection :prompt-line :revision 1 :rank-shown 2}
   :actor {:agent "codex-7" :session "01a1a3df-0582-71d3-a6e0-31341dde1b86"
           :job "turn-83b9f9f7-3b9a-487a-a2a6-3b6036200cb4"}
   :time "2026-11-05T18:16:09.341984669Z"}]}
```

The 驴/m40 attestation has no shown event: it ranked the pattern first but
was not the surface shown (parity odd). That is the case the design has to
hold: one claim, three attestations, one exposure.

### (b) one 馬 occurrence with its verbatim-check attestations — turn-x3SMUF

Source: `turn-x3SMUF.json` (claude-11-turn-51, session
8d4df137-fdc5-42c3-a5dd-ba6bb306959c, created 2026-10-25T15:02:30Z,
evidence id `emacs-f7b6b5ef28509d058f15f49fdfe7a116`). Proposed by
record-time 驴 (rank 1). Verdict at 2026-10-25T15:07:07.488782141Z, reader
馬-sonnet, window of 8 records, 4 of 4 roles mapped in both windows, status
`occurrence`. All four quotes occur verbatim in the record's `reply_text`
(re-checked today). The :7093 copy is `hx:lu.occurrence.fc12a8a25a91e6aa75d0`
over `e-lu-fc12a8a25a91e6aa75d0`.

```edn
{:clusion
 {:id "clusion:<digest>"
  :type-revision [:pattern/occurrence 1]
  :endpoints [[:pattern {:ref/store :pattern-library
                         :ref/id "process/recorded-but-not-surfaced"
                         :ref/digest "1361fc98b70b235afead85a07057eb66a6446014f0879048ea80c7c458a05f54"}]
              [:turn    {:ref/store :xiang-turn-store :ref/id "turn-x3SMUF"
                         :ref/evidence "emacs-f7b6b5ef28509d058f15f49fdfe7a116"}]]
  :asserted-content {:window ["turn-Xhj4EM" "turn-EyTXgc" "turn-4sGXo0" "turn-eGZUot"
                              "turn-imlMIH" "turn-GHQFV8" "turn-KsI8bT" "turn-x3SMUF"]
                     :proposal "clusion:<the 驴 proposal for this turn and pattern>"}}

 :attestations
 [{:id "att:<digest>" :subject-clusion-id "clusion:<digest>"
   :producer "馬-sonnet" :basis :model :standing :unverified :coverage :complete
   :method {:id "馬/occurrence" :version 1 :parameters {:window-n 8}}
   :content {:turn-mapped 4 :chain-mapped 4
             :maps {:I1 "morning-brief-queues-structured-items"
                    :I2 "brief-exists-but-nothing-shows-it"
                    :G  "missing-surface-between-record-and-person"
                    :B  "render-pending-items-to-docroot"}}
   :source-refs [{:ref/store :minus40 :ref/endpoint "http://127.0.0.1:7093"
                  :ref/id "hx:lu.occurrence.fc12a8a25a91e6aa75d0"
                  :ref/schema :lu/occurrence :ref/revision "2026-10-25T15:07:07.488782141Z"}]}
  {:id "att:<digest>" :subject-clusion-id "clusion:<digest>"
   :producer "ma_occurrence/check-map" :basis :exact :standing :verified :coverage :partial
   :method {:id "馬/check-map" :version 1 :parameters {:min-quote-words 5}}
   :content {:role :I1}
   :source-refs [{:ref/store :xiang-turn-store :ref/id "turn-x3SMUF"
                  :locator {:who :reply :quote "queues one item per War Machine run"}}]}
  {:id "att:<digest>" :subject-clusion-id "clusion:<digest>"
   :producer "ma_occurrence/check-map" :basis :exact :standing :verified :coverage :partial
   :method {:id "馬/check-map" :version 1}
   :content {:role :I2}
   :source-refs [{:ref/store :xiang-turn-store :ref/id "turn-x3SMUF"
                  :locator {:who :reply :quote "The Morning Brief exists, but nothing shows it to you."}}]}
  {:id "att:<digest>" :subject-clusion-id "clusion:<digest>"
   :producer "ma_occurrence/check-map" :basis :exact :standing :verified :coverage :partial
   :method {:id "馬/check-map" :version 1}
   :content {:role :G}
   :source-refs [{:ref/store :xiang-turn-store :ref/id "turn-x3SMUF"
                  :locator {:who :reply :quote "what's missing is a surface where you see the items"}}]}
  {:id "att:<digest>" :subject-clusion-id "clusion:<digest>"
   :producer "ma_occurrence/check-map" :basis :exact :standing :verified :coverage :partial
   :method {:id "馬/check-map" :version 1}
   :content {:role :B}
   :source-refs [{:ref/store :xiang-turn-store :ref/id "turn-x3SMUF"
                  :locator {:who :reply :quote "Render the pending items to the web docroot, since you view by terminal or web."}}]}]}
```

A failed check (the two `quote-not-found` cases in the census) renders as
the same `exact` attestation with `:standing :contradicted`, `:content
{:role … :reason "quote-not-found" :attempt <the model's map>}`.

### (c) one "no reader" void as an attempt receipt — turn-Ndo9tN

Source: `turn-Ndo9tN.json` (claude-13-turn-65, session
ed69fb38-e76d-4009-8bc7-880ea8a99bf8, created 2026-11-08T03:40:22Z,
evidence id `emacs-7b14ef35a1a77ea97abb1adad402758f`). Verdict at
2026-11-08T03:43:04.370122311Z, `reader "none"`; the pattern
`cascade-construction/run-it-on-a-real-case` (proposed by the embedding,
shown) has eight `{:element nil :why "no reader"}` parts and status void.
Today this was written to :7093 as `hx:lu.occurrence.a9c3614351f6bab2956e`
with `:status "void"`, which is the absence-read-as-a-value error the
receipt replaces. No 'clusion is asserted.

```edn
{:receipt
 {:id "receipt:<digest>"
  :dynamics {:id "馬/occurrence" :version 1}
  :inputs {:turn    {:ref/store :xiang-turn-store :ref/id "turn-Ndo9tN"
                     :ref/evidence "emacs-7b14ef35a1a77ea97abb1adad402758f"}
           :pattern {:ref/store :pattern-library
                     :ref/id "cascade-construction/run-it-on-a-real-case"
                     :ref/digest "3cb90507689605127f21440765fb47045b0c2e405238685642fc4e2dd3250582"}
           :proposal "clusion:<the embedding proposal for this turn and pattern>"
           :envelope {:window ["turn-YoF9El" "turn-BdnSuz" "turn-8YDRvK" "turn-1jgPTg"
                               "turn-FdABKG" "turn-iPlutK" "turn-oK7S3F" "turn-Ndo9tN"]
                      :roles [:I1 :I2 :G :B] :who #{:operator :reply}}}
  :outcome :not-run
  :reason "no reader"
  :actor {:tool "futon3c.xiang.ma-occurrence/verdict!" :seat nil}
  :time "2026-11-08T03:43:04.370122311Z"}}
```

Because the outcome is `not-run`, this receipt supports no claim of
absence and must carry no weight in 驴's aggregate. A `ran-no-output`
receipt differs only in `:outcome`, `:reason "void"` and `:actor {:seat
"馬-sonnet"}`, and it does support absence within the declared envelope.

### (d) one Giraffe needs edge with its attestation — T-car3-phase2-impl

Source: `futon3c/holes/tickets/giraffe-edges.json` (committed af320ba50,
2026-10-27), first edge: `T-car3-phase2-impl` needs `T-car3-queue-routing`,
read from `T-car3-phase2-impl.md` line 5 by zai-2 (pilot 1), reviewed by
claude-11; class absent, so `:closure` by default.

```edn
{:clusion
 {:id "clusion:<digest>"
  :type-revision [:needs 1]
  :endpoints [[:needing  {:ref/store :holes-tickets :ref/id "T-car3-phase2-impl"
                          :ref/revision "af320ba50"}]
              [:supplier {:ref/store :holes-tickets :ref/id "T-car3-queue-routing"
                          :ref/revision "af320ba50"}]]
  :asserted-content {:class :closure}}

 :attestations
 [{:id "att:<digest>" :subject-clusion-id "clusion:<digest>"
   :producer "zai-2" :basis :model :standing :unverified :coverage :complete
   :method {:id "giraffe/prose-reading" :version "pilot-1"}
   :content {:note "Phase 2 implementation is stated to come after the Phase-1 model was ratified, so impl could not be done until the Phase-1 ticket's model was ratified."}
   :source-refs [{:ref/store :holes-tickets :ref/id "T-car3-phase2-impl.md"
                  :ref/revision "af320ba50"
                  :locator {:line 5
                            :quote "Parent: `M-agency-hardening.md`. Phase 2 of Car-3, after Phase-1 model was\n**ratified-with-amendment** by claude-6 (2026-06-10). Branch base:"}}]}
  {:id "att:<digest>" :subject-clusion-id "clusion:<digest>"
   :producer "futon3c.giraffe.tickets/quote-at-line" :basis :exact :standing :verified :coverage :partial
   :method {:id "giraffe/quote-at-line" :version 1 :parameters {:lines-after 5 :whitespace :normalised}}
   :source-refs [{:ref/store :holes-tickets :ref/id "T-car3-phase2-impl.md"
                  :ref/revision "af320ba50" :locator {:line 5}}]}]

 :assessment
 {:id "assess:<digest>" :subject-clusion-id "clusion:<digest>"
  :verdict :accepted :actor "claude-11" :policy "M-giraffe pilot-1 review"
  :reason "reviewed" :time "2026-10-27"}}
```

The assessment's actor is an agent. Whether "M-giraffe pilot review" is an
institutional policy actor under core §10 is for C1 to say; if not, this
record stays a candidate with two attestations and no assessment.

## 5. Readers

The rule (refactored ARGUE P15, `arxana-wiring.edn` written-never-read):
a stored record kind with no named reader is not stored.

| Record kind | Named reader(s) | Standing |
|---|---|---|
| `pattern/proposal` 'clusion + attestations | 馬 (`patterns-of`, `ma_occurrence.clj:140`); the 驴/m40 aggregate rebuild; `scripts/xiangma_port_export.py` (patternVerdict port); the agent, through the prompt line | stored |
| `proposal/shown` events | 馬 (`:shown`, same fn); the port export; Joe through the REPL prompt line | stored |
| `pattern/occurrence` 'clusion + model attestation | the 驴/m40 aggregate (`score-patterns`, `lu_memory.clj:340`); Giraffe (planned: 馬's tensions, M-repl-circus roster); the port export | stored |
| `exact` check attestations | the aggregate's coverage input (turn/chain counts); the F1-style drift check when a turn is re-exported | stored |
| attempt receipts (`not-run`, `ran-no-output`, `failed`) | the aggregate (`ran-no-output` only); the positive query "proposals with no verdict" that replaces the anti-join (DERIVE-repl-circus-needs §3.3) | stored; `not-run` must carry no weight |
| materialised 驴 aggregate (+ watermark) | the prompt line (`read-m40`, `pattern_card_provider.clj:374`) | stored locally, rebuildable |
| `:lu/occurrence` on :7093 | none (`situations-of` has no callers; minus40 has no reader) | legacy, not indexed, not re-asserted from there |
| `xiang/scope` on :7093 | `scripts/scope_ledger.py`, `scripts/scope_view_data.py`, `emacs/xiang-scope-overlay.el` | referenced legacy |
| `xiang/act`, `xiang/open`, `xiang/answers` on :7093 | minus40 `open.clj` (the m40 tools) | referenced authority |
| `needs` 'clusions + attestations | Giraffe pass (`pass.clj:316`), `reread_mint.clj`, WM priority | stored |
| Giraffe pass outputs (`pass.edn`, `priority.edn`) | `reread_mint.clj`, `second_reading.clj`, `reread_cli.clj` | projections, not records |
| relationship reviews | none stored today | not stored until Giraffe reads them |
| `touched` 'clusions (planned) | inbox_zero projection (`infer_adapters.clj` consumers) | stored once written |
| `clock/clocked-on` | `inbox_zero/attribution.clj`, `agency/clock_lineage.clj`, `logic/cascade_real_live.clj:87` | plain route, preserved |
| `:memory/assert` | `peripheral/memory_recall.clj` (狗), `watcher/multi.clj:606`, minus40 history | plain route, preserved |
| `memory/recall` proposals (狗, planned) | the agent through the 狗 line; 馬 if it scores memories as it scores patterns | stored once a reader exists in code |
| 馬 observations (`ma_observe.py` evidence) | `turn_frames.py`, the 象 stepper (OBSERVED) | evidence, not an Arxana record |
| `:pattern-card/selection` | `pattern_card_provider.clj:231` | plain route, referenced |

## 6. What C1 should hear from the Circus (summary)

1. Three proposal producers (`embedding`, `驴/m40`, `驴`), each a method id
   with a version; the shown event carries the invoke job id.
2. The pattern external reference is keyed by library path; the file
   sha256 is its revision, not its identity.
3. The occurrence window is asserted content, so arity is fixed at 2
   (pattern, turn).
4. Receipts distinguish `not-run` (374 records today) from
   `ran-no-output`; only the second may weigh in 驴's aggregate. The live
   score penalises both.
5. Giraffe prose-reading is `basis model` with an `exact` quote check;
   the reviewer is an assessment whose actor is an agent.
6. Nothing on :7073 needs re-assertion; on :7093 only the 404 non-void
   verdict rows do, and from the turn records rather than from the edges.
7. The prompt path does not read the graph; C3's bounded reads serve the
   aggregate rebuild and the learning-loop queries.
