# MAP Q3 — REPL Circus / GCC path

## Answer

Live record census (2026-11-07, GET only against :7070 futon3c Agency and
:7073 futon1b, plus the turn file store):

- **Turns (operator):** 18,153 turn records in
  `~/.emacs-graph/session-turn-analysis` (`turn-*.json`); 4,810 have a
  published 象 analysis (`*.analysis.json`); 9,997 have drafts. Evidence-side,
  `author=joe` counts 23,481 entries (includes derived copies per
  M-repl-circus's stop-the-line note).
- **Replies / dispatches:** invoke ledger `/tmp/futon3c-invoke-jobs.edn*`
  holds ≥500 jobs (route caps at limit=500; journal has 842 lines, 8,652
  commission files) — volatile, in /tmp per M-repl-circus "Open".
- **Patterns:** 1,671 rows in the catalog TSV
  (`~/code/futon3/resources/sigils/patterns-index.tsv`, served by
  `GET /api/alpha/patterns/search`, http.clj:4179-4219). Per-turn pattern
  candidate sidecars: 2,339 (`*.patterns.json`). PSR evidence: 4; PUR
  (pattern-outcome) evidence: 699.
- **Missions:** 356 `:mission` entities in futon1b (`/api/alpha/census`).
- **Capabilities:** 36 `:capability` + 8 `:learning-loop/capability` entities;
  but all four `:learning-loop/*` hyperedge relations have count **0**.
- **Candidate-actions:** zero live. `compile-action` exists
  (arxana/compile/compiler.clj:174) behind `:action/compile`
  (arxana/service.clj:292-295) but is pure, persists nothing, and its only
  exercisers are tests and the futon4a demo fixture.
- **Receipts:** `make-receipt` (arxana/compile/admission.clj:48) has no
  caller outside its own ns/tests. GCC hyperedges in futon1b: only
  `:memory/assert` (1,961); `:operator-turn` entities: 8;
  `:learning-loop/operator-turn`: 14.

**Missing step, as precise as code allows:** no producer converts a settled
xiang turn's analysis into a D6 intent record, and no persistence/surface
route carries a compiled candidate-action to the War Machine. Concretely the
absent piece is a hook that (a) reads a turn record's `*.analysis.json`,
(b) mints an intent via `futon3c.arxana.compile.intent/make-intent` (which
has **no callers** outside tests), (c) calls `compiler/compile-action`, and
(d) writes the resulting `:candidate-action` to a store/route the WM
selector reads. `futon3c.aif.live-recommendation` only projects futon2
cascade-posterior decisions; nothing feeds arxana candidate-actions into
that candidate set. The GCC itself is also unwritten: turn/reply nodes are
not hyperedges in futon1b (M-repl-circus.md:143-149 vs. census zeros above).

## Evidence

- `GET localhost:7073/api/alpha/census?entity-type=:mission` →
  `{:count 356}`; `:capability` → 36; `:learning-loop/capability` → 8;
  `:operator-turn` → 8; `:candidate-invariant` → 3; `:learning-loop` → 0;
  `:cascade` → 0; `:pattern` → 0 (patterns live in the TSV, not XTDB).
- `GET localhost:7073/api/alpha/census?type=:memory/assert` →
  `{:kind :hyperedge, :count 1961}`; `:learning-loop/develops-capability`,
  `/evidences-capability`, `/belongs-to-mission`, `/shares-capability-with`,
  `:has-operator-turn` → all `{:count 0}` (checkpoint
  `2026-11-06T20:58:10Z`).
- `GET localhost:7070/api/alpha/evidence/count?claim-type=observation` →
  145,691; `author=joe` → 23,481; `type=pattern-outcome` → 699;
  `type=pattern-selection` → 4; `type=arse-qa` → 161; `type=memory` → 6,348.
- `ls ~/.emacs-graph/session-turn-analysis | grep -c 'turn-[^.]*\.json$'` →
  18153; `*.analysis.json` → 4810; `*.candidates.json` → 3245;
  `*.patterns.json` → 2339; `*.draft.json` → 9997 (store layout:
  futon3c/src/futon3c/xiang/turn_store.clj:1-19).
- `wc -l ~/code/futon3/resources/sigils/patterns-index.tsv` → 1672 lines
  (1671 non-comment; one header).
- `grep -rn "compile-action" futon3c/src` → only arxana/service.clj:295;
  tests at test/futon3c/arxana/compile_test.clj:71-89,
  compile_service_test.clj; demo fixture futon4a/examples/compile-request.json.
- `grep -rn "make-intent\|compile/intent" futon3c/src` outside
  arxana/compile → only the unrelated `apm/durable_coordinator.clj:118`
  make-intent (different signature, APM-internal).
- `grep -rn "make-receipt\|make-admission-decision" futon3c/src` → no
  callers outside arxana/compile/admission.clj itself.
- futon3c/src/futon3c/aif/live_recommendation.clj:1-22 — presentation-only
  projection; selection is futon2 `policy/select-action-cascades`; no arxana
  candidate-action input.
- futon3c/holes/missions/M-repl-circus.md:47-49 (GCC hyperedges decision),
  :143-149 (GCC as derived graph), :190-192 (invoke ledger in /tmp).
- giraffe-observation-series/README.md Step 1-4 — turn actions
  `POST /api/alpha/xiang/turns/:id/resolution` and `/pattern-outcome` are the
  attested-observation writes that exist; they stop at pattern-outcome
  evidence, not candidate actions.

## Ready — no new code

| Fact | Where |
|---|---|
| Turn/reply evidence model agreed and turns/replies recorded live | M-repl-circus.md evidence model; 18,153 turn records; count routes above |
| Xiang interpretation pipeline (record → analysis → candidates → outcome) with idempotent turn actions | turn_store.clj; giraffe README Steps 1-4; PUR route (http.clj:4392) |
| D6 compile waist: intent record shape, trust profiles, pure deterministic compiler, admission/receipt shapes | arxana/compile/{intent,compiler,admission}.clj, all test-exercised |
| `:action/compile` registered in the arxana service envelope | arxana/service.clj:292-295 |
| `:memory/assert` proves the hyperedge GCC pattern end-to-end | census 1,961; M-repl-circus.md:38-40 |

## Missing — actual work

| Gap | What would need to exist |
|---|---|
| Intent producer | A post-settlement hook (alongside the xiang packet emit, giraffe README Step 1) that mints `make-intent` records keyed by turn evidence-id; today `make-intent` has zero callers |
| Candidate-action persistence + surface | A write/read route (futon1b hyperedge or arxana store) for `compile-action` output; compile is pure and nothing stores its `:candidate-action` |
| WM uptake | A feed from stored candidate-actions into the futon2 cascade candidate set that live_recommendation projects; no such join exists |
| GCC population | Turn/reply/session nodes as futon1b hyperedges; `:learning-loop/*` and `:has-operator-turn` relations are registered but all count 0 |
| Admission/receipt exercise | `make-admission-decision`/`make-receipt` have no callers; no receipt records exist anywhere live |
| Durable dispatch store | Invoke ledger (dispatch evidence part 3) is in /tmp (M-repl-circus.md:190-192) |

## Open doubts

- Whether candidate-actions belong in futon1b as hyperedges (per the
  M-repl-circus "relations go in the GCC" decision) or in the arxana store's
  own journal; the two write paths (`/api/alpha/hyperedges/batch` vs
  `/api/alpha/arxana/command`) imply different authority stories.
- The 23,481 `author=joe` evidence count vs 18,153 turn files suggests
  derived copies are still being written under the operator author tag;
  did not verify composition (count route has no tag filter that matched:
  `tag=operator-turn` → 0).
- PSR count of 4 vs PUR 699 looks asymmetric; PSRs may flow through a
  different path than the evidence route I counted.
