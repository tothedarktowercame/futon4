# PaperPrinter residual — 2026-11-24

**Status: rendering works; informalisation validation is in progress** (`READINESS.md`: six gates, one met). The ink figures below are an audit surface, not a share of correct proofs.

## Finish line

- Frozen classes: **67**: 30 done, 2 not defects, 30 open, 5 unknown.
- Corpus gate: 8,778 records; accepted head with a full census: `ee47b461`.
- Proof errors: 0 at the accepted head. Performance class: closed.
- N1 (rows with an empty reason): **22 records** at this head, down from 1,035 records at the full-corpus freeze.
- C4 (blocks with a repeated left side, scanner count): **21 records**.
- Residual ink (audit surface; 25/35 open-or-unknown classes have markup, extractor precision not yet reviewed): **1.579%** of reader-visible proof-source characters, in 145 records; see `INK.md`.
- Ink change since `8dd66325`: union -0.252 points; C4 -6 records, C7 +2 records, C17 +2 records, N1 -23 records, P-sufficiency-wall -2 records.
- Largest colors: C4 0.79%, C23 0.18%, P-sufficiency-wall 0.18%, N1 0.17%.

## Work now

- ACTIVE `Q399` (integrate, N1): zai-6 on a31673ee (scan limited to 22 modules).
- ACTIVE `Q420` (observe, N1): kimi-4 (new seat) on a31673ee; codex-26 failed on quota with no report.
- ACTIVE `Q423` (integrate, C25): kimi-3 (new seat) on batch tip a31673ee; census q257 shows raw ENNReal.toNNReal as a composition factor in a00J06.
- ACTIVE `Q412` (integrate, N1): Q412c to kimi-1 on batch tip 8b0abc35 (codex-2 stopped on quota; its diff saved as patches).
- ACTIVE `Q424` (integrate, C4): zai-15 on batch tip a31673ee.
- ACTIVE `T001` (observe, P-misattributed-reason): kimi-6 for claude-16 on a31673ee: invoke-1795530432617-28017-ced6c102; packet ~/notes/claude-16/tail/T001.md.
- ACTIVE `T001` (observe, C23+P-premature-conclusion+P-missing-subproof+P-overexpanded-setup+P-opaque-proof+P-duplicate-tag+N14+C7+C22+C12): claude-15 (reading; report ~/notes/claude-15/tail-A/T001-REPORT.md).
- ACTIVE `T002` (observe, C23): kimi-5 on a31673ee snapshot; packet ~/notes/claude-15/tail-A/T002.md.
- ACTIVE `T002` (integrate, P-unnamed-library-result): claude-16 branch claude-16/T002 on a31673ee; evidence 14 Q410 modules vs q258; report ~/notes/claude-16/tail/REPORT.md.
- ACTIVE `T003` (data, N15): claude-16 97d9b4ac on a31673ee; the five pass (166 s).
- ACTIVE `T004` (integrate, P-implementation-vocabulary): claude-16 13e5b66b on a31673ee; 5 changed fields in 4 modules vs q258.
- WAITING `Q241` (integrate, C4), after `Q349`: HELD, not in flight (status corrected 2026-11-24). codex-2 c3bc1e97 (2264 tests; 3 t00A05 records change), reviewed and HELD, not for batch 6 as it stands: the partition by typed branch path is right, but the result is ten context-free displays printed after the case prose (both "= 1+3 since the condition holds" and "= 5-(5-s) since the condition fails" for one table), no easier to read than the one mixed chain; it is merged only together with Q248, which places each branch chain inside its case; authored on 91d38c1e (b0a112b7 = 91d38c1e + one notation rule); base column is the head it would be merged onto (94c86724).
- WAITING `Q206` (integrate, P-sufficiency-wall), after `Q256`: codex-13 stopped without a commit (draft in worktree codex-13-Q206, report Q206-REPORT.md): the loop guard keeps the captured rows (2250 tests) but in 4 modules 19 records change and at least 6 newly printed rows are source-less bridges; redo after Q245 decides how source-less and mixed bridges are emitted.
- WAITING `Q248` (integrate, P-sufficiency-wall), after `Q256`: to write after Q245 (row classes decide display vs sentence); authored on top of Q241 c3bc1e97 so that each branch-local chain is placed in its case; evidence ~/notes/claude-7/raw-names-480/proofs/Q208-census/.
- WAITING `Q404` (integrate, C4), after `Q424`: packet to write.
- WAITING `Q405` (integrate, N1), after `Q424`: packet to write.
- WAITING `Q411` (integrate, N1), after `Q424`: packet to write after Q403 lands; re-measure first.
- WAITING `Q366` (integrate, P-sufficiency-wall), after `Q350`: packet to write; after the N1 queue.
- WAITING `Q343` (observe, P-misattributed-reason), after `-`: intake INTAKE-inkup-2026-11-24.md; waits for a free seat.
- WAITING `Q344` (observe, P-implementation-vocabulary), after `-`: intake INTAKE-inkup-2026-11-24.md; waits for a free seat.
- WAITING `Q347` (observe, P-missing-subproof), after `-`: intake INTAKE-inkup-2026-11-24.md; waits for a free seat.
- WAITING `T005` (observe, P-implementation-vocabulary), after `T001`: packet ~/notes/claude-16/tail/T005.md for kimi-6 after T001.

## Backlog understanding

- Observation coverage: **35/35** open or unknown classes.
- Classification state: classified 4, classified (Q183) 1, classified (Q190, Q192) 1, classified (Q216) 2, classified (Q369) 1, classified (Q371) 1, classified (Q385) 1, classified (Q413b, Q422) 1, classified by position (claude-7 from renders; Q182/Q187 typed traces) 1, partly-classified 6, reader-reported; not typed 3, reader-reported; one site 2, stale-observation 3, unclassified 6, unknown 2.
- Historical grammar observations: falsified 7, proposed 2, superseded 1, supported 4.

## Next branches to inspect

1. **N1-reason-mechanisms** (N1), score 35: finish Q164 trace and refresh the head-frequency table.
2. **chain-ownership** (N2+C4), score 32: run Q157s adversarial fixtures, then census only after Q161r decision.
3. **real-corpus-test-state** (N15), score 31: run the full a97J08 real-corpus test file.
4. **sufficiency-producers** (P-sufficiency-wall), score 24: classify five declarations by producer path.
5. **duplicate-closer-inventory** (P-duplicate-closer), score 23: produce an exact corpus-wide inventory by typed-evidence shape.

The score uses ordinal 1–5 inputs: `4×blocking + 3×uncertainty + 2×volume + 2×split-value − 2×observation-cost − 2×overlap-risk`. It ranks the next observation, not the importance of the mathematical content.

## Throughput and uncertainty

- Packet ledger: 94 total; 56 merged, 16 rejected, 15 discovery.
- Since 2026-11-20: 49 packets; 26 merged, 11 rejected, 8 discovery.
- Planning band, not a promise: **3–7 focused working days** if the queued integrate packets land without a new mechanism cycle; **1–2 weeks** if C4/N2 or C23 needs one. The small tail is mostly hours per class, but the unclassified proof-quality group cannot be estimated responsibly until its observation pass.

## Acceleration levers

1. Keep one structural candidate in integration; use other seats for observation and additive readings.
2. Run the adversarial preflight before a full census; reject statement/chain flips and raw-name gains early.
3. Continue frequency-ordered N1 reading batches while the structural candidate is reviewed.
4. Classify the nine unclassified/stale/unknown tail rows before dispatching their implementations.
5. Preserve every supported, falsified, and superseded notation hypothesis in `observations.tsv`; this is the seed corpus for a later expository grammar rather than disposable queue exhaust.
