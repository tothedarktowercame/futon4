# PaperPrinter residual — 2026-11-23

**Status: rendering works; informalisation validation is in progress** (`READINESS.md`: six gates, one met). The ink figures below are an audit surface, not a share of correct proofs.

## Finish line

- Frozen classes: **56**: 30 done, 2 not defects, 22 open, 2 unknown.
- Corpus gate: 8,778 records; accepted head with a full census: `4168ba30`.
- Proof errors: 0 at the accepted head. Performance class: closed.
- N1 (rows with an empty reason): **180 records** at this head, down from 1,035 records at the full-corpus freeze.
- C4 (blocks with a repeated left side, scanner count): **28 records**.
- Residual ink (audit surface; 24/24 open-or-unknown classes have markup, extractor precision not yet reviewed): **4.032%** of reader-visible proof-source characters, in 391 records; see `INK.md`.
- Ink change since `faeab149`: union -0.281 points; C4 -26 records, C18 -1 records, C19 -11 records.
- Largest colors: N1 1.84%, C4 0.73%, P-sufficiency-wall 0.48%, C19 0.28%.

## Work now

- ACTIVE `Q188` (integrate, C4): codex-12 e2a013a0 reviewed OK; on claude7/batch3; HELD for batch 3.
- ACTIVE `Q191` (integrate, C18): codex-13 draft reviewed OK, committed by claude-7 on claude7/batch3; HELD for batch 3; removal of the no-op sufficiency sentence in t01A01 accepted.
- ACTIVE `Q229` (integrate, C19): codex-2 c3ab042b reviewed OK; on claude7/batch3; HELD for batch 3.
- ACTIVE `Q237` (observe, P-sufficiency-wall): codex-15 dispatched.
- ACTIVE `Q213` (integrate, C18): codex-26; batch 3; packet overnight/Q213.md.
- WAITING `N1-readings` (data, N1), after `-`: batch 3 accepted as d25599e9 (58 readings; 59 rows gain a reason, 0 errors). 19 readings withheld in total because a reason makes their row displayed (list: ~/notes/claude-7/Q158/readings-batch3-withheld.json + 5 earlier); they wait for Q165.
- WAITING `same-print-typed` (integrate, N1), after `Q169`: input: ~/notes/codex-12/Q165/same-print-rows.tsv.
- WAITING `typed-prune` (observe, N2+C4), after `-`: input: ~/notes/codex-12/Q165/guard-rows.tsv and the two rejected census renders in ~/storage/apm-endgame/.
- WAITING `Q197` (integrate, C23), after `Q188`: packet written (overnight/Q197.md); batch 4, after Q188 (both edit term_walk.py).
- WAITING `Q206` (integrate, P-sufficiency-wall), after `Q237`: packet written (overnight/Q206.md).
- WAITING `Q208` (integrate, P-sufficiency-wall), after `Q206`: packet written (overnight/Q208.md).
- WAITING `Q233` (integrate, P-missing-subproof), after `Q188`: packet written (overnight/Q233.md); batch 4, after Q188 (both edit render.py and term_walk.py).
- WAITING `Q234` (integrate, C16), after `Q213`: packet written (overnight/Q234.md); batch 4 (paper_walk.py only).
- WAITING `Q235` (integrate, C16), after `Q188`: packet written (overnight/Q235.md); batch 4 (render.py _sentence_core_parent).
- WAITING `Q236` (data, N1), after `-`: claude-7; readings drafted (overnight/Q236-readings.json); added on the batch-3 head, gated by the batch-4 census.

## Backlog understanding

- Observation coverage: **24/24** open or unknown classes.
- Classification state: classified 4, classified (Q183) 1, classified (Q190, Q192) 1, classified (Q216) 2, classified by position (claude-7 from renders; Q182/Q187 typed traces) 1, partly-classified 6, stale-observation 3, unclassified 5, unknown 1.
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
