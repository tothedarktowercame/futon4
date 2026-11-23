# PaperPrinter residual — 2026-11-23

**Status: rendering works; informalisation validation is in progress** (`READINESS.md`: six gates, one met). The ink figures below are an audit surface, not a share of correct proofs.

## Finish line

- Frozen classes: **56**: 30 done, 2 not defects, 22 open, 2 unknown.
- Corpus gate: 8,778 records; accepted head with a full census: `faeab149`.
- Proof errors: 0 at the accepted head. Performance class: closed.
- N1 (rows with an empty reason): **180 records** at this head, down from 1,035 records at the full-corpus freeze.
- C4 (blocks with a repeated left side, scanner count): **27 records**.
- Residual ink (audit surface; 24/24 open-or-unknown classes have markup, extractor precision not yet reviewed): **4.314%** of reader-visible proof-source characters, in 427 records; see `INK.md`.
- Ink change since `905efba6`: union -0.810 points; C18 -1 records, C19 -1 records, N1 +18 records, P-empty-claim -1 records, P-sufficiency-wall -86 records.
- Largest colors: N1 1.84%, C4 0.89%, P-sufficiency-wall 0.48%, C19 0.35%.

## Work now

- ACTIVE `Q186` (integrate, C22): codex-15; batch 2; packet overnight/Q186.md.
- ACTIVE `Q201` (integrate, P-sufficiency-wall): codex-12; batch 2; packet overnight/Q201.md.
- ACTIVE `Q211` (integrate, C18): codex-26; batch 2; packet overnight/Q211.md.
- ACTIVE `Q219` (integrate, P-missing-subproof): codex-2; batch 2; packet overnight/Q219.md.
- ACTIVE `Q220` (integrate, C16): codex-13; batch 2; packet overnight/Q220.md.
- WAITING `N1-readings` (data, N1), after `-`: batch 3 accepted as d25599e9 (58 readings; 59 rows gain a reason, 0 errors). 19 readings withheld in total because a reason makes their row displayed (list: ~/notes/claude-7/Q158/readings-batch3-withheld.json + 5 earlier); they wait for Q165.
- WAITING `same-print-typed` (integrate, N1), after `Q169`: input: ~/notes/codex-12/Q165/same-print-rows.tsv.
- WAITING `typed-prune` (observe, N2+C4), after `-`: input: ~/notes/codex-12/Q165/guard-rows.tsv and the two rejected census renders in ~/storage/apm-endgame/.
- WAITING `Q188` (integrate, C4), after `Q201`: packet written (overnight/Q188.md).
- WAITING `Q191` (integrate, C18), after `Q188`: packet written (overnight/Q191.md).
- WAITING `Q197` (integrate, C23), after `Q191`: packet written (overnight/Q197.md).
- WAITING `Q229` (integrate, C19), after `Q220`: packet written (overnight/Q229.md); goes to the first free seat of batch 3.
- WAITING `Q205` (integrate, P-sufficiency-wall), after `Q201`: packet written (overnight/Q205.md).
- WAITING `Q206` (integrate, P-sufficiency-wall), after `Q205`: packet written (overnight/Q206.md).
- WAITING `Q208` (integrate, P-sufficiency-wall), after `Q206`: packet written (overnight/Q208.md).
- WAITING `Q213` (integrate, C18), after `Q211`: packet written (overnight/Q213.md).

## Backlog understanding

- Observation coverage: **24/24** open or unknown classes.
- Classification state: classified 5, classified (Q183) 1, classified (Q190, Q192) 1, classified (Q216) 2, classified by position (claude-7 from renders; Q182/Q187 typed traces) 1, partly-classified 5, stale-observation 3, unclassified 5, unknown 1.
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
