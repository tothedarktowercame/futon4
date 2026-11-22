# PaperPrinter residual — 2026-11-22

## Finish line

- Frozen classes: **49**: 25 done, 2 not defects, 21 open, 1 unknown.
- Corpus gate: 8,778 records; accepted head recorded by the dispatch ledger: `51c23004`.
- Proof errors: 0 at the accepted head. Performance class: closed.
- Largest measured residual, N1: **627 rows in 359 records**, down from 1,928 rows in 1,035 records at the full-corpus freeze (67.5% of rows removed).
- C4 repeated-chain residual: last recorded **69 records / 95 rows**; the next count waits on N2.

## Work now

- ACTIVE `Q161r` (integrate, N1): accept-or-reject against 51c23004.
- ACTIVE `Q164` (observe, N1): mechanism report and adversarial fixtures.
- WAITING `Q157s` (integrate, N2+C4), after `Q161r`: rebase on next accepted head, then census.
- WAITING `N1-readings` (data, N1), after `Q161r`: next reviewed readings batch.

## Backlog understanding

- Observation coverage: **22/22** open or unknown classes.
- Classification state: classified 6, partly-classified 6, stale-observation 2, unclassified 7, unknown 1.
- Historical grammar observations: falsified 5, proposed 2, superseded 1, supported 4.

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
- Planning band, not a promise: **3–7 focused working days** if Q161r/Q157s settle the two structural seams; **1–2 weeks** if either requires another mechanism cycle. The small tail is mostly hours per class, but the unclassified proof-quality group cannot be estimated responsibly until its observation pass.

## Acceleration levers

1. Keep one structural candidate in integration; use other seats for observation and additive readings.
2. Run the adversarial preflight before a full census; reject statement/chain flips and raw-name gains early.
3. Continue frequency-ordered N1 reading batches while the structural candidate is reviewed.
4. Classify the nine unclassified/stale/unknown tail rows before dispatching their implementations.
5. Preserve every supported, falsified, and superseded notation hypothesis in `observations.tsv`; this is the seed corpus for a later expository grammar rather than disposable queue exhaust.
