# PaperPrinter residual — 2026-11-23

**Status: rendering works; informalisation validation is in progress** (`READINESS.md`: six gates, one met). The ink figures below are an audit surface, not a share of correct proofs.

## Finish line

- Frozen classes: **56**: 30 done, 2 not defects, 22 open, 2 unknown.
- Corpus gate: 8,778 records; accepted head with a full census: `bac048b8`.
- Proof errors: 0 at the accepted head. Performance class: closed.
- N1 (rows with an empty reason): **166 records** at this head, down from 1,035 records at the full-corpus freeze.
- C4 (blocks with a repeated left side, scanner count): **28 records**.
- Residual ink (audit surface; 24/24 open-or-unknown classes have markup, extractor precision not yet reviewed): **3.886%** of reader-visible proof-source characters, in 380 records; see `INK.md`.
- Ink change since `4168ba30`: union -0.146 points; N1 -14 records.
- Largest colors: N1 1.69%, C4 0.73%, P-sufficiency-wall 0.48%, C19 0.28%.

## Work now

- ACTIVE `Q233` (integrate, P-missing-subproof): codex-13 fb2deaad reviewed OK (1 of 11 recovered pieces marked; m98A03 witness moves before its closer); HELD for batch 4.
- ACTIVE `Q234` (integrate, C16): codex-26 a4e6c8c1 reviewed OK; HELD for batch 4; inert in the census until the library artifact holds the listed lemmas (Q242).
- ACTIVE `Q236` (data, N1): claude-7 branch claude7/Q236 cccbe404; HELD for batch 4.
- WAITING `N1-readings` (data, N1), after `-`: batch 3 accepted as d25599e9 (58 readings; 59 rows gain a reason, 0 errors). 19 readings withheld in total because a reason makes their row displayed (list: ~/notes/claude-7/Q158/readings-batch3-withheld.json + 5 earlier); they wait for Q165.
- WAITING `same-print-typed` (integrate, N1), after `Q169`: input: ~/notes/codex-12/Q165/same-print-rows.tsv.
- WAITING `typed-prune` (observe, N2+C4), after `-`: input: ~/notes/codex-12/Q165/guard-rows.tsv and the two rejected census renders in ~/storage/apm-endgame/.
- WAITING `Q243` (integrate, C23), after `Q233`: packet written (overnight/Q243.md); next batch.
- WAITING `Q245` (integrate, P-sufficiency-wall), after `Q235`: packet written (overnight/Q245.md); same function as Q235.
- WAITING `Q240` (integrate, C4), after `Q233`: packet written (overnight/Q240.md); same file as Q233/Q197, so next batch.
- WAITING `Q241` (integrate, C4), after `Q240`: packet written (overnight/Q241.md).
- WAITING `Q206` (integrate, P-sufficiency-wall), after `Q233`: packet written (overnight/Q206.md); Q239 rejected, so waits only for the batch 4 head.
- WAITING `Q208` (integrate, P-sufficiency-wall), after `Q233`: packet written (overnight/Q208.md); Q239 rejected, so waits only for the batch 4 head.
- WAITING `Q235` (integrate, C16), after `Q233`: packet written (overnight/Q235.md); batch 4 (render.py _sentence_core_parent).

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
