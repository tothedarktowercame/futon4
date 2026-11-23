# PaperPrinter residual — 2026-11-23

**Status: rendering works; informalisation validation is in progress** (`READINESS.md`: six gates, one met). The ink figures below are an audit surface, not a share of correct proofs.

## Finish line

- Frozen classes: **56**: 30 done, 2 not defects, 22 open, 2 unknown.
- Corpus gate: 8,778 records; accepted head with a full census: `91d38c1e`.
- Proof errors: 0 at the accepted head. Performance class: closed.
- N1 (rows with an empty reason): **166 records** at this head, down from 1,035 records at the full-corpus freeze.
- C4 (blocks with a repeated left side, scanner count): **28 records**.
- Residual ink (audit surface; 24/24 open-or-unknown classes have markup, extractor precision not yet reviewed): **3.881%** of reader-visible proof-source characters, in 380 records; see `INK.md`.
- Ink change since `bac048b8`: union -0.005 points; C16 -1 records.
- Largest colors: N1 1.69%, C4 0.74%, P-sufficiency-wall 0.48%, C19 0.28%.

## Work now

- ACTIVE `Q243` (integrate, C23): codex-12 84195a7f report read (opaque citation identity carried by Step copies; literal assumption aliases not printed; block is its own nested list via lexical-owner tokens; 9 modules: 27 changed proofs, 37 blocks, 0 errors; 2256 tests); open for the census read: 25 single-fact blocks whose fact is a cast of the assumption may still print the same formula twice; HELD for batch 5.
- ACTIVE `Q240` (integrate, C4): codex-2 868794de diff read (ConditionalSelectionProvenance on captured rows for if_pos/if_neg/dif_pos/dif_neg/ite_congr; survives reversal, relabelling, reason transfer; 252 records byte-identical; 2250 tests); HELD for batch 5.
- ACTIVE `Q246` (data, C7): claude-7 branch claude7/Q246 469dc608; a95A02 rendered with robformat-3: raw name gone; goes in with the library switch after batch 5.
- ACTIVE `Q235` (integrate, C16): codex-26 af66dd6c reviewed OK (two-factor transitivity closer drops the tactic tag and the bare transitivity citation; other citations and premises kept; a01A04 Steps 27 and 37; 2249 tests); HELD for batch 5.
- WAITING `N1-readings` (data, N1), after `-`: batch 3 accepted as d25599e9 (58 readings; 59 rows gain a reason, 0 errors). 19 readings withheld in total because a reason makes their row displayed (list: ~/notes/claude-7/Q158/readings-batch3-withheld.json + 5 earlier); they wait for Q165.
- WAITING `same-print-typed` (integrate, N1), after `Q169`: input: ~/notes/codex-12/Q165/same-print-rows.tsv.
- WAITING `typed-prune` (observe, N2+C4), after `-`: input: ~/notes/codex-12/Q165/guard-rows.tsv and the two rejected census renders in ~/storage/apm-endgame/.
- WAITING `Q245` (integrate, P-sufficiency-wall), after `Q235`: packet written (overnight/Q245.md); same function as Q235.
- WAITING `Q241` (integrate, C4), after `Q240`: packet written (overnight/Q241.md).
- WAITING `Q206` (integrate, P-sufficiency-wall), after `Q245`: codex-13 stopped without a commit (draft in worktree codex-13-Q206, report Q206-REPORT.md): the loop guard keeps the captured rows (2250 tests) but in 4 modules 19 records change and at least 6 newly printed rows are source-less bridges; redo after Q245 decides how source-less and mixed bridges are emitted.
- WAITING `Q248` (integrate, P-sufficiency-wall), after `Q245`: to write after Q245 (row classes decide display vs sentence).
- WAITING `Q249` (integrate, C16), after `Q246`: packet written (overnight/Q249.md); needs the robformat-3 library head.
- WAITING `Q250` (integrate, C16), after `Q245`: packet written (overnight/Q250.md); same function as Q235/Q245.

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
