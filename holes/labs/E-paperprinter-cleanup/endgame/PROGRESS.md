# PaperPrinter residual — 2026-11-23

**Status: rendering works; informalisation validation is in progress** (`READINESS.md`: six gates, one met). The ink figures below are an audit surface, not a share of correct proofs.

## Finish line

- Frozen classes: **56**: 30 done, 2 not defects, 22 open, 2 unknown.
- Corpus gate: 8,778 records; accepted head with a full census: `2a2b4d08`.
- Proof errors: 0 at the accepted head. Performance class: closed.
- N1 (rows with an empty reason): **159 records** at this head, down from 1,035 records at the full-corpus freeze.
- C4 (blocks with a repeated left side, scanner count): **28 records**.
- Residual ink (audit surface; 24/24 open-or-unknown classes have markup, extractor precision not yet reviewed): **3.603%** of reader-visible proof-source characters, in 354 records; see `INK.md`.
- Ink change since `94c86724`: union -0.003 points; no class changed its record count.
- Largest colors: N1 1.62%, C4 0.73%, P-sufficiency-wall 0.48%, P-empty-claim 0.27%.

## Work now

- ACTIVE `Q256` (integrate, P-sufficiency-wall): packet written (overnight/Q256.md); dispatched to codex-15 on b0a112b7 while batch 7 is censused (different function from Q249/Q252); authored on b0a112b7 (94c86724 adds only Q249 and Q252, which touch the Finset.card reading and the contradiction closer, not rewrite rows); to be merged onto 94c86724.
- ACTIVE `Q241` (integrate, C4): codex-2 c3bc1e97 (2264 tests; 3 t00A05 records change), reviewed and HELD, not for batch 6 as it stands: the partition by typed branch path is right, but the result is ten context-free displays printed after the case prose (both "= 1+3 since the condition holds" and "= 5-(5-s) since the condition fails" for one table), no easier to read than the one mixed chain; it is merged only together with Q248, which places each branch chain inside its case; authored on 91d38c1e (b0a112b7 = 91d38c1e + one notation rule); base column is the head it would be merged onto (94c86724).
- ACTIVE `Q253` (integrate, P-missing-subproof): packet overnight/Q253.md; dispatched to codex-12 on 94c86724; codex-12 26b088ea (2272 tests; 8 of 554 records change, each loses one Suppose block); diff read; trial batch 8 (92b8531d), census q244.
- ACTIVE `Q260` (integrate, C19): packet overnight/Q260.md; dispatched to codex-26 on 94c86724; codex-26 e5ff3506 (2269 tests; m02A02: 4 of 56 records change, 8 lines, parentheses only); diff read, held for the next batch.
- ACTIVE `Q261` (integrate, C19): packet overnight/Q261.md; dispatched to codex-26 on 94c86724 (Q260 reviewed and held; neighbouring function, same test file, merged in the same batch); codex-26 fbb7c7ed (2269 tests; m94J05: 3 of 64 records, 4 lines, parentheses only; one leading-minus template exists); diff read, held for the next batch.
- ACTIVE `Q262` (integrate, P-empty-claim): packet overnight/Q262.md; dispatched to codex-13 on 94c86724; codex-13 8d1993a3 (2270 tests; 22 claims in 20 records become "We have X — by (N)", 860 other records in those modules unchanged); diff read (10 lines in render.py), held for the next batch.
- ACTIVE `Q263` (integrate, P-empty-claim): packet overnight/Q263.md; dispatched to codex-12 on 94c86724 (helpers/paper_walk; Q262 is in render.py).
- ACTIVE `Q267` (integrate, P-duplicate-closer): packet overnight/Q267.md; dispatched to codex-2 on 94c86724; codex-2 cd2b3cdf (2271 tests; one-word change; 10 closers gone in 7 records, plus 1 more m93J05 record whose owner "First, we show X:" is left with nothing under it); diff read, held for the next batch; at the batch merge the completed-fact sentence of Q262 must cover a part owner (my review fix).
- ACTIVE `Q268` (integrate, P-duplicate-closer): packet overnight/Q268.md; dispatched to codex-2, authored on top of its held Q267 commit cd2b3cdf (same predicate).
- ACTIVE `Q270` (observe, N1): packet overnight/Q270.md; dispatched to codex-13 on 94c86724.
- ACTIVE `Q271` (integrate, P-missing-subproof): packet overnight/Q271.md; dispatched to codex-26, authored on top of Q253 26b088ea (same file, in census q244).
- WAITING `N1-readings` (data, N1), after `-`: batch 3 accepted as d25599e9 (58 readings; 59 rows gain a reason, 0 errors). 19 readings withheld in total because a reason makes their row displayed (list: ~/notes/claude-7/Q158/readings-batch3-withheld.json + 5 earlier); they wait for Q165.
- WAITING `same-print-typed` (integrate, N1), after `Q169`: input: ~/notes/codex-12/Q165/same-print-rows.tsv.
- WAITING `typed-prune` (observe, N2+C4), after `-`: input: ~/notes/codex-12/Q165/guard-rows.tsv and the two rejected census renders in ~/storage/apm-endgame/.
- WAITING `Q206` (integrate, P-sufficiency-wall), after `Q256`: codex-13 stopped without a commit (draft in worktree codex-13-Q206, report Q206-REPORT.md): the loop guard keeps the captured rows (2250 tests) but in 4 modules 19 records change and at least 6 newly printed rows are source-less bridges; redo after Q245 decides how source-less and mixed bridges are emitted.
- WAITING `Q248` (integrate, P-sufficiency-wall), after `Q256`: to write after Q245 (row classes decide display vs sentence); authored on top of Q241 c3bc1e97 so that each branch-local chain is placed in its case; evidence ~/notes/claude-7/raw-names-480/proofs/Q208-census/.
- WAITING `Q250` (integrate, C16), after `Q256`: packet written (overnight/Q250.md); same function as Q235/Q245.
- WAITING `Q254` (integrate, C23), after `Q253`: to write.
- WAITING `Q255` (integrate, P-missing-subproof), after `Q254`: to write.
- WAITING `Q264` (integrate, P-empty-claim), after `Q263`: to write (Q257-REPORT section C); same application seam as Q263.
- WAITING `Q265` (integrate, P-empty-claim), after `Q264`: to write (Q257-REPORT section D).
- WAITING `Q269` (integrate, P-duplicate-closer), after `Q268`: to write (Q258-REPORT section 3); walk metadata plus the same predicate.
- WAITING `Q272` (integrate, P-missing-subproof), after `Q271`: to write (Q266-REPORT section C; 5 heads with four or five direct arguments); APM closer sentence, the shared cap is not edited.
- WAITING `Q273` (integrate, P-sufficiency-wall), after `Q256`: to write (Q266-REPORT section A); first measure how many let Steps of species paper_equation_rewrite come from a have; after Q256 decides the rewrite row classes.
- WAITING `Q274` (integrate, P-missing-subproof), after `Q271`: to write (Q266-REPORT section B; a01J06 hcast); typed cast-normal comparison, never a comparison of rendered sides.

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
