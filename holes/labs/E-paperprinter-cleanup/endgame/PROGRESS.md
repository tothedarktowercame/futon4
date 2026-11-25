# PaperPrinter residual — 2026-11-25

**Status: rendering works; informalisation validation is in progress** (`READINESS.md`: six gates, one met). The ink figures below are an audit surface, not a share of correct proofs.

## Finish line

- Frozen classes: **67**: 30 done, 2 not defects, 30 open, 5 unknown.
- Corpus gate: 8,778 records; accepted head with a full census: `fb09614c`.
- Proof errors: 0 at the accepted head. Performance class: closed.
- N1 (rows with an empty reason): **18 records** at this head, down from 1,035 records at the full-corpus freeze.
- C4 (blocks with a repeated left side, scanner count): **12 records**.
- Residual ink (audit surface; 25/35 open-or-unknown classes have markup, extractor precision not yet reviewed): **0.803%** of reader-visible proof-source characters, in 82 records; see `INK.md`.
- Ink change since `ee47b461`: union -0.776 points; C4 -47 records, C7 -2 records, C12 -1 records, C16 +2 records, C17 -2 records, C23 -1 records, C26 -1 records, N1 -4 records, N14 -1 records, P-duplicate-tag -1 records, P-opaque-proof -1 records, P-overexpanded-setup -1 records, P-premature-conclusion -2 records, P-sufficiency-wall -6 records.
- Largest colors: C4 0.38%, P-sufficiency-wall 0.15%, N1 0.14%, C23 0.10%.

## Work now

- ACTIVE `Q428` (observe, P-lost-row): Q428-REPORT.md: all 19 lost/worse records first absent at bc8e9b6d; remover proof_rewrite_owner_filter (captured row whose local_fact_owner differs from the chain owner); Q428b measuring which typed predicate separates wrong from right drops | Q428b failed wall-clock at 64 min with no report; Q428c (kimi-7): write-up from the surviving traces | Q428c write-up done (Q428-REPORT.md ## Q428b, Q428b-TABLE.md): no predicate separates 17 WRONG from 7 RIGHT; typed endpoint predicate P2 has 0 RIGHT hits and reaches 7/17 -> Q460 (kimi-7) implements P2 as a partial fix; 10 records owed; Q404a rule A disjoint | Kimi 5-hour quota hit 17:39 UTC 2026-11-24; re-sent to zai-19 (Q460).
- ACTIVE `TB002` (integrate, P-unnamed-library-result): claude-16 branch claude-16/T002 bec6f0ce+9606a152 on a31673ee; 7 records in 14 Q410 modules gain one closing sentence vs q258; walk gate 696 passed; report ~/notes/claude-16/tail/REPORT.md.
- ACTIVE `TB003` (data, N15): claude-16 97d9b4ac on a31673ee; the five pass (166 s).
- ACTIVE `TB004` (integrate, P-implementation-vocabulary): claude-16 13e5b66b on a31673ee; 11 changed fields in 5 modules vs q258, all Nonempty readings; walk gate 695 passed.
- ACTIVE `TB006` (integrate, C18): claude-16 d03bfee5 on a31673ee (no rewrite-unit or wrapper-hook code): proof_decided_case_condition hook; Step 0: only t00A05 squarePerimeter_squareOfPerimeter in q258 (2 splits); 4-module render vs ee47b461: 1 record changes, only the two vacuous splits and dead arms; walk gate 695 passed. Not an erratum: the vacuous case was argued correctly.
- ACTIVE `TB009` (integrate, N1): claude-16 7402670c on 0f8ddc9b: Step 0 all 30 joins of 3+ parts come from ApmPaperPrinter.proof_rewrite_wrapper_label (one site); _apm_join_reason_parts lists them, algebra last; 14 declarations: 30 changed lines, all 3+ part rows; 6 two-part "algebra and X" rows flip (a97A08, b94J02, t96J02); Q388b pins updated; walk gate 720 passed.
- ACTIVE `TB011` (integrate, N1): claude-16 a0f41496+c44ed903 on d4219fd3; 4 Q399 modules (214 records) base vs new: 1 line changes (m93J06 hd); walk gate 727 passed; mutation caught.
- WAITING `Q241` (integrate, C4), after `Q349`: HELD, not in flight (status corrected 2026-11-24). codex-2 c3bc1e97 (2264 tests; 3 t00A05 records change), reviewed and HELD, not for batch 6 as it stands: the partition by typed branch path is right, but the result is ten context-free displays printed after the case prose (both "= 1+3 since the condition holds" and "= 5-(5-s) since the condition fails" for one table), no easier to read than the one mixed chain; it is merged only together with Q248, which places each branch chain inside its case; authored on 91d38c1e (b0a112b7 = 91d38c1e + one notation rule); base column is the head it would be merged onto (94c86724).
- WAITING `Q206` (integrate, P-sufficiency-wall), after `Q256`: codex-13 stopped without a commit (draft in worktree codex-13-Q206, report Q206-REPORT.md): the loop guard keeps the captured rows (2250 tests) but in 4 modules 19 records change and at least 6 newly printed rows are source-less bridges; redo after Q245 decides how source-less and mixed bridges are emitted.
- WAITING `Q248` (integrate, P-sufficiency-wall), after `Q256`: to write after Q245 (row classes decide display vs sentence); authored on top of Q241 c3bc1e97 so that each branch-local chain is placed in its case; evidence ~/notes/claude-7/raw-names-480/proofs/Q208-census/.
- WAITING `Q405` (integrate, N1), after `Q424`: packet to write.
- WAITING `Q411` (integrate, N1), after `Q424`: packet to write after Q403 lands; re-measure first.
- WAITING `Q366` (integrate, P-sufficiency-wall), after `Q350`: packet to write; after the N1 queue.
- WAITING `Q343` (observe, P-misattributed-reason), after `-`: intake INTAKE-inkup-2026-11-24.md; waits for a free seat.
- WAITING `Q344` (observe, P-implementation-vocabulary), after `-`: intake INTAKE-inkup-2026-11-24.md; waits for a free seat.
- WAITING `Q347` (observe, P-missing-subproof), after `-`: intake INTAKE-inkup-2026-11-24.md; waits for a free seat.
- WAITING `TB016` (observe, P-empty-chain-reason), after `TB014`: kimi-6 job failed on the Kimi 5-hour quota (HTTP 429) after rendering 20 decls/17 modules (~/notes/kimi-6/TB016/rendered.jsonl; 8.2 GB steps trace deleted), no report. Producers partly known from Q459 (a94J06 x3, m99A02: pending goal chains never claimed) and Q457 (G1 congrFun heads, in flight); re-measure N1 at the next census before re-sending. | kimi-6 invoke-1795541454707-28445-28ac53b7 (park-be6650e0); packet ~/notes/claude-16/tail/TB016.md, 20 rows / 16 modules via --declarations (m03J05 left out).
- WAITING `TB015` (observe, P-sufficiency-end), after `-`: claude-16 observe at 4fe695ba + census q258 ee47b461: STOP rule hit (>10): 468 records whose proof ends 'Hence P.' / 'It suffices to show Q.' (308 with P==Q verbatim, 160 differ; 104+138 modules); belled claude-7 (overlaps open class P-sufficiency-end, 849 records; Q385 sample); notes ~/notes/claude-16/tail/TB015-observe.md.
- WAITING `Q456` (integrate, C4), after `Q447`: STOPPED at Step 0 (zai-15): a98A07 #31 row 3 carries a content-free rewrite_source+named_citation; redirect (a) discount or (b) a95J03 only pending. Q456-REPORT.md.

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
