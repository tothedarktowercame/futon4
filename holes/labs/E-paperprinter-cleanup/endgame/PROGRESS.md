# PaperPrinter residual — 2026-11-26

**Status: rendering works; informalisation validation is in progress** (`READINESS.md`: six gates, one met). The ink figures below are an audit surface, not a share of correct proofs.

## Finish line

- Frozen classes: **67**: 30 done, 2 not defects, 30 open, 5 unknown.
- Corpus gate: 8,778 records; accepted head with a full census: `7df1bdcfdeeae0912cf463985749f1bffbe8a64e`.
- Proof errors: 0 at the accepted head. Performance class: closed.
- N1 (rows with an empty reason): **0 records** at this head, down from 1,035 records at the full-corpus freeze.
- C4 (blocks with a repeated left side, scanner count): **2 records**.
- Residual ink (audit surface; 25/35 open-or-unknown classes have markup, extractor precision not yet reviewed): **0.165%** of reader-visible proof-source characters, in 27 records; see `INK.md`.
- Ink change since `a7bebf09f3ae1029246ec875c929b401f59edace`: union -0.009 points; C4 -2 records.
- Largest colors: P-sufficiency-wall 0.08%, P-missing-subproof 0.03%, C4 0.03%, C23 0.01%.

## Work now

- ACTIVE `Q428` (observe, P-lost-row): Q428-REPORT.md: all 19 lost/worse records first absent at bc8e9b6d; remover proof_rewrite_owner_filter (captured row whose local_fact_owner differs from the chain owner); Q428b measuring which typed predicate separates wrong from right drops | Q428b failed wall-clock at 64 min with no report; Q428c (kimi-7): write-up from the surviving traces | Q428c write-up done (Q428-REPORT.md ## Q428b, Q428b-TABLE.md): no predicate separates 17 WRONG from 7 RIGHT; typed endpoint predicate P2 has 0 RIGHT hits and reaches 7/17 -> Q460 (kimi-7) implements P2 as a partial fix; 10 records owed; Q404a rule A disjoint | Kimi 5-hour quota hit 17:39 UTC 2026-11-24; re-sent to zai-19 (Q460).
- ACTIVE `TB003` (data, N15): claude-16 97d9b4ac on a31673ee; the five pass (166 s).
- ACTIVE `Q465b` (integrate, C4): zai-19; packet ~/notes/claude-16/coord/Q465b.md; invoke-1795574693142-28738-d3f11679; zai-19 timed out uncommitted; finished by claude-16 as bedbf907 (claude16/q465b): guard double-counted and keyed on per-row sources, now local facts only; awaiting census; fix 1a09d06e: skip chains Q452 rereads; candidate 4a5953cd, census q264; census q264 (cand 4a5953cd): a96A04/b97J03 lost their only "by Lemma 9/3" rows (break in chain read as return) -> claude-16 fix 4c72f3d1, pins db5d7271 on claude16/cand; census q266.
- ACTIVE `Q471A` (integrate, C4): zai-19; packet ~/notes/claude-16/coord/Q471A.md; invoke-1795577819330-28825-a014504d; base bedbf907; returned 217554b4 (1 decl, t95A01); in candidate claude16/cand 4a5953cd with Q465b + fix 1a09d06e (Q465b cut m02A02 probe_coeff conclusion); census q264.
- BUILT `cand31` bc6a181a = cand30 b8d383a0 + Q520 67ec8058 + Q514 b5c37dda + Q519 48ee0f75 + Q523 040c535b (conflicts: token refresh in _annotate_rewrites keeps value-walk(fact)/conjunct-arm/hypothesis-transport; term_walk tokens merged); targeted tests bg-1795704344262-239 -> /tmp/cand31-targeted.log; park-b7a94d17; census after q295.
- QUEUED `Q514` b5c37dda (reviewed clean, for cand31; t99A02 lineSphereTree wall expected to leave) (discover+fix, P-sufficiency-wall): claude-18 (new helper seat, Joe 2026-11-26); packet ~/notes/claude-16/coord/Q514.md + common12.md; invoke-1795699724533-29458-b6cf8016.
- ACTIVE `Q517B` (implement, C23): claude-21; packet ~/notes/claude-16/coord/Q517B.md; invoke-1795703932446-29485-6ac1cebb; park-753d6334.
- ACTIVE `Q518` (discover+fix, P-sufficiency-wall): claude-22 (new helper seat, Joe 2026-11-26); packet ~/notes/claude-16/coord/Q518.md + common12.md; invoke-1795699747553-29462-026e7623.
- QUEUED `Q519` 48ee0f75 (reviewed clean, for cand31; t01A03 wall expected to leave) (discover+fix, P-sufficiency-wall): claude-23 (new helper seat, Joe 2026-11-26); packet ~/notes/claude-16/coord/Q519.md + common12.md; invoke-1795699753473-29463-96abdabf.
- QUEUED `Q520` 67ec8058 (reviewed clean, for cand31; C16 ×2 expected to leave) (discover+fix, C16): claude-24 (new helper seat, Joe 2026-11-26); packet ~/notes/claude-16/coord/Q520.md + common12.md; invoke-1795699759462-29464-432c21f6.
- QUEUED `Q523` 040c535b (reviewed clean, for cand31; t02A06 wall expected to leave) (discover+fix, P-sufficiency-wall): claude-20; packet ~/notes/claude-16/coord/Q523.md + common12.md; invoke-1795699900856-29468-88a147b8.
- ACTIVE `Q525` (discover+fix, C23, P-sufficiency-wall): zai-16 (Zai month at 93%); packet ~/notes/claude-16/coord/Q525.md + common12.md; invoke-1795700346191-29477-ab3b1e79; park-4e4432fd.
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
- WAITING `Q484B` (implement, C4), after `Q484`: zai-16; packet ~/notes/claude-16/coord/Q484B.md; invoke-1795623401812-29087-32d3a28b; returned 3df1415b+912d9af0: on the census library Q484 already resolves the instance, so Q484B is only a fallback; held until q278 (cand7 = Q481+Q484) shows whether a98A07 is cut without it.

## Backlog understanding

- Observation coverage: **36/35** open or unknown classes.
- Classification state: classified 4, classified (Q183) 1, classified (Q190, Q192) 1, classified (Q216) 2, classified (Q369) 1, classified (Q371) 1, classified (Q385) 1, classified (Q413b, Q422) 1, classified by position (claude-7 from renders; Q182/Q187 typed traces) 1, not yet classified 1, partly-classified 6, reader-reported; not typed 3, reader-reported; one site 2, stale-observation 3, unclassified 6, unknown 2.
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
