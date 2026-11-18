# E-paperprinter-cleanup — the PaperPrinter endgame and its Arxana records

**Opened:** 2026-11-18 by claude-5, at Joe's direction: "At this point any further improvements or deficiencies along these lines should go into an E-paperprinter-cleanup excursion and we should redirect our focus to the mainline Arxana tasks." and "claude-7 is running and realistically should take ownership of E-paperprinter-cleanup and everything to do with the PaperPrinter process."
**Owner:** claude-7.
**Home:** M-arxana-incubator (`holes/missions/M-arxana-incubator.md`), which used PaperPrinter as VERIFY Witness 1; §5 has the experiment and its write-up, §6 the turn to mainline Arxana.
**Process (Joe, 2026-11-18):** "Hopefully it will not runs long turns that end in failure that slows the process down." claude-7's class-A job (`invoke-1794959226182-23376-b6844059`) ran two hours and was killed (exit 137) after Q88, before its report, leaving the m94J02 fix uncommitted. So: one merge or fix per turn, commit it, and report before starting the next.

## State at hand-over (2026-11-18, from claude-7's job log and the repos)

claude7/integrate (mfuton-share, local): Q84 1970149b, Q87 acfa8fed, Q85 c62a8ba6, Q88 9f3836f6, m94J02 fix 0c3d8631 (head). Nothing pushed.

## Open items

1. **Q79c** is not merged: it breaks `test_a97j06_trichotomy_…`, which passes at c62a8ba6 (codex-3's report called it pre-existing; claude-7 found it is not).
2. **m94J02 (class 10): done.** Committed as 0c3d8631 (claude-7, 2026-11-18) after the killed job: the APM `_with_bound` override keeps a term binder off the letters of chosen families when its body contains `Classical.choose`. The proof now reads "‖x_n − y‖ < ε for all sufficiently large n". Gates: unit 2118, real-corpus 52; 1 of 489 declarations changed. No Lean change was needed.
3. **The census: done.** r11 at 0c3d8631 over the 757 records flagged at r10, 0 errors. Records with any tracked defect: 162 → 85. "cases on A ∨ B" went 79 → 0; ⊥ 30, ⊤ 10, membership lambda 15, `\seqsplit` 14 and raw-word records 34 are unchanged. Table in `~/notes/claude-7/endgame-short-list-2026-11-17.md`, "Update 2026-11-18".
4. **Endgame list** (`~/notes/claude-1/paperprinter-endgame-list-2026-11-17.md`, claude-1 with claude-7): remaining classes, including class 14 (Path) and the m94J02 `x_x` route; Joe on C-10 to C-13: "just change the notation in the apm-lean proofs to get them to clean as well".
   **Status (claude-7, 2026-11-18).**
   - Class 10 is done as a printer fix (item 2).
   - Class 13 (t96J02 `cut_chart_chartInv`, a simp chain that goes back and forth) is now the Lean notation change, because Q79c is held (item 1). It is next in the queue.
   - New **class 15**: m93A01 `apm_m93A01_diagonal_single` renders `lp.single n 1` applied at j as `j` ("it suffices to show j = j"). This is a printer coercion bug, so it is a fix.
   - Class 4 needs the rewritten side (LHS or RHS) as typed data before any printer route.
   - Counts in claude-1's note: fixes 12, DiagramProver conflicts 2 (classes 2 and 9), acceptances 0, to check 1 (class 14).
5. **a02J03 after Q87.** The live Arxana finding `arxana:clusion:sha256:fccc8a8c…` (beta vs the Lean choose-family rule, hit 18 of borel_pi_eq_generateFrom_Icc) predates Q87. claude-5 sent codex-12 a discovery packet (`~/notes/claude-5/packets/codex-a02j03-after-q87-13.md`, job `invoke-1794966504679-23504-edd416e8`): rerun the beta probe at 9f3836f6, render before/at/after Q87, list Q87's functions that run, and propose whether Q87 supersedes a rule, adds a pre-empting rule, or neither. Its report goes to claude-7, who decides the record. Live Arxana writes need Joe's go. **Result (2026-11-18):** codex-12 stopped on a premise error that was claude-5's: the packet said Q87 leaves "one Choose sentence", but claude-7 had said "no extra Choose sentence: x' is chosen once". Head prints two Choose sentences, one for x_i and one for x'_i, which matches claude-7. Beta hits: 44 in the stored capture, 39 at 1970149b, 36 at acfa8fed and 9f3836f6; corpus hash unchanged; probe on/off renders byte-identical; the beta/choose branch itself is unchanged by Q87, which adds the family-under-bound-index reading. Hit alignment and the rule classification were not done. Report `~/notes/codex-12/a02j03-after-q87/REPORT.md`; new dated captures in `~/storage/arxana/importer/`.
   **claude-7's reading (2026-11-18), provisional:** Q87 is a *pre-empting rule*, not a supersession. `_apm_set_pi` is dispatched in `_apm_run` before the lambda/beta path, and the beta/choose branch is unchanged. The record will say this for fccc8a8c only if hit 18's host is a Set.pi argument. That needs the structural alignment codex-12 did not do. One caution: at numeric hit 18, `alias_hit` is already true at 1970149b, so whatever changed there predates Q87 (Q84, or something between 40d06435 and 1970149b). Next step (claude-7, one turn): align the hits by host term, not by ordinal. Then propose the record to Joe.
6. **Residuals.** Before the first accepted residual is written, settle where its extent and reopen condition go (DERIVE-2 §9 R4). claude-1's reading of the endgame list (limits stated only in prose: census extent, lower bounds, one probed path, provisional classes, classes set by ruling) gives the cases.
7. **Probe branches** (mission open question 6): codex-6 `codex-6-q84-alias-probe` bccaee1d and codex-12 `codex-12/step-probe` f87a1048 are local and unpushed; integration is Rob's repo and Joe's call.
8. **History patterns** (mission open question 1): the a97J08 wording history (reversal at 50444f21 → Q66 → Q78) and further supersessions (Q87 if recorded) are its data; the question itself stays with the mission.

9. **Q88 and the live a97J08 sentence records** (claude-5, 2026-11-18).
   - Q88 (9f3836f6) removes the first-split special case of `_merge_right_or_splits`. The live rule record for that function is Q83's `777d1f59…`, pinned at 1970149b.
   - All five a97J08 case-conclusion roles name that rule. Recording Q88 as a supersession would mark all five stale, which would be the first live run of the staleness reader.
   - **Before any record:** re-render a97J08 at 9f3836f6 and compare the five wordings. Staleness means "depends on a changed rule", not "changed".
   - If the wordings changed, propose new manifestations at head.
   - This goes to Joe for his go *first*, ahead of the a02J03 record (item 5).

10. **Class 13 and class 16: done (2026-11-18).**
   - Class 16 is a printer fix, a415b01f with test re-pins in 9756e848: an equality calc displays its steps as one chain, not "by transitivity". Over the 757 r11 records, 18 changed, 0 errors, and the count went 42 → 31.
   - Class 13 is a Lean notation change (STYLE.md S1–S3, apm-lean 6f4499099d). The corpus build branch was fast-forwarded with codex-25 and codex-4 sign-off, and the t96J02 corpus entry was swapped; its provenance is in PROVENANCE.md beside it.
   - Open, Joe's call: the corpus is pinned to the codex25 build, 38 commits behind apm-lean master.

11. **Item 9 checked, 2026-11-18: a97J08 is unchanged by Q88.**
   - apm_a97j08_boundary_sum_aux_disjoint was rendered at 1970149b (the commit the live records pin) and at head 9756e848, from the same corpus file (sha 95da7b18…, the one the manifestations record).
   - The two proofs are byte-identical: all five "In all four cases, we obtain a contradiction." are still there.
   - So staleness would flag five roles whose wording has not changed.
   - Proposed records, which need Joe's go:
     - a rule-content record for `_merge_right_or_splits` at 9f3836f6 that supersedes 777d1f59;
     - five new manifestations at 9756e848 with the same wordings, so the reader can tell "the rule changed" from "the sentence changed".
   - **Written live 2026-11-18 (Joe's go), claude-7:** rule `:q88` `607dc448…` (`_merge_right_or_splits` at 9f3836f6); `:supersedes` `5da29da1…` (successor q88, predecessor 777d1f59); five role-scoped `[:sentence/manifestation 3]` at 9756e848 with the unchanged wording. Read back equal. Script and log: `~/storage/arxana/diagramprover/write-q88-supersession-2026-11-18.clj`, `q88-supersession-2026-11-18.edn`.
   - The staleness reader now marks all five roles stale because of `5da29da1…`, as intended: the rule they depend on changed.
   - Reader gap found: `sentence-records/role-manifestations` reads revisions 1 and 2 only, so it does not return the new revision-3 records (counts after the write: 10, 1, 1, 1, 1). The fix is to add 3 to its revision list.
12. **Item 5 aligned, 2026-11-18: finding fccc8a8c was pre-empted by Q84, not Q87.**
   - The hits were matched by the structure of the term they occur in (binder names ignored, de Bruijn indices kept), not by position. The captures are in `~/storage/arxana/importer/`.
   - The host term of hit 18 (key 5458712d2fd2) still occurs 3 times at 1970149b, acfa8fed and 9f3836f6. At every one of them the decision is `alias_hit: true` (`_apm_choose_family_alias` returns before the beta decision), against `alias_hit: false` in the stored 40d06435 capture.
   - So the Q84 alias rule (merged 1970149b) pre-empts beta at this host, and Q87 does not touch it.
   - Q87 removes 4 other hits, all with `choose_family: false`, which are the Set.pi `(i ↦ [x_i, x'_i])(i)` redexes, one of them reduced. Those are beta on its own, not the pair.
   - Proposed record, which needs Joe's go: fccc8a8c is pre-empted by the Q84 rule `_apm_choose_family_alias` at 1970149b, with this alignment as the evidence. No Q87 supersession of the pair.

## Tools and records already built (futon3c master)

- DiagramProver: `diagramprover/` (term schemas, findings, opaque rules, lean_alias, publish_findings, arxana_records, sentence_records).
- Readers: `futon3c.arxana.dependence` (`fibre`, `staleness`), `store-finding-view`, `sentence-view`, `rule-fibre`, `role-manifestations`.
- Live records and write logs: `~/storage/arxana/diagramprover/` (first write 2026-11-17; rule-set diff; sentence records; scoped manifestations; 59 attestations, all 2026-11-18). Each live write so far has had Joe's go.
- Design: `holes/labs/M-arxana-incubator/DERIVE-2-registry-and-fidelity.md` §7–§9.
