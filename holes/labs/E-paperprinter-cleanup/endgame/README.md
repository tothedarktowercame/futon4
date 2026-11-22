# PaperPrinter endgame: process model and dry run (claude-10, 2026-11-20)

## Dispatch and backlog observation (2026-11-22)

Joe's intervention adds a second, symmetric lane: understand the incoming
backlog while one structural candidate moves through integration.  See
`DISPATCH.md`, `backlog.tsv`, and `dispatch.tsv`.  `python3 dispatch.py --check`
enforces complete backlog coverage and at most one active structural
integration candidate; observation work may remain parallel.

`observations.tsv` retains supported, falsified and superseded vocabulary or
notation hypotheses as evidence for a future expository grammar.  Run
`python3 progress.py > PROGRESS.md` for the operator-facing measurable
residual, current train, planning band and acceleration levers.

`ink.py` applies class-specific colors to affected proof paragraphs and reports
their union as a percentage of reader-visible proof-source ink.  `INK.md`
records the accepted-head census and its coverage limitation; the generated
HTML, annotation JSONL and TSV live beside the accepted-head renders.

`AUDIT.md` routes fine-grained findings among presentation residue,
dependency/scope loss, source-proof cleanup opportunities and mathematical
defects.  This prevents a striking printed sentence from being mistaken for a
bad Lean proof before its typed context has been checked.

Joe's go, 2026-11-20: freeze the list of defect classes, require a count that falls,
give each class one of three exits (printer fix, Lean notation change, accepted residual),
and stop when every frozen class has left. Status: draft, built from claude-7's queue logs
only. `classes.tsv` and `queue.tsv` are claude-10's reading of those logs and have not
been checked by claude-7; rows with size `?` need a census count. (Superseded: see "Checked by
claude-7" below. The model is v2, 9 checks; the dry run takes `--from-commit a6e043d3`.)

    bb -cp ~/code/futon3c/src check.clj endgame-process.json   # causal model, 8 checks
    python3 endgame.py --selftest
    python3 endgame.py --freeze 2026-11-20                     # dry run; --cap 3 --min-size 2

`check.clj` is copied from exotype `causal/check.clj` (branch c1/causal-model).
`counts.tsv` (commit, class, open; in merge order) does not exist yet: it is the per-merge
count the plan needs, and comes from a full rerender at the freeze commit.

## Checked by claude-7, 2026-11-20

- `classes.tsv` and `queue.tsv` are corrected against claude-7's notes and a scan of the head renders.
  No class has size `?` now.
- **Freeze commit:** mfuton-share `claude7/integrate` **a6e043d3** (Merge Q133r). Its full render (8,778 records,
  0 proof errors) is `~/storage/apm-endgame/freeze-a6e043d3/render-0?.jsonl.gz`. It is a composite: a full
  6-shard census at the Q133r merge candidate, the 49 modules touched by the review fixes rendered again at the
  final tree, and the 4 budget-edge proofs (class N8) taken from the last render in which they finished.
- **Scanner:** `scan.py` (with `leanism_scan_p1.py`, codex-3's raw-word scanner). It counts C1, C4, C5, C6, C7,
  C8, C16, C17, N1 and proof errors (N7) as records over the whole render. The other classes have no regex and
  are counted by declaration.
- **`counts.tsv`** has four commits: 6fa6d21c (Q109, the first full render), d57cfd0e, 487bb50a, a6e043d3.
  The dry run reports C4 and N1 rising at d57cfd0e. Cause: Q125 (62c164a8) made 13 proofs finish that had timed
  out, so their text entered the corpus; no rule changed those counts.
- **Why earlier counts looked closed.** Until Q109 every count in the queue log was taken on the 757 records
  still flagged at r10. "by transitivity 12 -> 0" and "empty rows 116 -> 76" are true there; on all 8,778 records
  they are 52 records and 1,928 rows. The freeze corpus has to be the whole render.

## No-residual mode (2026-11-21)

`python3 endgame.py --freeze 2026-11-20 --from-commit a6e043d3 --no-residual` applies Joe's ruling as reported
by claude-7 (open classes are cleaned up, not left as residue): no exception exit; a class with `cap` rejected
packets is listed as `redesign` and its queued packet as a NOTE for the reviewer. See O10 in the model.

## C4 scanner change (claude-10, 2026-11-21)

Q148 (claude-7) found that 155 of the 809 repeated rows at a6e043d3 were not chain rows: `scan.py` split an
`align*` body at every `\\`, including the line breaks of a nested `cases` or matrix. `align_rows` now splits
only at breaks outside a nested environment. C4 at a6e043d3: 273 records / 815 rows before, 248 / 699 after.
The drop is 116 rows, not Q148's 155; claude-7 traced the other 39 (2026-11-21): 38 of the
693 repeated rows at c57304d9 are whole chain rows that contain a nested `cases` and repeat as a row; the old
splitter cut them into fragments, which Q148 classed as artifacts, and as whole rows they are real repeats. `counts.tsv` C4 rows from
a6e043d3 on are recounted with the new scanner over the stored renders (248 to ac0c4de1, 246 from 4dbcba98);
the three earlier commits (272, 273, 273) keep the old scanner's numbers and have no stored render.
This fall comes from the scanner, not from a merge.
