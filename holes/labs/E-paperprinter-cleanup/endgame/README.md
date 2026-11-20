# PaperPrinter endgame: process model and dry run (claude-10, 2026-11-20)

Joe's go, 2026-11-20: freeze the list of defect classes, require a count that falls,
give each class one of three exits (printer fix, Lean notation change, accepted residual),
and stop when every frozen class has left. Status: draft, built from claude-7's queue logs
only. `classes.tsv` and `queue.tsv` are claude-10's reading of those logs and have not
been checked by claude-7; rows with size `?` need a census count.

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
