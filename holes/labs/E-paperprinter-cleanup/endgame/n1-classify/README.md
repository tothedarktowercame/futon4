# N1 empty-reason rows, classified from the typed proof (2026-11-20)

Corpus: the freeze render at mfuton-share a6e043d3 (8,778 records). Scanner N1: 1,928 rows in 1,035 records.

`classify.py` is codex-5's script (job invoke-1795169396947-25551-3fc51089, which ended before producing output);
claude-10 ran it, output path changed only. It pairs each rendered empty-reason row with the typed
`RewriteTransition` and reads the proof head the printer had before the paper-walk policy removed the reason.
`classify_textmatch.py` is claude-10's second pass for the 279 records where the count of typed empty
transitions differed from the count of rendered rows: it pairs by the text of the right-hand side.

Run from an mfuton-share worktree at a6e043d3:
`MFUTON_HOME=$PWD PYTHONPATH=src:.:/home/joe/notes/codex-1/w5/python-deps MFUTON_LEAN_NATIVE_MEMGUARD=0 ~/.venvs/mfuton314/bin/python classify.py --part I --parts 8`

## Result (`empty-reasons-full.tsv`, 1,928 rows, 1,035 records)

| typed head | rows | records |
|---|---|---|
| named library lemma (592 distinct heads in the first pass) | 1,458 | 837 |
| congruence step (`congrFun'`, `eq_of_heq`, `*_congr`, `Iff.mpr`, ...) | 365 | 280 |
| lemma of the project itself | 63 | 43 |
| `algebra` | 9 | 9 |
| no head, or `Eq.mp` (the script's class d) | 5 | 4 |
| not paired with a typed transition | 28 | 16 |

The congruence/library/project split is claude-10's regex over the head name, not the script's column 5
(which is f for all of them). Records can appear in more than one line.

## What this does and does not show

- The typed proof has a named lemma for 1,895 of 1,928 rows. Rows with no named fact behind them are 5.
  So "definitional step, nothing to cite" is not what most of these rows are at the typed level.
- It does not show how many rows a reader needs a reason for. `pow_mul`, `add_comm`, `integral_add` are named
  lemmas a reader would not want cited; `IntermediateField.adjoin.finrank` or
  `MeasureTheory.norm_integral_le_integral_norm` they would. The head name alone does not separate these,
  and Q108's 277 of 549 "identity or definitional" was a judgment of that second kind.
- Checked by hand: 8 random library-lemma rows; in each the named lemma fits the rendered step.
