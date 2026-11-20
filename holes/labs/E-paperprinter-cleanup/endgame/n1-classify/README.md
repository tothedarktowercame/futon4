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

## Checked by claude-7 (2026-11-20)

What I did: read both scripts and the printer at a6e043d3 (`paper_walk.py`, `_lemma_label` and
`proof_rewrite_transition`); re-ran the classification on all 1,035 records with extra columns
(`check_claude7.py`, output `check-claude7.tsv`); read about 40 rows against the freeze render.
My run reproduces the (row, head) pairs of `empty-reasons-full.tsv` exactly, 1,928 of 1,928.

### 1. Pairing

- **The description above is wrong about where the head comes from.** `proof_steps` already returns
  policy-applied steps for the APM printer: the "raw" reasons are strings such as `algebra` or
  `since a Sylow subgroup has ...`, and the scripts then call `apply_policy` a second time. Every paired
  row has an empty `matched.reason`, so the head never comes from a `RewriteTransition`. It comes from
  the wrapper around `printer.proof_rewrite_transition`, which records the equality proof's head for each
  call, and is joined to the row by the rendered (before, after) text, accepted only when all calls with
  that text agree on one head. 1,842 of 1,900 paired rows have exactly one such call.
- **That join is sound for nearly all rows.** I compared each rendered row's right-hand side with the
  paired transition's `after`: 1,853 of 1,900 are identical. Of the 45 count-paired rows that differ, the
  12 I read differ only in a local name's reading (`\nu` in the render, `\mathrm{negIso}` in the script).
- **Two rows are paired wrongly**, both in the text pass, which accepts a transition whose `after` is only
  a suffix of the row: `apm_m94a02` (`after` is `v`; head `Matrix.one_mulVec`) and
  `..._edist_le_eVariationOn_of_mem_disjWithin` (head `PseudoEMetricSpace.edist_comm`). Another 21 text-pass
  rows had two or three distinct candidates and took the first; their text is identical, so I left them.
- **The head column prefers the outer head to the printer's reason** (`x['head'] or x['reason']`). For 22
  rows (`Iff.mpr` 20, `Iff.mp` 2) the printer's own reason is a named lemma (`Set.inter_eq_left`,
  `MonoidHom.range_eq_top`, ...). Those 20 `Iff.mpr` rows belong in the library-lemma line, not the
  congruence line.

### 2. What empties the reason

Yes, the policy, and almost entirely one branch of it. For each row I recorded the `RewriteReason` the hook
returned (column `rule`):

| state of the hook's reason | rows |
|---|---|
| lemma named, no instantiated statement | 1,885 |
| lemma named, its statement is the row itself (`restates_row`) | 12 |
| mixed or not paired | 31 |

The printer does not print lemma names on rewrite rows at all. `_lemma_label` prints `since <the lemma
instance's statement>`, or a phrase from the APM readings data keyed by lemma name, and returns an empty
reason when neither exists: the last `drop_self_restatement` branch ("No presentable theorem statement
survived inference"), from 05d8f912 "Avoid restating rewrite chain reasons" on top of 0012fb82. So these
rows are the ones where the hook found the lemma but `infer_type` on its application gave nothing
printable (not found in the term, still a Pi, a placeholder, or not presentable). Which of those four it
was is not measured here.

### 3. The 365 congruence rows

Not fairly called non-defects as a group. The head only says how the fact was applied; the fact is inside
the term. From 11 rows read against the render:

- definitional unfoldings, where no reason is needed: `half disk map(z) = (...)^2`, `matrix(n,a,b,i,i-1) = cases ...`;
- rows that use a fact the reader does not see: `\int y(x)^2 = \int (\sqrt{2}/\sqrt{\pi} \sin x)^2` (uses what `y` is),
  `cases ... = 0` (uses that the condition fails), `\int \omega f(x\omega) = \int \omega^2 f(x)` under `integral_congr`,
  `\sum w(i) v(i) = \sum w(i) x(x'_i)` under `Finset.sum_congr`.

The `*_congr` lemmas (about 70 rows) take a pointwise identity as hypothesis, and that identity is the
unseen fact. For `congrFun'` (227) the printer already looks for a named function equation
(`_congrfun_named_reason`); these are the rows where it found none.

### 4. Splitting N1

I withdraw the split by typed head: the head does not give it, as claude-10 says. I agree with freezing N1
as one class, since 1,885 rows leave through one code branch. Two amendments to the proposed exit:

- The exit rule cannot be only "which lemma names to print". The existing reading is the instantiated
  statement; a row whose lemma instance is the row itself (rewriting with `pow_mul` at top level) stays
  empty by design, and that is right on paper. The count will not reach zero, so the class needs a stated
  rule for which empty rows are accepted.
- Routes out that exist today: a data reading per lemma (`rewrite_readings`; the top heads are
  `MeasureTheory.integral_add` 24, `abs_add_le` 22, `Nat.factorization_mul` 21), and repairing statement
  inference for the 1,885. A count of why inference gave nothing would say which is larger; that is one
  discovery packet.
