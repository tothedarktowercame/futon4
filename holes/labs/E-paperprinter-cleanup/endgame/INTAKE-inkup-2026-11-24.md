# Intake: InkUp dialogue trial findings (claude-12, 2026-11-24)

Source: `~/storage/futon6/inkup/paperprinter-findings-20261124.md` (36 entries; a99J07, a00J01, a96J06), sent by
claude-12 at Joe's request. Three reader seats compared a hand-written proof with the render of the accepted head
`7db1eb81` and asked about every step they could not follow; codex-18 answered from the Lean. The readers could
justify 13 of 29, 12 of 24 and 10 of 23 printed steps before the answers. Those three figures are reader
measurements on three records; they are not a corpus rate.

Why this matters for the lab: most of these places carry little or no residual ink. A sentence "by (3), (5), (6)"
or "by simplification" has a reason, so it is not counted in N1; an empty "Suppose w = 0" block prints no garbled
text. The ink measure does not see them. Each class below therefore needs its own typed corpus count before any
packet changes the printer (claude-7's mapping, 2026-11-24; the entry ids are those of the source file).

| Class (classes.tsv) | Entries | What the reader could not follow | First step |
|---|---|---|---|
| **P-unnamed-library-result** (new) | a99J07 Q3, Q4, Q10; a00J01 Q1, Q6, Q10, Q17; a96J06 Q2, Q7 | the Mathlib theorem that carries a step is printed as step numbers only, "Lemma n", or "by simplification" (maximum modulus, extreme value, L^p monotonicity, `tendsto_order`, Schwarz equality case, `Polynomial.eq_of_infinite_eval_eq`) | observe Q342: closers and facts whose proof head is a library constant with no reading, where the printed reason is only local citations or the generic simplification phrase; count by constant |
| **P-misattributed-reason** (new) | a99J07 Q6; a00J01 Q12; a96J06 Q1 | the printed reason is not what justifies the step: "by definition of the inverse" for an order argument; "by (5) and (7)" listing every tactic dependency; "this is (4)" pointing at the applicability fact and not at the hypothesis applied | observe Q343: for a fact proved by a tactic block, which premises are cited against which are used by the final term; "this is (n)" closers whose Step n is an argument, not the applied function |
| **P-implementation-vocabulary** (new) | a99J07 Q2, Q7; a00J01 Q7, Q9, Q11, Q15 | Lean implementation shows through: "evaluated through id" (eval₂), a constructed object named but never defined (degree-N reflection), a callee's bound-variable name (α) where the caller's is a′, `toReal`/ENNReal conversions hidden while their side conditions are printed | observe Q344: (a) application readings that print a ring-hom or coercion argument which is the identity; (b) obtain sentences whose bound name comes from the applied lemma's binder; (c) side-condition facts consumed only by an erased coercion (joins C4 class E of Q306) |
| **C24** (new) | a96J06 Q16 | "z − 0 · slope(g,0,0)" for (z − 0) · dslope(g,0,0): missing parentheses change the expression; also dslope printed as slope | check against the typed term, then fix packet Q345 (one site known; count subtraction-as-left-operand-of-product over the corpus) |
| **C25** (new) | a00J01 Q8 | the limit of n ↦ t^(1/n) is printed as ((n ↦ 1/n) ∘ n): the outer function of a composition is lost; "is continuous at on 0" | check against the typed term, then fix packet Q346 |
| P-missing-subproof (existing, open) | a99J07 Q12, Q13; a00J01 Q4, Q20; a96J06 Q6, Q9, Q10 | the closing step of a subproof is not printed: the final rewrite and monotone multiplication, norm multiplicativity with a unit factor, unit-measure rewrites, an empty upper-bound bullet, scaling cancellation, a range inclusion that stops at "n ≥ 0", the coefficient contradiction behind "absurd" | add these seven sites to the class's evidence; observe Q347: closers whose proof term has typed steps (rewrite, calc, apply) that produced no Step |
| P-empty-claim (existing, open) | a99J07 Q5 (twice: w and v) | "We claim w ≠ 0: Suppose w = 0" with nothing under it; the contradiction with ‖w‖ > 1 is lost | joins the class; the contradiction closer belongs with C23 (Q197 family) |
| P-sufficiency-wall (existing, open) | a99J07 Q11; a96J06 Q15 and the round-2 note | a `convert`/`simp` residual goal is printed as "It suffices to show X" after "Hence X" with the same expression; the real residual goal (the value equals 1) and its reason (r ≠ 0) are not printed | the a96J06 Declaration 2 site is a new witness: `convert … using 1 <;> simp`; add to Q336's successor |
| not a printer matter | a00J01 Q3, Q16; a96J06 Q5, Q8, Q18; the unused hypotheses in a00J01 Q4 | formal-proof choices, reader gaps, statement hypotheses | none; recorded so they are not re-raised |

Anonymous "Lemma n" numbering (a99J07 Q11, a96J06 Q15: the reader cannot tell which declaration Lemma 2 is) is a
page-assembly question (the declaration numbering exists in the library view but not in a single record read
alone); noted here, no packet until Joe says how records are to be read standalone.

## The a96J06 Lean simplification

claude-12 found that `Complex.affine_of_mapsTo_ball_of_norm_dslope_eq_div` applies directly with R1 = R2 = r
(`~/storage/futon6/inkup/a96J06/20261124T-dialogue/experiment-direct-lemma.lean`, theorem `direct_test`), which
would remove the rescaled function g, its six steps and Declaration 2. Not applied. The Lean source is not
claude-7's to change; if Joe wants it, it goes in after batch 14 is accepted, and a96J06 is then rerendered and
its changed records attributed to the source edit. Open decision for Joe.

## Status

Recorded 2026-11-24: classes.tsv and backlog.tsv rows for the five new classes; dispatch.tsv observe items
Q342–Q347 (waiting for a free seat; all seats are on batch 14). Each observe packet must measure over ≥15 modules
and stop before any printer change.
