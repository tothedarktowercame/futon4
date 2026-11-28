# GEN-INV report (claude-19, 2026-11-28): the walk's fallbacks

Discovery only. No code was changed anywhere. The inventory is `GEN-INV.tsv` beside this file:
one row per site, 49 rows, each with a file:line in the c8076d84 snapshot (`/tmp/integ-snap309`).

## How the counts were made
- **Page counts** (the `occurrences` and `records` columns): a regex for each site's phrase over the
  `proof` field of the 8,778 head records (`~/storage/apm-endgame/head-c8076d84/q309h-k-0?.jsonl`).
  The `count_method` column gives the pattern. Where several sites print the same words ("Hence
  \(X\).", "We have \(X\).", "algebra"), the page count is the shared total, and the row says so.
- **Typed probe, sample** (the `probe_records_of_N` column): the walk re-run (`proof_body`, as
  check_L1 does) under a line tracer on a random sample of head records (seed 5, records whose
  census `proof_seconds` < 4, so slow proofs are under-represented). The column counts the sampled
  records in which the site's emitting line ran. A line that ran may still have been dropped later
  (retention, coalescing), so this says which site is reached, not that its text was printed. It is
  used to split the shared phrases among their sites.
- **Typed probe, definition cards:** `_definitional_sides` on every definition in each module's
  constants table, counting lhs = rhs after removing `\leanref` wrappers. This covers every
  definition in the table, not only the ones a page shows as a card (see the card row below).
- Probe scripts and output: `/tmp/c19inv/` (`probe.py`, `walk.jsonl`, `cards-all.jsonl`,
  `count.py`, `attrib.py`). Run in systemd units with MemoryMax=12G / 8G.

## Sites ranked by records affected (fallbacks only; page counts)

| rank | site | phrase | records | occurrences |
|---|---|---|---|---|
| 1 | render.py:4074, 3984, 3986, 3941, 4181 | "Hence \(X\)." with no reason (all sites) | 3049 | 4048 |
| 2 | render.py:4171, 3905, 4210 | "We have \(X\)." with no reason and no align under it | 2477 | 5156 |
| 3 | paper_walk.py:6213 / 1742 / 1754 / 5911 / 5902 / 7946 | calc reason "algebra" (and "by algebra") | 1288 (+131) | 2457 (+170) |
| 4 | render.py:4008 | "which is immediate." / "which is immediate from (n)" | 915 / 392 | 1378 / 664 |
| 5 | render.py:361-365, 437, 452 (printed at 4179) | final closer "— by <tactic phrase>" read from the source text | 872 | 878 |
| 6 | render.py:4021 | "— by extensionality." with no premises | 777 | 1031 |
| 7 | render.py:3592 + 4181 | "Hence the claim." (part of rank 1) | 674 | 1098 |
| 8 | helpers.py:184, 189 → render.py:4023 | "— by direct computation" | 459 | 652 |
| 9 | render.py:4082 | "Hence X, the hypotheses being contradictory." | 249 | 376 |
| 10 | render.py:4029 | "— exhibited by an explicit element" | 217 | 247 |
| 11 | render.py:4111 | "Define a = v" with v a literal (31 rec) or an existing name (168 rec) | 199 | 259 |
| 12 | render.py:4025 | "— by induction" | 173 | 189 |
| 13 | render.py:4169 | "Applying the same function to both sides of (n) gives X." | 171 | 272 |
| 14 | render.py:4035 | "X, immediately." | 145 | 266 |
| 15 | render.py:3959 | "Hence X, by simplification." | 134 | 202 |
| 16 | render.py:4041 | "— by contradiction" (supposition not stated) | 107 | 137 |
| 17 | render.py:4027 | "— immediate from its type." | 104 | 180 |
| 18 | render.py:4031 / 3966 | "— by typeclass inference." / "; this is standard." | 66 / 2 | 73 / 4 |
| 19 | render.py:3921 | "Hence X, by definition." (no definition named) | 54 | 83 |
| 20 | definition cards (paper_lemmas.py:465) | card lhs = rhs | see below | |
| — | smaller | 3913, 1396/1398, 3964, 3957, 3947/3982, 3560/3568, 3909, 3927, 3968, 4000, 3348 | under 30 each | |

Rows marked "form" (not ranked): "In either case, the claim holds" (407 records), "Both parts are
shown; hence X" (724), "the first/second alternative holds" (171), "witnessed by" (62), "by
extensionality, using (n)" (179), the sorry sentence (29), and the lemma-keyed data readings
("by linearity of the integral", "by Lagrange's theorem", ...). "In either case" is a form when the
arms are printed and name their alternatives; D3 decides that, not this site.

**Definition cards.** On the 18 lineup modules, 10 of the 141 definitions in their tables print
lhs = rhs (a97J06 apm_a97J06_integrand; a99J04 f12, f21, f31, f32; b96J03 IsSolvable, IsNilpotent;
t96J05 E, I; t96J07 A). claude-16's count was 12 of 169 cards; my denominator is every definition
in the table, not the cards shown. Over all modules: see "Card count, all modules" at the end.
The lhs is `\leanref{<body>}{<name>}`: the paper printer unfolds the defined head to its own body
(latex_pp/paper/printer.py `_try_unfold_for_display`), so the card reads body = body.

## Three findings that change what the packet assumed
1. **The final closer's tactic phrase is not typed at all** (rank 5). `_proof_steps`
   (render.py:361-365) scans the Lean source for the last tactic line with a `_TACTIC_PHRASE`
   entry and puts that phrase on the theorem's final closer (render.py:437, 452; arms 396-403).
   It is the last automation line anywhere in the proof, not the step that closes the goal. Of the
   1,226 "— by <tactic>." on the pages, 598 are "by simplification".
2. **The typed head is thrown away for the species readings.** `_read_justification`
   (helpers.py:153-200) returns `("", [], species)` for ext, field, nonempty, instance,
   computation and the tactic namespaces: the head constant `c`, which says which ext lemma, which
   instance, which tactic certificate, is dropped at that point. Ranks 6, 8, 10, 17, 18 all start
   there, so one change (keep `c` on the Step) gives each of them its typed input.
3. **"algebra" has typed data behind it at every source, of two kinds.**
   - A lemma is on the row: rewrite-readings.json entries whose reading is "algebra" (108 lemma
     names), composition-normalization heads (paper_walk.py:5902), and the constructed simpa row
     (paper_walk.py:7946, both simp runs). Here "algebra" is a fallback: the lemma is known and
     not named.
   - A typed test decided it: `_arithmetic_wrapper_reason` (paper_walk.py:7376, 7380) prints
     "algebra" only when the row's endpoints differ by arithmetic or by a commutative-ring
     identity (Q470B/C). I marked that source "form".
   An empty reason prints an empty slot (or the Q504 definition-unfold reason), not "algebra".
   The page cannot split the 2,457 between the two kinds; the probe column gives the sampled
   records reaching each line.

## The five I would turn into forms first
1. **Bare "Hence X." / "We have X."** (ranks 1, 2, 7; about 4,000 records together, the largest
   group). Form: the sentence with the reason the closing term gives. Typed data: the term's head
   constant (`proof_head_name`, the have's value head) and its premise steps; for a library head,
   a reading keyed by that lemma (the claim-reasons / citation-readings data path) or a citation.
   Where neither exists the sentence becomes the gap "R2: no reason for this step". The sample
   probe puts most of rank 2 at render.py:4171 (restate, no premises) and 3905 (have whose chain
   was exhausted), not at Q139's 4210.
2. **"algebra"** (1,288 records). Form: "by <identity>" from the rewrite lemma, one reading per
   identity class (commutativity, associativity, distributivity, sign and subtraction rules,
   numeral evaluation), or the instantiated identity itself. Typed data: `row.reason.declaration`
   and the row's before/after exprs; the 108 "algebra" entries need a class each in the data file.
3. **"which is immediate"** (915 + 392 records). Form: one reading per trivial head (le_refl → "both
   sides being equal", zero_le → "since 0 ≤ n for every natural number n", Set.mem_univ → "every
   point lies in the whole space", ...). Typed data: the head constant (needs finding 2) and the
   statement it instantiates; about 15 heads in `_TRIVIAL_HEADS`.
4. **The final closer's tactic phrase** (872 records). Form: the tactic that produced the closing
   term, read from the term (the certificate's namespace, as `_TACTIC_NAMESPACES` does), with the
   facts it combines cited; otherwise a gap. Typed data: the closer's term head and its premises.
   This one is a wrong reason today, not only a missing one: the phrase can name a tactic that
   did not close this goal.
5. **"by extensionality" with no premises** (777 records). Form: "since f(x) = g(x) for every x" /
   "both sets have the same elements", stating the pointwise fact. Typed data: the ext lemma's
   argument, a lambda whose body is the per-point fact, rendered under its binder (and cited when
   it is itself a numbered step). Needs finding 2 for the lemma name (funext vs Set.ext vs
   LinearMap.ext choose the wording).

Next after these, smaller and cheap: "direct computation" (459, same input as 4), "exhibited by an
explicit element" (217, the element is Nonempty.intro's argument), "Define a = v" (199), and the
definition cards (render the lhs with the defined head kept opaque).

## What cannot be a form without Rob
- **The raw-name `\seqsplit` fallback** in `latex_pp/printer_core*.py`: a constant with no
  display rule. It does not appear on any census proof page at the head (0 of 8,778), but the walk
  still guards against it in conditions (render.py:3979) and the definition card lhs goes through
  the same const rule (term_walk.py:6146 comment). Making it a gap means the printer has to report
  "no display rule" instead of printing the name; that is Rob's code.
- **Definition cards:** probably not Rob's. The unfolding is in `latex_pp/paper/printer.py`
  (`_try_unfold_for_display`, which already keeps a local reading marked "opaque" as a name).
  If that file is ours, the card form needs no change in Rob's code; I did not check its owner.
- Everything else in the inventory is in `paper_proof_walk/` or `paper_walk.py`.

## Limits
- The page counts are regexes; shared phrases are not split by site on the page. The sample
  probe splits them by records only (a line running in a record), not by occurrence.
- The probe sample excludes records over 4 s in the census.
- `class` is my call per row: "fallback" means nothing typed stands behind the words as printed;
  "form" means the words come from typed data (a lemma-keyed reading, a named witness, printed
  parts).
