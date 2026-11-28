# The grammar a printed proof must satisfy (from 2026-11-27)

Joe, 2026-11-27: the classes should be kept as one coherent structure, so that a new class is
assimilated into it, and so that there is evidence a fix for one class does not undo another.
The work may stay open-ended; what must not happen is losing track of what each fix protects.

## The rules

`grammar.tsv` holds fourteen rules in six families (B4 added by Joe, 2026-11-28). A printed proof page must satisfy every rule.

| family | rules | what they protect |
|---|---|---|
| binding | B1–B4 | names: every name is introduced before use, nothing is introduced that is unused, a name keeps one meaning, and a definition introduces a new object |
| reference | R1–R2 | citations: every fact a step uses is cited, and the citation is the real justification |
| discourse | D1–D4 | the shape of the argument: goals before their proofs, each goal closed once, proof forms named as they are, calculations in order |
| locality | L1 | a sentence sits with its own step and under its own assumptions |
| vocabulary | V1 | mathematics, not Lean |
| truth | T1–T2 | what is printed means what Lean means, and is true as ordinary mathematics |

`class-rules.tsv` maps every class in `classes.tsv` to one rule, or to `-` (not a defect, or a
process problem such as speed). A class is a known way of breaking a rule; the rule is what is
being protected.

## Scope

The grammar defines acceptance (Joe, 2026-11-27): a page the grammar rejects still needs cleaning
up, and a page it accepts is done. In scope: the 8,778 APM records judged against these rules. The
fix goes where the violation comes from: the printer when the page misreads the proof, and the Lean
proof itself when the proof relies on something the grammar rejects and no faithful printing can
avoid it (for example a95A07 sums 1/n² over all n ∈ ℕ and drops n = 0 by 1/0² = 0; the page cannot
state that fact truthfully, so the proof is revised to sum over n ≥ 1). A change to a registered
problem STATEMENT is Joe's decision, never a packet's. Out of scope: explaining more than the
proof does, stylistic choices that break no rule, Rob's `latex_pp/printer_core*.py`. A finding that
breaks no rule goes to `backlog.tsv`; it does not reopen the work.

## Decisions the grammar makes

Each line is a choice the grammar settles explicitly, accept or reject, so that no page is accepted
by default. A new choice found later is added here before any fix.

| choice | decision | rule | date |
|---|---|---|---|
| a fact true only by a Lean junk value (x/0 = 0, 0⁻¹ = 0, log 0 = 0, √(neg) = 0, truncated ℕ subtraction where it truncates) | reject, glossed or not | T2 | 2026-11-27 |
| other totalised operations used at a point outside their domain: `deriv` of a non-differentiable function = 0, `∫` of a non-integrable function = 0, `∑'` of a non-summable family = 0, `limUnder` without a limit, `Real.arcsin`/`arccos` outside [−1, 1], ℕ/ℤ division read as exact division | reject. Counted on q303 by RC-T2 (claude-20, `~/notes/claude-20/rct2/totalised-q303.jsonl`): 13 declarations shown outside the domain (∫ 11, ∑' 1, ℕ/ℤ division 1; deriv, arcsin/arccos 0). Statement cases, Joe's call: a94J01 and a96A01 (integrability hypothesis missing). a01A10 is not one: its theorem `fresnel_integral` is stated as an improper limit; `fresnel_bochner_integral_eq_zero` and `not_fresnel_integral_statement` are side lemmas that no other declaration uses, saying the Bochner reading is 0 by the convention. They are proof-side: remove them from the proof (or the page). Proof-only: 8 (mostly "if not integrable then ∫ = 0 ≠ 1"). ℕ/ℤ `/` printed as `\frac` is a printer fix (⌊a/b⌋ unless divisibility is shown). Unmeasured sites (no domain witness in the typed data) are not counted as violations | T2 | 2026-11-27 |
| a sum, product or index set starting at 0 where the mathematics starts at 1 and the 0 term vanishes only by convention | reject | T2 | 2026-11-27 |
| "which is immediate" / "by simplification" as the reason for a step that uses local facts | reject: cite them (R6F) | R1 | 2026-11-26 |
| a decision procedure (omega, linarith, norm_num) as the reason for an arithmetic fact | accept, named as a computation, not as a proof by contradiction | D3 | 2026-11-26 |
| a value chosen by `Classical.choose` that the proof never uses | reject (t99A02 G_cayleyTree_aux_adjacency) | B2 | 2026-11-27 |
| a definition whose value is a literal or an object already named (a97J06 "Define γ = 2") | reject: a definition introduces a new object (Joe: "a definition introduces a new object seems like a good rule"); substitute the value, or gap B4 where the typed data cannot decide | B4 | 2026-11-28 |
| coercions (ℕ → ℝ, subtype → type) left implicit in a formula | accept when the meaning is unchanged; reject when a step reads as circular without them (m97A04 congrArg Fin.val) | T1 | 2026-11-27 |


## Admitting a new class

1. Name the rule it breaks. If it breaks none, it is a backlog item, not a class.
2. If it breaks a rule no existing statement covers, write the new rule into `grammar.tsv` first
   (and say so in the report to Joe): the grammar grows by a rule, not by an unattached class.
3. Add the class to `classes.tsv` and to `class-rules.tsv`. `rule_gate.py` refuses a census whose
   annotations carry a class with no rule.
4. Give it a census check where possible (an ink.py detector, preferably decided on typed data).

## Evidence that a fix does not undo another

At every candidate census, before acceptance:
- `python3 rule_gate.py <prev head annotations> <candidate annotations>`: every record that now
  breaks a rule it did not break before is a NEW line, and the candidate is held (exit 1).
- `citcount.py` and `citres.py` gate R1 and R2 (citations). Read every flag.
- The whole `paper_walk_test.py` on each branch tip. Each pin protects one earlier fix.
- **Changing a pin is a claim.** A commit that changes an expected page in a test names the rule the
  new text satisfies better. (2026-11-27: claude-16 updated q381 to a page that had lost the
  citation T₁ > 0, a broken R1, calling it an improvement; R1FC caught it a round later.)

Limits, stated so the gate is not read as more than it is: the census detectors are partial (the
`check` column of `grammar.tsv`); several are text patterns with false marks (B1: 10 of 16 marks
false at q303). A rule's count of zero means no detected violation, not none. A fixed random
reading sample, read against these rules, covers what the detectors cannot.

## The ledger: rigor per rule (from 2026-11-28)

Joe, 2026-11-28: "I don't think it is worth pushing until the grammar is solid, with all the checks
and all of the conformance sorted out. That way any bug reports would open a new aspect of the
grammar. We cleared the original \"ink\" but it didn't have the rigor of a grammar behind it."

Each rule stands at one level of a ladder; a level needs every level below it.

| level | name | what it takes |
|---|---|---|
| 0 | unchecked | the rule is stated in `grammar.tsv`; no typed check |
| 1 | checked | `rules/check_<RULE>.py` decides on typed data and runs at the census |
| 2 | validated | the check catches every constructed bad case in `rule-validation.tsv`, for its own reason (`match`), passes every good case, and a read sample of its flags is at least 13/15 real (`rule-precision.tsv`) |
| 3 | covered | the check decides at least 95% of records (provisional bar); a record with an "unmeasured" row is undecided, never clean |
| 4 | conformant | no violation, apart from records listed by name, with the reason, in `rule-exceptions.tsv` |

- `ledger.py HEAD_DIR` writes `HEAD_DIR/grammar-ledger.tsv` at every accepted head: per rule the
  level, the check, bad cases caught, precision read, records decided, records violating, named
  exceptions, and the old ink marks (text, informational; they never raise a level).
- `validate_rules.py HEAD_DIR` runs every case (from a snapshot of the head, with
  typed-flags-env.sh sourced) and writes `rule-validation-results.jsonl`. A bad case is a real
  record with the defect put back by one edit (`find` -> `replace`), or a record the head still gets
  wrong. A case whose `find` no longer occurs is STALE and fails until rewritten.
- Every defect found by reading, by Joe or anyone, becomes a bad case under its rule. If its rule's
  check misses it, the check is wrong; if no rule covers it, the grammar gains a rule or a sharper
  statement. Old ink classes become bad cases under their rules (`class-rules.tsv`).
- A candidate is accepted only if no rule's violations rise, no rule's coverage falls, and every
  case that passed at the previous head still passes.
- **Push gate (mfuton-share): all 14 rules at level 4.**

The first ledger (head 657a6fed): 5 rules at level 1 (B1, L1, R2, T1, T2), 8 at level 0, none
validated. The first B1 cases found a real miss on the first run: a name the printer invents for an
anonymous component (t96J04 "obtain f(w) = y") is not a binder of the term, so B1 never sees it.

## Generation: the grammar produces the page (from 2026-11-28)

Joe, 2026-11-28: "ultimately if it is a grammar it should work for generation, so it should stop
before emitting nonsense, because nonsense is not covered by the grammar." And: "generate as far as
we can without producing nonsense and then add validated rules that push generation forward."

Why the checks alone did not converge: they read only the census record (a theorem's statement and
proof, not the definition cards, lemma cards or page frame), and they were measured for precision
(are the flags real), never for recall (what share of the defects a reader finds they flag). Every
check is written against the shapes already found, so a new shape (a definition card printed as
A = A, 12 of 169 cards on the lineup) passes until someone reads the page.

The approach from here:

- **Forms.** The page is built from typed sentence forms: "Definition: f(x) = body", "Define a = v",
  "Suppose P ... contradiction", "We claim X: ... closed", a calculation row, a cited step. Each form
  is a constructor with typed preconditions taken from the 14 rules (the typed tests the checks
  already make). A form that cannot be built is not printed.
- **Gaps, not fallbacks.** Where the walk now prints something with no form behind it ("algebra",
  "by simplification", "Hence the claim", "Applying the same function to both sides", a raw name in
  `\seqsplit`, a head-glued-to-body definition card), it prints a marked gap naming the rule
  instead. The page is generated as far as it can be and shows where it stops.
  `fallback-map-c8076d84.txt` is the first map: the reason and closer phrases on the head's pages,
  by count (occurrences, records). Most are emitted in `paper_proof_walk/render.py`, which is ours;
  the raw-name fallback is in Rob's `printer_core` and is treated as a gap on our side.
- **Admitting a form.** A form is admitted when its bad and good cases pass (level 2 above, applied
  to the form). Work goes from the gaps, by count: choose a gap, write the form that covers it,
  admit it, and that stretch of every page fills in. Progress is one-way.
- **Known defects are the acceptance suite.** Every bad case in `rule-validation.tsv`, every flag
  read real, the old ink classes and every defect Joe reports is a test with three states:
  failing (the defect is still printed), gapped (the step is now a gap) and passing (an admitted
  form prints it correctly). Done means every test passing.
- **Credence and completeness, per node of the proof tree** (Joe, 2026-11-28: "a high level
  outline of the proof should be possible to generate now with high credence, but completeness is
  still low in many cases. as we fill in details, both credence and completeness get pushed down
  the tree until we are done"). The walk's Step tree is the tree. A node's *completeness* is the
  share of its subtree printed by admitted forms rather than gaps (computed). A node's *credence*
  is its form's measured record: cases passed, the share of its outputs read real, lowered by every
  reader-reported defect in that form's output; never a model's self-rating. An outline (statement,
  case split, induction, the claims and what each shows) comes from the term's structure and can be
  high-credence now; details fill in below it. The page always shows the outline, with each node's
  two numbers, and gaps beneath. Done: completeness 1 at every root, credence at the bar everywhere.
- **The checks stay** as the detectors that decide each test's state, and as the oracle for a new
  form; they are no longer the gate.

The end result, if it works: a semantic grammar for written mathematics at the level of these
proofs, whose forms are justified by the Lean terms they are built from.

## Done

Every rule at ledger level 4, and a random sample of 30 pages read against the rules finds no
violation. Anything else found then is a new bad case, and so a new aspect of the grammar.
