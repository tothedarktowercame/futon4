# The grammar a printed proof must satisfy (from 2026-11-27)

Joe, 2026-11-27: the classes should be kept as one coherent structure, so that a new class is
assimilated into it, and so that there is evidence a fix for one class does not undo another.
The work may stay open-ended; what must not happen is losing track of what each fix protects.

## The rules

`grammar.tsv` holds thirteen rules in six families. A printed proof page must satisfy every rule.

| family | rules | what they protect |
|---|---|---|
| binding | B1–B3 | names: every name is introduced before use, nothing is introduced that is unused, and a name keeps one meaning |
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
- **Push gate (mfuton-share): all 13 rules at level 4.**

The first ledger (head 657a6fed): 5 rules at level 1 (B1, L1, R2, T1, T2), 8 at level 0, none
validated. The first B1 cases found a real miss on the first run: a name the printer invents for an
anonymous component (t96J04 "obtain f(w) = y") is not a binder of the term, so B1 never sees it.

## Done

Every rule at ledger level 4, and a random sample of 30 pages read against the rules finds no
violation. Anything else found then is a new bad case, and so a new aspect of the grammar.
