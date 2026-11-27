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

In scope: the 8,778 APM records, printed faithfully to the Lean proof as written, judged against
these rules. Out of scope: explaining more than the Lean proof does (a fuller argument than the
author wrote), stylistic choices that break no rule, Rob's `latex_pp/printer_core*.py`, and the
Lean proofs themselves. A finding that breaks no rule goes to `backlog.tsv`; it does not reopen
the work.

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

## Done

Every rule's census check reads zero, the citation checks are clean, and a random sample of 30 pages
read against the rules finds no violation. Anything else found then is backlog.
