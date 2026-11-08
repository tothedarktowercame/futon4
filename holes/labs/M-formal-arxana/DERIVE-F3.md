# DERIVE-F3 — cross-manifestation mathematical meaning

Recorded by claude-13, 2026-11-08, from codex-21's answers (round 5, job
invoke-1794099415833-15425-816686ea). Core contract: DERIVE-core.md. Lean
substrate: v4.29 (Joe, 2026-11-07).

## F3 as written (mission §1)

> Use a small Mark8-informed APM proof with authored informal prose,
> readable Lean, generated formal rendering, and generated informalisation.
> Demonstrate verified relocation, partial formalisation, ambiguous
> alignment, and an explicit contradiction between authored and generated
> meaning. No artifact is merged into another; competing assertions and
> assessments remain attributable.

## Clarifications

1. **Fixture**: APM `a93J02` (MAP-Q8): authored informal source, Lean
   (115 lines, no sorry), authored TeX, LaTeXML HTML, and the pinned mark8
   judged alignment
   (`~/code/storage/apm/mining/alignment/r2-judged/a93J02.judged.json`).
   "Mark8-informed" = those judged links enter as candidate,
   `evidence/basis model` evidence, never authority.
2. **Generated informalisation**: generate one from the Lean. Record the
   exact Lean revision, model identity/version, prompt/configuration and
   output revision. Keep it distinct from the authored prose. Generated
   formal rendering plus mark8 alignment alone does not meet F3.
3. **Contradiction**: the DCT (informal) vs Abel's limit theorem (Lean)
   difference is a methodological divergence / partial proof-step
   correspondence, not a statement-level contradiction, and is kept as
   such. For the explicit contradiction, add a **clearly labelled synthetic
   negative fixture**: a controlled mutation of the generated
   informalisation that contains a statement-level error. The genuine
   example is not relabelled and no evidence is invented silently.
4. **Lean locators**: artifact revision + fully qualified declaration name
   is the stable locator. Source/codepoint or syntax ranges are
   revision-specific evidence. Tactic-step ranges are optional evidence,
   never identity.
5. **Mark8 verdicts** become `evidence/basis model` assessments of
   correspondence 'clusions, keeping judge/model version, reasons,
   candidate set and verdict. They are not human acceptance.
6. **Lean gate**: re-run Lean 4.29 for F3 acceptance against the pinned
   fixture bytes, as a focused integration check recording toolchain,
   worktree commit, command, output and digest. Recorded results are
   evidence/cache, not a substitute, and the Lean run stays out of the fast
   unit tests.

## Operator judgement (Joe, 2026-11-08, turn `turn-YoF9El`, evidence `emacs-8fdea78f5fab37bcf3fc65aa4aded28e`)

Joe agreed with the mark8 judge's readings for all nine a93J02 steps
(EXERCISE-F3-mark8-a93J02.md), and on the DCT/Abel difference:
"divergence is expected": the formalisation work never asked the formal
proof to follow the informal one. So:

- methodological divergence is a normal, expected correspondence outcome
  between authored informal and Lean proofs, not a defect to be resolved;
- the judge-endorsed links become human-basis accepted assessments; mark8
  links the judge overruled are rejected by both;
- mark8's over-linking ("reaching to find some connections that weren't
  there") is feedback for mark8, a separate issue outside this mission.

## Acceptance cases

- verified relocation: an authored-prose edit relocates its correspondence
  scopes (F0) without changing the Lean;
- partial formalisation: mark8 steps with no credible Lean match recorded
  with `evidence/coverage partial`;
- ambiguous alignment: a step with several candidate declarations kept as
  resolution `ambiguous`;
- methodological divergence: DCT vs Abel kept as its own typed
  correspondence, not a contradiction;
- explicit contradiction: the labelled synthetic mutation, with
  `evidence/standing contradicted`;
- no merging: every manifestation stays its own artifact; all assertions and
  assessments attributable.

## Depends on

The core's 'clusion types (correspondence as a 'clusion subtype, DERIVE-core
§7); a generated-informalisation run (new); a Lean 4.29 integration check in
the apm-lean worktree (new).
