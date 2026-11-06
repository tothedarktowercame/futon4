# A1 — JVM Arxana design-fitness argument

Mission: `M-jvm-arxana`. Phase responsibility: ARGUE. Intended owner: Zai-1.

ARGUE runs as a shadow of DERIVE by explicit operator decision. Codex-21 owns
DERIVE; Zai-1 owns construction and criticism of the argument for that design.
The argument may develop concurrently, but ARGUE cannot exit until DERIVE has
selected and specified an architecture.

This is not an argumentation-theoretic close reading of one operator passage.
It is an argumentation-theoretic workup of JVM Arxana: why the evolving design
is fit for the mission identified by Joe and for the facts established in MAP.

## Inputs

Read completely:

- `holes/missions/M-jvm-arxana.md`, especially the Gap, Candidate invariant,
  MAP answers and current DERIVE work plan;
- all six reports in `holes/labs/M-jvm-arxana/P*-report.md`;
- the ARGUE requirements in `holes/mission-lifecycle.md`;
- relevant patterns in `futon3/library/` and relevant numbered war-room
  rulings cited or discoverable from the mission's actual design questions.

Treat the DERIVE section as a moving input. Distinguish claims supported by
MAP evidence, commitments merely proposed by the current DERIVE plan, and
claims that cannot be settled until an architecture is selected.

## Work

1. State the principal fitness claim: what JVM Arxana must be fit for. Derive
   subordinate claims for shared authority, portable resolution,
   manifestation correspondence, consistency, thin clients and the
   inference-ready action boundary.
2. Build an explicit claim/support/objection structure. Every empirical
   premise must cite a MAP report location; every design premise must cite a
   DERIVE work product or decision still owed.
3. For each architecture alternative, identify its strongest supporting case,
   strongest objection, and the observation or decision that would answer the
   objection. Do not select the architecture on Codex-21's behalf.
4. Cross-reference applicable `futon3/library/` patterns by saying where and
   how each bears on the design. Search rather than decorating the argument
   with familiar names. Identify any pattern that requires a DERIVE revision.
5. Find and cite the relevant numbered war-room ruling as `@why ...`, or state
   with evidence that no ruling applies.
6. Maintain a design-obligation ledger keyed to D1–D7. An unsupported claim
   becomes an obligation or defeater; do not smooth it into persuasive prose.
7. Write the lifecycle-required trade-off summary, generalization notes and a
   jargon-free 3–5 sentence argument, all explicitly provisional where they
   depend on unfinished DERIVE choices.

Use argumentation machinery proportionate to the job. A compact claim graph or
table is preferable to a survey of argumentation theory. Preserve Joe's
motivating thesis—that digitally mediated discourse can become checkable
argument and computational action—as context, but argue for the design rather
than anatomizing the wording of that thesis.

## Coordination contract

- Zai-1 owns `holes/labs/M-jvm-arxana/A1-design-fitness-argument.md`.
- Codex-21 owns the mission DERIVE section and any DERIVE artifacts.
- Do not edit `holes/missions/M-jvm-arxana.md` during this first pass.
- Record the DERIVE commit used as the argument baseline.
- Mark architecture-dependent conclusions `provisional` and list the exact
  D-item or decision needed to settle each one.
- When the first pass is committed, report the commit and the highest-value
  DERIVE challenge to Codex-21. Later checkpoints may revise the same report.

## First-pass acceptance

Write and commit `holes/labs/M-jvm-arxana/A1-design-fitness-argument.md`.
It is accepted as an ARGUE scaffold when it contains:

- a traceable principal claim and subordinate claim structure;
- MAP-supported premises and explicit counterarguments;
- a comparison of all three architecture alternatives without selecting one;
- a D1–D7 obligation ledger;
- initial pattern and war-room cross-references;
- provisional trade-off, generalization and plain-language sections; and
- no claim that ARGUE or the mission is complete.

Commit only the report, with message `M-jvm-arxana ARGUE A1: ...`.
