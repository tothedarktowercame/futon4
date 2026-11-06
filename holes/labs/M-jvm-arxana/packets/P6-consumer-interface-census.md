# P6 — adjacent consumer interface census

Mission: `M-jvm-arxana`. Phase: MAP. Intended worker: one Zai seat.

Discovery only. Do not redesign the REPL Circus, Ways of Working, Giraffe or
War Machine, and do not move their domain rules into Arxana.

## Question

What minimum query inputs and outputs do the adjacent missions demonstrably
need from a shared Arxana authority?

## Work

1. Read `futon3c/holes/missions/M-repl-circus.md`,
   `futon3c/holes/missions/M-ways-of-working.md`, the current Giraffe mission,
   and the War Machine selection/action contracts they cite.
2. For each consumer, identify no more than three concrete reads and three
   concrete writes that already appear in its mission, code or tests.
3. Express each as an interface observation: caller, target identity, required
   inputs, returned facts, temporal/freshness need, and evidence source.
4. Mark domain-specific fields that must remain namespaced outside the common
   waist. Identify any two consumers that use the same shape under different
   names, but do not standardize them yet.

## Output and acceptance

Write `holes/labs/M-jvm-arxana/P6-consumer-interface-census-report.md` with a
bounded matrix of reads/writes, source citations, overlaps, namespaced fields,
and a Q6 `ready / missing` table.

Acceptance: no proposed interface is supported only by the design conversation;
each has a mission, code or test witness. Commit only the report with message
`M-jvm-arxana MAP P6: ...`.

