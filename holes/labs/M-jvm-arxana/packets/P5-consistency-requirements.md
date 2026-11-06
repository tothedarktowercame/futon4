# P5 — consistency and CRDT requirement audit

Mission: `M-jvm-arxana`. Phase: MAP. Intended worker: one Zai seat.

Discovery only. This packet must not choose a CRDT, compare libraries, or build
a collaboration spike. It asks what the system demonstrably requires today.

## Question

What concurrent editing occurs, what failures have been observed, and which
consistency property—if any—cannot be supplied by revision-checked
transactions and subscriptions?

## Work

1. Inventory current writers to an Arxana-backed or WYSIWYG source artifact:
   Emacs, filesystem/git/agents, browser edits, rebuild scripts and any other
   live writer found in code.
2. Trace current freshness, stale-source, conflict, undo and refusal handling
   in the WYSIWYG and Arxana paths.
3. Search mission records, tests and incidents for actual lost updates,
   divergent replicas, concurrent offline edits, cursor co-presence or merge
   expectations. Separate observed incidents from hypothetical use cases.
4. Produce a requirement matrix for single writer, sequential multi-client,
   simultaneous online writers and offline writers. Mark each required,
   optional, unsupported or unobserved, with evidence.
5. State the smallest future observation that would falsify the sufficiency of
   revision-checked transactions. Do not recommend an implementation.

## Output and acceptance

Write `holes/labs/M-jvm-arxana/P5-consistency-requirements-report.md` with the
writer inventory, incident citations, requirement matrix, current safeguards,
unknowns and a Q5 `ready / missing` table.

Acceptance: every concurrency claim is labelled observed or hypothetical. A
conclusion of “no present CRDT requirement found” is valid if evidenced.
Commit only the report with message `M-jvm-arxana MAP P5: ...`.

