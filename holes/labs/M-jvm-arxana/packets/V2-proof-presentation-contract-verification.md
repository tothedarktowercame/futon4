# V2 — proof-presentation consumer contract verification

Mission: `M-jvm-arxana`. Phase: VERIFY. Intended verifier: codex-22.

Independently verify the finalized D1–D7 design from the
futonzero-prelim-practice / proof-presentation perspective. Do not introduce an
APM-specific alignment mechanism and do not redesign JVM Arxana.

## Inputs

- `holes/missions/M-jvm-arxana.md`
- D1–D7 and `A2-final-design-fitness-argument.md` in this lab
- the proof-presentation client profile in D7
- the mark8, starmap, tutor-lesson and PythonEmit→LaTeXML-Oxide evidence used in
  the earlier client review

## Work

1. Trace R1–R6 from the client review through exact entities, manifestations,
   assertions, assessments, queries, authorities and acceptance layers.
2. Walk one semantic proof move through authored prose, Lean, generated formal
   rendering and generated informalization, allowing missing and ambiguous
   manifestations.
3. Verify authored/generated provenance cannot alias; mined/model/human evidence
   can disagree; explicit absence is attributable; accepted/rejected assessment
   does not mutate the assertion.
4. Separate transport, rendering integrity, correspondence correctness and
   usefulness evidence. Identify any place pedagogical success leaks into JVM
   conformance.
5. Mark each check `verified-by-spec`, `requires-spike`, `requires-instantiate`,
   or `contradiction`, with citations.

## Output

Write `holes/labs/M-jvm-arxana/V2-proof-presentation-contract-report.md` with
the trace, worked move, acceptance-layer matrix, failed/conditional checks and
a verdict.

Acceptance: no evidence source becomes authority by score alone; no total
alignment is forced; client usefulness stays attachable but non-semantic; any
design contradiction is a VERIFY stop-the-line finding. Commit only the report
with message `M-jvm-arxana VERIFY V2: ...`.
