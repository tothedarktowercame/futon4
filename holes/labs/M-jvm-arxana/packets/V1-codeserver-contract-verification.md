# V1 — code-server consumer contract verification

Mission: `M-jvm-arxana`. Phase: VERIFY. Intended verifier: Zai-1.

Independently verify the finalized D1–D7 design from the code-server / first
VS Code-class client perspective. This is verification, not another design
pass and not implementation of the demo.

## Inputs

- `holes/missions/M-jvm-arxana.md`
- D1–D7 and `A2-final-design-fitness-argument.md` in this lab
- futon6 `holes/T-codeserver-arxana-demo.md` at/after `68fd4a7`
- the T-arxana-ts materials cited by that ticket

## Work

1. Trace R1–R7 through exact D-item fields, operations, authorities and tests.
2. For each trace, mark `verified-by-spec`, `requires-spike`, `requires-
   instantiate`, or `contradiction` and cite the source.
3. Exercise representative payloads on paper: frozen AnnotationSet ingest,
   exact/located/unanchored/hash-mismatch resolution mapping, UTF-16 conversion,
   preview versus promotion, semantic digest comparison and zero-write mode.
4. Identify every place the demo could accidentally claim authority it lacks.
5. Check code-server adds no new authority question compared with desktop VS
   Code and that co-location with repository/terminal changes no ownership.

## Output

Write `holes/labs/M-jvm-arxana/V1-codeserver-contract-report.md` with the trace
matrix, example payload/results, failed/conditional checks and a verdict.

Acceptance: every R1–R7 row has a cited disposition; frozen JSON digest is not
misreported as a D5 semantic digest; no direct futon1 write is endorsed; any
design contradiction is stated as a VERIFY stop-the-line finding. Commit only
the report with message `M-jvm-arxana VERIFY V1: ...`.
