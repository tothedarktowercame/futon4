# P1 — live Arxana/XTDB schema census

Mission: `M-jvm-arxana`. Phase: MAP. Intended worker: one Zai seat.

Discovery only: read code and documentation, make bounded read-only API/XTQL
probes if the live service is available, and write one report. Do not change
code, schemas, stores, mission files or running services.

## Question

Which entity, hyperedge, provenance, temporal and salience shapes are canonical
on the live XTDB path, and which exist only as Emacs-side conventions?

## Work

1. Read `futon4/AGENTS.md`, storage-facing files under `futon4/dev/`, and the
   active futon1/futon1b XTDB and hyperedge routes they call.
2. Trace one article/entity, one typed hyperedge and one plexus or working-set
   record from writer to stored representation to reader. Cite file and line.
3. Record identity keys, required/optional attributes, endpoint shape,
   provenance, timestamps/valid-time, salience fields and query routes.
4. If a live read-only endpoint is available, capture one redacted example of
   each shape. Do not POST, transact or repair data.
5. Separate `canonical in storage`, `enforced by validator`, `convention in a
   client`, and `claimed in prose`.

## Output and acceptance

Write `holes/labs/M-jvm-arxana/P1-live-xtdb-schema-report.md` containing:

- a source-cited shape table;
- the three end-to-end traces;
- discrepancies between prose, code and live records;
- a `ready / missing` table limited to Q1;
- exact probe commands and summarized outputs.

Acceptance: every assertion names a file:line, live probe, or says explicitly
that it is unverified. Commit only the report with a message beginning
`M-jvm-arxana MAP P1:`. Do not edit the packet or mission.
