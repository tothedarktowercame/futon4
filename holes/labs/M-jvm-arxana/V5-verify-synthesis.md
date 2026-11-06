# V5 — VERIFY synthesis and receipt

Mission: `M-jvm-arxana`. Date: 2026-11-06.

## Verdict

**PASS.** The design has survived two independent consumer gauntlets, a
mechanical wiring audit and four targeted spikes.  No client requirement
contradicts D1–D7 and no unresolved semantic or authority choice remains.
Implementation evidence is explicitly carried into INSTANTIATE.

## Evidence receipt

| Check | Evidence | Result |
|---|---|---|
| Code-server client R1–R7 | `V1-codeserver-contract-report.md` | pass by spec; payload registration and transcripts instantiate |
| Proof-presentation client R1–R6 | `V2-proof-presentation-contract-report.md` | pass by spec; fixtures and evaluation records instantiate |
| Specification BOM | `V0-verification-bom.md` | all aspects specified or priced decline recorded |
| Wiring/authority closure | `V0-authority-wiring.edn` through `futon3c.diagramprover.wiring` | zero read-never-written, written-never-read, or multiply-written ports |
| Atomic expected-head | `V3-risk-spikes-report.md` S1 | one winner and one journal row |
| Fixture coherence | `V3-risk-spikes-report.md` S2 | defect detected; D4/D7 corrected; positive fixture gates INSTANTIATE |
| Proof query shape | `V4-client-risk-spikes-report.md` S3 | correct bounded results at observed corpus scale |
| Segment canonicalization | `V4-client-risk-spikes-report.md` S4 | ambiguity exposed and one authority-owned rule fixed |

## Completion-criteria pre-check

The seven IDENTIFY criteria are covered respectively by D1; D2/D3; D5; D4;
D6; D3/D7; and D6 plus the P6 consumer waist.  None can honestly be marked
complete before implementation: schema publication, JVM execution,
cross-editor equality, repaired bidirectional fixture, candidate compilation,
transaction routes and adjacent-consumer interfaces are INSTANTIATE
demonstrations.  VERIFY found no criterion absent from the design.

## Fidelity pre-check

D5 classifies sixteen donor capabilities as preserve, adapt or drop and gives
each preserved/adapted behaviour a tripwire.  V1 adds the code-server boundary
tests (UTF-16 conversion, preview labelling, zero pre-service writes); V2 adds
non-aliasing, disagreement, explicit absence, immutable assessment and layered
usefulness tests.  The tests are specified but service-backed execution is
properly an INSTANTIATE obligation.

## Verification-time revisions

1. The draft8 builder loses producer source paths and hashes same-name files
   under the wrong root.  D4/D7 now require exact producer-resolved paths; the
   repaired positive fixture is the first implementation gate.
2. A client segment revision names whole-artifact canonical bytes, never its
   JSON serialization or extracted segment alone.
3. The proof-profile query uses a derived status projection with client-owned
   ordering, preserving Arxana's policy boundary.

## Carry-forward to INSTANTIATE

- Repair and commit the coherent D4 positive fixture before client demos.
- Register versioned operation payloads and canonical response projections.
- Implement the authenticated conditional futon1b route, namespace refusal,
  idempotency journal and bypass/crash tests.
- Execute Emacs/code-server digest-equivalence transcripts.
- Run proof-profile fixtures for four manifestations, provenance disagreement,
  assessments, explicit absence, and usefulness independent of conformance.
- Parameterize and paginate the proof queries and test projection maintenance.

These are implementation and demonstration debts with fixed contracts, not
deferred architecture decisions.  A general theorem-prover model remains a
priced decline because the two consumer traces, wiring proof and empirical
spikes directly test the mission's actual boundaries.
