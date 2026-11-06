# V0 — VERIFY specification BOM and structural closure

Mission: `M-jvm-arxana`. Phase: VERIFY. Coordinator-owned.

## Specification bill of materials

| Aspect | Characteristic defect | Formalism/check | Level and disposition |
|---|---|---|---|
| Fitness claims | confounded evidence or claim stronger than witness | A2 claim/evidence/objection table plus V1/V2 independent consumer traces | scenario/argument level; executable use-case claims deferred to their named spikes |
| Records and artifacts | malformed identity, revision or provenance | D1–D6 typed examples, result algebras and tripwire schemas | schema/example level now; runtime validators belong to INSTANTIATE |
| Process/wiring | orphan wire, double writer, unowned decision | `V0-authority-wiring.edn` checked by `futon3c.diagramprover.wiring` | mechanical authored-map level; zero orphan/multiple-writer findings required |
| Runtime loop | stale overwrite, duplicate application, lost event | D3 state machine plus atomic-write spike | empirical spike required before VERIFY exit |
| Correspondence loop | mixed revisions or unverifiable mapping | D4 negative rejection and positive-rebuild feasibility spike | empirical spike required before VERIFY exit |
| Client equivalence | same-looking UI with different semantic answer | D5 semantic digest plus V1 code-server trace | contract trace now; real authority transcript belongs to INSTANTIATE |
| Domain usefulness | presentation success confused with authority correctness | V2 layered acceptance matrix | consumer evidence only; explicitly excluded from JVM conformance |
| Decisions | architectural choice without examined alternatives | D1–D7 IF/HOWEVER/THEN/BECAUSE records and progressive ARGUE receipts | complete at design level |
| Full formal proof | high cost with no native verifier for the whole mixed-language service | priced decline | revisit only if a client trace or spike exposes an invariant not expressible by wiring, schema or transaction tests |
| Source conformance | authored wiring drifts from future implementation | priced temporary hole | unavailable before components exist; mandatory conformance check in INSTANTIATE when files/namespaces are named |

No row is “not blocking”: the two empirical rows are VERIFY gates, and the two
priced declines carry explicit revisit triggers.

## Structural ownership interpretation

The wiring map represents authority-bearing records, not UI calls. External
sources (clients, ArtifactProvider, interpreter, policy) are boxes so every
input has an accountable producer. `headless-arxana` is the sole writer of
semantic results and candidate actions; `futon1b` is the sole writer of durable
outcomes, journal events and receipts. Policy alone writes admission; executor
alone turns it into a guarded command.

The map deliberately has two command wires: Arxana may issue internal
conditional persistence commands, while an executor may issue admitted guarded
actions. Both terminate at the same futon1b atomic boundary; neither writes
durable outcome directly.

## Mechanical check

Required findings:

```edn
{:written-never-read []
 :read-never-written []
 :multiply-written []}
```

The check is detect-level over an authored declaration. It proves closure and
single declared ownership, not implementation conformance. That limitation is
priced in the BOM and becomes an INSTANTIATE gate when source sites exist.

Checked 2026-11-06 from `/home/joe/code/futon3c` with
`futon3c.diagramprover.wiring` over the committed EDN declaration. Actual
result:

```edn
{:written-never-read []
 :read-never-written []
 :multiply-written []}
```

## Structural questions retained for synthesis

- V1 must confirm code-server does not introduce a second evaluator or store
  writer.
- V2 must confirm proof assessments/usefulness records are client/domain writes
  that cannot redefine Arxana results.
- The atomic spike must demonstrate the selected XTDB version can enforce one
  expected-head winner transactionally. Reserved-namespace route and bypass
  enforcement require implementation source and remain the BOM's mandatory
  INSTANTIATE source-conformance check.
- The fixture spike must demonstrate ArtifactProvider/correspondence inputs name
  one coherent revision set.
