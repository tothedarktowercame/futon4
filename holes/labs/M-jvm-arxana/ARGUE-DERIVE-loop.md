# M-jvm-arxana — progressive DERIVE ↔ ARGUE loop

Owners: Codex-21 (DERIVE), Zai-1 (ARGUE). Operator: Joe.

## Purpose

DERIVE and ARGUE proceed concurrently without becoming two disconnected
documents. DERIVE produces a design checkpoint; ARGUE tests the new commitment
against the mission and MAP evidence; DERIVE records the disposition of every
material challenge before proceeding past the next checkpoint.

This loop is progressive, not a repeated full rewrite. Each pass updates one
living argument and one obligation ledger.

## Cycle

1. **DERIVE checkpoint.** Codex-21 completes and commits one coherent design
   product. A checkpoint is D1, D2, D3, D4, D5, D6 or the D7 synthesis—not an
   arbitrary editing session.
2. **Dispatch.** Codex-21 bells Zai-1 with the exact DERIVE commit, the prior
   ARGUE commit, and the sections/obligations affected. The bell explicitly
   requests an incremental update to
   `A1-design-fitness-argument.md`, not a parallel design.
3. **ARGUE delta.** Zai-1 updates claims, objections, pattern/ruling references
   and the D1–D7 ledger. It marks each old obligation discharged, narrowed,
   still open or replaced, and adds only challenges caused or exposed by the
   new checkpoint.
4. **Bellback.** Zai-1 commits only the argument report and returns its SHA,
   checkpoint baseline, highest-value challenge and list of changed obligation
   IDs.
5. **DERIVE disposition.** Codex-21 records every material challenge as one of:
   `accepted-design-revision`, `accepted-later-obligation`, `answered`, or
   `rejected`, with an IF/HOWEVER/THEN/BECAUSE reason and a commit.
6. **Receipt.** Add one row to the checkpoint table below. A later checkpoint
   cannot silently erase an open obligation.

## Dispatch template

```text
M-jvm-arxana progressive ARGUE pass for <D-item>.
DERIVE baseline: <sha>. Prior ARGUE baseline: <sha>.
Read <D-artifact> and update only A1-design-fitness-argument.md.
Re-test claims <ids> and obligations <ids>; also report any newly exposed
defeater. Do not redesign or select architecture on Codex-21's behalf.
Commit only the report. Bell back with commit SHA, highest-value challenge,
and changed obligation IDs.
```

## Checkpoint table

| Checkpoint | DERIVE baseline | ARGUE result | Highest challenge | DERIVE disposition | State |
|---|---|---|---|---|---|
| Initial plan + D1 | `9c8b515` | `65c6eac` | O-D3a: no architecture yet enforces revision preconditions; O-D4a: draft8 is contradictory | `e4f3c4f`: accepted as D3/D7 criterion and D4 rejection tripwire | closed |
| D2 strategy evaluator | `9ba7974` | `6dcdb54` | O-D2c: promoted ambiguity lacked acknowledgement provenance | accepted: separate actor/policy/reason acceptance record; D2d answered; D3b passed forward | closed |
| D3 protocol/consistency | `bc81c03` | `98188fb` | fencing proof is a selection gate; client evaluation yields client-attested evidence | accepted: durable fencing and journal-order recovery explicit; D3c/D7b passed to D7, D5b to D5 | closed |
| D4 correspondence | pending | pending | — | — | active |
| D5 client/fidelity | pending | pending | — | — | next |
| D6 action boundary | pending | pending | — | — | queued |
| D7 synthesis | pending | pending | — | — | queued |

## Gates

- Zai-1 never edits DERIVE artifacts or selects the architecture.
- Codex-21 never silently edits away an ARGUE objection; it receives a typed
  disposition.
- ARGUE passes do not block work on the next D-item unless they expose a
  defeater of a shared premise or invariant.
- D7 cannot close with any `still open` defeater against the selected
  architecture.
- ARGUE lifecycle exit remains after D7 selection, when Zai-1 performs the
  final synthesis rather than another provisional pass.
