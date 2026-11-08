# DERIVE-F1 — intent-marked conversation

claude-13, 2026-11-08. Second draft; the first widened F1 beyond the
mission and was reverted (futon4 75f08b3, bf6fded). This one follows the
mission's F1 paragraph and completion criteria only, with the author's
clarifications (codex-21, 2026-11-08, job
invoke-1794098667698-15408-0169217d). Build-plan details from codex-21's
second round (job invoke-1794099069915-15414-903e7789) are folded in when
they land; until then §4–§6 are marked pending.

## 1. F1 as written (mission §1)

> Use real proforma turns containing `㊯`, `㊢`, and at least one ambiguous or
> incorrect mark. Materialize the source turn, interpretation 'clusion,
> target, evidence, and standing. Project the same records as: marked linear
> conversation; an outline or graph view; and a bounded GCC candidate-action
> view. Editing prose preserves the act identity where warranted; changing a
> mark proposes a new interpretation rather than rewriting history;
> ambiguity remains visible.

Completion criteria that F1 owns: F1 passes end to end on retained real
fixtures with the same record identities in all three projections; editing
an F1 manifestation exercises relocation and interpretation history instead
of refusing because bytes changed.

Operator constraint: demoable in Emacs through futon4a's JVM Arxana client.

## 2. Author's clarifications (codex-21, round 1)

1. **GCC view** = the existing action-bearing fragments and their targets,
   projected into a bounded, explicitly non-executable view. No D6 candidate
   compilation in F1 (that is F4).
2. **Storage** = extend the reserved JVM Arxana / futon1b arxana route with
   the minimal typed operations F1 needs. No generic non-`arxana/*`
   workaround. In-memory is acceptable for a spike, not for acceptance.
3. **Edited artifact** = a successor manifestation (an exported conversation
   document, edited in futon4a). The settled turn stays immutable; readings
   relocate onto the new revision through F0. Preserved act identity is an
   evidenced interpretation, never a mutation.
4. **Ambiguous vs incorrect** = two competing 'clusions show ambiguity.
   "Incorrect" needs an attributable assessment (who rejected which
   interpretation, under what policy/reason), with nothing deleted.
5. **Standing** is three separate things:
   - resolution state: `unique | ambiguous | missing | stale`;
   - interpretation assessment: `candidate | accepted | rejected`;
   - evidence qualification: `exact | parser-attested | model-attested |
     human-attested`.
   "Verified" must not mean both "the mark parsed" and "the reading is right".

## 3. What already exists (MAP)

- Source turns, readings, marks and alignments are produced live on every
  REPL turn (MAP-Q2): chat-turn evidence → turn record → 小象 draft → 象
  reading → settled `e-xiang-turn-*` evidence; 馬 alignment maps reply marks
  to acts.
- F0 resolver relocates scopes across content-addressed revisions (futon3c
  `a483b2e0f`).
- futon1b arxana conditional route: CAS head + idempotent message id, tested
  two-writer race (MAP-Q7); accepts only `:revision/ingest` (MAP-Q5).
- futon4a renders resolve/correspondence results with typed refusals
  (MAP-Q4/Q6); writes nothing.

## 4. Records — pending codex-21 round 2

## 5. Operations — pending codex-21 round 2

## 6. Increments and acceptance — pending codex-21 round 2
