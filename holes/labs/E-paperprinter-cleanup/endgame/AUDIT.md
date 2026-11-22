# PaperPrinter as a proof-quality audit

PaperPrinter checks a different boundary from kernel correctness and the
earlier APM source audit: whether a reader-visible proof preserves the
dependencies, scopes and explanatory roles that make a correct proof
intelligible.  A finding must be routed before it becomes a cleanup packet.

## Finding routes

1. **Presentation residue.**  Normalization makes an administrative premise
   reflexive or vacuous, while the surviving proof step remains sound.
   Example: t01A01's instantiated `ite_cond_eq_true` reason becomes
   `infinity = infinity` after permitted closed-numeral branch selection.
   Route: suppress or replace the reason in PaperPrinter; do not rewrite the
   mathematical proof merely to satisfy the renderer.

2. **Dependency or scope loss.**  A locally valid derived claim is detached
   from the assumption or subproof that licenses it.  Example: t96J07 derives
   `1 = 0` under `h : v = 0` while proving the nonzero witness, but displays it
   twice at depth zero without `h` or the contradiction-closing role.
   Route: repair typed scope/ownership in PaperPrinter; separately decide
   whether the source proof would benefit from a named contradiction block.

3. **Source-proof opportunity.**  The renderer faithfully exposes a proof
   that is correct but needlessly indirect, duplicated, or reliant on an
   opaque administrative detour.  Route: propose a Lean-source cleanup only
   when the typed dependencies show the awkwardness is in the source rather
   than information erased by printing.  Re-render before calling it closed.

4. **Mathematical defect.**  The typed claim or dependency is itself wrong or
   cannot be justified from the recorded context.  Route: stop printer work
   on that site and return it to the proof audit.  No current C23 evidence
   establishes this route: Lean's local contradiction proof is valid.

## Evidence required

Each audit finding records: the typed proposition; live local dependencies;
the enclosing proof role or nearest typed substitute; the exact loss or
normalization performed by the printer; corpus extent; and the narrowest
change.  Text matches such as `We have 1 = 0` are candidates, not instances:
Q192's static scan finds five other textual hits, but only t96J07 is presently
confirmed as C23.

This audit taxonomy is additive to the ink and class metrics.  Ink estimates
expository surface area; it does not decide whether the repair belongs in the
printer, the Lean source, or the mathematical proof.
