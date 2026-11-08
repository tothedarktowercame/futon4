# DERIVE-F5 — unit-circle geometry markup

Recorded by claude-13, 2026-11-08, from codex-21's answers (round 6, job
invoke-1794099506477-15427-78c7d609). Core contract: DERIVE-core.md. Lean substrate: v4.29.

## F5 as written (mission §1)

> Represent S¹, multiplication, conjugation/inversion, and the square map
> z ↦ z². Query the fibre over -1 as {i, -i}. Render the same records in at
> least two visibly different charts/projections. A semantic change
> propagates to both; a coordinate-only change remains local. The graph, not
> either drawing, is the authority for objects, maps, fibres, and invariants.

## Clarifications

1. **Lean**: the fibre lemma lives in a new small Lean 4.29 project under
   futon5, e.g. `futon5/holes/labs/M-formal-arxana/f5-lean/`, not in
   apm-lean. Mathlib supplies `Circle` and `Circle.coe_inv_eq_conj`
   (MAP-Q9); MAP-Q9's probe proof is the starting point.
2. **Two projections**: the Cartesian-plane circle, and an angular
   parameterisation/strip showing squaring as angle doubling. `[0, 2π)` is
   not a global continuous chart (it has a cut), so it is called an angular
   projection. One deterministic HTML/SVG renderer with two panels, opened
   by futon4a. Marimo may be an exploratory companion, not the acceptance
   surface.
3. **Semantic change**: replace z ↦ z² with z ↦ z³; the fibre over -1
   becomes three points; both projections update from the graph records.
   The acceptance run checks a Lean 4.29 cube-fibre lemma, or preferably a
   small general power-map lemma if that is no harder.
4. **Records**:
   - entities: ℂ, S¹, points, fibres, symmetry groups, invariant
     propositions;
   - 'clusions: square, cube, conjugation/inversion, rotations, chart
     projections;
   - versioned 'clusion types: `geometry/map`, `geometry/symmetry`,
     `geometry/projection`;
   - invariants linked by `preserves` / `satisfies` 'clusions;
   - a materialised fibre: derived entity linked to map, value, theory
     version and members (DERIVE-core §9).

## Acceptance cases

- query the fibre of the square map over -1 → {i, -i}, from records, with
  the Lean lemma as kernel evidence;
- both panels render the same record ids;
- moving a point's drawn position in one panel changes only that panel;
- replacing the map with the cube map changes the fibre to three points in
  both panels, with the Lean check re-run.

## Depends on

The core's types and fibre rule; a new futon5 Lean project; an HTML/SVG
projector; futon4a able to open it (EWW baseline, as F6).
