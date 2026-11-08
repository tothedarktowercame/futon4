# DERIVE-F6 — argument layout

Recorded by claude-13, 2026-11-08, from codex-21's answers (round 6, job
invoke-1794099506477-15427-78c7d609). Core contract: DERIVE-core.md.

## F6 as written (mission §1)

> Project a proof or argument into a Tufte-style page whose main claims,
> definitions, diagrams, sidenotes, and support relations are graph contents.
> Move a diagram in the layout without changing its content identity;
> separately change what it supports and require an attributable semantic
> relation edit.

## Clarifications

1. **Argument**: the F5 unit-circle mini-argument (it already has a diagram,
   a Lean proof, a map, a fibre and explanatory claims). a93J02 stays F3's
   fixture; cui bono can be a later generalisation.
2. **Renderer**: deterministic HTML/CSS with inline SVG. futon4a opens it in
   an Emacs HTML view (EWW as the baseline), with a companion outline/graph
   buffer addressing the same ids. The LaTeX Tufte class and browser-only
   xwidgets are not acceptance dependencies.
3. **Relations**: a `supports` 'clusion type (roles `supported-claim`,
   `supporting-content`, plus method and evidence fields); `qualifies` and
   `contradicts` as siblings if the fixture needs them. Diagram placement,
   margin vs main-flow position and pixel coordinates are projection data;
   changing the supported claim creates a new semantic 'clusion.

## Acceptance cases

- the page's claims, definitions, diagram, sidenotes and supports are graph
  records, each traceable from the rendering;
- moving the diagram (e.g. main flow → margin) keeps its id and changes only
  projection data;
- changing what the diagram supports requires a new `supports` 'clusion,
  attributable, with the old one kept.

## Depends on

F5 records and renderer; the core's projection-rule records (DERIVE-core
§4).
