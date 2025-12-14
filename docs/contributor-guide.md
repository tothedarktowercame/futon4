# Arxana Contributor Guide

## Phase-aware workflow

Prototype 1 keeps the fast-moving enhancements inside `dev/` while the
Org sources (`arxana/*.org`) remain the long-term canonical docs.  Until
Milestone 2 completes, contributors should:

1. Load the harness (`M-x arxana-build` or `./dev/run-tests.sh`) so the
tangled history and the dev modules share the same Emacs session.
2. Make iterative changes inside `dev/*.el` or `test/*.el`.  Every helper
still uses the same logical names that appear in the Org chapters, so the
future retangle remains straightforward.
3. Record the change in the living plan (`docs/reanimation-plan.org`) and
`arxana/README.md` whenever a milestone flips state.  If you touch a Part
VIII feature, add a short note referencing the Org chapter it originated
from.
4. Once a feature stabilizes, mirror it back into the matching Org file
and retangle (run `M-x arxana-build` with `C-u`).  The Org version stays
the source of truth when Phase 2 begins, so leave breadcrumbs (comments or
plan updates) that explain where the code should land.

## Generated code ↔ literate docs

The dev modules are effectively the “generated” code today.  To keep them
aligned with the literate Org chapters:

- Mention the originating chapter at the top of each helper you extend.
For example, `arxana-derivation.el` came from Part VIII; keep the header
comment accurate so future merges are trivial.
- When adding a new entry point, drop a short paragraph into the relevant
README/Org section explaining how it fits the plan.  The goal is to make
retangling a documentation exercise rather than archaeology.
- If you refactor a tangled function directly (`arxana-tangled.el`), move
that definition into `dev/` as soon as possible and note the change in
`docs/reanimation-plan.org` so we do not lose track of what needs to be
retangled.

## Pattern library contributions

The pattern-sync section in the README describes the day-to-day loop.  For
contributor clarity:

1. `M-x arxana-patterns-open` → enter the slug (e.g., `mojo/center`).
2. Edit the Org buffer the command opens; each stanza includes the Futon
component ids.
3. `C-c C-s` (`arxana-patterns-save`) to push text edits back to Futon.
4. Run `dev/run-tests.sh` (or the single `arxana-patterns` test when it
arrives) before opening a PR.

This guarantees design patterns round-trip through XTDB and that reviewers
can diff both the Org edits and the corresponding graph changes.

## QA / regression expectations

- Always run `dev/run-tests.sh`.  It tangles the sources, loads
`arxana-tangled.el`, and executes every suite in `test/`.
- If you change inclusion/integration flows, also run the scripted Org
walkthrough (see README “Scripted verification”) so the Scholia display and
relation buffers stay consistent with Futon.
- For storage changes, use the `docs/storage-bridge.org` curl snippets to
validate against a real Futon instance.

## Open limitations (next steps)

- Generalized pattern-language editing (treating an entire pattern set as a
single editable “language”) is still experimental.  Today’s tooling focuses
on individual pattern slugs; the multi-pattern workflow will ship in a later
milestone.
- Org→graph semantic mirroring is still one-way.  We can import/export Org
trees, but live edits inside Org buffers are not yet reflected into the
EAV stores automatically.

Track both of these gaps in README/plan updates so contributors know where
help is welcome.
