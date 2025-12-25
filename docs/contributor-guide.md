# Arxana Contributor Guide

## Phase-aware workflow

Prototype 1 keeps the fast-moving enhancements inside `dev/`, while
canonical docs live in XTDB.  Filesystem doc snapshots are optional and
should be treated as temporary unless explicitly promoted.

1. Load the harness (`M-x arxana-build` or `./dev/run-tests.sh`) so the dev
modules share the same Emacs session as your edits.
2. Make iterative changes inside `dev/*.el` or `test/*.el`.
3. Record the change in the living plan (`docs/reanimation-plan.org`) and
the relevant XTDB doc entry whenever a milestone flips state.
4. If you create a filesystem doc snapshot, tag it as temporary or
archival and note the XTDB doc id it mirrors.

## Dev code ↔ XTDB docs

To keep code and docs aligned:

- Keep module header comments accurate and point to the XTDB doc id (or
the temporary snapshot) that describes the behavior.
- When adding a new entry point, add a short paragraph to the relevant
XTDB doc entry explaining how it fits the plan.
- Track drift in `dev/org-sync-tracker.org` so missing doc updates are
easy to spot.

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

- Always run `dev/run-tests.sh`.  It loads the dev modules and executes
every suite in `test/`.
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
