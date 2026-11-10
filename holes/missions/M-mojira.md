# M-mojira — a code-server heads-up display for browsing the strategy layer

**Date:** 2026-11-10
**Status:** OPEN — IDENTIFY
**Owner:** Joe (operator); claude-cs technical driver (the code-server seat)
**Repos:** `futon4` (this mission; Arxana browser donors), `futon3c` (JVM
Arxana authority, Agency, Giraffe), `futon1b` (evidence store, read-only)
**Related:** `M-jvm-arxana.md` (the shared JVM authority Mojira will read
through), `holes/excursions/E-gcc-schema.md` (GCC, Part A and §15),
`futon3c/holes/excursions/E-giraffe-top-twenty-verdict.md`

## Operator anchor

Joe, 2026-11-10 (via claude-13's onboarding brief,
`~/notes/claude-13/claude-cs-01-onboarding.md`):

> what about developing Mojira as a code-server-native browser rather than an
> Emacs browser? So what Mojira would do would be 'watch' what I'm doing (like
> esports) and create the links, analytics, and so on? That's pretty much what
> I was thinking would be going on with code-server.

The name is "an homage to Mozilla so that we can keep in mind that Mojira is
meant to help us make sense of the priorities relative to the benefits that
we're trying to develop in the Mozilla Fellowship proposal". Mojira "could
include concepts of Mosaic, Jira, Plane, Cascade Live, nero, Federated Wiki,
Arxana, all mashed together as a 'strange beast'."

Joe, 2026-11-10, on the first working session: VS Code (code-server) becomes
a heads-up display beside Emacs, where he keeps working. "To be an effective
HUD it needs to be decluttered and focused."

## Gap

Joe works in Emacs; the strategy layer (missions, Giraffe's ranked items, GCC
nodes, the Fellowship benefits) lives in files and stores he has to go and
look at. Nothing watches the work as it happens and shows where it sits among
those priorities, and the GCC is described (E-gcc-schema Part A) but cannot
yet be browsed.

## What Mojira is (working statement)

A browser for the strategy layer that runs in code-server as a HUD. It watches
what Joe is doing, draws the links between that and the strategy layer, and
lets him drill down through them in the style of nero and the iPod-style
Arxana browser. Analytics (where time went, which items moved) come from that
record without extra bookkeeping. It is not a general web browser.

## Relation to M-jvm-arxana

Questions of entity identity, graph relation, scope and correspondence are
answered by the JVM Arxana authority, and Mojira is one more client of it, as
M-jvm-arxana's invariant requires: editor-local view state is a disposable
projection. Mojira does not grow its own store or datalog layer (see
E-gcc-schema §15 on futon1 being rewritten twice).

## Current constraints

- **Memory: 4 GB.** code-server runs as `code-server.service` with
  `MemoryHigh=4G`, `MemoryMax=6G`, and the claude-cs session runs inside it
  (2.9 GB in use on 2026-11-10 with nothing else running). Joe, 2026-11-10:
  "let's see how far we get with the 4GB." Heavy one-off jobs (headless
  Chromium screenshots) run in their own scope:
  `systemd-run --user --scope -p MemoryHigh=8G -p MemoryMax=10G …`.
- **Not a distraction.** Joe is working through Giraffe's top twenty and does
  not want to "get distracted building a new web browser". Mojira slices stay
  small and useful the same week.
- **Read-only against shared services.** No writes to :7070 (other than
  bells/parks) or :7073; no `/health?deep=true`.

## State at opening (2026-11-10)

- code-server :8090 user settings decluttered (no Welcome page, activity bar,
  status bar, command center, minimap or breadcrumbs; one tab at a time).
  Backup: `~/.local/share/code-server/User/settings.json.bak-2026-11-10`.
- Screenshot check: a Playwright headless-shell script loads :8090 and saves a
  PNG for comparison with Joe's own screenshots. It shows settings, not Joe's
  per-browser layout.

## Candidate first views (not yet a schedule)

1. **Top-twenty panel:** a tree of the frozen Giraffe list with each verdict;
   clicking an item opens its mission or excursion.
2. **Backlinks for the current file:** missions, excursions and top-twenty
   items that mention the file Joe is touching.
3. **Today's trail:** files and turns of the day, grouped by the mission each
   links to.

## Open questions for Joe

1. VS Code extension (tree view + webview) or marimo notebook as the first
   surface? claude-cs leans to the extension, marimo later for analytics.
2. Does "watch" cover only code-server, or also Emacs and REPL turns from the
   evidence store (read-only :7073)? Since Joe works in Emacs and uses
   code-server as the HUD, watching Emacs looks necessary.

## Ideas kept, not commitments

nero.el (https://github.com/emacsattic/nero); futon4's iPod-style Arxana
browser; ejira adapted to Plane; marimo for visualisation; Federated Wiki;
Cascade Live.
