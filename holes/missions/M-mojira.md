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

Joe, 2026-11-10, choosing the surface and the first interaction:

> So, I think we should focus on the VS Code extension. As a first
> approximation: this will be the place where the stack aggregates data
> linked to the GCC, with M-repl-circus being one supplier. Relevant prior
> art includes stack-hud-1 (pre-Circus) and *象 stepper* as well as the
> Frames and Diagram parts of Voxterm. While we will keep *象 stepper* as it
> is, we can deprecate the Frames and Diagram tabs in Voxterm b/c I don't use
> them and I don't want to waste compute producing things I don't use. A
> reasonable interaction to have here is to display the lexicon terms that
> come up per frame and their interconnections, perhaps a bit inspired by
> WebArxana or https://zone.hyperreal.enterprises/wip/formal-arxana/giraffe-local.html
> but probably with more of a text-browser feel (prior art is my own nero.el
> or Conkeror, surfingkeys, etc). Other relevant data comes from M-giraffe,
> possibly via Plane. Another way to think about it is as a way to understand
> where we are relative to Cascade Live: what is being worked on, what the
> benefits are, and then to be able to browse around in the space of possible
> tasks. After circa 1 month, it should considerably help me prepare a good
> Mozilla LOI, because I can say "not only am I interested in the kinds of
> things that you're interested in, but I have a real working prototype that
> supports me in living in line with these values."

## Decisions so far

- **Surface: a VS Code extension** in code-server (Joe, 2026-11-10). marimo
  is not the first surface.
- **Role: aggregator.** Mojira is where the stack's data linked to the GCC
  comes together. Suppliers named so far: M-repl-circus (frames/turns),
  M-giraffe (possibly via Plane), the lexicon, Cascade Live.
- **Horizon: about one month** (to ~2026-12-10), so that the prototype can be
  cited in the Mozilla LOI as something Joe actually lives with.
- **象 stepper stays as it is.** Voxterm's Frames and Diagram tabs are to be
  deprecated (see below).

## Prior art

- `futon0/contrib/stack-hud-2.el`, `futon0/holes/missions/M-stack-hud-refactor.md`,
  `futon0/analysis/excursions/E-stack-hud-cleanup.md` (stack HUD, pre-Circus).
- 象 stepper: `futon3c/web/stepper/` (served at `/wip/stepper/?agent=…`).
- Voxterm Frames tab (an iframe of the 象 stepper) and Diagram tab (dispatch,
  turn and cascade views), `voxterm/index.html` and `voxterm/server.py`.
- Lexicon graph: `wip/formal-arxana/lexicon-graph.html`,
  `wip/formal-arxana/giraffe-local.html`; the lexicon and workplan live in the
  durable :7094 Arxana stack.
- Text-browser feel: nero.el, Conkeror, Surfingkeys (keyboard-first,
  link-hinting, history as a first-class object).

## Voxterm Frames and Diagram deprecation (finding, 2026-11-10)

Both tabs fetch only while visible: Frames sets an iframe `src` when opened,
and Diagram's poll timer skips its fetch when the view is hidden. Removing the
tabs therefore saves little compute by itself. Whatever produces their data
server-side (the 象 cascade sidecar files read by `/xiang/cascade/`, the
`:7070` `diagram/dispatch` route, `wip-cascade-snapshot.timer`) may have other
consumers and has to be checked one by one before anything is switched off.

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

## Carried in from M-jvm-arxana (2026-11-10)

Completion condition 3 of M-jvm-arxana, carried here by Joe ("3 should carry
to M-mojira as the fastest route to demo something of that ilk"): **show Emacs
and VS Code consuming the same entity and scope-resolution response for one
live artifact revision.** State at handover: both clients have produced
identical semantic digests (resolve `0397248a…`, correspondence `9c91f35e…`),
but on the committed draft8 fixture with an in-memory store, on different days
(futon6 `holes/M-jvm-arxana-codeserver-demo-report.md:72-75`). What is missing
is one live revision, read by both clients from the JVM authority, side by
side.

## Checkpoint 1: basic working browser (2026-11-10)

Joe, 2026-11-10: save "a checkpoint section in M-mojira (with a git
reference) so that we could come back to this basic working version and build
the cool extensions ... from a strictly 'Mojira' standpoint rather than a
cool-features standpoint I think the priority is to understand the 'operator
focus' questions".

- **Code:** futon4 `17d775e`, tag `mojira-checkpoint-1`, `dev/mojira/`
  (`extension.js` loader, `core.js` pages/keys/webview). Installed by symlink
  `~/.local/share/code-server/extensions/futon4.mojira-0.1.0` and an entry in
  that folder's `extensions.json`.
- **Reads:** the lexicon (`~/notes/cascade-live-outline/lexicon/merge/all.json`,
  251 entities) and the pending index (`.../pending/pending-index.tsv`, 322
  items) from files; mission/excursion files under `~/code/*/holes/`.
- **Does:** home (status, pending state, where the pending work is), entity,
  pending-item, status, label and search pages; two-up with the right column
  following the selection; Tab/Shift+Tab between columns, Enter in the right
  column shifts the columns (Miller style); `f` hints; files open in Emacs via
  `emacsclient -n` (`mojira.openIn`, fallback to the editor); `core.js`
  hot-reloads on save.
- **code-server around it:** decluttered user settings (backups
  `settings.json.bak-2026-11-10`, `…bak-20261110-useterminal`), Chat AI
  features off, claude-cs in a terminal via futon6
  `tools/futon-relay-channel/claude-cs.sh`.
- **Not done:** the lineup pager, scopes and code+docs below; Q2 under "Open
  questions"; the operator-focus questions.

## Design notes: lineup columns, scopes, code+docs (claude-cs, 2026-11-10)

Joe, 2026-11-10: look at claude-21's lineup page
(`wip/apm-reading-sample/lineup/`) "since we might use similar ideas to add
further columns", note its "Arxana style scopes that are more complex than our
'press RET to activate' highlighting", and think about "how we'd display
code+docs in Mojira if we browse all the way to some code that has
documentation".

What the lineup page does (read from `a99J04.html`): five panes in a row
(problem, Lean source, generated proof, hand-written proof, reference
solution), 1–3 per screen, snap scrolling, Alt+←/→ by one pane, a chip per pane
lit while it is visible. Every occurrence of a declared object is a
`scope-token` carrying one `scope-<hash>` key; `scopeMeta` maps the key to its
target, kind (`declared`, `defined term`) and, when declared, its introduction
line and source. Hover lights the token (`scope-active`) and every peer in
every pane (`scope-peer`); click pins it and scrolls the source pane to the
introduction line (`source-focus`).

What Mojira takes from it:

1. **The trail becomes the lineup.** Each followed link adds a pane on the
   right (Federated Wiki: following from pane k drops the panes after k).
   1–3 panes per screen (`v` cycles); Tab/Shift+Tab move between panes;
   `[`/`]` page by one. Today's two-up is the 2-per-screen case.
2. **Links become scopes.** A link's key is what it names (entity id, item id,
   file + declaration). Selecting one lights its peers in every visible pane,
   so "where else does futon1b come up in this trail" is visible without
   following anything. Pinning one shows its introduction: the entity's own
   page, or in code the declaration line. Kind and introduction come from the
   authority's scope record, not from Mojira.
3. **Code+docs is a lineup of manifestations.** When the trail reaches a code
   file that has a code↔docs revision, the panes are [code] [docs] [Lean
   citations]. Moving through definitions in the code pane lights the doc
   entry and the citing Lean declarations; a definition whose body changed
   since its doc entry is marked (`:since`); RET opens the definition in Emacs
   at its line. The data is the `:codedocs/view` already served by the JVM
   authority (futon3c `codedocs_view.clj`: definitions in source order, doc
   spans, citing Lean declarations, standing), which futon4a
   `futon4a-codedocs.el` shows in Emacs as a two-pane client
   (CODEDOCS-INSTANTIATE.md, packets 5–8).
4. That also discharges the carried condition above: Emacs
   (`futon4a-codedocs.el`) and Mojira read the same `:codedocs/view` response
   for one live revision, side by side.

Order, each a small slice: (1) lineup pager, core.js only; (2) peer
highlighting on link keys; (3) the code+docs panes against a codedocs stack,
reusing the `arxana.v1` client in futon6 `tools/arxana-vscode/lib/client.js`.
Slice 3 needs a running codedocs stack to read; the demo one on :7091 is not
claude-cs's to call, so it waits for an agreed stack.

## Candidate view from M-daily-scan (Joe, 2026-11-10)

A triage view for the reworked daily scan (futon7
`holes/missions/M-daily-scan.md`, "Rework direction"): "a summary on the left
for example, and details on the right and invalidate either item by item or
the whole block in one go". Each invalidation is recorded against the scan's
stated predictions, because the scan is to be a learning loop.

## Current constraints

- **Memory: 4 GB.** code-server runs as `code-server.service` with
  `MemoryHigh=4G`, `MemoryMax=6G`, and the claude-cs session runs inside it
  (2.9 GB in use on 2026-11-10 with nothing else running). Joe, 2026-11-10:
  "let's see how far we get with the 4GB." Heavy one-off jobs (headless
  Chromium screenshots) run in their own scope:
  `systemd-run --user --scope -p MemoryHigh=8G -p MemoryMax=10G …`.
  Measured 2026-11-10: most of the 4 GB is code-server's per-window file
  watcher over all of `~/code` (2.3–2.9 GB for Joe's window; about 1 GB more
  for each headless screenshot window, kept for the 3 h reconnection grace).
  `files.watcherExclude` now drops the heavy folders. Screenshots should open a
  small folder (`?folder=/home/joe/code/futon4/dev/mojira`), not `~/code`.
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

1. (Answered 2026-11-10: VS Code extension.)
2. Does "watch" cover only code-server, or also Emacs and REPL turns from the
   evidence store (read-only :7073)? Since Joe works in Emacs and uses
   code-server as the HUD, watching Emacs looks necessary.

## Ideas kept, not commitments

nero.el (https://github.com/emacsattic/nero); futon4's iPod-style Arxana
browser; ejira adapted to Plane; marimo for visualisation; Federated Wiki;
Cascade Live.
