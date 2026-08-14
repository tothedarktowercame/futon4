# M-librarian — native 3D editing as a regular practice (Steam Frame)

Chartered 2026-08-10 from a 2026-08-09 operator remark during invoice-202506
planning. Status: **HEAD** — pre-IDENTIFY intake; explicitly a "bonus lap"
gated behind the client-facing VSAT delivery. Not yet scoped, not yet
resourced; the hardware it targets has not shipped.

## Operator-voice anchor

Joe, 2026-08-09 (verbatim, from the invoice-202506 planning session):

> Regarding the Librarian layout, I do slightly wonder if the a-frame
> in-browser stuff is really going to be "sufficient" for a nice demo. There
> are rumours that the Steam Frame will ship later this month and the device
> looks interesting enough that I might want to build a "Librarian" app
> that's native for Steam Frame, on the view that "editing in 3d" might
> actually be something I would want to do on a regular basis. But that's a
> "bonus lap" for after I do the client facing work.

Hardware: https://store.steampowered.com/hardware/steamframe

## What's already felt to be true

- The VSAT focus board (M-authoring-3d, `~/vsat/docs/missions/`) proved the
  interaction grammar: a story's scenes as floating 3D cards with a small
  closed verb set — read / edit / connect / create / remove / arrange. The
  verbs survived contact with a real corpus and a real client.
- The "Librarian" idea — depth-arranged, archive-stack spatial organisation
  rather than a flat wall — is the arrangement verb grown into a *place you
  work*, not a feature of someone else's site. The pull is personal practice:
  editing in 3D regularly, not demoing it occasionally.
- The doubt is specific and plausible: browser A-Frame on a headset may cap
  out below "nice" — input latency, text rendering (troika under WebXR),
  session friction — in exactly the ways a native OpenXR/SteamOS app would
  not.

## Anti-glibness discipline

- **Don't relitigate the demo.** The invoice-202506 Librarian line (WP2 in
  `~/code/invoices/plan-202506.md`) is drag + persist in A-Frame, trimmed to
  4 hours, and it is *sufficient for the client demo*. This mission is not a
  reason to gold-plate WP2 or to let native-app thinking creep into billed
  scope.
- **The hardware is a rumour until it isn't.** No design work against
  imagined specs; the mission stays at HEAD until a device exists (in hand or
  with published dev docs) and the VSAT delivery liability is discharged.
- **"Regular practice" is the claim to test, not assume.** The cheapest
  falsifier comes free: the WebXR pass on Quest 2 (WP5) is a lived trial of
  editing-in-headset. If that never gets used voluntarily after the demo,
  a native app won't fix it.

## Working-economy position

- **What underwrites it:** invoice-202506 WP2 builds the durable layer — the
  per-story pinned-arrangement schema and REST API. Standing design
  constraint (recorded 2026-08-09, memory
  `project_librarian_steam_frame_bonus_lap.md`): **keep the pinning model
  client-agnostic** — arrangements served over the ordinary API, not state
  baked into the A-Frame page — so a native client consumes the same
  arrangements and the bonus lap starts from a working backend, not a
  rewrite.
- **What it would underwrite:** a personal 3D workbench for corpus
  arrangement — VSAT stories first, but the arrangement-of-cards-in-space
  pattern generalises to the Arxana/futon reading-and-editing stack (cf. the
  stack-geometry and stereolithographic-view threads). If editing in 3D is
  real, this is an input surface for the whole working economy, not a VSAT
  epilogue.

## Clarity-gap / carried-forward tensions

1. **Sufficiency question, unanswered:** is the ceiling on "nice" actually
   A-Frame/browser, or is it design? Needs a felt comparison, not a
   benchmark. The Quest 2 WebXR pass gives the browser half of the data.
2. **Platform unknowns:** Steam Frame ship date, dev story (SteamOS/OpenXR
   toolchain, sideloading, controller/hand input), and whether "native"
   means Godot/Unity/raw OpenXR — all TBD at charter time.
3. **Scope boundary with VSAT:** a native Librarian reading VSAT
   arrangements is a *client* of vsat-dev's API. What auth story does a
   personal native client use against a handed-over site? (Post-handover,
   Joe may not control the deployment.) May argue for a local-first copy of
   the corpus instead.
4. **One practice or two:** is Librarian a VSAT feature grown up, or the
   first Arxana-side 3D surface that merely *demos* on VSAT data? Answer
   shapes where the code lives.

## Provenance

Operator remark in the legacy-CLI invoice-planning session, 2026-08-09;
captured same day as auto-memory `project_librarian_steam_frame_bonus_lap.md`
(~/.claude/projects/-home-joe-code/memory/); promoted to this mission doc
2026-08-10 at Joe's request. Related: `~/code/invoices/plan-202506.md` (WP2,
WP5), `~/vsat/docs/missions/M-authoring-3d.md` (verb set, slice 3 spec).

**HEAD exit criterion:** Joe ratifies this as faithful; IDENTIFY opens only
after (a) invoice-202506 delivery is discharged and (b) Steam Frame exists
with a known dev path. Until both, this file is the parking spot.
