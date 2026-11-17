# Arxana push: work packages (draft, claude-5, 2026-11-17)

Joe, 2026-11-17: "I'd rather break it down across work packages and then think about
who joins after. We might keep the existing pool focused on their missions but liaise
from outside to keep Arxana as a kind of "innovation hub" shared by these various
efforts."

Staffing is deliberately absent; each package says what it needs (Claude review,
Codex build, Zai memory work, Joe) so seats can be matched later. Each has one
deliverable, one acceptance test with the bad case it must catch, and its
dependencies. "Hub" says which other effort the package serves or draws on.

Model this rests on (excursion, last three sections): the Evidence Landscape is the
immutable base; 'clusions (edit, rewrite rule, link, requirement) are immutable,
attributed, dated records over it, updated only by later records that supersede them;
drafts are projections (static: pinned 'clusion versions; live: re-derived), with no
privileged head. Traceability above all (DERIVE-1).

Dates: Mozilla incubator closes **2026-11-20**; Fellowship LOI **2026-12-17**.

## Track I — the incubator application (dated)

**WP1 Incubator requirements.** The incubator form's questions
(`~/mozilla-incubator.txt`) and their limits as requirement records, each failing
until a field has text within its limit; shown as a list in Emacs and Mojira.
- Needs: a small build (file-backed first; jvm-arxana records when WP4 lands).
- Accept: the list matches the form question for question; a field over its limit
  shows failing. Bad case: a field one word over the limit must not read as met.
- Depends: none. Serves: WP2.

**WP2 Write the incubator fields as projections.** Joe writes each field from his turns
and draft 2 in the existing views, with sources in the side pane; the field text is
exported for the web form.
- Needs: Joe (the writing); a small build for the export.
- Accept: every field within its limit, every sentence traceable to a turn or marked as
  new writing. Bad case: an exported field differs from what the view shows.
- Depends: WP1. Fallback: if tooling is not ready by 2026-11-19, Joe submits from
  whatever is written; the tool never stands between him and the deadline.

## Track II — records (jvm-arxana / futon1b)

**WP3 Restart for valid time.** futon1b 5147aa8 / e4aa6e1 and futon3c 09ca6c8c4
(event time as valid time; as-of reads) go live.
- Needs: Joe's restart of futon1b and futon3c.
- Accept: a record written with a past :event-time reads back as of that time and not
  before. Bad case: as-of before the event returns the record.
- Depends: none. Serves: WP4, WP8.

**WP4 Projection and 'clusion-update records.** `:projection` (selection, order,
pinned 'clusion versions or a live query, optional `:as-of`) and `:clusion/update`
(`:supersedes`, `:scope` over derived projections, edit time, rationale), per the
Pijul study (`~/notes/zai-6/pijul-for-arxana-2026-11-17.md`).
- Needs: Codex build, Claude review.
- Accept: two projections over one passage, one edit scoped to one projection: the
  other projection is unchanged. Bad case: an edit leaking into a projection outside
  its scope.
- Depends: WP3 for as-of. Serves: WP2 (later), WP6, WP9.

**WP5 One head becomes "the projection on screen".** F2 keeps one accepted head per
artifact; projections need none.
- Needs: Codex build, Claude review (F2's tests must keep passing).
- Accept: two projections of one artifact advance independently. Bad case: advancing
  one moves the other.
- Depends: WP4.

**WP6 Requirement records.** A requirement 'clusion with a satisfaction relation
(Pijul has no counterpart): question, limit, deliverables, each a known-failing test.
- Needs: Codex build.
- Accept: a requirement flips from failing to met only when a projection's text meets
  it. Bad case: met by text outside the projection.
- Depends: WP4. Replaces WP1's file-backed records.

## Track III — Nipper

**WP7 Nip records and a nip writer.** A nip = query (strategy, terms, dials) + result
spans (turn id, codepoint span, text sha256) + acceptance (by, at)
(`~/notes/zai-13/nipper-design-2026-11-17.md`, with typed entries becoming edit
'clusions per Joe's model).
- Needs: Zai (memory work).
- Accept: a nip re-resolves to the same characters from its record alone. Bad case: a
  nip whose turn text changed digest must refuse, not re-anchor silently.
- Depends: none for the record; WP4 to store in jvm-arxana.

**WP8 Nipper proposes against a requirement.** Given a failing requirement (WP1/WP6),
Nipper returns candidate spans from the landscape, by its four strategies.
- Needs: Zai.
- Accept: for one incubator field, candidates appear in the side pane and accepting
  one writes a nip. Bad case: a candidate whose span does not match its turn.
- Depends: WP7, WP1.

## Track IV — editing and views

**WP9 A document edited by hand (Nelson's T2).** In Emacs, an insert, a rearrange and a
delete on one paragraph become 'clusions (and nips), never flat-file changes; the views
render the result.
- Needs: Claude or Codex build (Emacs), Claude review.
- Accept: after the three edits every character still answers who, when and from
  where (T0). Bad case: an edit that rewrites stored content instead of appending.
- Depends: WP4 (or a file-backed stand-in first, marked as such).

**WP10 Links in the demo.** Free-standing typed links (comment, benefit, beneficiary),
using what classic Arxana, jvm-arxana and Mojira already have, surviving edits (T3,
Nelson's Adam/Barker walkthrough).
- Needs: MAP first (where links live in jvm-arxana), then a build.
- Accept: a comment on a passage survives an insert and a partial delete. Bad case: the
  link reattaching to neighbouring text after the delete.
- Depends: WP9.

**WP11 Two projections side by side (T1).** Incubator and Fellowship drafts in two panes
with shared content highlighted on cursor-over (partial credit already earned; beams
in Mojira optional).
- Needs: Mojira build.
- Accept: moving onto shared text marks it in both panes. Bad case: marking text that
  is similar but not the same content.
- Depends: WP4, WP2.

## Track V — the Fellowship LOI (2026-12-17)

**WP12 The LOI as a second projection.** The 15 fields in
`futon7/holes/mozilla/LOI-draft-1.md` as requirements over the same landscape and
'clusions as the incubator draft, plus pullacross (T4: used and not-yet-used material).
- Needs: Joe, builds from WP4/6/8/11.
- Accept: an edit made for the incubator appears in the LOI only when its scope says so.
- Depends: Tracks II-IV.

## Hub: what each effort brings and takes

- **M-repl-circus** (closing): the GCC records and the turn records Nipper reads; the
  stepper's turn data is the landscape's newest edge.
- **MetaCA / simulation:** a candidate second landscape (run records instead of turns)
  for WP4's projections; no package depends on it.
- **WM / Giraffe:** requirements as known-failing tests resemble WM's wants; WP6 can
  borrow its vocabulary rather than invent one. No WM run is needed.
- **DiagramProver:** chainable rewrite rules ("bogus" → "nogood" → "no good") on its
  DPO engine; a later package, not in this push.

## Order

WP1 → WP2 is the dated path and should not wait on anything else. WP3 (Joe's restart),
WP4 and WP7 can start in parallel with it. WP9 is the first package that changes how
Joe writes; WP12 gathers everything.
