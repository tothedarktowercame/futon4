# VSATARCS

**VSATARCS** ("VSAT-shaped anthology of stories") is the Arxana-native
reader surface for the futon stack's self-description as a story
anthology.  Today it renders ~35 scene-structured Markdown stories
covering the stack's mission clusters, holistic-argument pillars, and
per-repo devmaps.  Implementation lives in
`dev/arxana-browser-vsatarcs.el`; entry point is `arxana://view/vsatarcs`.

## What it is

A reader, not a writer.  VSATARCS scans configured directories for
`*.md` files in **scene form** (each story consists of one or more
`## Scene: Title | anchor` blocks), parses them into a story-object
shape that anticipates a future `vsat-story` / `vsat-scene` entity
model, and renders them as browsable anthology entries with
intra-story / cross-story links.

The default source directory is `~/code/futon5a/holes/stories/`; the
landing page is `leaf-start-here.md` (returned to with `u`).  See
`arxana-vsatarcs-story-directories` and
`arxana-vsatarcs-landing-story` to retarget.

## Naming conventions

A single conceptual "story" in the anthology can have up to three
sibling files:

| Suffix | Role | Indexed in anthology? |
|---|---|---|
| `<name>.md` | The story itself — prose in scene form (`## Scene: Title \| anchor`). The thing humans read | **Yes** |
| `<name>.aif.md` | AIF+ annotation overlay, prose form.  Renders the AIF² typing of the story's spans (roles, claims, supports/attacks).  Not a story.  Used by AIF-aware tooling (renderers, the RewriteReview surface) to read structure off a story | **No** — filtered out |
| `<name>.aif.edn` | AIF+ annotation overlay, data form.  The canonical machine-readable hypergraph for the same story.  Source-of-truth for `<name>.aif.md` (which is generated from the `.edn` via `futon5a/scripts/render_*.clj`) | n/a — not Markdown |

Example triad: `leaf-2.md` (the story) + `leaf-2.aif.md` (its
annotation overlay, prose) + `leaf-2.aif.edn` (its annotation overlay,
data).

## Filter strategy

`arxana-browser-vsatarcs--story-paths` enumerates `*.md` files in the
configured directories and post-filters out anything matching
`*.aif.md` via `cl-remove-if`.  This keeps the anthology view clean —
without the filter, 16 zero-scene `.aif.md` companion files would
appear as empty "stories", cluttering the listing and confusing
newcomers.

The filter is intentionally narrow: it only excludes the `*.aif.md`
suffix, not arbitrary annotation siblings.  If new annotation suffixes
appear (e.g. `*.psr.md`, `*.trace.md`), extend the filter accordingly.

## What VSATARCS is not

- **Not a writer.**  It reads `.md` source; authoring happens in
  ordinary editing of those files.
- **Not the AIF+ atlas.**  The `.aif.md` / `.aif.edn` overlays are not
  intended to be browsed via VSATARCS.  An AIF+-specific reader (or
  the RewriteReview 3-up surface in `arxana-browser-rewrites.el`) is
  the appropriate consumer for that material.
- **Not the descriptive essay of the stack.**  VSATARCS today is a
  *static* anthology — a TOC over per-leaf stories.  Elevating it to
  a navigable structural map of the stack (Markov-blanket cut + arms
  + trajectory levels + acute fronts) is the target of follow-on
  work, tracked under
  `~/code/futon7/holes/M-interim-director-proxy-metric-inventory.md`
  (Phase 1 + Phase 2 of the descriptive-essay roadmap).

## Operational notes

- Adding a new story: create `<name>.md` in a directory listed in
  `arxana-vsatarcs-story-directories`, with one or more
  `## Scene: Title | anchor` blocks, then re-open VSATARCS.
- Adding annotations to an existing story: produce `<name>.aif.edn`
  (canonical) and optionally render `<name>.aif.md` via the
  `futon5a/scripts/render_*.clj` family.  Both will be ignored by
  VSATARCS' anthology scan, as intended.
- Testing: `dev/run-tests.sh` exercises
  `test/arxana-browser-vsatarcs-test.el`.

## Belief surface (R1)

VSATARCS carries an explicit per-entity belief state (per R1 of the
standard AIF completeness contract — see
`~/code/futon4/docs/vsatarcs-alignment-completeness.md` for the full
R1-R12 grading). The belief module is `dev/arxana-vsatarcs-belief.el`;
the reader's chrome surfaces it as a "Belief snapshot" block between
the scenes navigator and the scene body when a story is open.

**Status set.** Per-entity posteriors range over the M-INC v1
`state/*` tags: `spawned`, `refined`, `strengthened`, `addressed`,
`falsified`, `foreclosed`, `reopened`. The same set the WM-side
`futon2.aif.belief` carries, so per-entity comparisons reduce to
alist-lookup equality on shared entity-ids.

**Persistence.** Belief is read from and written to
`arxana-vsatarcs-belief-store-file` (default
`~/.emacs.d/var/vsatarcs-belief.eld`). Belief survives Emacs
sessions. The store file is a single readable elisp form holding
the (entity-id . posterior) alist.

**Reader keybindings (inside any VSATARCS buffer):**

| Key | Action |
|---|---|
| `B` | Bootstrap belief from `~/code/futon5a/holes/stack-annotations.edn` (uniform priors for every `:sections[]` entity not already tracked), persist to store, re-render |
| `R` | Reset in-memory belief (does NOT touch the store file); confirms before resetting |
| `i` | Read an elisp list of M-INC-compatible events from the minibuffer, ingest, persist, re-render |

Non-interactive entry points live in `dev/arxana-vsatarcs-belief.el`:
`arxana-vsatarcs-belief-ingest-events`,
`arxana-vsatarcs-belief-bootstrap-from-stack-annotations`,
`arxana-vsatarcs-belief-snapshot`,
`arxana-vsatarcs-belief-save` / `-load` / `-reset`,
`arxana-vsatarcs-belief-compare` (bilateral drift report between two
belief states; used by the alignment-mission apparatus).

**What's not yet wired (deferred).** Live M-INC event ingestion
(blocked on M-INC step (b) commit); story-scoped belief filtering
(coupled to R2 / observation channels, planned v0.3); cross-side
read of WM belief (blocked on a Drawbridge or analogous read path
to `futon2.aif.belief`). Each gap is recorded as a structured
`:enables` entry on the relevant closure in
`docs/vsatarcs-alignment-completeness.aif.edn`.

## See also

- `dev/arxana-browser-vsatarcs.el` — implementation
- `dev/arxana-browser-rewrites.el` — 3-up RewriteReview surface
  (consumes annotation `.edn` files via a corpus registry; default
  corpus remains the npt UKRN WP v12 essay)
- `dev/arxana-browser-essays.el` — alternative essay-browser surface
  (consumes `.el` manifest format)
- `~/code/futon5a/holes/stories/` — default story corpus
- `~/code/futon5a/scripts/render_aif2_prose.clj`,
  `render_leaf_prose.clj`, `render_external_prompt.clj` — AIF+
  prose generators producing the `.aif.md` companions
- `~/code/futon7/holes/M-interim-director-proxy-metric-inventory.md`
  — higher-order roadmap that subsumes VSATARCS' evolution
