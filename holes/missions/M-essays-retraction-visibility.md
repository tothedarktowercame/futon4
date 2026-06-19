# Mission: Essays Retraction Visibility — Persist Retained Annotations Across Reopen

**Date:** 2026-05-14
**Status:** READY TO EXECUTE
**Parent mission:** `M-essays-edit-cycle.md`
**Owner:** futon4 / Arxana Essays notes-pane and edit-cycle UI

## Purpose

The Essays subsystem now has two different retraction-visibility behaviors:

- During a live edit session, the notes buffer can already show
  soon-to-be-retracted annotations with strikethrough when their text
  overlay dies.
- After save and reopen, persisted `:retracted t` annotations disappear from
  the section-open path because the normal notes render still filters them
  out.

That split is incoherent. The user can see a retraction while editing, save,
and then lose the audit trace when the section reopens from manifest state.
This mission makes retraction visibility durable across the ordinary
save/reopen cycle.

## Current state

Evidence in `arxana-browser-essays.el`:

- `arxana-browser-essays--refresh-notes-retractions` already applies a live
  strikethrough preview in the notes buffer when an overlay has died during
  the current edit session.
- `arxana-browser-essays--open-section` still calls
  `--annotations-for-section manifest section-id` without
  `include-retracted`, so persisted retracted annotations are excluded from
  the render path that feeds `--render-section-notes`.
- `--render-section-notes` has no first-class branch for persisted
  retracted entries; the strikethrough is purely overlay decoration added by
  the live preview path.

## Scope

### In scope

- Make persisted `:retracted t` annotations visible in the notes pane after
  save/reopen.
- Unify the persisted-render and live-preview semantics so they do not
  contradict each other.
- Add tests covering:
  - a persisted retracted annotation visible after reopen
  - a live preview still appearing before save
  - a restored annotation losing the strikethrough again when re-anchored

### Out of scope

- Diachronic essay argument graphs.
- XTDB schema changes outside what is strictly needed for retraction
  visibility.
- Reworking the whole note entry visual language.

## Open questions

1. Should this mission also add `:retraction-kind` / `:retracted-at`
   manifest metadata, or should that be a follow-on once retained
   visibility itself is stable?
2. Should persisted retracted entries render inline markers such as
   `[retracted]`, or is the face + inspect path enough for the first pass?
3. If a previously retracted annotation becomes anchorable again, should the
   manifest auto-clear `:retracted`, or is that a manual decision?

## Exit criteria

- Opening a section whose manifest contains `:retracted t` annotations shows
  them in the notes pane rather than filtering them away.
- The visual treatment for persisted retractions and live pending
  retractions is coherent enough that the user can understand the state
  transition.
- Targeted ERT coverage lands in `test/arxana-browser-essays-test.el`.
- A checkpoint is appended to the parent mission with test counts and a
  one-line live-behavior note.

## Collaboration boundary

Suggested split if Claude takes this companion mission while Codex stays on
the parent:

- Claude owns persisted retraction visibility, note-render semantics, and
  tests in `arxana-browser-essays.el` / `arxana-browser-essays-test.el`.
- Codex owns the live registry rollout, migration verification, and the
  next mission decomposition for re-anchoring / flexiarg drift.
