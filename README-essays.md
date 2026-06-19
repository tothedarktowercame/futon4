# Arxana Essays + eoi-new

Arxana Essays is the annotated-edition surface for long-form documents
(`dev/arxana-browser-essays.el`); `eoi-new` is the launcher that produces
Essay-ready artefacts via the eoi-engine interview protocol. Together they
form a single loop:

1. `eoi-new` runs a structured interview, capturing Joe's verbatim answers
   and assembling them into an artefact + manifest.
2. The artefact is imported into Arxana Essays as an annotated edition.
3. Editing, retraction, and round-2 tension capture happen in-browser.

This README documents step 1 — how to start an EoI run from scratch.

## Accessing eoi-new

`eoi-new` lives at `~/code/futon0/scripts/eoi-new` (sibling of `cr`). Make
sure that directory is on `$PATH`, or invoke by full path.

Requires the same runtime as `cr`: Emacs daemon running, Agency (futon3c)
reachable on port 7070.

## Worked example: writing an EoI to Ineffable

Use case: you want to apply for a role at a company called Ineffable.

### 1. Stash the context

Save whatever the agent should read to seed the run — typically the job
posting (paste into a `.md` file if you only have the URL), plus any
background you want anchored on (the company's about page, prior
correspondence). One file is fine; multi-file context isn't supported yet,
so concatenate if you need to.

```sh
mkdir -p ~/npt/applications/ineffable-<role-slug>/
$EDITOR ~/npt/applications/ineffable-<role-slug>/context.md
```

### 2. Pick a flash type

| Flash | Shape | When |
|---|---|---|
| `eoi-outward` | first-contact letter, multi-round | you expect a reply and a dialogue (Glasgow Cogito shape) |
| `eoi-outward-one-shot` | formal application, round-collapse | single-pass artefact with no follow-up (Anthropic shape); every tension must be addressed now or marked not-applicable |
| `eoi-inward-mission` | new mission HEAD authored by Joe | seeding a fresh mission doc; HEAD section in Joe's register |
| `eoi-inward-institution-self-authored` | self-authored institution-object | declaring what an institution Joe writes (e.g. Hyperreal-Director) IS |
| `eoi-outward-into-self-authored-institution` | outward EoI into a self-authored institution | Joe-applies-to-Hyperreal-Director shape; composes Side A ; Side B |

If unsure: `eoi-outward` is the forgiving default. Switch to `-one-shot`
only when you're confident the artefact is genuinely single-pass.

### 3. Run it

```sh
eoi-new eoi-outward \
  --context ~/npt/applications/ineffable-<role-slug>/context.md \
  --out ~/npt/applications/ineffable-<role-slug>/
```

Or — if you forget the flash type — run `eoi-new` with no arguments and
pick from the interactive menu.

### 4. The agent spawns and bootstraps

A fresh Claude (`claude-N`) is registered and opens its REPL buffer. Its
first turn is a `claude:` block that:

- Reads the engine spec (`~/code/algorithms/eoi-engine.md`)
- Reads the schema (`~/code/atthangika-buckets.json`)
- Reads the context file you passed
- Confirms back the flash, round-collapse mode, context summary, and output
  destination
- Issues **Q1 (Right View)** in behavioural-proxy phrasing

The agent never sees this conversation thread; it knows only what's in the
engine spec, the schema, and the context file. Make the context file as
fat or thin as serves the run — if it's thin, the agent will ask follow-up
questions before Q1.

### 5. Answer in your own words

Short, bullets, telegraphic — your register. The agent maintains question
state across the five Path-arrows (View, Intention, Speech, Mindfulness,
Livelihood) plus the diagnostic-probe / tension surface.

Discipline the agent enforces:

- **One question per turn.** Never batches, never lists.
- **No paraphrase.** Your words appear verbatim in the artefact; the agent
  only orders them and provides minimal connective tissue (section labels,
  one italic orienting line per section). Voice-preserving copy-edits
  (article correction, typos, punctuation) are allowed; rewording is not.
- **Revision is cheap.** "Scratch that, my real answer is…" reopens the
  slot.

### 6. Closure

When the soft-floor (≥4 essential answers) is met and round-1 essentials
are covered, the agent offers closure:

> *"I have enough to assemble. Anything else you want to say first?"*

Add anything, then close. For one-shot flashes, closure cannot be granted
while any `address-now` tension still lacks verbatim content — the agent
will block until you supply it or retag the tension `not-applicable`.

### 7. Assembly + write-back

The agent assembles the artefact at `--out` using your verbatim answers,
section labels, and orienting italics. It writes the schema entry to
`atthangika-buckets.json` — creating the institution-object (e.g.
`:node/ineffable`) if it doesn't already exist — with referential
integrity enforced.

The output typically contains:

- `<flash-slug>-v1.md` — the artefact itself
- `annotations.el` — when the artefact lives in its own directory, the
  Arxana Essays manifest declaring sections and the (initially empty)
  annotation list
- `load-into-emacs.el` — one-shot loader that registers the essay with
  Arxana Essays and imports only that manifest

If the artefact lands in a shared directory that already contains multiple
markdown files (for example a strawmen container directory), use named
companions instead:

- `<slug>-annotations.el`
- `<slug>-load-into-emacs.el`

From here, import into Arxana Essays for round-2 diagnostic annotation
and editing.

## The file-role convention (read before ingesting an essay)

An essay directory separates three concerns. Keeping them separate is the
whole discipline; collapsing them is the classic ingest failure.

1. **The source markdown** (`<slug>.md`) is **canonical and read-only.**
   Annotations never get written *into* it. There is no
   `<slug>-annotated.md` to produce — an annotated edition is a *render*
   (overlays in the reading view), not a file. If you find yourself baking
   `> [hx:...]` blockquotes into the body, stop: that is the mistake.

2. **`annotations.edn` is the authoritative annotation layer** — author and
   edit annotations *here*, in EDN. Each annotation carries a `:section-id`,
   a `:passage` that **must be a verbatim substring of the source markdown**
   (the overlay attaches by exact text match — paraphrased or
   character-offset anchors silently fail to attach), a `:patterns` vector,
   `:severity`, `:status`, and a `:suggestion`/`:note`.

3. **`annotations.el` is a thin manifest only** — the `defconst` plus the
   `:essay` and `:sections` declarations, with an **empty `:annotations ()`**.
   When the loader (`arxana-browser-essays--load-manifest-file`) finds a
   sibling `annotations.edn`, it converts that file's annotations to the `.el`
   shape and **replaces** the manifest's annotation list — so the `.edn` is the
   source of truth and the `.el` is just the section manifest. Authoring
   annotations in the `.el` instead of the `.edn` is the second classic
   mistake; they will drift from the `.edn` and confuse everyone. Regenerate
   the `.el` from the `.edn` when sections change; do not hand-maintain both.

   The `.el`/`.edn` split is not arbitrary: `.el` is the Emacs-loadable
   manifest, `.edn` is the editable data. Edit data in the `.edn`.

`load-into-emacs.el` must build the manifest via
`arxana-browser-essays--load-manifest-file` (which folds in the `.edn`), then
pass *that* to `--import-manifest`. Passing the raw `defconst` symbol imports
zero annotations, because the symbol's `:annotations` is the empty list.

**Verify before trusting an ingest:** run `M-x
arxana-browser-essays-audit-passages`. It reports, per section, whether each
annotation's `:passage` actually anchors in the source. Two things it catches:
a passage that isn't a verbatim substring (fix the passage), and a section
whose `heading-text` doesn't match a `##` heading in the source (the extractor
keys on **flat `##` headings** — declared section structure lives in the
manifest's `:index`, not in `#`/`###` markdown depth, so normalise all section
headings to `##` and let `:index` carry the nesting).

## Caveats worth keeping in mind

- The agent has no memory of your thread with claude-3 (or any other
  ambient context). Everything it knows comes from the three files it
  reads on bootstrap. If the run feels under-grounded, enrich the
  context file and start over.
- The output directory doesn't have to exist; the agent creates it.
- If the agent batches questions, lists multiple tensions, or starts
  writing prose for you, name the invariant violation in the REPL
  ("I3 — one question per turn"; "I1 — no body-writing"). The engine
  will reset.
- For inward flashes (`eoi-inward-mission`,
  `eoi-inward-institution-self-authored`), the sixth slot is a
  **clarity-probe** ("what about this isn't yet clear to you?") rather
  than a diagnostic-probe. Open issues named at closure-offer time MUST
  be tagged before assembly — the engine enforces this.

## Related

- Engine spec: `~/code/algorithms/eoi-engine.md`
- Schema: `~/code/atthangika-buckets.json`
- Parent mission: `~/code/futon5a/holes/missions/M-expressions-of-interest.md`
- Essays browser: `dev/arxana-browser-essays.el`
- Annotation lifecycle (retraction visibility, diachronic graph):
  `holes/missions/M-essays-edit-cycle.md`
- Bell / whistle protocol (for cross-agent coordination outside
  eoi-new): `~/code/futon3c/README-bells-and-whistles.md`
