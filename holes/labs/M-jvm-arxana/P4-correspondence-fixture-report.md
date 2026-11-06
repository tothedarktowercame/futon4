# P4 — first cross-manifestation correspondence fixture (report)

Mission `M-jvm-arxana`, phase MAP, packet `packets/P4-correspondence-fixture.md`.
Discovery and fixture recommendation only: nothing was regenerated, no LaTeX was
edited, the WYSIWYG implementation was not changed, and no correspondence IDs
were invented. Where a field does not exist in the artifacts, it is recorded as
`absent`.

## Principal finding

**The committed Oxide pair — `futon5` commit `245773f`, `sandbox/*.tex` +
`page/draft8.html` + `sandbox/scopes.json` — already carries file identity,
revision identity, quote-verifiable source ranges and DOM ids across all three
editability classes, and it exposes a real, reproducible revision-identity
defect (the recorded source hashes describe the `.pristine` copies while the
page's anchors verify against the live sandbox sources), which is exactly the
mission risk a first correspondence fixture should test; it is recommended over
an Elisp-definition pair, whose derived artifacts (`.elc`) are untracked build
outputs with no stable DOM-equivalent identity.

## 1. The path traced

Build chain (all read, not run): `futon3c/emacs/latex-wysiwyg.el` (WYSIWYG
client; consumes `scopes.json` via `latex-wysiwyg-load-map`, line 64) →
`futon5/holes/tech-notes/paper/html-build/wysiwyg/rebuild-page.sh` (invokes
`latexml_oxide --source-map`, atomically installs the page, then runs
`make-scope-table.py`) → `make-scope-table.py` (resolves oxide's opaque file
indices from the `Info:source-map:source [i]` log lines, writes `scopes.json`
with `files`, `source-sha`, `anchor-count`, injects the client script) → the
Emacs map `latex-wysiwyg--file-map` used by `latex-wysiwyg--goto` (line ~167).
The scope-view side (`futon3c/web/scopes/`, fixtures such as
`mark7-0708.2185.json` with `offset_unit:
"unicode-codepoints-zero-based-end-exclusive"`) carries quotes and offsets but
no source-position correspondence to LaTeX; it is the projection layer, not the
correspondence layer.

Fixture ground truth, all committed in `futon5` at
`245773fef526c36b040576add694ce774afb46f3` (2026-08-09, "paper: release
candidate"):

| Artifact | sha256 (first 16) |
|---|---|
| `…/wysiwyg/sandbox/draft8.tex` | `de7d8f8ac0bc2440` |
| `…/wysiwyg/sandbox/intro-generated.tex` | `bccb3369c12b3cc0` |
| `…/wysiwyg/sandbox/part3-exotype.tex` | `3983ea06ae49f13f` |
| `…/wysiwyg/sandbox/.pristine/draft8.tex` | `988a1166f6841008` |
| `…/wysiwyg/sandbox/.pristine/intro-generated.tex` | `c502ab28542b02cc` |
| `…/wysiwyg/page/draft8.html` | `ceab935f175e9fd3` |
| `…/wysiwyg/sandbox/scopes.json` | `1a63f482876808d4` |

`sandbox/scopes.json` (built for `page: draft8.html`) maps indices
`0=draft8.tex, 1=intro-generated.tex, 2=part3-exotype.tex, 3=null`
(synthesised bibliography), records `source-sha` `draft8.tex:
988a1166f6841008`, `intro-generated.tex: c502ab28542b02cc`,
`part3-exotype.tex: 289abcd84d90f992`, anchor-count 1630.

**Observed discrepancy (reproducible without rebuild):** every anchor checked
below verifies *by quote* against the **live sandbox** sources, but the
`source-sha` values match the **`.pristine/`** copies — different files with
different line numbering (`\section{The Object}` is at line 316 in
`sandbox/draft8.tex` and line 334 in `.pristine/draft8.tex`; `\part*{Discussion}`
at 1102 vs 1114). So hash-based staleness checking (`latex-wysiwyg-stale-sources`)
and quote-based anchor verification describe **different source revisions** for
the same page. Not resolved here — recorded as a fixture fact. The practical
correspondence identity on this pair is therefore: git commit + quote, not the
recorded hash.

## 2. Three traces

### Trace 1 — prose paragraph (Class A)

- HTML: `<p class="ltx_p" data-sourcepos="1:10:2-1:10:2">The object of study is
  a finite family of rule-rewriting operators admitting exact classification
  rather than sampling. …` (`page/draft8.html`).
- Source: `sandbox/intro-generated.tex` line 10, column 2 = first character of
  the paragraph; quote verifies verbatim (first sentence identical; later
  `Figure \ref{fig:fig2pair}` is rendered resolved, so only the leading literal
  prose is a verbatim image).
- Preservation matrix:

| Field | Value |
|---|---|
| File identity | `intro-generated.tex` via scopes.json index 1 (from oxide log) |
| Revision/hash | git commit `245773f`; scopes.json sha says `c502ab28542b02cc` (= `.pristine`), live file is `bccb3369c12b3cc0` — **discrepant** |
| Source range | start `1:10:2`; **end absent** (degenerate point `10:2-10:2`, the 80 % case from the mission doc) |
| Quote | partially available: leading literal prose verbatim; `\parencite`/`\ref` render resolved |
| DOM identity | **absent** — the `<p>` has no `id` attribute, only `class="ltx_p"` |
| Semantic identity | **absent** — no entity/scope id; Emacs derives the paragraph end (`forward-paragraph`) at runtime |
| Generated/macro status | file is named `-generated` but has no GENERATED header; `latex-wysiwyg--generated-p` returns nil for it (checked by header heuristic, per source) |
| Editability class | A (literal body prose), with embedded Class B/C runs |

### Trace 2 — section title (Class B)

- HTML: `<section id="S2" class="ltx_section" data-sourcepos="0:316:1-0:316:21">`
  … `<h2 …>`The Object"` (title text verified in `page/draft8.html`).
- Source: `sandbox/draft8.tex:316` = `\section{The Object}`.

| Field | Value |
|---|---|
| File identity | `draft8.tex` via index 0 |
| Revision/hash | commit `245773f`; scopes.json sha `988a1166f6841008` = `.pristine`, live file `de7d8f8ac0bc2440` — **discrepant** |
| Source range | true range `0:316:1-0:316:21` (start ≠ end, as the mission doc predicts for `section`) |
| Quote | verbatim: rendered title "The Object" is the literal argument of `\section{...}` |
| DOM identity | `id="S2"` — present but **build-local** (stable only within this page build; not a durable entity id) |
| Semantic identity | the Essays join would use a `section-id`; none recorded in these artifacts — **absent** here |
| Generated/macro status | not generated |
| Editability class | B — argument-editable only |

### Trace 3 — mathematical expression (Class C/B)

- HTML: `<math id="S1.p2.m2" class="ltx_Math" data-sourcepos="1:10:221-1:10:227"
  alttext="P_{\sigma}" display="inline">` (`page/draft8.html`).
- Source: `sandbox/intro-generated.tex:10`; bytes at columns 219–226 are
  `$P_\sig$`. The recorded range 221–227 covers `_\sig$` — the macro body plus
  closing delimiter — a two-column offset from the naive `$…$` span; the range
  is a real range (start ≠ end), as predicted for `math`.

| Field | Value |
|---|---|
| File identity | `intro-generated.tex` via index 1 |
| Revision/hash | commit `245773f`; same scopes.json-vs-live discrepancy as trace 1 |
| Source range | range `1:10:221-1:10:227`; interior columns addressable only as a whole |
| Quote | source form `$P_\sig$` ≠ rendered `alttext="P_{\sigma}"` — the `\sig` macro is **expanded** in the manifestation |
| DOM identity | `id="S1.p2.m2"` — build-local, compositional (paragraph 2, math 1 of section S1) |
| Semantic identity | **absent** |
| Generated/macro status | macro-expanded (`\sig` → `\sigma`); the rendered form is not a verbatim image of the source span |
| Editability class | C in the rendered form; TeX-editable in source (Class B at best, per mission doc) |

## 3. Preservation matrix (summary)

| Identity | Survives? | Carrier |
|---|---|---|
| File identity | yes | scopes.json `files[]` + oxide log lines |
| Revision/hash | yes but **contradicted** | git commit; scopes.json `source-sha` vs live bytes disagree (`.pristine` hashes) |
| Source range start | yes, all three classes | `data-sourcepos` |
| Source range end | sections/math only; paragraphs degenerate | `data-sourcepos` |
| Quote | partially | DOM text vs source; macro/cite/ref runs are not verbatim |
| DOM identity | sections/math only; `<p>` absent | build-local `id` attributes |
| Semantic (entity) identity | **absent everywhere** | nothing in the chain mints one |
| Generated/macro status | derivable by heuristics (`-generated` name, GENERATED header, alttext-vs-source diff); not recorded | absent as a field |

## 4. Two candidate strategies

**Candidate O — the Oxide draft8 pair** (recommended): sources
`sandbox/{draft8,intro-generated,part3-exotype}.tex`, page
`page/draft8.html`, table `sandbox/scopes.json`, all committed at `futon5`
`245773f`; inspected above. Another worker reads four files and one commit and
can re-verify every claim here by grep, no rebuild.

**Candidate E — Elisp-definition pair**: source = a defun in a committed `.el`
(e.g. `latex-wysiwyg-load-map` in `futon3c/emacs/latex-wysiwyg.el`, file sha
`254a54ae6dd2f190`, last commit `e062f18e`), derived manifestation = the
byte-compiled `.elc`. Checked: **no `.elc` in `futon3c` or `futon4` is tracked
by git** (`git ls-files '*.elc'` is empty in both; e.g.
`futon4/dev/arxana-browser-missions.elc`, dated Nov 2, is untracked). The
derived side of this pair is a build output that another worker cannot inspect
from the repository without rebuilding — which the acceptance criterion
forbids. Its correspondence is also trivial: symbol name ⇒ definition position,
with no offsets, no quote ambiguity, no macro expansion, no per-build DOM ids —
it tests the entity-identity invariant the JVM side gets almost for free, and
none of the manifestation-correspondence risks that the Correspond slice is
supposed to prove.

**Recommendation: Candidate O.** It is committed, rebuild-free, and it tests
the actual mission risks: quote-vs-hash revision identity (the `.pristine`
discrepancy above is a live instance), degenerate paragraph anchors, build-local
DOM ids with no semantic identity, and macro-expanded Class-C text. Candidate E
should be revisited only when a *tracked* Elisp-derived artifact (e.g. a
committed fixture naming defuns) exists.

## 5. Q4 ready / missing

| Prerequisite for the correspondence fixture | Status |
|---|---|
| Committed LaTeX source + derived HTML pair | **ready** (futon5 `245773f`) |
| Source-position anchors in the derived artifact | **ready** (`data-sourcepos`, 1630 anchors) |
| File-index → file resolution record | **ready** (scopes.json + oxide log) |
| Revision identity of the pair | **ready with a caveat** — git commit is authoritative; recorded hashes contradict the live bytes |
| Semantic/entity identity on either side | **missing** (no correspondence IDs exist; this packet was forbidden to invent any) |
| Durable DOM identity | **missing** (ids are build-local) |
| Tracked Elisp-derived artifact for candidate E | **missing** (`.elc` untracked in both repos) |
