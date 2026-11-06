# I1 — fixture gate report

Mission: `M-jvm-arxana`. Phase: INSTANTIATE, increment 1 (fixture gate).
Date: 2026-11-06. Executor: kimi-1 (requisition from codex-21).

## Verdict

**PASS.** The draft8 manifest builder is repaired, the negative
revision-mismatch rejection test is committed, and a rebuilt coherent positive
fixture covers all three normative D4 cases. Same-basename files under the
wrong root are rejected at both builder and checker level.

## What was done

### Builder repair (futon5 `17dc53a`)

`holes/tech-notes/paper/html-build/wysiwyg/make-scope-table.py`:

- `resolve_from_log` no longer reduces oxide's `Info:source-map:source` paths
  to `Path(...).name`; the full producer-logged path is kept (the V3 S2 root
  cause).
- New `resolve_producer_path`: relative log paths resolve against an explicit
  `--producer-cwd` (the directory rebuild-page.sh runs oxide from); the
  resolved path must lie inside `--build-root` (default `--src`). A path
  outside the root yields a null mapping plus a `REJECTED ... outside allowed
  build root` note — never a basename substitution.
- `source-sha` hashes the exact bytes at each producer-resolved path;
  `src-dir` is the build root and `files` entries are paths relative to it,
  so `src-dir` + entry denotes the hashed bytes exactly.
- The fingerprint/`--input-order` fallback path is unchanged in behaviour and
  still populates hashes (regression-checked against a copy of draft9.html:
  2404 anchors, 4 files, hashes populated).

### Fixture artifacts (`.../wysiwyg/arxana-fixture/`)

- `scopes-negative-v1.json` — the original contradiction-bearing scope table,
  copied verbatim; the live `sandbox/scopes.json` was not overwritten (D4
  procedure step 6).
- `draft8.html` + `build.log` (untracked, reproducible) — clean rebuild:
  `latexml_oxide 0.7.5 --source-map --path=html-build
  --preload=apa7-html-shim.sty --dest=<fixture>/draft8.html
  html-build/wysiwyg/sandbox/draft8.tex` from the paper root. 1053 anchors, 3
  mapped sources. (10 pre-existing `Error:undefined` tikz/csquotes diagnostics,
  identical class to the original build; bibliography is inlined and carries
  no source index in this build.)
- `scopes.json` — positive map. Hashes now match the live sandbox bytes:
  `draft8.tex de7d8f8ac0bc2440`, `intro-generated.tex bccb3369c12b3cc0`,
  `part3-exotype.tex 3983ea06ae49f13f` — the revisions D4's manifest names.
- `manifest.json` — D4 fixture manifest v1 with full sha256 revision IDs,
  single build ID `arxana-build:sha256:7ae3821ffcf2265d`, tool version and
  source commit `245773fef526c36b040576add694ce774afb46f3`.
- `check-fixture.py` — the gate (see below). `run-tests.sh` — four tests.
- Page assets (`*.png/svg/css`) oxide copies next to `--dest` are git-ignored
  as duplicates of `page/`.

## Gate checks and results

`run-tests.sh` — **ALL FIXTURE GATE TESTS PASS**:

1. **Negative rejection:** `scopes-negative-v1.json` is rejected with three
   `fixture/revision-mismatch` errors (recorded `.pristine` hashes vs actual
   bytes), exit 1. Not warning-only (D4 tripwire 1).
2. **Positive coherence:** rebuilt `scopes.json --cases` → `COHERENT ... 1053
   anchors, 3 hashed sources, 3 normative cases`, exit 0:
   - *paragraph point:* `<p class="ltx_p" data-sourcepos="1:10:2-1:10:2">` —
     point anchor on `intro-generated.tex` line 10, no DOM id, leading literal
     quote "The object of study is a finite family" verified at source; no
     invented end.
   - *section range:* `<section id="S2" ... data-sourcepos="0:316:1-0:316:21">`
     ↔ `draft8.tex:316` `\section{The Object}`; DOM id treated as locator
     evidence only.
   - *math transformation:* `<math id="S1.p2.m2"
     data-sourcepos="1:10:221-1:10:227" alttext="P_{\sigma}">`; the named
     source range contains the `\sig` macro while the alttext shows the
     expansion `\sigma` — directional transformation, reverse literal edit
     unsupported.
   All 1053 anchors additionally pass file-index and line/column bounds
   checks against the mapped bytes.
3. **Wrong-root rejection:** a fabricated producer log naming the paper-root
   `draft8.tex` (same basename) with `--build-root sandbox` → mapping null +
   `REJECTED ... outside allowed build root`; control run with the paper root
   allowed accepts and hashes it (D4 tripwire: same-named file outside the
   producer's resolved source path is rejected even when counts look valid).
4. **Path escape:** a scope entry `../../../../draft8.tex` is rejected with
   `fixture/path-escape` by the checker.

## Commands (exact)

```sh
cd /home/joe/code/futon5/holes/tech-notes/paper
latexml_oxide --source-map --path=html-build --preload=apa7-html-shim.sty \
  --dest=html-build/wysiwyg/arxana-fixture/draft8.html \
  html-build/wysiwyg/sandbox/draft8.tex \
  > html-build/wysiwyg/arxana-fixture/build.log 2>&1   # exit 0
python3 html-build/wysiwyg/make-scope-table.py \
  html-build/wysiwyg/arxana-fixture/draft8.html \
  --src html-build/wysiwyg/sandbox --main draft8.tex \
  --build-root html-build/wysiwyg/sandbox --producer-cwd "$PWD"
html-build/wysiwyg/arxana-fixture/run-tests.sh          # ALL PASS, exit 0
```

## Scope notes

- Work confined to futon5 (this report excepted). No live stores touched; the
  live `page/draft8.html`, `sandbox/scopes.json` and `sandbox/.pristine/` are
  untouched. Unrelated working-tree changes preserved (tree was clean under
  `holes/tech-notes/paper`).
- The committed manifest's `built-at-commit` predates the fixture commit by
  construction; artifact revision IDs are over bytes, not git state.
- D4's "one build ID for all anchors" is recorded in the manifest (single
  build field); per-anchor build tagging is producer metadata oxide does not
  emit — noted, not blocking.

## Commits

- futon5 `17dc53a` — builder repair + fixture gate (9 files).

Gate holds; no blockers. Client demos remain gated on later increments per D7.
