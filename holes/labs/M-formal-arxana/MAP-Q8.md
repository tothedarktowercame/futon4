# MAP-Q8 — Mathematical fixtures

**Surveyed:** 2026-11-07 by kimi-1 for M-formal-arxana MAP (requisition from claude-13).
Read-only: file reads and greps only; no live-service writes needed.
F3 requirement (mission l.275–283): one small proof with authored informal
prose, readable Lean, generated formal rendering, generated informalisation,
and at least one known partial or contradictory correspondence.

## Answer

Three candidate fixtures, ranked:

**1. APM a93J02 cross-repo chain (largest, richest, mismatch is semantic).**
One proof (Σ(−1)ⁿ/(n+1) = log 2) in five manifestations: authored/generated
informal prose `futon3c/data/apm-informal-proofs/apm-a93J02.md` (137 lines,
identical text in `apm-lean/problems/a93J02/informal-solution.md`); problem
statement `apm-lean/problems/a93J02/problem.md` (22) + `problem.tex` (3) +
`proof-outline.md` (12); readable Lean `apm-lean/problems/a93J02/lean/Main.lean`
(115 lines, 0 sorrys); authored LaTeX `futon3c/resources/arxana/apm-demo/apm-a93J02.tex`
(297 lines); generated formal rendering `…/apm-demo/index.html` (latexml_oxide
0.7.5, producer log retained). The md's §2 carries an all-`sorry` Lean sketch
(partial formalisation, superseded by Main.lean). Mismatches: (a) strategy
contradiction — informal header "via dominated convergence", informal Step 2
"By the dominated convergence theorem", but Lean header l.4 "Apply the dominated
convergence theorem (via Abel's limit theorem)" and the actual proof uses
`tendsto_tsum_powerSeries_nhdsWithin_lt` (Abel, Main.lean l.85); no
`tendsto_integral_of_dominated_convergence` anywhere (grep). (b) The informal
"key insight" paragraph argues itself into a dead end and retracts ("…not
uniformly bounded. The right approach is different.") while the Proof uses the
rejected dominator — an authored internal contradiction. (c) `status.json`
`"sorry_count_main": 0` vs its own detail "Main theorem has 1 sorry in the
Abel's theorem application" (`grep -c sorry Main.lean` → 0).

**2. futon5 draft8 arxana-fixture (mid-size, contradiction is provenance-real).**
`futon5/holes/tech-notes/paper/html-build/wysiwyg/arxana-fixture/` (fixture id
`m-jvm-arxana/draft8-v1`, gate commit `17dc53a`): authored `draft8.tex` (1281
lines), generated formal rendering `intro-generated.tex` (40 lines, header
"GENERATED FROM intro.edn by scripts/render_intro.clj — DO NOT EDIT"; source
`paper/intro.edn`, 202 lines, argument structure as data) and `draft8.html`
(latexml, 2084 lines, 1630 anchors). Contradiction fixture retained:
`scopes-negative-v1.json` — "Its `source-sha` values describe `.pristine`
bytes, not the sources the page was built from. The gate MUST reject it with
`fixture/revision-mismatch`" (README.md); positive `scopes.json` notes
"index 3: unmapped (synthesised source…)" — honest ambiguity. Verified by
`futon3c/arxana/correspond_fixture.clj` (implemented, exercised by
`run-tests.sh`). No Lean; mismatch is byte-provenance, not mathematical meaning.

**3. futon3c apm-demo two-manifestation fixture (smallest, mismatch declared
partial).** `apm_fixture.clj` + `resources/arxana/apm-demo/manifest.json`:
11 scopes, 3 `:verified` (subsections) / 8 `:partial` (paragraphs, longest
shared word-run ≥ 4); math-only and too-short paragraphs deliberately
unregistered ("no invented symbol-level semantics", ns doc). Smallest honest
partial correspondence, but only two manifestations and no contradiction —
a sub-slice of candidate 1, not independent.

"Mark8" and "EmitPython": not found as artifacts or directories (searched
`grep -rli -E "Mark8|EmitPython|informalis"` over futon5, futon2/futon3c/futon4
holes, apm-lean; only wm-contract log mentions of the token "Mark8"). The
generated-formal and generated-informal roles above are played by
latexml_oxide, render_intro.clj, and the LLM-authored informal md.

## Evidence

- `futon3c/src/futon3c/arxana/apm_fixture.clj` l.1–27 (census + honest ranking of scope verification), l.33 `fixture-dir "arxana/apm-demo"`.
- `python3 -c … manifest.json` → `scopes: 11`, `Counter({'partial': 8, 'verified': 3})`; `wc -l apm-a93J02.tex index.html` → 297/297.
- `futon3c/src/futon3c/arxana/correspond_fixture.clj` l.1–9 (fixture doc), l.17 gate-commit `17dc53aa3904a0fa2de6fe2d6b57fd1f467aa89a`, l.63–71 `verify-committed!` (throws `:fixture/not-committed-gate`).
- `futon5/…/arxana-fixture/README.md` l.5–10 (negative scope table quote); `scopes.json` head ("generated-by make-scope-table.py", "index 3: unmapped"); `wc -l draft8.html` → 2084.
- `apm-lean/problems/a93J02/lean/Main.lean` l.4, l.85; `grep -c sorry` → 0; `status.json` `"sorry_count_main": 0` + classification_detail quote.
- `futon3c/data/apm-informal-proofs/apm-a93J02.md` l.7 (DCT header), Step 2 quote, all-sorry Lean block l.50–110.
- `futon5/holes/tech-notes/paper/html-build/wysiwyg/sandbox/intro-generated.tex` l.1–2 (generated-from header); `paper/intro.edn` 202 lines; `paper/scripts/render_intro.clj` exists.
- M-latex-wysiwyg `futon3c/holes/missions/M-latex-wysiwyg.md` l.28–48: latexml `--source-map` ground truth — 80 % of anchors are degenerate points, macro expansions carry no anchor (why scope correspondence is necessarily partial).

## Ready — no new code | Missing — actual work

| Ready — no new code | Missing — actual work |
|---|---|
| Candidate 1 is a complete four-plus-manifestation chain with a real semantic contradiction (DCT vs Abel) and a partial-formalisation layer (all-sorry sketch), all bytes on disk and hashable today | No generated informalisation *of the Lean* exists for any candidate (informal prose predates or ignores Main.lean); the F3 "generated informalisation" leg has no artifact — nothing found by `grep -rli informalis/informaliz` across futon5/apm-lean |
| Candidate 2 already has a gate-exercised contradiction fixture (negative scope table) and revision-pinned verification in exercised code (`correspond_fixture.clj`, `run-tests.sh`) | Correspondence between the two candidates' manifestations is recorded as scopes/manifests (bytes↔DOM) but there is no cross-repo assertion linking the a93J02 md/tex/html to apm-lean's Main.lean at all — the Lean manifestation is unjoined |
| Candidate 3 gives the smallest honest *partial* correspondence (8 declared `:partial` scopes, math deliberately unregistered) | Contradictions found are quoted in this survey but recorded nowhere as typed conflict records; candidate 1's status.json self-contradiction is latent, not asserted |

## Open doubts

- Whether the a93J02 informal md counts as "authored" or "generated informalisation": it reads as model output (self-correcting paragraphs, Mathlib census); its provenance is not recorded in the file.
- Whether the duplicated informal text (futon3c data vs apm-lean informal-solution.md, byte-similar 137 lines each) is a transclusion or a drifted copy — not diffed this session.
- "Mark8" in the question may name the futon5 paper project by an alias; if so, candidate 2 is the Mark8 leg, but no file confirmed the name.
- draft8 fixture `scopes.json` lists a 4th `null` file index (generated bibliography) — usable as an *unmappable manifestation* case, not yet assessed.
