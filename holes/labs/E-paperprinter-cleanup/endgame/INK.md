# Residual ink census at accepted head 51c23004

Generated from all 8,778 records in
`~/storage/apm-endgame/head-51c23004/q167h-k-*.jsonl` with `ink.py`.

This is a **lower bound**, not yet the final residual percentage.  It covers the
nine open classes with corpus-wide scanners.  The other thirteen open or
unknown classes are known by declarations or occurrence lists and need span
extractors before their ink can enter the union.

Ink here means characters in reader-visible LaTeX source after hidden
`leanref` targets are removed.  A matched occurrence colors its containing
proof paragraph.  It is therefore a measure of affected expository material,
not rasterized glyph area or the literal length of the defective token.

| Class | Color | Records | Marked characters | Total ink |
|---|---:|---:|---:|---:|
| C4 | `#fc8d59` | 69 | 110,067 | 0.827907% |
| C5 | `#fee090` | 2 | 590 | 0.004438% |
| C6 | `#e0f3f8` | 1 | 27,069 | 0.203609% |
| C7 | `#91bfdb` | 6 | 1,713 | 0.012885% |
| C8 | `#4575b4` | 2 | 641 | 0.004822% |
| C16 | `#984ea3` | 2 | 4,540 | 0.034149% |
| C17 | `#4daf4a` | 2 | 230 | 0.001730% |
| N1 | `#ff7f00` | 359 | 458,603 | 3.449541% |
| **Union** | — | **419** | **556,335** | **4.184666%** |
| Total corpus | — | 8,778 | 13,294,610 | 100% |

C1 has zero ink at this head and is omitted from the body of the table.  The
union removes overlaps where more than one class colors the same paragraph.

Generated artifacts:

- `~/storage/apm-endgame/head-51c23004/residual-ink-summary.tsv`
- `~/storage/apm-endgame/head-51c23004/residual-ink-annotations.jsonl.gz`
- `~/storage/apm-endgame/head-51c23004/residual-ink.html` — colored, expandable
  proofs; hover a span to see its class or overlapping classes.
