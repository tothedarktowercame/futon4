# Residual ink census

Generated with `ink.py` from the full 8,778-record census of each **accepted**
integration head (renders under `~/storage/apm-endgame/head-<sha>/`). Candidate
branches are never entered here.

Ink means characters of reader-visible LaTeX source after hidden `leanref`
targets are removed. A matched occurrence colors its containing proof
paragraph, so this measures affected expository material, not glyph area and
not the length of the defective token. The union removes overlaps.

It is a **lower bound**: only classes with a corpus-wide scanner are colored.
The `COVERAGE` row of each summary says how many open or unknown classes of
`classes.tsv` have an extractor.

## History (accepted heads only)

| Head | Extractors | Records marked | Union ink | N1 | C4 | C21 | C22 |
|---|---|---:|---:|---:|---:|---:|---:|
| 51c23004 | 9 (C1 C4–C8 C16 C17 N1) | 419 | 4.184666% | 3.449541% | 0.827907% | — | — |
| f79926c0 | 11 (+C21 C22) | 359 | 3.917859% | 1.657788% | 0.226068% | 1.237274% | 0.882294% |
| 0bf1d207 | same 9 | 184 | 1.855454% | 1.658308% | 0.226139% | — | — |
| 0bf1d207 | 11 (+C21 C22) | 360 | 3.925537% | 1.658308% | 0.226139% | 1.244111% | 0.882571% |

Reading the table: between 51c23004 and 0bf1d207 the nine-extractor union fell
from 4.18% to 1.86%. The rise to 3.93% in the eleven-extractor rows is two
classes becoming visible (C21 "a library lemma", C22 a numeral compared with
itself), not new defects. The +1 record from f79926c0 to 0bf1d207 is aunk03,
where Q175a dropped a no-op row and exposed a citation without a reading
(Q190; the reading is committed as data and prints once Q181 lands).

Known undercount: the C4 extractor colors blocks with a repeated left side
(27 records). The row-level metric used in review finds 89 repeated rows; the
difference is rows that return to an earlier expression on the right side.

## Classes without an extractor yet

C12, C18, C19, C23, N2, N14, N15 and the ten P-* classes. C18 and C19 have
occurrence lists from their packets; C23 waits on Q192; the P-* classes have
declaration inventories only. Each gets an interval extractor when its
inventory is next touched, and a new history row is added for the head at
which it first appears.

## Rules of use (Joe via codex-18, 2026-11-22)

1. Regenerate at every accepted head and record the delta beside the class
   counts; never from an unaccepted candidate.
2. Read the colored HTML for new, grown and moved colors, not only the total.
3. A lower total does not authorize a merge that regresses any record: the
   preflight and the read of every changed line still decide.
4. Priority weighs both what blocks the integrate lane and ink share.
5. Keep each head's summary and annotations: they are evidence for the later
   expository grammar.

Artifacts per head: `residual-ink-summary.tsv`,
`residual-ink-annotations.jsonl.gz`, `residual-ink.html`.
