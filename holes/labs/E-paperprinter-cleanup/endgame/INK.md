# Residual ink census

Generated with `ink.py` from the full 8,778-record census of each **accepted**
integration head (renders under `~/storage/apm-endgame/head-<sha>/`). Candidate
branches are never entered here (see `DISPATCH.md`).

Ink means characters of reader-visible LaTeX source after hidden `leanref`
targets are removed. A matched occurrence colors its containing proof
paragraph (a wall of sufficiency sentences colors the run itself; a known-site
class colors the whole proof). The union removes overlaps. This measures
affected expository material, not glyph area and not the length of the
defective token.

**Status (READINESS.md, Joe 2026-11-22): rendering works; informalisation
validation is in progress.** Ink shows where known classes touch the text. It
is not a percentage of correct or readable proofs, and full markup coverage is
not validation: gate 2 needs each extractor's spans checked against its
inventory, which has not been done, and the eight known-site classes do not
satisfy it at all.

## Complete palette, by accepted head

From 2026-11-22 every open or unknown class of `classes.tsv` has an extractor,
an alias or a stated reason for having no ink. The ledger is `ink.tsv`.

| Head | Records marked | Union ink |
|---|---:|---:|
| 51c23004 | 1272 | 15.158% |
| f79926c0 | 687 | 7.703% |
| 0bf1d207 | 679 | 7.536% |
| 50c42151 | 679 | 7.474% |
| 1eb58e25 | 666 | 7.327% |

## Classes at 0bf1d207

| Class | Records | Ink | How it is found |
|---|---:|---:|---|
| N1 | 161 | 1.658% | text pattern |
| P-sufficiency-wall | 136 | 1.470% | text pattern (4 or more consecutive "It suffices" sentences) |
| C21 | 118 | 1.244% | text pattern |
| C19 | 84 | 1.097% | text pattern |
| C22 | 65 | 0.883% | text pattern |
| C4 | 106 | 0.850% | text pattern (a chain shows the same visible expression on two rows) |
| C18 | 4 | 0.316% | text pattern |
| P-empty-claim | 49 | 0.269% | text pattern |
| P-duplicate-closer | 19 | 0.126% | text pattern |
| P-nonprop-suffices | 2 | 0.106% | known sites only |
| N14 | 1 | 0.105% | known sites only |
| C23 | 10 | 0.088% | text pattern |
| P-overexpanded-setup | 1 | 0.085% | known sites only |
| P-premature-conclusion | 2 | 0.029% | known sites only |
| P-duplicate-tag | 1 | 0.026% | known sites only |
| C12 | 1 | 0.021% | known sites only |
| P-missing-subproof | 2 | 0.015% | known sites only |
| C7 | 1 | 0.001% | text pattern |
| P-opaque-proof | 1 | 0.000% | known sites only |

Classes with zero ink at this head (C1, C5, C6, C8, C16, C17) are omitted.

## What the figure does and does not cover

- **Aliases.** N2 (chain order) is measured by the C4 extractor: its rows are
  the chains that return to an earlier expression. P-empty-chain-reason is
  measured by the N1 extractor (a row with an empty reason).
- **No ink.** N15 is the state of five real-corpus tests, not rendered text.
- **Known sites only.** C12, N14, P-nonprop-suffices, P-missing-subproof,
  P-premature-conclusion, P-opaque-proof, P-duplicate-tag and
  P-overexpanded-setup have no text symptom. They are colored at the
  declarations listed in `ink-sites.tsv` (whole proof). Six of them come from
  one reading of 13 proofs (Q129), so their corpus-wide extent is not known;
  for these the figure is a lower bound. A typed corpus count would need an
  observe packet per class.
- **Text patterns are estimates.** P-sufficiency-wall depends on the
  threshold (4 or more consecutive sentences: 136 records; 3 or more: 285).
  P-empty-claim and P-duplicate-closer match one layout each. C19 and C22
  match printed shapes and may include or miss a few places.
- **C4 on visible text.** 106 records show the same visible expression on two
  rows of a chain; 78 do when hidden link targets are compared too. The
  earlier scanner (27 records) only compared left sides.
- **Paragraph granularity.** One defective row colors its whole paragraph, and
  an itemize block is one paragraph, so the percentage overstates the defective
  text itself and is best read as "share of proof text a reader meets a known
  defect in".

## Rules of use (Joe via codex-18, 2026-11-22)

1. Record every accepted head with `accept_head.py`; compare a candidate with
   `ink_diff.py` from `/tmp`, never record it.
2. Read the colored HTML for new, grown and moved colors, not only the total.
3. A lower total does not authorize a merge that regresses any record.
4. Priority weighs both what blocks the integrate lane and ink share.
5. Keep each head's summary and annotations: they are evidence for the later
   expository grammar. When an extractor changes, re-record the stored heads so
   the history stays comparable.

Artifacts per head: `residual-ink-summary.tsv`,
`residual-ink-annotations.jsonl.gz`, `residual-ink.html`.

## Interoperability with Mark7 / InkUp inks

Residual ink and the Mark7 CMY inks are different axes and never share a
colour namespace. Mark7 marks human source character spans by pipeline layer
(C = S1 marks, M = S3 proof graph, Y = S4 expository scopes). Residual ink
marks PaperPrinter's rendered text by cleanup class.

**What the residual artifacts hold** (per accepted head, beside the renders):

- `residual-ink-annotations.jsonl.gz`: one object per marked record:
  `module`, `declaration`, `visible_chars`, `visible_sha256`, and `spans`, each
  `{class, start, end, anchor, color}`. Offsets are Python string indices into
  `visible(proof)`: the record's `proof` field with `\leanref` targets removed
  (`leanism_scan_p1.visible`). `anchor` is the first 60 characters of the span.
- `residual-ink-summary.tsv` and `ink.tsv`: per class records, characters, share.
- Join key: `(head, module, declaration)`; `declaration` alone is unique in
  the 8,778-record corpus today.

**Offsets do not survive a re-render.** They are valid only for the text whose
`visible_sha256` they were computed on. Across heads use the record key plus
the anchor, or a typed node. The typed walk's Step identities are
process-local, and the only typed anchor that reaches the render is the
`\leanref{..}{local:<declaration>:<binder>}` target in the raw proof, which is
a binder name and not unique (the same name recurs in sibling cases). A stable
node key (declaration plus position in the proof term) does not exist yet.

**Overlay record.** `ink-overlay.schema.json` defines one record per mark
with independent axes (`analytic_layer` S1/S3/S4, `residual_class`,
`presentation_role`, `plan_action`), a coordinate (text span with hash and
anchor, or typed node with a declared stability), evidence, provenance (head,
producer, how found) and review status. `ink_overlay.py HEAD` exports the
residual annotations in that form; `ink_overlay.py --validate FILE` checks any
producer's file. Review status has no "correct" value: the record can say a
span was checked against its inventory, not that a proof is right.
