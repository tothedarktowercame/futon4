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
| 51c23004 | 1272 | 15.086% |
| f79926c0 | 688 | 7.682% |
| 0bf1d207 | 680 | 7.514% |
| 50c42151 | 680 | 7.453% |
| 1eb58e25 | 667 | 7.306% |
| 89d58eb5 | 574 | 6.271% |
| 2e3d5f5e | 574 | 6.271% |
| 0398a663 | 538 | 5.941% |
| 57d47d46 | 539 | 5.991% |
| 905efba6 | 498 | 5.124% |
| faeab149 | 427 | 4.314% |
| 4168ba30 | 391 | 4.032% |
| bac048b8 | 380 | 3.886% |
| 91d38c1e | 380 | 3.881% |
| b0a112b7 | 370 | 3.785% |

Rows were re-recorded on 2026-11-22 after the C18 pattern was widened to negative numerals with spaces
("if - 1 = 1"), which Q209 found in one record the old pattern missed. Every head from f79926c0 on gained
that record. They were re-recorded again the same day after Q214 showed that the two known sites of
P-nonprop-suffices no longer show that defect (typed check, four modules); the sites were removed from
`ink-sites.tsv`, which had coloured both whole proofs at every head. The class stays unknown until a corpus-wide
typed count.

## Classes at 0bf1d207

| Class | Records | Ink | How it is found |
|---|---:|---:|---|
| N1 | 161 | 1.658% | text pattern |
| P-sufficiency-wall | 136 | 1.470% | text pattern (4 or more consecutive "It suffices" sentences) |
| C21 | 118 | 1.244% | text pattern |
| C19 | 84 | 1.097% | text pattern |
| C22 | 65 | 0.883% | text pattern |
| C4 | 106 | 0.850% | text pattern (a chain shows the same visible expression on two rows) |
| C18 | 5 | 0.369% | text pattern |
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

At 57d47d46 (Q224) the figure rises by 0.050 points because 29 proof walks that were silently truncated now
finish: about 34,000 characters of proof text became visible, and with them one record newly marked C17 and C7
and one newly marked N1. The marks are existing classes in text that was hidden before.

At 905efba6 (batch 1: Q210, Q200, Q203, Q185, Q225) ink_diff against 57d47d46 gives GONE 50 (C19 33, C22 16,
C18 1), MOVED 66 (P-sufficiency-wall 62, N1 3, C18 1) and NEW 1: C4 in t99J05_lefschetz_euler_inter, which is the
same span formerly counted as C18 (the selected case arm leaves "1 = 1 since 1 = 1", Q191). The extractor does not
count a sufficiency sentence that repeats the target of the one before it; such pairs rose from 15 to 316 in 260
records with Q203 and are Q201's scope, so this head's figure understates that class.

At faeab149 (batch 2: Q211, Q186, Q201, Q220, Q219) ink_diff against 905efba6 gives GONE 89 (P-sufficiency-wall 86,
C18 1, C19 1, P-empty-claim 1), SHRUNK 4, MOVED 36 and NEW 18, all N1: each is a calc block recovered by Q219 from a
transport argument, absent at 905efba6, one of whose rows has an empty reason cell. No record GROWN. The wall class
falls from 1.402% in 136 records to 0.481% in 50; repeated-target sufficiency pairs fall from 300 to 27. Not measured
by the extractor: 49 recovered calcs follow an unrelated sentence with no lead-in and 11 follow the closer they support.

At 4168ba30 (batch 3: Q191, Q229, Q188, Q213; Q205 dropped) ink_diff against faeab149 gives GONE 39 (C4 27, C19 11,
C18 1), MOVED 7 (parentheses added by Q229 shift the marked span by a few characters) and NEW 1:
apm_t00a05_squarePerimeter_squareOfPerimeter, C4. Q213 decides the closed condition `-1 = 1`; a following display
then loses its first row and its rows continue the previous chain, which at faeab149 already chained selections from
different branches without being marked. Follow-up Q238 (observe). No record GROWN. Q205 (the fallback makes no
bridge between a term and a proposition) was in the first trial merge and was removed: in the census it joined
unrelated displays and produced "since P iff P" sentences (Q237 report; redo Q239). Not measured by the extractor:
apm_a03J04_mul_ofReal_rpow_eq now has a proof with setup only, its single `X = X` display being removed; its statement
prints two different typed terms identically.

At bac048b8 (batch 4: Q233, Q234, Q236; Q197 and Q239 dropped) ink_diff against 4168ba30 gives GONE 14 (all N1: rows
that had an empty reason now carry a reading from rewrite-readings.json), MOVED 1 (apm_m98a04_translation_pairing_
fderiv_of_stage_dq_aux_hasFDerivAt: one of its empty reasons is filled, another remains), NEW 0, GROWN 0. Q233 changes
three records (the recovered calc is printed before the "Hence" sentence it supports); none was marked. Q234 changes
nothing with the robformat-2 library. Q197 (argument-position contradiction blocks) was in the first trial merge and
was removed: 2 proofs crashed and 141 records changed, 90 of 219 blocks restating the assumption (redo Q243). Q239
(mixed bridge kept as an unprinted boundary) was in the second trial merge and was removed: its one changed record,
apm_a99J08_rectBoundary_split_real, became a display in which equations are chained to terms (Q244 report; redo Q245).
Not measured by the extractor: that head record's two "It suffices to show P, since P iff Q" sentences are counted as
sufficiency wall, but the failed candidate's term-to-equation rows would not have been marked by any class.

At 91d38c1e (batch 5: Q235, Q240, Q243 and a review fix; Q208 dropped at census q238) ink_diff against bac048b8 gives
NEW 0, GROWN 0, MOVED 7 (C4 3, P-empty-claim 2, C23 1, N14 1), GONE 1 (C16: the a01A04 closer no longer repeats its
lemma tag). 144 records changed; 143 of them gain 225 contradiction blocks ("Suppose P. ... This is absurd, by (n).").
The MOVED records were read: the block text falls inside paragraphs that were already painted. Reading them found 183
list items holding only a nested list (census q239), removed by the review fix (16 remain where the block is the first
of several items). Not measured by ink and still open: blocks whose Suppose line is a derived fact, and "This is
absurd, by (n)" where (n) is not contradictory by itself (a94A06 upper_ne_pole; t96J07 inv_cont) -> observe Q251.

At b0a112b7 (91d38c1e + Q246, and the census library changes from robformat-2 to robformat-3, so this row differs from
the one above by library content as well as by one reading) ink_diff against 91d38c1e gives NEW 0, GROWN 0, MOVED 5,
GONE 10 (N1 7: rows gain a reason from the library; C16 3). 101 records change, the same set as the library-only
census q237 on bac048b8 less a01A04 (already changed by Q235): closers gain "Hence X, since A and B". No raw
Metric.ediam. Open from this change: closer factors that print alike though typed differently (Q249) and elementary
nonnegativity factors (Q250), neither marked by ink.
