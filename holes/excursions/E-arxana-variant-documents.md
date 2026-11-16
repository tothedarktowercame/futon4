# E-arxana-variant-documents — one author's clips, edited into variants, kept linked across time

**Status:** IDENTIFY (opened 2026-11-16 by claude, from a conversation with Joe).
**Owner:** Joe (operator). Technical driver not yet assigned.
**Home mission:** M-repl-circus (M-formal-arxana closed 2026-11-16 and handed its
practical work there).
**Relates:** M-formal-arxana F0 (edit robustness), F2 (collaborative editing),
F4 ('clusion composition) · E-essay-versioning-ux (essay variant lineage) ·
Nipper (zai-13 design, 2026-11-14; not built) · 狗 (buried verbatim quotes).
**Pressure test:** the Mozilla Fellowship LOI (deadline 2026-12-17), rearranged
from the incubator draft.

## HEAD

### Operator-voice anchor (Joe, 2026-11-16, this conversation)

> "Arxana would retain links, editing a normal markdown file does not"

> "if I am editing my own clips, then the "drift" isn't less-me!  But Arxana
> needs a way to deal with the complexity.  Google Docs, Etherpad, OT, CRDT are
> all somewhat relevant but not quite what we need."

> "Arxana *is* an implementation of Xanadu, the problem is that we just haven't
> seen the full power of it yet."

> "this clipping was created at time T, but edited into that variant at time
> T1, and copy-pasted into this other document at time T2"

### The concrete failure

`~/mozilla-incubator-draft-2.txt` (2026-11-16) was compiled from clips of Joe's
own turns and then edited by Joe as a flat text file. Measured 2026-11-16
against the operator-turn dump (`~/storage/operator-turns/`, 23,158 turns,
2026-02-18 to 2026-11-11) and the 533 most recent 象 operator records: of its
50 sentences, **0** appear verbatim in any source; **9** share an 8-word run
with a recent turn (the cell-line story, "trace the value back", the $300k
paragraph); **41** have no recoverable source. The clipping worked; the links
were lost at the first edit, because a markdown file has nowhere to keep them.
A "draft-2b" (same material rearranged for the Fellowship's six questions
instead of the incubator form) could not be produced as a parallel text.

### Why not OT / CRDT / Google Docs / Etherpad

Those make many concurrent editors converge on one document. This case is the
opposite: one author, one meaning, deliberately divergent renderings (one per
target form and word limit) that must stay linked. M-formal-arxana F2 already
says a CRDT/OT substrate is warranted "only if two legitimate histories must
survive without an accepted sequencer"; here the author is the sequencer.
Closer models: Xanadu (transclusion plus versions, shown as parallel text, which
Arxana implements) and patch-based version control (Pijul, Darcs: patches as
first-class objects with dependencies; conflicts kept as explicit alternatives,
as futon3c `text_rebase.clj` already does).

## 1. IDENTIFY

### Two things that must not be conflated

- **Provenance:** who produced this text and when. Every edit Joe makes to his
  own clip is another dated act of Joe's. Editing does not make a passage less
  his.
- **Correspondence:** does this text still match that span of that source?
  (futon3c `correspond.clj` standing: `:verified` … `:stale`.) This measures
  distance from one dated utterance, not distance from Joe's voice.

### Time model (XTDB 2, bitemporal)

Joe's example, as records. Each is immutable; none rewrites another.

| Act | Valid time (when it happened) | Example |
|---|---|---|
| utterance | T0 | Joe says it in a REPL turn, 2026-08-27 |
| clip | T | a span of that turn is taken as a clip |
| edit | T1 | Joe edits the clip into the incubator variant |
| place | T2 | the variant is copied (or transcluded) into the Fellowship LOI |

System time is when each record entered the store. The two differ in practice:
the operator-turn dump will be imported now (system time 2026-11) with valid
times from February onward. Questions this answers:

- the LOI as it stood on a given date (as-of on valid time);
- everywhere a given utterance has travelled, in order;
- what Joe had said about X by date D (the m40 `asof` question, over turns);
- a corrected source (e.g. a speech-to-text mishearing fixed later) keeps its
  valid time while system time records the correction.

### Candidate records (to be checked in MAP, not settled)

- `source/turn`: id, text digest, valid time T0. Imported lazily: a turn is
  registered on first citation, not all 23,158 up front.
- `act/clip`: source + locator (codepoint range + quote context), at T.
- `act/edit`: parent version(s), patch, author, at T1, optional reason (e.g.
  the slot's word limit forced it). A passage is a DAG of versions, not a
  single text: two edits of one clip for two forms are siblings with a common
  ancestor.
- `act/place`: passage version into an arrangement slot, at T2, mode
  `:transclude` (follows later versions) or `:copy` (pins this version, keeps
  `derives-from`).
- `arrangement`: a target form; slots with question text, word limit, ordered
  placements. Incubator and Fellowship LOI are two arrangements over one pool.

### Cases the pressure test produces

Status column is a first reading from the code names and docstrings, NOT a
check; MAP must confirm each.

| # | Case | Instance | Classic Arxana | jvm-arxana |
|---|---|---|---|---|
| 1 | Divergent variants | one passage edited differently for incubator and LOI | include/transclude are single-source; no sibling versions | unknown; F0 resolver handles copy as typed outcome |
| 2 | Edits as attributable acts | draft-2's edits have no record at all | no | text_rebase patches carry author/operation id |
| 3 | Suggested propagation | an LOI edit raises "also apply to incubator?" | no | no |
| 4 | Three granularities | clip (sub-sentence), passage (one claim), slot (~150 words) | scholia anchor on passages | codepoint locators at any size |
| 5 | Word limits as pressure | slot limit recorded as the reason for an edit | no | no |
| 6 | Round trip from outside | text pasted into Mozilla's web form and edited there | no | F1b2 relocation re-anchors across an edited projection |
| 7 | Cross-time queries | the T/T1/T2 example above | no | futon1b is XTDB 2; whether Arxana records use valid time is unchecked |

### Nipper's place

Nipper (zai-13, 2026-11-14: a reader over existing stores with depth and
breadth dials and four strategies) proposes candidate clips into the pool.
Joe choosing one is the clip act. It needs no capture of its own. 狗's 43
buried quotes are one weak index into the turns (claude-1, 2026-11-14: 11 of 43
under 12 words).

### Timing

The incubator closes 2026-11-20: write it by hand from draft 2, off this
excursion's critical path. The Fellowship LOI (2026-12-17) is the first real
use.

## 2. MAP — questions

1. Do classic include/transclude/identification (`futon4/dev/arxana-inclusion.el`,
   `arxana-derivation.el`) still work end to end against futon1b? Which
   survives a store round trip?
2. Do jvm-arxana records carry valid time, or only system time?
3. Does the F0 resolver's "copying" outcome give sibling versions (case 1), or
   only relocation of one?
4. Can `correspond.clj` assertions and `text_rebase.clj` patches be the
   `act/clip` and `act/edit` records, or are new types needed?
5. Arxana Essays (`futon4/README-essays.md`) already keep annotations off the
   body with verbatim `:passage` anchors in `annotations.edn`. Is that the
   same anchoring problem one level up, and does it break the same way when
   the essay body is edited?
6. What is a passage? Candidate: one claim (the cell-line story; the plumbers;
   the $300k split). Joe to judge from the first pass.

First bounded packet (discovery, not build): answer 1–5 against the code, with
the draft-2 material as test data, and re-anchor the 9 recoverable sentences.
