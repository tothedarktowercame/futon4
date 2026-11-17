# DERIVE-1 — Xanadu requirements, from Nelson's own texts, against the variants demo

claude-5, 2026-11-17, for E-arxana-variant-documents. Modelled on
`futon3c/holes/labs/M-象-2000/DERIVE-1-mccarthy-requirements.md`.

**Why (Joe, 2026-11-17):** "this is what I'd call a 'basic Xanadu-like demo'. But in
Xanadu, things get... hm... more interesting. Just like we did for Elephant-2000 I
think we should build some acceptance criteria that relate to what the original
author (Nelson or McCarthy respectively) was talking about. That doesn't mean that I
don't have my own ideas. But Ted Nelson is still alive at 89 years old and I'd love to
actually show him a demo that he'd be interested in."

**Sources** (local copies, `~/storage/references/nelson-xanadu/`, see its README;
line numbers are of the `.txt` files):
- **X** = `xanadu-xanasimp.txt`: Nelson, "Simplest complete Xanadu explanation"
  (xanadu.net/xanasimp, dated 12.03.12).
- **S** = `nelson-1999-xanalogical-brown.txt`: Nelson, "Xanalogical Structure, Needed
  Now More than Ever", ACM Computing Surveys 31(4), 1999 (web text; the printed
  version may differ).
- Not yet read here: *Literary Machines*; Nelson 1965 (ACM '65); the "17 rules"
  (only a secondary copy is on hand). Rows that depend on them are marked.

**Demo under test:** futon4a `futon4a-variants-diff.el` / `futon4a-variants-edition.el`
and Mojira `vd` (futon4 `dev/mojira/variants.js`), over
`~/storage/arxana-variants/draft2-variants.json`; jvm-arxana as surveyed in the
excursion's MAP findings (packet B).

Format per row: **N-n** — Nelson (source:line) → in the stack → demo now → gap.

## A. Content and documents

| N | Nelson | In the stack | Demo now | Gap |
|---|---|---|---|---|
| N1 | "Content is stabilized, that is, given permanent addresses and the promise of permanent availability. The content and addresses do not change but are added to." (X:34); "Nothing is changed or deleted in the pool of permanent content." (X:42) | Every piece of text, Joe's spoken turns *and* what he types into a draft, gets a permanent address on entry and is never rewritten | Sources yes: turn records carry ids and sha256 digests. Draft text no: it is a flat file Joe overwrites | Typed draft text has no permanent address; an edit destroys the previous text |
| N2 | A document is made "INDIRECTLY, by pointing at content to include and pointing at links to overlay" (X:12); "EDL (Edit Decision List): A list of content to be concatenated into a document" (X:50); "A sequential document or version is represented by a content list" (S:175) | A draft *is* a list of spans: spans of turns (transcluded) and spans of Joe's own typed content | The ops list in `draft2-variants.json` is an EDL in all but name, but it is reconstructed afterwards by diffing; the draft file stays authoritative | The EDL is derived, not primary. Nothing holds Joe's typed text as permanent content the EDL points at |
| N3 | Editing works "indirectly through the EDL and ODL": insert = "new content is instantly permanized and its addresses put in the EDL"; rearrange; "delete: a span is deleted from the EDL" (X:116–119); "All editing operations change these content lists" (S:181) | An editor whose operations are EDL operations; the rendered text is a projection | None: the views are read-only; edits happen in a text editor, outside | No EDL-level edit. F2 patches edit artifact bytes, not a content list |
| N4 | "Meanwhile, if we can show a xanadoc edited by hand, that will be wonderful." (X:114) | A hand-made EDL edit, shown in the views | — | Smallest possible step toward N3; a concrete acceptance test (T2 below) |
| N5 | Every character addressable: "its conkindex" (X:71); "in post-1965 models we have striven to take this to the level of individual characters" (S:121) | Codepoint offsets throughout | Yes: anchors are codepoint spans, checked against the source at display time | — |

## B. Transclusion

| N | Nelson | In the stack | Demo now | Gap |
|---|---|---|---|---|
| N6 | "Content to be quoted from other documents is not "copied" but transcluded. The stable address of each portion to be quoted is put on the edit decision list. This retains the connection to its original context." (X:44) | A quotation from a turn is a pointer to the turn's span | Partly: kept spans point at the turn (id + offsets), but only after reconstruction; the draft itself holds copies | Quotes are copies with recovered pointers; 42 of 51 draft segments have no recoverable source |
| N7 | "Transclusions are not copies and they are not instances, but *the same thing knowably and visibly in more than once place*." (S:141); "we must make visible the knowable identity" (S:143) | Identity of content is computed by address, never by text similarity | Partly: identity is by recovered address, but the recovery itself is lexical matching (packet A) | Once N2 holds, identity is by address and lexical recovery is only for legacy text |
| N8 | Source transclusion: "any quotation can be shown next to its original context, to see where it came from and what it originally meant" (X:59); "pull up the original context in a transpointing window" (S:303) | Point at quoted words, see the turn | **Yes**: side window / Mojira peek shows the turn with the span marked; the annotated edition shows sources for the paragraphs on screen | — |
| N9 | Local transclusion: "two xanadocs being read locally show contents which are identical" (X:60); found by "comparing all the source addresses of currently present content" (X:88–90) | Incubator answer and Fellowship answer open side by side; shared content shown as shared | None: one document (draft 2) against its sources | The variants case itself: two documents that share passages |

## C. Links

| N | Nelson | In the stack | Demo now | Gap |
|---|---|---|---|---|
| N10 | "XANALINK: unit representing what connects to what ... A xanalink is free-standing and permanently addressable. It contains no content and has a type." (X:52); "ODL (Overlay Decision List): A list of stable xanalinks to be applied to the concatenated content" (X:54) | Comments, annotations, benefit/beneficiary notes as typed, addressable records applied over content | None | No content links at all; only transclusion. Mojira's LOI notes are a natural first link type (comment, benefit, beneficiary) |
| N11 | "2-way links, n-way links"; "links with types" (X:19–20); "profuse, overlapping, bidirectional, or diversely created" (S:191) | Links are found from either end; many per span | None | — |
| N12 | Links survive versions: "a xanalink remains on any part of the content it connects which survives in a new version" (X:46); the Adam/Barker walkthrough, v1 → v2 insert → v3 delete, link 2053 "remains attached to all of these surviving characters" (S:249–265) | A comment on a draft passage stays on the surviving characters through Joe's edits | None; worse, the edition refuses to open when the draft file changes ("rebuild anchors") | This is the failure the excursion started from (draft 2 lost its links). Acceptance test T3 |

## D. Seeing: transpointing windows

| N | Nelson | In the stack | Demo now | Gap |
|---|---|---|---|---|
| N13 | "Side-by-side connected comparison of parallel documents on the computer screen has always been Xanadu's fundamental visualization" (S:71); "these connections must remain connected to the contents no matter how the windows scroll or move around" (S:85); beams recalculated on scroll (X:108–110) | Two panes, connected, staying connected while scrolling | Partly: the side pane follows cursor and scroll and marks the matching span; no visible beams; the left pane is the document, the right only fragments | Beams. Emacs in a terminal cannot draw them; Mojira's webview can (SVG over two columns) |
| N14 | Parallel documents: "commentaries, ... long and short versions of reports, ... translations" (S:67); versions compared side by side (S:107, 125) | Incubator long form vs Fellowship 150-word field; draft 1 vs draft 2 | None | — |
| N15 | Pullacross editing: "pull screen contents from old versions into new, seeing all points of origin and also seeing what contents have not yet been used" (S:119); "origin beams ... what's been left out" (S:121) | While writing the Fellowship LOI, the turns and incubator passages are on the left; what has been used and **what has not yet been used** are both visible | Partly: the used region of each turn is shaded and the nipped words marked; unused parts of a turn are visible but not called out | "Not yet used" across all of Joe's relevant turns is the useful part for writing |

## E. Versions

| N | Nelson | In the stack | Demo now | Gap |
|---|---|---|---|---|
| N16 | "what is most needed for writing and editing are ways to track the continuities among drafts" (S:113); intercompare version lists to show common content (S:295–299) | Every save of a draft is a version (an EDL); any two can be compared by address | None: draft 1 → draft 2 share no text (measured); no versions of draft 2 | Comes for free once N1–N3 hold; jvm-arxana F2 already records attributed, dated edits (packet B) |
| N17 | The link follows "*presumptive* inheritance of meaning among versions; and the reader's intelligence, plus access to other versions, should take care of the rest" (S:279) | When an edit deletes part of what a link or quote was attached to, show what survives and let the reader open the earlier version | — | Pairs with N12 |

## F. Addresses, re-use and rights

| N | Nelson | In the stack | Demo now | Gap |
|---|---|---|---|---|
| N18 | Permanent universal IDs (S:157); content "may be cached in many places, as long as it has the same permanent IDs" (S:183) | Evidence ids, sha256 digests, jvm-arxana revision ids | Partly | Typed draft content has no ID (N1) |
| N19 | Transcluding from a web page: "The web page is stabilized ... given a permanent address", then the portion's address goes in the EDL (X:122–124) | The Mozilla web form: text pasted there and back is stabilized, then transcluded | None | The round trip through Mozilla's form (excursion case 6) |
| N20 | Transcopyright: "in return for each excerpt being tethered to its original context ... the rightsholder gives permission in advance for all transclusion in any amount ... a gateway micropayment may be added" (S:357) | Attribution that tracks who typed what, the basis of the proposal's benefit argument (Mozilla area 4: "Attribution that holds up") | None (all content is Joe's) | Needs a second contributor before it means anything; ties the demo to the LOI's argument |

## Acceptance tests for a demo Nelson would recognise

Each test names its rows and the bad case it must catch.

- **T1 Transpointing windows with beams (N13, N9, N8).** Mojira shows two documents side
  by side with beams joining shared content, recomputed on scroll. Bad case: scroll
  one pane; a beam that keeps its old screen position fails.
- **T2 A xanadoc edited by hand (N2, N3, N4, N1).** One paragraph exists as an EDL of
  turn spans plus permanent typed content; an insert, a rearrange and a delete are made
  on the EDL, and the views render the result. Bad case: an edit that rewrites stored
  content instead of appending must be refused.
- **T3 Nelson's Adam/Barker walkthrough on our data (N12, N17, N10).** A comment link on a
  draft-2 passage, then v2 (insert in the middle), then v3 (delete part of the linked
  span): the link stays on the surviving characters in every version. Bad case: the
  link on the deleted characters must not reattach to neighbouring text.
- **T4 Pullacross for the LOI (N15, N14).** Writing one Fellowship LOI field from turns
  and incubator passages, the left pane shows used material and material not yet used.
  Bad case: a turn already fully used must not be offered as unused.
- **T5 Quote to context (N8, N6).** From any transcluded word, open its original context.
  Met today; keep it as a regression test.

## What this table does not cover

The "17 rules" and *Literary Machines* (versions as the operative unit, tumbler
addressing, the publishing and royalty model, privacy) are not read here; add rows
when a primary copy is available. Joe's own ideas sit alongside these rows, not inside
them; where the stack deliberately departs from Nelson, record it as a departure, as
the McCarthy table's "Dropped" column does.
