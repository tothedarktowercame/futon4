# M-arxana-incubator — Arxana as an innovation hub, proved on the Mozilla applications

**Status:** ARGUE (2026-11-17; DERIVE exit met, Joe)
**Owner:** Joe (operator); claude-5 drafting. Seats to be matched to work packages after MAP.
**Repos:** `futon4` (this mission; Mojira; classic Arxana as reference), `futon3c`
(jvm-arxana: `src/futon3c/arxana/`), `futon1b` (store: `futon1b_arxana.clj`),
`futon4a` (Emacs demo views), `futon7` (`holes/mozilla/` LOI draft)
**Grows out of:** `holes/excursions/E-arxana-variant-documents.md` (with its labs:
`DERIVE-1-nelson-requirements.md`, `WORKPACKAGES.md`)
**Related:** `M-formal-arxana` (closed 2026-11-16), `M-jvm-arxana`, `M-mojira`,
`futon3c` `M-repl-circus` (closing; GCC records), `futon3c` `M-diagramprover`
(rewriting engine)

## HEAD

### Operator-voice anchor (Joe, 2026-11-17)

> "pretty much everything *except* Arxana could go into the background or pause and we
> can have a kind of "all hands on deck" push around Arxana together with the Mozilla
> incubator draft."

> "I'd rather break it down across work packages and then think about who joins after.
> We might keep the existing pool focused on their missions but liaise from outside to
> keep Arxana as a kind of "innovation hub" shared by these various efforts for
> example."

> "that's a good outline, we could start M-arxana-incubator on the basis of this (at
> IDENTIFY). What I'd like to see in MAP would be the *types* that correspond to the
> WPs."

> "I think the *above-all* requirement is traceability, so e.g. a version 3 that
> remains grounded in the original source texts but adds new content, or an editor
> that we could get Murch and Rob onto that retains attribution to them while allowing
> cut-and-paste with attribution to me as well as links &c"

> "I work on "one" text but get both the Incubator and the Fellowship drafts as
> projections of it. A possible bonus is that that "one" text *is the Evidence
> Landscape*. ... we could sort of think of the Evidence Landscape as my "model" and the
> hand-edit 'clusions as little LoRA-like style customisations"

> "the parts exist separately but nothing joins them"

(Joe, 2026-11-17, adopting claude-5's summary of the MAP findings as his own statement
of the gap: "Nicely put, can you put that in the HEAD (supposed to be operator voice but
I couldn't have put it better myself!)". The parts are jvm-arxana's attributed patches,
pinned F1 manifests, F6 placements, relocation and span-addressed turns; what is missing
is the projection, requirement and nip that join them.)

> 🗣 (dictated; "Arcana" = Arxana, "closure" = Clojure, "plop" = PLoP) "So maybe I should
> just throw in a little bit of my own experience trying to use [Arxana], the classic
> Emacs edition, and some possibly additional requirements and use cases that come from
> that ... this goes all the way back to 2005, and we could dig in those historical
> documents, but it's probably better to work backwards from recent use cases like
> trying to keep [Clojure], lean and documentation all aligned, and that actually is still
> an active use case for the [PLoP] 2026 paper ... none of my agents are currently
> working on that workflow directly, but that is kind of the workflow center, I would
> say, that all of the different dimensions of the tetrahedron touch Upon and
> specifically, that paper is about the war machine and the way it relates to workflow
> is the war machine is an automation tool for working through work following patterns
> and priorities that I've put in place ... the giraffe is a tangent for presenting that
> work in some kind of priority order the meta CA is about how the AIF material works in
> a reduced case and I suppose even the proving stuff is about understanding how a formal
> formal Problem-solving process can relate to an informal process of learning, and I
> would describe all of those as matters of priority for the war machine so even though
> this particular mission is not about plop 2026 that could be seen as a live use case for
> this material and probably when we get to the end of this, Should become a test case
> that we're able to deal with but that's, again, different from the Mozilla textual
> work ... in the full generality, we have to deal with all of this stuff along the way
> and the relationship to the Mozilla thing is that the Mozilla thing effectively
> documents that complexity in a simple way." (Joe, 2026-11-17)

> 🗣 "what we did in the War Machine work about building the Aligned Lean and Clojure with
> a runtime certificate is effectively a good example of the kind of link that we want
> [Arxana] to help Keep Track of and I even did a little workup as a kind of prototype of
> trying to align the informal specification in the paper with the formal specification
> with the code and I think that that sense of aligning informal, formal, and code, and
> documentation ... is a good example of the kind of thing that we're going to want to be
> able to do and the kind of deliverable that would make this actually impactful for
> people because ... people don't have a great time achieving any of those aligned
> links, I would say, when they use agents and if we can achieve all of them, That's a
> big win." (Joe, 2026-11-17)

> 🗣 "informal specification in the paper[,] formal specification in [Lean,] aligned Code
> with a Runtime Certificate and Documentation those are effectively an instantiation of
> the tetrahedral facets and the difficulty is in holding them all together, which is
> kind of like I said, the good summary of what [Arxana] needs to do but the key issue
> here is that it's not holding them together in a static arrangement it's holding them
> together in a workflow and so that's where some of our stuff about modeling the way a
> document evolves has to get instantiated in another way in thinking about the way a
> workflow evolves so simply writing up a document, that's something you can do on
> Google Docs or Etherpad multiple people can contribute even Etherpad 15 years ago could
> track people's contributions according to what color the highlighting is and I think we
> need to articulate how it is that when you look at a workflow, We're doing something
> similar but different." (Joe, 2026-11-17)

### What is already felt to be true

- A Xanadu-like demo works (2026-11-17): a draft paragraph over the spoken turns it came
  from, in Emacs (futon4a `futon4a-variants-diff.el`, `-edition.el`) and Mojira (`vd`),
  with source transclusion shown on cursor-over and an annotated edition that shows the
  sources of the paragraphs on screen.
- The Evidence Landscape is large enough to write from: 23,158 operator turns,
  2026-02-18..11-11 (`~/storage/operator-turns/`), plus the 象 records after.
- jvm-arxana already has attributed patches, stored alternatives, relocation, F6 dated
  placements; futon1b now publishes `:event-time` as valid time and reads as of a time
  (futon1b 5147aa8, e4aa6e1; futon3c 09ca6c8c4; live after Joe's restart).

### Anti-glibness discipline

- Traceability is measured, not asserted: every character of a projection must answer
  who produced it, when, and from where (DERIVE-1 test T0), with the bad case built.
- Nelson's requirements are cited from his own texts by line (DERIVE-1, 36 quotations
  checked verbatim); departures are recorded as departures, not as gaps closed.
- The incubator deadline is real: the tool must never stand between Joe and submitting.

### Working-economy position

Underwrites: both Mozilla applications (incubator closes 2026-11-20; Fellowship LOI
2026-12-17), whose argument ("trace the value back to the people who are typing this
information in") this mission demonstrates working. Underwritten by: the landscape
(REPL Circus, 象), jvm-arxana, Mojira. As a hub it serves the other efforts without
taking their seats (see `WORKPACKAGES.md`, "Hub").

### Carried-forward tensions

- How "updates" sit on immutable objects: 'clusions superseded by later 'clusions, live
  vs static projections (Pijul study, `~/notes/zai-6/pijul-for-arxana-2026-11-17.md`).
- Requirements are not patches: they need a satisfaction relation Pijul lacks.
- **Document evolution vs workflow evolution** (Joe, 2026-11-17, above): articulate what
  Arxana tracks that collaborative editors (Etherpad's per-author colours, Google Docs)
  do not. claude-5's first attempt, for DERIVE to test: an editor tracks who typed which
  characters of one text; a workflow tracks which *relations between several texts*
  hold now (paper ↔ Lean ↔ code ↔ docs), since when, who or what established each
  (a person, an agent, a Lean build, a runtime certificate), and what work a broken
  relation creates. Differences that follow: the four manifestations change at
  different rates and by different actors; an alignment's status is evaluated by a
  check, not typed by an author, and goes stale when either side moves; a change on one
  facet reopens obligations on the others (the War Machine's queue); order matters
  ("before"); machines are authors too.
- **The MetaCA reduced model as a model of this workflow** (claude-1, 2026-11-17, at
  Joe's request: `~/notes/claude-1/tetrahedron-and-metaca-2026-11-17.md`). Holds: the
  reduced model's one relation is a check-evaluated alignment; a defect is where work
  happens; a cell's own writes add no defects, a neighbour's change does; "what gets
  selected is a history, not a condition" (exotype D21, `DESIGN-aif.md:924`;
  `WRITEUP-aif-controller.md:33`) matches "not a static arrangement but a workflow".
  Fails: one kind of cell vs four manifestations; a ring has no diagonals (six relations
  among four facets); two statuses vs seven; rest is dull there, the aim here; no
  authorship, free checks, unchosen writes. Proposed re-expansion (speculation): cell =
  one alignment note, rule = its manifestations at pinned revisions, defect = a failing
  or stale relation, neighbours = notes sharing a declaration; prediction before
  observation = STATED before PROVED. Would give Arxana a defect count as a projection's
  distance from holding, a ranking of the next repair, and a spread-of-staleness
  measure. Experiment: replay the four committed versions of
  `p4ng/plop-2026-alignment.json` against their pinned revisions with three predictions
  registered first; four versions is thin history.
  Joe's follow-up, "the "locality requirement" in MetaCA maps onto "what we can do using
  classical code in connection with a turn"" (驴 狗 猴 as local operations): claude-1's
  §5 holds it for what the animals write and the turn data they read, not for retrieval
  over shared stores; design consequence: span hashes, endpoint checks, limits, nip
  acceptance and stale-marking are turn-time (local), retrieval and a first
  whole-projection check are global; refuting check: recompute ~200 猴 focus records
  from the turn record and the previous focus header (<80% agreement refutes).
- Rewrite rules ("bogus" → "nogood" → "no good") may belong on DiagramProver's DPO
  engine rather than in patch theory.
- Where free-standing links live today in jvm-arxana and Mojira (Joe: they exist; the
  demo did not use them).
- The Sierpiński tetrahedron framing of the three strands in other ongoing work:

  simulation = variables = verbs : the MetaCA research which explores AIF in a minimal model
  automation = holes = nouns : the proof-theoretic work from apm-lean, mfuton
  interactive agents = organisation : M-repl-circus and the semantics of intent per McCarthy

  with Arxana as the fourth facet (evidence): TBD as a "cone" over the above

  all leading to a framing of a recursive "workflow" centre which touches on all of the above.
  
  See Figure 3 in https://zone.hyperreal.enterprises/wip/futon-2026.html for earlier work
  on this

### Provenance

Drafted by claude-5 on 2026-11-17 from that day's conversation with Joe (emacs-repl,
claude-5), the excursion, and its two lab files.

**HEAD exit: Not started.** For Joe to judge.

## 1. IDENTIFY

### Motivation

Joe's incubator draft 2 was compiled from clips of his own turns and then edited as a
flat file; measured 2026-11-16/17, 0 of its 51 sentences survive verbatim and 42 have
no recoverable source. The links were lost at the first edit because a markdown file
has nowhere to keep them. The proposal argues for traceable contribution, and its own
draft could not demonstrate it. Arxana can, if documents are projections over the
landscape and edits are 'clusions rather than overwrites.

### Theoretical anchoring

- Nelson: xanalogical structure, transclusion, EDL/ODL, links that survive versions,
  transpointing windows, transcopyright (DERIVE-1, sources in
  `~/storage/references/nelson-xanadu/`).
- Patch theory (Darcs, Pijul): patches as first-class, commutation, conflicts as states,
  channels without a privileged head.
- Bitemporal storage (XTDB 2): valid time for when an act happened, system time for when
  it was recorded.
- Joe's model: one text (the Evidence Landscape) with drafts as projections and hand
  edits as removable 'clusions.

### Scope

In: the twelve work packages in `WORKPACKAGES.md` (incubator requirements and fields;
projection, 'clusion-update and requirement records; F2 head → projection; Nipper nips
and proposals; editing by hand; links in the demo; two projections side by side; the
Fellowship LOI as a second projection).

Existing prototype of that link (found 2026-11-17): `p4ng/plop-2026-alignment.json`
(schema `p4ng/plop-alignment-v1`, 71 notes: 24 alignment, 38 upgrade, 9 question). Each
note anchors to a paper passage (`heading_contains` + verbatim `quote`), names the WM
pattern (R1, ...), and lists Lean declarations (module, decl, 40-hex revision, status
PROVED / STATED / ABSENT) and Clojure definitions (status VALIDATED /
EXISTS-UNVALIDATED / ABSENT-OR-LOST / REFUTED), with author and date;
`p4ng/inject-plop-alignment.py` validates each at its pinned revision and injects margin
notes into the paper's HTML. It is an informal ↔ formal ↔ code link with a per-endpoint
status, kept outside Arxana. The WM runtime certificate (futon2 `M-G-wm-wiring.md`,
Lean-validated) is the same idea at run time.

Later test case, named now (Joe, 2026-11-17): keeping Clojure, Lean and documentation
aligned for the PLoP 2026 paper on the War Machine (`p4ng/plop-2026*`, `sec-*-plop.tex`),
the "workflow centre" every facet of the tetrahedron touches. Not this mission's work,
but its end-of-mission test: the same projection, 'clusion and requirement types should
hold code, formal and informal manifestations of one argument in step. Classic Arxana's
use since 2005 is source material, read backwards from this recent use case.

Second live use case, named 2026-11-17 (Joe): "claude-7's "PaperPrinter endgame" is a
possibly a direct use case for Arxana. The issues that we are having there around cycling
&c are things that we should be able to resolve through a combination of an Arxana graph &
a DiagramProver analysis." In claude-5's reading: the reading matrix
(`~/notes/claude-7/raw-names-480/census/matrix-r8.tsv`) as rule 'clusions with
supersession; each rendered sentence a generated manifestation ("generated by" renderer
sha + declaration + rules, claude-7's interview); a sentence's wording history as its
fibre; "cycling" (a sentence rewritten back and forth across packets, e.g. a97J08 across
Q55–Q83) as a cycle in that history; an unintended supersession as a regression record;
and DiagramProver's DPO engine checking the reading rules as a rewrite system for
critical pairs (two rules that undo each other) before a render, not after. The
endgame's "clean except for a documented list" is a requirement with accepted residuals.
This puts DiagramProver rewrite rules (deferred below) on a live problem.

Out, for now: DiagramProver rewrite rules; transcopyright and payment; MetaCA run
records as a second landscape; a multi-author editor beyond the record design (T6 is
specified, not built, in this mission).  Not because they are unimportant but because
we need these foundations to make them work.

### Completion criteria

- [ ] The incubator application is submitted by 2026-11-20, with each field's text
      traceable sentence by sentence to a turn or marked as new writing (WP1, WP2).
- [ ] A projection record and a 'clusion-update record exist in jvm-arxana; an edit
      scoped to one projection leaves another projection of the same passage unchanged
      (WP4).
- [ ] A paragraph edited by hand (insert, rearrange, delete) in Emacs leaves every
      character answering who, when and from where (DERIVE-1 T0, T2; WP9).
- [ ] A comment link survives an insert and a partial delete without reattaching to
      neighbouring text (DERIVE-1 T3; WP10).
- [ ] The Fellowship LOI's 15 fields exist as a second projection over the same
      'clusions, with used and not-yet-used material visible (DERIVE-1 T4; WP12).

### Relationship to other missions

Depends on jvm-arxana (M-jvm-arxana, M-formal-arxana's F0–F6) and the landscape
(M-repl-circus, 象). Enables the Mozilla Fellowship LOI and gives M-mojira its first
writing surface. Liaises with WM/Giraffe (requirements resemble wants) and
DiagramProver (rewrite rules) without depending on them.

### Source material

- `holes/excursions/E-arxana-variant-documents.md` (cases, MAP findings packet B,
  Joe's model, 'clusions as editable beams)
- `holes/labs/E-arxana-variant-documents/DERIVE-1-nelson-requirements.md`,
  `WORKPACKAGES.md`
- `~/notes/zai-13/nipper-design-2026-11-17.md`, `~/notes/zai-6/pijul-for-arxana-2026-11-17.md`
- `~/storage/arxana-variants/` (draft-2 anchors, `para_diff.py`, `draft2-variants.json`)
- `~/mozilla-incubator.txt` (incubator questions), `~/mozilla-incubator-draft*.txt`,
  `futon7/holes/mozilla/LOI-draft-1.md`, `~/mozilla/PUBLIC_2027_2028_Fellowship_RFP-2.pdf`

### Owner and dependencies

Joe owns; claude-5 drafts and reviews. Seats per work package after MAP. Joe's restart
of futon1b and futon3c (WP3) is needed for valid time.

**IDENTIFY exit: Met.** Joe has read and agreed to the gap and scope, and
has clarified the Sierpiński framing.  Further evidence: Rob has been
working on a browser for neo4j, and attests that related problems show
up in his mfuton workflow.

## 2. MAP — survey questions

Joe, 2026-11-17: "What I'd like to see in MAP would be the *types* that correspond to the
WPs." For each work package, survey the types that already exist (jvm-arxana
`type_registry.clj` and record constructors, futon1b tables, classic Arxana's scholium
and inclusion structures, Mojira's file formats, the futon4a/para_diff JSON) and say
whether each is ready, partly there, or missing. Facts, not designs.

- **Q1 Landscape types (WP2, WP7).** What type does a turn have today in each store
  (operator-turn dump rows, 象 records, futon1b evidence entries, F1's materialised
  `xiang:<turn-id>` artifacts)? Which identity and time fields does each carry?
- **Q2 'Clusion types (WP4, WP9).** Which 'clusion types exist in jvm-arxana
  (`type_registry.clj`: interpretation, correspondence, placement, ...), what are their
  endpoints and content fields, and which could carry an edit (replace / insert /
  delete) over landscape spans?
- **Q3 Supersession and update (WP4).** Is there any existing record that supersedes
  another (F6's supersession, F2's applications, conflict choices)? With what fields?
- **Q4 Projection (WP4, WP5, WP11).** What is closest to a projection today: F2's
  artifact + head, F6's argument with placements, Mojira's LOI draft fields, the
  `draft2-variants.json` paragraph list? What does each lack?
- **Q5 Requirement (WP1, WP6, WP8).** What existing types express a question with a
  limit and a met/failing status: Mojira's `loi.js` fields, `check_limits.py`, WM wants,
  GCC items, test-registry warrants?
- **Q6 Nip (WP7, WP8).** Does any existing record hold a query plus its accepted result
  span (狗's buried quotes, m40 memory quotes, evidence-landscape search results)?
- **Q7 Link (WP10).** Where do free-standing typed links live today: classic Arxana
  scholia, jvm-arxana relations, Mojira notes, futon1b hyperedges? Which survive a
  change to the text they attach to?
- **Q8 Views (WP2, WP9, WP11).** Which types do the existing views consume (futon4a
  variants JSON, Mojira `vd`, F1/F2 Emacs clients), so new records can be rendered
  without new views?

Required MAP output: a table, one row per work package, of its types: ready / partly
there (with what is missing) / missing; and surprises.

### MAP findings (2026-11-17)

Two read-only surveys (claude-5's Explore surveyors), saved with full file:line
evidence: `~/notes/claude-5/map-arxana-incubator-A.md` (Q2, Q3, Q4, Q7: jvm-arxana and
futon1b at futon3c 45ebc6894, futon1b e4aa6e1) and
`~/notes/claude-5/map-arxana-incubator-B.md` (Q1, Q5, Q6, Q8). claude-5 spot-checked five
citations, all correct: `[:supersedes 1]` has no content fields (`type_registry.clj:163–166`);
automatic relocation is hardcoded to the F1 artifact (`formal_f1_relocation.clj:181`);
`:entity/get` allows only `:entity/id` (`schema.clj:19–23`); evidence validity starts at
insertion (`futon1b_evidence.clj:847–849`); 狗 drops its recall query when saving
(`dog_provider.clj:1381`).

**Answers in one line each**
- **Q1 Landscape:** four shapes of a turn (dumps, futon1b evidence, 象 records, F1's four
  fixtures). `:evidence/id` is the only id that joins them; `turn_id` is a per-session
  counter, not an identity. No store records *when Joe spoke* as valid time.
- **Q2 'Clusions:** 63 registered types, all append-only. Only `[:text/patch 1]` carries an
  edit, and only against an artifact's single head, not over turn spans.
- **Q3 Supersession:** four shapes, none general: `[:supersedes 1]` (no fields),
  `[:f6/re-supports 1]` (actor, time, reason, but readers ignore it), `[:d6/composite 2]`,
  F2 application/choice.
- **Q4 Projection:** the parts exist apart: F1's export manifest (ordered selection,
  pinned), F6 (attributed, dated order), F2 (edits). No record joins them.
- **Q5 Requirement:** only the Fellowship LOI's `<!-- field: x limit=N -->` markup
  combines question, limit and pass/fail, as markup checked by a script. The incubator
  form lists its questions but no limits on disk; Joe's "about 150 words or less" (turn of
  2026-11-14) is the only source. WM wants, GCC `waiting_on.met`, warrants have status
  without limits.
- **Q6 Nip:** nothing stores query + turn span + a person's acceptance. 狗 burials have
  spans without query or acceptance; 狗 recall has a query but drops it when saving;
  draft-2 anchors have verified spans from a matcher.
- **Q7 Links:** typed links are clusions in `arxana_clusions`; the relocator exists but
  runs only for the F1 artifact; no general span-anchored link type; futon1b's generic
  hyperedges are not written by jvm-arxana.
- **Q8 Views:** the futon4a variants views and Mojira `vd` share one static JSON with five
  op kinds (Mojira rejects any other); F1/F2/F6 clients speak arxana.v1 to demo ports. No
  view reads requirement or nip records.

**Types per work package**

| WP | Types it needs | Ready | Partly there (what is missing) | Missing |
|---|---|---|---|---|
| WP1 Incubator requirements | requirement (question, limit, status) | LOI field markup + `check_limits.py` + `loi.js` (Fellowship) | incubator: questions exist (`~/mozilla-incubator.txt`), limits only from Joe's "about 150 words" | requirement record type |
| WP2 Fields as projections | landscape turn; projection; export | variants JSON + views; `evidence_id` join key | projection = draft2 paragraph list (static, embedded sources, no pins) | projection record over turn spans |
| WP3 Valid time | record valid time; turn speech time | Arxana record `:event-time` → valid-from; as-of read (store adapter) | as-of not exposed by `:entity/get`; heads/revisions not covered | speech-time valid time for turns (evidence `:at` is ingestion time) |
| WP4 Projection + 'clusion update | projection; supersede-with-attribution; scope | F1 manifest (ordered, pinned); F6 placements (attributed order) | `[:supersedes 1]` has no actor/time/reason; F6 re-supports ignored by readers | one projection record; scope; static vs live |
| WP5 Head → projection | per-projection heads | F2 patches, alternatives, choices | one CAS head per artifact | sibling heads / projection-as-head |
| WP6 Requirement records | requirement + satisfaction relation | warrants pin status to shas; GCC `met` computed | WM wants (stated + boolean) | requirement type tied to a projection |
| WP7 Nips | nip = query + span + acceptance | span addressing (codepoints, sha256, `evidence_id`) | 狗 burials (span, no query/acceptance); recall (query, dropped) | nip record |
| WP8 Nipper proposes | requirement → candidate spans | 象 sentences/acts with offsets; text search | search returns whole entries | proposal records |
| WP9 Edited by hand | edit 'clusion over spans | `[:text/patch 1]` hunk form | edits only against an artifact head | edit over turn spans without rewriting an artifact |
| WP10 Links survive edits | span-anchored link; relocation | clusion substrate; relocator (exact/context/fuzzy, ambiguity kept) | relocation only for the F1 artifact | general span-locator link type; insert + partial-delete test |
| WP11 Side by side | two projections, shared spans | 2-up views with cursor-over highlighting | views read one static JSON; closed op set | a second projection; a view over records |
| WP12 LOI as projection | all of the above | LOI fields + limits as markup | — | depends on WP4/6/7/11 |

**Surprises**
- Codex-13's packet B is already out of date on valid time: Arxana records now take
  `:event-time` as valid-from (futon1b 5147aa8) with as-of reads (e4aa6e1).
- Turns have no speech-time valid time anywhere; this matters for the landscape as
  "the one text" (WP3 covers records, not turns).
- `turn_id` is not an identity (thousands of nulls and duplicates); `evidence_id` is, and
  670 operator 象 records lack one.
- The incubator's limits have no source on disk: the web form's real limits need
  checking before WP1 is built.
- 狗 already computes the query a nip would need, then drops it when saving.
- Mojira's `vd` rejects any op kind outside its five, so new record kinds need a
  deliberate extension, not just new data.
- F2's application time is the server clock at submission, not the edit time;
  `[:supersedes 1]`, patches and choices get no valid time at all.

### Interviews (2026-11-17)

Joe, 2026-11-17: interview the agents on the tetrahedron's three facets, then pass the
whole set to Codex for a gap analysis, "iteratively" so that claude-7's answer can be
added when it arrives. Answers (full text in each agent's notes):

- **Organisation: claude-13** (`~/notes/claude-13/arxana-incubator-interview-2026-11-17.md`).
  Records: 象 turn records, the animals' GCC records (驴 馬 狗 象 猴), items, supersession,
  needs, Giraffe runs, Lean chain certificates, clock decisions. Gaps: `[:supersedes 1]`
  has no actor/time/reason while CL-45 moves clocks on it (`clock_decision.clj:385`
  `:superseded-from`); corrections of an animal's reading have no record kind; typed
  absences are fields, not records. Missing types: "continues in", reading correction,
  typed absence. Next month: `[:supersedes 2]` (actor, time, reason, scope) with readers
  that honour it; test: Joe corrects one 象 reading and Mojira shows who/when/why.
  Corrections to MAP: the 象 record id `turn-XXXXXX` is stable (the dumps' `turn_id` is
  the counter); 象 records carry `created_at`, close to speech time, so speech time
  exists for turns since 2026-09-28 and is missing before.
- **Simulation: claude-1** (`~/notes/claude-1/arxana-incubator-interview-2026-11-17.md`).
  Records: exotype registrations, scripts, run dumps identified by sha256 lists, result
  files, the causal model JSON, Joe's rulings quoted into `PROGRAMME.md` (e.g. line 63 in
  the exotype-c1-causal worktree), review bells. Gaps: a claim's evidence is free text
  with no sha or span; rulings have no turn id; "predicted before the run" is shown only
  by commit order. Missing types: run record (code sha, parameters, seeds, dump
  hashes), prediction attested as earlier than its observation, "computed by this
  script from these inputs". Next month: nips over `results/*.txt` for ~30 figures in
  `WRITEUP-aif-controller.md`; test: a planted wrong figure is flagged, and changing a
  results file marks dependent figures stale. Challenges to MAP: the landscape is not
  only turns and text; a requirement can be a script-decided predicate (closer to a
  warrant) rather than a question with a word limit; no "before" relation.
- **Automation and proof: claude-7** (`~/notes/claude-7/arxana-incubator-interview-2026-11-17.md`,
  arrived after MAP closed; **feeds VERIFY**). Facet: rendering APM Lean proofs as paper
  proofs (mfuton PaperPrinter). Records: Lean declarations (module, decl, Main.lean
  lines), rendered text dumps (renderer commit not recorded in them), reading decisions
  (`~/notes/claude-7/raw-names-480/census/matrix-r8.tsv`, 3,528 rows: module,
  declaration, packet, rule, reading; checked), packets/merges joined only by log lines,
  tests pinning rendered text, census queries. Hardest gap: "which packet and rule
  produced this sentence, and which merge last changed it?" (a97J08's "In either case, we
  obtain a contradiction" rewritten by five packets; a regression traced by manual
  bisect). Missing types: "generated by" (sentence ← renderer sha + declaration + rules),
  accepted residual (documented exception), regression (an unintended supersession).
  Corrections: generated text should anchor on (declaration, step, typed term), not text
  spans (WP10); corrections there are to rules, not text (WP9); the landscape includes
  computed text, not only turns. Next month: the reading matrix as attributed,
  superseding rule 'clusions, and one proof projected with each sentence linked to its
  rules and Lean span; planted check: a changed reading shows as superseded and its
  sentences stale.
  Gap-analysis pass 2 (codex-13, futon4 9879fd2, appended to `GAP-ANALYSIS.md`; feeds
  VERIFY): 16 requirements, six new type gaps (G14 rewrite rule as a decision, G15
  generated-by / rule application, G16 structural locator on declaration + step + typed
  term, G17 accepted residual, G18 regression finding, G19 reproducible computed
  projection), six conflicts. Verdict on claude-7's test: **not yet** specified enough to
  pass; the design has the record families but lacks guarded rule identities, a complete
  sentence-to-rule mapping, structural anchors, renderer/corpus/rule pins, retained
  wording history, and the consumer that marks every dependent sentence stale. This is
  the PaperPrinter use case's gap list.
  Earlier note: Joe, 2026-11-17:
  "we should not *wait* on MAP until claude-7 comes back to us, but expect that
  claude-7's pass will contribute instead to VERIFY, i.e., we can start DERIVE when we
  get back codex-13's response". claude-7's answer and gap-analysis pass 2 therefore
  feed VERIFY, not MAP.

Common to both answers: supersession with actor and reason; nips. Facet-specific:
run record and "before" (simulation); reading correction and typed absence
(organisation).

**Open facts, resolved (2026-11-17):**
- Incubator limits: Joe, 2026-11-17: "I think all the short text fields are 150 words
  long." WP1 uses 150 words for every free-text field.
- Landscape size: the three dump files hold 23,160 rows; 23,146 distinct evidence ids
  (2 rows have no id; the rest of the difference is rows repeated where the files'
  date ranges meet). Earlier figures (23,158; survey B's 23,147) counted rows or included
  a null id. The landscape is **23,146 distinct turns**, 2026-02-18..11-11, plus the 象
  records after.

**MAP exit: Met.** Joe, 2026-11-17: "OK, at this point I think we can close MAP and move
to DERIVE". claude-7's interview and gap-analysis pass 2 feed VERIFY (Joe, above).

## 3. DERIVE

### DERIVE-1 — types, invariants, data flow (2026-11-17, claude-5, first cut)

`holes/labs/M-arxana-incubator/DERIVE-1-types.md`: six entity types (source turn with
`:evidence/id` identity and a time basis; manifestation revision; content; projection;
evaluation; typed absence), seven relation types (span ref; edit with scope; supersede v2;
link; requirement with evaluator kinds word-limit / predicate / deadline /
correspondence; nip; alignment after `plop-alignment-v1`), eight checkable invariants
(immutability, traceability T0, scope, pinned evaluation, locality, time, no silent
copy, typed absence), the data flow from landscape to renderers, and eight decisions in
IF/HOWEVER/THEN/BECAUSE form (records in jvm-arxana; no privileged head; status evaluated
not stored; one requirement family with several evaluators; turn identity and time;
edits target spans; the incubator path file-backed first; locality first).

- [ ] Joe's corrections to DERIVE-1.
- [x] Wiring diagram (futon5 exotype) for the landscape → Nipper → projection →
      evaluators → requirement list loop: `futon5/data/missions/arxana-incubator-exotype.edn`
      (futon5 d45ecee); `futon5.ct.mission/validate` all 8 checks pass; a planted
      write-back to the forms is caught (DERIVE-2 §1).
- [x] Fidelity contract against donors (`plop-alignment-v1`, F2 patches, F6 placements,
      variants JSON): DERIVE-2 §3, every `plop-alignment-v1` field mapped, round-trip
      tripwire through `inject-plop-alignment.py`.
- [x] Exact type-registry entries for the new types: DERIVE-2 §2 (proposed, in
      `type_registry.clj` form).
- [x] Rules for the five items the coverage check left open (partial deletion,
      "not yet used", reading as an entity, precedence, relocation beyond F1): DERIVE-2 §4.
- [x] Coverage check against Nelson's T0–T6 and the gap analysis's 13 type gaps (DERIVE-1 §7: 9 of 13 gaps covered, 3 partly, 1 deferred; 5 of 7 tests covered, 2 partly).

Nipper and requirements (the provisional edge; zai-13, 2026-11-17,
`~/notes/zai-13/nipper-and-requirements-2026-11-17.md`): three candidates, (a)
requirements drive Nipper via failing evaluations, (b) requirements only judge nips
afterwards, (c) recommended: two seed kinds on the same strategies, a memory seed ("already
said") and a gap seed from a requirement ("still needs"), so a nip records which kind
proposed it. Distinguishing test on the PLoP case: a passage whose Clojure endpoint is
missing; under (c) a gap seed can query before any evaluation exists. **Joe chose (c)**
(2026-11-17): "the "two kinds of seed" are good b/c they can support forward and backward
reasoning"; the edge is now a gap seed, and the nip record carries `:nip/seed`
(DERIVE-2 §2). Joe added: "I also think this is one way to form a 3/2 pushout square,
rather than starting with LEFT and RIGHT we could start with NEXT and PREV" (taken to
claude-1). claude-1's §6 of `~/notes/claude-1/tetrahedron-and-metaca-2026-11-17.md`:
PREV = memory seed, NEXT = gap seed, generic space = points both needed and already said,
arbiter = the passage as it stands with Joe's acceptance, blend = the written passage
with its nips; the requirement stands in for the turn that does not yet exist, which
gives a turn its missing RIGHT. Holds as a reading; fails on five points (agreement is a
check that can go stale; inputs are of different kinds; a word limit couples the
points; with no limit the union trivially wins; the point set is not fixed). Three kinds
of disagreement where the CA blend has one (differ → arbiter; needed but unsaid → work,
a new turn; said but unneeded → competes only under a limit). Squares paste along time:
each passage joins the next PREV. IF = PREV, HOWEVER = NEXT, BECAUSE = their meet, THEN =
the passage. Selector-swap analogue (always-PREV / always-NEXT / random / majority vs
arbiter): cheapest trial is how the 16 REFUTED statuses in the PLoP alignment file were
resolved; the exotype selector swap itself is planned (NEXT.md item 3), not yet run.
Joe, 2026-11-17: "I see this as related to the way in which patterns are linked with
"want tokens" by the War Machine". claude-5's reading, checked against
`futon3c/scripts/wiring_check.py` and `futon3c/holes/labs/M-象-2000/cascade-象2000.edn`:
a cascade has `:initial` tokens (fed in by a have-port), `:want` tokens (a want-port),
patterns with `:guard {:needs :forbids}` and `:produces`, and a list of open gaps
(`:status :open :token :wanted :nearest :consequence`); the derived wiring reports
**unfed wants** and **dangling outputs**. Correspondence: PREV / memory seed ≈ `:initial`
(have); NEXT / gap seed ≈ `:want`; the blend ≈ a path of licensed patterns from haves to
wants; claude-1's three disagreements ≈ conflict (`:forbids` / inhibits edges), unfed want
(needed, not said → work, and the cascade's open-gap record with its `:nearest` pattern
is what a gap seed hands Nipper), dangling output (said, not needed). Consequence to test
in DERIVE: a projection's requirements could be expressed as a cascade's want tokens,
so that the existing wiring derivation reports a projection's unfed wants and dangling
material with no new checker.

Review page for Joe (wiring, what was checked, questions):
https://zone.hyperreal.enterprises/wip/arxana-incubator-wiring.html, rendered with
Graphviz from the validated EDN by `holes/labs/M-arxana-incubator/wiring-to-dot.bb`.
Joe's corrections to DERIVE-1/2 wait for that review (Joe, 2026-11-17).

**DERIVE exit: Met.** Joe, 2026-11-17: "OK, I think this is certainly enough of a DERIVE
sketch that we could move to ARGUE and see what we come up with". Open in DERIVE and
carried into ARGUE: whether a projection's requirements can be expressed as cascade want
tokens checked by the existing wiring derivation.

## 4. ARGUE

**Current argument: `holes/labs/M-arxana-incubator/ARGUE-revised.md`** (claude-1 with
claude-5, futon4 7452df9, at Joe's request 2026-11-17: "I'd like to see more of a balance
between war room and nomadology patterns"). It argues from both sides at once: the war
room (bringing the outside in; tested by whether drift is detectable) and the war machine
(bringing the inside out; tested by whether anything outside the stack is better for it),
meeting at D&G's "constantly shifting borderline" (Prop III), which the design builds as
two operations on one store: projection appropriates, dissemination cuts loose. Its
`@why` lines: `war-room/wr-8`, `war-machine/state-capture`, and `war-room/wr-0` as the
hinge ("an open model, an available exit, and a verdict it cannot issue to itself"). Each
design element cites both sides and where they cut (sharpest: the requirement list read
as one score against Prop XII, "comparison always presupposes the appropriation").
Quotations checked by claude-1 (21 D&G against `atp.txt`; pattern conclusions) and
spot-checked by claude-5. Four proposals in it await Joe: no aggregate score across
projections or people; the return port instrumented from the start (`wr-27`); a view
that cannot open a fibre is refused; a contributor's exit in an open form.

The subsections below are the working record that led to it, kept with Joe's dated
statements.

### Rationale citation

`@why war-room/wr-8-typed-files-are-sources-of-truth` — "Make the typed argument files the
canonical self-model and generate prose from them, so drift becomes detectable instead of
perennial." This design applies that ruling to documents in general: the typed records
(projections, spans, edits, requirements, evaluations) are canonical, and every draft
(incubator answers, Fellowship LOI, PLoP paper margin notes) is regenerated from them.
Draft 2's failure (0 of 51 sentences verbatim, 42 untraceable) is WR-8's "drift
undetectable because there was no machine-readable intent" in a new place.

Also: `war-room/wr-4-inhabit-before-building` — the design was reached by inhabiting
working demos (the variants views, the annotated edition, Mojira `vd`) before any
record type was proposed; `war-room/wr-22-logic-model-before-code-is-a-sanctioned-verify-method`
— the wiring diagram is checked by futon5's validator, and claude-1's replay of the PLoP
alignment history is a logic-model test available to VERIFY before implementation.

### Theoretical coherence

- **Nelson (IDENTIFY anchor):** content stabilised with permanent addresses (E1/E2, I1),
  documents as lists of pointers (E4), transclusion not copy (R1, I7), links that survive
  versions (R4 + DERIVE-2 §4 partial-deletion rule), side-by-side comparison (views). The
  design departs from Nelson where Joe directed: beams are optional (cursor-over
  highlighting), and statuses are evaluated rather than authored (not in Nelson at all).
- **Patch theory (Darcs, Pijul):** 'clusions as first-class patches, projections as
  channels with no privileged head (D2), conflicts kept as records. Departure: requirements
  need a satisfaction relation patch theory lacks (D4).
- **Bitemporal storage:** event time as valid time for every act (futon1b 5147aa8), with
  the time basis of each turn stated (D5).
- **Joe's model:** one text (the landscape) with drafts as projections and edits as
  removable 'clusions (E4, R2, unwire). Held throughout.
- **New since IDENTIFY, and coherent with it:** the tetrahedron (four manifestations of one
  argument held together in a workflow, not a static arrangement); locality (most checks
  turn-local, I5/D8); the NEXT/PREV 3/2 square for Nipper's two seeds; the possible
  identity of requirements with War Machine want tokens. None of these contradicts the
  IDENTIFY anchors; each specialises "traceability above all" to relations, time and
  work rather than to characters alone.

### Trade-off summary

- **No real-time co-editing.** No CRDT/OT; one author sequences edits (M-formal-arxana F2's
  condition). Several authors (T6) are supported by attribution, not by concurrent
  editing. Given up: Etherpad-style simultaneous typing.
- **Statuses are not stored on links (D3).** More records and evaluator runs; gained:
  "stale" is a query, and a defect count is possible later.
- **Lazy registration of the landscape.** No bulk import of 23,146 turns; Nipper's
  retrieval stays global and costlier, in exchange for a store holding only what documents
  use.
- **File-backed first for the incubator (D7).** Two storage paths for a few weeks; gained:
  the 2026-11-20 deadline does not depend on a build.
- **Deferred:** simulation run records, transcopyright and payment, DiagramProver rewrite
  rules, a full multi-author editor. Each has a hook in the types (E2, R4 `:computed-from`,
  R5 `:predicate`), so deferring costs no redesign.

### Generalisation notes

- **Within the stack:** the same types serve the four facets: organisation (readings and
  their corrections, N/N-1 strands), simulation (result files as manifestations,
  predictions as requirements), automation and proof (Lean/Clojure endpoints in
  alignments), and documentation (drafts as projections). The PLoP 2026 paper is the named
  end-of-mission test.
- **Beyond it:** nothing in the types is specific to Joe's turns. Any group whose working
  record is a stream of utterances (a lab's chat, a co-op's meeting notes, the plumbers of
  the incubator draft) can register it as a landscape and write traceable documents over
  it. What would change: identity and time sources (D5) and who may attest (D4).

### Plain-language argument

When people write with AI agents, the words drift away from where they came from: a
proposal ends up saying things nobody can trace back to who said them or why. This design
keeps everything people actually said as a permanent record, and treats every document as
an arrangement of pieces of that record plus clearly marked new writing and edits. Because
each piece knows its source, anyone can see who said what, when, and whether a claim still
holds after the code, proof or paper it depends on has changed. The same arrangement can be
re-cut for different purposes — a funding application, a paper, a status report — without
losing the trail. That is what lets a group get credit for its work and catch its mistakes.

### Pattern cross-reference

Survey of `futon3/library/` against DERIVE-1/2 (claude-5's Explore surveyor, 2026-11-17;
`~/notes/claude-5/argue-pattern-survey-2026-11-17.md`, three conclusions spot-checked).
About 40 patterns bear on the design, from `translation/`, `contracts/`,
`test-registry/`, `象/`, `apparatus/`, `hygiene/`, `measurement/`, `memory/`, `aif/`,
`writing-coherence/`, `cascades/`, `war-machine/`, `war-room/`. Most support it: 象/翻译契约
(the unit is a fragment, dropped material recoverable by offset) and 象/收回亦是行
(withdrawal is an act; unwire ends valid time) match it closely. Six cut against it, and
ARGUE revises DERIVE accordingly (DERIVE-2 §5, eleven revisions): act ids minted at the
act, not derived from content (象/行有定名); a failing requirement stays failing until
discharged (apparatus/open-conditions-live-in-a-ledger); attestations carry independence
(aif/no-self-certification); closed vocabularies for absences and evaluator kinds,
fixing a DERIVE-1/2 mismatch (measurement/ghost-as-typed-sorry,
memory/surface-the-closed-set); absences carry their search (process/spec-silence-pair);
Nipper's two seed lanes are not merged into one score (aif/currency-before-merge). Further
revisions from supporting patterns: alignments state their claim and a falsifier;
imported statuses name their donor; memory seeds resolve to turn spans; renderers declare
loss and refuse typed; views rebuildable from records.

### The war-machine side: bringing the inside out (2026-11-17)

Joe: "while we've dealt here with the "war room" side (bringing the outside in) we haven't
dealt with the nomadology / "war machine" side, (bringing the inside out). First of all,
I'd say that Arxana does both information synthesis (to bring a projection of the graph
onto the screen) and information dissemination (to bring edits back to the graph)."

Sources examined (claude-5): `futon3/library/nomad/` (8 patterns), `futon3/library/
war-machine/` (13), the close reading in `~/notes/nomadology/` (README argument skeleton;
claude-16's `CROSSWALK-DG-WM.md`, with Joe's 2026-10-29 Mozilla framing and 2026-11-08
Prop IX answer); quotations below checked against `src/atp.txt`.

**What the design already has on this side.** Synthesis is A-register → A-project →
A-render (graph → screen). Dissemination back to the graph is A-edit: new content,
scoped edits, nips and unwires are appended to the store, and become material for every
other projection. In D&G's terms this is the borderline the crosswalk found closest to
the War Machine: "royal science continually appropriates the contents of vague or nomad
science while nomad science continually cuts the contents of royal science loose" (Prop
III). A projection striates (fields, word limits, a form's grid); dissemination cuts its
contents loose again, so a passage written for the incubator is free material for the
LOI and the PLoP paper.

**What it lacks.**
1. **Following the material** (Prop III, `atp.txt` l.14899: "One is obliged to follow when
   one is in search of the "singularities" of a matter, or rather of a material, and not
   out to discover a form"; also Prop III: "following is not at all the same thing as
   reproducing"; Prop VIII develops the theme, l.16360, "artisans are obliged to follow in
   another way as well". Attribution corrected 2026-11-17 after claude-1's check; first
   written here as Prop VIII). The
   crosswalk named this as the War Machine catalogue's missing pattern. In Arxana it is
   Nipper's memory seed read as following Joe's turns where they lead, not filling a
   form; the gap seed is the form. The design gives both, but nothing yet lets the
   landscape propose a projection no requirement asked for. Candidate: a third seed kind,
   or a view of "material that recurs and is in no projection".
2. **Dissemination outward, and the return.** The data flow ends at renderers (views,
   form text, margin notes). Bringing the inside out also means the material leaving
   the stack (a submitted application, a published paper, a page others read and annotate)
   and coming back as others' edits, reviews and attestations (Murch, Rob, PLoP
   reviewers, Mozilla). The diagram has no port for that return.
3. **Exteriority** (Axiom I, and Joe's 2026-10-29 condition: the harness "must not depend
   on any one provider"). Outward forms must be open (plain text, HTML, EDN) and readable
   without Arxana; nothing should require the stack to read what it disseminated.
4. **The object is the nomos, not the count** (Prop IX: "Make the desert, the steppe,
   grow; do not depopulate it"; Joe's answer, "benefit should lead to viability"). The
   requirement list counts what is met; it should not become the object. The object is
   that material gets reused and contributors gain (traceable credit, skills).

**Patterns that apply** (conclusions verbatim, abbreviated):
- `nomad/letter-from-the-future`: "write the letter you want to be able to send in two
  months; every clause that is not yet true is a work item" — this is the requirement
  list applied to the Fellowship LOI (due in a month): draft the LOI as if sent, each
  untrue clause a known-failing requirement; write it to three addressees and build the
  sentences that survive all three first. A method for WP12.
- `nomad/self-certifying-artifact`: produce "artifacts whose validity is machine- or
  institution-checked". A projection whose every sentence carries its source is such an
  artifact for the incubator: its traceability is checkable by a reader, not asserted.
- `nomad/owner-of-the-problem`: "the instrument is a proposal, not an application".
- `nomad/author-the-yardstick`: when a field's bottleneck "has no accepted measure, build
  the measure". Traceability of agent-assisted writing has no accepted measure; I2's
  per-character check is a candidate yardstick.
- `nomad/skip-the-flood`: occupy the layers the flood vacates, "(orchestration, digestion,
  evaluation)". Arxana is digestion and evaluation.
- `war-machine/state-capture`: "A nomadic practice is captured when it begins reproducing
  itself rather than following external flows; the test is whether today's work closes
  an external sorry or only improves the infrastructure for closing sorrys." A guard for
  this push: each work package should close something outside (the incubator submitted,
  a PLoP alignment kept true), not only build Arxana.
- `war-machine/operational-not-decorative`: "The test is whether the artifact changes
  behaviour when it fails." Requirement evaluations must change what Joe or Nipper does
  next, or they are decoration.
- `war-machine/inhabitation-threshold`, `half-blind-observation`, `spatial-over-tabular`,
  `dynamic-over-static`, `ideal-actual-gap`: for the views (inhabited, whole, shaped, over
  time) and for the requirement list as an ideal/actual gap.

**Proposed additions, for Joe's decision:**
- A return port in the wiring: outward publications (O-published, in open formats) and
  an input for others' edits, reviews and attestations (I-returns), entering A-edit and
  A-requirements with their authors (T6).
- A third seed kind, or a view, for material that recurs in the landscape and is in no
  projection (following, Prop VIII).
- `war-machine/state-capture` as a VERIFY check on this mission: for each work package,
  the external thing it closes.
- `nomad/letter-from-the-future` as the method for the Fellowship LOI (WP12).

**Joe's decision and two further points (2026-11-17):**

> "🈸: The additions sound very useful. VERIFY in this sense relates to Bergsonian
> problematics rather than abstractions. Something related that occurred to me is that the
> Cascade Live looks like a graph projected into 2D space (a web page) but actually it's
> easier to think about it embedded in 3D (all graphs do), with the *outcome* layer a 2D
> surface that is spanned by the actions. Then we could think about "regions" of
> interest. Arxana spatialises things in higher dimensions but should also admit this
> projection."

- **Accepted and applied:** the return port (O-published in open formats; I-returns for
  others' edits, reviews, attestations) and the following seed are in the wiring
  (futon5 `arxana-incubator-exotype.edn`, re-validated: all eight checks pass).
  `war-machine/state-capture` as a VERIFY check and `nomad/letter-from-the-future` as the
  LOI method are adopted.
- **VERIFY as Bergsonian problematics:** VERIFY here asks whether the problems were stated
  well (Bergson: the true problem is posed, not given; a badly stated problem has no good
  answer), not only whether abstractions check. It meets D&G's nomad science,
  "problematic, rather than theorematic" (Prop III), and `state-capture`'s test of whether
  work closes an external problem. To be written into VERIFY's plan: for each work
  package, the problem it states and the evidence the statement was right.
- **Projection to an outcome surface:** a graph is embedded in 3D; the outcome layer is a
  2D surface spanned by the actions; "regions" of interest are areas of that surface.
  For the design: Arxana's records live in a higher-dimensional space (spans, links,
  evaluations, time), and a view must admit the projection Joe describes: actions as the
  coordinates of an outcome surface, regions on it as selections. A region is then a way
  of defining a projection (E4): the material whose actions land in that region. To be
  added to DERIVE §6 Views when VERIFY plans the views.

### Directed 'clusions: down to the outcome surface and back up (2026-11-17)

Joe: "'clusions are definitely "directed" edges; I think M-jvm-arxana or M-formal-arxana
talks about the relationship to RDF. Here we'd want to look at the practical applications
for 'clusions. Their directed nature is roughly what should give the ability to move
between a "one dimension down" projection and "the graph itself"."

On RDF, M-formal-arxana's ARGUE settled it (`holes/labs/M-formal-arxana/argue/`, P14):
RDF-star, named graphs, PROV and SHACL can encode the pieces; Arxana differs by enforcing
them in one registry and one write path. Not reopened here.

claude-5's reading of what direction gives, for Joe to correct:
- **Down.** Following directed 'clusions forward ends at sinks: records nothing points
  out of. Those sinks are the outcome layer (passages written, requirements met or
  failing, code realised). Pushing every record forward to its sinks is the "one
  dimension down" projection: the outcome surface Joe described for Cascade Live,
  spanned by the actions (the edges into it).
- **Up.** From a region of that surface, following the same 'clusions backward gives
  its fibre: everything upstream that produced it. The fibre *is* "the graph itself" over
  that region, and a natural definition of a projection (E4): the material whose
  actions land in the region. M-formal-arxana already used this word for F5: the fibre of
  `z ↦ z²` over `-1` is `{i, -i}`, kept as two points rather than collapsed. Fibres here
  keep alternatives the same way (two sources for one passage stay two).
- **Needs a declared grading.** Down/up is well defined only along 'clusion types read
  acyclically (`[:supersedes …]` is declared `:acyclic`; many types are
  `:bounded-cyclic`). A view names which types it follows; that choice is the "height".

Practical applications, by 'clusion and direction:

| 'Clusion (direction) | Down: forward to outcomes | Up: back to the graph |
|---|---|---|
| Span ref (source → passage) | the passage as read | its sources (T0, I2) |
| Edit (old span → replacement) | the rendered text | what it replaced (track changes) |
| Nip (seed → accepted span) | what was accepted | why it was proposed: memory, gap or following seed |
| Supersede (old → new) | the current version | its history |
| Alignment (paper claim → Lean/Clojure) | what realises a claim | which claims rest on a declaration; the fibre of a changed declaration is exactly what goes stale (claude-1's spread measure) |
| Requirement (requirement → subject) | met / failing on the surface | the work a failing region implies |
| N/N-1 link (prev → next) | the latest turn | the strand behind it |

So the requirement list, the stale set and "regions of interest" are three readings of one
operation, a fibre over part of the outcome surface, rather than three features.

**ARGUE exit: Not met.**

## 5. VERIFY (planning, not started)

Joe, 2026-11-17: "what I'd say about the claude-7 use case is that this is a good
candidate for VERIFY. I.e. if we can help resolve this use case then we know that what we
are doing is real and useful."

This makes the PaperPrinter endgame VERIFY's principal witness, in the sense Joe gave
VERIFY earlier (Bergsonian problematics: a real problem, well stated) and as
`war-machine/state-capture` asks (it closes something outside Arxana).

- [ ] **Witness 1, PaperPrinter (claude-7):** a97J08's "In all four cases, we obtain a
      contradiction" resolves to its rule versions, pinned Main.lean lines and earlier
      wordings with actors and times; a scratch rule change leaves the old rule
      superseded, marks every dependent sentence stale and no unrelated one; the cycling
      cases claude-7 reports are visible as cycles in sentence histories, and at least one
      fighting rule pair is found by DiagramProver's critical-pair check before a render.
      Design: DERIVE-2 §6; gaps: `GAP-ANALYSIS.md` pass 2 (G14–G19); data: claude-7's
      reply (`~/notes/claude-7/endgame-cycling-for-arxana-2026-11-17.md`, in):
      three kinds of cycling, to keep apart: (1) a chain of packets each fixing the last
      (a97J08 Q55→Q56→Q66→Q78→Q83, Q88 re-pin; one real undo, Q56 found at Q61); (2) two
      rules undoing each other (Q72b beta reduction vs the Q59/Q84 choose families,
      round three times; Q79→Q79b→Q79c rewrite direction); (3) words with several
      readings where a supersession was never retired (e.g. `Fin.cases`: S2, q68, q74,
      checked in matrix-r8; claude-5 counts 72 of 777 words with more than one distinct
      reading, claude-7 79). Word readings fit pattern + guard + replacement; proof-step
      rules live in `paper_walk.py`/`render.py` over typed terms and need hand
      extraction. Candidate pair for DiagramProver: R1 beta-reduce `app(lam x. body, a)`
      vs R2 read `app(lam n. choose(spec n), i)` as `x_i`, on a02J03
      `borel_pi_eq_generateFrom_Icc`; non-joinable without R1's family exemption
      (mfuton-share efdeba34, the Q72b merge), joinable with it. DiagramProver
      (futon3c `diagramprover/`: matcher, rewrite, rule) has matching and rewriting but no
      critical-pair check yet.
- [ ] **Witness 2, the incubator application:** submitted by 2026-11-20 with each field
      traceable (WP1, WP2).
- [ ] **Witness 3, PLoP 2026 alignment** (end-of-mission test): `plop-alignment-v1`
      round-trips through the records and `inject-plop-alignment.py` validates it.
- [ ] For each work package, the problem it states and the evidence the statement was
      right (Joe: VERIFY as Bergsonian problematics).
- [ ] claude-7's interview and gap-analysis pass 2 checked against the design as built.
