# M-arxana-incubator — Arxana as an innovation hub, proved on the Mozilla applications

**Status:** VERIFY (2026-11-17; ARGUE exit met, Joe)
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

Revised 2026-11-17 after the wider survey (Joe: "ARGUE exit is not met for the simple
reason that the plain text argument was never revised following our wider survey"). It
now carries both directions (bringing the outside in, and the inside out), work that
spans prose, proofs and code, and change over time; it supersedes the earlier version
here and §8 of `ARGUE-revised.md`.

Completed the same day with the PaperPrinter connection (Joe: "The connection to the
PaperPrinter use case is not obvious ... An endgame analysis is needed b/c different
formal systems have competing requirements"). The second paragraph adapts claude-1's
two sentences in `~/notes/claude-1/tetrahedron-and-metaca-2026-11-17.md` §7.5; the
analysis behind them is §7 of that note, summarised under VERIFY, Witness 1.

When people work with AI agents, what gets written drifts away from what was actually
said, decided and checked, and afterwards nobody can tell who contributed what or whether a
claim still holds. This design keeps everything said, decided and checked as a permanent
shared record, and builds every document from pieces of it, whether that document is a
funding application, a paper, or the code and proofs a paper describes, so that any
sentence can be followed back to where it came from and to what it depends on. When
something changes, the record shows which sentences and claims are now out of date and
what work that creates, instead of leaving people to find out later by accident.

The record holds rules as well as sentences: for example, the rules that turn a checked
proof into readable mathematics. There the proof checker, the human reader and the tests
each ask for something different, and a fix that satisfies one can break another. Because
every rule and every change to it is kept, two rules that keep undoing each other, or a
test that insists on wording the proof contradicts, show up as a pattern in the history
rather than as one more fix. The aim is then not a text with no flaws, which may not
exist, but a short, signed list of the flaws we accept, and rules that still read the
next proof well.

It works in the other direction too: every edit and new piece of writing goes back into the shared
record, free to be reused elsewhere, and whatever is published can be read and answered
without our tools. That is how a group gets credit for its work, catches its mistakes
early, and lets the work grow in other people's hands.

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

**ARGUE exit: Met.** Joe, 2026-11-17: "OK that argument is good enough to proceed to
VERIFY." (The earlier "not met" was for the plain-language argument, revised that day
after the wider survey and completed with the PaperPrinter paragraph, a467a2e.)

## 5. VERIFY (current, from 2026-11-17)

Why this witness comes first (Joe, 2026-11-17): "if we can't help with *this* specific
endgame analysis, it is going to be nigh impossible to do resolve the complex
interlinkings and overridings that go into assembling a complex Arxana document." The
PaperPrinter rule set is a small, checkable case of what any assembled document has:
'clusions that override one another in chains, where a later override can undo an earlier
one, and a stable preference underneath that should still hold at the end.

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
      **Endgame analysis (claude-1, 2026-11-17, §7 of
      `~/notes/claude-1/tetrahedron-and-metaca-2026-11-17.md`; readings from MetaCA and
      AIF, none yet checked against PaperPrinter's records).**
      - *Stasis:* a pin that keeps a wrong state (the m96A05 "second alternative" pin,
        re-pinned at Q88), or a residual list so broad it selects nothing (exotype D16).
        *Chaos:* packets that move failures between sentences without reducing them,
        helped by pins copied from the renderer's own output (Q79b). Kind 1 above is
        mostly descent, not a cycle.
      - *Frustration* is kind 2: R1/R2 admit no order that satisfies both, so writes to
        sentences only move the defect and a rule change (the guard) removes it.
        *Meta-frustration* (the four systems admit no rendering at all) has no confirmed
        case; the nested-∨ case resolves because one pin is wrong.
      - *AIF:* F = failing evaluations weighted by the precision of their system, plus
        rule-set complexity (rules, guards, exemptions). Lean is a constraint; the
        reader a graded preference; a pin a prior whose precision should match the
        standing of its acceptance (claude-7's "approved vs incidental wording" is that
        missing precision); a residual a recorded, scoped lowering of precision. A
        cycle is a run of packets along which F does not fall.
      - *Cycling vs "the stabilisation of an ongoing ability to learn":* D16 — with
        counts that never decay, exploration stops after at most λ_x/m − 3 trials; with
        forgetting at rate ω it recurs at about ω(λ_x/m − 2). The wanted state: F flat
        on the old corpus, small positive turnover (new declarations, pins lapsing when
        their rule is superseded), no overlap recurring, low cost per new declaration.
      - *What the analysis computes from the records:* sentence histories classed as
        descent, displacement or reversal; a conflict graph over rules (critical pairs,
        plus regressions observed across rules); a frustration measure (proxy:
        unguarded non-joining pairs plus overlaps met more than once); each tension
        classed as resolvable by guard/order, resolvable by superseding a requirement,
        or irreducible (an accepted residual with scope and release); rates of
        regression, latency to finding, displacement.
      - *Stopping criterion (not "100% clean"):* every failing evaluation classed and
        every irreducible one recorded as a residual; no non-joining pair without a
        guard or recorded order; over the last k packets displacement near zero and no
        overlap recurring; the best available packet expected to open more than it
        closes; declarations held out from rule-writing render as cleanly as the rest
        (the test of an ongoing ability to learn).
      - *Not decidable from the records:* whether a rendering meeting every right
        requirement exists (a per-case judgment), and what turnover ω to aim for.
      - Citations checked by claude-5: D16 formulas (`DESIGN-aif.md` 1883–1896), blend
        cooling 0.26 vs 0.45 (`NEXT.md` 22–23), "if they have a refuge, it is not a
        lava" (`PROGRAMME.md` 63, Joe's), all in worktree `exotype-c1-causal`.
      **Pilot P2 result (codex-13, futon3c 465db51c1, reviewed by claude-5 2026-11-17).**
      DiagramProver now has `critical-pairs` (inclusion overlaps only, guards on
      matches, bounded exploration; a hit on the bound reports `:inconclusive`, never
      non-joining). On a hand-written one-binder term of the a02J03 shape, {R1, R2} gives a
      non-joining pair (`choose(spec i)` against `x_i`), and R1 with efdeba34's guard gives
      none, while still firing on an ordinary beta-redex. Partial: R1 is a ground instance
      whose left side *is* the overlap term, because the matcher is first-order and
      cannot express beta over an arbitrary body. So the check confirms a conflict we
      wrote down; it does not yet discover one from a general R1. Discovery needs a
      schematic R1 (a family of bodies, or a higher-order seam), and the term should come
      from the Lean declaration, not be hand-written.
      Next: claude-7 checks the encoding against paper_walk.py (job
      invoke-1794942240695-23271-847a8196). Once the framework is agreed, claude-1
      (MetaCA paused) is to be recruited for the DiagramProver work (Joe, 2026-11-17).
      **claude-7's fidelity check** (`~/notes/claude-7/r1r2-fidelity-2026-11-17.md`;
      the guard description checked by claude-5 against `git show efdeba34`): the overlap
      term and R1 are right, but the stages differ. efdeba34 only blocked beta, so the
      family printed as `(n ↦ x_n)(i)`; R2 (`x_i`) arrives with Q84, merged in claude-7's
      tree and not yet committed. So: R1 alone, the defect (one extra "Choose" sentence per
      index, because the choice registry keys instances, not families); guarded R1, no
      conflict because nothing fires; guarded R1 plus R2, `x_i`. The real guard strips all
      leading lambdas and blocks on a `Classical.choose` spine head; saturation, proof
      terms and the Q37/Q84 alias rule are separate conditions, the alias tried first.
      Q87 (a02J03) is the same pair on a proper sub-term (`Icc` head, so guarded R1 fires
      and exposes the family inside): the case the check should find unaided. Q84 is a
      different pair (R2 against Q37's alias, m93J06 `(γ(t))(t)` for `γ(t,t)`), plus a
      name-capture bug in R2 that is not a rule pair. Second witness proposed: Q83's
      right-nested-Or merge against Q78's nested headings on `Or.elim h (λa.P) (λbc.
      Or.elim bc (λb.Q) (λc.R))`, inner `Or.elim` a proper sub-term; real instance a97J08
      `boundary_sum_aux_disjoint`; Q83's review plants already produce the divergence.
      **Passed to claude-1** (Joe, 2026-11-17: "this may warrant an extension to
      DiagramProver (which was possibly part of our long-term plans anyway)"): design
      of rule schemas so the check finds Q87 unaided, overlaps beyond inclusion, guards
      and rule order, the Q83/Q78 second witness, and results as conflict-graph records.
      Relates to futon3c `M-diagramprover` item S3 ("rule schemas with type variables").
      Job invoke-1794942396633-23277-227b9fda.
      claude-1's design: `~/notes/claude-1/diagramprover-schemas-for-arxana-2026-11-17.md`
      (term schemas instantiated as ground rules against a host; a precursor to S3; four
      packets). Packet 1, term schemas with capture-avoiding beta and named guards:
      codex-13, futon3c 02013976a, reviewed by claude-5 and fast-forwarded to master.
      Carried to packet 2: a ground instance records its `:position` but
      `cp/applications` rewrites every identical subtree, so with two equal redexes each
      instance rewrites both; pair search must apply at the recorded position.
      Packet 2, `host-pairs` (codex-13 4a0d38f38, review fix 16fdf2bd7, on futon3c master):
      with R1 = plain beta (no `choose` in it) and R2, the check finds the R1/R2 conflict
      on the Q87-shaped host unaided: non-joining at the inner family, both as the family
      applied to the outer binder (in the host, before any step) and as the family
      applied at `i` (after the outer beta step). The three stages are told apart by
      observation (instantiated choice keys 2 / 0 / 0; unreduced 0 / 2 / 0) with no
      non-joining pair in any stage. The T3 "no pair" expectation was wrong (nested
      redexes join); codex-13 stopped and showed it. Reports are not yet merged (the same
      pair is reported once per explored state): packet 4. Next: packet 3, priority and
      `settled-by`, on the Q83/Q78 Or pair.
      Packet 3, strategy and `settled-by` (codex-13 7e7ec98cf, on futon3c master): the
      Q83/Q78 Or pair is non-joining unordered and settled by `#{:priority :traversal}`
      (outer-first with the merge above the nested headings gives the 3-way split;
      inner-first lets the nested rule destroy the merge's pattern); the right-disjunct
      guard's effect is shown on the host where it fails. Q87's unguarded pair is
      settled by priority. Three of claude-5's expectations were wrong and codex-13
      stopped each time with an executable counterexample (T3, T4b twice). Settling is
      whole-host sensitivity to one change at a time, not a proof of necessity. Not yet
      tested: a pair settled by `:nothing` (the candidate accepted residual).
      Packet 4, conflict findings as EDN records (codex-13 f1faa2f51, on futon3c master):
      one finding per (rule pair, overlap, status) with occurrences, kinds, witness path,
      caller-supplied pins (checker sha, schema-set digest, mode, bound, event time) and an
      evaluation of "these two rules join": `:met`, `:settled` (guard, priority or
      traversal), `:failing` (settled by nothing), `:failing-accepted` (with a residual
      id; never `:met`), `:unknown` (bound hit); cross-host merge counts recurrences.
      Q87 with R1 and R2 at equal priority is `:failing` (settled by nothing: the Q72b
      state, where no recorded order decided between them); with R2 given priority it is
      `:settled`. The `:nothing` class has its own test (two rules of equal priority on
      one term). This completes claude-1's four packets. Remaining before Witness 1 can
      be called on real data: an importer from the typed Lean corpus (hosts are still
      transcriptions), spine patterns for the Q84 alias pair, and storing findings as
      Arxana records next to the rule 'clusions.
      **On real data (2026-11-17).** claude-7 described the typed corpus
      (`~/notes/claude-7/importer-source-2026-11-17.md`): the family redex exists only in
      the printer's intermediate terms. claude-1 designed the import
      (`~/notes/claude-1/importer-design-2026-11-17.md`). Packet A (codex-6, mfuton-share
      local branch `codex-6/beta-probe`, c66eb3289 + 40d06435f, not pushed): a log-only
      probe at the beta decision, output byte-identical on and off; 44 hits captured for
      a02J03 `borel_pi_eq_generateFrom_Icc`, 0 for m02J01, in
      `~/storage/arxana/importer/`. Packet B (codex-13 18a338615, review fix d678bd0bd, on
      futon3c master): the hits load as DiagramProver hosts (binary applications,
      constants as operators, proof flags as metadata, typed refusals), and with plain
      beta and the printer's family test at equal priority, 12 of 44 a02J03 hits give
      the beta/family conflict, settled by nothing; the transcribed Q87 host gives the
      same finding shape. Review fix: the family rule's `:choose-family` guard defines
      where the rule applies, so it no longer counts as settling a conflict
      (`:domain-guards`); before the fix the real finding read `#{:guard}` and would
      have been evaluated `:settled`. Still open: flags dropped by `subst`; the Q84
      spine pair; Step encoding for a97J08; storing findings as Arxana records.
      **Items 1 and 4 done (2026-11-17).** Metadata survives substitution (codex-13
      302023cc7). Findings as Arxana records, in memory (design DERIVE-2 §7; codex-13
      07a55ef04, 44a8dfcb7): registry types `:rule/content`, `:conflict/finding`,
      `:residual/accepted`; run evidence on attestations; a finding is stale at t iff one
      of its rules has a `:supersedes` record valid at t (direct predecessor only); the
      reader shows `:failing-accepted` only with a residual valid at t, never `:met`.
      Planted check passes: superseding R1 stales the R1/R2 finding and not R2/R3; the
      real a02J03 finding goes stale when beta is superseded. Live writes wait on futon1b
      valid time (Joe's restart). Items 2 (Q84) and 3 (Steps): claude-7's notes in,
      claude-1 designing.
      **Item 2 (Q84) done (2026-11-17).** Design `~/notes/claude-1/q84-alias-pair-design-2026-11-17.md`.
      Probe at the alias branch (codex-6, mfuton-share local `bccaee1d`, not pushed): the real
      m93J06 witness is one binder and two arguments (31 hits; none with two binders), not the
      design's two and two. With one binder the inner application is already saturated, so a
      domain guard `:maximal-spine` (pattern arity = the recorded argument count) keeps the
      model to the spines the printer actually visits. Model (codex-13 2a02d95de, on futon3c
      master): with exemptions removed, alias-apply and family-reindex form one non-joining
      root pair on every captured hit (31/31 two-argument, 8/8 one-argument); with the full
      rules `settled-by` is `#{:guard}`, the binder-suffix test on family-reindex; dropping it
      gives the pre-Q84 re-indexed γ (defect count 1); the provenance variant differs from the
      printer only on a planted host. Item 3: Step probe accepted (codex-12 `b90ed3f9`, local);
      the replay table is in progress: raw Q56's successor on a97J08 is not well nested, so
      states are now a tree or a flat sequence, and well-nestedness is an observation.
      **Item 3 (Steps) done (2026-11-17).** Replay (codex-12, mfuton-share local `f87a1048`)
      runs the real Python rules on copies of the captured Steps and writes a transition
      table (`~/storage/arxana/importer/a97J08-step-table-*.json`). Read in DiagramProver as
      opaque rules (codex-13 4b0bdf1d3; findings validator extended with `:rule-form :opaque`,
      8683730a8; both on futon3c master). Results on a97J08: raw Q56 against Q83 is
      non-joining, complete, ends B (not well nested) and D; Q78 and Q83 form no pair; the
      rule-set diff {Q83} against {Q78, Q83} is different, arity-consistent true against
      false (Q78 run after Q83 gives a split of 20 arms against arity 4; Q78 is uncalled code);
      the right-disjunct identity check matters on a97J08 (bypassed: 62 Steps become 60,
      outer arity 7). Superseding Q56 by Q83 marks the R3 finding stale without deleting it.
      All four VERIFY items are done in memory; live Arxana writes are next (valid time live
      since Joe's restart, 2026-11-17).
      **First live write (2026-11-17, Joe's go).** One batch of 13 records to futon1b
      (codex-13 012ff9dbd, on futon3c master; log
      `~/storage/arxana/diagramprover/first-write-2026-11-17.edn`): six `:rule/content`
      records, three `:conflict/finding` records with run-evidence attestations (a02J03
      beta/choose family, m93J06 Q84, a97J08 raw Q56 against Q83), and Q83 supersedes raw Q56 at
      its real merge time, 16:16:12Z. Read back from the store, in a separate process, by
      claude-5: 13 of 13 present; a02J03 reads failing, m93J06 settled, a97J08 stale (failing);
      the supersession is absent at 16:16:11Z and present at 16:16:12Z. This is the first
      staleness reading taken from the live store rather than from memory.
      **Endgame target (Joe, 2026-11-17):** "if the list is not too long we may be able to get
      it to 100%, but best to start with getting to a short list first." Next: claude-7's first
      cut of the remaining defect list, then findings and residual records for it.
- [ ] **Witness 2, the incubator application:** submitted by 2026-11-20 with each field
      traceable (WP1, WP2).
- [ ] **Witness 3, PLoP 2026 alignment** (end-of-mission test): `plop-alignment-v1`
      round-trips through the records and `inject-plop-alignment.py` validates it.
- [ ] For each work package, the problem it states and the evidence the statement was
      right (Joe: VERIFY as Bergsonian problematics).
- [ ] claude-7's interview and gap-analysis pass 2 checked against the design as built.

### The Witness 1 experiment: what it showed and what remains (claude-5, 2026-11-17)

**Set-up.** The question was whether Arxana's records, together with DiagramProver, can help with the PaperPrinter endgame: rules that override one another, where a later fix can undo an earlier one. Three real conflicts were taken from claude-7's history:
- R1/R2 (beta against the choose family) on a02J03;
- Q84 (the alias against the re-index) on m93J06;
- raw Q56 against Q83 (the Or flattening against the right-nested merge) on a97J08.

For each conflict:
1. A log-only probe in the printer captured the real input at the rule's call site. Each probe was shown to leave the rendered output byte-identical.
2. DiagramProver modelled the rules. Term rules were written as schemas. The Step rules could not be faithfully transcribed, so they were run as the real Python functions, and the results were read as a transition table ("opaque" rules).
3. DiagramProver searched for conflicting pairs and said what settles each one: a guard, a priority, a traversal order, or nothing.
4. The findings, the rules, their run evidence and one supersession were written to the live store, then read back at chosen valid times.

The work ran as claude-1's designs, built as small Codex packets with a review between each. claude-7 checked each design against the printer code.

**Results.**
- **a02J03:** beta against the choose family conflicts on 12 of the 44 captured terms, with the same shape as the hand-written Q87 term. At equal priority nothing settles it, so it reads failing (the Q72b state).
- **m93J06:** the alias against the re-index conflicts on every captured hit, 31 with two arguments and 8 with one. The binder-suffix guard settles it. Dropping that guard gives the pre-Q84 defect.
- **a97J08:** raw Q56 against Q83 does not join, and the search is complete: Q56 leaves a badly nested Step sequence and Q83 a merged split. Q78 and Q83 form no pair. Adding Q78 after Q83 gives a split with 20 arms against arity 4; Q78 is uncalled code. Q83's right-disjunct identity check does change the a97J08 output.
- **Live store:** 13 records were written in one batch and read back. a02J03 reads failing, m93J06 settled, and a97J08 stale. The supersession is absent one second before the Q83 merge time and present at it.

**What was learned about the Arxana design.**
- **The record types held.** Rules, findings, run evidence and supersessions each have their own type, and none had to change shape while DiagramProver changed under them six times. The staleness rule (stale at t if a direct predecessor has a supersession valid at t) gave the right reading on live data.
- **Valid time does real work.** The a97J08 finding was made today about a rule superseded earlier in the day, so it reads stale from the moment it exists. That is the true history, and it can only be stated with valid time separate from recording time. The restart that brought valid time live (Joe, 2026-11-17) was needed for this; the replay-ordering fix is futon1b 4b2d6d8.
- **Superseded findings are kept, not deleted.** The earlier belief can still be read at an earlier time.
- **Reverse lookup exists, but the reader did not use it.** Staleness was computed in the reader's process from records whose ids it already held. The first draft of this write-up said the store had no reverse lookup. That was wrong (corrected the same day, claude-5): futon1b's C3 sidecar answers `:role-reverse` queries (edge type, endpoint role, endpoint id), and live it returns the a02J03 finding for the choose-family rule as `:rule-b`, and the supersession for raw Q56 as `:predecessor`. `store_http`'s capability map says `:hyperedge-read false` only because no client method wraps the query; `bounded-query!` reaches it. Two limits remain: `:role-reverse` is not as-of (valid time is accepted only for `:record-by-id`), so a reader must read each returned record at the valid time; and results are capped at 200 per query, with a `:truncated?` flag. Edges are only ever added, so reading the current edges and then filtering by valid time gives the right answer. What is missing is a reader in futon3c that does this, not a store capability.
- **One record has no type.** The rule-set diff (what changed in the output when the rule set changed) exists only in DiagramProver; Arxana has no type for it. It is the record claude-1's §7.4 needs in order to class a change as descent, displacement or reversal.

**What was learned about DiagramProver.** Each of these came from a real capture contradicting the design, and each was a stop with a runnable check:
- **Guard roles.** A guard that defines where a rule applies is different from one that exempts a case. Treating the first as settling a conflict would have recorded a02J03 as settled.
- **Lean operators.** Imported Lean terms carry vector operators, which the findings check first rejected.
- **The Q84 witness.** It has one binder and two arguments, not two of each, so a `:maximal-spine` guard is needed to keep the model to the applications the printer actually visits.
- **Opaque rules.** Rules that cannot be transcribed faithfully can still be checked by running them. Their findings need their own form (`:rule-form :opaque`).
- **Badly nested states.** These are real states, the Q56 defect itself, and must be kept rather than repaired.
- **Conflict or change.** A defect that comes from adding a rule is a difference between rule sets, not a conflict pair.

Most of the corrections were to the checker, not to Arxana.

**What was learned about the process.** Codex seats stopped six times with a runnable check, and four of those stops were wrong expectations of mine. A larger packet, or a review that only reran tests, would have shipped those errors. Byte-identity tests kept the printer probes honest. claude-7's fidelity checks caught claims a model alone would not: for example, the Q78 and Q83 pair does not exist on a97J08.

**Measured against the Witness 1 criterion above.**
- Met: a fighting rule pair found by the critical-pair check before a render, in fact three.
- Met for rules: a rule change marks its dependent findings stale and no others. Not yet tested for sentences.
- Not done: a97J08's sentence resolved to its rule versions, its pinned Main.lean lines and its earlier wordings.
- Not done: cycles made visible in sentence histories.

So the rule side of the witness is done. The sentence side was not started when this was written; it is now built in memory (open question 5).

**Open questions.**
1. **Can a history show a pattern?** The plain-language argument says rules that keep undoing each other "show up as a pattern in the history". The live history has one supersession and three findings, which is too few. Writing the Q55→Q56→Q66→Q78→Q83 chain and the Q72b/Q84 round trips as supersessions would test this.
2. **A staleness reader against the store.** Build the reader that, given a finding id and a valid time, finds supersessions of its rules through `:role-reverse` and reads each at that time, instead of computing from a record set the caller already holds. Open: whether the 200 cap and the per-record reads stay cheap enough for an assembled document, or need an as-of variant of `:role-reverse`. **Done for findings (2026-11-17):** `store-finding-view` (codex-13 308cf4a6a, on futon3c master) reads a finding's view from the store alone: reverse queries for supersessions of its rules and for residuals, then every candidate read at the valid time; a stale or truncated index gives `:inconclusive`, never an empty view. Live, from ids alone: a02J03 failing, m93J06 settled, a97J08 stale because of 8ca7b206; one second before its event time the finding is `:missing`. Three reverse queries and one read per candidate per finding; cost at document scale still unmeasured.
3. **A type for the rule-set diff.** Should Arxana add one, and would it be enough to class changes as descent, displacement or reversal? **Type added (2026-11-17):** `:rule-set/diff 1` (codex-13 a328ab236, on futon3c master), one endpoint, the `:supersession` it describes; content gives each version's rule ids, normal-form digests, observations and capture pins, and whether output and observations differ. First instance built and validated in a dry run, not yet written: Q83 superseding raw Q56 on a97J08, version a ends not well nested (outer arity 2), version b in the merged 4-way split; different. Whether it suffices to class descent, displacement and reversal needs a chain of more than one supersession (question 1). **Written live 2026-11-18 (Joe's go):** `arxana:clusion:sha256:540e3a7f…`, created, read back equal, absent one second before its event time, and found from the Q83 supersession by `:role-reverse` (log `~/storage/arxana/diagramprover/rule-set-diff-2026-11-18.edn`). Its pins carry a host path to the corpus file, as the first 13 records do; Joe (2026-11-18): not a concern this early, but whether `arxana://` designations could work more widely is a question for later.
4. **The endgame list.** claude-7 and claude-1 are drafting the remaining defect list, sorted into fixes, DiagramProver-checkable conflicts and proposed acceptances. Joe's target is "best to start with getting to a short list first", with 100% possible if the list is short. No residual has been written live yet; each acceptance goes to Joe first.
5. **Sentences.** Sentence-level records (a rendered sentence's dependence on rule versions and pins) are the other half of Witness 1 and were not built. **Built in memory (2026-11-18):** futon3c 3e95d08ca adds `:sentence/role`, `:sentence/manifestation` and `:rule/application` and builds a97J08's 72 records from the 8B switch-offs: 17 rules (Q83's live record reused), 5 roles, 45 applications. Each application keeps every intervention for its (sentence, rule) pair, since two units changing the same function gave different wordings. The planted check passes through the store readers: superseding Q83 stales all five roles and no other rule's supersession does. **Written live 2026-11-18 (Joe's go):** 72 new records (16 rules, 5 roles, 6 manifestations including Q83's own outer wording, 45 applications; Q83's rule is the existing record), all created, read back equal, absent one second before their event time. Live, Q83's fibre is the five roles and the outer role is not stale. The 8 pre-Q83 history manifestations were held until their content said the wordings are declaration-wide; with `:sentence/manifestation 2` (`:scope :declaration`, futon3c 3af38e409) they were written live later on 2026-11-18 (Joe's go; log `~/storage/arxana/diagramprover/sentence-manifestation-scope-2026-11-18.edn`). The outer role's ten-render wording history now reads back from the store; the two revision-1 records (Q83, head) carry no commit time, so their order relative to each other comes from the commit ids, not the store. Log `~/storage/arxana/diagramprover/sentence-records-a97J08-2026-11-18.edn`; details and limits in DERIVE-2 §8, 8C result.
6. **Leftover branches.** The mfuton-share probe branches (codex-6 bccaee1d, codex-12 f87a1048) are local and unpushed. step_probe.py duplicates beta_probe.py's helpers, which should be merged into one if the probes are integrated. Integration is Rob's repo and Joe's call.
7. **What a record must carry besides its content** (2026-11-18; Joe: "how does this generalise further to the kinds of things that Arxana needs to record in general cases?"). claude-1 found that most rows of the endgame list carry limits stated only in prose (census extent, lower bounds, one probed path, provisional classes, classes set by ruling), and that the live sentence records share the gap ("exactly five roles" holds only for single switch-off at head). The conflict findings do not: they carry an evidence attestation with the checker, mode and state bound. A survey of the registry found the layers already exist: attestations (basis, standing, coverage, method), assessments (verdict, actor, policy, reason) and receipts, with their enum values checked on write. The sentence records simply did not use them (no attestation was asked for in 8C). Open, in order: (a) attest every observation, with the procedure and what it ranged over in the method (**done for the sentence records, 2026-11-18**: 59 attestations live, DERIVE-2 §8); (b) whether `:coverage :complete` should require that denominator; (c) where judgements' extent and reopen condition go (assessment or residual content, today split); (d) a home for rulings on a record contract (d6 decision stages are a candidate, unchecked); (e) checking submissions before they are final (**enum values: built for new type revisions, futon3c fd4fee77b; DERIVE-2 §9 R6(a) with its two limits**): enum values in clusion content are checked only by builders, endpoints by kind and, where declared, type revision; nothing checks a claim about existing records (Joe, after packet 9 swapped two ids). Requirements R1 to R6 in DERIVE-2 §9 (Joe, 2026-11-18: proceed with the writes; the design discussion is extending DERIVE). Notes: `~/notes/claude-5/arxana-generalisation-survey.md`.

## 6. INSTANTIATE (draft, from 2026-11-18)

Joe, 2026-11-18: "I don't want our mission to be blocked just because we decided to base VERIFY on a real-world experiment. 🈰: We can continue to finalise. 🈘 That said, we should really be abstracting what we've learned from this verification process, and starting to develop a more generally usable instantiation of Arxana, with reference to the argument we put forward in ARGUE. In particular, M-formal-arxana already exists, and the instantiation is what I'm going to need to use to work on the next draft of my Mozilla LOI and any further work with Cascade Live and the GCC. These are not disconnected, since the Mozilla LOI isn't about "selling" Arxana but about explaining the fact that we have or are very soon to have a working hypergraph model of the way work, benefits, and harms flow through a reasonably complex system."

VERIFY stays open on Witness 1's last items (the Q87 supersession and the census, waiting on claude-7's Q88 run); finalising work on DERIVE-2 §9 continues alongside (packet 11, R6(a)). This section abstracts what the experiment showed and proposes the instantiation.

### Frame (proposed 2026-11-18, for agreement before any dispatch)

Joe, 2026-11-18: "I think for INSTANTIATE, a good first demo of whatever system we are building would be useful would be to develop a browsable version of Cascade Live. Cascade Live has a lot of features and we've seen that we can deal with many of them in hypergraphical terms. The details of models, harm, etc., are really mainly to do with data (whether inside a MetaCA or inside of Cascade Live etc) and Arxana's primary jobs are to make these things visible and inference-ready. ... I'd say we should hold dispatches of this nature until we have agreed the frame; specific details like this need to be viewable and inferable within Arxana, but at this point I would consider them "invariants" rather than "packets to build"."

**The demo.** Cascade Live, browsable from Arxana records: every node and every edge of the cascade is a record on the live store, and the pages are views computed from those records, not regenerated HTML.

**What Cascade Live is today** (sources dated; READER-RULES apply):
- the cascade skeleton (futon3c `holes/excursions/CASCADE-live-skeleton-2026-11-08.md`, claude-6): apex (1) → wings (5) → ways of working (13) → packets (37) → items (368 live + 202 repair records), with Mozilla's 11 questions as tests on the whole (futon7 `holes/argument-graph/`);
- the data research (`~/notes/cascade-live-outline/HANDOFF-arxana-2026-11-09.md`): the lexicon (247 entities, 472 relation edges), the pending index (322 items with pending level, benefit line, harm line H1–H7, named entities, 115 user stories), Joe's closures and rulings;
- the current page: futon3c `holes/excursions/pipeline-pattern-cascade-live.html` (last commit 2026-11-08), served by WebArxana at `/live/cascade.html`; regenerated, not record-backed;
- record-side pieces: the GCC workplan types and `futon3c.arxana.workplan` loader (slice `~/notes/cascade-live-outline/arxana/all-items.edn`), mostly written to demo stacks (E-gcc-schema B10).
The skeleton's own hole H5 is what the demo answers: "No edge is declared in a pattern file ... so none carries a because and none can be refused in review."

**Arxana's two jobs, as Joe states them.**
- *Visible:* any node can be opened to show what it is, where it lives, what links to it and why (each edge with its because, who said so, whether accepted, and when).
- *Inference-ready:* the questions Cascade Live is asked can be answered by queries over records, with their limits stated: which items serve this benefit line, what a closure or a supersession reaches (the dependence reader), what an item needs and who decided it, which of Mozilla's questions an item bears on.

**Invariants, not packets.** DERIVE-2 §9 (R1–R6) and §10 (harm and benefit, with claude-1's AIF reading) are kept as invariants the demo must respect wherever its data touches them: an observation states what it ranged over; a judgement says who, why, how far, what reopens it; a preference says who stated it, and only self-stated ones steer; effects on different parties are never netted; a harm names its quantity (risk, model error, ambiguity, information, cost, margin) when it is computed, and is a count with its method when not. Types are declared only when a view or query in the demo needs them, and then from the invariant.

**How it is judged.** The demo is done when (proposed): (1) every level of the skeleton is browsable from records, with each displayed edge's because and source; (2) three named questions are answered by store queries and shown in the page with their limits, one per job (a benefit line's items; a change's reach; a Mozilla question's supporting items); (3) the page and the existing `/live/cascade.html` can be compared side by side, and every difference is either a record or a stated gap.

### Plan (2026-11-18, after Joe agreed the frame)

Joe, 2026-11-18: "Yes this is a good frame." ... "I believe we have the framing and requirements we need, that they have been woven into a design, and that that design has been sufficiently validated. 🈰 To do INSTANTIATE, typically everything would be obvious from the work done in the mission so far, and we could simply build some Codex packets. However, because the mission has grown and is complex, I think the first thing that we need to ask Codex for is not a "build" but the suitable scaffolding in the form of invariants, core.logic or Reazon (for an Emacs-facing variant) and perhaps ramo similarly for any code-server facing variant, but above all NOT launching into simply building features without having something that we can understand in detail. ㊝ Therefore I would prioritise making the next steps with Arxana "self-documenting" insofar as we should be able to easily browse and understand the design choices that go into the system, not just have them sit in a markdown file, code, or Lean. The old futon4 "Docbook" was a reasonable example of what we could accomplish in that era, but the futon4a era needs a new start with this. 🈲 Therefor, while I would like Cascade Live to be a "demo" I propose we do a parallel build: Document Arxana's features in one demo, and document which aspects of Cascade Live they make browsable, interactable, and inferrable in another demo. If you want a clear division of work, we could make one of the demos in Emacs and one in code-server, then port them both to the opposite surface later."

**Two demos, built in parallel, one authority.**
- **Demo A, Arxana documents itself (Emacs first).** The design choices (types and their revisions, invariants, rulings, the requirements R1–R6 and §10's harm/benefit invariants, the decisions made on Codex stops) are records, browsable and checkable, not prose in markdown, code or Lean. Emacs because the lineage is there: futon4's Docbook (`README-docbook.md`: stubs, headings and entries in the store, invariants on linkage, an Arxana browser view) and its Live Invariants view (`README-invariants.md`, reading `futon-stack-invariant-model.edn`), and futon4a (the bounded Emacs client of the JVM authority). Relational checks in Reazon, as futon3c `emacs/agent-chat-invariants.el` already does.
- **Demo B, Cascade Live made browsable, interactable and inferable (code-server first).** For each Arxana feature from Demo A, which aspects of Cascade Live it makes browsable, interactable or inferable, shown on the cascade itself (§6 frame). code-server because the current Cascade Live page is web (`/live/cascade.html` in WebArxana). Relational checks in ramo (a JavaScript port of miniKanren, github.com/wjlewis/ramo; not yet in the stack).
- **One authority.** Both read the JVM Arxana store; the invariants are stated once as relations, checked by core.logic in the JVM (futon3c `logic/` has 36 files, 14 of them using core.logic (codex-13's count; the earlier "36 namespaces of this kind" was wrong), e.g. `structural_law.clj`: paired edges, dangling references, enum validity), and mirrored in Reazon and ramo for the client surfaces. Each demo is ported to the other surface later.
- **Link between them:** Demo B cites Demo A's feature records, so "which feature makes this visible" is itself a link.
- **Shared backend, and a coverage invariant** (Joe, 2026-11-18: "the demos should share the same M-jvm-arxana backend features, just different content and displays (and proofs of invariant behaviour). Presumably they WON'T drift much from each other in terms of features, but in any case the Arxana self-documentation should *document* any features used in the Cascade Live demo."). The demos differ only in content, display and the invariant proofs they show. Every backend feature Demo B uses must have a Demo A record documenting it; this is itself an invariant, checked like the others (a Demo B view or query citing a feature with no Demo A record is a violation).

**First Codex work: scaffolding, not features.** In order, each its own small packet with a review between:
1. *Discovery:* inventory the existing invariant machinery (futon3c `logic/`, `futon-stack-invariant-model.edn` and its Live Invariants view, Docbook's store-backed entries, the Reazon checks in futon3c `emacs/`), and the Arxana design invariants as stated (DERIVE-1, DERIVE-2 §2–§10, ARGUE, mission §5–§6), and propose one catalogue shape for "a design choice as a record" (statement, source, status, the check that enforces it if any, what it constrains). No code.
2. *The catalogue as records, with core.logic checks* for a first handful of invariants that the live store already exercises (for example: an observation has an attestation with its denominator; a supersession links clusions; a content value is in its declared enum).
3. *The Reazon mirror* for Demo A and *the ramo mirror* for Demo B, each checked against the core.logic result on the same records.
Features (views, pages) come after these, and only where a view needs them.

**Step 1 result (codex-13 packet 14, 2026-11-18; report `~/notes/codex-13/design-catalogue/REPORT.md`).** The existing machinery already separates three things the catalogue must keep apart: a declaration (an invariant family in `futon-stack-invariant-model.edn`, 19 families, 9 operational and 9 candidate by its own labels; a law in futon3c `logic/`), an execution (`invariant_runner.clj`: `:state`, `:loaded?`, `:violations`; a dormant domain is not a pass) and a violation or obligation (`obligation.clj`, `arxana_bridge.clj`, legacy hyperedges, not immutable Arxana records). Live Invariants' "operational" is a label, not a passing run. Of about 34 Arxana design invariants found in DERIVE-1/2, ARGUE and §5–§6, a few are enforced in code (registry immutability, valid time, enums via `write/execute`, residuals never make a failure met), several are partial, and most are stated only. Confirmed again: futon1b's append path checks the envelope, id prefix and subject only (`futon1b_arxana.clj:611–635`), so "enforced on write" means through futon3c `write/execute`. ramo 0.1.2 runs under code-server's bundled Node (`run()(q => eq(q,3))` gives `[3]`).

**Catalogue shape (decided, claude-5, from codex-13's proposal).** New types, because no existing type carries a design-choice contract (`:rule/content` is a rewrite rule; the d6 stages carry decision payloads, not applicability or checks):
- `:design/choice` (content: key, statement, kind `#{:constraint :ruling :decision}`, sources with repo/revision/path/section, lifecycle `#{:proposed :accepted :superseded}`, applicability, rationale and alternatives);
- `:design/check` (entry point, runtime `#{:core.logic :reazon :ramo :clojure}`, input and output contract, implementation digest, fixtures);
- fixed-arity links: choice → constrained target (type revisions through external references, since registry entries are not clusions), choice → check, choice → the ruling that shaped it, and `:supersedes` between choice revisions;
- each run of a check is an evidence attestation on the choice, with result `#{:pass :violation :unknown :dormant :error :stale}`; a missing result is never a pass.
"Enforced" is never stored: it is derived from a choice's linked checks and their latest runs. Joe's coverage rule (every feature Demo B uses has a Demo A record) is one `:design/choice` with a core.logic check.

**The query layer is the authority** (Joe, 2026-11-18: "Regarding core.logic and Reazon, slight divergences are interesting of course, but I'd say that XTQL as the "CTRL layer" remains a kind of final authority. If we can't produce the information in a query, then we can't conceptualise it as a 'clusion, arrow, record, or assemblage or anything else. In a sense, the Kanren variants are "the same" for program behaviour. That is, broadly, an intentional part of the design."). Consequences for the scaffolding:
- The input facts of every check are the result of a store query (XTQL in futon1b, reached through the bounded query routes); core.logic, Reazon and ramo are interchangeable evaluators over those facts. A divergence between them is a finding about an evaluator, not a second answer.
- Queryability is itself an invariant: a design choice, check, feature or Cascade Live node whose facts no store query can produce is not yet a record, however it is described in prose. It is listed as a stated-only gap until a query produces it.
- The fixtures for the mirrors (step 3) are shaped as query results; a later step replaces the in-memory fixtures with the same facts read from the store, and the three evaluators are run on that.

**Step 2 result (codex-13 packet 15, 2026-11-18; futon3c 65c3ac80d, on master; in memory, nothing written to the store).** `futon3c.arxana.design-catalogue`: five types (`:design/choice`, `:design/check`, `:design/checked-by`, `:design/shaped-by`, `:design/constrains`), check runs as evidence attestations with the result vocabulary, `enforcement-status` derived from a snapshot, and five worked choices: manifestation scope (checked through the real write path), every view opens its evidence fibre (no check: status `:unknown`, reason `:no-check`), one rule record per function (core.logic over the sentence records), Joe's demo-coverage rule (core.logic over Demo B citations), and "catalogue links connect the right kinds" (core.logic over the catalogue, including hand-made links). Round 1 stopped because the only write-time port check is the composite validator, which treats every endpoint as a clusion; decided (claude-5): catalogue links are not compositions, so their kinds are enforced by the builder and by the fifth choice's check, and `write.clj` is unchanged. Checked by claude-5: a citation of an undocumented feature gives `:violation` naming it; a hand-made `:design/checked-by` pointing at a choice gives `:violation`; the stated-only choice reads `:unknown`, not enforced. Not yet: `:design/shaped-by` has no worked instance; the Reazon and ramo mirrors are step 3.

**Step 3a result (codex-13 packet 16, 2026-11-18; futon3c e42fdc0b7, on master).** The Reazon mirror (`emacs/arxana-design-catalogue-reazon.el`) agrees with core.logic on 12 shared fixtures (`test/resources/arxana/design-catalogue-checks.json`, shaped as query results: ids and pairs, not records), passing and planted cases alike. Checked by claude-5: the ERT batch passes; changing one fixture's expected result (coverage-missing, from violation to pass) makes it fail; regenerating the fixture from core.logic reproduces the committed file byte for byte. Two evaluator differences found, both handled without changing meaning on finite facts: Reazon has no `nafc`, so negation is committed-choice (`reazon-conda`) over ground facts; and Reazon treats vectors as variables, so JSON arrays are encoded as tagged lists. Both depend on the facts being a finite, ground query result, which is what the query layer supplies.

**Step 3b result (codex-13 packet 17, 2026-11-18; futon3c 7fbeddba3, on master).** The ramo mirror (`js/arxana-design-catalogue-ramo/`, ramo pinned at 0.1.2 with its lockfile, `node_modules` ignored) reads the same fixture file and agrees with core.logic on all 12 cases, under code-server's bundled Node and the system Node. Checked by claude-5: 14 Node tests pass under code-server's Node; changing coverage-missing's expected result makes exactly that test fail. ramo has no `nafc` either: absence is a separate bounded witness query over the finite facts. So the three evaluators agree on finite ground query results, and each handles negation its own way, which is the condition Joe's ruling relies on.

**Step 4a result (codex-13 packet 18, 2026-11-18; report `~/notes/codex-13/catalogue-queryability/REPORT.md`).** The live query route (`/api/alpha/arxana/query`) offers ten bounded reads. They start from a known id: `:record-by-id` (with valid time), `:role-reverse` (by endpoint id; default 50, at most 200, with `:truncated?` and a stale-index refusal), `:clusion-endpoints`, `:attestations`, `:assessments`, `:attempts` (by subject), `:edge-join` (2–4 joined edge patterns over an allowlist of 12 type revisions, none of them `:design/*` or `:rule/content`), and three others. None lists records by type or selects by a content field. Consequences for the catalogue:
- records with no endpoints (`:design/choice`, `:design/check`, `:rule/content`, `:sentence/role`) cannot be found from the store unless their id is already known; so "every choice" is not yet a query result;
- `:attestations` caps at 200 and returns no total or truncation flag (checked by claude-5, `futon1b_arxana_index.clj:204–211`), so "the latest run of this check" can be silently incomplete: the store-side form of R1/R2's missing denominator;
- `dependence/reverse-records` sends no limit and gets the default 50;
- record content is stored as an EDN string (`:record/data`), so XTQL can return a record but cannot select on `:applies-to`, `:kind` or a qualname; that needs either client-side decoding of returned records or declared derived columns.
Proposed (not built): a bounded `:records-by-type` operation in the existing query dispatch (type revision, valid time, limit, fetch limit+1, explicit truncation), and a total/truncation flag on `:attestations`. Both are futon1b changes, so they need a server restart, which is Joe's call.

**Ruling on queryability, and a third demo** (Joe, 2026-11-18: "I've just realised that we might work on a 3rd demo, which would be an annotated version of my *Variants: annotated edition* Draft 2 LOI. The annotations could themselves be nipped from m40 or the Evidence Store. ... broadly Arxana should be capable of annotating anything, e.g., evidence store, derived views, objects with their designation in substrate-2, and so on. We are not going to backfill everything just to satisfy a style preference. That said, the style preference is good for something, and I think Arxana records "themselves" should follow the searchable-field variant."). So:
- **Arxana's own records** have their identity-content fields selectable in XTQL (searchable columns), not only an EDN string; this applies to the reserved Arxana tables, which are small, and can be derived and backfilled there.
- **Everything Arxana annotates** (evidence store entries, minus40 memories, derived views, substrate-2 objects by their designation) is reached by reference and decoded by the client; nothing outside Arxana is backfilled to make it searchable.
- **Demo C: an annotated edition of the incubator Draft 2** (`~/mozilla-incubator-draft-2.txt`; the draft whose 51 sentences lost their sources, §1 Motivation), annotations nipped from minus40 and the evidence store, building on futon4a's variants and annotated-edition views. It exercises the second point, as Demo A and the catalogue exercise the first.
- **Demo D, noted for later** (Joe, 2026-11-18: "create an annotated edition of the *Mozilla* RFP so that we can look at which parts of it we actually address. Ideal to use Nipper for that. ... What could be interseting would be to find ways to work across the annotated RFP and the actual proposal draft so that they stay aligned (a bit like Marimo/Pluto)"). An annotated RFP (`~/mozilla/PUBLIC_2027_2028_Fellowship_RFP-2.pdf`; text in `~/notes/cascade-live-outline/mozilla-rfp-2027-28.txt`), with Nipper proposing which RFP passages each draft passage addresses, and the two kept aligned reactively: a change in either marks the links that depend on it stale, as the dependence reader does for rules and sentences. After Demos A–C.
- **Rewriting Arxana history is allowed** (Joe, 2026-11-18: "if we come up with new requirements we *can* rewrite history by deleting records and reloading in bulk (for Arxana material which as you said there isn't that much of)... this may seem like an extreme measure but if new requirements appear it is better to do this sooner rather than later, and also I think better to do than special-casing all queries to deal with a small number of nonconformant historical records"). So the searchable projection is uniform (a clusion's searchable fields are its type's identity-content fields), with no per-type declaration and no exceptions for old records; packet 19 (codex-13) builds it in futon1b with `:records-by-type` and the attestation truncation flag.

**Step 4 query routes (codex-13 packet 19, 2026-11-18; futon1b b8b3fbd, merged to local master, not pushed, not yet running).** Round 1 stopped because the planned field index was SQLite, which XTQL cannot query; round 2 puts it in XTDB. Built: `:records-by-type` and `:records-by-field` in the existing `/api/alpha/arxana/query` dispatch, both XTQL over `:arxana_clusions` (the field query unified with a new `:arxana_clusion_fields` table), both honouring valid time, bounded (default 50, at most 200, fetch limit+1, `:truncated?`), refusing on a stale index; field rows written in the same transaction and valid time as the clusion (one row per scalar field, one per element of a vector of scalars, maps as one canonical EDN value); `:attestations` gains `:total` and `:truncated?`; `backfill-clusion-fields!`, idempotent, never run at boot. Checked by claude-5: the source diff, and the new tests rerun (2 tests, 31 assertions, covering truncation, as-of before write, an absent type, nested values not element-searchable, a second backfill writing nothing). Live use needs two steps by Joe: a futon1b restart (new routes; projection for new appends) and, separately, a go for the live backfill (historical Arxana records become field-searchable).

**Queries for the three demos, batched for one restart** (Joe, 2026-11-18: "I have an agent working on making futon1b safe to restart "whenever" but in the mean time if you can proceed with the mission, including designing additional XTQL queries as needed, we'll bring a bigger group of them online in the restart"). Derived from what each demo must produce by query (claude-5):
- **Demo A (Arxana documents itself):** every choice and check (`:records-by-type`, built); a choice's checks and their runs as one join (choice → `:design/checked-by` → check), which needs the catalogue link types admitted to `:edge-join` (its closed `edge-join-shapes` map, `futon1b_arxana.clj:305`, admits 12 type revisions, none of Arxana's own DiagramProver, sentence, design or workplan-serves types); the runs of a check (attestations whose method names the check id), which needs attestation fields searchable; what the store holds, per type (a census).
- **Demo B (Cascade Live):** items serving a benefit line, items naming an entity, what a change reaches: `:role-reverse` and `:edge-join` over the workplan types (`:workplan/serves`, `:workplan/names`, `:workplan/referent`), the same allowlist extension.
- **Demo C (annotated Draft 2):** annotations over a span of a pinned document revision (offsets overlap a range), and annotations pointing at an evidence entry or a minus40 memory: range queries on numeric fields, and searchable fields on external-reference records.
So the batch adds to packet 19's work, before any restart:
1. the uniform field projection extended from clusions to every Arxana record kind (external references, attestations, assessments), each row carrying its record kind; this is Joe's "Arxana records themselves" rule applied without exceptions;
2. a typed numeric value on field rows, and `:records-by-field-range` (field, lower and upper bound, inclusive/exclusive stated), for spans and counts;
3. `:type-census`: counts per type revision and per record kind at a valid time;
4. `edge-join-shapes` extended to Arxana's own two-or-more-role types, with a futon3c test that each entry's role set equals the registry's declaration, so the two cannot drift;
5. futon3c client wrappers for the new queries, and `dependence/reverse-records` asking for the cap it can use and reporting truncation.
Each query becomes a feature that Demo A must document (Joe's coverage rule), so each gets a `:design/choice` once the catalogue is written live.
- **Batch part 1 (codex-13 packet 20, futon1b 90aef61, local master, not pushed, not running).** The field projection covers all six reserved record kinds (clusions, external references, attestations, assessments, attempts, mark occurrences); for the non-clusion kinds, top-level fields plus one map level as two-element paths, so "runs of check X" is `:field [:method :check/id]`. Numeric values carry `:field/number`; `:records-by-field-range` takes explicit inclusive/exclusive bounds; `:type-census` counts per type revision and record kind at a valid time. Checked by claude-5: the diff; new tests (26 assertions) and packet 19's (31) rerun and pass. Limit: span overlap is done by the client intersecting two bounded range queries, which is incomplete if either is truncated (packet 21 asks for the single-query form). Packet 21 (admitting Arxana's types to `:edge-join`, from a registry snapshot with a drift test) is with codex-13.
- **Batch part 2 (codex-13 packet 21; futon1b abf2234, futon3c fd5c6c3ec; local masters, futon1b not pushed, not running).** `edge-join-shapes` is now built from a snapshot that futon3c exports from its registry (`futon3c.arxana.join-registry`; every clusion revision with two or more roles, 52) and commits into futon1b (`data/arxana/edge-join-shapes.edn`), merged with the 12 earlier entries, all of which agreed; 53 admitted in all. futon1b refuses to load if an earlier entry disagrees with the snapshot; a futon3c test fails if the registry gains a multi-role type and the snapshot is not refreshed. Checked by claude-5: with a planted two-role type added to the registry, the futon3c drift test fails, and passes again once removed; the futon1b join-registry tests rerun and pass. Part 3 (packet 22: single-query span overlap, futon3c client for the batch, the dependence reader asking for the full cap) is with codex-13.
- **Batch part 3 (codex-13 packet 22; futon1b 2cde1f9, futon3c 22b9df37c; local masters, futon1b not pushed, not running).** `:records-by-span-overlap`: one XTQL query unifying the start and end field rows with the record at the same valid time; half-open only (a closed interval is refused), empty stored spans (start = end) not returned, truncation over the overlap set itself. futon3c `futon3c.arxana.queries`: wrappers for all six new queries, passing `:truncated?` and `:total` through; `dependence/reverse-records` asks for 200. Checked by claude-5: the span test (spans touching the query at either end not returned; limit over the overlap) and the client tests against the real futon1b handler rerun and pass, together with the reader tests (10 tests, 76 assertions); the client tests need futon1b master on the classpath, so they fail until futon1b has the server change, as they did before I merged it.
- **Ready for one futon1b restart (Joe's).** It brings online: `:records-by-type`, `:records-by-field` (all six record kinds), `:records-by-field-range`, `:records-by-span-overlap`, `:type-census`, `:attestations` with `:total`/`:truncated?`, and `:edge-join` over 53 type revisions; field rows are written for new appends from then on. Existing readers are unaffected, except that `:attestations` replies gain two fields and futon3c's dependence reader asks for 200. Separately, a go for the live backfill (`backfill-clusion-fields!` over all record kinds; writes only derived field rows) makes the records written before the restart searchable by field.
- **Brought online 2026-11-18.** Joe restarted futon1b ("I restarted futon1b so all being well you can go on to step 2"); the new queries answer on the live store (a type census lists the live Arxana types). The backfill was run by claude-5 in pages of 500 records per transaction rather than the stock single-transaction function, after a dry count: 395,717 field rows (attestations 309,051; clusions 59,602; external references 27,034; assessments 30), 107 transactions, 2 min 15 s, the written count equal to the dry count. Checked on the live store: Q83's rule found by `:rule/name`; 8 declaration-scoped manifestations by `:scope`; 5 sentence roles by type; 45 switch-off attestations by `[:method :procedure]`; none of the scoped manifestations visible before their event time. Scripts and result: `~/storage/arxana/futon1b-backfill/`.

**Step 4b–c result: the catalogue live and read back by query (2026-11-18, claude-5, Joe's go).**
- *Running the ruling on live data first found two things the fixtures had not.* Term-schema rules (beta, family, the aliases) have no qualname and are not function rules; and raw Q56 and Q83 share a qualname as two versions of one function, Q56 superseded. The ruling now reads "a function rule record identifies one function ...; at a valid time at most one unsuperseded rule record names a function", and the fact set is produced by queries (`futon3c.arxana.design-catalogue-facts`: function patterns by field, minus supersession predecessors), leaving the three evaluators unchanged (futon3c 314e9fc89; test with the Q56/Q83 case).
- *Written:* 18 records (5 choices, 4 checks, 1 external reference to `[:sentence/manifestation 3]` in the registry file, 5 links, 3 check runs), all created. No run was recorded for the coverage check, because no Demo B exists yet to cite features; it reads `:unknown`, reason `:no-run`, rather than a vacuous pass.
- *Read back by query only:* choices, checks and links by `:records-by-type`; runs by `:records-by-field` on `[:method :check/id]`; the external reference by id from its link. Enforcement status from the queried records: scope, functions and links `:enforced`; evidence fibres `:unknown` (no check); coverage `:unknown` (no run).
- *Three evaluators on the queried facts:* core.logic, Reazon (ERT) and ramo (code-server's Node) agree on all three live cases (links, functions, coverage).
- *A gap found:* external references cannot be listed by `:records-by-type` (the server fixes that query to clusions), so "every external reference" is reachable only by field or through a link. A `:records-by-kind` listing, or letting `:records-by-type` take a record kind, would close it; not built.
- Files: `~/storage/arxana/design-catalogue/` (write log, live fixtures, scripts).

**Demo A, first feature (codex-13 packet 23, futon3c ee7b00631, on master).** A read-only `:catalogue/view` operation in the Arxana service: for each choice its statement, kind, lifecycle, sources, applies-to, checks with their latest runs, shaping rulings, constrained target and derived enforcement status, read by the same queries as the read-back and `:inconclusive` when any query is truncated or refused. Checked by claude-5: a fake store that truncates the checks query gives `:inconclusive :truncated`; the live read gives coverage unknown (no run), fibres unknown (no check), functions, links and scope enforced; tests rerun (3 tests, 32 assertions). The Emacs view over it (futon4a, private read-only endpoint on the live store) is packet 24.

**Demo A in Emacs (codex-13 packet 24, futon4a 779d215 with claude-5's review fix 4285834, on futon4a main).** `scripts/launch-catalogue-demo.sh http://localhost:7073` starts a private loopback JVM (port 7202) whose only operation is `:catalogue/view`; its store wrapper passes only `:records-by-type`, `:records-by-field` and `:record-by-id` upstream and refuses every write, and the endpoint refuses any other operation (recorded refusal in `examples/catalogue-write-refusal.json`). `M-x futon4a-catalogue` lists the choices with status faces; RET opens sources, checks with their latest runs and input pins, shaping rulings, the constrained registry coordinate and the enforcement reason; an inconclusive answer is shown as such. Checked by claude-5: launched against the live store and rendered (five choices, statuses as above), RET on `functions` shows its core.logic check and passing run; ERT 4/4. Review fix: the details buffer printed raw alists; it now prints one line per source, check and run. Stop with `scripts/stop-catalogue-demo.sh`.

**Debt as a feature** (Joe, 2026-11-18, after seeing the view: "We should maintain some clarity on "debt" as we go, which will itself be a cool feature for this and other builds, e.g., fibres say "none: no check enforces this choice"... it seems that's already built into the design with the amber vs green colouration."). Debt is what the derived status already says: a choice with no check (`:no-check`), a check with no run (`:no-run`), a stale or failing run, and, under the coverage rule, a feature used but not documented. To keep it clear as the catalogue grows: a debt summary at the top of the view (counts by reason, each reason opening its choices), computed from the same queried records and never stored; and the same summary for any other build that writes a catalogue. Noted for the next Demo A slice; not built.

**Backend features as catalogue choices (codex-13 packet 25, futon3c 21342c581, on master; in memory, not yet live).** Ten `:design/choice` records, one per backend feature Demo B will use (`:records-by-type`, `:records-by-field`, `:records-by-field-range`, `:records-by-span-overlap`, `:type-census`, `:attestations` completeness, `:edge-join` from the registry snapshot, the dependence reader, `:catalogue/view`, the futon4a read-only endpoint), each with sources pinned to the commit that built it and a `:design/check` running `backend-catalogue/probe`: finite cases with known answers, read-only, against a store or endpoint given explicitly. The runs are evidence about pinned fixtures (private XTDB nodes in the tests), attested with coverage `:partial`, not a certificate for the live store. Checked by claude-5: a store that drops `:truncated?` and an endpoint that accepts a write both give `:violation`; the tests rerun and pass.
- **Written live 2026-11-18 (Joe's go): 38 records**, 10 choices, 10 checks, 10 links and 8 runs. The runs were made against the live store and the live read-only endpoint, not the test transcripts (whose answers were built to match the probes): known answers from earlier write logs and readbacks (5 choices, so `:truncated?` at limit 4 and not at 5; Q83's rule by name; one run on the scope choice; one checked-by-plus-constrains join row; Q83's fibre the five a97J08 roles; the catalogue view complete; the endpoint refusing `:clusion/assert`). Range and span overlap have no run, because no live record has numeric span fields yet: they read `:unknown :no-run`, which is debt the view shows. Log `~/storage/arxana/design-catalogue/backend-catalogue-2026-11-18.edn`.
- **Found while viewing it:** most live catalogue views came back `:stale-index`. The XTQL catalogue queries were refused when the SQLite sidecar lagged and when the source sequence moved during the query, though neither protects an XTQL answer (one snapshot of the authoritative tables). Fixed in futon1b 45f7619 (both refusals removed; replies report `:watermark` and `:watermark/after`), reloaded from futon1b master into the running server by Drawbridge (no restart); futon3c cbdddb5e1 adds a bounded retry for any remaining stale answer. After the fix, three consecutive reads gave the full 15-choice catalogue. Remaining: a view takes about 10 s (about twenty queries, the run lookups over the 395,717-row field table), at the edge of futon4a's 10 s request timeout.

**Debt summary and a fast read (codex-13 packet 26; futon3c 8d76ca138, futon4a f9566eb, with claude-5's display fix on futon4a main).** Check runs now come from the indexed `:attestations` query by subject, hydrated by id and filtered to linked checks; a live read went from 12.6 s to 1.9 s (checked by claude-5: 1.8 s). The view derives a `:debt` section, never stored: counts and choice keys by reason (live: no-check 1, no-run 3, violated 0), coverage reported as not assessed until Demo B cites features (not as zero), and stale runs listed as not assessed because the records hold no dated history of implementation changes. In Emacs the summary heads the buffer; RET on a reason lists its choices. Checked by claude-5: a truncated `:attestations` reply gives `:inconclusive :truncated`; the test with a run set to `:violation` counts it; tests rerun (5 tests, 42 assertions; ERT 5/5). Joe, same day: an agent bringing up an Emacs view must not take his cursor; `futon4a-catalogue-display-function` lets an agent render without displaying and mark the buffer with its own cursor (futon3c `emacs/futon-agent-cursor.el`).

### What the PaperPrinter experiment showed, stated without PaperPrinter

1. **Change flows along recorded dependence, and the store can answer "what does this change reach?"** A producer (there, a renderer rule) is linked to what it acted on (a sentence) by an application record; a supersession of the producer makes everything in its fibre stale, read from the store alone at any valid time (`sentence-view`, `rule-fibre`, `store-finding-view`). Nothing in that is specific to rules and sentences: a feature and the turns it serves, a decision and the work it set going, a fix and the proofs it renders, have the same shape.
2. **Every claim of effect carries how it was established and over what.** "Q83 reaches exactly five sentences" is true only for single switch-off at head on one declaration, out of 23 tested interventions; the live attestations now say so (DERIVE-2 §8). ARGUE already asked for this ("absences carry their search", "attestations carry independence"); VERIFY found where it had been skipped.
3. **Four kinds of record, plus checking.** Things (identity only), observations (attested: method, pins, denominator), judgements (who, why, how far, what reopens), rulings on the record contract; and submissions checked against the store before they are final (DERIVE-2 §9, R1–R6).
4. **The process caught what the design missed.** Author ≠ reviewer, with Codex stopping on data that contradicted the packet, caught a wrong premise in nine rounds of the last two days (the k=m=2 witness, flat states, opaque findings, R7, the Q83 id, intervention multiplicity, swapped ids, 35 functions, string keys). Each stop was possible because the claim could be checked against committed data or the store.

### Work, benefits and harms as one flow

The LOI's claim (Joe, above) maps onto these shapes:
- **Work** is a producer with applications: a commit, a rule, a packet, a decision, each linked to what it acted on.
- **Benefit** is GCC's `[:workplan/serves 1]` to a benefit line, and E-gcc-schema §13b already requires that "a benefit claim rest on property claims that a test decides": in VERIFY's terms, an observation with its attestation.
- **Harm** is not yet in the record model: E-gcc-schema mentions it once (a WM want, "reduce harm"). VERIFY has the first concrete instances of its shape: a supersession that leaves dependants stale (Q83 over raw Q56), a reversal visible in a history (a97J08's wording at 50444f21, Q66 and Q78), a fix for one reader that breaks another (the endgame's competing requirements). A harm record would be an observation whose effect is negative for a named party, attested like any other, reached by the same reverse reading.

So the hypergraph model is: producers, applications, benefit and harm observations, judgements and rulings, with time; and the one reading that matters for the LOI is the fibre: from any change, who and what it reaches, for better or worse, and on what evidence.

### What exists, and what the instantiation needs

- **Exists, live on :7073:** the DiagramProver and sentence records (rules, findings, supersessions, a rule-set diff, roles, manifestations, applications, 59 attestations); three store-backed readers; attestation, assessment and receipt layers; supersedes; GCC workplan types (`:workplan/item`, `:benefit-line`, `:workplan/serves`), mostly written on demo stacks so far (E-gcc-schema B10).
- **Needed, in proposed order:**
  1. One general dependence reader in place of the three specific ones (`store-finding-view`, `sentence-view`, `rule-fibre` share the reverse-query, as-of read, stale/truncated handling): parameterised by the application edge and its roles. Small Codex packet. **Done 2026-11-18 (codex-13, futon3c 3414fc8d7, on master):** `futon3c.arxana.dependence` (`reverse-records`, `fibre`, `staleness`); the four readers (`store-finding-view`, `sentence-view`, `rule-fibre`, `role-manifestations`) rebuilt on it with their tests unchanged; a test on `[:workplan/serves 1]` (items serving a benefit line) shows it outside DiagramProver. Checked by claude-5: a supersession with its roles swapped does not stale. Found on the way: `[:workplan/serves 2]`, the revision whose `:item` is an entity, cannot take part in staleness, because `[:supersedes 1]` links clusions only; GCC items are entities, so "this item was replaced" cannot yet reach the benefit lines it served.
  2. Write-time checks (R6(a) in flight; R6(b) typed ports next).
  3. A harm observation, designed with the benefit claims of E-gcc-schema B8/B14 row 11 as one question, not two.
  4. A worked example in the LOI's own terms, all from live records: one piece of work (the Q83 merge), the benefit it brought (a97J08's finding settled, five case conclusions merged), the harm it carried or repaired (the reversal at Q66/Q78; the sentences stale under a supersession), each sentence of the example backed by a store query. This is what a reviewer of the LOI could be shown.
- **Links to the open registers:** E-gcc-schema B14 row 1 (are Joe's rulings inputs to a rebuild) is R5; row 2 (a policy for choosing among assessments) is R4; row 11 (benefit and property claims as types) is item 3 above. M-repl-circus owns B14; this mission would supply the record shapes, tested on PaperPrinter.

### Harm: the existing model (Thyra) and extensions (2026-11-18)

Joe, 2026-11-18: "Your proposed order sounds fine, with the requirement that we study the formal model of harm reduction (Thyra) and look at extensions." PaperPrinter follow-up moved to `holes/excursions/E-paperprinter-cleanup.md` (owner claude-7), same day.

**What exists** (survey by a claude-5 reader, sources spot-checked by claude-5):
- **The pattern.** `futon3/library/militant-pacifism/demonstrable-harm-reduction.flexiarg` (approved by Joe 2026-11-08): "Ask each deliverable for an optional, specific claim: whose stress or which tension it reduces, and what observation would show it ... Record the claim with the deliverable; review it later against what was observed. An absent claim is not a failure; a claim that observation contradicts is worth surfacing." And: "Do not score or rank on this field until there are observations to score against." `precept-of-non-harm.flexiarg` is the floor.
- **Harm lines H1–H7** (`~/notes/cascade-live-outline/disbenefits-avoided-2026-11-08.md`, claude-6; 128 items without a benefit word, sorted by that question): H1 the stack's records say something that is not so (32); H2 failures and losses nobody is told about (16); H3 Joe has to notice, nudge, re-send or wait (9); H4 the War Machine chooses wrongly, ties, or cannot choose (17); H5 state goes stale, drifts or piles up (9); H6 slow paths and wasted effort (6); H7 the model cannot be read or checked (11); none stated (28). "Of the 100 items that do state a trouble, 30 give no observation" that would show it reduced. Per-item TSV columns include `whose_trouble` and `observable`, both free text; `whose_trouble` is "nobody named" for 45 items.
- **Thyra.** futon3c `scripts/giraffe_party.py` reads the pending index's `harm` field; futon5 `lean-arxana/Circus/Characters.lean` specifies `thyraOrder` (items on a harm line, most ready first) with `thyra_only_harm_items` and the readiness monotonicity; `Circus/Basic.lean` carries harm as `Option String`. M-giraffe: Thyra's success is "an item on a harm line closed, or its harm observation went away", needing "repair records".
- **AIF reading** (futon5 `TN-gauntlet-party-aif.md`): Thyra is the risk term of G, "KL to preferences ('harm' = a violated preference)", silent under flat C.
- **A name clash.** futon2 `resources/wm/harm-rules.edn` has guardrail rules also labelled H1–H6 (destroy data, force-push, shared JVM restart, post as Joe, spend money, edit existing Lean): a different list.
- **Not found:** a harm record in the Arxana registry (only `:benefit-line`); any harm observation record; any link from repair records to harm lines; harm in the Mozilla drafts.

**Reading against VERIFY.** The pattern's THEN is the record structure VERIFY arrived at: a claim (whose stress, which tension), the observation that would show it, and a later review that can contradict it, which is an attestation with method and denominator (§9 R1) and the existing standing `:contradicted`. Its "do not score until there are observations" is R2/R3 (no grade without a denominator). Two harm lines are what the sentence records already compute: H1 (records that say what is not so) and H5 (state that goes stale) are the stale dependants that `futon3c.arxana.dependence/staleness` finds after a supersession.

**Proposed extensions** (for the harm/benefit design, §6 item 3):
1. A harm claim record: subject (the work item or deliverable), party (whose stress; a typed "nobody named" rather than blank), line (namespaced, e.g. `:disbenefit/H5`, so it cannot be confused with the guardrail rules), observable (what would show it reduced). Optional, per the pattern: an absent claim is not a failure.
2. A harm observation: an attestation on the claim (method, what was ranged over, standing `:verified` or `:contradicted`), so "its harm observation went away" is a dated record, not a status flip. For H1/H5 harms the observation can be computed by the dependence reader.
3. Repair records linked to the harm claims they address, which gives Thyra's success condition a query.
4. In Thyra's Lean model: harm from `Option String` to (line, party, observed?), and a theorem that closing an item never adds an open observed harm on its own line; ranking observed harms ahead of claimed ones would follow the pattern's "do not score until there are observations".
5. Per-party harm as per-party preferences (the TN's reading), recorded but not used to steer until a party's preference is stated by that party (the TN's dictating risk).
6. Typed ports (§9 R6(b)) on the new types from their first revision: the mechanism exists (`:endpoint/type-revisions`, used by the d6 types), so no separate packet is needed; a revision cannot gain them later.
Each harm or benefit claim's subject must be able to be superseded: the dependence test found that `[:workplan/serves 2]`'s entity endpoint cannot take part in `[:supersedes 1]`.

### Open

- Cascade Live: which of its records (packets, theses, the argument's steps) should be the second family for R2–R5, after the endgame census.
- Whether the general reader and the harm observation belong to this mission's INSTANTIATE or to M-repl-circus (owner of B14). Proposed: built here, handed over with the B14 rows they close.
