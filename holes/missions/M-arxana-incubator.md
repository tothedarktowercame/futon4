# M-arxana-incubator — Arxana as an innovation hub, proved on the Mozilla applications

**Status:** MAP (2026-11-17; IDENTIFY exit met, Joe)
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

Later test case, named now (Joe, 2026-11-17): keeping Clojure, Lean and documentation
aligned for the PLoP 2026 paper on the War Machine (`p4ng/plop-2026*`, `sec-*-plop.tex`),
the "workflow centre" every facet of the tetrahedron touches. Not this mission's work,
but its end-of-mission test: the same projection, 'clusion and requirement types should
hold code, formal and informal manifestations of one argument in step. Classic Arxana's
use since 2005 is source material, read backwards from this recent use case.

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
- **Automation and proof: claude-7** — pending (deep in PaperPrinter). Joe, 2026-11-17:
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

**MAP exit: Not met.** Every question is answered and the table is complete; the
verdict is Joe's.
