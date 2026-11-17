# DERIVE-1 — types, invariants and data flow (M-arxana-incubator)

claude-5, 2026-11-17, first cut for Joe to correct. Built from MAP (mission §2, the
per-WP types table), `GAP-ANALYSIS.md` (pass 1), the Pijul study
(`~/notes/zai-6/pijul-for-arxana-2026-11-17.md`), the Nipper design
(`~/notes/zai-13/nipper-design-2026-11-17.md`), claude-1's MetaCA/locality note, the PLoP
alignment prototype (`p4ng/plop-2026-alignment.json`, schema `p4ng/plop-alignment-v1`),
and Joe's statements in HEAD. Decisions are argued in IF/HOWEVER/THEN/BECAUSE form;
where Joe has already ruled, the ruling is cited instead.

Organising rule (Joe): **traceability above all**. Every type below exists so that a
rendered character, or an evaluated relation, can answer who or what produced it, when,
from where, and whether it still holds.

## 1. Entity types

| Id | Type | Identity | Kind | Fields (minimum) |
|---|---|---|---|---|
| E1 | **Source** (a turn of the landscape) | futon1b `:evidence/id` (MAP Q1: the only id joining dumps, evidence, 象, F1); 象 record id kept as an alias where present | ingested, immutable | text sha256; `:speech-time` + `:time-basis` (`:xiang-created-at` where a 象 record exists, else `:ingestion`, per MAP Q1 and claude-13); session; author |
| E2 | **Manifestation revision** (a non-turn source: paper passage, Lean declaration, Clojure definition, result file) | existing `arxana_revisions` artifact revision id (sha256 of bytes) + an in-artifact locator | ingested, immutable | artifact id; repo + 40-hex commit where from git (as `plop-alignment-v1` pins it) |
| E3 | **Content** (new writing typed by a person) | sha256 of text + author + time | authored, immutable | text; author; event time |
| E4 | **Projection** | its own record id; versions by supersession (R3) | authored | name/target (e.g. `incubator/2026`, `fellowship-loi/2027`, `plop-2026`); ordered entries (each a span ref R1 or content ref E3); applied 'clusion set: pinned versions (static) or a query (live); optional `:as-of` |
| E5 | **Evaluation** | record id | derived (by a check) | requirement or relation evaluated; subject pinned (projection version, revisions, digests); result `:met :failing :stale :unknown`; evaluator (kind, version or script@revision); event time |
| E6 | **Typed absence** | record id | derived or authored | subject; what is absent (`:session-start :prev-absent :prev-pending :no-source :interrupted`, ...); producer; time; reason |

## 2. Relation types ('clusions)

All are immutable, attributed (`:actor`), dated (`:event-time`, published as XTDB valid
time per futon1b 5147aa8), and updated only by R3.

| Id | Relation | Endpoints | Content fields |
|---|---|---|---|
| R1 | **Span ref** (a locator; used inside other records) | E1 or E2 | codepoint `[start end)`; source digest; span text sha256 |
| R2 | **Edit** | target span refs (R1) in a projection | op `:replace :insert :delete`; replacement (E3 ref, for replace/insert); `:scope` (set of projection names, or `:every-derived`); reason (optional, e.g. `:word-limit`) |
| R3 | **Supersede v2** (`[:supersedes 2]`) | successor, predecessor (any record) | `:actor :event-time :reason :scope` |
| R4 | **Link** | two or more endpoints: span refs, records | `:kind` (`:comment :benefit :beneficiary :continues-in :computed-from :prev` ...) |
| R5 | **Requirement** | subject: a projection slot, a relation, or a record | question or criterion text; evaluator kind (see D4); parameters (`:limit 150`, `:script`, `:deadline`, `:manifestations`) |
| R6 | **Nip** | result span refs (R1) | query (`:strategy :terms :dials`); `:accepted-by :accepted-at` (or `:rejected`) |
| R7 | **Alignment** (a R4 link across manifestations, after `plop-alignment-v1`) | paper span, Lean decl revisions, Clojure def revisions, doc span | `:kind` (`:alignment :upgrade :question`); per-endpoint status carried by E5 evaluations, not stored on the link |

## 3. Invariants (checkable)

- **I1 Immutability.** No record is rewritten. Check: every store write is an append
  (futon1b's immutability ASSERT already refuses a changed record with the same id).
- **I2 Traceability (T0).** Rendering projection P at version v maps every character to
  exactly one of: a span of an E1/E2 whose digest still matches, or an E3 content record;
  and lists the R2 edits applied with actor and time. Check: a renderer self-test that
  fails on any unmapped or doubly-mapped character.
- **I3 Scope.** Edit e applies to P iff P's name is in e's `:scope` (or the scope is
  `:every-derived`) and e's target spans are among P's entries. Check: two projections of
  one passage, an edit scoped to one; the other renders unchanged (WP4's test).
- **I4 Pinned evaluation.** Every E5 names the revisions/digests of everything it read; it
  is `:stale` as soon as any of them is no longer current. Check: change one pinned input;
  every evaluation that read it reports stale, none other does.
- **I5 Locality.** Evaluations of span digests, endpoint existence, word limits, nip
  acceptance and N/N-1 links are computable from the record and its direct endpoints at
  write time; anything needing retrieval or a whole projection is marked `:global`.
  Check: every evaluator declares `:local` or `:global`, and local ones read no record
  outside their endpoints.
- **I6 Time.** `:event-time` never later than ingestion (futon1b already refuses future
  times); a source's `:time-basis` says whether its time is speech time or ingestion.
- **I7 No silent copy (T6).** Text in a projection that equals a span of a source must be
  entered as a span ref (or nip), not as new content; content records carry their author.
  Check: an editor test pasting someone else's words yields a span ref to them.
- **I8 Typed absence.** Where a relation is expected and absent (no N-1 turn, no source for
  a sentence, an interrupted stage), an E6 record says so; nothing is silently missing.

## 4. Data flow

1. **Landscape in:** futon1b evidence (operator turns) and 象 records → E1 sources,
   registered lazily on first reference (no bulk import of 23,146 turns).
2. **Manifestations in:** paper, Lean, Clojure, result files → E2 via the existing
   `:revision/ingest` path, pinned to commits.
3. **Nipper:** queries (its four strategies) over E1/E2 → candidate spans → Joe accepts →
   R6 nips.
4. **Writing:** in Emacs (futon4a) or Mojira, Joe arranges nips and spans into a
   projection (E4) and types new content (E3); changes become R2 edits with scope, never
   file overwrites.
5. **Checks:** evaluators (word limit, span digest, Lean build status, Clojure validation,
   script predicates, N/N-1) write E5 evaluations; local ones at write time, global ones on
   demand.
6. **Out:** renderers produce (a) the existing variants JSON for the current views,
   (b) plain text per field for the Mozilla web form, (c) margin notes as
   `inject-plop-alignment.py` does today.

## 5. Decisions

**D1 Where the records live.** IF the records could live in a new file format or a new
store, HOWEVER the stack already has jvm-arxana's typed, append-only clusions with
attribution, digests and now valid time, and MAP found its parts usable, THEN all new
types are registered jvm-arxana clusion types (and record types) stored in futon1b,
BECAUSE "the parts exist separately but nothing joins them" names a joining problem, not a
storage problem, and a second store would add another part.

**D2 No privileged head.** IF F2 advances one head per artifact, HOWEVER projections must
diverge by scope and the Pijul study says channels have no head, THEN a projection's
"current" version is the newest projection record of that name not superseded (R3); F2's
head stays as it is for F2's own artifacts, BECAUSE this gives WP5 its behaviour without
changing F2's tested semantics first.

**D3 Status is evaluated, not stored on the relation.** IF the alignment prototype stores
PROVED/VALIDATED on each note, HOWEVER a status is only true at the revisions it was
checked at and goes stale when either side moves (Joe's workflow point; claude-1's
re-expansion), THEN statuses are separate E5 evaluations pinned to their inputs,
BECAUSE that makes "stale" a query (I4) and lets a defect count and a next-repair ranking
be computed later without changing the links.

**D4 One requirement family, several evaluators.** IF Mozilla needs word limits,
HOWEVER simulation needs script-decided predicates, organisation needs timed obligations,
and PLoP needs correspondence across manifestations (gap analysis C01), THEN R5 carries an
evaluator kind `:word-limit | :predicate | :deadline | :correspondence`, each with its
own parameters, BECAUSE one name for four semantics is the conflict C01 warned about, and
separate types would split the "known-failing tests" Joe wants in one list.

**D5 Turn identity and time.** IF `turn_id` looks like an identity, HOWEVER it is a
per-session counter with thousands of nulls and duplicates (MAP Q1), THEN E1's identity is
`:evidence/id`, and speech time is 象 `created_at` where present, else ingestion time with
`:time-basis :ingestion`, BECAUSE claude-13 showed 象 times are close to speech and the
survey showed evidence `:at` is not; marking the basis keeps T0 honest for turns before
2026-09-28.

**D6 Edits target spans, not artifact bytes.** IF `[:text/patch 1]` already carries
codepoint hunks, HOWEVER it applies to one artifact head by CAS, THEN R2 reuses the hunk
form (start, end, replacement) but targets span refs inside a projection, and applying it
is rendering, never rewriting a source, BECAUSE Joe's model makes hand edits removable
'clusions over an immutable landscape ("LoRA-like style customisations").

**D7 The incubator path runs file-backed first.** IF the records above need jvm-arxana
builds, HOWEVER the incubator closes on 2026-11-20 and the tool must not stand between Joe
and submitting (IDENTIFY), THEN WP1/WP2 use the same record shapes as JSON files under
`~/storage/arxana-incubator/`, marked file-backed, and migrate into jvm-arxana once WP4
lands, BECAUSE the shapes are what the views and tests depend on; where they are stored can
change later without changing them.

**D8 Locality first.** IF any check could run globally, HOWEVER claude-1's locality reading
(Joe: "what we can do using classical code in connection with a turn") shows most checks
need only a record and its endpoints, THEN evaluators declare `:local` or `:global` and
local ones run at write time (I5), BECAUSE that keeps statuses current as work happens
and confines the expensive checks to when they are asked for.

## 6. Views

- **Existing:** futon4a `futon4a-variants-diff` / `-edition`, Mojira `vd`. They read the
  variants JSON; a renderer from E4 projections emits it. Mojira's op set is closed (MAP
  Q8): adding an op kind is a deliberate extension of `variants.js`, with tests.
- **New:** a requirements list (each R5 with its newest E5: met / failing / stale /
  unknown, and its evaluator), in Emacs and Mojira; a field editor that writes R2/E3
  instead of a file (WP9); an alignment view for R7 links with their evaluations (the PLoP
  margin notes, read from records).

## 7. Coverage check (2026-11-17)

**Against the gap analysis's 13 type gaps** (`GAP-ANALYSIS.md` §2):

| Gap | Covered by | Status |
|---|---|---|
| G01 run and output manifest | E2 (result files as manifestation revisions); a run record itself is not defined | **Deferred**: simulation evidence is out of scope (IDENTIFY); E2 + R4 `:computed-from` are the hooks |
| G02 prediction / registration | R5 with `:predicate` evaluator, created before its E5 evaluation | **Partly**: the "fixed before the run" order needs G03 |
| G03 "before" with evidence | E1/E2 times and `:time-basis`; N/N-1 `:prev` links (R4) for turns | **Partly**: turn order yes (circus item 8); epistemic precedence between a prediction and an observation is not defined |
| G04 computed-from | R4 `:kind :computed-from` with E2 endpoints pinned to revisions | **Covered** as a link kind; the evaluator that checks it is not specified |
| G05 native reading identity | E1 identity (`:evidence/id`, 象 alias) + R1 span refs | **Partly**: a reading (an animal's output) as its own entity is not defined |
| G06 reading correction | R3 supersede v2 with actor, time, reason, scope | **Covered** in shape; which readers must honour it is WP4's acceptance |
| G07 typed absence | E6 | **Covered** |
| G08 continues-in | R4 `:kind :continues-in`, distinct from R3 | **Covered** |
| G09 attributed supersession | R3 | **Covered** |
| G10 general nip | R6 over R1 span refs of E1 *or* E2 (so result files too) | **Covered** |
| G11 requirement/evaluation | R5 with four evaluator kinds (D4) + E5 (D3) | **Covered** |
| G12 projection and authored content | E4 + E3 + R2 with scope | **Covered** |
| G13 general span link | R4 over R1 span refs | **Covered** in shape; relocation beyond F1 is not specified |

**Against Nelson's tests** (`E-arxana-variant-documents/DERIVE-1-nelson-requirements.md`):

| Test | Covered by | Status |
|---|---|---|
| T0 version 3 grounded and extended | I2 over E4 + E3 + R1 + R2 | **Covered** (invariant + check) |
| T1 transpointing windows | views over two E4 projections; beams optional (Joe) | **Covered** for highlighting |
| T2 edited by hand | R2 + E3, I1 | **Covered**; the editor is WP9 |
| T3 links survive edits | R4 + R1; I2 for what survives | **Partly**: what a link does when part of its span is deleted is not yet a rule |
| T4 pullacross | E4 + R6 + "not yet used" as a query over sources | **Partly**: the "not yet used" query is not specified |
| T5 quote to context | R1 span refs | **Covered** (met today) |
| T6 several authors | E3 author + I7 | **Covered** in shape; the multi-author editor is out of scope |

**Gaps this check opens for DERIVE-2:** a rule for partial deletion of a linked span
(T3); the "not yet used" query (T4); a reading as an entity (G05); epistemic precedence
(G03); relocation beyond the F1 artifact (G13).

## 8. Not yet done in DERIVE

- [ ] Wiring diagram (futon5 exotype): the loop Landscape → Nipper → projection →
      evaluators → requirement list → Joe, with its timescales.
- [ ] Fidelity contract against donors: `plop-alignment-v1` (every field preserved or
      mapped), F2 patches, F6 placements, the variants JSON.
- [ ] Exact jvm-arxana type-registry entries (type ids, revisions, roles) for R2-R7, E3-E6.
- [x] Review against Nelson's tests T0-T6 and the gap analysis's 13 type gaps (§7).
