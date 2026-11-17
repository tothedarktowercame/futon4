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
**Basis:** the jvm-arxana backend (futon3c `src/futon3c/arxana/`) and the
futon4a demos (`futon4a/lisp/futon4a-formal-f*.el`, launchers in
`futon4a/scripts/`). Classic Arxana (futon4 `dev/arxana-*.el`) is a reference
for what include/transclude/identification meant, not a basis (Joe,
2026-11-17: "The classic edition could be a reference but likely not a basis").

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

The jvm-arxana column is the packet B source audit below (2026-11-17);
Classic remains the initial reference reading. Short source names resolve to
the repository paths specified in that audit.

| # | Case | Instance | Classic Arxana (reference) | jvm-arxana (basis) |
|---|---|---|---|---|
| 1 | Divergent variants | one passage edited differently for incubator and LOI | include/transclude are single-source; no sibling versions | partial — stored patches/alternatives, but one accepted head per artifact (`formal_f2.clj:146–204`). |
| 2 | Edits as attributable acts | draft-2's edits have no record at all | no | present — patch author/op-id plus dated application record (`formal_f2.clj:16–22,51–60`); historical edit-time import remains missing. |
| 3 | Suggested propagation | an LOI edit raises "also apply to incubator?" | no | missing — F2 submits to one artifact and reloads its head; no sibling propagation proposal (`futon4a-formal-f2.el:315–335`). |
| 4 | Three granularities | clip (sub-sentence), passage (one claim), slot (~150 words) | scholia anchor on passages | partial — arbitrary codepoint spans (`correspond.clj:198–220`); claim/slot semantics are not supplied. |
| 5 | Word limits as pressure | slot limit recorded as the reason for an edit | no | missing — patch/application schemas have no slot/word-limit reason; choice has a reason only (`formal_f2.clj:16–22,51–60,251–256`). |
| 6 | Round trip from outside | text pasted into Mozilla's web form and edited there | no | partial — shifted spans resolve, duplicated spans remain ambiguous (`formal_f1_relocation.clj:94–155`); no external-form placement/import workflow. |
| 7 | Cross-time queries | the T/T1/T2 example above | no | partial — dated records exist, but append uses default XTDB times and no adapter as-of (`futon1b_arxana.clj:821`; `store_http.clj:18–22`). |

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

## 2. MAP — questions (against jvm-arxana; classic is reference only)

1. **Sources.** F1 (`formal_f1.clj`) materialises settled 象 turns as Formal
   Arxana records. Can a turn from `~/storage/operator-turns/` be materialised
   the same way? Which identity and time fields does the record carry: is
   there a valid time distinct from the store's system time?
2. **Divergent variants.** F2 (`formal_f2.clj`, futon1b CAS as sole
   sequencer) sequences patches against an artifact revision. Can two patch
   lines branch from one base revision and both remain addressable (siblings),
   or is the sequence strictly linear per artifact?
3. **Copy.** F0/F1b2 relocation (`formal_f1_relocation.clj`) types copying as
   an outcome. Does a copied scope become a second scope with lineage to the
   first, or only an ambiguous candidate set?
4. **Clips and edits as records.** Can a `correspond.clj` assertion (turn
   manifestation → passage revision, `:quote`/`:exact-range`) be the clip
   record, and an F2 patch the edit record? `correspond.clj` ships only an
   in-memory repository: what persists today?
5. **Essays anchors.** Arxana Essays (`futon4/README-essays.md`) keep
   verbatim `:passage` anchors in `annotations.edn`. Same anchoring problem
   one level up?
6. **What is a passage?** Candidate: one claim (the cell-line story; the
   plumbers; the $300k split). Joe to judge from the first pass.

Packets (discovery, not build, 2026-11-17):
- **A (data):** re-anchor draft 2's sentences to source turns, as a file a
  demo can load.
- **B (code survey):** answer 1–4 from the jvm-arxana code and the futon4a
  demos, each answer citing file:line.


## MAP findings (packet B, codex-13, 2026-11-17)

Source audit: futon3c HEAD `fa56bec2a`, futon4a `05e0e6a`, futon1b
`5be8ce8`; excursion read at futon4 `180a1f9`. Unless qualified below,
`.clj` paths are under `futon3c/src/futon3c/arxana/`; `.el` paths under
`futon4a/lisp/`. This is a code survey, not a claim that the demos or live
stores were exercised. No JVM, launcher, importer or store write was run.
Existing tests were read as executable specifications, not reported as passing
this turn. The answers below follow directly from the implementation branches.

### 1. Sources — partly

**Yes to reusable materialisation machinery; no to an existing arbitrary dump
importer or the proposed bitemporal contract.** `formal_f1.clj:101–130`
accepts a settled-turn-shaped map plus sentence/fragment analysis, and the
fixture wrapper reads a fixed list of turn JSON and analysis JSON files. It
is not a loader for `~/storage/operator-turns/`. A dump turn can be adapted,
but missing analysis must not be invented to satisfy that fixture format.
`formal_f1.clj:169–200` ingests UTF-8 bytes and appends claims, evidence,
assessments, marks and external references through the public service.

Identity is explicit: artifact `xiang:<turn-id>`, logical revision
`<turn-id>@settled`, and a separate content-derived byte revision
(`formal_f1.clj:111–120,175–182`). External-reference identity includes store,
record, schema, valid-time and digest (`records.clj:105–114`). However F1's
helper hardcodes valid-time `2026-11-08` (`formal_f1.clj:76–82`). That must
not be used as the historical utterance time for a dump import. The newer
production reference builder already names Evidence Landscape `body/text`,
pins its bytes and uses `created_at` (`turn_mention_writer.clj:71–94`); it
requires an evidence id and is not a dump importer either.

**The `:valid-time` field is record data, not proof of XTDB valid-time
publication.** `futon1b/futon1b_arxana.clj:821–832` submits `[:put-docs table
doc]` without a supplied valid interval; revision/head writes likewise use
plain put-docs at `:729–748`. Its newest-evidence documentation explicitly
calls valid time the append's system time (`:420–423`).
`store_http.clj:15–24` advertises `:entity-as-of false` and
`:hyperedge-valid-time false`. Missing: a lazy historical-turn registration
contract with authentic T0, distinct clip/edit/place act times, temporal
publication and as-of queries. A corrected source needs explicit revision
lineage; neither an external-reference date nor a new digest alone implements
the proposed valid-time/system-time correction history.

### 2. Divergent variants — partly

**Two submitted patches based on one revision remain addressable; two
independently accepted sibling heads of the same artifact do not.**
`formal_f2.clj:16–22,146–164` persists a content-addressed patch with base,
artifact id, author, operation id and hunks before examining the current head.
For an old base it traverses stored applications, composes and rebases onto
that head; it does not preserve the old base as an independently advancing
branch. Successful ingestion CASes the observed head and appends a from/to
application (`:165–181`). A CAS race refuses rather than choosing another head.

Conflicting patches produce stored conflict/alternative records with result
text and digests (`:113–129,182–202`). These are real addressable alternatives,
not two editable document branches. Choosing an alternative requires the
conflict's original head still be current, then advances that one head
(`:221–273`). The editor submits its artifact id and reloads that head
(`futon4a-formal-f2.el:315–335,337–352`). The existing two-instance test
expects the second disjoint patch to be **rebased**, preserving both edits in
one head (`test/futon3c/arxana/formal_f2_test.clj:88–108`).

Missing: a deliberate fork act linking separately named variant artifacts to
a common passage revision, independent heads/arrangements, and a policy/UI
for suggesting propagation. Separate artifact ids are a possible substrate,
not an already implemented sibling-version workflow. Conflict alternatives
must not be sold as intentional variants.

### 3. Copy — no to a new lineage-bearing scope; partial relocation support

`formal_f1_relocation.clj:60–79` constructs a fixture that duplicates a scope's
text. `:94–120` evaluates registered strategies in the successor revision;
`:122–155` appends candidates and source/target revision ids as evidence about
the **unchanged interpretation claim**. Ambiguous means disputed/partial,
not a chosen new scope. `:157–166` stores those attestations; it does not mint
an act/place or derives-from record for the copied occurrence.

The existing test explicitly requires two candidates for the duplicated
text and unchanged original claims
(`test/futon3c/arxana/formal_f1_relocation_test.clj:67–95`). The F1 editor
exposes relocation records at point (`futon4a-formal-f1.el:306`); presentation
of candidates is not copy lineage. Missing: an explicit copy/placement act,
second scope identity, and the pinned-copy versus follow-version distinction.
For importing edited web-form text, relocation can help find surviving spans,
but cannot infer which copy act happened or its T2.

### 4. Clips and edits as records — partly

**A correspondence assertion can describe the clip's revision-pinned
relationship, but is not yet a durable attributable clip act.**
`correspond.clj:198–241` validates both registered manifestation endpoints,
revision ids, locators, direction, cardinality, methods (`:quote` and
`:exact-range` included) and verification. Its semantic identity explicitly
excludes timestamps (`:243–254`); validation requires neither clip actor nor
clip time. Repeating the same semantic assertion is a duplicate in the
in-memory repository (`:109–118`). Therefore repeated clipping acts at T and
T1 need their own act identity rather than pretending to be two different
correspondences. Attributable assessments are separate from the relationship;
stale correspondence is not diminished authorship.

The generic `CorrespondenceRepository` is a persistence seam, but its supplied
implementation is an atom (`correspond.clj:51–146`). The service **does** wire
queries when a repository is supplied (`service.clj:732–746`); the namespace
header's broad “service/store wiring later” wording is outdated for queries.
It does not supply a durable implementation of this protocol. Do not confuse
this with F3's persisted **different schema**: `[:correspondence 1]` links a
manifestation scope to an entity (`type_registry.clj:202–205`), and
`formal_f3_correspond.clj:44–54,159–179` publishes those claims and evidence/
assessments through the store. That is working step-to-declaration persistence,
not a persisted two-manifestation D4 correspondence repository.

**An F2 patch is already a useful edit record.** It records author, op-id,
base and codepoint hunks (`formal_f2.clj:16–22`; `text_rebase.clj:78–97`), and
an accepted application records from/to revisions and a clock-supplied event
time (`formal_f2.clj:51–60,175–177`). Missing for this use: original offline
edit time, edit rationale such as a slot limit, explicit passage/variant
lineage and the clip-to-edit-to-placement query. Conflict choice has actor,
chosen-at and reason (`:221–256`), but that is not a general edit rationale.

F6 is also relevant existing work: `formal_f6.clj:32–40` makes an immutable,
attributed, dated placement with argument/item endpoints, zone and order;
`:114–118` exposes placement history ids. It is a useful predecessor for
arrangements, not evidence that copy/transclude mode, form questions or word
limits already exist. The case-table statuses above separate these primitives
from the missing variant-document workflow.

## DERIVE-1 — Xanadu requirements from Nelson's texts (2026-11-17)

Joe, 2026-11-17: "Just like we did for Elephant-2000 I think we should build some
acceptance criteria that relate to what the original author (Nelson or McCarthy
respectively) was talking about ... I'd love to actually show him a demo that he'd be
interested in." → `holes/labs/E-arxana-variant-documents/DERIVE-1-nelson-requirements.md`:
20 rows (N1–N20) quoting Nelson by line from local copies in
`~/storage/references/nelson-xanadu/` (xanasimp 2012; ACM Computing Surveys 1999),
each against the current demo, and five acceptance tests T1–T5 with their bad cases.
36 quotations checked verbatim against the cited lines. Literary Machines and the
17 rules are not yet read from a primary copy.

## Joe's model: one text, many projections (2026-11-17)

> "I work on "one" text but get both the Incubator and the Fellowship drafts as
> projections of it. A possible bonus is that that "one" text *is the Evidence
> Landscape*. So all texts are have throughlines to my turns, though as we've seen, I
> may also want to hand-edit the text considerably. (But we should maintain
> traceability, the fact that the Draft 2 looks mostly hand edited is somewhat an
> artifact of the fact that I wasn't editing it in an Arxana-aware frontend, per
> Nelson's T2.) Indeed we could sort of think of the Evidence Landscape as my "model"
> and the hand-edit 'clusions as little LoRA-like style customisations"

Consequences for the records (claude-5's reading, for Joe to correct):
- **Base:** the Evidence Landscape (every turn, permanent; N1 holds already).
- **Edit 'clusion:** a hand edit is an attributed, dated 'clusion on landscape spans
  (replace / insert / delete), not text owned by a document. It is addressable and can
  be switched on or off: the t/T views already show base-with and base-without.
- **Projection:** a target form (incubator, Fellowship LOI) = a selection and order of
  landscape spans + the set of edit 'clusions applied + the form's limits. Documents
  become thin; neither draft owns its text.
- **Divergence by scope:** an edit 'clusion attaches to its spans; whether it applies
  in every projection that selects those spans or only in one is the edit's scope.
  Shared edits flow to both drafts by construction (case 3, "suggested propagation",
  becomes the default question "this projection only, or everywhere?").
- **The LoRA analogy, where it holds:** small deltas over a large base, composable and
  removable. Where it differs: these deltas are explicit and attributable, not learned
  weights. It becomes literal if recurring edits ("bogus" → "nogood", "a harness" →
  "the kind of system you're talking about") are later generalised into style rules
  learned from the accumulated 'clusions.
- **Effect on zai-13's Nipper design** (`~/notes/zai-13/nipper-design-2026-11-17.md`):
  `:typed` entries owned by a document become edit 'clusions on the landscape; the
  per-document EDL becomes the projection's selection. Nips stay as designed.
