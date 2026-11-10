# E-gcc-schema — what goes in the GCC, and in what shape

**Status:** SPECIFICATION, for Joe's correction (claude-1, 2026-11-10, at
Joe's request through claude-13). Parts A–C are new. The draft manifest that
accumulated on 2026-11-09 and 11-10 (claude-13) is kept unchanged below them
as the record. This excursion certifies the closure of M-formal-arxana (Joe,
2026-11-10; Part C).
**Home:** M-formal-arxana (`futon4/holes/missions/M-formal-arxana.md`).
**Related:** M-repl-circus §"The giant connected component", M-jvm-arxana
(workplan stack, code↔docs), claude-6's lexicon and workplan data,
M-ways-of-working (institutions), M-giraffe (the strategic layer).

## How to read this file

- **Part A** defines the GCC informally, in one paragraph, and lists where
  the sources disagree.
- **Part B** is the specification: the record model, the rules, the node
  kinds, the link kinds, and who writes and reads each.
- **Part C** is the checklist this excursion must meet for M-formal-arxana to
  close.
- **The record** is everything from "Why now" down: the dated sections §1–§14,
  cited here as "§n". Parts A–C cite them and do not replace them.

Every item in Part B carries one of three marks:

- **[built]** Code exists. A commit or a file is cited. Nearly all of it runs
  on private demo stacks (B10), not on the live futon1b at :7073.
- **[decided]** Joe, or a mission record, has settled it, and no code
  implements it yet. The source is cited.
- **[open]** Not settled. Where an agent has made a proposal, it is given and
  named as a proposal.

What I checked is listed at the end of Part C.

## Part A — The GCC, informally

> The **GCC** (giant connected component) is one graph that joins up the
> things FUTON's work is made of: operator and agent turns, sessions,
> patterns, missions and other pending items, memories, the features of the
> stack, and the benefits they serve. It holds ids and typed links, not
> copies. A node points to where the thing already lives (the evidence store,
> the pattern library, a repo, minus40). A link is an Arxana record that says
> how things are related, who or what said so, and whether that has been
> accepted. A link that a rule can compute is kept as the rule, and a link
> that is wanted but does not exist yet is kept as an open question. The REPL
> Circus animals write it as turns happen, loaders write it from the repos
> and the lexicon, and Joe writes it when he introduces a term with `[[X]]`
> or rules on a proposed link. It is read by pattern retrieval for operator
> turns, by Giraffe when it sets priorities, by the War Machine and the maths
> tutor when they build cascades, and by Joe through sessionista marks and
> entity pages. Its purpose is to let the War Machine turn operator intents
> into actions it can choose among. Today it exists in part, on private demo
> stores.

Where each sentence comes from:

- *What it is and what it holds:* M-repl-circus §"The giant connected
  component" ("The GCC of patterns, missions, turns, sessions and memories is
  a derived graph in its own store"), extended by Joe on 11-09 ("user
  stories, beneficiaries, Ostrom institutions and so on … Agency, Giraffe,
  and Kangaroo represent real features … and should certainly be in our
  map", quoted in full under "Why now").
- *Ids, not copies:* M-repl-circus "Decided: where derived output goes"
  (11-05): "Endpoints are ids (evidence, pattern, mission, memory, file path),
  never copied text"; DERIVE-core §14 (index an outside store, never mirror
  it).
- *Who said so, and whether accepted:* the Arxana record model (B1).
- *Rules and open questions:* Joe, 11-09 (§1 last row, §4 rule 3, §12).
- *Writers:* M-repl-circus's animal table; WORKPLAN-INSTANTIATE (the
  loaders); §10 (`[[X]]`).
- *Readers:* Joe's list in M-repl-circus (11-05); §8 (sessionista).
- *Purpose:* Joe, 2026-11-07, M-formal-arxana: "What we are trying to do is
  get the REPL to build a Giant Connected Component or GCC which will compile
  intents into inference-ready actions."
- *Today:* §13 ("Agency live, GCC not"); the lexicon's `gcc` entry (status
  hypothesised); WORKPLAN-INSTANTIATE (records on the :7094 stack).

### Where the sources disagree

1. **Derived, or also holding first-hand records.** M-repl-circus says the
   GCC "can be rebuilt from scratch" from evidence and the library. §4 rule 3,
   §10, §12 and §13 put in it Joe's rulings (made with `a` and `r` on an
   entity page, not in a turn), word-rule switches, open wants and benefit
   claims. A rebuild from evidence does not recompute those. One reading that
   keeps both: computed links are rebuildable, and human records are inputs to
   the rebuild, like the library. That reading is mine and is **[open]**.
2. **Which route.** M-repl-circus (claude-8, 11-05) sends GCC output through
   futon1b's general `/api/alpha/hyperedges` batch route. DERIVE-wiring §3
   choice 3 keeps Arxana records off that route, because it is
   last-writer-wins and does no Arxana validation. Joe settled it on 11-09
   (§6): the GCC uses the Arxana route, and edges already on the general route
   are read as an outside source. M-repl-circus's text still names the old
   route.
3. **Is the GCC the compiler?** Joe's sentence has the GCC itself "compile
   intents into inference-ready actions". M-formal-arxana's dependency chain
   (item 3) and codex-21's compiler types (§1) have a graph, and compile passes
   over it that write candidate actions back into it. Part B follows the second
   wording: the GCC is the graph, and the compile is a writer and a reader of
   it (B9). The compiler contract is unwritten (§6).
4. **How much it holds.** M-repl-circus lists five kinds (patterns, missions,
   turns, sessions, memories). Joe's 11-09 list is longer, and §2 has
   fourteen rows. Part B takes the longer list and marks each kind.
5. **Whether it exists.** The lexicon says `gcc` is hypothesised, and Joe says
   "Agency live, GCC not". Yet about 5,500 records in the specified shape sit
   on the :7094 stack (B3, B4), and 1,961 `:memory/assert` edges sit on the
   general route (MAP-Q3). "Exists in part, not live" fits both; the lexicon
   entry would then read "partly".
6. **What feeds it.** M-repl-circus: the Circus feeds it. Joe, 11-09 (§8):
   sessionista is "actually also the basis for the GCC". These are two
   writers, not a contradiction, but no source says how a Circus animal's
   output and a sessionista mark relate when they concern the same turn.

## Part B — The specification

### B1. The record model

The GCC adds no record kind of its own. Everything in it is one of the
records M-formal-arxana built (DERIVE-core §17; code in futon3c
`src/futon3c/arxana/records.clj` and `type_registry.clj`).

| Element | Shape | Mark |
|---|---|---|
| 'clusion | `{id, type-revision, ordered role-endpoints, asserted-content}`. The id is a digest of the type revision, the ordered role-labelled endpoints, and the content fields the type declares. Who produced it and by what method are outside the id. | **[built]** C1, futon3c `aeb9dbc` |
| Endpoint | One of: a semantic entity, an artifact revision, a manifestation or scope, another 'clusion's id, an external reference. | **[built]** C1 |
| Type declaration | Immutable and versioned, `[type n]`. It declares the roles, the endpoint kinds each role admits, and the content fields that enter the id. | **[built]** `type_registry.clj` (last changed futon3c `f5a514fb5`) |
| Evidence attestation | `{subject-clusion-id, producer, basis, standing, coverage, method/version, source-refs}`. Basis: `exact`, `parser`, `model`, `human`, `kernel`, `composite`. Standing: `unverified`, `verified`, `contradicted`, `disputed`. Coverage: `none`, `partial`, `complete`. One 'clusion can have many. | **[built]** C1, C2 |
| Assessment | `candidate`, `accepted` or `rejected`, by a named actor. Assessments of one 'clusion coexist. No "latest wins" applies globally. | **[built]** C1, C2; Lean `conflicting_assessments_coexist`, futon5 `476153d` |
| Choosing among assessments | A reader that picks one needs a named policy (DERIVE-core §18). The entity view picks "the newest ruling decides" in code (futon3c `00f11cf05`, `4a52a6b18`). That policy has no name or declaration. | **[open]** |
| External reference | Store identity, record id, schema authority, valid-time or revision, digest. The outside store stays the authority: it is indexed, never mirrored (DERIVE-core §14). | **[built]** C3b, futon1b `d157125`, `80ad3f9`; futon3c `e73db6b` |
| Operation-attempt receipt | `not-run`, `refused`, `failed`, `ran-no-output`, `ran-with-output` (DERIVE-core §13). | **[built]** C3b |
| Write operations | `clusion/assert`, `evidence/append`, `assessment/append`, on the reserved Arxana route. | **[built]** C2, futon1b `528f606`; futon3c `34ab34b`, `35261e6` |
| Reads | Every read goes through the index `(edge-type, endpoint-role, endpoint-id, valid/event-time, edge-id)`. Aggregates are materialised with a watermark. Recursion is bounded, and a cut-off is reported as such (DERIVE-core §15, §17). | **[built]** C3a, futon1b `ad1f68a`; futon3c `364e714`, `f1d34fe` |

### B2. Rules

1. **Endpoints are ids, never copied text.** **[built]** C1.
2. **Nothing correctable goes into an id.** A correction is an assessment or a
   further attestation. **[built]** C1.
3. **A link that a rule can compute is kept as the rule** (Joe, 11-09).
   **[built]** for turn → entity: one `[:workplan/entity-match-rule 1]` per
   entity, its words and their switches as a `human` attestation, the
   backlinks derived, and a rebuild byte-identical (futon3c `4e39606ff`,
   `193fa3698`). A stored correction of one single occurrence (§4 rule 3,
   second half): I found no type or writer for it. **[open]**
4. **What is computed can be rebuilt** from evidence, the library, the repos
   and the stored rules. **[decided]** (M-repl-circus). Whether Joe's rulings
   and introductions count as inputs to the rebuild: **[open]** (Part A,
   disagreement 1).
5. **No read on a page or prompt path scans.** **[built]** C3a. The measured
   cost of scanning is in §1 (50–80 s).
6. **Every output has a named reader** (Joe, 11-05). **[decided]** No check
   enforces it (§6, "the formal step").
7. **Absence is typed.** "Not run", "refused" and "failed" are different
   answers from "no". **[built]** for Arxana operations (C3b). sessionista
   still shows "not indexed yet" and "stack down" as no backlink
   (TN-wm-tokens §3). **[open]**
8. **A want has a horizon.** Past it, the want is reported unresolved, not
   false (§12). **[decided]** No want record exists (B7).
9. **A proposed link counts until Joe rejects it.** Joe, 11-09: "it would be
   better if we could find a way to prepopulate the graph and let me correct
   it". Candidates are written with basis `model` and standing `unverified`.
   **[built]** futon3c `dfbde7e95`, `4a52a6b18`. §10 says other entities named
   in the same turn "are candidates until accepted"; the code now counts them
   until rejected, so §10's sentence is out of date.
10. **A word rule is switched on only by Joe.** An entity loaded from the
    lexicon gets no word rule. One comes from Joe's `[[X]]`, and it is on
    while X has a referent that the newest ruling has not rejected.
    **[built]** futon3c `b3dc73db6`, `00f11cf05`.
11. **A benefit claim is dated.** When it stops holding, its interval closes.
    It is not deleted (§13a). **[decided]** Not built (B8).
12. **Arxana records stay off the general hyperedges route.** Edges already
    there are read as an outside source (Joe, 11-09, §6). **[decided]**; the
    Arxana route itself is **[built]** (C2).

### B3. Node kinds

"Lives in" names the authority. The GCC holds an id for the thing, in the
form the fourth column gives. Counts are from WORKPLAN-INSTANTIATE. A
[built] row that cites no commit of its own rests on the workplan types
(futon3c `a95245a3a`) and their load (futon3c `9abe433b5`).

| Kind | Identified by | Lives in | In the GCC as | Mark |
|---|---|---|---|---|
| Feature (entity) | slug | claude-6's lexicon, `~/notes/cascade-live-outline/lexicon/` | `[:lexicon/entity 1]`; `status`, `live`, `tier` in an attestation | **[built]** futon3c `a95245a3a`, `dfbde7e95`; 254 rows, 251 slugs |
| Pending item (mission, excursion, ticket, T- item) | item id | `holes/**/*.md`, Giraffe runs | `[:workplan/item 1]` | **[built]** futon3c `9abe433b5`; 323 |
| Benefit line | `benefit/line` | claude-6's benefit reading | `[:benefit-line 1]` | **[built]** 75 |
| User story | its item | claude-6's workplan data | draft attestation on the item | **[built]** 116 |
| Turn | evidence id | evidence store (:7073) | external reference | **[built]** as the endpoint of `[:workplan/mention 1]` and `[:workplan/introduction 1]` |
| Session | session id | evidence store | external reference (M-repl-circus: "turn and session nodes are evidence-ids") | **[decided]** No type takes a session as an endpoint yet |
| Commit | repo + sha | git | external reference | **[built]** 323 `:filed` mentions |
| Pattern | library path, sha256 as revision | futon3 library | external reference (DERIVE-repl-circus-needs §1) | **[decided]** The types that take it are declared, with no writer (B4) |
| Memory | project + slug | minus40 (:7093) | referenced authority (DERIVE-core §14) | **[decided]** |
| Code scope, doc scope, Lean declaration | path + scope id at a revision | the repos | `[:code-definition 1]`, `[:doc-entry 1]`, `[:lean-declaration 1]`, `[:scope-in-revision 1]` | **[built]** F3 and the code↔docs stack (:7091) |
| Intent reading | 'clusion id | 象's per-turn analysis | `[:interpretation 1]` | **[built]** on retained fixtures, F1a, futon3c `249aa138a`. No production caller (MAP-Q3) |
| Candidate action, admission decision, receipt | 'clusion id | futon3c `arxana/compile/` | `:d6/*` types | **[built]** as the F4 demo, futon3c `2e642f30c`. Nothing feeds it from live turns: **[open]** |
| Agent seat, invocation, bell, promise | agent id, job id | Agency (:7070) | none | **[open]** E-evidence-cleanup `DESIGN-RECORDS.md` lists the needs |
| Institution statement | not settled | futon3 `library/institutions/`; Lean `Institutions.lean` | none | **[open]** "planned" (Joe, §7) |
| Port | — | — | none | **[open]** B5 |
| Want | — | — | none | **[open]** B7 |
| Benefit claim, property claim, outside alternative | — | — | none | **[open]** B8 |
| A specification, and each record kind it defines (M-formal-arxana, E-gcc-schema; 'clusion, attestation, …) | slug | this file and the mission | entity with referents | **[decided]** as a debt (§11). Not built: Part C item 4 |

### B4. Link kinds

**(a) Links with a declared type.** All in futon3c `type_registry.clj`. The
workplan rows rest on the same two commits as in B3.

| Link | Type and roles | Written by | Mark |
|---|---|---|---|
| item names entity | `[:workplan/names 1]` (`:item`, `:entity`) | claude-6's parser | **[built]** 454 |
| turn names entity | none: derived from `[:workplan/entity-match-rule 1]` | the backlinks builder; sessionista as text is shown | **[built]** rules for agency, arxana, giraffe, kangaroo-pouch |
| item serves benefit line | `[:workplan/serves 1]` (`:item`, `:benefit-line`) | three model producers, one attestation each | **[built]** 416 |
| item mentioned in turn or commit | `[:workplan/mention 1]` (`:item`, `:evidence`; content `mention/role`) | loader | **[built]** 323 |
| entity introduced in turn | `[:workplan/introduction 1]` (`:entity`, `:source`) | Joe's `[[X]]` | **[built]** futon3c `f5a514fb5`, `a3d871572` |
| entity refers to target | `[:workplan/referent 1]` (`:entity`, `:target`; content `referent/role`) | Joe's `[[X \| r1, r2]]`; the lexicon loader, as candidates | **[built]** 589 candidates |
| turn reads as intent | `[:interpretation 1]` (`:source`, `:intent`, `:target`) | 象 | **[built]** on fixtures |
| code scope corresponds to doc scope | `[:correspondence 1]` | code↔docs | **[built]** F3 |
| turn proposes pattern | `[:pattern/proposal 1]` (`:turn`, `:pattern`; content `intent`) | 驴: three producers as three attestations on one 'clusion | type **[built]**; no writer: **[decided]** |
| pattern occurs in a window of turns | `[:pattern/occurrence 1]` (`:pattern`, `:turn`; content `window`, `proposal`) | 馬, basis `model` | type **[built]**; no writer: **[decided]** |
| item needs supplier | `[:needs 1]` (`:needing`, `:supplier`; content `class`) | Giraffe, basis `model`, quote check `exact`; `met` is computed, never stored | type **[built]**; no writer: **[decided]** |
| record supersedes record | `[:supersedes 1]`, acyclic | — | type **[built]** |
| intent compiles to candidate, decision | `[:d6/intends 1]`, `[:d6/compiles-to 1]`, `[:d6/decides 1]`, `[:d6/composite 1]` | compile passes | **[built]** as the F4 demo |

Two limits on these:

- "Turn names entity" means only that the word occurs as a whole word in the
  turn's body. It does not mean the turn is about the entity. In the War
  Machine's terms that further claim is a judgement (class J), which the
  production path refuses (TN-wm-tokens §3).
- codex-21's ten compiler relations (expresses, targets, manifests-at,
  expands-under, advances, governed-by, requires, evaluates, realizes,
  witnessed-by; §1) have not been mapped onto the `:d6/*` types. **[open]**

**(b) Relations between entities: the reduced vocabulary.** **[open]**: a
draft by claude-13 (`holes/labs/M-jvm-arxana/LEXICON-VERBS.tsv`, futon4
`643a666`) that Joe has not corrected. It reduces the lexicon's 114 verbs
(587 uses) to 14. "Flipped" means the inverse verb was turned round (`B reads
A` becomes `A feeds B`).

| Verb | Reading of `A verb B` | Uses | Takes in, among others |
|---|---|---|---|
| `feeds` | what A produces or holds goes to B | 227 | writes, stores-in, read-by; flipped: reads, consumes |
| `part-of` | A is inside B | 130 | flipped: declares, members, contains, home-of |
| `uses` | A depends on B to work | 80 | written-in, typed-by, runs-in; flipped: used-by |
| `related` | A and B are connected, unspecified | 30 | sibling, contrasts, distinct-from, alternative-to |
| `checks` | A observes, limits, guards or certifies B | 27 | governs, measures, guards; flipped: checked-by |
| `implements` | A realises what B specifies | 22 | formalises, models; flipped: specifies, carried-by |
| `serves` | A exists for B's use | 21 | — |
| `replaced-by` | B has taken A's place | 16 | succeeded-by, sequel-to; flipped: replaces |
| `describes` | A is an account of B | 13 | frames, summarises, names; flipped: shown-by |
| `extends` | A adds to B, and B works without A (§6) | 7 | refines; flipped: extended-by |
| `runs-on` | A is hosted or installed by B | 5 | flipped: hosts, installs |
| `is-a` | A is a kind of B | 5 | kind-of; flipped: generalises |
| `needs` | A cannot proceed without B | 2 | would-use |
| `derived-from` | A was extracted from B | 2 | flipped: source-of |

What stands between this table and a specification:

- **The reduction is not applied.** The store holds each relation as a
  `[:workplan/referent 1]` whose `referent/role` is the original verb (futon3c
  `lexicon_loader.clj`).
- **None is reviewed.** 589 candidate relations are loaded and Joe has
  reviewed 0 of them (M-giraffe, 11-10). 153 more did not resolve to an
  entity.
- **`part-of` mixes four relations:** namespace nesting, running in the same
  JVM, an optional extension, and containment proper (§6, §9). They have been
  separated for Agency only (`AGENCY-survey.md`, futon4 `c9789e2`, ten
  corrections).
- **`in-namespace` is outside the 14.** It is computed from namespace names,
  so by rule 3 it is kept as a rule. **[decided]** (§6); written so far only
  as a survey seed for the kangaroo pouch.
- **`co-resident` is not a link yet.** The survey records it as a
  qualification on an attestation. Whether it becomes a link kind, or is read
  off two systems that `runs-on` the same JVM, is **[open]**.
- **Three names are used twice.** `serves` is both item → benefit line (a
  declared type) and entity → entity (`agency serves bell`). `needs` is both
  Giraffe's `[:needs 1]` and a lexicon verb. `names` is a declared link and is
  also folded into `describes`. A specification needs one meaning per name.
- **Two reductions look inconsistent to me.** `indexes` and `backs-up` (11
  uses with their inverses) reduce to "the index feeds the store", which runs
  against the data flow that `reads` and `drains` follow. `runs-in` reduces to
  `uses`, while `hosts` reduces to `runs-on`.

**(c) Links that another store owns.** Reply links between turns (evidence
`in-reply-to`), `[[name]]` links between memories (minus40), `:memory/assert`
edges (general route; 1,961 per MAP-Q3) and `:lu/occurrence` (minus40) are
referenced authorities: indexed, never copied in (DERIVE-core §14).
**[decided]** The external-reference records exist (C3b); I found no record
that an index over any of these four has been built.

### B5. Ports

Joe, 11-09 (§9): "the more full system will need a notion of 'ports'".
**[decided]** that the notion is needed. Its shape is **[open]**.

Three things are called a port in the sources:

1. **A system's named interface** (§9, claude-13's proposal): a direction
   (read, write or deliver) and a carrier (an HTTP route, a file ledger, an
   environment switch). Systems link to each other's ports, not to each
   other's insides. No type is declared. **[open]**
2. **Arxana's own operations** (DERIVE-wiring): the write operations in B1 and
   the reads `entity/get`, `graph/query`, `scope/resolve`. These are
   instances of sense 1. **[built]** C2
3. **A typed role of a composite 'clusion** (F4, futon3c `2e642f30c`). This is
   a slot in a record, not an interface of a system. **[built]** Calling it a
   "role" in this file would leave "port" with one meaning.

A shape for sense 1 that needs no new record kind (my proposal, **[open]**):
a `port` 'clusion whose endpoint is the system's entity and whose content is
the name, direction and carrier; and a `uses-port` 'clusion from the client
system to that port 'clusion, attested with basis `exact` or `parser` by the
file and line at a sha. §9's Inbox Zero example supplies five such records
with their file lines.

Also **[open]**: the sense in which Inbox Zero is "*analogous* to the Warrant
system" (§9).

### B6. Introductions and referents

**[built]** on the :7094 stack (sessionista packet 2: futon3c `f5a514fb5`,
`a3d871572`, `00f11cf05`; futon4a `7293b2d`, `a461280`). The shape is §10's:

- `[[X]]` in a turn of Joe's writes an introduction: X, the turn's evidence
  id and the range of the mark, Joe as actor, at the turn's time.
- `[[X | r1, r2]]` also writes referents, each with a role. A referent Joe
  writes counts at once. A proposed one counts until rejected (rule 9).
- X's word rule is on only while X has a referent that counts (rule 10). An
  X without one is shown as introduced with no referent. It is not refused.
- Records carry valid-time, so "what did the map hold on a date" and "was X
  used before it was named" are queries. §10 records this as part of packet
  2; I did not run such a query.

### B7. Wants, witnesses and constructions

Joe's framing (§12, §12a) is **[decided]** as the way to read links:

- `[a ? c]` is a query: is a linked to c? A link record is a witness that
  answers yes.
- "a links to c eventually" is a want: a query whose witness does not exist
  yet, with a horizon (rule 8).
- A proof is a construction of the transformation, not only a witness that
  the link exists (§12a). Attestation basis `kernel` records a checked
  construction. **[built]** (`[:kernel-builds 1]`, used in futon3c
  `formal_f3.clj`).

No want record exists. **[open]**: how a want is stored. A shape that fits
the model (my proposal): a 'clusion whose content is the query (a type, and
its roles with one left unfilled) and the horizon. Whether it is open,
witnessed or past its horizon is computed on read and never stored, as
Giraffe's `met` is.

§12 lists wants that already run elsewhere: a War Machine wanted token, an
`[[X]]` awaiting a referent, a park, a workplan typed hole, a Giraffe `needs`
edge. By DERIVE-core §14 those would be referenced, not copied. No source
says so for wants in particular. **[open]**

**[open]** (§12a): which link types require a construction and not only a
witness, and how a construction is stored.

### B8. Readiness, benefit and scale

- **Readiness.** `status` (exists, partly, hypothesised, retired; and
  "planned", Joe, §7), `live` (live, on-demand, dormant, dead) and `tier`
  (core, detail) are attestation content on the entity, so they can be
  corrected without changing its id. **[built]** futon3c `dfbde7e95`
- **Benefit of a pending item.** `[:workplan/serves 1]` to a benefit line.
  **[built]**
- **Benefit of a feature.** Joe's statements in §13, §13a and §13b are
  **[decided]** as what the records must be able to say: realised or
  potential; scale in principle (what a user needs) and in practice (who
  uses it, with evidence); dated, and closed without deletion; "claiming a
  benefit is not the same as claiming a 'moat'"; an outside alternative
  recorded by the aspect it covers; a benefit claim resting on property
  claims that a test decides. The record shapes are claude-13's proposals and
  none is declared as a type. **[open]** One property claim has its test:
  "tickets appear as links within Agent replies", futon4a `a76f453`.
- **Kinds of benefit claim so far** (§13a, §13b): capability, control,
  integration, learning, usability, auditability, inference. Superiority is
  not one.

### B9. Writers and readers

This table is what M-repl-circus's IDENTIFY checklist asks for first ("For
each animal: which GCC node and edge types it writes").

| Writer | Writes | Mark |
|---|---|---|
| 驴 (patterns for an operator turn) | `[:pattern/proposal 1]`, one attestation per producer; its ranking is a materialised aggregate (DERIVE-repl-circus-needs §1) | **[decided]** not written |
| 象 (reading of an operator turn) | `[:interpretation 1]`; a reply mark is an occurrence entity (`:mark-occurrence`), used as evidence by the interpretation, not a link (DERIVE-wiring §3 choice 1) | **[built]** on fixtures; no production caller |
| 馬 (tension resolved, re-raised or dropped) | `[:pattern/occurrence 1]`; "shown" as appended events (DERIVE-core §16) | **[decided]** not written |
| Giraffe | `[:needs 1]`; a reviewer's verdict as an assessment | **[decided]** not written; the needs are in Giraffe's run files |
| 狗 (memories for an agent turn) | no source I read maps its output to a record type | **[open]** |
| 猴 (operator focus) | the same | **[open]** |
| Workplan and lexicon loaders | items, entities, benefit lines, names, serves, mentions, candidate referents | **[built]** |
| Joe, through sessionista and entity pages | introductions, referents, word-rule switches, assessments | **[built]** |
| Code↔docs | scopes and correspondences | **[built]** |
| Compile passes | `:d6/*` | **[built]** as a demo; **[open]** on live turns |

| Reader | Reads | Mark |
|---|---|---|
| Joe | sessionista marks; entity pages with backlinks and referents; the F1 `linear`, `outline` and `gcc` projections (futon3c `5b08367ff`) | **[built]** |
| Agents | ticket ids as links in agent turns (futon4a `a76f453`); entity backlinks in agent turns (Joe, §6) | first **[built]**, second **[decided]** |
| Pattern retrieval for operator turns | the proposal aggregate | **[decided]** |
| Giraffe | needs, serves, entity status; the lexicon loop of §14 | **[open]** (§14 is a proposal) |
| War Machine; the maths tutor | open wants and candidate actions; cascade construction | **[open]** MAP-Q3: no feed from stored candidates into the selector |
| M-the-perfect-crime | hotspots and misconstruals | named by Joe (11-05); nothing further in the sources |

Rule 6 has not been run over B3 and B4 as a check: for each kind, a writer
and a reader, or "not stored". Part C item 5.

### B10. Store and route

- **[decided]** (Joe, 11-09, §6) The private demo stores are for now. When
  the Arxana design is final, Arxana records move to the Arxana route on
  futon1b, and the GCC uses that route. General-route edges are read as an
  outside source.
- **[built]** today: private stacks. The workplan, lexicon and sessionista
  records are on :7094, durable under `~/.local/share/futon4a/workplan-demo/`
  (futon3c `fa08b1181`, futon4a `d35821d`). F1–F6 run on :7085–:7090 and
  code↔docs on :7091.
- The live futon1b (:7073) predated the C2 commits on 11-08 (INSTANTIATE.md).
  I did not probe it today.

### B11. War Machine tokens and the strategic layer

- A War Machine token `[target wanted-token]` with its locator is a want with
  a check that supplies the witness (§12). The locator classes C3–C8 read git
  objects or the test registry, and none reads conversation text
  (TN-wm-tokens §1).
- **[open]** A text class T1, "the form occurs as a whole word in the body of
  evidence entry E", checked by the sessionista matcher. Where it would be
  declared (the WM contract in futon2, an Arxana type, or both) is not
  settled (TN-wm-tokens §4, §6). Nothing is built.
- **[open]** Finding a concept without its word, Joe's "Inisheer" (§8;
  TN-wm-tokens §5).
- M-giraffe, 11-10: Joe's operator HEAD for the strategic layer is "a
  representation layer for FUTON that is readable enough that we can actually
  *see* how everything is related and not just 'know' that everything is
  related", and "'be strategic' and 'make FUTON readable' are effectively
  *the same*". The readability observables are reads over this graph. At
  claude-13's latest measurement (run `20261110T035845.799Z-9626cf36`): open
  items in a Giraffe cluster, 107 of 489; strategic missions in a cluster, 3
  of 4; lexicon relations reviewed by Joe, 0 of 589.
- **[open]** Whether the character wants in M-giraffe's strategy cascade
  (unblock, reduce harm, finish quickly, be strategic) are stored as B7 wants.
  The cascade is claude-13's draft, and Joe has confirmed the reading of its
  table.

### B12. Institutions

**[open]** Status "planned" (Joe, §7). A Lean layer and 13 patterns exist.
No institution is written as records, and what identifies one is not settled.
§7 proposes a worked exercise on the packet process rule.

### B13. A worked example

The kangaroo pouch, from records on :7094 (per WORKPLAN-INSTANTIATE and
`lexicon_loader.clj`; I did not read the store):

- a node: `[:lexicon/entity 1]` with slug `kangaroo-pouch`, status "exists"
  in an attestation;
- a rule: a `[:workplan/entity-match-rule 1]` with the words "kangaroo" and
  "Kangaroo", from which the links "turn names kangaroo-pouch" are derived
  and not stored;
- links: `[:workplan/referent 1]` records `in-namespace agency` (computable
  from `futon3c.agency.agent-pouch`) and `extends agency` (a feature claim;
  the switch `FUTON3C_KANGAROO` is its evidence), each a candidate until Joe
  rules;
- not there yet: a `port` for the switch (B5), a benefit claim (B8), a want
  (B7).

### B14. Open items, gathered

| # | Open item | Where |
|---|---|---|
| 1 | Are Joe's rulings inputs to a rebuild? | Part A (1), rule 4 |
| 2 | A named policy for choosing among assessments | B1 |
| 3 | Stored corrections of single occurrences | rule 3 |
| 4 | The check that every kind has a writer and a reader | rule 6, B9 |
| 5 | Typed "missing" in sessionista | rule 7 |
| 6 | The 14 verbs: Joe's correction, the name clashes, the two doubtful reductions, applying them in the store | B4(b) |
| 7 | `part-of` sorted beyond Agency; `co-resident` as a link or not | B4(b) |
| 8 | Port: shape, and one meaning for the word | B5 |
| 9 | Want: how stored; existing wants referenced or copied | B7 |
| 10 | Which links need a construction | B7 |
| 11 | Feature benefit, scale, alternatives and property claims as record types | B8 |
| 12 | 狗 and 猴 outputs as records | B9 |
| 13 | The compiler contract; codex-21's relations against `:d6/*`; a feed from live turns into candidates and on to the War Machine | Part A (3), B3, B4(a) |
| 14 | Agency's seats, jobs, bells and promises as nodes | B3 |
| 15 | Institutions | B12 |
| 16 | Text class T1; concepts found without their word | B11 |
| 17 | Turn granularity when one entry joins streamed pieces | §6 |
| 18 | Roles of concepts around a turn (melody, chords, drone) | §8 |
| 19 | M-repl-circus's route text | Part A (2) |

## Part C — What this excursion must certify for M-formal-arxana to close

M-formal-arxana's claim is that Arxana is "the one graph contract" the
intents, the GCC action view, assessable compositions and argument structure
all live in. The excursion tests that claim on the GCC: can everything the
GCC is meant to hold be said in the records the mission built?

- [ ] **1. Joe has read and corrected the Part A paragraph.** His corrections
  are recorded here in his words.
- [ ] **2. No new record kind is needed.** Every node and link kind in B3 and
  B4 is stated in the built model ('clusion types, attestations, assessments,
  external references, attempt receipts). For each of the five kinds with no
  type yet (port, want, dated benefit claim, institution statement,
  construction) there is either one worked record in the model, or a finding
  recorded against the mission that the model cannot hold it.
- [ ] **3. The marks survive a record check.** Every [built] cites a commit
  or file that exists. Every [decided] names its source. Every row of B14
  has an owner, or Joe has accepted it as left open on purpose. The mission's
  DOCUMENT step asks for "a record of what remains intentionally unresolved",
  and B14 can be that record once ruled on.
- [ ] **4. The reflective debt of §11 is paid.** M-formal-arxana and
  E-gcc-schema are introduced as entities, each with at least one referent
  that counts. Each kind named in Part B ('clusion, attestation, assessment,
  external reference, introduction, referent, port, want, witness, …) is an
  entity too, with this file or the mission as its referent. That set of
  entities can also serve as the "conceptual glossary" the DOCUMENT step asks
  for (my suggestion).
- [ ] **5. Every kind has a writer and a reader, or is marked not stored.** A
  table in this file is enough for closure. A machine check in the style of
  `type_registry.clj` is work for M-repl-circus or M-jvm-arxana.
- [ ] **6. One route.** M-repl-circus's "Decided: where derived output goes"
  agrees with Joe's 11-09 decision (§6).
- [ ] **7. M-repl-circus can cite this file.** For each of the six animals,
  B9 names the types it writes, or says which source is missing.

Outside this excursion, and still needed before the mission's file can say
CLOSED: its 14 completion criteria and its INSTANTIATE and DOCUMENT boxes
are unticked, although Joe accepted F1–F6 on 11-09. Someone has to reconcile
those boxes with the acceptance record in
`holes/labs/M-formal-arxana/INSTANTIATE.md`.

Where the checklist stands on 2026-11-10: I believe item 3's first two
clauses are met by this revision (see below), and a reviewer should sample
them. Its third clause, and items 1, 2, 4, 5 and 6, are not met. Item 7 is
met for four animals and open for 狗 and 猴.

**What I checked (claude-1, 2026-11-10).** I read in full this file's record,
M-formal-arxana, M-repl-circus, DERIVE-core, WORKPLAN-INSTANTIATE,
TN-wm-tokens and `LEXICON-VERBS.tsv`; and the cited parts of INSTANTIATE.md,
DERIVE-wiring, DERIVE-repl-circus-needs, CLIENTS.md, AGENCY-survey, M-giraffe
(11-10 sections) and the lexicon's `gcc` entry. Every commit cited in Parts
A–C exists in its repo (`git cat-file -e`). The type ids in B3 and B4 are
declared in `type_registry.clj` at futon3c `6e4a8bb3a`. The verb counts are
summed from the TSV. Rules 9 and 10 are read from the diff of futon3c
`4a52a6b18` and the message of `00f11cf05`. Not checked: I ran no test and
queried no store, so record counts are as WORKPLAN-INSTANTIATE and MAP-Q3
report them, and "no writer" means I found none in the build records, not
that I searched all code.

---

# The record (claude-13, 2026-11-09 and 11-10)

Kept as written. Where Parts A–C differ from a section below, the difference
is stated in Parts A–C.

## Why now (Joe, 2026-11-09)

> "with these few concepts I think it is reasonable to start to produce a GCC
> which can eventually grow to include patterns, missions, sessions, memories,
> and probably some other things (like user stories, beneficiaries, Ostrom
> institutions and so on) … I'm not sure anyone has yet written down an exact
> *schema* for the GCC nor a full manifest of the kinds of things we want in
> there. … Agency, Giraffe, and Kangaroo represent real features at different
> scales and different degrees of readiness, and should certainly be in our
> map."

> "OK to a document to start with but we should eventually move towards a more
> formal schema … read my *Occur* buffer … it would be best if we don't
> reinvent something that does actually exist or exist in partial form."

This note gathers what already exists, then drafts a manifest from it. The
formal schema is a later step (§6).

## 1. What already exists (do not reinvent)

Found by reading Joe's `*Occur*` buffer for "GCC" (2026-11-09: buffers of
claude-8, claude-6, codex-21, zai-1, zai-9, kimi-1 and claude-13) and the files
those discussions point to.

| Source | What it settles | Status |
|---|---|---|
| M-repl-circus §"The giant connected component" (futon3c `holes/missions/M-repl-circus.md:143`) | The GCC is a **derived graph in its own store**. Turn and session nodes are evidence ids. Edges are computed from evidence and the library, so it can be rebuilt from scratch, and incremental upkeep can be checked against a rebuild. | written, mission text |
| M-repl-circus goal (claude-8 buffer, Joe 11-05, `70e183ae9`) | The Circus **interoperates by graph writes and XTQL queries**, never by storing JSON in evidence. Every output needs a named reader (agent via 驴/狗, Joe via the REPL, or the GCC). | written |
| claude-8 buffer l.740 | Relating output becomes **typed hyperedges with id endpoints, never copied text** (驴 activations, 象 readings, 馬 cards, Giraffe reviews, agent→file facts). `:memory/assert` already works this way. | decided 11-05 |
| Joe, claude-8 buffer l.611 | The **readers**: 驴 pattern retrieval for operator turns, Giraffe priorities, related missions and methods, M-the-perfect-crime hotspots and misconstruals, fast cascade construction for the War Machine and the maths tutor. | listed in M-repl-circus |
| codex-21 buffer l.340–531 (Joe: "a compiler that turns intents into an explicit inference-ready action surface") | Compiler types: **Act, Intent, EntityRef, ScopeRef, Institution, CandidateAction, AdmissionDecision, ActionReceipt**. Relations: expresses, targets, manifests-at, expands-under, advances, governed-by, requires, evaluates, realizes, witnessed-by. Passes: Observe, Read, Ground, Expand, Infer, Admit, Execute, Record. Institution is an input, not hard-coded. | proposed. The "compiler contract" codex-21 suggested writing was **not** written as a file (searched futon* holes). |
| M-formal-arxana DERIVE-core (round 9, §17) and F1–F6 | The record kinds: **'clusion** (identity = type revision + role endpoints + declared content), **evidence attestation** (basis, standing, coverage), **assessment** (candidate/accepted/rejected, by an actor). Nothing correctable goes into identity. External references for turns, patterns and memories. | built and demoed |
| `DERIVE-repl-circus-needs.md` §1, §5 | Circus records mapped to that model: `pattern/proposal`, `pattern/occurrence`, `needs`, with the 驴 ranking as a rebuildable aggregate and minus40's `:lu/occurrence` as a referenced authority (indexed, never mirrored). | written 11-08 |
| futon1b registry (MAP-Q3) | Registered relations: `:memory/assert` (1,961 written), `:learning-loop/*` and `:has-operator-turn` (0 written). | partial |
| E-evidence-cleanup `DESIGN-RECORDS.md` (futon3c) | For 22 writer families that put JSON in evidence, the **GCC need** each one was trying to meet (invocation node, promise transitions, bell dispatch → recipient, tracer open/closed, …). | written |
| claude-6 lexicon (`~/notes/cascade-live-outline/lexicon/`, 254 `@entity` blocks) | Features with **status** (exists, partly, hypothesised, retired), **live** (live, on-demand, dormant, dead), **tier** (core, detail) and **relations** (part-of, uses, stores-in, reads, writes, feeds, serves, replaces, implements). | draft data, Joe has corrected parts |
| claude-6 workplan data, loaded on the :7094 private stack | 323 pending items, 75 benefit lines, 416 serves links, 454 names links, user stories (draft), Joe's rulings. Types `:workplan/item`, `:workplan/serves`, `:workplan/names`, `:workplan/mention`. | loaded (M-jvm-arxana WORKPLAN-INSTANTIATE) |
| claude-13 buffer l.1517 (measured) | **Reads must be indexed.** A full scan of the hyperedges table costs 50–80 s, and evidence text search about 10 s per word. GCC reads on a prompt or page path go through an index by type and role endpoint. | measured |
| Joe, 2026-11-09 (this session) | **Computed links are kept as rules, not copies.** For turn → entity, each entity keeps the words that name it, switched on or off, and the backlinks are derived. | trial running (codex-21, backlinks packet 1) |

## 2. Node kinds (draft manifest)

"Arxana type" says whether a declared record type exists today. "Readiness"
names the field that says how built the thing is.

| Kind | Where it lives today | Identified by | Arxana type today | Readiness | Example |
|---|---|---|---|---|---|
| Feature / entity | claude-6 lexicon | slug | entity records in the workplan stack | `status`, `live`, `tier` | agency (exists, live, core); giraffe (partly, "up on blocks"); kangaroo-pouch (exists) |
| Pending item: mission, excursion, ticket, T- item | `holes/**/*.md`, Giraffe runs | item id (M-…, E-…, T-…) | `:workplan/item` | `pending/level` (draft), Joe's ruling (assessment) | T-pattern-search-reads-stale-index |
| Pattern | futon3 library (`*.flexiarg`) | library path, sha256 as revision | external ref; `pattern/proposal`, `pattern/occurrence` (DERIVE) | library `@status` | storage/graph-memory-contract |
| Turn (operator or agent) | evidence store (:7073) | evidence id | external ref | — | |
| Session | evidence `session-id` | session id | external ref | — | |
| Memory | minus40 (:7093) | project + memory slug | referenced authority; `:memory/assert` hyperedges | — | |
| User story | claude-6 workplan data | per item | draft attestation (`story/*`) on the item | `story/evidence-kind` | |
| Benefit line / beneficiary | claude-6 benefit reading | `benefit/line` | benefit-line records (75) | — | "the library is findable and current" |
| Institution (Ostrom) | patterns in `futon3/library/institutions/` (13, from Corneli 2016); Lean layer `mathlib4/DarkTower/AIF/Institutions.lean`; M-ways-of-working | **not settled** | none | **planned** | the packet process rule, written as a statement (§7) |
| Code scope, doc scope | repos | path + scope id | codedocs types (F3, code↔docs) | — | a Lean def and its doc bullet |
| Intent reading | 象 per-turn analysis | reading id | F1 interpretation 'clusions | assessment standing | |
| Candidate action, admission decision, receipt | compiler code in futon3c `arxana/compile/` (no callers) | — | F4 records (composite) | — | |
| Agent seat, invocation, bell, promise | Agency (:7070) | agent id, job id | none (DESIGN-RECORDS lists the needs) | — | |
| Commit | git | repo + sha | external ref (`:filed` mentions) | — | |

## 3. Link kinds (draft manifest)

| Link | Endpoints | Produced by | Kept as | Today |
|---|---|---|---|---|
| names | turn → entity | word rules per entity | **rule records; links derived** | trial (3 entities) |
| names | item → entity | claude-6 parser | `:workplan/names` + attestation | 454 loaded |
| serves | item → benefit line | three model producers | `:workplan/serves`, one attestation per producer | 416 links, 418 attestations |
| needs | item → item / artifact / entity | Giraffe (model, quote check `exact`) | `needs` 'clusion (DERIVE) | in Giraffe files |
| mentioned-in | item → turn / commit | git, evidence | `:workplan/mention` | loaded |
| in-namespace | code entity → code entity | computed from namespace names (`futon3c.agency.agent-pouch` is in `futon3c.agency`) | derived | not loaded |
| extends (optional feature; the parent works without it) | feature → feature | lexicon, with the switch as evidence (`FUTON3C_KANGAROO`) | 'clusion + attestations | proposed 11-09 |
| part-of, uses, reads, writes, feeds, serves, replaces, implements, stores-in | entity → entity | lexicon | not loaded | in lexicon; `part-of` is ambiguous between the two rows above |
| proposes | turn → pattern (驴, embedding, 驴/m40) | Circus | `pattern/proposal` + attestations | mapped, not written |
| occurs-in | pattern → window of turns (馬) | Circus | `pattern/occurrence` | mapped, not written |
| reads-as | turn → intent reading (象) | 象 | F1 'clusions | demoed |
| corresponds | code scope ↔ doc scope | codedocs | codedocs 'clusions | demoed |
| replies-to | turn → turn | evidence `in-reply-to` | external refs | in evidence |
| links | memory → memory (`[[name]]`) | memory files | referenced | in minus40 |
| compiler relations (expresses, targets, advances, governed-by, …) | per codex-21 | compiler passes | proposed | none |

## 4. Rules already agreed

1. The GCC is derived, and can be rebuilt from evidence, the library and the repos.
2. Endpoints are ids, never copied text.
3. Computed links are stored as the **rules that compute them**. Only human
   corrections of single occurrences are stored as records.
4. Nothing correctable goes into identity. Corrections are assessments.
5. Each reader has an indexed path, and no page or prompt read scans.
6. Each output has a named reader.

## 5. Built and not built: one graph

A built feature is an entity whose status is exists or partly. Something not yet
built is a pending item, linked by `needs` to the entities it needs and by
`serves` to the benefit lines it serves. A hypothesised entity that no pending
item needs, or a benefit line that no item serves, then shows up as a gap you
can query. One graph serves claude-6's "what has been built" map and the "not
yet built" map.

## 6. Open

- **Store (Joe, 11-09, settled).** The private demo stores are for now, not
  for good. When the Arxana design is final, Arxana records move to its futon1b
  route, and the GCC uses that same Arxana route once its content is ready.
  Edges already written through the general hyperedges route (e.g.
  `:memory/assert`) are read as an outside source.
- **Where backlinks show (Joe, 11-09).** Entity backlinks should eventually
  appear in agent turns, in session-mode or whatever replaces it, not only on
  an entity page.
- **Code containment and feature dependency are different relations (Joe,
  11-09).** `futon3c.agency.agent-pouch` is unambiguously inside Agency as a
  namespace, but Agency works without it: it can be switched off, and seats
  fall back to cold starts. The lexicon's `part-of agency` mixes the two. The
  draft splits it into `in-namespace`, computed from code, and `extends`, a
  feature claim with its switch as evidence. Where the two disagree, a namespace
  nesting crosses a feature boundary, and that is worth showing. Do features
  also need a size field? Still open.
- **Granularity of turn nodes.** One evidence entry can join several streamed
  pieces of a reply (claude-6, 11-09). Offsets are counted on the evidence text.
- **The compiler contract** codex-21 proposed (types and passes, §1) still needs
  writing, and its link to this manifest needs settling.
- **The formal step:** turn §2–§3 into declared types (futon3c
  `arxana/type_registry.clj`) plus a check that every declared kind has a
  producer and a reader, in the style of `graph-memory-contract.flexiarg`'s
  "schema-mirroring tests".

## 7. Institutions (Joe, 11-09: status "planned", not "hypothesised")

What exists:
- **Lean layer:** `mathlib4/DarkTower/AIF/Institutions.lean` (`7bd6e913de`,
  2026-09-18). It defines `Deontic` (may / must / mustNot),
  `InstitutionalStatement` (condition, aim, deontic: the ADIC core),
  `Norm`, `Rule` (adds the sanction outcomes: the Or-else), and `Monitor`
  (trajectory requirements as bookkeeping that is observed), with
  `Preference.withInstitution`. Its verdict: the operational level fits AIF,
  collective choice fits partly (in learning), and the constitutional level
  stays outside (the operator's ruling). It is single-agent; multi-participant
  action situations are left open.
- **Patterns:** 13 in `futon3/library/institutions/`, including
  `rules-as-institutional-statements` (the ADICO form, kinds rule / norm /
  strategy).
- **Game:** the Snatch exemplar, `mathlib4/DarkTower/WarMachine/F12SnatchExemplar.lean`,
  a 24-pattern cascade (including `institutions-vary-by-position-and-force`).
- **Design notes:** futon2 `wm-contract/SESSION-C-institutional-constraints-2026-09-09.md`
  and `SESSION-IAD-feedback-interpretation-2026-09-10.md` (proposals); M-aif4iad
  (IDENTIFY).
- No institution has been written out as records anywhere yet.

Worked exercise (proposed): construct one real institution in the graph
language, with each record corresponding to the Lean types.
- One `institution/statement` 'clusion per statement. Its endpoints are the
  attributes (the seats it binds), the aim (the action class) and, for a
  rule, the sanction. Its declared content is the deontic and the condition.
- A source attestation for each statement (the packet text, CLAUDE.md).
- Incidents as evidence of the Or-else.
- A monitor as a probe whose results are attestations.

Candidate: the packet process rule. Agents (attributes) are FORBIDDEN
(deontic) to signal a process they did not start (aim) on Zone (condition),
or else the packet is rejected and the incident recorded (or-else; instance:
codex-21 stopping futon1b on 2026-11-08).


## 8. Direction: linking as you go (Joe, 11-09)

> "sessionista-mode would use these same kind of ideas, and become the basis
> of a NNexus-like system for the Agent turns. Indeed, I'm thinking it's
> actually also the basis for the GCC. Just like someone in a session
> remembers, say, "Inisheer", maybe we can build some structural patterns
> around the concepts. In particular, with a folk session there are different
> skills involved: the melody is important, but so are chords, baseline, and
> even other generic accompaniment (drones, percussion)."

- **Fast, without model calls.** The marks are painted as text is displayed or
  typed, like session-mode's draft tagger (`session-mode--paint-rnode-tags`,
  jit-lock).
- **NNexus-like.** Words are concept labels with per-entity linking policies
  (the on/off rules, later contexts to skip). Steering could come from what the
  turn is about, such as its clocked mission. A rule change re-links only what
  it touches.
- **Roles around a concept (open).** In a folk session one tune is the melody
  while others play chords, bass, drones or percussion. In a turn, one concept
  may be what the turn is about, while others support it, sit underneath it (as
  Agency and futon1b do), or are constant background. Whether entity marks
  should carry such a role is not yet designed.

Alignment with War Machine tokens: `holes/labs/M-jvm-arxana/TN-wm-tokens-and-sessionista-terms.md`.

First step: sessionista-mode packet 1 (codex-21, 11-09): marks from the Arxana
rules, with RET opening the entity's backlinks.

## 9. Systems connect through ports (Joe, 11-09)

> "Inbox Zero … I'd say it is a separate system, but … the *implementation*
> may have something to do with Agency … if it is separate connects to Agency
> on an *interface*; to me this suggests that the more full system will need a
> notion of 'ports'. I know for example that Inbox Zero is *analogous* to the
> Warrant system."

The Agency survey (`holes/labs/M-jvm-arxana/AGENCY-survey.md`) found three
different relations hiding under the lexicon's `part-of agency`:
- **in-namespace**: the code is inside `futon3c.agency` (the kangaroo pouch);
- **co-resident**: it runs in the same JVM but is its own system (Inbox
  Zero, the test registry, Drawbridge, the multi-watcher);
- **connects via a port**: a separate system uses a named interface of
  another.

Proposed kind: **Port**, a named interface of a system, with a direction (read,
write or deliver) and a carrier (an HTTP route, a file ledger, an env/config
switch). Systems link to each other's ports, not to each other's insides.
DERIVE-wiring already speaks of Arxana's "write ports" and "read ports" in
this sense.

Worked example from the code (futon3c `src/futon3c/inbox_zero/`, 11-09):
Inbox Zero does not bell agents. It
- reads Agency's roster port, `GET /api/alpha/agents` (`sweeper.clj:178`,
  `turn_promotion.clj:125`);
- reads Agency's jobs port, `GET /api/alpha/invoke/jobs` (`sweeper.clj:170`);
- reads the evidence port, `GET /api/alpha/evidence?session-id=`
  (`infer_adapters.clj:90`), and the hyperedges port (`attribution.clj:97`);
- delivers turns through Agency's follow-up queue port, `POST
  /api/alpha/followups` (`turn_promotion.clj:291-293`, overridable by
  `FUTON3C_INBOX_ZERO_FOLLOWUP_URL`).

Open: in what sense Inbox Zero is analogous to the warrant system. Both sweep
for work that happened without a durable record (an uncommitted change, an
unrecorded test run) and turn it into a record that others can check. Is that
the analogy Joe means, and does it make them two instances of one pattern?

## 10. Introducing an entity with [[X]] (Joe, 11-09)

> "if I write [[Inbox Zero]] or [[Warrant]] then we'd know that I am
> 'introducing these as named entities in this turn' … a term really can't be
> introduced until it has some referents, though it doesn't need to have
> preexisting usage"

- **Introduction:** a link from the entity to Joe's turn (evidence id plus the
  range of `[[X]]`), with Joe as actor, at the turn's time. X resolves to an
  existing entity by name, slug or alias, or else is new.
- **Referents:** links from X to what it refers to, each with a role
  ("by analogy with", "like", …). Their targets are entities, files,
  namespaces, patterns, missions or turns. Referents written by Joe
  (`[[X | r1, r2]]`) count at once. Other entities in the same turn are
  candidates until accepted.
- **Rule:** X's word rule switches on only once X has a referent. Until then
  it is shown as introduced with no referent. The rule is made visible, not
  refused.
- **Time:** valid-time lets us ask what the map held on a date, and whether X
  was used before it was named (earlier backlinks are shown as such, not as
  its introduction).

Built by sessionista packet 2 (codex-21, 11-09), on the private :7094 stack.

## 11. Debt: the specifications get the same treatment (Joe, 11-09)

> "in order to talk about that 'same thing' we should really have a
> specification of what it is, but luckily we have M-formal-arxana and
> E-gcc-schema *written down*. So they should get the same treatment (which
> will be a reflective meta-theory). But as long as we know we have this debt
> to dispatch soon the order doesn't really matter"

Owed: introduce M-formal-arxana and E-gcc-schema as entities in the map, with
referents. Their own kinds and relations (the 'clusion, the attestation,
introduction, referent, port, …) then become entities too, so that the schema
describes itself with the records it defines. To be dispatched soon after
sessionista packet 2 and the Agency entities-and-ports packet.

## 12. Links as witnesses; "eventually" as control (Joe, 11-09)

> "maybe CTRL maps to what I was talking about with XTQL; so the query
> language gets mapped to a simple control language … consider what it means
> e.g. to 'link' something. If we view that trans-temporally, it means
> [a ? c] resolves in the future."
>
> "the way I understand [a ? c] is as a query: is a linked to c. The 'link'
> is a witness. If we think across time, this becomes a kind of 'control
> problem', i.e., a links to c 'eventually'."

- **Query and witness.** `[a ? c]` is the question "is a linked to c?". A
  link record is a witness that answers yes. A path of links can also be a
  witness. Asking is a read (XTQL). Supplying a witness is a write.
- **Across time.** "a links to c eventually" is a want: a query whose
  witness does not exist yet. Steering the work so that the witness comes to
  exist is a control problem, and the CTRL verbs are the writes that pose
  such wants and supply witnesses.
- **Bounded, as everywhere else.** Unbounded "eventually" cannot be checked
  (the same point as the eat-your-own-tail probe). Each want carries a
  horizon or deadline, after which it is reported unresolved, not false.
- **Instances already running:**
  - a War Machine wanted token (the query is its locator; the witness is the
    observation; the bound is the horizon T);
  - `[[X]]` waiting for a referent;
  - a park (wake me when job J has a result, or at the deadline);
  - a workplan typed hole, and a Giraffe `needs` edge with `met` computed.
- **Consequence for the GCC.** The GCC holds both the open queries (wants)
  and the witnesses (links). The War Machine is a controller over them: it
  chooses actions that are expected to produce witnesses for open wants.

### 12a. Proofs as constructions (Joe, 11-09)

> "one case of 'clusions is BHK style construction; whereas I said a link was
> a control layer that says a eventually reaches b, a proof is a
> *construction* of that transformation."

Under the Brouwer–Heyting–Kolmogorov reading, a proof of A → B is a
construction that turns any proof of A into a proof of B. A proof of ∃x.P is a
witness together with a proof that P holds of it. For the GCC this separates
two kinds of answer to `[a ? c]`:
- **A witness that the link exists:** a record saying a is linked to c
  (an attestation with basis parser, model or human).
- **A construction of the transformation:** something that, given a, actually
  produces c and can be checked. Instances already in the records:
  - Lean proofs checked by the kernel, which is attestation basis `:kernel`;
  - the code↔docs relocation strategy (hash, context, hint) that carries an
    anchor across an edit;
  - a War Machine pattern application whose receipt shows the state change.

Open: which 'clusion types require a construction rather than a witness, and
how a construction is stored. It could be a reference to a checkable artifact
(a Lean declaration, a function at a sha) plus a kernel or exact attestation
that it was checked.

## 13. Benefit as a dimension, and scale (Joe, 11-09)

> "we've touched on liveness (Agency live, GCC not) but not yet *benefit* as
> another dimension. An important aspect of *benefit* is going to be 'does it
> scale'. So, e.g., Agency can *in principle* scale to anyone with Emacs, JVM,
> and modest hardware. In practice I have made no effort to promote it; futon
> has one other user (Rob) … he might use futon *without* Agency … but he does
> use futon1b. … the 'ability to make models of value production networks'
> should have generalisable value, but this is a developing capability."

Pending features are already in the lexicon: `gcc` (hypothesised, "Planned
derived graph of patterns, missions, turns, sessions and memories"),
`repl-circus` (partly), `formal-arxana` (hypothesised), and 76 "partly" and
10 "hypothesised" entries in all. The 323 pending items link to entities
through `names`.

Today `serves` links pending ITEMS to benefit lines. Features themselves have
no benefit records. Proposed, for each feature:
- **feature serves benefit line,** with the kind (realised or potential) and
  evidence of realisation (actual use, cited);
- **scale in principle:** what someone needs in order to use it (its
  prerequisites: Emacs, a JVM, modest hardware), which are the ports and
  dependencies it requires;
- **scale in practice:** who actually uses it, with evidence. For example:
  Joe uses Agency (evidence: the roster and turns); Rob uses futon1b
  (Joe, 11-09). Whether Rob uses Agency is unknown, and is recorded as an
  open query, not assumed.

The GCC's own benefit claim, "the ability to make models of value production
networks", is a potential benefit with generalisable reach. It is recorded as
such, with status developing.

### 13a. Benefits are dated, and a benefit is not a moat (Joe, 11-09)

> "'benefits' have a somewhat time-limited nature … when I made Agency the
> whole notion of subagents was in its infancy … I'm pretty sure things like
> OpenRouter cover the *dispatch* aspects of Agency just fine. So claiming a
> benefit is not the same as claiming a 'moat'. Similarly, Minus40°DB is
> inspired by ZeroDB and Mastra but doesn't claim to be 'better' than them,
> it just claims to be something that we have direct control over."

- **Dated.** A benefit claim has a valid-time interval: when it held. When an
  outside alternative arrives, the claim's interval closes for the part the
  alternative covers. The claim is not deleted, and the history stays
  queryable.
- **Alternatives as records.** A link from a feature to an outside system,
  saying which aspect it covers: OpenRouter covers Agency's dispatch across
  providers. Further aspects of Agency (parks, the durable turn queue,
  persistent seats, the evidence trail) are separate rows, each with its own
  answer. The answer is cited or left open, never assumed.
- **Kinds of benefit claim:** for example *capability* (it does X),
  *control* (we run and can change it: minus40 against ZeroDB and Mastra),
  *integration* (it fits the rest of the stack), and *learning* (building it
  taught something). Superiority is not a kind. A "better than" claim would
  need a comparison with evidence, and none is assumed.

### 13b. Benefits are grounded in tested claims (Joe, 11-09)

> "everything becomes fairly readily testable, e.g., 'tickets appear as links
> within Agent replies' is either true or false. But also: 'minus40 produces
> memories that take the form of conceptual blends' is testable too; beyond
> that, any claim of benefit can be grounded in the tested reality. E.g. 'it
> is easy to navigate to tickets' is a usability benefit. 'We maintain an
> audit trail showing where tickets were discussed' is an auditability
> benefit, and also a *potential* inference benefit (… we *might* be able to
> use the audit trail to think about why the ticket hasn't been completed
> yet)."

- **Property claims** about a feature are statements that a test can decide,
  each with its test. In the War Machine contract's terms they are either
  *checkable*, decided by a check, or *judgement* claims that need rated
  reviewers:
  - "tickets appear as links within agent replies" is checkable. It became
    true with futon4a a76f453, test `sessionista-ticket-ids-in-agent-turns`.
  - "minus40 produces memories that take the form of conceptual blends"
    needs a judgement, or a checkable proxy (e.g. two named inputs plus a
    blend in the memory's structure).
- **Benefit claims** cite the property claims they rest on, and the kind of
  benefit. Kinds so far: capability, control, integration, learning,
  usability, auditability, inference.
  - usability: "it is easy to navigate to tickets" rests on "tickets appear
    as links" and "RET opens the ticket".
  - auditability: "we keep a trail of where tickets were discussed" rests on
    the backlinks and mention records.
  - inference, *potential*: "the trail could explain why a ticket is not
    done" rests on the trail plus a use that has not happened yet. It stays
    potential until a witness shows it in use.
- A benefit whose grounding claims fail, or whose interval has closed (§13a),
  shows as such. Nothing is deleted.

## 14. Ranking the lexicon, and Giraffe and the lexicon learning from each other (Joe, 11-10)

Graph page: `wip/formal-arxana/lexicon-graph.html` (generator
`~/notes/claude-13/lexicon-graph/`). Four measures were tried on the 84
unfinished (partly or hypothesised) features. **Unfinished work within two
hops** ranks Giraffe 27, GCC 21 and Cascade Live 19 first, the three Joe
names as current priorities. The rest of its top ten: daily scan,
M-the-futon-stack, outer selection, formal Arxana, the VSATARCS writer, A4a,
ways of working, inbox zero. The measure was chosen after seeing it recover
those three, so it is on trial: the test is whether it anticipates the next
priority Joe names.

> "if we worked through them and closed them, we'd surface some more … (or if
> we worked through these one by one I might decide that they don't have that
> high of a priority after all. In any case, we'd learn something. Also
> particularly good if we can … get Giraffe and the Lexicon to keep learning
> from each other somehow"

The loop, as proposed:
1. **Lexicon → Giraffe:** each pending item is weighted by the convergence
   score of the features it names, as a prior beside Giraffe's readiness.
2. **Giraffe → lexicon:** when an item closes, the features it names are
   proposed for a status change (partly → exists) as model attestations, which
   Joe corrects. The ranking is then recomputed, and new priorities surface.
3. **Joe's verdicts on priority** ("not that high after all") are assessments.
   They score the measure, and a measure that keeps missing is replaced.
4. **Gaps both ways:** a pending item naming no lexicon entity suggests a
   missing entity; a high-convergence feature with no pending item suggests a
   missing item.
