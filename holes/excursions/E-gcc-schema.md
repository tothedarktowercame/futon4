# E-gcc-schema — what goes in the GCC, and in what shape

**Status:** DRAFT manifest (claude-13, 2026-11-09). Not yet a formal schema.
**Home:** M-formal-arxana (`futon4/holes/missions/M-formal-arxana.md`).
**Related:** M-repl-circus §"The giant connected component", M-jvm-arxana
(workplan stack, code↔docs), claude-6's lexicon and workplan data,
M-ways-of-working (institutions).

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

- **One store or several.** This is open in M-repl-circus. DERIVE-wiring choice 3 keeps
  Arxana records off futon1b's plain `/api/alpha/hyperedges` route, while
  M-repl-circus (11-05) planned to write through it.
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

