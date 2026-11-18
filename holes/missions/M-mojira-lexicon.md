# M-mojira-lexicon — what a named entity and its links mean, before the GCC fills up

**Date:** 2026-11-18
**Status:** OPEN — IDENTIFY (priority: Joe, 2026-11-18, "as a matter of priority")
**Owner:** Joe (operator); claude-cs (driver)
**Repos:** `futon4` (this mission, Mojira), `futon3c` (the :7094 workplan
stack, sessionista introductions), `futon4a` (sessionista-mode),
`~/notes/cascade-live-outline/lexicon` (the draft lexicon)
**Related:** `M-mojira.md`, `holes/excursions/E-gcc-schema.md` (§10, B4, B6),
`futon3c/holes/excursions/E-ne-autocomplete.md` ("Later: the exact rules for
referents"), `futon3c/holes/missions/M-populate-substrate-2.md`
**On completion:** mint `[[lexicon]]`, with its links written in this file.

## Operator anchor

Joe, 2026-11-18, after asking why Nipper shows gray in sessionista and is
absent from Mojira, and hearing what `[[X | r]]` stores:

> this to me sounds like debt that needs a mission to sort out. First of all,
> I'd rather have agents add these links e.g. in their own mission files. So
> for example we might create an M-mojira-lexicon and mint [[lexicon]] as a
> thing when we complete that. I don't really want to have to split hairs
> about the ontological status of Nipper. At the same time, I also don't
> think your "role back" semantics is necessarily the correct one, it is a
> bit of shoehorning (I think) to call [Nipper] "part-of arxana". Maybe it's
> an app *built on* the Arxana features. I also am less convinced that it
> reads Xiang notes, but again, I don't understand the ins and outs of it.
> But in order that we don't create a GCC that is a huge mess, I think we
> need to sort this out as a matter of priority. What I know: we have
> substrate-2 as a reflection of real code that actually exists.

## 1. IDENTIFY

**The question.** What does it mean for a name to be an entity, and what does
a link from it say, such that agents can write the links in their own mission
files and the GCC stays readable?

### What was found on 2026-11-18 (claude-cs, read from code and the live store)

1. **Entities live in three places that do not agree.**
   - The draft lexicon: `@entity` blocks in
     `~/notes/cascade-live-outline/lexicon/{futon3c,futon2,storage,patterns,outer}.md`,
     parsed to `merge/all.json` (254 entries). Mojira reads only this.
   - `~/storage/lexicon/lexicon-vocab.json` (536 entities, generated
     2026-11-09 by `markup/export_vocab.py`). session-mode reads this.
   - The :7094 workplan stack: 13 entities with word rules (agency, arxana,
     evidence-flow, gcc, giraffe, kangaroo-pouch, mojira, nipper, 狗, 猴, 象,
     馬, 驴). sessionista-mode reads this.
   Nipper is only in the third: introduced once by Joe's `[[Nipper]]`, 0
   referents, form switched off, so sessionista draws it with
   `sessionista-pending-face` (gray). Mojira says "Not in the lexicon".
2. **One record type, three meanings.** `[:workplan/referent 1]` holds:
   - Joe's `[[X | r]]`, role `:explicit`: "X is grounded in r", relation
     unstated. The syntax has no slot for a relation;
   - `:candidate`: every other entity named in the same turn, counted until
     Joe rejects it (E-gcc-schema rule 9);
   - the lexicon loader's 589 relations, role = the lexicon's verb (`feeds`,
     `part-of`, ...), none reviewed.
   Only the writer distinguishes them.
3. **§10 asked for more than was built.** Joe's 11-09 design gave each
   referent a role ("by analogy with", "like", ...) and allowed targets that
   are files, namespaces, patterns, missions or turns. The build stores no
   role and accepts only existing entities as targets (Joe 11-16: "clear
   enough for now", details later).
4. **What a referent does today:** it switches the word on (yellow in
   sessionista) and shows as a link on the entity page. Nothing reads its
   meaning.

### Joe's direction (this turn), as constraints on the answer

- **Links are written by agents, in mission files**, not by Joe in REPL turns.
- **An entity is minted when its mission completes** (`[[lexicon]]` at the
  end of this one). Being an entity is not an ontological claim to argue.
- **No forced verbs.** The reduced 14-verb list (E-gcc-schema B4b) is not
  adopted by default: "part-of arxana" misdescribes something that may be
  "built on" Arxana. Relations are written as what they are, or not at all.
- **Ground in substrate-2**, the reflection of code that actually exists.
  A link whose target is real code can be checked; a link between two names
  cannot.

## 2. MAP (questions to answer from the record, not by guessing)

- **M1. Where substrate-2 is now and what it holds.** M-populate-substrate-2
  describes futon1a :7071, which is retired. Which store holds the code graph
  today (futon1b :7073?), with what kinds of record (namespace, file, def,
  commit), and how current is it?

  Joe, 2026-11-18, on M1: "this is why we need an entity map, we can't just
  go by historical records that applied at creation time." The question
  above had to be asked because a mission file still describes substrate-2
  as it was when written. So the entity map's links must be answerable from
  the live system at the time of reading, not copied from a document's
  creation date; a link that can no longer be confirmed shows as such.

  **Answer (claude-cs, 2026-11-18, live queries, not documents).**
  Nothing listens on :7071. substrate-2 is the `code/v05/*` hyperedges in
  futon1b :7073 (`GET /api/alpha/census?type=…`):

  | type | count |
  |---|---|
  | `code/v05/namespace` | 5,033 |
  | `code/v05/var` | 59,639 |
  | `code/v05/test` | 16,204 |
  | `code/v05/contains` | 39,328 |
  | `code/v05/commit` | 31,406 |
  | `code/v05/edits` | 915,651 |

  A var's id is `<repo>-d/<ns>/<name>`, e.g.
  `futon3c-d/futon3c.arxana.sessionista-introductions/resolve-slug`
  (written 11-09; present). Currency, sampled by the last commit before a
  date in four repos: all four commits from the morning of 11-18 are present;
  futon3c's last before 11-10 (`19ca073f3`) and futon1b's last before 11-10
  and 10-15 are absent. So it is live but has holes; why is not yet known.
  The type registry (`/api/alpha/types`) lists 170 entity and 95 relation
  types but none of the `code/v05/*` hyperedge types.
- **M1a. Can substrate-2 answer "X built on Y"?** (claude-13, paired by
  Joe 2026-11-18: "concepts like 'Nipper built on Arxana' become queries
  that this system should be able to help us resolve.")
  - Not yet. The `code/v05/*` types have no requires, calls or
    reads-record-type edge.
  - Nipper today, read from code by claude-13: `minus40/scripts/nip_query.py`
    (reads evidence over HTTP), `~/storage/arxana-variants/nip_merge.py`
    (writes a JSON file), `futon4a/lisp/futon4a-nip-accept.el` (reads it).
    None reads or writes an Arxana record type, so "Nipper built on Arxana"
    is the design intention (M-arxana-incubator WP8), not present fact.
  - Coverage (claude-13, live, 5,005 of 5,037 namespaces read): clj 3,578,
    py 1,239, el 176, flexiarg 7, cljs 5. **Not indexed:** any JavaScript
    (so Mojira), futon4a, minus40, `~/storage` scripts. All three Nipper
    files are unindexed, so "Nipper built-on Arxana" must read **unknown**
    today, not "not yet". The missing languages and repos are a gap in the
    code ingest, separate from this mission's edge.
  - Packet CL-76 (claude-13 → codex-13): `code/v05/writes-record-type` and
    `reads-record-type` from literal record types in indexed Clojure
    namespaces, and a `built-on?` query answering `:yes` / `:not-yet` /
    `:unknown` (`:unknown` whenever a subject file is unindexed or not
    Clojure). Acceptance: `futon3c.arxana.ma-strand` writes `[:ma/strand 1]`;
    `nip_merge.py` → `:unknown`; a Clojure namespace with no record types →
    `:not-yet`.
  - Framing (claude-13, from Joe's META/CTRL distinction,
    `holes/labs/M-arxana-incubator/NOTE-ctrl-items-and-scholium-programming.md`):
    a link is a CTRL claim, a deterministic query that answers yes / not
    yet / unknown when read.
- **M2. Mission-file links.** What syntax agents already use for links in
  mission files (`[[...]]` occurs, e.g. M-repl-circus `[[Nipper]]`), and what
  would read them: a loader over `holes/missions/*.md`, as the lexicon loader
  reads the `@entity` blocks?

  **Answer (claude-cs, 2026-11-18).**
  - `[[...]]` occurs in 212 files under `*/holes/` (futon0–6, futon1b). The
    most common targets: missions and excursions (`[[M-populate-substrate-2]]`
    42, `[[M-operational-vocabulary]]` 34, `[[E-wm-apparatus]]` 19), memory
    slugs (`[[project_kangaroo]]` 11), a few entity names (`[[Nipper]]`), and
    the placeholder `[[X]]` in schema prose (21).
  - **Nothing reads them as links.** Sessionista reads `[[X]]` only in Joe's
    REPL turns; `futon3c/scripts/scope_view_data.py` strips `[[...]]` from
    memory text; no loader walks `holes/**/*.md` for them.
  - **What does read mission files:** `futon3c/watcher/file_ingest.clj`
    writes `code/v05/file→mission`, "this mission's text mentions this
    file" (Joe's 2026-06-02 rename from `describes`; `relation/semantics
    :mission/mentions-file`, subtype `:mentions/stated`). 2,782 edges live.
    This is already a mission → real-code link in substrate-2, checked by
    path, and the nearest existing thing to what this mission wants.
  - **Proposal (not yet Joe's):** one reader over mission files that
    resolves each `[[X]]` in order: a mission/excursion id → a mission link;
    a memory slug → a memory link; otherwise an entity introduced by that
    mission, attributed to the commit's `Agent-Id` trailer. Such an entity
    is shown as pending until its mission's Status is COMPLETE, then minted.
    Relations from it (`built-on`, ...) are claims shown as yes / not yet /
    unknown by the queries M1a describes; a verb with no query reads
    "intended".
  - **Where the reader goes (claude-13, 2026-11-18):** parse in
    `file_ingest.clj` (it already re-reads a mission file when it changes,
    so no second watcher and no rescans); write through the workplan writer
    `sessionista_introductions` uses, so introductions and links sit in the
    same store. If M5 later picks a single entity store, only the write
    target moves. Details:
    1. Read Status with futon2's `mission-registry` classifier, as Giraffe
       does, so the two never disagree about whether a mission is complete.
    2. Attribute per new or changed `[[X]]` line on the change event (one
       blame each); a commit with no `Agent-Id` trailer is Joe, unsigned,
       not dropped.
    3. A target that matches nothing and no mission introduces is a typed
       **unresolved**, not a new entity, so a typo does not become an entity.
    4. A relation written beside a link is stored as written, verb and all;
       its query (e.g. CL-76's `built-on?`) runs when the link is read.
- **M3. The existing lexicon.** For the 254 `@entity` blocks: which name real
  code in `@where`/`@path` that substrate-2 can confirm, and which are ideas
  with no code yet (Nipper's case)?
- **M4. The 589 loader relations and the co-mention candidates.** Keep,
  convert or drop? They were written as `:workplan/referent` but mean
  something else.
- **M5. Readers.** Mojira, sessionista-mode and session-mode each read a
  different source. Which single source should all three read?

## 3. Exit

- One place entities come from, read by Mojira, sessionista and session-mode.
- Every stored link says what it means, or is visibly a bare "grounded in";
  no record type holds two meanings.
- Agents can add an entity and its links from a mission file, and the
  entity appears when the mission completes.
- `[[lexicon]]` minted, with its links written here.
