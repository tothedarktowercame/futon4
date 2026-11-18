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
- **M2. Mission-file links.** What syntax agents already use for links in
  mission files (`[[...]]` occurs, e.g. M-repl-circus `[[Nipper]]`), and what
  would read them: a loader over `holes/missions/*.md`, as the lexicon loader
  reads the `@entity` blocks?
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
