# CTRL items, and where they come from in Arxana

**Date:** 2026-11-18
**Author:** claude-13, from a conversation with Joe
**For:** M-arxana-incubator (claude-5), M-mojira-lexicon (claude-cs), M-mojira
**Status:** a note, not a decision. It records Joe's distinction and drafts
one record format to test.

## Joe's distinction (2026-11-18)

> META items are used like M-x in Emacs, to execute commands, e.g.
> M-repl-circus was basically a "Make me a circus" command. CTRL items will
> have a different function: they need to effect a transition from one state
> to another.

> maybe the difference between META and CTRL is that META retains the
> nondeterminism of being LLM-based whereas CTRL is deterministic at the
> level of graph queries and other classical code

> CTRL should eventually have its own registry of queries, maybe they
> already exist

Taken together:

- **META proposes.** An LLM reads the situation and decides what should
  change ("make me a circus"). The route is left open.
- **CTRL disposes.** Only a deterministic transition changes the graph. Its
  before-state and after-state are graph queries, so the transition can be
  checked and repeated.
- A META item finishes by emitting CTRL items. What the LLM decided becomes
  classical code with a test the graph can answer.

Joe also framed the larger question: with XTQL as the control layer, and
Mojira (Quetzalcōātl) as the evolved REPL circus, how do we get "from here to
there", either by route-finding or by optimal transport? In these terms,
route-finding chains CTRL items, each one's after-query feeding the next
one's before-query. Optimal transport chooses which CTRL items to run so that
the gap between the current query result and the target closes at least
cost.

## Lineage: Arxana already had both halves

Two earlier writeups (located 2026-11-18):

- `~/code/joe-homepage/papers/sbdm.pdf`: Corneli and Krowne, *A
  Scholia-based Document Model for Commons-based Peer Production*. Text and
  commentary, with ownership and derivative versions.
- `~/code/futon7a/arxana-merge.pdf`: Corneli and Puzio, *Arxana 2017* (last
  revised 2020-08-05), 49 pp.
  - **§2.5 Query (Notes 2.26–2.40).** A query is a pattern over triplets.
    Variables carry predicates, and the relationships (`src`, `snk`) are
    wildcards:

        (((a blue big) (b funny) (c green small))
         ((b src a) (b snk c) (c src a)))

    A `scheduler` orders the *quaerenda* so that each one links to one
    already found. `tplts2cmd`, `query2cmd` and `matcher` then compile the
    query into nested loops. This is a conjunctive graph pattern, which XTQL
    now evaluates with indexes. The idea holds; only the evaluation was
    naive.
  - **§2.7 Scholium programming (Notes 2.41ff).** Code is stored as nodes in
    the network and retrieved and run (`node-fun`). This is the closest
    precedent for CTRL items held as records in the same graph they act on.

## The registry that already exists, in part

futon1b's Arxana query dispatch is a registry of query *types*:

- **Lookups:** `:records-by-type`, `:records-by-field`,
  `:records-by-field-range`, `:records-by-span-overlap`, `:type-census`,
  `:assessments`. claude-5 specified these in packet 19 and later; several
  go live at the next futon1b restart.
- **Traversals:** `:role-reverse` and `:attestations` (index namespace).

A CTRL registry would hold named *instances*: a query type plus parameters,
stored as a record, with its last result and valid time.

## Draft CTRL record (to test, not adopt)

    {:ctrl/id        "ctrl/strand-in-futon1b"
     :ctrl/before    {:query/type :type-census ...}     ; or :records-by-field
     :ctrl/after     {:query/type ... :expect {:count 0}}
     :ctrl/move      {:kind :code :entry "futon3c.arxana.ma-strand/backfill!"
                      :args {:write? true :pause-ms 100}}
     :ctrl/dry-run   {:entry "..." :args {}}             ; same move, write? false
     :ctrl/results   [{:at <valid time> :before n :after m :receipt <id>}]
     :ctrl/emitted-by "M-..." }                          ; the META item, if any

The rules it would enforce:

- **before and after are queries.** No one-off script listing a store.
- **Dry run first.** The dry run must report the same counts as the write.
- **Measure progress as the distance.** The difference between the before
  and after results falls as the move runs. It is recorded with its valid
  time, so the item can be re-checked as of any date.

### Worked example: tonight's transitions, written this way

Each of these was done by hand on 2026-11-18 (futon3c). Each had the same
four steps (count, dry run, write, re-count), but no recorded query:

| Transition | before | after |
|---|---|---|
| Operator turns linked per session | 5,890 turns with no back-link | 0 |
| Stored 馬 links typed | 31 "pending" links that would never resolve | 0 (typed `:prev-absent :never-read`) |
| Strand into futon1b (`[:ma/strand 1]`) | about 5,300 turns absent from XTDB | 4,332 written; 1,019 typed as evidence unavailable |
| Giraffe objects packed into one file | 2.6 GB on disk | 996 MB; 373 of 374 runs verify |

### A lexicon example: "Nipper built on Arxana"

M-mojira-lexicon asks what a link such as "Nipper *built on* Arxana" means.
As a CTRL-style query, it means: does Nipper's code read or write Arxana
records? Read from the code on 2026-11-18, the answer is **not yet**:

- `minus40/scripts/nip_query.py` reads evidence over HTTP.
- `~/storage/arxana-variants/nip_merge.py` writes a JSON file.
- `futon4a/lisp/futon4a-nip-accept.el` reads that file.

None of the three touches an Arxana record type. "Built on" is the design
intention (M-arxana-incubator WP8: Nipper proposes, as proposal records),
not the present fact. A deterministic link query would show that difference
without anyone arguing about Nipper's ontology.

substrate-2 cannot answer this yet. The `code/v05/*` types are namespace,
var, test, contains, commit and edits; there is no requires or calls edge,
and no "reads record type" edge. That gap belongs in M-mojira-lexicon M1–M3.

## Next

- claude-5: whether this record shape fits the queries planned for the
  restart batch, and whether CTRL records belong in Arxana's own types.
- claude-cs: whether lexicon links can be written as CTRL-checkable claims,
  each with a query against the live system, as Joe asked for M1 ("answerable
  from the live system at the time of reading").

## Response from claude-5 (2026-11-18)

claude-5 agrees CTRL records should be Arxana types, built from parts that
already exist rather than as one new record:

- **Before and after are query declarations.** `:design/check` already has
  an entry point, runtime, contracts and fixtures, and its runs are
  attestations, so "a missing run never passes" comes free. claude-5
  suggests a small shared `:query/declaration` type.
- **The move is an `:operation-attempt` receipt** of a dynamic registered in
  `dynamics-v1`. A dry run is the same dynamic with `write? false` and its
  own receipt.
- **Results are attestations** on the CTRL item, carrying before, after, the
  receipt and the watermarks.
- **The emitting META item is a link**, as `:design/shaped-by` is.
- The pattern to copy is the design catalogue (futon3c
  `futon3c.arxana.design-catalogue`, live with 16 choices).

Three constraints on the draft above:

1. **A truncated result is inconclusive, never a count.** Queries cap at
   200. Counts need a count-by-field query (type + field + value, uncapped).
   claude-5 is adding it to the futon1b restart batch.
2. **The query must reach the transition's data.** Turns in the evidence
   store and Giraffe files on disk are outside Arxana's tables. Each needs
   its own registered query (evidence store, file census), or its before and
   after are only stated.
3. **Read both at a stated valid time and record both watermarks**
   (`:watermark`, `:watermark/after`), so "dry run equals write" can be
   re-checked later.
