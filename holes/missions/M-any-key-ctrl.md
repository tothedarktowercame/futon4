# M-any-key-ctrl — CTRL moves behind ※, so that any item is a few keys away

**Status:** IDENTIFY (opened 2026-11-21 by claude-5, from Joe's turns that night).
**Owner:** Joe (operator); claude-5 drafting. Builds go to Codex seats and are reviewed.
**Repos:** `futon4` (this mission; the ※ prompt `dev/mojira-dispatch.el`, codes
`dev/mojira-codes.json`, views `dev/mojira-*.el`), `futon3c` (the endpoints under
`/api/alpha/circus/`, the turn store), `futon1b` (the store the queries read).
**Related:** [M-mojira](M-mojira.md) (the ※ prompt is Mojira's),
[M-mojira-lexicon](M-mojira-lexicon.md), futon3c
[M-repl-circus](../../../futon3c/holes/missions/M-repl-circus.md) (closed 2026-11-17;
its CTRL move table and "5 key-presses" goal are carried here) and
[E-circus-in-practice](../../../futon3c/holes/excursions/E-circus-in-practice.md),
[M-arxana-together](M-arxana-together.md) (its checkpoint is a CTRL transition),
`holes/labs/M-arxana-incubator/NOTE-ctrl-items-and-scholium-programming.md`.

## HEAD

### Operator-voice anchor

Joe, 2026-11-21:

> notice that ※ m x p is a bona fide CTRL expression: it finds the paths to other
> missions via common patterns.

> I guess what's interesting here is what ※ can really empower, and the whole CTRL
> layer in general. Possibly rereading the Mozilla documentation and the .clj
> defproject design, we could start to think about which "moves" could be useful as
> CTRL sequences.

> I liked your point about ※ i (another variant could be ※ e to look at "EGO", i.e.,
> the current turn).... I also really like the search-based and constructive uses.
> If we haven't yet written up a M-any-key-ctrl mission we should do so.

Earlier, carried from M-repl-circus: "everything will be no more than 5 key-presses
away" (Joe, 2026-11-13); "META items are used like M-x ... CTRL items ... need to
effect a transition from one state to another"; "META retains the nondeterminism of
being LLM-based whereas CTRL is deterministic at the level of graph queries and other
classical code"; "CTRL should eventually have its own registry of queries" (Joe,
2026-11-18); "XTQL as the 'CTRL layer' remains a kind of final authority" (Joe,
2026-11-18).

### What is already felt to be true

- Three route-finding moves were built and used on 2026-11-21, each one keystroke
  sequence, each a deterministic query, each answering in under 0.5 s:
  - `※ m x p`: mission → its patterns → other missions citing them (futon3c
    2d63eaa7e; futon4 e158110, df0f08e, 71fbdfe, b8b5550);
  - `※ s x p`: current session → its patterns → other sessions of the last 24 h
    (futon3c 9bd121061, f7b41ef5e; futon4 ef0a189, b8b5550);
  - `※ t x p`: turn at point → its patterns → other turns of the session (futon3c
    96c2fa192, f7b41ef5e; futon4 8d4775b, c8875e5).
- One CTRL transition exists, though it was not built as one: `C-c C-s` in the shared
  incubator form (futon4a 1233325). Before: the hub revision differs from the form's
  head answers; after: they match, and each act names the revision it came from.
- The ※ prompt reads its codes from one table shared with code-server Mojira, so a
  move added once works on both surfaces.

### Anti-glibness discipline

A shortcode is not a CTRL move because it has a key. A move counts only when (a) its
result is a query anyone can rerun, (b) for a transition, its before- and after-states
are queries and a dry run reports the same counts as the write, and (c) its receipt
can be looked up by id. A view that renders prose an LLM wrote is META, not CTRL.
2026-11-21 example of the risk: `※ t x p` mostly links turns by I Ching hexagrams,
which the pattern lookup attaches to nearly every turn, so "shares a pattern" said
little there; and `※ m x p` counted `wr-3`, which the citing mission says it did *not*
follow, as a shared pattern.

### Working-economy position

It underwrites the Mozilla application's claims that can be checked: "insight into how
their data is collected, stored, transmitted, and used" (a move that shows a person
what the record holds about them) and "new design patterns ... become a playbook" (a
move that promotes a recurring passage into a pattern). It is underwritten by
futon1b's indexed queries and the turn store's per-session links; where an index is
missing, a move cannot be built honestly.

### Carried-forward tensions

- Which moves read and which write: route-finding moves are safe to add; transitions
  need consent and a social exotype (wr-3, recorded as not followed in
  M-arxana-together).
- "Cited" is not "applied": the pattern scopes do not distinguish a pattern used from
  one named as broken.
- Letters collide by position (`※ x 3` is a Codex seat, `※ m x p` is "across"); Joe
  accepts this for now.

### Provenance

Written by claude-5 on 2026-11-21 from Joe's turns that night, the built shortcodes,
claude-13's CTRL note (2026-11-18) and the CTRL table in M-repl-circus (2026-11-16).

## IDENTIFY

### Motivation

There is no registry of CTRL moves and no definition of what makes a ※ code one.
The codes table (`dev/mojira-codes.json`) mixes views, URIs and queries; the CTRL
moves of M-repl-circus (name, cite, ask, wait, hand back, mark) are not reachable
from ※ at all; and the only transition built so far (the form checkpoint) records
its effect but not its before- and after-queries.

### Candidate moves (from 2026-11-21; none decided)

| code | kind | query or transition | serves |
|---|---|---|---|
| `※ i` | read | what the record holds about me: turns, acts, words, evidence; where each is stored, who can read it | Mozilla "Intentional": insight into how data is collected |
| `※ e` | read | EGO: the current turn, its patterns, marks, links and what answered it | inspecting one act in full |
| `※ p x m` | read | pattern → missions that applied it, with outcomes | defproject `:royalty-per-application`; its refutation test |
| `※ w x u` | read | take-up: words one person coined and another used | defproject `:measure` |
| promote | transition | a recurring passage across N missions becomes a pattern with authors, cited by the N | "new design patterns ... become a playbook" |
| release | transition | a unit sends an item to an outlet (Data Collective, cq), with consent | defproject `:evidence-to`; Intentional |

### Scope in

- A written definition of a CTRL move (read or transition) and the checks it must pass.
- A registry: each ※ code that is a CTRL move names its query (and, for transitions,
  its before, after and dry run) in the codes table or beside it.
- The existing moves brought under that definition: m/s/t x p, the form checkpoint.
- `※ i` and `※ e` as the first new moves.

### Scope out

- META moves (LLM-proposed actions) except as emitters of CTRL items.
- New indexes for `※ p x m`, `※ w x u` and promote until MAP shows what is missing.
- Transitions that publish outside the stack (release) until a social exotype exists.

### Completion criteria

- [ ] A definition of a CTRL move is written, with the three checks above, and each
      existing ※ code is classified against it (CTRL read, CTRL transition, or not).
- [ ] m/s/t x p and the form checkpoint each name their query (and the checkpoint its
      before/after) where a reader of the codes table can find it.
- [ ] `※ i` answers for Joe from live data, with where each kind of record is stored.
- [ ] `※ e` shows the current turn's patterns, marks and links from the turn record.
- [ ] Every item in the Mojira start view is reachable within 5 key-presses (the
      M-repl-circus VERIFY measure), counted by a script, not asserted.
- [ ] Pattern scopes can say "named as not followed", so `※ m x p` stops pairing
      missions over a pattern one of them broke.

### Relationship to other missions

Depends on M-mojira (the prompt and its views) and futon1b's query routes. Carries the
CTRL part of closed M-repl-circus. Enables checkable versions of claims in the Mozilla
application (M-arxana-incubator).

### Source material

`futon4/dev/mojira-dispatch.el`, `futon4/dev/mojira-codes.json`,
`futon4/dev/mojira-related-*.el`; `futon3c/src/futon3c/circus/routes.clj`;
`futon3c/src/futon3c/xiang/turn_store.clj`; the CTRL note and the defproject
(`~/notes/claude-5/incubator-defproject.clj`); the Mozilla call text (hub document
`mozilla-call`).

## MAP note: the Peeragogy flops as test cases (claude-5, 2026-11-21)

Joe, 2026-11-21: "another good reference source in this regard could be the Peeragogy
Flops archive, could any of those problems be solved with CTRL queries? Or at least
helped with some?" Read against `~/notes/peeragogy-flops/ARGUMENTS.md` (claude-16,
2026-10-29). Each candidate move should name a flop defeater it would have caught;
that is a stricter test than "seems useful".

| flop | defeater (from ARGUMENTS.md) | CTRL move | solved, helped, or not reached |
|---|---|---|---|
| 1 Paragogy, LAK 2011 | "No measures or tools"; "the coined term is not earned" | rule changes by who proposed them (read); `※ w x u` take-up of the coined term (read) | measured, not argued |
| 2 Knight 2012 | "Test far weaker than the claim" | claims as open queries `[a ? c]` until a witness exists; a move listing claims with no witness | helped; "none central" is a judgement |
| 3 Kickstarter 2012 | contributions "accounted for in money and time" | `※ i` per person over commit trailers, evidence, attributed words | the accounting, yes; the demand defeaters, not reached |
| 4 PAR, e/merge 2012 | "the case undercuts the claim"; "internal inconsistency" | the PAR is a CTRL transition done by hand: what was supposed to happen = before-query, what happened = after-query | helped; self-consistency of a text is beyond current queries |
| 5 Wikimedia 2013 | "Benefit runs the wrong way"; "endorsements are all from the team" | carry-back: `※ p x m` with "applied by"; endorsers' links to the team by authorship | checkable before submitting |
| 6 MIT CLS 2018 | "Survivorship sample" | the set of people whose last contribution is older than a date | helped: the leavers can be found; asking them is social work |
| 8 Futures 2020 | "No comparison"; "fiction presented as evidence" | predictions with `:refuted-if` and their status (defproject `predictions`) | helped; a comparison condition is research design |

Not reached by any CTRL move: demand and who benefits (2, 3, 5) and why people stay
(6), which ARGUMENTS.md already lists as unchanged by agentic systems. CTRL makes them
visible (who benefited, who left), not wanted.

## Design notes kept (not decided)

- **Strong and weak links** (Joe, 2026-11-21: hexagram retrieval "might be OK if it was
  actually useful for anything. Maybe it is precisely useful for 'weak links'. We could
  create 2 layers of links accordingly"). `collaboration-coherence/weak-tie-conflation`:
  both kinds are useful, the failure is mistaking one for the other. Proposal:
  `iching/` and `iiching/` weak by namespace (test `eight-gates/` against the data),
  ranking by strong links only, weak links shown dimmed.
- **`C-u ※` as a universal toggle** (Joe, 2026-11-21): plain `※` returns strong links,
  `C-u ※` adds weak ones, for every move that has both readings.

## MAP note: "who" (claude-5, 2026-11-21)

Joe, 2026-11-21: "we don't have the 'who wants it' yet but if we had a stronger rolodex
we could start to make queries with 'who' involved". The demand and who-benefits
defeaters of flops 2, 3 and 5 need people as records.

What exists (field names only here; the records are private):
- People inside the stack, attributed per act: agent ids, Joe, and the Together editor
  logins (rob, murch).
- The sales Rolodex, futon7 `business/sales/sales.edn` (private repo): cards with
  `:card/name`, `:card/org`, `:card/role`, `:card/how-we-met`, `:card/first-contact`,
  `:card/relationship`, `:card/interest-territories`, `:card/referred-by`,
  `:card/referrals`, `:card/in-pipeline`; clients with `:client/stage`,
  `:client/engagement`; demos with `:demo/for`, `:demo/outcome`. Read by
  `dev/arxana-vsatarcs-sales.el` and `dev/arxana-fair.el` (paths fixed 2026-11-21,
  futon4 573bc65).
- Mail, indexed with notmuch; not linked to any mission, pattern or request.

Rules before anything is built (Mozilla "Intentional" applies to us too):
- Person records live only in private stores, never in a public repo.
- Built only from acts the person made (a message they sent, an edit under their login,
  a paper they co-wrote); every field records its source; no inferred traits.

Candidate moves once person records exist:
- [ ] who asked: requests for or about an item, by person and date (demand evidence).
- [ ] who used: take-up of a term or pattern, by person.
- [ ] who left: people with past acts and none since a date (flop 6's missing sample).
- [ ] `※ who x p`: people whose own acts touch a pattern.

## Candidate: `※ q ? w`, who is related to this question (Joe, 2026-11-21)

Joe: "one example that I have talked about with both Rob and Murch is basically
'inside this company, who should be talking to whom?' Run globally it would be a big
query. But if we narrowed it to ※ q ? w — (w)ho is related in some way to this
(q)uestion we could start to narrow it down. That could work well also in combination
with ArSE and that gets at the point that agents could also use the CTRL to move
around / do some stuff".

The `?` is M-repl-circus's open query `[a ? c]`: a want until a witness answers it.
As a CTRL sequence over what exists (futon3c `README-arse.md`):
1. **read** `※ q ? w`: for an ArSE question (or the turn at point), the people and
   agents whose own acts touch it: shared patterns (the same retrieval that tags turns,
   run on the question text), the same mission, authorship of the passages it cites,
   Rolodex interest territories. Ranked, top 10, strong links only unless `C-u`.
2. **transition** ask: route the question to a chosen person or agent as a typed bell
   (`--type query --ref <thread-id>`, which exists and does not duplicate the question);
   receipt = the job id.
3. **witness**: the answer (`--type answer --ref`) closes the open query.

Agents can run the same sequence: an agent holding an unanswered question (24 open on
2026-11-21, `GET /api/alpha/arse/unanswered`) picks whom to ask by query instead of by
guess. Scale is held down by starting from one question, not the company.

- [ ] `※ q ? w` returns ranked candidates for one live ArSE question, each with the
      acts that link them, in under 2 s.

Extension: [E-arse-cq](../excursions/E-arse-cq.md) (2026-11-21) bridges ArSE questions
to Mozilla.ai's cq knowledge units: a cq `query` before asking anyone, and an optional
`propose` ("release") after an answer. Gated on Joe for any install or remote.

## DOCUMENT, as a script: the first performance of `※ x p` (2026-11-21, 04:35–04:55Z)

Joe, 2026-11-21: "we could think of the document phase as a way to collect a kind of short
script of what actually happened in the action arena ... the action arena just takes that
theatrical thing and says, okay, well, it's repeatable in the real world". Written from
the invoke ledger (job events) and commit trailers; times UTC; nothing here is
reconstructed from memory except the order of Joe's turns.

**Dramatis personae.** JOE, the director. CLAUDE-5, the lead: writes the scenario,
casts, reviews. CODEX-26, builds endpoints (futon3c). CODEX-3, builds views (futon4).
THE LEDGER, which records every cue.

**Scene 1. The idea.** JOE: "※ m x p might work as a shortcode?" CLAUDE-5 checks the
store answers "which missions cite pattern X" (15 ms), writes one response shape for
both builders, and sends two packets.

**Scene 2. A missed cue.** 04:35. Both packets go out in brief mode, in which a seat
does nothing (the recommended script, `bell-file.sh`, never passed a mode). CLAUDE-5
notices, cancels both (04:36), resends as work, and fixes the script (futon3c
6dd9b5abb).

**Scene 3. Two builds in parallel.** 04:36–04:38 CODEX-26 builds the endpoint
(2d63eaa7e); 04:36–04:41 CODEX-3 builds the view (e158110). CLAUDE-5 reviews both and
loads the view into JOE's Emacs.

**Scene 4. The director's note.** JOE: "Maybe top 10 by default." 04:39–04:43 CODEX-3
adds the limit (df0f08e). CLAUDE-5 finds the header claims "of 11" when 11 is only what
was fetched, and fixes it himself (71fbdfe).

**Scene 5. The second number.** JOE asks for `※ s x p`. 04:44–04:47 CODEX-26 builds the
sessions endpoint (9bd121061); 04:44–04:46 CODEX-3 the view (ef0a189).

**Scene 6. The third, and a slow line.** 04:47–04:51 both build `※ t x p` (96c2fa192,
8d4775b). The endpoint answers correctly in 47 s. CLAUDE-5 sends it back; 04:51–04:55
CODEX-26 profiles first, finds 47 s spent in a projection the endpoint never needed, and
cuts it to 0.06 s (f7b41ef5e). JOE asks for "2 shared patterns" on each line; CLAUDE-5
makes it so in all three views (b8b5550, c8875e5).

**Curtain.** Three moves built, reviewed and loaded in twenty minutes of builders' time;
nine jobs, two of them wasted by the missed cue.

What the script shows that the commit log does not: the shape of the arena. One lead,
two builders in fixed lanes (endpoint, view), the director's notes arriving mid-scene,
and the lead fixing small things himself rather than recasting. Whether that shape
repeats in other missions is the action-arena question: a script read across several
performances is how the regularities (and the recurring missed cues) would show.
