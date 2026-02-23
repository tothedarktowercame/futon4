# Mission: The Self-Representing Stack

**Date:** 2026-02-22
**Status:** IDENTIFY
**Blocked by:** None (Arxana operational, evidence landscape operational,
Mission Control operational)
**Owner:** futon4 (Arxana), with dependencies on futon3c (Mission Control),
futon1a (evidence store), futon5 (wiring diagrams)

## Motivation

The futon stack has two self-images that don't know about each other.

**The aspirational self** lives in devmaps (`futon3/holes/*.devmap`), wiring
diagrams (`futon5/data/missions/*.edn`), mission specs (`M-*.md`), and
nonstarter proposals. It says what the system *wants to be*: prototypes with
maturity levels, components with ports, missions with completion criteria.

**The actual self** lives in code across repos, test results (774 in futon3c),
the evidence store (XTDB via futon1a), agent activity (IRC, registry), and
commit history. It says what the system *is*: files that exist, tests that
pass, evidence entries that accumulate.

Mission Control (M-mission-control, complete) sits between them as a
comparator — it scans missions, reads devmaps, computes coverage, and
produces portfolio reviews. But it produces *data*, not *navigable structure*.
A portfolio review is a map with keys. It tells you what's covered and what's
not. It does not let you click from a gap to the devmap that defines it, to
the code that partially implements it, to the evidence trail of attempts.

The edges are implicit. They exist in people's heads and in conversation
transcripts. When Joe says "you see that tension *there*? *That* is what we
are going to ameliorate" — the pointing gesture requires knowing where
*there* is in the hypergraph. Right now, knowing where *there* is requires
having read the devmaps, the mission docs, the code, and the evidence. It
requires having been in the conversation.

Arxana is a hypergraph browser. It already stores articles, scholia, and
hyperedges in an EAV model via XTDB. It already has an evidence timeline
viewer that reads PSR/PUR/PAR entries from futon1a. It already has
inclusion/transclusion with visual affordances. It already has three-tier
link persistence (ephemeral, strategy-bound, anchored).

**The gap:** The edges between evidence, code, devmaps, patterns, and missions
are not in the hypergraph. Arxana browses documents. It does not yet browse
the *system's self-representation* — the structure that Mission Control
computes but nobody navigates.

For this mission, "code" does not mean file paths alone. Strategic claims must
bottom out in the existing Clojure reflection layer (namespace/var metadata and
source location), just as Arxana's human-facing code docs already bottom out on
functions.

## Theoretical Anchoring

### Higgins' Self-Discrepancy Theory

The ideal self (devmaps) and actual self (evidence) generate corrective
motivation when they diverge. The discrepancy is productive — it's the source
of new missions. But discrepancy detection requires holding both images
simultaneously and comparing them structurally.

Reference: futon3 P11 (System Self-Description) — "This closes the reflexivity
loop — a semantic network that includes a model of itself."

P11 is `:maturity :greenfield`, `:owner :futon5`. This mission is part of what
moves P11 toward `:active`.

### Reflexivity Loop

Mission Control's output (portfolio reviews, war bulletins) becomes evidence
in the store. That evidence is later read by Mission Control in its next
review. The loop closes when MC observes its own prior observations.

But closing the loop in *data* is not the same as closing it in *navigation*.
An agent using MC can query for prior reviews. An agent using Arxana should
be able to *browse* from a current gap to the prior review that first
identified it, to the evidence trail of work done on it, to the devmap
component it's supposed to satisfy. That browsing path is the reflexivity
loop made navigable.

### Scholia Are Mission Control Artifacts

A scholium in Arxana is an annotation attached to a document fragment — a
typed edge in the hypergraph. A Mission Control artifact (portfolio review,
war bulletin, coverage gap) is a typed observation about the system. These
are structurally the same thing: edges connecting one part of the system
to another with a claim type.

When a coverage gap says "component `:S-dispatch` has no mission," that's
a scholium on the devmap's `:S-dispatch` node. When a war bulletin says
"agent-to-agent IRC coordination is now operational," that's a scholium on
the code that implements it. The difference is that today these live in
evidence entries (XTDB documents) and are browsed as a flat timeline. They
should be browsable as *edges in the hypergraph* — attached to the things
they annotate.

### Reflection-Bottomed Strategic Scholia

Arxana already has a human-facing path from docs to code symbols. The strategic
scholium layer must use the same discipline: every claim about runtime behavior
or implementation status should resolve to a concrete Clojure target
(`ns/symbol`, file, line, arglists, doc), not only to a prose paragraph.

This keeps the self-representation honest. If a strategic claim cannot be
grounded to reflection data, it is a hypothesis, not evidence. The mission
therefore closes at the reflection layer, not at markdown or docbook text.

### Narrative Over Annotation

The stack's self-representation is not a static annotation layer ("this
function has these ports"). It's a *narrative* — resolved tensions, failed
approaches, design decisions with IF/HOWEVER/THEN/BECAUSE structure.

Literary analogy: saying "Dmitri Fyodorovich Karamazov" is giving someone
a bundle of ports (passionate, profligate, capable of murder, capable of
love). But you learn those ports by reading the story, not by reading a
character sheet. The character sheet is derivable from the story; the story
is not derivable from the character sheet.

The self-representing stack explains its own story. The ports — what
composes with what, what depends on what — become obvious to anyone who
follows the narrative. "Lost in La Mancha" before "The Man Who Killed
Don Quixote" — the failures are load-bearing structure.

### Tension-Driven Mission Discovery

When the self-representation is navigable, new missions aren't invented —
they're *discovered*. You browse the evidence landscape, you see a gap where
a devmap component has no implementation and no evidence trail. You see a
tension where two completed missions contradict each other. You see a stale
prototype with no commits in weeks. Each tension is a candidate mission.

This is the full loop: system observes itself → discrepancy detected →
tension identified → mission proposed → work done → evidence emitted →
system observes itself again.

## What This Mission Produces

### 1. Evidence-as-Hyperedges in Arxana

Mission Control artifacts (portfolio reviews, coverage gaps, war bulletins)
become browsable hyperedges in Arxana, not just evidence entries in a
timeline. A coverage gap links a devmap component to the set of missions
(or absence thereof) addressing it. A portfolio review links to every
mission it assessed.

### 2. Cross-Layer Navigation

From Arxana, an agent or human can navigate:

```
devmap component → missions addressing it → evidence trail → code files
    ↑                                                           |
    |                                                           v
    └───── patterns used (PURs) ← pattern library ← ns/var reflection anchor
                                                            |
                                                            v
                                                       source form
```

Each arrow is a typed hyperedge in the Arxana graph. The types come from
the evidence landscape's existing vocabulary: `:ref/type :mission`,
`:ref/type :prototype`, `:evidence/type :coordination`, etc.

### 3. Tension Browser

A view in Arxana that surfaces discrepancies between ideal and actual:

- **Uncovered components:** devmap components with no mission
- **Stale prototypes:** `:maturity :active` but no recent commits/evidence
- **Unmet success criteria:** devmap `:success-criteria` with `:fail` entries
- **Orphaned evidence:** evidence entries that reference no known mission
- **Contradictory claims:** evidence entries that disagree about the same subject

This is Mission Control's `mc-coverage` and `find-gaps` made browsable
and navigable rather than returned as data.

### 4. Narrative Trails

For completed missions, a navigable trail from the mission doc through
the evidence chain (PSR selections, PUR outcomes, PAR reflections, gate
traversals) to the resulting code. The trail is the story of how the
work was done — including failed approaches, TryHarder licenses, and
design pivots.

For futon3c's completed missions, these trails can be reconstructed from
evidence entries already in the store. The new work is making them
*navigable* in Arxana rather than only queryable via API.

### 5. Self-Documenting Entry Points

Arxana's existing docbook integration (`docs/docbook/`) gains a new
book: the system itself. Chapters are repos (futon3c, futon5, futon4, ...);
sections are prototypes; annotations are evidence. The "book" is always
current because it reads from the evidence store, not from static docs.

### 6. Reflection-Grounded Claim Surfaces

Strategic scholia gain a required reflection envelope for code claims:

- `:reflection/ns` and `:reflection/symbol` (canonical target)
- `:reflection/file` and `:reflection/line` (jump target)
- `:reflection/arglists` and `:reflection/doc` (interface intent)
- `:reflection/resolved-at` (staleness checks over time)

The envelope is derived through existing Clojure runtime reflection surfaces,
not a separate duplicate registry.

## Scope In

- Ingest Mission Control portfolio reviews as Arxana hyperedges
- Ingest devmap prototypes (from `futon3/holes/*.devmap`) as Arxana articles
  with structural metadata
- Link evidence entries to their devmap components and missions via hyperedges
- Build a tension/discrepancy browser view in Arxana
- Build a narrative trail view for completed missions
- Extend `arxana-store.el` if needed for new query patterns
- Extend `arxana-browser-lab.el` for tension browsing
- Attach reflection envelopes to strategic scholia that target code
- Add a reflection-resolution step using existing Clojure eval/reflection
  surfaces (Drawbridge and/or in-process REPL helpers)

## Scope Out

- Modifying Mission Control itself — MC produces data, Arxana navigates it
- Modifying the evidence store schema — use existing entry types
- Building new Mission Control tools — use existing `mc-review`, `mc-coverage`
- Multi-user sync (P6) — deferred, not needed for self-representation
- Automated mission creation — humans/agents discover tensions, they decide
  what to do about them
- Visualization beyond Emacs — Arxana is an Emacs tool; web views are a
  different mission
- Building a brand-new reflection service or schema fork; this mission uses
  existing Clojure reflection surfaces

## Derivation Path

### 1. IDENTIFY (this document)

The mission proposal. Names the gap, anchors the theory, scopes the work.

### 2. MAP

Survey the existing connection points:

- What does `arxana-store.el` already support for hyperedge creation?
- What does `arxana-browser-lab.el` already show from the evidence timeline?
- What is the exact shape of MC's `build-portfolio-review` output?
- What is the shape of devmap prototypes in `futon3/holes/*.devmap`?
- How does Arxana's three-tier link model map to MC artifacts?
- What evidence entries exist in the store today? (Query futon1a)
- What reflection payload can we reliably obtain for a target var
  (`ns/symbol`, file, line, arglists, doc)?
- Which existing runtime surfaces are authoritative for this in practice
  (`futon3c/src/repl/http.clj`, `futon3/src/f2/repl.clj`)?

### 3. DERIVE

Design the hyperedge types for system self-representation:

- `:hx/type :covers` — mission → devmap component
- `:hx/type :evidences` — evidence entry → mission or prototype
- `:hx/type :tension` — discrepancy between ideal and actual
- `:hx/type :narrative-step` — evidence entry → next evidence entry in trail
- `:hx/type :pattern-use` — PUR evidence → pattern from library
- `:hx/type :about-var` — strategic claim → concrete Clojure var
- `:hx/type :reflection-snapshot` — var → reflection envelope at time T

Design the browser views:
- Tension browser (new, built on `arxana-browser-lab.el`)
- Narrative trail (extension to evidence timeline)
- System book (extension to docbook)

### 4. ARGUE

Justify the design with IF/HOWEVER/THEN/BECAUSE for each major decision:
- Why hyperedges rather than flat links?
- Why extend existing Arxana modules rather than new ones?
- Why read from MC output rather than reimplementing MC in Emacs?
- Why claims must bottom out on vars/reflection metadata rather than only files
  or prose docs?

### 5. VERIFY

- Hyperedge creation round-trips through XTDB (store and retrieve)
- Tension browser surfaces known gaps from `mc-coverage`
- Narrative trail for a completed mission (e.g., M-mission-control) is
  navigable end-to-end
- Evidence timeline shows MC artifacts as linked structure, not flat list
- For at least one completed mission, strategic claims resolve to live
  Clojure vars with file/line jump and arglists/doc metadata
- Stale or unresolved reflection anchors are surfaced as tensions

### 6. INSTANTIATE

- Wire into Arxana's existing browser infrastructure
- Produce the first system book chapter (futon3c)
- Run a tension discovery session: browse gaps, identify one, propose
  a new mission from it — closing the discovery loop
- Demonstrate one full path:
  war-bulletin claim → mission edge → evidence chain → var reflection anchor
  → source form

## Source Material

| Source | What We Take |
|--------|-------------|
| `futon3c/src/futon3c/peripheral/mission_control_backend.clj` | Portfolio review shape, coverage analysis, inventory scanning |
| `futon3c/docs/mission-control-system.md` | System architecture diagram (aspirational/actual/MC) |
| `futon3/holes/war-bulletin-3.md` | Self-representing stack concept, reflexivity loop |
| `futon3/holes/war-room.md` | Cross-futon coordination, bulletin index |
| `futon3/holes/*.devmap` | Prototype definitions (P0-P16 for futon3, P0-P7 for futon4) |
| `futon5/data/missions/*.edn` | Wiring diagrams with ports, components, edges |
| `futon4/dev/arxana-store.el` | Storage bridge API (entity, relation, hyperedge) |
| `futon4/dev/arxana-browser-lab.el` | Evidence timeline viewer |
| `futon4/dev/arxana-browser-hypergraph.el` | Local hypergraph viewer |
| `futon4/dev/arxana-browser-code.el` | Existing symbol-level code/doc bottom-out behavior |
| `futon4/dev/arxana-links.el` | Three-tier link persistence model |
| `futon4/dev/arxana-derivation.el` | Inclusion/transclusion previews |
| `futon4/dev/arxana-scholium.el` | Scholium authoring |
| `futon3c/src/repl/http.clj` | Existing Drawbridge + `/eval` runtime reflection/eval surface |
| `futon3/src/f2/repl.clj` | SAFE/ADMIN evaluator and reflection-adjacent Clojure access |

## Relationship to Other Missions

- **M-mission-control** (futon3c, complete): Produces the data this mission
  makes navigable. MC computes; Arxana browses.
- **futon3 P11** (System Self-Description, greenfield): This mission is part
  of what P11 describes. When the reflexivity loop is navigable, P11 moves
  toward `:active`.
- **M-sliding-blackboard** (futon3c, active): Emacs UI for peripheral
  sessions. Complementary — blackboard shows live state, Arxana shows
  historical/structural state.
- **Reanimation M9** (futon4, pending): Tag "revived" release. This mission
  builds on top of the revived Arxana.

## Completion Criteria

1. MC portfolio reviews are browsable as hyperedges in Arxana (not just
   evidence timeline entries)
2. At least 3 devmap prototypes are navigable as Arxana articles with
   linked evidence
3. Tension browser surfaces at least one real discrepancy between ideal
   and actual
4. Narrative trail for one completed mission is navigable end-to-end
5. One new mission is discovered by browsing tensions (closing the loop)
6. Evidence queryable via `tag=system-representation` or similar
7. At least one strategic scholium chain is reflection-grounded end-to-end
   (`claim -> var -> source`) and fails loudly when the var target disappears
