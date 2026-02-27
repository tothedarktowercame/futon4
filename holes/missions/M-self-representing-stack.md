# Mission: The Self-Representing Stack

**Date:** 2026-02-22
**Status:** DERIVE + ARGUE complete (2026-02-24), ready for VERIFY
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

### The Stack as One AIF+ Organism

(Added 2026-02-24, conversation between Joe and Claude.)

The strongest resolution of the "too much to hold in my head" problem:
represent the entire futon stack as a single futon5 AIF+ diagram.

Each repo is a subsystem with a timescale and a role:

| Subsystem | AIF Role | Timescale |
|-----------|----------|-----------|
| futon7 | Markov blanket (external world boundary) | boundary |
| futon0 | Human-organism coupling surface | social |
| futon3c | Social loop (real-time coordination) | social (seconds) |
| futon3b | Task loop + Baldwin cycle (gate pipeline + library evolution) | fast + glacial |
| futon3a | Interoception (pattern library query, internal state sensing) | on-demand |
| futon1a | Memory (evidence store, XTDB) | persistent |
| futon4 | Self-model (Arxana, reflexivity, navigable self-representation) | reflective |
| futon5 | Generative model (wiring diagrams, AIF+ formalism — not yet active) | structural |

**Nesting, not containment.** In AIF, blankets nest and compose. Whether
Joe is "inside" or "outside" the stack is not a metaphysical commitment
— it's answered differently depending on what you're analyzing. For
coding agents, the stack is the game map. For the stack as a whole, the
map is approximated by GitHub (repos, issues, PRs, adjacent projects,
other people's work). Same structure at different scales.

**futon3a as interoception, not perception.** futon3a queries the pattern
library — the organism sensing its own structural repertoire ("what
patterns do I have available?"). External perception (what's happening
in the world, what users want, what other systems are doing) arrives
through futon7.

**futon5 as generative model.** Currently holds *descriptions* of wiring
(the three exotypes). Not yet wired as active inference — it specifies
but does not yet generate predictions or drive inference. The stack-level
diagram would be the first instance where futon5 describes *itself as
part of the organism it specifies*.

**futon7 as existential infrastructure.** Joe's "vitality" (futon0 term)
is currently essential to the stack developing further — a single point
of failure. Rob collaborates on futon6 but collaboration isn't
redundancy. futon7 resolves this by giving the stack more than one
channel to the world: other people, other systems, other sources of
vitality. This isn't about making Joe dispensable — it's about the stack
being robust enough that Joe's role shifts from bottleneck to participant.

**Dependency ordering.** The stack can't present itself to the world
(futon7) if it can't represent itself to itself (futon4). The self-model
is prerequisite to the Markov blanket being useful. But futon7 work can
start in parallel because the SPOF problem is urgent.

**Stack-wide consideration:** The stack-level AIF+ diagram belongs in
futon5. This mission (M-self-representing-stack, futon4) is about making
that diagram *navigable* via Arxana. futon5 defines the structure; futon4
makes it browsable; futon7 is the Markov blanket the diagram specifies.

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

### Hyperedges as Frozen Dynamics

(Added 2026-02-24, conversation between Joe and Claude.)

A hyperedge is not a list. The distinction matters at every level:

- **List/set** — membership only. `#{p1 p2 p3}` says "these together."
- **Frame/slot** — membership + named roles. GOFAI, JSON schemas.
- **Hyperedge** — membership + roles + topology. How endpoints connect
  *through* the edge, not just *at* it.
- **Wiring diagram** — hyperedges composed with port discipline and
  timescale constraints. The futon5 exotypes live here.

A hyperedge is like a frozen function — not a lambda (stateless, sequential)
but more like a Petri net or tangled program graph: places, transitions,
tokens, firing rules, all captured in the structure but not currently
executing. You can inspect, browse, and reason about the dynamics before
(or without) activating them.

This connects to Ted Nelson's transclusion, which Joe generalized to
**'clusion**: "do some things to those things and then do something with
the result." Nelson's transclusion is the degenerate case (one operation:
copy; one source; one destination). 'Clusion is the general case: gather
from multiple sources, transform, compose. The CS parallel is monadic
bind — chaining operations in a context — but richer, because the dynamics
are concurrent, not sequential. Forces in a pattern don't resolve in order;
they're in tension *simultaneously*, and the pattern is the resolution of
that concurrent tension.

**Consequence for Arxana:** The `props` map on a hyperedge isn't just
metadata — it carries the dynamics that make the connection meaningful.
A tension hyperedge doesn't just say "these three things are connected";
it captures *why* there's a tension (what forces pull in what directions,
what's missing, what contradicts what). A collection hyperedge doesn't
just enumerate members; it captures what the members *do together* — the
'clusion that relates them.

The futon5 wiring diagrams are already at this level: a `social-exotype`
isn't a list of components, it's a topology where S-presence feeds
S-dispatch feeds S-invoke, with invariants constraining valid configurations.
Arxana's hyperedges should reference these diagrams, not replicate their
structure — the hyperedge points into the dynamics, it doesn't flatten
them into a membership list.

### Homoiconicity and Self-Representing Hypergraphs

EDN lives inside Clojure (data literals), and Clojure lives inside EDN
(quoted forms as inspectable structure). The frozen/molten distinction is
a phase transition, not a type boundary: `eval` thaws data into computation,
`quote` freezes computation into data. Same substance, different temperature.

This makes the "frozen dynamics" in hyperedge props *literally executable*
— not a metaphor. A 'clusion pipeline can be quoted Clojure forms in EDN,
browsable in Arxana as structure, evaluable via Drawbridge as computation.

**Self-representing hypergraphs.** Following the futon1a pattern: futon1a
doesn't just enforce invariants, it *exposes them* — any agent can query
an API endpoint and get a machine-readable answer to "what rules does this
store enforce?" Plus README-archivist documents how to create new invariants.
The system teaches you how to extend it.

The same pattern applies to hypergraphs. "What edge types exist?" "What
are valid endpoint configurations?" "What 'clusion does this edge perform?"
— all queryable. The hypergraph explains its own structure, and provides
the equivalent of README-archivist for extending that structure with new
types, new relations, new collection patterns.

**Forces as computation.** Tension hyperedges don't just *represent* forces
— with enough frozen dynamics in their structure, they *effectively are*
forces. The force vectors, severity, direction of pull are all evaluable.
This makes "high curvature" computable: query the hypergraph for where
tensions concentrate, where unresolved forces accumulate, where the graph
needs to relax. The answer comes from evaluating the frozen dynamics, not
from reading a dashboard and making a judgment call.

This is active inference applied to the stack itself. Free energy
minimization IS tension relaxation. The system computes its own strain
and proposes relaxation — new missions, refactoring, pattern application.
The self-representing hypergraph isn't a static self-portrait; it's a
nervous system.

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

Survey completed 2026-02-24 (Claude + Joe session).

**Q1: What does `arxana-store.el` support for hyperedge creation?**

Fully implemented. `arxana-store--post-hyperedge` POSTs to `/hyperedge` with
`hx/type` and `hx/endpoints` (arbitrary list of entity IDs). Binary relations
via `/relation` also work. Batch creation via `/relations/batch`. The write
transport layer is ready — but no higher-level module ever calls the hyperedge
path. It is plumbed but unused.

**Q2: What does `arxana-browser-lab.el` show from the evidence timeline?**

Reads evidence entries from futon1a via `GET /evidence`. Filters by type and
author. Tabulated list display, individual entry viewer, reply chain follower.
Groups by session-id (client-side). Known evidence types: `pattern-selection`,
`pattern-outcome`, `reflection`, `gate-traversal`, `coordination`, `forum-post`,
`mode-transition`, `presence-event`, `correction`, `conjecture`. Purely a viewer
— no hyperedge creation, no connection to the three-tier link model.

**Q3: What is the exact shape of MC's `build-portfolio-review`?**

```clojure
{:portfolio/missions         [MissionEntry...]
 :portfolio/devmap-summaries [DevmapSummary...]
 :portfolio/coverage         [CoverageEntry...]
 :portfolio/mana             ManaSnapshot
 :portfolio/summary          "42 missions (12 complete, ...)"
 :portfolio/gaps             ["devmap/Component -- no mission" ...]
 :portfolio/actionable       ["mission-id (in-progress) [repo]" ...]}
```

Each `CoverageEntry` has `:coverage/uncovered` — a vector of component IDs
with no mission. This is the tension data Arxana needs to make navigable.
Exposed via `POST /api/alpha/mission-control {"action":"review"}`.

**Q4: What is the shape of devmap prototypes?**

88 prototypes across 9 `.devmap` files in `futon3/holes/`. These are flexiarg
format — the same DSL that Arxana already represents for `futon3/library`
patterns. Devmaps are a different flavour of design pattern, not a different
format. Key structural handles per prototype: `! instantiated-by: Prototype N
— Title [sigils]`, `:maturity` (stub/greenfield/active/settled), `:depends-on
[futonN/PK ...]`, `evidence[path/to/artifact]`, `:success-criteria pass[...]
fail[...]`, `:owner`, `:psr-example`, `:pur-template`.

Arxana's existing flexiarg handling should extend to devmaps with small changes
(devmap-specific fields like `:maturity`, `:depends-on`, `:success-criteria`).

**Q5: How does the three-tier link model map to MC artifacts?**

Currently: no connection at all. The three-tier model (strategies → voiced
links → anchored scholia) operates on code↔doc links. Evidence entries from
MC are in a completely separate world. This gap is precisely what the mission
fills — MC artifacts need to become hyperedge endpoints, and the tension
browser needs to surface the connections the link model currently ignores.

**Q6: What evidence entries exist in the store today?**

Queryable via `GET /api/alpha/evidence`. Each entry has `:evidence/subject
{:ref/type :ref/id}`, `:evidence/type`, `:evidence/claim-type`,
`:evidence/in-reply-to` (threading), `:evidence/tags`. MC steps emit entries
tagged `[:peripheral :mission-control :step]` with body containing the full
tool result. Backfill entries tagged `[:mission :backfill :snapshot]`.

Note: tag-based query is NOT in `EvidenceQuery` — it's a post-filter in the
HTTP handler only. Adding `:query/tags` to the query schema would be a small
but useful enhancement.

**Q7: What reflection payload can we reliably obtain?**

Already live and deployed at `GET /api/alpha/reflect/var/:ns/:var`. Returns
a `ReflectionEnvelope`:

```clojure
{:reflection/ns          symbol
 :reflection/symbol      symbol
 :reflection/file        string
 :reflection/line        int
 :reflection/arglists    (([args...]) ...)
 :reflection/doc         string
 :reflection/resolved-at Instant
 :reflection/private?    bool
 :reflection/macro?      bool
 :reflection/dynamic?    bool}
```

Plus namespace listing (`/reflect/namespaces`), dependency graph
(`/reflect/deps/:ns`), and Java class reflection (`/reflect/java/:class`).
This is exactly the shape proposed in the mission doc for reflection-grounded
strategic scholia — it already exists and is authoritative.

**Q8: Which runtime surfaces are authoritative?**

- `futon3c/src/repl/http.clj` — Drawbridge `/eval` endpoint. POST raw Clojure
  code, authenticated via `x-admin-token` header. Can call any function in the
  running JVM.
- `futon3c/src/futon3c/reflection/` — `core.clj` (list-namespaces, reflect-ns,
  reflect-var, reflect-deps) and `envelope.clj` (ReflectionEnvelope shape).
- HTTP reflection endpoints at `/api/alpha/reflect/*` — live, no auth needed.

**MAP Summary: What's Ready vs What's Missing**

Ready (no new code needed):
- Hyperedge write path (arxana-store)
- Evidence read path (browser-lab, HTTP API)
- MC portfolio review with coverage/gaps (HTTP API)
- Reflection envelopes with file/line/arglists/doc (HTTP API)
- Flexiarg representation in Arxana (for devmap ingestion)

Missing (the actual DERIVE/INSTANTIATE work):
1. Hyperedge read path in Emacs — no `fetch-hyperedge` or `load-hyperedges`
2. Evidence → hyperedge bridge — converting MC artifacts to hyperedge endpoints
3. Devmap flavour support in flexiarg handling (small extension)
4. Tension browser view (new UI or extension of browser-lab)
5. Narrative trail view (per-mission evidence story)
6. Tag-based query in EvidenceQuery (small schema extension)

### 3. DERIVE

Started 2026-02-24 (Claude + Joe session). Inline IF/HOWEVER/THEN/BECAUSE
justifications accompany each design decision. The consolidated ARGUE
synthesis follows the DERIVE section.

#### Endpoint Entity Types

The hypergraph connects things. These are the things:

| Entity Type | Identity | Source | Already in XTDB? |
|---|---|---|---|
| Mission | `mission:<id>` | MC inventory, M-*.md files | As evidence subject refs |
| Devmap | `devmap:<futonN>` | `futon3/holes/*.devmap` | No — needs ingestion |
| Prototype | `prototype:<futonN/PK>` | Inside devmap files | No — needs ingestion |
| Component | `component:<devmap>/<id>` | futon5 wiring EDN | No — needs ingestion |
| Evidence entry | `e-<uuid>` | Evidence store | Yes |
| Pattern | `pattern:<id>` | `library/**/*.flexiarg` | As evidence refs |
| Clojure var | `var:<ns>/<symbol>` | Reflection API | No — created on demand |
| Repo/Subsystem | `repo:<futonN>` | Stack-level diagram | No — needs creation |

**IF** some endpoints already exist as evidence subject refs (`{:ref/type
:mission :ref/id "M-xxx"}`) **HOWEVER** these are embedded in evidence
entries, not first-class navigable entities **THEN** we create Arxana entities
for missions, prototypes, and components so they can be hyperedge endpoints
**BECAUSE** you can't browse *to* something that doesn't have an entity ID.

Devmaps and prototypes are flexiarg — Arxana already represents flexiarg
patterns from the library. Devmaps are a different flavour of design pattern,
so extending existing flexiarg handling to cover devmap-specific fields
(`:maturity`, `:depends-on`, `:success-criteria`) is a small change.

#### Hyperedge and Relation Types

**Design decision: relations (binary) vs hyperedges (n-ary)**

**IF** most connections are binary (mission covers component, evidence
evidences mission, PUR references pattern) **HOWEVER** some connections are
genuinely n-ary (a tension involves ideal + actual + gap; a narrative step
connects evidence, mission, and pattern in context) **THEN** use binary
relations as the default and reserve hyperedges for genuinely n-ary
connections **BECAUSE** binary relations are simpler to create, query, and
render, and `arxana-store-create-relation` is already well-exercised while
`arxana-store--post-hyperedge` has no callers yet.

**Binary relations** (type field on Arxana relation):

```
type: "covers"
  src: mission:<id>        dst: component:<devmap>/<id>
  props: { coverage-source: "mc-review", reviewed-at: "<ISO>" }
```
MC's `compute-coverage` produces the data. One relation per (mission,
component) pair. Created from portfolio review output.

```
type: "evidences"
  src: e-<uuid>            dst: mission:<id> | prototype:<futonN/PK>
  props: { evidence-type: "...", claim-type: "..." }
```
Reifies the `:evidence/subject` ref as a navigable relation. Created by
scanning existing evidence entries and matching subject refs to entities.

```
type: "pattern-use"
  src: e-<uuid>            dst: pattern:<id>
  props: { outcome: "success|partial|fail" }
```
Reifies `:evidence/pattern-id` from PUR entries as a navigable relation.

```
type: "narrative-step"
  src: e-<uuid>            dst: e-<uuid>
  props: { mission: "<id>", step-index: N }
```
Sequential ordering within a mission's evidence trail. Supplements
`:evidence/in-reply-to` (which is per-thread, not per-mission-narrative).

```
type: "about-var"
  src: <scholium-or-claim-id>   dst: var:<ns>/<symbol>
  props: { :reflection/file, :reflection/line, :reflection/resolved-at }
```
Grounds a strategic claim to a concrete Clojure var. The reflection envelope
fields are copied into props at creation time. If the var later moves or
disappears, `resolved-at` becomes stale — the tension browser surfaces this.

**IF** voiced links already connect code symbols to doc paragraphs
**HOWEVER** `about-var` connects *claims* (scholia, evidence, mission
assertions) to code symbols, which is a different direction **THEN** this
is a new relation type, not an extension of voiced links **BECAUSE** the
source is a strategic claim, not a doc paragraph.

**Hyperedges** (n-ary, using `hx/type`):

```
hx/type: "tension"
  endpoints: [component:<devmap>/<id>,   ;; the ideal (what should exist)
              mission:<id> | nil,         ;; the mission addressing it (or absent)
              e-<uuid> | nil]             ;; latest evidence (or absent)
  props: { tension-type: "uncovered|stale|unmet|contradictory",
           detected-at: "<ISO>",
           detected-by: "mc-review|tension-scan",
           summary: "S-dispatch has no mission" }
```
A tension is genuinely n-ary: it connects an aspiration (component/prototype),
the work addressing it (mission, possibly absent), and the evidence trail.
When a component has no mission, the second endpoint is nil — the absence IS
the tension.

**IF** we could model tensions as binary (component → gap-marker)
**HOWEVER** the whole point is navigability across layers — from gap to
mission to evidence to code **THEN** the tension hyperedge connects all
relevant layers in one browsable structure **BECAUSE** the user needs to
see the full context when pointing at a tension, not chase binary links.

```
hx/type: "reflection-snapshot"
  endpoints: [var:<ns>/<symbol>,
              <claim-or-scholium-id>]
  props: { :reflection/ns, :reflection/symbol, :reflection/file,
           :reflection/line, :reflection/arglists, :reflection/doc,
           :reflection/resolved-at }
```
A timestamped assertion about a var's state. Created when a strategic claim
is first grounded to reflection data. Re-verified periodically — if the var
moves or the signature changes, the snapshot becomes stale and a tension is
generated.

**IF** this could be an entity (like a scholium) rather than a hyperedge
**HOWEVER** it connects two things (the var identity and the claim that
references it) with temporal metadata **THEN** hyperedge is the right
shape **BECAUSE** the snapshot is about the *relationship* between claim
and var at a point in time, not about either endpoint alone.

```
hx/type: "collection"
  endpoints: [<member-id>, <member-id>, ...]   ;; n members
  props: { collection-type: "informed-by|forces|family-resemblance|...",
           label: "patterns that informed M-mission-control",
           owner: mission:<id> | pattern:<id> | ... }
```
A collection groups entities that share a family resemblance or contribute
to a common context. Examples: "patterns that informed this mission" (a list
of pattern refs), "forces in a tension" (the n-ary forces in pattern theory
that create the tension a pattern resolves). Unlike binary relations, the
membership is genuinely n-ary and the set itself is meaningful — you want to
browse "what informed this?" as a group, not chase individual links.

**IF** collections could be modeled as multiple binary relations (mission
→ pattern, mission → pattern, ...) **HOWEVER** the *set* carries meaning
that individual members don't — "these five patterns together informed this
mission" is different from five independent "uses pattern" relations
**THEN** a collection hyperedge preserves the grouping as a first-class
browsable structure **BECAUSE** in pattern theory, forces are n-ary (a
design pattern resolves tensions among *multiple* forces simultaneously),
and "family resemblance" (Wittgenstein) is inherently about the group, not
pairwise similarity.

#### Browser Views

**Tension browser** — new view in `arxana-browser-lab.el` or a companion
module. Reads tension hyperedges, displays as a navigable list grouped by
`tension-type`. Each tension is clickable → expands to show the connected
endpoints (component, mission, evidence). Actions: "propose mission" from
an uncovered component, "view evidence" for a stale prototype.

Data source: tension hyperedges, created by running MC's `compute-coverage`
+ `find-gaps` and converting the output to hyperedges. Can also be generated
by periodic "tension scan" that checks reflection-snapshot staleness.

**IF** we could reuse `arxana-browser-lab.el` directly **HOWEVER** the
evidence timeline is entry-centric (one row per entry) while tensions are
connection-centric (one row per gap) **THEN** the tension browser is a new
view, but it reuses the same infrastructure (tabulated-list-mode, HTTP
fetch, entity display) **BECAUSE** the data shape is different but the
rendering machinery is the same.

**Narrative trail** — extension to evidence timeline. Given a mission ID,
fetch all evidence entries with that mission as subject, follow
`narrative-step` relations to order them, render as a scrollable story.
Each entry shows: timestamp, author, type, and body summary. System events
(PSR/PUR/PAR) are compact; chat turns are expanded. Code blocks link to
source via `about-var` relations.

**IF** the evidence timeline already shows entries **HOWEVER** it shows
them as a flat list sorted by time, not as a per-mission story **THEN**
narrative trail adds mission-scoped filtering and `narrative-step` ordering
**BECAUSE** the story of how a mission was completed is the structure
that new contributors (human or agent) need to understand the system.

**System book** — extension to docbook integration. One "chapter" per repo
(futon0–futon7). Sections are prototypes from devmaps. Annotations are
evidence entries reified as scholia. The book is navigable in Arxana's
existing docbook browser. Content is generated from devmap ingestion +
evidence queries, not hand-authored.

**IF** we could write a static book **HOWEVER** the system changes
constantly — prototypes gain maturity, evidence accumulates, tensions
appear and resolve **THEN** the book is generated on demand from the
hypergraph, not authored as prose **BECAUSE** a static book becomes
stale the moment it's written; a generated book is always current.

### 4. ARGUE

**Synthesis informed by:** `gauntlet/world-is-hypergraph` [🌐/界],
`futon-theory/reverse-morphogenesis` [🔁/←], `math-informal/parametric-
tension-dissolution` [🔀/巽], `f6/self-play-loop` [♻️/遊],
`futon-theory/futonic-logic` [象/香/鹽], devmap P11 (System Self-Description)
[🔃/本].

The futon stack is not a system that needs a separate self-model bolted on
— it IS a hypergraph that agents already inhabit (`world-is-hypergraph`).
Documents, code, missions, evidence, and patterns are all nodes; scholia,
dependencies, coverage relations, and presence traces are all edges. The
task is not to build a representation of the world but to make the world
legible to its own inhabitants. When an agent says "I found the multiplex
function in drawbridge/core.clj," the phenomenology is identical to "I
found the vase on the dresser" (ALFWorld evidence, 2026-02-13). The
hypergraph is the game; Arxana is the browser that makes it playable.

The design above introduces three categories of structure — binary relations
for the common cases (covers, evidences, pattern-use, narrative-step,
about-var), hyperedges for genuinely n-ary connections (tensions, reflection
snapshots, collections), and browser views that render both. This follows
the parametric-tension-dissolution discipline: the tension between "simple
queryable relations" and "rich navigable structure" is dissolved by using
binary relations where the connection is binary and hyperedges where it
isn't, rather than forcing everything into one shape. The tension is
structural, not aesthetic — flattening a tension hyperedge into binary
pairs loses the simultaneous force field that makes it a tension.

Hyperedges are frozen dynamics, not frozen lists. A tension hyperedge
carries its force vectors (what pulls where, what's missing, what
contradicts what) as evaluable structure — Clojure forms inside EDN,
thawable via `eval`, browsable via Arxana. This makes "high curvature"
(accumulated unresolved tension) a computable property of the graph, not a
human judgment. The system can feel where it hurts. This is active inference
applied reflexively: free energy minimization IS tension relaxation, and the
hypergraph IS the nervous system that computes the strain.

The reflexivity loop that P11 describes ("a semantic network that includes a
model of itself") closes here. Following the futon1a pattern — where
invariants are not just enforced but queryable, and README-archivist
documents how to create new ones — the hypergraph should expose its own
structure via API: "what edge types exist?", "what are valid endpoint
configurations?", "what 'clusion does this edge perform?" The self-play
pattern (`f6/self-play-loop`) confirms the architecture: the graph that
agents improve IS the graph they query. When MC runs a portfolio review, it
emits evidence; that evidence becomes a hyperedge; the tension browser
surfaces it; a human or agent proposes a new mission; that mission's
evidence feeds back. The loop writes itself.

The reverse-morphogenesis pattern (`←`) grounds the epistemology: given the
stack's current form (what exists) and its desired form (what devmaps
specify), infer the constraints that would make the current form stable
under development pressure. Those constraints are the tensions. They aren't
bugs to fix — they're the structural information that tells you where the
system wants to go next. The self-representing stack doesn't just show you
what exists; it shows you what the existing form *implies* about what should
come next.

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

## Sensor Grounding: Portfolio Inference Channels

(Added 2026-02-27, conversation between Joe and Claude.)

This mission produces outputs that feed back into portfolio inference's
observation surface (`futon3c/src/futon3c/portfolio/observe.clj`). The
feedback path closes the loop: portfolio inference recommends actions →
self-representing stack makes the portfolio navigable → better observations
→ better recommendations.

### Direct Channel Feeds

| Output | Channel | Mechanism |
|--------|---------|-----------|
| Tension hyperedge count | `:gap-count` | Tension count replaces/augments mc-coverage gap count; more precise than heuristic component matching |
| Tension staleness (avg age of unresolved tensions) | `:review-age` | Stale tensions indicate the self-image is outdated; supplements days-since-last-review |
| Cross-layer navigation completeness | `:coverage-pct` | Browsable paths (devmap → mission → evidence → code) / total possible paths; replaces heuristic substring coverage |
| Narrative trail coverage | `:pattern-reuse` | Completed narrative trails that reuse evidence patterns; currently a placeholder (0.0) |

### Indirect Channel Effects

| Output | Channel | Effect |
|--------|---------|--------|
| Tension browser surfaces blocked missions | `:blocked-ratio` | Human/agent acts on surfaced blocks → unblocks missions |
| Narrative trails make evidence navigable | `:evidence-velocity` | More efficient evidence discovery → higher useful evidence production |
| Self-documenting entry points reduce onboarding friction | `:stall-count` | New agents find their way faster → fewer stalled missions |
| Reflection-grounded claims enable var-level audit | `:dependency-depth` | Accurate dependency tracking via reflection snapshots → better chain computation |

### Future Channel: Tension Density

When VERIFY/INSTANTIATE completes, a new observation channel candidate:

- **`:tension-density`** — ratio of unresolved tensions to total hyperedges
- Feeds urgency: high tension density → pressure toward BUILD mode
- Low tension density → system is relaxed → MAINTAIN mode
- Requires: tension hyperedge count is computable from Arxana XTDB queries
- This would extend the current 15-channel surface to 16

### Curvature as Free Energy

The mission doc (§Homoiconicity) states: "free energy minimization IS
tension relaxation." This is literally true in the portfolio inference
loop: prediction error (PE) across channels IS the free energy. When
tension hyperedges carry evaluable force vectors (frozen dynamics in
props), the tension count and severity can be computed from the hypergraph
and fed directly as observation data — making free energy computation
grounded in navigable structure rather than heuristic approximation.

The self-representing stack doesn't just make the portfolio *visible*;
it makes the portfolio inference *accurate*. Without it, the AIF loop
operates on heuristic sensors (substring matching for coverage, gap count
for spinoff pressure). With it, the sensors are grounded in typed
hyperedges with explicit endpoints — the observation surface becomes the
hypergraph itself.

## Basecamp: VERIFY Preparation

(Added 2026-02-27, Claude + Joe session. Portfolio inference tuning complete,
system transitioned from CONSOLIDATE to BUILD mode.)

### Readiness Status

| Item | Status | Notes |
|------|--------|-------|
| Arxana XTDB backend | Running | futon4 Arxana operational |
| Hyperedge write path | Ready | `arxana-store--post-hyperedge` plumbed, unused |
| Evidence store | Running | futon1a on :7071, queryable |
| MC portfolio review | Running | futon3c on :7070, `build-portfolio-review` |
| Reflection API | Running | `/api/alpha/reflect/var/:ns/:var` live |
| Flexiarg representation | Ready | Arxana already handles library patterns |
| Portfolio AIF loop | Running | BUILD mode, 15-channel surface, all sensors healthy |
| Hyperedge read path | **Missing** | No `fetch-hyperedge` in Emacs |
| Evidence → hyperedge bridge | **Missing** | Core VERIFY work |
| Tag-based evidence query | **Planned** | E-1 below |
| Tension export API | **Planned** | E-2 below |
| Per-mission evidence backfill | **Planned** | E-3 below |

### Phase 0: futon3c-side Enablers

Before the futon4-side VERIFY work begins, three futon3c enablers prepare
the data surface that Arxana will ingest. These exercise the portfolio
inference tuning and give Arxana structured data instead of string-based gaps.

**E-1: Tag-based evidence query (schema-level)**

Move tag filtering from HTTP post-filter into the store layer. Currently tags
are parsed from `?tag=mission,backfill` in the HTTP handler and applied after
`query*` returns all results. After E-1, `EvidenceQuery` includes `:query/tags`
and all backends (Atom, XTDB, HTTP proxy) filter natively.

Files: `shapes.clj` (schema), `backend.clj` (filter logic), `http_backend.clj`
(proxy passthrough), `http.clj` (simplified handler).

**E-2: Tension export endpoint**

`GET /api/alpha/mc/tensions` returns structured tension data pre-shaped for
Arxana hyperedge creation. Each tension is a typed map:

```
{:tension/type       :uncovered-component | :blocked-mission | :structural-invalid
 :tension/devmap     :social-exotype
 :tension/component  :S-dispatch
 :tension/mission    nil                    ;; nil = the tension
 :tension/detected-at "2026-02-27T..."
 :tension/summary    "social-exotype/S-dispatch — no mission"}
```

This replaces MC's current string-based `find-gaps` output with navigable
structure. The Arxana bridge (futon4-side) converts each TensionEntry into
a tension hyperedge with typed endpoints.

Files: `mission_control_backend.clj` (new `build-tension-export`),
`mission_control_shapes.clj` (TensionEntry shape), `http.clj` (route).

**E-3: Backfill endpoint**

`POST /api/alpha/mc/backfill` triggers `backfill-inventory` against the live
evidence store. Creates ~72 per-mission evidence entries tagged
`[:mission :backfill :snapshot]`, one per scanned mission. These entries give
every mission a queryable presence in the evidence landscape — including
M-self-representing-stack itself.

After backfill, Arxana can query `?tag=mission,backfill` (using E-1) to
discover all missions, then create entities and binary relations for them.

### Phase 1: futon4-side VERIFY (after enablers)

With structured tension data and per-mission evidence available via HTTP,
the VERIFY checklist becomes concrete wiring in Emacs Lisp:

1. **Hyperedge round-trip** — call `arxana-store--post-hyperedge` with a
   tension from `/api/alpha/mc/tensions`, retrieve it, confirm structure.
2. **Tension browser** — new view in `arxana-browser-lab.el` that fetches
   `/api/alpha/mc/tensions` and renders as navigable tabulated-list.
3. **Narrative trail** — fetch evidence by `?subject-id=<mission>&tag=backfill`,
   follow `:evidence/in-reply-to` chains, render per-mission story.
4. **Reflection grounding** — for M-mission-control (complete), resolve
   `build-portfolio-review` via `/api/alpha/reflect/var/futon3c.peripheral.mission-control-backend/build-portfolio-review`,
   create `about-var` relation linking claim to source.
5. **Staleness detection** — re-resolve reflection snapshots, surface any
   where `:reflection/resolved-at` is stale as tensions.

### Decision Log

| Decision | Rationale |
|----------|-----------|
| Start from futon3c enablers, not futon4 UI | Exercises portfolio inference tuning; gives Arxana structured data instead of screen-scraping string gaps |
| Tag query at store level, not just HTTP | All backends benefit; HttpBackend proxy to futon1a passes tags through |
| Tension export as separate endpoint | Distinct from `mc-review` (which returns the full portfolio); tensions are the specific data Arxana's hyperedges need |
| Backfill as on-demand endpoint, not startup | Explicit, repeatable, auditable; idempotent via deterministic IDs |
