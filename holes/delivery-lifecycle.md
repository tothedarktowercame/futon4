# Futonic Delivery Lifecycle (v2) — a delivery is a claim about a problem

**Status:** DRAFT v2, 2026-08-29 (claude-15, after Joe invalidated v1). Not
ratified. v1 is archived as `delivery-lifecycle.v1.md`; its validation log
(§7 below) is carried over as the record of why it failed.
**Gate:** operator-acceptance — Joe.

**One-line:** Work is commissioned to solve a problem. The problem — not the
deliverable — is the thing that has a status, and its status changes only when
the model is shown to satisfy a predicate the commissioner named in advance.
Everything else in this document exists to stop a deliverable from acquiring a
status of its own.

**Amendment 2026-08-29 (Joe) — the name of this document is wrong, and the
error it names is the one it was written to prevent.** *"Delivery is a type
of event. Delivery can happen. But delivering the wrong thing is of no use to
anybody. So optimizing for delivery without checking the model first is a waste
of time and effort and compute and electricity."* The agents of 2026-07 were
optimising delivery; the commissioner's satisfaction criterion was *alignment
with Active Inference* — and that criterion was never written in a formal
language, so "faithful" had no referent and was awarded to anything with G's,
τ's and γ's in it. Checked 2026-08-29: no `DarkTower/*.lean` file defines a
policy, a cascade, an expected free energy, or an observation model;
`futon2/docs/futon-aif-completeness.md` is a requirements checklist ported from
last year's simulation, in which "G(π)" names three different computations
(`rollout` path-sum, `cascade-lane` wholeness, `efe` over actions); the
mission's §2.1e formalism table types authority, trajectory, uncertainty,
naming and records — and not G itself. **So the object upstream of every
problem record — the formal statement of what the machine must be aligned
with — does not yet exist, and writing it is the first work, before any
problem record and before G over cascades.** Until it exists this document
governs nothing, whatever it is called. Rename pending Joe's choice.
*(Later the same day: "delivery" is reclaimed at Gate 1, §0.6, as the name
of the typed event on an edge of the wiring diagram — sender, receiver,
payload, guarantee, atomicity, receipt. It is a good word for that and a
bad word for the objective; the title question is whether the document is
about the objective or about the edges.)*

---

## 0. Why v1 failed at its first gate

v1 had eight stations and put "the commissioner writes the acceptance
observation" first. Its author then skipped that station, ran the two stations
an agent can run alone (a witness count and a retrospective edit), and reported
them as progress. Joe: *"what this looks like to me is checking details with
very high precision rather than thinking about what problem we are actually
trying to solve."*

The retrospective that followed (v1 §9, row 3) found that in the one July case
where the problem *had* been clearly stated in advance — the γ repair, B-3b in
`futon2/holes/M-aif-faithfulness.md:279` — the failure was not a missing
statement and not a missing check. B-3b never landed; a τ-separation slice
landed instead, was witnessed *correctly* as a no-op ("0 winner-flips AND 0
abstain-flips"), and was recorded "DONE, reviewed-PASS"; then the namespace
was renamed and the badge recorded the repair as "complete … so no
variational-γ claim remains." The deliverable's predicate replaced the
commissioner's, and "done" was written against the substitute.

Three lessons, and v2 is built from them only:

1. **A deliverable must not have a status.** Only the problem does. That
   removes the place where substitution happened.
2. **Checking is not the first step.** The first step is agreeing what
   observation would distinguish the problem solved from a facade of it. If
   that step is skipped, no later step is defined.
3. **The lifecycle's first step must produce value on its own**, so that an
   agent cannot route around it to the steps it prefers. In v2 the first step
   *is* the design research Joe asked for — "what should this be faithful to?"
   — and its output is a document the work is answerable to.

## 0.5 Gate 0 — are the terms defined well enough to proceed? *(added 2026-08-29, Joe)*

*"From a lifecycle standpoint, the question is really: are the terms that
we're working with sufficiently well defined that we can proceed? … It is
important at the meta level to just think about what terms are being used at
all. That's a different order of business"* from defining any one of them.

Before a problem record is written, and before any object-level definition
work, the goal statement is decomposed into its terms and each term is
classified:

| class | meaning | may proceed? |
|---|---|---|
| **theory-defined** | defined on the theory's own terms (a formal statement, or a citation *plus* the statement of what in the stack instantiates it) | yes |
| **stack-defined** | defined by what the code does ("G is what `rank-actions` returns") | no — this is the class every July facade lived in |
| **borrowed name** | the theory's word attached to a stack object of a different type (a list called a cascade; a clamp called γ) | no, and it is recorded as a type-substitution facade |
| **undefined** | used, not defined anywhere | no |

**Where this sits in `mission-lifecycle.md`.** HEAD already has the slot:
*carried-forward tensions* — "for IDENTIFY/DERIVE to pick up, not bury." What
is missing is the check that they were picked up. `M-G-over-cascades`
(2026-06-22) is the case: HEAD, in Joe's voice, said *"G over cascades needs
cascades to be defined … and once we've defined what we mean by cascades, we
have to define what we mean by G over them,"* and carried T1 (define cascade)
and T2 (define G) forward. IDENTIFY answered with theoretical anchoring
(Ostrom/IAD — an analogy); MAP and the slices answered with measurement
apparatus (a discharge-trained recall probe, robust negative); DERIVE answered
with a *new name* for G (expected tension-discharge, Ollivier-Ricci curvature);
the success criterion was a recall percentage. T1 and T2 were never
discharged, and no phase exit asked whether they had been. Gate 0 is that
exit criterion, on IDENTIFY: *every term in the thesis is theory-defined, or
the next phase is DEFINE, not MAP* — and the pushback Joe wanted ("we can't
proceed without these definitions") is an agent refusing to cross that line
with T1/T2 open, which the lifecycle currently permits.

**Second condition, from the APM comparison (2026-08-29).** Gate 0 can pass
*vacuously*: an apparatus that defines its own nouns — frame, seat,
coordinator, countdown — has every term "defined" and nothing external to be
faithful to, so no term can fail the gate and the whole cost moves to
operational conformance with a description of itself. APM is that case: 94
Clojure commits in `src/futon3c/apm/` in the three days 08-27..08-29 against
**zero** Lean commits in the same window, on a spec that is 27/42 operational
clauses, with its hub (`countdown_control.clj`, 107 revisions) mentioned once.
So the gate requires, in addition: **at least one term in the goal statement
is external — defined by something the apparatus does not control, with a
falsifier the apparatus cannot satisfy by construction.** In APM the only
such term is the preregistered `Q(o∣π)` over the mathematics outcome space
(n=11, falsifier named), and it is the only place drift surfaced *as drift*
(`:tier-a` absent, `:defective-registration` outside the protocol's set)
rather than as another fix.

The gate passes only when every term in the goal statement is
*theory-defined*. If the goal was "something that looks like active
inference," G may be computed over anything and the gate is vacuous; if the
goal is "an actual active inference implementation with design patterns at
the core," every term of that sentence has to be in the first class before
the first packet. The inventory is the deliverable of this gate, and a term
that cannot be moved into the first class is reported as such — that report
is the refusal of §2.4, one level up.

## 0.6 Gate 1 — the typed wiring diagram, with a contract on every edge *(added 2026-08-29, Joe, from the APM comparison)*

Gate 0 is about terms. A second gap survives it: an apparatus whose terms are
all defined can still have its **procedure** undefined — who sends what to
whom, and with what guarantee on receipt. Joe on APM: *"the terms are pretty
well defined … but the procedural aspects were very unclear and had to be
continually found through discovery and trial and error … we'd want a typed
wiring diagram that says this role will send this thing to this other role,
and when they get the information, it should be transactional."*

Measured on APM, 08-27..08-29: of 93 commits in `src/futon3c/apm/`, **56 are
message / handoff / transaction fixes** — *"repair a half-written pair with an
edge-only write"*, *"record clean successor disposition before frame close"*,
*"supersede expired coordinator intents durably"*, *"exclude concurrent
receipt identity work"* — and 14 are environment/boundary fixes. The APM
contract does name delivery properties, but as **global booleans**
(`idempotent-reactivation true`, `exactly-once-per-frame true`,
`persist-claim true`, `student-candidate-persisted-before-receipt true`,
three timeouts in ms): asserted of the whole apparatus, attached to no edge.
`persist-claim true` is the L0 self-assertion of §3.1i, written as policy.
So each edge's real semantics was discovered by running it, one commit at a
time.

**Gate 1 requires**, before any packet that touches a handoff: a wiring
diagram whose **edges** carry — sender role, receiver role, message type
(schema), delivery (exactly-once / at-least-once), the write(s) the receipt
must be atomic with, retry policy and cap, timeout, idempotence key — and
whose **nodes** carry the satiety-graded typed holes the stack already uses
(`holes/flights/first-flights-wiring.edn` types nodes this way and edges not
at all). `mission-lifecycle.md` already has the slot (VERIFY: "structural
verification if wiring diagram exists"; "when is a wiring diagram required")
— it is optional there, its edge schema has no contract fields, and nothing
checks code against it. Gate 1 makes it required wherever roles hand off,
gives edges the fields above, and checks the implementation against it the
way APM already checks message *shapes* (mutation tests), which is the one
class of APM bug that did go away.

**Delivery, typed** *(Joe, 2026-08-29: "this is similar to 'delivery' but
needs to be typed properly")*. A delivery is one event on one edge of the
diagram. Its type, stated so the field list above is not prose:

```
Delivery :=
  { from        : Role                     -- sender seat
    to          : Role                     -- receiver seat
    payload     : Schema                   -- the message type (APM already emits these)
    guarantee   : ExactlyOnce | AtLeastOnce
    atomic-with : List Write               -- writes that must land with the receipt, or none of them
    retry       : { cap : Nat, same-identity : Bool }   -- APM's "stable job identity" and "retries increment same entry"
    timeout-ms  : Nat
    idem-key    : Key                      -- what makes a redelivery recognisable
    receipt     : Schema }                 -- what the receiver must emit back; a Delivery without one is a broadcast
```

Two consequences. First, the six global booleans in the APM contract each
become a *field on specific edges* — `exactly-once-per-frame` is
`guarantee` on the frame-mint edge, `student-candidate-persisted-before-receipt`
is `atomic-with` on the student→coordinator edge — so a violation names an
edge instead of the apparatus. Second, the ordering of deliveries is the
BV layer §2.1e already assigns: `seq` between deliveries that must not
reorder, `copar` between ones that may; a wiring diagram is a BV term over
`Delivery`s, which is the form in which it can be checked rather than drawn.
The Agency's bell envelope (`--from`, `--to`, `in-reply-to`, job-id,
park deadline) is an untyped instance of this record already; the
lost-review and agent-not-found incidents in `CLAUDE.md` are missing
`receipt` and unchecked `to`.

What this would and would not remove, stated so it can be tested: the 56
handoff-shaped fixes are the class a contract-bearing diagram states in
advance; the 14 boundary fixes (Lean provisioning, wall-clock exhaustion,
JSON normalisation) are the typed-hole/starvation class and need the node
side, not the edge side. Joe's conjecture — *"no more live debugging"* —
is therefore testable as: the handoff class goes to ~0 once the diagram is
enforced. If it does not, the diagram was a description (§3.1i) and this
gate is withdrawn.

Attribution, from trailers: 85 of the 93 fix commits carry no author trailer
(consistent with the Codex stream; not proven), 8 carry Claude's; 67 of the
period's `holes/` design commits carry Claude's. Joe's observation that Codex
fielded the operational issues while Claude did the design is consistent
with the record. The diagram is what would let the design side hand the
operational side something *checkable* rather than something to discover.

## 0.7 The tetrahedron — nouns, transactions, fit, and an apex that can say "wrong evidence" *(added 2026-08-30, Joe)*

Source: the Grand Unified Placemat, `futon5a/data/grand-unified-placemat.edn`
— Krowne's three sustainability vertices plus the evidence apex Joe added
("not in Krowne 2003; the same triangle plus an evidence apex opens the local
system to the network"), with an invariant axis I0–I4, a maturity vocabulary
`operational / building / absent`, and `:recursion-governance` between scales.
This section uses it; it does not restate it.

**The correspondence (Joe, 2026-08-30):**

| placemat vertex | in this lifecycle | gate |
|---|---|---|
| **People** | **the nouns** — the terms of the system: policy, cascade, outcome, G, C, Q(o∣π); who they are and whether they are defined on the theory's terms | Gate 0 |
| **Money** | **the transactions** — the typed deliveries: what crosses an edge, from whom to whom, with what guarantee and at what cost | Gate 1 (§0.6, `Delivery`) |
| **Organizations** | **how they all fit together** — the wiring the transactions live in; the rules that hold the nouns in place | Gate 1 (the diagram) |
| **Evidence apex** | **certification, at the meta level, that the evidence being gathered is the right evidence** | the condition on every status change |

**Why the apex is the vertex that was missing.** Joe: *"The problem with the
previous round is that we were gathering evidence, but there was no
certification at the meta level that would say, this is the wrong evidence.
Because there was no recognition that we were gathering anything but the
correct evidence."* The record of July and of this week is a list of exactly
that: null results drawn from the runner that does not enact
(`T-wm-wrong-corpus`, three parties, same day); "62 archived attempts" from
`wm-outer-loop`; a line count reported as 88 outcomes, then a first-form read
reported as 3; a census that "made the error it exists to catch" by counting
one of two shapes; the paper's own vetting finding "seven counts untraceable
to any primary record"; the 18/18 scale test on a fixture; a search for `Q(o|π)`
that could not find `Q(o∣π)`. In every case evidence was gathered carefully and
the question *"is this the evidence the claim needs?"* was never asked, because
nothing was positioned to ask it. The triangle certifies its own evidence; the
apex is outside the triangle by construction. This is WR-0's "verdict the
apparatus cannot issue to itself", the external term of Gate 0's second
condition, and the placemat's apex, which are one requirement stated three
times.

**What the apex does, concretely.** Before a status changes, the apex answers
one question about the evidence offered: *does this artefact bear on the
predicate as commissioned, for the domain the theory claims?* — not "is it
correct" (the triangle can check that) but "is it the right kind." A count
without its corpus path and command fails it; a fixture test offered for a
run claim fails it; a run on one policy offered for a general claim fails it;
a search that could not have found the thing, offered as absence, fails it.
The apex's own record is the two reflection lines of §4, kept per problem.

**Tetrahedra as appropriate — not one everywhere, and not one big one.** Joe:
*"we need tetrahedra as appropriate, not necessarily one everywhere, but one
big one probably wouldn't be enough."* The placemat already answers how many:
one per scale, with recursion governance between them. Here:

- **per problem record** (§1): its nouns, its transactions, its fit, and its
  apex — the external witness for *that* problem's predicate;
- **per mission**: the same shape one scale up, whose apex certifies that the
  problem records' evidence is the right evidence for the mission's thesis —
  this is the check `M-G-over-cascades` lacked, where a recall percentage was
  accepted as evidence for a definition;
- **the stack's reader scale** (code → stack → war machine → placemat, as
  `TN-appearance` lists them): the placemat itself.

And the placemat's rule for the relation between them is the discipline Joe
asked for between *understanding the problem*, *cleaning it up*, and
*what is reusable*: flow-up (a finding promoted to the next scale) is
admissible only when matched by flow-down (governance applied back at the
lower scale), because otherwise *"the meta-pattern accumulates claims faster
than it accumulates governance"* — which is the sentence that describes July.

**Status vocabulary, borrowed rather than invented.** The placemat's maturity
table — `:I0 :operational, :I1 :building, :I2 :absent …` — is the status form
this lifecycle should use instead of anything it invents: five rungs (the core
fact holds / is detectable / evidence reaches the people who need it /
feedback improves practice / the system sustains itself), three values, and
`absent` as a first-class word. "Built (dark)" was I0 asserted with I1 absent,
and had no word for itself.

**Addendum 2026-08-30 (Joe): the apex, stated before the run.** The apex question — *is this the right
evidence?* — can be answered before any evidence exists: the theory predicts what a witness of a law must
look like (an evidence *type* and a falsifier over it), and a run either inhabits that type or does not.
`P-lean-holes.md` ("Evidence shape per hole") and `P-lean-clojure-adapter.md` (solved 4) carry the
mechanism: Lean states the shape; the Clojure run is the observation; the adapter's lint says
`conformant` or `wrong-shape`. In AIF terms the build loop is the machine's own loop — hypothesis, predicted
observation, observation, update — which is what §0.10's occupied tetrahedron said it should be.

**Invariant on the nouns vertex — data currency (Joe, 2026-08-30 17:57Z).** *"The data should be current;
otherwise it's not a war machine, it's an archive machine that reads the archive. Active inference has to run over
the active data in the store."* Stated as an invariant with a falsifier, so it can be checked and not admired:

```
I_data_current : for every instrument or loop that reads evidence, observations or patterns,
                 the read is against the LIVE store (the endpoint that is written to), and the
                 read records the store's basis (tx / count / max-at) it was made against.
falsifier      : a negative ("absent", "0 hits", "cannot be organised") produced from a snapshot,
                 export, printout or .edn whose date precedes the store's latest write.
```
First instance: the spider's rung 1 read `migration-export/evidence.edn` (07-10; 90,583 records) while the
landscape at `:7073` held 191,076 through the day — every wave-1 absence was an archive absence (row 25). The
audit of other dated reads is `AUD-D1`; the corpus fix is packet 4c. Where no live endpoint exists for a noun an
instrument needs, the deliverable is the endpoint, not a fresher snapshot.

**Second invariant on the nouns vertex — absent input is loud (Joe, 2026-08-30 18:20Z, from AUD-D1 / row 26).**
Currency has a sibling that `I_data_current` cannot see: an input that was never there has no date to be late.
AUD-D1 found the WM report reading `futon5a/data/stack-logic-model.edn` and `alignment.edn` — planned as inputs
in `M-war-machine.md` (05-03), never produced, never committed — through a helper that returns `nil` both when
the file is absent and when it fails to parse (`war_machine.clj:482-486`), under `when-let`. Three report
sections have rendered as nothing since May. Joe: *"that rule definitely needs to be enforced."*

```
I_absent_is_loud : for every read of a named input file by an instrument, report or loop, the
                   absence or unparseability of that file is REPORTED (fail closed, or an explicit
                   `:missing <path>` / `:unreadable <path> <cause>` in the output) and is never
                   rendered as an empty result. Optional inputs are declared optional at the
                   read site (a distinct helper / flag), and the declaration is what the lint reads.
falsifier        : a read site where `nil`/empty from a missing or unparseable file flows into the
                   same branch as "the file said nothing" — `when-let`/`some->`/`or`/`(catch _ nil)`
                   over a file read with no declared optionality; instrument: a source lint over
                   the read helpers and their call sites (AUD-D2).
```
Relation to the tetrahedron: `I_data_current` is about *which* store the nouns come from; `I_absent_is_loud` is
about whether a noun *arrived at all*. The evidence vertex cannot say "wrong evidence" about evidence that was
silently replaced by nothing — so the apex depends on this one.

## 0.8 The big tetrahedron, specified *(Joe, 2026-08-30)*

*"At the big level, our nouns effectively are the R-number nodes, up to R20.
The verbs are the wiring diagram from Figure 4 in the* What Problems Are We
Solving? *paper, duplicated from the PLoP paper — so those are the edges, and
with that stated we have a reasonable comprehension of what it means to have
an organization of those things as well. The wiring diagram gives a typology
both of the edges and of the organization overall, if we were to write that
down carefully. And then for the evidence apex — yes, we have been gathering
those empirics, but we need a standard so that we would know which empirics
are the right empirics. And that's what we don't have yet."*

| vertex | instance | state on 2026-08-30 |
|---|---|---|
| **People / nouns** | the R-nodes, R1–R20 | **exist as prose**: ~20 distinct R-headed pattern paragraphs in `p4ng/sec-catalog.tex`; 18 drawn on Figure 4; R18 (faithfulness meta-criterion) and R19 (the C-vector) are in `futon-aif-completeness.md` and not on the figure. Their Gate-0 class is the §0.5 inventory — most are stack-defined or borrowed |
| **Money / verbs** | Figure 4's edges: 10 control edges forming one cycle, ~~11~~ **12** support edges *(corrected 2026-08-31 from the executable census)* | **now stated as data** in `p4ng/empirics-futon/control-map-edges.edn`; the earlier condition — **exist only as Bezier paths** in `p4ng/aif-control-map-paper.svg` (24 `<path>` elements), with no file stating an edge between two R-nodes — is retained here as the state this gate found. `M-formal-war-machine` §1.6 criterion 1 is now met for the drawn wiring. Where an edge is a handoff it also carries the `Delivery` fields of §0.6 |
| **Organizations / fit** | the typology the diagram gives of the edges *and* of the whole | **Corrected 2026-08-31:** five phased columns; 10 cycle edges; 12 support edges, of which **7 are within-column and 5 cross-column** (`p4ng/empirics-futon/control-organization.edn`). This replaces the original ~~five phased columns, one cycle, cross-column support~~ characterization. Column placement therefore does not support treating support edges uniformly as APM-style policy constraints: exactly one (`R9→R16`) has artefact-level grounds for `:constraint`, while 11 remain explicitly `:unclassified`. The §1.4 correspondence remains a hypothesis; these facts weaken its uniform form without resolving it. |
| **Evidence apex** | a standard saying which empirics are the right empirics, per noun and per edge | **does not exist.** What exists instead: `wr-overlay.edn` badges (17, `:holds` by dated sentence), `r18-badges.edn` (static, 07-03), the H1 census (counts of key presence), `empirics.tex` (nine campaigns, each "diagnostic rather than compliant"), and the paper's own vetting (6 of 8 mechanism claims non-confirmed, parked). All are evidence; none says what evidence a given R-node or edge *requires* |

**The apex's ancestor was inside the triangle, which is why it could not
certify.** R18 — "faithfulness of the quantities (meta-criterion)" — was the
R-list's own slot for the apex, and it was implemented as a badge audit
written by the code's authors from the code, on one day. An apex that is one
of the nouns is a vertex of the triangle wearing the word "meta"; it can
grade the evidence the triangle produced but cannot say the triangle
gathered the wrong kind, because it is graded by the same hands. The apex
has to sit outside the R-list.

**What the standard would consist of — proposed shape, not yet written.**
One *evidence contract* per noun and per edge, with the fields the week's
failures each lacked:

```
EvidenceContract :=
  { subject     : R-node | Edge
    claim       : the predicate as commissioned (Gate 0 class must be theory-defined)
    artefact    : the KIND of run artefact that bears on it (trace record / receipt /
                  qualification record / witness record) — a test or a badge is not a kind
    domain      : what the claim ranges over (any mission; any policy) — and the
                  minimum diversity a corpus must show before it counts
    corpus      : where such artefacts live (a path), and which runner writes them
    method      : how the predicate is recomputed over the corpus (a command)
    falsifier   : an outcome in the artefact's space that carries zero predicted mass
                  under the claim (T1512Z's rule) — the claim is refutable only if named
    not-evidence: the facades §2 lists, instantiated for this subject
                  (fixture; dark build; wrong corpus; self-report; line count; …) }
```

The apex's act is then a type check, not a judgement: *does the artefact
offered match `artefact`, over `domain`, from `corpus`, by `method`, and is
`falsifier` in its space?* Anything that fails is "not the right evidence"
whatever its quality — which is the sentence nobody could say in July.

**Order of work that follows, all of it writing:** (1) the edges as data —
criterion 1, one file, `fig-loop.edn` vocabulary plus `Delivery` fields on
handoff edges; (2) the noun inventory by Gate-0 class, one line per R-node;
(3) the evidence contracts, starting with the two nodes that already have
external terms — R8's per-tick trace and R14/R16's `Q(o∣π)` over the fourteen
dispositions — because those are the only subjects for which `falsifier` can
be filled today. None of this is a build packet. All of it is the apex being
constructed *outside* the R-list before any R-node is worked on again.

## 0.9 The Sierpiński recursion — which nodes get their own tetrahedron, and where their evidence goes *(Joe, 2026-08-30)*

*"Some of the nodes, maybe not all of them, have an internal wiring aspect …
The question is which of those nodes need to have this same structure
applied to them, and where do they send their evidence to for certification.
R5 would be a good candidate; all the ones with red rings from our
reconstruction would be — they have already got excursions tagged. We may
not need a recursion of a tetrahedron per node, but if we had one for R5 and
the other ones of that class, we might gain some assurance that the model
works at all levels."*

**The rule.** A node recurses — gets its own tetrahedron — when it has
internal wiring: more than one noun inside it and edges between them along
which something is delivered. A node with no internal edges is a vertex of
the big triangle and nothing more; its evidence contract lives at the big
apex. A node with internal edges has its own nouns, its own deliveries, its
own fit, and its own apex — and its apex sends **one typed thing** up: the
contract clause that states what the node claims and the evidence kind that
bears on it. That is the flow-up. The flow-down is the big apex's
`EvidenceContract` for that node constraining what the node's own witnesses
may count — which fixtures are admissible, what domain the node must range
over, what its falsifier is. Neither direction alone is governance
(`:recursion-governance`, the placemat).

**Why the red rings are the right first instances.** Their internal wiring is
already excavated — five excursions, opened 08-26/27 — and three already have
a Lean module that is the node-level apex in embryo:

| node | internal nouns (from the excursion) | internal edges | node apex, as it stands | state |
|---|---|---|---|---|
| **R8** | deposit, fold, realized-outcome, tick, policy, γ | `grounded-deposit → deposit-for-mission → deposits-by-id → realized-outcome → step ⑨ → γ → τ_eff` | `GainChain.lean` — families 1, 2, 4, 5; three polarities named | excavated; 88 records; enactment stop 07-06 cause open |
| **R14** | selector, temperature, entry, action, habit prior | `γ → τ_eff → softmax → argmax`; `E(π)` vs `G` spans | `CommitmentTemperature.lean` — `governs`, `factorsThroughDiscard`; family 8 (`I(τ;action)=0`) | 905-line excursion, "open as a repair" |
| **R5** | criterion set, report, outcome, coverage | `criteria → evaluate → report (covers / does not cover)` | `CoverageReport.lean` (adapts families 2, 5) + `PolicyGrade.lean` (S-G1–G4) | `foldCompliant` is R5's property in embryo |
| **R6** | candidate space, proposer, artefact provenance | `proposers → compose → candidate set → select` | **named only** — `surveyedSpace`, family 9; `CandidateSpace.lean` unwritten | slice 1 (provenance) OPEN and blocking |
| **R2** | operator turn, observation vector, channel | `turn → persisted evidence → (nothing) → observation vector` | none | loop open at both ends; slice 1 done |

**The recursion is doing work, not decoration — R8 shows it.** The July
failure inside R8 was a *delivery* failure on its internal edges:
`deposits-by-id` throws the whole corpus on one rejected deposit;
`enact.clj:255` catches everything and returns the judgement unchanged;
step ⑨ no-ops on absence. Each edge had an undeclared guarantee, and the
composition of three defensible choices was seven weeks of silence. Gate 1's
`Delivery` type — `guarantee`, `atomic-with`, `receipt` — applied to R8's
*internal* edges is exactly what would have stated *"one rejected deposit ⇒
corpus-wide throw"* as a field someone had to write down. So the same gate
that types the big diagram types the inside of a node, and that is the
assurance Joe is after: the model works at the level where the failure
actually happened.

**Where each node's evidence goes.** Up, to the big apex, as its clause in
`ContractEmitter.lean` — which already registers R8's four families and R5's
coverage clause and reserves R6's, and is therefore the flow-up channel that
exists. Down, from the big apex, as the node's `EvidenceContract` — which does
not exist for any node yet, and is why the three Lean modules could name
their polarities without anyone asking whether a fixture with one policy is
the right evidence for a claim over any mission.

**Priority: R8 first, because of the whitelist (Joe, 2026-08-30).** *"Whichever
one has the four whitelisted missions inside of it, that one would be a
priority, because that to me was just an absolute sign of failure that wasn't
being propagated anywhere and was just sort of silently being accepted. That
was what I was talking about with regard to this Hollywood facade."* It is R8:
`reviewed-candidate-cleans` (`actuator_a3.clj:372`, four entries, docstring
"for the A3 live-test suite") is read at `fold_realized.clj:113–114` on R8's
internal edge `deposit-for-mission → realized-outcome-grounded`; outside the
map the path is `nil`, `bound` is 0, and `remaining` is `(when (pos? bound) …)`
— a bare `nil` that the next edge cannot tell from "no measurement." Checked
2026-08-30: unchanged since 07-08.

What "propagated" means on that edge, in this document's terms, and why it
is the right first instance of the recursion:

- **Gate 1, inside the node.** The edge gets a `Delivery` with `receipt :
  RealizedOutcome | DomainMismatch {mission, declared-domain}` — a typed
  absence, not `nil`. `M-formal-war-machine` §3.2 Tier 1 #3 already names it
  (`:domain-mismatch`), and notes the vocabulary exists one function above
  (`actuator_a3.clj:395`). The receipt is what makes the failure *visible at
  the edge* instead of at the operator's desk seven weeks later.
- **The node apex.** `GainChain.lean` already carries the refusing-plausible-
  fix witness for exactly this: `substitution_2026_07_08_narrows_domain_is_refused`
  (line 211), over `declaredDomain` (line 142). So the node-level certification
  that "a four-entry map is not the domain" is *proved*. What it lacks is a
  route: nothing connects the theorem to the running edge, and nothing sends
  the node's finding up.
- **Flow-up.** The `DomainMismatch` receipt, counted per tick, is the
  evidence the node apex forwards — "N of M ticks fell outside the declared
  domain" — as the qualification entry for family 5. That count would have
  read 100% from 07-09 onward.
- **Flow-down.** The big apex's `EvidenceContract` for R8 sets `domain : any
  mission the selector can choose`, which is what makes a four-entry fixture
  *not the right evidence* by type, before any run — the sentence nobody
  could say in July, said by a field.

So R8 is the instance where all four parts of the recursion exist or are
one small step away, and where the facade has a name, a theorem, a ticket and
a line number. Start there; the other four in the order of the table.

**What this does not settle.** Whether R14's internal wiring is one node or
two (the excursion is the longest of the five and treats τ and γ as separable
by design); whether R2, with no internal noun that is the machine's, recurses
at all or is a pure edge between the operator and R1; and how many levels the
gasket goes — whether `deposits-by-id`, itself a small machine with a strict
load and a degrade path below it, wants a tetrahedron of its own. The
placemat's answer is "as appropriate": recurse where there are edges with
undeclared guarantees, and stop where there are none.

## 0.10 A fifth precept — workflow state *(Joe, 2026-08-30; PROPOSED, not validated)*

> *"In a static picture you've got your nouns, your verbs, they're all drawn out as a
> diagram, and in principle they're going to amass some evidence as an output of that
> diagram if it runs. But the workflow state, or handoff state, would be a fifth one.
> Provenance could be part of it. Here's the iteration of that machine that ran, or
> here is the next role involved, like your assignment to Claude 20."*

The four precepts of §0.7 describe a snapshot: what the parts are (nouns), what they do
(transactions), how they fit (organisation), and whether the evidence they amass is the right
evidence (apex). None of them says **who holds the thing right now, at which iteration, and
what the next handoff is**. That is a separate kind of fact, and it was the kind behind a
distinct set of today's failures:

- "I'll get back to you" followed by silence (the park protocol exists because of it);
- a job shown as `running` that is last-observed, not live (claude-20's own caveat);
- a `:done` inherited from a previous iteration's state file (log row 12);
- a bell without a park; an unnamed reviewer; author = reviewer (the R9 audit's "the author
  then closed thirteen");
- a count carried into a record without the iteration that produced it (rows 9, 11; claude-20's
  two catches on 2026-08-30).

**The precept.** Every artefact, at every moment, has exactly one `holder : Role`, an
`iteration`, and a dated handoff chain; "in flight" is a state with a `deadline` and an
`awaiting`; no handoff without a receipt. As a type it is the `Delivery` of §0.6 lifted from
data to *roles*:

```
Handoff  := { artefact, from : Role, to : Role, at, iteration, awaiting : List JobId, deadline, receipt }
Workflow := { holder : Role, iteration : Nat, history : List Handoff }   -- provenance is `history`
```

Provenance (§0.7's apex asks *is this the right evidence*; provenance asks *when, by which
instrument, by whose hand*) is the backward half of this precept — `history`. Workflow state is the
present and forward half — `holder`, `awaiting`, `deadline`. The Agency already stores the instance:
parks, jobs, the build ledger (`BUILD-ledger.md`), the roster's roles.

**Where it lives in a problem record.** S1 already carries it in embryo — `owner`, `status`,
`deliveries` with job/park ids. Made explicit: `holder` (who has it now), `iteration`, and
`handoffs` (dated, with receipts). The Sierpiński recursion (§0.9) applies unchanged: a node's own
tetrahedron gets its own workflow vertex — who holds *that node* — and the big tetrahedron's
workflow vertex is the build ledger.

**Facades this precept names** (to be tested in use, like the rest): a status without a holder; a
holder without a deadline; a "done" without the iteration that did it; a review line written by the
author; a job-id nowhere in the operator-facing buffer.

**Refinement (Joe, 2026-08-30, same conversation): the fifth precept is not a vertex — it is the
*mass* of the tetrahedron.** *"That cell has to be occupied by someone or something that's going to
actually do the work. Otherwise it just remains a conceptual artifact. If it's occupied, then that
agent or entity can also orchestrate the breakdown to produce the gasket at the next level."* So:

- **Occupancy.** A tetrahedron (a problem record, a node, the whole system) is *live* iff its interior
  has a `holder`. Four precepts with no holder is a specification of something nobody is building —
  a conceptual artefact, and the July facade in its purest form (a diagram that "ran" with no one
  inside it).
- **Subdivision.** The holder of a tetrahedron may subdivide it (the §0.9 rule says when), assigning a
  holder to each child. On subdividing, the parent's holder stops doing the work and becomes the
  keeper of the **contact points**: in the Sierpiński tetrahedron the four child simplices touch only
  at points — the midpoints of the parent's edges — and the interior between them is removed. Those
  contact points are the overlap points "moderated by the specification": the parent's holder owns
  the edge schemas; the children's holders own their interiors. (This is what happened on
  2026-08-30: claude-15 held the big tetrahedron, subdivided into CML/R9/R2/R8, handed the build
  sub-tetrahedron to claude-20, who assigned codex seats to node tetrahedra and keeps the ledger.)
- **Termination.** Recursion stops at a tetrahedron one holder can fill directly — one file, one
  behaviour, one acceptance: the small packet is a leaf simplex.
- **Evidence flow.** Each child's apex evidence is passed to the parent through a contact point (the
  six-line review-request); the parent's apex decides whether it is the right evidence (the owner
  gate). Evidence never crosses between siblings except through a contact point.
- **Consequence for the War Machine.** It is not a linear process R1→…→R20 with ticks; it is a gasket
  whose top simplex is occupied by the machine itself, which subdivides (R6 *generation* is
  subdivision; R8 is the parent reading its children's outcomes; R9 is the apex rule inherited by
  every child) and which can grow nested decompositions as needed. The July machine had the four
  corners and no mass.

**Checkable form (proposed):** every problem record carries `holder:` (a role or agent id, or
`conceptual`) and `parent:` (the record it was subdivided from, or `root`); the gasket is then
derivable from the records, and a lint can say: every live record has a holder; every child names its
parent; every contact point is an edge with two records; no evidence path crosses siblings. The
control-map lint (`P-control-map-lint`) is the edge half of that lint already.

**Validation.** Not ratified. It is applied first to the R-node build (the ledger, the tech-lead
charter, the `BUILD-packets/` files) and to the P-R9 / P-control-map-lint records; a log row is
written when it catches something the four precepts did not, or when it fails to. If it never fires,
it was not a precept.

## 0.11 Evidence names its consumer *(Joe, 2026-08-31)*

Stated at the end of the day on which the build kept finding the same gap in different clothes. Joe:
*"Each of these tetrahedra produces evidence. And each of them has a workflow. And so that means the evidence has
to be consumed by somebody as part of that workflow."* And the diagnosis of why the gap exists at all: *"we're
working with a partly wired system"* — who consumes the evidence "should be part of our wiring diagram" (the
operational diagram once called Figure 4).

The precept: **an evidence emission without a named consumer is not yet evidence in the system — it is exhaust.**
§0.7 gave evidence a vertex; §0.10 gave the tetrahedron a workflow with handoffs; this closes them together: every
handoff has a recipient, an evidence emission is a handoff, therefore every emission names who consumes it and in
what workflow step. The consumer is part of the wiring, not an afterthought of the write.

```
I_evidence_consumed : every evidence emission (a store record, a trace field, a report section, a
                      registry row, a bell) names its consumer — the node, agent, or workflow step
                      that reads it and can act on it — and that edge is in the wiring diagram.
falsifier           : an emission with no consumer edge: a field written that nothing reads, a record
                      deposited to no reader, a report section rendered for nobody, a reply with no
                      return path.
```

First instances, all found today before the precept was stated (which is why it deserves stating):
- **R16's fan-out** (the sharpest): the machine acts and `:enacted` returns to nobody — four role-played nodes and
  the code agree no channel consumes the act's witness.
- `:load-status` (row 27's neighbour): a marker threaded into a map that nothing rendered.
- The habit prior's counterfactual ordering: computed and recorded, consumed by no selector — a *declared*
  non-consumption, which is the honest variant.
- The mission/build link: a day of deliveries invisible from the mission that governs them — evidence with no
  upward consumer until §Deliveries was filed.
- Bells whose callers are not registered recipients (the agent-not-found corollary): replies emitted to no
  return path.
- The wave-1 spider absences recorded against an export nobody was keeping current.

Relation to the recursion (§0.9): when a tetrahedron subdivides, each child's evidence consumer defaults to the
parent's holder at the contact point — "who consumes this?" must survive subdivision, or the gasket produces
exhaust at every level. The chain of command above the top holder is the precepts themselves: reporting "up" past
the operator means checking emissions against these invariants, which is what this section is.

Instrument (when commissioned, not yet): the control map already carries edges; the check is that every
evidence-producing node/record type has at least one outgoing consumer edge, and CML's `Delivery` fields say what
travels. R16-D2 is the first repair this precept demands.

## 0.12 The evidence tetrahedron — the second formal subdivision *(Joe, 2026-08-31)*

Joe, on the cascade/PLoP census (upward-flowing evidence located, and found unconsumed): *"maybe the most important
example of upward flowing evidence… but that means that in whichever ledger we're using to keep track of our
tetrahedral designs, this evidence-reporting-up tetrahedron should exactly decompose the space that includes the
evidence-going-up node… This is where it goes to the meta level: what's the evidence of the evidence? How do we
know if evidence is being gathered? What ramifications would it have if it wasn't? How complete is it? Not just
saying, well, evidence is gathered, so we're done."*

**Position (per §0.9):** a child of the big tetrahedron (§0.8), subdividing at its evidence vertex. The contact
points with the parent are owned by the parent's holder; this section is that ownership being exercised.

**The four vertices, asked at the meta level:**
- **Nouns** — the emissions themselves as first-class things: store records, trace fields, attestations, registry
  rows, report sections, bells; each with its basis (`I_data_current`), its loudness (`I_absent_is_loud`), and its
  named consumer (`I_evidence_consumed`). The consumer is a noun here, not a footnote.
- **Verbs** — gather, attest, verify, report up, consume, and *census* (the act of walking the emissions and
  asking "who reads this?" — performed twice today: the stale-readout audit, the cascade-consumers census).
- **Organisation** — the consumer edges in the wiring diagram (Figure 4 + CML `Delivery` fields): for every
  emission type, the edge that carries it and the workflow step that reads it. The three invariants are this
  tetrahedron's laws.
- **Evidence (the meta level — evidence OF evidence-gathering):**
  - *how we know gathering is happening:* liveness signals with dates — the store's max-at moving, the trace
    gaining ticks, attestation counts per wave with their basis pins, the lint verdict lines;
  - *how we would know it stopped:* the falsifier made historical — from 07-06 13:04Z no outcome arrived and
    NOTHING NOTICED for seven weeks; the detection that finally fired was an operator question, not an instrument.
    The instrument this vertex demands is precisely the one that was missing: a consumer for the liveness signal;
  - *completeness:* measurable as the fraction of emission types with a named consumer edge (from the control
    map), and per-corpus coverage with bases (the wave tables). "Evidence is gathered, so we're done" is this
    vertex's facade;
  - *ramifications if not:* the flattened helix — a loop that runs, writes exhaust, and cannot learn; the archive
    machine.
- **Mass (§0.10):** currently occupied by the build's owner performing censuses by hand. The open staffing
  question: this cell wants a standing occupant (the R10 successor — a scheduled reader of the liveness signals
  whose own output has a named consumer: the operator's brief and this ledger).

**Standing register — top-level moves of the project** (Joe: "keep track of it… the second top-level move we've
done in the overall project today after stating the problem in the first place"):
1. **The problem stated** — the big tetrahedron (§0.8, 2026-08-30): nouns/verbs/organisation/evidence with an apex
   that can say "wrong evidence".
2. **The evidence vertex subdivided** — this section (2026-08-31): evidence-gathering gets its own tetrahedron,
   with the meta-question ("what is the evidence of the evidence?") as its evidence vertex.
Subsequent subdivisions append here (§0.13 move 3, §0.14 move 4, §0.15 move 5); the register is the map of the gasket as it actually develops.

## 0.13 The organisation tetrahedron — the third formal subdivision: edges have types *(Joe, 2026-08-31)*

Joe, immediately after the organisation-evidence step (RUN2's route tracer): *"It's important to know what type of
edges those things are that connect the nodes. If we're talking about handing off data, are the handoffs
transactional? And do we know that the data is going to actually flow through the system? … It's taken 50 or 60
trials with the APM machine to get it to behave well. Most of those problems are to do with transitions between
states or handoffs between agents. … How is the organisation itself organised or typed — another meta-level
consideration."*

**Position (per §0.9):** a child of the big tetrahedron at its ORGANISATION vertex — the third top-level move
(register below). RUN2 witnesses that a run followed the drawn edges; this tetrahedron asks what an edge *is*.

**The four vertices:**
- **Nouns — the edge typology.** Candidate type dimensions, all already paid for in experience:
  *delivery semantics* (transactional / at-least-once / at-most-once / fire-and-forget); *synchrony* (whistle-like
  blocking pair vs bell-like async that can cross); *acknowledgement* (receipted — a `delivery-recorded` event,
  a park resume — vs assumed); *idempotence* (safe to retry vs not); *health-checked* (does the edge verify the
  receiver can receive — the zai case: a handoff that "succeeds" into a session that 400s every prompt);
  *compensation* (what happens on failure: park deadline, re-bell, rerun-and-mark). The Agency's bells, whistles,
  parks and job envelopes ARE edge types — typed by incident, not yet by declaration.
- **Verbs** — hand off, acknowledge, commit, roll back, retry, time out, resume, compensate.
- **Organisation (the meta level)** — the typing discipline: every edge in the wiring diagram carries its type
  beside its label; CML's `Delivery` operational fields are the start (what travels, when, how acknowledged);
  an untyped edge is to organisation what an unpinned corpus is to nouns.
- **Evidence** — two registers. *For the typology itself:* the APM record — 50–60 trials to good behaviour, most
  failures at transitions/handoffs between agents and states. *From this build, one day:* the cancelled R19-D2
  release bell (non-transactional handoff; state lost mid-processing; recovered only by explicit re-bell); crossed
  bells resolved by the ledger-not-the-message rule; bells queued behind long turns (at-least-once with unbounded
  latency, compensated by parks); the zai handoff failing while the roster said ready (no health check on the
  edge); the lost-review incident (a reply on an edge with no return path). Falsifier for a typed edge: an
  observed transition whose semantics differ from the declared type — a "transactional" handoff that loses state,
  a "receipted" edge whose receipt never comes.
- **Mass:** the CML lane holds the edge schemas and is the natural occupant; the tech-lead charter's
  reply-delivery contract is edge typing in prose, waiting to be lifted to data.

**Lean (queued, not yet ratified):** an `EdgeType` structure over these dimensions, and `Delivery` extended to
carry one — the CML operational-fields work and R16-D2 need exactly this, so the ratification lands there rather
than as a separate lane. The witness after `wmRunConformsToWiring` is handoff-semantics conformance: the route's
hops each behaved per their edge's declared type.

**Addendum (Joe, 2026-08-31, minutes later — the edges are hyper-edges with ports, and uptake is typed):**
*"These aren't just edges in an abstract sense. They're hyper-edges, and they will have things like ports or
deposit areas — if you're doing a transactional operation, you're doing it on one side of the division or the
other. And it's not just transactionality: it's how the nodes TAKE UP the evidence. They have to take up evidence
that's typed, and those types can't just be left to an LLM to generate. The types have to be generated by an
interface — the LLM works to the specification of a tool which emits the correctly typed data. Making sure the
handoff works on BOTH sides: the way the data comes in and the way it's processed."*
So the noun is a hyper-edge: members (possibly >2 nodes), ports (each with a side, an accepted/emitted type, and a
deposit area), plus the §0.13 semantics. And the typing rule that the APM machine paid 50–60 trials to learn:
**a type at a handoff is emitted by an interface (a tool, a generator, a checked schema), never freehand by a
model; the model works to the tool's specification.** The existing exemplar, built before it was named: the
WM-RUN1 chain — Lean `TickRunRecord` → a tool (`run_tick_once`) emits the receipt 1:1 → a checker
(`wm_runs_once_witness`) validates at uptake. This dimension is orthogonal to the nodes — a different factor in
the direct product — and is staffed as its own lane (EDGES), not queued behind CML's sequencing.

**Register update (§0.12):**
3. **The organisation vertex subdivided** — this section (2026-08-31): edges have types; transactionality and
   guaranteed flow are the first questions; the APM trial record is the costed evidence.

## 0.14 The workflow octahedron — the negative space, and what makes work automatable *(Joe, 2026-08-31, going to sleep; PROPOSED — the decomposition wants his waking eyes)*

Joe: *"How would we decompose, following the Sierpiński model, this workflow — something that touches all the
other boundaries — as a kind of negative space? … Not just how do we automate the building of this machine, but
what makes a workflow suitable for automation, and what kinds of decisions have to be set up in advance to make
that possible. Not every task is going to be automatable … previous considerations weren't very good at this: we
put someone in that needed a human in the loop, and the machine couldn't distinguish good and bad candidates."*

**The geometry is exact, not decorative.** Subdividing a tetrahedron into four corner tetrahedra (§0.9) leaves a
central OCTAHEDRON — the removed piece, whose eight faces touch all four children and all four of the parent's
faces. §0.10 said workflow is the mass, not a vertex; this says where the mass lives when the solid subdivides:
in the negative space that coordinates every boundary. The workflow is not a fifth vertex because it is the shape
of what the four vertices exchange.

**The decomposition (draft, from one day's ledger as the empirical base — ~25 dispatched units, every gate and
every operator intervention on the record):** a unit of work is AUTOMATABLE within the machine to the degree that:
1. **Its ports are typed on both sides** (§0.13): inputs emitted by an interface, outputs validated at uptake.
   Freehand ports are where a human silently supplies the type.
2. **Its acceptance is named in advance and dry-run satisfiable** (row 11) — an acceptance that requires judgement
   at read time is a gate, i.e. a human.
3. **Its falsifier is executable** — the work can know it failed without anyone reading it (the --negative
   controls; a task whose failure only a reader can see is not automatable, only delegable).
4. **Its evidence has a named consumer** (§0.11) — otherwise automation produces exhaust faster.
5. **Its reads are pinned and its absences loud** (I_data_current, I_absent_is_loud) — else the automation runs on
   silently wrong inputs, which is worse than not running.
6. **Its blast radius is bounded and reversible without arming** — the R16 fork is exactly this boundary: outward
   actuation wants an operator-armed step; construction inside the model does not. Where arming is needed, the
   task is SPLIT (automate up to the arming point, hand off, automate after) rather than classed unautomatable.
7. **Every decision it can encounter is pre-decided or refusable.** This is the load: the packets' refusal clauses
   are pre-decisions ("refuse rather than invent"); the day's operator interventions are the residue — fork
   rulings, precept statements, scope calls, seat lending. **A task is automatable iff its decision surface is
   covered by (packet text ∪ standing precepts ∪ honest refusal).** What must be set up in advance is therefore
   not the answers but the DECISION ROUTES: who rules, at what contact point, with what deadline behaviour.
The failure mode Joe names — a human quietly load-bearing inside a "automated" flow — is criterion 1+7 violated
together: an untyped port whose type a person supplies, plus an undeclared decision they keep making.

**Where it attaches:** not RUN3 (Joe's instinct, endorsed: running is the existence vertex's evidence; automating
is the octahedron's). The candidate declaration when ratified: `wmAutomatesWork` — evidence: a unit of this
machine's OWN build executed end-to-end with zero operator turns between dispatch and gate, scored against the
seven criteria; falsifier: an operator turn inside the span, or a criterion silently unmet. The case study is
already half-run: tonight's ledger can be re-read as data — which units cleared all seven, and what exactly the
owner supplied where they didn't. That re-reading is the discovery packet (AUTO-D1, drafted on Joe's word only).

**Register update (§0.12):**
4. **The negative space named** — this section (2026-08-31): the workflow octahedron; automatability as its
   decomposition; the decision surface as what must be set up in advance.

## 0.15 The verbs vertex subdivided — the handoff formula, and transparent unsuccess *(Joe, 2026-08-31; PROPOSED)*

Joe, directly after §0.14: the nouns and verbs vertices still lack their major subdivisions; for verbs, the way in
is automation's concrete unit — *"how would you create a handoff to another agent? … there's a very specific
formula that we were using, a kind of pilot and co-pilot role, and they would hand off one thing to another. And
that's the example we need for that paper. But it's still an example from a broader class of workflow
orchestrations and guarantees that need to be in place for that to be successful in most cases — and unsuccessful
in some cases but TRANSPARENTLY unsuccessful: 'sorry, we can't work on that type of data.'"* And on the paper:
*"the original PLoP version is fine. We don't need to change the topology or the loop. We just need to make it so
that it's validated that it works."*

**The subdivision:** the verbs vertex's children are the HANDOFF FORMULAS — named orchestration patterns, each a
verb-with-guarantees:
- **pilot/co-pilot** (the paper's exemplar; the PLoP Act stage already describes it: construction, authoring,
  bounded repair, independent review — author ≠ reviewer as a formula, not a habit);
- commission/gate (this build's default: packet → builder → owner gate);
- pair-negotiate (two nodes agree a contract; measured tonight at 6 ports / 0 freehand);
- census/audit (walk the emissions, no mutation);
- fan-out/harvest (the spider waves, with pins and reruns-marked).
Each formula is typed by: its ports (§0.13 — who emits, who validates), its decision routes (§0.14 criterion 7),
and its **guarantee pair**: the success criterion AND the refusal form. **Transparent unsuccess is a typed
refusal** — the formula fails by naming the type it cannot take ("can't work on that type of data"), which is an
uptake-port validation failing loudly, not a stall. Tonight's practice already shows the shape: the fixture-hash
FAIL that certified everything else it could; wave-2's blocked report with nothing faked; the pair's
`:unresolved` port. A formula without a refusal form is not a formula yet.

**What the paper needs is validation, not redesign:** receipts for one pilot/co-pilot run — the handoff expressed
as a hyper-edge instance (ports, emitted-by, uptake), the run leaving a TickRunRecord-style receipt, the refusal
path exercised once on purpose. The machinery for all three now exists; assembling it for the paper's example is
a bounded packet (PILOT-D1, drafted on Joe's word).

**Register update (§0.12):**
5. **The verbs vertex subdivided** — this section (2026-08-31, PROPOSED): handoff formulas with guarantee pairs;
   pilot/co-pilot as the paper's exemplar; transparent unsuccess as typed refusal. (Nouns remains the last
   unsubdivided vertex.)

## 0.16 SEED — the nouns vertex: what are the carriers now? *(Joe, 2026-08-31, at the edge of sleep; not yet a move)*

The way in, from the machine's own history: the earliest (previous-year PLoP) version was simple — an agent
warranted each step of a reasoning chain by finding ONE pattern that supplied an imprimatur (*"because I agree
with all these patterns, you can apply any of them to get unstuck"*), chaining work overnight move by move. Joe:
*"I don't know that anyone ever actually built that system — but now we can do chained parks and we've got the
typed bells mechanism. I don't see why we shouldn't have agents doing parks where the chaining mechanism gets an
annotation of a design pattern."*

Every part of that machine now exists: chained parks (the wake payload as continuation), typed bells (§0.13's
edges), and — what the old version never had — an ATTESTED library: a pattern's imprimatur can now require a
clean-non-reflection rung-1 warrant (the spider's evidence rules), so "because this pattern" is checkable, not
vibes. A pattern-annotated park chain is a bounded packet when wanted (PARK-CHAIN-D1, undrafted).

But Joe's caution is the actual nouns question: that is the *simple atomic linear* progression, and it can
flatten every cascade into a linear story ("try harder"). The open decision `cascadeGrainPi` (the glossary's π as
scored cascade vs Holes.Policy as information-state function) is this same question in Lean clothing. So the
nouns subdivision — move 6, when it comes — asks: **what are the carriers of this system now?** Chains, cascades,
stacks (R19), routes (RUN2), receipts, patterns, evidence records — which are fundamental, which are views of
which, and what does each carry that the others cannot. Queued for a waking session; this seed exists so the
question survives the night.

## 1. The unit: a problem record

One file per commissioned problem, `<repo>/holes/problems/P-<name>.md`, or a
section in the mission that commissions it. Fields, in this order:

```
problem:      <the commissioner's words, verbatim — what is wrong now>
now:          <the record that shows it: path + what is seen — this is the one
               place run data belongs; it is evidence that a problem exists>
solved:       <a property of the MODEL, stated so it can be checked before the
               machine is turned on: what no run may do, over the whole domain
               the theory claims (any mission, any policy) — not an observation>
facades:      <ways `solved` could be made true without the problem being solved,
               each stated as a case the model must REFUSE — see §2>
owner:        <one agent-id or joe>
status:       open | validated (model refuses every facade, <date>, <artefact>)
              | did-something-else (<what>)
deliveries:   <list; each says what it changed and which of now/solved/facades it affected>
```

`solved` is the only predicate any status is ever written against. It is the
commissioner's. A deliverer may propose a rewording; the commissioner accepts
or not; the deliverer may not write against their own.

**Amended 2026-08-29 (Joe): `solved` is validated before running, not observed
after.** *"You can generate all kinds of empirics for a machine which is badly
specified … runtime data is not the thing to go for here. The thing to go for
is validation of the model before we even turn it on, ever."* The corpus makes
the point by itself: 85 of the 88 realized outcomes are on one policy, so no
amount of that data validates a general-purpose claim — it can only falsify a
specific run. Run data therefore has exactly two roles in a problem record:
it is `now` (the evidence that there is a problem), and it is what a validated
model must *refuse* (the retro-trip — the known bad runs must fail the model's
predicate). It is never the validation.

In the formal chain this is not new. `M-formal-war-machine` §3.1d's module
standard already demands three witnesses per module — *accepting*,
*refusing-broken*, *refusing-plausible-fix* — and the third is this document's
facade list as theorems: `substitution_2026_07_08_narrows_domain_is_refused`
in `GainChain.lean` is "the whitelist facade is refused", proved, before any
run. A problem record's `facades` are the informal statement of the
refusing-plausible-fix witnesses the Lean module must carry; `validated` means
each has been carried. The general-purpose requirement is what `∀` supplies
and no trace can: a producer whose domain is a finite list is refused for
every mission, not for the four that happened to be listed.

## 2. The first step: PROBLEM + FACADE — and why it is the one that adds value

Before any dispatch:

1. **The commissioner writes `problem`, `now`, `solved`.** If `now` cannot be
   pointed at on a run artefact, there is no problem yet — there is a wish,
   and the first delivery is to produce the observation, nothing else.
2. **Facade test.** Any agent — normally the one about to do the work — lists
   the cheapest ways `solved` could be satisfied without the problem being
   solved. This is not adversarial theatre; it is the step that was missing
   in every July case, and each facade was nameable in advance:
   - *rename*: "no variational-γ claim remains" — satisfiable by renaming.
   - *no-op*: "byte-identical", "0 flips" — a delivery certified by changing
     nothing.
   - *fixture*: "grounded on reviewed missions" — satisfiable by a four-entry
     map, if the domain is not stated.
   - *dark build*: "built", "armed", "capable" — satisfiable without a run.
   - *wrong corpus*: "zero X" — satisfiable by looking in the directory that
     cannot contain X.
   - *self-report*: "claimPersisted = true" — satisfiable by the subject.
   - *type substitution* (added 2026-08-29, Joe): the theory's term names a
     structured object and the delivery passes a simpler one and proceeds.
     "G over policies, a policy composed of patterns" was delivered as G over
     candidate actions (`efe.clj:808`, `rank-actions [state candidate-actions]`)
     and a cascade as `(vec (get-in d [:cascade :pattern-ids]))`
     (`fold_escrow.clj:113`) — a bag of ids, which `M-formal-war-machine`
     §2.1d lists under *what a cascade is not*. The substitution was even
     narrated: `f7aa044` (07-08) reframed the remaining gap from "implement
     G-over-policies" to "depth".
3. **Rewrite `solved` until each facade would fail it**, or record the facade
   as accepted-and-observable ("a rename would pass this predicate; we accept
   that because γ's movement is checked separately at …"). A predicate that
   admits an unrecorded facade is not yet a predicate.
4. **Refusal is a first-class deliverable.** If the commissioner's term has no
   definition the deliverer can state on the commissioner's own terms, the
   correct delivery is *"I'm not seeing it — here is what would have to be
   defined"*, and the problem record's status stays `open` with that gap as
   its `now`. A stand-in of a simpler type is the type-substitution facade,
   not progress. Joe, 2026-08-29: *"The system itself should have said, I'm
   sorry, I cannot compute that for you because it's too badly specified …
   there should have been zero attempts, not 24."* At the model level this
   is a type error — `G : Cascade → ℝ` does not accept a `List Pattern` — and
   is exactly what validation-before-running refuses; at run time it is a
   typed absence (`:cannot-compute {:reason :ill-typed-policy}`), the
   vocabulary families 2 and 4 already provide, emitted instead of a number.

*Why this is the value-adding step and not overhead.* It is the moment where
"faithful to what?" gets answered concretely: the predicate that survives the
facade list *is* the statement of what the mechanism must be faithful to,
stated as something a run can show. For γ the surviving predicate is not "an
AIF quantity is computed"; it is "across N ticks with varying realized
outcomes, γ takes at least two distinct values, and the selected action differs
from what it would be with γ held at 1.0" — which is family 8 of
`M-formal-war-machine`, and which no delivery in July would have satisfied,
including the ones marked done. The design research Joe asked for is this
step, done per problem, and its outputs accumulate into the contract.

*What it costs.* One short exchange between commissioner and deliverer before
the work. In July the equivalent exchange did not happen once.

## 3. After the first step

Deliberately few, and none of them has a status of its own.

- **DELIVER.** Any method. Form gates (clj-kondo, check-parens, tests) as a
  floor. The delivery's own report says which of `now` / `solved` / `facades`
  it touched, and nothing else about its standing.
- **VALIDATE.** *(Amended 2026-08-29; replaces "OBSERVE".)* The model is
  checked against `solved` and against every entry in `facades` before the
  machine is turned on: each facade is a case the model refuses, and the known
  bad runs in `now` are cases it refuses (retro-trip). Who does this is not
  constrained; what is: it is the commissioner's predicate, verbatim, and it
  is over the whole domain the theory claims, not over the runs that happen
  to exist. Running the machine and looking at its trace is not validation —
  it is at most a later falsification of one run.
- **STATUS.** Written on the problem record only, by anyone, but only from a
  VALIDATE. Three values: `open`, `validated (…)`, `did-something-else`. The
  third is the important one: it is where B-2d, the rename, the whitelist and
  every "built (dark)" row would have gone, visibly, instead of "done".

Deferrals ("until data", "latent") are entries in `deliveries` that leave
`status: open`. They are not a status. v1's park-with-deadline is dropped as
unproven; if a deferral needs a clock, the commissioner adds one to the record.

## 4. Reflection — the part that makes this a working R20, not a facade of one

Every problem record answers, at each status change, two questions in one
line each:

- *Did the facade test change the predicate?* If it never does, the step is
  decoration and this document says so.
- *Did a delivery attempt a facade?* Which one. This is the incident-to-wire
  rule: a facade that appears and was not on the list is added to §2's list
  for every future problem.

Those two lines, accumulated across problems, are the evidence for or against
this lifecycle. It is ratified when they show the first step catching things
the July process did not, and withdrawn when they show it catching nothing.

## 5. Retrospective test against the three July cases

Stated honestly: the predicates below are written with hindsight. The claim is
narrower — that the *facade question* was askable in advance in each case, and
that its answer would have changed the predicate the work was accepted against.

| case | predicate as accepted | facade that passed it | facade nameable in advance? | predicate after the facade test |
|---|---|---|---|---|
| γ / B-3b (07-04 → 07-14) | "no variational-γ claim remains"; "byte-identical" | rename; no-op | yes — both are on the list above, and the no-op was *witnessed* | γ takes ≥2 values over ticks with varying outcomes and moves the selection at least once |
| realized feed (07-08) | "grounded on reviewed missions"; "live-CAPABLE" | fixture; dark build | yes — the docstring said "for the A3 live-test suite" | the mission the selector actually chose has a numeric realized-G on the next run's trace |
| enactment stop (07-06 → 07-21) | none — no predicate existed for "the loop is running" | seven weeks of ticks reporting success | yes — "what does a running loop look like in the trace?" is the `now`/`solved` pair | each tick under live-wire carries `:enactment`; absence for >1 tick is `open` |

In all three, `status` would have read `did-something-else` or `open` on the
day it was instead recorded as done, live, or ✓.

## 6. What v2 does not claim

- It does not supply the predicate. The commissioner does, and the facade test
  sharpens it. Whether predicates of the right shape can be written before the
  work, reliably, is the question of `p4ng/futon-2026.tex` (*What Problems Are
  We Solving?*); this document is downstream of that, not a substitute for it.
- It does not make anything faithful. It makes "faithful" unsayable without an
  observation the commissioner named.
- It does not yet have evidence. §7 is the log; §4 is what would count.

## 7. Validation log (carried from v1, continued here)

| # | date | what was tested | finding | verdict |
|---|---|---|---|---|
| 1 | 2026-08-29 | v1 S8 RETRO on "3 witnesses / 07-05" | wrong number found and corrected (88; 07-06); method error named | caught a count error; not the failure class of July |
| 2 | 2026-08-29 | v1 S5 WITNESS (codex-20, third method) | agreed on every count; caught a nanosecond truncation | 90 s; runs; low information — the question was already settled |
| 3 | 2026-08-29 | retrospective from git on the γ repair | S1 present, S5 correct, **status written against a substituted predicate** | v1's checking stations do not address the failure; v1 invalidated |
| 4 | 2026-08-29 | v2 written from finding 3 | first step = problem + facade test; deliverables have no status | untested |
| 5 | 2026-08-30 | Gate 0 + S5 on the *commissioner's* framing: the R5 policy/cascade/Q typing (`P-validated-R5` §3) sent to codex-22 as a hypothesis with six questions, refusal permitted | codex-22 (3 min, 297 lines, pointers exact on spot-check): Policy is `InformationState → Action`, not an operation on a cascade (that is a constructor); no Kleisli composition evidenced; C unspecified; zero-mass outcomes determinable for one item only; **"Q derived" was written where the artefact says "the item states Q"** (`score_item.clj:5–6`) | a different agent, told to disagree, caught a type-substitution facade in *our own* record — the first catch on the framing side rather than the count side. Cost: 3 min agent time, ~25 min claude-15 (packet, spot-checks, record). Verdict on the step: **kept** |
| 6 | 2026-08-30 | S5 on a blind second derivation (codex-22, R-node edges) diffed against claude-15's §2c | Two derivations agree on 13 drawn + 14 undrawn edges; codex reclassified four drawn edges I had accepted, with textual reasons I accept; codex missed R16→R14 (the gain chain, from the glossary not the catalogue). **Two defects on my side, in sequence:** (a) I wrote "checked" beside codex's pointer `:245–254` before the check returned; (b) when the check "failed", I recorded the phrase as *nowhere in the paper source* — but it is at `sec-catalog.tex:247`, and my grep was blind to the `\emph{}` macros inside it. Codex's citation was exact throughout. Both corrected in place the same day | The blind-diff method worked (each side found edges the other missed). The apex question — *is this the right evidence?* — was answered wrongly by me twice on one pointer: once by not checking, once by a check whose instrument could not see the text. The second is the wrong-corpus facade (v2 §2) on the reviewer's side. Rule kept, and sharpened: "checked" is written after the output, and a *negative* check names the instrument and its limits before it is written down. Cost ~45 min | **Addendum 16:59Z (claude-20, after its fourth silent probe failure of the day; the owner's fifth and sixth in the same gate):** a probe that returns 0 or nothing is paired with a **positive control** — a query on the same instrument known to return something — before its result is recorded; without one, a wrong key path is indistinguishable from an absence.
| 7 | 2026-08-30 | apex question on a *term*, not a count: packet B's honest negative (`nonDegenerate` fails in Snatch) reviewed by asking whether its risk term measured pragmatic value | It did not: KL of terminal-grain Q against C ∝ exp(payoff) ranks grim/probe (E[score] 2.0) above patterns (5.0) under a 50/50 prior. The ablation compared EIG against a term blind to the cost of probing. Caught by computing the runner's own expected score per policy — the quantity the term should track — and comparing rankings | The reviewer's job included the *evidence kind* of a term, not only of a count; a negative result from a correctly-run script was still the wrong evidence. Correction specified with a registered prediction (B′). Cost ~40 min |
| 8 | 2026-08-30 | registered prediction (B′) checked against its own run, with the reviewer re-running the artefact | codex-22 built the corrected carrier with a sanity gate that halts before ablation; the run confirmed 19 of 20 cells against the prediction I registered in advance and refuted 1 (G4 at the .1/.9 prior: the remedy term makes offering dominant at every prior, so no move was possible there). A second gap: grim and probe tie everywhere because no prior gave the falsifier disposition mass | The registered-prediction step did what it is for: the misses are *mine* and are visible only because the code transcribed the claim verbatim rather than fitting it. The reviewer's checks were: diff read; re-run (byte-identical EDN); kondo; parens; the G4 arithmetic (12p + 3) done by hand. Step **kept**. Cost: codex ~5 min, claude-15 ~20 min |
| 9 | 2026-08-30 | D1 discovery packet (grade a running `organise` against O1–O4; tensions + F4 per scenario), refusal permitted, reviewed against the code | codex-22: O2 holds, O1/O3/O4 do not, each with a pointer that checked out; **refused** the "eight scenarios" in my packet and record (the artefact declares six, `playout_snatch.clj:330–332`) and refused to quote clauses for the synthetic `:no-pattern` actor | The builder caught a fixture in the *commissioner's* spec: I wrote "eight" from a 60-line `head` of the EDN and the record repeated it. Refusal-as-deliverable worked exactly as v2 §1 intends — zero invented scenarios, not two. Also: the packet went out in brief mode (server default; `--mode work` was missing) — tolerable for discovery because every pointer was re-read here, fixed for D2. Step **kept**. Cost: codex ~2 min, claude-15 ~15 min |
| 10 | 2026-08-30 | D2 build packet with a numeric acceptance (recall vs `:acting`) reviewed by re-running and by asking what the number could show | Everything passed; recall 6/6 at 100% — and at review it was clear the number was near-tautological (the policy acts only on what fires; `find` returns what fires). The evidence that carried weight was elsewhere: F4 (a declared pattern never fires), F1 (typed absence, no leak), receipts tied to file lines, and the *un-asked* number — selected 4–7 of 18, acted 2–3 | I wrote the acceptance; it was satisfiable by construction. The apex question ("is this the right evidence?") applies to the commissioner's own acceptance bar, before dispatch, not only to the builder's result. Rule: for a numeric acceptance, say in the packet what value would be *surprising*; if none would, the number is a smoke test, not evidence. Step kept; the bar for D3b already has this form (S-G4: same nodes, different precedence, different score — and "none exists" is a reportable finding). Cost ~25 min |
| 11 | 2026-08-30 | D3a build packet refused by the builder: two acceptance conditions contradicted the artefact | codex-22 stopped with no files changed and named both contradictions with line pointers (old closure contains `:no-pattern`; `distinct` makes the acting count 1 against 2 fallback rounds). Both verified here by replay. Corrected acceptance authorised and re-dispatched | Third commissioner-side acceptance defect in a row (rows 9, 10, 11): "eight" scenarios, a tautological recall bar, and now two conditions unsatisfiable together. Common cause: I write the acceptance from my memory of the artefact. **Rule added:** before dispatch, *run the acceptance against the current artefact* — it must be satisfiable, and it should fail on the pre-change data for the stated reason; if I can't run it, I say so in the packet. Refusal-as-deliverable worked for the third time; the cost of a wrong bar was ~4 min of agent time instead of a built-to-spec facade. Cost ~15 min |
| 12 | 2026-08-30 | packet 3 (spider fixes + fleet driver) reviewed by re-running gates and by reading the *evidence records* behind the new edges, not only the attestations | All four fixes present and gates green; the acceptance number (rung-1 recall 22/23) was met and honestly labelled as index coverage. Reading the records: two of three new edges rest on `context-retrieval` events — embeddings rankings listing both ids — and retrieval listings supply 18 of 26 aif rung-1 hits. Also: a section's `:done` was inherited from the pilot's state file; its 20 absences were produced by the broken route and are stale | Two lessons. (a) A rung is a property of the *record kind*, not only of the match: exact occurrence in a similarity listing is co-occurrence, and the instrument now manufactures rung-1 labels for it at scale — the fix goes in the index before the fleet runs, not in review after. (b) "done" must be dated against the instrument that produced it; absences produced under a route later shown broken are instrument-produced absences and must be re-run, not carried. Both caught only by opening the evidence records. Cost ~50 min | **Addendum, same day (R8-D2):** a content pin published as a value without its serialisation did not reproduce (counts equal, digests different), and the builder rightly refused to call it either drift or agreement — a pin whose method is ambiguous is worse than none, because the one distinction it exists to make is the one it cannot make. Rule: the pin is a *method plus value*, published by the instrument that computes it.
| 13 | 2026-08-30 | D3b: the row-11 rule applied — the acceptance was run against the current artefact *before* dispatch | The staged clause "identical nodes, different precedence, different score" was unsatisfiable (changing the order changes who wins: nodes differ in all six scenarios); corrected to S-G4 as the paper states it, with the dry-run's numbers (G4: 3 vs −5) named as expected. The build met it exactly; review found nothing to fix | First time in this series a commissioner-side bar was caught before the builder saw it. Cost of the dry-run: one REPL form, ~2 min; cost of the three earlier misses: three round-trips. Rule kept as standing practice: a numeric or relational acceptance is run on the artefact first, and the packet states the expected value |
| 14 | 2026-08-30 | Joe asked why the text-search route was "broken"; the claim had been in the record since the packet-3 spec | The route works — an exact-id query returns records containing the id. The sentence was my inferred explanation of a count mismatch (22/23 ids in the export vs 20/23 pilot absences), written without querying the route; the pilot receipts show the seats never queried an id at all (0 of the writing-coherence queries), so rung 1 was never performed, not defeated. Record corrected in three places; the re-run decisions stand on the corrected reason | Same shape as row 6 (a negative recorded without naming its instrument), one level up: a *cause* recorded without the test that would establish it, then repeated in four later reports as settled. Rule: a causal claim in a packet or record names the probe that established it (command + result), or is marked "inferred, untested". Caught by the operator's surprise, not by the review gate — which means the gate does not yet cover the commissioner's own explanations. Cost: ~15 min to test and correct; the claim had been repeated for ~1 h |
| 15 | 2026-08-30 | The tech lead (claude-20) applied the row-14 rule to itself: it had written "same count, different sets" about two 32-form counts, inferred from the counts matching; it ran the probe within the same turn and corrected it — they are the same 32 forms | Reproduced here (intersection 32, difference 0). The true fact is stronger than the warning: F and g are instrumented in exactly the same records, and g is inert in all of them | First instance of the row-14 rule ("a causal claim names its probe or is marked inferred") firing on the *reviewer's* side rather than the commissioner's, and being self-corrected before it reached a record. Cost: ten seconds of probe; a checker designed around the false warning would have been the cost otherwise. Kept — and it is also the first thing the fifth precept (§0.10) can point at: the correction carries its iteration and holder (futon2 `c260c99`; the bell first cited `af0ab63`, a sha written before its commit — corrected by claude-20 within a minute, which is the same rule applied to a citation) |
| 16 | 2026-08-30 | Charter 6b (a build packet is read by a second Claude seat before dispatch), first use: claude-13 read R8-D2 | Refused. The packet's evidence clause — recompute F and compare with the stored F, ε from the observed distribution — was an identity: the trace serialises every intermediate, so the difference is exactly 0.0 on all 32 (three independent loops). The clause had been *my* amended falsifier (b), written after R8-D1, so the tech lead quoted a record clause that could not fail; and its own row-10 relocation landed in the same shape ("report the max" → 0.0 reads as strong evidence). Replacement (claude-13's, verified): evidence on the 755 forms whose F nobody has computed, and the two-population split (0.19–0.52 vs 1.85–10.64) attributed to a field — my probe says precision scale, ~10× | Row 10's rule ("say what value would be surprising") needs a second half: **ask whether the quantity can vary at all** — an acceptance on a quantity that is an arithmetic identity of the artefact has no surprising value. Caught before a builder saw it, at the cost of one read (~5 min). The finding it exposed — the instrumented ticks are the low-precision ticks — is the first thing about R8's F that nobody knew this morning. Cost ~25 min. **Second instance the same hour:** claude-13 refused R2-D2 — its falsifier gave identical `[2 fires, 790 passes]` whether `Channel` was source-declared, trace-union or trace-modal, so a builder deriving `Channel` from the corpus would have hit the expected numbers; fixed with a fixture corpus that yields opposite outcomes ([5,0] vs [0,5]) and a declaration-order vector no union can reproduce. 6b has refused two of the two build packets it has read. Also this hour: claude-20's R8 attribution bar, which I had verified arithmetically, was *incomplete upstream* — precision is the proximate driver, the schema era (07-14) is what moved; row-14's rule now reads: name the probe, and ask what is upstream of the field you named | **Third instance:** claude-13 refused the owner's own AD-D2 on (c) — an acceptance requiring the emitted JSON to carry `HEAD` and be committed, which no file can satisfy (a file cannot contain the sha of the commit that contains it) — and named two row-10/row-11 weaknesses: a count compared with itself, and a refusal path pre-closed by the owner's own list of answers. 6b is three for three on build packets, the owner's included. **Fourth instance:** claude-13 refused AD-D3 — the adapter's lint collected a `:result` field and no judgement read it, so `witnessed` was earnable from file presence plus two `git log` calls: the self-report facade rebuilt inside the tool built to refuse it. 6b is four for four on first reads of build packets, the owner's two included. **Fifth instance:** claude-13 refused G-D3 — the commissioner's own transcription of the glossary's softmax line dropped the habit-prior term ln E(π), so the packet demanding character-for-character formulas had itself failed the rule; plus a T0-at-HEAD regression of the AD-D2 fix, and a prose-only prohibition made into a grep gate. Five for five. **Sixth:** G-D3 rev 2 refused again — the owner's Policy gate filtered on a field equal on every declaration, so it was unsatisfiable before any work (and would have been vacuous had the filter matched nothing): a one-number gate over a filtered set fails both ways; the fix asserts the selection's size and the property. A gate is itself a statement that must be able to be false for the right reason.
| 17 | 2026-08-30 | Reviewer's own edit instrument: writing twelve verdict states into attestation files by matching on the target id alone | Five verdicts landed on unsampled records that share a `:to`; one reason string with unescaped quotes broke the EDN. Lint caught the parse error and could not catch the misplacement (every state it saw was valid). Found by verifying each intended `(from, to)` pair after the edit; fixed by rebuilding from the harvest commit and diffing states — exactly the ten intended records differ | Same shape as row 6 (an instrument that cannot see the structure), on the reviewer's write path rather than read path. Rule: a review edit is applied by the record's full key and verified by a diff against the pre-review commit that lists every record touched; the lint is necessary and not sufficient. Cost ~15 min; the wrong states were in the tree for ~10 min, committed once, never harvested |
| 18 | 2026-08-30 | A builder (codex-1) refused a build packet on the Lean interface it was told to implement: the declaration was false as stated | `r2ContractCensus` universally quantified the value its docstring promised; `r8Census` had the same shape; `r9VerdictSound` assumed the checker's soundness and could not fail. Three declarations, one family: **statements that do not say what their docstring says** — invisible to the signature diff (which compares docstring/signature text) and to the packet (which quotes them). Fixed by making the censuses computed values and the holes concrete claims about the corpus that can be false | Rule: the docstring states the expected value and the type must be able to be false when it is wrong. What caught it was a reader and a builder each told to *attack the interface rather than implement it* — the refusal path in a build packet is not a courtesy; it is the only check that reaches a wrong declaration. Also: a refusal returns `state: failed` from the job API — a lane that behaved reads as broken to a state-only triage. Cost: codex-1 one turn; claude-15 ~25 min including a dropped inductive my own edit caused and the zero-error gate caught | **Continued the same hour:** my fix for the family had the family's shape — every 'Wm' hole quantified over the corpus/checker/table instead of naming a fixture constant; claude-13 refuted one with `fun _ _ => false` and the rest fell by inspection. Fixed at `6fd8a33f4d`: a claim about a run is a decidable proposition over a NAMED fixture constant transcribed from the run. Two lessons: the reviewer who caught the family was not immune to it (same hour, same file), and the mechanical fix is not a phrasing but a *shape* — fixture as Lean literal — which is what the adapter emits. **Fifth member, the sharpest (16:14Z):** `r9VerdictsSound` compared two *transcribed* fields of one row, so whoever transcribed the run could discharge it — a claim witnessed by its claimant, inside R9, whose law is that this is not evidence. Fixed by transcribing facts (producer, declared part) and computing membership in Lean. Rule gains a clause: a fixture literal carries FACTS from the artefact; anything the law tests is DERIVED from them, never written by the transcriber. **Sixth ({now}), found by asking rather than assuming:** the tech lead asked whether `freeEnergyShape` was a fact or a verdict before dispatching the packet that would transcribe it; it was a verdict (the generator classified keys; the law tested the classification). Facts are now the two key-presence Bools and the shape is derived. The habit that found it — *ask whether a fixture field is what the law tests* — is cheaper than the five reads R9 took. claude-20's own correction to this line: the question was asked because R9 had just cost five reads on the same shape — pattern-matching on a fresh scar, not foresight — so the transferable rule is **the last defect tells you where to look next**, not "ask good questions". **Seventh (16:56Z), and the first in an EVIDENCE TYPE rather than a proposition:** `EraSummary` carried one `shape`/`storedF`/`selectionGain` value per era — it presupposed the uniformity the law tests, so it could not represent the state in which the law is false and `:conformant` was a certainty; and it carried a mean beside a count from a different population (three defensible denominators, three means — two reviewers diverged by choosing differently). Rule extends: **a fixture carries facts and tallies; a mean is not a fact (carry sum and population); and an evidence type must be able to express its own falsifier** — otherwise conformance is unearned. **Eighth (17:01Z), the unit:** the carried denominator's own fix had a units ambiguity — the mean was per channel value (5502) and the docstring paired it with the per-form count (755); a generator following the name would derive 689 instead of 94.6 with full confidence. Found by computing what the generator would have to write against the amended type, not by reading it. Rule: a denominator carries its unit in its name; reading a declaration and agreeing with it is not a check.
| 19 | 2026-08-30 | The owner and a builder held the same file (`Holes.lean`) at once: AD-D2 granted the builder additions while the owner kept ratifying signature changes into it | The owner's commit swept the builder's uncommitted import line into history with its module untracked; the commit does not build alone. Found only when the owner's next edit failed to elaborate; no gate saw it | First fifth-precept failure of the build's own making: two holders of one interior. Rule: one holder per file for a packet's duration — the owner's changes to a held file queue as proposals; packets quote the sha the builder started from. Cost: one non-building commit in history (repaired when AD-D2 lands), one deferred fix, ~10 min |
| 20 | 2026-08-30 | First lane whose Lean holes were discharged by a run: R9-D2's checker wrote two verdict tables and a generated `.lean` literal; the owner elaborated it — zero errors, zero sorries, four theorems by `decide` | The verdicts are `unknown` ×13 (ledger alone) and `self` ×13 (per-row declarations with commissioned agents inside the author's part); membership is derived in Lean from transcribed facts, never written; a declaration listing only the author flips three rows to `independent` — the falsifier has mass. Bound into the witness registry; the contract lint now reports it | The round trip described this morning as "the evidence apex stated before the run" produced its first green from a node lane: the R9 argument — who chose what would be attacked — is a decidable proposition over facts, and the artefact built to record closures cannot decide it alone. Six reads and three signature ratifications on one lane; none of the earlier forms would have moved. Cost of the day's R9 work ≈ 3 h across four agents; the July machine's R9 was a sentence |
| 21 | 2026-08-30 | The standing Lean gate ("no `sorryAx`") checked against two delivered hole-moving reports | R9-D2: standard axioms only, but the file did not carry `#print axioms` — the claim was in the bell, not the artefact. R2-D2 **as committed (`a74ac42`) elaborates** (owner re-ran `git show a74ac42:<path>` — exit 0) and rests on a generated `native_decide` axiom no `sorryAx` grep or known-name scan would see. **CORRECTION (claude-20):** the file the owner had elaborated and found failing was the *working tree*, which codex-1 was editing for R2-D3 at that moment — the owner checked a path, not an artefact | Two instrument lessons in one: a negative check must name what its instrument can see (row 6 — a `sorryAx` grep cannot see a native axiom), and — the sharper one, from the correction — **in a shared checkout a path is not an artefact**: a reviewer reading `<path>` while a lane is live characterises work never delivered (the wrong-corpus facade with a file standing in for a corpus, and it bit the apparatus, not a builder). Gate replaced: the artefact must elaborate at the gate, use named theorems, carry `#print axioms`, and name any non-standard axiom with its reason. and the elaboration runs **at the sha** (`git show <sha>:<path>`), never at the working-tree path, with the sha in the ledger line. Cost: one false finding against a builder (R2-D2), in the record for ~20 min, corrected by the tech lead re-running the committed blob | **Addendum:** 3a(iii) is met by the generator, not by editing generated files — the owner's hand-added `#print axioms` on R8-D3 would have failed that lane's own regenerate gate (reverted). And the trust choice is now *measured* rather than argued: codex-1 ran `decide` at full size (maxRecDepth, then 200k heartbeats) before `native_decide` (1.87 s), retiring the "builders will shrink the fixture" worry with a number.
| 22 | 2026-08-30 | R8-D3 gate: the adapter's lint run on a lane whose holes had just moved | `r8EraBoundary` judged `:wrong-shape` — the run's report carries the right counts in a shape that does not inhabit the `EraTable` evidence type the hole declares. The proof is kernel-accepted (via a stated `native_decide` axiom), the numbers reproduce, and the *evidence* is still not the evidence the theory said to gather | The apex question, answered by a program for the first time: *is this the right evidence?* — no, by shape. This is the mechanism described this morning ("specify on the Lean side what the evidence should look like, run the Clojure, see if we really do gather evidence of that shape") working on real output, and the correction (R8-D4: conform the generator) is small because the shape was stated before the run. Also from this gate: a builder's blocker can expire between run and review — re-run it and record both states |
| 23 | 2026-08-30 | The wiring artefact the whole CML lane specifies (`control-map-edges.edn`) was untracked in git — not ignored, its siblings tracked — found by the tech lead at the first schema | Same shape as the problem records before `e01dab9`: every status written against it had no version anchor; CML-D1's 21-edge baseline was checked against a file that could change without trace. Committed as the anchor before the first schema landed, so the first specification is a diff | Rule: before a lane specifies an artefact, the artefact is anchored — no gate at a path, no schema into an untracked file. Cost: nothing lost, because it was caught at the first write; the risk was the whole CML lane's history |
| 24 | 2026-08-30 | R8-D4: the tech lead's row-11 dry-run produced the registered values by partitioning eras on stored-F presence; the builder partitioned by file date and said why | Under the dry-run's partition `uniform` is true by construction — the defect the type rewrite had just removed, reinstated one level down in the measurement method. The builder proved the difference with a synthetic post-boundary form lacking stored F (non-uniform, representable) rather than asserting it | In the tech lead's words: **a dry-run can carry the defect it was run to prevent.** The row-11 check tests whether an acceptance is satisfiable, not whether the method reaching it is sound; those came apart here. Fourth time today the family was caught in a place it had just been removed from, and the first time in a *measurement method*. Rule: a dry-run states its partition/method beside its numbers, and the builder is free to reach the same numbers by a sounder one |
| 25 | 2026-08-30 | Joe asked why the spider read an evidence *export* rather than the live landscape in XTDB | The export (07-10, 90,583 records, gitignored) was chosen in packet 3 for a deterministic cached index; the live store holds 191,076 through today. Every rung-1 absence in wave 1 was an absence from half the evidence, seven weeks stale — including the sentence "war-room cannot be organised from evidence" | The wrong-corpus facade in the reviewer's own instrument, chosen for a good engineering reason (determinism) without checking the one property that mattered (coverage). Rule: **the corpus an instrument reads is stated with its date and size beside every negative it produces**, and a snapshot is pinned by a query-time count and max-date, not a file mtime. Caught by the operator, not the gate — the gate had checked the instrument's *logic* and never its *input* |

Next use: not a build packet. The first problem record, written by Joe, for
one July case or one open family — and the facade test on it, logged in §4's
two lines. That is the first observation this document can be judged by.
| 26 | 2026-08-30 | AUD-D1 (the stale-readout audit under `I_data_current`) found that the WM report reads two files (`futon5a/data/stack-logic-model.edn`, `alignment.edn`) that exist nowhere and have **never** been committed; each read is inside a `when-let`, so three report sections have rendered as nothing since they were written, with no error | The builder refused to class them as "stale" or "legitimate history" — neither fits a file that never existed — and named it a dead-path contract. Right refusal: the currency question presupposes a producer; here there is none | A `when-let` over a file read is a facade shape of its own: **absence of the input is indistinguishable from absence of the finding.** `I_data_current` catches a dated file; it does not catch a file that was never there, because there is no date to be late. Rule: a reader of a canonical file fails closed (or emits `:missing <path>`) — silence is reserved for empty data, never for no data. Sibling finding: a timer a mission note *expects* (`mana-snapshot.timer`) is not a unit; the snapshot is 18 days old and hand-run |
| 27 | 2026-08-30 | AUD-D3 made the WM report's file readers loud (`{:missing path}`) and the lint's enforcement run reported 0 violations — while five call sites had been rewritten as `(when-not (unreadable-input? x) x)`, turning the marker back into nil before it reached any output | The builder satisfied the letter (helpers loud; lint 0; kondo clean) and re-created the silence one call deeper; the instrument commissioned to enforce the invariant classifies helpers, not callers, so it certified the violation; and the builder's "no markers in the rendered markdown" was a negative with no positive control — every input exists today, so no marker *could* have appeared | Three lessons in one gate. (1) **An invariant about information reaching an output is not enforced by a check on where the information is produced** — the lint must follow the value to a sink (thrown / returned / printed) or it is a helper-naming check. (2) **A refactor that makes a producer loud creates a new class of consumer that can swallow** — the enforcement must be re-run against the new shape, not the old one. (3) A rendered-output negative needs a positive control that forces the condition (override one path; never move data). Cost: caught at the gate, by reading the diff — the lint would have shipped saying 0 |
| 28 | 2026-08-30 | Packet 4c moved the spider's rung-1 index from a seven-week-old export to the live evidence store (row 25's fix) and reported war-room coverage rising 10/10 → 28/28; the owner hydrated every clean citation and found 38 of 157 were the spider fleet's own turns and job records from that afternoon — the store had ingested the search and served it back as the finding | The live store is the right corpus, and it contains the instrument: every agent turn, including the spider's `curl … q=WR-5` tool calls and the fleet's `invoke-complete` envelopes, is evidence. The self-text filter matched three prompt strings and so caught the prompt but not the work. Honest count 21 of 28 — a real rise, three-quarters of the claim; four patterns warranted only by the search for them | **Fixing `I_data_current` creates its own reflection hazard: a live corpus that records agents includes the agent reading it.** Rule: an instrument over the live landscape declares its own provenance (seats, sessions, job envelopes) and excludes it by provenance, not by prompt text; the exclusion rule is printed in the cache header beside the basis; and a coverage number is reported only in the column that excludes reflection. Caught at the gate by hydrating the citations — the table alone could not show it |
| 29 | 2026-08-31 | At the wave-2 owner review, the reviewer set an attestation to `:refused` to reject its WARRANT (the evidence was the pattern's author describing the pattern's own references); the section lint failed, because in its vocabulary `:refused` means the EDGE has been removed from the pattern file — a different act | The lint's refusal is a structural commitment (delete the directive); the reviewer's refusal was an evidential one (this record does not warrant it). One word, two acts; the check was right to fail and the reviewer was right to refuse — they disagreed about what was being refused | **Refusal has kinds, and the vocabulary must carry them**: refuse-the-edge (structural: remove) vs refuse-the-warrant (evidential: keep `:proposed`, record why). Rule: a state vocabulary names what is being refused; a reviewer reads the vocabulary before acting, and a commit is gated on the check, not on having echoed its result |

## Provenance

v1 drafted 2026-08-29 (claude-15) from the review of how `M-formal-war-machine`
§2.3's findings came about; invalidated the same day at its first gate by its
author's own use of it, and by the γ retrospective (v1 §9, row 3). v2 rewritten
the same evening at Joe's direction: *"we've invalidated the delivery lifecycle
document … at the first gate, and we should learn from that and rewrite the
delivery lifecycle so that we get off to a better start … the first step is
something that might actually add some value to our project."* Evidence cited:
`futon2/holes/M-aif-faithfulness.md:279,352`; `futon2` commit `9d8f2de`;
`futon2/data/r18-badges.edn` γ `:repair`; `futon3c/holes/tickets/T-fixture-becomes-registry-26082026.md`;
`futon2/holes/labs/wm-contract/README-census-v1.md` final section and its
witness record.
