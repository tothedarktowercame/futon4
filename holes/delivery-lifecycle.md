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
| **Money / verbs** | Figure 4's edges: 10 control edges forming one cycle, 11 dashed support edges | **exist only as Bezier paths** in `p4ng/aif-control-map-paper.svg` (24 `<path>` elements). No file states an edge between two R-nodes. `M-formal-war-machine` §1.6 criterion 1 — "the wiring is data" — is the oldest open item in the mission and was never in the H-series. Precedent vocabulary: `p4ng/empirics-futon/fig-loop.edn` (`{:from :to :label :kind :status}`, with `:holds` as a predicate); where an edge is a handoff it also carries the `Delivery` fields of §0.6 |
| **Organizations / fit** | the typology the diagram gives of the edges *and* of the whole | follows from the edges once they are data: five phased columns, one cycle, cross-column support; which edges are transitions, which are constraints (the §1.4 hypothesis that support edges are APM-style policies is testable only then) |
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

**Validation.** Not ratified. It is applied first to the R-node build (the ledger, the tech-lead
charter, the `BUILD-packets/` files) and to the P-R9 / P-control-map-lint records; a log row is
written when it catches something the four precepts did not, or when it fails to. If it never fires,
it was not a precept.

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
| 6 | 2026-08-30 | S5 on a blind second derivation (codex-22, R-node edges) diffed against claude-15's §2c | Two derivations agree on 13 drawn + 14 undrawn edges; codex reclassified four drawn edges I had accepted, with textual reasons I accept; codex missed R16→R14 (the gain chain, from the glossary not the catalogue). **Two defects on my side, in sequence:** (a) I wrote "checked" beside codex's pointer `:245–254` before the check returned; (b) when the check "failed", I recorded the phrase as *nowhere in the paper source* — but it is at `sec-catalog.tex:247`, and my grep was blind to the `\emph{}` macros inside it. Codex's citation was exact throughout. Both corrected in place the same day | The blind-diff method worked (each side found edges the other missed). The apex question — *is this the right evidence?* — was answered wrongly by me twice on one pointer: once by not checking, once by a check whose instrument could not see the text. The second is the wrong-corpus facade (v2 §2) on the reviewer's side. Rule kept, and sharpened: "checked" is written after the output, and a *negative* check names the instrument and its limits before it is written down. Cost ~45 min |
| 7 | 2026-08-30 | apex question on a *term*, not a count: packet B's honest negative (`nonDegenerate` fails in Snatch) reviewed by asking whether its risk term measured pragmatic value | It did not: KL of terminal-grain Q against C ∝ exp(payoff) ranks grim/probe (E[score] 2.0) above patterns (5.0) under a 50/50 prior. The ablation compared EIG against a term blind to the cost of probing. Caught by computing the runner's own expected score per policy — the quantity the term should track — and comparing rankings | The reviewer's job included the *evidence kind* of a term, not only of a count; a negative result from a correctly-run script was still the wrong evidence. Correction specified with a registered prediction (B′). Cost ~40 min |
| 8 | 2026-08-30 | registered prediction (B′) checked against its own run, with the reviewer re-running the artefact | codex-22 built the corrected carrier with a sanity gate that halts before ablation; the run confirmed 19 of 20 cells against the prediction I registered in advance and refuted 1 (G4 at the .1/.9 prior: the remedy term makes offering dominant at every prior, so no move was possible there). A second gap: grim and probe tie everywhere because no prior gave the falsifier disposition mass | The registered-prediction step did what it is for: the misses are *mine* and are visible only because the code transcribed the claim verbatim rather than fitting it. The reviewer's checks were: diff read; re-run (byte-identical EDN); kondo; parens; the G4 arithmetic (12p + 3) done by hand. Step **kept**. Cost: codex ~5 min, claude-15 ~20 min |
| 9 | 2026-08-30 | D1 discovery packet (grade a running `organise` against O1–O4; tensions + F4 per scenario), refusal permitted, reviewed against the code | codex-22: O2 holds, O1/O3/O4 do not, each with a pointer that checked out; **refused** the "eight scenarios" in my packet and record (the artefact declares six, `playout_snatch.clj:330–332`) and refused to quote clauses for the synthetic `:no-pattern` actor | The builder caught a fixture in the *commissioner's* spec: I wrote "eight" from a 60-line `head` of the EDN and the record repeated it. Refusal-as-deliverable worked exactly as v2 §1 intends — zero invented scenarios, not two. Also: the packet went out in brief mode (server default; `--mode work` was missing) — tolerable for discovery because every pointer was re-read here, fixed for D2. Step **kept**. Cost: codex ~2 min, claude-15 ~15 min |
| 10 | 2026-08-30 | D2 build packet with a numeric acceptance (recall vs `:acting`) reviewed by re-running and by asking what the number could show | Everything passed; recall 6/6 at 100% — and at review it was clear the number was near-tautological (the policy acts only on what fires; `find` returns what fires). The evidence that carried weight was elsewhere: F4 (a declared pattern never fires), F1 (typed absence, no leak), receipts tied to file lines, and the *un-asked* number — selected 4–7 of 18, acted 2–3 | I wrote the acceptance; it was satisfiable by construction. The apex question ("is this the right evidence?") applies to the commissioner's own acceptance bar, before dispatch, not only to the builder's result. Rule: for a numeric acceptance, say in the packet what value would be *surprising*; if none would, the number is a smoke test, not evidence. Step kept; the bar for D3b already has this form (S-G4: same nodes, different precedence, different score — and "none exists" is a reportable finding). Cost ~25 min |
| 11 | 2026-08-30 | D3a build packet refused by the builder: two acceptance conditions contradicted the artefact | codex-22 stopped with no files changed and named both contradictions with line pointers (old closure contains `:no-pattern`; `distinct` makes the acting count 1 against 2 fallback rounds). Both verified here by replay. Corrected acceptance authorised and re-dispatched | Third commissioner-side acceptance defect in a row (rows 9, 10, 11): "eight" scenarios, a tautological recall bar, and now two conditions unsatisfiable together. Common cause: I write the acceptance from my memory of the artefact. **Rule added:** before dispatch, *run the acceptance against the current artefact* — it must be satisfiable, and it should fail on the pre-change data for the stated reason; if I can't run it, I say so in the packet. Refusal-as-deliverable worked for the third time; the cost of a wrong bar was ~4 min of agent time instead of a built-to-spec facade. Cost ~15 min |
| 12 | 2026-08-30 | packet 3 (spider fixes + fleet driver) reviewed by re-running gates and by reading the *evidence records* behind the new edges, not only the attestations | All four fixes present and gates green; the acceptance number (rung-1 recall 22/23) was met and honestly labelled as index coverage. Reading the records: two of three new edges rest on `context-retrieval` events — embeddings rankings listing both ids — and retrieval listings supply 18 of 26 aif rung-1 hits. Also: a section's `:done` was inherited from the pilot's state file; its 20 absences were produced by the broken route and are stale | Two lessons. (a) A rung is a property of the *record kind*, not only of the match: exact occurrence in a similarity listing is co-occurrence, and the instrument now manufactures rung-1 labels for it at scale — the fix goes in the index before the fleet runs, not in review after. (b) "done" must be dated against the instrument that produced it; absences produced under a route later shown broken are instrument-produced absences and must be re-run, not carried. Both caught only by opening the evidence records. Cost ~50 min |
| 13 | 2026-08-30 | D3b: the row-11 rule applied — the acceptance was run against the current artefact *before* dispatch | The staged clause "identical nodes, different precedence, different score" was unsatisfiable (changing the order changes who wins: nodes differ in all six scenarios); corrected to S-G4 as the paper states it, with the dry-run's numbers (G4: 3 vs −5) named as expected. The build met it exactly; review found nothing to fix | First time in this series a commissioner-side bar was caught before the builder saw it. Cost of the dry-run: one REPL form, ~2 min; cost of the three earlier misses: three round-trips. Rule kept as standing practice: a numeric or relational acceptance is run on the artefact first, and the packet states the expected value |
| 14 | 2026-08-30 | Joe asked why the text-search route was "broken"; the claim had been in the record since the packet-3 spec | The route works — an exact-id query returns records containing the id. The sentence was my inferred explanation of a count mismatch (22/23 ids in the export vs 20/23 pilot absences), written without querying the route; the pilot receipts show the seats never queried an id at all (0 of the writing-coherence queries), so rung 1 was never performed, not defeated. Record corrected in three places; the re-run decisions stand on the corrected reason | Same shape as row 6 (a negative recorded without naming its instrument), one level up: a *cause* recorded without the test that would establish it, then repeated in four later reports as settled. Rule: a causal claim in a packet or record names the probe that established it (command + result), or is marked "inferred, untested". Caught by the operator's surprise, not by the review gate — which means the gate does not yet cover the commissioner's own explanations. Cost: ~15 min to test and correct; the claim had been repeated for ~1 h |

Next use: not a build packet. The first problem record, written by Joe, for
one July case or one open family — and the facade test on it, logged in §4's
two lines. That is the first observation this document can be judged by.

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
