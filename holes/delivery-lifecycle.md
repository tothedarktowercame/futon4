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
governs nothing, whatever it is called. Rename pending Joe's choice; "delivery"
should not survive in the title.

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
