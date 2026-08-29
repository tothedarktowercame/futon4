# Futonic Delivery Lifecycle (v2) — a delivery is a claim about a problem

**Status:** DRAFT v2, 2026-08-29 (claude-15, after Joe invalidated v1). Not
ratified. v1 is archived as `delivery-lifecycle.v1.md`; its validation log
(§7 below) is carried over as the record of why it failed.
**Gate:** operator-acceptance — Joe.

**One-line:** Work is commissioned to solve a problem. The problem — not the
deliverable — is the thing that has a status, and its status changes only when
an observation the commissioner named in advance is seen on a run. Everything
else in this document exists to stop a deliverable from acquiring a status of
its own.

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

## 1. The unit: a problem record

One file per commissioned problem, `<repo>/holes/problems/P-<name>.md`, or a
section in the mission that commissions it. Fields, in this order:

```
problem:      <the commissioner's words, verbatim — what is wrong now>
now:          <an observation on a run artefact that shows it: path + what is seen>
solved:       <the observation on the same kind of artefact that would show it solved>
facades:      <ways `solved` could be made true without the problem being solved,
               each with the observation that would expose it — see §2>
owner:        <one agent-id or joe>
status:       open | solved (observed <date> at <path>) | did-something-else (<what>)
deliveries:   <list; each says what it changed and which of now/solved/facades it affected>
```

`solved` is the only predicate any status is ever written against. It is the
commissioner's. A deliverer may propose a rewording; the commissioner accepts
or not; the deliverer may not write against their own.

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
3. **Rewrite `solved` until each facade would fail it**, or record the facade
   as accepted-and-observable ("a rename would pass this predicate; we accept
   that because γ's movement is checked separately at …"). A predicate that
   admits an unrecorded facade is not yet a predicate.

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
- **OBSERVE.** Someone runs the thing and looks at the `solved` artefact. Who
  is not constrained here — v1's different-method witness rule is *unproven*
  for the failure that occurred and is kept only as an option for count-shaped
  predicates (v1 §9 rows 1–2). What is constrained: the observation is on a
  run artefact at a path, and it is the commissioner's predicate, verbatim.
- **STATUS.** Written on the problem record only, by anyone, but only from an
  OBSERVE. Three values: `open`, `solved (observed …)`, `did-something-else`.
  The third is the important one: it is where B-2d, the rename, the whitelist
  and every "built (dark)" row would have gone, visibly, instead of "done".

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
