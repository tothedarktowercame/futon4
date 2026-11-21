# Grid: the problems peeragogy is used for, against the futon repositories

**Date:** 2026-11-21. **Author:** claude-5, from a conversation with Joe. **Status:** a
first draft to argue with; cells are readings with a pointer, not verified claims.

Joe, 2026-11-21: "start to arrange these problems into kind of a grid because ... if we
think about the actual problems that are being solved at the level of each futon
repository, those are going to recur ... [FUTON is] about how can you build inquiry
processes that are shared with people so, you know, if you call that a learning group,
that's fine if you call it a business, that's also fine."

**Rows.** R1–R6 are what authors who use peeragogy say it is for: 178 problem sentences
from 195 abstracts (OpenAlex, search "peeragogy", 2026-11-21; reading in
`~/notes/claude-5/peeragogy-what-problem-2026-11-21.md`). R7 is not from that
literature: it is the need the Peeragogy flops kept meeting (evidence and accounting;
flops 1, 3, 4) and the one FUTON adds.

**Columns.** One per repository, from its README. A blank cell means no relation found,
not a gap to fill.

| | R1 authority over what is learned moves to learners and peers | R2 learning where the teacher is not the only source | R3 skills a lecture cannot give (collaboration, critical argument) | R4 scale: peers teach peers when teachers are few | R5 the facilitator's own development | R6 human–AI co-learning: consensus is not validation | R7 a shared record of the inquiry: what was learned, who contributed |
|---|---|---|---|---|---|---|---|
| **futon0** (install, the way in) | | | | INSTALL.md: others can run the core themselves | | | |
| **futon1b** (evidence store) | | the record is a source anyone can query | | | | | every turn and act stored, attributed |
| **futon2** (AIF lab, War Machine) | next action chosen from evidence, with Joe's assessment, not a ruling | | | | learned state changes from outcomes | Lean certificates: a claim must check, not just be agreed | |
| **futon3** (pattern library) | anyone writes a pattern; IF/HOWEVER/THEN/BECAUSE | patterns as a reference beside the teacher | the flexiarg form is an argument with its defeaters | a pattern carries a move to someone who was not there | PSR/PUR: patterns retained or revised from use | | patterns cite the work they came from |
| **futon3a** (pattern sidecar) | | guidance at the point of need, without a teacher present | | | | | |
| **futon3b** (gate pipeline) | | | | | | gates: a step passes on a check, not on approval | |
| **futon3c** (Agency: agents, bells, ArSE, turns) | operator turns marked by intent (象); agents propose, the person disposes | ArSE: agents and people answer each other's questions | author ≠ reviewer on every coding handoff | Codex seats take coordination labour (flop hypothesis p1) | | warrants and the test registry: a record, not a rerun or a vote | commit trailers name the agent, job and dispatcher |
| **futon4 / futon4a** (Arxana: hypertext, incubator form, Together) | layers and forks keep each person's revision; the owner picks | scholia: commentary sits beside the text it answers | several people write one document at once, every word attributed | | | | per-word provenance: written by Murch, kept by Joe |
| **futon5** (meta-evolution, wiring diagrams) | | | | | | a wiring diagram checks structure before code exists | |
| **futon6** (mathematics dictionary) | | a reference whose entries carry their derivation | informal and formal arguments side by side | | | | |
| **futon7** (Markov blanket: sensing, business) | the Daily Scan's question mode: Joe's verdicts steer the scan | | | | the daily loop learns from those verdicts | | benefit lines and named beneficiaries (L-7) |

## First readings of the grid

- R7 has an entry in most repositories and R1 in several: FUTON's centre is a shared,
  attributed record of an inquiry whose direction the participants keep. That is Joe's
  "inquiry processes that are shared with people", whether the group is a learning group
  or a business.
- R6 is FUTON's own contribution to the peeragogy problems: in human–AI co-learning,
  agreement among agents is not evidence. The 2026 paper "Oblique Peer Review: Extending
  Pyragogy Design Patterns ..." (OpenAlex W7163506477) reaches the same point from the
  peeragogy side.
- R4 and R5 are thin. Scale (R4) is where the flops' hypothesis p1 sits ("coordination
  labour costs less"), and it has one honest entry. The facilitator's own development
  (R5) is served only by loops that learn from Joe's verdicts.
- The literature's R2 and R3 are where peeragogy is strongest and FUTON's entries are
  mostly infrastructure; a workshop with the Peeragogy group would show whether those
  cells mean anything to them.

## Next

- Ask the testbeds which rows are theirs (rq1, "on paper, before FUTON is shown").
- Read the same way for Triangle Cubed's literature (employee ownership, co-op
  incubation) and add its rows; a row both share is a candidate for rq3.

## Needs, and two levels of problem (Joe, 2026-11-21)

Joe: "do you know that Max Neef table of human needs? ... if we were thinking about like
collective problems, probably there's collective acts like understand ... that's what a
process of shared inquiry is about so maybe we should hypothesize that there are
individual level problems and then there are also collective level problems and
potentially what we're talking about here can help with both or help bridge between
them".

Where Max-Neef was used before: futon3c `holes/missions/M-aif-over-the-operator.md`, a
keyword tree from Beer's VSM × Max-Neef, elaborated by three agents (`rnode-tree/`), to
detect which part of the operator loop a turn is about; held-out detection agreed with
turn bids 39% against 22% by chance.

Max-Neef's matrix has nine needs (subsistence, protection, affection, understanding,
participation, leisure, creation, identity, freedom) across four modes (being, having,
doing, interacting). Two of its ideas fit here:
- The **interacting** column is already the collective level: the settings in which a
  need is met together. For understanding, the doing column lists investigating,
  studying, experimenting, educating, analysing; the interacting column lists schools,
  groups, communities. Shared inquiry is understanding met in the interacting mode.
- **Satisfiers** differ from needs, and some are synergic (meeting several needs at
  once) while others are pseudo-satisfiers (giving the sense of a need met without
  meeting it). Peer learning is a candidate synergic satisfier (understanding,
  participation, identity, creation). Agreement among agents taken as validation (R6) is
  a pseudo-satisfier of understanding.

Rows read as needs (a reading to test): R1 freedom, participation; R2 understanding;
R3 understanding, participation, creation; R4 participation; R5 understanding, identity;
R6 understanding (guarding against a pseudo-satisfier); R7 identity (recognition),
protection (credit), participation.

**Hypothesis (to test, not assumed).** Each row has an individual form (a person's
learning, credit, say over what is next) and a collective form (a group's shared
understanding, its record, its rules). FUTON's acts are individual: a turn, an edit, a
mark. Its records are collective: the evidence store, the document, the pattern. The
bridge is the step where individual acts become a collective state that each member can
still trace back to themselves (per-word provenance is one instance).
Refuted if: in the testbeds, the problems people name are only individual (or only
collective), or the records do not change what the group knows or decides.

## Comparison case: Triangle Cubed against Peeragogy (Joe, 2026-11-21)

Joe: "did that ever come up with [Peeragogy] or do we care about this? Or maybe there's a
whole other set of problems from [Peeragogy] that if Murch isn't able to solve, his
triangle cubed thing isn't going to work very well". Murch's stated aim (application;
video intro): people control the data their AI agents use, understand it, and can trace
which contributions go into which things.

**Did it come up in Peeragogy?** Contribution tracing did, twice, and both times the
idea outlived the attempt (`~/notes/peeragogy-flops/ARGUMENTS.md`):
- Flop 3, Kickstarter 2012: "Contributions to a free/open project can be accounted for
  in money and time together"; "for people who contribute time and effort, some
  compensation may be needed". It failed on demand, not on the accounting, which now
  exists as infrastructure.
- Flop 5, Wikimedia 2013: the learning-patterns idea was taken up inside WMF while the
  grant was not funded; "the value went into a commons without returning to the people
  who contributed it" (the WMF part is from memory, to check).
Control over data did not: in the 195 abstracts, one sentence (2024) names data privacy,
as an AI-ethics challenge. That part of Murch's aim is new relative to Peeragogy.

**Peeragogy problems Triangle Cubed inherits:**
- Why people stay and why they leave (flop 6): unsolved; "the bonds of friendship" was
  asserted, not tested, and the leavers were never asked. In a co-op, a leaver takes or
  forfeits a share, so the question has a price.
- The hard roles (flop 6: Wrapper, Heartbeat, Newcomer): coordination labour, and
  whether a roundup written by an agent carries the social signal of being named ("our
  names had been mentioned"). Tracked contribution is a version of that signal.
- Demand (flop 3): accounting with nobody asking for it.
- Carry-back (flop 5): value leaving a group without returning.

**Reading.** In Peeragogy, credit was recognition with nothing at stake. Triangle Cubed
attaches a stake: ownership follows recorded work (hypothesis t1). That makes the
tracing worth more and its errors cost more, and it moves credit from the individual
level (my contribution) to the collective one (who owns, who decides), the bridge in the
two-level hypothesis above. It also makes t2 ("recording costs members little") harder:
recording that people feel as surveillance is a violator in Max-Neef's terms, not a
satisfier.

## A second apex question: how do groups solve collective problems? (Joe, 2026-11-21)

Joe: "maybe there's another level of an apex thesis, which is more like how do humans
solve collective problems ... through a process of active inference that's not only
active inference, but is also effective ... we could just make a list of big problems
... the UN has that big list ... which, if any, [how] many of those can futon help with,
and if it can help with a lot of them, that's great, and if it can help with only a few,
that's also really useful to know".

The list: the UN Sustainable Development Goals, 17 goals and 169 targets (UN Statistics
Division SDG API, fetched 2026-11-21; saved in
`~/storage/arxana-incubator/research/un-sdg-goals-targets-2026-11-21.json`). The COP
conferences cover climate only (goal 13).

FUTON could only help with a target through the way a group pursues it (knowing,
deciding, coordinating, accounting), not with the material outcome itself (water,
yields, emissions). So the first cut is which targets name a collective process.

Rough keyword pass (participation, accountability, transparency, access to information,
knowledge, capacity, partnership, cooperation, education, data, monitoring,
decision-making, institutions): **56 of 169 targets**. Densest in goal 17 (11 of 19),
goal 4 (8 of 10), goal 16 (5 of 12), goal 13 (4 of 5). Examples: 16.7 "responsive,
inclusive, participatory and representative decision-making at all levels"; 16.6
"effective, accountable and transparent institutions"; 17.6 cooperation on and access to
science, technology and innovation, and knowledge sharing; 4.7 learners acquire what
they need to promote sustainable development.

That pass over-counts (a keyword is not a process) and under-counts (process targets in
other words). Next: read all 169 against a rubric (material or process; if process,
which grid row R1-R7 and which Max-Neef need; individual or collective level), then
count. Refuted for FUTON if the process targets are few, or none maps to a row where
FUTON has more than infrastructure.

### Owned by everyone, or by nobody (Joe, 2026-11-21)

Joe: "you might think, that a collective problem would be something that everybody owns
but sometimes you could find that with collective problems, they turn out to be things
that nobody owns ... Climate change there's a commission ... but do they own that
problem? Do they own it enough that they could make a climate change futonic mission and
actually solve that problem? I kind of doubt it".

A futonic mission needs an owner (lifecycle IDENTIFY: "Owner and dependencies"), and
Ostrom's first principle for a commons is a group with clear boundaries. Ostrom's answer
for climate was polycentric: many bounded groups each owning a part, not one body owning
the whole ("A polycentric approach for coping with climate change", 2009/2010, from
memory; check). Benefit line C4 says the same at small scale: "route to the individual
who owns the problem".

Our own stack has the nobody-owns case: in `~/notes/commons-map/placements-who-acts-summary.md`
(2026-11-02), two work items are close calls because "its ruling names no one"
(E-R2-red-ring-fill) and "the stop-the-line names no one who lifts it"
(M-G-over-cascades).

Rubric field for the 169-target reading: **owner** — is there a bounded group that could
own a mission on this target, or a part of it? (none named; named but not bounded, as
"all countries"; a bounded group). A process target with no possible owner is one FUTON
cannot help with as a mission, however well it fits a grid row; it may still help a
bounded group own a part.

### Owning parts, and orchestrating the owners (Joe, 2026-11-21)

Joe: "they can own parts of the problem ... they could own monitoring climate change,
for example that's a much easier thing to own ... I'm a sensory organ that's able to
ascertain that problem so maybe the issue with futon is how to develop orchestrations
of people ... the owner could be Claude 5 ... and the orchestration of people that own
the aspects of the problem is what actually has to work".

In active-inference terms a collective problem has roles: sensing (monitoring),
modelling (understanding what is going on), acting, and keeping these connected. Beer's
VSM, already crossed with Max-Neef in M-aif-over-the-operator, names the same roles for
an organisation (operations, coordination, control, intelligence about the environment,
policy). Each role can have a bounded owner even when the whole problem has none. The
owner of the whole then does not need authority over the parts; it keeps them connected
(benefit line K10: "small contributions plus an integrating function that stitches them
together").

The stack already works this way on a small scale: on 2026-11-20/21 fable-1 owned the
notes and provenance records, Codex seats built, claude-5 reviewed and orchestrated, and
Joe decided. Whether that orchestration works for people who are not agents is the open
question, and the one the testbeds can answer.

Rubric, revised: for each process target, the roles (sense, model, act, connect), and for
each role whether a bounded owner exists (none / named but unbounded / bounded). The
count that matters: targets where every role but "connect" has a possible bounded owner.
Those are the ones where orchestration is the missing piece, which is what FUTON offers.

### The marks as actions in an action situation (Joe, 2026-11-21)

Joe: "think about our [proforma] marks ... some 20 odd individual acts like explore or
defer or withdraw ... actions that people may be able to take relative to some of these
problem facets ... rather than just ... the way a dialogue works we could actually say,
well let's articulate the way an action situation works ... in these ownerless
situations, nevertheless, actions still take place for good or for bad and how do we
sense make those? And then how do we change them? ... if you look at [馬], the agent
horse, it is actually starting to ... make a model of how that conversation changes
over time".

`~/notes/institutions-patterns/CROSSWALK-IAD-AIF.md` (2026-10-29) already maps Ostrom's
seven working parts of an action situation onto an active-inference model: participants,
positions (which policies a role may consider), actions (the policy space), control (the
transition model B), information (A, who observes what), outcomes, costs and benefits
(C). The marks fit the **actions** slot: the repertoire of acts in a situation. Several
are already acts that only some positions should make, which is what Ostrom's position
and choice rules govern: 🈲 constrain, ㊯ delegate, 🈸 ask-action, ㊣ approve,
🈡 withdraw, 🈝 defer. And 馬's model of how a conversation changes over time is a
learned transition model, B.

An ownerless situation, in these terms, is one where acts keep arriving (propose,
report-problem, ask) but no participant holds a position whose acts close anything
(approve, decide, withdraw, delegate to someone who accepts). That can be read from the
marks, not argued: our own record has it. The minus40 session brief lists "open asks
with no answer linked", twelve of them from 2026-11-03 to 2026-11-20 on 2026-11-21; each
is an ask in a situation where nobody's position answered it.

Candidate test, on our own data first: for each item or thread, the sequence of marked
acts and the positions that made them; flag situations with asks and proposals but no
closing act by anyone. Then, for the ones that matter, change the situation (give a
position the closing act, or route the ask to someone who holds one, which is
`※ q ? w`), and see whether the flag clears.

### Every act has beneficiaries and dis-beneficiaries (Joe, 2026-11-21)

Joe: "acts in general, probably have both beneficiaries and ... dis-beneficiaries ... if
I buy some food at the store ... someone had to grow that food and there's this whole
chain of benefit and harm ... data centers emit pollution which causes people health
problems ... by using AI agents, I'm ... outsourcing some harm to people in that area
... does it solve as a zero sum or does it solve as a positive trajectory?"

Our own accounting so far has one side. The 2026-11-08 readings found benefit claims for
84 items and "trouble avoided" for 128, and every harm named is a harm to the stack or
to Joe (the H trunk: wrong records, unseen failures, Joe having to nudge, stale state,
wasted compute, unreadable models). None names a harm to anyone outside: not the people
near the data centres our agents run in, not anyone whose work the models were trained
on. (Checked by search: no row mentions energy, emissions or pollution as a cost.)

Mozilla's call names this too: it is "not looking for systems that replicate the ways
that the largest AI companies and systems operate, in terms of scale, user agency,
privacy and resource use".

What is measurable now, for our own acts: compute per act. Agency job records and the
subscription usage readers give tokens per job; an energy and emissions estimate per job
would need a stated per-token figure with its source and its uncertainty, and the data
centre locations are not ours to know (the provider's). That is a dis-benefit column we
could fill honestly for every act, with the caveat written beside it.

Zero-sum or a positive trajectory can only be judged with both columns. One testable
form: over time, does the harm per unit of benefit fall (smaller models, fewer wasted
runs, work moved to classical code), and do the people bearing the harm get any of the
benefit? If the first rises or the second stays at nothing, the trajectory is not
positive, whatever the benefit lines say.

### Nobody owns the specification (Joe, 2026-11-21)

Joe: "supposing we were able even just to well specify them ... we were able to well
specify the APM problems ... Rob ... has been able to well-specify other mathematical
problems ... AI agents are very good at solving problems, on paper, if they're
sufficiently formalized so maybe that's one of the issues that no one is actually owning
the specification of these problems".

That adds a role before the others: **specify**. Sense, model, act and connect all
presuppose a statement of the problem that can be checked. Evidence from our own work:
- The APM problems became workable once each had a formal statement in a registry; the
  War Machine and the lemma seats work from those statements.
- Specifying is itself where things go wrong: the 2026-10-13 audit found statements that
  were vacuous or narrower than the problem, and Joe's rule since is that a narrower
  statement does not count (memory: no-narrower-stand-ins). An owned specification has
  to answer to the people who have the problem, or it drifts into a problem that is
  easier to solve.
- Ostrom has a grammar for specifying collective action:
  `institutions/rules-as-institutional-statements` ("ATTRIBUTES of participants who are
  OBLIGED, FORBIDDEN, or PERMITTED to ACT ... under specified CONDITIONS, OR ELSE"). A
  futonic mission's completion criteria are the same kind of thing for one group's
  problem.
- The SDGs have a partial specification already: each target has indicators (the global
  indicator framework), owned by the statistical community. That specifies the sense
  role (what to measure), not the problem (what would count as solved, by whom).

Rubric, revised again: for each process target, add **specify**: is there a statement a
group could check (prose target / measurable indicator / formal), and does anyone own
keeping it faithful to the problem? Hypothesis: for many collective problems the first
missing owner is the specification's, and an agent can hold the drafting while the
people who have the problem hold the "is this still our problem?" check.

**Correction (Joe, 2026-11-21): specifying is an act within belief, not a role before
it.** "a specification is only really a statement of belief ... if I say, here's the
specification of Fermat's Last Theorem in Lean, whether or not I have a proof of that, if
I say sorry, and we all agree ... that that's a statement of Fermat's Last Theorem, then
that's a statement of belief". So **specify** belongs inside **model** (the group's
beliefs), not ahead of the roles. A specification carries two beliefs that are settled
differently:
- that the statement is true: settled by a proof (or refuted), as for any theorem;
- that the statement *is* the problem: settled only by agreement among those who hold
  it. The 2026-10-13 APM audit checked this second belief, and found it wrong for some
  statements (vacuous or narrower).
For collective problems the second is the one nobody owns: a shared belief about what
the problem is, made explicit enough to check. Rubric: under model, record whether such a
statement exists and who has agreed that it is the problem.

### Atomic acts and complex processes (Joe, 2026-11-21)

Joe: "specification is actually ... a fairly complex, multi-sided act, it's not
necessarily something you can just write down in the same row as, say, defer ... you just
say, I'll do this later whereas specify, as we've seen with the war machine is a
progressive process that takes place ... over many iterations ... processes which are
complex are ones which need a specification in the first place".

So the marks are atomic acts (one turn: defer, approve, withdraw), and specifying is a
process made of many of them over time (propose, qualify, disagree, retract, propose
again, approve). Ostrom's levels fit the difference: operational acts happen inside the
rules; collective-choice acts make the rules the operational acts follow; constitutional
acts decide who makes those rules (`~/notes/institutions-patterns/CROSSWALK-IAD-AIF.md`,
"Levels and context"). Defer is operational. Specify is collective-choice: it fixes what
later acts will count as progress.

Working definition, to test: a process is complex when it cannot finish in one act and
several participants must agree on what would count as finished. Those are the ones that
need a specification first, and the ones nobody owns by default.

Test on our own turns: a specification process should show as a run of marks on one
object across many turns (qualify, disagree, retract, propose) ending in an approve or a
withdraw. 馬's turn data and the marks could find these runs, how long they take, and
which never close. The War Machine's statement history is a second record of the same
kind.

## Where this leads (Joe, 2026-11-21)

Joe: "effectively no one outside of mathematics really cares about [AI agents solving
maths problems] whereas ... a lot of people care about things like their income or
whether their kids are going to have a future ... one of my views with the HyperReal
Enterprise is basically ... that we could do what [Murch] has been talking about with
tracking contributions to beneficial products, but we could track contributions across
all kinds of supply chains and as well as track contributions to harms and see if we
could reduce them, not just produce more benefits ... if mathematicians can solve
complex problems, ... maybe we can also solve complex economic problems it's just going
to need a slightly different approach".

Thesis, as it stands after this conversation: complex collective problems, economic ones
included, can be worked the way the APM problems were (specify, then solve), provided
that (1) the specification is held as a belief the people who have the problem agree to,
(2) each part (sense, model, act) has a bounded owner and something keeps the owners
connected, and (3) every act's contributions are traced to both its beneficiaries and
its dis-beneficiaries, so that one can see whether harm per unit of benefit falls.

The "slightly different approach" relative to maths: there is no proof that settles the
second belief (that the statement is the problem); the people in the problem change as
it is worked; and benefits and harms fall on different people.

A first case we could do end to end, because we hold its whole record: the Mozilla
application itself, as a product. Contributions: per-word provenance in the incubator
form (Joe, Murch, Rob, the Peeragogy call, agents), commit trailers, the call text from
Mozilla. Harms: compute per agent job (tokens, with a stated energy figure), and anyone
whose words were used without being credited. If the ledger cannot be built honestly
for a product this small and this well recorded, it will not work for a supply chain.

### Proofs about policies, with statistics in them (Joe, 2026-11-21)

Joe: "somebody ... said he was working on specifying all the world's laws with lean ...
laws are like ... a language layer over ethics ... but what about ... a broader language
of actions and reactions? ... if we did enough of that, we could start to build something
approaching formalizable models ... those proofs wouldn't just be proofs of facts about
the world they'd be proofs about facts about policies about the world and ways of action
... if people act this way, then on average that will happen ... proofs with some
statistics built in".

Such statements already have a formal home: probabilistic model checking proves claims
of the form "under policy π, the probability of reaching outcome O is at least p" over
Markov decision processes (tools such as PRISM and Storm; named from memory). Active
inference scores policies the same way, by expected free energy. In our stack, the War
Machine's Lean-led route (memory: wm-lean-led-approach) is already a proof about a policy:
that the selection follows its stated model.

The two beliefs from "specifying is an act within belief" return here, one level up:
- that the statement holds of the model: settled by proof or model checking;
- that the model is how people actually act: settled only by observation and by the
  agreement of those modelled, and liable to change once people know the policy
  (they react to it).
So a proof about a policy is worth as much as the second belief, and the record of acts
(marks, provenance, benefits and harms) is what tests it.

### Multi-party acts as hyperedges (Joe, 2026-11-21)

Joe: "using these hypergraphs to think about actions which are multi-party actions so,
for example, coordinate ... might seem like you would have one party who does that, but
of course they need other parties to do it with so you have a multi-party the action,
which is not strictly non-hierarchical".

A mark today has one party, the speaker. Many acts are only complete when others act:
ask needs an answerer, delegate needs someone who accepts, approve answers a proposer,
coordinate needs the parties coordinated. As a hyperedge, the act has typed ends (roles),
and the roles can be asymmetric, which is Joe's "not strictly non-hierarchical": a
coordinator and the coordinated are both needed, but not interchangeable. The GCC already
stores hyperedges with typed ends (`:hx/ends` with `:role`, as in the mission-scope
edges: entity, environment, target-pattern).

Shape, to test:
- an act-hyperedge: `{:act :coordinate, :ends [{:role :initiator ...} {:role :party ...}
  ... {:role :object ...}], :requires #{:initiator :party}}`, plus optional beneficiary
  and dis-beneficiary ends (the benefit/harm columns);
- the act is complete when every required role has a filler who acted; until then it is
  open, which is the ownerless case at the level of one act;
- Ostrom's aggregation rules ("how individual actions compose into the joint B" in the
  IAD-AIF crosswalk) say what the joint act does once complete.

Instance already running: a bell and its park form a two-party act (asker, asked), with
the job id as the hyperedge and the auto-bellback as the second party's act. An open
ArSE question is the same act with the second role unfilled.

## Twelve roles: the Gauntlet party at Ostrom's three levels (Joe, 2026-11-21)

Joe: "if we could multiply the fictional characters out by ... a factor of three each ...
we'd have 12 factors ... of governance or ... guidance ... rather than looking at
abstractions of theories maybe we should think about what actual, real, implemented
human architecture would make something like triangle Cubed or the [Peeragogy] Project
work really well for its stated aims ... like 12 hires ... who exactly would we want
working with us? ... to move past this brain-body barrier and take real action in the
world".

The four characters as already defined for Giraffe's orderings (M-giraffe, 2026-11-10):
Thor unblocking, Merlin strategy, Questor quick reach, Thyra harm reduction. The factor
of three used here is Ostrom's three levels: operational (doing the work), collective
choice (making the rules the work follows), constitutional (deciding who makes the rules).

| | operational | collective choice | constitutional |
|---|---|---|---|
| **Thor** (unblocking) | builder: keeps the tools working | integrator: stitches contributions into one product, decides what merges (K10) | steward of openness: licence, forkability, the right to take the record elsewhere |
| **Merlin** (strategy) | scanner: finds what is known, who else works on it (Daily Scan, literature) | specification holder: keeps each problem statement faithful and agreed (the missing owner found above) | evaluator: tests whether the approach works (rq1–rq3; the apex questions) |
| **Questor** (quick reach) | facilitator: Peeragogy's Wrapper and Heartbeat, weekly contact | newcomer guide and translator: onboarding, other languages (the Indonesian teacher educators) | connector: brokers between groups, funders, clients |
| **Thyra** (harm reduction) | monitor: keeps the contribution and harm ledgers accurate (Ostrom: monitoring) | mediator: disputed contributions, members who leave (flop 6; Triangle Cubed's first test) | ethics and consent: privacy, people records, data control (Mozilla's "Intentional") |

First reading, from who holds these roles now (people named in this conversation only):
Joe holds most of Merlin and some of every column; Rob is Thor and Merlin on the formal
side; Murch is Questor at the constitutional level (talks with CEOs) and the source of
Triangle Cubed's aims; Charlie's card says "connector". Agents already hold parts of the
operational column (builder, scanner, the ledger-keeping half of monitor). **The Thyra
row has no human holder.** That matches the finding above that our harm accounting names
nobody outside the stack: nobody's job is the harm side.

Roles are not hires one-to-one: one person can hold several cells, and agents can hold
operational cells under a person who answers for them. The open question for "who
exactly" is the frames method again, for contributors rather than beneficiaries: find,
for each empty cell, the populations where such people are (e.g. institutional analysts
around the Institutional Grammar tools found by the 2026-11-21 scan; co-op mediators;
research-ethics people in the reproducibility networks).

### Thyra faces two ways: self-healing inside, the niche outside (Joe, 2026-11-21)

Joe: "in this war machine, we had the concept at least of self-healing ... in a human
resources kind of department you would find the same kind of thing ... help people get
back on track or make the people who are bad actors go away ... but ... harm reduction,
not just in the context of the organism itself, but what does it do outside the world in
its niche ... even pandas ... do go around harming bamboo plants, which they eat".

So each Thyra cell has an inward face and an outward face:

| | inward (the organisation) | outward (its niche) |
|---|---|---|
| monitor | is the organisation working as it should? (the War Machine's watchdog and debugger) | what do its acts cost others? (the harm column: compute, uncredited words, pollution where the data centres are) |
| mediator | disputes and departures; getting people back on track, Ostrom's graduated sanctions before removal | the people outside who bear the harm: are they heard, do they get any of the benefit? |
| ethics and consent | whose data, whose record (people records, consent) | the limit: harm kept within what the niche can bear and renew |

The War Machine's rule for self-healing carries over (memory: medic-is-not-a-click,
wm-run-loop-until-joe-stops): one self-heal attempt, then outside repair, never a loop
that heals itself indefinitely. Ostrom's graduated sanctions say the same for people:
small responses first, removal last.

The panda: no organism has zero outward harm; it eats. The working test is the one
fisheries and Ostrom's commons use: harm within what the niche regenerates (a harvest
below the regrowth rate), and the niche getting something back. For the stack: does the
compute harm per unit of benefit fall, and do the people bearing it see any benefit (the
two questions already written above).

### Levels of benefit, and the stack as practice (Joe, 2026-11-21)

Joe: "other biological organisms which are beneficial ... the wasp and the flower ...
that fit together ... benefits have to be construed there relative to species-level
survival ... understand at which level these benefits apply ... how benefits work
across levels ... individuals benefit from their species-level survival as well".
The fig and its wasp are the textbook case: an obligate mutualism, each species'
reproduction depending on the other, with cospeciation along the lineages (the general
term is coevolution).

So each benefit (and harm) line needs a **level**: individual, group, population or
lineage, ecosystem; and some benefits cross levels (an individual benefits from the
group's survival; a group's rule constrains an individual for the group's sake). That is
the two-level hypothesis above, with more levels. Rubric field for benefit lines: level,
and whether it passes to another level.

Thyra's real professions span both faces and several levels: psychotherapy and medicine
(an individual, inward), mediation and HR (a group, inward), peacekeeping (between
groups, outward). Thyra also heals herself.

Joe: "if we believe in the framing of the futon stack relative to the meditation cushion
framing, then at least it should be beneficial for internal harm reduction because the
futon stack should help the people who use it understand themselves better ... even just
using the futon stack, if that is true, is a form of internal harm reduction because
it's a kind of form of meditative practice".

Candidate harm line, as a hypothesis: **H8, the user's own understanding.** Using the
stack as a reflective practice reduces the user's own harm (understanding one's own
work, decisions and patterns). Evidence today: one user, Joe, and his own account.
Refuted if users of the testbeds report no change in understanding themselves, or if the
practice costs more than it returns (time, attention, dependence on the tools). This is
an inward, individual-level benefit; the testbeds would also show whether it passes to
the group level (a group that understands its own process better).

H8, as Joe states it (2026-11-21), under the principle "come and see" (a benefit claimed
should be one others can inspect): "over the last year, my thinking around these issues
has gotten clear ... it's not just a matter of a kind of gratification ... that I've been
able to do things I always wanted to do that's more of a benefit of the underlying
technology with LLMs ... Futon has provided a little bit of leverage in that regard ...
these are like mundane benefits ... the more noble benefits have to do with my
relationship to myself through a sense of clarity and also ... humility ... around my
own ability".

Two benefits, attributed separately:
- mundane (doing long-wanted things): mostly the language models, some leverage from
  FUTON;
- noble (clarity; humility about one's own ability): claimed for FUTON's practice.
The counterfactual that separates them: would plain chat with a language model, without
the records, patterns and missions, have given the same clarity?

Come-and-see checks on Joe's own year of record (to run, not yet run): how long his
specification runs take to close now against a year ago (the atomic/complex test above);
how often his turns redirect or retract; how often credit goes to others or to the
system rather than to himself. None measures clarity directly; each is something another
reader can check against the record, which is what "come and see" asks.
