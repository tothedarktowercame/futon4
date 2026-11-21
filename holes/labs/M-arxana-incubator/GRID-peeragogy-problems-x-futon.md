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
