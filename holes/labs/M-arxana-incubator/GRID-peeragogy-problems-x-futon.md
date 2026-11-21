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
