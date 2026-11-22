# M-implementation-reconsidered — ways of working as programs: chains, rules, and the record of what happened

**Status:** IDENTIFY (opened 2026-11-22 by claude-5, at Joe's request).
**Owner:** Joe (operator); claude-5 drafting.
**Repos:** `futon4` (this mission; the grid it grows from,
`holes/labs/M-arxana-incubator/GRID-peeragogy-problems-x-futon.md`), `futon7` (the
Daily Scan and its briefs), `futon3c` (`holes/FLOPS-futonic.md`).
**Related:** [M-any-key-ctrl](M-any-key-ctrl.md) (CTRL moves over the same records),
[M-arxana-incubator](M-arxana-incubator.md) (the application and its defproject),
[M-arxana-together](M-arxana-together.md) (per-word provenance, the first working
ledger).

## HEAD

### Operator-voice anchor

Joe, 2026-11-22: "So, I agree w/ the MAP, we should create a mission for it to live
within. M-implementation-reconsidered could be a good title." Earlier the same day:
"what I've been calling 'economics' might be more clearly understood as 'policy' ...
we're not talking about calculating supply and demand curves ... whereas we are talking
about things like how choosing one particular 'way of working' makes a difference to
outcomes, and how one way of working or business model might connect up with or depend on
another one, forming supply chains"; "if you combined supply chain as a kind of data
structure (like a Clojure map) and IG as a kind of processing language (a bit like the
'do' engine but also including 'do not' and maybe also useful to upgrade with a
probabilistic mode), we'd be able to express a lot of what goes on 'as programs' like our
defproject writeup"; "what we've found is likely the kind of modular toolkit that we'd
need to build something that work[s] well."

### What is already felt to be true

- Pressman and Wildavsky's *Implementation* (1973) studied how plans made in Washington
  came apart in Oakland; Triangle Cubed is based in Oakland.
- A modular toolkit exists in pieces (grid file, "The toolkit, as found"): chains as data
  (ValueFlows / REA), rules (Institutional Grammar 2.0), a probabilistic mode (model
  checking), co-design with the people modelled (Cormas, companion modelling), FUTON's
  records (provenance, marks, jobs, benefit and harm columns), and a flops record.
- A first sketch ties them together for Triangle Cubed's "ownership follows recorded
  work" (`~/notes/claude-5/incubator-defproject.clj`, end).

### Historical reference (Joe, 2026-11-22)

> Jon Agar's *The Government Machine* argues that the British civil service was a
> computer before computers, made of rules, records and clerks. So you could set Kafka
> next to Agar: one wrote it as fiction, the other as history.

(*The Government Machine: A Revolutionary History of the Computer*, MIT Press, 2003;
publication details from memory.) It is the precedent for this mission's claim: a way of
working is already a program, run by people on records; the question is whether writing
it down as one helps the people inside it, or produces Kafka's version.

### Anti-glibness discipline

A ways-of-working program is worth what its second belief is worth: that people act as
the rules say. Nothing counts as shown until it is checked against the record of what
happened, with the people modelled agreeing it describes them. "Everyone would just..."
plans fail the owner test (a bounded owner for each role) by construction.

### Carried-forward tensions

- Agar's and Kafka's machine: the same rules and records can serve the people in them or
  grind them. The difference is who can read, contest and change the rules (Ostrom's
  collective-choice level).
- Most of the toolkit is known from memory; MAP replaces that with what the sources say.

## IDENTIFY

### Motivation

Expert plans fail at implementation (the Oakland study; the unverified Mumbai water
story, FLOPS entry 4), and the people who study why are mostly off GitHub. FUTON already
keeps the kind of record they lack. No artefact yet joins a chain model, a rule language
and that record.

### Scope in

- MAP over the toolkit's sources (below).
- One worked case: Triangle Cubed's contribution and ownership rules, written as chain +
  rules, checked against a record.

### Scope out

- Supply and demand, market aggregates.
- Formalising whole legal codes (Catala and Lean-for-laws work exist; this mission is
  about policies in use, not statutes).

### MAP checklist

- [ ] valueflo.ws: the ValueFlows vocabulary; commitments vs events; who uses it
      (co-ops, distributed networks).
- [ ] REA (McCarthy, 1982) and ISO/IEC 15944-4: confirm what the model fixes.
- [ ] newinstitutionalgrammar.org and the InstitutionalGrammar GitHub organisation: IG
      2.0 components, the JSON schema, coding tools (ig-coder).
- [ ] Cormas docs and CIRAD's companion modelling: how models are iterated with
      participants; the serious-games extension.
- [ ] The Ostrom Workshop (Indiana University): whether it still runs, what tools and
      data it publishes (social-ecological systems resources).
- [ ] Implementation studies: Pressman and Wildavsky (1973); Scott, *Seeing Like a
      State* (1998); Problem-Driven Iterative Adaptation (Andrews, Pritchett, Woolcock);
      Ostrom on co-production (1996); Björkman, *Pipe Politics, Contested Waters* (2015).
- [ ] Agar, *The Government Machine* (2003): confirm the argument and the details.
- [ ] Settle or drop FLOPS entry 4 (the Mumbai water story) from a source.

### Completion criteria

- [ ] Every "from memory" in the toolkit table replaced by what the source says, with the
      source named.
- [ ] The Triangle Cubed sketch rewritten in the actual ValueFlows and IG 2.0 forms, and
      run over a sample record (ours, if Triangle Cubed has none yet).
- [ ] One group found that uses IG or companion modelling with local participants and
      writes up failed iterations, with a contact route through a person, not an agent.

## Concrete examples, both directions (Joe, 2026-11-22)

Joe: "some I think would say that 'Kafka's machine' had a concrete instantiation as the
Third Reich ... we should probably develop some concrete examples to illustrate why this
way of working has any potential benefit at all." No counterfactuals ("had Kafka had
FUTON"); cases where records, rules and the ability to contest them made a difference
someone else measured. All from memory below; each is a MAP item to check.

**Where records and rules did harm.**
- The Netherlands' population register in the 1940s: unusually complete records are
  often cited as one reason the share of Jews deported was far higher there than in
  France. The machine Agar describes, used for persecution.
- *Seeing Like a State*'s cases (Soviet collectivisation, villagisation in Tanzania):
  plans legible to the state, illegible to the people in them.

**Where records, rules and contest helped.**
- Ostrom's Los Angeles groundwater basins: users negotiated adjudication, a watermaster
  kept the records of pumping, and the overdraft was halted. Records, rules agreed by the
  users, and a monitor accountable to them.
- Brazil's condominial sewerage: designed block by block with residents; cheaper and
  maintained because residents co-produced it.
- Mondragon's cooperatives: each member's capital account records their stake, so
  ownership follows contribution over a working life; the nearest established case to
  Triangle Cubed's aim.
- Wikipedia: every edit recorded, every change contestable on its talk page.
- Our own, small: the ledger walkthrough (2026-11-21) found that our tool had erased part
  of Murch's credit, and the record let it be found and repaired.

**What separates the two lists, as a hypothesis.** In the harmful cases the records were
held by an authority over people who could not read or contest them; in the helpful ones
the people recorded could read the records, change the rules, and hold the monitor to
account (Ostrom's principles 3, 4 and 6). An example only counts for this mission if it
shows that difference with an outcome someone else measured.

- [ ] Check each case above against a source, keep or drop it, and add the measure.

## A working model of the co-op sector and peer production (Joe, 2026-11-22)

Joe: "what we could try to do (and it would be relevant to Triangle Cubed) is to make a
working model of the co-op 'sector' and open/peer production parts of the economy ...
#1 these entities are typically 'more open' about their finances and governance, so we
may be able to get our hands on data, and #2 they are probably less well studied than
the Fortune 500 (or Fortune 4) companies that folks in business schools take as case
studies".

Candidate open data, by kind (all from memory; each a MAP item to confirm, with terms of
use):
- **Ledgers in the open.** Open Collective: each collective's income and expenses are
  public, with an API; close to a ValueFlows event stream already.
- **Co-op sector.** The International Co-operative Alliance's World Cooperative Monitor
  (the largest co-ops by turnover, yearly); Co-operatives UK's annual co-op economy
  figures; Mondragon's annual reports.
- **Employee ownership (US).** Retirement-plan filings (Form 5500) cover employee stock
  ownership plans and are public; the National Center for Employee Ownership publishes
  summaries.
- **Peer production.** Wikimedia: complete edit dumps plus public financial filings;
  Apache Software Foundation: public board minutes; Debian: public votes under its
  constitution; open-source contribution histories (GitHub archives).
- **Nonprofit filings (US).** Form 990s, searchable through ProPublica's Nonprofit
  Explorer, for foundations such as Wikimedia and Mozilla.

For each entity the model has the same three parts as the Triangle Cubed sketch: the
chain (ValueFlows), the rules as stated (bylaws, charters, constitutions, as IG
statements), and the record of what happened (finances, votes, contributions). The gap
between stated rules and record is the espoused / in-use comparison, now with outside
data.

First step, to keep it small: one of each kind, all with open records: an Open
Collective (ledger), Apache (minutes and votes), Mondragon (annual reports). Then ask
which questions the three can answer together that a Fortune 500 case study cannot,
such as how ownership or credit follows contribution over time.

- [ ] Confirm each source, its access route and terms; pick the three.
- [ ] Encode one entity end to end: chain, rules, record, and the gap between them.
