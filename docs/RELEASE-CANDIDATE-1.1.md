# Futon Stack — Release Candidate 1.1

The Futon stack is a personal knowledge and workflow system built around a persistent hypergraph store, an Emacs-based interface, and Active Inference modeling for AI agent behavior. This release candidate marks the first external-facing version.

## Architecture Overview

- **Futon0** — Monitors and logs workflow activity across the stack; manages backups
- **Futon1** — Persistent hypergraph store (Datascript + XTDB) with NLP ingest pipeline
- **Futon2** — Active Inference testbed using ant colony simulation
- **Futon3** — WebSocket/HTTP message bus connecting components, plus pattern system and AI agent wrappers
- **Futon4** — Emacs interface for browsing and editing everything

---

## Futon0 — Activity Dashboard

A customizable dashboard that extracts workflow signals from existing activity without requiring manual logging. Rather than asking you to keep a journal, it mines traces you already produce: git commits, file edits, audio recordings, database changes.

Think of it like a window manager widget tray — you populate it with whatever signals matter to your workflow. The current configuration monitors:

- Code edit frequency across repositories
- Pattern imports to the database
- Recent audio recordings
- Meaningful events surfaced from the hypergraph (affect words + new concepts)

**Key components:**
- Vitality scanner: `futon4/docs/docbook/futon0/futon0-3158c09682ff.org`
- Negative space notifier: `futon4/docs/docbook/futon0/futon0-6dba47cda404.org`
- Git activity aggregator: `futon4/docs/docbook/futon0/futon0-bd95298965f1.org`
- Systemd automation: `futon4/docs/docbook/futon0/futon0-b19b2b939d81.org`

**Known limitations:**
- Telemetry data stores are periodic JSON snapshots; real-time shared state is still pending
- Some automation hooks remain stubby (backup status checks, boundary-evidence debt)

---

## Futon1 — Hypergraph Storage

A persistent knowledge hypergraph (Datascript in-memory with XTDB durability) that stores entities, relations, and types. Hyperedges can connect more than two nodes, enabling richer relationship modeling than traditional graphs.

Includes an open-world NLP ingest pipeline (Stanford CoreNLP) for extracting entities and relations from unstructured text. Currently used for tracking AI dialogues; designed as scaffolding for future mathematical document ingest.

**Key components:**
- Hypergraph store with XTDB durability: `futon4/docs/docbook/futon1/futon1-a3475e4ba7df.org`
- Entity and relation model: `futon4/docs/docbook/futon1/futon1-f4229cfc8578.org`
- Model invariants and certificates: `futon4/docs/docbook/futon1/futon1-98dba414ca3d.org`
- HTTP API surface: `futon4/docs/docbook/futon1/futon1-12e78e8a1316.org`

**Known limitations:**
- Running multiple Futon processes against the same data directory can trigger RocksDB lock errors
- Open-world ingest treats questions as assertions; there is no question→Datalog mapping yet

---

## Futon2 — Active Inference Testbed

An ant colony simulation implementing Active Inference: predictive coding, expected free energy minimization, and precision modulation. Compares AIF-driven agents against classic algorithms on identical grid worlds.

This is not a research contribution to Active Inference theory — it's an application of existing theory to prototype agent decision-making. The simulation serves as a reference implementation before applying AIF to real AI coding agents in Futon3.

**Key components:**
- AIF core orchestrator: `futon4/docs/docbook/futon2/futon2-7cdf464d3bc1.org`
- Observation layer: `futon4/docs/docbook/futon2/futon2-ba6ecb7d3a67.org`
- Predictive coding: `futon4/docs/docbook/futon2/futon2-a4e9e17b7603.org`
- Policy selection: `futon4/docs/docbook/futon2/futon2-086f3c67e20c.org`

**Known limitations:**
- Formal semantics documentation incomplete
- White-space detection property tests missing

---

## Futon3 — Message Bus and Patterns

The nervous system of the stack: a WebSocket/HTTP message bus (MUSN) connecting all components. Also hosts the pattern system and AI agent wrappers.

**Patterns** are design documents broader than Gang of Four software patterns. They're used for structuring arguments (that AI agents expand into articles), specifying stack features, and general design reasoning. The system can check whether AI agents are following declared patterns.

**Agent wrappers** (fucodex, fuclaude, fubar) model AI coding agent behavior and human interaction using Active Inference principles developed in Futon2.

**Key components:**
- HTTP and WebSocket transport: `futon4/docs/docbook/futon3/futon3-9682d94bf2e4.org`
- Stack status telemetry: `futon4/docs/docbook/futon3/futon3-7356d322d5aa.org`
- Tatami session management: `futon4/docs/docbook/futon3/futon3-574ad9c946f8.org`
- Pattern hints system: `futon4/docs/docbook/futon3/futon3-b3603516f181.org`

**Known limitations:**
- Tatami evidence does not yet flow back into pattern updates or storage-backed history
- Checks are not yet tied into persistent storage
- Proof-state histories need clearer structure for external consumption

---

## Futon4 — Emacs Interface

The daily driver: an Emacs-based browser for docbooks (AI-generated documentation), patterns, and other artifacts stored in Futon1.

Implements **Arxana**, a Ted Nelson-style hyperedge system linking documents and code. Unlike Knuth's literate programming (code embedded in docs), Arxana keeps docs and code separate but richly linked via hyperedges. These can be:

- Direct offset markup (TEI-style annotations)
- Dynamic finders that locate and annotate text on the fly

Hyperedges persist as EDN in the Futon1 database.

Multiple HUD layers feed information into the interface:
- Futon0 vitality markers (activity signals)
- Tatami HUD (agent interaction annotations)

**Key components:**
- Docbook browser: `futon4/docs/docbook/futon4/futon4-c31cf2a7cb5c.org`
- Pattern library overview: `futon4/docs/docbook/futon4/futon4-a36b4f60c62b.org`
- Editing workflow: `futon4/docs/docbook/futon4/futon4-7bde4c62d6e8.org`
- Scholium authoring: `futon4/docs/docbook/futon4/futon4-1f2aeeba8966.org`

**Known limitations:**
- Inclusion/transclusion UX is still WIP
- Import for modern doc formats is not working yet (export works)
- QA is in progress; the "revived" release tag is not yet published

---

## Roadmap

### System Self-Description

The stack currently has documentation and APIs at various layers — Futon1's storage API with checked invariants, Emacs Lisp functions callable via emacsclient, HTTP/WebSocket endpoints in Futon3. But these exist as separate artifacts: code, docs, and schemas that humans must correlate.

A future goal is unifying these into a machine-readable manifest where components are first-class entities in the hypergraph. Patterns would describe not just design structures but system capabilities — what the message bus can do, what functions the Emacs interface exposes, what invariants each layer enforces. This would enable:

- Agents querying the system about its own capabilities
- Executable documentation that stays synchronized with implementation
- Self-verification that the running system matches its declared structure

This closes the reflexivity loop: a semantic network that includes a model of itself.

Post-release milestone: start the manifest with the evidence-backed map at
`futon4/docs/evidence/rc-1.1-evidence.edn`, then evolve it into a full
system self-description (components, interfaces, invariants) in Futon1.

### Futon 5 — Meta-Patterns and Cross-Domain Formalism

Where Futon3's patterns are domain-specific, Futon 5 seeks a core set of meta-patterns (~256) capturing common structures of improvisation across domains. Inspired by Christopher Alexander's principles of order, but grounded computationally in elementary cellular automata as control features.

Target domains include:
- Cellular automata control (improvisation over simulation dynamics)
- Cyber ants (design-pattern-driven agents composed via category theory, runnable in Futon2)
- Music, crowdfunding, and other far-flung applications

The formalism is: patterns → category-theoretic schema → code generation (via LLM or other methods). This enables executable documentation — the pattern composition *is* the specification that generates implementation.

Futon 5's machinery also applies reflexively: the stack itself becomes a domain, enabling ongoing self-documentation as the system extends.

A key milestone: demonstrating that patterns developed in one domain actually transfer to another — not just that they can be written, but that they work.

### Futon 6 — Mathematical Knowledge

Modeling mathematical knowledge as first-class objects in the hypergraph: concepts, texts, proofs, and the relationships between them. Not just formal validity but informal scaffolding — design patterns for how proofs are constructed, making them teachable and understandable.

Goals:
- Store mathematical concepts and mark up texts with which concepts they use
- Capture proof structure both formally (validity) and informally (pedagogical path)
- Self-documenting proofs where the constructing agent gains assurance of correctness
- Feature completeness: all mathematical knowledge, built incrementally

Via Curry-Howard, the same machinery could extend to programs, but that's out of scope to avoid swallowing everything prematurely.

### Futon 7 — Accountability and Assurance

If mathematics models reality, and Futon 6 handles mathematical knowledge, then Futon 7 applies the same framework to economic and social systems — specifically as accountability infrastructure.

Current large-scale systems optimize for extraction rather than delivery. The issue isn't that companies are large — it's that costs, relationships, and externalities are structurally invisible. Environmental impacts, social costs, supply-chain relationships: these can't be recognized or talked about inside systems optimized for shareholder return. What's needed is genuine assurance that systems serve users' actual interests, with verifiable accountability rather than marketing claims.

Futon 7 provides this assurance layer. Just as free software proved to have benefits its founders didn't anticipate, transparent and accountable approaches to commerce and governance may prove beneficial in ways we can't yet predict.

Gravpad is a small instance of this principle: Arxana-style annotation applied to the open web, adding visibility to existing structures without requiring permission or replacement. Where platforms shape user behavior, Gravpad makes that same "control surface" available for other purposes — bending existing structures rather than breaking them.

Futon 7 also provides assurance for Futon 6: verification that the technological work is done well, not sloppily, and in the user's interest.

### Why This Progression Is Plausible

The lower layers aren't just specified — they validate the approach:

- **Futon3's pattern checker** already verifies its own devmap, demonstrating that flexiformal pattern-checking works for real development. This machinery will improve; the devmap outlines the path.
- **Tatami's pattern retrieval** feeds superficially-related patterns to LLMs, which reason about applicability. Interactive validation closes the loop. This is early evidence of pattern transfer across contexts.
- **Futon5's category-theoretic specs** will add verifiable composition rules. If the current system isn't convincing, the next release — with that machinery — may be.
- **Gravpad** shows the stack isn't monolithic. You can prototype forward on any layer once requisite tooling is unlocked; progress doesn't require completing everything below first.

Existence proofs from outside the project also ground the ambition: proof assistants show mathematical formalization is achievable; large-scale platforms show that sophisticated behavioral modeling works (the question is what it's optimized for).
