# A2 — final argument for JVM Arxana

Mission: `M-jvm-arxana`. Phase: ARGUE. Date: 2026-11-06.

This is the canonical synthesis. `A1-design-fitness-argument.md` and
`ARGUE-DERIVE-loop.md` retain the seven-pass objection and disposition trail;
D1–D7 contain the selected design.

## Thesis

The selected design is fit for purpose because it places every question that
must receive the same answer from Emacs, VS Code, code-server and document
views behind one headless JVM Arxana authority, pins every derived answer to
immutable artifact revisions and evidence, and leaves durable atomic mutation
with futon1b. It shares identity and inference-ready grounding without making
an editor, a rendering pipeline, a ranking model or an institution secretly
authoritative.

## Argument structure

| Claim | Evidence | Strongest objection | Disposition |
|---|---|---|---|
| The gap requires a shared authority, not another editor adapter | P1 found the nominal Emacs endpoint stale and the live authority narrower than client assumptions; P3 found four mutually unintelligible protocols | Fix the port and build a VS Code mode | Refuted: configuration repair cannot reconcile position, freshness, identity and error semantics |
| Resolution can be authoritative outside Emacs | P2 found stored declarations are portable data and anchor recovery is a pure string algorithm | Porting would invent semantics for five inert finders | D2 imports them declaration-only; executable translation is versioned, ambiguity-preserving and attributable |
| Correspondence must be revision-pinned evidence, not offsets | P4 found quote/range/transformation evidence and a real hash/anchor contradiction | The strongest fixture contradicts itself | D4 preserves it as a negative fixture and requires a coherent positive rebuild; contradiction becomes a tripwire |
| Revision refusal is sufficient now; CRDT is not presently required | P5 found no legitimate concurrent multi-writer intent; observed failures were stale/single-authority failures | The non-CRDT contract also lacked CAS | D3/D7 place atomic expected-head commit and journal in futon1b; the CRDT re-arm observation remains recorded |
| Thin clients can prove equal answers without equal UIs | P3 donor behaviours and D5 fidelity matrix | Coordinate conversion and presentation can create false equality | Versioned semantic digests exclude presentation/timing and include IDs, revisions, locations and evidence |
| The graph/action waist is common without importing policy | P6 found ID-addressed reads and typed ID-endpoint writes across four consumers | Candidate-action fields exceed the empirical interface | Chain topology is empirically warranted; richer fields are an explicit mission commitment, while policy/scoring stay namespaced |
| Headless Arxana over futon1b is the correct boundary | D7 comparison: A1 couples evolving semantics to storage; A3 duplicates evaluator authority | A2 creates a second consistency domain | Rejected variant: atomicity remains in futon1b; Arxana owns semantics, not an independent mutable head |
| The design's invariants are enforceable | D1–D7 provide single-locus assertions, tests and authority assignments | Generic futon1b routes could bypass Arxana uniqueness | Store reserves `arxana/*`; only authenticated conditional transactions may write it and validate uniqueness atomically |

## Why the hybrid is coherent

This is A2 with a deliberately narrow store primitive, not A1 under another
name. The distinction is ownership of semantic questions: Arxana owns strategy
execution, correspondence, protocol projections and candidate-action
compilation; futon1b owns durable records and transactional truth. A client
asks Arxana what a scope means and asks an admitted executor to attempt a
guarded change; it does not interpret raw XTDB records independently.

The split closes because the journal is durable futon1b truth and Arxana's
subscription stream is a replayable projection. ArtifactProvider verifies
content-addressed bytes. Authority-side evaluation is required for promotable
evidence. Generic writes cannot enter the reserved namespace. No question has
two final owners.

## Client-side fitness

Emacs and a desktop VS Code extension are two projections of the same
contract. A code-server deployment is a third projection/deployment shell: it
may colocate the VS Code extension, document pane, terminal and repository
access on a server, but co-location grants no semantic authority. It consumes
the same versioned protocol and canonical response digest as every other
client. Server-local filesystem positions and browser DOM IDs remain locators,
not identity.

The proof-presentation client profile exercises rather than changes the model.
A domain `proof/move` entity may have authored prose, Lean, generated formal
rendering and generated informalization manifestations. Mining, exact anchors,
model output and human review remain independent evidence. Attributable
correspondence assessments may accept, reject or explicitly record absence
without rewriting assertions. Pedagogical usefulness is linked consumer
evidence, not JVM conformance.

## Pattern cross-reference

| Pattern | Application and consequence |
|---|---|
| `storage/graph-memory-contract` | D1 documents durable entity/relation/provenance shapes rather than relying on client convention |
| `storage/canonical-interface` | D7 names Arxana as canonical semantic interface and futon1b as storage/transaction interface; clients do not choose |
| `storage/identity-uniqueness` | Content-addressed revisions, deterministic manifestation IDs and reserved namespace enforcement make uniqueness checkable |
| `storage/error-layer-hierarchy` | Futon1b typed refusals survive D3 envelopes; missing, unsupported, stale, conflict and failed remain distinct |
| `invariant-coherence/single-locus` | Each question/revision/semantics tuple has one authority result; violations enumerate conflicting tuples |
| `invariant-coherence/protocol-family-naming` | Shared-authority instances retain separate identity, resolution, correspondence and action-grounding namespaces |
| `cascades/repointable-declarations` | Legacy finder declarations remain visible and acquire executable semantics only through versioned translation |
| `storage/determinism-vs-expansion` | Extension properties are namespaced and tested; core meaning cannot drift through consumer fields |

No selected pattern overrules MAP evidence. Each pattern either produces an
enforced boundary or a named acceptance test.

## Theoretical coherence and rationale

The design preserves Arxana's identity-through-links account while moving
editor-local positions into disposable projections. It realizes
one-authority-per-question without making futon1b understand UI semantics. It
also supports the broader “action becomes inference” thesis in a bounded way:
source act, interpretation, grounding, candidate action, admission and receipt
are separate, queryable records, so an argument may become computational
without description silently becoming authorization.

- **@why war-room/wr-7:** frames count only when exercised ports interoperate;
  the shared protocol and semantic-digest transcript are that exercise.
- **@why war-room/wr-8:** typed records and fixtures, not prose claims, are the
  source of truth.
- **@why war-room/wr-26:** deferred CRDT and unsupported capabilities carry
  explicit re-arm conditions.
- **@why war-room/wr-24:** dropping salience/Plexus does not erase their former
  discipline; workflow audit and a follow-up contract remain required if use
  is witnessed.
- **@why war-room/wr-3:** the authority/wiring model precedes implementation.

## Trade-offs accepted

- A second JVM service and protocol boundary are accepted to isolate semantic
  evolution from durable storage.
- A narrow futon1b conditional route is accepted because atomicity cannot be
  safely simulated above an open store.
- Legacy records and IDs remain readable, accepting adapter complexity instead
  of destructive migration.
- Resolution recomputation and immutable revisions consume resources, buying
  reproducibility and honest staleness.
- Client-hosted evaluation is limited to previews; promotable evidence pays the
  cost of authority-side replay.
- CRDT collaboration, pedagogical policy and autonomous execution are deferred
  until observations justify them.

## Generalization

The architecture applies beyond Emacs and VS Code when a new client agrees on
the canonical protocol, revision identity, coordinate conversion and semantic
digest. New artifact media add ArtifactProvider and correspondence methods;
new domains add typed entities, manifestation roles and namespaced evaluation
records. They do not need to fork the authority model. What does not
generalize automatically is domain semantics: proof moves, tutorial outcomes,
Giraffe wants and War Machine verdicts remain witnessed client profiles.

## Remaining implementation obligations, not argument defects

- Build the negative and repaired positive D4 fixtures first.
- Implement and test futon1b conditional transaction and reserved namespace.
- Run D2 translation/evaluator and query/index tripwires.
- Demonstrate identical semantic digests from Emacs, VS Code/code-server and
  the Oxide pane.
- Run the workflow audit for salience/Plexus and the layered correctness versus
  usefulness evaluations.

These are VERIFY/INSTANTIATE evidence requirements. None leaves authority,
identity, consistency or schema meaning undecided.

## Plain-language argument

Today each editor and document view keeps its own partial idea of what a piece
of work means, so matching-looking screens can still disagree. JVM Arxana gives
them one place to ask, records which exact version and evidence produced the
answer, and refuses stale changes before they overwrite newer work. Editors,
web IDEs and rendered documents remain different windows, while institutional
rules and teaching judgments remain with their proper owners. This makes the
connections inspectable and actionable without pretending that a guessed link
is a fact or that a proposed action is permission to execute it.

## Verdict

The architecture is not merely workable: it follows from the observed split
protocols, portable-but-unexecuted strategy data, contradictory correspondence
fixture, stale-write incidents and common graph waist. The strongest
counterarguments changed the design—atomicity moved to futon1b, ambiguity
became explicit, fixture contradiction became a rejection test, and namespace
enforcement closed the bypass. With those dispositions, the fitness claim is
defensible and ARGUE may exit to VERIFY.
