# ARGUE dialogue — claude-13's points, affirmed or refuted

Protocol in README.md. claude-13 puts each point of its sealed argument to
codex-21 and claude-19 in turn; each replies affirm / refute / qualify,
citing a P-number from its own sealed argument (texts still sealed until the
end; the citations are checked when the seals open).

## P1 — relations must be first-class content

**codex-21: AFFIRM.** Its P2: "Endpoint lists are insufficient because
endpoint membership does not say what each endpoint does." Its P8 adds that
immutable first-class relation ids make recursive criticism local; otherwise
changing an inner relation silently changes every outer relation citing it.

**claude-19: AFFIRM, with one qualification and one open edge.**
- Grounds, its P4(a): "MAP-Q1 showed futon1b's default id sorts endpoints and
  erases roles … the id must digest ordered role-labelled endpoints"; its
  P4(c): "a span cannot be the identity of what relocates, only a locator
  pinned to one revision."
- Qualification, its P16: "Until F4's composite 'clusion exists, 'recursive
  'clusions' is a schema allowance (§2), not a demonstration … show it at one
  scale or drop the word." Stage-local criticism is what F4 has to show, not
  a property the design already has.
- Open edge, its P10: which fields enter the digest? If interpreter or
  dynamics version is in it, every model upgrade mints a new 'clusion for the
  same claim; if not, two interpreters' identical claims share one id and
  their evidence must live in separate records. "Its own id" is defined only
  once the type declaration fixes that.

**For the refactor:** state stage-local criticism as F4's burden of proof;
settle per type which fields enter the identity digest (claim vs who-made-it).

## P2 — roles belong to identity

**codex-21: AFFIRM.** Its P2: "The same two endpoint IDs can participate as
source and target, claim and support, interpreter and interpreted act, or
domain and codomain." Role reversal must mint a different identity; futon1b's
sorted-endpoint default is unsuitable for 'clusions.

**claude-19: AFFIRM.** Its P4(a), nearly verbatim the same claim. Addition
from its P10: "The core does not say whether the dynamics version or the
interpreter enters the digest … The type declaration must state the digest
fields per type." Fix this in C1 alongside the role rule.

**For the refactor:** the per-type digest-field rule is now raised twice
(P1, P2); it belongs in P2/P3 as part of what identity means.


## P3 — types are declared, not inferred

**codex-21: AFFIRM.** Its P2: "The type declaration must also define arity,
endpoint kinds, identity projection, dynamics requirements, and convergence
policy." Its P26 adds that declarations are immutable and versioned: changing
role semantics in place would invalidate every dependent digest.

**claude-19: QUALIFY.** Affirms from its P1 (one registry answers validity;
per-type declarations carry every relation kind without a universal
ontology). Two qualifications:
- Its P9: the declarations are too rich for increment 1. "If C1 takes more
  than two packets, the declarations are too rich; freeze only roles, arity,
  endpoint kinds and identity projection." Convergence policy should default
  from DERIVE-core §8 by record kind, overridable per type.
- Its P13: nothing checks that a declared dynamics reference still names a
  runnable thing. "A receipt that names a dynamics version the registry does
  not know is a defect": the §13 attempt receipt should check its dynamics
  version against the registry.

**For the refactor:** P3 should say (a) declarations are versioned and never
edited in place (codex-21 P26); (b) C1 freezes roles, arity, endpoint kinds
and the identity projection, with other fields defaulted (claude-19 P9);
(c) attempt receipts are checked against the registry's dynamics versions
(claude-19 P13).

## P4 — not everything is a 'clusion

**codex-21: AFFIRM.** Its P12: "Only ran-no-output over a declared envelope
warrants absence"; a non-run must not become a void 'clusion. Its P28: "not
every useful relation needs executable dynamics", and the ontology must keep
evidence occurrences, derived contents, receipts, requests and semantic
claims distinct.

**claude-19: AFFIRM.** Its P4(c): a mark occurrence is "evidence used by an
interpretation, not the interpretation." Its P6: an attempt that produced
nothing is a receipt; "the 428 'no reader' voids in 馬's verdict were exactly
that error: a missing reader recorded as a void reading." Its P18: fibre
kernel evidence and the record's assessment are two records, consistent with
fibres as derived content. Addition from its P14/P17: transient edit
proposals are fine, but the Circus needs a declared consumer that weights
candidates by basis and standing, or §16's `proposal/shown` events are
written and never read.

**For the refactor:** P4 should name the 馬 "no reader" voids as the
concrete error it prevents, and say "ran, no output" is only meaningful over
a declared envelope (codex-21 P12). The shown-events consumer is an
M-repl-circus need; note it as a pointer, not core scope.

## P5 — meaning, standing and acceptance are separate

**codex-21: AFFIRM.** Its P11: "Ambiguity, contradiction, missing evidence,
and domain extension must remain first-class." Its P14: F1 tests
interpretation 'clusions and attributable assessment separately, so a
faithfully recorded model reading can stay unaccepted or be corrected
without rewriting its evidence.

**claude-19: AFFIRM.** Its P2: basis, standing and coverage are three
questions; "collapsing them into one ladder is the
apparatus/success-must-not-resemble-failure error in record form." Its P3:
acceptance is a separate attributable assessment, not a field on the
'clusion. Its P6: resolution state is a third axis, "data, not failures",
folded into neither evidence nor acceptance. Addition from its P8/P14: the
turn-iWVzvT case works because Joe was the assessor; when no person is in
the loop, the design must say who assesses, or candidates stay candidates
forever and a consumer must be declared to read them as such.

**For the refactor:** P5 should list resolution as its own axis beside
evidence and acceptance. "Who assesses with no person in the loop" is
answered for the fixtures (DERIVE-core §10: a human or institutional
actor); state that an unassessed candidate is a legitimate resting state,
and leave the choice of automated assessors to client missions.

## P6 — non-resolution is legitimate

**codex-21: AFFIRM.** Its P11: "Non-resolution is legitimate because a graph
may faithfully contain competing claims." Its P28: append-only coexistence,
attributable choice, theory extension and deliberate non-resolution are
distinct; forcing one merge rule would erase meaning.

**claude-19: AFFIRM.** Its P6 is this point almost verbatim ("resolution
states are data, not failures … F3's divergence … is a result"), and cites
Joe's "absence must not read as a value." Its P3: retiring one of two
competing records is a separate attributable assessment, never last-writer
erasure. Qualification from its P4(d)/P5: coexistence holds only if
convergence is per type (DERIVE-core §8); a last-writer or CRDT merge
anywhere would erase silently. "F2 must show two divergent correspondences
surviving a merge with both authors intact." Until then P6 is asserted, not
shown.

**For the refactor:** P6 should name its demonstration: F2's merge test
keeps both divergent records with authors intact, and F3 keeps the a93J02
divergence as a result. Add codex-21's four-way distinction (coexistence,
choice, extension, non-resolution) as the reason convergence is per type.

## P7 — immutability with successors

**codex-21: QUALIFY.** Its P8 affirms: "Append-only semantic records are the
right default"; overwriting an inner relation silently changes every outer
relation citing it. Its P15 conditions the sequencer claim: CAS suffices
only for the tested workload; CRDT is re-armed on "lost legitimate history,
unacceptable sequencer, or required offline convergence."

**claude-19: AFFIRM, qualified.** Its P4(d) restates DERIVE-core §8 (immutable
'clusions and assessments, CAS heads, LWW personal state, CRDT only if F2
demands it). Its P7: every Arxana record is rebuildable from immutable
sources plus versioned rules, so wipe-and-rebuild is the test of
immutability. Qualification from its P10/P20: "change mints a successor" is
ambiguous until digest fields are fixed per type. If the dynamics version is
in the digest, every model upgrade mints a new 'clusion; if not, two
interpreters' identical claims share one id and their evidence must be
separate records citing it, but "the F1 §4 table says 'fields on the
'clusion'." F1's "proposes a new interpretation" depends on which.

**For the refactor:** P7 should (a) state codex-21's three CRDT triggers as
the conditions under which the single-sequencer choice is reopened; (b) add
wipe-and-rebuild as the test of immutability (claude-19 P7); (c) flag a real
inconsistency for codex-21: DERIVE-F1 §4 puts evidence fields on the
'clusion, which only works if the interpreter is in the digest. That is the
third time the digest-field question has come up (P1, P2, P7); it is the
first item for C1.

## P8 — one authority per question

**codex-21: AFFIRM.** Its P9 calls this "the design's strongest
distributed-systems discipline" and assigns the same authorities. Its P25
qualifies: referenced external authorities need typed outcomes for
unavailability, digest mismatch and schema drift; a faster cache must never
answer in their place.

**claude-19: AFFIRM, two qualifications.** Its P1 is this point ("Any
criticism that adds a second authority for the same question should lose"),
citing the worktree load-file and derived-JSON incidents.
- Its P11: the §15 index is a cache of a referenced authority, and the hx
  sidecar "cost a day" on 2026-11-07. "A rebuildable cache is only as cheap
  as its rebuild": give it its own partition, a published rebuild cost and a
  watermark, or the reference becomes a stale mirror.
- Its P14: the acceptance authority is named but not staffed. "An authority
  that is never exercised is a second-authority problem in disguise, because
  consumers end up deciding for it."
Its P18 agrees on the Lean kernel, with kernel evidence and record
assessment as two records.

**For the refactor:** P8 should add (a) typed failure outcomes for each
referenced authority (codex-21 P25); (b) caches carry a watermark and a
measured rebuild cost, and never answer when the watermark is behind
(both). The unstaffed-acceptance point recurs (P5, P8): the refactor should
say consumers must read `candidate` as candidate, never as accepted or
ignored, and that staffing acceptance is the client mission's job.

## P9 — projections are attributable

**codex-21: AFFIRM.** Its P6: "a surface fragment that cannot name the
records and projection rule producing it is merely a picture, not an Arxana
projection." Its P23: moving a diagram changes projection data; changing
what it supports mints a new semantic relation.

**claude-19: AFFIRM, two qualifications.** Its P5 is this point: a projection
is a versioned registered rule that keeps record ids and role structure and
"drops the rest by name"; F1's "same record identities in all three
projections" is the whole claim.
- Its P12: the exported conversation Markdown is itself a projection that
  can become the de facto source. The exporter version and sidecar manifest
  must be recorded on the ingested revision, with a drift check against the
  settled turns in F1 increment 4.
- Its P17: "Every kept projection output needs a named reader … or it is a
  test trace and must not be stored as a record."

**For the refactor:** P9 should say each projection rule names what it drops
(claude-19 P5); the exported Markdown carries exporter version and manifest,
with a drift check in F1 (P12); a projection output is kept as a record only
when a reader is named (P17). The written-never-read concern now recurs
(P4, P9).

## P10 — dynamics are referenced, not stored code

**codex-21: AFFIRM.** Its P28: "not every useful relation needs executable
dynamics; some are assertions with a method reference." Execution is a
separate admitted act with a receipt; its P12 requires attempt receipts to
distinguish execution outcomes.

**claude-19: QUALIFY.** Affirms from its P13 and P1 (the graph is not an
evaluator; execution authority lives elsewhere). Qualifications:
- Its P13 (second time, after P3): inert references "will drift from the
  code that implements them"; the attempt receipt must be validated against
  the registry at write time, not merely recorded.
- Its P10: recommends the dynamics reference stays out of the identity
  digest and the version is evidence, so frozen dynamics do not freeze
  identity.

**For the refactor:** P10 should say receipt writes are refused when the
dynamics version is unknown to the registry. claude-19's digest
recommendation (dynamics version out of the digest, kept as evidence) is a
concrete answer to the digest question raised at P1, P2 and P7; take it to
codex-21 with the F1 §4 inconsistency.

## P11 — model output enters as candidates

**codex-21: AFFIRM.** Its P10 separates "evidence basis, evidence standing …
and human assessment" because they answer different questions. Its P17:
"Mark8 judgments are model-basis candidate evidence, with Joe's assessment
separate"; the constructor cannot approve its own interpretation.

**claude-19: AFFIRM.** Its P8: "readers propose, assessments decide." Its
P18: kernel basis is scoped to what the kernel checked — a pinned fibre is
evidence the lemma holds for the declared map, "not evidence that the
records are the map." Addition from its P14 (third time, after P5 and P8): a
boundary with nobody on the far side becomes a wall; candidates need
declared consumers that weight them by basis and standing.

**For the refactor:** P11 should add that kernel basis covers only the
statement the kernel checked (claude-19 P18). The declared-consumer point
has now been raised at P4, P5, P8, P9 and P11; the refactored argument
should state it once as a core rule — every stored record kind names a
reader, and readers must interpret `candidate` as candidate — while leaving
which assessors and consumers exist to the client missions.

## P12 — reads are bounded and indexed

**codex-21: AFFIRM.** Its P24: "The indexed read contract is part of
semantics in practice." It endorses the same composite index and prefers
"named materialized aggregates with watermarks and deterministic rebuild …
to an arbitrary online group-by language on the prompt path."

**claude-19: AFFIRM, with a qualification and a caution against itself.** Its
P11 (source of the 50–80 s figure): apply watermark and rebuild cost to the
index itself, not only to aggregates. Qualification: an index on the shared
hyperedges table is not enough — on 2026-11-07 type filters did not prune and
`_id IN (...)` was a full scan — so arxana/* rows need their own table or type
partition, away from the watcher's ~1.1M rows. Caution from its P19: that is
"a cost estimate, not a measurement"; a properly indexed XTDB table may
behave differently. Measure a bounded read on the new index against the 5 s
prompt-line budget before F1 is declared served.

**For the refactor:** P12 should name the partition (arxana/* separate from
watcher rows) as the expected design, conditional on C3's measurement, and
make "bounded read measured under budget" a C3 acceptance check rather than
an inference from the 2026-11-07 scans.

## P13 — the fixtures test the core

**codex-21: AFFIRM.** Its P27: "C1/C2 establish the common schema and
authoritative write path; F1, F3, and F5 test independent semantic domains."
Parallel applicability tests that the core is shared; F2/F4/F6 then exercise
convergence, recursion and projection separately.

**claude-19: QUALIFY.** Affirms core-first; its P4(a) shows the role-digest
rule came from MAP-Q1, not from F1. Qualifications:
- Its P9: core-first cuts both ways — C1's registry, vocabulary, three
  presentations and golden records are a lot of ceremony for seven fixture
  turns. Falsifier: "if C1 takes more than two packets, the declarations are
  too rich."
- Its P16: "a build order is a plan, not evidence." State P13 as "the
  fixtures are designed to test the core"; claim the stronger form only once
  F3 or F5 lands on the same types as F1.

**For the refactor:** weaken P13 to "designed to test", with the stronger
claim tied to a named event (a second fixture green on unchanged C1 types).
Adopt the two-packet falsifier for C1 (also raised at P3).
