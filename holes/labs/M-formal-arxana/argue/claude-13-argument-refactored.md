# claude-13 argument for the DERIVE design — refactored after the dialogue

claude-13, 2026-11-08. The sealed original is `claude-13-argument.txt.gpg`
(sha256 `36808108…819f`, verified on opening). This version rewrites it in
light of DIALOGUE.md (P1–P14) and of the two opened seals. Each change cites
where it came from: `c21 Pn` = codex-21's sealed point n, `c19 Pn` =
claude-19's.

**Claim.** The design gives Arxana one small contract that six unlike
applications (conversation, collaborative text, mathematics across Lean and
prose, composition of processing stages, geometry, argument layout) and one
client (REPL Circus) can use without private representations, and it does not
let interpretation, presentation or storage convenience stand in for meaning
or authority. Every point below says what would show it; several are designed
but not yet shown, and say so.

## What changed, in one paragraph

All 14 points were affirmed or affirmed with qualifications; none was
refuted. Three points got weaker: P13 (fixtures are *designed* to test the
core, not yet evidence), P14 (RDF can encode the pieces; the difference is
enforcement), and "recursive/composable" (a schema allowance until F4). One
question came up five times and is now the first item for C1: which fields
enter a 'clusion's identity digest. One rule came up five times and is added
as P15: every stored record kind names its reader.

## Points

**P1. Relations are first-class content.** A 'clusion has its own id, type,
roles, evidence and standing, so it can be assessed, contradicted or
superseded without editing what it relates. Immutable ids keep criticism
local: otherwise changing an inner relation silently changes every outer one
citing it (c21 P8). *Shown by:* F1 (a reading rejected and succeeded without
touching the turn). *Not yet shown:* stage-local criticism inside a
composite; that is F4's burden (c19 P16).

**P2. Roles and the digest fields belong to identity.** The id digests
ordered, role-labelled endpoints (MAP-Q1; c21 P2; c19 P4a). Each type
declaration must also state which other fields enter the digest (c19 P10).
Proposed default, to put to codex-21: endpoints, roles and the asserted
content (e.g. the intent) in the digest; interpreter and dynamics version
outside it, recorded as evidence. *Known inconsistency:* DERIVE-F1 §4 puts
evidence fields on the 'clusion, which only works if the interpreter is in
the digest (c19 P20). C1 must resolve this before any 'clusion is written.

**P3. Types are declared, versioned and never edited in place.** A
declaration change mints a new declaration revision; editing role semantics
in place would invalidate every dependent digest, so the registry refuses it
(c21 P26). Migrations, compatibility queries and aliases are explicit
(c21 P26 — this was my weak point (a)). C1 freezes only roles, arity,
endpoint kinds and the identity projection; convergence policy and method
fields default by record kind from DERIVE-core §8 and are overridden per type
only when needed (c19 P9). *Falsifier:* if C1 takes more than two packets,
the declarations are too rich (c19 P9).

**P4. Not everything is a 'clusion.** Marks are occurrences used as
evidence; fibres are derived content; attempts are receipts; edit proposals
are mostly transient requests (c21 P28; c19 P4c, P18). The concrete error
this prevents: 馬/象 void verdicts where 428 parts were "no reader", a missing
reader recorded as a void reading (c19 P6). A receipt distinguishes "no
reader", "reader refused" and "ran over a declared envelope and found
nothing"; only the last warrants absence (c21 P12).

**P5. Meaning, standing and acceptance are three separate axes.** Resolution
(unique/ambiguous/missing/stale), evidence (basis, standing, coverage), and
attributable assessment (candidate/accepted/rejected) (c21 P10; c19 P2, P3,
P6). A faithfully recorded model reading can stay unaccepted or be corrected
without rewriting its evidence (c21 P14), as Joe did for turn-iWVzvT. An
unassessed candidate is a legitimate resting state.

**P6. Non-resolution is a legitimate result.** Competing claims coexist,
attributable; retiring one is an assessment or a successor, never
last-writer erasure. Coexistence, attributable choice, theory extension and
deliberate non-resolution are four different things, which is why
convergence is declared per type (c21 P11, P28; c19 P4d). *Shown by:* F3
keeps the a93J02 divergence as a result (Joe: divergence is expected). *Must
be shown by:* F2's merge keeps two divergent correspondences with both
authors intact (c19 P5); until then this is asserted for merges.

**P7. Immutability with successors.** 'Clusions, assessments, revisions,
type declarations and promoted projection rules are append-only; change
mints a successor (c21 P8). *Test:* the Arxana store can be wiped and rebuilt
from immutable sources plus versioned rules (c19 P7). One CAS sequencer
suffices for the tested workload; the CRDT question reopens on lost
legitimate history, an unacceptable sequencer, or required offline
convergence (c21 P15). F2's text rebase (canonical bytes, patch identity,
overlap classes, what counts as retained intent) is unspecified and needs a
spike in VERIFY before CAS is presented as adequate (c21 P16; my weak point
(b)).

**P8. One authority per question, placed where it can be exercised.**
Validity: JVM type registry. Atomic persistence of arxana/*: futon1b's
conditional route. Acceptance: an attributable human or institutional actor.
Lean truth: the 4.29 kernel. Layout: the registered projector or the user
(c21 P9; c19 P1). External stores are referenced, with typed outcomes for
unavailable authority, digest mismatch and schema drift (c21 P25). Every
cache — the §15 index included — publishes a watermark and a measured
rebuild cost, and never answers when its watermark is behind (c19 P11; my
weak point (c)). The hx sidecar cost a day on 2026-11-07; that is the
failure this prevents.

**P9. Projections are attributable.** A fragment that cannot name its
records and projection rule is a picture, not a projection (c21 P6). Each
rule names what it drops (c19 P5). Moving a diagram changes projection
data; changing what it supports mints a new relation (c21 P23). The
exported conversation Markdown is itself a projection that could become the
de facto source: the ingested revision records the exporter version and
manifest, and F1 increment 4 checks it for drift against the settled turns
(c19 P12).

**P10. Dynamics are referenced, not stored code.** A 'clusion carries an
inert dynamics reference; execution is a separately admitted act with a
receipt (c21 P28, P12). A receipt naming a dynamics version the registry
does not know is refused at write time, so references cannot drift silently
from code (c19 P13).

**P11. Model output enters as candidates.** 象, 馬, mark8 and the
informaliser write basis=model candidates; the constructor cannot approve its
own interpretation (c21 P17; c19 P8). Kernel basis covers only the statement
the kernel checked: a pinned fibre is evidence the lemma holds for the
declared map, not that the records are the map (c19 P18). F3's synthetic
contradiction tests recording and projecting a contradiction; the report
must say who or what detected it, and does not claim Arxana discovers
contradictions (c21 P18).

**P12. Reads are bounded and indexed; aggregates are materialised.** One
mandatory index (edge-type, endpoint-role, endpoint-id, time, edge-id);
named aggregates with watermarks and deterministic rebuild instead of an
online group-by on the prompt path (c21 P24). arxana/* records are expected
to need their own table or partition, away from the watcher's ~1.1M rows
(c19 P11), but that is an estimate (c19 P19). *C3 acceptance:* a bounded read
measured under the 5 s prompt-line budget on the real store. Recursive
traversal is bounded, and C1 or F4 states whether composition graphs are
acyclic, explicitly cyclic or fixed-point guarded (c21 P20).

**P13. The fixtures are designed to test the core** (weakened). The role
rule came from MAP-Q1, not F1 (c19 P4a), and F1, F3 and F5 cover independent
domains (c21 P27). But a build order is a plan, not evidence (c19 P16). The
stronger claim — the core is not F1-specific — is earned when a second
fixture (F3 or F5) is green on unchanged C1 types.

**P14. What makes this more than RDF-style annotation** (rewritten). RDF-star,
named graphs, PROV and SHACL can encode n-ary roles, provenance, statements
about statements and shapes (c21 P3; c19 P15). The difference is that Arxana
*enforces*, at one registry and one write path, what RDF leaves optional:
versioned types, role-sensitive identity, per-type convergence, the
evidence/standing/acceptance split, and authority and admission boundaries.
Recursive composition belongs on that list only once F4's composite exists,
with typed ports and flow rather than a list of stage ids (c21 P19; c19 P16).

**P15 (new). Every stored record kind names its reader.** Raised at P4, P5,
P8, P9 and P11 (c19 P14, P17). A projection output, `proposal/shown` event
or candidate is stored as a record only if a reader is named; readers must
treat `candidate` as candidate, never as accepted or ignored. Which assessors
and consumers exist (驴 reweighting, Giraffe) is the client missions' job;
the core only requires that one be named.

## Remaining weak points

(a) Type evolution is now specified as rules (P3), not tested. (b) F2 text
rebase is unspecified (P7). (c) Cache drift is addressed by watermarks (P8),
but no rebuild has been timed. (d) The cost of materialising every 象 turn as
records is still unmeasured; it rides on C3's measurement (P12). (e) The
digest-field choice in P2 is a proposal and needs codex-21's ruling.
