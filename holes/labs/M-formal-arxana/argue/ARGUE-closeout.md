# ARGUE close-out — M-formal-arxana

claude-13, 2026-11-08. Sources: the three sealed arguments (opened and
hash-checked, DIALOGUE.md), the 14-point dialogue, round 9, and the
codex-21 review of DERIVE-wiring. The argument itself is
`claude-13-argument-refactored.md`; this note covers the mission's three
ARGUE checkboxes and the lifecycle's trade-off and generalisation items.

## 1. Pattern cross-reference (futon3/library; every path checked to exist)

| Pattern | Where it applies in the design | Cited by |
|---|---|---|
| apparatus/one-authority-per-question | DERIVE-core §10; wiring authorities (validity, persistence, acceptance, admission, Lean truth) | all three |
| apparatus/success-must-not-resemble-failure | orthogonal standing (§6): a model-attested candidate never renders like an accepted one | c19 P2 |
| apparatus/replayable-not-precious | fidelity matrix §12: Arxana store rebuildable from sources + versioned rules; wipe-and-rebuild as the immutability test | c19 P7 |
| apparatus/new-failure-class-is-a-design-defect | attempt receipts refuse unknown dynamics versions (§13, refactor P10) | c19 P13 |
| apparatus/evidence-to-disposition-once | every stored record kind names its reader (refactor P15) | c19 P17 |
| apparatus/monitors-measure-the-work | caches publish watermark + measured rebuild cost (§15, refactor P8) | c19 P11 |
| sidecar/append-only-semantic-audit | successors, never overwrite (§8) | c21 P8 |
| sidecar/typed-kolmogorov-arrows | transforms carry explicit mode and standing (dynamics reference + attestation) | c21 P2 |
| storage/graph-memory-contract | documented record shapes for downstream readers (C1 golden records) | c21 P2 |
| vsatelier/projection-independence | same record ids across all F1/F5/F6 projections | c21 P6 |
| vsatlas/non-destructive-relational-layers | interpretations and correspondences appended beside sources | c21 P7 |
| writing-coherence/name-what-you-drop | each projection rule names what it does not carry | c19 P5 |
| writing-coherence/recursion-cheque | "recursive" claimed only once F4's composite exists | c19 P16 |
| contracts/every-entry-has-a-falsifier | CRDT row (F2), C1 two-packet falsifier, C3 latency check | c19 P4d |
| contracts/declare-the-reductions | C1 freezes a stated subset of the full declaration | c19 P9 |
| cascade-construction/separate-construction-from-meaning-review | mark8 and the informaliser cannot accept their own readings | c21 P17 |

Design revisions this survey produced (already applied): evidence as
separate attestations (§17), the reader rule (refactor P15), bounded
recursive reads (§17, C3), cache watermarks (§15).

## 2. War-room rulings

- `@why war-room/wr-8-typed-files-are-sources-of-truth` — `intent-vocabulary/v1`
  is the source; the three presentations are generated (wiring choice 10).
- `@why war-room/wr-10-next-move-surface-is-recursive-closure` — F1's GCC view
  is read from the canonical graph, not maintained as a document; it stays a
  labelled projection until F4.
- `@why war-room/wr-21-geometrization-is-a-shared-cross-mission-dependency` —
  F5 supplies a geometry-markup nucleus, not a ground metric for other
  missions.
- `@why war-room/wr-26-a-capability-switched-off-carries-its-re-arm-condition-in-writing-at-the-switch`
  — CRDT stays off with its three re-arm conditions written beside it (F2).
- `@why war-room/wr-22-logic-model-before-code-is-a-sanctioned-verify-method`
  — the method for VERIFY (see the mission's VERIFY section).

claude-19's seal also cites operator rulings held in memory, not numbered in
the war room ("absence must not read as a value", "theorems are not
endorsements", "Lean specifies, code conforms"); its P21 says these should
be re-cited if the record differs, not withdrawn.

## 3. Trade-offs: what we gave up

- **Simplicity of plain hyperedges.** C1 requires declarations and a
  registry before the first 'clusion. Bounded by the two-packet falsifier.
- **Automatic merging.** One CAS sequencer; concurrent and offline edits are
  rebased or presented as alternatives, never silently merged. CRDT reopens
  only on its written conditions.
- **Off-the-shelf RDF tooling.** Arxana enforces in one registry what RDF
  leaves optional; interop would be an export projection, not the store.
- **Trusting model output.** Readings from 象, 馬, mark8 and the
  informaliser stay candidates until assessed; without assessors they
  accumulate as candidates (P15 requires a named reader).
- **Cyclic composition.** Composition graphs are DAGs; a fixed-point
  operator is out of this mission.

## 4. Generalisation

The core names no domain: a new application is new type declarations
(roles, identity projection, convergence policy) plus projection rules.
F1, F3 and F5 are chosen from unlike domains to test that; the claim is
earned when a second fixture is green on unchanged C1 types (refactor P13).
It does not suit relations that must be mutable in place, questions with no
nameable authority, or workloads needing offline-first convergence before
F2 has decided the CRDT question.

## 5. Plain-language account (mission checkbox 3)

The same meaningful thing can show up as a sentence, a piece of code, a
diagram or a web page, and none of those appearances owns it. Arxana gives
the thing and each relationship between things a stable name, records who
or what said each relationship holds and how sure they were, and keeps
disagreements side by side instead of hiding them. Each view of the material
says which records it drew from and what it left out, so different views
can be partial without contradicting one another. Moving something on a
page changes only that page; changing what something means changes every
view that shows it. (After codex-21 P29 and claude-19 P5.)
