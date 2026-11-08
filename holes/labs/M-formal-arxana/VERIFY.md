# VERIFY — M-formal-arxana

claude-13, 2026-11-08. Method (Joe, 2026-11-08: "mixed methods"; war-room
`@why war-room/wr-22-logic-model-before-code-is-a-sanctioned-verify-method`):
a Lean 4.29 logic model checks the invariants that hold by construction;
spikes against the real substrate check what only measurement can settle.
Authors: claude-13 (Lean model, this note), zai-1 and kimi-2 (spikes).
codex-21 is not an author in VERIFY (Codex usage near zero).

## 1. Specification bill of materials

| Aspect | Characteristic defect | Formalism and level | Status |
|---|---|---|---|
| Storage | a claim overwritten or lost; evidence edited by an assessment | Lean model `futon5/lean-arxana` `Store` (append-only, claims unchanged by attest/assess/supersede, assessments never rewrite evidence) | **checked** (21e8c1e) |
| Storage (substrate) | reverse lookups too slow for the 5 s prompt line; arxana rows swept with watcher rows | spike S1 (zai-1): three layouts on a private XTDB 2.1.0 store, timings + index rebuild | running (`invoke-1794105643537-15544-7134217d`) |
| Protocol | an unowned write port; a port with two authorities; an input with no source | futon5 wiring diagram (exotype `.edn`) of DERIVE-wiring §1, structural check | **to do** (claude-13) |
| Recursive schema | cycles in composition; unbounded or repeating reads | Lean `Compose` (rank validator ⇒ acyclic; self-loop refused) and `Read` (terminates, ≤ maxNodes, no repeats, typed truncation) | **checked** |
| Recursive schema (ports) | a composite that is a bare list of stage ids (c21 P19) | priced decline: typed ports are F4's DERIVE detail; re-open when F4 defines port types, before F4 INSTANTIATE | declined, trigger named |
| Concurrency (records) | last-writer erasure of a competing claim | Lean `Store.merge` (keeps both sides, order-independent) | **checked** |
| Concurrency (text) | a silent winner in overlapping edits; offline edits lost | spike S2 (kimi-2): round-9 rebase contract, five overlap classes, offline replay, property test, multibyte | running (`invoke-1794105643965-15545-8d4de732`) |
| Interpretation | a model reading treated as accepted; acceptance implied by evidence | Lean `Store` (unassessed ⇒ candidate; evidence never changes acceptance; assessment is local) plus the F1 fixture with Joe's real assessment (turn-iWVzvT) | **checked** (model); fixture recorded |
| Identity | role-swapped claims merged; interpreter or version splitting one claim | Lean `Identity` (role swap changes id; futon1b default merges them) and `Store.two_interpreters_one_claim` | **checked** |
| Geometry | renderer coordinates taken as mathematical truth | F5's Lean lemmas (square and cube fibres) in this project, with Mathlib | priced decline until F5 starts: the lemmas *are* the F5 check; re-open at F5 INSTANTIATE, no later |
| Projection fidelity | a fragment with no source record; layout moves changing identity | Lean `Projection` (attributable; layout local; meaning propagates; evidence does not change a view) | **checked** (model); F1 three-view acceptance in INSTANTIATE |
| Decision | a choice made for unexamined reasons | IF/HOWEVER/THEN/BECAUSE, DERIVE-wiring §3, reviewed by codex-21 | **checked** |
| Loop | an act succeeds but its measurement is inadmissible | priced decline: no repeated loop in F1–F6; re-open if F4's admitted operation becomes repeated | declined, trigger named |
| Conformance (model ↔ code) | Clojure drifting from the checked model | Lean model emits test vectors the C1/C2 Clojure must pass | **to do**, before C2 is accepted |

Planted-bug check on the Lean model: five design defects (key drops roles,
last-writer merge, assessment rewrites evidence, read without visited check,
acceptance defaults to accepted) each failed `lake build` at the theorem
they violate. See `futon5/lean-arxana/README.md`.

## 2. Falsifiers and stop-the-line conditions

| Step | Falsifier (the claim fails if…) | Stop the line if… |
|---|---|---|
| C1 | it takes more than two packets (declarations too rich) | a declaration is edited in place instead of revised |
| C2 | a write bypasses the arxana route or the registry | a 'clusion id is computed without its roles |
| C3 | a bounded reverse lookup exceeds 5 s on the real store | a cache answers with its watermark behind |
| F1 | the three projections do not show the same record ids; an edited turn's reading is refused instead of relocated as a successor (c21 P4) | a fragment cannot name its record |
| F2 | two divergent edits do not both survive with their authors | nested, same-position or partial overlap picks a winner silently |
| F3 | the a93J02 divergence is relabelled a contradiction; the synthetic contradiction is marked without an attributable assessment citing the kernel run | a model-basis record shows as accepted |
| F4 | rejecting one stage changes another stage's standing; a cyclic composition is accepted | anything executes without an admission record |
| F5 | a coordinate move changes a semantic record, or a semantic change fails to reach both panels | renderer coordinates enter a 'clusion key |
| F6 | moving a diagram mints a `supports` 'clusion, or re-supporting one does not | pixel position enters identity |

## 3. Spikes not dispatched

- F3 Lean 4.29 integration check (a93J02 material builds under the apm-lean
  toolchain): low risk because apm-lean is already on 4.29; run at F3 start.
