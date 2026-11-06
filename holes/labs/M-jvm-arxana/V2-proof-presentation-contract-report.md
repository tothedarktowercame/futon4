# V2 — proof-presentation consumer contract verification report

**Mission:** `M-jvm-arxana`  
**Phase:** VERIFY  
**Verifier:** codex-22  
**Perspective:** `M-futonzero-prelim-practice` / proof-presentation consumer  
**Verdict:** **PASS — no stop-the-line contradiction**

## Verification basis

This verification treats D1–D7 and `A2-final-design-fitness-argument.md` as the
fixed design under review. It does not introduce an APM-specific alignment
mechanism or redesign JVM Arxana.

Status terms used below:

- `verified-by-spec` — the finalized design states the required identity,
  authority, query or separation rule without an unresolved semantic choice.
- `requires-spike` — the semantic contract is fixed, but feasibility or index
  shape still needs bounded empirical evidence before schema freeze.
- `requires-instantiate` — the design is coherent, but an implementation,
  fixture or executable tripwire is still owed.
- `contradiction` — two required design claims cannot both hold. None was found.

The consumer evidence is deliberately not treated as authority. Mark8 judged
2,454 informal steps across 347 problems, matched 636 to Lean declarations and
retained only 296 after 340 judge rejections
(`apm-lean/holes/labs/star-map/README.md:37-39`). The starmap says its Lean-only
tier mostly records formalization route rather than a study skill
(`apm-lean/holes/labs/star-map/README.md:25-27`). Three tutor lessons favor
ordered moves over tags but explicitly remain a three-example comparison
(`apm-lean/holes/labs/tutor/moves-vs-tags.md:49-53`). Mark8 also indexed a
faulty informal step without reporting a hole
(`apm-lean/holes/labs/tutor/lessons/a96A07.md:118-120`). These facts support
candidate evidence and evaluation records, not automatic correspondence.

## R1–R6 trace

| Requirement | Exact trace through the design | Status |
|---|---|---|
| **R1 — one move, multiple revision-pinned manifestations** | `proof/move` is a domain entity, not a core primitive; it may have zero or many authored-prose, Lean, generated-formal and generated-informal manifestations (D7:197-206). D1 supplies graph entity, artifact, immutable artifact revision and manifestation record classes (D1:34-47), then relates entity→manifestation→artifact with `:arxana/manifests` and manifestation→revision with `:arxana/observed-in` (D1:99-108). Headless Arxana owns the manifestation registry and correspondence assertions; futon1b persists them (D7:63-78). | `verified-by-spec`; the four-role fixture is `requires-instantiate`. |
| **R2 — partial, ambiguous, directional, absent and assessed correspondence** | A D4 assertion is immutable, directional and revision-pinned (D4:25-34); its standing includes `partial`, `ambiguous`, `unverified` and `stale`, and its cardinality may be one/many in either direction (D4:51-73). Queries return all ambiguous candidates and preserve direction and provenance (D4:146-152). Accepted/rejected judgments are separate immutable `correspondence-assessment` records; explicit absence names an expected role, source revision and bounded search envelope; conflicting assessments coexist without rewriting the assertion (D4:160-181). | `verified-by-spec`; assertion/assessment fixtures and mutation-refusal tests are `requires-instantiate`. |
| **R3 — authored and generated provenance cannot alias** | Artifact revisions are immutable content-addressed entities (D1:40-47). Manifestation IDs derive from authority, artifact family and semantic role; conflicting tuple definitions and role reuse are refused (D4:36-49, 190-205). The proof profile requires distinct IDs and provenance for generated and authored manifestations (D7:202-206). Generation must retain source revisions and tool/model identity/version (D4:204-205). | `verified-by-spec`; non-aliasing and regeneration tripwires are `requires-instantiate`. |
| **R4 — heterogeneous evidence never becomes authority by score alone** | D4 distinguishes what exact ranges, quote/context, source maps, DOM locators and transformations do and do not prove (D4:75-87). Lean/generated producer evidence names tool, version, build and exact revisions; symbol names are only candidate evidence (D4:183-188). Mining, anchors, models and human review may disagree while preserving independent provenance and source-specific standing (D4:204-209). D7 states directly that none becomes authoritative by score alone (D7:207-210); authority-side replay is required for promoted resolution evidence (D7:140-150). | `verified-by-spec`; ingestion and disagreement fixtures are `requires-instantiate`. |
| **R5 — all-manifestation and next-unmatched/ambiguous queries** | `correspondence/query` accepts revision-pinned locators and filters by direction, method, verification, role, revision and assessment verdict (D4:146-157). A client may supply a domain-owned move order and request the next absent/ambiguous item; Arxana applies but does not invent that order (D4:154-158; D7:215-218). Presentation remains client-owned (D7:215-218). | `verified-by-spec` semantically; query/index shape and cost are `requires-spike` (D7:140-145), followed by `requires-instantiate` query tests. |
| **R6 — correctness is separate from usefulness** | D7 separates transport, rendering integrity, correspondence correctness and usefulness (D7:220-228). Only the first three are JVM conformance; usefulness is namespaced consumer evidence linked to move, manifestation, revision, participant/cohort, instrument and outcome, and cannot change correspondence semantics (D7:230-240). A2 repeats that pedagogical usefulness is consumer evidence, not JVM conformance (A2:47-63). | `verified-by-spec`; layered end-to-end and usefulness records are `requires-instantiate`. |

## Worked semantic move

Use the domain move **replace a global invariant by a locally constant integer**
as a representative entity `proof/move:locally-constant-count`. The specific
mathematical content is a client-domain choice; none of the following changes
the Arxana core schema.

1. **Create the semantic entity.** The move receives a domain entity ID under
   D1's graph-entity rules. Its pedagogical label and ordering are namespaced
   domain data, not Arxana authority (D1:34-42; D7:197-203).
   **Status:** `verified-by-spec`.
2. **Register authored prose.** An authored-informal artifact has immutable
   revision `RI1`; manifestation `MI` has the authored-informal role and an
   `:arxana/manifests` edge from the move plus `:arxana/observed-in RI1`.
   **Status:** `verified-by-spec`; `requires-instantiate` for a real record.
3. **Register readable Lean.** Lean artifact revision `RL1` and manifestation
   `ML` remain distinct from `MI`. An exact range or declaration anchor may
   support a directional assertion `MI → ML`, but mark8/declaration matching is
   evidence only. If two Lean blocks remain viable, the assertion/query remains
   ambiguous and returns both (D4:69-87, 146-152).
   **Status:** `verified-by-spec`; actual alignment is `requires-instantiate`.
4. **Register generated formal rendering.** Generated TeX/HTML revision `RF1`
   and manifestation `MF` record the producing tool/version/build and exact
   input/output revisions. A source map can prove the producer associated a
   Lean coordinate with an output node; it does not prove semantic identity or
   editability (D4:64-66, 75-83, 183-188).
   **Status:** `verified-by-spec`; pipeline-to-record capture is
   `requires-instantiate`.
5. **Register generated informalization.** Generated-informal revision `RG1`
   and manifestation `MG` use a different semantic role and ID from `MI`, with
   model/tool/version and source revisions. A human may accept or reject an
   `ML → MG` assertion by adding an immutable assessment; the assertion itself
   does not change (D4:160-181, 204-209).
   **Status:** `verified-by-spec`; generation and review records are
   `requires-instantiate`.
6. **Allow missing roles.** The move remains valid if `ML`, `MF` or `MG` is
   absent because D7 permits zero-to-many manifestations. A claimed absence is
   not inferred from a missing row: it records expected role, source revision
   and bounded search envelope (D7:202-205; D4:177-180).
   **Status:** `verified-by-spec`; explicit-absence fixture is
   `requires-instantiate`.
7. **Query without forcing total alignment.** “Show this move everywhere”
   returns the available manifestation neighbourhood. “Next unmatched” uses a
   supplied proof-move order and assessment filters. Ambiguous candidates remain
   visible, and no client silently selects one (D4:146-158; D5:153-164).
   **Status:** `verified-by-spec`; index feasibility is `requires-spike`.

This walk has no step that requires every role to exist, every pair to align,
or every assertion to be accepted. Therefore the design does not force total
alignment.

## Provenance and disagreement checks

| Check | Evidence and result | Status |
|---|---|---|
| Different source bytes cannot alias as one revision | SHA-256 content identity and D1 invariant (D1:40-47, 171-175). | `verified-by-spec` |
| Authored and generated informalizations cannot alias | Different role tuples; conflicting definitions/role reuse refused; explicit tripwire (D4:36-49, 198-205). | `verified-by-spec`; `requires-instantiate` |
| Mined, model and human evidence can disagree | Independent provenance/standing and co-present conflicting assessments (D4:177-180, 204-209). | `verified-by-spec`; `requires-instantiate` |
| Explicit absence is attributable | Expected role, source entity/revision and bounded search envelope are required (D4:177-180). | `verified-by-spec`; `requires-instantiate` |
| Assessment cannot mutate assertion | Assertions and assessments are distinct immutable records; conflicts coexist (D4:160-181). | `verified-by-spec`; `requires-instantiate` |
| Assessment is not resolution acceptance or action admission | D4 states the distinction (D4:181); D7 preserves it as a single-locus invariant/profile rule (D7:127-128, 211-214). | `verified-by-spec` |
| Score cannot confer authority | D7 explicitly refuses score-alone authority and requires authority replay for promoted resolution evidence (D7:207-210, 140-150). | `verified-by-spec` |

## Acceptance-layer matrix

| Layer | JVM Arxana conformance? | Authority / evidence | Verification result |
|---|---|---|---|
| Transport | Yes | D3 protocol, revision refusal, journal and subscription semantics; headless Arxana protocol with futon1b atomic commit (D7:63-78, 181-195). | `verified-by-spec`; executable flows `requires-instantiate`. |
| Rendering integrity | Yes, for declared provenance/revision integrity—not readability | Producer/build identity, exact input/output revisions and revision-pinned locators (D4:183-188; D7:220-225). | `verified-by-spec`; capture fixture `requires-instantiate`. |
| Correspondence correctness | Yes | D4 assertion, ambiguity/direction rules, assessments and Arxana correspondence authority (D4:146-181; D7:63-78). | `verified-by-spec`; fixtures/tripwires `requires-instantiate`. |
| Usefulness | **No** | Consumer-namespaced evaluation linked to move, manifestation, revision, participant/cohort, instrument and outcome (D7:227-235). | `verified-by-spec`; evaluation data `requires-instantiate`. |

No pedagogical-success leak was found. The only use of readability or learner
success in the finalized design is the fourth, explicitly non-conforming
consumer layer. The PythonEmit→leantotex-python→LaTeXML-Oxide proof of concept
is expressly classified as a candidate rendering path rather than readability
or pedagogical evidence (D7:230-235). Pedagogical policy is also explicitly
deferred (A2:102-116).

## Failed and conditional checks

There are no `contradiction` results. The following checks cannot yet be marked
as executable verification:

1. **`requires-spike` — proof-profile query/index shape.** The semantic query
   is fixed, but D7 retains a query/index test before schema freeze
   (D7:140-145). The spike must demonstrate bounded all-manifestation and
   next-absent/ambiguous queries at representative corpus scale without moving
   ordering or scoring authority into Arxana.
2. **`requires-instantiate` — coherent correspondence fixture.** D4 requires
   the repaired positive fixture and all tripwires to be committed and run
   (D4:211-224). The four-role proof-move fixture should extend, not replace,
   that normative fixture.
3. **`requires-instantiate` — provenance and assessment tripwires.** Exercise
   non-aliasing, regeneration, conflicting evidence, accepted/rejected
   co-presence, bounded explicit absence and assertion immutability
   (D4:190-209).
4. **`requires-instantiate` — layered evaluation.** Demonstrate transport,
   rendering integrity and correspondence conformance independently from a
   useful/unuseful consumer rating. Changing the rating must not change any
   conformance result (D7:220-240).

These are implementation evidence debts, not unresolved meanings. A2 lists the
same fixture, query/index, client-digest and layered-evaluation work as VERIFY /
INSTANTIATE obligations rather than argument defects (A2:127-138).

## Verdict

**PASS — no stop-the-line contradiction.** R1–R6 trace coherently through the
final D1–D7 design. No evidence source becomes authority by score alone; no
total alignment is forced; authored and generated manifestations cannot alias;
missing and ambiguous correspondences remain representable; assessments are
attributable and non-mutating; and client usefulness remains attachable but
non-semantic. One bounded query/index spike and the enumerated INSTANTIATE
fixtures/tripwires remain before the consumer contract has executable evidence.
