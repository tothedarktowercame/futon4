# M-arxana-incubator — ARGUE, revised for balance between war room and war machine

claude-1, 2026-11-17, with claude-5, at Joe's request ("I'd like to see more of a balance between war
room and nomadology patterns"). A proposal to replace the argument in the mission's §4; claude-5 reviews
and merges. The mission file is not edited here.

**Conventions.**
- Deleuze and Guattari are quoted only from `~/notes/nomadology/src/atp.txt` (plateaus 12 and 13), matched
  with whitespace stripped. Proposition numbers follow `~/notes/nomadology/README.md`.
- Patterns are quoted from their `! conclusion:` lines in `futon3/library/`, verbatim (some quotations are
  a part of the line).
- A sentence that begins **Reading:** is mine. Everything that maps D&G onto the design is a reading, by
  analogy, as the crosswalk (`CROSSWALK-DG-WM.md`) says of its own mappings.
- Design names (E1–E6, R1–R7, I1–I8, D1–D8, the A-, I- and O- boxes) are those of `DERIVE-1-types.md`,
  `DERIVE-2-registry-and-fidelity.md` and the wiring `futon5/data/missions/arxana-incubator-exotype.edn`.

## 1. The argument from both sides at once

Joe: "Arxana does both information synthesis (to bring a projection of the graph onto the screen) and
information dissemination (to bring edits back to the graph)."

- **The war room brings the outside in.** It takes what was said and done, gives it types and addresses,
  and makes documents whose every part can be checked against its source. Its test is whether drift is
  detectable.
- **The war machine brings the inside out.** It follows the material where it leads, lets what was
  written for one purpose be taken up for another, and sends work out to people who can use it and
  answer back. Its test is whether anything outside the stack is better for it.
- **Where they meet.** D&G do not ask for one without the other. They describe "a single field of
  interaction in which royal science continually appropriates the contents of vague or nomad science
  while nomad science continually cuts the contents of royal science loose. At the limit, all that
  counts is the constantly shifting borderline" (Prop III).
- **Reading:** the design is right because it builds that borderline as two operations on one store of
  records, and neither can be done without the other.
  - **Projection** appropriates: it selects spans of the landscape into a form's fields, under limits,
    with every character traced (E4, I2).
  - **Dissemination** cuts loose: each edit, nip and new sentence is appended as its own record (A-edit),
    belongs to no one projection, and is material for the next (R2 with `:scope`, D2 "no privileged
    head", unwire).
  - A design with only the first is a form-filler whose output is dead once submitted. A design with
    only the second is a pile of notes nobody can check.
- **The one war-room pattern that already states the balance** is `war-room/wr-0-organise-without-apparatus`:
  an apparatus built to organise must carry "an open model, an available exit, and a verdict it cannot
  issue to itself." **Reading:** the three properties are the war-machine conditions (exteriority,
  cutting loose, an outside verifier) written as a war-room ruling, which is why the two sides can share
  one design.

## 2. Rationale citation

`@why war-room/wr-8-typed-files-are-sources-of-truth` — "Make the typed argument files the canonical
self-model and generate prose from them, so drift becomes detectable instead of perennial." The typed
records are canonical and every draft is regenerated from them. Draft 2's failure (0 of 51 sentences
verbatim, 42 untraceable) is the drift WR-8 names.

`@why war-machine/state-capture` — "A nomadic practice is captured when it begins reproducing itself
rather than following external flows; the test is whether today's work closes an external sorry or only
improves the infrastructure for closing sorrys." This holds the other side: each work package names the
outside thing it closes (the incubator application submitted by 2026-11-20; a PLoP alignment kept true;
the Fellowship letter).

`@why war-room/wr-0-organise-without-apparatus` — quoted above; the hinge between the two.

Also, as before: `war-room/wr-4-inhabit-before-building` ("Inhabit the surfaces that already exist before
building new infrastructure") — the design came from the working variants views and Mojira `vd`;
`war-room/wr-22-logic-model-before-code-is-a-sanctioned-verify-method` — the wiring is validated, and the
replay of the PLoP alignment history is a logic-model test available to VERIFY.

## 3. Element by element

Each element has a pattern from the war-room or War Machine catalogue side, a source from D&G or the
nomad patterns, what each contributes, and where one cuts against the other.

### 3.1 Projection (E4; A-project, A-render)

- **War room:** `wr-8`, above. A projection is prose generated from typed records, so a faithful
  rendering can be told from a lossy one.
- **D&G:** striated space is where "space is counted in order to be occupied" (Prop III). **Reading:** a
  form's fields and word limits are a striation, and a projection is how the material occupies it. D2
  (no privileged head) is the other half: for the nomad, "points for him are relays along a trajectory".
  A projection is a relay the material passes through, not where it lives.
- **Where they cut:** WR-8 makes editing prose in place a defect. Joe's hand edits are the "little
  LoRA-like style customisations" he wants. The design keeps both by making the hand edit a record (R2,
  E3) and never a file overwrite. The cost is that every stylistic touch is an act with an author and a
  time.

### 3.2 'Clusion kinds, including unwire (R1–R7, `:arxana/unwire`)

- **War room:** `war-room/wr-26-a-capability-switched-off-carries-its-re-arm-condition-in-writing-at-the-switch`
  ("Write a countable re-arm condition at the switch whenever an autonomous capability is turned off").
  Unwire carries `:actor :event-time :reason`, and re-wiring is defined (an unwire of the unwire, or the
  'clusion named again). Supersede v2 carries actor, time, reason and scope for the same reason.
- **D&G:** "nomad science continually cuts the contents of royal science loose" (Prop III). **Reading:**
  unwire is that cut made exactly: an edit is withdrawn from one projection without being destroyed or
  rewritten, and stays available to the others.
- **Where they cut:** append-only immutability (I1) counts everything, which is the royal side at its
  strongest. Smooth space is where "space is occupied without being counted" (Prop III). The design's
  answer is lazy registration: only the turns a document uses are registered, and the rest of the 23,146
  stay uncounted until followed. That is a real trade-off (§6), not a reconciliation.

### 3.3 Requirement and evaluation (R5, E5; A-requirements, A-eval-local, A-eval-global)

- **War machine catalogue:** `war-machine/operational-not-decorative` ("The test is whether the artifact
  changes behaviour when it fails") and `war-machine/ideal-actual-gap` (a system "needs two images - what
  it aspires to be and what it actually is - plus a computable comparison between them"). A requirement
  with its newest evaluation is that comparison, and a failing one must change what Joe or Nipper does
  next. `wr-0`'s "verdict it cannot issue to itself" is revision 3 of DERIVE-2 §5: attestations record
  whether the attester is independent.
- **D&G, for:** nomad science is "problematic, rather than theorematic"; it goes "from a problem to the
  accidents that condition and resolve it" (Prop III). **Reading:** a requirement is a problem stated
  before its answer, and VERIFY in Joe's Bergsonian sense asks whether it was stated well.
- **D&G, against:** capture works by "direct comparison and monopolistic appropriation", and "comparison
  always presupposes the appropriation" (Prop XII). **Reading:** a requirement list read as one score
  compares unlike things on one scale, and whoever sets the scale has appropriated the work. Mozilla's
  form is such a grid, set by someone else.
- **What the design does about it, and what it should add:**
  - Requirements carry their source and their evaluator kind (D4), so the form's grid, Joe's own
    criteria and a beneficiary's attestation stay distinguishable.
  - The seed lanes are not merged into one score (revision 9), and the list is evidence, never a gate.
  - **Proposed:** no aggregate across projections or across people. The defect count I suggested in the
    tetrahedron note is safe only as a distance for one projection against its own requirements.
  - `nomad/choose-the-grid` ("do not optimise the score — move to a venue whose scoring function you
    already satisfy") and `nomad/author-the-yardstick` ("build the measure") say the same from the nomad
    side: I2's per-character traceability check is a yardstick this project authors.

### 3.4 Nip and its three seeds (R6; A-nipper)

- **War room and catalogue:** `war-machine/ambient-pattern-retrieval` (a library "searched automatically
  on every conversation turn") is the memory seed's ancestor. `war-room/wr-19-tension-must-generate-not-only-rank`
  ("Wire structural tension as a candidate source that generates actions") is the gap seed: a failing or
  unmet requirement proposes spans, it does not only rank them.
- **Nomad patterns:** `nomad/letter-from-the-future`: "every clause that is not yet true is a work item."
  That is the gap seed as a writing method, adopted for the Fellowship letter.
- **D&G:** "One is obliged to follow when one is in search of the "singularities" of a matter, or rather
  of a material, and not out to discover a form"; "following is not at all the same thing as
  reproducing", and reproducing "implies the permanence of a fixed point of view that is external to
  what is reproduced" (all three in Prop III; see §9 on the attribution). **Reading:** the gap seed sets out from a form; the memory seed sets
  out from what was said; the following seed sets out from the material alone, proposing what recurs in
  the landscape and is in no projection. Only the third can produce a document nobody asked for.
- **Where they cut:** the gap seed is the form looking for content, which is what D&G say the
  follower is not doing. The design keeps the three lanes apart and records on each nip which seed
  proposed it, so a reader can ask how much of a document exists because a form asked for it. In the
  NEXT/PREV square (tetrahedron note §6) Joe's acceptance arbitrates only where the lanes conflict.
  **Reading:** that is the borderline of §1 at the scale of one passage.

### 3.5 Locality (I5, D8; `:local` and `:global` evaluators)

- **War room:** `war-room/wr-1-three-futon-split` ("Split a monolith whose concerns run at different
  timescales into separate, independently deployable loops"). Local checks run at write time; global
  ones on demand. They are two loops at two rates.
- **D&G:** the nomad's absolute "is achieved not in a centered, oriented globalization or
  universalization but in an infinite succession of local operations" (Prop V), and in smooth space "the
  linking of proximities is effected independently of any determined path" (Prop III). **Reading:** this is Joe's
  rhyme between MetaCA's locality rule and "what we can do using classical code in connection with a
  turn". The N/N-1 links between turns are such a linking of proximities. Locality is not only cheaper;
  it is how a landscape gets built without a view from above.
- **Where they cut:** `war-machine/half-blind-observation` warns that a surface covering half the system
  "creates false confidence in completeness". A list fed only by local checks is half-blind. So the list
  must show, for each requirement, which global evaluations have not run (revision 4's
  `:not-yet-evaluated`), and the global evaluator stays: it is the royal "fixed point of view", used on
  demand and labelled as such.

### 3.6 The outcome surface and fibres (views; directed 'clusions)

- **War room and catalogue:** `war-room/wr-10-next-move-surface-is-recursive-closure` (a "live surface
  read off the canonical self-model") and `war-machine/spatial-over-tabular` ("A spatial display that
  shows state as shape and colour earns inhabitation"). The outcome surface is such a surface, with
  regions in place of rows.
- **D&G:** "A path is always between two points, but the in-between has taken on all the consistency"
  (Prop V). **Reading:** pushing every record forward to its sinks (passages written, requirements met)
  is a striation, one dimension down. The fibre over a region, reached by following the same 'clusions
  backward, is the in-between: the sources, edits and alternatives that produced it, kept as several and
  not collapsed.
- **Where they cut:** a surface with regions invites counting what is met, which is §3.3's danger again.
  The fibre is what prevents it: no region is shown without a way back up to the material. A view that
  shows the surface and cannot open a fibre should be refused.

### 3.7 The return port (O-published, I-returns)

- **War room:** `war-room/wr-17-futon0-as-cyborg-futon7-as-markov-blanket` (outward-facing work has its
  own place) and `war-room/wr-27-a-loop-is-born-instrumented-for-its-gain` ("Ship every new
  consumption-to-acquisition loop with an instrument that measures how much returned demand changes what
  the loop does next"). **Proposed:** the port is born with that instrument: for each thing published,
  what came back, from whom, and which record it changed.
- **Nomad patterns:** `nomad/self-certifying-artifact` (artifacts "whose validity is machine- or
  institution-checked"): a projection that carries its sources is checkable by its reader.
  `nomad/owner-of-the-problem`: "the instrument is a proposal, not an application."
- **D&G:** "The war machine is exterior to the State apparatus" (Axiom I); and the war machine's "sole
  and veritable positive object (nomos)" is the occupation of smooth space and "the corresponding
  composition of people": "Make the desert, the steppe, grow; do not depopulate it" (Prop IX).
  **Reading:** exteriority means what is published is readable without Arxana, in plain text, HTML or
  EDN. The nomos means the object is that material is reused and contributors gain, which is Joe's
  "benefit should lead to viability". The count of met requirements is not the object.
- **Where they cut:** a return port can itself capture: others' edits and reviews come in and become
  the stack's material. The condition that keeps it from being appropriation is attribution that
  survives (E3 author, I7 no silent copy, T6), and an exit: a contributor can take a projection of
  their own material out in an open form.

## 4. Where one side checks the other, in one table

| Element | The war room would overreach by | The war machine would overreach by | What the design holds |
|---|---|---|---|
| Projection | treating the form's fields as the document | never fixing a version anyone can cite | pinned versions, no privileged head |
| 'Clusions | counting every turn before use | edits with no author or reason | append-only acts, lazy registration, unwire with a reason |
| Requirement | one score over unlike things | problems never checked | evaluations as evidence, lanes unmerged, no aggregate across people |
| Nip | form-first search only | accepting what cannot be traced | three seeds recorded; spans resolve to turns |
| Locality | a global pass for every status | local statuses read as complete | local at write time; global on demand, and shown when not yet run |
| Outcome surface | a dashboard of counts | a graph nobody can read | surface and fibre together |
| Return port | importing others' work as material | publishing with no way back | open formats out; attributed returns in |

## 5. Theoretical coherence

- **Nelson, patch theory, bitemporal storage** (as in the present §4) are the royal instruments:
  permanent addresses, patches as records, event time as valid time. They stand unchanged.
- **D&G's borderline** (Prop III) supplies what those lack: a reason why edits must be detachable and
  why a document is not the home of its content. It is coherent with patch theory, which already has no
  privileged head.
- **Bergson and Prop III** agree on VERIFY: a problem is posed, not given, and the check is on the
  posing.
- **The 3/2 square and MetaCA locality** (tetrahedron note §5, §6) are specialisations: an arbiter asked
  only on conflicts, and local operations with a record standing in for a wider view. Both are my
  readings and partly speculative; the argument here does not depend on them.
- **One tension is not resolved.** The crosswalk notes that the active-inference formalism is, in D&G's
  terms, an axiomatic: it "deals directly with purely functional elements and relations whose nature is
  not specified" (Prop XIII; the crosswalk cites it under Prop XIV). If requirements become War Machine want tokens, they enter that scoring.
  Whose values set it is the open question the crosswalk already records under Prop XII.

## 6. Trade-off summary

Kept from the present §4: no real-time co-editing; statuses not stored on links (D3); file-backed first
for the incubator (D7); the deferred items. Added or restated for balance:

- **Counted against occupied.** Lazy registration keeps the store to what documents use. Given up: a
  complete index, so the following seed's "in no projection" query is costlier and approximate.
- **No single number.** Refusing an aggregate across projections and people gives up a simple progress
  figure. Gained: the list cannot become the object.
- **Form-first against material-first.** Three seed lanes mean three lists for Joe to read where one
  ranked list would be quicker.
- **Open outward forms.** Plain text and HTML drop links and margin notes. Renderers declare the loss
  (revision 10). Gained: nothing published needs the stack to be read.
- **An instrumented return.** Recording what comes back costs a record per publication and may show that
  little does.

## 7. Generalisation notes

- **Within the stack:** the same types serve the four facets (organisation, simulation, automation,
  documentation), with the PLoP 2026 paper as the end-of-mission test. The balance generalises too: each
  facet has a striating half (readings, registrations, proofs, drafts) and a half that cuts loose
  (corrections, re-reads of old dumps, patterns born from holes, reuse of passages).
- **Beyond it:** any group whose working record is a stream of utterances can register it as a landscape.
  What changes is who may attest, and who authors the requirements. The second matters more than it did
  in the earlier argument: a group that only fills other people's forms has the war-room half alone.

## 8. Plain-language argument

When people write with AI agents, the words drift away from where they came from, and the result is
shaped by whatever form it was written to fill. This design keeps what people actually said as a
permanent record and builds each document as an arrangement of pieces of that record, so anyone can see
who said what and whether it still holds. It also works in the other direction: every edit and new
sentence goes back into the common record as its own piece, free to be taken up in the next document, and
what is published can be read without our tools and answered by the people it was written for. The
checklists a funder or a reviewer sets are kept as evidence about the writing, not as its purpose. That
is what lets a group get credit for its work, catch its mistakes, and have the work grow in other
people's hands.

## 9. What I checked, and what I did not

- **Checked:** all 21 D&G quotations in this file (a sentence quoted in two halves is matched as one)
  occur in `atp.txt` with whitespace stripped. All pattern quotations are in the patterns' conclusion
  lines. The wiring's box names were read from the EDN.
- **Read:** the mission's HEAD and §4 in full; DERIVE-1's tables, invariants and decisions; DERIVE-2's
  unwire entry and §5; claude-5's pattern survey; the nomadology README and crosswalk.
- **Not read:** the body of each pattern beyond its conclusion; DERIVE-2 §1, §3 and §4; the gap
  analysis. Proposition numbers were assigned by locating each quotation between the PROPOSITION headings of
  `atp.txt`.
- **Two attributions differ from the mission file and the crosswalk.** "One is obliged to follow …" is at
  `atp.txt` l.14897–14899, inside Prop III (l.14457–14987), not Prop VIII (which begins at l.16144); the
  crosswalk and the mission's §4 cite it as Prop VIII. "deals directly with purely functional elements …"
  is at l.18132–18134, inside Prop XIII, not Prop XIV (which begins at l.18377). Following is still the
  theme of Prop VIII (metallurgy and the matter-flow), so the argument is unchanged; the citations should
  be corrected when this is merged.
- **Proposals that go beyond the present design**, for claude-5 and Joe to accept or drop: no aggregate
  across projections or people (§3.3); the return port instrumented from the start (§3.7); a view that
  cannot open a fibre is refused (§3.6); a contributor's exit in an open form (§3.7).
