# A1 — JVM Arxana design-fitness argument (report)

Mission: `M-jvm-arxana`. Phase: ARGUE (shadow of DERIVE). Owner: Zai-1.
Baselines: pass 1 — DERIVE `784a2ab` + `aac8544`, with D1 draft `9c8b515`;
pass 2 — DERIVE checkpoint D2 `9ba7974`, prior ARGUE `65c6eac`;
pass 3 — DERIVE checkpoint D3 `bc81c03`, prior ARGUE `6dcdb54`;
pass 4 (this revision) — DERIVE checkpoint D4 `78a0e16`
(`holes/labs/M-jvm-arxana/D4-correspondence-model.md`), prior ARGUE `98188fb`,
per `ARGUE-DERIVE-loop.md`.
Status: fourth (progressive) pass; §1–§9 carry the pass-1 argument with
pass-2 deltas in §10, pass-3 deltas in §11 and pass-4 deltas in §12. Nothing
here claims ARGUE or the mission is complete, and nothing here selects an
architecture on DERIVE's behalf.

Citations: `P1`…`P6` are the MAP reports in this directory; `D1` is
`D1-canonical-model.md`; `M` is `holes/missions/M-jvm-arxana.md`. Empirical
premises cite MAP reports; design premises cite DERIVE work products or name
the decision still owed.

## 1. Principal fitness claim

**PC.** The evolving JVM Arxana design is fit for its purpose — making one
authority answer every identity, relation, scope, correspondence and
action-grounding question identically for all clients — *conditional on the
D1–D7 work products landing as drafted and on three specific obligations being
discharged* (see §6). The design is fit in the strong sense: given the MAP
evidence, each load-bearing design commitment is either supported by an
observation or is an explicitly named open decision, not a hope.

Why this is the right principal claim: the mission's candidate invariant (M,
"Candidate invariant") is an equivalence claim over clients. MAP established
that nothing today satisfies it — four mutually unintelligible protocols
(P3 §1–§3), no executor for strategy declarations (P2 "Principal finding"),
and an authority narrower than its own client implies (P1 §0–§1). The design's
fitness is therefore its capacity to close an evidenced gap, not to preserve a
fiction of an already-shared model.

### Subordinate claims

- **SC1 Shared authority.** One JVM-side component owns each authoritative
  question; clients receive projections. Support: P1 §0 (futon1b is the live
  authority); P3 §3 (vocabulary collisions prove no latent shared protocol).
  Owed: D7's authority assignment per question.
- **SC2 Portable resolution.** Scope resolution is a deterministic, revision-
  pinned computation over stored strategy data and artifact bytes. Support:
  P2 Principal finding and §1 (the anchor cascade is algorithmically pure;
  finder declarations are inert data). Owed: D2 finder/ambiguity semantics.
- **SC3 Manifestation correspondence.** Correspondence is a provenanced
  assertion between manifestations observed in *named revisions*, not a
  mapping between live files. Support: P4 Principal finding and Trace 1–3
  (quote-, range- and transformation-based identity; the draft8 revision
  contradiction). Owed: D4 repaired fixture and assertion schema.
- **SC4 Consistency without premature CRDT.** Revision-checked
  single-authority transactions are the correct current contract, with a
  recorded falsifying trigger for reopening CRDT. Support: P5 §§3–6 (both
  observed failures are single-authority/freshness failures; no observed
  concurrent legitimate multi-writer intent). Owed: D3 state machine and the
  CAS mechanism D1 §"Storage adapter" truthfully reports absent.
- **SC5 Thin clients.** Emacs and VS Code can be thin because every
  editor-local concept (markers, positions, decorations) is disposable
  projection. Support: P3 §1–§2 (each existing client demonstrates a required
  behaviour, none depends on owning identity). Owed: D5 command/view spec and
  Capability Preservation Matrix.
- **SC6 Inference-ready action boundary.** A read-only intent→candidate-action
  compilation with consumer policy namespaced outside is the waist four
  consumers already empirically use. Support: P6 "Principal finding" and
  §"Interface observations". Owed: D6 compiler spec and end-to-end example.

## 2. Claim / support / objection structure

| # | Claim | Support (empirical = MAP; design = DERIVE) | Strongest objection | State |
|---|---|---|---|---|
| C1 | The gap is real and correctly stated | P1 §0 (client on dead port 7071; live service on 7072/7073); P3 §3 (`file` means index vs filename) | "Just fix Emacs's port and add a VS Code mode" — refuted by P3: the protocols disagree in kind, not configuration | Supported |
| C2 | Futon1b's record families can host the extended vocabulary | P1 §1 (entity/hyperedge tables, extension fields, L0–L4 gates, read-back verification); D1 "Namespaced shapes" | Extending via `props` risks unindexed queries; D1 Q2 (assertion shape vs indexing) is open | Supported, one obligation (O-D1b) |
| C3 | Artifact revision must be content-addressed identity, not an API parameter | P1 §1 (server ignores `as-of`/`version`); P4 Principal finding (fixture revision contradiction); D1-DR2; **pass 4: D4 fixture manifest applies it concretely** (per-artifact revision IDs, one coherent build, rejection tripwire `fixture/revision-mismatch`) | Content-addressing every revision makes historical browsing storage-heavy; no observed need contradicts — accepted trade | Supported |
| C4 | The three-stage anchor algorithm ports and is the first tripwire | P2 Principal finding, §1 (`arxana-links.el:697`; tolerates unrelated prefix/suffix edits) | ~~`fuzzy-matched` not produced and finder-level ambiguity undefined (P2)~~ **Pass 2: repaired by D2-DR2** — all candidates enumerated, ambiguity explicit, fuzzy never relabelled verified; remaining objection is that fixture tripwires (D2 "Tripwire tests", incl. the duplicate-paragraph example 2) are specified but not yet run | Supported (D2), obligation narrowed to O-D2a |
| C5 | Resolutions are protocol-only until cited (D1-DR3) | D1 decision record; **pass 2: D2-DR5** (entity outcome + typed `:arxana/resolution-evidence-for` hyperedges on promotion, idempotent by semantic digest) | An audit trail that only persists cited resolutions may lose evidence of *wrong* resolutions; and the promotion acknowledgement itself carries no durable provenance — see §10, O-D2c | Supported (D1+D2), one obligation (O-D2c) |
| C6 | ~~Draft8 is a valid normative correspondence fixture~~ **Pass 4:** the original draft8 set is adopted as a *negative* fixture, and the normative positive fixture is a repaired manifest produced by D4's rejection/repair procedure | P4 §§1–3 (1,630 anchors, three correspondence classes, revision contradiction); D4-DR1 + "Repair and rejection procedure" | ~~Its scope hashes match `.pristine/` while page anchors verify against live sandbox sources~~ — **repaired in design, not yet in fact**: the repaired manifest/map is created and committed only in INSTANTIATE; until then no positive fixture exists, so C6 remains conditional on that execution (O-D4a narrowed, not discharged) | Supported at design level; execution owed (O-D4a) |
| C7 | No CRDT is required now; the falsifying trigger is recorded | P5 §§3, 5–6; **pass 3: D3-DR3 + CRDT re-arm section** (revision preconditions, refusal, journalling, resubmission; re-arm condition recorded with incident/artifact class/convergence property — the WR-26 re-arm carried at the contract) | ~~The XTDB API has no revision CAS and no subscription stream~~ **Pass 3: answered in design** — D3-DR2 makes an atomic stale-refusal mechanism a *conformance requirement* with two specified candidates; the remaining objection is that the sequencer candidate's lease/fencing proof is owed (O-D3a) | Supported empirically; design now specified, one proof owed (O-D3a) |
| C8 | Four consumers already share the narrow waist | P6 §§"Interface observations", "Principal finding" | Same-shape-different-name records (P6 §"Same shape") tempt premature standardization; P6 explicitly declines it — the design must too | Supported |
| C9 | Emacs salience/Plexus are correctly excluded from the canonical model | P1 §1 (salience fields silently dropped; Plexus is prose with no route/writer/reader); D1 record classes | Excluded features may be load-bearing for users; D1 rule 6 (report `unsupported`, never silently accept) is the right guard, but donor behaviour audit (D5 matrix) must confirm nothing preserved depends on them | Supported, obligation O-D5a |
| C10 | The design serves the theoretical anchor (checkable argument as computational action) | M "Operator anchor", "Theoretical anchoring"; SC6 | The read-only compilation path only *describes* actions; the anchor's "computational action" half lands in later missions — this mission's contribution is grounding, not autonomy (M "Out") | Supported as scoped |

## 3. Architecture alternatives — strongest case, strongest objection, decisive input

No selection is made here; that is D7's job.

### A1. Extended futon1b authority

- **Strongest case.** Fewest moving parts; the authority and the store share
  one consistency domain, so stale-refusal and transactions can be enforced at
  the same boundary that already has L0–L4 gates and read-back verification
  (P1 §1). `storage/canonical-interface` and `storage/deterministic-substrate`
  are satisfied by extending the one canonical HTTP surface.
- **Strongest objection.** Client protocol evolution (D3 position vocabulary,
  subscriptions, envelopes) becomes coupled to graph-store releases; every
  protocol iteration risks the durable authority. P3 shows client vocabularies
  are still unsettled (`file`, `text`, `agent` collisions) — coupling an
  unsettled protocol to the store is the risk MAP just spent a phase escaping.
- **Decisive input.** D7's failure-isolation and "cost of adding a second
  editor" matrix rows; how often D3 is expected to revise before VERIFY.

### A2. Headless Arxana service over futon1b (via D1 adapter)

- **Strongest case.** Only this option makes SC1/SC2 jointly enforceable by
  construction: strategy execution, revision pinning and correspondence
  computation live in one deterministic place (P2: the cascade is pure and
  portable), while the adapter's honest `capabilities` map (D1) keeps the store
  boundary truthful. The comparison matrix criteria in M (deterministic
  strategy results, failure isolation, second-editor cost) all favour it.
- **Strongest objection.** It is also the only option that *must* solve the
  missing-CAS/subscription problem above the store (P5 §2; D1 capabilities
  report `:entity-compare-and-set false`, `:subscription false`) — a new
  component owning revision checks introduces a two-phase surface where the
  service's cache can disagree with futon1b's truth. Deployability cost of a
  second JVM service.
- **Decisive input.** D3/D7: the specified mechanism (sequence numbers?
  content-hash preconditions, as the WYSIWYG path already uses per P5 §2?)
  and its recovery semantics tested against the two 2026-08-08 incidents.

### A3. Graph authority with client-hosted evaluators

- **Strongest case.** Cheapest reuse: editors already evaluate against buffers
  (P2 anchor cascade), and the WYSIWYG path already demonstrates guarded
  edits, typed refusal and journalling client-side (P3 §1; P5 §2).
- **Strongest objection.** It contradicts the candidate invariant's core:
  "same answer from the JVM authority" becomes "similar local answers from two
  evaluators". P3's evidence is decisive against it: the four existing clients
  are *exemplars of behaviours*, not implementations of one latent protocol —
  two independent evaluators would re-create today's mutual
  unintelligibility inside the new model.
- **Decisive input.** A demonstration that two client-hosted evaluators can be
  proven equivalent on the D2 fixture set (deterministic replay plus golden
  outputs). Absent that, the objection stands unanswered; if DERIVE keeps A3
  alive, that proof is its burden.

## 4. Pattern cross-reference (`futon3/library/`, searched not decorated)

- **`storage/graph-memory-contract`** — the mission's whole premise; D1's
  namespaced shapes and the `arxana/` namespace rule *are* this pattern
  applied. Requires no DERIVE revision; D1 satisfies it.
- **`storage/canonical-interface`** — bears on A1 vs A2: whether the canonical
  HTTP surface is the store's or the headless service's. D7 must answer which
  interface is *the* canonical one once, not per client.
- **`storage/identity-uniqueness`** — D1's identity rules (content-addressed
  revisions; stable `hx:<type>:<sorted-endpoints>` IDs) apply it; the
  `mission-scope` uncensused family (P1 §1 discrepancy D3) is a live
  counter-example *inside the store*, so D1's write-freeze rule 3 is required,
  and the census tripwire is where this pattern gets teeth.
- **`storage/error-layer-hierarchy`** — D1's typed-error preservation rule
  ("do not flatten typed refusals into `nil`") is this pattern; D3's error
  algebra must continue it upward through the protocol envelopes.
- **`invariant-coherence/single-locus`** — the candidate invariant is a
  single-locus claim: for each (question, artifact-revision) pair exactly one
  authoritative answer record. Framing it this way gives DERIVE a check
  vocabulary: violations enumerate the offending (I, A) pairs rather than
  "clients disagreed". **Suggests a DERIVE revision:** state the D7
  consolidated invariants in single-locus form so they are executable.
- **`invariant-coherence/protocol-family-naming`** — the
  `shared-authority/entity-identity` … `shared-authority/action-grounding`
  family in M already follows it (shape + instance naming). No revision
  needed; D7 should keep namespace discrimination rather than inventing a
  "sub-invariant" element.
- **`cascades/repointable-declarations`** — finder/strategy declarations are
  repointable stored declarations; D2's strategy-revision identity
  (content-digest versioning, D1 record classes) matches. Bears on O-D2a:
  ambiguity semantics must version with the strategy, not the executor.
- **`storage/determinism-vs-expansion`** and **`storage/schema-evolution-stability`**
  — bear on D1's extension-fields approach (C2 objection): expanding meaning
  through `props` must not let queries drift; D1 Q2 (indexing) is where this
  is decided. Flagged as a pattern requiring a DERIVE answer, not decoration.

No pattern was found that argues *against* the design as drafted; the
pressures are all "make X explicit/checkable", which is what §6 records.

## 5. War-room rationale

- **@why war-room/wr-7** (frames interoperate or they don't exist): the
  mission is exactly WR-7's discipline at protocol scale — P3's finding that
  the four clients "share only a weak vocabulary" is the observed
  non-interoperation; the design builds the model only where an exercised port
  (each client's demonstrated behaviour) demands it.
- **@why war-room/wr-8** (typed files are sources of truth): D1's typed
  record classes over legacy JSON plist blobs (compatibility rule 4: retain
  bytes, project to typed values) and the prohibition on prose-only Plexus
  (P1 §1) both rest on WR-8.
- **@why war-room/wr-26** (a capability switched off carries its re-arm
  condition at the switch): the deferred CRDT decision is a capability
  switched off, and P5 §6's "smallest falsifying observation" is precisely the
  countable re-arm condition WR-26 requires, carried at the D3 contract
  rather than in a discussion. Same for D1 rule 6 (salience writes report
  `unsupported` with the re-arm being a live writer/reader/invariant).
- **@why war-room/wr-24** (a removed constraint does not remove the discipline
  it supplied): Emacs's dropped salience/Plexus conventions (P1 §1) supplied
  ad-hoc salience discipline; D1 removes the fields from the canonical model
  but the mission must still say where that discipline now lives (outside, or
  a later schema proposal) — recorded as O-D1c.
- **@why war-room/wr-3** (social exotype before implementation): applies to
  D7's wiring diagram requirement — authority boundaries must be drawn and
  validated before any INSTANTIATE code. Not yet discharged; it is D7's
  deliverable, tracked as O-D7a.

## 6. Design-obligation ledger (D1–D7)

| ID | Obligation / defeater | Settles | Owed by | State (pass 2) |
|---|---|---|---|---|
| O-D1a | Strategy-finder independent entity vs embedded (D1 Q1) | C2, SC2 | D2 | **Discharged** by D2-DR4: embedded `(strategy-revision-id, ordinal)`, entity only when independently cited |
| O-D1b | Resolution assertion: entity+hyperedge vs single doc; futon1b indexing behaviour (D1 Q2) | C2, query-ability | D2/D7 | **Narrowed**: D2-DR5 selects entity + typed hyperedges (avoids property scans); the futon1b-indexing confirmation half remains, owed to D7 acceptance tests |
| O-D1c | Name where salience/Plexus discipline lives after exclusion (WR-24) | C9 | D7 or follow-up mission | Open |
| O-D2a | Finder dispatch, ordering, ambiguity semantics for the five inert forms; mark each executable/translated/advisory/unsupported | SC2, C4 | D2 | **Narrowed**: semantics defined (cascade modes, no implicit fallback, legacy classification table); remaining: authored translation receipts for existing stored strategies, pinned embedding inputs, and the tripwire fixtures actually run |
| O-D2b | Authority decision for artifact text access (who reads bytes) | SC2, A3 objection | D2/D7 | **Narrowed**: D2-DR3 + `ArtifactProvider` settle the contract (content-addressed revision, digest re-verified, no anonymous buffer text); which deployment implements it (repo reader / blob store / service cache) is now purely a D7 choice |
| O-D2c | **New (pass 2).** Promotion of `resolved-unverified`/`ambiguous` requires "explicit caller acknowledgement", but the stored assertion records no acknowledgement provenance — who/what accepted an ambiguous or unverified result is not durably part of the evidence | SC2/SC6 audit chain, C5 | D2 revision or D6 | **Open defeater** (narrow seam, high audit value) |
| O-D2d | **New (pass 2).** State where per-finder evidence events live when the result is *not* promoted — protocol-only per D1-DR3 is the implied reading, but D2's "every attempted finder appends an immutable event" does not say so | C5, storage growth | D2 clarification | Open (small) |
| O-D3a | Revision-CAS + subscription mechanism above futon1b; recovery semantics replay-tested against both P5 incidents | SC4, C7, A2 objection | D3/D7 | **Narrowed (pass 3)**: two conforming mechanisms specified (store-side conditional transaction; durable sequencer with lease/fencing), state machine, traces 1–6; remaining: D7 selects one and *proves* atomicity/fencing — the split-brain proof is the open half |
| O-D3b | **New (pass 2).** Align D2's read-side `stale-input` with D3's mutation-side `stale`/`conflict` outcomes so one word never names two refusal reasons across the protocol | SC4, error algebra | D3 | **Closed (pass 3)**: distinct definitions plus tripwire trace 7 |
| O-D3c | **New (pass 3).** D3 permits architecture 3 to survive as read-only client evaluation; but D2 evidence events produced by a client-hosted evaluator are client-attested, so promotion assertions would rest on client-claimed evidence. Decide whether conforming evidence requires authority-side evaluation | SC1, SC2, A3 disposition | D7 | Open |
| O-D4a | Repair draft8 revision contradiction before normative adoption | C6, SC3 | D4 → INSTANTIATE | **Narrowed (pass 4)**: D4-DR1 makes the contradiction a required *negative* fixture with a rejection/repair procedure and tripwires; "closed at design level" only — the repaired manifest/map is created and committed in INSTANTIATE, so execution is still owed |
| O-D4b | Durable manifestation/DOM identity scheme (fixture shows none exist) | SC3 | D4 | **Narrowed (pass 4)**: manifestation IDs minted by the fixture/model author, independent of DOM locators (which stay evidence); closed at design level, but the minting rule itself is ungoverned — see O-D4c |
| O-D4c | **New (pass 4).** Manifestation IDs are "minted by the fixture/model author" with no uniqueness/namespace/minting rule; a duplicate or reused manifestation ID would silently violate single-locus for (manifestation, semantic role). Specify the minting governance before INSTANTIATE scales beyond draft8 | SC3, single-locus | D7 or INSTANTIATE | Open |
| O-D5a | Capability Preservation Matrix incl. whether any preserved behaviour depends on dropped salience | C9, SC5 | D5 | Open |
| O-D5b | **New (pass 3).** Map current typed WYSIWYG refusals and edit journal into the D3 error algebra and write contract (named by D3 itself) | SC4, SC5, C9 | D5 | Open |
| O-D6a | End-to-end compile example on the repaired fixture; consumer attachment notes | SC6, C10 | D6 | Open |
| O-D7a | Architecture selection with completed matrix, exotype diagram (WR-3), invariants in executable single-locus form | PC, SC1, A1–A3 | D7 | Open |
| O-D7b | **New (pass 3).** Assign the owner of journal, head movement, lease/fencing tokens and event retention (named by D3) | SC1, SC4 | D7 | Open |
| O-D7c | **New (pass 4).** Assign the fixture builder and correspondence-assertion authority (named by D4) | SC3 | D7 | Open |

Unsupported claims found during argumentation were converted to obligations
here rather than argued around: C6 is an active defeater (the strongest
empirical input the argument has is currently self-contradictory), and C7's
comfort ("no CRDT needed") must not obscure O-D3a (the non-CRDT contract is
also unbuilt).

## 7. Provisional trade-off summary

What the design gives up, and why (provisional where D-items are open):

- **Compatibility over cleanliness.** It keeps futon1b's two record families
  and the legacy MD5/blob imports rather than migrating (D1 rules 1–5),
  accepting prop-based extension and a normalizer for the uncensused
  `mission-scope` family, because MAP showed replacement risk without
  authority gain. *Provisional on O-D1b.*
- **Determinism over immediacy.** Resolutions are recomputed from named
  revisions rather than trusted from clients (D1-DR3), accepting recompute
  cost, because identity-from-offsets is the failure P4 exposed.
- **Deferred collaboration over speculative CRDT.** Single-authority with a
  recorded falsifying trigger (P5 §6), accepting that a trigger event forces
  mid-lifecycle protocol revision. *Provisional on O-D3a being built at all.*
- **Grounding over autonomy.** Read-only candidate actions (M "Out"), so the
  "computational action" thesis advances only as far as inspectable grounding
  in this mission.
- **A3's cheapness is knowingly at risk.** If D7 selects A2, the project pays
  a second service and the CAS problem to buy the candidate invariant; if A1,
  it pays protocol-store coupling to buy one consistency domain. The argument
  does not pre-decide this.

## 8. Generalization notes

The design's shape generalizes beyond Emacs/VS Code: it is the recipe "find
the questions clients must answer identically, make each a single-locus
record, pin every derived answer to a content-addressed revision, and keep
domain policy namespaced at the boundary." Applying it elsewhere (e.g. other
editors, other artifact media) requires exactly the three things MAP had to
produce here: a live-store census (what is actually enforced, not what the
client sends), an executor census (what actually runs, not what is declared),
and a contradiction-bearing fixture. What would need to change: the record
classes are Arxana-specific; the adapter/capabilities pattern, the
revision-pinning rule and the WR-26-style deferred-decision discipline are
the portable parts.

## 9. Plain-language argument

Today, the tools that show and edit Arxana material each keep part of the
picture and answer the same question differently, so we cannot trust any one
answer. The plan is to build one service that owns the answers: it names each
piece of writing by exactly what its bytes are, so "which version were you
looking at?" always has a checkable answer, and it records how it matched
things up as evidence rather than assumption. Editors become windows onto
that service — two people in two editors see the same answer because there is
only one place the answer lives. Asking "what would changing this do?"
produces a written-down, inspectable proposal instead of a surprise, while
the decision to act stays with people and their own tools.

*(Every clause above is provisional exactly where §6's obligations are open;
the plain-language claim inherits the ledger's conditions, especially O-D3a
and O-D4a.)*

## 10. Pass 2 delta — D2 checkpoint (`9ba7974`)

Re-tested: SC2, C4, C5, the single-locus invariant, and O-D1a/O-D1b/O-D2a/O-D2b.
Statuses are recorded in §2 and §6; this section gives the reasoning.

### 10.1 SC2 and the five requested features — sufficiency and coherence

- **Declaration-only import** (D2-DR1): sufficient and correct. It refuses to
  manufacture execution semantics that P2 proved never existed; legacy records
  stay visible in the digest. Coherent with D1 compatibility rule 4.
- **Explicit translation** (D2-DR1 + legacy classification table): sufficient
  in form — every one of the five forms has a classification and a required
  translation. Two residual insufficiencies: (a) translations are authored
  receipts, so *who* translates and reviews is process debt outside D2's text
  (narrowed into O-D2a); (b) `surface-form-hybrid` expands into a composition
  whose sub-steps must each re-enter the classification — a translated hybrid
  containing an unpinned embedding step is advisory inside an executable
  strategy; D2 implies but does not state that a strategy's executability is
  the conjunction of its finders' statuses. Flagged for the D2 tripwire set.
- **Ambiguity preservation** (D2-DR2, result algebra): sufficient and the
  strongest part of the checkpoint. "Ordering never turns a tied or multiple
  set into uniqueness" is exactly the repair C4 demanded; statuses never
  conflate missing/unsupported/failed. The duplicate-paragraph example shows
  the defeater caught, not asserted.
- **ArtifactProvider digest verification** (D2-DR3): sufficient and coherent.
  Because the revision ID *is* the sha256 of the canonical bytes, re-computing
  the digest closes the integrity gap regardless of deployment; a provider
  cannot lie about which revision it served. The remaining question is purely
  deployability (D7) — recorded in O-D2b's narrowing.
- **Entity+hyperedge promotion** (D2-DR4/DR5): mutually coherent with D1-DR3
  and with the D1 relation catalogue (`:arxana/resolution-evidence-for`, 3+,
  roles match). Idempotent upsert by semantic digest makes the
  **single-locus invariant** checkable here: for (strategy-revision,
  artifact-revision, subject) there is exactly one assertion per outcome
  digest, and a new artifact revision mints a new assertion rather than
  mutating the old — both halves of single-locus (existence and uniqueness)
  are now expressible as a query. No violation found.

### 10.2 New defeater exposed

**O-D2c — acknowledgement provenance.** D2's promotion path admits
`resolved-unverified` and `ambiguous` results into durable assertions on
"explicit caller acknowledgement", but the stored assertion records outcome,
digests and evaluator version only. The acknowledgement — *which* caller,
under what policy, accepting which ambiguity — is exactly the institutional
fact this mission's evidence discipline exists to preserve; without it, the
audit chain is broken at the precise point where a human or tool judgement
turned an unresolved answer into durable evidence. This is a narrow seam
between D2's promotion shape and D6's admission decisions; either D2 records
an acknowledgement provenance field on the assertion, or D6 explicitly owns
the acknowledgement as an admission-style record that must reference the
assertion. It does not block D3 work.

Two smaller exposures: O-D2d (where non-promoted evidence events live) and
O-D3b (`stale-input` read-side vs `stale` mutation-side vocabulary alignment
when D3 lands).

### 10.3 Effect on the alternatives (no selection)

D2 strengthens A2's and A1's case and leaves A3's objection untouched: a
deterministic evaluator over digest-verified immutable bytes is now fully
specified, and it is the kind of component that must live behind one
authority boundary — client-hosted evaluators (A3) would have to reimplement
the digest-verification and evidence-sequence discipline per editor, which is
precisely the duplication the candidate invariant forbids. Noted as evidence,
not as a selection.

### 10.4 Checkpoint receipt summary

Changed obligations: **O-D1a discharged; O-D1b, O-D2a, O-D2b narrowed;
O-D2c, O-D2d, O-D3b added.** Highest-value challenge: O-D2c
(acknowledgement provenance at the promotion seam).

## 11. Pass 3 delta — D3 checkpoint (`bc81c03`)

Re-tested: SC1, SC4, C7, O-D3a/O-D3b, and all three alternatives. Challenged:
atomicity, crash recovery, fencing, journal/subscription semantics, status
distinctions, CRDT re-arm, and whether the non-conformance ruling for
client-hosted writes improperly pre-selects against architecture 3.

### 11.1 Requested challenge areas

- **Atomicity.** The serialized decision (compare → validate → journal →
  immutable revision → head move → outcome) is sound *as an ordering*; its
  soundness rests entirely on which mechanism serializes it. Mechanism 1 puts
  the whole sequence in one XTDB transaction — one truth, no gap. Mechanism 2
  (durable sequencer) achieves it only if reconstruction-before-serving and
  command-ID-idempotent futon1b writes hold. D3 says the right things; the
  proof is owed (O-D3a's open half).
- **Crash recovery.** Replay by message ID, republish-not-reapply for
  applied-unpublished, and "no success before durable outcome" close the
  classic gaps. Trace 5 tests the right seam. One unspecified corner:
  journalled-but-not-applied at crash replays forward — correct inside one
  sequencer, but under mechanism 2 the recovery order must be the journal
  order, not arrival order; worth one sentence in D7.
- **Fencing.** Correctly identified as the load-bearing risk of mechanism 2:
  a memory-only or non-durable lease reproduces exactly the 2026-08-08
  out-of-band-restore class of failure (two writers each believing they are
  head) — this time self-inflicted. The explicit non-conformance of mutex /
  GET-then-POST / client promise is well-grounded.
- **Journal/subscription semantics.** At-least-once with event-ID dedup,
  snapshot-cursor-then-events, `resync-required` instead of guessed replay,
  and "subscriptions are not correctness prerequisites" (a disconnected
  client still gets stale refusal) — coherent, and the last point correctly
  keeps subscriptions out of the correctness argument. Retention duration is
  unassigned (O-D7b).
- **Status distinctions.** `stale-input` (successful read-side result) vs
  `stale` (refused mutation precondition) vs `conflict` (invariant collision)
  are defined non-aliasingly and tripwired (trace 7). O-D3b closed.
- **CRDT re-arm.** Preserved verbatim in substance from P5 §6 and upgraded to
  the WR-26 standard: the re-arm condition is carried at the contract, and
  reopening additionally requires recording incident, artifact class and
  required convergence property. No objection.

### 11.2 Does the client-hosted-write ruling improperly pre-select?

**No — with one boundary to keep honest.** The ruling is derived, not
preferred: the candidate invariant makes each (question, revision) a
single-locus record, and D3-DR2 shows check-then-unguarded-write has a race no
client equivalence can close; any architecture admitting client-side writes
would violate the mission invariant regardless of what D7 prefers. So marking
client-hosted *writes* non-conforming is invariant enforcement, not
architecture selection. Two caveats:

1. A3's distinctive claim was client-hosted *evaluation*, not writes. D3
   leaves the read-only hybrid alive ("architecture 3 may remain read-only
   only"), so A3 is constrained, not eliminated — no improper pre-selection.
   But pass-2 §10.3 sharpened the real remaining question into O-D3c: D2's
   evidence events from a client-hosted evaluator are client-attested, so
   *promotion* assertions would rest on client-claimed evidence. D7, not D3,
   must decide whether conforming evidence requires authority-side
   evaluation. If it does, A3 narrows to display-only projection — which is
   SC5's thin-client claim anyway.
2. Process note: the enforcement table's "verdict" column scores candidates
   *for architectures* ("candidate for architecture 1/2"). The scoring is
   correct, but per the loop contract D7 owns selection; D7's matrix should
   cite this table rather than inherit its verdicts as decided.

### 11.3 Effect on the alternatives (no selection)

A1 gains the most from D3 (its conforming mechanism is the simplest and
single-truth); A2 remains viable but now carries an explicit proof debt
(lease/fencing); A3 survives only as the read-only hybrid, with O-D3c as its
remaining test. Net: the design has converged on "writes are authority-side
under any architecture" — which is the candidate invariant stated as a
conformance rule, and is the strongest single-locus statement in the design
so far.

### 11.4 Checkpoint receipt summary

Changed obligations: **O-D3a narrowed (mechanism specified; fencing/atomicity
proof owed); O-D3b closed; O-D3c, O-D5b, O-D7b added.** Highest-value
challenge: the lease/fencing proof for mechanism 2 is the only unsolved
failure mode left in the design — an unsound lease silently reintroduces the
out-of-band-restore failure class as an internal split-brain, so D7 must
treat the fencing proof as a selection gate, not a deployment detail.

## 12. Pass 4 delta — D4 checkpoint (`78a0e16`)

Re-tested: SC3, C3, C6, O-D4a/O-D4b, and the D1 manifestation/correspondence
shapes. Challenged: negative-vs-positive fixture distinction, coherent-build
repair, stable manifestation identity, directional/partial/ambiguous
semantics, premature "repaired fixture" claims, and D2/D3 compatibility.

### 12.1 Requested challenge areas

- **Negative vs positive fixture.** Sound and arguably the best move in the
  checkpoint: D4-DR1 keeps the contradiction-bearing input as a required
  *detection* fixture instead of quietly fixing the hashes — the defect P4
  found becomes a permanent tripwire (`fixture/revision-mismatch`, not
  warning-only) rather than erased history. The distinction is honest: the
  original is normative for *rejection*, the repaired manifest for *expected
  answers*.
- **Coherent-build repair.** The procedure is strong: hash from committed
  bytes, reject the map, rebuild from a clean checkout *or* recover a
  producer log, refuse mixed output (one build ID, one source revision set),
  never overwrite the input. The fallback — "if the committed HTML cannot be
  proven to derive from one coherent source set … a clean rebuild becomes the
  normative positive fixture" — is epistemically correct: it accepts that the
  normative fixture will be a *new* build, not the committed page, rather
  than laundering provenance. Deterministic-regeneration tripwire covers
  toolchain drift.
- **Stable manifestation identity.** Manifestation IDs minted by the author,
  DOM IDs demoted to locator evidence — exactly the D1 record-class rule, and
  the section tripwire (DOM ID changes across rebuild while manifestation ID
  stays stable) tests the right thing. Gap: nothing governs *minting* —
  uniqueness, namespace, who may mint, when an ID may be reused. At draft8
  scale this is fine; scaled to every manifestation in the corpus it is a
  single-locus hazard (new **O-D4c**).
- **Directional/partial/ambiguous semantics.** The method table's
  "proves / does not prove" columns are the right shape — each evidence kind
  carries its own limits, reversal is projection-only when declared
  reversible, partial anchors refuse to manufacture ends, ambiguous locators
  retain all candidates and queries never select among them. Consistent with
  D2's "ordering never turns a tied set into uniqueness".
- **Premature repair claim? No.** D4 states plainly that fixture repair is
  *specified, not executed*, and that INSTANTIATE creates and commits the
  repaired manifest while DERIVE does not rewrite futon5 artifacts. So C6 is
  repaired in design and still conditional in fact — the ledger keeps O-D4a
  open at execution level. The report resists claiming the repaired fixture
  before it exists.
- **D2 compatibility.** Assertion endpoints use the same location vocabulary
  as D2 candidates (`:artifact/revision-id`, codepoint, half-open range);
  evidence items are typed digests like D2 evidence events. Coherent. One
  axis to keep distinct: assertion `verification` states
  (`verified/partial/ambiguous/unverified/stale`) describe *evidence about a
  claim*, while D2 result statuses describe *an evaluation outcome* — same
  words, different objects. A vocabulary note (not an aliasing bug) should
  ride with D3's error algebra when D5 writes client-facing surfaces.
- **D3 compatibility.** Endpoints are revision-pinned and mutation contract
  untouched; `correspondence/query` is a read operation under the D3
  envelope. D4's `stale` assertion state (a queryable fact about evidence)
  is properly different from D3's `stale` error (a refused operation) — same
  non-aliasing discipline O-D3b enforced; no new conflict found.

### 12.2 Effect on D1 shapes and the alternatives (no selection)

The D1 `:arxana/corresponds` 4+ relation and the D4 assertion record are
mutually consistent (endpoints + revisions + evidence endpoint); D4 supplies
the instance semantics D1 left as shape. D4 is architecture-neutral: both A1
and A2 can host a fixture builder and assertion authority; it adds one more
D7 assignment (O-D7c) rather than tilting the comparison.

### 12.3 Sequencing exposure (operational, not a design defect)

The repaired positive fixture is now on the critical path of the first
demonstration: D5's client transcripts and D6's end-to-end example (O-D6a)
presume a correspondence fixture that will not exist until INSTANTIATE runs
the repair procedure. Until then, correspondence is testable only against
the negative fixture. D7's implementation increments must order the repaired
manifest *before* any client or compiler demonstration that consumes it —
recorded here so it cannot be silently reordered.

### 12.4 Checkpoint receipt summary

Changed obligations: **O-D4a narrowed (design-level closure; INSTANTIATE
execution owed); O-D4b narrowed (design-level closure; minting rule owed);
O-D4c, O-D7c added.** Highest-value challenge: manifestation-ID minting
governance (O-D4c) — "minted by the author" with no uniqueness or namespace
rule is a silent single-locus violation waiting at corpus scale; the rule
should be specified before INSTANTIATE, not discovered as duplicate IDs
after. Secondary: the §12.3 sequencing exposure — the repaired fixture is on
the critical path of every later demonstration.



