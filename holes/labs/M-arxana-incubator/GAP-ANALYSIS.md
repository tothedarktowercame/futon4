# M-arxana-incubator — gap analysis

## Pass 1 (organisation and simulation, codex-13, 2026-11-17)

This pass compares requirements with the supplied MAP evidence, not with a newly
verified deployment. No implementation code, live store or demo was exercised.
“Per survey A/B” below means a reported code finding; the accompanying code
locations are those surveys' citations, not fresh verification. Interview claims
remain attributed to their speaker. Status means **met** for the stated slice,
**partly** for an existing mechanism with a named missing part, and **missing** for
an absent end-to-end capability; it does not grant mission acceptance. Automation
and proof requirements remain unassessed pending the interview (M §2 Interviews).

The organising question is Joe's “above-all” traceability requirement, not whether
all facets can be rendered as prose. The tetrahedron assigns simulation to verbs,
automation to nouns/holes, organisation to interactive agents, and Arxana to
evidence; it does not establish that their evidence has one payload type
(M HEAD, Carried-forward tensions; S §5). The central gap remains joining the
existing parts (M HEAD, Operator-voice anchor; per survey A Q4).

### Source key and citation convention

Section references in each row resolve to these files. Code paths cited from A
are under `futon3c/src/futon3c/arxana/` unless explicitly qualified.

| Key | Source |
|---|---|
| M | [Mission](../../missions/M-arxana-incubator.md), HEAD, §1 IDENTIFY, §2 MAP (including per-WP table, Interviews and Open facts, resolved); read at futon4 `4c0f62a` |
| A | `/home/joe/notes/claude-5/map-arxana-incubator-A.md`, Q2/Q3/Q4/Q7 and Surprises; survey basis futon3c `45ebc6894`, futon1b `e4aa6e1` |
| B | `/home/joe/notes/claude-5/map-arxana-incubator-B.md`, Q1/Q5/Q6/Q8 and Surprises |
| O | `/home/joe/notes/claude-13/arxana-incubator-interview-2026-11-17.md`, organisation, §§1–5 |
| S | `/home/joe/notes/claude-1/arxana-incubator-interview-2026-11-17.md`, simulation, §§1–5 |
| W | [WORKPACKAGES.md](../E-arxana-variant-documents/WORKPACKAGES.md), WP1–WP12, Hub and Order |
| D | [DERIVE-1-nelson-requirements.md](../E-arxana-variant-documents/DERIVE-1-nelson-requirements.md), Above all, N1–N20, tests T0–T6 |

M's resolved facts supersede earlier survey counts/limit uncertainty: **23,146
distinct evidence ids**, not 23,158 turns or B's 23,147; 23,160 dump rows include
repeats and two rows without ids. WP1 uses **150 words per free-text field** by
Joe's ruling, rather than leaving the limit blocked on form inspection
(M §2 Open facts, resolved; contrast B Q1/Q5). This is a reported resolution,
not a new census or form measurement.

### 1. Requirements register

Stable IDs allow pass 2 to append requirements and revise assessments explicitly
without replacing this baseline. “Cross-facet” includes Joe's evidence-facet
requirements; it does not assert an automation interview result.

| ID | Requirement and source | Facet | Types needed | Status against MAP / missing part |
|---|---|---|---|---|
| J01 | Every character after editing answers who, when and from where (M HEAD Anti-glibness; D T0). | Cross-facet | permanent content, source span, attributed edit, projection version | **partly** — per survey A Q2/Q4, patches, locators and manifests exist separately; typed additions and edits are not one primary content-list history. |
| J02 | Editing one's own utterance changes correspondence, not authorship; preserve origin while adding new words (M HEAD traceability; D N1/N6/N16, T0). | Cross-facet | content identity, provenance act, correspondence assessment | **partly** — per survey A Q2/Q7, evidence/relocation can preserve claims, but edits over immutable landscape spans are missing. |
| J03 | Work on one source pool and obtain Incubator and Fellowship projections (M HEAD Operator-voice anchor). | Cross-facet | projection selection/order, content-list version | **missing** as a joined record — per survey A Q4, F1 ordering, F6 placement and F2 edits are separate. |
| J04 | One scoped edit leaves the other projection unchanged; static pins versus live derivation remain explicit (M §1 Completion criteria; HEAD tensions; W WP4/5). | Cross-facet | scoped edit/update, projection scope, pinned/live selector | **partly** — per survey A Q3/Q4, one F2 CAS head and stored alternatives do not provide independently scoped projections. |
| J05 | Hand insert, rearrange and delete are append-only content-list operations (M §1 Completion criteria; D T2). | Cross-facet | permanent typed content, edit/selection/reorder acts | **partly** — per survey A Q2, F2 codepoint hunks modify artifact revisions, not a primary list of source spans. |
| J06 | Joe/Murch/Rob attribution survives cut-and-paste; link authors remain identifiable (M HEAD; D T6). | Cross-facet | contributor identity, typed-content act, placement, link attestation | **partly** — per survey A Q2/Q3, attributed patches exist; cross-author content/placement provenance is not joined. The built editor is explicitly out of scope in M §1. |
| J07 | Comment links survive insert and partial delete, attaching only to surviving characters (M §1 Completion criteria; D T3). | Cross-facet | span-endpoint link, relocation/continuation evidence | **partly** — per survey A Q7, relocation is F1-specific and lacks the partial-delete survival contract. |
| J08 | Two projections show shared content by identity, staying connected through scrolling (D N7/N9/N13, T1; W WP11). | Cross-facet | two projection versions, shared span identity | **partly** — per survey B Q8, views consume one reconstructed JSON; D grants cursor-over partial credit, not two record-backed projections. |
| J09 | Show both used and not-yet-used material while writing the LOI (M §1 Completion criteria; D T4). | Cross-facet | projection selection, used-span coverage/complement | **partly** — D N15 reports shaded used regions, but no across-landscape unused accounting; per survey A Q4 no joined projection record. |
| J10 | Open the original context of a transcluded word (D T5). | Cross-facet | source span and pinned source context | **met for the current demo**, explicitly per D N8/T5 and B Q8; retain as regression, not proof of native capture. |
| J11 | New writing/clips are born with pointers, not reconstructed later by lexical matching (D Above all, Joe's corrections; N2/N6/N7). | Cross-facet | native capture, nip acceptance, permanent content | **missing** end to end — per survey B Q6/Q8, anchors are matcher output and views read derived JSON. |
| J12 | Every free-text incubator field obeys the ruled 150-word limit and displays failure when exceeded (M §2 Open facts; W WP1). | Application | requirement and versioned satisfaction | **partly** — per survey B Q5, Fellowship markup/checker exists; incubator requirement records and views do not. Limit uncertainty is resolved, not a remaining gap. |
| J13 | Submit by 2026-11-20; tooling must not block submission; each field traces to sources or is marked new (M HEAD/§1 Completion criteria; W WP2 fallback). | Application | field projection, export receipt, new-content provenance | **partly** — per survey B Q8, source views exist; export equality and native field provenance remain to demonstrate. Deadline is a constraint, not a record capability. |
| J14 | LOI's 15 fields form a second projection over the same material (M §1 Completion criteria; W WP12). | Application | field requirements, arrangements/projections | **partly** — per survey B Q5, 15 fields/limits exist as markup, not shared record-backed projections. |
| J15 | Distinguish act time from recording time and query historical state (M §1 Theoretical anchoring; W WP3/4). | Cross-facet | dated act, valid-time read, projection snapshot | **partly** — per survey A tables/Surprises, record valid-from and record-by-id as-of exist; constructors omit event-time, service entity/get and projection/head temporal reads remain absent. Deployment is separately gated by W WP3. |
| J16 | Map types to work packages; serve other efforts by liaison without first reassigning their seats (M HEAD; W opening/Hub). | Coordination | type/WP dependency register, scoped acceptance | **partly** — M's table exists; O/S add requirements not represented there. Staffing is explicitly undecided (W opening), not an inferred build dependency. |
| O01 | Point at turns, animal records, inventory/needs, runs, certificates, clock decisions and introductions; project turn → readings → priorities (O §1). | Organisation | typed external references, run/reading identity, projection | **partly** — per survey A Q2/Q4/Q7, generic endpoint/reference substrate exists; O lists disparate stores, and M WP2/4 contains no joined chain projection. |
| O02 | Query who declared supersession, when, why, and its scope (O §2, §4). | Organisation | `[:supersedes 2]`, attribution, scope | **partly** — per survey A Q3, v1 has no content fields; O must consult census attestation and git. |
| O03 | Identify which reading backs an edge natively by member + line sha, not repeated run attestations (O §2). | Organisation | reading identity, evidence-support link | **partly** — O reports CL-48's pin fix; per survey A Q7 the clusion/evidence substrate exists, but M table has no native reading type. |
| O04 | Correct/retract an animal reading with replacement and attribution, reaching Joe (O §2/3). | Organisation | reading correction/retraction, supersession | **missing** as a demonstrated workflow — O says obligation 2.3 was not demonstrated; per survey A Q3 no general attributed supersession honoured by projections. |
| O05 | List/query typed absences including interrupted reply, not-ranked, needs-unavailable and retained focus (O §2/3). | Organisation | typed absence with subject and producer | **partly** — O reports fields already present; no standalone listable absence record in M's per-WP table. Per survey A tables, attempts/evidence are adjacent mechanisms. |
| O06 | Static top-items/why-not views pin a run; live stepper follows one REPL's newest record (O §3). | Organisation | pinned/live projection, run/version pin | **partly** — O reports those consumers; per survey A Q4 lacks a common projection record and scope semantics. |
| O07 | Compute per-turn service levels: saved 2 min, animals 15 min, graph 30 min, priorities 6 h (O §3). | Organisation | timed requirement, satisfaction computation and evidence | **partly** — per survey B Q5, computed GCC met and warrants exist, but WP6's projection-text requirement excludes these timed predicates. |
| O08 | Preserve recall query and spans, with Joe's action as acceptance (O §3). | Organisation | nip query/result/acceptance | **partly** — per survey B Q6, spans exist but saved recall drops query; O §5 has not rechecked that finding. |
| O09 | Express finished work “continues in” new work without claiming supersession (O §3). | Organisation | typed continuation relation | **missing** in M table; per survey A Q3, successor/predecessor supersession is adjacent but asserts different semantics. |
| O10 | A correction updates Mojira who/when/why and regenerates a checking Lean chain certificate citing the replacement (O §4). | Organisation | correction, effective-reading projection, certificate derivation | **missing** end-to-end acceptance; O's proposed next-month test is stronger than W WP4's non-leakage test. |
| S01 | Name all evidence classes: registrations, scripts, dumps, results, design items, causal-model revisions, receipts, rulings and reviews (S §1). | Simulation | external artifact/reference, registration/run/result/receipt identity | **partly** — per survey A Q2 endpoint kinds can reference artifacts; S §5 says exotype files are not in surveyed stores, and M excludes run records. |
| S02 | Trace a claim/arrow to the exact result-file version and span, avoiding repeated grep (S §2). | Simulation | evidence link, result span, accepted nip | **partly** — per survey A Q2 span locators exist; per survey B Q6 no durable query+accepted-span nip. S reports prose evidence strings. |
| S03 | Prove predictions and scripts preceded observations/runs, including late-script exceptions (S §1/2/5). | Simulation | registration, prediction, attested before relation, exception | **missing** in M table; per survey A Q3 event times are adjacent, not ordering evidence. S reports commit order and a prose exception only. |
| S04 | Trace relayed rulings and holds back to Joe's turn (S §2). | Simulation | ruling-source link, turn reference | **partly** — per survey B Q1 evidence ids can join turns; S reports quotations without ids. Missing links cannot be invented from matching text. |
| S05 | Record who changed an arrow's status, on which review/evidence, including removed arrows and corrected claims (S §2/3). | Simulation | attributed supersession/assessment, review reference | **partly** — per survey A Q3 existing supersession lacks actor/time/reason; S reports a long status string and unlinked review acceptance. |
| S06 | Represent cancelled/unfinished runs and their partial dumps without treating them as scored completions (S §2). | Simulation | run lifecycle outcome, partial-output reference, typed absence | **missing** in M table; S reports partial dumps/prose only. Per survey A tables attempts are a possible substrate, not a run lifecycle. |
| S07 | Nip = query/reader script + accepted result span at a pinned sha (S §3). | Simulation | nip, script identity, result span, acceptance | **partly** — per survey B Q6 query/span/acceptance are not joined; W WP7 restricts results to turn spans. |
| S08 | Registration criteria and re-arm conditions are script-decided predicates, thresholds fixed before the run (S §3). | Simulation | predicate requirement, evaluation receipt, before relation | **partly** — per survey B Q5 warrants pin verdicts to shas; M WP1/6 text-question/limit shape does not express this contract. |
| S09 | Static pause-point write-ups and live causal-model rendering both retain their inputs (S §3). | Simulation | pinned/live projection, computed-from link | **partly** — S reports render.py and write-ups; per survey A Q4 no common projection record, and M table assumes turn-text selections. |
| S10 | Immutable run identity covers code sha, parameters, seeds, dump hashes and start/end without embedding large dumps (S §3). | Simulation | run record, external blob manifest | **missing** in M table and current scope; per survey A Q2 artifact/external-ref kinds are reusable pieces, not the run contract. |
| S11 | Score an earlier prediction as held/failed against a named observation (S §3). | Simulation | prediction, before link, scoring assessment | **missing** in M table; per survey A Q3 attribution/assessment mechanisms are only adjacent. |
| S12 | State output was computed by this script from these exact inputs, not quoted or edited (S §3). | Simulation | computed-from/derivation link, execution receipt | **missing** as a general relation in M table; per survey A Q3 text/application links a patch application only. |
| S13 | About 30 write-up figures are accepted nips; wrong values flag and changed result files stale every dependent figure (S §4). | Simulation | nip, value check, dependency invalidation | **missing** end-to-end test — per survey B Q6 partial anchors and per survey A Q7 relocation do not verify a reported numeric figure against its basis. |

No automation/proof row is marked met, partly or missing from conjecture. The
facet's HEAD description supplies context only; M §2 Interviews explicitly marks
claude-7 pending. Pass 2 should identify shared requirements by these IDs and add
new ones without silently overwriting O/S evidence.

### 2. Type gaps relative to the per-WP MAP table

These are proposed contracts, not declarations that a particular new constructor
name is approved. “Absent from table” does not imply “absent everywhere in the
stack”: closest mechanisms below are explicitly **per survey A/B**. Entries
already named by the table but underspecified for a facet are marked **widen**.

| Gap | Needed contract and requirement IDs | Closest existing type / cited survey evidence | WP consequence |
|---|---|---|---|
| G01 run and output manifest | Code revision, parameters/seeds, start/end, hashed external dumps; completed/cancelled/partial outcomes (S01/S06/S10). | Per survey A Q2, artifact-revision/external-ref endpoints (`type_registry.clj:9–10`) and per A tables `arxana_attempts` (`futon1b_arxana.clj:604–609`). Neither specifies a simulation run. | New optional run-evidence package; scope decision required because M §1 excludes it. WP4/7 must at least avoid assuming all sources are turn text. |
| G02 prediction / registration | Claim, fixed criterion and intended observation, later held/failed score (S03/S08/S11). | Per survey B Q5, WM criteria/wants (`futon2/aif/mission_criteria.clj:84–145`, `outcome_wants.clj:7–45`) and warrants (`validation.clj:173–193`). | New registration/scoring package or explicit WP6 extension, conditional on simulation scope. |
| G03 “before” with evidence | Attested precedence, named events and evidence such as commit ancestry; late-observation exception (S03). | Per survey A Q3, application `:event-time` is server submission time (`formal_f2.clj:51–60`), and choice `:chosen-at` (`:221–265`). A timestamp alone is not the requested attestation. | New ordering-evidence package; WP3 must not claim to satisfy it. Precedes prediction-scoring acceptance. |
| G04 computed-from | Typed output → script revision + exact inputs, with execution/receipt pin (S09/S12/O10). | Per survey A Q3, `[:text/application 1]` from/to/change/patch (`formal_f2.clj:51–60`); per B Q5 warrants pin status to hashes (`validation.clj:173–193`). Neither is a general computation relation. | New derivation-evidence package; WP4 projections and WP6 evaluations consume it. |
| G05 native reading identity/support | Reading member + line digest, linked to the claims it supports (O03). | Per survey A Q7, clusions and role columns (`futon1b_arxana.clj:604,256–270`); per O §2, CL-48 pins supply the identity but not a native type. | Extend WP4 or add organisation evidence package before correction acceptance. |
| G06 reading correction/retraction | Old reading, replacement or explicit withdrawal, actor/time/reason/scope; effective-reading selection (O04/O10). | Per survey A Q3, `[:supersedes 1]` (`type_registry.clj:163–166`) and attributed F6 re-support (`formal_f6.clj:150–181`), whose reader does not follow the chain (`:123–127`). | WP4 correction subtype/contract plus reader and certificate acceptance; retraction is not simply another positive reading. |
| G07 standalone typed absence | Subject, missing stage/outcome, producer, time, reason and evidence; listable without fabricating success (O05/S06). | Per survey A tables attempts/evidence are stored; per A Q7 relocation can report missing (`formal_f1_relocation.clj:122–155`); per O §2 absences are fields. | New small absence package shared by organisation and optional run lifecycle; WP4/6 views must expose it. |
| G08 continues-in | Finished work leads into later work without invalidating it (O09). | Per survey A Q3, acyclic supersedes (`type_registry.clj:163–166`) and composite predecessor (`formal_f4.clj:228–237,254–256`). Neither establishes continuation semantics. | Separate link kind in WP10 or a small continuation package; do not overload supersedes. |
| G09 attributed supersession, **widen** | `[:supersedes 2]` actor/time/reason/scope plus consumer rules (O02/S05). | Per survey A Q3, v1 has no fields; F6 re-support has actor/changed-at/reason (`formal_f6.clj:150–181`) but newest-support reading ignores chain (`:123–127`). | WP4 already names it; extend acceptance to actual organisation/simulation consumers rather than just adding fields. |
| G10 general nip, **widen** | Query strategy or reader script, accepted spans of a versioned source, actor/time; not turn-only (O08/S02/S07/S13). | Per survey B Q6, dog spans (`dog_provider.clj:907–1010`, `arxana/dog_memory.clj:204–259`), query dropped (`dog_provider.clj:1381`), search ephemeral ranges (`futon1b_text.clj:1402–1428`). | WP7/8 accept result-file sources and retain query; add simulation wrong-figure/staleness test. |
| G11 requirement/evaluation, **widen** | Text constraints, numerical predicates, timed service obligations; evaluation pins exact subject/basis and decision method (J12/O07/S08). | Per survey B Q5, LOI parser (`mojira/loi.js:15–29`), computed waiting_on (`gcc-view.js:107–115`), warrants (`validation.clj:173–193`). | WP6 needs typed criterion/evaluator variants rather than universal word-limit fields; keep WP1 the bounded application slice. |
| G12 projection and authored-content acts, **widen** | Primary ordered span list, independent scope, static/live policy, native new-text identity and authentic act times (J01–J06/O06/S09). | Per survey A Q4, F1 manifest (`formal_f1_export.clj:26–112`), F6 placements (`formal_f6.clj:107–138`), F2 single head; per A Q2/Q3, patch hunks and application time (`text_rebase.clj:78–97`, `formal_f2.clj:51–60`). | WP4/5/9 must join these, not merely export another JSON. WP11 needs a deliberate renderer contract (per B Q8 closed op set). |
| G13 general span link / origin of ruling or review, **widen** | Source/result spans and external review references retain source identity; bidirectional lookup and surviving-span semantics (J07/S02/S04/S05). | Per survey A Q2, D4 locators are in-memory (`correspond.clj:30–35,80–146,198–241`); per A Q7 existing clusions store typed links but relocation is F1-only (`formal_f1_relocation.clj:176–183`). | WP10 generalises endpoints and relocation; explicit review/turn reference acceptance must not infer provenance from matching quotations. |

The table cannot establish whether DiagramProver causal-model JSON already has a
suitable registered type: S §5 expressly does not know, and neither survey maps it.
Likewise script identity, design-item locators and Agency review references may use
existing artifact/external references, but their conformance contracts are not in
MAP (S §1; per survey A Q2). These are verification tasks, not evidence for inventing
a distinct record type for every file extension.

### 3. Conflicts and unresolved boundaries

This section states both positions. It does not choose one facet's semantics for
another. Two apparent contradictions are already narrowed by M's Interviews and
resolved-facts sections; their remaining evidence gaps are kept explicit.

| ID | Side one | Side two | What must remain explicit |
|---|---|---|---|
| C01 requirements | W WP1/6 and M Q5 specify question, word limit and projection-text satisfaction. | S §3/5 needs a numeric predicate decided by a script and fixed before a run; O §3 needs per-turn timed service levels. | The common name does not make these interchangeable. Criterion/evaluator types and what counts as evidence are undecided (G11). |
| C02 landscape | M HEAD/§1 and W model use immutable turns/text and removable edits; W WP7 defines turn-id/codepoint-span nips. | S §3/5 needs hash-identified runs with large external dumps, plus text results. | Text-span addressing fits result files, not necessarily ring-state dumps. Run payload/locator contracts remain outside the current plan (G01). |
| C03 scope | M §1 explicitly excludes MetaCA run records, DiagramProver rewrite rules and a built multi-author editor; W Hub says no package depends on simulation. | S asks for run, prediction and derivation records; Joe's traceability aspiration includes Murch/Rob, and D T6 tests multiple authors. | Register the demands without declaring the excluded builds authorised. Record design and tests can expose these needs while scope remains for Joe (J06, S10–S12). |
| C04 identity wording | Per survey B Q1, dump `turn_id` is a nullable/repeated session counter; `evidence_id` joins stores. | O §5 says the 象 record key `turn-XXXXXX` is stable and already used in pins/certificates. | M Interviews recognises they are different fields. Do not replace valid turn-record identity with a dump counter, or erase evidence-id join failures; namespace the identities (O01/J11). |
| C05 time wording | Per survey B Q1, evidence `:at` is ingestion time and no speech-time valid-time publication exists. Per survey A Surprises, application event-time is submission clock. | O §5 says 象 `created_at` is when the turn was made and drives Hou; M Interviews qualifies it as close to speech time since 2026-09-28. | A timestamp's existence, its accuracy as speech time, and its publication as valid time are three separate claims. Older-turn event-time authority and service-level semantics are unresolved; never backfill ingestion time as asserted speech time (J15/O07). |
| C06 supersession consumers | Per survey A Q3 and M table, no general attributed supersession honoured by projections; F6 newest support ignores its re-support chain. | O §5 reports CL-45 clocks and Hou already act on mission supersession via a file-pinned census attestation. | These are different consumers, not proof that either report is false. Inventory/clock support does not establish F6 correctness or animal-reading correction; audit each intended reader (O02/O04/O10). |
| C07 recall completeness | Per survey B Q6, query is computed then dropped on segment save. | O §5 reports late recalls retained as `:delivered false` claims and explicitly has not rechecked query retention. | Delivery retention and query provenance are distinct. Do not mark the nip complete on the strength of late-claim retention; verify saved query and acceptance independently (O08). |
| C08 ordering | W WP3 enables valid-time/as-of records; M table calls it temporal support. | S §5 requires a prediction attested to precede an observation; S §2 reports only commit order and a post-observation script exception. | A date field is not the requested evidence of epistemic ordering. Define what witnesses “before”, including negative/exception cases, before scoring predictions (G03). |
| C09 current projection | M/W require independent, scoped, pinned or live projections without a privileged head. | Per survey A Q4, F2 has one CAS head, F6 always selects latest placement and keys its history per item rather than argument+item. | Keeping F2 regressions green does not prove independent projections; specify where sequencing remains and how placement scope is distinguished (J04/G12). |
| C10 T1 acceptance | D's T1 literally calls for beams that recompute on scroll. | D Above all records Joe's correction that cursor-over highlighting earns partial credit and beams are optional; W WP11 also calls beams optional. | Propose an explicit amendment to T1 rather than claiming a no-beam implementation passes its literal wording (J08). |
| C11 dates and urgency | W WP1/2 dated path explicitly must not wait for tooling; fallback date is 2026-11-19. | W WP2's traceability criterion and D T0 require accountable new writing and provenance, while current views are reconstructed/read-only (per B Q8). | Submission fallback is not permission to assert native provenance or completed traceability; report fallback limitations without blocking submission (J11/J13). |

Not open conflicts: field limit and dump-count discrepancies have explicit later
rulings/count corrections in M §2 Open facts, resolved (above). No disagreement
between the two interviews about supersession or nips was found: they share those
needs but give different consumers and tests (O §4; S §4). That agreement is not
an implementation result.

### 4. Proposed changes to WORKPACKAGES.md (not applied)

The proposals below preserve the dated WP1 → WP2 path. “New package” here means a
candidate for review, not added mission scope or assigned staffing (M HEAD and §1
Scope; W opening/Order).

1. **WP1: resolve the recorded limit and retain the criterion's authority.** Replace
   “find limits” uncertainty with Joe's 150-word free-text ruling; retain form-question
   identity and distinguish choice fields from free text. Add boundary tests at 150
   and 151 words and show which criterion/version was checked. Do not turn WP1 into
   the general simulation evaluator. Sources: M §2 Open facts; per survey B Q5;
   W WP1; J12/C01.

2. **Add an identity/time-source preflight before native WP7/WP9 capture.** Specify
   dump evidence id versus 象 record id, source digest, source time authority and
   an explicit absence when a join/time is unknown. Test two distinct sessions with
   equal dump counters, a missing evidence id, and pre-September turns without 象.
   Never fabricate a join or speech time. This can run alongside WP1/2, not delay
   submission. Sources: per survey B Q1; O §5; M Interviews; J11/J15/C04/C05.

3. **Split WP3's deployment gate from temporal coverage.** Keep Joe's restart and
   past-event/as-of refusal test. Add a separate list of constructors that supply
   genuine act time and of readers that honour it: service entity/get, projections,
   heads/revisions are not supplied by record-by-id alone. Preserve unknown act
   times explicitly. Do not add “before” to WP3's completed claims. Sources:
   per survey A tables/Surprises (`formal_f2.clj:51–60`, `schema.clj:19–23`);
   W WP3; J15/C08.

4. **WP4: separate projection composition from attributed update semantics.** Name
   the primary selection/order record, scope identity, pinned versus live mode and
   effective-version rule. Add T0 provenance checks for every rendered character,
   and test placement of the same item in two arguments/projections without leakage.
   Keep the existing scoped-edit bad case. Sources: W WP4; per survey A Q4
   (`formal_f6.clj:114–116`); D T0; J01/J03/J04/G12.

5. **WP4 follow-on: organisation correction and attribution acceptance.** Add native
   reading member+line-sha support, `[:supersedes 2]` semantics and a correction/
   retraction case. Require old evidence to remain queryable; show replacement with
   who/when/why in Mojira and regenerate the chain certificate referencing it.
   A header/type declaration alone fails this package. Sequence after reading
   identity and attributed update contracts, before claiming organisation completion.
   Sources: O §§2–4; per survey A Q3; G05/G06/G09; O10.

6. **New small typed-absence/continuation packages, independently reviewable.** One
   exposes listable absences while preserving their original subject and reason;
   test interrupted/retained versus ordinary success. The other expresses “continues
   in” and tests that it does not suppress the predecessor as superseded. These can
   use the existing clusion substrate without requiring the whole editor. Sources:
   O §2/3; per survey A Q3/Q7; G07/G08. Their exact schemas remain a decision.

7. **WP5: retain F2 guarantees but test intentional divergence.** Test two projections
   sharing original content, independently edited, with neither head/placement
   changing the other. Stored conflict alternatives or successful rebase into one
   head do not count. Depends on WP4 composition/scope, not on new simulation runs.
   Sources: per survey A Q2/Q4; W WP5; J04/C09.

8. **WP6: make requirement satisfaction a versioned, evidence-backed relation.**
   Separate text-limit, numeric-predicate and timed-service criterion variants;
   evaluator identity and input hashes belong to the result. Add three bad cases:
   text outside the subject projection, threshold altered after observation, and a
   service-level verdict computed from unrelated timestamps. Reuse warrant/WM
   semantics only where they match, without converting booleans into proof.
   Sources: O §3, S §3/5; per survey B Q5; W WP6; G11/C01. Numeric evaluation
   implementation is a scope extension to approve, not a prerequisite for WP1.

9. **WP7/8: broaden source identity without losing the exact-span rule.** Preserve
   query/strategy/terms/dials or reader-script revision, accepted result span, source
   digest, acceptor and acceptance time. Test both turn and results-file nips;
   a proposed candidate is not accepted merely because its substring matches.
   Include saved-recall query retention as its own regression. For simulation's
   approximately 30 figures add wrong-value and changed-file dependency-staleness
   tests; replaying the right characters alone cannot verify a misreported figure.
   WP8 depends on the appropriate requirement variant and WP7, while WP7's record
   contract can proceed in parallel with WP4. Sources: W WP7/8; per survey B Q6;
   O §3/5, S §3/4; S13/G10.

10. **WP9/10: make T0/T2/T3 and T6 attribution explicit in native edit acceptance.**
    Insert must create permanent content; rearrange/delete change references rather
    than bytes; a partial deletion must leave only surviving link targets and retain
    access to the earlier version. Test arbitrary artifact links, not only the F1
    demo artifact. Add a record-level second-author paste test without silently
    bringing the excluded multi-author editor into scope. Sources: D T0/T2/T3/T6;
    W WP9/10; per survey A Q7; J05/J06/J07/C03.

11. **WP11/12: specify renderer input and preserve T5 while extending T1/T4.** Add
    a record-to-view contract with explicit unsupported kinds instead of assuming
    new record types fit the existing five ops. Test two projections by source
    address, not lexical similarity; ensure cursor-over remains correct after scroll.
    Amend T1 explicitly if beams remain optional. Keep quote-to-context T5 and add
    the “already fully used is not unused” T4 bad case. Test export bytes against
    rendered field bytes for WP2/12. Sources: per survey B Q8; D T1/T4/T5 and
    Joe's beam correction; W WP2/11/12; J08–J10/J13/C10.

12. **Propose, but do not yet schedule, a simulation-evidence track.** Candidate
    packages: (a) immutable run/partial-output manifest, (b) attested prediction-before-
    observation and scoring, (c) computed-from links/receipts over exact script/input
    revisions. First map existing external-reference and DiagramProver contracts;
    then test a changed dump hash, a post-observation prediction, and an output whose
    claimed input/script differs from the receipt. Dependency proposal: identity/
    digest contract → run/derivation contracts → before/scoring evaluation; text-result
    nips can proceed earlier. Sources: S §§1–5; per survey A Q2/Q3; G01–G04. M §1
    currently excludes run records, so these are a scope decision for Joe, not new
    blockers on the incubator or Fellowship writing paths.

Proposed sequencing therefore keeps **WP1 → WP2** independent; runs the source
identity audit, WP3 deployment, WP4 contract work and WP7 contract work alongside;
places WP5 and native WP9 after projection scope/content identity; and demands
consumer tests before claiming corrections, links or requirement satisfaction.
This is an analysis proposal grounded in W Order, O §4, S §4 and D T0–T6, not an
amendment of those sources. No work-package file was edited in this pass.

## Pass 2 (automation and proof, claude-7)

**Pending.** Append the interview source/version, new `Axx` requirement rows,
assessments against the same MAP baseline, additions to G/C lists, and proposed
WP deltas. Cite which pass-1 IDs are shared or disputed; retain the earlier rows
and explicitly record any revised status. In particular, do not assume that a
proof hole is a word-limit requirement or that a proof/checker receipt is a
simulation run until the interview supplies its contract (M HEAD tetrahedron;
M §2 Interviews: claude-7 pending).

### Pass-2 evidence and assessment boundary (codex-13, 2026-11-17)

The **Pending** paragraph above records the pass-1 handoff; this append fulfils it.
Pass 1 remains unchanged. This pass feeds VERIFY, not a reopening of MAP or DERIVE
(M §2 Interviews, claude-7 entry, lines 416–436; mission status ARGUE). The design
baseline read here is futon4 `7452df9bc197f58640093fdff5b03c0ec19709b8`.

Additional source keys:

| Key | Source and evidence level |
|---|---|
| I7 | `/home/joe/notes/claude-7/arxana-incubator-interview-2026-11-17.md`, §§1–5; interview testimony, not an independently exercised renderer |
| D1 | [DERIVE-1-types.md](DERIVE-1-types.md), especially §§1–3, decisions D3/D4/D6, §7 coverage; design text inspected in this pass |
| D2 | [DERIVE-2-registry-and-fidelity.md](DERIVE-2-registry-and-fidelity.md), §§2–5; proposed registry and fidelity contract inspected in this pass; §5 revisions take precedence over earlier forms |
| MX | `/home/joe/notes/claude-7/raw-names-480/census/matrix-r8.tsv`; header and a97J08 rows inspected directly in this pass |

Direct file verification: MX's header has module, declaration, word, kind, class,
packet, rule and reading. Rows 2358–2361 name q55 contradiction readings for
`apm_a97j08_boundary_sum_aux_disjoint`; row 3389 names q78's nested-case restriction;
row 3457 names q83's `paper_walk._merge_right_or_splits` and “In all n cases”.
These rows support the existence of a reading history, **not** the complete
sentence-to-rule provenance requested below. There are no actor, event-time,
renderer/corpus digest or supersession columns in that header. The exact Q66
ownership, Q83 merge, Main.lean:2628–2631 correspondence and Q56 regression remain
**per I7 §§2/4**, not freshly verified in code. No renderer, Lean build, live store
or demo was run; no claim below is a new implementation acceptance.

### 1. Additional requirements register

The status column uses pass 1's met / partly / missing against the MAP/interview
baseline. A proposed design mechanism is identified separately; “covered in design”
does not mean implemented. IDs A01–A16 are new requirements, not survey A citations.

| ID | Requirement and source | Facet | Types needed | Status against MAP / missing part |
|---|---|---|---|---|
| A01 | Address formal declarations and generated proofs at reproducible versions (I7 §1). | Automation/proof | E2 revisions; generated-output record; renderer/corpus pins | **Partly:** module/declaration identities and dumps exist; per I7 no corpus sha and renderer commit only in notes. D1 E2 supports immutable inputs, but does not supply those missing pins. |
| A02 | Answer which packet/rule generated a sentence and which merge changed it, without manual rerender/bisect (I7 §2). | Automation/proof | Rule, rule application, generated-by, packet/merge references | **Missing:** joins are log lines per I7 §1. D1 R4 computed-from is a relation kind, not a recorded per-sentence application. |
| A03 | Keep earlier wordings and attributed, dated, reasoned reading supersession (I7 §§2/4). | Automation/proof | Reading-rule revision; R3; output revision | **Partly:** packet prose/MX preserve fragments. D1 R3 and D2 §5.1 act identity cover attribution/history in shape; a rule record and wording-history consumer are absent from the proposed entries. Shares O02/O04, G06/G09. |
| A04 | Distinguish decided wording from incidental text in an executable test pin (I7 §2, m96A05 wrong alternative example). | Automation/proof | Decision/acceptance attached to a rule or output span; E5 evaluation | **Missing:** a passing pin alone does not establish an approved reading. D2 §5.3 independence helps attribute review, but does not classify what wording the review approved. |
| A05 | Static proof projection pinned by renderer sha, corpus sha and declaration list; live means rerender (I7 §3). | Automation/proof | E4 plus render recipe/input manifest | **Partly:** dumps and commits exist per I7 §1. D1 E4 / D2 §2 live queries select applied clusions; they do not define rerender execution or pin completeness. |
| A06 | Store a reading's Lean head/pattern, typed guard and phrase, with packet, reviewer and merge attribution (I7 §3). | Automation/proof | Rewrite-rule clusion; E2 source references; R3 | **Partly:** MX and packet prose hold readings; MX rule field is free text (I7 §1; header checked). No rewrite-rule entry in D2 §2. |
| A07 | Requirements can be executable rendered-text pins or census families reaching zero (I7 §§1/3). | Automation/proof | R5 predicate + E5, subject and census version pins | **Partly:** tests/censuses already exist per I7; not joined to Arxana. **Covered in design semantics** by D1 D4 and D2 §5.2; WP6 still has text-only acceptance. Shares S08/G11/C01. |
| A08 | Link formal span, generated sentence and informal exam statement (I7 §3). | Automation/proof | R4/R7, E2, structural generated locator | **Partly:** alignment and links are available per survey A Q4/Q7; D1 R4 permits multiple endpoints, but D2 §2 R4 has only from/to. No sentence structural locator contract. Shares G13. |
| A09 | Preserve census query and accepted defect/acceptable sets (I7 §§1/3). | Automation/proof | R6 query/results; acceptance disposition | **Partly:** family/round and records exist per I7. D1 R6 can reference E2 spans; D2 §5.8 says nip results are landscape spans and rejects memory without a turn, leaving non-turn census acceptance insufficiently explicit. Shares O08/S07/G10. |
| A10 | Correct generated text by changing its rules, never by hand-editing the generated output (I7 §§3/5). | Automation/proof | Rule revision; R3; generated-by | **Missing** as an Arxana workflow: D1 R2/D6 and WP9 target projection spans/E3 writing, not rules. This does not invalidate WP9 for authored prose. |
| A11 | Record an accepted residual: not clean, deliberately shipped with an attributed note (I7 §3). | Automation/proof | Exception/acceptance with subject/version/scope; E5 | **Missing:** E5 failing and human attestation are nearby mechanisms, but no residual disposition or its release scope is specified. D2 §5.2 correctly forbids silently discharging failure. |
| A12 | Expose unintended regression separately from deliberate reading replacement (I7 §§2/3). | Automation/proof | Regression finding; causal/suspected merge reference; evaluations | **Missing:** R3 records supersession, not the discovery that an accepted behaviour was unintentionally lost. Q56 found by Q61 is interview evidence, not a verified causal diagnosis here. |
| A13 | Anchor generated links on declaration, step and typed term across wording changes (I7 §5). | Automation/proof | Structural locator plus revision; rendered-span mapping | **Missing:** D1 R1 and D2 §2 define codepoint ranges; D2 §4 relocation searches moved spans, not stable typed referents. |
| A14 | Changing a reading supersedes the old rule and marks every dependent sentence stale (I7 §4 planted check). | Automation/proof | R3; complete generated-by dependency set; E5/output freshness | **Partly:** D1 I4 / D2 §5.3 define stale evaluations/attestations. They do not enumerate sentence dependencies or require a generated-sentence stale indicator. |
| A15 | Link Joe's work rules to their originating turns (I7 §1). | Cross-facet/automation | E1/R1; rule provenance link | **Partly:** memories exist without turn links per I7. D2 §5.8 requires resolution rather than inventing a source; this specifies honest absence, not recovered history. Shares O08/J01. |
| A16 | Demonstrate the a97J08 sentence query, wording history and scratch-change staleness end to end (I7 §4). | Automation/proof | A01–A06/A13–A14 together | **Missing:** neither D1 §7's coverage table nor D2 §3's donor tripwires includes this proof-rendering test. MX rows checked above are insufficient witnesses. |

Hole/sorry tracking is **not** a seventeenth requirement inferred from the facet's
name: I7 §5 explicitly disclaims knowledge of the solve campaign's records. This
keeps the pass-1 placeholder's warning in force (I7 §5; M HEAD tetrahedron).

### 2. Type gaps against the revised design

These extend G01–G13; they do not relabel design promises as deployed types.

| Gap | Closest design type / section | Coverage and missing contract | Affected work |
|---|---|---|---|
| G14 Rewrite-rule / reading decision | D1 E2, R3; D2 §4 reading-as-E2, §5.1 minted acts | **Partly:** an immutable output revision is not a typed head/pattern + guard + phrase + reviewer/packet/merge decision (I7 §3). Define the distinction between rule content and the act accepting it. | Extend WP4; new rule import/acceptance packet. |
| G15 Generated-by / rule application | D1 R4 computed-from; §7 G04 says its evaluator is unspecified; D2 §2 link | **Partly:** generic links can connect records, but no application records sentence, declaration/step, renderer/corpus pins and all R1…Rn rules (I7 §3(a)). No complete-dependency check. | New instrumented renderer packet; WP4/WP10. |
| G16 Structural generated locator | D1 E2 locator and R1; D2 §2 codepoint span, §4 relocation | **Partly:** declaration revisions fit E2, but step/typed-term identity and mapping to output spans across rerenders are unspecified (I7 §5). Text-search relocation is not that guarantee. | Generated-source branch of WP10, independent of WP9 hand edits. |
| G17 Accepted residual | D1 R5/E5; D2 §5.2 failing ledger and §5.3 attestation | **Missing as a disposition:** neither a failure nor an attestation says which defect is knowingly shipped, by whom, for which version/release (I7 §3(b)). Acceptance must not turn a failing predicate into met. | WP6 plus residual view/acceptance packet. |
| G18 Regression finding | D1 R3; D2 §5.2/3 evaluations and independence | **Missing as a finding:** need prior accepted subject, later failing subject, discovering evaluation and attribution of the finding; do not equate every R3 with an accident (I7 §§2/3(c)). | WP6 and rule-history view; new regression comparison packet. |
| G19 Reproducible computed projection | D1 E4, I2/I4; D2 §5.10/11 renderer loss/rebuild | **Partly:** rebuildability and loss declarations help, but no render recipe or sentence derivation schema. I2's exact-one-source-span-or-E3 character map cannot alone explain rule-generated words with several inputs (I7 §§3/5). | WP4/WP11; generated-character traceability acceptance alongside T0. |

Pass-1 assessment updates: C01/G11 are addressed **in design** by D1 D4's evaluator
families and D2 §5.2 (also meets I7's predicate requirement); WP6 still needs aligned
acceptance. C02's landscape restriction is addressed **in design for non-turn
sources** by E2, but not generated dependencies (G15/G19). G05 has an explicit
reading-as-E2 decision in D2 §4; that does not yet define I7's guarded rewrite rules.
G06/G09 attribution and G07 absence have design shapes, with §5 identity, search and
closed-vocabulary revisions; no implementation upgrade is inferred (D1 §7; D2 §§4–5).

### 3. Conflicts and boundaries for VERIFY

- **C12 Rule correction versus text correction.** I7 §§3/5 forbids hand editing
  generated proofs; D1 D6/R2 and W WP9 explicitly implement hand edits over spans.
  These are different authoring contracts, not a reason to remove prose editing.
  The design does not yet say which projection permits which operation.
- **C13 One-character source versus generated derivation.** D1 I2 requires exactly
  one E1/E2 span or E3 per rendered character. I7 §3(a) requires a sentence produced
  under several rules from a declaration/step. Storing a rendered dump as E2 can
  satisfy the immediate byte-origin mapping but does not answer what generated it;
  claiming that alone meets A02 would lose the requested provenance.
- **C14 Live selection versus live computation.** D2 §2 models a live applied-set
  query; I7 §3 calls live proof projection rerendering. D2 §5.11's periodic rebuild
  does not specify when a changed rule invalidates a sentence or which executable
  and inputs recreate it. S09's live causal-model projection similarly was not just
  reselection (pass-1 C09).
- **C15 Accepted failure versus discharge.** I7 §3(b) needs shipped residuals;
  D2 §5.2 requires failure remain open until current met evidence. Both requirements
  must remain visible: permission to ship is not predicate satisfaction. No policy
  connecting that disposition to the requirement list is yet specified.
- **C16 Non-turn nips versus turn-resolving seeds.** D1 R6 permits E2 results;
  I7 §3 needs census records, and S07 needs result spans. D2 §5.8 requires landscape
  spans and says a memory citing no turn cannot be accepted as a span. That rule
  makes sense for claiming turn provenance; whether it also bars a memory pointing
  to a Lean/result revision is not stated. Preserve this ambiguity for review.
- **C17 Link arity and the informal endpoint.** I7 §3 requests a three-way link;
  D1 R4 says two or more endpoints, while D2 §2 R4 has two roles. R7 could group an
  informal endpoint with Lean fields, but has no generated-sentence structural end.
  VERIFY needs a declared multi-link composition or a matching schema, not an
  assumption that the binary registry already expresses the whole claim.

These are specification comparisons, not newly reproduced implementation defects
(D1/D2 sections and I7 cited above). I7 agrees with S on executable predicates and
a landscape wider than turns, and with O on attributed reading correction; the
interviews do not require identical payloads for runs, animal readings and rewrite
rules (I7 §§3/5; S §§3–5; O §§3–4; pass-1 G01/G05/C01/C02).

### 4. Proposed design and work-package changes (not applied)

1. **D1 E2/E4, R4, I2 and D2 registry/fidelity:** add a generated-output contract,
   pinned render recipe and per-sentence rule-application provenance; retain byte
   origin separately from derivation. Pin corpus, renderer, rules and structural
   source address. Add a fidelity table for MX, packets, dumps and merges, explicitly
   recording unrecoverable historical attribution rather than inventing it
   (A01–A06; I7 §§1–4; D2 §3 donor precedent).
2. **D1 R1/R4 and D2 §4:** specify structural anchors and their version-change
   policy, with codepoint spans as rendering coordinates. Add a WP10 proof branch
   testing changed wording with unchanged referent, and refused/typed unresolved
   structural relocation when the referent changes. It depends on the generated
   mapping, not on WP9's prose editor (A08/A13; I7 §5).
3. **D1 R3/E5 and D2 §5:** define rule acceptance, attributed supersession,
   accepted residual and regression discovery as distinguishable acts. Preserve
   the §5.2 failing ledger and §5.3 independent-review distinction. Extend WP6's
   acceptance beyond projection text to versioned predicates and census counts;
   test that an accepted residual stays failing (A03/A04/A07/A11/A12).
4. **D1 I4 and D2 §5.11:** specify sentence dependency completeness and stale
   display on rule change, including an unaffected-sentence control. A new proof
   renderer integration packet must precede the a97J08 VERIFY packet; a generic
   computed-from link without recorded dependencies is not sufficient (A14/A16).
5. **D2 §5.8 and W WP7/8:** clarify non-turn census result acceptance and defect
   versus acceptable dispositions, keeping memory-seed provenance requirements
   explicit. Do not force a Lean result to invent a turn source (A09/A15; C16).
6. **W sequencing:** keep the dated WP1→WP2 submission path and prose WP9 tests;
   add rule/matrix import → generated mapping/structural anchors → history and
   staleness consumer → a97J08 acceptance as a separate proof track. Resolve C12–C17
   in a recorded design decision before claiming that track verified. No proposal
   here changes the closed DERIVE stage or authorises implementation (M Interviews;
   I7 §4; W Order).

### 5. Verdict on claude-7's proposed test

**Not yet: the design supplies useful record families, but does not specify enough
to pass the whole test, and no run here establishes a pass.** R3 can represent
who superseded what and when; E2 can pin Main.lean; R4/R7 can relate manifestations;
E5/I4 can expose stale evaluations. Missing are guarded rule identities and accepted
wording, a complete sentence-to-application mapping, structural step/term anchors,
renderer/corpus/rule pins, retained wording history, and the consumer that propagates
rule supersession to every dependent sentence (D1 §§1–3/7; D2 §§2–5; I7 §4).

The concrete VERIFY witness should resolve a97J08's “In all four cases, we obtain a
contradiction” to the rule versions (including the interview's Q66 ownership/Q83
merge claim), pinned Main.lean:2628–2631, and earlier wording records with actors and
times. Then a scratch rule change must leave the old rule queryable but superseded,
mark all dependent sentences stale, and leave unrelated sentences current. MX
3457 verifies a q83 rule/reading row exists; it does **not** witness those joins or
the planted test. Historical gaps should be reported as unresolved evidence, not
filled from the expected answer (I7 §4; MX rows cited above; D1 I4/I8; D2 §5.5).
