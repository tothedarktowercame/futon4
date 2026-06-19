**Status:** IDENTIFY (2026-05-05, draft 1)

# M-or-training-as-learning-system

Evaluating open-research training as a learning system: structured verification of the candidate framings produced by the ORP evaluation, ahead of UKRN-S design commitments.

## 1. IDENTIFY

### Motivation

The UK Reproducibility Network's Open Research Programme (ORP) has concluded a train-the-trainer effort across approximately twenty-five UK higher-education institutions. UKRN Services (UKRN-S), now established as a Community Interest Company, inherits the analytical infrastructure the ORP produced and faces design decisions about its training portfolio, evaluation infrastructure, and relationships with member institutions.

Two questions are open:

1. *What did the ORP evaluation establish, robustly, about how open-research training translates into sustained institutional practice?*
2. *Given those findings, what design commitments should UKRN-S make, and on what evidence?*

The ORP project produced two kinds of output. The first is *analytical infrastructure*: a T1 evaluation instrument (ethics-approved, KCL MRA-23/24-42968), a population model of ten institutional profiles scored on sixteen NPT factors, a role codebook of twelve roles and sixty-one action codes, five candidate institution-level design patterns, three candidate programme-level patterns, and a logic model factorising the institution-level patterns into two dimensions (delivery viability D and architectural sustainability A). The second is a set of *candidate framings* developed in the working paper that synthesises the ORP findings: a Krowne-tetrahedron account of how local sustainability becomes network-scale; a geometric synthesis (the Minimum Viable Sierpinski Gasket, or MVSG) describing UKRN-S as an evidence apex above three institutional implementation modes; an active-inference simulation suggesting that decisions held in concert on six recurring tensions move every institutional profile toward a multiplied region of D × A space, and that decisions held at low engagement produce drift.

The analytical infrastructure is artefactual: it exists, it is queryable, and what it implies under stated assumptions is checkable. The candidate framings are *candidates*. The mission is to determine which earn standing as the basis for UKRN-S design commitments and which require revision, deferral, or rejection.

### Theoretical anchoring

The mission draws on framings each of which is treated as falsifiable, not as a foundational commitment.

- **Normalisation Process Theory** (May & Finch 2009). The grammar the ORP transcripts were coded against. The codebook is treated as established for this mission; the question is what the model's outputs imply about the institutional landscape and the candidate patterns derived from it.
- **Pattern theory in the architectural-design tradition** (Alexander 1977; with the WP's adaptation to institutional design). Five candidate institution-level patterns and three candidate programme-level patterns are tested as binding-constraint diagnostics under simulation evidence and survey signals.
- **Active inference** (Friston, with the working-paper's adaptation to a population-model agent). The cyber-ant simulation in the WP recasts the population model as an active-inference agent. The framing is carried as a *verification tool*, not as a foundational commitment to active inference as the right account of UKRN-S behaviour.
- **The Krowne sustainability triangle** (Krowne 2003) and its tetrahedral extension (the WP). Candidate framing for how local sustainability becomes network-scale through a shareable-evidence apex. Verification: does the apex framing earn its keep against the empirical evidence the ORP produced?
- **The MVSG**. The WP's geometric synthesis. *Explicitly a candidate under verification*. Not advocated for in this mission until the verification step supports the advocacy.

The mission's verification work is to score each candidate framing against the empirical record, naming the evidence that supports or undermines it.

### Scope in

- A structured account of what the ORP evaluation establishes, where it under-constrains, and which of its outputs are robust enough to ground design commitments.
- A verification pass on each candidate framing carried in the WP (Krowne tetrahedron; MVSG; patterns-as-attractors; active-inference cyber-ant). For each: a verdict (*robust*, *robust under stated conditions*, *revised*, *set aside*) with explicit rationale.
- The bootstrap-phase actions (publish expectation scaffold; run the first T1 evaluation; share evidence back; extend evaluation to peer-led interviews) treated explicitly as *invariant-establishment* — the work that produces the evidence on which subsequent design depends. The bootstrap is not the mission's substantive contribution; it is the substrate the mission's contribution operates on.
- A sketch of the post-bootstrap build-out phase as *invariant-development* — the rollout of the training schema and any coupled-systems framework, conditional on the verified architecture.
- The mission's recommended UKRN-S design commitments, each grounded in named evidence rather than in a candidate framing.
- A plain-language argument (3–5 sentences, no jargon) that explains what the mission contributes and why it is right, accessible to readers outside the writing team.

### Scope out

- Real-data validation against fresh T1-wave evidence (separate mission, conditional on the first T1 wave running). This mission closes when the verification framework and the recommended design commitments are specified — ready to receive the T1-wave evidence and update.
- The futon-track exploration of cyber-ant colonies and the War Machine demonstration (separate work; sketched at `futon2/doc/TN-ukrns-buildout.md`).
- Implementation of specific training programme content. UKRN-S work; post-INSTANTIATE.
- The writing-coherence patterns library (separate mission `M-writing-ethics`).

### Completion criteria

These are testable conditions, not aspirations. Each must be answerable with evidence at VERIFY exit.

1. Each ORP output (T1 instrument, population model, role codebook, five institution-level patterns, three programme-level patterns, logic model) has a robustness verdict — *robust*, *robust-under-conditions*, *revised*, or *set aside* — with the conditions and rationale recorded.
2. Each candidate framing in the WP (Krowne tetrahedron; MVSG; patterns-as-attractors; active-inference cyber-ant) has a verification verdict, with the empirical evidence named.
3. The bootstrap-phase actions are specified as the invariant-establishment work UKRN-S will carry out. Each action has named completion criteria.
4. The recommended UKRN-S design commitments are stated. Each commitment cites the evidence and verdicts that support it.
5. The post-bootstrap build-out phase is sketched as a follow-on mission, with dependencies on this mission's verdicts named explicitly.
6. The plain-language argument exists. It can be read standalone by a board member, a funder, or a member-institution lead, and conveys what the mission contributes without jargon.

### Relationship to other missions

- *Predecessor*: the ORP evaluation work itself — T3 focus-group analysis, role codebook construction, pattern induction. Artefactual; this mission's source material.
- *Sibling*: `M-writing-ethics` — writing-coherence patterns applied to the WP draft. Concerned with prose discipline; this mission is concerned with empirical and architectural verification.
- *Enables*: `M-ukrns-rollout` — post-bootstrap build-out of the training schema; conditional on the verification verdicts produced here. `M-ukrns-coupled-systems` — the futon-track research piece coupling nest and landscape; sketched at `futon2/doc/TN-ukrns-buildout.md`.
- *Cross-references*: the working paper (`npt/working-paper/UKRN_WP_draft_v8.md`) is *the writing team's briefing*, not this mission's output. The mission's output is the verification verdicts and the recommended commitments.

### Source material

- `npt/working-paper/UKRN_WP_draft_v8.md` — the working paper synthesising the candidates this mission verifies.
- `npt/working-paper/assumptions-v2.edn` — population-model parameters: ten institutional profiles, sixteen NPT factors, five levers, five institution-level patterns with gate/support factor structure.
- `npt/working-paper/notebooks/ukrn_phase_space.clj` — Phase 1 baseline simulation (Clojure port of the population model under fixed tension schedules).
- `npt/working-paper/notebooks/ukrn_cyberant_aif.clj` — Phase 2B AIF cyber-ant simulation (active-inference agent, choice-point detection, narrative trace).
- ORP T3 focus-group transcripts (referenced via the role codebook).
- Apr 2025 and Apr 2026 ORP training-priorities surveys; BORS 25 survey-and-training analysis; Open Research Training Programme Curriculum proposal.
- `futon2/doc/TN-ukrns-buildout.md` — coupled-systems extension technote.
- Pattern libraries: `futon3/library/writing-coherence/`, `futon3/library/system-coherence/`, `futon3/library/futon-theory/`, `futon3/library/ants/`.
- `https://www.ukrn.org/training-schema/` — the training schema (~50 topics across seven research-stage categories) the post-bootstrap rollout would draw on.

### Owner and dependencies

- *Mission authoring (writing team)*: Joseph Corneli, with Claude as drafting collaborator. Carries IDENTIFY → MAP → DERIVE → ARGUE → VERIFY.
- *Mission implementation (implementing team)*: UKRN-S — Elle Lahart (training manager), Neil Jacobs (chair), Nic [...] (services lead). Carries INSTANTIATE → DOCUMENT, conditional on accepting the briefing at VERIFY exit.
- *Hand-off contract*: VERIFY closes with the design commitments specified and a verification verdict on each candidate framing. The implementing team takes the briefing and runs the rollout.
- *External dependencies*: the planned T1 evaluation phase (under KCL MRA-23/24-42968 ethics; first wave at approximately nine institutions). The first T1 wave is part of the bootstrap-phase invariant-establishment work; subsequent T1 waves feed real-data validation as a follow-on mission.

### Exit criterion

The mission's owners have read this proposal and agree the gap is real and the scope is right. For institutional missions where the writing team and implementing team are distinct, the implementing team's named owners agree as well. The IDENTIFY draft circulates to UKRN-S for comment before MAP begins.

### Notes on this draft

This is IDENTIFY draft 1. Per Joe's instruction (2026-05-05): *"no way to get it right the first time without data, but we can make it work for us as we go. Applied chaos."* Revisions are expected as MAP surfaces facts and DERIVE forces design choices that revise this scope. Revisions accrete in this document; they do not overwrite.
