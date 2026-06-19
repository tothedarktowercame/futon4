# Retrospective Work as MAP — A Method Note

**Status:** DRAFT v0 (2026-05-01)
**Author:** Joe Corneli + Claude (technical-editing role)
**Context:** Co-equal output of M-peeragogy-rewrite alongside the *what is peeragogy in 2026* essay and the next-wave flexiargs. See `futon4/holes/missions/M-peeragogy-rewrite.md` §4 (revised 2026-04-29) for why this is a co-equal output rather than a sub-task.
**Audience:** Community stewards, project caretakers, consultants (Agile Coach / facilitator / OD), and anyone whose community has a canonical artifact (a book, a codebase, a constitution, a curriculum, a wiki) that has aged and is asking to be renewed.
**Portability claim:** the method described here is general. The infrastructure (pattern library, sidecar annotations, browser-rendered axes) is one *implementation*; the method survives substitution.

---

## 1. What this method is

A canonical artifact in a community — a Handbook, a codebase, a course, a project charter, an organisation chart — accretes meaning the community itself can no longer fully see. When the community decides to renew the artifact, the natural move is to jump to *what should we change?*  This method names a different first move:

> **Treat the artifact's past as MAP-stage data. Let the past instruct the rubric you renew with — don't just sort the past by a rubric you brought from outside.**

Concretely: before any rewrite, refactor, or replacement, do a structured retrospective on the artifact and the practice that surrounds it, organised as the *MAP* phase of a futonic mission. The retrospective produces a rubric (built from the artifact's own self-claims), a corpus of voices around the practice (transcripts, interviews, adjacent literature, lived experience), a synthesis comparing prior to posterior, and a *verified* derivation of carrier-shape decisions for whatever comes next. The rewrite (or its alternative) is *downstream* of all of this.

The method is named **Retrospective Work as MAP** because it is the futonic *MAP* phase applied retrospectively to the community's own canonical artifact. The mission lifecycle in `futon4/holes/mission-lifecycle.md` provides the phase structure; this note describes how to use the MAP phase when the "system you're mapping" is your own community's history.

---

## 2. Lineage — three retrospectives that taught it

This method did not appear from nowhere. It crystallised across three distinct retrospective exercises on three different scales:

### 2.1 PlanetMath, 2000–2014 (CORNELI-thesis, `futon6/resources/CORNELI-thesis.pdf`)

Joe's PhD thesis was a 14-year retrospective on PlanetMath as a peer-production / peer-learning environment. It produced a vocabulary (the original *paragogy* patterns), a substantive critique of where the project had stalled, and structural claims about peer learning that fed into the later Peeragogy Project. The PhD was retrospective work on a community's canonical artifact — the artifact in this case being PlanetMath itself, including its software, governance, and active community. The retrospective was scholarly: it produced a thesis, not a refactor, and the thesis became upstream of subsequent work elsewhere.

### 2.2 M-live-geometric-stack, 2026 (futon3, complete 2026-04-28)

The futon stack's own retrospective on itself, run *live*. ~360,000 hyperedges across 16 labels covering 12 distinct codebases, with a tension-field (T) and gradient/laplacian operators that observe the practice as it runs. M-live-geometric-stack is a retrospective in the *substrate-2* sense: the stack ingests its own history and renders it as queryable structure. The boundary between "retrospective" and "live observation" collapses when the substrate updates as the history grows. Pairs with `M-reflective-discipline` (PSR/PUR/PAR-as-edges) which adds a tangent-bundle layer for reflective records over the substrate.

### 2.3 M-peeragogy-rewrite, 2026 (futon4, this mission)

A 10–14 year retrospective on the Peeragogy Handbook and the practice around it. Built a rubric of the Handbook's 2016 self-claims (60 immutable items), transcribed and ingested 13 podcast episodes covering 2020–2022 (~75,500 words), applied the rubric outward against the corpus with a *one substantive promotion per source* discipline (13 promotions across 13 episodes), synthesised prior-vs-posterior, derived a carrier-shape decision, verified the derivation against specific corpus quotes, and reframed the original mission's INSTANTIATE plan accordingly. The mission doc is the worked-example record; this note is the portable method extracted from doing it.

### 2.4 What the lineage gives the method

| Source | Contributes |
|---|---|
| PlanetMath PhD | The shape of *long-form retrospective producing a generative artifact, not a refactor* |
| M-live-geometric-stack | The shape of *the substrate observes itself*; the discipline of ingesting the past as queryable structure |
| M-peeragogy-rewrite | The discipline of *living rubric*, *one promotion per source*, *VERIFY against quotes*, *ARGUE reframe when the protagonist turns out to be the method* |

The method is portable because the moves do not depend on the futon stack. They depend on a community willing to treat its own past as data, and tooling adequate to make the past queryable in some form (a directory of transcripts and a markdown rubric is enough; the futon Essays browser is one richer-cost option).

---

## 3. The seven moves

The futonic mission lifecycle (`futon4/holes/mission-lifecycle.md`) defines 7 phases: IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE → DOCUMENT. The retrospective method uses the same 7 phases, but specialised:

### Move 1 — IDENTIFY-redux: confirm the gap, stay open on the carrier

Name the gap honestly. In the Peeragogy case: *the Handbook has aged unevenly; specific recommendations have rotted; the project visibly stalled in 2016.* This is the surface gap.

But: stay open on what *kind* of work resolves the gap. The first instinct is "rewrite the artifact." That instinct is a hypothesis, not a foregone conclusion. The IDENTIFY-redux explicitly enumerates carrier-shape options and *defers* the choice to DERIVE. M-peeragogy-rewrite §1.IDENTIFY listed five options:

- (a) rewrite the artifact chapter-by-chapter
- (b) refactor into a small boxed-set of mini-artifacts (zines)
- (c) hybrid — keep the original as reference, extract a smaller surface as entry point
- (d) some new shape that emerges from the survey
- (e) practice-supporting infrastructure rather than a text

The list is enumerable for any artifact. For a codebase: (a) rewrite, (b) extract microservices, (c) keep core, replace edges, (d) some new architecture, (e) tooling that makes the existing code easier to live with. For an organisation: (a) reorg, (b) split, (c) keep structure, change practice, (d) new shape, (e) supporting practice. The shape varies; the discipline of holding the carrier-question open during MAP is the constant.

### Move 2 — Bootstrap a prior rubric from the artifact's own self-claims

Read what the artifact says *about itself*. Not what its readers say about it; not what you remember about it; not what adjacent literature says about it. The artifact's own self-understanding.

In the Peeragogy case this meant the Handbook's preface, foreword, *How to use this Handbook*, *Peeragogical Assessment* chapter, *Antipatterns* chapter, *Peeragogy in Action* chapter — anything reflexive in the artifact's own voice. Extract its self-claims into a rubric: "things the artifact says it does / cares about / isn't / claims about its own form."

The rubric is the *yardstick* for reading the rest of the corpus. Building it from inside (rather than importing one from current academic / consulting / industry vocabulary) avoids smuggling outside frames into the survey. The rubric is the artifact's prior — what it claimed to be at the time it was written.

For the Peeragogy case the rubric landed at 60 items in 7 sections. For other artifacts the count and structure will vary. The discipline is: *the rubric must be from inside, not from outside.*

### Move 3 — Assemble an outward-looking corpus

Find recordings of the practice as it was actually lived, and as it has continued to evolve. In the Peeragogy case this was the podcast series — 13 episodes from 2020–2022, locally transcribed via faster-whisper, covering both peeragogy-internal voices and adjacent practitioners. The corpus deliberately includes voices who do not call themselves "peeragogues" — Wikipedia editors, open-source learning practitioners, contemplative-practice teachers, AI-ethics researchers — because the question being tested is *where do these patterns recur, named or unnamed.*

For a codebase the equivalent corpus might be: commit history, code review threads, retrospective notes, post-mortems, related-team-channels. For an organisation: meeting transcripts, employee interviews, exit interviews, internal memos, all-hands recordings. The constraint: the corpus must be *broad enough to surprise you*, not narrow enough to merely confirm.

### Move 4 — Apply the rubric, with living-rubric discipline

Read each source against the rubric. For each source, allow *one substantive promotion* into the rubric — a new item that the source surfaces and the rubric does not yet have. Other findings stay in per-source notes; the rubric grows by one item per source, not by everything.

Why one-per-source: prevents rubric inflation (every reading adds noise), forces ranking (which finding is most load-bearing for *this* source), and produces a cumulative rubric whose growth-shape is itself a finding (which sections grew most? which sections were already adequate?). Original prior items stay immutable; only new items get added.

In the Peeragogy case this produced 13 promotions across 13 episodes. The distribution across rubric sections turned out to be itself a finding: every promoted item was about *stance, discipline, structure, enabling-condition, bottleneck, or framing-reframe*. None of them added a new operational pattern. The Handbook's 12 process patterns were essentially intact in 2026 voice; what the post-2016 corpus extended most was the *texture of how the practice is held together*. That distribution-finding would have been invisible without the one-per-source discipline.

### Move 5 — Synthesise with care: corroborated / shifted / challenged / new

After all sources are read, write a synthesis comparing prior to posterior. Distinguish four findings explicitly:

- **Corroborated** — prior items that the corpus confirmed across multiple speakers / sources.
- **Shifted** — prior items whose meaning the corpus refined or refocused, without contradiction.
- **Challenged** — prior items the corpus *contradicted* or showed had not held up. Be honest here; this is the test of the synthesis.
- **New** — items the corpus surfaced that the prior simply did not have. The promoted items live here.

The synthesis is a MAP-stage artifact. It is *not* the renewal proposal. Holding the synthesis at MAP — naming it as findings, not as recommendations — protects against the temptation to start designing the renewal mid-survey.

For Peeragogy, the synthesis lives at `futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/00-synthesis-2016-prior-vs-2026-posterior.md`. The most load-bearing finding (corroborated 7+ times across the corpus): the practice predates the name. The most consequential finding (single outside voice, decisive): the practice is a worldview shift, not a method.

### Move 6 — DERIVE the carrier shape from MAP findings

With the synthesis on file, return to the carrier-shape options enumerated in IDENTIFY. Now they can be evaluated against findings rather than against intuition. Which carrier-shape *fits the work the practice actually does*?

For Peeragogy this resolved to (b)+(e): a small boxed-set of mini-artifacts (zines) plus practice-supporting infrastructure, *not* a chapter-by-chapter rewrite of the canonical artifact. The reasoning: the canonical artifact's history showed a stall mechanism that lives in its monolithic shape; the patterns within it had remained intact in 2026 voice; a renewal that decomposed the artifact into smaller, single-point-clarity pieces — and added scaffolding for the practice itself — preserved the patterns while routing around the stall mechanism.

For other artifacts the DERIVE may go differently. The discipline is: *the carrier choice is justified by MAP findings, not by the original gap-naming.*

### Move 7 — VERIFY against quotes; ARGUE may reframe

Before INSTANTIATE, verify the DERIVE design against the actual evidence base. Each load-bearing claim in DERIVE should trace to specific corpus quotes or specific synthesis findings. Distinguish *corpus-grounded* claims from *synthesis-inferences* (claims you derived but did not directly observe) from *mission-context* claims (background knowledge separate from the corpus). All three are valid; conflating them is not.

The VERIFY artifact for Peeragogy is at `futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/00-verify-derive-against-posterior.md`. It traces each load-bearing DERIVE claim to specific quotes from specific episodes from specific speakers, and flags four overstatement-traps for downstream artifacts to avoid.

VERIFY may reveal that the ARGUE phase needs a reframe. In the Peeragogy case, the original ARGUE was a self-flagellating *TryHarder* — "the artifact needs revision; here's the queue we built to do it." The VERIFY pass surfaced something the original ARGUE had missed: in doing the retrospective work, the practice had run its own discipline on its own canonical artifact. *The method was the protagonist*, not the artifact-revision the retrospective initially promised. The renewal becomes downstream-operational; the method-applied-to-self becomes the more interesting half.

This is a recurrent reframe in retrospective work: **the method is often more valuable than the artifact-revision that motivated it**, because the method is portable to other communities while the revision is local.

### What about INSTANTIATE and DOCUMENT?

INSTANTIATE produces the actual artifacts (renewed text, new infrastructure, handoff packages). DOCUMENT makes them findable. Both are mission-specific; the method note doesn't legislate them. But: the INSTANTIATE in retrospective work often is *not* shaped like "Claude / consultant does the work and Joe / community accepts it." If the retrospective is on a *peer-organised* artifact, the INSTANTIATE itself should be peer-organised — handoff packages to named potential coauthors, with the original team in a *technical-editing role* rather than primary-author role.

For M-peeragogy-rewrite this is §6 INSTANTIATE (revised 2026-04-29). For other communities the analog is: who in the community wants to take which piece of the renewal, what's the smallest viable contribution, what's the larger ask, how do we hand off without the renewal becoming centralised in one person's labour.

---

## 4. What this method is NOT

The discipline is sharper if we name what it is not. Five common mistakes:

### Not autopsy

Retrospective-as-MAP looks at the past for *generative* purposes — what should we build next, given what the past actually says? — not for forensic purposes. The question "what went wrong?" is welcome but not primary. The question "what work is the practice actually doing?" is primary.

### Not nostalgia

The past is data, not a golden age. The rubric extracts the artifact's self-claims because those claims are the artifact's prior — not because the original was right. Many prior items will be challenged by the corpus; the method must accept the challenges.

### Not litigating the past

The retrospective is not an audit of past contributors' decisions. The discipline is to read the past as a record of practice under the conditions that obtained then, not as a record of choices the present can grade. Where past choices look poor in hindsight, the method asks *what conditions would have made the better choice visible at the time?* — this is the conversion-factor question, not the blame question.

### Not Agile-style retrospective

A typical Agile retrospective is short, recurring, and tied to a specific delivery cadence (sprint, increment, release). Retrospective-as-MAP is one-off, long-form, and tied to a specific renewal question. The two are complementary; one does not substitute for the other. (For consultants doing both: the Agile retrospective is *Scrum's retrospective ceremony*; this method is closer to the kind of multi-month archival retrospective an OD consultant or organisational historian might run.)

### Not unfettered design

The temptation mid-MAP is to start sketching the renewal — "this would be a great chapter" — before MAP completes. The discipline is to hold renewal design at DERIVE; let MAP finish. The synthesis is what licenses the renewal design; pre-empting MAP smuggles in undertested intuitions.

---

## 5. The protagonist is the method

Retrospective-as-MAP, applied honestly, often turns out to produce more value as a *portable method* than as a *local renewal*. The local renewal will be specific to the artifact; the method generalises.

For M-peeragogy-rewrite this realisation forced an ARGUE reframe: the three artifacts the mission is producing (essay / next-wave-flexiargs / this method-note) are *co-equal outputs of the method-applied-to-self*, not three sub-tasks of a Handbook revision. The Handbook revision question becomes downstream-operational — informed by the worldview the essay names, the moves the flexiargs name, the discipline this method-note names — not the mission's stuck point.

For other communities the same reframe may apply. **The retrospective-as-MAP method is itself an artifact that other practices can pick up and use.** That portability is the value; the local renewal is a side effect.

This has implications for how to scope retrospective work:

- If you scope it as "we'll renew the artifact," you risk under-counting the method as a separate output.
- If you scope it as "we'll retrospect honestly and see what carrier-shape MAP suggests," the method-as-output is part of the success criteria from the start.

The latter is more honest about what retrospective work usually produces.

---

## 6. Reusability — what this method assumes, and what it doesn't

For another community / consultant to pick this method up, the following things are required:

### Required (substrate-light)

- A *willingness* to treat the past as data — not as authority, not as embarrassment, but as evidence about what work the practice does.
- Some way to render the past *queryable*. The minimum is a directory of source documents (transcripts, interview notes, archival material) and a markdown rubric file you can read against them. The maximum is a futon-style hypertext browser with ingested annotations. The minimum is enough.
- A community sponsor / steward / consultant who will hold the discipline of *not jumping to renewal design* until MAP completes. This is the hardest part. Most organisations cannot wait that long.

### Optional (substrate-rich)

- A pattern library where common notions / coordination moves can be reified as durable artifacts (futon's flexiarg format, but other formats work).
- Tooling for sidecar annotations (futon's Essays browser, but Hypothesis / Hypothes.is, marginalia tools, or even Google Docs comments can substitute).
- A mission-lifecycle discipline (futon's IDENTIFY → MAP → DERIVE → ARGUE → VERIFY → INSTANTIATE → DOCUMENT phases). For lighter use, the seven moves in §3 above can be followed without the mission-lifecycle scaffolding.

### Adoption surfaces

This method has been validated on three quite different cases (PhD-thesis-scale, live-stack-scale, mid-life-handbook-scale). Likely adoption surfaces include:

- **Open-source projects** facing a major architectural transition (the codebase is the canonical artifact)
- **Communities of practice** whose founding documents have aged
- **Organisations** doing a multi-year strategic-review retrospective (treat the strategy document as the artifact)
- **Curricula** undergoing renewal (treat the syllabus + course materials as the artifact)
- **Consulting engagements** where the deliverable is a renewal recommendation — this method is closer to *how the consultant should look* than to *what the consultant should produce*. (See §7 below.)

---

## 7. Consulting application (notes for the Agile-Coach / OD use case)

Several of the moves in this method translate directly into a consulting / facilitation engagement. Sketching the translation, briefly:

- **Move 1 (IDENTIFY-redux)** maps to *initial scoping conversation*: name the gap honestly, but resist the client's instinct to pre-commit to "rewrite the strategy" / "reorg the team" / "kill the product line." Hold the carrier-shape question open through MAP.
- **Move 2 (prior rubric)** maps to *internal-document review*: the strategy doc, the org charter, the team's stated values. Extract self-claims. Don't bring in McKinsey frameworks yet.
- **Move 3 (outward corpus)** maps to *interviews*: line / staff / cross-functional / customer / departed-employee. Specifically including voices that do not use the client's vocabulary. The retrospective survives importing voices that do not speak the practice's name.
- **Move 4 (living rubric, one promotion per source)** maps to *interview synthesis discipline*: per interview, allow one substantive new theme into the rubric; other findings stay in per-interview notes. Prevents synthesis inflation; forces ranking.
- **Move 5 (synthesis: corroborated / shifted / challenged / new)** maps to *interim findings report*: structured exactly this way, and shared back to the client *before* recommendations.
- **Move 6 (DERIVE the carrier from findings)** maps to *recommendation memo*: the carrier-shape decision is justified by findings, not by the consultant's prior intuition.
- **Move 7 (VERIFY)** maps to *evidence-base appendix*: each load-bearing claim in the recommendation memo traces to specific interview quotes or specific corpus findings. The appendix is what keeps the recommendation defensible against sponsor pushback.

For the UKRN-S Agile Coach context (Joe potentially as facilitator for quarterly / monthly retrospectives at scale): the seven moves can be applied at quarter-cadence as a multi-team retrospective, with the rubric refreshed annually. The corpus is the team's actual delivery records, retrospective notes, and stakeholder-feedback records. The method's discipline (one-per-source promotions, corroborated/shifted/challenged/new synthesis) is more rigorous than typical Scrum retrospectives and is a candidate differentiator for the engagement.

This is a candidate **🌐4 (paragogy revenue)** probe surface — see `futon5a/holes/stories/globe4-paragogy-revenue.md`. The method-note's deliverable-shape (interviews + synthesis + verified recommendation) is a paragogy-shaped paid service. Whether anyone pays for it is empirical.

---

## 8. Open questions

Honest about what this method note does not yet resolve:

- **Time-cost.** M-peeragogy-rewrite's MAP phase took ~2 days of intensive work for one transcribed-podcast-series corpus; a real consulting engagement on a year of interview data would scale very differently. The method has not yet been validated at the scale of a 6-month engagement.
- **Tool-substitution.** The substrate-rich version uses futon's Essays browser, sidecar annotations, and pattern libraries. The substrate-light version (markdown + directories) has been argued for but not actually run. Worth doing a substrate-light retrospective on a different artifact to confirm the substitution holds.
- **Sponsor-discipline.** The single hardest move is keeping the sponsor / community / client from jumping to renewal design mid-MAP. The method has no mechanism for this; it relies on the facilitator's authority. Whether this generalises to facilitators without prior trust is open.
- **What kinds of artifact resist the method.** This note assumes the artifact has reflexive material (self-claims). Artifacts that do not — terse codebases, undocumented systems, oral traditions — would need an adapted Move 2. The method has not been tested on non-reflexive artifacts.
- **Distinguishing carrier-shape (e) from (a)–(d).** Move 6's enumeration treats (e) "practice-supporting infrastructure" as one option among several, but (e) is structurally different from text-carriers (a)–(d): it requires building tooling, scheduling regular returns, sustaining a community. Whether a retrospective produces the labour to actually instantiate (e) is a separate question from whether MAP findings recommend it.

---

## 9. Where the worked example lives

- **Mission doc:** `futon4/holes/missions/M-peeragogy-rewrite.md` — full lifecycle record with §1.IDENTIFY, §2.MAP, §3.DERIVE, §4.ARGUE, §5.VERIFY, §6.INSTANTIATE all reflecting the 2026-04-29 reframes; prior versions preserved as `.prior` sections.
- **Rubric:** `futon4/holes/labs/M-peeragogy-rewrite/peeragogy-2016-prior.md` — 60 immutable items + 13 promotions.
- **Corpus:** `futon4/holes/labs/M-peeragogy-rewrite/podcasts/transcripts/<id>/<id>_full.txt` × 13 episodes; ~75,500 words total.
- **Per-episode notes:** `futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/{01..13}-<id>.md`.
- **Synthesis:** `futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/00-synthesis-2016-prior-vs-2026-posterior.md`.
- **VERIFY artifact:** `futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/00-verify-derive-against-posterior.md`.
- **Reviewer report (now MAP-stage evidence):** `futon4/holes/labs/M-peeragogy-rewrite/peeragogy-handbook-reviewer-report-2026-04-29.md`.
- **Proof-of-concept toolchain:** `futon4/scripts/peeragogy-{fetch-corpus,migrate-annotations,flip-to-mw,publish}.clj` plus `futon4/scripts/peeragogy-podcasts.py`.

---

## 10. Lineage references

- CORNELI, J. (2014). *Peer Produced Peer Learning: A Mathematics Case Study.* PhD Thesis, The Open University. `futon6/resources/CORNELI-thesis.pdf`. The earliest worked example of the retrospective-as-generative-artifact shape.
- M-live-geometric-stack, completed 2026-04-28. `futon3/holes/missions/M-live-geometric-stack.md`. The live-substrate version of retrospective; the method-as-substrate side.
- M-reflective-discipline (in progress). `futon2/holes/missions/M-reflective-discipline.md`. PSR/PUR/PAR-as-edges; the tangent-bundle layer over the substrate; downstream of M-live-geometric-stack.
- M-peeragogy-rewrite (in progress). `futon4/holes/missions/M-peeragogy-rewrite.md`. The Handbook-scale exemplar; this note is its co-equal output.

---

## Decision log

- **2026-05-01** — DRAFT v0 written as Artifact 3 of M-peeragogy-rewrite, ahead of any of the per-person handoff packages, so that future drafts of the essay and the next-wave-flexiargs can cite back to a stable method-note. Status: DRAFT v0; not yet reviewed by Joe.
- **TBD** — Joe review pass; revise §7 (consulting application) per Joe's UKRN-S engagement pattern; tighten §3 (the seven moves) once one substrate-light pilot has been run to validate the tool-substitution claim.
- **TBD** — once stable, link from `futon5a/holes/stories/globe4-paragogy-revenue.md` as a named Layer-3 (capability-applied) probe surface (per the three-layer paragogy revenue model).
- **TBD** — once stable, register as a docbook entry so the method is discoverable outside this mission's lab directory.

---

*This is a draft. Review welcomed; revisions expected.*
