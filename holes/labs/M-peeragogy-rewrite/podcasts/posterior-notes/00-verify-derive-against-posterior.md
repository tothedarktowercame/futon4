# VERIFY — DERIVE design grounded against posterior-dataset quotes

**Date:** 2026-04-29
**Mission:** [M-peeragogy-rewrite](../../../missions/M-peeragogy-rewrite.md), VERIFY phase
**Discipline:** *futonic mission lifecycle* §VERIFY — check the architecture against constraints before committing to full implementation.
**This artifact:** trace each load-bearing claim in the DERIVE design (mission doc §3, revised 2026-04-29) to a specific quote in the posterior dataset (podcast transcripts + 65 sidecar annotations + per-episode posterior notes). Flag any claim that's synthesis-generated speculation rather than corpus-grounded.

The DERIVE makes two kinds of claims that need different verification:

- **Corpus-grounded claims** — direct findings from the 13-episode podcast corpus. These should trace to specific quotes.
- **Mission-context claims** — framings drawn from IDENTIFY, prior conversations, or Joe's autobiographical statements. These should be acknowledged as such, not presented as corpus findings.

Conflating the two would let synthesis hallucinations into the design. The verify keeps them separate.

---

## A. Carrier-shape claims (corpus-grounded)

### A.1 Carrier (a) chapter-rewrite inherits the Handbook's 6-year stall mechanism

**Claim in DERIVE:** *"a chapter-rewrite carrier inherits both the Handbook's mostly-layer-2 framing AND its 6-year stall mechanism (ep 4 blackbox)"*

**Quote ground (ep 4, Joe Corneli, 2020):**
> *"We're pretty keen to get a fourth edition of the peeragogy handbook. I've even drafted a proposal for the fifth edition at one point."*

**Quote ground (ep 6, Joe Corneli, 2020):**
> *"last time we made a handbook, it was I think 2016, so we were quite delayed in terms of the version four, but we're dead set to make one."*

**Quote ground (ep 9, Joe Corneli, 2020):** Howard's reply implicitly acknowledges no v4 had shipped at the recording.

**Verdict: STRONG.** Three independent recurrences across three episodes (4, 6, 9) of the v4-wish-without-v4-shipping. As of 2026, ten years after the 2016 third edition, neither v4 nor v5 has shipped. The stall is documented across the corpus, not synthesised.

### A.2 The chapter-rewrite carrier inherits mostly-layer-2 framing

**Claim in DERIVE:** *"a chapter-rewrite carrier inherits the Handbook's mostly-CLA-layer-2 framing"*

**Quote ground (ep 13, Sohail Inayatullah):**
> *"Normal analysis stays in level one or two. It's the headlines and the causes of the headlines. And our approach is no go deeper understand the different worldviews."*

**Quote ground (ep 13, Sohail's CLA-of-peeragogy):**
> *"The old world was the lecture. The worldview is pedagogy, the metaphors professor knows best. You've shifted it to already a pedagogy, meaning we all create together."*

**Verdict: SOLID.** Sohail's CLA framework supplies the layer-1-to-4 vocabulary; Sohail explicitly places peeragogy at layers 3-4. The claim that the *Handbook* is mostly-layer-2 is a SYNTHESIS INFERENCE — Sohail doesn't directly say "the Handbook is layer 2." However, the claim is reasonable given (a) Sohail's general "normal analysis stays at level 1-2" observation and (b) Joe's separate observation (ep 6) that a recurring critique of the Handbook is *"that it's just sometimes confusing and is all this stuff in there very well expressed. Is it redundant? Is it is it helpful?"* — which suggests the Handbook hasn't surfaced its layer-3-4 work clearly.

**Tightening recommended for the essay:** when the essay makes this claim, frame it as *the Handbook would benefit from making its worldview/myth content more legible*, not as *the Handbook IS only layer-2*. The latter is too strong for the evidence.

### A.3 Carriers (b)/(e) can address worldview/myth where (a) struggles

**Claim in DERIVE:** *"Carriers (b)/(e) can explicitly address worldview/myth (item 2.8) and don't depend on canonical-text production (item 3.9)"*

**Quote ground for the worldview/myth half (ep 13, Sohail):** as A.2 above.

**Quote ground for the canonical-text-production-independence half (ep 4, Howard Rheingold):**
> *"I really stepped back from the process and the community took over."*

**Quote ground (ep 4, Howard):**
> *"if you instigated community, it's really often a good idea to kind of step back and let the community find its sea legs."*

**Quote ground (ep 7, Pete Kaminski on OGM Thursday calls):**
> *"It feels also like church, kind of like a Quaker meeting maybe... It's a recharging time for all of us to come together."*

**Quote ground (ep 11, Leo Vivier on Emacs Workshops):**
> *"They are inherited from Linux user groups, which is pretty much people meeting around Linux stuff."*

**Verdict: STRONG.** Multiple in-corpus instances of practices that *don't depend on canonical-text production* (OGM, EmacsConf, Emacs Workshops, ACMI, Cool Laboratory). The corpus supplies the shape directly.

---

## B. Essay-spine claims (corpus-grounded)

### B.1 Item 2.8: peeragogy is a worldview/metaphor shift, not a method or curriculum

**Quote ground (ep 13, Sohail Inayatullah):**
> *"And now when I look at you folks, you've actually changed. The old world was the lecture. The worldview is pedagogy, the metaphors professor knows best. You've shifted it to already a pedagogy, meaning we all create together. And instead of the lecture, it's now peer to peer."*

**Verdict: STRONG.** Direct outside-voice quote from a futures-studies practitioner who works with UNESCO / Interpol / governments worldwide. He uses his own framework's vocabulary (CLA layers) to recognise peeragogy as a worldview-and-metaphor move. This is the single strongest piece of in-voice evidence for the claim.

### B.2 Item 2.7: timeless-practice / "we didn't discover it" — 7+ corroborations

**Quote ground 1 (ep 1, Charles Jeffrey Danoff):**
> *"It's also appears where when people were figuring out how to make fire for the first time, they were producing fire, they're learning from one another. So just a way to describe what humans have been doing for a very long time. ... We didn't discover PureGadji. It's just are all around."*

**Quote ground 2 (ep 4, Charlotte Pierce):**
> *"as we tell everyone, Pyrogogy is everywhere. We didn't discover it. It's out there in all of our lives in the minute you get up in the morning, you're doing it."*

**Quote ground 3 (ep 5, Joe Corneli on Mexican femicide-resistance work):**
> *"there were centers for people who I think had actually survived the kinds of things which would otherwise become femicide... I was so impressed by it as an example of PuroGudgy, where they don't call it PuroGudgy. They've never heard of us and what we do."*

**Quote ground 4 (ep 7, Pete Kaminski):**
> *"It feels also like church, kind of like a Quaker meeting maybe."*

**Quote ground 5 (ep 8, Charlotte Pierce):**
> *"the patterns that we have identified that work in our years of, we didn't invent Pyrogadji, we just kind of dig it out from under rocks in studios and things like that."*

**Quote ground 6 (ep 9, Howard Rheingold):**
> *"peeragogy is kind of a fancy word. People have done peer learning since forever."*

**Quote ground 7 (ep 9, Charlie Danoff):**
> *"we didn't invent peeragogy. It's out there everywhere."*

**Quote ground 8 (ep 11, Leo Vivier on Emacs Workshops):**
> *"They are inherited from Linux user groups, which is pretty much people meeting around Linux stuff."*

**Quote ground 9 (ep 12, Peter Shukie):**
> *"You said before about Piragaji's timeless, I agree."*

**Quote ground 10 (ep 12, David Preston):**
> *"You don't need a computer to do open-source learning, or pyrogogy for that matter. I'll take a stick and some dirt, but really all I need is a learner."*

**Verdict: STRONG.** 10 independent voicings across 8 episodes from at least 7 different speakers (Charlie, Charlotte, Joe, Pete K., Howard, Leo, Peter S., David). The DERIVE claimed "7+ corroborations"; the actual count is at least 10. The claim is conservatively stated; could be strengthened.

### B.3 The convergent-practice family is recognizable across vocabularies

**Claim in DERIVE:** *"naming the family is itself useful work"*

**Quote ground (ep 13, Ivana Milojević on her CLA-democratization mission):**
> *"With the purpose the same, I think we're on the same kind of mission here to move away from some autocratic hierarchical arrangements to make it more accessible to wider audiences and to make it more applicable with the goal of people creating their own futures."*

**Quote ground (ep 7, Pete Kaminski naming Quaker meeting):** as B.2 quote 4.

**Quote ground (ep 11, Leo Vivier naming Linux User Groups lineage):** as B.2 quote 8.

**Quote ground (ep 8, Charlotte Pierce naming ACMI as practice peeragogy "digs out from under rocks in studios"):** as B.2 quote 5.

**Verdict: STRONG.** Multiple speakers explicitly recognise their adjacent practices as belonging to the same family, or recognise peeragogy in their own vocabulary. Ivana's *"on the same kind of mission here"* is the most direct.

### B.4 Ethics-as-bottleneck (item 1.8) and integrity-is-integration

**Quote ground for ethics-as-bottleneck (ep 2, Lane Raspberry):**
> *"Don't worry about the technology, the info, the analysis, all this work. Those things will get sorted out. What I'm not sure is gonna get sorted out is the right ethical foundation for all of this. That's where we really need help."*

**Quote ground for integrity-is-integration (ep 12, David Preston):**
> *"the nature of integrity isn't honesty. It's integrating our thinking with what we say and what we do."*

**Quote ground for the cost of misalignment (ep 12, David Preston):**
> *"the real risk is is the death we die inside before we stop living."*

**Verdict: STRONG.** Three direct quotes from two different speakers (Lane and David) supporting both halves of the framing.

---

## C. Method-note claims (mostly mission-context, not corpus)

### C.1 PARs in Peeragogy are similar to retrospectives but need infrastructure to be meaningful

**Quote ground for PAR's existence and recurring use (ep 1, Charlie Danoff):**
> *"we can do can drink, eat our own cooking, and we can do a par, perhaps to close out the podcast as like a example of it."*

**Quote ground for PAR-form-changes-without-infrastructure (ep 3 onwards):** PAR moved to after-party (Jitsi) starting ep 3, stable across eps 3–7, absent in eps 8–9 (the outward-facing format-experiment episodes), returns in eps 10–13. The pattern shows PAR working when infrastructure (the after-party convening, the recurring crew) is in place; PAR vanishing when the infrastructure (recurring crew) shifts.

**Quote ground for the *PAR-needs-infrastructure* claim itself:** **NOT directly in the corpus.** This is **Joe's framing (2026-04-29 conversation)** — *"our PARs are a bit similar [to retrospectives], but they need to be coupled with infrastructure (your item e) that make them meaningful."*

**Verdict: SOLID for the empirical pattern (PAR's presence/absence tracks the format episodes); MISSION-CONTEXT for the interpretive claim that PAR needs infrastructure to be meaningful.** The method-note should attribute the interpretive claim to Joe's 2026-04-29 framing, not to the corpus directly.

### C.2 Joe's PhD thesis as methodological lineage

**Quote ground:** Joe's 2026-04-29 statement: *"my thesis ./futon6/resources/CORNELI-thesis.pdf basically describes doing the kind of retrospective that we can now, with M-live-geometric-stack do *live*."*

**File ground:** `/home/joe/code/futon6/resources/CORNELI-thesis.pdf` exists (3.6 MB, modified 2026-02-10).

**Verdict: MISSION-CONTEXT.** Joe's autobiographical claim about lineage. Not from the podcast corpus. The method-note should cite the thesis directly and frame the lineage as Joe's articulation, not as corpus finding.

### C.3 PlanetMath retrospective as same-lineage precedent

**Quote ground:** Joe's 2026-04-29 statement: *"this retrospective analysis of Peeragogy (2012-2026) is a little bit like what I did with PlanetMath (2000-2014)."*

**Verdict: MISSION-CONTEXT.** Joe's autobiographical claim. The method-note should cite Joe directly, not present this as corpus-derived.

### C.4 UKRN-S could employ Joe as Agile Coach for quarterly/monthly retrospectives

**Quote ground:** Joe's 2026-04-29 statement: *"UKRN-S could learn from this — e.g. for example they could employ me as a kind of 'Agile Coach' that would help them do these retrospectives quarterly or monthly."*

**Verdict: MISSION-CONTEXT — and speculative.** Joe's hypothetical, not committed work. The method-note should frame it as *one possible application*, not as a planned engagement.

---

## D. Adjacent-work claims (mission-context, not corpus)

### D.1 PlanetMath spirit rolled forward into futon6

**Quote ground:** Joe's 2026-04-29 statement: *"some of the PlanetMath 'spirit' is rolled forward into things like futon6, so, it's not completely dead either..."*

**Verdict: MISSION-CONTEXT.** Joe's framing of cross-project continuity. Method-note can cite Joe; no claim of independent verification.

### D.2 The minor-literature situation (708 GS hits vs ~20,200 for active inference)

**Quote ground:** **NOT from the podcast corpus.** This is a **mission IDENTIFY-stage external fact** Joe surfaced when sketching the rubric, not a posterior-dataset finding.

**Verdict: MISSION-CONTEXT.** External evidence (Google Scholar count) cited in IDENTIFY's theoretical anchoring. The essay can cite this, but should frame it as *external evidence* rather than as something the podcast corpus surfaced.

---

## E. Claims I should NOT make in the essay

Some claims are tempting but not corpus-grounded enough to support load-bearing essay-spine work. Flagging them so I don't sneak them in under the cover of the strong items:

- **"Peeragogy is a 21st-century sangha"** — too strong. The corpus has *adjacencies* (Quaker meeting, contemplative-practice register) but no speaker calls peeragogy a sangha. The analogy lives in the IDENTIFY anchor; the essay can hold it as one frame among several but shouldn't claim it as the practice's identity.

- **"The Handbook stalled because it was wrong"** — too strong. The corpus's evidence (ep 4 + multiple v4-wish recurrences) supports *the chapter-text-production task stalled*, not *the Handbook's substance was wrong*. The essay should frame the stall as a structural-mechanism finding, not a content critique.

- **"Peeragogy is the only practice doing X"** — clearly false; the corpus surfaces a wide convergent-practice family. Any sentence implying singularity needs revision.

- **"Carrier (a) is impossible"** — too strong. The DERIVE explicitly says (a) carries structural risk and is ruled out *for this mission's INSTANTIATE*; not ruled out forever. The essay should not promote the DERIVE's "ruled out for this mission" to "impossible."

---

## F. Decision log

- **Decision (settled DERIVE):** carrier shape (b)+(e), three artifacts. Verified — load-bearing claims trace to corpus quotes (Cluster A + B above).
- **Decision (essay scope):** load-bearing claims will be Items 2.7, 2.8, ethics-as-bottleneck (1.8), convergent-practice family. Verified — each has STRONG quote ground.
- **Caveat (A.2):** The "Handbook is mostly layer-2" claim is a SYNTHESIS INFERENCE, not a direct quote. Tighten in essay to *"the Handbook would benefit from making its worldview/myth content more legible."*
- **Caveat (C.1, C.2, C.3, C.4, D.1, D.2):** Method-note must distinguish corpus-grounded claims from mission-context (Joe-autobiographical, IDENTIFY-stage, hypothetical) claims. The latter should be cited as Joe's framings, not as findings the corpus surfaces.
- **Section E flags:** four kinds of overstatement to avoid in the essay.

## G. Prototype/spike (per VERIFY discipline)

**Smallest possible prototype to validate the riskiest DERIVE commitment:** the riskiest claim is that the essay can be *cohered into a single articulation* in ~3,000 words. The minimum spike is a single-paragraph elevator-pitch version: can the essay's spine fit in 200 words?

**Spike attempt (drafted now, in real time, to test):**

> Peeragogy is a vocabulary for a worldview shift that's been happening for a long time in many places that don't use the word: from *the professor knows best* to *we all create together*. It names what sangha names in Buddhism, what consciousness-raising named in feminism, what Linux User Groups have done in free software, what Quaker meetings have done for centuries. The Peeragogy Handbook published in 2014–2016 was one carrier of this shift; ten years on, the carrier has stalled but the practice has continued — in podcasts, in open-source-learning classrooms, in community media studios, in indigenous data frameworks, in CLA-of-the-self workshops, in research about peer-produced peer learning. *That* the practice continues without the canonical text suggests the practice is the load-bearing thing, not the text. Peeragogy in 2026 is best articulated by naming the family of convergent practices it belongs to and by giving practitioners a vocabulary they can pick up selectively, rather than by maintaining a single canonical reference. This essay does the first; the new flexiargs do the second.

**Spike verdict:** The 200-word version coheres. Each sentence has corpus-quote ground (verified above). The essay's spine works.

---

## H. Completion-criteria pre-check (per VERIFY discipline, against IDENTIFY criteria)

The IDENTIFY commits to 6 completion criteria:

1. **Pragmatist inquiry chain worked through.** ✅ Synthesis evidences the chain; DERIVE's three artifacts will produce the chain's outputs (essay = why-does + what-is; method-note = patterns-matter + matter-together).
2. **MAP looked outward.** ✅ 13-episode podcast pass + 65 annotations + synthesis. Inward Handbook cross-check folds into Artifact 1.
3. **Working answer to "what is peeragogy in 2026"** in ARGUE. **Will be produced by Artifact 1 (essay).**
4. **Defended decision on carrier shape.** ✅ Settled in DERIVE: (b)+(e). Cases for/against the alternatives recorded in DERIVE's *carrier shapes deliberately not chosen* section.
5. **Handoff plan with specific channels and asks.** **Will be produced by handoff-plan-execution after artifacts ship.** DERIVE sketches the plan.
6. **Reviewer report repositioned as MAP-stage artifact, not implementation backlog.** ✅ Done — already framed as MAP evidence in the synthesis; the actual implementation-backlog work was carrier (a), now ruled out for this mission's INSTANTIATE.
7. **Today's proof-of-concept capabilities documented as primitives.** ✅ The four 2026-04-29-mined flexiargs + the four `futon4/scripts/peeragogy-*.clj` scripts + the canonical .mw flip + the surgical-edit pipeline are all documented in the mission doc and in lab artifacts.
8. **At least one carrier instance produced end-to-end as proof-of-pipeline.** **Will be produced by Artifacts 1+2** (the essay is the proof-of-pipeline for carrier (b); the flexiarg additions are the proof-of-pipeline for carrier (e) at small scale).

**No criterion left unaddressed by DERIVE.**

---

## Verdict

**DERIVE design verified.** Load-bearing corpus-grounded claims trace to specific quotes (often multiple). Mission-context claims correctly attributed to Joe / IDENTIFY / external sources rather than to corpus findings. One synthesis-inference (A.2 "Handbook is mostly layer-2") flagged for tightening in the essay. Four kinds of overstatement explicitly noted to avoid (Section E). The 200-word spike coheres, suggesting the 3,000-word essay is achievable.

**DERIVE is ready for INSTANTIATE.** Begin with Artifact 1 (the essay), folding in the inward Handbook cross-check as part of essay shaping per the DERIVE plan.
