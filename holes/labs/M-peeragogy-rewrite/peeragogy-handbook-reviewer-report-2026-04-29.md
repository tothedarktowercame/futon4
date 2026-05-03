# Peeragogy Handbook — Reviewer Report

**Date:** 2026-04-29
**Mission:** [M-peeragogy-rewrite](../../missions/M-peeragogy-rewrite.md)
**Source manifest:** `futon4/data/essays/wikibooks/peeragogy-handbook/peeragogy-handbook-book-manifest.el`
**Annotations sidecar:** `peeragogy-handbook-book-annotations.el` (88 annotations)
**Semilattice queue:** `peeragogy-handbook-revision-queue.edn` (32 clusters, 11 concerns)
**Pattern libraries used:** `futon3/library/{writing-coherence,peeragogy,collaboration-coherence,pattern-coherence}/`

---

## 1. Executive Summary

The Peeragogy Handbook (Wikibooks) is a 2014–2016 work that needs a *near-complete rewrite* rather than a refresh. The handbook's structural ambitions (a composing pattern language for peer learning) are still alive in the text; its specific tooling, links, and several pieces of imported source have not aged well. This report consolidates 88 standoff annotations across all 31 sections into a sequenced revision plan.

### 1.1 Major-severity findings (all source-fidelity catastrophes resolved as of 2026-04-29)

The 16 major-severity findings cluster into five distinct concerns. They are the *blockers and catastrophes* — the things that make individual chapters unreadable today, or that any 2026 reader would notice as embarrassing on first inspection. **All seven source-fidelity catastrophes are resolved as of 2026-04-29**: four were cleared by surgical wikitext edits to Wikibooks, and the remaining three turned out to be import-side artifacts (mojibake from Pandoc's lossy gfm-import) that simply do not exist in the live wikitext that Essays now treats as canonical (see Appendix A.2 on the round-trip discovery and the wikitext-canonical flip).

**Source-fidelity catastrophes** — the imported markdown was *not* a faithful representation of the source in several places. The Essays-side flip to wikitext-canonical (single source of truth, fetched directly from Wikibooks; no Pandoc round-trip) closes most of these without an edit:

- **Chapter 3 (Revised Intro)** — mojibake'd MediaWiki File embeds in the imported Markdown. **Resolved by canonical flip**: live wikitext has proper `[[File:PeeragogyV2 Cover Display.jpg|...]]` and `[[File:OERlogo.svg|...]]` constructs; the issue was Pandoc-side, not Wikibooks-side.
- **Chapter 4 (How to use this Handbook)** — V1.0 page is `#REDIRECT [[Peeragogy Handbook/How to use this Handbook]]`. **Resolved 2026-04-29** by adding `&redirects=1` to the corpus-fetch script; the local `.mw` cache now holds the redirect target's actual content (3066 bytes of Howard Rheingold prose).
- **Chapter 5 (Convening)** — corrupted image-embed fragment under "There will be a quiz" in the imported Markdown. **Resolved by canonical flip**: live wikitext has a proper `[[File:Rudyard Kipling from John Palmer.jpg|thumb|left|Engraving of Rudyard Kipling (1865-1936)...]]` File embed; the fragment was a Pandoc render failure on import.
- **Chapter 23 (Use Case)** — visible wiki vandalism in the source: keyboard-mash sequences (`jsdijs8dhj bnjdcujsj`, `KARTI K KUMAR AMAR BHAI LALTYUNC...`). **Closed 2026-04-29** (revids 4633110, 4633111).
- **Chapter 29 (Style Guide)** — vandalised opener (`that peeragogy handbook v1.o.really was a life saver.thank u.`). **Closed 2026-04-29** (revid 4633109).
- **Chapter 30 (Meet the Authors)** — raw MediaWiki table syntax (no longer a problem in the wikitext-canonical world; the table is intentional MediaWiki markup, not an import artifact), on top of every author having placeholder Affiliation "Former" and placeholder Quote "Welcomes any criticism or praise." **Partially closed 2026-04-29**: typo "Charlottee" → "Charlotte" (revid 4633112) and Jay Cross † marker added (revid 4633113). The placeholder author content is the only remaining surface-fidelity item — a content-collection task best handled via mailing-list ask.
- **Chapter 28 (Peeragogy in Action)** — leading "W" lost at the start of the opening sentence ("e have been writing the missing manual..."). **Closed 2026-04-29** (revid 4633108).

**Platform-currency catastrophes** — chapters whose tooling recommendations are heavily decayed. None of these have been addressed yet — they need editorial work and (per Appendix A.2) a wikitext-native editing path:

- **Chapter 6 (K-12 Peeragogy)** — anchored to August 2012; Edmodo defunct, Twitter→X migration, Classroom 2.0 quiet, Ning paid-only.
- **Chapter 11 (How to Organize a MOOC)** — Posterous, Google Reader, Tumblr-as-blogging-tool all aged out; Skype individual discontinued 2025.
- **Chapter 13 (The Workscape)** — sixteen-item bullet list of consumer-app analogues including Digg, Delicious, MetaFilter, Fark; reads as parody to a 2026 reader.
- **Chapter 24 (Peeragogies Technology)** — Google+ Hangouts (defunct 2019), Pearl Trees, Blackboard Collaborate (rebranded), Adobe Connect.
- **Chapter 26 (Real-time Meetings)** — Google+ Hangout is the *primary free-tier* recommendation, with a dedicated configuration subsection.

**Link rot** at scale. **Currency** of placeholder content (chapter 30 — partially addressed by today's † marker; affiliations + quotes still pending). **Posthumous-author** treatment (Jay Cross died 2015, still cited in present tense in chapter 18 and now marked with † in chapter 30 as of 2026-04-29; in-memoriam date footnote still pending per `C-posthumous-author` policy).

### 1.2 Recommended overall revision strategy

The semilattice queue resolves these into a sequenced strategy. Three observations shape the sequence:

1. **Tier-0 work is cheap to settle but blocks much else.** Four policy decisions (link-rot, platform-currency, posthumous-author, temporal-layering) unblock most chapter rewrites. Settle these first; everything downstream becomes mechanical.
2. **The pattern-cross-reference editorial pass is the highest-yield single move.** Combining `C4-pattern-cross-reference-sweep` (24 peeragogy-axis annotations) with `C-collaboration-coherence-sweep` (11 collaboration-coherence-axis annotations) is one editorial pass that resolves 35 annotations, makes the book's pattern languages visible *as languages* across 14 chapters, and costs one day of focused work.
3. **Chapter-level rewrites should be ordered by structural payoff, not severity.** The chapters with the most ambitious structural finding (chapter 24's full feature-axis rewrite, chapter 26's 2026-current real-time-meetings, chapter 11's RSS-aggregation-architecture-preserved-with-new-tools) deliver the most visible improvement per unit of work. Chapter 30's rewrite is mechanical content collection. Chapters 22 and 29 reposition the book's reflexive layer.

The recommended order:

```
Tier 0 — settle policies (1–2 days, sequencing decisions only)
   ├─ C2a-link-rot policy (sweep is mechanical once policy is set)
   ├─ C-platform-currency policy (feature axes + dated exemplars recommended)
   ├─ C-posthumous-author convention
   └─ C-temporal-layering convention
        │
Tier 1 — cross-cutting editorial sweeps (1–2 weeks)
   ├─ C4 + C-collaboration-coherence-sweep — one combined pass over 14 chapters
   ├─ C5 (meta-lede openers, 6 chapters)
   ├─ C6 / C7 / C9 / C10 (sentence-level tightening passes)
   ├─ C-heading-consistency-pass
   └─ C-preserve-positive-findings (discipline, applied throughout)
        │
Tier 2 — chapter rewrites (4–8 weeks)
   ├─ C8 chapter 24 (proof-of-pipeline; biggest structural payoff)
   ├─ C-chapter-26 (real-time meetings; clearest currency rewrite)
   ├─ C-chapter-13 (workscape; feature-primitive rewrite)
   ├─ C-chapter-11 (MOOC; preserve cMOOC architecture)
   ├─ C-chapter-6 (K-12; near-rewrite around current platforms)
   ├─ C-chapter-22 (Antipatterns; reformat to flexiarg shape)
   ├─ C-style-guide-rewrite (chapter 29; make it self-exhibit)
   ├─ C2 / C2b / C3 (intro / how-to / convening; rewrite from scratch)
   └─ C-meet-authors-rewrite (chapter 30; mostly content collection)
        │
Tier 3 — remaining mining (parallel, low-priority)
   ├─ Mine PAR into a flexiarg
   ├─ Mine meta-application (candidate meta-pattern)
   └─ Decide fate of dangling use-case pattern candidates
```

### 1.3 Preservation discipline (do not lose these in rewrite)

Round-2 and round-3 sweeps surfaced **seven passages where the book exhibits the pattern language at its best**. These should be *preserved and amplified*, not edited:

- **Chapter 9 (Adding Structure)** — "scarcity aware" design = the Stasis pattern's positive form. *Find the moving dimension* enacted.
- **Chapter 14 (Co-Facilitation)** — Paragogical Action Review's five steps end with a defined feedback action surface = the Navel-Gazing antipattern's positive form.
- **Chapter 17 (Peeragogical Assessment)** — explicit meta-application: applies its own assessment frame to the Handbook itself.
- **Chapter 20 (Patterns and Heuristics)** — the strongest single exhibit of the pattern language property in the entire book; cross-referenced patterns composing in a worked argument.
- **Chapter 26 (Real-time Meetings)** — explicit weak-to-strong tie conversion through synchronous media; named operationally.
- **Chapter 26 (Real-time Meetings) again** — seven named roles for live meetings, defined operationally with explicit role-shifting framing.
- **Chapter 28 (Peeragogy in Action)** — Wrapper role named explicitly, with reflective tone and empirical confirmation that it was load-bearing.

### 1.4 What the four annotation axes catch

| Axis | Marker | Catches |
|---|---|---|
| `annotation/writing-coherence` | 🤖 | Sentence-level / paragraph-level prose failures: meta-lede, scope-mush, hedged-lift, throat-clearing-close, floating-formalism, missing-mechanism, heading-inconsistency, list-without-frame |
| `annotation/critique` | 🔍 | Non-pattern-backed concerns: link rot, platform currency, importer artefacts, vandalism, placeholder content, posthumous-author, temporal-layering |
| `annotation/peeragogy` | 🗪 | Places where the book touches its own pattern language without naming it (positive: "this enacts pattern X"; negative: "this should cross-reference X but doesn't") |
| `annotation/collaboration-coherence` | 🗫 | Places where the antipatterns chapter's content (now mined into the new library) applies to the rest of the book — both as failure-mode detection and as preservation of positive forms |

---

## 2. Tier-0 Blockers — Settle First

### 2.1 Link-rot policy (`C2a-link-rot-sweep`)

Every chapter has at least one decayed link. Five annotations explicitly track it; many more would surface in a sweep. The decision is a single editorial choice that then makes the sweep mechanical.

**Options:**
- (a) Replace each with a 2026-current resource where one exists.
- (b) Internet Archive snapshot dated to original publication.
- (c) Drop the link, keep the text.
- (d) Case-by-case per chapter.

**Recommendation:** (a) where a current resource exists; (b) where the original resource is historically important; (c) where the link added little. Apply by hand during chapter rewrites rather than as a separate sweep — the per-link decision is editorial and hard to mechanize.

### 2.2 Platform-currency policy (`C-platform-currency-sweep`)

Ten chapters have specific platform recommendations that have aged out. Resolves around a structural choice:

**Options:**
- (a) Feature primitives only — describe what a tool needs to do without naming products.
- (b) Feature primitives + dated 2026-current exemplars — name products with explicit "as of 2026:" markers so future decay is visible.
- (c) Living-stack snapshot updated annually.

**Recommendation:** (b). Feature primitives carry the durable argument; dated exemplars make the recommendation usable without committing the book to perpetual currency.

### 2.3 Posthumous-author convention (`C-posthumous-author-convention`)

Jay Cross (died 2015) is cited in present tense in chapter 18 and listed without marker in chapter 30. Likely other contributors need posthumous treatment by 2026.

**Options:**
- (a) In-memoriam marker per author with date.
- (b) Date-only marker (date of death after the name).
- (c) Removal-with-acknowledgement (drop from author list, retain credit elsewhere).

**Recommendation:** (a) — keeps the author in the book with appropriate dignity; date marker makes the temporal context legible.

### 2.4 Temporal-layering convention (`C-temporal-layering-convention`)

Chapter 13 has a 2026-02-07 inserted edit ("Flaws of the Smart Workplace") sitting next to 2014 main text. Chapter 17 ends "alpha testing phase (which is beginning now!)" — true in 2014, historical now.

**Options:**
- (a) Absorb edits into main text and date the chapter as a whole.
- (b) Preserve historical block + companion 2026 section, with explicit dating.
- (c) Per-paragraph dating where layers mix.

**Recommendation:** (a) for most chapters — the rewrite is the editorial moment. (b) for chapters where the historical record matters (the Patterns chapter; the Authors chapter).

---

## 3. Cross-Cutting Findings — Editorial Sweeps

### 3.1 Pattern cross-reference sweep (combined `C4` + `C-collaboration-coherence-sweep`)

The book defines a pattern language in chapter 21 and an antipatterns chapter in chapter 22. **Across 14 other chapters, the prose touches one of these patterns without naming it.** Naming the pattern, with a cross-reference, is a one-line edit per occurrence. Combined, this is **35 annotations resolvable in a single editorial pass**.

This is the cheapest, highest-yield move in the whole rewrite. Doing this *before* chapter rewrites also stabilises the cross-references for later chapter-rewrite work, so the chapter rewrites can focus on substance rather than cross-reference plumbing.

Affected chapters (peeragogy axis, then collaboration-coherence axis):

- 1 (preface — wrapper role enacted), 3 (intro — newcomer pattern in active use), 5 (convening — polling-for-ideas implicit, heartbeat scare-quoted), 9 (roles decomposition; discerning-a-pattern via "why is this hard?"; stasis as scarcity-aware design), 10 (polling-explicit; roadmap as charter), 11 (Daily Newsletter as wrapper; gRSShopper as use-or-make; messy-with-lurkers re completion), 12 (carrying-capacity guidelines; newcomer section; long-tail explicit), 13 (activity-stream as wrapper; magical-thinking as feature-fantasy), 14 (roles in role list; moderation as rhythm-mismatch; PAR as navel-gazing positive form), 17 (meta-application; navel-gazing in feedback dynamics), 18 (use-or-make in corporate frame; magical-thinking in ROI), 21 (pattern-language self-reference; isolation-of-pattern-entries), 22 (source-self-reference), 24 (use-or-make as tool selection; magical-thinking as too-many-tools), 26 (PAR as meta-pattern candidate; tie-conversion exhibited).

### 3.2 Writing-coherence sweeps (sentence-level)

Six smaller passes; each is half a day or less:

- `C5-meta-lede-opener-rewrite` — six chapters with section openers that announce structure rather than make a claim. Mechanical rewrite.
- `C6-throat-clearing-close-rewrite` — two chapters with closes that end on authorial posture rather than a claim. Mechanical.
- `C7-scope-mush-list-rewrite` — five chapters with multi-promise lists. Pick a governing axis per list; demote or split the rest.
- `C9-hedged-lift-tightening` — chapter 21's sentence about Pattern Language. Single sentence.
- `C10-floating-formalism-integration` — chapters 14 and 17. Either weave the framework through the chapter or drop it.
- `C-heading-consistency-pass` — chapters 6, 22, 23 have heading inconsistencies (Phase/Stage; uneven antipattern subheadings; MediaWiki `=H=` syntax leak).

### 3.3 Other cross-cutting items

- `C-edition-dating-convention` — chapter 1's "two years ago" → absolute dates; convention for future editions.
- `C-missing-mechanism-fix` — chapter 18's ROI ratio needs a worked example or removal.

---

## 4. Chapter-by-Chapter

For each section: status, annotation count by axis, key findings, recommended action. Annotations marked `(P)` are `:preserve` (do not lose).

### 4.0 Overview — `1` annotation

- 🤖 throat-clearing-close. Closes on hope/invitation rather than claim.
- **Action:** Sentence-level rewrite via `C6`. End on the strongest claim the chapter makes.

### 4.1 Preface to the 3rd Edition — `2` annotations

- 🔍 relative-time references ("two years ago" without absolute date).
- 🗪 wrapper-role enacted but unnamed.
- **Action:** `C-edition-dating-convention` for absolute dates; `C4` cross-reference to peeragogy/wrapper. Preserve the dedication to George Brett.

### 4.2 Foreword — `1` annotation

- 🤖 throat-clearing-close.
- **Action:** Sentence-level rewrite via `C6`. End on the structural claim about the handbook's role; move invitation one sentence earlier.

### 4.3 Revised Intro — `3` annotations

- 🔍 ~~*(major)* mojibake'd File embeds throughout.~~ **Resolved 2026-04-29 by canonical flip**: live wikitext has proper `[[File:PeeragogyV2 Cover Display.jpg|...]]` and `[[File:OERlogo.svg|...]]`; the issue was Pandoc-import-side, not Wikibooks-side.
- 🔍 *(major)* link rot to socialmediaclassroom.com / peeragogy.org.
- 🗪 newcomer pattern actively in use but unnamed.
- **Action:** `C2-rewrite-intro`. Now blocks only on `C2a-link-rot-sweep` (the `C1-importer-fix` block is removed for this chapter). Rewrite from scratch using the Newcomer pattern as the framing device.

### 4.4 How to use this Handbook — `1` annotation

- 🔍 ~~*(major)* chapter is empty (only `1. REDIRECT` line).~~ **Resolved 2026-04-29**: corpus-fetch script now follows MediaWiki redirects; the local `.mw` cache holds the redirect target's actual content (3066 bytes of Howard Rheingold prose). Open editorial question: should the V1.0 page hold canonical content directly (push the redirect-target prose to the V1.0 page) or stay as a redirect?
- **Action:** `C2b-rewrite-how-to-use` is now optional — the chapter is no longer empty in Essays. Decide whether to rewrite for V1.0 voice or keep the redirect. If keeping the redirect, the page-title in the manifest could be updated to point at the canonical target.

### 4.5 Convening — `4` annotations

- 🔍 ~~*(major)* corrupted image-embed fragment under "There will be a quiz."~~ **Resolved 2026-04-29 by canonical flip**: live wikitext has a proper `[[File:Rudyard Kipling from John Palmer.jpg|thumb|left|Engraving of Rudyard Kipling (1865-1936)...]]` File embed; the fragment was a Pandoc render failure on import.
- 🗪 polling-for-ideas implicit (chapter is structurally a meta-poll for project setup).
- 🗪 heartbeat scare-quoted; should cross-reference the pattern.
- 🔍 platform list (Big Blue Button, Adobe Connect, Blackboard Collaborate) is 2014-vintage.
- **Action:** `C3-rewrite-convening`. Now blocks only on `C-platform-currency-sweep` and `C2a` (the `C1` block is removed for this chapter). Frame chapter as polling-for-ideas applied to project setup; cross-reference Heartbeat, Roles, Newcomer where they surface.

### 4.6 K-12 Peeragogy — `3` annotations *(major platform-currency)*

- 🔍 *(major)* dated to August 2012 by direct self-reference; entire platform mix decayed.
- 🗪 newcomer arc (Lurking → Entering → PLN → Face-to-face) implicit but unnamed.
- 🤖 phase/stage numbering inconsistent ("Phase 4" then "Stage 5").
- **Action:** `C-chapter-6-k12-rewrite`. Near-complete rewrite around durable connected-educator practices + 2026 platforms. Frame around the Newcomer pattern.

### 4.7 Researching Peeragogy — `1` annotation

- 🤖 meta-lede opener.
- **Action:** Sentence rewrite via `C5`. Open with the research problem itself.

### 4.8 Organizing Co-Learning — `1` annotation

- 🤖 meta-lede opener.
- **Action:** Sentence rewrite via `C5`. Open on the contextual-inheritance claim.

### 4.9 Adding Structure — `4` annotations

- 🔍 link rot to peeragogy.org/* internal references.
- 🗪 roles decomposition (TA/student/organiser) — strong unmarked Roles instance.
- 🗪 "why is this hard?" as discerning-a-pattern move.
- 🗫 "scarcity aware" design = stasis pattern's positive form. **(P)**
- **Action:** Cross-reference Roles and Discerning-a-Pattern via `C4`; cross-reference Stasis (preserve) via `C-collaboration-coherence-sweep`. Link rot per `C2a` policy.

### 4.10 The Student authored syllabus — `3` annotations

- 🗪 explicit "polled for additions" — direct polling-for-ideas instance.
- 🔍 platform references (Twitter, Google+, Facebook) need 2026 update.
- 🗪 team-charter is roadmap pattern applied to norms.
- **Action:** Cross-reference Polling-for-Ideas and Roadmap; rewrite platform list via `C-platform-currency-sweep` policy.

### 4.11 How to Organize a MOOC — `5` annotations *(major platform-currency)*

- 🔍 *(major)* tooling list heavily decayed (Posterous, Tumblr, Diigo, Delicious, Skype, Google Reader).
- 🗪 Daily Newsletter = canonical wrapper at MOOC scale.
- 🗪 gRSShopper = use-or-make decision (custom build with available alternatives).
- 🤖 "one might wonder why a course would want to be 'massive'" — meta-lede.
- 🗫 messy-with-lurkers in MOOC participation gradient.
- **Action:** `C-chapter-11-mooc-rewrite`. Preserve cMOOC vs xMOOC distinction and RSS-aggregation architecture. Replace tooling list. Frame around Wrapper and Use-or-Make patterns.

### 4.12 Participation — `4` annotations

- 🗪 carrying-capacity guidelines (first three bullets) — pattern in use.
- 🗪 explicit "newcomer section" recommendation — direct Newcomer pattern.
- 🤖 nine-bullet list with no organising frame.
- 🗫 explicit Long Tail / 90-9-1 reference — partial respect for power-law shape, but still aspires to "break" it.
- **Action:** Cross-reference Carrying-Capacity, Newcomer; reframe list around named patterns; resolve aspiration/structural-respect tension at the long-tail mention.

### 4.13 The Workscape — `5` annotations *(major platform-currency)*

- 🔍 *(major)* sixteen-item bullet list of consumer-app analogues; multiple defunct platforms.
- 🤖 same list also fails as scope-mush.
- 🗪 activity-stream as wrapper-pattern instance at corporate scale.
- 🔍 mixed temporal layers (2026-02-07 edit alongside 2014 main text).
- 🗫 magical-thinking in implicit "you can implement at reasonable cost" claim.
- **Action:** `C-chapter-13-workscape-rewrite`. Block on `C-platform-currency-sweep` and `C-temporal-layering-convention`. Replace consumer-app list with feature primitives + dated exemplars.

### 4.14 Co-Facilitation — `4` annotations

- 🗪 roles named operationally (moderator, technical recorder, note-taker) — Roles pattern instance.
- 🗪 "different rhythms of intervention" — Moderation pattern in one line.
- 🤖 Heron quote (hierarchical/cooperative/autonomous) is floating-formalism — quoted but not integrated.
- 🗫 PAR's five steps end with a defined feedback action surface = Navel-Gazing pattern's positive form. **(P)**
- **Action:** Cross-reference Roles, Moderation, Navel-Gazing (preserve); decide whether to weave the Heron frame through the chapter (`C10`) or drop it.

### 4.15 Designs for Co-Working — `1` annotation

- 🤖 meta-lede opener.
- **Action:** Sentence rewrite via `C5`. Open on the coordination problem itself.

### 4.16 Platform Design — `1` annotation

- 🤖 meta-lede opener.
- **Action:** Sentence rewrite via `C5`. Lead with the categorisation claim, not the article-overview.

### 4.17 Peeragogical Assessment — `4` annotations

- 🗪 explicit meta-application: applies own assessment frame to the Handbook. **(P)**
- 🤖 floating-taxonomy at end (UNIT OF ANALYSIS / Purpose / Feedback source / Models / Other considerations) — unintegrated bare lists.
- 🔍 "alpha testing phase (which is beginning now!)" — historically dated.
- 🗫 navel-gazing applicability: "if a participant is not learning, who is the question for?" raised but not given an action surface.
- **Action:** Preserve the meta-application framing (`C-preserve`); integrate the floating taxonomy or move to appendix (`C10`); update alpha-testing language; cross-reference Navel-Gazing and define the action surface.

### 4.18 Following the money — `4` annotations

- 🔍 Jay Cross posthumous (deceased 2015, present-tense citation, Vimeo links).
- 🤖 ROI ratio with no operational mechanism (missing-mechanism).
- 🗪 use-or-make tension implicit in corporate-vs-paragogical-praxis frames.
- 🗫 magical-thinking in implicit ROI-formula-as-substitute-for-method.
- **Action:** `C-posthumous-author-convention` for Jay Cross; `C-missing-mechanism-fix` for ROI; cross-reference Use-or-Make and Magical-Thinking; reframe corporate-ROI as one frame among several.

### 4.19 Thinking about patterns — `3` annotations

- 🔍 missing diagram (chapter promises one but none rendered in import).
- 🔍 Google+ Hangout reference (defunct).
- 🤖 thirteen patterns + seven antipatterns + sixteen use cases listed serially without composition — pattern-list-no-composition.
- **Action:** Restore or redraw diagram (`C1` / source-fidelity); update platform reference; reorganise lists to expose composition (per `C4` direction). The next chapter (Patterns and Heuristics) is the model.

### 4.20 Patterns and Heuristics — `2` annotations

- 🗪 strongest exhibit of pattern-language property in the entire book — multiple patterns composed in worked argument. **(P)**
- 🔍 link rot to peeragogy.org/patterns/* and Minsky OLPC URLs.
- **Action:** **Preserve as the model** for what the Patterns chapter (chapter 21) should look like. Update internal pattern links to resolve to the rewritten edition's chapters. The Minsky-OLPC mapping is itself worth re-elevating in the rewrite.

### 4.21 Patterns — `3` annotations

- 🤖 hedged-lift sentence about pattern language (Codex's seed pass).
- 🗪 chapter asserts pattern-language property without exhibiting it (claim, not demonstration).
- 🗫 patterns presented in isolation — meta-level isolation antipattern.
- **Action:** Tighten the hedged sentence (`C9`); add cross-references to each pattern entry so the chapter exhibits the language property it claims (`C4` + the meta-isolation finding); model after chapter 20.

### 4.22 Antipatterns — `5` annotations

- 🔍 Stasis section anchored to a specific 2013 SMC dev bottleneck.
- 🔍 socialmediaclassroom.com forum links across the chapter.
- 🗪 chapter is the source for the new collaboration-coherence library (mining target — done).
- 🤖 antipattern subheadings inconsistent in shape.
- 🗫 source-self-reference annotation marks provenance.
- **Action:** `C-chapter-22-antipatterns-rewrite`. Three options: (a) stay as-is and link to library; (b) reformat each entry as a flexiarg-shape block (TENSION/COMPOSITIONS/FAILURE-MODES); (c) replace with short narrative + library pointer. Recommendation: (b), most pattern-language-aligned.

### 4.23 Use Case — `3` annotations *(major source-fidelity: vandalism)*

- 🔍 ~~*(major)* visible wiki vandalism in source (`jsdijs8dhj bnjdcujsj`, `KARTI K KUMAR AMAR BHAI LALTYUNC...`).~~ **CLOSED 2026-04-29** (revids 4633110, 4633111).
- 🤖 MediaWiki heading syntax (`= Improved adaptivity =`) leaked through importer.
- 🗪 multiple use cases close with "Possible patterns to extract?" — dangling pattern candidates that never closed.
- **Action:** Importer heading-handling fix under `C1`; decide fate of pattern candidates under `C-mining-use-case-candidates` (mine / keep / drop per case).

### 4.24 Peeragogies Technology — `5` annotations *(major platform-currency)*

- 🤖 meta-lede opener (Codex's seed pass).
- 🔍 *(major)* heaviest tooling decay in the book (Google+ Hangouts, Pearl Trees, Blackboard Collaborate, Adobe Connect, Mumble).
- 🗪 use-or-make tension circled but unnamed.
- 🤖 five-axis taxonomy (Time/Place, Stages, Bloom's, Use Cases, Learning Functions) presented as parallel without composition rules.
- 🗫 magical-thinking in retrospective ("too many tools spoil the broth") — symptom named without dynamic.
- **Action:** `C8-chapter-24-full-rewrite`. **The proof-of-pipeline candidate.** Block on `C-platform-currency-sweep`. Frame around Use-or-Make pattern; durable feature axes; single primary organising axis (Use Cases is the most plausible).

### 4.25 Wiki — `1` annotation

- 🤖 scope-mush opener (four promises bundled).
- **Action:** Sentence rewrite via `C7`. Pick one governing promise.

### 4.26 Real-time Meetings — `4` annotations *(major platform-currency)*

- 🔍 *(major)* Google+ Hangout is *primary free-tier* recommendation, with dedicated config subsection.
- 🗪 seven roles for live meetings, defined operationally — strong Roles pattern instance. **(P)**
- 🗪 Paragogical Action Review as candidate flexiarg in its own right.
- 🗫 weak-to-strong tie conversion through synchronous media — explicit and operational. **(P)**
- **Action:** `C-chapter-26-rewrite`. Block on `C-platform-currency-sweep`. Replace Google+ Hangout with Jitsi / Big Blue Button / Mumble / Discord / Matrix. **Preserve role taxonomy and tie-conversion paragraph** — both are pattern-language exemplars.

### 4.27 How to get involved — `1` annotation

- 🤖 scope-mush opener.
- **Action:** Sentence rewrite via `C7`. Introduce one organising distinction before the contribution list.

### 4.28 Peeragogy in Action — `3` annotations

- 🔍 ~~truncated opening ("e have been writing..." — leading W lost in import).~~ **CLOSED 2026-04-29** (revid 4633108) — fixed at the source on Wikibooks; the import truncation was actually a long-stale Wikibooks-side typo, not an importer bug.
- 🔍 broken image references (`Image:http://metameso.org/~joe/OpenBook-2-N.jpg` rendered as text).
- 🗪 explicit Wrapper role naming with reflective tone — empirical confirmation. **(P)**
- **Action:** Image-reference fix needs wikitext-native edit (approach (b)); **preserve and amplify the Wrapper observation** (`C-preserve`).

### 4.29 Style Guide — `3` annotations *(major source-fidelity: vandalism)*

- 🔍 ~~*(major)* vandalised opener (`that peeragogy handbook v1.o.really was a life saver.thank u.`).~~ **CLOSED 2026-04-29** (revid 4633109).
- 🤖 chapter violates its own rules (hedged-lift in "don't overdo bullets" subsection, then has bullet list).
- 🗪 chapter is itself a Creating-a-Guide instance applied recursively to the project's writing.
- **Action:** `C-style-guide-rewrite`. **Rewrite each guideline so it exhibits the rule it states.** Reframe chapter as the project's reflexive Creating-a-Guide instance.

### 4.30 Meet the Authors — `3` annotations *(major source-fidelity + placeholder)*

- 🔍 *(major)* MediaWiki table syntax not converted to Markdown. (Importer-side; live wikitext on Wikibooks is fine.)
- 🔍 *(major)* placeholder content: every author has Affiliation "Former" and Quote "Welcomes any criticism or praise." Only Fabrizio Terzi's row carries real data.
- 🔍 ~~typo "Charlottee Pierce";~~ **CLOSED 2026-04-29** (revid 4633112). ~~Jay Cross deceased 2015 with no marker.~~ **PARTIALLY CLOSED 2026-04-29** (revid 4633113 — added † marker; in-memoriam date footnote still pending per `C-posthumous-author-convention` option (a)).
- **Action:** `C-meet-authors-rewrite`. Block on `C1` (Markdown-side table conversion) and `C-posthumous-author-convention` (date footnote). Collect actual author data, or replace the table with prose-based contributor acknowledgement. Mailing-list call for affiliations + quotes is the cheapest path here.

---

## 5. Preservation Digest

Seven passages where the book exhibits its pattern languages cleanly. The rewrite must keep all of these (with possible amplification) — this is the `C-preserve-positive-findings` discipline.

| Chapter | Annotation | Why preserve |
|---|---|---|
| 9 Adding Structure | `cc:adding-structure-find-moving-dimension` | "Scarcity-aware design" = Stasis pattern's positive form, named in the chapter's own language |
| 14 Co-Facilitation | `cc:co-facilitation-par-action-surface` | PAR's five steps = Navel-Gazing antipattern's positive form (feedback that closes with action) |
| 17 Peeragogical Assessment | `pg:assessment-discerning-self-reference` | Explicit meta-application: chapter applies its own assessment frame to the Handbook |
| 20 Patterns and Heuristics | `pg:patterns-heuristics-language-exhibited` | Strongest exhibit of pattern language property in the book; the model for chapter 21 |
| 26 Real-time Meetings | `pg:realtime-meetings-roles-textbook` | Seven named roles defined operationally with role-shifting framing |
| 26 Real-time Meetings | `cc:realtime-meetings-tie-conversion` | Weak-to-strong tie conversion through synchronous media, named operationally |
| 28 Peeragogy in Action | `pg:in-action-wrapper-explicit` | Wrapper role named explicitly with reflective tone |

---

## 6. Mining Queue (Remaining)

Three smaller mining tasks, parallel to other work:

- **`C-mining-PAR`** — Paragogical Action Review (cited in chapters 14 and 26) is a named, structured peer-learning practice with five steps and a cited theoretical lineage (Argyris double-loop). Candidate for a flexiarg in `futon3/library/peeragogy/` or a standalone collaboration-coherence-shape entry. Decision: peeragogy if the rewrite promotes PAR to the Patterns chapter; collaboration-coherence if PAR stays in operational chapters.
- **`C-mining-meta-application`** — Three chapters (17, 28, 29) exhibit reflexive self-application: assessment applied to the Handbook, the Handbook documenting itself in action, the Style Guide as a guide to writing the Handbook. Candidate for a `peeragogy/meta-application` flexiarg or addition to existing `peeragogy/discerning-a-pattern`.
- **`C-mining-use-case-candidates`** — Chapter 23's use cases include candidate pattern names that were never mined: Seeding Peer Communities, Emergent Standards, Emergent Assessment, Environment Scanning, Codifying Specialist Knowledge, Extracting Themes, Modelling Outcomes, Consensus Building. Each needs a fate decision.

---

## 7. How to Use This Report

- The report is a *narrative rendering* of the queue. The queue (`peeragogy-handbook-revision-queue.edn`) is the source of truth; it can be queried by chapter, by concern, by cluster, by axis.
- Annotations in the sidecar (`peeragogy-handbook-book-annotations.el`) are the source of truth for individual findings. Each is loaded into the live Arxana Essays browser with a marker (🤖 / 🔍 / 🗪 / 🗫) at its passage; clicking the marker shows the full diagnosis and suggested move.
- For each chapter rewrite, the procedure is:
  1. Open the chapter in the Essays browser.
  2. Read the markers in document order.
  3. Decide on the rewrite direction (the `:rationale` field of the relevant cluster in the queue is the guidance).
  4. Apply the rewrite. As findings close, mark the annotation `:closed` in the sidecar.
  5. Re-run the integrity check (`clojure -M -e ...` against the queue) to confirm consistency.
- New findings during rewrite become new annotations. Round 4 (next iteration) would integrate them and revise the queue.

---

## 8. Caveats

- **The queue is a seed of a living document.** New findings arrive with each pass; cluster boundaries will shift; some preservation items may be re-examined.
- **Pattern libraries are also under revision.** The 7 collaboration-coherence flexiargs were mined from chapter 22; using them on the rest of the book may surface refinements (the patterns may need PURs once exercised).
- **The report does not adjudicate the book's claims.** Annotations flag structural problems and currency problems; they do not review whether the book's substantive claims about peer learning are correct. That is a separate exercise.
- **Round 3 surfaced one significant theoretical decision (antipatterns as separate library on writing-coherence shape, not folded into peeragogy).** Future similar decisions may surface; the queue is the place where they get recorded.

---

## Appendix A. 2026-04-29 closures and round-trip discovery

A record of the surgical edits landed during the report's first day, and
the technical discovery that shaped the rewrite plan's editing approach.

### A.1 Surgical edits landed

Six surgical edits were pushed to Wikibooks via the
`Arided@futon-peeragogy` bot, clearing the source-fidelity items
addressable without a re-import. Each edit was a literal find-and-replace
against live wikitext (preserving File embeds, interwiki links,
templates), pushed with a basetimestamp conflict-detection guard.

| Page | Edit | revid |
|---|---|---|
| Peeragogy in Action (ch 28) | Restore leading "W" in opening sentence | 4633108 |
| Style Guide (ch 29) | Remove vandalised opener line | 4633109 |
| Use Case (ch 23) | Remove `jsdijs8dhj bnjdcujsj` keyboard-mash | 4633110 |
| Use Case (ch 23) | Remove broken `{{BookCatKARTI K KUMAR ...}}` prefix | 4633111 |
| Meet the Authors (ch 30) | Fix typo: Charlottee → Charlotte | 4633112 |
| Meet the Authors (ch 30) | Mark Jay Cross deceased with † | 4633113 |

These closures are reflected inline in the per-chapter sections (§§4.23,
4.28, 4.29, 4.30) — strikethrough text with revid annotation marks each
finding that landed on Wikibooks today.

The handbook is now visibly moving on Wikibooks: basetimestamps on ch 28
and ch 23 had been frozen at 2013 and 2024 respectively before today;
both now carry 2026-04-29 edits. This is the signal that precedes the
mailing-list handoff.

### A.2 Round-trip limitation discovered today

A test of the full Markdown → wikitext path through Pandoc revealed that
the round-trip is **lossy in both directions**:

- Wikibooks → Markdown (the original Codex import): drops File embeds
  (`[[Image:...]]`), interwiki links (`[[w:...]]`), templates
  (`{{...}}`), user signatures (`~~~~`), and MediaWiki table syntax.
- Markdown → Wikibooks (publishing back): Pandoc's gfm→mediawiki
  conversion does not restore lost wikitext-native constructs, and adds
  cosmetic noise (e.g. `<span id="...">` anchor tags) that needs
  post-processing.

**Consequence for the rewrite plan:** the original mental model — "edit
chapter Markdown locally, push back via the bot" — is not safe for
chapters where the live wikitext contains File embeds, interwiki links,
or templates. For those chapters the editing surface must be either:

- (a) **Surgical**: small literal find/replace at the wikitext level
  (proven today; what the closure table above used).
- (b) **Wikitext-native**: edit the live wikitext directly (with the bot
  or by hand), bypassing the Markdown layer.
- (c) **Round-trip-safe**: improve the importer to preserve the lossy
  constructs, *and* improve the publish path to round-trip them
  faithfully.

Approach (a) handles the catastrophes that are point-defects in
otherwise intact wikitext (vandalism, single-character typos, posthumous
markers). Approach (b) is the right path for the platform-currency
rewrites and the ch 3/4/5 substantive work. Approach (c) is the
long-term infrastructure investment.

### A.3 Tier-0 reframe in light of the discovery

The original Tier 0 (per §2) was framed as four policy decisions plus an
importer fix (`C1-importer-fix`). The today-confirmed reframe:

- **`C1-importer-fix` is descoped.** The Essays-side flip to
  wikitext-canonical (single source of truth, fetched directly from
  Wikibooks; corpus-fetch script follows MediaWiki redirects) removes
  the importer from the critical path entirely. Wikitext is read
  directly; nothing is converted; nothing round-trips. Whatever
  importer-side handlers a future re-import would need is now an
  internal operator concern, not a blocker on chapter rewrites.
- **C2a / C-platform-currency / C-posthumous-author /
  C-temporal-layering** remain as policy decisions; today's surgical
  edits exercised C-posthumous-author *informally* by adding the †
  marker per option (a) of §2.3 (in-memoriam-style date marker deferred
  to a footnote pass).
