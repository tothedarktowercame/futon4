**Status:** IDENTIFY-redux (revised 2026-04-29). Prior INSTANTIATE-track is suspended pending re-IDENTIFY. The proof-of-concept capabilities landed today (canonical .mw flip, surgical wikitext edits via bot, annotation migration) reframed as feasibility evidence rather than the start of execution. Mission name retained but the mission-shape is up for revision: rewrite vs. refactor vs. zine-decomposition vs. hybrid.

**Reframe summary:** today's surgical edits and the .mw-canonical flip showed that we *can* edit the Handbook via Arxana / Claude Code / wikibot. But the act of building that capability surfaced a question the prior IDENTIFY didn't engage: *what is the right thing to build, given that we now can build it?* The prior IDENTIFY (preserved as §1.prior, below) assumed "near-complete rewrite" was the answer. Reframe makes that one option among several.

# M-peeragogy-rewrite: scope, shape, and carriers for an updated peeragogy

## 1. IDENTIFY (revised 2026-04-29 — DRAFT for Joe review)

### Motivation

The Peeragogy Handbook (Wikibooks, 2014–2016) is a 31-chapter book on peer learning that has aged unevenly. Today's proof-of-concept work demonstrated that we *can* edit it — surgical wikitext edits via a bot (six landed, revids 4633108–4633113), canonical Essays-side flip to wikitext (single source of truth, no Pandoc round-trip), annotation migration to wikitext anchors (77/88 auto-anchored), all working. The act of building that capability raised a question chain that the prior IDENTIFY (and the reviewer report it produced) does not engage with — and the chain is the right shape for the mission rather than any single one of its links.

The Pragmatist inquiry order — Peirce / James / Dewey — settles the meaning of a concept by tracing its practical bearings. Applied here:

1. **Why does peeragogy?** Why does anyone need a vocabulary for peer-organised co-learning at all? What is the work that vocabulary does that adjacent vocabularies (peer production, communities of practice, collaborative learning, OSS governance) do not?
2. **What is peeragogy?** Settled by the answer to (1). If the work peeragogy does is X, then peeragogy is whatever-it-is-that-does-X. Multiple candidate articulations remain possible (genealogy, pattern-language, bridge, operational, critical — see Theoretical anchoring) but the choice between them is downstream of the work-being-done, not upstream.
3. **Why do these patterns matter, and why does it matter that we think about them together?** The 19 mined patterns (12 process + 7 antipattern) are claims about coordination moves that recur. Whether the claims hold is empirical: where do the moves actually recur, in 2026 contexts, and what is the cost of leaving them unnamed? "Together" matters because peer-organised work is not the same as individually-organised work; the patterns name moves that can only be seen when more than one person is involved.

The Handbook is one carrier for peeragogy among several. The pattern libraries (`futon3/library/peeragogy/` 12 + `futon3/library/collaboration-coherence/` 7) are *of general application*: they describe coordination moves that recur in any peer-organised collective work, not just learning-specific ones. The Handbook, by contrast, is a 31-chapter monolith whose pattern language is buried in chapter 21 with little cross-reference to its actual instances elsewhere. Joe has used peeragogy concepts in published work that does not depend on the Handbook's specific structure (e.g. `corneli2026patterns` in `~/code/p4ng/`, §sec:peeragogy). Other carriers include the pattern-library files themselves (already exist, reusable), the peeragogy podcast series (16+ episodes; transcribable locally), and the futon stack's own internal use of peeragogy patterns in coordination work.

The carrier-shape decision (rewrite the Handbook chapter-by-chapter / refactor into a boxed-set of mini-handbook zines / hybrid / something else that emerges from the question chain) is downstream of the inquiry, not part of its premise. The mission's job in IDENTIFY is to commit to the inquiry order; in MAP to look outward at where the patterns matter in 2026; in DERIVE to choose a carrier shape from what MAP surfaces; in ARGUE to defend the choice against the alternatives that MAP makes visible.

### Theoretical anchoring

- **Pragmatist inquiry order.** (Foregrounded 2026-04-29, Joe direction.) The meaning of "peeragogy" is settled by tracing its practical bearings — Peirce's pragmatic maxim, Dewey's inquiry-as-resolution-of-an-indeterminate-situation. *Why does peeragogy → what is peeragogy → why do these patterns matter (and matter-together)* is the inquiry chain. The question "what is peeragogy?" is not answered by definition or genealogy alone; it is answered by tracing where the patterns recur, what work they do, and what the cost is of leaving them unnamed. This commits the mission to looking *outward* in MAP — at the world, not just the Handbook or the disk drive.

- **Practice-supporting-doctrine analogy** (Joe direction 2026-04-29). Peeragogy may matter the way *sangha* matters in Buddhism, or *consciousness-raising* mattered in Marxism — not as a doctrine, but as the collective practice that lets a doctrine become live for those who hold it. Sangha is one of the Three Jewels alongside Buddha and Dharma; consciousness-raising was the small-group method by which structural conditions became visible to those experiencing them. In each case, the practice is what makes the rest do work. If peeragogy matters in this *kind* of way, then the weird-neologism surface ("peeragogy" reads as a clever portmanteau and undersells the substance) is doing what specialised vocabulary does in any practice tradition: marking that something specific is being attended to, rather than naming the thing itself. This anchor has implications for carrier shape: practice-traditions are typically sustained by *infrastructure for the practice* (retreats, gatherings, lineage transmission, regular returns) rather than by canonical texts. A book is one possible scaffold; it may not be the most natural one.

- **2026-as-contingent-timing.** Why does peeragogy matter *now*? Partly because AI-discourse monopolises attention in 2026 — *"imagine everyone was talking about tulips, or railroads, and you came along with a quiet invention of a completely different nature."* (Joe.) Peeragogy in 2026 is not anti-AI and not in conversation with AI; it is a different temperature in the same room. The salience of the practice-vocabulary increases when the dominant discourse drifts toward "machines learning faster than us"; *humans coordinating their own learning together* registers as a separate kind of move when held against that backdrop. This is a contingent timing argument distinct from the substantive "why does peeragogy" — both belong in ARGUE; conflating them would weaken both.
- **Pattern languages as common notions.** (Carries from prior IDENTIFY.) Following the Spinoza/Deleuze framing, a pattern is a productive capacity, not a style rule. The mined peeragogy patterns are candidate common notions for peer-learning coordination; the antipatterns are partially-adequate common notions whose completion is a writing-coherence-shape positive pattern.
- **Minor literature (D&G).** Google Scholar reports ~708 hits for "peeragogy"; "active inference" reports ~20,200 (~28×). Peeragogy is a *minor literature* in the D&G sense — it speaks from a position not assumed by the major language of education research. The minor-literature position is a feature when the work names something the major literature cannot see; it is a problem when the minor-literature position is mistaken for the work's claim to attention. ARGUE's job is to articulate which is which, dispassionately and without advocacy.
- **Pattern libraries as the primary asset.** The 19 mined patterns (12 process + 7 antipattern) are reusable across futon work and beyond. The Handbook is one delivery channel for them; it is not the patterns themselves. The mission treats the libraries as load-bearing and the Handbook as one of several possible delivery vehicles.
- **Candidate articulations of "what peeragogy is"** — held in tension as inputs to the Pragmatist inquiry, not as parallel theoretical anchors. Each is a hypothesis about *what work peeragogy does*; MAP looks outward to test which hypotheses bear fruit:
  - **Genealogy.** Paragogy (Corneli et al., 2009–2011) → peeragogy (Handbook + community of practice, 2012–2016) → continued use 2016–present. Downstream of pedagogy/andragogy. *Test:* is the genealogy doing live work for current readers, or is it inert citation hygiene?
  - **Pattern-language.** One of the few corpora that names co-learning patterns with cross-references and use cases. *Test:* are the patterns being picked up and reused, or are they sitting in chapter 21 unread?
  - **Bridge.** Sits between education research, OSS-style governance, and pattern-language methodology. *Test:* are practitioners in those adjacent fields finding peeragogy when they search for a co-learning vocabulary, or are they reinventing it locally?
  - **Operational.** Any group running a project-based co-learning effort (study groups, cohort-based courses, OSS contributor cohorts, professional CoPs, AI-agent coordination contexts) uses these moves whether named or not. *Test:* can we point at current 2026 collectives where the patterns are visibly enacted but unnamed?
  - **Critical.** Claims of "peer" sometimes paper over the labor of those who actually facilitate; the Handbook has uncritical platform endorsements; "peer" language can flatten power dynamics that warrant attention. *Test:* what does honest engagement with these critiques produce that advocacy would suppress?

### Scope in

- **Pursue the Pragmatist inquiry chain.** Why does peeragogy → what is peeragogy → why do these patterns matter (and matter-together). Each link of the chain produces material that lands in different phases: the "why" arguments in IDENTIFY/ARGUE, the "what" inventory in MAP, the patterns-matter evidence in MAP and in the chosen carrier output.
- **MAP sequencing: bootstrap a rubric from inside, then look out.** Joe direction 2026-04-29: read what the Handbook says *about itself* first, before evaluating other surfaces. The Handbook's self-claims (its preface, foreword, *How to use this Handbook*, chapter 17 *Peeragogical Assessment*'s self-application, chapter 22 *Antipatterns* read against the rest, chapter 28 *Peeragogy in Action* as empirical confirmation) constitute the *inside view* — what peeragogy claims to be in its own voice. Extracting those claims into a rubric — a checkable list of "things peeragogy says it does / things peeragogy says it cares about / things peeragogy says it isn't" — gives MAP a yardstick for evaluating *other* surfaces (podcast transcripts, adjacent literatures, current 2026 collectives) without the rubric being smuggled in from outside.
- **Then podcasts, as a semi-in / semi-out resource.** Joe direction 2026-04-29: the peeragogy podcast series (`PLG6fmEnfJR2yaWGiK0tSp8QSis4btdCzE`) is Joe-authored material (in) but it surveys co-learning practice in dialogue with guests (out). Reading transcripts against the handbook-self-claims rubric tests two things: do the podcasts hold the same self-understanding the Handbook does, and do the podcasts surface co-learning moves that the Handbook misses? Joe transcribes locally; transcripts feed MAP-stage analysis.
- **Then look outward at the world.** Survey current peer-learning collectives (which are still active in 2026? what new ones have emerged?), adjacent literatures using overlapping vocabulary (CoP, peer production, OSS governance, pattern languages in other domains, sangha-as-practice in contemplative literature, consciousness-raising-as-method in feminist/Marxist literature — the practice-supporting-doctrine analogues from theoretical anchoring), and current real-world contexts where the patterns would be useful but are unnamed (study groups, cohort-based courses, OSS contributor onboarding, professional CoPs, AI-agent coordination). Apply the rubric. The aim is to test the candidate articulations of peeragogy against where the patterns actually recur, with a yardstick that came from peeragogy's own self-understanding rather than from outside.
- **Inventory peeragogy concepts as currently used in Joe-authored work.** Specifically: `~/code/p4ng/` (active paper, citing peeragogy as background); the futon stack's own patterns library (12 + 7 mined this week); any other Joe-authored material that cites peeragogy. Also part of the inward MAP work, alongside handbook-self-claims and podcast-transcripts.
- **Settle "what is peeragogy in 2026" as a working answer** — paragraph-length, dispassionate, holding multiple framings tested against MAP findings. Lands in ARGUE.
- **Decide on the carrier shape.** Candidate options surfaced so far, all feasible given today's proof-of-concept capabilities:
  - (a) *Rewrite the existing Handbook chapter-by-chapter.* The prior IDENTIFY's frame.
  - (b) *Refactor the existing Handbook into a small boxed-set of mini-handbook zines.* Possibly one zine per pattern. Single-point clarity per zine.
  - (c) *Hybrid:* keep the Handbook as a reference text but extract a zine series as the entry point. The zines are the marketing surface; the Handbook is the depth.
  - (d) *Some other text-shape that emerges from MAP* — the outward survey may surface a shape we haven't yet imagined (a wiki-as-pattern-index, a series of in-stack PSRs/PURs that exhibit the patterns operationally, an essay collection citing the patterns in disparate contexts).
  - (e) *Practice-supporting infrastructure rather than a text* — if the practice-supporting-doctrine analogy lands (sangha / consciousness-raising), the right carrier may not be a book at all but rather scaffolding for the practice itself: facilitation kits for study groups, cohort-onboarding playbooks, regular-return formats (a recurring "peeragogy salon"), in-stack tooling that makes the patterns operational. The text-carriers (a)–(d) become *artifacts of the practice* rather than the practice's primary delivery channel.
  DERIVE-stage decision.
- **Define appropriately-scoped handoffs.** Not "blast the dormant peeragogy Google Group with a 450-line reviewer report." Instead, per-channel asks tuned to specific stakeholders: chapter-30 affiliations to the mailing list (low-cost contribution); methodological framing to a Wikibooks talk page where editors might read it; the "what is peeragogy in 2026" essay to its own publishing channel; the surgical-edit / canonical-flip pattern to other Wikibooks projects facing import-rot. DERIVE-stage.
- **Locally-transcribed peeragogy podcast series as additional source material.** Joe will transcribe the playlist (`PLG6fmEnfJR2yaWGiK0tSp8QSis4btdCzE`, 16+ episodes) locally — no Otter.AI dependency. Adds an audio-form voice the Handbook's text-form misses. In scope as MAP source.

### Scope out

- **Wholesale chapter-by-chapter rewrite of the existing Handbook as the implicit mission objective.** That's now option (a) of the carrier-shape decision, not the mission's pre-committed shape.
- **Adjudicating peeragogy's substantive claims about peer learning.** Out of scope (still). Potentially in scope for a future mission.
- **Building the Wikibooks publish capability.** Already done; the bot works; the canonical-flip lets us read live wikitext. Today's INSTANTIATE-equivalent work stands.
- **Building the patterns themselves.** Already mined into the libraries; this mission consumes them, doesn't re-mine.
- **Pursuing AI-specific applications of peeragogy.** Joe explicitly notes peeragogy matters more broadly than as-applied-to-AI; the mission should not narrow the frame to AI even though some current Joe work (p4ng) uses peeragogy in AI contexts.

### Completion criteria

1. **The Pragmatist inquiry chain has been worked through** — the why-does / what-is / why-do-the-patterns-matter chain produces material in the right phases (why's in IDENTIFY/ARGUE, what-is in MAP and ARGUE, patterns-matter evidence in MAP and the chosen carrier).
2. **MAP looked outward** — concrete findings about where the patterns recur in 2026 (named or unnamed), which adjacent literatures use overlapping vocabulary, which current collectives could pick up the patterns. Not just an inventory of the disk.
3. **A working answer to "what is peeragogy in 2026"** exists in ARGUE: dispassionate, tested against MAP findings, articulating both the minor-literature position's strengths and its limits, and explicit about which candidate articulations bore fruit and which did not.
4. **A defended decision on carrier shape** — (a) rewrite, (b) zines, (c) hybrid, (d) some shape that emerged from MAP — with the case for the chosen shape and cases against the alternatives recorded.
5. **A handoff plan** that names specific channels and specific asks, replacing the "mailing-list dump" instinct with appropriately-scoped per-channel routing. Each handoff says: who, what, why, what we're asking of them.
6. **The existing reviewer report** is repositioned as one of several MAP-stage artifacts feeding the new DERIVE — not the implicit mission deliverable.
7. **Today's proof-of-concept capabilities documented as available primitives**, with descriptions of when each is the right move (surgical-edit vs. wikitext-native rewrite vs. round-trip-safe importer; canonical-flip vs. preserve-Markdown).
8. **At least one carrier instance produced end-to-end** as proof-of-pipeline. Whichever falls out of the carrier-shape decision: one zine, one rewritten chapter, one short-form essay distilling peeragogy, an in-stack PSR/PUR sequence exhibiting the patterns operationally, etc.

### Relationship to other missions

- **Predecessor framing:** prior `M-peeragogy-rewrite` IDENTIFY (2026-04-27, see §1.prior below) assumed near-complete rewrite as the mission's shape. Its findings — 88 annotations, 32 clusters, semilattice queue, four-axis taxonomy, the reviewer report — are still load-bearing as evidence for *what shape the rewrite/refactor should take*. The reframe folds that work into MAP rather than discarding it.
- **Source for in-use peeragogy concepts:** `~/code/p4ng/` (corneli2026patterns paper). Patterns cited and used; the citation pattern there is one model for how the libraries get used externally without depending on the Handbook.
- **Pattern library siblings:** `futon3/library/{writing-coherence, pattern-coherence}` — same flexiarg shape; the peeragogy + collaboration-coherence libraries inherit this discipline.
- **Audio source:** Peeragogy podcast YouTube playlist (`PLG6fmEnfJR2yaWGiK0tSp8QSis4btdCzE`). To be transcribed locally (Joe-driven; no third-party transcription service in the loop).
- **Sibling missions in the futon ecosystem** that already use peeragogy patterns as coordination vocabulary (futon3c real-time coordination work; futon5a wiring-as-pattern work) — these are in-stack proofs that the patterns are general-application.

### Source material

- Today's proof-of-concept infrastructure: `futon4/scripts/peeragogy-{fetch-corpus,migrate-annotations,flip-to-mw,publish}.clj`.
- The reviewer report at `futon4/holes/labs/M-peeragogy-rewrite/peeragogy-handbook-reviewer-report-2026-04-29.md`.
- **The 2016 prior** at `futon4/holes/labs/M-peeragogy-rewrite/peeragogy-2016-prior.md` — rubric of self-claims extracted from the Handbook's reflexive chapters (preface, foreword, revised intro, how-to-use, overview, peeragogical assessment, antipatterns, peeragogy in action). The first MAP artifact; provides the yardstick the outward survey applies.
- **Podcast transcripts (semi-in/semi-out)** at `futon4/holes/labs/M-peeragogy-rewrite/podcasts/transcripts/<id>/<id>_full.txt` — 13 episodes of the Peeragogy Podcast, locally transcribed via faster-whisper-small with peeragogy vocabulary primed via initial_prompt; ~75,500 words across ~480 minutes of audio. Driver: `futon4/scripts/peeragogy-podcasts.py`.
- **Podcast Essays catalog** at `futon4/data/essays/podcasts/peeragogy-podcasts/peeragogy-podcasts-book-manifest.el` (+ empty annotations sidecar) — registers the 13 transcripts as a sibling catalog to the Handbook in the Essays browser. Loader module: `futon4/dev/arxana-browser-essays-podcasts.el`. The transcripts get the same three-axis annotation discipline as the Handbook chapters.
- The semilattice queue at `futon4/data/essays/wikibooks/peeragogy-handbook/peeragogy-handbook-revision-queue.edn`.
- The wikitext canonical corpus at `futon4/data/essays/wikibooks/peeragogy-handbook/wikitext/`.
- The pattern libraries at `futon3/library/peeragogy/` (12 patterns) and `futon3/library/collaboration-coherence/` (7 patterns).
- `~/code/p4ng/main.tex` §sec:peeragogy — exemplar of peeragogy concepts in active scholarly use.
- Peeragogy podcast playlist (audio source; local transcription pending — Joe-driven).
- Prior mission body §§1.prior – 7 (preserved below as the prior framing's working artifacts; serves as MAP-stage evidence in the new framing).

### Owner and dependencies

- **Owner:** Joe (architectural authority on peeragogy framing; what counts as a faithful articulation; carrier-shape decision; what an appropriately-scoped handoff looks like) + claude (mining existing usage, drafting candidate frames, executing the chosen shape, building any new infrastructure).
- **Primary repo:** futon4.
- **Secondary repos:** futon3 (pattern libraries; potential new libraries if the zine-decomposition needs structural support), p4ng (in-use exemplar).

---

## 1.prior IDENTIFY (2026-04-27 — superseded by §1 above; preserved as MAP-stage evidence)

The text below is the prior IDENTIFY framing. Its substance — the structural-law claim, the semilattice queue, the three-axis annotation system, the pattern mining — produced today's MAP and DERIVE artifacts. Those artifacts feed forward into the new framing as evidence about *what shape the rewrite/refactor/decomposition should take*. The prior IDENTIFY is no longer the mission's primary frame, but it is preserved verbatim as an artifact of how the work got here.

### Motivation

The Peeragogy Handbook is a 2014–2016 wikibook on peer learning, currently imported into the arxana Essays subsystem at `/home/joe/code/futon4/data/essays/wikibooks/peeragogy-handbook/`. The book's last edition shipped in 2016; specific platform recommendations have aged out (Google+ Hangouts is defunct; Pearl Trees is largely defunct; many cited URLs no longer resolve). The Wikibooks importer has produced visible artefacts (mojibake'd MediaWiki File embeds, an unfollowed `#REDIRECT` directive that left chapter 4 empty). Some chapters — notably *Peeragogies Technology* — would be assessed as out-of-date by any 2026 reader on first inspection.

The intent is a *near-complete rewrite*, not a light refresh. This is large enough that an unstructured editorial pass would lose track of which problems were systemic and which were chapter-local; the pattern-language ambitions of the book itself (a named, composing pattern set induced from peer-learning practice) demand that the rewrite preserve and strengthen the language, not just patch the prose.

A first-pass review by Codex (2026-04-27) produced 10 standoff `annotation/writing-coherence` items in a sidecar at `peeragogy-handbook-book-annotations.el`, addressed via the existing `arxana-browser-essays-wikibooks.el` merge path. That set is too sparse and too one-axis to drive a revision plan.

### The structural-law gap

In futon territory, structural-law work proceeds against a Candidate Invariant Queue: a backlog of unverified-but-tracked invariants with explicit lattice membership (multiple parents, queryable joins, prerequisite DAG). In Peeragogy land, the equivalent surface did not exist — annotations sat as free-floating comments with no aggregation across patterns, chapters, or causes. The mission's claim is that the Peeragogy Handbook's near-rewrite is a contained microcosm of structural-law work: a small enough corpus that a semilattice can be built explicitly and exercised end-to-end, while large enough to expose the failure modes of doing rewrite-scale editorial work without one.

### Theoretical anchoring

- **Pattern languages as common notions.** Following the same Spinoza/Deleuze framing as `M-writing-ethics`, a pattern is a productive capacity, not a style rule. The peeragogy patterns mined from the Handbook's *Patterns* chapter (Heartbeat, Wrapper, Newcomer, Roles, Moderation, Pattern Language, Carrying Capacity, Creating-a-Guide, Discerning-a-Pattern, Polling-for-Ideas, Use-or-Make, Roadmap) are candidate common notions: each names a coordination move that recurs across peer-learning situations. The Handbook itself claims Christopher Alexander's framing for these patterns; the rewrite should honour that claim by exhibiting compositional structure, not just listing names.
- **Semilattice as the structural-law artefact.** The CIQ analog for this mission lives in `peeragogy-handbook-revision-queue.edn`, modeled on `futon5a/holes/holistic-argument-aif2.edn` and `futon5a/data/stack-stereolithography-priority-queue.edn`. The semilattice property — each annotation has multiple parents (concern, cluster, section, pattern) and joins on any of those surfaces yield queryable views — is the design choice that distinguishes this mission's queue from a tree-shaped roadmap. A tree picks one organising axis (chapters, severity, …) and forces the rest into subordination; a semilattice keeps all axes live so the join structure surfaces clusters that single-axis traversal would miss.
- **Three-axis annotation system as the evidence layer.** The mission introduces `annotation/peeragogy` as a third subtype alongside the existing `annotation/critique` (🔍) and `annotation/writing-coherence` (🤖) — marker 🗪 (U+1F5EA), wired in both renderers in `arxana-browser-essays.el`. Each axis catches a different kind of failure: writing-coherence flags structural prose forms; critique flags non-pattern-backed concerns (link rot, staleness, import artefacts); peeragogy flags places where the book fails to apply its own pattern language. The three together are richer than any one: cross-axis joins (e.g. "chapters with both writing-coherence and pattern-self-application annotations") show the rewrite where to compound effort.
- **Pattern-self-application as a peeragogical invariant.** A distinctive finding of the seed semilattice: 7 of 25 annotations cluster under the concern *pattern-self-application* — places in the book where the prose touches a peeragogy pattern without naming it. This is the peeragogy-internal analog of the writing-coherence pattern *floating-formalism* (a frame is invoked without integration). It is also evidence that the Handbook's pattern language was a catalogue rather than a language at the time of the last edition; making it a language is itself a peeragogical move and should be done explicitly.

### Scope in

- **Pattern mining (done for Patterns chapter).** Twelve flexiargs at `futon3/library/peeragogy/` — Heartbeat, Wrapper, Pattern-Language, Carrying-Capacity, Creating-a-Guide, Discerning-a-Pattern, Moderation, Newcomer, Polling-for-Ideas, Use-or-Make, Roadmap, Roles. Format: `! conclusion / context / TENSION / COMPOSITIONS / CHECK / ABSENCE-SIGNALS` (the writing-coherence flexiarg shape with `FAILURE-MODES` renamed to `ABSENCE-SIGNALS` for positive process patterns).
- **Three-axis annotation sweep over all 31 sections.** Continuing through the 17 currently un-annotated chapters and the 8 under-annotated chapters. Target ~3 annotations per chapter on average across the three axes. Sweep order in `:evidence-gaps :next-sweep-priority`.
- **Renderer support for `annotation/peeragogy`.** Done. Marker 🗪 in both renderers; live-reloaded into the running Emacs session.
- **Semilattice revision queue (`.edn`, semilattice not tree).** Modeled on `holistic-argument-aif2.edn`. Six top-level concerns, 13 clusters, 3-tier priority queue. Queryable from any side via standard Clojure `edn` reader. Lattice integrity check passes (no dangling references between annotations, clusters, and priority queue).
- **Iterate.** The seed semilattice will be revised after each annotation-sweep round; cluster boundaries will shift as denser evidence arrives; the mission doc and the queue update together each round.
- **Rewrite execution (deferred to INSTANTIATE).** The actual chapter rewrites land in INSTANTIATE, ordered by the priority-queue tiers in the .edn (tier-0 blockers / tier-1 cross-cutting sweeps / tier-2 chapter rewrites).

### Scope out

- **Sync-to-Wikibooks workflow.** Joe explicitly noted at mission inception that round-trip sync to the live Wikibook is not yet known to be reproducible. This mission concerns the revision design and execution; pushing back to Wikibooks is a separate mission.
- **Mining the Antipatterns chapter into flexiargs.** Flagged in the queue as the highest-yield next mining target. Likely a parallel `futon3/library/peeragogy-antipatterns/` library. Deferred until at least one full sweep round closes — the queue's findings may suggest different framing.
- **Mining heuristics from the Patterns-and-Heuristics and Thinking-about-Patterns chapters.** Same reason; same deferral.
- **Content-level critique on the book's claims.** The annotation set flags structural problems and currency problems; it does not adjudicate the book's substantive claims about peer learning. That is a separate exercise.
- **Pattern-library calibration via PUR.** Each peeragogy/* annotation is a candidate exercise of the corresponding flexiarg, but recording PURs against the Patterns-chapter mining belongs to the next mission round, not this one.
- **Tooling for cluster-level pivots / org-mode roadmap rendering.** The semilattice is currently consumed by hand. Building Emacs-side helpers to render it as an org roadmap or to pivot by chapter / concern / cluster is plausible follow-on work but not in this mission.

### Completion criteria

1. **Three-axis annotation density.** Every section of the book has at least one annotation on the writing-coherence axis OR critique axis OR peeragogy axis (whichever the section deserves), with a target of ~3 per section averaged across all 31 sections.
2. **Pattern library coverage of the *Patterns* chapter.** All 12 named patterns from chapter 21 reified as flexiargs in `futon3/library/peeragogy/`. ✅ done 2026-04-27.
3. **Renderer wiring for the third axis.** `annotation/peeragogy` renders with a distinct marker in both buffer-overlay and notes-list renderers. ✅ done 2026-04-27 (marker 🗪).
4. **Semilattice revision queue (`.edn`).** Built, parses cleanly, integrity check passes (every annotation referenced in clusters exists in the index; every cluster referenced in the priority-queue is defined). ✅ done 2026-04-27 (seed pass; will revise iteratively).
5. **Each annotation has at least two semilattice parents (a concern and a cluster).** Currently true for all 25 seed annotations. The structural-law property: no orphan annotations.
6. **Per-cluster prerequisite DAG is acyclic.** Currently true for all 13 seed clusters. Verified during integrity check.
7. **Cluster-level resolution decisions on file.** For each tier-0 blocker cluster (`C1-importer-fix`, `C2a-link-rot-policy`, `C-platform-currency-policy`), an explicit resolution recorded in the queue. Not yet done; pending Joe.
8. **Rewrite of at least one tier-2 chapter cluster end-to-end as proof of pipeline.** Most plausibly `C8-chapter-24-full-rewrite` (Peeragogies Technology) — single chapter, four annotations resolve together, clearest test of whether the queue's clustering predicts coherent rewrite work. Not yet done.
9. **Reproducibility.** A new agent could walk the annotation → flexiarg → semilattice → rewrite pipeline from this mission doc + the queue file alone, on a different chapter or a different book.

### Relationship to other missions

- **Depends on:** the writing-coherence library (built 2026-04-24, M-writing-ethics); the three annotation subtypes plumbing in `arxana-browser-essays.el` (extended this mission to add the third); the Wikibooks importer in `arxana-browser-essays-wikibooks.el` and its sidecar merge path.
- **Sibling:** `M-writing-ethics`. This mission and that one share the standoff-comment + pattern + critique infrastructure but exercise it on different corpora and at different scales (one paper vs. one book; recent draft vs. 2016 publication). Findings on each side feed back to the shared apparatus.
- **Enables:** future Wikibooks revision missions (the semilattice + sidecar + renderer pattern generalises); a peeragogy-pattern library that other futon work can consume (futon3c, futon5a) — the patterns describe coordination moves that recur in distributed peer projects, including ones the futon stack itself runs.

### Source material

- `/home/joe/code/futon4/data/essays/wikibooks/peeragogy-handbook/` — book manifest, chapter manifests, source markdown, annotation sidecar, revision queue.
- `/home/joe/code/futon3/library/peeragogy/` — 12 flexiargs mined from chapter 21.
- `/home/joe/code/futon3/library/writing-coherence/` — 13 flexiargs (existing; sourced by Codex's first pass).
- `/home/joe/code/futon3/library/pattern-coherence/` — 6 flexiargs (existing; consulted via cross-references in the peeragogy library).
- `/home/joe/code/futon4/dev/arxana-browser-essays.el` — rendering pathway, three-axis subtype branches at lines ~2054 and ~2161.
- `/home/joe/code/futon4/dev/arxana-browser-essays-wikibooks.el` — sidecar merge path at line 446.
- `/home/joe/code/futon4/holes/mission-lifecycle.md` — this mission's procedural spec.
- `/home/joe/code/futon5a/holes/holistic-argument-aif2.edn` — design precedent for the semilattice.
- `/home/joe/code/futon5a/data/stack-stereolithography-priority-queue.edn` — design precedent for the priority-queue tier structure.

### Owner and dependencies

- **Owner:** joe (structural edits, library curation authority, link-rot/platform/importer policy decisions) + claude (annotation drafting, semilattice maintenance, mechanical rewrites).
- **Primary repo:** futon4.
- **Secondary repos:** futon3 (peeragogy and writing-coherence libraries).

## 2. MAP

### Inventory existing infrastructure (ready)

| Component | Status | Location |
|---|---|---|
| Wikibooks import + sidecar merge | ready | `dev/arxana-browser-essays-wikibooks.el:446` |
| `annotation/comment` rendering | ready | `dev/arxana-browser-essays.el:~2054, ~2161` |
| `annotation/critique` subtype (🔍) | ready (M-writing-ethics) | same |
| `annotation/writing-coherence` subtype (🤖) | ready (M-writing-ethics) | same |
| `annotation/peeragogy` subtype (🗪) | **added this mission** | same |
| `writing-coherence/` flexiarg library | ready (M-writing-ethics) | `futon3/library/writing-coherence/` (13 patterns) |
| `peeragogy/` flexiarg library | **added this mission** | `futon3/library/peeragogy/` (12 patterns) |
| `pattern-coherence/` flexiarg library | ready | `futon3/library/pattern-coherence/` (6 patterns) |
| Sidecar parse + paren-check | ready | `dev/check-parens.sh` |
| Live-reload via emacsclient | ready | standard arxana flow |

### Inventory missing (the actual work)

| Component | Status | Owner |
|---|---|---|
| Annotations for 17 un-annotated sections | missing | claude (in flight) |
| Additional annotations for 8 under-annotated sections | missing | claude |
| Antipattern flexiargs (chapter 22 mining) | deferred | claude after first sweep round |
| Heuristic flexiargs (chapters 19, 20 mining) | deferred | claude after first sweep round |
| Tier-0 cluster resolutions (C1, C2a, C-platform-currency) | missing | joe |
| Tier-1 cross-cutting sweeps (C4, C5, C6, C7, C9, C10) | missing | joe + claude |
| Tier-2 chapter rewrites (C2, C2b, C3, C8) | missing | joe + claude |
| Org-mode / browser surface for the queue | optional | claude on request |

### Survey questions

- **Q1 — How many of the book's 31 sections have at least one annotation?** 11 (with 25 total annotations across them as of 2026-04-27).
- **Q2 — How is the annotation distribution skewed?** Toward front-matter and pattern-related chapters. The middle of the book (chapters 9–13, 17–19, 22–23) is currently un-annotated.
- **Q3 — How many concerns dominate?** Six top-level concerns. Of these, *pattern-self-application* and *structural-framing* dominate by count; *import-quality* dominates by severity.
- **Q4 — Are there clusters that resolve together?** Thirteen identified. The largest by addressed-annotations is `C4-pattern-cross-reference-sweep` (7 annotations, single editorial pass); the largest by chapter-impact is `C-platform-currency-policy` (cross-cutting decision, blocks 5 chapters).
- **Q5 — What's the minimum-cost / highest-yield first move?** `C4-pattern-cross-reference-sweep`. Seven annotations, one editorial pass, no prerequisites, makes the book's pattern language visible-as-language for the first time.

### Surprises (recorded during MAP)

- **Source-text mismatch in the *Patterns* chapter.** The headings *Discerning a pattern* and *Moderation* in `peeragogy-handbook-v1-0-patterns.md` have content that's swapped: the body under *Moderation* opens with material that belongs to *Discerning a pattern*. Mining-time editorial decision: split the content across the two flexiargs based on actual subject, not heading. Logged for re-flagging during the annotation sweep.
- **`How to use this Handbook` (chapter 4) is empty.** The MediaWiki source contained only `1. REDIRECT Peeragogy Handbook/How to use this Handbook` and the importer did not follow the redirect. This is a major-severity import-quality finding that wasn't visible in Codex's first pass because Codex didn't look at chapters with whole-file rendering enabled and zero markdown body.
- **The Antipatterns chapter would yield a parallel flexiarg library.** Recognised during MAP but left out of scope; mining it would surface a class of concerns currently invisible because the antipattern flexiargs do not yet exist (e.g. "the book violates its own antipatterns somewhere").

## 3. DERIVE (revised 2026-04-29 — settled b+e carrier shape, three-artifact plan)

The 2016-prior-vs-2026-podcast-posterior synthesis at
`futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/00-synthesis-2016-prior-vs-2026-posterior.md`
provides material evidence for the carrier-shape decision. Joe's
direction (2026-04-29) settled it: **carrier (b) with strong (e)
elements**, not the chapter-by-chapter rewrite of carrier (a). The
rationale is structural, not stylistic: a chapter-rewrite carrier
inherits the Handbook's 6-year stall mechanism (ep 4 blackbox +
multiple recurring v4 wishes without action) and its mostly-CLA-layer-2
framing. Carriers (b)/(e) can explicitly address worldview/myth (item
2.8) and don't depend on canonical-text production (item 3.9).

The DERIVE produces three artifacts in a deliberate order, each with
the inward Handbook cross-check folded into its own work rather than
held as a prerequisite step.

### Three artifacts

**Artifact 1 — A short essay: "What is peeragogy in 2026"** (~3,000 words).

Names, in compact form, the worldview/metaphor shift the corpus surfaces:

- **Item 2.8 (peeragogy-as-worldview/metaphor-shift, not method)** as the spine — the Sohail-recognition that pedagogy → peeragogy is a CLA layers-3-4 move, not a layer-1-2 prescription.
- **Item 2.7 (timeless-practice / "we didn't discover it")** as second voice — 7+ corroborations across the corpus. Peeragogy is a vocabulary for what's already happening in many places that don't use the word.
- **The convergent-practice family** the corpus surfaces — sangha, Quaker meeting, Linux User Groups, OGM Thursday calls, ACMI-as-haven, CLA, Design Justice, indigenous data frameworks. Naming the family is itself useful.
- **The minor-literature situation honestly** — 708 GS hits vs ~20,200 for active inference. The minor-literature position can be claimed as a feature when the work names something the major literature can't see.
- **The ethics-as-bottleneck register (item 1.8)** and the *integrity is integration* line from David Preston (ep 12) — the practice serves the practitioner's relationship with their own integrity.

Inward Handbook cross-check folds in: the essay needs a focused read of chapter 21 (Patterns) and chapter 22 (Antipatterns) — sampled implicitly via the libraries this week, but worth a focused read to corroborate or qualify item 2.8. Plus reading any of the 23 unsampled chapters that bear on the worldview/metaphor framing.

The essay doubles as the publishable companion to the Handbook itself — short enough to ship, clear enough to land. **First test of whether the post-2016 voice has cohered into a single articulation.** If hard to write, that's evidence of remaining MAP work.

**Artifact 2 — Next wave of `futon3/library/peeragogy/` flexiargs.**

Already mined this week (4 new flexiargs, library now at 16): PAR, curating-not-experting, stewardship-succession, reciprocal-participation. Next-wave candidates surfaced by the corpus:

- *Calling-in not calling-out* (ep 10, Richard Butler — promoted to rubric 6.0b)
- *Check-in at meeting start* (ep 11, Leo Vivier — promoted to rubric 6.0c) — the bookend pair with PAR
- *Brave-safe space* (ep 10) — paired with calling-in
- *Listening as discipline* (ep 7, Charlotte) — newcomer-vantage-point evidence
- *Causal Layered Analysis* (ep 13, Sohail + Ivana) — methodology with explicit four-layer structure
- *Register-moderation / "toe down the level"* (ep 11, Leo) — anti-expertise-inflation discipline
- *Awareness + Storytelling + Education + Historical-Facts + Action* formula (ep 10, Richard) — challenging-conversation facilitation structure
- *Quality + psychological-safety as both-AND* (ep 8, James) — dual-axis discipline

Mine 4–6 of these; take the library to ~20+. Each carries `+ provenance:` lines naming the podcast episode and speakers per the format used for the four flexiargs landed 2026-04-29.

Inward cross-check folds in: each new flexiarg references the Handbook chapter where the pattern is closest (even if implicit), making the relationship between the library and the Handbook traceable.

**Artifact 3 — A general-purpose method note: "Retrospective work as MAP"** (separate artifact, not folded into the essay).

This is the *meta-artifact* — a note about how to do the kind of inquiry the M-peeragogy-rewrite mission did this week. It's not a Peeragogy-Handbook artifact; it's a futon-stack methodology artifact. **Lineage explicitly named:**

- **Joe's PhD thesis** at `futon6/resources/CORNELI-thesis.pdf` describes doing this kind of retrospective; the present method-note is its 2026 successor with running infrastructure.
- **M-live-geometric-stack** (futon3/holes/missions/) operationalises this *live* — substrate-2 makes the retrospective work continuous rather than episodic.
- **Peeragogy PARs** are structurally similar (close-the-loop reflection) but require the practice-supporting infrastructure (carrier shape (e)) to be meaningful — PAR alone is reflection-without-traction; PAR + infrastructure is reflection-with-traction.
- **The PlanetMath retrospective Joe did** circa 2014 is a precedent in the same lineage (different project, more decisively quieted, same shape of inquiry).

The method-note describes:
1. The shape of a retrospective that produces material for forward work (vs. retrospective as commemoration only).
2. The living-rubric discipline (one promotion per source) as a constraint that prevents rubric-balloon while forcing each source to leave a structural trace.
3. The prior-vs-posterior comparison as a synthesis output.
4. The infrastructure required: corpus access (shared transcripts / texts), annotation discipline, a settled rubric format, a place to hold per-source notes alongside cross-corpus synthesis.
5. **What it serves:** UKRN-S can adopt this as a quarterly or monthly practice (Joe potentially in an *Agile Coach* role facilitating). Other future Joe-led retrospectives can adopt directly.

Lands at `futon4/holes/labs/M-peeragogy-rewrite/retrospective-as-map-method.md`. Linked from the M-peeragogy-rewrite mission doc; citable from UKRN-S working paper at `~/npt/working-paper/`.

### Order of work

1. **Artifact 1 (essay)** first — single most coherence-forcing task. Once the essay exists, artifact 2 is shaped by what the essay names as load-bearing.
2. **Artifact 2 (flexiargs)** next — incremental, mostly mechanical from posterior notes.
3. **Artifact 3 (method-note)** alongside or after — general-purpose, ages well, can be revised as UKRN-S engagement clarifies.

### Handoff plan

Once the artifacts exist, handoffs route them appropriately (per IDENTIFY's *appropriately-scoped handoffs* commitment):

- **Mailing-list message:** announce the essay (small, finite, one-link ask). Replaces the stuck "blast a 450-line reviewer report" plan from the earlier mailing-list draft.
- **Wikibooks talk page:** post the essay's companion notes about the Handbook itself with explicit pointers to the rubric. Plus the surgical-edit closures landed 2026-04-29 (revids 4633108–4633113).
- **`futon3/library/peeragogy/`:** the new flexiargs ship there as they're mined.
- **UKRN-S working paper at `~/npt/working-paper/`:** cite the retrospective-as-MAP-method companion in the methods/related-work section if appropriate. *Optional but well-aligned* — UKRN-S could employ Joe as an Agile-Coach-equivalent to run quarterly/monthly retrospectives of its own network practice.
- **futon6:** the PlanetMath-spirit-rolled-forward connection makes the method-note a natural cross-reference point. *"Both PlanetMath and Peeragogy are things people can learn from"* — neither presented as best-practice; both as sources for forward work.

### Connections to adjacent work made explicit

- **PlanetMath (2000-2014)** — Joe's prior decade-plus open-knowledge-collaboration project. More decisively quieted than Peeragogy. The retrospective-as-MAP-method companion explicitly names it as an instance of the same lineage. The futon6 repo carries PlanetMath spirit forward.
- **UKRN-S (active)** — Joe's current decade-spanning network-practice work. Stewardship-succession (rubric 3.9), enabling-conditions (3.11), pre-existing-community (1.9), the locomotive metaphor (ep 11) all transfer directly. The method-note enables Joe to facilitate UKRN-S retrospectives at a quarterly/monthly cadence.
- **M-live-geometric-stack (futon3)** — the *live* version of the retrospective work the present method-note describes as a discrete-pass discipline. The two are complementary: discrete-pass works without substrate-2 in place; live works once substrate-2 is operational.
- **Joe's PhD thesis (futon6/resources/CORNELI-thesis.pdf)** — the methodological ancestor of all of the above. Cited explicitly in the method-note.

### Carrier shapes deliberately not chosen

- **Carrier (a) chapter rewrite**: structural risk too clear (the 6-year stall mechanism is the Handbook's history). Not ruled out for the future if the canonical text becomes worth maintaining; ruled out for *this* mission's INSTANTIATE.
- **Carrier (c) hybrid (rewritten Handbook + zine series)**: defers the structural risk of (a) without resolving it. If the zine series ships and the Handbook revival follows organically, that's option (c) emerging from the zine work — not committed to up front.
- **Carrier (d) other text-shape from MAP**: the corpus didn't surface a fourth text shape distinct from essay + zines + chapters. Stays open for future iteration.
- **Carrier (f) teaching course**: the 2020 Pilot Course (ep 9) is a historical instance worth investigating — if it shipped, it's evidence-for-(f); if it didn't, that's the same stall pattern at a different scale. Investigation deferred to follow-on; not committed to as the primary carrier here.

### IF / HOWEVER / THEN / BECAUSE — non-obvious design choices

- **IF** carrier (a) chapter-rewrite is the obvious move (it's what the prior IDENTIFY assumed),
  **HOWEVER** the corpus's evidence is structural — items 2.8, 3.9, 1.9, plus the 6-year stall — not stylistic,
  **THEN** decline carrier (a) for this mission's INSTANTIATE,
  **BECAUSE** repeating the same shape of work that materially failed for six years has no positive expected value, and the alternative carriers (b)/(e) are well-justified by the corpus.

- **IF** the inward Handbook cross-check is treated as a prerequisite to the essay,
  **HOWEVER** essay-shaping is what determines which Handbook chapters are load-bearing,
  **THEN** fold the cross-check into the essay's own work instead,
  **BECAUSE** the rubric becomes load-bearing only when applied to the Handbook through the lens of a question being answered, not as a parallel-track read-through.

- **IF** the method-note is folded into the essay's introduction (as I initially asked),
  **HOWEVER** Joe noted (2026-04-29) the method-note has lineage going back to his PhD thesis and serves UKRN-S consulting potential,
  **THEN** keep the method-note as a separate, general-purpose artifact,
  **BECAUSE** it's reusable infrastructure for any future Joe-led retrospective and shouldn't inherit the Peeragogy-essay's specific framing.

- **IF** the flexiarg additions wait until the essay is done,
  **HOWEVER** the flexiarg work is mostly mechanical from posterior notes and provides reference material for the essay's pattern-language section,
  **THEN** start flexiarg mining in parallel with essay drafting,
  **BECAUSE** essay completion benefits from being able to cite the flexiargs as already-shipped artifacts rather than as forthcoming.

### Pattern grounding

The DERIVE itself uses several patterns from the present library:

- `peeragogy/wrapper` (mined 2026-04-27) — the synthesis at `00-synthesis-2016-prior-vs-2026-posterior.md` IS a Wrapper for the 13-episode posterior pass.
- `peeragogy/discerning-a-pattern` (mined 2026-04-27) — the 13 promotions surface 13 cross-corpus patterns from the per-source noise.
- `peeragogy/use-or-make` (mined 2026-04-27) — the carrier-shape decision is exactly a use-or-make: use the Handbook (carrier a) vs make new carriers (b/e).
- `peeragogy/par` (mined 2026-04-29 from this mission) — the prior-vs-posterior comparison is a PAR at mission scale.
- `peeragogy/stewardship-succession` (mined 2026-04-29 from ep 4 of this mission) — the carrier-shape decision is itself shaped by stewardship-succession reasoning.

---

## 3.prior DERIVE (2026-04-27 — superseded by §3 above; preserved as MAP-stage evidence)

The text below is the prior DERIVE from the original mission framing
(structural-law-on-Peeragogy-Handbook + three-axis-annotation-system
design). Its substance produced the annotation discipline that made
the present rubric pass possible. The reframe (carrier shape b+e for
INSTANTIATE) supersedes it, but the annotation infrastructure it
designed remains in service.

### Entity types

- **Annotation.** Standoff comment with stable `:id`, anchored to a `:section-id` via a `:passage` selector, `:hx-type "annotation/comment"` plus a discriminator label (`annotation/critique` | `annotation/writing-coherence` | `annotation/peeragogy`). Persisted in the manifest sidecar at `peeragogy-handbook-book-annotations.el`. Identity: stable id, source = manifest sidecar, ingested = wikibooks loader merges into manifest at refresh time.
- **Concern.** A *cause-axis* node. Names a root cause that may resolve in different clusters. Identity: keyword (e.g. `:import-quality`, `:pattern-self-application`). Source = derived from annotation analysis; authored in the .edn queue.
- **Cluster.** A *work-unit* node. Groups annotations that resolve together and declares prerequisite edges to other clusters. Identity: keyword (e.g. `:C1-importer-fix`). Source = derived; authored in the .edn queue.
- **Section.** A book chapter, indexed 0–30 in the existing manifest. Identity: integer index + `:entity-id` URI. Source = wikibooks importer.
- **Pattern.** A flexiarg in `futon3/library/{writing-coherence,peeragogy,pattern-coherence}/`. Identity: pattern name (e.g. `peeragogy/heartbeat`). Source = library, hand-authored (some mined this mission).

### Relation types

- **Annotation → Section** (binary, `:section`): which chapter the annotation pins to.
- **Annotation → Concern** (n-ary, `:concerns`): the multi-parent edge that makes the structure a semilattice rather than a tree. An annotation can join several concerns simultaneously.
- **Annotation → Cluster** (n-ary, `:clusters`): the work-unit memberships. Same multi-parent property.
- **Annotation → Pattern** (binary, `:pattern`, optional): the source pattern when the annotation is pattern-backed.
- **Cluster → Cluster** (DAG, `:prerequisites` / `:unblocks`): topological ordering for the priority queue.
- **Cluster → Concern** (n-ary, implicit via shared annotations or explicit via `:addresses-concerns`): concerns the cluster makes progress on.

### Invariant rules (checkable)

- **I1 (no orphan annotations):** every annotation in the queue's `:annotations` index belongs to at least one concern AND at least one cluster.
- **I2 (no dangling cluster references):** every annotation id referenced in any cluster's `:addresses` exists in the queue's `:annotations` index.
- **I3 (no missing cluster definitions):** every cluster id referenced in the priority-queue tiers is defined in `:clusters`.
- **I4 (acyclic prerequisite DAG):** the prerequisite/unblocks relation between clusters has no cycles.
- **I5 (priority-queue topological order):** within `:priority-queue`, every cluster in tier-N has all its prerequisites in tier-`(< N)` or in `:tier-0-blockers`.
- **I6 (sidecar ↔ queue agreement):** every annotation id in the queue exists in the sidecar (`peeragogy-handbook-book-annotations.el`) and vice versa.

I1–I3 currently pass (`clojure -M -e '...'` cross-check, 2026-04-27, returned `:missing-from-clusters 0 :orphan-annotations 0 :missing-cluster-defs #{}`). I4 not yet automated (small enough cluster set to verify by inspection). I5 holds by construction in the seed pass. I6 not yet automated; passes by inspection because the queue was authored from the sidecar.

### Data flow

```
flexiarg library (futon3/library/{writing-coherence,peeragogy}/)
                     │
                     ▼
sidecar (peeragogy-handbook-book-annotations.el) ── merges into ──▶ book manifest ──▶ Essays browser (rendered live)
                     │                                                                                  │
                     ▼                                                                                  ▼
revision queue (peeragogy-handbook-revision-queue.edn)            three-axis markers per annotation: 🤖🔍🗪
                     │
                     ▼
priority-queue tiers ──▶ rewrite work (INSTANTIATE phase) ──▶ revised manifest (a future Wikibooks sync)
```

The dataflow is one-directional in this mission: mining produces flexiargs; flexiargs ground annotations; annotations populate the sidecar; the sidecar plus analysis populates the queue; the queue produces the rewrite plan. Future iterations re-enter at the annotation step (sweep adds to sidecar) and re-derive the queue.

### IF / HOWEVER / THEN / BECAUSE — non-obvious design choices

- **IF** the revision queue is a tree (one parent per annotation),
  **HOWEVER** annotations cluster on multiple axes (chapter, concern, cluster, pattern) and a tree forces one to dominate,
  **THEN** model the queue as a semilattice in `.edn` with multi-parent edges,
  **BECAUSE** the structural-law analog the mission set out to build is exactly the lattice property — joins on each axis surface different clusters, and a tree-shaped roadmap would systematically hide cross-axis findings.

- **IF** the third annotation subtype reuses an existing emoji,
  **HOWEVER** the existing markers (🤖 writing-coherence, 🔍 critique, 💬 generic comment) already saturate the visual vocabulary,
  **THEN** introduce 🗪 (U+1F5EA, Two Speech Bubbles),
  **BECAUSE** the Two Speech Bubbles glyph reads as "peer-to-peer dialogue" without overlap with the existing markers, and the semantics matches the peeragogy axis.

- **IF** the peeragogy flexiargs follow the writing-coherence shape verbatim,
  **HOWEVER** writing-coherence flexiargs are *anti-patterns* (their FAILURE-MODES list bad surface forms) and peeragogy patterns are *positive process patterns* (their absence is the failure),
  **THEN** keep the shape (`! conclusion / context / TENSION / COMPOSITIONS / CHECK / X-MODES`) but rename the closing block from `FAILURE-MODES` to `ABSENCE-SIGNALS`,
  **BECAUSE** the closing block's role is to enumerate detection cases for the pattern's *absence or failure*, and the writing-coherence semantics ("specific bad surface forms") inverts cleanly to "specific symptoms of absence" without changing the structural function.

- **IF** annotations live only in the sidecar and the queue is derived,
  **HOWEVER** the queue contains analytic information (concerns, clusters, prerequisites) that the sidecar cannot represent,
  **THEN** keep both as first-class artefacts — sidecar as evidence layer, `.edn` queue as analysis layer — with invariant I6 (sidecar ↔ queue agreement) as the consistency contract,
  **BECAUSE** mixing analysis into the sidecar would couple it to a particular queue design and break the wikibooks merge contract.

- **IF** the new `annotation/peeragogy` label needs a custom marker font,
  **HOWEVER** the user's default Emacs font (`BitstreamVeraSansMono Nerd Font`) doesn't include U+1F5EA and Noto Color Emoji is intentionally disabled,
  **THEN** register `Noto Sans Symbols2` for U+1F5EA via `set-fontset-font` (clean monochrome glyph, already on the system),
  **BECAUSE** this preserves the user's deliberate emoji-font choice while filling the single missing glyph; the registration is a one-liner in init.el, not a global font change.

### View / UI specifications

- **Buffer-overlay marker (`arxana-browser-essays.el:~2054`):** `🗪N` after each `annotation/peeragogy` passage, where `N` is the annotation index. Reuses existing closure prefix (`✓`).
- **Notes-list rendering (`arxana-browser-essays.el:~2161`):** `* 🗪N Peeragogy` heading plus the note body, in the same shape as the existing critique/writing-coherence/comment branches.
- **No new browser commands or views are added in this mission.** The existing essays browser surface picks up the new label via the cond cascade and displays accordingly.

### Wiring diagram

Not applicable (no AIF+ exotype involvement). The data-flow diagram above is sufficient.

### Fidelity contract

Not applicable (no port / rebuild involved). The mission *adds* one renderer branch and one new pattern library; it doesn't replace existing behaviour.

## 4. ARGUE (revised 2026-04-29 — methodological-self-application reframe)

The prior plain-language argument framed the mission as *the Handbook is from 2014–2016 and needs a near-complete rewrite — here is the queue we built to do the rewrite*. Joe noted (2026-04-29, after VERIFY): that framing is *self-flagellating TryHarder*. It misses what was actually produced. In doing the work, we created a method note (`retrospective-as-map-method.md`) — i.e. the practice ran its own retrospective discipline on its own canonical artifact, with its own pattern-language as the rubric, anchored in its own collected voices (the podcast). That method-applied-to-itself is the more interesting half of what landed.

The argument needs to reflect this. Otherwise we instantiate a new version of the Handbook; what we should be instantiating is *the method*, with the Handbook revision question as a downstream operational matter rather than the mission's protagonist.

### Pattern cross-references (updated against present library)

- **`peeragogy/par`** (mined 2026-04-29 from this mission). The full mission is structurally a PAR at mission scale: *what did we intend, what is happening, what are the perspectives, what did we learn or change, what should we change going forward.* The five-step structure visible across the IDENTIFY-redux → MAP → DERIVE arc.
- **`peeragogy/wrapper`** (mined 2026-04-27). The synthesis at `00-synthesis-2016-prior-vs-2026-posterior.md` IS a Wrapper for the 13-episode posterior pass — a maintained overview that lets a contributor see project state at a glance.
- **`peeragogy/discerning-a-pattern`** (mined 2026-04-27). The 13 promotions across the rubric pass surface 13 cross-corpus patterns from the per-source noise — discernment work in operation.
- **`peeragogy/curating-not-experting`** (mined 2026-04-29). The mission deliberately hosts conversation between the Handbook (artifact) and the podcast (corpus) and the rubric (method) without positioning any of the three as the expert. The synthesis is curatorial.
- **`peeragogy/stewardship-succession`** (mined 2026-04-29). The carrier-shape decision (b+e over a) is itself stewardship-succession reasoning — naming what the practice's next steward-shape needs to be, given that the prior canonical-text steward-shape stalled for 6 years (item 3.9).
- **`peeragogy/reciprocal-participation`** (mined 2026-04-29). The mission's outputs reciprocate the corpus: the speakers who supplied 75,500 words of transcript get cited specifically in the rubric, the annotations, the synthesis, and the artifacts to come.
- **`peeragogy/use-or-make`** (existing). The carrier-shape decision is a use-or-make: use the Handbook (carrier a) vs make new carriers (b/e). Decided make, with the use-mode left open for a future when the canonical text is worth maintaining.
- **`peeragogy/pattern-language`** (existing). The mission applies its own flexiarg recursively: the rubric, the annotations, and the synthesis exhibit cross-references between flexiarg library / handbook / posterior corpus, demonstrating the language property the Handbook's *Patterns* chapter claims for itself but does not exhibit.
- **`writing-coherence/floating-formalism`** (one annotation in the seed set, sec 14). The mission's own structure has the inverse property — every framework cited (rubric, podcast pass, the four 2026-04-29-mined flexiargs) is integrated with the work, not dropped.
- **`writing-coherence/scope-mush`** (three annotations in the seed set). The mission's *Scope in / Scope out* sections are deliberately structured to avoid the same pattern — each scope item is a committed promise.

### Theoretical coherence (updated)

The mission's IDENTIFY-redux (revised 2026-04-29) committed to the *Pragmatist inquiry chain* (why does → what is → why do the patterns matter and matter-together). The DERIVE produces three artifacts that walk the chain:

- **Artifact 1 (essay)** — answers *what is peeragogy in 2026* and *why does it matter*, using the worldview/metaphor-shift framing (item 2.8) and the timeless-practice framing (item 2.7) corroborated 10 times across the corpus.
- **Artifact 2 (next-wave flexiargs)** — extends the pattern library with the next-wave moves the corpus surfaced (calling-in, check-in, brave-safe-space, listening-as-discipline, CLA, register-moderation, etc.). Names the operational moves the practice makes.
- **Artifact 3 (method note)** — articulates the *method itself* — *the practice running its own retrospective discipline on its own canonical artifact* — as a reusable artifact for any community doing this kind of inquiry. Lineage: Joe's PhD thesis → M-live-geometric-stack (live version) → Peeragogy PARs (similar but need infrastructure).

The three artifacts are not three sub-tasks of a Handbook revision; they are three aspects of *the method having been applied to itself*. The Handbook revision question is downstream — what the operational decisions look like *given* the worldview shift the essay names, the moves the flexiargs name, the discipline the method-note names.

The reframe also resolves the prior IDENTIFY's tension between *build-framing* (recruit collaborators to a project) and *timeless-framing* (recruit noticing to a practice): the build-framing applies to the method (we built a method by doing the retrospective); the timeless-framing applies to peeragogy itself (we didn't invent it). Both are honest; they apply to different things.

### Trade-offs (updated)

- **Methodological-self-application as the protagonist vs Handbook revision as the protagonist.** Decided in favour of methodological-self-application (per Joe direction 2026-04-29). Cost: readers expecting a Handbook v4 announcement get a different deliverable; some prior contributors may experience the reframe as the project not finishing what it started. Benefit: the value extends past the project's own self-care; communities running their own retrospectives can use the method note directly; the Handbook revision question becomes downstream-operational rather than the mission's stuck point.
- **Three artifacts (essay + flexiargs + method-note) vs one canonical text (rewritten Handbook).** Decided in favour of three artifacts. Cost: more surfaces to maintain; no single reference point for newcomers to the project. Benefit: each artifact is independently shippable; the method-note is reusable beyond peeragogy; the flexiargs ship as they're mined; the essay can ship before the rewrite question is resolved.
- **Outside-voice as load-bearing (Sohail's CLA-of-peeragogy framing) vs inside-voice (the core crew's accounts).** Decided in favour of using both — outside-voice for the *what is peeragogy in 2026* essay-spine (item 2.8 with Sohail's quote), inside-voice for the corroborations (item 2.7 with 10 voicings from 7+ inside speakers). Cost: more complex framing to hold both; risk of seeming to outsource self-understanding. Benefit: the outside-voice recognition shows the worldview-shift is recognisable from adjacent disciplines, which is itself part of the *why peeragogy matters* answer.
- **PlanetMath retrospective as same-lineage precedent vs distinct case.** Decided to name the lineage. Cost: invites comparison the Handbook revision context doesn't strictly need. Benefit: positions the present work as a genre Joe has done before (and will do again, e.g., for UKRN-S), making the method-note a more honest articulation of the discipline.

### Plain-language argument (revised)

The Peeragogy project has spent ten years working on a pattern language for peer learning and peer production — patterns the project itself has been using to coordinate its own work. What it had not done was apply that method to its own canonical artifact, the Peeragogy Handbook, which was last published in 2016 and has been quietly stalled since. So we did that this week. We collected the project's own ongoing voices (a 13-episode podcast running 2020–2022, ~480 minutes, 75,500 words of locally-transcribed text), extracted what the 2016 Handbook claims about itself into a checkable rubric of 60 self-claims, and walked each podcast episode against the rubric — corroborating, qualifying, extending, occasionally challenging.

What we found is the more interesting half of the work. Speakers across the corpus consistently name peeragogy as *something that's been happening for a long time in many places that don't use the word*: 10 voicings from 7+ speakers across 8 episodes, with explicit references to Quaker meetings, Linux User Groups, Mexican femicide-resistance work, Buddhist sangha, consciousness-raising-circles. A futures-studies practitioner who works with UNESCO and governments worldwide names peeragogy in his own framework's terms — as a *worldview/metaphor shift* from "professor knows best" to "we all create together," not as a method or curriculum. The Handbook is one carrier of that shift; ten years on, the carrier has stalled but the practice has continued — in podcasts, in classrooms, in community media, in indigenous data work, in personal practice. The practice is the load-bearing thing, not the canonical text.

So we're producing three things that share that finding. **An essay** that names what peeragogy is in 2026 and why it matters — 3,000 words, a publishable companion to the Handbook rather than its replacement. **A handful of new pattern entries** in the practice's pattern library — concrete operational moves the corpus surfaced (calling-in not calling-out; check-in at the start of meetings; brave-safe-space; listening as a discipline; CLA as a peer-learning method) — taking the library from 16 to ~20 patterns. **A general-purpose method note** about how to do this kind of retrospective work — the practice running its own discipline on its own canonical artifact, with a clear lineage from Joe's PhD thesis through the futon stack's live-retrospective infrastructure — reusable for any community doing similar inquiry.

The Handbook revision question is now downstream-operational rather than the mission's protagonist. Whether and how a v4 lands is a question the worldview articulated in the essay, the moves named in the flexiargs, and the discipline described in the method note will inform — without needing to be answered first. Other communities running their own retrospectives can use the method note directly; the project's value extends past its own self-care.

---

## 4.prior ARGUE (2026-04-27 — superseded by §4 above; preserved as MAP-stage evidence)

The text below is the prior ARGUE from the original mission framing
(structural-law-on-Handbook + three-axis-annotation-system design).
Its pattern cross-references and theoretical coherence section
remain valid for the infrastructure that work produced; the
plain-language argument is the *self-flagellating-TryHarder* version
the reframe replaces.

### Pattern cross-references

- **`writing-coherence/floating-formalism`** (one annotation in the seed set, sec 14). The mission's own structure has the inverse property — every framework cited (semilattice precedent, three-axis annotation system, flexiarg shape) is integrated with the work, not dropped. This is itself a verification that the diagnostic library applies recursively to its own meta-text.
- **`writing-coherence/scope-mush`** (three annotations in the seed set, sections 24, 25, 27). The mission's *Scope in / Scope out* sections are deliberately structured to avoid the same pattern — each scope item is a committed promise, not a coverage gesture.
- **`pattern-coherence/internal-coherence`** (futon3 library). The semilattice's annotation → concern → cluster → priority-queue chain has the internal-coherence property: each entity's existence is justified by a relation it participates in, and the priority-queue tiers map back to the concerns they discharge.
- **`peeragogy/pattern-language`** (mined this mission). The mission applies its own flexiarg recursively: the rewrite plan exhibits cross-references between flexiarg library, sidecar, and queue, demonstrating the language property the Handbook's *Patterns* chapter claims for itself but does not exhibit. Pattern-self-application as a peeragogy invariant is enacted, not asserted.
- **`peeragogy/wrapper`** (mined this mission). The .edn queue *is* a wrapper artefact for the annotation set: a maintained overview that lets a contributor see project state at a glance, without reading every annotation in sequence.
- **`peeragogy/discerning-a-pattern`** (mined this mission). The semilattice's *concerns* node is precisely the discernment work: noticing repetitions across the annotation set (e.g. "5 chapters open with meta-lede"; "7 annotations cluster on pattern-self-application"), naming them carefully, hedging interpretation until the cluster is robust.

### Theoretical coherence

The mission serves the IDENTIFY-phase claim that the Peeragogy Handbook's near-rewrite is a contained microcosm of structural-law work. The semilattice is the structural-law artefact; the three-axis annotation system is the evidence layer feeding it; the pattern library is the vocabulary that lets the queue's concerns be specific rather than ad hoc. Each layer ties to a piece of futon theory (patterns as common notions; semilattice over tree; pattern-language as language not catalogue). The theory has not shifted; the mission is a load-bearing instance of it on a specific corpus.

### Trade-offs

- **Sidecar-as-source-of-truth vs. queue-as-source-of-truth.** Decided in favour of sidecar; the queue is derived. Cost: when annotations change, the queue must be re-derived (or hand-edited and re-checked against I6). Benefit: the wikibooks merge contract is unchanged; future imports can run without queue-aware tooling.
- **Single library `peeragogy/` vs. `peeragogy-praxis/` to signal that these are process patterns.** Decided in favour of single library; the existing siblings (`writing-coherence/`, `pattern-coherence/`) don't carry the praxis distinction and adding a third top-level kind would over-classify. Cost: a reader skimming the library directory cannot tell at a glance that peeragogy patterns are different in kind. Benefit: simpler library navigation; the `@style process-pattern` field in each flexiarg carries the distinction.
- **Three axes vs. more.** Decided to stop at three (writing-coherence, critique, peeragogy). A fourth axis (e.g. `annotation/staleness`) was considered for the link-rot / platform-currency findings, but `annotation/critique` is the established home for non-pattern-backed concerns, and folding staleness into critique keeps the axis count manageable. Cost: the *concern* dimension in the queue (`:link-rot`, `:platform-currency`) has to do work that an annotation label could have done. Benefit: the renderer cond cascade stays small; existing reading discipline is preserved.

### Plain-language argument

The Peeragogy Handbook is mostly from 2014–2016 and needs a near-complete rewrite. With only ten free-floating comments on it from a first pass, there's no way to tell which problems are systemic and which are chapter-local — the comments don't aggregate. So we built three things: a small library of named patterns about peer-learning practice (mined from the book's own *Patterns* chapter), an expanded annotation set across three axes (writing-style, content-currency, and pattern-application), and a structured "revision queue" file that links annotations into causes, work-units, and a priority order for the rewrite. The queue lets us see clusters the comments alone would never have shown — for instance, that seven separate places in the book invoke the book's own patterns without naming them, all fixable in one editorial pass. The rewrite then proceeds against the queue in priority order: settle the cross-cutting decisions first, run the easy editorial sweeps next, and only then rewrite individual chapters.

## 5. VERIFY (revised 2026-04-29 — DERIVE design verified against posterior corpus)

The full VERIFY artifact for the revised DERIVE is at
`futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/00-verify-derive-against-posterior.md`.
It traces each load-bearing claim in DERIVE §3 to specific quote
ground in the podcast posterior dataset (transcripts + 65 sidecar
annotations + per-episode notes), distinguishes corpus-grounded
claims from mission-context (Joe-autobiographical / IDENTIFY-stage /
external) claims, runs a 200-word elevator-pitch spike to confirm the
essay-spine coheres, and checks each IDENTIFY completion criterion
against the DERIVE plan.

### Headline findings

- **Carrier-shape claims STRONG.** The chapter-rewrite stall mechanism (item A.1) traces to three independent recurrences across eps 4, 6, 9 (Joe naming v4-wish-without-shipping). The carrier-(b)/(e) viability (item A.3) traces to multiple in-corpus instances of canonical-text-independent practices (OGM, EmacsConf, Emacs Workshops, ACMI, Cool Laboratory).
- **Essay-spine claims STRONG.** Item 2.8 (peeragogy as worldview/metaphor shift) traces to direct outside-voice quote from Sohail Inayatullah (ep 13). Item 2.7 (timeless-practice) traces to **10 independent voicings across 8 episodes from at least 7 different speakers** — DERIVE conservatively claimed "7+", actual count is higher.
- **Method-note claims correctly distinguished as mission-context.** Joe's PhD thesis lineage, PlanetMath retrospective, and UKRN-S Agile Coach hypothetical are NOT corpus findings; they are Joe's autobiographical/forward-looking framings. The method-note must attribute these to Joe directly, not present as corpus-derived.
- **One synthesis-inference flagged for tightening.** Claim "Handbook is mostly layer-2" (DERIVE A.2) is not a direct quote from the corpus; it's a reasonable inference from Sohail's general layer-1-2 observation plus Joe's ep 6 acknowledgement that the Handbook is criticised as confusing. Tighten in essay to *"the Handbook would benefit from making its worldview/myth content more legible"* rather than the stronger *"the Handbook IS only layer-2."*
- **Four overstatements explicitly flagged.** Section E of the verify artifact lists tempting-but-not-corpus-supported claims to avoid: "peeragogy is a 21st-century sangha" (too strong; analogy is one frame among several), "the Handbook stalled because it was wrong" (too strong; corpus supports stall-as-structural, not content-critique), "peeragogy is the only practice doing X" (clearly false), "carrier (a) is impossible" (DERIVE says structurally risky, not impossible).

### Spike (per VERIFY discipline)

A 200-word elevator-pitch version of the essay was drafted in real
time as a smallest-possible-prototype to validate the riskiest DERIVE
commitment (that the essay's spine can be cohered into a single
articulation in ~3,000 words). The spike coheres; each sentence has
corpus-quote ground (verified per sections A and B of the verify
artifact). **Essay-spine confirmed achievable.**

### Completion-criteria pre-check

All 8 IDENTIFY completion criteria are addressed by DERIVE; 4 are
already met by work landed this week (synthesis exists; carrier-shape
decided; reviewer report repositioned; proof-of-concept primitives
documented), 4 will be produced by Artifacts 1–3 + handoff-plan
execution (working answer, handoff plan, carrier instance,
proof-of-pipeline). **No criterion left unaddressed.**

### Decision log

- **DERIVE design VERIFIED.** Load-bearing corpus-grounded claims trace to specific quotes (often multiple). Mission-context claims correctly attributed.
- **One DERIVE-text revision recommended:** the *"mostly-layer-2 framing"* claim should be tightened in the essay to *"the Handbook would benefit from making its worldview/myth content more legible."* (Optional — the DERIVE text can stand; the tightening is for the essay's articulation, not the design.)
- **DERIVE is ready for INSTANTIATE.** Begin with Artifact 1 (the essay).

---

## 5.prior VERIFY (2026-04-27 — superseded by §5 above; preserved as MAP-stage evidence)

The text below is the prior VERIFY work for the structural-law /
annotation-axes design from the original IDENTIFY framing. The
verification it performed (semilattice integrity, sidecar paren-check,
live-Emacs round-trip, renderer reload) remains valid for the
infrastructure that work produced; the verification of the *DERIVE
design choices* is now superseded by §5 above against the
podcast-posterior corpus.

### Structural verification

- **Semilattice integrity.** Cross-checked via `clojure -M -e '...'`: 0 missing annotation references, 0 orphan annotations, 0 missing cluster definitions in the priority-queue. (2026-04-27.)
- **Sidecar paren-check.** Passes (`dev/check-parens.sh`).
- **Live-Emacs round-trip.** Browser query against the merged manifest returns 25 annotations; section-level query confirms new annotations land on their sections (chapter 24 verified by spot-check).
- **Renderer reload.** `load-file` of modified `arxana-browser-essays.el` succeeds in the running Emacs without restart; manifest still reports 25 annotations after reload.

### Spike

Not needed at this phase. The renderer change is a 12-line addition; the sidecar shape was already well-defined; the .edn structure was modeled on a working precedent (`holistic-argument-aif2.edn`).

### Completion-criteria pre-check

| Criterion | Status |
|---|---|
| 1. Three-axis annotation density | partial — 11 of 31 sections covered, sweep continuing |
| 2. Pattern library coverage of *Patterns* chapter | ✅ |
| 3. Renderer wiring for third axis | ✅ |
| 4. Semilattice queue exists and integrity holds | ✅ (seed pass) |
| 5. Each annotation has ≥2 semilattice parents | ✅ for all 25 seed annotations |
| 6. Acyclic prerequisite DAG | ✅ by inspection |
| 7. Tier-0 cluster resolutions on file | pending Joe |
| 8. At least one tier-2 chapter rewrite end-to-end | pending |
| 9. Reproducibility | partial — this mission doc + queue file provide the recipe; not yet tested by a fresh agent |

### Decision log

- **2026-04-27.** Decided to drop `annotation/peeragogy-coherence` in favour of `annotation/peeragogy` (Joe direction). The peeragogy axis flags places where the book could *apply* its own patterns more fully; the concern is not specifically coherence and the broader label respects that.
- **2026-04-27.** Decided 🗪 (U+1F5EA) for the peeragogy marker. Glyph not present in default frame font (`BitstreamVeraSansMono Nerd Font`); registered `Noto Sans Symbols2` for the codepoint via `set-fontset-font`.
- **2026-04-27.** Decided to defer mining of *Antipatterns* and *Patterns and Heuristics* chapters until after the first sweep round closes, on the principle that the existing seed sweep should re-derive the queue before introducing new pattern axes.

## 6. INSTANTIATE (revised 2026-04-29 — handoff-shaped, peeragogy-applied-to-self at the production layer)

Joe direction (2026-04-29, after ARGUE reframe): the typical futon-mission INSTANTIATE shape — *Claude does the work and Joe says OK* — is the wrong shape for this mission. *"If it's only me doing the work then it isn't really peeragogy."* Solo-driving the rewrite of a book about peer learning would itself be an instance of `collaboration-coherence/isolation`. The discipline applies to the practice's own meta-work as much as to its surface work.

INSTANTIATE therefore produces **a set of well-scoped invitations** rather than three finished artifacts handed in. **Claude+Joe role: technical editing by default** — preparing seed material, scaffolding the handoffs, supporting coauthors when they engage, integrating contributions into the artifacts. Joe explicitly leaves room to do more if no one picks up; the default is to invite first.

This makes INSTANTIATE itself a peeragogy-applied-to-self move: the act of inviting *is* the practice the artifacts describe.

### Caveat — invitations, not assignments

Named coauthors below have **not** said yes. Each is a candidate, chosen for fit between the candidate's actual work and the specific ask. The first move for each is the invitation; what each contributor produces (and whether they engage at all) shapes the rest. Some invitations will produce nothing; some will produce more than asked. Both are fine.

### Shape of each handoff package

Each invitation includes:

1. **What** — the specific scoped piece (one section, one flexiarg, a co-authoring role, a review role).
2. **Why this person** — fit between the ask and their work, in their voice not ours.
3. **What's already in place** — links to the rubric, posterior notes, podcast transcripts, annotations, and any seed draft.
4. **Smallest viable contribution** — what counts as done at the floor.
5. **Larger ask** — what the invitation opens out to if the contributor wants more.
6. **How to send** — channel (email, mailing list, Forum thread, direct message), framing, sender (Joe, Charlie, Charlotte, etc.).
7. **What we'll do as technical editors** — integrating, preserving voice, version control, attribution, etc.

The handoff packages are themselves a set of artifacts — a *coordination-layer deliverable* of INSTANTIATE alongside the four content artifacts from DERIVE (Artifacts 1–4 below).

### Per-artifact handoff sketch

**Artifact 1 — Essay "What is peeragogy in 2026"**

- *Lead-author candidates (one of, by mutual fit):* Joe Corneli (default), Charlotte Pierce (already produces project material), Charlie Danoff (hosts multiple episodes; has written for the project). One lead per essay, not three.
- *Section-author candidates (specific sections):*
  - The worldview/metaphor-shift spine — invite **Sohail Inayatullah** or **Ivana Milojević** (ep 13). Their CLA-of-peeragogy framing is the spine; an invitation to write 300–500 words from their own vantage would land naturally.
  - The historical-origin section — invite **Howard Rheingold** (eps 4 + 9). Ten-year retrospective from the convener.
  - The practice-supporting-infrastructure section — invite **Pete Kaminski** (ep 7). OGM Thursday calls as worked example.
  - The ethics-and-integrity section — invite **David Preston** (ep 12). The *integrity-is-integration* line; the *intellectual-sharecroppers* critique.
  - The territorial / reciprocal-participation section — invite **Paola Ricaurte** (ep 5). Body-as-first-territory; data-coloniality framing.
- *Smallest viable*: lead author drafts the spine + assembles section contributions as they arrive (or fills gaps). Larger ask: each section author writes their own ~500 words.
- *Claude+Joe role*: prepare a section-by-section seed outline drawn from the synthesis + posterior notes; technical-edit each section contribution; integrate into the essay; manage attribution.

**Artifact 2 — Next-wave flexiargs in `futon3/library/peeragogy/`**

- One flexiarg per pattern, with the speaker who voiced it as natural primary author:
  - `calling-in` — invite **Richard Butler** (ep 10, DEI consultant). His operational stance; his 550-person-workshop story is the worked example.
  - `check-in` — invite **Leo Vivier** (ep 11, EmacsConf). His operational discipline; the bookend pair with PAR.
  - `brave-safe-space` — invite **Richard Butler** (paired with calling-in).
  - `causal-layered-analysis` — invite **Sohail Inayatullah** or **Ivana Milojević** (ep 13). Their methodology; their book CLA 3.0 is forthcoming.
  - `listening-as-discipline` — possibly **Charlotte Pierce** (ep 7, who named it from a newcomer vantage point) or invite an OGM regular.
  - `register-moderation` (working name for "toe down the level") — invite **Leo Vivier** (paired with check-in).
- *Smallest viable*: Claude+Joe seed-draft 1–2 (e.g. *check-in* and *register-moderation* paired, since Leo is already engaged through Joe) using the corpus quotes from the relevant annotations. Each flexiarg is small (~200 lines using the established format) so a coauthor's revision pass is feasible.
- *Claude+Joe role*: provide the flexiarg-shape template, the corpus-quote ground (already in the annotations), and integration into the library; coauthor refines, adds compositions, names absence-signals from their own practice.

**Artifact 3 — Method note "Retrospective work as MAP"**

- *Lead author*: **Joe Corneli** (PhD-thesis lineage; M-live-geometric-stack work; UKRN-S engagement potential). Joe is the natural primary because the lineage is autobiographical.
- *Co-author candidate*: someone from the **UKRN-S working-paper team** if they would find the method useful for UKRN-S's own retrospective work. The co-authorship makes the method note immediately useful externally rather than purely peeragogy-internal.
- *Reviewer candidates*: **Pete Kaminski** (ep 7, OGM does similar work at smaller scale); a futures-studies practitioner from Sohail's circle; possibly someone from the UKRN-S network who has run quarterly retrospectives.
- *Smallest viable*: Joe drafts; Claude technical-edits; ships at `futon4/holes/labs/M-peeragogy-rewrite/retrospective-as-map-method.md` with a note that co-authorship is open.
- *Claude+Joe role*: Joe authors; Claude assists with structure, citation discipline, integration with the lineage references (CORNELI-thesis, M-live-geometric-stack, PARs in Peeragogy, PlanetMath retrospective).

**Artifact 4 — Mini-handbook "Futonic Missions for Collectives" (~48 pages)**

*Origin (2026-05-02).* Joe's sister diagnosed a failure mode in agentic-coding teams at her company: *"people build lots of stuff but a lot of it is never used."* Joe pointed her at `futon4/holes/mission-lifecycle.md`, but the act of recommending it surfaced a structural gap: the lifecycle as written assumes a single locus of authority (one operator + one or more agents). Multi-operator teams need a ported version with distributed phase-authorship, phase-boundary consent protocols, an *adoption invariant* in INSTANTIATE that explicitly rules out build-without-adoption, and pattern-grounding to the peeragogy coordination primitives (heartbeat, wrapper, roles, moderation, newcomer, polling-for-ideas). The retrospective-as-MAP method note (Artifact 3) §3 Move 7 and §6 already pre-shadow this gap; the mini-handbook would fill it.

*Scope of the artifact.* A printed-format mini-handbook of approximately 48 pages, suitable for a small team to read together (book-club cadence) and apply. Covers: the seven phases ported to multi-operator context; the role decomposition per phase; phase-boundary consent protocols (quorum, dissent-recording, the Quaker-meeting move); the adoption invariant and how INSTANTIATE closes; how peeragogy patterns (heartbeat, wrapper, roles, moderation, newcomer, polling-for-ideas) are threaded through the lifecycle; how `collaboration-coherence` antipatterns (isolation, magical-thinking, navel-gazing, stasis, weak-tie-conflation, messy-with-lurkers, misunderstanding-power-laws) appear as failure modes; worked examples (case studies, multi-team illustrations); a translation appendix for adjacent disciplines (Scrum, Sociocracy 3.0, Holacracy, Spotify model, Quaker process — what carries across, what doesn't).

*Discipline — careful writing, not whiz-off.* Joe's explicit direction (2026-05-02): *"we can't just whiz it off, we need to write it carefully."* The mini-handbook is itself a candidate paragogy-revenue Layer-3 surface (per `futon5a/holes/stories/globe4-paragogy-revenue.md`); shipping a low-quality draft would burn the candidate. The writing discipline:

1. **Apply retrospective-as-MAP to this artifact.** `mission-lifecycle.md` is the prior; the corpus is collected experience of multi-operator teams running mission-shaped work (the sister's company; UKRN-S working-paper team if engaged; futon-stack's own multi-repo coordination history; adjacent-discipline literatures). Same one-substantive-promotion-per-source discipline.
2. **Multiple drafts with named reviewers.** v0 (Joe + Claude); v1 (one team-of-practice reviewer); v2 (one academic reviewer for translation-appendix discipline); v3 (one external-team pilot read-and-apply); v4 (final revisions). Each version's reviewer is named, not anonymous.
3. **Worked examples are not optional.** Theory-only would betray the whole point. Each phase chapter ends with a concrete worked example from real multi-operator practice — preferably from at least three different domains (software team / academic project / community of practice).
4. **Peer-tested before shipping.** At least one external multi-operator team applies a draft to one of their own missions and reports back; their report becomes a published appendix or is integrated into the worked-examples chapter.
5. **Voice-and-form for the printed object.** ~48 pages is a real artifact constraint: chapters fit, examples breathe, the reader can carry it. Not a web-doc dump compressed to print. Format matters; a designer's pass at v3 or v4 is non-optional.
6. **Time horizon.** Months, not weeks. The mission's other artifacts can ship before this one is done; this one trails the rest by design.

*Lead-author candidates (one of, by mutual fit):* **Joe Corneli** (default; deepest familiarity with both the futonic discipline and the peeragogy patterns). **Charlie Danoff** (paragogy lineage; prior co-authoring with Joe on the original Peeragogy Handbook). **Charlotte Pierce** (project material expertise; OGM facilitation experience).

*Co-author candidates (specific chapters or sections):*
- The *adoption invariant* and *team-context failure mode* chapters — invite **Joe's sister** (named contributor pending consent) or another contributor with current agentic-coding-team management experience. Their direct experience is what makes the chapter credible to similar teams.
- The *phase-boundary consent protocols* chapter — invite a **Quaker-process** practitioner or a **Sociocracy 3.0** practitioner. The futonic ARGUE phase has structural relatives in both traditions; an outside voice can name the relatives without smuggling them in.
- The *role decomposition per phase* chapter — invite a **Holacracy** practitioner or someone with deep **Spotify-model** experience. The role-vs-person distinction is sharp in Holacracy in a way the futonic lifecycle has not yet articulated.
- The *worked examples* chapter — invite teams from at least three domains (a software team using mission-lifecycle.md; an academic-collective using it for a paper; a community-of-practice using it for a coordination problem). One worked example per domain; team's voice preserved; Claude+Joe technical-edit only.
- The *translation appendix* — invite an academic reviewer with comparative-process expertise, possibly **Sohail Inayatullah** (CLA's four layers map onto the worldview/method/structure/litany distinction the appendix needs to draw); possibly someone from the **Open Source Governance** literature.
- The *retrospective application* chapter — cross-reference Artifact 3 (method note); same lead-author (Joe) for consistency, or a co-author chapter pair.

*Smallest viable*: Joe drafts v0 over (estimated) 2–3 months as a working document at `futon4/holes/labs/M-peeragogy-rewrite/futonic-missions-for-collectives-draft.md`; Claude technical-edits; ships v1 to first named reviewer.
*Larger ask*: each chapter has a named co-author; the printed version goes through a designer; a piloting team applies a draft and contributes a report.

*Claude+Joe role*: Joe is primary author; Claude assists with structure, citation discipline, integration with the lineage references (mission-lifecycle.md, retrospective-as-map-method.md, peeragogy patterns library, collaboration-coherence library), case-study extraction from the futon-stack's own multi-repo coordination history, and version-management of the multi-draft cycle.

*Connections within the mission and beyond.*
- Cross-link to **Artifact 3 (method note)**: the method note's §3 Move 7 (peer-organised INSTANTIATE) and §6 (substrate-light vs substrate-rich) and §7 (consulting application) become spec material for the mini-handbook. The method note describes the *retrospective* discipline; the mini-handbook describes the *forward-going* discipline. Both apply the same seven phases to different temporal orientations.
- Cross-link to **`futon4/holes/mission-lifecycle.md`** (the single-operator version): the mini-handbook's first chapter explicitly references the single-operator version as the substrate it ports from. mission-lifecycle.md stays terse and unchanged; the mini-handbook is the ported version, sibling-not-replacement.
- Cross-link to **`futon3/library/peeragogy/`** and **`futon3/library/collaboration-coherence/`**: each phase's role-decomposition cites the relevant peeragogy patterns; each common failure mode cites the relevant collaboration-coherence antipattern.
- Cross-link to **`futon5a/holes/stories/globe4-paragogy-revenue.md`**: the mini-handbook is a candidate Layer-3 (capability-applied) probe surface — saleable as a teamwork-discipline framework to organisations facing the build-without-adoption failure mode. Whether anyone pays for it is empirical; the careful-writing discipline protects the option.

### Meta-invitation: the mailing-list message and the Wikibooks talk page

Two broadcast surfaces complement the per-person invitations:

- **Mailing-list message** (the appropriately-scoped handoff drafted earlier at `futon4/holes/labs/M-peeragogy-rewrite/peeragogy-mailing-list-handoff-2026-04-29.md`). Now revised to lead with the *method-applied-to-self* finding (per ARGUE reframe), not with *here is the reviewer report*. The mailing-list message becomes the broad invitation that surfaces who is interested in any of the three artifacts. Smallest ask: read the essay (when shipped). Larger ask: pick up one of the open invitations.
- **Wikibooks talk page** post — companion notes about the Handbook itself with explicit pointers to the rubric and the surgical-edit closures landed 2026-04-29 (revids 4633108–4633113). Posted via the bot. Lower coordination cost than the mailing list; reaches a different audience (Wikibooks editors who watch Handbook talk pages).

### Coordination scaffolding (the meta-artifact)

The handoff packages need somewhere to live and a way to track state. Suggested:

- A simple Forum thread or shared doc per invitation, with: ask, contributor, status (pending / declined / drafting / submitted / integrated), seed material links, deadline (soft), Claude+Joe technical-edit notes.
- A coordination overview (the *Wrapper* artifact in peeragogy terms) at `futon4/holes/labs/M-peeragogy-rewrite/coauthor-handoffs/index.md` listing all open invitations, their state, and the latest draft of each artifact-in-progress.
- Forum + email + direct-message channels per contributor's preference; Forum default for transparency.

### Sequencing

1. **Seed drafts first** — Claude+Joe produce a 200-word elevator-pitch version of the essay (the spike from VERIFY exists already), one or two seed flexiargs (e.g. *check-in* + *register-moderation* from Leo, since the corpus material is rich), and a method-note outline. These are *not* finished; they are the material the invitations point at.
2. **Invitations sent** — per-person, with the seed material attached and the smallest-viable / larger-ask spelled out. Joe sends; Claude can draft.
3. **Mailing list + Wikibooks talk page** sent ~1 week later, after some per-person responses are in. The broadcast invitations are stronger when there's already some traction visible.
4. **Integration as contributions arrive** — Claude+Joe technical-edit, integrate, attribute, ship.
5. **Joe-can-be-won-over fallback** — if specific invitations decline or go unanswered for a soft deadline, Joe can pick up the work himself (with Claude scaffolding) for items that matter to ship; for items that don't, the ones that get picked up are what ships.

### Pattern grounding

The handoff-shaped INSTANTIATE itself uses several patterns from the present library:

- `peeragogy/wrapper` — the coordination overview at `coauthor-handoffs/index.md` is a Wrapper artifact.
- `peeragogy/roles` — each handoff package names roles (lead author, section author, reviewer, technical editor) explicitly rather than leaving them to ambiguity.
- `peeragogy/newcomer` — the smallest-viable contribution per invitation is the Newcomer-pattern entry point; the larger ask is the deeper-engagement option.
- `peeragogy/curating-not-experting` (mined this mission) — Claude+Joe explicitly take a curatorial-not-expert stance: bringing voices into the artifacts, not authoring the artifacts and inviting endorsement.
- `peeragogy/reciprocal-participation` (mined this mission) — each contributor receives attribution, the artifact they helped produce, and (for outside invitations like Sohail) an explicit acknowledgement of how their work helped reshape peeragogy's self-understanding.
- `peeragogy/stewardship-succession` (mined this mission) — the handoff is *itself* stewardship-succession in operation: the practice's next stewards are the ones who pick up the invitations.

### Decision log

- **Decision (2026-04-29):** INSTANTIATE produces invitations, not finished artifacts. Claude+Joe role is technical-editing by default. Joe can be won over to do more if specific invitations decline.
- **Caveat:** named coauthors have not said yes. The list above is candidates-by-fit, not commitments. Some invitations will be ignored or declined; the artifacts that ship are the ones that find willing contributors (or that Joe decides to drive himself).
- **Decision (2026-05-02):** Added Artifact 4 — *Mini-handbook "Futonic Missions for Collectives" (~48 pages)*. Origin: Joe's sister diagnosed the *build-lots-use-little* failure mode in agentic-coding teams; the existing `mission-lifecycle.md` is single-operator-shaped and needs porting to a social substrate. Artifact 4's deliberate writing discipline (multiple drafts, named reviewers, peer-tested by an external multi-operator team, designer pass on the printed format, months-not-weeks horizon) is itself a peeragogy-applied-to-self move; the mini-handbook on collective discipline must itself be produced by collective discipline. Joe explicitly directed: *"we can't just whiz it off, we need to write it carefully."*

---

## 6.prior INSTANTIATE (2026-04-27 — superseded by §6 above; preserved as MAP-stage evidence)

The text below is the prior INSTANTIATE plan from the original mission framing (tier-0 blocker decisions + chapter-rewrite ordering). Its substance was overtaken by the carrier-shape decision in DERIVE §3 (revised) — chapter rewrites are no longer the primary INSTANTIATE work.

Not yet entered. Tier-0 blocker decisions are required before INSTANTIATE can begin (per completion-criterion 7). The expected sequence on entry is:

1. Resolve `:C1-importer-fix`, `:C2a-link-rot-policy`, `:C-platform-currency-policy` (cross-cutting decisions / one-time code fix).
2. Run `:C4-pattern-cross-reference-sweep` (highest-yield tier-1 cluster, no prerequisites).
3. Run remaining tier-1 sweeps in parallel.
4. Pick one tier-2 chapter rewrite as the proof-of-pipeline (likely `:C8-chapter-24-full-rewrite`).
5. Continue tier-2 rewrites in queue order, refreshing the queue after each chapter.

Each round closes with a new round of MAP (does the sweep find new concerns?) and DERIVE (does the cluster structure need revision?).

## 7. DOCUMENT

Not yet entered. Expected outputs:

- A docbook entry on the three-axis annotation system + `annotation/peeragogy` marker, generalising beyond the Peeragogy Handbook to any wikibooks-imported corpus.
- A docbook entry on the semilattice revision-queue pattern, citing the precedents (`holistic-argument-aif2.edn`, `stack-stereolithography-priority-queue.edn`) and this mission's queue as the third instance.
- A docbook entry on the `peeragogy/` flexiarg library — the 12 mined patterns, with cross-references into the writing-coherence and pattern-coherence libraries.

## Checkpoints

### 2026-04-27 — seed lattice in place

- 12 flexiargs at `futon3/library/peeragogy/` mined from chapter 21.
- 25 annotations in the sidecar (10 Codex + 15 this mission), covering 11 of 31 sections, across three axes.
- `annotation/peeragogy` rendered with marker 🗪 in both renderers; live-loaded.
- Semilattice queue at `peeragogy-handbook-revision-queue.edn` — 6 concerns, 13 clusters, 3-tier priority queue. Lattice integrity check passes.
- Mission status: MAP → DERIVE iterating; next move is the 17-chapter sweep, after which MAP and DERIVE both revise.

### 2026-04-27 (later) — round-2 sweep complete; lattice revised to v2

**Annotation density**

- 25 → **77 annotations** (3.1×).
- Section coverage: 11/31 → **31/31 (full)**.
- Distribution: 28 critique (was 5) + 27 peeragogy (was 7) + 22 writing-coherence (was 13).
- Per-section average: now 2.5 annotations/section.

**Concerns evolved**

- 6 → **10 concerns**. New: `:placeholder-content` (chapter 30), `:posthumous-author` (chapters 18, 30), `:pattern-mining-candidate` (chapters 22, 26, 17/28/29), `:pattern-language-exhibited` (chapters 17, 20, 26, 28).
- `:import-quality` broadened: now covers vandalism (ch 23, 29), MediaWiki table syntax (ch 30), MediaWiki heading syntax (ch 23), missing leading character (ch 28), broken image refs (ch 19, 28), in addition to the seed-pass mojibake / REDIRECT / fragment findings.
- `:platform-currency` confirmed widespread (10 chapters) and worse than seed suggested. Now a tier-0 sweep, not just a policy decision.
- `:pattern-self-application` is the dominant peeragogy-axis finding (24 annotations, 14 chapters). Single editorial pass (`C4`) addresses all of them.

**Surprises that changed scope**

- Chapter 23 contains visible wiki vandalism in the imported source — keyboard-mash sequences (`jsdijs8dhj bnjdcujsj`, `KARTI K KUMAR AMAR BHAI LALTYUNC...`). Source-side, not import-side. Required adding `C-source-fidelity-audit` cluster.
- Chapter 30 is structurally an *unfilled form*: every author's Affiliation is "Former" and every Quote is "Welcomes any criticism or praise". These are placeholders that no one ever replaced. Required adding `:placeholder-content` concern and `C-meet-authors-rewrite` cluster.
- Chapter 13 has a 2026-02-07 inserted "Flaws of the Smart Workplace" edit sitting next to 2014 main text, surfacing a temporal-layering convention need that wasn't visible in the seed pass. New cluster: `C-temporal-layering-convention`.
- Chapter 20 (Patterns and Heuristics) is the strongest exhibit of the pattern language property in the entire book — *should be preserved, not edited*. Surfaced the need for a positive-findings track in the queue (`:pattern-language-exhibited` concern + `C-preserve-positive-findings` cluster). The semilattice now distinguishes preserve from fix.

**Cluster DAG revised**

- 13 → **30 clusters**. New tier-2 chapter rewrites: ch 6 (k-12), ch 11 (mooc), ch 13 (workscape), ch 26 (real-time-meetings), ch 29 (style-guide), ch 30 (meet-authors). New tier-3 mining clusters: antipatterns, PAR, meta-application, use-case candidates.
- Tier-0 blockers still 3, but with broader scope (importer fix now addresses 8 annotations across 7 chapters, was 3 across 3).
- Cluster `:C-preserve-positive-findings` is structurally unusual: a discipline, not a task. It exists so the rewrite knows what to keep.

**Lattice integrity**

- Cross-check (`clojure -M -e '...'`, 2026-04-27) returned `:annotations 77 :clusters 30 :concerns 10 :missing-from-clusters 0 :orphan-annotations 0 :missing-cluster-defs #{}`. All invariants I1–I3 hold. I4 (acyclic prerequisite DAG) holds by inspection.

**Status: MAP → DERIVE second pass complete.** The queue is now usable as a roadmap. Next moves available in any order:

1. **Tier-0 work** (Joe decisions on link-rot policy, platform-currency policy; importer fix can run in parallel).
2. **Tier-1 cross-cutting sweeps** (`C4-pattern-cross-reference-sweep` is highest-yield; ~24 annotations, one editorial pass).
3. **Tier-3 mining** (Antipatterns chapter into a parallel `peeragogy-antipatterns/` library; PAR into a flexiarg). Surfaces new annotation axes that would justify a round 3 sweep.
4. **Tier-2 chapter rewrites** start once tier-0 is settled. `C-meet-authors-rewrite` is the most self-contained candidate; `C-chapter-26-rewrite` is the most platform-currency-driven. `C8-chapter-24-full-rewrite` remains the proof-of-pipeline candidate.

Round 3 (further density) is *not* recommended next. The recommendation is to start tier-3 mining first; the resulting flexiargs unlock new annotation axes, and round 3 then surfaces structurally different findings rather than more of the same.

### 2026-04-29 — round-3 mining + sweep complete; queue v3 lands

**New library and fourth annotation axis**

- Antipatterns chapter (sec 22) mined into `futon3/library/collaboration-coherence/` — 7 flexiargs: `isolation`, `magical-thinking`, `messy-with-lurkers`, `misunderstanding-power-laws`, `navel-gazing`, `stasis`, `weak-tie-conflation`.
- **Design decision**: separate library modeled on writing-coherence shape (failure-mode-named, positive `! conclusion`, FAILURE-MODES detection block), NOT folded into `peeragogy/`. Theoretical hinge: an antipattern is a partially adequate common notion — adequate enough to recognise the failure but not yet to act from; the completion move is the writing-coherence-shape positive pattern. Joe approved the separate-library decision and the name (`collaboration-coherence`, sibling to `agent` / `agency` / `coordination` / `pattern-coherence` / `writing-coherence`).
- **Fourth annotation axis added**: `annotation/collaboration-coherence`, marker 🗫 (Three Speech Bubbles, U+1F5EB), wired in both renderers in `arxana-browser-essays.el`. Same Noto Sans Symbols2 font registration covers both 🗪 and 🗫 since they're in the same Unicode range.

**Round-3 sweep**

- 11 new annotations across 11 chapters. **Total now 88** (88 = 22 writing-coherence + 28 critique + 27 peeragogy + 11 collaboration-coherence).
- Three of the new annotations are `:preserve` severity — places where the book *exhibits* the pattern's positive form. `C-preserve-positive-findings` cluster expanded from 4 to 7 entries.
- One compounded finding: chapter 21 (Patterns) has a meta-level isolation issue — the pattern entries themselves are presented in isolation, mirroring the antipattern they would name. Joins with the round-1 pattern-language self-reference annotation and the round-2 pattern-list-no-composition annotation in chapter 19.

**Queue v3 deltas**

- Annotations 77 → **88**. Concerns 10 → **11** (added `:collaboration-coherence-application`). Clusters 30 → **32** (added `C-collaboration-coherence-sweep`, `C-chapter-22-antipatterns-rewrite`).
- Lattice integrity holds: `:annotations 88 :clusters 32 :concerns 11 :orphan-annotations #{} :missing-cluster-defs #{}`. Sidecar ↔ queue agreement (I6) maintained.
- Tier-3 mining target (a) **closed** (Antipatterns library landed). (b) PAR and (c) meta-application remain pending as smaller follow-on items.

**Cross-axis joins now visible**

The four-axis queue exposes joins not visible in the three-axis seed:
- *Pattern-language is exhibited* (peeragogy axis, chapters 17 / 20 / 28) AND *exhibits the antipattern's positive form* (collaboration-coherence axis, chapters 9 / 14 / 26) — these are two facets of "what the book does right." Both feed `C-preserve-positive-findings`.
- *Pattern-self-application* (peeragogy axis, 14 chapters) AND *collaboration-coherence-application* (collaboration-coherence axis, 11 chapters) cluster on overlapping editorial-pass scope. Combined cross-reference sweep (C4 + C-collaboration-coherence-sweep) is the single highest-yield tier-1 work item.

**Queue is now ready to drive a reviewer report / issue queue end-to-end.** Each chapter-rewrite cluster has its annotation set, its prerequisites, its rationale, and its preservation discipline. The lattice supports queries by chapter, by concern, by cluster, by axis, by severity. INSTANTIATE can now begin against any tier of the priority queue without re-deriving design decisions from scratch.

### 2026-04-29 (later) — reviewer report rendered

[Reviewer report 2026-04-29](../labs/M-peeragogy-rewrite/peeragogy-handbook-reviewer-report-2026-04-29.md) — narrative rendering of the queue across all 31 sections, with executive summary leading on the 16 major-severity findings and the recommended overall revision strategy (tier-0 settle / tier-1 cross-cutting sweeps / tier-2 chapter rewrites / tier-3 remaining mining). 452 lines. Includes a preservation digest (seven `:preserve` items the rewrite must not lose) and a how-to-use guide that walks the per-chapter rewrite procedure.

This is the deliverable that closes the round-3 work. The queue plus the report together let a contributor walk into any chapter and know what to do without re-reading the lattice.

### 2026-04-29 (later) — tier-0 source-fidelity work begun

**Hybrid approach: GitHub source-replacement + manual-scrub-in-place**

Investigated `github.com/Peeragogy/Peeragogy.github.io` (org-mode source for peeragogy.org, last commit 2022-07-15). Findings: clean source for ~half the source-fidelity catastrophe chapters; structurally different from Wikibooks (org files don't 1:1-map to Wikibooks chapters); some chapters have no GitHub equivalent.

Strategy adopted: **don't write an org-mode importer or do a wholesale re-import** (cost outweighs benefit). Instead, hybrid per-chapter approach:

- **Source-replace** (chapters 3, 5): fetch the GitHub org file, Pandoc-convert to markdown, clean up org-specific artifacts (BACK TODO markers, sibling-file links), wrap with chapter title, drop into the data dir replacing the broken Wikibooks markdown. ~15 minutes per chapter.
- **Manual-scrub-in-place** (chapters 23, 28): the vandalism / truncation is local; the surrounding content is fine. Direct edit removes the bad string without disturbing the rest. ~2 minutes per chapter.

**Five annotations now carry `:closure` records:**

| Chapter | Annotation | Method |
|---|---|---|
| 3 | `cr:revised-intro-import-mojibake` | source-replacement-from-github |
| 3 | `cr:revised-intro-link-rot` | source-replacement-from-github (specific link gone with swap) |
| 5 | `cr:convening-import-fragment` | source-replacement-from-github |
| 23 | `cr:use-case-vandalism` | manual-scrub-in-place |
| 28 | `cr:in-action-truncated-opening` | manual-scrub-in-place |

Live manifest reports 88 total annotations, 5 closed. Renderer's `:closure` plumbing already shows ✓ prefix on closed annotation markers.

**Three tier-0 source-fidelity items remain (interactive):**

- Chapter 4 *How to use this Handbook* — empty redirect; no GitHub equivalent. Needs fresh writing.
- Chapter 29 *Style Guide* — vandalism opener + chapter's own self-violation issues; no GitHub equivalent. Needs editorial work.
- Chapter 30 *Meet the Authors* — MediaWiki table syntax + placeholder content; no GitHub equivalent. Needs content collection or alternative.

**Queue v4 deltas:**

- `:round-4-findings` block added with closure records and pending-item list.
- `:status` updated to `:tier-0-source-fidelity-partial`.
- `C1-importer-fix` scope reduces — most work is now done by the hybrid approach. Remaining importer concerns roll into the three pending chapters.

**Side note on annotation re-anchoring:** the chapter 3 / 5 source swap means several non-closed annotations on those chapters now have passages that don't exactly match the new content (e.g. `pg:revised-intro-newcomer-pattern-active` was anchored to `...edit this Wikibook!` which isn't in the new content, but the Newcomer pattern still applies to the new *Welcome!* section). The renderer's re-anchoring path handles this gracefully (`quality 'exact'` vs re-anchored); during the C4 cross-reference sweep these annotations will be either re-anchored or rewritten to match the new content. No closure is recorded for these — the underlying finding is still live.

**Status: tier-0 source-fidelity 5/8 closed; ready to proceed with the three interactive items.** Next move (Joe-driven): write fresh content for chapters 4, 29, 30, or settle one of the tier-0 policy decisions (link-rot, platform-currency, posthumous-author) to unblock tier-2 chapter rewrites.

### 2026-04-29 (later still) — tier-0 source-fidelity complete; queue v5 ready for coauthor handoff

**Three interactive tier-0 items closed**

| Ch | Issue | Method |
|---|---|---|
| 4 | Empty redirect | Pulled the canonical Wikibooks redirect-target page (`Peeragogy Handbook/How to use this Handbook`, Howard Rheingold author, last edited 2026-02-22 by Charles Jeffrey Danoff) via MediaWiki API; hand-converted to Markdown. The chapter now has its real content with provenance footnotes. |
| 29 | Vandalism opener | Removed the offending line. Editorial fill-in for the empty Overview heading deferred per Joe direction. |
| 30 | MediaWiki table + placeholders | Hand-converted MediaWiki table to Markdown (preserving the format Joe likes); fixed Charlotte Pierce typo; added † (1944–2015) for Jay Cross with Internet Time Group affiliation; seeded affiliations for Howard Rheingold and Charles Jeffrey Danoff. Remaining 23 affiliation cells and 25 quote cells marked *(?)* for coauthor fill-in. |

**All 7 major-severity source-fidelity catastrophes now resolved.** The book is structurally readable end-to-end: no mojibake, no vandalism, no MediaWiki syntax leaks, no truncations, no empty redirects, no raw placeholders without explicit markers. Total closures 5 → 9 across both rounds today.

**Affiliation-research finding**

Spot-checked six Wikibooks `User:` pages for affiliation data; none had biographical content. The canonical wiki source has the same `Former`/`Welcomes any criticism or praise` placeholders our import has. Affiliation collection is therefore not a source-fidelity issue but a content-collection task that requires per-author research or contributor self-report. Logged in `cr:meet-authors-placeholder-content` (still open).

**Queue v5 — ready for coauthor handoff**

Joe's framing: *"the queue is getting to be something I could hand off to coauthors, not just fix everything on my own (indeed, the latter would kind of be a failure mode for peeragogy)."* Solo-driving the rewrite of a book about peer learning would itself be an instance of `collaboration-coherence/isolation`. So the queue's job at this point is to make pick-up cheap.

New `:contributor-handoff` block in v5 organises open work by *coordination cost*:

- **Pick up without coordination** (cheapest entry, no alignment needed):
  - `chapter-30-affiliation-fillin` — per-row work, 5–15 minutes per contributor, addresses `cr:meet-authors-placeholder-content`
  - `link-rot-per-chapter` — per-chapter URL audit, 30–60 minutes
  - `pattern-cross-reference-per-chapter` — slot the named pattern into the prose where the annotation pinpoints it; 20–40 minutes per chapter; closes one annotation per cross-reference
- **Pick up with light coordination** (need a one-line decision first):
  - `tier-0-policy-decisions` — link-rot policy, platform-currency policy, temporal-layering convention
  - `sentence-level-rewrite-pass` — meta-lede / scope-mush / etc. sweeps
- **Pick up as named co-owner** (chapter-scale work):
  - `chapter-rewrite` — 10 chapters need substantive rewrites
  - `pattern-mining-extension` — PAR / meta-application / use-case candidates

Plus a `:claim-convention` field describing how a contributor marks an item picked up (`:claimed-by` + `:claim-date` on the annotation or cluster — wrapper-pattern applied to the queue itself), and a `:handoff-readiness-marker` listing the seven properties of the queue a new contributor can rely on.

**Mission status**

- Sidecar: 88 annotations, **9 closed** (live verified).
- Queue: v5, status `:tier-0-source-fidelity-complete-ready-for-coauthor-handoff`. Lattice integrity holds: `:annotations 88 :clusters 32 :concerns 11 :orphan-annotations 0 :missing-cluster-defs 0 :sidecar-vs-queue-mismatch 0`.
- Pattern libraries: writing-coherence (13), peeragogy (12), collaboration-coherence (7), pattern-coherence (6).
- Mission moves from solo INSTANTIATE preparation to multi-contributor INSTANTIATE-ready. The next iteration's unit of work is "a coauthor picks up an item from `:contributor-handoff` and works it." Solo work continues in parallel where Joe wants to drive specific items.

### 2026-04-29 (still later) — proof-of-concept landings; reframe to IDENTIFY-redux

**What landed today (proof-of-concept, not INSTANTIATE)**

- **Six surgical wikitext edits to Wikibooks** via `Arided@futon-peeragogy` bot (revids 4633108–4633113): ch 28 leading-W restored, ch 29 vandalism opener removed, ch 23 two vandalism strings removed, ch 30 typo + Jay Cross † marker. Proves the bot edit path works end-to-end with basetimestamp conflict-detection.
- **Round-trip discovery:** Markdown ↔ wikitext via Pandoc is lossy in both directions (drops File embeds, interwiki links, templates, MediaWiki tables; cosmetic noise on publish). Concluded that the Markdown-canonical model was untenable for any chapter containing live wikitext-native constructs.
- **Canonical-flip to wikitext:** Essays-side flipped to `.mw` as single source of truth. 31 chapters cached at `wikitext/<slug>.mw`; manifests' `:source-file` props point at `wikitext/<slug>.mw`; old `.md` imports moved to `import-archive/`; browser's `--source-file` resolver updated to construct `.mw` paths. Annotation sidecar migrated: 77/88 auto-anchored to wikitext spans; 11 dropped (mostly obsolete: import-side artifacts and vandalism we just cleared). Three new bb scripts in `futon4/scripts/`: `peeragogy-fetch-corpus.clj`, `peeragogy-migrate-annotations.clj`, `peeragogy-flip-to-mw.clj`. Plus the existing `peeragogy-publish.clj` extended with a `surgical-edit` mode.
- **Reviewer report restructured:** §1.1 now shows all 7 source-fidelity catastrophes as resolved (4 by surgical edits, 2 by canonical flip — the mojibake'd File embeds in ch 3 and the corrupted image-embed in ch 5 were Pandoc-import artifacts that don't exist in live wikitext, 1 by redirect-following in the fetch script — ch 4 now holds Howard Rheingold's actual prose); §2 dropped `C1-importer-fix` (descoped by the canonical flip); the §0 changelog material moved to Appendix A so the report opens at the executive summary.

**Why this is reframe, not INSTANTIATE-progress**

In conversation Joe noted: *"I think our changes are more of a sketch than an INSTANTIATION though. The current notes say that the book needs a near-complete rewrite, whereas what we have done today is a proof-of-concept that we can edit via Arxana / Claude Code / a wikibot."* Reading the prior IDENTIFY in light of today's capabilities revealed it had assumed a single answer ("near-complete rewrite") to a question that is actually open ("what is the right thing to build, given that we now can build it?"). The right move is back to IDENTIFY — this time with the proof-of-concept capability in hand and the option space actually visible.

**What the reframe preserves and what it opens**

- **Preserves:** the pattern libraries (12 + 7 mined patterns); the four-axis annotation system; the semilattice queue and reviewer report; the canonical-flip and surgical-edit infrastructure; the methodological commitment to a working answer for "what is peeragogy" before deciding the carrier shape.
- **Opens:** carrier-shape decision (rewrite / refactor-into-zines / hybrid); the dispassionate "why peeragogy matters in 2026" framing in ARGUE; the appropriately-scoped handoff plan (replacing the "blast the dormant Google Group" instinct); the "what other forms can peeragogy take" inventory (p4ng, podcasts, futon-stack-internal use).
- **Status:** prior IDENTIFY preserved as §1.prior; new IDENTIFY at §1 (DRAFT, awaiting Joe review). Mission status moved from `INSTANTIATE` back to `IDENTIFY-redux`. The 2026-04-27 → 2026-04-29 checkpoint trail stays intact as the genealogy of how we got here.

### 2026-04-29 (still later) — IDENTIFY shaped further; 2016 prior drafted as first MAP artifact

**Joe direction during IDENTIFY review.** Three substantive shapings landed in §1:

- **Pragmatist inquiry order.** *Why does peeragogy → what is peeragogy → why do these patterns matter (and matter-together).* Peirce/James/Dewey: meaning settled by practical bearings. Commits MAP to looking outward at the world, not just at the Handbook or the disk drive.
- **Practice-supporting-doctrine analogy.** Peeragogy may matter the way *sangha* matters in Buddhism, or *consciousness-raising* mattered in Marxism — collective practice that lets a doctrine become live. Pulls toward carrier shape (e): practice-supporting infrastructure, not a text. Carriers (a)/(b)/(c)/(d) become artifacts of the practice rather than its primary delivery channel.
- **2026-as-contingent-timing.** *"Imagine everyone was talking about tulips, or railroads, and you came along with a quiet invention of a completely different nature."* Peeragogy in 2026 is a different temperature in the same room as AI-discourse, not in conversation with it. A contingent timing argument distinct from the substantive "why does peeragogy" — both belong in ARGUE, not conflated.

**MAP sequencing committed.** Read what the Handbook says about itself FIRST → extract a rubric → apply outward. Order: handbook-self-claims → podcasts (semi-in/semi-out, locally transcribed by Joe; no Otter.AI / no third-party transcription) → adjacent literatures + current 2026 collectives. Rubric-from-inside is the bootstrap; outward survey applies it.

**First MAP artifact: the 2016 prior.** [`futon4/holes/labs/M-peeragogy-rewrite/peeragogy-2016-prior.md`](../labs/M-peeragogy-rewrite/peeragogy-2016-prior.md) — distillate of the Handbook's self-claims into a checkable rubric, organized around the Pragmatist questions (why does it / what is it / what does it care about / what is it not / what about its own form) plus a sixth section on the patterns and antipatterns themselves. Each claim cites its source chapter and carries a `### 2026 update: ___` placeholder for the posterior pass. The rubric is the yardstick MAP's outward survey applies to other surfaces (podcast transcripts, adjacent literatures, current 2026 collectives where the patterns recur named or unnamed). 7 sections, ~60 claim items, 5 chapters of the Handbook sampled (more to be cross-checked in the inward MAP step).

**Status:** IDENTIFY is settling; first MAP artifact landed. Next MAP work: cross-check the rubric against the 23 Handbook chapters not yet sampled (interior chapters can strengthen, weaken, or qualify the reflexive-chapter claims); pending podcast transcripts.
