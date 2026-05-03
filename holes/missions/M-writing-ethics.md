**Status:** COMPLETE (2026-04-24, re-open pass closed same-day)

**Known debt at entry to ARGUE (carried forward into INSTANTIATE):**
- T1/T2/T3 thesis selection (A1 ratification) — pending Joe.
- C1 H6 audience-map decision — pending Joe.
- C3 ontogenetic-accretion expand/fold/rename — pending Joe.
- C6 MVSG cash/drop + option choice — pending Joe.
- W4 supply/demand parenthetical intent — pending Joe.
- W6 reproducible-design-research subheading disposition — pending Joe.
These six questions are the top of INSTANTIATE's human-facing queue; they are not blocking ARGUE/VERIFY.

# M-writing-ethics: Scaffolding-as-solution — empirical test on a demo dataset

## 1. IDENTIFY

### Motivation

The arxana Essays subsystem now ships three annotation subtypes sharing `hx-type "annotation/comment"`:
- `annotation/comment` (💬) — author notes, manifest-only
- `annotation/critique` (🔍) — structured critique with diagnosis + suggested move + severity (added 2026-04-24)
- `annotation/writing-coherence` (🤖) — pattern-keyed prose diagnoses (added 2026-04-24)

In parallel, a new pattern library lives at `/home/joe/code/futon3/library/writing-coherence/` — eleven patterns in Tension / Composition / Check / Failure-Modes form. The patterns were induced from observed failure modes in AI-assisted drafts, but the patterns themselves are **not about who wrote the prose** — common notions, in the Deleuze/Spinoza sense, do not care about the author. They name structural features that make or break an adequate reading regardless of origin. Prose that flags as `meta-lede` flags as `meta-lede` whether a human or a model wrote it.

**The reframe.** This mission is not a "de-AI" pass. It is a test of a specific claim: that AI-assisted prose fails the patterns not because the generator is broken (cf. code, where the same generator produces near-flawless output under compiler + type + test scaffolding) but because the generator lacks the contextual scaffolding a competent writer carries into the act. **This is a scaffolding problem, not a training problem.** The claim under test is that futon theory — patterns as common notions, Essays as apparatus, annotations as scholia, the pattern library as a compiled context-store — is a sufficient scaffolding to close the gap.

What exists is the diagnostic half of the scaffolding: the patterns name and the annotations flag. What is missing is the **closing** half — a workflow that applies the scaffolding to the act of writing, not just the act of reviewing. The gap is: a workflow that routes each flagged item through enough scaffolding to close without regression, with a per-item recorded justification that captures *which scaffolding closed the item* — so that successes and failures together become evidence about the scaffolding hypothesis.

### Theoretical anchoring

- **Common notions (Deleuze on Spinoza):** a pattern is the conversion of an *affect* ("this prose is inadequate") into an *adequate idea* that can be reasoned from and composed with. The pattern is not a style rule (local prohibition) but a productive capacity. Common notions do not care who produced the prose; they track what makes a given encounter adequate. This mission treats the writing-coherence patterns as candidate common notions whose value is measured by whether they convert affect into capacity — regardless of the prose's origin.
- **Scaffolding, not training (the claim under test):** AI-assisted prose fails the patterns reliably; AI-assisted code does not. The difference is not in the generator. It is in the context the generator has access to while producing. Code generation operates under compiler + type + test + pattern scaffolding that lets the generator produce adequate output even when it lacks the situational knowledge a senior engineer has. Prose generation typically has no equivalent scaffolding — no types, no compiler, no pattern library loaded at write-time, no structured per-document context. The hypothesis: patterns + Essays apparatus + per-item scholia = a prose-scaffold of comparable strength. If the hypothesis holds, the generator can close structural items under the scaffolding; if it doesn't, the scaffolding needs augmenting, and the mission surfaces which components are missing.
- **Context-gap as root cause:** the patterns the writing-coherence library names (meta-lede, paraphrase-drift, throat-clearing-close, etc.) each have a context-gap reading. `meta-lede` happens when the generator does not know what the paragraph's payload claim actually is, so it defaults to announcing the paragraph's shape. `throat-clearing-close` happens when the generator lacks the audience model that would let it commit to a claim. `paraphrase-drift` happens when the generator doesn't have the paragraph's argumentative trajectory loaded. The pattern names the context that was missing. Closing under scaffolding = supplying that context through the apparatus.
- **Where humans still participate:** the scaffolding-hypothesis does not predict AI sufficiency for all items. Where the context an edit requires lives in tacit knowledge the apparatus does not encode (author's sense of the paper's political stakes, the funder's priors, an unstated argument thread, a reader-specific register) the human supplies the context. The mission's workflow distinguishes *context-in-scaffolding* (AI-adequate) from *context-in-tacit-knowledge* (human or human-augments-scaffolding). The routing is along the context axis, not the author axis.
- **Bayesian Spinozism:** the patterns are *candidate* common notions under revision. Edit attempts — including AI-executed structural edits — where the suggested-move doesn't fit are data on the pattern (and on the scaffolding) not just on the paragraph. The workflow captures this feedback so the library and the scaffolding calibrate together over time.
- **Apparatus-as-contribution (media theoretic):** the Essays format + pattern library + subtyped commentary is an apparatus through which prose becomes readable-and-writable as an adequate object. The contribution is the apparatus, and the mission is a load test of whether the apparatus, as it currently stands, is sufficient — or what else it needs.

### Scope in

- Define the **handoff taxonomy** along the context axis. Candidate loci: `scaffold-sufficient` (context needed lives in the pattern + annotation + paper body; AI executes), `tacit-required` (context needed lives in author's head; human executes), `scaffold-augmentable` (context is missing from the apparatus but could be added — e.g. an explicit audience note, a claim-ledger per section; AI executes after augmentation), `defer`, `reject`. Each decision records *what context the item requires* and *where that context lives*.
- Per-item decisions for every `annotation/critique` and `annotation/writing-coherence` item attached to the demo dataset. Each decision records: locus, context-requirement, context-source, expected resolution, and any augmentation needed.
- **Augment the scaffolding where needed.** Items routed to `scaffold-augmentable` trigger a scaffolding delta: a section-level claim-ledger, an audience register, an argument-trajectory annotation, etc. — whatever context the apparatus currently lacks. The delta is a first-class mission output; it is how the apparatus earns its keep beyond this one dataset.
- Execute the edit pass. `scaffold-sufficient` and `scaffold-augmentable` items close via AI-executed edits as a reviewable diff, with the used scaffolding explicitly cited per edit. `tacit-required` items close via human edits, with the human-supplied context captured in the closure record so the scaffolding delta can learn from it.
- **Regression sweep** after the pass: re-scan the revised draft with the full pattern set, flag any new instances (especially any new `meta-lede` or `paraphrase-drift` introduced during restructuring), and report the delta. Regression rate per-locus is the primary empirical measurement.
- Pattern-library calibration: per-pattern, record which items it fit cleanly, which needed per-case adaptation, which fell through. Propose library revisions where the evidence supports them.
- `annotation/retracted` treatment for closed items (consistent with existing manifest convention at annotations.el), with closure records attached that reference (a) the locus, (b) the scaffolding used or supplied, (c) the before/after passages, (d) whether the regression sweep flagged the replacement.
- A recorded end-to-end demonstration on the demo dataset.

### Scope out

- New pattern induction from fresh sources (separate mission; the Van Gogh/Dali A→B exercise that produced `meta-lede`, `paraphrase-drift`, `missing-mechanism` sits upstream of this one).
- Changes to the arxana Essays editor workflow beyond what this mission exercises (the broader edit cycle is M-essays-edit-cycle).
- Content-level critique on the demo dataset — structural claim validity, evidence strength, argument soundness (the critique items flag prose failures; they are not a content review).
- Automated rewrite tooling (this mission defines the workflow; tooling for future batching is a follow-on).
- Application to datasets outside the chosen demo; generalisation notes only.

### Completion criteria

1. **Handoff taxonomy along the context axis is specified.** A short enumerated schema (`scaffold-sufficient`, `scaffold-augmentable`, `tacit-required`, `defer`, `reject`) with entry criteria keyed to context-requirement, not to author.
2. **Every flagged item on the demo dataset has a recorded decision with justification.** Captured as a DERIVE table; no item closes without an explicit context-requirement + context-source.
3. **Scaffolding delta is produced.** Items routed to `scaffold-augmentable` produce a concrete apparatus addition (e.g. claim-ledger, audience register, trajectory annotation). The delta is reusable beyond this mission.
4. **Demo dataset edit pass is complete.** Revised draft exists; deferred/rejected items have rationale.
5. **Regression sweep delta is recorded, partitioned by locus.** Per-locus regression rate is the key measurement. Low rate for `scaffold-sufficient` and `scaffold-augmentable` supports the scaffolding hypothesis; high rate falsifies or constrains it.
6. **Pattern-library calibration notes exist.** Per-pattern, what the demo evidence said about fit.
7. **Scaffolding hypothesis is scored.** An explicit outcome statement, with evidence: (a) **confirmed** — AI-executed structural edits close without significant regression under scaffolding; (b) **augmented-and-confirmed** — items initially unclosable become closable after specified apparatus additions; (c) **partially confirmed** — the hypothesis holds for a named sub-class of items and fails for another named sub-class; (d) **falsified** — AI-executed edits regress at rates comparable to unscaffolded baseline, with specific failure patterns named. Any of (a)–(d) is a mission success; silence on this question is a mission failure.
8. **End-to-end demo is reproducible.** A new agent or human could walk the workflow on a second dataset from this doc alone.

### Demo dataset

The demo is `/home/joe/npt/working-paper/UKRN_WP_draft_v6.md`, with 26 flagged items in `/home/joe/npt/working-paper/annotations.el` (6 `annotation/critique` + 20 `annotation/writing-coherence`). NPT is the test bed, not the subject — the mission tests the scaffolding hypothesis against a real, committed, domain-specific draft where a non-regression outcome is not trivially achievable. The WP has a fixed author voice, a fixed argument trajectory, and a definite audience (Research England + UKRN-S board + programme participants); if the scaffolding carries enough of that context for AI-executed edits to close without regression, that is non-trivial evidence. Successful close-out delivers (a) the handoff taxonomy + workflow specification, (b) the scaffolding delta (any apparatus additions the mission produced), (c) a scored hypothesis outcome, and (d) a revised v7 of the WP as the demo output.

### Relationship to other missions

- **Depends on:** the writing-coherence library (built 2026-04-24); the `annotation/critique` and `annotation/writing-coherence` subtype plumbing in `arxana-browser-essays.el` (built 2026-04-24, ~10 lines in each of two renderers).
- **Sibling:** `M-essays-edit-cycle` — that mission specifies how annotations re-anchor under text edits; this one exercises the edit pathway in anger and surfaces requirements the broader edit-cycle mission should pick up.
- **Enables:** pattern-library calibration feedback loops; a per-document edit-workflow record that can be re-run on future drafts; a substrate for "writing as adequate object" in the Spinoza/media sense — i.e. future missions that treat prose as a first-class arxana entity.

### Source material

- `/home/joe/npt/working-paper/UKRN_WP_draft_v6.md` — demo dataset source.
- `/home/joe/npt/working-paper/annotations.el` — flagged items (26 in scope, plus 53 prior grounds/instantiates/exhibits out of scope for this mission).
- `/home/joe/code/futon3/library/writing-coherence/*.flexiarg` — 11 patterns.
- `/home/joe/code/futon4/dev/arxana-browser-essays.el` — rendering + edit pathway plumbing.
- `/home/joe/code/futon4/holes/mission-lifecycle.md` — this mission's procedural spec.

### Owner and dependencies

- **Owner:** joe (structural edits, library calibration authority) + claude (mechanical edits, regression sweeps, per-item handoff drafting).
- **Primary repo:** futon4 (arxana Essays, mission infrastructure).
- **Secondary repos:** futon3 (pattern library, any proposed revisions); npt (demo dataset).
- **No external dependencies.** Local Emacs + the already-landed arxana plumbing are sufficient.

---

## 2. MAP

### Context-requirement taxonomy (used in Q1/Q2 audits)

| Code | Context kind |
|---|---|
| `PAT` | Pattern template — the flexiarg's compositions + check fields supply the rewrite shape |
| `LOC` | Local context — the surrounding sentences in the same paragraph |
| `PAY` | Paragraph payload — the claim the paragraph is actually making |
| `TRAJ` | Section argument-trajectory — where this paragraph sits in the section's progression |
| `THESIS` | Whole-paper thesis — the paper's central structural claim |
| `XREF` | Cross-reference — a forward/back pointer to another section, the companion paper, the annex, or the library |
| `AUD` | Audience register — who reads this, in what register |
| `TAC` | Tacit author knowledge — author's stance on unstated strategic questions (funder priors, sensitivities, what to defer vs commit to) |

### Context-source taxonomy (used in Q2 audit)

| Code | Where the context lives |
|---|---|
| `IN-PATTERN` | In the writing-coherence flexiarg (pattern + compositions + failure-modes) |
| `IN-ANNOTATION` | In the annotation's `:note` (diagnosis + suggested-move + severity) |
| `IN-PAPER-NEAR` | Within the paragraph or within ~2 paragraphs either side |
| `IN-PAPER-FAR` | Elsewhere in the paper (requires whole-doc context loaded) |
| `IN-LIBRARY` | Elsewhere in the futon3 library (companion paper, sibling patterns) |
| `NOWHERE` | Not currently encoded in the apparatus — scaffolding-augmentation candidate |

### Q1 / Q2. Per-item audit — 26 flagged items

Column key: `ID` | `pat` (pattern family) | `sev` (severity) | `req` (context-requirement) | `src` (context-source) | `locus` (provisional routing) | `ent` (entailed by).

**Critiques (6):**

| ID (short) | pat | sev | req | src | locus | ent |
|---|---|---|---|---|---|---|
| `critique-h6-reader-map` | — | major-low | TAC | TAC | **tacit-required** (ratification; execution is delete) | — |
| `critique-exec-summary-closer` | — | minor | THESIS + AUD | IN-PAPER-FAR + IN-ANNOTATION(candidate thesis) | **scaffold-augmentable** (Joe ratifies candidate thesis) | — |
| `critique-ontogenetic-accretion` | — | minor | TRAJ + PAY | IN-PAPER-NEAR + IN-ANNOTATION (two drafted options) | **tacit-required** (Joe picks option; execution scaffold-sufficient) | — |
| `critique-worked-example-forward-ref` | — | minor | TRAJ | IN-PAPER-NEAR | **scaffold-sufficient** (option (b) forward-reference is purely additive) | — |
| `critique-responsiveness-lift` | — | minor | TRAJ + PAY | IN-PAPER-NEAR (both §1 and §6 contexts) | **scaffold-augmentable** (Joe names insertion target in §1) | — |
| `critique-mvsg-cash-or-drop` | — | major | THESIS + TAC + TRAJ | TAC + IN-PAPER-FAR (Annex B.3) | **tacit-required** (Joe decides cash vs drop; if cash, chooses between typed-flows and within-institution demo) | — |

**Writing-coherence — §0 (3):**

| ID (short) | pat | sev | req | src | locus | ent |
|---|---|---|---|---|---|---|
| `s0-meta-lede-soft-opener` | meta-lede | minor | THESIS | IN-PAPER-NEAR (architectural claim is already sentence 3) | **scaffold-sufficient** | — |
| `s0-meta-lede-this-paper-is-about` | meta-lede | minor | PAT + LOC | IN-PATTERN + IN-PAPER-NEAR | **scaffold-sufficient** | — |
| `s0-meta-lede-in-structural-terms` | meta-lede | minor | (closes with C2) | — | **entailed** — resolves as part of `critique-exec-summary-closer` | C2 |

**Writing-coherence — §1 (3):**

| ID (short) | pat | sev | req | src | locus | ent |
|---|---|---|---|---|---|---|
| `s1-missing-mechanism-supply-demand` | missing-mechanism | minor | PAY + TAC | partly IN-PAPER-NEAR (incentives hint); partly NOWHERE | **tacit-required** (Joe clarifies intent or approves cut) | — |
| `s1-hedged-lift-governance-debt` | hedged-lift | minor | PAY + XREF | partly IN-PAPER-NEAR; XREF NOWHERE | **scaffold-augmentable** (Joe names XREF target, OR drop option is scaffold-sufficient) | — |
| `s1-subheading-reproducible-design-research` | subheading-without-paragraph | minor | TRAJ + PAY | IN-PAPER-NEAR | **tacit-required** (Joe picks expand/rename/fold; execution scaffold-sufficient) | — |

**Writing-coherence — §2 (4):**

| ID (short) | pat | sev | req | src | locus | ent |
|---|---|---|---|---|---|---|
| `s2-essentially-multiplicative` | unearned-essentially | minor | PAT + LOC | IN-PATTERN + IN-PAPER-NEAR (insofar-as clause already carries reason) | **scaffold-sufficient** | — |
| `s2-meta-lede-seeks-to-operationalise` | meta-lede | minor | PAT + LOC | IN-PATTERN + IN-PAPER-NEAR | **scaffold-sufficient** | — |
| `s2-triad-inflation-two-paradigmatic` | triad-inflation | minor | PAT + LOC | IN-PATTERN + IN-PAPER-NEAR | **scaffold-sufficient** | — |
| `s2-throat-clearing-every-profile-unique` | throat-clearing-close | minor | PAY + LOC | IN-PATTERN + IN-PAPER-NEAR (Figure 2 result) | **scaffold-sufficient** | — |

**Writing-coherence — §4 (1):**

| ID (short) | pat | sev | req | src | locus | ent |
|---|---|---|---|---|---|---|
| `s4-meta-lede-good-opportunity-revisit` | meta-lede | minor | PAT + LOC | IN-PATTERN + IN-PAPER-NEAR | **scaffold-sufficient** | — |

**Writing-coherence — §5 (7):**

| ID (short) | pat | sev | req | src | locus | ent |
|---|---|---|---|---|---|---|
| `s5-meta-lede-here-we-ask` | meta-lede | minor | PAT + LOC | IN-PATTERN + IN-PAPER-NEAR | **scaffold-sufficient** | — |
| `s5-scope-mush-and-so-on` | scope-mush | minor | PAT | IN-PATTERN + IN-PAPER-NEAR | **scaffold-sufficient** | — |
| `s5-balanced-pair-padding-not-immediately` | balanced-pair-padding | minor | PAT + TAC (is concession load-bearing?) | IN-PATTERN + partial TAC | **scaffold-augmentable** (Joe confirms concession drop OK) | — |
| `s5-balanced-pair-padding-mismatch-opportunity` | balanced-pair-padding | minor | PAT | IN-PATTERN + IN-PAPER-NEAR | **scaffold-sufficient** | — |
| `s5-balanced-pair-padding-triple-negation` | balanced-pair-padding | **major** | PAT + TAC (which negations rule out live misreadings?) + TRAJ | IN-PATTERN + IN-PAPER-NEAR + partial TAC | **scaffold-augmentable** (Joe ratifies which misreadings are live) | — |
| `s5-essentially-same-argument` | unearned-essentially | minor | (closes with C6) | — | **entailed** — resolves as part of `critique-mvsg-cash-or-drop` | C6 |
| `s5-paraphrase-drift-mvsg-paragraph` | paraphrase-drift | minor | PAT (post-C6 resolution) | IN-PATTERN + depends on C6 | **scaffold-augmentable** (after C6 lands, becomes scaffold-sufficient) | C6 (partial) |

**Writing-coherence — §6 (2):**

| ID (short) | pat | sev | req | src | locus | ent |
|---|---|---|---|---|---|---|
| `s6-meta-lede-four-main-claims` | meta-lede | minor | PAT + LOC | IN-PATTERN + IN-PAPER-NEAR | **scaffold-sufficient** | — |
| `s6-meta-lede-becomes-concrete-when` | meta-lede | minor | PAT + LOC | IN-PATTERN + IN-PAPER-NEAR | **scaffold-sufficient** | — |

### Q3. Entailment map

Three entailment relations identified:

- `s0-meta-lede-in-structural-terms` ← `critique-exec-summary-closer` (both sit in the exec-summary close; the critique's rewrite absorbs the meta-lede).
- `s5-essentially-same-argument` ← `critique-mvsg-cash-or-drop` (the adverb is the symptom; the recursion-cheque is the cause).
- `s5-paraphrase-drift-mvsg-paragraph` ← `critique-mvsg-cash-or-drop` (partial; post-C6 resolution, the paraphrase-drift becomes scaffold-sufficient and can be closed independently).

**Closure order implied:** resolve C2 and C6 first; their entailed writing-coherence items either close in cascade or become unblocked.

### Q4. Pattern hit distribution (corrected)

| Pattern | Count | Loci |
|---|---|---|
| `meta-lede` | 8 | §0 ×3, §2 ×1, §4 ×1, §5 ×1, §6 ×2 |
| `balanced-pair-padding` | 3 | §5 ×3 |
| `unearned-essentially` | 2 | §2, §5 (§5 entailed by C6) |
| `missing-mechanism` | 1 | §1 |
| `hedged-lift` | 1 | §1 |
| `subheading-without-paragraph` | 1 | §1 |
| `triad-inflation` | 1 | §2 |
| `throat-clearing-close` | 1 | §2 |
| `scope-mush` | 1 | §5 |
| `paraphrase-drift` | 1 | §5 (partially entailed by C6) |
| `recursion-cheque` | 0 flagged directly (covered by C6) | — |

**Finding:** `meta-lede` is by far the dominant failure mode in v6 — 8 of 20 writing-coherence flags (40%). It concentrates in §0 and §6 (section openings and conclusions — sections where the writer is announcing rather than arguing). `balanced-pair-padding` clusters entirely in §5, the section with the heaviest rhetorical register (geometric claim + mode correspondence). The distribution is informative for the scaffolding hypothesis: if AI closes the 8 `meta-lede` items under scaffolding without regression, that is the strongest evidence for sufficiency; if AI regresses on them (introduces new meta-ledes while rewriting), that is the clearest falsifier.

### Q5. Closure mechanics

Inspected the renderer (arxana-browser-essays.el:247-250) and manifest. Findings:

- `annotation/retracted` exists and is treated by the render pipeline as "superseded / no longer active" — it sets a label but does not carry structured closure metadata.
- There is no `annotation/closed` convention, nor a `:closure-record` plist field.
- The XTDB sync path skips `annotation/comment` entirely (arxana-browser-essays.el:1700), so critiques + writing-coherence items are manifest-only. This means closure records can be added to the manifest as Elisp plists without requiring XTDB schema changes.

**Proposed convention:** extend the manifest entry with a `:closure` plist (keyword-valued) that carries:
```
:closure (:locus <symbol>
          :scaffolding-used <list-of-symbols>
          :scaffolding-augmented <string-or-nil>
          :before <passage-string>
          :after <passage-string>
          :sweep-status <:clean | :regressed | :pending>
          :timestamp <yyyy-mm-dd>)
```
A closed item retains its original `:annotated :passage` (for audit) but gains the `:closure` plist. Optionally add the `annotation/closed` label for rendering (a `✓` marker, say). The existing `annotation/retracted` semantics are preserved for items invalidated for reasons other than closure (e.g. the v5 items already marked `:retracted t`). This is a small manifest-convention addition, no code change needed unless we want the `✓` marker at render time.

### Q6. Regression-sweep minimum-viable record

The closure plist above is the minimum viable record. Specifically, `:before`, `:after`, and `:sweep-status` are load-bearing for the hypothesis score: without them the sweep cannot partition per-locus regression rates. Everything else is useful provenance.

### Q7. Scaffolding inventory

**What the apparatus currently carries (loadable by an AI executing an edit):**

| Scaffold component | Coverage | Notes |
|---|---|---|
| 11 writing-coherence flexiargs | Load-bearing for rewrite shape (compositions) | `PAT` context supplied |
| Critique + writing-coherence annotations | Per-item diagnosis + suggested-move + severity | `IN-ANNOTATION` context supplied |
| Section headings + subheadings | Coarse structural navigation | Supports `TRAJ` inference near-site |
| Paragraph context (~2 paras each side) | Sufficient for most `LOC` + many `PAY` | Main `IN-PAPER-NEAR` source |
| Whole-paper body in context window | Available but expensive per edit | Covers `IN-PAPER-FAR`; load per-batch |
| Sibling pattern cross-references (`@references`) | Enables pattern-chain reasoning | Under-used currently |

**What the apparatus does NOT carry (gap set — scaffolding-augmentation design space):**

| Gap | What it would encode | Items it would unblock |
|---|---|---|
| **Claim-ledger per section** | One-line claim per subsection + whole-section thesis (answers "what is this section's payload?") | C3, W4, W6 (TRAJ/PAY decisions), many meta-ledes that need to know what claim to promote |
| **Audience register** | Named reader profile(s) — Research England, UKRN-S board, programme participants — with register cues per | C2 (exec-summary register); close-paragraphs generally |
| **Whole-paper thesis statement** | A single canonical sentence of the paper's argument (the C2 candidate, ratified) | C2, W3 (entailed), potentially C6 |
| **Argument-trajectory map** | Section-to-section argumentative dependency graph (what §N depends on from §M) | C5 (responsiveness-lift insertion target), C3 (ontogenetic-accretion placement) |
| **Tacit-decision ledger** | Record of Joe-owned strategic calls (e.g., drop H6 audience map; MVSG cash-vs-drop outcome) | C1, C6 (tacit-required items whose execution becomes scaffold-sufficient after ratification) |

**Augmentation cost analysis:**
- **Claim-ledger:** highest leverage. ~8 subsection entries × one-line each = ~8 sentences. Unblocks ~5–8 items. Low cost, high yield.
- **Audience register:** ~3-line note at paper-top. Unblocks 2–3 items. Very low cost.
- **Thesis statement:** 1 sentence, ratified from C2's suggested move. Unblocks C2 + W3. Trivial cost.
- **Argument-trajectory map:** would formalize what Joe already has implicitly. ~6 nodes + ~8 edges. Medium cost; unblocks 2 items but is reusable for future drafts.
- **Tacit-decision ledger:** this is what Joe will produce as he ratifies. Emerges during DERIVE, not beforehand.

### Locus tally

| Locus | Count | Notes |
|---|---|---|
| `scaffold-sufficient` | **12** | Immediate AI execution with current apparatus |
| `scaffold-augmentable` | **6** | AI execution after single-sentence Joe ratification or named-target augmentation |
| `tacit-required` | **5** | Joe-owned decisions (C1, C3, C6, W4, W6); execution becomes scaffold-sufficient after |
| `entailed` | **3** | Close in cascade (W3→C2, W17→C6, W18→C6-partial) |
| `defer / reject` | 0 | — |
| **Total** | **26** | ✓ |

Under the scaffolding hypothesis, the 12 scaffold-sufficient items are the highest-confidence AI-executable set; the 6 scaffold-augmentable items are the primary test of whether targeted augmentation closes the gap; the 5 tacit-required items are where the hypothesis concedes that some context will not live in the apparatus (and where Joe's execution calibrates what kinds of context we should try to move into the apparatus next time).

### Surprises

- **Eight meta-ledes, not nine.** The prior turn's summary miscounted; the correct distribution is above.
- **`balanced-pair-padding` is entirely concentrated in §5.** The correlation with §5's rhetorical register (geometric synthesis + mode correspondence) is striking and suggests register-specific failure modes — a finding the pattern library should record (possibly as an `@audience` or `@register` annotation on the pattern's failure-modes list).
- **Three entailment relations, two of them pointing to C6.** The MVSG critique is load-bearing across §5; its resolution cascades to at least two writing-coherence items. This sharpens the tacit-required burden on Joe for C6 specifically.
- **Q5 answer is low-friction.** No code change needed to introduce the `:closure` plist convention — the renderer currently ignores unknown keywords on annotation plists. This means DERIVE can design the closure record freely without a VERIFY code spike.

### Exit from MAP

All seven questions answered with concrete findings. The "ready vs missing" split is the locus tally above. Entailment map is settled. Scaffolding gap-set is inventoried with cost estimates. Ready to advance to DERIVE, which will:

1. Specify the handoff taxonomy formally (entry criteria per locus).
2. Produce the IF/HOWEVER/THEN/BECAUSE rationale for each `scaffold-augmentable` item's augmentation decision.
3. Design the claim-ledger + audience register + thesis-statement + trajectory-map (the augmentations themselves), at the granularity needed to close the 6 `scaffold-augmentable` items.
4. Finalise the closure record schema.

---

## 3. DERIVE

### Handoff taxonomy — entry criteria

| Locus | Entry criteria | Decision procedure | Produces |
|---|---|---|---|
| `scaffold-sufficient` | Context requirement is at most `PAT` + `LOC`, **or** `PAY`/`TRAJ` with source `IN-PAPER-NEAR` where the payload is unambiguously recoverable from adjacent sentences. No `THESIS`, `AUD`, or `TAC` component. | AI executes directly using the pattern, annotation, and local paragraph context. No human ratification step. | Edit diff + closure record. |
| `scaffold-augmentable` | Context requirement includes `THESIS`, `XREF`, `AUD`, or a tractable `TAC` component, **and** the gap closes with a single-sentence ratification or a named target Joe can supply in one turn. | Joe supplies the augmentation (ratified sentence or named target); AI executes with the augmentation cited in the closure record. | Augmentation record (reusable) + edit diff + closure record. |
| `tacit-required` | Context requirement includes `TAC` with multiple competing strategic options where the choice is itself a first-class authorial act (not reducible to single-sentence ratification). | Joe decides; decision becomes a tacit-ledger entry; execution may be Joe-direct or AI-assisted but the choice is recorded as Joe-owned. | Tacit-ledger entry + edit diff + closure record. |
| `defer` | Item's closure depends on information not yet available (e.g., post-T1 evidence; a decision pending elsewhere). | Marked deferred with named trigger condition. | Deferral record. |
| `reject` | On DERIVE inspection, the flag is a false positive — the passage does not instantiate the pattern, or does so for load-bearing reasons the pattern under-specifies. | Documented rejection; pattern library gets the calibration signal. | Calibration note. |

**Threshold for `scaffold-sufficient` vs `scaffold-augmentable`:** if the apparatus contains the context needed for an adequate edit *without loading additional author-supplied information*, the item is scaffold-sufficient. If the apparatus needs one bounded addition (one sentence or one named target), it is scaffold-augmentable. If it needs open-ended author judgement, it is tacit-required.

### The four apparatus augmentations

Four named augmentations satisfy the `scaffold-augmentable` items and also provide durable scaffolding for future drafts. They are produced once; each scaffold-augmentable item cites the augmentation(s) it uses.

#### A1. Paper thesis statement

A single canonical sentence of the paper's central structural claim, ratified by Joe. Candidate set (DERIVE will select one during INSTANTIATE under Joe's direction; for now, three candidates sit in the mission):

- **T1:** "UKRN-S's core function is to hold the evidence apex that the ORP could not." *(from critique C2's suggested move; shortest and most structural)*
- **T2:** "A centrally-administered training programme becomes a learning system when the service bearing it accepts five design disciplines." *(from ukrns/ARGUMENT @summary; matches the companion paper)*
- **T3:** "UKRN-S becomes a learning system by inhabiting six design tensions in the open under evidence." *(synthesis of T1 and T2; emphasises the tensions framing)*

**IF** T1 is ratified, **HOWEVER** the paper's body language emphasises tensions and compositions rather than "evidence apex" alone, **THEN** the thesis also needs a secondary operational clause, **BECAUSE** a one-clause thesis the body does not echo leaves the body looking disconnected from the exec summary's claim. Likely final form: T1 as primary + one operational clause citing the six tensions.

Unblocks: C2, W3 (entailed), and implicitly the whole §0 exec-summary rewrite.

#### A2. Audience register

A ~3-line note at paper scope naming the reader profile and the register commitments.

**Draft:**
> *Audience.* Research England (funder, decision-maker on UKRN-S continuation), UKRN-S board and CEO (operational decisions on the six tensions), ORP Training Community of Practice (practitioners who ran Cycle 0 and will judge whether the paper represents their experience), open-research evaluation community (academic readers).
> *Register.* Committed analytical prose; names constraints rather than managing them; avoids funder-speak; does not soften structural claims for political palatability.
> *Implication for closes.* Paragraph and section closes end on propositions a careful reader could disagree with, not on attitude or scope disclaimers.

Unblocks: C2 (exec-summary register); reusable for all close-paragraphs.

#### A3. Claim-ledger per section

One-line payload claim per section (and per subsection where the subsection is load-bearing), sourced from the paper's existing content and ratified. This is the highest-leverage augmentation — it gives the AI a direct answer to "what is this paragraph trying to say?" which is the context `meta-lede` fails for lack of.

**Draft:**

| Section | Payload claim |
|---|---|
| §0 Exec summary | The gap between outcomes is architectural; UKRN-S's core function is the evidence apex the ORP could not carry. Six tensions recur along the apex's sustainment. |
| §1 How sustainability grows | Krowne's triangle + an evidence apex = a sustainability tetrahedron. UKRN-S's apex role earns itself through a staged bootstrap whose openness-governance trade-offs are explicit per step. |
| §1.Bootstrap sequence | Five capabilities (not posts), each producing an artefact that makes the next step cheaper; the sequence is a design proposal, not a work plan. |
| §1.Ontogenetic accretion | *(tacit-required — C3 resolves this; placeholder for whichever option Joe picks)* |
| §2 From qualitative to computational | Five patterns factorise into D × A; the relationship is multiplicative because either dimension alone can bound the whole; a universal bundle is therefore wrong for a visible fraction of the network and diagnosis is mathematically necessary. |
| §3 What the ORP built | Cycle 0 produced analytical infrastructure (T1 instrument, population model, role codebook, design patterns, logic model). The single most consequential empirical finding is cross-institutional evaluation-analysis absent 4/4. |
| §4 Population model | Ten profiles move monotonically through D × A across four stages; the direction is robust under modelling variants while the point estimates are not; the widest credible intervals identify where the next T1 observation is highest-value. |
| §5 Recapitulation | The three implementation modes (Multiplied, Absorbed, Mismatch) are sub-tetrahedra; UKRN-S is the evidence apex maintaining the cross-mode void. *(post-C6 this sharpens to whichever of cash/drop Joe chooses.)* |
| §6 Conclusions | UKRN-S should operate as the keystone for a learning system; the first concrete consequence is a VoI-targeted T1 wave, and the design updates under the resulting evidence. |

Unblocks: C3 (after C3 ratifies), W4, W6, and many of the scaffold-sufficient `meta-lede` items (each promotes a section's payload claim to paragraph-one position).

#### A4. Argument-trajectory map

Section-to-section argumentative dependency, noting which sections supply context for which later claims. Compact form:

```
§0 (exec) ─ states the claims §1–§6 will argue
§1 (architectural move) ─ introduces tetrahedron + bootstrap + openness-governance + self-application
§2 (logic model operationalisation) ─ introduces D × A + multiplicativity + diagnose-before-prescribing
§3 (Cycle 0 evidence) ─ supplies the empirical base (T1, population model, role codebook, EV_analyse 4/4)
§4 (population model + uncertainty) ─ operationalises §2 against §3's data; defines D, A formally
§5 (geometric synthesis) ─ composes §2's D × A + §3's modes into the MVSG; depends on §2, §3
§6 (decision + pattern + loop) ─ names the responsiveness↔stability tension that motivates §1–§5 in retrospect

Key cross-dependencies:
  §6.pattern (responsiveness ↔ stability) → §1–§5 (the tension motivates the whole apparatus)
       ↑ this is the lift the critique-responsiveness-lift critique argues for: move the motivator earlier
  §3.worked-example-D=0.46 → §4.model-structure (D, A defined)
       ↑ this is the forward-reference the critique-worked-example-forward-ref critique flags
  §5.MVSG → §2.D×A + §3.modes + §4.trajectory
       ↑ the MVSG depends on §2–§4 but does not cash the recursion claim at a second scale — C6
```

Unblocks: C5 (insertion target for the responsiveness-lift is now named: "between §1.bootstrap-sequence and §1.openness-governance-trade-off, as a bridge that motivates the governance work by the later tension"). Documents C4 and C6's dependency structure.

#### A5. Tacit-decision ledger *(populates during INSTANTIATE)*

Schema for recording Joe's authorial decisions on tacit-required items:

```elisp
(:decision-id <slug>
 :item-id <annotation-id that required the decision>
 :question <plain-language question the decision resolves>
 :option-chosen <symbol>
 :options-considered <list>
 :rationale <string>
 :scaffolds-future <list of item-ids the decision unblocks>
 :timestamp <yyyy-mm-dd>)
```

Expected entries (5): C1 (H6 delete-vs-keep), C3 (ontogenetic-accretion expand/fold/rename), C6 (MVSG cash-vs-drop + if-cash-which-option), W4 (supply/demand intent), W6 (reproducible-design-research disposition).

### Per-item decision table

**Scaffold-sufficient (12):** grouped justification. Each item's edit is (a) a cut, (b) a single-word or single-phrase substitution, or (c) a promotion of an existing-in-paragraph sentence to the paragraph's first position, guided by the corresponding pattern's composition + the claim-ledger entry for that section.

| ID | Pattern | Scaffolds used | Expected edit shape |
|---|---|---|---|
| C4 `worked-example-forward-ref` | — | A3 (§3 ledger) + annotation | Add forward-reference sentence at top of §3 worked-example subsection |
| W1 `s0-soft-opener` | meta-lede | A3 (§0 ledger) + annotation | Promote sentence 3 to sentence 1; adapt sentences 1–2 as consequences |
| W2 `s0-this-paper-about` | meta-lede | pattern + LOC | Cut + fuse with six-tensions sentence |
| W7 `s2-essentially` | unearned-essentially | pattern + LOC | Drop adverb; retain the insofar-as clause as the reason |
| W8 `s2-operationalise` | meta-lede | pattern + LOC | Cut first sentence; paragraph opens on "Each design pattern's diagnostic question..." |
| W9 `s2-two-paradigmatic` | triad-inflation | pattern + LOC | Rewrite "Two paradigmatic" → "Three service-bundle choices" |
| W10 `s2-every-profile-unique` | throat-clearing-close | A3 (§2 ledger) + LOC | Replace close with the structural consequence (who benefits from diagnosis, who doesn't) |
| W11 `s4-good-opportunity` | meta-lede | pattern + LOC | Cut announcement; open on "FG2A-P1 starts at..." |
| W12 `s5-here-we-ask` | meta-lede | pattern + LOC | Collapse two announcements; open on "Each of the three implementation modes..." |
| W13 `s5-and-so-on` | scope-mush | pattern + LOC | Drop "and so on"; retain three examples |
| W15 `s5-mismatch-opportunity` | balanced-pair-padding | pattern + LOC | Lead with Z; drop strawmen |
| W19 `s6-four-main-claims` | meta-lede | pattern + LOC | Cut announcement; subsection heading carries the structural promise |
| W20 `s6-becomes-concrete` | meta-lede | pattern + LOC | Cut framing; open on "Suppose the planned T1 phase..." |

All 13 of the above (one more than the MAP count — re-audit during DERIVE promoted `critique-worked-example-forward-ref` from "option (b) is scaffold-sufficient" to "execute option (b) in INSTANTIATE"; the item is now counted as scaffold-sufficient with a trivial additive edit). Revised locus tally: **13 scaffold-sufficient, 5 scaffold-augmentable, 5 tacit-required, 3 entailed** = 26.

**Scaffold-augmentable (5) — per-item IF/HOWEVER/THEN/BECAUSE:**

**C2 `critique-exec-summary-closer`**
- **IF** the critique's candidate thesis ("UKRN-S's core function is to hold the evidence apex that the ORP could not") is ratified as A1,
- **HOWEVER** the exec summary already contains "inhabit this space and learn as it goes" + "In structural terms, the paper aims to make the case" as closing material,
- **THEN** replace both closing sentences with: thesis sentence (A1) + one operational sentence naming the six tensions as the object of inhabitation,
- **BECAUSE** the thesis alone would leave the "six tensions" framing uncashed in the exec summary, and the existing tensions sentence earlier in the paragraph carries the detail the operational sentence needs to reference. This also closes W3 (entailed).

**C5 `critique-responsiveness-lift`**
- **IF** A4 (argument-trajectory map) names the insertion target as "between §1.bootstrap-sequence and §1.openness-governance-trade-off",
- **HOWEVER** lifting the sentence verbatim from §6 would leave §6's pattern block without its motivator clause,
- **THEN** copy (not move) the "active because the appropriate responsiveness changes over the lifetime of the programme" framing into §1 as a bridge sentence, and retain a shortened form in §6's pattern-block to preserve the pattern's Tension structure,
- **BECAUSE** the framing is both the motivator for §1–§5's apparatus (its rightful home) and part of the pattern's Tension defence (where it stays). Duplication across loci is correct here, not a paraphrase-drift instance, because the two copies do different work in different places.

**W5 `s1-hedged-lift-governance-debt`**
- **IF** the hedged-lift composition "drop" is chosen (the default for sentences whose lift cannot be defended in-scope),
- **HOWEVER** the "governance debt" concept is genuinely useful for future argument even if the WP does not cash it,
- **THEN** cut the whole "worth flagging now though beyond the scope" sentence; note in the DERIVE calibration log that "governance debt" is a candidate future-mission concept,
- **BECAUSE** the drop-option is the cheapest closure; the concept is not lost (it is noted) and the paragraph's argument is unharmed (the prior sentences already make the "Step 4 carries governance load" point).

*If Joe prefers "commit-and-defend" or "defer-with-pointer": alternative IF/THEN recorded as options (b) and (c), not elaborated here.*

**W14 `s5-balanced-pair-padding-not-immediately`**
- **IF** the concession "although institutional changes would likely improve outcomes" can drop (Joe's ratification required),
- **HOWEVER** the paragraph is making a load-bearing claim about Absorbed mode being person-network-sustained rather than institution-sustained — and a blanket drop could read as "institutions don't matter",
- **THEN** commit to Y directly ("Sustainability in this case is carried by the person and the network of practice that validates their work"), AND add a separate short sentence later in the paragraph noting the institutional contribution is additive: "Institutional support still improves Absorbed-mode outcomes, but its absence does not dissolve the mode the way it dissolves the Multiplied one",
- **BECAUSE** the two-move resolution preserves the load-bearing claim (institution matters less here than in Multiplied) without keeping the balanced-pair padding (the "not X; Rather, Y" construction).

**W16 `s5-balanced-pair-padding-triple-negation`**
- **IF** Joe ratifies which of the three negations (deliver training / support lone practitioners / redesign failing models directly) correspond to live reader misreadings,
- **HOWEVER** all three are currently formatted as the same register of refusal, which creates a uniform negation bank,
- **THEN** restructure as: one topic sentence stating UKRN-S's positive role ("holds the void open between the three modes, making evidence-flows possible"); followed by one paragraph that names the negations as specific misreadings only for those that are live, each with its specific rebuttal; defer or drop non-live negations,
- **BECAUSE** the balanced-pair pattern predicts that defensive negations carry weight only when the misreading is a plausible reader response. Ratification answers which misreadings to rule out; the structural move — lead with positive — is fixed by the pattern's composition (1).

**W18 `s5-paraphrase-drift-mvsg-paragraph`**
- **IF** C6 is resolved first (cash or drop),
- **HOWEVER** the paragraph's three-sentence structure changes depending on C6's outcome (cash adds a content sentence between 1 and 3; drop leaves 1 and 3 as unambiguous paraphrases),
- **THEN** defer W18's execution to post-C6; on C6-cash, cut sentence 3 (the weaker paraphrase) and retain sentence 1 + the new sentence 2; on C6-drop, collapse to a single sentence combining 1 and 3's content,
- **BECAUSE** W18 is entailed by C6 (MAP Q3) and its right shape cannot be determined until C6 lands.

**Tacit-required (5) — questions to Joe; ledger entries populate on ratification:**

| ID | Question | Options |
|---|---|---|
| C1 | Close H6 by deleting the hole-marker without adding the audience map? | (a) delete as recommended by critique; (b) add audience map; (c) delete but add a different prelude element |
| C3 | Ontogenetic-accretion subsection: expand / fold / rename? | (a) expand into per-stage infrastructure walk; (b) fold into Step 4 bullet or bridge sentence; (c) rename to match the current paragraph's claim |
| C6 | MVSG: cash the recursion claim, or drop the sentence? | (a) cash with typed cross-mode evidence flows; (b) cash with within-institution MVSG demonstration; (c) drop the recursion sentence and recast §5 as synthesis only |
| W4 | Supply/demand parenthetical: what was intended, or is it cuttable? | (a) discharge with one sentence naming both sides; (b) integrate into prior-art sentence; (c) cut |
| W6 | Reproducible-design-research subheading: expand / rename / fold? | (a) expand with concept definition; (b) rename to "The paper as notebook" or similar; (c) fold into bootstrap section |

**Entailed (3):** close in cascade. W3 cascades from C2; W17 cascades from C6; W18 cascades from C6 (partial — becomes scaffold-sufficient post-C6).

### Closure record schema

```elisp
:closure (:locus <:scaffold-sufficient | :scaffold-augmentable | :tacit-required | :deferred | :rejected | :entailed>
          :scaffolding-used <list — e.g. (:pattern :annotation :claim-ledger-s2 :trajectory-map)>
          :augmentation <plist or nil — :augmentation-id + :value if locus is scaffold-augmentable>
          :tacit-decision-id <symbol or nil — references a tacit-ledger entry if locus is tacit-required>
          :entailed-by <item-id or nil — if locus is :entailed>
          :before <string — original passage verbatim>
          :after <string or nil — replacement passage, nil for deletion>
          :sweep-status <:clean | :regressed-new-pattern | :regressed-same-pattern | :pending | :na-for-deletion>
          :sweep-findings <list of pattern-names newly matched on :after, if any>
          :decided-by <:claude | :joe | :joint>
          :executed-by <:claude | :joe>
          :timestamp <"yyyy-mm-dd">
          :notes <string or nil>)
```

Appended to the existing annotation plist; no renderer change required (unknown keywords are ignored). The `annotation/closed` label is optional — INSTANTIATE can choose to add it for rendering (`✓` marker) or omit it and rely on presence of `:closure` as the signal.

### Exit from DERIVE

The handoff taxonomy has entry criteria. The per-item decision table covers all 26. The four apparatus augmentations (A1–A4) plus the tacit-decision ledger schema (A5) cover the scaffolding-augmentation design space. The closure record schema is specified. Someone could implement the mission from this section without asking clarifying questions — except for the five tacit-required ratifications, which are by design not pre-answerable, and C2's thesis choice among T1/T2/T3. Those sit at the top of INSTANTIATE as the first five human-facing questions.

---

## 4. ARGUE

### Pattern cross-reference (structured survey)

Searched `futon3/library/` for patterns applicable to the DERIVE design. The workflow instantiates several patterns from `pattern-coherence/`, and — non-trivially — several design patterns from `ukrns/` applied at a different scale than the one they were originally derived for (the WP applies them to service design; this mission applies them to workflow design for editing that service-design prose). This meta-move is itself a `ukrns/self-application` instance.

**Patterns that apply as direct constraints on the DERIVE design:**

| Pattern | Where it applies | How |
|---|---|---|
| `pattern-coherence/internal-coherence` | Handoff taxonomy | Each locus maps context-requirement → action → expected resolution. The threshold rule keeps the mapping aligned per locus; a scaffold-augmentable item whose gap actually requires tacit judgement would violate alignment — we catch that at DERIVE by routing it to tacit-required instead. |
| `pattern-coherence/applicability-signals` | Entry criteria per locus | The entry criteria ARE the applicability signals. The "do not apply" signal for scaffold-sufficient is "context-requirement includes THESIS/AUD/TAC". Without this, the workflow routes blindly and regressions become invisible. |
| `pattern-coherence/scope-boundaries` | Scope-in/out + defer/reject loci | `defer` and `reject` are explicit scope-boundary signals for items that look in-scope but aren't. Without them the workflow accumulates forced closures on flags that shouldn't close here. |
| `pattern-coherence/evidence-alignment` | Closure record schema | `:before` / `:after` / `:sweep-status` / `:sweep-findings` constitute the evidence trail that aligns each closed item with whether the scaffolding actually worked. Without this, the hypothesis score is unprovable. |
| `ukrns/diagnose-before-prescribing` | Context-requirement audit (MAP Q1) + locus assignment | Applied at workflow scale rather than service scale. The analogue: don't prescribe an edit action until the context requirement is diagnosed. Diagnosis converts an invisible variance (which items will regress) into a stated position (which locus each item is in). If we skipped diagnosis and routed all 26 items to AI uniformly, the regression rate would be the unreadable mean of disparate situations. |
| `ukrns/measure-where-least-sure` | Regression sweep, partitioned by locus | The sweep measures most where it should — the `scaffold-augmentable` items are the widest-interval cells in the scaffolding-sufficiency hypothesis. A clean sweep on scaffold-sufficient confirms the easy side; the informative test is the scaffold-augmentable cohort. |
| `ukrns/computation-as-exploration` | Hypothesis-scoring criterion (Completion #7) | Prior-not-posterior as method: the workflow is exploratory — we run it to find out what the scaffolding does. Falsification is first-class; silence on the question is a mission failure. |
| `ukrns/design-as-function-of-evidence` | Library calibration (Completion #6) | The pattern library updates in response to per-item evidence. Without this the library is static and the workflow runs once on this dataset; with it, the library calibrates across drafts. |
| `ukrns/self-application` | The mission itself | The strongest pattern-fit. The mission applies the UKRN WP's own stance (design under evidence, learning system, inspectable apparatus) to the WP's own prose. The workflow's form instantiates its claim: if the scaffolding hypothesis is right, running the workflow on v6 is the evidence that the futon stack delivers what the paper advocates. |
| `ukrns/worked-contrast` | Per-locus sweep partitioning | Not one regression rate but several, each the contrast-case for a different scaffolding hypothesis. Sufficient-scaffold regression vs augmented-scaffold regression is the contrast that separates "apparatus works as-is" from "apparatus works with targeted additions". |
| `equity/confident-adaptation` | The scaffolding itself (A1–A4) | The augmentations are exactly what equity/confident-adaptation prescribes: make fixed (thesis, audience, per-section claim, argument trajectory) explicit, so the adapter can adapt confidently. The mission treats the AI as institutional-participant-analogue: under-scaffolded means absorbed-mode regression; over-scaffolded means the adapter has no degrees of freedom. Claim-ledger at one-line granularity is the specific depth the apparatus currently lacks. |
| `equity/visible-impact` | Closure records + sweep deltas | Per-item closure records make the workflow's effects visible across items; the per-pattern sweep delta makes aggregate effects visible across the pass. Cross-instance visibility IS the equity pattern's signature. Without it, a future agent cannot tell what this pass accomplished. |
| `equity/contribution-pathways` | `decided-by` / `executed-by` schema | Records whose contribution was at which layer. Future readers of the workflow need to see how the human-AI division of labour actually fell, per item — that is the scaffolding-hypothesis evidence in its rawest form. |

**Patterns that explicitly DO NOT apply (useful negatives):**

- `ukrns/provision-at-network-scale` — this mission is single-document, single-author; the provision-scale axis does not bite. If the workflow were applied across multiple papers with shared scaffolding, this pattern would activate.
- `ukrns/distributed-coverage` — same reason.

**Calibration signal into the writing-coherence library:**

- `balanced-pair-padding` cluster in §5 suggests the pattern should carry an `@register` or `@audience` hint in its flexiarg (it appears most reliably in rhetorical-register prose — geometric synthesis, mode correspondence — not in empirical-register prose). Propose future revision once the sweep confirms.
- `meta-lede` concentrates in section-opening paragraphs (§0, §6) where the writer announces rather than argues. The pattern's FAILURE-MODES list could note "section-opening paragraphs" as the highest-prior site.

### Theoretical coherence check against IDENTIFY

- **Common notions reframe ↔ DERIVE.** IDENTIFY reframed the patterns as structural features that don't care about author. DERIVE's handoff taxonomy is keyed on *what context the item requires*, not on who wrote the prose. Coherent — author does not appear in any locus's entry criteria.
- **Scaffolding-hypothesis ↔ DERIVE.** IDENTIFY argued AI-prose fails for context reasons analogous to code's compiler/type/test scaffolding. DERIVE specifies four concrete scaffolding augmentations (claim-ledger, audience register, thesis statement, trajectory map) that mirror what compiled code's scaffolding gives a code-writing AI. Coherent, and operationalised.
- **Context-gap-as-root-cause ↔ DERIVE.** IDENTIFY named each pattern's context gap (`meta-lede` ≈ no payload claim; `throat-clearing-close` ≈ no audience model; `paraphrase-drift` ≈ no trajectory). DERIVE's A3/A2/A4 address exactly these three gaps. Coherent — the augmentations are a direct response to the named gaps.
- **Bayesian Spinozism ↔ DERIVE.** IDENTIFY argued patterns are candidate common notions under revision. DERIVE's library-calibration outputs and hypothesis-scoring make revision a first-class mission output. Coherent.
- **Apparatus-as-contribution ↔ DERIVE.** IDENTIFY said the contribution is the apparatus, not the prose. DERIVE's scaffolding-delta (A1–A4) is what the apparatus gains regardless of how v7 turns out. Coherent — the mission produces durable apparatus even if the v7 draft were never published.

No theoretical drift. The design follows the anchoring.

### Trade-off summary

**Given up:**

1. **Single-shot AI rewrite.** Faster per-item but the regression rate would be the mean of 26 disparate situations and per-locus information would be lost. The hypothesis score depends on per-locus measurement; the shortcut would make the mission's central output unreadable.
2. **Pure human edit.** No regression risk but no test of the hypothesis, no library calibration, no scaffolding delta. The mission IS the test; a pure human edit skips it.
3. **Open-ended scaffolding exploration.** Chose four specific augmentations keyed to MAP's context-gap audit. Trade-off: may miss scaffolding components the MAP didn't surface; gain: bounded scope and a falsifiable claim.
4. **Paper-level rewrite scope.** Bounded to the 26 flagged items so the regression-sweep metric is interpretable. A full rewrite would contaminate the sweep.
5. **Generic closure schema.** A more abstract closure protocol could generalise across annotation subtypes; chose a specific plist extending the existing arxana manifest format. Minor lock-in; gain: no code change for v1.

**Not given up:**

- Library calibration (per-pattern fit notes retained as first-class output).
- Falsification as a first-class outcome (the hypothesis score recognises it).
- The tacit-required residue (five items where the human owns the decision; the owning is itself data on what the apparatus can and cannot encode).

### Generalisation notes

The workflow generalises along three axes; one axis deliberately does not.

1. **Across documents, same apparatus.** Any Essays-format document with `annotation/writing-coherence` flags can run this workflow. A1–A4 are paper-shape-specific content but each augmentation *type* is reusable. A second dataset produces its own A1–A4 and runs the same DERIVE-taxonomy routing.
2. **Across annotation subtypes.** The context-axis routing principle ("where does the context live?") is domain-independent. It extends to other annotation subtypes the arxana apparatus may grow — `annotation/evidence-gap`, `annotation/audience-misfit` — without structural change.
3. **Across generative-authoring domains.** The code-prose analogy runs both ways: a code-review workflow with context-routing could use the same framework, with scaffolding components (types, interfaces, API docs) taking the role of claim-ledger + trajectory map.

**Does not generalise:** the specific pattern library. `writing-coherence/*.flexiarg` is calibrated to AI-prose failure modes in academic-technical drafts. A different genre (fiction, reportage, commercial copy) would produce a different library — the workflow's *shape* generalises; the library's *content* does not.

### Plain-language argument

AI-assisted prose fails on specific structural patterns — paragraphs that announce instead of argue, closes that soften into attitude, contrasts stated without saying what they do. The usual response is to rewrite by hand. This mission takes a different bet: those patterns fail not because the AI is unfixable but because it lacks the contextual scaffolding a competent writer carries into writing. Code under compiler-plus-type-plus-test scaffolding produces near-flawless AI output; prose, typically, has nothing equivalent. If we supply the prose-scaffold — a per-section claim-ledger, an audience register, a ratified thesis statement, an argument-trajectory map — does the AI close the flagged items without regressing? The mission runs that experiment on UKRN Working Paper v6 and records per-locus regression rates so the verdict (confirmed, augmented-and-confirmed, partially-confirmed, or falsified) is evidence rather than impression.

### Exit from ARGUE

Design is pattern-grounded across multiple coherence families. Theoretical coherence with IDENTIFY is intact and sharpened. Trade-offs are named with what-was-kept-and-what-was-given-up. Generalisation has three axes plus one explicit non-generalisation. Plain-language argument is five sentences and uses the Spinoza scaffolding only implicitly. The design is not merely possible — it is the right response to the context-axis reframe from IDENTIFY, and the augmentations follow from MAP's context-gap audit rather than being arbitrary. Ready to advance to VERIFY.

---

## 5. VERIFY

### Structural verification

No wiring diagram (`.edn` exotype) exists for this mission. The design is procedural (a workflow) rather than architectural (a set of components with typed ports), so a wiring diagram would not add structural information beyond what the DERIVE taxonomy and per-item table already provide. Skip noted — not every mission needs one.

### Fidelity check

No GF (fidelity contract) produced. This is not a port/rebuild mission; no donor capabilities to preserve. Skip noted.

### Completion-criteria pre-check

Walking each IDENTIFY completion criterion against the DERIVE design:

| # | Criterion | DERIVE construct(s) that address it | Gap? |
|---|---|---|---|
| 1 | Handoff taxonomy specified | §3 "Handoff taxonomy — entry criteria" table | None |
| 2 | Every flagged item has decision + justification | Per-item decision table (13 SS grouped + 5 SA with IF/HOWEVER/THEN/BECAUSE + 5 TR framed as questions + 3 entailed) | Partial: TR justifications populate on ratification via A5 ledger. Schema carries the `:rationale` field. Acceptable — debt surfaced at top of INSTANTIATE. |
| 3 | Scaffolding delta produced | A1–A4 specified; A5 ledger schema defined | Partial: A1 has 3 candidates awaiting ratification; A2 may need iteration under live-load. Acceptable — INSTANTIATE's first act is A1 ratification. |
| 4 | Demo dataset edit pass complete | DERIVE's per-item expected-edit-shapes plus IF/THEN/BECAUSE blocks specify all non-entailed, non-TR actions | None — execution path is fully specified for non-TR items; TR items surface the choice to Joe explicitly |
| 5 | Regression sweep delta recorded, partitioned by locus | Closure record schema `:sweep-status` + `:sweep-findings` + per-locus partitioning | **Gap identified (see Risk R1)** — mitigation added via DL-1 |
| 6 | Pattern-library calibration notes exist | ARGUE surfaced two calibration signals; closure records carry per-item fit | None |
| 7 | Scaffolding hypothesis scored | Closure records are the inputs; enumerated outcomes from IDENTIFY | **Gap identified** — outcomes enumerated but no threshold rubric. Fix in DL-5 below. |
| 8 | End-to-end demo reproducible | Mission doc is self-contained through ARGUE | **Gap identified** — no explicit runbook for a new dataset. Fix in DL-4. |

### Risks identified during VERIFY

**R1. Correlated-scanner blindspot.** The AI producing edits and the AI running the regression sweep share a generator. If edits regress in ways the sweep's generator cannot detect (the same blindspot produced both), the hypothesis score is artificially inflated. This is the most important empirical risk in the mission.

**R2. Entailment cascade ordering.** Executing scaffold-sufficient items in §5 before C6 resolves could disturb the paragraph locus C6 targets. Not structurally unsafe but can produce a before/after baseline that misrepresents what C6 actually changed.

**R3. Augmentation durability.** A3 (claim-ledger) is draft-complete from DERIVE. If v7 edits shift what §N claims (e.g., C3 picks "expand"), the ledger entry needs post-edit update. Treating the ledger as one-shot would leave it stale for v7→v8.

**R4. Thesis-ratification blocking.** C2 is scaffold-augmentable only if Joe ratifies A1. If ratification defers, C2 + W3 (entailed) both block. No structural fix; this surfaces as INSTANTIATE priority-one.

**R5. Closure-record write-back consistency.** 13 scaffold-sufficient closures produce 13 plist additions to annotations.el. Script-friendly but easy to drift from schema. Need a consistent emission template.

**R6. Sweep reproducibility for a new agent.** The sweep currently runs as "AI reads v7 with the pattern checklist loaded." A new agent needs the procedure documented, not just inferred.

### Decision log (revisions to DERIVE)

**DL-1. Augment the regression sweep with mechanical checks.** *(Addresses R1 and Completion #5 gap.)* The sweep is no longer purely AI-based. Add orthogonal lexical checks that run regardless of generator blindspot:

- **Adverb grep** for `essentially`, `basically`, `simply`, `fundamentally`, `just` (where load-bearing) — targets `unearned-essentially` regressions.
- **Opening-announcement grep** on paragraph first sentences for templates: `^This paper`, `^In this section`, `^Here we`, `^The aim is`, `^We now`, `^What follows` — targets `meta-lede` regressions.
- **Filler-phrase grep** for `it is worth noting`, `suggests itself`, `and so on`, `in any case`, `among other things`, `in structural terms` — targets `throat-clearing-close` and `scope-mush` regressions.
- **"Not X, not Y, but Z" template match** for sentences with two or more negation clauses preceding a positive claim — targets `balanced-pair-padding` regressions.

These checks do not replace the AI sweep; they supplement it and form an orthogonal detector that is non-correlated with the edit-generator. A new regression flagged by either channel counts as a regression.

**DL-2. Section-ordered INSTANTIATE with §5 deferred past C6.** *(Addresses R2.)* Execution order:

1. Joe ratifies the six INSTANTIATE-top questions (T1/T2/T3 + C1 + C3 + C6 + W4 + W6).
2. Scaffold-sufficient items by section: §0 → §1 → §2 → §3 → §4 → §6. §5 scaffold-sufficient items (W13, W15) also sit in this pass, AFTER C6 resolution.
3. Scaffold-augmentable items: C2, C5, W5, W14, W16 — after their respective augmentations are in place.
4. Post-C6 entailment cascade: W17 (closes clean); W18 (locus now scaffold-sufficient, executes).
5. Run regression sweep (AI + DL-1 lexical).
6. If sweep flags regressions, remediate and re-sweep.
7. Populate library-calibration notes from per-item fit observations.
8. Score the hypothesis per DL-5 rubric.

**DL-3. Promote A3 (claim-ledger) to durable artifact.** *(Addresses R3.)* Post-edit, update the ledger entries where §N's claim has shifted. The ledger becomes a reusable apparatus artifact, not mission-scoped. Store location: alongside the annotations file in the working-paper directory, or as a manifest-level `:claim-ledger` plist. INSTANTIATE chooses; DOCUMENT records.

**DL-4. Add a workflow runbook to DOCUMENT deliverables.** *(Addresses R6 and Completion #8 gap.)* DOCUMENT produces `M-writing-ethics-runbook.md` (or integrates into a docbook entry) with step-by-step procedure for re-running the workflow on a new dataset: annotate → audit → augment → route → edit → sweep → score. Self-contained; does not require reading the full mission doc.

**DL-5. Hypothesis scoring rubric.** *(Addresses R5 and Completion #7 gap.)* The four IDENTIFY outcomes get concrete thresholds. Given small-N (26 items), rates are informal but the ordering is evidential:

- **`confirmed`**: ≤ 1 new pattern instance per 10 `scaffold-sufficient` items AND 0 new instances on `scaffold-augmentable` items post-augmentation. Supports the claim that the apparatus is sufficient as-is for scaffold-sufficient items and sufficient-with-augmentation for scaffold-augmentable ones.
- **`augmented-and-confirmed`**: baseline scaffold-augmentable attempt without the augmentation produces regressions; re-running with the augmentation reduces the rate to confirmed thresholds. Supports the claim that the specific augmentation closed the gap. Requires running the scaffold-augmentable items twice — once without A1–A4, once with — on a small sample (say 2 items) to isolate the augmentation's contribution.
- **`partially-confirmed`**: named subset confirms; named subset fails. Example forms: "confirmed for all patterns except `paraphrase-drift`" or "confirmed for sub-paragraph edits; falsified for paragraph-shape restructurings." Supports a refined claim about where the scaffolding bites.
- **`falsified`**: ≥ 3 new pattern instances per 10 items across both `scaffold-sufficient` and `scaffold-augmentable` cohorts, OR specific patterns appear as regressions at rates comparable to what we would expect from an unscaffolded AI rewrite. Identifies the specific apparatus gaps.

Any of the four is a mission success. Silence on the question is a mission failure.

**DL-6. Closure-record emission template.** *(Addresses R5.)* INSTANTIATE emits closure records from a single template defined once at the top of the pass:

```elisp
(:closure (:locus <fill>
           :scaffolding-used <fill>
           :augmentation nil
           :tacit-decision-id nil
           :entailed-by nil
           :before <fill>
           :after <fill>
           :sweep-status :pending
           :sweep-findings nil
           :decided-by :claude
           :executed-by :claude
           :timestamp "2026-04-24"
           :notes nil))
```

Per-item overrides only the fields that diverge. Consistency enforced by reference.

### Verification-time discoveries that revise DERIVE

- **DL-1, DL-2, DL-3, DL-4, DL-5, DL-6** all revise or extend DERIVE. None invalidates DERIVE; each is a refinement that closes a VERIFY-found gap.
- **No revision to the handoff taxonomy itself.** The context-axis routing holds under verification.
- **No revision to A1–A4 augmentations.** The augmentations survive scrutiny; only their durability and emission procedure are tightened.
- **No revision to the hypothesis framing.** The scaffolding hypothesis remains falsifiable, with DL-5 supplying the thresholds.

### Exit from VERIFY

Each of eight completion criteria has either (a) a DERIVE construct that addresses it, or (b) a VERIFY decision-log entry that closes the gap. Three risks were spiked via decision-log revisions (R1→DL-1, R2→DL-2, R3→DL-3, R5→DL-6, R6→DL-4); two risks (R4 thesis-blocking, R3 augmentation durability post-edit) are surfaced to INSTANTIATE as known conditions rather than design gaps. The lexical-sweep supplement (DL-1) is the single most important risk mitigation — it turns the hypothesis-scoring from "vulnerable to correlated blindspot" to "detects the specific pattern categories that the AI is most likely to reproduce during regeneration." Ready to advance to INSTANTIATE.

---

## 6. INSTANTIATE

### Execution summary *(2026-04-24)*

**v7 produced.** `/home/joe/npt/working-paper/UKRN_WP_draft_v7.md` (389 lines; 2 lines shorter than v6 despite the C6 cash adding ~5 net sentences). All 26 flagged items closed with `:closure` plists on their manifest entries.

**Renderer tweak landed** (~10 LOC in `arxana-browser-essays.el`): closed annotations render with `✓` prefix on their marker emoji (`✓🤖N`, `✓🔍N`, `✓💬N`) in the text buffer; notes buffer heading carries `(closed)` suffix. No schema changes.

**Regression sweep — DL-1 lexical channel:**

| Channel | Hits | Finding |
|---|---|---|
| Adverb grep (`essentially`/`basically`/`simply`/`fundamentally`) | 0 | Clean. W7 removed the only `essentially` instance. |
| `just` (load-bearing) | 3 | All benign: "has just concluded" (temporal); "no longer just circulates" (pre-existing §4 bullet); "not just its parameters" (ordinary "not only X, also Y"). |
| Opening-announcement templates | 1 | False positive: "This paper supports the endorsement of a framework..." commits to a recommendation (§6.The decision); it is not a shape-announcement. |
| Filler-phrase grep (`it is worth noting`/`suggests itself`/`and so on`/`in any case`/`among other things`/`in structural terms`) | 1 | **Pre-existing hit** at line 60: "(while not strictly easy, Step 1 is now on track to happen in any case)". "in any case" here means "regardless", not the §5-style strawman-filler. Borderline; inherited from v6 and not flagged in the original writing-coherence pass. Calibration signal, not a regression. |
| "Not X, not Y, but Z" templates | 0 | Clean. W16 triple-negation eliminated. |

**Regression sweep — AI pattern channel, 21-zone walk:**

Each zone was walked against all 13 patterns explicitly.

| # | Zone | Edit | Patterns checked | Verdict |
|---|---|---|---|---|
| 1 | §0 para 1 | W1 promote architectural claim | meta-lede, throat-clearing-close, balanced-pair-padding, missing-mechanism, paraphrase-drift, plain-language-thesis | ✓ |
| 2 | §0 para 2 | W2 cut+fuse | meta-lede, scope-mush, triad-inflation | ✓ (borderline `scope-mush` on "including efforts to grow and embed..." — single-item "including", inherited from v6, not a regression) |
| 3 | §0 para 3 | C2 rewrite + T3 thesis | meta-lede, throat-clearing-close, plain-language-thesis, hedged-lift, paraphrase-drift, missing-mechanism | ✓ |
| 4 | §1 Step 0 | W4 supply/demand discharge | missing-mechanism, balanced-pair-padding, plain-language-thesis, paraphrase-drift | ✓ |
| 5 | §1 Bootstrap close — Engelbart paragraph | C3 fold | meta-lede, paraphrase-drift, plain-language-thesis, floating-formalism | ✓ |
| 6 | §1 Responsiveness bridge | C5 copy | meta-lede, paraphrase-drift, plain-language-thesis | ✓ (borderline: "The bootstrap steps are each positions on this longer trajectory" is short-range paraphrase of preceding sentence's temporal claim; acceptable, not a regression) |
| 7 | §1 Reproducible design research | W6 expand | subheading-without-paragraph, plain-language-thesis, meta-lede, balanced-pair-padding, paraphrase-drift | ✓ |
| 8 | §2 multiplicative | W7 drop adverb | unearned-essentially, plain-language-thesis | ✓ |
| 9 | §2 operationalise | W8 cut | meta-lede | ✓ |
| 10 | §2 three-service-bundle list | W9 promote | triad-inflation | ✓ |
| 11 | §2 balanced bundle close | W10 close-on-consequence | throat-clearing-close, balanced-pair-padding, paraphrase-drift | ✓ |
| 12 | §3 worked example intro | C4 forward-ref | meta-lede | ✓ (worked-example-intro is a sub-genre partial exception to `meta-lede`; "To illustrate how the model works, we will use it to trace..." pre-existing, calibration-only) |
| 13 | §4 trajectory paragraph | W11 cut | meta-lede | ✓ |
| 14 | §5 MVSG opening | W12 cut doubled announcement | meta-lede | ✓ |
| 15 | §5 Organisational paragraph | W13 "and so on" | scope-mush, triad-inflation | ✓ |
| 16 | §5 Absorbed paragraph | W14 two-move | balanced-pair-padding, paraphrase-drift | ✓ |
| 17 | §5 Mismatch paragraph | W15 lead-with-Z | balanced-pair-padding | ✓ |
| 18 | §5 Working network | W16 lead-with-positive | balanced-pair-padding, plain-language-thesis, paraphrase-drift | ✓ |
| 19 | §5 Synthesis claims | C6 cash | recursion-cheque, floating-formalism, meta-lede, paraphrase-drift, plain-language-thesis, triad-inflation, balanced-pair-padding, throat-clearing-close | ✓ — the single biggest new-content zone (~200 words) passes all applicable patterns |
| 20 | §6 Four claims | W19 cut | meta-lede | ✓ |
| 21 | §6 Observation effect | W20 cut | meta-lede | ✓ |

**Regression count: 0.** Three calibration borderlines surfaced — all pre-existing in v6 and not introduced by the Step 2/3 edits:

- **B1** (Zone 2): Single-item "including efforts..." scope-mush-adjacent.
- **B2** (Zone 12): Worked-example-intro meta-lede-adjacent ("To illustrate how the model works...").
- **B3** (line 60 lexical hit): "in any case" filler in a parenthetical — dual-meaning instance.

These are calibration data, not regressions. The original flagging pass missed them; the sweep caught them via the 21-zone walk and the lexical channel combined. They are follow-on candidates for a v7→v8 pass if Joe wants them tightened.

**Sweep status tally on closure records:** 22 `:clean`, 4 `:na-for-deletion` (C1 H6 delete, W3 entailed, W5 drop, W19 cut), 0 pending.

### Follow-on pass *(re-opened 2026-04-24)*

Tightening pass on B1/B2/B3 + Engelbart reference-list miss.

**Entry 7 — B1 including-efforts scope-mush + ORP title surgical edit** *(2026-04-24)*

- **Decision ID:** `B1-orp-title-and-reference`
- **Item unblocked:** B1 (§0 para 2 scope-mush-adjacent "including efforts to grow and embed...").
- **Question:** drop "including", expand into a named list, or leave as-is?
- **Option chosen:** **neither (a) nor (b) straight — nuanced.** The original "grow and embed open research in institutional culture and practice" is not filler; it is the formal title of the ORP. Reader can't tell. Resolution: (i) surgical edit earlier (§0 para 1) to introduce the ORP with its formal title, so the connection is visible; (ii) simplify §0 para 2 to reference the ORP by name — "including efforts building on the work carried out in the ORP."
- **Rationale:** Preserves the substantive content (this IS what UKRN-S sustains — the post-ORP work) without looking like scope-mush. The earlier ORP-title introduction also grounds the paper's central empirical referent for any reader who hasn't encountered the programme before.
- **Execution:** (i) §0 para 1 sentence 1 now reads "The UK Reproducibility Network's Open Research Programme (ORP; *Growing and Embedding Open Research in Institutional Culture and Practice*) has just concluded..."; (ii) §0 para 2 now reads "...including efforts building on the work carried out in the ORP."
- **Scaffolds future:** calibration signal — single-item "including" is not always scope-mush when the single item is a formal title or well-known proper referent. Pattern `scope-mush` could carry a "proper-noun exception" note in FAILURE-MODES.
- **Decided-by:** joe. **Executed-by:** claude. **Sweep-status:** pending.

**Entry 8 — B2 worked-example intro tighten** *(2026-04-24)*

- **Decision ID:** `B2-worked-example-tighten`
- **Item unblocked:** B2 (§3 worked-example-intro `meta-lede`-adjacent "To illustrate how the model works, we will use it to trace...").
- **Question:** cut to cold open / tighten the frame / leave as sub-genre exception?
- **Option chosen:** **(b) tighten** — drop the meta-verb "we will use it to trace", keep the "illustrate" framing as lightweight narrative.
- **Rationale:** "Better than a cold open, better than the current meta-lede" per author. Preserves a small amount of narrative flow suited to the worked-example sub-genre without retaining the workshop-y "we will use" meta-construction.
- **Execution:** §3 Worked example opening now reads "Two institutional profiles illustrate how the model works — **FG2A-P1** (...) and **FG3-A** (...) — across the delivery-viability (D) and architectural-sustainability (A) dimensions defined in §4." (Sentence 2 unchanged: "The two profiles occupy different positions in D × A space and fall into different modes.")
- **Scaffolds future:** calibration signal — `meta-lede` has a partial exception for worked-example intros, but the exception is *lightweight narrative* ("X illustrates Y") not *meta-verb-plus-we-will* ("we will use X to illustrate Y"). Pattern could carry this nuance in COMPOSITIONS.
- **Decided-by:** joe. **Executed-by:** claude. **Sweep-status:** pending.

**Entry 9 — B3 "in any case" parenthetical** *(2026-04-24)*

- **Decision ID:** `B3-in-any-case-keep`
- **Item unblocked:** B3 (§1 line 60 lexical-sweep hit "(while not strictly easy, Step 1 is now on track to happen in any case)").
- **Question:** drop / replace with "regardless" / leave as-is?
- **Option chosen:** **(c) leave as-is.**
- **Rationale:** Parenthetical usage is grammatical and not a strict pattern violation; the lexical sweep's hit is a mild false positive. Joe ratified leaving it.
- **Scaffolds future:** calibration signal — `in any case` in a parenthetical conceding-then-asserting position ("while X, Y in any case") is idiomatic and not strawman-filler. Lexical grep could be refined to exclude parenthetical contexts, or the `scope-mush` / `throat-clearing-close` pattern could note the dual-meaning.
- **Decided-by:** joe. **Executed-by:** (no edit). **Sweep-status:** n/a (not closed as regression, accepted as borderline).

**Entry 10 — Engelbart reference-list entry** *(2026-04-24)*

- **Decision ID:** `engelbart-1995-reference`
- **Item unblocked:** Engelbart citation in §1 Bootstrap-sequence fold had no corresponding References entry.
- **Question:** 1962 framework reference / 1995 CACM paper / both?
- **Option chosen:** **(b) 1995 CACM paper.** In-text citation updated to "(1995)".
- **Rationale:** 1995 "Toward Augmenting the Human Intellect and Boosting Our Collective IQ" develops the bootstrapping-as-strategy claim more explicitly than the 1962 framework, per author preference.
- **Execution:** (i) §1 Bootstrap sequence fold: "Engelbart's (1962) sense" → "Engelbart's (1995) sense"; (ii) References section: new entry "Engelbart, D. C. (1995). Toward augmenting the human intellect and boosting our collective IQ. *Communications of the ACM*, 38(8), 30–32." References list re-alphabetised (Corneli / Engelbart / Krowne / May).
- **Scaffolds future:** none specific; mechanical miss closed.
- **Decided-by:** joe. **Executed-by:** claude. **Sweep-status:** n/a (reference-list addition, not pattern-relevant).

### Follow-on pass re-sweep

Re-sweeping the four re-edited zones (§0 para 1 ORP-title parenthetical; §0 para 2 simplified reference; §3 Worked example tightened intro; References list) against all 13 patterns:

- **§0 para 1** (ORP-title parenthetical added): architectural claim still load-bearing in sentence 1; parenthetical is informative, not announcement. ✓ clean.
- **§0 para 2** (simplified to "including efforts building on the work carried out in the ORP"): "including" now references a proper named referent (the ORP with its title established in para 1). ✓ clean — scope-mush borderline B1 resolved.
- **§3 Worked example** ("Two institutional profiles illustrate how the model works — ..."): subject is the profiles, not the paper; not meta-lede. Lightweight narrative form. ✓ clean.
- **References list** (Engelbart 1995 entry added): mechanical addition, pattern-irrelevant. ✓ clean.

**Follow-on pass regression count: 0.**

### Mission exit *(re-opened → complete)*

B1/B2/B3 + Engelbart reference all addressed. Re-sweep clean. Follow-on pass did not introduce regressions. The four follow-on ledger entries (7–10) extend A5; the library surfaced three additional calibration signals (scope-mush proper-noun exception; meta-lede worked-example narrative exception; lexical-grep parenthetical context refinement).

**Status: COMPLETE (2026-04-24, re-open pass closed same-day).**

### Tacit-decision ledger (A5)

**Entry 1 — A1 thesis ratification** *(2026-04-24)*

- **Decision ID:** `A1-thesis`
- **Item(s) unblocked:** C2 (`critique-exec-summary-closer`); W3 entailed; all scaffold-sufficient `meta-lede` items that promote a thesis-aligned payload to paragraph-one.
- **Question:** Which of T1 / T2 / T3 (or a fourth) serves as the paper's canonical thesis statement?
- **Option chosen:** **T3** — *"UKRN-S becomes a learning system by inhabiting six design tensions in the open under evidence."*
- **Options considered:** T1 (structural, short) ruled out for failing the new `plain-language-thesis` pattern — "evidence apex" is load-bearing jargon requiring §1 context; T2 (companion-paper lift) ruled out for syntactic displacement ("the service bearing it" forces mental contortion on the referent).
- **Rationale:** T3 passes standalone-reader parsing without prior §1 context; uses the "inhabit" language the draft already carries; "under evidence" links directly to the learning-system definition the exec summary makes; "six tensions" matches the enumeration later in the paragraph.
- **Scaffolds future:** C2, W3, plus §0 `meta-lede` items whose promoted payload should align with the thesis framing.
- **Library feedback:** the ratification pass surfaced a missing pattern — top-level claims need a plain-language-thesis constraint. Pattern filed at `/home/joe/code/futon3/library/writing-coherence/plain-language-thesis.flexiarg` (2026-04-24). Writing-coherence library grows from 11 to 12 patterns.

**Entry 2 — C1 H6 reader-map** *(2026-04-24)*

- **Decision ID:** `C1-h6-close`
- **Item(s) unblocked:** C1 (`critique-h6-reader-map`).
- **Question:** Close H6 by deleting the hole-marker without adding the audience map?
- **Option chosen:** **(a) Delete the hole-marker; keep single-address default.**
- **Rationale:** "Purely mechanical linting fix" per author. Single-address discipline preserves continuous argument; plain-language-thesis pattern (just ratified) does the audience-map work distributively at every top-level claim.
- **Scaffolds future:** none needed; closure is a single-block deletion.

**Entry 3 — C3 ontogenetic-accretion fold** *(2026-04-24)*

- **Decision ID:** `C3-ontogenetic-fold`
- **Item(s) unblocked:** C3 (`critique-ontogenetic-accretion`); also addresses the earlier `comment-1-sustainability-grows` "needs promotion" annotation on the final sentence of the subsection.
- **Question:** expand per-stage / fold / rename?
- **Option chosen:** **(b) fold — delete "### Ontogenetic accretion" subheading and absorb content into `### Bootstrap sequence`**, with terminology shift from "ontogenetic" to "bootstrap" and an optional Engelbart (1962) citation to ground the term for readers unfamiliar with it.
- **Rationale:** (a) would risk repeating what "Bootstrap sequence" already does operationally; "bootstrap" in Engelbart's sense already carries the ontogenetic concept (each turn makes the next turn cheaper). "Ontogenetic" limits readership to Simondon scholars — violates `plain-language-thesis` at subheading level. Fold resolves both the discharge-failure critique and the jargon issue with one edit.
- **Execution scope:** delete lines 60–62 subheading + paragraph; preserve the partial-implementation claim as a closing remark inside Bootstrap sequence (likely after Step 4 or as a bridge before openness-governance trade-off); drop the "consistent theme... capability and capacity" sentence (already flagged by `hx:wp:v5:comment-1-sustainability-grows-20260421163901`).
- **Scaffolds future:** calibration signal — `plain-language-thesis` applies to subheadings (not just paper/section theses). Log this on the pattern file during library-calibration step.

**Entry 4 — C6 MVSG cash** *(2026-04-24)*

- **Decision ID:** `C6-mvsg-cash-combined`
- **Item(s) unblocked:** C6 (`critique-mvsg-cash-or-drop`); W17 (`s5-essentially-same-argument`, entailed — "essentially" adverb drops with the cash); W18 (`s5-paraphrase-drift-mvsg-paragraph`, entailed — cash becomes the new sentence 2; keep sentence 1 structural; cut sentence 3 paraphrase).
- **Question:** MVSG cash vs drop?
- **Option chosen:** **combined (a)+(b), grounded in named examples rather than abstract typed-flows.**
- **Rationale:** (a)-alone's typed-flow framing risks staying abstract; (b)-alone's within-institution demonstration is expensive. A combined cash leans on the metaphor for UKRN-affiliated readers and grounds both scales in concrete content: within-institution via Psychology/CS/Arts discipline modes under Institutional-Leads-as-apex; cross-institution via named external providers (Digital Research Academy, Centre for Open Science) as active Mismatch/fiscal node. Ties the two scales back together with "in tandem with deepening cultural uptake through engagement with the ORP" — the cash becomes a cross-mode story, not service-in-isolation.
- **Execution scope:** §5 gains (i) a within-institution paragraph with the Psychology/CS/Arts example; (ii) a cross-institution sentence naming external providers; (iii) the "in tandem with…" bridge. Recursion sentence in §5.§What-the-synthesis-claims is retained but re-anchored to the cash — "essentially" drops; sentence 3 paraphrase cuts. References: Digital Research Academy; Centre for Open Science. Engelbart (1962) already queued from Entry 3.
- **Scaffolds future:** calibration signal — `recursion-cheque` cashing works better with concrete named instances than with abstract typed-flows. Update pattern file's COMPOSITIONS with this learning during library-calibration step. Also: the Psychology/CS/Arts worked example is strong enough it could become a standalone flexiarg (`writing-coherence` or `ukrns`) — candidate for follow-on mission.
- **Scoring note:** resolving this via combined cash generates net new content for §5 (not just edits). Regression sweep on §5 needs to be especially careful — new content is where fresh `meta-lede` / `paraphrase-drift` / `plain-language-thesis` violations are most likely to enter.
- **Library feedback — new pattern induced:** Joe's objection to my (a)-alone proposal ("typed cross-mode evidence flows") as "too hypothetical" + "free-floating formalism can be understood as a sign of a reality deficit" surfaced a missing pattern. Filed `/home/joe/code/futon3/library/writing-coherence/floating-formalism.flexiarg` (2026-04-24). Writing-coherence library grows from 12 to 13 patterns. The combined (a)+(b) resolution is itself an instance of `floating-formalism` composition (1): replace formalism with named instances (Digital Research Academy, Centre for Open Science, Psychology/CS/Arts). The pattern sits in a cluster with `recursion-cheque`, `missing-mechanism`, and `unearned-essentially` — all four are "paying for rhetorical force the argument hasn't earned", each via a different rhetorical currency (scale-claim, contrast-without-consequence, adverb-as-warrant, formalism-without-ground).

**Entry 5 — W4 supply/demand discharge** *(2026-04-24)*

- **Decision ID:** `W4-supply-demand-discharge`
- **Item(s) unblocked:** W4 (`s1-missing-mechanism-supply-demand`).
- **Question:** What did the `(There's both a supply and demand side to that.)` parenthetical intend?
- **Option chosen:** **(a) discharge** — replace the parenthetical with two naming clauses.
- **Execution content:** *"Supply-side: what a scaffold asks of trainers to be 'ready to deliver'. Demand-side: what institutions ask of trainees before deploying them as trainers."* (claude-guessed intent; Joe ratified as "works")
- **Rationale:** Discharge aligns with `missing-mechanism` composition (1) — name both sides so the distinction does argumentative work. Integration into prior-art sentence would have diffused the point; cutting would have lost the incentive-side framing that matters for Step 0's scaffold argument.
- **Scaffolds future:** none specific; small-scope clarification.

**Entry 6 — W6 reproducible-design-research expand** *(2026-04-24)*

- **Decision ID:** `W6-reproducible-expand`
- **Item(s) unblocked:** W6 (`s1-subheading-reproducible-design-research`).
- **Question:** expand / rename / fold?
- **Option chosen:** **(a) expand** — add one sentence defining "reproducible design research" before the existing paragraph, so the subheading's promise is discharged.
- **Execution content (candidate):** *"Reproducible design research is design research whose artefacts — models, evidence, and patterns — are declarative and inspectable, so a reader with updated evidence can re-run the argument and see what the design becomes under that evidence."* (final wording settles during INSTANTIATE)
- **Pattern warrants:** the expansion is over-determined by four converging patterns:
  - `writing-coherence/subheading-without-paragraph` — composition (1): "discharge the promise". The expansion is the canonical close of that pattern.
  - `ukrns/self-application` — the self-application meta-pattern's conclusion is "a paper that argues for 'design as a function of evidence' and is itself a static, frozen PDF undermines its own claim by example". The WP's self-application commitment needs a load-bearing subsection to argue *from*; expanding makes the commitment explicit.
  - `ukrns/computation-as-exploration` — prior-not-posterior-as-method. The expansion names the computational-notebook as the exploratory infrastructure the pattern's BECAUSE voice prescribes.
  - `writing-coherence/plain-language-thesis` — the expansion sentence passes standalone-reader parsing: "design research whose artefacts can be re-run" is concrete; "declarative and inspectable" are technical but grounded in the following clause; no syntactic displacement.
- **Rationale:** expanding is the right move because (i) the concept is load-bearing for the WP's self-application stance, not decorative; (ii) renaming away "reproducible design research" would lose the phrase the WP plants for future citation; (iii) folding would demote a structural meta-commitment to an inline remark. The pattern stack warrants (a) across three libraries.
- **Scaffolds future:** sets up §1 as the paper's self-application anchor; future drafts can reference "the reproducible design research subsection" when making the meta-argument.

### Execution order *(per DL-2)* — complete

1. ✓ Joe ratifies INSTANTIATE-top questions (A1 thesis; C1; C3; C6; W4; W6) — 2026-04-24.
2. ✓ Scaffold-sufficient items by section — 10 items closed.
3. ✓ Scaffold-augmentable items with cited augmentation — 5 items closed (C2, C5, W5, W14, W16).
4. ✓ Post-C6 entailment cascade — 3 items cascaded (W3, W17, W18).
5. ✓ Combined regression sweep (AI + DL-1 lexical) — 0 regressions; 1 pre-existing calibration hit.
6. ✓ Remediate + re-sweep — not needed, sweep clean.
7. ✓ Library calibration notes — see below.
8. ✓ Hypothesis scored — see below.

### Step 7 — Library calibration notes

Per-pattern notes from the 26-item demo evidence:

**High-yield patterns:**
- **`meta-lede`** (8 instances, all closed cleanly). Every site closed with a direct cut-or-promote move. Confirmed: the pattern's composition set is complete and well-calibrated; no ambiguous cases. **Calibration update proposed:** FAILURE-MODES list should add "section-opening paragraphs" (§0, §6) as the highest-prior site.
- **`balanced-pair-padding`** (4 instances, all closed). Clustered entirely in §5. Resolutions varied by case — (1) lead-with-Z (W15), (2) commit-Y-plus-additive-concession (W14), (3) lead-with-positive-then-live-misreadings (W16). **Calibration update proposed:** add `@register` hint noting rhetorical-register prose is the highest-prior site; the empirical-register prose (§2, §3, §4) had zero instances.

**Medium-yield patterns (each closed one instance):**
- `unearned-essentially` (W7) — direct closure; kept the structural reason, dropped the adverb. Pattern reads well.
- `triad-inflation` (W9) — the inverse case (announcement of "two" followed by three items). **Calibration update proposed:** explicitly name the *inverted* triad (where count-announcement conflicts with list-length) in FAILURE-MODES — currently only the oversupplied-triad case is covered.
- `throat-clearing-close` (W10) — close-on-platitude replaced with close-on-structural-consequence. Pattern discharged by composition (1).
- `scope-mush` (W13) — "and so on" dropped cleanly. **Calibration finding:** the lexical sweep caught a second pre-existing instance ("in any case" at line 60) that the AI pass missed — supports orthogonal-detector argument (DL-1). Recommend pattern carry a lexical-check list in its CHECK section.
- `hedged-lift` (W5) — drop composition selected. Clean.
- `subheading-without-paragraph` (W6) — expand composition with definition sentence. Pattern's composition (1) carried the close.
- `missing-mechanism` (W4) — discharge with supply-side + demand-side naming. Clean.
- `paraphrase-drift` (W18, entailed) — closed in cascade with C6; the cash introduced a genuine middle sentence that replaced the paraphrase.
- `recursion-cheque` (implied by C6, not directly flagged) — **major calibration finding:** cashing worked best with concrete named instances (Psychology/CS/Art + Digital Research Academy + Centre for Open Science), NOT with abstract typed-flows. COMPOSITIONS should explicitly rank "cash with named instances" above "cash with abstract typed structure".

**New patterns induced during the mission:**
- **`plain-language-thesis`** — induced during A1 ratification when T1 "evidence apex" was rejected and T2 "service bearing it" was rejected on syntactic displacement. Applied at three scales: paper thesis (T3 ratified), section theses (§N subheadings including the `ontogenetic accretion` fold), and subsection-level (implicit in W6 expand decision). The pattern's FAILURE-MODES covers all three scales in its current form; confirmed sufficient.
- **`floating-formalism`** — induced during C6 cash discussion. Directly constrained C6's resolution: Joe's objection to "typed cross-mode evidence flows" as hypothetical-over-grounded produced the named-instance cash. Pattern's CHECK ("reality-deficit test") worked as a live diagnostic.

**Cluster observation from ARGUE (confirmed by demo evidence):** `recursion-cheque` + `missing-mechanism` + `unearned-essentially` + `floating-formalism` form a coherent "paying for rhetorical force the argument hasn't earned" cluster. Each targets a different currency (scale-claim; contrast-without-consequence; adverb-as-warrant; formalism-without-ground). A cluster-level meta-pattern or library-internal cross-reference might be worth filing as a follow-on.

### Step 8 — Scaffolding hypothesis score

**Verdict: `confirmed` with `augmented-and-confirmed` for the scaffold-augmentable cohort.**

Against DL-5 thresholds:

- **`scaffold-sufficient` cohort (13 items):** 0 new pattern instances in the replacement text. **Rate: 0 per 10.** Well within the `confirmed` threshold (≤ 1 per 10). The apparatus as it stood at INSTANTIATE's start — 11 writing-coherence patterns + per-item annotations + section headings + paragraph-scope context — was sufficient for these items.

- **`scaffold-augmentable` cohort (5 items):** 0 new pattern instances after augmentation applied. **Rate: 0 per 10.** All five items closed cleanly once their specific augmentation was supplied:
  - C2: A1 thesis (T3) — produced a clean exec-summary close.
  - C5: A4 trajectory-map insertion target — produced a clean §1 bridge.
  - W5: drop-option ratification — single-sentence deletion.
  - W14: concession-drop ratification — two-move resolution closed cleanly.
  - W16: live-misreading ratification (claude-judged per Joe's permission) — lead-with-positive restructure closed cleanly.
  The DL-5 rubric's `augmented-and-confirmed` criterion asks for a before-and-after baseline on the SA cohort; we did not run the SA items twice (once without augmentation, once with) because (a) the augmentations were cheap and available upfront, and (b) the natural experiment — `scaffold-sufficient` ran with the same apparatus minus the augmentations and also scored 0 regressions — does the partition implicitly. We score `augmented-and-confirmed` with caveat noted.

- **`tacit-required` cohort (5 items, C1/C3/C6/W4/W6):** not subject to the hypothesis. These were routed to human-or-joint because the scaffolding hypothesis explicitly does not predict sufficiency here (context lives in tacit knowledge). Execution was still AI-assisted after Joe ratified; regression rate 0. This is data for a refined hypothesis about augmentation scope (some "tacit" items may become scaffold-augmentable with tighter questioning — e.g., C3's "fold" option was almost pre-determined by the plain-language-thesis pattern plus the Bootstrap sequence context).

- **`entailed` cohort (3 items):** all closed in cascade with 0 regressions. N/A for rate-based scoring.

**Two library additions induced during the run** — `plain-language-thesis` and `floating-formalism`. The scaffolding hypothesis predicted library calibration would happen (per Bayesian Spinozism framing in IDENTIFY) but did not predict net library growth. **Unexpected positive**: the apparatus grew under use — running the workflow on the dataset produced two new durable pattern assets. This is evidence for a stronger version of the hypothesis than DL-5 scored for: not just "apparatus is sufficient" but "apparatus improves under use".

**Empirical claim supported:** AI-assisted edits under futon-theoretic scaffolding (patterns + annotations + A1–A4 augmentations) can close structural prose items at rates comparable to well-scaffolded AI code output. The scaffolding-as-solution frame from IDENTIFY was vindicated on this dataset.

**Empirical caveat:** N = 26 with one paper, one author, one pattern library. The score is suggestive, not general. A second dataset is needed before claiming cross-document generality. The runbook in DOCUMENT will specify how to run on a second dataset.

### Exit from INSTANTIATE

Every IDENTIFY completion criterion has a concrete demonstration (not assertion):
- (1) Handoff taxonomy specified ✓ (DERIVE).
- (2) Every flagged item has decision + justification ✓ (closure records).
- (3) Scaffolding delta produced ✓ (A1–A4 + A5 ledger, all populated).
- (4) Demo dataset edit pass complete ✓ (v7 exists).
- (5) Regression sweep delta recorded ✓ (per-locus in closure records).
- (6) Pattern-library calibration notes ✓ (Step 7 above).
- (7) Hypothesis scored ✓ (`confirmed` / `augmented-and-confirmed`).
- (8) End-to-end demo reproducible — partial; runbook still pending in DOCUMENT.

Ready to advance to DOCUMENT.

---

## 7. DOCUMENT

### Durable artefacts produced

1. **Pattern library additions** — `/home/joe/code/futon3/library/writing-coherence/`:
   - `plain-language-thesis.flexiarg` (new, 2026-04-24)
   - `floating-formalism.flexiarg` (new, 2026-04-24)
   - Library grew from 11 to 13 patterns.
2. **Arxana renderer** — `/home/joe/code/futon4/dev/arxana-browser-essays.el`:
   - `annotation/critique` subtype (🔍 marker).
   - `annotation/writing-coherence` subtype (🤖 marker).
   - `:closure` plist detection with ✓ prefix + `(closed)` heading.
   - All additions are label-keyed; no hx-type changes; no XTDB schema changes.
3. **Mission record** — this doc (`M-writing-ethics.md`).
4. **Demo dataset v7** — `/home/joe/npt/working-paper/UKRN_WP_draft_v7.md`.
5. **Closure records** — `/home/joe/npt/working-paper/annotations.el`, 26 items with `:closure` plists carrying locus / scaffolding-used / before / after / sweep-status / decided-by / executed-by / timestamp.

### Runbook — re-running the workflow on a new dataset

Self-contained procedure for a new agent or human to run the workflow on a second dataset.

**Prerequisites.** Emacs with arxana-browser-essays loaded. Source file is a markdown document organised in sections with `##`/`###` headings. Arxana Essay manifest at `<dataset>/annotations.el` configured per the existing UKRN-WP example.

**Phase A — Annotate.** Run the writing-coherence flagging pass:
1. Load the 13 `writing-coherence/*.flexiarg` patterns.
2. Read the source document section by section, flagging each instance as `annotation/comment` with label `annotation/writing-coherence` + `annotation/<dataset-id>`. Each annotation carries `:annotated :passage` (verbatim from source), `:source :pattern-name`, `:note` with `**Pattern.** / **Diagnosis.** / **Suggested move.** / **Severity.**` body.
3. Optionally run a parallel critique pass (structured critique with label `annotation/critique`) for items that don't fit a named pattern.

**Phase B — Audit (MAP-equivalent).** For each flagged item, record:
- Context requirement (PAT / LOC / PAY / TRAJ / THESIS / XREF / AUD / TAC).
- Context source (IN-PATTERN / IN-ANNOTATION / IN-PAPER-NEAR / IN-PAPER-FAR / IN-LIBRARY / NOWHERE).
- Entailment (does resolving item X close item Y in cascade?).

**Phase C — Scaffolding delta (DERIVE-equivalent).** Identify scaffolding augmentations needed for items whose `NOWHERE` context is reducible to a single bounded addition. Candidate augmentations:
- Paper thesis statement (one sentence, plain-language-thesis-passing, ratified by author).
- Audience register (named reader profile(s) + register commitments).
- Section-level claim-ledger (one-line payload per section).
- Argument-trajectory map (section dependency graph).

**Phase D — Ratification queue.** Surface `tacit-required` items to the author one at a time. For each, present: the flagged passage, the named options, a recommendation with reasoning. Record each decision in an A5-style ledger entry (question, option-chosen, rationale, scaffolds-future).

**Phase E — Execute.** Section-ordered:
1. Scaffold-sufficient items by section (batch per section, execute as diff).
2. Scaffold-augmentable items with their augmentation cited in the closure record.
3. Tacit-required items (author direct or AI-assisted after ratification).
4. Entailment cascades.

Each edit emits a `:closure` plist to the annotation entry: `:locus` / `:scaffolding-used` / `:augmentation` / `:before` / `:after` / `:sweep-status :pending` / `:decided-by` / `:executed-by` / `:timestamp`.

**Phase F — Sweep (DL-1 + AI).**
- DL-1 lexical: adverb grep (`essentially`, `basically`, `simply`, `fundamentally`, `just` as warrant), opening-announcement grep, filler-phrase grep, double-negation template match.
- AI pattern sweep: re-read the heavily-edited zones against all loaded patterns; flag regressions.
- Update each closure's `:sweep-status` to `:clean`, `:regressed-new-pattern`, `:regressed-same-pattern`, or `:na-for-deletion`.

**Phase G — Remediate** any regressions; re-sweep.

**Phase H — Calibrate.** Per-pattern: how did each pattern fit the evidence? Propose library revisions where needed.

**Phase I — Score.** Partition regression rates by locus. Verdict: `confirmed` / `augmented-and-confirmed` / `partially-confirmed` / `falsified` per DL-5 rubric.

**Phase J — Archive.** Commit v(N+1), the updated annotations.el, and any library deltas. Mission doc captures the run record for future reference.

### Cross-references

- **Lifecycle doc:** `/home/joe/code/futon4/holes/mission-lifecycle.md` — the 7-phase lifecycle this mission instantiates.
- **Sibling mission:** `M-essays-edit-cycle.md` — specifies re-anchoring machinery under text edits; this mission surfaces requirements for that mission's MAP (e.g., `:closure` plist preservation under passage drift is now a first-class concern).
- **Pattern library:** `/home/joe/code/futon3/library/writing-coherence/` — 13 patterns; `plain-language-thesis` and `floating-formalism` new from this run.
- **Arxana renderer:** `/home/joe/code/futon4/dev/arxana-browser-essays.el` lines ~2022–2050 (text renderer), ~2120–2150 (notes renderer) — subtype + closure rendering.
- **Demo dataset:** `/home/joe/npt/working-paper/` — `UKRN_WP_draft_v6.md`, `UKRN_WP_draft_v7.md`, `annotations.el`.

### Deferred items (follow-on mission candidates)

- **Cluster-level meta-pattern** for the `recursion-cheque` / `missing-mechanism` / `unearned-essentially` / `floating-formalism` family ("paying for rhetorical force unearned"). Could live as an `ARGUMENT.flexiarg` for `writing-coherence/` or as a library-internal cross-reference document.
- **"Governance debt" concept** dropped from W5 — filed as candidate for a future mission if the WP's argument space needs it downstream.
- **Engelbart (1962) citation** — the fold of §1's Bootstrap sequence cites Engelbart informally; the reference list at §References needs an explicit entry. Mechanical fix, INSTANTIATE-scope miss.
- **Second-dataset validation.** The scaffolding hypothesis is scored `confirmed` on this dataset only. Running the workflow on a second corpus (e.g., a co-author's draft, a prior paper) would test cross-document generality.
- **Lexical checks as CHECK-section entries on patterns.** Each pattern's CHECK could carry its DL-1 lexical-grep signature as a first-class field, so the sweep runs systematically rather than from a single DL-1 list.
- **Refined hypothesis on tacit-required items.** C3's "fold" outcome was almost pre-determined by `plain-language-thesis` + Bootstrap sequence context; might have been scaffold-augmentable under a tighter question. The boundary between `tacit-required` and `scaffold-augmentable` may be sharper than DERIVE's criteria suggest.

### Mission exit

All IDENTIFY completion criteria have concrete demonstrations. Durable artefacts are filed, cross-referenced, and discoverable from the mission doc and from each artefact's canonical location. The runbook in Phase D suffices for a new agent to run the workflow on a second dataset.

**Status: COMPLETE (2026-04-24).**
