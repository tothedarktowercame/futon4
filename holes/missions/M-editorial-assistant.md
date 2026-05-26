# M-editorial-assistant: Reviewer-and-operator audit loop over the essay-revise substrate

**Date:** 2026-05-21 (HEAD authored from operator dialogue).
**Status:** HEAD. The queue substrate exists (`arxana-vsatarcs-essay-revision-queue.el` v0.5.28); the writer-action class exists (`arxana-vsatarcs-essay-revise.el` v0.5.29). The editorial-assistant layer is yet to land — this mission scopes it.
**Owner:** Joe (operator); claude-4 (co-author).
**Blocked by:** None directly. Builds on v0.5.29 essay-revise + v0.5.28 essay-revision-queue.
**Companion missions:** `M-vsatarcs-writer` (action-class family that `:essay-revise` lives in); `M-essays-edit-cycle` (the prose-edit-cycle substrate this overlaps with at the annotation layer); `M-the-futon-stack` Q6 (the consent-gate / autopen apparatus the peripheral plugs into when it lands).

## HEAD — Operator-voice anchor

> "I think what we have here is the making of a new futon4 mission. M-editorial-assistant.md — we can start by build the queue, and I could progress things without any AI assistance, just logging work."
>
> — Joe, 2026-05-21 emacs-repl

### What's already felt to be true

The essay-revision queue (v0.5.28) ranks futon7a essays by an AIF-shaped G-proxy combining staleness × cross-link-density × pattern-hits. The ranking matches operator-intuition (faq > index > history > about per live smoke), which suggests the substrate-driven ranking carries real editorial signal. The writer-action class `:essay-revise` (v0.5.29) closes the predict-effects + admissibility loop. **What's missing is the surface between them where the operator actually does the revision work** — and where each revision is accompanied by a textual rationale that a future reader (or auditing agent) can use to reconstruct WHY the change happened.

### Working example (preserved verbatim from operator dialogue, 2026-05-21)

The Operator's Foreword currently starts:

> "— I call it 'futonic discipline'"

A reviewer's suggested move: **"One-sentence orienting opener stating: (a) what the essay is (operator's foreword to the futon stack), (b) what 'it' is."**

Operator's options:
- **(a)** Do exactly what the reviewer asks (replace cold open with orienting opener).
- **(b)** Keep cold open but flesh it out so the reader establishes meaningful context unaided.

If operator chooses (b), the one-line rationale recorded against the reviewer-comment is:

> "I like the cold open, but I changed it so the reader establishes a meaningful context."

**This is the canonical first row of the editorial-assistant log.** It captures: comment text, proposed move, operator decision, rationale string, resulting edit. The shape generalises — every reviewer-comment carries `(option-a, option-b, rationale-if-not-a)` and every operator resolution carries either "addressed exactly" or "addressed differently + rationale" or "rejected + rationale".

### Anti-glibness discipline

The mission would be superficial if:

- The editorial-assistant agent just rubber-stamps operator rationales without checking substance against the reviewer comment. Discipline: the agent's pushback contract is a binary verdict (`:accepted` | `:pushback-with-diagnostic`); accepting everything is structurally a tell that the prompt isn't doing work.
- The queue's G-proxy diagnostics become reviewer-comment proxies without being made operator-readable. The G-proxy must round-trip to natural-language form ("this essay carries 3 references to companies — Codota, DeepCode, Andela — that are no longer cited as current AI/ML coding-assistants; recommend revising") before it counts as a usable reviewer-comment.
- We ship the peripheral before exercising the manual log. Stage 1 (manual log, no AI) is the input-shape validator for Stage 2 (peripheral). Skipping Stage 1 means freezing the comment + resolution shape against a hypothesised workflow rather than a lived one.

### Working-economy position

This mission underwrites the **niche-creation framing** (Joe 2026-05-20): each essay in `~/code/futon7a/` is an entity in the corpus-niche; cross-essay coherence is the niche's free energy; revision is action. v0.5.28 surfaced revision demand to the operator; v0.5.29 made the same demand visible to the apparatus. **M-editorial-assistant adds the interactive surface where demand becomes revision via a logged process**, so the operator's revision moves accumulate as evidence (rationales) that the peripheral or downstream readers can audit.

Underwriting it: the v0.5.22 safety-property family + `:essay-revise` admissibility (proposed-content ≠ current). The peripheral, when it lands, plugs into the same consent-gate-substitutable shape claude-2 established (`arxana-vsatarcs-consent.el`).

### Clarity-gap / carried-forward tensions

- **Where reviewer-comments come from** is unresolved. Three candidates: (a) auto-generated from the queue's G-proxy diagnostics; (b) operator-authored as inline markup in the essay buffer; (c) both layered (G-proxy seeds, operator extends). Stage 1 must commit to one; (a) is the substrate-aligned choice but loses the operator's own editorial voice. **Joe's 2026-05-21 working example is shape (b)** — the reviewer comment is textual, not an auto-derived G-proxy diagnostic. The mission's first concrete decision is whether to honour that shape or migrate it.
- **Where the log lives** is unresolved. Candidates: per-essay sidecar `.edn` (e.g. `~/code/futon7a/about.editorial-log.edn`); single futon7a-wide log; XTDB hyperedges via futon1a. Sidecar is simplest for Stage 1; XTDB-backed lands naturally when the peripheral needs the log queryable across essays.
- **What "addressed" means for compound comments** is unresolved. A reviewer comment may contain multiple sub-requests; an operator resolution may address some and reject others. Stage 1 can finesse with one-comment-one-resolution; Stage 2's peripheral may need a finer-grained shape.
- **Multi-pass editing** is unresolved. If the operator revises an essay, gets pushback, revises again — does the second resolution shadow the first, or do they stack as a thread? Stack-as-thread is closer to git/PR-review precedent.
- **Authorship vs review separation** is unresolved. The operator is currently both author and audience-of-reviewer-comments; if a comment comes from operator-self-review, the peripheral's pushback ("I don't see how that addresses the substance") becomes operator-against-operator. This is fine as a discipline but worth naming explicitly so it doesn't collapse into self-rubber-stamping.

### Provenance

Authored 2026-05-21 from the emacs-repl dialogue between Joe (operator) and claude-4 (co-author). Sequence: Joe asked how to use `:essay-revise` interactively; claude-4 proposed an open + annotate + propose-with-predict-effects surface; Joe extended the proposal to an editorial-assistant peripheral analogous to `eoi-engine`; Joe authored the working-example that becomes this mission's canonical first log row. No flash, no interview, no separate intake artifact — the operator's voice in this doc is preserved verbatim from that session.

## 1. IDENTIFY

### The gap

Today the apparatus can rank essays by revision demand (read-side) and predict the effect of a proposed revision (write-side), but the operator has no surface to (i) read the per-essay reviewer-comments alongside the essay buffer, (ii) record an operator resolution per comment with a one-line rationale, (iii) accumulate those resolutions into a durable log that future-self or an auditing agent can read back. The propose-revision command stays untriggerable without these surfaces, so the queue surfaces demand and the action class admits proposals but the workflow that ties them stays in the operator's head.

### What this mission is

A two-stage build:

- **Stage 1 (manual log; no AI).** A schema + storage for per-essay reviewer comments and operator resolutions. A reader-mode chrome block that surfaces both alongside the queue. Commands to add a comment, mark a comment addressed (with summary edit), reject a comment with rationale, and view the resolution history of an essay. **The operator can fully exercise the editorial workflow without any agent involvement.** The log accumulates as substrate.
- **Stage 2 (editorial-assistant peripheral).** A new flash profile in the `eoi-engine` family (`editorial-assistant-essay-revision.edn` or similar). The peripheral receives `(original-content, proposed-content, reviewer-comments-with-operator-rationale)` and emits `:accepted` or `:pushback-with-diagnostic`. The peripheral runs only on operator demand (M-x command in essay buffer post-edit); it never auto-runs. Pushback is a *suggestion to the operator*, not a gate.

### What this mission is NOT

- **Not a replacement for the consent-gate**. The consent-gate (claude-2's `arxana-vsatarcs-consent.el`) handles `:essay-revise`-as-action-class admission. The editorial-assistant audits the *substance* of the operator's resolution rationale; it is content-level, not action-level. Separation matters because Stage 2's peripheral could pushback on a perfectly admissible action.
- **Not a writer agent**. The peripheral never proposes prose. It only audits operator-proposed prose against operator-recorded rationale.
- **Not Q6 (kill-switch / debug surface / reversal)**. The editorial-assistant is *a* peripheral, not the full Q6 apparatus. The handoff at `~/code/futon0/holes/handoffs/r12-to-stack-Q6-2026-05-21.md` covers Q6's narrow R12-unblocking subset; this mission stays scoped to the editorial loop only.
- **Not an autopen at any stage**. The peripheral's verdict is advisory; the operator decides. Autopen migration belongs to a downstream mission once Stage 2 has been used long enough to know what verdicts deserve autopen-promotion.

### Scope in / scope out

**In scope (Stage 1):**

- Schema for reviewer-comments + operator-resolutions (likely EDN sidecar per essay).
- Storage layer: read + write per-essay; round-trip across Emacs sessions.
- Reader-mode chrome block extending `arxana-browser-vsatarcs.el` with a 12th block listing top-N essays with comment-counts + unresolved-count.
- Commands: `M-x arxana-vsatarcs-editorial-add-comment`, `-resolve-comment`, `-reject-comment`, `-show-essay-with-comments`.
- A worked example committed: the Operator's Foreword's first reviewer-comment + operator-resolution per Joe's 2026-05-21 dialogue is the canonical first log entry.

**In scope (Stage 2):**

- Flash profile for the editorial-assistant peripheral.
- CLI wrapper invoking a fresh agent against the flash profile per `eoi-engine` pattern.
- `M-x arxana-vsatarcs-editorial-audit-current-revision` calling out to the wrapper; result rendered as a posframe or side-window with the verdict + diagnostic.
- One worked audit: operator revises an essay; calls the audit; result is logged alongside the resolution.

**Out of scope:**

- Auto-running the peripheral on every save.
- Generating reviewer-comments via an agent. Reviewer-comments are operator-authored (or, in a sub-question, possibly seeded from the queue's G-proxy diagnostics — see open questions).
- Multi-operator workflows. This mission is single-operator (Joe); collaborative editing is a downstream mission.
- The propose-revision command that constructs `:essay-revise` actions from buffer state. That command is needed and will land as part of Stage 1's command set, but its predict-effects-rendering UX is a sibling concern that may grow its own mission if it spawns enough open questions.

### Open questions (each is a research surface)

#### Q1 — Reviewer-comments source

(a) operator-authored only; (b) G-proxy-derived only; (c) hybrid. Joe's worked example is shape (a). The G-proxy diagnostics are already substrate-aligned but operator-textual comments carry the operator's own editorial voice. **Recommended first move:** ship (a) in Stage 1; add G-proxy auto-seeded comments as a Stage 1.5 extension if the manual workflow surfaces demand for them.

#### Q2 — Log storage shape

(a) per-essay sidecar `.edn` (e.g. `~/code/futon7a/about.editorial-log.edn`); (b) single corpus-wide log; (c) XTDB-backed via futon1a. Stage 1 wants the simplest substrate that survives Emacs restarts — sidecar. Stage 2 wants the log queryable across essays — futon1a-backed. **Recommended:** sidecar in Stage 1; add a one-direction sync to futon1a once the peripheral lands.

#### Q3 — Resolution states

Minimum set: `:pending`, `:addressed`, `:rejected-with-rationale`. Worth considering: `:partially-addressed`, `:superseded-by-later-comment`, `:deferred`. **Recommended:** start with the minimum three; promote states only when a manual-log workflow produces real instances of the others.

#### Q4 — Peripheral verdict shape

(a) binary `:accepted` | `:pushback`; (b) tertiary `:accepted` | `:pushback` | `:abstain` (the peripheral declines to judge); (c) graded confidence. Joe's example phrasing — "I don't see how that addresses the substance" — is binary in shape (pushback with explanation). **Recommended:** binary in Stage 2; `:abstain` as Stage 2.5 if the peripheral surfaces cases where it shouldn't judge.

#### Q5 — Audit threading

If the operator revises in response to pushback, does the second resolution shadow the first or stack as a thread? Git/PR-review precedent stacks. **Recommended:** stack from day one — append-only log is simpler than mutation and gives a real history.

#### Q6 — Authorship-vs-review separation in single-operator mode

When operator is both reviewer-comment author and resolver, the loop risks rubber-stamping. **Recommended:** name the discipline (operator commits to authoring comments at-comment-time without considering whether they'll be addressable; resolver is a separate cognitive stance), but don't enforce technically. Trust-based; revisit if rubber-stamping is observed.

### Owner and dependencies

- **Owner:** Joe (operator); claude-4 (co-author for Stage 1; Stage 2 may bring in claude-2 or a fresh agent depending on whether the peripheral wants the writer-action-class context).
- **Dependencies:**
  - `arxana-vsatarcs-essay-revision-queue.el` v0.5.28 — substrate for ranking essays by revision demand.
  - `arxana-vsatarcs-essay-revise.el` v0.5.29 — action class that the propose-revision command produces.
  - `arxana-browser-vsatarcs.el` — chrome host for the new block.
  - `arxana-vsatarcs-consent.el` (claude-2) — referenced for separation-of-concerns (this mission does NOT replace the consent-gate).
  - `~/code/algorithms/eoi-engine.md` — pattern reference for Stage 2's peripheral shape.
  - `~/code/futon3/library/peripherals/head-flash/` — directory where the Stage 2 flash profile would live.

### Completion criteria

Stage 1 completes when:

1. The reviewer-comment + operator-resolution schema is committed and stable.
2. Per-essay log files round-trip across Emacs sessions.
3. The chrome block surfaces unresolved-comment counts alongside the queue.
4. The commands work end-to-end for at least three real essays (Operator's Foreword + ≥2 futon7a essays).
5. The Operator's Foreword worked example from the HEAD section is the literal first log entry.

Stage 2 completes when:

1. The flash profile is committed.
2. The CLI wrapper invokes a fresh agent and returns structured output.
3. At least one operator-revision-cycle is audited by the peripheral; the verdict is logged.
4. The peripheral's pushback discipline is exercised at least once (operator submits an under-substantive rationale; peripheral pushes back; operator records the disagreement or revises).

### Risk register

- **Reviewer-comments become operator-self-affirmation.** Mitigation: Q6's named discipline; periodic operator-review of own resolutions.
- **The peripheral always accepts.** Mitigation: the anti-glibness check above — accepting-everything is a tell. Calibrate the flash profile against deliberately-weak rationales in initial tests.
- **The peripheral always pushes back.** Mitigation: same calibration, opposite direction; the prompt must make `:accepted` a real verdict.
- **Stage 1's workflow doesn't survive operator use.** Mitigation: Stage 1's commands ship to operator hands before Stage 2 is designed; if the workflow doesn't hold, Stage 2 redesigns rather than overlaying on broken substrate.
- **The log accumulates faster than it's read.** Future-operator-or-agent never queries it; the apparatus becomes write-only substrate. Mitigation: chrome block surfaces unresolved counts so the queue's read-side already pulls log state into operator view; the log is read by side-effect of using the apparatus daily.

## 2. MAP / DERIVE / ARGUE / VERIFY / INSTANTIATE / DOCUMENT

Empty at HEAD. Phase work starts when Stage 1 enters DERIVE.

## Checkpoints

### HEAD — 2026-05-21

- Operator-voice anchor preserved verbatim from emacs-repl dialogue.
- Working example identified (Operator's Foreword opening; reviewer-suggested orienting-opener vs. operator-preferred cold-open-fleshed-out).
- Two-stage build named (manual log → editorial-assistant peripheral).
- Six open questions surfaced with recommended Stage 1 answers.
- No code shipped this checkpoint; substrate (queue v0.5.28, essay-revise v0.5.29) is the inheritance from prior missions.

### Next-move

Joe's call. Two natural starting points:

- **DERIVE the Stage 1 schema** — author the EDN sidecar shape; commit the Operator's Foreword first log entry; THEN write code that reads it.
- **INSTANTIATE the Stage 1 commands directly** — write `M-x arxana-vsatarcs-editorial-add-comment` etc. against an inline-defined schema; let the schema crystallise from working code.

The first is more disciplined; the second is faster. Recommended: schema-first, since the worked example from HEAD gives a concrete first row to validate the schema against before code freezes shape.
