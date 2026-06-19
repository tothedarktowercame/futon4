# Futonic EOI Lifecycle

*The Expression-of-Interest is the atomic unit of the Hyperreal outreach engine — and it is **not** "a
letter we wing out." An EOI is a **witnessed claim about a real interest**: drafting it builds, and
sending it tests, a model of what Joe is actually interested in (and who is interested back). It
parallels the Mission lifecycle (`mission-lifecycle.md`) and the Campaign lifecycle
(`campaign-lifecycle.md`) — a built-to-end, phased object whose transitions are **witnessed, not
asserted.***

**Date:** 2026-06-04 (Joe + claude-3, plumbing before porcelain — pre-`arxana-vsatarcs-sales` build).
**Status:** SPEC / HEAD. The model; the Sales-channel implementation is downstream of ratifying this.

## Why a first-class EOI lifecycle (the grounding)

Two failures this exists to prevent:
1. **Claimed-not-witnessed status drift.** A flat `.edn` let `anthropic-fellows-2026` read both
   `:drafted-not-sent` *and* "Anthropic already sent" — a *claimed* send with no witness. The lifecycle
   makes **SEND a witnessed transition** (a logged send event), so "sent" cannot be asserted into being.
2. **Fantasy-fixation interests.** An EOI expresses *interests*. An interest with no backing activity
   ("I'm interested in computer mathematics" but never a real proof) is a **fantasy-fixation** — and an
   EOI built on one is itself laundering. The lifecycle gates drafting on the **interest's grounding**.

The EOI is therefore the concrete face of `M-expressions-of-interest` (the interest-model corpus) and
emits typed events into `M-interest-network-coupling`'s **interest-event-vocabulary** — each sent EOI's
reply/silence is real evidence updating the interest model (the only non-fantasy signal).

## The theoretical core (Joe, 2026-06-04) — map-reduce to one EOI; the fabric; the scattering experiment

**1. Each EOI map-reduces into ONE overall EOI.** A single EOI (Anthropic, Aleks, Glasgow…) is a *local
map* — one target's slice of Joe's interests, themed. The **REDUCE** folds every such map into a single
**overall expression of interest**. The individual EOIs are not independent letters; they are
projections of one coherent interest-structure, and the engine's job is to keep that structure coherent
as new EOIs land.

**2. The reduce-target is a *fabric*, not a shopping-basket.** Today's interest representation is a
**bipartite hub-and-spoke** (mission↔file co-occurrence; the stand-alone PDF) — a shopping-basket. It is
*not wrong, but it is not itself a coherent expression of interest.* A coherent EOI is a **semantic
fabric** that **covers the actual FUTON work** — "the stars we steer by," not a backwater. Coherence =
**connectedness = coverage**.

**3. Coverage IS grounding (the unification) — but uncovered ≠ automatically fantasy.** The fabric and
Phase-0 grounding are the same mechanism: **witnessed** iff a connected thread *covering real work*;
**latent** iff covers work but under-surfaced (→ `M-stack-stereolithography`). An interest with **no
substrate-2 analogue** is **legitimately-new XOR legitimately-bogus** — discriminated by the
`E-half-mil-audit` *grows-at-edges* finding: a **frontier-edge** interest (adjacent to real work, the
adjacent-possible) is **legitimately new → keep, it's the growing edge**; a **disconnected island** (no
trajectory toward the work) is the **fantasy-fixation → spike**. The new-vs-island audit happens in
`eoi-next` (editorial). See [[E-interest-mining]].

**4. The campaign is a scattering experiment surveying substrate-4.** With Customer-1 (Joe) as the
known **calibration** reference (the n=1 anchor, [[M-buyer-discovery]]), each *sent* EOI is a **probe
fired into substrate-4** (the non-Joe real world / the xeno; `C-pudding-prover` §5b). The **scattering
cross-section** — who replies, who is silent — reconstructs the structure of substrate-4: *which
interests resonate with the market.* The cold-conversion frontier (`C-pudding-prover`) is the scattering
actually striking genuine xeno targets; the fabric is what the scattering *refines* (replies update the
interest-network posterior — the "only real signal"). So the EOI engine is the **survey instrument**,
the fabric is the **map it produces**, and Customer-1 is the **reference standard** it is calibrated
against.

## Phases

| # | Phase | What happens | Artifact / engine | Witnessed by |
|---|---|---|---|---|
| **0** | **INTEREST-GROUND** *(≈ HEAD/ARGUE)* | name the interest(s) the EOI expresses; **classify grounding** (below). Fantasy-fixation ⇒ **do not draft — spike it.** | the interest-network; C-pudding-prover lemmas | an activity-witness for the interest (a proof, a commit, a shipped thing) |
| **1** | **DRAFT-NEW** *(≈ IDENTIFY)* | `eoi-new` engine flash → first expression. **Upgrade (Joe 2026-06-04): it has gotten boring — it surfaces only a *subset* of the eightfold-path arrows and leaves low confidence the interests were captured.** Fix: cover (or explicitly account for) **all 8 path items**, and end with a confidence/coverage check. | `eoi-engine` `eoi-new` flash (versioned here) | the raw draft exists + a coverage report |
| **2** | **MAP-REDUCE** *(≈ MAP)* | **MAP:** turn this EOI into a per-EOI **themed semantic network** — *Essay-Ethics-demo-style* (`/wa#/pins/…`: clearly-identified themes, clear arrangement), **not** the hard-to-follow `/wa#/interest-network` hub-and-spoke. **REDUCE:** fold its themes into the **one overall interest-fabric** (and draw this EOI's draft *from* the fabric — bidirectional). | **`eoi-next`** (editorial) → the Essays/network surface; the fabric | the per-EOI themed network renders + the fabric updates |
| **3** | **PROFORMA** *(≈ INSTANTIATE)* | 2nd draft written into the target's actual proforma **where available** (e.g. the Fellows form-fill); else the free-form letter (e.g. the Aleks email) | **`eoi-next`** editorial move → `futon5a/essays/<target>/…` | the proforma-instantiated draft exists |
| **4** | **SEND** *(≈ VERIFY — the load-bearing transition)* | the EOI is **actually sent** | a **typed `cold-outreach`/send evidence event** (date, target, channel) | **a logged send event — NEVER an `.edn` assertion** |
| **5** | **OUTCOME** *(≈ DELIVER)* | reply / silence-after-window logged | the reply, or the window expiring | a logged reply, or a witnessed silence-timeout |

**Phase 4 is the one the Anthropic bug violated** — and is the only phase that moves C-pudding-prover's
cold-conversion frontier. Phases 0–3 are foraging/authoring (no conversion sampled); 5 begins sampling
the response rate.

## The SEND-witness event (shared object — schema closed with claude-1, 2026-06-04)

The Phase-4 SEND emits **one typed evidence event** that is simultaneously the **lifecycle SEND-witness**, the
**C-pudding-prover §3 evidence-store intake**, and a **"pudding" for the §9 pudding-prover machine**. One object,
three roles. Schema:

```clojure
{:event          :outreach-sent
 :plants-thesis  <:T2.3-cold | :T1.5-warm | :hedge-anthropic | ...>   ; routing key → §8 registry entry
 :lead-class     <:cold-scan-lead | :warm | :known>                  ; makes anti-laundering checkable at intake
 :lead           <target>            :sent-at <ts>
 :send-witness   <verifiable send artifact: email message-id | form-submission-confirmation | receipt>
                                     ; ← the SEND's OWN proof, distinct from :witness-sources
 :witness-sources [...]              ; content-grounding triangulation (cross-sectional; §9.2)
 :outcome        <:reply | :silence-after-window | :bounce>
 :outcome-window <duration>          :outcome-logged-at <ts>          ; silence = WITNESSED timeout, not absence
 :projection     {:from <draft-essay-id> :to <final-essay-id>
                  :redact [..] :flatten [..] :substitute [..] :add [..]}}  ; = the draft→final provenance edge
```

**Three intake machine-checks (enforce §2 anti-laundering at intake, not by promise):**
1. **ROUTING** — `:plants-thesis` routes the event to its §8 registry entry. *First* such ⇒ `:satisfied-base-case`
   (n:0→1); the recurring **stream** (a query over many, not a field) ⇒ **cadence** ⇒ `:capability` (§9.2).
   *Cadence is longitudinal; triangulation is per-event — kept separate.*
2. **LEAD-CLASS GUARD** — `:plants-thesis :T2.3-cold` **requires** `:lead-class :cold-scan-lead`. A warm/known lead
   (Aleks, Anthropic) **physically cannot camp the cold thesis** — the mismatch is rejected.
3. **SEND-WITNESS GUARD** — `:event :outreach-sent` **requires** non-nil `:send-witness`. A send without its proof is
   rejected as claimed-not-witnessed. *(Guards 2+3 together make both laundering paths — wrong-lead-class AND
   no-real-send — unrepresentable. This is the schema-level kill of the "Anthropic already sent" bug.)*

**Scope of the n's:** this event moves **n_outreach** (the response rate = C-pudding-prover Milestone A).
**n_conversion** (Milestone B — a reply that becomes an engagement) is a *separate, downstream* follow-on event
**`:event :engagement-converted`** linking back to this one — *not* overloaded here.

**Fabric-feed is orthogonal (not routed):** *every* EOI feeds the Track-B interest-fabric (coverage=grounding)
as a side-effect of being sent, regardless of `:plants-thesis`. The fabric-grounding gates which propositions are
real enough to enter the §8 registry at all (witnessed-by-coverage vs fantasy-fixation → spike).

**Division of labour (closed):** claude-3 owns this event spec (lifecycle side); claude-1 wires the §8 intake
(routing + the two-part guard + the cadence query + the n_outreach/`:engagement-converted` split) and the §9
"propositions are fabric-grounded, not just ledger-asserted" note.

**THE GATE (joint consensus, claude-1 + claude-3, 2026-06-04):** the machinery is specced enough; **nothing moves
the crux until Joe runs ONE cold cycle** — a scan lead (NOT Eric/Rob/Aleks/Anthropic) **sent + logged with its
`:send-witness`**. (i) land this shared event; (ii) Joe plants the first cold base-camp into it (n:0→1). Everything
else is enrichment on top of that base case.

## The interest-grounding cross-cut (the deep point)

Every EOI carries interests; each interest gets a **grounding status** (the claimed-vs-witnessed
discipline, applied to interests). **This status *is* the interest's connectedness in the fabric** (core
#3) — the two are one mechanism:

- **witnessed** — backed by real activity (proofs, commits, shipped work). Legitimately expressible.
- **latent** — real but mostly-dormant (subtle, complex, longstanding, under-surfaced). **Do not spike —
  surface its detail via `M-stack-stereolithography`** (reveal the layered depth so the EOI can speak it).
- **fantasy-fixation** — claimed but no backing activity. **Spike it.** An EOI built on a fantasy
  interest is anti-laundering's interest-level case: it markets an interest that isn't real.

So C-pudding-prover's **pudding-proofs *are* the grounding of the interests EOIs express** — "futon6 could
transform mathematics" is the interest *computer-mathematics*, witnessed only by actually proving things.
INTEREST-GROUND (phase 0) is where that check happens, before a fantasy gets drafted.

## States (the status enum the Sales channel renders)

`:interest-grounded?` ∈ `{:witnessed :latent :fantasy-spiked}`
`:eoi/phase` ∈ `{:draft-new :aggregated :proforma :sent :replied :silence :spiked}`
`:eoi/claimed-status` vs **`:eoi/witnessed-status`** — surfaced separately; **drift is flagged red**
(the Anthropic case: claimed `:sent`, witnessed `:proforma` → red).

## Witnessed-status discipline

`:sent` (and `:replied`) are reachable **only via a logged witness event**. The Sales channel shows
claimed vs witnessed side-by-side and refuses to silently promote claimed→witnessed. This is VSATARCS
principle #8 (explicit, not fuzzy) carried into the outreach pipeline — it is exactly what would have
caught "Anthropic already sent."

## Couplings

- `M-expressions-of-interest` (futon5a) — the interest-model corpus; EOIs are its members.
- `M-interest-network-coupling` (CLOSED) — the **interest-event-vocabulary** EOI transitions emit into;
  the posterior-update ("each sent EOI's reply updates the Joe-model").
- `M-stack-stereolithography` — surfaces latent-real interests so the EOI can express their depth.
- `C-pudding-prover` — SEND/OUTCOME are its cold-conversion frontier; pudding-proofs ground the interests.
- `arxana-vsatarcs-sales` (the Sales channel) — the porcelain that renders this lifecycle (built *after*
  this spec is ratified).
- `eoi-engine` (`futon3/library/eoi-engine`) — owns DRAFT-NEW (the `eoi-new` flash; may need upgrades).

## Worked example — `anthropic-fellows-2026`

INTEREST-GROUND: interest = *work-on-AI-safety/agentic-systems*; grounding = witnessed (the stack itself).
DRAFT-NEW ✓ (`eoi-new`, the Q1–Q9 run). AGGREGATE ✓. PROFORMA ✓ (`anthropic-fellows-2026-form-fill-v1.md`).
**SEND ✗** — actually `:drafted-not-sent`. The `.edn`'s "Anthropic already sent" was a **claimed status with
no send-witness** → the exact drift this lifecycle flags red. OUTCOME: n/a (not sent).

## Conventions

- Phase-mapping to the sibling lifecycles is given in the Phases table (≈ column). The EOI lifecycle is
  *shorter and more concrete* than Mission/Campaign, but shares the spine: **a witnessed VERIFY-analogue
  (SEND) is the gate, and the object is built to end** (it resolves at OUTCOME, or is SPIKED at phase 0).
- `eoi-new` upgrades land here (DRAFT-NEW) and are versioned with the lifecycle, since the theoretical
  model leans on the EOI being a faithful interest-expression, not a form-letter.
