# Coauthor Invitation Handoff Packages (DRAFT)

**Mission:** M-peeragogy-rewrite
**Status:** DRAFT — nothing leaves the machine. These are tiered invitation
packages matched to tie strength per `collaboration-coherence/weak-tie-conflation`.
The operator sends (or doesn't); these are drafts only.
**Date:** 2026-07-11

## The Tiering Discipline

The `weak-tie-conflation` pattern says: match the ask to the tie. A mailing-list
blast asks too much of weak ties and too little of strong ones. The fix is
per-person, per-tier invitation:

| Tier | Tie type | Coordination cost | Ask size | Time commitment |
|------|----------|-------------------|----------|-----------------|
| Weak | Acquaintance, former collaborator | Near-zero | 5-min affiliation fill-in | One email reply |
| Medium | Active contact, shared interest | Light coordination | Review + comment on one artifact | 30-60 min |
| Strong | Close collaborator, co-author history | Real coordination | Co-author a section/artifact | Multi-session |

---

## Tier 1: WEAK (5-min affiliation fill-in)

**For:** people who have been peripherally connected to peeragogy — former
Handbook contributors who haven't been active, Peeragogy Podcast listeners
who've engaged, Cool Laboratory participants.

**Collaborator type example:** a former Handbook chapter contributor who
moved to other projects (the doc names "Distributed contributors" in the
INSTANTIATE section).

**Package draft:**

> Subject: Peeragogy in 2026 — quick check-in
>
> Hi [name],
>
> We're revisiting the Peeragogy Handbook for the first time since the 2016
> edition. The project has accumulated a lot of new material — a podcast
> series, pattern library, annotation system — and we're putting together
> a short essay on "What is peeragogy in 2026?"
>
> You contributed to [chapter/section] in the Handbook. We'd love to know:
> are you still working in this space? Is there one thing about peeragogy
> that you've learned since then that we should include?
>
> A one-line reply is plenty. No obligation.
>
> — Joe

**Why weak tier:** the ask is 5 minutes, no coordination needed, the reply
is optional. This respects the tie strength — asking for a chapter rewrite
would be the weak-tie-conflation failure mode.

---

## Tier 2: MEDIUM (review + comment on one artifact)

**For:** active contacts with shared interests who could contribute expertise
but aren't co-authors — CLA practitioners, OGM participants, UKRN colleagues.

**Collaborator type example:** a CLA/futures-studies researcher (the doc
references "Sohail" as a podcast guest with CLA expertise — a medium tie:
active contact, shared theoretical ground, not a Handbook co-author).

**Package draft:**

> Subject: Peeragogy 2026 — would you review our CLA framing?
>
> Hi [name],
>
> We're writing a short essay reframing peeragogy for 2026, and the
> worldview/metaphor layer (CLA levels 3-4) is central to our argument.
> Since you spoke with us about CLA on the podcast, would you be willing
> to read a 500-word section and tell us if we've represented it fairly?
>
> No writing needed — just your honest reaction. 30-60 minutes.
>
> The draft, the posterior synthesis, and the annotation system are all
> available for reference if you want context.
>
> — Joe

**Why medium tier:** the ask is a focused review (not co-authorship), with
light coordination (send draft, get feedback). The tie is strong enough to
support a substantive ask but not a co-author commitment.

---

## Tier 3: STRONG (co-author a section/artifact)

**For:** close collaborators with co-author history — people who have
already built things with Joe and could carry a section.

**Collaborator type example:** a long-term co-author (the doc names
"Charlotte" and "Charlie" as strong ties in the INSTANTIATE section —
both have deep peeragogy history).

**Package draft:**

> Subject: Co-authoring the 2026 Peeragogy essay
>
> Hi [name],
>
> I've been working on a peeragogy rewrite for the past few months —
> 88 annotations on the Handbook, a podcast-pattern analysis, a revision
> queue. The carrier shape is settled: a short essay, a next-wave of
> pattern flexiargs, a method note, and eventually a mini-handbook.
>
> The essay is the one that needs you. "What is peeragogy in 2026?" —
> 3,000 words, naming the worldview shift, the timeless-practice family,
> the minor-literature situation honestly. I have the material but the
> essay needs a co-author voice, not just mine.
>
> You know this material as well as I do. Would you co-author the essay
> with me? I'm thinking: I draft, you redraft, we iterate. Two to three
> sessions over a month.
>
> Everything is on disk and inspectable: the queue, the annotations,
> the posterior synthesis. You can see exactly where the essay sits.
>
> — Joe

**Why strong tier:** the ask is co-authorship — real work, real
coordination, real commitment. This is only appropriate for a strong tie
who has the standing and history to carry the work. Asking this of a
weak tie would be the failure mode.

---

## Verification: Inspectable Intermediates (ob-6)

The `ai4ci/collective-intelligence-framing` pattern says inspectable
cross-referenced artifacts prevent slide into folklore. These are the
real files:

| Artifact | Path | Condition |
|----------|------|-----------|
| Revision queue v5 | `futon4/data/essays/wikibooks/peeragogy-handbook/peeragogy-handbook-revision-queue.edn` | EXISTS: semilattice queue, 32 clusters, 11 concerns, 3-tier priority |
| Four-axis annotations (Handbook) | `futon4/data/essays/wikibooks/peeragogy-handbook/peeragogy-handbook-book-annotations.el` | EXISTS: 88 annotations across 4 axes, 31/31 sections |
| Four-axis annotations (Podcasts) | `futon4/data/essays/podcasts/peeragogy-podcasts/peeragogy-podcasts-book-annotations.el` | EXISTS: podcast annotations, same 4-axis system |
| Posterior synthesis | `futon4/holes/labs/M-peeragogy-rewrite/podcasts/posterior-notes/00-synthesis-2016-prior-vs-2026-posterior.md` | EXISTS: 13 cross-corpus promotions |
| Reviewer report | `futon4/holes/labs/M-peeragogy-rewrite/peeragogy-handbook-reviewer-report-2026-04-29.md` | EXISTS: 452 lines, narrative rendering of queue |
| 2016-prior rubric | `futon4/holes/labs/M-peeragogy-rewrite/peeragogy-2016-prior.md` | EXISTS: handbook self-claims extracted |
| Retrospective-as-MAP method | `futon4/holes/labs/M-peeragogy-rewrite/retrospective-as-map-method.md` | EXISTS: general-purpose MAP method note |
| Mailing-list handoff | `futon4/holes/labs/M-peeragogy-rewrite/peeragogy-mailing-list-handoff-2026-04-29.md` | EXISTS: weak-tier handoff (predecessor to this doc) |
| .mw canonical source | `futon4/data/essays/wikibooks/peeragogy-handbook/wikitext/*.mw` | EXISTS: 31 chapters, Pandoc lossiness eliminated |
| Peeragogy pattern library | `futon3/library/peeragogy/*.flexiarg` | EXISTS: 16 flexiargs (the library the next-wave candidates extend) |
| Podcast transcripts | `futon4/holes/labs/M-peeragogy-rewrite/podcasts/transcripts/*/*_full.txt` | EXISTS: 13 transcripts (~75,500 words) |

All paths verified as real files on disk (2026-07-11).

---

## Outward Boundary

**No invitations are sent by this flight.** These are DRAFTS. The operator
sends or doesn't. Nothing leaves the machine. The tiering discipline
(weak-tie-conflation) is applied in the draft structure; the actual
matching of real names to tiers is the operator's call.
