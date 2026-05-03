# Peeragogy mailing list — handoff message draft (2026-04-29)

**Status:** Draft for Joe's review. Not yet sent.
**Audience:** Peeragogy mailing list (current + historical contributors).
**Goal:** Signal that the handbook is moving again, attach the reviewer
report, and make a small concrete ask that produces forward motion
without gating on infrastructure work.

---

## Subject line options

- "Peeragogy Handbook — quietly moving again, and a small ask"
- "Reviewer report on the Wikibooks Peeragogy Handbook (and: it's moving)"
- "Peeragogy Handbook v1.0 — first edits in years just landed; what's next"

Recommended: the third — concrete, time-anchored, low-key.

---

## Body draft

Hi all,

Some of you know I've been circling back to the Peeragogy Handbook on
Wikibooks — the v1.0 mirror at
https://en.wikibooks.org/wiki/Peeragogy_Handbook_V1.0 — for a while now.
This is a short note to say two things: the handbook is moving again,
and there's a reviewer report you might find useful.

**What just happened.** Six small edits landed on Wikibooks today, the
first activity on those pages in some cases since 2013. They're cleanups,
not rewrites — visible vandalism removed from the Use Case and Style
Guide chapters, a leading "W" restored in the Peeragogy in Action
chapter, a typo fixed in Meet the Authors, and a † added to mark Jay
Cross (1944–2015). Edit log:
https://en.wikibooks.org/wiki/Special:Contributions/Arided
(the recent ones are from a bot account, Arided@futon-peeragogy, but the
edits are mine.)

**Why this is small and not a rewrite.** I went through every section of
the V1.0 handbook over the last few weeks, reading it as a 2026 reader
would. The result is a 450-line reviewer report covering 31 sections, 88
findings, 32 clusters, and an ordered revision plan. Two themes
dominate: source-fidelity (the Wikibooks pages have collected vandalism
and import artefacts the original editors never saw) and
platform-currency (Google+ Hangouts, Posterous, Diigo, Edmodo and a
dozen other tooling references that any 2026 reader notices first).

The report is here: [link to be filled in — see "Where to host" below].

**What I'm asking for.** Three things, in order of cost:

1. **Eyes on the report.** If you contributed to the v1.0 handbook and
   recognise a chapter or a passage, you're the right reader for this.
   Disagreements with the report are useful; the report is a draft
   reading, not a conclusion.

2. **Affiliations and quotes for chapter 30 (Meet the Authors).** Every
   row in that chapter currently has the placeholder Affiliation
   "Former" and the placeholder Quote "Welcomes any criticism or
   praise." If you'd like an actual affiliation and quote — or want your
   name removed, or want a posthumous acknowledgement noted — send it
   in this thread. This is the cheapest single improvement we can make
   to the handbook and it requires people, not infrastructure.

3. **Co-conspirators for the rewrite.** The report's recommendation is a
   near-complete rewrite, sequenced in three tiers. Tier 1 is "editorial
   sweeps" (mostly mechanical once policy is set); Tier 2 is the
   chapter rewrites that deliver visible improvement. If any of that
   appeals — picking one chapter is a self-contained contribution — say
   so and I'll sketch the per-chapter scope.

**What's not in scope right now.** The platform-currency rewrites
(Google+ Hangouts in chapter 26, Posterous/Tumblr in chapter 11, etc.)
need editorial decisions before the writing happens — feature
primitives only? feature primitives plus dated 2026 exemplars?
living-stack annual snapshots? — and I'd rather settle that policy with
the group than commit unilaterally. Section 2.3 of the report lays out
the options; reply on this thread if you have a strong view.

The goal is to get the handbook back to a state where a 2026 reader
recognises the structural argument (a pattern language for peer
learning) without being thrown by the surface — vandalism, dead links,
defunct platforms, placeholder author rows. The structural argument is
still alive in the text. The surface needs work.

— Joe

P.S. The reviewer report names seven passages that exhibit the pattern
language at its best and are flagged as preserve-do-not-edit. Chapter
20 (Patterns and Heuristics), chapter 26's role taxonomy, chapter 28's
Wrapper observation. If you wrote those, you wrote the most
load-bearing pages in the book.

---

## Where to host the report for the link

The report file is `futon4/holes/labs/M-peeragogy-rewrite/peeragogy-handbook-reviewer-report-2026-04-29.md`,
which is local. To share it on the mailing list it needs a public URL.
Options:

- **Wikibooks talk page** (`Talk:Peeragogy Handbook V1.0`) — most
  contextually correct, gives every Wikibooks reader access too. Could
  paste the report there as a single talk-page section, or as a
  subpage. The bot can do this — it's the same edit path.
- **Personal site / arxana.io** — under your control, easy to update,
  but moves the conversation off Wikibooks.
- **GitHub gist** — quickest, but lowest discoverability and zero
  Wikibooks integration.

Recommendation: post to `Talk:Peeragogy Handbook V1.0` as the canonical
location, link to that from the email. The talk-page format also gives
contributors a place to reply per-section without needing email or
github accounts.

---

## Pre-send checklist

- [ ] Joe reviews and edits the body
- [ ] Decide where to host the report (recommend Wikibooks talk page)
- [ ] Push report to chosen host; capture URL
- [ ] Insert URL into the message body where bracketed
- [ ] Identify the exact mailing list address(es) — Peeragogy still uses
      the Google Group `peeragogy@googlegroups.com`? Or has it moved?
- [ ] Send

---

## Notes on what we deliberately do NOT promise in this message

- "We'll have a v2.0 by [date]" — no calendar commitments. The rewrite
  is community-paced.
- "The bot will do all future edits" — the bot is for surgical fixes
  the operator approves; substantive edits are human work.
- "Round-tripping through our local editor works" — it doesn't, fully.
  Today's testing exposed lossy Pandoc round-trips for File embeds,
  templates, and interwiki links. Section 0.1 of the report
  acknowledges this; the message above does not promise capabilities
  we haven't proven.
- "The original Peeragogy Handbook material is being replaced" — it
  isn't. Wikibooks v1.0 is the canonical instance the report addresses.
  No fork-and-rebrand is on the table.
