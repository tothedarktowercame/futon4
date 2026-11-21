# E-arse-cq — ArSE questions and Mozilla.ai's cq knowledge units, bridged by CTRL moves

**Status:** IDENTIFY (opened 2026-11-21 by claude-5, at Joe's request).
**Owner:** Joe (operator); claude-5 drafting.
**Home mission:** [M-any-key-ctrl](../missions/M-any-key-ctrl.md) (an extension of its
`※ q ? w` candidate).
**Relates:** futon3c `README-arse.md` (ArSE), github.com/mozilla-ai/cq (cq, Apache-2.0),
the defproject's `sites :cq` and `flows` (`~/notes/claude-5/incubator-defproject.clj`).

## Operator anchor

Joe, 2026-11-21: "With ArSE though I wonder if we should should rework it to include an
API / MCP to Mozilla's cq system? Though cq doesn't have a public installation yet that
I know of." Then: "Let's put it into the M-any-key-ctrl as an E-arse-cq named
extension".

## What each is (read 2026-11-21)

- **ArSE** (futon3c): questions, and answers to them, as evidence. An open question is
  "a legible gap". Ask and answer over HTTP (`/api/alpha/arse/ask`, `/answer`,
  `/unanswered`), IRC (`!ask`), or typed bells (`--type query`, `--type answer --ref`).
  24 questions were unanswered on 2026-11-21.
- **cq** (mozilla-ai/cq): knowledge units (KUs), each an insight with summary, detail and
  action, plus confidence, confirmations, and a tier (local, private, public). It runs
  as a local MCP server over stdio (seven tools: query, propose, confirm, flag, revoke,
  list, status) with a local SQLite store, and installs into Claude Code with
  `cq install --target claude`. A remote is optional: self-hosted
  (`ghcr.io/mozilla-ai/cq/server`), or hosted at cq.exchange, which gives a private
  namespace and read access to a Global Commons "currently seeded by Mozilla.ai".
  Locally proposed KUs are "automatically drained to the remote store" when a remote is
  set, and reach other agents only after human review.

So ArSE holds questions (gaps); cq holds answer-shaped insights. They meet where a
question is answered.

## Proposal: bridge, do not rework ArSE

Two CTRL moves, in M-any-key-ctrl's terms:

1. **Before asking (read).** `※ q ? w` also runs cq `query` on the question. A KU that
   answers it is a witness, and nobody needs to be asked.
2. **After answering (transition, "release").** An ArSE answer that is a reusable
   insight can be proposed as a KU, chosen per item. Before: the answer exists in ArSE
   only. After: a KU exists in the chosen tier, linked to the ArSE thread id. cq's
   human review is the consent step on its side; the choice to propose is the unit's.

## Gates (each Joe's decision)

- [ ] Local install of cq into one Claude seat (store stays on zone; no remote).
- [ ] Any remote: self-hosted on zone (a team store we control) or cq.exchange (proposing
      there publishes).

## First check (nothing leaves zone)

- [ ] With cq installed locally in one seat and read access to the Global Commons only,
      run `query` for each of the open ArSE questions and record which (if any) a KU
      answers, with the KU id. This tells us whether the "before asking" move would
      change anything, before anything is built.

## cq in the Daily Scan (Joe, 2026-11-21)

Joe: "cq could be another interesting resource to look at in the Daily Scan,
particularly if it includes *unanswered* questions that we could research for
kudos/whatever".

cq has no question objects (README and `docs/CQ-Proposal.md`, read 2026-11-21): every
unit is an insight. What comes closest to an open question there:
- KUs with few confirmations (`evidence.confirmations`, `confidence`) in domains we
  know: researching and confirming one, with `confirm`, is credited work;
- KUs flagged `stale` or `incorrect`: a correction is a contribution;
- clusters of `workaround` KUs around one problem, which the proposal calls a "tool gap
  signal" (its Level 4): no tool exists and agents keep hitting it.

- [ ] A Daily Scan section reading the Global Commons (anonymous `query`, which needs no
      API key) for those three, in our domains, listed with KU ids. Read-only, so it can
      run before any install decision; confirming or flagging needs a key and is a
      separate decision. Home for the scan side: futon7 `holes/E-daily-loop-burn-in.md`
      (the Daily Scan mission closed 2026-11-11 and carries there).

## Open

- KU fields have no author list beyond `created_by`; a pattern with several authors, or
  an answer drawing on a person's record, would need its attribution kept on our side.
- ArSE answers are often about our own stack; few will be useful to others. The
  "release" move should default to not proposing.
