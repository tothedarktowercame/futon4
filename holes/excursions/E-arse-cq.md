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

### First read of the Global Commons (claude-5, 2026-11-21, anonymous, read-only)

Found by the node discovery document (`https://cq.exchange/.well-known/cq-node.json` →
`https://api.cq.exchange/api/v1`); `GET /knowledge?domains=D&limit=N` (N ≤ 50) needs no
key. 27 common domains probed; none hit the 50 cap. Raw responses: `/tmp/c5/cq/`
(scratch).
- 78 distinct units. Confirmations: 34 have 1, 24 have 2, the rest 3–8.
- 3 flagged: one `duplicate` (HubSpot OAuth, `ku_196a1c6c…`), two `incorrect` with the
  correction written in the flag: Corepack `corepack install` (`ku_c3e26eb2…`) and a
  live SQLite copy missing WAL rows (`ku_8b4aa929…`; the flag says the summary wrongly
  calls the rows uncommitted). The SQLite one is in our domain: our sidecars are SQLite.
- `lifecycle.kind` is empty on every unit read, and `created_by` is blank on public
  units, so the proposal's "tool gap signal" cannot be computed from public data yet.

So for the Daily Scan, today: units with one confirmation in our domains, and flagged
units, are the available leads; the tool-gap reading waits on cq filling `kind`.

## Extending cq to research questions (Joe, 2026-11-21)

Joe: "cq is apparently an open project so we could see if they are amenable to
*extending* to research questions." cq's CONTRIBUTING asks for an issue before new
features ("Discuss major changes first"); no issue mentions questions or requests
(167 issues read 2026-11-21; the nearest, #289, is about SDK request/response types).

Draft issue (not posted; posting is Joe's, under his own account):

> **Open questions as a knowledge-unit kind?**
> cq records what agents learned. Agents also record what they could not find out:
> we keep these as questions (an internal Q&A store, "ArSE"), and an unanswered one is
> a useful signal on its own, the per-question version of your "tool gap signal".
> Would you consider a `question` kind, or a lifecycle state such as `wanted`, so that
> an agent can propose "X is not known" with its context, others can attach evidence,
> and a unit that answers it can name the question it closes? We would be glad to
> prototype it against a self-hosted server and report back.

- [ ] Joe decides whether to open the issue.

## Open

- KU fields have no author list beyond `created_by`; a pattern with several authors, or
  an answer drawing on a person's record, would need its attribution kept on our side.
- ArSE answers are often about our own stack; few will be useful to others. The
  "release" move should default to not proposing.
