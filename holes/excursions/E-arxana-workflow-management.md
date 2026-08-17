# E-arxana-workflow-management

**Owner (implementer):** unassigned · **Opened:** 2026-08-17 by claude-2 (Analyst,
M-apm-demonstration) at Joe's direction · **Surface:** Arxana ledger subsystems
(futon4 `dev/arxana-vsatarcs-*.el`) + the flexiarg library (futon3 `library/`).

## Why

The flexiarg directive census (2026-08-17, `futon3/flexiarg-directives.edn`)
found **workflow state living inside pattern files** — mutable status in a
versioned, reviewed artifact:

| directive | uses | families | value |
|---|---|---|---|
| `@status` | 65 | 10 | `[status[blocked] blocked-by[vsatlas/funder-brief]]` |
| `@verdict` | 26 | ukrns | `stale` / `early` |
| `@confidence` | 6 | t3 | `high` |
| `@review` | 5 | math-formalization, math-strategy, pattern-mining | `claude-2 2026-08-12 APPROVE, on the slice-3 corroboration…` |

**Joe's ruling, 2026-08-17:** *"I don't see this as worth including in the
pattern text at all, even as metadata. If we need to handle workflow, we should
find some other way to do that… and broadly, we do have a way with things like
the Arxana Ledger and similar subsystems of Arxana."*

So these 102 uses are **queued here for cleanup**, not standardised in the
flexiarg format. `flexiarg-directives.edn` marks them `:excursion-queued` with a
pointer to this file; the ingest whitelist excludes them.

## What is actually being asked

1. **A workflow surface that is not the pattern file.** The Arxana ledger
   subsystems are the named candidate. They need work — that work is this
   excursion's substance, not a precondition for it.
2. **A migration for the 102 existing uses**, per directive:
   - `@status` — carries a real relation inside a state field:
     `blocked-by[vsatlas/funder-brief]` is a genuine pattern→pattern edge. The
     *state* goes to the ledger; the *edge* is a separate question for the
     flexiarg semantic axis (`@why`/`@how`/`@see-also`), and should not be lost
     in the move.
   - `@verdict`, `@confidence` — single-family, plain scalars; ledger rows.
   - `@review` — a judgment trail. **Check first whether it duplicates the
     substrate's own `review-history`** on memory/pattern attachments, which
     already records reviewer, verdict and date. If it does, the migration is a
     deletion, not a move.

## Constraints

- Pattern files are versioned, reviewed artifacts. Nothing that changes without
  an editorial act belongs in them.
- Do not delete `@status` values before the `blocked-by` edges are captured
  somewhere. Losing a real relation to tidy a field would be the worse trade.
- The flexiarg ingest whitelist stays closed to all four regardless of how this
  excursion resolves (`futon3/README-flexiarg.md` §5c).

## Status

**Queued, unstarted.** Raised so the pattern-library reorg
(M-apm-demonstration, W.77 onward) is not blocked on a workflow decision.
