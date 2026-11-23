# PaperPrinter endgame dispatch protocol

**Operator ruling, 2026-11-22.** Integration repair and understanding the
incoming backlog are symmetric work.  Run both, but do not confuse them.

`backlog.tsv` is the observation vector: every open or unknown class has one
row saying what data is arriving, how well it is classified, the present
mechanism hypothesis, and the next observation that would reduce uncertainty.
An observation may change this ledger; it does not authorize printer code.

`dispatch.tsv` is the integration train.  Its lanes are:

- `observe`: read-only census, classification, tracing, or preparation of
  additive data readings.  Several may run at once.
- `data`: additive readings whose changed-output contract is "only insert the
  intended reason/reading".  These may be prepared in parallel, but merge only
  against the accepted head.
- `integrate`: control-flow or structural printer changes.  Several may be
  `active` at once as one **batch** (Joe, 2026-11-23: the box is free and the
  packets are parallelisable), on these conditions: every member is authored on
  the same accepted head; no member depends on an unfinished item, so items
  that touch the same function stay chained by `depends_on` and are `waiting`;
  the members are merged together in one trial merge and gated by ONE full
  census.  In that census every changed record is attributed to the member
  whose change explains it; a record changed by two members, or by none, is
  read in full.  A member whose changes fail review is dropped from the trial
  merge and the census is rerun without it.  The ink comparison is run on the
  batch census and each NEW/GROWN record is attributed the same way.

Before a structural full census, the candidate must pass the fixed adversarial
preflight: repeated rows; empty reasons; raw names; statement-to-chain flips;
`0 > 0`; printed self-equalities; lost algebra reasons; and the budget-edge
modules.  The full 8,778-record census remains the merge gate.

`observations.tsv` is append-only evidence for the later expository grammar.
Do not erase a notation or vocabulary hypothesis when it fails.  Mark it
`falsified` or `superseded`, link its successor, and retain the measured
evidence.  `supported` means supported on the stated corpus and commit, not a
timeless printer law.  `proposed` is deliberately distinguishable from both.

`branches.tsv` makes the choice of what to inspect next explicit.  Its ordinal
scores estimate volume, uncertainty, expected class-splitting value,
observation cost, overlap risk, and schedule blocking.  `branch_priority.py`
ranks observations with a published formula; the ranking is revisable data,
not an oracle or an implementation authorization.

Run:

    python3 dispatch.py --check
    python3 branch_priority.py
    python3 progress.py > PROGRESS.md

The check requires every open/unknown class to be present in `backlog.tsv`, no
active integration item that depends on an unfinished one, one shared accepted
head for the active batch, an accepted-head base for every active
integration/data item, a named mechanism and preflight, and an explicit
dependency for waiting integration work.  Update the ledgers before dispatch,
not retrospectively at merge time.

## Residual ink is part of every merge decision (Joe, 2026-11-22)

Before the merge decision, on the candidate's full census (renders still in `/tmp`):

    python3 ink.py '/tmp/qNh-k-0?.jsonl' --annotations /tmp/qN-ink.jsonl.gz --html /tmp/qN-ink.html
    python3 ink_diff.py <accepted head sha> /tmp/qN-ink.jsonl.gz

`ink_diff.py` lists every record whose color is NEW, GROWN, MOVED, SHRUNK or GONE and exits 1
on any NEW or GROWN. Each NEW or GROWN record is read in the colored HTML and either explained
in the merge message and report (for example a class that became visible, with its packet) or
the candidate is rejected or fixed. A lower total never excuses a regressed record. Candidate
ink stays in `/tmp`; it is not progress.

After the merge commit, with the census stored under `~/storage/apm-endgame/head-<sha>/`:

    python3 accept_head.py <sha> '~/storage/apm-endgame/head-<sha>/qNh-k-0?.jsonl'

This appends the class counts to `counts.tsv`, writes the ink summary, annotations and HTML
beside the renders, appends the head to `ink.tsv`, and regenerates `PROGRESS.md`. It refuses a
sha that is not on the integration branch, renders outside the head's storage directory, and
a census of fewer than 8,778 records. `dispatch.py --check` fails while the last counted head
has no ink row, so the next dispatch cannot go out before the head is recorded.

A class gets an interval extractor in `ink.py` when its inventory is next touched (a packet
written, an observe report read); the `COVERAGE` row says how many open classes have one.
The report to Joe after each merge carries the ink line and the `ink_diff.py` tally.

The integrate order is whatever `dispatch.tsv` says (a batch of `active` items on one head, the rest
`waiting` with `depends_on`). Observation and data items may run in parallel.
