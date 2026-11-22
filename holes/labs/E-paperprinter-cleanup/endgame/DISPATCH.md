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
- `integrate`: control-flow or structural printer changes.  Exactly one may be
  `active`.  Later structural candidates are `waiting` and name their
  dependency; they are rebased and remeasured after the active candidate is
  accepted or rejected.

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
more than one active integration item, an accepted-head base for every active
integration/data item, a named mechanism and preflight, and an explicit
dependency for waiting integration work.  Update the ledgers before dispatch,
not retrospectively at merge time.

At each accepted head (after the merge commit and its full census are stored
under `~/storage/apm-endgame/head-<sha>/`), also run:

    python3 scan.py '<renders glob>' <sha> | cut -f1-3 >> counts.tsv
    python3 ink.py '<renders glob>' --summary .../residual-ink-summary.tsv \
        --annotations .../residual-ink-annotations.jsonl.gz --html .../residual-ink.html

and add the head's row to `INK.md`. Ink is recorded for accepted heads only. A
lower total never authorizes a merge that regresses a record; see `INK.md`.

The integrate order is whatever `dispatch.tsv` says (one `active`, the rest
`waiting` with `depends_on`). Observation and data items may run in parallel.
