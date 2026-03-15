# Invariants Interface

The Arxana invariants interface currently has two distinct surfaces:

1. Live violations
2. Candidate invariant queue

They are related, but they are not the same kind of thing.

## Live violations

The `Live Violations` view is store-backed. It reads active
`invariant/violation` hyperedges from futon1a and shows concrete instances
that were emitted by futon3c's structural-law aggregate.

These rows are:

- specific
- instance-level
- potentially actionable
- archivable by reconciliation

Examples:

- `required-outputs/missing-phase-outputs`
- `dependency-satisfaction/proved-with-unproved-deps`

Resolution means changing the underlying state so the next aggregate run no
longer emits the violation. Archiving does not mean deleting the row by hand;
it means rerunning the aggregate so the bridge republishes the old hyperedge as
`state = cleared` and `active = false`.

## Candidate invariant queue

The `Candidate Queue` view is registry-backed. It reads concrete candidate
invariant entries from `futon3c/docs/structural-law-inventory.sexp`.

These rows are:

- invariant-level
- not active defects
- design pressure
- evidence for what might later become always-on law

Examples:

- `repl-turns-emit-evidence`
- `violations-become-obligations`
- `math-tool-home-repo`

The queue is meant to surface promotion pressure: which concrete invariants are
waiting to be brought online, which family they belong to, and which repo or
family definition currently owns them.

## Why the split exists

This is intentionally a recapitulation-in-advance surface.

Eventually, separate docbooks and per-repo invariant pages may explain each
family in local detail. For now, Arxana needs one place where you can see:

- what is currently violated in the live stack
- what invariant families are waiting to be brought online

Keeping violations and candidates separate avoids a common confusion:
candidates are not bugs, and active violations are not the whole invariant
story.

## Current sources of truth

- Live violations:
  `futon3c/src/futon3c/logic/arxana_bridge.clj`
- Candidate queue:
  `futon3c/docs/structural-law-inventory.sexp`
- Browser UI:
  `futon4/dev/arxana-browser-core.el`
  `futon4/dev/arxana-browser-lab.el`
