# P2 — scope strategy and cascade census

Mission: `M-jvm-arxana`. Phase: MAP. Intended worker: one Zai seat.

Discovery only. Do not port, refactor or generalize a strategy. Do not edit
production code or stores.

## Question

What strategy and finder forms exist, how are cascades represented and
evaluated, and which depend intrinsically on Emacs state?

## Work

1. Start with `futon4/dev/arxana-links.el` and
   `futon4/dev/arxana-data-constraints.el`; follow direct callers and tests.
2. Inventory constructors, stored fields, finder kinds, cascade/fallback
   ordering, ambiguity behaviour and failure results.
3. Trace at least three real examples: a named definition or code entity, a
   document heading/article target, and a relative or fallback strategy.
4. For every primitive used by those examples, classify its dependency as
   pure data/string, filesystem, parser/language service, Emacs buffer/marker,
   interactive UI, or external service.
5. Test the claim that an enclosing anchor plus a relative finder is stable
   against unrelated text before/after it, using existing tests or a disposable
   read-only fixture analysis. Do not add a test yet.

## Output and acceptance

Write `holes/labs/M-jvm-arxana/P2-scope-strategy-census-report.md` with:

- a strategy grammar/table as it exists, not as it should be;
- three source-cited traces;
- the dependency classification;
- observed ambiguity and failure semantics;
- `portable as-is / needs adapter / Emacs-intrinsic / unknown` columns;
- a Q2 `ready / missing` table.

Acceptance: distinguish stored strategy data from executable Elisp closures or
UI state. Commit only the report with message `M-jvm-arxana MAP P2: ...`.

