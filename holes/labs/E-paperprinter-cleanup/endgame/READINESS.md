# Informalisation readiness: not ready to announce

**Operator ruling, 2026-11-22.**  Residual ink is an audit-surface measure, not
a success percentage.  The current informalisation must not be announced as
successful while discovery is still opening substantial classes or while a
colored class lacks a reviewed mechanism and repair disposition.

At the current recorded head, `PROGRESS.md` reports 7.536% residual ink with
23/23 open-or-unknown classes assigned markup.  That establishes visibility,
not validation.  In particular:

- C18 through C23 were found after the first ink census;
- C23 shows that a kernel-correct local contradiction can be printed without
  the assumption that licenses it;
- C4's paragraph extractor undercounts the independently reviewed row metric;
- several P-* colors come from inventories whose individual mechanisms have
  not been classified; and
- the integration queue still contains active and waiting cleanup packets.

## Gates for a success claim

1. **Render integrity.**  The full frozen corpus renders with zero proof
   errors and the agreed performance budget.  Currently met for 8,778 records.
2. **Measured coverage.**  Every open or unknown class has an extractor whose
   span precision has been checked against its inventory.  Marking whole
   declarations from an inventory is useful as an upper surface estimate but
   does not satisfy this gate by itself.  Not yet met.
3. **Known-backlog closure.**  Every frozen class is fixed, shown not to be a
   defect, or routed with Joe's explicit ruling; no active integration item
   remains.  Not yet met.
4. **Dependency audit.**  Typed local assumptions, contradiction-closing
   roles, citations and subproof ownership survive into the informal proof.
   C23 is the first explicit scope class; its corpus extent and repair are
   still in progress.  Not yet met.
5. **Regression-free accepted head.**  The adversarial preflight, full census,
   changed-line review and colored-HTML review find no per-record regression.
   Aggregate ink reduction cannot substitute for this gate.  Not yet met for
   the final head because there is no final head.
6. **Post-cleanup validation pass.**  After the last repair, run a frozen,
   corpus-wide discovery pass that is not used to tune that repair.  Any new
   class reopens the relevant gate.  Not yet run.

Until all six gates hold, reports should say **rendering works; informalisation
validation is in progress**, followed by class and ink measurements.  They
should not translate residual ink into a percentage of semantic or expository
correctness.

## Larger contribution

The useful product is not only a prose renderer.  PythonEmit, the typed proof
walk, PaperPrinter and the audit ledgers can form a proof-understanding
pipeline:

1. retain elaborated proof structure and dependency scope;
2. emit a readable account;
3. compare that account with the typed dependencies;
4. classify losses, administrative residue and source-proof opportunities;
5. return narrow repairs either to the renderer or to the Lean source; and
6. preserve the resulting vocabulary and presentation evidence as an
   expository grammar.

That pipeline is potentially useful for generated developments far larger
than the present APM corpus.  Its contribution would be the ability to audit,
distil and navigate proof structure—not a claim that large accepted Lean
files are already understandable because they compile.
