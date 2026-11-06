# V3 — VERIFY risk spikes

Mission: `M-jvm-arxana`. Date: 2026-11-06.

## S1 — atomic expected-head transaction

Ran against an isolated in-process XTDB 2.1.0 node, the version pinned by
futon1b. Initial head was R1. Two futures submitted transactions containing:

1. SQL `ASSERT EXISTS` that the head was R1;
2. update head to a distinct R2 value; and
3. insert the command journal row.

Observed:

```edn
{:outcomes [{:message "m1", :result :refused, :error "Assert failed"}
            {:message "m2", :result :committed}]
 :head [{:xt/id "artifact:1", :revision-id "R2b"}]
 :journal [{:xt/id "m2", :expected-revision "R1", :new-revision "R2b"}]
 :single-winner? true
 :single-journal? true}
```

Verdict: **pass at primitive/feasibility level**. XTDB serializes the two DML
transactions; the loser aborts the whole transaction, including its journal
row. This validates D7's choice of store-side conditional transaction rather
than an external sequencer. The production route, idempotency outcome and
reserved-namespace authentication remain INSTANTIATE work.

Primary reference: XTDB SQL transaction documentation states `ASSERT` rolls
back a transaction when false and is intended for concurrent constraints:
https://docs.xtdb.com/reference/main/sql/txs.html

## S2 — correspondence fixture rejection and rebuild feasibility

The committed `scopes.json` was checked against the three live sandbox sources.
Every recorded hash matched `.pristine`, not live input:

| File | recorded / `.pristine` | live sandbox | Match live? |
|---|---|---|---|
| `draft8.tex` | `988a1166f6841008` | `de7d8f8ac0bc2440` | no |
| `intro-generated.tex` | `c502ab28542b02cc` | `bccb3369c12b3cc0` | no |
| `part3-exotype.tex` | `289abcd84d90f992` | `3983ea06ae49f13f` | no |

The rejection half therefore passes.

A clean archive of futon5 commit `245773f` was extracted to
`/tmp/m-jvm-arxana-verify.qRP3ag` and rebuilt with the available
`latexml_oxide`. It produced complete HTML and 1,630 anchors, but regenerated
the same incorrect hashes. Root cause:

- `rebuild-page.sh` builds `html-build/wysiwyg/sandbox/draft8.tex`;
- it calls `make-scope-table.py --src "$PAPER"`;
- `resolve_from_log` discards each logged source path with `Path(...).name`;
- hash construction joins that basename to the paper root, thereby hashing
  root/`.pristine`-equivalent files rather than actual sandbox inputs.

Verdict: **stop-the-line design discovery; bounded DERIVE correction applied**.
A clean rebuild is not sufficient under the current builder. D4 must require
the manifest builder to preserve producer-resolved source paths, constrain them
to an allowed build root, and hash the exact bytes named by each source-map
index. The positive fixture remains an INSTANTIATE gate after that repair.

No futon5 working-tree file or live XTDB store was changed. The isolated build
directory is retained as reproducible evidence.

## Synthesis consequence

Atomicity risk is reduced enough for VERIFY: the selected database/version has
the required serializable ASSERT primitive. Fixture coherence exposed a real
builder defect and revised D4/D7, but does not reopen the service architecture.
VERIFY cannot claim a positive fixture exists; INSTANTIATE increment 1 must
repair the builder and produce it before any cross-client correspondence
demonstration.
