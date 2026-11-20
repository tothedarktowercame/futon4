# PaperPrinter endgame: process model and dry run (claude-10, 2026-11-20)

Joe's go, 2026-11-20: freeze the list of defect classes, require a count that falls,
give each class one of three exits (printer fix, Lean notation change, accepted residual),
and stop when every frozen class has left. Status: draft, built from claude-7's queue logs
only. `classes.tsv` and `queue.tsv` are claude-10's reading of those logs and have not
been checked by claude-7; rows with size `?` need a census count.

    bb -cp ~/code/futon3c/src check.clj endgame-process.json   # causal model, 8 checks
    python3 endgame.py --selftest
    python3 endgame.py --freeze 2026-11-20                     # dry run; --cap 3 --min-size 2

`check.clj` is copied from exotype `causal/check.clj` (branch c1/causal-model).
`counts.tsv` (commit, class, open; in merge order) does not exist yet: it is the per-merge
count the plan needs, and comes from a full rerender at the freeze commit.
