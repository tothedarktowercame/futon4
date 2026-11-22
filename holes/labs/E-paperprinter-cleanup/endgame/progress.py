#!/usr/bin/env python3
"""Render a measurable PaperPrinter residual report from the endgame ledgers."""
import collections
import csv
import os
from datetime import date

from branch_priority import score

HERE = os.path.dirname(os.path.abspath(__file__))


def read(name):
    with open(os.path.join(HERE, name), newline="") as f:
        return list(csv.DictReader(f, delimiter="\t"))


def main():
    classes = read("classes.tsv")
    backlog = read("backlog.tsv")
    dispatch = read("dispatch.tsv")
    queue = read("queue.tsv")
    observations = read("observations.tsv")
    branches = read("branches.tsv")
    states = collections.Counter(r["state"] for r in classes)
    outcomes = collections.Counter(r["outcome"] for r in queue)
    recent = [r for r in queue if r["date"] >= "2026-11-20"]
    recent_outcomes = collections.Counter(r["outcome"] for r in recent)
    active = [r for r in dispatch if r["status"] == "active"]
    waiting = [r for r in dispatch if r["status"] == "waiting"]
    classified = collections.Counter(r["classification"] for r in backlog)
    history = collections.Counter(r["status"] for r in observations)

    print(f"# PaperPrinter residual — {date.today().isoformat()}\n")
    print("## Finish line\n")
    print(f"- Frozen classes: **{len(classes)}**: {states['done']} done, "
          f"{states['notdefect']} not defects, {states['open']} open, {states['unknown']} unknown.")
    print("- Corpus gate: 8,778 records; accepted head recorded by the dispatch ledger: `51c23004`.")
    print("- Proof errors: 0 at the accepted head. Performance class: closed.")
    print("- Largest measured residual, N1: **627 rows in 359 records**, down from "
          "1,928 rows in 1,035 records at the full-corpus freeze (67.5% of rows removed).")
    print("- C4 repeated-chain residual: last recorded **69 records / 95 rows**; the next count waits on N2.")

    print("\n## Work now\n")
    for r in active:
        print(f"- ACTIVE `{r['item']}` ({r['lane']}, {r['classes']}): {r['output']}.")
    for r in waiting:
        print(f"- WAITING `{r['item']}` ({r['lane']}, {r['classes']}), after `{r['depends_on']}`: {r['output']}.")

    print("\n## Backlog understanding\n")
    print("- Observation coverage: **%d/%d** open or unknown classes." %
          (len(backlog), states['open'] + states['unknown']))
    print("- Classification state: " + ", ".join(f"{k} {v}" for k, v in sorted(classified.items())) + ".")
    print("- Historical grammar observations: " + ", ".join(f"{k} {v}" for k, v in sorted(history.items())) + ".")

    print("\n## Next branches to inspect\n")
    ranked = sorted(branches, key=lambda r: (-score(r), r["branch"]))
    for rank, row in enumerate(ranked[:5], 1):
        print(f"{rank}. **{row['branch']}** ({row['classes']}), score {score(row)}: {row['next_probe']}.")
    print("\nThe score uses ordinal 1–5 inputs: `4×blocking + 3×uncertainty + 2×volume + "
          "2×split-value − 2×observation-cost − 2×overlap-risk`. It ranks the next observation, "
          "not the importance of the mathematical content.")

    print("\n## Throughput and uncertainty\n")
    print(f"- Packet ledger: {len(queue)} total; {outcomes['merged']} merged, {outcomes['rejected']} rejected, "
          f"{outcomes['discovery']} discovery.")
    print(f"- Since 2026-11-20: {len(recent)} packets; {recent_outcomes['merged']} merged, "
          f"{recent_outcomes['rejected']} rejected, {recent_outcomes['discovery']} discovery.")
    print("- Planning band, not a promise: **3–7 focused working days** if Q161r/Q157s settle the two structural seams; "
          "**1–2 weeks** if either requires another mechanism cycle. The small tail is mostly hours per class, "
          "but the unclassified proof-quality group cannot be estimated responsibly until its observation pass.")

    print("\n## Acceleration levers\n")
    print("1. Keep one structural candidate in integration; use other seats for observation and additive readings.")
    print("2. Run the adversarial preflight before a full census; reject statement/chain flips and raw-name gains early.")
    print("3. Continue frequency-ordered N1 reading batches while the structural candidate is reviewed.")
    print("4. Classify the nine unclassified/stale/unknown tail rows before dispatching their implementations.")
    print("5. Preserve every supported, falsified, and superseded notation hypothesis in `observations.tsv`; "
          "this is the seed corpus for a later expository grammar rather than disposable queue exhaust.")


if __name__ == "__main__":
    main()
