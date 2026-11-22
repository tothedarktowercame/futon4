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
    print("**Status: rendering works; informalisation validation is in progress** (`READINESS.md`: six gates, "
          "one met). The ink figures below are an audit surface, not a share of correct proofs.\n")
    print("## Finish line\n")
    print(f"- Frozen classes: **{len(classes)}**: {states['done']} done, "
          f"{states['notdefect']} not defects, {states['open']} open, {states['unknown']} unknown.")
    counts = read("counts.tsv")
    head = counts[-1]["commit"]
    at_head = {r["class"]: r["open"] for r in counts if r["commit"] == head}
    print(f"- Corpus gate: 8,778 records; accepted head with a full census: `{head}`.")
    print("- Proof errors: 0 at the accepted head. Performance class: closed.")
    print(f"- N1 (rows with an empty reason): **{at_head.get('N1', '?')} records** at this head, down from "
          "1,035 records at the full-corpus freeze.")
    print(f"- C4 (blocks with a repeated left side, scanner count): **{at_head.get('C4', '?')} records**.")
    ink = [r for r in read("ink.tsv")] if os.path.exists(os.path.join(HERE, "ink.tsv")) else []
    heads = list(dict.fromkeys(r["head"] for r in ink))
    if head in heads:
        now = {r["class"]: r for r in ink if r["head"] == head}
        print(f"- Residual ink (audit surface; {now['COVERAGE']['marked_chars']} have markup, extractor precision not yet reviewed): "
              f"**{float(now['UNION']['percent_total']):.3f}%** of reader-visible proof-source characters, "
              f"in {now['UNION']['records']} records; see `INK.md`.")
        i = heads.index(head)
        if i:
            was = {r["class"]: r for r in ink if r["head"] == heads[i - 1]}
            moved = []
            for cls, r in now.items():
                if cls in ("TOTAL", "COVERAGE", "UNION") or cls not in was:
                    continue
                d = int(r["records"]) - int(was[cls]["records"])
                if d:
                    moved.append(f"{cls} {d:+d} records")
            delta = float(now["UNION"]["percent_total"]) - float(was["UNION"]["percent_total"])
            print(f"- Ink change since `{heads[i - 1]}`: union {delta:+.3f} points"
                  + ("; " + ", ".join(moved) if moved else "; no class changed its record count") + ".")
        largest = sorted((r for c, r in now.items() if c not in ("TOTAL", "COVERAGE", "UNION")),
                         key=lambda r: -float(r["percent_total"]))[:4]
        print("- Largest colors: " + ", ".join(f"{r['class']} {float(r['percent_total']):.2f}%" for r in largest
                                               if float(r["percent_total"]) > 0) + ".")
    else:
        print(f"- Residual ink: NOT recorded for `{head}`: run `accept_head.py`.")

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
    print("- Planning band, not a promise: **3–7 focused working days** if the queued integrate packets land without a new mechanism cycle; "
          "**1–2 weeks** if C4/N2 or C23 needs one. The small tail is mostly hours per class, "
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
