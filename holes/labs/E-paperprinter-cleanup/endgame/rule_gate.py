#!/usr/bin/env python3
"""Per-rule gate between two censuses: does a candidate break a rule a record kept before?

  rule_gate.py PREV_ANNOTATIONS CAND_ANNOTATIONS

Annotations are ink.py --annotations output (.jsonl or .jsonl.gz).  Each ink class is mapped to
its rule by class-rules.tsv; rules are defined in grammar.tsv (GRAMMAR.md).  For every rule prints
the records that violate it before and after, and lists each record that violates a rule it did
not violate before (a NEW line).  Exit 1 if there is any NEW line.  Rules with no census detector
are listed as unmeasured: for them this gate is silent, not clean.  Citations (R1, R2) are also
gated by citcount.py and citres.py, which this script does not run.
"""
import csv, gzip, json, os, sys
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))


def table(name, key):
    with open(os.path.join(HERE, name), newline="") as f:
        return {r[key]: r for r in csv.DictReader(f, delimiter="\t")}


def violations(path, rule_of):
    opener = gzip.open if path.endswith(".gz") else open
    out = defaultdict(set)
    with opener(path, "rt", encoding="utf-8") as f:
        for line in f:
            r = json.loads(line)
            for span in r.get("spans", []):
                rule = rule_of.get(span["class"], "?")
                out[rule].add(r["declaration"])
    return out


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    grammar = table("grammar.tsv", "rule")
    rule_of = {c: r["rule"] for c, r in table("class-rules.tsv", "class").items()}
    sys.path.insert(0, HERE)
    from ink import PALETTE
    measured = {rule_of.get(c) for c in PALETTE}
    before, after = (violations(p, rule_of) for p in sys.argv[1:])
    new = 0
    print("rule\tfamily\tbefore\tafter\tnew")
    for rule, g in grammar.items():
        added = sorted(after[rule] - before[rule])
        new += len(added)
        tag = "" if rule in measured else "\t(unmeasured at census)"
        print(f"{rule}\t{g['family']}\t{len(before[rule])}\t{len(after[rule])}\t{len(added)}{tag}")
        for d in added:
            print(f"  NEW {rule} {d}")
    if "?" in before or "?" in after:
        print("  classes with no rule in class-rules.tsv: fix the map before trusting this gate")
        new += 1
    sys.exit(1 if new else 0)


if __name__ == "__main__":
    main()
