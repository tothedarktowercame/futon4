#!/usr/bin/env python3
"""Rank observation branches by schedule value and expected information gain.

All inputs are ordinal 1..5.  The score is deliberately transparent rather
than statistical: blocking and uncertainty dominate, volume and the chance of
splitting a mixed class help, and observation cost and rule overlap subtract.
"""
import csv
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
FIELDS = ("volume", "uncertainty", "split_value", "observation_cost", "overlap_risk", "blocking")


def read_rows(path=None):
    with open(path or os.path.join(HERE, "branches.tsv"), newline="") as f:
        return list(csv.DictReader(f, delimiter="\t"))


def score(row):
    values = {k: int(row[k]) for k in FIELDS}
    return (4 * values["blocking"] + 3 * values["uncertainty"]
            + 2 * values["volume"] + 2 * values["split_value"]
            - 2 * values["observation_cost"] - 2 * values["overlap_risk"])


def validate(rows):
    errors = []
    names = [r["branch"] for r in rows]
    if len(names) != len(set(names)):
        errors.append("duplicate branch name")
    for row in rows:
        for field in FIELDS:
            try:
                value = int(row[field])
            except ValueError:
                errors.append(f"{row['branch']}: {field} is not an integer")
                continue
            if value not in range(1, 6):
                errors.append(f"{row['branch']}: {field}={value}, expected 1..5")
        if not row["classes"].strip() or not row["next_probe"].strip():
            errors.append(f"{row['branch']}: missing classes or next_probe")
    return errors


def main():
    rows = read_rows()
    errors = validate(rows)
    if errors:
        for error in errors:
            print("VIOLATION " + error)
        return 1
    ranked = sorted(rows, key=lambda r: (-score(r), r["branch"]))
    print("rank\tscore\tbranch\tclasses\tnext_probe")
    for rank, row in enumerate(ranked, 1):
        print(f"{rank}\t{score(row)}\t{row['branch']}\t{row['classes']}\t{row['next_probe']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
