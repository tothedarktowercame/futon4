#!/usr/bin/env python3
"""Validate the PaperPrinter observation backlog and single integration train."""
import argparse
import csv
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))


def read(name):
    with open(os.path.join(HERE, name), newline="") as f:
        return list(csv.DictReader(f, delimiter="\t"))


def validate(classes, backlog, dispatch, observations=None):
    errors = []
    open_classes = {r["id"] for r in classes if r["state"] in {"open", "unknown"}}
    observed = [r["class"] for r in backlog]
    if len(observed) != len(set(observed)):
        errors.append("backlog.tsv contains duplicate class rows")
    missing = sorted(open_classes - set(observed))
    extra = sorted(set(observed) - open_classes)
    if missing:
        errors.append("open/unknown classes missing from backlog.tsv: " + " ".join(missing))
    if extra:
        errors.append("backlog.tsv classes are not open/unknown: " + " ".join(extra))
    for row in backlog:
        for key in ("incoming_data", "classification", "mechanism", "next_observation", "disposition"):
            if not row[key].strip():
                errors.append(f"backlog {row['class']}: empty {key}")

    active_integrations = [r for r in dispatch if r["lane"] == "integrate" and r["status"] == "active"]
    if len(active_integrations) > 1:
        errors.append("more than one active integration item: " + " ".join(r["item"] for r in active_integrations))
    known_items = {r["item"] for r in dispatch}
    accepted_bases = {r["base"] for r in active_integrations}
    for row in dispatch:
        if row["lane"] not in {"observe", "data", "integrate"}:
            errors.append(f"{row['item']}: unknown lane {row['lane']}")
        if row["status"] not in {"active", "waiting", "done", "rejected"}:
            errors.append(f"{row['item']}: unknown status {row['status']}")
        if row["lane"] in {"data", "integrate"}:
            if not row["mechanism"].strip() or not row["preflight"].strip():
                errors.append(f"{row['item']}: integration/data work needs mechanism and preflight")
        if row["lane"] in {"data", "integrate"} and row["status"] == "active" and not row["base"].strip():
            errors.append(f"{row['item']}: active merge candidate has no accepted-head base")
        if row["lane"] == "integrate" and row["status"] == "waiting":
            dep = row["depends_on"]
            if dep in {"", "-"}:
                errors.append(f"{row['item']}: waiting integration item has no dependency")
            elif dep not in known_items:
                errors.append(f"{row['item']}: unknown dependency {dep}")
        for cls in row["classes"].split("+"):
            # A finished item may name a class it closed; only pending work must target an open class.
            if row["status"] in {"active", "waiting"} and cls not in open_classes:
                errors.append(f"{row['item']}: class {cls} is not open/unknown")
    if len(accepted_bases) > 1:
        errors.append("active integration items do not share one accepted head")
    if observations is not None:
        ids = [r["id"] for r in observations]
        if len(ids) != len(set(ids)):
            errors.append("observations.tsv contains duplicate ids")
        known_observations = set(ids)
        known_classes = {r["id"] for r in classes}
        allowed = {"proposed", "supported", "falsified", "superseded"}
        for row in observations:
            if row["class"] not in known_classes:
                errors.append(f"observation {row['id']}: unknown class {row['class']}")
            if row["status"] not in allowed:
                errors.append(f"observation {row['id']}: unknown status {row['status']}")
            parent = row["supersedes"]
            if parent not in {"", "-"} and parent not in known_observations:
                errors.append(f"observation {row['id']}: unknown superseded observation {parent}")
            for key in ("at", "kind", "vocabulary_or_notation", "claim", "evidence"):
                if not row[key].strip():
                    errors.append(f"observation {row['id']}: empty {key}")
    return errors


def selftest():
    classes = [
        {"id": "A", "state": "open"},
        {"id": "B", "state": "unknown"},
        {"id": "C", "state": "done"},
    ]
    backlog = [
        dict(zip(("class", "incoming_data", "classification", "mechanism", "next_observation", "disposition"),
                 (x, "data", "known", "cause", "look", "route")))
        for x in ("A", "B")
    ]
    base = dict(lane="integrate", status="active", base="abc", classes="A",
                mechanism="cause", preflight="matrix", depends_on="-", output="decision")
    assert validate(classes, backlog, [dict(base, item="q1")]) == []
    errors = validate(classes, backlog, [dict(base, item="q1"), dict(base, item="q2")])
    assert any("more than one" in e for e in errors), errors
    waiting = dict(base, item="q2", status="waiting", classes="B", depends_on="-")
    errors = validate(classes, backlog, [dict(base, item="q1"), waiting])
    assert any("no dependency" in e for e in errors), errors
    print("selftest ok: backlog coverage and a single structural integration train are enforced")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--check", action="store_true")
    ap.add_argument("--selftest", action="store_true")
    args = ap.parse_args()
    if args.selftest:
        return selftest()
    errors = validate(read("classes.tsv"), read("backlog.tsv"), read("dispatch.tsv"), read("observations.tsv"))
    if errors:
        for error in errors:
            print("VIOLATION " + error)
        return 1
    print("dispatch ok: backlog covers every open/unknown class; one active structural integration item")
    return 0


if __name__ == "__main__":
    sys.exit(main())
