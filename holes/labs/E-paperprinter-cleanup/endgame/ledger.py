#!/usr/bin/env python3
"""The grammar ledger: where each of the 13 rules stands at one accepted head.

  ledger.py HEAD_DIR [OUT.tsv]

HEAD_DIR is ~/storage/apm-endgame/head-<sha>/ (census shards *h-k-0?.jsonl and rule-flags.jsonl,
written by accept_head.py).  Writes OUT.tsv (default HEAD_DIR/grammar-ledger.tsv) and prints it.

Joe, 2026-11-28: nothing goes to mfuton-share until the grammar is solid, with all the checks and
all of the conformance sorted out, so that any later bug report opens a new aspect of the grammar.
Rigor is tracked per rule, as a level on a fixed ladder (GRAMMAR.md, "The ledger"):

  0 unchecked   grammar.tsv states the rule; no typed check exists (rules/check_<RULE>.py).
  1 checked     a typed check exists and runs at the census.
  2 validated   the check catches every constructed bad case in rule-validation.tsv and passes every
                good one (validate_rules.py -> rule-validation-results.jsonl), and a read sample of
                its flags is >= 13/15 real (rule-precision.tsv), or, for a rule with
                fewer flags, every flag at the head was read and >= 13/15 of them are real.  A check with no bad case is not
                validated: a check nothing can fail proves nothing.
  3 covered     the check decides >= COVERAGE of the records: a record with any "unmeasured" row
                for the rule is undecided, never counted as clean.
  4 conformant  no record violates the rule, except those listed by name in rule-exceptions.tsv.

A level needs every level below it.  The push gate is level 4 on all 13 rules.
"""
import collections, csv, glob, gzip, json, os, sys

HERE = os.path.dirname(os.path.abspath(__file__))
COVERAGE = 0.95          # provisional bar for level 3 (claude-16, 2026-11-28); revisit with Joe
PRECISION = (13, 15)     # the discovery-sample bar used since 2026-11-26
LEVELS = ["unchecked", "checked", "validated", "covered", "conformant"]


def tsv(name):
    path = os.path.join(HERE, name)
    if not os.path.exists(path):
        return []
    with open(path, encoding="utf-8") as f:
        return [r for r in csv.DictReader(f, delimiter="\t", quoting=csv.QUOTE_NONE) if not (r.get("rule") or "").startswith("#")]


def jsonl(path):
    if not os.path.exists(path):
        return []
    with open(path, encoding="utf-8") as f:
        return [json.loads(l) for l in f if l.strip()]


def main(head_dir, out_path=None):
    head_dir = os.path.abspath(os.path.expanduser(head_dir))
    head = os.path.basename(head_dir).removeprefix("head-")
    records = set()
    for path in sorted(glob.glob(os.path.join(head_dir, "*h-k-0?.jsonl"))):
        for row in jsonl(path):
            records.add((row["module"], row["declaration"]))
    total = len(records)
    if not total:
        sys.exit(f"no census shards in {head_dir}")

    violating, undecided = collections.defaultdict(set), collections.defaultdict(set)
    for f in jsonl(os.path.join(head_dir, "rule-flags.jsonl")):
        key = (f["module"], f["declaration"])
        (undecided if f.get("unmeasured") else violating)[f["rule"]].add(key)

    # The checks that ran at this head: rule-flags.jsonl.rules (rule_checks.py, from 2026-11-28);
    # for older heads, a rule with at least one row.  A check written after the census has not
    # measured this head: it is "checked" but decides nothing here.
    ran_path = os.path.join(head_dir, "rule-flags.jsonl.rules")
    if os.path.exists(ran_path):
        with open(ran_path, encoding="utf-8") as f:
            ran = {line.split("\t")[0] for line in f if line.strip()}
    else:
        ran = set(violating) | set(undecided)

    exceptions = collections.defaultdict(set)
    for r in tsv("rule-exceptions.tsv"):
        exceptions[r["rule"]].add((r["module"], r["declaration"]))
    cases = collections.defaultdict(list)
    for r in tsv("rule-validation.tsv"):
        cases[r["rule"]].append(r["id"])
    results = {(r["rule"], r["id"]): r for r in jsonl(os.path.join(HERE, "rule-validation-results.jsonl"))
               if r.get("head") == head}
    reads = collections.defaultdict(lambda: [0, 0])
    read_keys = collections.defaultdict(set)
    for r in tsv("rule-precision.tsv"):
        reads[r["rule"]][1] += 1
        reads[r["rule"]][0] += r["verdict"] == "real"
        read_keys[r["rule"]].add((r["module"], r["declaration"]))

    # Text marks (ink classes mapped by class-rules.tsv): what is known of a rule with no typed
    # check.  Informational only: a text mark never raises a level.
    rule_of = {r["class"]: r["rule"] for r in tsv("class-rules.tsv")}
    marked = collections.defaultdict(set)
    ann = os.path.join(head_dir, "residual-ink-annotations.jsonl.gz")
    if os.path.exists(ann):
        with gzip.open(ann, "rt", encoding="utf-8") as f:
            for line in f:
                a = json.loads(line)
                for sp in a.get("spans", []):
                    marked[rule_of.get(sp["class"], "?")].add((a["module"], a["declaration"]))

    rows = []
    for g in tsv("grammar.tsv"):
        rule = g["rule"]
        checked = os.path.exists(os.path.join(HERE, "rules", f"check_{rule}.py"))
        ids = cases[rule]
        passed = sum(1 for i in ids if results.get((rule, i), {}).get("ok"))
        real, read = reads[rule]
        measured = checked and rule in ran
        # claude-16, 2026-11-28: a rule that fires fewer than 15 times at this head can never reach
        # 15 reads.  When every violating record at this head has been read, the read is the whole
        # population, not a sample: the ratio bar alone applies.
        census_read = measured and violating[rule] and violating[rule] <= read_keys[rule]
        precise = ((read >= PRECISION[1] or census_read)
                   and real * PRECISION[1] >= PRECISION[0] * read)
        validated = checked and ids and passed == len(ids) and precise
        undec = undecided[rule] - violating[rule] if measured else records
        decided = total - len(undec)
        viol = violating[rule] - exceptions[rule]
        level = 0
        if checked:
            level = 1
            if validated:
                level = 2
                if measured and decided >= COVERAGE * total:
                    level = 3
                    if not viol:
                        level = 4
        rows.append({
            "rule": rule, "family": g["family"], "level": f"{level} {LEVELS[level]}",
            "check": f"rules/check_{rule}.py" if checked else "none (" + g["check"][:60] + ")",
            "cases": f"{passed}/{len(ids)} pass" if ids else "none",
            "precision": (f"{real}/{read} real" + (" (every flag)" if census_read else "")) if read else "not read",
            "decided": f"{decided}/{total} ({100 * decided / total:.1f}%)" if measured else
                       ("not run at this head" if checked else "-"),
            "violating": f"{len(viol)} ({100 * len(viol) / total:.1f}%)" if measured else "unknown",
            "exceptions": str(len(exceptions[rule] & violating[rule])),
            "text_marks": str(len(marked[rule])),
            "head": head,
        })
    out_path = out_path or os.path.join(head_dir, "grammar-ledger.tsv")
    with open(out_path, "w", encoding="utf-8", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0]), delimiter="\t", lineterminator="\n")
        w.writeheader()
        w.writerows(rows)
    with open(out_path, encoding="utf-8") as f:
        sys.stdout.write(f.read())
    at4 = sum(r["level"].startswith("4") for r in rows)
    print(f"\n{at4}/{len(rows)} rules conformant; push gate needs {len(rows)}/{len(rows)}.  records {total}")


if __name__ == "__main__":
    if len(sys.argv) not in (2, 3):
        sys.exit(__doc__)
    main(*sys.argv[1:])
