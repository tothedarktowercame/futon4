#!/usr/bin/env python3
"""Run each rule check on its constructed bad cases and its good cases (the ledger's level 2).

  validate_rules.py HEAD_DIR

Reads rule-validation.tsv: rule, id, module, declaration, find, replace, expect, match, note.
  expect=flag   a bad case: the record's page with `find` replaced by `replace` (the defect put back
                by hand) must get a violation from rules/check_<rule>.py.  `find` empty: the record as
                it stands at the head must be flagged (a defect the head still has).  `match`, if
                given, must occur in the violation's reason: a bad case is caught for its own
                defect, not for another one on the same page.
  expect=clean  a good case: the record (after the edit, if any) must get no violation.
A newline in find/replace is written ⏎ (a backslash-n would collide with TeX such as \\neq).
`find` must occur exactly once in the record's page; otherwise the case is STALE (the page moved
on) and fails until it is rewritten.  An "unmeasured" answer fails a case: a check that cannot
decide was not shown to catch anything.

Writes rule-validation-results.jsonl next to this file (one row per case, with the head), which
ledger.py reads.  Run like rule_checks.py: from a git-archive snapshot of the head, with
~/notes/claude-16/coord/typed-flags-env.sh sourced.
"""
import csv, glob, importlib.util, json, os, sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = ["src", ".", "test/mfuton", HERE]
import typed_flags  # noqa: E402


def load_check(rule):
    path = os.path.join(HERE, "rules", f"check_{rule}.py")
    if not os.path.exists(path):
        return None
    spec = importlib.util.spec_from_file_location(f"check_{rule}", path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


def main(head_dir):
    head_dir = os.path.abspath(os.path.expanduser(head_dir))
    head = os.path.basename(head_dir).removeprefix("head-")
    with open(os.path.join(HERE, "rule-validation.tsv"), encoding="utf-8") as f:
        cases = [r for r in csv.DictReader(f, delimiter="\t", quoting=csv.QUOTE_NONE) if not r["rule"].startswith("#")]
    wanted = {(c["module"], c["declaration"]) for c in cases}
    rows = {}
    for path in sorted(glob.glob(os.path.join(head_dir, "*h-k-0?.jsonl"))):
        with open(path, encoding="utf-8") as f:
            for line in f:
                r = json.loads(line)
                if (r["module"], r["declaration"]) in wanted:
                    rows[(r["module"], r["declaration"])] = r
    checks, out = {}, []
    for c in cases:
        res = {"rule": c["rule"], "id": c["id"], "head": head, "expect": c["expect"], "ok": False}
        row = rows.get((c["module"], c["declaration"]))
        check = checks.setdefault(c["rule"], load_check(c["rule"]))
        if check is None:
            res["why"] = "no check for this rule"
        elif row is None:
            res["why"] = "record not in the census"
        else:
            row = dict(row)
            find, replace = c["find"].replace("⏎", "\n"), c["replace"].replace("⏎", "\n")
            if find:
                n = (row.get("proof") or "").count(find)
                if n != 1:
                    res["why"] = f"STALE: find occurs {n} times"
                else:
                    row["proof"] = row["proof"].replace(find, replace)
            if "why" not in res:
                const = typed_flags.constants(row["module"]).get(row["declaration"])
                try:
                    hits = check.check(row, const, typed_flags.constants) or []
                except Exception as ex:  # report, never guess
                    hits = [{"reason": f"unmeasured: check raised {type(ex).__name__}: {ex}"}]
                unmeasured = [h for h in hits if h["reason"].startswith("unmeasured")]
                viol = [h for h in hits if not h["reason"].startswith("unmeasured")
                        and (c["expect"] != "flag" or (c.get("match") or "") in h["reason"])]
                if c["expect"] == "flag":
                    res["ok"] = bool(viol)
                    res["why"] = viol[0]["reason"] if viol else (
                        "unmeasured: " + unmeasured[0]["reason"] if unmeasured else "not flagged")
                else:
                    res["ok"] = not viol and not unmeasured
                    res["why"] = (viol or unmeasured or [{"reason": "clean"}])[0]["reason"]
        out.append(res)
        print(f"{'ok  ' if res['ok'] else 'FAIL'} {c['rule']} {c['id']} expect={c['expect']}: {res['why'][:140]}")
    keep = []
    path = os.path.join(HERE, "rule-validation-results.jsonl")
    if os.path.exists(path):
        with open(path, encoding="utf-8") as f:
            keep = [l for l in f if json.loads(l).get("head") != head]
    with open(path, "w", encoding="utf-8") as f:
        f.writelines(keep)
        for r in out:
            f.write(json.dumps(r, ensure_ascii=False, sort_keys=True) + "\n")
    print(f"{sum(r['ok'] for r in out)}/{len(out)} cases pass at {head}")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        sys.exit(__doc__)
    main(sys.argv[1])
