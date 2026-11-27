#!/usr/bin/env python3
"""Run every rule check (rules/check_<RULE>.py) over a census; write one flag per violation.

  rule_checks.py 'SHARDS_GLOB' OUT.jsonl [RULES_DIR]

A check module defines
    RULE = "T2"                       # a rule id in grammar.tsv
    def check(row, const, constants): # -> list of {"reason": str, "anchor": str}
row is the census record (module, declaration, proof, ...); const is that declaration's
elaborated constant (type_expression, value_expression) or None; constants(module) returns the
module's constants by dotted name.  Decide on typed data; use the page text only to locate the
violation (anchor: a verbatim substring of row["proof"], or "" for the statement / whole record).
A check that cannot decide a record returns [{"reason": "unmeasured: ...", "anchor": ""}];
those are written with "unmeasured": true and are not violations.

Run like typed_flags.py: from a git-archive snapshot of the census head, with
~/notes/claude-16/coord/typed-flags-env.sh sourced.  rule_gate.py reads OUT.jsonl (GRAMMAR.md).
"""
import glob, importlib.util, json, os, sys, time

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path[:0] = ["src", ".", "test/mfuton", HERE]
import typed_flags  # noqa: E402  (constants(module) with the corpus provider)


def load_checks(rules_dir):
    checks = []
    for path in sorted(glob.glob(os.path.join(rules_dir, "check_*.py"))):
        spec = importlib.util.spec_from_file_location(os.path.basename(path)[:-3], path)
        mod = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(mod)
        checks.append(mod)
    return checks


def main(pattern, out_path, rules_dir):
    checks = load_checks(rules_dir)
    counts = {c.RULE: [0, 0, 0.0] for c in checks}  # violations, unmeasured, seconds
    with open(out_path, "w", encoding="utf-8") as out:
        for path in sorted(glob.glob(pattern)):
            for line in open(path, encoding="utf-8"):
                row = json.loads(line)
                try:
                    const = typed_flags.constants(row["module"]).get(row["declaration"])
                except Exception as ex:
                    print(f"ERROR constants {row['module']}: {str(ex)[:120]}", file=sys.stderr)
                    const = None
                for c in checks:
                    t0 = time.time()
                    try:
                        hits = c.check(row, const, typed_flags.constants) or []
                    except Exception as ex:  # report, never guess
                        hits = [{"reason": f"unmeasured: check raised {type(ex).__name__}: {str(ex)[:100]}", "anchor": ""}]
                    counts[c.RULE][2] += time.time() - t0
                    for h in hits:
                        um = h["reason"].startswith("unmeasured")
                        counts[c.RULE][1 if um else 0] += 1
                        out.write(json.dumps({"rule": c.RULE, "module": row["module"],
                                              "declaration": row["declaration"], "reason": h["reason"],
                                              "anchor": h.get("anchor", ""), "unmeasured": um},
                                             ensure_ascii=False, sort_keys=True) + "\n")
    for rule, (v, u, s) in counts.items():
        print(f"{rule}\tviolations {v}\tunmeasured {u}\t{s:.0f}s", file=sys.stderr)


if __name__ == "__main__":
    if len(sys.argv) not in (3, 4):
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2], sys.argv[3] if len(sys.argv) == 4 else os.path.join(HERE, "rules"))
