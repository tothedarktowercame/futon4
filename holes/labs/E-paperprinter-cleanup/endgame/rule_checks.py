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
import glob, importlib.util, json, os, signal, sys, time
from concurrent.futures import ProcessPoolExecutor

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


#: claude-16, 2026-11-28: a per-record, per-check alarm.  check_D1 re-walks every record with no
#: deadline, and one walk in q309h-k-02-s00 ran for over three hours and held up the whole run.  A
#: check over max(ALARM_MIN, ALARM_MULT x the census render time) is unmeasured, never clean.  A
#: check that sets its own SIGALRM (check_D4) replaces this one for its own walk.
ALARM_MIN = float(os.environ.get("RULE_CHECK_ALARM_MIN", "120"))
ALARM_MULT = float(os.environ.get("RULE_CHECK_ALARM_MULT", "5"))


class CheckTimeout(BaseException):
    """claude-23 (GEN-STALL): not an Exception.  The walk turns any Exception from a render call into
    a fallback (52 `except Exception` sites in paper_proof_walk/, paper_walk.py, latex_pp/paper/), so
    a TimeoutError raised there is swallowed and the walk carries on with no alarm left armed."""


def _over_alarm(*_a):
    raise CheckTimeout("check over its per-record alarm")


def run_shard(args):
    """One shard in its own process: constants are loaded once per module (typed_flags caches them)."""
    path, part, rules_dir = args
    checks = load_checks(rules_dir)
    counts = {c.RULE: [0, 0, 0.0] for c in checks}  # violations, unmeasured, seconds
    with open(part, "w", encoding="utf-8", buffering=1) as out:  # line-buffered: progress is visible
        for line in open(path, encoding="utf-8"):
            row = json.loads(line)
            try:
                const = typed_flags.constants(row["module"]).get(row["declaration"])
            except Exception as ex:
                print(f"ERROR constants {row['module']}: {str(ex)[:120]}", file=sys.stderr)
                const = None
            for c in checks:
                t0 = time.time()
                signal.signal(signal.SIGALRM, _over_alarm)
                # re-fires every 5 s until cleared: one swallowed alarm does not disarm the deadline
                signal.setitimer(signal.ITIMER_REAL, max(ALARM_MIN, ALARM_MULT * (row.get("proof_seconds") or 0)), 5)
                try:
                    hits = c.check(row, const, typed_flags.constants) or []
                except CheckTimeout:
                    hits = [{"reason": "unmeasured: check over its per-record alarm", "anchor": ""}]
                except Exception as ex:  # report, never guess
                    hits = [{"reason": f"unmeasured: check raised {type(ex).__name__}: {str(ex)[:100]}", "anchor": ""}]
                finally:
                    signal.setitimer(signal.ITIMER_REAL, 0)
                counts[c.RULE][2] += time.time() - t0
                for h in hits:
                    um = h["reason"].startswith("unmeasured")
                    counts[c.RULE][1 if um else 0] += 1
                    out.write(json.dumps({"rule": c.RULE, "module": row["module"],
                                          "declaration": row["declaration"], "reason": h["reason"],
                                          "anchor": h.get("anchor", ""), "unmeasured": um},
                                         ensure_ascii=False, sort_keys=True) + "\n")
    return counts


def main(pattern, out_path, rules_dir):
    """Shards run in parallel, one process each (a census has 6): loading every module's
    constants is the cost, about 45 minutes serially."""
    paths = sorted(glob.glob(pattern))
    jobs = [(p, f"{out_path}.part{i}", rules_dir) for i, p in enumerate(paths)]
    total = {}
    with ProcessPoolExecutor(max_workers=len(jobs) or 1) as pool:
        for counts in pool.map(run_shard, jobs):
            for rule, (v, u, s) in counts.items():
                t = total.setdefault(rule, [0, 0, 0.0])
                t[0] += v; t[1] += u; t[2] += s
    with open(out_path, "w", encoding="utf-8") as out:
        for _p, part, _r in jobs:
            with open(part, encoding="utf-8") as f:
                out.write(f.read())
            os.remove(part)
    for rule, (v, u, s) in total.items():
        print(f"{rule}\tviolations {v}\tunmeasured {u}\tcheck time {s:.0f}s", file=sys.stderr)
    # Which checks ran (ledger.py): a rule with no rows is clean only if its check ran here.
    with open(out_path + ".rules", "w", encoding="utf-8") as f:
        for rule, (v, u, s) in sorted(total.items()):
            f.write(f"{rule}\t{v}\t{u}\t{s:.0f}\n")


if __name__ == "__main__":
    if len(sys.argv) not in (3, 4):
        sys.exit(__doc__)
    main(sys.argv[1], sys.argv[2], sys.argv[3] if len(sys.argv) == 4 else os.path.join(HERE, "rules"))
