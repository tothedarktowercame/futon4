#!/usr/bin/env python3
"""Record an ACCEPTED integration head: class counts, residual ink, progress report.

  accept_head.py SHA 'RENDERS_GLOB'

Refuses a sha that is not on the integration branch, renders that are not stored under
~/storage/apm-endgame/head-SHA/, and a census that is not the full corpus. Candidates are
measured with ink.py into /tmp and compared with ink_diff.py; they are never recorded here.
"""
import csv, glob, os, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
INTEGRATE = os.path.expanduser("~/worktrees/mfuton-share/claude7-integrate")
BRANCH = "claude7/integrate"
RECORDS = 8778


def run(*cmd, **kw):
    return subprocess.run(cmd, check=True, text=True, capture_output=True, **kw).stdout


def ink_rows():
    path = os.path.join(HERE, "ink.tsv")
    if not os.path.exists(path):
        return []
    with open(path, newline="") as f:
        return list(csv.DictReader(f, delimiter="\t"))


def main():
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    sha, pattern = sys.argv[1], os.path.expanduser(sys.argv[2])
    store = os.path.expanduser(f"~/storage/apm-endgame/head-{sha}")
    full = run("git", "-C", INTEGRATE, "rev-parse", sha).strip()
    if not full.startswith(sha):
        sys.exit(f"REFUSED: {sha} is not the abbreviation of a commit")
    if subprocess.run(["git", "-C", INTEGRATE, "merge-base", "--is-ancestor", full, BRANCH]).returncode:
        sys.exit(f"REFUSED: {sha} is not on {BRANCH}: an unaccepted candidate is not recorded")
    files = sorted(glob.glob(pattern))
    if not files or any(os.path.dirname(os.path.realpath(p)) != os.path.realpath(store) for p in files):
        sys.exit(f"REFUSED: renders must be stored under {store}/ first")
    n = sum(1 for p in files for _ in open(p, encoding="utf-8"))
    if n != RECORDS:
        sys.exit(f"REFUSED: {n} records, not the full census of {RECORDS}")

    with open(os.path.join(HERE, "counts.tsv"), newline="") as f:
        counted = {r["commit"] for r in csv.DictReader(f, delimiter="\t")}
    if sha not in counted:
        out = run(sys.executable, os.path.join(HERE, "scan.py"), pattern, sha)
        with open(os.path.join(HERE, "counts.tsv"), "a") as f:
            for line in out.splitlines():
                f.write("\t".join(line.split("\t")[:3]) + "\n")

    summary = os.path.join(store, "residual-ink-summary.tsv")
    run(sys.executable, os.path.join(HERE, "ink.py"), pattern, "--summary", summary,
        "--annotations", os.path.join(store, "residual-ink-annotations.jsonl.gz"),
        "--html", os.path.join(store, "residual-ink.html"))
    with open(summary, newline="") as f:
        rows = list(csv.DictReader(f, delimiter="\t"))
    old = ink_rows()
    kept = [r for r in old if r["head"] != sha]
    prev_head = kept[-1]["head"] if kept else None
    prev = {r["class"]: r for r in kept if r["head"] == prev_head}
    path = os.path.join(HERE, "ink.tsv")
    with open(path, "w") as f:
        f.write("head\tclass\trecords\tmarked_chars\tpercent_total\n")
        for r in kept:
            f.write("\t".join(r[k] for k in ("head", "class", "records", "marked_chars", "percent_total")) + "\n")
        for r in rows:
            f.write("\t".join([sha, r["class"], r["records"], r["marked_chars"], r["percent_total"]]) + "\n")

    with open(os.path.join(HERE, "PROGRESS.md"), "w") as f:
        f.write(run(sys.executable, os.path.join(HERE, "progress.py")))
    print(f"accepted head {sha}: counts, ink and PROGRESS.md recorded; previous head {prev_head}")
    for r in rows:
        if r["class"] in ("TOTAL", "COVERAGE"):
            continue
        p = prev.get(r["class"])
        if p is None:
            note = "new extractor"
        else:
            note = f"{float(r['percent_total']) - float(p['percent_total']):+.6f} points, {int(r['records']) - int(p['records']):+d} records"
        if p is None or r["percent_total"] != p["percent_total"] or r["records"] != p["records"]:
            print(f"  {r['class']}: {r['percent_total']}% in {r['records']} records ({note})")
    print(f"review surface: {store}/residual-ink.html ; python3 ink_diff.py {prev_head} {sha}")


if __name__ == "__main__":
    main()
