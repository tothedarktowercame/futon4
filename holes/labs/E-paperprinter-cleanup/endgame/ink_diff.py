#!/usr/bin/env python3
"""Per-record ink comparison between two annotation files (accepted head vs head, or head vs candidate).

  ink_diff.py BASE NEW        # each a head sha (stored annotations) or a path to *.jsonl.gz

Lists, per record and class: NEW color (class absent from the record before), GROWN (more marked
paragraphs), MOVED (same number of paragraphs, different extent), SHRUNK and GONE. Exit status 1
when any record has a NEW or GROWN color: a lower total does not excuse those; each must be read
and explained (or the candidate rejected) before a merge.
"""
import collections, gzip, json, os, sys


def load(arg):
    path = arg if os.path.exists(arg) else os.path.expanduser(
        f"~/storage/apm-endgame/head-{arg}/residual-ink-annotations.jsonl.gz")
    out = {}
    with gzip.open(path, "rt", encoding="utf-8") as f:
        for line in f:
            r = json.loads(line)
            per = collections.defaultdict(lambda: [0, 0])
            for s in r["spans"]:
                per[s["class"]][0] += 1
                per[s["class"]][1] += s["end"] - s["start"]
            out[(r["module"], r["declaration"])] = {k: tuple(v) for k, v in per.items()}
    return out


def compare(base, new):
    rows = []
    for key in sorted(set(base) | set(new)):
        b, n = base.get(key, {}), new.get(key, {})
        for cls in sorted(set(b) | set(n)):
            (bp, bc), (np_, nc) = b.get(cls, (0, 0)), n.get(cls, (0, 0))
            if (bp, bc) == (np_, nc):
                continue
            kind = ("NEW" if bp == 0 else "GONE" if np_ == 0 else "GROWN" if np_ > bp
                    else "SHRUNK" if np_ < bp else "MOVED")
            rows.append((kind, cls, key[1], bp, np_, bc, nc))
    return rows


def selftest():
    base = {("m", "a"): {"N1": (2, 100)}, ("m", "b"): {"C4": (1, 50)}, ("m", "c"): {"C4": (1, 40)}}
    new = {("m", "a"): {"N1": (1, 60)}, ("m", "b"): {"C4": (1, 50), "C22": (1, 30)}, ("m", "c"): {"C4": (2, 90)}}
    kinds = {(k, c, d) for k, c, d, *_ in compare(base, new)}
    assert kinds == {("SHRUNK", "N1", "a"), ("NEW", "C22", "b"), ("GROWN", "C4", "c")}, kinds
    assert compare(base, base) == []
    print("selftest ok: a new color and a grown color are reported although total ink fell")


def main():
    if sys.argv[1:] == ["--selftest"]:
        return selftest()
    if len(sys.argv) != 3:
        sys.exit(__doc__)
    rows = compare(load(sys.argv[1]), load(sys.argv[2]))
    order = {"NEW": 0, "GROWN": 1, "MOVED": 2, "SHRUNK": 3, "GONE": 4}
    rows.sort(key=lambda r: (order[r[0]], r[1], r[2]))
    print("kind\tclass\tdeclaration\tparagraphs_before\tparagraphs_after\tchars_before\tchars_after")
    for r in rows:
        print("\t".join(map(str, r)))
    tally = collections.Counter((r[0], r[1]) for r in rows)
    print("# " + (", ".join(f"{k} {c}: {v}" for (k, c), v in sorted(tally.items(), key=lambda x: (order[x[0][0]], x[0][1]))) or "no per-record change"),
          file=sys.stderr)
    return 1 if any(r[0] in ("NEW", "GROWN") for r in rows) else 0


if __name__ == "__main__":
    sys.exit(main())
