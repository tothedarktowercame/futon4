#!/usr/bin/env python3
"""Export residual-ink annotations of an accepted head as proof-ink overlay records (ink-overlay.schema.json).

  ink_overlay.py HEAD [--out FILE.jsonl.gz]     # reads ~/storage/apm-endgame/head-HEAD/residual-ink-annotations.jsonl.gz
  ink_overlay.py --validate FILE                # checks every record against the schema's rules
  ink_overlay.py --selftest

The validator is written out by hand (no jsonschema on this machine); it enforces the same required
fields, enums and coordinate rules as the schema file.
"""
import csv, gzip, json, os, subprocess, sys

HERE = os.path.dirname(os.path.abspath(__file__))
SCHEMA = json.load(open(os.path.join(HERE, "ink-overlay.schema.json")))
P = SCHEMA["properties"]
TEXT_SPACES = {"render-visible", "render-raw", "lean-source", "human-source"}


def check(rec):
    errs = []

    def obj(value, spec, where):
        if not isinstance(value, dict):
            errs.append(f"{where}: not an object")
            return
        for key in spec.get("required", []):
            if key not in value:
                errs.append(f"{where}: missing {key}")
        for key, v in value.items():
            sub = spec["properties"].get(key)
            if sub is None:
                errs.append(f"{where}: unknown field {key}")
            elif "enum" in sub and v not in sub["enum"]:
                errs.append(f"{where}.{key}: {v!r} not allowed")
            elif "const" in sub and v != sub["const"]:
                errs.append(f"{where}.{key}: must be {sub['const']!r}")
            elif sub.get("type") == "array" and "enum" in sub.get("items", {}):
                errs.extend(f"{where}.{key}: {x!r} not allowed" for x in v if x not in sub["items"]["enum"])

    obj(rec, SCHEMA, "record")
    for key in ("record", "coordinate", "axes", "provenance", "review"):
        if isinstance(rec.get(key), dict):
            obj(rec[key], P[key], key)
    c = rec.get("coordinate", {})
    if c.get("space") in TEXT_SPACES:
        for key in ("start", "end", "text_sha256"):
            if key not in c:
                errs.append(f"coordinate: a text span needs {key}")
        if "start" in c and "end" in c and not 0 <= c["start"] <= c["end"]:
            errs.append("coordinate: start/end out of order")
    if c.get("space") == "typed-node" and not ("node" in c and "node_stability" in c):
        errs.append("coordinate: a typed node needs node and node_stability")
    if isinstance(rec.get("axes"), dict) and not rec["axes"]:
        errs.append("axes: at least one axis")
    return errs


def export(head, out):
    path = os.path.expanduser(f"~/storage/apm-endgame/head-{head}/residual-ink-annotations.jsonl.gz")
    with open(os.path.join(HERE, "ink-sites.tsv"), newline="") as f:
        sites = {(r["declaration"], r["class"]) for r in csv.DictReader(f, delimiter="\t")}
    commit = subprocess.run(["git", "-C", HERE, "log", "-1", "--format=%h", "--", "ink.py"],
                            capture_output=True, text=True).stdout.strip()
    n = 0
    with gzip.open(path, "rt", encoding="utf-8") as src, gzip.open(out, "wt", encoding="utf-8") as dst:
        for line in src:
            r = json.loads(line)
            for s in r["spans"]:
                rec = {
                    "schema": "proof-ink.overlay/v0",
                    "record": {"corpus": "apm", "module": r["module"], "declaration": r["declaration"]},
                    "coordinate": {"space": "render-visible", "start": s["start"], "end": s["end"],
                                   "text_sha256": r["visible_sha256"], "anchor": s.get("anchor", "")},
                    "axes": {"residual_class": s["class"]},
                    "evidence": ["ink.py extractor for " + s["class"]],
                    "provenance": {"producer": "futon4 endgame ink.py", "producer_commit": commit, "head": head,
                                   "how": "known-site" if (r["declaration"], s["class"]) in sites else "text-pattern"},
                    "review": {"status": "unreviewed"},
                }
                errs = check(rec)
                if errs:
                    sys.exit(f"export produced an invalid record: {errs}")
                dst.write(json.dumps(rec, sort_keys=True, ensure_ascii=False) + "\n")
                n += 1
    print(f"{n} overlay records for head {head} -> {out}")


def validate(path):
    opener = gzip.open if path.endswith(".gz") else open
    bad = n = 0
    with opener(path, "rt", encoding="utf-8") as f:
        for i, line in enumerate(f, 1):
            if not line.strip():
                continue
            n += 1
            errs = check(json.loads(line))
            if errs:
                bad += 1
                print(f"line {i}: " + "; ".join(errs))
    print(f"{n} records, {bad} invalid")
    return 1 if bad else 0


def selftest():
    good = {"schema": "proof-ink.overlay/v0", "record": {"corpus": "apm", "module": "m", "declaration": "d"},
            "coordinate": {"space": "render-visible", "start": 0, "end": 5, "text_sha256": "0" * 64},
            "axes": {"residual_class": "C23"}, "provenance": {"producer": "x", "head": "h"},
            "review": {"status": "unreviewed"}}
    assert check(good) == []
    assert check(dict(good, axes={})), "empty axes must fail"
    assert check(dict(good, axes={"residual_class": "C23", "analytic_layer": ["K"]})), "unknown layer must fail"
    assert check(dict(good, coordinate={"space": "render-visible", "start": 0, "end": 5})), "span without hash must fail"
    assert check(dict(good, coordinate={"space": "typed-node", "node": "n"})), "node without stability must fail"
    assert check(dict(good, review={"status": "correct"})), "a correctness status must fail"
    node = dict(good, coordinate={"space": "typed-node", "node": "d:h2", "node_stability": "binder-name"},
                axes={"residual_class": "C23", "analytic_layer": ["S3"], "presentation_role": "assumption",
                      "plan_action": "nest"})
    assert check(node) == []
    print("selftest ok: axes are independent; spans need a text hash; no correctness status exists")


if __name__ == "__main__":
    a = sys.argv[1:]
    if a == ["--selftest"]:
        selftest()
    elif len(a) == 2 and a[0] == "--validate":
        sys.exit(validate(a[1]))
    elif a:
        out = a[a.index("--out") + 1] if "--out" in a else os.path.expanduser(
            f"~/storage/apm-endgame/head-{a[0]}/residual-ink-overlay.jsonl.gz")
        export(a[0], out)
    else:
        sys.exit(__doc__)
