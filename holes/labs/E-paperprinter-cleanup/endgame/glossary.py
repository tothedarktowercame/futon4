#!/usr/bin/env python3
"""Export PaperPrinter's formal-to-reader glossary and measure contested ink.

The render supplies observations, not verdicts: several visible forms for one
Lean target may just be different arguments.  A target is contested only when
``glossary-claims.tsv`` contains incompatible live claims for it, or a claim
whose status is explicitly ``contested``.
"""
import argparse
import collections
import csv
import glob
import html
import json
import os

HERE = os.path.dirname(os.path.abspath(__file__))


def leanrefs(text):
    """Yield (visible, target, source_start, source_end) for balanced leanrefs."""
    needle = r"\leanref{"
    pos = 0
    while True:
        start = text.find(needle, pos)
        if start < 0:
            return
        fields = []
        cursor = start + len(needle)
        for field in range(2):
            depth, begin = 1, cursor
            while cursor < len(text) and depth:
                if text[cursor] == "{" and (cursor == 0 or text[cursor - 1] != "\\"):
                    depth += 1
                elif text[cursor] == "}" and (cursor == 0 or text[cursor - 1] != "\\"):
                    depth -= 1
                cursor += 1
            if depth:
                break
            fields.append(text[begin:cursor - 1])
            if field == 0:
                if cursor >= len(text) or text[cursor] != "{":
                    break
                cursor += 1
        if len(fields) == 2:
            yield fields[0], fields[1], start, cursor
            pos = cursor
        else:
            pos = start + len(needle)


def read_claims(path):
    claims = collections.defaultdict(list)
    with open(path, encoding="utf-8", newline="") as stream:
        for row in csv.DictReader(stream, delimiter="\t"):
            if row.get("formal_name"):
                claims[row["formal_name"]].append(row)
    return claims


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("pattern", help="glob for accepted-head JSONL renders")
    ap.add_argument("--readings", required=True, help="citation-readings.json")
    ap.add_argument("--claims", default=os.path.join(HERE, "glossary-claims.tsv"))
    ap.add_argument("--tsv", required=True)
    ap.add_argument("--html")
    ap.add_argument("--summary", required=True)
    args = ap.parse_args()

    curated = json.load(open(args.readings, encoding="utf-8"))
    claims = read_claims(args.claims)
    forms = collections.defaultdict(collections.Counter)
    examples = collections.defaultdict(list)
    target_chars = collections.Counter()
    total_visible_chars = 0
    records = 0
    for path in sorted(glob.glob(args.pattern)):
        with open(path, encoding="utf-8") as stream:
            for line in stream:
                row = json.loads(line)
                proof = row.get("proof") or ""
                records += 1
                refs = list(leanrefs(proof))
                total_visible_chars += len(proof) - sum((end - start) - len(visible)
                                                        for visible, _, start, end in refs)
                for visible, target, _, _ in refs:
                    if target.startswith("local:"):
                        continue
                    forms[target][visible] += 1
                    target_chars[target] += len(visible)
                    if len(examples[target]) < 2:
                        examples[target].append(f"{row.get('module','')}::{row.get('declaration','')}")

    names = sorted(set(forms) | set(curated) | set(claims))
    rows, contested_names = [], set()
    for name in names:
        live = [r for r in claims[name] if r.get("status") in ("accepted", "contested")]
        readings = {r.get("reading", "") for r in live if r.get("reading")}
        explicitly_contested = any(r.get("status") == "contested" for r in live)
        conflicted = explicitly_contested or len(readings) > 1
        if conflicted:
            contested_names.add(name)
        rows.append({
            "formal_name": name,
            "curated_reading": curated.get(name, ""),
            "claim_state": "contested" if conflicted else ("accepted" if live or name in curated else "unreviewed"),
            "claims": " | ".join(f"{r.get('status')}:{r.get('reading')}" for r in claims[name]),
            "surface_forms": len(forms[name]),
            "occurrences": sum(forms[name].values()),
            "visible_chars": target_chars[name],
            "examples": " | ".join(examples[name]),
        })

    fields = list(rows[0]) if rows else ["formal_name"]
    with open(args.tsv, "w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=fields, delimiter="\t")
        writer.writeheader(); writer.writerows(rows)

    contested_chars = sum(target_chars[n] for n in contested_names)
    summary = {
        "records": records, "formal_targets": len(names), "curated_readings": len(curated),
        "targets_with_multiple_surface_forms": sum(len(v) > 1 for v in forms.values()),
        "contested_targets": len(contested_names), "contested_visible_chars": contested_chars,
        "total_visible_chars": total_visible_chars,
        "contested_percent_total": 100 * contested_chars / total_visible_chars if total_visible_chars else 0,
    }
    with open(args.summary, "w", encoding="utf-8") as stream:
        json.dump(summary, stream, indent=2, sort_keys=True); stream.write("\n")

    if args.html:
        with open(args.html, "w", encoding="utf-8") as stream:
            stream.write("<!doctype html><meta charset='utf-8'><title>PaperPrinter glossary</title>")
            stream.write("<h1>PaperPrinter glossary</h1><p>Surface diversity is evidence, not automatically conflict.</p><table border=1><tr>")
            stream.write("".join(f"<th>{html.escape(f)}</th>" for f in fields) + "</tr>")
            for row in rows:
                style = " style='background:#ffd6d6'" if row["claim_state"] == "contested" else ""
                stream.write(f"<tr{style}>" + "".join(f"<td>{html.escape(str(row[f]))}</td>" for f in fields) + "</tr>")
            stream.write("</table>")


if __name__ == "__main__":
    main()
