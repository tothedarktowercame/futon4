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
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from leanism_scan_p1 import visible


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
                claims[(row["formal_name"], row.get("role") or "unspecified")].append(row)
    return claims


def read_curated(specs):
    """Read ROLE=PATH JSON maps (plain PATH remains the citation role)."""
    out = collections.defaultdict(list)
    for spec in specs:
        role, path = spec.split("=", 1) if "=" in spec else ("citation", spec)
        data = json.load(open(path, encoding="utf-8"))
        if isinstance(data, dict):
            for name, reading in data.items():
                if isinstance(reading, str):
                    out[(name, role)].append((reading, os.path.basename(path)))
                elif isinstance(reading, dict):
                    for subrole, text in reading.items():
                        if isinstance(text, str):
                            out[(name, f"{role}:{subrole}")].append((text, os.path.basename(path)))
        elif isinstance(data, list):
            for rule in data:
                if isinstance(rule, dict) and rule.get("const"):
                    out[(rule["const"], role)].append((json.dumps(rule, sort_keys=True), os.path.basename(path)))
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("pattern", help="glob for accepted-head JSONL renders")
    ap.add_argument("--readings", action="append", required=True,
                    help="repeatable ROLE=PATH JSON reading source; plain PATH means citation")
    ap.add_argument("--claims", default=os.path.join(HERE, "glossary-claims.tsv"))
    ap.add_argument("--tsv", required=True)
    ap.add_argument("--html")
    ap.add_argument("--summary", required=True)
    args = ap.parse_args()

    curated = read_curated(args.readings)
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
                total_visible_chars += len(visible(proof))
                for shown, target, _, _ in refs:
                    if target.startswith("local:"):
                        continue
                    forms[target][shown] += 1
                    target_chars[target] += len(shown)
                    if len(examples[target]) < 2:
                        examples[target].append(f"{row.get('module','')}::{row.get('declaration','')}")

    names = sorted(set(forms) | {name for name, _ in curated} | {name for name, _ in claims})
    rows, contested_names = [], set()
    for name in names:
        roles = sorted({role for n, role in curated if n == name} | {role for n, role in claims if n == name})
        role_conflicts = []
        all_claims = []
        has_live = False
        for role in roles:
            rs = claims[(name, role)]
            live = [r for r in rs if r.get("status") in ("accepted", "contested")]
            has_live = has_live or bool(live)
            readings = {r.get("reading", "") for r in live if r.get("reading")}
            if any(r.get("status") == "contested" for r in live) or len(readings) > 1:
                role_conflicts.append(role)
            all_claims.extend(f"{role}:{r.get('status')}:{r.get('reading')}" for r in rs)
        conflicted = bool(role_conflicts)
        if conflicted:
            contested_names.add(name)
        rows.append({
            "formal_name": name,
            "curated_reading": " | ".join(f"{role}:{text}" for (n, role), vals in curated.items()
                                             if n == name for text, _ in vals),
            "reading_sources": " | ".join(f"{role}:{source}" for (n, role), vals in curated.items()
                                            if n == name for _, source in vals),
            "claim_state": "contested" if conflicted else (
                "accepted" if has_live or any(n == name for n, _ in curated) else "unreviewed"),
            "contested_roles": " | ".join(role_conflicts),
            "claims": " | ".join(all_claims),
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
        "records": records, "formal_targets": len(names),
        "curated_readings": sum(len(v) for v in curated.values()),
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
