#!/usr/bin/env python3
"""Annotate scanner-visible residual paragraphs and measure their visible-source ink.

This is deliberately a lower bound: it covers the nine open classes with a
corpus-wide scanner (C1, C4, C5, C6, C7, C8, C16, C17, N1).  Classes currently
known only by declaration lists need interval extractors before they enter the
percentage.  "Ink" is characters in reader-visible LaTeX source after hidden
leanref targets are removed, not rasterized glyph area.
"""
import argparse
import collections
import csv
import glob
import gzip
import html
import json
import os
import re
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
from leanism_scan_p1 import hits, visible
from scan import align_rows

PALETTE = {
    "C1": "#d73027", "C4": "#fc8d59", "C5": "#fee090", "C6": "#e0f3f8",
    "C7": "#91bfdb", "C8": "#4575b4", "C16": "#984ea3", "C17": "#4daf4a",
    "N1": "#ff7f00", "C21": "#a65628", "C22": "#f781bf",
}
FAMILY = {
    "C1": re.compile(r"distinguish cases on[^.]*\\lor"),
    "C5": re.compile(r"\\bot"),
    "C6": re.compile(r"\\top"),
    "C8": re.compile(r"\\mapsto\s*[A-Za-z0-9_'{}]+\s*\\in\b"),
    "C16": re.compile(r"by transitivity"),
    "C17": re.compile(r"\\seqsplit"),
    "N1": re.compile(r"&& \\text\{\}"),
    "C21": re.compile(r"a library lemma"),
    "C22": re.compile(r"(?<![_\d.{])(?<!\\cdot )(?<![-+] )0 (?:\\, )?> (?:\\, )?0(?![\d.])"),
}
LOCAL = re.compile(r"^[A-Za-z]_[A-Za-z]'?$|^self$")
ALIGN = re.compile(r"\\begin\{align\*\}(.*?)\\end\{align\*\}", re.S)


def paragraph(text, start, end):
    left = text.rfind("\n\n", 0, start)
    right = text.find("\n\n", end)
    return (0 if left < 0 else left + 2, len(text) if right < 0 else right)


def merge(intervals):
    out = []
    for start, end in sorted(intervals):
        if out and start <= out[-1][1]:
            out[-1] = (out[-1][0], max(out[-1][1], end))
        else:
            out.append((start, end))
    return out


def class_intervals(text):
    found = collections.defaultdict(list)
    for cls, rx in FAMILY.items():
        for hit in rx.finditer(text):
            found[cls].append(paragraph(text, hit.start(), hit.end()))
    for word in hits(text):
        if LOCAL.match(word):
            continue
        for hit in re.finditer(re.escape(word), text):
            found["C7"].append(paragraph(text, hit.start(), hit.end()))
    for block in ALIGN.finditer(text):
        rows = [r.strip() for r in align_rows(block.group(1)) if r.strip()]
        lhs = [r.split("&&", 1)[0].strip() for r in rows]
        if len(lhs) != len(set(lhs)):
            found["C4"].append(paragraph(text, block.start(), block.end()))
    return {cls: merge(spans) for cls, spans in found.items()}


def open_out(path):
    return gzip.open(path, "wt", encoding="utf-8") if path.endswith(".gz") else open(path, "w", encoding="utf-8")


def colored_html(text, spans):
    events = collections.defaultdict(lambda: {"start": [], "end": []})
    for item in spans:
        events[item["start"]]["start"].append(item["class"])
        events[item["end"]]["end"].append(item["class"])
    points = sorted({0, len(text), *events})
    active, out = set(), []
    for index, point in enumerate(points[:-1]):
        for cls in events[point]["end"]:
            active.discard(cls)
        active.update(events[point]["start"])
        nxt = points[index + 1]
        piece = html.escape(text[point:nxt])
        if active:
            classes = ",".join(sorted(active))
            colors = [PALETTE[c] for c in sorted(active)]
            background = colors[0] if len(colors) == 1 else f"linear-gradient(90deg,{','.join(colors)})"
            out.append(f'<mark title="{classes}" style="background:{background}">{piece}</mark>')
        else:
            out.append(piece)
    return "".join(out)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("pattern")
    ap.add_argument("--annotations")
    ap.add_argument("--summary")
    ap.add_argument("--html")
    args = ap.parse_args()
    total = records = marked_records = 0
    ink = collections.Counter()
    class_records = collections.Counter()
    union_ink = 0
    annotations = open_out(args.annotations) if args.annotations else None
    html_out = open(args.html, "w", encoding="utf-8") if args.html else None
    if html_out:
        html_out.write("<!doctype html><meta charset='utf-8'><title>PaperPrinter residual ink</title>"
                       "<style>body{font-family:sans-serif}pre{white-space:pre-wrap;border:1px solid #ccc;padding:1em}"
                       "mark{color:inherit}summary{font-family:monospace}</style><h1>PaperPrinter residual ink</h1>")
        html_out.write("<p>Lower-bound markup for the scanner-backed classes. "
                       "Hover colored spans for class identifiers.</p>")
    try:
        for path in sorted(glob.glob(args.pattern)):
            with open(path, encoding="utf-8") as stream:
                for line in stream:
                    row = json.loads(line)
                    proof = visible(row.get("proof") or "")
                    records += 1
                    total += len(proof)
                    spans = class_intervals(proof)
                    if not spans:
                        continue
                    marked_records += 1
                    all_spans = []
                    serial = []
                    for cls, intervals in sorted(spans.items()):
                        class_records[cls] += 1
                        for start, end in intervals:
                            ink[cls] += end - start
                            all_spans.append((start, end))
                            serial.append({"start": start, "end": end, "class": cls,
                                           "color": PALETTE[cls]})
                    union_ink += sum(end - start for start, end in merge(all_spans))
                    if annotations:
                        annotations.write(json.dumps({"module": row.get("module"),
                                                      "declaration": row.get("declaration"),
                                                      "visible_chars": len(proof),
                                                      "spans": serial}, sort_keys=True) + "\n")
                    if html_out:
                        title = html.escape(f"{row.get('module')} :: {row.get('declaration')}")
                        html_out.write(f"<details><summary>{title}</summary><pre>{colored_html(proof, serial)}</pre></details>")
    finally:
        if annotations:
            annotations.close()
        if html_out:
            html_out.close()
    with open(os.path.join(HERE, "classes.tsv"), newline="") as stream:
        open_classes = [r["id"] for r in csv.DictReader(stream, delimiter="\t")
                        if r["state"] in ("open", "unknown")]
    covered = sum(1 for cls in open_classes if cls in PALETTE)
    lines = ["class\tcolor\trecords\tmarked_chars\tpercent_total"]
    for cls in sorted(PALETTE, key=lambda x: (x[0], int(x[1:]))):
        lines.append(f"{cls}\t{PALETTE[cls]}\t{class_records[cls]}\t{ink[cls]}\t{(100*ink[cls]/total if total else 0):.6f}")
    lines.extend([
        f"UNION\t#000000\t{marked_records}\t{union_ink}\t{(100*union_ink/total if total else 0):.6f}",
        f"TOTAL\t-\t{records}\t{total}\t100.000000",
        f"COVERAGE\t-\t-\t{covered}/{len(open_classes)} open-or-unknown classes\tlower-bound",
    ])
    output = "\n".join(lines) + "\n"
    if args.summary:
        with open(args.summary, "w", encoding="utf-8") as stream:
            stream.write(output)
    print(output, end="")
    return 0


if __name__ == "__main__":
    sys.exit(main())
