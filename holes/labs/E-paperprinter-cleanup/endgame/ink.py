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
import hashlib
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
    "C7": "#91bfdb", "C8": "#4575b4", "C16": "#984ea3", "C26": "#bc80bd", "C17": "#4daf4a",
    "N1": "#ff7f00", "C21": "#a65628", "C22": "#f781bf", "C23": "#999999",
    "C18": "#66c2a5", "C19": "#8da0cb", "C12": "#e78ac3", "N14": "#a6d854",
    "P-sufficiency-wall": "#ffd92f", "P-empty-claim": "#e5c494", "P-duplicate-closer": "#b3b3b3",
    "P-nonprop-suffices": "#1b9e77", "P-missing-subproof": "#d95f02", "P-premature-conclusion": "#7570b3",
    "P-opaque-proof": "#e7298a", "P-duplicate-tag": "#66a61e", "P-overexpanded-setup": "#e6ab02",
    "P-claim-binders-unintroduced": "#a6761d", "R3": "#666666",
}
FAMILY = {
    "C1": re.compile(r"distinguish cases on[^.]*\\lor"),
    "C5": re.compile(r"\\bot"),
    "C6": re.compile(r"\\top"),
    "C8": re.compile(r"\\mapsto\s*[A-Za-z0-9_'{}]+\s*\\in\b"),
    # C16 is the bare closer; a sentence that names the middle term ("through \(m\)") is not a defect;
    # the form that cites facts without the middle term is its own class (scanner change 2026-11-24, INK.md)
    "C16": re.compile(r"by transitivity(?! through)(?!, using)"),
    "C26": re.compile(r"by transitivity, using"),
    "C17": re.compile(r"\\seqsplit"),
    "N1": re.compile(r"&& \\text\{\}"),
    "C21": re.compile(r"a library lemma"),
    "C18": re.compile(r"\\text\{if \}\s*(?:-\s*)?\d+\s*(?:=|\\neq)\s*(?:-\s*)?\d+(?![\d.])"),
    "C19": re.compile(r"-\s+-\s*[A-Za-z\\]"),
    # a claim with nothing under it: the colon ends the line and the next line is not its proof
    "P-empty-claim": re.compile(r"We claim \\\((?:(?!\\\)).)*\\\):(?: ⟦\d+⟧)?[ ]*(?=\n\\item|\n\\end|\n\n|$)"),
    # an owner sentence whose only child is a closer giving the same reason
    "P-duplicate-closer": re.compile(r"— ([^\n]*?)\.(?: ⟦\d+⟧)?\n\\begin\{itemize\}\n\\item Hence the claim — \1\.\n\\end\{itemize\}"),
    # a "1 = 0" given with its reason ("— by ...") that the next item of the same list turns into the
    # contradiction is in place, not detached (scanner change 2026-11-25, Q468: m02A01, a93A02, m99A05,
    # b98J04).  "— this is (n)" (a restated assumption, Q467) and a bare "We have 1 = 0." stay marked.
    "C23": re.compile(r"We have \\\(1 = 0\\\)(?! — by [^\n]*\n\\item (?:This is absurd, by|This contradicts|[^\n]*the hypotheses being contradictory))"),
    # a claim over \forall whose sub-proof starts without introducing the quantified variables and
    # hypotheses (have value `fun`/`intro` walked as argument plumbing, letE under app_depth > 0); R1,
    # claude-19 2026-11-26: 138 lines / 107 records at q298, 12/12 sampled defects.  A "Let" that
    # introduces only some binders is not caught (lower bound).
    "P-claim-binders-unintroduced": re.compile(r"We claim (?:\\leanref\{\}\{local:[^}]*\})?\\\(\\forall[^\n]*\n\\begin\{itemize\}\n\\item (?!(?:Let|Suppose|Fix|Assume)\b)"),
    "C22": re.compile(r"(?<![_\d.{])(?<!\\cdot )(?<![-+] )0 (?:\\, )?> (?:\\, )?0(?![\d.])"),
}
# classes measured by another class's extractor, and classes that are not about rendered text
ALIAS = {"N2": "C4", "P-empty-chain-reason": "N1", "P-truncated-walk": "P-missing-subproof"}
NO_INK = {"N15"}
WALL = 4  # consecutive "It suffices" sentences that count as a wall
UNIT = re.compile(r"\n+|(?<=\))\. (?=[A-Z])")
ROW_REASON = re.compile(r"\s*&& \\text\{(.*)\}( \\\\)?$")


def load_sites():
    sites = collections.defaultdict(set)
    with open(os.path.join(HERE, "ink-sites.tsv"), newline="") as stream:
        for row in csv.DictReader(stream, delimiter="\t"):
            sites[row["declaration"]].add(row["class"])
    return sites


SITES = load_sites()

# Typed classes: the census text cannot decide them, so typed_flags.py writes
# typed-flags.jsonl beside the shards (one row per flagged declaration) and ink.py adds the
# text half.  R3 (claude-22, 2026-11-26): `by_contra h` proof whose page closes "the hypotheses
# being contradictory" with no supposition sentence.  Without a flags file the class is
# reported as not measured, never as zero.
TYPED = {}
R3_CLOSER = re.compile(r"the hypotheses being contradictory")
R3_STATED = re.compile(r"for contradiction|Suppose \\\(\\mathop\{\\neg\}|Suppose[^\n]*\\(notin|neq)")


def load_typed(pattern):
    dirs = {os.path.dirname(os.path.realpath(p)) for p in glob.glob(pattern)}
    flags = {}
    for d in dirs:
        path = os.path.join(d, "typed-flags.jsonl")
        if os.path.exists(path):
            for line in open(path, encoding="utf-8"):
                r = json.loads(line)
                flags.setdefault(r["declaration"], set()).add(r["class"])
    return flags if flags or any(os.path.exists(os.path.join(d, "typed-flags.jsonl")) for d in dirs) else None


def wall_spans(text):
    """Runs of at least WALL consecutive sentences that each begin "It suffices"."""
    spans, run, pos = [], [], 0
    bounds = [(m.start(), m.end()) for m in UNIT.finditer(text)] + [(len(text), len(text))]
    for start, end in bounds:
        unit = text[pos:start].strip()
        if unit.startswith("\\item "):
            unit = unit[6:]
        if unit.startswith("It suffices"):
            run.append((pos, start))
        elif unit not in ("", "\\begin{itemize}", "\\end{itemize}"):
            if len(run) >= WALL:
                spans.append((run[0][0], run[-1][1]))
            run = []
        pos = end
    if len(run) >= WALL:
        spans.append((run[0][0], run[-1][1]))
    return spans


def repeated_row(block):
    """True when a displayed chain prints the same expression on two rows (row-level review metric)."""
    seen = set()
    for line in block.strip().split("\n"):
        mm = ROW_REASON.search(line)
        body = line[:mm.start()] if mm else line
        body = re.sub(r"^&(=|\\\w+|[<>])?\s*", "", body.replace(" \\\\", "")).strip()
        if body in seen:
            return True
        seen.add(body)
    return False


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


IMPOSSIBLE_CASE = re.compile(r"^\\item Case \d+: .*, which is impossible\.$")


def c22_in_impossible_case(text, pos):
    """A "0 > 0" that is the hypothesis of a case the page itself rules out is not C22
    (scanner change 2026-11-26, Q516: t99A02 treeContraction_resp unfolds a disjunction
    whose second case carries 0 < t at t = 0; "Case 2: ... and 0 > 0, which is impossible.").
    Exempt: the hit's own line is such a case line, or it is the "We distinguish" line
    that lists the cases and a later line of the same paragraph is such a case line
    carrying a C22 match.  A "0 > 0" stated as a row, goal or fact stays marked."""
    left = text.rfind("\n", 0, pos) + 1
    right = text.find("\n", pos)
    line = text[left:right if right >= 0 else len(text)]
    if IMPOSSIBLE_CASE.match(line):
        return True
    if line.startswith("\\item We distinguish "):
        _start, end = paragraph(text, pos, pos)
        rest = text[(right if right >= 0 else len(text)):end].split("\n")
        return any(IMPOSSIBLE_CASE.match(l) and FAMILY["C22"].search(l) for l in rest)
    return False


C23_PROVED_BY_DISPLAY = re.compile(
    r"We have \\\(1 = 0\\\)\.\n\\begin\{align\*\}\n(?P<rows>.*?)\n\\end\{align\*\} ⟦(?P<tag>\d+)⟧\n+"
    r"(?:\\item )?This is absurd, by \((?P=tag)\)\.", re.S)


def c23_proved_by_display(text, pos):
    """A "1 = 0" whose proof is the computation displayed directly under it is not C23
    (scanner change 2026-11-26, Q525B: t00A04 aux_point_nonzero prints "We have 1 = 0."
    then 1 = e(e^{-1}(1)) = e(0) = 0 with a reason on every step, tagged (6), then
    "This is absurd, by (6).").  Exempt only when every row after the first carries a
    reason and the absurdity cites the display's own tag.  A bare or partly justified
    "1 = 0" stays marked."""
    m = C23_PROVED_BY_DISPLAY.match(text, pos)
    if not m:
        return False
    rows = [r.strip() for r in align_rows(m.group("rows")) if r.strip()]
    return len(rows) >= 2 and all("&& \\text{" in r for r in rows[1:])


def class_intervals(text, declaration=None):
    found = collections.defaultdict(list)
    for cls in SITES.get(declaration, ()):
        if text:
            found[cls].append((0, len(text)))  # known site only: the whole proof is colored
    if TYPED and "R3" in TYPED.get(declaration, ()) and not R3_STATED.search(text):
        for hit in R3_CLOSER.finditer(text):
            found["R3"].append(paragraph(text, hit.start(), hit.end()))
    for start, end in wall_spans(text):
        found["P-sufficiency-wall"].append((start, end))
    for cls, rx in FAMILY.items():
        for hit in rx.finditer(text):
            if cls == "C22" and c22_in_impossible_case(text, hit.start()):
                continue
            if cls == "C23" and c23_proved_by_display(text, hit.start()):
                continue
            found[cls].append(paragraph(text, hit.start(), hit.end()))
    for word in hits(text):
        if LOCAL.match(word):
            continue
        for hit in re.finditer(re.escape(word), text):
            found["C7"].append(paragraph(text, hit.start(), hit.end()))
    for block in ALIGN.finditer(text):
        rows = [r.strip() for r in align_rows(block.group(1)) if r.strip()]
        lhs = [r.split("&&", 1)[0].strip() for r in rows]
        if len(lhs) != len(set(lhs)) or repeated_row(block.group(1)):
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
    global TYPED
    TYPED = load_typed(args.pattern)
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
                    spans = class_intervals(proof, row.get("declaration"))
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
                                           "anchor": proof[start:start + 60],
                                           "color": PALETTE[cls]})
                    union_ink += sum(end - start for start, end in merge(all_spans))
                    if annotations:
                        annotations.write(json.dumps({"module": row.get("module"),
                                                      "declaration": row.get("declaration"),
                                                      "visible_chars": len(proof),
                                                      "visible_sha256": hashlib.sha256(proof.encode("utf-8")).hexdigest(),
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
    covered = sum(1 for cls in open_classes if cls in PALETTE or cls in ALIAS or cls in NO_INK)
    lines = ["class\tcolor\trecords\tmarked_chars\tpercent_total"]
    for cls in sorted(PALETTE, key=lambda x: (x[0], int(x[1:]) if x[1:].isdigit() else 99, x)):
        if cls == "R3" and TYPED is None:
            continue  # typed class not measured: no typed-flags.jsonl beside the shards
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
