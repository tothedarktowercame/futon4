#!/usr/bin/env python3
"""Count reader-visible Lean words in statement and optional proof fields."""
import collections
import gzip
import json
import re
import sys

def _drop_href_targets(b):
    """\\href{target}{text} shows only its text; the URL target is not on the page
    (scanner change 2026-11-26, Q521: a95A07's "by Lean's convention x/0 = 0" gloss
    links to a URL whose host read as a dotted Lean word)."""
    out, i = [], 0
    while True:
        j = b.find("\\href{", i)
        if j < 0:
            out.append(b[i:])
            return "".join(out)
        out.append(b[i:j]); k = j + 6; d = 1
        while d:
            d += {"{": 1, "}": -1}.get(b[k], 0); k += 1
        i = k  # resume at the {text} group, which stays visible


def visible(b):
    b = _drop_href_targets(b)
    out, i = [], 0
    while True:
        j = b.find("\\leanref{", i)
        if j < 0:
            out.append(b[i:])
            return "".join(out)
        out.append(b[i:j]); k = j + 9; s = k; d = 1
        while d:
            d += {"{": 1, "}": -1}.get(b[k], 0); k += 1
        shown = b[s:k - 1]; k += 1; d = 1
        while d:
            d += {"{": 1, "}": -1}.get(b[k], 0); k += 1
        out.append(visible(shown)); i = k

W = re.compile(r"(?<![\\A-Za-z])[A-Za-z][A-Za-z0-9']*(?:[._][A-Za-z][A-Za-z0-9']*)+"
               r"|(?<![\\A-Za-z])(?:[a-z]+[A-Z]|[A-Z]{2,}[a-z]|[A-Z][a-z]+[A-Z])[A-Za-z0-9']*"
               r"|\\operatorname\{[a-z]+ [a-z ]+\}|\bmatch \d|\bself\b|\.\d\b")
ALLOW = {"a.e", "C_b"}

def hits(text):
    text = re.sub(r"\\seqsplit\{([^}]*)\}", r"\1", visible(text or ""))
    return {h for h in W.findall(text) if h not in ALLOW}

def main(path):
    op = gzip.open if path.endswith(".gz") else open
    statement_counts = collections.Counter(); proof_counts = collections.Counter()
    statement_examples = {}; proof_examples = {}
    records = proofs_rendered = proofs_failed = proofs_timed_out = 0
    statements_with = proofs_with = 0
    with op(path, "rt", encoding="utf-8") as stream:
        for line in stream:
            r = json.loads(line)
            if r.get("declaration") == "<module>":
                continue
            records += 1
            sh = hits(r.get("body"))
            statements_with += bool(sh)
            for h in sh:
                statement_counts[h] += 1; statement_examples.setdefault(h, r["declaration"])
            if "proof" not in r:
                continue
            if r.get("proof_error"):
                proofs_failed += 1
                proofs_timed_out += r["proof_error"] == "timeout"
                continue
            if r.get("proof") is None:
                continue
            proofs_rendered += 1
            ph = hits(r.get("proof"))
            proofs_with += bool(ph)
            for h in ph:
                proof_counts[h] += 1; proof_examples.setdefault(h, r["declaration"])
    print(f"records: {records}")
    print(f"statements: {statements_with} with a Lean-ism; {len(statement_counts)} distinct")
    print(f"proofs: {proofs_rendered} rendered, {proofs_failed} failed, {proofs_timed_out} timed out; "
          f"{proofs_with} with a Lean-ism; {len(proof_counts)} distinct")
    print("TOP STATEMENT LEAN WORDS")
    for h, n in statement_counts.most_common(): print(f"{n}\t{h}\t{statement_examples[h]}")
    print("TOP PROOF LEAN WORDS")
    for h, n in proof_counts.most_common(): print(f"{n}\t{h}\t{proof_examples[h]}")
    print("PROOF-ONLY VOCABULARY")
    for h, n in (proof_counts - statement_counts).most_common(): print(f"{n}\t{h}\t{proof_examples[h]}")

if __name__ == "__main__":
    main(sys.argv[1])
