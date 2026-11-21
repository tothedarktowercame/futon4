#!/usr/bin/env python3
"""Per-class counts over a full APM render (claude-7, 2026-11-20). Text scanner: it counts the
classes that have a regex; the others are counted by declaration lists in classes.tsv.

  scan.py '/tmp/q133z-k-0?.jsonl' [commit]      # prints commit<TAB>class<TAB>records<TAB>occurrences

Corpus: every record of the render (8,778 at the freeze), not the 757-record tracked subset the
queue log counted on until Q109.  leanism_scan_p1.py is codex-3's raw-word scanner, copied here
so the scanner is frozen with the ledger.
"""
import collections, glob, json, os, re, sys
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from leanism_scan_p1 import hits, visible

FAMILY = {
    'C1': re.compile(r'distinguish cases on[^.]*\\lor'),
    'C5': re.compile(r'\\bot'),
    'C6': re.compile(r'\\top'),
    # a predicate lambda `x ↦ x ∈ S`; not `(x ↦ f x) ∈ L^1`, which is a function in a space
    'C8': re.compile(r'\\mapsto\s*[A-Za-z0-9_\'{}]+\s*\\in\b'),
    'C16': re.compile(r'by transitivity'),
    'C17': re.compile(r'\\seqsplit'),
    'N1': re.compile(r'&& \\text\{\}'),
}
# raw words that are local names with a subscript (c_A, q_K, f_n) or the English "self": not defects
LOCAL = re.compile(r"^[A-Za-z]_[A-Za-z]'?$|^self$")

NEST = re.compile(r'\\(begin|end)\{[^}]*\}|\\\\')

def align_rows(block):
    """Split an align* body at its own row breaks. A break inside a nested environment (cases, a matrix)
    is a line of that environment, not a chain row (Q148: 155 of 809 repeats at a6e043d3 came from those)."""
    rows, depth, start = [], 0, 0
    for m in NEST.finditer(block):
        if m.group(1) == 'begin': depth += 1
        elif m.group(1) == 'end': depth -= 1
        elif depth == 0:
            rows.append(block[start:m.start()]); start = m.end()
    rows.append(block[start:])
    return rows

def main(pattern, commit='-'):
    rec = collections.Counter(); occ = collections.Counter(); n = errors = 0
    for path in sorted(glob.glob(pattern)):
        for line in open(path):
            r = json.loads(line); n += 1
            errors += bool(r.get('proof_error'))
            proof = r.get('proof') or ''
            for k, rx in FAMILY.items():
                c = len(rx.findall(proof))
                if c: rec[k] += 1; occ[k] += c
            words = {w for w in hits(proof) if not LOCAL.match(w)}
            if words: rec['C7'] += 1; occ['C7'] += len(words)
            # C4: a displayed chain that prints the same expression on two rows
            repeats = 0
            for block in re.findall(r'\\begin\{align\*\}(.*?)\\end\{align\*\}', visible(proof), re.S):
                rows = [x.split('&&')[0].strip() for x in align_rows(block)]
                rows = [x for x in rows if x]
                repeats += len(rows) - len(set(rows))
            if repeats: rec['C4'] += 1; occ['C4'] += repeats
    print(f'{commit}\trecords\t{n}\t{n}')
    print(f'{commit}\tproof-errors\t{errors}\t{errors}')
    for k in sorted(set(FAMILY) | {'C4', 'C7'}, key=lambda s: (s[0], int(s[1:]))):
        print(f'{commit}\t{k}\t{rec[k]}\t{occ[k]}')

if __name__ == '__main__':
    main(*sys.argv[1:3])
