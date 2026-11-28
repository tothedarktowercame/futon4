"""GAP census check (claude-19, packet GEN-GAP): count the marked gaps the printer emitted.

What it decides.  Where the paper walk has no typed form for a step it prints a marked gap,
`\\grammargap{<rule>}{<why>}` (`paper_proof_walk.render.grammar_gap`), in place of a fallback
sentence.  The marker is the printer's own typed output, not reader text: the walk emits it only
from a code path that decided, on the Step's typed data, that no form applies (GRAMMAR.md,
"Generation": gaps, not fallbacks).  So this check does not judge the page; it reports one flag
per marker, with the rule the marker names, so the ledger can count gaps per rule:

  reason  "gap: <rule> <why>"     (for example "gap: R2 no typed reason for this step")
  anchor  the sentence the marker sits in (text before it on its line, up to 160 characters)

A marker whose rule is not a grammar.tsv id, or whose braces do not close, is reported as
"gap: malformed marker", so a broken emitter shows up instead of disappearing from the count.

How.  A brace-aware scan of `row["proof"]` (the census proof field) and `row["body"]` for the
macro name; the two arguments are read with the same brace matching the page uses.  `const` and
`constants` are not needed: the decision is on the marker the printer emitted.

Misses:
  * A gap the printer emitted and a later pass removed (dropped as a repeated fact, folded into
    a chain row) is not on the page and is not counted.  The count is of gaps on the page.
  * Definition and lemma cards are not census records, so a gap on a card is not counted.
  * The check counts gaps; it does not find fallbacks that are still printed as prose.  The
    fallback sites not yet converted are in ~/notes/claude-16/coord/gen1/GEN-INV.tsv.
"""

RULE = "GAP"

_MACRO = "\\grammargap"
import os as _os

# The grammar's rule ids, read from grammar.tsv so a rule Joe adds (B4, 2026-11-28) is a valid gap.
with open(_os.path.join(_os.path.dirname(_os.path.dirname(_os.path.abspath(__file__))), "grammar.tsv"),
          encoding="utf-8") as _f:
    _RULES = frozenset(line.split("\t", 1)[0] for line in _f if line.strip() and not line.startswith("rule\t"))


def _braced(text, start):
    """(content, index after the closing brace) for a `{...}` group at `start`, or None."""
    while start < len(text) and text[start] == " ":
        start += 1
    if start >= len(text) or text[start] != "{":
        return None
    depth = 0
    for i in range(start, len(text)):
        if text[i] == "{":
            depth += 1
        elif text[i] == "}":
            depth -= 1
            if depth == 0:
                return text[start + 1:i], i + 1
    return None


def gaps(text):
    """[(rule, why, position)] for every marker in `text`; rule None for a malformed marker."""
    out = []
    pos = 0
    while (found := text.find(_MACRO, pos)) >= 0:
        after = found + len(_MACRO)
        if after < len(text) and text[after].isalpha():  # a longer macro name
            pos = after
            continue
        first = _braced(text, after)
        second = _braced(text, first[1]) if first else None
        if second is None or first[0] not in _RULES:
            out.append((None, "", found))
            pos = after
            continue
        out.append((first[0], second[0], found))
        pos = second[1]
    return out


def check(row, const=None, constants=None):
    hits = []
    for field in ("body", "proof"):
        text = row.get(field) or ""
        for rule, why, at in gaps(text):
            line_start = text.rfind("\n", 0, at) + 1
            anchor = text[max(line_start, at - 160):at].strip()
            hits.append({"reason": f"gap: {rule} {why}" if rule else "gap: malformed marker",
                         "anchor": anchor})
    return hits
