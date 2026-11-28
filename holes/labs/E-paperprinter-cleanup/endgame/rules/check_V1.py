"""V1 census check (claude-26, packet RC-V1): the page prints mathematical notation and words, never
a Lean name the printer had no reading for.

What it decides.  Every constant the printer renders is tagged on the page as
`\\leanref{<display>}{<FQN>}` (printer_core `_r_const` / entity_link), so the page itself carries the
typed constant behind each display.  For each tagged constant the check asks the census printer
(an `ApmPaperPrinter` built by `make_blueprint_printer` over the "apm-proofs" display layer, the same
construction `latex_pp.paper.lint` uses) whether it has a reading:

  * ruled: `_has_rendering_rule(FQN)` (const rule, override, the book's const datum or app rule, a
    ported app method, binop, bounded predicate, interval) or a `local-definitions.json` display.
    A ruled constant is not judged (see Misses).
  * unruled: the printer's own fallback for it, `_const_body(FQN)`, is then the raw name: the
    `\\text{\\seqsplit{<stripped name>}}` marker, or for a problem-local name the camel-case words
    `_camel_words(leaf)`.

A violation is an unruled constant whose display shows that fallback:
  "raw name"    the display contains the stripped Lean name (or its leaf, when the leaf is
                identifier-shaped: an inner capital after a lower-case letter, an underscore, or a
                letter-digit run), or a `\\seqsplit` marker;
  "name words"  the display spells the leaf's camel words (or the words of the printer's own
                `\\operatorname{...}` fallback for it) and those words keep identifier residue
                (a token with digits, or two or more one-letter tokens: "is m 03 j 02 solution",
                "is c 2"), i.e. the words are the identifier cut apart, not a reading.
Also, page-wide: any `\\seqsplit` (the printer's unruled-name marker, kept for exactly this count),
and any dotted name of a constant of the declaration's elaborated terms (type and value) or of a
constant tagged anywhere on the page, ruled or not, that is printed verbatim outside a `\\leanref` target (a raw `HilbertBasis.repr` in a citation or a chain).

The page text is used to find the tags and to anchor; the decision is the printer's lookup for the
tagged FQN and the constants of the typed terms.  No declaration names, no word list.
Records with no proof text are judged on the statement (`body`) alone; anchor "" there.

Cost: the printer is built once per process (~0.7 s); each record is one regex pass plus one cached
lookup per distinct constant (about 2 ms a record; the typed-term walk adds the constants load
that rule_checks.py already pays).

Misses:
  * a RULED constant whose reading itself carries implementation words ("the Fintype cardinality",
    "Mathlib (summing a convergent series)", "Suppose N is a \\text{Type}"): the rule's text is not
    judged; deciding it needs a vocabulary, which this check does not have;
  * an unruled problem-local name printed as clean words ("superlevel(p)", "lemniscate(p)"): words
    with no identifier residue are passed (whether they were defined on the page is B1);
  * Lean syntax that is not a constant: a predicate lambda x ↦ x ∈ S (C8), hidden or shown implicit
    arguments (P-hidden-arguments), a raw ⊥/⊤ that is correct notation in the wrong place (C5, C6),
    raw hypothesis names in math (hsH): there is no tagged constant to look up;
  * a display that drops a map argument (C12) or shows the wrong object (C15 lp.single as j):
    those are T1 (the formula denotes the term), not vocabulary.
"""
import re

RULE = "V1"

_P = None  # (printer, helpers, LeanTypeExpression, local readings)


def _printer():
    global _P
    if _P is None:
        from dataclasses import replace
        from mfuton.lean.blueprint_render.project_display_layer import project_display_layer
        from mfuton.lean.blueprint_render.blueprint_printer import make_blueprint_printer
        from mfuton.lean.blueprint_render.paper_walk import ApmPaperPrinter
        from mfuton.lean.ast.lean_parser.python_compiled_expression import LeanTypeExpression
        from mfuton.lean.latex_pp.paper import helpers as H
        from mfuton.lean.latex_pp.paper import printer as PP
        from mfuton.mfuton_home import mfuton_home
        layer = replace(project_display_layer("apm-proofs", home=mfuton_home()), printer_class=ApmPaperPrinter)
        _P = (make_blueprint_printer(layer), H, LeanTypeExpression, PP._LOCAL_DEFINITIONS)
    return _P


_STATUS = {}


def _status(fqn):
    """None if the printer has a reading for fqn; else (stripped name, leaf, fallback output)."""
    if fqn in _STATUS:
        return _STATUS[fqn]
    p, H, LTE, local = _printer()
    st = None
    if not (p._has_rendering_rule(fqn) or (local.get(fqn) or {}).get("display")):
        try:
            fb = p._const_body(LTE(fqn)).encode()[0]
        except Exception:
            fb = ""
        stripped = p.stripped_const(fqn).replace("«", "").replace("»", "")
        leaf = H._erase_private_prefix(fqn).rsplit(".", 1)[-1]
        if fb == "":          # instance values and casts print as nothing
            st = None
        else:
            st = (stripped, leaf, fb, H._camel_words(leaf))
    _STATUS[fqn] = st
    return st


_READING = {}


def _reading(fqn):
    r"""A ruled constant's own reading when it prints alone (Nat -> \mathbb{N}, Real.pi -> \pi), or
    None when the rule applies only to an application (its bare body is the generic fallback)."""
    if fqn not in _READING:
        p, H, LTE, _local = _printer()
        try:
            out = p._const_body(LTE(fqn)).encode()[0]
        except Exception:
            out = ""
        _READING[fqn] = None if (not out or "\\seqsplit" in out) else out
    return _READING[fqn]


def H_numbered(leaf):
    """The printer's numbered-symbol reading (S2 -> S_{2}): notation, not words."""
    return _printer()[1]._numbered_symbol(leaf)


def _anchors(s):
    """(start, end, display, target) of each \\leanref{display}{target} in s, outermost first."""
    out, i = [], 0
    while True:
        j = s.find("\\leanref{", i)
        if j < 0:
            return out
        k, depth = j + 9, 1
        while k < len(s) and depth:
            if s[k] == "\\":
                k += 2
                continue
            depth += {"{": 1, "}": -1}.get(s[k], 0)
            k += 1
        disp = s[j + 9:k - 1]
        if k < len(s) and s[k] == "{":
            m = s.find("}", k)
            out.append((j, m + 1, disp, s[k + 1:m], k))
            i = j + 9          # descend: nested anchors are found next
        else:
            i = k


_ID_SHAPED = re.compile(r"[a-z][A-Z]|_|[A-Za-z][0-9]|[0-9][A-Za-z]")
_NESTED = re.compile(r"\\leanref\{")


def _plain(tex):
    """Words of a display: anchor targets dropped, TeX control words and braces removed."""
    tex = _outside_targets(tex)
    tex = re.sub(r"\\[A-Za-z]+", " ", tex)
    tex = re.sub(r"\\.", " ", tex)
    return " ".join(re.sub(r"[{}$^_(),]", " ", tex).split()).lower()


def _residue(words):
    toks = words.split()
    return any(re.search(r"\d", t) for t in toks) or sum(len(t) == 1 and t.isalpha() for t in toks) >= 2


def _outside_targets(s):
    """s with every \\leanref target ({FQN} / {local:...}) removed."""
    cuts = sorted((k, end) for _s, end, _d, _t, k in _anchors(s))
    out, i = [], 0
    for k, end in cuts:
        if k >= i:
            out.append(s[i:k])
            i = end
    out.append(s[i:])
    return "".join(out)


def _term_consts(const):
    from mfuton.lean.latex_pp import _expr
    acc, seen = set(), set()
    for root in (getattr(const, "type_expression", None), getattr(const, "value_expression", None)):
        stack, fuel = [root], 0
        while stack and fuel < 300000:
            e = stack.pop()
            fuel += 1
            if e is None or id(e) in seen:
                continue
            seen.add(id(e))
            if _expr.ctor_of(e) == "const":
                acc.add(str(_expr.const_name(e)))
            try:
                stack.extend(e.children())
            except Exception:
                pass
    return acc


def _line(text, pos):
    a = text.rfind("\n", 0, pos) + 1
    b = text.find("\n", pos)
    return text[a:b if b >= 0 else len(text)]


def check(row, const, constants):
    proof = row.get("proof") or ""
    body = row.get("body") or ""
    hits, seen = [], set()

    def add(reason, field, pos):
        key = (reason, field)
        if key in seen:
            return
        seen.add(key)
        hits.append({"reason": reason, "anchor": _line(proof, pos) if field == "proof" else ""})

    for field, text in (("body", body), ("proof", proof)):
        for start, end, disp, target, _k in _anchors(text):
            if not target or target.startswith("local:"):
                continue
            st = _status(target)
            if st is None:
                # claude-16 (review, 2026-11-28): a constant WITH a reading printed under its Lean
                # name (\leanref{Nat}{Nat} for \mathbb{N}).  Judged where the reading is the
                # constant's own (it prints alone) and does not itself contain the name.
                reading = _reading(target)
                leaf = _printer()[1]._erase_private_prefix(target).rsplit(".", 1)[-1]
                own = _outside_targets(_NESTED.split(disp)[0])
                if (reading and len(leaf) > 1 and leaf not in reading
                        and re.search(r"(?<![A-Za-z0-9\\])" + re.escape(leaf) + r"(?![A-Za-z0-9])", own)):
                    add(f"raw name: {target} has the reading {reading} but prints as its Lean name", field, start)
                continue
            stripped, leaf, fb, words = st
            own = _NESTED.split(disp)[0]  # the display's own text, before any nested anchor
            shown = _outside_targets(disp)
            if "\\seqsplit" in own or any(
                    len(n) > 1 and ("." in n or _ID_SHAPED.search(n))
                    and re.search(r"(?<![A-Za-z0-9.])" + re.escape(n) + r"(?![A-Za-z0-9])", shown)
                    for n in {stripped, leaf}):
                add(f"raw name: {target} has no reading in the printer and prints as its Lean name", field, start)
            elif H_numbered(leaf) is None and any(w and _residue(w) and re.search(r"(?<![a-z0-9])" + re.escape(w) + r"(?![a-z0-9])", _plain(disp))
                     for w in (words, _plain(fb) if fb.startswith("\\operatorname") else "")):
                add(f"name words: {target} has no reading in the printer and prints as its identifier cut into words", field, start)
        for m in re.finditer(r"\\seqsplit\{", text):
            add("raw name: \\seqsplit marker (the printer's unruled-name fallback) on the page", field, m.start())
        tagged = {t for _s, _e, _d, t, _k in _anchors(body + proof) if t and not t.startswith("local:")}
        if True:
            outside = _outside_targets(text)
            for c in (_term_consts(const) if const is not None else set()) | tagged:
                if "." not in c or c.startswith("_private") or "._" in c:
                    continue
                pos = outside.find(c)
                if pos >= 0 and not outside[pos + len(c):pos + len(c) + 1].isalnum() and (pos == 0 or not (outside[pos - 1].isalnum() or outside[pos - 1] == ".")):
                    # map the position back to the raw text for the anchor
                    raw = text.find(c)
                    add(f"raw name: {c} (a constant of the term or tagged on the page) printed verbatim as its dotted Lean name", field, raw if raw >= 0 else 0)
    return hits
