"""B2 census check (claude-18, packet RC-B2): nothing is introduced or stated that the proof
does not use.

What it decides.  Every introduction the page makes with the printer's typed anchor
`\\leanref{}{local:<decl>:<name>}` on a `Define` line or a `We have` line is looked up among the
binders of the declaration's elaborated terms (type_expression and value_expression):
  * Define <name>: the `letE` binders named <name> (a statement `let` or a proof `let`/`set`).
  * We have <name>: the `lam` / `letE` binders named <name> (`have`, `obtain`, `intro`).
Violations:
  B2-alias      a Define whose let value is a local variable (typed: the letE value, mdata
                stripped, is a bound/free variable, or a constant applied to exactly one explicit
                argument that is such a variable, e.g. `c.hom`, `FundamentalGroupoid.mk x`):
                "Define c_g = c", "Define z = x" name a thing already named.  The wrapper form
                also needs the page's right side to be a bare name (so `E = equivalence(P)` is not
                an alias: it applies a function).
  B2-dead-let   a Define none of whose letE binders is referred to in its body (typed: bvar 0 is
                not loose in the letE body): the definition is never used by the proof.
  B2-unused-define  a Define whose display (the text left of " = " in that Define line) occurs
                nowhere on the page after the Define's formula, although the term uses the let: the
                page introduces a name and then states everything with the value unfolded
                (t92J05 "Define ι_* = …" with a closing line that writes the value).  Typed part:
                the name is a letE binder; the page is read only for where its display occurs.
  B2-restated   a Define of a let-bound name (typed) whose right side repeats an earlier Define of
                the same name that is still in scope (not in a sibling item or sibling "Case n:"):
                a definition restated.  Which lets the two lines stand for cannot be paired one by
                one, so the repetition is read on the page (two lets with one value, or one let
                printed twice, read the same).  Sibling case branches that define
                the name differently are not restatements.
  B2-dead-fact  a We-have fact none of whose binders is referred to in its scope (typed: bvar 0 not
                loose in the binder's body): a stated fact the proof never uses.
Unmeasured: no elaborated constant / value; a Define or We-have anchor whose name binds nothing
in the terms (the check cannot tell which term the line stands for).

Cost: one pass over each term computing loose-bvar sets bottom-up (memoised by node identity);
no walk.  Page text is used only to locate the lines and the displays.

Misses:
  * stated facts and Defines printed WITHOUT a local anchor (anonymous `this`, "By definition, …"
    lines, Take/Let lines), and facts consumed only by adjacency on the page;
  * a fact that the term uses but the page never cites (that is a citation rule, not B2);
  * shadowed names: if any binder with the name is used, the line counts as used;
  * B2-unused-define with a one-letter display that also occurs as a different symbol later
    (counted as a use: a miss, never a false flag);
  * dead setup that the printer already silences (Q287) is not on the page, so not seen.
"""
import re

RULE = "B2"

_ANCHOR = re.compile(r"\\leanref\{\}\{local:([^{}]*):([^{}:]*)\}")


def _strip_mdata(e):
    from mfuton.lean.latex_pp import _expr
    while e is not None and _expr.ctor_of(e) == "mdata":
        e = _expr.mdata_child(e)
    return e


class _Terms:
    """Binders by name, with a loose-bvar oracle, for one constant's terms."""

    def __init__(self, const):
        self.memo = {}
        self.lets = {}   # name -> [letE nodes]
        self.binders = {}  # name -> [lam/letE nodes]
        seen = set()
        for f in ("type_expression", "value_expression"):
            e = getattr(const, f, None)
            if e is not None:
                self._index(e, seen)

    def _index(self, root, seen):
        stack = [root]
        while stack:
            e = stack.pop()
            if e is None or id(e) in seen:
                continue
            seen.add(id(e))
            b = e.binding
            if b is not None:
                name = b.name.dotted() if b.name is not None else ""
                if b.kind == "let":
                    self.lets.setdefault(name, []).append(e)
                if b.kind in ("let", "lambda"):
                    self.binders.setdefault(name, []).append(e)
            if e.function is not None:
                stack.append(e.function)
            stack.extend(a for a in (e.arguments or ()) if a is not None)

    def loose(self, root):
        """Loose de Bruijn indices of `root` (iterative, memoised by node identity)."""
        memo = self.memo
        stack = [(root, False)]
        while stack:
            e, done = stack.pop()
            if e is None or id(e) in memo:
                continue
            kids = ([e.function] if e.function is not None else []) + [a for a in (e.arguments or ()) if a is not None]
            if not done:
                stack.append((e, True))
                stack.extend((k, False) for k in kids if id(k) not in memo)
                continue
            s = set()
            if e.bound_index is not None:
                s.add(e.bound_index)
            if e.binding is not None:
                args = list(e.arguments or ())
                body_i = 2 if e.binding.kind == "let" else 1
                for i, a in enumerate(args):
                    if a is None:
                        continue
                    if i == body_i:
                        s |= {j - 1 for j in memo.get(id(a), ()) if j > 0}
                    else:
                        s |= memo.get(id(a), frozenset())
                if e.function is not None:
                    s |= memo.get(id(e.function), frozenset())
            else:
                for k in kids:
                    s |= memo.get(id(k), frozenset())
            memo[id(e)] = frozenset(s)
        return memo.get(id(root), frozenset())

    def used(self, node):
        from mfuton.lean.latex_pp import _expr
        body = _expr.binder_body(node)
        return 0 in self.loose(body)


def _line_end(text, i):
    j = text.find("\n", i)
    return len(text) if j < 0 else j


def _visible(text):
    """Page text with leanref targets removed (`\\leanref{D}{T}` -> `D`), for display search."""
    out = re.sub(r"\}\{(?:local:)?[^{}\s\\]*\}", "}", text)
    return out.replace("\\leanref{", "{")


def _display_of_define(text, end_of_anchor):
    """The display of a Define line: the math after the anchor, up to its first top-level ' = '."""
    m = re.match(r"\\\(((?:(?!\\\)).)*?) = ", text[end_of_anchor:end_of_anchor + 400], re.S)
    return m.group(1).strip() if m else ""


def _occurs(display, text):
    vis_disp = _visible(display)
    if not vis_disp:
        return True
    pat = re.escape(vis_disp)
    if re.match(r"[A-Za-z]", vis_disp):
        pat = r"(?<![A-Za-z\\])" + pat
    if re.search(r"[A-Za-z]$", vis_disp):
        pat = pat + r"(?![A-Za-z])"
    return re.search(pat, _visible(text)) is not None


def check(row, const, constants):
    text = row.get("proof") or ""
    decl = row.get("declaration")
    if const is None or getattr(const, "value_expression", None) is None:
        return [{"reason": "unmeasured: no elaborated constant", "anchor": ""}] if "\\leanref{}{local:" in text else []
    terms = None
    hits, unmeasured = [], []
    defined = {}
    for m in _ANCHOR.finditer(text):
        if m.group(1) != decl:
            continue
        name = m.group(2)
        start = text.rfind("\n", 0, m.start()) + 1
        lead = text[start:m.start()]
        lead = re.sub(r"^\\item\s*", "", lead.strip())
        kind = "define" if lead.endswith("Define") else "fact" if lead.endswith("We have") else None
        if kind is None or name in ("this", ""):
            continue
        if terms is None:
            terms = _Terms(const)
        line = text[start:_line_end(text, m.end())]
        anchor = line[:200]
        if kind == "define":
            nodes = terms.lets.get(name, [])
            if not nodes:
                unmeasured.append({"reason": f"unmeasured: Define {name} binds no let in the terms", "anchor": anchor})
                continue
            rhs = _rhs_of_define(text, m.end())
            if name in defined:
                if rhs and any(r == rhs and _in_scope(text, pos, m.start()) for pos, r in defined[name]):
                    hits.append({"reason": f"B2-restated: Define {name} restated (same definition still in scope)", "anchor": anchor})
                defined[name].append((m.start(), rhs))
                continue
            defined[name] = [(m.start(), rhs)]
            if any(_is_alias_value(_expr_let_value(n), rhs) for n in nodes):
                hits.append({"reason": f"B2-alias: Define {name} names a local already named (its let value is a variable)", "anchor": anchor})
                continue
            if not any(terms.used(n) for n in nodes):
                hits.append({"reason": f"B2-dead-let: Define {name} is never used (no let body refers to it)", "anchor": anchor})
                continue
            display = _display_of_define(text, m.end())
            close = text.find("\\)", m.end())
            rest = text[(close + 2 if close >= 0 else _line_end(text, m.end())):]
            if display and not _occurs(display, rest):
                hits.append({"reason": f"B2-unused-define: Define {name} ({display}) is not used on the page after it", "anchor": anchor})
        else:
            nodes = terms.binders.get(name, [])
            if not nodes:
                unmeasured.append({"reason": f"unmeasured: fact {name} binds nothing in the terms", "anchor": anchor})
                continue
            if not any(terms.used(n) for n in nodes):
                hits.append({"reason": f"B2-dead-fact: fact {name} is never used (no binder body refers to it)", "anchor": anchor})
    return hits + unmeasured


_STRUCT = re.compile(r"\\begin\{itemize\}|\\end\{itemize\}|\\item\b|Case \d+:")


def _in_scope(text, earlier, later):
    """Is a line at `earlier` still in scope at `later`?  It holds until the itemize containing it
    closes, a sibling \\item starts at its level, or a sibling "Case n:" starts at its level."""
    depth = text.count("\\begin{itemize}", 0, earlier) - text.count("\\end{itemize}", 0, earlier)
    d = depth
    for mm in _STRUCT.finditer(text, earlier, later):
        tok = mm.group(0)
        if tok.startswith("\\begin"):
            d += 1
        elif tok.startswith("\\end"):
            d -= 1
            if d < depth:
                return False
        elif d == depth:
            return False
    return True


def _same_value_twice(nodes):
    """Typed restatement: two let binders of the name whose values are the same term."""
    vals = [_strip_mdata(_expr_let_value(n)) for n in nodes]
    for i in range(len(vals)):
        for j in range(i + 1, len(vals)):
            if vals[i] == vals[j]:
                return True
    return False


_TOKEN = re.compile(r"^\\?[A-Za-z]+(?:_\{[^{}()]*\}|_[A-Za-z0-9])?(?:\^\{\\prime\}|')?$")


def _rhs_of_define(text, end_of_anchor):
    m = re.match(r"\\\(((?:(?!\\\)).)*?) = ((?:(?!\\\)).)*)\\\)", text[end_of_anchor:end_of_anchor + 600], re.S)
    if not m:
        return ""
    rhs = m.group(2)
    for _ in range(6):
        rhs = re.sub(r"\\leanref\{((?:[^{}]|\{[^{}]*\})*)\}\{[^{}]*\}", r"\1", rhs)
    return rhs.strip()


def _is_alias_value(v, rhs=""):
    """A let value that is a local variable: bare (typed alone decides), or under a wrapper whose
    only explicit argument is that variable (a coercion/structure map: `c.hom`, `mk x`) when the
    page prints the right side as that bare name ("c_g = c"), not as an application."""
    from mfuton.lean.latex_pp import _expr
    v = _strip_mdata(v)
    if _ctor(v) in ("bvar", "fvar"):
        return True
    if not _TOKEN.match(rhs or ""):
        return False
    if _ctor(v) != "app" or _ctor(_expr.app_head(v)) != "const":
        return False
    args = _expr.app_args(v)
    infos = _expr.app_arg_binder_ordinals(v)
    explicit = [a for a, bi in zip(args, infos) if _expr.binder_info_is_explicit(bi)]
    return len(explicit) == 1 and _ctor(_strip_mdata(explicit[0])) in ("bvar", "fvar")


def _expr_let_value(n):
    from mfuton.lean.latex_pp import _expr
    return _expr.let_value(n)


def _ctor(e):
    from mfuton.lean.latex_pp import _expr
    return _expr.ctor_of(e) if e is not None else ""
