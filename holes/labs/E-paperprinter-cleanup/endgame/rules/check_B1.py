"""B1 census check (claude-19, packet RC-B1): every local name a page line uses is introduced
on the page, in an enclosing scope, before that use.

Typed part: the names that CAN be unintroduced are the local binder names of the declaration's
elaborated terms (lam / letE / forallE binders of value_expression and type_expression, not
`Name.isInternal`), rendered with the printer's own `name_to_latex`.  A page token that is not
one of those displays is never a use (constants, notation, numbers are not locals).

Text part (the census record carries only text): where each name is introduced and used.
  * introductions: the printer's typed intro anchors `\\leanref{}{local:<decl>:<name>}` (emitted
    from the binder that the Let/Suppose/Obtain/Define/Case line introduces); plus the leading
    tokens of an anchorless Let/Fix/Take line (the statement's "Let \\(X, \\iota\\) be types",
    "Take \\(i = …\\)");
  * scope: an introduction holds until the itemize that contains it closes;
  * uses: occurrences of a local display inside math (\\( \\), \\[ \\], align) that are not
    bound in that formula (∀/∃ prefixes, big-operator subscripts, set-builder, (x : T) ↦,
    x ↦, d x) and not inside \\text/\\operatorname/\\mathrm/… or a constant's atomic symbol.
A use with no introduction in scope is a violation; anchor = that line.
Not covered: the rule's second half (the statement shows every hypothesis the proof uses).
"""
import re

RULE = "B1"

_name_to_latex = None


def _n2l(name):
    global _name_to_latex
    if _name_to_latex is None:
        from mfuton.lean.latex_pp.printer_helpers import name_to_latex
        _name_to_latex = name_to_latex
    try:
        return _name_to_latex(name, {})
    except Exception:
        return None


def _is_internal(name):
    return name == "" or "✝" in name or any(p.startswith("_") for p in name.split("."))


_BINDER_CTORS = ("lam", "forallE", "letE")


def _binder_names(expr, out, seen, consts=None):
    from mfuton.lean.latex_pp import _expr
    stack = [expr]
    while stack:
        e = stack.pop()
        if e is None or id(e) in seen:
            continue
        seen.add(id(e))
        if consts is not None and _expr.is_const(e):
            consts.add(str(_expr.const_name(e)))
        if _expr.ctor_of(e) in _BINDER_CTORS:
            n = _expr.binder_name(e)
            if n:
                out.add(str(n))
        try:
            stack.extend(c for c in e.children() if c is not None)
        except Exception:
            pass


def _local_displays(const):
    names = set()
    seen = set()
    for f in ("type_expression", "value_expression"):
        e = getattr(const, f, None)
        if e is not None:
            _binder_names(e, names, seen)
    pairs = []
    for n in sorted(names):  # sorted: a set's order follows PYTHONHASHSEED, and the result must not
        if _is_internal(n):
            continue
        d = _n2l(n)
        if not d or d.startswith("\\mathrm{h") or len(d) > 40:
            continue  # hypothesis names never print in math
        pairs.append((d, n))
    return pairs


def _statement_displays(const):
    """Displays of the statement's leading ∀/→ binders: the printed statement (the record's
    `body`, "For x ∈ S, we have …") introduces them before the proof starts."""
    from mfuton.lean.latex_pp import _expr
    out = set()
    e = getattr(const, "type_expression", None)
    while e is not None and _expr.ctor_of(e) == "forallE":
        n = str(_expr.binder_name(e) or "")
        if not _is_internal(n):
            d = _n2l(n)
            if d:
                out.add(d)
        e = _expr.binder_body(e)
    return out


# constants the printer writes bare (no leanref): a bare display equal to one of these, in a
# declaration whose terms reference the constant, is that constant and not a local
_BARE_CONSTANTS = {"Complex.I": "i"}


def _bare_constant_displays(const):
    consts = set()
    for f in ("type_expression", "value_expression"):
        e = getattr(const, f, None)
        if e is not None:
            _binder_names(e, set(), set(), consts)
    return {d for c, d in _BARE_CONSTANTS.items() if c in consts}


# ---------------------------------------------------------------- text helpers
def _group(s, i):
    """s[i] == '{': return (content, index after the matching '}')."""
    depth = 0
    for j in range(i, len(s)):
        if s[j] == "{":
            depth += 1
        elif s[j] == "}":
            depth -= 1
            if depth == 0:
                return s[i + 1:j], j + 1
    return s[i + 1:], len(s)


_ATOM = re.compile(r"^(\\?[A-Za-z]+|\\math\w+\{[^{}]*\}|[A-Za-z]_\{?\w+\}?|\\[A-Za-z]+_\{?\w+\}?)$")
_TEXT_BINDER = re.compile(r"\b(for all|for every|for each|for some|for a\.e\.|for almost|frequently|eventually|there (?:is|exists))")
_WRAPS = ("\\text", "\\operatorname", "\\mathrm", "\\mathbb", "\\mathcal", "\\mathfrak",
          "\\mathit", "\\mathsf", "\\mathbf", "\\seqsplit", "\\textit", "\\textbf")


def _sub_norm(t):
    """One subscript form: the printer writes a name's subscript both as L_0 and L_{0}."""
    return re.sub(r"_\{([A-Za-z0-9])\}", r"_\1", t)


def _clean(s, displays, anchors, decl):
    """Drop constant names (leanref 2nd arg; atomic constant symbols), wrapper contents, and
    record intro anchors as markers \x00name\x00."""
    out = []
    i = 0
    n = len(s)
    prefix = "local:" + decl + ":"
    while i < n:
        if s.startswith("\\leanref{", i):
            a, j = _group(s, i + 8)
            if j < n and s[j] == "{":
                b, k = _group(s, j)
            else:
                b, k = "", j
            if a == "" and b.startswith("local:"):
                name = b[len(prefix):] if b.startswith(prefix) else b.rsplit(":", 1)[-1]
                out.append("\x00" + name + "\x00")
            elif _ATOM.match(a.strip()) and a.strip() not in displays:
                out.append(" § ")
            else:
                out.append(_clean(a, displays, anchors, decl))
            i = k
            continue
        hit = None
        for w in _WRAPS:
            if s.startswith(w + "{", i):
                hit = w
                break
        if hit:
            a, j = _group(s, i + len(hit))
            whole = s[i:j]
            if whole in displays or whole == "\\mathrm{d}":
                out.append(" " + whole + " ")
            elif hit == "\\text" and _TEXT_BINDER.search(a):
                out.append(" \\forall ")  # notation binder: "P(x) for all x near a"
            else:
                out.append(" § ")
            i = j
            continue
        out.append(s[i])
        i += 1
    return "".join(out)


_BIGOPS = r"\\(?:bigcup|bigcap|sum|prod|bigsqcup|bigoplus|lim|limsup|liminf|sup|inf|max|min|iSup|iInf|coprod|bigvee|bigwedge|int|oint|esssup)"
_REL = re.compile(r"\\in\b|\\notin|\\colon|:|=|\\subseteq|\\subset|\\le(?:q)?\b|\\ge(?:q)?\b|<|>|\\to\b|\\ne(?:q)?\b|\\mid")


def _math_segments(line, state):
    """Yield math text of a cleaned line; state tracks open \\[ and align across lines."""
    segs = []
    if state.get("align"):
        if "\\end{align" in line:
            state["align"] = False
            line = line.split("\\end{align", 1)[0]
        segs.append(line)
        return segs
    if "\\begin{align" in line:
        state["align"] = True
        rest = line.split("\\begin{align", 1)[1]
        rest = rest.split("}", 1)[1] if "}" in rest else ""
        if "\\end{align" in rest:
            state["align"] = False
            rest = rest.split("\\end{align", 1)[0]
        segs.append(rest)
        line = line.split("\\begin{align", 1)[0]
    if state.get("display"):
        if "\\]" in line:
            a, line = line.split("\\]", 1)
            segs.append(a)
            state["display"] = False
        else:
            segs.append(line)
            return segs
    for m in re.finditer(r"\\\((.*?)\\\)|\\\[(.*?)(\\\]|$)", line):
        if m.group(1) is not None:
            segs.append(m.group(1))
        else:
            segs.append(m.group(2))
            if m.group(3) == "":
                state["display"] = True
    return segs


_NOT_A_USE = (r"(?![A-Za-z])"
              r"(?!_\{?\\(?:bullet|ast|star|circ)\b)(?!_\{?\*)"   # x_\bullet, x_*: another name
              r"(?!_\{?[+-])")   # f_{+}, f_{-} (fplus, fminus): another name


def _display_rx(alts):
    rx = re.compile(r"(?<![A-Za-z\\])(" + "|".join(re.escape(d) for d in alts) + r")" + _NOT_A_USE)
    return rx


_CLASS_NOTATION = re.compile(r"\^\{?(?:\d+|\\infty|p|k|q)\b\}?")   # C^{1}, L^{p}: function classes


def _paren_binders(seg):
    """Contents of every balanced "( ... )" directly followed by \\mapsto, nested parentheses in
    the type allowed: "(s : S_{1}(0)) \\mapsto"."""
    out = []
    for i, ch in enumerate(seg):
        if ch != "(":
            continue
        depth = 0
        for j in range(i, len(seg)):
            if seg[j] == "(":
                depth += 1
            elif seg[j] == ")":
                depth -= 1
                if depth == 0:
                    if re.match(r"\s*\\mapsto", seg[j + 1:]) and ":" in seg[i + 1:j]:
                        out.append(seg[i + 1:j])
                    break
    return out


def _top_split(s):
    parts, depth, cur = [], 0, ""
    for ch in s:
        if ch in "({[":
            depth += 1
        elif ch in ")}]":
            depth -= 1
        if ch == "," and depth == 0:
            parts.append(cur); cur = ""
        else:
            cur += ch
    return parts + [cur]


def _bound_tokens(seg, rx):
    """Display tokens bound inside a formula segment."""
    bound = set()
    def toks(t):
        return {m.group(1) for m in rx.finditer(t)}
    sq = re.sub(r"\\[,;:!](?![A-Za-z])", " ", seg)  # spacing macros: "\exists!\, x" has a comma
    for m in re.finditer(r"\\(?:forall|exists!?|nexists)\s*(.*?)(,|$)", sq):
        head = m.group(1)
        r = _REL.search(head)
        if r:
            bound |= toks(head[:r.start()])
            if r.group(0).startswith("\\ne"):
                t = toks(head[r.end():])
                bound |= t
        else:
            bound |= toks(head)
    for m in re.finditer(_BIGOPS + r"\s*_([A-Za-z])(?![A-Za-z])", seg):
        bound |= toks(m.group(1))   # \bigcup_k (after _sub_norm)
    for m in re.finditer(_BIGOPS + r"\s*_\{", seg):
        sub, _ = _group(seg, m.end() - 1)
        r = _REL.search(sub)
        bound |= toks(sub[:r.start()] if r else sub)
    for m in re.finditer(r"(?:\\left)?\\\{\s*(.*?)(\\mid|\\in|:|\|)", seg):
        bound |= toks(m.group(1))
    for inner in _paren_binders(seg):
        for part in _top_split(inner):
            bound |= toks(part.split(":", 1)[0])
    for m in re.finditer(r"([^\s(),]+)\s*\\mapsto", seg):
        bound |= toks(m.group(1))
    for m in re.finditer(r"\\mathrm\{d\}\s*([A-Za-z\\][A-Za-z_{}0-9]*)|\\,\s*d\s*([A-Za-z])\b", seg):
        bound |= toks(m.group(0))
    for m in re.finditer(r"\\mathrm\{d\}\s*(?:\\?[A-Za-z]+|§)\s*((?:\([^()]*\)\s*)+)", seg):
        bound |= toks(re.findall(r"\(([^()]*)\)", m.group(1))[-1])   # d mu(2)(x): the last group
    for m in re.finditer(r"\)_\{\s*([^{}]*?)\s*(?:\\in\b|=|\})", seg):
        bound |= toks(m.group(1))   # family (D^{i})_{i \in N}
    for m in re.finditer(r"\\lambda\s*(.*?)(,|\.|\\mapsto)", seg):
        bound |= toks(m.group(1))
    return bound


_LEAD = re.compile(r"^(?:\\item\s+)?(?:Let|Fix|Take|Choose|Pick|Write|Set|Consider|We take)\b")
_INLINE_INTRO = re.compile(r"(?:^|[.:⟧]\s*)(?:Let|Fix|Take|Choose|Pick|Write|Set|Consider|We take)\s*$")
_BYDEF = re.compile(r"^(?:\\item\s+)?By definition,?\s*\\\(\s*\\leanref\{")


def _claim_forall_heads(line, segs, rx):
    """A claim heading "We claim / we show \\(\\forall x ..., P\\):" opens a block in which x is
    the claim's own variable: an outer x of the same name does not introduce it (m94J05
    eq_zero_of_second_deriv_zero: statement x, claim "forall x in [a, b], w(x) = w(a)")."""
    t = re.sub(r"\s*⟦\d+⟧\s*$", "", line.strip())
    if not t.endswith(":") or not segs:
        return set()
    seg = segs[-1].replace("\x00", " ")
    out = set()
    while True:
        m = re.match(r"\s*\\forall\s+([^,]*),(?:\\\s|\\;|\\,|\s)*", seg)
        if not m:
            break
        head = m.group(1)
        r = _REL.search(head)
        out |= {x.group(1) for x in rx.finditer(head[:r.start()] if r else head)}
        seg = seg[m.end():]
    return out


def _bydef_params(raw, rx):
    """'By definition, f(k, x) = ...' unfolds a definition at its own parameters: the names in
    the first leanref's display (the left-hand side) are the definition's formal parameters for
    this line.  (Text rule: the census record does not carry which binders an unfolding uses.)"""
    m = _BYDEF.match(raw.strip())
    if not m:
        return set()
    lhs, _ = _group(raw.strip(), m.end() - 1)
    return {t.group(1) for t in rx.finditer(lhs)}


def check(row, const, constants):
    proof = row.get("proof") or ""
    if not proof.strip():
        return []
    if const is None or getattr(const, "value_expression", None) is None:
        return [{"reason": "unmeasured: no elaborated constant/value for the declaration", "anchor": ""}]
    decl = row["declaration"]
    disp = {}
    name_disp = {}  # every display a name produces: two locals (x0, x₀) may share one, and an
    for d, n in _local_displays(const):  # introduction of either introduces it
        ds = [_sub_norm(d)]
        m = re.match(r"^([A-Za-z]|\\[A-Za-z]+)(\d+)$", d)
        if m:  # name_to_latex("L0") is "L0", the page prints L_{0}
            ds.append(_sub_norm(m.group(1) + "_{" + m.group(2) + "}"))
        for x in ds:
            disp.setdefault(x, n)
            name_disp.setdefault(n, set()).add(x)
    if not disp:
        return []
    alts = sorted(disp, key=len, reverse=True)
    rx = _display_rx(alts)
    bare = _bare_constant_displays(const)
    scopes = [{_sub_norm(d) for d in _statement_displays(const)}]
    hidden = [set()]   # per scope: names a claim heading's leading forall rebinds (shadowing)
    pending = set()
    state = {}
    hits = []
    reported = set()
    for raw in proof.split("\n"):
        s = raw.strip()
        if s.startswith("\\begin{itemize}"):
            scopes.append(set())
            hidden.append(pending)
            pending = set()
            continue
        if s.startswith("\\end{itemize}"):
            if len(scopes) > 1:
                scopes.pop()
                hidden.pop()
            continue
        pending = set()
        line = _sub_norm(_clean(raw, disp, None, decl))
        # anchors: intro marker followed by its math segment
        intro = set()
        for m in re.finditer("\x00([^\x00]*)\x00", line):
            nm = m.group(1)
            ds = name_disp.get(nm) or {_sub_norm(_n2l(nm))}
            intro |= {d for d in ds if d}
        lead = _LEAD.match(line.strip())
        segs = _math_segments(line, state)
        if True:
            # anchorless Let/Take: leading tokens of segments not preceded by an anchor, on a line
            # that starts with Let/Take, or right after an inline "Take"/"Let" (sentences of a
            # flattened sub-proof: "... It suffices to show P. Take \(a = w_x\).")
            for m in re.finditer(r"(\x00[^\x00]*\x00)?\s*\\\((.*?)\\\)", line):
                if m.group(1):
                    continue
                if not lead and not _INLINE_INTRO.search(line[:m.start()]):
                    continue
                seg = m.group(2)
                r = _REL.search(seg)
                intro |= {t.group(1) for t in rx.finditer(seg[:r.start()] if r else seg)}
        before = set()
        for sc_i, hd_i in zip(scopes, hidden):
            before = (before - hd_i) | sc_i
        pending = _claim_forall_heads(line, segs, rx)
        bydef = _bydef_params(raw, rx)
        in_scope = before | intro | bydef
        for seg in segs:
            seg_clean = seg.replace("\x00", " ")
            bound = _bound_tokens(seg_clean, rx)
            for t in rx.finditer(seg_clean):
                d = t.group(1)
                if d in in_scope or d in bound:
                    continue
                if d in bare:
                    continue  # Complex.I printed as a bare i
                if d in ("C", "L") and _CLASS_NOTATION.match(seg_clean, t.end()):
                    continue  # C^{1}, L^{p} name a function class, not the local C / L
                base = re.sub(r"(?:'+|\^\{\\prime+\})$", "", d)
                if base != d and (base in in_scope or base in bound):
                    continue  # f' read as the derivative of an introduced f
                key = (d, len(scopes))
                if key in reported:
                    continue
                reported.add(key)
                hits.append({"reason": f"uses {d} (local {disp[d]}) with no introduction in scope",
                             "anchor": raw.strip()[:200]})
        scopes[-1] |= intro
    return hits
