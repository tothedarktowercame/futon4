"""T1: a printed formula denotes what the Lean term denotes (grouping, argument order, operator scope).

claude-21, RC-L1T1 (2026-11-27).  Typed criterion:

For every maximal arithmetic node of the declaration's statement and value (HAdd, HSub, HMul,
HDiv, HSMul, Neg, HPow, Inv, Function.comp; a pointwise operation applied to an argument), the
module's own printer renders the node at its binder scope.  The node is judged only when that
rendering occurs verbatim on the page (row["proof"]; the statement row["body"] gives anchor "").
The rendering is parsed back with a small precedence parser (+ - < * / juxtaposition < unary - <
^, function application, parentheses, \\frac).

T1b (claude-16's RC-T1b, 2026-11-27): the node is judged by VALUE NORMAL FORM, not tree shape.  The
term side keeps the term's arithmetic operators, each leaf read as the parse of the leaf's own
rendering; both sides are normalised the same way (+ and · flattened, unary minus and a − b as
the factor −1, a/b as a·b⁻¹, integer literals as coefficients, an integer power of a single term
expanded, a sum used as a factor kept opaque with its sign pulled out).  A node is flagged only
when the page read by ordinary precedence has a different normal form: -(a·b), (-a)·b and
-1·a·b agree; z - 0·s and (z - 0)·s do not.  Not judged: a printed group the parser cannot read
(566 nodes at q303), and a leaf that prints differently alone than inside the formula (55).

Violations:
  - grouping: the parsed operator tree differs from the term's (C24: (z - 0) * s printed z - 0 * s);
  - sign: a binary minus directly followed by a unary minus, "a - -b" (C19; this part is a rule
    about the text itself);
  - power: a power whose printed exponent differs from the rendered exponent term
    (P-swapped-power: (id ^ 2) c printed 2^{c});
  - composition: a printed f ∘ g whose operands differ from the rendered f and g (C25).

Misses (stated, not guessed): nodes whose rendering the page does not contain verbatim (the walk
renamed a binder, instantiated a variable, or printed a rewritten form); operators outside the
list (set operations, order, ∑/∫ bodies are opaque atoms); the text of ordinary leaves is not
compared (only exponents and composition operands are).
"""
from __future__ import annotations

import functools
import re
from pathlib import Path

RULE = "T1"

import atexit, sys  # noqa: E402
STATS = {"records": 0, "nodes": 0, "on_page": 0, "parsed": 0, "flagged": 0}
atexit.register(lambda: print("T1 coverage " + " ".join(f"{k}={v}" for k, v in STATS.items()), file=sys.stderr))

from mfuton.lean.blueprint_render.paper_proof_walk import term_walk as W  # noqa: E402
from mfuton.lean.blueprint_render.project_display_layer import project_display_layer, with_module_namespaces  # noqa: E402
from mfuton.lean.blueprint_render.blueprint_printer import make_blueprint_printer  # noqa: E402

E, H = W._expr, W._helpers

# op const -> (kind, arity of the unapplied operation, index of first explicit operand)
OPS = {
    "HAdd.hAdd": ("+", 6, 4), "HSub.hSub": ("-", 6, 4), "HMul.hMul": ("*", 6, 4),
    "HDiv.hDiv": ("/", 6, 4), "HSMul.hSMul": ("*", 6, 4), "HPow.hPow": ("^", 6, 4),
    "Neg.neg": ("neg", 3, 2), "Inv.inv": ("inv", 3, 2), "Function.comp": ("comp", 5, 3),
}
TRANSPARENT_SUFFIXES = (".cast", "Complex.ofReal", "NNReal.toReal", "ENNReal.ofNNReal", "Int.ofNat")


@functools.lru_cache(maxsize=4)
def _printer(module, constants):
    table = constants(module)
    layer = with_module_namespaces(project_display_layer("apm-proofs", home=Path(".").resolve()),
                                   list(table.values()))
    return make_blueprint_printer(layer, omit_binders=False, const_table=table)


# ---------------------------------------------------------------- typed side
def _op(e):
    head, args = W._spine(e)
    if E.ctor_of(head) != "const":
        return None, head, args
    name = E.const_name(head)
    return (OPS.get(name), name, args)


def _transparent(e):
    """A coercion prints as its operand (or with an arrow); read through it."""
    for _ in range(4):
        head, args = W._spine(H._strip_mdata(e))
        if E.ctor_of(head) == "const" and E.const_name(head).endswith(TRANSPARENT_SUFFIXES) and args:
            e = args[-1][0]
        else:
            break
    return H._strip_mdata(e)


def typed_tree(e):
    """('+', [(sign, t)]), ('*', [(exp, t)]), ('neg', t), ('^', b, x, xe), ('comp', f, g),
    ('app', t), or ('atom', e)."""
    e = _transparent(e)
    spec, name, args = _op(e)
    if spec is None:
        return ("atom", e)
    kind, arity, first = spec
    if len(args) > arity:          # pointwise operation applied to arguments
        return ("applied", kind, e)
    if len(args) < arity:
        return ("atom", e)
    ops = [a for a, _ in args[first:arity]]
    if kind in ("+", "-"):
        left, right = typed_tree(ops[0]), typed_tree(ops[1])
        items = _sum_items(left, 1) + _sum_items(right, 1 if kind == "+" else -1)
        return ("+", items)
    if kind in ("*", "/"):
        left, right = typed_tree(ops[0]), typed_tree(ops[1])
        items = _prod_items(left, 1) + _prod_items(right, 1 if kind == "*" else -1)
        return ("*", items)
    if kind == "neg":
        return ("neg", typed_tree(ops[0]))
    if kind == "inv":
        return ("^", typed_tree(ops[0]), ("atom", None), None)
    if kind == "^":
        return ("^", typed_tree(ops[0]), typed_tree(ops[1]), ops[1])
    if kind == "comp":
        return ("comp", ops[0], ops[1], None, e)
    return ("atom", e)


def _sum_items(t, sign):
    if t[0] == "+":
        return [(s * sign, u) for s, u in t[1]]
    return [(sign, t)]


def _prod_items(t, exp):
    if t[0] == "*":
        return [(x * exp, u) for x, u in t[1]]
    return [(exp, t)]


# ---------------------------------------------------------------- printed side
class Unparsed(Exception):
    pass


def _group(s, i):
    """s[i] == '{': return (content, index after the matching '}')."""
    d, j = 0, i
    while j < len(s):
        if s[j] == "{":
            d += 1
        elif s[j] == "}":
            d -= 1
            if d == 0:
                return s[i + 1:j], j + 1
        elif s[j] == "\\" and j + 1 < len(s):
            j += 1
        j += 1
    raise Unparsed("unbalanced brace")


def strip_leanref(s):
    out, i = [], 0
    while True:
        j = s.find("\\leanref{", i)
        if j < 0:
            out.append(s[i:])
            return "".join(out)
        out.append(s[i:j])
        x, k = _group(s, j + 8)
        if k < len(s) and s[k] == "{":
            _, k = _group(s, k)
        out.append(strip_leanref(x))
        i = k


_SKIP = re.compile(r"\\[,;:! ]|\\left|\\right|\\big[lr]?|\\Big[lr]?|\\bigg[lr]?|\s+")
_CMD_ATOM = re.compile(r"\\(operatorname|mathrm|mathbb|mathcal|text|mathbf|mathit|overline|tilde|hat|bar|widetilde|dot|vec)\b")


def tokenize(s):
    toks, i = [], 0
    while i < len(s):
        if s.startswith("\\left|", i):  # \left| … \right| is one absolute value, nesting respected
            d, k = 1, i + 6
            while d and k < len(s):
                if s.startswith("\\left|", k):
                    d, k = d + 1, k + 6
                elif s.startswith("\\right|", k):
                    d, k = d - 1, k + 7
                else:
                    k += 1
            if d:
                raise Unparsed("unclosed \\left|")
            toks.append(("atom", "|" + s[i + 6:k - 7] + "|"))
            i = k
            continue
        m = _SKIP.match(s, i)
        if m and m.end() > i:
            i = m.end()
            continue
        c = s[i]
        if c in "+-/^()":
            toks.append((c, c))
            i += 1
            continue
        if c == "{":
            x, i = _group(s, i)
            toks.append(("brace", x))
            continue
        if c == "_":  # subscript sticks to the previous atom
            i += 1
            if i < len(s) and s[i] == "{":
                x, i = _group(s, i)
            else:
                x, i = s[i], i + 1
            if not toks or toks[-1][0] != "atom":
                raise Unparsed("subscript without base")
            toks[-1] = ("atom", toks[-1][1] + "_{" + x + "}")
            continue
        if c == "'":
            if toks and toks[-1][0] == "atom":
                toks[-1] = ("atom", toks[-1][1] + "'")
                i += 1
                continue
            raise Unparsed("prime")
        if c.isdigit() or (c == "." and i + 1 < len(s) and s[i + 1].isdigit()):
            m = re.match(r"\d+(\.\d+)?", s[i:])
            toks.append(("atom", m.group()))
            i += m.end()
            continue
        if c.isalpha() or c in "αβγδεζηθικλμνξπρστυφχψωΓΔΘΛΞΠΣΦΨΩ":
            toks.append(("atom", c))
            i += 1
            continue
        if c == "\\":
            m = re.match(r"\\([A-Za-z]+)", s[i:])
            if not m:
                raise Unparsed("control symbol " + s[i:i + 2])
            cmd = m.group(1)
            i += m.end()
            if cmd in ("cdot", "times"):
                toks.append(("*", "*"))
            elif cmd == "circ":
                toks.append(("comp", "comp"))
            elif cmd in ("frac", "tfrac", "dfrac"):
                a, i = _group(s, _skipws(s, i))
                b, i = _group(s, _skipws(s, i))
                toks.append(("frac", (a, b)))
            elif cmd in ("sqrt",):
                i = _skipws(s, i)
                if i < len(s) and s[i] == "[":
                    k = s.index("]", i)
                    i = k + 1
                a, i = _group(s, _skipws(s, i))
                toks.append(("atom", "\\sqrt{" + a + "}"))
            elif _CMD_ATOM.match("\\" + cmd):
                parts = ["\\" + cmd]
                i = _skipws(s, i)
                while i < len(s) and s[i] == "{":
                    a, i = _group(s, i)
                    parts.append("{" + a + "}")
                toks.append(("atom", "".join(parts)))
            elif cmd == "langle":
                d, k = 1, i
                while d and k < len(s):
                    if s.startswith("\\langle", k):
                        d, k = d + 1, k + 7
                    elif s.startswith("\\rangle", k):
                        d, k = d - 1, k + 7
                    else:
                        k += 1
                if d:
                    raise Unparsed("unclosed langle")
                toks.append(("atom", "\\langle" + s[i:k]))
                i = k
            elif cmd in ("lvert", "lVert", "vert", "Vert", "|"):
                close = {"lvert": "\\rvert", "lVert": "\\rVert", "vert": "\\vert", "Vert": "\\Vert"}[cmd]
                k = s.find(close, i)
                if k < 0:
                    raise Unparsed("unclosed norm")
                toks.append(("atom", "\\" + cmd + s[i:k] + close))
                i = k + len(close)
            elif cmd in ("infty", "pi", "alpha", "beta", "gamma", "delta", "varepsilon", "epsilon", "zeta",
                         "eta", "theta", "vartheta", "iota", "kappa", "lambda", "mu", "nu", "xi", "rho", "sigma",
                         "tau", "upsilon", "phi", "varphi", "chi", "psi", "omega", "Gamma", "Delta", "Theta",
                         "Lambda", "Xi", "Pi", "Sigma", "Phi", "Psi", "Omega", "ell", "prime", "exp", "log",
                         "sin", "cos", "tan", "sinh", "cosh", "tanh", "arcsin", "arccos", "arctan", "ln",
                         "det", "dim", "deg", "max", "min", "gcd", "Re", "Im", "lfloor", "rfloor", "lceil", "rceil"):
                if cmd in ("lfloor", "lceil"):
                    close = "\\rfloor" if cmd == "lfloor" else "\\rceil"
                    k = s.find(close, i)
                    if k < 0:
                        raise Unparsed("unclosed floor")
                    toks.append(("atom", "\\" + cmd + s[i:k] + close))
                    i = k + len(close)
                else:
                    toks.append(("atom", "\\" + cmd))
            else:
                toks.append(("bad", "\\" + cmd))  # unknown command: an opaque token; the parser refuses it
            continue
        if c == "|" and s.startswith("|_", i) and toks and toks[-1][0] == "atom":
            # a restriction μ|_{s} sticks to its base
            j = i + 2
            if j < len(s) and s[j] == "{":
                x, j = _group(s, j)
            else:
                x, j = s[j], j + 1
            toks[-1] = ("atom", toks[-1][1] + "|_{" + x + "}")
            i = j
            continue
        if c == "|":
            k = s.find("|", i + 1)
            if k < 0:
                raise Unparsed("unclosed |")
            toks.append(("atom", s[i:k + 1]))
            i = k + 1
            continue
        toks.append(("bad", c))
        i += 1
    return toks


def _skipws(s, i):
    while i < len(s) and s[i].isspace():
        i += 1
    return i


class _Parser:
    """sum := prod (('+'|'-') prod)* ; prod := unary (('*'|'/'|juxta) unary)* ;
    unary := '-' unary | comp ; comp := pow ('comp' pow)* ; pow := post ('^' unary_or_group)? ;
    post := primary ('(' ... ')')*  (application) ."""

    def __init__(self, toks):
        self.t, self.i = toks, 0
        self.signs = []  # C19: (binary op, unary minus) adjacencies

    def peek(self):
        return self.t[self.i][0] if self.i < len(self.t) else None

    def next(self):
        tok = self.t[self.i]
        self.i += 1
        return tok

    def parse(self):
        tree = self.sum()
        if self.i != len(self.t):
            raise Unparsed("trailing " + str(self.t[self.i][0]))
        return tree

    def sum(self):
        if self.peek() == "-":  # a leading minus negates the whole first product: -a·b = -(a·b)
            self.next()
            items = _sum_items(self.prod(), -1)
        else:
            items = _sum_items(self.prod(), 1)
        while self.peek() in ("+", "-"):
            op = self.next()[0]
            if self.peek() == "-":
                self.signs.append(op)
            items += _sum_items(self.prod(), 1 if op == "+" else -1)
        return items[0][1] if len(items) == 1 and items[0][0] == 1 else ("+", items)

    def prod(self):
        items = _prod_items(self.unary(), 1)
        while self.peek() in ("*", "/", "atom", "brace", "frac"):
            if self.peek() in ("atom", "brace", "frac"):  # juxtaposition: an implicit product (T1b)
                items += _prod_items(self.unary(), 1)
                continue
            op = self.next()[0]
            items += _prod_items(self.unary(), 1 if op == "*" else -1)
        return items[0][1] if len(items) == 1 and items[0][0] == 1 else ("*", items)

    def unary(self):
        if self.peek() == "-":
            self.next()
            return ("neg", self.unary())
        return self.comp()

    def comp(self):
        parts = [self.pow()]
        while self.peek() == "comp":
            self.next()
            parts.append(self.pow())
        return parts[0] if len(parts) == 1 else ("pcomp", parts)

    def pow(self):
        base = self.post()
        if self.peek() == "^":
            self.next()
            tok = self.next()
            if tok[0] == "brace":
                exp_text = tok[1]
            elif tok[0] == "atom":
                exp_text = tok[1]
            else:
                raise Unparsed("exponent")
            return ("p^", base, _parse_text(exp_text), exp_text)
        return base

    def post(self):
        node = self.primary()
        while self.peek() == "(":
            arg = self._paren_text()
            node = ("papp", node, arg)
        return node

    def _paren_text(self):
        self.next()
        start, d = self.i, 1
        while self.i < len(self.t):
            k = self.t[self.i][0]
            if k == "(":
                d += 1
            elif k == ")":
                d -= 1
                if d == 0:
                    inner = self.t[start:self.i]
                    self.i += 1
                    return inner
            self.i += 1
        raise Unparsed("unclosed (")

    def primary(self):
        k = self.peek()
        if k == "(":
            inner = self._paren_text()
            try:
                sub = _Parser(inner)
                tree = sub.parse()
                self.signs += sub.signs
                return tree
            except Unparsed:
                return ("popaque", _detok(inner))
        if k == "atom":
            return ("patom", self.next()[1])
        if k == "brace":
            return _parse_text(self.next()[1])
        if k == "frac":
            a, b = self.next()[1]
            return ("*", _prod_items(_parse_text(a), 1) + _prod_items(_parse_text(b), -1))
        raise Unparsed("primary " + str(k))


def _detok(toks):
    return "".join(t[1] if isinstance(t[1], str) else "frac" for t in toks)


def _parse_text(s):
    try:
        return _Parser(tokenize(s)).parse()
    except Unparsed:
        return ("patom", s)


def _norm(s):
    s = _SKIP.sub("", strip_leanref(s))
    while s.startswith("(") and s.endswith(")") and _balanced(s[1:-1]):
        s = s[1:-1]
    while s.startswith("{") and s.endswith("}") and _balanced(s[1:-1], "{", "}"):
        s = s[1:-1]
    return s


def _balanced(s, o="(", c=")"):
    d = 0
    for ch in s:
        d += ch == o
        d -= ch == c
        if d < 0:
            return False
    return d == 0


# ---------------------------------------------------------------- comparison
def canon(t):
    """Normal form up to the laws the page may use silently: associativity of + and ·, and moving
    a negation out of a product or into a sum.  Atoms and powers keep their structure."""
    k = t[0]
    if k == "neg":
        return canon(("+", [(-1, t[1])]))
    if k == "+":
        items = []
        for s, u in t[1]:
            u = canon(u)
            if u[0] == "+":
                items += [(s * s2, u2) for s2, u2 in u[1]]
            else:
                items.append((s, u))
        return items[0][1] if len(items) == 1 and items[0][0] == 1 else ("+", items)
    if k == "*":
        items, sign = [], 1
        for x, u in t[1]:
            u = canon(u)
            if u[0] == "+" and len(u[1]) == 1 and u[1][0][0] == -1:
                sign, u = -sign, u[1][0][1]
            if u[0] == "*":
                items += [(x * x2, u2) for x2, u2 in u[1]]
            else:
                items.append((x, u))
        prod = items[0][1] if len(items) == 1 and items[0][0] == 1 else ("*", items)
        return prod if sign == 1 else ("+", [(-1, prod)])
    if k == "^":
        return ("^", canon(t[1]), t[2], t[3])
    if k == "p^":
        return ("p^", canon(t[1]), t[2], t[3])
    return t


def compare(t, p, render):
    """None when the printed tree p reads as the typed tree t; else a reason."""
    return _compare(canon(t), canon(p), render)


def _compare(t, p, render):
    kind = t[0]
    if p[0] in ("patom", "papp") and kind in ("+", "*", "^"):
        return None  # the printer shows a name for the value (a let alias, a definition): not judged
    if kind in ("atom", "applied"):
        if kind == "applied" and t[1] == "^":
            return _check_applied_pow(t[2], p, render)
        return None
    if kind == "+":
        if p[0] != "+":
            return f"a sum/difference of {len(t[1])} terms is printed as {_shape(p)}"
        if len(p[1]) != len(t[1]):
            if _expanded_len(t, render) == len(p[1]):
                return None  # an operand whose own rendering is a sum shows its terms in line
            return f"a sum/difference of {len(t[1])} terms is printed with {len(p[1])} terms"
        for (st, ut), (sp, up) in zip(t[1], p[1]):
            if st != sp:
                return "a summand's sign differs between term and page"
            r = _compare(ut, up, render)
            if r:
                return r
        return None
    if kind == "*":
        if p[0] != "*":
            return f"a product/quotient of {len(t[1])} factors is printed as {_shape(p)}"
        if len(p[1]) != len(t[1]):
            if _expanded_len(t, render) == len(p[1]):
                return None  # an operand whose own rendering is a product shows its factors in line
            return f"a product/quotient of {len(t[1])} factors is printed with {len(p[1])} factors"
        for (xt, ut), (xp, up) in zip(t[1], p[1]):
            if xt != xp:
                return "a factor is a numerator in the term but a denominator on the page (or the reverse)"
            r = _compare(ut, up, render)
            if r:
                return r
        return None
    if kind == "neg":
        if p[0] == "neg":
            return compare(t[1], p[1], render)
        if p[0] == "+" and len(p[1]) == 1 and p[1][0][0] == -1:
            return compare(t[1], p[1][0][1], render)
        return f"a negation is printed as {_shape(p)}"
    if kind == "^":
        if p[0] != "p^":
            return f"a power is printed as {_shape(p)}"
        r = _compare(t[1], p[1], render)
        if r:
            return r
        if t[3] is not None and t[2][0] != "atom":
            return _compare(canon(t[2]), canon(p[2]), render)
        if t[3] is not None:
            want = _norm(render(t[3]))
            if want and _norm(p[3]) != want and p[2][0] in ("patom", "papp"):
                return f"a power's exponent is printed {_norm(p[3])!r}, the term's exponent is {want!r}"
        return None
    if kind == "comp":
        if p[0] != "pcomp" or len(p[1]) != 2:
            return None  # printed applied/unfolded: not judged
        f, g = _norm(render(t[1])), _norm(render(t[2]))
        parts = _split_top(strip_leanref(render(t[4])), "\\circ")
        if len(parts) != 2:
            return None
        pf, pg = (_norm(x) for x in parts)
        if (pf, pg) != (f, g):
            return f"a composition f ∘ g is printed with operands {pf!r} ∘ {pg!r}; the term's are {f!r} ∘ {g!r}"
        return None
    return None


def _expanded_len(t, render):
    """Length of t's chain when each atom operand is counted by its own rendering's chain."""
    n = 0
    for _, u in t[1]:
        expr = u[1] if u[0] == "atom" else (u[2] if u[0] == "applied" else None)
        if expr is None:
            n += 1
            continue
        try:
            q = canon(_Parser(tokenize(strip_leanref(render(expr)))).parse())
        except Exception:
            n += 1
            continue
        n += len(q[1]) if q[0] == t[0] else 1
    return n


def _check_applied_pow(e, p, render):
    head, args = W._spine(e)
    exp_text = _norm(render(args[5][0]))
    exps = []

    def collect(q):
        if q[0] == "p^":
            exps.append(_norm(q[3]))
        if q[0] == "papp":
            collect(q[1])

    collect(p)
    if not exps:
        return None  # printed in another form (e.g. unfolded); not judged
    if exp_text and exp_text not in exps:
        return (f"a pointwise power applied to an argument is printed with exponent {exps[0]!r}; "
                f"the term's exponent is {exp_text!r}")
    return None


def _split_top(s, sep):
    out, d, last, i = [], 0, 0, 0
    while i < len(s):
        ch = s[i]
        if ch in "({[":
            d += 1
        elif ch in ")}]":
            d -= 1
        elif d == 0 and s.startswith(sep, i):
            out.append(s[last:i])
            i += len(sep)
            last = i
            continue
        i += 1
    out.append(s[last:])
    return out


def _unparse(q):
    if q[0] == "patom":
        return q[1]
    return "\x00"  # a structured operand never equals a rendered f/g text


def _shape(p):
    return {"+": "a sum", "*": "a product", "neg": "a negation", "p^": "a power", "papp": "an application",
            "patom": "an atom", "pcomp": "a composition"}.get(p[0], p[0])


# ---------------------------------------------------------------- T1b: value normal form
from fractions import Fraction  # noqa: E402

_INT = re.compile(r"^\d+$")


def _parse_rendering(text):
    try:
        return _Parser(tokenize(strip_leanref(text))).parse()
    except Exception:
        return ("patom", _norm(text))


class _LeafOffPage(Exception):
    pass


def _leaf(e, render, whole):
    text = render(e)
    if whole is not None and _norm(_text_key(text)) not in whole:
        # the leaf prints differently alone than inside the formula: no reading to compare
        raise _LeafOffPage(text)
    return _parse_rendering(text)


def conv(e, render, fold=False, root=True, whole=None):
    """The Lean term as a printed-tree: its arithmetic operators from the term, each leaf as the parse
    of the leaf's own rendering (so only the operator structure is compared).  With `fold`, a
    structured subterm the printer shows as a name or an application is a leaf (a let alias)."""
    inner = _transparent(e)
    spec, name, args = _op(inner)
    if spec is None or len(args) != spec[1] or spec[0] == "comp":
        # a leaf is read as the printer shows it: a coercion applied to arguments (`⇑a x`)
        # is not its last argument
        return _leaf(e, render, whole)
    if inner is not e and _norm(_text_key(render(e))) != _norm(_text_key(render(inner))):
        # a coercion is read through only when it prints as its operand (a cast); a coerced
        # function applied to an argument (`⇑f (x + ε)`) prints as an application: a leaf
        return _leaf(e, render, whole)
    e = inner
    if fold and not root:
        shown = _parse_rendering(render(e))
        if shown[0] in ("patom", "papp"):
            return shown
    kind, arity, first = spec
    ops = [a for a, _ in args[first:arity]]
    sub = lambda x: conv(x, render, fold, False, whole)  # noqa: E731
    if kind in ("+", "-"):
        return ("+", [(1, sub(ops[0])), (1 if kind == "+" else -1, sub(ops[1]))])
    if kind in ("*", "/"):
        return ("*", [(1, sub(ops[0])), (1 if kind == "*" else -1, sub(ops[1]))])
    if kind == "neg":
        return ("neg", sub(ops[0]))
    if kind == "inv":
        return ("p^", sub(ops[0]), ("neg", ("patom", "1")), None)
    if kind == "^":
        return ("p^", sub(ops[0]), sub(ops[1]), None)
    return _parse_rendering(render(e))


def _key(f):
    return tuple(sorted(((tuple(sorted(m, key=repr)), str(Fraction(c))) for m, c in f.items() if c != 0), key=repr))


def _text_key(t):
    t = _SKIP.sub("", strip_leanref(t))
    return re.sub(r"\\[td]frac", r"\\frac", t)


def _has_opaque(p):
    if not isinstance(p, tuple) or not p:
        return False
    if p[0] == "popaque":
        return True
    return any(_has_opaque(x) for x in p[1:] if isinstance(x, tuple)) or any(
        _has_opaque(u) for x in p[1:] if isinstance(x, list) for item in x
        if isinstance(item, tuple) and len(item) == 2 for u in (item[1],))


def _leaf_key(p):
    if p[0] in ("patom", "popaque"):
        return ("A", _text_key(p[1]))
    if p[0] == "papp":
        return ("F", _key(nf(p[1])), _text_key(_detok(p[2])))
    if p[0] == "pcomp":
        return ("C", tuple(_key(nf(x)) for x in p[1]))
    return ("?", repr(p))


def _single(f):
    items = [(m, c) for m, c in f.items() if c != 0]
    return items[0] if len(items) == 1 else None


def _mono(m):
    return frozenset(m.items())


def _mul(f, g):
    """Product of two single-term forms (callers keep products single-term)."""
    (m1, c1), (m2, c2) = _single(f) or (frozenset(), 0), _single(g) or (frozenset(), 0)
    d = dict(m1)
    for k, x in m2:
        d[k] = d.get(k, 0) + x
        if d[k] == 0:
            del d[k]
    return {_mono(d): c1 * c2}


def _as_factor(f, exp):
    """f as a factor with exponent exp (±1, or an integer power)."""
    s = _single(f)
    if s is not None and (exp > 0 or s[1] != 0):
        m, c = s
        return {_mono({k: x * exp for k, x in m}): Fraction(c) ** exp}
    if not f or all(c == 0 for c in f.values()):
        return {frozenset(): 0}
    # a sum as a factor: its sign is pulled out, so -(a - b)·x and (b - a)·x and -((a - b)·x) agree
    lead = sorted(((tuple(sorted(m, key=repr)), c) for m, c in f.items() if c != 0), key=lambda mc: repr(mc[0]))[0][1]
    if lead < 0:
        return {_mono({("S", _key({m: -c for m, c in f.items()})): exp}): Fraction(-1) ** exp}
    return {_mono({("S", _key(f)): exp}): 1}


def nf(p):
    """Value normal form: {monomial: coefficient}, + and · flattened, unary minus and a − b as the
    factor −1, a/b as a·b⁻¹, integer literals as coefficients, (u)ⁿ with a single-term u expanded.
    A sum of several terms inside a product is an opaque factor (no distribution)."""
    k = p[0]
    if k == "+":
        out = {}
        for s, u in p[1]:
            for m, c in nf(u).items():
                out[m] = out.get(m, 0) + s * c
        return {m: c for m, c in out.items() if c != 0} or {frozenset(): 0}
    if k == "neg":
        return {m: -c for m, c in nf(p[1]).items()}
    if k == "*":
        acc = {frozenset(): 1}
        for x, u in p[1]:
            acc = _mul(acc, _as_factor(nf(u), x))
        return acc
    if k == "p^":
        base, ex = nf(p[1]), nf(p[2])
        se = _single(ex)
        if se is not None and not se[0] and Fraction(se[1]).denominator == 1 and int(se[1]) != 0:
            return _as_factor(base, int(se[1]))
        return {_mono({("P", _key(base), _key(ex)): 1}): 1}
    if k == "patom" and p[1].strip().startswith("-") and len(p[1].strip()) > 1:
        # an unparsed group keeps its leading unary minus as text: read it as the factor −1
        return {m: -c for m, c in nf(("patom", p[1].strip()[1:].strip())).items()}
    if k == "patom" and _INT.match(p[1].strip()):
        return {frozenset(): int(p[1].strip())}
    return {_mono({_leaf_key(p): 1}): 1}


def _show(f):
    return str(len(f)) + " term(s)"


def judge(e, p, render):
    """T1b: a pointwise power applied to an argument and a composition keep their text checks (the
    exponent, the operands); every other arithmetic node is compared by value normal form."""
    tt = typed_tree(e)
    if tt[0] == "applied" and tt[1] == "^":
        return _check_applied_pow(tt[2], p, render)
    if tt[0] == "comp":
        return compare(tt, p, render)
    return t1_compare(e, p, render)


def t1_compare(e, p, render):
    """None when the page, read by ordinary precedence, has the term's value normal form."""
    if _has_opaque(p):
        STATS["opaque"] = STATS.get("opaque", 0) + 1
        return "unmeasured: a printed group the parser cannot read"
    page = _key(nf(p))
    whole = _text_key(render(e))
    try:
        if _key(nf(conv(e, render, whole=whole))) == page:
            return None
        if _key(nf(conv(e, render, fold=True, whole=whole))) == page:
            return None
    except _LeafOffPage:
        STATS["leaf_off_page"] = STATS.get("leaf_off_page", 0) + 1
        return "unmeasured: a Lean leaf prints differently alone than inside the formula"
    return "read by ordinary precedence, the printed formula is a different expression than the term"


# ---------------------------------------------------------------- traversal
def _nodes(e, names, out, seen, budget):
    """Maximal arithmetic nodes of e with the binder names in scope."""
    stack = [(e, names)]
    while stack and len(out) < budget:
        e, names = stack.pop()
        if e is None or id(e) in seen:
            continue
        seen.add(id(e))
        e = H._strip_mdata(e)
        k = E.ctor_of(e)
        if k in ("lam", "forallE"):
            stack.append((E.binder_body(e), names + [E.binder_name(e) or "x"]))
            stack.append((E.binder_domain(e), names))
            continue
        if k == "letE":
            stack.append((E.binder_body(e), names + [E.binder_name(e) or "x"]))
            stack.append((E.let_value(e), names))
            stack.append((E.binder_domain(e), names))
            continue
        if k != "app":
            continue
        spec, name, args = _op(e)
        if spec is not None and len(args) >= spec[1]:
            out.append((e, names))
            # also the operands of a pointwise application's arguments, and lambdas inside operands
            continue
        for a, _ in args:
            stack.append((a, names))
        if E.ctor_of(W._spine(e)[0]) != "const":
            stack.append((W._spine(e)[0], names))
    return out


def check(row, const, constants):
    proof, body = row.get("proof") or "", row.get("body") or ""
    if const is None:
        return [{"reason": "unmeasured: no elaborated constant", "anchor": ""}]
    printer = _printer(row["module"], constants)
    nodes = []
    seen = set()
    _nodes(const.type_expression, [], nodes, seen, 400)
    if const.value_expression is not None:
        _nodes(const.value_expression, [], nodes, seen, 400)
    hits, done = [], set()
    STATS["records"] += 1
    STATS["nodes"] += len(nodes)

    for e, names in nodes:
        def render(x, names=names):
            printer.binders = list(names)
            return printer.render(x)
        try:
            tex = render(e)
        except Exception:
            continue
        if not tex or tex in done:
            continue
        done.add(tex)
        where = tex if tex in proof else ("" if tex in body else None)
        if where is None:
            continue
        STATS["on_page"] += 1
        try:
            parser = _Parser(tokenize(strip_leanref(tex)))
            p = parser.parse()
        except Unparsed:
            hits.append({"reason": "unmeasured: the page formula does not parse", "anchor": tex})
            continue
        except Exception as ex:
            hits.append({"reason": f"unmeasured: parser raised {type(ex).__name__}", "anchor": tex})
            continue
        STATS["parsed"] += 1
        if "-" in parser.signs:
            hits.append({"reason": "a minus sign directly before a negation without parentheses", "anchor": where})
            continue
        try:
            reason = judge(e, p, render)
        except Exception as ex:
            reason = f"unmeasured: judge raised {type(ex).__name__}"
        if reason:
            # an unjudged node is anchored by its own rendering, so statement nodes are told apart
            hits.append({"reason": reason, "anchor": tex if reason.startswith("unmeasured") else where})
    printer.binders = []
    STATS["flagged"] += sum(not h["reason"].startswith("unmeasured") for h in hits)
    return hits
