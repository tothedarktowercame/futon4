"""T2 census check (claude-20, 2026-11-27): no fact on the page holds only by a Lean convention.

Facts examined (typed): the declaration's statement (type_expression) and every `have`/`let`
binder type in the proof term (letE nodes of value_expression), each with the binder context
it lives in (the statement's foralls, the proof's lambdas and earlier haves).

A convention SITE inside a fact is one of
  x / d        (HDiv.hDiv, any carrier)   convention point d = 0
  d⁻¹          (Inv.inv)                  d = 0
  Real.log d                              d ≤ 0  (log 0 = 0, log x = log |x|)
  Real.sqrt d                             d < 0
  a - b on ℕ   (HSub.hSub Nat)            a < b
A site is a VIOLATION when the fact reaches the convention point:
  * the argument is a literal at the point (1/0, 0⁻¹, log 0, √(-1), 2 - 3), or
  * it vanishes (resp. is negative / a < b) when the summation/series index variables of the
    fact take the value 0 (λ given to tsum, Summable, HasSum, Multipliable, tprod, or to
    Finset.sum/prod over range k (k ≠ 0) or an interval whose lower end is 0),
and no hypothesis in context excludes the point (t ≠ 0, ¬ t = 0, 0 < t, t > 0, t < 0 for /, ⁻¹;
0 < t for log; 0 ≤ t / 0 < t for √, or t = -s with s < 0 / s ≤ 0; b ≤ a / b < a for a - b).
A site whose argument cannot be decided (a free quantity with no such hypothesis) makes the
record UNMEASURED unless the record also has a violation.

A convention LEMMA used in the proof term (division, inverse, log, sqrt lemmas in LEMMAS) is
also a violation: the Lean relies on the convention.  A truncated-subtraction lemma
(Nat.sub_eq_zero_of_le, tsub_eq_zero_of_le, ..._iff_le) makes the record unmeasured instead:
in m95J03 every such use is simp evaluating 0 - 1 inside the dead branch of an `if j ≤ i`, which
the page does not reach; n - n = 0 (both sides the same term) is exempt.
"""
import collections

from mfuton.lean.latex_pp import _expr as X

RULE = "T2"

#: convention lemmas, with why each states a convention value.
LEMMAS = {
    "div_zero": "a / 0 = 0",
    "zero_div_zero": "0 / 0 = 0",
    "Nat.div_zero": "n / 0 = 0 on ℕ",
    "Int.ediv_zero": "a / 0 = 0 on ℤ",
    "Int.div_zero": "a / 0 = 0 on ℤ",
    "EuclideanDomain.div_zero": "a / 0 = 0",
    "div_eq_zero_iff": "a / b = 0 ↔ a = 0 ∨ b = 0: the b = 0 disjunct is the convention",
    "inv_zero": "0⁻¹ = 0",
    "inv_eq_zero": "a⁻¹ = 0 ↔ a = 0: only by 0⁻¹ = 0",
    "zero_inv": "0⁻¹ = 0 (alias)",
    "Real.log_zero": "log 0 = 0",
    "Real.log_neg_eq_log": "log (-x) = log x",
    "Real.sqrt_eq_zero_of_nonpos": "x ≤ 0 → √x = 0",
    "Real.sqrt_eq_zero'": "√x = 0 ↔ x ≤ 0",
    "Nat.sub_eq_zero_of_le": "n ≤ m → n - m = 0 (truncated, unless n = m)",
    "Nat.sub_eq_zero_iff_le": "n - m = 0 ↔ n ≤ m (truncated)",
    "tsub_eq_zero_of_le": "a ≤ b → a - b = 0 (truncated, unless a = b)",
    "tsub_eq_zero_iff_le": "a - b = 0 ↔ a ≤ b (truncated)",
}
# Real.log_abs (log |x| = log x) is not listed: used left to right (simp normal form) it turns the
# ordinary log |x| into Lean's log x, an unprinted intermediate; the printed facts are judged as sites.
# A lemma use is exempt when its result only feeds a refutation (the enclosing term proves False)
# or the disjunction it yields is resolved against the convention disjunct.
_REFUTE_HEADS = {"False.elim", "absurd", "False.rec", "False.casesOn", "Not.elim", "Classical.byContradiction",
                 "Decidable.byContradiction", "Classical.by_contradiction", "by_contra", "Not.intro"}
_RESOLVED = {("Or.resolve_right", "div_eq_zero_iff"), ("Or.casesOn", "div_eq_zero_iff"), ("Or.elim", "div_eq_zero_iff")}  # a / b = 0 → a = 0 ∨ b = 0, b = 0 excluded
_SUB_EXEMPT_IF_EQUAL = {"Nat.sub_eq_zero_of_le", "tsub_eq_zero_of_le"}
_SUB_LEMMAS = {"Nat.sub_eq_zero_of_le", "tsub_eq_zero_of_le", "Nat.sub_eq_zero_iff_le", "tsub_eq_zero_iff_le"}

_SERIES = {"tsum", "Summable", "HasSum", "Multipliable", "HasProd", "tprod"}
_FINSET = {"Finset.sum", "Finset.prod"}
_POSITIVE_CONSTS = {"Real.pi"}
_POSITIVE_HEADS = {"Real.exp", "Real.cosh", "Nat.factorial", "Nat.succ", "Real.sinh_pos"}
_NONNEG_HEADS = {"abs", "Norm.norm", "Real.sqrt", "Dist.dist", "NNReal.toReal", "Real.exp",
                 "Nat.cast", "ENNReal.toReal", "nnnorm"}

UNKNOWN = object()
_SEEN = set()
import sys as _sys
_sys.setrecursionlimit(max(_sys.getrecursionlimit(), 20000))


def _strip(e):
    while X.ctor_of(e) == "mdata":
        e = X.mdata_child(e)
    return e


def _spine(e):
    e = _strip(e)
    if X.ctor_of(e) != "app":
        return e, []
    h, a = X.app_spine(e)
    return _strip(h), [_strip(x) for x, _ in a]


def _head(e):
    h, a = _spine(e)
    return (X.const_name(h) if X.ctor_of(h) == "const" else None), a


def _num(e):
    """Integer value of a numeral, or None."""
    e = _strip(e)
    k = X.ctor_of(e)
    if k == "lit":
        v = X.lit_value(e)
        return v if isinstance(v, int) else None
    n, a = _head(e)
    if n == "OfNat.ofNat" and len(a) >= 2:
        return _num(a[1])
    if n in ("Zero.zero",):
        return 0
    if n in ("One.one",):
        return 1
    if n in ("Nat.cast", "Int.cast", "NatCast.natCast", "IntCast.intCast") and a:
        return _num(a[-1])
    if n == "Neg.neg" and a:
        v = _num(a[-1])
        return -v if v is not None else None
    return None


def canon(e, depth):
    """Structural key with bound variables as absolute binder levels, numerals normalised."""
    e = _strip(e)
    v = _num(e)
    if v is not None:
        return ("num", v)
    k = X.ctor_of(e)
    if k == "bvar":
        return ("lvl", depth - 1 - X.bvar_index(e))
    if k == "const":
        return ("c", X.const_name(e))
    if k == "fvar":
        return ("f", X.var_name(e))
    if k == "app":
        h, a = _spine(e)
        n = X.const_name(h) if X.ctor_of(h) == "const" else None
        if n in ("Nat.cast", "Int.cast", "NatCast.natCast"):  # a cast is the same quantity
            return canon(a[-1], depth)
        return ("app", canon(h, depth), tuple(canon(x, depth) for x in a))
    if k in ("lam", "forallE"):
        return (k, canon(X.binder_domain(e), depth), canon(X.binder_body(e), depth + 1))
    if k == "lit":
        return ("lit", str(X.lit_value(e)))
    if k == "proj":
        return ("proj", X.proj_field_index(e), canon(X.proj_struct(e), depth))
    return (k, id(e))


class Ctx:
    """Binder stack: entries (domain, depth_of_domain, role, facts learned from the domain)."""

    def __init__(self):
        self.stack = []

    def push(self, domain, role=None):
        depth = len(self.stack)
        facts = collections.defaultdict(set)
        self._facts = facts
        self._learn(domain, depth)
        self.stack.append((domain, depth, role, facts))

    def pop(self):
        self.stack.pop()

    @property
    def facts(self):
        merged = collections.defaultdict(set)
        for entry in self.stack:
            for kind, keys in entry[3].items():
                merged[kind] |= keys
        return merged

    def _learn(self, t, depth):
        n, a = _head(t)
        if n == "And" and len(a) == 2:
            self._learn(a[0], depth)
            self._learn(a[1], depth)
            return
        if n == "Membership.mem" and len(a) == 5:
            self._learn_mem(a[3], a[4], depth)
            return
        if n == "Ne" and len(a) == 3:
            self._facts["ne"].add((canon(a[1], depth), canon(a[2], depth)))
            if _num(a[2]) == 0:
                self._facts["nz"].add(canon(a[1], depth))
            if _num(a[1]) == 0:
                self._facts["nz"].add(canon(a[2], depth))
        elif n == "Not" and len(a) == 1:
            m, b = _head(a[0])
            if m == "Eq" and len(b) == 3:
                if _num(b[2]) == 0:
                    self._facts["nz"].add(canon(b[1], depth))
                if _num(b[1]) == 0:
                    self._facts["nz"].add(canon(b[2], depth))
        elif n in ("LT.lt", "GT.gt", "LE.le", "GE.ge") and len(a) == 4:
            lo, hi = (a[2], a[3]) if n in ("LT.lt", "LE.le") else (a[3], a[2])
            strict = n in ("LT.lt", "GT.gt")
            self._facts["le"].add((canon(lo, depth), canon(hi, depth)))
            if _num(lo) == 0:
                self._facts["nn"].add(canon(hi, depth))
                if strict:
                    self._facts["pos"].add(canon(hi, depth))
                    self._facts["nz"].add(canon(hi, depth))
            elif _num(lo) is not None and _num(lo) > 0:
                self._facts["nn"].add(canon(hi, depth))
                self._facts["pos"].add(canon(hi, depth))
                self._facts["nz"].add(canon(hi, depth))
            if _num(hi) == 0:
                self._facts["np"].add(canon(lo, depth))
                if strict:
                    self._facts["nz"].add(canon(lo, depth))
                    self._facts["neg"].add(canon(lo, depth))

    def _learn_mem(self, st, x, depth):
        """x ∈ Ioi/Ioo/Ioc a (a ≥ 0), Ici/Icc/Ico a (a > 0 → pos, a = 0 → nn), Iio/Iic b (b ≤ 0 / b < 0 → neg)."""
        m, b = _head(st)
        if m is None:
            return
        kind = m.rsplit(".", 1)[-1]
        if not (m.startswith("Set.") or m.startswith("Finset.")) or len(b) < 1:
            return
        cx = canon(x, depth)
        if kind in ("Ioi", "Ioo", "Ioc", "Ici", "Icc", "Ico"):
            lo = _num(b[-2] if kind in ("Ioo", "Ioc", "Icc", "Ico") else b[-1])
            if lo is None:
                return
            if lo > 0 or (lo == 0 and kind in ("Ioi", "Ioo", "Ioc")):
                for f in ("pos", "nz", "nn"):
                    self._facts[f].add(cx)
            elif lo == 0:
                self._facts["nn"].add(cx)
        elif kind in ("Iio", "Iic"):
            hi = _num(b[-1])
            if hi is not None and (hi < 0 or (hi == 0 and kind == "Iio")):
                for f in ("neg", "nz", "np"):
                    self._facts[f].add(cx)

    def role(self, e, depth):
        """Role of a bvar's binder ('index0' = a summation index ranging over 0)."""
        e = _strip(e)
        if X.ctor_of(e) != "bvar":
            return None
        lvl = depth - 1 - X.bvar_index(e)
        if 0 <= lvl < len(self.stack):
            return self.stack[lvl][2]
        return None

    def has(self, kind, e, depth):
        return canon(e, depth) in self.facts[kind]


def _value_at_zero(e, ctx, depth):
    """Value of e when every summation index ranging over 0 is 0: an int, 'pos' (> 0),
    'nn' (≥ 0), 'nz' (≠ 0) or UNKNOWN.  Only decides what it can prove."""
    e = _strip(e)
    v = _num(e)
    if v is not None:
        return v
    k = X.ctor_of(e)
    if k == "bvar":
        if ctx.role(e, depth) == "index0":
            return 0
        if ctx.role(e, depth) == "index1":
            return "pos"
        return _from_hyp(e, ctx, depth)
    if k == "const":
        return "pos" if X.const_name(e) in _POSITIVE_CONSTS else UNKNOWN
    if k != "app":
        return _from_hyp(e, ctx, depth)
    n, a = _head(e)
    if n in ("Nat.cast", "Int.cast", "NatCast.natCast", "IntCast.intCast", "NNReal.toReal") and a:
        r = _value_at_zero(a[-1], ctx, depth)
        return r if r is not UNKNOWN else ("nn" if n == "Nat.cast" else UNKNOWN)
    if n == "HPow.hPow" and len(a) == 6:
        b, ex = _value_at_zero(a[4], ctx, depth), _num(a[5])
        if ex == 0:
            return 1
        if isinstance(b, int) and ex is not None and ex > 0:
            return b ** ex
        if b in ("pos",) :
            return "pos"
        if b == "nz":
            return "pos" if ex is not None and ex % 2 == 0 else "nz"
        if ex is not None and ex > 0 and ex % 2 == 0:
            return "nn"
        return UNKNOWN
    if n == "HMul.hMul" and len(a) == 6:
        x, y = _value_at_zero(a[4], ctx, depth), _value_at_zero(a[5], ctx, depth)
        if x == 0 or y == 0:
            return 0
        if isinstance(x, int) and isinstance(y, int):
            return x * y
        if _isp(x) and _isp(y):
            return "pos"
        if _isnz(x) and _isnz(y):
            return "nz"
        return UNKNOWN
    if n == "HAdd.hAdd" and len(a) == 6:
        x, y = _value_at_zero(a[4], ctx, depth), _value_at_zero(a[5], ctx, depth)
        if isinstance(x, int) and isinstance(y, int):
            return x + y
        if (_isp(x) and _isnn(y)) or (_isnn(x) and _isp(y)):
            return "pos"
        if _isnn(x) and _isnn(y):
            return "nn"
        return _from_hyp(e, ctx, depth)
    if n == "HSub.hSub" and len(a) == 6:
        x, y = _value_at_zero(a[4], ctx, depth), _value_at_zero(a[5], ctx, depth)
        if isinstance(x, int) and isinstance(y, int):
            r = x - y
            return max(r, 0) if _is_nat(a[0]) else r
        if not _is_nat(a[0]):
            p = (canon(a[4], depth), canon(a[5], depth))
            if p in ctx.facts["ne"] or (p[1], p[0]) in ctx.facts["ne"]:
                return "nz"
        return _from_hyp(e, ctx, depth)
    if n == "Neg.neg" and a:
        x = _value_at_zero(a[-1], ctx, depth)
        if isinstance(x, int):
            return -x
        if x == "nz":
            return "nz"
        if ctx.has("neg", a[-1], depth):
            return "pos"
        if ctx.has("np", a[-1], depth):
            return "nn"
        return _from_hyp(e, ctx, depth)
    if n == "Nat.succ":
        return "pos"
    if n in _POSITIVE_HEADS:
        return "pos"
    if n in _NONNEG_HEADS:
        return _from_hyp(e, ctx, depth, default="nn")
    return _from_hyp(e, ctx, depth)


def _from_hyp(e, ctx, depth, default=UNKNOWN):
    if ctx.has("pos", e, depth):
        return "pos"
    if ctx.has("nz", e, depth):
        return "nz"
    if ctx.has("nn", e, depth):
        return "nn"
    return default


def _isp(x):
    return x == "pos" or (isinstance(x, int) and x > 0)


def _isnn(x):
    return x in ("pos", "nn") or (isinstance(x, int) and x >= 0)


def _isnz(x):
    return x in ("pos", "nz") or (isinstance(x, int) and x != 0)


def _is_nat(t):
    t = _strip(t)
    return X.ctor_of(t) == "const" and X.const_name(t) == "Nat"


def _finset_role(s, ctx, depth):
    """'index0' if the Finset contains 0, 'index1' if it provably does not, None if unknown."""
    n, a = _head(s)
    if n == "Finset.range" and a:
        k = _num(a[-1])
        return "index0" if (k is None or k > 0) else "index1"
    if n in ("Finset.Icc", "Finset.Ico", "Finset.Iic", "Finset.Iio") and len(a) >= 2:
        lo = _num(a[-2]) if n in ("Finset.Icc", "Finset.Ico") else 0
        return "index0" if lo == 0 else ("index1" if lo is not None and lo > 0 else None)
    if n in ("Finset.Ioc", "Finset.Ioo") and len(a) >= 2:
        lo = _num(a[-2])
        return "index1" if lo is not None and lo >= 0 else None
    return None


def _sites(fact, ctx, depth, out):
    """Collect (verdict, what) for every convention site in a fact."""
    e = _strip(fact)
    k = X.ctor_of(e)
    if k in ("lam", "forallE"):
        _sites(X.binder_domain(e), ctx, depth, out)
        ctx.push(X.binder_domain(e))
        _sites(X.binder_body(e), ctx, depth + 1, out)
        ctx.pop()
        return
    if k == "letE":
        for ch in e.arguments[:2]:
            _sites(ch, ctx, depth, out)
        ctx.push(X.binder_domain(e))
        _sites(X.binder_body(e), ctx, depth + 1, out)
        ctx.pop()
        return
    if k != "app":
        return
    n, a = _head(e)
    # summation binders: give the index its range role
    lam_role = {}
    if n in _SERIES and a:
        for i, x in enumerate(a):
            if X.ctor_of(x) == "lam" and _num_type(X.binder_domain(x)):
                lam_role[i] = "index0"
    elif n in _FINSET and len(a) >= 5:
        role = _finset_role(a[3], ctx, depth)
        if X.ctor_of(a[4]) == "lam":
            lam_role[4] = role
    _judge(n, a, ctx, depth, out)
    h, _ = _spine(e)
    if X.ctor_of(h) != "const":
        _sites(h, ctx, depth, out)
    for i, x in enumerate(a):
        if i in lam_role and lam_role[i] and X.ctor_of(x) == "lam":
            _sites(X.binder_domain(x), ctx, depth, out)
            ctx.push(X.binder_domain(x), role=lam_role[i])
            _sites(X.binder_body(x), ctx, depth + 1, out)
            ctx.pop()
        else:
            _sites(x, ctx, depth, out)


def _num_type(t):
    t = _strip(t)
    return X.ctor_of(t) == "const" and X.const_name(t) in ("Nat", "Int")


_FIELD_TYPES = {"Real", "Complex", "Rat", "NNReal", "ENNReal", "EReal"}
_FIELD_MARKS = ("Field", "DivisionRing", "GroupWithZero", "DivisionSemiring", "Matrix")


def _names(e, out, cap=200):
    stack = [e]
    while stack and len(out) < cap:
        x = _strip(stack.pop())
        if X.ctor_of(x) == "const":
            out.append(X.const_name(x))
        elif X.ctor_of(x) == "app":
            h, a = _spine(x)
            stack.append(h)
            stack.extend(a)
    return out


def _group_op(ty, inst):
    """Inverse/division in a group (FreeGroup, a quotient group, Circle): total, no junk value."""
    names = _names(ty, [])
    if names and names[0] in _FIELD_TYPES:
        return False
    names = _names(inst, names)
    text = " ".join(names)
    if any(m in text for m in _FIELD_MARKS) or any(t in names for t in _FIELD_TYPES):
        return False
    return "Group" in text


def _judge(n, a, ctx, depth, out):
    if n in ("HDiv.hDiv", "Inv.inv") and a and _group_op(a[0], a[3] if n == "HDiv.hDiv" and len(a) == 6 else a[1] if len(a) > 1 else a[0]):
        return
    if n == "HDiv.hDiv" and len(a) == 6:
        _zero_site(a[5], ctx, depth, out, "x/0")
    elif n == "Inv.inv" and len(a) == 3:
        _zero_site(a[2], ctx, depth, out, "0⁻¹")
    elif n == "Real.log" and len(a) == 1:
        v = _value_at_zero(a[0], ctx, depth)
        if isinstance(v, int) and v <= 0:
            out.append(("violation", "log of %d" % v))
        elif not _isp(v):
            out.append(("unknown", "log of an undecided argument"))
    elif n == "Real.sqrt" and len(a) == 1:
        v = _value_at_zero(a[0], ctx, depth)
        if isinstance(v, int) and v < 0:
            out.append(("violation", "√ of %d" % v))
        elif not _isnn(v):
            out.append(("unknown", "√ of an undecided argument"))
    elif n == "HSub.hSub" and len(a) == 6 and _is_nat(a[0]):
        x, y = a[4], a[5]
        if canon(x, depth) == canon(y, depth) or _num(y) == 0:
            return
        if (canon(y, depth), canon(x, depth)) in ctx.facts["le"]:
            return
        vx, vy = _value_at_zero(x, ctx, depth), _value_at_zero(y, ctx, depth)
        if isinstance(vx, int) and isinstance(vy, int):
            if vx < vy:
                out.append(("violation", "ℕ subtraction %d - %d" % (vx, vy)))
            return
        if isinstance(vy, int) and _num(y) is not None and ctx.role(x, depth) is None:
            # n - k with k a numeral: decided by a hypothesis k ≤ n / k - 1 < n, else unknown
            if any(lo == canon(y, depth) and hi == canon(x, depth) for lo, hi in ctx.facts["le"]):
                return
            if vy == 1 and ctx.has("pos", x, depth) or vy == 1 and ctx.has("nz", x, depth):
                return
        out.append(("unknown", "ℕ subtraction with undecided order"))


def _zero_site(d, ctx, depth, out, what):
    v = _value_at_zero(d, ctx, depth)
    if v == 0:
        out.append(("violation", what))
    elif not _isnz(v):
        out.append(("unknown", what + " with an undecided denominator"))


def _anchor(row, name):
    if name:
        s = "local:%s:%s}" % (row["declaration"], name)
        if s in (row.get("proof") or ""):
            return s
    return ""


def _lemma_uses(e, ctx, depth, out, have_name):
    pass


def check(row, const, constants):
    if const is None or const.type_expression is None:
        return [{"reason": "unmeasured: no elaborated constant", "anchor": ""}]
    hits, unknown = [], []
    # the statement
    ctx = Ctx()
    out = []
    _sites(const.type_expression, ctx, 0, out)
    for verdict, what in out:
        (hits if verdict == "violation" else unknown).append(("statement", what, ""))
    # the proof: haves and convention lemmas
    if const.value_expression is not None:
        ctx = Ctx()
        _SEEN.clear()
        _walk_value(const.value_expression, ctx, 0, row, hits, unknown, None)
    res, seen = [], set()
    for where, what, anchor in hits:
        key = (where, what, anchor)
        if key in seen:
            continue
        seen.add(key)
        res.append({"reason": "%s: %s" % (where, what), "anchor": anchor})
    if not res and unknown:
        u = collections.Counter(what for _, what, _ in unknown)
        return [{"reason": "unmeasured: " + "; ".join("%s ×%d" % kv for kv in sorted(u.items())), "anchor": ""}]
    return res


def _walk_value(e, ctx, depth, row, hits, unknown, have):
    _rec(e, ctx, depth, row, hits, unknown, have)


def _is_false(t):
    t = _strip(t)
    while X.ctor_of(t) == "forallE":
        t = _strip(X.binder_body(t))
    return X.ctor_of(t) == "const" and X.const_name(t) == "False"


def _negation_hyp(h, ctx, depth):
    """h is a bound hypothesis of type ¬ P or … → False."""
    h = _strip(h)
    if X.ctor_of(h) != "bvar":
        return False
    lvl = depth - 1 - X.bvar_index(h)
    if not 0 <= lvl < len(ctx.stack):
        return False
    dom = _strip(ctx.stack[lvl][0])
    n, _ = _head(dom)
    if X.ctor_of(dom) == "forallE" and not _is_false(dom):  # ∀ z, G z ≠ 0 applied fully
        while X.ctor_of(dom) == "forallE":
            dom = _strip(X.binder_body(dom))
        n, _ = _head(dom)
        return n in ("Not", "Ne")
    n, _ = _head(dom)
    return n in ("Not", "Ne") or (X.ctor_of(dom) == "forallE" and _is_false(dom))


def _branch_refutes(x, ctx, depth):
    """x = fun h => body, where body applies a negation hypothesis or a refutation head."""
    x = _strip(x)
    if X.ctor_of(x) != "lam":
        return False
    ctx.push(X.binder_domain(x))
    try:
        b = _strip(X.binder_body(x))
        if X.ctor_of(b) != "app":
            return False
        n, _ = _head(b)
        h, _ = _spine(b)
        return n in _REFUTE_HEADS or _negation_hyp(h, ctx, depth + 1)
    finally:
        ctx.pop()


def _rec(e, ctx, depth, row, hits, unknown, have, mode=None):
    e = _strip(e)
    key = (id(e), depth, have, mode)
    if key in _SEEN:
        return
    _SEEN.add(key)
    k = X.ctor_of(e)
    if k == "letE":
        name = X.binder_name(e)
        out = []
        _sites(X.binder_domain(e), ctx, depth, out)
        for verdict, what in out:
            (hits if verdict == "violation" else unknown).append(("have " + name, what, _anchor(row, name)))
        _rec(X.let_value(e), ctx, depth, row, hits, unknown, name,
             "refute" if _is_false(X.binder_domain(e)) else mode)
        ctx.push(X.binder_domain(e))
        _rec(X.binder_body(e), ctx, depth + 1, row, hits, unknown, have, mode)
        ctx.pop()
        return
    if k in ("lam", "forallE"):
        ctx.push(X.binder_domain(e))
        _rec(X.binder_body(e), ctx, depth + 1, row, hits, unknown, have, mode)
        ctx.pop()
        return
    if k == "const" or k == "app":
        n, a = _head(e)
        if n in LEMMAS:
            exempt = False
            if n in _SUB_EXEMPT_IF_EQUAL:
                terms = [x for x in a if X.ctor_of(x) not in ("lam",)]
                # implicit n m are the two numeric arguments before the proof
                nums = [x for x in a[:-1]][-2:]
                exempt = len(nums) == 2 and canon(nums[0], depth) == canon(nums[1], depth)
            if mode == "refute" or (mode or "").startswith("resolved:") and (mode[9:], n) in _RESOLVED:
                exempt = True
            if not exempt:
                where = ("have " + have) if have else "proof"
                if n in _SUB_LEMMAS:
                    # m95J03: simp/norm_num rewrite 0 - 1 = 0 inside the dead branch of an
                    # `if j ≤ i`; whether the reading reaches the branch is not decided here.
                    unknown.append((where, "truncated-subtraction lemma %s in the proof" % n, ""))
                else:
                    hits.append((where, "uses %s (%s)" % (n, LEMMAS[n]), _anchor(row, have)))
        if k == "app":
            h, _ = _spine(e)
            sub = mode
            if mode != "refute" and (n in _REFUTE_HEADS or _negation_hyp(h, ctx, depth)):
                sub = "refute"
            if X.ctor_of(h) != "const":
                _rec(h, ctx, depth, row, hits, unknown, have, mode)
            for i, x in enumerate(a):
                m = sub
                if sub != "refute" and n in ("Or.resolve_right", "Or.resolve_left") and i == 2:
                    m = "resolved:" + n
                elif sub != "refute" and n in ("Or.casesOn", "Or.elim") and len(a) == 6 and i == 3 \
                        and _branch_refutes(a[5], ctx, depth):
                    m = "resolved:" + n  # the b = 0 branch is closed by a hypothesis
                elif sub != "refute" and (mode or "").startswith("resolved:") and n not in ("Iff.mp", "Iff.mpr"):
                    m = None  # the resolved mode reaches only the Iff.mp lemma producing the disjunction
                _rec(x, ctx, depth, row, hits, unknown, have, m)
        return
    if k == "proj":
        _rec(X.proj_struct(e), ctx, depth, row, hits, unknown, have, mode)
        return
    for ch in e.children():
        _rec(ch, ctx, depth, row, hits, unknown, have, mode)
