"""B3 census check (claude-20, packet RC-B3): a defined name stands for its value from then on;
one name names one object.

Decides five things.  Typed data (the declaration's elaborated value/type terms) decides which
page names are binders, which binders are data (not hypotheses), how many distinct binders carry
a name, whether they nest, and what a let's value is; the page text only locates.

  (a) two binders, one display, overlapping scopes.  An introduction (intro anchor
      `\\leanref{}{local:<decl>:<n>}` followed by its formula) of a DATA binder prints display d
      while an earlier introduction of d is still in scope (its itemize is still open) and the two
      are different binders: different binder names, or one name that the term binds at least
      twice with one occurrence inside the other's body.  Reason "two binders printed as d".
  (b) one binder, two displays.  A data binder name the term binds exactly once is introduced
      twice on the page with different leading displays.  Reason "one binder n printed as d1 and d2".
  (c) one display for two objects in one formula (C10, x_{x}).  A formula binds a letter d
      (\\forall/\\exists/set-builder/\\mapsto/big operator, or a text binder "for all sufficiently
      large d") and in the same formula a different object is displayed with head d: a leanref
      whose target is a constant (e.g. a Classical.choose family x_{k}) and whose display is
      d_{...} or d(...).  Typed gate: the term has a binder whose display is d.
      Reason "formula binds d and also shows another object as d".
  (d) a let's value printed where its name stands (P-unfolded-local).  For a `Define n = V` line
      whose n is a letE (or `have`/`fun` applied) DATA binder of the term with a non-atomic value
      (not a numeral, constant or variable), the page rendering of V (leanref wrappers dropped)
      occurs again in a formula after the Define, while n is in scope.  A formula that also occurs
      in the printed statement is exempt (a closing restatement of the theorem's goal is outside
      the let).  Reason "value of n printed where n stands".
  (e) one tag, two facts (P-duplicate-tag).  A tag ⟦k⟧ is attached to two different lines.
      Reason "tag (k) names two facts".

Unmeasured: no elaborated constant/value for a record with a proof, or a term larger than the
binder walk's budget (400,000 visits).

Misses:
  * (d) is a text match of the value as the Define line prints it; a copy in another normal form
    is not seen (aunk03 kernel_integral_eq_modified prints R = n + 1/2 and later n + 2^{-1} in a
    lemma's instantiated statement).  Seeing that needs the walker's Step propositions and a
    normaliser (cost: one walk per record).
  * (a)/(b) see introductions only through intro anchors; a binder introduced without an anchor
    (an anchorless Let/Take, a binder inside a formula) is not compared.  Hypotheses (Prop
    binders) are not compared by display: their names never print; their visible key is the tag (e).
  * (c) sees a second object only when it is a leanref to a constant; two locals x in one formula
    (an outer x used under a formula's own \\forall x) cannot be told apart in text and are not flagged.
  * A rename by the printer that keeps one display apart (x', n for atTop) is correct and clean.
"""
import os, re, importlib.util

RULE = "B3"

_B1 = None


def _b1():
    """check_B1's text helpers (formula segments, leanref cleaning, bound tokens)."""
    global _B1
    if _B1 is None:
        here = os.path.dirname(os.path.abspath(__file__))
        for p in (os.path.join(here, "check_B1.py"),
                  os.path.expanduser("~/code/futon4/holes/labs/E-paperprinter-cleanup/endgame/rules/check_B1.py")):
            if os.path.exists(p):
                spec = importlib.util.spec_from_file_location("check_B1_for_B3", p)
                _B1 = importlib.util.module_from_spec(spec)
                spec.loader.exec_module(_B1)
                break
    return _B1


# ---------------------------------------------------------------- typed part
def _binders(const):
    """name -> list of binder records {data: bool, value: expr|None, node: expr, inner: set of
    ids of same-named binders strictly inside its body}."""
    from mfuton.lean.latex_pp import _expr
    out = {}
    budget = [400000]

    def is_data(dom):
        try:
            return _expr.is_prop(dom) is False
        except Exception:
            return False

    def walk(root):
        # iterative pre-order with the stack of enclosing binder names
        stack = [(root, ())]
        seen = set()
        while stack and budget[0] > 0:
            e, enclosing = stack.pop()
            if e is None:
                continue
            key = (id(e), enclosing[-3:] if enclosing else ())
            if key in seen:
                continue
            seen.add(key)
            budget[0] -= 1
            c = _expr.ctor_of(e)
            if c in ("lam", "forallE", "letE"):
                n = str(_expr.binder_name(e) or "")
                rec = None
                if n:
                    val = _expr.let_value(e) if c == "letE" else None
                    rec = {"data": is_data(_expr.binder_domain(e)), "value": val, "node": e,
                           "enclosing": enclosing}
                    out.setdefault(n, []).append(rec)
                stack.append((_expr.binder_domain(e), enclosing))
                if c == "letE":
                    stack.append((_expr.let_value(e), enclosing))
                stack.append((_expr.binder_body(e), enclosing + ((n, id(e)),) if n else enclosing))
                continue
            if c == "app":
                # `(fun n => body) v` (a `have n := v` / `let` elaborated as an applied lambda)
                head, args = _expr.app_spine(e)
                if _expr.ctor_of(head) == "lam" and args:
                    n = str(_expr.binder_name(head) or "")
                    if n:
                        out.setdefault(n, []).append({"data": is_data(_expr.binder_domain(head)),
                                                      "value": args[0][0], "node": head,
                                                      "enclosing": enclosing, "applied": True})
            try:
                stack.extend((ch, enclosing) for ch in e.children() if ch is not None)
            except Exception:
                pass

    for f in ("value_expression", "type_expression"):
        e = getattr(const, f, None)
        if e is not None:
            walk(e)
    if budget[0] <= 0:
        return None  # the term is larger than the walk budget: undecided
    return out


def _nested_same_name(recs):
    """Two binder occurrences of one name, one inside the other's body."""
    ids = {id(r["node"]) for r in recs}
    for r in recs:
        if any(n_id in ids and n_id != id(r["node"]) for _n, n_id in r["enclosing"]):
            return True
    return False


def _atomic(e):
    from mfuton.lean.latex_pp import _expr
    if e is None:
        return True
    c = _expr.ctor_of(e)
    if c in ("bvar", "fvar", "const", "lit", "sort", "mvar"):
        return True
    if c == "mdata":
        return _atomic(_expr.mdata_child(e))
    if c == "app":
        head, args = _expr.app_spine(e)
        if _expr.ctor_of(head) == "const" and str(_expr.const_name(head)) in ("OfNat.ofNat", "OfScientific.ofScientific", "Nat.cast", "Int.cast"):
            return all(_atomic(a) for a, _ in args)
    return False


# ---------------------------------------------------------------- text part
def _strip_links(s):
    """Drop \\leanref{A}{B} -> A (recursively), local intro anchors, sizing and spacing macros."""
    B1 = _b1()
    out, i, n = [], 0, len(s)
    while i < n:
        if s.startswith("\\leanref{", i):
            a, j = B1._group(s, i + 8)
            k = j
            if j < n and s[j] == "{":
                _b, k = B1._group(s, j)
            if a:
                out.append(_strip_links(a))
            i = k
            continue
        out.append(s[i])
        i += 1
    t = "".join(out)
    t = re.sub(r"\\(?:left|right)(?![A-Za-z])", "", t)
    t = re.sub(r"\\[,;:!](?![A-Za-z])", " ", t)
    return re.sub(r"\s+", " ", t).strip()


_SEG = re.compile(r"\\\((.*?)\\\)|\\\[(.*?)\\\]")
_TAG = re.compile(r"⟦(\d+)⟧")
_ANCHOR = re.compile(r"\\leanref\{\}\{local:([^}]*)\}\s*\\\((.*?)\\\)")
_LEADTOK = re.compile(r"^\s*((?:\\math[a-z]+\{[^{}]*\}|\\[A-Za-z]+|[A-Za-z])(?:_\{[^{}]*\}|_[A-Za-z0-9])?(?:\^\{\\prime+\}|'+)?)")
_TEXT_BINDER_VAR = re.compile(
    r"(?:for all|for every|for each|for some|for almost every|frequently|eventually)"
    r"(?: sufficiently large| sufficiently small| near [^}]*)?\s*\}?\s*(?:\\\))?\s*\\?\(?\s*([A-Za-z])(?![A-Za-z])")


_FAMILY_CONSTS = {"Classical.choose", "Exists.choose", "Classical.choose_spec", "Classical.indefiniteDescription",
                  "Classical.epsilon"}


def _top_ops(value):
    """Binary operators of a rendering outside every bracket: {"add", "mul"}."""
    depth, flat = 0, []
    for ch in value:
        if ch in "({[":
            depth += 1
        elif ch in ")}]":
            depth -= 1
        elif depth == 0:
            flat.append(ch)
    t = "".join(flat)
    ops = set()
    if re.search(r"\s[+-]\s", t):
        ops.add("add")
    if re.search(r"\\cdot|\\times|\\circ|/", t):
        ops.add("mul")
    return ops


def _subterm_match(vrx, seg, value):
    """A match of the value's rendering that is a whole operand: a sum is not glued to a product
    or power (2 \\cdot |x_0| + 2 does not contain the sum |x_0| + 2), a product not to a power.
    A bracketed value ((D)) is a whole operand wherever it stands."""
    ops = _top_ops(value)
    for m in vrx.finditer(seg):
        before = seg[:m.start()].rstrip()
        after = seg[m.end():].lstrip()
        tight = ()
        if "add" in ops:
            tight = ("\\cdot", "\\times", "^", "_", "/", "-", "\\circ")
        elif "mul" in ops:
            tight = ("^", "_")
        if tight and (before.endswith(tight) or after.startswith(tight)):
            continue
        return True
    return False


def _lines_with_scope(proof):
    """Yield (line_index, raw_line, scope_path): scope_path is the tuple of open itemize ids."""
    path, counter = [0], [0]
    for i, raw in enumerate(proof.split("\n")):
        s = raw.strip()
        if s.startswith("\\begin{itemize}"):
            counter[0] += 1
            path.append(counter[0])
            continue
        if s.startswith("\\end{itemize}"):
            if len(path) > 1:
                path.pop()
            continue
        yield i, raw, tuple(path)


def _in_scope(intro_path, here_path):
    return here_path[:len(intro_path)] == intro_path


def check(row, const, constants):
    proof = row.get("proof") or ""
    if not proof.strip():
        return []
    if const is None or getattr(const, "value_expression", None) is None:
        return [{"reason": "unmeasured: no elaborated constant/value for the declaration", "anchor": ""}]
    B1 = _b1()
    decl = row["declaration"]
    binders = _binders(const)
    if binders is None:
        return [{"reason": "unmeasured: binder walk budget (400000 nodes) exhausted", "anchor": ""}]
    hits, seen = [], set()

    def hit(reason, anchor):
        if reason in seen:
            return
        seen.add(reason)
        hits.append({"reason": reason, "anchor": anchor.strip()[:200]})

    lines = list(_lines_with_scope(proof))
    prefix = "local:" + decl + ":"

    # (a)/(b): data introductions by anchor
    intros = []   # (line, path, name, display, raw)
    for i, raw, path in lines:
        for m in _ANCHOR.finditer(raw):
            key = m.group(1)
            name = key[len(decl) + 1:] if key.startswith(decl + ":") else key.rsplit(":", 1)[-1]
            recs = binders.get(name)
            if not recs or not all(r["data"] for r in recs):
                continue
            t = _LEADTOK.match(_strip_links(m.group(2)))
            if not t:
                continue
            d = B1._sub_norm(t.group(1))
            # an introduction after a sentence on the same item ("First, we show ...: Let i be
            # given.") opens a flattened sub-proof: its scope is that line only
            before = re.sub(r"^\\item\s*", "", raw[:m.start()].strip())
            local = bool(re.search(r"[.:]\s*(?:Let|Fix|Take|Suppose|Define|Obtain|We have|From)?[^.:]*$", before)) and bool(before)
            define = m.group(2) if raw[:m.start()].rstrip().endswith("Define") else None
            intros.append((i, path + (("line", i),) if local else path, name, d, raw, define))
    for a in range(len(intros)):
        i2, p2, n2, d2, raw2, def2 = intros[a]
        for b in range(a):
            i1, p1, n1, d1, _raw1, def1 = intros[b]
            if i1 == i2 or not _in_scope(p1, p2):
                continue
            if def1 is not None and def1 == def2:
                continue  # the same definition restated: one object
            if d1 == d2:
                different = (n1 != n2) or (len(binders[n1]) >= 2 and _nested_same_name(binders[n1]))
                if different:
                    hit(f"two binders printed as {d2} in overlapping scopes ({n1}, {n2})", raw2)
            elif n1 == n2 and len(binders[n1]) == 1:
                hit(f"one binder {n1} printed as {d1} and {d2}", raw2)

    # (c): a formula binding d that also shows a constant-backed object headed d
    local_disp = set()
    for n, recs in binders.items():
        if any(r["data"] for r in recs) and not B1._is_internal(n):
            d = B1._n2l(n)
            if d:
                local_disp.add(B1._sub_norm(d))
    if local_disp:
        rx = B1._display_rx(sorted(local_disp, key=len, reverse=True))
        for i, raw, path in lines:
            for m in _SEG.finditer(raw):
                seg = m.group(1) if m.group(1) is not None else m.group(2)
                # a primed letter (x') is the printer's rename apart: another display
                plain = re.sub(r"(?<![A-Za-z\\])[A-Za-z](?:'+|\^\{\\prime+\})", " § ", _strip_links(seg))
                bound = set(B1._bound_tokens(B1._sub_norm(plain), rx))
                bound |= {v for v in _TEXT_BINDER_VAR.findall(plain) if v in local_disp}
                if not bound:
                    continue
                for lm in re.finditer(r"\\leanref\{", seg):
                    a, j = B1._group(seg, lm.end() - 1)
                    if j >= len(seg) or seg[j] != "{":
                        continue
                    tgt, _ = B1._group(seg, j)
                    if tgt not in _FAMILY_CONSTS:
                        continue  # a family chosen in the proof is a separate object; notation
                                  # over the bound variable itself (a_{j}, f in L^p) is not
                    hm = re.match(r"\s*([A-Za-z])(?:_\{|_[A-Za-z0-9]|\()", _strip_links(a))
                    if hm and hm.group(1) in bound:
                        hit(f"formula binds {hm.group(1)} and also shows another object ({tgt}) as {hm.group(1)}", raw)

    # (d): Define n = V, V printed again in n's scope
    stmt = _strip_links(row.get("body") or "")
    for i, raw, path in lines:
        m = re.search(r"Define \\leanref\{\}\{" + re.escape(prefix) + r"([^}]*)\}\s*\\\((.*?)\\\)", raw)
        if not m:
            continue
        name, formula = m.group(1), _strip_links(m.group(2))
        recs = [r for r in binders.get(name, ()) if r["data"] and r["value"] is not None]
        if not recs or all(_atomic(r["value"]) for r in recs):
            continue
        lhs_rhs = formula.split("=", 1)
        if len(lhs_rhs) != 2:
            continue
        value = lhs_rhs[1].strip()
        if len(value) < 3 or re.fullmatch(r"[A-Za-z0-9]+", value):
            continue
        # a value starting with "(" must not be an argument list: tr(D) is not a copy of (D)
        ndisp = lhs_rhs[0].strip()
        ndisp_rx = re.compile(r"(?<![A-Za-z\\])" + re.escape(ndisp) + r"(?![A-Za-z0-9])")
        vrx = re.compile((r"(?<![A-Za-z\\}\)\]])" if value[0] in "([" else r"(?<![A-Za-z\\])")
                         + re.escape(value) + r"(?![A-Za-z0-9_^])")
        for j, raw2, path2 in lines:
            if j <= i or not _in_scope(path, path2):
                continue
            if re.search(r"Define \\leanref\{\}\{" + re.escape(prefix + name) + r"\}", raw2):
                break  # redefined
            if re.search(r"Define \\leanref\{\}\{", raw2):
                continue  # another definition's own right-hand side (u_C = the same formula as u_R)
            for sm in _SEG.finditer(raw2):
                seg = _strip_links(sm.group(1) if sm.group(1) is not None else sm.group(2))
                if _subterm_match(vrx, seg, value) and seg not in stmt and not ndisp_rx.search(vrx.sub(" ", seg)):
                    hit(f"value of {name} printed where {name} stands ({value[:60]})", raw2)
                    break
        # align blocks
        in_align = False
        for j, raw2, path2 in lines:
            s = raw2.strip()
            if "\\begin{align" in s:
                in_align = True
            if in_align and j > i and _in_scope(path, path2) and not s.startswith("Define"):
                seg = _strip_links(s)
                if _subterm_match(vrx, seg, value) and "\\begin{align" not in s:
                    hit(f"value of {name} printed where {name} stands ({value[:60]})", raw2)
            if "\\end{align" in s:
                in_align = False

    # (e): one tag on two lines
    where = {}
    for i, raw, path in lines:
        for t in set(_TAG.findall(raw)):
            where.setdefault(t, []).append(raw)
    for t, rows in where.items():
        if len(rows) >= 2:
            hit(f"tag ({t}) names two facts", rows[1])
    return hits
