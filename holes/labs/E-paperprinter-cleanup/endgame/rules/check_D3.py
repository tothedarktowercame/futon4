"""D3: a proof form reads as what it is (claude-25, 2026-11-28).

A case split names its alternatives; a proof by contradiction states its supposition; a decision
procedure's computation is not presented as an argument.  Five parts, one per old ink class.  Each part
decides on the declaration's typed proof term (const.value_expression); the page text only LOCATES the
sentence that presents that proof form (anchor = a verbatim substring of row["proof"]).

  split (C1)          The term splits a disjunction A ∨ B (Or.casesOn / Or.elim / Or.rec, where neither
                      side is the negation of the other, so not `by_cases` through `em`), and the page has
                      "distinguish cases on \\(M\\)": the split is named by the disjunction instead of its
                      alternatives (the printer's right form is "we distinguish \\(A\\) and \\(B\\)" + "Case k").
                      A "cases on" sentence is charged to the Or split only when the term has no decidable
                      split (dite / ite / Decidable.byCases / Classical.byCases / Decidable.casesOn, or an Or
                      over p ∨ ¬p) that could own it, or when M itself contains \\lor.
  alternative (C9)    The page says "the k-th alternative holds" more often than the term contains an Or
                      introduction chain selecting alternative k (Or.inr^(k-1) (Or.inl _), or Or.inr^(k-1) _
                      as the last alternative).  Occurrences are counted along the term's tree unfolding,
                      so a sub-proof shared between goals (`<;>`) counts once per goal.
  supposition (R3)    The term has an author proof by contradiction (Decidable/Classical.byContradiction whose
                      lambda binder h is not hygienic and is used by the body), the page presents its argument
                      (it is the theorem's final body and the page closes "the hypotheses being contradictory",
                      or a have/let binder inside its body is anchored on the page), and h is never introduced
                      (no \\leanref{}{local:<decl>:h} anchor).  A byContradiction that is the whole value of a
                      fact printed "— by contradiction" is not charged here: its argument is not on the page.
  procedure (R2)      "— by contradiction" on a fact "We have \\leanref{}{local:<decl>:NAME}..." whose typed
                      value is a hygienic byContradiction (omega / linarith / ...'s own refutation); the
                      theorem closer "..., the hypotheses being contradictory" over a hygienic final body;
                      an unanchored "— by contradiction" line when every byContradiction in the term is
                      hygienic.  (Mixed records leave the unanchored line undecided: "unmeasured".)
  self-comparison (C22)  The term states a strict comparison of a numeral with itself (LT.lt/GT.gt n n) and
                      the page prints "n > n" / "n < n" as a row or goal instead of "a contradiction"
                      (a split's own "We distinguish" / "Case k:" label of an impossible case is not charged).

Misses:
  * split: a split printed with the wrong alternatives, or with "Case k" items that do not match the typed
    arms (alternatives are not rendered and compared); n-way splits whose page form is fine but whose
    arm count differs; `match` splits on data (no typed Or): they are not decided.
  * alternative: a page that names alternatives in the wrong order where both indices occur in the term
    (multiset test); Or intros inside library arguments raise the typed count and can hide a misnaming.
  * supposition: binder names are matched by name, so an anchor for another binder with the same name
    hides a missing supposition; suppositions stated in words without an anchor are counted missing.
  * procedure: a decision procedure that does not go through byContradiction (decide, norm_num, simp
    closing by evaluation) presented as an argument; decision procedures named but mis-phrased.
  * self-comparison: only numeral literals (OfNat.ofNat / raw literals); a variable self-comparison is not
    decided.
Cost: one traversal of the value per record (shared sub-terms visited once); no walk.
"""
import re

from mfuton.lean.blueprint_render.paper_proof_walk import term_walk as W
from mfuton.lean.blueprint_render.paper_walk import _case_expr_key

RULE = "D3"

E = W._expr
H = W._helpers
CONTRA = {"Decidable.byContradiction", "Classical.byContradiction"}
OR_ELIM = {"Or.casesOn", "Or.elim", "Or.rec"}
DECIDABLE_SPLIT = {"dite", "ite", "Decidable.byCases", "Classical.byCases", "Decidable.casesOn",
                   "Decidable.rec", "Or.by_cases", "Classical.by_cases", "Decidable.by_cases"}
ORDINAL = {"first": 1, "second": 2, "third": 3, "fourth": 4, "fifth": 5, "sixth": 6, "seventh": 7,
           "eighth": 8, "ninth": 9, "tenth": 10}

CASES_ON = re.compile(r"distinguish cases on \\\((.*?)\\\)")
ALT = re.compile(r"the (first|second|third|fourth|fifth|sixth|seventh|eighth|ninth|tenth|\d+-th) alternative holds")
CLOSER = "the hypotheses being contradictory"
BY_CONTRA = "— by contradiction"
FACT = re.compile(r"^(?:\\item )?We have \\leanref\{\}\{local:([^}]*)\}")
CASE_LINE = re.compile(r"^(?:\\item )?(?:We distinguish|we distinguish|Case \d+:)|.*, we distinguish ")
SELF_CMP = re.compile(r"(?<![_\d.{])(?<!\\cdot )(?<![-+] )(\d+) (?:\\, )?(?:>|<) (?:\\, )?(\d+)(?![\d.])")


def _name(head):
    return E.const_name(head) if E.ctor_of(head) == "const" else ""


def _strip(e):
    return H._strip_mdata(e)


def _is_not_of(a, b):
    """a is `Not b` (typed)."""
    head, args = W._spine(_strip(a))
    return _name(head) == "Not" and args and _case_expr_key(args[-1][0]) == _case_expr_key(b)


def _is_numeral(e):
    e = _strip(e)
    if E.ctor_of(e) == "lit":
        return True
    head, args = W._spine(e)
    return _name(head) == "OfNat.ofNat" and len(args) >= 2 and E.ctor_of(_strip(args[1][0])) == "lit"


def _refutation_kind(e):
    """'tactic' / 'author' / 'author-unused' for a byContradiction node, else None."""
    head, args = W._spine(_strip(e))
    if _name(head) not in CONTRA or not args:
        return None
    lam = _strip(args[-1][0])
    if E.ctor_of(lam) != "lam":
        return None
    name = E.binder_name(lam) or ""
    if H._is_internal(name):
        return "tactic"
    return "author" if 0 in W._collect_outer_bvars(E.binder_body(lam)) else "author-unused"


def _value_head(value):
    """The proof node a fact value is, under lambdas/mdata/plumbing."""
    x = W._final_body(value)
    try:
        x = H._strip_mdata(H._unwrap_plumbing(H._strip_mdata(x)))
    except Exception:
        x = _strip(x)
    return x


def _scan_lets(body):
    """have/let binder names inside a refutation's body (letE and letFun continuations)."""
    names, stack, seen = set(), [body], set()
    while stack:
        e = stack.pop()
        if e is None or id(e) in seen:
            continue
        seen.add(id(e))
        x = _strip(e)
        if E.ctor_of(x) == "letE":
            names.add(E.binder_name(x) or "")
        head, args = W._spine(x)
        if _name(head) == "letFun" and len(args) >= 4 and E.ctor_of(_strip(args[3][0])) == "lam":
            names.add(E.binder_name(_strip(args[3][0])) or "")
        if _name(head) in CONTRA:
            continue  # a nested refutation is its own unit
        stack.extend(e.children())
    names.discard("")
    return names


def _alt_counts(value):
    """Alternative index -> number of OCCURRENCES of Or-intro chains selecting it.

    The term is a DAG (tactics such as `<;>` share one sub-proof between goals); the page prints a
    shared sub-proof once per occurrence, so occurrences are counted along the tree unfolding (path
    counts), not as distinct nodes."""
    order, seen, stack = [], set(), [(value, False)]
    while stack:
        e, done = stack.pop()
        if done:
            order.append(e)
            continue
        if e is None or id(e) in seen:
            continue
        seen.add(id(e))
        stack.append((e, True))
        for ch in e.children():
            if id(ch) not in seen:
                stack.append((ch, False))
    occ = {id(value): 1}
    inner = set()
    alts = {}
    for e in reversed(order):  # parents before children
        n = occ.get(id(e), 0)
        if E.ctor_of(e) != "mdata":
            head, args = W._spine(e)
            hn = _name(head)
            if hn in ("Or.inl", "Or.inr") and len(args) >= 3 and id(e) not in inner:
                r, y = 0, e
                while True:
                    yh, ya = W._spine(_strip(y))
                    if _name(yh) == "Or.inr" and len(ya) >= 3:
                        r += 1
                        y = ya[2][0]
                        if _name(W._spine(_strip(y))[0]) in ("Or.inl", "Or.inr"):
                            inner.add(id(_strip(y)))
                        continue
                    break
                alts[r + 1] = alts.get(r + 1, 0) + n
        for ch in e.children():
            occ[id(ch)] = occ.get(id(ch), 0) + n
    return alts


def _scan(value):
    """One pass over the term: everything the five parts need."""
    info = {"or_splits": 0, "or_em": 0, "decidable": 0, "alts": {}, "refutations": [],
            "bindings": {}, "self_cmp": set()}
    stack, seen, chain_inner = [value], set(), set()
    while stack:
        e = stack.pop()
        if e is None or id(e) in seen:
            continue
        seen.add(id(e))
        x = _strip(e)
        c = E.ctor_of(x)
        if c == "letE":
            info["bindings"].setdefault(E.binder_name(x), []).append(E.let_value(x))
        head, args = W._spine(x)
        n = _name(head)
        if n == "letFun" and len(args) >= 4 and E.ctor_of(_strip(args[3][0])) == "lam":
            info["bindings"].setdefault(E.binder_name(_strip(args[3][0])), []).append(args[2][0])
        if n in OR_ELIM and len(args) >= 2:
            a, b = args[0][0], args[1][0]
            if _is_not_of(a, b) or _is_not_of(b, a):
                info["or_em"] += 1
            else:
                info["or_splits"] += 1
        elif n in DECIDABLE_SPLIT:
            info["decidable"] += 1
        elif n in ("Or.inl", "Or.inr") and id(x) not in chain_inner and len(args) >= 3:
            r, y = 0, x
            while True:
                yh, ya = W._spine(_strip(y))
                if _name(yh) == "Or.inr" and len(ya) >= 3:
                    chain_inner.add(id(_strip(y)))
                    r += 1
                    y = ya[2][0]
                    continue
                if _name(yh) == "Or.inl":
                    chain_inner.add(id(_strip(y)))
                    k = r + 1
                else:
                    k = r + 1  # the last alternative
                break
            info["alts"][k] = info["alts"].get(k, 0) + 1
        elif n in ("LT.lt", "GT.gt") and len(args) >= 4:
            lhs, rhs = args[-2][0], args[-1][0]
            if _is_numeral(lhs) and _case_expr_key(lhs) == _case_expr_key(rhs):
                info["self_cmp"].add(n)
        kind = _refutation_kind(x)
        if kind is not None:
            lam = _strip(args[-1][0])
            inner = set()
            for v in _scan_lets(E.binder_body(lam)):
                inner.add(v)
            info["refutations"].append((kind, E.binder_name(lam) or "", id(x), inner))
        stack.extend(e.children())
    return info


def _line_of(text, pos):
    left = text.rfind("\n", 0, pos) + 1
    right = text.find("\n", pos)
    return text[left:right if right >= 0 else len(text)]


def check(row, const, constants):
    text = row.get("proof") or ""
    if not text:
        return []
    if const is None or getattr(const, "value_expression", None) is None:
        return [{"reason": "unmeasured: no typed proof term", "anchor": ""}]
    decl = row["declaration"]
    info = _scan(const.value_expression)
    info["alts"] = _alt_counts(const.value_expression)
    out = []

    # split (C1)
    for m in CASES_ON.finditer(text):
        if not info["or_splits"]:
            continue
        owner = info["decidable"] + info["or_em"]
        if not owner or "\\lor" in m.group(1):
            out.append({"reason": "split: an Or split (Or.casesOn/elim) is presented as "
                                  "'distinguish cases on' its disjunction, not by its alternatives",
                        "anchor": m.group(0)})

    # alternative (C9)
    page = {}
    for m in ALT.finditer(text):
        w = m.group(1)
        k = ORDINAL.get(w) or int(w.split("-")[0])
        page.setdefault(k, []).append(m.group(0))
    for k, hits in sorted(page.items()):
        if len(hits) > info["alts"].get(k, 0):
            out.append({"reason": f"alternative: the page says alternative {k} holds {len(hits)} time(s); "
                                  f"the term selects alternative {k} {info['alts'].get(k, 0)} time(s) "
                                  f"(typed selections: {dict(sorted(info['alts'].items()))})",
                        "anchor": hits[0]})

    # procedure (R2) and the fact values the supposition part must not charge
    fact_contra_nodes = set()
    kinds_all = {r[0] for r in info["refutations"]}
    for line_start in [0] + [m.end() for m in re.finditer("\n", text)]:
        line = text[line_start:text.find("\n", line_start) if text.find("\n", line_start) >= 0 else len(text)]
        if BY_CONTRA not in line:
            continue
        fm = FACT.match(line)
        if fm and fm.group(1).startswith(decl + ":"):
            name = fm.group(1)[len(decl) + 1:]
            values = info["bindings"].get(name)
            if not values:
                out.append({"reason": f"unmeasured: fact {name} has no typed binder", "anchor": ""})
                continue
            heads = [_value_head(v) for v in values]
            kinds = [_refutation_kind(h) for h in heads]
            fact_contra_nodes.update(id(h) for h in heads)
            if all(k == "tactic" for k in kinds):
                out.append({"reason": f"procedure: fact {name} is a decision procedure's own refutation "
                                      "(hygienic byContradiction) printed as '— by contradiction'",
                            "anchor": line[:200]})
            elif any(k == "tactic" for k in kinds) and not all(k == "tactic" for k in kinds) and \
                    any(k is not None and k != "tactic" for k in kinds):
                out.append({"reason": f"unmeasured: fact {name} has several values of mixed kinds", "anchor": ""})
        else:
            if kinds_all == {"tactic"}:
                out.append({"reason": "procedure: '— by contradiction' where every byContradiction in the "
                                      "term is a decision procedure's own refutation",
                            "anchor": line[:200]})
            elif "tactic" in kinds_all and kinds_all & {"author", "author-unused"}:
                out.append({"reason": "unmeasured: unanchored '— by contradiction' line, mixed refutations",
                            "anchor": ""})
    if CLOSER in text:
        final = _value_head(const.value_expression)
        if _refutation_kind(final) == "tactic":
            out.append({"reason": "procedure: the theorem closer 'the hypotheses being contradictory' "
                                  "is a decision procedure's own refutation",
                        "anchor": CLOSER})

    # supposition (R3)
    final_id = id(_value_head(const.value_expression))
    for kind, name, node, inner in info["refutations"]:
        if kind != "author" or node in fact_contra_nodes:
            continue
        shown = [n for n in sorted(inner) if "\\leanref{}{local:" + decl + ":" + n + "}" in text]
        presented = (node == final_id and CLOSER in text) or bool(shown)
        if presented and "\\leanref{}{local:" + decl + ":" + name + "}" not in text:
            out.append({"reason": f"supposition: proof by contradiction on '{name}' is never stated "
                                  "(its binder is not introduced on the page"
                                  + (f"; its argument is printed: {', '.join(shown[:3])}" if shown else "") + ")",
                        "anchor": CLOSER if node == final_id and CLOSER in text else ""})

    # self-comparison (C22)
    if info["self_cmp"]:
        for m in SELF_CMP.finditer(text):
            if CASE_LINE.match(_line_of(text, m.start())):
                continue  # an impossible case's own label (Q280: t99A02), not a row or goal
            if m.group(1) == m.group(2):
                out.append({"reason": "self-comparison: a numeral compared strictly with itself is printed "
                                      "as a row/goal instead of as the contradiction",
                            "anchor": m.group(0)})
    # dedupe
    seen, uniq = set(), []
    for h in out:
        key = (h["reason"], h["anchor"])
        if key not in seen:
            seen.add(key)
            uniq.append(h)
    return uniq
