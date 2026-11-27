"""R2: the reason printed for a step is what justifies it (claude-22, 2026-11-27).

Decided for FACT sentences: "We have \\leanref{}{local:<decl>:<NAME>}\\(P\\) ... <reason>", where NAME is
a have/let binder of the declaration's typed proof term.  The step's typed proof is that binder's
value.  Its uses are the local facts (bound variables escaping the value, named by the binder stack)
and the constants it mentions.  The reason is resolved on the page: "(n)" -> the local name whose
anchor carries tag n (⟦n⟧ on its line, or "(n)" after its statement), "\\leanref{Lemma k}{C}" -> C.

Violations:
  * "by (n)" / "using (n)" where the name of (n) is not used by the value;
  * "this is (n)" where the value does not use (n), or also rests on another fact the page tags, or on a
    module lemma (library rewriting and transport are allowed: an unnamed library rewrite is R1);
  * hidden intermediaries (have binders the page never anchors) count as what they were built from;
  * "by Lemma k" where the value never mentions that constant.
  * "by definition of X" where the value's proof sub-terms use lemmas other than transport plumbing
    and X's own equation lemmas (the equation-lemma test goes partly by name: heuristic).
Text is used only to locate sentences and to resolve tags; the decision is on the typed value.
Not decided (and not flagged): closers, displayed-chain rows, step-kind reasons ("by transitivity",
"algebra").  Traversals are memoised: one binder scan per (module, declaration), one use scan and
one proof-head scan per fact value (the constants live in typed_flags' cache, so ids are stable).  A fact sentence whose binder or tag cannot be found
returns "unmeasured".
"""
import re

from mfuton.lean.latex_pp import _expr

RULE = "R2"

ANCHOR = re.compile(r"\\leanref\{\}\{local:([^}]*)\}")
TAG = re.compile(r"⟦(\d+)⟧")
PAREN_TAG = re.compile(r"\\\) \((\d+)\)")
FACT = re.compile(r"^(?:\\item )?We have \\leanref\{\}\{local:([^}]*)\}")
LEMMA = re.compile(r"\\leanref\{Lemma \d+\}\{([^}]*)\}")


def _children(e):
    """(child, binders pushed) pairs for a generic traversal."""
    c = _expr.ctor_of(e)
    if c in ("lam", "forallE"):
        return [(_expr.binder_domain(e), None), (_expr.binder_body(e), _expr.binder_name(e) or "_")]
    if c == "letE":
        return [(_expr.binder_domain(e), None), (_expr.let_value(e), None),
                (_expr.binder_body(e), _expr.binder_name(e) or "_")]
    if c == "mdata":
        return [(_expr.mdata_child(e), None)]
    if c == "proj":
        return [(_expr.proj_struct(e), None)]
    if c == "app":
        head, args = _expr.app_spine(e)
        out = [] if _expr.ctor_of(head) in ("const", "bvar") else [(head, None)]
        return out + [(a, None) for a, _bi in args]
    return []


_BIND_CACHE, _USE_CACHE, _HEAD_CACHE = {}, {}, {}


def _bindings_cached(key, value):
    got = _BIND_CACHE.get(key)
    if got is None or got[0] is not value:
        got = _BIND_CACHE[key] = (value, _bindings(value))
    return got[1]


def _uses_cached(value, names):
    k = (id(value), names)
    got = _USE_CACHE.get(k)
    if got is None or got[0] is not value:
        got = _USE_CACHE[k] = (value, _uses(value, names))
    return got[1]


def _proof_heads_cached(value):
    got = _HEAD_CACHE.get(id(value))
    if got is None or got[0] is not value:
        got = _HEAD_CACHE[id(value)] = (value, _proof_heads(value))
    return got[1]


TYPES_KEY = "\0types"


def _bindings(value):
    """name -> list of (fact value, binder stack at the value): letE binders and letFun continuations."""
    found = {}
    types = {}  # name -> (statement, binder stack): read only for witness provenance
    found[TYPES_KEY] = types
    seen = {}  # shared sub-terms: visit each (node, binder stack) once; values keep ids alive

    def visit(e, names):
        work = [(e, names)]
        while work:
            x, nm = work.pop()
            if (id(x), nm) in seen:
                continue
            seen[(id(x), nm)] = x
            c = _expr.ctor_of(x)
            if c == "letE":
                found.setdefault(_expr.binder_name(x), []).append((_expr.let_value(x), nm))
                types.setdefault(_expr.binder_name(x), []).append((_expr.binder_domain(x), nm))
            elif c == "app":
                head, args = _expr.app_spine(x)
                if (_expr.ctor_of(head) == "const" and _expr.const_name(head) == "letFun"
                        and len(args) >= 4 and _expr.ctor_of(args[3][0]) == "lam"):
                    found.setdefault(_expr.binder_name(args[3][0]), []).append((args[2][0], nm))
                    types.setdefault(_expr.binder_name(args[3][0]), []).append((args[0][0], nm))
            for child, pushed in _children(x):
                work.append((child, nm + (pushed,) if pushed is not None else nm))
    visit(value, ())
    return found


def _uses(value, names):
    """Local names (outside the value) and constants the value mentions."""
    locs, consts = set(), set()
    work = [(value, 0)]
    seen = {}  # values keep the nodes alive, so ids are not reused
    while work:
        x, d = work.pop()
        if (id(x), d) in seen:
            continue
        seen[(id(x), d)] = x
        c = _expr.ctor_of(x)
        if c == "bvar":
            i = _expr.bvar_index(x)
            if i >= d and i - d < len(names):
                locs.add(names[-(i - d + 1)])
            continue
        if c == "const":
            consts.add(_expr.const_name(x))
            continue
        if c == "app":
            head, _args = _expr.app_spine(x)
            hc = _expr.ctor_of(head)
            if hc == "const":
                consts.add(_expr.const_name(head))
            elif hc == "bvar":
                i = _expr.bvar_index(head)
                if i >= d and i - d < len(names):
                    locs.add(names[-(i - d + 1)])
        for child, pushed in _children(x):
            work.append((child, d + (1 if pushed is not None else 0)))
    return locs, consts


def _head_local(value, names):
    """The local fact a value IS (possibly applied / under lambdas), or None."""
    x, d = value, 0
    while _expr.ctor_of(x) in ("lam", "mdata"):
        if _expr.ctor_of(x) == "lam":
            x, d = _expr.binder_body(x), d + 1
        else:
            x = _expr.mdata_child(x)
    head = _expr.app_spine(x)[0] if _expr.ctor_of(x) == "app" else x
    if (_expr.ctor_of(head) == "const"
            and _expr.const_name(head) in ("congrArg", "congrFun", "congrArg'", "congrFun'")):
        # the fact (n) evaluated at a point / under a function: still "(n)" on paper
        for a, _bi in _expr.app_spine(x)[1]:
            a = _expr.mdata_child(a) if _expr.ctor_of(a) == "mdata" else a
            if _expr.ctor_of(a) == "bvar":
                i = _expr.bvar_index(a)
                if i >= d and i - d < len(names):
                    return names[-(i - d + 1)]
    if _expr.ctor_of(head) == "mdata":
        head = _expr.mdata_child(head)
    if _expr.ctor_of(head) == "bvar":
        i = _expr.bvar_index(head)
        if i >= d and i - d < len(names):
            return names[-(i - d + 1)]
    return None


def _expanded(locs, binds, shown, depth=0, seen=None):
    """Local uses with hidden intermediaries replaced by their own uses.

    A have/let binder the page never anchors (not "local:<decl>:<name>}" anywhere) is invisible to the
    reader; a step that uses it really rests on whatever it was built from.  E.g. a00J01-style
    `hαeval := hα.eval` (hidden) used by `minpoly.eq_of_irreducible hP hαeval`: "by (1) and (2)" with
    (2) = hα is honest."""
    seen = set() if seen is None else seen
    out = set()
    for n in locs:
        if n in shown or n not in binds or n in seen or depth > 6:
            out.add(n)
            continue
        seen.add(n)
        for v, nm in binds[n]:
            out |= _expanded(_uses_cached(v, nm)[0], binds, shown, depth + 1, seen)
    return out


#: compiler/tactic-made constants (simp sets, equation lemmas, match/proof auxiliaries): never a "Lemma k"
AUX_LEMMA = re.compile(r"\._(proof|simp|auxLemma|unfold)_|\.(eq_\d+|eq_def|match_\d+|proof_\d+)$|\._@\.")


_INNER_CACHE = {}


def _inner_names(value):
    """Names of binders inside the value (an obtain / intro within the step's own proof)."""
    got = _INNER_CACHE.get(id(value))
    if got is None or got[0] is not value:
        names, work, seen = set(), [value], {}
        while work:
            x = work.pop()
            if id(x) in seen:
                continue
            seen[id(x)] = x
            for child, pushed in _children(x):
                if pushed is not None:
                    names.add(pushed)
                work.append(child)
        got = _INNER_CACHE[id(value)] = (value, names)
    return got[1]


def _internal(name):
    return "._@." in name or "_hyg" in name


def _module_lemmas(consts, module_constants, decl):
    """Constants the value mentions that are theorems of the module (cited on paper as "Lemma k")."""
    out = set()
    for n in consts:
        k = module_constants.get(n)
        if (k is not None and n != decl and not AUX_LEMMA.search(n)
                and _expr.is_prop(k.type_expression) is True):
            out.add(n)
    return out


TRANSPORT = {"Eq.mp", "Eq.mpr", "cast"}


def _this_is(value, names):
    """(head local, rewrite parts) of a value read as "this is (n)": the value is the fact (n), possibly
    applied (instantiated, side conditions as arguments), possibly transported along rewrites.
    Strips lambdas, mdata, `id x`, `id (fun h => B) a` (show/simpa: the binder is (n) again),
    congrArg/congrFun, and Eq.mp/Eq.mpr/cast (whose equation proofs are the rewrite parts)."""
    x, nm, parts = value, tuple(names), []
    for _ in range(64):
        c = _expr.ctor_of(x)
        if c == "mdata":
            x = _expr.mdata_child(x)
            continue
        if c == "lam":
            nm, x = nm + (_expr.binder_name(x) or "_",), _expr.binder_body(x)
            continue
        if c == "letE":  # have-chain (e.g. inside show ... from): the step is the chain's last term
            nm, x = nm + (_expr.binder_name(x) or "_",), _expr.binder_body(x)
            continue
        if c != "app":
            break
        head, args = _expr.app_spine(x)
        hn = _expr.const_name(head) if _expr.ctor_of(head) == "const" else None
        expl = [a for a, bi in args if _expr.binder_info_name(bi) == "default"]
        if hn == "letFun" and len(args) >= 4 and _expr.ctor_of(args[3][0]) == "lam":
            lam = args[3][0]
            nm, x = nm + (_expr.binder_name(lam) or "_",), _expr.binder_body(lam)
            continue
        if hn == "id" and expl:
            if len(expl) >= 2 and _expr.ctor_of(expl[0]) == "lam":
                lam = expl[0]  # (fun h => B) a: B with h := a; h is named like the fact it receives
                nm, x = nm + (_expr.binder_name(lam) or "_",), _expr.binder_body(lam)
                parts.extend((a, nm[:-1]) for a in expl[1:])
                continue
            x = expl[0]  # id x a b: x applied to a b
            continue
        if hn in TRANSPORT and len(expl) >= 2:
            # Eq.mp/Eq.mpr/cast (h : α = β) (a : α): a is transported; further arguments apply the result
            parts.append((expl[0], nm))
            x = expl[1]
            continue
        if hn == "Eq.symm" and expl:
            x = expl[0]
            continue
        if hn == "Eq.trans" and len(expl) >= 2:
            # one side is the fact (a local), the other a rewrite: follow the side headed by a local
            heads = [_this_is(a, nm)[0] for a in expl[:2]]
            k = 1 if heads[1] is not None and heads[0] is None else 0
            if heads[k] is not None:
                parts.append((expl[1 - k], nm))
                x = expl[k]
                continue
        if hn in ("Iff.mp", "Iff.mpr") and expl:
            x = expl[0]  # (n) used as an iff, applied to its side argument
            continue
        if hn in ("congrArg", "congrFun", "congrArg'", "congrFun'"):
            proofs = [a for a in expl if _expr.is_proof(a) is not False and _expr.ctor_of(a) != "lam"]
            if proofs:
                x = proofs[-1] if hn.startswith("congrArg") else proofs[0]
                continue
        break
    head = _expr.app_spine(x)[0] if _expr.ctor_of(x) == "app" else x
    while _expr.ctor_of(head) == "mdata":
        head = _expr.mdata_child(head)
    if _expr.ctor_of(head) == "bvar":
        i = _expr.bvar_index(head)
        if i < len(nm):
            return nm[-(i + 1)], parts
    return None, parts


_PLOC_CACHE = {}


def _proof_locals(value, names):
    """Outer local names the value uses AS PROOFS (a bvar occurrence, or a bvar-headed application, whose
    node is a proof): hypotheses and facts, not data variables."""
    k = (id(value), names)
    got = _PLOC_CACHE.get(k)
    if got is not None and got[0] is value:
        return got[1]
    out, work, seen = set(), [(value, 0)], {}
    while work:
        x, d = work.pop()
        if (id(x), d) in seen:
            continue
        seen[(id(x), d)] = x
        c = _expr.ctor_of(x)
        b = None
        if c == "bvar":
            b = x
        elif c == "app":
            h = _expr.app_spine(x)[0]
            if _expr.ctor_of(h) == "bvar":
                b = h
        if b is not None and _expr.is_proof(x) is True:
            i = _expr.bvar_index(b)
            if i >= d and i - d < len(names):
                out.add(names[-(i - d + 1)])
        if c == "bvar":
            continue
        for child, pushed in _children(x):
            work.append((child, d + (1 if pushed is not None else 0)))
    _PLOC_CACHE[k] = (value, out)
    return out


def _expanded_proofs(value, names, binds, shown, depth=0):
    """Proof locals of the value, hidden intermediaries replaced by their own proof locals."""
    out = set()
    for n in _proof_locals(value, names):
        if n in shown or n not in binds or depth > 6:
            out.add(n)
            continue
        for v, nm in binds[n]:
            out |= _expanded_proofs(v, nm, binds, shown, depth + 1)
    return out


_REWRITE_WRAPPERS = {"congrArg", "congrFun", "congrArg'", "congrFun'", "propext", "id", "Eq.symm",
                     "of_eq_true", "eq_true", "Eq.trans", "congr", "Iff.symm"}


def _rewrite_heads(value, names):
    """What a rewrite step rewrites WITH: the constant or local heading its equation, through congrArg/
    congrFun/propext/id/Eq.symm/Eq.trans.  The arguments of that head (side conditions: abs_of_pos hw,
    div_lt_iff₀ … hw_pos) are not rewrites."""
    out, work = set(), [(value, tuple(names))]
    for _ in range(256):
        if not work:
            break
        x, nm = work.pop()
        c = _expr.ctor_of(x)
        if c == "mdata":
            work.append((_expr.mdata_child(x), nm))
            continue
        if c == "lam":
            work.append((_expr.binder_body(x), nm + (_expr.binder_name(x) or "_",)))
            continue
        head = _expr.app_spine(x)[0] if c == "app" else x
        hc = _expr.ctor_of(head)
        if hc == "const":
            hn = _expr.const_name(head)
            if hn in _REWRITE_WRAPPERS and c == "app":
                expl = [a for a, bi in _expr.app_spine(x)[1] if _expr.binder_info_name(bi) == "default"]
                proofs = [a for a in expl if _expr.is_proof(a) is not False]
                work.extend((a, nm) for a in (proofs or expl))
                continue
            out.add(hn)
        elif hc == "bvar":
            i = _expr.bvar_index(head)
            if i < len(nm):
                out.add(nm[-(i + 1)])
    return out


def _defined_name(reason):
    """The X of "by definition of X": the constant of a \\leanref in the clause, else a word of two or more
    letters in braces (\\operatorname{sinc}), else unnamed ("" -- equation-lemma-like names are accepted).
    A one-letter brace (the p of L^{p}) is notation, not the defined object."""
    m = re.findall(r"\\leanref\{[^{}]*(?:\{[^{}]*\}[^{}]*)*\}\{([^{}]+)\}", reason)
    if m:
        return m[-1]
    words = [w for w in re.findall(r"\{([A-Za-z_][^{}\\]*)\}", reason) if len(w) >= 2]
    return words[-1] if words else ""


def _tag_names(proof, decl):
    """tag number -> local name: the first ⟦n⟧ or "\\) (n)" after an anchor, before the next anchor."""
    tags = {}
    for line in proof.split("\n"):
        anchors = [(m.start(), m.group(1)) for m in ANCHOR.finditer(line)]
        for k, (pos, ref) in enumerate(anchors):
            end = anchors[k + 1][0] if k + 1 < len(anchors) else len(line)
            seg = line[pos:end]
            name = ref.split(":")[-1]
            found = [(m.start(), m.group(1)) for m in TAG.finditer(seg)]
            found += [(m.start(), m.group(1)) for m in PAREN_TAG.finditer(seg)]
            if found:
                tags.setdefault(min(found)[1], name)
    return tags


#: proof constants that transport or unfold, not reasons of their own
PLUMBING = {"Eq.mpr", "Eq.mp", "id", "congrArg", "congrFun", "congr", "Eq.refl", "rfl", "Eq.trans",
            "Eq.symm", "of_eq_true", "eq_self", "propext", "Iff.rfl", "Iff.refl", "Eq.ndrec", "Eq.rec",
            "cast", "Eq.subst", "eq_true", "Eq.casesOn", "letFun", "trivial", "True.intro", "of_eq_false",
            "eq_false", "And.intro", "congrFun'", "congrArg'", "Iff.mpr", "Iff.mp", "Eq.mpr_prop", "Eq.mpr_not", "implies_congr", "forall_congr"}


def _proof_heads(value):
    """Names of constants heading proof sub-terms of the value, outside instance arguments."""
    out = set()
    work = [value]
    seen = {}  # values keep the nodes alive, so ids are not reused
    while work:
        x = work.pop()
        if id(x) in seen:
            continue
        seen[id(x)] = x
        c = _expr.ctor_of(x)
        if c in ("app", "const") and _expr.is_proof(x) is True:
            head = _expr.app_spine(x)[0] if c == "app" else x
            if _expr.ctor_of(head) == "const":
                name = _expr.const_name(head)
                tail = name.split(".")[-1]
                if not (tail.startswith("inst") or re.match(r"to[A-Z]", tail)):
                    out.add(name)
        if c == "app":
            head, args = _expr.app_spine(x)
            if _expr.ctor_of(head) not in ("const", "bvar"):
                work.append(head)
            for a, bi in args:
                if _expr.binder_info_name(bi) != "instImplicit":
                    work.append(a)
            continue
        for child, _p in _children(x):
            work.append(child)
    return out


def _definition_lemma(name, defined):
    """An equation lemma of the defined constant: its own name, X.eq_def/eq_1, or <last>_def/_eq_..."""
    if not defined:  # the reason names no constant: accept equation-lemma-like names only
        tail = name.split(".")[-1]
        return "def" in tail or "_eq" in tail or tail.startswith("eq_")
    last = defined.split(".")[-1]
    tail = name.split(".")[-1]
    return (name == defined or name.startswith(defined + ".") or tail.startswith(last)
            or tail.endswith("_def") and last in tail)


def _reason(line):
    """The reason clause of a fact sentence (after its statement), or ''."""
    m = re.search(r"\\\)(?:\s*⟦\d+⟧)?\s*(— .*|, by .*|, using .*)$", line)
    return m.group(1) if m else ""


def check(row, const, constants):
    proof = row.get("proof") or ""
    if "We have \\leanref{}{local:" not in proof:
        return []
    if const is None or getattr(const, "value_expression", None) is None:
        return [{"reason": "unmeasured: no typed value", "anchor": ""}]
    binds = None
    tags = None
    hits, unmeasured = [], []
    for line in proof.split("\n"):
        m = FACT.match(line)
        if not m:
            continue
        reason = _reason(line)
        cites = re.findall(r"\((\d+)\)", reason) if ("by (" in reason or "using (" in reason
                                                     or "this is (" in reason) else []
        lemmas = LEMMA.findall(reason)
        defined = []
        if reason.startswith(", by definition of"):
            defined = [_defined_name(reason)]
        if not cites and not lemmas and not defined:
            continue
        if binds is None:
            binds = _bindings_cached((row["module"], row["declaration"]), const.value_expression)
            tags = _tag_names(proof, row["declaration"])
            shown = set(re.findall(r"local:[^}]*:([^:}]*)\}", proof))
            try:
                mod_consts = constants(row["module"])
            except Exception:
                mod_consts = {}
        name = m.group(1).split(":")[-1]
        cands = binds.get(name)
        if not cands:
            unmeasured.append(f"fact {name}: binder not in typed value")
            continue
        uses = [_uses_cached(v, nm) for v, nm in cands]
        anchor = line  # the whole sentence: a binder name can repeat on one page (m93J02 hdiff x2)
        if reason.startswith("— this is ("):
            # "this is (n)[, (m) ...]": the step is the cited fact(s), up to library rewriting and
            # transport.  Violated when a cited fact is not used, or when the value also rests on a
            # fact the page tags but the reason does not name, or on a module lemma.
            targets = []
            for n in dict.fromkeys(cites):
                t = tags.get(n)
                if t is None:
                    unmeasured.append(f"fact {name}: tag ({n}) unresolved")
                    targets = None
                    break
                targets.append(t)
            if targets is None:
                continue
            tagged = set(tags.values())
            verdicts = []
            for (locs, consts), (v, nm) in zip(uses, cands):
                if len(targets) == 1:
                    head, parts = _this_is(v, nm)
                    for _ in range(4):  # a hidden alias (e.g. hαeval := hα ...) is what it was built from
                        if head is None or head in shown or head == targets[0] or head not in binds:
                            break
                        hv, hnm = binds[head][0]
                        head, more = _this_is(hv, hnm)
                        parts = parts + more
                    why = []
                    if head != targets[0]:
                        # not (n) itself: still "(n) plus library reasoning" when (n) is the only fact
                        # the value rests on and no module lemma is used (e.g. linarith from (n))
                        pf = _expanded_proofs(v, nm, binds, shown) - {name}
                        mods = _module_lemmas(consts, mod_consts, row["declaration"])
                        if not (targets[0] in pf and not (pf - set(targets)) and not mods):
                            other = sorted(pf - set(targets))
                            why.append(f"is not ({cites[0]})={targets[0]} (its head is {head or 'not a local fact'}"
                                       + (f"; also rests on {', '.join(other[:3])}" if other else "")
                                       + (f"; uses module lemma {', '.join(sorted(mods)[:2])}" if mods else "") + ")")
                    else:
                        for pv, pn in parts:
                            rh = _rewrite_heads(pv, pn)
                            o = sorted({t for h in rh for t in _expanded({h}, binds, shown)} & tagged
                                       - set(targets) - {name})
                            ml = sorted(_module_lemmas(rh, mod_consts, row["declaration"]))
                            if o:
                                why.append(f"rewrites with tagged {', '.join(o)}")
                            if ml:
                                why.append(f"rewrites with module lemma {', '.join(ml)}")
                else:  # "this is (1), (2), and (3)": the value combines exactly the cited facts
                    ex = _expanded(locs, binds, shown)
                    why = [f"does not use {t}" for t in targets if t not in ex]
                verdicts.append(why)
            if all(verdicts):
                hits.append({"reason": f"'this is ({', '.join(dict.fromkeys(cites))})' but the value of {name} "
                                       + "; ".join(dict.fromkeys(verdicts[0])), "anchor": anchor})
            continue
        for n in dict.fromkeys(cites):
            target = tags.get(n)
            if target is None:
                unmeasured.append(f"fact {name}: tag ({n}) unresolved")
            elif not any(target in _expanded(locs, binds, shown) or target in _inner_names(v)
                         for (locs, _c), (v, _nm) in zip(uses, cands)):
                if any(_internal(n) for locs, _c in uses for n in _expanded(locs, binds, shown)):
                    # the value rests on an anonymous (hygienic) binder, e.g. an obtain/intro pattern
                    # variable; it may carry (n), and the binder stack cannot say which fact it is
                    unmeasured.append(f"fact {name}: ({n}) unused but the value uses a hygienic local")
                    continue
                stmt = [_uses_cached(t, tn)[0] for t, tn in binds[TYPES_KEY].get(name, [])]
                prov = any(target in _expanded(sl, binds, set()) for sl in stmt)  # through shown data too
                hits.append({"reason": f"cites ({n})={target}, which the value of {name} does not use"
                                       + ("; (n) occurs only in the statement (provenance of a witness)" if prov else ""),
                             "anchor": anchor})
        for x in defined:
            per = [{h for h in _proof_heads_cached(v) if h not in PLUMBING and not _definition_lemma(h, x)}
                   for v, _nm in cands]
            extra = set().union(*per)
            if extra and all(per):
                hits.append({"reason": f"'by definition of {x or '(unnamed)'}' but the value of {name} uses {', '.join(sorted(extra)[:4])}",
                             "anchor": anchor})
        for c in dict.fromkeys(lemmas):
            if not any(c in consts for _l, consts in uses):
                hits.append({"reason": f"cites Lemma {c}, which the value of {name} does not mention",
                             "anchor": anchor})
    if unmeasured and not hits:
        return [{"reason": "unmeasured: " + "; ".join(unmeasured[:3]), "anchor": ""}]
    return hits
