"""R1: every fact a step uses is cited -- by its tag, by a named lemma, or by a named definition
(claude-22, 2026-11-28).

What it decides.  A STEP is a page sentence whose typed anchor names a have/let/letFun binder of the
declaration's proof term ("We have \\leanref{}{local:<decl>:<NAME>}\\(P\\) ...", "We claim ..."), or
the declaration's own proof (the "main" step: every line outside the anchored steps' zones).  The
step's typed proof is the binder's value (or the declaration's value).  Its OWN uses are computed on
that term, not on text:
  * local facts: bound variables escaping the value whose occurrence is a proof (`is_proof`), named by
    the binder stack; a have the page never anchors (a hidden intermediary) counts as what it was
    built from; the values of anchored binders inside the term are separate steps and are skipped;
  * module lemmas: constants heading proof sub-terms (outside instance arguments) that are theorems of
    the module (`is_prop` type; compiler auxiliaries excluded).
A used local fact that the page TAGS (⟦n⟧ or "(n)" after its statement, i.e. it can be cited) must be
cited in the step's zone, and a used module lemma must be named there ("Lemma k" / \\leanref to it, or
"by definition of X" for X's own equation lemmas).  The zone is the step's line, the display block
(align) that follows it, and the itemize block that follows it (its sub-proof), up to the next step
at the same level; for the main step, every line outside the other zones.

Violations (reason texts; `match` keys in cases.tsv):
  * "uses (n)=<name>, not cited"        -- a tagged local fact the step's term uses;
  * "uses module lemma <C>, not named"  -- a module theorem the step's term uses;
  * "row with an empty reason"          -- a display row with no reason (`&& \\text{}` or no `&&`) in
                                           a zone whose typed term uses a citable fact;
  * "'a library lemma'"                 -- the generic placeholder in a zone whose term uses a theorem;
  * "no reason; its proof is <C>"       -- a fact sentence with no reason at all (or only "by
                                           simplification") whose typed proof IS a named library
                                           theorem applied (its head after transports).
Text is used to locate sentences, zones and citations and to resolve tags; each decision is on the
typed term.  A step whose binder is not in the typed value is "unmeasured" (never guessed clean).

Misses:
  * facts the page prints WITHOUT a tag (they cannot be cited by tag; "We have P." then used later is
    not flagged) and data binders;
  * library theorems inside a multi-lemma proof (simp / rw chains): only the head of a bare sentence
    is checked; rows reading "algebra" / "by simplification" that rewrite with a library lemma are not;
  * per-row attribution: a zone counts as citing (n) if any line in it does; a row that lost its own
    reason while a sibling row still cites the same fact is caught only by the empty-reason test;
  * lines without a typed anchor inside the main step ("Hence ...") are judged together, by the
    declaration's own term.
Cost: one binder scan per declaration and one use scan per step (memoised); no walk.
"""
import re

from mfuton.lean.latex_pp import _expr

RULE = "R1"

ANCHOR = re.compile(r"\\leanref\{\}\{local:([^}]*)\}")
TAG = re.compile(r"⟦(\d+)⟧")
PAREN_TAG = re.compile(r"\\\) \((\d+)\)")
STEP = re.compile(r"^(?:\\item )?(?:We have|We claim) \\leanref\{\}\{local:([^}]*)\}")
LEMMA_REF = re.compile(r"\\leanref\{(?:[^{}]|\{(?:[^{}]|\{[^{}]*\})*\})*\}\{([^{}]+)\}")
AUX_LEMMA = re.compile(r"\._(proof|simp|auxLemma|unfold)_|\.(eq_\d+|eq_def|match_\d+|proof_\d+)$|\._@\.")
#: proof constants that transport, unfold or assemble, not reasons of their own
PLUMBING = {"Eq.mpr", "Eq.mp", "id", "congrArg", "congrFun", "congr", "Eq.refl", "rfl", "Eq.trans",
            "Eq.symm", "of_eq_true", "eq_self", "propext", "Iff.rfl", "Iff.refl", "Eq.ndrec", "Eq.rec",
            "cast", "Eq.subst", "eq_true", "Eq.casesOn", "letFun", "trivial", "True.intro", "of_eq_false",
            "eq_false", "And.intro", "congrFun'", "congrArg'", "Iff.mpr", "Iff.mp", "Iff.intro",
            "implies_congr", "forall_congr", "absurd", "False.elim", "Or.inl", "Or.inr", "Exists.intro",
            "And.left", "And.right", "Classical.byContradiction", "Not.elim", "le_refl", "le_rfl",
            "Subtype.mk", "Subtype.property", "Exists.choose_spec", "Classical.choose_spec"}
TACTIC_NS = ("Mathlib.Tactic.", "Lean.", "Linarith.", "Std.Tactic.", "Mathlib.Meta.", "Nat.decEq",
             "Decidable.", "instDecidable", "Real.Ioo", "Batteries.")


def _children(e):
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


_BIND_CACHE = {}


def _bindings(key, value):
    """name -> [(value, binder stack)] for letE binders and letFun continuations (memoised per decl)."""
    got = _BIND_CACHE.get(key)
    if got is not None and got[0] is value:
        return got[1]
    found, seen, work = {}, {}, [(value, ())]
    while work:
        x, nm = work.pop()
        if (id(x), nm) in seen:
            continue
        seen[(id(x), nm)] = x
        c = _expr.ctor_of(x)
        if c == "letE":
            found.setdefault(_expr.binder_name(x), []).append((_expr.let_value(x), nm))
        elif c == "app":
            head, args = _expr.app_spine(x)
            if (_expr.ctor_of(head) == "const" and _expr.const_name(head) == "letFun"
                    and len(args) >= 4 and _expr.ctor_of(args[3][0]) == "lam"):
                found.setdefault(_expr.binder_name(args[3][0]), []).append((args[2][0], nm))
        for child, pushed in _children(x):
            work.append((child, nm + (pushed,) if pushed is not None else nm))
    _BIND_CACHE[key] = (value, found)
    return found


_USE_CACHE = {}


def _own_uses(value, names, shown):
    """(proof locals, proof-head constants) of the value, not descending into the values of anchored
    (shown) binders inside it: those are steps of their own.  Only occurrences in PROOF POSITION count:
    under a data or proposition node (an argument of a definition, `f α hα`; a binder's type; the
    statement inside a transport) or inside a tactic's own certificate (Lean.Omega, Mathlib.Tactic...)
    a hypothesis is a parameter or a captured context entry, not a fact the step uses."""
    k = (id(value), names)
    got = _USE_CACHE.get(k)
    if got is not None and got[0] is value:
        return got[1]
    locs, consts, seen = set(), set(), {}
    work = [(value, names, True)]
    while work:
        x, nm, live = work.pop()
        if (id(x), nm, live) in seen:
            continue
        seen[(id(x), nm, live)] = x
        c = _expr.ctor_of(x)
        if c == "mdata":
            work.append((_expr.mdata_child(x), nm, live))
            continue
        if c in ("lam", "forallE"):
            work.append((_expr.binder_domain(x), nm, False))
            work.append((_expr.binder_body(x), nm + (_expr.binder_name(x) or "_",),
                         live and _expr.is_proof(x) is not False))
            continue
        if c == "letE":
            bn = _expr.binder_name(x) or "_"
            if bn not in shown:
                v = _expr.let_value(x)
                work.append((v, nm, live and _expr.is_proof(v) is not False))
            work.append((_expr.binder_body(x), nm + (bn,), live))
            continue
        head, args = _expr.app_spine(x) if c == "app" else (x, [])
        hc = _expr.ctor_of(head)
        proof = _expr.is_proof(x) is True
        if live and proof:
            if hc == "bvar":
                i = _expr.bvar_index(head)
                if i < len(nm):
                    locs.add(nm[-(i + 1)])
            if hc == "const":
                consts.add(_expr.const_name(head))
        if c != "app":
            continue
        hn = _expr.const_name(head) if hc == "const" else ""
        if (hn == "letFun" and len(args) >= 4 and _expr.ctor_of(args[3][0]) == "lam"
                and (_expr.binder_name(args[3][0]) or "") in shown):
            work.append((args[3][0], nm, live))  # the continuation; the anchored value is its own step
            continue
        child_live = live and proof and not hn.startswith(TACTIC_NS)
        if hc not in ("const", "bvar"):
            work.append((head, nm, child_live))
        for a2, bi in args:
            if _expr.binder_info_name(bi) != "instImplicit":
                work.append((a2, nm, child_live and _expr.is_proof(a2) is not False))
    out = (locs, consts)
    _USE_CACHE[k] = (value, out)
    return out


def _expanded(value, names, binds, shown, depth=0, seen=None):
    """Own uses with hidden intermediaries (binders the page never anchors) replaced by their uses."""
    seen = set() if seen is None else seen
    locs, consts = _own_uses(value, names, shown)
    out_l, out_c = set(), set(consts)
    for n in locs:
        if n in shown or n not in binds or n in seen or depth > 6:
            out_l.add(n)
            continue
        seen.add(n)
        for v, nm in binds[n]:
            l2, c2 = _expanded(v, nm, binds, shown, depth + 1, seen)
            out_l |= l2
            out_c |= c2
    return out_l, out_c


def _tag_names(proof):
    """tag number -> local name: the first ⟦n⟧ or "\\) (n)" after an anchor, before the next anchor."""
    tags = {}
    for line in proof.split("\n"):
        anchors = [(m.start(), m.group(1)) for m in ANCHOR.finditer(line)]
        for k, (pos, ref) in enumerate(anchors):
            end = anchors[k + 1][0] if k + 1 < len(anchors) else len(line)
            seg = line[pos:end]
            found = [(m.start(), m.group(1)) for m in TAG.finditer(seg)]
            found += [(m.start(), m.group(1)) for m in PAREN_TAG.finditer(seg)]
            if found:
                tags.setdefault(min(found)[1], ref.split(":")[-1])
                st = re.search(r"\\\((.*?)\\\)", seg)
                if st:
                    STATEMENTS[ref.split(":")[-1]] = _norm(st.group(1))
    return tags


STATEMENTS = {}  # local name -> its printed statement (normalised), filled by _tag_names per record


def _norm(tex):
    return re.sub(r"\\[,;! ]|\s+", "", tex)


def _restated(text):
    """Statements a zone restates as its premise ("since \\(c < t\\)")."""
    return {_norm(m) for m in re.findall(r"since \\\((.*?)\\\)", text)}


def _zone_end(lines, i):
    """Index after step i's zone: its line, a following align block, a following itemize block."""
    j = i + 1
    while j < len(lines):
        if lines[j].startswith("\\begin{align"):
            while j < len(lines) and not lines[j].startswith("\\end{align"):
                j += 1
            j += 1
        elif lines[j].startswith("\\begin{itemize}"):
            depth = 0
            while j < len(lines):
                depth += lines[j].count("\\begin{itemize}") - lines[j].count("\\end{itemize}")
                j += 1
                if depth <= 0:
                    break
        else:
            break
    return j


def _cited(text):
    """(tag numbers cited, constant names named, defined names) in a zone's text.  A tag that merely
    labels a statement (⟦n⟧, "\\) (n)") is not a citation."""
    t = TAG.sub("", PAREN_TAG.sub("\\)", text))
    nums = set(re.findall(r"\((\d+)\)", t))
    consts = set(LEMMA_REF.findall(text))
    defined = set()
    for m in re.finditer(r"by definition of ([^,.;&]*)", text):
        frag = m.group(1)
        refs = LEMMA_REF.findall(frag)
        words = [w for w in re.findall(r"\{([A-Za-z_][^{}\\]*)\}", frag) if len(w) >= 2]
        defined |= set(refs) | set(words) | {"?"}
    return nums, consts, defined


def _definition_lemma(name, defined):
    tail = name.split(".")[-1]
    for d in defined:
        if d == "?":
            if "def" in tail or "_eq" in tail or tail.startswith("eq_") or tail.endswith("_apply"):
                return True
            continue
        last = d.split(".")[-1]
        if name == d or name.startswith(d + ".") or tail.startswith(last) or (tail.endswith("_def") and last in tail):
            return True
    return False


def _module_lemmas(consts, module_constants, decl):
    out = set()
    for n in consts:
        k = module_constants.get(n)
        if (k is not None and n != decl and not AUX_LEMMA.search(n)
                and getattr(k, "constructor", None) is None and getattr(k, "recursor", None) is None
                and _expr.is_prop(k.type_expression) is True):
            # a constructor of the module's inductive predicate is its definition, not a lemma
            out.add(n)
    return out


def _head_const(value):
    """The constant a proof term IS, after lambdas, mdata, transports (Eq.mpr/Eq.mp/id/cast/Eq.symm)."""
    x = value
    for _ in range(64):
        c = _expr.ctor_of(x)
        if c == "mdata":
            x = _expr.mdata_child(x)
            continue
        if c == "lam":
            x = _expr.binder_body(x)
            continue
        head, args = _expr.app_spine(x) if c == "app" else (x, [])
        if _expr.ctor_of(head) != "const":
            return None
        n = _expr.const_name(head)
        expl = [a for a, bi in args if _expr.binder_info_name(bi) == "default"]
        if n in ("Eq.mpr", "Eq.mp", "cast") and len(args) >= 4:
            x = args[3][0]
            continue
        if n in ("id", "Eq.symm", "Iff.mp", "Iff.mpr") and expl:
            x = expl[-1] if n == "id" else expl[0]
            continue
        return n
    return None


EMPTY_ROW = re.compile(r"&&\s*\\text\{\s*\}\s*(?:\\\\)?\s*$")


def _empty_rows(zone_lines):
    out, in_align, first = [], False, False
    for ln in zone_lines:
        if ln.startswith("\\begin{align"):
            in_align, first = True, True
            continue
        if ln.startswith("\\end{align"):
            in_align = False
            continue
        if in_align:
            row = ln.strip()  # the census prints one display row per line
            if not row:
                continue
            if first:
                first = False
                if "&&" not in row:
                    continue  # the chain's opening term
            if EMPTY_ROW.search(row) or (row.startswith("&") and "&&" not in row):
                out.append(ln)
    return out


def check(row, const, constants):
    proof = row.get("proof") or ""
    if not proof.strip():
        return []
    if const is None or getattr(const, "value_expression", None) is None:
        return [{"reason": "unmeasured: no typed value", "anchor": ""}]
    decl = row["declaration"]
    value = const.value_expression
    binds = _bindings((row["module"], decl), value)
    shown = set(re.findall(r"local:[^}]*:([^:}]*)\}", proof))
    STATEMENTS.clear()
    tags = _tag_names(proof)
    tag_of = {}  # a name can label several facts (shadowing: two binders called hx); any of them counts
    for n, name in tags.items():
        tag_of.setdefault(name, set()).add(n)
    try:
        mod_consts = constants(row["module"])
    except Exception:
        mod_consts = {}
    lines = proof.split("\n")
    steps, owned = [], set()
    i = 0
    while i < len(lines):
        m = STEP.match(lines[i])
        if m:
            end = _zone_end(lines, i)
            steps.append((m.group(1).split(":")[-1], i, end))
            # nested steps keep their own zones; the outer zone still covers their lines for citations
            owned.update(range(i, end))
            i += 1
            continue
        i += 1
    hits, unmeasured = [], []

    def judge(name, cands, zone_lines, anchor, bare_line=None):
        text = "\n".join(zone_lines)
        nums, named, defined = _cited(text)
        per = [_expanded(v, nm, binds, shown) for v, nm in cands]
        # a fact counts as used when every reading of the binder uses it (shadowed names: all)
        locs = set.intersection(*[l for l, _c in per]) if per else set()
        consts = set.intersection(*[c for _l, c in per]) if per else set()
        found = []
        restated = _restated(text)
        for loc in sorted(locs):
            ns = tag_of.get(loc)
            if ns is None or loc == name:
                continue
            if not (ns & nums) and STATEMENTS.get(loc) not in restated:
                found.append(f"uses ({min(ns, key=int)})={loc}, not cited")
        missing = [c for c in sorted(_module_lemmas(consts, mod_consts, decl))
                   if c not in named and not _definition_lemma(c, defined)]
        # "Lemma k" printed without its \\leanref (inside a row's \\text): the page numbers lemmas per
        # record, so k cannot be resolved here; each such number may stand for one used module lemma.
        plain = set(re.findall(r"(?<!\\leanref\{)Lemma (\d+)(?!\}\{)", text)) - set(
            re.findall(r"\\leanref\{Lemma (\d+)\}", text))
        if len(missing) > len(plain):
            for c in missing:
                found.append(f"uses module lemma {c}, not named")
        citable = bool(locs & set(tag_of)) or bool(_module_lemmas(consts, mod_consts, decl)) or any(
            c not in PLUMBING and not c.startswith(TACTIC_NS) for c in consts)
        if citable:
            for ln in _empty_rows(zone_lines):
                found.append("row with an empty reason")
                anchor = ln
                break
            if "a library lemma" in text:
                found.append("'a library lemma' names no lemma")
        if bare_line is not None and not found:
            reason = re.search(r"\\\)(?:\s*⟦\d+⟧|\s*\(\d+\))?\s*(.*)$", bare_line)
            tail = reason.group(1).strip() if reason else ""
            if tail in ("", ".", ":") or tail.startswith(("— by simplification", ", by simplification")):
                hcs = {_head_const(v) for v, _nm in cands}
                if len(hcs) == 1:
                    hc = hcs.pop()
                    k = mod_consts.get(hc) if hc else None
                    if (hc and hc not in PLUMBING and not hc.startswith(TACTIC_NS) and k is None
                            and not AUX_LEMMA.search(hc) and hc not in named and "._" not in hc):
                        found.append(f"no reason; its proof is {hc}")
        for f in dict.fromkeys(found):
            hits.append({"reason": f"step {name}: {f}", "anchor": anchor})

    for name, i, end in steps:
        cands = binds.get(name)
        if not cands:
            unmeasured.append(f"step {name}: binder not in typed value")
            continue
        zone = lines[i:end]
        judge(name, cands, zone, lines[i], bare_line=lines[i] if end == i + 1 else None)
    main_lines = [ln for k, ln in enumerate(lines) if k not in owned]
    if main_lines:
        body, names = value, ()
        while _expr.ctor_of(body) in ("lam", "mdata"):  # the statement's binders: Let / Suppose / (n)
            if _expr.ctor_of(body) == "mdata":
                body = _expr.mdata_child(body)
            else:
                names, body = names + (_expr.binder_name(body) or "_",), _expr.binder_body(body)
        judge(decl.split(".")[-1], [(body, names)], main_lines, "")
    if unmeasured and not hits:
        return [{"reason": "unmeasured: " + "; ".join(unmeasured[:3]), "anchor": ""}]
    return hits
