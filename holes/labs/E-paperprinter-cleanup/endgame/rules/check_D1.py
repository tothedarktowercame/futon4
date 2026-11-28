"""D1 census check (claude-23, packet RC-D1): a goal is stated before it is reduced or proved:
a part says what it shows, and an enumeration is complete (a Second has a First).

It runs the paper walk for the record (the same `proof_body` the census runs) and keeps the
final Step list that `_layout` lays out, with the sentence the renderer made for each Step.
What is a part, a sibling, a calculation's owner, or a sufficiency target is decided on those
typed Steps (kind, case_index, depth, rewrite rows and their `after_is_prop`).  The census page
(`row["proof"]`) is read only to see what was printed at those Steps (so a page edited by hand,
as in a bad case, is judged as printed) and to anchor the violation.

Shapes (reason prefixes):
  "second without first": sibling `part` Steps (same parent, same depth, consecutive
      case_index 1..n under one constructor); a part with case_index >= 2 is printed with its
      ordinal lead ("Second, ..."), and the case_index-1 part is printed without "First, ".
      (old ink class P-second-without-first)
  "sufficiency target is not a proposition": a page sentence "It suffices to show \\( X \\)"
      whose X is the display of a walked rewrite row endpoint typed as a term
      (`after_is_prop is False`) and of no proposition-typed row.  (P-nonprop-suffices)
  "calculation with no statement of its goal": a have/let/part Step whose sentence is a
      displayed chain (align) and, on the page, nothing between the line's lead and the align
      states a formula.  (N14, N4: the chain is printed but its goal is never stated)

Unmeasured: the walk fails or yields no Steps; an enumeration whose parts cannot be found
on the page either with or without their lead; a sufficiency target X that matches no walked
row (the typed target is unknown).  Never reported clean.

Misses:
  * N14's original form, a calc whose rows are not displayed at all (t96J02 paste_cut): no
    sentence carries the rows, so there is nothing to locate.  Not covered.
  * a goal stated only by a later line (the statement after its proof); only "before the
    calculation, in the same line" is checked.
  * enumerations that are not `part` Steps (Case 1/Case 2 branches, obtain components).
  * a sufficiency sentence whose target was rewritten by hand into a formula that matches no
    walked row is unmeasured, not flagged.
Cost: one paper walk per record (module constants are loaded once per module and cached).
"""
import re

RULE = "D1"

_ORDINAL = re.compile(r"(?:\\item )?(First|Second|Third|Next),(?: |$)")
_cache = {}


def _module_env(module, constants):
    env = _cache.get(module)
    if env is not None:
        return env
    import paper_walk_test as T
    from mfuton.lean.blueprint_render.library_statements import LibraryStatements
    from mfuton.lean.blueprint_render.project_display_layer import project_display_layer, with_module_namespaces
    consts = constants(module)
    table = dict(consts) if isinstance(consts, dict) else {c.name.dotted(): c for c in consts}
    layer = with_module_namespaces(project_display_layer("apm-proofs", home=T.REPO), list(table.values()))
    pid = module.split(".")[1]
    if "lib" not in _cache:
        _cache["lib"] = LibraryStatements(T.LIBRARY)
    env = (table, layer, T.SOURCE / pid / "lean/Main.lean")
    if len(_cache) > 6:
        for k in [k for k in _cache if k != "lib"][:3]:
            del _cache[k]
    _cache[module] = env
    return env


def _walk(row, constants):
    """(step lists, sentence-by-step-id): every Step list `_layout` laid out for the record
    (the proof and any sub-layouts), or None."""
    from mfuton.lean.blueprint_render.paper_proof_walk import render as R
    from mfuton.lean.blueprint_render.paper_proof_walk import proof_body
    table, layer, source = _module_env(row["module"], constants)
    ci = table.get(row["declaration"])
    if ci is None:
        return None
    cap, sentences = [], {}
    orig_gp, orig_s = R._gather_prelude, R._sentence

    def gp(steps):
        cap.append(list(steps))
        return orig_gp(steps)

    def sentence(step, has_block=False, *, apm_policy=False):
        out = orig_s(step, has_block, apm_policy=apm_policy)
        sentences[id(step)] = out
        return out

    R._gather_prelude, R._sentence = gp, sentence
    try:
        proof_body(ci, layer, source, const_table=table, dep_table=_cache["lib"])
    finally:
        R._gather_prelude, R._sentence = orig_gp, orig_s
    if not cap:
        return None
    return cap, sentences


def _parent(steps, i):
    d = steps[i].depth
    for j in range(i - 1, -1, -1):
        if steps[j].depth < d:
            return j
    return -1


def _strip_lead(s):
    m = _ORDINAL.match(s)
    return s[m.end():] if m else s


_STATED = re.compile(r"we show \\\((?:.*?)\\\)[.:] ")


def _unled_forms(line):
    """The forms a led sentence takes when the part prints without its ordinal: the bare
    sentence capitalised, and, for "First, we show X. It suffices ...", the remainder (the
    sufficiency sentence alone, which is how the P-second-without-first producer printed it)."""
    bare = _strip_lead(line)
    out = []
    if bare:
        out.append(bare[0].upper() + bare[1:])
        m = _STATED.match(bare)
        if m and bare[m.end():]:
            out.append(bare[m.end():])
    return out


def _printed_lead(page, sentence):
    """True/False: the page prints this Step's sentence with / without its ordinal lead;
    None: neither form is on the page."""
    s = sentence.strip()
    if not s:
        return None
    first_line = s.split("\n")[0]
    # The page's line can differ from the walk's sentence after its first clause (reasons are
    # finished later); a prefix locates it.
    if _ORDINAL.match(first_line):
        if _prefix(first_line) in page:
            return True
        for bare in _unled_forms(first_line):
            if _prefix(bare) in page:
                return False
        return None
    if _prefix(first_line) in page:
        return False
    return None


def _prefix(line, n=70):
    return line[:n]


def _enumerations(steps, sentences, page):
    hits, unmeasured = [], []
    groups = {}
    for i, s in enumerate(steps):
        if s.kind == "part" and s.case_index >= 1:
            groups.setdefault((_parent(steps, i), s.depth), []).append(i)
    for (_p, _d), idx in groups.items():
        by_index = {}
        for i in idx:
            by_index.setdefault(steps[i].case_index, i)
        if 1 not in by_index or not any(k >= 2 for k in by_index):
            continue
        first = by_index[1]
        lead1 = _printed_lead(page, sentences.get(id(steps[first]), ""))
        later = [(k, i) for k, i in sorted(by_index.items()) if k >= 2]
        leads = [(k, i, _printed_lead(page, sentences.get(id(steps[i]), ""))) for k, i in later]
        if lead1 is None or any(l is None for _k, _i, l in leads):
            unmeasured.append("enumeration part not found on the page")
            continue
        for k, i, l in leads:
            if l and not lead1:
                s1 = sentences.get(id(steps[first]), "").strip().split("\n")[0]
                anchor = next((_prefix(f) for f in _unled_forms(s1) if _prefix(f) in page), "")
                hits.append({"reason": f"second without first: part {k} prints its ordinal, part 1 does not",
                             "anchor": anchor})
                break
    return hits, unmeasured


def _all_rows(steps):
    """Every walked rewrite row (an object with `after` and `after_is_prop`) reachable from the
    Steps: their chains, pieces, legacy chains, transport calcs, sufficiency closures."""
    rows, seen = [], set()
    stack = [(s, 0) for s in steps]
    while stack:
        obj, d = stack.pop()
        if obj is None or id(obj) in seen or d > 5 or isinstance(obj, (str, int, float, bool)):
            continue
        seen.add(id(obj))
        if hasattr(obj, "after_is_prop") and hasattr(obj, "after"):
            rows.append(obj)
            continue
        if isinstance(obj, (list, tuple)):
            stack.extend((x, d + 1) for x in obj)
            continue
        fields = getattr(obj, "__dataclass_fields__", None)
        if fields is None:
            continue
        for k in fields:
            if k in ("expr", "binder_expr", "goal_expr", "local_fact_owner", "citation_identity"):
                continue
            try:
                v = getattr(obj, k)
            except Exception:
                continue
            if isinstance(v, (list, tuple)) or hasattr(v, "__dataclass_fields__"):
                stack.append((v, d + 1))
    return rows


_SUFF = re.compile(r"It suffices to show \\\( (.*?) \\\)")


def _sufficiency(steps, page):
    hits, unmeasured = [], []
    rows = _all_rows(steps)
    typed = {}
    for r in rows:
        for side, prop in ((getattr(r, "after", None), getattr(r, "after_is_prop", None)),
                           (getattr(r, "before", None), getattr(r, "after_is_prop", None))):
            if isinstance(side, str) and side:
                typed.setdefault(side.strip(), set()).add(prop)
    for s in steps:
        for t in (getattr(s, "tex", None), getattr(s, "goal", None)):
            if isinstance(t, str) and t:
                typed.setdefault(t.strip(), set()).add(True)
    for m in _SUFF.finditer(page):
        x = m.group(1).strip()
        props = typed.get(x)
        if props is None:
            unmeasured.append("sufficiency target matches no walked row")
            continue
        if False in props and True not in props:
            hits.append({"reason": "sufficiency target is not a proposition: " + x[:60],
                         "anchor": m.group(0)})
    return hits, unmeasured


def _calculations(steps, sentences, page):
    hits = []
    for s in steps:
        if s.kind not in ("have", "let", "part"):
            continue
        sent = sentences.get(id(s), "") or ""
        k = sent.find("\\begin{align*}")
        if k < 0:
            continue
        block = sent[k:]
        end = block.find("\\end{align*}")
        block = block[:end + len("\\end{align*}")] if end >= 0 else block
        at = page.find(block)
        if at < 0:
            continue
        # max(.., 0): with no earlier item or blank line the slice page[-1:at] was empty, so a
        # calculation in the first paragraph always flagged (b97J03 read, 2026-11-28).
        line_start = max(page.rfind("\\item ", 0, at), page.rfind("\n\n", 0, at), 0)
        before = page[line_start:at]
        if "\\(" not in before and "\\[" not in before:
            hits.append({"reason": f"calculation with no statement of its goal ({s.kind})",
                         "anchor": "\n".join(block.split("\n")[:2])})
    return hits


def check(row, const, constants):
    page = row.get("proof") or ""
    if not page:
        return []
    try:
        walked = _walk(row, constants)
    except Exception as ex:
        return [{"reason": f"unmeasured: walk raised {type(ex).__name__}", "anchor": ""}]
    if walked is None:
        return [{"reason": "unmeasured: no walked steps", "anchor": ""}]
    layouts, sentences = walked
    every = [s for steps in layouts for s in steps]
    out, unmeasured = [], []
    for steps in layouts:
        h, u = _enumerations(steps, sentences, page); out += h; unmeasured += u
    h, u = _sufficiency(every, page); out += h; unmeasured += u
    out += _calculations(list({id(s): s for s in every}.values()), sentences, page)
    seen = set()
    out = [h for h in out if not ((h["reason"], h["anchor"]) in seen or seen.add((h["reason"], h["anchor"])))]
    if not out and unmeasured:
        return [{"reason": "unmeasured: " + unmeasured[0], "anchor": ""}]
    return out
