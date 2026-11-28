"""D4 census check (claude-19, packet RC-D4): a calculation shows each step once, in order, with
its reason.

Typed part.  The check re-runs the paper walk on the record (the snapshot's `proof_body`, as
claude-19's R1RS detector does) and reads the final Step list that `proof_body` lays out:
  * every calculation row the walk captured: `Step.rewrite_chain`, its `rewrite_pieces` and
    `transport_calc_pieces` (each row: before / after / relation / reason, with typed endpoints
    `before_expr` / `after_expr` where the walk kept them);
  * Step kinds, depths, species, premises and `transported_to_owner` (for restatements).
Text part: the page (`row["proof"]`) is the thing judged.  Its printed calculation rows are the
rows of each `align*` block (`& first`, `&rel next && \\text{reason}`) and each run of consecutive
"It suffices to show \\( X \\)" sentences.  A printed row is located in the typed rows by its
rendering with entity links and spacing removed (the walk renders the same typed terms the page
prints).

Violations (the reason names the defect class):
  * "repeated row" (C4, P-sufficiency-wall): a printed row's expression equals an earlier row's
    expression in the same block (A = ... = A, or the same "It suffices to show" target twice);
    the reason says whether the term itself returns to A or the page prints a row the term has once.
  * "row out of order" (N2): a printed row is reached in the typed rows, but only from a row the
    block prints after it, so the printed order is not the term's.
  * "row does not follow" (N2): no typed row (in either orientation) joins the printed row to the
    row printed before it, not even through unprinted rows; the block joins unrelated steps.
  * "lost row" (P-lost-row): the typed path from one printed row to the next passes through a row
    with a cited reason (not "algebra" / empty, and not a no-op whose sides print alike) that the
    page never prints.
  * "row without a reason": an align row after the first whose reason column is empty although
    its typed row has a reason (the page dropped it).
  * "restate" (R1RS): one fact printed twice, as stated and as the form its own transport re-reads
    it in: shape A (a claim whose sole visible child is the closer its own transport minted) and
    shape B (a "restate" closer citing only the fact printed just before it), from
    ~/notes/claude-19/r1rs/detect_r1rs.py; flagged only when both lines are on the page.
  * "parallel steps": three or more consecutive sibling facts, each a "restate" of the same single
    premise (aunk03 norm_contourIntegral_le: four "— this is (1)" lines), one step printed as
    several.

Unmeasured: the walk fails or finds no Steps for a record whose page has a calculation; or a printed
block (align or sufficiency run) has a row that matches no typed row, so its order and reasons
cannot be decided (the record is undecided, never clean).

Misses:
  * Rows are located by rendering: two distinct typed terms that print alike are one row to this
    check (C4's "distinct typed terms with one rendering"), and a page row the render step
    rewrote after the walk (cast collisions, coalesced excursions) is unmatched -> unmeasured.
  * Orientation is not checked: rows recovered from Eq.mpr transports may be printed reversed, so
    the path search uses each typed row in both directions; a ≤/< chain printed backwards is not
    caught.
  * A lost row whose reason is "algebra" / empty is not flagged (the render prunes such rows on
    purpose; P-lost-row's 6 intended omissions are of this kind).
  * Prose calculations outside align / "It suffices" sentences (a "We have A = B, by X." line
    followed by "Hence B = C") are not read as rows.
  * Shape A/B restatements are only those the R1RS detector names; "restate" closers with no
    transport (R1RS-REPORT's 159) are counted only through the parallel-steps rule.
Cost: one paper walk per record (seconds; the module's constants are loaded once per process).
"""
import os
import re
import sys

RULE = "D4"

sys.setrecursionlimit(max(sys.getrecursionlimit(), 20000))

_CACHE = {}  # the R1RS detector module
_NO_REASON = {"", "algebra", "by algebra"}
#: re-walk cap, as check_L1: a record whose census render took longer is unmeasured
CAP = float(os.environ.get("D4_CAP_SECONDS", "6"))
ALARM = 4 * CAP + 10


# ---------------------------------------------------------------- walk (typed data)

_LAYERS, _LIB = {}, []


def _layer(module, constants):
    from mfuton.lean.blueprint_render.project_display_layer import project_display_layer, with_module_namespaces
    if module not in _LAYERS:
        _LAYERS.clear()
        _LAYERS[module] = with_module_namespaces(
            project_display_layer("apm-proofs", home=os.environ.get("MFUTON_HOME", ".")),
            list(constants(module).values()))
    return _LAYERS[module]


def _library():
    from pathlib import Path
    from mfuton.lean.blueprint_render.library_statements import LibraryStatements
    if not _LIB:
        _LIB.append(LibraryStatements(Path(os.environ["MFUTON_TYPED_TEST_LIBRARY"])))
    return _LIB[0]


def _detector():
    d = _CACHE.get("detector")
    if d is None:
        path = os.path.join(os.path.dirname(os.path.abspath(__file__)), "detect_r1rs.py")
        if not os.path.exists(path):
            path = os.path.expanduser("~/notes/claude-19/r1rs/detect_r1rs.py")
        import importlib.util
        spec = importlib.util.spec_from_file_location("d4_detect_r1rs", path)
        d = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(d)
        # its walk hook (transport classification for its report) is not installed: it walks every
        # transport's equality proof (over 90 s on a01A05); this check reads only the shapes
        _CACHE["detector"] = d
    return d


def walk_steps(row, const, constants):
    """The final Step list proof_body lays out (as check_L1 re-renders), or None."""
    import time
    from pathlib import Path
    from mfuton.lean.blueprint_render.paper_proof_walk import render as R
    from mfuton.lean.native.config import project_lean_checkout
    det = _detector()
    pid = row["module"].split(".")[1]
    source = (project_lean_checkout("apm-proofs") or Path("/nonexistent")) / "problems" / pid / "lean/Main.lean"
    cap = []
    orig = R._gather_prelude

    def gp(steps):
        cap.append(list(steps))
        return orig(steps)
    R._gather_prelude = gp
    det.TRANSPORTS.clear()
    try:
        R.proof_body(const, _layer(row["module"], constants), source, const_table=constants(row["module"]),
                     dep_table=_library(), deadline=time.monotonic() + max(6.0, 3 * CAP))
    finally:
        R._gather_prelude = orig
    return cap[-1] if cap else None


# ---------------------------------------------------------------- text normalisation

def _group(tex, i):
    """(content, end) of the balanced {...} group starting at tex[i] == '{', or None."""
    if i >= len(tex) or tex[i] != "{":
        return None
    depth, j = 0, i
    while j < len(tex):
        c = tex[j]
        if c == "\\":
            j += 2
            continue
        if c == "{":
            depth += 1
        elif c == "}":
            depth -= 1
            if depth == 0:
                return tex[i + 1:j], j + 1
        j += 1
    return None


def _plain(tex):
    """The page text with every \\leanref{shown}{target} replaced by its shown text."""
    tex = tex or ""
    out, i, tag = [], 0, "\\leanref"
    while True:
        k = tex.find(tag, i)
        if k < 0:
            out.append(tex[i:])
            return "".join(out)
        g1 = _group(tex, k + len(tag))
        g2 = _group(tex, g1[1]) if g1 else None
        if not g2:
            out.append(tex[i:k + len(tag)])
            i = k + len(tag)
            continue
        out.append(tex[i:k])
        out.append(_plain(g1[0]))
        i = g2[1]


def norm(tex):
    t = _plain(tex)
    t = re.sub(r"\\(left|right|bigl|bigr|Bigl|Bigr)(?=[^a-zA-Z])", "", t)
    t = re.sub(r"\\[,;:!]|\\quad|~", "", t)
    return re.sub(r"\s+", "", t)


_REL = re.compile(r"^(=|\\leq?|\\geq?|\\le|\\ge|<|>|\\neq?|\\iff|\\implies|\\Rightarrow|\\Leftrightarrow|"
                  r"\\subseteq|\\supseteq|\\subset|\\equiv|\\approx|\\sim|\\to|\\lt|\\gt|\\mid)(?![a-zA-Z])\s*")


def _strip_rel(expr):
    expr = expr.strip()
    m = _REL.match(expr)
    return (expr[m.end():], m.group(1)) if m else (expr, "")


def _reason_text(part):
    p = part.strip()
    m = re.fullmatch(r"\\text\{(.*)\}", p, re.S)
    return (m.group(1) if m else p).strip()


# ---------------------------------------------------------------- printed rows

class PRow:
    def __init__(self, expr, reason, start, line, kind):
        self.expr, self.reason, self.start, self.line, self.kind = expr, reason, start, line, kind
        self.key = norm(expr)


def printed_blocks(page):
    """[(kind, [PRow])] for align blocks and runs of 'It suffices to show' sentences."""
    blocks = []
    for m in re.finditer(r"\\begin\{align\*?\}(.*?)\\end\{align\*?\}", page, re.S):
        body = m.group(1)
        if "\\begin{" in body:
            continue
        rows, off = [], m.start(1)
        for raw in re.split(r"\\\\\s*\n", body):
            line = raw.strip()
            pos = page.find(raw.strip(), off) if line else -1
            if pos >= 0:
                off = pos
            if not line or line.startswith("& \\text{It suffices"):
                continue
            parts = line.split("&&", 1)
            expr = parts[0].strip()
            if expr.startswith("&"):
                expr = expr[1:]
            expr, rel = _strip_rel(expr)
            rows.append(PRow(expr, _reason_text(parts[1]) if len(parts) > 1 else None, pos, line, "align"))
        if len(rows) >= 2:
            blocks.append(("align", rows))
    # sufficiency runs: consecutive sentences "It suffices to show \( X \)" with only space between
    pat = re.compile(r"It suffices to show \\\( (.*?) \\\)([^.\n]*)\.")
    run, last_end = [], None
    for m in pat.finditer(page):
        gap = page[last_end:m.start()] if last_end is not None else None
        if run and (gap is None or gap.strip() not in ("", "\\item", "\\end{itemize}\n\\begin{itemize}\n\\item")):
            if len(run) >= 2:
                blocks.append(("suffices", run))
            run = []
        run.append(PRow(m.group(1), m.group(2).strip(" ,"), m.start(), m.group(0), "suffices"))
        last_end = m.end()
    if len(run) >= 2:
        blocks.append(("suffices", run))
    return blocks


# ---------------------------------------------------------------- typed rows

class TRow:
    def __init__(self, row, chain_id, idx):
        self.before = norm(getattr(row, "before", None) or (row[0] if isinstance(row, tuple) else ""))
        self.after = norm(getattr(row, "after", None) or (row[1] if isinstance(row, tuple) and len(row) > 1 else ""))
        reason = getattr(row, "reason", None)
        if reason is None and isinstance(row, tuple) and len(row) > 2:
            reason = row[2]
        self.reason = (reason or "").strip() if isinstance(reason, str) else ""
        self.chain, self.idx = chain_id, idx
        self.noop = self.before == self.after

    @property
    def cited(self):
        return self.reason.lower() not in _NO_REASON and not self.noop


def typed_rows(steps):
    out, cid = [], 0
    for s in steps:
        chains = [s.rewrite_chain] + [p.rewrite_chain for p in getattr(s, "rewrite_pieces", [])] \
            + [p.rewrite_chain for p in getattr(s, "transport_calc_pieces", [])]
        for ch in chains:
            if not ch:
                continue
            cid += 1
            for i, r in enumerate(ch):
                try:
                    out.append(TRow(r, cid, i))
                except Exception:
                    pass
    return out


def _adjacency(trows):
    adj = {}
    for t in trows:
        adj.setdefault(t.before, []).append((t.after, t))
        adj.setdefault(t.after, []).append((t.before, t))
    return adj


def _path(adj, src, dst, limit=8):
    """Shortest list of typed rows joining src to dst (either orientation), or None."""
    if src == dst:
        return []
    frontier, seen, back = [src], {src}, {}
    for _ in range(limit):
        nxt = []
        for node in frontier:
            for b, t in adj.get(node, ()):
                if b in seen:
                    continue
                seen.add(b)
                back[b] = (node, t)
                if b == dst:
                    path = []
                    while b != src:
                        b, t = back[b]
                        path.append(t)
                    return path[::-1]
                nxt.append(b)
        frontier = nxt
        if not frontier:
            break
    return None


def _anchor(page, prow):
    return prow.line if prow.line and page.count(prow.line) >= 1 else ""


def check_blocks(page, trows):
    hits, unmeasured = [], []
    ends = _adjacency(trows)
    pairs = {}
    for t in trows:
        pairs.setdefault(frozenset((t.before, t.after)), []).append(t)
    for kind, rows in printed_blocks(page):
        unmatched = [r for r in rows if r.key not in ends]
        # a sufficiency run's first target is reached from the goal, which the run does not print
        if unmatched:
            unmeasured.append(f"unmeasured: {kind} row not found in the typed rows: {_plain(unmatched[0].expr)[:80]}")
            continue
        seen = {}
        for k, r in enumerate(rows):
            if r.key in seen:
                j = seen[r.key]
                n_typed = sum(1 for t in trows if t.after == r.key)
                why = ("the term's chain returns to it" if n_typed >= 2 or any(t.before == r.key and t.after == r.key for t in trows)
                       else "the term has this row once")
                label = "It suffices to show" if kind == "suffices" else "align"
                hits.append({"reason": f"repeated row: {label} row {k + 1} repeats row {j + 1} ({why}): "
                                       f"{_plain(r.expr)[:80]}", "anchor": _anchor(page, r)})
                continue
            seen[r.key] = k
            if k == 0 or kind == "suffices":
                continue
            prev = rows[k - 1]
            if prev.key == r.key:
                continue
            direct = pairs.get(frozenset((prev.key, r.key)), [])
            if direct:
                t = direct[0]
                if kind == "align" and not r.reason and t.cited:
                    hits.append({"reason": f"row without a reason: align row {k + 1} prints no reason; "
                                           f"its typed row has '{_plain(t.reason)[:60]}'", "anchor": _anchor(page, r)})
                continue
            later = {rows[m].key for m in range(k + 1, len(rows))}
            path = _path(ends, prev.key, r.key)
            if path is None:
                hits.append({"reason": f"row does not follow: no typed row joins align row {k} to row {k + 1}: "
                                       f"{_plain(r.expr)[:80]}", "anchor": _anchor(page, r)})
                continue
            inner = set()
            for t in path:
                inner |= {t.before, t.after}
            inner -= {prev.key, r.key}
            if inner & later:
                hits.append({"reason": f"row out of order: align row {k + 1} is reached in the term only through a "
                                       f"row printed after it: {_plain(r.expr)[:80]}", "anchor": _anchor(page, r)})
                continue
            printed = {x.key for x in rows}
            lost = [t for t in path if t.cited
                    and not ({t.before, t.after} - {prev.key, r.key}) <= printed]
            if lost:
                t = lost[0]
                hits.append({"reason": f"lost row: between align rows {k} and {k + 1} the term has a step "
                                       f"'{_plain(t.reason)[:60]}' the page does not print", "anchor": _anchor(page, r)})
    return hits, unmeasured


# ---------------------------------------------------------------- restatements

def check_restate(page, steps, det):
    hits = []
    plain_page = norm(page)
    for h in det.detect(steps):
        o, c = steps[h["owner"]], steps[h["child"]]
        ko, kc = norm(o.tex), norm(c.tex)
        if not ko or not kc or ko not in plain_page or kc not in plain_page:
            continue
        if (ko == kc or kc in ko or ko in kc) and plain_page.count(kc) < 2:
            continue  # the closer prints as "Hence the claim.": the fact is on the page once
        anchor = c.tex if c.tex and page.count(c.tex) == 1 else ""
        hits.append({"reason": f"restate: shape {h['shape']}: one fact printed twice, as stated and as its transport "
                               f"re-reads it: {_plain(c.tex)[:80]}", "anchor": anchor})
    return hits


def _same_premise(t, s, prem):
    if t.species != "restate" or t.depth != s.depth or len(t.premises) != 1:
        return False
    q = t.premises[0]
    return q is prem or (isinstance(prem, str) and isinstance(q, str) and q == prem)


_FACT_LINE = re.compile(r"(?m)^(?:\\item )?(We have|Hence|Thus|So) (?:\\leanref\{\}\{[^}]*\})?\\\((.+?)\\\)(?=[ .,]|$)")


def _tail(page, m):
    """The reason text after a fact line's formula, up to the end of its line."""
    end = page.find("\n", m.end())
    return page[m.end():end if end >= 0 else len(page)]


def _reason_key(tail):
    t = re.sub(r"⟦\d+⟧", "", _plain(tail))
    return re.sub(r"[\s.,—]|by|^,", "", t)


def _adds_nothing(page, prev, m):
    """The Hence line gives no reason of its own: none, the previous line's reason again, or
    'this is (t)' for the previous line's own tag."""
    ht, pt = _tail(page, m), _tail(page, prev)
    hk = _reason_key(ht)
    if not hk or hk == _reason_key(pt):
        return True
    tag = re.search(r"⟦(\d+)⟧", pt)
    return bool(tag) and hk == f"thisis({tag.group(1)})"


def check_unwalked_restate(page, steps):
    """A printed "Hence \\(Y\\)" line directly after a printed fact, where Y is the rendering of no
    visible Step: the page states a fact twice that the walk lays out once."""
    keys = {norm(s.tex) for s in steps if s.kind != "silent" and s.tex}
    keys |= {norm(getattr(s, "goal", "") or "") for s in steps}
    hits, prev = [], None
    for m in _FACT_LINE.finditer(page):
        k = norm(m.group(2))
        between = page[prev.end():m.start()] if prev is not None else None
        adjacent = between is not None and between.strip(" .\n") .replace("\\begin{itemize}", "").replace(
            "\\end{itemize}", "").replace("\\item", "").strip(" .\n") == "" or (
            between is not None and re.fullmatch(r"[^\n]*\n(\\begin\{itemize\}\n|\\end\{itemize\}\n)*", between))
        if (m.group(1) == "Hence" and prev is not None and adjacent and k and k not in keys
                and norm(prev.group(2)) in keys and k != norm(prev.group(2))
                and _adds_nothing(page, prev, m)):
            hits.append({"reason": f"restate: a printed fact the walk has no step for, directly after the fact it "
                                   f"re-reads: {_plain(m.group(2))[:80]}", "anchor": m.group(0)})
        prev = m
    return hits


def check_parallel(page, steps):
    from mfuton.lean.blueprint_render.paper_proof_walk import term_walk as TW
    hits, i = [], 0
    vis = [s for s in steps if s.kind != "silent"]
    while i < len(vis):
        s = vis[i]
        if s.species == "restate" and len(s.premises) == 1 and s.kind in TW.FACT_KINDS + ("closer",):
            prem = s.premises[0]
            j = i + 1
            while j < len(vis) and _same_premise(vis[j], s, prem):
                j += 1
            if j - i >= 3 and norm(s.tex) and norm(s.tex) in norm(page):
                p = prem if isinstance(prem, str) else f"({getattr(prem, 'tag', '?')})"
                hits.append({"reason": f"parallel steps: {j - i} consecutive facts each restate {p} alone; "
                                       f"one step printed as {j - i}", "anchor": ""})
            i = j
        else:
            i += 1
    return hits


def check(row, const, constants):
    page = row.get("proof") or ""
    if not page.strip():
        return []
    if const is None or const.value_expression is None:
        return [{"reason": "unmeasured: no elaborated value", "anchor": ""}]
    if (row.get("proof_seconds") or 0) > CAP:
        return [{"reason": f"unmeasured: census render took {row.get('proof_seconds')} s > {CAP} s re-walk cap",
                 "anchor": ""}]
    import signal

    def _alarm(*_a):  # the walk's own deadline is not checked in every phase (a01A05 ran past it)
        raise TimeoutError(f"re-walk over {ALARM:.0f} s")
    old = signal.signal(signal.SIGALRM, _alarm)
    signal.alarm(int(ALARM))
    try:
        steps = walk_steps(row, const, constants)
    except Exception as ex:
        return [{"reason": f"unmeasured: re-render failed ({type(ex).__name__}: {str(ex)[:60]})", "anchor": ""}]
    finally:
        signal.alarm(0)
        signal.signal(signal.SIGALRM, old)
    has_calc = "\\begin{align" in page or "It suffices to show" in page
    if steps is None:
        return [{"reason": "unmeasured: the walk laid out no Steps", "anchor": ""}] if has_calc else []
    det = _detector()
    hits, unmeasured = check_blocks(page, typed_rows(steps))
    hits += check_restate(page, steps, det)
    hits += check_parallel(page, steps)
    hits += check_unwalked_restate(page, steps)
    return hits + [{"reason": u, "anchor": ""} for u in unmeasured[:1]]
