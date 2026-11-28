"""D2 census check (claude-24, packet RC-D2, 2026-11-28): every announced goal is proved under it
and closed exactly once.

Decides on the walker's final Step list (the list `proof_body` lays out, captured at
`render._gather_prelude`, as claude-19's detect_r1rs.py does), re-walked per record.  A GOAL OWNER is
a visible Step of kind have / let / part / branch that has a block (visible Steps deeper than it
directly after it), plus the proof itself (the depth-0 Steps).  Its DIRECT CHILDREN are the visible
Steps of its block at the block's least depth.  A child CLOSES the owner when it is a `closer`, a
`split` whose every branch closes, a `part`/`direction` (each part is judged as its own owner), or a
fact Step (not judged: a block may end on the fact that is its goal in another form).  A block that
ends on a setup Step (intro / assume / obtain / define / witness) does not close.

Typed violations (Step tree; anchor = the owner's line on the page when it can be found):
  - "truncated walk": the record's walk hit its visit budget (row walk_exhausted, typed by the walk);
  - "missing closer": the owner's last direct child does not close it (P-missing-closer,
    P-missing-subproof, P-sufficiency-end when the last child is a suffices/rewrite setup Step);
    for the proof itself only when the last depth-0 Step is an intro/assume/obtain/define/witness;
  - "closed twice": two closers among the direct children (not judged in a block with iff
    directions, where each direction has its own closer) (P-duplicate-closer, C3, N5), not counting
    a reduction followed by a closer ("It suffices to show Y. Hence Y");
  - "sufficiency end": the last direct child (or the proof's last Step) is a REDUCTION: a closer
    whose rewrite chain's last row rewrites the goal into another proposition (after_is_prop, not
    True), printed "It suffices to show Y", with no reason of its own (name / justified_by / premises) and
    nothing after it closing Y (P-sufficiency-end); at the proof's end also when the walk has the
    reason but the page's last paragraph is a lone "It suffices …" line (nothing printed after it);
  - "closer before its proof": a closer followed by a further non-closer direct child
    (P-premature-conclusion);
  - "stale goal": the owner's block opens with intro/assume Steps (the goal is now the body) and
    its closer restates the owner's own proposition, so the goal left by the intros is never stated
    (q308 t96J02 "Hence the claim" for f injective after "Let x, y; suppose f(x)=f(y)").
Page violations, located by the typed owner (a line carrying the owner's local anchor
`local:<decl>:<name>` or its tag ⟦n⟧ and ending in ":"):
  - "empty claim": the announced line has no block under it on the page (P-empty-claim,
    P-missing-subproof, P-opaque-proof when the pointwise steps are dropped);
  - "closer missing from the page": the walk closes the owner with a closer Step but the page block's
    last item is not a closing sentence (Hence / This contradicts / This is absurd / The hypotheses /
    In either case / In all (four) cases / In each … / Both parts / Both directions / This case / a case split);
  - "closed twice on the page": the page block has two identical closing items;
  - "closer before its proof on the page": a closing item followed by a non-closing one;
  - "empty sentence": an item or paragraph with no text but its tag (P-empty-sentence).
"unmeasured" when the walk cannot run (no constant, load or walk error, no Step list).

Misses: a branch or claim block that ends on a reduction whose walk reason the page drops (only
the proof's last paragraph is read for that); a block ending on a fact Step that is not its goal (judging it needs the goal after
the block's intros, which the Step list does not carry); parts ("First, we show X:" / "Second, …") on the page side (no anchor; their typed side is
checked); an owner whose page line has neither tag nor local anchor; whether a closer's reason is
right (R rules); a closer that states a different proposition than the owner's other than the
stale-goal shape; P-opaque-proof as such (an extensionality closer with no pointwise step is
not distinguished from a closer by a cited fact); root-level page edits (only the typed root test).
Cost: one walk per record (module constants and display layer cached per module), ~0.3-3 s/record.
"""
import re
import sys

RULE = "D2"
sys.setrecursionlimit(20000)

_MOD = {}
_OWNER_KINDS = ("have", "let", "part", "branch")
_OPEN_KINDS = ("intro", "assume", "typaram")
_SETUP_KINDS = ("intro", "assume", "typaram", "obtain_var", "obtain_hyp", "define", "witness")
_ROOT_BAD_LAST = _SETUP_KINDS
_CLOSING_SENTENCE = ("Hence", "Thus", "This contradicts", "This is absurd", "The hypotheses",
                     "In either case", "In all ", "In every ", "In each ", "In both ", "Both parts",
                     "Both directions", "This case")
_CLOSING_PAGE = ("Hence", "Thus", "This contradicts", "This is absurd", "The hypotheses",
                 "In either case", "In all ", "In every ", "In each ", "In both ", "Both parts",
                 "Both directions", "This case", "We distinguish", "To show", "To derive", "Case ")


def _module(module, constants):
    hit = _MOD.get(module)
    if hit is None:
        import paper_walk_test as T
        from mfuton.lean.blueprint_render.project_display_layer import project_display_layer, with_module_namespaces
        from mfuton.lean.blueprint_render.library_statements import LibraryStatements
        table = constants(module)
        layer = with_module_namespaces(project_display_layer("apm-proofs", home=T.REPO), list(table.values()))
        lib = _MOD.get("__lib__")
        if lib is None:
            lib = _MOD["__lib__"] = LibraryStatements(T.LIBRARY)
        hit = _MOD[module] = (table, layer, lib, T.SOURCE / module.split(".")[1] / "lean/Main.lean")
    return hit


def _steps(row, const, constants):
    from mfuton.lean.blueprint_render.paper_proof_walk import render as R
    from mfuton.lean.blueprint_render.paper_proof_walk import proof_body
    table, layer, lib, src = _module(row["module"], constants)
    cap = []
    orig = R._gather_prelude

    def gp(steps):
        cap.append(list(steps))
        return orig(steps)
    R._gather_prelude = gp
    try:
        proof_body(const, layer, src, const_table=table, dep_table=lib)
    finally:
        R._gather_prelude = orig
    return cap[-1] if cap else None


def _key(e):
    from mfuton.lean.blueprint_render import paper_walk
    if e is None:
        return None
    try:
        return paper_walk._scoped_case_key(e, e.loose_bound_variable_range)
    except Exception:
        return None


def _reduction(s):
    """A closer whose rewrite chain turns the goal into another proposition (its last row's `after`
    is a Prop that is not `True`): the page says "It suffices to show <after>", a reduction that
    leaves a goal, not a closing of it."""
    if s.kind != "closer" or not getattr(s, "rewrite_chain", None):
        return False
    last = s.rewrite_chain[-1]
    if not getattr(last, "after_is_prop", False):
        return False
    note = getattr(last, "after_notation", None)
    return not (isinstance(note, tuple) and len(note) > 1 and note[0] == "const" and note[1] == "True")


def _open_reduction(s):
    """A reduction with no reason of its own (no tactic reading in name, no justified_by): the
    residual goal is left unproved.  With a reason the page reads "It suffices to show Y. This holds
    by R." and the goal is closed."""
    return _reduction(s) and not (s.name or s.justified_by or s.premises)


def _visible(s):
    return s.kind != "silent" and s.case_closure != "suppress"


def _block(steps, i):
    d, out, j = steps[i].depth, [], i + 1
    while j < len(steps) and steps[j].depth > d:
        if _visible(steps[j]):
            out.append(j)
        j += 1
    return out


def _direct(steps, idx):
    if not idx:
        return []
    m = min(steps[j].depth for j in idx)
    return [j for j in idx if steps[j].depth == m]


def _closes(steps, j, owner_key):
    s = steps[j]
    if s.kind == "closer":
        return True
    if s.kind == "split":
        kids = _direct(steps, _block(steps, j))
        branches = [b for b in kids if steps[b].kind == "branch"]
        return bool(branches) and all(_branch_closes(steps, b) for b in branches)
    if s.kind in ("part", "direction"):
        return True  # a conjunction/iff block: each part is an owner of its own, checked there
    if s.kind in _SETUP_KINDS:
        return False
    return True  # a fact Step ends the block: not judged (see Misses)


def _branch_closes(steps, b):
    kids = _direct(steps, _block(steps, b))
    return bool(kids) and _closes(steps, kids[-1], None)


def _anchor(proof, decl, s):
    """The page line that announces Step s (its local anchor or tag), else ""."""
    keys = []
    if s.name:
        keys.append("\\leanref{}{local:" + decl + ":" + s.name + "}")
    if s.tag:
        keys.append("⟦" + str(s.tag) + "⟧")
    lines = proof.split("\n")
    for n, line in enumerate(lines):
        if any(k in line for k in keys) and (
                re.search(r":\s*(⟦\d+⟧)?\s*$", line)
                # claude-16 (review, 2026-11-28): "We have X. ⟦n⟧" with its sub-proof in the itemize
                # below is an announcing line too (1469 such blocks at c8076d84, 20% of the total); for it
                # only the empty block is judged (below)
                or (n + 1 < len(lines) and lines[n + 1].startswith("\\begin{itemize}"))):
            return line
    return ""


def _page_block(lines, k):
    """Items directly under the announced line lines[k]: (items, found_block)."""
    j = k + 1
    if j >= len(lines) or not lines[j].startswith("\\begin{itemize}"):
        return [], False
    depth, items = 0, []
    for ln in lines[j:]:
        if ln.startswith("\\begin{itemize}"):
            depth += 1
            continue
        if ln.startswith("\\end{itemize}"):
            depth -= 1
            if depth == 0:
                break
            continue
        if depth == 1 and ln.startswith("\\item"):
            items.append(ln[5:].strip())
        elif depth == 1 and items:
            items[-1] += "\n" + ln
    return items, True


def _strip_tag(t):
    return re.sub(r"\s*⟦\d+⟧\s*$", "", t).strip()


def check(row, const, constants):
    hits = []
    proof = row.get("proof") or ""
    if str(row.get("walk_exhausted")) == "True":
        hits.append({"reason": "truncated walk: the walk hit its visit budget and the page reads as complete",
                     "anchor": ""})
    # page-only text defects (no owner needed)
    for para in re.split(r"\n\s*\n", proof):
        if proof and not _strip_tag(para.strip()) and para.strip():
            hits.append({"reason": "empty sentence: a paragraph with only its tag", "anchor": para.strip()})
    for ln in proof.split("\n"):
        if ln.startswith("\\item") and not _strip_tag(ln[5:]):
            hits.append({"reason": "empty sentence: an item with no text", "anchor": ln})
    if const is None:
        return hits + [{"reason": "unmeasured: no elaborated constant", "anchor": ""}]
    try:
        steps = _steps(row, const, constants)
    except Exception as ex:
        return hits + [{"reason": f"unmeasured: walk raised {type(ex).__name__}: {str(ex)[:80]}", "anchor": ""}]
    if not steps:
        return hits + [{"reason": "unmeasured: the walk produced no Step list", "anchor": ""}]
    decl = row["declaration"]
    lines = proof.split("\n")
    owners = [i for i, s in enumerate(steps) if _visible(s) and s.kind in _OWNER_KINDS and _block(steps, i)]
    for i in owners:
        s = steps[i]
        kids = _direct(steps, _block(steps, i))
        okey = _key(s.expr) if s.kind != "branch" else None
        line = _anchor(proof, decl, s) if s.kind != "branch" else ""
        what = f"{s.kind} {s.name or '(unnamed)'}"
        closers = [j for j in kids if steps[j].kind == "closer"]
        # a reduction followed by a closer is "It suffices to show Y. Hence Y …": one closing
        finals = [j for n, j in enumerate(closers) if not (_reduction(steps[j]) and n + 1 < len(closers))]
        if _open_reduction(steps[kids[-1]]):
            hits.append({"reason": f"sufficiency end: the block of {what} ends on a reduction of its goal "
                                   "(It suffices to show …) that nothing closes", "anchor": line})
        elif not _closes(steps, kids[-1], okey):
            hits.append({"reason": f"missing closer: the block of {what} ends on a {steps[kids[-1]].kind} "
                                   f"Step ({steps[kids[-1]].species or '-'}), its goal is never stated",
                         "anchor": line})
        directions = any(steps[j].kind == "direction" for j in kids)
        if len(finals) > 1 and not directions:  # an iff block: each direction closes once (not judged)
            same = len({(steps[j].justified_by, tuple(map(str, steps[j].premises))) for j in finals}) < len(finals)
            hits.append({"reason": "closed twice: " + str(len(finals)) + f" closers in the block of {what}"
                                   + (" (same reason)" if same else ""), "anchor": line})
        for j in ([] if directions else finals):
            later = [m for m in kids if m > j and steps[m].kind not in ("closer", "part", "direction")]
            if later:
                hits.append({"reason": f"closer before its proof: the closer of {what} is followed by a "
                                       f"{steps[later[0]].kind} Step", "anchor": line})
                break
        if (s.kind in ("have", "let", "part") and steps[kids[0]].kind in _OPEN_KINDS
                and closers and okey is not None and _key(steps[closers[-1]].expr) == okey):
            hits.append({"reason": f"stale goal: the block of {what} opens with {steps[kids[0]].kind} Steps "
                                   "and its closer restates the claim, not the goal left by them",
                         "anchor": line})
        # page side, located by the typed owner
        if line:
            k = lines.index(line)
            items, found = _page_block(lines, k)
            if not found or not items:
                hits.append({"reason": f"empty claim: the page announces {what} with nothing under it",
                             "anchor": line})
            elif not re.search(r":\s*(⟦\d+⟧)?\s*$", line):
                # "We have X. ⟦n⟧" states X first; its sub-proof needs no closing sentence and may be
                # split into directions, so only the empty block is judged (claude-16, review: the
                # closing tests here gave 12 false flags on 40 such records)
                pass
            else:
                if closers and not items[-1].startswith(_CLOSING_PAGE):
                    hits.append({"reason": f"closer missing from the page: {what} is closed by the walk but its "
                                           "page block ends without a closing sentence", "anchor": line})
                fin = [n for n, t in enumerate(items) if t.startswith(_CLOSING_SENTENCE)]
                if fin and any(not items[m].startswith(_CLOSING_PAGE) for m in range(fin[0] + 1, len(items))):
                    hits.append({"reason": f"closer before its proof on the page: the block of {what} states "
                                           "its conclusion before a later step", "anchor": line})
                closing = [_strip_tag(t) for t in items if t.startswith(_CLOSING_SENTENCE)]
                if len(closing) != len(set(closing)):
                    hits.append({"reason": f"closed twice on the page: the block of {what} repeats its closing "
                                           "sentence", "anchor": line})
    roots = [i for i, s in enumerate(steps) if _visible(s) and s.depth == 0]
    last_para = [p for p in re.split(r"\n\s*\n", proof.strip()) if p.strip()][-1:] or [""]
    if roots and _open_reduction(steps[roots[-1]]):
        hits.append({"reason": "sufficiency end: the proof ends on a reduction of its goal (It suffices to "
                               "show …) that nothing closes", "anchor": ""})
    elif (roots and _reduction(steps[roots[-1]]) and last_para[0].startswith("It suffices")
          and "\n" not in last_para[0].strip()):  # nothing after the reduction (no "This holds …", no fact)
        hits.append({"reason": "sufficiency end on the page: the proof's last reduction has a reason in the "
                               "walk but the page ends on It suffices to show … without it",
                     "anchor": last_para[0].split("\n")[0]})
    elif roots and steps[roots[-1]].kind in _ROOT_BAD_LAST:
        hits.append({"reason": f"missing closer: the proof ends on a {steps[roots[-1]].kind} Step, its goal is "
                               "never stated", "anchor": ""})
    return hits
