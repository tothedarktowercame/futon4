"""L1: a sentence appears with the step it describes, under the assumptions it depends on.

claude-21, RC-L1T1 (2026-11-27).  Decided on typed walk data: the record is re-rendered with the
snapshot's own proof_body, with passive wrappers on term_walk._Walk and render._sentence that
record goal occurrences and binder scopes (no mfuton file is edited; the wrappers change nothing
in the output).  A record violates L1 when

  L1c  a sentence's rewrite piece belongs to a different goal than the sentence's step: the
       piece's GoalOccurrence, followed up its parents to the step's own occurrence, passes a
       `proof_argument` occurrence (the rewrite is the proof of an argument, a `by simpa` two
       levels down), or never reaches the step's occurrence at all (R1TP);
  L1b  a fact Step is followed, one level deeper, by a claim Step that the walk marked
       `argument_claim` (the inline proof of an argument of that fact's application): the
       sentence is printed before, and detached from, a step it depends on (C23 at q303);
  L1a  a Step is recorded while a hypothesis (Prop domain, not instance-implicit) that the walk
       pushed silently (a plumbing lambda, no intro/assume Step) is in scope and used by the
       lambda's body: the sentence stands under an assumption the page never states.

Cost: a full re-render of the census is about 6000 s, so a record is re-rendered only when its
page contains a sentence that can break L1b/L1c (a sufficiency sentence, or a fact sentence
directly followed by a nested claim) - text used only to select and locate - and the census
rendered it in at most CAP seconds.  Heavier candidates are returned as unmeasured.  L1a is
therefore measured only on the re-rendered records (stated in the report).
"""
from __future__ import annotations

import os
import re
import time
from pathlib import Path

RULE = "L1"
CAP = float(os.environ.get("L1_CAP_SECONDS", "2"))
#: L1a is off by default: on t01A03 + a96A07 it made 36 of 47 flags, most under tactic-hygienic
#: hypotheses (`a._@._internal…`) whose sentences the page does state correctly (RC-L1T1-REPORT).
L1A = os.environ.get("L1_SILENT_HYPOTHESIS", "0") == "1"

from mfuton.lean.blueprint_render.paper_proof_walk import term_walk as W, render as R  # noqa: E402
from mfuton.lean.blueprint_render.project_display_layer import project_display_layer, with_module_namespaces  # noqa: E402
from mfuton.lean.blueprint_render.library_statements import LibraryStatements  # noqa: E402
from mfuton.lean.native.config import project_lean_checkout  # noqa: E402

E, H = W._expr, W._helpers
_SHAPE_B = re.compile(r"We have [^\n]*— by [^\n]*\n\\begin\{itemize\}\n\\item We claim")
_QUIET_KINDS = {"intro", "assume", "typaram", "silent", "branch", "direction"}

# ------------------------------------------------------------ passive instrumentation
_STATE = {"active": False, "walks": [], "sentences": [], "piece_stack": []}


class _Steps(list):
    __slots__ = ("_w",)

    def _mark(self, x):
        w = self._w
        try:
            x._l1_occ = w._goal_occurrence
            x._l1_silent = tuple(n for _, n in getattr(w, "_l1_silent", ()))
        except Exception:
            pass

    def append(self, x):
        if _STATE["active"]:
            self._mark(x)
        list.append(self, x)

    def insert(self, i, x):
        if _STATE["active"]:
            self._mark(x)
        list.insert(self, i, x)


_orig_init = W._Walk.__init__
_orig_walk = W._Walk._walk
_orig_sentence = R._sentence
_orig_rpt = R._rewrite_piece_text


def _init(self, printer):
    _orig_init(self, printer)
    if _STATE["active"]:
        s = _Steps()
        s._w = self
        self.steps = s
        self._l1_depths = [0]
        self._l1_silent = []
        _STATE["walks"].append(self)


def _walk(self, e):
    depths = getattr(self, "_l1_depths", None)
    if depths is None or not _STATE["active"]:
        return _orig_walk(self, e)
    pushed = self._plumbing_lambda_depth > depths[-1]
    if pushed:
        pushed = False
        try:
            sb = self._scope_bindings[-1]
            lam = sb.binder_node
            if (self.step_of_binder and self.step_of_binder[-1] is None and lam is not None
                    and E.ctor_of(lam) == "lam"
                    and E.binder_info_name(E.binder_info_ordinal(lam)) != "instImplicit"
                    and self._is_prop(E.binder_domain(lam)) is True
                    and 0 in W._collect_outer_bvars(E.binder_body(lam))):
                self._l1_silent.append((len(self.step_of_binder) - 1, E.binder_name(lam) or "?"))
                pushed = True
        except Exception:
            pushed = False
    depths.append(self._plumbing_lambda_depth)
    try:
        return _orig_walk(self, e)
    finally:
        depths.pop()
        if pushed:
            self._l1_silent.pop()


def _sentence(step, *a, **k):
    if not _STATE["active"]:
        return _orig_sentence(step, *a, **k)
    _STATE["piece_stack"].append([])
    try:
        out = _orig_sentence(step, *a, **k)
    finally:
        pieces = _STATE["piece_stack"].pop()
    _STATE["sentences"].append((step, out, pieces))
    if _STATE["piece_stack"]:
        _STATE["piece_stack"][-1].extend(pieces)
    return out


def _rpt(piece, *a, **k):
    out = _orig_rpt(piece, *a, **k)
    if _STATE["active"] and _STATE["piece_stack"] and out:
        _STATE["piece_stack"][-1].append((piece, out))
    return out


W._Walk.__init__ = _init
W._Walk._walk = _walk
R._sentence = _sentence
R._rewrite_piece_text = _rpt

_LAYERS = {}
_LIB = []


def _layer(module, constants):
    if module not in _LAYERS:
        _LAYERS.clear()
        _LAYERS[module] = with_module_namespaces(
            project_display_layer("apm-proofs", home=Path(".").resolve()), list(constants(module).values()))
    return _LAYERS[module]


def _library():
    if not _LIB:
        _LIB.append(LibraryStatements(Path(os.environ["MFUTON_TYPED_TEST_LIBRARY"])))
    return _LIB[0]


def _occ_path(occ, target):
    """Roles from occ up to (not including) target; None when target is not an ancestor."""
    roles = []
    while occ is not None:
        if occ is target or (target is not None and occ.token == target.token):
            return roles
        roles.append(occ.origin.role)
        occ = occ.parent
    return None


def _anchor(text, proof):
    if not text:
        return ""
    for cut in (len(text), 160, 100, 60):
        frag = text[:cut]
        if frag and frag in proof:
            return frag
    return ""


def check(row, const, constants):
    proof = row.get("proof") or ""
    candidate = ("suffices to" in proof) or bool(_SHAPE_B.search(proof))
    if not candidate:
        return []
    if const is None or const.value_expression is None:
        return [{"reason": "unmeasured: no elaborated value", "anchor": ""}]
    if (row.get("proof_seconds") or 0) > CAP:
        return [{"reason": f"unmeasured: census render took {row.get('proof_seconds')} s > {CAP} s re-walk cap",
                 "anchor": ""}]
    pid = row["module"].split(".")[1]
    source = (project_lean_checkout("apm-proofs") or Path("/nonexistent")) / "problems" / pid / "lean/Main.lean"
    _STATE.update(active=True, walks=[], sentences=[], piece_stack=[])
    try:
        R.proof_body(const, _layer(row["module"], constants), source, const_table=constants(row["module"]),
                     dep_table=_library(), deadline=time.monotonic() + max(6.0, 3 * CAP))
    except Exception as ex:
        _STATE["active"] = False
        return [{"reason": f"unmeasured: re-render failed ({type(ex).__name__})", "anchor": ""}]
    _STATE["active"] = False
    hits, seen = [], set()

    def add(reason, anchor):
        key = (reason.split(":")[0], anchor)
        if key not in seen:
            seen.add(key)
            hits.append({"reason": reason, "anchor": anchor})

    # L1c: a sentence's rewrite piece from another goal occurrence
    for step, out, pieces in _STATE["sentences"]:
        own = getattr(step, "_l1_occ", None)
        if own is None:
            continue
        for piece, text in pieces:
            occ = getattr(piece, "occurrence", None)
            if occ is None or text not in (out or ""):
                continue
            path = _occ_path(occ, own)
            if path is None:
                add("L1c: the sentence's rewrite belongs to a goal outside its step's goal", _anchor(text, proof))
            elif "proof_argument" in path:
                add("L1c: the sentence's rewrite is the proof of an argument ("
                    + " < ".join(path) + "), printed as its step's own", _anchor(text, proof))
    for w in _STATE["walks"][:1]:
        steps = list(w.steps)
        for i, s in enumerate(steps):
            # L1b
            if (i + 1 < len(steps) and not s.argument_claim and s.kind not in _QUIET_KINDS
                    and steps[i + 1].argument_claim and steps[i + 1].depth == s.depth + 1
                    and s.kind in ("let", "have", "fact")):
                add("L1b: a fact is printed before the inline proof of an argument it depends on",
                    _anchor("We have \\(" + s.tex + "\\)", proof) or _anchor(s.tex, proof))
            # L1a
            silent = getattr(s, "_l1_silent", ())
            if L1A and silent and s.kind not in _QUIET_KINDS and s.tex:
                add("L1a: printed under the unstated hypothesis " + ", ".join(silent), _anchor(s.tex, proof))
    _STATE.update(walks=[], sentences=[])
    return hits
