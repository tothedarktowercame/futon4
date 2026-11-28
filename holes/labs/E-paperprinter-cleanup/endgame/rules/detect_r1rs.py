"""R1RS detector: a fact printed twice, once as stated and once as the form its
proof's transport re-reads it in.  Runs the paper walk per census record and
inspects the final Step list (the one `proof_body` lays out).  Typed data only:
Step identity (`transported_to_owner`, `premises`), Step kinds/species/depths, and
the scoped structural key of the steps' proposition expressions.

Usage (from a snapshot root, typed-flags-env.sh sourced):
  python detect_r1rs.py OUT.jsonl CENSUS.jsonl... [--workers N]
"""
from __future__ import annotations
import json, sys, os, time, traceback
from multiprocessing import Pool

sys.setrecursionlimit(20000)
sys.path.insert(0, "test/mfuton")


def _key(e):
    from mfuton.lean.blueprint_render import paper_walk
    if e is None:
        return None
    try:
        return paper_walk._scoped_case_key(e, e.loose_bound_variable_range)
    except Exception:
        return None


_PLUMB = {"congrArg", "congr", "congrFun", "id", "Eq.refl", "rfl", "propext", "Eq.mpr", "Eq.mp",
          "Eq.trans", "Eq.symm", "of_eq_true", "eq_self", "funext", "implies_congr", "forall_congr",
          "eq_true", "Iff.refl", "iff_self", "Eq", "Iff", "True", "letFun"}


def _consts(e, acc, fuel=[0]):
    from mfuton.lean.latex_pp import _expr
    from mfuton.lean.blueprint_render.paper_proof_walk import helpers as H
    from mfuton.lean.blueprint_render.paper_proof_walk.term_walk import _spine
    stack = [e]
    seen = 0
    while stack and seen < 200000:
        seen += 1
        x = H._strip_mdata(stack.pop())
        k = _expr.ctor_of(x)
        if k == "const":
            acc.add(_expr.const_name(x))
        elif k == "app":
            h, args = _spine(x)
            stack.append(h)
            stack.extend(a[0] if isinstance(a, tuple) else a for a in args)
        elif k in ("lam", "forallE"):
            stack.append(_expr.binder_domain(x)); stack.append(_expr.binder_body(x))
        elif k == "letE":
            stack.append(_expr.binder_domain(x)); stack.append(_expr.let_value(x)); stack.append(_expr.binder_body(x))
    return acc


#: id(transport target expression) -> classification, filled by the walk hook below
TRANSPORTS = {}
DEBUG = []


def _classify_transport(argv):
    """Typed reading of one `Eq.mpr`/`Eq.mp` transport (α, β, h : α = β, p)."""
    from mfuton.lean.latex_pp import _expr
    from mfuton.lean.blueprint_render.paper_proof_walk import helpers as H
    from mfuton.lean.blueprint_render.paper_proof_walk.term_walk import _spine, _strip_const
    inner = H._strip_mdata(argv[3])
    for _ in range(8):
        head, _a = _spine(inner)
        name = _strip_const(_expr.const_name(head)) if _expr.ctor_of(head) == "const" else ""
        if name in ("id", "Eq.symm", "letFun") or _expr.ctor_of(inner) == "mdata":
            nxt = H._unwrap_plumbing(inner)
            if nxt is inner:
                break
            inner = H._strip_mdata(nxt)
            continue
        break
    head, _a = _spine(H._strip_mdata(inner))
    inner_transport = (_expr.ctor_of(head) == "const"
                       and _strip_const(_expr.const_name(head)) in ("Eq.mp", "Eq.mpr"))
    if os.environ.get("R1RS_NO_LEMMAS"):
        stated, lemma_names = set(), set()
    else:
        stated = _consts(argv[0], set()) | _consts(argv[1], set())
        lemma_names = _consts(argv[2], set())
    lemmas = sorted(c for c in lemma_names
                    if c not in stated and _strip_const(c) not in _PLUMB
                    and not c.startswith("inst") and ".inst" not in c and "._@." not in c)
    # Locals the equality proof uses that neither side of the transport mentions: the
    # hypotheses a conditional rewrite consumes (`abs_of_nonneg h`, `Multiset.cons_erase h`).
    # None means the two forms differ by unfolding/normal-form identities alone.
    from mfuton.lean.blueprint_render.paper_proof_walk.term_walk import _collect_outer_bvars
    try:
        extra = (_collect_outer_bvars(argv[2]) - _collect_outer_bvars(argv[0])
                 - _collect_outer_bvars(argv[1]))
    except Exception:
        extra = {-1}
    info = dict(both_sides=inner_transport, lemmas=lemmas[:12], uses_local_hyps=len(extra))
    inner_target = None
    if inner_transport:
        inner_argv = [a for a, _bi in _a]
        if len(inner_argv) >= 4:
            inner_target = inner_argv[1]
    return info, inner_target


def install_hook():
    from mfuton.lean.latex_pp import _expr
    from mfuton.lean.blueprint_render.paper_proof_walk import term_walk as TW
    from mfuton.lean.blueprint_render.paper_proof_walk import helpers as H
    if getattr(TW._Walk, "_r1rs_hooked", False):
        return
    orig = TW._Walk._walk
    def walk(self, e):
        try:
            x = H._strip_mdata(e)
            if _expr.ctor_of(x) == "app":
                head, args = TW._spine(x)
                if (_expr.ctor_of(head) == "const"
                        and TW._strip_const(_expr.const_name(head)) in ("Eq.mpr", "Eq.mp")
                        and len(args) >= 4):
                    argv = [a for a, _bi in args]
                    # the walk sets goal_expr = argv[1] for both heads (term_walk Eq.mpr branch)
                    target = argv[1]
                    if id(target) not in TRANSPORTS:
                        info, inner_target = _classify_transport(argv)
                        TRANSPORTS[id(target)] = (target, info)
                        if inner_target is not None:
                            TRANSPORTS[id(inner_target)] = (inner_target, dict(info, via="inner Eq.mp"))
        except Exception:
            pass
        return orig(self, e)
    TW._Walk._walk = walk
    TW._Walk._r1rs_hooked = True


def _transport_of(step):
    hit = TRANSPORTS.get(id(step.expr)) if step.expr is not None else None
    return hit[1] if hit is not None and hit[0] is step.expr else None


def _block(steps, i):
    """Visible steps strictly deeper than steps[i] that directly follow it."""
    owner = steps[i]
    out, j = [], i + 1
    while j < len(steps) and steps[j].depth > owner.depth:
        if steps[j].kind != "silent":
            out.append(j)
        j += 1
    return out


def detect(steps):
    from mfuton.lean.blueprint_render.paper_proof_walk import term_walk as TW
    hits = []
    for i, owner in enumerate(steps):
        if owner.kind == "silent":
            continue
        # Shape A: a claim whose sole visible child is the closer its own transport minted.
        if owner.kind in ("have", "let", "part") and owner.case_closure != "duplicate_closer_removed":
            kids = _block(steps, i)
            if len(kids) == 1:
                child = steps[kids[0]]
                if child.kind == "closer" and child.transported_to_owner is owner:
                    ko, kc = _key(owner.expr), _key(child.expr)
                    same = ko is not None and ko == kc
                    hits.append(dict(shape="A", owner=i, child=kids[0], same_prop=same,
                                     transport=_transport_of(child),
                                     owner_chain=bool(owner.rewrite_chain or owner.rewrite_pieces),
                                     owner_reason=bool(owner.justified_by or owner.premises),
                                     owner_species=owner.species,
                                     through=getattr(child, "through_restating_transport", False)))
        # Shape B: a closer that only restates the fact immediately before it, in another form.
        if (owner.kind == "closer" and owner.species == "restate" and len(owner.premises) == 1):
            prev = i - 1
            while prev >= 0 and steps[prev].kind == "silent":
                prev -= 1
            if prev >= 0 and steps[prev].depth == owner.depth and steps[prev].kind in TW.FACT_KINDS:
                p = steps[prev]
                prem = owner.premises[0]
                cites_prev = (prem is p) or (isinstance(prem, str) and p.tag and prem == f"({p.tag})")
                if cites_prev:
                    kp, kc = _key(p.expr), _key(owner.expr)
                    hits.append(dict(shape="B", owner=prev, child=i, transport=_transport_of(owner),
                                     same_prop=(kp is not None and kp == kc),
                                     owner_chain=bool(p.rewrite_chain or p.rewrite_pieces),
                                     owner_reason=bool(p.justified_by or p.premises),
                                     owner_species=p.species,
                                     through=getattr(owner, "through_restating_transport", False)))
    return hits


def _plain(tex):
    import re
    tex = re.sub(r"\\leanref\{\}\{[^}]*\}", "", tex or "")
    for _ in range(8):
        tex = re.sub(r"\\leanref\{((?:[^{}]|\{(?:[^{}]|\{[^{}]*\})*\})*)\}\{[^}]*\}", r"\1", tex)
    return tex


def run_module(args):
    module, decls = args
    import paper_walk_test as T
    from mfuton.lean.blueprint_render.paper_proof_walk import render as R
    from mfuton.lean.blueprint_render.paper_proof_walk import proof_body
    from mfuton.lean.blueprint_render.library_statements import LibraryStatements
    from mfuton.lean.blueprint_render.project_display_layer import project_display_layer, with_module_namespaces
    from mfuton.lean.entity_surface.source_extraction.population_pipeline import EmitPythonEntitySurfaceProvider
    install_hook()
    cap = []
    orig = R._gather_prelude
    def gp(steps):
        cap.append(list(steps)); return orig(steps)
    R._gather_prelude = gp
    out = []
    try:
        constants = EmitPythonEntitySurfaceProvider("", corpus_root=str(T.CORPUS)).elaborated_constants_for_module(module)
        table = {c.name.dotted(): c for c in constants}
        layer = with_module_namespaces(project_display_layer("apm-proofs", home=T.REPO), constants)
        pid = module.split(".")[1]
        lib = LibraryStatements(T.LIBRARY)
    except Exception as exc:
        return [dict(module=module, error="load: " + repr(exc)[:200])]
    for decl in decls:
        if decl not in table:
            out.append(dict(module=module, declaration=decl, error="missing")); continue
        cap.clear(); TRANSPORTS.clear()
        try:
            proof_body(table[decl], layer, T.SOURCE / pid / "lean/Main.lean", const_table=table, dep_table=lib)
        except Exception as exc:
            out.append(dict(module=module, declaration=decl, error=repr(exc)[:200])); continue
        if not cap:
            out.append(dict(module=module, declaration=decl, hits=[])); continue
        steps = cap[-1]
        hits = detect(steps)
        for h in hits:
            o, c = steps[h["owner"]], steps[h["child"]]
            h["owner_tex"] = _plain(o.tex)[:400]; h["child_tex"] = _plain(c.tex)[:400]
            h["owner_kind"], h["child_kind"] = o.kind, c.kind
            h["owner_tag"], h["child_jb"] = o.tag, c.justified_by
            h["child_premises"] = [p if isinstance(p, str) else f"step:{getattr(p,'tag',0)}" for p in c.premises]
        out.append(dict(module=module, declaration=decl, hits=hits))
    return out


def main():
    argv = sys.argv[1:]
    workers = 1
    if "--workers" in argv:
        k = argv.index("--workers"); workers = int(argv[k + 1]); del argv[k:k + 2]
    out_path, census = argv[0], argv[1:]
    by_mod = {}
    for f in census:
        for line in open(f):
            r = json.loads(line)
            by_mod.setdefault(r["module"], []).append(r["declaration"])
    jobs = sorted(by_mod.items())
    t0 = time.time()
    with open(out_path, "w") as fh, Pool(workers, maxtasksperchild=4) as pool:
        for n, rows in enumerate(pool.imap_unordered(run_module, jobs), 1):
            for row in rows:
                fh.write(json.dumps(row) + "\n")
            fh.flush()
            print(f"{n}/{len(jobs)} {time.time()-t0:.0f}s", file=sys.stderr, flush=True)


if __name__ == "__main__":
    main()
