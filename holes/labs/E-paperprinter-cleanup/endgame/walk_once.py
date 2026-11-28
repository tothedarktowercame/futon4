"""walk_once (claude-23, GEN-STALL): one paper walk per census record, shared by the checks that
re-walk (check_D1, check_D2).  Before: each check ran its own `proof_body`, with the walk's
per-proof memo tables off (they are opened by `ApmPaperPrinter.proof_walk`, which the census goes
through and the checks did not), so a record cost two uncached walks.

walk(row, constants) -> (layouts, sentences): every Step list `_layout` laid out (the last is the
proof's final list) and the sentence `_sentence` made for each Step, by id(step).  The result, or
the exception the walk raised (a timeout included), is kept for the record last walked, so the
second check reuses it instead of walking again."""
RULE = None
_MOD = {}
_last = {"key": None, "value": None, "error": None}


def _module(module, constants):
    hit = _MOD.get(module)
    if hit is None:
        import paper_walk_test as T
        from mfuton.lean.blueprint_render.library_statements import LibraryStatements
        from mfuton.lean.blueprint_render.project_display_layer import project_display_layer, with_module_namespaces
        table = constants(module)
        layer = with_module_namespaces(project_display_layer("apm-proofs", home=T.REPO), list(table.values()))
        if "__lib__" not in _MOD:
            _MOD["__lib__"] = LibraryStatements(T.LIBRARY)
        for k in [k for k in _MOD if k != "__lib__"]:  # one module at a time: shards are module-sorted
            del _MOD[k]
        hit = _MOD[module] = (table, layer, T.SOURCE / module.split(".")[1] / "lean/Main.lean")
    return hit


def walk(row, constants):
    key = (row["module"], row["declaration"])
    if _last["key"] == key:
        if _last["error"] is not None:
            raise _last["error"]
        return _last["value"]
    _last.update(key=key, value=None, error=None)
    try:
        _last["value"] = _walk(row, constants)
    except BaseException as ex:
        _last["error"] = ex
        raise
    return _last["value"]


def _walk(row, constants):
    from mfuton.lean.blueprint_render.paper_proof_walk import render as R
    from mfuton.lean.blueprint_render.paper_walk import ApmPaperPrinter
    table, layer, source = _module(row["module"], constants)
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
        ApmPaperPrinter.proof_walk(ci, layer, source, const_table=table, dep_table=_MOD["__lib__"])
    finally:
        R._gather_prelude, R._sentence = orig_gp, orig_s
    return (cap, sentences) if cap else None
