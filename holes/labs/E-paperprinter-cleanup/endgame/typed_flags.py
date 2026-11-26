#!/usr/bin/env python3
"""Typed flags for ink classes that census text cannot decide (R3, 2026-11-26).

R3: a theorem whose proof is `by_contra h` -- its final body is
`Decidable/Classical.byContradiction (fun h => ...)` with an author-named binder that the
body uses -- and whose page closes "..., the hypotheses being contradictory."  On the
page the supposition is never stated when the lambda is walked as plumbing (term_walk
`_walk_inner` plumbing-binder branch).  The text alone cannot tell this from omega's
internal contradiction (hygienic binder) or from genuinely inconsistent case hypotheses
(claude-22 R3-REPORT; text alone flags 324 records, typed + text 63 at 13f55912).  ink.py adds the text half: no
"for contradiction" / "Suppose \\neg" sentence on the page.

Run from a git-archive snapshot of the census head with the gate environment
(PYTHONPATH=src:.:test/mfuton, MFUTON_TYPED_TEST_CORPUS, ...):
    python3 typed_flags.py '<dir>/qNNNh-k-0?.jsonl' <dir>/typed-flags.jsonl
ink.py reads <dir>/typed-flags.jsonl when it sits beside the shards.
"""
import functools
import glob
import json
import sys

sys.path[:0] = ["src", ".", "test/mfuton"]
import paper_walk_test as T  # noqa: E402  (corpus root and entity provider)
from mfuton.lean.blueprint_render.paper_proof_walk import term_walk as W  # noqa: E402

CLOSER = "the hypotheses being contradictory"
FACT_REASON = "— by contradiction."
CONTRADICTION_HEADS = {"Decidable.byContradiction", "Classical.byContradiction"}


@functools.lru_cache(None)
def constants(module):
    provider = T.EmitPythonEntitySurfaceProvider("", corpus_root=str(T.CORPUS))
    return {c.name.dotted(): c for c in provider.elaborated_constants_for_module(module)}


def r3_shape(value):
    """Class of the proof's typed shape: "R3", "R3-hygienic", or False."""
    head, args = W._spine(W._final_body(value))
    if W._expr.ctor_of(head) != "const" or W._expr.const_name(head) not in CONTRADICTION_HEADS:
        return False
    if not args:
        return False
    lam = W._helpers._strip_mdata(args[-1][0])
    if W._expr.ctor_of(lam) != "lam":
        return False
    name = W._expr.binder_name(lam) or ""
    if W._helpers._is_internal(name):
        return "R3-hygienic"  # omega and other tactics' own contradiction: counted, not inked yet
    return "R3" if 0 in W._collect_outer_bvars(W._expr.binder_body(lam)) else False


def refutation_binders(value):
    """Binder kinds of every byContradiction term in ``value`` ("author" / "tactic").

    A tactic's own refutation (omega, linarith, ...) binds a hygienic name; an author's
    `by_contra h` binds h.  Does not descend into a byContradiction's argument."""
    kinds, stack, seen = [], [value], set()
    while stack:
        e = stack.pop()
        if e is None or id(e) in seen:
            continue
        seen.add(id(e))
        head, args = W._spine(W._helpers._strip_mdata(e))
        if W._expr.ctor_of(head) == "const" and W._expr.const_name(head) in CONTRADICTION_HEADS:
            lam = W._helpers._strip_mdata(args[-1][0]) if args else None
            internal = (lam is not None and W._expr.ctor_of(lam) == "lam"
                        and W._helpers._is_internal(W._expr.binder_name(lam) or ""))
            kinds.append("tactic" if internal else "author")
            continue
        stack.extend(e.arguments)
        if e.function is not None:
            stack.append(e.function)
    return kinds


def main(pattern, out_path):
    flagged = checked = errors = 0
    with open(out_path, "w", encoding="utf-8") as out:
        for path in sorted(glob.glob(pattern)):
            for line in open(path, encoding="utf-8"):
                row = json.loads(line)
                proof = row.get("proof") or ""
                if FACT_REASON in proof:
                    # R2 (claude-20, 2026-11-26): "— by contradiction." on an ordinary fact
                    # that a decision procedure proved.  Flag a record only when every
                    # refutation in it is a tactic's own (lower bound: mixed records skipped).
                    try:
                        kinds = refutation_binders(constants(row["module"])[row["declaration"]].value_expression)
                    except Exception as ex:
                        errors += 1
                        print(f"ERROR {row['module']} {row['declaration']}: {str(ex)[:120]}", file=sys.stderr)
                        kinds = []
                    if kinds and set(kinds) == {"tactic"}:
                        out.write(json.dumps({"module": row["module"], "declaration": row["declaration"],
                                              "class": "R2"}, sort_keys=True) + "\n")
                if CLOSER not in proof:
                    continue
                checked += 1
                try:
                    hit = r3_shape(constants(row["module"])[row["declaration"]].value_expression)
                except Exception as ex:  # report, never guess
                    errors += 1
                    print(f"ERROR {row['module']} {row['declaration']}: {str(ex)[:120]}", file=sys.stderr)
                    continue
                if hit:
                    flagged += hit == "R3"
                    out.write(json.dumps({"module": row["module"], "declaration": row["declaration"],
                                          "class": hit}, sort_keys=True) + "\n")
    print(f"checked {checked} closer records; R3-shaped {flagged}; errors {errors}")
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1], sys.argv[2]))
