"""Bad and good cases for suite.py (claude-24, GEN-SUITE). Run: python3 test_suite.py

Synthetic heads (one record each) pin every branch of the state decision; two real cases pin it on
the stored heads: the R2 gaps constructed by make_cases.py, and a defect whose record is clean.
"""
import json, os, sys, tempfile

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import suite

STORAGE = os.path.expanduser("~/storage/apm-endgame")
MOD, DECL = "problems.x00A00.lean.Main", "apm_x00a00_demo"


def _head(proof, flags, rules=("R1", "R2", "D2", "GAP")):
    d = tempfile.mkdtemp(prefix="c24-gs-test-")
    with open(os.path.join(d, "q999h-k-00.jsonl"), "w") as f:
        f.write(json.dumps({"module": MOD, "declaration": DECL, "proof": proof}) + "\n")
    with open(os.path.join(d, "rule-flags.jsonl"), "w") as f:
        for rule, anchor, reason, *um in flags:
            f.write(json.dumps({"module": MOD, "declaration": DECL, "rule": rule, "anchor": anchor,
                                "reason": reason, "unmeasured": bool(um)}) + "\n")
    with open(os.path.join(d, "rule-flags.jsonl.rules"), "w") as f:
        f.write("".join(f"{r}\t0\t0\n" for r in rules))
    return d


def _state(head, rule="R1", anchor="", key="", declaration=DECL, source="rule-precision"):
    recs, flags, rules, _ = suite.load_head(head)
    row = dict(id="t", source=source, rule=rule, module=MOD, declaration=declaration,
               anchor=anchor, key=key, note="")
    return suite.decide(row, recs, flags, rules, {})


PAGE = ("Let \\(x\\) be given.\n\\begin{itemize}\n"
        "\\item We have \\(x \\in S\\) — immediate from its type. ⟦1⟧\n"
        "\\item We have \\(f(x) = 0\\) — by algebra. ⟦2⟧\n\\end{itemize}\n\nHence the claim.")


def test_failing_flag_at_anchor():
    h = _head(PAGE, [("R1", "\\item We have \\(f(x) = 0\\) — by algebra.", "step h2: uses (1)=hx, not cited")])
    assert _state(h, anchor="We have f(x) = 0 — by algebra")[0] == "failing"


def test_flag_elsewhere_is_not_at_the_anchor():
    h = _head(PAGE, [("R1", "\\item We have \\(f(x) = 0\\) — by algebra.", "step h2: uses (1)=hx, not cited")])
    # an ink row: the flag elsewhere does not count at the anchor
    assert _state(h, anchor="We have x ∈ S — immediate from its type", source="ink@test")[0] == "passing"
    # a real read whose sentence is still printed is failing (recall miss), not passing (claude-16 review)
    st = _state(h, anchor="We have x ∈ S — immediate from its type")
    assert st[0] == "failing" and "recall miss" in st[1]


def test_gapped_when_the_step_became_a_gap():
    gap = PAGE.replace("— immediate from its type", "\\grammargap{R2}{no typed reason for this step}")
    h = _head(gap, [("GAP", "\\item We have \\(x \\in S\\) \\grammargap{R2}{no typed", "gap: R2 no typed reason for this step")])
    st, det = _state(h, rule="R2", anchor="We have \\(x \\in S\\) — immediate from its type. ⟦1⟧")
    assert st == "gapped" and "R2" in det, (st, det)


def test_gap_wins_over_a_flag_in_the_same_region():
    gap = PAGE.replace("— by algebra", "\\grammargap{R2}{no typed reason for this step}")
    h = _head(gap, [("R1", "\\item We have \\(f(x) = 0\\)", "step h2: uses (1)=hx, not cited")])
    assert _state(h, anchor="We have f(x) = 0 — by algebra")[0] == "gapped"


def test_short_prefix_without_a_gap_is_gone():
    # three matching words not followed by a gap do not locate the anchor
    h = _head(PAGE, [])
    assert _state(h, anchor="We have f(x) = 1 — by the mean value theorem")[1].startswith("anchor gone")


def test_passing_when_anchor_gone_and_record_clean():
    h = _head(PAGE, [("D2", "", "missing closer: ...")])
    st, det = _state(h, anchor="We have y ∈ T — by Lemma 3")
    assert st == "passing" and "clean" in det, (st, det)


def test_stale_when_anchor_gone_and_rule_still_flags():
    h = _head(PAGE, [("R1", "\\item We have \\(f(x) = 0\\)", "step h2: uses (1)=hx, not cited")])
    assert _state(h, anchor="We have y ∈ T — by Lemma 3")[0] == "stale"


def test_typed_locator_matches_by_reason():
    h = _head(PAGE, [("D2", "", "missing closer: the block of part (unnamed) ends on a witness Step")])
    assert _state(h, rule="D2", key="match:ends on a witness Step", source="rule-validation")[0] == "failing"
    assert _state(h, rule="D2", key="match:closed twice", source="rule-validation")[0] == "passing"


def test_stale_without_record_or_rule():
    h = _head(PAGE, [], rules=("R1",))
    assert _state(h, declaration="other")[0] == "stale"
    assert _state(h, declaration="")[0] == "stale"
    assert _state(h, rule="T1", anchor="We have f(x) = 0")[0] == "stale"


def test_only_unmeasured_flags_are_stale():
    h = _head(PAGE, [("R1", "\\item We have \\(f(x) = 0\\)", "unmeasured: fact h2: tag (2) unresolved", 1)])
    assert _state(h, anchor="We have f(x) = 0 — by algebra")[0] == "stale"


def _real_rows(path):
    return suite._tsv(path)


def test_real_r2_gaps_read_gapped_at_c8415d75():
    """make_cases.py rows: the c8076d84 sentence '… — immediate from its type' a reader marks; at
    c8415d75 the step prints \\grammargap{R2}. Every constructed row must read gapped there."""
    rows = _real_rows(os.path.join(os.path.dirname(os.path.abspath(__file__)), "gap-rows.tsv"))
    recs, flags, rules, _ = suite.load_head(os.path.join(STORAGE, "head-c8415d75"))
    cache, bad = {}, []
    for r in sorted(rows, key=lambda r: r["declaration"]):
        st, det = suite.decide(r, recs, flags, rules, cache)
        if st != "gapped":
            bad.append((r["declaration"], st, det))
    assert len(rows) >= 100 and not bad, (len(rows), bad[:5])


def test_real_clean_record_reads_passing():
    """prec-100: a D4 restate read real (t96j05 exists_regular_radius, 'Hence ∀x ∃x'); at
    c8415d75 the sentence is gone and D4 flags nothing on the record: passing."""
    rows = [r for r in suite._tsv(os.path.join(os.path.dirname(os.path.abspath(__file__)), "suite-rows.tsv"))
            if r["source"] == "rule-precision" and r["declaration"] == "apm_t96j05_exists_regular_radius"
            and r["rule"] == "D4"]
    recs, flags, rules, _ = suite.load_head(os.path.join(STORAGE, "head-c8415d75"))
    assert rows
    assert not [f for f in flags[(rows[0]["module"], rows[0]["declaration"])] if f["rule"] == "D4"]
    st, det = suite.decide(rows[0], recs, flags, rules, {})
    assert st == "passing", (st, det)


if __name__ == "__main__":
    fails = 0
    for name, fn in sorted(globals().items()):
        if name.startswith("test_") and callable(fn):
            try:
                fn()
                print("ok  ", name)
            except AssertionError as e:
                fails += 1
                print("FAIL", name, e)
    print(f"{fails} failed")
    sys.exit(1 if fails else 0)
