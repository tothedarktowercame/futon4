#!/usr/bin/env python3
"""Constructed suite rows for the bad cases (claude-24, GEN-SUITE).

gap-rows.tsv: one row per GAP marker at head NEW (the R2 gaps) whose step, at head OLD, printed
"immediate from its type". The row is what a reader marking that sentence at OLD would have
written: rule R2, the record, and the OLD sentence as the anchor. At NEW it must read gapped.

usage: make_cases.py OLD_HEAD_DIR NEW_HEAD_DIR OUTDIR
"""
import json, os, re, sys
from collections import defaultdict

import suite


def main(old, new, outdir):
    recs_o, flags_o, _, _ = suite.load_head(old)
    recs_n, flags_n, _, _ = suite.load_head(new)
    gap_rows, n_gaps, why = [], 0, defaultdict(int)
    for key, fs in sorted(flags_n.items()):
        for f in fs:
            if f["rule"] != "GAP":
                continue
            n_gaps += 1
            if key not in recs_o:
                why["record not at old head"] += 1
                continue
            vis = suite._visible(recs_o[key])
            kind, reg = suite.locate(vis, suite.tokens(vis), suite.anchor_words(f["anchor"]))
            if kind not in ("found", "partial"):
                why["step not located at old head"] += 1
                continue
            seg = vis[reg[0]:reg[1]]
            if "immediate from its type" not in seg:
                why["old step does not read 'immediate from its type'"] += 1
                continue
            seg = re.sub(r"^\\item\s*", "", seg.strip())
            gap_rows.append(dict(id=f"gapcase-{len(gap_rows) + 1:03d}", source="constructed-gap",
                                 rule="R2", module=key[0], declaration=key[1], anchor=seg,
                                 key="", note=f"reader mark at old head: {seg[:80]}"))
    suite._write(os.path.join(outdir, "gap-rows.tsv"), suite.COLS, gap_rows)
    print(f"{n_gaps} GAP markers at new head; {len(gap_rows)} rows constructed; skipped {dict(why)}")


if __name__ == "__main__":
    main(*sys.argv[1:4])
