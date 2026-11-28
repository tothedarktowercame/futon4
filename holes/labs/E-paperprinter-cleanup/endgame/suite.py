#!/usr/bin/env python3
"""The known-defect suite: every known defect is a test with three states at a head.

GRAMMAR.md "Generation": "Known defects are the acceptance suite ... failing (the defect is still
printed), gapped (the step is now a gap) and passing (an admitted form prints it correctly)."

Two commands.

  suite.py build [--endgame DIR] [--tests paper_walk_test.py] [--ink-heads DIR] --out suite-rows.tsv
      Writes the suite definition, one row per known defect, from:
        - rule-validation.tsv rows with expect=flag and an empty find (head cases: defects the head
          printed when the case was written). Locator: the rule, the record and the case's `match`.
        - rule-precision.tsv rows with verdict real (flags read real on a page). Locator: the rule,
          the record, the flag's reason, and a quoted page fragment from the note when it has one.
        - defect-pins.tsv (defects pinned by a test). The record is taken from the test's body (the
          module and declaration it walks); the anchor is the note's first quoted fragment.
        - residual-ink annotations of every stored head (the extractor's ink marks, the old ink
          classes), deduplicated by (declaration, class, anchor), earliest head kept. The class's
          rule comes from class-rules.tsv; classes whose rule is "-" (not a defect) are left out.
      The definition is frozen, so the states at two heads are comparable.

  suite.py state HEADDIR [--rows suite-rows.tsv] [--out DIR]
      Reads HEADDIR's census records (q*-k-*.jsonl, field "proof" is the page) and rule-flags.jsonl
      (GAP rows are the \\grammargap markers) and writes suite-<head>.tsv with a state per row, then
      prints the counts per rule.

How a state is decided (per row):
  - The record (module, declaration) must be in the head's census, else STALE (no record).
  - The anchor is located in the page by its word sequence: \\leanref targets removed
    (leanism_scan_p1.visible), LaTeX commands dropped, \\mathbb-style letters dropped, the
    remaining [A-Za-z0-9] runs compared. A quoted note fragment is split at "…" and its longest
    piece is used. If the whole sequence is not found, its longest prefix of at least half its
    words (and at least 4) is tried ("found, partial"): a step whose reason became a gap keeps its
    statement words. The anchor's region is the enclosing \\item or paragraph.
  - A flag is AT the anchor when its own anchor is located in a region that overlaps the row's
    region, or, for a typed flag (no anchor) or a row without an anchor, when its reason matches
    the row's key (the case's `match` as a substring; a read reason by word overlap >= 0.6).
  - GAPPED: a \\grammargap of the row's rule lies in the anchor's region, or one of any rule lies in
    the anchor's own item (a step whose reason became a gap; the gap's rule is recorded), or,
    for a row without an anchor, the record carries a gap of the row's rule and no flag matches.
  - FAILING: a flag of the row's rule is at the anchor and no gap covers it.
  - PASSING: the anchor is located and nothing of the rule is at it; or the anchor text is gone
    and the rule's check is clean on the record (it flags nothing of that rule there).
  - STALE (undecidable, counted): no record; the anchor is gone but the rule still flags the
    record elsewhere (the defect may have moved); the only flags at the anchor are "unmeasured";
    the rule was not run at the head; the row has no record locator; the rule has a record-level
    (anchorless) unmeasured flag on the record and would otherwise read passing.
  An ink row's region is its span's length from the anchor (the span is a paragraph or a whole
  itemize block; offsets do not survive a re-render, lengths roughly do), ended on a boundary.
  For ink rows the detail also says whether the head's own residual-ink annotations still mark
  the same span (a cross-check by the ink extractor, not part of the state).
  The detail column says which branch decided it. "passing, anchor verbatim" marks a row whose
  defect text is still printed word for word while the check is silent on it: by the definition
  that is passing, but it is a check that does not see the defect (recall), not a fix.

Misses: an anchor of fewer than 3 words, or one that is mostly symbols, cannot be located and
counts as gone; offsets of ink spans are not used (they do not survive a re-render); reader
findings with no record locator (InkUp intake entries) are not in the suite.
"""
import argparse, csv, glob, gzip, json, os, re, sys
from collections import Counter, defaultdict

ENDGAME = os.path.dirname(os.path.abspath(__file__))
STORAGE = os.path.expanduser("~/storage/apm-endgame")
COLS = ["id", "source", "rule", "module", "declaration", "anchor", "key", "note"]


def _visible(text):
    sys.path.insert(0, ENDGAME)
    from leanism_scan_p1 import visible
    try:
        return visible(text or "")
    except Exception:
        return text or ""


# ---- word-sequence locator --------------------------------------------------------------------
_TOK = re.compile(r"\\[A-Za-z]+|[A-Za-z0-9]+")
_DROP_NEXT = {"\\mathbb", "\\mathcal", "\\mathfrak", "\\mathscr", "\\mathbf"}


def tokens(vis):
    """[(word, start, end)] of a visible text; commands dropped, \\mathbb{X} letters dropped."""
    out, skip = [], False
    for m in _TOK.finditer(vis):
        w = m.group(0)
        if w.startswith("\\"):
            skip = w in _DROP_NEXT
            continue
        if skip:
            skip = False
            continue
        out.append((w, m.start(), m.end()))
    return out


def _find(seq, words):
    n = len(words)
    for i in range(len(seq) - n + 1):
        if seq[i:i + n] == words:
            return i
    return -1


def anchor_words(anchor):
    anchor = re.sub(r"\\grammargap\{[^}]*\}\{[^}]*\}", " ", anchor or "")
    # flag anchors are cut fragments: drop a leading piece of a \leanref target and local targets
    anchor = re.sub(r"^[^\s\\{}]*?\}|^\w*ref\{\}\{[^}]*\}", "", anchor)
    anchor = re.sub(r"\{local:[^}]*\}", "{}", anchor)
    if not anchor.strip():
        return []
    pieces = [p for p in re.split(r"…|\.\.\.|\\ldots", anchor)]
    best = max(pieces, key=lambda p: len(tokens(_visible(p))))
    return [w for w, _, _ in tokens(_visible(best))]


def region(vis, start, end):
    """Expand [start, end) to the enclosing \\item or paragraph."""
    cuts = [vis.rfind("\\item", 0, start + 1), vis.rfind("\n\n", 0, start)]
    s = max(c for c in cuts + [0])
    nxt = [c for c in (vis.find(t, end) for t in ("\\item", "\n\n", "\\begin{", "\\end{")) if c >= 0]
    e = min(nxt) if nxt else len(vis)
    return s, max(e, end)


def locate(vis, toks, words):
    """(kind, (s, e)) with kind found | partial | gone | none."""
    if not words:
        return "none", None
    if len(words) < 3:
        return "gone", None
    seq = [w for w, _, _ in toks]
    i = _find(seq, words)
    kind = "found"
    if i < 0:
        kind = "partial"
        lo = max(4, (len(words) + 1) // 2)
        for n in range(len(words) - 1, lo - 1, -1):
            i = _find(seq, words[:n])
            if i >= 0:
                words = words[:n]
                break
    if i < 0:
        # the step's tail became a gap: a prefix of >= 3 words followed directly by \grammargap
        for n in range(len(words) - 1, 2, -1):
            for j in range(len(seq) - n + 1):
                if seq[j:j + n] == words[:n] and \
                        re.match(r"(?:\s|\\\)|\\(?!grammargap)[A-Za-z]+|[(){}\[\]])*\\grammargap",
                                 vis[toks[j + n - 1][2]:]):
                    return "partial", region(vis, toks[j][1], toks[j + n - 1][2])
        return "gone", None
    return kind, region(vis, toks[i][1], toks[i + len(words) - 1][2])


def _overlap(a, b):
    return a and b and a[0] < b[1] and b[0] < a[1]


def _jac(a, b):
    A, B = set(re.findall(r"[A-Za-z]+", a.lower())), set(re.findall(r"[A-Za-z]+", b.lower()))
    return len(A & B) / max(1, len(A | B))


def key_matches(key, keykind, reason):
    if not key:
        return True
    if keykind == "match":
        return key in reason
    return _jac(key, reason) >= 0.6


# ---- build ------------------------------------------------------------------------------------
def _tsv(path):
    with open(path) as f:
        lines = [l.rstrip("\n") for l in f if not l.startswith("#") and l.strip()]
    head = lines[0].split("\t")
    return [dict(zip(head, [c.replace("⏎", "\n") for c in (l.split("\t") + [""] * len(head))[:len(head)]]))
            for l in lines[1:]]


def _cell(v):
    return str(v or "").replace("\t", " ").replace("\n", "⏎")


def _write(path, cols, rows):
    with open(path, "w") as f:
        f.write("\t".join(cols) + "\n")
        for r in rows:
            f.write("\t".join(_cell(r.get(k, "")) for k in cols) + "\n")


def _first_quote(note):
    m = re.search(r"[\"“']([^\"”']{8,})[\"”']", note or "")
    return m.group(1) if m else ""


_CODE = re.compile(r"^(?:[abmt]\d\d[AJ]\d\d|aunk\d\d|[a-z]\d\d[a-z]\d\d)$", re.I)


def _pin_locator(body, census_keys):
    lits = re.findall(r"\"([^\"\n]+)\"", body)
    mods = {k[0] for k in census_keys}
    by_decl = defaultdict(set)
    for m, d in census_keys:
        by_decl[d].add(m)
    module = None
    for s in lits:
        if s.startswith("problems.") and s in mods:
            module = s
            break
        if _CODE.match(s):
            cand = [m for m in mods if m.lower() == f"problems.{s}.lean.main".lower()]
            if cand:
                module = cand[0]
                break
    for s in lits:
        if s in by_decl and (module is None or module in by_decl[s]):
            return (module or sorted(by_decl[s])[0]), s
    return module or "", ""


def build(args):
    E = args.endgame
    rows = []
    # census keys from the newest head, for the pin locator
    keys, pages = set(), defaultdict(list)
    for fn in glob.glob(os.path.join(args.keys_head, "q*-k-*.jsonl")):
        for l in open(fn):
            r = json.loads(l)
            keys.add((r["module"], r["declaration"]))
            pages[r["module"]].append((r["declaration"], r.get("proof") or ""))
    for r in _tsv(os.path.join(E, "rule-validation.tsv")):
        if r["expect"] == "flag" and not r["find"]:
            rows.append(dict(id=r["id"], source="rule-validation", rule=r["rule"], module=r["module"],
                             declaration=r["declaration"], anchor="", key="match:" + r["match"],
                             note=r["note"]))
    n = 0
    for r in _tsv(os.path.join(E, "rule-precision.tsv")):
        if r.get("verdict") != "real":
            continue
        n += 1
        rows.append(dict(id=f"prec-{n:03d}", source="rule-precision", rule=r["rule"],
                         module=r["module"], declaration=r["declaration"],
                         anchor=_first_quote(r.get("note", "")), key="reason:" + r["reason"],
                         note=r.get("note", "")))
    src = open(args.tests).read() if args.tests else ""
    for r in _tsv(os.path.join(E, "defect-pins.tsv")):
        m = re.search(rf"\ndef {re.escape(r['test'])}\(.*?(?=\ndef |\Z)", src, re.S)
        mod, decl = _pin_locator(m.group(0), keys) if m else ("", "")
        anchor = _first_quote(r["note"])
        code = re.match(r"\s*([A-Za-z]{1,4}\d\d[A-Za-z]\d\d)\b", r["note"])
        if not decl and code and anchor:
            # a synthetic test: find the record the note quotes, in the module it names
            want = anchor_words(anchor)
            mods = [k for k in pages if k.lower() == f"problems.{code.group(1)}.lean.main".lower()]
            hits = []
            for d, p in (pages[mods[0]] if mods else []):
                vis = _visible(p)
                if locate(vis, tokens(vis), want)[0] == "found":
                    hits.append(d)
            if len(hits) == 1:
                mod, decl = mods[0], hits[0]
        rows.append(dict(id="pin-" + r["test"], source="defect-pins:" + r["group"], rule=r["rule"],
                         module=mod, declaration=decl, anchor=anchor, key="",
                         note=r["note"]))
    cr = {r["class"]: r["rule"] for r in _tsv(os.path.join(E, "class-rules.tsv"))}
    seen, unmapped = set(), Counter()
    files = sorted(glob.glob(os.path.join(args.ink_heads, "head-*/residual-ink-annotations.jsonl.gz")),
                   key=os.path.getmtime)
    for fn in files:
        head = os.path.basename(os.path.dirname(fn))[5:13]
        for l in gzip.open(fn):
            a = json.loads(l)
            for s in a["spans"]:
                k = (a["declaration"], s["class"], s["anchor"])
                if k in seen:
                    continue
                seen.add(k)
                rule = cr.get(s["class"])
                if rule in (None, "-"):
                    unmapped[(s["class"], rule)] += 1
                    continue
                rows.append(dict(id=f"ink-{len(seen):05d}", source=f"ink@{head}", rule=rule,
                                 module=a["module"], declaration=a["declaration"],
                                 anchor=s["anchor"], key=f"span:{s['end'] - s['start']}",
                                 note=f"class {s['class']}"))
    _write(args.out, COLS, rows)
    print(f"{len(rows)} rows -> {args.out}", Counter(r["source"].split("@")[0].split(":")[0] for r in rows))
    if unmapped:
        print("left out (class rule - or unknown):", dict(unmapped))


# ---- state ------------------------------------------------------------------------------------
def load_head(hd):
    recs = {}
    for fn in sorted(glob.glob(os.path.join(hd, "q*-k-*.jsonl"))):
        for l in open(fn):
            r = json.loads(l)
            recs[(r["module"], r["declaration"])] = r.get("proof") or ""
    flags = defaultdict(list)
    for l in open(os.path.join(hd, "rule-flags.jsonl")):
        f = json.loads(l)
        flags[(f["module"], f["declaration"])].append(f)
    rules = set()
    rp = os.path.join(hd, "rule-flags.jsonl.rules")
    if os.path.exists(rp):
        rules = {l.split("\t")[0] for l in open(rp) if l.strip()}
    else:
        rules = {f["rule"] for fs in flags.values() for f in fs}
    inkmarks = defaultdict(set)
    ap = os.path.join(hd, "residual-ink-annotations.jsonl.gz")
    if os.path.exists(ap):
        for l in gzip.open(ap):
            a = json.loads(l)
            for sp in a["spans"]:
                inkmarks[(a["module"], a["declaration"])].add((sp["class"], sp["anchor"]))
    return recs, flags, rules, inkmarks


_GAP = re.compile(r"\\grammargap\{([^}]*)\}")


def decide(row, recs, flags, rules, cache):
    rule = row["rule"]
    key = (row["module"], row["declaration"])
    if not row["declaration"]:
        return "stale", "no record locator"
    if rule not in rules:
        return "stale", f"rule {rule} not run at this head"
    if key not in recs:
        return "stale", "record not in census"
    if key not in cache:
        vis = _visible(recs[key])
        toks = tokens(vis)
        fl = []
        for f in flags.get(key, []):
            if f["anchor"]:
                k2, reg = locate(vis, toks, anchor_words(f["anchor"]))
                fl.append((f, reg if k2 in ("found", "partial") else None))
            else:
                fl.append((f, None))
        cache.clear()
        cache[key] = (vis, toks, fl)
    vis, toks, fl = cache[key]
    kk, _, kv = row["key"].partition(":")
    words = [w for w in anchor_words(row["anchor"])]
    kind, reg = locate(vis, toks, words)
    item = reg
    if kk == "span" and reg is not None:
        # an ink span covers a paragraph or an itemize block: keep its length, end on a boundary
        reg = region(vis, reg[0], min(len(vis), reg[0] + int(kv)))
        kk, kv = "", ""
    mine = [(f, r) for f, r in fl if f["rule"] == rule]
    real = [(f, r) for f, r in mine if not f.get("unmeasured")]
    gaps_rec = [(f, r) for f, r in fl if f["rule"] == "GAP"]

    def at(pairs):
        out = []
        for f, r in pairs:
            if reg is not None and r is not None:
                if _overlap(reg, r):
                    out.append(f)
            elif key_matches(kv, kk, f["reason"]) and (kv or reg is None):
                out.append(f)
        return out

    if kind in ("found", "partial"):
        g = sorted({m for m in _GAP.findall(vis[item[0]:item[1]])} |
                   {m for m in _GAP.findall(vis[reg[0]:reg[1]]) if m == rule})
        if g:
            return "gapped", f"anchor {kind}; gap {','.join(g)} at it"
        hit = at(real)
        if hit:
            return "failing", f"anchor {kind}; {hit[0]['reason'][:90]}"
        if at([p for p in mine if p[0].get("unmeasured")]):
            return "stale", f"anchor {kind}; only unmeasured flags at it"
        if any(f.get("unmeasured") and not f["anchor"] for f, _ in mine):
            return "stale", f"anchor {kind}; {rule} unmeasured on the record (typed, no anchor)"
        if kind == "found" and row["source"] == "rule-precision":
            # claude-16 review: a defect read as real whose sentence is still printed word for word
            # is not fixed; the check's silence is a recall miss (m94J02 "We have n ≠ 0.", D3).
            return "failing", f"anchor found verbatim; read real, {rule} check silent (recall miss)"
        verb = " verbatim" if kind == "found" and row["source"] != "rule-validation" else ""
        return "passing", f"anchor {kind}{verb}; no {rule} flag at it"
    if kind == "none":
        hit = at(real)
        if hit:
            return "failing", f"typed locator; {hit[0]['reason'][:90]}"
        rg = [f for f, _ in gaps_rec if f["reason"].startswith(f"gap: {rule} ")] or \
             [m for m in _GAP.findall(vis) if m == rule]
        if rg:
            return "gapped", f"typed locator; record carries a {rule} gap and no matching flag"
        if at([p for p in mine if p[0].get("unmeasured")]):
            return "stale", "typed locator; only unmeasured flags match"
        return "passing", f"typed locator; no matching {rule} flag"
    # anchor gone
    if kv:
        hit = [f for f, _ in real if key_matches(kv, kk, f["reason"])]
        if hit:
            return "failing", f"anchor gone; reason still flagged: {hit[0]['reason'][:80]}"
    if not real and not any(f.get("unmeasured") and not f["anchor"] for f, _ in mine):
        return "passing", f"anchor gone; {rule} clean on the record"
    if not real:
        return "stale", f"anchor gone; {rule} unmeasured on the record (typed, no anchor)"
    return "stale", f"anchor gone; {rule} still flags the record ({len(real)})"


def state(args):
    hd = args.head.rstrip("/")
    head = os.path.basename(hd).replace("head-", "")
    rows = _tsv(args.rows)
    recs, flags, rules, inkmarks = load_head(hd)
    cache, out = {}, []
    rows.sort(key=lambda r: (r["module"], r["declaration"]))
    for r in rows:
        st, det = decide(r, recs, flags, rules, cache)
        if r["source"].startswith("ink@") and st != "gapped":
            cls = r["note"].replace("class ", "")
            marks = inkmarks.get((r["module"], r["declaration"]), set())
            if (cls, r["anchor"]) in marks:
                det += "; this head's ink still marks it"
            elif any(c == cls for c, _ in marks):
                det += f"; this head's ink marks {cls} elsewhere on the record"
        out.append(dict(r, state=st, detail=det))
    out.sort(key=lambda r: (r["source"], r["id"]))
    os.makedirs(args.out, exist_ok=True)
    path = os.path.join(args.out, f"suite-{head}.tsv")
    cols = ["id", "source", "rule", "module", "declaration", "state", "detail", "anchor", "key"]
    _write(path, cols, out)
    by = defaultdict(Counter)
    for r in out:
        by[r["rule"]][r["state"]] += 1
    states = ["failing", "gapped", "passing", "stale"]
    print(f"head {head}: {len(out)} rows -> {path}")
    print("rule\t" + "\t".join(states))
    for rule in sorted(by):
        print(rule + "\t" + "\t".join(str(by[rule][s]) for s in states))
    tot = Counter(r["state"] for r in out)
    print("all\t" + "\t".join(str(tot[s]) for s in states))
    return out


def main(argv=None):
    p = argparse.ArgumentParser()
    sp = p.add_subparsers(dest="cmd", required=True)
    b = sp.add_parser("build")
    b.add_argument("--endgame", default=ENDGAME)
    b.add_argument("--tests", default="/tmp/integ-snap303/test/mfuton/paper_walk_test.py")
    b.add_argument("--ink-heads", default=STORAGE)
    b.add_argument("--keys-head", default=os.path.join(STORAGE, "head-c8415d75"))
    b.add_argument("--out", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "suite-rows.tsv"))
    s = sp.add_parser("state")
    s.add_argument("head")
    s.add_argument("--rows", default=os.path.join(os.path.dirname(os.path.abspath(__file__)), "suite-rows.tsv"))
    s.add_argument("--out", default=os.path.dirname(os.path.abspath(__file__)))
    a = p.parse_args(argv)
    build(a) if a.cmd == "build" else state(a)


if __name__ == "__main__":
    main()
