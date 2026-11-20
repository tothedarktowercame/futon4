#!/usr/bin/env python3
"""Dry run of the endgame rules over classes.tsv and queue.tsv (model: endgame-process.json).

  endgame.py --freeze 2026-11-18 [--cap 3] [--min-size 2] [--enforce]
  endgame.py --selftest

A class found on or before the freeze date is frozen. Each frozen class ends in one of:
  left       state done or notdefect
  exception  rejected >= cap, or size <= min-size with no packet running: proposed to Joe as a Lean
             notation change or a residual (at the cap, run the critical-pair check first)
  fix        one more printer packet
  count      size not known: needs a census count before it can be sorted
Reported as violations (exit 1 with --enforce):
  a queue item naming a class that is not in classes.tsv;
  a non-discovery queue item dated after the freeze for a class that is not frozen;
  a packet running, pending or later for a class already at the cap;
  with counts.tsv (commit<TAB>class<TAB>open, in merge order): a class whose count rises.
  --from-commit SHA starts that comparison at SHA (the freeze commit): counts before it were taken
  on a corpus that was still growing (Q125 made 13 proofs finish), so a rise there is not a regression.
"""
import argparse, csv, os, sys

HERE = os.path.dirname(os.path.abspath(__file__))

def read(path):
    with open(path, newline='') as f:
        return list(csv.DictReader(f, delimiter='\t'))

def exit_of(c, cap, min_size):
    if c['state'] in ('done', 'notdefect'):
        return 'left'
    if int(c['rejected']) >= cap:
        return 'exception'
    if c['size'] == '?':
        return 'count'
    if int(c['size']) <= min_size and c['state'] != 'running':
        return 'exception'
    return 'fix'

def run(classes, queue, counts, freeze, cap, min_size, from_commit=None):
    by_id = {c['id']: c for c in classes}
    frozen = {c['id'] for c in classes if c['found'] <= freeze}
    exits = {i: exit_of(by_id[i], cap, min_size) for i in sorted(frozen)}
    violations, outside = [], []
    for q in queue:
        ids = q['class'].split('+')
        for i in ids:
            if i not in by_id:
                violations.append(f"{q['item']}: class {i} is not in the ledger")
        known = [i for i in ids if i in by_id]
        if q['date'] > freeze and q['outcome'] != 'discovery' and any(i not in frozen for i in known):
            outside.append(q['item'])
        if q['outcome'] in ('running', 'pending', 'later'):
            for i in known:
                if int(by_id[i]['rejected']) >= cap:
                    violations.append(f"{q['item']}: class {i} is at the cap ({by_id[i]['rejected']} rejected) and still queued")
    last = {}
    if from_commit:
        commits = [r['commit'] for r in counts]
        if from_commit not in commits:
            violations.append(f"counts.tsv has no rows for the freeze commit {from_commit}")
        counts = counts[commits.index(from_commit):] if from_commit in commits else []
    for r in counts:
        k = r['class']
        if k in last and int(r['open']) > last[k][1]:
            violations.append(f"{r['commit']}: class {k} count rose {last[k][1]} -> {r['open']} (was {last[k][0]})")
        last[k] = (r['commit'], int(r['open']))
    return frozen, exits, outside, violations

def selftest():
    cl = [dict(id='A', found='2026-01-01', size='10', unit='records', rejected='3', state='open', note=''),
          dict(id='B', found='2026-01-01', size='1', unit='records', rejected='0', state='open', note=''),
          dict(id='C', found='2026-01-05', size='9', unit='records', rejected='0', state='open', note=''),
          dict(id='D', found='2026-01-01', size='9', unit='records', rejected='0', state='open', note='')]
    qu = [dict(item='q1', date='2026-01-02', **{'class': 'A'}, outcome='running'),     # over the cap, still queued
          dict(item='q2', date='2026-01-06', **{'class': 'C'}, outcome='merged'),      # class found after the freeze
          dict(item='q3', date='2026-01-06', **{'class': 'Z'}, outcome='merged'),      # class not in the ledger
          dict(item='q4', date='2026-01-06', **{'class': 'C'}, outcome='discovery'),   # discovery is allowed
          dict(item='q5', date='2026-01-02', **{'class': 'D'}, outcome='merged')]      # ordinary
    co = [dict(commit='c1', **{'class': 'D'}, open='9'), dict(commit='c2', **{'class': 'D'}, open='11')]
    frozen, exits, outside, v = run(cl, qu, co, '2026-01-03', 3, 2)
    assert frozen == {'A', 'B', 'D'}, frozen
    assert exits == {'A': 'exception', 'B': 'exception', 'D': 'fix'}, exits
    assert outside == ['q2'], outside
    assert len(v) == 3 and 'q1' in v[0] and 'q3' in v[1] and 'c2' in v[2], v
    assert run(cl, qu[4:], co[:1], '2026-01-03', 3, 2)[3] == []
    assert run(cl, qu[4:], co, '2026-01-03', 3, 2, 'c2')[3] == []                      # rise before the freeze commit
    assert 'c9' in run(cl, qu[4:], co, '2026-01-03', 3, 2, 'c9')[3][0]                 # freeze commit never counted
    co3 = co + [dict(commit='c3', **{'class': 'D'}, open='12')]
    assert 'c3' in run(cl, qu[4:], co3, '2026-01-03', 3, 2, 'c2')[3][0]                # rise after it is reported
    print('selftest ok: over-cap packet, post-freeze class, unknown class and a rising count are each reported')

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--freeze'); ap.add_argument('--cap', type=int, default=3)
    ap.add_argument('--min-size', type=int, default=2)
    ap.add_argument('--from-commit')
    ap.add_argument('--enforce', action='store_true'); ap.add_argument('--selftest', action='store_true')
    a = ap.parse_args()
    if a.selftest:
        return selftest()
    classes, queue = read(os.path.join(HERE, 'classes.tsv')), read(os.path.join(HERE, 'queue.tsv'))
    cpath = os.path.join(HERE, 'counts.tsv')
    counts = read(cpath) if os.path.exists(cpath) else []
    frozen, exits, outside, v = run(classes, queue, counts, a.freeze, a.cap, a.min_size, a.from_commit)
    by_id = {c['id']: c for c in classes}
    after = [q for q in queue if q['date'] > a.freeze and q['outcome'] != 'discovery']
    print(f"freeze {a.freeze}, cap {a.cap}, min size {a.min_size}: {len(frozen)} of {len(classes)} classes frozen")
    for kind in ('left', 'fix', 'exception', 'count'):
        ids = [i for i, e in exits.items() if e == kind]
        print(f"  {kind:9} {len(ids):2}  {' '.join(ids)}")
    for i, e in exits.items():
        if e in ('exception', 'count', 'fix'):
            c = by_id[i]
            print(f"    {e:9} {i:26} size {c['size']} {c['unit']}, rejected {c['rejected']}, {c['state']}: {c['note']}")
    print(f"  packets after the freeze: {len(after)}; for classes outside the frozen list: {len(outside)}")
    print(f"  counts.tsv: {'%d rows' % len(counts) if counts else 'absent, so the per-merge count is not checked'}")
    for line in v:
        print('  VIOLATION ' + line)
    sys.exit(1 if (v and a.enforce) else 0)

if __name__ == '__main__':
    main()
