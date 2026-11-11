'use strict';
const test = require('node:test');
const assert = require('node:assert');
const P = require('../patterns');

const A = `@flexiarg fam/a
@title Pattern A
@references [fam/b
             other/c fam/not-there]
@why fam/b
@keywords [fam/c]

! conclusion:
  Do the thing.
  + IF: something holds
    over two lines.
  + HOWEVER: a tension.
  + THEN: act.
  + BECAUSE: reasons.
`;
const known = new Set(['fam/a', 'fam/b', 'other/c', 'fam/c']);

test('header links: only known ids, kind = header, skipped headers ignored', () => {
  const p = P.pattern('fam/a', A, known);
  assert.deepStrictEqual(p.links, [
    { kind: 'references', to: 'fam/b' }, { kind: 'references', to: 'other/c' }, { kind: 'why', to: 'fam/b' }]);
  assert.strictEqual(p.title, 'Pattern A');
  assert.strictEqual(p.parts.IF, 'something holds over two lines.');
  assert.strictEqual(p.parts.BECAUSE, 'reasons.');
});

test('proposals: union of producers, top 3 each, ~ stripped', () => {
  const r = {
    proposed_patterns: [{ ranked: [{ id: 'x/1' }, { id: 'x/2' }] }, { patterns: ['~x/9'] }],
    embedding_ranked: [{ id: 'x/3' }],
    lu_ranked: { ranked: [{ id: 'x/1' }, { id: 'x/4' }] },
  };
  assert.deepStrictEqual(P.proposals(r), [
    { id: 'x/1', sources: ['embedding', '驴'] }, { id: 'x/2', sources: ['embedding'] },
    { id: 'x/9', sources: ['embedding'] }, { id: 'x/4', sources: ['驴'] }]);
});

test('components come from authored links only; co-proposal stays an edge', () => {
  const lib = new Map(['fam/a', 'fam/b', 'other/c', 'fam/c'].map((id) => [id, { id, links: [] }]));
  lib.get('fam/a').links = [{ kind: 'references', to: 'fam/b' }];
  const turns = [{ id: 't1', proposals: [{ id: 'fam/a', sources: ['驴'] }, { id: 'other/c', sources: ['驴'] }], verdicts: {} }];
  const g = P.build(lib, turns);
  assert.notStrictEqual(g.compOf.get('fam/a'), g.compOf.get('other/c'));
  assert.strictEqual(g.compOf.get('fam/a'), g.compOf.get('fam/b'));
  assert.deepStrictEqual(g.coOf.get('fam/a'), [{ with: 'other/c', turns: ['t1'] }]);
  assert.deepStrictEqual(g.edges, { authored: 1, observed: 1 });
});
