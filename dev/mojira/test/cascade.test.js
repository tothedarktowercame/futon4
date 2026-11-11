'use strict';
// On the recorded cascade (futon3c holes/labs/M-repl-circus/cascade-2026-11-11-turn,
// e95aaa086), not invented data.
const test = require('node:test');
const assert = require('node:assert');
const fs = require('fs');
const R = require('../cascade-reader');
const V = require('../cascade-view');

const DIR = '/home/joe/code/futon3c/holes/labs/M-repl-circus/cascade-2026-11-11-turn';
const have = fs.existsSync(DIR + '/occurrences.json');

test('recorded cascade: 2 complete, 1 glue, partials 3/4: 2 and 2/4: 3', { skip: !have && 'lab files absent' }, () => {
  const c = R.read(DIR);
  assert.strictEqual(c.occurrences.length, 26);
  assert.deepStrictEqual(c.complete.sort(), ['cascade-construction/read-what-exists-first',
    'forward-model/the-forward-model-is-a-pattern-cascade']);
  assert.deepStrictEqual(c.glue, [{ element: 'voxterm-diagram-tab-start-incomplete',
    ends: [['cascade-construction/read-what-exists-first', 'G'],
      ['forward-model/the-forward-model-is-a-pattern-cascade', 'I2']] }]);
  const n = (k) => c.partial.filter((p) => c.occurrences.find((o) => o.pattern === p).n === k).length;
  assert.strictEqual(n(3), 2);
  assert.strictEqual(n(2), 3);
  assert.strictEqual(c.notes.validator, 'validate-cascade stops at the executable layer (:population-executable-set)');
});

test('layout: the glue sits between its two owners; 7 elements for 8 parts', { skip: !have && 'lab files absent' }, () => {
  const c = R.read(DIR);
  const L = V.layout(c, false);
  assert.strictEqual(L.els.length, 7);
  assert.strictEqual(L.edges.length, 8);
  const x = (id) => L.nodes.find((n) => n.id === id).x;
  const g = L.els.find((e) => e.glue);
  assert.strictEqual(g.id, 'voxterm-diagram-tab-start-incomplete');
  const [a, b] = [x('cascade-construction/read-what-exists-first'), x('forward-model/the-forward-model-is-a-pattern-cascade')].sort((p, q) => p - q);
  assert.ok(a < g.x && g.x < b, `glue x ${g.x} not between ${a} and ${b}`);
  const P = V.layout(c, true);
  assert.strictEqual(P.parts.length, 5);
});

test('a void map is kept with its reason, not drawn', () => {
  const o = R.occurrence({ pattern: 'f/p', maps: { I1: { element: 'e', source: 'q' }, I2: { element: null, reason: 'no match' } } });
  assert.strictEqual(o.n, 1);
  assert.deepStrictEqual(o.maps.I2, { void: 'no match' });
  assert.deepStrictEqual(o.maps.G, { void: 'no map' });
});

test('minimal view: 2 occurrence nodes, 1 glue edge (BECAUSE, HOWEVER, voxterm...), nothing else', { skip: !have && 'lab files absent' }, () => {
  const c = R.read(DIR);
  const L = V.minimalLayout(c);
  assert.deepStrictEqual(L.nodes.map((n) => n.id), ['cascade-construction/read-what-exists-first',
    'forward-model/the-forward-model-is-a-pattern-cascade']);
  assert.deepStrictEqual(L.edges.map((e) => [e.fromPart, e.toPart, e.element]),
    [['BECAUSE', 'HOWEVER', 'voxterm-diagram-tab-start-incomplete']]);
  assert.deepStrictEqual(Object.keys(L).sort(), ['edges', 'height', 'nodes', 'width']);   // no element or partial nodes
  const html = V(c, { esc: (s) => String(s), link: (h, t) => t, trim: (s) => s, partsOf: () => ({}) }).page('minimal').html;
  const svgPart = html.slice(html.indexOf('<svg'), html.indexOf('</svg>'));
  assert.strictEqual((svgPart.match(/data-href="cn:/g) || []).length, 2);
  assert.strictEqual((svgPart.match(/data-href="ce:/g) || []).length, 1);
  assert.ok(!/class="el|occ partial/.test(svgPart));
});

test('minimal text is cascade_sexp.py minimal form (626fd8942)', { skip: !have && 'lab files absent' }, () => {
  assert.strictEqual(V.minimalSexp(R.read(DIR)), [
    '(cascade "emacs-d1ceee43f003a912322bce0c0f951169" "2026-11-11T16:15:24.046120661Z"',
    '  (cascade-construction/read-what-exists-first BECAUSE #1=voxterm-diagram-tab-start-incomplete)',
    '  (forward-model/the-forward-model-is-a-pattern-cascade HOWEVER #1#))'].join('\n'));
});

test('the cycle key visits minimal, detailed, partials, and returns', () => {
  const seen = [];
  let h = 'cascade';
  for (let i = 0; i < 3; i++) { seen.push(h); h = V.nextView(h); }
  assert.deepStrictEqual(seen, ['cascade', 'cascade:detail', 'cascade:partials']);
  assert.strictEqual(h, 'cascade');
});
