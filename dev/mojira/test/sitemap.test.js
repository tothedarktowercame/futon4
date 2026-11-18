'use strict';
// Mojira's map of itself (sitemap.js): the crawl, the picture, and the check
// that every kind of page core.js can draw is on the map.
const test = require('node:test');
const assert = require('node:assert');
const fs = require('fs');
const path = require('path');
const M = require('../sitemap');
const G = require('../gcc-view');

const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text, cls) => `<a class="ln${cls ? ' ' + cls : ''}" data-href="${esc(href)}">${esc(text)}</a>`;
const trim = (s, n) => (String(s).length > n ? String(s).slice(0, n - 1) + '…' : String(s));

test('every kind of page render() draws is on the map, and no other', () => {
  const src = fs.readFileSync(path.join(__dirname, '..', 'core.js'), 'utf8');
  const body = src.slice(src.indexOf('function render(href)'), src.indexOf('\n}\n', src.indexOf('function render(href)')));
  const drawn = new Set([...body.matchAll(/kind === '([^']+)'/g)].map((m) => m[1]));
  for (const set of body.matchAll(/([A-Z_]+_KINDS)\.has\(kind\)/g)) {
    const decl = src.match(new RegExp('const ' + set[1] + ' = new Set\\(\\[([^\\]]*)\\]'));
    for (const m of decl[1].matchAll(/'([^']+)'/g)) drawn.add(m[1]);
  }
  drawn.add('home');                       // render()'s fallback
  drawn.add('pmiss');                      // pagePattern's fallback, linked as pmiss
  assert.ok(drawn.size > 25, 'the parse found too few kinds: ' + [...drawn]);
  assert.deepStrictEqual([...drawn].sort(), Object.keys(M.KINDS).sort());
});

// A made-up site: home links to a status page and the GCC list; one page
// is not loaded; one link is to a kind that does not exist; files go outside.
const SITE = {
  home: link('status:open', 'open') + link('gcc', 'items') + link('file:/x/y.md', 'y') + link('nope:1', 'bad'),
  'status:open': link('e:a', 'a') + link('e:b', 'b'),
  'e:a': link('e:b', 'b') + link('top', 'top'),
  'e:b': '<div data-cycle="e:a"></div>',
  gcc: link('gi:M-1', 'M-1') + link('ext:http://h/?a=1&b=2', 'web'),
  'gi:M-1': '',
};
const render = (h) => ({ html: SITE[h] });

test('the crawl follows links and c views, counts kinds, depths, outside things and broken links', () => {
  const m = M.crawl(render, { loaded: (h) => h !== 'top' });
  assert.deepStrictEqual([...m.kinds.get('e').pages].sort(), ['e:a', 'e:b']);
  assert.strictEqual(m.kinds.get('e').depth, 2);
  assert.strictEqual(m.kinds.get('gi').depth, 2);
  assert.strictEqual(m.kinds.get('top').notLoaded, 1);
  assert.strictEqual(m.kinds.get('top').rendered, 0);
  assert.deepStrictEqual([...m.external.get('file')], ['/x/y.md']);
  assert.deepStrictEqual([...m.external.get('ext')], ['http://h/?a=1&b=2'], 'hrefs are unescaped');
  assert.deepStrictEqual(m.broken.map((b) => b.href), ['nope:1']);
  assert.strictEqual(m.edges.get('e\te'), 2, 'a link and a c view');
  assert.strictEqual(m.truncated, false);
});

test('the crawl stops at its per-kind cap and says so', () => {
  const m = M.crawl(render, { maxPerKind: 1 });
  assert.strictEqual(m.kinds.get('e').rendered, 1);
  assert.ok(m.kinds.get('e').unexplored >= 1);
  assert.strictEqual(m.truncated, true);
});

test('the picture: the whole map on one page, never more than five lines out of a dot, text not shrunk', () => {
  // Joe 2026-11-18: "the site map on one page please"; "no more than five
  // nodes from any choice point"; "all text must be easy to read".
  const m = M.crawl(r2);
  const tree = M.capTree(M.folders(m));
  let count = 0, nested = 0;
  const kinds = [];
  const walk = (n, d) => { count++; if (d >= 2 && (n.children || []).length) nested++; if (n.href) kinds.push(n.href); (n.children || []).forEach((c) => walk(c, d + 1)); };
  walk(tree, 0);
  assert.ok(nested >= 1, 'the fixture should have folders below the first level');
  const html = M.picture(m, '', { esc, link }).html;
  // every node of the tree is drawn, on this one page
  assert.strictEqual((html.match(/<circle|<rect/g) || []).length, count);
  const hrefs = M.hrefsIn(html);
  for (const h of kinds) assert.ok(hrefs.includes(h), 'missing ' + h);
  assert.ok(!hrefs.some((h) => /^mapg:/.test(h)), 'no drilling into fragments');
  // Joe's five, as a literal: lines out of any one dot
  const out = {};
  for (const x of html.matchAll(/data-from="(\d+)"/g)) out[x[1]] = (out[x[1]] || 0) + 1;
  for (const [id, k] of Object.entries(out)) assert.ok(k <= 5, `dot ${id} has ${k} lines out`);
  // readable: no viewBox, panel-size text, and no two labels on the same row and column
  assert.doesNotMatch(html, /viewBox/, 'a viewBox lets the SVG shrink its text');
  assert.match(html, /<svg class="cascade mapg"[^>]* style="font-size:14px"/);
  const spots = [...html.matchAll(/<text x="([\d.]+)" y="([\d.]+)"/g)].map((x) => x[1] + ',' + x[2]);
  assert.strictEqual(new Set(spots).size, spots.length, 'two labels drawn on top of each other');
  // within one column, labels are at least a line of text apart
  const cols = {};
  for (const x of html.matchAll(/<text x="([\d.]+)" y="([\d.]+)"/g)) (cols[x[1]] = cols[x[1]] || []).push(+x[2]);
  for (const ys of Object.values(cols)) {
    ys.sort((a, b) => a - b);
    for (let i = 1; i < ys.length; i++) assert.ok(ys[i] - ys[i - 1] >= 14, `rows ${ys[i - 1]} and ${ys[i]} are closer than a line of text`);
  }
  assert.match(html, /data-cycle="map"/);
});

test('on the GCC fixture: every item the list links to is on the map', () => {
  const V = G({ esc, link, trim });
  const src = { dir: G.FIXTURE, fixture: true };
  const r = (h) => {
    const [k, ...a] = h.split(':');
    if (k === 'gcc') return V.list(src);
    if (k === 'gi') return V.item(src, a.join(':'));
    if (k === 'gi-raw') return V.raw(src, a.join(':'));
    if (k === 'gparty') return V.party(src);
    return { html: '' };
  };
  const m = M.crawl(r, { start: 'gcc' });
  const listed = M.hrefsIn(V.list(src).html).filter((h) => h.startsWith('gi:'));
  assert.ok(listed.length >= 2);
  for (const h of listed) assert.ok(m.kinds.get('gi').pages.has(h), h);
  assert.ok(m.kinds.get('gi-raw').pages.size >= 2, 'the c views of the items');
  assert.deepStrictEqual(m.broken, []);
});

// Folders (Joe 2026-11-18: "all text must be easy to read"; "no more than
// five nodes from any choice point").
const walk = (n, f) => { f(n); (n.children || []).forEach((c) => walk(c, f)); };
const SECTIONED = {
  home: ['A', 'B', 'C', 'D', 'E', 'F', 'G'].map((h, i) => `<h2>${h} <span class="dim">(x)</span></h2>` + link(['status:s', 'gcc', 'top', 'loi', 'triage', 'all', 'pg'][i], h)).join('')
    + link('label:l', 'l'),
  'status:s': ['e:1', 'e:2'].map((h) => link(h, h)).join(''),
  gcc: '<h2>Items</h2>' + link('gi:a', 'a') + '<h2>Raw</h2>' + link('gi-raw:a', 'a') + link('gparty', 'p'),
  'gi:a': link('e:3', 'a later way to entities'),
};
const r2 = (h) => ({ html: SECTIONED[h] || '' });

test('folders: every reached kind once, no choice point over five, plain text', () => {
  const m = M.crawl(r2);
  const tree = M.capTree(M.folders(m));
  const seen = [];
  walk(tree, (n) => {
    assert.ok((n.children || []).length <= 5, `${n.label} offers ${n.children.length}`);   // Joe's five, not the module's constant
    if (n.href) seen.push(n.href);
  });
  const reached = [...m.kinds.values()].filter((k) => k.depth < Infinity).map((k) => 'map:' + k.kind);
  assert.deepStrictEqual(seen.sort(), reached.sort());
  const html = M.folderView(m, { esc, link }).html;
  assert.doesNotMatch(html, /<svg/);
  assert.match(html, /<span class="gs-tag">more\/<\/span>/, 'seven sections and a loose link need a "more" folder');
  assert.match(html, /data-cycle="mapg"/, 'c goes to the picture');
});

test('folders: a page\'s headings become folders, the dim aside is dropped, a one-kind heading is not a folder', () => {
  const m = M.crawl(r2);
  const tree = M.folders(m);
  const gcc = [];
  walk(tree, (n) => { if (n.href === 'map:gcc') gcc.push(n); });
  const labels = gcc[0].children.map((c) => c.label);
  // Items holds one kind, so it shows the kind itself; Raw holds two, so it is a folder
  assert.deepStrictEqual(labels.sort(), ['GCC items', 'Raw']);
  assert.deepStrictEqual(gcc[0].children.find((c) => c.label === 'Raw').children.map((c) => c.label).sort(), ['GCC items raw', 'Gauntlet party']);
  const eUnder = [];
  walk(tree, (n) => (n.children || []).forEach((c) => { if (c.href === 'map:e') eUnder.push(n.label); }));
  assert.deepStrictEqual(eUnder, ['entities by status'], 'a kind sits where the crawl first found it');
  const top = tree.children.map((c) => c.label);
  assert.ok(!top.some((l) => /\(x\)/.test(l)), 'the dim aside leaked into a folder name: ' + top);
});

// CL-80: invented directed kind graphs, independent of the live services.
function graph(kinds, edges) {
  return { kinds: new Map(kinds.map((kind) => [kind, { kind, depth: 0 }])),
    edges: new Map(edges.map(([a, b]) => [a + '\t' + b, 1])) };
}

test('hop distances: one-way chain has six unreachable distinct ordered pairs', () => {
  const m = graph(['home', 'a', 'b', 'c'], [['home', 'a'], ['a', 'b'], ['b', 'c']]);
  const d = M.distances(m, { via: 'direct' });
  assert.strictEqual(d.diameter, 3);
  assert.strictEqual(d.unreachable, 6);
  assert.strictEqual(d.over5, 0);
  assert.strictEqual(d.hops.get('home').get('c'), 3);
  assert.strictEqual(d.hops.get('c').get('home'), null);
  assert.strictEqual(d.hops.get('c').get('c'), 0);
  assert.strictEqual(d.worst.length, 6);
  assert.ok(d.worst.every((p) => p.distance === 'none'));
});

test('hop distances: home hub connects leaves, direct forbids returning through it', () => {
  const m = graph(['home', 'a', 'b', 'c'], ['a', 'b', 'c'].flatMap((k) => [['home', k], [k, 'home']]));
  const home = M.distances(m, { via: 'home' }), direct = M.distances(m, { via: 'direct' });
  assert.strictEqual(home.diameter, 2);
  assert.strictEqual(home.unreachable, 0);
  assert.strictEqual(home.hops.get('a').get('b'), 2);
  assert.strictEqual(direct.unreachable, 9);
  assert.strictEqual(direct.hops.get('a').get('b'), null);
  assert.strictEqual(direct.hops.get('home').get('b'), 1);
  assert.strictEqual(m.edges.size, 6, 'pure: input edges unchanged');
});

test('hop distances: seven-kind cycle has exactly seven ordered pairs over five', () => {
  const kinds = ['a', 'b', 'c', 'd', 'e', 'f', 'g'];
  const m = graph(kinds, kinds.map((k, i) => [k, kinds[(i + 1) % kinds.length]]));
  for (const via of ['home', 'direct']) {
    const d = M.distances(m, { via });
    assert.strictEqual(d.diameter, 6);
    assert.strictEqual(d.over5, 7);
    assert.strictEqual(d.unreachable, 0);
    assert.strictEqual(d.worst.length, 7);
    assert.ok(d.worst.every((p) => p.distance === 6));
  }
});

test('hop distances: unreached drawable kinds and external/broken edges are not pairs', () => {
  const m = graph(['home', 'gcc'], [['home', 'gcc'], ['gcc', 'file'], ['gcc', 'nope'], ['gcc', 'pt']]);
  m.kinds.set('pt', { kind: 'pt', depth: Infinity });
  const d = M.distances(m);
  assert.deepStrictEqual(d.kinds, ['gcc', 'home']);
  assert.strictEqual(d.unreachable, 1);
  assert.ok(d.unreached.includes('pt'));
  assert.ok(d.unreached.includes('herd'));
  assert.ok(!d.unreached.includes('gcc'));
  assert.ok(!d.hops.has('pt'));
  assert.deepStrictEqual(M.distances(graph([], [])).worst, []);
});

test('hop distances: worst list puts unreachable first and caps at twenty', () => {
  const kinds = Array.from({ length: 8 }, (_, i) => 'k' + i);
  const d = M.distances(graph(kinds, kinds.slice(1).map((k, i) => [kinds[i], k])));
  assert.strictEqual(d.over5, 3);
  assert.strictEqual(d.unreachable, 28);
  assert.strictEqual(d.worst.length, 20);
  assert.ok(d.worst.every((p) => p.distance === 'none'));
});

test('offline fixture crawl: both map views show summaries and linked pairs without rerendering', () => {
  let calls = 0;
  const m = M.crawl((h) => { calls++; return render(h); }, { loaded: (h) => h !== 'top' });
  const before = calls;
  for (const via of ['home', 'direct']) {
    const d = M.distances(m, { via });
    assert.deepStrictEqual([d.kinds.length, d.diameter, d.over5, d.unreachable], [6, 3, 0, 21]);
  }
  for (const html of [M.folderView(m, { esc, link }).html, M.picture(m, '', { esc, link }).html]) {
    assert.match(html, /Via home \(allowed\)/);
    assert.match(html, /Direct \(without returning home\)/);
    assert.strictEqual((html.match(/unreachable pairs: 21/g) || []).length, 2);
    assert.match(html, /data-href="map:gcc"/);
    assert.match(html, /Drawable but not reached \(excluded from pairs\)/);
  }
  assert.strictEqual(calls, before, 'distance display uses only the completed crawl');
});
