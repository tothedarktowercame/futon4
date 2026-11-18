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

test('the picture: one choice point at a time, never more than five dots below, text not shrunk', () => {
  const m = M.crawl(r2);
  const tree = M.capTree(M.folders(m));
  // every folder in the tree, drawn: at most 5 children, plus the node and its parent
  const paths = [];
  const visit = (n, p) => { paths.push(p); (n.children || []).forEach((c, i) => { if ((c.children || []).length) visit(c, p ? p + '/' + i : String(i)); }); };
  visit(tree, '');
  assert.ok(paths.length >= 3, 'the fixture should have nested folders: ' + paths);
  for (const p of paths) {
    const html = M.picture(m, p, { esc, link }).html;
    const dots = (html.match(/<circle|<rect/g) || []).length;
    assert.ok(dots <= 5 + 1, `${p || 'home'} draws ${dots} dots`);   // Joe's five, plus the chosen node
    assert.doesNotMatch(html, /viewBox/, 'a viewBox lets the SVG shrink its text');
    assert.match(html, /<svg class="cascade mapg"[^>]* style="font-size:14px"/);
    assert.match(html, /data-cycle="map"/);
    if (p) assert.ok(M.hrefsIn(html).includes('mapg' + (p.includes('/') ? ':' + p.split('/').slice(0, -1).join('/') : '')), 'no way up from ' + p);
  }
  // home: its children open a level down when they have children, else list their pages
  const home = M.hrefsIn(M.picture(m, '', { esc, link }).html);
  assert.ok(home.some((h) => /^mapg:\d+$/.test(h)) && home.some((h) => /^map:/.test(h)));
  // a path that is not there falls back to home rather than failing
  assert.match(M.picture(m, '9/9/9', { esc, link }).html, /, from home/);
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
