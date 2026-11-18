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

test('the picture: a link per kind, hollow kinds no link reaches, broken links in red', () => {
  const m = M.crawl(render);
  const html = M.view(m, { esc, link }).html;
  const hrefs = M.hrefsIn(html);
  for (const k of ['home', 'status', 'e', 'gcc', 'gi', 'top', 'file', 'ext', 'broken']) assert.ok(hrefs.includes('map:' + k), 'no node for ' + k);
  // reached by nothing in SITE: drawn hollow
  assert.match(html, /<title>letter of intent: Mojira can draw it, no link reaches it<\/title><circle class="occ partial"/);
  assert.match(html, /<title>searches: Mojira can draw it, no link reaches it \(key \/\)<\/title>/);
  assert.match(html, /<circle class="occ bad"/);
  assert.doesNotMatch(html, /<title>lexicon entities: [^<]*no link reaches/);
  const page = M.kindPage(m, 'e', { esc, link }).html;
  assert.ok(M.hrefsIn(page).includes('e:a') && M.hrefsIn(page).includes('e:b'));
  assert.match(page, /linked from: .*entities by status/);
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
