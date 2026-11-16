'use strict';
// The GCC item view (M-repl-circus demo 1, contract v2) on the invented
// fixture in test/gcc-preview-fixture (format: ~/notes/claude-13/demo-01-contract.md).
// M-fixture-alpha has a "display" block; T-fixture-beta has none.
const test = require('node:test');
const assert = require('node:assert');
const fs = require('fs');
const path = require('path');
const G = require('../gcc-view');

const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text, cls) => `<a class="ln${cls ? ' ' + cls : ''}" data-href="${esc(href)}">${esc(text)}</a>`;
const trim = (s, n) => (String(s).length > n ? String(s).slice(0, n - 1) + '…' : String(s));
const V = G({ esc, link, trim });
const src = { dir: G.FIXTURE, fixture: true };
const alpha = () => V.item(src, 'M-fixture-alpha').html;
const hrefs = (html) => [...html.matchAll(/data-href="([^"]*)"/g)].map((m) => m[1]);
const items = new Set(fs.readdirSync(path.join(G.FIXTURE, 'items')).map((f) => f.replace(/\.json$/, '')));
const file = JSON.parse(fs.readFileSync(path.join(G.FIXTURE, 'items', 'M-fixture-alpha.json'), 'utf8'));

test('the sections come in the mock-up order', () => {
  const html = alpha();
  const at = ['WHAT GIRAFFE READ', 'WAITING ON', 'RELATED', 'WHY IT MATTERS', 'NAMES', 'MENTIONED']
    .map((s) => html.indexOf(`<div class="gs-h">${s}`));
  assert.ok(at.every((i) => i > 0), 'a section is missing: ' + at);
  assert.deepStrictEqual(at, [...at].sort((a, b) => a - b));
  assert.ok(html.indexOf('[started · MAP]') < at[0], 'the header comes first');
});

test('a waiting_on id links to gi:<id> only when that item has a page', () => {
  const html = alpha();
  assert.match(html, /met-not-met">✗<\/span> <a class="ln" data-href="gi:T-fixture-beta">T-fixture-beta<\/a>/);
  // E-fixture-gamma and M-fixture-delta have no item file: plain text
  assert.match(html, /met-met">✓<\/span> E-fixture-gamma /);
  assert.match(html, /met-unknown">\?<\/span> M-fixture-delta /);
  assert.ok(!hrefs(html).includes('gi:E-fixture-gamma'));
  assert.ok(!hrefs(html).includes('gi:M-fixture-delta'));
});

test('every link on the item page goes to an item that exists; no record id is a link target', () => {
  const html = alpha();
  const recordIds = new Set(file.records.map((r) => r.id).concat((file.display.mentions.list || []).map((m) => m.turn)));
  for (const h of hrefs(html)) {
    assert.match(h, /^gi:/, 'not an item link: ' + h);
    assert.ok(items.has(h.slice(3)), 'link to an item with no page: ' + h);
    assert.ok(!recordIds.has(h.slice(3)), 'link to a record id: ' + h);
  }
  assert.ok(hrefs(html).length >= 1);
});

test('no 64-hex string appears outside the raw view', () => {
  const hex = /[0-9a-f]{64}/;
  assert.match(JSON.stringify(file.records), hex, 'the fixture must hold a 64-hex digest for this test to mean anything');
  assert.doesNotMatch(alpha(), hex);
  assert.doesNotMatch(V.list(src).html, hex);
  assert.match(V.raw(src, 'M-fixture-alpha').html, hex);
});

test('earlier readings and mentions are folded behind e and m; warnings and absences in words', () => {
  const html = alpha();
  assert.match(html, /2 earlier readings .*\[e show\]/);
  assert.match(html, /<div class="fold" data-fold="e">.*2026-11-08 <span class="dim">of bbbb222<\/span>/);
  assert.match(html, /3 times — first 2026-11-05 .*latest 2026-11-12 .*\[m list\]/);
  assert.match(html, /<div class="fold" data-fold="m">/);
  assert.match(html, /! self-check: FIXTURE/);
  assert.match(html, /benefit line G9: <span class="absent">G9's text is not in the data/);
  assert.match(html, /question it serves: <span class="absent">none recorded/);
  assert.match(html, /M-fixture-delta +<span class="absent">no item named M-fixture-delta/);
});

test('an item with no display block falls back to a one-line note; c reaches the raw records', () => {
  const html = V.item(src, 'T-fixture-beta').html;
  assert.match(html, /has no "display" block yet/);
  assert.doesNotMatch(html, /WHAT GIRAFFE READ/);
  assert.match(html, /data-cycle="gi-raw:T-fixture-beta"/);
  assert.match(V.raw(src, 'T-fixture-beta').html, /data-cycle="gi:T-fixture-beta"/);
});

test('the fixture is marked as invented; a missing preview falls back to it; record-like ids are refused', () => {
  assert.deepStrictEqual(G.source('/nonexistent/gcc-preview'), src);
  assert.match(V.list(src).html, /FIXTURE: invented content/);
  assert.throws(() => V.item(src, 'preview/giraffe-reading-1/19e68b98e376b681'), /not an item id/);
  assert.ok(!G.hasItem(src.dir, '../index'));
});

test('conformance is fresh, visible on every page and escaped', () => {
  const dir = fs.mkdtempSync(path.join(require('os').tmpdir(), 'mojira-conformance-'));
  try {
    fs.cpSync(G.FIXTURE, dir, { recursive: true });
    const s = { dir, fixture: false };
    const renders = [() => V.list(s), () => V.item(s, 'M-fixture-alpha'),
      () => V.item(s, 'T-fixture-beta'), () => V.party(s),
      () => V.raw(s, 'M-fixture-alpha'), () => V.partyRaw(s)];
    const file = path.join(dir, 'conformance.json');
    for (const render of renders) assert.ok(!render().html.includes('Conformance:'));
    fs.writeFileSync(file, JSON.stringify({ failing: ['<bad>'], run: '<toy>', checked_at_ms: 0 }));
    for (const render of renders) {
      const html = render().html;
      assert.ok(html.includes('<p class="err">Conformance: FAIL — &lt;bad&gt; (run &lt;toy&gt;, checked 1970-01-01T00:00:00.000Z)</p>'));
      assert.equal((html.match(/Conformance:/g) || []).length, 1);
    }
    fs.writeFileSync(file, JSON.stringify({ failing: [] }));
    for (const render of renders) assert.ok(!render().html.includes('Conformance:'));
    fs.writeFileSync(file, '{');
    for (const render of renders) assert.ok(render().html.includes('Conformance: result unreadable'));
  } finally { fs.rmSync(dir, { recursive: true, force: true }); }
});
