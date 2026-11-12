'use strict';
// The GCC item view (M-repl-circus demo 1) on the invented fixture in
// test/gcc-preview-fixture (format: ~/notes/claude-13/demo-01-contract.md).
const test = require('node:test');
const assert = require('node:assert');
const G = require('../gcc-view');

const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text, cls) => `<a class="ln${cls ? ' ' + cls : ''}" data-href="${esc(href)}">${esc(text)}</a>`;
const trim = (s, n) => (String(s).length > n ? String(s).slice(0, n - 1) + '…' : String(s));
const V = G({ esc, link, trim });
const src = { dir: G.FIXTURE, fixture: true };
const alpha = () => V.item(src, 'M-fixture-alpha').html;
// The <li> of one record, by its id.
const li = (html, id) => {
  const m = html.match(new RegExp(`<li class="gcc-rec[^"]*" data-status="\\w*"><span class="gcc-status">\\w+</span> <b>${id}</b>`));
  assert.ok(m, 'no record ' + id);
  return m[0];
};

test('records are grouped by type, in the order first seen', () => {
  const html = alpha();
  const heads = [...html.matchAll(/<h2>(\[[^\]]+\])/g)].map((m) => m[1]);
  assert.deepStrictEqual(heads, ['[:workplan/item 1]', '[:giraffe/reading 1]', '[:lexicon/names 1]',
    '[:serves 1]', '[:turn/mention 1]', '[:pending/level 1]']);
});

test('proposed records are drawn faint; built ones are not', () => {
  const html = alpha();
  assert.match(li(html, 'reading/alpha-3'), /class="gcc-rec proposed/);
  assert.match(li(html, 'mention/t1'), /class="gcc-rec proposed/);
  assert.doesNotMatch(li(html, 'names/alpha-arxana'), /proposed/);
  assert.doesNotMatch(li(html, 'item/M-fixture-alpha'), /proposed"/);
  // links too: the proposed needs edge is faint, the built ones are not
  const needs = [...html.matchAll(/<li class="(gcc-rec[^"]*)" data-status="(\w+)">[^<]*<span class="gcc-status">\w+<\/span> <span class="verb">\[:needs 1\]/g)];
  assert.strictEqual(needs.length, 3);
  for (const [, cls, status] of needs) assert.strictEqual(cls.includes('proposed'), status === 'proposed');
});

test('supersession is one chain, newest first, older ones marked superseded', () => {
  const [g] = G.groups(require('./gcc-preview-fixture/items/M-fixture-alpha.json').records)
    .filter((x) => x.type === '[:giraffe/reading 1]');
  assert.strictEqual(g.chains.length, 1);
  assert.deepStrictEqual(g.chains[0].map((r) => r.id), ['reading/alpha-3', 'reading/alpha-2', 'reading/alpha-1']);
  const html = alpha();
  assert.doesNotMatch(li(html, 'reading/alpha-3'), /superseded"/);
  assert.match(li(html, 'reading/alpha-2'), /superseded/);
  assert.ok(html.indexOf('<b>reading/alpha-3</b>') < html.indexOf('<b>reading/alpha-1</b>'));
});

test('needs links show met, not met and unknown', () => {
  const html = alpha();
  assert.match(html, /T-fixture-beta<\/a> <span class="met met-not-met">not met/);
  assert.match(html, /E-fixture-gamma<\/a> <span class="met met-met">met/);
  assert.match(html, /M-fixture-delta<\/a> <span class="met met-unknown">unknown/);
});

test('absences are rendered in words, including an absent state group', () => {
  const html = V.item(src, 'T-fixture-beta').html;
  assert.match(html, /state group: <span class="absent">absent: no state-group record: the ticket has no Status line/);
  assert.match(html, /<li class="absent">No Giraffe reading: Giraffe has not read this item<\/li>/);
  assert.match(html, /<li class="absent">No question link: no item-&gt;question record exists<\/li>/);
  assert.match(alpha(), /state group: started/);
});

test('the raw view cycles with the item view and shows the exact records', () => {
  const html = alpha();
  assert.match(html, /data-cycle="gi-raw:M-fixture-alpha"/);
  const raw = V.raw(src, 'M-fixture-alpha').html;
  assert.match(raw, /data-cycle="gi:M-fixture-alpha"/);
  assert.match(raw, /&quot;supersedes&quot;: &quot;reading\/alpha-2&quot;/);
});

test('the fixture is marked as invented; a missing preview falls back to it', () => {
  assert.deepStrictEqual(G.source('/nonexistent/gcc-preview'), src);
  assert.match(V.list(src).html, /FIXTURE: invented content/);
  assert.match(V.list(src).html, /data-href="gi:T-fixture-beta"/);
  assert.throws(() => V.item(src, '../index'), /bad item id/);
});
