'use strict';
// The Gauntlet party page (gcc-view.js party) on the invented fixture
// test/gcc-preview-fixture/party.json (format: ~/notes/claude-13/party-contract.md).
const test = require('node:test');
const assert = require('node:assert');
const fs = require('fs');
const path = require('path');
const G = require('../gcc-view');

const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text, cls) => `<a class="ln${cls ? ' ' + cls : ''}" data-href="${esc(href)}">${esc(text)}</a>`;
const trim = (s, n) => (String(s).length > n ? String(s).slice(0, n - 1) + '…' : String(s));
const V = G({ esc, link, trim });
const src = G.partySource('/nonexistent');
const html = () => V.party(src).html;
const hrefs = (h) => [...h.matchAll(/data-href="([^"]*)"/g)].map((m) => m[1]);
const items = new Set(fs.readdirSync(path.join(G.FIXTURE, 'items')).map((f) => f.replace(/\.json$/, '')));
const P = JSON.parse(fs.readFileSync(path.join(G.FIXTURE, 'party.json'), 'utf8'));

test('four characters in order, ten lines each, with role and rule', () => {
  const cols = html().split('PARTY VOTE')[0].split('<div class="party-col">').slice(1);
  assert.strictEqual(cols.length, 4);
  assert.deepStrictEqual(cols.map((c) => c.match(/<div class="gs-h">(\w+)/)[1]), ['Thor', 'Merlin', 'Questor', 'Thyra']);
  for (const c of cols) {
    assert.strictEqual((c.match(/<div class="gs-r">\d+ /g) || []).length, 10);
    assert.match(c, /<div class="gs-r dim">FIXTURE: first the items/);
  }
  assert.match(cols[0], /<span class="dim">unblocking<\/span>/);
});

test('the party vote lists 12, each with who chose it', () => {
  const vote = html().split('PARTY VOTE')[1];
  const rows = [...vote.matchAll(/<div class="gs-r">(\d+) .*?<span class="chooser">← (\w+)<\/span>/g)];
  assert.strictEqual(rows.length, 12);
  assert.deepStrictEqual(rows.slice(0, 4).map((r) => r[2]), ['Thor', 'Merlin', 'Questor', 'Thyra']);
});

test('item ids link to gi:<id> only when the item has a page; nothing else is linked', () => {
  const h = html();
  const hs = hrefs(h).filter((x) => x !== 'gparty-raw');
  assert.ok(hs.length > 0);
  for (const x of hs) {
    assert.match(x, /^gi:/);
    assert.ok(items.has(x.slice(3)), 'link to an item with no page: ' + x);
  }
  // an item with a page is linked; one without is plain text; a record id is never linked
  assert.match(h, /data-href="gi:M-fixture-alpha"/);
  assert.match(h, /\d+ M-fixture-c — /);
  assert.ok(P.party.top.some((e) => e.item.includes('/')), 'the fixture must hold a record id for this test to mean anything');
  assert.ok(!hrefs(h).some((x) => x.includes('/')));
});

test('absences in words; raw JSON behind c', () => {
  assert.match(html(), /<div class="gs-r absent">No Thyra's harm scores: FIXTURE: no harm record/);
  assert.match(html(), /data-cycle="gparty-raw"/);
  assert.match(V.partyRaw(src).html, /data-cycle="gparty"/);
  assert.match(V.partyRaw(src).html, /&quot;chosen_by&quot;: &quot;thor&quot;/);
});
