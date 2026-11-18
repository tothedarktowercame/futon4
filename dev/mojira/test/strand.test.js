'use strict';
const test = require('node:test');
const assert = require('node:assert');
const S = require('../strand-view');

const F = (f) => require('./strand-fixture/' + f);
const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text) => `<a data-href="${esc(href)}">${esc(text)}</a>`;

test('renders every 馬 connection status and reuses the supplied count formatter', () => {
  let calls = 0;
  const counts = (c) => { calls++; return `COUNTS:${c['驴'] || 0}/${c['馬'] || 0}`; };
  const V = S({ esc, link, counts, now: () => Date.parse('2026-11-18T03:02:00Z') });
  const h = V.render(F('initial.json')).html;
  assert.match(h, /馬 previous pending · 2m old/);
  assert.match(h, /馬 previous absent: no-reply/);
  assert.match(h, /馬 session start/);
  assert.match(h, /COUNTS:2\/0/);
  assert.strictEqual(calls, 3, 'the shared formatter is called once per turn');
});

test('linked connection is a short predecessor link and items use gi links', () => {
  const V = S({ esc, link, counts: () => '' });
  const h = V.render(F('delta.json')).html;
  assert.match(h, /馬 ← <a data-href="pt:turn-003">turn-003<\/a>/);
  assert.match(h, /data-href="gi:M-four"/);
  assert.match(h, /data-href="pt:turn-004"/);
  assert.match(h, /data-href="step:turn-004"/);
});

test('since merge prepends new turns and replaces only refreshed turns', () => {
  const initial = F('initial.json');
  const oldOne = initial.turns[2];
  const merged = S.merge(initial, F('delta.json'), F('refresh.json'));
  assert.deepStrictEqual(merged.turns.map((t) => t.id), ['turn-004', 'turn-003', 'turn-002', 'turn-001']);
  assert.strictEqual(merged.turns[3], oldOne, 'an old turn is retained, not rebuilt');
  assert.strictEqual(merged.turns[1].ma_connection.status, 'linked', 'newest prior turn is refreshed in place');
  assert.strictEqual(merged.turns[0].gcc['馬'].count, 3);
});

test('tally reports the accumulating connected component', () => {
  const merged = S.merge(F('initial.json'), F('delta.json'), F('refresh.json'));
  assert.deepStrictEqual(S.tally(merged), {
    turns: 4,
    edges: { strand: 3, mention: 2, focus: 1, proposed: 1, occurs: 1 },
    items: 3,
    sessionLinks: 3,
  });
});
