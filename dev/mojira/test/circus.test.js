'use strict';
// Giraffe's top items and "why not?" (circus-view.js) on responses captured
// from the live routes on 2026-11-17 (test/circus-fixture/).
const test = require('node:test');
const assert = require('node:assert');
const C = require('../circus-view');

const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text, cls) => `<a class="ln${cls ? ' ' + cls : ''}" data-href="${esc(href)}">${esc(text)}</a>`;
const trim = (s) => String(s);
const items = new Set(['M-giraffe']);   // pretend only M-giraffe has a GCC item page
const V = C({ esc, link, trim, hasItem: (x) => items.has(x) });
const F = (f) => require('./circus-fixture/' + f);
const hrefs = (h) => [...h.matchAll(/data-href="([^"]*)"/g)].map((m) => m[1]);

test('top items: one line per item in rank order, each with a why link', () => {
  const d = F('top-items.json');
  const h = V.top(d).html;
  const ranks = [...h.matchAll(/<div class="gs-r">(\d+) /g)].map((m) => Number(m[1]));
  assert.deepStrictEqual(ranks, d.items.map((i) => i.rank));
  for (const it of d.items) assert.ok(hrefs(h).includes('wn:' + it.item));
});

test('an item links to its GCC page only when it has one', () => {
  const h = V.top(F('top-items.json')).html;
  assert.ok(hrefs(h).includes('gi:M-giraffe'));
  assert.ok(!hrefs(h).some((x) => x.startsWith('gi:') && x !== 'gi:M-giraffe'));
  assert.ok(!hrefs(h).some((x) => x.includes('clusion')), 'no clusion id is a link');
});

test('focus with no 猴 link is marked as an open question; one with 猴 is not', () => {
  const h = V.top(F('top-items.json')).html;
  const line = (item) => h.split('\n').find((l) => l.includes('wn:' + item));
  assert.match(line('M-formal-patterns-redux'), /open question: in focus, no 猴 link/);
  assert.doesNotMatch(line('M-giraffe'), /open question/);
});

test('turn lines: six counts, and Lean-checked only when the check says ok', () => {
  const h = V.top(F('top-items-certified.json')).html;
  assert.match(h, /data-href="pt:turn-Ug5qdo">turn-Ug5qdo<\/a> <a class="ln" data-href="step:turn-Ug5qdo">象<\/a> .*驴3 馬3 狗3 象3 m1 猴3 <a class="ln cert"[^>]*>Lean-checked/);
  const latest = V.top(F('top-items.json')).html;
  assert.doesNotMatch(latest, /Lean-checked/, 'the newer run has no certificate yet');
  const unchecked = { items: [{ rank: 1, item: 'M-x', 'priority-band': 'focus', 'focus-relations': [
    { turn: { id: 't' }, 'gcc-counts': { '驴': 1, '猴': 0 }, certificate: { path: '/x.lean', check: { ok: false, exit: 1 } } }] }] };
  const u = V.top(unchecked).html;
  assert.doesNotMatch(u, /Lean-checked/);
  assert.match(u, /not checked, exit 1/);
  assert.match(u, /<span class="absent">猴0<\/span>/);
  assert.match(u, /<span class="absent">馬–<\/span>/);
});

test('why not: the ladder highlights where the item stopped, with reason and a source link', () => {
  const ex = V.whyNot(F('why-not-M-capability-levels.json')).html;
  assert.match(ex, /input → <b class="stop">excluded<\/b>/);
  assert.match(ex, /STOPPED AT EXCLUDED<\/div><div class="gs-r">operator gated/);
  assert.ok(hrefs(ex).includes('file:/home/joe/code/futon0/holes/M-capability-levels.md'));
  const ok = V.whyNot(F('why-not-M-giraffe.json')).html;
  assert.match(ok, /ranked → <b class="stop">focus<\/b>/, 'ranked with a focus relation reaches focus');
  assert.match(ok, /Lean-checked/);
  assert.match(V.whyNot({ status: 'error', reason: 'run-not-found', item: 'M-q' }).html, /run-not-found/);
});
