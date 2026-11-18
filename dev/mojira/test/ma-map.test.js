'use strict';
const test = require('node:test');
const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const M = require('../ma-map-view');

const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text) => `<a class="ln" data-href="${esc(href)}">${esc(text)}</a>`;

const fixture = {
  session: 'session-invented',
  turns: [
    { id: 'turn-003', created_at: '2026-11-18T03:03:00Z',
      ma_connection: { status: 'linked', 'turn/id': 'turn-002' },
      ma: { status: 'read', window: ['turn-001', 'turn-003'], threads: [
        { pattern: 'pattern-beta', status: 'occurrence', parts: {
          G: { window: 'chain', from: 'turn-001' }, B: { window: 'turn', from: 'turn-003' } } }] },
      open_acts: [{ act: 'act-b', kind: 'report' }, { act: 'act-c', kind: 'ask-action' }] },
    { id: 'turn-002', created_at: '2026-11-18T03:02:00Z',
      ma_connection: { status: 'linked', 'turn/id': 'turn-001' },
      ma: { status: 'read', threads: [
        { pattern: 'pattern-alpha', status: 'occurrence', parts: {
          I1: { window: 'turn', from: 'turn-002' }, I2: { window: 'chain', from: 'turn-001' } } },
        { pattern: 'pattern-beta', status: 'partial', parts: {
          I1: { window: 'turn', from: 'turn-002' } } }] },
      open_acts: [{ act: 'act-a', kind: 'clarify' }, { act: 'act-b', kind: 'report' }] },
    { id: 'turn-001', created_at: '2026-11-18T03:01:00Z',
      ma_connection: { status: 'session-start' },
      ma: { status: 'read', threads: [
        { pattern: 'pattern-alpha', status: 'partial', parts: {
          I1: { window: 'turn', from: 'turn-001' } } }] },
      open_acts: [{ act: 'act-a', kind: 'clarify' }] },
  ],
  edges: [
    { from: 'turn-003', to: 'turn-001', kind: 'quotes', pattern: 'pattern-beta', part: 'G' },
  ],
};

test('layout orders lanes by first appearance and places dropped marks at the missing turn', () => {
  const l = M.layout(fixture);
  assert.deepStrictEqual(l.lanes, ['pattern-alpha', 'pattern-beta']);
  assert.deepStrictEqual(l.dropped.map((d) => [d.pattern, d.turn]), [['pattern-alpha', 'turn-003']]);
  assert.strictEqual(l.quotes[0].tx, l.columns[0].x, 'quote reaches the quoted turn column');
});

test('alignment differences report opened, closed, and carried backlog', () => {
  const l = M.layout(fixture);
  assert.deepStrictEqual(l.acts[1].opened.map((a) => a.act), ['act-b']);
  assert.strictEqual(l.acts[2].closed, 1);
  assert.deepStrictEqual(l.acts[2].opened.map((a) => a.act), ['act-c']);
  assert.strictEqual(l.acts[2].backlog, 2);
});

test('render draws linked nodes, filled occurrences, hollow partials, quotes, and statuses', () => {
  const html = M({ esc, link, now: () => Date.parse('2026-11-18T03:04:00Z') }).render(fixture).html;
  assert.match(html, /<a class="ln ma-node"[^>]*data-href="pat:pattern-alpha"/);
  assert.match(html, /thread occurrence/);
  assert.match(html, /thread partial/);
  assert.match(html, /class="quote"/);
  assert.match(html, /pattern-beta G quotes turn-001/);
  assert.match(html, /<title>pattern-alpha dropped at turn-003<\/title><path class="dropped" data-turn="turn-003"/);
  assert.doesNotMatch(html, /pattern-alpha dropped at turn-002/);
  assert.match(html.replace(/<[^>]+>/g, ' '), /馬 linked ←\s+turn-002/);   // may wrap onto two lines
  assert.match(html, /data-href="pt:turn-003"/);
  assert.match(html, /data-href="step:turn-003"/);
});

test('map shares the strand monitor cache and its only poller', () => {
  const core = fs.readFileSync(path.join(__dirname, '..', 'core.js'), 'utf8');
  assert.strictEqual((core.match(/setInterval\(/g) || []).length, 1);
  assert.match(core, /\['strand', 'mamap'\]\.includes/);
  assert.match(core, /cached\.data = strandView\.merge/);
  assert.match(core, /maMapView\(\{ esc, link \}\)/);
});

// Joe 2026-11-18: "mojira://mamap/ definitely fails the readability of font
// size invariant". The SVG keeps its own size (no viewBox, so the browser
// never shrinks it), declares panel-size text, and wraps labels to fit
// their column rather than shrinking them.
test('馬 map text is drawn at panel size and fits its column', () => {
  const many = { ...fixture, turns: Array.from({ length: 20 }, (_, i) => ({ ...fixture.turns[i % 3], id: 'turn-' + String(100 + i) })) };
  const html = M({ esc, link, now: () => Date.parse('2026-11-18T03:04:00Z') }).render(many).html;
  assert.doesNotMatch(html, /viewBox/, 'a viewBox lets the SVG shrink its text');
  assert.doesNotMatch(html, /max-width:100%/);
  assert.match(html, /<svg class="cascade ma-map"[^>]* style="font-size:14px/);
  // lane labels end before the first column, even for long single words
  const long = { ...fixture, turns: fixture.turns.map((t) => ({ ...t, ma: { ...t.ma, threads: (t.ma.threads || []).map((th) => ({ ...th, pattern: 'storage/incomprehensibilities-mirroring' })) } })) };
  const L2 = M.layout(long);
  const widest = Math.max(...[...L2.laneLines.values()].flat().map((l) => l.length));
  assert.ok(4 + widest * 13 * 0.62 + 10 <= L2.columns[0].x, `lane label of ${widest} chars reaches the first column at ${L2.columns[0].x}`);
  // every line of text fits its column: at 14px a character is about 8.7px wide
  const step = M.layout(many).columns[1].x - M.layout(many).columns[0].x;
  for (const m of html.matchAll(/<tspan[^>]*>([^<]*)<\/tspan>|<text[^>]*>([^<]+)<\/text>/g)) {
    const t = (m[1] || m[2] || '').replace(/&[a-z]+;/g, 'x');
    assert.ok(t.length * 14 * 0.62 <= step, `"${t}" is wider than a column (${step}px)`);
  }
});
