'use strict';
// Joe 2026-11-18: "a flowchart that runs top to bottom, a bit like a git
// merge diagram, but showing the intents that are realised".
const test = require('node:test');
const assert = require('node:assert');
const G = require('../intent-graph-view');

const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text) => `<a class="ln" data-href="${esc(href)}">${esc(text)}</a>`;

// Newest first, as the strand route sends. Invented acts:
//   t1 opens p1 (proposal) and a1 (ask); t2 opens p2; t3 realises p1 by a
//   commit and withdraws a1; p2 stays open; t3 also closes x0, opened
//   before the window (no lane for it).
const data = { session: 'session-invented', turns: [
  { id: 'turn-t3', created_at: '2026-11-18T03:03:00Z',
    closed_acts: [{ act: 'p1', kind: 'propose' }, { act: 'a1', kind: 'ask-action' }, { act: 'x0', kind: 'propose' }],
    acts: [{ id: 'c1', kind: 'commit', author: 'agent-invented', target: 'p1', about: 'built it', 'port?': false },
           { id: 'w1', kind: 'withdraw', author: 'operator', target: 'a1', 'port?': false }] },
  { id: 'turn-t2', created_at: '2026-11-18T03:02:00Z', closed_acts: [],
    acts: [{ id: 'p2', kind: 'propose', author: 'operator', about: 'a second idea', 'port?': true },
           { id: 'q2', kind: 'clarify', author: 'operator', about: 'a question', 'port?': true }] },
  { id: 'turn-t1', created_at: '2026-11-18T03:01:00Z', closed_acts: [],
    acts: [{ id: 'p1', kind: 'propose', author: 'operator', about: 'cache the reads', 'port?': true },
           { id: 'a1', kind: 'ask-action', author: 'operator', about: 'fix the font', 'port?': true }] },
] };

test('rows run oldest first; intents branch at their turn and join where closed', () => {
  const L = G.layout(data);
  assert.deepStrictEqual(L.rows.map((r) => r.type + ':' + (r.turn ? r.turn.id : r.act ? r.act.id : r.id)),
    ['turn:turn-t1', 'open:p1', 'open:a1', 'turn:turn-t2', 'open:p2', 'turn:turn-t3', 'close:p1', 'close:a1']);
  assert.strictEqual(L.closeAt.get('p1').closer.id, 'c1', 'realised by the act that targets it');
  assert.strictEqual(L.closeAt.get('p1').ended, false);
  assert.strictEqual(L.closeAt.get('a1').ended, true, 'a withdrawal ends the intent');
  assert.deepStrictEqual(L.stillOpen, ['p2']);
  assert.ok(!L.closeAt.has('x0'), 'an intent opened before the window gets no lane');
  assert.strictEqual(L.realised, 1);
  assert.strictEqual(L.ended, 1);
  assert.strictEqual(L.questions, 1, 'a question opens a port but is counted, not drawn');
  assert.ok(!L.openAt.has('q2'));
});

test('a freed lane is reused, so width follows what is open at once', () => {
  const L = G.layout(data);
  assert.strictEqual(L.maxLanes, 3);   // p1, a1, p2 open together at t2
  const more = { ...data, turns: [{ id: 'turn-t4', created_at: '2026-11-18T03:04:00Z', closed_acts: [],
    acts: [{ id: 'p3', kind: 'propose', author: 'operator', about: 'third', 'port?': true }] }, ...data.turns] };
  const M = G.layout(more);
  assert.strictEqual(M.maxLanes, 3, 'p3 takes a lane freed at t3');
  assert.strictEqual(M.openAt.get('p3').lane, M.openAt.get('p1').lane);
});

test('render: labels say what each intent is and what realised it, at panel size', () => {
  const html = G({ esc, link }).render(data).html;
  const text = html.replace(/<title>.*?<\/title>/g, '').replace(/<[^>]+>/g, ' ').replace(/\s+/g, ' ');
  assert.match(text, /propose cache the reads \(you\)/);
  assert.match(text, /realised: commit by agent-invented · built it/);
  assert.match(text, /× withdraw by you/);
  assert.match(text, /1 realised · 1 withdrawn · 1 still open · 3 turns · 1 questions and reports not drawn/);
  assert.match(html, /class="lane open"/);
  assert.doesNotMatch(html, /viewBox/, 'a viewBox lets the SVG shrink its text');
  assert.match(html, /<svg class="intents"[^>]* style="font-size:14px/);
  // one text line per row, never two on one row
  const ys = [...html.matchAll(/<text[^>]* y="(\d+)"/g)].map((m) => m[1]);
  assert.strictEqual(new Set(ys).size, ys.length, 'two labels on one row');
});
