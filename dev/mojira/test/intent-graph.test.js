'use strict';
// Joe 2026-11-18: "a flowchart that runs top to bottom, a bit like a git
// merge diagram"; then: "the interest is not really in tracking the 'open'
// items ... but the items that actually are addressed and form part of the
// exchange: an ask item that is answered or an unresolved item that I then
// resolve are the typical cases".
const test = require('node:test');
const assert = require('node:assert');
const G = require('../intent-graph-view');

const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text) => `<a class="ln" data-href="${esc(href)}">${esc(text)}</a>`;

// Newest first, as the strand route sends. Invented acts:
//   t1: you ask a1, answered in the same turn by the agent's report r1;
//       you open u1 (unresolved) and p1 (a proposal nobody takes up);
//   t2: the agent asks q2;
//   t3: you resolve u1 (approve), answer q2, and withdraw x0, which was
//       opened before these turns.
const data = { session: 'session-invented', turns: [
  { id: 'turn-t3', created_at: '2026-11-18T03:03:00Z',
    closed_acts: [{ act: 'u1', kind: 'unresolved' }, { act: 'q2', kind: 'clarify' }, { act: 'x0', kind: 'propose' }],
    acts: [{ id: 'v3', kind: 'approve', author: 'operator', target: 'u1', about: 'settled it', 'port?': false },
           { id: 'v4', kind: 'report', author: 'operator', target: 'q2', 'port?': false },
           { id: 'w0', kind: 'withdraw', author: 'operator', target: 'x0', 'port?': false }] },
  { id: 'turn-t2', created_at: '2026-11-18T03:02:00Z', closed_acts: [],
    acts: [{ id: 'q2', kind: 'clarify', author: 'agent-invented', about: 'which file?', 'port?': true }] },
  { id: 'turn-t1', created_at: '2026-11-18T03:01:00Z', closed_acts: [{ act: 'a1', kind: 'ask-action' }],
    acts: [{ id: 'a1', kind: 'ask-action', author: 'operator', about: 'fix the font', 'port?': true },
           { id: 'u1', kind: 'unresolved', author: 'operator', about: 'where it lives', 'port?': true },
           { id: 'p1', kind: 'propose', author: 'operator', about: 'nobody took this up', 'port?': true },
           { id: 'r1', kind: 'report', author: 'agent-invented', target: 'a1', about: 'fixed', 'port?': false }] },
] };

test('only addressed items are drawn; an ask answered in its own turn is one of them', () => {
  const L = G.layout(data);
  assert.deepStrictEqual(L.rows.map((r) => r.type + ':' + (r.turn ? r.turn.id : r.act ? r.act.id : r.id)),
    ['turn:turn-t1', 'open:a1', 'open:u1', 'close:a1',
     'turn:turn-t2', 'open:q2',
     'turn:turn-t3', 'close:u1', 'close:q2', 'close:x0']);
  assert.ok(!L.openAt.has('p1'), 'an item left open is not drawn');
  assert.strictEqual(L.leftOpen, 1);
  assert.strictEqual(L.closeAt.get('a1').closer.id, 'r1', 'answered by the act that targets it');
  assert.strictEqual(L.closeAt.get('u1').closer.kind, 'approve', 'an unresolved item you then resolve');
  assert.strictEqual(L.closeAt.get('x0').earlier, true, 'opened before these turns: a row, no branch');
  assert.strictEqual(L.closeAt.get('x0').ended, true);
  assert.strictEqual(L.addressed, 3);
  assert.strictEqual(L.ended, 1);
});

test('a freed lane is reused', () => {
  const L = G.layout(data);
  assert.strictEqual(L.maxLanes, 2);   // a1 and u1 at once; q2 takes a1's lane
  assert.strictEqual(L.openAt.get('q2').lane, L.openAt.get('a1').lane);
});

test('render: what was raised, what addressed it, at panel size', () => {
  const html = G({ esc, link }).render(data).html;
  const text = html.replace(/<title>.*?<\/title>/g, '').replace(/<[^>]+>/g, ' ').replace(/\s+/g, ' ');
  assert.match(text, /ask-action fix the font \(you\)/);
  assert.match(text, /addressed: report by agent-invented · fixed/);
  assert.match(text, /addressed: approve by you · settled it/);
  assert.match(text, /× withdraw by you \(an earlier propose, opened before these turns\)/);
  assert.match(text, /3 addressed · 1 withdrawn · 3 turns · 1 left open, not drawn/);
  assert.doesNotMatch(text, /nobody took this up/);
  assert.doesNotMatch(html, /class="lane open"/);
  assert.doesNotMatch(html, /viewBox/, 'a viewBox lets the SVG shrink its text');
  assert.match(html, /<svg class="intents"[^>]* style="font-size:14px/);
  const ys = [...html.matchAll(/<text[^>]* y="(\d+)"/g)].map((m) => m[1]);
  assert.strictEqual(new Set(ys).size, ys.length, 'two labels on one row');
});
