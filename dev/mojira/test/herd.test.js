'use strict';
const test = require('node:test');
const assert = require('node:assert');
const fs = require('node:fs');
const path = require('node:path');
const H = require('../herd-view');

const esc = (s) => String(s == null ? '' : s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const link = (href, text) => `<a class="ln" data-href="${esc(href)}">${esc(text)}</a>`;
const fixture = {
  cursor: '2026-11-18T06:04:00Z',
  lanes: [
    { session: 'session-a', agent: 'claude-13', turns: [
      { id: 'turn-a1', created_at: '2026-11-18T06:00:00Z', ma_status: 'read', threads_n: 4 },
      { id: 'turn-a2', created_at: '2026-11-18T06:04:00Z', ma_status: 'pending', threads_n: 1 }] },
    { session: 'session-b', agent: 'claude-5', turns: [
      { id: 'turn-b1', created_at: '2026-11-18T06:02:00Z', ma_status: 'no-proposed-patterns', threads_n: 0 }] },
  ],
  bells: [
    { at: '2026-11-18T06:01:00Z', from_agent: 'claude-13', from_session: 'session-a', to_agent: 'codex-15', turn_id: 'turn-a1', kind: 'request' },
    { at: '2026-11-18T06:03:00Z', from_agent: 'codex-15', to_agent: 'claude-5', to_session: 'session-b', turn_id: 'turn-b1', kind: 'answer' },
  ],
};

test('layout has one lane per session plus endpoint-only agents on a shared time axis', () => {
  const l = H.layout(fixture);
  assert.deepStrictEqual(l.rows.map((r) => r.key), ['session:session-a', 'session:session-b', 'agent:codex-15']);
  assert.ok(l.turns.find((p) => p.turn.id === 'turn-a1').x < l.turns.find((p) => p.turn.id === 'turn-b1').x);
  assert.strictEqual(l.bells[0].from.key, 'session:session-a');
  assert.strictEqual(l.bells[0].to.key, 'agent:codex-15');
  assert.strictEqual(l.bells[1].from.key, 'agent:codex-15');
  assert.strictEqual(l.bells[1].to.key, 'session:session-b');
});

test('render makes every dot and arc navigable and highlights one agent herd', () => {
  const html = H({ esc, link }).render(fixture, 'herd:claude-13').html;
  assert.match(html, /<a class="ln" data-href="pt:turn-a1">/);
  assert.match(html, /<a class="ln step-dot" data-href="step:turn-a1">/);
  assert.match(html, /<a class="ln herd-bell" data-href="pt:turn-a1">/);
  assert.match(html, /<a class="ln herd-bell dimmed" data-href="pt:turn-b1">/);
  assert.match(html, /herd-lane session dimmed/);
  assert.match(html, /data-href="mamap:session-a"/);
  assert.match(html, /data-href="strand:session-a"/);
});

test('cursor merge appends new material and does not duplicate old material', () => {
  const delta = { cursor: '2026-11-18T06:05:00Z', lanes: [
    { session: 'session-a', agent: 'claude-13', turns: [{ id: 'turn-a3', created_at: '2026-11-18T06:05:00Z', ma_status: 'read', threads_n: 2 }] },
  ], bells: [fixture.bells[0], { at: '2026-11-18T06:05:00Z', from_agent: 'claude-5', from_session: 'session-b', to_agent: 'claude-13', to_session: 'session-a', kind: 'request' }] };
  const merged = H.merge(fixture, delta);
  assert.deepStrictEqual(merged.lanes.find((l) => l.session === 'session-a').turns.map((t) => t.id), ['turn-a1', 'turn-a2', 'turn-a3']);
  assert.strictEqual(merged.bells.length, 3);
  assert.strictEqual(merged.cursor, delta.cursor);
});

test('core gives herd one visibility-aware cursor poller', () => {
  const core = fs.readFileSync(path.join(__dirname, '..', 'core.js'), 'utf8');
  assert.match(core, /cached\.data = herdView\.merge\(cached\.data, delta\)/);
  assert.match(core, /panel && panel\.visible && history\[history\.length - 1\] === href/);
  assert.strictEqual((core.match(/setInterval\(/g) || []).length, 1);
  assert.match(core, /if \(href\.startsWith\('herd:'\)\) pollHerd\(href\)/);
  assert.match(core, /herdUrl\(href, cached\.data\.cursor\)/);
});
