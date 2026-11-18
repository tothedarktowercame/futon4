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

// Joe 2026-11-18, after the intent graph: "can you get the herd to do a
// similar vertical timeline with git merge styling?"
test('vertical layout: one column per session, then bell-only agents; one row per event, oldest first', () => {
  const l = H.layout(fixture);
  assert.deepStrictEqual(l.lanes.map((r) => r.key), ['session:session-a', 'session:session-b', 'agent:codex-15']);
  assert.deepStrictEqual(l.events.map((e) => (e.type === 'turn' ? e.turn.id : `${e.bell.from_agent}>${e.bell.to_agent}`)),
    ['turn-a1', 'claude-13>codex-15', 'turn-b1', 'codex-15>claude-5', 'turn-a2']);
  const ys = l.events.map((e) => e.y);
  assert.deepStrictEqual(ys, [...ys].sort((a, b) => a - b), 'rows run down in time order');
  assert.strictEqual(new Set(ys).size, ys.length, 'one event per row');
  assert.strictEqual(l.bells[0].from.key, 'session:session-a');
  assert.strictEqual(l.bells[0].to.key, 'agent:codex-15');
  assert.strictEqual(l.bells[1].to.key, 'session:session-b');
  // a lane's line runs from its first event to its last
  assert.deepStrictEqual(l.span.get('session:session-a'), { top: ys[0], bottom: ys[4] });
  assert.deepStrictEqual(l.span.get('agent:codex-15'), { top: ys[1], bottom: ys[3] });
});

test('render: every dot and bell navigable, labels at panel size, one agent highlighted', () => {
  const html = H({ esc, link }).render(fixture, 'herd:claude-13').html;
  assert.match(html, /<a class="ln" data-href="pt:turn-a1">/);
  assert.match(html, /<a class="ln" data-href="step:turn-a1">/);
  assert.match(html, /<a class="ln herd-bell" data-href="pt:turn-a1">/);
  assert.match(html, /<a class="ln herd-bell dimmed" data-href="pt:turn-b1">/);
  assert.match(html, /herd-lane session herd-c-orange dimmed/);
  assert.match(html, /data-href="mamap:session-a"/);
  assert.match(html, /data-href="strand:session-a"/);
  const text = html.replace(/<title>.*?<\/title>/g, '').replace(/<style>[\s\S]*?<\/style>/g, '').replace(/<[^>]+>/g, ' ').replace(/\s+/g, ' ');
  assert.match(text, /06:01 claude-13 → codex-15 request/);
  assert.match(text, /06:00 claude-13 turn a1 · 4 馬/);
  assert.doesNotMatch(html, /viewBox/, 'a viewBox lets the SVG shrink its text');
  assert.match(html, /<svg class="herd-v"[^>]* style="font-size:14px/);
  const ys = [...html.matchAll(/<text x="[\d.]+" y="([\d.]+)"/g)].map((m) => m[1]);
  assert.strictEqual(new Set(ys).size, ys.length, 'two labels on one row');
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

test('core gives herd one visibility-aware cursor poller', () => {
  const core = fs.readFileSync(path.join(__dirname, '..', 'core.js'), 'utf8');
  assert.match(core, /cached\.data = herdView\.merge\(cached\.data, delta\)/);
  assert.match(core, /panel && panel\.visible && history\[history\.length - 1\] === href/);
  assert.strictEqual((core.match(/setInterval\(/g) || []).length, 1);
  assert.match(core, /if \(href\.startsWith\('herd:'\)\) pollHerd\(href\)/);
  assert.match(core, /herdUrl\(href, cached\.data\.cursor\)/);
});

test('a bell without a sender session attaches to that agent\'s session lane', () => {
  // Live 2026-11-18: bells from claude-13 carry no session, so they drew a
  // second "claude-13" agent lane beside its session lane.
  const data = { ...fixture, bells: [{ at: '2026-11-18T06:01:00Z', from_agent: 'claude-13', to_agent: 'codex-15', turn_id: 'turn-x', kind: 'request' }] };
  const l = H.layout(data);
  assert.strictEqual(l.bells[0].from.key, 'session:session-a');
  assert.ok(!l.lanes.some((r) => r.key === 'agent:claude-13'), 'no duplicate agent lane');
});

function groundFixture(via = ['proposed']) {
  const members = [
    { session: 'session-b', agent: 'claude-5', turns: ['turn-b1'], first_at: '2026-11-18T06:02:00Z' },
    { session: 'session-a', agent: 'claude-13', turns: ['turn-a2', 'turn-a1'], first_at: '2026-11-18T06:00:00Z' },
  ];
  return { ...fixture, common_ground: [
    { kind: 'mission', key: 'M-invented', lanes: members, via: [] },
    { kind: 'pattern', key: 'p<invented>', lanes: members, via },
  ] };
}

test('common ground is listed as text: entries shared by two or more sessions, in first_at order', () => {
  const g = H.commonGround(groundFixture());
  assert.deepStrictEqual(g.map((e) => e.key), ['M-invented', 'p<invented>']);
  assert.deepStrictEqual(g[1].lanes.map((l) => l.agent), ['claude-13', 'claude-5']);
  const html = H({ esc, link }).render(groundFixture()).html;
  assert.match(html, /COMMON GROUND/);
  assert.match(html, /p&lt;invented&gt;/);
  const one = { ...fixture, common_ground: [{ kind: 'pattern', key: 'solo', via: [], lanes: [{ session: 'session-a', agent: 'claude-13', first_at: fixture.cursor }] }] };
  assert.deepStrictEqual(H.commonGround(one), [], 'one session is not common ground');
});

test('cursor polls keep old common ground and merge new supporting turns without duplicates', () => {
  const base = groundFixture(), same = H.merge(base, { lanes: [], bells: [], common_ground: [] });
  assert.strictEqual(H.commonGround(same).length, 2);
  const delta = { common_ground: [{ ...base.common_ground[1], via: ['occurs'], lanes: [
    { session: 'session-a', agent: 'claude-13', turns: ['turn-a2'], first_at: fixture.cursor },
  ] }] };
  const merged = H.merge(same, delta), pattern = merged.common_ground.find((e) => e.kind === 'pattern');
  assert.strictEqual(merged.common_ground.length, 2);
  assert.strictEqual(pattern.lanes.length, 2);
  assert.deepStrictEqual(pattern.lanes.find((l) => l.session === 'session-a').turns, ['turn-a2', 'turn-a1']);
  assert.strictEqual(pattern.lanes.find((l) => l.session === 'session-a').first_at, '2026-11-18T06:00:00Z');
});
