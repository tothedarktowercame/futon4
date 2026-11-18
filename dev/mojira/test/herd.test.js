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

test('a bell without a sender session attaches to that agent\'s session lane', () => {
  // Live 2026-11-18: bells from claude-13 carry no session, so they drew a
  // second "claude-13" agent lane beside its session lane.
  const data = { ...fixture, bells: [{ at: '2026-11-18T06:01:00Z', from_agent: 'claude-13', to_agent: 'codex-15', turn_id: 'turn-x', kind: 'request' }] };
  const l = H.layout(data);
  assert.strictEqual(l.bells[0].from.key, 'session:session-a');
  assert.ok(!l.rows.some((r) => r.key === 'agent:claude-13'), 'no duplicate agent lane');
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

test('common ground joins visible first supporting marks in first_at order and spans the mission support', () => {
  const data = groundFixture(), l = H.layout(data), g = H.commonGround(data, l);
  assert.strictEqual(g.bands.length, 1);
  assert.strictEqual(g.bands[0].x, l.turns.find((p) => p.turn.id === 'turn-a1').x);
  assert.strictEqual(g.bands[0].width, l.turns.find((p) => p.turn.id === 'turn-a2').x - g.bands[0].x);
  assert.deepStrictEqual(g.links.map((p) => [p.from.turn.id, p.to.turn.id]), [['turn-a1', 'turn-b1']]);
  assert.strictEqual(g.links[0].dash, '2 5');
});

test('proposal-only links are dotted; occurrences are solid, with escaped titles', () => {
  assert.strictEqual(H.patternDash(['proposed']), '2 5');
  assert.strictEqual(H.patternDash(['occurs']), 'none');
  assert.strictEqual(H.patternDash(['proposed', 'occurs']), 'none');
  const view = H({ esc, link });
  assert.match(view.render(groundFixture()).html, /stroke-dasharray="2 5"[^>]*><title>p&lt;invented&gt; · proposed/);
  assert.match(view.render(groundFixture(['proposed', 'occurs'])).html,
    /stroke-dasharray="none"[^>]*><title>p&lt;invented&gt; · proposed, occurs/);
});

test('off-screen lanes and turn IDs are skipped without borrowing another lane mark', () => {
  const data = groundFixture();
  for (const entry of data.common_ground) {
    entry.lanes = [entry.lanes[0],
      { session: 'missing-session', turns: ['turn-a1'], first_at: fixture.cursor },
      { session: 'session-a', turns: ['missing-turn', 'turn-b1'], first_at: fixture.cursor }];
  }
  assert.deepStrictEqual(H.commonGround(data), { bands: [], links: [] });
  assert.doesNotThrow(() => H({ esc, link }).render(data));
});

test('common-ground checkbox is default-on and hides both layers with local CSS', () => {
  const html = H({ esc, link }).render(groundFixture()).html;
  assert.match(html, /class="herd-common-toggle"[^>]*type="checkbox" checked/);
  assert.match(html, /<label for="herd-common-toggle">common ground<\/label>/);
  assert.match(html, /\.herd-common-toggle:not\(:checked\) ~ svg \.herd-common-ground \{ display: none; \}/);
  assert.ok(html.indexOf('class="herd-common-toggle"') < html.indexOf('<svg'));
  assert.ok(html.indexOf('<g class="herd-common-ground herd-missions">') < html.indexOf('<g class="herd-lane session'));
  assert.match(html, /<g class="herd-common-ground herd-patterns">/);
  assert.doesNotMatch(html, /onchange=|fetch\(/);
});

test('cursor polls keep old common ground and merge new supporting turns without duplicates', () => {
  const base = groundFixture(), same = H.merge(base, { lanes: [], bells: [], common_ground: [] });
  assert.deepStrictEqual(H.commonGround(same).links.map((p) => p.dash), ['2 5']);
  const delta = { common_ground: [{ ...base.common_ground[1], via: ['occurs'], lanes: [
    { session: 'session-a', agent: 'claude-13', turns: ['turn-a2'], first_at: fixture.cursor },
  ] }] };
  const merged = H.merge(same, delta), pattern = merged.common_ground.find((e) => e.kind === 'pattern');
  assert.strictEqual(merged.common_ground.length, 2);
  assert.strictEqual(pattern.lanes.length, 2);
  assert.deepStrictEqual(pattern.lanes.find((l) => l.session === 'session-a').turns, ['turn-a2', 'turn-a1']);
  assert.strictEqual(pattern.lanes.find((l) => l.session === 'session-a').first_at, '2026-11-18T06:00:00Z');
  assert.strictEqual(H.commonGround(merged).links[0].dash, 'none');
});
