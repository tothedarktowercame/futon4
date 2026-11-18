// Parallel operator sessions and the bells crossing between them, as a
// vertical timeline in git-merge style (Joe 2026-11-18, after the intent
// graph: "can you get the herd to do a similar vertical timeline with git
// merge styling?"). One narrow coloured column per session (and per agent
// seen only at a bell's end), one row per event, oldest at the top: a turn
// is a dot in its session's column; a bell branches from the sender's
// column into the receiver's. Every row has a label at the panel's text
// size; no viewBox, so nothing is scaled. Common ground (patterns and
// missions shared across sessions) is listed under the graph as text.
// Pure: the already-merged /api/alpha/circus/herd response -> {title, html}.
'use strict';

const FONT = 14, ROW = 24, LANE = 18, X0 = 14, R = 5;
const COLOURS = ['blue', 'orange', 'green', 'purple', 'red', 'yellow'];

function short(id) { return String(id || '?').slice(-8); }
function timeMs(s) { const n = Date.parse(s || ''); return Number.isFinite(n) ? n : 0; }
function agentsArg(href) { return String(href || '').split(':').slice(1).join(':').split(',').map((x) => x.trim()).filter(Boolean); }

function merge(current, delta) {
  const base = current || { lanes: [], bells: [] }, incoming = delta || {};
  const lanes = new Map((base.lanes || []).map((l) => [l.session, { ...l, turns: [...(l.turns || [])] }]));
  for (const lane of incoming.lanes || []) {
    const old = lanes.get(lane.session) || { session: lane.session, agent: lane.agent, turns: [] };
    const turns = new Map((old.turns || []).map((t) => [t.id, t]));
    for (const turn of lane.turns || []) turns.set(turn.id, turn);
    lanes.set(lane.session, { ...old, ...lane, turns: [...turns.values()].sort((a, b) => timeMs(a.created_at) - timeMs(b.created_at)) });
  }
  const bells = new Map();
  for (const b of [...(base.bells || []), ...(incoming.bells || [])]) {
    bells.set([b.at, b.from_agent, b.from_session || '', b.to_agent, b.to_session || '', b.turn_id || '', b.kind || ''].join('\0'), b);
  }
  // Cursor deltas describe only their new window, just as lanes and bells do.
  const ground = new Map();
  for (const entry of [...(base.common_ground || []), ...(incoming.common_ground || [])]) {
    const key = JSON.stringify([entry.kind, entry.key]), old = ground.get(key);
    const members = new Map((old?.lanes || []).map((lane) => [lane.session, lane]));
    for (const lane of entry.lanes || []) {
      const prev = members.get(lane.session);
      members.set(lane.session, { ...prev, ...lane,
        first_at: prev && timeMs(prev.first_at) < timeMs(lane.first_at) ? prev.first_at : lane.first_at,
        turns: [...new Set([...(prev?.turns || []), ...(lane.turns || [])])] });
    }
    ground.set(key, { ...old, ...entry, lanes: [...members.values()],
      via: [...new Set([...(old?.via || []), ...(entry.via || [])])] });
  }
  return { ...base, ...incoming, lanes: [...lanes.values()], bells: [...bells.values()].sort((a, b) => timeMs(a.at) - timeMs(b.at)),
    cursor: incoming.cursor || base.cursor, common_ground: [...ground.values()] };
}

function layout(data, highlighted = []) {
  const firstAt = (l) => Math.min(...(l.turns || []).map((t) => timeMs(t.created_at)).filter(Boolean), Infinity);
  const sessions = [...(data.lanes || [])].sort((a, b) => firstAt(a) - firstAt(b))
    .map((l) => ({ key: `session:${l.session}`, kind: 'session', session: l.session, agent: l.agent, data: l }));
  const sessionKeys = new Set(sessions.map((l) => l.session));
  // A bell names its sender's agent but usually not its session. Attach it
  // to that agent's session lane when exactly one of its lanes spans the
  // bell's time (or it has only one lane), not to a separate agent lane.
  const sessionFor = (agent, at) => {
    const own = sessions.filter((l) => l.agent === agent);
    if (own.length === 1) return own[0].session;
    const t = timeMs(at);
    const spanning = own.filter((l) => {
      const ts = (l.data.turns || []).map((x) => timeMs(x.created_at)).filter(Boolean);
      return ts.length && Math.min(...ts) <= t && t <= Math.max(...ts);
    });
    return spanning.length === 1 ? spanning[0].session : null;
  };
  const sideSession = (b, side) => {
    const s = b[`${side}_session`];
    return s && sessionKeys.has(s) ? s : sessionFor(b[`${side}_agent`], b.at);
  };
  const bellsSorted = [...(data.bells || [])].sort((a, b) => timeMs(a.at) - timeMs(b.at));
  const known = new Set();
  const agents = [];
  for (const b of bellsSorted) for (const side of ['from', 'to']) {
    const a = b[`${side}_agent`];
    if (a && !sideSession(b, side) && !known.has(a)) { known.add(a); agents.push({ key: `agent:${a}`, kind: 'agent', agent: a }); }
  }
  const lanes = [...sessions, ...agents].map((l, i) => ({ ...l, index: i, x: X0 + i * LANE, colour: COLOURS[i % COLOURS.length] }));
  const laneByKey = new Map(lanes.map((l) => [l.key, l]));
  const laneOf = (b, side) => {
    const s = sideSession(b, side);
    return laneByKey.get(s ? `session:${s}` : `agent:${b[`${side}_agent`]}`);
  };
  const events = [
    ...sessions.flatMap((l) => (l.data.turns || []).map((turn) => ({ type: 'turn', at: timeMs(turn.created_at), turn, lane: laneByKey.get(l.key) }))),
    ...bellsSorted.map((bell) => ({ type: 'bell', at: timeMs(bell.at), bell, from: laneOf(bell, 'from'), to: laneOf(bell, 'to') }))
      .filter((e) => e.from && e.to),
  ].sort((a, b) => a.at - b.at || (a.type === b.type ? 0 : a.type === 'turn' ? -1 : 1));
  events.forEach((e, i) => { e.y = (i + 1) * ROW; });
  // each lane's line runs from its first event to its last
  const span = new Map();
  const touch = (lane, y) => { if (!lane) return; const s = span.get(lane.key) || { top: y, bottom: y }; span.set(lane.key, { top: Math.min(s.top, y), bottom: Math.max(s.bottom, y) }); };
  // A bell's branch leaves its sender's column half a row above the bell's
  // row and enters the receiver's at the row, so those are the points the
  // lines must reach (Joe 2026-11-18: a lane whose first event was a bell it
  // sent showed the branch starting in empty space above the lane).
  for (const e of events) { if (e.type === 'turn') touch(e.lane, e.y); else { touch(e.from, e.from === e.to ? e.y : e.y - ROW / 2); touch(e.to, e.y); } }
  const hi = new Set(highlighted);
  const active = (agent) => !hi.size || hi.has(agent);
  return { lanes, events, span, active, textX: X0 + lanes.length * LANE + 8,
    // kept for callers that read them: the turn and bell events
    turns: events.filter((e) => e.type === 'turn'), bells: events.filter((e) => e.type === 'bell'),
    height: (events.length + 1) * ROW };
}

// Patterns and missions that more than one session took up, as text.
function commonGround(data) {
  return (data.common_ground || []).map((entry) => {
    const seen = new Set();
    const lanes = (entry.lanes || []).filter((l) => !seen.has(l.session) && seen.add(l.session))
      .sort((a, b) => timeMs(a.first_at) - timeMs(b.first_at));
    return { kind: entry.kind, key: entry.key, via: entry.via || [], lanes };
  }).filter((e) => e.lanes.length >= 2);
}

function statusClass(turn) {
  if (turn.unavailable || turn.ma_status === 'unavailable') return 'unavailable';
  if (turn.ma_status === 'pending') return 'pending';
  if (turn.ma_status === 'no-proposed-patterns') return 'no-proposed-patterns';
  return 'read';
}

function hhmm(at) { return String(at || '').slice(11, 16); }

module.exports = function herdView({ esc, link }) {
  function render(data, href = 'herd:') {
    const selected = agentsArg(href), L = layout(data, selected), ground = commonGround(data);
    const col = (lane) => `herd-c-${lane.colour}`;
    const who = (lane, agent) => `<tspan class="${lane ? col(lane) : ''}">${esc(agent || '?')}</tspan>`;
    const out = [];
    for (const lane of L.lanes) {
      const s = L.span.get(lane.key);
      if (!s) continue;
      const dim = L.active(lane.agent) ? '' : ' dimmed';
      out.push(`<path class="herd-lane ${lane.kind} ${col(lane)}${dim}" d="M${lane.x},${s.top} V${s.bottom}"/>`);
    }
    for (const e of L.events) {
      const y = e.y;
      if (e.type === 'turn') {
        const t = e.turn, dim = L.active(e.lane.agent) ? '' : ' dimmed';
        out.push(`<g class="herd-turn${dim}"><a class="ln" data-href="turn:${esc(t.id)}"><title>${esc(t.id)} · ${esc(statusClass(t))}</title><circle class="${col(e.lane)}" cx="${e.lane.x}" cy="${y}" r="${R}"/></a>`
          + `<a class="ln" data-href="turn:${esc(t.id)}"><text x="${L.textX}" y="${y + 5}"><tspan class="dim">${esc(hhmm(t.created_at))}</tspan> ${who(e.lane, e.lane.agent)} <tspan class="dim">turn ${esc(String(t.id || '?').replace(/^turn-/, ''))}${t.mission_id ? ' · ' + esc(t.mission_id) : ''}${t.threads_n ? ` · ${esc(t.threads_n)} 馬` : ''}</tspan></text></a></g>`);
      } else {
        const b = e.bell, fx = e.from.x, tx = e.to.x;
        const touched = !selected.length || selected.includes(b.from_agent) || selected.includes(b.to_agent);
        const href2 = b.turn_id ? `turn:${b.turn_id}` : `herd:${b.to_agent || ''}`;
        const d = fx === tx ? `M${fx},${y - ROW / 2} V${y}` : `M${fx},${y - ROW / 2} C${fx},${y} ${tx},${y - ROW / 2} ${tx},${y}`;
        out.push(`<a class="ln herd-bell${touched ? '' : ' dimmed'}" data-href="${esc(href2)}"><title>${esc(b.kind || 'bell')} · ${esc(b.from_agent)} → ${esc(b.to_agent)}${b.turn_id ? ' · ' + esc(b.turn_id) : ''}</title>`
          + `<path class="${col(e.from)}" d="${d}"/><circle class="bell-end ${col(e.to)}" cx="${tx}" cy="${y}" r="${R - 2}"/>`
          + `<text x="${L.textX}" y="${y + 5}"><tspan class="dim">${esc(hhmm(b.at))}</tspan> ${who(e.from, b.from_agent)} → ${who(e.to, b.to_agent)} <tspan class="dim">${esc(b.kind || 'bell')}</tspan></text></a>`);
      }
    }
    const longest = Math.max(0, ...out.filter((o) => o.includes('<text')).map((o) => o.replace(/<title>.*?<\/title>/g, '').replace(/<[^>]+>/g, '').replace(/&[a-z]+;/g, 'x').length));
    const width = Math.ceil(L.textX + longest * FONT * 0.62 + 20);
    const svg = `<svg class="herd-v" width="${width}" height="${L.height + 4}" style="font-size:${FONT}px;max-width:none">
${out.join('\n')}
</svg>`;
    const legend = L.lanes.map((lane) => (lane.kind === 'session'
      ? `<li><span class="herd-key ${col(lane)}">●</span> ${esc(lane.agent)} <span class="dim">${esc(short(lane.session))}</span> · ${link('mamap:' + lane.session, 'intent graph')} · ${link('strand:' + lane.session, 'strand')}</li>`
      : `<li><span class="herd-key ${col(lane)}">○</span> ${link('herd:' + lane.agent, lane.agent)} <span class="dim">seen only at a bell's end</span></li>`)).join('');
    const groundList = ground.length ? `<div class="gs-h">COMMON GROUND</div><ul>${ground.map((g) => `<li>${esc(g.kind)} ${g.kind === 'pattern' ? link('pat:' + g.key, g.key) : esc(g.key)} <span class="dim">${g.lanes.map((l) => esc(l.agent)).join(' → ')}${g.via.length ? ' · ' + esc(g.via.join(', ')) : ''}</span></li>`).join('')}</ul>` : '';
    const focus = selected.length ? ` · highlighting ${selected.join(', ')}` : '';
    return { title: '馬 herd', html: `<div data-herd><style>
svg.herd-v text { fill: var(--vscode-editor-foreground); font-size: 14px; } svg.herd-v .dim { fill: var(--vscode-descriptionForeground); }
svg.herd-v path { fill: none; stroke-width: 2; } svg.herd-v .herd-lane.agent { stroke-dasharray: 3 3; }
svg.herd-v .dimmed { opacity: .3; } svg.herd-v .bell-end { fill: var(--vscode-editor-background); stroke-width: 2; }
${COLOURS.map((c) => `svg.herd-v path.herd-c-${c}, svg.herd-v .bell-end.herd-c-${c} { stroke: var(--vscode-charts-${c}); } svg.herd-v circle.herd-c-${c}:not(.bell-end) { fill: var(--vscode-charts-${c}); } svg.herd-v tspan.herd-c-${c}, .herd-key.herd-c-${c} { fill: var(--vscode-charts-${c}); color: var(--vscode-charts-${c}); }`).join('\n')}
</style><h1>馬 herd <span class="dim">parallel sessions${esc(focus)}</span></h1><p>${link('home', 'Mojira home')}</p>
<ul>${legend}</ul>
<p class="dim">oldest at the top · a dot is a turn · a branch is a bell, from the sender's column into the receiver's</p>
<div style="overflow-x:auto">${svg}</div>${groundList}</div>` };
  }
  return { render };
};

module.exports.merge = merge;
module.exports.layout = layout;
module.exports.agentsArg = agentsArg;
module.exports.commonGround = commonGround;
