// Parallel operator-session timelines and the bells crossing between them.
// Pure: the already-merged /api/alpha/circus/herd response -> {title, html}.
'use strict';

const { wrap, spread } = require('./cascade-view');

const X0 = 152, X1 = 1050, Y0 = 62, SESSION_GAP = 46, AGENT_GAP = 27;

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
  const sessions = (data.lanes || []).map((l) => ({ key: `session:${l.session}`, kind: 'session', session: l.session, agent: l.agent, data: l }));
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
  const endpointAgents = [];
  for (const b of data.bells || []) {
    if (!sideSession(b, 'from')) endpointAgents.push(b.from_agent);
    if (!sideSession(b, 'to')) endpointAgents.push(b.to_agent);
  }
  const known = new Set();
  const agents = endpointAgents.filter((a) => a && !known.has(a) && known.add(a))
    .map((agent) => ({ key: `agent:${agent}`, kind: 'agent', agent }));
  let wish = Y0;
  const rows = spread([...sessions, ...agents].map((r) => {
    const gap = r.kind === 'session' ? SESSION_GAP : AGENT_GAP;
    const out = { ...r, x: wish, gap }; wish += gap; return out;
  }), AGENT_GAP).map((r) => ({ ...r, y: r.x }));
  const rowByKey = new Map(rows.map((r) => [r.key, r]));
  const times = [...sessions.flatMap((l) => (l.data.turns || []).map((t) => timeMs(t.created_at))), ...(data.bells || []).map((b) => timeMs(b.at))].filter(Boolean);
  const min = times.length ? Math.min(...times) : 0, rawMax = times.length ? Math.max(...times) : min + 1;
  const max = rawMax === min ? min + 1 : rawMax;
  const x = (at) => X0 + (timeMs(at) - min) * (X1 - X0) / (max - min);
  const turns = sessions.flatMap((r) => (r.data.turns || []).map((turn) => ({ turn, row: r, x: x(turn.created_at), y: rowByKey.get(r.key).y })));
  const endpoint = (b, side) => {
    const session = sideSession(b, side);
    return rowByKey.get(session ? `session:${session}` : `agent:${b[`${side}_agent`]}`);
  };
  const bells = (data.bells || []).map((bell) => ({ bell, from: endpoint(bell, 'from'), to: endpoint(bell, 'to'), x: x(bell.at) })).filter((b) => b.from && b.to);
  const hi = new Set(highlighted);
  const active = (agent) => !hi.size || hi.has(agent);
  return { rows, turns, bells, active, min, max, width: X1 + 30,
    height: Math.max(180, (rows.at(-1) || { y: Y0 }).y + 55) };
}

// Geometry is based only on supporting marks present in this rendered view.
function commonGround(data, L = layout(data)) {
  const marks = new Map();
  for (const point of L.turns) {
    if (!Number.isFinite(point.x) || point.x < X0 || point.x > X1) continue;
    if (!marks.has(point.row.session)) marks.set(point.row.session, new Map());
    marks.get(point.row.session).set(point.turn.id, point);
  }
  const bands = [], links = [];
  const x = (at) => Math.max(X0, Math.min(X1, X0 + (timeMs(at) - L.min) * (X1 - X0) / (L.max - L.min)));
  for (const entry of data.common_ground || []) {
    const seen = new Set();
    const members = (entry.lanes || []).flatMap((lane) => {
      if (seen.has(lane.session)) return [];
      seen.add(lane.session);
      const points = [...new Set(lane.turns || [])].map((id) => marks.get(lane.session)?.get(id)).filter(Boolean)
        .sort((a, b) => timeMs(a.turn.created_at) - timeMs(b.turn.created_at));
      return points.length ? [{ lane, points, first: points[0], at: timeMs(lane.first_at) || timeMs(points[0].turn.created_at) }] : [];
    }).sort((a, b) => a.at - b.at);
    if (members.length < 2) continue;
    if (entry.kind === 'mission') {
      const start = x(new Date(Math.min(...members.map((m) => m.at))).toISOString());
      const end = Math.max(...members.flatMap((m) => m.points.map((point) => point.x)));
      bands.push({ entry, members, x: start, width: Math.max(2, end - start) });
    } else if (entry.kind === 'pattern') {
      for (let i = 1; i < members.length; i += 1) {
        links.push({ entry, from: members[i - 1].first, to: members[i].first, dash: patternDash(entry.via) });
      }
    }
  }
  return { bands, links };
}

function patternDash(via = []) {
  return via.length && via.every((kind) => kind === 'proposed') ? '2 5' : 'none';
}

function statusClass(turn) {
  if (turn.unavailable || turn.ma_status === 'unavailable') return 'unavailable';
  if (turn.ma_status === 'pending') return 'pending';
  if (turn.ma_status === 'no-proposed-patterns') return 'no-proposed-patterns';
  return 'read';
}

module.exports = function herdView({ esc, link }) {
  function render(data, href = 'herd:') {
    const selected = agentsArg(href), L = layout(data, selected), ground = commonGround(data, L);
    const row = (r) => {
      const dim = L.active(r.agent) ? '' : ' dimmed';
      if (r.kind === 'agent') return `<a class="ln herd-lane endpoint${dim}" data-href="herd:${esc(r.agent)}"><title>bell endpoint only</title><text x="4" y="${r.y + 4}">${esc(r.agent)}</text></a>`;
      return `<g class="herd-lane session${dim}"><a class="ln" data-href="mamap:${esc(r.session)}"><text x="4" y="${r.y}">${esc(wrap(r.agent, 16, 1).join(' '))} · ${esc(short(r.session))}</text></a><a class="ln" data-href="strand:${esc(r.session)}"><text class="strand-link" x="4" y="${r.y + 14}">strand</text></a></g>`;
    };
    const turn = (p) => {
      const t = p.turn, cls = statusClass(t), dim = L.active(p.row.agent) ? '' : ' dimmed';
      const radius = 3 + Math.min(8, Math.sqrt(Number(t.threads_n || 0)) * 2);
      return `<g class="herd-turn ${cls}${dim}"><a class="ln" data-href="pt:${esc(t.id)}"><title>${esc(t.id)} · ${esc(cls)} · ${esc(t.threads_n || 0)} threads</title><circle cx="${p.x}" cy="${p.y}" r="${radius}"/></a><a class="ln step-dot" data-href="step:${esc(t.id)}"><text x="${p.x}" y="${p.y - radius - 3}" text-anchor="middle">象</text></a></g>`;
    };
    const bell = (p, i) => {
      const b = p.bell, touch = !selected.length || selected.includes(b.from_agent) || selected.includes(b.to_agent);
      const bend = 22 + (i % 5) * 7, href = b.turn_id ? `pt:${b.turn_id}` : `herd:${b.to_agent || ''}`;
      return `<a class="ln herd-bell${touch ? '' : ' dimmed'}" data-href="${esc(href)}"><title>${esc(b.kind || 'bell')} · ${esc(b.from_agent)} → ${esc(b.to_agent)}${b.turn_id ? ' · ' + esc(b.turn_id) : ''}</title><path d="M${p.x},${p.from.y} C${p.x + bend},${p.from.y} ${p.x + bend},${p.to.y} ${p.x},${p.to.y}"/></a>`;
    };
    const band = (b) => {
      const ys = b.members.map((m) => m.first.y), top = Math.min(...ys), bottom = Math.max(...ys);
      const dim = b.members.some((m) => L.active(m.lane.agent)) ? '' : ' dimmed';
      return `<g class="herd-mission${dim}"><title>${esc(b.entry.key)}</title>
${ys.map((y) => `<rect x="${b.x}" y="${y - 13}" width="${b.width}" height="26" rx="4"/>`).join('')}
<path d="M${b.x},${top} V${bottom}"/><text x="${b.x + 4}" y="${top - 17}">${esc(b.entry.key)}</text></g>`;
    };
    const pattern = (p) => {
      const dim = L.active(p.from.row.agent) || L.active(p.to.row.agent) ? '' : ' dimmed';
      return `<path class="herd-pattern${dim}" stroke-dasharray="${p.dash}" d="M${p.from.x},${p.from.y} L${p.to.x},${p.to.y}"><title>${esc(p.entry.key)} · ${esc((p.entry.via || []).join(', '))}</title></path>`;
    };
    const svg = `<svg class="cascade herd" viewBox="0 0 ${L.width} ${L.height}" width="${L.width}" style="max-width:100%;height:auto">
<g class="herd-common-ground herd-missions">${ground.bands.map(band).join('\n')}</g>
<g class="herd-common-ground herd-patterns">${ground.links.map(pattern).join('\n')}</g>
${L.rows.map(row).join('\n')}
${L.bells.map(bell).join('\n')}
${L.turns.map(turn).join('\n')}
</svg>`;
    const focus = selected.length ? ` · highlighting ${selected.join(', ')}` : '';
    return { title: '馬 herd', html: `<div data-herd><style>
[data-herd] > .herd-common-toggle:not(:checked) ~ svg .herd-common-ground { display: none; }
svg.herd .herd-mission rect { fill: var(--vscode-editorInfo-foreground, #49c); fill-opacity: .09; }
svg.herd .herd-mission path { fill: none; stroke: var(--vscode-editorInfo-foreground, #49c); stroke-opacity: .25; }
svg.herd .herd-mission text { fill: var(--vscode-descriptionForeground, #888); font-size: 10px; }
svg.herd .herd-pattern { fill: none; stroke: var(--vscode-charts-purple, #a78bd4); stroke-width: 1.4; stroke-opacity: .65; stroke-linecap: round; }
</style><h1>馬 herd <span class="dim">parallel sessions${esc(focus)}</span></h1><p>${link('home', 'Mojira home')}</p><input class="herd-common-toggle" id="herd-common-toggle" type="checkbox" checked><label for="herd-common-toggle">common ground</label>${svg}<p class="dim">shared time axis · dot size = mapped threads · arcs = bells · thin lanes = bell-only agents</p></div>` };
  }
  return { render };
};

module.exports.merge = merge;
module.exports.layout = layout;
module.exports.agentsArg = agentsArg;

module.exports.commonGround = commonGround;
module.exports.patternDash = patternDash;
