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
  return { ...base, ...incoming, lanes: [...lanes.values()], bells: [...bells.values()].sort((a, b) => timeMs(a.at) - timeMs(b.at)),
    cursor: incoming.cursor || base.cursor };
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

function statusClass(turn) {
  if (turn.unavailable || turn.ma_status === 'unavailable') return 'unavailable';
  if (turn.ma_status === 'pending') return 'pending';
  if (turn.ma_status === 'no-proposed-patterns') return 'no-proposed-patterns';
  return 'read';
}

module.exports = function herdView({ esc, link }) {
  function render(data, href = 'herd:') {
    const selected = agentsArg(href), L = layout(data, selected);
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
    const svg = `<svg class="cascade herd" viewBox="0 0 ${L.width} ${L.height}" width="${L.width}" style="max-width:100%;height:auto">
${L.rows.map(row).join('\n')}
${L.bells.map(bell).join('\n')}
${L.turns.map(turn).join('\n')}
</svg>`;
    const focus = selected.length ? ` · highlighting ${selected.join(', ')}` : '';
    return { title: '馬 herd', html: `<div data-herd><h1>馬 herd <span class="dim">parallel sessions${esc(focus)}</span></h1><p>${link('home', 'Mojira home')}</p>${svg}<p class="dim">shared time axis · dot size = mapped threads · arcs = bells · thin lanes = bell-only agents</p></div>` };
  }
  return { render };
};

module.exports.merge = merge;
module.exports.layout = layout;
module.exports.agentsArg = agentsArg;
