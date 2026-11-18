// Live map of 馬's per-pattern threads along one operator-turn strand.
// Pure: the strand monitor's already-merged route data -> {title, html}.
'use strict';

const { wrap, spread } = require('./cascade-view');
const { connectionLabel } = require('./strand-view');

const X0 = 92, XSTEP = 132, Y0 = 86, YSTEP = 58;

function short(id) { return String(id || '?').slice(-8); }

function threadIndex(turn) {
  return new Map((((turn || {}).ma || {}).threads || []).map((t) => [t.pattern, t]));
}

function layout(data) {
  const turns = [...(data.turns || [])].reverse();
  const lanes = [], seen = new Set();
  for (const turn of turns) for (const thread of (((turn || {}).ma || {}).threads || [])) {
    if (!seen.has(thread.pattern)) { seen.add(thread.pattern); lanes.push(thread.pattern); }
  }
  const columns = turns.map((turn, i) => ({ turn, x: X0 + i * XSTEP, i }));
  const laneRows = spread(lanes.map((pattern, i) => ({ pattern, x: Y0 + i * YSTEP })), YSTEP)
    .map((r) => ({ pattern: r.pattern, y: r.x }));
  const laneY = new Map(laneRows.map((r) => [r.pattern, r.y]));
  const nodes = [];
  for (const col of columns) for (const thread of threadIndex(col.turn).values()) {
    const n = Object.keys(thread.parts || {}).length;
    nodes.push({ turn: col.turn.id, pattern: thread.pattern, status: thread.status,
      parts: thread.parts || {}, n, x: col.x, y: laneY.get(thread.pattern), r: 4 + n * 2 });
  }
  const nodeAt = new Map(nodes.map((n) => [`${n.turn}\0${n.pattern}`, n]));
  const dropped = [];
  for (let i = 1; i < columns.length; i++) for (const pattern of lanes) {
    if (nodeAt.has(`${columns[i - 1].turn.id}\0${pattern}`)
        && !nodeAt.has(`${columns[i].turn.id}\0${pattern}`)) {
      dropped.push({ pattern, turn: columns[i].turn.id, x: columns[i].x, y: laneY.get(pattern) });
    }
  }
  const xByTurn = new Map(columns.map((c) => [c.turn.id, c.x]));
  const quotes = (data.edges || []).filter((e) => e.kind === 'quotes')
    .map((e) => ({ ...e, node: nodeAt.get(`${e.from}\0${e.pattern}`), tx: xByTurn.get(e.to) }))
    .filter((e) => e.node && e.tx != null && e.tx !== e.node.x);
  const acts = columns.map((col, i) => {
    const current = col.turn.open_acts || [], previous = i ? columns[i - 1].turn.open_acts || [] : null;
    const prevIds = new Set((previous || []).map((a) => a.act));
    const curIds = new Set(current.map((a) => a.act));
    return { turn: col.turn.id, x: col.x, backlog: current.length,
      opened: previous ? current.filter((a) => !prevIds.has(a.act)) : [],
      closed: previous ? previous.filter((a) => !curIds.has(a.act)).length : 0 };
  });
  return { turns, lanes, columns, laneRows, nodes, dropped, quotes, acts,
    width: Math.max(320, X0 * 2 + Math.max(0, turns.length - 1) * XSTEP),
    height: Y0 + Math.max(1, lanes.length) * YSTEP + 120 };
}

function actClass(kind) { return 'act-' + String(kind || 'unknown').replace(/[^a-z0-9_-]/gi, '-'); }

module.exports = function maMapView({ esc, link, now = () => Date.now() }) {
  function render(data) {
    const L = layout(data);
    const lane = (r) => `<text class="lane" x="4" y="${r.y + 4}">${esc(wrap(r.pattern, 12, 2).join(' '))}</text>`;
    const quote = (e, i) => {
      const lift = 18 + (i % 4) * 7, mx = (e.node.x + e.tx) / 2;
      return `<a class="ln quote-edge" data-href="pt:${esc(e.to)}"><title>${esc(e.pattern)} ${esc(e.part)} quotes ${esc(e.to)}</title><path class="quote" d="M${e.node.x},${e.node.y - e.node.r} Q${mx},${e.node.y - lift} ${e.tx},${e.node.y}"/></a>`;
    };
    const node = (n) => `<a class="ln ma-node" data-href="pat:${esc(n.pattern)}"><title>${esc(n.pattern)} · ${esc(n.status)} · ${esc(Object.entries(n.parts).map(([p, v]) => `${p} ${v.window} from ${v.from}`).join('; '))}</title><circle class="thread ${n.status === 'occurrence' ? 'occurrence' : 'partial'}" cx="${n.x}" cy="${n.y}" r="${n.r}"/></a>`;
    const drop = (d) => `<a class="ln dropped-node" data-href="pt:${esc(d.turn)}"><title>${esc(d.pattern)} dropped at ${esc(d.turn)}</title><path class="dropped" data-turn="${esc(d.turn)}" d="M${d.x - 5},${d.y - 5} l10,10 m0,-10 l-10,10"/></a>`;
    const col = (c) => {
      const y = L.height - 78;
      return `<g class="turn-col" data-turn="${esc(c.turn.id)}"><a class="ln" data-href="pt:${esc(c.turn.id)}"><text x="${c.x}" y="${y}" text-anchor="middle">${esc(short(c.turn.id))}</text></a><a class="ln" data-href="step:${esc(c.turn.id)}"><text x="${c.x}" y="${y + 13}" text-anchor="middle">象 ${esc(String(c.turn.created_at || '').slice(11, 16))}</text></a><text class="connection" x="${c.x}" y="${y + 27}" text-anchor="middle">馬 ${esc(connectionLabel(c.turn, now()))}</text></g>`;
    };
    const act = (a) => {
      const y = L.height - 32;
      return `<g class="acts" data-turn="${esc(a.turn)}">${a.opened.map((x, i) => `<rect class="opened ${actClass(x.kind)}" x="${a.x - 10 + i * 4}" y="${y}" width="3" height="8"><title>opened ${esc(x.act)} · ${esc(x.kind || 'unknown')}</title></rect>`).join('')}<text x="${a.x}" y="${y + 19}" text-anchor="middle">${esc(a.backlog)} open${a.closed ? ` · ${a.closed} closed` : ''}</text></g>`;
    };
    const svg = `<svg class="cascade ma-map" viewBox="0 0 ${L.width} ${L.height}" width="${L.width}" style="max-width:100%;height:auto">
${L.laneRows.map(lane).join('\n')}
${L.quotes.map(quote).join('\n')}
${L.nodes.map(node).join('\n')}
${L.dropped.map(drop).join('\n')}
${L.columns.map(col).join('\n')}
${L.acts.map(act).join('\n')}
</svg>`;
    return { title: '馬 map', html: `<div data-ma-map><h1>馬 map <span class="dim">${esc(data.session || '?')}</span></h1><p>${link('strand:' + (data.session || ''), 'Circus monitor')} · ${link('herd:', '馬 herd')}</p>${svg}<p class="dim">oldest left · filled occurrence · hollow partial · × dropped · ticks acts opened here · number carried open backlog</p></div>` };
  }
  return { render };
};

module.exports.layout = layout;
