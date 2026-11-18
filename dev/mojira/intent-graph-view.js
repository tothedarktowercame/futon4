// Intent graph: one session's intents as branches, top to bottom.
// Joe 2026-11-18: "a flowchart that runs top to bottom, a bit like a git
// merge diagram, but showing the intents that are realised rather than
// commits or merges". Rows run oldest to newest. Each intent (an act that
// opens a port: a proposal, a 🈸 ask, an offer, ...) branches off the
// trunk at the turn that said it, and merges back at the turn that closed
// it, labelled with the act that closed it; a withdrawal ends in ×; an
// intent still open runs to the bottom. Text at the panel's size, no
// viewBox: one row per line of text, so nothing is scaled or overlaps.
// Pure: the strand route's merged data -> {title, html}.
'use strict';

const FONT = 14, ROW = 24, TRUNK = 14, LANE = 12, R = 5;
const ENDS = new Set(['withdraw', 'retract', 'cancel']);
// Intents are acts that set out to get something done. Questions, problem
// reports and checks also open a port (they expect an answer) but are not
// realised, so they are counted, not drawn: drawn, they filled the graph
// with lanes that never close (29 of 30 on the first live session).
const INTENTS = new Set(['propose', 'ask-action', 'offer', 'delegate', 'promise']);

function short(id) { return String(id || '?').slice(-6); }
function hhmm(iso) { return String(iso || '').slice(11, 16); }
function who(author) { return author === 'operator' ? 'you' : (author || '?'); }

// Rows and lanes from the strand data (turns newest first, as the route sends).
function layout(data) {
  const turns = [...(data.turns || [])].reverse();
  const acts = new Map();
  const closerOf = new Map();
  for (const t of turns) for (const a of t.acts || []) {
    acts.set(a.id, { ...a, turn: t.id });
    if (a.target && !closerOf.has(a.target)) closerOf.set(a.target, { ...a, turn: t.id });
  }
  const opened = new Set();
  let questions = 0;
  const rows = [];
  const lanes = [];          // lane index -> intent id or null (free)
  const laneOf = new Map();
  let maxLanes = 0;
  for (const t of turns) {
    rows.push({ type: 'turn', turn: t });
    for (const c of t.closed_acts || []) {
      if (!laneOf.has(c.act)) continue;   // opened before this window
      const lane = laneOf.get(c.act);
      const closer = closerOf.get(c.act) || null;
      rows.push({ type: 'close', id: c.act, lane, closer, ended: Boolean(closer && ENDS.has(closer.kind)) });
      lanes[lane] = null;
      laneOf.delete(c.act);
    }
    for (const a of t.acts || []) {
      if (!a['port?'] || opened.has(a.id)) continue;
      opened.add(a.id);
      if (!INTENTS.has(a.kind)) { questions++; continue; }
      let lane = lanes.indexOf(null);
      if (lane < 0) { lane = lanes.length; lanes.push(null); }
      lanes[lane] = a.id;
      laneOf.set(a.id, lane);
      maxLanes = Math.max(maxLanes, lanes.length);
      rows.push({ type: 'open', act: a, lane });
    }
  }
  rows.forEach((r, i) => { r.y = (i + 1) * ROW; });
  const openAt = new Map(rows.filter((r) => r.type === 'open').map((r) => [r.act.id, r]));
  const closeAt = new Map(rows.filter((r) => r.type === 'close').map((r) => [r.id, r]));
  const stillOpen = [...laneOf.keys()];
  const bottom = (rows.length + 1) * ROW;
  return { turns, rows, openAt, closeAt, stillOpen, maxLanes, bottom, questions,
    realised: [...closeAt.values()].filter((r) => !r.ended).length,
    ended: [...closeAt.values()].filter((r) => r.ended).length,
    textX: TRUNK + (maxLanes + 1) * LANE + 10 };
}

module.exports = function intentGraphView({ esc, link }) {
  function render(data) {
    const L = layout(data);
    const laneX = (lane) => TRUNK + (lane + 1) * LANE;
    const out = [];
    if (L.rows.length) out.push(`<path class="trunk" d="M${TRUNK},${L.rows[0].y} V${L.rows[L.rows.length - 1].y}"/>`);
    // lanes: from the open row down to the close row, or to the bottom
    for (const [id, o] of L.openAt) {
      const c = L.closeAt.get(id);
      const x = laneX(o.lane);
      const end = c ? c.y - ROW / 2 : L.bottom - ROW / 2;
      out.push(`<path class="lane ${c ? (c.ended ? 'ended' : 'realised') : 'open'}" d="M${TRUNK},${o.y - ROW / 2} C${TRUNK},${o.y} ${x},${o.y - ROW / 2} ${x},${o.y} V${end}"/>`);
      if (c) out.push(`<path class="lane ${c.ended ? 'ended' : 'realised'}" d="M${x},${end} C${x},${c.y} ${TRUNK},${c.y - ROW / 2} ${TRUNK},${c.y}"/>`);
    }
    const text = (y, body, cls = '') => `<text class="${cls}" x="${L.textX}" y="${y + 5}">${body}</text>`;
    let mission = null;
    for (const r of L.rows) {
      if (r.type === 'turn') {
        const t = r.turn;
        // the mission only where it changes, not on every row
        const m = t.mission_id && t.mission_id !== mission ? ' · ' + esc(t.mission_id) : '';
        mission = t.mission_id || mission;
        out.push(`<a class="ln" data-href="pt:${esc(t.id)}"><title>${esc(t.id)}</title><circle class="turn" cx="${TRUNK}" cy="${r.y}" r="${R}"/>${text(r.y, `<tspan class="dim">${esc(hhmm(t.created_at))} turn ${esc(short(t.id))}</tspan>${m}`, 'turnlabel')}</a>`);
      } else if (r.type === 'open') {
        const a = r.act;
        out.push(`<circle class="intent" cx="${laneX(r.lane)}" cy="${r.y}" r="${R - 1}"><title>${esc(a.id)}</title></circle>`);
        out.push(text(r.y, `<tspan class="kind">${esc(a.kind)}</tspan> ${esc(a.about || short(a.id))} <tspan class="dim">(${esc(who(a.author))})</tspan>`));
      } else {
        const c = r.closer;
        const what = r.ended ? `× ${esc(c.kind)}` : `realised: ${esc(c ? c.kind : 'closed')}`;
        out.push(text(r.y, `<tspan class="${r.ended ? 'endedlabel' : 'realisedlabel'}">${what}</tspan>${c ? ` <tspan class="dim">by ${esc(who(c.author))}${c.about ? ' · ' + esc(c.about) : ''}</tspan>` : ''}`));
      }
    }
    // as wide as the longest label (about 0.62em a character), so a panel
    // scrolls sideways only when a label really is that long
    const longest = Math.max(0, ...out.filter((o) => o.includes('<text')).map((o) => o.replace(/<title>.*?<\/title>/g, '').replace(/<[^>]+>/g, '').replace(/&[a-z]+;/g, 'x').length));
    const width = Math.ceil(L.textX + longest * FONT * 0.62 + 20);
    const svg = `<svg class="intents" width="${width}" height="${L.bottom + 4}" style="font-size:${FONT}px;max-width:none">
${out.join('\n')}
</svg>`;
    const summary = `${L.realised} realised · ${L.ended} withdrawn · ${L.stillOpen.length} still open · ${L.turns.length} turns`
      + (L.questions ? ` · ${L.questions} questions and reports not drawn` : '');
    return { title: 'intent graph', html: `<div data-ma-map><h1>Intent graph <span class="dim">${esc(data.session || '?')}</span></h1>
<p>${link('strand:' + (data.session || ''), 'Circus monitor')} · ${link('herd:', '馬 herd')}</p>
<p class="dim">oldest at the top · each intent branches off at the turn that said it and joins back where it was realised · × withdrawn · a line to the bottom is still open</p>
<p>${esc(summary)}</p>
<div style="overflow-x:auto">${svg}</div></div>` };
  }
  return { render };
};

module.exports.layout = layout;
