// Intent graph: the exchange in one session, top to bottom.
// Joe 2026-11-18: "a flowchart that runs top to bottom, a bit like a git
// merge diagram, but showing the intents that are realised"; then: "the
// interest is not really in tracking the 'open' items which I might read
// and implicitly accept but the items that actually are addressed and form
// part of the exchange: an ask item that is answered or an unresolved item
// that I then resolve are the typical cases".
// So only acts that open a port AND are closed in the window are drawn, of
// any kind (a question answered is part of the exchange). Each branches off
// the trunk at the turn that said it and joins back where it was
// addressed, labelled with the act that addressed it; a withdrawal ends in
// x. Items left open are counted, not drawn. An ask and its answer often
// share one turn record (the operator's turn and the agent's reply), so a
// turn's openings are laid out before its closings.
// Text at the panel's size, no viewBox, one label per row.
// Pure: the strand route's merged data -> {title, html}.
'use strict';

const FONT = 14, ROW = 24, TRUNK = 14, LANE = 12, R = 5;
const ENDS = new Set(['withdraw', 'retract', 'cancel']);

function short(id) { return String(id || '?').slice(-6); }
function hhmm(iso) { return String(iso || '').slice(11, 16); }
function who(author) { return author === 'operator' ? 'you' : (author || '?'); }

// Rows and lanes from the strand data (turns newest first, as the route sends).
function layout(data) {
  const turns = [...(data.turns || [])].reverse();
  const closerOf = new Map();
  const closedIn = new Set();
  for (const t of turns) {
    for (const a of t.acts || []) if (a.target && !closerOf.has(a.target)) closerOf.set(a.target, { ...a, turn: t.id });
    for (const c of t.closed_acts || []) closedIn.add(c.act);
  }
  const seen = new Set();
  let leftOpen = 0;
  const rows = [];
  const lanes = [];          // lane index -> act id or null (free)
  const laneOf = new Map();
  let maxLanes = 0;
  for (const t of turns) {
    rows.push({ type: 'turn', turn: t });
    for (const a of t.acts || []) {
      if (!a['port?'] || seen.has(a.id)) continue;
      seen.add(a.id);
      if (!closedIn.has(a.id)) { leftOpen++; continue; }
      let lane = lanes.indexOf(null);
      if (lane < 0) { lane = lanes.length; lanes.push(null); }
      lanes[lane] = a.id;
      laneOf.set(a.id, lane);
      maxLanes = Math.max(maxLanes, lanes.length);
      rows.push({ type: 'open', act: a, lane });
    }
    for (const c of t.closed_acts || []) {
      const closer = closerOf.get(c.act) || null;
      const ended = Boolean(closer && ENDS.has(closer.kind));
      if (laneOf.has(c.act)) {
        const lane = laneOf.get(c.act);
        rows.push({ type: 'close', id: c.act, lane, closer, ended });
        lanes[lane] = null;
        laneOf.delete(c.act);
      } else if (!seen.has(c.act)) {
        seen.add(c.act);   // opened before this window: a row, no branch
        rows.push({ type: 'close', id: c.act, lane: null, kind: c.kind, closer, ended, earlier: true });
      }
    }
  }
  rows.forEach((r, i) => { r.y = (i + 1) * ROW; });
  const openAt = new Map(rows.filter((r) => r.type === 'open').map((r) => [r.act.id, r]));
  const closes = rows.filter((r) => r.type === 'close');
  const closeAt = new Map(closes.map((r) => [r.id, r]));
  const bottom = (rows.length + 1) * ROW;
  return { turns, rows, openAt, closeAt, maxLanes, bottom, leftOpen,
    addressed: closes.filter((r) => !r.ended).length,
    ended: closes.filter((r) => r.ended).length,
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
      const cls = c.ended ? 'ended' : 'realised';
      const end = c.y - ROW / 2;
      out.push(`<path class="lane ${cls}" d="M${TRUNK},${o.y - ROW / 2} C${TRUNK},${o.y} ${x},${o.y - ROW / 2} ${x},${o.y} V${end}"/>`);
      out.push(`<path class="lane ${cls}" d="M${x},${end} C${x},${c.y} ${TRUNK},${c.y - ROW / 2} ${TRUNK},${c.y}"/>`);
    }
    const text = (y, body, cls = '') => `<text class="${cls}" x="${L.textX}" y="${y + 5}">${body}</text>`;
    let mission = null;
    for (const r of L.rows) {
      if (r.type === 'turn') {
        const t = r.turn;
        // the mission only where it changes, not on every row
        const m = t.mission_id && t.mission_id !== mission ? ' · ' + esc(t.mission_id) : '';
        mission = t.mission_id || mission;
        out.push(`<a class="ln" data-href="turn:${esc(t.id)}"><title>${esc(t.id)}</title><circle class="turn" cx="${TRUNK}" cy="${r.y}" r="${R}"/>${text(r.y, `<tspan class="dim">${esc(hhmm(t.created_at))} turn ${esc(short(t.id))}</tspan>${m}`, 'turnlabel')}</a>`);
      } else if (r.type === 'open') {
        const a = r.act;
        out.push(`<circle class="intent" cx="${laneX(r.lane)}" cy="${r.y}" r="${R - 1}"><title>${esc(a.id)}</title></circle>`);
        out.push(text(r.y, `<tspan class="kind">${esc(a.kind)}</tspan> ${esc(a.about || short(a.id))} <tspan class="dim">(${esc(who(a.author))})</tspan>`));
      } else {
        const c = r.closer;
        const what = r.ended ? `× ${esc(c.kind)}` : `addressed${c ? ': ' + esc(c.kind) : ''}`;
        const by = c ? ` <tspan class="dim">by ${esc(who(c.author))}${c.about ? ' · ' + esc(c.about) : ''}</tspan>` : '';
        const earlier = r.earlier ? ` <tspan class="dim">(an earlier ${esc(r.kind || 'item')}, opened before these turns)</tspan>` : '';
        if (r.earlier) out.push(`<circle class="intent" cx="${TRUNK + LANE}" cy="${r.y}" r="${R - 1}"><title>${esc(r.id)}</title></circle>`);
        out.push(text(r.y, `<tspan class="${r.ended ? 'endedlabel' : 'realisedlabel'}">${what}</tspan>${by}${earlier}`));
      }
    }
    // as wide as the longest label (about 0.62em a character), so a panel
    // scrolls sideways only when a label really is that long
    const longest = Math.max(0, ...out.filter((o) => o.includes('<text')).map((o) => o.replace(/<title>.*?<\/title>/g, '').replace(/<[^>]+>/g, '').replace(/&[a-z]+;/g, 'x').length));
    const width = Math.ceil(L.textX + longest * FONT * 0.62 + 20);
    const svg = `<svg class="intents" width="${width}" height="${L.bottom + 4}" style="font-size:${FONT}px;max-width:none">
${out.join('\n')}
</svg>`;
    const summary = `${L.addressed} addressed · ${L.ended} withdrawn · ${L.turns.length} turns · ${L.leftOpen} left open, not drawn`;
    return { title: 'intent graph', html: `<div data-ma-map><h1>Intent graph <span class="dim">${esc(data.session || '?')}</span></h1>
<p>${link('strand:' + (data.session || ''), 'Circus monitor')} · ${link('herd:', '馬 herd')}</p>
<p class="dim">oldest at the top · each item branches off at the turn that said it and joins back where it was addressed · × withdrawn · items left open are not drawn</p>
<p>${esc(summary)}</p>
<div style="overflow-x:auto">${svg}</div></div>` };
  }
  return { render };
};

module.exports.layout = layout;
