// One session's accumulating Circus graph (futon3c CL-66).
// Pure: route JSON + shared circus count formatter -> {title, html}.
'use strict';

const KINDS = ['strand', 'mention', 'focus', 'proposed', 'occurs', 'partial', 'occurrence', 'quotes'];

function merge(current, delta, refresh) {
  const base = current || { turns: [], edges: [] };
  const incoming = [...((delta && delta.turns) || []), ...((refresh && refresh.turns) || [])];
  const replacements = new Map(incoming.map((t) => [t.id, t]));
  const newIds = new Set(((delta && delta.turns) || []).map((t) => t.id));
  const oldIds = new Set((base.turns || []).map((t) => t.id));
  const prepended = ((delta && delta.turns) || [])
    .filter((t) => !oldIds.has(t.id))
    .map((t) => replacements.get(t.id));
  const retained = (base.turns || []).map((t) => replacements.get(t.id) || t);
  const turns = [...prepended, ...retained.filter((t) => !newIds.has(t.id))];
  const edgeMap = new Map();
  const replacedSources = new Set(incoming.map((t) => t.id));
  const oldEdges = (base.edges || []).filter((e) => !replacedSources.has(e.from));
  for (const e of [...oldEdges, ...((delta && delta.edges) || []), ...((refresh && refresh.edges) || [])]) {
    edgeMap.set([e.from, e.to, e.kind, e.pattern || '', e.part || ''].join('\0'), e);
  }
  return {
    ...base, ...(delta || {}),
    head: (delta && delta.head) || base.head,
    cursor: (delta && delta.cursor) || base.cursor,
    turns, edges: [...edgeMap.values()],
  };
}

function tally(data) {
  const edges = Object.fromEntries(KINDS.map((k) => [k, 0]));
  for (const e of data.edges || []) if (Object.hasOwn(edges, e.kind)) edges[e.kind]++;
  return {
    turns: (data.turns || []).length,
    edges,
    items: new Set((data.turns || []).flatMap((t) => t.items || [])).size,
    sessionLinks: edges.strand,
  };
}

function countInput(gcc) {
  return Object.fromEntries(Object.entries(gcc || {}).map(([animal, r]) =>
    [animal, r && r.status === 'present' ? Number(r.count || 0) : 0]));
}

function pendingAge(createdAt, now) {
  const ms = Date.parse(createdAt || '');
  if (!Number.isFinite(ms)) return '?';
  const seconds = Math.max(0, Math.floor((now - ms) / 1000));
  if (seconds < 60) return seconds + 's';
  if (seconds < 3600) return Math.floor(seconds / 60) + 'm';
  return Math.floor(seconds / 3600) + 'h';
}

function connectionLabel(t, now = Date.now()) {
  const c = t.ma_connection || {};
  if (c.status === 'linked') return `linked ← ${c['turn/id'] || t.prev_operator_turn_id || '?'}`;
  if (c.status === 'session-start') return 'session start';
  if (c.status === 'prev-absent') return `previous absent: ${c.reason || 'unknown'}`;
  if (c.status === 'prev-pending') return `previous pending · ${pendingAge(t.created_at, now)} old`;
  return `${c.status || 'connection missing'} · ${pendingAge(t.created_at, now)} old`;
}

module.exports = function strandView({ esc, link, counts, now = () => Date.now() }) {
  function connection(t) {
    const c = t.ma_connection || {};
    if (c.status === 'linked') {
      const prev = c['turn/id'] || t.prev_operator_turn_id || '?';
      return `<span class="dim">馬 ← ${link('pt:' + prev, String(prev).slice(-8))}</span>`;
    }
    const label = connectionLabel(t, now());
    if (c.status === 'session-start') return `<span class="dim">馬 ${esc(label)}</span>`;
    return `<span class="absent pending">馬 ${esc(label)}</span>`;
  }

  function row(t) {
    return `<div class="gs-r strand-turn" data-turn="${esc(t.id)}">${link('pt:' + t.id, t.id)} ${link('step:' + t.id, '象')} <span class="dim">${esc(String(t.created_at || '').slice(0, 16).replace('T', ' '))}</span> ${connection(t)} ${counts(countInput(t.gcc))}${(t.items || []).length ? ' · ' + t.items.map((x) => link('gi:' + x, x)).join(' ') : ''}</div>`;
  }

  function summary(data) {
    const ts = tally(data);
    const edgeText = KINDS.map((k) => `${esc(k)} ${esc(ts.edges[k])}`).join(' · ');
    return `${esc(ts.turns)} turns · ${edgeText} · ${esc(ts.items)} distinct items · ${esc(ts.sessionLinks)} session links · newest first`;
  }

  function render(data) {
    const rows = (data.turns || []).map(row).join('\n');
    return { title: 'Circus monitor', html: `<div class="gcc circus-strand">
<h1>Circus monitor <span class="dim">${esc(data.session || '?')}</span></h1>
<p>${link('mamap:' + (data.session || ''), '馬 map')}</p>
<p class="dim" data-strand-summary>${summary(data)}</p>
<div data-strand-turns>${rows || '<div class="gs-r absent">no operator turns in this session</div>'}</div></div>` };
  }

  return { render, connection, row, summary };
};

module.exports.merge = merge;
module.exports.tally = tally;
module.exports.countInput = countInput;
module.exports.connectionLabel = connectionLabel;
