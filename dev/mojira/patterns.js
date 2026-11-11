// The pattern graph for Mojira (M-mojira, "Patterns" block; Joe 2026-11-11:
// "a graph aggregator not a summation aggregator").
//
// Nodes are patterns (futon3 library .flexiarg files) and the operator turns
// that proposed them (象 turn records). Edges are of two origins, kept apart:
//   authored  - a library header names another pattern (@references, @why,
//               @see-also, @builds-from, @next, ...): the kind is the header;
//   observed  - a turn proposed the pattern (source: embedding, 驴, 驴/m40),
//               with 馬's verdict on that turn when there is one.
// Two patterns proposed in the same turn are neighbours through that turn;
// the turn is kept on the edge, so the page shows the path, not a score.
//
// Pure: no vscode, no fs walking here (pattern-index.js does the reading).
'use strict';

const HEADER = /^@([a-z][a-z0-9-]*)\s*(.*)$/;
// family/name, or family/sub/name; families can be non-Latin (象/...).
const PID = /[\p{L}\p{N}][\p{L}\p{N}-]*(?:\/[\p{L}\p{N}](?:[\p{L}\p{N}.-]*[\p{L}\p{N}])?)+/gu;

// Header fields of one .flexiarg text: {key: raw value}, values continued on
// indented lines until the next header or a blank line.
function headers(text) {
  const out = {};
  let key = null;
  for (const line of String(text).split('\n')) {
    const m = line.match(HEADER);
    if (m) { key = m[1]; out[key] = (out[key] ? out[key] + ' ' : '') + m[2]; continue; }
    if (key && /^\s+\S/.test(line) && !/^\s*[!+]/.test(line)) { out[key] += ' ' + line.trim(); continue; }
    key = null;
    if (/^\s*!/.test(line)) break;   // the body starts; headers are done
  }
  return out;
}

// The four parts and the conclusion, first sentence or so of each.
function parts(text) {
  const out = {};
  let cur = null;
  for (const line of String(text).split('\n')) {
    const top = line.match(/^\s{0,2}! ([a-z-]+):\s*(.*)$/i) || line.match(/^  \+ ([A-Za-z-]+):\s*(.*)$/);
    if (top) { cur = top[1].toUpperCase(); out[cur] = top[2]; continue; }
    if (cur && /^\s{3,}\S/.test(line) && !/^\s*\+ /.test(line)) out[cur] += ' ' + line.trim();
    else if (/^\s*\+ /.test(line)) cur = null;
  }
  for (const k of Object.keys(out)) out[k] = out[k].replace(/\s+/g, ' ').trim();
  return out;
}

const SKIP = new Set(['flexiarg', 'arg', 'title', 'tone', 'audience', 'style', 'sigils', 'keywords', 'futon',
  'provenance', 'number', 'status', 'hex', 'hamming-weight', 'bits', 'binary', 'trigrams']);

// One library pattern: {id, family, title, status, parts, links: [{kind, to}]}.
// Only tokens naming a pattern that exists (KNOWN) become links.
function pattern(id, text, known) {
  const h = headers(text);
  const links = [];
  for (const [kind, v] of Object.entries(h)) {
    if (SKIP.has(kind)) continue;
    for (const to of new Set(v.match(PID) || [])) if (to !== id && known.has(to)) links.push({ kind, to });
  }
  return { id, family: id.split('/')[0], title: (h.title || '').trim(), status: (h.status || '').trim(),
    parts: parts(text), links };
}

// The patterns proposed to a turn record, each with the producers that
// proposed it. Follows futon3c xiang/ma_occurrence.clj patterns-of: the
// render's ranked ids (or its patterns), embedding_ranked, m40_ranked and
// lu_ranked, top 3 of each source.
function proposals(record) {
  const ids = (xs) => (xs || []).map((x) => (x && x.id != null ? String(x.id) : String(x))).map((s) => s.replace(/^~/, '')).filter(Boolean);
  const rendered = [];
  for (const e of record.proposed_patterns || []) rendered.push(...(e.ranked && e.ranked.length ? ids(e.ranked) : ids(e.patterns)));
  const groups = [
    ['embedding', [...new Set(rendered.concat(ids(record.embedding_ranked)))]],
    ['驴/m40', ids((record.m40_ranked || {}).ranked)],
    ['驴', ids((record.lu_ranked || {}).ranked)],
  ];
  const by = new Map();
  for (const [src, xs] of groups) for (const id of xs.slice(0, 3)) {
    if (!by.has(id)) by.set(id, []);
    if (!by.get(id).includes(src)) by.get(id).push(src);
  }
  return [...by].map(([id, sources]) => ({ id, sources }));
}

// 馬's verdict per pattern for this record: {pattern: {status, why: [..]}}.
function verdicts(record) {
  const out = {};
  const pv = record.pattern_verdicts;
  for (const p of (pv && pv.patterns) || []) {
    const why = [...new Set(Object.values(p.chain || {}).map((m) => m && m.why).filter(Boolean))];
    out[p.pattern] = { status: p.status, why, reader: pv.reader };
  }
  return out;
}

// What the index keeps of one turn record (small, so the cache stays small).
function turnSummary(id, record) {
  const props = proposals(record);
  if (!props.length) return null;
  return {
    id, turn: record.turn_id || null, session: record.session_id || null, agent: record.agent_id || null,
    at: record.created_at || null,
    said: String(record.source_text || '').replace(/\s+/g, ' ').slice(0, 240),
    proposals: props, verdicts: verdicts(record),
  };
}

// The graph over LIBRARY (Map id -> pattern) and TURNS (array of summaries).
function build(library, turns) {
  const out = new Map(), into = new Map(), byPat = new Map(), co = new Map();
  const add = (m, k, v) => { if (!m.has(k)) m.set(k, []); m.get(k).push(v); };
  for (const p of library.values()) for (const l of p.links) { add(out, p.id, l); add(into, l.to, { kind: l.kind, from: p.id }); }
  for (const t of turns) {
    const ids = t.proposals.map((x) => x.id);
    for (const x of t.proposals) add(byPat, x.id, { turn: t.id, sources: x.sources, verdict: t.verdicts[x.id] || null });
    for (const a of ids) for (const b of ids) if (a !== b) {
      const k = a + '\u0000' + b;
      if (!co.has(k)) co.set(k, []);
      co.get(k).push(t.id);
    }
  }
  const coOf = new Map();
  for (const [k, ts] of co) { const [a, b] = k.split('\u0000'); add(coOf, a, { with: b, turns: ts }); }

  // Connected components of the authored graph. Observed co-proposal is
  // kept out: a turn proposes ~4.5 patterns, and with those pairs as edges
  // nearly every proposed pattern lands in one component, which says
  // nothing. Where use and authoring disagree is shown per pattern instead.
  const parent = new Map();
  const find = (x) => { while (parent.get(x) !== x) { parent.set(x, parent.get(parent.get(x))); x = parent.get(x); } return x; };
  const node = (x) => { if (!parent.has(x)) parent.set(x, x); };
  const union = (a, b) => { node(a); node(b); const ra = find(a), rb = find(b); if (ra !== rb) parent.set(ra, rb); };
  for (const id of library.keys()) node(id);
  for (const id of byPat.keys()) node(id);
  let authored = 0, observed = 0;
  for (const [a, ls] of out) for (const l of ls) { union(a, l.to); authored++; }
  for (const k of co.keys()) { const [a, b] = k.split('\u0000'); if (a < b) observed++; }
  const comps = new Map();
  for (const x of parent.keys()) add(comps, find(x), x);
  const components = [...comps.values()].sort((a, b) => b.length - a.length);
  const compOf = new Map();
  components.forEach((c, i) => c.forEach((x) => compOf.set(x, i)));

  return { library, turns: new Map(turns.map((t) => [t.id, t])), out, into, byPat, coOf, components, compOf,
    edges: { authored, observed } };
}

module.exports = { headers, parts, pattern, proposals, verdicts, turnSummary, build };
