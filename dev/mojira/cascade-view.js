// Mojira's picture of one per-turn pattern cascade (M-repl-circus, first
// draft for Joe to react to). Layout after Moran 1971 figure 1: patterns
// above, the situation elements their parts map to below, wired by curves,
// general to specific. Complete occurrences are filled circles; the glue is
// the element two of them share, drawn between them. Partials (3/4, 2/4) are
// hollow circles on a lower tier, shown on request, wired by dotted curves.
//
// Every node is an <a class="ln"> inside the SVG, so j/k, f-hints, Tab and
// the right column work as on any text page. Pure: data in, html out.
'use strict';
const { ROLES, PART_OF } = require('./cascade-reader');

const STEP = 124, Y_OCC = 78, Y_EL = 210, Y_PART = 360, Y_PEL = 480, R = 16;

function short(id) { return String(id).split('/').pop(); }

// Kebab names broken at hyphens into lines of about WIDTH characters.
function wrap(name, width = 14, max = 3) {
  const words = String(name).split('-'), lines = [];
  let cur = '';
  for (const w of words) {
    if (cur && (cur + '-' + w).length > width) { lines.push(cur + '-'); cur = w; }
    else cur = cur ? cur + '-' + w : w;
  }
  if (cur) lines.push(cur);
  if (lines.length > max) { lines.length = max; lines[max - 1] = lines[max - 1].replace(/-?$/, '…'); }
  return lines;
}

// Spread xs (sorted by wish) so neighbours are at least GAP apart.
function spread(items, gap) {
  items.sort((a, b) => a.x - b.x);
  for (let i = 1; i < items.length; i++) if (items[i].x < items[i - 1].x + gap) items[i].x = items[i - 1].x + gap;
  return items;
}

function layout(c, withPartials) {
  const occ = new Map(c.occurrences.map((o) => [o.pattern, o]));
  const complete = c.complete.map((p) => occ.get(p));
  const glued = new Set(c.glue.map((g) => g.element));
  // Element order: each occurrence's own elements, then the ones it shares
  // with the next, so a glue sits between its owners.
  const order = [];
  const placed = new Set();
  const put = (e) => { if (!placed.has(e)) { placed.add(e); order.push(e); } };
  complete.forEach((o, i) => {
    const mine = ROLES.map((r) => o.maps[r].element).filter(Boolean);
    mine.filter((e) => !glued.has(e)).forEach(put);
    const next = complete[i + 1];
    const nextEls = next ? new Set(ROLES.map((r) => next.maps[r].element)) : new Set();
    mine.filter((e) => glued.has(e) && nextEls.has(e)).forEach(put);
    mine.forEach(put);
  });
  const X0 = 70;
  const el = new Map(order.map((e, i) => [e, { id: e, x: X0 + i * STEP, y: Y_EL, glue: glued.has(e), tier: 1 }]));
  const nodes = complete.map((o) => {
    const xs = ROLES.map((r) => o.maps[r].element).filter(Boolean).map((e) => el.get(e).x);
    return { id: o.pattern, x: xs.reduce((a, b) => a + b, 0) / xs.length, y: Y_OCC, n: o.n, complete: true };
  });
  spread(nodes, 150);
  const parts = [], pels = new Map();
  if (withPartials) {
    for (const p of c.partial.map((id) => occ.get(id))) {
      const xs = ROLES.map((r) => p.maps[r].element).filter((e) => e && el.has(e)).map((e) => el.get(e).x);
      parts.push({ id: p.pattern, x: xs.length ? xs.reduce((a, b) => a + b, 0) / xs.length : X0, y: Y_PART, n: p.n, complete: false });
    }
    spread(parts, STEP);
    for (const p of parts) for (const r of ROLES) {
      const e = occ.get(p.id).maps[r].element;
      if (e && !el.has(e) && !pels.has(e)) pels.set(e, { id: e, x: p.x, y: Y_PEL, glue: false, tier: 3 });
    }
    spread([...pels.values()], STEP);
  }
  const allEls = new Map([...el, ...pels]);
  const xs = [...nodes, ...parts, ...allEls.values()].map((n) => n.x);
  const width = Math.max(...xs) + 90;
  const height = (withPartials ? Y_PEL + 80 : Y_EL + 80);
  const edges = [];
  for (const n of nodes.concat(parts)) {
    const o = occ.get(n.id);
    for (const r of ROLES) {
      const e = o.maps[r].element;
      if (e && allEls.has(e)) edges.push({ from: n, to: allEls.get(e), role: r, dotted: !n.complete });
    }
  }
  return { nodes, parts, els: [...allEls.values()], edges, width, height };
}

function svg(c, withPartials, esc) {
  const L = layout(c, withPartials);
  const text = (x, y, lines, cls, anchor = 'middle') => `<text class="${cls}" x="${x}" y="${y}" text-anchor="${anchor}">${lines.map((l, i) => `<tspan x="${x}" dy="${i ? '1.15em' : 0}">${esc(l)}</tspan>`).join('')}</text>`;
  const edge = (e) => {
    const a = e.from, b = e.to;
    const down = b.y > a.y;
    const y0 = down ? a.y + R : a.y - R;
    const y1 = down ? b.y - 7 : b.y + 7 + 3 * 13;   // up-going edges end below the element's label
    const my = (y0 + y1) / 2;
    const t = 0.62, u = 1 - t;   // label position on the cubic
    const lx = u * u * u * a.x + 3 * u * u * t * a.x + 3 * u * t * t * b.x + t * t * t * b.x;
    const ly = u * u * u * y0 + 3 * u * u * t * my + 3 * u * t * t * my + t * t * t * y1;
    return `<path class="edge${e.dotted ? ' dotted' : ''}" d="M${a.x},${y0} C${a.x},${my} ${b.x},${my} ${b.x},${y1}"/>`
      + (e.dotted ? '' : `<text class="role" x="${lx}" y="${ly}" text-anchor="middle">${PART_OF[e.role]}</text>`);
  };
  const occNode = (n) => `<a class="ln" data-href="cn:${esc(n.id)}"><title>${esc(n.id)} (${n.n}/4)</title>
<circle class="${n.complete ? 'occ' : 'occ partial'}" cx="${n.x}" cy="${n.y}" r="${R}"/>
${text(n.x, n.complete ? n.y - R - 30 : n.y + R + 14, wrap(short(n.id), n.complete ? 22 : 14, n.complete ? 2 : 3), 'olabel')}
<text class="nlabel" x="${n.x}" y="${n.y + 4}" text-anchor="middle">${n.n}/4</text></a>`;
  const elNode = (e) => `<a class="ln" data-href="ce:${esc(e.id)}"><title>${esc(e.id)}</title>
${e.glue ? `<circle class="glue" cx="${e.x}" cy="${e.y}" r="9"/>` : `<rect class="el${e.tier === 3 ? ' partial' : ''}" x="${e.x - 6}" y="${e.y - 6}" width="12" height="12"/>`}
${text(e.x, e.y + 22, wrap(e.id), 'elabel')}
${e.glue ? `<text class="gluetag" x="${e.x + 13}" y="${e.y + 4}" text-anchor="start">glue</text>` : ''}</a>`;
  return `<svg class="cascade" viewBox="0 0 ${L.width} ${L.height}" width="${L.width}" style="max-width:100%;height:auto">
${L.edges.map(edge).join('\n')}
${L.nodes.map(occNode).join('\n')}
${L.els.filter((e) => e.tier === 1).map(elNode).join('\n')}
${L.parts.map(occNode).join('\n')}
${L.els.filter((e) => e.tier === 3).map(elNode).join('\n')}
</svg>`;
}

// ------------------------------------------------------------ minimal view
// Joe (2026-11-11): "something like a 2 or 3 line form showing the two
// patterns and their glue". One circle per complete occurrence, and a line
// between two occurrences for each glue, labelled with the parts it joins
// and the element. Says what cascade_sexp.py's minimal form says, no more.

// Pattern order of the parts (cascade_sexp.py PARTS).
const SEXP_PARTS = [['IF', 'I1'], ['HOWEVER', 'I2'], ['THEN', 'B'], ['BECAUSE', 'G']];

function minimalLayout(c) {
  const occ = new Map(c.occurrences.map((o) => [o.pattern, o]));
  const GAP = 300, X0 = 110, Y = 70;
  const nodes = c.complete.map((p, i) => ({ id: p, x: X0 + i * GAP, y: Y, n: occ.get(p).n }));
  const at = new Map(nodes.map((n) => [n.id, n]));
  const edges = [];
  for (const g of c.glue) for (let i = 0; i + 1 < g.ends.length; i++) {
    const [[pa, ra], [pb, rb]] = [g.ends[i], g.ends[i + 1]];
    edges.push({ element: g.element, from: at.get(pa), to: at.get(pb), fromPart: PART_OF[ra], toPart: PART_OF[rb] });
  }
  return { nodes, edges, width: X0 * 2 + (nodes.length - 1) * GAP, height: 150 };
}

// DOM order is the j/k order: each pattern, then the glue it leads into.
function minimalSvg(c, esc) {
  const L = minimalLayout(c);
  const node = (n) => `<a class="ln" data-href="cn:${esc(n.id)}"><title>${esc(n.id)}</title>
<circle class="occ" cx="${n.x}" cy="${n.y}" r="${R}"/>
<text class="olabel" x="${n.x}" y="${n.y + R + 18}" text-anchor="middle">${wrap(short(n.id), 26, 2).map((l, i) => `<tspan x="${n.x}" dy="${i ? '1.15em' : 0}">${esc(l)}</tspan>`).join('')}</text></a>`;
  const edge = (e, k) => {
    const a = e.from, b = e.to, lift = 36 + 18 * k;
    const mx = (a.x + b.x) / 2, my = a.y - lift;
    return `<a class="ln" data-href="ce:${esc(e.element)}"><title>glue: ${esc(e.element)}</title>
<path class="glue-edge" d="M${a.x + R},${a.y} Q${mx},${my - lift * 0.4} ${b.x - R},${b.y}"/>
<circle class="glue" cx="${mx}" cy="${my + 4}" r="6"/>
<text class="gluelabel" x="${mx}" y="${my - 8}" text-anchor="middle">${esc(e.element)}</text>
<text class="role" x="${a.x + R + 6}" y="${a.y - 10}" text-anchor="start">${e.fromPart}</text>
<text class="role" x="${b.x - R - 6}" y="${b.y - 10}" text-anchor="end">${e.toPart}</text></a>`;
  };
  return `<svg class="cascade" viewBox="0 0 ${L.width} ${L.height}" width="${L.width}" style="max-width:100%;height:auto">
${L.nodes.map((n) => node(n) + L.edges.map((e, k) => (e.from === n ? edge(e, k) : '')).join('')).join('\n')}
</svg>`;
}

// The minimal s-expression, as cascade_sexp.py writes it (without its
// mode line).
function minimalSexp(c) {
  const occ = new Map(c.occurrences.map((o) => [o.pattern, o]));
  const holders = new Map();
  for (const p of c.complete) for (const [, r] of SEXP_PARTS) {
    const e = occ.get(p).maps[r].element;
    if (!holders.has(e)) holders.set(e, new Set());
    holders.get(e).add(p);
  }
  const labels = new Map();
  const q = (s) => '"' + String(s == null ? '' : s).replace(/\\/g, '\\\\').replace(/"/g, '\\"') + '"';
  const lines = [`(cascade ${q(c.turn.evidence)} ${q(c.turn.at)}`];
  for (const p of c.complete) {
    const glued = [];
    for (const [part, r] of SEXP_PARTS) {
      const e = occ.get(p).maps[r].element;
      if (holders.get(e).size < 2) continue;
      if (labels.has(e)) glued.push(`${part} #${labels.get(e)}#`);
      else { labels.set(e, labels.size + 1); glued.push(`${part} #${labels.get(e)}=${e}`); }
    }
    lines.push(`  (${p}${glued.length ? ' ' + glued.join(' ') : ''})`);
  }
  lines[lines.length - 1] += ')';
  return lines.join('\n');
}

// The three views, in the order the cycle key visits them.
const VIEWS = ['cascade', 'cascade:detail', 'cascade:partials'];
function nextView(href) { return VIEWS[(VIEWS.indexOf(href) + 1) % VIEWS.length]; }

module.exports = function views(c, { esc, link, trim, partsOf }) {
  const occ = new Map(c.occurrences.map((o) => [o.pattern, o]));
  const glueBy = new Map(c.glue.map((g) => [g.element, g]));

  function page(view) {
    const withPartials = view === 'partials';
    const href = view === 'partials' ? 'cascade:partials' : view === 'detail' ? 'cascade:detail' : 'cascade';
    const n3 = c.partial.filter((p) => occ.get(p).n === 3).length, n2 = c.partial.length - n3;
    const status = [`formally legal: ${c.complete.length} occurrence${c.complete.length === 1 ? '' : 's'}, ${c.glue.length} glue`,
      c.notes.validator, c.notes.review].filter(Boolean).join('; ');
    return { title: 'cascade', html: `<h1>A turn as a pattern cascade <span class="dim">${esc((c.turn.at || '').slice(0, 16).replace('T', ' '))}</span></h1>
<p class="status-line">${esc(status)}</p>
<div data-cycle="${esc(nextView(href))}"></div>
${view === 'minimal' ? minimalSvg(c, esc) : svg(c, withPartials, esc)}
<p class="dim">${view === 'minimal' ? 'minimal view' : withPartials ? `detailed view, with the partial occurrences (3/4: ${n3}, 2/4: ${n2})` : 'detailed view'} · <b>c</b> cycles minimal → detailed → with partials</p>
${view === 'minimal' ? `<pre class="sexp">${esc(minimalSexp(c))}</pre>`
    : `<p class="dim">filled: complete (4/4) · hollow: partial · ● glue: an element two occurrences share · voids are not drawn</p>`}
<h2>The turn</h2><p class="dim">evidence ${esc(c.turn.evidence || '?')}</p><p>${esc(trim(c.turn.text, 900))}</p>
<p class="dim">read from ${esc(c.source || '')}</p>` };
  }

  function pattern(id) {
    const o = occ.get(id);
    if (!o) return { title: id, html: `<h1>${esc(id)}</h1><p class="err">Not a candidate in this cascade.</p>` };
    const parts = partsOf(id) || {};
    return { title: short(id), html: `<h1>${esc(short(id))} <span class="dim">${o.n}/4 · ${o.n === 4 ? 'complete' : 'partial'}</span></h1>
<p class="dim">${link('pat:' + id, id)}${o.citedBy ? ' · mapped by ' + esc(o.citedBy) : ''}</p>
${ROLES.map((r) => {
      const m = o.maps[r];
      return `<h2 class="part">${PART_OF[r]} <span class="dim">${r}</span></h2>
<p class="dim">${esc(trim(parts[PART_OF[r]] || '(part text not found)', 300))}</p>
${m.element ? `<p>→ ${link('ce:' + m.element, m.element)}${glueBy.has(m.element) ? ' <span class="gluetag">glue</span>' : ''}</p><blockquote>${esc(m.quote)}</blockquote>`
        : `<p class="dim">void: ${esc(m.void)}</p>`}`;
    }).join('\n')}` };
  }

  function element(id) {
    const uses = [];
    for (const o of c.occurrences) for (const r of ROLES) if (o.maps[r].element === id) uses.push([o, r]);
    const g = glueBy.get(id);
    return { title: id, html: `<h1>${esc(id)}</h1>
<p class="dim">a situation element of the turn${g ? ` · <span class="gluetag">glue</span> between ${g.ends.map(([p, r]) => esc(short(p)) + ' ' + PART_OF[r]).join(' and ')}` : ''}</p>
${g ? '<p class="dim">The witness for this glue is shape-checked only (claude-13\'s review): it attests the shared name, not that the two squares agree here.</p>' : ''}
<h2>Parts that map to it</h2><ul>${uses.map(([o, r]) => `<li>${link('cn:' + o.pattern, short(o.pattern))} <span class="dim">${PART_OF[r]} · ${o.n}/4</span><blockquote>${esc(o.maps[r].quote)}</blockquote></li>`).join('')}</ul>` };
  }

  return { page, pattern, element };
};

module.exports.layout = layout;
module.exports.minimalLayout = minimalLayout;
module.exports.minimalSexp = minimalSexp;
module.exports.VIEWS = VIEWS;
module.exports.nextView = nextView;
module.exports.wrap = wrap;
