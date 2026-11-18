// Mojira's map of itself (mojira://map; Joe 2026-11-18: "a 'cascade' style
// site-map of its own referents ... a graphical table of contents of what's
// actually addressable"). Nothing is stored: each time the map is opened,
// crawl() renders pages from home and follows their links (data-href) and
// their c views (data-cycle), so the map is what can be reached now.
//
//   crawl(render, opts) -> {kinds, edges, external, broken, truncated}
//   view(map, helpers)  -> {title, html}: kinds of page as circles in rows by
//                          link distance from home, curves for the links
//                          between kinds, the outside things (files, URLs,
//                          turns in the stepper) underneath, and hollow
//                          circles for kinds Mojira can draw but no link reaches.
//   kindPage(map, kind, helpers) -> the pages of one kind (mojira://map/<kind>)
//
// KINDS is every kind of page core.js's render() draws; a test checks that
// render() and KINDS name the same kinds, so a new page can't be missing here.
'use strict';

const KINDS = {
  home: 'Mojira home',
  e: 'lexicon entities', p: 'pending items', status: 'entities by status', label: 'pending by state',
  all: 'all pending', search: 'searches', triage: 'triage', ti: 'triage items',
  pg: 'pattern graph', pc: 'pattern components', pf: 'pattern families', pat: 'patterns',
  pt: 'turns', pturns: 'recent turns', pout: 'outside the main component', pmiss: 'missing patterns',
  cascade: 'turn cascade', cn: 'cascade occurrences', ce: 'cascade elements',
  top: 'top items', 'top-raw': 'top items raw', wn: 'why-not', 'wn-raw': 'why-not raw',
  strand: 'Circus strand', mamap: '馬 thread map',
  gcc: 'GCC item list', gi: 'GCC items', 'gi-raw': 'GCC items raw', gparty: 'Gauntlet party', 'gparty-raw': 'party raw',
  vd: 'draft variants', vds: 'variant sources', loi: 'letter of intent', lf: 'letter fields',
  codes: 'shortcodes', map: 'this map', mapg: 'this map as a picture',
};
// Reached by a key or a shortcode rather than a link.
const KEYED = { search: '/', codes: ': ?', map: ': M', mapg: 'c on the map' };
// Links that leave Mojira: opened in Emacs, the browser, or the 象 stepper.
const EXTERNAL = { file: 'files (Emacs)', ext: 'web pages', step: 'turns in the 象 stepper' };

const kindOf = (href) => String(href).split(':')[0];
const unesc = (s) => s.replace(/&quot;/g, '"').replace(/&#39;/g, "'").replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&amp;/g, '&');

// The hrefs a page's html links to (links and its c view), each with the
// <h2> heading it sits under, if any: the page's own grouping of its links.
function linksIn(html) {
  const out = [];
  let section = null;
  for (const m of String(html).matchAll(/<h2[^>]*>(.*?)<\/h2>|data-(href|cycle)="([^"]*)"/g)) {
    if (m[1] !== undefined) section = unesc(m[1].replace(/<span class="dim">.*?<\/span>/g, '').replace(/<[^>]+>/g, '')).trim();
    else if (m[3]) out.push({ href: unesc(m[3]), section });
  }
  return out;
}
const hrefsIn = (html) => linksIn(html).map((l) => l.href);

// RENDER(href) -> {html}; LOADED(href) -> false when the page's data is not
// in memory yet (the crawl never waits on Agency or a pattern build).
function crawl(render, { start = 'home', loaded = () => true, maxPerKind = 300, budgetMs = 1500, now = Date.now } = {}) {
  const t0 = now();
  const kinds = new Map(), edges = new Map(), external = new Map(), broken = [], waiting = [];
  const K = (k) => {
    if (!kinds.has(k)) kinds.set(k, { kind: k, pages: new Set(), rendered: 0, depth: Infinity, notLoaded: 0, unexplored: 0 });
    return kinds.get(k);
  };
  const seen = new Set([start]);
  const queue = [[start, 0]];
  let truncated = false;
  K(kindOf(start)).pages.add(start);
  K(kindOf(start)).depth = 0;
  while (queue.length) {
    const [href, depth] = queue.shift();
    const k = K(kindOf(href));
    if (k.rendered >= maxPerKind || now() - t0 > budgetMs) { k.unexplored++; truncated = true; continue; }
    if (!loaded(href)) { k.notLoaded++; waiting.push(href); continue; }
    let html;
    try { html = render(href).html; } catch (e) { broken.push({ from: href, href, why: e.message }); continue; }
    k.rendered++;
    for (const { href: to, section } of linksIn(html)) {
      const tk = kindOf(to);
      if (tk === 'map' || tk === 'mapg') continue;
      const ek = k.kind + '\t' + tk;
      edges.set(ek, (edges.get(ek) || 0) + 1);
      if (EXTERNAL[tk]) {
        if (!external.has(tk)) external.set(tk, new Set());
        external.get(tk).add(to.slice(tk.length + 1));
        continue;
      }
      if (!KINDS[tk]) { broken.push({ from: href, href: to, why: 'no such kind of page' }); continue; }
      const target = K(tk);
      target.pages.add(to);
      // the folder it sits in: where the crawl first found it
      if (depth + 1 < target.depth && tk !== k.kind) { target.parent = k.kind; target.section = section; }
      target.depth = Math.min(target.depth, depth + 1);
      if (!seen.has(to)) { seen.add(to); queue.push([to, depth + 1]); }
    }
  }
  return { kinds, edges, external, broken, waiting, truncated, ms: now() - t0 };
}

// ------------------------------------------------------------------ folders
// The main view (Joe 2026-11-18: "all text must be easy to read"). Each kind
// of page sits in the folder of the kind where the crawl first found it;
// a page that groups its links under headings (home does) gives each
// heading its own folder. Ordinary text, so j/k, Enter, f and y work.
// MAX_CHOICES: no folder offers more than this many lines at once ("no more
// than five nodes from any choice point"); the rest go under "more".
const MAX_CHOICES = 5;

function folders(map) {
  const kids = new Map();
  for (const k of map.kinds.values()) {
    if (!KINDS[k.kind] || !k.parent) continue;
    if (!kids.has(k.parent)) kids.set(k.parent, []);
    kids.get(k.parent).push(k);
  }
  // node: {label, href?, note?, children[]}
  const node = (k) => {
    const children = kids.get(k.kind) || [];
    const sections = [...new Set(children.map((c) => c.section || ''))];
    const leaf = (c) => node(c);
    let ch;
    if (sections.length > 1) {
      ch = sections.map((sec) => {
        const inSec = children.filter((c) => (c.section || '') === sec).map(leaf);
        return inSec.length === 1 ? inSec[0] : { label: sec || 'other', folder: true, children: inSec };
      });
    } else ch = children.map(leaf);
    const more = k.notLoaded || k.unexplored ? '+' : '';
    return { label: KINDS[k.kind], href: 'map:' + k.kind, note: k.pages.size + more + (k.pages.size === 1 && !more ? ' page' : ' pages'), children: ch };
  };
  return node(map.kinds.get('home'));
}

// Split any list longer than MAX_CHOICES into the first MAX_CHOICES - 1 and
// a "more" folder holding the rest (itself split the same way).
function cap(list) {
  if (list.length <= MAX_CHOICES) return list;
  return list.slice(0, MAX_CHOICES - 1).concat([{ label: 'more', folder: true, children: cap(list.slice(MAX_CHOICES - 1)) }]);
}
function capTree(n) { return { ...n, children: cap((n.children || []).map(capTree)) }; }

function folderView(map, { esc, link }) {
  const tree = capTree(folders(map));
  const line = (n) => (n.href ? link(n.href, n.label) : n.folder ? `<span class="gs-tag">${esc(n.label)}/</span>` : esc(n.label)) + (n.note ? ` <span class="dim">${esc(n.note)}</span>` : '');
  const ul = (list) => (list.length ? `<ul class="tree">${list.map((n) => `<li>${line(n)}${ul(n.children || [])}</li>`).join('')}</ul>` : '');
  const outside = [...map.external].map(([k, v]) => ({ label: EXTERNAL[k], href: 'map:' + k, note: v.size + (v.size === 1 ? ' item' : ' items'), children: [] }));
  const unreached = Object.keys(KINDS).filter((id) => !map.kinds.has(id) || map.kinds.get(id).depth === Infinity);
  const keyed = unreached.filter((id) => KEYED[id]).map((id) => ({ label: KINDS[id], note: 'key ' + KEYED[id], children: [] }));
  const missing = unreached.filter((id) => !KEYED[id]).map((id) => ({ label: KINDS[id], href: 'map:' + id, note: map.waiting.length ? 'not reached yet' : 'no link reaches it', children: [] }));
  const block = (title, list) => (list.length ? `<div class="gs-h">${esc(title)}</div>${ul(cap(list))}` : '');
  return { title: 'map', html: `<div class="gcc" data-cycle="mapg">
<h1>Mojira's map <span class="dim">— what can be reached from home, now</span></h1>
<p class="dim">Enter opens a kind's pages · c the picture · y copies a mojira:// id</p>
${map.waiting.length ? `<p class="absent">still loading: ${esc([...new Set(map.waiting.map((h) => KINDS[kindOf(h)]))].join(', '))}. The map redraws when they arrive.</p>` : ''}
${map.broken.length ? `<p class="err">${link('map:broken', map.broken.length + ' broken links')}</p>` : ''}
${ul([tree].map((n) => ({ ...n, children: n.children })))}
${block('OUTSIDE MOJIRA', outside)}${block('BY KEY', keyed)}${block('NOT REACHED', missing)}</div>` };
}

// ------------------------------------------------------------------ picture

const STEP = 118, ROW = 165, TOP = 40;
const radius = (n) => 7 + 4 * Math.log10(1 + n);

function wrap(s, width = 16) {
  const out = [];
  let cur = '';
  for (const w of String(s).split(' ')) {
    if (cur && (cur + ' ' + w).length > width) { out.push(cur); cur = w; } else cur = cur ? cur + ' ' + w : w;
  }
  if (cur) out.push(cur);
  return out.slice(0, 3);
}

function layout(map) {
  const rows = [];
  for (const k of map.kinds.values()) {
    if (!KINDS[k.kind] || k.depth === Infinity) continue;
    (rows[k.depth] = rows[k.depth] || []).push({ id: k.kind, label: KINDS[k.kind], n: k.pages.size, k });
  }
  const reached = rows.filter(Boolean);
  const unreached = Object.keys(KINDS).filter((id) => !map.kinds.has(id) || map.kinds.get(id).depth === Infinity)
    .map((id) => ({ id, label: KINDS[id], n: 0, hollow: true, key: KEYED[id] }));
  const outside = [...map.external].map(([id, s]) => ({ id, label: EXTERNAL[id], n: s.size, outside: true }));
  if (map.broken.length) outside.push({ id: 'broken', label: 'broken links', n: map.broken.length, bad: true });
  const all = [...reached, outside, unreached].filter((r) => r.length);
  const width = Math.max(...all.map((r) => r.length)) * STEP + STEP;
  const nodes = new Map();
  all.forEach((row, i) => row.forEach((n, j) => {
    n.x = Math.round((width - row.length * STEP) / 2 + STEP * (j + 0.5));
    n.y = TOP + i * ROW;
    n.r = radius(n.n);
    nodes.set(n.id, n);
  }));
  return { nodes, width, height: TOP + all.length * ROW, rows: all.length };
}

function view(map, { esc, link }) {
  const L = layout(map);
  const N = L.nodes;
  const curves = [];
  for (const [ek, count] of map.edges) {
    const [a, b] = ek.split('\t').map((id) => N.get(id));
    if (!a || !b || a === b) continue;
    const w = (1 + Math.log2(count) / 2).toFixed(1);
    const y0 = a.y + (b.y >= a.y ? a.r : -a.r), y1 = b.y + (b.y > a.y ? -b.r : b.r);
    const my = a.y === b.y ? a.y - 46 : (y0 + y1) / 2;
    curves.push(`<path class="edge" style="stroke-width:${w};opacity:.55" d="M${a.x},${a.y === b.y ? a.y - a.r : y0} C${a.x},${my} ${b.x},${my} ${b.x},${a.y === b.y ? b.y - b.r : y1}"><title>${esc(a.label)} → ${esc(b.label)}: ${count} link${count === 1 ? '' : 's'}</title></path>`);
  }
  const node = (n) => {
    const k = n.k;
    const cls = n.bad ? 'occ bad' : n.hollow ? 'occ partial' : n.outside ? 'el' : 'occ';
    const shape = n.outside && !n.bad
      ? `<rect class="${cls}" x="${n.x - n.r}" y="${n.y - n.r}" width="${2 * n.r}" height="${2 * n.r}"/>`
      : `<circle class="${cls}" cx="${n.x}" cy="${n.y}" r="${n.r}"/>`;
    const more = k && (k.notLoaded || k.unexplored) ? '+' : '';
    const count = n.hollow ? (n.key ? 'key ' + n.key : map.waiting.length ? 'not reached yet' : 'no link') : n.n + more;
    const lines = wrap(n.label).map((l, i) => `<tspan x="${n.x}" dy="${i ? 13 : 0}">${esc(l)}</tspan>`).join('');
    const tip = n.hollow ? `${n.label}: Mojira can draw it, no link reaches it${n.key ? ' (key ' + n.key + ')' : ''}${map.waiting.length && !n.key ? '; ' + map.waiting.length + ' pages are still loading and may link to it' : ''}`
      : n.bad ? `${n.n} links to pages Mojira cannot draw`
      : `${n.label}: ${n.n} page${n.n === 1 ? '' : 's'}${k && k.notLoaded ? ', some not loaded yet' : ''}${k && k.unexplored ? ', more not followed' : ''}`;
    return `<a class="ln" data-href="map:${esc(n.id)}"><title>${esc(tip)}</title>${shape}
<text class="olabel" x="${n.x}" y="${n.y + n.r + 14}" text-anchor="middle">${lines}</text>
<text class="elabel" x="${n.x}" y="${n.y + n.r + 14 + 13 * wrap(n.label).length}" text-anchor="middle">${esc(count)}</text></a>`;
  };
  const pages = [...map.kinds.values()].reduce((s, k) => s + k.pages.size, 0);
  const outsideN = [...map.external.values()].reduce((s, v) => s + v.size, 0);
  const hollow = [...N.values()].filter((n) => n.hollow).length;
  return { title: 'map (picture)', html: `<div class="gcc" data-cycle="map">
<h1>Mojira's map <span class="dim">— as a picture · c back to folders</span></h1>
<p class="dim">${pages} pages of ${[...N.values()].filter((n) => !n.hollow && !n.outside).length} kinds · ${outsideN} things outside Mojira · ${hollow} kinds not reached${map.broken.length ? ` · <span class="err">${map.broken.length} broken links</span>` : ''}${map.waiting.length ? ` · <span class="absent">loading ${map.waiting.length} more (${esc([...new Set(map.waiting.map((h) => KINDS[kindOf(h)]))].join(', '))}); the map redraws when they arrive</span>` : ''}${map.truncated ? ' · + means more pages than the crawl followed' : ''} · crawled in ${esc(map.ms)} ms</p>
<p class="dim">rows: distance from home in links · then files, URLs and turns outside Mojira (squares) · then hollow: kinds reached only by a key · select a circle for its pages · y copies a mojira:// id</p>
<svg class="cascade sitemap" viewBox="0 0 ${L.width} ${L.height}" width="${L.width}" style="max-width:100%;height:auto">
${curves.join('\n')}
${[...N.values()].map(node).join('\n')}
</svg></div>` };
}

// The pages of one kind, as links, and which kinds link to it.
function kindPage(map, kind, { esc, link }) {
  const label = KINDS[kind] || EXTERNAL[kind] || (kind === 'broken' ? 'broken links' : kind);
  const from = [...map.edges].filter(([ek]) => ek.split('\t')[1] === kind).map(([ek, n]) => [ek.split('\t')[0], n]);
  const to = [...map.edges].filter(([ek]) => ek.split('\t')[0] === kind).map(([ek, n]) => [ek.split('\t')[1], n]);
  const kl = ([k, n]) => `${link('map:' + k, KINDS[k] || EXTERNAL[k] || k)} <span class="dim">${n}</span>`;
  let body;
  if (kind === 'broken') body = map.broken.map((b) => `<li>${esc(b.href)} <span class="dim">on ${link(b.from, b.from)}: ${esc(b.why)}</span></li>`).join('');
  else if (EXTERNAL[kind]) body = [...(map.external.get(kind) || [])].slice(0, 300).map((x) => `<li>${link(kind + ':' + x, x)}</li>`).join('');
  else {
    const k = map.kinds.get(kind);
    body = k ? [...k.pages].slice(0, 300).map((h) => `<li>${link(h, h.includes(':') ? h.slice(kind.length + 1) : KINDS[kind])}</li>`).join('')
      : `<li class="absent">no link reaches it${KEYED[kind] ? '; key ' + esc(KEYED[kind]) : ''}</li>`;
    if (k && (k.notLoaded || k.unexplored)) body += `<li class="dim">and more: ${k.notLoaded ? k.notLoaded + ' not loaded yet' : ''}${k.notLoaded && k.unexplored ? ', ' : ''}${k.unexplored ? k.unexplored + ' not followed' : ''}</li>`;
  }
  return { title: 'map: ' + label, html: `<div class="gcc"><h1>${esc(label)} <span class="dim">— on ${link('map', "Mojira's map")}</span></h1>
${from.length ? `<p>linked from: ${from.map(kl).join(' · ')}</p>` : ''}${to.length ? `<p>links to: ${to.map(kl).join(' · ')}</p>` : ''}
<ul>${body}</ul></div>` };
}

module.exports = { KINDS, KEYED, EXTERNAL, MAX_CHOICES, hrefsIn, linksIn, crawl, folders, capTree, folderView, view, kindPage };
