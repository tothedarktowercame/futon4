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
  strand: 'Circus strand', mamap: 'intent graph', herd: '馬 herd', turn: 'turn records',
  bf: 'Benefit filings', 'bf-raw': 'Benefit filings raw', 'bf-evidence': 'Benefit reading evidence',
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
// The whole capped tree on one page (Joe 2026-11-18: "the site map on one
// page please"; opening one choice point at a time is for another day).
// Drawn as an indented tree: every node on its own row, indented by its
// depth, so every label sits on its own line at the panel's text size,
// nothing overlaps, and the width is the deepest indent plus the longest
// label (a left-to-right layout was 1421px wide for 35 nodes). It is
// the folders' capped tree, so no dot has more than MAX_CHOICES lines out
// ("no more than five nodes from any choice point"). Squares are folders,
// circles are kinds; a kind's label opens its pages. The SVG is drawn at its
// own size with no viewBox, so the text is never scaled down.
const FONT = 14, NOTE = 13, ROW = 28, INDENT = 30, R = 6, CHAR = 0.62;

function picture(map, _p, { esc }) {
  const tree = capTree(folders(map));
  const nodes = [];
  const textW = (n) => (n.label.length + (n.folder ? 1 : 0)) * FONT * CHAR + (n.note ? 8 + n.note.length * NOTE * CHAR : 0);
  const place = (n, depth, parent) => {
    const me = { n, depth, parent, id: nodes.length, y: (nodes.length + 1) * ROW, x: R + 4 + depth * INDENT };
    nodes.push(me);
    (n.children || []).forEach((c) => place(c, depth + 1, me.id));
  };
  place(tree, 0, null);
  const width = Math.ceil(Math.max(...nodes.map((m) => m.x + R + 6 + textW(m.n))) + 8);
  const height = (nodes.length + 1) * ROW;
  const out = [];
  for (const m of nodes) {
    if (m.parent === null) continue;
    const p = nodes[m.parent];
    out.push(`<path class="edge" data-from="${p.id}" d="M${p.x},${p.y + R} V${m.y - 8} Q${p.x},${m.y} ${p.x + 8},${m.y} H${m.x - R - 2}"/>`);
  }
  for (const m of nodes) {
    const { n, x, y } = m;
    const shape = n.folder
      ? `<rect class="el" x="${x - R}" y="${y - R}" width="${2 * R}" height="${2 * R}"/>`
      : `<circle class="occ" cx="${x}" cy="${y}" r="${R}"/>`;
    const text = `<text x="${x + R + 6}" y="${y}" dominant-baseline="middle"><tspan class="olabel">${esc(n.label + (n.folder ? '/' : ''))}</tspan>${n.note ? `<tspan class="elabel" dx="8">${esc(n.note)}</tspan>` : ''}</text>`;
    out.push(n.href ? `<a class="ln" data-href="${esc(n.href)}"><title>${esc(n.label)}: its pages</title>${shape}${text}</a>` : shape + text);
  }
  return { title: 'map: picture', html: `<div class="gcc" data-cycle="map">
<h1>Mojira's map <span class="dim">— the whole of it, from home · c folders</span></h1>
<p class="dim">squares are folders, circles are kinds of page · a kind's name lists its pages · y copies a mojira:// id</p>
<svg class="cascade mapg" width="${width}" height="${height}" style="font-size:${FONT}px">
${out.join('\n')}
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

module.exports = { KINDS, KEYED, EXTERNAL, MAX_CHOICES, hrefsIn, linksIn, crawl, folders, capTree, folderView, picture, kindPage };
