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
// The same capped tree as the folders, drawn one choice point at a time
// (Joe 2026-11-18: "no more than five nodes from any choice point", after a
// picture of every kind at once put 16 dots in a row): the parent above,
// the chosen node, and its children (at most MAX_CHOICES) below. mapg:<path>,
// path = child indices from home ("2/0"). Text at the panel's size: the SVG
// is drawn at its own width and never scaled down.
const FONT = 14, COL = 170, W_MIN = 3 * COL;

function wrap(s, width = 18) {
  const out = [];
  let cur = '';
  for (const w of String(s).split(' ')) {
    if (cur && (cur + ' ' + w).length > width) { out.push(cur); cur = w; } else cur = cur ? cur + ' ' + w : w;
  }
  if (cur) out.push(cur);
  return out.slice(0, 3);
}

function nodeAt(tree, p) {
  let n = tree;
  const idx = String(p || '').split('/').filter((x) => x !== '').map(Number);
  for (const i of idx) {
    if (!n.children || !n.children[i]) return null;
    n = n.children[i];
  }
  return { n, idx };
}

function picture(map, p, { esc }) {
  const tree = capTree(folders(map));
  const at = nodeAt(tree, p) || nodeAt(tree, '');
  const { n, idx } = at;
  const here = idx.join('/');
  const parentPath = idx.slice(0, -1).join('/');
  const kids = n.children || [];
  const width = Math.max(W_MIN, kids.length * COL);
  const cx = width / 2;
  const Y = { up: 30, focus: 120, kids: 250 };
  const label = (x, y, lines, cls) => `<text class="${cls}" x="${x}" y="${y}" text-anchor="middle">${lines.map((l, i) => `<tspan x="${x}" dy="${i ? FONT + 3 : 0}">${esc(l)}</tspan>`).join('')}</text>`;
  const shape = (m, x, y, r) => (m.folder
    ? `<rect class="el" x="${x - r}" y="${y - r}" width="${2 * r}" height="${2 * r}"/>`
    : `<circle class="occ" cx="${x}" cy="${y}" r="${r}"/>`);
  const inside = (m) => ((m.children || []).length ? `${m.children.length} inside` : '');
  const sub = (m) => [m.note, inside(m)].filter(Boolean).join(' · ');
  const out = [];
  if (idx.length) {
    const parent = nodeAt(tree, parentPath).n;
    out.push(`<path class="edge" d="M${cx},${Y.up + 8} L${cx},${Y.focus - 12}"/>`);
    out.push(`<a class="ln" data-href="mapg${parentPath ? ':' + esc(parentPath) : ''}"><title>up to ${esc(parent.label)}</title>${label(cx, Y.up, ['↑ ' + parent.label], 'olabel')}</a>`);
  }
  kids.forEach((m, i) => {
    const x = (width - kids.length * COL) / 2 + COL * (i + 0.5);
    out.push(`<path class="edge" d="M${cx},${Y.focus + 12} C${cx},${(Y.focus + Y.kids) / 2} ${x},${(Y.focus + Y.kids) / 2} ${x},${Y.kids - 12}"/>`);
    const href = (m.children || []).length ? 'mapg:' + (here ? here + '/' : '') + i : m.href;
    const body = `${shape(m, x, Y.kids, 10)}${label(x, Y.kids + 30, wrap(m.label + (m.folder ? '/' : '')), 'olabel')}${label(x, Y.kids + 30 + (FONT + 3) * wrap(m.label).length, [sub(m)], 'elabel')}`;
    out.push(href ? `<a class="ln" data-href="${esc(href)}"><title>${esc(m.label)}</title>${body}</a>` : body);
  });
  const focusBody = `${shape(n, cx, Y.focus, 12)}${label(cx + 0, Y.focus + 34, [n.label + (n.folder ? '/' : '')], 'olabel')}${label(cx, Y.focus + 34 + FONT + 3, [n.note || ''], 'elabel')}`;
  out.push(n.href ? `<a class="ln" data-href="${esc(n.href)}"><title>${esc(n.label)}: its pages</title>${focusBody}</a>` : focusBody);
  const height = Y.kids + 30 + (FONT + 3) * 4 + 10;
  return { title: 'map: ' + n.label, html: `<div class="gcc" data-cycle="map">
<h1>Mojira's map <span class="dim">— ${esc(n.label)}${here ? '' : ', from home'} · c folders</span></h1>
<p class="dim">a dot with things inside opens one level down · the middle dot lists its pages · ↑ goes up · y copies a mojira:// id</p>
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
