// Mojira's pattern pages over the graph from patterns.js (M-mojira,
// "Patterns" block). Each page shows a node and its edges: authored links
// (library headers) and observed ones (turns that proposed the pattern, and
// the patterns proposed with it). Counts appear only as dim annotations on
// the edges; the lists are ordered by family and id, or by time for turns.
// Pure: takes the graph and the html helpers, returns {title, html}.
'use strict';

module.exports = function pages(g, { esc, link, trim }) {
  const PARTS = ['IF', 'HOWEVER', 'THEN', 'BECAUSE'];
  const byId = (a, b) => (a < b ? -1 : a > b ? 1 : 0);
  const fam = (id) => id.split('/')[0];
  const main = new Set(g.components[0] || []);
  const compName = (i) => (i === 0 ? 'the main component' : 'component ' + (i + 1));
  const patLink = (id) => {
    const p = g.library.get(id);
    return link('pat:' + id, id, p ? (main.has(id) ? '' : 'pat-out') : 'pat-missing');
  };
  const turnDay = (t) => (t.at || '').slice(0, 16).replace('T', ' ');
  const grouped = (ids, extra) => {
    const m = new Map();
    for (const id of [...ids].sort(byId)) { if (!m.has(fam(id))) m.set(fam(id), []); m.get(fam(id)).push(id); }
    return [...m].map(([f, xs]) => `<li>${link('pf:' + f, f + '/')} ${xs.map((x) => patLink(x) + (extra ? extra(x) : '')).join(' · ')}</li>`).join('');
  };
  const proposedIn = (id) => (g.byPat.get(id) || []).length;
  const verdictText = (v) => (v ? `馬 ${esc(v.status)}${v.why.length ? ' (' + esc(v.why.join(', ')) + ')' : ''}` : '');

  function overview() {
    const comps = g.components.filter((c) => c.length > 1);
    const singles = g.components.filter((c) => c.length === 1);
    const kinds = new Map();
    for (const ls of g.out.values()) for (const l of ls) kinds.set(l.kind, (kinds.get(l.kind) || 0) + 1);
    const famsOf = (c) => {
      const m = new Map();
      for (const x of c) m.set(fam(x), (m.get(fam(x)) || 0) + 1);
      return [...m].sort((a, b) => b[1] - a[1]).slice(0, 5).map(([f]) => f).join(', ');
    };
    const usedOut = [...g.byPat.keys()].filter((id) => g.library.has(id) && !main.has(id));
    const missing = [...g.byPat.keys()].filter((id) => !g.library.has(id));
    const families = [...new Set([...g.library.keys()].map(fam))].sort(byId);
    return { title: 'patterns', html: `<h1>The pattern graph</h1>
<p class="dim">${g.library.size} library patterns · ${g.edges.authored} authored links · ${g.turns.size} turns proposing ${g.byPat.size} patterns · index ${esc(g.stats || '')}</p>
<h2>Authored links by header</h2><p class="dim">${[...kinds].sort((a, b) => b[1] - a[1]).map(([k, n]) => `@${esc(k)} ${n}`).join(' · ')}</p>
<h2>Components of the authored graph</h2><ul>
${comps.map((c) => `<li>${link('pc:' + g.components.indexOf(c), compName(g.components.indexOf(c)))} <span class="dim">${c.length} patterns: ${esc(famsOf(c))}</span></li>`).join('')}
<li>${link('pc:single', 'patterns with no authored links')} <span class="dim">${singles.length}</span></li></ul>
<h2>Where use and authoring disagree</h2><ul>
<li>${link('pout', 'proposed in turns, outside the main component')} <span class="dim">${usedOut.length}</span></li>
<li>${link('pmiss', 'proposed in turns, not in the library')} <span class="dim">${missing.length}</span></li></ul>
<h2>Turns</h2><ul><li>${link('pturns', 'recent turns and the patterns proposed to them')}</li></ul>
<h2>Families</h2><p>${families.map((f) => link('pf:' + f, f)).join(' · ')}</p>` };
  }

  function component(arg) {
    const ids = arg === 'single' ? g.components.filter((c) => c.length === 1).flat() : g.components[Number(arg)] || [];
    const title = arg === 'single' ? 'no authored links' : compName(Number(arg));
    return { title, html: `<h1>${esc(title)} <span class="dim">${ids.length} patterns</span></h1>
<p class="dim">Grouped by family. Patterns proposed in turns carry the number of turns.</p>
<ul>${grouped(ids, (x) => (proposedIn(x) ? ` <span class="dim">${proposedIn(x)}</span>` : ''))}</ul>` };
  }

  function family(f) {
    const ids = [...g.library.keys()].filter((id) => fam(id) === f).sort(byId);
    return { title: f, html: `<h1>${esc(f)}/ <span class="dim">${ids.length} patterns</span></h1>
<ul>${ids.map((id) => {
      const p = g.library.get(id), c = g.compOf.get(id);
      const where = c == null ? '' : g.components[c].length === 1 ? 'no authored links' : compName(c);
      return `<li>${patLink(id)} <span class="dim">${esc(trim(p.title, 70))} · ${esc(where)}</span></li>`;
    }).join('')}</ul>` };
  }

  function pattern(id) {
    const p = g.library.get(id);
    const outs = g.out.get(id) || [], ins = g.into.get(id) || [];
    const byKind = (ls, end) => {
      const m = new Map();
      for (const l of ls) { if (!m.has(l.kind)) m.set(l.kind, new Set()); m.get(l.kind).add(l[end]); }
      return [...m].map(([k, s]) => `<li><span class="verb">@${esc(k)}</span> ${[...s].sort(byId).map(patLink).join(', ')}</li>`).join('');
    };
    const uses = (g.byPat.get(id) || []).map((u) => ({ ...u, t: g.turns.get(u.turn) })).filter((u) => u.t)
      .sort((a, b) => byId(b.t.at || '', a.t.at || ''));
    const authoredNb = new Set(outs.map((l) => l.to).concat(ins.map((l) => l.from)));
    const co = (g.coOf.get(id) || []).slice().sort((a, b) => byId(a.with, b.with));
    const coBoth = co.filter((c) => authoredNb.has(c.with)), coOnly = co.filter((c) => !authoredNb.has(c.with));
    const coLi = (cs) => cs.map((c) => `<li>${patLink(c.with)} <span class="dim">in ${c.turns.length} turn${c.turns.length === 1 ? '' : 's'}</span></li>`).join('');
    const c = g.compOf.get(id);
    const head = p
      ? `<h1>${esc(p.title || id)}</h1>
<p class="dim">${esc(id)} · ${link('pf:' + fam(id), fam(id) + '/')} · ${c != null ? link('pc:' + (g.components[c].length === 1 ? 'single' : c), g.components[c].length === 1 ? 'no authored links' : compName(c)) : ''}${p.status ? ' · ' + esc(p.status) : ''} · ${link('file:' + p.file, 'open the .flexiarg')}</p>
${PARTS.filter((k) => p.parts[k]).map((k) => `<p><b>${k}</b> ${esc(trim(p.parts[k], 320))}</p>`).join('')}`
      : `<h1>${esc(id)}</h1><p class="err">Not in the library: a producer proposed an id with no .flexiarg behind it.</p>`;
    return { title: id, html: `${head}
<h2>Links it makes <span class="dim">authored</span></h2><ul>${byKind(outs, 'to') || '<li class="dim">none</li>'}</ul>
<h2>Links to it <span class="dim">authored</span></h2><ul>${byKind(ins, 'from') || '<li class="dim">none</li>'}</ul>
<h2>Turns that proposed it <span class="dim">observed, newest first</span></h2><ul>${uses.slice(0, 40).map((u) =>
      `<li>${link('pt:' + u.turn, turnDay(u.t))} <span class="dim">${esc(u.sources.join('+'))} ${verdictText(u.verdict)}</span> ${esc(trim(u.t.said, 70))}</li>`).join('') || '<li class="dim">none</li>'}
${uses.length > 40 ? `<li class="dim">and ${uses.length - 40} older</li>` : ''}</ul>
<h2>Proposed with it, and linked in the library</h2><ul>${coLi(coBoth) || '<li class="dim">none</li>'}</ul>
<h2>Proposed with it, not linked in the library</h2><ul>${coLi(coOnly) || '<li class="dim">none</li>'}</ul>` };
  }

  function turn(id) {
    const t = g.turns.get(id);
    if (!t) return { title: id, html: `<h1>${esc(id)}</h1><p class="err">No proposals recorded for this turn.</p>` };
    const ids = t.proposals.map((x) => x.id), set = new Set(ids);
    const inner = [];
    for (const a of ids) for (const l of g.out.get(a) || []) if (set.has(l.to)) inner.push([a, l.kind, l.to]);
    return { title: id, html: `<h1>${esc(turnDay(t))} <span class="dim">${esc(t.agent || '')} · ${esc(id)}</span></h1>
<p class="dim">session ${esc(t.session || '?')}${t.turn ? ' · ' + esc(t.turn) : ''}</p>
<p>${esc(t.said)}${t.said.length >= 240 ? '…' : ''}</p>
<h2>Proposed patterns</h2><ul>${t.proposals.map((x) =>
      `<li>${patLink(x.id)} <span class="dim">${esc(x.sources.join('+'))} ${verdictText(t.verdicts[x.id])}</span></li>`).join('')}</ul>
<h2>Library links among them</h2><ul>${inner.map(([a, k, b]) => `<li>${patLink(a)} <span class="verb">@${esc(k)}</span> ${patLink(b)}</li>`).join('')
      || '<li class="dim">none: the library does not link these patterns to each other</li>'}</ul>` };
  }

  function turns() {
    const ts = [...g.turns.values()].sort((a, b) => byId(b.at || '', a.at || '')).slice(0, 100);
    return { title: 'turns', html: `<h1>Recent turns and their patterns <span class="dim">newest first</span></h1>
<ul>${ts.map((t) => `<li>${link('pt:' + t.id, turnDay(t))} <span class="dim">${esc(t.agent || '')} · ${t.proposals.length}</span> ${esc(trim(t.said, 80))}</li>`).join('')}</ul>` };
  }

  function outside() {
    const ids = [...g.byPat.keys()].filter((id) => g.library.has(id) && !main.has(id));
    return { title: 'outside', html: `<h1>Proposed in turns, outside the main component <span class="dim">${ids.length}</span></h1>
<p class="dim">These patterns are in use, but the library does not link them into the main component. Each carries the number of turns that proposed it.</p>
<ul>${grouped(ids, (x) => ` <span class="dim">${proposedIn(x)}</span>`)}</ul>` };
  }

  function missing() {
    const ids = [...g.byPat.keys()].filter((id) => !g.library.has(id));
    return { title: 'not in library', html: `<h1>Proposed in turns, not in the library <span class="dim">${ids.length}</span></h1>
<ul>${grouped(ids, (x) => ` <span class="dim">${proposedIn(x)}</span>`)}</ul>` };
  }

  return { overview, component, family, pattern, turn, turns, outside, missing };
};
