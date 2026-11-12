// The GCC item view (M-repl-circus demo 1, "GCC load design"): one backlog
// item with everything the GCC would hold about it, read from the preview
// files the projector writes (~/storage/gcc-preview/; format:
// ~/notes/claude-13/demo-01-contract.md). Files are re-read on every render,
// so a regenerated preview shows on the next redraw (r, or moving the cursor).
//
// Records whose status is "proposed" (a schema line Joe has not seen yet) are
// drawn faint and dashed, as proposed things are (E-gcc-schema §16).
'use strict';
const fs = require('fs');
const path = require('path');

const FIXTURE = path.join(__dirname, 'test', 'gcc-preview-fixture');

// {dir, fixture} for DIR when it has an index.json, else the test fixture.
function source(dir) {
  if (dir && fs.existsSync(path.join(dir, 'index.json'))) return { dir, fixture: false };
  return { dir: FIXTURE, fixture: true };
}

function readIndex(dir) { return JSON.parse(fs.readFileSync(path.join(dir, 'index.json'), 'utf8')); }
function readItem(dir, id) {
  if (!/^[\w.-]+$/.test(id)) throw new Error('bad item id ' + id);
  return JSON.parse(fs.readFileSync(path.join(dir, 'items', id + '.json'), 'utf8'));
}

// Records grouped by type, in first-seen order. Within a type, supersession
// chains, newest first: a chain starts at a record no other record
// supersedes and follows `supersedes` back. Returns [{type, chains: [[rec]]}].
function groups(records) {
  const byType = new Map();
  for (const r of records || []) {
    if (!byType.has(r.type)) byType.set(r.type, []);
    byType.get(r.type).push(r);
  }
  return [...byType].map(([type, rs]) => {
    const byId = new Map(rs.map((r) => [r.id, r]));
    const replaced = new Set(rs.map((r) => r.supersedes).filter(Boolean));
    const chains = [];
    const used = new Set();
    for (const r of rs) {
      if (replaced.has(r.id)) continue;
      const chain = [];
      for (let x = r; x && !used.has(x.id); x = byId.get(x.supersedes)) { chain.push(x); used.add(x.id); }
      chains.push(chain);
    }
    // A cycle leaves records no head reaches: show them rather than lose them.
    for (const r of rs) if (!used.has(r.id)) { chains.push([r]); used.add(r.id); }
    return { type, chains };
  });
}

const MET = { true: 'met', false: 'not met', null: 'unknown' };
const metOf = (l) => MET[String(l.computed && 'met' in l.computed ? l.computed.met : null)] || 'unknown';

module.exports = function views({ esc, link, trim }) {
  const val = (v) => esc(typeof v === 'string' ? v : JSON.stringify(v));
  const cls = (r) => (r.status === 'proposed' ? ' proposed' : '');
  const statusTag = (r) => `<span class="gcc-status">${esc(r.status || '?')}</span>`;
  const prov = (p) => (p ? `<span class="dim">${esc(p.source || '?')} · ${esc(p.ref || '')} · ${esc(p.basis || '?')}</span>` : '<span class="dim">no provenance</span>');
  const fields = (f) => Object.entries(f || {}).map(([k, v]) => `<span class="dim">${esc(k)}</span> ${val(v)}`).join(' · ');
  const banner = (src) => (src.fixture
    ? `<p class="err">FIXTURE: invented content from ${esc(path.relative(path.dirname(__dirname), src.dir))}; the projector's preview is not there yet.</p>` : '');

  function list(src) {
    const ix = readIndex(src.dir);
    return { title: 'GCC items', html: `<h1>GCC items <span class="dim">${ix.items.length}</span></h1>
${banner(src)}<p class="dim">generated ${esc(ix.generated_at || '?')} · shapes ${esc(ix.shapes_file || '?')}</p>
<ul>${ix.items.map((it) => `<li>${link('gi:' + it.id, it.id)} <span class="dim">${esc(it.kind || '')} · ${esc(trim(it.title || '', 60))} · ${Object.values(it.counts || {}).reduce((a, b) => a + b, 0)} records</span></li>`).join('')}</ul>` };
  }

  function stateGroup(sg) {
    if (sg && typeof sg === 'object') return `<span class="absent">absent: ${esc(sg.absent || 'no reason given')}</span>`;
    return sg ? esc(sg) : '<span class="absent">absent: no value and no reason</span>';
  }

  function record(r, i) {
    const old = i > 0;
    return `<li class="gcc-rec${cls(r)}${old ? ' superseded' : ''}" data-status="${esc(r.status || '')}">${statusTag(r)} <b>${esc(r.id)}</b>${old ? ' <span class="dim">superseded</span>' : ''}${r.supersedes ? ` <span class="dim">supersedes ${esc(r.supersedes)}</span>` : ''}<br>${fields(r.fields)}<br>${prov(r.provenance)}</li>`;
  }

  function item(src, id) {
    const d = readItem(src.dir, id);
    const it = d.item || {};
    const gs = groups(d.records);
    const links = d.links || [];
    const absences = d.absences || [];
    return { title: id, html: `<div data-cycle="gi-raw:${esc(id)}">
<h1>${esc(it.title || id)} <span class="dim">${esc(id)}</span></h1>
${banner(src)}<p class="dim">${esc(it.repo || '?')} · ${esc(it.path || '?')} · state group: ${stateGroup(it.state_group)}</p>
<p class="dim">faint and dashed: proposed (a schema line not yet seen) · c shows the raw records</p>
${gs.map((g) => `<h2>${esc(g.type)} <span class="dim">${g.chains.reduce((a, c) => a + c.length, 0)}</span></h2>
${g.chains.map((c) => `<ol class="gcc-chain">${c.map(record).join('')}</ol>`).join('')}`).join('')}
<h2>Links <span class="dim">${links.length}</span></h2><ul>${links.map((l) => `<li class="gcc-rec${cls(l)}" data-status="${esc(l.status || '')}">${statusTag(l)} <span class="verb">${esc(l.type)}</span> ${esc(l.from)} → ${link('gi:' + l.to, l.to)} <span class="met met-${metOf(l).replace(' ', '-')}">${metOf(l)}</span>${l.fields && Object.keys(l.fields).length ? ' · ' + fields(l.fields) : ''}</li>`).join('') || '<li class="dim">none</li>'}</ul>
<h2>Absent <span class="dim">${absences.length}</span></h2><ul>${absences.map((a) => `<li class="absent">No ${esc(a.what)}: ${esc(a.reason)}</li>`).join('') || '<li class="dim">nothing recorded as absent</li>'}</ul></div>` };
  }

  function raw(src, id) {
    const d = readItem(src.dir, id);
    return { title: id + ' (raw)', html: `<div data-cycle="gi:${esc(id)}">
<h1>${esc(id)} <span class="dim">raw records · c back to the view</span></h1>
${banner(src)}<pre class="sexp">${esc(JSON.stringify(d, null, 2))}</pre></div>` };
  }

  return { list, item, raw };
};
module.exports.groups = groups;
module.exports.source = source;
module.exports.metOf = metOf;
module.exports.FIXTURE = FIXTURE;
