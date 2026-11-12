// The GCC item view (M-repl-circus demo 1, "GCC load design"): one backlog
// item and what the GCC holds about it, read from the preview files the
// projector writes (~/storage/gcc-preview/; format, v2:
// ~/notes/claude-13/demo-01-contract.md). Files are re-read on every render,
// so a regenerated preview shows on the next redraw (r, or moving the cursor).
//
// The item page renders the file's "display" block only, as plain text laid
// out like claude-13's mock-up (Joe, 2026-11-12: "much more readable &
// usable" than one box per record). The records, links and absences, with
// their hashes and provenance, are behind c (the raw view). Links go item to
// item only, and only to items that have their own file; record ids are
// never link targets.
'use strict';
const fs = require('fs');
const path = require('path');

const FIXTURE = path.join(__dirname, 'test', 'gcc-preview-fixture');
const ITEM_ID = /^[\w.\-\u0080-￿]+$/u;

// {dir, fixture} for DIR when it has an index.json, else the test fixture.
function source(dir) {
  if (dir && fs.existsSync(path.join(dir, 'index.json'))) return { dir, fixture: false };
  return { dir: FIXTURE, fixture: true };
}

function readIndex(dir) { return JSON.parse(fs.readFileSync(path.join(dir, 'index.json'), 'utf8')); }
function hasItem(dir, id) { return typeof id === 'string' && ITEM_ID.test(id) && fs.existsSync(path.join(dir, 'items', id + '.json')); }
function readItem(dir, id) {
  if (typeof id !== 'string' || !ITEM_ID.test(id)) throw new Error('not an item id: ' + id);
  return JSON.parse(fs.readFileSync(path.join(dir, 'items', id + '.json'), 'utf8'));
}

const MARK = { true: '✓', false: '✗', null: '?' };

module.exports = function views({ esc, link, trim }) {
  const banner = (src) => (src.fixture
    ? `<p class="err">FIXTURE: invented content from ${esc(path.relative(path.dirname(__dirname), src.dir))}; the projector's preview is not there yet.</p>` : '');
  const absent = (x) => x && typeof x === 'object' && 'absent' in x;
  const word = (x, none) => (absent(x) ? `<span class="absent">${esc(x.absent || none)}</span>` : x == null || x === '' ? `<span class="dim">${esc(none)}</span>` : esc(x));
  const head = (name, note) => `<div class="gs-h">${esc(name)}${note ? ` <span class="dim">${note}</span>` : ''}</div>`;
  const row = (html, cls) => `<div class="gs-r${cls ? ' ' + cls : ''}">${html}</div>`;

  function list(src) {
    const ix = readIndex(src.dir);
    return { title: 'GCC items', html: `<h1>GCC items <span class="dim">${ix.items.length}</span></h1>
${banner(src)}<p class="dim">generated ${esc(ix.generated_at || '?')}</p>
<ul>${ix.items.map((it) => `<li>${link('gi:' + it.id, it.id)} <span class="dim">${esc(it.kind || '')} · ${esc(trim(it.title || '', 70))}</span></li>`).join('')}</ul>` };
  }

  function item(src, id) {
    const d = readItem(src.dir, id);
    const v = d.display;
    // An item id becomes a link only when that item has its own page.
    const itemRef = (x) => (hasItem(src.dir, x) ? link('gi:' + x, x) : esc(x));
    if (!v) {
      const it = d.item || {};
      return { title: id, html: `<div class="gcc" data-cycle="gi-raw:${esc(id)}">
<h1>${esc(id)} <span class="dim">${esc(it.title || '')}</span></h1>${banner(src)}
<p class="absent">This item's file has no "display" block yet (the projector writes it), so there is nothing to show but the raw records: c.</p></div>` };
    }
    const h = v.header || {};
    const out = [];
    out.push(`<h1>${esc(h.id || id)} <span class="dim">— ${esc(h.title || '')}</span></h1>
<p class="gs-tag">[${word(h.state_group, 'no state group')} · ${word(h.declared_status, 'no Status line')}]</p>
<p class="dim">${esc(h.path || '')}</p>`);

    // WHAT GIRAFFE READ
    const r = v.reading;
    if (!r || absent(r)) out.push(head('WHAT GIRAFFE READ') + row(word(r || { absent: 'no reading recorded' }, 'no reading')));
    else {
      const ver = r.of_current_version === true ? "of this document's current version"
        : r.of_current_version === false ? 'of an EARLIER version of this document' : 'version not known';
      out.push(head('WHAT GIRAFFE READ', `(latest reading, ${esc(ver)}${r.read_at ? ', ' + esc(r.read_at) : ''}${r.reader ? ', ' + esc(r.reader) : ''})`));
      out.push(row(`“${word(r.summary, 'no summary')}”`, 'quote'));
      out.push(row(`<span class="dim">—</span> ${word(r.status_as_read, 'no status')} · ${r.link_count == null ? '?' : esc(r.link_count)} links found`, 'sub'));
      const earlier = r.earlier || [];
      const n = r.earlier_count == null ? earlier.length : r.earlier_count;
      if (n) {
        out.push(row(`${esc(n)} earlier reading${n === 1 ? '' : 's'} <span class="dim">(of earlier versions)</span> <span class="dim">[e show]</span>`));
        out.push(`<div class="fold" data-fold="e">${earlier.map((x) => row(`${esc(x.read_at || '?')} <span class="dim">of ${esc(x.of_version || '?')}</span> “${esc(trim(x.summary || '', 110))}”`, 'sub')).join('')}</div>`);
      }
      for (const w of r.warnings || []) out.push(row(`! ${esc(w)}`, 'warn'));
    }

    // WAITING ON
    const wo = v.waiting_on || [];
    const lines = [...new Map(wo.filter((x) => x.line != null || x.quote).map((x) => [x.line + '\u0000' + x.quote, x])).values()];
    const from = lines.length === 1 ? `(needs, from line ${esc(lines[0].line)}: “${esc(trim(lines[0].quote || '', 80))}”)` : '(needs)';
    out.push(head('WAITING ON', wo.length ? from : ''));
    if (!wo.length) out.push(row('<span class="dim">nothing recorded</span>'));
    for (const x of wo) {
      const m = MARK[String(x.met == null ? null : x.met)] || '?';
      out.push(row(`<span class="met met-${m === '✓' ? 'met' : m === '✗' ? 'not-met' : 'unknown'}">${m}</span> ${itemRef(x.id)}  ${word(x.state_group, 'no state group')}${x.declared_status ? ` — “${esc(x.declared_status)}”` : ''}${x.title ? ` <span class="dim">${esc(trim(x.title, 70))}</span>` : ''}${lines.length > 1 && x.line != null ? ` <span class="dim">(line ${esc(x.line)})</span>` : ''}`));
    }

    // RELATED
    const rel = v.related || [];
    out.push(head('RELATED') + row(rel.length ? rel.map((x) => itemRef(x.id) + (x.resolved === false ? ' <span class="dim">(unresolved)</span>' : '')).join(' · ') : '<span class="dim">none recorded</span>'));

    // WHY IT MATTERS
    const why = v.why || {};
    out.push(head('WHY IT MATTERS'));
    for (const b of why.benefit_lines || []) {
      const who = [b.producer, b.draft ? 'draft' : null].filter(Boolean).join(', ');
      out.push(row(`serves benefit line ${esc(b.code || '?')}: ${absent(b.text) ? `<span class="absent">${esc(b.text.absent)}</span>` : esc(b.text || '')}${who ? ` <span class="dim">(${esc(who)})</span>` : ''}`));
    }
    if (!(why.benefit_lines || []).length) out.push(row('<span class="dim">no benefit line recorded</span>'));
    const q = why.question;
    out.push(row(!q || absent(q) ? `question it serves: <span class="absent">none recorded</span>${q && q.absent ? ` <span class="dim">(${esc(q.absent)})</span>` : ''}`
      : `question it serves: ${esc(q.id || '')} “${esc(q.text || '')}”`));

    // NAMES
    const names = v.names || [];
    out.push(head('NAMES', names.length ? `(${names.length} lexicon entit${names.length === 1 ? 'y' : 'ies'})` : '')
      + row(names.length ? names.map((x) => esc(x.label || x.slug)).join(' · ') : '<span class="dim">none</span>'));

    // MENTIONED
    const mn = v.mentions || {};
    const at = (x) => (x ? `${esc(x.date || '?')} <span class="dim">(${esc([x.role, x.who].filter(Boolean).join(', '))})</span>` : '?');
    if (!mn.count) out.push(head('MENTIONED') + row('<span class="dim">no mentions recorded</span>'));
    else {
      out.push(head('MENTIONED') + row(`${esc(mn.count)} time${mn.count === 1 ? '' : 's'} — first ${at(mn.first)} · latest ${at(mn.latest)} <span class="dim">[m list]</span>`));
      out.push(`<div class="fold" data-fold="m">${(mn.list || []).map((x) => row(`${esc(x.date || '?')} ${esc(x.role || '')} <span class="dim">${esc(x.who || '')}</span>`, 'sub')).join('')}</div>`);
    }
    out.push(`<p class="dim">c raw records (hashes, ids, provenance)</p>`);
    return { title: id, html: `<div class="gcc" data-cycle="gi-raw:${esc(id)}">${banner(src)}${out.join('\n')}</div>` };
  }

  function raw(src, id) {
    const d = readItem(src.dir, id);
    return { title: id + ' (raw)', html: `<div data-cycle="gi:${esc(id)}">
<h1>${esc(id)} <span class="dim">raw records · c back to the view</span></h1>
${banner(src)}<pre class="sexp">${esc(JSON.stringify(d, null, 2))}</pre></div>` };
  }

  return { list, item, raw };
};
module.exports.source = source;
module.exports.hasItem = hasItem;
module.exports.FIXTURE = FIXTURE;
