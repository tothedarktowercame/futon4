'use strict';
// Pure view over para_diff.py's read-only export. Offsets are codepoints.
const esc = s => String(s ?? '').replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
const words = s => [...String(s || '').matchAll(/\S+/gu)];
const norm = s => s.toLowerCase().replace(/[^\p{L}\p{N}_]/gu, '');
function parse(text) {
  const d = JSON.parse(text);
  if (!d.draft || !Array.isArray(d.paragraphs)) throw new Error('invalid variants document');
  for (const p of d.paragraphs) {
    if (!Number.isInteger(p.n) || !Array.isArray(p.ops) || !Array.isArray(p.sources) || !Array.isArray(p.segments)) throw new Error('invalid paragraph');
    if (p.ops.some(o => !['kept','new','added','dropped','changed'].includes(o.op))) throw new Error('unknown edit operation');
  }
  return d;
}
const link = (href, text, cls = '') => `<a class="ln ${cls}" tabindex="0" data-href="${esc(href)}">${esc(text)}</a>`;
function anchor(p, o) {
  if (['new','added'].includes(o.op)) return { written: true };
  const index = p.sources.findIndex(s => s.id === o.source), source = p.sources[index];
  const a = o.source_start, b = o.source_end, chars = Array.from(source?.text || '');
  const slice = chars.slice(a, b).join('');
  // Kept words use the producer's case/punctuation-insensitive comparison;
  // changed/dropped old words are exact apart from collapsed whitespace.
  const equal = o.op === 'kept'
    ? JSON.stringify(words(slice).map(w => norm(w[0]))) === JSON.stringify(words(o.text).map(w => norm(w[0])))
    : words(slice).map(w => w[0]).join(' ') === words(o.old).map(w => w[0]).join(' ');
  const valid = source && Number.isInteger(a) && Number.isInteger(b) && a >= 0 && b > a && b <= chars.length
    && (a === 0 || /\s/u.test(chars[a - 1])) && (b === chars.length || /\s/u.test(chars[b]))
    && Number.isInteger(source.used_start) && Number.isInteger(source.used_end)
    && source.used_start >= 0 && source.used_start <= a && source.used_end >= b && source.used_end <= chars.length && equal;
  return { source, index, a, b, chars, valid };
}
function pieces(o, mode) {
  if (o.op === 'kept' || o.op === 'new') return [['text', o.text]];
  if (o.op === 'added') return [['new', o.new]];
  if (o.op === 'dropped') return mode === 'full' ? [['old', o.old]] : [];
  return mode === 'full' ? [['old', o.old], ['new', o.new]] : [['new', o.new]];
}
function render(d, arg = '') {
  if (!arg) return {title: 'Draft 2 variants', html: `<h1>Draft 2 over its source turns</h1><p>${esc(d.draft.note)}</p><ul>${d.paragraphs.map(p => `<li>${link('vd:' + p.n, `${p.n}. ${p.title} — ${p.linked} of ${p.segments.length} linked`)}</li>`).join('')}</ul>`};
  const [n, requested] = arg.split(':'), mode = ['full','additions','clean'].includes(requested) ? requested : 'full';
  const idx = d.paragraphs.findIndex(p => String(p.n) === n), p = d.paragraphs[idx];
  if (!p) throw new Error('paragraph not found');
  const body = p.ops.map((o, i) => {
    const a = anchor(p, o);
    const warning = !a.written && !a.valid ? '<span class="err">anchor does not match source</span> ' : '';
    return warning + pieces(o, mode).map(([part, text]) => words(text).map((w, wi) => {
      const written = o.op === 'new' || part === 'new';
      const cls = `vd-word${mode !== 'clean' && written ? ' vd-added' : ''}${part === 'old' ? ' vd-dropped' : ''}${mode === 'full' && !written && a.valid ? ' vd-source vd-s' + a.index % 3 : ''}`;
      return link(`vds:${p.n}:${i}:${part}:${wi}`, w[0], cls);
    }).join(' ')).join(' ');
  }).join(' ');
  return {title: `Draft 2 · ${p.n}`, html: `<section data-vd="${p.n}" data-vd-mode="${mode}" data-vd-prev="${d.paragraphs[idx - 1]?.n || ''}" data-vd-next="${d.paragraphs[idx + 1]?.n || ''}"><h1>${esc(p.n)}. ${esc(p.title)}</h1><p>${p.linked} of ${p.segments.length} linked · ${esc(mode)} · ${link('vd', 'paragraphs')}</p><p>${esc(d.draft.note)}</p><p>t: full/clean · T: additions/full · n/p: next/previous · j/k: select word</p><p class="vd-text">${body}</p></section>`};
}
function renderSource(d, arg) {
  const [n, i, part, wi] = arg.split(':'), p = d.paragraphs.find(p => String(p.n) === n), o = p?.ops[Number(i)];
  if (!o) throw new Error('word not found');
  const a = anchor(p, o);
  if (a.written) return {html: '<p>Written in the draft; no spoken source</p>'};
  if (!a.valid) return {html: '<p class="err">anchor does not match source</p>'};
  let start = a.a, end = a.b;
  if (part !== 'new') {
    const w = words(a.chars.slice(a.a, a.b).join(''))[Number(wi)];
    if (!w) throw new Error('word not found');
    start += Array.from(a.chars.slice(a.a, a.b).join('').slice(0, w.index)).length;
    end = start + Array.from(w[0]).length;
  }
  const cuts = [...new Set([0, a.source.used_start, start, end, a.source.used_end, a.chars.length])].sort((x,y) => x-y);
  let text = '';
  for (let j = 0; j + 1 < cuts.length; j++) {
    const x = cuts[j], y = cuts[j+1], t = esc(a.chars.slice(x,y).join(''));
    text += x >= start && y <= end ? `<mark data-vd-here>${t}</mark>` : x >= a.source.used_start && y <= a.source.used_end ? `<span class="vd-used">${t}</span>` : t;
  }
  const date = new Date(a.source.at), time = Number.isNaN(date.getTime()) ? 'time unavailable' : date.toISOString().replace('T',' ').replace('.000Z',' UTC');
  return {html: `<h2>[${a.index+1}] spoken ${esc(time)}</h2><p>${esc(a.source.id)}</p><div class="vd-source-text">${text}</div>`};
}
const css = `.vd-text a.vd-word { color: inherit; text-decoration: none; cursor: pointer; }
.vd-text a.vd-added { color: #268b45; }
.vd-text a.vd-dropped { text-decoration: line-through; }
.vd-text a.vd-source { border-bottom: 1px solid #888; }
.vd-text a.vd-s1 { border-bottom-style: double; border-bottom-width: 3px; }
.vd-text a.vd-s2 { border-bottom-style: dotted; }
.vd-source-text { white-space: pre-wrap; }
.vd-used { background: var(--vscode-editor-selectionHighlightBackground, #7774); }
[data-vd-here] { background: var(--vscode-editor-findMatchHighlightBackground, #e9b84a); color: inherit; outline: 2px solid var(--vscode-focusBorder); }`;
module.exports = {parse, anchor, render, renderSource, css};
