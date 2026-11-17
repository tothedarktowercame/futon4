// Giraffe's top items and "why not?" (M-repl-circus; claude-13's routes,
// futon3c 56efba156 + 7b6b28c95):
//   GET /api/alpha/circus/top-items?run=latest&n=10
//   GET /api/alpha/circus/why-not?run=latest&item=M-...
// Pure: takes the route's JSON and the html helpers, returns {title, html}.
// Plain text like the party page. Items link to their GCC item page only when
// it exists; a turn links to Mojira's turn page, and 象 opens it in the
// stepper in Emacs (turn-stepper-open-turn, futon3c c4bb4216a); "Lean-checked" only when the
// certificate's check says ok (a file alone is not a check).
'use strict';

const ANIMALS = ['驴', '馬', '狗', '象', 'mentions', '猴'];
const LADDER = ['input', 'excluded', 'rejected', 'pass', 'priority', 'ranked', 'focus'];

module.exports = function views({ esc, link, trim, hasItem }) {
  const itemRef = (x) => (hasItem(x) ? link('gi:' + x, x) : esc(x));
  const word = (s) => esc(String(s == null ? '' : s).replace(/-/g, ' '));

  // 驴3 馬3 狗3 象3 m1 猴3, a zero or a missing count marked.
  function counts(c) {
    return ANIMALS.map((a) => {
      const n = c ? c[a] : undefined;
      const label = a === 'mentions' ? 'm' : a;
      return n ? `${esc(label)}${esc(n)}` : `<span class="absent">${esc(label)}${n === 0 ? '0' : '–'}</span>`;
    }).join(' ');
  }

  function cert(c) {
    if (!c || !c.path) return '';
    if (c.check && c.check.ok === true) return ` ${link('file:' + c.path, 'Lean-checked', 'cert')}`;
    return ` <span class="dim">${link('file:' + c.path, 'certificate')} (not checked${c.check ? ', exit ' + esc(c.check.exit) : ''})</span>`;
  }

  function turnLine(r) {
    const t = r.turn || {};
    return `<div class="gs-r sub">${t.id ? link('pt:' + t.id, t.id) + ' ' + link('step:' + t.id, '象') : '<span class="absent">no turn</span>'} <span class="dim">${esc(String(t.created_at || '').slice(0, 16).replace('T', ' '))}</span> ${counts(r['gcc-counts'])}${cert(r.certificate)}</div>`;
  }

  // A turn whose 猴 header named nothing, so focus went to its REPL's
  // mission tag (resolved through GCC supersedes).
  function tagLine(t) {
    const p = t['supersession-path'] || [t.tag];
    return `<div class="gs-r sub">${t['turn-id'] ? link('pt:' + t['turn-id'], t['turn-id']) + ' ' + link('step:' + t['turn-id'], '象') : '<span class="absent">no turn</span>'} <span class="dim">${esc(String(t.created_at || '').slice(0, 16).replace('T', ' '))} ${esc(t.seat || '')}</span> via mission tag ${p.map(esc).join(' → ')}</div>`;
  }
  const viaTag = (it) => {
    const t = (it['focus-tags'] || [])[0];
    return t ? ' <span class="dim">via mission tag ' + (t['supersession-path'] || [t.tag]).map(esc).join(' → ') + '</span>' : '';
  };

  function top(d) {
    const items = d.items || [];
    const rows = items.map((it) => {
      const rel = it['focus-relations'] || [];
      const tags = it['focus-tags'] || [];
      const hou = rel.some((r) => (r['gcc-counts'] || {})['猴']);
      const open = it['priority-band'] === 'focus' && !hou && !tags.length;
      const tagOnly = it['priority-band'] === 'focus' && !hou && tags.length;
      return `<div class="gs-r">${esc(it.rank)} ${itemRef(it.item)} — ${word(it['priority-band'] || 'no band')} · ${word(it['cutoff-status'] || '?')} <span class="dim">(pass rank ${esc(it['pass-rank'] == null ? '?' : it['pass-rank'])})</span> ${link('wn:' + it.item, 'why')}${open ? ' <span class="absent">open question: in focus, no 猴 link</span>' : ''}${tagOnly ? viaTag(it) : ''}</div>
${rel.slice(0, 3).map(turnLine).join('')}${tags.slice(0, Math.max(0, 3 - Math.min(rel.length, 3))).map(tagLine).join('')}${rel.length + tags.length > 3 ? `<div class="gs-r sub dim">and ${rel.length + tags.length - 3} more turns</div>` : ''}${!rel.length && !tags.length && it['focus-reason'] ? `<div class="gs-r sub dim">no turn behind it: ${word(it['focus-reason'])}</div>` : ''}`;
    }).join('\n');
    return { title: 'top items', html: `<div class="gcc" data-cycle="top-raw">
<h1>Giraffe's top items <span class="dim">run ${esc(d.run || d['run-id'] || '?')}</span></h1>
<p class="dim">generated ${esc(d.generated_at || '?')} · ${items.length} of the run's ranked items${d['truncated?'] ? ' (more ranked below)' : ''} · under each item, the turns behind it: 驴 馬 狗 象 m(entions) 猴 · why: the stage ladder · c raw JSON</p>
${rows || '<div class="gs-r absent">no ranked items in this run</div>'}</div>` };
  }

  function whyNot(d) {
    if (d.status !== 'ok') return { title: 'why not', html: `<h1>Why not ${esc(d.item || '')}?</h1><p class="err">${esc(d.reason || d.status || 'no answer')}</p>` };
    const stop = d.stage;
    const reached = d.stage === 'ranked' && d['focus-status'] === 'present' ? 'focus' : stop;
    const at = LADDER.indexOf(reached);
    const ladder = LADDER.map((s, i) => (s === reached ? `<b class="stop">${esc(s)}</b>` : i < at ? esc(s) : `<span class="dim">${esc(s)}</span>`)).join(' → ');
    const ev = d.evidence || {};
    const evLine = (k, v) => {
      if (v && typeof v === 'object' && v.path) return `<div class="gs-r sub">${word(k)}: ${link('file:' + v.path, v.path.replace('/home/joe/code/', ''))}</div>`;
      if (v && typeof v === 'object') return `<div class="gs-r sub">${word(k)}: ${esc(JSON.stringify(v))}</div>`;
      return `<div class="gs-r sub">${word(k)}: ${esc(v)}</div>`;
    };
    const rel = d['focus-relations'] || [];
    return { title: 'why ' + d.item, html: `<div class="gcc" data-cycle="wn-raw:${esc(d.item)}">
<h1>${itemRef(d.item)} <span class="dim">— where it stands in run ${esc(d.run || '?')}</span></h1>
<div class="gs-h">LADDER</div><div class="gs-r">${ladder}</div>
<div class="gs-h">${reached === 'focus' ? 'REACHED FOCUS' : 'STOPPED AT ' + esc(String(stop || '?').toUpperCase())}</div><div class="gs-r">${word(d.reason || 'no reason given')}</div>
${Object.entries(ev).map(([k, v]) => evLine(k, v)).join('')}
<div class="gs-h">FOCUS</div><div class="gs-r">${d['focus-status'] === 'present' ? 'present' + (d['focus-route'] ? ` <span class="dim">by ${esc(d['focus-route'] === 'both' ? 'relation and mission tag' : d['focus-route'].replace('-', ' '))}</span>` : '') : `<span class="absent">absent${d['focus-reason'] ? ': ' + word(d['focus-reason']) : ''}</span>`}</div>
${rel.map(turnLine).join('')}${(d['focus-tags'] || []).map(tagLine).join('')}
<p class="dim">c raw JSON</p></div>` };
  }

  const raw = (title, back, d) => ({ title: title + ' (raw)', html: `<div data-cycle="${esc(back)}"><h1>${esc(title)} <span class="dim">raw · c back</span></h1><pre class="sexp">${esc(JSON.stringify(d, null, 2))}</pre></div>` });

  return { top, whyNot, raw, counts };
};
module.exports.LADDER = LADDER;
