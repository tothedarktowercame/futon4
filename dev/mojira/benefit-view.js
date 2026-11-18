'use strict';
// Pure: returned facts and host check in, HTML out. No store or file access.
const esc = x => String(x == null ? '' : x).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const word = x => typeof x === 'string' ? x.replace(/^:/, '') : x;
const link = (to, label) => `<a class="ln" data-href="${esc(to)}">${esc(label)}</a>`;
const json = x => `<pre>${esc(JSON.stringify(x, null, 2))}</pre>`;
function title(record) {
  const c = record && record['asserted-content'] || {};
  return c['benefit/line'] || c['item/id'] || c['entity/slug'] || (record && record.id) || '(record id missing)';
}
function reading(r) {
  if (!r) return '<p class="err">Reading missing</p>';
  const facts = (r.evidence || []).map(a => `<p class="dim">source ${esc((a['source-refs'] || []).map(ref => ref.path || ref.id || JSON.stringify(ref)).join(', ') || 'not recorded')} · source date ${esc((a.content || {})['source/date'] || (a.method || {})['source/date'] || 'not recorded')} · attested ${esc(a['event-time'] || 'time not recorded')} · basis ${esc(word(a.basis) || 'not recorded')} · standing ${esc(word(a.standing) || 'not recorded')} · coverage ${esc(word(a.coverage) || 'not recorded')}</p>`).join('');
  return `<div><b>${esc(title(r.record))}</b>${esc((r.evidence || []).map(a => (a.content || {})['benefit/name'] || (a.content || {})['entity/name']).filter(Boolean).join(' · '))} · evidence ${esc(word(r['evidence-completeness']) || 'unknown')}${facts || '<p>No attestations returned.</p>'}</div>`;
}
function render(data, { id = '', raw = false, check = { result: 'not-run' }, evidence = null } = {}) {
  const back = 'bf' + (id ? ':' + id : '');
  const cycle = (raw ? 'bf' : 'bf-raw') + (id ? ':' + id : '');
  const head = `<div data-cycle="${esc(cycle)}"><h1>Benefit filings</h1>${link('bf', 'All benefit lines')} · c: ${raw ? 'readable view' : 'raw records'}`;
  const rows = [];
  const row = r => { const n = rows.push(r) - 1; return reading(r) + link('bf-evidence:' + encodeURIComponent(id) + ':' + n, 'Open this evidence in the right column'); };
  let html;
  if (!data) html = `<p>Not read yet. Open ${link(back, 'this page')} to request it; moving through links does not read detail pages.</p>`;
  else if (word(data.status) === 'reading') html = '<p role="status">Reading… Detail pages can take about 50 seconds.</p>';
  else if (raw) html = json(data);
  else if (word(data.status) !== 'ok') html = `<p>Evidence fibre check (ramo): not run</p><h2 class="err">${esc(word(data.status) || 'refused')}</h2>${json(data)}`;
  else if (word(data.mode) === 'list') html = `<p>${(data.benefits || []).length} benefit lines · valid time ${esc(data['valid-time'])}</p><ul>${(data.benefits || []).map(b => `<li>${link('bf:' + b.benefit.id, title(b.benefit))} · item filings ${esc(b['serves-v1'])} · substrate identities ${esc(b['serves-v2'])}${row({ record: b.benefit, evidence: b.evidence, 'evidence-completeness': b['evidence-completeness'] })}</li>`).join('')}</ul>`;
  else html = `<p>Evidence fibre check (ramo): <strong>${esc(check.result)}</strong></p>${check.violations && check.violations.length ? json(check.violations) : ''}
<p>Historical filings · valid time ${esc(data['valid-time'])}. Evidence acquired for the whole page; open each row below to inspect it.</p>${row(data.benefit)}
<h2>Serving items (serves 1)</h2><ul>${(data.items || []).map(it => `<li>${row(it.item)}<h3>Serving readings</h3>${(it.serves || []).map(row).join('')}<h3>Named entities</h3>${it.names && it.names.length ? it.names.map(n => `<section>${row(n.entity)}<div>Naming link: ${row(n.link)}</div></section>`).join('') : '<p>No names recorded.</p>'}</li>`).join('')}</ul>
<h2>Substrate identities (serves 2)</h2><p>Separate identity lane; not joined to item names.</p><ul>${(data.substrate || []).map(s => `<li>${esc(s['entity/id'])}${row(s.serves)}</li>`).join('')}</ul>`;
  if (evidence !== null) html = rows[evidence] ? '<h2>Reading evidence</h2>' + json(rows[evidence]) : '<p class="err">Reading not available; open its benefit page first.</p>';
  return { title: 'Benefit filings', html: head + html + '</div>' };
}
module.exports = { render, reading };
