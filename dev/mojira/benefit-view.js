'use strict';
// Pure: returned facts and host check in, HTML out. No store or file access.
const esc = x => String(x == null ? '' : x).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
const prose = x => String(x == null ? '' : x).replace(/\*|`/g, '');
const word = x => typeof x === 'string' ? x.replace(/^:/, '') : x;
const link = (to, label) => `<a class="ln" data-href="${esc(to)}">${esc(label)}</a>`;
const json = x => `<pre>${esc(JSON.stringify(x, null, 2))}</pre>`;
function title(record) {
  const c = record && record['asserted-content'] || {};
  return c['benefit/line'] || c['item/id'] || c['entity/slug'] || (record && record.id) || '(record id missing)';
}
function provenance(r) {
  return (r.evidence || []).map(a => {
    const refs = (a['source-refs'] || []).map(ref => (ref.path || ref.id || 'source coordinate not recorded') + (ref.line != null ? ':' + ref.line : '')).join(', ');
    const when = a['event-time'] || (a.content || {})['source/date'] || (a.method || {})['source/date'] || 'time not recorded';
    return `${refs || 'source file not recorded'} · ${word(a.basis) || 'basis not recorded'} · ${when} · ${word(a.standing) || 'standing not recorded'} · ${word(a.coverage) || 'coverage not recorded'}`;
  }).join('; ');
}
function reading(r) {
  if (!r) return '<p class="err">Reading missing</p>';
  const cs = (r.evidence || []).map(a => a.content || {});
  const name = cs.map(c => c['entity/name'] || c.name || c['benefit/name']).find(Boolean);
  const descriptions = [...new Set(cs.map(c => c['entity/what'] || c.what || c['sheet/one-line'] || c['story/benefit']).filter(x => typeof x === 'string'))];
  const t = title(r.record);
  const label = t.startsWith('arxana:') ? word((r.record['type-revision'] || [])[0]) || 'Reading' : t;
  return `<div class="benefit-reading"><b>${esc(name || label)}</b>${name && name !== label ? ' · ' + esc(label) : ''}${descriptions.map(d => `<p>${esc(d)}</p>`).join('')}<p class="dim provenance">${esc(provenance(r) || 'No attestations returned.')} · evidence ${esc(word(r['evidence-completeness']) || 'unknown')}</p></div>`;
}
function clusters(benefits) {
  const groups = new Map();
  for (const b of benefits) {
    const key = b.label && b.label['trunk-name'] ? b.label.trunk : 'unnamed';
    if (!groups.has(key)) groups.set(key, { key, name: key === 'unnamed' ? 'Unnamed codes' : b.label['trunk-name'], count: 0, rows: [] });
    const group = groups.get(key); group.rows.push(b); group.count += Number(b['serves-v1'] || 0) + Number(b['serves-v2'] || 0);
  }
  return [...groups.values()].sort((a, b) => (a.key === 'unnamed') - (b.key === 'unnamed') || b.count - a.count || a.key.localeCompare(b.key)).map(g => ({ ...g, rows: g.rows.sort((a, b) => title(a.benefit).localeCompare(title(b.benefit), undefined, { numeric: true })) }));
}
function overview(data) {
  return `<p>${(data.benefits || []).length} benefit lines · valid time ${esc(data['valid-time'])}</p><p>Names from source files, not yet records. Groups ordered by total filings across both identity lanes; these counts are not unique items.</p>` + clusters(data.benefits || []).map(g => `<section><h2>${esc(g.name)} · ${g.count} filings</h2><ul>${g.rows.map(b => `<li>${link('bf:' + b.benefit.id, title(b.benefit) + ' — ' + (b.label ? prose(b.label.name) : 'name not found'))}${b.label && b.label.gloss !== b.label.name ? ' · ' + esc(b.label.gloss) : ''} · item filings ${esc(b['serves-v1'])} · substrate identities ${esc(b['serves-v2'])}</li>`).join('')}</ul></section>`).join('');
}
function render(data, { id = '', raw = false, check = { result: 'not-run' }, evidence = null } = {}) {
  const cycle = (raw ? 'bf' : 'bf-raw') + (id ? ':' + id : '');
  const head = `<div data-cycle="${esc(cycle)}"><h1>Benefit filings</h1>${link('bf', 'All benefit lines')} · c: ${raw ? 'readable view' : 'raw records'}`;
  const rows = [];
  const row = r => { const n = rows.push(r) - 1; return reading(r) + link('bf-evidence:' + encodeURIComponent(id) + ':' + n, 'Open this evidence in the right column'); };
  let html;
  if (!data) html = '<p role="status">Reading…</p>';
  else if (word(data.status) === 'reading') html = '<p role="status">Reading… Fetching benefit filings and their evidence.</p>';
  else if (raw) html = json(data);
  else if (word(data.status) !== 'ok') html = `<p>Evidence fibre check (ramo): not run</p><h2 class="err">${esc(word(data.status) || 'refused')}</h2><p>${esc(word(data.reason) || 'The read did not return a complete page.')}</p><p>Press c for the raw response.</p>`;
  else if (word(data.mode) === 'list') html = overview(data);
  else html = `<p>Evidence fibre check (ramo): <strong>${esc(check.result)}</strong></p>${check.violations && check.violations.length ? '<p class="err">' + check.violations.map(v => esc((v.path || []).join(' / ') + ': ' + (v.reason || 'incomplete evidence'))).join('; ') + '</p>' : ''}
<p>Historical filings · valid time ${esc(data['valid-time'])}. Evidence acquired for the whole page; open each row below to inspect it.</p>${row(data.benefit)}
<h2>Serving items (serves 1)</h2><ul>${(data.items || []).map(it => `<li>${row(it.item)}<h3>Serving readings</h3>${(it.serves || []).map(row).join('')}<h3>Named entities</h3>${it.names && it.names.length ? it.names.map(n => `<section>${row(n.entity)}<div>Naming link: ${row(n.link)}</div></section>`).join('') : '<p>No names recorded.</p>'}</li>`).join('')}</ul>
<h2>Substrate identities (serves 2)</h2><p>Separate identity lane; not joined to item names.</p><ul>${(data.substrate || []).map(s => `<li>${esc(s['entity/id'])}${row(s.serves)}</li>`).join('')}</ul>`;
  if (evidence !== null) html = rows[evidence] ? '<h2>Reading evidence</h2>' + reading(rows[evidence]) + '<p>Press c on the benefit page for raw records.</p>' : '<p class="err">Reading not available; open its benefit page first.</p>';
  return { title: 'Benefit filings', html: head + html + '</div>' };
}
module.exports = { render, reading, clusters };
