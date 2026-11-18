'use strict';
// Finite ground facts from the returned page, mirroring
// futon3c.arxana.fibre-check/benefit-fibres. No query or completeness inference.
const { Rel, run, eq, conde, succeedo, failo } = require('ramo');
const word = x => typeof x === 'string' ? x.replace(/^:/, '') : x;
function readings(view) {
  const rows = [[['benefit'], view.benefit]];
  (view.items || []).forEach((item, i) => {
    rows.push([['items', i, 'item'], item.item]);
    (item.serves || []).forEach((r, j) => rows.push([['items', i, 'serves', j], r]));
    (item.names || []).forEach((n, j) => {
      rows.push([['items', i, 'names', j, 'link'], n.link], [['items', i, 'names', j, 'entity'], n.entity]);
    });
  });
  (view.substrate || []).forEach((s, i) => rows.push([['substrate', i, 'serves'], s.serves]));
  return rows;
}
const invalid = Rel((q, rows) => conde(...rows.map(([path, r]) => [
  // Clojure truth: only nil and false fail; an empty evidence vector is valid.
  (r && r.record && r.record.id != null && r.record.id !== false
    && Array.isArray(r.evidence) && word(r['evidence-completeness']) === 'complete') ? failo : succeedo,
  eq(q, { view: 'cascade/benefit-filings', path, reason: 'reading-without-evidence' })
])));
function check(view) {
  if (!view || word(view.status) !== 'ok' || word(view.mode) !== 'detail')
    return { result: 'not-run', violations: [], reason: 'no-complete-detail' };
  const violations = run()(q => invalid(q, readings(view)));
  return { result: violations.length ? 'violation' : 'pass', violations };
}
module.exports = { check, readings };
