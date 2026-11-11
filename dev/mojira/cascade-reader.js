// Reads one per-turn pattern cascade from a lab directory (M-repl-circus,
// futon3c holes/labs/M-repl-circus/cascade-2026-11-11-turn/). No schema for
// cascades in the GCC is agreed yet, so this reads the recorded files
// directly; it is the one module to swap for a GCC query later.
//
// Output (plain data, no layout):
//   { turn: {at, evidence, text},
//     occurrences: [{pattern, n, maps: {I1|I2|G|B: {element, quote} | {void: reason}}}],
//     complete: [pattern], partial: [pattern] (n = 2 or 3),
//     glue: [{element, ends: [[pattern, role], ...]}]   (shared by complete occurrences),
//     notes: {validator, review}, source }
'use strict';
const fs = require('fs');
const path = require('path');

const ROLES = ['I1', 'I2', 'G', 'B'];
// CASCADE-SPEC-v3 §2: IF = I1, HOWEVER = I2, BECAUSE = G, THEN = B.
const PART_OF = { I1: 'IF', I2: 'HOWEVER', G: 'BECAUSE', B: 'THEN' };

function occurrence(o) {
  const maps = {};
  for (const r of ROLES) {
    const m = (o.maps || {})[r] || {};
    maps[r] = m.element ? { element: String(m.element), quote: m.source || m.quote || '' }
      : { void: m.reason || m.why || 'no map' };
  }
  return { pattern: o.pattern, n: ROLES.filter((r) => maps[r].element).length, maps, citedBy: o['cited-by'] || null };
}

// Glue: a situation element named by parts of two or more complete
// occurrences (CASCADE-SPEC-v3 §3.1).
function glueOf(occs) {
  const at = new Map();
  for (const o of occs) for (const r of ROLES) {
    const e = o.maps[r].element;
    if (!e) continue;
    if (!at.has(e)) at.set(e, []);
    at.get(e).push([o.pattern, r]);
  }
  return [...at].filter(([, ends]) => new Set(ends.map(([p]) => p)).size > 1).map(([element, ends]) => ({ element, ends }));
}

function fromRecords(occurrencesJson, frame, verdictText) {
  const occs = occurrencesJson.map(occurrence);
  const complete = occs.filter((o) => o.n === 4);
  const t = (frame && frame.turn) || {};
  const clause = String(verdictText || '').match(/:clause\s+(:[\w-]+)/);
  return {
    turn: { at: t.at || null, evidence: t.evidence_id || null, text: t.text || '' },
    occurrences: occs,
    complete: complete.map((o) => o.pattern),
    partial: occs.filter((o) => o.n === 2 || o.n === 3).map((o) => o.pattern),
    glue: glueOf(complete),
    notes: {
      validator: clause ? `validate-cascade stops at the executable layer (${clause[1]})` : null,
      // claude-13's review, end of verdict.md (2026-11-11), in brief.
      review: /## Review/.test(String(verdictText || ''))
        ? "review: forward-model's maps are thin, and the glue's witness is shape-checked only" : null,
    },
  };
}

function read(dir) {
  const j = (f) => JSON.parse(fs.readFileSync(path.join(dir, f), 'utf8'));
  let verdict = '';
  try { verdict = fs.readFileSync(path.join(dir, 'verdict.md'), 'utf8'); } catch (_) { verdict = ''; }
  return { ...fromRecords(j('occurrences.json'), j('frame.json'), verdict), source: dir };
}

module.exports = { ROLES, PART_OF, occurrence, glueOf, fromRecords, read };
