// Daily-scan triage for Mojira (M-mojira "Candidate view from M-daily-scan").
// Pure helpers: no vscode, no process spawning, so node --test can check them.
// The data comes only through futon7's CLI (`bb daily-loop brief --json`,
// `bb daily-loop verdict ...`); Mojira keeps no copy of it.
'use strict';

const VERDICTS = ['worth', 'not-now', 'invalid'];

// cheshire writes keywords without the colon; accept either form.
function verdictName(v) { return v == null ? null : String(v).replace(/^:/, ''); }

// One row per item, with the ruling from the ledger when there is one (the
// last line for an item wins, as later lines are later rulings).
function rowsFromBrief(brief) {
  const ruled = new Map();
  for (const v of brief['brief/verdicts'] || []) ruled.set(v['item/id'], v);
  return (brief['brief/items'] || []).map((it) => {
    const v = ruled.get(it['item/id']);
    const p = it['item/prediction'] || {};
    return {
      id: it['item/id'],
      summary: it['item/summary'] || '',
      pWorth: typeof p['p-worth'] === 'number' ? p['p-worth'] : null,
      why: p.why || '',
      criteria: it['item/criteria'] || [],
      verdict: v ? verdictName(v.verdict) : null,
      note: v && v.note ? v.note : null,
    };
  });
}

function unruledCount(brief) { return rowsFromBrief(brief).filter((r) => !r.verdict).length; }

function briefArgs(briefId) {
  return ['daily-loop', 'brief'].concat(briefId ? [briefId] : []).concat(['--json']);
}

// The CLI reads the verdict with edn/read-string, so it is passed as a
// keyword (":worth"); a bare "worth" reads as a symbol and is refused
// (futon7 daily_loop.clj cmd-verdict, 2026-11-10).
function verdictArgs(briefId, itemId, verdict, note) {
  if (!VERDICTS.includes(verdict)) throw new Error('verdict must be one of ' + VERDICTS.join(', '));
  if (!briefId || !itemId) throw new Error('brief id and item id (or "all") are required');
  const args = ['daily-loop', 'verdict', briefId, itemId, ':' + verdict];
  if (note && note.trim()) args.push(note.trim());
  return args;
}

// The CLI reports refusals as "ERROR: ..." on stdout with a non-zero exit.
function cliError(stdout, stderr, code) {
  const line = String(stdout || '').split('\n').find((l) => l.startsWith('ERROR:'));
  if (line) return line;
  if (code) return (String(stderr || '').split('\n').find((l) => l.trim()) || ('exit ' + code)).trim();
  return null;
}

module.exports = { VERDICTS, verdictName, rowsFromBrief, unruledCount, briefArgs, verdictArgs, cliError };
