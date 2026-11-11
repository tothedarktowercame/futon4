#!/usr/bin/env node
// Builds the inputs of Mojira's pattern graph (patterns.js) and prints them
// as JSON on stdout: the library patterns and the turn summaries.
//
//   node pattern-index.js <library-dir> <turn-dir> <cache-file>
//
// The turn store is large (~40k records, ~600 MB), so each record's summary
// is cached by file name and mtime; a rerun parses only new or changed
// records. Mojira runs this in a child process so the extension host is not
// blocked. Read-only on both sources; the cache is Mojira's own.
'use strict';
const fs = require('fs');
const path = require('path');
const P = require('./patterns');

// Every pattern under DIR: a .flexiarg file is one pattern, its id the path
// without the extension (family/name, or family/sub/name); a .multiarg file
// holds several, each starting at a line "@arg <id>".
function readLibrary(dir) {
  const texts = [];
  const walk = (d) => {
    for (const e of fs.readdirSync(d, { withFileTypes: true })) {
      const f = path.join(d, e.name);
      if (e.isDirectory()) { if (!e.name.startsWith('.')) walk(f); continue; }
      if (e.name.endsWith('.flexiarg')) {
        texts.push([path.relative(dir, f).slice(0, -'.flexiarg'.length), fs.readFileSync(f, 'utf8'), f]);
      } else if (e.name.endsWith('.multiarg')) {
        const chunks = fs.readFileSync(f, 'utf8').split(/^(?=@arg\s)/m);
        for (const c of chunks) {
          const m = c.match(/^@arg\s+(\S+)/);
          if (m) texts.push([m[1], c, f]);
        }
      }
    }
  };
  walk(dir);
  const known = new Set(texts.map(([id]) => id));
  const seen = new Set();
  return texts.filter(([id]) => (seen.has(id) ? false : seen.add(id)))
    .map(([id, text, file]) => ({ ...P.pattern(id, text, known), file }));
}

function readTurns(dir, cacheFile) {
  let cache = {};
  try { cache = JSON.parse(fs.readFileSync(cacheFile, 'utf8')); } catch (_) { cache = {}; }
  const next = {};
  let parsed = 0;
  for (const f of fs.readdirSync(dir)) {
    if (!f.endsWith('.json')) continue;
    let st;
    try { st = fs.statSync(path.join(dir, f)); } catch (_) { continue; }
    const c = cache[f];
    if (c && c.m === st.mtimeMs) { next[f] = c; continue; }
    let s = null;
    try {
      const r = JSON.parse(fs.readFileSync(path.join(dir, f), 'utf8'));
      if (r && typeof r === 'object' && !Array.isArray(r)) s = P.turnSummary(f.slice(0, -5), r);
    } catch (_) { s = null; }
    parsed++;
    next[f] = { m: st.mtimeMs, s };
  }
  try {
    fs.mkdirSync(path.dirname(cacheFile), { recursive: true });
    fs.writeFileSync(cacheFile + '.tmp', JSON.stringify(next));
    fs.renameSync(cacheFile + '.tmp', cacheFile);
  } catch (_) { /* a missing cache only costs time */ }
  return { turns: Object.values(next).map((c) => c.s).filter(Boolean), parsed, records: Object.keys(next).length };
}

if (require.main === module) {
  const [lib, turnDir, cacheFile] = process.argv.slice(2);
  if (!lib || !turnDir || !cacheFile) { console.error('usage: pattern-index.js <library-dir> <turn-dir> <cache-file>'); process.exit(2); }
  const t0 = Date.now();
  const library = readLibrary(lib);
  const { turns, parsed, records } = readTurns(turnDir, cacheFile);
  process.stdout.write(JSON.stringify({ library, turns, stats: { patterns: library.length, records, parsed, ms: Date.now() - t0 } }));
}

module.exports = { readLibrary, readTurns };
