'use strict';
// Mojira's lint, run with the tests (Joe 2026-11-18: "a 'clj-kondo' style
// linter so that we could check that kind of thing routinely before I see
// the demo", after "TypeError sitemap.folderView is not a function").
//   1. every module parses;
//   2. the webview script, as core.js's shell() builds it, parses;
//   3. every X.name used on a module required as X = require('./m') is
//      something m exports;
//   4. a reload of core.js gets fresh helper modules (the folderView bug:
//      sitemap.js was not re-required, so new core.js ran the old sitemap).
const test = require('node:test');
const assert = require('node:assert');
const fs = require('fs');
const path = require('path');
const vm = require('vm');
const Module = require('module');

const DIR = path.join(__dirname, '..');
const OURS = fs.readdirSync(DIR).filter((f) => f.endsWith('.js'));

// vscode exists only inside the editor; a stub with the settings' defaults.
const props = require('../package.json').contributes.configuration.properties;
const defaults = Object.fromEntries(Object.entries(props).map(([k, v]) => [k.replace(/^mojira\./, ''), v.default]));
const stub = { workspace: { getConfiguration: () => ({ get: (k) => defaults[k] }) }, window: {}, env: {}, Uri: {}, ViewColumn: {}, commands: {} };
const load0 = Module._load;
Module._load = function (req, ...rest) { return req === 'vscode' ? stub : load0.call(this, req, ...rest); };
const freshCore = () => { delete require.cache[require.resolve('../core.js')]; return require('../core.js'); };

test('every module parses', () => {
  for (const f of OURS) {
    const src = fs.readFileSync(path.join(DIR, f), 'utf8').replace(/^#!.*/, '');   // node strips a shebang too
    assert.doesNotThrow(() => new vm.Script(Module.wrap(src), { filename: f }), f);
  }
});

test('the webview script parses as built', () => {
  const html = freshCore()._test.shell();
  const scripts = [...html.matchAll(/<script>([\s\S]*?)<\/script>/g)].map((m) => m[1]);
  assert.ok(scripts.length >= 1 && scripts[0].length > 1000, 'no webview script found');
  for (const s of scripts) assert.doesNotThrow(() => new vm.Script(s, { filename: 'webview' }));
});

test('every module member used is exported', () => {
  const bad = [];
  for (const f of OURS) {
    if (f === 'extension.js') continue;
    // code only: comments and quoted strings name files ("triage.js"), which are not uses
    const src = fs.readFileSync(path.join(DIR, f), 'utf8').replace(/\/\*[\s\S]*?\*\//g, '').replace(/^\s*\/\/.*$/gm, '').replace(/ \/\/ .*$/gm, '')
      .replace(/'(?:[^'\\\n]|\\.)*'(?!\);)/g, "''");
    for (const m of src.matchAll(/const (\w+) = require\('(\.\/[\w-]+)'\);/g)) {
      const [, name, rel] = m;
      const exp = require(path.join(DIR, rel));
      const used = new Set([...src.matchAll(new RegExp('\\b' + name + '\\.(\\w+)', 'g'))].map((u) => u[1]));
      for (const u of used) if (!(u in exp)) bad.push(`${f}: ${name}.${u} (${rel} does not export it)`);
    }
  }
  assert.deepStrictEqual(bad, []);
});

test('reloading core.js re-requires every helper module', () => {
  freshCore();
  const helpers = Object.keys(require.cache).filter((k) => k.startsWith(DIR + path.sep) && path.dirname(k) === DIR
    && !['core.js', 'extension.js'].includes(path.basename(k)));
  assert.ok(helpers.some((k) => k.endsWith('sitemap.js')), 'core.js should require sitemap.js');
  const before = new Map(helpers.map((k) => [k, require.cache[k].exports]));
  freshCore();
  const stale = helpers.filter((k) => require.cache[k] && require.cache[k].exports === before.get(k)).map((k) => path.basename(k));
  assert.deepStrictEqual(stale, [], 'still the old module after a reload');
});
